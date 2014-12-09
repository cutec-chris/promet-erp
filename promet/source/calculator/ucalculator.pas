{*******************************************************************************
  Copyright (C) Christian Ulrich info@cu-tec.de

  This source is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 2 of the License, or commercial alternative
  contact us for more information

  This code is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
  to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
  MA 02111-1307, USA.
Created 22.02.2014
*******************************************************************************}
unit ucalculator;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils,  Forms, Controls, Graphics, Dialogs, DBGrids,
  Buttons, Menus, ActnList, XMLPropStorage, StdCtrls, Utils, uIntfStrConsts, db,
  memds, FileUtil, SynEdit, SynMemo, SynHighlighterSQL, Translations, md5,
  ComCtrls, ExtCtrls, DbCtrls, Grids, uSystemMessage,ucalc,SynCompletion;
type

  { TfMain }

  TfMain = class(TForm)
    acLogin: TAction;
    acLogout: TAction;
    acDeleteEnviroment: TAction;
    ActionList1: TActionList;
    cbEnviroment: TComboBox;
    Enviroment: TDatasource;
    Label1: TLabel;
    lInfo: TLabel;
    MainMenu: TMainMenu;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    miLanguage: TMenuItem;
    miMandant: TMenuItem;
    miOptions: TMenuItem;
    Output: TSynMemo;
    Input: TSynMemo;
    SpeedButton1: TSpeedButton;
    SynSQLSyn1: TSynSQLSyn;
    procedure acDeleteEnviromentExecute(Sender: TObject);
    procedure acLoginExecute(Sender: TObject);
    procedure acLogoutExecute(Sender: TObject);
    procedure cbEnviromentExit(Sender: TObject);
    procedure cbEnviromentSelect(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FSynCompletionExecute(Sender: TObject);
    procedure InputKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    { private declarations }
    FHistory : TStringList;
    FHistoryIndex : Integer;
    FSynCompletion: TSynCompletion;
  public
    { public declarations }
    CalcEnviroment : TCalcEnviroments;
    procedure DoCreate;
  end;
var
  fMain: TfMain;
implementation
{$R *.lfm}
uses uBaseApplication, uData, uBaseDbInterface, uOrder,uStatistic,LCLType,
  MathParser,uDataSet,uScriptEditor;
procedure TfMain.DoCreate;
begin
  with Application as IBaseApplication do
    begin
      SetConfigName('Calculator');
    end;
  with Application as IBaseDbInterface do
    LoadMandants;
end;
procedure TfMain.acLoginExecute(Sender: TObject);
begin
  with Application as IBaseApplication do
    if not Login then
      begin
        Application.Terminate;
        exit;
      end;
  acLogin.Enabled:=False;
  acLogout.Enabled:=True;
  CalcEnviroment := TCalcEnviroments.Create(nil);
  CalcEnviroment.CreateTable;
  CalcEnviroment.Typ := 'CALC';
  CalcEnviroment.Open;
  if CalcEnviroment.Count=0 then
    begin
      CalcEnviroment.Insert;
      CalcEnviroment.FieldByName('NAME').AsString:=strStandard;
      CalcEnviroment.Post;
    end;
  cbEnviroment.Items.Clear;
  while not CalcEnviroment.EOF do
    begin
      cbEnviroment.AddItem(CalcEnviroment.FieldByName('NAME').AsString,nil);
      CalcEnviroment.Next;
    end;
  cbEnviroment.ItemIndex:=0;
  CalcEnviroment.First;
  cbEnviromentSelect(nil);
  with Application as IBaseDBInterface do
    begin
      FHistory.Text := DBConfig.ReadString('CALCHISTORY','');
      FHistoryIndex:=FHistory.Count;
    end;
end;
procedure TfMain.acDeleteEnviromentExecute(Sender: TObject);
begin
  cbEnviroment.Items.Delete(cbEnviroment.ItemIndex);
  CalcEnviroment.Delete;
end;
procedure TfMain.acLogoutExecute(Sender: TObject);
begin
  with Application as IBaseApplication do
    Logout;
end;
procedure TfMain.cbEnviromentExit(Sender: TObject);
begin
  if not CalcEnviroment.Locate('NAME',cbEnviroment.Text,[]) then
    begin
      CalcEnviroment.Insert;
      CalcEnviroment.FieldByName('NAME').AsString:=cbEnviroment.Text;
      CalcEnviroment.Post;
      cbEnviroment.Items.Add(cbEnviroment.Text);
    end;
  cbEnviromentSelect(nil);
end;
procedure TfMain.cbEnviromentSelect(Sender: TObject);
begin
  with CalcEnviroment.Variables do
    begin
      Active:=True;
      First;
      Output.Clear;
      while not EOF do
        begin
          Output.Append(FieldByName('NAME').AsString+'='+FieldByName('FORMULA').AsString+'='+FieldByName('RESULT').AsString);
          Next;
        end;
    end;
  Input.SetFocus;
end;
procedure TfMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  FreeAndNil(CalcEnviroment);
  with Application as IBaseApplication do
    begin
      with Application as IBaseDBInterface do
        begin
          DBConfig.WriteString('CALCHISTORY',FHistory.Text);
        end;
      Data.ActiveUsers.DataSet.AfterScroll:=nil;
      SaveConfig;
      DoExit;
    end;
end;
procedure TfMain.FormCreate(Sender: TObject);
begin
  CalcEnviroment := nil;
  FHistory := TStringList.Create;
  FSynCompletion := TSynCompletion.Create(Self);
  FSynCompletion.CaseSensitive := False;
  FSynCompletion.AddEditor(Input);
  FSynCompletion.OnExecute:=@FSynCompletionExecute;
  //FSynCompletion.OnUTF8KeyPress:=@FSynCompletionUTF8KeyPress;
  //FSynCompletion.OnSearchPosition:=@FSynCompletionSearchPosition;
end;
procedure TfMain.FormDestroy(Sender: TObject);
begin
  FSynCompletion.Free;
  FHistory.Free;
end;
procedure TfMain.FormShow(Sender: TObject);
begin
  if not acLogin.Enabled then exit;
  with Application as IBaseApplication do
    RestoreConfig; //Must be called when Mainform is Visible
  acLogin.Execute;
end;

procedure TfMain.FSynCompletionExecute(Sender: TObject);
function GetCurWord:string;
var
  S:string;
  i,j:integer;
begin
  Result:='';
  with TSynCompletion(Sender).Editor do
    begin
      S:=Trim(Copy(LineText, 1, CaretX));
      I:=Length(S);
      while (i>0) and (S[i]<>'.') do Dec(I);
      if (I>0) then
      begin
        J:=i-1;
        //Get table name
        while (j>0) and (S[j] in ['A'..'z','"']) do Dec(j);
        Result:=trim(Copy(S, j+1, i-j-1));
      end;
    end;
end;
var
  s: String;
begin
  with FSynCompletion.ItemList do
    begin
      Clear;
      s := GetCurWord;
      with CalcEnviroment.Variables do
        begin
          Active:=True;
          First;
          Output.Clear;
          while not EOF do
            begin
              if copy(lowercase(FieldByName('NAME').AsString),0,length(s))=lowercase(s) then
                Add(FieldByName('NAME').AsString);
              Next;
            end;
        end;

    end;
end;

procedure TfMain.InputKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState
  );
var
  sl: TStringList;
  i: Integer;
  aData: TDataSet;
  aValue: Extended;
begin
  if Key = VK_RETURN then
    begin
      lInfo.Visible:=False;
      Key := 0;
      sl := TStringList.Create;
      if CalcEnviroment.Calculate(Input.Text,sl,aData,aValue) then
        begin
          while FHistory.IndexOf(Input.Text)>0 do
            FHistory.Delete(FHistory.IndexOf(Input.Text));
          FHistoryIndex := FHistory.Add(Input.Text)+1;
          for i := 0 to sl.Count-1 do
            Output.Append(sl[i]);
          Input.Clear;
          if Assigned(aData) and (aData.Active) and (aData.RecordCount>0) and (aData.FieldDefs.Count>1) then
            begin
              fDataSet := TfDataSet.Create(Self);
              fDataSet.Show;
              fDataSet.DataSet:=aData;
            end
          else if aValue <> 0 then
            begin
              if Assigned(aData) then aData.Free;
              Output.Append('='+FloatToStr(aValue));
              Input.Text:=StringReplace(FloatToStr(aValue),DecimalSeparator,'.',[rfReplaceAll]);
              Input.CaretX := length(Input.Text);
            end
          else if Assigned(aData) then aData.Free;
        end
      else
        begin
          lInfo.Caption:=sl.Text;
          lInfo.Visible:=True;
        end;
      sl.Free;
    end
  else if Key = VK_UP then
    begin
      if FHistoryIndex>0 then
        begin
          dec(FHistoryIndex);
          if FHistoryIndex>FHistory.Count-1 then
            FHistoryIndex:=FHistory.Count-1;
          Input.Text := FHistory[FHistoryIndex];
        end
      else FHistoryIndex := FHistory.Count;
    end
  else if Key = VK_DOWN then
    begin
      if FHistoryIndex<FHistory.Count-1 then
        begin
          inc(FHistoryIndex);
          Input.Text := FHistory[FHistoryIndex];
        end
      else FHistoryIndex := 0;
    end;
end;
initialization
end.
