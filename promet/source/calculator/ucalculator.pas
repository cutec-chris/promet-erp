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
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, DBGrids,
  Buttons, Menus, ActnList, XMLPropStorage, StdCtrls, Utils, uIntfStrConsts, db,
  memds, FileUtil, SynEdit, SynMemo, SynHighlighterSQL, Translations, md5,
  ComCtrls, ExtCtrls, DbCtrls, Grids, uSystemMessage;
type

  { TfMain }

  TfMain = class(TForm)
    acLogin: TAction;
    acLogout: TAction;
    ActionList1: TActionList;
    MainMenu: TMainMenu;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    miLanguage: TMenuItem;
    miMandant: TMenuItem;
    miOptions: TMenuItem;
    Output: TSynMemo;
    Input: TSynMemo;
    SynSQLSyn1: TSynSQLSyn;
    procedure acLoginExecute(Sender: TObject);
    procedure acLogoutExecute(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure InputKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    { private declarations }
  public
    { public declarations }
    Variables : TStringlist;
    procedure DoCreate;
  end;
var
  fMain: TfMain;
implementation
uses uBaseApplication, uData, uBaseDbInterface, uOrder,uStatistic,LCLType,
  MathParser;
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
end;
procedure TfMain.acLogoutExecute(Sender: TObject);
begin
  with Application as IBaseApplication do
    Logout;
end;
procedure TfMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  with Application as IBaseApplication do
    begin
      Data.ActiveUsers.DataSet.AfterScroll:=nil;
      SaveConfig;
      DoExit;
    end;
end;

procedure TfMain.FormCreate(Sender: TObject);
begin
  Variables := TStringList.Create;
end;

procedure TfMain.FormDestroy(Sender: TObject);
begin
  Variables.Free;
end;

procedure TfMain.FormShow(Sender: TObject);
begin
  if not acLogin.Enabled then exit;
  with Application as IBaseApplication do
    RestoreConfig; //Must be called when Mainform is Visible
  acLogin.Execute;
  if Assigned(Data) then
    begin
    end;
end;

procedure TfMain.InputKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState
  );
var
  Stmt: TSQLStatemnt;
  aParser: TMathParser;
  aTree: PTTermTreeNode;
  aIn: String;
  aVar: String;
  aOut: String;
  aDS: TDataSet;
begin
  if Key = VK_RETURN then
    begin
      Key := 0;
      if copy(Input.Text,0,2)='--' then
        begin
          Output.Append(Input.Text);
          Input.Text:='';
          exit;
        end;
      aIn := Input.text;
      if RPos('=',aIn)>0 then
        begin
          aVar := copy(aIn,RPos('=',aIn)+1,length(aIn));
          aIn := copy(aIn,0,RPos('=',aIn)-1);
        end;
      aParser := TMathParser.Create;
      try
        aTree := aParser.ParseTerm(aIn);
      except
        aTree := nil;
      end;
      if Assigned(aTree) then
        begin
          try
            Output.Append(aParser.FormatTerm(aTree));
            aOut := FloatToStr(aParser.CalcTree(aTree));
            if aVar<>'' then
              Output.Append('='+aOut+'='+aVar)
            else
              Output.Append('='+aOut);
            if aVar <> '' then
              Variables.Values[aVar] := aIn;
          except
            on e : Exception do
              Output.Append('='+e.Message);
          end;
        end
      else
        begin
          Stmt := TSQLStatemnt.Create;
          Stmt.SQL:=aIn;
          if Stmt.Parse then
            begin
              aDS := Data.GetNewDataSet(Stmt.SQL);
              try
                aDS.Open;
                Output.Append(Stmt.SQL);
                aOut := aDS.Fields[0].AsString;
                if aVar<>'' then
                  Output.Append('='+aOut+'='+aVar)
                else
                  Output.Append('='+aOut);
                if aVar <> '' then
                  Variables.Values[aVar] := aIn;
              except
              end;
              aDS.Free;
            end;
          Stmt.Free;
        end;
      aParser.Free;
      Input.Clear;
    end;
end;

initialization
  {$I ucalculator.lrs}
end.
