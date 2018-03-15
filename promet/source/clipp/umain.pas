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
Created 04.12.2013
*******************************************************************************}
unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, types, FileUtil, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, ComCtrls, ActnList, Clipbrd, Menus, Buttons, process,
  uclipp, uMainTreeFrame, DB, LCLType;

type
  TfMain = class(TForm)
    acLogin: TAction;
    acLogout: TAction;
    acAdd: TAction;
    acRestore: TAction;
    acDelete: TAction;
    acSave: TAction;
    acCancel: TAction;
    ActionList2: TActionList;
    Datasource: TDatasource;
    eSearch: TEdit;
    eName: TEdit;
    Image1: TImage;
    iTemp1: TImage;
    iTemp2: TImage;
    iTemp3: TImage;
    iTemp4: TImage;
    iTemp5: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    lTemp1: TLabel;
    lTemp2: TLabel;
    lTemp3: TLabel;
    lTemp4: TLabel;
    lTemp5: TLabel;
    MainMenu: TMainMenu;
    mDesc: TMemo;
    miSet: TMenuItem;
    miGet: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    miClear: TMenuItem;
    miLanguage: TMenuItem;
    miMandant: TMenuItem;
    miOptions: TMenuItem;
    Panel1: TPanel;
    Panel3: TPanel;
    pClipboard: TPanel;
    pmTempBoards: TPopupMenu;
    pTemp: TPanel;
    pTemp1: TPanel;
    pTemp2: TPanel;
    pTemp3: TPanel;
    pTemp4: TPanel;
    pTemp5: TPanel;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    Timer1: TTimer;
    tRefreshTemps: TTimer;
    tvMain: TPanel;
    Panel2: TPanel;
    Splitter1: TSplitter;
    ToolBar1: TToolBar;
    bNew: TToolButton;
    brestore: TToolButton;
    procedure acAddExecute(Sender: TObject);
    procedure acCancelExecute(Sender: TObject);
    procedure acDeleteExecute(Sender: TObject);
    procedure acLoginExecute(Sender: TObject);
    procedure acLogoutExecute(Sender: TObject);
    procedure acRestoreExecute(Sender: TObject);
    procedure acSaveExecute(Sender: TObject);
    procedure DatasourceStateChange(Sender: TObject);
    procedure eNameEditingDone(Sender: TObject);
    procedure eNameKeyPress(Sender: TObject; var Key: char);
    procedure eSearchKeyPress(Sender: TObject; var Key: char);
    function fMainTreeFrameOpen(aEntry: TTreeEntry): Boolean;
    procedure fMainTreeFrameSelectionChanged(aEntry: TTreeEntry);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure mDescEditingDone(Sender: TObject);
    procedure mDescKeyPress(Sender: TObject; var Key: char);
    procedure miClearClick(Sender: TObject);
    procedure miSetClick(Sender: TObject);
    procedure pmTempBoardsClose(Sender: TObject);
    procedure pmTempBoardsPopup(Sender: TObject);
    procedure pTemp1DblClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure tRefreshTempsTimer(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    DataSet : TClipp;
    TempDataSet : TClipp;
    SearchDS : TClipp;
    procedure DoCreate;
    procedure RefreshView;
    function BuildCaption(aDS: TClipp): string;
  end;

var
  fMain: TfMain;
implementation
uses uBaseApplication, uData, uBaseDbInterface,
  uDocuments,uFilterFrame,uIntfStrConsts,uPrometFrames,uBaseDbClasses,
  uWikiFrame;
resourcestring
  strDirmustSelected                 = 'Wählen (oder erstellen) Sie ein Verzeichnis um den Eintrag abzulegen.';
  strRestoreCompleted                = 'in Zwischenablage';
  strRestorePartCompleted            = 'teilweise in Zwischenablage';
  strClear                           = '<leer>';
{$R *.lfm}
procedure TfMain.acAddExecute(Sender: TObject);
var
  aFormat: LongWord;
  i: Integer;
  aMime: String;
  aProc: TProcess;
  aExt: String;
  aStream: TFileStream;
  aMStream: TMemoryStream;
  aFStream: TFileStream;
  aOK: Boolean;
  aName: String;
  ParentID: String;
begin
  if Assigned(fMainTreeFrame.tvMain.Selected)
  and (TTreeEntry(fMainTreeFrame.tvMain.Selected.Data).Typ = etDir) then
    begin
      if InputQuery(strName,strName,aName) then
        begin
          Screen.Cursor:=crHourGlass;
          DataSet.Insert;
          DataSet.FieldByName('NAME').AsString:=aName;
          Data.SetFilter(Data.Tree,'',0,'','ASC',False,True,False);
          Data.GotoBookmark(Data.Tree,TTreeEntry(fMainTreeFrame.tvMain.Selected.Data).Rec);
          ParentID := Data.Tree.Id.AsString;
          DataSet.FieldByName('TREEENTRY').AsString:=ParentID;
          DataSet.AddFromClipboard;
          RefreshView;
          DataSet.Post;
          fMainTreeFrame.tvMain.Selected.Collapse(False);
          fMainTreeFrame.tvMain.Selected.Expand(False);
          Screen.Cursor:=crDefault;
        end;
    end
  else Showmessage(strDirmustSelected);
end;

procedure TfMain.acCancelExecute(Sender: TObject);
begin
  DataSet.DataSet.Cancel;
end;

procedure TfMain.acDeleteExecute(Sender: TObject);
begin
  if dataSet.Count>0 then
    begin
      DataSet.Delete;
      fMainTreeFrame.tvMain.Selected.Delete;
    end;
  RefreshView;
end;
procedure TfMain.acLoginExecute(Sender: TObject);
var
  WikiFrame: TfWikiFrame;
  Node: TTreeNode;
  miNew: TMenuItem;
  aDocuments: TDocuments;
  aStat: TTreeNode;
begin
  with Application as IBaseApplication do
    if not Login then
      begin
        Application.Terminate;
        exit;
      end;
  acLogin.Enabled:=False;
  acLogout.Enabled:=True;
  uClipp.AddToMainTree;
  if fMainTreeFrame.tvMain.Items.Count>0 then
    fMainTreeFrame.tvMain.Items[0].Expanded:=True;
  DataSet := TClipp.Create(nil);
  TempDataSet := TClipp.Create(nil);
  Datasource.DataSet := DataSet.DataSet;
  SearchDS := TClipp.Create(nil);
  tRefreshTempsTimer(nil);
  tRefreshTemps.Enabled:=True;
end;
procedure TfMain.acLogoutExecute(Sender: TObject);
begin
  tRefreshTemps.Enabled:=false;
  TempDataSet.Free;
  DataSet.Free;
  SearchDS.Free;
  with Application as IBaseApplication do
    Logout;
end;
procedure TfMain.acRestoreExecute(Sender: TObject);
var
  aRes: TRestoreResult;
begin
  Screen.Cursor:=crHourGlass;
  if DataSet.Count>0 then
    begin
      aRes := DataSet.RestoreToClipboard;
      if aRes = rrFully then
        begin
          pClipboard.Caption:=strRestoreCompleted;
          pClipboard.Color:=clLime;
        end
      else if aRes = rrPartially then
        begin
          pClipboard.Caption:=strRestorePartCompleted;
          pClipboard.Color:=clYellow;
        end;
    end;
  Screen.Cursor:=crDefault;
end;
procedure TfMain.acSaveExecute(Sender: TObject);
begin
  DataSet.Post;
end;
procedure TfMain.DatasourceStateChange(Sender: TObject);
begin
  acSave.Enabled:=DataSet.CanEdit;
  acCancel.Enabled:=DataSet.CanEdit;
end;
procedure TfMain.eNameEditingDone(Sender: TObject);
begin
  if DataSet.Count>0 then
    begin
      if not DataSet.CanEdit then DataSet.DataSet.Edit;
      DataSet.FieldByName('NAME').AsString := eName.Text;
    end;
end;

procedure TfMain.eNameKeyPress(Sender: TObject; var Key: char);
begin
  eNameEditingDone(Sender);
end;
procedure TfMain.eSearchKeyPress(Sender: TObject; var Key: char);
begin
  Timer1.Enabled:=True;
end;
function TfMain.fMainTreeFrameOpen(aEntry: TTreeEntry): Boolean;
begin
  if aEntry.Typ = etClipboardItem then
    begin
      DataSet.Filter(aEntry.Filter);
      DataSet.GotoBookmark(aEntry.Rec);
      if DataSet.Count>0 then
        acRestore.Execute;
    end;
end;
procedure TfMain.fMainTreeFrameSelectionChanged(aEntry: TTreeEntry);
begin
  DataSet.Close;
  if aEntry.Typ = etClipboardItem then
    begin
      DataSet.Filter(aEntry.Filter);
      DataSet.GotoBookmark(aEntry.Rec);
    end;
  RefreshView;
end;
procedure TfMain.FormCreate(Sender: TObject);
begin
  uMainTreeFrame.fMainTreeFrame := TfMainTree.Create(Self);
  fMainTreeFrame.Parent := tvMain;
  fMainTreeFrame.Align:=alClient;
  fMainTreeFrame.SearchOptions:='CLIPP';
  fMainTreeFrame.OnSelectionChanged:=@fMainTreeFrameSelectionChanged;
  fMainTreeFrame.OnOpen:=@fMainTreeFrameOpen;
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
procedure TfMain.mDescEditingDone(Sender: TObject);
begin
  if DataSet.Count>0 then
    begin
      if not DataSet.CanEdit then DataSet.DataSet.Edit;
      DataSet.FieldByName('DESC').AsString := mDesc.Lines.Text;
    end;
end;
procedure TfMain.mDescKeyPress(Sender: TObject; var Key: char);
begin
  mDescEditingDone(Sender);
end;
procedure TfMain.miClearClick(Sender: TObject);
var
  aCon: TComponent;
  aTag: PtrInt;
begin
  aTag := TComponent(Sender).Tag;
  TempDataSet.Filter(Data.QuoteField('NAME')+'='+Data.QuoteValue('Temp'+IntToStr(aTag)));
  while TempDataSet.Count>0 do
    TempDataSet.Delete;
  aCon := FindComponent('lTemp'+IntToStr(aTag));
  if Assigned(aCon) then
    TPanel(aCon).Caption:=strClear;
end;
procedure TfMain.miSetClick(Sender: TObject);
var
  aTag: PtrInt;
  aCon: TComponent;
begin
  Screen.Cursor:=crHourGlass;
  aTag := TComponent(Sender).Tag;
  TempDataSet.Filter(Data.QuoteField('NAME')+'='+Data.QuoteValue('Temp'+IntToStr(aTag)));
  if TempDataSet.Count>0 then
    TempDataSet.DataSet.Edit
  else
    begin
      TempDataSet.Append;
      TempDataSet.FieldByName('NAME').AsString:='Temp'+IntToStr(aTag);
    end;
  TempDataSet.AddFromClipboard;
  TempDataSet.Post;
  aCon := FindComponent('lTemp'+IntToStr(aTag));
  if Assigned(aCon) then
    TLabel(aCon).Caption:=TempDataSet.FieldByName('CHANGEDBY').AsString;
  Screen.Cursor:=crDefault;
end;

procedure TfMain.pmTempBoardsClose(Sender: TObject);
begin
  tRefreshTemps.Enabled:=True;
end;

procedure TfMain.pmTempBoardsPopup(Sender: TObject);
var
  aCon: TControl;
  i: Integer;
  AWinControl: TWinControl;
  ScreenPos: types.TPoint;
  ClientPos: TPoint;
begin
  tRefreshTemps.Enabled:=False;
  ScreenPos := Mouse.CursorPos;
  AWinControl := Self;
  if Assigned(AWinControl) then
    begin
      ClientPos := AWinControl.ScreenToClient(ScreenPos);
      aCon := AWinControl.ControlAtPos(ClientPos,
                        [capfAllowDisabled, capfAllowWinControls, capfRecursive]);
    end;
  while Assigned(aCon) and (aCon.ClassType<>TPanel) do
    begin
      if aCon.Parent is TControl then
        aCon := TControl(aCon.Parent)
      else aCon := nil;
    end;
  if Assigned(aCon) then
    begin
      for i := 0 to pmTempBoards.Items.Count-1 do
        begin
          pmTempBoards.Items[i].Enabled:=True;
          pmTempBoards.Items[i].Tag := aCon.Tag
        end;
    end
  else
    begin
      for i := 0 to pmTempBoards.Items.Count-1 do
        pmTempBoards.Items[i].Enabled:=False;
    end;
end;
procedure TfMain.pTemp1DblClick(Sender: TObject);
var
  aTag: PtrInt;
  aRes: TRestoreResult;
  aCon: TControl;
begin
  Screen.Cursor:=crHourGlass;
  aCon := TControl(Sender);
  while Assigned(aCon) and (aCon.ClassType<>TPanel) do
    begin
      if aCon.Parent is TControl then
        aCon := TControl(aCon.Parent)
      else aCon := nil;
    end;
  if (not Assigned(aCon)) and (Sender is TMenuItem) then
    aTag := TComponent(Sender).Tag
  else
    aTag := aCon.Tag;
  TempDataSet.Filter(Data.QuoteField('NAME')+'='+Data.QuoteValue('Temp'+IntToStr(aTag)));
  if TempDataSet.Count>0 then
    begin
      aRes := TempDataSet.RestoreToClipboard;
      if aRes = rrFully then
        begin
          pClipboard.Caption:=strRestoreCompleted;
          pClipboard.Color:=clLime;
        end
      else if aRes = rrPartially then
        begin
          pClipboard.Caption:=strRestorePartCompleted;
          pClipboard.Color:=clYellow;
        end;
    end;
  Screen.Cursor:=crDefault;
end;
procedure TfMain.Timer1Timer(Sender: TObject);
  function NodeThere(aRec : largeInt) : TTreeNode;
  var
    aNode: TTreeNode;
  begin
    Result := nil;
    if uMainTreeFrame.fMainTreeFrame.tvMain.Items.Count>0 then
      aNode := uMainTreeFrame.fMainTreeFrame.tvMain.Items[0];
    while Assigned(aNode) do
      begin
        if Assigned(aNode.Data) and (TTreeEntry(aNode.Data).Rec = aRec) then
          begin
            Result := aNode;
            break;
          end;
        aNode := aNode.GetNext;
      end;
  end;
  function ExpandDir(aRec : LargeInt) : TTReeNode;
  var
    bParent : Variant;
  begin
    Result := NodeThere(aRec);
    if not Assigned(Result) then
      begin
        if Data.Tree.DataSet.Locate('SQL_ID',aRec,[]) then
          begin
            bParent := Data.Tree.FieldByName('PARENT').AsVariant;
            ExpandDir(bParent);
          end;
        Result := NodeThere(aRec);
      end;
    Result.Expand(False);
  end;
var
  aParent: TTreeNode;
  i: Integer;
  aName: String;
begin
  Timer1.Enabled:=False;
  if eSearch.Text<>'' then
    begin
      SearchDS.Filter(data.ProcessTerm(Data.QuoteField('NAME')+'='+Data.QuoteValue('*'+eSearch.Text+'*')),1);
      if SearchDS.Count>0 then
        begin
          while not SearchDS.EOF do
            begin
              aParent := ExpandDir(SearchDS.FieldByName('TREEENTRY').AsVariant);
              if Assigned(aParent) then
                begin
                  for i := 0 to aParent.Count-1 do
                    begin
                      if Assigned(aParent.Items[i].Data) and (TTreeEntry(aParent.Items[i].Data).Rec = SearchDS.Id.AsVariant) then
                        begin
                          fMainTreeFrame.tvMain.Selected:=aParent.Items[i];
                          exit;
                        end;
                    end;
                end;
              SearchDS.Next;
            end;
        end;
    end;
end;
procedure TfMain.tRefreshTempsTimer(Sender: TObject);
var
  i: Integer;
  aConName: String;
  aConName1: String;
begin
  tRefreshTemps.Enabled:=False;
  for i := 0 to pTemp.ControlCount-1 do
    if copy(pTemp.Controls[i].Name,0,5) = 'pTemp' then
      begin
        aConName := 'lTemp'+copy(pTemp.Controls[i].Name,6,10);
        TLabel(FindComponent(aConName)).Caption:=strClear;
        TLabel(FindComponent(aConName)).Hint:='';
        aConName := 'iTemp'+copy(pTemp.Controls[i].Name,6,10);
        TImage(FindComponent(aConName)).Hint := '';
      end;
  TempDataSet.Filter(Data.ProcessTerm(Data.QuoteField('NAME')+'='+Data.QuoteValue('Temp*')));
  while not TempDataSet.EOF do
    begin
      if Assigned(FindComponent('l'+TempDataSet.FieldByName('NAME').AsString)) then
        begin
          TPanel(FindComponent('l'+TempDataSet.FieldByName('NAME').AsString)).Caption:=BuildCaption(TempDataSet);
          TImage(FindComponent('i'+TempDataSet.FieldByName('NAME').AsString)).Hint:=TempDataSet.FieldByName('DESCRIPTION').AsString;
          Tlabel(FindComponent('l'+TempDataSet.FieldByName('NAME').AsString)).Hint:=TempDataSet.FieldByName('DESCRIPTION').AsString;
        end;
      TempDataSet.Next;
    end;
  tRefreshTemps.Enabled:=True;
end;
procedure TfMain.DoCreate;
begin
  with Application as IBaseApplication do
    begin
      SetConfigName('Clipp');
    end;
  with Application as IBaseDbInterface do
    LoadMandants;
end;
procedure TfMain.RefreshView;
begin
  Panel2.Enabled := False;
  Image1.Picture.Clear;
  mDesc.Lines.Clear;
  eName.Text:='';
  if DataSet.Count>0 then
    begin
      eName.Text:=DataSet.FieldByName('NAME').AsString;
      mDesc.Lines.Text:=DataSet.FieldByName('DESC').AsString;
      Panel2.Enabled:=True;
      pClipboard.Color:=clWindow;
      pClipboard.Caption:='';
    end;
end;

function TfMain.BuildCaption(aDS: TClipp) : string;
var
  aTime: Extended;
  aTimeres: String;
begin
  Result := aDS.FieldByName('CHANGEDBY').AsString;
  aTime := Now()-aDS.TimeStamp.AsDateTime;
  if aTime*MinsPerDay<60 then
    aTimeres := 'vor '+IntToStr(trunc(aTime*MinsPerDay))+'m'
  else if aTime<1 then
    aTimeres := 'vor '+IntToStr(trunc(aTime*HoursPerDay))+'h'
  else
    aTimeres := 'vor '+IntToStr(trunc(aTime))+'T'
  ;
  Result := Result+lineending+aTimeRes;
end;

end.

