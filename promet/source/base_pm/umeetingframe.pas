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
Created 28.02.2013
*******************************************************************************}
unit uMeetingFrame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, db, FileUtil, LR_Class, LR_DBSet, Forms, Controls,
  ActnList, ComCtrls, ExtCtrls, StdCtrls, Buttons, DbCtrls, EditBtn,
  uprometframesinplace, uPrometFrames, uExtControls, ugridview, Dialogs,
  uIntfStrConsts, variants, DBGrids, Grids, Graphics, Menus, DBZVDateTimePicker,
  ZVDateTimePicker, uBaseSearch, uBaseDbClasses,uBaseDatasetInterfaces;

type
  THackCustomGrid = class(TCustomGrid);

  { TfMeetingFrame }

  TfMeetingFrame = class(TPrometMainFrame)
    acAddPos: TAction;
    acCancel: TAction;
    acClose: TAction;
    acCopy: TAction;
    acDelete: TAction;
    acDelPos: TAction;
    acImport: TAction;
    acMAkeSubTask: TAction;
    acPaste: TAction;
    acPrint: TAction;
    acRefresh: TAction;
    acRights: TAction;
    acSave: TAction;
    acSetTreeDir: TAction;
    acShowTreeDir: TAction;
    acStartTimeRegistering: TAction;
    acAddTopic: TAction;
    acAppendPos: TAction;
    acRenumber: TAction;
    acRestart: TAction;
    acSetTopic: TAction;
    acPermanentEditormode: TAction;
    acTerminate: TAction;
    Action2: TAction;
    ActionList: TActionList;
    acUnmakeSubTask: TAction;
    bAddPos: TSpeedButton;
    bAddPos1: TSpeedButton;
    bAddPos2: TSpeedButton;
    bAddPos3: TSpeedButton;
    bDeletePos: TSpeedButton;
    bDeletePos2: TSpeedButton;
    bDeletePos5: TSpeedButton;
    bRenumber: TSpeedButton;
    Bevel1: TBevel;
    Bevel3: TBevel;
    Bevel4: TBevel;
    Bevel5: TBevel;
    Bevel6: TBevel;
    Bevel7: TBevel;
    Bevel9: TBevel;
    bExecute: TSpeedButton;
    bExecute1: TSpeedButton;
    bRefresh1: TSpeedButton;
    bpermanenetEditor: TSpeedButton;
    bRenumber1: TSpeedButton;
    cbStatus: TComboBox;
    Datasource: TDatasource;
    dtStart: TDBZVDateTimePicker;
    dtEnd: TDBZVDateTimePicker;
    eName: TDBEdit;
    Entrys: TDatasource;
    MandantDetails: TDatasource;
    MenuItem1: TMenuItem;
    miDelete: TMenuItem;
    pmAction: TPopupMenu;
    pNav2: TPanel;
    sbMenue1: TSpeedButton;
    Users: TDatasource;
    ExtRotatedLabel1: TExtRotatedLabel;
    ExtRotatedLabel3: TExtRotatedLabel;
    ExtRotatedLabel4: TExtRotatedLabel;
    Label1: TLabel;
    Label2: TLabel;
    Label5: TLabel;
    lbResults: TListBox;
    Memo1: TDBMemo;
    Label3: TLabel;
    Label4: TLabel;
    lname: TLabel;
    pcPages: TExtMenuPageControl;
    Panel1: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    Panel8: TPanel;
    pComponents: TPanel;
    PEntrys: TfrDBDataSet;
    PUsers: TfrDBDataSet;
    PList: TfrDBDataSet;
    pNav1: TPanel;
    pSearch: TPanel;
    Report: TfrReport;
    sbMenue: TSpeedButton;
    tbLeft: TPanel;
    tsMeeting: TTabSheet;
    ToolBar1: TPanel;
    ToolButton1: TSpeedButton;
    ToolButton2: TSpeedButton;
    procedure acAddPosExecute(Sender: TObject);
    procedure acAddTopicExecute(Sender: TObject);
    procedure acAppendPosExecute(Sender: TObject);
    procedure acCancelExecute(Sender: TObject);
    procedure acCloseExecute(Sender: TObject);
    procedure acDeleteExecute(Sender: TObject);
    procedure acDelPosExecute(Sender: TObject);
    procedure acMAkeSubTaskExecute(Sender: TObject);
    procedure acPermanentEditormodeExecute(Sender: TObject);
    procedure acPrintExecute(Sender: TObject);
    procedure acRefreshExecute(Sender: TObject);
    procedure acRenumberExecute(Sender: TObject);
    procedure acRestartExecute(Sender: TObject);
    procedure acRightsExecute(Sender: TObject);
    procedure acSaveExecute(Sender: TObject);
    procedure acSetTopicExecute(Sender: TObject);
    procedure acTerminateExecute(Sender: TObject);
    procedure ActiveSearchEndSearch(Sender: TObject);
    procedure ActiveSearchItemFound(aIdent: string; aName: string;
      aStatus: string; aActive: Boolean; aLink: string;aPrio :Integer; aItem: TBaseDBList=nil);
    procedure acUnmakeSubTaskExecute(Sender: TObject);
    procedure cbStatusSelect(Sender: TObject);
    procedure DataSetChange(Sender: TObject);
    procedure DatasourceStateChange(Sender: TObject);
    procedure eNameChange(Sender: TObject);
    procedure FGridViewCellButtonClick(Sender: TObject; Cell: TPoint;
      Field: TColumn);
    procedure FGridViewDblClick(Sender: TObject);
    procedure FGridViewDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure FGridViewDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure FGridViewGetCellText(Sender: TObject; aCol: TColumn;
      aRow: Integer; var NewText: string; aFont: TFont);
    function FGridViewSearchKey(Sender: TObject; X, Y: Integer;
      Field: TColumn; var Key: Word; Shift: TShiftState; SearchString: string) : Boolean;
    procedure FGridViewSetupPosition(Sender: TObject; Columns: TGridColumns);
    function fSearchOpenOwnerItem(aLink: string): Boolean;
    function fSearchOpenProjectItem(aLink: string): Boolean;
    function fSearchOpenUserItem(aLink: string): Boolean;
    procedure lbResultsDblClick(Sender: TObject);
    procedure mShorttextChange(Sender: TObject);
    procedure DoInsertInplaceSearch(Data : PtrInt);
    procedure ReportGetValue(const ParName: String; var ParValue: Variant);
    procedure sbMenueClick(Sender: TObject);
  private
    { private declarations }
    FGridView: TfGridView;
    FOwners : TStringList;
    ActiveSearch : TSearch;
    FEditable : Boolean;
    procedure DoOpen;override;
    function SetRights : Boolean;
    procedure AddDocuments(Sender: TObject);
    procedure AddLinks(Sender: TObject);
    procedure AddUsers(Sender : TObject);
  public
    { public declarations }
    constructor Create(AOwner: TComponent); override;
    function OpenFromLink(aLink: string): Boolean; override;
    procedure New;override;
    destructor Destroy; override;
    procedure ShowFrame; override;
    procedure SetLanguage; override;
  end;
procedure AddToMainTree(aAction : TAction;Node : TTreeNode);
var
  MainNode : TTreeNode;
  aNewName: String;
implementation
uses uMainTreeFrame,Utils,uData,umeeting,uBaseDBInterface,uSearch,
  LCLType,uSelectReport,uDocuments,uDocumentFrame,uLinkFrame,umeetingusers,
  uBaseVisualApplication,uProjects,LCLProc,uNRights,utask;
resourcestring
  strSearchFromUsers                       = 'Mit Öffnen wird der gewählte Nutzer in die Liste übernommen';
  strSearchFromMeetings                    = 'Mit Öffnen wird das gewählte Projekt in die Liste übernommen';
  strNewMeeting                            = 'neue Besprechung';
  strEnterMeetingName                      = 'geben Sie einen neuen Namen für die Besprechung an';
  strMeetingName                           = 'Besprechungsname';
  strUnassigned                            = 'kein Bezug';
  strNoUserSelected                        = 'Es ist kein Benutzer oder Verantwortlicher zugewiesen !';
procedure AddToMainTree(aAction: TAction; Node: TTreeNode);
var
  Node1: TTreeNode;
begin
  Node1 := fMainTreeFrame.tvMain.Items.AddChildObject(Node,'',TTreeEntry.Create);
  TTreeEntry(Node1.Data).Typ := etAction;
  TTreeEntry(Node1.Data).Action := aAction;
  Node1 := fMainTreeFrame.tvMain.Items.AddChildObject(Node,'',TTreeEntry.Create);
  TTreeEntry(Node1.Data).Typ := etMeetingList;
end;

procedure ReplaceField(aField: TField; aOldValue: string; var aNewValue: string
  );
begin
  if aField.FieldName='NAME' then
    begin
      with aField.DataSet as IBaseManageDB do
        if TableName='MEETINGS' then
          aNewValue := aNewName;
    end;
end;

procedure TfMeetingFrame.acSaveExecute(Sender: TObject);
begin
  if Assigned(FConnection) then
    begin
      FDataSet.CascadicPost;
      //Data.Commit(FConnection);
      //Data.StartTransaction(FConnection);
      acRefresh.Execute;
    end;
end;

procedure TfMeetingFrame.acSetTopicExecute(Sender: TObject);
begin
  if FGridView.GotoActiveRow then
    begin
      if FGridView.DataSet.CanEdit then FGridView.Post;
      FGridView.DataSet.Edit;
      FGridView.DataSet.FieldByName('DESC').AsString:=trim(FGridView.DataSet.FieldByName('DESC').AsString)+' ('+strUnassigned+')';
      FGridView.DataSet.FieldByName('LINK').AsString:='NONE@NONE';
      FGridView.Post;
    end;
end;

procedure TfMeetingFrame.acTerminateExecute(Sender: TObject);
var
  aTask: TTaskList;
  aStart,aEnd,aDur : TDateTime;
begin
  if FGridView.GotoActiveRow then
    begin
      FGridView.Post;
      aTask := TTaskList.Create(nil);
      aTask.Insert;
      aTask.FieldByName('USER').AsVariant:=TMeetings(DataSet).Entrys.FieldByName('USER').AsVariant;
      aTask.FieldByName('OWNER').AsString:=TMeetings(DataSet).Entrys.FieldByName('OWNER').AsString;
      if TMeetings(DataSet).Entrys.FieldByName('USER').IsNull then
        aTask.FieldByName('USER').AsVariant:=TMeetings(DataSet).Entrys.FieldByName('OWNER').AsVariant;
      if aTask.FieldByName('USER').IsNull then
        Showmessage(strNoUserSelected)
      else if not aTask.Terminate(Now(),aStart,aEnd,aDur) then
        Showmessage(strFailed)
      else
        begin
          TMeetings(DataSet).Entrys.Edit;
          TMeetings(DataSet).Entrys.FieldByName('DUEDATE').AsDateTime:=aEnd;
          acRefresh.Execute;
        end;
      aTask.Cancel;
      aTask.Free;
    end;
end;

procedure TfMeetingFrame.ActiveSearchEndSearch(Sender: TObject);
begin
  if not ActiveSearch.Active then
    begin
      if not ActiveSearch.NewFound then
        begin
          ActiveSearch.Start(ActiveSearch.SearchString,ActiveSearch.NextSearchLevel);
          exit;
        end;
      if (ActiveSearch.Count=0) and (lbResults.Items.Count=0) then
        pSearch.Visible:=False;
    end;
end;

procedure TfMeetingFrame.ActiveSearchItemFound(aIdent: string; aName: string;
  aStatus: string; aActive: Boolean; aLink: string; aPrio: Integer;
  aItem: TBaseDBList);
begin
  with pSearch do
    begin
      if not Visible then
        Visible := True;
    end;
  if aActive then
    if lbResults.Items.IndexOf(aName)=-1 then
      lbResults.Items.AddObject(aName ,TLinkObject.Create(aLink));
end;

procedure TfMeetingFrame.acUnmakeSubTaskExecute(Sender: TObject);
begin
  FGridView.UnSetChild;
end;

procedure TfMeetingFrame.cbStatusSelect(Sender: TObject);
var
  tmp: String;
begin
  tmp := copy(cbStatus.text,pos('(',cbStatus.text)+1,length(cbStatus.text));
  tmp := copy(tmp,0,pos(')',tmp)-1);
  if not FDataSet.CanEdit then FDataSet.DataSet.Edit;
  FDataSet.FieldByName('STATUS').AsString:=tmp;
end;

procedure TfMeetingFrame.DataSetChange(Sender: TObject);
begin
  acSave.Enabled := DataSet.CanEdit or DataSet.Changed;
  acCancel.Enabled:= DataSet.CanEdit or DataSet.Changed;
end;

procedure TfMeetingFrame.DatasourceStateChange(Sender: TObject);
begin
  debugln('StateChange');
end;

procedure TfMeetingFrame.eNameChange(Sender: TObject);
begin
  TabCaption := eName.Text;
  acSave.Enabled := DataSet.CanEdit or DataSet.Changed;
  acCancel.Enabled:= DataSet.CanEdit or DataSet.Changed;
end;

procedure TfMeetingFrame.FGridViewCellButtonClick(Sender: TObject;
  Cell: TPoint; Field: TColumn);
var
  i: Integer;
begin
  i := 0;
  if Field.FieldName = 'USER' then
    begin
      fSearch.AllowSearchTypes(strUsers);
      fSearch.eContains.Clear;
      fSearch.sgResults.RowCount:=1;
      fSearch.OnOpenItem:=@fSearchOpenUserItem;
      fSearch.Execute(True,'TASKSU',strSearchFromUsers);
      fSearch.SetLanguage;
    end
  else if Field.FieldName = 'OWNER' then
    begin
      fSearch.AllowSearchTypes(strUsers);
      fSearch.eContains.Clear;
      fSearch.sgResults.RowCount:=1;
      fSearch.OnOpenItem:=@fSearchOpenOwnerItem;
      fSearch.Execute(True,'TASKSU',strSearchFromUsers);
      fSearch.SetLanguage;
    end
  ;
end;

procedure TfMeetingFrame.FGridViewDblClick(Sender: TObject);
var
  Res: Boolean = False;
begin
  if FGridView.GotoActiveRow then
    begin
      if Data.GotoLink(TMeetings(DataSet).Entrys.FieldByName('LINK').AsString) then Res := True;
    end;
  if not Res then FGridView.gList.EditorMode:=True;
end;

procedure TfMeetingFrame.FGridViewDragDrop(Sender, Source: TObject; X,
  Y: Integer);
var
  nData: uMainTreeFrame.TTreeEntry;
  aProjects: TProject;
  aLink: String;
begin
  if Source = uMainTreeFrame.fMainTreeFrame.tvMain then
    begin
      nData := TTreeEntry(uMainTreeFrame.fMainTreeFrame.tvMain.Selected.Data);
      if nData.Typ=etProject then
        begin
          aProjects := TProject.CreateEx(Self,Data);
          aProjects.CreateTable;
          Data.SetFilter(aProjects,nData.Filter);
          Data.GotoBookmark(aProjects,nData.Rec);
          aLink := Data.BuildLink(aProjects.DataSet);
          aProjects.Free;
        end
      else if nData.Typ = etLink then
        aLink := nData.Link;
      if Data.GetLinkDesc(aLink)<>'' then
        begin
          if (FGridView.DataSet.Changed) then
            FGridView.Post;
          if not (FGridView.DataSet.Canedit) then
            FGridView.Append;
          FGridView.DataSet.FieldByName('DESC').AsString:=Data.GetLinkDesc(aLink);
          FGridView.DataSet.FieldByName('LINK').AsString:=aLink;
          FGridView.Post;
          FGridView.SyncActiveRow(FGridView.DataSet.Id.AsVariant,False,True);
        end;
    end
  else
  if (Source = fSearch.sgResults) then
    begin
      aLink := fSearch.GetLink;
      FGridView.Append;
      FGridView.DataSet.FieldByName('DESC').AsString:=Data.GetLinkDesc(aLink);
      FGridView.DataSet.FieldByName('LINK').AsString:=aLink;
      FGridView.Post;
    end;
end;

procedure TfMeetingFrame.FGridViewDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Accept := False;
  if Assigned(uMainTreeFrame.fMainTreeFrame)
  and (Source = uMainTreeFrame.fMainTreeFrame.tvMain)
  and (
     (TTreeEntry(uMainTreeFrame.fMainTreeFrame.tvMain.Selected.Data).Typ = etProject)
  or (TTreeEntry(uMainTreeFrame.fMainTreeFrame.tvMain.Selected.Data).Typ = etLink)
  ) then
    Accept := True;
  if Assigned(fSearch) and (Source = fSearch.sgResults) then
    begin
      with fSearch.sgResults do
        if copy(fSearch.GetLink,0,7) = 'PROJECT' then
          Accept := True;
    end;
end;

procedure TfMeetingFrame.FGridViewGetCellText(Sender: TObject; aCol: TColumn;
  aRow: Integer; var NewText: string; aFont: TFont);
var
  aUser: String;
begin
  if aCol.FieldName = 'DESC' then
    begin
      if Assigned(aFont) then
        begin
          if (pos('('+strProjectProcess,NewText)>0)
          or (pos('('+strMasterdata,NewText)>0)
          or (pos('('+strUnassigned,NewText)>0)
          then
            aFont.Style := [fsBold]
          else
            aFont.Style := [];
        end;
    end
  else if aCol.FieldName = 'OWNER' then
    begin
      aUser := StringReplace(NewText,'=','',[rfReplaceAll]);
      if aUser = '' then exit;
      if FOwners.Values[aUser] <> '' then
        NewText := FOwners.Values[aUser]
      else
        begin
          if FGridView.GotoRow(TRowObject(FGridView.gList.Objects[0,aRow]).Rec) then
            begin
              FOwners.Values[aUser] := TMeetings(FDataSet).Entrys.OwnerName;
              if trim(FOwners.Values[aUser]) <> '' then
                NewText:=FOwners.Values[aUser];
            end;
        end;
    end
  else  if aCol.FieldName = 'USER' then
    begin
      aUser := StringReplace(NewText,'=','',[rfReplaceAll]);
      if aUser = '' then exit;
      if trim(FOwners.Values[aUser]) <> '' then
        NewText := FOwners.Values[aUser]
      else
        begin
          if FGridView.GotoRow(TRowObject(FGridView.gList.Objects[0,aRow]).Rec) then
            begin
              FOwners.Values[aUser] := TMeetings(FDataSet).Entrys.UserName;
              if trim(FOwners.Values[aUser]) <> '' then
                NewText:=FOwners.Values[aUser];
            end;
        end;
    end;
end;

function TfMeetingFrame.FGridViewSearchKey(Sender: TObject; X, Y: Integer;
  Field: TColumn; var Key: Word; Shift: TShiftState; SearchString: string) : Boolean;
var
  SearchTypes : TFullTextSearchTypes = [];
  SearchLocations : TSearchLocations;
  i: Integer;
  tmp: TCaption;
begin
  Result := False;
  if Assigned(Field) and ((Field.FieldName='USER') or (Field.FieldName='OWNER')) then
    begin
      if SearchString = '' then
        begin
          if pSearch.Visible then
            begin
              case Key of
              VK_PRIOR,
              VK_UP:
                begin
                  if lbResults.ItemIndex = -1 then
                    begin
                      lbResults.ItemIndex:=0;
                      pSearch.Visible := False;
                    end
                  else
                    begin
                      lbResults.ItemIndex:=lbResults.ItemIndex-1;
                      Key := 0;
                    end;
                end;
              VK_NEXT,
              VK_DOWN:
                begin
                  if lbResults.ItemIndex = -1 then
                    lbResults.ItemIndex:=0
                  else
                  if lbResults.ItemIndex < lbResults.Count-1 then
                    lbResults.ItemIndex:=lbResults.ItemIndex+1;
                  Key := 0;
                end;
              VK_RETURN:
                begin
                  Application.QueueAsyncCall(@DoInsertInplaceSearch,0);
                  Result := True;
                  Key := 0;
                end;
              VK_ESCAPE:
                begin
                  pSearch.Visible:=False;
                  Key := 0;
                end;
              end;
            end;
        end
      else
        begin
          if not pSearch.Visible then
            begin
              if tbLeft.Visible then
                pSearch.Left:=tbLeft.Width+X
              else pSearch.Left:=X;
              pSearch.Top:=Y;
              if FGridView.gHeader.Visible then
                pSearch.Top:=pSearch.Top+FGridView.gHeader.Height;
            end;
          if Assigned(ActiveSearch) then
            ActiveSearch.Abort;
          SearchTypes := SearchTypes+[fsShortnames];
          SearchTypes := SearchTypes+[fsIdents];
          SearchTypes := SearchTypes+[fsDescription];
          SetLength(SearchLocations,length(SearchLocations)+1);
          if (Field.FieldName='USER') or (Field.FieldName='OWNER') then
            SearchLocations[length(SearchLocations)-1] := strUsers;
          lbResults.Items.Clear;
          if Assigned(ActiveSearch) then
            ActiveSearch.Free;
          ActiveSearch := TSearch.Create(SearchTypes,SearchLocations,True,5);
          ActiveSearch.Sender := TComponent(Sender);
          ActiveSearch.OnItemFound:=@ActiveSearchItemFound;
          ActiveSearch.OnEndSearch:=@ActiveSearchEndSearch;
          ActiveSearch.Start(SearchString);
          Application.ProcessMessages;
        end;
    end;
end;

procedure TfMeetingFrame.FGridViewSetupPosition(Sender: TObject;
  Columns: TGridColumns);
var
  i: Integer;
begin
  for i := 0 to FGridView.Columns.Count-1 do
    begin
      if TColumn(FGridView.Columns[i]).FieldName = 'USER' then
        begin
          FGridView.Columns[i].ButtonStyle:=cbsEllipsis;
        end
      else if TColumn(FGridView.Columns[i]).FieldName = 'OWNER' then
        begin
          FGridView.Columns[i].ButtonStyle:=cbsEllipsis;
        end
      ;
    end;
end;

function TfMeetingFrame.fSearchOpenOwnerItem(aLink: string): Boolean;
var
  aCount: Integer;
  aUser: TUser;
begin
  Result := False;
  aUser := TUser.CreateEx(Self,Data);
  aUser.SelectFromLink(aLink);
  aUser.Open;
  Result := aUser.Count>0;
  pSearch.Visible:=False;
  if Result and FGridView.GotoActiveRow then
    begin
      if TMeetings(DataSet).Entrys.CanEdit then
        TMeetings(DataSet).Entrys.DataSet.Post;
      TMeetings(DataSet).Entrys.DataSet.Edit;
      TMeetings(DataSet).Entrys.FieldByName('OWNER').AsString := aUser.FieldByName('ACCOUNTNO').AsString;
      FGridView.SyncActiveRow(DataSet.GetBookmark,False,True,True);
      FGridView.gList.EditorMode:=False;
      FGridView.SetEdited;
    end
  else
    begin
      if TMeetings(DataSet).Entrys.CanEdit then
        TMeetings(DataSet).Entrys.DataSet.Post;
      TMeetings(DataSet).Entrys.DataSet.Edit;
      TMeetings(DataSet).Entrys.FieldByName('OWNER').Clear;
      FGridView.SyncActiveRow(DataSet.GetBookmark,False,True,True);
      FGridView.gList.EditorMode:=False;
      FGridView.SetEdited;
    end;
  aUser.Free;
end;

function TfMeetingFrame.fSearchOpenProjectItem(aLink: string): Boolean;
begin
  Result := True;
  if FGridView.DataSet.State<>dsInsert then
    FGridView.Append;
  FGridView.DataSet.FieldByName('DESC').AsString:=Data.GetLinkDesc(aLink);
  FGridView.DataSet.FieldByName('LINK').AsString:=aLink;
  FGridView.Post;
  FGridView.SyncActiveRow(FGridView.DataSet.Id.AsVariant,False,True);
  FGridView.SetFocus;
end;

function TfMeetingFrame.fSearchOpenUserItem(aLink: string): Boolean;
var
  aCount: Integer;
  aUser: TUser;
begin
  Result := False;
  aUser := TUser.CreateEx(Self,Data);
  aUser.SelectFromLink(aLink);
  aUser.Open;
  Result := aUser.Count>0;
  pSearch.Visible:=False;
  if Result and FGridView.GotoActiveRow then
    begin
      if TMeetings(DataSet).Entrys.CanEdit then
        TMeetings(DataSet).Entrys.DataSet.Post;
      TMeetings(DataSet).Entrys.DataSet.Edit;
      TMeetings(DataSet).Entrys.FieldByName('USER').AsString := aUser.FieldByName('ACCOUNTNO').AsString;
      FGridView.SyncActiveRow(TMeetings(DataSet).Entrys.GetBookmark,False,True,True);
      FGridView.gList.EditorMode:=False;
      FGridView.SetEdited;
    end
  else
    begin
      if TMeetings(DataSet).Entrys.CanEdit then
        TMeetings(DataSet).Entrys.DataSet.Post;
      TMeetings(DataSet).Entrys.DataSet.Edit;
      TMeetings(DataSet).Entrys.FieldByName('USER').Clear;
      FGridView.SyncActiveRow(DataSet.GetBookmark,False,True,True);
      FGridView.gList.EditorMode:=False;
      FGridView.SetEdited;
    end;
  aUser.Free;
end;

procedure TfMeetingFrame.lbResultsDblClick(Sender: TObject);
var
  Key: Word;
  Shift: TShiftState;
begin
  if lbResults.ItemIndex < 0 then exit;
  pSearch.Visible:=False;
  Application.ProcessMessages;
  if copy(TLinkObject(lbResults.Items.Objects[lbResults.ItemIndex]).Link,0,5) = 'USERS' then
    begin
      FGridView.BeginUpdate;
      if FGridView.Columns[FGridView.gList.Col-1].FieldName='USER' then
        fSearchOpenUserItem(TLinkObject(lbResults.Items.Objects[lbResults.ItemIndex]).Link)
      else
        fSearchOpenOwnerItem(TLinkObject(lbResults.Items.Objects[lbResults.ItemIndex]).Link);
      FGridView.EndUpdate;
    end
  ;
  pSearch.Visible:=False;
  FGridView.SetFocus;
  Key := VK_TAB;
  Shift := [];
  THackCustomGrid(FGridView.gList).KeyDown(Key,Shift);
  FGridView.gList.EditorMode:=True;
end;

procedure TfMeetingFrame.mShorttextChange(Sender: TObject);
var
  tmp: String;
begin
  TabCaption := eName.Text;
  acSave.Enabled := DataSet.CanEdit or DataSet.Changed;
  acCancel.Enabled:= DataSet.CanEdit or DataSet.Changed;
end;

procedure TfMeetingFrame.DoInsertInplaceSearch(Data: PtrInt);
begin
  lbResultsDblClick(nil);
end;

procedure TfMeetingFrame.ReportGetValue(const ParName: String;
  var ParValue: Variant);
var
  aKey: String;
begin
  if Uppercase(ParName) = 'USER' then
    begin
      aKey := StringReplace(TMeetings(FDataSet).Entrys.FieldByName('USER').AsString,'=','',[rfReplaceAll]);
      if trim(FOwners.Values[aKey]) = '' then
        FOwners.Values[aKey] := TMeetings(FDataSet).Entrys.UserName;
      ParValue := FOwners.Values[aKey]
    end
  else if Uppercase(ParName) = 'OWNER' then
    begin
      aKey := StringReplace(TMeetings(FDataSet).Entrys.FieldByName('OWNER').AsString,'=','',[rfReplaceAll]);
      if trim(FOwners.Values[aKey]) = '' then
        FOwners.Values[aKey] := TMeetings(FDataSet).Entrys.OwnerName;
      ParValue := FOwners.Values[aKey]
    end;
end;

procedure TfMeetingFrame.sbMenueClick(Sender: TObject);
begin
  TSpeedButton(Sender).PopupMenu.PopUp(TSpeedButton(Sender).ClientOrigin.x,TSpeedButton(Sender).ClientOrigin.y+TSpeedButton(Sender).Height);
end;

procedure TfMeetingFrame.acCancelExecute(Sender: TObject);
begin
  if Assigned(FConnection) then
    begin
      FDataSet.CascadicCancel;
      //Data.Rollback(FConnection);
      //Data.StartTransaction(FConnection);
    end;
end;

procedure TfMeetingFrame.acAddPosExecute(Sender: TObject);
begin
  FGridView.SetActive;
  FGridView.Insert;
  FGridView.gList.EditorMode:=True;
end;

procedure TfMeetingFrame.acAddTopicExecute(Sender: TObject);
var
  i: Integer;
begin
  fSearch.AllowSearchTypes(strProjects+','+strMasterdata);
  fSearch.eContains.Clear;
  fSearch.sgResults.RowCount:=1;
  fSearch.OnOpenItem:=@fSearchOpenProjectItem;
  fSearch.Execute(True,'MEETP',strSearchFromMeetings);
  fSearch.SetLanguage;
end;

procedure TfMeetingFrame.acAppendPosExecute(Sender: TObject);
begin
  FGridView.SetActive;
  FGridView.InsertAfter;
  FGridView.gList.EditorMode:=True;
end;

procedure TfMeetingFrame.acCloseExecute(Sender: TObject);
begin
  CloseFrame;
end;

procedure TfMeetingFrame.acDeleteExecute(Sender: TObject);
begin
  if MessageDlg(strRealdelete,mtInformation,[mbYes,mbNo],0) = mrYes then
    begin
      Screen.Cursor := crHourglass;
      DataSet.DataSet.Delete;
      FDataSet.CascadicCancel;
      //Data.Commit(FConnection);
      //Data.StartTransaction(FConnection);
      acClose.Execute;
      Screen.Cursor := crDefault;
    end;
end;

procedure TfMeetingFrame.acDelPosExecute(Sender: TObject);
begin
  if MessageDlg(strRealdelete,mtInformation,[mbYes,mbNo],0) = mrYes then
    FGridView.Delete;
end;

procedure TfMeetingFrame.acMAkeSubTaskExecute(Sender: TObject);
begin
  FGridView.SetChild;
end;

procedure TfMeetingFrame.acPermanentEditormodeExecute(Sender: TObject);
begin
  Application.ProcessMessages;
  if acPermanentEditormode.Checked then
    FGridView.gList.Options:=FGridView.gList.Options+[goAlwaysShowEditor]
  else
  FGridView.gList.Options:=FGridView.gList.Options-[goAlwaysShowEditor];
  with Application as IBaseDbInterface do
    begin
      if acPermanentEditorMode.Checked then
        DBConfig.WriteString('EMEEVIS','Y')
      else
        DBConfig.WriteString('EMEEVIS','');
    end;
end;

procedure TfMeetingFrame.acPrintExecute(Sender: TObject);
var
  Hist : IBaseHistory;
begin
  fSelectReport.Report := Report;
  PList.DataSet := DataSet.DataSet;
  fSelectReport.SetLanguage;
  acRefresh.Execute;
  Entrys.DataSet := TMeetings(DataSet).Entrys.DataSet;
  Users.DataSet := TMeetings(DataSet).Users.DataSet;
  MandantDetails.DataSet:=Data.MandantDetails.DataSet;
  Data.MandantDetails.Open;
  with FDataSet.DataSet as IBaseManageDB do
    begin
      fSelectReport.ReportType := 'MEE';
    end;
  fSelectReport.DataSet := DataSet;
  fSelectReport.SavePossible:=True;
  fSelectReport.Execute;
  if fSelectReport.Booked then
    begin
      if Assigned(FConnection) then
        begin
          FDataSet.CascadicPost;
          //Data.Commit(FConnection);
          //Data.StartTransaction(FConnection);
        end;
      SetRights;
    end;
end;

procedure TfMeetingFrame.acRefreshExecute(Sender: TObject);
begin
  FGridView.Refresh;
end;

procedure TfMeetingFrame.acRenumberExecute(Sender: TObject);
begin
  FGridView.RenumberRows;
  FGridView.Refresh(False);
end;

procedure TfMeetingFrame.acRestartExecute(Sender: TObject);
var
  bMeeting: TMeetings;
  aMeeting: TMeetings;
  aLink: String;
begin
  aMeeting := DataSet as TMeetings;
  aNewName := InputBox(strMeetingName,strEnterMeetingName,aMeeting.Text.AsString);
  bMeeting := TMeetings.Create(nil);
  bMeeting.ImportFromXML(aMeeting.ExportToXML,False,@ReplaceField);
  bMeeting.Edit;
  bMeeting.FieldByName('DATE').Clear;
  bMeeting.Post;
  aLink := Data.BuildLink(bMeeting.DataSet);
  Data.GotoLink(aLink);
  bMeeting.Free;
end;

procedure TfMeetingFrame.acRightsExecute(Sender: TObject);
begin
  fNRights.Execute(DataSet.Id.AsVariant);
end;

procedure TfMeetingFrame.DoOpen;
var
  aType: Char;
  aFound: Boolean;
  tmp: String;
  aDocuments: TDocuments;
  aDocFrame: TfDocumentFrame;
begin
  SetRights;
  Datasource.DataSet := DataSet.DataSet;
  if Data.Users.Rights.Right('OPTIONS') > RIGHT_READ then
    begin
      pcPages.CanHaveCustomTabs(@TBaseVisualApplication(Application).OnAddCustomTab);
    end;
  aType := 'E';
  if not Data.States.DataSet.Locate('TYPE;STATUS',VarArrayOf([aType,FDataSet.FieldByName('STATUS').AsString]),[loCaseInsensitive]) then
    begin
      Data.SetFilter(Data.States,'');
      aFound := Data.States.DataSet.Locate('TYPE;STATUS',VarArrayOf([aType,FDataSet.FieldByName('STATUS').AsString]),[loCaseInsensitive]);
    end
  else aFound := True;
  if aFound then
    begin
  cbStatus.Items.Add(Data.States.FieldByName('STATUSNAME').AsString+' ('+Data.States.FieldByName('STATUS').AsString+')');
  cbStatus.Text := Data.States.FieldByName('STATUSNAME').AsString+' ('+Data.States.FieldByName('STATUS').AsString+')';
    end
  else cbStatus.Text:=FDataSet.FieldByName('STATUS').AsString;
  tmp := trim(Data.States.FieldByName('DERIVATIVE').AsString);
  if (length(tmp) = 0) or (tmp[length(tmp)] <> ';') then
    tmp := tmp+';';
  if tmp <> ';' then
    begin
      while pos(';',tmp) > 0 do
        begin
          if Data.States.DataSet.Locate('TYPE;STATUS',VarArrayOf([aType,copy(tmp,0,pos(';',tmp)-1)]),[loCaseInsensitive]) then
            cbStatus.Items.Add(Data.States.FieldByName('STATUSNAME').AsString+' ('+Data.States.FieldByName('STATUS').AsString+')');
          tmp := copy(tmp,pos(';',tmp)+1,length(tmp));
        end;
    end
  else
    begin
      Data.SetFilter(Data.States,Data.QuoteField('TYPE')+'='+Data.QuoteValue(aType));
      with Data.States.DataSet do
        begin
          First;
          while not eof do
            begin
              if cbStatus.Items.IndexOf(Data.States.FieldByName('STATUSNAME').AsString+' ('+Data.States.FieldByName('STATUS').AsString+')') = -1 then
                cbStatus.Items.Add(Data.States.FieldByName('STATUSNAME').AsString+' ('+Data.States.FieldByName('STATUS').AsString+')');
              Next;
            end;
        end;
    end;
  pcPages.AddTabClass(TfMeetingUsers,strMeetingUsers,@AddUsers);
  TMeetings(DataSet).Users.Open;
  if TMeetings(DataSet).Users.Count > 0 then
    pcPages.AddTab(TfMeetingUsers.Create(Self),False);
  TMeetings(DataSet).Entrys.Open;
  with FGridView do
    begin
      BaseName:='PMEMIS';
      DefaultRows:='GLOBALWIDTH:%;DESC:300;OWNER:70;USER:70;DUEDATE:60;POSNO:40;';
      Align := alClient;
      SortField:='TIMESTAMPD';
      //ShortTextField:='DESC';
      TextField:='DESC';
      IdentField:='DESC';
      TreeField:='PARENT';
      ExpandedField:='EXPANDED';
      SortField:='POSNO';
      NumberField:='POSNO';
      HasChildsField:='HASCHILDS';
      SortDirection:=TSortDirection.sdAscending;
      ReadOnly:=False;
      OnDblClick:=@FGridViewDblClick;
    end;
  FGridView.OnSetupPosition:=@FGridViewSetupPosition;
  FGridView.OnCellButtonClick:=@FGridViewCellButtonClick;
  FGridView.OnGetCellText:=@FGridViewGetCellText;
  FGridView.OnSearchKey:=@FGridViewSearchKey;
  FGridView.WordWrap:=True;
  FGridView.DataSet := TMeetings(DataSet).Entrys;
  FGridView.gList.Col:=1;
  FGridView.gList.Col:=0;
  pcPages.AddTabClass(TfDocumentFrame,strFiles,@AddDocuments);
  if (FDataSet.State <> dsInsert) and (fDataSet.Count > 0) then
    begin
      aDocuments := TDocuments.CreateEx(Self,Data);
      aDocuments.CreateTable;
      aDocuments.Select(DataSet.Id.AsInteger,'E',DataSet.Id.AsString,Null,Null);
      aDocuments.Open;
      if aDocuments.Count = 0 then
        aDocuments.Free
      else
        begin
          aDocFrame := TfDocumentFrame.Create(Self);
          pcPages.AddTab(aDocFrame,False);
          aDocFrame.DataSet := aDocuments;
        end;
    end;
  pcPages.AddTabClass(TfLinkFrame,strLinks,@AddLinks);
  TMeetings(DataSet).Links.Open;
  if TMeetings(DataSet).Links.Count > 0 then
    pcPages.AddTab(TfLinkFrame.Create(Self),False);
  with Application as TBaseVisualApplication do
    AddTabClasses('MEE',pcPages);
  Entrys.DataSet := TMeetings(DataSet).Entrys.DataSet;
  with Application as IBaseDBInterface do
    acPermanentEditormode.Checked:=DBConfig.ReadString('EMEEVIS','N') = 'Y';
  bpermanenetEditor.Down:=acPermanentEditormode.Checked;
  acPermanentEditormodeExecute(nil);
  inherited DoOpen;
end;

function TfMeetingFrame.SetRights: Boolean;
begin
  FEditable := DataSet.FieldByName('DATE').IsNull;
  FGridView.SetRights(DataSet.FieldByName('DATE').IsNull);
  eName.ReadOnly:=not DataSet.FieldByName('DATE').IsNull;
  Memo1.ReadOnly:=not DataSet.FieldByName('DATE').IsNull;
  acAddPos.Enabled:=DataSet.FieldByName('DATE').IsNull;
  acAppendPos.Enabled:=DataSet.FieldByName('DATE').IsNull;
  acAddTopic.Enabled:=DataSet.FieldByName('DATE').IsNull;
  acDelPos.Enabled:=DataSet.FieldByName('DATE').IsNull;
  acMAkeSubTask.Enabled:=DataSet.FieldByName('DATE').IsNull;
  acUnmakeSubTask.Enabled:=DataSet.FieldByName('DATE').IsNull;
  dtStart.Enabled:=DataSet.FieldByName('DATE').IsNull;
  dtEnd.Enabled:=DataSet.FieldByName('DATE').IsNull;
  cbStatus.Enabled:=DataSet.FieldByName('DATE').IsNull;
  acDelete.Enabled:=DataSet.FieldByName('DATE').IsNull;
  bRenumber.Enabled:=DataSet.FieldByName('DATE').IsNull;
  acSetTopic.Enabled:=DataSet.FieldByName('DATE').IsNull;
end;

procedure TfMeetingFrame.AddDocuments(Sender: TObject);
var
  aDocuments: TDocuments;
begin
  if not Assigned(TfDocumentFrame(Sender).DataSet) then
    begin
      aDocuments := TDocuments.CreateEx(Self,Data);
      TfDocumentFrame(Sender).DataSet := aDocuments;
      TfDocumentFrame(Sender).Refresh(DataSet.Id.AsVariant,'E',DataSet.Id.AsString,Null,Null);
    end;
  TPrometInplaceFrame(Sender).SetRights(FEditable);
end;

procedure TfMeetingFrame.AddLinks(Sender: TObject);
begin
  TfLinkFrame(Sender).BaseName:='ME';
  TfLinkFrame(Sender).DataSet := TMeetings(FDataSet).Links;
  TPrometInplaceFrame(Sender).SetRights(FEditable);
end;

procedure TfMeetingFrame.AddUsers(Sender: TObject);
begin
  TfLinkFrame(Sender).DataSet := TMeetings(FDataSet).Users;
  TPrometInplaceFrame(Sender).SetRights(FEditable);
end;

constructor TfMeetingFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOwners := TStringList.Create;
  FGridView := TfGridView.Create(Self);
  FGridView.Parent := tsMeeting;
  FGridView.Align:=alClient;
  FGridView.OnDragOver:=@FGridViewDragOver;
  FGridView.OnDragDrop:=@FGridViewDragDrop;
end;

function TfMeetingFrame.OpenFromLink(aLink: string): Boolean;
begin
  if not (copy(aLink,0,pos('@',aLink)-1) = 'MEETINGS') then exit;
  if rpos('{',aLink) > 0 then
    aLink := copy(aLink,0,rpos('{',aLink)-1)
  else if rpos('(',aLink) > 0 then
    aLink := copy(aLink,0,rpos('(',aLink)-1);
  CloseConnection;
  if not Assigned(FConnection) then
    FConnection := Data.GetNewConnection;
  //Data.StartTransaction(FConnection);
  DataSet := TMeetings.CreateEx(Self,Data,FConnection);
  DataSet.OnChange:=@DataSetChange;
  Data.SetFilter(FDataSet,Data.QuoteField('SQL_ID')+'='+Data.QuoteValue(copy(aLink,pos('@',aLink)+1,length(aLink))),1);
  if FDataSet.Count > 0 then
    begin
      TabCaption := TMeetings(FDataSet).Text.AsString;
      DoOpen;
      Result := True;
    end;
end;

procedure TfMeetingFrame.New;
var
  aFrame: TfMeetingUsers;
begin
  CloseConnection;
  if not Assigned(FConnection) then
    FConnection := Data.GetNewConnection;
  //Data.StartTransaction(FConnection);
  DataSet := TMeetings.CreateEx(Self,Data,FConnection);
  DataSet.OnChange:=@DataSetChange;
  DataSet.Select(0);
  DataSet.Open;
  DataSet.DataSet.Insert;
  DoOpen;
  acSave.Enabled := False;
  acCancel.Enabled:= False;
  aFrame := TfMeetingUsers.Create(Self);
  pcPages.AddTab(aFrame,True,strMeetingUsers);
  TabCaption:=strNewMeeting;
end;

destructor TfMeetingFrame.Destroy;
begin
  if Assigned(FConnection) then
    begin
      CloseConnection(acSave.Enabled);
      DataSet.Destroy;
      DataSet := nil;
      FreeAndNil(FConnection);
    end;
  FOwners.Free;
  inherited Destroy;
end;

procedure TfMeetingFrame.ShowFrame;
begin
  inherited ShowFrame;
  FGridView.SetActive;
end;

procedure TfMeetingFrame.SetLanguage;
begin

end;

{$R *.lfm}

end.

