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
*******************************************************************************}
unit uMainTreeFrame;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, FileUtil,  Forms, Controls, ComCtrls, ActnList,
  Menus, ExtCtrls, uBaseDBInterface, uBaseDbClasses,uExtControls, db;
type
  TEntryTyp = (etNone,etAction,etDir,
               etFavourites,etLink,
               etMasterdata,
               etCustomer,etSupplier,etCustomers,etCustomerList,etEmployee,etDepartment,
               etArticle,
                 etArticleList,
               etProjects,
                 etProject,etProcess,
               etTasks,
                 etMyTasks,
                 etTaskDepartment,
                 etTaskUser,
                 etTaskPlan,
               etCalendar,
                 etMyCalendar,
                 etCalendarDepartment,
                 etCalendarUser,
                 etCalendarDir,
                 etAttPlan,
               etMessages,etMessageDir,etMessageBoard,
               etTimeRegistering,
               etOrders,
                 etOrderList,
                 etNewOrder,
               etSearch,
               etFinancial,
                 etBanking,
                   etAccounts,etAccount,etNewAccount,etAccountlist,
                   etNewTransfer,
                   etAccountingQue,
                 etSalesList,
               etFiles,
               etDocuments,
               etImages,
               etDocumentDir,
               etLists,
               etStorage,
                 etStoragejournal,
                 etInventory,
                 etWebshop,
                 etDisposition,
               etStatistics,
                 etStatistic,
               etMeetings,
                 etMeetingList,
               etWiki,
                 etWikiPage,
               etClipboard,
                 etClipboardItem,
               etAllObjects
               );
  TTreeEntry = class
  public
    Rec : Int64;
    Filter : string;
    Link : string;
    DataSource : TBaseDbDataSet;
    DataSourceType : TBaseDbDataSetClass;
    Typ : TEntryTyp;
    Text : array[0..2] of string;
    SubText : TStrings;
    Obj : TObject;
    Action : TAction;
    LinkIcon : Integer;
    constructor Create;
    destructor Destroy;override;
  end;
  TSelectionChangedEvent = procedure(aEntry : TTreeEntry) of object;
  TOpenEvent = function(aEntry : TTreeEntry) : Boolean of object;
  TLinkEvent = function(aLink : string;aSender : TObject) : Boolean of object;
  TNewLinkEvent = function(aLink : string;aSender : TObject) : TBaseDBDataSet of object;

  { TfMainTree }

  TfMainTree = class(TFrame)
    acOpen: TAction;
    acAddDirectory: TAction;
    acRenameDirectory: TAction;
    acDeleteDirectory: TAction;
    acSearch: TAction;
    acAddBoard: TAction;
    acRights: TAction;
    acDeleteLink: TAction;
    acRestoreStandard: TAction;
    acCopyAsLink: TAction;
    acPasteLink: TAction;
    ActionList1: TActionList;
    DblClickTimer: TIdleTimer;
    MenuItem1: TMenuItem;
    pmTree: TPopupMenu;
    tvMain: TTreeView;
    procedure acAddBoardExecute(Sender: TObject);
    procedure acAddDirectoryExecute(Sender: TObject);
    procedure acCopyAsLinkExecute(Sender: TObject);
    procedure acDeleteDirectoryExecute(Sender: TObject);
    procedure acDeleteLinkExecute(Sender: TObject);
    procedure acHideEntryExecute(Sender: TObject);
    procedure acOpenExecute(Sender: TObject);
    procedure acPasteLinkExecute(Sender: TObject);
    procedure acRenameDirectoryExecute(Sender: TObject);
    procedure acRestoreStandardExecute(Sender: TObject);
    procedure acRightsExecute(Sender: TObject);
    procedure acSearchExecute(Sender: TObject);
    procedure DblClickTimerTimer(Sender: TObject);
    procedure DeleteNodeCall(Data: PtrInt);
    procedure tvMainAdvancedCustomDrawItem(Sender: TCustomTreeView;
      Node: TTreeNode; State: TCustomDrawState; Stage: TCustomDrawStage;
      var PaintImages, DefaultDraw: Boolean);
    procedure tvMainClick(Sender: TObject);
    procedure tvMainDblClick(Sender: TObject);
    procedure tvMainDeletion(Sender: TObject; Node: TTreeNode);
    procedure tvMainDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure tvMainDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure tvMainEdited(Sender: TObject; Node: TTreeNode; var S: string);
    procedure tvMainEditing(Sender: TObject; Node: TTreeNode;
      var AllowEdit: Boolean);
    procedure tvMainExpanded(Sender: TObject; Node: TTreeNode);
    procedure tvMainExpanding(Sender: TObject; Node: TTreeNode;
      var AllowExpansion: Boolean);
    procedure tvMainKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure tvMainSelectionChanged(Sender: TObject);
  private
    FDragDrop: TDragDropEvent;
    FNode : TTreeNode;
    FDragOver: TDragOverEvent;
    FLinkNew: TNewLinkEvent;
    FLinkOpen: TLinkEvent;
    FOpen: TOpenEvent;
    FSearchOptions: string;
    FSelChanged: TSelectionChangedEvent;
    FSerachOptions: string;
    FLastSelected: TTreeEntry;
    { private declarations }
  public
    { public declarations }
    StartupTypes : TStringList;
    pcPages : TExtMenuPageControl;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function OpenLink(aLink: string; Sender: TObject): Boolean;
    function NewFromLink(aLink: string; Sender: TObject): TBaseDBdataSet;
    function GetTreeEntry : Variant;
    function GetNodeText(aNode : TTreeNode) : string;
    function GetEntryText(aNode : TEntryTyp) : string;
    function GetBigIconTexts : string;
    procedure SaveTreeOptions;
    procedure RestoreExpands;
    property SearchOptions : string read FSearchOptions write FSerachOptions;
    property OnSelectionChanged : TSelectionChangedEvent read FSelChanged write FSelChanged;
    property OnOpen : TOpenEvent read FOpen write FOpen;
    property OnOpenFromLink : TLinkEvent read FLinkOpen write FLinkOpen;
    property OnNewFromLink : TNewLinkEvent read FLinkNew write FLinkNew;
    property OnDragOver : TDragOverEvent read FDragOver write FDragOver;
    property OnDragDrop : TDragDropEvent read FDragDrop write FDragDrop;
  end;
var
  fMainTreeFrame : TfMainTree;


implementation
{$R *.lfm}
uses uData,uPrometFrames,LCLType,Dialogs,uIntfStrConsts, FPCanvas,
  uBaseVisualControls, Graphics, Utils,UtilsVis, LCLProc, uPerson,uMasterdata,uProjects,
  uWiki,uSearch,Themes,uFilterFrame,uNRights,uStatistic,uClipp,Clipbrd,
  uBaseVisualApplication,uError,uBaseApplication;
resourcestring
  strRestartNessesary                         = 'Starten Sie die Anwendung neu !';
  strRealMove                                 = 'Verzeichnis wirklich nach "%s" verschieben ?';
constructor TTreeEntry.Create;
begin
  Action := nil;
  SubText := nil;
  LinkIcon:=-1;
end;
destructor TTreeEntry.Destroy;
begin
  if Assigned(SubText) then SubText.Free;
  inherited Destroy;
end;
procedure TfMainTree.tvMainSelectionChanged(Sender: TObject);
var
  DataT: TTreeEntry;
  Found: Boolean = False;
  aFrame: TPrometMainFrame;
  i: Integer;
  New: TMenuItem;
  Typ: String;
  tmp: String;
begin
  if not Assigned(tvMain.Selected) then
    begin
      pmTree.Items.Clear;
      New := TMenuItem.Create(pmTree);
      New.Action := acRestoreStandard;
      pmTree.Items.Add(New);
      exit;
    end;
  DataT := TTreeEntry(tvMain.Selected.Data);
  if not Assigned(DataT) then
    begin
      FLastSelected :=nil;
      exit;
    end;
  if Assigned(FSelChanged) then
    FSelChanged(DataT);
  if FLastSelected <> DataT then
    pmTree.Items.Clear
  else exit;
  FLastSelected := DataT;
  if  Assigned(DataT) then
    begin
      if DataT.Typ = etDir then
        begin
          Data.SetFilter(Data.Tree,'',0,'','ASC',False,True,True);
          Data.Tree.GotoBookmark(DataT.Rec);
          Typ := Data.Tree.FieldByName('TYPE').AsString;
        end;
      if ((DataT.Typ = etDir)
      or (DataT.Typ = etCustomers)
      or (DataT.Typ = etCalendar)
      or (DataT.Typ = etCalendarDir)
      or (DataT.Typ = etMasterdata)
      or (DataT.Typ = etProjects)
      or (DataT.Typ = etStatistics)
      or (DataT.Typ = etWiki)
      or (DataT.Typ = etMessages)
      or (DataT.Typ = etMessageDir)
      or (DataT.Typ = etMessageBoard)
      or (DataT.Typ = etFavourites)
      or (DataT.Typ = etDocuments)
      or (DataT.Typ = etImages)
      or (DataT.Typ = etDocumentDir)
      or (DataT.Typ = etClipboard)
      )
      then
        begin
          New := TMenuItem.Create(pmTree);
          New.Action := acAddDirectory;
          acAddDirectory.Enabled:=(Data.Users.Rights.Right('TREE') > RIGHT_READ) or (DataT.Typ = etFavourites) or (Typ='F');
          pmTree.Items.Add(New);
        end;
      if (DataT.Typ = etMessages)
      or (DataT.Typ = etMessageDir)
      or (DataT.Typ = etMessageBoard)
      then
        begin
          New := TMenuItem.Create(pmTree);
          New.Action := acAddBoard;
          acAddBoard.Enabled:=Data.Users.Rights.Right('TREE') > RIGHT_READ;
          pmTree.Items.Add(New);
        end;
      if (DataT.Typ = etDir)
      or (DataT.Typ = etMessageDir)
      or (DataT.Typ = etMessageBoard)
      or (DataT.Typ = etDocumentDir)
      or (DataT.Typ = etCalendarDir)
      then
        begin
          New := TMenuItem.Create(pmTree);
          New.Action := acRenameDirectory;
          acRenameDirectory.Enabled:=(Data.Users.Rights.Right('TREE') > RIGHT_READ) or (DataT.Typ = etFavourites) or (Typ='F');
          pmTree.Items.Add(New);
          New := TMenuItem.Create(pmTree);
          New.Action := acDeleteDirectory;
          acDeleteDirectory.Enabled:=(Data.Users.Rights.Right('TREE') > RIGHT_WRITE) or (DataT.Typ = etFavourites) or (Typ='F');
          pmTree.Items.Add(New);
        end;
      if (DataT.Typ = etDir)
      or (DataT.Typ = etMessageDir)
      or (DataT.Typ = etMessageBoard)
      or (DataT.Typ = etStatistic)
      or (DataT.Typ = etDocumentDir)
      or (DataT.Typ = etCalendarDir)
      then
        begin
          New := TMenuItem.Create(pmTree);
          New.Action := acRights;
          acRights.Enabled:= ((Data.Users.Rights.Right('TREE') >= RIGHT_PERMIT))
             or (DataT.Typ = etFavourites) or (Typ='F')
             or ((DataT.Typ = etStatistic)  and (Data.Users.Rights.Right('STATISTICS') >= RIGHT_PERMIT))
             ;
          pmTree.Items.Add(New);
        end;
      if (DataT.Typ = etDir) and (Typ='F') then
        begin
          New := TMenuItem.Create(pmTree);
          New.Action := acPasteLink;
          pmTree.Items.Add(New);
          tmp := ClipBoard.AsText;
          acPasteLink.Enabled:=(Clipboard.HasFormat(LinkClipboardFormat) or (pos('://',tmp) > 0));
        end;
      case DataT.Typ of
      etArticle,
      etProject,etProcess,
      etCustomer,etSupplier,
      etStatistic,
      etWikiPage,
      etClipboardItem,
      etLink:
        begin
          New := TMenuItem.Create(pmTree);
          New.Action := acCopyAsLink;
          pmTree.Items.Add(New);
        end;
      end;
      if (DataT.Typ = etLink)
      then
        begin
          New := TMenuItem.Create(pmTree);
          New.Action := acDeleteLink;
          pmTree.Items.Add(New);
        end;
    end
  else if Assigned(DataT) and ((DataT.Typ = etLink)) then
    begin
      New := TMenuItem.Create(pmTree);
      New.Action := acDeleteLink;
      pmTree.Items.Add(New);
    end;
  if tvMain.Selected.Level=0 then
    begin
      if pmTree.Items.Count>0 then
        begin
          New := TMenuItem.Create(pmTree);
          New.Caption:='-';
        end;
      {
      New := TMenuItem.Create(pmTree);
      New.Action := acHideEntry;
      pmTree.Items.Add(New);
      }
    end;
end;
constructor TfMainTree.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSearchOptions:='MAIN';
  StartupTypes := TStringList.Create;
end;

destructor TfMainTree.Destroy;
begin
  StartupTypes.Destroy;
  inherited Destroy;
end;

function TfMainTree.OpenLink(aLink: string; Sender: TObject): Boolean;
begin
  with BaseApplication as IBaseApplication do
    Info('OpenLink:'+aLink);
  Result := False;
  if Assigned(FLinkOpen) then
    Result := FLinkOpen(aLink,Sender);
end;
function TfMainTree.NewFromLink(aLink: string; Sender: TObject
  ): TBaseDBdataSet;
begin
  with BaseApplication as IBaseApplication do
    Info('NewFromLink:'+aLink);
  result := nil;
  if Assigned(FLinkNew) then
    Result := FLinkNew(aLink,Sender);
end;
procedure TfMainTree.tvMainKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_RETURN then
    begin
      acOpen.Execute;
    end;
end;
procedure TfMainTree.acOpenExecute(Sender: TObject);
var
  DataT: TTreeEntry;
begin
  if not Assigned(tvMain.Selected) then exit;
  DataT := TTreeEntry(tvMain.Selected.Data);
  case DataT.Typ of
  etAction:
    begin
      DataT.Action.Execute;
      with BaseApplication as IBaseApplication do
        Info('Action Open:'+DataT.Action.Name);
    end;
  etSearch:
    begin
      acSearch.Execute;
      with BaseApplication as IBaseApplication do
        Info('Action Open:'+acSearch.Name);
    end
  else
    begin
      if Assigned(FOpen) then
        FOpen(DataT);
      with BaseApplication as IBaseApplication do
        Info('Open:'+DataT.Link);
    end;
  end;
  Screen.Cursor:=crDefault;
end;
procedure TfMainTree.acPasteLinkExecute(Sender: TObject);
var
  DataT2: TTreeEntry;
  Stream: TStringStream;
  aLinks: String;
  aLink: String;
  aLinkDesc: String;
  aIcon: Integer;
  addToLinked: Boolean;
  aDS: TLinks;
begin
  DataT2 := TTreeEntry(tvMain.Selected.Data);
  if DataT2.Typ<>etDir then exit;
  Data.SetFilter(Data.Tree,'',0,'','ASC',False,True,True);
  if Data.Tree.GotoBookmark(DataT2.Rec) then
    begin
      aDS := TLinks.Create(nil);
      Stream := TStringStream.Create('');
      if (pos('://',ClipBoard.AsText) > 0) then
        begin
          aLinks := ClipBoard.AsText;
          if copy(aLinks,length(aLinks)-1,1)<>';' then aLinks := aLinks+';';
          while pos(';',aLinks)>0 do
            begin
              aLink := copy(aLinks,0,pos(';',aLinks)-1);
              aLinks := copy(aLinks,pos(';',aLinks)+1,length(aLinks));
              aLinkDesc := aLink;
              aIcon := Data.GetLinkIcon(aLink);
              with aDS do
                begin
                  Insert;
                  FieldByName('RREF_ID').AsVariant := Data.Tree.Id.AsVariant;
                  FieldByName('LINK').AsString := aLink;
                  FieldByName('NAME').AsString := aLinkDesc;
                  FieldByName('ICON').AsInteger := aIcon;
                  FieldByName('CHANGEDBY').AsString := Data.Users.IDCode.AsString;
                  Post;
                end;
            end;
        end
      else if Clipboard.GetFormat(LinkClipboardFormat,Stream) then
        begin
          Stream.Position:=0;
          aLinks := Stream.DataString;
        end
      else
        fError.ShowWarning(strCantgetClipboardContents);
      Stream.Free;
      addToLinked := False;
      while pos(';',aLinks) > 0 do
        begin
          aLink := copy(aLinks,0,pos(';',aLinks)-1);
          aLinks := copy(aLinks,pos(';',aLinks)+1,length(aLinks));
          aLinkDesc := Data.GetLinkDesc(aLink);
          aIcon := Data.GetLinkIcon(aLink);
          with aDS do
            begin
              Insert;
              FieldByName('RREF_ID').AsVariant := Data.Tree.Id.AsVariant;
              FieldByName('LINK').AsString := aLink;
              FieldByName('NAME').AsString := aLinkDesc;
              FieldByName('ICON').AsInteger := aIcon;
              FieldByName('CHANGEDBY').AsString := Data.Users.IDCode.AsString;
              Post;
            end;
        end;
      aDS.Free;
    end;
end;
procedure TfMainTree.acRenameDirectoryExecute(Sender: TObject);
var
  DataT : TTreeEntry;
  s: String;
  aTree: TTree;
begin
  DataT := TTreeEntry(tvMain.Selected.Data);
  if not Assigned(DataT) then exit;
  Data.SetFilter(Data.Tree,'',0,'','ASC',False,True,True);
  if Data.GotoBookmark(Data.Tree,DataT.Rec) then
    begin
      s := InputBox(strRename,strNewName,Data.Tree.FieldByName('NAME').AsString);
      aTree := TTree.CreateEx(Self,Data);
      Data.SetFilter(aTree,'');
      aTree.DataSet.Edit;
      aTree.FieldByName('NAME').AsString := S;
      aTree.DataSet.Post;
      aTree.Free;
      DataT.Text[0] := S;
    end;
end;

procedure TfMainTree.acRestoreStandardExecute(Sender: TObject);
begin
  with Application as IBaseDBInterface do
    DBConfig.WriteString('TREEENTRYS:'+ApplicationName,GetBigIconTexts);
  Showmessage(strRestartNessesary);
end;

procedure TfMainTree.acRightsExecute(Sender: TObject);
var
  DataT: TTreeEntry;
  aDataSet: TBaseDBDataset;
begin
  DataT := TTreeEntry(tvMain.Selected.Data);
  if not Assigned(DataT) then exit;
  if (DataT.Typ = etDir)
  or (DataT.Typ = etMessageDir)
  or (DataT.Typ = etMessageBoard)
  or (DataT.Typ = etCalendarDir)
  or (DataT.Typ = etDocumentDir)
  then
    begin
      Data.SetFilter(Data.Tree,'',0,'','ASC',False,True,True);
      Data.GotoBookmark(Data.Tree,DataT.Rec);
      fNRights.Execute(Data.Tree.Id.AsVariant);
    end
  else if (DataT.Typ = etStatistic)
  then
    begin
      aDataSet := DataT.DataSourceType.CreateEx(Self,Data);
      with aDataSet.DataSet as IBaseDBFilter do
        Filter := DataT.Filter;
      aDataSet.Open;
      if aDataSet.Count > 0 then
        fNRights.Execute(aDataSet.Id.AsVariant);
      aDataSet.Free;
    end;
end;

procedure TfMainTree.acSearchExecute(Sender: TObject);
begin
  fSearch.SetLanguage;
  fSearch.OnOpenItem:=nil;
  fSearch.Execute(False,FSearchOptions,'');
end;
procedure TfMainTree.acAddDirectoryExecute(Sender: TObject);
var
  ParentID : string;
  Typ : string;
  DataT : TTreeEntry;
  Node1: TTreeNode;
  aRights: TPermissions;
  NewTyp: TEntryTyp = etDir;
begin
  DataT := TTreeEntry(tvMain.Selected.Data);
  if not Assigned(DataT) then exit;
  Data.SetFilter(Data.Tree,'',0,'','ASC',False,True,False);
  if (DataT.Typ = etDir)
  or (DataT.Typ = etMessageDir)
  or (DataT.Typ = etMessageBoard)
  or (DataT.Typ = etCalendarDir)
  then
    begin
      Data.GotoBookmark(Data.Tree,DataT.Rec);
      ParentID := Data.Tree.Id.AsString;
      Typ := Data.Tree.FieldByName('TYPE').AsString;
    end
  else if (DataT.Typ = etCustomers) then
    begin
      ParentID := '0';
      Typ := 'C';
    end
  else if (DataT.Typ = etMessages) then
    begin
      ParentID := '0';
      Typ := 'N';
    end
  else if (DataT.Typ = etMasterdata) then
    begin
      ParentID := '0';
      Typ := 'M';
    end
  else if (DataT.Typ = etProjects) then
    begin
      ParentID := '0';
      Typ := 'P';
    end
  else if (DataT.Typ = etWiki) then
    begin
      ParentID := '0';
      Typ := 'W';
    end
  else if (DataT.Typ = etStatistics) then
    begin
      ParentID := '0';
      Typ := 'S';
    end
  else if (DataT.Typ = etCalendar)
       then
    begin
      ParentID := '0';
      Typ := 'A';
      NewTyp := etCalendarDir;
    end
  else if (DataT.Typ = etFavourites) then
    begin
      ParentID := '0';
      Typ := 'F';
    end
  else if (DataT.Typ = etClipboard) then
    begin
      ParentID := '0';
      Typ := 'Z';
    end
  else if (DataT.Typ = etDocuments)
       or (DataT.Typ = etImages)
       or (DataT.Typ = etDocumentDir)
       then
    begin
      Data.GotoBookmark(Data.Tree,DataT.Rec);
      ParentID := Data.Tree.Id.AsString;
      Typ := Data.Tree.FieldByName('TYPE').AsString;
      if DataT.Typ=etImages then
        Typ := 'I'
      else
        Typ := 'D';
      NewTyp := etDocumentDir;
    end
  else
    ParentID := '0';
  Data.Tree.Append;
  Data.Tree.FieldByName('PARENT').AsString := ParentID;
  Data.Tree.FieldByName('NAME').AsString := InputBox(strName,strName,strNewDir);
  Data.Tree.FieldByName('TYPE').AsString := Typ;
  Data.Tree.dataSet.Post;
  if tvMain.Selected.Expanded or (DataT.Typ <> etDir) then
    begin
      Node1 := tvMain.Items.AddChildObject(tvMain.Selected,'',TTreeEntry.Create);
      TTreeEntry(Node1.Data).Rec := Data.GetBookmark(Data.Tree);
      TTreeEntry(Node1.Data).DataSource := Data.Tree;
      TTreeEntry(Node1.Data).Text[0] := Data.Tree.FieldByName('NAME').AsString;
      TTreeEntry(Node1.Data).Typ := newTyp;
      if Typ = 'N' then
        TTreeEntry(Node1.Data).Typ := etMessageDir;
      tvMain.Items.AddChildObject(Node1,'',TTreeEntry.Create);
    end;
  if (DataT.Typ = etFavourites) or ((Typ = 'F') and (ParentID <> '0')) then //Add user right
    begin
      Data.Permissions.Open;
      if ParentID <> '0' then
        begin
          aRights := TPermissions.CreateEx(Self,Data);
          Data.SetFilter(aRights,Data.QuoteField('REF_ID_ID')+'='+Data.QuoteValue(ParentID));
          with aRights.DataSet do
            begin
              While not EOF do
                begin
                  Data.Permissions.DataSet.Insert;
                  Data.Permissions.FieldByName('REF_ID_ID').AsVariant:=Data.Tree.Id.AsVariant;
                  Data.Permissions.FieldByName('USER').AsString:=FieldByName('USER').AsString;
                  Data.Permissions.FieldByName('RIGHT').AsString:=FieldByName('RIGHT').AsString;
                  Data.Permissions.DataSet.Post;
                  Next;
                end;
            end;
        end
      else
        begin
          Data.Permissions.DataSet.Insert;
          Data.Permissions.FieldByName('REF_ID_ID').AsVariant:=Data.Tree.Id.AsVariant;
          Data.Permissions.FieldByName('USER').AsString:=Data.Users.FieldByName('SQL_ID').AsString;
          Data.Permissions.FieldByName('RIGHT').AsString:='4';
          Data.Permissions.DataSet.Post;
        end;
    end;
  Data.SetFilter(Data.Tree,'',0,'','ASC',False,True,True);
end;

procedure TfMainTree.acCopyAsLinkExecute(Sender: TObject);
var
  aNode: TTreeNode;
  DataT: TTreeEntry;
  aDataSet: TBaseDBDataset;
  Stream: TStringStream;
  aLinks: TLinks;
begin
  aNode := tvMain.Selected;
  DataT := TTreeEntry(tvMain.Selected.Data);
  if not Assigned(DataT) then exit;
  if DataT.Typ = etLink then
    begin
      aLinks := TLinks.Create(nil);
      aLinks.Select(DataT.Rec);
      aLinks.Open;
      if aLinks.Count>0 then
        begin
          Stream := TStringStream.Create(aLinks.FieldByName('LINK').AsString);
          Clipboard.AddFormat(LinkClipboardFormat,Stream);
          Stream.Free;
        end;
      aLinks.Free;
    end
  else
    begin
      aDataSet := DataT.DataSourceType.CreateEx(Self,Data);
      aDataSet.ActualFilter := DataT.Filter;
      aDataSet.Open;
      if aDataSet.Count > 0 then
        begin
          Stream := TStringStream.Create(Data.BuildLink(aDataSet.DataSet));
          Clipboard.AddFormat(LinkClipboardFormat,Stream);
          Stream.Free;
        end;
      aDataSet.Free;
    end;
end;

procedure TfMainTree.acAddBoardExecute(Sender: TObject);
var
  ParentID : string;
  Typ : string;
  DataT : TTreeEntry;
  Node1: TTreeNode;
begin
  DataT := TTreeEntry(tvMain.Selected.Data);
  if not Assigned(DataT) then exit;
  if  (DataT.Typ <> etMessageDir)
  and (DataT.Typ <> etMessageBoard)
  and (DataT.Typ <> etMessages)
  then exit;
  Data.SetFilter(Data.Tree,'',0,'','ASC',False,True,False);
  Typ := 'B';
  if (DataT.Typ = etDir)
  or (DataT.Typ = etMessageDir)
  or (DataT.Typ = etMessageBoard)
  then
    begin
      Data.GotoBookmark(Data.Tree,DataT.Rec);
      ParentID := Data.Tree.Id.AsString;
    end
  else
    ParentID := '0';
  Data.Tree.DataSet.Last;
  Data.Tree.DataSet.Append;
  Data.Tree.FieldByName('PARENT').AsString := ParentID;
  Data.Tree.FieldByName('NAME').AsString := InputBox(strName,strName,strNewDir);
  Data.Tree.FieldByName('TYPE').AsString := Typ;
  Data.Tree.dataSet.Post;
  if tvMain.Selected.Expanded or (DataT.Typ <> etDir) then
    begin
      Node1 := tvMain.Items.AddChildObject(tvMain.Selected,'',TTreeEntry.Create);
      TTreeEntry(Node1.Data).Rec := Data.GetBookmark(Data.Tree);
      TTreeEntry(Node1.Data).DataSource := Data.Tree;
      TTreeEntry(Node1.Data).Text[0] := Data.Tree.FieldByName('NAME').AsString;
      TTreeEntry(Node1.Data).Typ := etDir;
      if Typ = 'B' then
        TTreeEntry(Node1.Data).Typ := etMessageBoard;
      tvMain.Items.AddChildObject(Node1,'',TTreeEntry.Create);
    end;
end;

procedure TfMainTree.acDeleteDirectoryExecute(Sender: TObject);
var
  DataT : TTreeEntry;
  aNode: TTreeNode;
  aTree: TTree;
begin
  aNode := tvMain.Selected;
  DataT := TTreeEntry(tvMain.Selected.Data);
  if not Assigned(DataT) then exit;
  if MessageDlg(strRealdelete,mtInformation,[mbYes,mbNo],0) = mrYes then
    begin
      aTree := TTree.CreateEx(Self,Data);
      Data.SetFilter(aTree,'');
      if Data.GotoBookmark(aTree,DataT.Rec) then
        begin
          aTree.DataSet.Delete;
          FNode := aNode;
          Application.QueueAsyncCall(@DeleteNodeCall,PtrInt(@FNode));
        end;
      aTree.Free;
    end;
end;
procedure TfMainTree.acDeleteLinkExecute(Sender: TObject);
var
  aLinks: TLinks;
  aNode: TTreeNode;
  DataT: TTreeEntry;
begin
  aNode := tvMain.Selected;
  DataT := TTreeEntry(tvMain.Selected.Data);
  if not Assigned(DataT) then exit;
  if (DataT.Typ <> etLink)
  then exit;
  aLinks := TLinks.Create(nil);
  aLinks.Select(DataT.Rec);
  aLinks.Open;
  if aLinks.Count>0 then
    begin
      aLinks.Delete;
      FNode := aNode;
      Application.QueueAsyncCall(@DeleteNodeCall,PtrInt(@FNode));
    end;
  aLinks.Free;
end;

procedure TfMainTree.acHideEntryExecute(Sender: TObject);
begin
  FNode := tvMain.Selected;
  Application.QueueAsyncCall(@DeleteNodeCall,PtrInt(@FNode));
  tvMain.Selected := nil;
  SaveTreeOptions;
end;

procedure TfMainTree.DblClickTimerTimer(Sender: TObject);
begin
  DblClickTimer.Enabled:=False;
  acOpen.Execute;
end;
procedure TfMainTree.DeleteNodeCall(Data: PtrInt);
begin
  try
    tvMain.Items.Delete(TTreeNode(Pointer(Data)^));
  except
  end;
end;
resourcestring
  strMyTasks                    = 'meine Aufgaben';
  strFinancial                  = 'Finanzen';
  strBanking                    = 'Bankverbindungen';
  strNewAccount                 = 'Neues Konto';
  strNewTransfer                = 'Neue Überweisung';
  strAccountingQue              = 'Warteschlange';
  strMycalendar                 = 'Mein Kalender';
procedure TfMainTree.tvMainAdvancedCustomDrawItem(Sender: TCustomTreeView;
  Node: TTreeNode; State: TCustomDrawState; Stage: TCustomDrawStage;
  var PaintImages, DefaultDraw: Boolean);
var
  aData: TTreeEntry;
  NodeRect: TRect;
  Style: TFPBrushStyle;
  CellText: String;
  aImageIndex: Integer;
  aImageList : TImageList;
  i: Integer;
  tmp: String;
  aWidth: Integer;
  tmDetails: TThemedElementDetails;
  aNodeRect: TRect;
  Details: TThemedElementDetails;
  BKDrawn: Boolean;
  bgRect: TRect;
begin
  if Stage = cdPrePaint then
    begin
      NodeRect := Node.DisplayRect(True);
      NodeRect.Left := 0;
      NodeRect.Right := Sender.Width;
      Style := Sender.Canvas.Brush.Style;
      Sender.Canvas.Brush.Style := bsSolid;
      Sender.Canvas.Pen.Color:=Sender.Canvas.Brush.Color;
      Sender.Canvas.FillRect(NodeRect);
    end;
  if Stage <> cdPostPaint then exit;
  if not Assigned(Node.Data) then exit;
  aData := TTreeEntry(Node.Data);
  if not Assigned(aData) then exit;
  NodeRect := Node.DisplayRect(True);
  //Background
  BKDrawn := False;
  if (cdsSelected in State) then
    begin
      if tvoThemedDraw in tvMain.Options then
        begin
          bgRect := NodeRect;
          bgRect.Right:=tvMain.Width;
          if Focused then
            Details := ThemeServices.GetElementDetails(ttItemSelected)
          else
            Details := ThemeServices.GetElementDetails(ttItemSelectedNotFocus);
          if ThemeServices.HasTransparentParts(Details) then
          begin
            tvMain.Canvas.Brush.Color := tvMain.BackgroundColor;
            tvMain.Canvas.FillRect(bgRect);
          end;
          ThemeServices.DrawElement(tvMain.Canvas.Handle, Details, bgRect, nil);
          BKDrawn := True;
        end;
      if not BKDrawn then
        begin
          tvMain.Canvas.Brush.Color := tvMain.SelectionColor;
          tvMain.Canvas.FillRect(bgRect);
        end;
    end;
  NodeRect.Right := Sender.Width-NodeRect.Left;
  Sender.Canvas.Brush.Style := bsClear;
  CellText := GetNodeText(Node);
  aImageIndex := -1;
  aImageList := nil;
  try
  case aData.Typ of
  etMasterdata:
    begin
      aImageIndex := 0;
      aImageList := fVisualControls.ImageListBig;
    end;
  etCustomers:
    begin
      aImageIndex := IMAGE_PERSON;
      aImageList := fVisualControls.ImageListBig;
    end;
  etTasks:
    begin
      aImageIndex := IMAGE_TASK;
      aImageList := fVisualControls.ImageListBig;
    end;
  etSearch:
    begin
      aImageIndex := IMAGE_SEARCH;
      aImageList := fVisualControls.ImageListBig;
    end;
  etMessages:
    begin
      aImageIndex := 5;
      aImageList := fVisualControls.ImageListBig;
    end;
  etCalendar:
    begin
      aImageIndex := 4;
      aImageList := fVisualControls.ImageListBig;
    end;
  etFinancial:
    begin
      aImageIndex := IMAGE_FINANCIAL;
      aImageList := fVisualControls.ImageListBig;
    end;
  etOrders:
    begin
      aImageIndex := IMAGE_ORDERS;
      aImageList := fVisualControls.ImageListBig;
    end;
  etWiki:
    begin
      aImageIndex := IMAGE_WIKI;
      aImageList := fVisualControls.ImageListBig;
    end;
  etStatistics:
    begin
      aImageIndex := IMAGE_STATISTICS;
      aImageList := fVisualControls.ImageListBig;
    end;
  etTimeRegistering:
    begin
      aImageIndex := IMAGE_TIME;
      aImageList := fVisualControls.ImageListBig;
    end;
  etProjects:
    begin
      aImageIndex := IMAGE_PROJECTS;
      aImageList := fVisualControls.ImageListBig;
    end;
  etFavourites:
    begin
      aImageIndex := IMAGE_FAVOURITES;
      aImageList := fVisualControls.ImageListBig;
    end;
  etDocuments:
    begin
      aImageIndex := 17;
      aImageList := fVisualControls.ImageListBig;
    end;
  etImages:
    begin
      aImageIndex := 16;
      aImageList := fVisualControls.ImageListBig;
    end;
  end;
  if aImageIndex = -1 then
    case aData.Typ of
    etDir,etDocumentDir:aImageIndex := IMAGE_FOLDER;
    etCustomers:aImageIndex := IMAGE_PERSON;
    etCustomer,etEmployee:aImageIndex := IMAGE_PERSON;
    etCalendarUser,etMyCalendar,etCalendarDir:aImageIndex:=105;
    etTaskUser:aImageIndex:=IMAGE_TASK;
    etArticle:aImageIndex := IMAGE_MASTERDATA;
    etSupplier:aImageIndex := IMAGE_SUPPLIER;
    etAction:aImageIndex := aData.Action.ImageIndex;
    etMessageDir:aImageIndex := IMAGE_MESSAGEOPEN;
    etMessageBoard:aImageIndex := IMAGE_TABLEDIR;
    etFinancial:aImageIndex:= IMAGE_FINANCIAL;
    etBanking:aImageIndex := IMAGE_BANKING;
    etAccounts:aImageIndex:= IMAGE_ACCOUNTS;
    etAccount:aImageIndex := IMAGE_ACCOUNT;
    etNewAccount:aImageIndex := IMAGE_NEWACCOUNT;
    etNewTransfer:aImageIndex := IMAGE_NEWTRANSFER;
    etAccountingQue:aImageIndex := IMAGE_ACCOUNTINGQUE;
    etOrderList,etArticleList,etCustomerList,etMeetingList:aImageIndex := IMAGE_ORDERLIST;
    etNewOrder:aImageIndex := IMAGE_ORDERPAGE;
    etWikiPage:aImageIndex := 40;
    etSalesList:aImageIndex := 9;
    etProject:aImageIndex := 13;
    etProcess:aImageIndex := 103;
    etFiles:aImageIndex := 19;
    etLists:aImageIndex := 24;
    etStatistic:aImageIndex := 58;
    etInventory:aImageIndex := 24;
    etMeetings:aImageIndex := 31;
    etTaskPlan,etAttPlan:aImageIndex:=4;
    etClipboardItem:aImageIndex:=111;
    end;
  if (aImageIndex <> -1) and (not Assigned(aImageList)) then
    aImageList := fVisualControls.Images;
  if (aData.LinkIcon > -1) and (aImageIndex = -1) then
    begin
      aImageList := fVisualControls.Images;
      aImageIndex := aData.LinkIcon;
    end;
  if aImageIndex <> -1 then
    begin
      aImageList.Draw(Sender.Canvas,NodeRect.Left,NodeRect.Top,aImageIndex,true);
      NodeRect.Left:=NodeRect.Left+aImageList.Width;
    end;
  NodeRect.Left:=NodeRect.Left+3;
  Sender.Canvas.Font.Size:=Sender.Font.Size;
  //Text
  if cdsSelected in State then
    begin
      if Focused then
        tmDetails := ThemeServices.GetElementDetails(ttItemSelected)
      else
        tmDetails := ThemeServices.GetElementDetails(ttItemSelectedNotFocus);
      Sender.Canvas.Font.Color:=clHighlightText
    end
  else
    begin
      Sender.Canvas.Font.Color:=Sender.Font.Color;
      tmDetails := ThemeServices.GetElementDetails(ttItemNormal)
    end;
  if Sender.Width-NodeRect.Left < Sender.Width then
    begin
      aNodeRect := NodeRect;
      aNoderect.Right:=NodeRect.Left+Sender.Width;
      try
        if (tvoThemedDraw in Sender.Options) then
          ThemeServices.DrawText(Sender.Canvas, tmDetails, TextCut(Sender.Canvas,Sender.Width-NodeRect.Left,Celltext), aNodeRect,DT_SINGLELINE or DT_NOPREFIX, 0)
        else
          Sender.Canvas.TextOut(NodeRect.Left,NodeRect.Top,TextCut(Sender.Canvas,Sender.Width-NodeRect.Left,Celltext));
      except
      end;
    end;
  if Assigned(aData.SubText) then
    begin
      {$IFDEF LCLgtk2}
      //Sender.Canvas.Font.Size:=8;
      {$ELSE}
      Sender.Canvas.Font.Size:=7;
      {$ENDIF}
      Sender.Canvas.Font.Color:=clGrayText;
      for i := 0 to aData.SubText.Count-1  do
        begin
          if i=0 then
            aWidth := (NodeRect.Right-NodeRect.Left)-(Sender.Canvas.TextExtent(TextCut(Sender.Canvas,NodeRect.Right-NodeRect.Left,Celltext)).cx+25)
          else
            aWidth := (NodeRect.Right-NodeRect.Left)-25;
          Sender.Canvas.TextOut(Sender.Width-Sender.Canvas.TextExtent(TextCut(Sender.Canvas,aWidth,aData.SubText[i])).cx-25,NodeRect.Top,TextCut(Sender.Canvas,aWidth,aData.SubText[i]));
          NodeRect.Top:=NodeRect.Top+Sender.Canvas.TextExtent(aData.SubText[i]).cy;
        end;
      Sender.Canvas.Font.Size:=Sender.Font.Size;
      Sender.Canvas.Font.Style:=Sender.Font.Style;
      Sender.Canvas.Font.Color:=Sender.Font.Color;
    end
  else if aData.Typ = etAction then
    begin
      i := Sender.Canvas.TextExtent(CellText).cx;
      Sender.Canvas.Font.Size:=7;
      Sender.Canvas.Font.Color:=clGrayText;
      tmp := '('+ShortCutToText(aData.Action.ShortCut)+')';
      if tmp <> '()' then
        Sender.Canvas.TextOut(NodeRect.Left+i+6,NodeRect.Top,tmp);
    end;
  except
  end;
  Sender.Canvas.Brush.Style := Style;
  DefaultDraw := True;
end;

procedure TfMainTree.tvMainClick(Sender: TObject);
var
  DataT: TTreeEntry;
begin
  if not Assigned(tvMain.Selected) then exit;
  DataT := TTreeEntry(tvMain.Selected.Data);
  case DataT.Typ of
  etAction:DataT.Action.Execute;
  etSearch:acSearch.Execute;
  else
    if Assigned(FOpen) then
      FOpen(DataT);
  end;
  Screen.Cursor:=crDefault;
end;

procedure TfMainTree.tvMainDblClick(Sender: TObject);
begin
  DblClickTimer.Enabled:=True;
end;
procedure TfMainTree.tvMainDeletion(Sender: TObject; Node: TTreeNode);
begin
  if Assigned(Node.Data) then
    TTreeEntry(Node.Data).Free;
end;
procedure TfMainTree.tvMainDragDrop(Sender, Source: TObject; X, Y: Integer);
var
  DataT : TTreeEntry;
  aFrame: TPrometMainFrame;
  DataT2: TTreeEntry;
  aNewParent: Variant;
  aDataSet: TBaseDBDataset;
  aLinks: TLinks;
  aLink: String;
  aPProject: TProject;
  DataSourceType : TBaseDbDataSetClass;
  aNewDS: TBaseDBDataset;
  aNode: TTreeNode;
  tmp: String;
  aNode1: TTreeNode;
  aTargetNode: TTreeNode;
begin
  if Assigned(FDragDrop) then
    begin
      FDragDrop(Sender,Source,X,Y);
    end;
  if Source = Sender then
    begin
      DataT := TTreeEntry(tvMain.Selected.Data);
      case DataT.Typ of
      etCustomer,
      etSupplier,
      etLink,
      etArticle,
      etProject,
      etProcess,
      etStatistic,
      etWikiPage:
        begin
          aTargetNode:=tvMain.GetNodeAt(X,Y);
          if Assigned(aTargetNode) and Assigned(aTargetNode.Data) then
            if (TTreeEntry(aTargetNode.Data).Typ = etDir) or (TTreeEntry(aTargetNode.Data).Typ=etDocumentDir) then
              begin
                case DataT.Typ of
                etCustomer,
                etSupplier,
                etArticle,
                etLink,
                etProject,
                etProcess,
                etStatistic,
                etWikiPage:
                  begin
                    DataT2 := TTreeEntry(aTargetNode.Data);
                    Data.SetFilter(Data.Tree,'',0,'','ASC',False,True,True);
                    Data.Tree.GotoBookmark(DataT2.Rec);
                    if (Data.Tree.FieldByName('TYPE').AsString <> 'F') then
                      begin
                        aNewParent:=Data.Tree.Id.AsVariant;
                        acOpen.Execute;
                        aFrame := TPrometMainFrame(pcPages.ActivePage.Controls[0]);
                        with aFrame.DataSet.DataSet do
                          begin
                            Edit;
                            FieldByName('TREEENTRY').AsVariant:=aNewParent;
                            Post;
                          end;
                        with TPrometMainFrame(pcPages.ActivePage.Controls[0]) do
                          begin
                            if Assigned(Connection) then
                              begin
                                DataSet.CascadicPost;
                                if UseTransactions then
                                  begin
                                    Data.CommitTransaction(Connection);
                                    Data.StartTransaction(Connection);
                                  end;
                              end;
                          end;
                        tvMain.Selected.Delete;
                        aNode1 := aTargetNode;
                        if Assigned(aNode1) then
                          begin
                            aNode1.Collapse(True);
                            aNode1.HasChildren:=True;
                            aNode1.Expand(False);
                          end;
                      end
                    else if  (DataT.Typ = etLink) then
                      begin
                        aDataSet := DataT.DataSourceType.CreateEx(Self,Data);
                        with aDataSet.DataSet as IBaseDBFilter do
                          Filter := DataT.Filter;
                        aDataSet.Open;
                        if aDataSet.Count > 0 then
                          begin
                            aDataSet.DataSet.Edit;
                            aDataSet.FieldByName('RREF_ID').AsVariant := Data.Tree.Id.AsVariant;
                            aDataSet.DataSet.Post;
                            aTargetNode.Collapse(True);
                            aTargetNode.HasChildren:=True;
                            aTargetNode.Expand(False);
                            tvMain.Selected.Delete;
                          end;
                        aDataSet.Free;
                      end
                    else //Favourite
                      begin
                        if DataT.Link = '' then
                          begin
                            aDataSet := DataT.DataSourceType.CreateEx(Self,Data);
                            with aDataSet.DataSet as IBaseDBFilter do
                              Filter := DataT.Filter;
                            aDataSet.Open;
                            if aDataSet.Count > 0 then
                              begin
                                aLinks := TLinks.CreateEx(Self,Data);
                                aLinks.Append;
                                aLinks.FieldByName('RREF_ID').AsVariant := Data.Tree.Id.AsVariant;
                                aLink := Data.BuildLink(aDataSet.DataSet);
                                aLinks.FieldByName('NAME').AsString := Data.GetLinkDesc(aLink);
                                aLinks.FieldByName('LINK').AsString := aLink;
                                aLinks.FieldByName('ICON').AsInteger := Data.GetLinkIcon(aLink);
                                aLinks.FieldByName('CHANGEDBY').AsString := Data.Users.IDCode.AsString;
                                aLinks.DataSet.Post;
                                aLinks.Free;
                              end;
                            aDataSet.Free;
                            aTargetNode.Collapse(True);
                            aTargetNode.HasChildren:=True;
                            aTargetNode.Expand(False);
                          end;
                      end;
                  end;
              end;
            end;
          if Assigned(tvMain.GetNodeAt(X,Y)) and Assigned(tvMain.GetNodeAt(X,Y).Data) then
            if TTreeEntry(tvMain.GetNodeAt(X,Y).Data).Typ = etProject then
              begin
                aPProject := TProject.Create(nil);
                DataT2 := TTreeEntry(tvMain.GetNodeAt(X,Y).Data);
                aPProject.Select(DataT2.Rec);
                aPProject.Open;
                if (aPProject.Count > 0) and (pcPages.ActivePage.ControlCount>0) then
                  begin
                    acOpen.Execute;
                    aFrame := TPrometMainFrame(pcPages.ActivePage.Controls[0]);
                    with aFrame.DataSet.DataSet do
                      begin
                        Edit;
                        FieldByName('PARENT').AsVariant:=aPProject.Id.AsVariant;
                      end;
                    with TPrometMainFrame(pcPages.ActivePage.Controls[0]) do
                      begin
                        if Assigned(Connection) then
                          begin
                            DataSet.CascadicPost;
                            if UseTransactions then
                              begin
                                Data.CommitTransaction(Connection);
                                Data.StartTransaction(Connection);
                              end;
                          end;
                      end;
                    if Assigned(tvMain.Selected) then
                      tvMain.Selected.Delete;
                    aNode := tvMain.GetNodeAt(X,Y).Parent;
                    tmp := tvMain.GetNodeAt(X,Y).Text;
                    if Assigned(aNode) then
                      begin
                        aNode.Collapse(True);
                        aNode.HasChildren:=True;
                        aNode.Expand(False);
                        aNode := aNode.FindNode(tmp);
                        if Assigned(aNode) then
                          begin
                            aNode.HasChildren:=True;
                            aNode.Expand(False);
                          end;
                      end;
                  end;
                aPProject.Free;
              end;
        end;
      etDir,etDocumentDir,etMessageDir,etMessageBoard:
        begin
          aTargetNode := tvMain.GetNodeAt(X,Y);
          DataT2 := TTreeEntry(aTargetNode.Data);
          if Assigned(aTargetNode) and Assigned(aTargetNode.Data) then
            if (TTreeEntry(aTargetNode.Data).Typ = etDir)
            or (TTreeEntry(aTargetNode.Data).Typ = etDocumentDir)
            or (TTreeEntry(aTargetNode.Data).Typ = etMessageDir)
            or (TTreeEntry(aTargetNode.Data).Typ = etMessageBoard)
            then
              begin
                Data.SetFilter(Data.Tree,'',0,'','ASC',False,True,True);
                Data.Tree.GotoBookmark(DataT2.Rec);
                if MessageDlg(Format(strRealMove,[Data.Tree.Text.AsString]),mtInformation,[mbYes,mbNo],0) = mrYes then
                  begin
                    if Data.Tree.FieldByName('TYPE').AsString <> 'F' then
                      begin
                        aNewParent := Data.Tree.id.AsVariant;
                        Data.SetFilter(Data.Tree,'',0,'','ASC');
                        Data.Tree.GotoBookmark(DataT.Rec);
                        with Data.Tree.DataSet do
                          begin
                            Edit;
                            FieldByName('PARENT').AsVariant:=aNewParent;
                            Post;
                          end;
                        aTargetNode.Collapse(True);
                        aTargetNode.HasChildren:=True;
                        aTargetNode.Expand(False);
                        try
                          tvMain.Selected.Delete;
                        except
                        end;
                      end;
                  end;
              end;
        end;
      end;
      if Assigned(tvMain.Selected) and (tvMain.Selected.Level=0) and Assigned(tvMain.GetNodeAt(X,Y)) and (tvMain.GetNodeAt(X,Y).Level=0) then
        begin
          tvMain.Selected.MoveTo(tvMain.GetNodeAt(X,Y),naInsertBehind);
          SaveTreeOptions;
        end;
    end
  else if Source = pcPages then
    begin
      if Assigned(pcPages.ActivePage) and (pcPages.ActivePage.ControlCount > 0) and (pcPages.ActivePage.Controls[0] is TPrometMainFrame) then
        begin
          if Assigned(tvMain.GetNodeAt(X,Y)) and Assigned(tvMain.GetNodeAt(X,Y).Data) then
            if TTreeEntry(tvMain.GetNodeAt(X,Y).Data).Typ = etDir then
              begin
                if (TPrometMainFrame(pcPages.ActivePage.Controls[0]).DataSet is TPerson)
                or (TPrometMainFrame(pcPages.ActivePage.Controls[0]).DataSet is TMasterdata)
                or (TPrometMainFrame(pcPages.ActivePage.Controls[0]).DataSet is TProject)
                then
                  begin
                    DataT2 := TTreeEntry(tvMain.GetNodeAt(X,Y).Data);
                    Data.SetFilter(Data.Tree,'',0,'','ASC',False,True,True);
                    Data.Tree.GotoBookmark(DataT2.Rec);
                    if Data.Tree.FieldByName('TYPE').AsString <> 'F' then
                      begin
                        if not TPrometMainFrame(pcPages.ActivePage.Controls[0]).DataSet.CanEdit then
                          TPrometMainFrame(pcPages.ActivePage.Controls[0]).DataSet.DataSet.Edit;
                        TPrometMainFrame(pcPages.ActivePage.Controls[0]).DataSet.FieldByName('TREEENTRY').AsInteger:=Data.Tree.Id.AsVariant;
                        with TPrometMainFrame(pcPages.ActivePage.Controls[0]) do
                          begin
                            if Assigned(Connection) then
                              begin
                                DataSet.CascadicPost;
                                if UseTransactions then
                                  begin
                                    Data.CommitTransaction(Connection);
                                    Data.StartTransaction(Connection);
                                  end;
                              end;
                          end;
                        tvMain.BeginUpdate;
                        tvMain.GetNodeAt(X,Y).Collapse(True);
                        tvMain.GetNodeAt(X,Y).HasChildren:=True;
                        tvMain.GetNodeAt(X,Y).Expand(False);
                        tvMain.EndUpdate;
                      end
                    else //Favourite
                      begin
                        aLinks := TLinks.CreateEx(Self,Data);
                        aLinks.Append;
                        aLinks.FieldByName('RREF_ID').AsVariant := Data.Tree.Id.AsVariant;
                        aLink := Data.BuildLink(TPrometMainFrame(pcPages.ActivePage.Controls[0]).DataSet.DataSet);
                        aLinks.FieldByName('NAME').AsString := Data.GetLinkDesc(aLink);
                        aLinks.FieldByName('LINK').AsString := aLink;
                        aLinks.FieldByName('ICON').AsInteger := Data.GetLinkIcon(aLink);
                        aLinks.FieldByName('CHANGEDBY').AsString := Data.Users.IDCode.AsString;
                        aLinks.DataSet.Post;
                        aLinks.Free;
                        tvMain.GetNodeAt(X,Y).Collapse(True);
                        tvMain.GetNodeAt(X,Y).HasChildren:=True;
                        tvMain.GetNodeAt(X,Y).Expand(False);
                      end;
                  end;
              end;
        end;
    end
  else if (Source is TExtDBGrid) and (TExtDBGrid(Source).Owner is TfFilter) then
    begin
      DataT := TTreeEntry(tvMain.Selected.Data);
      if (TTreeEntry(tvMain.GetNodeAt(X,Y).Data).Typ = etDir) then
        begin
          if (TfFilter(TExtDBGrid(Source).Owner).DataSet is TPersonList)
          or (TfFilter(TExtDBGrid(Source).Owner).DataSet is TMasterdataList)
          or (TfFilter(TExtDBGrid(Source).Owner).DataSet is TProjectList) then
            begin
              DataT2 := TTreeEntry(tvMain.GetNodeAt(X,Y).Data);
              Data.SetFilter(Data.Tree,'',0,'','ASC',False,True,True);
              Data.Tree.GotoBookmark(DataT2.Rec);
              if Data.Tree.FieldByName('TYPE').AsString <> 'F' then
                begin
                  try
                    DataSourceType:=TBaseDBDataSetClass(TfFilter(TExtDBGrid(Source).Owner).DataSet.ClassType);
                    aNewDS := DataSourceType.Create(nil);
                    with aNewDS.DataSet as IBaseDBFilter do
                      begin
                        UsePermissions:=False;
                      end;
                    aNewDS.Select(TfFilter(TExtDBGrid(Source).Owner).DataSet.Id.AsVariant);
                    aNewDS.Open;
                    if not aNewDS.CanEdit then
                      aNewDS.DataSet.Edit;
                    aNewDS.FieldByName('TREEENTRY').AsVariant:=Data.Tree.Id.AsVariant;
                    aNewDS.DataSet.Post;
                    aNewDS.Free;
                    tvMain.BeginUpdate;
                    tvMain.GetNodeAt(X,Y).Collapse(True);
                    tvMain.GetNodeAt(X,Y).HasChildren:=True;
                    tvMain.GetNodeAt(X,Y).Expand(False);
                    tvMain.EndUpdate;
                  except
                  end;
                end
              else //Favourite
                begin
                  aLinks := TLinks.CreateEx(Self,Data);
                  aLinks.Append;
                  aLinks.FieldByName('RREF_ID').AsVariant := Data.Tree.Id.AsVariant;
                  aLink := Data.BuildLink(TfFilter(TExtDBGrid(Source).Owner).DataSet.DataSet);
                  aLinks.FieldByName('NAME').AsString := Data.GetLinkDesc(aLink);
                  aLinks.FieldByName('LINK').AsString := aLink;
                  aLinks.FieldByName('ICON').AsInteger := Data.GetLinkIcon(aLink);
                  aLinks.FieldByName('CHANGEDBY').AsString := Data.Users.IDCode.AsString;
                  aLinks.DataSet.Post;
                  aLinks.Free;
                  tvMain.GetNodeAt(X,Y).Collapse(True);
                  tvMain.GetNodeAt(X,Y).HasChildren:=True;
                  tvMain.GetNodeAt(X,Y).Expand(False);
                end;
            end;
        end;
      if (TTreeEntry(tvMain.GetNodeAt(X,Y).Data).Typ = etProject) then
        begin
          if (TfFilter(TExtDBGrid(Source).Owner).DataSet is TProjectList) then
            begin
              aPProject := TProject.Create(nil);
              DataT2 := TTreeEntry(tvMain.GetNodeAt(X,Y).Data);
              aPProject.Select(DataT2.Rec);
              aPProject.Open;
              if aPProject.Count>0 then
                begin
                  DataSourceType:=TBaseDBDataSetClass(TfFilter(TExtDBGrid(Source).Owner).DataSet.ClassType);
                  aNewDS := DataSourceType.Create(nil);
                  with aNewDS.DataSet as IBaseDBFilter do
                    begin
                      UsePermissions:=False;
                    end;
                  aNewDS.Select(TfFilter(TExtDBGrid(Source).Owner).DataSet.Id.AsVariant);
                  aNewDS.Open;
                  if not aNewDS.CanEdit then
                    aNewDS.DataSet.Edit;
                  aNewDS.FieldByName('PARENT').AsVariant:=aPProject.Id.AsVariant;
                  aNewDS.DataSet.Post;
                  aNewDS.Free;
                  tvMain.BeginUpdate;
                  aNode := tvMain.GetNodeAt(X,Y).Parent;
                  tmp := tvMain.GetNodeAt(X,Y).Text;
                  if Assigned(aNode) then
                    begin
                      aNode.Collapse(True);
                      aNode.HasChildren:=True;
                      aNode.Expand(False);
                      aNode := aNode.FindNode(tmp);
                      if Assigned(aNode) then
                        begin
                          aNode.HasChildren:=True;
                          aNode.Expand(False);
                        end;
                    end;
                  tvMain.EndUpdate;
                end;
              aPProject.Free;
            end;
        end;
    end
  else if Assigned(fSearch) and (Source = fSearch.sgResults) then
    begin
      if Assigned(tvMain.GetNodeAt(X,Y)) and Assigned(tvMain.GetNodeAt(X,Y).Data) then
        if TTreeEntry(tvMain.GetNodeAt(X,Y).Data).Typ = etDir then
          begin
            DataT2 := TTreeEntry(tvMain.GetNodeAt(X,Y).Data);
            Data.SetFilter(Data.Tree,'',0,'','ASC',False,True,True);
            Data.Tree.GotoBookmark(DataT2.Rec);
            if Data.Tree.FieldByName('TYPE').AsString = 'F' then
//              if DataT.Link = '' then
                begin
                  aLinks := TLinks.CreateEx(Self,Data);
                  aLinks.Append;
                  aLinks.FieldByName('RREF_ID').AsVariant := Data.Tree.Id.AsVariant;
                  aLink := fSearch.GetLink;
                  aLinks.FieldByName('NAME').AsString := Data.GetLinkDesc(aLink);
                  aLinks.FieldByName('LINK').AsString := aLink;
                  aLinks.FieldByName('ICON').AsInteger := Data.GetLinkIcon(aLink);
                  aLinks.FieldByName('CHANGEDBY').AsString := Data.Users.IDCode.AsString;
                  aLinks.DataSet.Post;
                  aLinks.Free;
                  tvMain.GetNodeAt(X,Y).Collapse(True);
                  tvMain.GetNodeAt(X,Y).HasChildren:=True;
                  tvMain.GetNodeAt(X,Y).Expand(False);
                end;
          end;
    end;
end;
procedure TfMainTree.tvMainDragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
var
  DataT : TTreeEntry;
  DataT2: TTreeEntry;
  aTyp: String;
begin
  Accept := False;
  if not Assigned(Source) then exit;
  if not Assigned(tvMain.GetNodeAt(X,Y)) then exit;
  if Assigned(FDragOver) then
    begin
      FDragOver(Sender,Source,X,Y,State,Accept);
      if Accept then exit;
    end;
  if Source = Sender then
    begin
      if not Assigned(tvMain.Selected) then exit;
      DataT := TTreeEntry(tvMain.Selected.Data);
      case DataT.Typ of
      etCustomer,
      etSupplier,
      etArticle,
      etProject,
      etProcess,
      etDir,etDocumentDir,etMessageDir,
      etLink,
      etWikiPage,
      etStatistic:
        begin
          if Assigned(tvMain.GetNodeAt(X,Y)) and Assigned(tvMain.GetNodeAt(X,Y).Data) then
            if (TTreeEntry(tvMain.GetNodeAt(X,Y).Data).Typ = etDir) or (TTreeEntry(tvMain.GetNodeAt(X,Y).Data).Typ = etDocumentDir) or (TTreeEntry(tvMain.GetNodeAt(X,Y).Data).Typ = etMessageDir) or (TTreeEntry(tvMain.GetNodeAt(X,Y).Data).Typ = etMessageBoard) then
              if DataT.Rec <> TTreeEntry(tvMain.GetNodeAt(X,Y).Data).Rec then
                begin
                  if ((DataT.Typ = etDir) or (DataT.Typ = etDocumentDir) or (DataT.Typ = etMessageDir) or (DataT.Typ = etMessageBoard))
                  and ((TTreeEntry(tvMain.GetNodeAt(X,Y).Data).Typ = etDir) or (TTreeEntry(tvMain.GetNodeAt(X,Y).Data).Typ = etDocumentDir)) then
                    begin
                      DataT2 := TTreeEntry(tvMain.GetNodeAt(X,Y).Data);
                      Data.SetFilter(Data.Tree,'',0,'','ASC',False,True,True);
                      Data.Tree.GotoBookmark(DataT2.Rec);
                      aTyp := Data.Tree.FieldByName('TYPE').AsString;
                      Data.Tree.GotoBookmark(DataT.Rec);
                      Accept := Data.Tree.FieldByName('TYPE').AsString = aTyp;
                    end
                  else
                    Accept := True;
                end;
          if (not Accept) then
            if Assigned(tvMain.GetNodeAt(X,Y)) and Assigned(tvMain.GetNodeAt(X,Y).Data) then
              if TTreeEntry(tvMain.GetNodeAt(X,Y).Data).Typ = etProject then
                if DataT.Rec <> TTreeEntry(tvMain.GetNodeAt(X,Y).Data).Rec then
                  Accept:=True;
        end;
      end;
      //if (tvMain.Selected.Level=0) and Assigned(tvMain.GetNodeAt(X,Y)) and (tvMain.GetNodeAt(X,Y).Level=0) then
      //  Accept := True;
    end
  else if Source = pcPages then
    begin
     if Assigned(pcPages.ActivePage) and (pcPages.ActivePage.ControlCount > 0) and (pcPages.ActivePage.Controls[0] is TPrometMainFrame) then
       begin
         if Assigned(tvMain.GetNodeAt(X,Y)) and Assigned(tvMain.GetNodeAt(X,Y).Data) then
           begin
             if TTreeEntry(tvMain.GetNodeAt(X,Y).Data).Typ = etDir then
               Accept := True;
             if TTreeEntry(tvMain.GetNodeAt(X,Y).Data).Typ = etProject then
               Accept := True;
           end;
       end;
    end
  else if (Source is TExtDBGrid) and (TExtDBGrid(Source).Owner is TfFilter) then
    begin
      DataT := TTreeEntry(tvMain.Selected.Data);
      if (TTreeEntry(tvMain.GetNodeAt(X,Y).Data).Typ = etDir) then
        if (TfFilter(TExtDBGrid(Source).Owner).DataSet is TPersonList)
        or (TfFilter(TExtDBGrid(Source).Owner).DataSet is TMasterdataList)
        or (TfFilter(TExtDBGrid(Source).Owner).DataSet is TProjectList) then
          Accept := True;
      if (TTreeEntry(tvMain.GetNodeAt(X,Y).Data).Typ = etProject) then
        if (TfFilter(TExtDBGrid(Source).Owner).DataSet is TProjectList) then
          Accept := True;
    end
  else if Assigned(fSearch) and (Source = fSearch.sgResults) then
    begin
      if Assigned(tvMain.GetNodeAt(X,Y)) and Assigned(tvMain.GetNodeAt(X,Y).Data) then
        begin
          if TTreeEntry(tvMain.GetNodeAt(X,Y).Data).Typ = etDir then
            begin
              DataT2 := TTreeEntry(tvMain.GetNodeAt(X,Y).Data);
              Data.SetFilter(Data.Tree,'',0,'','ASC',False,True,True);
              Data.Tree.GotoBookmark(DataT2.Rec);
              aTyp := Data.Tree.FieldByName('TYPE').AsString;
              Accept := aTyp = 'F';
            end;
          if TTreeEntry(tvMain.GetNodeAt(X,Y).Data).Typ = etProject then
            begin
              DataT2 := TTreeEntry(tvMain.GetNodeAt(X,Y).Data);
              Data.SetFilter(Data.Tree,'',0,'','ASC',False,True,True);
              Data.Tree.GotoBookmark(DataT2.Rec);
              aTyp := Data.Tree.FieldByName('TYPE').AsString;
              Accept := aTyp = 'P';
            end;
        end;
    end;
end;
procedure TfMainTree.tvMainEdited(Sender: TObject; Node: TTreeNode;
  var S: string);
var
  DataT : TTreeEntry;
  aDataSet: TBaseDBDataset;
  aTree: TTree;
begin
  DataT := TTreeEntry(Node.Data);
  if not Assigned(DataT) then exit;
  if (DataT.Typ <> etDir) and (DataT.Typ <> etWikiPage) and (DataT.Typ <> etLink) and (DataT.Typ <> etStatistic) then exit;
  Data.SetFilter(Data.Tree,'',0,'','ASC',False,True,True);
  if (DataT.Typ = etDir) then
    begin
      aTree := TTree.CreateEx(Self,Data);
      Data.SetFilter(aTree,'');
      Data.GotoBookmark(aTree,DataT.Rec);
      aTree.DataSet.Edit;
      aTree.FieldByName('NAME').AsString := S;
      aTree.DataSet.Post;
      aTree.Free;
      DataT.Text[0] := S;
      Node.Text:='';
    end
  else if (DataT.Typ = etWikiPage) or (DataT.Typ = etLink) or (DataT.Typ = etStatistic) then
    begin
      Data.GotoBookmark(Data.Tree,DataT.Rec);
      aDataSet := DataT.DataSourceType.CreateEx(Self,Data);
      with aDataSet.DataSet as IBaseDBFilter do
        Filter := DataT.Filter;
      aDataSet.Open;
      with aDataSet.DataSet do
        begin
          Edit;
          FieldByName('NAME').AsString := S;
          Post;
        end;
      aDataSet.Free;
      DataT.Text[0] := S;
      Node.Text:='';
    end;
end;
procedure TfMainTree.tvMainEditing(Sender: TObject; Node: TTreeNode;
  var AllowEdit: Boolean);
var
  DataT : TTreeEntry;
begin
  if not Assigned(tvMain.Selected) then exit;
  DataT := TTreeEntry(tvMain.Selected.Data);
  AllowEdit := (Assigned(DataT) and (
     ((DataT.Typ = etDir) and (Data.Users.Rights.Right('TREE') > RIGHT_READ))
  or ((DataT.Typ = etWikiPage) and (Data.Users.Rights.Right('WIKI') > RIGHT_READ))
  or ((DataT.Typ = etLink))
  or ((DataT.Typ = etStatistic) and (Data.Users.Rights.Right('STATISTICS') > RIGHT_READ))
  ));
  if not AllowEdit then exit;
  Node.Text:=DataT.Text[0];
end;

procedure TfMainTree.tvMainExpanded(Sender: TObject; Node: TTreeNode);
begin
  if Node.Level=0 then SaveTreeOptions;
end;

procedure TfMainTree.tvMainExpanding(Sender: TObject; Node: TTreeNode;
  var AllowExpansion: Boolean);
var
  DataT: TTreeEntry;
  ID: String;
  Typ: String;
  aList: TBaseDBList;
  Node1: TTreeNode;
  aTyp: TEntryTyp;
  aPerson: TPerson;
  Node2: TTreeNode;
  aListL: TLinks;
  aProject: TProject;
  bTree: TTree;
  function GetHasChildren(aNode : TTreeNode) : Boolean;
  begin
    Result := False;
    if (TTreeEntry(Node1.Data).Typ=etProject) or (TTreeEntry(Node1.Data).Typ=etProcess) then
      begin
        aProject := TProject.Create(nil);
        aProject.SelectFromParent(aList.Id.AsVariant);
        aProject.Open;
        Result := aProject.Count>0;
        aProject.Free;
      end;
  end;
  procedure AddEntry;
  var
    i: Integer;
    aRec: LargeInt;
  begin
    aRec := aList.GetBookmark;
    if aRec > 0 then
      for i := 0 to Node.Count-1 do
        if TTreeEntry(Node.Items[i].Data).Rec = arec then exit;
    Node1 := tvMain.Items.AddChildObject(Node,'',TTreeEntry.Create);
    TTreeEntry(Node1.Data).Rec := aList.GetBookmark;
    with aList.DataSet as IBaseManageDB do
      TTreeEntry(Node1.Data).Filter:=Data.QuoteField(TableName)+'.'+Data.QuoteField('SQL_ID')+'='+Data.QuoteValue(IntToStr(aList.GetBookmark));
    TTreeEntry(Node1.Data).DataSourceType := TBaseDBDataSetClass(aList.ClassType);
    if aList.Number <> aList.Id then
      TTreeEntry(Node1.Data).Text[0] := aList.Text.AsString+' ('+aList.Number.AsString+')'
    else
      TTreeEntry(Node1.Data).Text[0] := aList.Text.AsString;
    if Assigned(aList.Status) and (aList.Status.AsString<>'') then
      TTreeEntry(Node1.Data).Text[0] := TTreeEntry(Node1.Data).Text[0]+' ['+aList.Status.AsString+']';
    TTreeEntry(Node1.Data).Typ := aTyp;
    Node1.HasChildren:=GetHasChildren(Node1);
  end;
begin
  DataT := TTreeEntry(Node.Data);
  if not Assigned(DataT) then
    exit;
  tvMain.BeginUpdate;
  Screen.Cursor:=crHourglass;
  if (DataT.Typ = etDir)
  or (DataT.Typ = etDocumentDir)
  or (DataT.Typ = etMessageDir)
  or (DataT.Typ = etMessageBoard)
  or (DataT.Typ = etCalendarDir)
  then
    begin
      Data.SetFilter(Data.Tree,'',0,'','DESC',False,True,True);
      Data.Tree.GotoBookmark(DataT.Rec);
      ID := IntToStr(Int64(Data.Tree.Id.AsVariant));
      Typ := Data.Tree.FieldByName('TYPE').AsString;
      Data.SetFilter(Data.Tree,Data.QuoteField('PARENT')+'='+ID,0,'','ASC',False,True,True);
      Node.DeleteChildren;
      try
      //Add directories
      bTree := TTree.Create(nil);
      while not Data.Tree.DataSet.EOF do
        begin
          Node1 := tvMain.Items.AddChildObject(Node,'',TTreeEntry.Create);
          TTreeEntry(Node1.Data).Rec := Data.GetBookmark(Data.Tree);
          TTreeEntry(Node1.Data).DataSource := Data.Tree;
          TTreeEntry(Node1.Data).Text[0] := Data.Tree.FieldByName('NAME').AsString;
          if Typ = 'D' then
            begin
              TTreeEntry(Node1.Data).Typ := etDocumentDir;
              bTree.Filter(Data.QuoteField('PARENT')+'='+Data.QuoteValue(Data.Tree.Id.AsVariant));
              if bTree.Count>0 then
                tvMain.Items.AddChild(Node1,'');
            end
          else if Typ = 'B' then
            begin
              TTreeEntry(Node1.Data).Typ := etMessageBoard;
              bTree.Filter(Data.QuoteField('PARENT')+'='+Data.QuoteValue(Data.Tree.Id.AsVariant));
              if bTree.Count>0 then
                tvMain.Items.AddChild(Node1,'');
            end
          else if Typ = 'N' then
            begin
              TTreeEntry(Node1.Data).Typ := etMessageDir;
              bTree.Filter(Data.QuoteField('PARENT')+'='+Data.QuoteValue(Data.Tree.Id.AsVariant));
              if bTree.Count>0 then
                tvMain.Items.AddChild(Node1,'');
            end
          else if Typ = 'A' then
            begin
              TTreeEntry(Node1.Data).Typ := etCalendarDir;
              bTree.Filter(Data.QuoteField('PARENT')+'='+Data.QuoteValue(Data.Tree.Id.AsVariant));
              if bTree.Count>0 then
                tvMain.Items.AddChild(Node1,'');
            end
          else
            begin
              TTreeEntry(Node1.Data).Typ := etDir;
              tvMain.Items.AddChild(Node1,'');
            end;
          Data.Tree.DataSet.Next;
        end;
      bTree.Free;
      //Add Entrys
      if (Typ = 'C') and (Data.Users.Rights.Right('CUSTOMERS') > RIGHT_NONE) then //Contacts
        begin
          aList := TPersonList.CreateEx(Self,Data);
          aTyp := etCustomer;
          Data.SetFilter(aList,Data.QuoteField('TREEENTRY')+'='+ID,0,'','ASC',False,True,True);
          aList.DataSet.First;
          aPerson := TPerson.CreateEx(Self,Data);
          while not aList.DataSet.EOF do
            begin
              AddEntry;
              if aList.FieldByName('TYPE').AsString = 'S' then
                TTreeEntry(Node1.Data).Typ := etSupplier;
              aPerson.Select(aList.Id.AsVariant);
              aPerson.Employees.Open;
              with aPerson.Employees.DataSet do
                begin
                  First;
                  while not EOF do
                    begin
                      if (trim(FieldByName('DEPARTMENT').AsString) = '') then
                        begin
                          Node2 := tvMain.Items.AddChildObject(Node1,'',TTreeEntry.Create);
                          TTreeEntry(Node2.Data).Rec := aPerson.Employees.GetBookmark;
                          TTreeEntry(Node2.Data).Filter:=Data.QuoteField('ACCOUNTNO')+'='+Data.QuoteValue(FieldByName('EMPLOYEE').AsString);
                          TTreeEntry(Node2.Data).DataSourceType := TPersonList;
                          TTreeEntry(Node2.Data).Text[0] := FieldByName('NAME').AsString;
                          TTreeEntry(Node2.Data).Typ := etEmployee;
                        end
                      else
                        begin
                          Node2 := Node1.FindNode(FieldByName('DEPARTMENT').AsString);
                          if not Assigned(Node2) then
                            Node2 := tvMain.Items.AddChild(Node1,FieldByName('DEPARTMENT').AsString);
                          Node2 := tvMain.Items.AddChildObject(Node2,'',TTreeEntry.Create);
                          TTreeEntry(Node2.Data).Rec := aPerson.Employees.GetBookmark;
                          TTreeEntry(Node2.Data).Filter:=Data.QuoteField('ACCOUNTNO')+'='+Data.QuoteValue(FieldByName('EMPLOYEE').AsString);
                          TTreeEntry(Node2.Data).DataSourceType := TPersonList;
                          TTreeEntry(Node2.Data).Text[0] := FieldByName('NAME').AsString;
                          TTreeEntry(Node2.Data).Typ := etEmployee;
                        end;
                      Next;
                    end;
                end;
              aList.DataSet.Next;
            end;
          aPerson.Free;
          aList.Free;
        end
      else if (Typ = 'M') and ((Data.Users.Rights.Right('MASTERDATA') > RIGHT_NONE)) then //Masterdata
        begin
          aList := TMasterdataList.CreateEx(Self,Data);
          aTyp := etArticle;
          Data.SetFilter(aList,Data.QuoteField('TREEENTRY')+'='+Data.QuoteValue(ID)+' AND '+Data.QuoteField('ACTIVE')+'='+Data.QuoteValue('Y'),0,'','ASC',False,True,True);
          aList.DataSet.First;
          while not aList.DataSet.EOF do
            begin
              AddEntry;
              aList.DataSet.Next;
            end;
          aList.Free;
        end
      else if (Typ = 'P') and ((Data.Users.Rights.Right('PROJECTS') > RIGHT_NONE)) then //Projekte
        begin
          aList := TProjectList.CreateEx(Self,Data);
          aTyp := etProject;
          with aList.DataSet as IBaseDBFilter do
            Data.SetFilter(aList,Data.QuoteField('TREEENTRY')+'='+Data.QuoteValue(ID)+' AND '+Data.ProcessTerm(Data.QuoteField('PARENT')+'='+Data.QuoteValue('')),0,'','ASC',False,True,True);
          aList.DataSet.First;
          while not aList.DataSet.EOF do
            begin
              AddEntry;
              if aList.FieldByName('TYPE').AsString = 'C' then
                TTreeEntry(Node1.Data).Typ := etProcess;
              aList.DataSet.Next;
            end;
          aList.Free;
        end
      else if (Typ = 'W') and (Data.Users.Rights.Right('WIKI') > RIGHT_NONE) then //Wiki
        begin
          aList := TWikiList.CreateEx(Self,Data);
          aTyp := etWikiPage;
          Data.SetFilter(aList,Data.QuoteField('TREEENTRY')+'='+Data.QuoteValue(ID),0,'','ASC',False,True,True);
          aList.DataSet.First;
          while not aList.DataSet.EOF do
            begin
              AddEntry;
              aList.DataSet.Next;
            end;
          aList.Free;
        end
      else if (Typ = 'S') and (Data.Users.Rights.Right('STATISTICS') > RIGHT_NONE) then //Statistics
        begin
          aList := TStatistic.CreateEx(Self,Data);
          aTyp := etStatistic;
          Data.SetFilter(aList,Data.QuoteField('TREEENTRY')+'='+Data.QuoteValue(ID),0,'','ASC',False,True,True);
          aList.DataSet.First;
          while not aList.DataSet.EOF do
            begin
              AddEntry;
              aList.DataSet.Next;
            end;
          aList.Free;
        end
      else if (Typ = 'F') then //Favorites
        begin
          aListL := TLinks.CreateEx(Self,Data);
          aTyp := etLink;
          Data.SetFilter(aListL,Data.QuoteField('RREF_ID')+'='+Data.QuoteValue(ID),0,'','ASC',False,True,True);
          aListL.DataSet.First;
          while not aListL.DataSet.EOF do
            begin
              Node1 := tvMain.Items.AddChildObject(Node,'',TTreeEntry.Create);
              TTreeEntry(Node1.Data).Rec := aListL.GetBookmark;
              with aListL.DataSet as IBaseManageDB do
                TTreeEntry(Node1.Data).Filter:=Data.QuoteField(TableName)+'.'+Data.QuoteField('SQL_ID')+'='+Data.QuoteValue(IntToStr(aListL.GetBookmark));
              TTreeEntry(Node1.Data).DataSourceType := TLinks;
              TTreeEntry(Node1.Data).Rec := aListL.GetBookmark;
              TTreeEntry(Node1.Data).Text[0] := aListL.FieldByName('NAME').AsString;
              TTreeEntry(Node1.Data).Link:=aListL.FieldByName('LINK').AsString;
              TTreeEntry(Node1.Data).Typ := etLink;
              TTreeEntry(Node1.Data).LinkIcon:=Data.GetLinkIcon(aListL.FieldByName('LINK').AsString);
              aListL.DataSet.Next;
            end;
          aListL.Free;
        end
      else if (Typ = 'Z') then //Clipboard
        begin
          aList := TClipp.CreateEx(Self,Data);
          aTyp := etClipboardItem;
          with aList.DataSet as IBaseDBFilter do
            Data.SetFilter(aList,Data.QuoteField('TREEENTRY')+'='+Data.QuoteValue(ID),0,'','ASC',False,True,True);
          aList.DataSet.First;
          while not aList.DataSet.EOF do
            begin
              AddEntry;
              aList.DataSet.Next;
            end;
          aList.Free;
        end
      except

      end;
    end
  else if (DataT.Typ=etProject) or (DataT.Typ=etProcess) then
    begin
      aTyp := etProject;
      Node.DeleteChildren;
      aProject := TProject.Create(nil);
      aProject.SelectFromParent(DataT.Rec);
      aProject.Open;
      aProject.DataSet.First;
      aList := aProject;
      aProject := nil;
      while not aList.DataSet.EOF do
        begin
          AddEntry;
          if aList.FieldByName('TYPE').AsString = 'C' then
            TTreeEntry(Node1.Data).Typ := etProcess;
          aList.DataSet.Next;
        end;
      aList.Free;
    end;
  Screen.Cursor:=crDefault;
  tvMain.EndUpdate;
end;
function TfMainTree.GetTreeEntry: Variant;
var
  DataT: TTreeEntry;
begin
  Result := -1;
  if not Assigned(tvMain.Selected) then exit;
  DataT := TTreeEntry(tvMain.Selected.Data);
  if not (DataT.Typ = etDir) then exit;
  Data.SetFilter(Data.Tree,'',0,'','ASC',False,True,True);
  if Data.Tree.GotoBookmark(DataT.Rec) then
    Result := Data.Tree.Id.AsVariant;
end;

function TfMainTree.GetBigIconTexts: string;
begin
  Result := '';
  Result := Result+GetEntryText(etSearch)+';';
  Result := Result+GetEntryText(etFavourites)+';';
  Result := Result+GetEntryText(etMessages)+';';
  Result := Result+GetEntryText(etTasks)+';';
  Result := Result+GetEntryText(etCalendar)+';';
  Result := Result+GetEntryText(etOrders)+';';
  Result := Result+GetEntryText(etCustomers)+';';
  Result := Result+GetEntryText(etMasterdata)+';';
  Result := Result+GetEntryText(etProjects)+';';
  Result := Result+GetEntryText(etWiki)+';';
  Result := Result+GetEntryText(etDocuments)+';';
  Result := Result+GetEntryText(etImages)+';';
  Result := Result+GetEntryText(etLists)+';';
  Result := Result+GetEntryText(etMeetings)+';';
  Result := Result+GetEntryText(etInventory)+';';
  Result := Result+GetEntryText(etFinancial)+';';
  Result := Result+GetEntryText(etStatistics)+';';
  Result := Result+GetEntryText(etAllObjects)+';';
end;

procedure TfMainTree.SaveTreeOptions;
var
  aNode: TTreeNode;
  aOpt,aExp: String;
begin
  aNode := tvMain.Items.GetFirstNode;
  aOpt := '';
  aExp := '';
  while Assigned(aNode) do
    begin
      aOpt := aOpt+GetNodeText(aNode)+';';
      if aNode.Expanded then
        aExp := aExp+GetNodeText(aNode)+';';
      aNode := aNode.GetNextSibling;
    end;
  with Application as IBaseDBInterface do
    begin
      if (trim(aOpt)<>'') then
        DBConfig.WriteString('TREEENTRYS:'+ApplicationName,aOpt);
      DBConfig.WriteString('TREEEXPAND:'+ApplicationName,aExp);
    end;
end;

procedure TfMainTree.RestoreExpands;
var
  aNode: TTreeNode;
  aExp: String;
begin
  aNode := tvMain.Items[0];
  with Application as IBaseDBInterface do
    aExp := DBConfig.ReadString('TREEEXPAND:'+ApplicationName,'');
  while Assigned(aNode) do
    begin
      if pos(GetNodeText(aNode)+';',aExp)>0 then
        aNode.Expand(False);
      aNode := aNode.GetNextSibling;
    end;
end;

function TfMainTree.GetNodeText(aNode: TTreeNode): string;
var
  aData: TTreeEntry;
  CellText: String;
begin
  if not Assigned(aNode.Data) then exit;
  aData := TTreeEntry(aNode.Data);
  if not Assigned(aData) then exit;
  Celltext := GetEntryText(aData.Typ);
  if Celltext = '' then
    CellText := aData.Text[0]
  else if Celltext = 'ACTION' then
    CellText := aData.Action.Caption;
  Result := CellText;
end;

function TfMainTree.GetEntryText(aNode: TEntryTyp): string;
var
  Celltext: String = '';
begin
  case aNode of
    etCustomers:Celltext := strCustomers;
    etCustomerList:Celltext := strCustomerList;
    etMasterdata:Celltext := strMasterdata;
    etTimeRegistering:Celltext := strTimetools;
    etOrders:Celltext := strOrders;
    etOrderList:Celltext := strOrderList;
    etTasks:Celltext := strTasks;
    etArticleList:Celltext := strArticleList;
    etMyTasks:Celltext := strMyTasks;
    etTaskPlan:CellText:=strTaskPlan;
    etAttPlan:CellText := strAttPlan;
    etSearch:CellText := strSearch;
    etFinancial:Celltext := strFinancial;
    etBanking:CellText := strBanking;
    etAccounts:Celltext := strAccounts;
    etNewAccount:Celltext:=strNewAccount;
    etNewTransfer:Celltext := strNewTransfer;
    etAccountingQue:Celltext := strAccountingQue;
    etCalendar:Celltext := strCalendar;
    etMyCalendar:Celltext := strMyCalendar;
    etProjects:Celltext := strProjects;
    etMessages:CellText := strMessages;
    etSalesList:Celltext := strSalesList;
    etWiki:Celltext := strWiki;
    etMeetings:CellText:=strMeetings;
    etMeetingList:CellText:=strMeetingList;
    etStatistics:Celltext := strStatistics;
    etFiles:Celltext := strFiles;
    etDocuments:Celltext := strDocumentsOnly;
    etImages:Celltext := strImages;
    etLists:Celltext := strLists;
    etFavourites:Celltext := strFavourites;
//    etStorage:CellText := strStorage;
//    etStorageJournal: CellText := fStorageJournal.Caption;
    etInventory:CellText := strInventory;
//    etWebshop:Celltext := fWebshop.Caption;
//    etDisposition:Celltext := fDisposition.Caption;
    etAllObjects:CellText := strAllElements;
    etAction:CellText := 'ACTION';
  end;
  Result := Celltext;
end;

initialization
end.
