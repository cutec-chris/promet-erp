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
Created 01.06.2006
*******************************************************************************}
unit uMain;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  ComCtrls, ExtCtrls, ActnList, Buttons, StdCtrls, uBaseApplication,
  uBaseDBClasses, uExtControls, uBaseVisualApplication,db,uBaseSearch,uMainTreeFrame,
  uWikiFrame,DBGrids,Grids, types, simpleipc,uEnterTime;
type
  THackListBox = class(TListBox);

  { TfMain }

  TfMain = class(TForm)
    acLogin: TAction;
    acLogout: TAction;
    acContact: TAction;
    acNewContact: TAction;
    acMasterdata: TAction;
    acNewMeeting: TAction;
    acOrders: TAction;
    acTasks: TAction;
    acProjects: TAction;
    acCalendar: TAction;
    acMessages: TAction;
    acTimeRegistering: TAction;
    acDeleteWholeMessageDir: TAction;
    acOpen: TAction;
    acNewTermin: TAction;
    acNewMasterdata: TAction;
    acNewOrder: TAction;
    acSalesList: TAction;
    acSalesListPay: TAction;
    acDeleteDirectory: TAction;
    acHelpIndex: TAction;
    acNewProject: TAction;
    acNewMessage: TAction;
    acCombineSaleItems: TAction;
    acNewTask: TAction;
    acShowTree: TAction;
    acForward: TAction;
    acBack: TAction;
    acInfo: TAction;
    acNewList: TAction;
    acDeleteListeEntry: TAction;
    acNewInventory: TAction;
    acCollectInventory: TAction;
    acBookInventory: TAction;
    acCloseTab: TAction;
    acChangePasswort: TAction;
    acClearList: TAction;
    acNewStatistics: TAction;
    acMeetings: TAction;
    acTaskPlan: TAction;
    acPauseTime: TAction;
    acStandartTime: TAction;
    acRefreshOrderList: TAction;
    acAttPlan: TAction;
    acNewAccount: TAction;
    acWiki: TAction;
    ActionList1: TActionList;
    ApplicationProperties1: TApplicationProperties;
    bBack: TSpeedButton;
    bDependencies: TSpeedButton;
    Bevel3: TBevel;
    Bevel5: TBevel;
    Bevel6: TBevel;
    bFfwd: TToolButton;
    bPauseTime: TSpeedButton;
    bSearch: TSpeedButton;
    eContains: TEdit;
    IPC: TSimpleIPCClient;
    IPCTimer: TIdleTimer;
    Image1: TImage;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    MenuItem3: TMenuItem;
    Panel3: TPanel;
    Panel6: TPanel;
    Panel7: TPanel;
    Panel8: TPanel;
    pTimes: TPanel;
    RefreshTimer: TIdleTimer;
    lbResults: TListBox;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    miHelpIndex: TMenuItem;
    miHelp: TMenuItem;
    miOptions: TMenuItem;
    miLanguage: TMenuItem;
    miSettings: TMenuItem;
    Panel1: TPanel;
    Panel2: TPanel;
    pmHistory: TPopupMenu;
    pSearch: TPanel;
    pcPages: TExtMenuPageControl;
    MainMenu1: TMainMenu;
    miView: TMenuItem;
    miLogout: TMenuItem;
    miLogin: TMenuItem;
    miMandant: TMenuItem;
    SpeedButton1: TSpeedButton;
    spTree: TSplitter;
    SearchTimer: TTimer;
    tbMenue: TToolButton;
    ToolBar1: TToolBar;
    ToolBar2: TToolBar;
    ToolButton2: TToolButton;
    tsHelp: TTabSheet;
    tvMain: TPanel;
    procedure acAttPlanExecute(Sender: TObject);
    procedure acBackExecute(Sender: TObject);
    procedure acBookInventoryExecute(Sender: TObject);
    procedure acCalendarExecute(Sender: TObject);
    procedure acChangePasswortExecute(Sender: TObject);
    procedure acClearListExecute(Sender: TObject);
    procedure acCloseTabExecute(Sender: TObject);
    procedure acCollectInventoryExecute(Sender: TObject);
    procedure acCombineSaleItemsExecute(Sender: TObject);
    procedure acContactExecute(Sender: TObject);
    procedure acDeleteListeEntryExecute(Sender: TObject);
    procedure acDeleteWholeMessageDirExecute(Sender: TObject);
    procedure acForwardExecute(Sender: TObject);
    procedure acHelpIndexExecute(Sender: TObject);
    procedure acInfoExecute(Sender: TObject);
    procedure acLoginExecute(Sender: TObject);
    procedure acLogoutExecute(Sender: TObject);
    procedure acMasterdataExecute(Sender: TObject);
    procedure acMeetingsExecute(Sender: TObject);
    procedure acMessagesExecute(Sender: TObject);
    procedure acNewAccountExecute(Sender: TObject);
    procedure acNewContactExecute(Sender: TObject);
    procedure acNewInventoryExecute(Sender: TObject);
    procedure acNewListExecute(Sender: TObject);
    procedure acNewMasterdataExecute(Sender: TObject);
    procedure acNewMeetingExecute(Sender: TObject);
    procedure acNewMessageExecute(Sender: TObject);
    procedure acNewOrderExecute(Sender: TObject);
    procedure acNewProjectExecute(Sender: TObject);
    procedure acNewStatisticsExecute(Sender: TObject);
    procedure acNewTerminExecute(Sender: TObject);
    procedure acOpenExecute(Sender: TObject);
    procedure acOrdersExecute(Sender: TObject);
    procedure acPauseTimeExecute(Sender: TObject);
    procedure acProjectsExecute(Sender: TObject);
    procedure acRefreshOrderListExecute(Sender: TObject);
    procedure acRenameDirectoryExecute(Sender: TObject);
    procedure acSalesListExecute(Sender: TObject);
    procedure acSalesListPayExecute(Sender: TObject);
    procedure acShowTreeExecute(Sender: TObject);
    procedure acStandartTimeExecute(Sender: TObject);
    procedure acTaskPlanExecute(Sender: TObject);
    procedure acTasksExecute(Sender: TObject);
    procedure acTimeRegisteringExecute(Sender: TObject);
    procedure acWikiExecute(Sender: TObject);
    procedure aFrameTfFilterClose(Sender: TObject; var CloseAction: TCloseAction
      );
    procedure aFrameTfFilterDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure aFrameTfFilterDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure aInvPosCloseClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure aItemClick(Sender: TObject);
    procedure ApplicationProperties1ShowHint(var HintStr: string;
      var CanShow: Boolean; var HintInfo: THintInfo);
    procedure ApplicationTBaseVisualApplicationUserTabAdded(Sender: TObject);
    procedure bSearchClick(Sender: TObject);
    procedure DataSearchresultItem(aIdent: string; aName: string;
      aStatus: string;aActive : Boolean; aLink: string; aItem: TBaseDBList=nil);
    procedure eContainsEnter(Sender: TObject);
    procedure eContainsExit(Sender: TObject);
    procedure eContainsKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure eContainsKeyPress(Sender: TObject; var Key: char);
    procedure fMainTreeFrameDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure fMainTreeFrameDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    function fMainTreeFrameNewFromLink(aLink: string; aSender: TObject
      ): TBaseDBDataSet;
    function fMainTreeFrameOpen(aEntry: TTreeEntry): Boolean;
    function fMainTreeFrameOpenFromLink(aLink: string; aSender: TObject
      ): Boolean;
    procedure fMainTreeFrameSelectionChanged(aEntry: TTreeEntry);
    procedure fMainTreeFrametvMainExpanding(Sender: TObject; Node: TTreeNode;
      var AllowExpansion: Boolean);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure IPCTimerTimer(Sender: TObject);
    procedure LanguageItemClick(Sender: TObject);
    procedure lbResultsDblClick(Sender: TObject);
    procedure lbResultsDrawItem(Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
    procedure lbResultsExit(Sender: TObject);
    procedure lbResultsKeyPress(Sender: TObject; var Key: char);
    procedure miOptionsClick(Sender: TObject);
    procedure miSettingsClick(Sender: TObject);
    function OpenAction(aLink: string; Sender: TObject): Boolean;
    function OpenOption(aLink: string; Sender: TObject): Boolean;
    procedure pmHistoryPopup(Sender: TObject);
    procedure DoRefreshActiveTab(Sender: TObject);
    procedure SearchTimerTimer(Sender: TObject);
    procedure SenderTfFiltergListDrawColumnCell(Sender: TObject;
      const Rect: TRect; DataCol: Integer; Column: TColumn;
      State: TGridDrawState);
    procedure SenderTfFilterViewDetails(Sender: TObject);

      procedure SenderTfMainTaskFrameControlsSenderTfMainTaskFrameTfTaskFrameStartTime
      (Sender: TObject; aProject, aTask: string);
    procedure TfFilteracOpenExecute(Sender: TObject);
  private
    { private declarations }
    FHistory: THistory;
    SearchText: String;
    ActiveSearch : TSearch;
    SearchLinks : TStringList;
    FMessageNode : TTreeNode;
    FCalendarNode : TTreeNode;
    FTaskNode : TTreeNode;
    FTimeReg : TfEnterTime;
    aTime : Int64;
    procedure AddCustomerList(Sender: TObject);
    procedure AddMasterdataList(Sender: TObject);
    procedure AddOrderList(Sender: TObject);
    procedure AddListsList(Sender: TObject);
    procedure AddDocPages(Sender: TObject);
    procedure AddInventoryList(Sender: TObject);
    procedure AddSalesList(Sender: TObject);
    procedure AddCalendar(Sender: TObject);
    procedure AddProjectList(Sender: TObject);
    procedure AddTaskList(Sender: TObject);
    procedure AddMeetingList(Sender : TObject);
    procedure AddWiki(Sender: TObject);
    function CommandReceived(Sender : TObject;aCommand : string) : Boolean;
    procedure RefreshCalendar;
    procedure RefreshMessages;
    procedure RefreshTasks;
    procedure IntSetLanguage(aLang : string);
    procedure ImportFavorites;
  public
    { public declarations }
    property TimeReg : TfEnterTime read FTimeReg;
    function DoCreate : Boolean;
  end;

  { TStaarterThread }

  { TStarterThread }

  TStarterThread = class(TThread)
  private
    Node,Node1,Node2,Node3,aNode : TTreeNode;
    miNew: TMenuItem;
    procedure NewNode;
    procedure NewNode1;
    procedure NewNode2;
    procedure NewNode3;
    procedure NewMenu;
    procedure ShowAll;
    procedure AddStatistics;
    procedure AddTimeReg;
    procedure AddTimeReg2;
  public
    constructor Create;
    procedure Execute; override;
  end;

var
  fMain: TfMain;
implementation
{$R *.lfm}
uses uBaseDBInterface,uIntfStrConsts,uSearch,uFilterFrame,uPerson,uData,
  uPersonFrame, uPrometFrames, uMessageFrame, uMessageEdit, LCLType, uCalendarFrame,
  uAccounting,uAccountingFrame,uAccountingQue,uAccountingTransfer,uMessages,uDocuments,
  uOrder,uArticleFrame,uMasterdata,uOrderFrame,uBookAccounting,
  uOptions,uUserOptions,uMandantOptions,uSystemOptions,uStateOptions,uCategoryOptions,uOrderTypeOptions,
  uUserFieldDefOptions,uStorageTypeOptions,uCurrencyOptions,uLanguageOptions,
  uRepairOptions, uSyncOptions, uDocumentOptions, uPhoneOptions, uMailOptions,
  uHelpContainer,uProjects,uProjectFrame,Math,uSkypePhone,LCLIntf,uWiki,
  uTask,uDocumentProcess,uDocumentFrame,uPrometFramesInplaceDB,uInfo,
  uProcessOptions,Utils,uBaseERPDBClasses,umaintasks,utasks,uTaskEdit,LCLProc,
  usplash,ufavorites,uBaseVisualControls,uStatisticFrame,uwait,uprometipc,uMeetingFrame,
  umeeting,uEditableTab,umanagedocframe,uBaseDocPages,uTaskPlan,uattendanceplan,
  uTimeFrame,uTimeOptions,uWizardnewaccount,uCalendar,
  uOptionsFrame
  {$ifdef WINDOWS}
  {$ifdef CPU32}
  ,uTAPIPhone
  {$endif}
  {$endif}
  ;
resourcestring
  strBrowserFavourites          = 'Browser-Lesezeichen';
  strAdding                     = 'initialisiere ';
  strNewTermin                  = 'Neuer Termin';
  strNewOrder                   = '%s erstellen';
  strWIki                       = 'Wiki';
  strSystem                     = 'System';
  strStates                     = 'Status';
  strCategory                   = 'Kategorie';
  strOrdertype                  = 'Auftragstypen';
  strUserFieldDefs              = 'Benutzerfelder';
  strStorageTypes               = 'Lagertypen';
  strCurrencies                 = 'Währungen';
  strLanguages                  = 'Sprachen';
  strRepair                     = 'Reparatur';
  strPhones                     = 'Telefonie';
  strMessageAccounts            = 'Nachrichtenkonten';
  strSearchText                 = '<hier tippen um zu suchen>';
  strProcessesOpen              = 'Es sind noch Dateien offen (Dateiverwaltung), wirklich schließen ?';
  strNewInventory               = 'Neue Inventur';
  strInventory                  = 'Inventur';
  strDeletingMessages           = 'lösche Nachrichten, noch %d Nachichten';
  strLogin                      = 'Login...';

function OnMessageReceived(aMessage: string): Boolean;
var
  tmp: String;
begin
  Result := False;
  if copy(aMessage,0,9) = 'OpenLink(' then
    begin
      tmp := copy(aMessage,10,length(aMessage));
      tmp := copy(tmp,0,length(tmp)-1);
      Data.GotoLink(tmp);
      Result := True;
    end;
end;

procedure TfMain.RefreshMessages;
begin
  if not Assigned(FMessageNode) then exit;
  uMessageFrame.RefreshMessages(FMessageNode);
  if Assigned(pcPages.ActivePage) and (pcPages.ActivePage.ControlCount > 0) and (pcPages.ActivePage.Controls[0] is TfMessageFrame) then
    TfMessageFrame(pcPages.ActivePage.Controls[0]).DoRefresh;
end;
procedure TfMain.RefreshCalendar;
begin
  uCalendarFrame.RefreshCalendar(FCalendarNode);
end;
procedure TfMain.RefreshTasks;
begin
  uTasks.RefreshTasks(FTaskNode);
end;
procedure TfMain.IntSetLanguage(aLang: string);
begin
  try
    LoadLanguage(aLang);
    fOptions.SetLanguage;
  except
  end;
end;
procedure TfMain.ImportFavorites;
var
  aList : TStrings;
  aLinks: TLinks;
  i: Integer;
  aLink: String;
begin
  Data.SetFilter(Data.Tree,Data.QuoteField('NAME')+'='+Data.QuoteValue(strBrowserFavourites)+' and '+Data.QuoteField('TYPE')+'='+Data.QuoteValue('F'),0,'','ASC',False,True,True);
  if Data.Tree.Count > 0 then
    begin
      aList := GetFavorites;
      aLinks := TLinks.Create(nil,Data);
      Data.SetFilter(aLinks,Data.QuoteField('REFERENCE')+'='+Data.QuoteValue('BROWSERIMPORT'));
      for i := aList.Count-1 downto 0 do
        begin
          aLink := aList.ValueFromIndex[i];
          if not aLinks.DataSet.Locate('LINK',aLink,[]) then
            begin
              aLinks.Append;
              aLinks.FieldByName('RREF_ID').AsVariant := Data.Tree.Id.AsVariant;
              aLinks.FieldByName('NAME').AsString := aList.Names[i];
              aLinks.FieldByName('LINK').AsString := aLink;
              aLinks.FieldByName('ICON').AsInteger := IMAGE_WEBSITE;
              aLinks.FieldByName('REFERENCE').AsString := 'BROWSERIMPORT';
              aLinks.FieldByName('CHANGEDBY').AsString := Data.Users.FieldByName('IDCODE').AsString;
              aLinks.DataSet.Post;
            end
          else break;
        end;
      aList.Free;
    end;
end;
function TfMain.DoCreate : Boolean;
begin
  fMainTreeFrame.tvMain.OnExpanding:=@fMainTreeFrametvMainExpanding;
  acLogin.Execute;
  Result := not acLogin.Enabled;
end;
function TfMain.CommandReceived(Sender: TObject; aCommand: string): Boolean;
begin
  Result := False;
  if aCommand = 'Message.refresh' then
    begin
      RefreshMessages;
      Result := True;
    end;
end;
procedure TfMain.AddCustomerList(Sender : TObject);
begin
  with Sender as TfFilter do
    begin
      TabCaption := strCustomerList;
      FilterType:='C';
      DefaultRows:='GLOBALWIDTH:680;ACCOUNTNO:100;NAME:400;MATCHCODE:200;';
      Dataset := TPersonList.Create(nil,Data);
      //gList.OnDrawColumnCell:=nil;
      if Data.Users.Rights.Right('CUSTOMERS') > RIGHT_READ then
        AddToolbarAction(acNewContact);
    end;
end;
procedure TfMain.AddMasterdataList(Sender: TObject);
begin
  with Sender as TfFilter do
    begin
      TabCaption := strArticleList;
      FilterType:='M';
      DefaultRows:='GLOBALWIDTH:910;ID:150;VERSION:100;LANGUAGE:60;MATCHCODE:200;SHORTTEXT:400;';
      Dataset := TMasterdataList.Create(nil,Data);
      //gList.OnDrawColumnCell:=nil;
      if (Data.Users.Rights.Right('MASTERDATA') > RIGHT_READ) or (Data.Users.Rights.Right('ARTICLES') > RIGHT_READ) or (Data.Users.Rights.Right('BENEFITS') > RIGHT_READ) or (Data.Users.Rights.Right('PARTSLIST') > RIGHT_READ) then
        AddToolbarAction(acNewMasterdata);
    end;
end;
procedure TfMain.AddOrderList(Sender: TObject);
var
  forderFrame : TfOrderFrame;
begin
  with Sender as TfFilter do
    begin
      TabCaption := strOrderList;
      FilterType:='O';
      DefaultRows:='GLOBALWIDTH:659;STATUS:50;NUMBER:100;CUSTNO:100;CUSTNAME:300;PAYEDON:28;DELIVERED:28;DONE:28;';
      Dataset := TOrderList.Create(nil,Data);
      OnDrawColumnCell:=@fOrderFrame.gListDrawColumnCell;
      if Data.Users.Rights.Right('ORDERS') > RIGHT_READ then
        AddToolbarAction(acNewOrder);
      if Data.Users.Rights.Right('ORDERS') >= RIGHT_PERMIT then
        AddContextAction(acRefreshOrderList);
    end;
end;
procedure TfMain.AddListsList(Sender: TObject);
begin
  with Sender as TfFilter do
    begin
      TabCaption := strLists;
      FilterType:='L';
      DefaultRows:='GLOBALWIDTH:100;NAME:100;';
      Dataset := TLists.Create(nil,Data);
      //gList.OnDrawColumnCell:=nil;
      Editable:=True;
      AddToolbarAction(acNewList);
    end;
end;

procedure TfMain.AddDocPages(Sender: TObject);
begin
  with Sender as TfManageDocFrame do
    begin
      TabCaption := strDocuments;
      Open;
      if Assigned(TFrame(Sender).OnEnter) then
        TFrame(Sender).OnEnter(Sender)
    end;
end;

procedure TfMain.AddInventoryList(Sender: TObject);
begin
  with Sender as TfFilter do
    begin
      TabCaption := strInventorys;
      FilterType:='INV';
      DefaultRows:='GLOBALWIDTH:240;INVNO:40;DESC:100;DATE:70;CREATEDBY:30;';
      Dataset := TInventorys.Create(nil,Data);
      //gList.OnDrawColumnCell:=nil;
      Editable:=True;
      AddToolbarAction(acNewInventory);
    end;
end;
procedure TfMain.AddSalesList(Sender: TObject);
var
  ads: TAccountingJournal;
begin
  with Sender as TfFilter do
    begin
      TabCaption := strSalesList;
      FilterType:='S';
      DefaultRows:='GLOBALWIDTH:825;PAYEDON:100;ORDERNO:100;STATUS:30;NUMBER:100;CUSTNO:70;CUSTNAME:100;NETPRICE:50;DISCOUNT:50;VATH:50;VATF:50;GROSSPRICE:100;';
      ads := TAccountingJournal.Create(nil,Data);
      ads.CreateTable;
      Dataset := ads;
      //gList.OnDrawColumnCell:=nil;
      AddToolbarAction(acSalesListPay);
      OnViewDetails :=@SenderTfFilterViewDetails;
//      AddToolbarAction(acCombineSaleItems);
    end;
end;
procedure TfMain.AddCalendar(Sender: TObject);
begin
  with Sender as TfCalendarFrame do
    begin
      Caption := strCalendar;
      OpenDir(Data.Users.Id.AsInteger);
    end;
end;
procedure TfMain.AddProjectList(Sender: TObject);
var
  fProjectFrame : TfProjectFrame;
begin
  with Sender as TfFilter do
    begin
      TabCaption := strProjectList;
      FilterType:='P';
      DefaultRows:='GLOBALWIDTH:280;TYPE:30;ID:70;NAME:100;STATUS:60;';
      Dataset := TProjectList.Create(nil,Data);
      OnDrawColumnCell:=@SenderTfFiltergListDrawColumnCell;
      if Data.Users.Rights.Right('PROJECTS') > RIGHT_READ then
        AddToolbarAction(acNewProject);
    end;
end;
procedure TfMain.AddTaskList(Sender: TObject);
var
  aDataset: TTaskList;
begin
  with Sender as TfMainTaskFrame do
    begin
      with Controls[0] as TfTaskFrame do
        begin
          GridView.SortField:='LPRIORITY';
          aDataset := TTaskList.Create(nil,Data);
          BaseFilter:=Data.QuoteField('ACTIVE')+'='+Data.QuoteValue('Y');
          aDataSet.Open;
          tbTop.Visible:=True;
          tbLeft.Visible:=False;
          DataSet := aDataSet;
          SetRights(True);
          FTaskNode := Self.FTaskNode;
          OnStartTime:=@SenderTfMainTaskFrameControlsSenderTfMainTaskFrameTfTaskFrameStartTime;
        end;
  end;
end;
procedure TfMain.AddMeetingList(Sender: TObject);
begin
  with Sender as TfFilter do
    begin
      TabCaption := strMeetingList;
      FilterType:='E';
      DefaultRows:='GLOBALWIDTH:180;NAME:100;STATUS:60;';
      Dataset := TMeetings.Create(nil,Data);
      //gList.OnDrawColumnCell:=nil;
      AddToolbarAction(acNewMeeting);
    end;
end;
procedure TfMain.AddWiki(Sender: TObject);
begin
  with Sender as TfWikiFrame do
    begin
      TabCaption := strWiki;
      SetRights(Data.Users.Rights.Right('WIKI')>RIGHT_READ);
      OpenFromLink('WIKI@INDEX');
    end;
end;
{ TStaarterThread }

procedure TStarterThread.NewNode;
begin
  Node := fMainTreeFrame.tvMain.Items.AddChildObject(nil,'',TTreeEntry.Create);
end;

procedure TStarterThread.NewNode1;
begin
  Node1 := fMainTreeFrame.tvMain.Items.AddChildObject(Node,'',TTreeEntry.Create);
end;

procedure TStarterThread.NewNode2;
begin
  Node2 := fMainTreeFrame.tvMain.Items.AddChildObject(Node1.Parent,'',TTreeEntry.Create);
end;

procedure TStarterThread.NewNode3;
begin
  Node3 := fMainTreeFrame.tvMain.Items.AddChildObject(Node2,'',TTreeEntry.Create);
end;

procedure TStarterThread.NewMenu;
begin
  miNew := TmenuItem.Create(fMain.miView);
  fMain.miView.Add(miNew);
end;

procedure TStarterThread.ShowAll;
begin
  //fMain.Invalidate;
  fMainTreeFrame.tvMain.Invalidate;
end;

procedure TStarterThread.AddStatistics;
begin
  uStatisticFrame.AddToMainTree(fMain.acNewStatistics);
end;

procedure TStarterThread.AddTimeReg;
begin
  if (Data.Users.Rights.Right('TIMEREG') > RIGHT_NONE) then
    begin
      if not fMain.IPC.ServerRunning then
        begin
          fOptions.RegisterOptionsFrame(TfTimeOptions.Create(fOptions),strTimetools,strPersonalOptions);
          Application.CreateForm(TfEnterTime,fMain.FTimeReg);
          fMain.FTimeReg.Node:=MainNode;
          fMain.FTimeReg.PauseBtn := fMain.bPauseTime;
          fMain.FTimeReg.DoSetup;
          fMain.FTimeReg.SetupDB;
          fMain.pTimes.Visible := True;
        end;
    end;
end;

procedure TStarterThread.AddTimeReg2;
begin
  if (Data.Users.Rights.Right('TIMEREG') > RIGHT_NONE) then
    begin
      if not fMain.IPC.ServerRunning then
        begin
          MainNode := fMainTreeFrame.tvMain.Items.AddChildObject(nil,'',TTreeEntry.Create);
          MainNode.Height := 34;
          TTreeEntry(MainNode.Data).Typ := etTimeRegistering;
          fMain.FTimeReg.refreshNode;
        end;
    end;
end;

constructor TStarterThread.Create;
begin
  FreeOnTerminate:=True;
  inherited Create(False);
end;

procedure TStarterThread.Execute;
var
  aTime: QWord;
  aDocuments: TDocument;
  aDataSet: TBaseDbDataSet;
  aTree: TTree;
  aOrderType: TOrderTyp;
  DefaultOrder: Boolean;
  aConn: TComponent;
  aDS: TMeetings;
  Accounts: TAccounts;
  aCal: TCalendar;
begin
  aConn := Data.GetNewConnection;
  aTree := TTree.Create(nil,Data,aConn);
  Synchronize(@NewMenu);
  miNew.Action := fMainTreeFrame.acSearch;
  //Timeregistering
  Synchronize(@AddTimeReg);
  //Documents
  aDocuments := TDocument.Create(nil,Data);
  aDocuments.CreateTable;
  aDocuments.Destroy;
  //Messages
  aDataSet := TMessage.Create(nil,Data,aConn);
  TMessage(aDataSet).CreateTable;
  aDataSet.Destroy;
  fMain.pcPages.AddTabClass(TfMessageFrame,strMessages,nil,Data.GetLinkIcon('MESSAGEIDX@'),True);
  Data.RegisterLinkHandler('MESSAGEIDX',@fMainTreeFrame.OpenLink,@fMainTreeFrame.NewFromLink);
  AddSearchAbleDataSet(TMessageList);
  AddSearchAbleDataSet(TLists);
  {$ifndef heaptrc}
  try
    TBaseVisualApplication(Application).MessageHandler.SendCommand('*receiver','Receive('+Data.Users.FieldByName('NAME').AsString+')');
  except
  end;
  {$endif}
  fMain.RefreshMessages;
  //Add PIM Entrys
  if Data.Users.Rights.Right('CALENDAR') > RIGHT_NONE then
    begin
      aCal := TCalendar.Create(nil,Data);
      aCal.CreateTable;
      acal.Free;
      fMain.pcPages.AddTabClass(TfCalendarFrame,strCalendar,@fMain.AddCalendar,Data.GetLinkIcon('CALENDAR@'),True);
      fMain.RefreshCalendar;
    end;
  //Orders
  aDataSet := TOrder.Create(nil,Data,aConn);
  TOrder(aDataSet).CreateTable;
  aDataSet.Destroy;
  fMain.pcPages.AddTabClass(TfFilter,strOrderList,@fMain.AddOrderList,Data.GetLinkIcon('ORDERS@'),True);
  Data.RegisterLinkHandler('ORDERS',@fMainTreeFrame.OpenLink,@fMainTreeFrame.NewFromLink);
  AddSearchAbleDataSet(TOrderList);

  //Add Contacts
  {$region}
  if Data.Users.Rights.Right('CUSTOMERS') > RIGHT_NONE then
    begin
      aDataSet := TPerson.Create(nil,Data,aConn);
      TPerson(aDataSet).CreateTable;
      aDataSet.Destroy;
      Data.Countries.CreateTable;
      fMain.pcPages.AddTabClass(TfFilter,strCustomerList,@fMain.AddCustomerList,Data.GetLinkIcon('CUSTOMERS@'),True);
      Data.RegisterLinkHandler('CUSTOMERS',@fMainTreeFrame.OpenLink,@fMainTreeFrame.NewFromLink);
      AddSearchAbleDataSet(TPersonList);
      AddSearchAbleDataSet(TPersonContactData);
      AddSearchAbleDataSet(TPersonAddress);
      Synchronize(@NewMenu);
      miNew.Action := fMain.acContact;
      NewNode;
      TTreeEntry(Node.Data).Typ := etCustomers;
      Node.Height := 34;
      NewNode1;
      TTreeEntry(Node1.Data).Typ := etCustomerList;
      NewNode1;
      TTreeEntry(Node1.Data).Typ := etAction;
      TTreeEntry(Node1.Data).Action := fMain.acNewContact;
      Data.SetFilter(aTree,'(('+Data.QuoteField('PARENT')+'=0) and ('+Data.QuoteField('TYPE')+'='+Data.QuoteValue('C')+'))',0,'','ASC',False,True,True);
      aTree.DataSet.First;
      while not aTree.dataSet.EOF do
        begin
          NewNode1;
          TTreeEntry(Node1.Data).Rec := Data.GetBookmark(aTree);
          TTreeEntry(Node1.Data).DataSource := aTree;
          TTreeEntry(Node1.Data).Text[0] := aTree.FieldByName('NAME').AsString;
          TTreeEntry(Node1.Data).Typ := etDir;
          fMainTreeFrame.tvMain.Items.AddChildObject(Node1,'1',TTreeEntry.Create);
          aTree.DataSet.Next;
        end;
    end;
  {$endregion}
  //debugln('Contacts: '+IntToStr(GetTickCount64-aTime));
  Synchronize(@ShowAll);
  //Add Masterdata stuff
  {$region}
  if (Data.Users.Rights.Right('MASTERDATA') > RIGHT_NONE) then
    begin
      aDataSet := TMasterdata.Create(nil,Data,aConn);
      TMasterdata(aDataSet).CreateTable;
      aDataSet.Destroy;
      fMain.pcPages.AddTabClass(TfFilter,strArticleList,@fMain.AddMasterdataList,Data.GetLinkIcon('MASTERDATA@'),True);
      Data.RegisterLinkHandler('MASTERDATA',@fMainTreeFrame.OpenLink,@fMainTreeFrame.NewFromLink);
      AddSearchAbleDataSet(TMasterdataList);
      Synchronize(@NewMenu);
      miNew.Action := fMain.acMasterdata;
      NewNode;
      Node.Height := 32;
      TTreeEntry(Node.Data).Typ := etMasterdata;
      NewNode1;
      TTreeEntry(Node1.Data).Typ := etArticleList;
      NewNode1;
      TTreeEntry(Node1.Data).Typ := etAction;
      TTreeEntry(Node1.Data).Action := fMain.acNewMasterdata;
      Data.SetFilter(aTree,'(('+Data.QuoteField('PARENT')+'=0) and ('+Data.QuoteField('TYPE')+'='+Data.QuoteValue('M')+'))',0,'','ASC',False,True,True);
      aTree.DataSet.First;
      while not aTree.dataSet.EOF do
        begin
          NewNode1;
          TTreeEntry(Node1.Data).Rec := Data.GetBookmark(aTree);
          TTreeEntry(Node1.Data).DataSource := aTree;
          TTreeEntry(Node1.Data).Text[0] := aTree.FieldByName('NAME').AsString;
          TTreeEntry(Node1.Data).Typ := etDir;
          fMainTreeFrame.tvMain.Items.AddChildObject(Node1,'',TTreeEntry.Create);
          aTree.DataSet.Next;
        end;
    end;
  {$endregion}
  //debugln('Masterdata: '+IntToStr(GetTickCount64-aTime));
  Synchronize(@ShowAll);
  //Projects
  uProjectFrame.AddToMainTree(fMain.acNewProject);
  if (Data.Users.Rights.Right('PROJECTS') > RIGHT_NONE) then
    begin
      fMain.pcPages.AddTabClass(TfFilter,strProjectList,@fMain.AddProjectList,Data.GetLinkIcon('PROJECTS@'),True);
    end;
  //debugln('Projects: '+IntToStr(GetTickCount64-aTime));
  Synchronize(@ShowAll);
  //Wiki
  {$region}
  if (Data.Users.Rights.Right('WIKI') > RIGHT_NONE) then
    begin
      fMain.pcPages.AddTabClass(TfWikiFrame,strWiki,@fMain.AddWiki,Data.GetLinkIcon('WIKI@'),True);
      Data.RegisterLinkHandler('WIKI',@fMainTreeFrame.OpenLink,@fMainTreeFrame.NewFromLink);
      AddSearchAbleDataSet(TWikiList);
      NewNode;
      Node.Height := 34;
      TTreeEntry(Node.Data).Typ := etWiki;
      Data.SetFilter(aTree,'(('+Data.QuoteField('PARENT')+'=0) and ('+Data.QuoteField('TYPE')+'='+Data.QuoteValue('W')+'))',0,'','ASC',False,True,True);
      aTree.DataSet.First;
      while not aTree.dataSet.EOF do
        begin
          NewNode1;
          TTreeEntry(Node1.Data).Rec := Data.GetBookmark(aTree);
          TTreeEntry(Node1.Data).DataSource := aTree;
          TTreeEntry(Node1.Data).Text[0] := aTree.FieldByName('NAME').AsString;
          TTreeEntry(Node1.Data).Typ := etDir;
          fMainTreeFrame.tvMain.Items.AddChildObject(Node1,'',TTreeEntry.Create);
          aTree.DataSet.Next;
        end;
    end;
  {$endregion}
  //debugln('Wiki: '+IntToStr(GetTickCount64-aTime));
  Synchronize(@ShowAll);
  {$region}
  try
    if (Data.Users.Rights.Right('DOCUMENTS') > RIGHT_NONE) then
      begin
        Data.RegisterLinkHandler('DOCUMENTS',@fMainTreeFrame.OpenLink);
        NewNode;
        TTreeEntry(Node.Data).Typ := etFiles;
        umanagedocframe.AddToMainTree;
      end;
  except
  end;
  {$endregion}
  //debugln('Documents: '+IntToStr(GetTickCount64-aTime));
  Synchronize(@ShowAll);
  {$region}
  if (Data.Users.Rights.Right('LISTS') > RIGHT_NONE) then
    begin
      aDataSet := TLists.Create(nil,Data,aConn);
      TLists(aDataSet).CreateTable;
      aDataSet.Destroy;
      Data.RegisterLinkHandler('LISTS',@fMainTreeFrame.OpenLink);
      NewNode;
      TTreeEntry(Node.Data).Typ := etLists;
    end;
  {$endregion}
  //debugln('Lists: '+IntToStr(GetTickCount64-aTime));
  Synchronize(@ShowAll);
  {$region}
  if (Data.Users.Rights.Right('MEETINGS') > RIGHT_NONE) then
    begin
      umeetingframe.AddToMainTree(fMain.acNewMeeting);
      fMainTreeFrame.tvMain.Items[0].Expanded:=True;
      fMain.pcPages.AddTabClass(TfFilter,strMeetingList,@fMain.AddMeetingList,-1,True);
      Data.RegisterLinkHandler('MEETINGS',@fMainTreeFrame.OpenLink);
      aDS := TMeetings.Create(nil,Data,aConn);
      aDS.CreateTable;
      aDS.Free;
    end;
  {$endregion}
  //debugln('Doc/Lists: '+IntToStr(GetTickCount64-aTime));
  Synchronize(@ShowAll);
  {$region}
  if (Data.Users.Rights.Right('INVENTORY') > RIGHT_NONE) then
    begin
      aDataSet := TInventorys.Create(nil,Data,aConn);
      TInventorys(aDataSet).CreateTable;
      aDataSet.Destroy;
      Data.RegisterLinkHandler('INVENTORY',@fMainTreeFrame.OpenLink);
      NewNode;
      TTreeEntry(Node.Data).Typ := etInventory;
    end;
  {$endregion}
  //Financial
  {$region}
  if (Data.Users.Rights.Right('BANKACCNTS') > RIGHT_NONE)
  or (Data.Users.Rights.Right('SALESLIST') > RIGHT_NONE) then
    begin
      NewNode;
      Node.Height := 34;
      TTreeEntry(Node.Data).Typ := etFinancial;
      if Data.Users.Rights.Right('BANKACCNTS') > RIGHT_NONE then
        begin
          Data.RegisterLinkHandler('ACCOUNTEXCHANGE',@fMainTreeFrame.OpenLink);
          NewNode1;
          TTreeEntry(Node1.Data).Typ := etBanking;
          NewNode2;
          TTreeEntry(Node2.Data).Typ := etAccounts;
          NewNode3;
          TTreeEntry(Node3.Data).Typ := etNewAccount;
          Accounts := TAccounts.Create(nil,Data,aConn);
          Accounts.CreateTable;
          Accounts.Open;
          Accounts.DataSet.First;
          while not Accounts.DataSet.EOF do
            begin
              NewNode3;
              TTreeEntry(Node3.Data).Rec := Accounts.GetBookmark;
              TTreeEntry(Node3.Data).Text[0] := Accounts.FieldByName('NAME').AsString;
              TTreeEntry(Node3.Data).Typ := etAccount;
              Accounts.DataSet.Next;
            end;
          Accounts.Free;
          NewNode2;
          TTreeEntry(Node2.Data).Typ := etNewTransfer;
          NewNode2;
          TTreeEntry(Node2.Data).Typ := etAccountingQue;
        end;
      if Data.Users.Rights.Right('SALESLIST') > RIGHT_NONE then
        begin
          fMain.pcPages.AddTabClass(TfFilter,strSalesList,@fMain.AddSalesList,-1,True);
          NewNode1;
          TTreeEntry(Node1.Data).Typ := etSalesList;
          TTreeEntry(Node1.Data).Action := fMain.acSalesList;
        end;
    end;
  {$endregion}
  //debugln('Inventory/Financial: '+IntToStr(GetTickCount64-aTime));
  //Add Statistics
  {$ifndef heaptrc}
  Synchronize(@AddStatistics);
  {$endif}
  //Timeregistering
  Synchronize(@AddTimeReg2);
  AddSearchAbleDataSet(TUser);
  {$IFDEF CPU32}
  uSkypePhone.RegisterPhoneLines;
  {$ENDIF}
  {$IFDEF WINDOWS}
  {$IFDEF CPU32}
  uTAPIPhone.RegisterPhoneLines;
  {$ENDIF}
  {$ENDIF}


  if Application.HasOption('startuptype') then
    begin
      if fMainTreeFrame.tvMain.Items.Count > 0 then
        aNode := fMainTreeFrame.tvMain.Items[0];
      while Assigned(aNode) do
        begin
          if (Application.GetOptionValue('startuptype') = aNode.Text)
          or (Application.GetOptionValue('startuptype') = fMainTreeFrame.GetNodeText(aNode)) then
            begin
              fMainTreeFrame.tvMain.Selected := aNode;
              break;
            end;
          aNode := aNode.GetNextSibling;
        end;
    end;
  aTree.Free;
  aConn.Free;
end;

procedure TfMain.acLoginExecute(Sender: TObject);
var
  Node: TTreeNode;
  bStart: TStarterThread;
  miNew: TMenuItem;
  aWiki: TWikiList;
  WikiFrame: TfWikiFrame;
  aDocuments: TDocument;
  procedure NewNode;
  begin
    Node := fMainTreeFrame.tvMain.Items.AddChildObject(nil,'',TTreeEntry.Create);
    fMainTreeFrame.tvMain.Items.AddChild(Node,'');
  end;
  procedure NewMenu;
  begin
    miNew := TmenuItem.Create(fMain.miView);
    fMain.miView.Add(miNew);
  end;

begin
  fMain.Hide;
  aTime := GetTickCount64;
  fSplash := TfSplash.Create(Self);
  try
    with Application as IBaseApplication do
      fSplash.lVersion.Caption:= strVersion+' '+StringReplace(FormatFloat('0.0', AppVersion),',','.',[])+'.'+IntToStr(AppRevision);
    fSplash.Show;
    fSplash.AddText(strLogin);;
    Application.ProcessMessages;
    with Application as IBaseApplication do
      if not Login then
        begin
          Application.Terminate;
          exit;
        end;
    with Application as IBaseDBInterface do
      begin
        acLogin.Enabled:=False;
        acLogout.Enabled:=True;
        with BaseApplication as IBaseDBInterface do
          Caption := MandantName+' - Promet-ERP';
        if Assigned(TBaseVisualApplication(Application).MessageHandler) then
          TBaseVisualApplication(Application).MessageHandler.RegisterCommandHandler(@CommandReceived);
        //debugln('BaseLogin: '+IntToStr(GetTickCount64-aTime));
        aWiki := TWikiList.Create(nil,Data);
        aWiki.CreateTable;
        aWiki.Free;
        WikiFrame := TfWikiFrame.Create(Self);
        WikiFrame.Parent := tsHelp;
        WikiFrame.Align := alClient;
        try
          WikiFrame.OpenWikiPage('Promet-ERP-Help/index',True);
        except
        end;
        WikiFrame.SetRights(Data.Users.Rights.Right('WIKI')>RIGHT_READ);
        //debugln('Wiki: '+IntToStr(GetTickCount64-aTime));
        //Add Search Node
        Node := fMainTreeFrame.tvMain.Items.AddChildObject(nil,'',TTreeEntry.Create);
        Node.Height := 34;
        TTreeEntry(Node.Data).Typ := etSearch;
        //Actions
        Data.RegisterLinkHandler('ACTION',@OpenAction);
        //Options
        Data.RegisterLinkHandler('OPTION',@OpenOption);
        NewNode;
        Node.Height := 34;
        TTreeEntry(Node.Data).Typ := etFavourites;
        //Messages
        if Data.Users.Rights.Right('MESSAGES') > RIGHT_NONE then
          begin
            NewMenu;
            miNew.Action := fMain.acMessages;
            NewNode;
            Node.Height := 34;
            TTreeEntry(Node.Data).Typ := etMessages;
            fMainTreeFrame.StartupTypes.Add(strMessages);
            fMain.FMessageNode := Node;
          end;
        //Tasks
        if (Data.Users.Rights.Right('TASKS') > RIGHT_NONE) then
          begin
            NewNode;
            Node.Height := 34;
            TTreeEntry(Node.Data).Typ := etTasks;
            FTaskNode := Node;
          end;
        //PIM
        if Data.Users.Rights.Right('CALENDAR') > RIGHT_NONE then
          begin
            NewMenu;
            miNew.Action := fMain.acCalendar;
            NewNode;
            Node.Height := 34;
            TTreeEntry(Node.Data).Typ := etCalendar;
            FCalendarNode := Node;
          end;
        //Orders,Production,...
        if Data.Users.Rights.Right('ORDERS') > RIGHT_NONE then
          begin
            NewMenu;
            miNew.Action := fMain.acOrders;
            NewNode;
            Node.Height := 32;
            TTreeEntry(Node.Data).Typ := etOrders;
           end;

        //bStart := TStarterThread.Create;

        with Application as IBaseDbInterface do
          FHistory.Text := DBConfig.ReadString('HISTORY','');
        if Application.HasOption('hidetree') then
          begin
            tvMain.Visible:=False;
            spTree.Visible:=False;
            acShowTree.Checked:=False;
          end;
      end;
    //debugln('LoginTime: '+IntToStr(GetTickCount64-aTime));
  finally
    fSplash.Hide;
    fMain.Visible:=True;
  end;
  IPCTimer.Enabled:=True;
end;
procedure TfMain.acContactExecute(Sender: TObject);
var
  i: Integer;
  Found: Boolean = false;
  aFrame: TfFilter;
begin
  Application.ProcessMessages;
  for i := 0 to pcPages.PageCount-2 do
    if (pcPages.Pages[i].ControlCount > 0) and (pcPages.Pages[i].Controls[0] is TfFilter) and (TfFilter(pcPages.Pages[i].Controls[0]).Dataset is TPersonList) then
      begin
        pcPages.PageIndex:=i;
        Found := True;
      end;
  if not Found then
    begin
      aFrame := TfFilter.Create(Self);
      pcPages.AddTab(aFrame,True,'',Data.GetLinkIcon('CUSTOMERS@'),False);
      AddCustomerList(aFrame);
      aFrame.Open;
    end;
end;
procedure TfMain.acDeleteListeEntryExecute(Sender: TObject);
begin
  if Assigned(pcPages.ActivePage) and (pcPages.ActivePage.ControlCount > 0) and (pcPages.ActivePage.Controls[0] is TfFilter) then
    begin
      TfFilter(pcPages.ActivePage.Controls[0]).DataSet.Delete;
    end;
end;
procedure TfMain.acBackExecute(Sender: TObject);
begin
  FHistory.GoBack;
end;

procedure TfMain.acAttPlanExecute(Sender: TObject);
var
  aFrame: TfAttPlan;
begin
  aFrame := TfAttPlan.Create(Self);
  aFrame.TabCaption := strAttPlan;
  pcPages.AddTab(aFrame,True,'',Data.GetLinkIcon('TASKS@'),False);
  aFrame.Populate(Data.Users.FieldByName('PARENT').AsVariant,Data.Users.Id.AsVariant);
end;

procedure TfMain.acBookInventoryExecute(Sender: TObject);
begin

end;

procedure TfMain.acCalendarExecute(Sender: TObject);
var
  i: Integer;
  Found: Boolean = false;
  aFrame: TfCalendarFrame;
begin
  Application.ProcessMessages;
  for i := 0 to pcPages.PageCount-2 do
    if (pcPages.Pages[i].ControlCount > 0)
    and (pcPages.Pages[i].Controls[0] is TfCalendarFrame)
    and (TfCalendarFrame(pcPages.Pages[i].Controls[0]).aDirectory = IntToStr(Data.Users.Id.AsInteger))
    then
      begin
        pcPages.PageIndex:=i;
        Found := True;
      end;
  if not Found then
    begin
      aFrame := TfCalendarFrame.Create(Self);
      pcPages.AddTab(aFrame,True,'',Data.GetLinkIcon('CALENDAR@'));
      AddCalendar(aFrame);
    end;
end;
procedure TfMain.acChangePasswortExecute(Sender: TObject);
begin
  with Application as IBaseApplication do
    ChangePasswort;
end;
procedure TfMain.acClearListExecute(Sender: TObject);
begin
  if Assigned(pcPages.ActivePage) and (pcPages.ActivePage.ControlCount > 0) and (pcPages.ActivePage.Controls[0] is TfFilter) then
    begin
      with TfFilter(pcPages.ActivePage.Controls[0]).DataSet.DataSet do
        begin
          First;
          while not EOF do
            begin
              Edit;
              FieldByName('ACTIVE').AsString:='N';
              Post;
              Next;
            end;

        end;
    end;
end;
procedure TfMain.acCloseTabExecute(Sender: TObject);
begin
  if Assigned(pcPages.ActivePage) and (pcPages.ActivePage.ControlCount > 0) and (pcPages.ActivePage.Controls[0] is TPrometMainFrame)
  and (pcPages.PageCount > 2) then
    TPrometMainFrame(pcPages.ActivePage.Controls[0]).CloseFrame;
end;

procedure TfMain.acCollectInventoryExecute(Sender: TObject);
begin

end;

procedure TfMain.acCombineSaleItemsExecute(Sender: TObject);
var
  aAccounting: TAccountingJournal;
  aOrderNo: String;
  aFilter: String;
begin
  aAccounting := TAccountingJournal.Create(Self,Data);
  with TToolbar(TAction(Sender).ActionComponent.Owner).Owner as TfFilter do
    begin
      DataSet.DataSet.First;
      while not DataSet.DataSet.EOF do
        begin
          aOrderNo := DataSet.FieldByName('ORDERNO').AsString;
          with aAccounting.DataSet as IBaseDBFilter do
            aFilter := Data.ProcessTerm('ORDERNO='+Data.QuoteValue(copy(aOrderNo,0,length(aOrderNo)-2)+'??'));
          Data.SetFilter(aAccounting,aFilter);
//          aAccounting.DataSet.Last;
          aAccounting.DataSet.Next;
//          if (aAccounting.Count = 2) and (aAccounting.FieldByName('STATUS').AsString='PR') then
          if (aAccounting.Count > 1) and (aAccounting.FieldByName('ORDERNO').AsString <> DataSet.FieldByName('ORDERNO').AsString) and (aAccounting.FieldByName('STATUS').AsString='PR') then //and (aAccounting.FieldByName('STATUS').AsString='SS') then
            begin
              with aAccounting.DataSet do
                begin
                  Showmessage(FieldByName('STATUS').AsString+FieldByName('ORDERNO').AsString);
                  Edit;
                  FieldByName('ORDERNO').AsString  := DataSet.FieldByName('ORDERNO').AsString;
                  FieldByName('CUSTNO').AsString   := DataSet.FieldByName('CUSTNO').AsString;
                  FieldByName('CUSTNAME').AsString := DataSet.FieldByName('CUSTNAME').AsString;
                  FieldByName('STATUS').AsString := DataSet.FieldByName('STATUS').AsString;
                  FieldByName('DATE').AsDateTime   := DataSet.FieldByName('DATE').AsDateTime;
                  FieldByName('NUMBER').AsString   := DataSet.FieldByName('NUMBER').AsString;
                  FieldByName('ODATE').AsDateTime   := DataSet.FieldByName('ODATE').AsDateTime;
                  if FieldDefs.IndexOf('VATH') > -1 then
                    begin
                      FieldByName('VATH').AsString     := DataSet.FieldByName('VATH').AsString;
                      FieldByName('VATF').AsString     := DataSet.FieldByName('VATF').AsString;
                    end;
                  FieldByName('NETPRICE').AsFloat   := DataSet.FieldByName('NETPRICE').AsFloat;
                  FieldByName('GROSSPRICE').AsFloat := DataSet.FieldByName('GROSSPRICE').AsFloat;
                  Post;
                end;
              DataSet.Delete;
            end
          else
            DataSet.DataSet.Next;
        end;
    end;
  aAccounting.Free;
end;
procedure TfMain.acDeleteWholeMessageDirExecute(Sender: TObject);
var
  ID: String;
  nData : TTreeEntry;
  aMessages: TMessageList;
  aMessage: TMessage;
  aFrame: TTabSheet;
  aConn: TComponent;
  a: Integer;
begin
  nData := TTreeEntry(fMainTreeFrame.tvMain.Selected.Data);
  if not Assigned(nData) then exit;
  if MessageDlg(strRealdelete,mtInformation,[mbYes,mbNo],0) = mrYes then
    begin
      Data.SetFilter(Data.Tree,'',0,'','ASC',False,True,True);
      if not Data.GotoBookmark(Data.Tree,nData.Rec) then exit;
      aConn := Data.GetNewConnection;
      Data.StartTransaction(aConn);
      ID := Data.Tree.Id.AsString;
      aMessages := TMessageList.Create(Self,Data);
      Data.SetFilter(aMessages,Data.QuoteField('TREEENTRY')+'='+ID);
      Data.DeletedItems.DataSet.Open;
      a := aMessages.Count;
      aTime := GetTickCount;
      fWaitForm.ShowInfo(Format(strDeletingMessages,[a]));
      fWaitForm.Show;
      aTime := GetTickCount;
      while not aMessages.DataSet.EOF do
        begin
{
          if Data.IsSQLDB then
            aMessages.DataSet.Delete
          else
}
            begin
              if GetTickCount-aTime > 1000 then
                begin
                  fWaitForm.ShowInfo(Format(strDeletingMessages,[a]));
                  aTime := GetTickCount;
                end;
              aMessage := TMessage.Create(Self,Data,aConn);
              aMessage.Select(aMessages.Id.AsInteger);
              aMessage.Open;
              aMessage.Delete;
              aMessage.Free;
              a := a-1;
              aMessages.DataSet.Next;
              if not fWaitForm.Visible then break;
            end;
        end;
{      if Data.IsSQLDB then
        begin
          Data.GetNewDataSet('delete from '+Data.QuoteField('MESSAGES')+' where '+Data.QuoteField('ID')+' not in (select '+Data.QuoteField('ID')+' from '+Data.QuoteField('MESSAGEIDX')+')');
        end;
}     fWaitForm.Hide;
      aMessages.Free;
      Data.CommitTransaction(aConn);
      aConn.Free;
      RefreshMessages;
    end;
end;
procedure TfMain.acForwardExecute(Sender: TObject);
begin
  FHistory.GoFwd;
end;
procedure TfMain.acHelpIndexExecute(Sender: TObject);
var
  aCreated: Boolean = False;
begin
  if not Assigned(fHelpContainer) then
    begin
      fHelpContainer.SetLanguage;
      aCreated := True;
    end;
  fHelpContainer.Show;
  if aCreated then
    fHelpContainer.WikiFrame.OpenWikiPage('Promet-ERP-Help/index',True);
end;
procedure TfMain.acInfoExecute(Sender: TObject);
begin
  fInfo.SetLanguage;
  with Application as IBaseApplication do
    begin
     fInfo.Version:=AppVersion;
     fInfo.Revision:=AppRevision;
     fInfo.ProgramName:=Appname;
     fInfo.InfoText:=vInfo;
     fInfo.Copyright:='2006-2012 C. Ulrich';
    end;
  fInfo.SetLanguage;
  fInfo.Execute;
end;
procedure TfMain.acLogoutExecute(Sender: TObject);
begin
  FreeAndNil(fOptions);
  if Assigned(fHelpContainer) then
    FreeAndNil(fHelpContainer);
  if Assigned(tsHelp) and (tsHelp.ControlCount > 0) then
    tsHelp.Controls[0].Destroy;
  pcPages.CloseAll;
  with Application as IBaseApplication do
    Logout;
end;
procedure TfMain.acMasterdataExecute(Sender: TObject);
var
  i: Integer;
  Found: Boolean = false;
  aFrame: TfFilter;
begin
  Application.ProcessMessages;
  for i := 0 to pcPages.PageCount-2 do
    if (pcPages.Pages[i].ControlCount > 0) and (pcPages.Pages[i].Controls[0] is TfFilter) and (TfFilter(pcPages.Pages[i].Controls[0]).Dataset is TMasterdataList) then
      begin
        pcPages.PageIndex:=i;
        Found := True;
      end;
  if not Found then
    begin
      aFrame := TfFilter.Create(Self);
      pcPages.AddTab(aFrame,True,'',Data.GetLinkIcon('MASTERDATA@'),False);
      AddMasterdataList(aFrame);
      aFrame.Open;
    end;
end;
procedure TfMain.acMeetingsExecute(Sender: TObject);
var
  i: Integer;
  Found: Boolean = False;
  aFrame: TfFilter;
begin
  Application.ProcessMessages;
  for i := 0 to pcPages.PageCount-2 do
    if (pcPages.Pages[i].ControlCount > 0) and (pcPages.Pages[i].Controls[0] is TfFilter) and (TfFilter(pcPages.Pages[i].Controls[0]).Dataset is TMeetings) then
      begin
        pcPages.PageIndex:=i;
        Found := True;
      end;
  if not Found then
    begin
      aFrame := TfFilter.Create(Self);
      pcPages.AddTab(aFrame,True,'',Data.GetLinkIcon('MEETINGS@'),False);
      AddMeetingList(aFrame);
      aFrame.Open;
    end;
end;
procedure TfMain.acMessagesExecute(Sender: TObject);
var
  i: Integer;
  Found: Boolean = false;
  aFrame: TfMessageFrame;
begin
  Screen.Cursor:=crHourglass;
  for i := 0 to pcPages.PageCount-2 do
    if (pcPages.Pages[i].ControlCount > 0) and (pcPages.Pages[i].Controls[0] is TfMessageFrame) then
      begin
        pcPages.PageIndex:=i;
        Found := True;
      end;
  if not Found then
    begin
      aFrame := TfmessageFrame.Create(Self);
      pcPages.AddTab(aFrame,True,'',Data.GetLinkIcon('MESSAGEIDX@'));
      aFrame.FMessageNode := FMessageNode;
      TfMessageFrame(aFrame).OpenDir(TREE_ID_MESSAGES);
      TfMessageFrame(aFrame).Caption:=strMessages;
    end;
  Screen.Cursor:=crDefault;
end;

procedure TfMain.acNewAccountExecute(Sender: TObject);
begin
  fWizardNewAccount.SetLanguage;
  fWizardNewAccount.InitWizard;
  fWizardNewAccount.ShowModal;
end;

procedure TfMain.acNewContactExecute(Sender: TObject);
var
  aFrame: TfPersonFrame;
begin
  Application.ProcessMessages;
  aFrame := TfPersonFrame.Create(Self);
  pcPages.AddTab(aFrame);
  aFrame.SetLanguage;
  aFrame.New;
end;
procedure TfMain.acNewInventoryExecute(Sender: TObject);
var
  aDS : TInventoryPos;
begin
  if Assigned(pcPages.ActivePage) and (pcPages.ActivePage.ControlCount > 0) and (pcPages.ActivePage.Controls[0] is TfFilter) then
    begin
      aDS := TInventoryPos(TfFilter(pcPages.ActivePage.Controls[0]).DataSet);
    end
  else exit;
  aDS.Inventory.Append;
  with aDS.Inventory.DataSet do
    begin
      FieldByName('DESC').AsString:=Dialogs.InputBox(strNewInventory,strName,strInventory);
      FieldByName('STATUS').AsString:='';
    end;
end;
procedure TfMain.acNewListExecute(Sender: TObject);
var
  aName: String;
  aFrame: TfFilter;
  aList: TLists;
begin
  aName := InputBox(strName,strNewList,strNewList);
  aList := TLists.Create(Self,Data);
  aList.Append;
  aList.FieldByName('NAME').AsString:=aName;
  aList.DataSet.Post;

end;
procedure TfMain.acNewMasterdataExecute(Sender: TObject);
var
  aFrame: TfArticleFrame;
begin
  Application.ProcessMessages;
  aFrame := TfArticleFrame.Create(Self);
  pcPages.AddTab(aFrame);
  aFrame.SetLanguage;
  aFrame.New;
end;

procedure TfMain.acNewMeetingExecute(Sender: TObject);
var
  aFrame: TfMeetingFrame;
begin
  Application.ProcessMessages;
  aFrame := TfMeetingFrame.Create(Self);
  pcPages.AddTab(aFrame);
  aFrame.SetLanguage;
  aFrame.New;
end;

procedure TfMain.acNewMessageExecute(Sender: TObject);
var
  fMessageEdit: TfMessageEdit;
begin
  fMessageEdit := TfMessageEdit.Create(Self);
  fMessageEdit.SendMailTo('');
end;
procedure TfMain.acNewOrderExecute(Sender: TObject);
var
  aFrame: TfOrderFrame;
begin
  Application.ProcessMessages;
  aFrame := TfOrderFrame.Create(Self);
  pcPages.AddTab(aFrame);
  aFrame.SetLanguage;
  aFrame.New;
end;
procedure TfMain.acNewProjectExecute(Sender: TObject);
var
  aFrame: TfProjectFrame;
begin
  Application.ProcessMessages;
  aFrame := TfProjectFrame.Create(Self);
  pcPages.AddTab(aFrame);
  aFrame.SetLanguage;
  aFrame.OnStartTime:=@SenderTfMainTaskFrameControlsSenderTfMainTaskFrameTfTaskFrameStartTime;
  aFrame.New;
end;

procedure TfMain.acNewStatisticsExecute(Sender: TObject);
var
  aFrame: TfStatisticFrame;
begin
  Application.ProcessMessages;
  aFrame := TfStatisticFrame.Create(Self);
  pcPages.AddTab(aFrame);
  aFrame.SetLanguage;
  aFrame.New;
end;

procedure TfMain.acNewTerminExecute(Sender: TObject);
var
  i: Integer;
  Found: Boolean = false;
  aFrame: TfCalendarFrame;
begin
  Application.ProcessMessages;
  for i := 0 to pcPages.PageCount-2 do
    if (pcPages.Pages[i].ControlCount > 0) and (pcPages.Pages[i].Controls[0] is TfCalendarFrame) then
      begin
        pcPages.PageIndex:=i;
        Found := True;
        aFrame := TfCalendarFrame(pcPages.Pages[i].Controls[0]);
      end;
  if not Found then
    begin
      aFrame := TfCalendarFrame.Create(Self);
      pcPages.AddTab(aFrame,True,'',Data.GetLinkIcon('CALENDAR@'));
      AddCalendar(aFrame);
    end;
  aFrame.New;
end;
procedure TfMain.acOpenExecute(Sender: TObject);
begin

end;
procedure TfMain.acOrdersExecute(Sender: TObject);
var
  i: Integer;
  Found: Boolean = false;
  aFrame: TfFilter;
begin
  Application.ProcessMessages;
  for i := 0 to pcPages.PageCount-2 do
    if (pcPages.Pages[i].ControlCount > 0) and (pcPages.Pages[i].Controls[0] is TfFilter) and (TfFilter(pcPages.Pages[i].Controls[0]).Dataset is TOrderList) then
      begin
        pcPages.PageIndex:=i;
        Found := True;
      end;
  if not Found then
    begin
      aFrame := TfFilter.Create(Self);
      pcPages.AddTab(aFrame,True,'',Data.GetLinkIcon('ORDERS@'),False);
      AddOrderList(aFrame);
      aFrame.Open;
    end;
end;

procedure TfMain.acPauseTimeExecute(Sender: TObject);
begin
  if Assigned(FTimeReg) then
    begin
      if not bPauseTime.Down then
        FTimeReg.acStart.Execute
      else
        FTimeReg.acPause.Execute;
    end;
end;

procedure TfMain.acProjectsExecute(Sender: TObject);
var
  i: Integer;
  Found: Boolean = False;
  aFrame: TfFilter;
begin
  Application.ProcessMessages;
  for i := 0 to pcPages.PageCount-2 do
    if (pcPages.Pages[i].ControlCount > 0) and (pcPages.Pages[i].Controls[0] is TfFilter) and (TfFilter(pcPages.Pages[i].Controls[0]).Dataset is TProjectList) then
      begin
        pcPages.PageIndex:=i;
        Found := True;
      end;
  if not Found then
    begin
      aFrame := TfFilter.Create(Self);
      pcPages.AddTab(aFrame,True,'',Data.GetLinkIcon('PROJECTS@'),False);
      AddprojectList(aFrame);
      aFrame.Open;
    end;
end;

procedure TfMain.acRefreshOrderListExecute(Sender: TObject);
var
  aOrders: TOrderList;
  aOrder: TOrder;
begin
  aOrders := TOrderList.Create(nil,Data);
  Data.SetFilter(aOrders,Data.ProcessTerm(Data.QuoteField('ACTIVE')+'='+Data.QuoteValue('')),0);
  while not aOrders.EOF do
    begin
      aOrder:=TOrder.Create(nil,Data);
      aOrder.Select(aOrders.FieldByName('ORDERNO').AsString);
      aOrder.Open;
      aOrder.Free;
      aOrders.DataSet.Refresh;
    end;
  aOrders.Free;
end;

procedure TfMain.acRenameDirectoryExecute(Sender: TObject);
begin

end;
procedure TfMain.acSalesListExecute(Sender: TObject);
var
  i: Integer;
  Found: Boolean = False;
  aFrame: TfFilter;
begin
  Application.ProcessMessages;
  for i := 0 to pcPages.PageCount-2 do
    if (pcPages.Pages[i].ControlCount > 0) and (pcPages.Pages[i].Controls[0] is TfFilter) and (TfFilter(pcPages.Pages[i].Controls[0]).Dataset is TAccountingJournal) then
      begin
        pcPages.PageIndex:=i;
        Found := True;
      end;
  if not Found then
    begin
      aFrame := TfFilter.Create(Self);
      pcPages.AddTab(aFrame,True,strSalesList,-1,False);
      AddSalesList(aFrame);
      aFrame.Open;
    end;
end;
procedure TfMain.acSalesListPayExecute(Sender: TObject);
begin
  if Assigned(pcPages.ActivePage) and (pcPages.ActivePage.ControlCount > 0) and (pcPages.ActivePage.Controls[0] is TfFilter) and (TfFilter(pcPages.ActivePage.Controls[0]).DataSet is TAccountingJournal) then
    begin
      fBookAccounting.SetLanguage;
      fBookAccounting.Accountingjournal := TfFilter(pcPages.ActivePage.Controls[0]).DataSet as TAccountingjournal;
      fBookAccounting.Execute(0,Now());
    end;
end;
procedure TfMain.acShowTreeExecute(Sender: TObject);
begin
  tvMain.Visible:=acShowTree.Checked;
  spTree.Visible:=acShowTree.Checked;
end;

procedure TfMain.acStandartTimeExecute(Sender: TObject);
begin
  if Assigned(FTimeReg) then
    begin
      FTimeReg.acStartstandartEntry.Execute;
    end;
end;

procedure TfMain.acTaskPlanExecute(Sender: TObject);
var
  aFrame: TfTaskPlan;
begin
  aFrame := TfTaskPlan.Create(Self);
  aFrame.TabCaption := strTaskPlan;
  pcPages.AddTab(aFrame,True,'',Data.GetLinkIcon('TASKS@'),False);
  if Data.Users.FieldByName('POSITION').AsString = 'LEADER' then
    aFrame.Populate(Data.Users.FieldByName('PARENT').AsVariant,Null)
  else
    aFrame.Populate(Data.Users.FieldByName('PARENT').AsVariant,Data.Users.Id.AsVariant);
end;

procedure TfMain.acTasksExecute(Sender: TObject);
var
  i: Integer;
  Found: Boolean = False;
  aFrame: TfMainTaskFrame;
begin
  Application.ProcessMessages;
  for i := 0 to pcPages.PageCount-2 do
    if (pcPages.Pages[i].ControlCount > 0)
    and (pcPages.Pages[i].Controls[0] is TfMainTaskFrame)
    and (TfMainTaskFrame(pcPages.Pages[i].Controls[0]).Tasks.UserID = Data.Users.Id.AsVariant)
    then
      begin
        pcPages.PageIndex:=i;
        Found := True;
      end;
  if not Found then
    begin
      aFrame := TfMainTaskFrame.Create(Self);
      aFrame.TabCaption := strTasks;
      pcPages.AddTab(aFrame,True,'',Data.GetLinkIcon('TASKS@'),False);
      aFrame.Tasks.UserID:=Data.Users.Id.AsVariant;
      AddTaskList(aFrame);
    end;
end;

procedure TfMain.acTimeRegisteringExecute(Sender: TObject);
var
  i: Integer;
  Found: Boolean = false;
  aFrame: TfTimeFrame;
begin
  Application.ProcessMessages;
  for i := 0 to pcPages.PageCount-2 do
    if (pcPages.Pages[i].ControlCount > 0) and (pcPages.Pages[i].Controls[0] is TfTimeFrame) then
      begin
        pcPages.PageIndex:=i;
        Found := True;
      end;
  if (not Found) and Assigned(FTimeReg) then
    begin
      aFrame := TfTimeFrame.Create(Self);
      FTimeReg.BorderStyle:=bsNone;
      fTimeReg.Parent := aFrame;
      aFrame.FTimeReg := FTimeReg;
      fTimeReg.SetRights;
      FTimeReg.Visible:=True;
      fTimereg.Align:=alClient;
      FTimereg.Timer.Enabled:=True;
      FTimeReg.Calculate;
      aFrame.DoOpen;
      pcPages.AddTab(aFrame,True,strTimetools,-1,False);
    end;
end;

procedure TfMain.acWikiExecute(Sender: TObject);
var
  i: Integer;
  Found: Boolean = false;
  aFrame: TfWikiFrame;
begin
  Application.ProcessMessages;
  for i := 0 to pcPages.PageCount-2 do
    if (pcPages.Pages[i].ControlCount > 0) and (pcPages.Pages[i].Controls[0] is TfWikiFrame) and (pcPages.Pages[i] <> tsHelp) then
      begin
        pcPages.PageIndex:=i;
        Found := True;
      end;
  if not Found then
    begin
      aFrame := TfWikiFrame.Create(Self);
      pcPages.AddTab(aFrame,True,'',Data.GetLinkIcon('WIKI@'),False);
      AddWiki(aFrame);
      aFrame.SetRights(Data.Users.Rights.Right('WIKI')>RIGHT_READ);
    end;
end;
procedure TfMain.aFrameTfFilterClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  TListEntrys(TfFilter(Sender).DataSet).List.Free;
end;
procedure TfMain.aFrameTfFilterDragDrop(Sender, Source: TObject; X, Y: Integer);
var
  aRec: String;
  OldFilter: String;
  aLink: String;
  aLinkDesc: String;
  aIcon: Integer;
  nData: TTreeEntry;
  aDS: TBaseDbDataSet;
begin
  if Assigned(fSearch) and (Source = fSearch.sgResults) then
    begin
      aLink := fSearch.GetLink;
      aLinkDesc := Data.GetLinkDesc(aLink);
      aIcon := Data.GetLinkIcon(aLink);
      with TExtDBGrid(Sender).DataSource.DataSet do
        begin
          Insert;
          FieldByName('LINK').AsString := aLink;
          FieldByName('NAME').AsString := aLinkDesc;
          FieldByName('ICON').AsInteger := aIcon;
          Post;
        end;
    end
  else
    begin
      if Source = uMainTreeFrame.fMainTreeFrame.tvMain then
        begin
          nData := TTreeEntry(uMainTreeFrame.fMainTreeFrame.tvMain.Selected.Data);
          aDS := nData.DataSourceType.Create(Self,Data);
          Data.SetFilter(aDS,nData.Filter);
          Data.GotoBookmark(aDS,nData.Rec);
          aLink := Data.BuildLink(aDS.DataSet);
          aLinkDesc := Data.GetLinkDesc(aLink);
          aIcon := Data.GetLinkIcon(aLink);
          aDS.Free;
          with TExtDBGrid(Sender).DataSource.DataSet do
            begin
              Insert;
              FieldByName('LINK').AsString := aLink;
              FieldByName('NAME').AsString := aLinkDesc;
              FieldByName('ICON').AsInteger := aIcon;
              Post;
            end;
        end
    end;
end;
procedure TfMain.aFrameTfFilterDragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
begin
  Accept := False;
  if Assigned(fSearch) and (Source = fSearch.sgResults) then
    begin
      with fSearch.sgResults do
        Accept := trim(fSearch.GetLink) <> '';
    end;
  if Assigned(uMainTreeFrame.fMainTreeFrame)
  and (Source = uMainTreeFrame.fMainTreeFrame.tvMain)
  and ((TTreeEntry(uMainTreeFrame.fMainTreeFrame.tvMain.Selected.Data).Typ = etCustomer)
  or (TTreeEntry(uMainTreeFrame.fMainTreeFrame.tvMain.Selected.Data).Typ = etSupplier)
  or (TTreeEntry(uMainTreeFrame.fMainTreeFrame.tvMain.Selected.Data).Typ = etMasterdata)
  )
  then
    Accept := True;
end;
procedure TfMain.aInvPosCloseClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  TInventoryPos(TfFilter(Sender).DataSet).Inventory.Free;
end;
procedure TfMain.aItemClick(Sender: TObject);
begin
  FHistory.HistoryIndex:=TMenuItem(Sender).Tag;
end;
procedure TfMain.ApplicationProperties1ShowHint(var HintStr: string;
  var CanShow: Boolean; var HintInfo: THintInfo);
begin
  if fSearch.ShowHint(HintStr,CanShow,HintInfo) then exit;
  if Assigned(pcPages.ActivePage) and (pcPages.ActivePage.ControlCount > 0) and (pcPages.ActivePage.Controls[0] is TPrometMainFrame) then
    if TPrometMainFrame(pcPages.ActivePage.Controls[0]).ShowHint(HintStr,CanShow,HintInfo) then exit;
end;
procedure TfMain.ApplicationTBaseVisualApplicationUserTabAdded(Sender: TObject);
var
  aFrame : TEditableFrame;
begin
  if Data.Users.Rights.Right('OPTIONS') > RIGHT_READ then
    aFrame.SetupTabEditor(TTabSheet(Sender));
end;
procedure TfMain.bSearchClick(Sender: TObject);
var
  SearchTypes : TFullTextSearchTypes = [];
  SearchLocations : TSearchLocations;
  i: Integer;
begin
  if bSearch.Caption = strAbort then
    begin
      SearchText := '';
      if Assigned(ActiveSearch) then
        ActiveSearch.Abort;
      bSearch.Caption := strSearch;
      {$IFDEF MAINAPP}
      if (fsSerial in SearchTypes) then
        fOrders.acViewList.Execute;
      {$ENDIF}
      exit;
    end;
  SearchTypes := SearchTypes+[fsShortnames];
  SearchTypes := SearchTypes+[fsIdents];
  SearchTypes := SearchTypes+[fsSerial];
  SearchTypes := SearchTypes+[fsBarcode];
  SearchTypes := SearchTypes+[fsCommission];
  SearchTypes := SearchTypes+[fsDescription];
  fSearch.SetLanguage;
  fSearch.LoadOptions('MAIN');
  for i := 0 to fSearch.cbSearchtype.Count-1 do
    if fSearch.cbSearchtype.Checked[i] then
      begin
        SetLength(SearchLocations,length(SearchLocations)+1);
        SearchLocations[length(SearchLocations)-1] := fSearch.cbSearchType.Items[i];
      end;

  lbResults.Clear;
  SearchLinks.Clear;
  bSearch.Caption := strAbort;
  SearchText := eContains.Text;
  ActiveSearch := TSearch.Create(SearchTypes,SearchLocations,True,5);
  ActiveSearch.OnItemFound:=@DataSearchresultItem;
  ActiveSearch.Start(eContains.Text);
  while ActiveSearch.Active do Application.ProcessMessages;
  pSearch.Height := Max(2,Min(lbResults.Count,7))*25;
  pSearch.Left := Panel8.Left+33;
  bSearch.Caption:=strSearch;
end;
procedure TfMain.DataSearchresultItem(aIdent: string; aName: string;
  aStatus: string;aActive : Boolean; aLink: string; aItem: TBaseDBList=nil);
begin
  if aActive then
    begin
      lbResults.AddItem(Data.GetLinkDesc(aLink),nil);
      SearchLinks.Add(aLink);
      pSearch.Visible:=True;
    end;
end;
procedure TfMain.eContainsEnter(Sender: TObject);
begin
  TEdit(Sender).Font.Color:=clWindowText;
  TEdit(Sender).Clear;
end;
procedure TfMain.eContainsExit(Sender: TObject);
begin
  TEdit(Sender).Text:=strSearchText;
  TEdit(Sender).Font.Color:=clGrayText;
  if fMain.ActiveControl <> lbResults then
    begin
      pSearch.Visible:=False;
    end;
end;
procedure TfMain.eContainsKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
  VK_PRIOR,
  VK_UP:
    begin
      if lbResults.ItemIndex = -1 then
        lbResults.ItemIndex:=0;
      lbResults.ItemIndex:=lbResults.ItemIndex-1;
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
      lbResultsDblClick(nil);
      Key := 0;
    end;
  end;
end;
procedure TfMain.eContainsKeyPress(Sender: TObject; var Key: char);
begin
  if Key = #27 then
    pSearch.Visible:=False
  else
    begin
      SearchTimer.Enabled:=True;
    end;
end;
procedure TfMain.fMainTreeFrameDragDrop(Sender, Source: TObject; X, Y: Integer);
var
  aLink: String;
  aFilter: TfFilter;
  aTNode: TTreeNode;
  aMessage: TMessage;
  aTreeEntry: TTreeEntry;
  aTask: TTask;
  ls: TListItem;
  aPages: TDocPages;
begin
  if not Assigned(Source) then exit;
  if (Source is TListView) and (TListView(Source).Owner is TfDocumentFrame) then
    begin
      aTNode := fMainTreeFrame.tvMain.GetNodeAt(X,Y);
      if Assigned(aTNode) then
        begin
          aTreeEntry := TTreeEntry(aTNode.Data);
          if aTreeEntry.Typ = etDocuments then
            begin
              aPages := TDocPages.Create(nil,Data);
              ls := TListView(Source).Selected;
              fWaitForm.ShowInfo(ls.Caption);
              fWaitform.ProgressBar1.Max:=TListView(Source).SelCount;
              fWaitform.ProgressBar1.Position:=0;
              fWaitform.ProgressBar1.Style:=pbstNormal;
              fWaitForm.Show;
              Application.ProcessMessages;
              while Assigned(ls) do
                begin
                  if TfDocumentFrame(TListView(Source).Owner).GotoEntry(ls) then
                    begin
                      aPages.Add(TDocuments(TfDocumentFrame(TListView(Source).Owner).DataSet));
                      fWaitForm.ShowInfo(ls.Caption);
                      fWaitform.ProgressBar1.Position:=fWaitform.ProgressBar1.Position+1;
                    end;
                  ls := TListView(Source).GetNextItem(ls, sdBelow, [lisSelected]);
                end;
              fWaitform.ProgressBar1.Style:=pbstMarquee;
              fWaitform.Hide;
              aPages.Free;
            end;
        end;
    end
  else if (Source is TExtDBGrid) and (TComponent(Source).Owner is TfFilter) then
    begin
      aLink := TfFilter(TComponent(Source).Owner).GetLink;
      aFilter := TfFilter(TComponent(Source).Owner);
      if aFilter.DataSet is TMessageList then
        begin
          aTNode := fMainTreeFrame.tvMain.GetNodeAt(X,Y);
          if Assigned(aTNode) then
            begin
              aTreeEntry := TTreeEntry(aTNode.Data);
              if (aTreeEntry.Typ = etMessageDir)
              or (aTreeEntry.Typ = etMessageBoard)
              then
                begin
                  Data.SetFilter(Data.Tree,'',0,'','ASC',False,True,True);
                  Data.Tree.GotoBookmark(aTreeEntry.Rec);
                  aMessage := TMessage.Create(Self,Data);
                  aMessage.SelectFromLink(aLink);
                  aMessage.Open;
                  if aMessage.Count > 0 then
                    begin
                      aMessage.DataSet.Edit;
                      aMessage.FieldByName('TREEENTRY').AsVariant:=Data.Tree.Id.AsVariant;
                      aMessage.DataSet.Post;
                      DoRefreshActiveTab(nil);
                    end;
                  aMessage.Free;
                end
              else if (aTreeEntry.Typ = etTasks) then
                begin
                  aMessage := TMessage.Create(Self,Data);
                  aMessage.SelectFromLink(aLink);
                  aMessage.Open;
                  if aMessage.Count > 0 then
                    begin
                      aMessage.DataSet.Edit;
                      aMessage.FieldByName('READ').AsString := 'Y';
                      aMessage.DataSet.Post;
                      aTask := TTask.Create(nil,Data);
                      aTask.Insert;
                      aTask.FieldByName('SUMMARY').AsString:=aMessage.Text.AsString;
                      aTask.FieldByName('USER').AsString:=Data.Users.FieldByName('ACCOUNTNO').AsString;
                      aMessage.Content.Open;
                      aTask.FieldByName('DESC').AsString:=aMessage.Content.AsString;
                      aTask.DataSet.Post;
                      aTask.Links.Add(Data.BuildLink(aMessage.DataSet));
                      aTask.Free;
                    end;
                  aMessage.Free;
                end;
            end;
        end;
    end
  else if (Source is TComponent) and (TComponent(Source).Owner is TfManageDocFrame) then
    begin
      aTNode := fMainTreeFrame.tvMain.GetNodeAt(X,Y);
      if Assigned(aTNode) then
        begin
          aTreeEntry := TTreeEntry(aTNode.Data);
          if (aTreeEntry.Typ = etDocumentDir)
          then
            begin
              if TfManageDocFrame(TComponent(Source).Owner).GotoCurrentItem then
                begin
                  if not TfManageDocFrame(TComponent(Source).Owner).DataSet.CanEdit then
                    TfManageDocFrame(TComponent(Source).Owner).DataSet.DataSet.Edit;
                  TfManageDocFrame(TComponent(Source).Owner).DataSet.FieldByName('TREEENTRY').AsVariant:=aTreeEntry.Rec;
                  TfManageDocFrame(TComponent(Source).Owner).DataSet.Post;
                  TfManageDocFrame(TComponent(Source).Owner).acRefresh.Execute;
                end;
            end;
        end;
    end;
end;
procedure TfMain.fMainTreeFrameDragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
var
  aLink: String;
  aFilter: TfFilter;
  aNode: TTreeNode;
  aTreeEntry: TTreeEntry;
  aTNode: TTreeNode;
begin
  //Check for FilterFrame
  Accept := False;
  if not Assigned(Source) then exit;
  if (Source is TListView) and (TListView(Source).Owner is TfDocumentFrame) then
    begin
      aTNode := fMainTreeFrame.tvMain.GetNodeAt(X,Y);
      if Assigned(aTNode) then
        begin
          aTreeEntry := TTreeEntry(aTNode.Data);
          if aTreeEntry.Typ = etDocuments then
            Accept := True;
        end;
    end
  else if (Source is TExtDBGrid) and (TComponent(Source).Owner is TfFilter) then
    begin
      aLink := TfFilter(TComponent(Source).Owner).GetLink;
      aFilter := TfFilter(TComponent(Source).Owner);
      if aFilter.DataSet is TMessageList then
        begin
          aTNode := fMainTreeFrame.tvMain.GetNodeAt(X,Y);
          if Assigned(aTNode) then
            begin
              aTreeEntry := TTreeEntry(aTNode.Data);
              if (aTreeEntry.Typ = etMessageDir)
              or (aTreeEntry.Typ = etMessageBoard)
              or (aTreeEntry.Typ = etTasks)
              then
                Accept := True;
            end;
        end;
    end
  else if (Source is TComponent) and (TComponent(Source).Owner is TfManageDocFrame) then
    begin
      aTNode := fMainTreeFrame.tvMain.GetNodeAt(X,Y);
      if Assigned(aTNode) then
        begin
          aTreeEntry := TTreeEntry(aTNode.Data);
          if (aTreeEntry.Typ = etDocumentDir)
          then
            Accept := True;
        end;
    end;
end;
function TfMain.fMainTreeFrameNewFromLink(aLink: string; aSender: TObject
  ): TBaseDBDataSet;
var
  aFrame: TPrometMainFrame;
begin
  Result := nil;
  if copy(aLink,0,8) = 'CUSTOMER' then
    begin
      aFrame := TfPersonFrame.Create(Self);
      pcPages.AddTab(aFrame);
      aFrame.SetLanguage;
      aFrame.New;
      Result := aFrame.DataSet;
    end;
end;
function TfMain.fMainTreeFrameOpen(aEntry: TTreeEntry): Boolean;
var
  aDataSet: TBaseDBDataset;
  aFrame: TfOrderFrame;
  aFrame1: TfMainTaskFrame;
  aFrame2: TfCalendarFrame;
  Found: Boolean = False;
  i: Integer;
begin
  if not Assigned(aEntry) then
    exit;
  Screen.Cursor:=crHourglass;
  Application.ProcessMessages;
  case aEntry.Typ of
  etSalesList:
    begin
      aEntry.Action.Execute;
    end;
  etCustomerList,etCustomers,etArticleList,etOrderList,
  etTasks,etMyTasks,etProjects,etCalendar,etMyCalendar,
  etMessages,etMessageDir,etMessageBoard:
    begin
      pcPages.SetFocus;
    end;
  etLink:
    begin
      fMainTreeFrame.OpenLink(aEntry.Link,Self);
    end;
  etCustomer,etEmployee,etArticle,etProject,etProcess,etStatistic:
    begin
      aDataSet := aEntry.DataSourceType.Create(Self,Data);
      with aDataSet.DataSet as IBaseDBFilter do
        Filter := aEntry.Filter;
      aDataSet.Open;
      if aDataSet.Count > 0 then
        fMainTreeFrame.OpenLink(Data.BuildLink(aDataSet.DataSet),Self);
      aDataSet.Free;
    end;
  etTaskUser:
    begin
      for i := 0 to pcPages.PageCount-2 do
        if (pcPages.Pages[i].ControlCount > 0)
        and (pcPages.Pages[i].Controls[0] is TfMainTaskFrame)
        and (TfMainTaskFrame(pcPages.Pages[i].Controls[0]).Tasks.UserID = aEntry.Rec)
        then
          begin
            pcPages.PageIndex:=i;
            Found := True;
          end;
      if not Found then
        begin
          aFrame1 := TfMainTaskFrame.Create(Self);
          pcPages.AddTab(aFrame1,True,strTasks +' '+ aEntry.Text[0],Data.GetLinkIcon('TASKS@'),False);
          TfMainTaskFrame(aFrame1).Tasks.UserID := aEntry.Rec;
          AddTaskList(aFrame1);
        end;
    end;
  etCalendarUser:
    begin
      for i := 0 to pcPages.PageCount-2 do
        if (pcPages.Pages[i].ControlCount > 0)
        and (pcPages.Pages[i].Controls[0] is TfCalendarFrame)
        and (TfCalendarFrame(pcPages.Pages[i].Controls[0]).aDirectory = IntToStr(aEntry.Rec))
        then
          begin
            pcPages.PageIndex:=i;
            Found := True;
          end;
      if not Found then
        begin
          aFrame2 := TfCalendarFrame.Create(Self);
          pcPages.AddTab(aFrame2,True,strCalendar +' '+ aEntry.Text[0],Data.GetLinkIcon('CALENDAR@'),False);
          TfCalendarFrame(aFrame2).OpenDir(aEntry.Rec);
        end;
    end;
  etNewAccount:
    begin
      fWizardNewAccount.InitWizard;
      fWizardNewAccount.ShowModal;
    end;
  etAccountingQue:
    begin
      fAccountingQue.SetLanguage;
      fAccountingQue.Show;
    end;
  etNewTransfer:
    begin
      fTransfer.SetLanguage;
      fTransfer.Show;
    end;
  etNewOrder:
    begin
      Application.ProcessMessages;
      aFrame := TfOrderFrame.Create(Self);
      pcPages.AddTab(aFrame);
      aFrame.SetLanguage;
      aDataSet := aEntry.DataSourceType.Create(Self,Data);
      aDataSet.Open;
      aDataSet.GotoBookmark(aEntry.Rec);
      aFrame.New(aDataSet.FieldByName('STATUS').AsString);
      aDataSet.Destroy;
    end;
  etTaskPlan:
    begin
      acTaskPlan.Execute;
    end;
  etAttPlan:
    begin
      acAttPlan.Execute;
    end;
  etWikiPage:
    begin
      aDataSet := aEntry.DataSourceType.Create(Self,Data);
      with aDataSet.DataSet as IBaseDBFilter do
        Filter := aEntry.Filter;
      aDataSet.Open;
      if aDataSet.Count > 0 then
        fMainTreeFrame.OpenLink(Data.BuildLink(aDataSet.DataSet),Self);
      aDataSet.Free;
    end;
  end;
end;
function TfMain.fMainTreeFrameOpenFromLink(aLink: string; aSender: TObject
  ): Boolean;
var
  aFrame: TPrometMainFrame;
  aMessageEdit: TfMessageEdit;
  tmp: String;
  tmp1: String;
  Found: Boolean;
  i: Integer;
  aList: TLists;
  aInv: TInventorys;
  aDoc: TDocuments;
  aDocs: TTabSheet;
  FTaskEdit: TfTaskEdit;
begin
  Result := False;
  Screen.Cursor:=crHourGlass;
  Application.ProcessMessages;
  if not Application.HasOption('o','doubleopen') then
    begin
      for i := 1 to pcPages.PageCount-1 do
        if (pcPages.Pages[i].ControlCount > 0) and (pcPages.Pages[i].Controls[0] is TPrometMainFrame) then
          if TPrometMainFrame(pcPages.Pages[i].Controls[0]).Link = aLink then
            begin
              pcPages.ActivePage := pcPages.Pages[i];
              Screen.Cursor:=crDefault;
              Result := True;
              exit;
            end;
    end;
  FHistory.Add(aLink);
  if copy(aLink,0,8) = 'CUSTOMER' then
    begin
      aFrame := TfPersonFrame.Create(Self);
      aFrame.SetLanguage;
      if aFrame.OpenFromLink(aLink) then
        begin
          pcPages.AddTab(aFrame);
          Result := True;
        end
      else aFrame.Free;
    end
  else if copy(aLink,0,11) = 'MESSAGEIDX@' then
    begin
      aMessageEdit := TfMessageEdit.Create(Self);
      aMessageEdit.OpenFromLink(aLink);
    end
  else if copy(aLink,0,11) = 'MASTERDATA@' then
    begin
      aFrame := TfArticleFrame.Create(Self);
      aFrame.SetLanguage;
      if aFrame.OpenFromLink(aLink) then
        begin
          pcPages.AddTab(aFrame);
          Result := True;
        end
      else aFrame.Free;
    end
  else if copy(aLink,0,7) = 'ORDERS@' then
    begin
      aFrame := TfOrderFrame.Create(Self);
      aFrame.SetLanguage;
      if aFrame.OpenFromLink(aLink) then
        begin
          pcPages.AddTab(aFrame);
          TfOrderFrame(aFrame).DoSelect;
          Result := True;
        end
      else aFrame.Free;
    end
  else if copy(aLink,0,5) = 'WIKI@' then
    begin
      if Assigned(pcPages.ActivePage) and (pcPages.ActivePage.ControlCount > 0) and (pcPages.ActivePage.Controls[0] is TfWikiFrame) then
        aFrame := TfWikiFrame(pcPages.ActivePage.Controls[0])
      else
        begin
          aFrame := TfWikiFrame.Create(Self);
          pcPages.AddTab(aFrame);
          aFrame.SetLanguage;
          TfWikiFrame(aFrame).SetRights(Data.Users.Rights.Right('WIKI')>RIGHT_READ);
        end;
      aFrame.OpenFromLink(aLink);
      Result := True;
    end
  else if (copy(aLink,0,11) = 'STATISTICS@') then
    begin
      aFrame := TfStatisticFrame.Create(Self);
      aFrame.OpenFromLink(aLink);
      pcPages.AddTab(aFrame);
      aFrame.SetLanguage;
      Result := True;
    end
  else if (copy(aLink,0,9) = 'PROJECTS@')
       or (copy(aLink,0,12) = 'PROJECTS.ID@') then
    begin
      aFrame := TfProjectFrame.Create(Self);
      aFrame.SetLanguage;
      TfProjectFrame(aFrame).OnStartTime:=@SenderTfMainTaskFrameControlsSenderTfMainTaskFrameTfTaskFrameStartTime;
      if aFrame.OpenFromLink(aLink) then
        begin
          pcPages.AddTab(aFrame);
          Result := True;
        end
      else aFrame.Free;
    end
  else if (copy(aLink,0,6) = 'TASKS@') then
    begin
      FTaskEdit := TfTaskEdit.Create(Self);
      FTaskEdit.Execute(aLink);
      FTaskEdit.Free;
    end
  else if (copy(aLink,0,16) = 'ACCOUNTEXCHANGE@') then
    begin
      tmp := aLink;
      tmp   := copy(tmp, pos('@', tmp) + 1, length(tmp));
      tmp1 := copy(tmp, 0, pos('&&', tmp) - 1);
      Found := False;
      if Assigned(pcPages.ActivePage) and (pcPages.ActivePage.ControlCount > 0)
      and (pcPages.ActivePage.Controls[0] is TfAccountingFrame) and (IntToStr(TfAccountingFrame(pcPages.ActivePage.Controls[0]).Account) = tmp1) then
        begin
          aFrame := TfAccountingFrame(pcPages.ActivePage.Controls[0]);
          Found := True;
        end
      else
        aFrame := TfAccountingFrame.Create(Self);
      Result := aFrame.OpenFromLink(aLink);
      if Result then
        begin
          if not Found then
            pcPages.AddTab(aFrame);
          aFrame.SetLanguage;
        end else aFrame.Free;
    end
  else if (copy(aLink,0,6) = 'LISTS@') then
    begin
      aList := TLists.Create(Self,Data);
      aList.SelectFromLink(aLink);
      aList.Open;
      aList.Entrys.Open;
      //TODO:Use Connection
      aFrame := TfFilter.Create(Self);
      pcPages.AddTab(aFrame,True,'',Data.GetLinkIcon('LISTS@'),False);
      with aFrame as TfFilter do
        begin
          TabCaption := aList.FieldByName('NAME').AsString;
          FilterType:='LE';
          DefaultRows:='GLOBALWIDTH:125;ACTIVE:25;NAME:100;';
          Dataset := aList.Entrys;
          DestroyDataSet:=False;
          TfFilter(aFrame).acOpen.OnExecute:=@TfFilteracOpenExecute;
          TfFilter(aFrame).OnClose:=@aFrameTfFilterClose;
          gList.OnDragOver:=@aFrameTfFilterDragOver;
          gList.OnDragDrop:=@aFrameTfFilterDragDrop;
          AddToolbarAction(acClearList);
          AddToolbarAction(acDeleteListeEntry);
        end;
      TfFilter(aFrame).Open;
    end
  else if (copy(aLink,0,9) = 'DOCUMENTS') then
    begin
      aDoc:=TDocuments.Create(Self,Data);
      aDoc.SelectByLink(aLink);
      aDoc.Open;
      if aDoc.Count > 0 then
        begin
          case aDoc.FieldByName('TYPE').AsString of
          'C':
            begin
              aFrame := TfPersonFrame.Create(Self);
              aFrame.SetLanguage;
              if aFrame.OpenFromLink('CUSTOMERS.ID@'+aDoc.FieldByName('REF_ID_ID').AsString) then
                begin
                  pcPages.AddTab(aFrame);
                  Application.ProcessMessages;
                  aDocs := TfPersonFrame(aFrame).pcPages.GetTab(TfDocumentFrame);
                  if Assigned(aDocs) then
                    begin
                      TfPersonFrame(aFrame).pcPages.ActivePage := aDocs;
                      Application.ProcessMessages;
                      TfDocumentFrame(aDocs.Controls[0]).SelectDocument(aLink);
                    end;
                  Result := True;
                end
              else aFrame.Free;
            end;
          'M':
            begin
              aFrame := TfArticleFrame.Create(Self);
              aFrame.SetLanguage;
              if aFrame.OpenFromLink('MASTERDATA.ID@'+aDoc.FieldByName('REF_ID_ID').AsString) then
                begin
                  pcPages.AddTab(aFrame);
                  Application.ProcessMessages;
                  aDocs := TfArticleFrame(aFrame).pcPages.GetTab(TfDocumentFrame);
                  if Assigned(aDocs) then
                    begin
                      TfArticleFrame(aFrame).pcPages.ActivePage := aDocs;
                      Application.ProcessMessages;
                      TfDocumentFrame(aDocs.Controls[0]).SelectDocument(aLink);
                    end;
                  Result := True;
                end
              else aFrame.Free;
            end;
          'P':
            begin
              aFrame := TfProjectFrame.Create(Self);
              aFrame.SetLanguage;
              TfProjectFrame(aFrame).OnStartTime:=@SenderTfMainTaskFrameControlsSenderTfMainTaskFrameTfTaskFrameStartTime;
              if aFrame.OpenFromLink('PROJECTS.ID@'+aDoc.FieldByName('REF_ID_ID').AsString) then
                begin
                  pcPages.AddTab(aFrame);
                  Application.ProcessMessages;
                  aDocs := TfProjectFrame(aFrame).pcPages.GetTab(TfDocumentFrame);
                  if Assigned(aDocs) then
                    begin
                      TfProjectFrame(aFrame).pcPages.ActivePage := aDocs;
                      Application.ProcessMessages;
                      TfDocumentFrame(aDocs.Controls[0]).SelectDocument(aLink);
                    end;
                  Result := True;
                end
              else aFrame.Free;
            end;
          end;
        end;
      aDoc.Free;
    end
  else if (copy(aLink,0,10) = 'INVENTORY@') then
    begin
      aInv := TInventorys.Create(Self,Data);
      aInv.SelectFromLink(aLink);
      aInv.Open;
      aInv.Positions.Open;
      //TODO:Use Connection
      aFrame := TfFilter.Create(Self);
      pcPages.AddTab(aFrame,True,'',Data.GetLinkIcon('INVENTORY@'),False);
      with aFrame as TfFilter do
        begin
          TabCaption := aInv.FieldByName('DESC').AsString;
          FilterType:='INVPOS';
          DefaultRows:='GLOBALWIDTH:355;PONO:25;IDENT:70;SHORTTEXT:120;STORAGE:30;QUANTITY:30;QUANTITYC:30;QUANTITYU:50;';
          Dataset := aInv.Positions;
          DestroyDataSet:=False;
          TfFilter(aFrame).OnClose:=@aInvPosCloseClose;
//          gList.OnDragOver:=@aFrameTfFilterDragOver;
//          gList.OnDragDrop:=@aFrameTfFilterDragDrop;
          AddToolbarAction(acBookInventory);
          AddToolbarAction(acCollectInventory);
        end;
      TfFilter(aFrame).Open;
    end
  else if (copy(aLink,0,9) = 'MEETINGS@') then
    begin
      aFrame := TfMeetingFrame.Create(Self);
      aFrame.OpenFromLink(aLink);
      pcPages.AddTab(aFrame);
      aFrame.SetLanguage;
      Result := True;
    end
  else Data.GotoLink(aLink)
  ;
  Screen.Cursor:=crDefault;
end;
procedure TfMain.fMainTreeFrameSelectionChanged(aEntry: TTreeEntry);
var
  Found: Boolean = False;
  i: Integer;
  aFrame: TPrometMainFrame;
  aIFrame: TPrometInplaceDBFrame;
  New: TMenuItem;
begin
  case aEntry.Typ of
  etCustomerList,etCustomers:
    begin
      acContact.Execute;
    end;
  etMasterdata,etArticleList:
    begin
      acMasterdata.Execute;
    end;
  etOrders,etOrderList:
    begin
      acOrders.Execute;
    end;
  etTasks,etMyTasks:
    begin
      acTasks.Execute;
    end;
  etProjects:
    begin
      acProjects.Execute;
    end;
  etCalendar,etMyCalendar:
    begin
      acCalendar.Execute;
    end;
  etMessages:
    begin
      acMessages.Execute;
    end;
  etWiki:
    begin
      acWiki.Execute;
    end;
  etMessageDir,etMessageBoard:
    begin
      Application.ProcessMessages;
      if Assigned(pcPages.ActivePage) and (pcPages.ActivePage.ControlCount > 0) and (pcPages.ActivePage.Controls[0] is TfMessageFrame) then
        Found := True;
      if not Found then
        for i := 0 to pcPages.PageCount-2 do
          if (pcPages.Pages[i].ControlCount > 0) and (pcPages.Pages[i].Controls[0] is TfMessageFrame) then
            begin
              pcPages.PageIndex:=i;
              Found := True;
            end;
      if not Found then
        begin
          aFrame := TfmessageFrame.Create(Self);
          pcPages.AddTab(aFrame);
          TfMessageFrame(aFrame).FMessageNode := FMessageNode;
        end;
      if not Data.GotoBookmark(Data.Tree,aEntry.Rec) then
        begin
          Data.SetFilter(Data.Tree,'',0,'','ASC',False,True,True);
          if not Data.GotoBookmark(Data.Tree,aEntry.Rec) then
            exit;
        end;
      if Data.Tree.Id.AsInteger = TREE_ID_DELETED_MESSAGES then
        begin
          New := TMenuItem.Create(nil);
          New.Action := acDeleteWholeMessageDir;
          fMainTreeFrame.pmTree.Items.Add(New);
        end;
      if (pcPages.ActivePage.ControlCount > 0) and (pcPages.ActivePage.Controls[0] is TfMessageFrame) then
        TfMessageFrame(pcPages.ActivePage.Controls[0]).OpenDir(Data.Tree.Id.AsVariant);
    end;
  etDocumentDir:
    begin
      Application.ProcessMessages;
      if Assigned(pcPages.ActivePage) and (pcPages.ActivePage.ControlCount > 0) and (pcPages.ActivePage.Controls[0] is TfManageDocFrame) then
        Found := True;
      if not Found then
        for i := 0 to pcPages.PageCount-2 do
          if (pcPages.Pages[i].ControlCount > 0) and (pcPages.Pages[i].Controls[0] is TfManageDocFrame) then
            begin
              pcPages.PageIndex:=i;
              Found := True;
            end;
      if not Found then
        begin
          aFrame := TfManageDocFrame.Create(Self);
          pcPages.AddTab(aFrame,True,'',Data.GetLinkIcon('DOCPAGES@'),False);
          AddDocPages(aFrame);
        end;
      if not Data.GotoBookmark(Data.Tree,aEntry.Rec) then
        begin
          Data.SetFilter(Data.Tree,'',0,'','ASC',False,True,True);
          if not Data.GotoBookmark(Data.Tree,aEntry.Rec) then
            exit;
        end;
      if (pcPages.ActivePage.ControlCount > 0) and (pcPages.ActivePage.Controls[0] is TfManageDocFrame) then
        TfManageDocFrame(pcPages.ActivePage.Controls[0]).OpenDir(Data.Tree.Id.AsVariant);
    end;
  etAccount:
    begin
      Application.ProcessMessages;
      if Assigned(pcPages.ActivePage) and (pcPages.ActivePage.ControlCount > 0) and (pcPages.ActivePage.Controls[0] is TfAccountingFrame) then
        Found := True;
      if not Found then
        for i := 0 to pcPages.PageCount-2 do
          if (pcPages.Pages[i].ControlCount > 0) and (pcPages.Pages[i].Controls[0] is TfAccountingFrame) then
            begin
              pcPages.PageIndex:=i;
              Found := True;
            end;
      if not Found then
        begin
          aFrame := TfAccountingFrame.Create(Self);
          pcPages.AddTab(aFrame);
        end;
      if (pcPages.ActivePage.ControlCount > 0) and (pcPages.ActivePage.Controls[0] is TfAccountingFrame) then
        TfAccountingFrame(pcPages.ActivePage.Controls[0]).OpenAccount(aEntry.Rec);
    end;
  etFiles:
    begin
      Application.ProcessMessages;
      if Assigned(pcPages.ActivePage) and (pcPages.ActivePage.ControlCount > 0) and (pcPages.ActivePage.Controls[0] is TfDocumentFrame) then
        Found := True;
      if not Found then
        for i := 0 to pcPages.PageCount-2 do
          if (pcPages.Pages[i].ControlCount > 0) and (pcPages.Pages[i].Controls[0] is TfDocumentFrame) then
            begin
              pcPages.PageIndex:=i;
              Found := True;
            end;
      if not Found then
        begin
          aIFrame := TfDocumentFrame.Create(Self);
          pcPages.AddTab(aIFrame);
        end;
      if (pcPages.ActivePage.ControlCount > 0) and (pcPages.ActivePage.Controls[0] is TfDocumentFrame) then
        with TfDocumentFrame(pcPages.ActivePage.Controls[0]) do
          begin
            TabCaption := strFiles;
            pHeader.Visible := True;
            pLeft.Visible := False;
            DataSet := TDocuments.Create(nil,Data);
            Refresh(1,'D','1',Null,Null);
          end;
    end;
  etDocuments:
    begin
      Application.ProcessMessages;
      for i := 0 to pcPages.PageCount-2 do
        if (pcPages.Pages[i].ControlCount > 0) and (pcPages.Pages[i].Controls[0] is TfManageDocFrame)  then
          begin
            pcPages.PageIndex:=i;
            Found := True;
          end;
      if not Found then
        begin
          aFrame := TfManageDocFrame.Create(Self);
          pcPages.AddTab(aFrame,True,'',Data.GetLinkIcon('DOCPAGES@'),False);
          AddDocPages(aFrame);
        end;
      TfManageDocFrame(pcPages.ActivePage.Controls[0]).OpenDir(Null);
    end;
  etLists:
    begin
      Application.ProcessMessages;
      for i := 0 to pcPages.PageCount-2 do
        if (pcPages.Pages[i].ControlCount > 0) and (pcPages.Pages[i].Controls[0] is TfFilter) and (TfFilter(pcPages.Pages[i].Controls[0]).Dataset is TLists) then
          begin
            pcPages.PageIndex:=i;
            Found := True;
          end;
      if not Found then
        begin
          aFrame := TfFilter.Create(Self);
          pcPages.AddTab(aFrame,True,'',Data.GetLinkIcon('LISTS@'),False);
          AddListsList(aFrame);
          TfFilter(aFrame).Open;
        end;
    end;
  etInventory:
    begin
      Application.ProcessMessages;
      for i := 0 to pcPages.PageCount-2 do
        if (pcPages.Pages[i].ControlCount > 0) and (pcPages.Pages[i].Controls[0] is TfFilter) and (TfFilter(pcPages.Pages[i].Controls[0]).Dataset is TInventorys) then
          begin
            pcPages.PageIndex:=i;
            Found := True;
          end;
      if not Found then
        begin
          aFrame := TfFilter.Create(Self);
          pcPages.AddTab(aFrame,True,'',Data.GetLinkIcon('INVENTORY@'),False);
          AddInventoryList(aFrame);
          TfFilter(aFrame).Open;
        end;
    end;
  etMeetings,etMeetingList:
    begin
      acMeetings.Execute;
    end;
  etTimeRegistering:
    begin
      acTimeRegistering.Execute;
    end;
  end;
end;

procedure TfMain.fMainTreeFrametvMainExpanding(Sender: TObject;
  Node: TTreeNode; var AllowExpansion: Boolean);
var
  DataT: TTreeEntry;
  Node1: TTreeNode;
  aOrderType: TOrderTyp;
  DefaultOrder: Boolean;
  Node2: TTreeNode;
begin
  DataT := TTreeEntry(Node.Data);
  if not Assigned(DataT) then
    exit;
  if (Node.Count=1) and (Node.Items[0].Data=nil) then
    begin
      Node.DeleteChildren;
      case DataT.Typ of
      etFavourites:
        begin
          Data.SetFilter(Data.Tree,Data.QuoteField('PARENT')+'=0 and '+Data.QuoteField('TYPE')+'='+Data.QuoteValue('F'),0,'','ASC',False,True,True);
          Data.Tree.DataSet.First;
          while not Data.Tree.dataSet.EOF do
            begin
              Node1 := fMainTreeFrame.tvMain.Items.AddChildObject(Node,'',TTreeEntry.Create);
              TTreeEntry(Node1.Data).Rec := Data.Tree.GetBookmark;
              TTreeEntry(Node1.Data).DataSource := Data.Tree;
              TTreeEntry(Node1.Data).Text[0] := Data.Tree.FieldByName('NAME').AsString;
              TTreeEntry(Node1.Data).Typ := etDir;
              Node1.HasChildren:=True;
              Data.Tree.DataSet.Next;
            end;
        end;
      etMessages:
        begin
          Data.SetFilter(Data.Tree,Data.QuoteField('PARENT')+'=0 and '+Data.QuoteField('TYPE')+'='+Data.QuoteValue('N')+' OR '+Data.QuoteField('TYPE')+'='+Data.QuoteValue('B'),0,'','ASC',False,True,True);
          Data.Tree.DataSet.First;
          while not Data.Tree.dataSet.EOF do
            begin
              Node1 := fMainTreeFrame.tvMain.Items.AddChildObject(Node,'',TTreeEntry.Create);
              TTreeEntry(Node1.Data).Rec := Data.Tree.GetBookmark;
              TTreeEntry(Node1.Data).DataSource := Data.Tree;
              TTreeEntry(Node1.Data).Text[0] := Data.Tree.FieldByName('NAME').AsString;
              if Data.Tree.FieldByName('TYPE').AsString = 'N' then
                TTreeEntry(Node1.Data).Typ := etMessageDir
              else if Data.Tree.FieldByName('TYPE').AsString = 'B' then
                TTreeEntry(Node1.Data).Typ := etMessageBoard;
              Data.Tree.DataSet.Next;
            end;
        end;
      etTasks:
        begin
          uTasks.AddToMainTree(fMain.acNewTask,fMain.FTaskNode);
        end;
      etCalendar:
        begin
          uCalendarFrame.AddToMainTree(fMain.acNewTermin,fMain.FCalendarNode);
          Node1 := fMainTreeFrame.tvMain.Items.AddChildObject(Node,'',TTreeEntry.Create);
          TTreeEntry(Node1.Data).Typ := etAttPlan;
          TTreeEntry(Node1.Data).Action := fMain.acAttPlan;
        end;
      etOrders:
        begin
          Node1 := fMainTreeFrame.tvMain.Items.AddChildObject(Node,'',TTreeEntry.Create);
          TTreeEntry(Node1.Data).Typ := etOrderList;
          if Data.Users.Rights.Right('ORDERS') > RIGHT_READ then
            begin
              aOrderType := TOrderTyp.Create(nil,Data);
              aOrderType.Open;
              Data.SetFilter(aOrderType,'('+Data.QuoteField('SI_ORDER')+' = ''Y'')');
              aOrderType.DataSet.First;
              DefaultOrder := False;
              while not aOrderType.DataSet.EOF do
                begin
                  Node2 := fMainTreeFrame.tvMain.Items.AddChildObject(Node1.Parent,'',TTreeEntry.Create);
                  if (aOrderType.FieldByName('TYPE').AsString = '0') and (not DefaultOrder) then
                    begin
                      TTreeEntry(Node2.Data).Typ := etAction;
                      TTreeEntry(Node2.Data).Action := fMain.acNewOrder;
                      DefaultOrder := True;
                    end
                  else
                    begin
                      TTreeEntry(Node2.Data).Typ := etNewOrder;
                      TTreeEntry(Node2.Data).Rec := Data.GetBookmark(aOrderType);
                      TTreeEntry(Node2.Data).DataSource := aOrderType;
                      TTreeEntry(Node2.Data).DataSourceType:=TOrderTyp;
                      TTreeEntry(Node2.Data).Text[0] := Format(strNewOrder,[aOrderType.FieldByName('STATUSNAME').AsString]);
                    end;
                  aOrderType.DataSet.Next;
                end;
              Data.SetFilter(aOrderType,'('+Data.QuoteValue('SI_PROD')+' = '+Data.QuoteValue('Y')+')');
              aOrderType.DataSet.First;
              while not aOrderType.DataSet.EOF do
                begin
                  Node2 := fMainTreeFrame.tvMain.Items.AddChildObject(Node1.Parent,'',TTreeEntry.Create);
                  TTreeEntry(Node2.Data).Typ := etNewOrder;
                  TTreeEntry(Node2.Data).Rec := Data.GetBookmark(aOrderType);
                  TTreeEntry(Node2.Data).DataSource := aOrderType;
                  TTreeEntry(Node2.Data).DataSourceType:=TOrderTyp;
                  TTreeEntry(Node2.Data).Text[0] := Format(strNewOrder,[aOrderType.FieldByName('STATUSNAME').AsString]);
                  aOrderType.DataSet.Next;
                end;
              Data.SetFilter(aOrderType,'');
              aOrderType.DataSet.Locate('TYPE','0',[loCaseInsensitive,loPartialKey]);
              fMain.acNewOrder.Caption := Format(strNewOrder,[aOrderType.FieldByName('STATUSNAME').AsString]);
              aOrderType.Free;
            end
          else
            fMain.acNewOrder.Enabled:=False;
        end;
      end;
    end;

  fMainTreeFrame.tvMainExpanding(Sender,Node,AllowExpansion);
end;

procedure TfMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  IPCTimer.Enabled:=False;
  RefreshTimer.Enabled:=False;
  ImportFavorites;
  if Assigned(FTimeReg) then
    begin
      FTimereg.StopActualTime;
      FTimeReg.Destroy;
    end;
  while FHistory.Count>15 do FHistory.Delete(0);
  with Application as IBaseDbInterface do
    DBConfig.WriteString('HISTORY',FHistory.Text);
  if Assigned(fOptions) then
    FreeAndNil(fOptions);
  if Assigned(fHelpContainer) then
    FreeAndNil(fHelpContainer);
  if Assigned(tsHelp) and (tsHelp.ControlCount > 0) then
    tsHelp.Controls[0].Destroy;
  pcPages.CloseAll;
  with Application as IBaseApplication do
    begin
      SaveConfig;
      DoExit;
    end;
end;
procedure TfMain.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  CanClose := True;
  if (uDocumentProcess.ProcessList.Count > 0)
  and (MessageDlg(strProcessesOpen,mtInformation,[mbNo,mbYes],0) = mrNo) then
    CanClose := False;
end;
procedure TfMain.FormCreate(Sender: TObject);
var
  sl: TStringList;
  i: Integer;
  aNewItem : TMenuItem;
begin
  fEnterTime := nil;
  with Application as IBaseApplication do
    begin
      SetConfigName('PrometERP');
      AppVersion:={$I ../base/version.inc};
      AppRevision:={$I ../base/revision.inc};
    end;
  with Application as TBaseVisualApplication do
    OnUserTabAdded:=@ApplicationTBaseVisualApplicationUserTabAdded;
  with Application as IBaseDbInterface do
    if not LoadMandants then
      begin
        Application.Terminate;
        exit;
      end;
  InstallExt('plink', 'Promet-ERP-Link', 'Promet-ERP Link',AppendPathDelim(AppendPathDelim(Application.Location)+'tools')+'linksender'+ExtractFileExt(Application.ExeName),'%1');
  FHistory := THistory.Create;
  FHistory.FwdAction := acForward;
  FHistory.RewAction := acBack;
  bSearch.Caption:=strSearch;
  SearchLinks := TStringList.Create;
  uMainTreeFrame.fMainTreeFrame := TfMainTree.Create(Self);
  fMainTreeFrame.pcPages := pcPages;
  fMainTreeFrame.Parent := tvMain;
  fMainTreeFrame.Align:=alClient;
  fMainTreeFrame.OnNewFromLink:=@fMainTreeFrameNewFromLink;
  fMainTreeFrame.OnOpenFromLink:=@fMainTreeFrameOpenFromLink;
  fMainTreeFrame.OnOpen:=@fMainTreeFrameOpen;
  fMainTreeFrame.OnSelectionChanged:=@fMainTreeFrameSelectionChanged;
  fMainTreeFrame.OnDragOver:=@fMainTreeFrameDragOver;
  fMainTreeFrame.OnDragDrop:=@fMainTreeFrameDragDrop;
  with BaseApplication as IBaseApplication do
    begin
      if Language = '' then
        Language := 'Deutsch';
      IntSetLanguage(Language);
      miLanguage.Clear;
      sl := TStringList.Create;
      if FileExistsUTF8(AppendPathDelim(AppendPathDelim(ProgramDirectory) + 'languages')+'languages.txt') then
        sl.LoadFromFile(UTF8ToSys(AppendPathDelim(AppendPathDelim(ProgramDirectory) + 'languages')+'languages.txt'));
      for i := 0 to sl.Count-1 do
        begin
          aNewItem := TMenuItem.Create(nil);
          aNewItem.Caption := sl[i];
          aNewItem.AutoCheck := True;
          aNewItem.OnClick :=@LanguageItemClick;
          aNewItem.GroupIndex := 11;
          miLanguage.Add(aNewItem);
          if UpperCase(aNewItem.Caption) = UpperCase(Language) then
            begin
              aNewItem.Checked := True;
            end;
        end;
      sl.Free;
    end;
  uprometipc.OnMessageReceived:=@OnMessageReceived;
end;
procedure TfMain.FormDestroy(Sender: TObject);
begin
  SearchLinks.Destroy;
  FHistory.Free;
  uMainTreeFrame.fMainTreeFrame.Destroy;
end;
procedure TfMain.FormShow(Sender: TObject);
begin
  with Application as IBaseApplication do
    RestoreConfig; //Must be called when Mainform is Visible
end;

procedure TfMain.IPCTimerTimer(Sender: TObject);
begin
  IPCTimer.Enabled:=False;
  PeekIPCMessages;
  IPCTimer.Enabled:=True;
end;

procedure TfMain.LanguageItemClick(Sender: TObject);
var
  i: Integer;
begin
  with BaseApplication as IBaseApplication do
    begin
      for i := 0 to miLanguage.Count-1 do
        if miLanguage[i].Caption = Language then
          miLanguage[i].Checked := false;
      TmenuItem(Sender).Checked := True;
      Language := TmenuItem(Sender).Caption;
      IntSetLanguage(Language);
    end;
  tvMain.Invalidate;
end;
procedure TfMain.lbResultsDblClick(Sender: TObject);
begin
  if lbResults.ItemIndex < 0 then exit;
  eContains.SelectNext(eContains,True,True);
  pSearch.Visible:=False;
  Data.GotoLink(SearchLinks[lbresults.ItemIndex]);
end;
procedure TfMain.lbResultsDrawItem(Control: TWinControl; Index: Integer;
  ARect: TRect; State: TOwnerDrawState);
begin
  with Control as TListBox do
    begin
      canvas.fillrect(arect);
      if (Index > -1) and (Data.GetLinkIcon(SearchLinks[Index]) > -1) then
        uBaseVisualControls.fVisualControls.Images.Draw(Canvas,aRect.left,aRect.top,Data.GetLinkIcon(SearchLinks[Index]));
      canvas.textout(aRect.left+16+2,aRect.top,
                     items[index]);
    end;
end;
procedure TfMain.lbResultsExit(Sender: TObject);
begin
  pSearch.Visible:=False;
end;
procedure TfMain.lbResultsKeyPress(Sender: TObject; var Key: char);
begin
  if Key = #27 then
    pSearch.Visible:=false;
end;
procedure TfMain.miOptionsClick(Sender: TObject);
begin
  fOptions.ShowModal;
end;

procedure TfMain.miSettingsClick(Sender: TObject);
begin
  Screen.Cursor:=crHourGlass;
  Application.ProcessMessages;
  if not miOptions.Enabled then
    begin
      fOptions.RegisterOptionsFrame(TfMessageOptions.Create(fOptions),strMessageAccounts,strPersonalOptions);
      fOptions.RegisterOptionsFrame(TfDocumentOptions.Create(fOptions),strFiles,strPersonalOptions);
      fOptions.RegisterOptionsFrame(TfPhoneOptions.Create(fOptions),strPhones,strPersonalOptions);
      if Assigned(Data) and (Data.Users.Rights.Right('OPTIONS') > RIGHT_READ) then
        begin
          fOptions.RegisterOptionsFrame(TfMandantOptions.Create(fOptions),strMandant,strGeneralOptions);
          fOptions.RegisterOptionsFrame(TfProcessOptions.Create(fOptions),strProcesses,strGeneralOptions);
          fOptions.RegisterOptionsFrame(TfSystemOptions.Create(fOptions),strSystem,strGeneralOptions);
          fOptions.RegisterOptionsFrame(TfSyncOptions.Create(fOptions),strSync,strGeneralOptions);
          fOptions.RegisterOptionsFrame(TfStateOptions.Create(fOptions),strStates,strGeneralOptions);
          fOptions.RegisterOptionsFrame(TfCategoryOptions.Create(fOptions),strCategory,strGeneralOptions);
          fOptions.RegisterOptionsFrame(TfOrderTypeOptions.Create(fOptions),strOrderType,strGeneralOptions);
          fOptions.RegisterOptionsFrame(TfUserOptions.Create(fOptions),strUsers,strGeneralOptions);
          fOptions.RegisterOptionsFrame(TfUserFieldOptions.Create(fOptions),strUserFieldDefs,strGeneralOptions);
          fOptions.RegisterOptionsFrame(TfStorageTypeOptions.Create(fOptions),strStorageTypes,strGeneralOptions);
          fOptions.RegisterOptionsFrame(TfCurrencyOptions.Create(fOptions),strCurrencies,strGeneralOptions);
          fOptions.RegisterOptionsFrame(TfLanguageOptions.Create(fOptions),strLanguages,strGeneralOptions);
          fOptions.RegisterOptionsFrame(TfRepairOptions.Create(fOptions),strRepair,strGeneralOptions);
        end;
      miOptions.Enabled:=True;
    end;
  Screen.Cursor:=crDefault;
end;
function TfMain.OpenAction(aLink: string; Sender: TObject): Boolean;
var
  aAction: String;
  aAc: TContainedAction;
begin
  Result := False;
  aAction := copy(aLink,pos('@',aLink)+1,length(aLink));
  aAc := ActionList1.ActionByName(aAction);
  if Assigned(aAc) then
    begin
      aAc.Execute;
      Result := True;
    end;
end;

function TfMain.OpenOption(aLink: string; Sender: TObject): Boolean;
var
  i: Integer;
  aNode: TTreeNode;
begin
  Result := False;
  for i := 0 to fOptions.FormsList.Count-1 do
    if UpperCase(TOptionsFrame(fOptions.FormsList.Items[i]).ClassName)=UpperCase(copy(aLink,pos('@',aLink)+1,length(aLink))) then
      begin
        aNode := fOptions.tvMain.Items[0];
        while Assigned(aNode) and (aNode.Text<>TOptionsFrame(fOptions.FormsList.Items[i]).Caption) do
          aNode := aNode.GetNext;
        if Assigned(aNode) and (aNode.Text=TOptionsFrame(fOptions.FormsList.Items[i]).Caption) then
          begin
            fOptions.tvMain.Selected:=aNode;
            fOptions.ShowModal;
            Result := True;
          end;
      end;
end;

procedure TfMain.pmHistoryPopup(Sender: TObject);
var
  aItem: TMenuItem;
  i: Integer;
begin
  pmHistory.Items.Clear;
  for i := FHistory.Count-1 downto 0 do
    begin
      aItem := TMenuItem.Create(nil);
      aItem.Caption:=Data.GetLinkDesc(FHistory[i]);
      aItem.OnClick:=@aItemClick;
      if i = FHistory.HistoryIndex then
        aItem.Default := True;
      aItem.Tag := i;
      pmHistory.Items.Add(aItem);
    end;
end;
procedure TfMain.DoRefreshActiveTab(Sender: TObject);
begin
  if WindowState = wsMinimized then exit;
  if not Visible then exit;
  if Assigned(pcPages.ActivePage) then
    if pcPages.ActivePage.ControlCount > 0 then
      if pcPages.ActivePage.Controls[0] is TExtControlFrame then
        TExtControlFrame(pcPages.ActivePage.Controls[0]).DoRefresh;
end;
procedure TfMain.SearchTimerTimer(Sender: TObject);
begin
  SearchTimer.Enabled:=False;
  bSearchClick(nil);
end;
procedure TfMain.SenderTfFiltergListDrawColumnCell(Sender: TObject;
  const Rect: TRect; DataCol: Integer; Column: TColumn; State: TGridDrawState);
begin
  if ((not Assigned(TDBgrid(Sender).DataSource))
  or (not Assigned(TDBgrid(Sender).DataSource.DataSet))
  or (not TDBgrid(Sender).DataSource.DataSet.Active)
  ) then exit;
  with (Sender as TDBGrid), Canvas do
    begin
      Canvas.FillRect(Rect);
      if Column.FieldName = 'NAME' then
        begin
          if (not TDBgrid(Sender).DataSource.DataSet.FieldByName('TARGET').IsNull)
          and (TDBgrid(Sender).DataSource.DataSet.FieldByName('TARGET').AsDateTime < Now())
          and (TDBgrid(Sender).DataSource.DataSet.FieldByName('END').IsNull)
          then
            Canvas.Font.Color := clRed;
          DefaultDrawColumnCell(Rect, DataCol, Column, State);
        end
      else if Column.FieldName = 'TYPE' then
        begin
          if TDBgrid(Sender).DataSource.DataSet.FieldByName('TYPE').AsString='C' then
            fVisualControls.Images.Draw(Canvas,Rect.Left,Rect.Top,103)
          else
            fVisualControls.Images.Draw(Canvas,Rect.Left,Rect.Top,13)
        end
      else
        begin
          DefaultDrawColumnCell(Rect, DataCol, Column, State);
        end;
      end;
end;
procedure TfMain.SenderTfFilterViewDetails(Sender: TObject);
begin
  Data.GotoLink('ORDERS@'+TfFilter(Sender).DataSet.FieldByName('ORDERNO').AsString);
end;

procedure TfMain.SenderTfMainTaskFrameControlsSenderTfMainTaskFrameTfTaskFrameStartTime
  (Sender: TObject; aProject, aTask: string);
begin
  if Assigned(FTimeReg) then
    begin
      FTimeReg.Project:=aProject;
      FTimeReg.Task:=aTask;
      FTimeReg.Link:='';
      FTimereg.mNotes.Clear;
      FTimereg.acStartExecute(nil);
    end;
end;

procedure TfMain.TfFilteracOpenExecute(Sender: TObject);
begin
  if Assigned(pcPages.ActivePage) and (pcPages.ActivePage.ControlCount > 0) and (pcPages.ActivePage.Controls[0] is TfFilter) then
    begin
      if TfFilter(pcPages.ActivePage.Controls[0]).DataSet.Count>0 then
        Data.GotoLink(TfFilter(pcPages.ActivePage.Controls[0]).DataSet.FieldByName('LINK').AsString);
    end;
end;

end.

