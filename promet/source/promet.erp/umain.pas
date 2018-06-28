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
  Classes, SysUtils, FileUtil, ExtendedNotebook, Forms, Controls, Graphics,
  Dialogs, Menus, ComCtrls, ExtCtrls, ActnList, Buttons, StdCtrls,
  uBaseApplication, uBaseDBClasses, uExtControls, uBaseVisualApplication, db,
  uBaseSearch, uMainTreeFrame, uWikiFrame, DBGrids, Grids, CheckLst, types,
  uEnterTime, uBaseDatasetInterfaces;
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
    acAttPlan: TAction;
    acNewAccount: TAction;
    acRoughPlanning: TAction;
    acStartPage: TAction;
    acProjectOverview: TAction;
    acStatistics: TAction;
    acSalesListBook: TAction;
    acCommandline: TAction;
    acElements: TAction;
    acNewObject: TAction;
    acPasswords: TAction;
    acNewScript: TAction;
    acMasterdataVersionate: TAction;
    acProduction: TAction;
    acShowSearch: TAction;
    acSearchOptions: TAction;
    acNewScheme: TAction;
    acTimeRegistration: TAction;
    acNewProductionOrder: TAction;
    acWindowize: TAction;
    acWiki: TAction;
    ActionList1: TActionList;
    ApplicationProperties1: TApplicationProperties;
    bBack: TSpeedButton;
    bDependencies: TSpeedButton;
    Bevel1: TBevel;
    Bevel3: TBevel;
    Bevel6: TBevel;
    Bevel7: TBevel;
    bFfwd: TToolButton;
    bPauseTime: TSpeedButton;
    cbSearchIn: TCheckListBox;
    cbSearchType: TCheckListBox;
    eSearch: TEdit;
    IPCTimer: TIdleTimer;
    Label3: TLabel;
    Label6: TLabel;
    lSearchIn: TLabel;
    lSearchtype: TLabel;
    MenuItem3: TMenuItem;
    Menuitem12: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    Panel1: TPanel;
    pSearchOptions: TPanel;
    pPages: TPanel;
    Panel6: TPanel;
    pCloseTab: TPanel;
    pSeparateWindow: TPanel;
    pTimes: TPanel;
    pTimes1: TPanel;
    RefreshTimer: TIdleTimer;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    miHelpIndex: TMenuItem;
    miHelp: TMenuItem;
    miOptions: TMenuItem;
    miLanguage: TMenuItem;
    miSettings: TMenuItem;
    pmHistory: TPopupMenu;
    pcPages: TExtMenuPageControl;
    MainMenu1: TMainMenu;
    miView: TMenuItem;
    miLogout: TMenuItem;
    miLogin: TMenuItem;
    miMandant: TMenuItem;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    spTree: TSplitter;
    tbMenue: TToolButton;
    tbTreeVisible: TSpeedButton;
    tbTreeVisible1: TSpeedButton;
    SearchTimer: TTimer;
    ToolBar1: TToolBar;
    ToolBar2: TToolBar;
    ToolButton1: TToolButton;
    tvSearch: TTreeView;
    tsStartpage: TTabSheet;
    tvMain: TPanel;
    tvSearchP: TPanel;
    tvMainAll: TPanel;
    procedure acAttPlanExecute(Sender: TObject);
    procedure acBackExecute(Sender: TObject);
    procedure acBookInventoryExecute(Sender: TObject);
    procedure acCalendarExecute(Sender: TObject);
    procedure acChangePasswortExecute(Sender: TObject);
    procedure acClearListExecute(Sender: TObject);
    procedure acCloseTabExecute(Sender: TObject);
    procedure acCollectInventoryExecute(Sender: TObject);
    procedure acCombineSaleItemsExecute(Sender: TObject);
    procedure acCommandlineExecute(Sender: TObject);
    procedure acContactExecute(Sender: TObject);
    procedure acDeleteListeEntryExecute(Sender: TObject);
    procedure acDeleteWholeMessageDirExecute(Sender: TObject);
    procedure acElementsExecute(Sender: TObject);
    procedure acForwardExecute(Sender: TObject);
    procedure acHelpIndexExecute(Sender: TObject);
    procedure acInfoExecute(Sender: TObject);
    procedure acLoginExecute(Sender: TObject);
    procedure acLogoutExecute(Sender: TObject);
    procedure acMasterdataExecute(Sender: TObject);
    procedure acMasterdataVersionateExecute(Sender: TObject);
    procedure acMeetingsExecute(Sender: TObject);
    procedure acMessagesExecute(Sender: TObject);
    procedure acNewAccountExecute(Sender: TObject);
    procedure acNewContactExecute(Sender: TObject);
    procedure acNewInventoryExecute(Sender: TObject);
    procedure acNewListExecute(Sender: TObject);
    procedure acNewMasterdataExecute(Sender: TObject);
    procedure acNewMeetingExecute(Sender: TObject);
    procedure acNewMessageExecute(Sender: TObject);
    procedure acNewObjectExecute(Sender: TObject);
    procedure acNewOrderExecute(Sender: TObject);
    procedure acNewProductionOrderExecute(Sender: TObject);
    procedure acNewProjectExecute(Sender: TObject);
    procedure acNewSchemeExecute(Sender: TObject);
    procedure acNewScriptExecute(Sender: TObject);
    procedure acNewStatisticsExecute(Sender: TObject);
    procedure acNewTaskExecute(Sender: TObject);
    procedure acNewTerminExecute(Sender: TObject);
    procedure acOpenExecute(Sender: TObject);
    procedure acOrdersExecute(Sender: TObject);
    procedure acPasswordsExecute(Sender: TObject);
    procedure acPauseTimeExecute(Sender: TObject);
    procedure acProductionExecute(Sender: TObject);
    procedure acProjectOverviewExecute(Sender: TObject);
    procedure acProjectsExecute(Sender: TObject);
    procedure acRoughPlanningExecute(Sender: TObject);
    procedure acSalesListBookExecute(Sender: TObject);
    procedure acSalesListExecute(Sender: TObject);
    procedure acSalesListPayExecute(Sender: TObject);
    procedure acSearchOptionsExecute(Sender: TObject);
    procedure acShowSearchExecute(Sender: TObject);
    procedure acShowTreeExecute(Sender: TObject);
    procedure acStandartTimeExecute(Sender: TObject);
    procedure acStartPageExecute(Sender: TObject);
    procedure acStatisticsExecute(Sender: TObject);
    procedure acTaskPlanExecute(Sender: TObject);
    procedure acTasksExecute(Sender: TObject);
    procedure acTimeRegisteringExecute(Sender: TObject);
    procedure acTimeRegistrationExecute(Sender: TObject);
    procedure acWikiExecute(Sender: TObject);
    procedure acWindowizeExecute(Sender: TObject);
    procedure aFrameInvDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure aFrameInvDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure aFrameTfFilterClose(Sender: TObject; var CloseAction: TCloseAction
      );
    procedure aFrameTfFilterDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure aFrameTfFilterDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure aItemClick(Sender: TObject);
    procedure ApplicationProperties1ShowHint(var HintStr: string;
      var CanShow: Boolean; var HintInfo: THintInfo);
    procedure ApplicationTBaseVisualApplicationUserTabAdded(Sender: TObject);
    procedure cbSearchTypeClick(Sender: TObject);
    procedure eSearchChange(Sender: TObject);
    procedure eSearchEnter(Sender: TObject);
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
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure fSearchActiveSearchItemFound(aIdent: string; aName: string;
      aStatus: string; aActive: Boolean; aLink: string; aPriority: Integer=0;
      aItem: TBaseDBList=nil);
    procedure fSearchItemFound(aIdent: string; aName: string; aStatus: string;
      aActive: Boolean; aLink: string; aPriority: Integer=0; aItem: TBaseDBList=
      nil);
    procedure fSearchSearchDone(Sender: TObject);
    procedure IPCTimerTimer(Sender: TObject);
    procedure miOptionsClick(Sender: TObject);
    function OpenAction(aLink: string; Sender: TObject): Boolean;
    function OpenFilter(aLink: string; Sender: TObject): Boolean;
    function OpenOption(aLink: string; Sender: TObject): Boolean;
    procedure pmHistoryPopup(Sender: TObject);
    procedure DoRefreshActiveTab(Sender: TObject);
    procedure SearchTimerTimer(Sender: TObject);
    procedure SenderTfFilterDrawColumnCell(Sender: TObject; const Rect: TRect;
      DataCol: Integer; Column: TColumn; State: TGridDrawState);
    procedure SenderTfFiltergListDrawColumnCell(Sender: TObject;
      const Rect: TRect; DataCol: Integer; Column: TColumn;
      State: TGridDrawState);
    procedure SenderTfFilterViewDetails(Sender: TObject);
    procedure SenderTfFilterViewElementDetails(Sender: TObject);

      procedure SenderTfMainTaskFrameControlsSenderTfMainTaskFrameTfTaskFrameStartTime
        (Sender: TObject; aProject, aTask, aCategory: string);
    procedure TfFilteracOpenExecute(Sender: TObject);
    procedure tvSearchDblClick(Sender: TObject);
    procedure tvSearchStartDrag(Sender: TObject; var DragObject: TDragObject);
  private
    { private declarations }
    WikiFrame: TfWikiFrame;
    FHistory: THistory;
    SearchText: String;
    ActiveSearch : TSearch;
    SearchLinks : TStringList;
    FMessageNode : TTreeNode;
    FCalendarNode : TTreeNode;
    FTaskNode : TTreeNode;
    FTimeReg : TfEnterTime;
    FSearchNode: TTreeNode;
    aTime : Int64;
    procedure AddCustomerList(Sender: TObject);
    procedure AddMasterdataList(Sender: TObject);
    procedure AddOrderList(Sender: TObject);
    procedure AddProductionOrderList(Sender: TObject);
    procedure AddCommandline(Sender: TObject);
    procedure AddStatisticList(Sender: TObject);
    procedure AddListsList(Sender: TObject);
    procedure AddDocPages(Sender: TObject);
    procedure AddInventoryList(Sender: TObject);
    procedure AddSalesList(Sender: TObject);
    procedure AddCalendar(Sender: TObject);
    procedure AddProjectList(Sender: TObject);
    procedure AddTaskList(Sender: TObject);
    procedure AddMeetingList(Sender : TObject);
    procedure AddWiki(Sender: TObject);
    procedure AddSchemeList(Sender : TObject);
    procedure AddElementList(Sender: TObject);
    function CommandReceived(Sender : TObject;aCommand : string) : Boolean;
    procedure RefreshCalendar;
    procedure RefreshMessages;
    procedure RefreshTasks;
    procedure ImportFavorites;
  public
    { public declarations }
    property TimeReg : TfEnterTime read FTimeReg;
    function DoCreate : Boolean;
  end;

  { TStarterThread }

  TStarterThread = class(TThread)
  private
    Node,Node1,Node2,Node3,aNode : TTreeNode;
    miNew: TMenuItem;
    aConn: TComponent;
    aRightIn : string;
    aRightOut : Integer;
    FInfo : string;
    DataSetType : TBaseDBDatasetClass;
    procedure NewNode;
    procedure NewNode1;
    procedure NewNode2;
    procedure NewNode3;
    procedure NewMenu;
    procedure ShowAll;
    procedure AddTimeReg;
    procedure AddTimeReg2;
    procedure NewConn;
    procedure StartReceive;
    procedure DoStartupType;
    procedure RefreshTasks;
    procedure Expand;
    procedure AddWiki;
    procedure DoGetRight;
    procedure AddSearch;
    procedure RegisterPhoneLines;
    function GetRight(aRight : string) : Integer;
    procedure DoRealInfo;
    procedure DoCalendar;
    procedure DoInfo(aMsg : string);
    procedure DoSynchronize(AMethod: TThreadMethod);
  public
    constructor Create(aSuspended : Boolean = False);
    procedure Execute; override;
    destructor Destroy; override;
  end;

var
  fMain: TfMain;
implementation
{$R *.lfm}
uses uBaseDBInterface,uIntfStrConsts,uSearch,uFilterFrame,uPerson,uData,lazutf8sysutils,
  uPersonFrame, uPrometFrames, uMessageFrame, uMessageEdit, LCLType, uCalendarFrame,
  uAccounting,uAccountingFrame,uAccountingQue,uAccountingTransfer,uMessages,uDocuments,
  uOrder,uArticleFrame,uMasterdata,uOrderFrame,uBookAccounting,
  uOptions,uUserOptions,uMandantOptions,utableoptions,uSystemOptions,uStateOptions,
  uCategoryOptions,uOrderTypeOptions,
  uUserFieldDefOptions,uStorageTypeOptions,uCurrencyOptions,uLanguageOptions,
  uRepairOptions, uSyncOptions, uDocumentOptions, uPhoneOptions, uMailOptions,
  uScriptOptions,uvisualoptions,uProcessManager,
  uHelpContainer,uProjects,uProjectFrame,Math,uSkypePhone,LCLIntf,uWiki,
  uTask,uDocumentProcess,uDocumentFrame,uPrometFramesInplaceDB,uInfo,
  uProcessOptions,Utils,uBaseERPDBClasses,umaintasks,utasks,uTaskEdit,LCLProc,
  usplash,ufavorites,uBaseVisualControls,uStatisticFrame,uwait,uprometipc,uMeetingFrame,
  umeeting,uEditableTab,umanagedocframe,uBaseDocPages,uTaskPlan,uattendanceplan,
  uTimeFrame,uTimeOptions,uWizardnewaccount,uCalendar,uRoughpklanningframe,uStatistic,
  uOptionsFrame,uprojectoverviewframe,uimportoptions,uEventEdit,uGeneralStrConsts,
  ufinancialoptions,ubookfibuaccount,ucommandline,uobjectframe,uscriptframe,uprometscripts,
  uPasswords,uArticleVersion,uscheme,uschemeframe,utimeregistration
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
  strDatabaseSettings           = 'Datenbankeinstellungen';
  strScripts                    = 'Scripte';
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
  strVisualOptions              = 'Ansichtsoptionen';
  strSearchText                 = '<hier tippen um zu suchen>';
  strProcessesOpen              = 'Es sind noch Dateien offen (Dateiverwaltung), wirklich schließen ?';
  strMessageOpen                = 'Es sind noch Nachrichten offen (e-Mail), wirklich schließen ?';
  strNewInventory               = 'Neue Inventur';
  strInventory                  = 'Inventur';
  strDeletingMessages           = 'lösche Nachrichten, noch %d Nachichten';
  strLogin                      = 'Login...';
  strNoStartPage = '<b>keine Startseite vorhanden, bitte fragen Sie Ihren '
    +'Administrator nach der Einrichtung einer Startseite</b>';

function OnMessageReceived(aMessage: string): Boolean;
var
  tmp: String;
begin
  Result := False;
  if copy(aMessage,0,9) = 'OpenLink(' then
    begin
      if fMain.WindowState=wsMinimized then
        fMain.WindowState:=wsNormal;
      fMain.BringToFront;
      tmp := copy(aMessage,10,length(aMessage));
      tmp := copy(tmp,0,length(tmp)-1);
      Data.GotoLink(tmp);
      Result := True;
    end
  else
    fMain.CommandReceived(fMain,aMessage);
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
procedure TfMain.ImportFavorites;
var
  aList : TStrings;
  aLinks: TLinks;
  i: Integer;
  aLink: String;
begin
  Data.Tree.DataSet.Filter:=Data.QuoteField('NAME')+'='+Data.QuoteValue(strBrowserFavourites)+' and '+Data.QuoteField('TYPE')+'='+Data.QuoteValue('F');
  Data.Tree.DataSet.Filtered:=True;
  if Data.Tree.Count > 0 then
    begin
      aList := GetFavorites;
      aLinks := TLinks.Create(nil);
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
              aLinks.FieldByName('CHANGEDBY').AsString := Data.Users.IDCode.AsString;
              aLinks.DataSet.Post;
            end
          else break;
        end;
      aLinks.Free;
      aList.Free;
    end;
  Data.Tree.DataSet.Filtered:=false;
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
    end
  else if Assigned(FTimeReg) then
    Result :=FTimeReg.CommandReceived(Sender,aCommand);
end;
procedure TfMain.AddCustomerList(Sender : TObject);
begin
  with Sender as TfFilter do
    begin
      TabCaption := strCustomerList;
      FilterType:='C';
      FilterRow:=True;
      DefaultRows:='GLOBALWIDTH:%;ACCOUNTNO:100;NAME:400;MATCHCODE:200;';
      Dataset := TPersonList.Create(nil);
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
      FilterRow:=True;
      DefaultRows:='GLOBALWIDTH:%;ID:150;VERSION:100;LANGUAGE:60;MATCHCODE:200;SHORTTEXT:400;';
      Dataset := TMasterdataList.Create(nil);
      //gList.OnDrawColumnCell:=nil;
      if (Data.Users.Rights.Right('MASTERDATA') > RIGHT_READ) or (Data.Users.Rights.Right('ARTICLES') > RIGHT_READ) or (Data.Users.Rights.Right('BENEFITS') > RIGHT_READ) or (Data.Users.Rights.Right('PARTSLIST') > RIGHT_READ) then
        begin
          AddToolbarAction(acNewMasterdata);
          AddContextAction(acMasterdataVersionate);
        end;
    end;
end;
procedure TfMain.AddOrderList(Sender: TObject);
var
  forderFrame : TfOrderFrame;
  aFilter: String = '';
begin
  with Sender as TfFilter do
    begin
      Tag := 0; //Type Orders
      TabCaption := strOrderList;
      FilterType:='O';
      FilterRow:=True;
      DefaultRows:='GLOBALWIDTH:%;STATUS:50;NUMBER:100;CUSTNO:100;CUSTNAME:300;PAYEDON:28;DELIVERED:28;DONE:28;';
      Dataset := TOrderList.Create(nil);
      TOrderList(DataSet).OrderType.Filter('');
      TOrderList(DataSet).OrderType.First;
      while not TOrderList(DataSet).OrderType.EOF do
        begin
          if TOrderList(DataSet).OrderType.FieldByName('SI_ORDER').AsString='Y' then
            aFilter += ' OR '+Data.QuoteField('STATUS')+'='+Data.QuoteValue(TOrderList(DataSet).OrderType.FieldByName('STATUS').AsString);
          TOrderList(DataSet).OrderType.Next;
        end;
      aFilter := copy(aFilter,4,length(aFilter));
      aFilter := '('+aFilter+') AND ('+Data.QuoteField('ACTIVE')+'<>'+Data.QuoteValue('N')+')';
      BaseFilter:=aFilter;
      acFilter.Execute;
      OnDrawColumnCell:=@fOrderFrame.gListDrawColumnCell;
      if Data.Users.Rights.Right('ORDERS') > RIGHT_READ then
        AddToolbarAction(acNewOrder);
    end;
end;

procedure TfMain.AddProductionOrderList(Sender: TObject);
var
  forderFrame : TfOrderFrame;
  aFilter: String = '';
begin
  with Sender as TfFilter do
    begin
      Tag:=1; //Productionorders
      TabCaption := strProductionOrders;
      FilterType:='OP';
      FilterRow:=True;
      DefaultRows:='GLOBALWIDTH:%;STATUS:50;NUMBER:100;CUSTNO:100;CUSTNAME:300;PAYEDON:28;DELIVERED:28;DONE:28;';
      Dataset := TOrderList.Create(nil);
      TOrderList(DataSet).OrderType.Filter('');
      TOrderList(DataSet).OrderType.First;
      while not TOrderList(DataSet).OrderType.EOF do
        begin
          if TOrderList(DataSet).OrderType.FieldByName('SI_PROD').AsString='Y' then
            aFilter += ' OR '+Data.QuoteField('STATUS')+'='+Data.QuoteValue(TOrderList(DataSet).OrderType.FieldByName('STATUS').AsString);
          TOrderList(DataSet).OrderType.Next;
        end;
      aFilter := copy(aFilter,4,length(aFilter));
      BaseFilter:=aFilter;
      acFilter.Execute;
      OnDrawColumnCell:=@fOrderFrame.gListDrawColumnCell;
      if Data.Users.Rights.Right('ORDERS') > RIGHT_READ then
        AddToolbarAction(acNewProductionOrder);
    end;
end;

procedure TfMain.AddCommandline(Sender: TObject);
begin
  with Sender as TfCommandline do
    begin
      TabCaption := strCommandline;
    end;
end;

procedure TfMain.AddStatisticList(Sender: TObject);
begin
  with Sender as TfFilter do
    begin
      TabCaption := strStatisticList;
      FilterType:='U';
      FilterRow:=True;
      DefaultRows:='GLOBALWIDTH:%;NAME:100;CHANGEDBY:30;TIMESTAMPD:50;';
      Dataset := TStatistic.Create(nil);
      if Data.Users.Rights.Right('STATISTICS') > RIGHT_READ then
        AddToolbarAction(acNewStatistics);
    end;
end;

procedure TfMain.AddListsList(Sender: TObject);
begin
  with Sender as TfFilter do
    begin
      TabCaption := strLists;
      FilterType:='L';
      FilterRow:=True;
      DefaultRows:='GLOBALWIDTH:%;NAME:100;';
      Dataset := TLists.Create(nil);
      //gList.OnDrawColumnCell:=nil;
      Editable:=True;
      AddToolbarAction(acNewList);
    end;
end;

procedure TfMain.AddDocPages(Sender: TObject);
begin
  with Sender as TfManageDocFrame do
    begin
      Open(TfManageDocFrame(Sender).Typ);
      if Assigned(TFrame(Sender).OnEnter) then
        TFrame(Sender).OnEnter(Sender);
    end;
end;

procedure TfMain.AddInventoryList(Sender: TObject);
begin
  with Sender as TfFilter do
    begin
      TabCaption := strInventory;
      FilterType:='INV';
      FilterRow:=True;
      DefaultRows:='GLOBALWIDTH:%;INVNO:40;DESC:100;DATE:70;CREATEDBY:30;';
      Dataset := TInventorys.Create(nil);
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
      FilterRow:=True;
      DefaultRows:='GLOBALWIDTH:%;PAYEDON:100;ORDERNO:100;STATUS:30;NUMBER:100;CUSTNO:70;CUSTNAME:100;NETPRICE:50;DISCOUNT:50;VATH:50;VATF:50;GROSSPRICE:100;ACCOUNT:100;';
      ads := TAccountingJournal.Create(nil);
      ads.CreateTable;
      Dataset := ads;
      //gList.OnDrawColumnCell:=nil;
      AddToolbarAction(acSalesListBook);
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
      OpenDir(Data.Users.Id.AsVariant);
    end;
end;
procedure TfMain.AddProjectList(Sender: TObject);
begin
  with Sender as TfFilter do
    begin
      TabCaption := strProjectList;
      FilterType:='P';
      FilterRow:=True;
      DefaultRows:='GLOBALWIDTH:%;TYPE:30;ID:70;NAME:100;STATUS:60;';
      Dataset := TProjectList.Create(nil);
      with DataSet.DataSet as IBaseDbFilter do
        UsePermissions := True;
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
          GridView.NumberField:='LPRIORITY';
          GridView.SortField:='LPRIORITY';
          TfMainTaskFrame(Sender).Connection := Data.GetNewConnection;
          aDataset := TTaskList.CreateEx(nil,Data,Connection);
          BaseFilter:=Data.QuoteField('ACTIVE')+'='+Data.QuoteValue('Y');
          aDataSet.Open;
          tbTop.Visible:=True;
          pToolbar.Visible:=False;
          TfMainTaskFrame(Sender).DataSet := aDataSet;
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
      FilterRow:=True;
      FilterType:='E';
      DefaultRows:='GLOBALWIDTH:%;NAME:200;STATUS:60;DATE:100;CREATEDBY:60;';
      Dataset := TMeetings.Create(nil);
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
      OpenFromLink('WIKI@Wiki/index');
    end;
end;

procedure TfMain.AddSchemeList(Sender: TObject);
begin
  with Sender as TfFilter do
    begin
      Caption := strSchemeList;
      FilterType:='SH';
      FilterRow:=True;
      DefaultRows:='GLOBALWIDTH:%;NAME:100;STATUS:60;';
      Dataset := TSchemeList.Create(nil);
      gList.OnDrawColumnCell:=nil;
      AddToolbarAction(acNewScheme);
    end;
end;

procedure TfMain.AddElementList(Sender: TObject);
var
  aObj: TObjects;
  aBtn: TToolButton;
  aItem: TMenuItem;
begin
  with Sender as TfFilter do
    begin
      TabCaption := strObjectList;
      FilterType:='D';
      FilterRow:=True;
      DefaultRows:='GLOBALWIDTH:%;ICON:10;NAME:100;NUMBER:70;STATUS:60;';
      aObj := TObjects.Create(nil);
      with aObj.DataSet as IBaseDbFilter do
        UsePermissions := True;
      TfFilter(Sender).Dataset := aObj;
      aBtn := AddToolbarAction(acNewObject);
      OnDrawColumnCell:=@SenderTfFilterDrawColumnCell;
      OnViewDetails:=@SenderTfFilterViewElementDetails;
      aBtn.DropdownMenu := TPopupMenu.Create(aBtn);
      aBtn.Style:=tbsDropDown;

      aItem := TMenuItem.Create(aBtn);
      aBtn.DropdownMenu.Items.Add(aItem);
      aItem.Action := acNewObject;

      aItem := TMenuItem.Create(aBtn);
      aBtn.DropdownMenu.Items.Add(aItem);
      aItem.Action := acNewContact;

      aItem := TMenuItem.Create(aBtn);
      aBtn.DropdownMenu.Items.Add(aItem);
      aItem.Action := acNewOrder;

      aItem := TMenuItem.Create(aBtn);
      aBtn.DropdownMenu.Items.Add(aItem);
      aItem.Action := acNewMasterdata;

      aItem := TMenuItem.Create(aBtn);
      aBtn.DropdownMenu.Items.Add(aItem);
      aItem.Action := acNewTermin;

      aItem := TMenuItem.Create(aBtn);
      aBtn.DropdownMenu.Items.Add(aItem);
      aItem.Action := acNewMeeting;

      aItem := TMenuItem.Create(aBtn);
      aBtn.DropdownMenu.Items.Add(aItem);
      aItem.Action := acNewScheme;

      aItem := TMenuItem.Create(aBtn);
      aBtn.DropdownMenu.Items.Add(aItem);
      aItem.Action := acNewScript;
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

procedure TStarterThread.AddTimeReg;
begin
  try
  if (Data.Users.Rights.Right('TIMEREG') > RIGHT_NONE) then
    begin
      if not Assigned(fOptions) then
        Application.CreateForm(TfOptions,fOptions);
      fOptions.RegisterOptionsFrame(TfTimeOptions.Create(fOptions),strTimetools,strPersonalOptions);
      Application.CreateForm(TfEnterTime,fMain.FTimeReg);
      fMain.FTimeReg.Node:=MainNode;
      fMain.FTimeReg.PauseBtn := fMain.bPauseTime;
      fMain.FTimeReg.DoSetup;
      fMain.FTimeReg.SetupDB;
      fMain.pTimes.Visible := True;
      SendIPCMessage('noop',GetTempDir+'PMSTimeregistering');
    end;
  except
    DoInfo('Timeregistering Error:');
  end;
end;

procedure TStarterThread.AddTimeReg2;
begin
  try
  if (Data.Users.Rights.Right('TIMEREG') > RIGHT_NONE) then
    begin
      if Assigned(fMain.FTimeReg) then
        begin
          MainNode := fMainTreeFrame.tvMain.Items.AddChildObject(nil,'',TTreeEntry.Create);
          MainNode.Height := 34;
          TTreeEntry(MainNode.Data).Typ := etTimeRegistering;
          fMain.FTimeReg.Node := MainNode;
          fMain.FTimeReg.RefreshNode;
          Node1 := fMainTreeFrame.tvMain.Items.AddChildObject(MainNode,'',TTreeEntry.Create);
          TTreeEntry(Node1.Data).Typ := etTimeRegisteringsmall;
          Node := fMainTreeFrame.tvMain.Items.AddChildObject(MainNode,'',TTreeEntry.Create);
          TTreeEntry(Node.Data).Typ := etTimeRegistration;
        end;
    end;
  except
    MainNode.Free;
    DoInfo('Timeregistering Error:RefreshNode');
  end;
end;

procedure TStarterThread.NewConn;
begin
  aConn := Data.GetNewConnection;
end;

procedure TStarterThread.StartReceive;
begin
  {$ifndef heaptrc}
  try
    TBaseVisualApplication(Application).MessageHandler.SendCommand('*receiver','Receive('+Data.Users.FieldByName('NAME').AsString+')');
  except
  end;
  {$endif}
  fMain.RefreshMessages;
end;

procedure TStarterThread.DoStartupType;
var
  aOldSelected: String;
begin
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
    end
  else
    begin
      with Application as IBaseDbInterface do
        begin
          aOldSelected := DBConfig.ReadString('TREENODE','');
          if fMainTreeFrame.tvMain.Items.Count > 0 then
            aNode := fMainTreeFrame.tvMain.Items[0];
          while Assigned(aNode) do
            begin
              if (aOldSelected = aNode.Text)
              or (aOldSelected = fMainTreeFrame.GetNodeText(aNode)) then
                begin
                  fMainTreeFrame.tvMain.Selected := aNode;
                  break;
                end;
              aNode := aNode.GetNextSibling;
            end;
        end;
    end;
end;
procedure TStarterThread.RefreshTasks;
begin
  uTasks.RefreshTasks(fMain.FTaskNode);
end;

procedure TStarterThread.Expand;
begin
  fMainTreeFrame.RestoreExpands;
end;

procedure TStarterThread.AddWiki;
var
  aWiki: TWikiList;
  aStartPagetext: String;
  aUser: TUser;
begin
  aWiki := TWikiList.Create(nil);
  aWiki.CreateTable;
  try
    fMain.WikiFrame.SetRights(True);
    aUser := TUser.Create(nil);
    aUser.Open;
    aUser.Locate('SQL_ID',Data.Users.Id.AsVariant,[]);
    while (not aWiki.FindWikiPage('Promet-ERP-Help/users/'+StringReplace(aUser.UserName.AsString,' ','_',[rfReplaceAll]))) and (not aUser.FieldByName('PARENT').IsNull) do
      begin
        aUser.Locate('SQL_ID',aUser.FieldByName('PARENT').AsVariant,[]);
      end;
    if not fMain.WikiFrame.OpenWikiPage('Promet-ERP-Help/users/'+StringReplace(aUser.UserName.AsString,' ','_',[rfReplaceAll]),false) then
      if not fMain.WikiFrame.OpenWikiPage('Promet-ERP-Help/users/Administrator',false) then
        fMain.WikiFrame.ShowHTML(strNoStartPage);
  except
    fMain.WikiFrame.DataSet.Cancel;
  end;
  aWiki.Free;
end;

procedure TStarterThread.DoGetRight;
begin
  aRightOut := Data.Users.Rights.Right(aRightIN)
end;

procedure TStarterThread.AddSearch;
begin
  if Assigned(fMain.FSearchNode) then
    fMain.FSearchNode.Visible:=True;
end;

procedure TStarterThread.RegisterPhoneLines;
begin
  {$IFDEF CPU32}
  try
    uSkypePhone.RegisterPhoneLines;
  except
  end;
  {$ENDIF}
  {$IFDEF WINDOWS}
  {$IFDEF CPU32}
  try
  uTAPIPhone.RegisterPhoneLines;
  except
  end;
  {$ENDIF}
  {$ENDIF}
end;

function TStarterThread.GetRight(aRight: string): Integer;
begin
  aRightIn := aRight;
  Synchronize(@DoGetRight);
  Result := aRightOut;
end;

procedure TStarterThread.DoRealInfo;
begin
  Application.ProcessMessages;
  with Application as IBaseApplication do
    Info('StarterThread:'+FInfo);
end;

procedure TStarterThread.DoCalendar;
begin
  fMain.RefreshCalendar;
end;

procedure TStarterThread.DoInfo(aMsg: string);
begin
  FInfo := aMsg;
  Synchronize(@DorealInfo);
end;

procedure TStarterThread.DoSynchronize(AMethod: TThreadMethod);
begin
  {.$ifdef WINDOWS}
  Synchronize(AMethod);
  {.$else}
  //AMethod;
  {.$ENDIF}
end;

constructor TStarterThread.Create(aSuspended: Boolean);
begin
  if not BaseApplication.HasOption('disablethreads') then
    begin
      {$ifndef UNIX}
      FreeOnTerminate:=True;
      Priority:=tpLowest;
      inherited Create(aSuspended);
      {$else}
      Execute;
      {$endif}
    end
  else
    Execute;
end;

procedure TStarterThread.Execute;
var
  aTime: QWord;
  aDataSet: TBaseDbDataSet;
  aCal: TCalendar;
  aDS: TMeetings;
begin
  DoInfo('start');
  aConn := nil;
  DoSynchronize(@NewMenu);
  miNew.Action := fMainTreeFrame.acSearch;
  DoInfo('Startuptype');
  DoSynchronize(@DoStartupType);
  DoInfo('Wiki');
  DoSynchronize(@AddWiki);
  DoInfo('Objects,Tree,...');
  //All Objects
  fMain.pcPages.AddTabClass(TfFilter,strObjectList,@fMain.AddElementList,Data.GetLinkIcon('ALLOBJECTS@'),True);
  //Documents
  DoInfo('Documents');
  DataSetType:=TDocuments;
  Data.RegisterLinkHandler('ALLOBJECTS',@fMainTreeFrame.OpenLink,TObjects);
  Data.RegisterLinkHandler('SCRIPTS',@fMainTreeFrame.OpenLink,TBaseScript);
  //Messages
  DoInfo('Messages');
  if GetRight('MESSAGES') > RIGHT_NONE then
    begin
      try
        DataSetType:=TMessageList;
        fMain.pcPages.AddTabClass(TfMessageFrame,strMessages,nil,Data.GetLinkIcon('MESSAGEIDX@'),True);
        Data.RegisterLinkHandler('MESSAGES',@fMainTreeFrame.OpenLink,uMessages.TMessage);
        AddSearchAbleDataSet(TMessageList);
      except
      end;
    end;
  DoSynchronize(@StartReceive);
  //Tasks
  DoInfo('Tasks');
  if (GetRight('TASKS') > RIGHT_NONE) then
    begin
      try
      Synchronize(@RefreshTasks);
      Data.RegisterLinkHandler('TASKS',@fMainTreeFrame.OpenLink,TTask,TTaskList);
      except
      end;
    end;
  //Add PIM Entrys
  DoInfo('PIM');
  if GetRight('CALENDAR') > RIGHT_NONE then
    begin
      try
      DataSetType:=TCalendar;
      fMain.pcPages.AddTabClass(TfCalendarFrame,strCalendar,@fMain.AddCalendar,Data.GetLinkIcon('CALENDAR@'),True);
      DoSynchronize(@DoCalendar);
      except
      end;
    end;
  //Orders
  DoInfo('Orders');
  if GetRight('ORDERS') > RIGHT_NONE then
    begin
      try
      DataSetType:=TOrder;
      fMain.pcPages.AddTabClass(TfFilter,strOrderList,@fMain.AddOrderList,Data.GetLinkIcon('ORDERS@'),True);
      Data.RegisterLinkHandler('ORDERS',@fMainTreeFrame.OpenLink,TOrder);
      AddSearchAbleDataSet(TOrderList);
      except
      end;
    end;
  //Production
  DoInfo('Production');
  if GetRight('PRODUCTION') > RIGHT_NONE then
    begin
      try
      DataSetType:=TOrder;
      fMain.pcPages.AddTabClass(TfFilter,strProductionOrders,@fMain.AddOrderList,Data.GetLinkIcon('ORDERS@'),True);
      Data.RegisterLinkHandler('ORDERS',@fMainTreeFrame.OpenLink,TOrder);
      AddSearchAbleDataSet(TOrderList);
      except
      end;
    end;
  //Add Contacts
  DoInfo('Contacts');
  if GetRight('CUSTOMERS') > RIGHT_NONE then
    begin
      try
      DataSetType:=TPerson;
      DataSetType:=TCountries;
      fMain.pcPages.AddTabClass(TfFilter,strCustomerList,@fMain.AddCustomerList,Data.GetLinkIcon('CUSTOMERS@'),True);
      Data.RegisterLinkHandler('CUSTOMERS',@fMainTreeFrame.OpenLink,TPerson);
      AddSearchAbleDataSet(TPersonList);
      AddSearchAbleDataSet(TPersonContactData);
      AddSearchAbleDataSet(TPersonAddress);
      except
      end;
    end;
  //Add Masterdata stuff
  DoInfo('Masterdata');
  if (GetRight('MASTERDATA') > RIGHT_NONE) then
    begin
      try
      DataSetType:=TMasterdata;
      fMain.pcPages.AddTabClass(TfFilter,strArticleList,@fMain.AddMasterdataList,Data.GetLinkIcon('MASTERDATA@'),True);
      Data.RegisterLinkHandler('MASTERDATA',@fMainTreeFrame.OpenLink,TMasterdata);
      AddSearchAbleDataSet(TMasterdataList);
      except
      end;
    end;
  //Projects
  DoInfo('Projects');
  if (GetRight('PROJECTS') > RIGHT_NONE) then
    begin
      try
      DataSetType:=TProject;
      fMain.pcPages.AddTabClass(TfFilter,strProjectList,@fMain.AddProjectList,Data.GetLinkIcon('PROJECTS@'),True);
      Data.RegisterLinkHandler('PROJECT',@fMainTreeFrame.OpenLink,TProject);
      AddSearchAbleDataSet(TProjectList);
      except
      end;
    end;
  //Wiki
  DoInfo('Wiki');
  Data.RegisterLinkHandler('WIKI',@fMainTreeFrame.OpenLink,TWikiList);
  if (GetRight('WIKI') > RIGHT_NONE) then
    begin
      try
      fMain.pcPages.AddTabClass(TfWikiFrame,strWiki,@fMain.AddWiki,Data.GetLinkIcon('WIKI@'),True);
      AddSearchAbleDataSet(TWikiList);
      except
      end;
    end;
  //Documents
  DoInfo('Documents');
  if (GetRight('DOCUMENTS') > RIGHT_NONE) then
    begin
      try
      Data.RegisterLinkHandler('DOCUMENTS',@fMainTreeFrame.OpenLink,TDocument);
      Data.RegisterLinkHandler('DOCPAGES',@fMainTreeFrame.OpenLink,TDocPages);
      DataSetType:=TDocPages;
      except
      end;
      AddSearchAbleDataSet(TBaseScript);
    end;
  //Lists
  DoInfo('Lists');
  if (GetRight('LISTS') > RIGHT_NONE) then
    begin
      try
      DataSetType:=TLists;
      Data.RegisterLinkHandler('LISTS',@fMainTreeFrame.OpenLink,TLists);
      AddSearchAbleDataSet(TLists);
      except
      end;
    end;
  //Meetings
  DoInfo('Meetings');
  if (GetRight('MEETINGS') > RIGHT_NONE) then
    begin
      try
      DataSetType:=TMeetings;
      fMain.pcPages.AddTabClass(TfFilter,strMeetingList,@fMain.AddMeetingList,-1,True);
      Data.RegisterLinkHandler('MEETINGS',@fMainTreeFrame.OpenLink,TMeetings);
      AddSearchAbleDataSet(TMeetings);
      except
      end;
    end;
  //Inventory
  DoInfo('Inventory');
  if (GetRight('INVENTORY') > RIGHT_NONE) then
    begin
      try
      DataSetType:=TInventorys;
      Data.RegisterLinkHandler('INVENTORY',@fMainTreeFrame.OpenLink,TInventorys);
      except
      end;
    end;
  //Statistics
  DoInfo('Statistics');
  Data.RegisterLinkHandler('STATISTICS',@fMainTreeFrame.OpenLink,TStatistic);
  if (GetRight('STATISTICS') > RIGHT_NONE) then
    begin
      try
      fMain.pcPages.AddTabClass(TfFilter,strStatisticList,@fMain.AddStatisticList,Data.GetLinkIcon('STATISTICS@'),True);
      AddSearchAbleDataSet(TStatistic);
      except
      end;
    end;
  //Scheme
  DoInfo('Scheme');
  if (GetRight('SCHEME') > RIGHT_NONE) then
    begin
      try
      Data.RegisterLinkHandler('SCHEME',@fMainTreeFrame.OpenLink,TSchemeList);
      fMain.pcPages.AddTabClass(TfFilter,strSchemeList,@fMain.AddSchemeList,Data.GetLinkIcon('SCHEME@'),True);
      AddSearchAbleDataSet(TSchemeList);
      except
      end;
    end;
  //Timeregistering
  DoInfo('Timeregistering');
  DoSynchronize(@AddTimeReg);
  AddSearchAbleDataSet(TUser);
  Data.RegisterLinkHandler('USERS',@fMainTreeFrame.OpenLink,TUser);
  //History
  DoInfo('History');
  if GetRight('DOCUMENTS') > RIGHT_NONE then
    begin
      try
      AddSearchAbleDataSet(TBaseHistory);
      Data.RegisterLinkHandler('HISTORY',@fMainTreeFrame.OpenLink,TBaseHistory);
      except
      end;
    end;
  DoInfo('Phonelines');
  DoSynchronize(@RegisterPhoneLines);
  //aConn.Free;
  DoInfo('Search');
  DoSynchronize(@AddSearch);
  //Expand Tree
  DoInfo('ExpandTree');
  DoSynchronize(@Expand);
  DoInfo('Timeregistering');
  DoSynchronize(@AddTimeReg2);
end;

destructor TStarterThread.Destroy;
begin
  if Application.HasOption('hidetree') then
    begin
      fMain.acShowTree.Checked:=False;
      fMain.acShowTreeExecute(nil);
    end
  else
    begin
      with Application as IBaseDbInterface do
        fMain.acShowTree.Checked := DBConfig.ReadBoolean('SHOWTREE',True);
      //fMain.acShowTreeExecute(nil);
    end;
  DoInfo('StarterThread:end');
  inherited Destroy;
end;

procedure TfMain.acLoginExecute(Sender: TObject);
var
  Node: TTreeNode;
  miNew: TMenuItem;
  aWiki: TWikiList;
  aDocuments: TDocument;
  bStart: TStarterThread;
  aItems: TStringList;
  aIt: String;
  SomethingFound: Boolean;
  Node1: TTreeNode;
  aStartPagetext: String;
  aUser: TUser;
  tmp: String;
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
    fSplash.AddText(strLogin);
    Application.ProcessMessages;
    with Application as IBaseApplication do
      while not Login do
        if Application.Terminated then exit;
    fSplash.AddText(strAdding+strLinks);
    with Application as IBaseDBInterface do
      begin
        acLogin.Enabled:=False;
        acLogout.Enabled:=True;
        with BaseApplication as IBaseDBInterface do
          Caption := MandantName+' - Promet-ERP';
        if Assigned(TBaseVisualApplication(Application).MessageHandler) then
          TBaseVisualApplication(Application).MessageHandler.RegisterCommandHandler(@CommandReceived);
        if Assigned(BaseApplication) then with BaseApplication as IBaseApplication do
          debug('BaseLogin: '+IntToStr(GetTickCount64-aTime));
        WikiFrame := TfWikiFrame.Create(nil);
        WikiFrame.Parent := fMain.tsStartpage;
        WikiFrame.Align := alClient;
        aItems := TStringList.Create;
        aItems.Delimiter:=';';
        //aItems.DelimitedText := DBConfig.ReadString('TREEENTRYS:'+ApplicationName,fMainTreeFrame.GetBigIconTexts);
        aItems.DelimitedText := fMainTreeFrame.GetBigIconTexts;
        //Actions
        Data.RegisterLinkHandler('ACTION',@OpenAction,nil);
        //Actions
        Data.RegisterLinkHandler('FILTER',@OpenFilter,nil);
        //Options
        Data.RegisterLinkHandler('OPTION',@OpenOption,TOptions);
        SomethingFound := False;
        while not SomethingFound do
          begin
            while aItems.Count>0 do
              with fMainTreeFrame do
              begin
                tmp := aItems[0];
                if tmp = GetEntryText(etSearch) then
                  begin
                    //Add Search Node
                    Node := fMainTreeFrame.tvMain.Items.AddChildObject(nil,'',TTreeEntry.Create);
                    Node.Height := 34;
                    TTreeEntry(Node.Data).Typ := etSearch;
                    SomethingFound:=True;
                    Node.Visible:=False;
                    FSearchNode := Node;
                  end
                else if tmp = GetEntryText(etFavourites) then
                  begin
                    NewNode;
                    Node.Height := 34;
                    TTreeEntry(Node.Data).Typ := etFavourites;
                    SomethingFound:=True;
                  end
                else if tmp = GetEntryText(etMessages) then
                  begin
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
                        acNewMessage.Visible:=True;
                      end;
                    SomethingFound:=True;
                  end
                else if tmp = GetEntryText(etTasks) then
                  begin
                    //Tasks
                    if (Data.Users.Rights.Right('TASKS') > RIGHT_NONE) then
                      begin
                        NewNode;
                        Node.Height := 34;
                        TTreeEntry(Node.Data).Typ := etTasks;
                        FTaskNode := Node;
                        acNewTask.Visible:=True;
                      end;
                    SomethingFound:=True;
                  end
                else if tmp = GetEntryText(etCalendar) then
                  begin
                    //PIM
                    if Data.Users.Rights.Right('CALENDAR') > RIGHT_NONE then
                      begin
                        NewMenu;
                        miNew.Action := fMain.acCalendar;
                        NewNode;
                        Node.Height := 34;
                        TTreeEntry(Node.Data).Typ := etCalendar;
                        FCalendarNode := Node;
                        acNewTermin.Visible:=True;
                      end;
                    SomethingFound:=True;
                  end
                else if tmp = GetEntryText(etOrders) then
                  begin
                    //Orders,Production,...
                    if Data.Users.Rights.Right('ORDERS') > RIGHT_NONE then
                      begin
                        NewMenu;
                        miNew.Action := fMain.acOrders;
                        NewNode;
                        Node.Height := 32;
                        TTreeEntry(Node.Data).Typ := etOrders;
                        acNewOrder.Visible:=True;
                      end;
                    SomethingFound:=True;
                  end
                else if tmp = GetEntryText(etProduction) then
                  begin
                    //Orders,Production,...
                    if Data.Users.Rights.Right('PRODUCTION') > RIGHT_NONE then
                      begin
                        NewMenu;
                        miNew.Action := fMain.acProduction;
                        NewNode;
                        Node.Height := 32;
                        TTreeEntry(Node.Data).Typ := etProduction;
                        acNewOrder.Visible:=True;
                      end;
                    SomethingFound:=True;
                  end
                else if tmp = GetEntryText(etCustomers) then
                  begin
                    //Contacts
                    if Data.Users.Rights.Right('CUSTOMERS') > RIGHT_NONE then
                      begin
                        NewMenu;
                        miNew.Action := fMain.acContact;
                        NewNode;
                        TTreeEntry(Node.Data).Typ := etCustomers;
                        Node.Height := 34;
                        acNewContact.Visible:=True;
                      end;
                    SomethingFound:=True;
                  end
                else if tmp = GetEntryText(etMasterdata) then
                  begin
                    //Add Masterdata stuff
                    if (Data.Users.Rights.Right('MASTERDATA') > RIGHT_NONE) then
                      begin
                        NewMenu;
                        miNew.Action := fMain.acMasterdata;
                        NewNode;
                        Node.Height := 32;
                        TTreeEntry(Node.Data).Typ := etMasterdata;
                        acNewMasterdata.Visible:=True;
                      end;
                    SomethingFound:=True;
                  end
                else if tmp = GetEntryText(etProjects) then
                  begin
                    //Projects
                    if (Data.Users.Rights.Right('PROJECTS') > RIGHT_NONE) then
                      begin
                        NewNode;
                        Node.Height := 32;
                        TTreeEntry(Node.Data).Typ := etProjects;
                        acNewProject.Visible:=True;
                      end;
                    SomethingFound:=True;
                  end
                else if tmp = GetEntryText(etWiki) then
                  begin
                    //Wiki
                    if (Data.Users.Rights.Right('WIKI') > RIGHT_NONE) then
                      begin
                        NewNode;
                        Node.Height := 34;
                        TTreeEntry(Node.Data).Typ := etWiki;
                      end;
                    SomethingFound:=True;
                  end
                else if tmp = GetEntryText(etDocuments) then
                  begin
                    //Documents
                    if (Data.Users.Rights.Right('DOCUMENTS') > RIGHT_NONE) then
                      begin
                        Node := fMainTreeFrame.tvMain.Items.AddChildObject(nil,'',TTreeEntry.Create);
                        Node.Height := 34;
                        TTreeEntry(Node.Data).Typ := etDocuments;
                        Node1 := fMainTreeFrame.tvMain.Items.AddChildObject(Node,'',TTreeEntry.Create);
                        TTreeEntry(Node1.Data).Typ := etFiles;
                        Node := fMainTreeFrame.tvMain.Items.AddChildObject(nil,'',TTreeEntry.Create);
                        TTreeEntry(Node.Data).Typ := etImages;
                        Node.Height := 34;
                      end;
                    SomethingFound:=True;
                  end
                else if tmp = GetEntryText(etLists) then
                  begin
                    //Lists
                    if (Data.Users.Rights.Right('LISTS') > RIGHT_NONE) then
                      begin
                        NewNode;
                        TTreeEntry(Node.Data).Typ := etLists;
                      end;
                    SomethingFound:=True;
                  end
                else if tmp = GetEntryText(etMeetings) then
                  begin
                    //Meetings
                    if (Data.Users.Rights.Right('MEETINGS') > RIGHT_NONE) then
                      begin
                        NewNode;
                        TTreeEntry(Node.Data).Typ := etMeetings;
                      end;
                    SomethingFound:=True;
                  end
                else if tmp = GetEntryText(etInventory) then
                  begin
                    //Inventory
                    if (Data.Users.Rights.Right('INVENTORY') > RIGHT_NONE) then
                      begin
                        NewNode;
                        TTreeEntry(Node.Data).Typ := etInventory;
                      end;
                    SomethingFound:=True;
                  end
                else if tmp = GetEntryText(etFinancial) then
                  begin
                    //Financial
                    if (Data.Users.Rights.Right('BANKACCNTS') > RIGHT_NONE)
                    or (Data.Users.Rights.Right('SALESLIST') > RIGHT_NONE) then
                      begin
                        NewNode;
                        Node.Height := 34;
                        TTreeEntry(Node.Data).Typ := etFinancial;
                      end;
                    SomethingFound:=True;
                  end
                else if tmp = GetEntryText(etStatistics) then
                  begin
                    //Statistics
                    if (Data.Users.Rights.Right('STATISTICS') > RIGHT_NONE) then
                      begin
                        NewMenu;
                        miNew.Action := fMain.acStatistics;
                        NewNode;
                        Node.Height := 34;
                        TTreeEntry(Node.Data).Typ := etStatistics;
                      end;
                    SomethingFound:=True;
                  end
                else if tmp = GetEntryText(etAllObjects) then
                  begin
                    NewMenu;
                    miNew.Action := fMain.acElements;
                    NewNode;
                    TTreeEntry(Node.Data).Typ := etAllObjects;
                    SomethingFound:=True;
                  end;
                aItems.Delete(0);
              end;
            if not SomethingFound then
              aItems.DelimitedText := fMainTreeFrame.GetBigIconTexts;
          end;
        aItems.free;
        fSplash.AddText(strRefresh);
        fMain.acShowTree.Checked:=True;
        fMain.acShowTreeExecute(nil);
        fSplash.Hide;
        fMain.Show;
        Application.ProcessMessages;
        {$ifdef LINUX}
        pCloseTab.BorderSpacing.Top:=3;
        pSeparateWindow.BorderSpacing.Top:=3;
        {$endif}
        bStart := TStarterThread.Create(False);

        with Application as IBaseDbInterface do
          FHistory.Text := DBConfig.ReadString('HISTORY','');
      end;
  finally
  end;
  TBaseVisualApplication(BaseApplication).LoginDone;
  if Assigned(BaseApplication) then with BaseApplication as IBaseApplication do
    debug('LoginTime: '+IntToStr(GetTickCount64-aTime));
  fSplash.Hide;
  fMain.Show;
  IPCTimer.Enabled:=True;
end;
procedure TfMain.acContactExecute(Sender: TObject);
var
  i: Integer;
  Found: Boolean = false;
  aFrame: TfFilter;
begin
  if Data.Users.Rights.Right('CUSTOMERS') < RIGHT_READ then exit;
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
  i: Integer;
  Found: Boolean = False;
begin
  Application.ProcessMessages;
  for i := 0 to pcPages.PageCount-2 do
    if (pcPages.Pages[i].ControlCount > 0) and (pcPages.Pages[i].Controls[0] is TfAttPlan) then
      begin
        pcPages.PageIndex:=i;
        Found := True;
      end;
  if not Found then
    begin
      aFrame := TfAttPlan.Create(Self);
      aFrame.TabCaption := strAttPlan;
      pcPages.AddTab(aFrame,True,'',Data.GetLinkIcon('TASKS@'),False);
      aFrame.Populate(Data.Users.FieldByName('PARENT').AsVariant,Data.Users.Id.AsVariant);
    end;
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
  and (pcPages.PageCount > 2) and (pcPages.ActivePage.PageIndex>0) then
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
  aAccounting := TAccountingJournal.CreateEx(Self,Data);
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

procedure TfMain.acCommandlineExecute(Sender: TObject);
var
  i: Integer;
  Found: Boolean = false;
  aFrame: TfCommandline;
begin
  Application.ProcessMessages;
  for i := 0 to pcPages.PageCount-2 do
    if (pcPages.Pages[i].ControlCount > 0) and (pcPages.Pages[i].Controls[0] is TfCommandLine) then
      begin
        pcPages.PageIndex:=i;
        Found := True;
      end;
  if not Found then
    begin
      aFrame := TfCommandline.Create(Self);
      pcPages.AddTab(aFrame,True,'',115,False);
      AddCommandline(aFrame);
    end;
end;

procedure TfMain.acDeleteWholeMessageDirExecute(Sender: TObject);
var
  ID: String;
  nData : TTreeEntry;
  aMessages: TMessageList;
  aMessage: uMessages.TMessage;
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
      ID := IntToStr(TREE_ID_DELETED_MESSAGES);
      aMessages := TMessageList.CreateEx(Self,Data);
      Data.SetFilter(aMessages,Data.QuoteField('TREEENTRY')+'='+ID);
      Data.DeletedItems.DataSet.Open;
      a := aMessages.Count;
      aTime := GetTickCount;
      fWaitForm.ShowInfo(Format(strDeletingMessages,[a]));
      fWaitForm.Show;
      aTime := GetTickCount;
      while not aMessages.DataSet.EOF do
        begin
          aMessages.Delete
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

procedure TfMain.acElementsExecute(Sender: TObject);
var
  i: Integer;
  Found: Boolean;
  aFrame: TfFilter;
begin
  Application.ProcessMessages;
  for i := 0 to pcPages.PageCount-2 do
    if (pcPages.Pages[i].ControlCount > 0) and (pcPages.Pages[i].Controls[0] is TfFilter) and (TfFilter(pcPages.Pages[i].Controls[0]).Dataset is TObjects) then
      begin
        pcPages.PageIndex:=i;
        Found := True;
      end;
  if not Found then
    begin
      aFrame := TfFilter.Create(Self);
      pcPages.AddTab(aFrame,True,'',Data.GetLinkIcon('ALLOBJECTS@'),False);
      AddElementList(aFrame);
      aFrame.Open;
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
     fInfo.Copyright:='2006-2014 C. Ulrich';
    end;
  fInfo.SetLanguage;
  fInfo.Execute;
end;
procedure TfMain.acLogoutExecute(Sender: TObject);
begin
  FreeAndNil(fOptions);
  if Assigned(fHelpContainer) then
    FreeAndNil(fHelpContainer);
  if Assigned(tsStartpage) and (tsStartpage.ControlCount > 0) then
    tsStartpage.Controls[0].Destroy;
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
  if Data.Users.Rights.Right('MASTERDATA') < RIGHT_READ then exit;
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

procedure TfMain.acMasterdataVersionateExecute(Sender: TObject);
var
  aMasterdata: TMasterdataList;
  gList: TExtDBGrid;
  i: Integer;

  procedure Versionate;
  var
    bMasterdata: TMasterdata;
  begin
    bMasterdata := TMasterdata.Create(nil);
    bMasterdata.Select(aMasterdata.Id.AsVariant);
    bMasterdata.Open;
    if not bMasterdata.Versionate(fVersionate.eVersion.Text,fVersionate.cbActivate.Checked) then ShowMessage(strError);
    bMasterdata.Free;
  end;

begin
  if Assigned(pcPages.ActivePage) and (pcPages.ActivePage.ControlCount > 0) and (pcPages.ActivePage.Controls[0] is TfFilter) and (TfFilter(pcPages.ActivePage.Controls[0]).DataSet is TMasterdataList) then
    begin
      if fVersionate.Execute then
        begin
          aMasterdata := TfFilter(pcPages.ActivePage.Controls[0]).DataSet as TMasterdataList;
          gList := TfFilter(pcPages.ActivePage.Controls[0]).gList;
          if gList.SelectedRows.Count > 0 then
            begin
              for i := 0 to gList.SelectedRows.Count-1 do
                begin
                  gList.DataSource.DataSet.GotoBookmark(Pointer(gList.SelectedRows.Items[i]));
                  Versionate;
                end;
              gList.SelectedRows.Clear;
            end
          else
            with Application as IBaseDbInterface do
              Versionate;
        end;
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
  if Data.Users.Rights.Right('MESSAGES') < RIGHT_READ then exit;
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
  FreeAndNil(fOptions);
end;

procedure TfMain.acNewContactExecute(Sender: TObject);
var
  aFrame: TfPersonFrame;
begin
  if Data.Users.Rights.Right('CUSTOMERS') < RIGHT_WRITE then exit;
  Application.ProcessMessages;
  aFrame := TfPersonFrame.Create(Self);
  pcPages.AddTab(aFrame);
  aFrame.SetLanguage;
  aFrame.New;
end;
procedure TfMain.acNewInventoryExecute(Sender: TObject);
var
  aDS : TInventorys;
begin
  if Assigned(pcPages.ActivePage) and (pcPages.ActivePage.ControlCount > 0) and (pcPages.ActivePage.Controls[0] is TfFilter) then
    begin
      aDS := TInventorys(TfFilter(pcPages.ActivePage.Controls[0]).DataSet);
    end
  else exit;
  aDS.Append;
  with aDS do
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
  aList := TLists.CreateEx(Self,Data);
  aList.Append;
  aList.FieldByName('NAME').AsString:=aName;
  aList.DataSet.Post;

end;
procedure TfMain.acNewMasterdataExecute(Sender: TObject);
var
  aFrame: TfArticleFrame;
begin
  if Data.Users.Rights.Right('MASTERDATA') < RIGHT_WRITE then exit;
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
  if Data.Users.Rights.Right('MEETINGS') < RIGHT_WRITE then exit;
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
  if Data.Users.Rights.Right('MESSAGES') < RIGHT_WRITE then exit;
  fMessageEdit := TfMessageEdit.Create(nil);
  fMessageEdit.SendMailTo('');
end;

procedure TfMain.acNewObjectExecute(Sender: TObject);
var
  aFrame: TfObjectFrame;
begin
  Application.ProcessMessages;
  aFrame := TfObjectFrame.Create(Self);
  pcPages.AddTab(aFrame);
  aFrame.SetLanguage;
  aFrame.New;
end;

procedure TfMain.acNewOrderExecute(Sender: TObject);
var
  aFrame: TfOrderFrame;
begin
  if Data.Users.Rights.Right('ORDERS') < RIGHT_WRITE then exit;
  Application.ProcessMessages;
  aFrame := TfOrderFrame.Create(Self);
  pcPages.AddTab(aFrame);
  aFrame.SetLanguage;
  aFrame.New;
end;

procedure TfMain.acNewProductionOrderExecute(Sender: TObject);
var
  aFrame: TfOrderFrame;
  aOrderT: TOrderTyp;
begin
  if Data.Users.Rights.Right('PRODUCTION') < RIGHT_WRITE then exit;
  Application.ProcessMessages;
  aFrame := TfOrderFrame.Create(Self);
  aOrderT := TOrderTyp.Create(nil);
  pcPages.AddTab(aFrame);
  aFrame.SetLanguage;
  aOrderT.Open;
  aOrderT.Filter(Data.QuoteField('TYPE')+'='+Data.QuoteValue('7'));
  aFrame.New(aOrderT.FieldByName('STATUS').AsString);
  aOrderT.Free;
end;

procedure TfMain.acNewProjectExecute(Sender: TObject);
var
  aFrame: TfProjectFrame;
begin
  if Data.Users.Rights.Right('PROJECTS') < RIGHT_WRITE then exit;
  Application.ProcessMessages;
  aFrame := TfProjectFrame.Create(Self);
  pcPages.AddTab(aFrame);
  aFrame.SetLanguage;
  aFrame.OnStartTime:=@SenderTfMainTaskFrameControlsSenderTfMainTaskFrameTfTaskFrameStartTime;
  aFrame.New;
end;

procedure TfMain.acNewSchemeExecute(Sender: TObject);
var
  aFrame: TfShemeFrame;
begin
  Application.ProcessMessages;
  aFrame := TfShemeFrame.Create(Self);
  pcPages.AddTab(aFrame);
  aFrame.SetLanguage;
  aFrame.New;
end;

procedure TfMain.acNewScriptExecute(Sender: TObject);
var
  aFrame: TfScriptFrame;
begin
  Application.ProcessMessages;
  aFrame := TfScriptFrame.Create(Self);
  pcPages.AddTab(aFrame);
  aFrame.SetLanguage;
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

procedure TfMain.acNewTaskExecute(Sender: TObject);
var
  aTask: TTask;
  aEditor: TfTaskEdit;
begin
  if Data.Users.Rights.Right('TASKS') < RIGHT_WRITE then exit;
  Application.ProcessMessages;
  aTask := TTask.Create(nil);
  aTask.Insert;
  aTask.FieldByName('USER').AsString := Data.Users.FieldByName('ACCOUNTNO').AsString;
  aTask.Post;
  aEditor := TfTaskEdit.Create(Self);
  aEditor.Execute(Data.BuildLink(aTask.DataSet));
  aEditor.Free;
  aTask.Free;
end;

procedure TfMain.acNewTerminExecute(Sender: TObject);
var
  i: Integer;
  Found: Boolean = false;
  aFrame: TfCalendarFrame;
begin
  if Data.Users.Rights.Right('CALENDAR') < RIGHT_WRITE then exit;
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
  if tvSearchP.Visible then
    begin
      if Assigned(tvSearch.Selected) and Assigned(tvSearch.Selected.Data) then
        data.GotoLink(TTreeEntry(tvSearch.Selected.Data).Link);
    end;
end;

procedure TfMain.acOrdersExecute(Sender: TObject);
var
  i: Integer;
  Found: Boolean = false;
  aFrame: TfFilter;
begin
  if Data.Users.Rights.Right('ORDERS') < RIGHT_READ then exit;
  Application.ProcessMessages;
  for i := 0 to pcPages.PageCount-2 do
    if (pcPages.Pages[i].ControlCount > 0) and (pcPages.Pages[i].Controls[0] is TfFilter) and (TfFilter(pcPages.Pages[i].Controls[0]).Dataset is TOrderList) and (TfFilter(pcPages.Pages[i].Controls[0]).Tag=0) then
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

procedure TfMain.acPasswordsExecute(Sender: TObject);
begin
  fPasswords.Execute;
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

procedure TfMain.acProductionExecute(Sender: TObject);
var
  i: Integer;
  Found: Boolean = false;
  aFrame: TfFilter;
begin
  Application.ProcessMessages;
  for i := 0 to pcPages.PageCount-2 do
    if (pcPages.Pages[i].ControlCount > 0) and (pcPages.Pages[i].Controls[0] is TfFilter) and (TfFilter(pcPages.Pages[i].Controls[0]).Dataset is TOrderList) and (TfFilter(pcPages.Pages[i].Controls[0]).Tag=1) then
      begin
        pcPages.PageIndex:=i;
        Found := True;
      end;
  if not Found then
    begin
      aFrame := TfFilter.Create(Self);
      pcPages.AddTab(aFrame,True,'',Data.GetLinkIcon('ORDERS@'),False);
      AddProductionOrderList(aFrame);
      aFrame.Open;
    end;
end;

procedure TfMain.acProjectOverviewExecute(Sender: TObject);
var
  RoughFrame: TfProjectOVFrame;
begin
  RoughFrame := TfProjectOVFrame.Create(Self);
  pcPages.AddTab(RoughFrame);
  RoughFrame.StartFilling;
end;

procedure TfMain.acProjectsExecute(Sender: TObject);
var
  i: Integer;
  Found: Boolean = False;
  aFrame: TfFilter;
begin
  if Data.Users.Rights.Right('PROJECTS') < RIGHT_READ then exit;
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

procedure TfMain.acRoughPlanningExecute(Sender: TObject);
var
  RoughFrame: TfRoughPlanningFrame;
begin
  RoughFrame := TfRoughPlanningFrame.Create(Self);
  pcPages.AddTab(RoughFrame);
  RoughFrame.StartFilling;
end;

procedure TfMain.acSalesListBookExecute(Sender: TObject);
var
  aAccountingjournal: TAccountingJournal;
begin
  if fBookFibuAccount.Execute then
    begin
      if Assigned(pcPages.ActivePage) and (pcPages.ActivePage.ControlCount > 0) and (pcPages.ActivePage.Controls[0] is TfFilter) and (TfFilter(pcPages.ActivePage.Controls[0]).DataSet is TAccountingJournal) then
        begin
          fBookAccounting.SetLanguage;
          aAccountingjournal := TfFilter(pcPages.ActivePage.Controls[0]).DataSet as TAccountingjournal;
          if fBookFibuAccount.cbWholeList.Checked then
            begin
              aAccountingjournal.First;
              while not aAccountingjournal.EOF do
                begin
                  aAccountingjournal.Edit;
                  aAccountingjournal.FieldByName('ACCOUNT').AsString:=fBookFibuAccount.DataSet.FieldByName('ACCOUNTNO').AsString;
                  aAccountingjournal.Post;
                  aAccountingjournal.Next;
                end;
            end
          else
            begin
              aAccountingjournal.Edit;
              aAccountingjournal.FieldByName('ACCOUNT').AsString:=fBookFibuAccount.DataSet.FieldByName('ACCOUNTNO').AsString;
              aAccountingjournal.Post;
            end;
        end;
    end;
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

procedure TfMain.acSearchOptionsExecute(Sender: TObject);
var
  i: Integer;
begin
  pSearchOptions.Visible:=acSearchOptions.Checked;
  if pSearchOptions.Visible then
    begin
      fSearch.LoadOptions('MAIS');
      cbSearchType.Items.Assign(fSearch.cbSearchType.Items);
      for i := 0 to fSearch.cbSearchType.Items.Count-1 do
        cbSearchType.Checked[i] := fSearch.cbSearchType.Checked[i];
      cbSearchIn.Items.Assign(fSearch.cbSearchIn.Items);
      for i := 0 to fSearch.cbSearchIn.Items.Count-1 do
        cbSearchIn.Checked[i] := fSearch.cbSearchIn.Checked[i];
    end;
end;

procedure TfMain.acShowSearchExecute(Sender: TObject);
begin

end;

procedure TfMain.acShowTreeExecute(Sender: TObject);
begin
  BeginFormUpdate;
  BeginUpdateBounds;
  if acShowTree.Checked or acShowSearch.Checked then
    begin
      tvMainAll.Visible:=True;
      spTree.Visible:=True;
    end
  else
    begin
      tvMainAll.Visible:=False;
      spTree.Visible:=false;
    end;
  if acShowTree.Checked and acShowSearch.Checked then
    begin
      if Sender=acShowSearch then
        acShowTree.Checked:=False
      else
        acShowSearch.Checked:=False;
    end;
  with Application as IBaseDbInterface do
    DBConfig.WriteBoolean('SHOWTREE',acShowTree.Checked or acShowSearch.Checked);
  if tvMainAll.Visible then
    begin
      ppages.Anchors := [akTop,akLeft,akRight,akBottom];
      pPages.Align:=alnone;
      pPages.Width:=fMain.Width-tvMainAll.Width;
      pPages.Height:=fmain.Height;
    end
  else pPages.Align:=alClient;
  tvMain.Visible:=acShowTree.Checked;
  tvSearchP.Visible:=acShowSearch.Checked;
  if tvMain.Visible then
    begin
      pSearchOptions.Visible:=False;
      acSearchOptions.Checked:=False;
    end;
  if tvSearchP.Visible then
    begin
      tvSearchP.SetFocus;
      eSearch.SelectAll;
      eSearch.SetFocus;
    end;
  FormResize(Self);
  EndUpdateBounds;
  EndFormUpdate;
end;

procedure TfMain.acStandartTimeExecute(Sender: TObject);
begin
  if Assigned(FTimeReg) then
    begin
      FTimeReg.acStartstandartEntry.Execute;
    end;
end;

procedure TfMain.acStartPageExecute(Sender: TObject);
begin
  pcPages.TabIndex:=0;
end;

procedure TfMain.acStatisticsExecute(Sender: TObject);
var
  i: Integer;
  Found: Boolean = false;
  aFrame: TfFilter;
begin
  Application.ProcessMessages;
  for i := 0 to pcPages.PageCount-2 do
    if (pcPages.Pages[i].ControlCount > 0) and (pcPages.Pages[i].Controls[0] is TfFilter) and (TfFilter(pcPages.Pages[i].Controls[0]).Dataset is TStatistic) then
      begin
        pcPages.PageIndex:=i;
        Found := True;
      end;
  if not Found then
    begin
      aFrame := TfFilter.Create(Self);
      pcPages.AddTab(aFrame,True,'',Data.GetLinkIcon('STATISTICS@'),False);
      AddStatisticList(aFrame);
      aFrame.Open;
    end;
end;

procedure TfMain.acTaskPlanExecute(Sender: TObject);
var
  aFrame: TfTaskPlan;
begin
  aFrame := TfTaskPlan.Create(Self);
  aFrame.TabCaption := strTaskPlan;
  pcPages.AddTab(aFrame,True,'',Data.GetLinkIcon('TASKS@'),False);
  if Data.Users.Rights.Right('RESOURCEVIEW')>RIGHT_READ then
    begin
      aFrame.Populate(Null,Null)
    end
  else
    begin
      if Data.Users.FieldByName('POSITION').AsString = 'LEADER' then
        aFrame.Populate(Data.Users.FieldByName('PARENT').AsVariant,Null)
      else
        aFrame.Populate(Data.Users.FieldByName('PARENT').AsVariant,Data.Users.Id.AsVariant);
    end;
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

procedure TfMain.acTimeRegistrationExecute(Sender: TObject);
var
  RoughFrame: TfTimeRegistration;
begin
  RoughFrame := TfTimeRegistration.Create(Self);
  pcPages.AddTab(RoughFrame);
end;

procedure TfMain.acWikiExecute(Sender: TObject);
var
  i: Integer;
  Found: Boolean = false;
  aFrame: TfWikiFrame;
begin
  Application.ProcessMessages;
  for i := 0 to pcPages.PageCount-2 do
    if (pcPages.Pages[i].ControlCount > 0) and (pcPages.Pages[i].Controls[0] is TfWikiFrame) and (pcPages.Pages[i] <> tsStartpage) then
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

procedure TfMain.acWindowizeExecute(Sender: TObject);
begin
  if Assigned(pcPages.ActivePage) and (pcPages.ActivePage.ControlCount > 0) and (pcPages.ActivePage.Controls[0] is TPrometMainFrame)
  and (pcPages.PageCount > 2) then
    TPrometMainFrame(pcPages.ActivePage.Controls[0]).Windowize;
end;

procedure TfMain.aFrameInvDragDrop(Sender, Source: TObject; X, Y: Integer);
var
  aLink: String;
  nData: TTreeEntry;
  aDS: TBaseDBDataset;

  procedure AddFromLink(bLink : string);
  var
    aMd: TMasterdata;
  begin
    aMd := TMasterdata.Create(nil);
    aMd.SelectFromLink(bLink);
    aMd.Open;
    if aMd.Count>0 then
      begin
        with TExtDBGrid(Sender).DataSource.DataSet do
          begin
            aMd.Storage.Open;
            while not aMd.Storage.EOF do
              begin
                Insert;
                FieldByName('IDENT').AsString:=aMd.FieldByName('ID').AsString;
                FieldByName('VERSION').AsString:=aMd.FieldByName('VERSION').AsString;
                FieldByName('LANGUAGE').AsString:=aMd.FieldByName('LANGUAGE').AsString;
                FieldByName('SHORTTEXT').AsString:=aMd.FieldByName('SHORTTEXT').AsString;
                FieldByName('POSNO').AsInteger:=RecordCount+1;
                FieldByName('STORAGE').AsString:=aMd.Storage.FieldByName('STORAGEID').AsString;
                FieldByName('QUANTITYU').AsString:=aMd.FieldByName('QUANTITYU').AsString;
                Post;
                aMd.Storage.Next;
              end;
          end;
      end;
    aMd.Free;
  end;
begin
  if Assigned(fSearch) and (Source = fSearch.sgResults) then
    begin
      aLink := fSearch.GetLink;
      with TExtDBGrid(Sender).DataSource.DataSet do
        begin
          AddFromLink(aLink);
        end;
    end
  else
    begin
      if Source = uMainTreeFrame.fMainTreeFrame.tvMain then
        begin
          nData := TTreeEntry(uMainTreeFrame.fMainTreeFrame.tvMain.Selected.Data);
          aDS := nData.DataSourceType.CreateEx(Self,Data);
          Data.SetFilter(aDS,nData.Filter);
          Data.GotoBookmark(aDS,nData.Rec);
          aLink := Data.BuildLink(aDS.DataSet);
          aDS.Free;
          with TExtDBGrid(Sender).DataSource.DataSet do
            begin
              AddFromLink(aLink);
            end;
        end
    end;
end;

procedure TfMain.aFrameInvDragOver(Sender, Source: TObject; X, Y: Integer;
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
  and ((TTreeEntry(uMainTreeFrame.fMainTreeFrame.tvMain.Selected.Data).Typ = etMasterdata)
  )
  then
    Accept := True;
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
      aIcon := Data.GetLinkIcon(aLink,True);
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
          aDS := nData.DataSourceType.CreateEx(Self,Data);
          Data.SetFilter(aDS,nData.Filter);
          Data.GotoBookmark(aDS,nData.Rec);
          aLink := Data.BuildLink(aDS.DataSet);
          aLinkDesc := Data.GetLinkDesc(aLink);
          aIcon := Data.GetLinkIcon(aLink,True);
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

procedure TfMain.cbSearchTypeClick(Sender: TObject);
var
  i: Integer;
begin
  fSearch.cbSearchType.Items.Assign(cbSearchType.Items);
  for i := 0 to fSearch.cbSearchType.Items.Count-1 do
    fSearch.cbSearchType.Checked[i] := cbSearchType.Checked[i];
  fSearch.cbSearchIn.Items.Assign(cbSearchIn.Items);
  for i := 0 to fSearch.cbSearchIn.Items.Count-1 do
    fSearch.cbSearchIn.Checked[i] := cbSearchIn.Checked[i];
  fSearch.SaveOptions;
  fSearch.SearchLevel:=0;
  fSearch.SetUpSearch;
  tvSearch.Items.Clear;
  fSearch.DoSearch(nil);
end;

procedure TfMain.eSearchChange(Sender: TObject);
begin
  SearchTimer.Enabled:=True;
end;

procedure TfMain.eSearchEnter(Sender: TObject);
begin
  if eSearch.Text=strSearchText then
    begin
      eSearch.Text:='';
      eSearch.Font.Color:=clWindowText;
    end;
  eSearch.SelectAll;
end;

procedure TfMain.fMainTreeFrameDragDrop(Sender, Source: TObject; X, Y: Integer);
var
  aLink: String;
  aFilter: TfFilter;
  aTNode: TTreeNode;
  aMessage: uMessages.TMessage;
  aTreeEntry: TTreeEntry;
  aTask: TTask;
  ls: TListItem;
  aPages: TDocPages;
  aLinks: String;
begin
  if not Assigned(Source) then exit;
  Screen.Cursor:=crHourGlass;
  if (Source is TListView) and (TListView(Source).Owner is TfDocumentFrame) then
    begin
      aTNode := fMainTreeFrame.tvMain.GetNodeAt(X,Y);
      if Assigned(aTNode) then
        begin
          aTreeEntry := TTreeEntry(aTNode.Data);
          if (aTreeEntry.Typ = etDocuments)
          or (aTreeEntry.Typ = etImages)
          then
            begin
              aPages := TDocPages.Create(nil);
              if (aTreeEntry.Typ = etDocuments) then
                aPages.Typ:='D'
              else
                aPages.Typ:='I';
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
      aLinks := TfFilter(TComponent(Source).Owner).GetLink(False);
      aLink := copy(aLinks,0,pos(';',aLinks)-1);
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
                  aMessage := uMessages.TMessage.CreateEx(Self,Data);
                  while pos(';',aLinks)>0 do
                    begin
                      aLink := copy(aLinks,0,pos(';',aLinks)-1);
                      aLinks := copy(aLinks,pos(';',aLinks)+1,length(aLinks));
                      aMessage.SelectFromLink(aLink);
                      aMessage.Open;
                      if aMessage.Count > 0 then
                        begin
                          aMessage.DataSet.Edit;
                          aMessage.FieldByName('TREEENTRY').AsVariant:=Data.Tree.Id.AsVariant;
                          aMessage.FieldByName('GRP_ID').Clear;
                          aMessage.DataSet.Post;
                          DoRefreshActiveTab(nil);
                        end;
                    end;
                  aMessage.Free;
                end
              else if (aTreeEntry.Typ = etTasks) then
                begin
                  aMessage := uMessages.TMessage.CreateEx(Self,Data);
                  aMessage.SelectFromLink(aLink);
                  aMessage.Open;
                  if aMessage.Count > 0 then
                    begin
                      aMessage.DataSet.Edit;
                      aMessage.FieldByName('READ').AsString := 'Y';
                      aMessage.DataSet.Post;
                      aTask := TTask.Create(nil);
                      aTask.Insert;
                      aTask.FieldByName('SUMMARY').AsString:=aMessage.Text.AsString;
                      aTask.FieldByName('USER').AsString:=Data.Users.FieldByName('ACCOUNTNO').AsString;
                      aTask.FieldByName('DESC').AsString:=aMessage.AsString;
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
            end
          else if (aTreeEntry.Typ = etDocuments) then
            begin
              if TfManageDocFrame(TComponent(Source).Owner).GotoCurrentItem then
                begin
                  if not TfManageDocFrame(TComponent(Source).Owner).DataSet.CanEdit then
                    TfManageDocFrame(TComponent(Source).Owner).DataSet.DataSet.Edit;
                  TfManageDocFrame(TComponent(Source).Owner).DataSet.FieldByName('TYPE').AsString:='D';
                  TfManageDocFrame(TComponent(Source).Owner).DataSet.Post;
                  TfManageDocFrame(TComponent(Source).Owner).acRefresh.Execute;
                end;
            end
          else if (aTreeEntry.Typ = etImages) then
            begin
              if TfManageDocFrame(TComponent(Source).Owner).GotoCurrentItem then
                begin
                  if not TfManageDocFrame(TComponent(Source).Owner).DataSet.CanEdit then
                    TfManageDocFrame(TComponent(Source).Owner).DataSet.DataSet.Edit;
                  TfManageDocFrame(TComponent(Source).Owner).DataSet.FieldByName('TYPE').AsString:='I';
                  TfManageDocFrame(TComponent(Source).Owner).DataSet.Post;
                  TfManageDocFrame(TComponent(Source).Owner).acRefresh.Execute;
                end;
            end;
        end;
    end;
  Screen.Cursor:=crDefault;
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
          if (aTreeEntry.Typ = etDocuments)
          or (aTreeEntry.Typ = etImages)
          then
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
          if (aTreeEntry.Typ = etDocuments)
          then
            Accept := True;
          if (aTreeEntry.Typ = etImages)
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
  aMFrame: TfMessageFrame;
  aNew: TMenuItem;
  tmp: Char;
  aDFrame: TfManageDocFrame;
  aAFrame: TfAccountingFrame;
  aIFrame: TfDocumentFrame;
begin
  if not Assigned(aEntry) then
    exit;
  Screen.Cursor:=crHourglass;
  Application.ProcessMessages;
  with Application as IBaseDbInterface do
    if DBConfig.ReadBoolean('HIDETREE',false) and acShowTree.Checked then
      acShowTree.Execute;
  case aEntry.Typ of
  etSalesList:
    begin
      aEntry.Action.Execute;
    end;
  etCustomerList,etCustomers,etArticleList,etOrderList,etProductionList,
  etTasks,etMyTasks,etProjects,
  etLink:
    begin
      fMainTreeFrame.OpenLink(aEntry.Link,Self);
    end;
  etCustomer,etEmployee,etSupplier,etArticle,etProject,etProcess,etStatistic:
    begin
      aDataSet := aEntry.DataSourceType.CreateEx(Self,Data);
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
  etMyCalendar:acCalendar.Execute;
  etCalendarUser,etCalendarDir:
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
      aDataSet := aEntry.DataSourceType.CreateEx(Self,Data);
      aDataSet.Open;
      aDataSet.GotoBookmark(aEntry.Rec);
      aFrame.New(aDataSet.FieldByName('STATUS').AsString);
      aDataSet.Destroy;
    end;
  etTaskPlan:
    begin
      acTaskPlan.Execute;
    end;
  etTimeRegistration:
    begin
      acTimeRegistration.Execute;
    end;
  etTimeregisteringSmall:
    begin
      acTimeRegistering.Execute;
    end;
  etAttPlan:
    begin
      acAttPlan.Execute;
    end;
  etWikiPage:
    begin
      aDataSet := aEntry.DataSourceType.CreateEx(Self,Data);
      with aDataSet.DataSet as IBaseDBFilter do
        Filter := aEntry.Filter;
      aDataSet.Open;
      if aDataSet.Count > 0 then
        fMainTreeFrame.OpenLink(Data.BuildLink(aDataSet.DataSet),Self);
      aDataSet.Free;
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
          aMFrame := TfmessageFrame.Create(Self);
          pcPages.AddTab(aMFrame);
          TfMessageFrame(aMFrame).FMessageNode := FMessageNode;
        end;
      if not Data.GotoBookmark(Data.Tree,aEntry.Rec) then
        begin
          Data.SetFilter(Data.Tree,'',0,'','ASC',False,True,True);
          if not Data.GotoBookmark(Data.Tree,aEntry.Rec) then
            exit;
        end;
      if Data.Tree.Id.AsInteger = TREE_ID_DELETED_MESSAGES then
        begin
          aNew := TMenuItem.Create(fMainTreeFrame.pmTree);
          aNew.Action := acDeleteWholeMessageDir;
          fMainTreeFrame.pmTree.Items.Add(aNew);
        end;
      if (pcPages.ActivePage.ControlCount > 0) and (pcPages.ActivePage.Controls[0] is TfMessageFrame) then
        TfMessageFrame(pcPages.ActivePage.Controls[0]).OpenDir(Data.Tree.Id.AsVariant);
    end;
  etDocumentDir:
    begin
      Application.ProcessMessages;
      tmp := 'I';
      if Assigned(pcPages.ActivePage) and (pcPages.ActivePage.ControlCount > 0) and (pcPages.ActivePage.Controls[0] is TfManageDocFrame) and (TfManageDocFrame(pcPages.ActivePage.Controls[0]).Typ=tmp) then
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
          aDFrame := TfManageDocFrame.Create(Self);
          pcPages.AddTab(aDFrame,True,'',Data.GetLinkIcon('DOCPAGES@'),False);
          AddDocPages(aDFrame);
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
          aAFrame := TfAccountingFrame.Create(Self);
          pcPages.AddTab(aAFrame);
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
            pToolbar.Visible := False;
            DataSet := TDocuments.Create(nil);
            Refresh(1,'D','1',Null,Null,0);
          end;
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
  aBaseHist: TBaseHistory;
  aCalFrame: TfCalendarFrame;
  aEventEdit: TfEventEdit;
  aObjs: TObjects;
begin
  Result := False;
  Screen.Cursor:=crHourGlass;
  try
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
  if copy(aLink,0,8) = 'CUSTOMER' then
    begin
      if Data.Users.Rights.Right('CUSTOMERS') < RIGHT_READ then exit;
      aFrame := TfPersonFrame.Create(Self);
      aFrame.SetLanguage;
      if aFrame.OpenFromLink(aLink) then
        begin
          pcPages.AddTab(aFrame);
          Result := True;
        end
      else aFrame.Free;
    end
  else if (copy(aLink,0,10) = 'MESSAGEIDX')
       or (copy(aLink,0,8) = 'MESSAGES')
  then
    begin
      aMessageEdit := TfMessageEdit.Create(nil);
      aMessageEdit.OpenFromLink(aLink);
    end
  else if copy(aLink,0,10) = 'MASTERDATA' then
    begin
      if Data.Users.Rights.Right('MASTERDATA') < RIGHT_READ then exit;
      aFrame := TfArticleFrame.Create(Self);
      aFrame.SetLanguage;
      if aFrame.OpenFromLink(aLink) then
        begin
          pcPages.AddTab(aFrame);
          Result := True;
        end
      else aFrame.Free;
    end
  else if copy(aLink,0,10) = 'ALLOBJECTS' then
    begin
      aObjs := TObjects.Create(nil);
      aObjs.SelectByLink(aLink);
      aObjs.Open;
      if aObjs.FieldByName('LINK').IsNull or (copy(aObjs.FieldByName('LINK').AsString,0,10) = 'ALLOBJECTS') then
        begin
          aFrame := TfObjectFrame.Create(Self);
          aFrame.SetLanguage;
          if aFrame.OpenFromLink(aLink) then
            begin
              pcPages.AddTab(aFrame);
              Result := True;
            end
          else aFrame.Free;
        end
      else fMainTreeFrameOpenFromLink(aObjs.FieldByName('LINK').AsString,aSender);
      aObjs.Free;
    end
  else if copy(aLink,0,7) = 'SCRIPTS' then
    begin
      aFrame := TfScriptFrame.Create(Self);
      aFrame.SetLanguage;
      if aFrame.OpenFromLink(aLink) then
        begin
          pcPages.AddTab(aFrame);
          Result := True;
        end
      else aFrame.Free;
    end
  else if copy(aLink,0,6) = 'ORDERS' then
    begin
      if (Data.Users.Rights.Right('ORDERS') < RIGHT_READ)
      and (Data.Users.Rights.Right('PRODUCTION') < RIGHT_READ) then
        begin
          exit;
        end;
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
  else if copy(aLink,0,4) = 'WIKI' then
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
  else if (copy(aLink,0,10) = 'STATISTICS') then
    begin
      if Data.Users.Rights.Right('STATISTICS') < RIGHT_READ then exit;
      aFrame := TfStatisticFrame.Create(Self);
      aFrame.OpenFromLink(aLink);
      pcPages.AddTab(aFrame);
      aFrame.SetLanguage;
      Result := True;
    end
  else if (copy(aLink,0,8) = 'PROJECTS') then
    begin
      if Data.Users.Rights.Right('PROJECTS') < RIGHT_READ then exit;
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
  else if (copy(aLink,0,5) = 'TASKS') then
    begin
      Screen.Cursor:=crDefault;
      FTaskEdit := TfTaskEdit.Create(Self);
      FTaskEdit.Execute(aLink);
      FTaskEdit.Free;
      Result := True;
    end
  else if (copy(aLink,0,8) = 'CALENDAR') then
    begin
      Screen.Cursor:=crDefault;
      aCalFrame := TfCalendarFrame.Create(nil);
      TCalendar(aCalFrame.DataSet).SelectFromLink(aLink);
      aCalFrame.DataSet.Open;
      aCalFrame.DataStore.LoadEvents;
      aEventEdit := TfEventEdit.Create(Self);
      if aCalFrame.DataStore.Resource.Schedule.EventCount>0 then
        aEventEdit.Execute(aCalFrame.DataStore.Resource.Schedule.GetEvent(0),aCalFrame.DataStore.Resource,aCalFrame.DataStore.Directory,aCalFrame.DataStore);
      aEventEdit.Free;
      aCalFrame.Free;
      Result := True;
    end
  else if (copy(aLink,0,15) = 'ACCOUNTEXCHANGE') then
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
  else if (copy(aLink,0,5) = 'LISTS') then
    begin
      aList := TLists.CreateEx(Self,Data);
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
          DefaultRows:='GLOBALWIDTH:%;ACTIVE:25;NAME:100;';
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
  else if (copy(aLink,0,7) = 'HISTORY') then
    begin
      aBaseHist := TBaseHistory.Create(nil);
      aBaseHist.SelectFromLink(aLink);
      abaseHist.Open;
      aLink := aBaseHist.FieldByName('OBJECT').AsString;
      aBaseHist.Free;
      Data.GotoLink(aLink);
    end
  else if (copy(aLink,0,9) = 'DOCUMENTS') then
    begin
      if Data.Users.Rights.Right('DOCUMENTS') < RIGHT_READ then exit;
      aDoc:=TDocuments.CreateEx(Self,Data);
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
  else if (copy(aLink,0,9) = 'INVENTORY') then
    begin
      aInv := TInventorys.CreateEx(Self,Data);
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
          DefaultRows:='GLOBALWIDTH:%;POSNO:25;IDENT:70;SHORTTEXT:120;STORAGE:30;QUANTITY:30;QUANTITYC:30;QUANTITYU:50;';
          Dataset := aInv.Positions;
          DestroyDataSet:=False;
          if aInv.FieldByName('DATE').IsNull then
            begin
              TfFilter(aFrame).Editable:=True;
              gList.OnDragOver:=@aFrameInvDragOver;
              gList.OnDragDrop:=@aFrameInvDragDrop;
              AddToolbarAction(acDeleteListeEntry);
              AddToolbarAction(acBookInventory);
              AddToolbarAction(acCollectInventory);
            end;
        end;
      TfFilter(aFrame).Open;
    end
  else if (copy(aLink,0,8) = 'MEETINGS') then
    begin
      aFrame := TfMeetingFrame.Create(Self);
      aFrame.OpenFromLink(aLink);
      pcPages.AddTab(aFrame);
      aFrame.SetLanguage;
      Result := True;
    end
  else if (copy(aLink,0,7) = 'SCHEME@')
       or (copy(aLink,0,10) = 'SCHEME.ID@') then
    begin
      aFrame := TfShemeFrame.Create(Self);
      aFrame.OpenFromLink(aLink);
      pcPages.AddTab(aFrame);
      aFrame.SetLanguage;
      Result := True;
    end
  //else
  //  Result := Data.GotoLink(aLink)
  ;
  if Result then
    FHistory.Add(aLink);
  finally
    Screen.Cursor:=crDefault;
  end;
end;
procedure TfMain.fMainTreeFrameSelectionChanged(aEntry: TTreeEntry);
var
  Found: Boolean = False;
  i: Integer;
  aFrame: TPrometMainFrame;
  aIFrame: TPrometInplaceDBFrame;
  New: TMenuItem;
  tmp: Char;
  Result : Boolean = false;
begin
  case aEntry.Typ of
  etCustomerList,etCustomers:
    begin
      acContact.Execute;
      result := True;
    end;
  etMasterdata,etArticleList:
    begin
      acMasterdata.Execute;
      result := True;
    end;
  etOrders,etOrderList:
    begin
      acOrders.Execute;
      result := True;
    end;
  etProduction,etProductionList:
    begin
      acProduction.Execute;
      result := True;
    end;
  etStatistics:
    begin
      acStatistics.Execute;
      result := True;
    end;
  etTasks,etMyTasks:
    begin
      acTasks.Execute;
      result := True;
    end;
  etProjects:
    begin
      acProjects.Execute;
      result := True;
    end;
  etCalendar:
    begin
      acCalendar.Execute;
      result := True;
    end;
  etMessages:
    begin
      acMessages.Execute;
      result := True;
    end;
  etWiki:
    begin
      acWiki.Execute;
      result := True;
    end;
  etDocuments,etImages:
    begin
      Application.ProcessMessages;
      if aEntry.Typ=etImages then
        tmp := 'I'
      else tmp := 'D';
      aFrame := nil;
      for i := 0 to pcPages.PageCount-2 do
        if (pcPages.Pages[i].ControlCount > 0)
        and (pcPages.Pages[i].Controls[0] is TfManageDocFrame)
        and (TfManageDocFrame(pcPages.Pages[i].Controls[0]).Typ=tmp)
        then
          begin
            aFrame := TPrometmainFrame(pcPages.Pages[i].Controls[0]);
            pcPages.PageIndex:=i;
            Found := True;
          end;
      if not Found then
        begin
          aFrame := TfManageDocFrame.Create(Self);
          if aEntry.Typ=etImages then
            begin
              TfManageDocFrame(aFrame).Typ:='I';
              TfManageDocFrame(aFrame).TabCaption:=strImages;
            end
          else
            begin
              TfManageDocFrame(aFrame).Typ:='D';
              TfManageDocFrame(aFrame).TabCaption:=strDocumentsOnly;
            end;
          pcPages.AddTab(aFrame,True,'',Data.GetLinkIcon('DOCPAGES@'),False);
          AddDocPages(aFrame);
          if Assigned(aFrame) then
            TfManageDocFrame(aFrame).OpenDir(Null);
          result := True;
        end;
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
          result := True;
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
          result := True;
        end;
    end;
  etMeetings,etMeetingList:
    begin
      acMeetings.Execute;
      result := True;
    end;
  etTimeRegistering:
    begin
      acTimeRegistering.Execute;
      result := True;
    end;
  etAllObjects:
    begin
      acElements.Execute;
      result := True;
    end;
  end;
  with Application as IBaseDbInterface do
    if Result and DBConfig.ReadBoolean('HIDETREE',false) and acShowTree.Checked then
      acShowTree.Execute;
end;

procedure TfMain.fMainTreeFrametvMainExpanding(Sender: TObject;
  Node: TTreeNode; var AllowExpansion: Boolean);
var
  DataT: TTreeEntry;
  Node1: TTreeNode;
  aOrderType: TOrderTyp;
  DefaultOrder: Boolean;
  Node2: TTreeNode;
  Accounts: TAccounts;
  Node3: TTreeNode;
  bTree: TTree;
begin
  DataT := TTreeEntry(Node.Data);
  if not Assigned(DataT) then
    exit;
  if (Node.Count=1) and (Node.Items[0].Data=nil) then
    begin
      Data.Tree.FilterEx('',0,'','ASC',False,True,True);
      Node.DeleteChildren;
      case DataT.Typ of
      etFavourites:
        begin
          Data.Tree.DataSet.Filter:='(('+Data.QuoteField('PARENT')+'='+Data.QuoteValue('0')+') and ('+Data.QuoteField('TYPE')+'='+Data.QuoteValue('F')+'))';
          Data.Tree.DataSet.Filtered:=True;
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
          Data.Tree.DataSet.Filtered:=False;
        end;
      etMessages:
        begin
          Data.Tree.DataSet.Filter:=Data.QuoteField('PARENT')+'='+Data.QuoteValue('0');
          Data.Tree.DataSet.Filtered:=False;
          Data.Tree.DataSet.First;
          bTree := TTree.Create(nil);
          while not Data.Tree.dataSet.EOF do
            begin
              if (Data.Tree.FieldByName('PARENT').AsString='0') and
                ((Data.Tree.FieldByName('TYPE').AsString='N')
              or (Data.Tree.FieldByName('TYPE').AsString='B'))
              then
                begin
                  Node1 := fMainTreeFrame.tvMain.Items.AddChildObject(Node,'',TTreeEntry.Create);
                  TTreeEntry(Node1.Data).Rec := Data.Tree.GetBookmark;
                  TTreeEntry(Node1.Data).DataSource := Data.Tree;
                  TTreeEntry(Node1.Data).Text[0] := Data.Tree.FieldByName('NAME').AsString;
                  if Data.Tree.FieldByName('TYPE').AsString = 'N' then
                    begin
                      TTreeEntry(Node1.Data).Typ := etMessageDir;
                      bTree.Filter(Data.QuoteField('PARENT')+'='+Data.QuoteValue(Data.Tree.Id.AsVariant));
                      if bTree.Count>0 then
                        fMainTreeFrame.tvMain.Items.AddChild(Node1,'');
                    end
                  else if Data.Tree.FieldByName('TYPE').AsString = 'B' then
                    begin
                      TTreeEntry(Node1.Data).Typ := etMessageBoard;
                      bTree.Filter(Data.QuoteField('PARENT')+'='+Data.QuoteValue(Data.Tree.Id.AsVariant));
                      if bTree.Count>0 then
                        fMainTreeFrame.tvMain.Items.AddChild(Node1,'');
                    end;
                end;
              Data.Tree.DataSet.Next;
            end;
          bTree.Free;
          Data.Tree.DataSet.Filtered:=False;
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
              aOrderType := TOrderTyp.Create(nil);
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
              Data.SetFilter(aOrderType,'');
              aOrderType.DataSet.Locate('TYPE','0',[loCaseInsensitive,loPartialKey]);
              fMain.acNewOrder.Caption := Format(strNewOrder,[aOrderType.FieldByName('STATUSNAME').AsString]);
              aOrderType.Free;
            end
          else
            fMain.acNewOrder.Enabled:=False;
        end;
      etProduction:
        begin
          Node1 := fMainTreeFrame.tvMain.Items.AddChildObject(Node,'',TTreeEntry.Create);
          TTreeEntry(Node1.Data).Typ := etProductionList;
          if Data.Users.Rights.Right('PRODUCTION') > RIGHT_READ then
            begin
              aOrderType := TOrderTyp.Create(nil);
              aOrderType.Open;
              Data.SetFilter(aOrderType,'('+Data.QuoteField('SI_PROD')+' = ''Y'')');
              aOrderType.DataSet.First;
              DefaultOrder := False;
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
              aOrderType.Free;
            end
          else
            fMain.acNewOrder.Enabled:=False;
        end;
      etCustomers:
        begin
          Node1 := fMainTreeFrame.tvMain.Items.AddChildObject(Node,'',TTreeEntry.Create);
          TTreeEntry(Node1.Data).Typ := etCustomerList;
          Node1 := fMainTreeFrame.tvMain.Items.AddChildObject(Node,'',TTreeEntry.Create);
          TTreeEntry(Node1.Data).Typ := etAction;
          TTreeEntry(Node1.Data).Action := fMain.acNewContact;
          Data.Tree.DataSet.Filter:='(('+Data.QuoteField('PARENT')+'='+Data.QuoteValue('0')+') and ('+Data.QuoteField('TYPE')+'='+Data.QuoteValue('C')+'))';
          Data.Tree.DataSet.Filtered:=True;
          Data.Tree.DataSet.First;
          while not Data.Tree.dataSet.EOF do
            begin
              Node1 := fMainTreeFrame.tvMain.Items.AddChildObject(Node,'',TTreeEntry.Create);
              TTreeEntry(Node1.Data).Rec := Data.GetBookmark(Data.Tree);
              TTreeEntry(Node1.Data).DataSource := Data.Tree;
              TTreeEntry(Node1.Data).Text[0] := Data.Tree.FieldByName('NAME').AsString;
              TTreeEntry(Node1.Data).Typ := etDir;
              fMainTreeFrame.tvMain.Items.AddChildObject(Node1,'1',TTreeEntry.Create);
              Data.Tree.DataSet.Next;
            end;
          Data.Tree.DataSet.Filtered:=False;
        end;
      etMasterdata:
        begin
          Node1 := fMainTreeFrame.tvMain.Items.AddChildObject(Node,'',TTreeEntry.Create);
          TTreeEntry(Node1.Data).Typ := etArticleList;
          Node1 := fMainTreeFrame.tvMain.Items.AddChildObject(Node,'',TTreeEntry.Create);
          TTreeEntry(Node1.Data).Typ := etAction;
          TTreeEntry(Node1.Data).Action := fMain.acNewMasterdata;
          Data.Tree.DataSet.Filter:='(('+Data.QuoteField('PARENT')+'='+Data.QuoteValue('0')+') and ('+Data.QuoteField('TYPE')+'='+Data.QuoteValue('M')+'))';
          Data.Tree.DataSet.Filtered:=True;
          Data.Tree.DataSet.First;
          while not Data.Tree.dataSet.EOF do
            begin
              Node1 := fMainTreeFrame.tvMain.Items.AddChildObject(Node,'',TTreeEntry.Create);
              TTreeEntry(Node1.Data).Rec := Data.GetBookmark(Data.Tree);
              TTreeEntry(Node1.Data).DataSource := Data.Tree;
              TTreeEntry(Node1.Data).Text[0] := Data.Tree.FieldByName('NAME').AsString;
              TTreeEntry(Node1.Data).Typ := etDir;
              fMainTreeFrame.tvMain.Items.AddChildObject(Node1,'',TTreeEntry.Create);
              Data.Tree.DataSet.Next;
            end;
          Data.Tree.DataSet.Filtered:=False;
        end;
      etProjects:
        begin
          uprojectoverviewframe.AddToMainTree(fMain.acProjectOverview,Node);
          uProjectFrame.AddToMainTree(fMain.acNewProject,Node);
          uRoughpklanningframe.AddToMainTree(fMain.acRoughPlanning,Node);
        end;
      etWiki:
        begin
          Data.Tree.DataSet.Filter:='(('+Data.QuoteField('PARENT')+'='+Data.QuoteValue('0')+') and ('+Data.QuoteField('TYPE')+'='+Data.QuoteValue('W')+'))';
          Data.Tree.DataSet.Filtered:=True;
          Data.Tree.DataSet.First;
          while not Data.Tree.dataSet.EOF do
            begin
              if Data.Tree.DataSet.FieldByName('PARENT').AsString='0' then
                begin
                  Node1 := fMainTreeFrame.tvMain.Items.AddChildObject(Node,'',TTreeEntry.Create);
                  TTreeEntry(Node1.Data).Rec := Data.GetBookmark(Data.Tree);
                  TTreeEntry(Node1.Data).DataSource := Data.Tree;
                  TTreeEntry(Node1.Data).Text[0] := Data.Tree.FieldByName('NAME').AsString;
                  TTreeEntry(Node1.Data).Typ := etDir;
                  fMainTreeFrame.tvMain.Items.AddChildObject(Node1,'',TTreeEntry.Create);
                end;
              Data.Tree.DataSet.Next;
            end;
          Data.Tree.DataSet.Filtered:=False;
        end;
      etDocuments,etImages:
        begin
          umanagedocframe.AddToMainTree(Node);
        end;
      etMeetings:
        begin
          if Data.Users.Rights.Right('MEETINGS') > RIGHT_NONE then
            umeetingframe.AddToMainTree(fMain.acNewMeeting,Node);
        end;
      etFinancial:
        begin
          if Data.Users.Rights.Right('BANKACCNTS') > RIGHT_NONE then
            begin
              Data.RegisterLinkHandler('ACCOUNTEXCHANGE',@fMainTreeFrame.OpenLink,TAccountExchange);
              Node1 := fMainTreeFrame.tvMain.Items.AddChildObject(Node,'',TTreeEntry.Create);
              TTreeEntry(Node1.Data).Typ := etBanking;
              Node2 := fMainTreeFrame.tvMain.Items.AddChildObject(Node1,'',TTreeEntry.Create);
              TTreeEntry(Node2.Data).Typ := etAccounts;
              Node3 := fMainTreeFrame.tvMain.Items.AddChildObject(Node2,'',TTreeEntry.Create);
              TTreeEntry(Node3.Data).Typ := etNewAccount;
              Accounts := TAccounts.Create(nil);
              Accounts.CreateTable;
              Accounts.Open;
              Accounts.DataSet.First;
              while not Accounts.DataSet.EOF do
                begin
                  Node3 := fMainTreeFrame.tvMain.Items.AddChildObject(Node2,'',TTreeEntry.Create);
                  TTreeEntry(Node3.Data).Rec := Accounts.GetBookmark;
                  TTreeEntry(Node3.Data).Text[0] := Accounts.FieldByName('NAME').AsString;
                  TTreeEntry(Node3.Data).Typ := etAccount;
                  Accounts.DataSet.Next;
                end;
              Accounts.Free;
              Node2 := fMainTreeFrame.tvMain.Items.AddChildObject(Node1,'',TTreeEntry.Create);
              TTreeEntry(Node2.Data).Typ := etNewTransfer;
              Node2 := fMainTreeFrame.tvMain.Items.AddChildObject(Node1,'',TTreeEntry.Create);
              TTreeEntry(Node2.Data).Typ := etAccountingQue;
            end;
          if Data.Users.Rights.Right('SALESLIST') > RIGHT_NONE then
            begin
              fMain.pcPages.AddTabClass(TfFilter,strSalesList,@fMain.AddSalesList,-1,True);
              Node1 := fMainTreeFrame.tvMain.Items.AddChildObject(Node,'',TTreeEntry.Create);
              TTreeEntry(Node1.Data).Typ := etSalesList;
              TTreeEntry(Node1.Data).Action := fMain.acSalesList;
            end;
        end;
      etStatistics:
        begin
          uStatisticFrame.AddToMainTree(fMain.acNewStatistics,Node);
        end;
      end;
    end;
  fMainTreeFrame.tvMainExpanding(Sender,Node,AllowExpansion);
end;

procedure TfMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if Assigned(BaseApplication) then with BaseApplication as IBaseApplication do
    Debug('fMain:FormClose enter');
  fMain.Hide;
  IPCTimer.Enabled:=False;
  RefreshTimer.Enabled:=False;
  try
    ImportFavorites;
  except
  end;
  if Assigned(FTimeReg) then
    begin
      FTimereg.StopActualTime;
      FTimeReg.Destroy;
      DeleteFile(UniToSys(GetTempDir+'PMSTimeregistering'));
    end;
  while FHistory.Count>15 do FHistory.Delete(0);
  with Application as IBaseDbInterface do
    begin
      DBConfig.WriteString('HISTORY',FHistory.Text);
      if Assigned(fMainTreeFrame.tvMain.Selected) then
        DBConfig.WriteString('TREENODE',fMainTreeFrame.GetNodeText(fMainTreeFrame.tvMain.Selected))
      else
        DBConfig.WriteString('TREENODE','');
    end;
  if Assigned(fOptions) then
    FreeAndNil(fOptions);
  if Assigned(fHelpContainer) then
    FreeAndNil(fHelpContainer);
  if Assigned(tsStartpage) and (tsStartpage.ControlCount > 0) then
    tsStartpage.Controls[0].Destroy;
  with Application as IBaseApplication do
    begin
      SaveConfig;
      DoExit;
    end;
  try
    pcPages.CloseAll;
  except
  end;
  if Assigned(BaseApplication) then with BaseApplication as IBaseApplication do
    Debug('fMain:FormClose exit');
end;
procedure TfMain.FormCloseQuery(Sender: TObject; var CanClose: boolean);
var
  i: Integer;
begin
  CanClose := True;
  if (uDocumentProcess.ProcessList.Count > 0)
  and (MessageDlg(strProcessesOpen,mtInformation,[mbNo,mbYes],0) = mrNo) then
    CanClose := False;
  for i := 0 to Screen.FormCount-1 do
    if Screen.Forms[i] is TfMessageEdit then
      begin
        if (MessageDlg(strMessageOpen,mtInformation,[mbNo,mbYes],0) = mrNo) then
          CanClose := False;
        break;
      end;
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
  InstallExt('plink', 'Promet-ERP-Link', 'Promet-ERP Link',AppendPathDelim(Application.Location)+'linksender'+ExtractFileExt(Application.ExeName),'%1');
  FHistory := THistory.Create;
  FHistory.FwdAction := acForward;
  FHistory.RewAction := acBack;
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
  uprometipc.OnMessageReceived:=@OnMessageReceived;
  {$IFDEF LCLCARBON}
  spTree.Width:=1;
  {$ELSE}
  spTree.ResizeStyle:=rsPattern;
  {$ENDIF}
end;
procedure TfMain.FormDestroy(Sender: TObject);
begin
  SearchLinks.Destroy;
  FHistory.Free;
  uMainTreeFrame.fMainTreeFrame.Destroy;
end;

procedure TfMain.FormResize(Sender: TObject);
begin
  if pPages.Align=alnone then
    begin
      pPages.Width:=fMain.Width-tvMainAll.Width-spTree.Width;
      {$if declared(lcl_version)}
      if lcl_fullversion>1260 then
        pPages.Height:=fmain.Height-MainMenu1.Height
      else
        pPages.Height:=fmain.Height-20;
      {$else}
      pPages.Height:=fmain.Height-20;
      {$endif}
    end;
end;

procedure TfMain.FormShow(Sender: TObject);
begin
  with Application as IBaseApplication do
    RestoreConfig; //Must be called when Mainform is Visible
  with Application as TBaseVisualApplication do
    LoadLanguageMenu(miLanguage);
end;

procedure TfMain.fSearchActiveSearchItemFound(aIdent: string; aName: string;
  aStatus: string; aActive: Boolean; aLink: string; aPriority: Integer=0;
  aItem: TBaseDBList=nil);
var
  tItem: TTreeNode;
begin
  tItem := tvSearch.Items.Add(nil,aName);
end;

procedure TfMain.fSearchItemFound(aIdent: string; aName: string;
  aStatus: string; aActive: Boolean; aLink: string; aPriority: Integer=0;
  aItem: TBaseDBList=nil);
var
  bItem: TTreeNode;
begin
  bItem := tvSearch.Items.AddChildObject(nil,aName+' ('+aIdent+')',TTreeEntry.Create);
  TTreeEntry(bItem.Data).Link:=aLink;
  TTreeEntry(bItem.Data).Typ:=etLink;
  if aStatus<>'' then
    bItem.Text:=bItem.Text+' ['+aStatus+']';
  bItem.ImageIndex:=Data.GetLinkIcon(aLink,True);
  bItem.SelectedIndex:=bItem.ImageIndex;
end;

procedure TfMain.fSearchSearchDone(Sender: TObject);
begin
  tvSearch.Cursor:=crDefault;
end;

procedure TfMain.IPCTimerTimer(Sender: TObject);
begin
  IPCTimer.Enabled:=False;
  PeekIPCMessages;
  if Assigned(FTimeReg) then
    PeekIPCMessages(GetTempDir+'PMSTimeregistering');
  IPCTimer.Enabled:=True;
end;

procedure TfMain.miOptionsClick(Sender: TObject);
begin
  Screen.Cursor:=crHourGlass;
  Application.ProcessMessages;
  if not (miOptions.Tag=1) then
    begin
      fOptions.RegisterOptionsFrame(TfVisualOptions.Create(fOptions),strVisualOptions,strPersonalOptions);
      fOptions.RegisterOptionsFrame(TfMessageOptions.Create(fOptions),strMessageAccounts,strPersonalOptions);
      fOptions.RegisterOptionsFrame(TfDocumentOptions.Create(fOptions),strFiles,strPersonalOptions);
      fOptions.RegisterOptionsFrame(TfPhoneOptions.Create(fOptions),strPhones,strPersonalOptions);
      if Assigned(Data) and ((Data.Users.Rights.Right('OPTIONS') > RIGHT_READ) or (Data.Users.Rights.Right('MASTERDATAOPTIONS') > RIGHT_READ)) then
        begin
          fOptions.RegisterOptionsFrame(TfMandantOptions.Create(fOptions),strMandant,strMasterdataOptions);
          fOptions.RegisterOptionsFrame(TfUserOptions.Create(fOptions),strUsers,strMasterdataOptions);
          fOptions.RegisterOptionsFrame(TfSystemOptions.Create(fOptions),strSystem,strMasterdataOptions);
          fOptions.RegisterOptionsFrame(TfTableOptions.Create(fOptions),strDatabaseSettings,strMasterdataOptions);
          fOptions.RegisterOptionsFrame(TfStateOptions.Create(fOptions),strStates,strMasterdataOptions);
          fOptions.RegisterOptionsFrame(TfCategoryOptions.Create(fOptions),strCategory,strMasterdataOptions);
          fOptions.RegisterOptionsFrame(TfFinancialOptions.Create(fOptions),strFinance,strMasterdataOptions);
          fOptions.RegisterOptionsFrame(TfOrderTypeOptions.Create(fOptions),strOrderType,strMasterdataOptions);
          fOptions.RegisterOptionsFrame(TfStorageTypeOptions.Create(fOptions),strStorageTypes,strMasterdataOptions);
          fOptions.RegisterOptionsFrame(TfCurrencyOptions.Create(fOptions),strCurrencies,strMasterdataOptions);
          fOptions.RegisterOptionsFrame(TfLanguageOptions.Create(fOptions),strLanguages,strMasterdataOptions);
        end;
      if Assigned(Data) and (Data.Users.Rights.Right('OPTIONS') > RIGHT_READ) then
        begin
          fOptions.RegisterOptionsFrame(TfProcessOptions.Create(fOptions),strProcesses,strAutomationOptions);
          fOptions.RegisterOptionsFrame(TfScriptOptions.Create(fOptions),strScripts,strAutomationOptions);
          fOptions.RegisterOptionsFrame(TfSyncOptions.Create(fOptions),strSync,strAutomationOptions);
          fOptions.RegisterOptionsFrame(TfUserFieldOptions.Create(fOptions),strUserFieldDefs,strGeneralOptions);
          fOptions.RegisterOptionsFrame(TfImportOptions.Create(fOptions),strimportexport,strGeneralOptions);
          fOptions.RegisterOptionsFrame(TfRepairOptions.Create(fOptions),strRepair,strMasterdataOptions);
        end;
      miOptions.Tag:=1;
    end;
  Screen.Cursor:=crDefault;
  fOptions.ShowModal;
end;

function TfMain.OpenAction(aLink: string; Sender: TObject): Boolean;
var
  aAction: String;
  aAc: TContainedAction;
  aFrame: TfOrderFrame;
begin
  Result := False;
  aAction := copy(aLink,pos('@',aLink)+1,length(aLink));
  aAc := ActionList1.ActionByName(aAction);
  if Assigned(aAc) then
    begin
      aAc.Execute;
      Result := True;
    end
  else if Uppercase(copy(aLink,0,18))='ACTION@ACNEWORDER(' then
    begin
      aLink := copy(aLink,19,length(aLink)-19);
      Application.ProcessMessages;
      aFrame := TfOrderFrame.Create(Self);
      pcPages.AddTab(aFrame);
      aFrame.SetLanguage;
      aFrame.New(aLink);
    end;
end;
function TfMain.OpenFilter(aLink: string; Sender: TObject): Boolean;
var
  aFrame: TfFilter;
  aName: String;
begin
  if pos('{',aLink) > 0 then
    aLink := copy(aLink,0,pos('{',aLink)-1)
  else if rpos('(',aLink) > 0 then
    aLink := copy(aLink,0,rpos('(',aLink)-1);
  if IsNumeric(copy(aLink,pos('@',aLink)+1,length(aLink))) then
    begin
      Data.Filters.Filter('',0);
      if Data.Filters.GotoBookmark(StrToInt64(copy(aLink,pos('@',aLink)+1,length(aLink)))) then
        begin
          aName := Data.Filters.FieldByName('NAME').AsString;
          case Data.Filters.FieldByName('TYPE').AsString of
          'C':
            begin
              aFrame := TfFilter.Create(Self);
              pcPages.AddTab(aFrame,True,'',Data.GetLinkIcon('CUSTOMERS@'),False);
              AddCustomerList(aFrame);
              aFrame.cbFilter.Text:=aName;
              aFrame.cbFilterSelect(nil);
            end;
          'M':
            begin
              aFrame := TfFilter.Create(Self);
              pcPages.AddTab(aFrame,True,'',Data.GetLinkIcon('MASTERDATA@'),False);
              AddMasterdataList(aFrame);
              aFrame.cbFilter.Text:=aName;
              aFrame.cbFilterSelect(nil);
            end;
          'O':
            begin
              aFrame := TfFilter.Create(Self);
              pcPages.AddTab(aFrame,True,'',Data.GetLinkIcon('ORDERS@'),False);
              AddOrderList(aFrame);
              aFrame.cbFilter.Text:=aName;
              aFrame.cbFilterSelect(nil);
            end;
          'P':
            begin
              aFrame := TfFilter.Create(Self);
              pcPages.AddTab(aFrame,True,'',Data.GetLinkIcon('PROJECTS@'),False);
              AddProjectList(aFrame);
              aFrame.cbFilter.Text:=aName;
              aFrame.cbFilterSelect(nil);
            end;
          'U':
            begin
              aFrame := TfFilter.Create(Self);
              pcPages.AddTab(aFrame,True,'',Data.GetLinkIcon('STATISTICS@'),False);
              AddStatisticList(aFrame);
              aFrame.cbFilter.Text:=aName;
              aFrame.cbFilterSelect(nil);
            end;
          'E':
            begin
              aFrame := TfFilter.Create(Self);
              pcPages.AddTab(aFrame,True,'',Data.GetLinkIcon('MEETINGS@'),False);
              AddMeetingList(aFrame);
              aFrame.cbFilter.Text:=aName;
              aFrame.cbFilterSelect(nil);
            end;
          'D':
            begin
              aFrame := TfFilter.Create(Self);
              pcPages.AddTab(aFrame,True,'',Data.GetLinkIcon('ALLOBJECTS@'),False);
              AddElementList(aFrame);
              aFrame.cbFilter.Text:=aName;
              aFrame.cbFilterSelect(nil);
            end;
          end;
        end;
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
      aItem := TMenuItem.Create(pmHistory);
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
  try
  if Assigned(pcPages.ActivePage) then
    if pcPages.ActivePage.ControlCount > 0 then
      if pcPages.ActivePage.Controls[0] is TExtControlFrame then
        TExtControlFrame(pcPages.ActivePage.Controls[0]).DoRefresh;
  except
  end;
end;

procedure TfMain.SearchTimerTimer(Sender: TObject);
begin
  SearchTimer.Enabled:=False;
  if eSearch.Text<>strSearchText then
    begin
      fSearch.SetLanguage;
      fSearch.LoadOptions('MAIS');
      fSearch.OnItemFound:=@fSearchItemFound;
      fSearch.OnSearchDone:=@fSearchSearchDone;
      tvSearch.Items.Clear;
      tvSearch.Cursor:=crHourGlass;
      fSearch.eContains.Text:=eSearch.Text;
    end;
end;

procedure TfMain.SenderTfFilterDrawColumnCell(Sender: TObject;
  const Rect: TRect; DataCol: Integer; Column: TColumn; State: TGridDrawState);
begin
  if ((not Assigned(TDBgrid(Sender).DataSource))
  or (not Assigned(TDBgrid(Sender).DataSource.DataSet))
  or (not TDBgrid(Sender).DataSource.DataSet.Active)
  ) then exit;
  with (Sender as TDBGrid), Canvas do
    begin
      Canvas.FillRect(Rect);
      if Column.FieldName = 'ICON' then
        begin
          fVisualControls.Images.Draw(Canvas,Rect.Left,Rect.Top,Column.Field.AsInteger);
        end
      else
        begin
          DefaultDrawColumnCell(Rect, DataCol, Column, State);
        end;
      end;
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
      else if Column.FieldName = 'NEEDSACTION' then
        begin
          if TDBgrid(Sender).DataSource.DataSet.FieldByName('NEEDSACTION').AsString='Y' then
            fVisualControls.Images.Draw(Canvas,Rect.Left,Rect.Top,117);
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

procedure TfMain.SenderTfFilterViewElementDetails(Sender: TObject);
begin
  if TfFilter(Sender).DataSet.FieldByName('LINK').AsString<>'' then
    Data.GotoLink(TfFilter(Sender).DataSet.FieldByName('LINK').AsString)
  else
    Data.GotoLink('ALLOBJECTS@'+TfFilter(Sender).DataSet.Id.AsString);
end;

procedure TfMain.SenderTfMainTaskFrameControlsSenderTfMainTaskFrameTfTaskFrameStartTime
  (Sender: TObject; aProject, aTask,aCategory: string);
begin
  if Assigned(FTimeReg) then
    begin
      FTimeReg.Project:=aProject;
      FTimeReg.Task:=aTask;
      FTimeReg.Link:='';
      FTimeReg.cbCategory.Text:=aCategory;
      FTimereg.mNotes.Clear;
      FTimereg.StartTimereg;
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

procedure TfMain.tvSearchDblClick(Sender: TObject);
begin
  acOpen.Execute;
end;

procedure TfMain.tvSearchStartDrag(Sender: TObject; var DragObject: TDragObject
  );
var
  aSel: TTreeNode;
begin
  aSel := tvSearch.GetNodeAt(tvSearch.ScreenToClient(Mouse.CursorPos).x,tvSearch.ScreenToClient(Mouse.CursorPos).y);
  if Assigned(aSel) and Assigned(aSel) then
    begin
      if TTreeEntry(aSel.Data).Link<>'' then
        begin
          DragObject := TDragEntry.Create(Sender as TControl);
          with DragObject as TDragEntry do
            Links := TTreeEntry(aSel.Data).Link;
        end;
    end;
end;

end.

