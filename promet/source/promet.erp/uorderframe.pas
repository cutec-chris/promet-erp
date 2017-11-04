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
unit uOrderFrame;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, FileUtil, Forms, Controls, ExtCtrls, ActnList, ComCtrls,
  StdCtrls, DbCtrls, Buttons, db, uPrometFrames, uExtControls, uFilterFrame,
  uIntfStrConsts, uPositionFrame, DBGrids, Grids, Graphics, Dialogs, Menus,
  LR_Class, LR_DBSet,uOrder,uMasterdata, uPerson, uPreviewFrame,uBaseSearch,
  uBaseDbClasses,LCLType;
type

  { TfOrderFrame }

  TfOrderFrame = class(TPrometMainFrame)
    {$region}
    acCancel: TAction;
    acSave: TAction;
    acClose: TAction;
    acStartTimeRegistering: TAction;
    acCopy: TAction;
    acPaste: TAction;
    acDelete: TAction;
    acPrint: TAction;
    acMarkasDone: TAction;
    acCreateTransfer: TAction;
    acAddAddress: TAction;
    acRights: TAction;
    acImport: TAction;
    acExport: TAction;
    acRestart: TAction;
    ActionList: TActionList;
    bAddressDelete1: TSpeedButton;
    Bevel1: TBevel;
    Bevel2: TBevel;
    Bevel3: TBevel;
    Bevel4: TBevel;
    Bevel5: TBevel;
    Bevel6: TBevel;
    bExecute1: TSpeedButton;
    cbCurrency: TDBComboBox;
    cbPaymentTarget: TComboBox;
    cbStatus: TComboBox;
    DBGrid1: TDBGrid;
    DBGrid2: TDBGrid;
    eVAT1: TDBEdit;
    lCurrency1: TLabel;
    lCurrency2: TLabel;
    lCurrency3: TLabel;
    lCurrency4: TLabel;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    OrderQMTest: TDataSource;
    OrderQMTestDetail: TDataSource;
    POrderQMTest: TfrDBDataSet;
    POrderQMTestDetail: TfrDBDataSet;
    PPaymentTargets: TfrDBDataSet;
    PaymentTargets: TDatasource;
    deDate1: TDBText;
    deNumber1: TDBText;
    dlOrdernumber1: TDBText;
    ExtRotatedLabel1: TExtRotatedLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    lbResults: TListBox;
    lDate1: TLabel;
    lNumber1: TLabel;
    lOrdernumber1: TLabel;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    Panel1: TPanel;
    pAddresses: TPanel;
    Panel3: TPanel;
    pPosition: TPanel;
    Panel4: TPanel;
    Panel6: TPanel;
    Panel7: TPanel;
    Panel8: TPanel;
    pSearch: TPanel;
    PUsers: TfrDBDataSet;
    sbMenue: TSpeedButton;
    Splitter1: TSplitter;
    ToolBar1: TPanel;
    ToolButton1: TSpeedButton;
    ToolButton2: TSpeedButton;
    ToolButton4: TSpeedButton;
    Users: TDatasource;
    MandantDetails: TDatasource;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    pTop: TPanel;
    POrderImages: TfrDBDataSet;
    pOrderRepairDetail: TfrDBDataSet;
    POrderRepair: TfrDBDataSet;
    POrderPos: TfrDBDataSet;
    POrderAddress: TfrDBDataSet;
    POrders: TfrDBDataSet;
    frReport: TfrReport;
    OrderImages: TDatasource;
    OrderRepairDetail: TDatasource;
    OrderRepair: TDatasource;
    OrderPos: TDatasource;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    miDelete: TMenuItem;
    miPaste: TMenuItem;
    miCopy: TMenuItem;
    miStartTimeregistering: TMenuItem;
    mOrderNote3: TDBMemo;
    mOrderNote4: TDBMemo;
    mOrderNote5: TDBMemo;
    OrderAddress: TDatasource;
    eGross: TDBEdit;
    eNet: TDBEdit;
    eVAT: TDBEdit;
    lCurrency: TLabel;
    lPaymentTarget: TLabel;
    Orders: TDatasource;
    pAddress: TPanel;
    pBottom: TPanel;
    pcHeader: TExtMenuPageControl;
    pMain: TPanel;
    pmAction: TPopupMenu;
    pPreviewT: TPanel;
    spPreview: TSplitter;
    Splitter2: TSplitter;
    TabSheet3: TTabSheet;
    tsCommon: TTabSheet;
    tsFooter1: TPageControl;
    tsHeader1: TTabSheet;
    tsNote1: TTabSheet;
    {$endregion}
    procedure acAddAddressExecute(Sender: TObject);
    procedure acCancelExecute(Sender: TObject);
    procedure acCloseExecute(Sender: TObject);
    procedure acCreateTransferExecute(Sender: TObject);
    procedure acDeleteExecute(Sender: TObject);
    procedure acExportExecute(Sender: TObject);
    procedure acGotoAddressExecute(Sender: TObject);
    procedure acImportExecute(Sender: TObject);
    procedure acMarkasDoneExecute(Sender: TObject);
    procedure acPrintExecute(Sender: TObject);
    procedure acRestartExecute(Sender: TObject);
    procedure acRightsExecute(Sender: TObject);
    procedure acSaveExecute(Sender: TObject);
    procedure ActiveSearchEndItemSearch(Sender: TObject);
    procedure cbPaymentTargetSelect(Sender: TObject);
    procedure cbStatusSelect(Sender: TObject);
    procedure lbResultsDblClick(Sender: TObject);
    procedure OrdersStateChange(Sender: TObject);
    procedure sbMenueClick(Sender: TObject);
    procedure SenderTComboBoxActiveSearchItemFound(aIdent: string;
      aName: string; aStatus: string;aActive : Boolean; aLink: string;aPrio :Integer; aItem: TBaseDBList=nil);
    procedure spPreviewMoved(Sender: TObject);
    function TOrderGetSerial(Sender: TOrder; aMasterdata: TMasterdata;
      aQuantity: Integer): Boolean;
    function TOrderGetStorage(Sender: TOrder; aStorage: TStorage): Boolean;
  private
    { private declarations }
    FPosFrame: TfPosition;
    PreviewFrame: TfPreview;
    FEditable : Boolean;
    ActiveSearch : TSearch;
    FOpenLink : string;
    procedure DoOpenLink(Data : PtrInt);
    procedure AddAdditional(Sender : TObject);
    procedure AddDates(Sender: TObject);
    procedure AddOverview(Sender : TObject);
    procedure AddHistory(Sender : TObject);
    procedure AddDocuments(Sender : TObject);
    procedure AddLinks(Sender : TObject);
    procedure InsertPerson(aPerson : TPerson);
    procedure OnSearchKey(Sender: TObject; X, Y: Integer; var Key: Word;
      Shift: TShiftState; SearchString : string);
    procedure OnSendMessage(aReport: TfrReport; aMail: string;
      aSubject: string; aText: string; var isPrepared: Boolean);
  protected
    procedure DoOpen;override;
    procedure DoOpen(AddHist: Boolean=True);
    function SetRights : Boolean;
  public
    { public declarations }
    procedure gListDrawColumnCell(Sender: TObject;
      const Rect: TRect; DataCol: Integer; Column: TColumn;
      State: TGridDrawState);
    constructor Create(AOwner: TComponent); override;
    destructor Destroy;override;
    function OpenFromLink(aLink : string) : Boolean;override;
    procedure New;override;
    procedure New(aStatus : string);
    procedure SetLanguage;override;
    procedure GotoPosition;
    procedure DoSelect;
    procedure ShowPreview(DocID : Int64);
    procedure ShowFrame; override;
    procedure SetFocus; override;
    procedure RefreshAddress;
  end;
implementation
{$R *.lfm}
uses uData,uBaseVisualControls,uOrderAdditionalFrame,uOverviewFrame,
  uOrderDateFrame,uHistoryFrame,uDocuments,uDocumentFrame,uLinkFrame,
  uRowEditor,uSearch,uBaseDBInterface,uChangeStatus,uError,
  uSelectReport,uNewStorage,uMainTreeFrame,uRepairPositionFrame,uQSPositionFrame,
  uDetailPositionFrame,uBaseVisualApplication,uPersonFrame,uPersonFinance,
  uAccountingTransfer,uPrometFramesInplace,Utils,uorderaddressframe,uMessageEdit,
  uNRights,uBookSerial,uBaseApplication,utextpositionframe,urepairimageframe,
  LCLVersion,uscriptimport,uCreateProductionOrder;
resourcestring
  strAdditional                       = 'Zusätzlich';
  strDates                            = 'Datum';
  strSearchfromOrderAddress           = 'Die Suche waurde aus der Auftragsverwaltung gestartet wenn Sie eine Addresse auswählen, wird diese in dne aktuellen Auftrag übernommen';
  strNoCustomerFound                  = 'kein Kunde gefunden !';
  strOrderToChange                    = 'Auftrag %s %s wird';
  strChangeIn                         = 'geändert in';
  strDerivate                         = 'angeleitet to';
  strNoOrderTypeSelected              = 'Zu dieser Belegart ist kein Auftragstyp ausgewählt, er kann nicht gewandelt werden !';
  strChangeOrderIn                    = '%s %s ist das richtig ?';
  strCustomerhasNoAddress             = 'Der Kontakt hat keine Adresse, möchten Sie eine hinzufügen ?';

procedure TfOrderFrame.OnSendMessage(aReport: TfrReport; aMail: string; aSubject: string;
  aText: string;var isPrepared : Boolean);
var
  i: Integer;
  fMessageEdit: TfMessageEdit;
begin
  {$IF ((LCL_MAJOR >= 1) and (LCL_MINOR > 5))}
  FOR i := 0 TO ExportFilters.Count - 1 DO
    if pos('PDF',Uppercase(ExportFilters[i].FilterDesc)) > 0 then
  {$else}
  FOR i := 0 TO frFiltersCount - 1 DO
    if pos('PDF',Uppercase(frFilters[i].FilterDesc)) > 0 then
  {$endif}
      if aReport.PrepareReport then
        begin
          isPrepared := True;
          with BaseApplication as IBaseApplication do
            begin
              {$IF ((LCL_MAJOR >= 1) and (LCL_MINOR > 5))}
              aReport.ExportTo(ExportFilters[i].ClassRef,GetInternalTempDir+aSubject+'.pdf');
              {$else}
              aReport.ExportTo(frFilters[i].ClassRef,GetInternalTempDir+aSubject+'.pdf');
              {$endif}
              fMessageEdit := TfMessageEdit.Create(nil);
              fMessageEdit.SendMailToWithDoc(aMail,aSubject,aText,GetInternalTempDir+aSubject+'.pdf',True);
            end;
        end;
end;

procedure TfOrderFrame.DoOpen;
begin
  inherited DoOpen;
  DoOpen(True);
end;

procedure TfOrderFrame.OnSearchKey(Sender: TObject; X, Y: Integer; var Key: Word;
  Shift: TShiftState; SearchString : string);
var
  SearchTypes : TFullTextSearchTypes = [];
  SearchLocations : TSearchLocations;
  i: Integer;
  tmp: TCaption;
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
              if lbResults.SelCount=1 then
                begin
                  lbResultsDblClick(nil);
                  Key := 0;
                end
              else pSearch.Visible:=False;
            end;
          VK_ESCAPE:
            begin
              if (not pSearch.Focused) and (not lbResults.Focused) then
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
          pSearch.Left:=pAddresses.Left+X;
          pSearch.Top:=ScreenToClient(pAddresses.ClientToScreen(Point(X,Y))).Y;
        end;
      if Assigned(ActiveSearch) then
        ActiveSearch.Abort;
      SearchTypes := SearchTypes+[fsShortnames];
      SearchTypes := SearchTypes+[fsIdents];
      SetLength(SearchLocations,length(SearchLocations)+1);
      SearchLocations[length(SearchLocations)-1] := strCustomers;
      SetLength(SearchLocations,length(SearchLocations)+1);
      SearchLocations[length(SearchLocations)-1] := strAdresses;
      lbResults.Items.Clear;
      if not Assigned(ActiveSearch) then
        ActiveSearch := TSearch.Create(SearchTypes,SearchLocations,True,5);
      ActiveSearch.Sender := TComponent(Sender);
      ActiveSearch.OnItemFound:=@SenderTComboBoxActiveSearchItemFound;
      ActiveSearch.OnEndSearch:=@ActiveSearchEndItemSearch;
      ActiveSearch.Start(SearchString);
    end;
end;
procedure TfOrderFrame.acCloseExecute(Sender: TObject);
begin
  CloseFrame;
end;
procedure TfOrderFrame.acCreateTransferExecute(Sender: TObject);
var
  aFilter: String;
  aFrame: TfPersonFrame;
  i: Integer;
begin
  if pAddresses.ControlCount > 0 then
    TfOrderAddress(pAddresses.Controls[0]).acGotoAddress.Execute;
  if  (fMainTreeFrame.pcPages.ActivePage.ControlCount > 0)
  and (fMainTreeFrame.pcPages.ActivePage.Controls[0] is TfPersonFrame) then
    begin
      aFrame := TfPersonFrame(fMainTreeFrame.pcPages.ActivePage.Controls[0]);
      for i := 0 to aFrame.pcPages.PageCount-1 do
        if aFrame.pcPages.Pages[i].ControlCount > 0 then
          if aFrame.pcPages.Pages[i].Controls[0] is TfPersonFinance then
            begin
              TfPersonFinance(aFrame.pcPages.Pages[i].Controls[0]).bTransferClick(nil);
              fTransfer.mPurpose.Clear;
              fTransfer.mPurpose.Lines.Add(strCommission+' '+DataSet.FieldByName('COMMISSION').AsString);
              fTransfer.eAmount.Text:=FormatFloat('0.00',DataSet.FieldByName('GROSSPRICE').AsFloat);
            end;
      aFrame.acClose.Execute;
    end;
end;
procedure TfOrderFrame.acDeleteExecute(Sender: TObject);
begin
  if MessageDlg(strRealdelete,mtInformation,[mbYes,mbNo],0) = mrYes then
    begin
      Screen.Cursor := crHourglass;
      Application.ProcessMessages;
      DataSet.Delete;
      Abort;
      acClose.Execute;
      Screen.Cursor := crDefault;
    end;
end;

procedure TfOrderFrame.acExportExecute(Sender: TObject);
begin
  if fScriptImport.Execute(icExport,'O',FDataSet) then
    DataSet.DataSet.Refresh;
end;

procedure TfOrderFrame.acGotoAddressExecute(Sender: TObject);
var
  aPerson: TPerson;
begin
  aPerson := TPerson.CreateEx(Self,Data);
  aPerson.SelectByAccountNo(TOrder(DataSet).Address.FieldByName('ACCOUNTNO').AsString);
  aPerson.Open;
  if aPerson.Count > 0 then
    begin
      Data.GotoLink(Data.BuildLink(aPerson.DataSet));
    end;
  aPerson.Free;
end;

procedure TfOrderFrame.acImportExecute(Sender: TObject);
begin
  if fScriptImport.Execute(icImport,'O',FDataSet) then
    begin
      DataSet.DataSet.Refresh;
      FPosFrame.Refresh;
    end;
end;

procedure TfOrderFrame.acMarkasDoneExecute(Sender: TObject);
begin
  if not DataSet.CanEdit then
    DataSet.DataSet.Edit;
  DataSet.FieldByName('DONE').AsString := 'Y';
end;
procedure TfOrderFrame.acPrintExecute(Sender: TObject);
begin
  if acSave.Enabled then
    acSave.Execute;
  fSelectReport.DoSetup;
  fSelectReport.OnSendMessage:=@OnSendMessage;
  fSelectReport.SetLanguage;
  TOrder(DataSet).OnGetStorage:=@TOrderGetStorage;
  TOrder(DataSet).OnGetSerial:=@TOrderGetSerial;
  fSelectReport.ReportType := 'OR'+DataSet.FieldByName('STATUS').AsString;
  frReport.DataType:=dtDataSource;
  fSelectReport.Report := frReport;
  MandantDetails.DataSet := Data.MandantDetails.DataSet;
  Data.MandantDetails.Open;
  OrderRepair.DataSet := TOrder(DataSet).Positions.Repair.DataSet;
  OrderRepairDetail.DataSet := TOrder(DataSet).Positions.Repair.Details.DataSet;
  OrderQMTest.DataSet := TOrder(DataSet).Positions.QMTest.DataSet;
  OrderQMTestDetail.DataSet := TOrder(DataSet).Positions.QMTest.Details.DataSet;
  OrderImages.DataSet := TOrder(DataSet).Positions.Images.DataSet;
  TOrder(DataSet).Positions.Images.Open;
  Users.DataSet := Data.Users.DataSet;
  PaymentTargets.DataSet := Data.PaymentTargets.DataSet;
  fSelectReport.DataSet := DataSet;
  DataSet.CascadicPost;
  FPosFrame.GridView.GotoActiveRow;
  if UseTransactions then
    begin
      with Application as IBaseDbInterface do
        Data.CommitTransaction(Connection);
    end;
  fSelectReport.Execute;
  fSelectReport.OnSendMessage:=nil;
  if UseTransactions then
    begin
      with Application as IBaseDbInterface do
        Data.StartTransaction(Connection);
    end;
  if fSelectReport.Booked then
    if acSave.Enabled then
      acSave.Execute;
  if fSelectReport.Booked then
    begin
      FOpenLink := Data.BuildLink(DataSet.DataSet);
      Application.QueueAsyncCall(@DoOpenLink,PtrInt(nil));
    end;
  TOrder(DataSet).Positions.Images.Close;
end;

procedure TfOrderFrame.acRestartExecute(Sender: TObject);
begin
  TOrder(FDataSet).Duplicate;
  DoOpen(False);
end;

procedure TfOrderFrame.acRightsExecute(Sender: TObject);
begin
  fNRights.Execute(DataSet.Id.AsVariant);
end;
procedure TfOrderFrame.acCancelExecute(Sender: TObject);
begin
  Abort;
end;
procedure TfOrderFrame.acAddAddressExecute(Sender: TObject);
var
  aAddress: TfOrderAddress;
  aInsert: Boolean;
begin
  if (TOrder(DataSet).Address.CanEdit) then
    begin
      aInsert := TOrder(DataSet).Address.DataSet.State = dsInsert;
      TOrder(DataSet).Address.Post;
      if aInsert then
        TfOrderAddress(pAddresses.Controls[0]).Rec:=TOrder(fDataSet).Address.GetBookmark;
    end;
  aAddress := TfOrderAddress.Create(Self);
  aAddress.Name:='';
  aAddress.Parent := pAddresses;
  aAddress.Align:=alLeft;
  aAddress.Show;
  aAddress.SetLanguage;
  aAddress.Left:=pAddresses.Width+1;
  pAddresses.Width:=pAddresses.Width+aAddress.Width;
  aAddress.DataSet := TOrder(fDataSet).Address;
  aAddress.OnSearchKey:=@OnSearchKey;
  aAddress.Rec:=0;
  if aAddress.CanFocus then
    aAddress.SetFocus;
  RefreshAddress;
end;
procedure TfOrderFrame.acSaveExecute(Sender: TObject);
begin
  Save;
end;
procedure TfOrderFrame.ActiveSearchEndItemSearch(Sender: TObject);
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

procedure TfOrderFrame.AddDates(Sender: TObject);
begin
  with Sender as TfOrderDateFrame do
    begin
      IsNeeded(FDataSet.DataSet);
      SetRights(FEditable);
    end;
end;

procedure TfOrderFrame.cbPaymentTargetSelect(Sender: TObject);
begin
  if Data.PaymentTargets.DataSet.Locate('TEXT',cbPaymentTarget.Text,[]) then
    begin
      if not DataSet.CanEdit then
        DataSet.DataSet.Edit;
      DataSet.FieldByName('PAYMENTTAR').AsString := Data.PaymentTargets.FieldByName('ID').AsString;
    end;
end;
procedure TfOrderFrame.cbStatusSelect(Sender: TObject);
var
  tmp,
  newnumber: String;
  OrderType: LongInt;
  Rec: String;

  procedure RestoreStatus;
  begin
    TOrder(DataSet).OrderType.DataSet.Locate('STATUS',TOrder(DataSet).FieldByName('STATUS').AsString,[loCaseInsensitive]);
    cbStatus.Text := TOrder(DataSet).OrderType.FieldByName('STATUSNAME').AsString+' ('+TOrder(DataSet).OrderType.FieldByName('STATUS').AsString+')';
  end;

begin
  newnumber := '';
  tmp := copy(cbStatus.text,pos('(',cbStatus.text)+1,length(cbStatus.text));
  tmp := copy(tmp,0,pos(')',tmp)-1);
  fChangeStatus.SetLanguage;
  fChangeStatus.lOrder.Caption := Format(strOrdertochange,[TOrder(DataSet).FieldByName('STATUS').AsString,TOrder(DataSet).FieldByName('ORDERNO').AsString]);
  if tmp = TOrder(DataSet).FieldByName('STATUS').AsString then exit;
  if not TOrder(DataSet).OrderType.DataSet.Locate('STATUS',tmp,[loCaseInsensitive]) then
    Data.SetFilter(TOrder(DataSet).OrderType,'');
  if TOrder(DataSet).OrderType.DataSet.Locate('STATUS',tmp,[loCaseInsensitive]) then
    begin
      OrderType := StrToIntDef(trim(copy(TOrder(DataSet).OrderType.FieldByName('TYPE').AsString,0,2)),0);
      if trim(TOrder(DataSet).OrderType.FieldByName('TYPE').AsString) = '' then
        begin
          fError.ShowError(strNoOrderTypeSelected);
          RestoreStatus;
          exit;
        end;
      fChangeStatus.lChange.Caption := strChangeIn;
      fChangeStatus.lTarget.Caption := Format(strChangeOrderIn,[tmp,TOrder(DataSet).FieldByName('ORDERNO').AsString]);
      if TOrder(DataSet).OrderType.FieldByName('ISDERIVATE').AsString = 'Y' then
        begin
          fChangeStatus.lChange.Caption := strDerivate;
          fChangeStatus.lTarget.Caption := Format(strChangeOrderIn,[tmp,newnumber]);
        end;
      if fChangeStatus.Execute then
        begin
          if not fChangeStatus.cbFiltered.Checked then
            FPosFrame.GridView.ClearFilters;
          Save;
          if UseTransactions then
            begin
              with Application as IBaseDbInterface do
                Data.CommitTransaction(Connection);
            end;
          TOrder(DataSet).ChangeStatus(tmp);
          if UseTransactions then
            begin
              with Application as IBaseDbInterface do
                Data.StartTransaction(Connection);
            end;
          DoOpen(False);
        end
      else
        Restorestatus;
    end
  else
    RestoreStatus;
end;
procedure TfOrderFrame.lbResultsDblClick(Sender: TObject);
var
  aPerson: TPerson;
  aAdress: TBaseDbAddress;
begin
  if TfOrderAddress(ActiveSearch.Sender).EnterEdit then
    begin
      if lbResults.ItemIndex < 0 then exit;
      pSearch.Visible:=False;
      aPerson := TPerson.Create(nil);
      aPerson.SelectFromLink(TLinkObject(lbResults.Items.Objects[lbResults.ItemIndex]).Link);
      aPerson.Open;
      if aPerson.Count=1 then
        begin
          aPerson.Address.Open;
          TfOrderAddress(ActiveSearch.Sender).DataSet.Assign(aPerson);
          TfOrderAddress(ActiveSearch.Sender).DataSet := TOrder(DataSet).Address;
          GotoPosition;
        end
      else
        begin
          aAdress := TPersonAddress.Create(nil);
          aAdress.SelectFromLink(TLinkObject(lbResults.Items.Objects[lbResults.ItemIndex]).Link);
          aAdress.Open;
          if aAdress.Count=1 then
            begin
              TfOrderAddress(ActiveSearch.Sender).DataSet.Assign(aAdress);
              TfOrderAddress(ActiveSearch.Sender).DataSet := TOrder(DataSet).Address;
            end;
          aAdress.Free;
        end;
      aPerson.Free;
    end;
end;
procedure TfOrderFrame.OrdersStateChange(Sender: TObject);
begin
  acSave.Enabled := DataSet.CanEdit or DataSet.Changed;
  acCancel.Enabled:= DataSet.CanEdit or DataSet.Changed;
end;
procedure TfOrderFrame.sbMenueClick(Sender: TObject);
begin
  TSpeedButton(Sender).PopupMenu.PopUp(TSpeedButton(Sender).ClientOrigin.x,TSpeedButton(Sender).ClientOrigin.y+TSpeedButton(Sender).Height);
end;
procedure TfOrderFrame.SenderTComboBoxActiveSearchItemFound(aIdent: string;
  aName: string; aStatus: string; aActive: Boolean; aLink: string;
  aPrio: Integer; aItem: TBaseDBList);
begin
  with pSearch do
    begin
      if not Visible then
        Visible := True;
    end;
  if aActive then
    if lbResults.Items.IndexOf(Data.GetLinkDesc(aLink))=-1 then
      lbResults.Items.AddObject(Data.GetLinkDesc(aLink) ,TLinkObject.Create(aLink));
end;
procedure TfOrderFrame.spPreviewMoved(Sender: TObject);
begin
  with Application as IBaseDbInterface do
    DBConfig.WriteInteger('ORDERPREVIEWWIDTH',pPreviewT.Width);
end;

function TfOrderFrame.TOrderGetSerial(Sender: TOrder; aMasterdata: TMasterdata;aQuantity : Integer): Boolean;
begin
  Result := fBookSerial.Execute(Sender,aMasterdata,aQuantity);
end;

function TfOrderFrame.TOrderGetStorage(Sender: TOrder; aStorage: TStorage
  ): Boolean;
begin
  Result := fNewStorage.Execute(Sender,aStorage);
end;
procedure TfOrderFrame.DoOpenLink(Data: PtrInt);
begin
  TfOrderFrame(Self).OpenFromLink(FOpenLink);
end;
procedure TfOrderFrame.AddAdditional(Sender: TObject);
begin
  with Sender as TfOrderAdditionalFrame do
    begin
      Order.DataSet := FDataSet.DataSet;
      InitOrder(TOrder(FDataSet));
      TPrometInplaceFrame(Sender).SetRights(FEditable);
    end;
end;
procedure TfOrderFrame.AddOverview(Sender: TObject);
begin
  with Sender as TfOrderOverviewFrame do
    begin
      Order.DataSet := FDataSet.DataSet;
      ParentOrder := TOrder(FDataSet);
      TPrometInplaceFrame(Sender).SetRights(FEditable);
    end;
end;
procedure TfOrderFrame.AddHistory(Sender: TObject);
begin
  TfHistoryFrame(Sender).BaseName:='ORDER';
  TfHistoryFrame(Sender).DataSet := TOrder(FDataSet).History;
  TPrometInplaceFrame(Sender).SetRights(FEditable);
end;
procedure TfOrderFrame.AddDocuments(Sender: TObject);
var
  aDocuments: TDocuments;
begin
  if not Assigned(TfDocumentFrame(Sender).DataSet) then
    begin
      aDocuments := TDocuments.CreateEx(Self,Data);
      TfDocumentFrame(Sender).DataSet := aDocuments;
      TfDocumentFrame(Sender).Refresh(DataSet.Id.AsVariant,'O',DataSet.FieldByName('ORDERNO').AsString,Null,Null,0);
      TPrometInplaceFrame(Sender).SetRights(FEditable);
    end;
end;
procedure TfOrderFrame.AddLinks(Sender: TObject);
begin
  TfLinkFrame(Sender).BaseName:='ORDER';
  TfLinkFrame(Sender).DataSet := TOrder(FDataSet).Links;
  TfLinkFrame(Sender).Order := TOrder(FDataSet);
  TPrometInplaceFrame(Sender).SetRights(FEditable);
end;
procedure TfOrderFrame.InsertPerson(aPerson: TPerson);
var
  aFrame: TfPersonFrame;
begin
  if aPerson.Address.Count > 0 then
    begin
      TOrder(DataSet).Address.Assign(aPerson);
      if TOrder(DataSet).Address.CanEdit then
        begin
          try
            TOrder(DataSet).Address.DataSet.Post;
            GotoPosition;
          except
          end;
        end;
    end
  else if MessageDlg(strCustomerhasNoAddress,mtInformation,[mbYes,mbNo],0) = mrYes then
    begin
      TOrder(DataSet).Address.DataSet.Cancel;
      aFrame := TfPersonFrame.Create(Self);
      fMainTreeFrame.pcPages.AddTab(aFrame);
      aFrame.SetLanguage;
      aFrame.OpenFromLink(Data.BuildLink(aPerson.DataSet));
      aFrame.AddAddress;
      aFrame.eName.SetFocus;
      aFrame.CustomerOf := Data.BuildLink(DataSet.DataSet);
    end;
end;
procedure TfOrderFrame.DoOpen(AddHist : Boolean = True);
var
  OrderTyp: LongInt;
  tmp: String;
  aFrame: TFrame;
  aDocuments: TDocuments;
  aDocFrame: TfDocumentFrame;
  Editable: Boolean;
  aAddress: TfOrderAddress;
begin
  pMain.Visible:=False;
  pPreviewT.Visible:=False;
  spPreview.Visible := False;
  while pAddresses.ControlCount > 0 do
    pAddresses.Controls[0].Free;
  Orders.DataSet := FDataSet.DataSet;
  if not FDataSet.DataSet.Active then
    FDataSet.Open;
  TOrder(FDataSet).OpenItem;
  if not TOrder(fDataSet).Address.DataSet.Active then
    TOrder(fDataSet).Address.DataSet.Open;
  OrderAddress.DataSet := TOrder(fDataSet).Address.DataSet;
  with OrderAddress,DataSet do
    begin
      First;
      pAddresses.Width:=0;
      while not EOF do
        begin
          aAddress := TfOrderAddress.Create(Self);
          aAddress.Name:='';
          aAddress.Parent := pAddresses;
          aAddress.Align:=alLeft;
          aAddress.Show;
          aAddress.SetLanguage;
          aAddress.Left:=pAddresses.Width+1;
          aAddress.Rec:=TOrder(fDataSet).Address.GetBookmark;
          aAddress.DataSet := TOrder(fDataSet).Address;
          aAddress.OnSearchKey:=@OnSearchKey;
          Next;
        end;
    end;
  TabCaption := TOrder(FDataSet).OrderType.FieldByName('STATUSNAME').AsString+' '+TOrder(FDataSet).FieldByName('ORDERNO').AsString;
  cbStatus.Items.Clear;
  cbStatus.Text := '';
  if not TOrder(FDataSet).OrderType.DataSet.Locate('STATUS',TOrder(FDataSet).FieldByName('STATUS').AsString,[loCaseInsensitive]) then
    begin
      Data.SetFilter(TOrder(FDataSet).OrderType,'');
      TOrder(FDataSet).OrderType.DataSet.Locate('STATUS',TOrder(FDataSet).FieldByName('STATUS').AsString,[loCaseInsensitive]);
    end;
  cbStatus.Items.Add(TOrder(FDataSet).OrderType.FieldByName('STATUSNAME').AsString+' ('+TOrder(FDataSet).OrderType.FieldByName('STATUS').AsString+')');
  cbStatus.Text := TOrder(FDataSet).OrderType.FieldByName('STATUSNAME').AsString+' ('+TOrder(FDataSet).OrderType.FieldByName('STATUS').AsString+')';
  tmp := trim(TOrder(FDataSet).OrderType.FieldByName('DERIVATIVE').AsString);
  OrderTyp := StrToIntDef(trim(copy(TOrder(FDataSet).OrderType.FieldByName('TYPE').AsString,0,2)),0);
  if (length(tmp) = 0) or (tmp[length(tmp)] <> ';') then
    tmp := tmp+';';
  while pos(';',tmp) > 0 do
    begin
      if TOrder(FDataSet).OrderType.DataSet.Locate('STATUS',copy(tmp,0,pos(';',tmp)-1),[loCaseInsensitive]) then
        cbStatus.Items.Add(TOrder(FDataSet).OrderType.FieldByName('STATUSNAME').AsString+' ('+TOrder(FDataSet).OrderType.FieldByName('STATUS').AsString+')');
      tmp := copy(tmp,pos(';',tmp)+1,length(tmp));
    end;
  Editable := SetRights;
  RefreshAddress;

  pcHeader.NewFrame(TfOrderAdditionalFrame,(FDataSet.State = dsInsert) or TfOrderAdditionalFrame(aFrame).IsNeeded(FDataSet.DataSet),strAdditional,@AddAdditional);

  pcHeader.AddTabClass(TfOrderOverviewFrame,strOverview,@AddOverview);
  if Assigned(pcHeader.GetTab(TfOrderOverviewFrame)) then
    pcHeader.GetTab(TfOrderOverviewFrame).Free;
  if (FDataSet.Count > 1) then
    pcHeader.AddTab(TfOrderOverviewFrame.Create(Self),False);

  pcHeader.AddTabClass(TfOrderDateFrame,strDates,@AddDates);
  if Assigned(pcHeader.GetTab(TfOrderDateFrame)) then
    pcHeader.GetTab(TfOrderDateFrame).Free;
  aFrame := TfOrderDateFrame.Create(Self);
  if TfOrderDateFrame(aFrame).IsNeeded(FDataSet.DataSet) or (DataSet.State=dsInsert) then
    begin
      pcHeader.AddTab(aFrame,False);
      TfOrderDateFrame(aFrame).SetRights(FEditable);
    end
  else aFrame.Free;

  pcHeader.AddTabClass(TfDocumentFrame,strFiles,@AddDocuments);
  if Assigned(pcHeader.GetTab(TfDocumentFrame)) then
    pcHeader.GetTab(TfDocumentFrame).Free;
  if (FDataSet.State <> dsInsert) and (fDataSet.Count > 0) and (not Assigned(pcHeader.GetTab(TfDocumentFrame))) then
    begin
      aDocuments := TDocuments.CreateEx(Self,Data);
      aDocuments.CreateTable;
      aDocuments.Select(DataSet.Id.AsVariant,'O',DataSet.FieldByName('ORDERNO').AsString,Null,Null);
      aDocuments.Open;
      if aDocuments.Count = 0 then
        aDocuments.Free
      else
        begin
          aDocFrame := TfDocumentFrame.Create(Self);
          aDocFrame.DataSet := aDocuments;
          pcHeader.AddTab(aDocFrame,False);
          aDocFrame.SetRights(Editable);
        end;
    end;

  TOrder(DataSet).Links.Open;
  pcHeader.NewFrame(TfLinkFrame,(TOrder(DataSet).Links.Count > 0),strLinks,@AddLinks);

  TOrder(fDataSet).Positions.Open;
  FPosFrame.BaseName:='ORDERS'+DataSet.FieldByName('STATUS').AsString;
  FPosFrame.Dataset := TOrder(fDataSet).Positions;
  FPosFrame.SetLanguage;
  if not Data.PaymentTargets.DataSet.Active then
    Data.PaymentTargets.Open;
  if Data.PaymentTargets.DataSet.Locate('ID',DataSet.FieldByName('PAYMENTTAR').AsString,[]) then
    cbPaymenttarget.Text := Data.PaymentTargets.FieldByName('TEXT').AsString
  else
    cbPaymenttarget.Text := '';
  OrderPos.DataSet := Torder(fDataSet).Positions.DataSet;
  acCreateTransfer.Visible:= OrderTyp = 3;
  with Application as TBaseVisualApplication do
    AddTabClasses('ORH',pcHeader);
  with Application as TBaseVisualApplication do
    AddTabs(pcHeader);
  pcHeader.PageIndex:=0;
  pMain.Visible:=True;
  TOrder(DataSet).History.Open;
  pcHeader.NewFrame(TfHistoryFrame,(TOrder(DataSet).History.Count > 0) and AddHist,strHistory,@AddHistory);
  inherited DoOpen;
  FPosFrame.SetFocus;
  if Dataset.State = dsInsert then
    begin
      if pAddresses.ControlCount > 0 then
        TfOrderAddress(pAddresses.Controls[0]).SetFocus;
      exit;
    end;
  //All what not depends on new order
  if TOrder(DataSet).Address.Count > 0 then
    TOrder(DataSet).Address.DataSet.Locate('TYPE','DAD',[loPartialKey]);
  pcHeader.Change;
end;
function TfOrderFrame.SetRights : Boolean;
var
  OrderType: LongInt;
  Editable: Boolean;
  i: Integer;
  Editable2: Boolean = False;
begin
  TOrder(FDataSet).OrderType.DataSet.Locate('STATUS',TOrder(FDataSet).DataSet.FieldByName('STATUS').AsString,[]);
  OrderType := StrToIntDef(trim(copy(TOrder(FDataSet).OrderType.FieldByName('TYPE').AsString,0,2)),0);
  FEditable := Application.HasOption('e','editall') or ((Data.Users.Rights.Right('ORDERS') > RIGHT_READ) and (DataSet.FieldByName('DATE').IsNull));
  EditAble := FEditable;
  Result := Editable;
  if (Data.Users.Rights.Right('ORDERS') > RIGHT_DELETE) and (TOrder(FDataSet).OrderType.FieldByName('CHANGEABLE').AsString='Y') then
    begin
      Editable := True;
      Editable2:=True;
    end;
  cbStatus.Enabled:=(not Editable) and (Data.Users.Rights.Right('ORDERS') > RIGHT_READ);
  if Editable2 then
    cbStatus.Enabled:=True;
  Self.FPosFrame.SetRights(Editable);
  acAddAddress.Enabled:=Editable;
  acDelete.Enabled:=Editable and (Data.Users.Rights.Right('ORDERS') > RIGHT_WRITE);
  cbPaymentTarget.Enabled := Editable;
  cbCurrency.Enabled := Editable;
  acRestart.Enabled:=True;
  for i := 0 to pAddresses.ControlCount-1 do
    TfOrderAddress(pAddresses.Controls[i]).SetRights(Editable);
  acRights.Enabled:=Data.Users.Rights.Right('ORDERS') >= RIGHT_PERMIT;
end;
procedure TfOrderFrame.RefreshAddress;
var
  i: Integer;
begin
  pAddresses.Width:=pAddresses.ControlCount*230;
  if (pAddresses.ControlCount = 0) and FEditable then
    begin
      acAddAddress.Execute;
    end;
  if pAddresses.ControlCount>0 then
    TfOrderAddress(pAddresses.Controls[pAddresses.ControlCount-1]).DataSet := TOrder(DataSet).Address;
end;
procedure TfOrderFrame.DoSelect;
begin
  if (not cbStatus.Enabled) then
    begin
      if TOrder(DataSet).Address.Count = 0 then
        begin
          if FEditable and (pAddresses.ControlCount>0) then
            TfOrderAddress(pAddresses.Controls[0]).SetFocus;
        end
      else
        FPosFrame.SetFocus;
    end;
  deDate1.Constraints.MaxWidth:=deDate1.Canvas.TextExtent('00.00.0000 ').cx;
end;
procedure TfOrderFrame.ShowPreview(DocID: Int64);
begin
  PreviewFrame.LoadFromDocuments(DocID);
  pPreviewT.Visible:=True;
  spPreview.Visible:=True;
  with Application as IBaseDbInterface do
    pPreviewT.Width:=DBConfig.ReadInteger('ORDERPREVIEWWIDTH',500);
end;
procedure TfOrderFrame.ShowFrame;
begin
  inherited ShowFrame;
  //FPosFrame.GridView.SetupHeader;
end;

procedure TfOrderFrame.SetFocus;
begin
  inherited SetFocus;
  FPosFrame.SetFocus;
end;

procedure TfOrderFrame.GotoPosition;
begin
  FPosFrame.SetFocus;
  acSave.Enabled := DataSet.CanEdit or DataSet.Changed;
  acCancel.Enabled:= DataSet.CanEdit or DataSet.Changed;
end;
procedure TfOrderFrame.gListDrawColumnCell(Sender: TObject;
  const Rect: TRect; DataCol: Integer; Column: TColumn; State: TGridDrawState);
begin
  if ((not Assigned(TDBgrid(Sender).DataSource))
  or (not Assigned(TDBgrid(Sender).DataSource.DataSet))
  or (not TDBgrid(Sender).DataSource.DataSet.Active)
  ) then exit;
  with (Sender as TDBGrid), Canvas do
    begin
      Canvas.FillRect(Rect);
      if Column.FieldName = 'PAYEDON' then
        begin
          if not TDBgrid(Sender).DataSource.DataSet.FieldByName('PAYEDON').IsNull then
            fVisualControls.Images.Draw(Canvas,Rect.Left,Rect.Top,9);
        end
      else if Column.FieldName = 'DELIVERED' then
        begin
          if TDBgrid(Sender).DataSource.DataSet.FieldByName('DELIVERED').AsString = 'Y' then
            fVisualControls.Images.Draw(Canvas,Rect.Left,Rect.Top,10);
        end
      else if Column.FieldName = 'DONE' then
        begin
          if TDBgrid(Sender).DataSource.DataSet.FieldByName('DONE').AsString = 'Y' then
            fVisualControls.Images.Draw(Canvas,Rect.Left,Rect.Top,74);
        end
      else
        begin
          if copy(TDBgrid(Sender).DataSource.DataSet.FieldByName('ORDERNO').AsString,length(TDBgrid(Sender).DataSource.DataSet.FieldByName('ORDERNO').AsString)-1,2) = '00' then
            Canvas.Font.Style := [fsBold]
          else
            Canvas.Font.Style := [];
          DefaultDrawColumnCell(Rect, DataCol, Column, State);
        end;
      end;
end;
constructor TfOrderFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDataSet := nil;
  FPosFrame := TfPosition.Create(Self);
  FPosFrame.Parent := pPosition;
  FPosFrame.Align:=alClient;
  FPosFrame.Show;
  FPosFrame.InplaceFrames[0] := TfDetailPositionFrame.Create(FPosFrame);
  FPosFrame.InplaceFrames[1] := TfDetailPositionFrame.Create(FPosFrame);
  FPosFrame.InplaceFrames[2] := TfDetailPositionFrame.Create(FPosFrame);
  FPosFrame.InplaceFrames[3] := TfTextPositionFrame.Create(FPosFrame);
  FPosFrame.InplaceFrames[5] := TfRepairPositionFrame.Create(FPosFrame);
  FPosFrame.InplaceFrames[6] := TfQSPositionFrame.Create(FPosFrame);
  FPosFrame.InplaceFrames[8] := TfRepairImageFrame.Create(FPosFrame);
  FPosFrame.InplaceFrames[9] := TfDetailPositionFrame.Create(FPosFrame);
  FPosFrame.FormName:='ORD';
  PreviewFrame := TfPreview.Create(Self);
  PreviewFrame.Parent := pPreviewT;
  PreviewFrame.Align := alClient;
  PreviewFrame.Show;
  {$ifdef DARWIN}
  cbStatus.Style:=csDropdown;
  {$endif}
end;
destructor TfOrderFrame.Destroy;
begin
  pcHeader.CloseAll;
  FPosFrame.Free;
  PreviewFrame.Free;
  if Assigned(FDataSet) then
    begin
      DataSet.Destroy;
      DataSet := nil;
    end;
  inherited Destroy;
end;
function TfOrderFrame.OpenFromLink(aLink: string): Boolean;
begin
  inherited;
  if not (copy(aLink,0,6) = 'ORDERS') then exit;
  if rpos('{',aLink) > 0 then
    aLink := copy(aLink,0,rpos('{',aLink)-1)
  else if rpos('(',aLink) > 0 then
    aLink := copy(aLink,0,rpos('(',aLink)-1);
  FPosFrame.Dataset:=nil;
  CloseConnection(acSave.Enabled);
  OpenConnection;
  FreeAndNil(FDataSet);
  DataSet := TOrder.CreateEx(Self,Data,FConnection);
  DataSet.OnChange:=@OrdersStateChange;
  TOrder(DataSet).SelectFromLink(aLink);
  DataSet.Open;
  Result := DataSet.Count>0;
  if Result then
    DoOpen(False);
end;
procedure TfOrderFrame.New;
begin
  New('');
end;
procedure TfOrderFrame.New(aStatus: string);
var
  aId : Variant;
begin
  inherited New;
  FreeAndNil(FDataSet);
  DataSet := TOrder.CreateEx(Self,Data,FConnection);
  TOrder(DataSet).OrderType.Open;
  DataSet.OnChange:=@OrdersStateChange;
  if aStatus <> '' then
    if not TOrder(DataSet).OrderType.DataSet.Locate('STATUS',aStatus,[]) then exit;
  if TOrder(DataSet).OrderType.FieldByName('SI_PROD').AsString='Y' then
    begin
      if not fCreateProductionOrder.Execute(TOrder(DataSet),'','') then
        begin
          acClose.Execute;
          exit;
        end;
      aId := TOrder(DataSet).Id.AsVariant;
      TOrder(DataSet).Free;
      DataSet := TOrder.CreateEx(Self,Data,FConnection);
      DataSet.OnChange:=@OrdersStateChange;
      DataSet.Select(aId);
      DataSet.Open;
    end
  else
    DataSet.Insert;
  DoOpen(False);
  acSave.Enabled := False;
  acCancel.Enabled:= False;
end;
procedure TfOrderFrame.SetLanguage;
var
  i: Integer;
begin
  for i := 0 to pAddresses.ControlCount-1 do
    TfOrderAddress(pAddresses.Controls[i]).SetLanguage;
  FPosFrame.SetLanguage;
  if not Data.PaymentTargets.DataSet.Active then
    Data.PaymentTargets.Open;
  cbPaymentTarget.Clear;
  with Data.PaymentTargets.DataSet do
    begin
      First;
      while not EOF do
        begin
          cbPaymentTarget.Items.Add(Data.PaymentTargets.FieldByName('TEXT').AsString);
          Next;
        end;
    end;
end;
initialization
//  TBaseVisualApplication(Application).RegisterForm(TfOrderFrame);
end.

