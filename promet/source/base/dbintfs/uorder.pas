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
unit uOrder;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, uBaseDbClasses, db, uBaseApplication,
  uBaseERPDBClasses, uMasterdata, uPerson, Variants, uAccounting;
type
  TOrderTyp = class(TBaseDBDataSet)
  public
    procedure DefineFields(aDataSet : TDataSet);override;
  end;
  TOrderList = class(TBaseERPList,IBaseHistory)
  private
    FHistory : TBaseHistory;
    FOrderTyp: TOrdertyp;
    function GetHistory: TBaseHistory;
  protected
    function GetTextFieldName: string;override;
    function GetNumberFieldName : string;override;
    function GetBookNumberFieldName : string;override;
    function GetStatusFieldName : string;override;
    function GetCommissionFieldName: string;override;
  public
    constructor Create(aOwner: TComponent; DM: TComponent; aConnection: TComponent=nil;
      aMasterdata: TDataSet=nil); override;
    destructor Destroy; override;
    function GetStatusIcon: Integer; override;
    procedure Open; override;
    procedure DefineFields(aDataSet : TDataSet);override;
    property History : TBaseHistory read FHistory;
    property OrderType : TOrdertyp read FOrderTyp;
  end;
  TOrderQMTestDetails = class(TBaseDBDataSet)
  public
    procedure DefineFields(aDataSet : TDataSet);override;
  end;
  TOrderQMTest = class(TBaseDBDataSet)
  private
    FDetails: TOrderQMtestDetails;
  public
    constructor Create(aOwner : TComponent;DM : TComponent;aConnection : TComponent = nil;aMasterdata : TDataSet = nil);override;
    destructor Destroy;override;
    function CreateTable : Boolean;override;
    procedure DefineFields(aDataSet : TDataSet);override;
    property Details : TOrderQMtestDetails read FDetails;
  end;
  TOrder = class;
  TRepairProblems = class(TBaseDBDataSet)
    procedure DefineFields(aDataSet : TDataSet);override;
  end;
  TOrderRepairDetail = class(TBaseDbDataSet)
    procedure DefineFields(aDataSet : TDataSet);override;
  end;
  TOrderRepair = class(TBaseDBDataSet)
  private
    FDetails: TOrderRepairDetail;
  public
    procedure DefineFields(aDataSet : TDataSet);override;
    procedure FillDefaults(aDataSet : TDataSet);override;
    constructor Create(aOwner : TComponent;DM : TComponent;aConnection : TComponent = nil;aMasterdata : TDataSet = nil);override;
    destructor Destroy;override;
    function CreateTable : Boolean;override;
    property Details : TOrderRepairDetail read FDetails;
  end;
  TOrderPos = class(TBaseDBPosition)
  private
    FOrder: TOrder;
    FOrderRepair: TOrderRepair;
    FQMTest: TOrderQMTest;
  protected
    function GetAccountNo : string;override;
    procedure PosPriceChanged(aPosDiff,aGrossDiff :Extended);override;
    procedure PosWeightChanged(aPosDiff : Extended);override;
    function GetCurrency : string;override;
    function GetOrderTyp : Integer;
  public
    constructor Create(aOwner : TComponent;DM : TComponent;aConnection : TComponent = nil;aMasterdata : TDataSet = nil);override;
    destructor Destroy;override;
    function CreateTable : Boolean;override;
    procedure Assign(aSource : TPersistent);override;
    procedure DefineFields(aDataSet : TDataSet);override;
    procedure FillDefaults(aDataSet : TDataSet);override;
    property QMTest : TOrderQMTest read FQMTest;
    property Order : TOrder read FOrder write FOrder;
    property Repair : TOrderRepair read FOrderRepair;
  end;
  TOrderAddress = class(TBaseDBAddress)
  private
    FOrder: TOrder;
  public
    constructor Create(aOwner: TComponent; DM: TComponent;
       aConnection: TComponent=nil; aMasterdata: TDataSet=nil); override;
    procedure DefineFields(aDataSet : TDataSet);override;
    procedure CascadicPost; override;
    procedure Assign(Source: TPersistent); override;
    procedure Post; override;
    property Order : TOrder read FOrder write FOrder;
  end;
  TOrderPosTyp = class(TBaseDBDataSet)
  public
    procedure DefineFields(aDataSet : TDataSet);override;
  end;
  TDispatchTypes = class(TBaseDBDataSet)
  public
    procedure DefineFields(aDataSet : TDataSet);override;
    procedure SelectByCountryAndWeight(aCountry : string;aWeight : real);
  end;
  TPaymentTargets = class(TBaseDBDataSet)
    procedure DefineFields(aDataSet : TDataSet);override;
  end;
  TOrderLinks = class(TLinks)
  public
    procedure FillDefaults(aDataSet : TDataSet);override;
  end;
  TOnGetStorageEvent = function(Sender : TOrder;aStorage : TStorage) : Boolean of object;
  TOnGetSerialEvent = function(Sender : TOrder;aMasterdata : TMasterdata;aQuantity : Integer) : Boolean of object;

  { TOrder }

  TOrder = class(TOrderList,IPostableDataSet,IShipableDataSet)
  private
    FFailMessage: string;
    FLinks: TOrderLinks;
    FOnGetSerial: TOnGetSerialEvent;
    FOnGetStorage: TOnGetStorageEvent;
    FOrderAddress: TOrderAddress;
    FOrderPos: TOrderPos;
    fOrigId : string;
    function GetCommission: TField;
  public
    constructor Create(aOwner : TComponent;DM : TComponent;aConnection : TComponent = nil;aMasterdata : TDataSet = nil);override;
    destructor Destroy;override;
    function CreateTable : Boolean;override;
    procedure FillDefaults(aDataSet : TDataSet);override;
    procedure Select(aID : string);overload;
    procedure Open;override;
    procedure RefreshActive;
    procedure CascadicPost;override;
    procedure CascadicCancel;override;
    property Commission : TField read GetCommission;
    property Address : TOrderAddress read FOrderAddress;
    property Positions : TOrderPos read FOrderPos;
    property Links : TOrderLinks read FLinks;
    property OnGetStorage : TOnGetStorageEvent read FOnGetStorage write FOnGetStorage;
    property OnGetSerial : TOnGetSerialEvent read FOnGetSerial write FOnGetSerial;
    procedure Recalculate;
    function DoPost: TPostResult;
    function FailMessage : string;
    function ChangeStatus(aNewStatus : string) : Boolean;
    procedure ShippingOutput;
    function PostArticle(aTyp, aID, aVersion, aLanguage: variant; Quantity: real; QuantityUnit, PosNo: string; var aStorage: string; var OrderDelivered: boolean) : Boolean;
    function DoBookPositionCalc(AccountingJournal : TAccountingJournal) : Boolean;
    function FormatCurrency(Value : real) : string;
    function CalcDispatchType : Boolean;
  end;
implementation
uses uBaseDBInterface, uBaseSearch, uData, Process, UTF8Process,uRTFtoTXT;
resourcestring
  strStatusnotfound             = 'Statustyp nicht gefunden, bitte wenden Sie sich an Ihren Administrator';
  strMainOrdernotfound          = 'Hauptvorgang nicht gefunden !';
  strNumbersetnotfound          = 'Nummernkreis nicht gefunden, bitte wenden Sie sich an Ihren Administrator';
  strBookingAborted             = 'Buchung abgebrochen';
  strOrderActionPost            = 'Vorgang als %s gebucht';
  strOrderPosted                = 'Vorgang gebucht';
  strOrders                     = 'Aufträge';
  strAlreadyPosted              = 'Der Vorgang ist bereits gebucht !';
  strDispatchTypenotfound       = 'Die gewählte Versandart existiert nicht !';

procedure TOrderRepairDetail.DefineFields(aDataSet: TDataSet);
begin
  with aDataSet as IBaseManageDB do
    begin
      TableName := 'ORDERREPAIRDETAIL';
      if Assigned(ManagedFieldDefs) then
        with ManagedFieldDefs do
          begin
            Add('ASSEMBLY',ftString,60,False);
            Add('PART',ftString,60,False);
            Add('ERROR',ftString,120,False);
          end;
    end;
end;
procedure TOrderRepair.DefineFields(aDataSet: TDataSet);
begin
  with aDataSet as IBaseManageDB do
    begin
      TableName := 'ORDERREPAIR';
      if Assigned(ManagedFieldDefs) then
        with ManagedFieldDefs do
          begin
            Add('OPERATION',ftString,20,False);
            Add('ERRDESC',ftMemo,0,False);
            Add('NOTES',ftMemo,0,False);
            Add('INTNOTES',ftMemo,0,False);
            Add('WARRENTY',ftString,1,True);
          end;
    end;
end;
procedure TOrderRepair.FillDefaults(aDataSet: TDataSet);
begin
  with aDataSet,BaseApplication as IBaseDbInterface do
    begin
      FieldByName('WARRENTY').AsString := 'U';
    end;
  inherited FillDefaults(aDataSet);
end;
constructor TOrderRepair.Create(aOwner: TComponent; DM: TComponent;
  aConnection: TComponent; aMasterdata: TDataSet);
begin
  inherited Create(aOwner, DM, aConnection, aMasterdata);
  FDetails := TOrderRepairDetail.Create(Owner,DM,aConnection,DataSet);
end;
destructor TOrderRepair.Destroy;
begin
  FDetails.Destroy;
  inherited Destroy;
end;
function TOrderRepair.CreateTable : Boolean;
begin
  Result := inherited CreateTable;
  FDetails.CreateTable;
end;
procedure TOrderPosTyp.DefineFields(aDataSet: TDataSet);
begin
  with aDataSet as IBaseManageDB do
    begin
      TableName := 'ORDERPOSTYP';
      if Assigned(ManagedFieldDefs) then
        with ManagedFieldDefs do
          begin
            Add('NAME',ftString,3,True);
            Add('TYPE',ftString,1,True);
          end;
    end;
end;
procedure TOrderLinks.FillDefaults(aDataSet: TDataSet);
begin
  inherited FillDefaults(aDataSet);
  aDataSet.FieldByName('RREF_ID').AsVariant:=(Parent as TOrder).Id.AsVariant;
end;

constructor TOrderAddress.Create(aOwner: TComponent; DM: TComponent;
  aConnection: TComponent; aMasterdata: TDataSet);
begin
  inherited Create(aOwner, DM, aConnection, aMasterdata);
end;

procedure TOrderAddress.DefineFields(aDataSet: TDataSet);
begin
  inherited DefineFields(aDataSet);
  with aDataSet as IBaseManageDB do
    begin
      TableName := 'ORDERADDR';
      if Assigned(ManagedFieldDefs) then
        with ManagedFieldDefs do
          begin
            Add('ACCOUNTNO',ftString,20,False);
          end;
    end;
end;

procedure TOrderAddress.CascadicPost;
begin
  Order.DataSet.DisableControls;
  if Order.Address.DataSet.Active and (Order.Address.Count>0) then
    begin
      if Order.FieldByName('CUSTNO').AsString<>FieldByName('ACCOUNTNO').AsString then
        begin
          if not Order.CanEdit then
            Order.DataSet.Edit;
          Order.FieldByName('CUSTNO').AsString := FieldByName('ACCOUNTNO').AsString;
        end;
      if Order.FieldByName('CUSTNAME').AsString<>FieldByName('NAME').AsString then
        begin
          if not Order.CanEdit then
            Order.DataSet.Edit;
          Order.FieldByName('CUSTNAME').AsString := FieldByName('NAME').AsString;
        end;
    end;
  Order.DataSet.EnableControls;
  inherited CascadicPost;
end;

procedure TOrderAddress.Assign(Source: TPersistent);
var
  aAddress: TBaseDbAddress;
  Person: TPerson;
begin
  if not Order.CanEdit then
    Order.DataSet.Edit;
  inherited Assign(Source);
  if Source is TBaseDBAddress then
    begin
      aAddress := Source as TBaseDbAddress;
      Order.FieldByName('CUSTNAME').AsString := aAddress.FieldByName('NAME').AsString;
    end
  else if Source is TPerson then
    begin
      Person := Source as TPerson;
      if not Order.CanEdit then
        Order.DataSet.Edit;
      Order.FieldByName('CUSTNO').AsString := Person.FieldByName('ACCOUNTNO').AsString;
    end;
end;

procedure TOrderAddress.Post;
begin
  inherited Post;
end;

procedure TRepairProblems.DefineFields(aDataSet: TDataSet);
begin
  with aDataSet as IBaseManageDB do
    begin
      TableName := 'REPAIRPROBLEMS';
      if Assigned(ManagedFieldDefs) then
        with ManagedFieldDefs do
          begin
            Add('PROBLEM',ftString,60,True);
          end;
    end;
end;
procedure TPaymentTargets.DefineFields(aDataSet: TDataSet);
begin
  with aDataSet as IBaseManageDB do
    begin
      TableName := 'PAYMENTTARGETS';
      if Assigned(ManagedFieldDefs) then
        with ManagedFieldDefs do
          begin
            Add('ID',ftString,2,True);
            Add('NAME',ftString,10,True);
            Add('TEXT',ftString,30,True);
            Add('FACCOUNTS',ftMemo,0,false);
            Add('CASHDISC',ftFloat,0,True);               //Skonto
            Add('CASHDISCD',ftInteger,0,True);            //Skonto Tage
            Add('DAYS',ftInteger,0,True);                 //Tage
            Add('DEFAULTPT',ftString,1,True);
          end;
    end;
end;
procedure TDispatchTypes.DefineFields(aDataSet: TDataSet);
begin
  with aDataSet as IBaseManageDB do
    begin
      TableName := 'DISPATCHTYPES';
      UpdateFloatFields:=True;
      if Assigned(ManagedFieldDefs) then
        with ManagedFieldDefs do
          begin
            Add('ID',ftString,3,True);
            Add('COUNTRY',ftString,3,True);
            Add('NAME',ftString,20,false);
            Add('OUTPUTDRV',ftString,60,false);
            Add('WEIGHT',ftFloat,0,false);
            Add('ARTICLE',ftString,40,false);
          end;
    end;
end;
procedure TDispatchTypes.SelectByCountryAndWeight(aCountry: string;aWeight : real);
var
  aFilter: String;
begin
  with  DataSet as IBaseDBFilter, BaseApplication as IBaseDBInterface, DataSet as IBaseManageDB do
    begin
      Filter := '('+QuoteField('COUNTRY')+'='+QuoteValue(aCountry)+') AND ('+QuoteField('WEIGHT')+'>='+QuoteValue(FloatToStr(aWeight))+')';
      SortFields:='WEIGHT';
    end;
end;
procedure TOrderTyp.DefineFields(aDataSet: TDataSet);
begin
  with aDataSet as IBaseManageDB do
    begin
      TableName := 'ORDERTYPE';
      if Assigned(ManagedFieldDefs) then
        with ManagedFieldDefs do
          begin
            Add('STATUS',ftString,4,True);
            Add('STATUSNAME',ftString,30,True);
            Add('TYPE',ftString,1,false);
            Add('ICON',ftInteger,0,false);
            Add('ISDERIVATE',ftString,1,false);
            Add('DERIVATIVE',ftString,30,false);
            Add('NUMBERSET',ftString,30,false);
            Add('DEFPOSTYP',ftString,3,False);
            Add('TEXTTYP',ftInteger,0,False);
            Add('SI_ORDER',ftString,1,false);  //im Auftrag anzeigen
            Add('SI_POS',ftString,1,false);    //in der Kasse anzeigen (Point of Sale)
            Add('SI_PROD',ftString,1,false);   //in der Produktion anzeigen
            Add('SI_ACC',ftString,1,false);    //in der Fibu anzeigen (Accounting)
            Add('SI_INVR',ftString,1,false);   //im Rechnungseingang anzeigen (Invoice Receipt)
            Add('SI_INVO',ftString,1,false);   //im Rechnungsausgang anzeigen (Outgoing Invoice)

            Add('B_STORAGE',ftString,1,false); //Lagerbuchung                 (+ 0 -)
            Add('B_RESERVED',ftString,1,false);//Lagerbuchung Reserviert      (+ 0 -)
            Add('B_STORDER',ftString,1,false); //Lagereintrag im Hauptvorgang (+ 0 -)
            Add('B_JOURNAL',ftString,1,false); //Kassenbuch                   (+ 0 -)
            Add('B_SERIALS',ftString,1,false); //Serienummerverwaltung        (+ 0 -)
            Add('B_INVR',ftString,1,false);    //Rechnungseingang             (+ 0 -)
            Add('B_INVO',ftString,1,false);    //Rechnungsausgang             (+ 0 -)
            Add('B_DUNNING',ftString,1,false); //Mahnwesen                    (+ 0 -)
            Add('B_CHIST',ftString,1,false);   //Kundenhistorie               (+ 0)
          end;
    end;
end;
procedure TOrderQMTestDetails.DefineFields(aDataSet: TDataSet);
begin
  with aDataSet as IBaseManageDB do
    begin
      TableName := 'ORDERQMTESTDETAIL';
      if Assigned(ManagedFieldDefs) then
        with ManagedFieldDefs do
          begin
            Add('STEP',ftInteger,0,False);
            Add('EXPECTED',ftMemo,0,False);
            Add('RESULT',ftMemo,0,False);
            Add('RESULTSHORT',ftString,1,False);
          end;
    end;
end;
constructor TOrderQMTest.Create(aOwner: TComponent; DM : TComponent;aConnection: TComponent;
  aMasterdata: TDataSet);
begin
  inherited Create(aOwner, DM, aConnection, aMasterdata);
  FDetails := TOrderQMTestDetails.Create(aOwner,DM,aConnection,DataSet);
end;
destructor TOrderQMTest.Destroy;
begin
  FDetails.Free;
  inherited Destroy;
end;
function TOrderQMTest.CreateTable : Boolean;
begin
  Result := inherited CreateTable;
  FDetails.CreateTable;
end;
procedure TOrderQMTest.DefineFields(aDataSet: TDataSet);
begin
  with aDataSet as IBaseManageDB do
    begin
      TableName := 'ORDERQMTEST';
      if Assigned(ManagedFieldDefs) then
        with ManagedFieldDefs do
          begin
            Add('ID',ftLargeint,0,False);
            Add('NAME',ftString,20,False);
            Add('RESULT',ftString,1,False);
            Add('NOTES',ftMemo,0,False);
            Add('RAWDATA',ftMemo,0,False);
            Add('TESTTIME',ftDateTime,0,False);
            Add('TESTEND',ftDateTime,0,False);
          end;
    end;
end;

function TOrder.GetCommission: TField;
begin
  result := FieldByName('COMMISSION');
end;

constructor TOrder.Create(aOwner: TComponent; DM : TComponent;aConnection: TComponent;
  aMasterdata: TDataSet);
begin
  inherited Create(aOwner,DM, aConnection, aMasterdata);
  UpdateFloatFields:=True;
  with BaseApplication as IBaseDbInterface do
    begin
      with DataSet as IBaseDBFilter do
        begin
          BaseSortFields := 'ORDERNO';
          SortFields := 'ORDERNO';
          SortDirection := sdAscending;
          UsePermissions:=False;
        end;
    end;
  FOrderAddress := TOrderAddress.Create(Self,DM,aConnection,DataSet);
  FOrderAddress.Order := Self;
  FOrderPos := TOrderPos.Create(Self,DM,aConnection,DataSet);
  FOrderPos.Order:=Self;
  FLinks := TOrderLinks.Create(Self,DM,aConnection);
end;
destructor TOrder.Destroy;
begin
  FOrderAddress.Destroy;
  FOrderPos.Destroy;
  FreeAndnil(FLinks);
  inherited Destroy;
end;
function TOrder.CreateTable : Boolean;
begin
  Result := inherited CreateTable;
  FOrderAddress.CreateTable;
  FOrderPos.CreateTable;
  FOrderTyp.CreateTable;
  FHistory.CreateTable;
end;
procedure TOrder.FillDefaults(aDataSet: TDataSet);
begin
  with aDataSet,BaseApplication as IBaseDBInterface do
    begin
      FieldByName('ORDERNO').AsString := Data.Numbers.GetNewNumber('ORDERS') + '00';
      FieldByName('STATUS').AsString  := OrderType.FieldByName('STATUS').AsString;
      Data.SetFilter(Data.Currency,Data.QuoteField('DEFAULTCUR')+'='+Data.QuoteValue('Y'));
      if Data.Currency.Count > 0 then
        FieldByName('CURRENCY').AsString := Data.Currency.FieldByName('SYMBOL').AsString;
      Data.PaymentTargets.Open;
      if Data.PaymentTargets.DataSet.Locate('DEFAULTPT', 'Y', []) then
        FieldByName('PAYMENTTAR').AsString := Data.PaymentTargets.FieldByName('ID').AsString;
      FieldByName('ACTIVE').AsString := 'Y';
      FieldByName('DOAFQ').AsDateTime := Now();
      FieldByName('VATH').AsFloat     := 0;
      FieldByName('VATF').AsFloat     := 0;
      FieldByName('NETPRICE').AsFloat := 0;
      FieldByName('DISCOUNT').AsFloat := 0;
      FieldByName('GROSSPRICE').AsFloat := 0;
      FieldByName('DONE').AsString    := 'N';
      FieldByName('DELIVERED').AsString := 'N';
      FieldByName('CREATEDBY').AsString := Data.Users.IDCode.AsString;
    end;
end;
procedure TOrder.Select(aID : string);
var
  aFilter: String;
begin
  with  DataSet as IBaseDBFilter, BaseApplication as IBaseDBInterface, DataSet as IBaseManageDB do
    begin
      if length(aID) > 4 then
        begin
          aFilter :=         '('+QuoteField('ORDERNO')+'>='+QuoteValue(copy(aID,0,length(aID)-2)+'00')+') and ';
          aFilter := aFilter+'('+QuoteField('ORDERNO')+'<='+QuoteValue(copy(aID,0,length(aID)-2)+'99')+')';
        end
      else
        aFilter :=  QuoteField('ORDERNO')+'='+QuoteValue(aID);
      Filter := aFilter;
      FOrigID := aID;
      Limit := 99;
    end;
end;
procedure TOrder.Open;
begin
  if not Assigned(DataSet) then exit;
  if DataSet.Active then exit;
  inherited Open;
  DataSet.Locate('ORDERNO',FOrigID,[]);
  OrderType.Open;
  OrderType.DataSet.Locate('STATUS',DataSet.FieldByName('STATUS').AsString,[]);
  if FieldByName('ACTIVE').IsNull then
    RefreshActive;
end;

procedure TOrder.RefreshActive;
var
  aRec: TBookmark;
  Found: Boolean = False;
begin
  if not DataSet.Active then exit;
  aRec := DataSet.GetBookmark;
  DataSet.Last;
  OrderType.Open;
  while not DataSet.BOF do
    begin
      OrderType.DataSet.Locate('STATUS',DataSet.FieldByName('STATUS').AsString,[]);
      if (OrderType.FieldByName('ISDERIVATE').AsString<>'Y')
      and not Found then
        begin
          if DataSet.FieldByName('ACTIVE').AsString<>'Y' then
            begin
              if not CanEdit then DataSet.Edit;
              DataSet.FieldByName('ACTIVE').AsString := 'Y';
              DataSet.Post;
            end;
          Found := True;
        end
      else
        begin
          if DataSet.FieldByName('ACTIVE').AsString<>'N' then
            begin
              if not CanEdit then DataSet.Edit;
              DataSet.FieldByName('ACTIVE').AsString := 'N';
              DataSet.Post;
            end;
        end;
      DataSet.Prior;
    end;
  DataSet.GotoBookmark(aRec);
  DataSet.FreeBookmark(aRec);
  OrderType.DataSet.Locate('STATUS',DataSet.FieldByName('STATUS').AsString,[]);
end;

procedure TOrder.CascadicPost;
begin
  Recalculate;
  FOrderAddress.CascadicPost;
  FHistory.CascadicPost;
  FOrderPos.CascadicPost;
  FLinks.CascadicPost;
  inherited CascadicPost;
end;
procedure TOrder.CascadicCancel;
begin
  FHistory.CascadicCancel;
  FOrderAddress.CascadicCancel;
  FOrderPos.CascadicCancel;
  FLinks.CascadicCancel;
  inherited CascadicCancel;
end;

procedure TOrder.Recalculate;
var
  aPos: Double = 0;
  aGrossPos: Double = 0;
  aVatH : Double = 0;
  aVatV : Double = 0;
begin
  Positions.Open;
  Positions.First;
  while not Positions.EOF do
    begin
      if  (Positions.PosTypDec<>4)  //Only Positions that should calculated
      and (Positions.PosTypDec<>1)
      and (Positions.PosTypDec<>2)
      and (Positions.PosTypDec<>3)
      and (Positions.PosTypDec<>5)
      then
        begin
          aPos += Positions.FieldByName('POSPRICE').AsFloat;
          aGrossPos += Positions.FieldByName('GROSSPRICE').AsFloat;
          with BaseApplication as IBaseDbInterface do
            begin
              if not Data.Vat.DataSet.Active then
                Data.Vat.Open;
              Data.Vat.DataSet.Locate('ID',VarArrayof([Positions.FieldByName('VAT').AsString]),[]);
              if Data.Vat.FieldByName('ID').AsInteger=1 then
                aVatV += Positions.FieldByName('GROSSPRICE').AsFloat-Positions.FieldByName('POSPRICE').AsFloat
              else
                aVatH += Positions.FieldByName('GROSSPRICE').AsFloat-Positions.FieldByName('POSPRICE').AsFloat;
            end;
        end;
          if Positions.PosTypDec=4 then //Subtotal
            begin
              Positions.Edit;
              Positions.FieldByName('POSPRICE').AsFloat := aPos;
              Positions.FieldByName('GROSSPRICE').AsFloat := aGrossPos;
              Positions.Post;
            end;
      Positions.Next;
    end;
  if not CanEdit then DataSet.Edit;
  FieldByName('VATH').AsFloat:=aVatH;
  FieldByName('VATF').AsFloat:=aVatV;
  FieldByName('NETPRICE').AsFloat:=aPos;
  FieldByName('NETPRICE').AsFloat:=aPos;
  FieldByName('GROSSPRICE').AsFloat:=InternalRound(aGrossPos);
  if CanEdit then DataSet.Post;
end;

function TOrder.DoPost: TPostResult;
var
  Orders: TOrderList;
  Accountingjournal: TAccountingJournal = nil;
  MasterdataList: TMasterdataList = nil;
  Person: TPerson = nil;
  MainOrder: TOrder = nil;

  MainOrderId: LargeInt = 0;
  OrderTyp: Integer;
  OrderDone: Boolean;
  OrderDelivered: Boolean;
  aStorage: String;
  F_Date: Double;
  F_POSNO: String;
  F_QUANTITY: Double;
  aState: TDataSetState;
  aNumbers: TNumbersets = nil;
  aMainOrder: TOrder;
begin
  FFailMessage:='';
  Result := prFailed;
  CascadicPost;
  Recalculate;
  Orders := TOrderList.Create(Owner,DataModule,Connection);
  MasterdataList := TMasterdataList.Create(Owner,DataModule,Connection);
  MainOrder := TOrder.Create(Owner,DataModule,Connection);
  MainOrder.Select(Self.FieldByName('ORDERNO').AsString);
  MainOrder.Open;
  Data.StorageType.Open;
  while ((MainOrder.OrderType.FieldByName('ISDERIVATE').AsString = 'Y') or (MainOrder.FieldByName('NUMBER').IsNull)) and (not MainOrder.DataSet.BOF) do
    begin
      MainOrder.DataSet.Prior;
      if not MainOrder.OrderType.Dataset.Locate('STATUS',MainOrder.FieldByName('STATUS').AsString, [loCaseInsensitive]) then
        break;
    end;
  if (MainOrder.OrderType.FieldByName('ISDERIVATE').AsString <> 'Y') and (not MainOrder.FieldByName('NUMBER').IsNull) then
    begin
      MainOrderId := MainOrder.GetBookmark;
      MainOrder.Positions.Open;
    end
  else
    FreeAndNil(MainOrder);
  if not OrderType.DataSet.Locate('STATUS', DataSet.FieldByName('STATUS').AsString, [loCaseInsensitive]) then
    raise Exception.Create(strStatusnotfound);
  OrderTyp := StrToIntDef(trim(copy(OrderType.FieldByName('TYPE').AsString, 0, 2)), 0);
  Address.Open;
  if OrderTyp = 2 then
    Address.DataSet.Locate('TYPE', 'DAD', [])
  else
    Address.DataSet.Locate('TYPE', 'IAD', []);
  //Prüfung bereits gebucht ?
  if not (DataSet.FieldByName('NUMBER').IsNull) then
    begin
      Result := pralreadyPosted;
    end;
  if (result <> pralreadyPosted) or BaseApplication.HasOption('e','editall') then
    begin
      //Überprüfen ob Nummernkreis existent
      with Data.Numbers.DataSet as IBaseDBFilter do
        Filter := Data.QuoteField('TABLENAME')+'='+Data.QuoteValue(OrderType.FieldByName('NUMBERSET').AsString);
      Data.Numbers.Open;
      if Data.Numbers.Count = 0 then
        raise Exception.Create(strNumbersetnotfound);
      Accountingjournal := TAccountingjournal.Create(Owner,DataModule,Connection);
      Accountingjournal.CreateTable;
      Data.StartTransaction(Connection,True);
      try
        OrderDone      := True;
        OrderDelivered := True;
        with Accountingjournal.DataSet as IBaseDBFilter do
          begin
            Data.SetFilter(Accountingjournal,
            Data.ProcessTerm(Data.QuoteField('ORDERNO')+'='+Data.QuoteValue(DataSet.FieldByName('ORDERNO').AsString))
            );
          end;
        while Accountingjournal.Count > 1 do
          begin
            Accountingjournal.DataSet.Last;
            Accountingjournal.Delete;
          end;
        //Belegnummer und Datum vergeben
        DataSet.Edit;
        aNumbers := TNumberSets.Create(Owner,Data,Connection);
        with aNumbers.DataSet as IBaseDBFilter do
          Filter := Data.QuoteField('TABLENAME')+'='+Data.QuoteValue(OrderType.FieldByName('NUMBERSET').AsString);
        aNumbers.Open;
        if Result <> pralreadyPosted then
          begin
            DataSet.FieldByName('NUMBER').AsString := aNumbers.GetNewNumber(OrderType.FieldByName('NUMBERSET').AsString);
            DataSet.FieldByName('DATE').AsDateTime := Now();
          end;
        if DataSet.FieldByName('ODATE').IsNull then
          DataSet.FieldByName('ODATE').AsDateTime := Now();
        DataSet.Post;
        //Alle Positionen durchgehen
        Positions.DataSet.First;
        while not Positions.DataSet.EOF do
          begin
            //Auftragsmenge setzen falls Auftrag
            if OrderTyp = 1 then
              begin
                Positions.DataSet.Edit;
                Positions.FieldByName('QUANTITYO').AsFloat := Positions.FieldByName('QUANTITY').AsFloat;
                if Positions.CanEdit then
                  Positions.DataSet.Post;
              end;
            //Lager buchen
            //Ist aktuelle Position ein Artikel ?
            MasterdataList.Select(Positions.Ident.AsString,Positions.FieldByName('VERSION').AsVariant,Positions.FieldByName('LANGUAGE').AsVariant);
            MasterdataList.Open;
            if MasterdataList.Count > 0 then
              begin
                if ((MasterdataList.FieldByName('TYPE').AsString = 'A') or (MasterdataList.FieldByName('TYPE').AsString = 'P')) then
                  if Positions.FieldByName('QUANTITY').AsFloat <> 0 then
                    begin
                      if Positions.FieldByName('STORAGE').IsNull then
                        aStorage := trim(DataSet.FieldByName('STORAGE').AsString)
                      else
                        aStorage := trim(Positions.FieldByName('STORAGE').AsString);
                      if (MasterdataList.FieldByName('NOSTORAGE').AsString <> 'Y') then
                        if not  PostArticle(MasterdataList.FieldByName('TYPE').AsString,
                                            MasterdataList.FieldByName('ID').AsString,
                                            MasterdataList.FieldByName('VERSION').AsVariant,
                                            MasterdataList.FieldByName('LANGUAGE').AsVariant,
                                            Positions.FieldByName('QUANTITY').AsFloat,
                                            MasterdataList.FieldByName('QUANTITYU').AsString,
                                            Positions.FieldByName('POSNO').AsString,
                                            aStorage,
                                            OrderDelivered) then
                          begin
                            raise(Exception.Create(strBookingAborted));
                          end;
                      if aStorage <> Positions.FieldByName('STORAGE').AsString then
                        begin
                          Positions.DataSet.Edit;
                          Positions.FieldByName('STORAGE').AsString := aStorage;
                          if Positions.CanEdit then
                            Positions.DataSet.Post;
                        end;
                      //Bei Bestellungseingang Lieferzeit setzen
    //TODO:
    {
                      if (OrderTyp = 2) and
                        (OrderType.FieldByName('B_STORAGE').AsString = '+') then
                        if Data.Supplier.FieldByName('ACCOUNTNO').AsString = Data.OrderAddress.FieldByName('ACCOUNTNO').AsString then
                          begin
                            OldDeliverTime :=
                              Data.Supplier.FieldByName('DELIVERTM').AsInteger;
                            if F_Date <> 0 then
                              begin
                                Data.Supplier.DataSet.Edit;
                                Data.Supplier.FieldByName('DELIVERTM').AsInteger := (OldDeliverTime + trunc(Now() - F_Date)) div 2;
                                Data.Supplier.DataSet.Post;
                              end;
                          end;
    }
                    end;
                end;
            //Menge geliefert setzen
            F_Date := 0;
            F_POSNO := Positions.FieldByName('POSNO').AsString;
            if (not OrderType.FieldByName('B_STORDER').IsNull)
            and (OrderType.FieldByName('B_STORDER').AsString <> '0')
            and Assigned(MainOrder)then
              begin
                //Im Hauptvorgang menge geliefert buchen
                F_QUANTITY := Positions.FieldByName('QUANTITY').AsFloat;
                if (OrderType.FieldByName('B_STORDER').AsString = '-') then
                  F_QUANTITY := -F_QUANTITY
                else if (OrderType.FieldByName('B_STORDER').AsString <> '+') then
                  F_QUANTITY := 0;
                F_Date := DataSet.FieldByName('DATE').AsDateTime;
                if MainOrder.Positions.DataSet.Locate('POSNO', F_POSNO, [loCaseInsensitive]) then
                  with Mainorder.Positions.DataSet do
                    begin
                      Edit;
                      FieldByName('QUANTITYD').AsFloat := FieldByName('QUANTITYD').AsFloat + F_QUANTITY;
                      Post;
                      if FieldByName('QUANTITYD').AsFloat <
                         FieldByName('QUANTITY').AsFloat then
                        OrderDelivered := False;
                    end;
              end;
            //Wenn Rechnung, dann QUANTITYC im Hauptvorgang setzen
            if (OrderTyp = 3) and (OrderType.FieldByName('B_INVO').AsString <> '0') then
              begin
                F_QUANTITY := Positions.FieldByName('QUANTITY').AsFloat;
                if Assigned(MainOrder) then
                  begin
                    if MainOrder.Positions.DataSet.Locate('POSNO',F_POSNO, [loCaseInsensitive]) then
                      with MainOrder.Positions.DataSet do
                      begin
                        Edit;
                        FieldByName('QUANTITYC').AsFloat :=  FieldByName('QUANTITYC').AsFloat + F_QUANTITY;
                        Post;
                        if FieldByName('QUANTITYC').AsFloat < FieldByName('QUANTITY').AsFloat then
                          OrderDone := False;
                      end;
                  end;
              end
            else
              OrderDone := False;
            //Feld "Bestellt" buchen
            //Fibukonten buchen bzw splittbeträge für verschiedenen konten zusammenrechnen
            //und im Kassenbuch mit buchen

            DoBookPositionCalc(AccountingJournal);
            Positions.DataSet.Next;
          end;
        //Auftrag Fertig Flag setzen
        if OrderDone or OrderDelivered then
          begin
            if Assigned(MainOrder) then
              begin
                MainOrder.DataSet.Edit;
                if OrderDone then
                  MainOrder.FieldByName('DONE').AsString := 'Y';
                if OrderDelivered then
                begin
                  MainOrder.FieldByName('DELIVERED').AsString := 'Y';
                  MainOrder.FieldByName('DELIVEREDON').AsDateTime := Now();
                end;
                if MainOrder.CanEdit then
                  Mainorder.DataSet.Post;
              end;
          end;
        //Kundenhistorie buchen
        Person := TPerson.Create(Owner,DataModule,Connection);
        with Person.DataSet as IBaseDBFilter do
          Filter := Data.QuoteField('ACCOUNTNO')+'='+Data.QuoteValue(Address.FieldByName('ACCOUNTNO').AsString);
        Person.Open;
        if Person.Count > 0 then
          begin
            Person.History.Open;
            Person.History.AddItem(Person.DataSet,
                                   Format(strOrderActionPost, [DataSet.FieldByName('STATUS').AsString]),
                                   Data.BuildLink(Self.DataSet),
                                   DataSet.FieldByName('ORDERNO').AsString,
                                   DataSet,
                                   ACICON_ORDERPOSTED,
                                   DataSet.FieldByName('COMMISSION').AsString);
          end;
        //Vorgangsgeschichte Buchen
        if Self.CanEdit then DataSet.Post;
        History.Open;
        History.AddItem(Self.DataSet,strOrderPosted,'',DataSet.FieldByName('STATUS').AsString+' '+DataSet.FieldByName('NUMBER').AsString,nil,ACICON_ORDERPOSTED);
        //Auftragsansicht Neuprüfen
        Data.CommitTransaction(Connection);
        Positions.First;
        Result := prSuccess;
      except
        on e : Exception do
          begin
            Result := prFailed;
            Data.RollbackTransaction(Connection);
            FFailMessage := e.Message;
            //debugln(e.Message);
            DataSet.Refresh;
          end;
      end;
    end;
  FreeAndnil(Orders);
  FreeAndNil(MasterdataList);
  FreeAndNil(AccountingJournal);
  FreeAndNil(MainOrder);
  FreeAndNil(Person);
  FreeAndNil(aNumbers);
end;

function TOrder.FailMessage: string;
begin
  Result := FFailMessage;
end;

function TOrder.ChangeStatus(aNewStatus: string): Boolean;
var
  aOrderType: LongInt;
  Copied: String;
  newnumber: String;
  BM: LongInt;
  OldFilter: String;
  OldRec: Variant;
begin
  Result := False;
  if not OrderType.DataSet.Locate('STATUS',aNewStatus,[loCaseInsensitive]) then
    Data.SetFilter(OrderType,'');
  if OrderType.DataSet.Locate('STATUS',aNewStatus,[loCaseInsensitive]) then
    begin
      aOrderType := StrToIntDef(trim(copy(OrderType.FieldByName('TYPE').AsString,0,2)),0);
      if trim(OrderType.FieldByName('TYPE').AsString) = '' then
        exit;
      if not (OrderType.FieldByName('ISDERIVATE').AsString = 'Y') then
        begin
          DataSet.Edit;
          DataSet.FieldByName('DONE').AsString := 'Y';
          DataSet.Post;
        end;
      {$region}  //CopyOrder
      with DataSet as IBaseDBFilter do
        OldFilter := Filter;
      OldRec := GetBookmark;
      with DataSet as IBaseDBFilter do
        Filter := Data.QuoteField('ORDERNO')+'='+Data.QuoteValue(DataSet.FieldByName('ORDERNO').AsString);
      DataSet.Open;
      Copied := ExportToXML;
      Select(DataSet.FieldByName('ORDERNO').AsString);
      Open;
      if not GotoBookmark(OldRec) then
        begin
          raise Exception.Create('OldRec not found');
          exit;
        end;
      Positions.DisableCalculation;
      BM := GetBookmark;
      DataSet.Last;
      newnumber := copy(DataSet.FieldByName('ORDERNO').AsString,  length(DataSet.FieldByName('ORDERNO').AsString)-1,2);
      if NewNumber = '' then
        begin
          raise Exception.Create('NewNumber is NULL');
          exit;
        end;
      newnumber := copy(DataSet.FieldByName('ORDERNO').AsString,0,length(DataSet.FieldByName('ORDERNO').AsString)-2)+Format('%.2d',[StrToIntDef(newnumber,0)+1]);
      GotoBookmark(BM);
      with DataSet do
        begin
          Append;
          FieldByName('ORDERNO').AsString := newnumber;
          FieldByName('STATUS').AsString := aNewStatus;
          FieldByName('DOAFQ').Clear;
          FieldByName('DWISH').Clear;
          FieldByName('VATH').Clear;
          FieldByName('VATF').Clear;
          FieldByName('NETPRICE').Clear;
          FieldByName('DISCOUNT').Clear;
          FieldByName('GROSSPRICE').Clear;
        end;
      ImportFromXML(Copied);
      with DataSet do
        begin
          Edit;
          FieldByName('NUMBER').Clear;
          FieldByName('DATE').Clear;
          if aOrderType <> 3 then
            FieldByName('ODATE').Clear;
          Post;
        end;
      Positions.EnableCalculation;
      with Positions.DataSet do
        begin
          First;
          while not EOF do
            begin
              Edit;
              if aOrderType = 3 then
                begin
                  if FieldByName('QUANTITYO').IsNull then
                    FieldByName('QUANTITYO').AsFloat := FieldByName('QUANTITY').AsFloat;
                  if FieldByName('QUANTITYD').IsNull then
                    FieldByName('QUANTITY').AsFloat := FieldByName('QUANTITY').AsFloat-FieldByName('QUANTITYC').AsFloat
                  else
                    FieldByName('QUANTITY').AsFloat := FieldByName('QUANTITYD').AsFloat-FieldByName('QUANTITYC').AsFloat
                end
              else
                FieldByName('QUANTITY').AsFloat := FieldByName('QUANTITY').AsFloat-FieldByName('QUANTITYD').AsFloat;
              Post;
              Next;
            end;
        end;
      {$endregion}
      if not (OrderType.FieldByName('ISDERIVATE').AsString = 'Y') then
        begin //Auftrag geändert
          DataSet.Edit;
          DataSet.FieldByName('DONE').AsString := 'N';
          DataSet.Post;
        end;
      RefreshActive;
    end;
end;
procedure TOrder.ShippingOutput;
var
  OrderTyp: Integer;
  aProcess: TProcessUTF8;
  CommaCount: Integer;
  tmp: String;
begin
  if not OrderType.DataSet.Locate('STATUS', DataSet.FieldByName('STATUS').AsString, [loCaseInsensitive]) then
    raise Exception.Create(strStatusnotfound);
  OrderTyp := StrToIntDef(trim(copy(OrderType.FieldByName('TYPE').AsString, 0, 2)), 0);
  //Prüfung ob Versandart existiert
  if ((OrderTyp = 2) //Lieferschein
  or  (OrderTyp = 3)) //Rechnung
  then
    begin
      Data.SetFilter(Data.Dispatchtypes,Data.QuoteField('ID')+'='+Data.QuoteValue(trim(copy(DataSet.FieldByName('SHIPPING').AsString,0,3))));
      if not Data.Locate(Data.Dispatchtypes,'ID',copy(DataSet.FieldByName('SHIPPING').AsString,0,3),[loPartialKey]) then
        begin
          raise Exception.Create(strDispatchTypenotfound);
          exit;
        end;
    end;
  //Versandausgabe
  if  ((OrderTyp = 2) //Lieferschein
  or   (OrderTyp = 3)) //Rechnung
  then
    begin
      aProcess := TProcessUTF8.Create(Self);
      aProcess.ShowWindow := swoHide;
      aProcess.Options:= [poNoConsole,poWaitOnExit];
      aProcess.CommandLine := '"'+ExtractFileDir(ParamStr(0))+DirectorySeparator+'plugins'+DirectorySeparator;
      if pos(' ',Data.Dispatchtypes.FieldByName('OUTPUTDRV').AsString) > 0 then
        begin
          aProcess.CommandLine := aProcess.Commandline+copy(Data.Dispatchtypes.FieldByName('OUTPUTDRV').AsString,0,pos(' ',Data.Dispatchtypes.FieldByName('OUTPUTDRV').AsString)-1)+ExtractFileExt(Paramstr(0))+'"';
          aProcess.Commandline := aProcess.Commandline+copy(Data.Dispatchtypes.FieldByName('OUTPUTDRV').AsString,pos(' ',Data.Dispatchtypes.FieldByName('OUTPUTDRV').AsString)+1,length(Data.Dispatchtypes.FieldByName('OUTPUTDRV').AsString));
        end
      else aProcess.CommandLine := aProcess.Commandline+Data.Dispatchtypes.FieldByName('OUTPUTDRV').AsString+ExtractFileExt(Paramstr(0))+'"';
      CommaCount := 0;
      Address.Open;
      with Address.DataSet do
        begin
          tmp :=  ' --address="';
          if trim(FieldbyName('TITLE').AsString) <>'' then
            tmp := tmp+FieldbyName('TITLE').AsString+',';
          tmp := tmp+FieldbyName('NAME').AsString+',';
          if trim(FieldbyName('ADDITIONAL').AsString) <>'' then
            tmp := tmp+FieldbyName('ADDITIONAL').AsString+',';
          tmp := tmp+FieldbyName('ADDRESS').AsString+',';
          tmp := tmp+FieldbyName('COUNTRY').AsString+' '+FieldbyName('ZIP').AsString+' '+FieldbyName('CITY').AsString+'"';
        end;
      aProcess.Commandline := aProcess.Commandline+tmp;
      aProcess.Execute;
      aProcess.Free;
    end;
end;
function TOrder.PostArticle(aTyp, aID, aVersion, aLanguage: variant;
  Quantity: real; QuantityUnit, PosNo: string; var aStorage: string;
  var OrderDelivered: boolean): Boolean;
var
  OrderTyp: LongInt;
  Masterdata: TMasterdata;
  aQuantity : real = 0;
  aReserved : real = 0;
  aBooked: Real;
  aFirstStorage: Boolean;
begin
  with BaseApplication as IBaseDbInterface do
    begin
      try
      try
        Result := True;
        OrderTyp := StrToIntDef(trim(copy(OrderType.FieldByName('TYPE').AsString, 0, 2)), 0);
        Masterdata := TMasterdata.Create(Owner,DataModule,Connection);
        Masterdata.CreateTable;
        Masterdata.Select(aID,aVersion,aLanguage);
        Masterdata.Open;
        if Masterdata.DataSet.Locate('ID;VERSION;LANGUAGE', VarArrayOf([aID, aVersion, aLanguage]), []) then
          if (Masterdata.FieldByName('NOSTORAGE').AsString <> 'Y') then
            begin
              if (Masterdata.FieldByName('TYPE').AsString = 'P') and
                (Masterdata.FieldByName('PTYPE').AsString = 'L') then
                begin //BoM List we must book every Position
                  Masterdata.Positions.Open;
                  Masterdata.Positions.DataSet.First;
                  while not Masterdata.Positions.DataSet.EOF do
                    begin
                      PostArticle(aTyp, //TODO: Maybe an problem ???
                        Masterdata.Positions.FieldByName('IDENT').AsVariant,
                        Masterdata.Positions.FieldByName('VERSION').AsVariant,
                        Masterdata.Positions.FieldByName('LANGUAGE').AsVariant,
                        Masterdata.Positions.FieldByName('QUANTITY').AsFloat * Quantity,
                        Masterdata.Positions.FieldByName('QUANTITYU').AsString,
                        PosNo,
                        aStorage,
                        OrderDelivered);//or so ????  aNone);
                      Masterdata.Positions.DataSet.Next;
                    end;
                end
              else
                begin
                  if (OrderType.FieldByName('B_STORAGE').AsString = '+')
                  or (OrderType.FieldByName('B_STORAGE').AsString = '-')
                  or (OrderType.FieldByName('B_RESERVED').AsString ='+')
                  or (OrderType.FieldByName('B_RESERVED').AsString = '-')
                  then
                    begin
                      Masterdata.Storage.Open;
                      if OrderType.FieldByName('B_STORAGE').AsString = '+' then
                        begin
                          aQuantity:=Quantity;
                        end
                      else if OrderType.FieldByName('B_STORAGE').AsString = '-' then
                        begin
                          aQuantity:=-Quantity;
                        end;
                      if (OrderType.FieldByName('B_RESERVED').AsString = '+') and
                        (not ((OrderTyp = 7) and (Quantity > 0))) then
                        begin
                          aReserved:=Quantity;
                          OrderDelivered := False;
                        end
                      else if (OrderType.FieldByName('B_RESERVED').AsString = '-') and
                        (not ((OrderTyp = 7) and (Quantity > 0))) then
                      begin
                        aReserved:=-Quantity;
                      end;
                      aFirstStorage := True;
                      while aQuantity <> 0 do
                        begin
                          //Ist Lagerdatensatz gleich zu buchendes Lager
                          if (not Masterdata.Storage.Locate('STORAGEID', trim(copy(aStorage, 0, 3)), [loCaseInsensitive])) or (not aFirstStorage) or (Masterdata.FieldByName('USEBATCH').AsString='Y') then
                            if (not Data.StorageType.Locate('ID',trim(copy(aStorage, 0, 3)),[loCaseInsensitive])) or (not aFirstStorage) or (Masterdata.FieldByName('USEBATCH').AsString='Y') then
                              begin
                                if Masterdata.Storage.FieldByName('STORAGEID').AsString<>trim(copy(aStorage, 0, 3)) then
                                  Masterdata.Storage.Locate('STORAGEID', trim(copy(aStorage, 0, 3)), [loCaseInsensitive]);
                                if Assigned(FOnGetStorage) then
                                  if FOnGetStorage(Self,Masterdata.Storage) then
                                    aStorage := Masterdata.Storage.FieldByName('STORAGEID').AsString
                                  else raise Exception.Create('User aborted');
                              end;
                          aBooked := Masterdata.Storage.DoPost(OrderType,Self,aStorage,aQuantity,aReserved,QuantityUnit,PosNo);
                          if aBooked = 0 then
                            raise Exception.Create('Post Article failed');
                          aQuantity := aQuantity-aBooked;
                          aFirstStorage:=False;
                        end;

                    end
                  else
                    OrderDelivered := False;
                end;
            end;
      except
        on e : Exception do
          begin
            //debugln('Postarticle:'+e.Message);
            Result := False;
          end;
      end;
      finally
        Masterdata.Free;
      end;
    end;
end;
function TOrder.DoBookPositionCalc(AccountingJournal : TAccountingJournal): Boolean;
var
  aType : string;
  aOrderNo: String;
  Invert: Boolean;
begin
  Result := True;
  Invert := False;
  if (OrderType.FieldByName('B_JOURNAL').AsString = '-') then
    begin
      aType := 'KJ';
    end
  else if (OrderType.FieldByName('B_JOURNAL').AsString = '+') then
    begin
      aType := 'KS';
      Invert := True;
    end
  else if OrderType.FieldByName('B_INVR').AsString = '-' then
    begin
      aType := 'IR';
    end
  else if OrderType.FieldByName('B_INVR').AsString = '+' then
    begin
      aType := 'BR';
      Invert := True;
    end
  else if OrderType.FieldByName('B_INVO').AsString = '-' then
    begin
      aType   := 'IO';
    end
  else if OrderType.FieldByName('B_INVO').AsString = '+' then
    begin
      aType   := 'BO';
      Invert := True;
    end
  else exit;
  aOrderNo := DataSet.FieldByName('ORDERNO').AsString;
  with Accountingjournal.DataSet do
    begin
      if AccountingJournal.DataSet.Locate('VATT', VarArrayOf([Positions.FieldByName('VAT').AsString]),[loCaseInsensitive,loPartialKey]) then
        Accountingjournal.DataSet.Edit
      else
        begin
          Insert;
          FieldByName('CURRENCY').AsString := DataSet.FieldByName('CURRENCY').AsString;
          FieldByName('PAYMENTTAR').AsString := DataSet.FieldByName('PAYMENTTAR').AsString;
          FieldByName('PAYMENT').AsString  := 'N';
          FieldByName('VATT').AsInteger    := Positions.FieldByName('VAT').AsInteger;
          FieldByName('NETPRICE').AsFloat  := 0;
          FieldByName('GROSSPRICE').AsFloat:= 0;
        end;
      FieldByName('TYPE').AsString     := aType;
      FieldByName('ORDERNO').AsString  := aOrderNo;
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
      if Invert then
        begin
          FieldByName('NETPRICE').AsFloat   := FieldByName('NETPRICE').AsFloat-Positions.FieldByName('POSPRICE').AsFloat;
          FieldByName('GROSSPRICE').AsFloat := FieldByName('GROSSPRICE').AsFloat-Positions.FieldByName('GROSSPRICE').AsFloat;
        end
      else
        begin
          FieldByName('NETPRICE').AsFloat   := FieldByName('NETPRICE').AsFloat+Positions.FieldByName('POSPRICE').AsFloat;
          FieldByName('GROSSPRICE').AsFloat := FieldByName('GROSSPRICE').AsFloat+Positions.FieldByName('GROSSPRICE').AsFloat;
        end;
      Post;
    end;
end;
function TOrder.FormatCurrency(Value: real): string;
begin
  Result := FormatFloat('0.00',Value)+' '+DataSet.FieldByName('CURRENCY').AsString;
end;
function TOrder.CalcDispatchType: Boolean;
var
  aDisp: TDispatchTypes;
  aMasterdata: TMasterdata;
  aPosTyp: TPositionTyp;
begin
  aPosTyp := TPositionTyp.Create(Self,DataModule,Connection);
  aPosTyp.Open;
  if aPosTyp.DataSet.Locate('TYPE',6,[loCaseInsensitive]) then
    begin
      //Find new Dispatchtype
      aDisp := TDispatchTypes.Create(Self,DataModule,Connection);
      if not Address.DataSet.Active then Address.Open;
      if Address.Count > 0 then
        begin
          aDisp.SelectByCountryAndWeight(Address.FieldByName('COUNTRY').AsString,DataSet.FieldByName('WEIGHT').AsFloat);
          aDisp.Open;
        end;
      //Delete old Dispatchtypes
      while Positions.DataSet.Locate('POSTYP',aPosTyp.FieldByName('NAME').AsString,[loCaseInsensitive]) do
        Positions.DataSet.Delete;
      if (aDisp.Count > 0) then
        begin
          while (not aDisp.DataSet.EOF) and (aDisp.FieldByName('ARTICLE').AsString = '') do
            aDisp.DataSet.Next;
          aMasterdata := TMasterdata.Create(Self,DataModule,Connection);
          aMasterdata.Select(aDisp.FieldByName('ARTICLE').AsString);
          aMasterdata.Open;
          if aMasterdata.Count > 0 then
            begin
              Positions.Append;
              Positions.Assign(aMasterdata);
              Positions.FieldByName('POSTYP').AsString:=aPosTyp.FieldByName('NAME').AsString;
              Positions.DataSet.Post;
            end;
          aMasterdata.Free;
        end;
      aDisp.Free;
    end;
  aPosTyp.Free;
end;
function TOrderPos.GetAccountNo: string;
begin
  if Assigned(Order) and (Order.Address.Count>0) then
    Result:=Order.Address.FieldByName('ACCOUNTNO').AsString
  else inherited;
end;
procedure TOrderPos.PosPriceChanged(aPosDiff, aGrossDiff: Extended);
begin
  if not ((Order.DataSet.State = dsEdit) or (Order.DataSet.State = dsInsert)) then
    Order.DataSet.Edit;
  Order.FieldByName('NETPRICE').AsFloat := Order.FieldByName('NETPRICE').AsFloat+aPosDiff;
  Data.PaymentTargets.Open;
  if Data.PaymentTargets.DataSet.Locate('ID',Order.FieldByName('PAYMENTTAR').AsString,[]) then
    Order.FieldByName('DISCPRICE').AsFloat := InternalRound(Order.FieldByName('NETPRICE').AsFloat-((Order.FieldByName('NETPRICE').AsFloat/100)*Data.PaymentTargets.FieldByName('CASHDISC').AsFloat));
  Order.FieldByName('GROSSPRICE').AsFloat := Order.FieldByName('GROSSPRICE').AsFloat+aGrossDiff;
end;
procedure TOrderPos.PosWeightChanged(aPosDiff : Extended);
begin
  if not ((Order.DataSet.State = dsEdit) or (Order.DataSet.State = dsInsert)) then
    Order.DataSet.Edit;
  Order.FieldByName('WEIGHT').AsFloat := Order.FieldByName('WEIGHT').AsFloat+aPosDiff;
end;
function TOrderPos.GetCurrency: string;
begin
  Result:=Order.FieldByName('CURRENCY').AsString;
end;
function TOrderPos.GetOrderTyp: Integer;
begin
  Result := 0;
  if Order.OrderType.DataSet.Locate('STATUS', Order.FieldByName('STATUS').AsString, [loCaseInsensitive]) then
    Result := StrToIntDef(trim(copy(Order.OrderType.FieldByName('TYPE').AsString, 0, 2)), 0);
end;
constructor TOrderPos.Create(aOwner: TComponent; DM : TComponent;aConnection: TComponent;
  aMasterdata: TDataSet);
begin
  inherited Create(aOwner, DM,aConnection, aMasterdata);
  FQMTest := TOrderQMTest.Create(Owner,DM,aConnection,DataSet);
  FOrderRepair := TOrderRepair.Create(Owner,DM,aConnection,DataSet);
end;
destructor TOrderPos.Destroy;
begin
  FOrderRepair.Destroy;
  FQMTest.Destroy;
  inherited Destroy;
end;
function TOrderPos.CreateTable : Boolean;
begin
  Result := inherited CreateTable;
  FOrderRepair.CreateTable;
  FQMTest.CreateTable;
end;
procedure TOrderPos.Assign(aSource: TPersistent);
var
  aMasterdata: TMasterdata;
  tmpPID: String;
begin
  inherited Assign(aSource);
  if aSource is TMasterdata then
    begin
      aMasterdata := aSource as TMasterdata;
      //Use Text that is setted in Ordertype
      if (not Order.OrderType.FieldByName('TEXTTYP').IsNull) then
        DataSet.FieldByName('TEXT').Clear;
      if (not Order.OrderType.FieldByName('TEXTTYP').IsNull) and (Order.OrderType.FieldByName('TEXTTYP').AsInteger > 0) then
        if aMasterdata.Texts.DataSet.Locate('TEXTTYPE',VarArrayOf([Order.OrderType.FieldByName('TEXTTYP').AsInteger]),[loCaseInsensitive]) then
          begin
            if CanHandleRTF then
              DataSet.FieldByName('TEXT').AsString := aMasterdata.Texts.FieldByName('TEXT').AsString
            else
              DataSet.FieldByName('TEXT').AsString := RTF2Plain(aMasterdata.Texts.FieldByName('TEXT').AsString);
          end;
      tmpPID := Order.FieldByName('PID').AsString;
      tmpPID := StringReplace(tmpPID,'...','',[rfReplaceAll]);
      if pos(aMasterdata.Number.AsString,tmpPID) = 0 then
        begin
          if length(tmpPID+','+aMasterdata.Number.AsString) > 49 then
            tmpPID := tmpPID+'...'
          else
            tmpPID := tmpPID+','+aMasterdata.Number.AsString;
          if copy(tmpPID,0,1) = ',' then
            tmpPID := copy(tmpPID,2,length(tmpPID));
          if not Order.CanEdit then
            Order.DataSet.Edit;
          Order.FieldByName('PID').AsString:=tmpPID;
        end;
    end;
end;
procedure TOrderPos.DefineFields(aDataSet: TDataSet);
begin
  inherited DefineFields(aDataSet);
  with aDataSet as IBaseManageDB do
    begin
      TableName := 'ORDERPOS';
      DefineUserFields(aDataSet);
    end;
end;
procedure TOrderPos.FillDefaults(aDataSet: TDataSet);
begin
  with aDataSet,BaseApplication as IBaseDbInterface do
    begin
      PosTyp.DataSet.Locate('TYPE', '0', []);
      if TOrder(Parent).OrderType.DataSet.Locate('STATUS',TOrder(Parent).FieldByName('STATUS').AsString, [loCaseInsensitive, loPartialKey]) then
        if PosTyp.DataSet.Locate('NAME',TOrder(Parent).OrderType.FieldByName('DEFPOSTYP').AsString,[loCaseInsensitive, loPartialKey]) then
          FieldByName('POSTYP').AsString := TOrder(Parent).OrderType.FieldByName('DEFPOSTYP').AsString;
      if DataSet.FieldDefs.IndexOf('ORDERNO') > -1 then
        FieldByName('ORDERNO').AsString  := TOrder(Parent).FieldByName('ORDERNO').AsString;
    end;
  inherited FillDefaults(aDataSet);
end;
function TOrderList.GetHistory: TBaseHistory;
begin
  Result := FHistory;
end;
function TOrderList.GetTextFieldName: string;
begin
  Result:='CUSTNAME';
end;
function TOrderList.GetNumberFieldName: string;
begin
  Result:='ORDERNO';
end;
function TOrderList.GetBookNumberFieldName: string;
begin
  Result:='NUMBER';
end;
function TOrderList.GetStatusFieldName: string;
begin
  Result:='STATUS';
end;
function TOrderList.GetCommissionFieldName: string;
begin
  Result:='COMMISSION';
end;
constructor TOrderList.Create(aOwner: TComponent; DM: TComponent;
  aConnection: TComponent; aMasterdata: TDataSet);
begin
  inherited Create(aOwner, DM, aConnection, aMasterdata);
  FHistory := TBaseHistory.Create(Self,DM,aConnection,DataSet);
  FOrderTyp := TOrderTyp.Create(Self,DM,aConnection);
  with BaseApplication as IBaseDbInterface do
    begin
      with DataSet as IBaseDBFilter do
        begin
          UsePermissions:=True;
        end;
    end;
end;
destructor TOrderList.Destroy;
begin
  FHistory.Destroy;
  FOrderTyp.Destroy;
  inherited Destroy;
end;

function TOrderList.GetStatusIcon: Integer;
var
  aStat: String;
begin
  Result := -1;
  aStat := FStatusCache.Values[FieldByName(GetStatusFieldName).AsString];
  if aStat <> '' then Result := StrToIntDef(aStat,-1)
  else
    begin
      OrderType.Open;
      if OrderType.DataSet.Locate('STATUS',DataSet.FieldByName('STATUS').AsString,[]) then
        Result := StrToIntDef(OrderType.DataSet.FieldByName('ICON').AsString,-1);
      FStatusCache.Values[FieldByName(GetStatusFieldName).AsString] := IntToStr(Result);
    end;
end;

procedure TOrderList.Open;
begin
  with  DataSet as IBaseDBFilter, BaseApplication as IBaseDBInterface, DataSet as IBaseManageDB do
    begin
      if Filter='' then
        Filter := QuoteField('ACTIVE')+'='+QuoteValue('Y')+' or '+Data.ProcessTerm(QuoteField('ACTIVE')+'='+QuoteValue(''));;
    end;
  inherited Open;
end;

procedure TOrderList.DefineFields(aDataSet: TDataSet);
begin
  with aDataSet as IBaseManageDB do
    begin
      TableName := 'ORDERS';
      TableCaption := strOrders;
      UpdateFloatFields:=True;
      if Assigned(ManagedFieldDefs) then
        with ManagedFieldDefs do
          begin
            Add('ORDERNO',ftInteger,0,True);
            Add('ACTIVE',ftString,1,False);
            Add('STATUS',ftString,4,True);
            Add('LANGUAGE',ftString,3,False);
            Add('DATE',ftDate,0,False);
            Add('NUMBER',ftString,20,False);
            Add('CUSTNO',ftString,20,False);
            Add('CUSTNAME',ftString,200,False);
            Add('DOAFQ',ftDate,0,False);                    //Anfragedatum
            Add('DWISH',ftDate,0,False);                    //Wunschdatum
            Add('DAPPR',ftDate,0,False);                    //Bestätigt (Approved)
            Add('ODATE',ftDate,0,False);                    //Original Date
            Add('STORAGE',ftString,3,False);
            Add('CURRENCY',ftString,5,False);
            Add('PAYMENTTAR',ftString,2,False);
            Add('PID',ftString,50,False);
            Add('SHIPPING',ftString,3,False);
            Add('SHIPPINGD',ftDate,0,False);
            Add('WEIGHT',ftFloat,0,False);
            Add('VATH',ftFloat,0,False);                   //Halbe MwSt
            Add('VATF',ftFloat,0,False);                   //Volle MwSt
            Add('NETPRICE',ftFloat,0,False);                //Nettopreis
            Add('DISCPRICE',ftFloat,0,False);              //Skontopreis
            Add('DISCOUNT',ftFloat,0,False);                //Rabatt
            Add('GROSSPRICE',ftFloat,0,False);              //Bruttoprice
            Add('DONE',ftString,1,False);
            Add('DELIVERED',ftString,1,False);
            Add('PAYEDON',ftDate,0,False);
            Add('DELIVEREDON',ftDate,0,False);
            Add('COMMISSION',ftString,30,False);
            Add('NOTE',ftMemo,0,False);
            Add('HEADERTEXT',ftMemo,0,False);
            Add('FOOTERTEXT',ftMemo,0,False);
            Add('CHANGEDBY',ftString,4,False);
            Add('CREATEDBY',ftString,4,False);
          end;
      if Assigned(ManagedIndexdefs) then
        with ManagedIndexDefs do
          begin
            Add('ORDERNO','ORDERNO',[ixUnique]);
            Add('NUMBER','NUMBER',[]);
            Add('DATE','DATE',[]);
            Add('ODATE','ODATE',[]);
            Add('STATUS','STATUS',[]);
            Add('CUSTNO','CUSTNO',[]);
            Add('CUSTNAME','CUSTNAME',[]);
          end;
      DefineUserFields(aDataSet);
    end;
end;
initialization
end.

