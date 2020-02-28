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
  Classes, SysUtils, uBaseDbClasses, db,
  uBaseERPDBClasses, uMasterdata, uPerson, Variants ,uBaseDatasetInterfaces2;
type

  { TOrderTyp }

  TOrderTyp = class(TBaseDBDataSet)
  private
    FStatus,FStatusName,FType,FDerivative,FNumberSet,FDefPostyp,
      FB_Storage,FB_Reserved,FB_STOrder,FB_Journal,FB_Serials,FB_InvR,FB_InvO,FB_Dunning,FB_CHist: string;
    FIcon,FTextTyp : Integer;
    FIsDerivate,FDoCopy,FChangeAble,FRoundPos,
      FSI_Order,FSI_Pos,FSI_Prod,FSI_Acc,FSI_InvR,FSI_InvO: Boolean;
  public
    class function GetRealTableName: string; override;
  published
    property Status: string index 4 read FStatus write FStatus;
    property StatusName: string index 30 read FStatusName write FStatusName;
    property Typ: string index 1 read FType write FType;
    property Icon: Integer read FIcon write FIcon;
    property IsDerivate: Boolean read FIsDerivate write FIsDerivate;
    property Derivative: string index 30 read FDerivative write FDerivative;
    property DoCopy: Boolean read FDoCopy write FDoCopy;    //Auftrag kopieren ? (Nur bei N wird nicth kopiert)
    property NumberSet: string index 30 read FNumberSet write FNumberSet;
    property DefPostyp: string index 3 read FDefPostyp write FDefPostyp; //welcher positionstyp wird nach insert gesetzt?
    property TextTyp: Integer read FTextTyp write FTextTyp;  //welcher text ist standardtext
    property ChangeAble: Boolean read FChangeAble write FChangeAble;//nach Buchen änderbar

    property RoundPos: Boolean read FRoundPos write FRoundPos;  //Positionen werden gerundet

    property SI_Order: Boolean read FSI_Order write FSI_Order;  //im Auftrag anzeigen
    property SI_Pos: Boolean read FSI_Pos write FSI_Pos;    //in der Kasse anzeigen (Point of Sale)
    property SI_Prod: Boolean read FSI_Prod write FSI_Prod;   //in der Produktion anzeigen
    property SI_Acc: Boolean read FSI_Acc write FSI_Acc;    //in der Fibu anzeigen (Accounting)
    property SI_InvR: Boolean read FSI_InvR write FSI_InvR;   //im Rechnungseingang anzeigen (Invoice Receipt)
    property SI_InvO: Boolean read FSI_InvO write FSI_InvO;   //im Rechnungsausgang anzeigen (Outgoing Invoice)

    property B_Storage: string index 1 read FB_Storage write FB_Storage; //Lagerbuchung                 (+ 0 -)
    property B_Reserved: string index 1 read FB_Reserved write FB_Reserved;//Lagerbuchung Reserviert      (+ 0 -)
    property B_STOrder: string index 1 read FB_STOrder write FB_STOrder; //Lagereintrag im Hauptvorgang (+ 0 -)
    property B_Journal: string index 1 read FB_Journal write FB_Journal; //Kassenbuch                   (+ 0 -)
    property B_Serials: string index 1 read FB_Serials write FB_Serials; //Serienummerverwaltung        (+ 0 -)
    property B_InvR: string index 1 read FB_InvR write FB_InvR;    //Rechnungseingang             (+ 0 -)
    property B_InvO: string index 1 read FB_InvO write FB_InvO;    //Rechnungsausgang             (+ 0 -)
    property B_Dunning: string index 1 read FB_Dunning write FB_Dunning; //Mahnwesen                    (+ 0 -)
    property B_CHist: string index 1 read FB_CHist write FB_CHist;   //Kundenhistorie               (+ 0)
  end;

  { TOrderTypes }

  TOrderTypes = class(TAbstractMasterDetail)
  public
    class function GetObjectTyp: TClass; override;
  end;

  { TOrderList }

  TOrderList = class(TBaseERPList,IBaseHistory)
  private
    FOrderNo : Integer;
    FActive,FDone,FDelivered : Boolean;
    FStatus,FLanguage,FNumber,FCustNo,FCustName,FCustZip,FEMail,FStorage,FCurrency,
      FPaymentTar,FPID,FPVersion,FPLanguage,FShipping,FCommission,FProject,FProjectNr,
      FNote,FHeaderText,FFooterText,FChangedBy,FCreatedBy: string;
    FDate,FDoAFQ,FDWish,FODate,FDAppr,FShippingD,FPayedOn,FDeliveredOn : TDateTime;
    FPQuantity,FWeight,FVatH,FVatF,FNetPrice,FDiscPrice,FDiscount,FGrossPrice : double;
    FHistory : TBaseHistory;
    FOrderTyp: TOrdertypes;
    FProjectID : Int64;
    FOrigID: String;
    function GetHistory: TBaseHistory;
    function GetOrderTyp: TOrdertypes;
  protected
  public
    constructor Create(aParent : TPersistent);override;
    destructor Destroy; override;
    class function MapField(aField: string): string; override;
    class function GetRealTableName: string; override;
    function GetStatusIcon: Integer; override;
    procedure Select(aID : string);overload;
    function GetTyp: string; override;
    function SelectFromCommission(aNumber : string) : Boolean;
    procedure OpenItem(AccHistory: Boolean=True); override;
    property History : TBaseHistory read FHistory;
    property OrderType : TOrdertypes read GetOrderTyp;
    function SelectOrderType : Boolean;
  published
    property OrderNo: Integer read FOrderNo write FOrderNo;
    property Active: Boolean read FActive write FActive;
    property Status: string index 4 read FStatus write FStatus;
    property Language: string index 3 read FLanguage write FLanguage;
    property Date: TDateTime read FDate write FDate;
    property Number: string index 20 read FNumber write FNumber;
    property CustNo: string index 20 read FCustNo write FCustNo;
    property CustName: string index 200 read FCustName write FCustName;
    property CustZip: string index 8 read FCustZip write FCustZip;
    property EMail: string index 200 read FEMail write FEMail;                //Vorgangsmail z.b. bei Angabe im Webshop oder RMA System
    property DoAFQ: TDateTime read FDoAFQ write FDoAFQ;                    //Anfragedatum
    property DWish: TDateTime read FDWish write FDWish;                    //Wunschdatum
    property DAppr: TDateTime read FDAppr write FDAppr;                    //Bestätigt (Approved)
    property ODate: TDateTime read FODate write FODate;                    //Original Date
    property Storage: string index 3 read FStorage write FStorage;
    property Currency: string index 5 read FCurrency write FCurrency;
    property PaymentTar: string index 2 read FPaymentTar write FPaymentTar;
    property PID: string index 250 read FPID write FPID;                   //Produktid wird mit Artikeln befüllt die hinzugefügt werden beim Produktionsauftrag = zu Fertigender Artikel
    property PVersion: string index 8 read FPVersion write FPVersion;               //Version des zu fertigen Artikels
    property PLanguage: string index 4 read FPLanguage write FPLanguage;              //Sprache des zu fertigen Artikels
    property PQuantity: double read FPQuantity write FPQuantity;                //Fertigungsmenge
    property Shipping: string index 3 read FShipping write FShipping;
    property ShippingD: TDateTime read FShippingD write FShippingD;
    property Weight: double read FWeight write FWeight;
    property VatH: double read FVatH write FVatH;                   //Halbe MwSt
    property VatF: double read FVatF write FVatF;                   //Volle MwSt
    property NetPrice: double read FNetPrice write FNetPrice;                //Nettopreis
    property DiscPrice: double read FDiscPrice write FDiscPrice;              //Skontopreis
    property Discount: double read FDiscount write FDiscount;                //Rabatt
    property GrossPrice: double read FGrossPrice write FGrossPrice;              //Bruttoprice
    property Done: Boolean read FDone write FDone;
    property Delivered: Boolean read FDelivered write FDelivered;
    property PayedOn: TDateTime read FPayedOn write FPayedOn;
    property DeliveredOn: TDateTime read FDeliveredOn write FDeliveredOn;
    property Commission: string index 30 read FCommission write FCommission;
    property ProjectID: Int64 read FProjectID write FProjectID;
    property Project: string index 260 read FProject write FProject;
    property ProjectNr: string index 20 read FProjectNr write FProjectNr;
    property Note: string read FNote write FNote;
    property HeaderText: string read FHeaderText write FHeaderText;
    property FooterText: string read FFooterText write FFooterText;
    property ChangedBy: string index 4 read FChangedBy write FChangedBy;
    property CreatedBy: string index 4 read FCreatedBy write FCreatedBy;
  end;

  { TOrderQMTestDetail }

  TOrderQMTestDetail = class(TBaseDBDataSet)
  private
    FModul,FName,FType,FMUnit,FExpected,FResult : string;
    FStep : Integer;
    FResultShort : Boolean;
  public
    class function MapField(aField: string): string; override;
  published
    property Modul: string index 60 read FModul write FModul;
    property Step: Integer read FStep write FStep;
    property Name: string index 60 read FName write FName;
    property Typ: string index 10 read FType write FType;
    property MUnit: string index 20 read FMUnit write FMUnit;
    property Expected : string read FExpected write FExpected;
    property Result : string read FResult write FResult;
    property ResultShort: Boolean read FResultShort write FResultShort;
  end;
  TOrderQMTestDetails = class(TAbstractMasterDetail)
  public
    class function GetObjectTyp: TClass; override;
  end;
  TOrderQMTest = class(TBaseDBDataSet)
  private
    FDetails: TOrderQMtestDetails;
    FID : Int64;
    FName,FSerial,FNotes,FRawData : string;
    FTestTime,FTestEnd : TDateTime;
    FDuration : Integer;
    FResult : Boolean;
  public
    constructor Create(aParent : TPersistent);override;
    destructor Destroy;override;
  published
    property ID: Int64 read FID write FID;
    property Name: string index 20 read FName write FName;
    property Result: Boolean read FResult write FResult;
    property Serial: string index 30 read FSerial write FSerial;
    property Notes : string read FNotes write FNotes;
    property RawData: string read FRawData write FRawData;
    property TestTime : TDateTime read FTestTime write FTestTime;
    property TestEnd : TDateTime read FTestEnd write FTestEnd;
    property Duration: Integer read FDuration write FDuration;
    property Details : TOrderQMtestDetails read FDetails;
  end;

  { TOrderQMTests }

  TOrderQMTests = class(TAbstractMasterDetail)
  public
    class function GetObjectTyp: TClass; override;
  end;
  TOrder = class;
  TRepairProblem = class(TBaseDBDataSet)
  private
    FProblem : string;
  published
    property Problem: string index 60 read FProblem write FProblem;
  end;
  TOrderRepairDetail = class(TBaseDbDataSet)
  private
    FAssembly,FPart,FError : string;
  published
    property Assembly: string index 60 read FAssembly write FAssembly;
    property Part: string index 60 read FPart write FPart;
    property Error: string index 120 read FError write FError;
  end;
  TOrderRepairDetails = class(TAbstractMasterDetail)
  public
    class function GetObjectTyp: TClass; override;
  end;
  TRepairImageLinks = class(TLinks)
  public
    procedure FillDefaults; override;
  end;

  { TOrderRepairImage }

  TOrderRepairImage = class(TBaseDbDataSet)
  private
    FName,FCustName,FStatus,FSymtoms,FCategory,FUser,FDesc,FSolve,FNotes,FIntNotes : string;
    FCounter : Integer;
    FDetail: TOrderRepairDetail;
    FHistory: TBaseHistory;
    FImages: TImages;
    FLinks: TRepairImageLinks;
    FDS : TDataSource;
    procedure SetStatus(AIndex: Integer; AValue: string);
  public
    constructor Create(aParent : TPersistent);override;
    destructor Destroy; override;
    procedure Open; override;
    property RepairDetail : TOrderRepairDetail read FDetail;
    property History : TBaseHistory read FHistory;
    property Images : TImages read FImages;
    property Links : TRepairImageLinks read FLinks;
  published
    property Name: string index 100 read FName write FName;
    property CustName: string index 100 read FCustName write FCustName;
    property Status: string index 4 read FStatus write SetStatus;
    property Symtoms: string index 800 read FSymtoms write FSymtoms;
    property Category: string index 40 read FCategory write FCategory;
    property User: string index 20 read FUser write FUser;
    property Desc: string read FDesc write FDesc;
    property Solve: string read FSolve write FSolve;
    property Notes: string read FNotes write FNotes;
    property IntNotes: string read FIntNotes write FIntNotes;
    property Counter: Integer read FCounter write FCounter;
  end;
  TOrderRepair = class(TBaseDBDataSet)
  private
    FDetails: TOrderRepairDetails;
    FID : Integer;
    FOperation,FErrDesc,FNotes,FIntNotes,FImagename,FChangedBy : string;
    FWarrenty : Boolean;
    FErrImage : Int64;
    FTime : double;
  public
    constructor Create(aParent : TPersistent);override;
    destructor Destroy;override;
  published
    property ID: Integer read FID write FID;
    property Operation: string index 20 read FOperation write FOperation;
    property ErrDesc: string read FErrDesc write FErrDesc;
    property Notes: string read FNotes write FNotes;
    property IntNotes: string read FIntNotes write FIntNotes;
    property Warrenty: Boolean read FWarrenty write FWarrenty;
    property ErrImage: Int64 read FErrImage write FErrImage;
    property Imagename: string index 100 read FImagename write FImagename;
    property Time: double read FTime write FTime;
    property ChangedBy: string index 4 read FChangedBy write FChangedBy;
    property Details : TOrderRepairDetails read FDetails;
  end;

  { TOrderRepairs }

  TOrderRepairs = class(TAbstractMasterDetail)
  public
    class function GetObjectTyp: TClass; override;
  end;

  { TOrderPos }

  TOrderPos = class(TBaseDBPosition)
  private
    FOrder: TOrder;
    FOrderRepair: TOrderRepairs;
    FQMTest: TOrderQMTests;
    FCostCentre,FAccount,FProjectNr : string;
  protected
    function GetAccountNo : string;override;
    procedure PosPriceChanged(aPosDiff,aGrossDiff :Extended);override;
    procedure PosWeightChanged(aPosDiff : Extended);override;
    function Round(aValue: Extended): Extended; override;
    function RoundPos(aValue : Extended) : Extended;override;
    function GetCurrency : string;override;
    function GetOrderTyp : Integer;override;
  public
    constructor Create(aParent : TPersistent);override;
    destructor Destroy;override;
    procedure Assign(aSource : TPersistent);override;
    procedure FillDefaults; override;
  published
    property CostCentre: string index 10 read FCostCentre write FCostCentre;//Kostenstelle
    property Account: string index 10 read FAccount write FAccount; //Fibu Konto
    property ProjectNr: string index 20 read FProjectNr write FProjectNr;
    property Repair : TOrderRepairs read FOrderRepair;
    property QMTest : TOrderQMTests read FQMTest;
  end;

  { TOrderPositions }

  TOrderPositions = class(TAbstractMasterDetail)
  public
    class function GetObjectTyp: TClass; override;
  end;

  { TOrderAddress }

  TOrderAddress = class(TBaseDBAddress)
  private
    FAccountNo : string;
  public
    class function GetRealTableName: string; override;
    procedure Assign(Source: TPersistent); override;
  published
    property AccountNo: string index 20 read FAccountNo write FAccountNo;
  end;

  { TOrderAddresses }

  TOrderAddresses = class(TAbstractMasterDetail)
  public
    class function GetObjectTyp: TClass; override;
  end;
  TOrderPosTyp = class(TBaseDBDataSet)
  private
    FName,FType : string;
  public
  published
    property Name: string index 3 read FName write FName;
    property Typ: string index 1 read FType write FType;
  end;
  TDispatchType = class(TBaseDBDataSet)
  private
    FID,FCountry,FName,FOutputDrv,FArticle : string;
    FWeight : double;
  public
  published
    property ID: string index 3 read FID write FID;
    property Country: string index 3 read FCountry write FCountry;
    property Name: string index 20 read FName write FName;
    property OutputDrv: string index 60 read FOutputDrv write FOutputDrv;
    property Weight: double read FWeight write FWeight;
    property Article: string index 40 read FArticle write FArticle;
  end;
  TPaymentTargets = class(TBaseDBDataSet)
  private
    FID,FName,FText,FFAccounts : string;
    FCashDisc : double;
    FCashDiscD,FDays : Integer;
    FDefaultPT : Boolean;
  published
    property ID: string index 2 read FID write FID;
    property Name: string index 10 read FName write FName;
    property Text: string index 30 read FText write FText;
    property FAccounts: string read FFAccounts write FFAccounts;
    property CashDisc: double read FCashDisc write FCashDisc;               //Skonto
    property CashDiscD: Integer read FCashDiscD write FCashDiscD;            //Skonto Tage
    property Days: Integer read FDays write FDays;                 //Tage
    property DefaultPT: Boolean read FDefaultPT write FDefaultPT;
  end;
  TOrderLinks = class(TLinks)
  public
    procedure FillDefaults;override;
  end;
  TOnGetStorageEvent = function(Sender : TOrder;aStorage : TStorage) : Boolean of object;
  TOnGetSerialEvent = function(Sender : TOrder;aMasterdata : TMasterdata;aQuantity : Integer) : Boolean of object;
  TOrder = class(TOrderList,IPostableDataSet,IShipableDataSet)
  private
    //FCurrency: TCurrency;
    FFailMessage: string;
    FLinks: TOrderLinks;
    FOnGetSerial: TOnGetSerialEvent;
    FOnGetStorage: TOnGetStorageEvent;
    FOrderAddress: TOrderAddresses;
    FOrderPos: TOrderPositions;
    function GetCommission: TField;
    procedure ReplaceParentFields(aField: TField; aOldValue: string;
      var aNewValue: string);
    function Round(Value: Extended): Extended;
  public
    constructor Create(aParent : TPersistent);override;
    destructor Destroy;override;
    procedure FillDefaults;override;
    procedure Open;override;
    procedure RefreshActive(aOrderno: string='');
    property Commission : TField read GetCommission;
    function CombineItems(aRemoteLink: string): Boolean;
    property OnGetStorage : TOnGetStorageEvent read FOnGetStorage write FOnGetStorage;
    property OnGetSerial : TOnGetSerialEvent read FOnGetSerial write FOnGetSerial;
    //property Currency : TCurrency read FCurrency;
    function SelectCurrency : Boolean;
    procedure Recalculate;
    function ChangeStatus(aNewStatus : string) : Boolean;override;
    procedure ShippingOutput;
    function DoPost: TPostResult;
    function PostArticle(aTyp, aID, aVersion, aLanguage: variant; Quantity: real; QuantityUnit, PosNo: string; var aStorage: string; var OrderDelivered: boolean) : Boolean;
    //function DoBookPositionCalc(AccountingJournal : TAccountingJournal) : Boolean;
    function FailMessage : string;
    function FormatCurrency(Value : real) : string;
    function CalcDispatchType : Boolean;
    function GetOrderTyp: Integer;
    function SelectFromLink(aLink: string): Boolean; override;
    function Duplicate : Boolean;override;
  published
    property Positions : TOrderPositions read FOrderPos;
    property Address : TOrderAddresses read FOrderAddress;
    property Links : TOrderLinks read FLinks;
  end;
implementation
uses uIntfStrConsts,uData;
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

{ TOrderTypes }

class function TOrderTypes.GetObjectTyp: TClass;
begin
  Result := TOrderTyp;
end;

{ TOrderAddresses }

class function TOrderAddresses.GetObjectTyp: TClass;
begin
  Result := TOrderAddress;
end;

{ TOrderQMTestDetail }

class function TOrderQMTestDetail.MapField(aField: string): string;
begin
  Result:=inherited MapField(aField);
  if Result = 'Typ' then
    Result := 'TYPE'
  else if Result = 'MUnit' then
    Result := 'UNIT'
  ;
end;

{ TOrderRepairs }

class function TOrderRepairs.GetObjectTyp: TClass;
begin
  Result := TOrderRepair;
end;

{ TOrderQMTests }

class function TOrderQMTests.GetObjectTyp: TClass;
begin
  Result := TOrderQMTest;
end;

{ TOrderRepairDetails }

class function TOrderRepairDetails.GetObjectTyp: TClass;
begin
  Result := TOrderRepairDetail;
end;

{ TOrderRepairImage }

procedure TOrderRepairImage.SetStatus(AIndex: Integer; AValue: string);
begin
  if FStatus = AValue then exit;
  History.AddItem(Self,Format(strStatusChanged,[FStatus,AValue]),'','',nil,ACICON_STATUSCH);
  FStatus := AValue;
end;
procedure TOrderRepairImage.Open;
begin
  inherited Open;
end;

{ TOrderPositions }

class function TOrderPositions.GetObjectTyp: TClass;
begin
  Result := TOrderPos;
end;

procedure TOrder.ReplaceParentFields(aField: TField; aOldValue: string;
  var aNewValue: string);
var
  aRec: Variant;
begin
  if (aField.FieldName='PARENT') and (aOldValue<>'') then
    begin
      aField.DataSet.Post;
      aRec := aField.DataSet.FieldByName('SQL_ID').AsVariant;
      if aField.DataSet.Locate('OLD_ID',aOldValue,[]) then
        aNewValue:=aField.DataSet.FieldByName('SQL_ID').AsString;
      aField.DataSet.Locate('SQL_ID',aRec,[]);
      if (aField.DataSet.State=dsBrowse) then
        aField.DataSet.Edit;
    end;
end;

procedure TRepairImageLinks.FillDefaults;
begin
  inherited FillDefaults;
  RRef_ID:=(Parent as TOrderRepairImage).SQL_ID;
end;
constructor TOrderRepairImage.Create(aParent: TPersistent);
begin
  inherited;
  FHistory := TBaseHistory.Create(Self);
  FImages := TImages.Create(Self);
  FLinks := TRepairImageLinks.Create(Self);
  FDetail := TOrderRepairDetail.Create(Self);
end;

destructor TOrderRepairImage.Destroy;
begin
  FDS.Free;
  FDetail.Free;
  FLinks.Free;
  FImages.Free;
  FHistory.Free;
  inherited Destroy;
end;
constructor TOrderRepair.Create(aParent: TPersistent);
begin
  inherited;
  FDetails := TOrderRepairDetails.Create(Self);
end;
destructor TOrderRepair.Destroy;
begin
  FDetails.Free;
  inherited Destroy;
end;
procedure TOrderLinks.FillDefaults;
begin
  inherited FillDefaults;
  RRef_ID:=(Parent as TOrder).SQL_ID;
end;

class function TOrderAddress.GetRealTableName: string;
begin
  Result:='ORDERADDR';
end;

procedure TOrderAddress.Assign(Source: TPersistent);
var
  aAddress: TBaseDbAddress;
  Person: TPerson;
begin
  {
  if not Active then Open;
  if not Order.CanEdit then
    Order.DataSet.Edit;
  inherited Assign(Source);
  if Source is TBaseDBAddress then
    begin
      aAddress := Source as TBaseDbAddress;
      Order.FieldByName('CUSTNAME').AsString := aAddress.FieldByName('NAME').AsString;
      Order.FieldByName('CUSTZIP').AsString := aAddress.FieldByName('ZIP').AsString;
    end
  else if Source is TPerson then
    begin
      Person := Source as TPerson;
      if not Order.CanEdit then
        Order.DataSet.Edit;
      Order.FieldByName('CUSTNO').AsString := Person.FieldByName('ACCOUNTNO').AsString;
      Person.ContactData.Open;
      if Person.ContactData.Locate('TYPE','MLB',[loPartialKey])
      or Person.ContactData.Locate('TYPE','MAIL',[loPartialKey])
      or Person.ContactData.Locate('TYPE','MLP',[loPartialKey])
      then
        Order.FieldByName('EMAIL').AsString := Person.ContactData.FieldByName('DATA').AsString;
    end;
  }
end;

class function TOrderTyp.GetRealTableName: string;
begin
  Result:='ORDERTYPE';
end;

class function TOrderQMTestDetails.GetObjectTyp: TClass;
begin
  Result := TOrderQMTestDetail;
end;
constructor TOrderQMTest.Create(aParent: TPersistent);
begin
  inherited;
  FDetails := TOrderQMTestDetails.Create(Self);
end;
destructor TOrderQMTest.Destroy;
begin
  FDetails.Free;
  inherited Destroy;
end;
function TOrder.GetCommission: TField;
begin
  result := FieldByName('COMMISSION');
end;

function TOrder.Round(Value: Extended): Extended;
  function RoundToGranularity(aValue, aGranularity: Double): Double;
  begin
    Result := Trunc(aValue / aGranularity + 0.5 + 1E-10) * aGranularity
  end;
var
  nk: Integer = 2;
begin
  {TODO
  if SelectCurrency and (Currency.FieldByName('DECIMALPL').AsInteger>0) then
    nk := Currency.FieldByName('DECIMALPL').AsInteger;
  if SelectCurrency and (Currency.FieldByName('ROUNDGRAN').AsFloat>0) then
    Result := InternalRound(RoundToGranularity(Value,Currency.FieldByName('ROUNDGRAN').AsFloat),nk)
  else Result := InternalRound(Value,nk);
  }
end;

constructor TOrder.Create(aParent: TPersistent);
begin
  inherited;
  FOrderAddress := TOrderAddresses.Create(Self);
  FOrderPos := TOrderPositions.Create(Self);
  FLinks := TOrderLinks.Create(Self);
  //FCurrency := TCurrency.CreateEx(Self,DataModule,aConnection);
end;
destructor TOrder.Destroy;
begin
  //FCurrency.Free;
  FOrderAddress.Free;
  FOrderPos.Free;
  FreeAndnil(FLinks);
  inherited Destroy;
end;
procedure TOrder.FillDefaults;
begin
  {
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
  }
end;
procedure TOrderList.Select(aID : string);
var
  aFilter: String;
begin
  {
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
  }
end;

function TOrderList.GetTyp: string;
begin
  Result:='O';
end;

function TOrderList.SelectFromCommission(aNumber: string): Boolean;
var
  aFilter: String;
begin
  {
  with  DataSet as IBaseDBFilter, BaseApplication as IBaseDBInterface, DataSet as IBaseManageDB do
    begin
      aFilter :=  QuoteField('COMMISSION')+'='+QuoteValue(aNumber);
      Filter := aFilter;
      Limit := 99;
    end;
  }
end;

procedure TOrder.Open;
begin
  inherited Open;
  OrderType.Locate('STATUS',Status,[]);
  //Address.Open;
  SelectCurrency;
end;

procedure TOrder.RefreshActive(aOrderno: string);
var
  aRec: TBookmark;
  Found: Boolean = False;
  MainOrder: TOrderList;
begin
  {
  if not DataSet.Active then exit;
  if Orderno = '' then
    Orderno := Self.FieldByName('ORDERNO').AsString;
  try
  if Orderno <> '' then
    begin
      MainOrder := TOrderList.CreateEx(Owner,DataModule,Connection);
      MainOrder.Select(Self.FieldByName('ORDERNO').AsString);
      Mainorder.SortDirection:=sdDescending;
      MainOrder.SortFields:='ORDERNO';
      MainOrder.Open;
      MainOrder.First;
      OrderType.Open;
      while not MainOrder.DataSet.EOF do
        begin
          OrderType.DataSet.Locate('STATUS',MainOrder.FieldByName('STATUS').AsString,[]);
          if (OrderType.FieldByName('ISDERIVATE').AsString<>'Y')
          and not Found then
            begin
              if MainOrder.FieldByName('ACTIVE').AsString<>'Y' then
                begin
                  if not MainOrder.CanEdit then MainOrder.Edit;
                  MainOrder.FieldByName('ACTIVE').AsString := 'Y';
                  MainOrder.Post;
                end;
              Found := True;
            end
          else
            begin
              if MainOrder.FieldByName('ACTIVE').AsString<>'N' then
                begin
                  if not MainOrder.CanEdit then MainOrder.Edit;
                  MainOrder.FieldByName('ACTIVE').AsString := 'N';
                  MainOrder.Post;
                end;
            end;
          MainOrder.Next;
        end;
      if (not Found) and (MainOrder.Count>0) then //no order active ??
        begin
          MainOrder.Last;
          if not MainOrder.CanEdit then MainOrder.Edit;
          MainOrder.FieldByName('ACTIVE').AsString := 'Y';
          MainOrder.Post;
        end;
      OrderType.DataSet.Locate('STATUS',DataSet.FieldByName('STATUS').AsString,[]);
      MainOrder.Free;
    end;
  except
  end;
  }
end;

function TOrder.CombineItems(aRemoteLink: string): Boolean;
var
  newnumber: String;
  aClass: TBaseDBDatasetClass;
  aObject: TBaseDBDataset;
  BM: Variant;
  tmp: String;
begin
  Result := False;
  {
  Result := True;
  if TBaseDBModule(DataModule).DataSetFromLink(aRemoteLink,aClass) then
    begin
      aObject := aClass.CreateEx(nil,DataModule);
      if not (aObject is TOrderList) then
        begin
          aObject.Free;
          exit;
        end;
      TBaseDbList(aObject).SelectFromLink(aRemoteLink);
      BM := GetBookmark;
      DataSet.Last;
      newnumber := copy(DataSet.FieldByName('ORDERNO').AsString,length(DataSet.FieldByName('ORDERNO').AsString)-1,2);
      GotoBookmark(BM);
      if NewNumber = '' then
        begin
          raise Exception.Create('NewNumber is NULL');
          exit;
        end;
      aObject.Open;
      if aObject.Count>0 then
        begin
          aObject.First;
          while not aObject.EOF do
            begin
              tmp := copy(DataSet.FieldByName('ORDERNO').AsString,0,length(DataSet.FieldByName('ORDERNO').AsString)-2)+Format('%.2d',[StrToIntDef(newnumber,0)+1]);
              newnumber:=IntToStr(StrToInt(newnumber)+1);
              aObject.Edit;
              aObject.FieldByName('ORDERNO').AsString:=tmp;
              aObject.Post;
              aObject.Next;
            end;
        end;
      aObject.Free;
    end;
  }
end;

function TOrder.SelectCurrency: Boolean;
begin
  {
  if not FCurrency.Locate('SYMBOL',FieldByName('CURRENCY').AsString,[]) then
    begin
      FCurrency.Filter(Data.QuoteField('SYMBOL')+'='+Data.QuoteValue(FieldByName('CURRENCY').AsString));
      result := FCurrency.Locate('SYMBOL',FieldByName('CURRENCY').AsString,[]);
    end
  else Result := True;
  }
end;

procedure TOrder.Recalculate;
var
  aPos: Double = 0;
  aGrossPos: Double = 0;
  aVatH : Double = 0;
  aVatV : Double = 0;
  Vat: TVat;
begin
  {
  Vat := TVat.Create(nil);
  Positions.DataSet.DisableControls;
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
              if not Vat.DataSet.Active then
                Vat.Open;
              Vat.DataSet.Locate('ID',VarArrayof([Positions.FieldByName('VAT').AsString]),[]);
              if Vat.FieldByName('ID').AsInteger=1 then
                aVatV += Positions.FieldByName('GROSSPRICE').AsFloat-Positions.FieldByName('POSPRICE').AsFloat
              else
                aVatH += Positions.FieldByName('GROSSPRICE').AsFloat-Positions.FieldByName('POSPRICE').AsFloat;
            end;
        end;
          if Positions.PosTypDec=4 then //Subtotal
            begin
              Positions.Edit;
              Positions.FieldByName('POSPRICE').AsFloat := aPos;
              Positions.FieldByName('GROSSPRICE').AsFloat := Round(aGrossPos);
              Positions.Post;
            end;
      Positions.Next;
    end;
  if not CanEdit then DataSet.Edit;
  FieldByName('VATH').AsFloat:=Round(aVatH);
  FieldByName('VATF').AsFloat:=Round(aVatV);
  FieldByName('NETPRICE').AsFloat:=Round(aPos);
  FieldByName('GROSSPRICE').AsFloat:=Round(aGrossPos);
  if CanEdit then DataSet.Post;
  Positions.DataSet.EnableControls;
  Vat.Free;
  }
end;

function TOrder.DoPost: TPostResult;
var
  Orders: TOrderList;
  //Accountingjournal: TAccountingJournal = nil;
  MasterdataList: TMasterdataList = nil;
  //Person: TPerson = nil;
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
  {
  Orders := TOrderList.CreateEx(nil,DataModule,Connection);
  MasterdataList := TMasterdataList.CreateEx(nil,DataModule,Connection);
  MainOrder := TOrder.CreateEx(nil,DataModule,Connection);
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
      Accountingjournal := TAccountingjournal.CreateEx(nil,DataModule,Connection);
      Accountingjournal.CreateTable;
      with Accountingjournal.DataSet as IBaseDBFilter do
        begin
          Data.SetFilter(Accountingjournal,
          Data.ProcessTerm(Data.QuoteField('ORDERNO')+'='+Data.QuoteValue(DataSet.FieldByName('ORDERNO').AsString))
          );
        end;
      Data.StartTransaction(Connection read write ;
      try
        OrderDone      := True;
        OrderDelivered := True;
        while Accountingjournal.Count > 1 do
          begin
            Accountingjournal.DataSet.Last;
            Accountingjournal.Delete;
          end;
        //Belegnummer und Datum vergeben
        DataSet.Edit;
        aNumbers := TNumberSets.CreateEx(nil,Data,Connection);
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
        OpenItem(False);
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
        Person := TPerson.CreateEx(nil,DataModule,Connection);
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
        RefreshActive;
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
  FreeAndNil(Person);
  FreeAndNil(aNumbers);
  FreeAndNil(AccountingJournal);
  FreeAndnil(Orders);
  FreeAndNil(MasterdataList);
  FreeAndNil(MainOrder);
  }
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
  {
  OrderType.Open;
  if not OrderType.DataSet.Locate('STATUS',aNewStatus,[loCaseInsensitive]) then
    Data.SetFilter(OrderType,'');
  if OrderType.DataSet.Locate('STATUS',aNewStatus,[loCaseInsensitive]) then
    begin
      aOrderType := StrToIntDef(trim(copy(OrderType.FieldByName('TYPE').AsString,0,2)),0);
      if trim(OrderType.FieldByName('TYPE').AsString) = '' then
        exit;
      TBaseDBModule(DataModule).StartTransaction(Connection);
      try
        History.Open;
        History.AddItem(Self.DataSet,Format(strStatusChanged,[Status.AsString,aNewStatus]),'','',nil,ACICON_STATUSCH);
        if not (OrderType.FieldByName('ISDERIVATE').AsString = 'Y') then
          begin
            DataSet.Edit;
            DataSet.FieldByName('DONE').AsString := 'Y';
            DataSet.Post;
          end;
        if (not Assigned(OrderType.FieldByName('DOCOPY'))) or (OrderType.FieldByName('DOCOPY').AsString <> 'N') then
          begin //Copy Order
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
            ImportFromXML(Copied,False,@ReplaceParentFields);
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
            Result:=True;
          end
        else
          begin
            with DataSet do
              begin
                Edit;
                FieldByName('STATUS').AsString := aNewStatus;
                FieldByName('DOAFQ').Clear;
                FieldByName('DWISH').Clear;
                FieldByName('VATH').Clear;
                FieldByName('VATF').Clear;
                FieldByName('NETPRICE').Clear;
                FieldByName('DISCOUNT').Clear;
                FieldByName('GROSSPRICE').Clear;
                FieldByName('NUMBER').Clear;
                FieldByName('DATE').Clear;
                if aOrderType <> 3 then
                  FieldByName('ODATE').Clear;
                Post;
              end;
            Result:=True;
          end;
        if not (OrderType.FieldByName('ISDERIVATE').AsString = 'Y') then
          begin //Auftrag geändert
            DataSet.Edit;
            DataSet.FieldByName('DONE').AsString := 'N';
            DataSet.Post;
          end;
        TBaseDBModule(DataModule).CommitTransaction(Connection);
        RefreshActive;
        OpenItem(False);
      except
        on e : Exception do
          begin
            TBaseDBModule(DataModule).RollbackTransaction(Connection);
            GotoBookmark(OldRec);
            raise;
          end;
      end;
    end;
  }
end;
procedure TOrder.ShippingOutput;
var
  OrderTyp: Integer;
  //aProcess: TProcess;
  CommaCount: Integer;
  tmp: String;
  Dispatchtypes: TDispatchTypes;
begin
  {
  Dispatchtypes := TDispatchTypes.Create(nil);
  if not OrderType.DataSet.Locate('STATUS', DataSet.FieldByName('STATUS').AsString, [loCaseInsensitive]) then
    raise Exception.Create(strStatusnotfound);
  OrderTyp := StrToIntDef(trim(copy(OrderType.FieldByName('TYPE').AsString, 0, 2)), 0);
  //Prüfung ob Versandart existiert
  if ((OrderTyp = 2) //Lieferschein
  or  (OrderTyp = 3)) //Rechnung
  then
    begin
      Data.SetFilter(Dispatchtypes,Data.QuoteField('ID')+'='+Data.QuoteValue(trim(copy(DataSet.FieldByName('SHIPPING').AsString,0,3))));
      if not Dispatchtypes.Locate('ID',copy(DataSet.FieldByName('SHIPPING').AsString,0,3),[loPartialKey]) then
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
      aProcess := TProcess.Create(Self);
      aProcess.ShowWindow := swoHide;
      aProcess.Options:= [poNoConsole,poWaitOnExit];
      aProcess.CommandLine := '"'+ExtractFileDir(ParamStr(0))+DirectorySeparator+'plugins'+DirectorySeparator;
      if pos(' ',Dispatchtypes.FieldByName('OUTPUTDRV').AsString) > 0 then
        begin
          aProcess.CommandLine := aProcess.Commandline+copy(Dispatchtypes.FieldByName('OUTPUTDRV').AsString,0,pos(' ',Dispatchtypes.FieldByName('OUTPUTDRV').AsString)-1)+ExtractFileExt(Paramstr(0))+'"';
          aProcess.Commandline := aProcess.Commandline+copy(Dispatchtypes.FieldByName('OUTPUTDRV').AsString,pos(' ',Dispatchtypes.FieldByName('OUTPUTDRV').AsString)+1,length(Dispatchtypes.FieldByName('OUTPUTDRV').AsString));
        end
      else aProcess.CommandLine := aProcess.Commandline+Dispatchtypes.FieldByName('OUTPUTDRV').AsString+ExtractFileExt(Paramstr(0))+'"';
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
  Dispatchtypes.Free;
  }
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
  {
  with BaseApplication as IBaseDbInterface do
    begin
      try
      try
        Result := True;
        OrderTyp := StrToIntDef(trim(copy(OrderType.FieldByName('TYPE').AsString, 0, 2)), 0);
        Masterdata := TMasterdata.CreateEx(nil,DataModule,Connection);
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
        FreeAndNil(Masterdata);
      end;
    end;
  }
end;
{
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
          FieldByName('NETPRICE').AsFloat   := Round(FieldByName('NETPRICE').AsFloat-Positions.FieldByName('POSPRICE').AsFloat);
          FieldByName('GROSSPRICE').AsFloat := Round(FieldByName('GROSSPRICE').AsFloat-Positions.FieldByName('GROSSPRICE').AsFloat);
        end
      else
        begin
          FieldByName('NETPRICE').AsFloat   := Round(FieldByName('NETPRICE').AsFloat+Positions.FieldByName('POSPRICE').AsFloat);
          FieldByName('GROSSPRICE').AsFloat := Round(FieldByName('GROSSPRICE').AsFloat+Positions.FieldByName('GROSSPRICE').AsFloat);
        end;
      Post;
    end;
end;
}
function TOrder.FormatCurrency(Value: real): string;
begin
  Result := FormatFloat('0.00',Value)+' '+Currency;
end;
function TOrder.CalcDispatchType: Boolean;
var
  aDisp: TDispatchTypes;
  aMasterdata: TMasterdata;
  aPosTyp: TPositionTyp;
begin
  {
  aPosTyp := TPositionTyp.CreateEx(Self,DataModule,Connection);
  aPosTyp.Open;
  if aPosTyp.DataSet.Locate('TYPE',6,[loCaseInsensitive]) then
    begin
      //Find new Dispatchtype
      aDisp := TDispatchTypes.CreateEx(Self,DataModule,Connection);
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
          aMasterdata := TMasterdata.CreateEx(Self,DataModule,Connection);
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
  }
end;

function TOrder.GetOrderTyp: Integer;
begin
  Result := -1;
  //if OrderType.DataSet.Locate('STATUS', DataSet.FieldByName('STATUS').AsString, [loCaseInsensitive]) then
  //  Result := StrToIntDef(trim(copy(OrderType.FieldByName('TYPE').AsString, 0, 2)), -1);
end;

function TOrder.SelectFromLink(aLink: string): Boolean;
begin
  {
  if pos('{',aLink) > 0 then
    aLink := copy(aLink,0,pos('{',aLink)-1)
  else if rpos('(',aLink) > 0 then
    aLink := copy(aLink,0,rpos('(',aLink)-1);
  with DataSet as IBaseManageDB do
    begin
      if copy(aLink,0,pos('@',aLink)-1) = TableName then
        begin
          if IsNumeric(copy(aLink,pos('@',aLink)+1,length(aLink))) then
            begin
              Select(copy(aLink,pos('@',aLink)+1,length(aLink)));
              result := True;
            end;
        end
      else
        Result:=inherited SelectFromLink(aLink);
    end;
  }}}
end;

function TOrder.Duplicate: Boolean;
var
  Copied: String;
  OldRec: LargeInt;
begin
  {
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
  with DataSet do
    begin
      Append;
      FieldByName('DOAFQ').Clear;
      FieldByName('DWISH').Clear;
      FieldByName('VATH').Clear;
      FieldByName('VATF').Clear;
      FieldByName('NETPRICE').Clear;
      FieldByName('DISCOUNT').Clear;
      FieldByName('GROSSPRICE').Clear;
    end;
  ImportFromXML(Copied,False,@ReplaceParentFields);
  with DataSet do
    begin
      Edit;
      FieldByName('NUMBER').Clear;
      FieldByName('DATE').Clear;
      FieldByName('ODATE').Clear;
      Post;
    end;
  Positions.EnableCalculation;
  }
end;

function TOrderPos.GetAccountNo: string;
begin
  {
  if Assigned(Order) and (Order.Address.Count>0) then
    Result:=Order.Address.FieldByName('ACCOUNTNO').AsString
  else inherited;
  }
end;
procedure TOrderPos.PosPriceChanged(aPosDiff, aGrossDiff: Extended);
begin
  {
  if not ((Order.DataSet.State = dsEdit) or (Order.DataSet.State = dsInsert)) then
    Order.DataSet.Edit;
  Order.FieldByName('NETPRICE').AsFloat := Round(Order.FieldByName('NETPRICE').AsFloat+aPosDiff);
  Data.PaymentTargets.Open;
  if Data.PaymentTargets.DataSet.Locate('ID',Order.FieldByName('PAYMENTTAR').AsString,[]) then
    Order.FieldByName('DISCPRICE').AsFloat := Round(Order.FieldByName('NETPRICE').AsFloat-((Order.FieldByName('NETPRICE').AsFloat/100)*Data.PaymentTargets.FieldByName('CASHDISC').AsFloat));
  Order.FieldByName('GROSSPRICE').AsFloat := Round(Order.FieldByName('GROSSPRICE').AsFloat+aGrossDiff);
  }
end;
procedure TOrderPos.PosWeightChanged(aPosDiff : Extended);
begin
  {
  if not ((Order.DataSet.State = dsEdit) or (Order.DataSet.State = dsInsert)) then
    Order.DataSet.Edit;
  Order.FieldByName('WEIGHT').AsFloat := Order.FieldByName('WEIGHT').AsFloat+aPosDiff;
  }
end;

function TOrderPos.Round(aValue: Extended): Extended;
begin
  if Assigned(Parent) then
    Result := (Parent as TOrder).Round(aValue)
  else Result:=inherited Round(aValue);
end;
function TOrderPos.RoundPos(aValue: Extended): Extended;
begin
  if (Parent as TOrder).SelectOrderType and ((Parent as TOrder).OrderType. FieldByName('ROUNDPOS').AsString='Y') then
    Result := Round(aValue)
  else result := aValue;
end;

function TOrderPos.GetCurrency: string;
begin
  Result:=(Parent as TOrder).Currency;
end;
function TOrderPos.GetOrderTyp: Integer;
begin
  Result := 0;
  {
  if Order.OrderType.DataSet.Locate('STATUS', Order.FieldByName('STATUS').AsString, [loCaseInsensitive]) then
    Result := StrToIntDef(trim(copy(Order.OrderType.FieldByName('TYPE').AsString, 0, 2)), 0);
  }
end;
constructor TOrderPos.Create(aParent: TPersistent);
begin
  inherited;
  FQMTest := TOrderQMTests.Create(Self);
  FOrderRepair := TOrderRepairs.Create(Self);
end;
destructor TOrderPos.Destroy;
begin
  FOrderRepair.Free;
  FQMTest.Free;
  inherited Destroy;
end;
procedure TOrderPos.Assign(aSource: TPersistent);
var
  aMasterdata: TMasterdata;
  tmpPID: String;
begin
  inherited Assign(aSource);
  {
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
  }
end;
procedure TOrderPos.FillDefaults;
begin
  {TODO
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
  }
end;
function TOrderList.GetHistory: TBaseHistory;
begin
  Result := FHistory;
end;

function TOrderList.GetOrderTyp: TOrdertypes;
begin
  Result := FOrderTyp;
end;

constructor TOrderList.Create(aParent: TPersistent);
begin
  inherited;
  FHistory := TBaseHistory.Create(Self);
  FOrderTyp := TOrderTypes.Create(Self);
end;

destructor TOrderList.Destroy;
begin
  FHistory.Free;
  FOrderTyp.Free;
  inherited Destroy;
end;

class function TOrderList.MapField(aField: string): string;
begin
  Result:=inherited MapField(aField);
  if Result = 'PQuantity' then
    Result := 'PQUATITY'
  ;
end;

class function TOrderList.GetRealTableName: string;
begin
  Result:='ORDERS';
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
      if OrderType.Locate('STATUS',Status,[]) then
        Result := TOrderTyp(OrderType.DataSet).Icon;
      FStatusCache.Values[FieldByName(GetStatusFieldName).AsString] := IntToStr(Result);
    end;
end;
procedure TOrderList.OpenItem(AccHistory: Boolean);
var
  //aObj: TObjects;
  aID: String;
  aFilter: String;
begin
  {
  if Self.Count=0 then exit;
  try
    try
      aObj := TObjects.Create(nil);
      if (DataSet.State<>dsInsert) and (DataSet.FieldByName('NUMBER').IsNull) then
        begin
          if not Data.TableExists(aObj.TableName) then
            begin
              aObj.CreateTable;
              aObj.Free;
              aObj := TObjects.CreateEx(nil,Data,nil,DataSet);
            end;
          with aObj.DataSet as IBaseDBFilter do
            begin
              aID := FieldByName('ORDERNO').AsString;
              if length(aID) > 4 then
                begin
                  aFilter :=         '('+Data.QuoteField('NUMBER')+'>='+Data.QuoteValue(copy(aID,0,length(aID)-2)+'00')+') and ';
                  aFilter := aFilter+'('+Data.QuoteField('NUMBER')+'<='+Data.QuoteValue(copy(aID,0,length(aID)-2)+'99')+')';
                end
              else
                aFilter :=  Data.QuoteField('NUMBER')+'='+Data.QuoteValue(aID);
              Filter := aFilter;
              Limit := 0;
            end;
          aObj.Open;
          if aObj.Count=0 then
            begin
              aObj.Insert;
              aObj.Text.AsString := Data.GetLinkDesc(Data.BuildLink(Self.DataSet));
              aObj.FieldByName('SQL_ID').AsVariant:=Self.Id.AsVariant;
              if Assigned(Self.Matchcode) then
                aObj.Matchcode.AsString := Self.Matchcode.AsString;
              if Assigned(Self.Status) then
                aObj.Status.AsString := Self.Status.AsString;
              aObj.Number.AsVariant:=Self.Number.AsVariant;
              aObj.FieldByName('LINK').AsString:=Data.BuildLink(Self.DataSet);
              aObj.FieldByName('ICON').AsInteger:=Data.GetLinkIcon(Data.BuildLink(Self.DataSet) read write ;
              aObj.Post;
              Self.GenerateThumbnail;
            end
          else //Modify existing
            begin
              while aObj.Count>1 do
                aObj.Delete;
              if CanEdit then
                aObj.Edit;
              if aObj.Text.AsString<>Data.GetLinkDesc(Data.BuildLink(Self.DataSet)) then
                begin
                  aObj.Edit;
                  aObj.Text.AsString := Data.GetLinkDesc(Data.BuildLink(Self.DataSet));
                end;
              if aObj.Number.AsString<>Self.Number.AsString then
                begin
                  aObj.Edit;
                  aObj.Number.AsString := Self.Number.AsString;
                end;
              if Assigned(Self.Status) and (aObj.Status.AsString<>Self.Status.AsString) then
                begin
                  aObj.Edit;
                  aObj.Status.AsString := Self.Status.AsString;
                end;
              if aObj.FieldByName('LINK').AsString<>Data.BuildLink(Self.DataSet) then
                begin
                  aObj.Edit;
                  aObj.FieldByName('LINK').AsString:=Data.BuildLink(Self.DataSet);
                end;
              if aObj.CanEdit then
                aObj.Post;
            end;
        end;
    finally
      aObj.Free;
    end;
  except
  end;
  }
end;

function TOrderList.SelectOrderType: Boolean;
begin
  Result := FOrderTyp.Locate('STATUS',Status,[]);
end;

initialization
  RegisterdataSetClass('ORDERS',TOrder);
  RegisterdataSetClass('ORDERPOS',TOrderPos);
  RegisterdataSetClass('ORDERQMTEST',TOrderQMTest);
  RegisterdataSetClass('ORDERREPAIR',TOrderRepair);
  RegisterdataSetClass('ORDERPOSTYP',TOrderPosTyp);
end.

