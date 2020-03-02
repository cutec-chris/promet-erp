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
unit uBaseERPDBClasses;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, uBaseDbClasses, db, ContNrs, uIntfStrConsts,uBaseDatasetInterfaces2;
type
  TPostResult = (prSuccess,prAlreadyPosted,prFailed);
  IPostableDataSet = interface['{26EC4496-0D5A-4BFC-A712-C9001F5A0599}']
    function DoPost : TPostResult;
    function FailMessage : string;
  end;
  IShipableDataSet = interface['{1B460C3D-F5C3-4CD6-BD09-01F54683D661}']
    procedure ShippingOutput;
  end;
  TBaseERPList = class(TBaseDbList)
  private
    FUpdateHistory: Boolean;
  protected
    property UpdateHistory : Boolean read FUpdateHistory write FUpdateHistory;
  public
  published
  end;
  TStorageTypes = class(TBaseDBDataSet)
  private
    FID,FName,FDefaultSt : string;
  public
  published
    property ID : string index 3 read FID write FID;
    property NAME : string index 30 read FName write FName;
    property DEFAULTST : string index 1 read FDefaultSt write FDefaultSt;
  end;
  TCurrency = class(TBaseDBDataSet)
  private
    FSymbol,FName,FMask,FDefaultCUR : string;
    FDecimalPL : SmallInt;
    FFactor,FRoundGran : Double;
  public
    function Convert(Price: real; SourceCurr, TargetCurr: string): real;
  published
    property Symbol : string index 5 read FSymbol write FSymbol;
    property Name : string index 40 read FName write FName;
    property Mask : string index 20 read FMask write FMask;
    property DecimalPL : SmallInt read FDecimalPL write FDecimalPL;
    property Factor : double read FFactor write FFactor;
    property DefaultCur : string index 1 read FDefaultCUR write FDefaultCUR;
    property RoundGran: double read FRoundGran write FRoundGran;
  end;
  TPaymentTargets = class(TBaseDBDataSet)
  private
    FId,FName,FText,FDefaultPT : string;
    FAAccounts : TBlobData;
    FCashDisc : double;
    FCashDiscD,FDays : Integer;
  public
  published
    property ID : string index 2 read FID write FID;
    property Name : string index 10 read FName write FName;
    property Text : string index 30 read FText write FText;
    property FAccounts : TBlobData read FAAccounts write FAAccounts;
    property CashDisc : double read FCashDisc write FCashDisc;               //Skonto
    property CashDiscD : Integer read FCashDiscD write FCashDiscD;            //Skonto Tage
    property Days : Integer read FDays write FDays;                 //Tage
    property DefaultPt : string index 1 read FDefaultPT write FDefaultPT;
  end;

  { TPositionTyp }

  TPositionTyp = class(TBaseDBDataSet)
  private
    FName,FType,FText : string;
  public
    class function GetRealTableName: string; override;
    class function MapField(aField: string): string; override;
  published
    property Name : string index 3 read FName write FName;
    property Typ : string index 1 read FType write FType;
    property Text : string index 60 read FText write FText;
  end;
  TPriceType = class(TBaseDBDataSet)
  private
    FType,FSymbol,FName : string;
  public
  published
    property Typ : string index 1 read FType write FType;
    property Symbol : string index 4 read FSymbol write FSymbol;
    property Name : string index 40 read FName write FName;
  end;

  { TPriceTypes }

  TPriceTypes = class(TAbstractMasterDetail)
  public
    class function GetObjectTyp: TClass; override;
    function Get(aSymbol : string) : Integer;
  end;

  TVat = class(TBaseDBDataSet)
  private
    FId : Integer;
    FValue : double;
    FName,FFKey : string;
  published
    property ID : Integer read FId write FId;
    property VALUE : double read FValue write FValue;
    property NAME : string index 50 read FName write FName;
    property FKEY : string index 10 read FFKey write FFKey; //Fibu Schlüssel (Financial accounting key ??)
  end;
  TBaseDBPosition = class;
  TPositionCalc = class(TbaseDBDataSet)
  private
    FRefID : Int64;
    FType,FCustomer : string;
    FPrice,FMinCount,FMaxCount : double;
  protected
    class function MapField(aField: string): string; override;
    class function GetRealTableName: string; override;
  public
  published
    property Ref_Id : Int64 read FRefID write FRefID;
    property Typ : string index 4 read FType write FType;
    property Price : double read FPrice write FPrice;
    property MinCount : double read FMinCount write FMinCount;
    property MaxCount : double read FMaxCount write FMaxCount;
    property Customer : string index 20 read FCustomer write FCustomer;
  end;
  TBaseDBPosition = class(TBaseDbDataSet)
  private
    //FImages: TImages;
    FPosNo : Integer;
    FPosFormat: string;
    FPosTyp,FTPosNo,FIdent,FVersion,FLanguage,FTextType,FShorttext,
      FStorage,FSerial,FManufacNr,FUnit,FScript,FScriptVer,FScriptFunc,FPrepText,FWorkText,FChangedBy,FCreatedBy: string;
    FWeight,FAvalible,FQuantity,FQuantityD,FQuantityC,FQuantityO,FSetupTime,FPlanTime,FTime,FBufferTime,
      FPurchase,FSellprice,FComprice,FDiscount,FRepairtime,FPosPrice,FGrossPrice : double;
    FActive : Boolean;
    FText : string;
    FVat : SmallInt;
    FImageRef,FParent,FOldId : Int64;
    FDelivery,FStartDate,FEarliest,FLatest : TDateTime;
    FCalculationDisabled : Integer;
    FDoNumber: Boolean;
    FUseRTF: Boolean;
    OldPosPrice : real;
    OldGrossPrice : real;
    OldPosWeight : real;
    FOldPosNo: Integer;
    PriceTypes : TPriceTypes;
    function GetIdent: TField;
    function GetPosNo: TField;
    function GetPosTyp: TPositionTyp;
    procedure DoCalcPosPrice(Setprice : Boolean = False);
    procedure DoCalcGrossPosPrice;
    procedure SetGrossPrice(AValue: double);
    procedure SetPosTyp(AIndex: Integer; AValue: string);
    procedure SetQuantity(AValue: double);
    procedure SetSellProce(AValue: double);
    procedure SetVat(AValue: SmallInt);
  protected
    FPosCalc: TPositionCalc;
    function GetAccountNo : string;virtual;
    procedure PosPriceChanged(aPosDiff,aGrossDiff :Extended);virtual;
    function Round(aValue : Extended) : Extended;virtual;
    function RoundPos(aValue : Extended) : Extended;virtual;
    procedure PosWeightChanged(aPosDiff :Extended);virtual;
    function IsOrderToSupplier : Boolean;virtual;
    function IsProductionOrder : Boolean;virtual;
    function GetCurrency : string;virtual;abstract;
    function GetPosTypDec : Integer;
    function GetOrderTyp : Integer;virtual;
    procedure DoModifyPosPrice;virtual;
    procedure DoDataChange(Sender: TObject; Field: TField);virtual;
    procedure DoBeforeDelete;virtual;
    procedure DoDelete;virtual;
    procedure DoPost;virtual;
    procedure DoInsert;virtual;
    procedure DoEdit;virtual;
  public
    constructor CreateEx(Owner: TPersistent;Module: TComponent); override;
    destructor Destroy;override;
    procedure FillDefaults;override;
    class function MapField(aField: string): string; override;
    procedure Assign(Source: TPersistent); override;
    property PosTyp : TPositionTyp read GetPosTyp;
    property PosTypDec : Integer read GetPosTypDec;
    property PosCalc : TPositionCalc read FPosCalc;
    //property Images : TImages read FImages;
    property PosFormat : string read FPosFormat write FPosFormat;
    property CanHandleRTF : Boolean read FUseRTF write FUseRTF;
    procedure DisableCalculation;
    procedure EnableCalculation;
    function IsCalculationDisabled : Boolean;
    procedure AppendSubTotal;
    //Fields
  published
    property PosNo : Integer read FPosNo write FPosNo;
    property PosType : string index 3 read FPosTyp write SetPosTyp;
    property Active : Boolean read FActive write FActive;
    property TPosNo : string index 15 read FTPosNo write FTPosNo;                //Auschreibungsnummer
    property Ident : string index 40 read FIdent write FIdent;
    property Version : string index 25 read FVersion write FVersion;
    property Language : string index 3 read FLanguage write FLanguage;
    property TextType : string index 1 read FTextType write FTextType;
    property Shorttext : string index 200 read FShorttext write FShorttext;
    property Text : string read FText write FText;
    property Storage : string index 3 read FStorage write FStorage;                //Lagerentname
    property Serial : string index 20 read FSerial write FSerial;                //Serienummer
    property ManufacNr : string index 40 read FManufacNr write FManufacNr;
    property Weight : double read FWeight write FWeight;
    property Avalible : double read FAvalible write FAvalible;                //verfügbar
    property Delivery : TDateTime read FDelivery write FDelivery;                 //wann verfügbar
    property Quantity : double read FQuantity write SetQuantity;                //Menge
    property QuantityD : double read FQuantityD write FQuantityD;               //Menge Geliefert
    property QuantityC : double read FQuantityC write FQuantityC;               //Menge berechnet
    property QuantityO : double read FQuantityO write FQuantityO;               //Auftragsmenge
    property QuantityU : string index 10 read FUnit write FUnit;             //Mengeneinheit
    property SetupTime : double read FSetupTime write FSetupTime;               //Rüstzeit
    property PlanTime : double read FPlanTime write FPlanTime;                //geplante Zeit
    property Time : double read FTime write FTime;                    //benötigte Zeit
    property BufferTime : double read FBufferTime write FBufferTime;              //Wartezeit (wann darf nächste Aufgabe frühestens starten)
    property StartDate : TDateTime read FStartDate write FStartDate;
    property DueDate : TDateTime read FStartDate write FStartDate;
    property Earliest : TDateTime read FEarliest write FEarliest;
    property Latest : TDateTime read FLatest write FLatest;

    property Purchase : double read FPurchase write FPurchase;                //Einkaufspreis
    property SellPrice : double read FSellprice write SetSellProce;               //Verkaufspreis
    property ComPrice : double read FComprice write FComprice;                //Common Price
    property Discount : double read FDiscount write FDiscount;                //Rabatt
    property Vat : SmallInt read FVat write SetVat;                  //MwSt Typ
    property RepairTime : double read FRepairtime write FRepairtime;              //reparaturzeit
    property PosPrice : double read FPosPrice write FPosPrice;                //Gesamtpreis
    property GrossPrice : double read FGrossPrice write SetGrossPrice;              //Bruttoprice
    property ImageRef : Int64 read FImageRef write FImageRef;
    property ParentID : Int64 read FParent write FParent;
    property Old_Id : Int64 read FOldId write FOldId;
    property Script : string index 60 read FScript write FScript;
    property ScriptVer : string index 8 read FScriptVer write FScriptVer;
    property ScriptFunc : string index 60 read FScriptFunc write FScriptFunc;
    property PrepText : string index 100 read FPrepText write FPrepText;
    property WorkText : string index 100 read FWorkText write FWorkText;
    property ChangedBy : string index 4 read FChangedBy write FChangedBy;
    property CreatedBy : string index 4 read FCreatedBy write FCreatedBy;
  end;
  TStorageType = class(TBaseDBDataSet)
  private
    FId,FName : string;
    FDefaultSt : Boolean;
  published
    property ID : string index 3 read FId write FId;
    property Name : string index 30 read FName write FName;
    property DefaultSt : Boolean read FDefaultSt write FDefaultSt;
  end;
  TStorageJournal = class(TBaseDBDataSet)
  private
    FStorageID,FOStatus,FType,FId,FVersion,FLanguage,FSerial,FNote,FQuantityU : string;
    FOrderNo,FPosNo : Integer;
    FQuantity : double;
  published
    property StorageID : string index 3 read FStorageID write FStorageID;
    property OrderNo : Integer read FOrderNo write FOrderNo;
    property OStatus : string index 3 read FOStatus write FOStatus;
    property PosNo : Integer read FPosNo write FPosNo;
    property Typ : string index 1 read FType write FType;
    property ID : string index 20 read FId write FId;
    property Version : string index 25 read FVersion write FVersion;
    property Language : string index 3 read FLanguage write FLanguage;
    property Serial : string index 30 read FSerial write FSerial;
    property Note : string index 500 read FNote write FNote;
    property Quantity : double read FQuantity write FQuantity;
    property QuantityU : string index 10 read FQuantityU write FQuantityU;
  end;
  TCountries = class(TBaseDBDataSet)
  private
    FId,FName,FLanguage : string;
  published
    property ID : string index 3 read FId write FId;
    property Name : string index 30 read FName write FName;
    property Language : string index 2 read FLanguage write FLanguage;
  end;
  TLanguages = class(TBaseDbDataSet)
  private
    FLanguage,FISO6391,FISO6392,FDateformat,FTimeFormat,FSTDCURR,FDECSEP,FTHOUSEP,
      FCountry,FDEFAULTLNG : string;
    FSaturation,FCompclose,FTitles : TBlobData;
  published
    property Language : string index 30 read FLanguage write FLanguage;
    property ISO6391 : string index 2 read FISO6391 write FISO6391;
    property ISO6392 : string index 3 read FISO6392 write FISO6392;
    property DateFormat : string index 20 read FDateformat write FDateformat;
    property TimeFormat : string index 8 read FTimeFormat write FTimeFormat;
    property STDCurr : string index 3 read FSTDCURR write FSTDCURR;
    property DecSep : string index 1 read FDECSEP write FDECSEP;
    property ThouSep : string index 1 read FTHOUSEP write FTHOUSEP;
    property Satutation : TBlobData read FSaturation write FSaturation;
    property Compclose: TBlobData read FCompclose write FCompclose;
    property Titles : TBlobdata read FTitles write FTitles;
    property Country : string index 30 read FCountry write FCountry;
    property DefalulLng : string index 1 read FDEFAULTLNG write FDEFAULTLNG;
  end;
  TStates = class(TBaseDBDataSet)
  private
    FType,FStatus,FStatusName,FDerivate,FColor : string;
    FActive : Boolean;
    FIcon : Integer;
  published
    property TYP : string index 1 read FType write FType;
    property STATUS : string index 4 read FStatus write FStatus;
    property STATUSNAME : string index 30 read FStatusName write FStatusName;
    property DERIVATIVE : string index 30 read FDerivate write FDerivate;
    property ACTIVE : Boolean read FActive write FActive;
    property COLOR : string index 8 read FColor write FColor;
    property ICON : Integer read FIcon write FIcon;
  end;
  TCategory = class(TBaseDBDataSet)
  private
    FType,FName,FColor : string;
    FActive : Boolean;
  published
    property TYP : string index 1 read FType write  FType;
    property NAME : string index 60 read FName write FName;
    property COLOR : string index 30 read FColor write FColor;
    property ACTIVE : Boolean read FActive write FActive;
  end;

  { TFinancialAccounts }

  TFinancialAccounts = class(TBaseDBDataSet)
  private
    FAccountno,FName : string;
    FActive : Boolean;
  public
    class function GetRealTableName: string; override;
  published
    property Accountno : string index 10 read FAccountno write FAccountno;
    property Name : string index 60 read FName write FName;
    property Active : Boolean read FActive write FActive;
  end;
  TUnits = class(TBaseDBDataSet)
  private
    FName : string;
  published
    property Name : string index 160 read FName write FName;
  end;
  TDispatchType = class(TBaseDBDataSet)
  private
    FId,FCountry,FName,FOutputDrv,FArticle : string;
    FWeight : double;
  published
    property ID : string index 3 read FID write FID;
    property Country : string index 3 read FCountry write FCountry;
    property Name : string index 20 read FName write FName;
    property OutputDrv : string index 60 read FOutputDrv write FOutputDrv;
    property Weight : Double read FWeight write FWeight;
    property Article : string index 40 read FArticle write FArticle;
  end;

  { TDispatchTypes }

  TDispatchTypes = class(TAbstractMasterDetail)
  public
    class function GetObjectTyp: TClass; override;
    procedure SelectByCountryAndWeight(aCountry: string;aWeight : real);
  end;
  TRepairProblems = class(TBaseDBDataSet)
  private
    FProblem : string;
  published
    property Problem : string index 60 read FProblem write FProblem;
  end;
  TTextTypes = class(TBaseDBDataSet)
  private
    FName : string;
    FTyp : Integer;
  published
    property Name : string index 20 read FName write FName;
    property Typ : Integer read FTyp write FTyp;
  end;
  TInventoryPos = class(TBaseDbDataSet)
  private
    FIdent,FVersion,FLanguage,FShorttext,FStorage,FQuantityU,FCurrency : string;
    FPosNo : Integer;
    FQuantity,FQuantityC,FPurchase,FPrice : double;
  public
    property PosNo : Integer read FPosNo write FPosNo;
    property Ident : string index 20 read FIdent write FIdent;
    property Version : string index 25 read FVersion write FVersion;
    property Language : string index 3 read FLanguage write FLanguage;
    property Shorttext : string index 200 read FShorttext write FShorttext;
    property Storage : string index 3 read FStorage write FStorage;
    property Quantity : double read FQuantity write FQuantity;
    property QuantityC : double read FQuantityC write FQuantityC;
    property QuantityU : string index 10 read FQuantityU write FQuantityU;
    property Purchase : double read FPurchase write FPurchase;
    property Price : double read FPrice write FPrice;
    property Currency : string index 5 read FCurrency write FCurrency;
  end;
  TInventoryPositions = class(TBaseERPList)
  end;

  { TInventorys }

  TInventory = class(TBaseDbList)
  private
    FPos: TInventoryPositions;
    FInvNo : Integer;
    FDesc,FStatus,FCreatedBy : string;
    FDate : TDateTime;
    FPurchase,FPurchased,FPrice,FPriced : double;
  public
    procedure FillDefaults; override;
  published
    property InvNo : Integer read FInvNo write FInvNo;
    property Desc : string index 30 read FDesc write FDesc;
    property Status : string index 4 read FStatus write FStatus;
    property Date : TDateTime read FDate write FDate;
    property Purchase : double read FPurchase write FPurchase;
    property Price : double read FPrice write FPrice;
    property Purchased : double read FPurchased write FPurchased;
    property Priced : double read FPriced write FPriced;
    property CreatedBy : string index 4 read FCreatedBy write FCreatedBy;
    property Positions : TInventoryPositions read FPos;
  end;
  function InternalRound(Value: Extended;nk : Integer = 4): Extended;
//Often used and not often changed global Tables
var
  TextTyp : TTextTypes;
  Vat : TVat;
  Units : TUnits;
  DispatchTypes : TDispatchTypes;
  PriceTypes : TPriceTypes;
  RepairProblems : TRepairProblems;
implementation
uses Math,Variants,uRTFtoTXT,uData;
resourcestring
  strEdited                        = 'bearbeitet';
  strCreated                       = 'erstellt';
  strStorageTypes                  = 'Lagertypen';
  strSubTotal                      = 'Zwischensumme';
function InternalRound(Value: Extended;nk : Integer = 4): Extended;
var
  multi,nValue: Extended;
begin
  multi := IntPower(10, nk);
  nValue := (Value*multi);
  Result := (Trunc(nValue) + Trunc (Frac(nValue) * 2))/multi;
end;

{ TDispatchTypes }

class function TDispatchTypes.GetObjectTyp: TClass;
begin
  Result := TDispatchType;
end;

procedure TDispatchTypes.SelectByCountryAndWeight(aCountry: string;
  aWeight: real);
var
  aFilter: String;
begin
  {
  with  DataSet as IBaseDBFilter, BaseApplication as IBaseDBInterface, DataSet as IBaseManageDB do
    begin
      Filter := '('+QuoteField('COUNTRY')+'='+QuoteValue(aCountry)+') AND ('+QuoteField('WEIGHT')+'>='+QuoteValue(FloatToStr(aWeight))+')';
      SortFields:='WEIGHT';
    end;
  }
end;

{ TFinancialAccounts }

class function TFinancialAccounts.GetRealTableName: string;
begin
  Result:='FACCOUNTS';
end;
procedure TInventory.FillDefaults;
begin
  //FieldByName('INVNO').AsInteger := RecordCount+1;
end;

class function TPriceTypes.GetObjectTyp: TClass;
begin
  Result := TPriceType;
end;

function TPriceTypes.Get(aSymbol: string): Integer;
begin
  Result := 0;
  if Locate('SYMBOL', trim(aSymbol), []) then
    Result := StrToIntDef(copy(FieldByName('TYPE').AsString, 0, 2), 0);
end;
class function TPositionCalc.MapField(aField: string): string;
begin
  Result := aField;
  if aField = 'TYP' then Result := 'TYPE';
end;

class function TPositionCalc.GetRealTableName: string;
begin
  Result:='ORDERPOSCALC';
end;
class function TPositionTyp.GetRealTableName: string;
begin
  Result := 'ORDERPOSTYP';
end;

class function TPositionTyp.MapField(aField: string): string;
begin
  Result:=inherited MapField(aField);
  if Result = 'Typ' then
    Result := 'TYPE'
  ;
end;

function TBaseDBPosition.GetIdent: TField;
begin
  Result := FieldByName('IDENT');
end;
function TBaseDBPosition.GetPosNo: TField;
begin
  Result := FieldByName('POSNO');
end;
function TBaseDBPosition.GetPosTyp: TPositionTyp;
begin
  Result := nil;
  {FPosTyp.Open;
  FPosTyp.DataSet.Locate('NAME',Self.FieldByName('POSTYP').AsVariant,[loCaseInsensitive]);
  Result := FPosTyp;}
end;
function TBaseDBPosition.GetPosTypDec: Integer;
var
  aTyp: TPositionTyp;
begin
  aTyp := GetPosTyp;
  Result := -1;
  if Assigned(aTyp) then
    Result := StrToIntDef(StringReplace(Trim(aTyp.FieldByName('TYPE').AsString),#13,'',[rfReplaceAll]),-1);
end;
procedure TBaseDBPosition.DoModifyPosPrice;
var
  DecimalPlaces: Integer = -2;
  tmp: Extended;
begin
  if FCalculationDisabled > 0 then exit;
  try
  {
  if (not DataSet.FieldByName('QUANTITY').IsNull) and (not TryStrToFloat(DataSet.FieldByName('QUANTITY').AsString,tmp)) then exit;
  if (not DataSet.FieldByName('SELLPRICE').IsNull) and (not TryStrToFloat(DataSet.FieldByName('SELLPRICE').AsString,tmp)) then exit;
  if (not DataSet.FieldByName('COMPRICE').IsNull) and (not TryStrToFloat(DataSet.FieldByName('COMPRICE').AsString,tmp)) then exit;
  if (not DataSet.FieldByName('DISCOUNT').IsNull) and (not TryStrToFloat(DataSet.FieldByName('DISCOUNT').AsString,tmp)) then exit;
  DisableCalculation;
    //Menge
    tmp := DataSet.FieldByName('SELLPRICE').AsFloat*DataSet.FieldByName('QUANTITY').AsFloat;
    //+CommonPrice
    tmp := tmp+DataSet.FieldByName('COMPRICE').AsFloat;
    //*Discont
    tmp := tmp-(tmp*DataSet.FieldByName('DISCOUNT').AsFloat/100);
    if not ((State = dsInsert) or (State = dsEdit)) then
      DataSet.Edit;
    if (GetPosTypDec = 1)
    or (GetPosTypDec = 2)
    or (GetPosTypDec = 4)
    then
      DataSet.FieldByName('POSPRICE').AsFloat := 0
    else
      DataSet.FieldByName('POSPRICE').AsFloat := tmp;
    if DataSet.FieldByName('VAT').AsString <> '' then
      begin
        with BaseApplication as IBaseDbInterface do
          begin
            if not Vat.DataSet.Active then
              Vat.Open;
            Vat.DataSet.Locate('ID',VarArrayof([DataSet.FieldByName('VAT').AsString]),[]);
            DataSet.FieldByName('GROSSPRICE').AsFloat := RoundPos(DataSet.FieldByName('POSPRICE').AsFloat*(1+(Vat.FieldByName('VALUE').AsFloat/100)));
          end;
      end;
      }
  finally
    EnableCalculation;
  end;
end;
procedure TBaseDBPosition.DoDataChange(Sender: TObject; Field: TField);
begin
end;
procedure TBaseDBPosition.DoBeforeDelete;
begin
end;
procedure TBaseDBPosition.DoDelete;
begin
end;
procedure TBaseDBPosition.DoPost;
begin
end;
procedure TBaseDBPosition.DoInsert;
begin
end;
procedure TBaseDBPosition.DoEdit;
begin
end;
procedure TBaseDBPosition.DoCalcPosPrice(Setprice: Boolean);
var
  CSPrice,
  Sprice,
  CPrice,
  Price : real;
  APrice : real;
  AMPrice : real;
  EKPrice : real;
  Rec,CRec,SRec,CSRec : Variant;
begin
  DisableCalculation;
  {
  try
    //Einkaufspreis
    EKPrice := 0;
    //Priorität hat Kundenpreis+Staffel
    CSPrice := 0;
    //Danach Staffelpreis
    SPrice := 0;
    //Danach Kunden Preis
    CPrice := 0;
    //Danach Allgemeiner Preis
    Price := 0;

    AMPrice := 0;
    APrice := 0;

    //zuerst alle 3 suchen und Allgemeinpreis dabei errechnen
    //Ausgelassen werden alle Allgemeinpreise die nicht in den MAX und MINCount passen
    PosCalc.Open;
    with BaseApplication as IBaseDbInterface do
      begin
        PriceTypes.Open;
        if Assigned(PosCalc) then
          begin
            PosCalc.DataSet.First;
            while not PosCalc.DataSet.EOF do
              begin
                //Einkauf
                if PriceTypes.Get(PosCalc.FieldByName('TYPE').AsString) = 1 then
                  EKPrice := PosCalc.FieldByName('PRICE').AsFloat;
                //Allgemeinpreis mengenunabhängig
                if PriceTypes.Get(PosCalc.FieldByName('TYPE').AsString) = 6 then
                  APrice := APrice+PosCalc.FieldByName('PRICE').AsFloat
                //Allgemeinpreis mengenabhängig
                else if  (PriceTypes.Get(PosCalc.FieldByName('TYPE').AsString) = 5)
                     and (DataSet.FieldByName('QUANTITY').AsFloat >= PosCalc.FieldByName('MINCOUNT').AsFloat)
                     and (DataSet.FieldByName('QUANTITY').AsFloat <= PosCalc.FieldByName('MAXCOUNT').AsFloat) then
                  AMPrice := AMPrice+PosCalc.FieldByName('PRICE').AsFloat
                //Verkaufspreis
                else if (PriceTypes.Get(PosCalc.FieldByName('TYPE').AsString) = 4) then
                  begin
                    if  (PosCalc.FieldByName('CUSTOMER').AsString = GetAccountNo)
                    and (not PosCalc.FieldByName('MINCOUNT').IsNULL)
                    and (DataSet.FieldByName('QUANTITY').AsFloat >= PosCalc.FieldByName('MINCOUNT').AsFloat)
                    and ((DataSet.FieldByName('QUANTITY').AsFloat <= PosCalc.FieldByName('MAXCOUNT').AsFloat) or (PosCalc.FieldByName('MAXCOUNT').IsNull)) then
                      begin
                        CSRec := PosCalc.GetBookmark;
                        CSPrice := PosCalc.FieldByName('PRICE').AsFloat
                      end
                    else if (not PosCalc.FieldByName('MINCOUNT').IsNULL)
                    and (DataSet.FieldByName('QUANTITY').AsFloat >= PosCalc.FieldByName('MINCOUNT').AsFloat)
                    and ((DataSet.FieldByName('QUANTITY').AsFloat <= PosCalc.FieldByName('MAXCOUNT').AsFloat) or (PosCalc.FieldByName('MAXCOUNT').IsNull)) then
                      begin
                        SRec := Data.GetBookmark(PosCalc);
                        SPrice := PosCalc.FieldByName('PRICE').AsFloat
                      end
                    else if  (PosCalc.FieldByName('CUSTOMER').AsString = GetAccountNo)
                    and (PosCalc.FieldByName('MINCOUNT').IsNULL) then
                      begin
                        CRec := Data.GetBookmark(PosCalc);
                        CPrice := PosCalc.FieldByName('PRICE').AsFloat
                      end
                    else if (PosCalc.FieldByName('CUSTOMER').AsString = '')
                    and (PosCalc.FieldByName('MINCOUNT').IsNULL) then
                      begin
                        Rec := Data.GetBookmark(PosCalc);
                        Price := PosCalc.FieldByName('PRICE').AsFloat;
                      end;
                  end;
                PosCalc.DataSet.Next;
              end;
          end;
        if not ((State = dsInsert) or (State = dsEdit)) then
          DataSet.Edit;
        if CSPrice > 0 then
          begin
            if not SetPrice then
              DataSet.FieldByName('SELLPRICE').AsFloat := CSPrice+AMPrice
            else if Data.GotoBookmark(PosCalc,CSRec) then
              begin
                if (PosCalc.DataSet.State <> dsEdit) and (PosCalc.DataSet.State <> dsInsert) then
                  PosCalc.DataSet.Edit;
                PosCalc.FieldByName('PRICE').AsFloat := DataSet.FieldByName('SELLPRICE').AsFloat;
              end;
          end
        else if SPrice > 0 then
          begin
            if not SetPrice then
              DataSet.FieldByName('SELLPRICE').AsFloat := SPrice+AMPrice
            else if Data.GotoBookmark(PosCalc,SRec) then
              begin
                if (PosCalc.DataSet.State <> dsEdit) and (PosCalc.DataSet.State <> dsInsert) then
                  PosCalc.DataSet.Edit;
                PosCalc.FieldByName('PRICE').AsFloat := DataSet.FieldByName('SELLPRICE').AsFloat;
              end;
          end
        else if CPrice > 0 then
          begin
            if not SetPrice then
              DataSet.FieldByName('SELLPRICE').AsFloat := CPrice+AMPrice
            else if Data.GotoBookmark(PosCalc,CRec) then
              begin
                if (PosCalc.DataSet.State <> dsEdit) and (PosCalc.DataSet.State <> dsInsert) then
                  PosCalc.DataSet.Edit;
                PosCalc.FieldByName('PRICE').AsFloat := DataSet.FieldByName('SELLPRICE').AsFloat;
              end;
          end
        else
          begin
            if not SetPrice then
              DataSet.FieldByName('SELLPRICE').AsFloat := Price+AMPrice
            else if Data.GotoBookmark(PosCalc,Rec) then
              begin
                if (PosCalc.DataSet.State <> dsEdit) and (PosCalc.DataSet.State <> dsInsert) then
                  PosCalc.DataSet.Edit;
                PosCalc.FieldByName('PRICE').AsFloat := DataSet.FieldByName('SELLPRICE').AsFloat;
              end
            else
              begin
                if PosCalc.Count = 0 then
                  PosCalc.DataSet.Append
                else if not PosCalc.CanEdit then
                  PosCalc.DataSet.Edit;
                PriceTypes.DataSet.Locate('TYPE',4,[]);
                PosCalc.FieldByName('TYPE').AsString := Pricetypes.FieldByName('SYMBOL').AsString;
                PosCalc.FieldByName('PRICE').AsFloat := DataSet.FieldByName('SELLPRICE').AsFloat;
              end;
          end;
        DataSet.FieldByName('COMPRICE').AsFloat := APrice;
        DataSet.FieldByName('PURCHASE').AsFloat := EKPrice;
      end;
  finally
    EnableCalculation;
    PosCalc.DataSet.EnableControls;
    DataSet.EnableControls;
    DoModifyPosPrice;
  end;
  }
end;
procedure TBaseDBPosition.DoCalcGrossPosPrice;
var
  tmp: Extended = 0;
begin
  if FCalculationDisabled > 0 then exit;
  {
  if (not DataSet.FieldByName('QUANTITY').IsNull) and (not TryStrToFloat(DataSet.FieldByName('QUANTITY').AsString,tmp)) then exit;
  if (not DataSet.FieldByName('DISCOUNT').IsNull) and (not TryStrToFloat(DataSet.FieldByName('DISCOUNT').AsString,tmp)) then exit;
  DisableCalculation;
  PosCalc.DataSet.DisableControls;
  try
    //remove vat
    tmp := DataSet.FieldByName('GROSSPRICE').AsFloat;
    if DataSet.FieldByName('VAT').AsString <> '' then
      begin
        with BaseApplication as IBaseDbInterface do
          begin
            if not Vat.DataSet.Active then
              Vat.Open;
            Vat.DataSet.Locate('ID',VarArrayof([DataSet.FieldByName('VAT').AsString]),[]);
            tmp := DataSet.FieldByName('GROSSPRICE').AsFloat/(1+(Vat.FieldByName('VALUE').AsFloat/100));
          end;
      end;
    if (GetPosTypDec = 1)
    or (GetPosTypDec = 2) then
      begin
        DataSet.FieldByName('POSPRICE').AsFloat := 0;
        DataSet.FieldByName('GROSSPRICE').AsFloat := 0;
      end
    else
      DataSet.FieldByName('POSPRICE').AsFloat := tmp;
    // div Discont
    tmp := tmp+(tmp*DataSet.FieldByName('DISCOUNT').AsFloat/100);
    if not ((State = dsInsert) or (State = dsEdit)) then
      DataSet.Edit;
    //-CommonPrice
    tmp := tmp-DataSet.FieldByName('COMPRICE').AsFloat;
    // div Menge
    if Assigned(DataSet.FieldByName('SELLPRICE')) and Assigned(DataSet.FieldByName('QUANTITY')) then
      DataSet.FieldByName('SELLPRICE').AsFloat := tmp/DataSet.FieldByName('QUANTITY').AsFloat;

  finally
    EnableCalculation;
    PosCalc.DataSet.EnableControls;
    DoCalcPosPrice(True);
  end;
  }
end;

procedure TBaseDBPosition.SetGrossPrice(AValue: double);
begin
  if FGrossPrice=AValue then Exit;
  FGrossPrice:=AValue;
  DoCalcGrossPosPrice;
end;

procedure TBaseDBPosition.SetPosTyp(AIndex: Integer; AValue: string);
begin
  if FPosTyp=AValue then exit;
  FPosTyp:=AValue;
  DoModifyPosPrice;
end;
procedure TBaseDBPosition.SetQuantity(AValue: double);
begin
  if FQuantity=AValue then Exit;
  FQuantity:=AValue;
  DoModifyPosPrice;
end;

procedure TBaseDBPosition.SetSellProce(AValue: double);
begin
  if FSellprice=AValue then Exit;
  FSellprice:=AValue;
  DoCalcPosPrice(True);
end;

procedure TBaseDBPosition.SetVat(AValue: SmallInt);
begin
  if FVat=AValue then Exit;
  FVat:=AValue;
  DoModifyPosPrice;
end;

function TBaseDBPosition.GetAccountNo: string;
begin
  Result := '';
end;
procedure TBaseDBPosition.PosPriceChanged(aPosDiff, aGrossDiff: Extended);
begin
end;

function TBaseDBPosition.Round(aValue: Extended): Extended;
begin
  Result := InternalRound(aValue);
end;

function TBaseDBPosition.RoundPos(aValue: Extended): Extended;
begin
  Result := aValue;
end;

procedure TBaseDBPosition.PosWeightChanged(aPosDiff: Extended);
begin
end;
function TBaseDBPosition.IsOrderToSupplier: Boolean;
begin
  Result := False;
end;
function TBaseDBPosition.IsProductionOrder: Boolean;
begin
  Result := False;
end;
function TBaseDBPosition.GetOrderTyp: Integer;
begin
  Result := 0;
end;
constructor TBaseDBPosition.CreateEx(Owner: TPersistent; Module: TComponent);
begin
  inherited CreateEx(Owner,Module);
  FUseRTF:=False;
  FPosFormat := '%d';
  //FPosTyp := TPositionTyp.CreateEx(Owner,DataModule,aConnection);
  //FImages := TImages.CreateEx(Self,DataModule,aConnection,DataSet);
  //FImages.ActualFilter := TBaseDBModule(DataModule).QuoteField('REF_ID')+'='+':'+TBaseDBModule(DataModule).QuoteField('IMAGEREF');
  //FPosCalc := TPositionCalc.CreateExIntegrity(Self,DataModule,False,aConnection,DataSet);
  //FVat := TVat.CreateEx(Self,DataModule,Connection);
  //PriceTypes := TPriceTypes.CreateEx(Self,DataModule,Connection);
end;
destructor TBaseDBPosition.Destroy;
begin
  //FPriceTypes.Destroy;
  //FPosCalc.Destroy;
  inherited Destroy;
end;
procedure TBaseDBPosition.FillDefaults;
begin
  DisableCalculation;
//      if FieldByName('POSTYP').Required then
  FieldByName('POSTYP').AsString   := PosTyp.FieldByName('NAME').AsString;
  //PosNo.AsInteger := (FOldPosNo+1);
  //FOldPosNo:=PosNo.AsInteger;
//      FieldByName('TPOSNO').AsString   := fOrders.TPosNumber;
  //FieldByName('TEXTTYPE').AsString := '0';
  //FieldByName('VAT').AsString      := '1';
  //FieldByName('QUANTITY').AsFloat  := 1;
  EnableCalculation;
end;

class function TBaseDBPosition.MapField(aField: string): string;
begin
  Result:=aField;
  if aField = 'ParentID' then
    Result := 'PARENT'
  else if aField = 'PosType' then
    Result := 'POSTYP'
  ;
end;

procedure TBaseDBPosition.Assign(Source: TPersistent);
{
var
  aMasterdata : TMasterdata;
  aQuantity: Double;
  bMasterdata: TMasterdata;
  tParent : Variant;

  procedure InsertData(Masterdata : TMasterdata;Quantity : float;aParent : Variant;Texttype : Integer = 0;Active : string = 'Y');
  var
    aVat: String;
  begin
    DisableCalculation;
    if (DataSet.State <> dsInsert) and (DataSet.State <> dsEdit) then
      DataSet.Edit;
    DataSet.FieldByName('IDENT').AsString:=Masterdata.Number.AsString;
    DataSet.FieldByName('SHORTTEXT').AsString:=Masterdata.Text.AsString;
    DataSet.FieldByName('VERSION').AsVariant := Masterdata.FieldByName('VERSION').AsVariant;
    DataSet.FieldByName('LANGUAGE').AsVariant := MasterData.FieldByName('LANGUAGE').AsVariant;
    DataSet.FieldByName('SHORTTEXT').AsString := MasterData.FieldByName('SHORTTEXT').AsString;
    DataSet.FieldByName('MANUFACNR').AsString := MasterData.FieldByName('MANUFACNR').AsString;
    DataSet.FieldByName('IMAGEREF').AsVariant := MasterData.FieldByName('IMAGEREF').AsVariant;
    if DataSet.FieldByName('IMAGEREF').IsNull then
      DataSet.FieldByName('IMAGEREF').AsVariant := MasterData.Id.AsVariant;
    DataSet.FieldByName('PARENT').AsVariant := aParent;
    DataSet.FieldByName('QUANTITY').AsFloat := Quantity;
    DataSet.FieldByName('WEIGHT').AsFloat := MasterData.FieldByName('WEIGHT').AsFloat;
    DataSet.FieldByName('QUANTITYU').AsString := MasterData.FieldByName('QUANTITYU').AsString;
    DataSet.FieldByName('ACTIVE').AsString := Active;
    aVat := MasterData.FieldByName('VAT').AsString;
    if pos(' ',aVat)>0 then aVat := copy(aVat,0,pos(' ',aVat)-1);
    DataSet.FieldByName('VAT').AsString := aVat;
    Masterdata.Texts.Open;
    if Masterdata.Texts.DataSet.Locate('TEXTTYPE',VarArrayOf([TextType]),[loCaseInsensitive]) then
      begin
        if CanHandleRTF then
          DataSet.FieldByName('TEXT').AsString := Masterdata.Texts.FieldByName('TEXT').AsString
        else
          DataSet.FieldByName('TEXT').AsString := RTF2Plain(Masterdata.Texts.FieldByName('TEXT').AsString);
      end;
    OldPosPrice := 0;
    OldGrossPrice := 0;
    PosCalc.Open;
    while PosCalc.Count > 0 do PosCalc.DataSet.Delete;
    if IsOrderToSupplier then //Supplierprices der Artikel verwenden statt der Verkaufspreise
      begin
        //TODO:implement me
      end
    else
      begin
        Masterdata.Prices.Open;
        while not Masterdata.Prices.DataSet.EOF do
          begin
            if  ((Masterdata.Prices.FieldByName('VALIDFROM').IsNull) or (Masterdata.Prices.FieldByName('VALIDFROM').AsDateTime > Now()))
            and ((Masterdata.Prices.FieldByName('VALIDTO').IsNull) or (Masterdata.Prices.FieldByName('VALIDTO').AsDateTime < Now()))
            and (   (Masterdata.Prices.GetPriceType = 4)
                 or (Masterdata.Prices.GetPriceType = 1)
                 or (Masterdata.Prices.GetPriceType = 5)
                 or (Masterdata.Prices.GetPriceType = 6))then
              begin
                PosCalc.DataSet.Append;
                PosCalc.FieldByName('TYPE').AsString := Masterdata.Prices.FieldByName('PTYPE').AsString;
                with BaseApplication as IBaseDbInterface do
                  PosCalc.FieldByName('PRICE').AsFloat := Data.Currency.Convert(Masterdata.Prices.FieldByName('PRICE').AsFloat,Masterdata.Prices.FieldByName('CURRENCY').AsString,GetCurrency);
                if not Masterdata.Prices.FieldByName('MINCOUNT').IsNull then
                  PosCalc.FieldByName('MINCOUNT').AsString := Masterdata.Prices.FieldByName('MINCOUNT').AsString;
                if not Masterdata.Prices.FieldByName('MAXCOUNT').IsNull then
                  PosCalc.FieldByName('MAXCOUNT').AsString := Masterdata.Prices.FieldByName('MAXCOUNT').AsString;
                PosCalc.FieldByName('CUSTOMER').AsString := Masterdata.Prices.FieldByName('CUSTOMER').AsString;
                PosCalc.DataSet.Post;
              end;
            Masterdata.Prices.DataSet.Next;
          end;
      end;
    OldPosWeight := 0;
    EnableCalculation;
    DoCalcPosPrice;
  end;
}
begin
{
  if Source is TMasterdata then
    begin
      aMasterdata := Source as TMasterdata;
      try
        aQuantity := DataSet.FieldByName('QUANTITY').AsFloat;
      except
        aQuantity := 1;
      end;
      InsertData(aMasterdata,aQuantity,Null);
      Post;
      tParent := FieldByName('SQL_ID').AsVariant;
      //TODO:tParent funktioniert nicht
      if (((aMasterdata.FieldByName('PTYPE').AsString = 'P') and (GetOrderTyp = 7))
      or  ((aMasterdata.FieldByName('PTYPE').AsString = 'O'))) then
        begin
          aMasterdata.Positions.Open;
          with aMasterdata.Positions.DataSet do
            begin
              Last;
              while not BOF do
                begin
                  with BaseApplication as IBaseDbInterface do
                    bMasterdata := TMasterdata.CreateEx(Self,Data);
                  bMasterdata.Select(FieldByName('IDENT').AsString,FieldByName('VERSION').AsVariant,FieldByName('LANGUAGE').AsVariant);
                  bMasterdata.Open;
                  if bMasterdata.Count = 0 then
                    begin
                      bMasterdata.Select(FieldByName('IDENT').AsString);
                      bMasterdata.Open;
                    end;
                  if bMasterdata.Count > 0 then
                    begin
                      DataSet.Append;
                      if Self.GetOrderTyp = 7 then
                        begin
                          //Bei produktionsauftrag Positionsnummer in Ausschreibungsposition kopieren
                          Edit;
                          FieldByName('TPOSNO').AsVariant:=aMasterdata.Positions.DataSet.FieldByName('POSNO').AsVariant;
                          if not Assigned(uBaseERPDBClasses.TextTyp) then
                            uBaseERPDBClasses.TextTyp := TTextTypes.Create(nil);
                          Texttyp.Open;
                          //Artikeldaten einfügen
                          if TextTyp.Locate('TYP','7',[]) then
                            InsertData(bMasterdata,(-FieldByName('QUANTITY').AsFloat)*aQuantity,tParent,TextTyp.DataSet.RecNo,FieldByName('ACTIVE').AsString)
                          else
                            InsertData(bMasterdata,(-FieldByName('QUANTITY').AsFloat)*aQuantity,tParent,0,FieldByName('ACTIVE').AsString);
                        end
                      else
                        InsertData(bMasterdata,  FieldByName('QUANTITY').AsFloat *aQuantity,tParent,0,FieldByName('ACTIVE').AsString);
                      DataSet.FieldByName('SHORTTEXT').AsString:=aMasterdata.Positions.FieldByName('SHORTTEXT').AsString; //use Shorttext from Piecelist
                      DataSet.FieldByName('POSTYP').AsString:=aMasterdata.Positions.FieldByName('POSTYP').AsString;
                      DataSet.FieldByName('ACTIVE').AsString:=aMasterdata.Positions.FieldByName('ACTIVE').AsString;
                      DataSet.FieldByName('PARENT').AsVariant := tParent;
                      DataSet.FieldByName('TPOSNO').AsVariant:=aMasterdata.Positions.DataSet.FieldByName('POSNO').AsVariant;
                    end
                  else //No Article
                    begin
                      DataSet.Append;
                      DisableCalculation;
                      DataSet.FieldByName('SHORTTEXT').AsString:=aMasterdata.Positions.FieldByName('SHORTTEXT').AsString;
                      if Self.GetOrderTyp <> 7 then //Dont copy Text in Production order so the production system is getting it live from Doku
                        DataSet.FieldByName('TEXT').AsString:=aMasterdata.Positions.FieldByName('TEXT').AsString;
                      DataSet.FieldByName('POSTYP').AsString:=aMasterdata.Positions.FieldByName('POSTYP').AsString;
                      DataSet.FieldByName('PARENT').AsVariant := tParent;
                      DataSet.FieldByName('TPOSNO').AsVariant:=aMasterdata.Positions.DataSet.FieldByName('POSNO').AsVariant;
                      DataSet.FieldByName('QUANTITY').AsString:=aMasterdata.Positions.FieldByName('QUANTITY').AsString;
                      DataSet.FieldByName('WEIGHT').AsString:=aMasterdata.Positions.FieldByName('WEIGHT').AsString;
                      DataSet.FieldByName('QUANTITYU').AsString:=aMasterdata.Positions.FieldByName('QUANTITYU').AsString;
                      DataSet.FieldByName('ACTIVE').AsString:=aMasterdata.Positions.FieldByName('ACTIVE').AsString;
                      if Self.GetOrderTyp = 7 then
                        DataSet.FieldByName('QUANTITY').AsFloat := (-FieldByName('QUANTITY').AsFloat)*aQuantity
                      else
                        DataSet.FieldByName('QUANTITY').AsFloat := FieldByName('QUANTITY').AsFloat*aQuantity;
                      DataSet.FieldByName('PARENT').AsVariant := tParent;
                      EnableCalculation;
                      DataSet.Post;
                    end;
                  bMasterdata.Destroy;
                  Prior;
                end;
            end;
          aQuantity:=-aQuantity;
        end;
    end
  else
    inherited Assign(Source);
}
end;

procedure TBaseDBPosition.DisableCalculation;
begin
  inc(FCalculationDisabled);
end;
procedure TBaseDBPosition.EnableCalculation;
begin
  if FCalculationDisabled > 0 then
    dec(FCalculationDisabled);
end;
function TBaseDBPosition.IsCalculationDisabled: Boolean;
begin
  Result := FCalculationDisabled > 0;
end;

procedure TBaseDBPosition.AppendSubTotal;
var
  aPos: TPositionTyp;
begin
  {
  aPos := TPositionTyp.CreateEx(nil,DataModule,Connection);
  aPos.Open;
  if aPos.DataSet.Locate('TYPE',4,[loCaseInsensitive]) then
    begin
      Dataset.Insert;
      DataSet.FieldByName('POSTYP').AsString:=aPos.FieldByName('NAME').AsString;
      DataSet.FieldByName('SHORTTEXT').AsString:=strSubTotal;
      Dataset.Post;
    end;
  aPos.Free;
  }
end;

function TCurrency.Convert(Price: real; SourceCurr, TargetCurr: string): real;
var
  tmp: real;
begin
  {
  Result := price;
  if SourceCurr = TargetCurr then
    exit;
  Open;
  if not DataSet.Locate('SYMBOL', SourceCurr, []) then
  begin
    Result := 0;
    exit;
  end;
  tmp := Price * DataSet.FieldByName('FACTOR').AsFloat;
  if not DataSet.Locate('SYMBOL', TargetCurr, []) then
  begin
    Result := 0;
    exit;
  end;
  if DataSet.FieldByName('FACTOR').AsFloat > 0 then
    Result := tmp / DataSet.FieldByName('FACTOR').AsFloat
  else
    Result := 0;
  }
end;
initialization
  TextTyp:=nil;
  Vat := nil;
  Units := nil;
  DispatchTypes := nil;
  PriceTypes := nil;
  RepairProblems := nil;
  RegisterdataSetClass('COUNTRIES',TCountries);
  RegisterdataSetClass('STORAGETYPE',TStorageTypes);
  RegisterdataSetClass('CURRENCY',TCurrency);
  RegisterdataSetClass('PAYMENTTARGETS',TPaymentTargets);
  RegisterdataSetClass('TEXTTYP',TTextTypes);
  RegisterdataSetClass('REPAIRPROBLEMS',TRepairProblems);
  RegisterdataSetClass('CATEGORY',TCategory);
end.

