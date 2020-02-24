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
unit uMasterdata;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, db, uBaseDbClasses, uBaseERPDBClasses, uIntfStrConsts,uBaseDatasetInterfaces2;
type
  TMasterdataList = class(TBaseERPList)
  private
    FType,FID,FVersion,FLanguage,FStatus,FBarcode,FMatchCode,FShorttext,FQuantityU,
      FPType,FWarrenty,FManuFacnr,FDispoType,FCostCentre,FAccount,FCategory,
      FCurrency,FScript,FScriptVer,FScriptFunc,FPrepText,FWorkText,FCreatedBy,FChangedBy : string;
    FTreeentry,FImageRef : Int64;
    FVat : ShortInt;
    FWeight,FSellprice,FPurchase : double;
    FValidFrom,FValidTo,FCRDate,FCHDate : TDateTime;
    FRepairtime,FPackageUnit,FValidToMe : Integer;
    FActive,FUseSerial,FOwnProd,FSaleItem,FUseBatch,FNoStorage,FIsTemplate : Boolean;
    FAccountinginfo : TBlobData;
  protected
    function GetMatchCodeFieldName: string;override;
    function GetTextFieldName: string;override;
    function GetNumberFieldName : string;override;
    function GetStatusFieldName : string;override;
  public
    function GetTyp: string; override;
    procedure OpenItem(AccHistory: Boolean=True); override;
    function SelectFromLink(aLink: string): Boolean; override;
    function SelectFromLinkwoVersion(aLink: string): Boolean;
    class function MapField(aField: string): string; override;
    property Accountinginfo : TBlobData read FAccountinginfo write FAccountinginfo; //Fibu Info
  published
    property Typ : string index 1 read FType write FType;
    property ID : string index 40 read FID write FID;
    property Version : string index 25 read FVersion write FVersion;
    property Language : string index 3 read FLanguage write FLanguage;
    property Active : Boolean read FActive write FActive;
    property Status : string index 4 read FStatus write FStatus;
    property Barcode : string index 20 read FBarcode write FBarcode;
    property MatchCode : string index 200 read FMatchCode write FMatchCode;
    property Shorttext : string index 240 read FShorttext write FShorttext;
    property Treeentry : Int64 read FTreeentry write FTreeentry;
    property QuantityU : string index 10 read FQuantityU write FQuantityU;//Mengeneinheit
    property Vat : ShortInt read FVat write FVat;        //Mehrwertsteuer
    property UseSerial : Boolean read FUseSerial write FUseSerial;
    property OwnProd : Boolean read FOwnProd write FOwnProd;
    property SaleItem : Boolean read FSaleItem write FSaleItem;
    property UseBatch : Boolean read FUseBatch write FUseBatch;
    property NoStorage : Boolean read FNoStorage write FNoStorage;
    property PType : string index 1 read FPType write FPType;
    property Weight : double read FWeight write FWeight;
    property Repairtime : Integer read FRepairtime write FRepairtime;     //max. Reparaturzeit
    property PackageUnit : Integer read FPackageUnit write FPackageUnit;     //Verpackungseinheit
    property Warrenty : string index 10 read FWarrenty write FWarrenty;
    property ManuFacnr : string index 40 read FManuFacnr write FManuFacnr;
    property DispoType : string index 1 read FDispoType write FDispoType;   //Planart 0=nicht Disponieren 1=Volldispo 2=disponieren 3=Mindestbestand
    property ValidFrom : TDateTime read FValidFrom write FValidFrom;   //Ein/Auslaufsteuerung
    property ValidTo : TDateTime read FValidTo write FValidTo;     //gültig bis Datum
    property ValidToMe : Integer read FValidToMe write FValidToMe;//gültig bis Menge
    property CostCentre : string index 10 read FCostCentre write FCostCentre;//Kostenstelle
    property Account : string index 10 read FAccount write FAccount; //Fibu Konto
    property Category : string index 60 read FCategory write FCategory;
    property Sellprice : double read FSellprice write FSellprice;
    property Purchase : double read FPurchase write FPurchase;
    property IsTemplate : Boolean read FIsTemplate write FIsTemplate;
    property Currency : string index 5 read FCurrency write FCurrency;
    property ImageRef : Int64 read FImageRef write FImageRef;
    property Script : string index 60 read FScript write FScript;
    property ScriptVer : string index 8 read FScriptVer write FScriptVer;
    property ScriptFunc : string index 60 read FScriptFunc write FScriptFunc;
    property PrepText : string index 100 read FPrepText write FPrepText;
    property WorkText : string index 100 read FWorkText write FWorkText;
    property CRDate : TDateTime read FCRDate write FCRDate;
    property CHDate : TDateTime read FCHDate write FCHDate;
    property ChangedBy : string index 4 read FChangedBy write FChangedBy;
    property CreatedBy : string index 4 read FCreatedBy write FCreatedBy;
  end;
  TMasterdata = class;
  TMDPos = class(TBaseDBPosition)
  private
  protected
    function GetCurrency : string;override;
    procedure PosPriceChanged(aPosDiff,aGrossDiff :Extended);override;
    procedure PosWeightChanged(aPosDiff :Extended);override;
  public
    class function GetRealTableName: string; override;
  end;

  { TMDPositions }

  TMDPositions = class(TAbstractMasterDetail)
  public
    class function GetObjectTyp: TClass; override;
  end;
  TSerial = class(TBaseDbDataSet)
  private
    FSerial,FNote : string;
  public
    class function GetRealTableName: string; override;
  published
    property Serial : string index 30 read FSerial write FSerial;
    property Note : string index 500 read FNote write FNote;
  end;
  TSerials = class(TAbstractMasterDetail)
  public
    class function GetObjectTyp: TClass; override;
  end;
  TStorage = class(TBaseDBDataSet)
  private
    FJournal: TStorageJournal;
    FStorageID,FStorName,FPlace,FQuantityU : string;
    FQuantity,FReserved : double;
    FCharge : Integer;
    function GetJournal: TStorageJournal;
  public
    property Journal : TStorageJournal read GetJournal;
    function DoPost(OrderType: TBaseDBDataset; Order: TBaseDBDataset;
      aStorage: string; aQuantity, aReserve: real; QuantityUnit, PosNo: string
  ): real;
  published
    property StorageID : string index 3 read FStorageID write FStorageID;
    property StorName : string index 30 read FStorName write FStorName;
    property Place : string index 20 read FPlace write FPlace;
    property Quantity : double read FQuantity write FQuantity;
    property Reserved : double read FReserved write FReserved;
    property QuantityU : string index 10 read FQuantityU write FQuantityU;
    property Charge : Integer read FCharge write FCharge;
  end;
  TStorageList = class(TAbstractMasterDetail)
  public
    class function GetObjectTyp: TClass; override;
  end;
  TSupplierPrices = class(TBaseDBDataSet)
  private
    FFromUnit,FDiscount,FPrice : double;
    FQuantityU,FCurrency : string;
  published
    property FromUnit : double read FFromUnit write FFromUnit;
    property QuantityU : string index 10 read FQuantityU write FQuantityU;
    property Discount : double read FDiscount write FDiscount;
    property Price : double read FPrice write FPrice;
    property Currency : string index 3 read FCurrency write FCurrency;
  end;
  TSupplier = class(TBaseDBDataSet)
  private
    FPrices: TSupplierPrices;
    FAccountNo,FName,FEID,FTransCUR : string;
    FDeliverTm : Integer;
    FTransport : double;
  public
    property Prices : TSupplierPrices read FPrices;
  published
    property AccountNo : string index 60 read FAccountNo write FAccountNo;
    property Name : string index 260 read FName write FName;
    property DeliverTm : Integer read FDeliverTm write FDeliverTm;
    property EID : string index 30 read FEID write FEID;
    property Transport : double read FTransport write FTransport;
    property TransCUR : string index 3 read FTransCUR write FTransCUR;
  end;
  TSuppliers = class(TAbstractMasterDetail)
  public
    class function GetObjectTyp: TClass; override;
  end;
  TMasterdataLinks = class(TAbstractMasterDetail)
  public
    class function GetObjectTyp: TClass; override;
    function Add(AObject: TObject): Integer;
  end;
  TMasterdataPrice = class(TBaseDbDataSet)
  private
    FDS: TDataSource;
    FMasterdata: TMasterdataList;
    FPType,FNote,FCurrency,FCustomer : string;
    FMinCount,FMaxCount,FPrice : double;
    FValidFrom,FValidTo : TDateTime;
    procedure SetPrice(AValue: double);
  public
    class function GetRealTableName: string; override;
    constructor CreateEx(Owner: TPersistent;Module: TComponent); override;
    procedure FillDefaults; override;
    function GetPriceType : Integer;
    function FormatCurrency(Value : real) : string;
    property Masterdata : TMasterdataList read FMasterdata write FMasterdata;
  published
    constructor Create;
    property PType : string index 4 read FPType write FPType;
    property Price : double read FPrice write SetPrice;
    property Note : string index 500 read FNote write FNote;
    property Currency : string index 3 read FCurrency write FCurrency;
    property MinCount : double read FMinCount write FMinCount;
    property MaxCount : double read FMaxCount write FMaxCount;
    property ValidFrom : TDateTime read FValidFrom write FValidFrom;
    property ValidTo : TDateTime read FValidTo write FValidTo;
    property Customer : string index 20 read FCustomer write FCustomer;
  end;
  TMasterdataPrices = class(TAbstractMasterDetail)
  public
    class function GetObjectTyp: TClass; override;
  end;
  TMdProperty = class(TBaseDbDataSet)
  private
    FProperty,FValue,FQuantityU : string;
  public
    property Prop : string index 50 read FProperty write FProperty;
    property Value : string index 50 read FValue write FValue;
    property QuantityU : string index 10 read FQuantityU write FQuantityU;
  end;
  TMdProperties = class(TAbstractMasterDetail)
  public
    class function GetObjectTyp: TClass; override;
  end;
  TMasterdataText = class(TBaseDbDataSet)
  private
    FTextType : Integer;
    FText : TBlobData;
  public
    class function GetRealTableName: string; override;
  published
    property TextType : Integer read FTextType write FTextType;
    property Text : TBlobData read FText write FText;
  end;
  TMasterdataTexts = class(TAbstractMasterDetail)
  public
    class function GetObjectTyp: TClass; override;
  end;
  TRepairParts = class(TBaseDbDataSet)
  private
    FPart : string;
  public
    property Part : string index 60 read FPart write FPart;
  end;
  TRepairAssembly = class(TBaseDbDataSet)
  private
    FParts: TRepairParts;
    FAssembly : string;
  public
    property Parts : TRepairParts read FParts;
  published
    property Assembly : string index 60 read FAssembly write FAssembly;
  end;
  TRepairAssembies = class(TAbstractMasterDetail)
  public
    class function GetObjectTyp: TClass; override;
  end;
  TMasterdata = class(TMasterdataList,IBaseHistory)
  private
    FAssembly: TRepairAssembies;
    FHistory: TBaseHistory;
    FImages: TImages;
    FLinks: TMasterdataLinks;
    FMeasurement: TMeasurements;
    FPosition: TMDPositions;
    FPrices: TMasterdataPrices;
    FProperties: TMdProperties;
    FSerials: TSerials;
    FStateChange: TNotifyEvent;
    FStorage: TStorageList;
    FSupplier: TSuppliers;
    FTexts: TMasterdataTexts;
    function GetHistory : TBaseHistory;
    function QueryInterface(constref iid: tguid; out obj): longint; stdcall;
    procedure SetId(AValue: string);
    procedure SetStatus(AValue: string);
    function _AddRef: longint; stdcall;
    function _Release: longint; stdcall;
  public
    constructor CreateEx(Owner: TPersistent;Module: TComponent); override;
    destructor Destroy;override;
    procedure FillDefaults;override;
    function Copy(aNewVersion : Variant;aNewLanguage : Variant;cPrices : Boolean = True;
                                                               cProperties : Boolean = True;
                                                               cTexts : Boolean = True;
                                                               cSupplier : Boolean = True;
                                                               cPiecelists : Boolean = True) : Boolean;
    function Versionate(aNewversion : Variant;aMakeActive : Boolean = True;cPrices : Boolean = True;
                                                               cProperties : Boolean = True;
                                                               cTexts : Boolean = True;
                                                               cSupplier : Boolean = True;
                                                               cPiecelists : Boolean = True) : Boolean;
    property OnStateChange : TNotifyEvent read FStateChange write FStateChange;
  published
    property Status : string read FStatus write SetStatus;
    property ID : string read FID write SetId;
    //property Storage : TStorageList read FStorage;
    property Positions : TMDPositions read FPosition;
    //property History : TBaseHistory read FHistory;
    //property Images : TImages read FImages;
    //property Links : TMasterdataLinks read FLinks;
    //property Texts : TMasterdataTexts read FTexts;
    //property Supplier : TSuppliers read FSupplier;
    //property Prices : TMasterdataPrices read FPrices;
    //property Properties : TMdProperties read FProperties;
    //property Assembly : TRepairAssembies read FAssembly;
    //property Serials : TSerials read FSerials;
    //property Measurements : TMeasurements read FMeasurement;
  end;
implementation
uses uData, Utils;

{ TMDPositions }

class function TMDPositions.GetObjectTyp: TClass;
begin
  Result := TMDPos;
end;

{ TSerials }

class function TSerials.GetObjectTyp: TClass;
begin
  Result := TSerial;
end;

{ TSerial }

class function TSerial.GetRealTableName: string;
begin
  Result:='SERIALS';
end;

{ TSuppliers }

class function TSuppliers.GetObjectTyp: TClass;
begin
  Result := TSupplier;
end;

{ TRepairAssembies }

class function TRepairAssembies.GetObjectTyp: TClass;
begin
  Result := TRepairAssembly;
end;

{ TMdProperties }

class function TMdProperties.GetObjectTyp: TClass;
begin
  Result := TMdProperty;
end;

constructor TMasterdataPrice.Create;
begin
  PType:='SAP';
  {TODO
  if Data.Currency.DataSet.Active and Data.Currency.DataSet.Locate('DEFAULTCUR', 'Y', []) then
    FieldByName('CURRENCY').AsString := Data.Currency.FieldByName('SYMBOL').AsString;
  }
end;

{ TMasterdataText }

class function TMasterdataText.GetRealTableName: string;
begin
  Result:='TEXTS';
end;

{ TMasterdataTexts }

class function TMasterdataTexts.GetObjectTyp: TClass;
begin
  Result:=TMasterdataText;
end;

{ TMasterdataLinks }

class function TMasterdataLinks.GetObjectTyp: TClass;
begin
  Result := TLinks;
end;

function TMasterdataLinks.Add(AObject: TObject): Integer;
begin
  Result := inherited Add(AObject);
  TLinks(Result).RRef_ID:=(Parent as TMasterdata).SQL_ID;
end;

{ TStorageList }

class function TStorageList.GetObjectTyp: TClass;
begin
  Result := TStorage;
end;

procedure TMasterdataPrice.SetPrice(AValue: double);
begin
  if FPrice=AValue then Exit;
  FPrice:=AValue;
  {TODO
  if Masterdata.FieldByName('CURRENCY').AsString='' then
    begin
      Masterdata.Edit;
      if Data.Currency.DataSet.Active and Data.Currency.DataSet.Locate('DEFAULTCUR', 'Y', []) then
        Masterdata.FieldByName('CURRENCY').AsString := Data.Currency.FieldByName('SYMBOL').AsString;
      Masterdata.Post;
    end;
  case GetPriceType of
  1:if Assigned(Masterdata.FieldByName('PURCHASE')) then
    begin
      Masterdata.Edit;
      Masterdata.FieldByName('PURCHASE').AsFloat:=Data.Currency.Convert(DataSet.FieldByName('PRICE').AsFloat,DataSet.FieldByName('CURRENCY').AsString,Masterdata.DataSet.FieldByName('CURRENCY').AsString);
      Masterdata.Post;
    end;
  4:if Assigned(Masterdata.FieldByName('SELLPRICE')) then
      begin
        Masterdata.Edit;
        Masterdata.FieldByName('SELLPRICE').AsFloat:=Data.Currency.Convert(DataSet.FieldByName('PRICE').AsFloat,DataSet.FieldByName('CURRENCY').AsString,Masterdata.DataSet.FieldByName('CURRENCY').AsString);
        Masterdata.Post;
      end;
  end;
  }
end;

class function TMasterdataPrice.GetRealTableName: string;
begin
  Result:='MDPRICES';
end;

constructor TMasterdataPrice.CreateEx(Owner: TPersistent; Module: TComponent);
begin
  inherited CreateEx(Owner,Module);
end;

procedure TMasterdataPrice.FillDefaults;
begin
  inherited FillDefaults;
end;

function TMasterdataPrice.GetPriceType: Integer;
var
  PriceType: TPriceTypes;
begin
  Result := 0;
  {TODO
  try
    PriceType := TPriceTypes.CreateEx(Self,DataModule,Connection);
    PriceType.Open;
    if PriceType.DataSet.Locate('SYMBOL', trim(DataSet.FieldByName('PTYPE').AsString), []) then
      Result := StrToIntDef(copy(Pricetype.FieldByName('TYPE').AsString, 0, 2), 0);
  finally
    PriceType.Free;
  end;
  }
end;
function TMasterdataPrice.FormatCurrency(Value: real): string;
begin
  Result := FormatFloat('0.00',Value)+' '+Currency;
end;

class function TMasterdataPrices.GetObjectTyp: TClass;
begin
  Result := TMasterdataPrice;
end;

function TStorage.GetJournal: TStorageJournal;
begin
  if not Assigned(FJournal) then
    FJournal := TStorageJournal.CreateEx(Self,DataModule);
  Result := FJournal;
end;
function TStorage.DoPost(OrderType: TBaseDBDataset; Order: TBaseDBDataset;
  aStorage: string; aQuantity, aReserve: real; QuantityUnit,PosNo: string): real;
var
  JournalCreated: Boolean;
  r: Real;
  IntSerial: Integer;
begin
  IntSerial := 1;
  Result := 0;
  try
    if not Active then Open;
    //Lager selektieren oder anlegen
    {
    if not Data.StorageType.Active then
      Data.StorageType.Open;
    if ((FieldByName('STORAGEID').AsString<>trim(copy(aStorage, 0, 3)))
    and (not Locate('STORAGEID', trim(copy(aStorage, 0, 3)), [loCaseInsensitive])))
    or ((Parent.FieldByName('USEBATCH').AsString='Y') and (aQuantity>0)) then
      begin
        //Kein Lager vorhanden ? dann Tragen wir das Hauptlager ein (sollte ja nicht zuoft vorkommen)
        Data.StorageType.DataSet.Locate('DEFAULTST', 'Y', [loCaseInsensitive]);
        Data.StorageType.DataSet.Locate('ID',trim(copy(aStorage, 0, 3)),[loCaseInsensitive]);
        with DataSet do
          begin
            Append;
            if DataSet.FieldDefs.IndexOf('TYPE') > -1 then
              begin
                FieldByName('TYPE').AsString := Parent.FieldByName('TYPE').AsString;
                FieldByName('ID').AsString := Parent.FieldByName('ID').AsString;
                FieldByName('VERSION').AsVariant := Parent.FieldByName('VERSION').AsString;
                FieldByName('LANGUAGE').AsVariant := Parent.FieldByName('LANGUAGE').AsString;
              end;
            FieldByName('STORAGEID').AsString := Data.StorageType.FieldByName('ID').AsString;
            FieldByName('STORNAME').AsString := Data.StorageType.FieldByName('NAME').AsString;
            FieldByName('QUANTITY').AsFloat := 0;
            FieldByName('QUANTITYU').AsString := QuantityUnit;
            if ((Parent.FieldByName('USEBATCH').AsString='Y') and (aQuantity>0)) then
              begin //new Batch
                FieldByName('CHARGE').AsString:=Order.FieldByName('ORDERNO').AsString;
              end;
            aStorage := FieldByName('STORAGEID').AsString;
            Post;
          end;
      end;
    //Buchen
    Edit;
    if (FieldByName('QUANTITY').AsFloat>0) and (FieldByName('QUANTITY').AsFloat - FieldByName('RESERVED').AsFloat + aQuantity < 0) then
      begin
        Result := aQuantity-(FieldByName('QUANTITY').AsFloat - FieldByName('RESERVED').AsFloat + aQuantity);
        aQuantity:=aQuantity-Result;
      end
    else Result := aQuantity;
    FieldByName('QUANTITY').AsFloat := FieldByName('QUANTITY').AsFloat + aQuantity;
    FieldByName('RESERVED').AsFloat := FieldByName('RESERVED').AsFloat + aReserve;
    DataSet.Post;
    JournalCreated := False;
    //Serienummern buchen
    if (OrderType.FieldByName('B_STORAGE').AsString <> '0') and ((TMasterdata(Parent).FieldByName('USESERIAL').AsString = 'Y') or (TOrder(Order).Positions.FieldByName('SERIAL').AsString<>'') or (OrderType.FieldByName('B_SERIALS').AsString = 'P')) then
      begin
        if (OrderType.FieldByName('B_SERIALS').AsString = '+')
        or (OrderType.FieldByName('B_SERIALS').AsString = 'P')
        then
          begin
            r := aQuantity;
            while r >= 1 do
              begin
                JournalCreated := False;
                if (TOrder(Order).Positions.FieldByName('SERIAL').AsString='') or (IntSerial =0) then
                  begin
                    if Assigned(TOrder(Order).OnGetSerial) then
                      if TOrder(Order).OnGetSerial(TOrder(Order),TMasterdata(Parent),1) then
                        begin
                          Journal.Insert;
                          Journal.FieldByName('STORAGEID').AsString := FieldByName('STORAGEID').AsString;
                          Journal.FieldByName('ORDERNO').AsString := Order.FieldByName('ORDERNO').AsString;
                          Journal.FieldByName('OSTATUS').AsString := Order.FieldByName('STATUS').AsString;
                          Journal.FieldByName('POSNO').AsString   := PosNo;
                          Journal.FieldByName('TYPE').AsString    := Parent.FieldByName('TYPE').AsString;
                          Journal.FieldByName('ID').AsString      := Parent.FieldByName('ID').AsString;
                          Journal.FieldByName('VERSION').AsString := Parent.FieldByName('VERSION').AsString;
                          Journal.FieldByName('LANGUAGE').AsString := Parent.FieldByName('LANGUAGE').AsString;
                          Journal.FieldByName('SERIAL').AsString  := TMasterdata(Parent).Serials.FieldByName('SERIAL').AsString;
                          if Assigned(Journal.FieldByName('NOTE')) and (not Journal.FieldByName('NOTE').ReadOnly) then
                            Journal.FieldByName('NOTE').AsString  := TMasterdata(Parent).Serials.FieldByName('NOTE').AsString;
                          Journal.FieldByName('QUANTITY').AsFloat := 1;
                          Journal.FieldByName('QUANTITYU').AsString := QuantityUnit;
                          Journal.Post;
                        end;
                        r := r - 1;
                        JournalCreated := True;
                    if not JournalCreated then
                      begin
                        Result := 0;
                        exit;
                      end;
                  end
                else
                  begin
                    Journal.Insert;
                    Journal.FieldByName('STORAGEID').AsString := FieldByName('STORAGEID').AsString;
                    Journal.FieldByName('ORDERNO').AsString := Order.FieldByName('ORDERNO').AsString;
                    Journal.FieldByName('OSTATUS').AsString := Order.FieldByName('STATUS').AsString;
                    Journal.FieldByName('POSNO').AsString   := PosNo;
                    Journal.FieldByName('TYPE').AsString    := Parent.FieldByName('TYPE').AsString;
                    Journal.FieldByName('ID').AsString      := Parent.FieldByName('ID').AsString;
                    Journal.FieldByName('VERSION').AsString := Parent.FieldByName('VERSION').AsString;
                    Journal.FieldByName('LANGUAGE').AsString := Parent.FieldByName('LANGUAGE').AsString;
                    Journal.FieldByName('SERIAL').AsString  := TOrder(Order).Positions.FieldByName('SERIAL').AsString;
                    Journal.FieldByName('QUANTITY').AsFloat := 1;
                    Journal.FieldByName('QUANTITYU').AsString := QuantityUnit;
                    Journal.Post;
                    dec(IntSerial);
                    with TMasterdata(Journal.Parent).Serials do
                      begin
                        Insert;
                        FieldByName('SERIAL').AsString:=TOrder(Order).Positions.FieldByName('SERIAL').AsString;
                        Post;
                      end;
                    r := r - 1;
                    JournalCreated := True;
                  end;
              end;
          end
        else if OrderType.FieldByName('B_SERIALS').AsString = '-' then
          begin
            r := aQuantity;
            while r >= 1 do
              begin
                JournalCreated := False;
                if (TOrder(Order).Positions.FieldByName('SERIAL').AsString='') or (IntSerial =0) then
                  begin
                    if Assigned(TOrder(Order).OnGetSerial) then
                      if TOrder(Order).OnGetSerial(TOrder(Order),TMasterdata(Parent),-1) then
                        begin
                          Journal.Insert;
                          Journal.FieldByName('STORAGEID').AsString := FieldByName('STORAGEID').AsString;
                          Journal.FieldByName('ORDERNO').AsString := Order.FieldByName('ORDERNO').AsString;
                          Journal.FieldByName('OSTATUS').AsString := Order.FieldByName('STATUS').AsString;
                          Journal.FieldByName('POSNO').AsString   := PosNo;
                          Journal.FieldByName('TYPE').AsString    := Parent.FieldByName('TYPE').AsString;
                          Journal.FieldByName('ID').AsString      := Parent.FieldByName('ID').AsString;
                          Journal.FieldByName('VERSION').AsString := Parent.FieldByName('VERSION').AsString;
                          Journal.FieldByName('LANGUAGE').AsString := Parent.FieldByName('LANGUAGE').AsString;
                          Journal.FieldByName('SERIAL').AsString  := TMasterdata(Parent).Serials.FieldByName('SERIAL').AsString;
                          if Assigned(Journal.FieldByName('NOTE')) then
                            Journal.FieldByName('NOTE').AsString  := TMasterdata(Parent).Serials.FieldByName('NOTE').AsString;
                          Journal.FieldByName('QUANTITY').AsFloat := -1;
                          Journal.FieldByName('QUANTITYU').AsString := QuantityUnit;
                          Journal.Post;
                          if TMasterdata(Parent).Serials.Locate('SERIAL',TMasterdata(Parent).Serials.FieldByName('SERIAL').AsString,[]) then
                            TMasterdata(Parent).Serials.Delete;
                          r := r - 1;
                          JournalCreated := True;
                        end;
                    if not JournalCreated then
                      begin
                        Result := 0;
                        exit;
                      end;
                  end
                else
                  begin
                    Journal.Insert;
                    Journal.FieldByName('STORAGEID').AsString := FieldByName('STORAGEID').AsString;
                    Journal.FieldByName('ORDERNO').AsString := Order.FieldByName('ORDERNO').AsString;
                    Journal.FieldByName('OSTATUS').AsString := Order.FieldByName('STATUS').AsString;
                    Journal.FieldByName('POSNO').AsString   := PosNo;
                    Journal.FieldByName('TYPE').AsString    := Parent.FieldByName('TYPE').AsString;
                    Journal.FieldByName('ID').AsString      := Parent.FieldByName('ID').AsString;
                    Journal.FieldByName('VERSION').AsString := Parent.FieldByName('VERSION').AsString;
                    Journal.FieldByName('LANGUAGE').AsString := Parent.FieldByName('LANGUAGE').AsString;
                    Journal.FieldByName('SERIAL').AsString  := TOrder(Order).Positions.FieldByName('SERIAL').AsString;
                    Journal.FieldByName('QUANTITY').AsFloat := -1;
                    Journal.FieldByName('QUANTITYU').AsString := QuantityUnit;
                    Journal.Post;
                    if TMasterdata(Parent).Serials.Locate('SERIAL',TOrder(Order).Positions.FieldByName('SERIAL').AsString,[]) then
                      TMasterdata(Parent).Serials.Delete;
                    r := r - 1;
                    dec(IntSerial);
                    JournalCreated := True;
                  end;
              end;
          end;
      end;
    //Journal erstellen falls nicht schon von den Serienummern gebucht wurde
    if (OrderType.FieldByName('B_STORAGE').AsString <> '0') then
      if not JournalCreated and (OrderType.FieldByName('B_STORAGE').AsString <> '0') then
        begin
          Journal.Insert;
          Journal.FieldByName('STORAGEID').AsString := FieldByName('STORAGEID').AsString;
          Journal.FieldByName('ORDERNO').AsString := Order.FieldByName('ORDERNO').AsString;
          Journal.FieldByName('OSTATUS').AsString := Order.FieldByName('STATUS').AsString;
          Journal.FieldByName('POSNO').AsString := PosNo;
          Journal.FieldByName('TYPE').AsString    := Parent.FieldByName('TYPE').AsString;
          Journal.FieldByName('ID').AsString      := Parent.FieldByName('ID').AsString;
          Journal.FieldByName('VERSION').AsString := Parent.FieldByName('VERSION').AsString;
          Journal.FieldByName('LANGUAGE').AsString := Parent.FieldByName('LANGUAGE').AsString;
          Journal.FieldByName('QUANTITY').AsFloat := aQuantity;
          Journal.FieldByName('QUANTITYU').AsString := QuantityUnit;
          Journal.Post;
        end;
    }
  except
    result := 0;
  end;
end;

function TMasterdata.QueryInterface(constref iid: tguid; out obj): longint;
  stdcall;
begin
end;

procedure TMasterdata.SetId(AValue: string);
begin
  if FID=AValue then Exit;
  FID:=AValue;
  FHistory.AddItem(Self,Format(strNumberChanged,[AValue]),'','',Self,ACICON_EDITED);
end;

procedure TMasterdata.SetStatus(AValue: string);
begin
  if FStatus=AValue then Exit;
  FHistory.AddItem(Self,Format(strStatusChanged,[FStatus,AValue]),'','',nil,ACICON_STATUSCH);
  FStatus:=AValue;
  if Assigned(FStateChange) then
    FStateChange(Self);
  OpenItem(False);
end;

function TMasterdata._AddRef: longint; stdcall;
begin
end;

function TMasterdata._Release: longint; stdcall;
begin
end;

function TMasterdataList.GetTyp: string;
begin
  Result := 'M';
end;

function TMasterdata.GetHistory: TBaseHistory;
begin
  Result := FHistory;
end;
constructor TMasterdata.CreateEx(Owner: TPersistent; Module: TComponent);
begin
  inherited CreateEx(Owner, Module);
  FPosition := TMDPositions.Create(Self);
  FStorage := TStorageList.Create(Self);
  FHistory := TBaseHistory.Create(Self);
  FImages := TImages.Create(Self);
  FLinks := TMasterdataLinks.Create(Self);
  FTexts := TMasterdataTexts.Create(Self);
  FPrices := TMasterdataPrices.Create(Self);
  FProperties := TMdProperties.Create(Self);
  FAssembly := TRepairAssembies.Create(Self);
  FSupplier := TSuppliers.Create(Self);
  FSerials := TSerials.Create(Self);
  FMeasurement := TMeasurements.Create(Self);
end;
destructor TMasterdata.Destroy;
begin
  FMeasurement.Free;
  FSerials.Free;
  FPosition.Free;
  FStorage.Free;
  FHistory.Free;
  FImages.Free;
  FTexts.Free;
  FLinks.Free;
  FPrices.Free;
  FProperties.Free;
  FAssembly.Free;
  FSupplier.Free;
  inherited Destroy;
end;

procedure TMasterdata.FillDefaults;
begin
  {
  Vat := TVat.CreateEx(Self,DataModule,Connection);
  Vat.Open;
  with aDataSet,BaseApplication as IBaseDBInterface do
    begin
      aDataSet.DisableControls;
      if FieldByName('ID').IsNull then
        FieldByName('ID').AsString      := Data.Numbers.GetNewNumber('ARTICLES');
      FieldByName('TYPE').AsString    := 'A';
      FieldByName('TREEENTRY').AsVariant := TREE_ID_MASTERDATA_UNSORTED;
      FieldByName('USESERIAL').AsString := 'N';
      FieldByName('OWNPROD').AsString := 'N';
      FieldByName('UNIT').AsInteger   := 1;
      TBaseDBModule(DataModule).Languages.Open;
      if TBaseDBModule(DataModule).Languages.Locate('DEFAULTLNG','Y',[loCaseInsensitive]) then
        FieldByName('LANGUAGE').AsString := TBaseDBModule(DataModule).Languages.FieldByName('ISO6391').AsString
      else
        FieldByName('LANGUAGE').AsString := 'DE';
      FieldByName('CRDATE').AsDateTime := Date;
      FieldByName('ACTIVE').AsString  := 'Y';
      FieldByName('PTYPE').AsString  := 'P';
      FieldByName('VAT').AsString     := Vat.FieldByName('ID').AsString;
      FieldByName('CREATEDBY').AsString := Data.Users.IDCode.AsString;
      FieldByName('CHANGEDBY').AsString := Data.Users.IDCode.AsString;
      aDataSet.EnableControls;
    end;
  Vat.Free;
  }
end;
function TMasterdata.Copy(aNewVersion: Variant; aNewLanguage: Variant;
  cPrices: Boolean; cProperties: Boolean; cTexts: Boolean; cSupplier: Boolean;
  cPiecelists: Boolean): Boolean;
var
  bMasterdata: TMasterdata;
begin
  {
  Result := True;
  bMasterdata := TMasterdata.CreateEx(Self,DataModule,Self.Connection);
  try
    try
      bMasterdata.Select(Id.AsVariant);
      bMasterdata.Append;
      FDS.DataSet:=Nil;
      bMasterdata.FDS.DataSet:=Nil;
      bMasterdata.DirectAssign(Self);
      if aNewVersion <> bMasterdata.Version.AsVariant then
        bMasterdata.Version.AsVariant:=aNewVersion;
      if aNewLanguage <> bMasterdata.Language.AsVariant then
        bMasterdata.Language.AsVariant:=aNewLanguage;
      bMasterdata.CascadicPost;
      FDS.DataSet:=DataSet;
      bMasterdata.FDS.DataSet:=DataSet;
      if cPrices then
        begin
          Prices.Open;
          while not Prices.EOF do
            begin
              bMasterdata.Prices.Append;
              bMasterdata.Prices.DirectAssign(Prices);
              Prices.Next;
            end;
        end;
      if cProperties then
        begin
          Properties.Open;
          while not Properties.EOF do
            begin
              bMasterdata.Properties.Append;
              bMasterdata.Properties.DirectAssign(Properties);
              Properties.Next;
            end;
        end;
      if cTexts then
        begin
          Texts.Open;
          while not Texts.EOF do
            begin
              bMasterdata.Texts.Append;
              bMasterdata.Texts.DirectAssign(Texts);
              Texts.Next;
            end;
        end;
      if cSupplier then
        begin
          Supplier.Open;
          while not Supplier.EOF do
            begin
              bMasterdata.Supplier.Append;
              bMasterdata.Supplier.DirectAssign(Supplier);
              Supplier.Next;
            end;
        end;
      if cPiecelists then
        begin
          Positions.Open;
          while not Positions.EOF do
            begin
              bMasterdata.Positions.Append;
              bMasterdata.Positions.DirectAssign(Positions);
              bMasterdata.Positions.FieldByName('REF_ID').AsVariant:=bMasterdata.Id.AsVariant;
              Positions.Next;
            end;
        end;

      Self.Select(bMasterdata.Id.AsVariant);
      Self.Open;
    except
      Result := False;
    end;
  finally
    bMasterdata.Free;
  end;
  DataSet.Edit;
  Change;
  }
end;

function TMasterdata.Versionate(aNewversion: Variant; aMakeActive: Boolean;
  cPrices: Boolean; cProperties: Boolean; cTexts: Boolean; cSupplier: Boolean;
  cPiecelists: Boolean): Boolean;
var
  bMasterdata: TMasterdata;
begin
  {
  Result := Copy(aNewversion,FieldByName('LANGUAGE').AsVariant,cPrices,cProperties,cTexts,cSupplier,cPiecelists);
  if aMakeActive then
    begin
      bMasterdata := TMasterdata.CreateEx(Self,DataModule,Self.Connection);
      try
        try
          bMasterdata.Select(Number.AsString);
          bMasterdata.Open;
          while not bMasterdata.EOF do
            begin
              bMasterdata.Edit;
              if bMasterdata.Id.AsVariant<>Self.Id.AsVariant then
                bMasterdata.FieldByName('ACTIVE').AsString:='N'
              else
                bMasterdata.FieldByName('ACTIVE').AsString:='Y';
              bMasterdata.FieldByName('IMAGEREF').AsVariant:=Id.AsVariant;//Versionated Version uses Images from us...
              bMasterdata.Post;
              bMasterdata.Next;
            end;
        except
          Result := False;
        end;
      finally
        bMasterdata.Free;
      end;
    end
  else
    begin
      Edit;
      FieldByName('ACTIVE').AsString:='N';
      Post;
    end;
  }
end;

function TMDPos.GetCurrency: string;
begin
  Result:=(Parent as TMasterdata).Currency;
end;

procedure TMDPos.PosPriceChanged(aPosDiff, aGrossDiff: Extended);
begin
  //TODO: Implement me (Einkaufspreis anpassen)
end;
procedure TMDPos.PosWeightChanged(aPosDiff: Extended);
begin
  Weight := Weight+aPosDiff;
end;

class function TMDPos.GetRealTableName: string;
begin
  Result:='MDPOSITIONS';
end;
function TMasterdataList.GetMatchCodeFieldName: string;
begin
  Result:='MATCHCODE';
end;
function TMasterdataList.GetTextFieldName: string;
begin
  Result:='SHORTTEXT';
end;
function TMasterdataList.GetNumberFieldName: string;
begin
  Result:='ID';
end;
function TMasterdataList.GetStatusFieldName: string;
begin
  Result:='STATUS';
end;
procedure TMasterdataList.OpenItem(AccHistory: Boolean);
{var
  aObj: TObjects = nil;
  aID: String;
  aFilter: String;
  }
begin
  {
  if Self.Count=0 then exit;
  try
    try
      aObj := TObjects.Create(nil);
      if (DataSet.State<>dsInsert) and (Self.FieldByName('ACTIVE').AsString='Y') then
        begin
          if not Data.TableExists(aObj.TableName) then
            begin
              aObj.CreateTable;
              aObj.Free;
              aObj := TObjects.CreateEx(nil,Data,nil,DataSet);
            end;
          with aObj.DataSet as IBaseDBFilter do
            begin
              aID := FieldByName('ID').AsString;
              aFilter :=  Data.QuoteField('NUMBER')+'='+Data.QuoteValue(aID);
              Filter := aFilter;
              Limit := 0;
            end;
          aObj.Open;
          if aObj.Count=0 then
            begin
              aObj.Insert;
              aObj.Text.AsString := Self.Text.AsString;
              aObj.FieldByName('SQL_ID').AsVariant:=Self.Id.AsVariant;
              if Assigned(Self.Matchcode) then
                aObj.Matchcode.AsString := Self.Matchcode.AsString;
              if Assigned(Self.Status) then
                aObj.Status.AsString := Self.Status.AsString;
              aObj.Number.AsVariant:=Self.Number.AsVariant;
              aObj.FieldByName('LINK').AsString:=Data.BuildLink(Self.DataSet);
              aObj.FieldByName('ICON').AsInteger:=Data.GetLinkIcon(Data.BuildLink(Self.DataSet) read  write ;
              aObj.FieldByName('VERSION').AsString:=Self.FieldByName('VERSION').AsString;
              aObj.Post;
              Self.GenerateThumbnail;
            end
          else //Modify existing
            begin
              try
                while aObj.Count>1 do
                  aObj.Delete;
              except
              end;
              if CanEdit then
                aObj.Edit;
              if aObj.Text.AsString<>Self.Text.AsString then
                begin
                  aObj.Edit;
                  aObj.Text.AsString := Self.Text.AsString;
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
              if Assigned(Self.Matchcode) and (aObj.Matchcode.AsString<>Self.Matchcode.AsString) then
                begin
                  aObj.Edit;
                  aObj.Matchcode.AsString := Self.Matchcode.AsString;
                end;
              if aObj.FieldByName('LINK').AsString<>Data.BuildLink(Self.DataSet) then
                begin
                  aObj.Edit;
                  aObj.FieldByName('LINK').AsString:=Data.BuildLink(Self.DataSet);
                end;
              if aObj.FieldByName('VERSION').AsString<>Self.FieldByName('VERSION').AsString then
                begin
                  aObj.Edit;
                  aObj.FieldByName('VERSION').AsString:=Self.FieldByName('VERSION').AsString;
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

function TMasterdataList.SelectFromLink(aLink: string) : Boolean;
var
  tmp1: String;
  tmp2: String;
  tmp3: String;
  tmp2v : Variant;
  tmp3v : Variant;
begin
  Result := inherited SelectFromLink(aLink);
  if not Result then
    begin
      tmp2v := Null;
      tmp3v := Null;
      if not (copy(aLink,0,pos('@',aLink)-1) = 'MASTERDATA') then exit;
      if rpos('{',aLink) > 0 then
        aLink := copy(aLink,0,rpos('{',aLink)-1)
      else if rpos('(',aLink) > 0 then
        aLink := copy(aLink,0,rpos('(',aLink)-1);
      aLink   := copy(aLink, pos('@', aLink) + 1, length(aLink));
      tmp1 := copy(aLink, 0, pos('&&', aLink) - 1);
      aLink   := copy(aLink, pos('&&', aLink) + 2, length(aLink));
      tmp2 := copy(aLink, 0, pos('&&', aLink) - 1);
      aLink   := copy(aLink, pos('&&', aLink) + 2, length(aLink));
      tmp3 := aLink;
      if tmp2 <> '' then tmp2v := tmp2;
      if tmp3 <> '' then tmp3v := tmp3;
      //Select(tmp1,tmp2v,tmp3v);
      //Result := True;
    end;
end;

function TMasterdataList.SelectFromLinkwoVersion(aLink: string): Boolean;
var
  tmp1: String;
  tmp2: String;
  tmp3: String;
  tmp2v : Variant;
  tmp3v : Variant;
begin
  Result := inherited SelectFromLink(aLink);
  if not Result then
    begin
      tmp2v := Null;
      tmp3v := Null;
      if not (copy(aLink,0,pos('@',aLink)-1) = 'MASTERDATA') then exit;
      if rpos('{',aLink) > 0 then
        aLink := copy(aLink,0,rpos('{',aLink)-1)
      else if rpos('(',aLink) > 0 then
        aLink := copy(aLink,0,rpos('(',aLink)-1);
      aLink   := copy(aLink, pos('@', aLink) + 1, length(aLink));
      tmp1 := copy(aLink, 0, pos('&&', aLink) - 1);
      aLink   := copy(aLink, pos('&&', aLink) + 2, length(aLink));
      tmp2 := copy(aLink, 0, pos('&&', aLink) - 1);
      aLink   := copy(aLink, pos('&&', aLink) + 2, length(aLink));
      tmp3 := aLink;
      if tmp2 <> '' then tmp2v := tmp2;
      if tmp3 <> '' then tmp3v := tmp3;
      //Select(tmp1,tmp2v);
      //Result := True;
    end;
end;

class function TMasterdataList.MapField(aField: string): string;
begin
  Result := aField;
  if Result = 'Typ' then
    Result := 'TYPE'
  else if Result = 'PackageUnit' then
    Result := 'UNIT'
  ;
end;

initialization
  RegisterdataSetClass('MASTERDATA',TMasterdata);
end.

