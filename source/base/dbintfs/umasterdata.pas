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
  Classes, SysUtils, db, uBaseDbClasses, uBaseERPDBClasses, uIntfStrConsts,uBaseDatasetInterfaces;
type

  { TMasterdataList }

  TMasterdataList = class(TBaseERPList)
  protected
    function GetMatchCodeFieldName: string;override;
    function GetTextFieldName: string;override;
    function GetNumberFieldName : string;override;
    function GetStatusFieldName : string;override;
  public
    constructor CreateEx(aOwner: TComponent; DM: TComponent;
       aConnection: TComponent=nil; aMasterdata: TDataSet=nil); override;
    constructor Create(aOwner : TComponent);override;
    function GetTyp: string; override;
    procedure DefineFields(aDataSet : TDataSet);override;
    procedure OpenItem(AccHistory: Boolean=True); override;
    procedure Select(aID : string);overload;
    procedure Select(aID : string;aVersion : Variant;aLanguage : Variant);overload;
    procedure Select(aID : string;aVersion : Variant);overload;
    function SelectFromLink(aLink: string): Boolean; override;
    function SelectFromLinkwoVersion(aLink: string): Boolean;
  end;
  TMasterdataHistory = class(TBaseHistory)
  end;
  TMasterdata = class;

  { TMDPos }

  TMDPos = class(TBaseDBPosition)
    procedure DataSetAfterPost(aDataSet: TDataSet);
  private
    FMasterdata: TMasterdata;
  protected
    function GetCurrency : string;override;
    procedure PosPriceChanged(aPosDiff,aGrossDiff :Extended);override;
    procedure PosWeightChanged(aPosDiff :Extended);override;
  public
    constructor CreateEx(aOwner : TComponent;DM : TComponent=nil;aConnection : TComponent = nil;aMasterdata : TDataSet = nil);override;
    procedure DefineFields(aDataSet : TDataSet);override;
    property Masterdata : TMasterdata read FMasterdata write FMasterdata;
  end;
  TSerials = class(TBaseDbDataSet)
  public
    procedure DefineFields(aDataSet : TDataSet);override;
  end;
  TStorage = class(TBaseDBDataSet)
  private
    FJournal: TStorageJournal;
    function GetJournal: TStorageJournal;
  public
    constructor CreateEx(aOwner : TComponent;DM : TComponent=nil;aConnection : TComponent = nil;aMasterdata : TDataSet = nil);override;
    destructor Destroy; override;
    function CreateTable : Boolean;override;
    procedure DefineFields(aDataSet : TDataSet);override;
    property Journal : TStorageJournal read GetJournal;
    function DoPost(OrderType: TBaseDBDataset; Order: TBaseDBDataset;
      aStorage: string; aQuantity, aReserve: real; QuantityUnit, PosNo: string
  ): real;
  end;

  { TSupplierPrices }

  TSupplierPrices = class(TBaseDBDataSet)
  private
  public
    constructor CreateEx(aOwner: TComponent; DM: TComponent; aConnection: TComponent
  =nil; aMasterdata: TDataSet=nil); override;
    procedure DefineFields(aDataSet : TDataSet);override;
  end;
  TSupplier = class(TBaseDBDataSet)
  private
    FPrices: TSupplierPrices;
  public
    constructor CreateEx(aOwner : TComponent;DM : TComponent=nil;aConnection : TComponent = nil;aMasterdata : TDataSet = nil);override;
    destructor Destroy; override;
    function CreateTable : Boolean;override;
    procedure DefineFields(aDataSet : TDataSet);override;
    property Prices : TSupplierPrices read FPrices;
  end;
  TMasterdataLinks = class(TLinks)
  public
    procedure FillDefaults(aDataSet : TDataSet);override;
  end;

  { TMasterdataPrices }

  TMasterdataPrices = class(TBaseDbDataSet)
    procedure FDSDataChange(Sender: TObject; Field: TField);
  private
    FDS: TDataSource;
    FMasterdata: TMasterdataList;
  public
    constructor CreateEx(aOwner: TComponent; DM: TComponent; aConnection: TComponent=nil; aMasterdata: TDataSet=nil); override;
    destructor Destroy; override;
    procedure DefineFields(aDataSet : TDataSet);override;
    procedure FillDefaults(aDataSet: TDataSet); override;
    function GetPriceType : Integer;
    function FormatCurrency(Value : real) : string;
    property Masterdata : TMasterdataList read FMasterdata write FMasterdata;
  end;
  TMdProperties = class(TBaseDbDataSet)
  public
    procedure DefineFields(aDataSet : TDataSet);override;
  end;
  TMasterdataTexts = class(TBaseDbDataSet)
  public
    procedure DefineFields(aDataSet : TDataSet);override;
  end;
  TRepairParts = class(TBaseDbDataSet)
  public
    procedure DefineFields(aDataSet : TDataSet);override;
  end;
  TRepairAssembly = class(TBaseDbDataSet)
  private
    FParts: TRepairParts;
  public
    constructor CreateEx(aOwner : TComponent;DM : TComponent=nil;aConnection : TComponent = nil;aMasterdata : TDataSet = nil);override;
    destructor Destroy;override;
    procedure DefineFields(aDataSet : TDataSet);override;
    function CreateTable : Boolean;override;
    property Parts : TRepairParts read FParts;
  end;
  TMasterdata = class(TMasterdataList,IBaseHistory)
    procedure FDSDataChange(Sender: TObject; Field: TField);
    procedure FSupplierDataSetAfterPost(aDataSet: TDataSet);
  private
    FAssembly: TRepairAssembly;
    FHistory: TMasterdataHistory;
    FImages: TImages;
    FLinks: TMasterdataLinks;
    FMeasurement: TMeasurement;
    FPosition: TMDPos;
    FPrices: TMasterdataPrices;
    FProperties: TMdProperties;
    FSerials: TSerials;
    FStateChange: TNotifyEvent;
    FStorage: TStorage;
    FSupplier: TSupplier;
    FTexts: TMasterdataTexts;
    FDS: TDataSource;
    FSTatus : string;
    function GetHistory : TBaseHistory;
    function GetLanguage: TField;
    function GetVersion: TField;
  public
    constructor CreateEx(aOwner : TComponent;DM : TComponent=nil;aConnection : TComponent = nil;aMasterdata : TDataSet = nil);override;
    destructor Destroy;override;
    procedure Open;override;
    function CreateTable : Boolean;override;
    procedure DefineFields(aDataSet : TDataSet);override;
    procedure FillDefaults(aDataSet : TDataSet);override;
    procedure CascadicPost;override;
    procedure CascadicCancel;override;
    property Version : TField read GetVersion;
    property Language : TField read GetLanguage;
    property Positions : TMDPos read FPosition;
    property History : TMasterdataHistory read FHistory;
    property Images : TImages read FImages;
    property Links : TMasterdataLinks read FLinks;
    property Texts : TMasterdataTexts read FTexts;
    property Storage : TStorage read FStorage;
    property Supplier : TSupplier read FSupplier;
    property Prices : TMasterdataPrices read FPrices;
    property Properties : TMdProperties read FProperties;
    property Assembly : TRepairAssembly read FAssembly;
    property Serials : TSerials read FSerials;
    property Measurements : TMeasurement read FMeasurement;
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
    function Find(aIdent : string;Unsharp : Boolean = False) : Boolean;override;
    procedure GenerateThumbnail; override;
    property OnStateChange : TNotifyEvent read FStateChange write FStateChange;
  end;
implementation
uses uBaseDBInterface, uBaseSearch, uBaseApplication, uBaseApplicationTools,
  uData, Utils,uOrder,uthumbnails,uprometscripts;

constructor TSupplierPrices.CreateEx(aOwner: TComponent; DM: TComponent;
  aConnection: TComponent; aMasterdata: TDataSet);
begin
  inherited CreateEx(aOwner, DM, aConnection, aMasterdata);
  UpdateFloatFields:=True;
end;

procedure TSupplierPrices.DefineFields(aDataSet: TDataSet);
begin
  with aDataSet as IBaseManageDB do
    begin
      TableName := 'SUPPLIERPRICES';
      TableCaption:=strPrices;
      if Assigned(ManagedFieldDefs) then
        with ManagedFieldDefs do
          begin
            Add('FROMUNIT',ftFloat,0,False);
            Add('QUANTITYU',ftString,10,False);
            Add('DISCOUNT',ftFloat,0,False);
            Add('PRICE',ftFloat,0,True);
            Add('CURRENCY',ftString,3,False);
          end;
    end;
end;
constructor TSupplier.CreateEx(aOwner: TComponent; DM: TComponent;
  aConnection: TComponent; aMasterdata: TDataSet);
begin
  inherited CreateEx(aOwner, DM, aConnection, aMasterdata);
  FPrices := TSupplierPrices.CreateEx(Owner,DataModule,aConnection,DataSet);
end;
destructor TSupplier.Destroy;
begin
  FPrices.Free;
  inherited Destroy;
end;
function TSupplier.CreateTable : Boolean;
begin
  Result := inherited CreateTable;
  FPrices.CreateTable;
end;
procedure TSupplier.DefineFields(aDataSet: TDataSet);
begin
  with aDataSet as IBaseManageDB do
    begin
      TableName := 'SUPPLIER';
      TableCaption:=strSupplier;
      if Assigned(ManagedFieldDefs) then
        with ManagedFieldDefs do
          begin
            Add('ACCOUNTNO',ftString,60,True);
            Add('NAME',ftString,260,True);
            Add('DELIVERTM',ftInteger,0,False);
            Add('EID',ftString,30,False);
            Add('TRANSPORT',ftFloat,0,False);
            Add('TRANSCUR',ftString,3,False);
          end;
    end;
end;
procedure TRepairParts.DefineFields(aDataSet: TDataSet);
begin
  with aDataSet as IBaseManageDB do
    begin
      TableName := 'REPAIRPARTS';
      TableCaption:=strRepairParts;
      if Assigned(ManagedFieldDefs) then
        with ManagedFieldDefs do
          begin
            Add('PART',ftString,60,False);
          end;
    end;
end;
constructor TRepairAssembly.CreateEx(aOwner: TComponent; DM: TComponent;
  aConnection: TComponent; aMasterdata: TDataSet);
begin
  inherited CreateEx(aOwner, DM, aConnection, aMasterdata);
  FParts := TRepairParts.CreateEx(Self,DataModule,aConnection,DataSet);
end;
destructor TRepairAssembly.Destroy;
begin
  FParts.Free;
  inherited Destroy;
end;
procedure TRepairAssembly.DefineFields(aDataSet: TDataSet);
begin
  with aDataSet as IBaseManageDB do
    begin
      TableName := 'REPAIRASSEMBLY';
      TableCaption:=strRepairAssembly;
      if Assigned(ManagedFieldDefs) then
        with ManagedFieldDefs do
          begin
            Add('ASSEMBLY',ftString,60,False);
          end;
    end;
end;

function TRepairAssembly.CreateTable : Boolean;
begin
  Result := inherited CreateTable;
  FParts.CreateTable;
end;

procedure TMasterdataTexts.DefineFields(aDataSet: TDataSet);
begin
  with aDataSet as IBaseManageDB do
    begin
      TableName := 'TEXTS';
      TableCaption:=strTexts;
      if Assigned(ManagedFieldDefs) then
        with ManagedFieldDefs do
          begin
            Add('TEXTTYPE',ftInteger,0,False);
            Add('TEXT',ftMemo,0,False);
          end;
    end;
end;
procedure TMdProperties.DefineFields(aDataSet: TDataSet);
begin
  with aDataSet as IBaseManageDB do
    begin
      TableName := 'PROPERTIES';
      TableCaption:=strProperties;
      if Assigned(ManagedFieldDefs) then
        with ManagedFieldDefs do
          begin
            Add('PROPERTY',ftString,50,false);
            Add('VALUE',ftString,50,false);
            Add('UNIT',ftString,10,false);
          end;
    end;
end;

procedure TMasterdataPrices.FDSDataChange(Sender: TObject; Field: TField);
begin
  if not Assigned(Field) then exit;
  if DataSet.ControlsDisabled then exit;
  if Field.FieldName = 'PRICE' then
    begin
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
    end;
end;

constructor TMasterdataPrices.CreateEx(aOwner: TComponent; DM: TComponent;
  aConnection: TComponent; aMasterdata: TDataSet);
begin
  inherited CreateEx(aOwner, DM, aConnection, aMasterdata);
  FDS := TDataSource.Create(Self);
  FDS.DataSet := DataSet;
  FDS.OnDataChange:=@FDSDataChange;
end;

destructor TMasterdataPrices.Destroy;
begin
  FDS.Free;
  inherited Destroy;
end;

procedure TMasterdataPrices.DefineFields(aDataSet: TDataSet);
begin
  with aDataSet as IBaseManageDB do
    begin
      TableName := 'MDPRICES';
      TableCaption:=strPrices;
      UpdateFloatFields:=True;
      if Assigned(ManagedFieldDefs) then
        with ManagedFieldDefs do
          begin
            Add('PTYPE',ftString,4,True);
            Add('PRICE',ftFloat,0,false);
            Add('NOTE',ftString,500,False);
            Add('CURRENCY',ftString,3,true);
            Add('MINCOUNT',ftFloat,0,False);
            Add('MAXCOUNT',ftFloat,0,False);
            Add('VALIDFROM',ftDate,0,False);
            Add('VALIDTO',ftDate,0,False);
            Add('CUSTOMER',ftString,20,False);
          end;
    end;
end;

procedure TMasterdataPrices.FillDefaults(aDataSet: TDataSet);
begin
  inherited FillDefaults(aDataSet);
  FieldByName('PTYPE').AsString:='SAP';
  if Data.Currency.DataSet.Active and Data.Currency.DataSet.Locate('DEFAULTCUR', 'Y', []) then
    FieldByName('CURRENCY').AsString := Data.Currency.FieldByName('SYMBOL').AsString;
end;

function TMasterdataPrices.GetPriceType: Integer;
var
  PriceType: TPriceTypes;
begin
  Result := 0;
  try
    PriceType := TPriceTypes.CreateEx(Self,DataModule,Connection);
    PriceType.Open;
    if PriceType.DataSet.Locate('SYMBOL', trim(DataSet.FieldByName('PTYPE').AsString), []) then
      Result := StrToIntDef(copy(Pricetype.FieldByName('TYPE').AsString, 0, 2), 0);
  finally
    PriceType.Free;
  end;
end;
function TMasterdataPrices.FormatCurrency(Value: real): string;
begin
  Result := FormatFloat('0.00',Value)+' '+DataSet.FieldByName('CURRENCY').AsString;
end;
procedure TMasterdataLinks.FillDefaults(aDataSet: TDataSet);
begin
  inherited FillDefaults(aDataSet);
  aDataSet.FieldByName('RREF_ID').AsVariant:=(Parent as TMasterdata).Id.AsVariant;
end;
procedure TSerials.DefineFields(aDataSet: TDataSet);
begin
  with aDataSet as IBaseManageDB do
    begin
      TableName := 'SERIALS';
      TableCaption:=strSerial;
      if Assigned(ManagedFieldDefs) then
        with ManagedFieldDefs do
          begin
            Add('SERIAL',ftString,30,False);
            Add('NOTE',ftString,500,False);
          end;
    end;
end;

function TStorage.GetJournal: TStorageJournal;
begin
  if not Assigned(FJournal) then
    begin
      FJournal := TStorageJournal.CreateEx(Self,DataModule,Connection);
      FJournal.CreateTable;
    end;
  Result := FJournal;
end;

constructor TStorage.CreateEx(aOwner: TComponent; DM : TComponent;aConnection: TComponent;
  aMasterdata: TDataSet);
begin
  inherited CreateEx(aOwner, DM,aConnection, aMasterdata);
end;

destructor TStorage.Destroy;
begin
  if Assigned(FJournal) then FJournal.Free;
  inherited Destroy;
end;

function TStorage.CreateTable : Boolean;
begin
  Result := inherited CreateTable;
end;

procedure TStorage.DefineFields(aDataSet: TDataSet);
begin
  with aDataSet as IBaseManageDB do
    begin
      UpdateFloatFields:=True;
      TableName := 'STORAGE';
      TableCaption:=strStorage;
      if Assigned(ManagedFieldDefs) then
        with ManagedFieldDefs do
          begin
            Add('STORAGEID',ftString,3,True);
            Add('STORNAME',ftString,30,True);
            Add('PLACE',ftString,20,False);
            Add('QUANTITY',ftFloat,0,False);
            Add('RESERVED',ftFloat,0,False);
            Add('QUANTITYU',ftString,10,False);
            Add('CHARGE',ftInteger,0,False);
          end;
    end;
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
  except
    result := 0;
  end;
end;

function TMasterdata.GetVersion: TField;
begin
  Result := DataSet.FieldByName('VERSION');
end;

function TMasterdataList.GetTyp: string;
begin
  Result := 'M';
end;

procedure TMasterdata.FDSDataChange(Sender: TObject; Field: TField);
begin
  if not Assigned(Field) then exit;
  if DataSet.ControlsDisabled then exit;
  if Field.FieldName = 'STATUS' then
    begin
      if FStatus=Field.AsString then exit;
      History.Open;
      History.AddItem(Self.DataSet,Format(strStatusChanged,[FStatus,Field.AsString]),'','',nil,ACICON_STATUSCH);
      FStatus := Field.AsString;
      if Assigned(FStateChange) then
        FStateChange(Self);
      OpenItem(False);
    end;
  if (Field.FieldName = 'ID') then
    begin
      History.AddItem(Self.DataSet,Format(strNumberChanged,[Field.AsString]),'','',DataSet,ACICON_EDITED);
    end;
end;

procedure TMasterdata.FSupplierDataSetAfterPost(aDataSet: TDataSet);
begin
  Change;
end;

function TMasterdata.GetHistory: TBaseHistory;
begin
  Result := History;
end;
function TMasterdata.GetLanguage: TField;
begin
  Result := DataSet.FieldByName('LANGUAGE');
end;
constructor TMasterdata.CreateEx(aOwner: TComponent;DM : TComponent; aConnection: TComponent;
  aMasterdata: TDataSet);
begin
  inherited CreateEx(aOwner, DM, aConnection, aMasterdata);
  with BaseApplication as IBaseDbInterface do
    begin
      with DataSet as IBaseDBFilter do
        begin
          UsePermissions:=False;
        end;
    end;
  FPosition := TMDPos.CreateEx(Self, DataModule,aConnection,DataSet);
  FPosition.Masterdata:=Self;
  FStorage := TStorage.CreateEx(Self,DataModule,aConnection,DataSet);
  FHistory := TMasterdataHistory.CreateEx(Self,DataModule,aConnection,DataSet);
  FImages := TImages.CreateEx(Self,DataModule,aConnection,DataSet);
  FLinks := TMasterdataLinks.CreateEx(Self,DataModule,aConnection);
  FTexts := TMasterdataTexts.CreateEx(Self,DataModule,aConnection,DataSet);
  FPrices := TMasterdataPrices.CreateEx(Self,DataModule,aConnection,DataSet);
  FPrices.Masterdata:=Self;
  FProperties := TMdProperties.CreateEx(Self,DataModule,aConnection,DataSet);
  FAssembly := TRepairAssembly.CreateEx(Self,DataModule,aConnection,DataSet);
  FSupplier := TSupplier.CreateEx(Self,DataModule,aConnection,DataSet);
  FSerials := TSerials.CreateEx(Self,DataModule,aConnection,DataSet);
  FMeasurement := TMeasurement.CreateEx(Self,DataModule,aConnection,DataSet);
  FDS := TDataSource.Create(Self);
  FDS.DataSet := DataSet;
  FDS.OnDataChange:=@FDSDataChange;
  FSupplier.DataSet.AfterPost:=@FSupplierDataSetAfterPost;
end;
destructor TMasterdata.Destroy;
begin
  FDS.Free;
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

procedure TMasterdata.Open;
begin
  inherited Open;
  FStatus := Status.AsString;
end;

function TMasterdata.CreateTable : Boolean;
var
  aUnits: TUnits;
begin
  Result := inherited CreateTable;
  FPosition.CreateTable;
  FStorage.CreateTable;
  FHistory.CreateTable;
  FTexts.CreateTable;
  FLinks.CreateTable;
  FPrices.CreateTable;
  FProperties.CreateTable;
  FSerials.CreateTable;
  FAssembly.CreateTable;
  FSupplier.CreateTable;
  aUnits := TUnits.CreateEx(nil,DataModule,Connection);
  aUnits.CreateTable;
  aUnits.Free;
end;
procedure TMasterdata.DefineFields(aDataSet: TDataSet);
begin
  inherited DefineFields(aDataSet);
  with aDataSet as IBaseDbFilter, BaseApplication as IBaseDbInterface do
    BaseFilter := '';
end;
procedure TMasterdata.FillDefaults(aDataSet: TDataSet);
var
  Vat: TVat;
begin
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
end;
procedure TMasterdata.CascadicPost;
begin
  FPosition.CascadicPost;
  FStorage.CascadicPost;
  FHistory.CascadicPost;
  FTexts.CascadicPost;
  FLinks.CascadicPost;
  FPrices.CascadicPost;
  FProperties.CascadicPost;
  FAssembly.CascadicPost;
  inherited CascadicPost;
end;
procedure TMasterdata.CascadicCancel;
begin
  FPosition.CascadicCancel;
  FStorage.CascadicCancel;
  FHistory.CascadicCancel;
  FTexts.CascadicCancel;
  FLinks.CascadicCancel;
  FPrices.CascadicCancel;
  FProperties.CascadicCancel;
  FAssembly.CascadicCancel;
  inherited CascadicCancel;
end;
function TMasterdata.Copy(aNewVersion: Variant; aNewLanguage: Variant;
  cPrices: Boolean; cProperties: Boolean; cTexts: Boolean; cSupplier: Boolean;
  cPiecelists: Boolean): Boolean;
var
  bMasterdata: TMasterdata;
begin
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
end;

function TMasterdata.Versionate(aNewversion: Variant; aMakeActive: Boolean;
  cPrices: Boolean; cProperties: Boolean; cTexts: Boolean; cSupplier: Boolean;
  cPiecelists: Boolean): Boolean;
var
  bMasterdata: TMasterdata;
begin
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
end;

function TMasterdata.Find(aIdent: string;Unsharp : Boolean = False): Boolean;
begin
  with DataSet as IBaseDbFilter,BaseApplication as IBaseDbInterface do
    Filter := '('+Data.QuoteField(GetNumberFieldName)+'='+Data.QuoteValue(aIdent)+') OR ('+Data.QuoteField('MATCHCODE')+'='+Data.QuoteValue(aIdent)+') and ('+Data.QuoteField('ACTIVE')+'='+Data.QuoteValue('Y')+')';
  Open;
  Result := Count > 0;
  if (not Result) and Unsharp then
    begin
      with DataSet as IBaseDbFilter,BaseApplication as IBaseDbInterface do
        Filter := '('+Data.ProcessTerm(Data.QuoteField(GetNumberFieldName)+'='+Data.QuoteValue(aIdent+'*'))+') OR ('+Data.ProcessTerm(Data.QuoteField('MATCHCODE')+'='+Data.QuoteValue(aIdent+'*'))+')';
      Open;
      Result := Count > 0;
    end;
end;

procedure TMasterdata.GenerateThumbnail;
var
  aThumbnail: TThumbnails;
begin
  aThumbnail := TThumbnails.CreateEx(nil,DataModule);
  aThumbnail.CreateTable;
  aThumbnail.SelectByRefId(Self.Id.AsVariant);
  aThumbnail.Open;
  if aThumbnail.Count=0 then
    Images.GenerateThumbnail(aThumbnail);
  aThumbnail.Free;
end;

procedure TMDPos.DataSetAfterPost(aDataSet: TDataSet);
begin
  FMasterdata.Change;
end;

function TMDPos.GetCurrency: string;
begin
  Result:=Masterdata.FieldByName('CURRENCY').AsString;
end;

procedure TMDPos.PosPriceChanged(aPosDiff, aGrossDiff: Extended);
begin
  //TODO: Implement me (Einkaufspreis anpassen)
end;
procedure TMDPos.PosWeightChanged(aPosDiff: Extended);
begin
  if not ((Masterdata.DataSet.State = dsEdit) or (Masterdata.DataSet.State = dsInsert)) then
    Masterdata.DataSet.Edit;
  Masterdata.FieldByName('WEIGHT').AsFloat := Masterdata.FieldByName('WEIGHT').AsFloat+aPosDiff;
end;
constructor TMDPos.CreateEx(aOwner: TComponent; DM: TComponent;
  aConnection: TComponent; aMasterdata: TDataSet);
begin
  inherited CreateEx(aOwner, DM,aConnection, aMasterdata);
  DataSet.AfterPost:=@DataSetAfterPost;
end;
procedure TMDPos.DefineFields(aDataSet: TDataSet);
begin
  inherited DefineFields(aDataSet);
  with aDataSet as IBaseManageDB do
    begin
      TableName := 'MDPOSITIONS';
    end;
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
constructor TMasterdataList.CreateEx(aOwner: TComponent; DM: TComponent;
  aConnection: TComponent; aMasterdata: TDataSet);
begin
  inherited CreateEx(aOwner, DM, aConnection, aMasterdata);
  with BaseApplication as IBaseDbInterface do
    begin
      with DataSet as IBaseDBFilter do
        begin
          UsePermissions:=True;
        end;
    end;
end;

constructor TMasterdataList.Create(aOwner: TComponent);
begin
  CreateEx(aOwner,Data,nil,nil);
end;

procedure TMasterdataList.DefineFields(aDataSet: TDataSet);
begin
  with aDataSet as IBaseManageDB do
    begin
      TableName := 'MASTERDATA';
      TableCaption := strMasterdata;
      UpdateFloatFields:=True;
      if Assigned(ManagedFieldDefs) then
        with ManagedFieldDefs do
          begin
            Add('TYPE',ftString,1,True);
            Add('ID',ftString,40,True);
            Add('VERSION',ftString,25,False);
            Add('LANGUAGE',ftString,3,False);
            Add('ACTIVE',ftString,1,True);
            Add('STATUS',ftString,4,false);
            Add('BARCODE',ftString,20,False);
            Add('MATCHCODE',ftString,200,False);
            Add('SHORTTEXT',ftString,240,False);
            Add('TREEENTRY',ftLargeint,0,True);
            Add('QUANTITYU',ftString,10,False);//Mengeneinheit
            Add('VAT',ftString,1,True);        //Mehrwertsteuer
            Add('USESERIAL',ftString,1,False);
            Add('OWNPROD',ftString,1,False);
            Add('SALEITEM',ftString,1,False);
            Add('USEBATCH',ftString,1,False);
            Add('NOSTORAGE',ftString,1,False);
            Add('PTYPE',ftString,1,False);
            Add('WEIGHT',ftFloat,0,False);
            Add('REPAIRTIME',ftInteger,0,False);     //max. Reparaturzeit
            Add('UNIT',ftInteger,0,False);     //Verpackungseinheit
            Add('WARRENTY',ftString,10,False);
            Add('MANUFACNR',ftString,40,False);
            Add('DISPOTYPE',ftString,1,False);   //Planart 0=nicht Disponieren 1=Volldispo 2=disponieren 3=Mindestbestand
            Add('VALIDFROM',ftDate,0,False);   //Ein/Auslaufsteuerung
            Add('VALIDTO',ftDate,0,False);     //gültig bis Datum
            Add('VALIDTOME',ftInteger,0,False);//gültig bis Menge
            Add('COSTCENTRE',ftString,10,False);//Kostenstelle
            Add('ACCOUNT',ftString,10,False); //Fibu Konto
            Add('ACCOUNTINGINFO',ftMemo,0,False); //Fibu Info
            Add('CATEGORY',ftString,60,False);
            Add('SELLPRICE',ftFloat,0,False);
            Add('PURCHASE',ftFloat,0,False);
            Add('ISTEMPLATE',ftString,1,False);
            Add('CURRENCY',ftString,5,False);
            Add('IMAGEREF',ftLargeint,0,False);
            Add('SCRIPT',ftString,60,False);
            Add('SCRIPTVER',ftString,8,False);
            Add('SCRIPTFUNC',ftString,60,False);
            Add('PRSCRIPT',ftString,60,False);
            Add('PRSCRIPTVER',ftString,8,False);
            Add('PRSCRIPTFUNC',ftString,160,False);
            Add('PREPTEXT',ftString,100,False);
            Add('WORKTEXT',ftString,100,False);
            Add('CRDATE',ftDate,0,False);
            Add('CHDATE',ftDate,0,False);
            Add('CHANGEDBY',ftString,4,False);
            Add('CREATEDBY',ftString,4,true);
          end;
      if Assigned(ManagedIndexdefs) then
        with ManagedIndexDefs do
          begin
            Add('ID','TYPE;ID;VERSION;LANGUAGE',[ixUnique]);
            Add('BARCODE','BARCODE',[]);
            Add('SHORTTEXT','SHORTTEXT',[]);
            Add('STATUS','STATUS',[]);
            Add('MATCHCODE','MATCHCODE',[]);
          end;
      if Data.ShouldCheckTable(TableName) then
        DefineUserFields(aDataSet);
    end;
  with aDataSet as IBaseDbFilter, BaseApplication as IBaseDbInterface do
    BaseFilter := Data.QuoteField('ACTIVE')+'='+Data.QuoteValue('Y');
end;

procedure TMasterdataList.OpenItem(AccHistory: Boolean);
var
  aObj: TObjects = nil;
  aID: String;
  aFilter: String;
begin
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
              aObj.FieldByName('ICON').AsInteger:=Data.GetLinkIcon(Data.BuildLink(Self.DataSet),True);
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
end;

procedure TMasterdataList.Select(aID: string);
begin
  SelectFromNumber(aID);
end;
procedure TMasterdataList.Select(aID: string; aVersion: Variant; aLanguage: Variant
  );
begin
  with BaseApplication as IBaseDbInterface do
    begin
      with DataSet as IBaseDBFilter do
        begin
          Filter := '('
                   +Data.ProcessTerm(Data.QuoteField('ID')+'='+Data.QuoteValue(aID))+' and '
                   +Data.ProcessTerm(Data.QuoteField('VERSION')+'='+VarToStr(aVersion))+' and '
                   +Data.ProcessTerm(Data.QuoteField('LANGUAGE')+'='+VarToStr(aLanguage))+')';
        end;
    end;
end;

procedure TMasterdataList.Select(aID: string; aVersion: Variant);
begin
  with BaseApplication as IBaseDbInterface do
    begin
      with DataSet as IBaseDBFilter do
        begin
          Filter := '('
                   +Data.ProcessTerm(Data.QuoteField('ID')+'='+Data.QuoteValue(aID))+' and '
                   +Data.ProcessTerm(Data.QuoteField('VERSION')+'='+VarToStr(aVersion))+')';
        end;
    end;
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
      Select(tmp1,tmp2v,tmp3v);
      Result := True;
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
      Select(tmp1,tmp2v);
      Result := True;
    end;
end;

initialization
  RegisterdataSetClass('MASTERDATA',TMasterdata);
end.

