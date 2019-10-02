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
  Classes, SysUtils, uBaseDbClasses, db, ContNrs, uIntfStrConsts,uBaseDatasetInterfaces;
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
    constructor CreateEx(aOwner: TComponent; DM: TComponent; aConnection: TComponent=nil;
      aMasterdata: TDataSet=nil); override;
    function CombineItems(aRemoteLink : string) : Boolean;virtual;
  published
    procedure CascadicPost;override;
    procedure CascadicCancel; override;
  end;
  TStorageTypes = class(TBaseDBDataSet)
  public
    procedure DefineFields(aDataSet : TDataSet);override;
  end;
  TCurrency = class(TBaseDBDataSet)
  public
    procedure DefineFields(aDataSet : TDataSet);override;
    function Convert(Price: real; SourceCurr, TargetCurr: string): real;
  end;
  TPaymentTargets = class(TBaseDBDataSet)
  public
    procedure DefineFields(aDataSet : TDataSet);override;
  end;
  TPositionTyp = class(TBaseDBDataSet)
  public
    procedure DefineFields(aDataSet : TDataSet);override;
  end;
  TPriceTypes = class(TBaseDBDataSet)
  public
    procedure DefineFields(aDataSet : TDataSet);override;
    function Get(aSymbol : string) : Integer;
  end;
  TVat = class(TBaseDBDataSet)
    procedure DefineFields(aDataSet : TDataSet);override;
  end;
  TBaseDBPosition = class;

  { TPositionCalc }

  TPositionCalc = class(TbaseDBDataSet)
  private
    FPosition: TBaseDBPosition;
  published
    constructor CreateEx(aOwner: TComponent; DM: TComponent;
      aConnection: TComponent=nil; aMasterdata: TDataSet=nil); override;
    procedure DefineFields(aDataSet : TDataSet);override;
    property Position : TBaseDBPosition read FPosition;
  end;

  { TBaseDBPosition }

  TBaseDBPosition = class(TBaseDbDataSet)
    procedure DataSetAfterCancel(aDataSet: TDataSet);
    procedure DataSetAfterDelete(aDataSet: TDataSet);
    procedure DataSetAfterPost(aDataSet: TDataSet);
    procedure DataSetBeforeCancel(aDataSet: TDataSet);
    procedure DataSetBeforeDelete(aDataSet: TDataSet);
    procedure FIntDataSourceDataChange(Sender: TObject; Field: TField);
    procedure FIntDataSourceStateChange(Sender: TObject);
  private
    FImages: TImages;
    FPosFormat: string;
    FPosTyp : TPositionTyp;
    FIntDataSource : TDataSource;
    FCalculationDisabled : Integer;
    FDoNumber: Boolean;
    FUseRTF: Boolean;
    OldPosPrice : real;
    OldGrossPrice : real;
    OldPosWeight : real;
    FOldPosNo: Integer;
    FVat: TVat;
    PriceTypes : TPriceTypes;
    function GetIdent: TField;
    function GetPosNo: TField;
    function GetPosTyp: TPositionTyp;
    procedure DoCalcPosPrice(Setprice : Boolean = False);
    procedure DoCalcGrossPosPrice;
    function GetShorttext: TField;
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
    constructor CreateEx(aOwner : TComponent;DM : TComponent=nil;aConnection : TComponent = nil;aMasterdata : TDataSet = nil);override;
    destructor Destroy;override;
    function CreateTable : Boolean;override;
    procedure Open;override;
    procedure DefineFields(aDataSet : TDataSet);override;
    procedure FillDefaults(aDataSet : TDataSet);override;
    procedure Assign(Source: TPersistent); override;
    procedure DirectAssign(Source: TPersistent);override;
    property PosTyp : TPositionTyp read GetPosTyp;
    property PosTypDec : Integer read GetPosTypDec;
    property PosCalc : TPositionCalc read FPosCalc;
    property Images : TImages read FImages;
    property Vat : TVat read FVat;
    property Ident : TField read GetIdent;
    property PosFormat : string read FPosFormat write FPosFormat;
    property CanHandleRTF : Boolean read FUseRTF write FUseRTF;
    procedure DisableCalculation;
    procedure EnableCalculation;
    function IsCalculationDisabled : Boolean;
    procedure AppendSubTotal;
    //Fields
    property PosNo : TField read GetPosNo;
    property Shorttext : TField read GetShorttext;
  end;
  TStorageTyp = class(TBaseDBDataSet)
  public
    procedure DefineFields(aDataSet : TDataSet);override;
  end;
  TStorageJournal = class(TBaseDBDataSet)
  public
    procedure DefineFields(aDataSet : TDataSet);override;
  end;
  TCountries = class(TBaseDBDataSet)
    procedure DefineFields(aDataSet : TDataSet);override;
  end;
  TLanguages = class(TBaseDbDataSet)
    procedure DefineFields(aDataSet : TDataSet);override;
  end;
  TStates = class(TBaseDBDataSet)
    procedure DefineFields(aDataSet : TDataSet);override;
  end;
  TCategory = class(TBaseDBDataSet)
    procedure DefineFields(aDataSet : TDataSet);override;
    constructor CreateEx(aOwner: TComponent; DM: TComponent; aConnection: TComponent
  =nil; aMasterdata: TDataSet=nil); override;
  end;
  TFinancialAccounts = class(TBaseDBDataSet)
    procedure DefineFields(aDataSet : TDataSet);override;
  end;
  TUnits = class(TBaseDBDataSet)
    procedure DefineFields(aDataSet : TDataSet);override;
  end;
  TDispatchTypes = class(TBaseDBDataSet)
    procedure DefineFields(aDataSet : TDataSet);override;
  end;
  TRepairProblems = class(TBaseDBDataSet)
    procedure DefineFields(aDataSet : TDataSet);override;
  end;
  TTextTypes = class(TBaseDBDataSet)
    procedure DefineFields(aDataSet : TDataSet);override;
  end;
  TInventoryPos = class(TBaseDbDataSet)
  private
    FInv: TBaseDbList;
  public
    procedure DefineFields(aDataSet : TDataSet);override;
    property Inventory : TBaseDbList read FInv;
  end;

  { TInventorys }

  TInventorys = class(TBaseDbList)
  private
    FPos: TInventoryPos;
  public
    constructor CreateEx(aOwner : TComponent;DM : TComponent=nil;aConnection : TComponent = nil;aMasterdata : TDataSet = nil);override;
    destructor Destroy; override;
    function CreateTable : Boolean;override;
    procedure FillDefaults(aDataSet: TDataSet); override;
    procedure DefineFields(aDataSet : TDataSet);override;
    property Positions : TInventoryPos read FPos;
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
uses uBaseDBInterface,uMasterdata, uBaseApplication,Math,Variants,uRTFtoTXT,
  uDocuments,usync,uprometscripts;
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

{ TFinancialAccounts }

procedure TFinancialAccounts.DefineFields(aDataSet: TDataSet);
begin
  with aDataSet as IBaseManageDB do
    begin
      TableName := 'FACCOUNTS';
      if Assigned(ManagedFieldDefs) then
        with ManagedFieldDefs do
          begin
            Add('ACCOUNTNO',ftString,10,true);
            Add('NAME',ftString,60,True);
            Add('ACTIVE',ftString,1,false);
          end;
    end;
end;

procedure TCategory.DefineFields(aDataSet: TDataSet);
begin
  with aDataSet as IBaseManageDB do
    begin
      TableName := 'CATEGORY';
      if Assigned(ManagedFieldDefs) then
        with ManagedFieldDefs do
          begin
            Add('TYPE',ftString,1,false);
            Add('NAME',ftString,60,True);
            Add('COLOR',ftString,30,False);
            Add('ACTIVE',ftString,1,false);
          end;
    end;
end;

constructor TCategory.CreateEx(aOwner: TComponent; DM: TComponent;
  aConnection: TComponent; aMasterdata: TDataSet);
begin
  inherited;
  with BaseApplication as IBaseDbInterface do
    begin
      with DataSet as IBaseManageDB do
        UseIntegrity:=False;
      with DataSet as IBaseDBFilter do
        begin
          Limit := 0;
        end;
    end;
end;

procedure TInventoryPos.DefineFields(aDataSet: TDataSet);
begin
  with aDataSet as IBaseManageDB do
    begin
      TableName := 'INVENTORYPOS';
      TableCaption := strInventoryPos;
      if Assigned(ManagedFieldDefs) then
        with ManagedFieldDefs do
          begin
            Add('POSNO',ftInteger,0,True);
            Add('IDENT',ftString,20,True);
            Add('VERSION',ftString,25,False);
            Add('LANGUAGE',ftString,3,False);
            Add('SHORTTEXT',ftString,200,True);
            Add('STORAGE',ftString,3,True);
            Add('QUANTITY',ftFloat,0,False);
            Add('QUANTITYC',ftFloat,0,False);
            Add('QUANTITYU',ftString,10,False);
            Add('PURCHASE',ftFloat,0,False);
            Add('PRICE',ftFloat,0,False);
            Add('CURRENCY',ftString,5,False);
          end;
    end;
end;
constructor TInventorys.CreateEx(aOwner: TComponent; DM: TComponent;
  aConnection: TComponent; aMasterdata: TDataSet);
begin
  inherited CreateEx(AOwner,DM,aConnection,aMasterdata);
  FPos := TInventoryPos.CreateEx(Owner,DataModule,aConnection,DataSet);
  FPos.FInv := Self;
end;
destructor TInventorys.Destroy;
begin
  inherited Destroy;
end;
function TInventorys.CreateTable : Boolean;
begin
  Result := inherited CreateTable;
  FPos.CreateTable;
end;

procedure TInventorys.FillDefaults(aDataSet: TDataSet);
begin
  inherited FillDefaults(aDataSet);
  with aDataSet,BaseApplication as IBaseDbInterface do
    begin
      FieldByName('INVNO').AsInteger := RecordCount+1;
    end;
end;

procedure TInventorys.DefineFields(aDataSet: TDataSet);
begin
  with aDataSet as IBaseManageDB do
    begin
      TableName := 'INVENTORY';
      TableCaption := strInventory;
      if Assigned(ManagedFieldDefs) then
        with ManagedFieldDefs do
          begin
            Add('INVNO',ftInteger,0,True);
            Add('DESC',ftString,30,True);
            Add('STATUS',ftString,4,True);
            Add('DATE',ftDateTime,0,False);
            Add('PURCHASE',ftFloat,0,False);
            Add('PRICE',ftFloat,0,False);
            Add('PURCHASED',ftFloat,0,False);
            Add('PRICED',ftFloat,0,False);
            Add('CREATEDBY',ftString,4,True);
          end;
    end;
end;
constructor TBaseERPList.CreateEx(aOwner: TComponent; DM: TComponent;
  aConnection: TComponent; aMasterdata: TDataSet);
begin
  inherited CreateEx(aOwner, DM, aConnection, aMasterdata);
  FUpdateHistory:=True;
end;
procedure TBaseERPList.CascadicPost;
var
  Hist : IBaseHistory;
  sType: String;
  i: Integer;
  saType: String;
  aField: TField;
begin
  if CanEdit then
    begin
      Post;
      if UpdateHistory and Supports(Self, IBaseHistory, Hist) then
        begin
          if not Hist.History.DataSet.Active then Hist.History.Open;
          if (Hist.History.Count=0) or (DataSet.State = dsInsert) then
            begin
              sType := strCreated;
            end
          else
            begin
              sType := strEdited;
              if not Hist.History.ChangedDuringSession then
                begin
                  try
                    saType:=' (';
                    for i := 0 to DataSet.Fields.Count-1 do
                      begin
                        aField := DataSet.Fields[i];
                        if aField.OldValue<>aField.NewValue then
                          saType := saType+','+aField.FieldName;
                      end;
                    saType:=Stringreplace(saType,'(,','(',[rfReplaceAll])+')';
                    saType:=Stringreplace(saType,'()','',[rfReplaceAll]);
                  except
                    saType:='';
                  end;
                end;
            end;
          sType:=sType+saType;
          Hist.History.AddItem(Self.DataSet,sType,'','',nil,0,'',True,True,False);
          Hist.History.ChangedDuringSession := False;
        end;
    end;
  inherited CascadicPost;
end;
procedure TBaseERPList.CascadicCancel;
var
  Hist : IBaseHistory;
begin
  inherited CascadicCancel;
  if Supports(Self, IBaseHistory, Hist) then
    Hist.History.ChangedDuringSession := False;
end;

function TBaseERPList.CombineItems(aRemoteLink: string): Boolean;
var
  aClass: TBaseDBDatasetClass;
  aObject: TBaseDBDataset;
  Hist,OwnHist : IBaseHistory;
  aDoc: TDocuments;
  aSync: TSyncItems;
begin
  if TBaseDBModule(DataModule).DataSetFromLink(aRemoteLink,aClass) then
    begin
      aObject := aClass.CreateEx(nil,DataModule);
      TBaseDbList(aObject).SelectFromLink(aRemoteLink);
      aObject.Open;
      if aObject.Count>0 then
        begin
          //Combine History
          if Supports(aObject, IBaseHistory, Hist)
          and Supports(Self, IBaseHistory, OwnHist)
          then
            begin
              with Hist.GetHistory.DataSet as IBaseManageDB do
                UpdateStdFields := False;
              with Hist.GetHistory do
                begin
                  while not EOF do
                    begin
                      Edit;
                      FieldByName('REF_ID').AsVariant:=Self.Id.AsVariant;
                      Post;
                      Next;
                    end;
                end;
            end;
          //Combine Documents
          aDoc := TDocuments.CreateEx(nil,DataModule);
          aDoc.SelectByReference(aObject.Id.AsVariant);
          aDoc.Open;
          while not aDoc.EOF do
            begin
              aDoc.Edit;
              aDoc.FieldByName('REF_ID').AsVariant:=Self.Id.AsVariant;
              aDoc.Post;
              aDoc.Next;
            end;
          aDoc.Free;
          //Combine SyncItems
          aSync:= TSyncItems.CreateEx(nil,DataModule);
          aSync.SelectByReference(aObject.Id.AsVariant);
          aSync.Open;
          while not aSync.EOF do
            begin
              aSync.Edit;
              aSync.FieldByName('LOCAL_ID').AsVariant:=Self.Id.AsVariant;
              aSync.Post;
              aSync.Next;
            end;
          aSync.Free;
        end;
      aObject.Delete;
      aObject.Free;
    end;
end;

procedure TTextTypes.DefineFields(aDataSet: TDataSet);
begin
  with aDataSet as IBaseManageDB do
    begin
      TableName := 'TEXTTYP';
      TableCaption:=strTexttypes;
      if Assigned(ManagedFieldDefs) then
        with ManagedFieldDefs do
          begin
            Add('NAME',ftString,20,True);
            Add('TYP',ftInteger,0,False);
          end;
    end;
end;
procedure TRepairProblems.DefineFields(aDataSet: TDataSet);
begin
  with aDataSet as IBaseManageDB do
    begin
      TableName := 'REPAIRPROBLEMS';
      TableCaption:=strRepairProblems;
      if Assigned(ManagedFieldDefs) then
        with ManagedFieldDefs do
          begin
            Add('PROBLEM',ftString,60,True);
          end;
    end;
end;
procedure TPriceTypes.DefineFields(aDataSet: TDataSet);
begin
  with aDataSet as IBaseManageDB do
    begin
      TableName := 'PRICETYPES';
      TableCaption:=strPriceTypes;
      if Assigned(ManagedFieldDefs) then
        with ManagedFieldDefs do
          begin
            Add('TYPE',ftString,1,True);
            Add('SYMBOL',ftString,4,True);
            Add('NAME',ftString,40,True);
          end;
    end;
end;
function TPriceTypes.Get(aSymbol: string): Integer;
begin
  Result := 0;
  Open;
  if DataSet.Locate('SYMBOL', trim(aSymbol), []) then
    Result := StrToIntDef(copy(DataSet.FieldByName('TYPE').AsString, 0, 2), 0);
end;

constructor TPositionCalc.CreateEx(aOwner: TComponent; DM: TComponent;
  aConnection: TComponent; aMasterdata: TDataSet);
begin
  inherited CreateEx(aOwner, DM, aConnection, aMasterdata);
  UpdateFloatFields:=True;
end;

procedure TPositionCalc.DefineFields(aDataSet: TDataSet);
begin
  with aDataSet as IBaseManageDB do
    begin
      TableName := 'ORDERPOSCALC';
      TableCaption:=strPositionCalc;
      if Assigned(ManagedFieldDefs) then
        with ManagedFieldDefs do
          begin
            Add('REF_ID',ftLargeint,0,True);
            Add('TYPE',ftString,4,True);
            Add('PRICE',ftFloat,0,False);
            Add('MINCOUNT',ftFloat,0,False);
            Add('MAXCOUNT',ftFloat,0,False);
            Add('CUSTOMER',ftString,20,False);
          end;
    end;
end;
procedure TDispatchTypes.DefineFields(aDataSet: TDataSet);
begin
  with aDataSet as IBaseManageDB do
    begin
      TableName := 'DISPATCHTYPES';
      TableCaption:=strDispatchtyoes;
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
procedure TUnits.DefineFields(aDataSet: TDataSet);
begin
  with aDataSet as IBaseManageDB do
    begin
      TableName := 'UNITS';
      if Assigned(ManagedFieldDefs) then
        with ManagedFieldDefs do
          begin
            Add('NAME',ftString,160,false);
          end;
    end;
end;
procedure TStates.DefineFields(aDataSet: TDataSet);
begin
  with aDataSet as IBaseManageDB do
    begin
      TableName := 'STATES';
      if Assigned(ManagedFieldDefs) then
        with ManagedFieldDefs do
          begin
            Add('TYPE',ftString,1,false);
            Add('STATUS',ftString,4,True);
            Add('STATUSNAME',ftString,30,True);
            Add('DERIVATIVE',ftString,30,false);
            Add('ACTIVE',ftString,1,false);
            Add('COLOR',ftString,8,false);
            Add('ICON',ftInteger,0,false);
          end;
      if Assigned(ManagedIndexdefs) then
        with ManagedIndexDefs do
          begin
            Add('TYPE','TYPE',[]);
            Add('STATUS','STATUS',[]);
          end;
    end;
end;
procedure TVat.DefineFields(aDataSet: TDataSet);
begin
  with aDataSet as IBaseManageDB do
    begin
      TableName := 'VAT';
      if Assigned(ManagedFieldDefs) then
        with ManagedFieldDefs do
          begin
            Add('ID',ftInteger,0,True);
            Add('VALUE',ftFloat,0,false);
            Add('NAME',ftString,50,false);
            Add('FKEY',ftString,10,false); //Fibu Schlüssel (Financial accounting key ??)
          end;
    end;
end;
procedure TLanguages.DefineFields(aDataSet: TDataSet);
begin
  with aDataSet as IBaseManageDB do
    begin
      TableName := 'LANGUAGES';
      TableCaption:=strLanguages;
      if Assigned(ManagedFieldDefs) then
        with ManagedFieldDefs do
          begin
            Add('LANGUAGE',ftString,30,True);
            Add('ISO6391',ftString,2,False);
            Add('ISO6392',ftString,3,False);
            Add('DATEFORMAT',ftString,20,true);
            Add('TIMEFORMAT',ftString,8,true);
            Add('STDCURR',ftString,3,true);
            Add('DECSEP',ftString,1,False);
            Add('THOUSEP',ftString,1,False);
            Add('SATUTATION',ftMemo,0,False);
            Add('COMPCLOSE',ftMemo,0,False);
            Add('TITLES',ftMemo,0,False);
            Add('COUNTRY',ftString,30,False);
            Add('DEFAULTLNG',ftString,1,False);
          end;
    end;
end;
procedure TCountries.DefineFields(aDataSet: TDataSet);
begin
  with aDataSet as IBaseManageDB do
    begin
      TableName := 'COUNTRIES';
      if Assigned(ManagedFieldDefs) then
        with ManagedFieldDefs do
          begin
            Add('ID',ftString,3,True);
            Add('NAME',ftString,30,True);
            Add('LANGUAGE',ftString,2,False);
          end;
    end;
end;
procedure TStorageJournal.DefineFields(aDataSet: TDataSet);
begin
  with aDataSet as IBaseManageDB do
    begin
      TableName := 'STORAGEJOURNAL';
      if Assigned(ManagedFieldDefs) then
        with ManagedFieldDefs do
          begin
            Add('STORAGEID',ftString,3,True);
            Add('ORDERNO',ftInteger,0,False);
            Add('OSTATUS',ftString,3,False);
            Add('POSNO',ftInteger,0,False);
            Add('TYPE',ftString,1,False);
            Add('ID',ftString,20,False);
            Add('VERSION',ftString,25,False);
            Add('LANGUAGE',ftString,3,False);
            Add('SERIAL',ftString,30,False);
            Add('NOTE',ftString,500,False);
            Add('QUANTITY',ftFloat,0,False);
            Add('QUANTITYU',ftString,10,False);
          end;
    end;
end;
procedure TStorageTyp.DefineFields(aDataSet: TDataSet);
begin
  with aDataSet as IBaseManageDB do
    begin
      TableName := 'STORAGETYPE';
      if Assigned(ManagedFieldDefs) then
        with ManagedFieldDefs do
          begin
            Add('ID',ftString,3,True);
            Add('NAME',ftString,30,True);
            Add('DEFAULTST',ftString,1,True);
          end;
    end;
end;
procedure TPositionTyp.DefineFields(aDataSet: TDataSet);
begin
  with aDataSet as IBaseManageDB do
    begin
      TableName := 'ORDERPOSTYP';
      TableCaption:=strPositionType;
      if Assigned(ManagedFieldDefs) then
        with ManagedFieldDefs do
          begin
            Add('NAME',ftString,3,True);
            Add('TYPE',ftString,1,True);
            Add('TEXT',ftString,60,False);
          end;
    end;
end;

procedure TBaseDBPosition.DataSetAfterCancel(aDataSet: TDataSet);
begin
  if FDoNumber then
    FOldPosNo:=PosNo.AsInteger;
end;

procedure TBaseDBPosition.DataSetAfterDelete(aDataSet: TDataSet);
var
  PosPrice: Double;
  GrossPrice: Double;
begin
  DoDelete;
  if (OldGrossPrice <> 0)
  or (OldPosPrice <> 0) then
    begin
      PosPrice := (-OldPosPrice);
      GrossPrice := (-OldGrossPrice);
      PosPriceChanged(PosPrice,GrossPrice);
    end;
  if OldPosWeight <> 0 then
    begin
      PosPrice := (-OldPosWeight);
      PosWeightChanged(PosPrice);
    end;
end;
procedure TBaseDBPosition.DataSetAfterPost(aDataSet: TDataSet);
var
  PosPrice: Extended;
  GrossPrice: Extended;
begin
  if IsCalculationDisabled then exit;
  DoPost;
  if (OldGrossPrice <> DataSet.FieldByName('GROSSPRICE').AsFloat)
  or (OldPosPrice <> DataSet.FieldByName('POSPRICE').AsFloat) then
    begin
      PosPrice := (DataSet.FieldByName('POSPRICE').AsFloat-OldPosPrice);
      GrossPrice := (DataSet.FieldByName('GROSSPRICE').AsFloat-OldGrossPrice);
      PosPriceChanged(PosPrice,GrossPrice);
    end;
  if (OldPosWeight <> DataSet.FieldByName('WEIGHT').AsFloat*DataSet.FieldByName('QUANTITY').AsFloat) then
    begin
      PosPrice := ((DataSet.FieldByName('WEIGHT').AsFloat*DataSet.FieldByName('QUANTITY').AsFloat)-OldPosWeight);
      PosWeightChanged(PosPrice);
    end;
end;

procedure TBaseDBPosition.DataSetBeforeCancel(aDataSet: TDataSet);
begin
  FDoNumber := DataSet.State=dsInsert;
end;

procedure TBaseDBPosition.DataSetBeforeDelete(aDataSet: TDataSet);
begin
  OldPosPrice := DataSet.FieldByName('POSPRICE').AsFloat;
  OldGrossPrice := DataSet.FieldByName('GROSSPRICE').AsFloat;
  OldPosWeight := DataSet.FieldByName('WEIGHT').AsFloat*DataSet.FieldByName('QUANTITY').AsFloat;
end;
procedure TBaseDBPosition.FIntDataSourceDataChange(Sender: TObject;
  Field: TField);
var
  tmp: Extended;
begin
  if not Assigned(Field) then exit;
  if FCalculationDisabled > 0 then exit;
  DoDataChange(Sender,Field);
  if (Field.FieldName = 'DISCOUNT')
  or (Field.FieldName = 'IS_OPTION')
  or (Field.FieldName = 'IS_ALTERNA')
  or (Field.FieldName = 'POSPRICE') then
    DoModifyPosPrice
  else
  if (Field.FieldName = 'QUANTITY') then
    begin
      DoCalcPosPrice;
    end
  else if (Field.FieldName = 'POSTYP') then
    begin
      DoModifyPosPrice;
    end
  else if (Field.FieldName = 'SELLPRICE') then
    begin
      DoCalcPosPrice(True);
    end
  else if (Field.FieldName = 'GROSSPRICE') then
    begin
      DoCalcGrossPosPrice;
    end
  else if (Field.FieldName = 'VAT') then
    begin
      DoModifyPosPrice;
    end
  ;
end;
procedure TBaseDBPosition.FIntDataSourceStateChange(Sender: TObject);
begin
  if FIntDataSource.State=dsInsert then
    begin
      OldPosPrice := 0;
      OldGrossPrice := 0;
      OldPosWeight := 0;
      DoInsert;
    end
  else if FIntDataSource.State=dsEdit then
    begin
      OldPosPrice := DataSet.FieldByName('POSPRICE').AsFloat;
      OldGrossPrice := DataSet.FieldByName('GROSSPRICE').AsFloat;
      OldPosWeight := DataSet.FieldByName('WEIGHT').AsFloat*DataSet.FieldByName('QUANTITY').AsFloat;
      DoEdit;
    end;
end;
function TBaseDBPosition.GetIdent: TField;
begin
  Result := DataSet.FieldByName('IDENT');
end;
function TBaseDBPosition.GetPosNo: TField;
begin
  Result := FieldByName('POSNO');
end;
function TBaseDBPosition.GetPosTyp: TPositionTyp;
begin
  Result := nil;
  FPosTyp.Open;
  FPosTyp.DataSet.Locate('NAME',Self.FieldByName('POSTYP').AsVariant,[loCaseInsensitive]);
  Result := FPosTyp;
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
  if (not DataSet.FieldByName('QUANTITY').IsNull) and (not TryStrToFloat(DataSet.FieldByName('QUANTITY').AsString,tmp)) then exit;
  if (not DataSet.FieldByName('SELLPRICE').IsNull) and (not TryStrToFloat(DataSet.FieldByName('SELLPRICE').AsString,tmp)) then exit;
  if (not DataSet.FieldByName('COMPRICE').IsNull) and (not TryStrToFloat(DataSet.FieldByName('COMPRICE').AsString,tmp)) then exit;
  if (not DataSet.FieldByName('DISCOUNT').IsNull) and (not TryStrToFloat(DataSet.FieldByName('DISCOUNT').AsString,tmp)) then exit;
  DisableCalculation;
  try
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
  DataSet.DisableControls;
  PosCalc.DataSet.DisableControls;
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
end;
procedure TBaseDBPosition.DoCalcGrossPosPrice;
var
  tmp: Extended = 0;
begin
  if FCalculationDisabled > 0 then exit;
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
end;

function TBaseDBPosition.GetShorttext: TField;
begin
  Result := FieldByName('SHORTTEXT');
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
constructor TBaseDBPosition.CreateEx(aOwner: TComponent; DM : TComponent;aConnection: TComponent;
  aMasterdata: TDataSet);
begin
  inherited CreateEx(aOwner, DM,aConnection, aMasterdata);
  FUseRTF:=False;
  UpdateFloatFields:=True;
  FPosFormat := '%d';
  FPosTyp := TPositionTyp.CreateEx(Owner,DataModule,aConnection);
  FImages := TImages.CreateEx(Self,DataModule,aConnection,DataSet);
  FImages.ActualFilter := TBaseDBModule(DataModule).QuoteField('REF_ID')+'='+':'+TBaseDBModule(DataModule).QuoteField('IMAGEREF');
  with BaseApplication as IBaseDbInterface do
    begin
      with DataSet as IBaseDBFilter do
        begin
          Limit := 0;
          SortFields:='POSNO';
          SortDirection:=sdAscending;
        end;
    end;
  FPosCalc := TPositionCalc.CreateExIntegrity(Self,DataModule,False,aConnection,DataSet);
  FIntDataSource := TDataSource.Create(Self);
  FIntDataSource.DataSet := DataSet;
  FIntDataSource.OnDataChange:=@FIntDataSourceDataChange;
  FIntDataSource.OnStateChange:=@FIntDataSourceStateChange;
  DataSet.AfterPost:=@DataSetAfterPost;
  DataSet.BeforeDelete:=@DataSetBeforeDelete;
  DataSet.AfterDelete:=@DataSetAfterDelete;
  DataSet.AfterCancel:=@DataSetAfterCancel;
  DataSet.BeforeCancel:=@DataSetBeforeCancel;
  FVat := TVat.CreateEx(Self,DataModule,Connection);
  PriceTypes := TPriceTypes.CreateEx(Self,DataModule,Connection);
end;
destructor TBaseDBPosition.Destroy;
begin
  PriceTypes.Destroy;
  Vat.Destroy;
  FPosCalc.Destroy;
  FPosTyp.Destroy;
  FIntDataSource.Destroy;
  inherited Destroy;
end;
function TBaseDBPosition.CreateTable : Boolean;
var
  aPosCalc: TPositionCalc;
begin
  Result := inherited CreateTable;
  with FPosCalc.DataSet as IBaseManageDB do
    with BaseApplication as IBaseDbInterface do
      if not Data.TableExists(TableName) then
        begin
          aPosCalc := TPositionCalc.CreateExIntegrity(Self,DataModule,False,Connection);
          aPosCalc.CreateTable;
          aPosCalc.Free;
        end;
end;
procedure TBaseDBPosition.Open;
var
  aRec: LargeInt;
begin
  inherited Open;
  FPosTyp.Open;
  aRec := GetBookmark;
  Last;
  FOldPosNo := PosNo.AsInteger;
  GotoBookmark(aRec);
end;
procedure TBaseDBPosition.DefineFields(aDataSet: TDataSet);
begin
  with aDataSet as IBaseManageDB do
    begin
      TableCaption:=strPositions;
      if Assigned(ManagedFieldDefs) then
        with ManagedFieldDefs do
          begin
            Add('POSNO',ftInteger,0,True);
            Add('POSTYP',ftString,3,False);
            Add('ACTIVE',ftString,1,False);
            Add('TPOSNO',ftString,15,False);                //Auschreibungsnummer
            Add('IDENT',ftString,40,False);
            Add('VERSION',ftString,25,False);
            Add('LANGUAGE',ftString,3,False);
            Add('TEXTTYPE',ftString,1,False);
            Add('SHORTTEXT',ftString,200,False);
            Add('TEXT',ftMemo,0,False);
            Add('STORAGE',ftString,3,False);                //Lagerentname
            Add('SERIAL',ftString,20,False);                //Serienummer
            Add('MANUFACNR',ftString,40,False);
            Add('WEIGHT',ftFloat,0,False);
            Add('AVALIBLE',ftFloat,0,False);                //verfügbar
            Add('DELIVERY',ftDate,0,False);                 //wann verfügbar
            Add('QUANTITY',ftFloat,0,False);                //Menge
            Add('QUANTITYD',ftFloat,0,False);               //Menge Geliefert
            Add('QUANTITYC',ftFloat,0,False);               //Menge berechnet
            Add('QUANTITYO',ftFloat,0,False);               //Auftragsmenge
            Add('QUANTITYU',ftString,10,False);             //Mengeneinheit
            Add('SETUPTIME',ftFloat,0,False);               //Rüstzeit
            Add('PLANTIME',ftFloat,0,False);                //geplante Zeit
            Add('TIME',ftFloat,0,False);                    //benötigte Zeit
            Add('BUFFERTIME',ftFloat,0,False);              //Wartezeit (wann darf nächste Aufgabe frühestens starten)
            Add('STARTDATE',ftDateTime,0,False);
            Add('DUEDATE',ftDateTime,0,False);
            Add('EARLIEST',ftDateTime,0,False);
            Add('LATEST',ftDateTime,0,False);

            Add('PURCHASE',ftFloat,0,False);                //Einkaufspreis
            Add('SELLPRICE',ftFloat,0,False);               //Verkaufspreis
            Add('COMPRICE',ftFloat,0,False);                //Common Price
            Add('DISCOUNT',ftFloat,0,False);                //Rabatt
            Add('VAT',ftSmallInt,0,False);                  //MwSt Typ
            Add('REPAIRTIME',ftFloat,0,False);              //reparaturzeit
            Add('POSPRICE',ftFloat,0,False);                //Gesamtpreis
            Add('GROSSPRICE',ftFloat,0,False);              //Bruttoprice
            Add('IMAGEREF',ftLargeInt,0,False);
            Add('PARENT',ftLargeInt,0,False);
            Add('OLD_ID',ftLargeInt,0,False);
            Add('SCRIPT',ftString,60,False);
            Add('SCRIPTVER',ftString,8,False);
            Add('SCRIPTFUNC',ftString,60,False);
            Add('PRSCRIPT',ftString,60,False);
            Add('PRSCRIPTVER',ftString,8,False);
            Add('PRSCRIPTFUNC',ftString,160,False);
            Add('PREPTEXT',ftString,100,False);
            Add('WORKTEXT',ftString,100,False);
            Add('CHANGEDBY',ftString,4,False);
            Add('CREATEDBY',ftString,4,False);
          end;
      if Assigned(ManagedIndexdefs) then
        with ManagedIndexDefs do
          begin
            Add('IDENT','IDENT',[]);
            Add('POSNO','POSNO',[]);
            Add('SHORTTEXT','SHORTTEXT',[]);
            Add('SERIAL','SERIAL',[]);
          end;
      //UpdateChangedBy:=False;
    end;
end;
procedure TBaseDBPosition.FillDefaults(aDataSet: TDataSet);
begin
  DisableCalculation;
  with aDataSet,BaseApplication as IBaseDbInterface do
    begin
//      if FieldByName('POSTYP').Required then
      FieldByName('POSTYP').AsString   := PosTyp.FieldByName('NAME').AsString;
      PosNo.AsInteger := (FOldPosNo+1);
      FOldPosNo:=PosNo.AsInteger;
    //      FieldByName('TPOSNO').AsString   := fOrders.TPosNumber;
      FieldByName('TEXTTYPE').AsString := '0';
      FieldByName('VAT').AsString      := '1';
      FieldByName('QUANTITY').AsFloat  := 1;
    end;
  EnableCalculation;
end;
procedure TBaseDBPosition.Assign(Source: TPersistent);
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

begin
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
end;

procedure TBaseDBPosition.DirectAssign(Source: TPersistent);
begin
  DisableCalculation;
  inherited DirectAssign(Source);
  EnableCalculation;
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
end;

procedure TPaymentTargets.DefineFields(aDataSet: TDataSet);
begin
  with aDataSet as IBaseManageDB do
    begin
      TableName := 'PAYMENTTARGETS';
      TableCaption:=strPaymentTargets;
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
procedure TCurrency.DefineFields(aDataSet: TDataSet);
begin
  with aDataSet as IBaseManageDB do
    begin
      TableName := 'CURRENCY';
      TableCaption:=strCurrency;
      if Assigned(ManagedFieldDefs) then
        with ManagedFieldDefs do
          begin
            Add('SYMBOL',ftString,5,True);
            Add('NAME',ftString,40,True);
            Add('MASK',ftString,20,True);
            Add('DECIMALPL',ftSmallInt,0,True);
            Add('FACTOR',ftFloat,0,True);
            Add('DEFAULTCUR',ftString,1,False);
            Add('ROUNDGRAN',ftFloat,0,False);
          end;
    end;
end;
function TCurrency.Convert(Price: real; SourceCurr, TargetCurr: string): real;
var
  tmp: real;
begin
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
end;
procedure TStorageTypes.DefineFields(aDataSet: TDataSet);
begin
  with aDataSet as IBaseManageDB do
    begin
      TableName := 'STORAGETYPE';
      TableCaption:=strStorageTypes;
      if Assigned(ManagedFieldDefs) then
        with ManagedFieldDefs do
          begin
            Add('ID',ftString,3,True);
            Add('NAME',ftString,30,True);
            Add('DEFAULTST',ftString,1,True);
          end;
    end;
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

