{*******************************************************************************
Dieser Sourcecode darf nicht ohne gültige Geheimhaltungsvereinbarung benutzt werden
und ohne gültigen Vertriebspartnervertrag weitergegeben werden.
You have no permission to use this Source without valid NDA
and copy it without valid distribution partner agreement
Christian Ulrich
info@cu-tec.de
Created 01.06.2006
*******************************************************************************}
unit uAccounting;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, db, uBaseDBClasses, uBaseApplication;
type
  TAccountExchange = class(TBaseDBDataSet)
  public
    procedure DefineFields(aDataSet : TDataSet);override;
    procedure Open;override;
  end;
  TAccounts = class(TBaseDBDataSet)
  private
    FExchange: TAccountExchange;
  public
    constructor Create(aOwner : TComponent;DM : TComponent;aConnection : TComponent = nil;aMasterdata : TDataSet = nil);override;
    procedure Open;override;
    destructor Destroy;override;
    procedure DefineFields(aDataSet : TDataSet);override;
    function CreateTable : Boolean;override;
    property Exchange : TAccountExchange read FExchange;
  end;
  TAccountingJournal = class(TBaseDBDataSet)
  private
    FirstOpen: Boolean;
  public
    constructor Create(aOwner : TComponent;DM : TComponent;aConnection : TComponent = nil;aMasterdata : TDataSet = nil);override;
    procedure DefineFields(aDataSet : TDataSet);override;
    procedure Open;override;
  end;
implementation
uses uBaseDBInterface;

constructor TAccountingJournal.Create(aOwner: TComponent; DM: TComponent;
  aConnection: TComponent; aMasterdata: TDataSet);
begin
  FirstOpen := True;
  inherited Create(aOwner, DM, aConnection, aMasterdata);
  UpdateFloatFields:=True;
end;

procedure TAccountingJournal.DefineFields(aDataSet: TDataSet);
begin
  with aDataSet as IBaseManageDB do
    begin
      TableName := 'ACCOUNTINGJOURNAL';
      if Assigned(ManagedFieldDefs) then
        with ManagedFieldDefs do
          begin
            Add('TYPE',ftString,2,True);
            Add('ORDERNO',ftInteger,0,True);
            Add('STATUS',ftString,4,True);
            Add('DATE',ftDate,0,False);
            Add('ODATE',ftDate,0,False);
            Add('NUMBER',ftString,20,False);
            Add('CUSTNO',ftString,20,False);
            Add('CUSTNAME',ftString,200,False);
            Add('CURRENCY',ftString,5,False);
            Add('DUNNINGLVL',ftInteger,0,False);
            Add('PAYMENTTAR',ftString,2,False);
            Add('VATT',ftInteger,0,True);                   //MwSt Satz
            Add('NETPRICE',ftFloat,0,False);                //Nettopreis
            Add('GROSSPRICE',ftFloat,0,False);              //Bruttoprice
            Add('PAYPRICE',ftFloat,0,False);                //bereits bezahlt
            Add('PAYMENT',ftString,1,True);
            Add('PAYEDON',ftDate,0,False);
          end;
    end;
end;
procedure TAccountingJournal.Open;
begin
  if FirstOpen then
    begin
      with  DataSet as IBaseDBFilter, BaseApplication as IBaseDBInterface, DataSet as IBaseManageDB do
        begin
          SortDirection := sdDescending;
          BaseSortFields := 'ODATE';
          SortFields := 'ODATE';
          Limit := 100;
        end;
      FirstOpen := False;
    end;
  inherited Open;
end;
procedure TAccountExchange.DefineFields(aDataSet: TDataSet);
begin
  UpdateFloatFields:=True;
  with aDataSet as IBaseManageDB do
    begin
      TableName := 'ACCOUNTEXCHANGE';
      if Assigned(ManagedFieldDefs) then
        with ManagedFieldDefs do
          begin
            Add('TYPE',ftString,1,True);
            Add('RSORTCODE',ftString,12,False);
            Add('RACCOUNTNO',ftString,40,True);
            Add('VALUE',ftFloat,0,True);
            Add('CURRENCY',ftString,5,False);
            Add('BALLANCE',ftFloat,0,False);
            Add('VALUEDATE',ftDateTime,0,False);
            Add('DATE',ftDate,0,False);
            Add('NAME',ftString,150,False);
            Add('PURPOSE',ftMemo,0,False);
            Add('CHECKED',ftString,1,False);
            Add('VOUCHER',ftString,60,False);
            Add('CATEGORY',ftString,60,False);
            Add('CHECKSUM',ftString,35,False);
          end;
    end;
end;

procedure TAccountExchange.Open;
begin
  with  DataSet as IBaseDBFilter, BaseApplication as IBaseDBInterface, DataSet as IBaseManageDB do
    begin
      SortFields := 'VALUEDATE,SQL_ID';
      SortDirection := sdDescending;
    end;
  inherited Open;
end;
constructor TAccounts.Create(aOwner: TComponent; DM: TComponent;
  aConnection: TComponent; aMasterdata: TDataSet);
begin
  inherited Create(aOwner, DM, aConnection, aMasterdata);
  FExchange := TAccountExchange.Create(Owner,DM,aConnection,DataSet);
end;
procedure TAccounts.Open;
begin
  inherited Open;
  FExchange.Open;
end;
destructor TAccounts.Destroy;
begin
  inherited Destroy;
  Exchange.Free;
end;
procedure TAccounts.DefineFields(aDataSet: TDataSet);
begin
  with aDataSet as IBaseManageDB do
    begin
      TableName := 'ACCOUNTS';
      if Assigned(ManagedFieldDefs) then
        with ManagedFieldDefs do
          begin
            Add('TYPE',ftString,3,True);
            Add('SORTCODE',ftString,12,True);
            Add('ACCOUNTNO',ftString,40,True);
            Add('NAME',ftString,30,False);
            Add('NOTES',ftMemo,0,False);
            Add('FTSNAME',ftString,30,False);
          end;
    end;
end;
function TAccounts.CreateTable : Boolean;
begin
  Result := inherited CreateTable;
  FExchange.CreateTable;
end;

end.

