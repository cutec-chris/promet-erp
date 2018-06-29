unit ucreatetable;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, fpcunit, testutils, testregistry, uData, uBaseDbClasses,db,
  uBaseDBInterface,uBaseDatasetInterfaces;
type
  TSubTable = class(TBaseDBDataSet)
    procedure DefineFields(aDataSet : TDataSet);override;
  end;

  { TTable }

  TTable = class(TBaseDBDataSet)
  private
    FSubTable: TSubTable;
  published
    procedure DefineFields(aDataSet : TDataSet);override;
    constructor CreateEx(aOwner: TComponent; DM: TComponent=nil;
      aConnection: TComponent=nil; aMasterdata: TDataSet=nil); override;
    destructor Destroy;override;
    property SubTable : TSubTable read FSubTable;
  end;

  TableCreate= class(TTestCase)
  private //not executed tests
    procedure OpenTableInTransaction;
    procedure OpenSubTableInTransaction;
    procedure CloseSubTableInTransaction;
  published
    procedure CheckForTable;
    procedure CreateTable;
    procedure OpenTable;
    procedure CloseTable;
    procedure DeleteTable;

    procedure OpenTransaction;
    procedure CreateTableInTransaction;
    procedure DeleteTableInTransaction;
    procedure CloseTableInTransaction;
    procedure CloseTransaction;
  end;

implementation
var
  aTable : TTable;
  aTransaction : TComponent;

procedure TSubTable.DefineFields(aDataSet: TDataSet);
begin
  with aDataSet as IBaseManageDB do
    begin
      TableName := 'TESTSUBTABLE';
      if Assigned(ManagedFieldDefs) then
        with ManagedFieldDefs do
          begin
            Add('TESTFIELD',ftString,25,True);
          end;
    end;
end;
procedure TTable.DefineFields(aDataSet: TDataSet);
begin
  with aDataSet as IBaseManageDB do
    begin
      TableName := 'TESTTABLE';
      if Assigned(ManagedFieldDefs) then
        with ManagedFieldDefs do
          begin
            Add('TESTFIELD',ftString,25,True);
          end;
    end;
end;
constructor TTable.CreateEx(aOwner: TComponent; DM: TComponent;
  aConnection: TComponent; aMasterdata: TDataSet);
begin
  inherited CreateEx(aOwner, DM, aConnection, aMasterdata);
  FSubTable := TSubTable.CreateEx(aOwner, DM, aConnection, DataSet);
end;
destructor TTable.Destroy;
begin
  FSubTable.Destroy;
  inherited Destroy;
end;
procedure TableCreate.CheckForTable;
begin
  Check(Data.TableExists('TESTTABLE') = False,'Table already exists !');
end;
procedure TableCreate.CreateTable;
begin
  aTable := TTable.Create(nil);
  aTable.CreateTable;
end;
procedure TableCreate.OpenTable;
begin
  aTable.Open;
  with aTable.DataSet do
    begin
      Append;
      FieldByName('TESTFIELD').AsString := 'test';
      Post;
    end;
end;
procedure TableCreate.CloseTable;
begin
  aTable.Close;
  aTable.Destroy;
end;
procedure TableCreate.DeleteTable;
begin
  Data.ExecuteDirect('DROP TABLE '+Data.QuoteField('TESTTABLE'));
  Data.Tables.Clear;
end;
procedure TableCreate.OpenTransaction;
begin
  aTransaction := Data.GetNewConnection;
end;
procedure TableCreate.CreateTableInTransaction;
begin
  aTable := TTable.CreateEx(nil,Data,aTransaction);
  aTable.CreateTable;
  aTable.SubTable.CreateTable;
end;
procedure TableCreate.OpenTableInTransaction;
begin
  aTable.Select(0);
  aTable.Open;
  with aTable.DataSet do
    begin
      Append;
      FieldByName('TESTFIELD').AsString := 'test';
      Post;
    end;
end;
procedure TableCreate.OpenSubTableInTransaction;
begin
  aTable.SubTable.Open;
end;
procedure TableCreate.CloseSubTableInTransaction;
begin
end;
procedure TableCreate.CloseTableInTransaction;
begin
  aTable.Destroy;
end;
procedure TableCreate.CloseTransaction;
begin
  Data.RollbackTransaction(aTransaction);
  aTransaction.Destroy;
end;
procedure TableCreate.DeleteTableInTransaction;
begin
  try
    Data.ExecuteDirect('DROP TABLE '+Data.QuoteField('TESTSUBTABLE'));
    Data.ExecuteDirect('DROP TABLE '+Data.QuoteField('TESTTABLE'));
  except
  end;
end;
initialization
  RegisterTest(TableCreate);
end.

