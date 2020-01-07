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
unit uBaseDatasetInterfaces;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DB, mORMot, SynCommons, mORMotDB, SynDB;

type
  TSortDirection = (sdAscending, sdDescending, sdIgnored);

  { IBaseDbFilter }

  IBaseDbFilter = interface['{7EBB7ABE-1171-4333-A609-C0F59B1E2C5F}']
    function GetBaseSortDirection: TSortDirection;
    function GetfetchRows: Integer;
    function GetParameterValue(const Name: string): Variant;
    function GetUseBaseSorting: Boolean;
    procedure SetBaseSortDirection(AValue: TSortDirection);
    function GetBaseSorting: string;
    procedure SetBaseSorting(AValue: string);
    procedure SetBaseSortFields(const AValue: string);
    function GetBaseSortFields: string;
    function GetFields: string;
    procedure SetfetchRows(AValue: Integer);
    procedure SetFields(const AValue: string);
    function GetSQL: string;
    procedure SetParameterValue(const Name: string; AValue: Variant);
    procedure SetSQL(const AValue: string);
    function GetFilter: string;
    function GetIntFilter: string;
    procedure SetFilter(const AValue: string);
    function GetBaseFilter: string;
    procedure SetBaseFilter(const AValue: string);
    function GetFilterTables: string;
    function GetLimit: Integer;
    function GetSortDirection: TSortDirection;
    function GetSortFields: string;
    function GetLocalSortFields: string;
    function GetSortLocal: Boolean;
    procedure SetSortLocal(const AValue: Boolean);
    procedure SetFilterTables(const AValue: string);
    procedure Setlimit(const AValue: Integer);
    procedure SetSortDirection(const AValue: TSortDirection);
    procedure SetSortFields(const AValue: string);
    procedure SetLocalSortFields(const AValue: string);
    function GetUsePermissions: Boolean;
    procedure SetUseBaseSorting(AValue: Boolean);
    procedure SetUsePermisions(const AValue: Boolean);
    function GetDistinct: Boolean;
    procedure SetDistinct(const AValue: Boolean);
    procedure DoExecSQL;
    function NumRowsAffected : Integer;

    property FullSQL : string read GetSQL write SetSQL;
    property Filter : string read GetFilter write SetFilter;
    property FetchRows : Integer read GetfetchRows write SetfetchRows;
    property BaseFilter : string read GetBaseFilter write SetBaseFilter;
    property Limit : Integer read GetLimit write Setlimit;
    property Fields : string read GetFields write SetFields;
    property SortFields : string read GetSortFields write SetSortFields;
    property LocalSortFields : string read GetLocalSortFields write SetLocalSortFields;
    property BaseSortFields : string read GetBaseSortFields write SetBaseSortFields;
    property BaseSorting : string read GetBaseSorting write SetBaseSorting;
    property UseBaseSorting : Boolean read GetUseBaseSorting write SetUseBaseSorting;
    property BaseSortDirection : TSortDirection read GetBaseSortDirection write SetBaseSortDirection;
    property SortDirection : TSortDirection read GetSortDirection write SetSortDirection;
    property Distinct : Boolean read GetDistinct write SetDistinct;
    property SortLocal : Boolean read GetSortLocal write SetSortLocal;
    property FilterTables : string read GetFilterTables write SetFilterTables;
    property UsePermissions : Boolean read GetUsePermissions write SetUsePermisions;
    property Parameter[const Name: string]: Variant read GetParameterValue write SetParameterValue;
  end;

  { IBaseManageDB }

  IBaseManageDB = interface['{271BD4A2-2720-49DA-90A6-AA64FB2B9862}']
  function GetAsReadonly: Boolean;
    function GetConnection: TComponent;
    function GetDataSource: TDataSource;
    function GetMasterdataSource: TDataSource;
    procedure SetConnection(aConn : TComponent);
    function GetManagedFieldDefs: TFieldDefs;
    function GetManagedIndexDefs: TIndexDefs;
    function GetTableCaption: string;
    function GetTableName: string;
    function GetTableNames: string;
    function GetUpChangedBy: Boolean;
    function GetUpStdFields: Boolean;
    function GetUseIntegrity: Boolean;
    procedure SetAsReadOnly(AValue: Boolean);
    procedure SetDataSource(AValue: TDataSource);
    procedure SetMasterdataSource(AValue: TDataSource);
    procedure SetUpChangedBy(AValue: Boolean);
    procedure SetUpStdFields(AValue: Boolean);
    procedure SetTableCaption(const AValue: string);
    procedure SetTableName(const AValue: string);
    procedure SetTableNames(const AValue: string);
    procedure SetOrigTable(AValue: TComponent);
    function GetOrigTable : TComponent;
    procedure SetUseIntegrity(AValue: Boolean);
    property ManagedFieldDefs : TFieldDefs read GetManagedFieldDefs;
    property ManagedIndexDefs : TIndexDefs read GetManagedIndexDefs;
    property TableName : string read GetTableName write SetTableName;
    property TableCaption : string read GetTableCaption write SetTableCaption;
    property UseIntegrity : Boolean read GetUseIntegrity write SetUseIntegrity;
    property UpdateStdFields : Boolean read GetUpStdFields write SetUpStdFields;
    property UpdateChangedBy : Boolean read GetUpChangedBy write SetUpChangedBy;
    property DBConnection : TComponent read GetConnection write SetConnection;
    property AsReadOnly : Boolean read GetAsReadonly write SetAsReadOnly;
    property MasterSource : TDataSource read GetMasterdataSource write SetMasterdataSource;
    property DataSource : TDataSource read GetDataSource write SetDataSource;
  end;

  { IBaseSubDataSets }

  IBaseSubDataSets = interface['{CB011ABE-E465-4BD4-AA49-D3A8852AA012}']
    function GetSubDataSet(aName : string): TComponent;
    function GetCount : Integer;
    function GetSubDataSetIdx(aIdx : Integer): TComponent;
    procedure RegisterSubDataSet(aDataSet : TComponent);
    property SubDataSet[aIdx : Integer] : TComponent read GetSubDataSetIdx;
  end;

  IBaseModifiedDS = interface['{311D0DE7-9248-4412-8195-B69EAB813895}']
    function IsChanged : Boolean;
  end;

  { IBaseDBConnection }

  IBaseDBConnection = Interface['{FA8047C2-585E-4951-90B2-B97B9CB4F0FB}']
    function DoSetProperties(aProp : string) : Boolean;
    function DoCreateDBFromProperties(aProp : string) : Boolean;
    function GetHandle : Pointer;
    function DoGetDBLayerType : string;
    function DoInitializeConnection : Boolean;
    function DoExecuteDirect(aSQL : string) : Integer;
    function DoStartTransaction(ForceTransaction : Boolean = False): Boolean;
    function DoCommitTransaction: Boolean;
    function DoRollbackTransaction: Boolean;
    function IsTransactionActive: Boolean;
    procedure DoAbstractDisconnect;
    procedure DoAbstractConnect;
    function DoGetTableNames(aTables : TStrings) : Boolean;
    function DoGetTriggerNames(aTriggers : TStrings) : Boolean;
    function DoGetColumns(aTableName: string): TStrings;
    function DoGetIndexes(aTableName: string): TStrings;
    function GetDatabaseName : string;
    function IsConnected : Boolean;
    function GetLimitAfterSelect : Boolean;
    function GetLimitSTMT : string;
    function GetUniID(aConnection : TComponent;Generator : string;Tablename : string;AutoInc : Boolean) : Variant;
    function GetSyncOffset: Integer;
    procedure SetSyncOffset(const AValue: Integer);
    function UseExtData : Boolean;
    function GetDatabaseDir : string;
  end;

  { TAbstractDBDataset }

  TAbstractDBDataset = class(TSQLRecord)
  private
    FChanged: Boolean;
    FDoChange:Integer;
    FOnChanged: TNotifyEvent;
    FOnRemoved: TNotifyEvent;
    FUpdateFloatFields: Boolean;
    FDataSet: TDataSet;
    DoCheck: Boolean;
    FParent: TAbstractDBDataset;
    FWasOpen : Boolean;
    FSecModified: Boolean;
    FTableName : string;
    function GetActive: Boolean;
    function GetCanEdit: Boolean;
    function GetCaption: string;
    function GetConnection: TComponent;
    function GetCount: Integer;
    function GetFilter: string;
    function GetFRows: Integer;
    function GetFullCount: Integer;
    function GetIsReadOnly: Boolean;
    function GetLimit: Integer;
    function GetSortDirection: TSortDirection;
    function GetSortFields: string;
    function GetState: TDataSetState;
    function GetTableName: string;
    procedure SetActive(AValue: Boolean);
    procedure SetFilter(AValue: string);
    procedure SetFRows(AValue: Integer);
    procedure SetLimit(AValue: Integer);
    procedure SetIsReadOnly(AValue: Boolean);
    procedure SetSortDirection(AValue: TSortDirection);
    procedure SetSortFields(AValue: string);
  protected
    FFreeDataSet : Boolean;
  public
    //constructor CreateExIntegrity(aOwner : TComponent;DM : TComponent;aUseIntegrity : Boolean;aConnection : TComponent = nil;aMasterdata : TDataSet = nil);virtual;
    //constructor CreateEx(aOwner : TComponent;DM : TComponent;aConnection : TComponent = nil;aMasterdata : TDataSet = nil);virtual;
    constructor Create;override;
    class procedure DefineTable(Model: TSQLModel);
    class procedure InitializeTable(Server: TSQLRestServer;
  const FieldName: RawUTF8; Options: TSQLInitializeTableOptions); override;
    destructor Destroy; override;
    property DataSet : TDataSet read FDataSet write FDataSet;
    procedure Open;virtual;
    procedure Close;virtual;
    class procedure DefineFields(aDataSet : TDataSet);virtual;
    procedure DefineDefaultFields(aDataSet : TDataSet;HasMasterSource : Boolean);virtual;
    procedure DefineUserFields(aDataSet: TDataSet);virtual;
    procedure FillDefaults(aDataSet : TDataSet);virtual;
    procedure SetDisplayLabels(aDataSet : TDataSet);virtual;
    procedure DisableChanges;virtual;
    procedure EnableChanges;virtual;
    procedure Change;virtual;
    procedure UnChange;virtual;
    function CreateTable : Boolean;virtual;
    function CheckTable : Boolean;
    function AlterTable : Boolean;virtual;
    property Count : Integer read GetCount;
    property FullCount : Integer read GetFullCount;
    property Connection : TComponent read GetConnection;
    property State : TDataSetState read GetState;
    property TableName : string read GetTableName write FTableName;
    procedure CascadicPost;virtual;
    procedure CascadicCancel;virtual;
    function Delete : Boolean;virtual;
    procedure Insert;virtual;
    procedure Append;virtual;
    procedure First;virtual;
    procedure Last;virtual;
    procedure Next;virtual;
    procedure Prior;virtual;
    procedure Post;virtual;
    procedure Edit;virtual;
    procedure Cancel;virtual;
    function Locate(const keyfields: string; const keyvalues: Variant; options: TLocateOptions) : boolean; virtual;
    function EOF : Boolean;virtual;
    function FieldByName(const aFieldName : string) : db.TField;virtual;

    function CheckForInjection(aQuery : string) : Boolean;virtual;
    //procedure DoBeforeDelete;virtual;
    //procedure DoAfterDelete;virtual;
    property UpdateFloatFields : Boolean read FUpdateFloatFields write FUpdateFloatFields;
    property OnChange : TNotifyEvent read FOnChanged write FOnChanged;
    property OnRemove : TNotifyEvent read FOnRemoved write FOnRemoved;
    property Changed : Boolean read FChanged;
    procedure Filter(aFilter : string;aLimit : Integer = 0);virtual;
    procedure FilterEx(aFilter : string;aLimit : Integer = 0;aOrderBy : string = '';aSortDirection : string = 'ASC';aLocalSorting : Boolean = False;aGlobalFilter : Boolean = True;aUsePermissions : Boolean = False;aFilterIn : string = '');virtual;
    property ActualFilter : string read GetFilter write SetFilter;
    property ActualLimit : Integer read GetLimit write SetLimit;
    property SortFields : string read GetSortFields write SetSortFields;
    property SortDirection : TSortDirection read GetSortDirection write SetSortDirection;
    property FetchRows : Integer read GetFRows write SetFRows;
    property Parent : TAbstractDBDataset read FParent;
    property CanEdit : Boolean read GetCanEdit;
    property IsReadOnly : Boolean read GetIsReadOnly write SetIsReadOnly;
    property Active : Boolean read GetActive write SetActive;
    property Caption : string read GetCaption;
  end;

var
  Data: TSQLDBConnectionProperties;

implementation

//uses uAbstractDBLayer;

procedure TAbstractDBDataset.FilterEx(aFilter: string; aLimit: Integer;
  aOrderBy: string; aSortDirection: string; aLocalSorting: Boolean;
  aGlobalFilter: Boolean; aUsePermissions: Boolean; aFilterIn: string);
begin
end;

procedure TAbstractDBDataset.Filter(aFilter: string; aLimit: Integer);
begin
  if (not ((ActualFilter=aFilter) and (aLimit=ActualLimit))) or (not DataSet.Active) then
    FilterEx(aFilter,aLimit);
end;

constructor TAbstractDBDataset.Create;
begin
  inherited Create;
  FFreeDataSet:=True;
  FUpdateFloatFields := false;
end;
type

  { TFakeTable }

  TFakeTable = class(TDataSet,IBaseManageDB)
  private
    FManagedFieldDefs : TFieldDefs;
    FManagedIndexDefs : TIndexDefs;
    FTableName : string;
  protected
    ParentObj : TAbstractDBDataset;
    constructor Create(AOwner: TComponent); override;
    //IBaseManageDB
    function GetManagedFieldDefs: TFieldDefs;
    function GetManagedIndexDefs: TIndexDefs;
    function GetTableName: string;
    procedure SetTableName(const AValue: string);
    function GetTableNames: string;
    function GetConnection: TComponent;
    function GetTableCaption: string;
    procedure SetTableCaption(const AValue: string);
    function GetUpStdFields: Boolean;
    procedure SetUpStdFields(AValue: Boolean);
    function GetUpChangedBy: Boolean;
    procedure SetUpChangedBy(AValue: Boolean);
    function GetUseIntegrity: Boolean;
    procedure SetUseIntegrity(AValue: Boolean);
    function GetAsReadonly: Boolean;
    procedure SetAsReadonly(AValue: Boolean);
    procedure SetConnection(aConn: TComponent);
    function GetMasterdataSource: TDataSource;
    procedure SetMasterdataSource(AValue: TDataSource);
    procedure SetTableNames(const AValue: string);
    procedure SetOrigTable(AValue: TComponent);
    function GetOrigTable : TComponent;
    procedure SetDataSource(AValue: TDataSource);
  end;

{ TFakeTable }

constructor TFakeTable.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FManagedFieldDefs := TFieldDefs.Create(Self);
  FManagedIndexDefs := TIndexDefs.Create(Self);
end;
function TFakeTable.GetManagedFieldDefs: TFieldDefs;
begin
  Result := FManagedFieldDefs;
end;
function TFakeTable.GetManagedIndexDefs: TIndexDefs;
begin
  Result := FManagedIndexDefs;
end;
function TFakeTable.GetTableName: string;
begin
  Result := FTableName;
end;
procedure TFakeTable.SetTableName(const AValue: string);
begin
  FTableName := AValue;
end;
function TFakeTable.GetTableNames: string;
begin
end;
function TFakeTable.GetConnection: TComponent;
begin
end;
function TFakeTable.GetTableCaption: string;
begin
end;
procedure TFakeTable.SetTableCaption(const AValue: string);
begin
end;
function TFakeTable.GetUpStdFields: Boolean;
begin
end;
procedure TFakeTable.SetUpStdFields(AValue: Boolean);
begin
end;
function TFakeTable.GetUpChangedBy: Boolean;
begin
end;
procedure TFakeTable.SetUpChangedBy(AValue: Boolean);
begin
end;
function TFakeTable.GetUseIntegrity: Boolean;
begin
end;
procedure TFakeTable.SetUseIntegrity(AValue: Boolean);
begin
end;
function TFakeTable.GetAsReadonly: Boolean;
begin
end;
procedure TFakeTable.SetAsReadonly(AValue: Boolean);
begin
end;
procedure TFakeTable.SetConnection(aConn: TComponent);
begin
end;
function TFakeTable.GetMasterdataSource: TDataSource;
begin
end;
procedure TFakeTable.SetMasterdataSource(AValue: TDataSource);
begin
end;
procedure TFakeTable.SetTableNames(const AValue: string);
begin
end;
procedure TFakeTable.SetOrigTable(AValue: TComponent);
begin
end;
function TFakeTable.GetOrigTable: TComponent;
begin
end;
procedure TFakeTable.SetDataSource(AValue: TDataSource);
begin
end;
class procedure TAbstractDBDataset.DefineTable(Model : TSQLModel);
var
  aTable : TFakeTable;
  aMapping: PSQLRecordPropertiesMapping;
begin
  aTable := TFakeTable.Create(nil);
  try
    DefineFields(aTable);
    aMapping := VirtualTableExternalMap(Model,TSQLRecordClass(Self.ClassType),Data,'"'+aTable.GetTableName+'"');
    aMapping^.MapFields(['ID','SQL_ID'//,
  //                       'TYP','TYPE'
                        ]); // no ID/RowID for our aggregates
    aMapping^.SetOptions(aMapping^.Options+[rpmQuoteFieldName]);
  finally
    aTable.Free;
  end;
end;

class procedure TAbstractDBDataset.InitializeTable(Server: TSQLRestServer;
  const FieldName: RawUTF8; Options: TSQLInitializeTableOptions);
begin
  inherited InitializeTable(Server, FieldName, Options);
  DefineTable(Server.Model);
end;

destructor TAbstractDBDataset.Destroy;
begin
  if FFreeDataSet then
    FreeAndNil(FDataSet);
  inherited Destroy;
end;

procedure TAbstractDBDataset.FillDefaults(aDataSet: TDataSet);
begin
end;

procedure TAbstractDBDataset.SetDisplayLabels(aDataSet: TDataSet);
begin
end;

procedure TAbstractDBDataset.DisableChanges;
begin
  inc(FDoChange);
end;

procedure TAbstractDBDataset.EnableChanges;
begin
  if FDoChange > 0 then
    dec(FDoChange);
end;
procedure TAbstractDBDataset.CascadicPost;
begin
  if CanEdit then
    begin
      Post;
      UnChange;
      if Assigned(FOnChanged) then
        FOnChanged(Self);
    end;
end;
procedure TAbstractDBDataset.CascadicCancel;
begin
  if (FDataSet.State = dsEdit) or (FDataSet.State = dsInsert) then
    FDataSet.Cancel;
  UnChange;
  if Assigned(FOnChanged) then
    FOnChanged(Self);
end;
procedure TAbstractDBDataset.Change;
begin
  if FDoChange > 0 then exit;
  if fChanged then exit;
  FChanged := True;
  if Assigned(FOnChanged) then
    FOnChanged(Self);
end;

procedure TAbstractDBDataset.UnChange;
begin
  try
    if not FChanged then exit;
    FChanged:=False;
    if Assigned(FOnChanged) then
      FOnChanged(Self);
  except
  end;
end;

function TAbstractDBDataset.CreateTable: Boolean;
var
  aSQL: String;
  i: Integer;
  RestartTransaction: Boolean = False;
  NewTableName: String;
  tmpp: SizeInt;
begin
  Result := False;
end;

function TAbstractDBDataset.CheckTable: Boolean;
var
  i: Integer;
  aIndexes: TStrings;
begin
  Result := False;
end;

function TAbstractDBDataset.AlterTable: Boolean;
var
  i: Integer;
  aSQL: String;
  tmpSize: Integer;
  aChanged: Boolean = False;
  aIndexes: TStrings;
  tmp: String;
  tmp1: String;
begin
  //writeln('Altering ',Self.TableName);
end;

function TAbstractDBDataset.Delete: Boolean;
begin
  Result := False;
  if FDataSet.Active and (Count > 0) then
    begin
      Change;
      FDataSet.Delete;
      Result := True;
    end;
end;

procedure TAbstractDBDataset.Insert;
begin
  DataSet.Insert;
end;
procedure TAbstractDBDataset.Append;
begin
  DataSet.Append;
end;

procedure TAbstractDBDataset.First;
begin
  DataSet.First;
end;

procedure TAbstractDBDataset.Last;
begin
  DataSet.Last;
end;

procedure TAbstractDBDataset.Next;
begin
  DataSet.Next;
end;
procedure TAbstractDBDataset.Prior;
begin
  DataSet.Prior;
end;

procedure TAbstractDBDataset.Post;
begin
  if CanEdit then
    FDataSet.Post;
end;

procedure TAbstractDBDataset.Edit;
begin
  if not CanEdit then
    DataSet.Edit;
end;

procedure TAbstractDBDataset.Cancel;
begin
  if Assigned(FDataSet) and (FDataSet.Active) then
    FDataSet.Cancel;
end;

function TAbstractDBDataset.Locate(const keyfields: string;
  const keyvalues: Variant; options: TLocateOptions): boolean;
begin
  Result := False;
  if DataSet.Active then
    Result := DataSet.Locate(keyfields,keyvalues,options);
end;

function TAbstractDBDataset.EOF: Boolean;
begin
  Result := True;
  if Assigned(FDataSet) and (FDataSet.Active) then
    Result := FDataSet.EOF;
end;

function TAbstractDBDataset.FieldByName(const aFieldName: string): db.TField;
begin
  Result := nil;
  if Assigned(DataSet) then
    begin
      if not DataSet.Active and FWasOpen then
        Open;
      if DataSet.FieldDefs.IndexOf(aFieldName)>=0 then
        Result := DataSet.FieldByName(aFieldname);
    end;
end;

function TAbstractDBDataset.CheckForInjection(aQuery: string): Boolean;
begin
  Result := False;
end;

function TAbstractDBDataset.GetCaption: string;
begin
end;
function TAbstractDBDataset.GetCanEdit: Boolean;
begin
  Result := Assigned(Self) and (Self is TAbstractDBDataset) and Assigned(fdataSet) and (FDataSet.State = dsEdit) or (FDataSet.State = dsInsert);
end;

function TAbstractDBDataset.GetActive: Boolean;
begin
  Result := False;
  if Assigned(FDataSet) then
    Result := FDataSet.Active;
end;

function TAbstractDBDataset.GetCount: Integer;
begin
  if DataSet.Active then
    Result := DataSet.RecordCount
  else
    Result := -1;
end;
function TAbstractDBDataset.GetConnection: TComponent;
begin
end;
function TAbstractDBDataset.GetFilter: string;
begin
  result := '';
  if not Assigned(DataSet) then exit;
end;
function TAbstractDBDataset.GetFRows: Integer;
begin
  result := -1;
  if not Assigned(DataSet) then exit;
end;
function TAbstractDBDataset.GetFullCount: Integer;
var
  aDS: TDataSet;
  aFilter: String;
begin
end;

function TAbstractDBDataset.GetLimit: Integer;
begin
  result := -1;
  if not Assigned(DataSet) then exit;
end;

function TAbstractDBDataset.GetIsReadOnly: Boolean;
begin
end;

function TAbstractDBDataset.GetSortDirection: TSortDirection;
begin
  if not Assigned(DataSet) then exit;
end;

function TAbstractDBDataset.GetSortFields: string;
begin
  if not Assigned(DataSet) then exit;
end;

function TAbstractDBDataset.GetState: TDataSetState;
begin
end;

function TAbstractDBDataset.GetTableName: string;
begin
  Result := FTableName;
end;

procedure TAbstractDBDataset.SetActive(AValue: Boolean);
begin
  if (not AValue) and Active then
    Close
  else if AValue and (not Active) then
    Open;
end;
procedure TAbstractDBDataset.SetFilter(AValue: string);
begin
  if GetFilter = AValue then exit;
end;
procedure TAbstractDBDataset.SetFRows(AValue: Integer);
begin
end;

procedure TAbstractDBDataset.SetLimit(AValue: Integer);
begin
end;

procedure TAbstractDBDataset.SetIsReadOnly(AValue: Boolean);
begin
end;

procedure TAbstractDBDataset.SetSortDirection(AValue: TSortDirection);
begin
  if not Assigned(DataSet) then exit;
end;

procedure TAbstractDBDataset.SetSortFields(AValue: string);
begin
  if not Assigned(DataSet) then exit;
end;
procedure TAbstractDBDataset.Open;
var
  Retry: Boolean = False;
  aCreated: Boolean = False;
  aOldFilter: String = '';
  aOldLimit: Integer;
  aErr, OldFields: String;
  OldLimit: Integer;
begin
  if not Assigned(FDataSet) then exit;
end;

procedure TAbstractDBDataset.Close;
begin
  if not Assigned(FDataSet) then exit;
end;

class procedure TAbstractDBDataset.DefineFields(aDataSet: TDataSet);
begin
end;

procedure TAbstractDBDataset.DefineDefaultFields(aDataSet: TDataSet;
  HasMasterSource: Boolean);
begin
end;

procedure TAbstractDBDataset.DefineUserFields(aDataSet: TDataSet);
begin
end;

end.

