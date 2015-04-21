library filedb;

{$mode objfpc}{$H+}
{$DEFINE USE_BIN_STR}
{$INTERFACES CORBA}

uses
  Classes,sysutils, db;

function GetNewConnection: TComponent;
begin
end;
procedure Disconnect(aConnection : TComponent);
begin
end;
function SetProperties(aProp : string;Connection : TComponent = nil) : Boolean;
begin
end;
function CreateDBFromProperties(aProp: string): Boolean;
begin
end;
function IsSQLDB : Boolean;
begin
  Result := True;
end;
function GetNewDataSet(aSQL : string;aConnection : TComponent = nil;MasterData : TDataSet = nil;aOrigtable : TObject = nil) : TDataSet;
begin
end;
procedure DestroyDataSet(DataSet : TDataSet);
begin
end;
function Ping(aConnection : TComponent) : Boolean;
begin
end;
function DateToFilter(aValue : TDateTime) : string;
begin
end;
function DateTimeToFilter(aValue : TDateTime) : string;
begin
end;
function GetUniID(aConnection : TComponent = nil;Generator : string = 'GEN_SQL_ID';AutoInc : Boolean = True) : Variant;
begin
end;
procedure StreamToBlobField(Stream : TStream;DataSet : TDataSet;Fieldname : string);
begin
end;
procedure BlobFieldToStream(DataSet: TDataSet; Fieldname: string;dStream: TStream);
begin
end;
function GetErrorNum(e: EDatabaseError): Integer;
begin
end;
procedure DeleteExpiredSessions;
begin
end;
function QuoteField(aField: string): string;
begin
end;
function StartTransaction(aConnection : TComponent;ForceTransaction : Boolean = False): Boolean;
begin
end;
function CommitTransaction(aConnection : TComponent): Boolean;
begin
end;
function RollbackTransaction(aConnection : TComponent): Boolean;
begin
end;
function TableExists(aTableName : string;aConnection : TComponent = nil;AllowLowercase: Boolean = False) : Boolean;
begin
end;
function TriggerExists(aTriggerName: string; aConnection: TComponent=nil; AllowLowercase: Boolean=False): Boolean;
begin
end;
function GetDBType: string;
begin
end;
function CreateTrigger(aTriggerName: string; aTableName: string; aUpdateOn: string; aSQL: string;aField : string = ''; aConnection: TComponent=nil): Boolean;
begin
end;
function DropTable(aTableName : string) : Boolean;
begin
end;
function FieldToSQL(aName : string;aType : TFieldType;aSize : Integer;aRequired : Boolean) : string;
begin
end;
function GetColumns(TableName : string) : TStrings;
begin
end;

procedure ScriptCleanup;
begin
end;

function ScriptDefinition : PChar;stdcall;
begin
  Result := 'function SetProperties(aProp : string;Connection : TComponent) : Boolean;stdcall;'
       +#10+'function CreateDBFromProperties(aProp: string): Boolean;'
       +#10+'function IsSQLDB : Boolean;'
       +#10+'function GetNewDataSet(aSQL : string;aConnection : TComponent;MasterData : TDataSet;aOrigtable : TObject) : TDataSet;'
       +#10+'procedure DestroyDataSet(DataSet : TDataSet);'
       +#10+'function Ping(aConnection : TComponent) : Boolean;'
       +#10+'function DateToFilter(aValue : TDateTime) : string;'
       +#10+'function DateTimeToFilter(aValue : TDateTime) : string;'
       +#10+'function GetUniID(aConnection : TComponent;Generator : string;AutoInc : Boolean) : Variant;'
       //+#10+'procedure StreamToBlobField(Stream : TStream;DataSet : TDataSet;Fieldname : string);'
       //+#10+'procedure BlobFieldToStream(DataSet: TDataSet; Fieldname: string;dStream: TStream);'
       //+#10+'function GetErrorNum(e: EDatabaseError): Integer;'
       +#10+'function GetNewConnection: TComponent;'
       +#10+'function QuoteField(aField: string): string;'
       +#10+'procedure Disconnect(aConnection : TComponent);'
       +#10+'function StartTransaction(aConnection : TComponent;ForceTransaction : Boolean): Boolean;'
       +#10+'function CommitTransaction(aConnection : TComponent): Boolean;'
       +#10+'function RollbackTransaction(aConnection : TComponent): Boolean;'
       +#10+'function TableExists(aTableName : string;aConnection : TComponent;AllowLowercase: Boolean) : Boolean;'
       +#10+'function TriggerExists(aTriggerName: string; aConnection: TComponent; AllowLowercase: Boolean): Boolean;'
       +#10+'function GetDBType: string;'
       +#10+'function DropTable(aTableName : string) : Boolean;'
       //+#10+'function GetColumns(TableName : string) : TStrings;'

            ;
end;

exports
  SetProperties,
  CreateDBFromProperties,
  GetNewConnection,
  IsSQLDB,
  GetNewDataSet,
  DestroyDataSet,
  Ping,
  DateToFilter,
  DateTimeToFilter,
  GetUniID,
  StreamToBlobField,
  BlobFieldToStream,
  GetErrorNum,
  DeleteExpiredSessions,
  QuoteField,
  Disconnect,
  StartTransaction,
  CommitTransaction,
  RollbackTransaction,
  TableExists,
  TriggerExists,
  GetDBType,
  CreateTrigger,
  DropTable,

  ScriptCleanup,
  ScriptDefinition;

end.
