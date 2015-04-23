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
Created 20.04.2015
*******************************************************************************}
library zsqldb;

{$mode objfpc}{$H+}
{$DEFINE USE_BIN_STR}
{$INTERFACES CORBA}

uses
  Classes,sysutils, zcomponent_nogui, ZConnection, db, ZSqlMetadata,
  ZAbstractRODataset, ZDataset, ZSequence,ZAbstractConnection,
  ZSqlMonitor,Utils,uEncrypt,ZDbcIntfs, uzsqldbdataset;

var
  FMainConnection : TZConnection;
  FLimitAfterSelect: Boolean;
  FLimitSTMT: String;
  FDBTyp: String;
  Sequence: TZSequence;
  IgnoreOpenRequests : Boolean;
resourcestring
  strUnknownDbType                = 'Unbekannter Datenbanktyp';
  strDatabaseConnectionLost       = 'Die Datenbankverbindung wurde verlohren !';

function GetNewConnection: TComponent;
begin
end;
procedure Disconnect(aConnection : TComponent);
begin
end;
function TableExists(aTableName : string;aConnection : TComponent = nil;AllowLowercase: Boolean = False) : Boolean;
begin
end;
function QuoteField(aField: string): string;
begin
end;
function SetProperties(aProp : string;Connection : TComponent = nil) : Boolean;
var
  tmp: String;
  FConnection : TZConnection;
  FProperties: String;
begin
  FProperties := aProp;
  FConnection := TZConnection(Connection);
  if not Assigned(FConnection) then
    begin
      FConnection := FMainConnection;
      if FConnection.Connected then
        FConnection.Disconnect;
    end;
  Result := True;
  tmp := aProp;
  try
    if FConnection.Connected then
      FConnection.Disconnect;
    FConnection.Port:=0;
    FConnection.Properties.Clear;
    FConnection.Properties.Add('timeout=2');
    FConnection.ClientCodepage:='UTF8';
    FConnection.Protocol:='';
    FConnection.User:='';
    FConnection.Password:='';
    FConnection.HostName:='';
    FConnection.Database:='';
    FConnection.Properties.Clear;
    FConnection.Protocol:=copy(tmp,0,pos(';',tmp)-1);
    Assert(FConnection.Protocol<>'',strUnknownDbType);
    tmp := copy(tmp,pos(';',tmp)+1,length(tmp));
    FConnection.HostName := copy(tmp,0,pos(';',tmp)-1);
    if pos(':',FConnection.HostName) > 0 then
      begin
        FConnection.Port:=StrToInt(copy(FConnection.HostName,pos(':',FConnection.HostName)+1,length(FConnection.HostName)));
        FConnection.HostName:=copy(FConnection.HostName,0,pos(':',FConnection.HostName)-1);
      end
    else if pos('/',FConnection.HostName) > 0 then
      begin
        FConnection.Port:=StrToInt(copy(FConnection.HostName,pos('/',FConnection.HostName)+1,length(FConnection.HostName)));
        FConnection.HostName:=copy(FConnection.HostName,0,pos('/',FConnection.HostName)-1);
      end;
    tmp := copy(tmp,pos(';',tmp)+1,length(tmp));
    FConnection.Database:=copy(tmp,0,pos(';',tmp)-1);
    tmp := copy(tmp,pos(';',tmp)+1,length(tmp));
    FConnection.User := copy(tmp,0,pos(';',tmp)-1);
    tmp := copy(tmp,pos(';',tmp)+1,length(tmp));
    if copy(tmp,0,1) = 'x' then
      FConnection.Password := Decrypt(copy(tmp,2,length(tmp)),99998)
    else
      FConnection.Password := tmp;
    if (copy(FConnection.Protocol,0,6) = 'sqlite')
    or (copy(FConnection.Protocol,0,8) = 'postgres')
    then
      begin
        FConnection.TransactIsolationLevel:=tiNone;
        if (copy(FConnection.Protocol,0,6) = 'sqlite') then
          if not FileExists(FConnection.Database) then
            raise Exception.Create('Databasefile dosend exists');
      end
    else if (copy(FConnection.Protocol,0,5) = 'mssql') then
      FConnection.TransactIsolationLevel:=tiReadUnCommitted
    else if (copy(FConnection.Protocol,0,8) = 'firebird')
    or (copy(FConnection.Protocol,0,9) = 'interbase')
    or (copy(FConnection.Protocol,0,5) = 'mysql')
    then
      begin
        FConnection.TransactIsolationLevel:=tiReadCommitted;
      end;
    FConnection.Connected:=True;
    FLimitAfterSelect := False;
    FLimitSTMT := 'LIMIT %d';
    FDBTyp := FConnection.Protocol;
    if FConnection.Protocol = 'sqlite-3' then
      begin
//        FConnection.ExecuteDirect('PRAGMA synchronous = NORMAL;');
//        FConnection.ExecuteDirect('PRAGMA cache_size = 5120;');
//        FConnection.ExecuteDirect('PRAGMA auto_vacuum = FULL;');
        FConnection.ExecuteDirect('PRAGMA journal_mode = MEMORY;');
        FConnection.ExecuteDirect('PRAGMA recursive_triggers = ON;');
        FConnection.ExecuteDirect('PRAGMA foreign_keys = ON;');
        FConnection.ExecuteDirect('PRAGMA case_sensitive_like = ON;');
      end
    else if (copy(FConnection.Protocol,0,8) = 'firebird')
         or (copy(FConnection.Protocol,0,9) = 'interbase') then
      begin
        FDBTyp := 'firebird';
        FLimitSTMT := 'ROWS 1 TO %d';
        if not Assigned(Sequence) then
          begin
            Sequence := TZSequence.Create(nil);
          end;
      end
    else if FConnection.Protocol = 'mssql' then
      begin
        FLimitAfterSelect := True;
        FLimitSTMT := 'TOP %d';
      end;
  except on e : Exception do
    begin
      //if Assigned(BaseApplication) then
      //  with BaseApplication as IBaseDBInterface do
      //    LastError := e.Message;
      Result := False;
    end;
  end;
  if Result then
    begin
      if not (TableExists('USERS') and TableExists('GEN_SQL_ID') and TableExists('GEN_AUTO_ID')) then //Create generators
        begin
          try
            if (copy(FConnection.Protocol,0,8) = 'firebird')
            or (copy(FConnection.Protocol,0,9) = 'interbase') then
              begin
                FConnection.ExecuteDirect('EXECUTE BLOCK AS BEGIN'+lineending
                                         +'if (not exists(select 1 from rdb$generators where rdb$generator_name = ''GEN_SQL_ID'')) then'+lineending
                                         +'execute statement ''CREATE SEQUENCE GEN_SQL_ID;'';'+lineending
                                         +'END;');
                FConnection.ExecuteDirect('EXECUTE BLOCK AS BEGIN'+lineending
                                         +'if (not exists(select 1 from rdb$generators where rdb$generator_name = ''GEN_AUTO_ID'')) then'+lineending
                                         +'execute statement ''CREATE SEQUENCE GEN_AUTO_ID;'';'+lineending
                                         +'END;');
              end
            else if copy(FConnection.Protocol,0,6) = 'sqlite' then
              begin
                FConnection.ExecuteDirect('CREATE TABLE IF NOT EXISTS "GEN_SQL_ID"("SQL_ID" BIGINT NOT NULL PRIMARY KEY,ID BIGINT);');
                FConnection.ExecuteDirect('CREATE TABLE IF NOT EXISTS "GEN_AUTO_ID"("SQL_ID" BIGINT NOT NULL PRIMARY KEY,ID BIGINT);');
              end
            else
              begin
                if not TableExists('GEN_SQL_ID') then
                  FConnection.ExecuteDirect('CREATE TABLE '+QuoteField('GEN_SQL_ID')+'('+QuoteField('SQL_ID')+' BIGINT NOT NULL PRIMARY KEY,'+QuoteField('ID')+' BIGINT);');
                if not TableExists('GEN_AUTO_ID') then
                  FConnection.ExecuteDirect('CREATE TABLE '+QuoteField('GEN_AUTO_ID')+'('+QuoteField('SQL_ID')+' BIGINT NOT NULL PRIMARY KEY,'+QuoteField('ID')+' BIGINT);');
              end
          except on e : Exception do
            begin
              //if Assigned(BaseApplication) then
              //  with BaseApplication as IBaseDBInterface do
              //    LastError := e.Message;
              Result := False;
            end;
          end;
        end;
    end;
end;
function CreateDBFromProperties(aProp: string): Boolean;
var
  FConnection: TZConnection;
  tmp: String;
  aPassword: String;
  aUser: String;
  aDatabase: String;
begin
  FConnection := TZConnection.Create(nil);
  //if Assigned(BaseApplication) then
  //  with BaseApplication as IBaseDBInterface do
  //    LastError := '';
  tmp := aProp;
  FConnection.Protocol:=copy(tmp,0,pos(';',tmp)-1);
  Assert(FConnection.Protocol<>'',strUnknownDbType);
  tmp := copy(tmp,pos(';',tmp)+1,length(tmp));
  FConnection.HostName := copy(tmp,0,pos(';',tmp)-1);
  if pos(':',FConnection.HostName) > 0 then
    begin
      FConnection.Port:=StrToInt(copy(FConnection.HostName,pos(':',FConnection.HostName)+1,length(FConnection.HostName)));
      FConnection.HostName:=copy(FConnection.HostName,0,pos(':',FConnection.HostName)-1);
    end
  else if pos('/',FConnection.HostName) > 0 then
    begin
      FConnection.Port:=StrToInt(copy(FConnection.HostName,pos('/',FConnection.HostName)+1,length(FConnection.HostName)));
      FConnection.HostName:=copy(FConnection.HostName,0,pos('/',FConnection.HostName)-1);
    end;
  tmp := copy(tmp,pos(';',tmp)+1,length(tmp));
  aDatabase:=copy(tmp,0,pos(';',tmp)-1);
  tmp := copy(tmp,pos(';',tmp)+1,length(tmp));
  aUser := copy(tmp,0,pos(';',tmp)-1);
  FConnection.User:=aUser;
  tmp := copy(tmp,pos(';',tmp)+1,length(tmp));
  FConnection.Database:=aDatabase;
  if copy(tmp,0,1) = 'x' then
    aPassword := Decrypt(copy(tmp,2,length(tmp)),99998)
  else
    aPassword := tmp;
  FConnection.Password:=aPassword;
  if (copy(FConnection.Protocol,0,8) = 'postgres')
  then
    begin
      FConnection.Database:='postgres';
    end
    else if (copy(FConnection.Protocol,0,5) = 'mssql') then
      FConnection.Properties.Add('CreateNewDatabase=CREATE DATABASE "'+aDatabase+'"')
    else if (copy(FConnection.Protocol,0,8) = 'firebird')
    or (copy(FConnection.Protocol,0,9) = 'interbase')
    then
      begin
        if FConnection.HostName <> '' then
          FConnection.Properties.Add('CreateNewDatabase=CREATE DATABASE '''+FConnection.HostName+':'+aDatabase+''' USER '''+aUser+''' PASSWORD '''+aPassword+''' PAGE_SIZE = 4096 DEFAULT CHARACTER SET UTF8')
        else
          FConnection.Properties.Add('CreateNewDatabase=CREATE DATABASE '''+aDatabase+''' USER '''+aUser+''' PASSWORD '''+aPassword+''' PAGE_SIZE = 4096 DEFAULT CHARACTER SET UTF8');
      end
    else if (copy(FConnection.Protocol,0,6) = 'sqlite') then
      begin
        ForceDirectories(ExtractFileDir(FConnection.Database));
      end;
  try
    FConnection.Connected:=True;
  except
    on e : Exception do
    //if Assigned(BaseApplication) then
    //  with BaseApplication as IBaseDBInterface do
    //    LastError := e.Message;
  end;
  if (copy(FConnection.Protocol,0,8) = 'postgres')
  then
    begin
      Result := FConnection.ExecuteDirect('CREATE DATABASE "'+aDatabase+'" WITH OWNER = "'+aUser+'" ENCODING = ''UTF8'' CONNECTION LIMIT = -1;');
      FConnection.Disconnect;
      FConnection.Database:=aDatabase;
    end;
  FConnection.Connected:=True;
  Result := FConnection.Connected;
  FConnection.Free;
end;
function IsSQLDB : Boolean;
begin
  Result := True;
end;
function GetNewDataSet(aSQL : string;aConnection : TComponent = nil;MasterData : TDataSet = nil;aOrigtable : TObject = nil) : TDataSet;
begin
  if IgnoreOpenrequests then exit;
  Result := FDataSetClass.Create(Self);
  if not Assigned(aConnection) then
    aConnection := MainConnection;
  with TZeosDBDataSet(Result) do
    begin
      Connection := TZConnection(aConnection);
      FTableNames := aTables;
      aTable.DefineFields(Result);
      aTable.DefineDefaultFields(Result,Assigned(Masterdata));
      FOrigTable := aTable;
      if Assigned(Masterdata) then
        begin
          if not Assigned(TZeosDBDataSet(MasterData).MasterDataSource) then
            begin
              TZeosDBDataSet(MasterData).MasterDataSource := TDataSource.Create(Self);
              TZeosDBDataSet(MasterData).MasterDataSource.DataSet := MasterData;
            end;
          DataSource := TZeosDBDataSet(MasterData).MasterDataSource;
          MasterSource := TZeosDBDataSet(MasterData).MasterDataSource;
          with Masterdata as IBaseSubDataSets do
            RegisterSubDataSet(aTable);
        end;
    end;
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
function StartTransaction(aConnection : TComponent;ForceTransaction : Boolean = False): Boolean;
begin
end;
function CommitTransaction(aConnection : TComponent): Boolean;
begin
end;
function RollbackTransaction(aConnection : TComponent): Boolean;
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
