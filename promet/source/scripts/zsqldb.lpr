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
  Classes, sysutils, zcomponent_nogui, db, uzsqldbdataset,uzsqldbfunctions;

procedure ScriptCleanup;
begin
end;

function ScriptDefinition : PChar;stdcall;
begin
  Result := 'function SetProperties(aProp : string;Connection : TComponent) : Boolean;'
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
