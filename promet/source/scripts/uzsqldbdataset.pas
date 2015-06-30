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
Created 23.04.2015
*******************************************************************************}
unit uzsqldbdataset;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ZClasses, ZDataset,ubasedatasetinterfaces,DB,ZSqlMetadata,
  ZConnection,ZAbstractConnection;

type
  TZeosDBDataSet = class(TZQuery,IBaseDBFilter,IBaseManageDB,IBaseSubDatasets,IBaseModifiedDS)
  private
    FSubDataSets : Tlist;
    FFields : string;
    FFilter,FBaseFilter : string;
    FLimit : Integer;
    FMDS: TDataSource;
    FSortDirection : TSortDirection;
    FSortFields : string;
    FTableNames : string;
    FDefaultTableName : string;
    FManagedFieldDefs : TFieldDefs;
    FManagedIndexDefs : TIndexDefs;
    FOrigTable : TAbstractDBDataset;
    FUsePermissions : Boolean;
    FTableCaption : string;
    FDistinct : Boolean;
    DoCheck: Boolean;
    FUpStdFields : Boolean;
    FUpChangedBy : Boolean;
    FBaseSortFields : string;
    FBaseSorting : string;
    FBaseSortDirection : TSortDirection;
    FUseBaseSorting : Boolean;
    FUseIntegrity : Boolean;
    FChangeUni : Boolean;
    FSQL : string;
    FHasNewID : Boolean;
    procedure SetNewIDIfNull;
    function BuildSQL : string;
    function IndexExists(IndexName : string) : Boolean;
    procedure WaitForLostConnection;
  protected
    //Internal DataSet Methods that needs to be changed
    procedure InternalOpen; override;
    procedure InternalRefresh; override;
    procedure InternalPost; override;
    procedure DoAfterInsert; override;
    procedure DoBeforePost; override;
    procedure DoBeforeInsert; override;
    procedure DoBeforeEdit; override;
    procedure SetFieldData(Field: TField; Buffer: Pointer); override;
    procedure DoBeforeDelete; override;
    procedure DoAfterDelete; override;
    procedure DoAfterScroll; override;
    procedure DoBeforeCancel; override;
    //IBaseDBFilter
    function GetFields: string;
    function GetBaseFilter: string;
    function GetLimit: Integer;
    function GetSortDirection: TSortDirection;
    function GetSortFields: string;
    function GetLocalSortFields : string;
    function GetBaseSortFields: string;
    function GetSortLocal: Boolean;
    procedure SetFields(const AValue: string);
    function GetFilter: string;
    procedure SetFilter(const AValue: string);
    procedure SetBaseFilter(const AValue: string);
    function GetSQL: string;
    procedure SetSQL(const AValue: string);
    procedure Setlimit(const AValue: Integer);
    procedure SetSortDirection(const AValue: TSortDirection);
    procedure SetSortFields(const AValue: string);
    procedure SetLocalSortFields(const AValue : string);
    procedure SetBaseSortFields(const AValue: string);
    procedure SetSortLocal(const AValue: Boolean);
    function GetFilterTables: string;
    procedure SetFilterTables(const AValue: string);
    function GetUsePermissions: Boolean;
    procedure SetUsePermisions(const AValue: Boolean);
    function GetDistinct: Boolean;
    procedure SetDistinct(const AValue: Boolean);
    function GetBaseSorting: string;
    procedure SetBaseSorting(AValue: string);
    function GetBaseSortDirection: TSortDirection;
    procedure SetBaseSortDirection(AValue: TSortDirection);
    function GetUseBaseSorting: Boolean;
    procedure SetUseBaseSorting(AValue: Boolean);
    function GetfetchRows: Integer;
    procedure SetfetchRows(AValue: Integer);
    //IBaseManageDB
    function GetManagedFieldDefs: TFieldDefs;
    function GetManagedIndexDefs: TIndexDefs;
    function GetTableName: string;
    procedure SetTableName(const AValue: string);
    function CreateTable : Boolean;
    function CheckTable : Boolean;
    function AlterTable : Boolean;
    function GetConnection: TComponent;
    function GetTableCaption: string;
    procedure SetTableCaption(const AValue: string);
    function GetUpStdFields: Boolean;
    procedure SetUpStdFields(AValue: Boolean);
    function GetUpChangedBy: Boolean;
    procedure SetUpChangedBy(AValue: Boolean);
    function GetUseIntegrity: Boolean;
    procedure SetUseIntegrity(AValue: Boolean);
    //IBaseSubDataSets
    function GetSubDataSet(aName : string): TComponent;
    procedure RegisterSubDataSet(aDataSet : TComponent);
    function GetCount : Integer;
    function GetSubDataSetIdx(aIdx : Integer): TComponent;
    //IBaseModifiedDS
    function IsChanged: Boolean;
  public
    constructor Create(AOwner : TComponent);override;
    destructor Destroy;override;
    property MasterDataSource : TDataSource read FMDS write FMDS;
    property DefaultTableName : string read FDefaultTableName;
    procedure DoExecSQL;
    property Tablenames : string read FTableNames write FTableNames;
    property OrigTable : TAbstractDBDataset read FOrigTable write FOrigTable;
    function NumRowsAffected: Integer;
  end;

implementation

uses uzsqldbfunctions;

procedure TZeosDBDataSet.SetNewIDIfNull;
begin
  if (FieldDefs.IndexOf('AUTO_ID') = -1) and (FieldDefs.IndexOf('SQL_ID') > -1) and  FieldByName('SQL_ID').IsNull then
    begin
      FieldByName('SQL_ID').AsVariant:=GetUniID(Connection);
      FHasNewID:=True;
    end
  else if (FieldDefs.IndexOf('SQL_ID') = -1) and (FieldDefs.IndexOf('AUTO_ID') > -1) and FieldByName('AUTO_ID').IsNull then
    begin
      FieldByName('AUTO_ID').AsVariant:=GetUniID(Connection,'GEN_AUTO_ID');
      FHasNewID:=True;
    end;
end;

function TZeosDBDataSet.BuildSQL : string;
function BuildJoins : string;
var
  aDS : string;
  tmp: String;
begin
  if not (pos(',',FTableNames) > 0) then
    begin
      Result := FTableNames;
      if Result = '' then
        Result := FDefaultTableName;
      Result := QuoteField(Result);
      exit;
    end;
  tmp := FTableNames+',';
  Result := copy(FTableNames,0,pos(',',FTableNames)-1);
  aDS := Result;
  tmp := copy(tmp,pos(',',tmp)+1,length(tmp));
  while pos(',',tmp) > 0 do
    begin
      Result := Result+ ' inner join '+QuoteField(copy(tmp,0,pos(',',tmp)-1))+' on '+QuoteField(copy(tmp,0,pos(',',tmp)-1))+'.REF_ID='+aDS+'.SQL_ID';
      aDS := QuoteField(copy(tmp,0,pos(',',tmp)-1));
      tmp := copy(tmp,pos(',',tmp)+1,length(tmp));
    end;
end;

var
  aFilter: String;
  aRefField: String;
  tmp: String;
  SResult: String;
  PJ: String = '';
  PW: String = '';

  procedure BuildSResult;
  begin
    SResult := '';
    if pos(',',QuoteField(FSortFields)) = 0 then
      begin
        sResult += QuoteField(FDefaultTableName)+'.'+QuoteField(FSortFields);
        if FSortDirection = sdAscending then
          sResult += ' ASC'
        else if FSortDirection = sdDescending then
          sResult += ' DESC'
        else
          begin
            if FBaseSortDirection = sdAscending then
              sResult += ' ASC'
            else if FBaseSortDirection = sdDescending then
              sResult += ' DESC'
          end;
      end
    else
      begin
        tmp := FSortFields;
        while pos(',',tmp) > 0 do
          begin
            sResult += QuoteField(FDefaultTableName)+'.'+QuoteField(copy(tmp,0,pos(',',tmp)-1));
            tmp := copy(tmp,pos(',',tmp)+1,length(tmp));
            if FSortDirection = sdAscending then
              sResult += ' ASC'
            else
              sResult += ' DESC';
            if trim(tmp) > '' then
              sResult+=',';
          end;
        if tmp <> '' then
          begin
            sResult += QuoteField(FDefaultTableName)+'.'+QuoteField(tmp);
            if FSortDirection = sdAscending then
              sResult += ' ASC'
            else
              sResult += ' DESC';
          end;
      end;
  end;

begin
  if FSQL <> '' then
    begin
      BuildSResult;
      if (FManagedFieldDefs.IndexOf('AUTO_ID') = -1) and (FUsersFilter <> '') and FUsePermissions then
        begin
          PJ := ' LEFT JOIN '+QuoteField('PERMISSIONS')+' ON ('+QuoteField('PERMISSIONS')+'.'+QuoteField('REF_ID_ID')+'='+QuoteField(FDefaultTableName)+'.'+QuoteField('SQL_ID')+')';
          PW := ' AND ('+aFilter+') AND (('+FUsersFilter+') OR '+QuoteField('PERMISSIONS')+'.'+QuoteField('USER')+' is NULL)';
        end
      else if (FManagedFieldDefs.IndexOf('AUTO_ID') = -1) and FUsePermissions then
        begin
          PJ := ' LEFT JOIN '+QuoteField('PERMISSIONS')+' ON ('+QuoteField('PERMISSIONS')+'.'+QuoteField('REF_ID_ID')+'='+QuoteField(FDefaultTableName)+'.'+QuoteField('SQL_ID')+')';
          PW := ' AND ('+QuoteField('PERMISSIONS')+'.'+QuoteField('USER')+' is NULL)'
        end;
      PW := StringReplace(PW,'AND ()','',[rfReplaceAll]);
      Result := StringReplace(StringReplace(StringReplace(FSQL,'@PERMISSIONJOIN@',PJ,[]),'@PERMISSIONWHERE@',PW,[]),'@DEFAULTORDER@',SResult,[]);
    end
  else if Assigned(FOrigTable) then
    begin
      Result := 'SELECT ';
      if FDistinct then
        Result := Result+'DISTINCT ';
      if FLimitAfterSelect and ((FLimit > 0)) then
        Result += Format(FLimitSTMT,[FLimit])+' ';
      if FFields = '' then
        Result += QuoteField(FDefaultTableName)+'.'+'* '
      else
        Result += FFields+' ';
      aFilter := FFilter;
      if (FBaseFilter <> '') and (aFilter <> '') then
        aFilter := '('+fBaseFilter+') and ('+aFilter+')'
      else if (FBaseFilter <> '') then
        aFilter := '('+fBaseFilter+')';
      if Assigned(DataSource) then
        begin
          with Self as IBaseManageDb do
            begin
              if ManagedFieldDefs.IndexOf('AUTO_ID') > -1 then
                aRefField := 'AUTO_ID'
              else
                aRefField := 'SQL_ID';
            end;
          if aFilter <> '' then
            aFilter := '('+aFilter+') and ('+QuoteField('REF_ID')+'=:'+QuoteField(aRefField)+')'
          else
            aFilter := QuoteField('REF_ID')+'=:'+QuoteField(aRefField);
        end;
      if (FManagedFieldDefs.IndexOf('AUTO_ID') = -1) and (FUsersFilter <> '') and FUsePermissions then
        Result += 'FROM '+BuildJoins+' LEFT JOIN '+QuoteField('PERMISSIONS')+' ON ('+QuoteField('PERMISSIONS')+'.'+QuoteField('REF_ID_ID')+'='+QuoteField(FDefaultTableName)+'.'+QuoteField('SQL_ID')+') WHERE ('+aFilter+') AND (('+FUsersFilter+') OR '+QuoteField('PERMISSIONS')+'.'+QuoteField('USER')+' is NULL)'
      else if (FManagedFieldDefs.IndexOf('AUTO_ID') = -1) and FUsePermissions then
        Result += 'FROM '+BuildJoins+' LEFT JOIN '+QuoteField('PERMISSIONS')+' ON ('+QuoteField('PERMISSIONS')+'.'+QuoteField('REF_ID_ID')+'='+QuoteField(FDefaultTableName)+'.'+QuoteField('SQL_ID')+') WHERE ('+aFilter+') AND ('+QuoteField('PERMISSIONS')+'.'+QuoteField('USER')+' is NULL)'
      else
        Result += 'FROM '+BuildJoins+' WHERE ('+aFilter+')';
      Result := StringReplace(Result,' WHERE () AND ','WHERE ',[]);
      Result := StringReplace(Result,' WHERE ()','',[]);
      if (FSortFields <> '') and ((FSortDirection <> sdIgnored) or (FBaseSortDirection <> sdIgnored)) then
        begin
          BuildSResult;
          if FUseBaseSorting then
            Result += ' ORDER BY '+Format(FBaseSorting,[sResult])
          else
            Result += ' ORDER BY '+sResult;
        end;
      if (FLimit > 0) and (not FLimitAfterSelect) then
        Result += ' '+Format(FLimitSTMT,[FLimit]);
    end
  else
    Result := SQL.text;
  if Assigned(FOrigTable) then FLastStatement := Result;
end;
function TZeosDBDataSet.IndexExists(IndexName: string): Boolean;
var
  Metadata: TZSQLMetadata;
  CustomQuery: TZQuery;
begin
  CustomQuery := TZQuery.Create(Self);
  CustomQuery.Connection := Connection;
  if (copy(TZConnection(FMainConnection).Protocol,0,8) = 'firebird')
  or (copy(TZConnection(FMainConnection).Protocol,0,9) = 'interbase') then
    begin
      CustomQuery.SQL.Text := 'select rdb$index_name from rdb$indices where rdb$index_name='+QuoteValue(indexname);
      CustomQuery.Open;
      Result := CustomQuery.RecordCount > 0;
      CustomQuery.Close;
    end
  else if (copy(TZConnection(FMainConnection).Protocol,0,6) = 'sqlite') then
    begin
      CustomQuery.SQL.Text := 'select name from SQLITE_MASTER where "TYPE"=''index'' and NAME='+QuoteValue(indexname);
      CustomQuery.Open;
      Result := CustomQuery.RecordCount > 0;
      CustomQuery.Close;
    end
  else if (copy(TZConnection(FMainConnection).Protocol,0,5) = 'mssql') then
    begin
      CustomQuery.SQL.Text := 'select name from dbo.sysindexes where NAME='+QuoteValue(indexname);
      CustomQuery.Open;
      Result := CustomQuery.RecordCount > 0;
      CustomQuery.Close;
    end
  else if (copy(TZConnection(FMainConnection).Protocol,0,8) = 'postgres') then
    begin
      CustomQuery.SQL.Text := 'select * from pg_class where relname='+QuoteValue(indexname);
      CustomQuery.Open;
      Result := CustomQuery.RecordCount > 0;
      CustomQuery.Close;
    end
  else
    begin
      Metadata := TZSQLMetaData.Create(TZConnection(FMainConnection));
      MetaData.Connection := Connection;
      MetaData.MetadataType:=mdIndexInfo;
      Metadata.Catalog:=TZConnection(FMainConnection).Catalog;
      Metadata.TableName:=copy(indexname,0,pos('_',indexname)-1);
      MetaData.Filter:='INDEX_NAME='+QuoteValue(indexname);
      MetaData.Filtered:=True;
      MetaData.Active:=True;
      Result := MetaData.RecordCount > 0;
      MetaData.Free;
    end;
  CustomQuery.Free;
end;

procedure TZeosDBDataSet.WaitForLostConnection;
var
  aConnThere: Boolean;
begin
  if not Ping(Connection) then
    begin
      //if Assigned(OnConnectionLost) then
      //  OnConnectionLost(TZeosDBDM(Owner));
      aConnThere := False;
      while not aConnThere do
        begin
          if GetCurrentThreadID=MainThreadID then
            begin
              //if Assigned(OnDisconnectKeepAlive) then
              //  OnDisconnectKeepAlive(TZeosDBDM(Owner));
            end;
          try
            if Ping(Connection) then aConnThere := True
            else sleep(200);
          except
            sleep(200);
          end;
        end;
      //if Assigned(OnConnect) then
      //  OnConnect(TZeosDBDM(Owner));
    end;
end;

function TZeosDBDataSet.CreateTable : Boolean;
var
  aSQL: String;
  i: Integer;
  bConnection: TZAbstractConnection = nil;
//  bConnection: TZConnection = nil;
  GeneralQuery: TZQuery;
  RestartTransaction: Boolean = False;
begin
  Result := False;
  if Assigned(FOrigTable) and (FFields = '') then
    begin
      if FFields = '' then
        DoCheck := True;
      bConnection := Connection;
      Result := True;
      aSQL := 'CREATE TABLE '+QuoteField(Uppercase(Self.FDefaultTableName))+' ('+lineending;
      if FManagedFieldDefs.IndexOf('AUTO_ID') = -1 then
        aSQL += FieldToSQL('SQL_ID',ftLargeInt,0,True)+' PRIMARY KEY,'+lineending
      else
        begin
          aSQL += FieldToSQL('AUTO_ID',ftLargeInt,0,True)+' PRIMARY KEY,'+lineending;
        end;
      if Assigned(MasterSource) then
        begin
          aSQL += FieldToSQL('REF_ID',ftLargeInt,0,True);
          if FUseIntegrity then
            begin
              with MasterSource.DataSet as IBaseManageDB do
                begin
                  if ManagedFieldDefs.IndexOf('AUTO_ID') = -1 then
                    aSQL += ' REFERENCES '+QuoteField(TZeosDBDataSet(MasterSource.DataSet).DefaultTableName)+'('+QuoteField('SQL_ID')+') ON DELETE CASCADE'
                  else
                    aSQL += ' REFERENCES '+QuoteField(TZeosDBDataSet(MasterSource.DataSet).DefaultTableName)+'('+QuoteField('AUTO_ID')+') ON DELETE CASCADE';
                end;
              if (copy(TZConnection(FMainConnection).Protocol,0,6) = 'sqlite') then
                aSQL += ' DEFERRABLE INITIALLY DEFERRED';
            end;
          aSQL+=','+lineending;
        end;
      for i := 0 to FManagedFieldDefs.Count-1 do
        if FManagedFieldDefs[i].Name <> 'AUTO_ID' then
          aSQL += FieldToSQL(FManagedFieldDefs[i].Name,FManagedFieldDefs[i].DataType,FManagedFieldDefs[i].Size,FManagedFieldDefs[i].Required)+','+lineending;
      aSQL += FieldToSQL('TIMESTAMPD',ftDateTime,0,True)+');';
      try
        try
          GeneralQuery := TZQuery.Create(Self);
          GeneralQuery.Connection := bConnection;
          GeneralQuery.SQL.Text := aSQL;
          GeneralQuery.ExecSQL;
          if bConnection.InTransaction then
            begin
              CommitTransaction(bConnection);
              StartTransaction(bConnection);
            end;
        except
        end;
      finally
        GeneralQuery.Destroy;
      end;
    end;
  Close;
end;
function TZeosDBDataSet.AlterTable: Boolean;
var
  i: Integer;
  aSQL: String;
  GeneralQuery: TZQuery;
  Changed: Boolean;
  aConnection : TZAbstractConnection;
//  aConnection : TZConnection;
begin
  Result := False;
  try
    if FFields <> '' then exit;
    for i := 0 to FManagedFieldDefs.Count-1 do
      if (FieldDefs.IndexOf(FManagedFieldDefs[i].Name) = -1) and (FManagedFieldDefs[i].Name <> 'AUTO_ID') then
        begin
          aSQL := 'ALTER TABLE '+QuoteField(FDefaultTableName)+' ADD '+FieldToSQL(FManagedFieldDefs[i].Name,FManagedFieldDefs[i].DataType,FManagedFieldDefs[i].Size,False)+';';
          aConnection := Connection;
          GeneralQuery := TZQuery.Create(Self);
          try
            GeneralQuery.Connection := aConnection;
            GeneralQuery.SQL.Text := aSQL;
            GeneralQuery.ExecSQL;
          finally
            GeneralQuery.Free;
          end;
          Changed := True;
          Result := True;
        end;
    aSQL := '';
    if Assigned(FManagedIndexDefs) then
      for i := 0 to FManagedIndexDefs.Count-1 do                                           //Primary key
        if (not IndexExists(Uppercase(Self.DefaultTableName+'_'+FManagedIndexDefs.Items[i].Name))) and (FManagedIndexDefs.Items[i].Name <>'SQL_ID') then
          begin
            aSQL := aSQL+'CREATE ';
            if ixUnique in FManagedIndexDefs.Items[i].Options then
              aSQL := aSQL+'UNIQUE ';
            aSQL := aSQL+'INDEX '+QuoteField(Uppercase(Self.DefaultTableName+'_'+FManagedIndexDefs.Items[i].Name))+' ON '+QuoteField(Self.DefaultTableName)+' ('+QuoteField(StringReplace(FManagedIndexDefs.Items[i].Fields,';',QuoteField(','),[rfReplaceAll]))+');'+lineending;
            if aSQL <> '' then
              begin
                try
                  GeneralQuery := TZQuery.Create(Self);
                  GeneralQuery.Connection := Connection;
                  GeneralQuery.SQL.Text := aSQL;
                  GeneralQuery.ExecSQL;
                finally
                  GeneralQuery.Free;
                  aSQL := '';
                end;
              end;
            Result := True;
          end;
  except
    Result := False;
  end;
  //TBaseDBModule(Owner).UpdateTableVersion(Self.FDefaultTableName);
end;
procedure TZeosDBDataSet.InternalOpen;
var
  a: Integer;
begin
  //if Assigned(FOrigTable) then
  //  TBaseDBModule(ForigTable.DataModule).LastTime := GetTicks;
  if IgnoreOpenRequests then exit;
  try
    inherited InternalOpen;
  except
    InternalClose;
    if Ping(Connection) then
      inherited InternalOpen
    else
      begin
        WaitForLostConnection;
        inherited InternalOpen;
      end;
  end;
  try
  if Assigned(FOrigTable) then
    begin
      FOrigTable.SetDisplayLabels(Self);
      if FOrigTable.UpdateFloatFields then
        begin
          DisableControls;
          for a := 0 to Fields.Count -1 do
            begin
              if Fields[a] is TFloatField then
                begin
                  if Fields[a].Name = 'WEIGHT' then
                    begin
                      TFloatField(Fields[a]).DisplayFormat := '#,##0.000##';
                      TFloatField(Fields[a]).EditFormat := '0.000##';
                      TFloatField(Fields[a]).Precision:=5;
                    end
                  else
                    begin
                      TFloatField(Fields[a]).DisplayFormat := '#,##0.00##';
                      TFloatField(Fields[a]).EditFormat := '0.00##';
                      TFloatField(Fields[a]).Precision:=5;
                    end;
                end;
              if (Fields[a] is TDateTimeField)
              or (Fields[a] is TDateField)
              then
                TDateTimeField(Fields[a]).DisplayFormat := ShortDateFormat+' '+ShortTimeFormat;
            end;
          EnableControls;
        end;
    end;
  except
    begin
      FOrigTable:=nil;
      raise;
    end;
  end;
end;

procedure TZeosDBDataSet.InternalRefresh;
begin
  if IgnoreOpenRequests then exit;
  try
    inherited InternalRefresh;
  except
    InternalClose;
    if not Active then
      begin
        if Ping(Connection) then
          InternalOpen
        else
          begin
            WaitForLostConnection;
            InternalOpen;
          end;
      end;
  end;
end;

procedure TZeosDBDataSet.InternalPost;
var
  ok : boolean = false;
  rc : Integer = 0;

  function CheckID : Boolean;
  var
    aDs: TDataSet;
  begin
    if (FieldDefs.IndexOf('AUTO_ID') = -1) and (FieldDefs.IndexOf('SQL_ID') > -1)  then
      begin
        aDs := GetNewDataSetSQL('select '+QuoteField('SQL_ID')+' from '+QuoteField(DefaultTableName)+' where '+QuoteField('SQL_ID')+'='+QuoteValue(FieldByName('SQL_ID').AsVariant));
      end
    else if (FieldDefs.IndexOf('SQL_ID') = -1) and (FieldDefs.IndexOf('AUTO_ID') > -1) then
      begin
        aDs := GetNewDataSetSQL('select '+QuoteField('AUTO_ID')+' from '+QuoteField(DefaultTableName)+' where '+QuoteField('AUTO_ID')+'='+QuoteValue(FieldByName('AUTO_ID').AsVariant));
      end;
    aDs.Open;
    Result := aDs.RecordCount>0;
    aDs.Free;
  end;

  procedure CleanID;
  begin
    if (FieldDefs.IndexOf('AUTO_ID') = -1) and (FieldDefs.IndexOf('SQL_ID') > -1)  then
      begin
        FieldByName('SQL_ID').AsVariant:=Null
      end
    else if (FieldDefs.IndexOf('SQL_ID') = -1) and (FieldDefs.IndexOf('AUTO_ID') > -1) then
      begin
        FieldByName('AUTO_ID').AsVariant:=Null;
      end;
  end;

begin
  while not ok do
    begin
      ok := True;
      try
        inherited InternalPost;
      except
        begin
          inc(rc);
          ok := false;
          if (FHasNewID and (rc<3)) then
            begin
              CleanID;
              SetNewIDIfNull;
              while CheckID do
                begin
                  CleanID;
                  SetNewIDIfNull;
                end;
            end
          else
            begin
              raise;
              exit;
            end;
        end;
      end;
    end;
end;

procedure TZeosDBDataSet.DoAfterInsert;
begin
  inherited DoAfterInsert;
  if Assigned(FOrigTable) then
    begin
      FOrigTable.DisableChanges;
      FOrigTable.FillDefaults(Self);
      FOrigTable.EnableChanges;
    end;
end;
procedure TZeosDBDataSet.DoBeforePost;
begin
  inherited DoBeforePost;
  if Assigned(Self.FOrigTable) then
    Self.FOrigTable.DisableChanges;
  FHasNewID:=False;
  try
  SetNewIDIfNull;
  if FUpStdFields and Assigned(FOrigTable) then
    begin
      if (FieldDefs.IndexOf('TIMESTAMPD') > -1) then
        FieldByName('TIMESTAMPD').AsDateTime:=Now();
      if (FieldDefs.IndexOf('CREATEDBY') > -1) and (FieldByName('CREATEDBY').IsNull) then
        FieldByName('CREATEDBY').AsString:=UserCode;
      if FUpChangedBy and (FieldDefs.IndexOf('CHANGEDBY') > -1) then
        FieldByName('CHANGEDBY').AsString:=UserCode;
    end;
  if Assigned(DataSource) and (FieldDefs.IndexOf('REF_ID')>-1) and  Assigned(FieldByName('REF_ID')) and FieldbyName('REF_ID').IsNull then
    begin
      if DataSource.DataSet.FieldDefs.IndexOf('AUTO_ID') > -1 then
        FieldbyName('REF_ID').AsVariant:=DataSource.DataSet.FieldByName('AUTO_ID').AsVariant
      else
        FieldbyName('REF_ID').AsVariant:=DataSource.DataSet.FieldByName('SQL_ID').AsVariant;
    end;
  finally
    if Assigned(Self.FOrigTable) then
      Self.FOrigTable.EnableChanges;
  end;
end;
procedure TZeosDBDataSet.DoBeforeInsert;
begin
  if Assigned(DataSource) then
    begin
      if (DataSource.State <> dsInsert) and (DataSource.DataSet.RecordCount = 0) then
        begin
          DataSource.DataSet.Append;
        end;
      if (DataSource.DataSet.State = dsInsert) then
        begin
          DataSource.DataSet.Post;
          DataSource.DataSet.Edit;
        end;
    end;
  inherited DoBeforeInsert;
end;

procedure TZeosDBDataSet.DoBeforeEdit;
begin
  inherited DoBeforeEdit;
end;

procedure TZeosDBDataSet.DoBeforeDelete;
begin
  inherited DoBeforeDelete;
  try
    if Assigned(FOrigTable) and Assigned(FOrigTable.OnRemove) then FOrigTable.OnRemove(FOrigTable);
    if GetUpStdFields = True then
      DeleteItem(FOrigTable);
  except
  end;
end;
procedure TZeosDBDataSet.DoAfterDelete;
begin
  inherited DoAfterDelete;
  if Assigned(FOrigTable) then
    FOrigTable.Change;
end;
procedure TZeosDBDataSet.DoAfterScroll;
begin
  inherited DoAfterScroll;
  if Assigned(ForigTable) then
    FOrigTable.UnChange;
end;

procedure TZeosDBDataSet.DoBeforeCancel;
begin
  inherited DoBeforeCancel;
  if State = dsInsert then
    begin
      if Assigned(FOrigTable) and Assigned(FOrigTable.OnRemove) then FOrigTable.OnRemove(FOrigTable);
    end;
end;

function TZeosDBDataSet.GetFields: string;
begin
  Result := FFields;
end;
function TZeosDBDataSet.GetFilter: string;
begin
  Result := FFilter;
end;
function TZeosDBDataSet.GetBaseFilter: string;
begin
  Result := FBaseFilter;
end;
function TZeosDBDataSet.GetLimit: Integer;
begin
  Result := FLimit;
end;
function TZeosDBDataSet.GetSortDirection: TSortDirection;
begin
  Result := FSortDirection;
end;
function TZeosDBDataSet.GetSortFields: string;
begin
  Result := FSortFields;
end;

function TZeosDBDataSet.GetLocalSortFields: string;
begin
  Result := SortedFields;
end;

function TZeosDBDataSet.GetBaseSortFields: string;
begin
  Result := FBaseSortFields;
end;
function TZeosDBDataSet.GetSortLocal: Boolean;
begin
  Result := SortType <> stIgnored;
end;
procedure TZeosDBDataSet.SetFields(const AValue: string);
begin
  FFields := AValue;
  Close;
  SQL.text := BuildSQL;
end;
procedure TZeosDBDataSet.SetFilter(const AValue: string);
begin
  if (FFilter=AValue) and (SQL.text<>'') then exit;
  if CheckForInjection(AValue) then exit;
  FFilter := AValue;
  FSQL := '';
  Close;
  SQL.text := BuildSQL;
end;
procedure TZeosDBDataSet.SetBaseFilter(const AValue: string);
begin
  FBaseFilter := AValue;
  Close;
  SQL.text := BuildSQL;
end;
function TZeosDBDataSet.GetSQL: string;
begin
  Result := FSQL;
end;
procedure TZeosDBDataSet.SetSQL(const AValue: string);
begin
  FSQL := AValue;
  Close;
  SQL.text := BuildSQL;
end;
procedure TZeosDBDataSet.Setlimit(const AValue: Integer);
begin
  if FLimit = AValue then exit;
  FLimit := AValue;
  Close;
  SQL.text := BuildSQL;
end;
procedure TZeosDBDataSet.SetSortDirection(const AValue: TSortDirection);
begin
  FSortDirection := AValue;
  if not GetSortLocal then
    begin
      Close;
      SQL.text := BuildSQL;
    end;
end;
procedure TZeosDBDataSet.SetSortFields(const AValue: string);
begin
  FSortFields := AValue;
end;

procedure TZeosDBDataSet.SetLocalSortFields(const AValue: string);
begin
  SortedFields:=AValue;
end;

procedure TZeosDBDataSet.SetBaseSortFields(const AValue: string);
begin
  FBaseSortFields := AValue;
end;
procedure TZeosDBDataSet.SetSortLocal(const AValue: Boolean);
begin
  if AValue then
    begin
      if FSortDirection = sdAscending then
        SortType := stAscending
      else if FSortDirection = sdDescending then
        SortType := stDescending
      else
        SortType := stIgnored;
    end
  else
    SortType := stIgnored;
end;
function TZeosDBDataSet.GetFilterTables: string;
begin
  Result := FTableNames;
end;
procedure TZeosDBDataSet.SetFilterTables(const AValue: string);
begin
  if AValue = FTableNames then exit;
  FTableNames := AValue;
  Close;
  SQL.text := BuildSQL;
end;
function TZeosDBDataSet.GetUsePermissions: Boolean;
begin
  Result := FUsePermissions;
end;
procedure TZeosDBDataSet.SetUsePermisions(const AValue: Boolean);
begin
  if AValue = FUsePermissions then exit;
  FUsePermissions := AValue;
  Close;
  SQL.text := BuildSQL;
end;
function TZeosDBDataSet.GetDistinct: Boolean;
begin
  Result := FDistinct;
end;
procedure TZeosDBDataSet.SetDistinct(const AValue: Boolean);
begin
  if AValue = FDistinct then exit;
  FDistinct := AValue;
  Close;
  SQL.text := BuildSQL;
end;
function TZeosDBDataSet.GetBaseSorting: string;
begin
  Result := FBaseSorting;
end;
procedure TZeosDBDataSet.SetBaseSorting(AValue: string);
begin
  FBaseSorting := AValue;
end;

function TZeosDBDataSet.GetBaseSortDirection: TSortDirection;
begin
  Result := FBaseSortDirection;
end;
procedure TZeosDBDataSet.SetBaseSortDirection(AValue: TSortDirection);
begin
  FBaseSortDirection := AValue;
end;
function TZeosDBDataSet.GetUseBaseSorting: Boolean;
begin
  Result := FUseBaseSorting;
end;
procedure TZeosDBDataSet.SetUseBaseSorting(AValue: Boolean);
begin
  FUseBaseSorting := AValue;
  SQL.text := BuildSQL;
end;
function TZeosDBDataSet.GetfetchRows: Integer;
begin
  result := FetchRow;
end;
procedure TZeosDBDataSet.SetfetchRows(AValue: Integer);
begin
  FetchRow:=AValue;
end;
function TZeosDBDataSet.GetManagedFieldDefs: TFieldDefs;
begin
  Result := FManagedFieldDefs;
end;
function TZeosDBDataSet.GetManagedIndexDefs: TIndexDefs;
begin
  Result := FManagedIndexDefs;
end;
function TZeosDBDataSet.GetTableName: string;
begin
  Result := FDefaultTableName;
end;
procedure TZeosDBDataSet.SetTableName(const AValue: string);
begin
  FDefaultTableName := AValue;
end;
function TZeosDBDataSet.GetConnection: TComponent;
begin
  Result := Connection;
end;
function TZeosDBDataSet.GetTableCaption: string;
begin
  Result := FTableCaption;
end;
procedure TZeosDBDataSet.SetTableCaption(const AValue: string);
begin
  FTableCaption := AValue;
end;
function TZeosDBDataSet.GetUpStdFields: Boolean;
begin
  Result := FUpStdFields;
end;

procedure TZeosDBDataSet.SetUpStdFields(AValue: Boolean);
begin
  FUpStdFields := AValue;
end;

function TZeosDBDataSet.GetUpChangedBy: Boolean;
begin
  Result := FUpChangedBy;
end;

procedure TZeosDBDataSet.SetUpChangedBy(AValue: Boolean);
begin
  FUpChangedBy:=AValue;
end;

function TZeosDBDataSet.GetUseIntegrity: Boolean;
begin
  Result := FUseIntegrity;
end;
procedure TZeosDBDataSet.SetUseIntegrity(AValue: Boolean);
begin
  FUseIntegrity:=AValue;
end;
procedure TZeosDBDataSet.SetFieldData(Field: TField; Buffer: Pointer);
var
  tmp: String;
begin
  inherited;
  {
  try
    if ((Field.DataType=ftString)
    or (Field.DataType=ftWideString)
    ) and (not FChangeUni)
    then
      begin
        tmp := SysToUni(Field.AsString);
        if tmp <> Field.AsString then
          begin
            FChangeUni := True;
            Field.AsString:=tmp;
            FChangeUni := False;
          end;
      end;
  except
  end;
  }
  if Assigned(FOrigTable) then
    FOrigTable.Change;
end;
function TZeosDBDataSet.GetSubDataSet(aName: string): TBaseDBDataSet;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to FSubDataSets.Count-1 do
    with TBaseDBDataSet(FSubDataSets[i]) as IBaseManageDB do
      if TableName = aName then
        Result := TBaseDBDataSet(FSubDataSets[i]);
end;
procedure TZeosDBDataSet.RegisterSubDataSet(aDataSet: TBaseDBDataSet);
begin
  FSubDataSets.Add(aDataSet);
end;
function TZeosDBDataSet.GetCount: Integer;
begin
  Result := FSubDataSets.Count;
end;
function TZeosDBDataSet.GetSubDataSetIdx(aIdx: Integer): TBaseDBDataSet;
begin
  Result := nil;
  if aIdx < FSubDataSets.Count then
    Result := TBaseDbDataSet(FSubDataSets[aIdx]);
end;
function TZeosDBDataSet.IsChanged: Boolean;
begin
  Result := Modified;
  if Assigned(FOrigTable) then
    Result := ForigTable.Changed;
end;
constructor TZeosDBDataSet.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  DoCheck := False;
  fBaseSorting := '%s';
  FChangeUni:=False;
  FUseBaseSorting:=False;
  FBaseSortDirection:=sdIgnored;
  FManagedFieldDefs := TFieldDefs.Create(Self);
  FManagedIndexDefs := TIndexDefs.Create(Self);
  FSubDataSets := TList.Create;
  FUsePermissions := False;
  Options:= [doCalcDefaults, doAlwaysDetailResync, doDontSortOnPost, doPreferPrepared{, doPreferPreparedResolver}];
  FOrigTable := nil;
  SortType := stIgnored;
  FUpStdFields := True;
  FUpChangedBy := True;
  FUseIntegrity:=False;//disable for sync
end;
destructor TZeosDBDataSet.Destroy;
begin
  //TODO: Free Subdatasets ??
  FManagedFieldDefs.Free;
  FManagedIndexDefs.Free;
  FSubDataSets.Free;
  try
    inherited Destroy;
  except
  end;
end;

procedure TZeosDBDataSet.DoExecSQL;
begin
  ExecSQL;
end;

function TZeosDBDataSet.NumRowsAffected: Integer;
begin
  Result := RowsAffected;
end;


end.

