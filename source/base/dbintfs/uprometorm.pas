unit uPrometORM;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TypInfo, SQLDB, Rtti,Contnrs,memds,
  MSSQLConn,
  SQLite3Conn,
  PQConnection,
  ubasedatasetinterfaces2;

type
  TContext = record
    Transaction : TSQLTransaction;
    Id : TThreadID;
  end;

  { TLockedQuery }

  TLockedQuery = class
  private
    cs : TRTLCriticalSection;
  public
    Query : TSQLQuery;
    constructor Create(aQuery : TSQLQuery);
    destructor Destroy; override;
    procedure Lock;
    procedure Unlock;
  end;

  { TQueryTable }

  TQueryTable = class(TObjectList)
  private
    FFields: TStrings;
    FTableName: string;
    function GetTable(Table : Integer): TQueryTable;
    procedure SetTableName(AValue: string);
  public
    constructor Create(aClass : TClass);
    destructor Destroy; override;
    property Fields : TStrings read FFields;
    property TableName : string read FTableName write SetTableName;
    property SubTables[Table : Integer] : TQueryTable read GetTable;
    function BuildSelect(aFilter,aFields : string) : string;
  end;

  { TSQLDBDataModule }

  TSQLDBDataModule = class(TComponent)
  private
    FMandants: TStringList;
    FTables : array of TQueryTable;
    MainConnection : TSQLConnection;
    function GetMandants: TStringList;
    function GetTable(aClass: TClass): TQueryTable;
  public
    Querys : array of TLockedQuery;
    Contexts : array of TContext;
    ConfigPath : string;
    Mandant : string;
    procedure Connect;
    function GetConnection(ConnectString : string) : TSQLConnection;
    function FindDataSet(aThreadId : TThreadID;SQL : string) : TLockedQuery;
    //generates SQL to Fill all Published properties and Gerneric TFPGList Types
    //(generates recursive joined Query for default TFPGList Type (or if only one is avalible) and separate Querys for all other)
    //only when this query fails the table structure for all sub-tables is checked so without changes of the table structure we dont have overhead
    procedure Load(Obj: TPersistent; Selector: Variant; Cascadic: Boolean);
    //Generates recursive an update Statement per record if SQL_ID is filled or n insert stetement if not
    procedure Save(Obj: TPersistent; Selector: Variant; Cascadic: Boolean);
    function Select(Obj: TClass; aFilter: string; aFields: string): TMemDataset;

    property Mandants : TStringList read GetMandants;
    destructor Destroy; override;
  end;

var
  //Globally used Querylist to hold all Update/Insert Querys globally prepared avalible
  Data : TSQLDBDataModule;

implementation

uses uEncrypt;

{ TQueryTable }

procedure TQueryTable.SetTableName(AValue: string);
begin
  if FTableName=AValue then Exit;
  FTableName:=AValue;
end;

function TQueryTable.GetTable(Table : Integer): TQueryTable;
begin
  Result := Items[Table] as TQueryTable;
end;

constructor TQueryTable.Create(aClass: TClass);
  procedure ListClassProperties(Obj: TClass;Prefix : string);
  var
    ctx: TRttiContext;
    objType: TRttiType;
    Prop: TRttiProperty;
    aTyp, bTyp: TClass;
  begin
    ctx := TRttiContext.Create;
    objType := ctx.GetType(Obj.ClassInfo);
     for Prop in objType.GetProperties do
       begin
         Fields.Add(Prop.Name);
         if (Prop.PropertyType.TypeKind=tkClass) then
           begin
             if TRttiInstanceType(Prop.PropertyType.BaseType).MetaClassType=TAbstractMasterDetail then
               begin
                 aTyp := TRttiInstanceType(Prop.PropertyType).MetaClassType;
                 try
                   bTyp := TAbstractMasterDetail(aTyp).GetObjectTyp;
                   Add(TQueryTable.Create(bTyp))
                 except
                   //on e : exception do
                   //  debugln(e.message);
                 end;
               end;
           end;
       end;
  end;
begin
  inherited Create(True);
  FFields := TStringList.Create;
  FTableName:=copy(aClass.ClassName,2,length(aClass.ClassName));
  ListClassProperties(aClass,copy(aClass.ClassName,2,length(aClass.ClassName)));
end;

destructor TQueryTable.Destroy;
begin
  FFields.Free;
  inherited Destroy;
end;

function TQueryTable.BuildSelect(aFilter, aFields: string): string;
begin

end;

{ TSQLDBDataModule }

function TSQLDBDataModule.GetMandants: TStringList;
begin
  if not Assigned(FMandants) then
    FMandants := TStringList.Create;
  if FMandants.Count=0 then
    begin

    end;
end;

function TSQLDBDataModule.GetTable(aClass : TClass): TQueryTable;
var
  i: Integer;
begin
  for i := 0 to length(FTables)-1 do
    if FTables[i].TableName=copy(aClass.ClassName,2,length(aClass.ClassName)) then
      begin
        Result := FTables[i];
        exit;
      end;
  Result := TQueryTable.Create(aClass);
  Setlength(FTables,length(FTables)+1);
  FTables[length(Ftables)-1] := Result;
end;

procedure TSQLDBDataModule.Connect;
var
  ConfigFile: TStringList;
  Port: Integer;
  Properties : TStringList;
  Protocol, User, Password, HostName, Database, tmp, FDatabaseDir: String;
  FEData: Boolean;
begin
  ConfigFile := TStringList.Create;
  ConfigFile.LoadFromFile(ConfigPath+Mandant+'.perml');
  try
    MainConnection := GetConnection(ConfigFile[1]);
    MainConnection.Connected:=True;
  finally
    ConfigFile.Free;
  end;
end;

function TSQLDBDataModule.GetConnection(ConnectString: string): TSQLConnection;
var
  Port: Integer;
  Properties : TStringList;
  Protocol, User, Password, HostName, Database, tmp, FDatabaseDir: String;
  FEData: Boolean;
begin
  Properties := TStringList.Create;
  Port:=0;
  Properties.Clear;
  Protocol:='';
  User:='';
  Password:='';
  HostName:='';
  Database:='';
  tmp := ConnectString;
  if copy(tmp,0,pos(';',tmp)-1) <> 'sqlite-3-edata' then
    Protocol:=copy(tmp,0,pos(';',tmp)-1)
  else
    begin
      Protocol:='sqlite-3';
      FEData:=True;
    end;
  //Assert(Protocol<>'',strUnknownDbType);
  tmp := copy(tmp,pos(';',tmp)+1,length(tmp));
  HostName := copy(tmp,0,pos(';',tmp)-1);
  if pos(':',HostName) > 0 then
    begin
      Port:=StrToInt(copy(HostName,pos(':',HostName)+1,length(HostName)));
      HostName:=copy(HostName,0,pos(':',HostName)-1);
    end
  else if pos('/',HostName) > 0 then
    begin
      Port:=StrToInt(copy(HostName,pos('/',HostName)+1,length(HostName)));
      HostName:=copy(HostName,0,pos('/',HostName)-1);
    end;
  tmp := copy(tmp,pos(';',tmp)+1,length(tmp));
  Database:=copy(tmp,0,pos(';',tmp)-1);
  FDatabaseDir:=ExtractFileDir(ExpandFileName(Database));
  tmp := copy(tmp,pos(';',tmp)+1,length(tmp));
  User := copy(tmp,0,pos(';',tmp)-1);
  tmp := copy(tmp,pos(';',tmp)+1,length(tmp));
  if copy(tmp,0,1) = 'x' then
    Password := Decrypt(copy(tmp,2,length(tmp)),word(99998))
  else
    Password := tmp;
  Result := TSQLConnector.Create(Self);
  if copy(Protocol,0,10)='postgresql' then
    TSQLConnector(Result).ConnectorType:='PostgreSQL'
  else if copy(Protocol,0,6)='sqlite' then
    TSQLConnector(Result).ConnectorType:='SQLite3'
  else if copy(Protocol,0,5)='mssql' then
    TSQLConnector(Result).ConnectorType:='MSSQLServer'
  else
    raise Exception.Create('Unknown Database Server Type');
  TSQLConnector(Result).HostName:=HostName;
  TSQLConnector(Result).DatabaseName:=Database;
  TSQLConnector(Result).UserName:=User;
  TSQLConnector(Result).Password:=Password;
  TSQLConnector(Result).Params.Assign(Properties);
  TSQLConnector(Result).Params.Add('port='+IntToStr(Port));
  TSQLConnector(Result).Params.Add('application_name=''Avamm''');;
  Properties.Free;
end;

function TSQLDBDataModule.FindDataSet(aThreadId: TThreadID; SQL: string
  ): TLockedQuery;
begin
  Result := nil;
end;

procedure TSQLDBDataModule.Load(Obj: TPersistent;Selector : Variant; Cascadic: Boolean);
var
  aTable: TQueryTable;
begin
  aTable := GetTable(Obj.ClassType);
end;

procedure TSQLDBDataModule.Save(Obj: TPersistent;Selector : Variant; Cascadic: Boolean);
var
  aTable: TQueryTable;
begin
  aTable := GetTable(Obj.ClassType);
end;

function TSQLDBDataModule.Select(Obj: TClass; aFilter: string; aFields: string
  ): TMemDataset;
var
  aTable: TQueryTable;
  aDataSet: TLockedQuery;
begin
  aTable := GetTable(Obj);
  aDataSet := FindDataSet(ThreadID,aTable.BuildSelect(aFilter,aFields));
  aDataSet.Query.Open;
  Result := TMemDataset.Create(nil);
  Result.CopyFromDataset(aDataSet.Query);
  aDataSet.Query.Close;
  aDataSet.Unlock;
end;

destructor TSQLDBDataModule.Destroy;
begin
  FreeAndNil(MainConnection);
  inherited Destroy;
end;

{ TLockedQuery }

constructor TLockedQuery.Create(aQuery: TSQLQuery);
begin
  InitCriticalSection(cs);
  Query := aQuery;
end;

destructor TLockedQuery.Destroy;
begin
  DoneCriticalSection(cs);
  inherited Destroy;
end;

procedure TLockedQuery.Lock;
begin
  EnterCriticalSection(cs);
end;

procedure TLockedQuery.Unlock;
begin
  LeaveCriticalSection(cs);
end;

initialization
  Data := TSQLDBDataModule.Create(nil);
finalization
  FreeAndNil(Data);
end.

