unit uPrometORM;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TypInfo, SQLDB,
  MSSQLConn,
  SQLite3Conn,
  PQConnection,
  ubasestreamer;

type
  TContext = record
    Transaction : TSQLTransaction;
    Id : TThreadID;
  end;

  { TSQLStreamer }

  TSQLStreamer = class(TBaseStreamer)
  private
    FContext : TContext;
    FFilter : string;
  public
    //we try to use this class with the same transaction/connection the whole time
    //so its added during Constructor when using it in another thread, it should be used with another Transaction
    constructor Create(Context : TThreadID);
    //generates SQL to Fill all Published properties and Gerneric TFPGList Types
    //(generates recursive joined Query for default TFPGList Type (or if only one is avalible) and separate Querys for all other)
    //only when this query fails the table structure for all sub-tables is checked so without changes of the table structure we dont have overhead
    procedure Load(Cascadic : Boolean);override;
    //Generates recursive an update Statement per record if SQL_ID is filled or n insert stetement if not
    procedure Save(Cascadic : Boolean);override;
    function Select(aFilter: string): Integer; overload; override;
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

  { TSQLDBDataModule }

  TSQLDBDataModule = class(TComponent)
  private
    FMandants: TStringList;
    MainConnection : TSQLConnection;
    function GetMandants: TStringList;
  public
    Querys : array of TLockedQuery;
    Contexts : array of TContext;
    ConfigPath : string;
    Mandant : string;
    procedure Connect;
    function GetConnection(ConnectString : string) : TSQLConnection;
    property Mandants : TStringList read GetMandants;
    destructor Destroy; override;
  end;

var
  //Globally used Querylist to hold all Update/Insert Querys globally prepared avalible
  Data : TSQLDBDataModule;

implementation

uses uEncrypt;

{ TSQLDBDataModule }

function TSQLDBDataModule.GetMandants: TStringList;
begin
  if not Assigned(FMandants) then
    FMandants := TStringList.Create;
  if FMandants.Count=0 then
    begin

    end;
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

{ TSQLStreamer }

constructor TSQLStreamer.Create(Context: TThreadID);
var
  i: Integer;
begin
  i := 0;
  while i < length(Data.Contexts) do
    begin
      if Data.Contexts[i].Id = Context then
        begin
          FContext := Data.Contexts[i];
          exit;
        end;
      inc(i);
    end;
  FContext.Id:=Context;
  FContext.Transaction := TSQLTransaction.Create(nil);
  Setlength(Data.Contexts,length(Data.Contexts)+1);
  Data.Contexts[length(Data.Contexts)-1] := FContext;
  FContext.Transaction.DataBase := Data.MainConnection;
end;

procedure TSQLStreamer.Load(Cascadic: Boolean);
begin

end;

procedure TSQLStreamer.Save(Cascadic: Boolean);
begin

end;

function TSQLStreamer.Select(aFilter: string): Integer;
begin
  FFilter := aFilter;
end;

initialization
  Data := TSQLDBDataModule.Create(nil);
finalization
  FreeAndNil(Data);
end.

