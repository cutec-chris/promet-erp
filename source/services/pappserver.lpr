program pappserver;
uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils, CustApp, ubasedbclasses, general_nogui, LazFileUtils,
  mORMot,mORMotDB,mORMotSQLite3,SynSQLite3,SynDBZeos,uEncrypt,SynDB,SynCommons,
  mORMotHttpServer,uBaseDatasetInterfaces,SynLog;

type

  { TSQLDBPrometConnectionProperties }

  TSQLDBPrometConnectionProperties = class(TSQLDBZEOSConnectionProperties)
  public
    function SQLFieldCreate(const aField: TSQLDBColumnCreate;
      var aAddPrimaryKey: RawUTF8): RawUTF8; override;
  end;

  { TProcessManager }

  TProcessManager = class(TCustomApplication)
  protected
    procedure DoRun; override;
    function AddConnection(aProp: string): TSQLDBConnectionProperties;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;
var
  Application: TProcessManager;

function TSQLDBPrometConnectionProperties.SQLFieldCreate(
  const aField: TSQLDBColumnCreate; var aAddPrimaryKey: RawUTF8): RawUTF8;
begin
  Result:=inherited SQLFieldCreate(aField, aAddPrimaryKey);
end;

{ TProcessManager }

procedure TProcessManager.DoRun;
begin
  inherited DoRun;
  sleep(1);
end;

function TProcessManager.AddConnection(aProp: string): TSQLDBConnectionProperties;
var
  Port: Integer;
  Properties : TStringList;
  Protocol, User, Password, HostName, Database, tmp, FDatabaseDir: String;
  FEData: Boolean;
begin
  Properties := TStringList.Create;
  Port:=0;
  Properties.Clear;
  Properties.Add('timeout=3');
  Protocol:='';
  User:='';
  Password:='';
  HostName:='';
  Database:='';
  tmp := aProp;
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
  Result := TSQLDBPrometConnectionProperties.Create(Protocol+'://'+HostName+':'+IntToStr(Port),Database,User,Password);
  Properties.Free;
end;

constructor TProcessManager.Create(TheOwner: TComponent);
var
  Model: TSQLModel;
  FDB: TSQLRestServerDB;
  ConfigPath, Mandant: String;
  ConfigFile: TStringList;
  HttpServer: TSQLHttpServer;
  aMapping: PSQLRecordPropertiesMapping;
  i: Integer;
begin
  inherited Create(TheOwner);
  writeln('connecting...');
  Model := TSQLModel.Create([TUser,TRights,TActiveUsers,TAuthSources,TOptions{,TUserfielddefs,TNumbersets,TNumberRanges,TNumberPools,TPayGroups}]);
  Model.Root:='promet';
  with TSQLLog.Family do begin
     Level := LOG_STACKTRACE+[sllEnter];
     EchoToConsole := LOG_STACKTRACE+[sllInfo,sllEnter,sllLeave,sllClient]; // log all events to the console
   end;
  ConfigPath := GetOptionValue('config-path');
  if ConfigPath = '' then ConfigPath:=AppendPathDelim(GetAppConfigDir(True))+'prometerp';
  ConfigPath := AppendPathDelim(ConfigPath);
  Mandant := GetOptionValue('mandant');
  if Mandant = '' then Mandant := 'Standard';
  ConfigFile := TStringList.Create;
  try
    ConfigFile.LoadFromFile(ConfigPath+Mandant+'.perml');
  except
    on e : exception do
      begin
        writeln(e.Message);
        exit;
      end;
  end;
  uBaseDatasetInterfaces.Data := AddConnection(ConfigFile[1]);
  ConfigFile.Free;
  for i := 0 to length(Model.Tables)-1 do
    TBaseDBDataset(Model.Tables[i]).DefineTable(Model);
  SQLite3 := TSQLite3LibraryDynamic.Create;
  FDB := TSQLRestServerDB.Create(Model, ':memory:');
  FDB.DB.Synchronous := smOff;
  writeln('checking database structure...');
  FDB.CreateMissingTables(0,[itoNoAutoCreateGroups, itoNoAutoCreateUsers]);
  try
    HttpServer := TSQLHttpServer.Create('8085', [FDB],'+', useBidirSocket);
    //HttpServer.AccessControlAllowOrigin := '*'
  except
    on e : exception do
      begin
        writeln(e.Message);
        exit;
      end;
  end;
  writeln('...done.');
end;

destructor TProcessManager.Destroy;
begin
  inherited Destroy;
end;

begin
  Application := TProcessManager.Create(nil);
  Application.Run;
  Application.Free;
end.

