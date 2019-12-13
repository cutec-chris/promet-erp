program pappserver;
uses Classes, SysUtils, CustApp, ubasedbclasses, general_nogui, LazFileUtils,
  mORMot,mORMotDB,mORMotSQLite3,SynSQLite3,SynDBZeos,uEncrypt,SynDB,SynCommons,
  mORMotHttpServer;

type

  { TSQLDBPrometConnectionProperties }

  TSQLDBPrometConnectionProperties = class(TSQLDBZEOSConnectionProperties)
  public
    function SQLGetField(const aTableName: RawUTF8): RawUTF8; override;
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

function TSQLDBPrometConnectionProperties.SQLGetField(const aTableName: RawUTF8
  ): RawUTF8;
begin
  Result:='"'+Uppercase(inherited SQLGetField(aTableName))+'"';
end;

{ TProcessManager }

procedure TProcessManager.DoRun;
begin
  inherited DoRun;
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
  Props: TSQLDBConnectionProperties;
  HttpServer: TSQLHttpServer;
  aMapping: PSQLRecordPropertiesMapping;
begin
  inherited Create(TheOwner);
  Model := TSQLModel.Create([TUser]);
  Model.Root:='promet';
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
  Props := AddConnection(ConfigFile[1]);
  ConfigFile.Free;
  aMapping := VirtualTableExternalMap(Model,TUser,Props,'"USERS"');
  aMapping^.MapFields(['ID','"SQL_ID"']); // no ID/RowID for our aggregates
  aMapping^.MapFields(['TYP','"TYPE"']); // no ID/RowID for our aggregates
  //VirtualTableExternalRegister(Model, [TUser], Props);
  SQLite3 := TSQLite3LibraryDynamic.Create;
  FDB := TSQLRestServerDB.Create(Model, ':memory:');
  FDB.CreateMissingTables;
  try
    HttpServer := TSQLHttpServer.Create('8085', [FDB],'+', useBidirSocket);
    HttpServer.AccessControlAllowOrigin := '*'
  except
    on e : exception do
      begin
        writeln(e.Message);
        exit;
      end;
  end;
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

