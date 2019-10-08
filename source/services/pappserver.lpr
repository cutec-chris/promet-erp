program pappserver;
uses Classes, SysUtils, CustApp, ubasedbclasses, general_nogui, LazFileUtils,
  mORMot,mORMotDB,mORMotSQLite3,SynSQLite3,SynDBZeos,uEncrypt,SynDB,SynCommons;

type

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
  {
  Result := TSynConnectionDefinition.Create;
  Result.Kind:='TSQLDBZEOSConnectionProperties';
  Result.DatabaseName:=Database;
  Result.ServerName:=Protocol+'://'+HostName+':'+IntToStr(Port);
  Result.User:=User;
  Result.PasswordPlain:=Password;
  }
  Result := TSQLDBZEOSConnectionProperties.Create(Protocol+'://'+HostName+':'+IntToStr(Port),Database,User,Password);
  Properties.Free;
end;

constructor TProcessManager.Create(TheOwner: TComponent);
var
  Model: TSQLModel;
  FDB: TSQLRestServerDB;
  ConfigPath, Mandant: String;
  ConfigFile: TStringList;
  Props: TSQLDBConnectionProperties;
begin
  inherited Create(TheOwner);
  Model := TSQLModel.Create([TUser]);
  ConfigPath := GetOptionValue('config-path');
  if ConfigPath = '' then ConfigPath:=AppendPathDelim(GetAppConfigDir(True))+DirectorySeparator+'prometerp';
  ConfigPath := AppendPathDelim(ConfigPath);
  Mandant := GetOptionValue('mandant');
  if Mandant = '' then Mandant := 'Standard';
  ConfigFile := TStringList.Create;
  ConfigFile.LoadFromFile(ConfigPath+Mandant+'.perml');
  Props := AddConnection(ConfigFile[1]);
  ConfigFile.Free;
  VirtualTableExternalRegister(Model, [TUser], Props);
  SQLite3 := TSQLite3LibraryDynamic.Create;
  FDB := TSQLRestServerDB.Create(Model, ':memory:');
  FDB.CreateMissingTables;
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

