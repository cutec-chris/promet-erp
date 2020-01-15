program pappserver;
uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils, CustApp, general_nogui,
  uBaseDatasetInterfaces2, pprometdbintfs,LazFileUtils,ubasedbclasses,
  fpjson,fpjsonrtti1,SQLite3Conn;

type
  { TProcessManager }
  TProcessManager = class(TCustomApplication)
  private
  protected
    procedure DoRun; override;
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
  sleep(1);
end;
{
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
}
constructor TProcessManager.Create(TheOwner: TComponent);
var
  ConfigPath, Mandant: String;
  ConfigFile: TStringList;
  i: Integer;
  User: TUser;
  aJson: TJSONObject;
  Streamer: TJSONStreamer;
  JSONString: TJSONStringType;
  Opt: TOption;
begin
  inherited Create(TheOwner);
  writeln('connecting...');
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
  User := TUser.Create;
  User.NAME := 'It´se me, Mario';
  Opt := TOption.Create;
  Opt.OPTION:='TEST';
  Opt.VALUE:='Jeah';
  User.Options.Add(Opt);
  User.Options.Add(Opt);
  User.Options.Items[0].OPTION:='x';
  Streamer := TJSONStreamer.Create(nil);
  Streamer.Options:=Streamer.Options + [jsoStreamTlist];
  for i := 1 to 1000 do
    JSONString := Streamer.ObjectToJSONString(User);
  WriteLn(JSONString);
  Streamer.Free;
  {
  aObj := TOp
  uBaseDatasetInterfaces.Data := AddConnection(ConfigFile[1]);
  ConfigFile.Free;
  for i := 0 to length(Model.Tables)-1 do
    if Model.Tables[i].InheritsFrom(TBaseDBDataset) then
      TBaseDBDataset(Model.Tables[i]).DefineTable(Model);
  SQLite3 := TSQLite3LibraryDynamic.Create;
  FDB := TSQLRestServerDB.Create(Model, ':memory:');
  FDB.DB.Synchronous := smOff;
  //FDB.AcquireExecutionMode[execORMGet] := amBackgroundThread;
  //FDB.AcquireExecutionMode[execORMWrite] := amBackgroundThread;
  writeln('checking database structure...');
  FDB.CreateMissingTables(0,[itoNoAutoCreateGroups, itoNoAutoCreateUsers,itoNoCreateMissingField]);
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
  }
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

