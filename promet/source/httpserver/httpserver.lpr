program httpserver;
{$mode objfpc}{$H+}
uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, pcmdprometapp, CustApp, uBaseCustomApplication, lnetbase,
  lNet, uBaseDBInterface, md5,uData,eventlog,
  fileutil,lconvencoding,uBaseApplication,lhttp,lwebserver,uSessionDBClasses,
  lEvents;
type
  TDataHandler = class(TURIHandler)
  protected
    function HandleURI(ASocket: TLHTTPServerSocket): TOutputItem; override;
  public
  end;
  TPHTTPServer = class(TBaseCustomApplication)
    procedure OnError(const msg: string; aSocket: TLSocket);
    procedure ServerAccess(AMessage: string);
    procedure ServerLog(aSocket: TLSocket; DirectionIn: Boolean;
      aMessage: string);
    function ServerLogin(aSocket: TLSocket; aUser, aPasswort: string
      ): Boolean;
  private
    Server: TLHTTPServer;
    FileHandler: TFileHandler;
    CGIHandler: TCGIHandler;
    DataHandler: TDataHandler;
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;
function TDataHandler.HandleURI(ASocket: TLHTTPServerSocket): TOutputItem;
var
  URL: string;
begin
  Result := nil;
  URL := ASocket.FRequestInfo.Argument;
  if copy(url,0,7) = 'promet/' then //special range with promet functionality we need session management here
    begin

//      aStream := TMemoryStream.Create;
//      Result := TStreamOutput.Create(ASocket,aStream,True);

    end;
end;
procedure TPHTTPServer.OnError(const msg: string; aSocket: TLSocket);
begin
  Writeln(msg);
end;
procedure TPHTTPServer.ServerAccess(AMessage: string);
begin
  Writeln(AMessage);
end;
procedure TPHTTPServer.ServerLog(aSocket: TLSocket; DirectionIn: Boolean;
  aMessage: string);
begin
  Writeln(aMessage);
end;

function TPHTTPServer.ServerLogin(aSocket: TLSocket; aUser,
  aPasswort: string): Boolean;
begin
  Result := False;
  with Self as IBaseDBInterface do
    begin
      if Data.Users.DataSet.Locate('LOGINNAME',aUser,[]) then
        begin
          if (Data.Users.CheckPasswort(aPasswort)) then
            Result := True;
        end;
    end;
  with Self as IBaseApplication do
    begin
      if Result then
        Log('Login:'+aUser)
      else
        Error('Login failed:'+aUser);
    end;
end;
procedure TPHTTPServer.DoRun;
begin
  with Self as IBaseDBInterface do
    begin
      DBLogout;
      if not Login then exit;
    end;
  if Server.Listen(8080) then
    while not Terminated do Server.CallAction;
  // stop program loop
  Terminate;
end;

constructor TPHTTPServer.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  Server := TLHTTPServer.Create(nil);
  Server.ServerSoftware := 'phttpd/0.1';
//  Server.TimeOut := -1;
  Server.OnError:=@OnError;
  Server.OnAccess:=@ServerAccess;

  DataHandler := TDataHandler.Create;

  FileHandler := TFileHandler.Create;
  FileHandler.MimeTypeFile := 'mime.types';
  FileHandler.DocumentRoot := 'httpdocs';

  CGIHandler := TCGIHandler.Create;
  CGIHandler.FCGIRoot := 'cgi-bin';
  CGIHandler.FDocumentRoot := 'httpdocs';
  CGIHandler.FEnvPath := 'cgi-bin';
  CGIHandler.FScriptPathPrefix := '/cgi-bin';

  Server.RegisterHandler(DataHandler);
  Server.RegisterHandler(FileHandler);
  Server.RegisterHandler(CGIHandler);

  FileHandler.DirIndexList.Add('index.html');
  FileHandler.DirIndexList.Add('index.htm');
  FileHandler.DirIndexList.Add('index.cgi');
end;

destructor TPHTTPServer.Destroy;
begin
  Server.Free;
  DataHandler.Free;
  FileHandler.Free;
  CGIHandler.Free;
  inherited Destroy;
end;

var
  Application: TPHTTPServer;

{$R *.res}

begin
  Application:=TPHTTPServer.Create(nil);
  Application.Run;
  Application.Free;
end.
