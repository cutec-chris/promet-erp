unit upwebdavserver;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, dom, xmlread, xmlwrite, uappserverhttp,udavserver,uAppServer,
  uprometdavserver,uData,uBaseApplication;

type

  { TPrometWebDAVMaster }

  TPrometWebDAVMaster = class(TWebDAVMaster)
    procedure ServerAccess(aSocket: TDAVSession; Info: string);
  private
    ServerFunctions : TPrometServerFunctions;
  public
    constructor Create;override;
    destructor Destroy; override;
  end;

implementation

var
  DavServer : TPrometWebDAVMaster = nil;

function HandleDAVRequest(Sender : TAppNetworkThrd;Method, URL: string;SID : string;Parameters,Headers : TStringList;Input,Output : TMemoryStream;ResultStatusText : string): Integer;
var
  i: Integer;
  aSock: TDAVSession = nil;
  s: String;
  tmp: String;
  sl: TStringList;
begin
  Result := 404;
  ResultStatusText := '';
  if BaseApplication.HasOption('nodav') then exit;
  if pos('/configuration/',Url)=1 then exit;
  if pos('/wiki/',Url)=1 then exit;
  if not Assigned(uData.Data) then exit;
  try
    if not Assigned(DavServer) then
      begin
        DavServer := TPrometWebDAVMaster.Create;
      end;
    for i := 0 to Sender.Objects.Count-1 do
      if TObject(Sender.Objects[i]) is TDAVSession then
        aSock := TDAVSession(Sender.Objects[i]);
    if not Assigned(aSock) then
      begin
        aSock := TDAVSession.Create(DavServer);
        aSock.Socket := Sender;
        Sender.Objects.Add(aSock);
      end;
    aSock.Parameters.Clear;
    for i := 0 to Headers.Count-1 do
      begin
        s := Headers[i];
        tmp := copy(s,0,pos(':',s)-1);
        aSock.Parameters.Add(lowercase(tmp)+':'+trim(copy(s,pos(':',s)+1,length(s))));
      end;
    Result := aSock.ProcessHttpRequest(Method,URL,Headers,Input,Output);
    ResultStatusText:=aSock.ResultStatus;
  except
    on e : Exception do
      begin
        Result := 500;
        sl := TStringList.Create;
        sl.Add(e.Message);
        sl.SaveToStream(Output);
        sl.Free;
      end;
  end;
end;

procedure TPrometWebDAVMaster.ServerAccess(aSocket: TDAVSession; Info: string);
begin
  writeln('DAV:'+Info);
end;

constructor TPrometWebDAVMaster.Create;
begin
  inherited Create;
  ServerFunctions := TPrometServerFunctions.Create;
  OnAccess:=@ServerAccess;
  OnGetDirectoryList:=@ServerFunctions.ServerGetDirectoryList;
  OnMkCol:=@ServerFunctions.ServerMkCol;
  OnDelete:=@ServerFunctions.ServerDelete;
  OnMove:=@ServerFunctions.ServerMove;
  OnPutFile:=@ServerFunctions.ServerPutFile;
  OnPostFile:=@ServerFunctions.ServerPutFile;
  OnGetFile:=@ServerFunctions.ServerGetFile;
  OnReadAllowed:=@ServerFunctions.ServerReadAllowed;
  OnWriteAllowed:=@ServerFunctions.ServerReadAllowed;
  OnUserLogin:=@ServerFunctions.ServerUserLogin;
  OnLock:=@ServerFunctions.DoLock;
  OnUnLock:=@ServerFunctions.DoUnlock;
end;

destructor TPrometWebDAVMaster.Destroy;
begin
  ServerFunctions.Free;
  inherited Destroy;
end;

initialization
  uappserverhttp.RegisterHTTPHandler(@HandleDAVRequest);
finalization
  FreeAndNil(DavServer);
end.

