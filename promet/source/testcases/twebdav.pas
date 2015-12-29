unit twebdav;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry,udavserver,
  Sockets,uhttputil,uprometdavserver;

type

  { TWebDAVTest }

  TWebDAVTest= class(TTestCase)
  private
    function SendRequest(aRequest: string): string;
  published
    procedure SetUp; override;
    procedure PropfindRoot;
    procedure FindPrincipal;
    procedure OptionsDepth0;
  end;

  { TestSocket }

  TestSocket = class(TDAVSocket)
  public
    constructor Create(hsock: tSocket); override;
  end;

implementation

var
  Socket : TestSocket;
  aReq : TStringList;
  Server : TWebDAVServer;
  ServerFunctions: TPrometServerFunctions;

{ TestSocket }

constructor TestSocket.Create(hsock: tSocket);
begin
  Headers := TStringList.Create;
  Parameters := TStringList.Create;
  Parameters.NameValueSeparator:=':';
  InputData := TMemoryStream.Create;
  OutputData := TMemoryStream.Create;
end;

function TWebDAVTest.SendRequest(aRequest: string): string;
var
  aURL: String;
begin
  aReq.Text:=aRequest;
  aURL := aReq[0];
  aReq.Delete(0);
  Socket.Headers.Clear;
  while (aReq.Count>0) and (aReq[0]<>'') do
    begin
      Socket.Headers.Add(aReq[0]);
      aReq.Delete(0);
    end;
  aReq.Delete(0);
  aReq.SaveToStream(Socket.InputData);
  Socket.ProcessHttpRequest(copy(aURL,0,pos(' ',aURL)-1),copy(aURL,pos(' ',aURL)+1,length(aURL)));
  Result := IntToStr(Socket.Status)+LineEnding+MemoryStreamToString(Socket.OutputData);
end;

procedure TWebDAVTest.SetUp;
begin
  inherited SetUp;
  aReq := TStringList.Create;
  Socket := TestSocket.Create(0);
  Server := TWebDAVServer.Create;
  Socket.Creator:=Server;
  ServerFunctions := TPrometServerFunctions.Create;
  Server.OnGetDirectoryList:=@ServerFunctions.ServerGetDirectoryList;
  Server.OnMkCol:=@ServerFunctions.ServerMkCol;
  Server.OnDelete:=@ServerFunctions.ServerDelete;
  Server.OnPutFile:=@ServerFunctions.ServerPutFile;
  Server.OnGetFile:=@ServerFunctions.ServerGetFile;
  Server.OnReadAllowed:=@ServerFunctions.ServerReadAllowed;
  Server.OnWriteAllowed:=@ServerFunctions.ServerReadAllowed;
  Server.OnUserLogin:=@ServerFunctions.ServerUserLogin;
end;

procedure TWebDAVTest.PropfindRoot;
var
  aRes: String;
begin
  aRes := SendRequest(
   'PROPFIND / HTTP/1.1'+#13
  +'Depth: 0'+#13
  +'Prefer: return-minimal'+#13
  +'Content-Type: application/xml; charset=utf-8'+#13
  +#13
//  +'<?xml version="1.0" encoding="utf-8"?>'+#13
  +'<d:propfind xmlns:d="DAV:" xmlns:cs="http://calendarserver.org/ns/">'+#13
  +'  <d:prop>'+#13
  +'     <d:displayname />'+#13
  +'     <cs:getctag />'+#13
  +'  </d:prop>'+#13
  +'</d:propfind>'+#13
  );
  Check(copy(aRes,0,pos(#10,aRes)-1)='401','Wrong Answer to PropfindRoot');
end;

procedure TWebDAVTest.FindPrincipal;
var
  aRes, tmp: String;
begin
  aRes := SendRequest(
    'PROPFIND / HTTP/1.1'+#13
  +'Depth: 0'+#13
  +'Prefer: return-minimal'+#13
  +'Content-Type: application/xml; charset=utf-8'+#13
  +''+#13
  +'<d:propfind xmlns:d="DAV:">'+#13
  +'  <d:prop>'+#13
  +'     <d:current-user-principal />'+#13
  +'  </d:prop>'+#13
  +'</d:propfind>'+#13);
  Check(copy(aRes,0,pos(#10,aRes)-1)='401','Wrong Answer to Propfind Principal');
  tmp := copy(ares,pos('d:current-user-principal',aRes)+24,length(aRes));

end;

procedure TWebDAVTest.OptionsDepth0;
begin

end;

initialization

  RegisterTest(TWebDAVTest);
end.

