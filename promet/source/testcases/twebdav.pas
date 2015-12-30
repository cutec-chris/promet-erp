unit twebdav;

{$mode objfpc}{$H+}

//Checks according to http://sabre.io/dav/building-a-caldav-client/

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry,udavserver,
  Sockets,uhttputil,uprometdavserver,uData;

type

  { TWebDAVTest }

  TWebDAVTest= class(TTestCase)
  private
    function SendRequest(aRequest: string): string;
  published
    procedure SetUp; override;
    procedure PropfindRoot;
    procedure FindPrincipal;
    procedure FindPrincipalOnWellKnown;
    procedure FindPrincipalCalendarHome;
    procedure FindPrincipalCalendars;
    procedure CheckNamespaceUsage;
    procedure AddEvent;
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
  UserURL,UserCalenderURL : string;

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
  aURL, tmp: String;
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
  tmp := copy(aURL,pos(' ',aURL)+1,length(aURL));
  tmp := trim(copy(tmp,0,pos('HTTP',tmp)-1));
  Socket.ProcessHttpRequest(copy(aURL,0,pos(' ',aURL)-1),tmp);
  Result := IntToStr(Socket.Status)+LineEnding+MemoryStreamToString(Socket.OutputData);
end;
procedure TWebDAVTest.SetUp;
begin
  inherited SetUp;
  aReq := TStringList.Create;
  Socket := TestSocket.Create(0);
  Server := TWebDAVServer.Create;
  Socket.Creator:=Server;
  Socket.User:=Data.Users.Id.AsString;
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
  Check(copy(aRes,0,pos(#10,aRes)-1)='207','Wrong Answer to PropfindRoot');
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
  Check(copy(aRes,0,pos(#10,aRes)-1)='207','Wrong Answer to Propfind Principal');
  tmp := copy(ares,pos('d:current-user-principal',aRes)+24,length(aRes));
  tmp := copy(tmp,2,pos('/d:current-user-principal',tmp)-3);
  tmp := trim(Stringreplace(tmp,#10,'',[rfReplaceAll]));
  Check(pos('d:href',tmp)>0,'Wrong Principal');
  tmp := copy(tmp,pos('>',tmp)+1,length(tmp));
  tmp := copy(tmp,0,pos('<',tmp)-1);
  UserURL := trim(tmp);
  Check(UserURL<>'','Wrong Principal URL');
end;
procedure TWebDAVTest.FindPrincipalOnWellKnown;
var
  aRes, tmp: String;
begin
  aRes := SendRequest(
    'PROPFIND /.well-known/caldav/ HTTP/1.1'+#13
  +'Depth: 0'+#13
  +'Prefer: return-minimal'+#13
  +'Content-Type: application/xml; charset=utf-8'+#13
  +''+#13
  +'<d:propfind xmlns:d="DAV:">'+#13
  +'  <d:prop>'+#13
  +'     <d:current-user-principal />'+#13
  +'  </d:prop>'+#13
  +'</d:propfind>'+#13);
  Check(copy(aRes,0,pos(#10,aRes)-1)='207','Wrong Answer to Propfind Principal');
  tmp := copy(ares,pos('d:current-user-principal',aRes)+24,length(aRes));
  tmp := copy(tmp,2,pos('/d:current-user-principal',tmp)-3);
  tmp := trim(Stringreplace(tmp,#10,'',[rfReplaceAll]));
  Check(pos('d:href',tmp)>0,'Wrong Principal');
  tmp := copy(tmp,pos('>',tmp)+1,length(tmp));
  tmp := copy(tmp,0,pos('<',tmp)-1);
  UserURL := trim(tmp);
  Check(UserURL<>'','Wrong Principal URL');
end;
procedure TWebDAVTest.FindPrincipalCalendarHome;
var
  aRes, tmp: String;
begin
  aRes := SendRequest(
    'PROPFIND '+UserURL+' HTTP/1.1'+#13
  +'Depth: 0'+#13
  +'Prefer: return-minimal'+#13
  +'Content-Type: application/xml; charset=utf-8'+#13
  +''+#13
  +'<d:propfind xmlns:d="DAV:" xmlns:c="urn:ietf:params:xml:ns:caldav">'+#13
  +'  <d:prop>'+#13
  +'     <c:calendar-home-set />'+#13
  +'  </d:prop>'+#13
  +'</d:propfind>'+#13);
  Check(copy(aRes,0,pos(#10,aRes)-1)='207','Wrong Answer to Calendar Home Sets');
  tmp := copy(ares,pos('c:calendar-home-set',aRes)+20,length(aRes));
  tmp := copy(tmp,2,pos('/c:calendar-home-set',tmp)-3);
  tmp := trim(Stringreplace(tmp,#10,'',[rfReplaceAll]));
  Check(pos('d:href',tmp)>0,'Wrong Home Set');
  tmp := copy(tmp,pos('>',tmp)+1,length(tmp));
  tmp := copy(tmp,0,pos('<',tmp)-1);
  UserCalenderURL := trim(tmp);
  Check(UserCalenderURL<>'','Wrong Home Set URL');
end;
procedure TWebDAVTest.FindPrincipalCalendars;
var
  aRes: String;
begin
  aRes := SendRequest(
   'PROPFIND '+UserCalenderURL+' HTTP/1.1'+#13
  +'Depth: 1'+#13
  +'Prefer: return-minimal'+#13
  +'Content-Type: application/xml; charset=utf-8'+#13
  +''+#13
  +'<d:propfind xmlns:d="DAV:" xmlns:cs="http://calendarserver.org/ns/" xmlns:c="urn:ietf:params:xml:ns:caldav">'+#13
  +'  <d:prop>'+#13
  +'     <d:resourcetype />'+#13
  +'     <d:displayname />'+#13
  +'     <cs:getctag />'+#13
  +'     <c:supported-calendar-component-set />'+#13
  +'  </d:prop>'+#13
  +'</d:propfind>'+#13);
  Check(copy(aRes,0,pos(#10,aRes)-1)='207','Wrong Answer to Calendar Home Sets');
end;

procedure TWebDAVTest.CheckNamespaceUsage;
var
  aRes: String;
begin
  aRes := SendRequest(
   'PROPFIND /caldav/ HTTP 1.1'+#13
  +'depth:1'+#13
  +'content-type:application/xml; charset=utf-8'+#13
  +''+#13
  +'<?xml version="1.0" encoding="UTF-8" ?>'+#13
  +'<propfind xmlns="DAV:" xmlns:CAL="urn:ietf:params:xml:ns:caldav" xmlns:CARD="urn:ietf:params:xml:ns:carddav"><prop><CAL:supported-calendar-component-set /><resourcetype /><displayname /><current-user-privilege-set /><n0:calendar-color xmlns:n0="http://apple.com/ns/ical/" /><CAL:calendar-description /><CAL:calendar-timezone /></prop></propfind>'+#13);
  Check(copy(aRes,0,pos(#10,aRes)-1)='207','Wrong Answer to Calendar Home Sets');
  Check(pos('xmlns:D="DAV:"',aRes)>0,'DAV Namespace missing');
  Check(pos('xmlns:CAL="urn:ietf:params:xml:ns:caldav"',aRes)>0,'CalDAV Namespace missing');
  Check(pos('xmlns:CARD="urn:ietf:params:xml:ns:carddav"',aRes)>0,'CardDAV Namespace missing');
  //TODO:Check(pos('xmlns:n0="http://apple.com/ns/ical/"',aRes)>0,'ICal Namespace missing');
end;

initialization

  RegisterTest(TWebDAVTest);
end.

