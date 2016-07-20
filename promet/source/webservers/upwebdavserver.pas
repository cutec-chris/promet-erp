unit upwebdavserver;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, dom, xmlread, xmlwrite, uappserverhttp,udavserver,uAppServer;

implementation

var
  DavServer : TWebDAVMaster = nil;

function HandleDAVRequest(Sender : TAppNetworkThrd;Method, URL: string;Headers : TStringList;Input,Output : TStream): Integer;
var
  i: Integer;
  aSock: TDAVSession = nil;
begin
  if not Assigned(DavServer) then
    begin
      DavServer := TWebDAVMaster.Create;
    end;
  for i := 0 to Sender.Objects.Count-1 do
    if TObject(Sender.Objects[i]) is TDAVSession then
      aSock := TDAVSession(Sender.Objects[i]);
  if not Assigned(aSock) then
    aSock := TDAVSession.Create(DavServer);
end;

initialization
  uappserverhttp.RegisterHTTPHandler(@HandleDAVRequest);

end.

