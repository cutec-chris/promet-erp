unit upwebdavserver;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, dom, xmlread, xmlwrite, uappserverhttp,udavserver,uAppServer;

implementation

var
  DavServer : TWebDAVMaster = nil;

function HandleDAVRequest(Sender : TAppNetworkThrd;Method, URL: string;Headers : TStringList;Input,Output : TMemoryStream): Integer;
var
  i: Integer;
  aSock: TDAVSession = nil;
  aParameters: TStringList;
begin
  Result := 500;
  try
    if not Assigned(DavServer) then
      begin
        DavServer := TWebDAVMaster.Create;
      end;
    for i := 0 to Sender.Objects.Count-1 do
      if TObject(Sender.Objects[i]) is TDAVSession then
        aSock := TDAVSession(Sender.Objects[i]);
    if not Assigned(aSock) then
      begin
        aParameters := TStringList.Create;
        aSock := TDAVSession.Create(DavServer,aParameters);
      end;
    Result := aSock.ProcessHttpRequest(Method,URL,Headers,Input,Output);
  except
    Result:=500;
  end;
end;

initialization
  uappserverhttp.RegisterHTTPHandler(@HandleDAVRequest);

end.

