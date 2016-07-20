unit upwebdavserver;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, dom, xmlread, xmlwrite, uappserverhttp,udavserver;

implementation

var
  DavServer : TWebDAVServer = nil;

function HandleDAVRequest(Sender : TObject;Method, URL: string;Headers : TStringList;Input,Output : TStream): Integer;
begin
end;

initialization
  uappserverhttp.RegisterHTTPHandler(@HandleDAVRequest);

end.

