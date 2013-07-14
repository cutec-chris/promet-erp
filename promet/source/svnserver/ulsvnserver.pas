unit ulsvnserver;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uLWebDAVServer;
type
  TLSVNURIHandler = class(TLWebDAVURIHandler)
  end;
  TLSVNServer = class(TLWebDAVServer)
  protected
    procedure RegisterHandlers;override;
  public
  end;
implementation
procedure TLSVNServer.RegisterHandlers;
begin
  URIHandler := TLWebDAVURIHandler.Create;
  FileHandler := TLWebDAVFileHandler.Create;
  Self.RegisterHandler(URIHandler);
  Self.RegisterHandler(FileHandler);
end;
end.

