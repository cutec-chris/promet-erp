unit uwebserver;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, httpdefs, fpHTTP, fpWeb;

type
  TTWebServer = class(TFPWebModule)
    procedure DataModuleRequest(Sender: TObject; ARequest: TRequest;
      AResponse: TResponse; var Handled: Boolean);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  TWebServer: TTWebServer;

implementation
{$R *.lfm}
procedure TTWebServer.DataModuleRequest(Sender: TObject; ARequest: TRequest;
  AResponse: TResponse; var Handled: Boolean);
begin
  AResponse.Content:='Juhuu';
  AResponse.ContentType:='text/html';
  AResponse.Code:=200;
  Handled:=True;
end;

initialization
  RegisterHTTPModule('apps', TTWebServer);
end.

