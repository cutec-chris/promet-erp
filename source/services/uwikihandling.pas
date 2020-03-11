unit uwikihandling;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpHTTP, fpWeb, HTTPDefs;

type

  { TWikiModule }

  TWikiModule = class(TFPWebModule)
  public
    constructor CreateNew(AOwner: TComponent; CreateMode: Integer); override;
    procedure DoHandleRequest(Sender: TObject; ARequest: TRequest;
      AResponse: TResponse; var Handled: Boolean);
  end;

implementation

{ TWikiModule }

constructor TWikiModule.CreateNew(AOwner: TComponent; CreateMode: Integer);
begin
  inherited CreateNew(AOwner, CreateMode);
  OnRequest:=@DoHandleRequest;
end;

procedure TWikiModule.DoHandleRequest(Sender: TObject; ARequest: TRequest;
  AResponse: TResponse; var Handled: Boolean);
begin
  Response.Code:=404;
  Response.CodeText:='Not found';
  Handled:=True;
end;

initialization
  RegisterHTTPModule('wiki', TWikiModule, True);
end.

