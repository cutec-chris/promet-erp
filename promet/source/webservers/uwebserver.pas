{*******************************************************************************
  Copyright (C) Christian Ulrich info@cu-tec.de

  This source is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 2 of the License, or commercial alternative
  contact us for more information

  This code is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
  to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
  MA 02111-1307, USA.

info@cu-tec.de
*******************************************************************************}
unit uwebserver;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, httpdefs, fpHTTP, fpWeb,uDocuments;

type

  { TTWebServer }

  TTWebServer = class(TFPWebModule)
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
    procedure DataModuleRequest(Sender: TObject; ARequest: TRequest;
      AResponse: TResponse; var Handled: Boolean);
  private
    { private declarations }
    FDocuments: TDocuments;
    FTempPath : string;
  public
    { public declarations }
  end;

var
  TWebServer: TTWebServer;

implementation
uses uData,uBaseApplication,uBaseDbClasses,FileUtil;
{$R *.lfm}

procedure TTWebServer.DataModuleCreate(Sender: TObject);
begin
  FDocuments := TDocuments.Create(nil,Data);
  FDocuments.Select(1,'D',0);
  FDocuments.OpenPath('apps/','/');
  FTempPath:=AppendPathDelim(AppendPathDelim(GetInternalTempDir)+'httpdocs');
end;

procedure TTWebServer.DataModuleDestroy(Sender: TObject);
begin
  FDocuments.Free;
end;

procedure TTWebServer.DataModuleRequest(Sender: TObject; ARequest: TRequest;
  AResponse: TResponse; var Handled: Boolean);
var
  aPath: String;
begin
  aPath := ARequest.URL;
  if not FileExists(FTempPath+aPath) then
    begin

    end;
  AResponse.Content:='Juhuu';
  AResponse.ContentType:='text/html';
  AResponse.Code:=200;
  Handled:=True;
end;

initialization
  RegisterHTTPModule('apps', TTWebServer);
end.

