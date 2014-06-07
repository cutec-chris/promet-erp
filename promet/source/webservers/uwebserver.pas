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
  Classes, SysUtils, FileUtil, lNetComponents,lwebserver,lEvents,lhttp, lNet,
  LCLProc;

type
  { TPFileHandler }

  TPFileHandler = class(TURIHandler)
  protected
    function HandleURI(ASocket: TLHTTPServerSocket): TOutputItem; override;
  public
    DocumentRoot: string;
  end;

  { TfWebServer }

  TfWebServer = class(TDataModule)
    LSSLSessionComponent1: TLSSLSessionComponent;
    Server: TLHTTPServerComponent;
    procedure DataModuleCreate(Sender: TObject);
    procedure ServerAccess(AMessage: string);
    procedure ServerError(const msg: string; aSocket: TLSocket);
  private
    { private declarations }
  public
    FileHandler: TPFileHandler;
    { public declarations }
  end;

var
  fWebServer: TfWebServer;

implementation
uses uBaseApplication,uBaseDBInterface,uBaseDbClasses;
{ TfWebServer }

procedure TfWebServer.DataModuleCreate(Sender: TObject);
begin
  FileHandler := TPFileHandler.Create;
  FileHandler.DocumentRoot := GetInternalTempDir+'phttpdocs';
  ForceDirectoriesUTF8(GetInternalTempDir+'phttpdocs');

  Server.RegisterHandler(FileHandler);
  if not Server.Listen(Server.Port) then
    raise Exception.Create('listen failed');
end;

procedure TfWebServer.ServerAccess(AMessage: string);
begin
  debugln(AMessage);
end;

procedure TfWebServer.ServerError(const msg: string; aSocket: TLSocket);
begin
  debugln('Error:'+msg);
end;

{ TPFileHandler }

function TPFileHandler.HandleURI(ASocket: TLHTTPServerSocket): TOutputItem;
begin
  Result := nil;
end;


{$R *.lfm}

end.

