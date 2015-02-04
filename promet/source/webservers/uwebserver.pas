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
uses uData,uBaseApplication,uBaseDbClasses,Utils;
{$R *.lfm}

procedure TTWebServer.DataModuleCreate(Sender: TObject);
begin
  FDocuments := TDocument.Create(nil);
  FDocuments.Select(1,'D',0);
  FDocuments.OpenPath('apps/','/');
  with BaseApplication as IBaseApplication do
    FTempPath:=AppendPathDelim(GetInternalTempDir)+'httpdocs';
end;

procedure TTWebServer.DataModuleDestroy(Sender: TObject);
begin
  FDocuments.Free;
end;

procedure TTWebServer.DataModuleRequest(Sender: TObject; ARequest: TRequest;
  AResponse: TResponse; var Handled: Boolean);
var
  aPath: String;
  aStream: TFileStream;
begin
  aPath := StringReplace(ARequest.URL,'/',DirectorySeparator,[rfReplaceAll]);
  if copy(aPath,length(aPath),1)=DirectorySeparator then
    aPath := aPath+'index.html';
  if not FileExists(FTempPath+aPath) then
    begin
      if FDocuments.OpenPath(aPath,DirectorySeparator) then
        begin
          aStream := TFileStream.Create(FTempPath+aPath,fmCreate);
          TDocument(FDocuments).CheckoutToStream(aStream);
          aStream.Free;
        end;
    end;
  if FileExists(UniToSys(FTempPath+aPath)) then
    begin
      AResponse.ContentStream:=TFileStream.Create(FTempPath+aPath,fmOpenRead);
      AResponse.ContentType:='text/'+copy(ExtractFileExt(aPath),2,length(aPath));
      if AResponse.ContentType='text/htm' then
        AResponse.ContentType:='text/html';
      AResponse.Code:=200;
    end
  else
    begin
      AResponse.Content:='not found';
      AResponse.ContentType:='text/html';
      AResponse.Code:=404;
    end;
  AResponse.SendContent;
  Handled:=True;
end;

initialization
  RegisterHTTPModule('apps', TTWebServer);
end.

