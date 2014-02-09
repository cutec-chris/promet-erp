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
Created 01.06.2006
*******************************************************************************}
unit udownloads;
{$mode objfpc}{$H+}
interface
uses
  SysUtils, Classes, httpdefs, fpHTTP, fpWeb, FileUtil,LCLProc;
type
  TfmDownloads = class(TFPWebModule)
    procedure DataModuleRequest(Sender: TObject; ARequest: TRequest;
      AResponse: TResponse; var Handled: Boolean);
  private
    { private declarations }
  public
    { public declarations }
  end; 
var
  fmDownloads: TfmDownloads;
implementation
{$R *.lfm}
uses uBaseApplication,uData,uDocuments,Utils,uError,uBaseWebSession;
procedure TfmDownloads.DataModuleRequest(Sender: TObject; ARequest: TRequest;
  AResponse: TResponse; var Handled: Boolean);
var
  aPath: String;
  aFile: TFileStream;
  Documents : TDocument;
  aExt: String;
begin
  TBaseWebSession(Session).AddHistoryUrl(ARequest.PathInfo);
  with BaseApplication as IBaseApplication do
    begin
      aPath := ARequest.PathInfo;
      if copy(aPath,0,1) = '/' then
        aPath := copy(aPath,2,length(aPath));
      aPath := CleanAndExpandDirectory(Config.ReadString('DOCROOTPATH','')+aPath);
      if copy(aPath,length(aPath),1) = '/' then
        aPath := copy(aPath,0,length(aPath)-1);
      if pos('?',aPath) > 0 then
        aPath := copy(aPath,0,pos('?',aPath)-1);
      aExt := ExtractFileExt(aPath);
    end;
  if not FileExists(aPath) then
    begin
      Documents := TDocument.Create(Self,Data);
      Data.SetFilter(Documents,'"TYPE"=''W'' and "NAME"='+Data.QuoteValue(ValidateFileName(copy(ExtractFileName(aPath),0,rpos('.',ExtractFileName(aPath))-1))),1);
      if Documents.DataSet.RecordCount > 0 then
        begin
          aFile := TFileStream.Create(aPath,fmCreate);
          Data.BlobFieldToStream(Documents.DataSet,'DOCUMENT',aFile);
          aFile.Free;
        end;
      Documents.Free;
    end;
  if FileExistsUTF8(aPath) and not DirectoryExistsUTF8(aPath) then
    begin
      aFile := TFileStream.Create(UTF8ToSys(aPath),fmOpenRead,fmShareDenyNone);
      AResponse.ContentType := 'application/'+copy(aExt,2,length(aExt));
      AResponse.Code := 200;
      AResponse.ContentStream := aFile;
      Aresponse.ContentLength := aFile.Size;
      AResponse.Expires := HTTPDate(Now()+31);
      AResponse.SendContent;
      aFile.Free;
    end
  else
    begin
      //writeln('udownloads:File not found:'+aPath);
      AResponse.Code := 404;
      AResponse.CodeText := 'Not found';
      AResponse.SendContent;
    end;
  Handled := True;
end;
initialization
  RegisterHTTPModule('downloads', TfmDownloads);
end.

