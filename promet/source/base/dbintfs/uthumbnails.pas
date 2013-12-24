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
Created 22.12.2013
*******************************************************************************}
unit uthumbnails;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uDocuments,Utils,Graphics,FileUtil,variants,
  FPImage,fpreadgif,FPReadPSD,FPReadPCX,FPReadTGA,FPReadJPEGintfd,fpthumbresize,
  FPWriteJPEG;

function GetThumbnailPath(aDocument : TDocuments;aWidth : Integer=310;aHeight : Integer=428) : string;
function GetThumbnailBitmap(aDocument : TDocuments;aWidth : Integer=310;aHeight : Integer=428) : TBitmap;
function GetThumbTempDir : string;
function GenerateThumbNail(aName : string;aFullStream,aStream : TStream;aWidth : Integer=310;aHeight : Integer=428) : Boolean;

implementation

function GetThumbnailPath(aDocument: TDocuments;aWidth : Integer=310;aHeight : Integer=428): string;
var
  bDocument: TDocument;
  aFullStream: TMemoryStream;
  aFilename: String;
  aStream: TFileStream;
  aNumber: String;
  DelStream: Boolean;
begin
  Result := '';
  aFilename := AppendPathDelim(GetThumbTempDir)+VarToStr(aDocument.Ref_ID);
  aFilename := aFilename+'_'+IntToStr(aWidth)+'x'+IntToStr(aHeight);
  aFilename:=aFilename+'.jpg';
  if not FileExists(aFilename) then
    begin
      aNumber := aDocument.FieldByName('NUMBER').AsString;
      bDocument := TDocument.Create(nil,aDocument.DataModule);
      bDocument.SelectByNumber(aNumber);
      bDocument.Open;
      aFullStream := TMemoryStream.Create;
      bDocument.CheckoutToStream(aFullStream);
      bDocument.Free;
      aStream := TFileStream.Create(aFilename,fmCreate);
      GenerateThumbNail(aFilename,aFullStream,aStream,aWidth,aHeight);
      DelStream := aStream.Size=0;
      aStream.Free;
      if DelStream then
        DeleteFileUTF8(aFilename)
      else Result := aFilename;
    end
  else
    Result := aFilename;
end;

function GetThumbnailBitmap(aDocument: TDocuments;aWidth : Integer=310;aHeight : Integer=428): TBitmap;
var
  aJpg: TJPEGImage;
  aFilename: String;
begin
  Result := TBitmap.Create;
  try
    aFilename := GetThumbNailPath(aDocument,aWidth,aHeight);
    if aFilename='' then exit;
    aJpg := TJPEGImage.Create;
    try
      aJpg.LoadFromFile(aFilename);
    except
      begin
        aJpg.Free;
        exit;
      end;
    end;
    Result.Width:=aJpg.Width;
    Result.Height:=aJpg.Height;
    Result.Canvas.Draw(0,0,aJpg);
  finally
    aJpg.Free;
  end;
end;

function GetThumbTempDir: string;
begin
  Result := GetTempDir+'promet_thumbs';
end;

function GenerateThumbNail(aName: string; aFullStream, aStream: TStream;
  aWidth: Integer; aHeight: Integer): Boolean;
var
  Img: TFPMemoryImage = nil;
  i: Integer;
  e: String;
  r: Integer;
  s: String;
  d: TIHData;
  h: TFPCustomImageReaderClass = nil;
  reader: TFPCustomImageReader;
  Msg: String;
  iOut: TFPMemoryImage;
  wr: TFPWriterJPEG;
  area: TRect;
begin
  Result := True;
  try
    e := lowercase (ExtractFileExt(aName));
    if (e <> '') and (e[1] = '.') then
      System.delete (e,1,1);
    s := e + ';';
    if (s = 'jpg;') or (s='jpeg;') then
      h := TFPReaderJPEG
    else
      for i := 0 to ImageHandlers.Count-1 do
        if pos(s,ImageHandlers.Extentions[ImageHandlers.TypeNames[i]]+';')>0 then
          begin
            h := ImageHandlers.ImageReader[ImageHandlers.TypeNames[i]];
            break;
          end;
    if assigned (h) then
      begin
        Img := TFPMemoryImage.Create(0, 0);
        Img.UsePalette := false;
        reader := h.Create;
        if reader is TFPReaderJPEG then
          begin
            TFPReaderJPEG(reader).MinHeight:=aHeight;
            TFPReaderJPEG(reader).MinWidth:=aWidth;
          end;
        try
          Img.LoadFromStream(aFullStream, reader);
          if reader is TFPReaderJPEG then
            begin
            end;
        except
          FreeAndNil(Img);
        end;
        Reader.Free;
      end;
    if Assigned(Img) then
      begin
        iOut := ThumbResize(Img, aWidth, aHeight, area);
        wr := TFPWriterJPEG.Create;
        wr.ProgressiveEncoding:=True;
        iOut.SaveToStream(aStream,wr);
        wr.Free;
        iOut.Free;
        Img.Free;
      end;
  except
    Result := False;
  end;
end;

end.

