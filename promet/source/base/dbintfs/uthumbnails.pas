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
  Classes, SysUtils, uDocuments,Utils,variants,
  FPImage,fpreadgif,FPReadPSD,FPReadPCX,FPReadTGA,FPReadJPEGintfd,fpthumbresize,
  FPWriteJPEG,FPReadBMP,process,uBaseDbClasses,FPCanvas,FPImgCanv,
  uBaseDBInterface,db,uBaseDatasetInterfaces
  {$IFDEF LCL}
  ,Graphics
  {$ENDIF}
  ;

type
  TThumbnails = class(TBaseDBDataset)
  public
    procedure DefineFields(aDataSet: TDataSet); override;
    procedure SelectByRefId(aId : Variant);
  end;

function GetThumbnailPath(aDocument : TDocuments;aWidth : Integer=310;aHeight : Integer=428) : string;
function GetThumbTempDir : string;
function ClearThumbDir : Boolean;
function GenerateThumbNail(aName : string;aFullStream,aStream : TStream;aText : string;aWidth : Integer=310;aHeight : Integer=428) : Boolean;
function GenerateThumbNail(aName : string;aFileName : string;aStream : TStream;aText : string;aWidth : Integer=310;aHeight : Integer=428) : Boolean;

implementation

uses uBaseApplication,SecureUtils;
var
  ThumbDir : string;

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
  try
  aFilename := GetThumbTempDir+VarToStr(aDocument.Ref_ID);
  aFilename := aFilename+'_'+IntToStr(aWidth)+'x'+IntToStr(aHeight);
  aFilename:=aFilename+'.jpg';
  if not FileExists(aFilename) then
    begin
      aNumber := aDocument.FieldByName('NUMBER').AsString;
      bDocument := TDocument.CreateEx(nil,aDocument.DataModule);
      bDocument.SelectByNumber(aNumber);
      bDocument.Open;
      aFullStream := TMemoryStream.Create;
      bDocument.CheckoutToStream(aFullStream);
      bDocument.Free;
      try
        aStream := TFileStream.Create(aFilename,fmCreate);
        GenerateThumbNail(aFilename,aFullStream,aStream,'',aWidth,aHeight);
        DelStream := aStream.Size=0;
        aStream.Free;
      except
        DelStream:=True;
      end;
      try
        if DelStream then
          DeleteFile(UniToSys(aFilename))
        else Result := aFilename;
      except
      end;
    end
  else
    Result := aFilename;
  except
    Result:='';
  end;
end;

function GetThumbTempDir: string;
begin
  if ThumbDir='' then
    begin
      try
        Result := GetTempDir+'promet_thumbs';
        if Assigned(BaseApplication) and (not BaseApplication.ConsoleApplication) then
          if Supports(BaseApplication,IBaseApplication) then
            with BaseApplication as IBaseApplication do
              Result := GetInternalTempDir+'promet_thumbs';
        ForceDirectories(UniToSys(Result));
        ThumbDir:=Result;
      except
      end;
    end
  else Result:=ThumbDir;
end;

function ClearThumbDir: Boolean;
begin
  Result := RemoveDir(UniToSys(GetThumbTempDir));
end;

function GenerateThumbNail(aName: string; aFullStream, aStream: TStream;
  aText: string; aWidth: Integer; aHeight: Integer): Boolean;
var
  e: String;
  r: Integer;
  s: String;
  d: TIHData;
  aFStream: TFileStream;
  aFilename: String;
begin
  Result := True;
  e := lowercase (ExtractFileExt(aName));
  if (e <> '') and (e[1] = '.') then
    System.delete (e,1,1);
  s := e + ';';
  with BaseApplication as IBaseApplication do
    begin
      aFilename := GetInternalTempDir+'rpv.'+e;
      aFStream := TFileStream.Create(GetInternalTempDir+'rpv.'+e,fmCreate);
    end;
  if aFullStream.Position=aFullStream.Size then
    aFullStream.Position:=0;
  aFStream.CopyFrom(aFullStream,aFullStream.Size-aFullStream.Position);
  aFStream.Free;
  Result := GenerateThumbNail(aName,aFilename,aStream,aText,aWidth,aHeight);
end;

function GenerateThumbNail(aName: string; aFileName: string; aStream: TStream;
  aText: string; aWidth: Integer; aHeight: Integer): Boolean;
var
  Img: TFPMemoryImage = nil;
  e: String;
  s: String;
  h: TFPCustomImageReaderClass = nil;
  reader: TFPCustomImageReader;
  Msg: String;
  iOut: TFPMemoryImage;
  wr: TFPWriterJPEG;
  area: TRect;
  aProcess: TProcess;
  i: Integer;
  sl: TStringList;
  {$IFDEF LCL}
  Printer: TBitmap;
  {$ENDIF}
  randlinks: Int64;
  randoben: Int64;
  zeile: Integer;
  x: Int64;
  y: Integer;
  LineHeight: Extended;
  aPrinter: TFPImageCanvas;
  aOldPos: Int64;
  function ConvertExec(aCmd,aExt : string) : Boolean;
  begin
    aProcess := TProcess.Create(nil);
    {$IFDEF WINDOWS}
    aProcess.Options:= [poNoConsole, poWaitonExit,poNewConsole, poStdErrToOutPut, poNewProcessGroup];
    {$ELSE}
    aProcess.Options:= [poWaitonExit,poStdErrToOutPut];
    {$ENDIF}
    aProcess.ShowWindow := swoHide;
    aProcess.CommandLine := aCmd;
    aProcess.CurrentDirectory := AppendPathDelim(ExtractFileDir(ParamStr(0)))+'tools';
    try
      aProcess.Execute;
    except
      on e : Exception do
        begin
          with BaseApplication as IBaseApplication do
            Error('Convert Error:'+e.Message);
          result := False;
        end;
    end;
    Result := FileExists(aFileName+aExt);
    aProcess.Free;
    if Result then
      begin
        Img := TFPMemoryImage.Create(0, 0);
        Img.UsePalette := false;
        try
          Img.LoadFromFile(aFileName+aExt);
          SysUtils.DeleteFile(aFileName+aExt);
        except
          FreeAndNil(Img);
        end;
      end;
  end;
begin
  Result := False;
  with BaseApplication as IBaseApplication do
    Debug('Generate Thumbnail:Enter');
  try
    e := lowercase (ExtractFileExt(aName));
    if (e <> '') and (e[1] = '.') then
      System.delete (e,1,1);
    s := e + ';';
    if (s = 'jpg;') or (s='jpeg;') then
      h := TFPReaderJPEG
    else
      for i := 0 to ImageHandlers.Count-1 do
        if pos(s,ImageHandlers.{$IF FPC_FULLVERSION>20604}Extensions{$ELSE}Extentions{$ENDIF}[ImageHandlers.TypeNames[i]]+';')>0 then
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
          Img.LoadFromFile(aFilename, reader);
          if reader is TFPReaderJPEG then
            begin
            end;
        except
          FreeAndNil(Img);
        end;
        Reader.Free;
      end
    else if (s = 'pdf;') then
      begin
        Result := ConvertExec(Format({$IFDEF WINDOWS}AppendPathDelim(AppendPathDelim(ExtractFileDir(SysToUni(ParamStr(0))))+'tools')+{$ENDIF}'pdftopng -l 1 %s %s',[aFileName,aFilename]),'-000001.png');
        if not Result then
          Result := ConvertExec(Format({$IFDEF WINDOWS}AppendPathDelim(AppendPathDelim(ExtractFileDir(SysToUni(ParamStr(0))))+'tools')+'gswin32'+{$ELSE}'gs'+{$ENDIF}' -q -dBATCH -dMaxBitmap=300000000 -dNOPAUSE -dSAFER -sDEVICE=bmp16m -dTextAlphaBits=4 -dGraphicsAlphaBits=4 -dFirstPage=1 -dLastPage=1 -sOutputFile=%s %s -c quit',[aFileName+'.bmp',aFileName]),'.bmp');
      end
    else
      begin
        Result := ConvertExec(Format({$IFDEF WINDOWS}AppendPathDelim(AppendPathDelim(ExtractFileDir(SysToUni(ParamStr(0))))+'tools')+{$ENDIF}'convert %s[1] -resize %d -alpha off +antialias "%s"',[aFileName,500,afileName+'.bmp']),'.bmp');
        if not Result then
          Result := ConvertExec(Format({$IFDEF WINDOWS}AppendPathDelim(AppendPathDelim(ExtractFileDir(SysToUni(ParamStr(0))))+'tools')+{$ENDIF}'ffmpeg -i "%s" -qscale 0 -vframes 1 "%s"',[aFileName,aFileName+'.bmp']),'.bmp');
        if not Result then
          Result := ConvertExec(Format({$IFDEF WINDOWS}AppendPathDelim(AppendPathDelim(ExtractFileDir(SysToUni(ParamStr(0))))+'tools')+{$ENDIF}'avconv -i "%s" -qscale 0 -vframes 1 "%s"',[aFileName,aFileName+'.bmp']),'.bmp');
      end;
    SysUtils.DeleteFile(aFileName);
    if Assigned(Img) then
      begin
        iOut := DoThumbResize(Img, aWidth, aHeight, area);
        wr := TFPWriterJPEG.Create;
        wr.ProgressiveEncoding:=True;
        aOldPos:=aStream.Position;
        iOut.SaveToStream(aStream,wr);
        aStream.Position:=aOldPos;
        wr.Free;
        iOut.Free;
        Img.Free;
        Result := True;
      end;
  except
    Result := False;
  end;
  if not result and (trim(aText)<>'') then
    begin
      Img := TFPMemoryImage.Create(aWidth, aHeight);
      Img.UsePalette := false;
      aPrinter := TFPImageCanvas.create(Img);
      sl := TStringList.Create;
      sl.Text:=aText;
      while sl.Count>80 do sl.Delete(79);
      aPrinter.Brush.FPColor:=FPColor(65535,65535,65535);//white
      aPrinter.Rectangle(0,0,aWidth,aHeight);
      randlinks:=round(aWidth/100);
      randoben:=round(aHeight/100);
      //Schrift-Einstellungen:
      aPrinter.Font.Name:='Courier New';
      LineHeight := ((aHeight-(randoben*2))/80);
      aPrinter.Font.FPColor:=FPColor(0,0,0);
      {$ifndef CPUARM}
      aPrinter.Font.Size:=1;
      while aPrinter.TextExtent('Äg').cy< LineHeight do
        aPrinter.Font.Size:=aPrinter.Font.Size+1;
      {$endif}
      x:=randlinks;
      y:=randoben+1;
      for zeile:=0 to sl.Count-1 do
        begin
        aPrinter.TextOut(x, y, sl[zeile]);
        y:=round(randoben+(LineHeight*Zeile));
        end;
      sl.Free;
      wr := TFPWriterJPEG.Create;
      wr.ProgressiveEncoding:=True;
      aOldPos := aStream.Position;
      Img.SaveToStream(aStream,wr);
      aStream.Position:=aOldPos;
      wr.Free;
      Img.Free;
      aPrinter.Free;
    end;
  with BaseApplication as IBaseApplication do
    Debug('Generate Thumbnail:Exit');
end;

procedure TThumbnails.DefineFields(aDataSet: TDataSet);
begin
  with aDataSet as IBaseManageDB do
    begin
      TableName := 'THUMBNAILS';
      if Assigned(ManagedFieldDefs) then
        with ManagedFieldDefs do
          begin
            Add('REF_ID_ID',ftLargeint,0,True);
            Add('THUMBNAIL',ftBlob,0,False);
          end;
      if Assigned(ManagedIndexdefs) then
        with ManagedIndexDefs do
          Add('REF_ID_ID','REF_ID_ID',[]);
    end;
end;

procedure TThumbnails.SelectByRefId(aId: Variant);
begin
  with BaseApplication as IBaseDbInterface do
    begin
      with Self.DataSet as IBaseDBFilter do
        begin
          if aId <> Null then
            Filter := Data.QuoteField('REF_ID_ID')+'='+Data.QuoteValue(VarToStr(aId))
          else
            Filter := Data.QuoteField('REF_ID_ID')+'='+Data.QuoteValue('0');
          Limit := 0;
        end;
    end;
end;

initialization
  ThumbDir := '';
finalization
  DeleteDirectorySecure(GetThumbTempDir,false);
end.

