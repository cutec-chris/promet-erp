{*
 * << P o w e r P d f >> -- PdfImages.pas
 * << Standerd image classes defination >>
 *
 * Copyright (c) 1999-2001 T.KANNO. <takeshi_kanno@est.hi-ho.ne.jp>
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Library General Public License as published
 * by the Free Software Foundation; either version 2 of the License, or any
 * later version.
 *
 * This library is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU Library general Public License for more
 * details.
 *
 * You should have received a copy of the GNU Library General Public License
 * along with this library.
 *
 * 2001.03.14 Create.
 * 2001.07.30 Implemented Indexed color image.
 * 2001.08.26 changed some definations and methods to work with kylix.
 * 2001.09.01 changed the implementation of the image.
 *
 *}
{$IFDEF LAZ_POWERPDF}
{$H+}
{$ENDIF}
unit PdfImages;

interface

{$IFDEF UNIX}
  {$IFDEF LAZ_POWERPDF}
  {$ELSE}
  {$DEFINE USE_CLX}
  {$ENDIF}
{$ENDIF}

uses
  SysUtils,
  {$IFNDEF USE_CLX}
  {$IFDEF LAZ_POWERPDF}
  LCLType, LCLIntf, Graphics, FPImage, IntfGraphics, BmpComn,
  {$ELSE}
  Windows, Graphics,
  {$ENDIF}
  {$ELSE}
  QGraphics, Qt,
  {$ENDIF}
  Classes, PdfTypes, PdfDoc;

type
  TPdfImageCreator = class(TPersistent)
  public
    function CreateImage(AImage: TGraphic; ObjectMgr: TPdfObjectMgr=nil): TPdfImage; virtual;
  end;

  { TPdfBitmapImage }

  TPdfBitmapImage = class(TPdfImageCreator)
  private
    function CreateIndexedColorArray(ABitmap: TBitmap): TPdfArray;
    function CreateMaskStream(AImage: TFPCustomImage): TPDfImage;
  public
    function CreateImage(AImage: TGraphic; ObjectMgr: TPdfObjectMgr=nil): TPdfImage; override;
  end;

  EPdfInvalidImageFormat = class(Exception);

  function CreatePdfImage(AImage: TGraphic; ImageClassName: string; ObjectMgr: TPdfObjectMgr=nil): TPdfImage;

implementation

function CreatePdfImage(AImage: TGraphic; ImageClassName: string; ObjectMgr: TPdfObjectMgr=nil): TPdfImage;
var
  PdfImageCreator: TPdfImageCreator;
begin
  Result := nil;
  {$IFDEF LAZ_POWERPDF}
  PdfImageCreator := TPdfImageCreator(PdfLazFindClass(ImageClassName).Create);
  {$ELSE}
  PdfImageCreator := TPdfImageCreator(FindClass(ImageClassName).Create);
  {$ENDIF}
  try
    if PdfImageCreator = nil then
      raise Exception.CreateFmt('AddImage --InvalidImageClassName:%s', [ImageClassName]);
    Result := PdfImageCreator.CreateImage(AImage, ObjectMgr);
  finally
    PdfImageCreator.Free;
  end;
end;

{ TPdfImageCreator }
function TPdfImageCreator.CreateImage(AImage: TGraphic; ObjectMgr: TPdfObjectMgr=nil): TPdfImage;
begin
  result := nil;
end;

{$IFDEF USE_CLX}
type
  TColorTable = array[0..MaxInt div SizeOf(QRgb)-1] of QRgb;
  PColorTable = ^TColorTable;
{$ENDIF}

function TPdfBitmapImage.CreateIndexedColorArray(ABitmap: TBitmap): TPdfArray;
var
  {$IFNDEF USE_CLX}
  PalEntries: array[0..255] of TPaletteEntry;
  {$ELSE}
  PalEntries: PColorTable;
  CRgb: Cardinal;
  pb: PByteArray;
  {$ENDIF}
  i: integer;
  ColorTable: TPdfBinary;
  NumOfColors: integer;
  S: string;
begin
  // creating color table from palette of bitmap.

  if ABitmap.PixelFormat <> pf8bit then
    raise EPdfInvalidImageFormat.Create('only 8 bit color image is allowed.');

  NumOfColors := 256;
  {$IFNDEF USE_CLX}
  if GetPaletteEntries(ABitmap.Palette, 0, NumOfColors + 1, PalEntries) = 0 then
    raise EPdfInvalidImageFormat.Create('failed to get Palette..');
  {$ELSE}
  PalEntries := PColorTable(ABitmap.ColorTable);
  {$ENDIF}
  ColorTable := TPdfBinary.Create;
  S := '<';

  {$IFNDEF USE_CLX}
  for i := 0 to NumOfColors - 1 do
    with PalEntries[i] do
      S := S + IntToHex(peRed, 2) +
           IntToHex(peGreen, 2) +
           IntToHex(peBlue, 2) +
           ' ';
  {$ELSE}
  for i := 0 to NumOfColors - 1 do
  begin
    CRgb := PalEntries[i];
    pb := PByteArray(@CRgb);
    S := S + IntToHex(pb[2], 2) +
         IntToHex(pb[1], 2) +
         IntToHex(pb[0], 2) +
           ' ';
  end;
  {$ENDIF}

  S := S + '>';
  ColorTable.Stream.Write(PChar(S)^, Length(S));

  result := TPdfArray.CreateArray(nil);
  with result do
  begin
    AddItem(TPdfName.CreateName('Indexed'));
    AddItem(TPdfName.CreateName('DeviceRGB'));
    AddItem(TPdfNumber.CreateNumber(NumOfColors - 1));
    AddItem(ColorTable);
  end;
end;

function TPdfBitmapImage.CreateMaskStream(AImage: TFPCustomImage): TPDfImage;
var
  pb: PByteArray;
  y: Integer;
  x: Integer;
begin
  result := TPdfImage.CreateStream(nil);
  with result do
  try
    with Attributes do
    begin
      AddItem('Type', TPdfName.CreateName('XObject'));
      AddItem('Subtype', TPdfName.CreateName('Image'));
      AddItem('Width', TPdfNumber.CreateNumber(aImage.Width));
      AddItem('Height', TPdfNumber.CreateNumber(aImage.Height));
      AddItem('BitsPerComponent', TPdfNumber.CreateNumber(8));
      AddItem('ColorSpace',TPdfName.CreateName('DeviceGray'));
      if USE_ZLIB then
        PdfArrayByName('Filter').AddItem(TPdfName.CreateName('FlateDecode'));

      new(pb);
      for y := 0 to AImage.Height - 1 do
      begin
        for x := 0 to AImage.Width-1 do
          pb^[x] := AImage.Pixels[x,y];
        Stream.Write(pb^, AImage.Width);
      end;
      dispose(pb);
    end;

  finally
  end;
end;

function TPdfBitmapImage.CreateImage(AImage: TGraphic; ObjectMgr: TPdfObjectMgr=nil): TPdfImage;
var
  ABitmap: TBitmap;
  x, y: integer;
  pb: PByteArray;
  b: Byte;
{$IFDEF LAZ_POWERPDF}
  aIntfImage: TLazIntfImage;
  aColor    : TFPColor;
  Alpha      : TFPMemoryImage;
  maskimage : TPDFImage;
  hasAlpha   : boolean;
{$endif}

{$IFDEF USE_CLX}
const
  PIXEL_COLOR_SIZE = 4;
{$ENDIF}
begin

  result := TPdfImage.CreateStream(nil);
  with result do
  try
    with Attributes do
    begin
      AddItem('Type', TPdfName.CreateName('XObject'));
      AddItem('Subtype', TPdfName.CreateName('Image'));
    end;

    ABitmap := TBitmap.Create;
    
    with ABitmap do
    try
      Assign(AImage);

{$IFDEF FPC}
      aIntfImage := TLazIntfImage.Create(0,0);
      aIntfImage.LoadFromBitmap(aBitmap.Handle, aBitmap.MaskHandle);
{$ENDIF}
      // if bitmap image has less then 8 bit color, set PixelFormat to 8 bit.
      if (PixelFormat = pf1bit) or
         {$IFNDEF USE_CLX}
         (PixelFormat = pf4bit) or
         {$ENDIF}
         (PixelFormat = pf8bit) then
        PixelFormat := pf8bit
      else
        {$IFNDEF USE_CLX}
        PixelFormat := pf24Bit;
        {$ELSE}
        PixelFormat := pf32Bit;
        {$ENDIF}

      // translate TBitmap object to pdf image format.
      if PixelFormat = pf8bit then
      begin
{$IFNDEF FPC}
        for y := 0 to Height - 1 do
        begin
          pb := ScanLine[y];
          Stream.Write(pb^, Width);
{$ELSE}
        for y := 0 to aintfimage.Height - 1 do
        begin
          new(pb);
          
          for x := 0 to aintfimage.Width-1 do
          begin
            aColor := aIntfImage.Colors[x,y];
            { kleurwaarden worden als 16bits waarden opgeslagen, we kappen er
              dus 8 van af.
              red is willekeurig genomen
            }
            pb^[x] := acolor.red shr 8;
          end;

          Stream.Write(pb^, Width);
          dispose(pb);
{$ENDIF}
        end;
        Attributes.AddItem('ColorSpace', CreateIndexedColorArray(ABitmap));
      end
      else
      begin
{$ifndef fpc}
        for y := 0 to Height - 1 do
        begin
          pb := ScanLine[y];
          x := 0;
          while x < Width * PIXEL_COLOR_SIZE - 1 do
          begin
            b := pb[x];
            pb[x] := pb[x+2];
            pb[x+2] := b;
            Stream.Write(pb[x], 3);
            x := x + PIXEL_COLOR_SIZE;
          end;
          Attributes.AddItem('ColorSpace', TPdfName.CreateName('DeviceRGB'));
        end;
{$else}
        Alpha := TFPMemoryImage.Create(AImage.Width, AImage.Height);
        Alpha.UsePalette := true;
        Alpha.Palette.Count := 256;
        for x:=0 to $FF do
        begin
          aColor.Red:=x;
          aColor.Red:=(aColor.Red shl 8) + aColor.Red;
          aColor.Green:=aColor.Red;
          aColor.Blue:=aColor.Red;
          Alpha.Palette.Color[x]:=aColor;
        end;
        HasAlpha := false;

        for y := 0 to aintfimage.Height - 1 do
        begin
          new(pb);
          for x := 0 to aintfimage.Width-1 do
          begin
            aColor := aIntfImage.Colors[x,y];
            pb[ 0 ] := acolor.red shr 8;
            pb[ 1 ] := acolor.green shr 8;
            pb[ 2 ] := acolor.blue shr 8;
            Stream.write(pb[ 0 ], 3);

            b := acolor.alpha shr 8;
            Alpha.Pixels[x,y] := b;

            if acolor.Alpha<>AlphaOpaque then
              HasAlpha := true;
          end;
          dispose(pb);
          Attributes.AddItem('ColorSpace', TPdfName.CreateName('DeviceRGB'));
        end;

        if HasAlpha then begin
          MaskImage := CreateMaskStream(Alpha);
          if ObjectMgr<>nil then
            ObjectMgr.AddObject(MaskImage);
          Attributes.AddItem('SMask', MaskImage);
        end;

        Alpha.Free;
{$endif}
      end;

      with Attributes do
      begin
{$IFDEF FPC}
        AddItem('Width', TPdfNumber.CreateNumber(aintfimage.Width));
        AddItem('Height', TPdfNumber.CreateNumber(aintfimage.Height));
{$ELSE}
        AddItem('Width', TPdfNumber.CreateNumber(abitmap.Width));
        AddItem('Height', TPdfNumber.CreateNumber(abitmap.Height));
{$ENDIF}
        AddItem('BitsPerComponent', TPdfNumber.CreateNumber(8));
        if USE_ZLIB then
          PdfArrayByName('Filter').AddItem(TPdfName.CreateName('FlateDecode'));
      end;
    finally
      Free;
    end;
    
{$IFDEF FPC}
    aIntfImage.Free();
{$ENDIF}
  except
    result.Free;
    raise;
  end;
end;

initialization
  {$IFDEF LAZ_POWERPDF}
  PdfLazRegisterClassAlias(TPdfBitmapImage, 'Pdf-Bitmap');
  {$ELSE}
  RegisterClassAlias(TPdfBitmapImage, 'Pdf-Bitmap');
  {$ENDIF}

finalization
  UnRegisterClass(TPdfBitmapImage);

end.

