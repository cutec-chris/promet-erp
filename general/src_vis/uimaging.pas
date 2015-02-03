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
Created 06.03.2014
*******************************************************************************}
unit uImaging;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IntfGraphics, FPimage, LCLType,Graphics,FPImgCanv,
  GraphUtil,math
  {$IF FPC_FULLVERSION>=20601}
  ,FPImgGauss
  {$ENDIF}
  ;

type
  TRGBTripleArray = array[0..32767] of TRGBTriple;
  PRGBTripleArray = ^TRGBTripleArray;

procedure Sharpen(Input: TLazIntfImage; Correction: integer);
procedure Delight(Input: TFPCustomImage);

implementation

procedure Sharpen(Input: TLazIntfImage; Correction: integer);
type
  TRGBArray = array[word] of TRGBTriple;
  PRGBArray = ^TRGBArray;
var
  Filter: array[0..8] of integer; // matrice de 3 * 3 pixels
  Red, Green, Blue, NewR, NewG, NewB, I, PosX, PosY, mX, mY, dX, dY, Diviseur: integer;
  TabScanlineBmp: array of PRGBTripleArray;
  TabScanlineFinalBmp: array of PRGBTripleArray;
  FinalBmp: TLazIntfImage;
begin
  {$IF FPC_FULLVERSION>=20601}
  for I := 0 to High(Filter) do
    if I in [0, 2, 6, 8] then
      Filter[I] := -Correction
    else if I = 4 then
      Filter[I] := (Correction * 4) + 128 // +128 permet une correction bien Ă©talĂ©e
    else
      Filter[I] := 0;
  Diviseur := Filter[4] - (Correction * 4);

  FinalBmp := TLazIntfImage.CreateCompatible(Input, Input.Width, Input.Height);

  try
    FinalBmp.Assign(Input);
    SetLength(TabScanlineBmp, Input.Height);
    SetLength(TabScanlineFinalBmp, Input.Height);
    for I := 0 to Input.Height - 1 do
      begin
        TabScanlineBmp[I] := Input.GetDataLineStart(I);
        TabScanlineFinalBmp[I] := FinalBmp.GetDataLineStart(I);
      end;

    for PosY := 0 to Input.Height - 1 do
      for PosX := 0 to Input.Width - 1 do
        begin
          NewR := 0;
          NewG := 0;
          NewB := 0;
          for dY := -1 to 1 do
            for dX := -1 to 1 do
              begin
                //position du pixel Ĺ• traiter
                mY := PosY + dY;
                mX := PosX + dX;
                //VĂ©rification des limites pour Ă©viter les effets de bord
                //Lecture des composantes RGB de chaque pixel
                if (mY >= 1) and (mY <= Input.Height - 1) and
                  (mX >= 1) and (mX <= Input.Width - 1) then
                  begin
                    Red := TabScanlineBmp[mY]^[mX].rgbtRed;
                    Green := TabScanlineBmp[mY]^[mX].rgbtGreen;
                    Blue := TabScanlineBmp[mY]^[mX].rgbtBlue;
                  end
                else
                  begin
                    Red := TabScanlineBmp[PosY]^[PosX].rgbtRed;
                    Green := TabScanlineBmp[PosY]^[PosX].rgbtGreen;
                    Blue := TabScanlineBmp[PosY]^[PosX].rgbtBlue;
                  end;

                I := 4 + (dY * 3) + dX; // I peut varier de 0 Ĺ• 8
                NewR := NewR + Red * Filter[I];
                NewG := NewG + Green * Filter[I];
                NewB := NewB + Blue * Filter[I];
              end;

          NewR := NewR div Diviseur;
          NewG := NewG div Diviseur;
          NewB := NewB div Diviseur;
          if NewR > 255 then
            NewR := 255
          else if NewR < 0 then
            NewR := 0;
          if NewG > 255 then
            NewG := 255
          else if NewG < 0 then
            NewG := 0;
          if NewB > 255 then
            NewB := 255
          else if NewB < 0 then
            NewB := 0;
          TabScanlineFinalBmp[PosY]^[PosX].rgbtRed := NewR;
          TabScanlineFinalBmp[PosY]^[PosX].rgbtGreen := NewG;
          TabScanlineFinalBmp[PosY]^[PosX].rgbtBlue := NewB;
        end;

    Input.Assign(FinalBmp);

  finally
    FinalBmp.Free;
    Finalize(TabScanlineBmp);
    Finalize(TabScanlineFinalBmp);
  end;
  {$ENDIF}
end;

procedure Delight(Input: TFPCustomImage);
var
  aImage: TFPMemoryImage;
  aCanvas: TFPImageCanvas;
  bImage: TFPCustomImage;
  H: Byte;
  L: Byte;
  S: Byte;
  y: Integer;
  x: Integer;
  aPic: TPicture;
  cImage: TFPMemoryImage;
  cH: Byte;
  cL: Byte;
  cS: Byte;
  BlockX: Extended;
  BlockY: Extended;
  aBlockX: Integer;
  aBlockY: Integer;
  Row1: PRGBTripleArray;
  Row0: PRGBTripleArray;
begin
  {$IF FPC_FULLVERSION>=20601}
  bImage := Input;
  aImage := TFPMemoryImage.Create(round((100*bImage.Width)/bImage.Height),100);
  cImage := TFPMemoryImage.Create(bImage.Width,bImage.Height);
  cImage.Assign(bImage);
  Input.SaveToFile(GetTempDir+'rpv2.jpg');
  BlockX := bImage.Width/aImage.Width;
  BlockY := bImage.Height/aImage.Height;
  aCanvas := TFPImageCanvas.Create(aImage);
  aImage.UsePalette:=False;
  aCanvas.StretchDraw(0,0,aImage.Width,aImage.Height,bImage);
  for y := 1 to aImage.Height-1 do
    begin
      for x := 0 to aImage.Width-1 do
        begin
          ColorToHLS(FPColorToTColor(aImage.Colors[x,y-1]),H,L,S);
          ColorToHLS(FPColorToTColor(aImage.Colors[x,y]),cH,cL,cS);
          aImage.Colors[x,y] := TColorToFPColor(HLStoColor(cH,L+round(Cl-L),cS));
        end;
    end;
  GaussianBlur(aImage,6,Rect(0,0,aImage.Width,aImage.Height));
  aBlockX := 0;
  aBlockY := 0;
  ColorToHLS(FPColorToTColor(aImage.Colors[0,0]),H,L,S);
  for y := 0 to cImage.Height-1 do
    for x := 0 to cImage.Width-1 do
      begin
        if (trunc(x/BlockX)<>aBlockX)
        or (trunc(y/BlockY)+1<>aBlockY) then
          begin
            aBlockX := min(trunc(x/BlockX),aImage.Width-1);
            aBlockY := min(trunc(y/BlockY)+1,aImage.Height-1);
            ColorToHLS(FPColorToTColor(aImage.Colors[aBlockX,aBlockY]),H,L,S);
          end;
        ColorToHLS(FPColorToTColor(cImage.Colors[x,y]),cH,cL,cS);
        cImage.Colors[x,y] := TColorToFPColor(HLStoColor(cH,210+min(round(cL-L),255),cS));
      end;
  Input.Assign(cImage);
  aCanvas.Free;
  aImage.Free;
  cImage.Free;
  {$ENDIF}
end;


end.

