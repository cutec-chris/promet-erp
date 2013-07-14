unit ucrossrelation; 

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, IntfGraphics, LCLType, Graphics, Math;

type
  TRGBQuadArray = array[0..32767] of TRGBQuad;
  PRGBQuadArray = ^TRGBQuadArray;

  function ZNCC(ImgSrc, ImgIn: TLazIntfImage;Offset : TPoint):Single;

implementation

procedure TColor2RGB(const Color: TColor; var R, G, B: Byte); inline;
begin
  // convert hexa-decimal values to RGB
  R := Color and $FF;
  G := (Color shr 8) and $FF;
  B := (Color shr 16) and $FF;
end;

function RGB2TColor(const R, G, B: Byte): Integer; inline;
begin
  // convert hexa-decimal values to RGB
  Result := R + G shl 8 + B shl 16;
end;

// ZeroMeanNormalizedCross-Correlation (ZNCC) (wird MAXIMAL bei guter Übereinstimmung)

function ZNCC(ImgSrc, ImgIn: TLazIntfImage;Offset : TPoint):Single;
var
  x, y:integer;
  P1,P2:array [0..100] of PRGBQuadArray;

  a, b, zaehler, nenner1, nenner2, nenner, summe1, summe2, mean1, mean2:single;

  ZNCCvalue:Extended;
begin
  // ZeroMeanNormalizedCross-Correlation (ZNCC) (wird MAXIMAL bei guter Übereinstimmung)
  zaehler:=0.0;
  nenner1:=0.0;
  nenner2:=0.0;
  summe1:=0.0;
  summe2:=0.0;

  // Bildformat auf 24bit setzen (also ohne Alpha-Kanal)
//  Bild1.PixelFormat := pf24bit;
//  Bild2.PixelFormat := pf24bit;
  // Summen bilden
  for y:=0 to ImgSrc.Height-1 do
  begin
    P1[y]:=ImgSrc.GetDataLineStart(y);
    P2[y]:=ImgIn.GetDataLineStart(y+Offset.y);

    for x:=0 to ImgSrc.Width-1 do
    begin
      summe1:=summe1+RGB2TColor(P1[y][x].rgbRed, P1[y][x].rgbGreen, P1[y][x].rgbBlue);
      summe2:=summe2+RGB2TColor(P2[y][x+Offset.x].rgbRed, P2[y][x+Offset.x].rgbGreen, P2[y][x+Offset.x].rgbBlue);
    end;
  end;

  mean1:=(1/power((ImgSrc.Width-1)+(ImgSrc.Height-1)+1,2))*summe1;
  mean2:=(1/power((ImgSrc.Width-1)+(ImgSrc.Height-1)+1,2))*summe2;
  for x:=0 to ImgSrc.Width-1 do
  begin
    for y:=0 to ImgSrc.Height-1 do
    begin
      a:=RGB2TColor(P1[y][x].rgbRed, P1[y][x].rgbGreen, P1[y][x].rgbBlue)-mean1;
      b:=RGB2TColor(P2[y][x+Offset.x].rgbRed, P2[y][x+Offset.x].rgbGreen, P2[y][x+Offset.x].rgbBlue)-mean2;
      zaehler:=zaehler+(a*b);
      nenner1:=nenner1+power(a, 2);
      nenner2:=nenner2+power(b, 2);
    end;
  end;
  nenner:=sqrt(nenner1*nenner2);

  if nenner>0 then
    ZNCCvalue:=zaehler/nenner
  else
    ZNCCvalue:=0.0;

  result:=ZNCCvalue*100;
end;

end.

