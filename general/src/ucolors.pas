unit uColors;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,Graphics,LCLIntf;

function Ligthen(InputColor: TColor; n: Extended): TColor;

implementation

function Ligthen(InputColor: TColor; n: Extended): TColor;
var
  r,g,b,y,u,v: integer;
  temp: Integer;
begin
  // RGB To YUV
  r := GetRValue(InputColor);
  g := GetGValue(InputColor);
  b := GetBValue(InputColor);
  y := g*150 + b*29 + r*77; // 0.587 x 256, 0.114 x 256, 0.299 x 256
  u := (round(b) shl 8 - y) * 144;         // 0.564 x 256
  v := (round(r) shl 8 - y) * 183;         // 0,713 x 256
  y :=round(y) shr 8;
  u :=round(u) shr 16 + $80;
  v :=round(v) shr 16 + $80;
  //Modify
  y := y+round(y*n);
  //YUV To RGB
  temp := y + (u - $80) * 256 div 144;
  if temp > 0 then b:=temp else b:=0;
  if temp > 255 then b:=255;
  temp := y + (v - $80) * 256 div 183  ;
  if temp > 0 then r:=temp else r:=0;
  if temp > 255 then r:=255;
  temp := (y shl 8 - u*29 - v*77) div 150;
  if temp > 0 then g:=temp else g:=0;
  if temp > 255 then g:=255;
  Result := RGB(byte(Round(r)),byte(Round(g)),byte(Round(b)));
end;

end.

