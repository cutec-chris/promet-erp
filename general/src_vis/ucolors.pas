unit uColors;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,Graphics,LCLIntf,GraphUtil;

function Ligthen(InputColor: TColor; n: Extended): TColor;

implementation

function Ligthen(InputColor: TColor; n: Extended): TColor;
var
  H, S, L: Word;
begin
  ColorRGBToHLS(InputColor, H, L, S);
  Result := ColorHLSToRGB(H, round(255*n), S);
end;

end.

