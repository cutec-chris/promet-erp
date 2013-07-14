unit fpthumbresize;

//22.6.2010 Theo

{$MODE objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpimage, FPImgCanv, FPCanvas;

function ThumbResize(SImg: TFPMemoryImage; W, H: integer; out Area: TRect): TFPMemoryImage;
function FpRawResize(w, h: integer; bmp: TFPMemoryImage): TFPMemoryImage;
procedure Proportional(sw, sh, tw, th: integer; out w, h: integer; out area: TRect);


implementation

function ThumbResize(SImg: TFPMemoryImage; W, H: integer; out Area: TRect): TFPMemoryImage;
var Canv: TFPImageCanvas;
  TmpI: TFPMemoryImage;
  rw, rh: integer;
begin
  if (SImg.Width > 2 * W) or (SImg.Height > 2 * H) then
  begin
    Proportional(SImg.Width, SImg.Height, 2 * W, 2 * H, rw, rh, Area);
    TmpI := FpRawResize(rw, rh, SImg);
    SImg.Assign(TmpI);
    TmpI.free;
  end;

  Proportional(SImg.Width, SImg.Height, W, H, rw, rh, Area);
  Result := TFPMemoryImage.Create(0, 0);
  Result.UsePalette:=false;
  Result.Width:=rw;
  Result.Height:=rh;

  Canv := TFPImageCanvas.create(Result);
  Canv.StretchDraw(0, 0, rw, rh, SImg);
  Canv.free;
end;

function MulDiv(Number, Num, Den: Integer): Integer;
begin
  if Den = 0 then
  begin
    Result := -1;
    Exit;
  end;
  Result := (Int64(Number) * Num) div Den;
end;


procedure Proportional(sw, sh, tw, th: integer; out w, h: integer; out area: TRect);
var half: integer;
begin
  if sw / sh < tw / th
    then begin
    area.Top := 0;
    area.Bottom := tw;
    w := MulDiv(th, sw, sh);
    h := th;
    half := (tw - w) div 2;
    area.Left := half;
    area.Right := area.Left + w;
  end
  else begin
    area.Left := 0;
    area.Right := tw;
    h := MulDiv(tw, sh, sw);
    w := tw;
    Half := (th - h) div 2;
    area.Top := half;
    area.Bottom := area.Top + h;
  end;
end;



function FpRawResize(w, h: integer; bmp: TFPMemoryImage): TFPMemoryImage;
var x, y: integer;
  zoomh, zoomw: single;
begin
  Result := TFPMemoryImage.Create(0, 0);
  Result.UsePalette:=false;

  if h > 0 then zoomh := h / bmp.Height;
  if w > 0 then zoomw := w / bmp.Width;

  if h = 0 then zoomh := zoomw;
  if w = 0 then zoomw := zoomh;

  w := Round(Bmp.Width * zoomw);
  h := Round(Bmp.Height * zoomh);

  Result.Height := h;
  Result.Width := w;
  for y := 0 to h - 1 do
    for x := 0 to w - 1 do
    begin
      Result.Colors[X, Y] := bmp.Colors[Round(x / zoomw), Round(y / zoomh)]
    end;
end;


end.