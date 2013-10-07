unit ubasic;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,zint,uhelper;

function ZBarcode_Create(): PointerTo_zint_symbol;
procedure ZBarcode_Clear(symbol: PointerTo_zint_symbol);
procedure ZBarcode_Delete(symbol: PointerTo_zint_symbol);

implementation

function ZBarcode_Create(): PointerTo_zint_symbol;
var
  symbol: PointerTo_zint_symbol;
  i: Integer;
begin
  symbol := GetMem (SizeOf (zint_symbol));
  if not Assigned(symbol) then
  begin
    exit (nil);
  end;
  FillByte(symbol^, SizeOf (zint_symbol),0);
  symbol^.symbology := BARCODE_CODE128;
  symbol^.height := 0;
  symbol^.whitespace_width := 0;
  symbol^.border_width := 0;
  symbol^.output_options := 0;
  symbol^.rows := 0;
  symbol^.width := 0;
  strcpy (symbol^.fgcolour, '000000');
  strcpy (symbol^.bgcolour, 'ffffff');
  strcpy (symbol^.outfile, '');
  symbol^.scale := 1.0;
  symbol^.option_1 := -1;
  symbol^.option_2 := 0;
  symbol^.option_3 := 928;
  symbol^.show_hrt := 1;
  symbol^.input_mode := DATA_MODE;
  strcpy (symbol^.primary, '');
  FillByte (symbol^.encoded_data[0][0], SizeOf (symbol^.encoded_data),0);
  i := 0;
  while i < 178 do
  begin
    symbol^.row_height[i] := 0;
    Inc (i);
  end;
  symbol^.bitmap := nil;
  symbol^.bitmap_width := 0;
  symbol^.bitmap_height := 0;
  exit (symbol);
end;

procedure ZBarcode_Clear(symbol: PointerTo_zint_symbol);
var
  j: Integer;
  i: Integer;
begin
  i := 0;
  while i < symbol^.rows do
  begin
    j := 0;
    while j < symbol^.width do
    begin
      unset_module (symbol, i, j);
      Inc (j);
    end;
    Inc (i);
  end;
  symbol^.rows := 0;
  symbol^.width := 0;
  symbol^.text[0] := 0;
  symbol^.errtxt[0] := #0;
  if Assigned(symbol^.bitmap) then
  begin
    FreeMem (symbol^.bitmap);
  end;
  symbol^.bitmap := nil;
  symbol^.bitmap_width := 0;
  symbol^.bitmap_height := 0;
end;

procedure ZBarcode_Delete(symbol: PointerTo_zint_symbol);
var
  l: PointerTo_zint_render_line;
  line: PointerTo_zint_render_line;
  s: PointerTo_zint_render_string;
  TheString: PointerTo_zint_render_string;
begin
  if Boolean(symbol^.bitmap <> nil) then
  begin
    FreeMem (symbol^.bitmap);
  end;
  if Boolean(symbol^.rendered <> nil) then
  begin
    line := symbol^.rendered^.lines;
    while Assigned(line) do
    begin
      l := line;
      line := line^.next;
      FreeMem (l);
    end;
    TheString := symbol^.rendered^.strings;
    while Assigned(TheString) do
    begin
      s := TheString;
      TheString := TheString^.next;
      FreeMem (s^.text);
      FreeMem (s);
    end;
    FreeMem (symbol^.rendered);
  end;
  FreeMem (symbol);
end;

end.

