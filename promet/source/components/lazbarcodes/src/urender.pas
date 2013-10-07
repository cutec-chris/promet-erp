unit urender; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,zint,uhelper;

function render_plot_create_line(x: Single; y: Single; width: Single; length: Single): PointerTo_zint_render_line;
function render_plot_add_line(symbol: PointerTo_zint_symbol; line: PointerTo_zint_render_line; last_line: PointerTo_PointerTo_zint_render_line): Integer;
function render_plot_create_ring(x: Single; y: Single; radius: Single; line_width: Single): PointerTo_zint_render_ring;
function render_plot_add_ring(symbol: PointerTo_zint_symbol; ring: PointerTo_zint_render_ring; last_ring: PointerTo_PointerTo_zint_render_ring): Integer;
function render_plot_create_hexagon(x: Single; y: Single): PointerTo_zint_render_hexagon;
function render_plot_add_hexagon(symbol: PointerTo_zint_symbol; hexagon: PointerTo_zint_render_hexagon; last_hexagon: PointerTo_PointerTo_zint_render_hexagon): Integer;
function render_plot_add_string(symbol: PointerTo_zint_symbol; text: PBYTE; x: Single; y: Single; fsize: Single; width: Single; last_string: PointerTo_PointerTo_zint_render_string): Integer;
function render_plot(symbol: PointerTo_zint_symbol; width: Single; height: Single): Integer;

implementation

const GL_CONST=2.8346;

function render_plot(symbol: PointerTo_zint_symbol; width: Single; height: Single): Integer;
var
  render: PointerTo_zint_render;
  last_line: PointerTo_zint_render_line = nil;
  line: PointerTo_zint_render_line = nil;
  last_string: PointerTo_zint_render_string = nil;
  //last_ring: PointerTo_zint_render_ring = nil;
  //ring: PointerTo_zint_render_ring = nil;
  //last_hexagon: PointerTo_zint_render_hexagon = nil;
  //hexagon: PointerTo_zint_render_hexagon = nil;
  this_row: Integer;
  latch: Integer;
  block_width: Integer;
  r: Integer;
  i: Integer;
  row_posn: Single = 0.0;
  row_height: Single = 0.0;
  preset_height: Single = 0.0;
  large_bar_height: Single = 0.0;
  textwidth: Single = 0.0;
  textpos: Single = 0.0;
  addon_width_x: Integer;
  main_symbol_width_x: Integer;
  textdone: Integer;
  yoffset: Integer;
  xoffset: Integer;
  text_height: Integer;
  text_offset: Integer;
  textpart: array [0..10-1] of Char;
  addon: array [0..6-1] of Char;
  total_area_width_x: Integer;
  total_symbol_width_x: Integer;
  symbol_lead_in: Integer;
  large_bar_count: Integer;
  addon_text_posn: Single;
  default_text_posn: Single;
  scaler: Single;
  //locale: pchar = nil;
  hide_text: Integer = 0;
  required_aspect: Single;
  symbol_aspect: Single = 1;
  x_dimension: Single;
  upceanflag: Integer = 0;
  addon_latch: Integer = 0;
begin
  Result:=0;
  {INITCODE} last_line := nil;
  {INITCODE} line := nil;
  {INITCODE} last_string := nil;
  //{INITCODE} last_ring := nil;
  //{INITCODE} ring := nil;
  //{INITCODE} last_hexagon := nil;
  //{INITCODE} hexagon := nil;
  {INITCODE} row_posn := 0.0;
  {INITCODE} row_height := 0.0;
  {INITCODE} preset_height := 0.0;
  {INITCODE} large_bar_height := 0.0;
  {INITCODE} textwidth := 0.0;
  {INITCODE} textpos := 0.0;
  //{INITCODE} locale := nil;
  {INITCODE} hide_text := 0;
  {INITCODE} symbol_aspect := 1;
  {INITCODE} upceanflag := 0;
  GetMem(Symbol^.rendered,sizeof(zint_render));
  render:=Symbol^.rendered;

  render^.lines := nil;
  render^.strings := nil;
  render^.rings := nil;
  render^.hexagons := nil;
//  locale := setlocale (LC_ALL, 'C');
  row_height := 0;
  textdone := 0;
  textpos := 0.0;
  main_symbol_width_x := symbol^.width;
  addon[0]:=#0;
  strcpy (addon, '');
  symbol_lead_in := 0;
  addon_text_posn := 0.0;
  addon_width_x := 0;
  latch := 0;
  r := 0;
  if IsTrue(is_extendable (symbol^.symbology)) then
  begin
    i := 0;
    while i < sysutils.strlen (@symbol^.text[0]) do
    begin
      if IsTrue(latch = 1) then
      begin
        addon[r] := char(symbol^.text[i]);
        Inc (r);
      end;
      if IsTrue(char(symbol^.text[i]) = '+') then
      begin
        latch := 1;
      end;
      Inc (i);
    end;
  end;
  addon[r] := #0;
  if (symbol^.show_hrt=0) or (sysutils.strlen (@symbol^.text[0]) = 0) then
  begin
    hide_text := 1;
    text_offset := 0;
    text_height := 0;
  end else begin
    text_height := 9;
    text_offset := 2;
  end;
  while not (module_is_set (symbol, symbol^.rows - 1, symbol_lead_in)) do
  begin
    Inc (symbol_lead_in);
  end;
  if IsTrue(((((((symbol^.symbology = BARCODE_EANX)) and ((symbol^.rows = 1)))) or ((symbol^.symbology = BARCODE_EANX_CC)))) or ((symbol^.symbology = BARCODE_ISBNX))) then
  begin
    case sysutils.strlen (@symbol^.text[0]) of
      13,
      16,
      19:
        begin
          if IsTrue(symbol^.whitespace_width = 0) then
          begin
            symbol^.whitespace_width := 10;
          end;
          main_symbol_width_x := 96 + symbol_lead_in;
          upceanflag := 13;
        end;
      2:
        begin
          main_symbol_width_x := 22 + symbol_lead_in;
          upceanflag := 2;
        end;
      5:
        begin
          main_symbol_width_x := 49 + symbol_lead_in;
          upceanflag := 5;
        end;
      otherwise
        begin
          main_symbol_width_x := 68 + symbol_lead_in;
          upceanflag := 8;
        end;
    end;
    case sysutils.strlen (@symbol^.text[0]) of
      11,
      16: addon_width_x := 31;
      14,
      19: addon_width_x := 58;
    end;
  end;
  if IsTrue(((((symbol^.symbology = BARCODE_UPCA)) and ((symbol^.rows = 1)))) or ((symbol^.symbology = BARCODE_UPCA_CC))) then
  begin
    upceanflag := 12;
    if Boolean(symbol^.whitespace_width < 10) then
    begin
      symbol^.whitespace_width := 10;
      main_symbol_width_x := 96 + symbol_lead_in;
    end;
    case sysutils.strlen (@symbol^.text[0]) of
      15: addon_width_x := 31;
      18: addon_width_x := 58;
    end;
  end;
  if Boolean(((((symbol^.symbology = BARCODE_UPCE)) and ((symbol^.rows = 1)))) or ((symbol^.symbology = BARCODE_UPCE_CC))) then
  begin
    upceanflag := 6;
    if symbol^.whitespace_width = 0 then
    begin
      symbol^.whitespace_width := 10;
      main_symbol_width_x := 51 + symbol_lead_in;
    end;
    case sysutils.strlen (@symbol^.text[0]) of
      11: addon_width_x := 31;
      14: addon_width_x := 58;
    end;
  end;
  total_symbol_width_x := main_symbol_width_x + addon_width_x;
  total_area_width_x := total_symbol_width_x + (2 * (symbol^.border_width + symbol^.whitespace_width));
  xoffset := symbol^.border_width + symbol^.whitespace_width;
  yoffset := symbol^.border_width;
  large_bar_count := 0;
  preset_height := 0.0;
  i := 0;
  while i < symbol^.rows do
  begin
    preset_height := preset_height + (symbol^.row_height[i]);
    if symbol^.row_height[i] = 0 then
    begin
      Inc (large_bar_count);
    end;
    Inc (i);
  end;
  if large_bar_count = 0 then
  begin
    required_aspect := width / height;
    symbol_aspect := (total_symbol_width_x + (2 * xoffset)) / (preset_height + (2 * yoffset) + text_offset + text_height);
    symbol^.height := trunc(preset_height);
    if required_aspect > symbol_aspect then
    begin
      scaler := height / (preset_height + (2 * yoffset) + text_offset + text_height);
      render^.width := symbol_aspect * height;
      render^.height := height;
    end else begin
      scaler := width / (total_symbol_width_x + (2 * xoffset));
      render^.width := width;
      render^.height := width / symbol_aspect;
    end;
  end else begin
    scaler := width / (total_symbol_width_x + (2 * xoffset));
    symbol^.height := Trunc((height / scaler) - ((2 * yoffset) + text_offset + text_height));
    render^.width := width;
    render^.height := height;
  end;
  if large_bar_count=0 then begin
    large_bar_height:=0;
  end else begin
    large_bar_height := (symbol^.height - preset_height) / large_bar_count;
  end;
  if ((symbol^.output_options and BARCODE_BOX) <> 0) or ((symbol^.output_options and BARCODE_BIND) <> 0) then
  begin
    default_text_posn := (symbol^.height + text_offset + symbol^.border_width + symbol^.border_width) * scaler;
  end else begin
    default_text_posn := (symbol^.height + text_offset + symbol^.border_width) * scaler;
  end;
  x_dimension := render^.width / total_area_width_x;
  x_dimension := x_dimension / GL_CONST;


  if render^.height < ((x_dimension * ((2 * symbol^.border_width) + text_offset + text_height)) + 2.0) * GL_CONST then
  begin
    render^.height := ((x_dimension * ((2 * symbol^.border_width) + text_offset + text_height)) + 2.0) * GL_CONST;
  end;
  if render^.width < (2.0 * GL_CONST) then
  begin
    render^.width := (2.0 * GL_CONST);
  end;
  if symbol^.symbology = BARCODE_CODABAR then
  begin
    if x_dimension < 0.191 then
    begin
      render^.width := 0.191 * GL_CONST * total_area_width_x;
    end;
    if render^.height < ((x_dimension * ((2 * symbol^.border_width) + text_offset + text_height)) + 5.0) * GL_CONST then
    begin
      render^.height := ((x_dimension * ((2 * symbol^.border_width) + text_offset + text_height)) + 5.0) * GL_CONST;
    end;
  end;
  if symbol^.symbology = BARCODE_CODE49 then
  begin
    if x_dimension < 0.191 then
    begin
      render^.width := 0.191 * GL_CONST * total_area_width_x;
      render^.height := render^.width / symbol_aspect;
    end;
  end;
  if upceanflag <> 0 then
  begin
    render^.width := 0.330 * GL_CONST * total_area_width_x;
    case upceanflag of
      6,
      12,
      13: render^.height := ((0.330 * ((2 * symbol^.border_width) + text_offset + text_height)) + 22.85) * GL_CONST;
      8: render^.height := ((0.330 * ((2 * symbol^.border_width) + text_offset + text_height)) + 18.23) * GL_CONST;
      otherwise
        render^.height := ((0.330 * ((2 * symbol^.border_width) + text_offset + text_height)) + 21.10) * GL_CONST;
    end;
  end;
(*
  if Boolean(symbol^.symbology = BARCODE_ONECODE) then
  begin
    render^.width := <*Error: Missing expression 191 *> * GL_CONST * total_area_width_x;
    render^.height := <*Error: Missing expression 191 *> * GL_CONST;
  end;
  if Boolean(((symbol^.symbology = BARCODE_POSTNET)) or ((symbol^.symbology = BARCODE_PLANET))) then
  begin
    render^.width := <*Error: Missing expression 191 *> * GL_CONST * total_area_width_x;
    render^.height := <*Error: Missing expression 191 *> * GL_CONST;
  end;
  if Boolean(((((symbol^.symbology = BARCODE_AUSPOST)) or ((symbol^.symbology = BARCODE_AUSREPLY)))) or ((((symbol^.symbology = BARCODE_AUSROUTE)) or ((symbol^.symbology = BARCODE_AUSREDIRECT))))) then
  begin
    render^.width := <*Error: Missing expression 191 *> * GL_CONST * total_area_width_x;
    render^.height := <*Error: Missing expression 191 *> * GL_CONST;
  end;
  if Boolean(((symbol^.symbology = BARCODE_RM4SCC)) or ((symbol^.symbology = BARCODE_KIX))) then
  begin
    render^.width := <*Error: Missing expression 191 *> * GL_CONST * total_area_width_x;
    render^.height := <*Error: Missing expression 191 *> * GL_CONST;
  end;
*)

  if symbol^.symbology = BARCODE_MAXICODE then
  begin
(*    scaler := GL_CONST;
    render^.width := <*Error: Missing expression 191 *> * scaler;
    render^.height := <*Error: Missing expression 191 *> * scaler;
    ring := render_plot_create_ring (<*Error: Missing expression 191 *> * scaler, <*Error: Missing expression 191 *> * scaler, <*Error: Missing expression 191 *> * scaler, <*Error: Missing expression 191 *> * scaler);
    render_plot_add_ring (symbol, ring, @last_ring);
    ring := render_plot_create_ring (<*Error: Missing expression 191 *> * scaler, <*Error: Missing expression 191 *> * scaler, <*Error: Missing expression 191 *> * scaler, <*Error: Missing expression 191 *> * scaler);
    render_plot_add_ring (symbol, ring, @last_ring);
    ring := render_plot_create_ring (<*Error: Missing expression 191 *> * scaler, <*Error: Missing expression 191 *> * scaler, <*Error: Missing expression 191 *> * scaler, <*Error: Missing expression 191 *> * scaler);
    render_plot_add_ring (symbol, ring, @last_ring);
    r := 0;
    while r < symbol^.rows do
    begin
      i := 0;
      while i < symbol^.width do
      begin
        if Boolean(module_is_set (symbol, r, i)) then
        begin
          hexagon := render_plot_create_hexagon (((i * <*Error: Missing expression 191 *>) + (iif (r and 1,<*Error: Missing expression 191 *>,<*Error: Missing expression 191 *>))) * scaler, ((r * <*Error: Missing expression 191 *>) + <*Error: Missing expression 191 *>) * scaler);
          render_plot_add_hexagon (symbol, hexagon, @last_hexagon);
        end;
        Inc (i);
      end;
      Inc (r);
    end;*)
  end else begin
    {INITCODE} addon_latch := 0;
    r := 0;
    while r < symbol^.rows do
    begin
      this_row := r;
      if symbol^.row_height[this_row] = 0 then
      begin
        row_height := large_bar_height;
      end else begin
        row_height := symbol^.row_height[this_row];
      end;
      row_posn := 0;
      i := 0;
      while i < r do
      begin
        if symbol^.row_height[i] = 0 then
        begin
          row_posn := row_posn + large_bar_height;
        end else begin
          row_posn := row_posn + (symbol^.row_height[i]);
        end;
        Inc (i);
      end;
      row_posn := row_posn + yoffset;
      i := 0;
      if module_is_set (symbol, this_row, 0) then
      begin
        latch := 1;
      end else begin
        latch := 0;
      end;
      repeat
        block_width := 0;
        repeat
          Inc (block_width);
        until (module_is_set (symbol, this_row, i + block_width) <> module_is_set (symbol, this_row, i));
        if (((addon_latch = 0)) and ((r = (symbol^.rows - 1)))) and ((i > main_symbol_width_x)) then
        begin
          addon_text_posn := row_posn * scaler;
          addon_latch := 1;
        end;
        if latch = 1 then
        begin
          if addon_latch = 0 then
          begin
            line := render_plot_create_line ((i + xoffset) * scaler, (row_posn) * scaler, block_width * scaler, row_height * scaler);
          end else begin
            line := render_plot_create_line ((i + xoffset) * scaler, (row_posn + 10.0) * scaler, block_width * scaler, (row_height - 5.0) * scaler);
          end;
          latch := 0;
          render_plot_add_line (symbol, line, @last_line);
        end else begin
          latch := 1;
        end;
        i := i + block_width;
      until not (i < symbol^.width);
      Inc (r);
    end;
  end;
  xoffset := xoffset - symbol_lead_in;
  row_posn := (row_posn + large_bar_height) * scaler;
  if NotBoolean(hide_text) then
  begin
    if upceanflag = 8 then
    begin
      i := 0;
      line := symbol^.rendered^.lines;
      while line <> nil do
      begin
        case i of
          0,
          1,
          10,
          11,
          20,
          21: line^.length := line^.length + ((5.0 * scaler));
        end;
        Inc (i);
        line := line^.next;
      end;
      i := 0;
      while i < 4 do
      begin
        textpart[i] := char(symbol^.text[i]);
        Inc (i);
      end;
      textpart[4] := #0;
      textpos := 17;
      textwidth := 4.0 * 8.5;
      render_plot_add_string (symbol, PBYTE (@textpart[0]), (textpos + xoffset) * scaler, default_text_posn, 11.0 * scaler, textwidth * scaler, @last_string);
      i := 0;
      while i < 4 do
      begin
        textpart[i] := char(symbol^.text[i + 4]);
        Inc (i);
      end;
      textpart[4] := #0;
      textpos := 50;
      render_plot_add_string (symbol, PBYTE (@textpart[0]), (textpos + xoffset) * scaler, default_text_posn, 11.0 * scaler, textwidth * scaler, @last_string);
      textdone := 1;
      case strlen (addon) of
        2:
          begin
            textpos := xoffset + 86;
            textwidth := 2.0 * 8.5;
            render_plot_add_string (symbol, PBYTE (@addon[0]), textpos * scaler, addon_text_posn * scaler, 11.0 * scaler, textwidth * scaler, @last_string);
          end;
        5:
          begin
            textpos := xoffset + 100;
            textwidth := 5.0 * 8.5;
            render_plot_add_string (symbol, PBYTE (@addon[0]), textpos * scaler, addon_text_posn * scaler, 11.0 * scaler, textwidth * scaler, @last_string);
          end;
      end;
    end;
    (*
    if Boolean(upceanflag = 13) then
    begin
      i := 0;
      line := symbol^.rendered^.lines;
      while line <> nil do
      begin
        case i of
          0,
          1,
          14,
          15,
          28,
          29: line^.length := line^.length + ((<*Error: Missing expression 191 *> * scaler));
        end;
        Inc (i);
        line := line^.next;
      end;
      textpart[0] := symbol^.text[0];
      textpart[1] := '\0';
      textpos := -5;
      textwidth := <*Error: Missing expression 191 *>;
      render_plot_add_string (symbol, PBYTE (textpart), (textpos + xoffset) * scaler, default_text_posn, <*Error: Missing expression 191 *> * scaler, textwidth * scaler, @last_string);
      i := 0;
      while i < 6 do
      begin
        textpart[i] := symbol^.text[i + 1];
        Inc (i);
      end;
      textpart[6] := '\0';
      textpos := 25;
      textwidth := <*Error: Missing expression 191 *> * <*Error: Missing expression 191 *>;
      render_plot_add_string (symbol, PBYTE (textpart), (textpos + xoffset) * scaler, default_text_posn, <*Error: Missing expression 191 *> * scaler, textwidth * scaler, @last_string);
      i := 0;
      while i < 6 do
      begin
        textpart[i] := symbol^.text[i + 7];
        Inc (i);
      end;
      textpart[6] := '\0';
      textpos := 72;
      render_plot_add_string (symbol, PBYTE (textpart), (textpos + xoffset) * scaler, default_text_posn, <*Error: Missing expression 191 *> * scaler, textwidth * scaler, @last_string);
      textdone := 1;
      case strlen (addon) of
        2:
          begin
            textpos := xoffset + 114;
            textwidth := <*Error: Missing expression 191 *> * <*Error: Missing expression 191 *>;
            render_plot_add_string (symbol, PBYTE (addon), textpos * scaler, addon_text_posn * scaler, <*Error: Missing expression 191 *> * scaler, textwidth * scaler, @last_string);
          end;
        5:
          begin
            textpos := xoffset + 128;
            textwidth := <*Error: Missing expression 191 *> * <*Error: Missing expression 191 *>;
            render_plot_add_string (symbol, PBYTE (addon), textpos * scaler, addon_text_posn * scaler, <*Error: Missing expression 191 *> * scaler, textwidth * scaler, @last_string);
          end;
      end;
    end;
    *)
    (*
    if Boolean(upceanflag = 12) then
    begin
      i := 0;
      line := symbol^.rendered^.lines;
      while line <> nil do
      begin
        case i of
          0:
          1:
          2:
          3:
          14:
          15:
          26:
          27:
          28:
          29: line^.length := line^.length + ((<*Error: Missing expression 191 *> * scaler));
        end;
        Inc (i);
        line := line^.next;
      end;
      textpart[0] := symbol^.text[0];
      textpart[1] := '\0';
      textpos := -5;
      textwidth := <*Error: Missing expression 191 *>;
      render_plot_add_string (symbol, PBYTE (textpart), (textpos + xoffset) * scaler, default_text_posn + (<*Error: Missing expression 191 *> * scaler), <*Error: Missing expression 191 *> * scaler, textwidth * scaler, @last_string);
      i := 0;
      while i < 5 do
      begin
        textpart[i] := symbol^.text[i + 1];
        Inc (i);
      end;
      textpart[5] := '\0';
      textpos := 27;
      textwidth := <*Error: Missing expression 191 *> * <*Error: Missing expression 191 *>;
      render_plot_add_string (symbol, PBYTE (textpart), (textpos + xoffset) * scaler, default_text_posn, <*Error: Missing expression 191 *> * scaler, textwidth * scaler, @last_string);
      i := 0;
      while i < 5 do
      begin
        textpart[i] := symbol^.text[i + 6];
        Inc (i);
      end;
      textpos := 68;
      render_plot_add_string (symbol, PBYTE (textpart), (textpos + xoffset) * scaler, default_text_posn, <*Error: Missing expression 191 *> * scaler, textwidth * scaler, @last_string);
      textpart[0] := symbol^.text[11];
      textpart[1] := '\0';
      textpos := 100;
      textwidth := <*Error: Missing expression 191 *>;
      render_plot_add_string (symbol, PBYTE (textpart), (textpos + xoffset) * scaler, default_text_posn + (<*Error: Missing expression 191 *> * scaler), <*Error: Missing expression 191 *> * scaler, textwidth * scaler, @last_string);
      textdone := 1;
      case strlen (addon) of
        2:
          begin
            textpos := xoffset + 116;
            textwidth := <*Error: Missing expression 191 *> * <*Error: Missing expression 191 *>;
            render_plot_add_string (symbol, PBYTE (addon), textpos * scaler, addon_text_posn * scaler, <*Error: Missing expression 191 *> * scaler, textwidth * scaler, @last_string);
          end;
        5:
          begin
            textpos := xoffset + 130;
            textwidth := <*Error: Missing expression 191 *> * <*Error: Missing expression 191 *>;
            render_plot_add_string (symbol, PBYTE (addon), textpos * scaler, addon_text_posn * scaler, <*Error: Missing expression 191 *> * scaler, textwidth * scaler, @last_string);
          end;
      end;
    end;
    *)
    (*
    if Boolean(upceanflag = 6) then
    begin
      i := 0;
      line := symbol^.rendered^.lines;
      while line <> nil do
      begin
        case i of
          0:
          1:
          14:
          15:
          16: line^.length := line^.length + ((<*Error: Missing expression 191 *> * scaler));
        end;
        Inc (i);
        line := line^.next;
      end;
      textpart[0] := symbol^.text[0];
      textpart[1] := '\0';
      textpos := -5;
      textwidth := <*Error: Missing expression 191 *>;
      render_plot_add_string (symbol, PBYTE (textpart), (textpos + xoffset) * scaler, default_text_posn + (<*Error: Missing expression 191 *> * scaler), <*Error: Missing expression 191 *> * scaler, textwidth * scaler, @last_string);
      i := 0;
      while i < 6 do
      begin
        textpart[i] := symbol^.text[i + 1];
        Inc (i);
      end;
      textpart[6] := '\0';
      textpos := 24;
      textwidth := <*Error: Missing expression 191 *> * <*Error: Missing expression 191 *>;
      render_plot_add_string (symbol, PBYTE (textpart), (textpos + xoffset) * scaler, default_text_posn, <*Error: Missing expression 191 *> * scaler, textwidth * scaler, @last_string);
      textpart[0] := symbol^.text[7];
      textpart[1] := '\0';
      textpos := 55;
      textwidth := <*Error: Missing expression 191 *>;
      render_plot_add_string (symbol, PBYTE (textpart), (textpos + xoffset) * scaler, default_text_posn + (<*Error: Missing expression 191 *> * scaler), <*Error: Missing expression 191 *> * scaler, textwidth * scaler, @last_string);
      textdone := 1;
      case strlen (addon) of
        2:
          begin
            textpos := xoffset + 70;
            textwidth := <*Error: Missing expression 191 *> * <*Error: Missing expression 191 *>;
            render_plot_add_string (symbol, PBYTE (addon), textpos * scaler, addon_text_posn * scaler, <*Error: Missing expression 191 *> * scaler, textwidth * scaler, @last_string);
          end;
        5:
          begin
            textpos := xoffset + 84;
            textwidth := <*Error: Missing expression 191 *> * <*Error: Missing expression 191 *>;
            render_plot_add_string (symbol, PBYTE (addon), textpos * scaler, addon_text_posn * scaler, <*Error: Missing expression 191 *> * scaler, textwidth * scaler, @last_string);
          end;
      end;
    end;
    *)
    if textdone = 0 then
    begin
      render_plot_add_string (symbol, symbol^.text, ((symbol^.width / 2.0) + xoffset) * scaler, default_text_posn, 9.0 * scaler, 0.0, @last_string);
    end;
  end;
  case symbol^.symbology of
    BARCODE_MAXICODE:
      begin
        //Do nothing...
      end;
    otherwise
      begin
        if (symbol^.output_options and BARCODE_BIND) <> 0 then
        begin
          if (symbol^.rows > 1) and is_stackable (symbol^.symbology) then
          begin
            r := 1;
            while r < symbol^.rows do
            begin
              line := render_plot_create_line (xoffset * scaler, ((r * row_height) + yoffset - 1) * scaler, symbol^.width * scaler, 2.0 * scaler);
              render_plot_add_line (symbol, line, @last_line);
              Inc (r);
            end;
          end;
        end;
        if (((symbol^.output_options and BARCODE_BOX) <> 0)) or (((symbol^.output_options and BARCODE_BIND) <> 0)) then
        begin
          line := render_plot_create_line (0, 0, (symbol^.width + xoffset + xoffset) * scaler, symbol^.border_width * scaler);
          render_plot_add_line (symbol, line, @last_line);
          line := render_plot_create_line (0, (symbol^.height + symbol^.border_width) * scaler, (symbol^.width + xoffset + xoffset) * scaler, symbol^.border_width * scaler);
          render_plot_add_line (symbol, line, @last_line);
        end;
        if (symbol^.output_options and BARCODE_BOX) <> 0 then
        begin
          line := render_plot_create_line (0, 0, symbol^.border_width * scaler, (symbol^.height + (2 * symbol^.border_width)) * scaler);
          render_plot_add_line (symbol, line, @last_line);
          line := render_plot_create_line ((symbol^.width + xoffset + xoffset - symbol^.border_width) * scaler, 0, symbol^.border_width * scaler, (symbol^.height + (2 * symbol^.border_width)) * scaler);
          render_plot_add_line (symbol, line, @last_line);
        end;
      end;
  end;
  (*
  if Boolean(locale) then
  begin
    setlocale (LC_ALL, locale);
  end;
  *)
  Result:=1;
end;

function render_plot_create_line(x: Single; y: Single; width: Single; length: Single): PointerTo_zint_render_line;
var
  line: PointerTo_zint_render_line;
begin
  line := GetMem (SizeOf (zint_render_line));
  line^.next := nil;
  line^.x := x;
  line^.y := y;
  line^.width := width;
  line^.length := length;
  exit (line);
end;

function render_plot_add_line(symbol: PointerTo_zint_symbol; line: PointerTo_zint_render_line; last_line: PointerTo_PointerTo_zint_render_line): Integer;
begin
  if last_line^<>nil then
  begin
    (last_line^)^.next := line;
  end else begin
    symbol^.rendered^.lines := line;
  end;
  last_line^ := line;
  exit (1);
end;

function render_plot_create_ring(x: Single; y: Single; radius: Single; line_width: Single): PointerTo_zint_render_ring;
var
  ring: PointerTo_zint_render_ring;
begin
  ring := GetMem (SizeOf (zint_render_ring));
  ring^.next := nil;
  ring^.x := x;
  ring^.y := y;
  ring^.radius := radius;
  ring^.line_width := line_width;
  exit (ring);
end;

function render_plot_add_ring(symbol: PointerTo_zint_symbol; ring: PointerTo_zint_render_ring; last_ring: PointerTo_PointerTo_zint_render_ring): Integer;
begin
  if last_ring^<>nil then
  begin
    (last_ring^)^.next := ring;
  end else begin
    symbol^.rendered^.rings := ring;
  end;
  last_ring^ := ring;
  exit (1);
end;

function render_plot_create_hexagon(x: Single; y: Single): PointerTo_zint_render_hexagon;
var
  hexagon: PointerTo_zint_render_hexagon;
begin
  hexagon := GetMem (SizeOf (zint_render_hexagon));
  hexagon^.next := nil;
  hexagon^.x := x;
  hexagon^.y := y;
  exit (hexagon);
end;

function render_plot_add_hexagon(symbol: PointerTo_zint_symbol; hexagon: PointerTo_zint_render_hexagon; last_hexagon: PointerTo_PointerTo_zint_render_hexagon): Integer;
begin
  if last_hexagon^<>nil then
  begin
    (last_hexagon^)^.next := hexagon;
  end else begin
    symbol^.rendered^.hexagons := hexagon;
  end;
  last_hexagon^ := hexagon;
  exit (1);
end;

function render_plot_add_string(symbol: PointerTo_zint_symbol; text: PBYTE; x: Single; y: Single; fsize: Single; width: Single; last_string: PointerTo_PointerTo_zint_render_string): Integer;
var
  pstring: PointerTo_zint_render_string;
begin
  pstring := GetMem (SizeOf (zint_render_string));
  pstring^.next := nil;
  pstring^.x := x;
  pstring^.y := y;
  pstring^.width := width;
  pstring^.fsize := fsize;
  pstring^.length := sysutils.strlen (pchar(text));
  pstring^.text := GetMem (SizeOf (BYTE) * (sysutils.strlen (pchar(text)) + 1));
  strcpy (pchar(pstring^.text), pchar(text));
  if last_string^<>nil then
  begin
    (last_string^)^.next := pstring;
  end else begin
    symbol^.rendered^.strings := pstring;
  end;
  last_string^ := pstring;
  exit (1);
end;

end.

