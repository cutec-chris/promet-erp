unit udatamatrix;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,uhelper,ureedsolomon,zint;

const
  MAXBARCODE=3116;

function dmatrix(symbol: PointerTo_zint_symbol; source: PBYTE; length: Integer): Integer;

implementation

const

DM_ASCII  =1;
DM_C40    =2;
DM_TEXT   =3;
DM_X12    =4;
DM_EDIFACT=5;
DM_BASE256=6;

c40_shift: array [0..127] of integer = (
	1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
	0, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
	2, 2, 2, 2, 2, 2, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
	2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3
);

c40_value: array [0..127] of integer = (
	0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,
	3,0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,4,5,6,7,8,9,10,11,12,13,
	15,16,17,18,19,20,21,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,
	22,23,24,25,26,0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31
);

text_shift: array [0..127] of integer = (
	1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
	0, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
	2, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
	2, 2, 2, 2, 2, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3, 3, 3, 3, 3
);

text_value: array [0..127] of integer = (
	0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,
	3,0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,4,5,6,7,8,9,10,11,12,13,
	15,16,17,18,19,20,21,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,
	22,23,24,25,26,0,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,27,28,29,30,31
);

intsymbol: array [0..29] of integer = (
	0,1,3,5,7,8,10,12,13,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,2,4,6,9,11,14
);

matrixH: array [0..29] of integer = (
	10, 12, 8, 14, 8, 16, 12, 18, 20, 12, 22, 16, 24, 26, 16, 32, 36, 40, 44, 48,
	52, 64, 72, 80, 88, 96, 104, 120, 132, 144
);

matrixW: array [0..29] of integer = (
	10, 12, 18, 14, 32, 16, 26, 18, 20, 36, 22, 36, 24, 26, 48, 32, 36, 40, 44,
	48, 52, 64, 72, 80, 88, 96, 104, 120, 132, 144
);

matrixFH: array [0..29] of integer = (
	10, 12, 8, 14, 8, 16, 12, 18, 20, 12, 22, 16, 24, 26, 16, 16, 18, 20, 22, 24,
	26, 16, 18, 20, 22, 24, 26, 20, 22, 24
);

matrixFW: array [0..29] of integer = (
	10, 12, 18, 14, 16, 16, 26, 18, 20, 18, 22, 18, 24, 26, 24, 16, 18, 20, 22,
	24, 26, 16, 18, 20, 22, 24, 26, 20, 22, 24
);

matrixbytes: array [0..29] of integer = (
	3, 5, 5, 8, 10, 12, 16, 18, 22, 22, 30, 32, 36, 44, 49, 62, 86, 114, 144,
	174, 204, 280, 368, 456, 576, 696, 816, 1050, 1304, 1558
);

matrixdatablock: array [0..29] of integer = (
	3, 5, 5, 8, 10, 12, 16, 18, 22, 22, 30, 32, 36, 44, 49, 62, 86, 114, 144,
	174, 102, 140, 92, 114, 144, 174, 136, 175, 163, 156
);

matrixrsblock: array [0..29] of integer = (
	5, 7, 7, 10, 11, 12, 14, 14, 18, 18, 20, 24, 24, 28, 28, 36, 42, 48, 56, 68,
	42, 56, 36, 48, 56, 68, 56, 68, 62, 62
);


procedure ecc200placementbit(parray: PInteger; NR: Integer; NC: Integer; r: Integer; c: Integer; p: Integer; b: BYTE);
begin
  if IsTrue(r < 0) then
  begin
    r := r + NR;
    c := c + (4 - ((NR + 4) mod 8));
  end;
  if IsTrue(c < 0) then
  begin
    c := c + NC;
    r := r + (4 - ((NC + 4) mod 8));
  end;
  parray[r * NC + c] := (p shl 3) + b;
end;

procedure ecc200placementblock(parray: PInteger; NR: Integer; NC: Integer; r: Integer; c: Integer; p: Integer);
begin
  ecc200placementbit (parray, NR, NC, r - 2, c - 2, p, 7);
  ecc200placementbit (parray, NR, NC, r - 2, c - 1, p, 6);
  ecc200placementbit (parray, NR, NC, r - 1, c - 2, p, 5);
  ecc200placementbit (parray, NR, NC, r - 1, c - 1, p, 4);
  ecc200placementbit (parray, NR, NC, r - 1, c - 0, p, 3);
  ecc200placementbit (parray, NR, NC, r - 0, c - 2, p, 2);
  ecc200placementbit (parray, NR, NC, r - 0, c - 1, p, 1);
  ecc200placementbit (parray, NR, NC, r - 0, c - 0, p, 0);
end;

procedure ecc200placementcornerA(parray: PInteger; NR: Integer; NC: Integer; p: Integer);
begin
  ecc200placementbit (parray, NR, NC, NR - 1, 0, p, 7);
  ecc200placementbit (parray, NR, NC, NR - 1, 1, p, 6);
  ecc200placementbit (parray, NR, NC, NR - 1, 2, p, 5);
  ecc200placementbit (parray, NR, NC, 0, NC - 2, p, 4);
  ecc200placementbit (parray, NR, NC, 0, NC - 1, p, 3);
  ecc200placementbit (parray, NR, NC, 1, NC - 1, p, 2);
  ecc200placementbit (parray, NR, NC, 2, NC - 1, p, 1);
  ecc200placementbit (parray, NR, NC, 3, NC - 1, p, 0);
end;

procedure ecc200placementcornerB(parray: PInteger; NR: Integer; NC: Integer; p: Integer);
begin
  ecc200placementbit (parray, NR, NC, NR - 3, 0, p, 7);
  ecc200placementbit (parray, NR, NC, NR - 2, 0, p, 6);
  ecc200placementbit (parray, NR, NC, NR - 1, 0, p, 5);
  ecc200placementbit (parray, NR, NC, 0, NC - 4, p, 4);
  ecc200placementbit (parray, NR, NC, 0, NC - 3, p, 3);
  ecc200placementbit (parray, NR, NC, 0, NC - 2, p, 2);
  ecc200placementbit (parray, NR, NC, 0, NC - 1, p, 1);
  ecc200placementbit (parray, NR, NC, 1, NC - 1, p, 0);
end;

procedure ecc200placementcornerC(parray: PInteger; NR: Integer; NC: Integer; p: Integer);
begin
  ecc200placementbit (parray, NR, NC, NR - 3, 0, p, 7);
  ecc200placementbit (parray, NR, NC, NR - 2, 0, p, 6);
  ecc200placementbit (parray, NR, NC, NR - 1, 0, p, 5);
  ecc200placementbit (parray, NR, NC, 0, NC - 2, p, 4);
  ecc200placementbit (parray, NR, NC, 0, NC - 1, p, 3);
  ecc200placementbit (parray, NR, NC, 1, NC - 1, p, 2);
  ecc200placementbit (parray, NR, NC, 2, NC - 1, p, 1);
  ecc200placementbit (parray, NR, NC, 3, NC - 1, p, 0);
end;

procedure ecc200placementcornerD(parray: PInteger; NR: Integer; NC: Integer; p: Integer);
begin
  ecc200placementbit (parray, NR, NC, NR - 1, 0, p, 7);
  ecc200placementbit (parray, NR, NC, NR - 1, NC - 1, p, 6);
  ecc200placementbit (parray, NR, NC, 0, NC - 3, p, 5);
  ecc200placementbit (parray, NR, NC, 0, NC - 2, p, 4);
  ecc200placementbit (parray, NR, NC, 0, NC - 1, p, 3);
  ecc200placementbit (parray, NR, NC, 1, NC - 3, p, 2);
  ecc200placementbit (parray, NR, NC, 1, NC - 2, p, 1);
  ecc200placementbit (parray, NR, NC, 1, NC - 1, p, 0);
end;

procedure ecc200placement(parray: PInteger; NR: Integer; NC: Integer);
var
  p: Integer;
  c: Integer;
  r: Integer;
begin
  r := 0;
  while r < NR do
  begin
    c := 0;
    while c < NC do
    begin
      parray[r * NC + c] := 0;
      Inc (c);
    end;
    Inc (r);
  end;
  p := 1;
  r := 4;
  c := 0;
  repeat
    if IsTrue(IsTrue(r = NR) AND  IsTrue(BooleanNot (c))) then
    begin
      ecc200placementcornerA (parray, NR, NC, PostInc (p));
    end;
    if IsTrue(IsTrue(IsTrue(r = NR - 2) AND  IsTrue(BooleanNot (c))) AND  IsTrue(NC mod 4)) then
    begin
      ecc200placementcornerB (parray, NR, NC, PostInc (p));
    end;
    if IsTrue(IsTrue(IsTrue(r = NR - 2) AND  IsTrue(BooleanNot (c))) AND  IsTrue((NC mod 8) = 4)) then
    begin
      ecc200placementcornerC (parray, NR, NC, PostInc (p));
    end;
    if IsTrue(IsTrue(IsTrue(r = NR + 4) AND  IsTrue(c = 2)) AND  IsTrue(BooleanNot ((NC mod 8)))) then
    begin
      ecc200placementcornerD (parray, NR, NC, PostInc (p));
    end;
    repeat
      if IsTrue(IsTrue(IsTrue(r < NR) AND  IsTrue(c >= 0)) AND  IsTrue(BooleanNot (parray[r * NC + c]))) then
      begin
        ecc200placementblock (parray, NR, NC, r, c, PostInc (p));
      end;
      r := r - 2;
      c := c + 2;
    until not (IsTrue(r >= 0) AND  IsTrue(c < NC));
    Inc (r);
    c := c + 3;
    repeat
      if IsTrue(IsTrue(IsTrue(r >= 0) AND  IsTrue(c < NC)) AND  IsTrue(BooleanNot (parray[r * NC + c]))) then
      begin
        ecc200placementblock (parray, NR, NC, r, c, PostInc (p));
      end;
      r := r + 2;
      c := c - 2;
    until not (IsTrue(r < NR) AND  IsTrue(c >= 0));
    r := r + 3;
    Inc (c);
  until not (IsTrue(r < NR) OR  IsTrue(c < NC));
  if IsTrue(BooleanNot (parray[NR * NC - 1])) then
  begin
    parray[NR * NC - NC - 2] := 1;
    parray[NR * NC - 1] := 1;
  end;
end;

procedure ecc200(binary: PBYTE; bytes: Integer; datablock: Integer; rsblock: Integer; skew: Integer);
var
  b: Integer;
  blocks: Integer;
  p: Integer;
  n: Integer;
  ecc: array [0..256-1] of BYTE;
  buf: array [0..256-1] of BYTE;
begin
  {INITCODE} blocks := (bytes + 2) div datablock;
  rs_init_gf ($12D);
  rs_init_code (rsblock, 1);
  b := 0;
  while b < blocks do
  begin
    p := 0;
    n := b;
    while n < bytes do
    begin
      buf[PostInc (p)] := binary[n];
      n := n + blocks;
    end;
    rs_encode (p, buf, ecc);
    p := rsblock - 1;
    n := b;
    while n < rsblock * blocks do
    begin
      if IsTrue(skew) then
      begin
        if IsTrue(b < 8) then
        begin
          binary[bytes + n + 2] := ecc[PostDec (p)];
        end else begin
          binary[bytes + n - 8] := ecc[PostDec (p)];
        end;
      end else begin
        binary[bytes + n] := ecc[PostDec (p)];
      end;
      n := n + blocks;
    end;
    Inc (b);
  end;
  rs_free;
end;

function isx12(source: BYTE): Integer;
begin
  if IsTrue(source = 13) then
  begin
    exit (1);
  end;
  if IsTrue(source = 42) then
  begin
    exit (1);
  end;
  if IsTrue(source = 62) then
  begin
    exit (1);
  end;
  if IsTrue(source = 32) then
  begin
    exit (1);
  end;
  if IsTrue(IsTrue((source >= BYTE('0'))) AND  IsTrue((source <= BYTE('9')))) then
  begin
    exit (1);
  end;
  if IsTrue(IsTrue((source >= BYTE('A'))) AND  IsTrue((source <= BYTE('Z')))) then
  begin
    exit (1);
  end;
  exit (0);
end;

procedure dminsert(binary_string: PChar; posn: Integer; newbit: Char);
var
  lend: Integer;
  i: Integer;
begin
  lend := sysutils.strlen (binary_string);
  i := lend;
  while i > posn do
  begin
    binary_string[i] := binary_string[i - 1];
    Dec (i);
  end;
  binary_string[posn] := newbit;
end;

procedure insert_value(binary_stream: PBYTE; posn: Integer; streamlen: Integer; newbit: BYTE);
var
  i: Integer;
begin
  i := streamlen;
  while i > posn do
  begin
    binary_stream[i] := binary_stream[i - 1];
    Dec (i);
  end;
  binary_stream[posn] := newbit;
end;

function look_ahead_test(source: PBYTE; sourcelen: Integer; position: Integer; current_mode: Integer; gs1: Integer): Integer;
var
  best_count: Single;
  b256_count: Single;
  edf_count: Single;
  x12_count: Single;
  text_count: Single;
  c40_count: Single;
  ascii_count: Single;
  best_scheme: Integer;
  done: Integer;
  sp: Integer;
  reduced_char: Char;
begin
  if IsTrue(current_mode = DM_ASCII) then
  begin
    ascii_count := 0.0;
    c40_count := 1.0;
    text_count := 1.0;
    x12_count := 1.0;
    edf_count := 1.0;
    b256_count := 1.25;
  end else begin
    ascii_count := 1.0;
    c40_count := 2.0;
    text_count := 2.0;
    x12_count := 2.0;
    edf_count := 2.0;
    b256_count := 2.25;
  end;
  case current_mode of
    DM_C40: c40_count := 0.0;
    DM_TEXT: text_count := 0.0;
    DM_X12: x12_count := 0.0;
    DM_EDIFACT: edf_count := 0.0;
    DM_BASE256: b256_count := 0.0;
  end;
  sp := position;
  while IsTrue((sp < sourcelen)) AND  IsTrue((sp <= (position + 8))) do
  begin
    if IsTrue(source[sp] <= 127) then
    begin
      reduced_char := char(source[sp]);
    end else begin
      reduced_char := char(source[sp] - 127);
    end;
    if IsTrue(IsTrue((source[sp] >= BYTE('0'))) AND  IsTrue((source[sp] <= BYTE('9')))) then
    begin
      ascii_count := ascii_count + 0.5;
    end else begin
      ascii_count := ascii_count + 1.0;
    end;
    if IsTrue(source[sp] > 127) then
    begin
      ascii_count := ascii_count + 1.0;
    end;
    done := 0;
    if IsTrue(reduced_char = ' ') then
    begin
      c40_count := c40_count + ((2.0 / 3.0));
      done := 1;
    end;
    if IsTrue(IsTrue((reduced_char >= '0')) AND  IsTrue((reduced_char <= '9'))) then
    begin
      c40_count := c40_count + ((2.0 / 3.0));
      done := 1;
    end;
    if IsTrue(IsTrue((reduced_char >= 'A')) AND  IsTrue((reduced_char <= 'Z'))) then
    begin
      c40_count := c40_count + ((2.0 / 3.0));
      done := 1;
    end;
    if IsTrue(source[sp] > 127) then
    begin
      c40_count := c40_count + ((4.0 / 3.0));
    end;
    if IsTrue(done = 0) then
    begin
      c40_count := c40_count + ((4.0 / 3.0));
    end;
    done := 0;
    if IsTrue(reduced_char = ' ') then
    begin
      text_count := text_count + ((2.0 / 3.0));
      done := 1;
    end;
    if IsTrue(IsTrue((reduced_char >= '0')) AND  IsTrue((reduced_char <= '9'))) then
    begin
      text_count := text_count + ((2.0 / 3.0));
      done := 1;
    end;
    if IsTrue(IsTrue((reduced_char >= 'a')) AND  IsTrue((reduced_char <= 'z'))) then
    begin
      text_count := text_count + ((2.0 / 3.0));
      done := 1;
    end;
    if IsTrue(source[sp] > 127) then
    begin
      text_count := text_count + ((4.0 / 3.0));
    end;
    if IsTrue(done = 0) then
    begin
      text_count := text_count + ((4.0 / 3.0));
    end;
    if IsTrue(isx12 (source[sp])) then
    begin
      x12_count := x12_count + ((2.0 / 3.0));
    end else begin
      x12_count := x12_count + 4.0;
    end;
    done := 0;
    if IsTrue(IsTrue((source[sp] >= BYTE(' '))) AND  IsTrue((source[sp] <= BYTE('^')))) then
    begin
      edf_count := edf_count + ((3.0 / 4.0));
    end else begin
      edf_count := edf_count + 6.0;
    end;
    if IsTrue(IsTrue(gs1) AND  IsTrue((source[sp] = BYTE('[')))) then
    begin
      edf_count := edf_count + 6.0;
    end;
    if IsTrue(sp >= (sourcelen - 5)) then
    begin
      edf_count := edf_count + 6.0;
    end;
    if IsTrue(IsTrue(gs1) AND  IsTrue((source[sp] = BYTE('[')))) then
    begin
      b256_count := b256_count + 4.0;
    end else begin
      b256_count := b256_count + 1.0;
    end;
    Inc (sp);
  end;
  best_count := ascii_count;
  best_scheme := DM_ASCII;
  if IsTrue(b256_count <= best_count) then
  begin
    best_count := b256_count;
    best_scheme := DM_BASE256;
  end;
  if IsTrue(edf_count <= best_count) then
  begin
    best_count := edf_count;
    best_scheme := DM_EDIFACT;
  end;
  if IsTrue(text_count <= best_count) then
  begin
    best_count := text_count;
    best_scheme := DM_TEXT;
  end;
  if IsTrue(x12_count <= best_count) then
  begin
    best_count := x12_count;
    best_scheme := DM_X12;
  end;
  if IsTrue(c40_count <= best_count) then
  begin
    best_count := c40_count;
    best_scheme := DM_C40;
  end;
  exit (best_scheme);
end;

function dm200encode(symbol: PointerTo_zint_symbol; source: PBYTE; target: PBYTE; last_mode: PInteger; length: Integer): Integer;
var
  gs1: Integer;
  i: Integer;
  tp: Integer;
  sp: Integer;
  next_mode: Integer;
  current_mode: Integer;
  inputlen: Integer;
  c40_p: Integer;
  c40_buffer: array [0..6-1] of Integer;
  text_p: Integer;
  text_buffer: array [0..6-1] of Integer;
  x12_p: Integer;
  x12_buffer: array [0..6-1] of Integer;
  edifact_p: Integer;
  edifact_buffer: array [0..8-1] of Integer;
  debug: Integer = 0;
  binary: array of Char;
  value: Integer;
  shift_set: Integer;
  iv: Integer;
  binary_count: Integer;
  temp: Integer;
  prn: Integer;
begin
  {INITCODE} inputlen := length;
  {INITCODE} debug := 0;
  SetLength(binary,2 * inputlen-1);
  sp := 0;
  tp := 0;
  memset (@c40_buffer[0], 0, 6);
  c40_p := 0;
  memset (@text_buffer[0], 0, 6);
  text_p := 0;
  memset (@x12_buffer[0], 0, 6);
  x12_p := 0;
  memset (@edifact_buffer[0], 0, 8);
  edifact_p := 0;
  strcpy (binary, '');
  current_mode := DM_ASCII;
  next_mode := DM_ASCII;
  if IsTrue(symbol^.input_mode = GS1_MODE) then
  begin
    gs1 := 1;
  end else begin
    gs1 := 0;
  end;
  if IsTrue(gs1) then
  begin
    target[tp] := 232;
    Inc (tp);
    concat (binary, ' ');
    if IsTrue(debug) then
    begin
      write( format ('FN1 ',[]) );
    end;
  end;
  if IsTrue(symbol^.output_options and READER_INIT) then
  begin
    if IsTrue(gs1) then
    begin
      strcpy (symbol^.errtxt, 'Cannot encode in GS1 mode and Reader Initialisation at the same time');
      exit (ERROR_INVALID_OPTION);
    end else begin
      target[tp] := 234;
      Inc (tp);
      concat (binary, ' ');
      if IsTrue(debug) then
      begin
        write( format ('RP ',[]) );
      end;
    end;
  end;
  while sp < inputlen do
  begin
    current_mode := next_mode;
    if IsTrue(current_mode = DM_ASCII) then
    begin
      next_mode := DM_ASCII;
      if IsTrue(IsTrue(istwodigits (source, sp)) AND  IsTrue(((sp + 1) <> inputlen))) then
      begin
        target[tp] := (10 * ctoi (source[sp])) + ctoi (source[sp + 1]) + 130;
        if IsTrue(debug) then
        begin
          write( format ('N%d ',[target[tp] - 130]) );
        end;
        Inc (tp);
        concat (binary, ' ');
        sp := sp + 2;
      end else begin
        next_mode := look_ahead_test (source, inputlen, sp, current_mode, gs1);
        if IsTrue(next_mode <> DM_ASCII) then
        begin
          case next_mode of
            DM_C40:
              begin
                target[tp] := 230;
                Inc (tp);
                concat (binary, ' ');
                if IsTrue(debug) then
                begin
                  write( format ('C40 ',[]) );
                end;
              end;
            DM_TEXT:
              begin
                target[tp] := 239;
                Inc (tp);
                concat (binary, ' ');
                if IsTrue(debug) then
                begin
                  write( format ('TEX ',[]) );
                end;
              end;
            DM_X12:
              begin
                target[tp] := 238;
                Inc (tp);
                concat (binary, ' ');
                if IsTrue(debug) then
                begin
                  write( format ('X12 ',[]) );
                end;
              end;
            DM_EDIFACT:
              begin
                target[tp] := 240;
                Inc (tp);
                concat (binary, ' ');
                if IsTrue(debug) then
                begin
                  write( format ('EDI ',[]) );
                end;
              end;
            DM_BASE256:
              begin
                target[tp] := 231;
                Inc (tp);
                concat (binary, ' ');
                if IsTrue(debug) then
                begin
                  write( format ('BAS ',[]) );
                end;
              end;
          end;
        end else begin
          if IsTrue(source[sp] > 127) then
          begin
            target[tp] := 235;
            if IsTrue(debug) then
            begin
              write( format ('FN4 ',[]) );
            end;
            Inc (tp);
            target[tp] := (source[sp] - 128) + 1;
            if IsTrue(debug) then
            begin
              write( format ('A%02X ',[target[tp] - 1]) );
            end;
            Inc (tp);
            concat (binary, '  ');
          end else begin
            if IsTrue(IsTrue(gs1) AND  IsTrue((source[sp] = BYTE('[')))) then
            begin
              target[tp] := 232;
              if IsTrue(debug) then
              begin
                write( format ('FN1 ',[]) );
              end;
            end else begin
              target[tp] := source[sp] + 1;
              if IsTrue(debug) then
              begin
                write( format ('A%02X ',[target[tp] - 1]) );
              end;
            end;
            Inc (tp);
            concat (binary, ' ');
          end;
          Inc (sp);
        end;
      end;
    end;
    if IsTrue(current_mode = DM_C40) then
    begin
      next_mode := DM_C40;
      if IsTrue(c40_p = 0) then
      begin
        next_mode := look_ahead_test (source, inputlen, sp, current_mode, gs1);
      end;
      if IsTrue(next_mode <> DM_C40) then
      begin
        target[tp] := 254;
        Inc (tp);
        concat (binary, ' ');
        next_mode := DM_ASCII;
        if IsTrue(debug) then
        begin
          write( format ('ASC ',[]) );
        end;
      end else begin
        if IsTrue(source[sp] > 127) then
        begin
          c40_buffer[c40_p] := 1;
          Inc (c40_p);
          c40_buffer[c40_p] := 30;
          Inc (c40_p);
          shift_set := c40_shift[source[sp] - 128];
          value := c40_value[source[sp] - 128];
        end else begin
          shift_set := c40_shift[source[sp]];
          value := c40_value[source[sp]];
        end;
        if IsTrue(IsTrue(gs1) AND  IsTrue((source[sp] = BYTE('[')))) then
        begin
          shift_set := 2;
          value := 27;
        end;
        if IsTrue(shift_set <> 0) then
        begin
          c40_buffer[c40_p] := shift_set - 1;
          Inc (c40_p);
        end;
        c40_buffer[c40_p] := value;
        Inc (c40_p);
        if IsTrue(c40_p >= 3) then
        begin
          iv := (1600 * c40_buffer[0]) + (40 * c40_buffer[1]) + (c40_buffer[2]) + 1;
          target[tp] := iv div 256;
          Inc (tp);
          target[tp] := iv mod 256;
          Inc (tp);
          concat (binary, '  ');
          if IsTrue(debug) then
          begin
            write( format ('[%d %d %d] ',[c40_buffer[0], c40_buffer[1], c40_buffer[2]]) );
          end;
          c40_buffer[0] := c40_buffer[3];
          c40_buffer[1] := c40_buffer[4];
          c40_buffer[2] := c40_buffer[5];
          c40_buffer[3] := 0;
          c40_buffer[4] := 0;
          c40_buffer[5] := 0;
          c40_p := c40_p - 3;
        end;
        Inc (sp);
      end;
    end;
    if IsTrue(current_mode = DM_TEXT) then
    begin
      next_mode := DM_TEXT;
      if IsTrue(text_p = 0) then
      begin
        next_mode := look_ahead_test (source, inputlen, sp, current_mode, gs1);
      end;
      if IsTrue(next_mode <> DM_TEXT) then
      begin
        target[tp] := 254;
        Inc (tp);
        concat (binary, ' ');
        next_mode := DM_ASCII;
        if IsTrue(debug) then
        begin
          write( format ('ASC ',[]) );
        end;
      end else begin
        if IsTrue(source[sp] > 127) then
        begin
          text_buffer[text_p] := 1;
          Inc (text_p);
          text_buffer[text_p] := 30;
          Inc (text_p);
          shift_set := text_shift[source[sp] - 128];
          value := text_value[source[sp] - 128];
        end else begin
          shift_set := text_shift[source[sp]];
          value := text_value[source[sp]];
        end;
        if IsTrue(IsTrue(gs1) AND  IsTrue((source[sp] = BYTE('[')))) then
        begin
          shift_set := 2;
          value := 27;
        end;
        if IsTrue(shift_set <> 0) then
        begin
          text_buffer[text_p] := shift_set - 1;
          Inc (text_p);
        end;
        text_buffer[text_p] := value;
        Inc (text_p);
        if IsTrue(text_p >= 3) then
        begin
          iv := (1600 * text_buffer[0]) + (40 * text_buffer[1]) + (text_buffer[2]) + 1;
          target[tp] := iv div 256;
          Inc (tp);
          target[tp] := iv mod 256;
          Inc (tp);
          concat (binary, '  ');
          if IsTrue(debug) then
          begin
            write( format ('[%d %d %d] ',[text_buffer[0], text_buffer[1], text_buffer[2]]) );
          end;
          text_buffer[0] := text_buffer[3];
          text_buffer[1] := text_buffer[4];
          text_buffer[2] := text_buffer[5];
          text_buffer[3] := 0;
          text_buffer[4] := 0;
          text_buffer[5] := 0;
          text_p := text_p - 3;
        end;
        Inc (sp);
      end;
    end;
    if IsTrue(current_mode = DM_X12) then
    begin
      {INITCODE} value := 0;
      next_mode := DM_X12;
      if IsTrue(text_p = 0) then
      begin
        next_mode := look_ahead_test (source, inputlen, sp, current_mode, gs1);
      end;
      if IsTrue(next_mode <> DM_X12) then
      begin
        target[tp] := 254;
        Inc (tp);
        concat (binary, ' ');
        next_mode := DM_ASCII;
        if IsTrue(debug) then
        begin
          write( format ('ASC ',[]) );
        end;
      end else begin
        if IsTrue(source[sp] = 13) then
        begin
          value := 0;
        end;
        if IsTrue(source[sp] = BYTE('*')) then
        begin
          value := 1;
        end;
        if IsTrue(source[sp] = BYTE('>')) then
        begin
          value := 2;
        end;
        if IsTrue(source[sp] = BYTE(' ')) then
        begin
          value := 3;
        end;
        if IsTrue(IsTrue((source[sp] >= BYTE('0'))) AND  IsTrue((source[sp] <= BYTE('9')))) then
        begin
          value := (source[sp] - BYTE('0')) + 4;
        end;
        if IsTrue(IsTrue((source[sp] >= BYTE('A'))) AND  IsTrue((source[sp] <= BYTE('Z')))) then
        begin
          value := (source[sp] - BYTE('A')) + 14;
        end;
        x12_buffer[x12_p] := value;
        Inc (x12_p);
        if IsTrue(x12_p >= 3) then
        begin
          iv := (1600 * x12_buffer[0]) + (40 * x12_buffer[1]) + (x12_buffer[2]) + 1;
          target[tp] := iv div 256;
          Inc (tp);
          target[tp] := iv mod 256;
          Inc (tp);
          concat (binary, '  ');
          if IsTrue(debug) then
          begin
            write( format ('[%d %d %d] ',[x12_buffer[0], x12_buffer[1], x12_buffer[2]]) );
          end;
          x12_buffer[0] := x12_buffer[3];
          x12_buffer[1] := x12_buffer[4];
          x12_buffer[2] := x12_buffer[5];
          x12_buffer[3] := 0;
          x12_buffer[4] := 0;
          x12_buffer[5] := 0;
          x12_p := x12_p - 3;
        end;
        Inc (sp);
      end;
    end;
    if IsTrue(current_mode = DM_EDIFACT) then
    begin
      {INITCODE} value := 0;
      next_mode := DM_EDIFACT;
      if IsTrue(edifact_p = 3) then
      begin
        next_mode := look_ahead_test (source, inputlen, sp, current_mode, gs1);
      end;
      if IsTrue(next_mode <> DM_EDIFACT) then
      begin
        edifact_buffer[edifact_p] := 31;
        Inc (edifact_p);
        next_mode := DM_ASCII;
      end else begin
        if IsTrue(IsTrue((source[sp] >= BYTE('@'))) AND  IsTrue((source[sp] <= BYTE('^')))) then
        begin
          value := source[sp] - BYTE('@');
        end;
        if IsTrue(IsTrue((source[sp] >= BYTE(' '))) AND  IsTrue((source[sp] <= BYTE('?')))) then
        begin
          value := source[sp];
        end;
        edifact_buffer[edifact_p] := value;
        Inc (edifact_p);
        Inc (sp);
      end;
      if IsTrue(edifact_p >= 4) then
      begin
        target[tp] := (edifact_buffer[0] shl 2) + ((edifact_buffer[1] and $30) shr 4);
        Inc (tp);
        target[tp] := ((edifact_buffer[1] and $0F) shl 4) + ((edifact_buffer[2] and $3C) shr 2);
        Inc (tp);
        target[tp] := ((edifact_buffer[2] and $03) shl 6) + edifact_buffer[3];
        Inc (tp);
        concat (binary, '   ');
        if IsTrue(debug) then
        begin
          write( format ('[%d %d %d %d] ',[edifact_buffer[0], edifact_buffer[1], edifact_buffer[2], edifact_buffer[3]]) );
        end;
        edifact_buffer[0] := edifact_buffer[4];
        edifact_buffer[1] := edifact_buffer[5];
        edifact_buffer[2] := edifact_buffer[6];
        edifact_buffer[3] := edifact_buffer[7];
        edifact_buffer[4] := 0;
        edifact_buffer[5] := 0;
        edifact_buffer[6] := 0;
        edifact_buffer[7] := 0;
        edifact_p := edifact_p - 4;
      end;
    end;
    if IsTrue(current_mode = DM_BASE256) then
    begin
      next_mode := look_ahead_test (source, inputlen, sp, current_mode, gs1);
      if IsTrue(next_mode = DM_BASE256) then
      begin
        target[tp] := source[sp];
        if IsTrue(debug) then
        begin
          write( format ('B%02X ',[target[tp]]) );
        end;
        Inc (tp);
        Inc (sp);
        concat (binary, 'b');
      end else begin
        next_mode := DM_ASCII;
        if IsTrue(debug) then
        begin
          write( format ('ASC ',[]) );
        end;
      end;
    end;
    if IsTrue(tp > 1558) then
    begin
      exit (0);
    end;
  end;
  if IsTrue(c40_p = 2) then
  begin
    target[tp] := 254;
    Inc (tp);
    target[tp] := source[inputlen - 2] + 1;
    Inc (tp);
    target[tp] := source[inputlen - 1] + 1;
    Inc (tp);
    concat (binary, '   ');
    if IsTrue(debug) then
    begin
      write( format ('ASC A%02X A%02X ',[target[tp - 2] - 1, target[tp - 1] - 1]) );
    end;
    current_mode := DM_ASCII;
  end;
  if IsTrue(c40_p = 1) then
  begin
    target[tp] := 254;
    Inc (tp);
    target[tp] := source[inputlen - 1] + 1;
    Inc (tp);
    concat (binary, '  ');
    if IsTrue(debug) then
    begin
      write( format ('ASC A%02X ',[target[tp - 1] - 1]) );
    end;
    current_mode := DM_ASCII;
  end;
  if IsTrue(text_p = 2) then
  begin
    target[tp] := 254;
    Inc (tp);
    target[tp] := source[inputlen - 2] + 1;
    Inc (tp);
    target[tp] := source[inputlen - 1] + 1;
    Inc (tp);
    concat (binary, '   ');
    if IsTrue(debug) then
    begin
      write( format ('ASC A%02X A%02X ',[target[tp - 2] - 1, target[tp - 1] - 1]) );
    end;
    current_mode := DM_ASCII;
  end;
  if IsTrue(text_p = 1) then
  begin
    target[tp] := 254;
    Inc (tp);
    target[tp] := source[inputlen - 1] + 1;
    Inc (tp);
    concat (binary, '  ');
    if IsTrue(debug) then
    begin
      write( format ('ASC A%02X ',[target[tp - 1] - 1]) );
    end;
    current_mode := DM_ASCII;
  end;
  if IsTrue(x12_p = 2) then
  begin
    target[tp] := 254;
    Inc (tp);
    target[tp] := source[inputlen - 2] + 1;
    Inc (tp);
    target[tp] := source[inputlen - 1] + 1;
    Inc (tp);
    concat (binary, '   ');
    if IsTrue(debug) then
    begin
      write( format ('ASC A%02X A%02X ',[target[tp - 2] - 1, target[tp - 1] - 1]) );
    end;
    current_mode := DM_ASCII;
  end;
  if IsTrue(x12_p = 1) then
  begin
    target[tp] := 254;
    Inc (tp);
    target[tp] := source[inputlen - 1] + 1;
    Inc (tp);
    concat (binary, '  ');
    if IsTrue(debug) then
    begin
      write( format ('ASC A%02X ',[target[tp - 1] - 1]) );
    end;
    current_mode := DM_ASCII;
  end;
  i := 0;
  while i < tp do
  begin
    if IsTrue(binary[i] = 'b') then
    begin
      if IsTrue(IsTrue((i = 0)) OR  IsTrue((IsTrue((i <> 0)) AND  IsTrue((binary[i - 1] <> 'b'))))) then
      begin
        binary_count := 0;
        while binary[binary_count + i] = 'b' do
        begin
          Inc (binary_count);
        end;
        if IsTrue(binary_count <= 249) then
        begin
          dminsert (@binary[0], i, 'b');
          insert_value (target, i, tp, binary_count);
          Inc (tp);
        end else begin
          dminsert (@binary[0], i, 'b');
          dminsert (@binary[0], i + 1, 'b');
          insert_value (target, i, tp, (binary_count div 250) + 249);
          Inc (tp);
          insert_value (target, i + 1, tp, binary_count mod 250);
          Inc (tp);
        end;
      end;
    end;
    Inc (i);
  end;
  i := 0;
  while i < tp do
  begin
    if IsTrue(binary[i] = 'b') then
    begin
      prn := ((149 * (i + 1)) mod 255) + 1;
      temp := target[i] + prn;
      if IsTrue(temp <= 255) then
      begin
        target[i] := temp;
      end else begin
        target[i] := temp - 256;
      end;
    end;
    Inc (i);
  end;
  if IsTrue(debug) then
  begin
    write( format ('\n\n',[]) );
    i := 0;
    while i < tp do
    begin
      write( format ('%02X ',[target[i]]) );
      Inc (i);
    end;
    write( format ('\n',[]) );
  end;
  (last_mode)^ := current_mode;
  exit (tp);
end;

procedure add_tail(target: PBYTE; tp: Integer; tail_length: Integer; last_mode: Integer);
var
  temp: Integer;
  prn: Integer;
  i: Integer;
begin
  case last_mode of
    DM_C40,
    DM_TEXT,
    DM_X12:
      begin
        target[tp] := 254;
        Inc (tp);
        Dec (tail_length);
      end;
    {>WARNING< code without ending break}
  end;
  i := tail_length;
  while i > 0 do
  begin
    if IsTrue(i = tail_length) then
    begin
      target[tp] := 129;
      Inc (tp);
    end else begin
      prn := ((149 * (tp + 1)) mod 253) + 1;
      temp := 129 + prn;
      if IsTrue(temp <= 254) then
      begin
        target[tp] := temp;
        Inc (tp);
      end else begin
        target[tp] := temp - 254;
        Inc (tp);
      end;
    end;
    Dec (i);
  end;
end;

function data_matrix_200(symbol: PointerTo_zint_symbol; source: PBYTE; length: Integer): Integer;
var
  skew: Integer = 0;
  i: Integer = 0;
  //inputlen: Integer = 0;
  binary: array [0..2200-1] of BYTE;
  binlen: Integer;
  calcsize: Integer;
  optionsize: Integer;
  symbolsize: Integer;
  error_number: Integer = 0;
  taillength: Integer = 0;
  rsblock: Integer;
  bytes: Integer;
  datablock: Integer;
  FW: Integer;
  FH: Integer;
  W: Integer;
  H: Integer;
  last_mode: Integer;
  grid: PBYTE;
  places: PInteger;
  NR: Integer;
  NC: Integer;
  y: Integer;
  x: Integer;
  v: Integer;
begin
  {INITCODE} skew := 0;
  {INITCODE} i := 0;
  //{INITCODE} inputlen := 0;
  {INITCODE} error_number := 0;
  {INITCODE} taillength := 0;
  {INITCODE} grid := nil;
  //inputlen := length;
  binlen := dm200encode (symbol, source, binary, @last_mode, length);
  if IsTrue(binlen = 0) then
  begin
    strcpy (symbol^.errtxt, 'Data too long to fit in symbol');
    exit (ERROR_TOO_LONG);
  end;
  if IsTrue(IsTrue((symbol^.option_2 >= 1)) AND  IsTrue((symbol^.option_2 <= 30))) then
  begin
    optionsize := intsymbol[symbol^.option_2 - 1];
  end else begin
    optionsize := -1;
  end;
  calcsize := 29;
  i := 29;
  while i > -1 do
  begin
    if IsTrue(matrixbytes[i] >= binlen) then
    begin
      calcsize := i;
    end;
    Dec (i);
  end;
  if IsTrue(symbol^.option_3 = DM_SQUARE) then
  begin
    case calcsize of
      2,
      4,
      6,
      9,
      11,
      14: Inc (calcsize);
      {>WARNING< code without ending break}
    end;
  end;
  symbolsize := optionsize;
  if IsTrue(calcsize > optionsize) then
  begin
    symbolsize := calcsize;
    if IsTrue(optionsize <> -1) then
    begin
      error_number := WARN_INVALID_OPTION;
      strcpy (symbol^.errtxt, 'Data does not fit in selected symbol size');
    end;
  end;
  H := matrixH[symbolsize];
  W := matrixW[symbolsize];
  FH := matrixFH[symbolsize];
  FW := matrixFW[symbolsize];
  bytes := matrixbytes[symbolsize];
  datablock := matrixdatablock[symbolsize];
  rsblock := matrixrsblock[symbolsize];
  taillength := bytes - binlen;
  if IsTrue(taillength <> 0) then
  begin
    add_tail (binary, binlen, taillength, last_mode);
  end;
  if IsTrue(symbolsize = 29) then
  begin
    skew := 1;
  end;
  ecc200 (binary, bytes, datablock, rsblock, skew);
  NC := W - 2 * (W div FW);
  NR := H - 2 * (H div FH);
  places := PInteger (GetMem (NC * NR * SizeOf (Integer)));
  ecc200placement (places, NR, NC);
  grid := PBYTE (GetMem (W * H));
  memset (grid, 0, W * H);
  y := 0;
  while y < H do
  begin
    x := 0;
    while x < W do
    begin
      grid[y * W + x] := 1;
      Inc (x);
    end;
    x := 0;
    while x < W do
    begin
      grid[(y + FH - 1) * W + x] := 1;
      x := x + 2;
    end;
    y := y + FH;
  end;
  x := 0;
  while x < W do
  begin
    y := 0;
    while y < H do
    begin
      grid[y * W + x] := 1;
      Inc (y);
    end;
    y := 0;
    while y < H do
    begin
      grid[y * W + x + FW - 1] := 1;
      y := y + 2;
    end;
    x := x + FW;
  end;
  y := 0;
  while y < NR do
  begin
    x := 0;
    while x < NC do
    begin
      {INITCODE} v := places[(NR - y - 1) * NC + x];
      if IsTrue(IsTrue(v = 1) OR  IsTrue((IsTrue(v > 7) AND  IsTrue((binary[(v shr 3) - 1] and (1 shl (v and 7))))))) then
      begin
        grid[(1 + y + 2 * (y div (FH - 2))) * W + 1 + x + 2 * (x div (FW - 2))] := 1;
      end;
      Inc (x);
    end;
    Inc (y);
  end;
  y := H - 1;
  while y >= 0 do
  begin
    x := 0;
    while x < W do
    begin
      if IsTrue(grid[W * y + x]) then
      begin
        set_module (symbol, (H - y) - 1, x);
      end;
      Inc (x);
    end;
    symbol^.row_height[(H - y) - 1] := 1;
    Dec (y);
  end;
  FreeMem (grid);
  FreeMem (places);
  symbol^.rows := H;
  symbol^.width := W;
  exit (error_number);
end;

function dmatrix(symbol: PointerTo_zint_symbol; source: PBYTE; length: Integer): Integer;
var
  error_number: Integer;
begin
  if IsTrue(symbol^.option_1 <= 1) then
  begin
    error_number := data_matrix_200 (symbol, source, length);
  end else begin
    strcpy (symbol^.errtxt, 'Older Data Matrix standards are no longer supported');
    error_number := ERROR_INVALID_OPTION;
  end;
  exit (error_number);
end;

end.

