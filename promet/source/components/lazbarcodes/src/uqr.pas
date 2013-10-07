unit uqr;

{$mode objfpc}{$H+}

interface

uses sysutils,ureedsolomon,uhelper,zint,usjis;

function  qr_code(symbol: PointerTo_zint_symbol; source: PBYTE; length: Integer): Integer;
function  microqr(symbol: PointerTo_zint_symbol; source: PBYTE; length: Integer): Integer;

implementation

const LEVEL_L	= 1;
const LEVEL_M =	2;
const LEVEL_Q	= 3;
const LEVEL_H	= 4;

const
qr_total_codewords: array [0..39] of integer = (
  	26, 44, 70, 100, 134, 172, 196, 242, 292, 346, 404, 466, 532, 581, 655, 733, 815,
  	901, 991, 1085, 1156, 1258, 1364, 1474, 1588, 1706, 1828, 1921, 2051,
  	2185, 2323, 2465, 2611, 2761, 2876, 3034, 3196, 3362, 3532, 3706
  );

qr_data_codewords_L: array [0..39] of integer = (
	19, 34, 55, 80, 108, 136, 156, 194, 232, 274, 324, 370, 428, 461, 523, 589, 647,
	721, 795, 861, 932, 1006, 1094, 1174, 1276, 1370, 1468, 1531, 1631,
	1735, 1843, 1955, 2071, 2191, 2306, 2434, 2566, 2702, 2812, 2956
);

qr_data_codewords_M: array [0..39] of integer = (
	16, 28, 44, 64, 86, 108, 124, 154, 182, 216, 254, 290, 334, 365, 415, 453, 507,
	563, 627, 669, 714, 782, 860, 914, 1000, 1062, 1128, 1193, 1267,
	1373, 1455, 1541, 1631, 1725, 1812, 1914, 1992, 2102, 2216, 2334
);

qr_data_codewords_Q: array [0..39] of integer = (
	13, 22, 34, 48, 62, 76, 88, 110, 132, 154, 180, 206, 244, 261, 295, 325, 367,
	397, 445, 485, 512, 568, 614, 664, 718, 754, 808, 871, 911,
	985, 1033, 1115, 1171, 1231, 1286, 1354, 1426, 1502, 1582, 1666
);

qr_data_codewords_H: array [0..39] of integer = (
	9, 16, 26, 36, 46, 60, 66, 86, 100, 122, 140, 158, 180, 197, 223, 253, 283,
	313, 341, 385, 406, 442, 464, 514, 538, 596, 628, 661, 701,
	745, 793, 845, 901, 961, 986, 1054, 1096, 1142, 1222, 1276
);

qr_blocks_L: array [0..39] of integer = (
	1, 1, 1, 1, 1, 2, 2, 2, 2, 4, 4, 4, 4, 4, 6, 6, 6, 6, 7, 8, 8, 9, 9, 10, 12, 12,
	12, 13, 14, 15, 16, 17, 18, 19, 19, 20, 21, 22, 24, 25
);

qr_blocks_M: array [0..39] of integer = (
	1, 1, 1, 2, 2, 4, 4, 4, 5, 5, 5, 8, 9, 9, 10, 10, 11, 13, 14, 16, 17, 17, 18, 20,
	21, 23, 25, 26, 28, 29, 31, 33, 35, 37, 38, 40, 43, 45, 47, 49
);

qr_blocks_Q: array [0..39] of integer = (
	1, 1, 2, 2, 4, 4, 6, 6, 8, 8, 8, 10, 12, 16, 12, 17, 16, 18, 21, 20, 23, 23, 25,
	27, 29, 34, 34, 35, 38, 40, 43, 45, 48, 51, 53, 56, 59, 62, 65, 68
);

qr_blocks_H: array [0..39] of integer = (
	1, 1, 2, 4, 4, 4, 5, 6, 8, 8, 11, 11, 16, 16, 18, 16, 19, 21, 25, 25, 25, 34, 30,
	32, 35, 37, 40, 42, 45, 48, 51, 54, 57, 60, 63, 66, 70, 74, 77, 81
);

qr_sizes: array [0..39] of integer = (
	21, 25, 29, 33, 37, 41, 45, 49, 53, 57, 61, 65, 69, 73, 77, 81, 85, 89, 93, 97,
	101, 105, 109, 113, 117, 121, 125, 129, 133, 137, 141, 145, 149, 153, 157, 161, 165, 169, 173, 177
);


qr_annex_c: array [0..31] of cardinal = (
	$5412, $5125, $5e7c, $5b4b, $45f9, $40ce, $4f97, $4aa0, $77c4, $72f3, $7daa, $789d,
	$662f, $6318, $6c41, $6976, $1689, $13be, $1ce7, $19d0, $0762, $0255, $0d0c, $083b,
	$355f, $3068, $3f31, $3a06, $24b4, $2183, $2eda, $2bed
);

qr_annex_d: array [0..33] of integer = (
	$07c94, $085bc, $09a99, $0a4d3, $0bbf6, $0c762, $0d847, $0e60d, $0f928, $10b78,
	$1145d, $12a17, $13532, $149a6, $15683, $168c9, $177ec, $18ec4, $191e1, $1afab,
	$1b08e, $1cc1a, $1d33f, $1ed75, $1f250, $209d5, $216f0, $228ba, $2379f, $24b0b,
	$2542e, $26a64, $27541, $28c69
);

qr_annex_c1: array [0..31] of integer = (
	$4445, $4172, $4e2b, $4b1c, $55ae, $5099, $5fc0, $5af7, $6793, $62a4, $6dfd, $68ca, $7678, $734f,
	$7c16, $7921, $06de, $03e9, $0cb0, $0987, $1735, $1202, $1d5b, $186c, $2508, $203f, $2f66, $2a51, $34e3,
	$31d4, $3e8d, $3bba
);

qr_align_loopsize: array [0..39] of integer = (
  	0, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4, 4, 4, 5, 5, 5, 5, 5, 5, 5, 6, 6, 6, 6, 6, 6, 6, 7, 7, 7, 7, 7, 7
  );
qr_table_e1: array [0..272] of integer = (
	6, 18, 0, 0, 0, 0, 0,
	6, 22, 0, 0, 0, 0, 0,
	6, 26, 0, 0, 0, 0, 0,
	6, 30, 0, 0, 0, 0, 0,
	6, 34, 0, 0, 0, 0, 0,
	6, 22, 38, 0, 0, 0, 0,
	6, 24, 42, 0, 0, 0, 0,
	6, 26, 46, 0, 0, 0, 0,
	6, 28, 50, 0, 0, 0, 0,
	6, 30, 54, 0, 0, 0, 0,
	6, 32, 58, 0, 0, 0, 0,
	6, 34, 62, 0, 0, 0, 0,
	6, 26, 46, 66, 0, 0, 0,
	6, 26, 48, 70, 0, 0, 0,
	6, 26, 50, 74, 0, 0, 0,
	6, 30, 54, 78, 0, 0, 0,
	6, 30, 56, 82, 0, 0, 0,
	6, 30, 58, 86, 0, 0, 0,
	6, 34, 62, 90, 0, 0, 0,
	6, 28, 50, 72, 94, 0, 0,
	6, 26, 50, 74, 98, 0, 0,
	6, 30, 54, 78, 102, 0, 0,
	6, 28, 54, 80, 106, 0, 0,
	6, 32, 58, 84, 110, 0, 0,
	6, 30, 58, 86, 114, 0, 0,
	6, 34, 62, 90, 118, 0, 0,
	6, 26, 50, 74, 98, 122, 0,
	6, 30, 54, 78, 102, 126, 0,
	6, 26, 52, 78, 104, 130, 0,
	6, 30, 56, 82, 108, 134, 0,
	6, 34, 60, 86, 112, 138, 0,
	6, 30, 58, 86, 114, 142, 0,
	6, 34, 62, 90, 118, 146, 0,
	6, 30, 54, 78, 102, 126, 150,
	6, 24, 50, 76, 102, 128, 154,
	6, 28, 54, 80, 106, 132, 158,
	6, 32, 58, 84, 110, 136, 162,
	6, 26, 54, 82, 110, 138, 166,
	6, 30, 58, 86, 114, 142, 170
);

micro_qr_sizes: array [0..3] of integer = (
	11, 13, 15, 17
);

(*
bullseye_compressed: array [0..1115] of Cardinal = (
	0,0,0,0,0,255,248,0,0,0,0,0,
	0,0,0,0,31,255,255,192,0,0,0,0,
	0,0,0,1,255,255,255,252,0,0,0,0,
	0,0,0,7,255,255,255,255,0,0,0,0,
	0,0,0,31,255,255,255,255,192,0,0,0,
	0,0,0,127,255,255,255,255,240,0,0,0,
	0,0,1,255,255,255,255,255,252,0,0,0,
	0,0,7,255,255,255,255,255,255,0,0,0,
	0,0,15,255,255,0,7,255,255,128,0,0,
	0,0,63,255,240,0,0,127,255,224,0,0,
	0,0,127,255,128,0,0,15,255,240,0,0,
	0,0,255,252,0,0,0,1,255,248,0,0,
	0,1,255,240,0,0,0,0,127,252,0,0,
	0,3,255,224,0,0,0,0,63,254,0,0,
	0,7,255,128,0,0,0,0,15,255,0,0,
	0,15,255,0,0,0,0,0,7,255,128,0,
	0,31,252,0,0,127,240,0,1,255,192,0,
	0,63,248,0,7,255,255,0,0,255,224,0,
	0,127,240,0,63,255,255,224,0,127,240,0,
	0,127,224,0,255,255,255,248,0,63,240,0,
	0,255,192,1,255,255,255,252,0,31,248,0,
	1,255,128,7,255,255,255,255,0,15,252,0,
	1,255,0,15,255,255,255,255,128,7,252,0,
	3,255,0,63,255,255,255,255,224,7,254,0,
	3,254,0,127,255,192,31,255,240,3,254,0,
	7,252,0,255,252,0,1,255,248,1,255,0,
	7,252,1,255,240,0,0,127,252,1,255,0,
	15,248,1,255,192,0,0,31,252,0,255,128,
	15,240,3,255,128,0,0,15,254,0,127,128,
	31,240,7,255,0,0,0,7,255,0,127,192,
	31,224,7,254,0,0,0,3,255,0,63,192,
	63,224,15,252,0,0,0,1,255,128,63,224,
	63,224,31,248,0,63,192,0,255,192,63,224,
	63,192,31,240,0,255,240,0,127,192,31,224,
	63,192,63,224,3,255,252,0,63,224,31,224,
	127,192,63,224,7,255,254,0,63,224,31,240,
	127,128,63,192,15,255,255,0,31,224,15,240,
	127,128,127,192,31,255,255,128,31,240,15,240,
	127,128,127,128,63,255,255,192,15,240,15,240,
	127,128,127,128,63,255,255,192,15,240,15,240,
	255,0,127,128,127,240,255,224,15,240,7,240,
	255,0,255,128,127,192,63,224,15,248,7,240,
	255,0,255,0,255,128,31,240,7,248,7,240,
	255,0,255,0,255,128,31,240,7,248,7,240,
	255,0,255,0,255,0,15,240,7,248,7,240,
	255,0,255,0,255,0,15,240,7,248,7,240,
	255,0,255,0,255,0,15,240,7,248,7,240,
	255,0,255,0,255,0,15,240,7,248,7,240,
	255,0,255,0,255,128,31,240,7,248,7,240,
	255,0,255,0,255,128,31,240,7,248,7,240,
	255,0,255,0,127,192,63,224,7,248,7,240,
	255,0,255,128,127,240,255,224,15,248,7,240,
	255,0,127,128,63,255,255,192,15,240,7,240,
	127,128,127,128,63,255,255,192,15,240,15,240,
	127,128,127,128,31,255,255,128,15,240,15,240,
	127,128,127,192,15,255,255,0,31,240,15,240,
	127,128,63,192,7,255,254,0,31,224,15,240,
	127,192,63,224,3,255,252,0,63,224,31,240,
	63,192,63,224,0,255,240,0,63,224,31,224,
	63,192,31,240,0,63,192,0,127,192,31,224,
	63,224,31,248,0,0,0,0,255,192,63,224,
	63,224,15,252,0,0,0,1,255,128,63,224,
	31,224,7,254,0,0,0,3,255,0,63,192,
	31,240,7,255,0,0,0,7,255,0,127,192,
	15,240,3,255,128,0,0,15,254,0,127,128,
	15,248,1,255,192,0,0,31,252,0,255,128,
	7,252,1,255,240,0,0,127,252,1,255,0,
	7,252,0,255,252,0,1,255,248,1,255,0,
	3,254,0,127,255,192,31,255,240,3,254,0,
	3,255,0,63,255,255,255,255,224,7,254,0,
	1,255,0,15,255,255,255,255,128,7,252,0,
	1,255,128,7,255,255,255,255,0,15,252,0,
	0,255,192,1,255,255,255,252,0,31,248,0,
	0,127,224,0,255,255,255,248,0,63,240,0,
	0,127,240,0,63,255,255,224,0,127,240,0,
	0,63,248,0,7,255,255,0,0,255,224,0,
	0,31,252,0,0,127,240,0,1,255,192,0,
	0,15,255,0,0,0,0,0,7,255,128,0,
	0,7,255,128,0,0,0,0,15,255,0,0,
	0,3,255,224,0,0,0,0,63,254,0,0,
	0,1,255,240,0,0,0,0,127,252,0,0,
	0,0,255,252,0,0,0,1,255,248,0,0,
	0,0,127,255,128,0,0,15,255,240,0,0,
	0,0,63,255,240,0,0,127,255,224,0,0,
	0,0,15,255,255,0,7,255,255,128,0,0,
	0,0,7,255,255,255,255,255,255,0,0,0,
	0,0,1,255,255,255,255,255,252,0,0,0,
	0,0,0,127,255,255,255,255,240,0,0,0,
	0,0,0,31,255,255,255,255,192,0,0,0,
	0,0,0,7,255,255,255,255,0,0,0,0,
	0,0,0,1,255,255,255,252,0,0,0,0,
	0,0,0,0,31,255,255,192,0,0,0,0,
	0,0,0,0,0,255,248,0,0,0,0,0
);

hexagon: array [0..119] of integer = (
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
	0, 0, 0, 0, 1, 1, 1, 0, 0, 0,
	0, 0, 0, 1, 1, 1, 1, 1, 0, 0,
	0, 0, 1, 1, 1, 1, 1, 1, 1, 0,
	0, 1, 1, 1, 1, 1, 1, 1, 1, 1,
	0, 1, 1, 1, 1, 1, 1, 1, 1, 1,
	0, 1, 1, 1, 1, 1, 1, 1, 1, 1,
	0, 1, 1, 1, 1, 1, 1, 1, 1, 1,
	0, 0, 1, 1, 1, 1, 1, 1, 1, 0,
	0, 0, 0, 1, 1, 1, 1, 1, 0, 0,
	0, 0, 0, 0, 1, 1, 1, 0, 0, 0,
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0
);

const SSET='0123456789ABCDEF';
*)

function in_alpha(glyph: Integer): Integer;
var
  retval: Integer = 0;
  cglyph: Char;
begin
  {INITCODE} retval := 0;
  {INITCODE} cglyph := Char (glyph);
  if IsTrue(((cglyph >= '0')) and ((cglyph <= '9'))) then
  begin
    retval := 1;
  end;
  if IsTrue(((cglyph >= 'A')) and ((cglyph <= 'Z'))) then
  begin
    retval := 1;
  end;
  case cglyph of
    ' ',
    '$',
    '%',
    '*',
    '+',
    '-',
    '.',
    '/',
    ':': retval := 1;
  end;
  exit (retval);
end;

procedure define_mode(mode: PChar; jisdata: PInteger; length: Integer; gs1: Boolean);
var
  j: Integer;
  mlen: Integer;
  i: Integer;
begin
  i := 0;
  while i < length do
  begin
    if IsTrue(jisdata[i] > $FF) then
    begin
      mode[i] := 'K';
    end else begin
      mode[i] := 'B';
      if IsTrue(in_alpha (jisdata[i])) then
      begin
        mode[i] := 'A';
      end;
      if IsTrue(gs1) and ((jisdata[i] = integer('['))) then
      begin
        mode[i] := 'A';
      end;
      if IsTrue(((jisdata[i] >= integer('0'))) and ((jisdata[i] <= integer('9')))) then
      begin
        mode[i] := 'N';
      end;
    end;
    Inc (i);
  end;
  i := 0;
  while i < length do
  begin
    if IsTrue(mode[i] = 'N') then
    begin
      if IsTrue(((((i <> 0)) and ((mode[i - 1] <> 'N')))) or ((i = 0))) then
      begin
        mlen := 0;
        while (((mlen + i) < length)) and ((mode[mlen + i] = 'N')) do
        begin
          Inc (mlen);
        end;
        if IsTrue(mlen < 6) then
        begin
          j := 0;
          while j < mlen do
          begin
            mode[i + j] := 'A';
            Inc (j);
          end;
        end;
      end;
    end;
    Inc (i);
  end;
  i := 0;
  while i < length do
  begin
    if IsTrue(mode[i] = 'A') then
    begin
      if IsTrue(((((i <> 0)) and ((mode[i - 1] <> 'A')))) or ((i = 0))) then
      begin
        mlen := 0;
        while (((mlen + i) < length)) and ((mode[mlen + i] = 'A')) do
        begin
          Inc (mlen);
        end;
        if IsTrue(mlen < 6) then
        begin
          j := 0;
          while j < mlen do
          begin
            mode[i + j] := 'B';
            Inc (j);
          end;
        end;
      end;
    end;
    Inc (i);
  end;
end;

function estimate_binary_length(mode: PChar; length: Integer; gs1: Boolean): Integer;
var
  count: Integer = 0;
  i: Integer = 0;
  current: Char = #0;
  a_count: Integer = 0;
  n_count: Integer = 0;
begin
  {INITCODE} count := 0;
  {INITCODE} i := 0;
  {INITCODE} current := #0;
  {INITCODE} a_count := 0;
  {INITCODE} n_count := 0;
  if IsTrue(gs1) then
  begin
    count := count + 4;
  end;
  i := 0;
  while i < length do
  begin
    if IsTrue(mode[i] <> current) then
    begin
      case mode[i] of
        'K':
          begin
            count := count + (12 + 4);
            current := 'K';
          end;
        'B':
          begin
            count := count + (16 + 4);
            current := 'B';
          end;
        'A':
          begin
            count := count + (13 + 4);
            current := 'A';
            a_count := 0;
          end;
        'N':
          begin
            count := count + (14 + 4);
            current := 'N';
            n_count := 0;
          end;
      end;
    end;
    case mode[i] of
      'K': count := count + 13;
      'B': count := count + 8;
      'A':
        begin
          Inc (a_count);
          if IsTrue((a_count and 1) = 0) then
          begin
            count := count + 5;
            a_count := 0;
          end else begin
            count := count + 6;
          end;
        end;
      'N':
        begin
          Inc (n_count);
          if IsTrue((n_count mod 3) = 0) then
          begin
            count := count + 3;
            n_count := 0;
          end else begin
            if IsTrue((n_count and 1) = 0) then
            begin
              count := count + 3;
            end else begin
              count := count + 4;
            end;
          end;
      end;
    end;
    Inc (i);
  end;
  exit (count);
end;

procedure qr_bscan(binary: PChar; data: Integer; h: Integer);
begin
  while h<>0 do
  begin
    concat (binary, iif (data and h,'1','0'));
    h := h shr 1;
  end;
end;

procedure qr_bscan(var binary: array of char; data: Integer; h: Integer);
begin
  qr_bscan(pchar(@binary[0]),data,h);
end;

procedure qr_binary(datastream: PInteger; version: Integer; target_binlen: Integer; mode: PChar; jisdata: PInteger; length: Integer; gs1: Boolean; est_binlen: Integer);
var
  debug: Integer = 0;
  position: Integer = 0;
  scheme: Integer = 1;
  i: Integer = 1;
  short_data_block_length: Integer = 1;
  padbits: BYTE;
  data_block: Char;
  current_bytes: Integer;
  current_binlen: Integer;
  percent: Integer;
  toggle: Integer;
  binary: array of Char;
  jis: Integer;
  prod: Integer;
  lsb: Integer;
  msb: Integer;
  byte: Integer;
  count: Integer;
//  prod: Integer;
  second: Integer = 0;
  first: Integer = 0;
//  count: Integer;
//  prod: Integer;
  third: Integer = 0;
//  second: Integer = 0;
//  first: Integer = 0;
begin
  {INITCODE} debug := 0;
  {INITCODE} position := 0;
  {INITCODE} scheme := 1;
  {INITCODE} i := 1;
  {INITCODE} short_data_block_length := 1;
  SetLength(binary,est_binlen + 12);

  strcpy (binary, '');
  if IsTrue(gs1) then
  begin
    concat (binary, '0101');
  end;
  if IsTrue(version <= 9) then
  begin
    scheme := 1;
  end else begin
    if IsTrue(((version >= 10)) and ((version <= 26))) then
    begin
      scheme := 2;
    end else begin
      if IsTrue(version >= 27) then
      begin
        scheme := 3;
      end;
    end;
  end;
  if IsTrue(debug) then
  begin
    i := 0;
    while i < length do
    begin
      write( format ('%s',[mode[i]]) );
      Inc (i);
    end;
    write( LineEnding );
  end;
  percent := 0;
  repeat
    data_block := mode[position];
    short_data_block_length := 0;
    repeat
      Inc (short_data_block_length);
    until NotBoolean ((((short_data_block_length + position) < length)) and ((mode[position + short_data_block_length] = data_block)));
    case data_block of
      'K':
        begin
          concat (binary, '1000');
          qr_bscan (binary, short_data_block_length, $20 shl (scheme * 2));
          if IsTrue(debug) then
          begin
            write( format ('Kanji block (length %d)'+LineEnding+char(9),[short_data_block_length]) );
          end;
          i := 0;
          while i < short_data_block_length do
          begin
            {INITCODE} jis := jisdata[position + i];
            if IsTrue(jis > $9FFF) then
            begin
              jis := jis - $C140;
            end;
            msb := (jis and $FF00) shr 4;
            lsb := (jis and $FF);
            prod := (msb * $C0) + lsb;
            qr_bscan (binary, prod, $1000);
            if IsTrue(debug) then
            begin
              write( format ('0x%.4X ',[prod]) );
            end;
            Inc (i);
          end;
          if IsTrue(debug) then
          begin
            write(LineEnding);
          end;
        end;
      'B':
        begin
          concat (binary, '0100');
          qr_bscan (binary, short_data_block_length, iif (scheme > 1,$8000,$80));
          if IsTrue(debug) then
          begin
            write( format ('Byte block (length %d)'+LineEnding+chr(9),[short_data_block_length]) );
          end;
          i := 0;
          while i < short_data_block_length do
          begin
            {INITCODE} byte := jisdata[position + i];
            if IsTrue(gs1) and ((byte = integer('['))) then
            begin
              byte := $1D;
            end;
            qr_bscan (binary, byte, $80);
            if IsTrue(debug) then
            begin
              write( format ('0x%.2X(%d) ',[byte, byte]) );
            end;
            Inc (i);
          end;
          if IsTrue(debug) then
          begin
            write( LineEnding );
          end;
        end;
      'A':
        begin
          concat (binary, '0010');
          qr_bscan (binary, short_data_block_length, $40 shl (2 * scheme));
          if IsTrue(debug) then
          begin
            write( format ('Alpha block (length %d)'+LineEnding+chr(9),[short_data_block_length]) );
          end;
          i := 0;
          while i < short_data_block_length do
          begin
            {INITCODE} second := 0;
            {INITCODE} first := 0;
            if IsTrue(percent = 0) then
            begin
              if IsTrue(gs1) and ((jisdata[position + i] = integer('%'))) then
              begin
                first := posn (RHODIUM, '%');
                second := posn (RHODIUM, '%');
                count := 2;
                prod := (first * 45) + second;
                Inc (i);
              end else begin
                if IsTrue(gs1) and ((jisdata[position + i] = integer('['))) then
                begin
                  first := posn (RHODIUM, '%');
                end else begin
                  first := posn (RHODIUM, Char (jisdata[position + i]));
                end;
                count := 1;
                Inc (i);
                prod := first;
                if IsTrue(mode[position + i] = 'A') then
                begin
                  if IsTrue(gs1) and ((jisdata[position + i] = integer('%'))) then
                  begin
                    second := posn (RHODIUM, '%');
                    count := 2;
                    prod := (first * 45) + second;
                    percent := 1;
                  end else begin
                    if IsTrue(gs1) and ((jisdata[position + i] = integer('['))) then
                    begin
                      second := posn (RHODIUM, '%');
                    end else begin
                      second := posn (RHODIUM, Char (jisdata[position + i]));
                    end;
                    count := 2;
                    Inc (i);
                    prod := (first * 45) + second;
                  end;
                end;
              end;
            end else begin
              first := posn (RHODIUM, '%');
              count := 1;
              Inc (i);
              prod := first;
              percent := 0;
              if IsTrue(mode[position + i] = 'A') then
              begin
                if IsTrue(gs1) and ((jisdata[position + i] = integer('%'))) then
                begin
                  second := posn (RHODIUM, '%');
                  count := 2;
                  prod := (first * 45) + second;
                  percent := 1;
                end else begin
                  if IsTrue(gs1) and ((jisdata[position + i] = integer('['))) then
                  begin
                    second := posn (RHODIUM, '%');
                  end else begin
                    second := posn (RHODIUM, Char (jisdata[position + i]));
                  end;
                  count := 2;
                  Inc (i);
                  prod := (first * 45) + second;
                end;
              end;
            end;
            qr_bscan (binary, prod, iif (count = 2,$400,$20));
            if IsTrue(debug) then
            begin
              write( format ('0x%.4X ',[prod]) );
            end;
          end;
          if IsTrue(debug) then
          begin
            write( LineEnding );
          end;
        end;
      'N':
        begin
          concat (binary, '0001');
          qr_bscan (binary, short_data_block_length, $80 shl (2 * scheme));
          if IsTrue(debug) then
          begin
            write( format ('Number block (length %d)'+LineEnding+chr(9),[short_data_block_length]) );
          end;
          i := 0;
          while i < short_data_block_length do
          begin
            {INITCODE} third := 0;
            {INITCODE} second := 0;
            {INITCODE} first := 0;
            first := posn (NEON, Char (jisdata[position + i]));
            count := 1;
            prod := first;
            if IsTrue(mode[position + i + 1] = 'N') then
            begin
              second := posn (NEON, Char (jisdata[position + i + 1]));
              count := 2;
              prod := (prod * 10) + second;
              if IsTrue(mode[position + i + 2] = 'N') then
              begin
                third := posn (NEON, Char (jisdata[position + i + 2]));
                count := 3;
                prod := (prod * 10) + third;
              end;
            end;
            qr_bscan (binary, prod, 1 shl (3 * count));
            if IsTrue(debug) then
            begin
              write( format ('0x%.4X (%d)',[prod, prod]) );
            end;
            i := i + count;
          end;
          if IsTrue(debug) then
          begin
            write( LineEnding );
          end;
        end;
    end;
    position := position + short_data_block_length;
  until NotBoolean (position < length);
  concat (binary, '0000');
  current_binlen := strlen (binary);
  padbits := 8 - (current_binlen mod 8);
  if IsTrue(padbits = 8) then
  begin
    padbits := 0;
  end;
  current_bytes := (current_binlen + padbits) div 8;
  i := 0;
  while i < padbits do
  begin
    concat (binary, '0');
    Inc (i);
  end;
  i := 0;
  while i < current_bytes do
  begin
    datastream[i] := $00;
    if IsTrue(binary[i * 8] = '1') then
    begin
      datastream[i] := datastream[i] + $80;
    end;
    if IsTrue(binary[i * 8 + 1] = '1') then
    begin
      datastream[i] := datastream[i] + $40;
    end;
    if IsTrue(binary[i * 8 + 2] = '1') then
    begin
      datastream[i] := datastream[i] + $20;
    end;
    if IsTrue(binary[i * 8 + 3] = '1') then
    begin
      datastream[i] := datastream[i] + $10;
    end;
    if IsTrue(binary[i * 8 + 4] = '1') then
    begin
      datastream[i] := datastream[i] + $08;
    end;
    if IsTrue(binary[i * 8 + 5] = '1') then
    begin
      datastream[i] := datastream[i] + $04;
    end;
    if IsTrue(binary[i * 8 + 6] = '1') then
    begin
      datastream[i] := datastream[i] + $02;
    end;
    if IsTrue(binary[i * 8 + 7] = '1') then
    begin
      datastream[i] := datastream[i] + $01;
    end;
    Inc (i);
  end;
  toggle := 0;
  i := current_bytes;
  while i < target_binlen do
  begin
    if IsTrue(toggle = 0) then
    begin
      datastream[i] := $EC;
      toggle := 1;
    end else begin
      datastream[i] := $11;
      toggle := 0;
    end;
    Inc (i);
  end;
  if IsTrue(debug) then
  begin
    write( format ('Resulting codewords:'+LineEnding+chr(9),[]) );
    i := 0;
    while i < target_binlen do
    begin
      write( format ('0x%.2X ',[datastream[i]]) );
      Inc (i);
    end;
    write( LineEnding );
  end;
end;

procedure add_ecc(fullstream: PInteger; datastream: PInteger; version: Integer; data_cw: Integer; blocks: Integer);
var
  ecc_cw: Integer;
  short_data_block_length: Integer;
  qty_long_blocks: Integer;
  qty_short_blocks: Integer;
  ecc_block_length: Integer;
  debug: Integer = 0;
  posn: Integer = 0;
  length_this_block: Integer = 0;
  j: Integer = 0;
  i: Integer = 0;
  data_block: array of BYTE;
  ecc_block: array of BYTE;
  interleaved_data: array of Integer;
  interleaved_ecc: array of Integer;
begin
  {INITCODE} ecc_cw := qr_total_codewords[version - 1] - data_cw;
  {INITCODE} short_data_block_length := data_cw div blocks;
  {INITCODE} qty_long_blocks := data_cw mod blocks;
  {INITCODE} qty_short_blocks := blocks - qty_long_blocks;
  {INITCODE} ecc_block_length := ecc_cw div blocks;
  {INITCODE} debug := 0;
  {INITCODE} posn := 0;
  {INITCODE} length_this_block := 0;
  {INITCODE} j := 0;
  {INITCODE} i := 0;
  SetLength(data_block,short_data_block_length + 2);
  SetLength(ecc_block,ecc_block_length + 2);
  SetLength(interleaved_data,data_cw + 2);
  SetLength(interleaved_ecc,ecc_cw + 2);
  posn := 0;
  i := 0;
  while i < blocks do
  begin
    if IsTrue(i < qty_short_blocks) then
    begin
      length_this_block := short_data_block_length;
    end else begin
      length_this_block := short_data_block_length + 1;
    end;
    j := 0;
    while j < ecc_block_length do
    begin
      ecc_block[j] := 0;
      Inc (j);
    end;
    j := 0;
    while j < length_this_block do
    begin
      data_block[j] := BYTE (datastream[posn + j]);
      Inc (j);
    end;
    rs_init_gf ($11D);
    rs_init_code (ecc_block_length, 0);
    rs_encode (length_this_block, @data_block[0], @ecc_block[0]);
    rs_free;
    if IsTrue(debug) then
    begin
      write( format ('Block %d: ',[i + 1]) );
      j := 0;
      while j < length_this_block do
      begin
        write( format ('%2X ',[data_block[j]]) );
        Inc (j);
      end;
      if IsTrue(i < qty_short_blocks) then
      begin
        write( format ('   ',[]) );
      end;
      write( format (' // ',[]) );
      j := 0;
      while j < ecc_block_length do
      begin
        write( format ('%.2X ',[ecc_block[ecc_block_length - j - 1]]) );
        Inc (j);
      end;
      write( LineEnding );
    end;
    j := 0;
    while j < short_data_block_length do
    begin
      interleaved_data[(j * blocks) + i] := Integer (data_block[j]);
      Inc (j);
    end;
    if IsTrue(i >= qty_short_blocks) then
    begin
      interleaved_data[(short_data_block_length * blocks) + (i - qty_short_blocks)] := Integer (data_block[short_data_block_length]);
    end;
    j := 0;
    while j < ecc_block_length do
    begin
      interleaved_ecc[(j * blocks) + i] := Integer (ecc_block[ecc_block_length - j - 1]);
      Inc (j);
    end;
    posn := posn + length_this_block;
    Inc (i);
  end;
  j := 0;
  while j < data_cw do
  begin
    fullstream[j] := interleaved_data[j];
    Inc (j);
  end;
  j := 0;
  while j < ecc_cw do
  begin
    fullstream[j + data_cw] := interleaved_ecc[j];
    Inc (j);
  end;
  if IsTrue(debug) then
  begin
    write( format (LineEnding+'Data Stream: '+LineEnding,[]) );
    j := 0;
    while j < (data_cw + ecc_cw) do
    begin
      write( format ('%.2X ',[fullstream[j]]) );
      Inc (j);
    end;
    write( LineEnding );
  end;
end;

procedure place_finder(grid: PBYTE; size: Integer; x: Integer; y: Integer);
var
  yp: Integer;
  xp: Integer;
const
  finder: array [0..48] of integer= (
        1, 1, 1, 1, 1, 1, 1,
        1, 0, 0, 0, 0, 0, 1,
        1, 0, 1, 1, 1, 0, 1,
        1, 0, 1, 1, 1, 0, 1,
        1, 0, 1, 1, 1, 0, 1,
        1, 0, 0, 0, 0, 0, 1,
        1, 1, 1, 1, 1, 1, 1
        );
begin
  xp := 0;
  while xp < 7 do
  begin
    yp := 0;
    while yp < 7 do
    begin
      if IsTrue(finder[xp + (7 * yp)] = 1) then
      begin
        grid[((yp + y) * size) + (xp + x)] := $11;
      end else begin
        grid[((yp + y) * size) + (xp + x)] := $10;
      end;
      Inc (yp);
    end;
    Inc (xp);
  end;
end;

procedure place_align(grid: PBYTE; size: Integer; x: Integer; y: Integer);
var
  yp: Integer;
  xp: Integer;
const
  alignment: array [0..24] of integer = (
        1, 1, 1, 1, 1,
        1, 0, 0, 0, 1,
        1, 0, 1, 0, 1,
        1, 0, 0, 0, 1,
        1, 1, 1, 1, 1
        );
begin
  x := x - 2;
  y := y - 2;
  xp := 0;
  while xp < 5 do
  begin
    yp := 0;
    while yp < 5 do
    begin
      if IsTrue(alignment[xp + (5 * yp)] = 1) then
      begin
        grid[((yp + y) * size) + (xp + x)] := $11;
      end else begin
        grid[((yp + y) * size) + (xp + x)] := $10;
      end;
      Inc (yp);
    end;
    Inc (xp);
  end;
end;

procedure setup_grid(grid: PBYTE; size: Integer; version: Integer);
var
  toggle: Integer = 1;
  i: Integer = 1;
  ycoord: Integer;
  xcoord: Integer;
  y: Integer;
  x: Integer;
  loopsize: Integer;
begin
  {INITCODE} toggle := 1;
  {INITCODE} i := 1;
  i := 0;
  while i < size do
  begin
    if IsTrue(toggle = 1) then
    begin
      grid[(6 * size) + i] := $21;
      grid[(i * size) + 6] := $21;
      toggle := 0;
    end else begin
      grid[(6 * size) + i] := $20;
      grid[(i * size) + 6] := $20;
      toggle := 1;
    end;
    Inc (i);
  end;
  place_finder (grid, size, 0, 0);
  place_finder (grid, size, 0, size - 7);
  place_finder (grid, size, size - 7, 0);
  i := 0;
  while i < 7 do
  begin
    grid[(7 * size) + i] := $10;
    grid[(i * size) + 7] := $10;
    grid[(7 * size) + (size - 1 - i)] := $10;
    grid[(i * size) + (size - 8)] := $10;
    grid[((size - 8) * size) + i] := $10;
    grid[((size - 1 - i) * size) + 7] := $10;
    Inc (i);
  end;
  grid[(7 * size) + 7] := $10;
  grid[(7 * size) + (size - 8)] := $10;
  grid[((size - 8) * size) + 7] := $10;
  if IsTrue(version <> 1) then
  begin
    loopsize := qr_align_loopsize[version - 1];
    x := 0;
    while x < loopsize do
    begin
      y := 0;
      while y < loopsize do
      begin
        xcoord := qr_table_e1[((version - 2) * 7) + x];
        ycoord := qr_table_e1[((version - 2) * 7) + y];
        if not IsTrue((grid[(ycoord * size) + xcoord] and $10)) then
        begin
          place_align (grid, size, xcoord, ycoord);
        end;
        Inc (y);
      end;
      Inc (x);
    end;
  end;
  i := 0;
  while i < 8 do
  begin
    grid[(8 * size) + i] := grid[(8 * size) + i] + $20;
    grid[(i * size) + 8] := grid[(i * size) + 8] + $20;
    grid[(8 * size) + (size - 1 - i)] := $20;
    grid[((size - 1 - i) * size) + 8] := $20;
    Inc (i);
  end;
  grid[(8 * size) + 8] := grid[(8 * size) + 8] + 20;
  grid[((size - 1 - 7) * size) + 8] := $21;
  if IsTrue(version >= 7) then
  begin
    i := 0;
    while i < 6 do
    begin
      grid[((size - 9) * size) + i] := $20;
      grid[((size - 10) * size) + i] := $20;
      grid[((size - 11) * size) + i] := $20;
      grid[(i * size) + (size - 9)] := $20;
      grid[(i * size) + (size - 10)] := $20;
      grid[(i * size) + (size - 11)] := $20;
      Inc (i);
    end;
  end;
end;

function cwbit(datastream: PInteger; i: Integer): Integer;
var
  word: Integer;
  bit: Integer;
  resultant: Integer = 0;
begin
  {INITCODE} word := i div 8;
  {INITCODE} bit := i mod 8;
  {INITCODE} resultant := 0;
  case bit of
    0:
      begin
        if IsTrue(datastream[word] and $80) then
        begin
          resultant := 1;
        end else begin
          resultant := 0;
        end;
      end;
    1:
      begin
        if IsTrue(datastream[word] and $40) then
        begin
          resultant := 1;
        end else begin
          resultant := 0;
        end;
      end;
    2:
      begin
        if IsTrue(datastream[word] and $20) then
        begin
          resultant := 1;
        end else begin
          resultant := 0;
        end;
      end;
    3:
      begin
        if IsTrue(datastream[word] and $10) then
        begin
          resultant := 1;
        end else begin
          resultant := 0;
        end;
      end;
    4:
      begin
        if IsTrue(datastream[word] and $08) then
        begin
          resultant := 1;
        end else begin
          resultant := 0;
        end;
      end;
    5:
      begin
        if IsTrue(datastream[word] and $04) then
        begin
          resultant := 1;
        end else begin
          resultant := 0;
        end;
      end;
    6:
      begin
        if IsTrue(datastream[word] and $02) then
        begin
          resultant := 1;
        end else begin
          resultant := 0;
        end;
      end;
    7:
      begin
        if IsTrue(datastream[word] and $01) then
        begin
          resultant := 1;
        end else begin
          resultant := 0;
        end;
      end;
  end;
  exit (resultant);
end;

procedure populate_grid(grid: PBYTE; size: Integer; datastream: PInteger; cw: Integer);
var
  direction: Integer = 1;
  row: Integer = 0;
  y: Integer;
  x: Integer;
  n: Integer;
  i: Integer;
begin
  {INITCODE} direction := 1;
  {INITCODE} row := 0;
  n := cw * 8;
  y := size - 1;
  i := 0;
  repeat
    x := (size - 2) - (row * 2);
    if IsTrue(x < 6) then
    begin
      Dec (x);
    end;
    if not IsTrue((grid[(y * size) + (x + 1)] and $F0)) then
    begin
      if IsTrue(cwbit (datastream, i)) then
      begin
        grid[(y * size) + (x + 1)] := $01;
      end else begin
        grid[(y * size) + (x + 1)] := $00;
      end;
      Inc (i);
    end;
    if IsTrue(i < n) then
    begin
      if not IsTrue((grid[(y * size) + x] and $F0)) then
      begin
        if IsTrue(cwbit (datastream, i)) then
        begin
          grid[(y * size) + x] := $01;
        end else begin
          grid[(y * size) + x] := $00;
        end;
        Inc (i);
      end;
    end;
    if IsTrue(direction) then
    begin
      Dec (y);
    end else begin
      Inc (y);
    end;
    if IsTrue(y = -1) then
    begin
      Inc (row);
      y := 0;
      direction := 0;
    end;
    if IsTrue(y = size) then
    begin
      Inc (row);
      y := size - 1;
      direction := 1;
    end;
  until not (i < n);
end;

function evaluate(grid: PBYTE; size: Integer; pattern: Integer): Integer;
var
  block: Integer;
  y: Integer;
  x: Integer;
  resultcode: Integer = 0;
  state: Char;
  p: Integer;
  dark_mods: Integer;
  k: Integer;
  percentage: Integer;
  local: array of Char;
begin
  {INITCODE} resultcode := 0;
  SetLength(local,size * size);
  x := 0;
  while x < size do
  begin
    y := 0;
    while y < size do
    begin
      case pattern of
        0:
          begin
            if IsTrue(grid[(y * size) + x] and $01) then
            begin
              local[(y * size) + x] := '1';
            end else begin
              local[(y * size) + x] := '0';
            end;
          end;
        1:
          begin
            if IsTrue(grid[(y * size) + x] and $02) then
            begin
              local[(y * size) + x] := '1';
            end else begin
              local[(y * size) + x] := '0';
            end;
          end;
        2:
          begin
            if IsTrue(grid[(y * size) + x] and $04) then
            begin
              local[(y * size) + x] := '1';
            end else begin
              local[(y * size) + x] := '0';
            end;
          end;
        3:
          begin
            if IsTrue(grid[(y * size) + x] and $08) then
            begin
              local[(y * size) + x] := '1';
            end else begin
              local[(y * size) + x] := '0';
            end;
          end;
        4:
          begin
            if IsTrue(grid[(y * size) + x] and $10) then
            begin
              local[(y * size) + x] := '1';
            end else begin
              local[(y * size) + x] := '0';
            end;
          end;
        5:
          begin
            if IsTrue(grid[(y * size) + x] and $20) then
            begin
              local[(y * size) + x] := '1';
            end else begin
              local[(y * size) + x] := '0';
            end;
          end;
        6:
          begin
            if IsTrue(grid[(y * size) + x] and $40) then
            begin
              local[(y * size) + x] := '1';
            end else begin
              local[(y * size) + x] := '0';
            end;
          end;
        7:
          begin
            if IsTrue(grid[(y * size) + x] and $80) then
            begin
              local[(y * size) + x] := '1';
            end else begin
              local[(y * size) + x] := '0';
            end;
          end;
      end;
      Inc (y);
    end;
    Inc (x);
  end;
  x := 0;
  while x < size do
  begin
    state := local[x];
    block := 0;
    y := 0;
    while y < size do
    begin
      if IsTrue(local[(y * size) + x] = state) then
      begin
        Inc (block);
      end else begin
        if IsTrue(block > 5) then
        begin
          resultcode := resultcode + ((3 + block));
        end;
        block := 0;
        state := local[(y * size) + x];
      end;
      Inc (y);
    end;
    if IsTrue(block > 5) then
    begin
      resultcode := resultcode + ((3 + block));
    end;
    Inc (x);
  end;
  y := 0;
  while y < size do
  begin
    state := local[y * size];
    block := 0;
    x := 0;
    while x < size do
    begin
      if IsTrue(local[(y * size) + x] = state) then
      begin
        Inc (block);
      end else begin
        if IsTrue(block > 5) then
        begin
          resultcode := resultcode + ((3 + block));
        end;
        block := 0;
        state := local[(y * size) + x];
      end;
      Inc (x);
    end;
    if IsTrue(block > 5) then
    begin
      resultcode := resultcode + ((3 + block));
    end;
    Inc (y);
  end;
  x := 0;
  while x < size do
  begin
    y := 0;
    while y < (size - 7) do
    begin
      p := 0;
      if IsTrue(local[(y * size) + x] = '1') then
      begin
        p := p + $40;
      end;
      if IsTrue(local[((y + 1) * size) + x] = '1') then
      begin
        p := p + $20;
      end;
      if IsTrue(local[((y + 2) * size) + x] = '1') then
      begin
        p := p + $10;
      end;
      if IsTrue(local[((y + 3) * size) + x] = '1') then
      begin
        p := p + $08;
      end;
      if IsTrue(local[((y + 4) * size) + x] = '1') then
      begin
        p := p + $04;
      end;
      if IsTrue(local[((y + 5) * size) + x] = '1') then
      begin
        p := p + $02;
      end;
      if IsTrue(local[((y + 6) * size) + x] = '1') then
      begin
        p := p + $01;
      end;
      if IsTrue(p = $5D) then
      begin
        resultcode := resultcode + 40;
      end;
      Inc (y);
    end;
    Inc (x);
  end;
  y := 0;
  while y < size do
  begin
    x := 0;
    while x < (size - 7) do
    begin
      p := 0;
      if IsTrue(local[(y * size) + x] = '1') then
      begin
        p := p + $40;
      end;
      if IsTrue(local[(y * size) + x + 1] = '1') then
      begin
        p := p + $20;
      end;
      if IsTrue(local[(y * size) + x + 2] = '1') then
      begin
        p := p + $10;
      end;
      if IsTrue(local[(y * size) + x + 3] = '1') then
      begin
        p := p + $08;
      end;
      if IsTrue(local[(y * size) + x + 4] = '1') then
      begin
        p := p + $04;
      end;
      if IsTrue(local[(y * size) + x + 5] = '1') then
      begin
        p := p + $02;
      end;
      if IsTrue(local[(y * size) + x + 6] = '1') then
      begin
        p := p + $01;
      end;
      if IsTrue(p = $5D) then
      begin
        resultcode := resultcode + 40;
      end;
      Inc (x);
    end;
    Inc (y);
  end;
  dark_mods := 0;
  x := 0;
  while x < size do
  begin
    y := 0;
    while y < size do
    begin
      if IsTrue(local[(y * size) + x] = '1') then
      begin
        Inc (dark_mods);
      end;
      Inc (y);
    end;
    Inc (x);
  end;
  percentage := 100 * (dark_mods div (size * size));
  if IsTrue(percentage <= 50) then
  begin
    k := ((100 - percentage) - 50) div 5;
  end else begin
    k := (percentage - 50) div 5;
  end;
  resultcode := resultcode + (10 * k);
  exit (resultcode);
end;

function apply_bitmask(grid: PBYTE; size: Integer): Integer;
var
  y: Integer;
  x: Integer;
  p: BYTE;
  penalty: array [0..8-1] of Integer;
  pattern: Integer;
  best_pattern: Integer;
  best_val: Integer;
  bit: Integer;
  mask: array of BYTE;
  eval: array of BYTE;
begin
  SetLength(mask,size * size);
  SetLength(eval,size * size);
  x := 0;
  while x < size do
  begin
    y := 0;
    while y < size do
    begin
      mask[(y * size) + x] := $00;
      if not IsTrue((grid[(y * size) + x] and $F0)) then
      begin
        if IsTrue(((y + x) and 1) = 0) then
        begin
          mask[(y * size) + x] := mask[(y * size) + x] + $01;
        end;
        if IsTrue((y and 1) = 0) then
        begin
          mask[(y * size) + x] := mask[(y * size) + x] + $02;
        end;
        if IsTrue((x mod 3) = 0) then
        begin
          mask[(y * size) + x] := mask[(y * size) + x] + $04;
        end;
        if IsTrue(((y + x) mod 3) = 0) then
        begin
          mask[(y * size) + x] := mask[(y * size) + x] + $08;
        end;
        if IsTrue((((y div 2) + (x div 3)) and 1) = 0) then
        begin
          mask[(y * size) + x] := mask[(y * size) + x] + $10;
        end;
        if IsTrue((((y * x) and 1) + ((y * x) mod 3)) = 0) then
        begin
          mask[(y * size) + x] := mask[(y * size) + x] + $20;
        end;
        if IsTrue(((((y * x) and 1) + ((y * x) mod 3)) and 1) = 0) then
        begin
          mask[(y * size) + x] := mask[(y * size) + x] + $40;
        end;
        if IsTrue(((((y + x) and 1) + ((y * x) mod 3)) and 1) = 0) then
        begin
          mask[(y * size) + x] := mask[(y * size) + x] + $80;
        end;
      end;
      Inc (y);
    end;
    Inc (x);
  end;
  x := 0;
  while x < size do
  begin
    y := 0;
    while y < size do
    begin
      if IsTrue(grid[(y * size) + x] and $01) then
      begin
        p := $FF;
      end else begin
        p := $00;
      end;
      eval[(y * size) + x] := mask[(y * size) + x] xor p;
      Inc (y);
    end;
    Inc (x);
  end;
  pattern := 0;
  while pattern < 8 do
  begin
    penalty[pattern] := evaluate (@eval[0], size, pattern);
    Inc (pattern);
  end;
  best_pattern := 0;
  best_val := penalty[0];
  pattern := 1;
  while pattern < 8 do
  begin
    if IsTrue(penalty[pattern] < best_val) then
    begin
      best_pattern := pattern;
      best_val := penalty[pattern];
    end;
    Inc (pattern);
  end;
  x := 0;
  while x < size do
  begin
    y := 0;
    while y < size do
    begin
      bit := 0;
      case best_pattern of
        0:
          begin
            if IsTrue(mask[(y * size) + x] and $01) then
            begin
              bit := 1;
            end;
          end;
        1:
          begin
            if IsTrue(mask[(y * size) + x] and $02) then
            begin
              bit := 1;
            end;
          end;
        2:
          begin
            if IsTrue(mask[(y * size) + x] and $04) then
            begin
              bit := 1;
            end;
          end;
        3:
          begin
            if IsTrue(mask[(y * size) + x] and $08) then
            begin
              bit := 1;
            end;
          end;
        4:
          begin
            if IsTrue(mask[(y * size) + x] and $10) then
            begin
              bit := 1;
            end;
          end;
        5:
          begin
            if IsTrue(mask[(y * size) + x] and $20) then
            begin
              bit := 1;
            end;
          end;
        6:
          begin
            if IsTrue(mask[(y * size) + x] and $40) then
            begin
              bit := 1;
            end;
          end;
        7:
          begin
            if IsTrue(mask[(y * size) + x] and $80) then
            begin
              bit := 1;
            end;
          end;
      end;
      if IsTrue(bit = 1) then
      begin
        if IsTrue(grid[(y * size) + x] and $01) then
        begin
          grid[(y * size) + x] := $00;
        end else begin
          grid[(y * size) + x] := $01;
        end;
      end;
      Inc (y);
    end;
    Inc (x);
  end;
  exit (best_pattern);
end;

procedure add_format_info(grid: PBYTE; size: Integer; ecc_level: Integer; pattern: Integer);
var
  format: Integer;
  seq: Cardinal;
  i: Integer;
begin
  {INITCODE} format := pattern;
  case ecc_level of
    LEVEL_L: format := format + $08;
    LEVEL_Q: format := format + $18;
    LEVEL_H: format := format + $10;
  end;
  seq := qr_annex_c[format];
  i := 0;
  while i < 6 do
  begin
    grid[(i * size) + 8] := grid[(i * size) + 8] + ((seq shr i) and $01);
    Inc (i);
  end;
  i := 0;
  while i < 8 do
  begin
    grid[(8 * size) + (size - i - 1)] := grid[(8 * size) + (size - i - 1)] + ((seq shr i) and $01);
    Inc (i);
  end;
  i := 0;
  while i < 6 do
  begin
    grid[(8 * size) + (5 - i)] := grid[(8 * size) + (5 - i)] + ((seq shr (i + 9)) and $01);
    Inc (i);
  end;
  i := 0;
  while i < 7 do
  begin
    grid[(((size - 7) + i) * size) + 8] := grid[(((size - 7) + i) * size) + 8] + ((seq shr (i + 8)) and $01);
    Inc (i);
  end;
  grid[(7 * size) + 8] := grid[(7 * size) + 8] + ((seq shr 6) and $01);
  grid[(8 * size) + 8] := grid[(8 * size) + 8] + ((seq shr 7) and $01);
  grid[(8 * size) + 7] := grid[(8 * size) + 7] + ((seq shr 8) and $01);
end;

procedure add_version_info(grid: PBYTE; size: Integer; version: Integer);
var
  i: Integer;
  version_data: cardinal;
begin
  {INITCODE} version_data := qr_annex_d[version - 7];
  i := 0;
  while i < 6 do
  begin
    grid[((size - 11) * size) + i] := grid[((size - 11) * size) + i] + ((version_data shr (i * 3)) and $01);
    grid[((size - 10) * size) + i] := grid[((size - 10) * size) + i] + ((version_data shr ((i * 3) + 1)) and $01);
    grid[((size - 9) * size) + i] := grid[((size - 9) * size) + i] + ((version_data shr ((i * 3) + 2)) and $01);
    grid[(i * size) + (size - 11)] := grid[(i * size) + (size - 11)] + ((version_data shr (i * 3)) and $01);
    grid[(i * size) + (size - 10)] := grid[(i * size) + (size - 10)] + ((version_data shr ((i * 3) + 1)) and $01);
    grid[(i * size) + (size - 9)] := grid[(i * size) + (size - 9)] + ((version_data shr ((i * 3) + 2)) and $01);
    Inc (i);
  end;
end;

function qr_code(symbol: PointerTo_zint_symbol; source: PBYTE; length: Integer): Integer;
var
  est_binlen: Integer;
  glyph: Integer;
  j: Integer;
  i: Integer;
  error_number: Integer;
  size: Integer;
  blocks: Integer;
  target_binlen: Integer;
  max_cw: Integer;
  version: Integer;
  autosize: Integer;
  ecc_level: Integer;
  gs1: Boolean;
  bitmask: Integer;
  utfdata: array of Integer;
  jisdata: array of Integer;
  mode: array of Char;
  datastream: array of Integer;
  fullstream: array of Integer;
  grid: array of BYTE;
begin
  SetLength(utfdata,length + 1);
  SetLength(jisdata,length + 1);
  SetLength(mode,length + 1);

  gs1 := (symbol^.input_mode = GS1_MODE);
  case symbol^.input_mode of
    DATA_MODE:
      begin
        i := 0;
        while i < length do
        begin
          jisdata[i] := Integer (source[i]);
          Inc (i);
        end;
      end;
    otherwise
      begin
        error_number := utf8toutf16 (symbol, source, @utfdata[0], @length);
        if IsTrue(error_number <> 0) then
        begin
          exit (error_number);
        end;
        i := 0;
        while i < length do
        begin
          if IsTrue(utfdata[i] <= $FF) then
          begin
            jisdata[i] := utfdata[i];
          end else begin
            j := 0;
            glyph := 0;
            repeat
              if IsTrue(sjis_lookup[j * 2] = utfdata[i]) then
              begin
                glyph := sjis_lookup[(j * 2) + 1];
              end;
              Inc (j);
            until NotBoolean (((j < 6843)) and ((glyph = 0)));
            if IsTrue(glyph = 0) then
            begin
              strcpy (symbol^.errtxt, 'Invalid character in input data');
              exit (ERROR_INVALID_DATA);
            end;
            jisdata[i] := glyph;
          end;
          Inc (i);
        end;
      end;
  end;
  define_mode (@mode[0], @jisdata[0], length, gs1);
  est_binlen := estimate_binary_length (@mode[0], length, gs1);
  ecc_level := LEVEL_L;
  max_cw := 2956;
  if IsTrue(((symbol^.option_1 >= 1)) and ((symbol^.option_1 <= 4))) then
  begin
    case symbol^.option_1 of
      1:
        begin
          ecc_level := LEVEL_L;
          max_cw := 2956;
        end;
      2:
        begin
          ecc_level := LEVEL_M;
          max_cw := 2334;
        end;
      3:
        begin
          ecc_level := LEVEL_Q;
          max_cw := 1666;
        end;
      4:
        begin
          ecc_level := LEVEL_H;
          max_cw := 1276;
        end;
    end;
  end;
  if IsTrue(est_binlen > (8 * max_cw)) then
  begin
    strcpy (symbol^.errtxt, 'Input too long for selected error correction level');
    exit (ERROR_TOO_LONG);
  end;
  autosize := 40;
  i := 39;
  while i >= 0 do
  begin
    case ecc_level of
      LEVEL_L:
        begin
          if IsTrue((8 * qr_data_codewords_L[i]) >= est_binlen) then
          begin
            autosize := i + 1;
          end;
        end;
      LEVEL_M:
        begin
          if IsTrue((8 * qr_data_codewords_M[i]) >= est_binlen) then
          begin
            autosize := i + 1;
          end;
        end;
      LEVEL_Q:
        begin
          if IsTrue((8 * qr_data_codewords_Q[i]) >= est_binlen) then
          begin
            autosize := i + 1;
          end;
        end;
      LEVEL_H:
        begin
          if IsTrue((8 * qr_data_codewords_H[i]) >= est_binlen) then
          begin
            autosize := i + 1;
          end;
        end;
    end;
    Dec (i);
  end;
  if IsTrue(((symbol^.option_2 >= 1)) and ((symbol^.option_2 <= 40))) then
  begin
    if IsTrue(symbol^.option_2 > autosize) then
    begin
      version := symbol^.option_2;
    end else begin
      version := autosize;
    end;
  end else begin
    version := autosize;
  end;
  if IsTrue(est_binlen <= qr_data_codewords_M[version - 1]) then
  begin
    ecc_level := LEVEL_M;
  end;
  if IsTrue(est_binlen <= qr_data_codewords_Q[version - 1]) then
  begin
    ecc_level := LEVEL_Q;
  end;
  if IsTrue(est_binlen <= qr_data_codewords_H[version - 1]) then
  begin
    ecc_level := LEVEL_H;
  end;
  target_binlen := qr_data_codewords_L[version - 1];
  SetLength(datastream,target_binlen + 1);
  blocks := qr_blocks_L[version - 1];
  case ecc_level of
    LEVEL_M:
      begin
        target_binlen := qr_data_codewords_M[version - 1];
        blocks := qr_blocks_M[version - 1];
      end;
    LEVEL_Q:
      begin
        target_binlen := qr_data_codewords_Q[version - 1];
        blocks := qr_blocks_Q[version - 1];
      end;
    LEVEL_H:
      begin
        target_binlen := qr_data_codewords_H[version - 1];
        blocks := qr_blocks_H[version - 1];
      end;
  end;
  SetLength(fullstream,qr_total_codewords[version - 1] + 1);
  qr_binary (@datastream[0], version, target_binlen, @mode[0], @jisdata[0], length, gs1, est_binlen);
  add_ecc (@fullstream[0], @datastream[0], version, target_binlen, blocks);
  size := qr_sizes[version - 1];
  SetLength(grid,size * size);
  i := 0;
  while i < size do
  begin
    j := 0;
    while j < size do
    begin
      grid[(i * size) + j] := 0;
      Inc (j);
    end;
    Inc (i);
  end;
  setup_grid (@grid[0], size, version);
  populate_grid (@grid[0], size, @fullstream[0], qr_total_codewords[version - 1]);
  bitmask := apply_bitmask (@grid[0], size);
  add_format_info (@grid[0], size, ecc_level, bitmask);
  if IsTrue(version >= 7) then
  begin
    add_version_info (@grid[0], size, version);
  end;
  symbol^.width := size;
  symbol^.rows := size;
  i := 0;
  while i < size do
  begin
    j := 0;
    while j < size do
    begin
      if IsTrue(grid[(i * size) + j] and $01) then
      begin
        set_module (symbol, i, j);
      end;
      Inc (j);
    end;
    symbol^.row_height[i] := 1;
    Inc (i);
  end;
  exit (0);
end;


function micro_qr_intermediate(binary: PChar; jisdata: PInteger; mode: PChar; length: Integer; kanji_used: PInteger; alphanum_used: PInteger; byte_used: PInteger): Integer;
var
  debug: Integer = 0;
  position: Integer = 0;
  i: Integer;
  short_data_block_length: Integer;
  data_block: Char;
  buffer: array [0..2-1] of Char;
  jis: Integer;
  prod: Integer;
  lsb: Integer;
  msb: Integer;
  byte: Integer;
  count: Integer;
//  prod: Integer;
  second: Integer = 0;
  first: Integer = 0;
//  count: Integer;
//  prod: Integer;
  third: Integer = 0;
//  second: Integer = 0;
//  first: Integer = 0;
begin
  {INITCODE} debug := 0;
  {INITCODE} position := 0;
  strcpy (binary, '');
  if IsTrue(debug) then
  begin
    i := 0;
    while i < length do
    begin
      write( format ('%c',[mode[i]]) );
      Inc (i);
    end;
    write( LineEnding );
  end;
  repeat
    if IsTrue(sysutils.strlen (binary) > 128) then
    begin
      exit (ERROR_TOO_LONG);
    end;
    data_block := mode[position];
    short_data_block_length := 0;
    repeat
      Inc (short_data_block_length);
    until NotBoolean ((((short_data_block_length + position) < length)) and ((mode[position + short_data_block_length] = data_block)));
    case data_block of
      'K':
        begin
          concat (binary, 'K');
          kanji_used^ := 1;
          buffer[0] := char(short_data_block_length);
          buffer[1] := #0;
          concat (binary, buffer);
          if IsTrue(debug) then
          begin
            write( format ('Kanji block (length %d)'+LineEnding+chr(9),[short_data_block_length]) );
          end;
          i := 0;
          while i < short_data_block_length do
          begin
            {INITCODE} jis := jisdata[position + i];
            if IsTrue(jis > $9FFF) then
            begin
              jis := jis - $C140;
            end;
            msb := (jis and $FF00) shr 4;
            lsb := (jis and $FF);
            prod := (msb * $C0) + lsb;
            qr_bscan (binary, prod, $1000);
            if IsTrue(debug) then
            begin
              write( format ('0x%.4X ',[prod]) );
            end;
            if IsTrue(sysutils.strlen (binary) > 128) then
            begin
              exit (ERROR_TOO_LONG);
            end;
            Inc (i);
          end;
          if IsTrue(debug) then
          begin
            write( LineEnding );
          end;
        end;
      'B':
        begin
          concat (binary, 'B');
          byte_used^ := 1;
          buffer[0] := char(short_data_block_length);
          buffer[1] := #0;
          concat (binary, buffer);
          if IsTrue(debug) then
          begin
            write( format ('Byte block (length %d)'+LineEnding+chr(9),[short_data_block_length]) );
          end;
          i := 0;
          while i < short_data_block_length do
          begin
            {INITCODE} byte := jisdata[position + i];
            qr_bscan (binary, byte, $80);
            if IsTrue(debug) then
            begin
              write( format ('0x%.4X ',[byte]) );
            end;
            if IsTrue(sysutils.strlen (binary) > 128) then
            begin
              exit (ERROR_TOO_LONG);
            end;
            Inc (i);
          end;
          if IsTrue(debug) then
          begin
            write( LineEnding );
          end;
        end;
      'A':
        begin
          concat (binary, 'A');
          alphanum_used^ := 1;
          buffer[0] := char(short_data_block_length);
          buffer[1] := #0;
          concat (binary, buffer);
          if IsTrue(debug) then
          begin
            write( format ('Alpha block (length %d)'+LineEnding+chr(9),[short_data_block_length]) );
          end;
          i := 0;
          while i < short_data_block_length do
          begin
            {INITCODE} second := 0;
            {INITCODE} first := 0;
            first := posn (RHODIUM, Char (jisdata[position + i]));
            count := 1;
            prod := first;
            if IsTrue(mode[position + i + 1] = 'A') then
            begin
              second := posn (RHODIUM, Char (jisdata[position + i + 1]));
              count := 2;
              prod := (first * 45) + second;
            end;
            qr_bscan (binary, prod, 1 shl (5 * count));
            if IsTrue(debug) then
            begin
              write( format ('0x%.4X ',[prod]) );
            end;
            if IsTrue(sysutils.strlen (binary) > 128) then
            begin
              exit (ERROR_TOO_LONG);
            end;
            i := i + 2;
          end;
          if IsTrue(debug) then
          begin
            write( LineEnding );
          end;
        end;
      'N':
        begin
          concat (binary, 'N');
          buffer[0] := char(short_data_block_length);
          buffer[1] := #0;
          concat (binary, buffer);
          if IsTrue(debug) then
          begin
            write( format ('Number block (length %d)'+LineEnding+chr(9),[short_data_block_length]) );
          end;
          i := 0;
          while i < short_data_block_length do
          begin
            {INITCODE} third := 0;
            {INITCODE} second := 0;
            {INITCODE} first := 0;
            first := posn (NEON, Char (jisdata[position + i]));
            count := 1;
            prod := first;
            if IsTrue(mode[position + i + 1] = 'N') then
            begin
              second := posn (NEON, Char (jisdata[position + i + 1]));
              count := 2;
              prod := (prod * 10) + second;
            end;
            if IsTrue(mode[position + i + 2] = 'N') then
            begin
              third := posn (NEON, Char (jisdata[position + i + 2]));
              count := 3;
              prod := (prod * 10) + third;
            end;
            qr_bscan (binary, prod, 1 shl (3 * count));
            if IsTrue(debug) then
            begin
              write( format ('0x%.4X (%d)',[prod, prod]) );
            end;
            if IsTrue(sysutils.strlen (binary) > 128) then
            begin
              exit (ERROR_TOO_LONG);
            end;
            i := i + 3;
          end;
          if IsTrue(debug) then
          begin
            write( LineEnding );
          end;
        end;
    end;
    position := position + short_data_block_length;
  until not (position < length - 1);
  exit (0);
end;

procedure get_bitlength(count: PInteger; stream: PChar);
var
  i: Integer;
  length: Integer;
begin
  length := sysutils.strlen (stream);
  i := 0;
  while i < 4 do
  begin
    count[i] := 0;
    Inc (i);
  end;
  i := 0;
  repeat
    if IsTrue(((stream[i] = '0')) or ((stream[i] = '1'))) then
    begin
      Inc (count[0]);
      Inc (count[1]);
      Inc (count[2]);
      Inc (count[3]);
      Inc (i);
    end else begin
      case stream[i] of
        'K':
          begin
            count[2] := count[2] + 5;
            count[3] := count[3] + 7;
            i := i + 2;
          end;
        'B':
          begin
            count[2] := count[2] + 6;
            count[3] := count[3] + 8;
            i := i + 2;
          end;
        'A':
          begin
            count[1] := count[1] + 4;
            count[2] := count[2] + 6;
            count[3] := count[3] + 8;
            i := i + 2;
          end;
        'N':
          begin
            count[0] := count[0] + 3;
            count[1] := count[1] + 5;
            count[2] := count[2] + 7;
            count[3] := count[3] + 9;
            i := i + 2;
          end;
      end;
    end;
  until not (i < length);
end;

procedure microqr_expand_binary(binary_stream: PChar; full_stream: PChar; version: Integer);
var
  length: Integer;
  i: Integer;
begin
  length := sysutils.strlen (binary_stream);
  i := 0;
  repeat
    case binary_stream[i] of
      '1':
        begin
          concat (full_stream, '1');
          Inc (i);
        end;
      '0':
        begin
          concat (full_stream, '0');
          Inc (i);
        end;
      'N':
        begin
          case version of
            1: concat (full_stream, '0');
            2: concat (full_stream, '00');
            3: concat (full_stream, '000');
          end;
          qr_bscan (full_stream, integer(binary_stream[i + 1]), 4 shl version);
          i := i + 2;
        end;
      'A':
        begin
          case version of
            1: concat (full_stream, '1');
            2: concat (full_stream, '01');
            3: concat (full_stream, '001');
          end;
          qr_bscan (full_stream, integer(binary_stream[i + 1]), 2 shl version);
          i := i + 2;
        end;
      'B':
        begin
          case version of
            2: concat (full_stream, '10');
            3: concat (full_stream, '010');
          end;
          qr_bscan (full_stream, integer(binary_stream[i + 1]), 2 shl version);
          i := i + 2;
        end;
      'K':
        begin
          case version of
            2: concat (full_stream, '11');
            3: concat (full_stream, '011');
          end;
          qr_bscan (full_stream, integer(binary_stream[i + 1]), 1 shl version);
          i := i + 2;
        end;
    end;
  until not (i < length);
end;

procedure micro_qr_m1(binary_data: PChar);
var
  latch: Integer;
  i: Integer;
  remainder: Integer;
  bits_left: Integer;
  bits_total: Integer;
  ecc_codewords: Integer;
  data_codewords: Integer;
  ecc_blocks: array [0..3-1] of BYTE;
  data_blocks: array [0..4-1] of BYTE;
begin
  bits_total := 20;
  latch := 0;
  bits_left := bits_total - sysutils.strlen (binary_data);
  if IsTrue(bits_left <= 3) then
  begin
    i := 0;
    while i < bits_left do
    begin
      concat (binary_data, '0');
      Inc (i);
    end;
    latch := 1;
  end else begin
    concat (binary_data, '000');
  end;
  if IsTrue(latch = 0) then
  begin
    bits_left := bits_total - sysutils.strlen (binary_data);
    if IsTrue(bits_left <= 4) then
    begin
      i := 0;
      while i < bits_left do
      begin
        concat (binary_data, '0');
        Inc (i);
      end;
      latch := 1;
    end;
  end;
  if IsTrue(latch = 0) then
  begin
    remainder := 8 - (sysutils.strlen (binary_data) mod 8);
    if IsTrue(remainder = 8) then
    begin
      remainder := 0;
    end;
    i := 0;
    while i < remainder do
    begin
      concat (binary_data, '0');
      Inc (i);
    end;
    bits_left := bits_total - sysutils.strlen (binary_data);
    if IsTrue(bits_left > 4) then
    begin
      remainder := (bits_left - 4) div 8;
      i := 0;
      while i < remainder do
      begin
        if IsTrue(i and 1) then begin
          concat (binary_data, '00010001');
        end else begin
          concat (binary_data, '11101100');
        end;
        Inc (i);
      end;
    end;
    concat (binary_data, '0000');
  end;
  data_codewords := 3;
  ecc_codewords := 2;
  i := 0;
  while i < (data_codewords - 1) do
  begin
    data_blocks[i] := 0;
    if IsTrue(binary_data[i * 8] = '1') then
    begin
      data_blocks[i] := data_blocks[i] + $80;
    end;
    if IsTrue(binary_data[(i * 8) + 1] = '1') then
    begin
      data_blocks[i] := data_blocks[i] + $40;
    end;
    if IsTrue(binary_data[(i * 8) + 2] = '1') then
    begin
      data_blocks[i] := data_blocks[i] + $20;
    end;
    if IsTrue(binary_data[(i * 8) + 3] = '1') then
    begin
      data_blocks[i] := data_blocks[i] + $10;
    end;
    if IsTrue(binary_data[(i * 8) + 4] = '1') then
    begin
      data_blocks[i] := data_blocks[i] + $08;
    end;
    if IsTrue(binary_data[(i * 8) + 5] = '1') then
    begin
      data_blocks[i] := data_blocks[i] + $04;
    end;
    if IsTrue(binary_data[(i * 8) + 6] = '1') then
    begin
      data_blocks[i] := data_blocks[i] + $02;
    end;
    if IsTrue(binary_data[(i * 8) + 7] = '1') then
    begin
      data_blocks[i] := data_blocks[i] + $01;
    end;
    Inc (i);
  end;
  data_blocks[2] := 0;
  if IsTrue(binary_data[16] = '1') then
  begin
    data_blocks[2] := data_blocks[2] + $08;
  end;
  if IsTrue(binary_data[17] = '1') then
  begin
    data_blocks[2] := data_blocks[2] + $04;
  end;
  if IsTrue(binary_data[18] = '1') then
  begin
    data_blocks[2] := data_blocks[2] + $02;
  end;
  if IsTrue(binary_data[19] = '1') then
  begin
    data_blocks[2] := data_blocks[2] + $01;
  end;
  rs_init_gf ($11D);
  rs_init_code (ecc_codewords, 0);
  rs_encode (data_codewords, data_blocks, ecc_blocks);
  rs_free;
  i := 0;
  while i < ecc_codewords do
  begin
    qr_bscan (binary_data, ecc_blocks[ecc_codewords - i - 1], $80);
    Inc (i);
  end;
end;

procedure micro_qr_m2(binary_data: PChar; ecc_mode: Integer);
var
  latch: Integer;
  i: Integer;
  remainder: Integer;
  bits_left: Integer;
  bits_total: Integer;
  ecc_codewords: Integer;
  data_codewords: Integer;
  ecc_blocks: array [0..7-1] of BYTE;
  data_blocks: array [0..6-1] of BYTE;
begin
  latch := 0;
  if IsTrue(ecc_mode = LEVEL_L) then
  begin
    bits_total := 40;
  end;
  if IsTrue(ecc_mode = LEVEL_M) then
  begin
    bits_total := 32;
  end;
  bits_left := bits_total - sysutils.strlen (binary_data);
  if IsTrue(bits_left <= 5) then
  begin
    i := 0;
    while i < bits_left do
    begin
      concat (binary_data, '0');
      Inc (i);
    end;
    latch := 1;
  end else begin
    concat (binary_data, '00000');
  end;
  if IsTrue(latch = 0) then
  begin
    remainder := 8 - (sysutils.strlen (binary_data) mod 8);
    if IsTrue(remainder = 8) then
    begin
      remainder := 0;
    end;
    i := 0;
    while i < remainder do
    begin
      concat (binary_data, '0');
      Inc (i);
    end;
    bits_left := bits_total - sysutils.strlen (binary_data);
    remainder := bits_left div 8;
    i := 0;
    while i < remainder do
    begin
      concat (binary_data, iif (i and 1,'00010001','11101100'));
      Inc (i);
    end;
  end;
  if IsTrue(ecc_mode = LEVEL_L) then
  begin
    data_codewords := 5;
    ecc_codewords := 5;
  end;
  if IsTrue(ecc_mode = LEVEL_M) then
  begin
    data_codewords := 4;
    ecc_codewords := 6;
  end;
  i := 0;
  while i < data_codewords do
  begin
    data_blocks[i] := 0;
    if IsTrue(binary_data[i * 8] = '1') then
    begin
      data_blocks[i] := data_blocks[i] + $80;
    end;
    if IsTrue(binary_data[(i * 8) + 1] = '1') then
    begin
      data_blocks[i] := data_blocks[i] + $40;
    end;
    if IsTrue(binary_data[(i * 8) + 2] = '1') then
    begin
      data_blocks[i] := data_blocks[i] + $20;
    end;
    if IsTrue(binary_data[(i * 8) + 3] = '1') then
    begin
      data_blocks[i] := data_blocks[i] + $10;
    end;
    if IsTrue(binary_data[(i * 8) + 4] = '1') then
    begin
      data_blocks[i] := data_blocks[i] + $08;
    end;
    if IsTrue(binary_data[(i * 8) + 5] = '1') then
    begin
      data_blocks[i] := data_blocks[i] + $04;
    end;
    if IsTrue(binary_data[(i * 8) + 6] = '1') then
    begin
      data_blocks[i] := data_blocks[i] + $02;
    end;
    if IsTrue(binary_data[(i * 8) + 7] = '1') then
    begin
      data_blocks[i] := data_blocks[i] + $01;
    end;
    Inc (i);
  end;
  rs_init_gf ($11D);
  rs_init_code (ecc_codewords, 0);
  rs_encode (data_codewords, data_blocks, ecc_blocks);
  rs_free;
  i := 0;
  while i < ecc_codewords do
  begin
    qr_bscan (binary_data, ecc_blocks[ecc_codewords - i - 1], $80);
    Inc (i);
  end;
  exit;
end;

procedure micro_qr_m3(binary_data: PChar; ecc_mode: Integer);
var
  latch: Integer;
  i: Integer;
  remainder: Integer;
  bits_left: Integer;
  bits_total: Integer;
  ecc_codewords: Integer;
  data_codewords: Integer;
  ecc_blocks: array [0..9-1] of BYTE;
  data_blocks: array [0..12-1] of BYTE;
begin
  latch := 0;
  if IsTrue(ecc_mode = LEVEL_L) then
  begin
    bits_total := 84;
  end;
  if IsTrue(ecc_mode = LEVEL_M) then
  begin
    bits_total := 68;
  end;
  bits_left := bits_total - sysutils.strlen (binary_data);
  if IsTrue(bits_left <= 7) then
  begin
    i := 0;
    while i < bits_left do
    begin
      concat (binary_data, '0');
      Inc (i);
    end;
    latch := 1;
  end else begin
    concat (binary_data, '0000000');
  end;
  if IsTrue(latch = 0) then
  begin
    bits_left := bits_total - sysutils.strlen (binary_data);
    if IsTrue(bits_left <= 4) then
    begin
      i := 0;
      while i < bits_left do
      begin
        concat (binary_data, '0');
        Inc (i);
      end;
      latch := 1;
    end;
  end;
  if IsTrue(latch = 0) then
  begin
    remainder := 8 - (sysutils.strlen (binary_data) mod 8);
    if IsTrue(remainder = 8) then
    begin
      remainder := 0;
    end;
    i := 0;
    while i < remainder do
    begin
      concat (binary_data, '0');
      Inc (i);
    end;
    bits_left := bits_total - sysutils.strlen (binary_data);
    if IsTrue(bits_left > 4) then
    begin
      remainder := (bits_left - 4) div 8;
      i := 0;
      while i < remainder do
      begin
        concat (binary_data, iif (i and 1,'00010001','11101100'));
        Inc (i);
      end;
    end;
    concat (binary_data, '0000');
  end;
  if IsTrue(ecc_mode = LEVEL_L) then
  begin
    data_codewords := 11;
    ecc_codewords := 6;
  end;
  if IsTrue(ecc_mode = LEVEL_M) then
  begin
    data_codewords := 9;
    ecc_codewords := 8;
  end;
  i := 0;
  while i < (data_codewords - 1) do
  begin
    data_blocks[i] := 0;
    if IsTrue(binary_data[i * 8] = '1') then
    begin
      data_blocks[i] := data_blocks[i] + $80;
    end;
    if IsTrue(binary_data[(i * 8) + 1] = '1') then
    begin
      data_blocks[i] := data_blocks[i] + $40;
    end;
    if IsTrue(binary_data[(i * 8) + 2] = '1') then
    begin
      data_blocks[i] := data_blocks[i] + $20;
    end;
    if IsTrue(binary_data[(i * 8) + 3] = '1') then
    begin
      data_blocks[i] := data_blocks[i] + $10;
    end;
    if IsTrue(binary_data[(i * 8) + 4] = '1') then
    begin
      data_blocks[i] := data_blocks[i] + $08;
    end;
    if IsTrue(binary_data[(i * 8) + 5] = '1') then
    begin
      data_blocks[i] := data_blocks[i] + $04;
    end;
    if IsTrue(binary_data[(i * 8) + 6] = '1') then
    begin
      data_blocks[i] := data_blocks[i] + $02;
    end;
    if IsTrue(binary_data[(i * 8) + 7] = '1') then
    begin
      data_blocks[i] := data_blocks[i] + $01;
    end;
    Inc (i);
  end;
  if IsTrue(ecc_mode = LEVEL_L) then
  begin
    data_blocks[11] := 0;
    if IsTrue(binary_data[80] = '1') then
    begin
      data_blocks[2] := data_blocks[2] + $08;
    end;
    if IsTrue(binary_data[81] = '1') then
    begin
      data_blocks[2] := data_blocks[2] + $04;
    end;
    if IsTrue(binary_data[82] = '1') then
    begin
      data_blocks[2] := data_blocks[2] + $02;
    end;
    if IsTrue(binary_data[83] = '1') then
    begin
      data_blocks[2] := data_blocks[2] + $01;
    end;
  end;
  if IsTrue(ecc_mode = LEVEL_M) then
  begin
    data_blocks[9] := 0;
    if IsTrue(binary_data[64] = '1') then
    begin
      data_blocks[2] := data_blocks[2] + $08;
    end;
    if IsTrue(binary_data[65] = '1') then
    begin
      data_blocks[2] := data_blocks[2] + $04;
    end;
    if IsTrue(binary_data[66] = '1') then
    begin
      data_blocks[2] := data_blocks[2] + $02;
    end;
    if IsTrue(binary_data[67] = '1') then
    begin
      data_blocks[2] := data_blocks[2] + $01;
    end;
  end;
  rs_init_gf ($11D);
  rs_init_code (ecc_codewords, 0);
  rs_encode (data_codewords, data_blocks, ecc_blocks);
  rs_free;
  i := 0;
  while i < ecc_codewords do
  begin
    qr_bscan (binary_data, ecc_blocks[ecc_codewords - i - 1], $80);
    Inc (i);
  end;
  exit;
end;

procedure micro_qr_m4(binary_data: PChar; ecc_mode: Integer);
var
  latch: Integer;
  i: Integer;
  remainder: Integer;
  bits_left: Integer;
  bits_total: Integer;
  ecc_codewords: Integer;
  data_codewords: Integer;
  ecc_blocks: array [0..15-1] of BYTE;
  data_blocks: array [0..17-1] of BYTE;
begin
  latch := 0;
  if IsTrue(ecc_mode = LEVEL_L) then
  begin
    bits_total := 128;
  end;
  if IsTrue(ecc_mode = LEVEL_M) then
  begin
    bits_total := 112;
  end;
  if IsTrue(ecc_mode = LEVEL_Q) then
  begin
    bits_total := 80;
  end;
  bits_left := bits_total - sysutils.strlen (binary_data);
  if IsTrue(bits_left <= 9) then
  begin
    i := 0;
    while i < bits_left do
    begin
      concat (binary_data, '0');
      Inc (i);
    end;
    latch := 1;
  end else begin
    concat (binary_data, '000000000');
  end;
  if IsTrue(latch = 0) then
  begin
    remainder := 8 - (sysutils.strlen (binary_data) mod 8);
    if IsTrue(remainder = 8) then
    begin
      remainder := 0;
    end;
    i := 0;
    while i < remainder do
    begin
      concat (binary_data, '0');
      Inc (i);
    end;
    bits_left := bits_total - sysutils.strlen (binary_data);
    remainder := bits_left div 8;
    i := 0;
    while i < remainder do
    begin
      concat (binary_data, iif (i and 1,'00010001','11101100'));
      Inc (i);
    end;
  end;
  if IsTrue(ecc_mode = LEVEL_L) then
  begin
    data_codewords := 16;
    ecc_codewords := 8;
  end;
  if IsTrue(ecc_mode = LEVEL_M) then
  begin
    data_codewords := 14;
    ecc_codewords := 10;
  end;
  if IsTrue(ecc_mode = LEVEL_Q) then
  begin
    data_codewords := 10;
    ecc_codewords := 14;
  end;
  i := 0;
  while i < data_codewords do
  begin
    data_blocks[i] := 0;
    if IsTrue(binary_data[i * 8] = '1') then
    begin
      data_blocks[i] := data_blocks[i] + $80;
    end;
    if IsTrue(binary_data[(i * 8) + 1] = '1') then
    begin
      data_blocks[i] := data_blocks[i] + $40;
    end;
    if IsTrue(binary_data[(i * 8) + 2] = '1') then
    begin
      data_blocks[i] := data_blocks[i] + $20;
    end;
    if IsTrue(binary_data[(i * 8) + 3] = '1') then
    begin
      data_blocks[i] := data_blocks[i] + $10;
    end;
    if IsTrue(binary_data[(i * 8) + 4] = '1') then
    begin
      data_blocks[i] := data_blocks[i] + $08;
    end;
    if IsTrue(binary_data[(i * 8) + 5] = '1') then
    begin
      data_blocks[i] := data_blocks[i] + $04;
    end;
    if IsTrue(binary_data[(i * 8) + 6] = '1') then
    begin
      data_blocks[i] := data_blocks[i] + $02;
    end;
    if IsTrue(binary_data[(i * 8) + 7] = '1') then
    begin
      data_blocks[i] := data_blocks[i] + $01;
    end;
    Inc (i);
  end;
  rs_init_gf ($11D);
  rs_init_code (ecc_codewords, 0);
  rs_encode (data_codewords, data_blocks, ecc_blocks);
  rs_free;
  i := 0;
  while i < ecc_codewords do
  begin
    qr_bscan (binary_data, ecc_blocks[ecc_codewords - i - 1], $80);
    Inc (i);
  end;
end;

procedure micro_setup_grid(grid: PBYTE; size: Integer);
var
  toggle: Integer = 1;
  i: Integer = 1;
begin
  {INITCODE} toggle := 1;
  {INITCODE} i := 1;
  i := 0;
  while i < size do
  begin
    if IsTrue(toggle = 1) then
    begin
      grid[i] := $21;
      grid[(i * size)] := $21;
      toggle := 0;
    end else begin
      grid[i] := $20;
      grid[(i * size)] := $20;
      toggle := 1;
    end;
    Inc (i);
  end;
  place_finder (grid, size, 0, 0);
  i := 0;
  while i < 7 do
  begin
    grid[(7 * size) + i] := $10;
    grid[(i * size) + 7] := $10;
    Inc (i);
  end;
  grid[(7 * size) + 7] := $10;
  i := 0;
  while i < 8 do
  begin
    grid[(8 * size) + i] := grid[(8 * size) + i] + $20;
    grid[(i * size) + 8] := grid[(i * size) + 8] + $20;
    Inc (i);
  end;
  grid[(8 * size) + 8] := grid[(8 * size) + 8] + 20;
end;

procedure micro_populate_grid(grid: PBYTE; size: Integer; full_stream: PChar);
var
  direction: Integer = 1;
  row: Integer = 0;
  y: Integer;
  x: Integer;
  n: Integer;
  i: Integer;
begin
  {INITCODE} direction := 1;
  {INITCODE} row := 0;
  n := sysutils.strlen (full_stream);
  y := size - 1;
  i := 0;
  repeat
    x := (size - 2) - (row * 2);
    if not IsTrue((grid[(y * size) + (x + 1)] and $F0)) then
    begin
      if IsTrue(full_stream[i] = '1') then
      begin
        grid[(y * size) + (x + 1)] := $01;
      end else begin
        grid[(y * size) + (x + 1)] := $00;
      end;
      Inc (i);
    end;
    if IsTrue(i < n) then
    begin
      if not IsTrue((grid[(y * size) + x] and $F0)) then
      begin
        if IsTrue(full_stream[i] = '1') then
        begin
          grid[(y * size) + x] := $01;
        end else begin
          grid[(y * size) + x] := $00;
        end;
        Inc (i);
      end;
    end;
    if IsTrue(direction) then
    begin
      Dec (y);
    end else begin
      Inc (y);
    end;
    if IsTrue(y = 0) then
    begin
      Inc (row);
      y := 1;
      direction := 0;
    end;
    if IsTrue(y = size) then
    begin
      Inc (row);
      y := size - 1;
      direction := 1;
    end;
  until not (i < n);
end;

function micro_evaluate(grid: PBYTE; size: Integer; pattern: Integer): Integer;
var
  retval: Integer;
  filter: Integer = 0;
  i: Integer = 0;
  sum2: Integer = 0;
  sum1: Integer = 0;
begin
  {INITCODE} filter := 0;
  {INITCODE} i := 0;
  {INITCODE} sum2 := 0;
  {INITCODE} sum1 := 0;
  case pattern of
    0: filter := $01;
    1: filter := $02;
    2: filter := $04;
    3: filter := $08;
  end;
  sum1 := 0;
  sum2 := 0;
  i := 1;
  while i < size do
  begin
    if IsTrue(grid[(i * size) + size - 1] and filter) then
    begin
      Inc (sum1);
    end;
    if IsTrue(grid[((size - 1) * size) + i] and filter) then
    begin
      Inc (sum2);
    end;
    Inc (i);
  end;
  if IsTrue(sum1 <= sum2) then
  begin
    retval := (sum1 * 16) + sum2;
  end else begin
    retval := (sum2 * 16) + sum1;
  end;
  exit (retval);
end;

function micro_apply_bitmask(grid: PBYTE; size: Integer): Integer;
var
  y: Integer;
  x: Integer;
  p: BYTE;
  value: array [0..8-1] of Integer;
  pattern: Integer;
  best_pattern: Integer;
  best_val: Integer;
  bit: Integer;
  mask: array of BYTE;
  eval: array of BYTE;
begin
  SetLength(mask,size * size);
  SetLength(eval,size * size);
  x := 0;
  while x < size do
  begin
    y := 0;
    while y < size do
    begin
      mask[(y * size) + x] := $00;
      if not IsTrue((grid[(y * size) + x] and $F0)) then
      begin
        if IsTrue((y and 1) = 0) then
        begin
          mask[(y * size) + x] := mask[(y * size) + x] + $01;
        end;
        if IsTrue((((y div 2) + (x div 3)) and 1) = 0) then
        begin
          mask[(y * size) + x] := mask[(y * size) + x] + $02;
        end;
        if IsTrue(((((y * x) and 1) + ((y * x) mod 3)) and 1) = 0) then
        begin
          mask[(y * size) + x] := mask[(y * size) + x] + $04;
        end;
        if IsTrue(((((y + x) and 1) + ((y * x) mod 3)) and 1) = 0) then
        begin
          mask[(y * size) + x] := mask[(y * size) + x] + $08;
        end;
      end;
      Inc (y);
    end;
    Inc (x);
  end;
  x := 0;
  while x < size do
  begin
    y := 0;
    while y < size do
    begin
      if IsTrue(grid[(y * size) + x] and $01) then
      begin
        p := $FF;
      end else begin
        p := $00;
      end;
      eval[(y * size) + x] := mask[(y * size) + x] xor p;
      Inc (y);
    end;
    Inc (x);
  end;
  pattern := 0;
  while pattern < 8 do
  begin
    value[pattern] := micro_evaluate (@eval[0], size, pattern);
    Inc (pattern);
  end;
  best_pattern := 0;
  best_val := value[0];
  pattern := 1;
  while pattern < 4 do
  begin
    if IsTrue(value[pattern] > best_val) then
    begin
      best_pattern := pattern;
      best_val := value[pattern];
    end;
    Inc (pattern);
  end;
  x := 0;
  while x < size do
  begin
    y := 0;
    while y < size do
    begin
      bit := 0;
      case best_pattern of
        0:
          begin
            if IsTrue(mask[(y * size) + x] and $01) then
            begin
              bit := 1;
            end;
          end;
        1:
          begin
            if IsTrue(mask[(y * size) + x] and $02) then
            begin
              bit := 1;
            end;
          end;
        2:
          begin
            if IsTrue(mask[(y * size) + x] and $04) then
            begin
              bit := 1;
            end;
          end;
        3:
          begin
            if IsTrue(mask[(y * size) + x] and $08) then
            begin
              bit := 1;
            end;
          end;
      end;
      if IsTrue(bit = 1) then
      begin
        if IsTrue(grid[(y * size) + x] and $01) then
        begin
          grid[(y * size) + x] := $00;
        end else begin
          grid[(y * size) + x] := $01;
        end;
      end;
      Inc (y);
    end;
    Inc (x);
  end;
  exit (best_pattern);
end;

function microqr(symbol: PointerTo_zint_symbol; source: PBYTE; length: Integer): Integer;
var
  size: Integer;
  glyph: Integer;
  j: Integer;
  i: Integer;
  binary_stream: array [0..200-1] of Char;
  full_stream: array [0..200-1] of Char;
  utfdata: array [0..40-1] of Integer;
  jisdata: array [0..40-1] of Integer;
  mode: array [0..40-1] of Char;
  byte_used: Integer = 0;
  alphanum_used: Integer = 0;
  kanji_used: Integer = 0;
  error_number: Integer = 0;
  version_valid: array [0..4-1] of Integer;
  binary_count: array [0..4-1] of Integer;
  version: Integer;
  autoversion: Integer;
  ecc_level: Integer;
  format_full: Integer;
  format: Integer;
  bitmask: Integer;
  a_count: Integer;
  n_count: Integer;
  grid: array of BYTE;
begin
  {INITCODE} byte_used := 0;
  {INITCODE} alphanum_used := 0;
  {INITCODE} kanji_used := 0;
  {INITCODE} error_number := 0;
  if IsTrue(length > 35) then
  begin
    strcpy (symbol^.errtxt, 'Input data too long');
    exit (ERROR_TOO_LONG);
  end;
  i := 0;
  while i < 4 do
  begin
    version_valid[i] := 1;
    Inc (i);
  end;
  case symbol^.input_mode of
    DATA_MODE:
      begin
        i := 0;
        while i < length do
        begin
          jisdata[i] := Integer (source[i]);
          Inc (i);
        end;
      end;
    otherwise
      begin
        error_number := utf8toutf16 (symbol, source, utfdata, @length);
        if IsTrue(error_number <> 0) then
        begin
          exit (error_number);
        end;
        i := 0;
        while i < length do
        begin
          if IsTrue(utfdata[i] <= $FF) then
          begin
            jisdata[i] := utfdata[i];
          end else begin
            j := 0;
            glyph := 0;
            repeat
              if IsTrue(sjis_lookup[j * 2] = utfdata[i]) then
              begin
                glyph := sjis_lookup[(j * 2) + 1];
              end;
              Inc (j);
            until not (((j < 6843)) and ((glyph = 0)));
            if IsTrue(glyph = 0) then
            begin
              strcpy (symbol^.errtxt, 'Invalid character in input data');
              exit (ERROR_INVALID_DATA);
            end;
            jisdata[i] := glyph;
          end;
          Inc (i);
        end;
      end;
  end;
  define_mode (mode, jisdata, length, false);
  n_count := 0;
  a_count := 0;
  i := 0;
  while i < length do
  begin
    if IsTrue(((jisdata[i] >= integer('0'))) and ((jisdata[i] <= integer('9')))) then
    begin
      Inc (n_count);
    end;
    if IsTrue(in_alpha (jisdata[i])) then
    begin
      Inc (a_count);
    end;
    Inc (i);
  end;
  if IsTrue(a_count = length) then
  begin
    i := 0;
    while i < length do
    begin
      mode[i] := 'A';
      Inc (i);
    end;
  end;
  if IsTrue(n_count = length) then
  begin
    i := 0;
    while i < length do
    begin
      mode[i] := 'N';
      Inc (i);
    end;
  end;
  error_number := micro_qr_intermediate (binary_stream, jisdata, mode, length, @kanji_used, @alphanum_used, @byte_used);
  if IsTrue(error_number <> 0) then
  begin
    strcpy (symbol^.errtxt, 'Input data too long');
    exit (error_number);
  end;
  get_bitlength (binary_count, binary_stream);
  if IsTrue(byte_used) then
  begin
    version_valid[0] := 0;
    version_valid[1] := 0;
  end;
  if IsTrue(alphanum_used) then
  begin
    version_valid[0] := 0;
  end;
  if IsTrue(kanji_used) then
  begin
    version_valid[0] := 0;
    version_valid[1] := 0;
  end;
  if IsTrue(binary_count[0] > 20) then
  begin
    version_valid[0] := 0;
  end;
  if IsTrue(binary_count[1] > 40) then
  begin
    version_valid[1] := 0;
  end;
  if IsTrue(binary_count[2] > 84) then
  begin
    version_valid[2] := 0;
  end;
  if IsTrue(binary_count[3] > 128) then
  begin
    strcpy (symbol^.errtxt, 'Input data too long');
    exit (ERROR_TOO_LONG);
  end;
  ecc_level := LEVEL_L;
  if IsTrue(((symbol^.option_1 >= 1)) and ((symbol^.option_2 <= 4))) then
  begin
    ecc_level := symbol^.option_1;
  end;
  if IsTrue(ecc_level = LEVEL_H) then
  begin
    strcpy (symbol^.errtxt, 'Error correction level H not available');
    exit (ERROR_INVALID_OPTION);
  end;
  if IsTrue(ecc_level = LEVEL_Q) then
  begin
    version_valid[0] := 0;
    version_valid[1] := 0;
    version_valid[2] := 0;
    if IsTrue(binary_count[3] > 80) then
    begin
      strcpy (symbol^.errtxt, 'Input data too long');
      exit (ERROR_TOO_LONG);
    end;
  end;
  if IsTrue(ecc_level = LEVEL_M) then
  begin
    version_valid[0] := 0;
    if IsTrue(binary_count[1] > 32) then
    begin
      version_valid[1] := 0;
    end;
    if IsTrue(binary_count[2] > 68) then
    begin
      version_valid[2] := 0;
    end;
    if IsTrue(binary_count[3] > 112) then
    begin
      strcpy (symbol^.errtxt, 'Input data too long');
      exit (ERROR_TOO_LONG);
    end;
  end;
  autoversion := 3;
  if IsTrue(version_valid[2]) then
  begin
    autoversion := 2;
  end;
  if IsTrue(version_valid[1]) then
  begin
    autoversion := 1;
  end;
  if IsTrue(version_valid[0]) then
  begin
    autoversion := 0;
  end;
  version := autoversion;
  if IsTrue(((symbol^.option_2 >= 1)) and ((symbol^.option_2 <= 4))) then
  begin
    if IsTrue(symbol^.option_2 >= autoversion) then
    begin
      version := symbol^.option_2;
    end;
  end;
  if IsTrue(version = 3) then
  begin
    if IsTrue(binary_count[3] <= 112) then
    begin
      ecc_level := LEVEL_M;
    end;
    if IsTrue(binary_count[3] <= 80) then
    begin
      ecc_level := LEVEL_Q;
    end;
  end;
  if IsTrue(version = 2) then
  begin
    if IsTrue(binary_count[2] <= 68) then
    begin
      ecc_level := LEVEL_M;
    end;
  end;
  if IsTrue(version = 1) then
  begin
    if IsTrue(binary_count[1] <= 32) then
    begin
      ecc_level := LEVEL_M;
    end;
  end;
  full_stream[0]:=#0;
  strcpy (full_stream, '');
  microqr_expand_binary (binary_stream, full_stream, version);
  case version of
    0: micro_qr_m1 (full_stream);
    1: micro_qr_m2 (full_stream, ecc_level);
    2: micro_qr_m3 (full_stream, ecc_level);
    3: micro_qr_m4 (full_stream, ecc_level);
  end;
  size := micro_qr_sizes[version];
  SetLength(grid,size * size);
  i := 0;
  while i < size do
  begin
    j := 0;
    while j < size do
    begin
      grid[(i * size) + j] := 0;
      Inc (j);
    end;
    Inc (i);
  end;
  micro_setup_grid (@grid[0], size);
  micro_populate_grid (@grid[0], size, full_stream);
  bitmask := micro_apply_bitmask (@grid[0], size);
  format := 0;
  case version of
    1:
      begin
        case ecc_level of
          1: format := 1;
          2: format := 2;
        end;
      end;
    2:
      begin
        case ecc_level of
          1: format := 3;
          2: format := 4;
        end;
      end;
    3:
      begin
        case ecc_level of
          1: format := 5;
          2: format := 6;
          3: format := 7;
        end;
      end;
  end;
  format_full := qr_annex_c1[(format shl 2) + bitmask];
  if IsTrue(format_full and $4000) then
  begin
    grid[(8 * size) + 1] := grid[(8 * size) + 1] + $01;
  end;
  if IsTrue(format_full and $2000) then
  begin
    grid[(8 * size) + 2] := grid[(8 * size) + 2] + $01;
  end;
  if IsTrue(format_full and $1000) then
  begin
    grid[(8 * size) + 3] := grid[(8 * size) + 3] + $01;
  end;
  if IsTrue(format_full and $800) then
  begin
    grid[(8 * size) + 4] := grid[(8 * size) + 4] + $01;
  end;
  if IsTrue(format_full and $400) then
  begin
    grid[(8 * size) + 5] := grid[(8 * size) + 5] + $01;
  end;
  if IsTrue(format_full and $200) then
  begin
    grid[(8 * size) + 6] := grid[(8 * size) + 6] + $01;
  end;
  if IsTrue(format_full and $100) then
  begin
    grid[(8 * size) + 7] := grid[(8 * size) + 7] + $01;
  end;
  if IsTrue(format_full and $80) then
  begin
    grid[(8 * size) + 8] := grid[(8 * size) + 8] + $01;
  end;
  if IsTrue(format_full and $40) then
  begin
    grid[(7 * size) + 8] := grid[(7 * size) + 8] + $01;
  end;
  if IsTrue(format_full and $20) then
  begin
    grid[(6 * size) + 8] := grid[(6 * size) + 8] + $01;
  end;
  if IsTrue(format_full and $10) then
  begin
    grid[(5 * size) + 8] := grid[(5 * size) + 8] + $01;
  end;
  if IsTrue(format_full and $08) then
  begin
    grid[(4 * size) + 8] := grid[(4 * size) + 8] + $01;
  end;
  if IsTrue(format_full and $04) then
  begin
    grid[(3 * size) + 8] := grid[(3 * size) + 8] + $01;
  end;
  if IsTrue(format_full and $02) then
  begin
    grid[(2 * size) + 8] := grid[(2 * size) + 8] + $01;
  end;
  if IsTrue(format_full and $01) then
  begin
    grid[(1 * size) + 8] := grid[(1 * size) + 8] + $01;
  end;
  symbol^.width := size;
  symbol^.rows := size;
  i := 0;
  while i < size do
  begin
    j := 0;
    while j < size do
    begin
      if IsTrue(grid[(i * size) + j] and $01) then
      begin
        set_module (symbol, i, j);
      end;
      Inc (j);
    end;
    symbol^.row_height[i] := 1;
    Inc (i);
  end;
  exit (0);
end;

end.
