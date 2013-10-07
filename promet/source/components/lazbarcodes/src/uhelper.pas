unit uhelper;

{$mode objfpc}{$H+}

interface

uses sysutils,zint;

const RHODIUM = '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ $%*+-./:';
const NEON =    '0123456789';

function IsTrue(aBoolean: Boolean): Boolean;
function IsTrue(aInteger: Integer): Boolean;
function iif(const aBoolean: integer; const aCh1,aCh2: pchar): pchar;
function iif(const aBoolean: integer; const aCh1,aCh2: char): char;
function iif(const aBoolean: Boolean; const aV1,aV2: Word): word;
procedure concat(const aText: pchar; const aChar: char);
procedure concat(const aText: pchar; const aText2: pchar);
procedure concat(var aText: array of char; const aText2: pchar);
procedure strcpy(const aText: pchar; const aText2: pchar);
procedure strcpy(var aText: array of char; const aText2: pchar); overload;
function posn(const aText: pchar; const aChar: Char): integer;
function strlen(const aText: array of char): integer;
function is_sane(test_string: PChar; source: PBYTE; length: Integer): Integer;
function utf8toutf16(symbol: PointerTo_zint_symbol; source: PBYTE; vals: PInteger; length: PInteger): Integer;
procedure set_module(symbol: PointerTo_zint_symbol; y_coord: Integer; x_coord: Integer);
procedure unset_module(symbol: PointerTo_zint_symbol; y_coord: Integer; x_coord: Integer);
function module_is_set(symbol: PointerTo_zint_symbol; y_coord: Integer; x_coord: Integer): Boolean;
function PostInc(var v: integer): integer;
function PostDec(var v: integer): integer;
function is_extendable(symbology: Integer): Boolean;
function is_stackable(symbology: Integer): Boolean;
function NotBoolean(const aValue: integer): Boolean;
function NotBoolean(const aValue: Boolean): Boolean;
function latin1_process(symbol: PointerTo_zint_symbol; source: PBYTE; preprocessed: PBYTE; length: PInteger): Integer;
function ctoi(c: char): integer;
function ctoi(c: BYTE): integer;
function BooleanNot(const aValue: integer): Boolean;
procedure memset(const p: Pointer; const aValue: BYTE; const aSize: integer);
function istwodigits(source: PBYTE; position: Integer): Boolean;

implementation  

function istwodigits(source: PBYTE; position: Integer): Boolean;
begin
  if (Char(source[position]) in ['0'..'9']) and (Char(source[position+1]) in ['0'..'9']) then begin
    Result:=true;
  end else begin
    Result:=false;
  end;
end;

function PostInc(var v: integer): integer;
begin
  Result:=v;
  inc(v);
end;

function PostDec(var v: integer): integer;
begin
  Result:=v;
  Dec(v);
end;

function IsTrue(aBoolean: Boolean): Boolean;
begin
  Result:=aBoolean;
end;

function IsTrue(aInteger: Integer): Boolean;
begin
  if aInteger=0 then Result:=false else Result:=true;
end;

function iif(const aBoolean: integer; const aCh1, aCh2: pchar): pchar;
begin
  if aBoolean=0 then begin
    Result:=aCh2;
  end else begin
    Result:=aCh1;
  end;
end;

function iif(const aBoolean: integer; const aCh1,aCh2: char): char;
begin
  if aBoolean=0 then begin
    Result:=aCh2;
  end else begin
    Result:=aCh1;
  end;
end;

function iif(const aBoolean: Boolean; const aV1,aV2: Word): word;
begin
  if aBoolean then Result:=aV1 else Result:=aV2;
end;

procedure concat(const aText: pchar; const aChar: char);
var
  i: integer;
begin
  i:=sysutils.strlen(aText);
  aText[i]:=aChar;
  inc(i);
  aText[i]:=#0;
end;

procedure concat(const aText: pchar; const aText2: pchar);
var
  i: integer;
begin
  i:=sysutils.strlen(aText);
  move(aText2^,aText[i],sysutils.strlen(aText2)+1);
end;

procedure concat(var aText: array of char; const aText2: pchar);
begin
  concat(pchar(@aText[0]),aText2);
end;

procedure strcpy(const aText: pchar; const aText2: pchar);
begin
  move(aText2^,aText^,sysutils.strlen(aText2)+1);
end;

procedure strcpy(var aText: array of char; const aText2: pchar);
begin
  move(aText2^,aText[0],sysutils.strlen(aText2)+1);
end;

function posn(const aText: pchar; const aChar: Char): integer;
var
  p: Pchar;
  c: integer;
begin
  p:=aText;
  c:=0;
  while p^<>#0 do begin
    if p^<>aChar then begin
      inc(c);
      inc(p);
    end else begin
      Exit(c);
    end;
  end;
  Result:=0;
end;

function strlen(const aText: array of char): integer;
begin
  Result:=sysutils.strlen(pchar(@aText[0]));
end;

function is_sane(test_string: PChar; source: PBYTE; length: Integer): Integer;
var
  latch: Boolean;
  j: Cardinal;
  i: Integer;
  lt: Cardinal;
begin
  {INITCODE} lt := sysutils.strlen (test_string);
  i := 0;
  while i < length do
  begin
    latch := FALSE;
    j := 0;
    while j < lt do
    begin
      if Boolean(source[i] = BYTE(test_string[j])) then
      begin
        latch := TRUE;
        break;
      end;
      Inc (j);
    end;
    if Boolean(NotBoolean (latch)) then
    begin
      exit (ERROR_INVALID_DATA);
    end;
    Inc (i);
  end;
  exit (0);
end;

function utf8toutf16(symbol: PointerTo_zint_symbol; source: PBYTE; vals: PInteger; length: PInteger): Integer;
var
  error_number: Integer;
  jpos: Integer;
  bpos: Integer;
  next: Integer;
begin
  bpos := 0;
  jpos := 0;
  error_number := 0;
  next := 0;
  repeat
    if source[bpos] <= $7F then
    begin
      vals[jpos] := source[bpos];
      next := bpos + 1;
      Inc (jpos);
    end else begin
      if (source[bpos] >= $80) and (source[bpos] <= $BF) then
      begin
        strcpy (symbol^.errtxt, 'Corrupt Unicode data');
        exit (ERROR_INVALID_DATA);
      end;
      if (source[bpos] >= $C0) and (source[bpos] <= $C1) then
      begin
        strcpy (symbol^.errtxt, 'Overlong encoding not supported');
        exit (ERROR_INVALID_DATA);
      end;
      if (source[bpos] >= $C2) and (source[bpos] <= $DF) then
      begin
        vals[jpos] := ((source[bpos] and $1F) shl 6) + (source[bpos + 1] and $3F);
        next := bpos + 2;
        Inc (jpos);
      end else begin
        if (source[bpos] >= $E0) and (source[bpos] <= $EF) then
        begin
          vals[jpos] := ((source[bpos] and $0F) shl 12) + ((source[bpos + 1] and $3F) shl 6) + (source[bpos + 2] and $3F);
          next := bpos + 3;
          Inc (jpos);
        end else begin
          if source[bpos] >= $F0 then
          begin
            strcpy (symbol^.errtxt, 'Unicode sequences of more than 3 bytes not supported');
            exit (ERROR_INVALID_DATA);
          end;
        end;
      end;
    end;
    bpos := next;
  until not (bpos < length^);
  length^ := jpos;
  exit (error_number);
end;

function module_is_set(symbol: PointerTo_zint_symbol; y_coord: Integer; x_coord: Integer): Boolean;
var
  x_sub: Integer;
  x_char: Integer;
begin
  x_char := x_coord div 7;
  x_sub := x_coord mod 7;
  {$PUSH}{$R-}
  Result:=((symbol^.encoded_data[y_coord][x_coord div 7] shr (x_coord mod 7)) and 1)<>0;
  {$POP}
  exit;

  Result:=false;
  case x_sub of
    0: if Boolean((symbol^.encoded_data[y_coord][x_char] and $01) <> 0) then
       begin
         result := true;
       end;
    1: if Boolean((symbol^.encoded_data[y_coord][x_char] and $02) <> 0) then
       begin
         result := true;
       end;
    2: if Boolean((symbol^.encoded_data[y_coord][x_char] and $04) <> 0) then
       begin
         result := true;
       end;
    3: if Boolean((symbol^.encoded_data[y_coord][x_char] and $08) <> 0) then
       begin
         result := true;
       end;
    4: if Boolean((symbol^.encoded_data[y_coord][x_char] and $10) <> 0) then
       begin
         result := true;
       end;
    5: if Boolean((symbol^.encoded_data[y_coord][x_char] and $20) <> 0) then
       begin
         result := true;
       end;
    6: if Boolean((symbol^.encoded_data[y_coord][x_char] and $40) <> 0) then
       begin
         result := true;
       end;
    else
       result:=false;
  end;
end;

procedure set_module(symbol: PointerTo_zint_symbol; y_coord: Integer; x_coord: Integer);
var
  x_sub: Integer;
  x_char: Integer;
begin
  x_char := x_coord div 7;
  x_sub := x_coord mod 7;
  case x_sub of
    0: symbol^.encoded_data[y_coord][x_char] := symbol^.encoded_data[y_coord][x_char] + $01;
    1: symbol^.encoded_data[y_coord][x_char] := symbol^.encoded_data[y_coord][x_char] + $02;
    2: symbol^.encoded_data[y_coord][x_char] := symbol^.encoded_data[y_coord][x_char] + $04;
    3: symbol^.encoded_data[y_coord][x_char] := symbol^.encoded_data[y_coord][x_char] + $08;
    4: symbol^.encoded_data[y_coord][x_char] := symbol^.encoded_data[y_coord][x_char] + $10;
    5: symbol^.encoded_data[y_coord][x_char] := symbol^.encoded_data[y_coord][x_char] + $20;
    6: symbol^.encoded_data[y_coord][x_char] := symbol^.encoded_data[y_coord][x_char] + $40;
  end;
end;

procedure unset_module(symbol: PointerTo_zint_symbol; y_coord: Integer; x_coord: Integer);
var
  x_sub: Integer;
  x_char: Integer;
begin
  x_char := x_coord div 7;
  x_sub := x_coord mod 7;
  case x_sub of
    0: symbol^.encoded_data[y_coord][x_char] := symbol^.encoded_data[y_coord][x_char] - $01;
    1: symbol^.encoded_data[y_coord][x_char] := symbol^.encoded_data[y_coord][x_char] - $02;
    2: symbol^.encoded_data[y_coord][x_char] := symbol^.encoded_data[y_coord][x_char] - $04;
    3: symbol^.encoded_data[y_coord][x_char] := symbol^.encoded_data[y_coord][x_char] - $08;
    4: symbol^.encoded_data[y_coord][x_char] := symbol^.encoded_data[y_coord][x_char] - $10;
    5: symbol^.encoded_data[y_coord][x_char] := symbol^.encoded_data[y_coord][x_char] - $20;
    6: symbol^.encoded_data[y_coord][x_char] := symbol^.encoded_data[y_coord][x_char] - $40;
  end;
end;

procedure to_upper(a: PBYTE);
begin
  while a^<>0 do begin
    if char(a^) in ['a'..'z'] then begin
      a^:=BYTE(upCase(char(a^)));
    end;
    inc(a);
  end;
end;

function ctoi(c: char): integer;
begin
  if c in ['0'..'9'] then begin
    Result:=BYTE(c)-48;
  end else begin
    Result:=-1;
  end;
end;

function ctoi(c: BYTE): integer;
begin
  Result:=ctoi(char(c));
end;

function BooleanNot(const aValue: integer): Boolean;
begin
  Result:=NotBoolean(aValue);
end;

procedure memset(const p: Pointer; const aValue: BYTE; const aSize: integer);
begin
  FillByte(PBYTE(p)^,aSize,aValue);
end;

function is_extendable(symbology: Integer): Boolean;
begin
  if symbology = BARCODE_EANX then
  begin
    exit (true);
  end;
  if symbology = BARCODE_UPCA then
  begin
    exit (true);
  end;
  if symbology = BARCODE_UPCE then
  begin
    exit (true);
  end;
  if symbology = BARCODE_ISBNX then
  begin
    exit (true);
  end;
  if symbology = BARCODE_UPCA_CC then
  begin
    exit (true);
  end;
  if symbology = BARCODE_UPCE_CC then
  begin
    exit (true);
  end;
  if symbology = BARCODE_EANX_CC then
  begin
    exit (true);
  end;
  exit (false);
end;

function is_stackable(symbology: Integer): Boolean;
begin
  if symbology < BARCODE_PDF417 then
  begin
    exit (true);
  end;
  if symbology = BARCODE_CODE128B then
  begin
    exit (true);
  end;
  if symbology = BARCODE_ISBNX then
  begin
    exit (true);
  end;
  if symbology = BARCODE_EAN14 then
  begin
    exit (true);
  end;
  if symbology = BARCODE_NVE18 then
  begin
    exit (true);
  end;
  if symbology = BARCODE_KOREAPOST then
  begin
    exit (true);
  end;
  if symbology = BARCODE_PLESSEY then
  begin
    exit (true);
  end;
  if symbology = BARCODE_TELEPEN_NUM then
  begin
    exit (true);
  end;
  if symbology = BARCODE_ITF14 then
  begin
    exit (true);
  end;
  if symbology = BARCODE_CODE32 then
  begin
    exit (true);
  end;
  exit (false);
end;

function NotBoolean(const aValue: integer): Boolean;
begin
  if aValue=0 then Result:=true else Result:=false;
end;

function NotBoolean(const aValue: Boolean): Boolean;
begin
  if aValue=false then Result:=true else Result:=false;
end;

function latin1_process(symbol: PointerTo_zint_symbol; source: PBYTE; preprocessed: PBYTE; length: PInteger): Integer;
var
  next: Integer;
  i: Integer;
  j: Integer;
begin
  j := 0;
  i := 0;
  repeat
    next := -1;
    if Boolean(source[i] < 128) then
    begin
      preprocessed[j] := source[i];
      Inc (j);
      next := i + 1;
    end else begin
      if Boolean(source[i] = $C2) then
      begin
        preprocessed[j] := source[i + 1];
        Inc (j);
        next := i + 2;
      end;
      if Boolean(source[i] = $C3) then
      begin
        preprocessed[j] := source[i + 1] + 64;
        Inc (j);
        next := i + 2;
      end;
    end;
    if Boolean(next = -1) then
    begin
      strcpy (symbol^.errtxt, 'error: Invalid character in input string (only Latin-1 characters supported)');
      exit (ERROR_INVALID_DATA);
    end;
    i := next;
  until not (i < length^);
  preprocessed[j] := 0;
  length^ := j;
  exit (0);
end;

end.
