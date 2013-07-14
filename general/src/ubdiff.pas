unit ubdiff;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils; 

type
  size_t = Cardinal;
  Psize_t = ^size_t;
  TBlock = array[0..0] of size_t;
  PBlock = ^TBlock;
  TShortIntArray = array[0..(MaxInt div SizeOf(ShortInt) - 1)] of ShortInt;
  PShortIntArray = ^TShortIntArray;

procedure BSDiff(fn, newfn,Output: TStream);

implementation

const
  VERSION_        = '0.2.1(p)'; {update}
  FORMAT_VERSION  = '02';
  BUFFER_SIZE     = 4096;


{ output format to use }
type
  TFormat = (FMT_BINARY,FMT_FILTERED, FMT_QUOTED);
{ structure for a matching block }
  TMatch = record
  oofs: size_t; {pos in old file}
  nofs: size_t; {pos in new file}
  len: size_t;  {length: 0 if no match}
  end;
  PMatch = ^TMatch;

{ default options }
var
  min_len: size_t = 24;
  Outputformat: TFormat = FMT_BINARY;
  verbose: Integer = 0;

function ByteToOct(V: Byte): string;
var
  Idx: Integer;
  M: Byte;
begin
  Result := '';
  for Idx := 1 to 3 do
  begin
    M := V mod 8;
    V := V div 8;
    Result := Chr(M + Ord('0')) + Result;
  end;
end;

function StrToULDec(const s: PChar; var endp: PChar): Longword;
begin
  endp := s;
  Result := 0;
  while endp^ in ['0'..'9'] do
  begin
    Result := 10 * Result + (Ord(endp^) - Ord('0'));
    inc(endp);
  end;
end;

{ compare positions a and b in data area, consider maximum length dlen }

function block_sort_compare(a: size_t;b: size_t; data: PShortIntArray;dlen: size_t): Integer;
var
  pa: PShortInt;
  pb: PShortInt;
  len: size_t;
begin
  pa := @data[a];
  pb := @data[b];
  len := dlen - a;
  if dlen - b < len then
    len := dlen - b;
  while (len <> 0) and (pa^ = pb^) do
  begin
    Inc(pa); Inc(pb); Dec(len);
  end;
  if len = 0 then
  begin
    Result := a - b; Exit;
  end;
  Result := pa^ - pb^;
end;

{ the `sink element' part of heapsort }

procedure block_sort_sink(le: size_t;ri: size_t; block: PBlock;data: PShortIntArray; dlen: size_t);
var
  i, j, x: size_t;
begin
  i := le;
  x := block^[i];
  while True do
  begin
    j := 2*i+1;
    if j >= ri then
      Break;
    if j < ri-1 then
      if block_sort_compare(block^[j],block^[j+1], data, dlen) < 0 then
        Inc(j);
    if block_sort_compare(x, block^[j],data, dlen) > 0 then
      Break;
    block[i] := block[j];
    i := j;
  end;
  block^[i] := x;
end;

{ returns array of offsets into data, sorted by position }
{ returns 0 on error (out of memory) }

function block_sort(data: PShortIntArray;dlen: size_t): PBlock;
var
  block: PBlock;
  i, le, ri: size_t;
  x: size_t;
begin
  block := GetMem(sizeof(size_t) * dlen);
  if not Assigned(block) or (dlen = 0) then
  begin
    Result := nil;
    Exit;
  end;
  
  { initialize unsorted data }
  for i := 0 to Pred(dlen) do
    block^[i] := i;

  { heapsort }
  le := dlen div 2;
  ri := dlen;
  while le > 0 do
  begin
    Dec(le);
    block_sort_sink(le, ri, block, data, dlen);
  end;
  while ri > 0 do
  begin
    x := block^[le];
    block[le] := block[ri-1];
    block^[ri-1] := x;
    Dec(ri);
    block_sort_sink(le, ri, block,data, dlen);
  end;
  Result := block;
end;

{ find maximum length substring starting at sub, at most max bytes
   data, block, len characterize source fill *index returns found location
   return value is found length }

function find_string(data: PShortIntArray;block: PBlock; len: size_t;
  sub: PShortInt; max: size_t;index: Psize_t): size_t;
var
  first, last: size_t;
  mid: size_t;
  l0, l: size_t;
  pm: PShortInt;
  sm: PShortInt;
  retval: size_t;
begin
  first := 0; last := len - 1;
//mid := 0;
//l0 := 0; l := 0;
  retval := 0;
  index^ := 0;

  while first <= last do
  begin
    mid := (first + last) div 2;
    pm := @data[block^[mid]];
    sm := sub;
    l := len - block^[mid];
    if l > max then
      l := max;
    l0 := l;
    while (l <> 0) and (pm^ = sm^) do
    begin
      Dec(l); Inc(pm); Inc(sm);
    end;

    { we found a `match' of length l0-l,position block[mid] }
    if l0 - l > retval then
    begin
      retval := l0 - l;
      index^ := block^[mid];
    end;

    if (l = 0) or (pm^ < sm^) then
      first := mid + 1
      else
      begin
        last := mid;
        if last <> 0 then
          Dec(last)
        else
          Break;
      end;
  end;
  Result := retval;
end;

{ load file, returning pointer to file data, exits with error message if out
  of memory or not found }
function load_file(Input : TStream;size_ret: Psize_t): PShortIntArray;
var
  data: PShortIntArray;
  buffer: array[0..BUFFER_SIZE-1] of Char;
  len: size_t;
  cur_len: size_t;
  tmp: PShortIntArray;
begin
  { read file }
  cur_len := 0;
  data := nil;
  len := Input.Read(buffer,BUFFER_SIZE);
  while (len > 0) do
  begin
    tmp := data;
    ReallocMem(tmp, cur_len + len);
    if not Assigned(tmp) then
      raise Exception.Create('Virtual memory exhausted');
    data := tmp;
    Move(buffer, data[cur_len],len);
    Inc(cur_len, len);
    len := Input.Read(buffer,BUFFER_SIZE);
  end;
  
  if (Input.Position < Input.Size) then
    raise Exception.Create('Error reading Input');

  { exit }
  if Assigned(size_ret) then
    size_ret^ := cur_len;
  Result := data;
end;

{ pack long in little-endian format into p }
procedure pack_long(p: PShortInt;l: Longint);
begin
  p^ := l and $FF;
  Inc(p);
  p^ := (l shr 8) and $FF;
  Inc(p);
  p^ := (l shr 16) and $FF;
  Inc(p);
  p^ := (l shr 24) and $FF;
end;

{ compute simple checksum }
function checksum(data: PShortInt;len: size_t): Longint;
var
  l: Longint;
begin
  l := 0;
  while len <> 0 do
  begin
    Dec(len);
    l := ((l shr 30) and 3)
      or (l shl 2);
    l := l xor Ord(data^);
    Inc(data);
  end;
  Result := l;
end;

{ print header for 'BINARY' format }
procedure print_binary_header(Output : TStream; oldl, newl: size_t);
var
  head: array[0..15] of ShortInt;
begin
  Move('bdiff' + FORMAT_VERSION + #$1A, head[0], 8); {8 bytes}
  pack_long(@head[8], oldl);
  pack_long(@head[12], newl);
  Output.Write(head,16);
end;

{ print header for text formats }
procedure print_text_header(Output : TStream;olds, news: size_t);
var
  s : string;
begin
  s := Format('%% --- %s (%d bytes)'#13#10+ '%% +++ %s (%d bytes)'#13#10,['oldfile', olds, 'newfile', news]);
  Output.Write(s,length(s));
end;

{ print data as C-escaped string }
procedure print_quoted_data(Output : TStream;data: PShortInt;len: size_t);
begin
  while (len <> 0) do
  begin
    if (Char(data^) in [#32..#126]) and (Char(data^) <> '\') then
      Output.Write(data,1)
    else
      Output.Write(ByteToOct(data^ and $FF),length(ByteToOct(data^ and $FF)));
    Inc(data);
    Dec(len);
  end;
end;

{ print data with non-printing characters filtered }
procedure print_filtered_data(Output : TStream;data: PShortInt; len: size_t);
begin
  while len <> 0  do
  begin
    if (Char(data^) in [#32..#126]) then
      Output.Write(data,1)
    else
      Output.Write('.',1);
    Inc(data);
    Dec(len);
  end;
end;

{ print information for binary diff chunk }
procedure print_binary_add(Output : TStream;data: PShortInt; len: size_t);
var
  buf: array[0..7] of ShortInt;
  { why 0..7? why not 0..3 - only 4 bytes used }
begin
  Output.Write('+',1);
  pack_long(@buf[0], len);
  Output.Write(buf,4);
  Output.Write(data,len);
end;

{ print information for filtered diff chunk }
procedure print_filtered_add(Output : TStream;data: PShortInt; len: size_t);
begin
  Output.Write('+',1);
  print_filtered_data(Output,data, len);
  Output.Write(#13#10,2);
end;

{ print information for quoted diff chunk }
procedure print_quoted_add(Output : TStream;data: PShortInt; len: size_t);
begin
  Output.Write('+',1);
  print_quoted_data(Output,data, len);
  Output.Write(#13#10,2);
end;

{ print information for copied data in text mode }
procedure print_text_copy(Output : TStream;nbase: PShortIntArray; npos: size_t;obase: PShortIntArray; opos: size_t;len: size_t);
var
  s : string;
begin
  Output.Write(s,length(s));
  if Outputformat = FMT_FILTERED then
    print_filtered_data(Output,@nbase[npos],len)
  else
    print_quoted_data(Output,@nbase[npos],len);
  Output.Write(#13#10,2);
end;

{ print information for copied data in binary mode }
procedure print_binary_copy(Output : TStream;nbase: PShortIntArray; npos: size_t;obase: PShortIntArray; opos: size_t;len: size_t);                         //
var
  rec: array[0..11] of ShortInt;
begin
  Output.Write('@',1);
  pack_long(@rec[0], opos);
  pack_long(@rec[4], len);
  pack_long(@rec[8],checksum(@nbase[npos], len));
  Output.Write(rec,12);
end;

{ find maximum-length match }
procedure bs_find_max_match(m_ret: PMatch;data: PShortIntArray; sort: PBlock;len: size_t;text: PShortInt; tlen: size_t);
var
  found_pos: size_t;
  found_len: size_t;
begin
  m_ret^.len := 0;  {no match}
  m_ret^.nofs := 0;
  while (tlen <> 0) do
  begin
    found_len := find_string(data, sort, len, text, tlen, @found_pos);
    if found_len >= min_len then
    begin
      m_ret^.oofs := found_pos;
      m_ret^.len := found_len;
      Exit;
    end;
    Inc(text);
    Inc(m_ret^.nofs);
    Dec(tlen);
  end;
end;

{ main routine: generate diff }
procedure BSDiff(fn, newfn,Output: TStream);
var
  data: PShortIntArray;
  data2: PShortIntArray;
  len: size_t;
  len2, todo, nofs: size_t;
  sort: PBlock;
  match: TMatch;
begin
  { initialize }
//  log_status('loading old file');
  data := load_file(fn, @len);
//  log_status('loading new file');
  data2 := load_file(newfn, @len2);
//  log_status('block sorting old file');
  sort := block_sort(data, len);
  if not Assigned(sort) then
  begin
    raise Exception.Create('virtual memory exhausted');
    Exit;
  end;
//  log_status('generating patch');
  case OutputFormat of
  FMT_BINARY:print_binary_header(Output,len,len2);
  FMT_FILTERED,FMT_QUOTED:print_text_header(Output,len,len2);
  end;
  { main loop }
  todo := len2;
  nofs := 0;
  while (todo <> 0) do
  begin
    { invariant: nofs + todo = len2 }
    bs_find_max_match(@match, data,sort, len, @data2[nofs], todo);
    if match.len <> 0 then
    begin
      { found a match }
      if match.nofs <> 0 then
        { preceded by a "copy" block }
        begin
          case OutputFormat of
          FMT_BINARY:print_binary_add(Output,@data2[nofs], match.nofs);
          FMT_FILTERED:print_filtered_add(Output,@data2[nofs], match.nofs);
          FMT_QUOTED:print_quoted_add(Output,@data2[nofs], match.nofs);
          end;
        end;
      Inc(nofs, match.nofs);
      Dec(todo, match.nofs);
      case OutputFormat of
      FMT_BINARY:print_binary_copy(Output,data2, nofs,data, match.oofs, match.len);
      FMT_FILTERED,FMT_QUOTED:print_text_copy(Output,data2, nofs,data, match.oofs, match.len);
      end;
      Inc(nofs, match.len);
      Dec(todo, match.len);
    end
    else
    begin
      case OutputFormat of
      FMT_BINARY:print_binary_add(Output,@data2[nofs], todo);
      FMT_FILTERED:print_filtered_add(Output,@data2[nofs], todo);
      FMT_QUOTED:print_quoted_add(Output,@data2[nofs], todo);
      end;
      Break;
    end;
  end;
//  log_status('done');
end;


end.

