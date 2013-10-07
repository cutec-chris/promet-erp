unit ureedsolomon;

{$mode objfpc}{$H+}

interface

procedure rs_init_gf(poly: Integer);
procedure rs_init_code(nsym: Integer; index: Integer);
procedure rs_encode(len: Integer; data: PBYTE; res: PBYTE);
procedure rs_encode_long(len: Integer; data: PCardinal; res: PCardinal);
procedure rs_free();

implementation

var
//  gfpoly: integer;
//  symsize: integer;		// in bits
  logmod: integer;		// 2**symsize - 1
  rlen: integer;

  logt: PInteger = nil;
  alog: PInteger = nil;
  rspoly: PInteger = nil;

procedure rs_init_gf(poly: Integer);
var
  b: Integer;
  p: Integer;
  v: Integer;
  m: Integer;
begin
  b := 1;
  m := 0;
  while b <= poly do
  begin
    Inc (m);
    b := b shl 1;
  end;
  b := b shr 1;
  Dec (m);
//  gfpoly := poly;
//  symsize := m;
  logmod := (1 shl m) - 1;
  logt := PInteger (GetMem (SizeOf (Integer) * (logmod + 1)));
  alog := PInteger (GetMem (SizeOf (Integer) * logmod));
  p := 1;
  v := 0;
  while v < logmod do
  begin
    alog[v] := p;
    logt[p] := v;
    p := p shl 1;
    if (p and b)<>0 then
    begin
			p:=p xor poly;
    end;
    Inc (v);
  end;
end;

procedure rs_init_code(nsym: Integer; index: Integer);
var
  k: Integer;
  i: Integer;
begin
  rspoly := PInteger (GetMem (SizeOf (Integer) * (nsym + 1)));
  rlen := nsym;
  rspoly[0] := 1;
  i := 1;
  while i <= nsym do
  begin
    rspoly[i] := 1;
    k := i - 1;
    while k > 0 do
    begin
      if rspoly[k]<>0 then
      begin
        rspoly[k] := alog[(logt[rspoly[k]] + index) mod logmod];
      end;
			rspoly[k] := rspoly[k] xor rspoly[k - 1];
      Dec (k);
    end;
    rspoly[0] := alog[(logt[rspoly[0]] + index) mod logmod];
    Inc (index);
    Inc (i);
  end;
end;

procedure rs_encode(len: Integer; data: PBYTE; res: PBYTE);
var
  k: Integer;
  m: Integer;
  i: Integer;
begin
  i := 0;
  while i < rlen do
  begin
    res[i] := 0;
    Inc (i);
  end;
  i := 0;
  while i < len do
  begin
    m := res[rlen - 1] xor data[i];
    k := rlen - 1;
    while k > 0 do
    begin
      if (m<>0) and (rspoly[k]<>0) then
      begin
        res[k] := integer(res[k - 1]) xor alog[(logt[m] + logt[rspoly[k]]) mod logmod];
      end else begin
        res[k] := integer(res[k - 1]);
      end;
      Dec (k);
    end;
    if (m<>0) and (rspoly[0]<>0) then
    begin
      res[0] := alog[(logt[m] + logt[rspoly[0]]) mod logmod];
    end else begin
      res[0] := 0;
    end;
    Inc (i);
  end;
end;

procedure rs_encode_long(len: Integer; data: PCardinal; res: PCardinal);
var
  k: Integer;
  m: Integer;
  i: Integer;
begin
  i := 0;
  while i < rlen do
  begin
    res[i] := 0;
    Inc (i);
  end;
  i := 0;
  while i < len do
  begin
    m := res[rlen - 1] xor data[i];
    k := rlen - 1;
    while k > 0 do
    begin
      if (m<>0) and (rspoly[k]<>0) then
      begin
        res[k] := Integer(res[k - 1]) xor alog[(logt[m] + logt[rspoly[k]]) mod logmod];
      end else begin
        res[k] := res[k - 1];
      end;
      Dec (k);
    end;
    if (m<>0) and (rspoly[0]<>0) then
    begin
      res[0] := alog[(logt[m] + logt[rspoly[0]]) mod logmod];
    end else begin
      res[0] := 0;
    end;
    Inc (i);
  end;
end;

procedure rs_free();
begin
  freeMem (logt);
  freeMem (alog);
  freeMem (rspoly);
  rspoly := nil;
end;

end.

