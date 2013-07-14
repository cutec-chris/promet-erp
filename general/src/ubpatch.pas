unit ubpatch;

interface

uses
  LCLIntf, SysUtils, Classes;

procedure BSPatch(const src,patch,dest: TStream);

implementation

const
  FORMAT_VERSION = '02';
  BUFFER_SIZE = 4096;

{ compute simple checksum }
function checksum(data: PChar; len: Cardinal; l: Longint): Longint;
begin
  while len <> 0 do
    begin
      Dec(len);
      l := ((l shr 30) and 3) or (l shl 2);
      l := l xor PShortInt(data)^;
      Inc(data);
    end;
  Result := l;
end;

{ get 32-bit quantity from char array }
function getlong(p: PChar): Longint;
var
  q: PByte;
  l: LongWord;
begin
  q := PByte(p);
  l := q^;  Inc(q);
  l := l + 256 * q^;  Inc(q);
  l := l + 65536 * q^;  Inc(q);
  l := l + 16777216 * q^;
  Result := l;
end;

{ copy data from one stream to another,computing checksums }
{ allows dest = 0 }
procedure copy_data(src, dest: TStream;amount, check: Longint;src_is_patch: Integer);
var
  chk: Longint;
  buffer: array[0..BUFFER_SIZE-1] of Char;
  now: cardinal;
begin
  chk := 0;
  while amount <> 0 do
    begin
      if amount > BUFFER_SIZE then
        now := BUFFER_SIZE
      else
        now := amount;
      if Src.Read(buffer,now)  <> now then
        begin
          if Src.Position = Src.Size then
            begin
              if src_is_patch <> 0 then
                raise Exception.Create('Patch garbled - unexpected end of data')
              else
                raise Exception.Create('Source file does not match patch');
            end
          else
            begin
              if src_is_patch <> 0 then
                raise Exception.Create('Error reading patch file')
              else
                raise Exception.Create('Error reading source file');
            end;
        end;
      if Assigned(dest) then
        if Dest.Write(Buffer,now) <> now then
          raise Exception.Create('Error writing temporary file');
      chk := checksum(buffer, now, chk);
      Dec(amount, now);
    end;
  if (src_is_patch = 0) and (chk <> check) then
    raise Exception.Create('Source file does not match patch');
end;

{ apply patch }
procedure BSPatch(const src,patch,dest: TStream);
var
  header: array[0..15] of Char;
  p: PChar;
  q: PChar;
  srclen, destlen: Longint;
  size: Longint;
  ofs: Longint;
  c: byte;
const
  error_msg = 'Patch garbled - invalid section ''%''';
begin
  { read header }
  if Patch.Read(header,16) <> 16 then
    raise Exception.Create('Patch not in BINARY format');
  if StrLComp(header, PChar('bdiff' + FORMAT_VERSION + #$1A), 8) <> 0 then
    raise Exception.Create('Patch not in BINARY format');
  srclen := getlong(@header[8]);
  destlen := getlong(@header[12]);
  { apply patch }
  while True do
    begin
      Patch.Read(c,1);
      if c = -1 then
        Break;
      case Integer(c) of
        Integer('@'):
          begin
            { copy from source }
            if Patch.Read(header,12) <> 12 then
              raise Exception.Create('Patch garbled - unexpected end of data');
            size := getlong(@header[4]);
            ofs := getlong(@header[0]);
            if (ofs < 0) or (size <= 0)
            or (ofs > srclen)
            or (size > srclen)
            or (size+ofs > srclen) then
              raise Exception.Create('Patch garbled - invalid change request');
            Src.Position := ofs;
            if Src.Position <> ofs then
              raise Exception.Create('''fseek'' on source file failed');
            copy_data(Src, Dest, size,getlong(@header[8]), 0);
            Dec(destlen, size);
          end;
        Integer('+'):
          begin
            { copy N bytes from patch }
            if Patch.Read(header,4) <> 4 then
              raise Exception.Create('Patch garbled - unexpected end of data');
            size := getlong(@header[0]);
            copy_data(Patch, Dest, size,0, 1);
            Dec(destlen, size);
          end;
        else
          begin
            StrRScan(error_msg, '%')^ := Char(c);
            raise Exception.Create(error_msg);
          end;
    end;
    if destlen < 0 then
      raise Exception.Create('Patch garbled - patch file longer than announced in header');
  end;
  if destlen <> 0 then
    raise Exception.Create('Patch garbled - destination file shorter than announced in header');
end;

end.
