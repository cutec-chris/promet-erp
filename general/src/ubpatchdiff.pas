unit ubpatchdiff;

interface

implementation

uses
  LCLIntf, SysUtils;

const
  FORMAT_VERSION = '02';
  BUFFER_SIZE = 4096;

var
  progname: string;                     //char* progname;
  tempfile: string = '';                //char* tempfile = 0;
  tempfd: Integer = 0;                  //FILE* tempfd = 0;


{ emulates C std lib feof function using Windows file handle}
function feof(stream: Integer): Boolean;
var
  CurPos: Integer;
  Size: Integer;
begin
  CurPos := SysUtils.FileSeek(stream, 0, 1);
//TODO  Size := SysUtils.GetFileSize(stream, nil);
  Result := CurPos = Size;
end;
                                        //
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
procedure copy_data(src, dest: Integer;amount, check: Longint;src_is_patch: Integer);
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
      if SysUtils.FileRead(src,buffer,now) <> now then
        begin
          if feof(src) then
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
      if dest <> 0 then
        if SysUtils.FileWrite(dest,buffer,now) <> now then
          raise Exception.Create('Error writing temporary file');
      chk := checksum(buffer, now, chk);
      Dec(amount, now);
    end;
  if (src_is_patch = 0) and (chk <> check) then
    raise Exception.Create('Source file does not match patch');
end;

{ apply patch }
procedure bpatch_(const src,dest: PChar);
var
  sf: Integer; {source file}
  df: Integer; {destination file}
  header: array[0..15] of Char;
  p: PChar;
  q: PChar;
  srclen, destlen: Longint;
  size: Longint;
  ofs: Longint;
  c: Integer;
const
  error_msg = 'Patch garbled - invalid section ''%''';
begin
  { read header }
  if SysUtils.FileRead(stdin,@header,16) <> 16 then
    raise Exception.Create('Patch not in BINARY format');
  if StrLComp(header, PChar('bdiff' + FORMAT_VERSION + #$1A), 8) <> 0 then
    raise Exception.Create('Patch not in BINARY format');
  srclen := getlong(@header[8]);
  destlen := getlong(@header[12]);
  { open source file }
  sf := FileOpen(src,fmOpenRead + fmShareDenyNone);
  if sf <= 0 then
    begin
      perror(src);
      Halt(1);
    end;
  { create temporary file }
  if StrLen(dest) = 0 then
    error_exit('Empty destination file name');
  { hack source file name to get a suitable temp file name }
  tempfile := dest;
  p := StrRScan(PChar(tempfile), '/');
  if not Assigned(p) then
    p := PChar(tempfile)
  else
    Inc(p);
{$IFDEF MSWINDOWS} //something with the drive letters
  q := StrRScan(p, '\');
  if Assigned(q) then
    p := q + 1;
  q := StrRScan(p, ':');
  if Assigned(q) then
    p := q + 1;
{$ENDIF}
  p^ := '$';
  df := FileCreate(tempfile);
  if df <= 0 then
    error_exit('Can''t create temporary file');
  tempfd := df;
  { apply patch }
  while True do
    begin
      c := fgetc(stdin);
      if c = -1 then
        Break;
      case c of
        Integer('@'):
          begin
            { copy from source }
            if SysUtils.ReadFile(stdin,@header,12) <> 12 then
              raise Exception.Create('Patch garbled - unexpected end of data');
            size := getlong(@header[4]);
            ofs := getlong(@header[0]);
            if (ofs < 0) or (size <= 0)
            or (ofs > srclen)
            or (size > srclen)
            or (size+ofs > srclen) then
              raise Exception.Create('Patch garbled - invalid change request');
            if fseek(sf, ofs,0) <> 0 then
              raise Exception.Create('''fseek'' on source file failed');
            copy_data(sf, df, size,getlong(@header[8]), 0);
            Dec(destlen, size);
          end;
        Integer('+'):
          begin
            { copy N bytes from patch }
            if Sysutils.readFile(stdin,@header,4) <> 4 then
              raise Exception.Create('Patch garbled - unexpected end of data');
            size := getlong(@header[0]);
            copy_data(stdin, df, size,0, 1);
            Dec(destlen, size);
          end;
        else
          begin
            fclose(sf);
            fclose(df);
            StrRScan(error_msg, '%')^ := Char(c);
            raise Exception.Create(error_msg);
          end;
    end;
    if destlen < 0 then
      raise Exception.Create('Patch garbled - patch file longer than announced in header');
  end;
  if destlen <> 0 then
    raise Exception.Create('Patch garbled - destination file shorter than announced in header');
  fclose(sf);
  fclose(df);
  tempfd := 0;
  if not RenameFile(tempfile, dest) then
    begin
      raise Exception.Create('Can''t rename temporary file');
    end;
  tempfile := '';
end;

end.
