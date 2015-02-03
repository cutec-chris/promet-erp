unit SecureUtils; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Utils
  {$IFDEF WINDOWS}
  , Windows,RtlConsts
  {$ENDIF}
  ;
  
type
  TSecureDeleteMethod = (dmDoD522022,dmOverride,dmSecure);
  
{$IFDEF WINDOWS}
{ TFlagFileStream }

TFlagFileStream = Class(THandleStream)
  Public
    constructor Create(const FileName: string; const AMode: word; Flags: DWord);
    Destructor Destroy; Override;
  End;
{$ENDIF}
  
function DeleteSecure(Filename : string;Method : TSecureDeleteMethod = dmSecure) : Boolean;
//function DeleteDirectorySecure(const DirectoryName: string;OnlyChilds: boolean;Method : TSecureDeleteMethod = dmSecure): boolean;

implementation

{
function DeleteDirectorySecure(const DirectoryName: string;OnlyChilds: boolean;Method : TSecureDeleteMethod): boolean;
var
  FileInfo: TSearchRec;
  CurSrcDir: String;
  CurFilename: String;
begin
  Result:=false;
  CurSrcDir:=CleanAndExpandDirectory(DirectoryName);
  if FindFirstUTF8(CurSrcDir+GetAllFilesMask,faAnyFile,FileInfo)=0 then
    begin
      repeat
        // check if special file
        if (FileInfo.Name='.') or (FileInfo.Name='..') or (FileInfo.Name='') then
          continue;
        CurFilename:=CurSrcDir+FileInfo.Name;
        if (FileInfo.Attr and faDirectory)>0 then begin
          if not DeleteDirectorySecure(CurFilename,false,Method) then exit;
        end else begin
          if not DeleteSecure(CurFilename,Method) then exit;
        end;
      until FindNextUTF8(FileInfo)<>0;
    end;
  SysUtils.FindClose(FileInfo);
  if (not OnlyChilds) and (not RemoveDirUTF8(DirectoryName)) then exit;
  Result:=true;
end;
}
function WriteData(Filename : string;Data : byte) : Boolean;
const buffersize = 1024;
var
  fs : THandleStream;
  i : Integer;
  buffer : array[0..buffersize-1] of byte;
begin
  for i := 0 to buffersize-1 do
    buffer[i] := Data;
  Result := True;
  try
{$IFDEF WINDOWS}
    fs:=TFlagFileStream.Create(Filename,fmOpenWrite,{FILE_FLAG_NO_BUFFERING}FILE_FLAG_WRITE_THROUGH);
{$ELSE}
    fs:=TFileStream.Create(Filename,fmOpenWrite);
{$ENDIF}
    repeat
      fs.Write(buffer, buffersize);
    until fs.Position + 1 >= fs.Size;
{$IFDEF WINDOWS}
    FlushFileBuffers(fs.Handle);
{$ENDIF}
    fs.Free;
  except
    Result := False;
  end;
end;



procedure ZeroFillDelete(FileName: string);
var
  fs: THandleStream;
  i:  integer;
  procedure RandomWrite;
  var
    b : array[0..1024] of byte;
    i : Integer;
  begin
    repeat
      for i := 0 to 1024 do
        b[i] := Random(256);
      fs.Write(b, 1024);
    until fs.Position + 1 >= fs.Size;
  end;
  procedure WritePattern(pattern: byte);
  var
    i : Integer;
  const patt: array[5..31] of dword = ($555555, $AAAAAA, $924924, $492492,
        $249249, 0, $111111, $222222, $333333, $444444, $555555, $666666,
        $777777, $888888, $999999, $AAAAAA, $BBBBBB, $CCCCCC, $DDDDDD,
        $EEEEEE, $FFFFFF, $924924, $492492, $249249, $6DB6DB, $B6DB6D, $DB6DB6);
  var d : array[0..512] of dword;
  begin
      for i := 0 to 512 do
        d[i] := patt[pattern] shl 8;
    repeat fs.Write(d, 512*sizeof(dword)); until fs.Position + 3 >= fs.Size;
  end;
begin
  if not FileExists(FileName) then Exit;
  for i := 1 to 35 do
  try
{$IFDEF WINDOWS}
    fs:=TFlagFileStream.Create(Filename,fmOpenWrite,{FILE_FLAG_NO_BUFFERING}FILE_FLAG_WRITE_THROUGH);
{$ELSE}
    fs:=TFileStream.Create(Filename,fmOpenWrite);
{$ENDIF}
    try
      if (i < 5) or (i > 31) then RandomWrite
      else WritePattern(i);
    finally
{$IFDEF WINDOWS}
      FlushFileBuffers(fs.Handle);
{$ENDIF}
      fs.Free;
    end;
  except Exit; end;
end;


function DeleteSecure(Filename: string; Method: TSecureDeleteMethod): Boolean;
var
  i,a: Integer;
  aFilename: String;
  newFilename: String;
begin
  Result:=False;
  case Method of
  dmDoD522022:
    begin
      Result := WriteData(UniToSys(Filename),0);
      Result := Result and WriteData(UniToSys(Filename),$FF);
      Randomize;
      Result := Result and WriteData(UniToSys(Filename),Random($FF));
    end;
  dmOverride:
    begin
      Result := WriteData(UniToSys(Filename),0);
    end;
  dmSecure:
    begin
      ZeroFillDelete(UniToSys(Filename));
      Result := True;
    end;
  end;
  aFilename := Filename;
  for i := 0 to 35 do
    begin
      newFilename := '';
      for a := 0 to 15 do
        newFilename := newFilename+chr($30+Random(26));
      newFilename := ValidateFilename(newFilename);
      if RenameFile(aFilename,newFilename) then
        aFilename := newFilename;
    end;
  SysUtils.DeleteFile(aFilename);
end;

{$IFDEF WINDOWS}
{ TFlagFileStream }

constructor TFlagFileStream.Create(const FileName: string; const AMode: word; Flags: DWord);
var
  lHandle: THandle;
begin
  if AMode = fmCreate then
  begin
    lHandle := CreateFile(PChar(FileName), GENERIC_READ Or GENERIC_WRITE, 0, Nil, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL or Flags, 0);
    if lHandle = INVALID_HANDLE_VALUE then
      raise EFCreateError.CreateResFmt(PResStringRec(@SFCreateError), [FileName]);
  end
  else
  begin
    lHandle := CreateFile(PChar(FileName), GENERIC_READ Or GENERIC_WRITE,0, Nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL or Flags, 0);
    if lHandle = INVALID_HANDLE_VALUE then
      raise EFOpenError.CreateResFmt(PResStringRec(@SFOpenError), [FileName]);
  end;
  inherited Create(lHandle);
end;

destructor TFlagFileStream.Destroy;
begin
  FileClose(Handle);
  inherited Destroy;
end;
{$ENDIF}

end.

