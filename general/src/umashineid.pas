unit umashineid;

{$mode objfpc}{$H+}

interface

uses
  {$IFDEF MSWINDOWS}
  Windows,
  {$ELSE}
  BaseUnix,
  {$ENDIF}
  Classes, SysUtils;

function CreateUserID : LongInt;
function CreateMachineID : LongInt;

implementation

function AddStringToLong(const Buf; BufSize : LongInt) : LongInt;
var
  BufBytes  : TByteArray absolute Buf;
  I: Integer;
begin
  Result := 0;
  for I := 0 to BufSize - 1 do
    Result := Result+BufBytes[i] shl I*4;
end;

{$IFDEF WIN32}
function CreateUserID : LongInt;
const
  sCurVer   = 'Software\Microsoft\Windows\CurrentVersion';
  sCurVerNT = 'Software\Microsoft\Windows NT\CurrentVersion';
  sRegOwner = 'RegisteredOwner';
  sRegOrg   = 'RegisteredOrganization';
var
  UserInfoFound: Boolean;
  RegKey  : HKEY;
  Buf     : array [0..1023] of Byte;
  I: Integer;
begin
  Result := 0;
  UserInfoFound := False;
  { first look for registered info in \Windows\CurrentVersion }
  if (RegOpenKeyEx(HKEY_LOCAL_MACHINE, sCurVer, 0, KEY_QUERY_VALUE, RegKey) = ERROR_SUCCESS) then
    begin
      I := SizeOf(Buf);
      if RegQueryValueEx(RegKey, sRegOwner, nil, nil, @Buf, @I) = ERROR_SUCCESS then
        begin
          UserInfoFound := True;
          Result := Result+AddStringToLong(Buf,I);
          I := SizeOf(Buf);
          if RegQueryValueEx(RegKey, sRegOrg, nil, nil, @Buf, @I) = ERROR_SUCCESS then
            Result := result+AddStringToLong(Buf, I);
      end;
      RegCloseKey(RegKey);
    end;
  if not UserInfoFound then
    if (RegOpenKeyEx(HKEY_LOCAL_MACHINE, sCurVerNT, 0, KEY_QUERY_VALUE, RegKey) = ERROR_SUCCESS) then
      begin
        I := SizeOf(Buf);
        if RegQueryValueEx(RegKey, sRegOwner, nil, nil, @Buf, @I) = ERROR_SUCCESS then
          begin
            Result := Result+AddStringToLong(Buf, I);
            I := SizeOf(Buf);
            if RegQueryValueEx(RegKey, sRegOrg, nil, nil, @Buf, @I) = ERROR_SUCCESS then
              Result := Result+AddStringToLong(Buf, I);
          end;
        RegCloseKey(RegKey);
      end;
end;
{$ELSE}
function CreateUserID : LongInt;
var
  tmp: String;
  i: Integer;
begin
  tmp := GetEnvironmentVariable('USERNAME');
  if tmp = '' then
    tmp := GetEnvironmentVariable('USER');
  Result := 0;
  for i := 0 to length(tmp)-1 do
    Result := Result+ord(tmp[i]);
end;
{$ENDIF}

{$IFDEF WINDOWS}
function CreateMachineID : LongInt;
type
  TUuidCreateSequential = function (lpGUID : Pointer): HResult; stdcall;
var
  hRPCTR4 : THandle;
  UuidCreateSequential : TUuidCreateSequential;
  I       : DWord;
  Drive   : AnsiChar;
  mDrive : string;
  SysInfo : TSystemInfo;
  UserInfoFound : Boolean;
  Buf     : array [0..1023] of Byte;
begin
  Result := 0;
  {include system specific information}
  GetSystemInfo(SysInfo);
  {$IFDEF VER2_2_0}
  PDWord(@Buf[0])^ := SysInfo.u.dwOemId;
  {$ELSE}
  PDWord(@Buf[0])^ := SysInfo.dwOemId;
  {$ENDIF}
  PDWord(@Buf[4])^ := SysInfo.dwProcessorType;
  Result := Result+AddStringToLong(Buf, 8);

  {include drive specific information}
  for Drive := 'C' to 'D' do
    begin
      mDrive := Drive + ':\';
      if (GetDriveType(PAnsiChar(mDrive)) = DRIVE_FIXED) then
        begin
          FillChar(Buf, Sizeof(Buf), 0);
          Buf[0] := Byte(Drive);
          GetVolumeInformation(PAnsiChar(mDrive), nil, 0, PDWord(@Buf[1]){serial number}, I{not used}, I{not used}, nil, 0);
          Result := Result+AddStringToLong(Buf, 5);
       end;
    end;
end;
{$ENDIF}
{$IFDEF UNIX}
function CreateMachineID : LongInt;
var
  I       : LongInt;
  RegKey  : DWord;
  GUID1   : TGUID;
  GUID2   : TGUID;
  Drive   : Integer;
  Buf     : array [0..2047] of Byte;
  iFileHandle : LongInt;
  tmp: String;
begin
  Result := 0;
  {include system specific information}
//  iFileHandle := FileOpen('/proc/cpuinfo', fmopenRead or fmShareDenyNone);
//  I := FileRead(iFileHandle, Buf,2048);
//  if I > 0 then  Result := Result+AddStringToLong(Buf, I-1);
//  FileClose(iFileHandle);

  iFileHandle := FileOpen('/proc/sys/kernel/hostname', fmopenRead or fmShareDenyNone);
  I := FileRead(iFileHandle, Buf, 2048);
  if I > 0 then  Result := result+AddStringToLong(Buf, I-1);
  FileClose(iFileHandle);

  {include network ID}
  CreateGuid(GUID1);
  CreateGuid(GUID2);
  {check to see if "network" ID is available}
  if (GUID1.D4[2] = GUID2.D4[2]) and
     (GUID1.D4[3] = GUID2.D4[3]) and
     (GUID1.D4[4] = GUID2.D4[4]) and
     (GUID1.D4[5] = GUID2.D4[5]) and
     (GUID1.D4[6] = GUID2.D4[6]) and
     (GUID1.D4[7] = GUID2.D4[7]) then
    Result := Result+AddStringToLong(GUID1.D4[2], 6);
  {$IFDEF DARWIN}
  tmp := ExecProcessEx('system_profiler -detailLevel minimal');
  tmp := copy(tmp,pos('Serial Number',tmp)+13,length(tmp));
  tmp := copy(tmp,pos(':',tmp)+1,length(tmp));
  tmp := copy(tmp,0,pos(lineending,tmp)-1);
  Result := result+AddStringToLong(tmp[1],length(tmp)-1);
  {$ENDIF}

end;
{$ENDIF}


end.


