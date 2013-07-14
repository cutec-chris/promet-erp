{**************************************************************************************************}
{                                                                                                  }
{  Y core library (Ycl)                                                                            }
{                                                                                                  }
{  YclFileUtils - file utility functions                                                           }
{                                                                                                  }
{  The contents of this file are subject to the Y Library Public License Version 1.0 (the          }
{  "License"); you may not use this file except in compliance with the License. You may obtain a   }
{  copy of the License at http://delphi.pjh2.de/                                                   }
{                                                                                                  }
{  Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF  }
{  ANY KIND, either express or implied. See the License for the specific language governing rights }
{  and limitations under the License.                                                              }
{                                                                                                  }
{  The Original Code is: YclFileUtils.pas.                                                         }
{  The Initial Developer of the Original Code is Peter J. Haas (libs@pjh2.de). Portions created    }
{  by Peter J. Haas are Copyright (C) 2000-2005 Peter J. Haas. All Rights Reserved.                }
{                                                                                                  }
{  Contributor(s):                                                                                 }
{   Hendrik Friedel, Udo Nesshoever, Christian Hintz (testing GetFreeSpace64, GetTotalSpace64)     }
{                                                                                                  }
{  You may retrieve the latest version of this file at the homepage of Peter J. Haas, located at   }
{  http://delphi.pjh2.de/                                                                          }
{                                                                                                  }
{**************************************************************************************************}

// For history see end of file

{$I Ycl.inc}

unit YclFileUtils;

interface
uses
  {$IFDEF WIN32}
  Windows,
  {$ENDIF WIN32}
  SysUtils, Classes,
  YclBase, YclDateTimes;

const
  WindowsPathDelimiter = '\';
  UnixPathDelimiter = '/';
  {$IFDEF WIN32}
  OSPathDelimiter = WindowsPathDelimiter;
  {$ENDIF WIN32}
  {$IFDEF UNIX}
  OSPathDelimiter = UnixPathDelimiter;
  {$ENDIF UNIX}

{$IFNDEF RTL_XPLATFORM ~}
function IncludeTrailingPathDelimiter(const Value: String): String;
function ExcludeTrailingPathDelimiter(const Value: String): String;
function ForceDirectories(Dir: String): Boolean;
{$ENDIF ~RTL_XPLATFORM}

procedure AddFileList(RootDir: String; List: TStrings);
procedure GetFileList(RootDir: String; List: TStrings);

function UnixPathToOSPath(const Value: String): String;
function OSPathToUnixPath(const Value: String): String;

function GetUnambiguousFilename(
    const Filename: String;
    MaxNumber: Cardinal{$IFDEF SUPPORTS_DEFAULTPARAMS} = High(Cardinal){$ENDIF}
  ): String;

// prüft ob DirName ein existierendes Verzeichnis ist
function DirectoryExists(const DirName: String): Boolean;

// ***********************************  File Date / Time  ************************************

// Get last write time per TDataTime or TUnixTime
//   Filename: Filename
//   Value: Return the time value
//   IsUTCBased: Returned time value should be a UTC value
function GetFileLastWriteTime(
    const FileName: String;
    out Value: TDateTime;
    const IsUTCBased: Boolean{$IFDEF SUPPORTS_DEFAULTPARAMS} = False{$ENDIF}
  ): Boolean; overload;
function GetFileLastWriteTime(
    const FileName: String;
    out Value: TUnixTime32;
    const IsUTCBased: Boolean{$IFDEF SUPPORTS_DEFAULTPARAMS} = False{$ENDIF}
  ): Boolean; overload;
function GetFileLastWriteTime(
    const FileName: String;
    out Value: TUnixTime64;
    const IsUTCBased: Boolean{$IFDEF SUPPORTS_DEFAULTPARAMS} = False{$ENDIF}
  ): Boolean; overload;

// Get last access time per TDataTime or TUnixTime
function GetFileLastAccessTime(
    const FileName: String;
    out Value: TDateTime;
    const IsUTCBased: Boolean{$IFDEF SUPPORTS_DEFAULTPARAMS} = False{$ENDIF}
  ): Boolean; overload;
function GetFileLastAccessTime(
    const FileName: String;
    out Value: TUnixTime32;
    const IsUTCBased: Boolean{$IFDEF SUPPORTS_DEFAULTPARAMS} = False{$ENDIF}
  ): Boolean; overload;
function GetFileLastAccessTime(
    const FileName: String;
    out Value: TUnixTime64;
    const IsUTCBased: Boolean{$IFDEF SUPPORTS_DEFAULTPARAMS} = False{$ENDIF}
  ): Boolean; overload;

// Get creation time per TDataTime or TUnixTime
function GetFileCreationTime(
    const FileName: String;
    out Value: TDateTime;
    const IsUTCBased: Boolean{$IFDEF SUPPORTS_DEFAULTPARAMS} = False{$ENDIF}
  ): Boolean; overload;
function GetFileCreationTime(
    const FileName: String;
    out Value: TUnixTime32;
    const IsUTCBased: Boolean{$IFDEF SUPPORTS_DEFAULTPARAMS} = False{$ENDIF}
  ): Boolean; overload;
function GetFileCreationTime(
    const FileName: String;
    out Value: TUnixTime64;
    const IsUTCBased: Boolean{$IFDEF SUPPORTS_DEFAULTPARAMS} = False{$ENDIF}
  ): Boolean; overload;

// Get all times per TDataTime or TUnixTime
//   Filename: Filename
//   LastWriteTime: Last write time value
//   LastAccessTime: Last access time value
//   CreationTime: Creation time value
//   IsUTCBased: Time value is a UTC value
function GetFileTimes(
    const FileName: String;
    out LastWriteTime, LastAccessTime, CreationTime: TDateTime;
    const IsUTCBased: Boolean{$IFDEF SUPPORTS_DEFAULTPARAMS} = False{$ENDIF}
  ): Boolean; overload;
function GetFileTimes(
    const FileName: String;
    out LastWriteTime, LastAccessTime, CreationTime: TUnixTime32;
    const IsUTCBased: Boolean{$IFDEF SUPPORTS_DEFAULTPARAMS} = False{$ENDIF}
  ): Boolean; overload;
function GetFileTimes(
    const FileName: String;
    out LastWriteTime, LastAccessTime, CreationTime: TUnixTime64;
    const IsUTCBased: Boolean{$IFDEF SUPPORTS_DEFAULTPARAMS} = False{$ENDIF}
  ): Boolean; overload;

// Set last write time per TDataTime or TUnixTime
//   Filename: Filename
//   Value: The time value
//   IsUTCBased: Time value is a UTC value
function SetFileLastWriteTime(
    const FileName: String;
    const Value: TDateTime;
    const IsUTCBased: Boolean{$IFDEF SUPPORTS_DEFAULTPARAMS} = False{$ENDIF}
  ): Boolean; overload;
function SetFileLastWriteTime(
    const FileName: String;
    const Value: TUnixTime32;
    const IsUTCBased: Boolean{$IFDEF SUPPORTS_DEFAULTPARAMS} = False{$ENDIF}
  ): Boolean; overload;
function SetFileLastWriteTime(
    const FileName: String;
    const Value: TUnixTime64;
    const IsUTCBased: Boolean{$IFDEF SUPPORTS_DEFAULTPARAMS} = False{$ENDIF}
  ): Boolean; overload;

// Set last access time per TDataTime or TUnixTime
function SetFileLastAccessTime(
    const FileName: String;
    const Value: TDateTime;
    const IsUTCBased: Boolean{$IFDEF SUPPORTS_DEFAULTPARAMS} = False{$ENDIF}
  ): Boolean; overload;
function SetFileLastAccessTime(
    const FileName: String;
    const Value: TUnixTime32;
    const IsUTCBased: Boolean{$IFDEF SUPPORTS_DEFAULTPARAMS} = False{$ENDIF}
  ): Boolean; overload;
function SetFileLastAccessTime(
    const FileName: String;
    const Value: TUnixTime64;
    const IsUTCBased: Boolean{$IFDEF SUPPORTS_DEFAULTPARAMS} = False{$ENDIF}
  ): Boolean; overload;

// Set creation time per TDataTime or TUnixTime
function SetFileCreationTime(
    const FileName: String;
    const Value: TDateTime;
    const IsUTCBased: Boolean{$IFDEF SUPPORTS_DEFAULTPARAMS} = False{$ENDIF}
  ): Boolean; overload;
function SetFileCreationTime(
    const FileName: String;
    const Value: TUnixTime32;
    const IsUTCBased: Boolean{$IFDEF SUPPORTS_DEFAULTPARAMS} = False{$ENDIF}
  ): Boolean; overload;
function SetFileCreationTime(
    const FileName: String;
    const Value: TUnixTime64;
    const IsUTCBased: Boolean{$IFDEF SUPPORTS_DEFAULTPARAMS} = False{$ENDIF}
  ): Boolean; overload;

// Set all times per TDataTime or TUnixTime
//   Filename: Filename
//   LastWriteTime: Last write time value
//   LastAccessTime: Last access time value
//   CreationTime: Creation time value
//   IsUTCBased: Time value is a UTC value
function SetFileTimes(
    const FileName: String;
    const LastWriteTime, LastAccessTime, CreationTime: TDateTime;
    const IsUTCBased: Boolean{$IFDEF SUPPORTS_DEFAULTPARAMS} = False{$ENDIF}
  ): Boolean; overload;
function SetFileTimes(
    const FileName: String;
    const LastWriteTime, LastAccessTime, CreationTime: TUnixTime32;
    const IsUTCBased: Boolean{$IFDEF SUPPORTS_DEFAULTPARAMS} = False{$ENDIF}
  ): Boolean; overload;
function SetFileTimes(
    const FileName: String;
    const LastWriteTime, LastAccessTime, CreationTime: TUnixTime64;
    const IsUTCBased: Boolean{$IFDEF SUPPORTS_DEFAULTPARAMS} = False{$ENDIF}
  ): Boolean; overload;

// ****************************  File Date / Time  Single Value  *****************************
type
  TFileTimeKind = (ftLastAccess, ftLastWrite, ftCreation);

// Get time value per TDataTime or TUnixTime
//   Filename: Filename
//   Value: Return the time value
//   Times: Kind of the time value
//   IsUTCBased: Returned time value should be a UTC value
function GetFileTime(
    const Filename: String;
    out Value: TDateTime;
    Times: TFileTimeKind;
    IsUTCBased: Boolean{$IFDEF SUPPORTS_DEFAULTPARAMS} = False{$ENDIF}
  ): Boolean; overload;
function GetFileTime(
    const Filename: String;
    out Value: TUnixTime32;
    Times: TFileTimeKind;
    IsUTCBased: Boolean{$IFDEF SUPPORTS_DEFAULTPARAMS} = False{$ENDIF}
  ): Boolean; overload;
function GetFileTime(
    const Filename: String;
    out Value: TUnixTime64;
    Times: TFileTimeKind;
    IsUTCBased: Boolean{$IFDEF SUPPORTS_DEFAULTPARAMS} = False{$ENDIF}
  ): Boolean; overload;

// Set time value per TDataTime or TUnixTime
//   Filename: Filename
//   Value: The time value
//   Times: Kind of the time value
//   IsUTCBased: Time value is a UTC value
function SetFileTime(
    const FileName: String;
    const Value: TDateTime;
    Times: TFileTimeKind;
    IsUTCBased: Boolean{$IFDEF SUPPORTS_DEFAULTPARAMS} = False{$ENDIF}
  ): Boolean; overload;
function SetFileTime(
    const FileName: String;
    const Value: TUnixTime32;
    Times: TFileTimeKind;
    IsUTCBased: Boolean{$IFDEF SUPPORTS_DEFAULTPARAMS} = False{$ENDIF}
  ): Boolean; overload;
function SetFileTime(
    const FileName: String;
    const Value: TUnixTime64;
    Times: TFileTimeKind;
    IsUTCBased: Boolean{$IFDEF SUPPORTS_DEFAULTPARAMS} = False{$ENDIF}
  ): Boolean; overload;

// ******************************  File Date / Time  TFileTime  ******************************

// Get time value per TFileTime
//   Filename: Filename
//   Value: Return the time value
//   Times: Kind of the time value
//   IsUTCBased: Returned time value should be a UTC value
function FTGetFileTime(
    const Filename: String;
    out Value: TWinFileTime;
    Times: TFileTimeKind;
    IsUTCBased: Boolean{$IFDEF SUPPORTS_DEFAULTPARAMS} = False{$ENDIF}
  ): Boolean;

// Get all times per TFileTime
//   Filename: Filename
//   LastWriteTime: Last write time value
//   LastAccessTime: Last access time value
//   CreationTime: Creation time value
//   IsUTCBased: Time value is a UTC value
function FTGetFileTimes(
    const FileName: String;
    out LastWriteFileTime, LastAccessFileTime, CreationFileTime: TWinFileTime;
    IsUTCBased: Boolean{$IFDEF SUPPORTS_DEFAULTPARAMS} = False{$ENDIF}
  ): Boolean;

// Set time value per TFileTime
//   Filename: Filename
//   Value: The time value
//   Times: Kind of the time value
//   IsUTCBased: Time value is a UTC value
function FTSetFileTime(
    const FileName: String;
    const Value: TWinFileTime;
    Times: TFileTimeKind;
    IsUTCBased: Boolean{$IFDEF SUPPORTS_DEFAULTPARAMS} = False{$ENDIF}
  ): Boolean;

// Set all times per TFileTime
//   Filename: Filename
//   LastWriteTime: Last write time value
//   LastAccessTime: Last access time value
//   CreationTime: Creation time value
//   IsUTCBased: Time value is a UTC value
function FTSetFileTimes(
    const FileName: String;
    const LastWriteFileTime, LastAccessFileTime, CreationFileTime: TWinFileTime;
    IsUTCBased: Boolean{$IFDEF SUPPORTS_DEFAULTPARAMS} = False{$ENDIF}
  ): Boolean;

// ***************************************  File Size  ***************************************

// Diese Funktionen unterstützen UNC-Pfadangaben

// return the total number of free bytes on the disk that are available to
// the user associated with the calling thread
function GetFreeSpace64(const Path: String): UInt64;

// return the total number of bytes on the disk that are available to the
// user associated with the calling thread
function GetTotalSpace64(const Path: String): UInt64;

// ****************************  Windows-spezifische Funktionen  *****************************

{$IFDEF WIN32}
// return the root (e.g. 'C:\')
function ExtractFileRoot(const Path: String): String;

// prüft, ob eine Diskette im Laufwerk liegt
function DiskInDrive(Drive: String): Boolean;

// gibt den langen Pfad zurück
function GetLongPathName(Path: String): String;
{$ENDIF WIN32}

implementation
uses
  {$IFNDEF RTL_XPLATFORM ~}
  FileCtrl,          // ForceDirectories
  {$ENDIF ~RTL_XPLATFORM}
  YclDTConversions;

{$IFNDEF RTL_XPLATFORM}
function IncludeTrailingPathDelimiter(const Value: String): String;
begin
  Result := Value;
  if Result <> '' then begin
    if Result[Length(Result)] <> OSPathDelimiter then
      Result := Result + OSPathDelimiter;
  end;
end;

function ExcludeTrailingPathDelimiter(const Value: String): String;
begin
  Result := Value;
  if Result <> '' then begin
    if Result[Length(Result)] = OSPathDelimiter then
      Delete(Result, Length(Result), 1);
  end;
end;

function ForceDirectories(Dir: String): Boolean;
begin
  Result := FileCtrl.ForceDirectories(Dir);
end;
{$ENDIF ~RTL_XPLATFORM}

procedure AddFileList(RootDir: String; List: TStrings);

  procedure ScanDir(const Dir: String);
  var
    Info: TSearchRec;
  begin
    if Dir <> '' then
      List.Add(Dir);
    if FindFirst(RootDir + Dir + '*.*', -1, Info) = 0 then
    try
      repeat
        if (Info.Attr and faDirectory) <> 0 then begin
          if (Info.Name <> '.') and (Info.Name <> '..') then
            ScanDir(Dir + Info.Name + OSPathDelimiter);
        end
        else
          List.Add(Dir + Info.Name);
      until FindNext(Info) <> 0;
    finally
      SysUtils.FindClose(Info);
    end;
  end;

begin
  RootDir := IncludeTrailingPathDelimiter(RootDir);
  ScanDir('');
end;

procedure GetFileList(RootDir: String; List: TStrings);
begin
  List.BeginUpdate;
  try
    List.Clear;
    AddFileList(RootDir, List);
  finally
    List.EndUpdate;
  end;
end;

function UnixPathToOSPath(const Value: String): String;
{$IFNDEF UNIX ~}
var
  i: Integer;
{$ENDIF ~UNIX}
begin
  Result := Value;
  {$IFNDEF UNIX ~}
  for i := 1 to Length(Result) do
  if Result[I] = UnixPathDelimiter then
    Result[I] := OSPathDelimiter;
  {$ENDIF ~UNIX}
end;

function OSPathToUnixPath(const Value: String): String;
{$IFNDEF UNIX ~}
var
  i: Integer;
{$ENDIF ~UNIX}
begin
  Result := Value;
  {$IFNDEF UNIX ~}
  for i := 1 to Length(Result) do
  if Result[I] = OSPathDelimiter then
    Result[I] := UnixPathDelimiter;
  {$ENDIF ~UNIX}
end;

function GetUnambiguousFilename(
    const Filename: String;
    MaxNumber: Cardinal{$IFDEF SUPPORTS_DEFAULTPARAMS} = High(Cardinal){$ENDIF}
  ): String;
var
  Path, Name, Ext: String;
  Number: Cardinal;
begin
  Path := ExcludeTrailingPathDelimiter(ExtractFilePath(Filename));
  // bei relativer Angabe, relativ zum gestarteten Programm
  if Path = '' then
    Path := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)))
  else
    Path := Path + OSPathDelimiter;
  Ext := ExtractFileExt(Filename);
  Name := ChangeFileExt(ExtractFilename(Filename), '');
  Result := Format('%s%s%s', [Path, Name, Ext]);
  if FileExists(Result) then begin
    Number := 1;
    repeat
      Result := Format('%s%s%d%s', [Path, Name, Number, Ext]);
      Inc(Number);
    until (not FileExists(Result)) or (Number > MaxNumber);
    if Number > MaxNumber then
      Result := '';
  end;
end;

function DirectoryExists(const DirName: String): Boolean;
var
  Info: TSearchRec;
begin
  Result := FindFirst(DirName, -1, Info) = 0;
  if Result then
  try
    Result := (Info.Attr and faDirectory) <> 0;
  finally
    SysUtils.FindClose(Info);
  end;
end;

// ***********************************  File Date / Time  ************************************

// Get last write time per TDataTime or TUnixTime
//   Filename: Filename
//   Value: Return the time value
//   IsUTCBased: Returned time value should be a UTC value
function GetFileLastWriteTime(
    const FileName: String;
    out Value: TDateTime;
    const IsUTCBased: Boolean{$IFDEF SUPPORTS_DEFAULTPARAMS} = False{$ENDIF}
  ): Boolean; overload;
begin
  Result := GetFileTime(Filename, Value, ftLastWrite, IsUTCBased);
end;

function GetFileLastWriteTime(
    const FileName: String;
    out Value: TUnixTime32;
    const IsUTCBased: Boolean{$IFDEF SUPPORTS_DEFAULTPARAMS} = False{$ENDIF}
  ): Boolean; overload;
begin
  Result := GetFileTime(Filename, Value, ftLastWrite, IsUTCBased);
end;

function GetFileLastWriteTime(
    const FileName: String;
    out Value: TUnixTime64;
    const IsUTCBased: Boolean{$IFDEF SUPPORTS_DEFAULTPARAMS} = False{$ENDIF}
  ): Boolean; overload;
begin
  Result := GetFileTime(Filename, Value, ftLastWrite, IsUTCBased);
end;

// Get last access time per TDataTime or TUnixTime
function GetFileLastAccessTime(
    const FileName: String;
    out Value: TDateTime;
    const IsUTCBased: Boolean{$IFDEF SUPPORTS_DEFAULTPARAMS} = False{$ENDIF}
  ): Boolean; overload;
begin
  Result := GetFileTime(Filename, Value, ftLastAccess, IsUTCBased);
end;

function GetFileLastAccessTime(
    const FileName: String;
    out Value: TUnixTime32;
    const IsUTCBased: Boolean{$IFDEF SUPPORTS_DEFAULTPARAMS} = False{$ENDIF}
  ): Boolean; overload;
begin
  Result := GetFileTime(Filename, Value, ftLastAccess, IsUTCBased);
end;

function GetFileLastAccessTime(
    const FileName: String;
    out Value: TUnixTime64;
    const IsUTCBased: Boolean{$IFDEF SUPPORTS_DEFAULTPARAMS} = False{$ENDIF}
  ): Boolean; overload;
begin
  Result := GetFileTime(Filename, Value, ftLastAccess, IsUTCBased);
end;

// Get creation time per TDataTime or TUnixTime
function GetFileCreationTime(
    const FileName: String;
    out Value: TDateTime;
    const IsUTCBased: Boolean{$IFDEF SUPPORTS_DEFAULTPARAMS} = False{$ENDIF}
  ): Boolean; overload;
begin
  Result := GetFileTime(Filename, Value, ftCreation, IsUTCBased);
end;

function GetFileCreationTime(
    const FileName: String;
    out Value: TUnixTime32;
    const IsUTCBased: Boolean{$IFDEF SUPPORTS_DEFAULTPARAMS} = False{$ENDIF}
  ): Boolean; overload;
begin
  Result := GetFileTime(Filename, Value, ftCreation, IsUTCBased);
end;

function GetFileCreationTime(
    const FileName: String;
    out Value: TUnixTime64;
    const IsUTCBased: Boolean{$IFDEF SUPPORTS_DEFAULTPARAMS} = False{$ENDIF}
  ): Boolean; overload;
begin
  Result := GetFileTime(Filename, Value, ftCreation, IsUTCBased);
end;

// Get all times per TDataTime or TUnixTime
//   Filename: Filename
//   LastWriteTime: Last write time value
//   LastAccessTime: Last access time value
//   CreationTime: Creation time value
//   IsUTCBased: Time value is a UTC value
function GetFileTimes(
    const FileName: String;
    out LastWriteTime, LastAccessTime, CreationTime: TDateTime;
    const IsUTCBased: Boolean{$IFDEF SUPPORTS_DEFAULTPARAMS} = False{$ENDIF}
  ): Boolean; overload;
var
  LastWriteFileTime, LastAccessFileTime, CreationFileTime: TWinFileTime;
begin
  Result := FTGetFileTimes(FileName, LastWriteFileTime, LastAccessFileTime, CreationFileTime,
      IsUTCBased);
  if Result then begin
    LastWriteTime  := LocalFileTimeToDateTime(LastWriteFileTime);
    LastAccessTime := LocalFileTimeToDateTime(LastAccessFileTime);
    CreationTime   := LocalFileTimeToDateTime(CreationFileTime);
  end;
end;

function GetFileTimes(
    const FileName: String;
    out LastWriteTime, LastAccessTime, CreationTime: TUnixTime32;
    const IsUTCBased: Boolean{$IFDEF SUPPORTS_DEFAULTPARAMS} = False{$ENDIF}
  ): Boolean; overload;
var
  LastWriteFileTime, LastAccessFileTime, CreationFileTime: TWinFileTime;
begin
  Result := FTGetFileTimes(FileName, LastWriteFileTime, LastAccessFileTime, CreationFileTime,
      IsUTCBased);
  if Result then begin
    LastWriteTime  := FileTimeToUnixTime(LastWriteFileTime);
    LastAccessTime := FileTimeToUnixTime(LastAccessFileTime);
    CreationTime   := FileTimeToUnixTime(CreationFileTime);
  end;
end;

function GetFileTimes(
    const FileName: String;
    out LastWriteTime, LastAccessTime, CreationTime: TUnixTime64;
    const IsUTCBased: Boolean{$IFDEF SUPPORTS_DEFAULTPARAMS} = False{$ENDIF}
  ): Boolean; overload;
var
  LastWriteFileTime, LastAccessFileTime, CreationFileTime: TWinFileTime;
begin
  Result := FTGetFileTimes(FileName, LastWriteFileTime, LastAccessFileTime, CreationFileTime,
      IsUTCBased);
  if Result then begin
    LastWriteTime  := FileTimeToUnixTime64(LastWriteFileTime);
    LastAccessTime := FileTimeToUnixTime64(LastAccessFileTime);
    CreationTime   := FileTimeToUnixTime64(CreationFileTime);
  end;
end;

// Set last write time per TDataTime or TUnixTime
//   Filename: Filename
//   Value: The time value
//   IsUTCBased: Time value is a UTC value
function SetFileLastWriteTime(
    const FileName: String;
    const Value: TDateTime;
    const IsUTCBased: Boolean{$IFDEF SUPPORTS_DEFAULTPARAMS} = False{$ENDIF}
  ): Boolean; overload;
begin
  Result := SetFileTime(Filename, Value, ftLastWrite, IsUTCBased);
end;

function SetFileLastWriteTime(
    const FileName: String;
    const Value: TUnixTime32;
    const IsUTCBased: Boolean{$IFDEF SUPPORTS_DEFAULTPARAMS} = False{$ENDIF}
  ): Boolean; overload;
begin
  Result := SetFileTime(Filename, Value, ftLastWrite, IsUTCBased);
end;

function SetFileLastWriteTime(
    const FileName: String;
    const Value: TUnixTime64;
    const IsUTCBased: Boolean{$IFDEF SUPPORTS_DEFAULTPARAMS} = False{$ENDIF}
  ): Boolean; overload;
begin
  Result := SetFileTime(Filename, Value, ftLastWrite, IsUTCBased);
end;

// Set last access time per TDataTime or TUnixTime
function SetFileLastAccessTime(
    const FileName: String;
    const Value: TDateTime;
    const IsUTCBased: Boolean{$IFDEF SUPPORTS_DEFAULTPARAMS} = False{$ENDIF}
  ): Boolean; overload;
begin
  Result := SetFileTime(Filename, Value, ftLastAccess, IsUTCBased);
end;

function SetFileLastAccessTime(
    const FileName: String;
    const Value: TUnixTime32;
    const IsUTCBased: Boolean{$IFDEF SUPPORTS_DEFAULTPARAMS} = False{$ENDIF}
  ): Boolean; overload;
begin
  Result := SetFileTime(Filename, Value, ftLastAccess, IsUTCBased);
end;

function SetFileLastAccessTime(
    const FileName: String;
    const Value: TUnixTime64;
    const IsUTCBased: Boolean{$IFDEF SUPPORTS_DEFAULTPARAMS} = False{$ENDIF}
  ): Boolean; overload;
begin
  Result := SetFileTime(Filename, Value, ftLastAccess, IsUTCBased);
end;

// Set creation time per TDataTime or TUnixTime
function SetFileCreationTime(
    const FileName: String;
    const Value: TDateTime;
    const IsUTCBased: Boolean{$IFDEF SUPPORTS_DEFAULTPARAMS} = False{$ENDIF}
  ): Boolean; overload;
begin
  Result := SetFileTime(Filename, Value, ftCreation, IsUTCBased);
end;

function SetFileCreationTime(
    const FileName: String;
    const Value: TUnixTime32;
    const IsUTCBased: Boolean{$IFDEF SUPPORTS_DEFAULTPARAMS} = False{$ENDIF}
  ): Boolean; overload;
begin
  Result := SetFileTime(Filename, Value, ftCreation, IsUTCBased);
end;

function SetFileCreationTime(
    const FileName: String;
    const Value: TUnixTime64;
    const IsUTCBased: Boolean{$IFDEF SUPPORTS_DEFAULTPARAMS} = False{$ENDIF}
  ): Boolean; overload;
begin
  Result := SetFileTime(Filename, Value, ftCreation, IsUTCBased);
end;

// Set all times per TDataTime or TUnixTime
//   Filename: Filename
//   LastWriteTime: Last write time value
//   LastAccessTime: Last access time value
//   CreationTime: Creation time value
//   IsUTCBased: Time value is a UTC value
function SetFileTimes(
    const FileName: String;
    const LastWriteTime, LastAccessTime, CreationTime: TDateTime;
    const IsUTCBased: Boolean{$IFDEF SUPPORTS_DEFAULTPARAMS} = False{$ENDIF}
  ): Boolean; overload;
var
  LastWriteFileTime, LastAccessFileTime, CreationFileTime: TWinFileTime;
begin
  LastWriteFileTime  := DateTimeToLocalFileTime(LastWriteTime);
  LastAccessFileTime := DateTimeToLocalFileTime(LastAccessTime);
  CreationFileTime   := DateTimeToLocalFileTime(CreationTime);
  Result := FTSetFileTimes(FileName, LastWriteFileTime, LastAccessFileTime, CreationFileTime,
      IsUTCBased);
end;

function SetFileTimes(
    const FileName: String;
    const LastWriteTime, LastAccessTime, CreationTime: TUnixTime32;
    const IsUTCBased: Boolean{$IFDEF SUPPORTS_DEFAULTPARAMS} = False{$ENDIF}
  ): Boolean; overload;
var
  LastWriteFileTime, LastAccessFileTime, CreationFileTime: TWinFileTime;
begin
  LastWriteFileTime := UnixTimeToFileTime(LastWriteTime);
  LastAccessFileTime := UnixTimeToFileTime(LastAccessTime);
  CreationFileTime := UnixTimeToFileTime(CreationTime);
  Result := FTSetFileTimes(FileName, LastWriteFileTime, LastAccessFileTime, CreationFileTime,
      IsUTCBased);
end;

function SetFileTimes(
    const FileName: String;
    const LastWriteTime, LastAccessTime, CreationTime: TUnixTime64;
    const IsUTCBased: Boolean{$IFDEF SUPPORTS_DEFAULTPARAMS} = False{$ENDIF}
  ): Boolean; overload;
var
  LastWriteFileTime, LastAccessFileTime, CreationFileTime: TWinFileTime;
begin
  LastWriteFileTime := UnixTime64ToFileTime(LastWriteTime);
  LastAccessFileTime := UnixTime64ToFileTime(LastAccessTime);
  CreationFileTime := UnixTime64ToFileTime(CreationTime);
  Result := FTSetFileTimes(FileName, LastWriteFileTime, LastAccessFileTime, CreationFileTime,
      IsUTCBased);
end;

// ****************************  File Date / Time  Single Value  *****************************

// Get time value per TDataTime or TUnixTime
//   Filename: Filename
//   Value: Return the time value
//   Times: Kind of the time value
//   IsUTCBased: Returned time value should be a UTC value
function GetFileTime(
    const Filename: String;
    out Value: TDateTime;
    Times: TFileTimeKind;
    IsUTCBased: Boolean{$IFDEF SUPPORTS_DEFAULTPARAMS} = False{$ENDIF}
  ): Boolean; overload;
var
  LocalFileTime: TWinFileTime;
begin
  Result := FTGetFileTime(Filename, LocalFileTime, Times, IsUTCBased);
  if Result then
    Value := LocalFileTimeToDateTime(LocalFileTime);
end;

function GetFileTime(
    const Filename: String;
    out Value: TUnixTime32;
    Times: TFileTimeKind;
    IsUTCBased: Boolean{$IFDEF SUPPORTS_DEFAULTPARAMS} = False{$ENDIF}
  ): Boolean; overload;
var
  LocalFileTime: TWinFileTime;
begin
  Result := FTGetFileTime(Filename, LocalFileTime, Times, IsUTCBased);
  if Result then
    Value := FileTimeToUnixTime(LocalFileTime);
end;

function GetFileTime(
    const Filename: String;
    out Value: TUnixTime64;
    Times: TFileTimeKind;
    IsUTCBased: Boolean{$IFDEF SUPPORTS_DEFAULTPARAMS} = False{$ENDIF}
  ): Boolean; overload;
var
  LocalFileTime: TWinFileTime;
begin
  Result := FTGetFileTime(Filename, LocalFileTime, Times, IsUTCBased);
  if Result then
    Value := FileTimeToUnixTime64(LocalFileTime);
end;

// Set time value per TDataTime or TUnixTime
//   Filename: Filename
//   Value: The time value
//   Times: Kind of the time value
//   IsUTCBased: Time value is a UTC value
function SetFileTime(
    const FileName: String;
    const Value: TDateTime;
    Times: TFileTimeKind;
    IsUTCBased: Boolean{$IFDEF SUPPORTS_DEFAULTPARAMS} = False{$ENDIF}
  ): Boolean; overload;
var
  LocalFileTime: TWinFileTime;
begin
  LocalFileTime := DateTimeToLocalFileTime(Value);
  Result := FTSetFileTime(Filename, LocalFileTime, Times, IsUTCBased);
end;

function SetFileTime(
    const FileName: String;
    const Value: TUnixTime32;
    Times: TFileTimeKind;
    IsUTCBased: Boolean{$IFDEF SUPPORTS_DEFAULTPARAMS} = False{$ENDIF}
  ): Boolean; overload;
var
  LocalFileTime: TWinFileTime;
begin
  LocalFileTime := UnixTimeToFileTime(Value);
  Result := FTSetFileTime(Filename, LocalFileTime, Times, IsUTCBased);
end;

function SetFileTime(
    const FileName: String;
    const Value: TUnixTime64;
    Times: TFileTimeKind;
    IsUTCBased: Boolean{$IFDEF SUPPORTS_DEFAULTPARAMS} = False{$ENDIF}
  ): Boolean; overload;
var
  LocalFileTime: TWinFileTime;
begin
  LocalFileTime := UnixTime64ToFileTime(Value);
  Result := FTSetFileTime(Filename, LocalFileTime, Times, IsUTCBased);
end;

// ******************************  File Date / Time  TFileTime  ******************************

// Get time value per TFileTime
//   Filename: Filename
//   Value: Return the time value
//   Times: Kind of the time value
//   IsUTCBased: Returned time value should be a UTC value
function FTGetFileTime(
    const Filename: String;
    out Value: TWinFileTime;
    Times: TFileTimeKind;
    IsUTCBased: Boolean{$IFDEF SUPPORTS_DEFAULTPARAMS} = False{$ENDIF}
  ): Boolean;
{$IFDEF WIN32}
var
  SearchHandle: THandle;
  Win32FindData: TWin32FindData;
  FileTime: TFileTime;
begin
  SearchHandle := FindFirstFile(PChar(FileName), Win32FindData);
  Result := SearchHandle <> INVALID_HANDLE_VALUE;
  if not Result then
    Exit;
  Windows.FindClose(SearchHandle);
  case Times of
    ftLastAccess: FileTime := Win32FindData.ftLastAccessTime;
    ftLastWrite:  FileTime := Win32FindData.ftLastWriteTime;
    ftCreation:   FileTime := Win32FindData.ftCreationTime;
  else
    Result := False;
    Exit;
  end;
  // keine Umrechnugn auf lokale Zeit
  if IsUTCBased then begin
    Value := FileTime;
    Result := True;
  end
  // auf lokale Zeit umrechnen
  else begin
    Result := FileTimeToLocalFileTime(FileTime, Value);
  end;
end;
{$ENDIF WIN32}
{$IFDEF UNIX}
begin
  { TODO : implementieren }
end;
{$ENDIF UNIX}

// Get all times per TFileTime
//   Filename: Filename
//   LastWriteTime: Last write time value
//   LastAccessTime: Last access time value
//   CreationTime: Creation time value
//   IsUTCBased: Time value is a UTC value
function FTGetFileTimes(
    const FileName: String;
    out LastWriteFileTime, LastAccessFileTime, CreationFileTime: TWinFileTime;
    IsUTCBased: Boolean{$IFDEF SUPPORTS_DEFAULTPARAMS} = False{$ENDIF}
  ): Boolean;
{$IFDEF WIN32}
var
  SearchHandle: THandle;
  Win32FindData: TWin32FindData;
begin
  SearchHandle := FindFirstFile(PChar(FileName), Win32FindData);
  Result := SearchHandle <> INVALID_HANDLE_VALUE;
  if not Result then
    Exit;
  Windows.FindClose(SearchHandle);
  // keine Umrechnugn auf lokale Zeit
  if IsUTCBased then begin
    CreationFileTime   := Win32FindData.ftCreationTime;
    LastAccessFileTime := Win32FindData.ftLastAccessTime;
    LastWriteFileTime  := Win32FindData.ftLastWriteTime;
    Result := True;
  end
  // auf lokale Zeit umrechnen
  else begin
    Result := FileTimeToLocalFileTime(Win32FindData.ftCreationTime,   CreationFileTime) and
              FileTimeToLocalFileTime(Win32FindData.ftLastAccessTime, LastAccessFileTime) and
              FileTimeToLocalFileTime(Win32FindData.ftLastWriteTime,  LastWriteFileTime);
  end;
end;
{$ENDIF WIN32}
{$IFDEF UNIX}
begin
  { TODO : implementieren }
end;
{$ENDIF UNIX}

// Set time value per TFileTime
//   Filename: Filename
//   Value: The time value
//   Times: Kind of the time value
//   IsUTCBased: Time value is a UTC value
function FTSetFileTime(
    const FileName: String;
    const Value: TWinFileTime;
    Times: TFileTimeKind;
    IsUTCBased: Boolean{$IFDEF SUPPORTS_DEFAULTPARAMS} = False{$ENDIF}
  ): Boolean;
{$IFDEF WIN32}
var
  Handle: THandle;
  FileTime: TFileTime;
begin
  Result := False;
  Handle := CreateFile(PChar(FileName), GENERIC_WRITE, FILE_SHARE_READ,
                       Nil, OPEN_EXISTING, 0, 0);
  if Handle <> INVALID_HANDLE_VALUE then
  try
    // keine Umrechnugn von lokaler Zeit
    if IsUTCBased then begin
      FileTime := Value;
      Result := True;
    end
    // von lokaler Zeit umrechnen
    else begin
      Result := LocalFileTimeToFileTime(Value, FileTime);
    end;
    if Result then begin
      case Times of
        ftLastAccess:
          Result := Windows.SetFileTime(Handle, Nil, @FileTime, Nil);
        ftLastWrite:
          Result := Windows.SetFileTime(Handle, Nil, Nil, @FileTime);
        ftCreation:
          Result := Windows.SetFileTime(Handle, @FileTime, Nil, Nil);
      else
        Result := False;
      end;
    end;
  finally
    CloseHandle(Handle);
  end;
end;
{$ENDIF WIN32}
{$IFDEF UNIX}
begin
  { TODO : implementieren }
end;
{$ENDIF UNIX}

// Set all times per TFileTime
//   Filename: Filename
//   LastWriteTime: Last write time value
//   LastAccessTime: Last access time value
//   CreationTime: Creation time value
//   IsUTCBased: Time value is a UTC value
function FTSetFileTimes(
    const FileName: String;
    const LastWriteFileTime, LastAccessFileTime, CreationFileTime: TWinFileTime;
    IsUTCBased: Boolean{$IFDEF SUPPORTS_DEFAULTPARAMS} = False{$ENDIF}
  ): Boolean;
{$IFDEF WIN32}
var
  Handle: THandle;
  UTCLastWriteFileTime, UTCLastAccessFileTime, UTCCreationFileTime: TFileTime;
begin
  Result := False;
  Handle := CreateFile(PChar(FileName), GENERIC_WRITE, FILE_SHARE_READ,
                       Nil, OPEN_EXISTING, 0, 0);
  if Handle <> INVALID_HANDLE_VALUE then
  try
    if IsUTCBased then begin
      UTCLastWriteFileTime := LastWriteFileTime;
      UTCLastAccessFileTime := LastAccessFileTime;
      UTCCreationFileTime := CreationFileTime;
      Result := True;
    end
    else begin
      Result := LocalFileTimeToFileTime(LastWriteFileTime, UTCLastWriteFileTime) and
                LocalFileTimeToFileTime(LastAccessFileTime, UTCLastAccessFileTime) and
                LocalFileTimeToFileTime(CreationFileTime, UTCCreationFileTime);
    end;
    if Result then begin
      Result := Windows.SetFileTime(Handle, @UTCCreationFileTime, @UTCLastAccessFileTime,
          @UTCLastWriteFileTime);
    end;
  finally
    CloseHandle(Handle);
  end;
end;
{$ENDIF WIN32}
{$IFDEF UNIX}
begin
  { TODO : implementieren }
end;
{$ENDIF UNIX}

// ***************************************  File Size  ***************************************

procedure GetSpace64(const Path: String; var TotalBytes, FreeBytes: UInt64);
{$IFDEF WIN32}
begin
  if not SysUtils.GetDiskFreeSpaceEx(Pointer(ExtractFileRoot(Path)), FreeBytes, TotalBytes, Nil) then
    RaiseLastOSError;
end;
{$ENDIF WIN32}
{$IFDEF UNIX}
begin
  { TODO : implementieren }
end;
{$ENDIF UNIX}

function GetFreeSpace64(const Path: String): UInt64;
var
  x: UInt64;
begin
  GetSpace64(Path, x, Result);
end;

function GetTotalSpace64(const Path: String): UInt64;
var
  x: UInt64;
begin
  GetSpace64(Path, Result, x);
end;

// ****************************  Windows-spezifische Funktionen  *****************************

{$IFDEF WIN32}
// return the root (e.g. 'C:\')
function ExtractFileRoot(const Path: String): String;
begin
  Result := ExtractFileDrive(Path);
  // add trailing backslash
  if Result <> '' then begin
    if Result[Length(Result)] <> '\' then
      Result := Result + '\';
  end;
end;

function DiskInDrive(Drive: String): Boolean;
var
  ErrorMode: DWord;
  MaximumComponentLength, FileSystemFlags: DWord;
begin
  Drive := ExtractFileRoot(Drive);
  Result := Length(Drive) > 0;
  if not Result then
    Exit;
  ErrorMode := SetErrorMode(SEM_FAILCRITICALERRORS);
  try
    Result := GetVolumeInformation(
        Pointer(Drive), Nil, 0, Nil, MaximumComponentLength, FileSystemFlags, Nil, 0);
  finally
    SetErrorMode(ErrorMode);
  end;
end;

{ TODO : optimalere Version }
function GetLongPathName(Path: String): String;
var
  i: Integer;
  SearchHandle: THandle;
  FindData: TWin32FindData;
  IsBackSlash: Boolean;
begin
  Path := ExpandFileName(Path);
  Result := ExtractFileDrive(Path);
  i := Length(Result);
  if Length(Path) <= I then   // only drive
     Exit;
  if Path[I + 1] = '\' then begin
    Result := Result + '\';
    Inc(I);
  end;
  Delete(Path, 1, I);
  repeat
    I := Pos('\', Path);
    IsBackSlash := I > 0;
    if not IsBackSlash then
      I := Length(Path) + 1;
    SearchHandle := FindFirstFile(PChar(Result + Copy(Path, 1, I - 1)), FindData);
    if SearchHandle <> INVALID_HANDLE_VALUE then begin
      try
        Result := Result + FindData.cFileName;
        if IsBackSlash then
          Result := Result + '\';
      finally
        Windows.FindClose(SearchHandle);
      end;
    end
    else begin
      Result := Result + Path;
      Break;
    end;
    Delete(Path, 1, I);
  until Length(Path) = 0;
end;
{$ENDIF WIN32}

// *******************************************************************************************

//  History:
//  2005-07-29, Peter J. Haas
//   - move RaiseLastOSError from YclFileUtils to YclBase
//
//  2005-07-08, Peter J. Haas
//   - add GetSpace64, GetFreeSpace64, GetTotalSpace64 (from DiskSpace64)
// 
//  2005-07-05, Peter J. Haas
//   - add DiskInDrive, DirectoryExists (from other unit)
//
//  2005-02-19, Peter J. Haas
//   - add GetFileTimes, FTGetFileTimes
//   - rename FTGetFileDateTime to GetFileTime
//   - rename FTSetFileDateTime to SetFileTime
//   - add GetFileLastWriteTime, GetFileLastAccessTime, GetFileCreationTime, GetFileTimes
//         SetFileLastWriteTime, SetFileLastAccessTime, SetFileCreationTime, SetFileTimes
//         GetFileTime, SetFileTime overloads for TUnixTime32 and TUnixTime64
//
//  2005-02-09, Peter J. Haas
//   - YCL version
//
//  2003-10-18, Peter J. Haas (file time functions)
//   - add comments
//   - add SetFileTimes and FTSetFileTimes
//
//  2001-09-24, Peter J. Haas (file time functions)
//   - BugFix: Insert own JEDI.INC
//
//  2001-09-23, Peter J. Haas (file time functions)
//   - rename FileTimes to PJHFileTimes
//   - move LocalFileTimeToDateTime and DateTimeToLocalFileTime to unit PJHDateTimes
//
//  2001-09-17, Peter J. Haas (file time functions)
//   - First version

//  2001-04-10, Peter J. Haas (DiskSpace64 Version 2.0)
//   - add support for UNC in Delphi 5
//
//  2000-04-08, Peter J. Haas (DiskSpace64 Version 1.0)
//   - final release, without Changes
//
//  2000-03-25, Peter J. Haas (DiskSpace64 Version 0.9 beta)
//   - First version

end.
