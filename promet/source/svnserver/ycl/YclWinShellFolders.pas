{**************************************************************************************************}
{                                                                                                  }
{  Y core library (Ycl)                                                                            }
{                                                                                                  }
{  YclWinShellFolders - Windows shell folder utility functions                                     }
{                                                                                                  }
{  The contents of this file are subject to the Y Library Public License Version 1.0 (the          }
{  "License"); you may not use this file except in compliance with the License. You may obtain a   }
{  copy of the License at http://delphi.pjh2.de/                                                   }
{                                                                                                  }
{  Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF  }
{  ANY KIND, either express or implied. See the License for the specific language governing rights }
{  and limitations under the License.                                                              }
{                                                                                                  }
{  The Original Code is: YclWinShellFolders.pas.                                                   }
{  The Initial Developer of the Original Code is Peter J. Haas (libs@pjh2.de). Portions created    }
{  by Peter J. Haas are Copyright (C) 2000-2005 Peter J. Haas. All Rights Reserved.                }
{                                                                                                  }
{  Contributor(s):                                                                                 }
{                                                                                                  }
{  You may retrieve the latest version of this file at the homepage of Peter J. Haas, located at   }
{  http://delphi.pjh2.de/                                                                          }
{                                                                                                  }
{**************************************************************************************************}

// For history see end of file

{$I Ycl.inc}

unit YclWinShellFolders;

interface
uses
  Windows, YwalShFolder;

// CSIDL -> Path
// To get extended error information, call GetLastError
// ERROR_CALL_NOT_IMPLEMENTED do mean, SHGetFolderPath is not available
function ShellGetFolderPathA(CSIDL: Integer; out Path: AnsiString; Default: Boolean = False): Boolean;
function ShellGetFolderPathW(CSIDL: Integer; out Path: WideString; Default: Boolean = False): Boolean;
function ShellGetFolderPath(CSIDL: Integer; out Path: String; Default: Boolean = False): Boolean;

implementation
uses
  YclWinRtdl;
  
const
  Shell32DLLName  = 'Shell32.dll';
  ShFolderDLLName = 'ShFolder.dll';
  SHGetFolderPathNameA = 'SHGetFolderPathA';
  SHGetFolderPathNameW = 'SHGetFolderPathW';

var
  ShFolderDLLHandle: HModule = 0;
  SHGetFolderPathA: TSHGetFolderPathFuncA = Nil;
  SHGetFolderPathW: TSHGetFolderPathFuncW = Nil;

// from ShlObj.h
const
  SHGFP_TYPE_CURRENT = 0;   // current value for user, verify it exists
  SHGFP_TYPE_DEFAULT = 1;   // default value, may not exist

procedure InitShGetFolderPath;
begin
  if ShFolderDLLHandle = 0 then begin
    // try Shell32
    ShFolderDLLHandle := LoadLibrary(Shell32DLLName);
    if ShFolderDLLHandle <> 0 then begin
      SHGetFolderPathA := GetProcAddress(ShFolderDLLHandle, SHGetFolderPathNameA);
      SHGetFolderPathW := GetProcAddress(ShFolderDLLHandle, SHGetFolderPathNameW);
    end;
    // if not, then try the migration DLL
    if not Assigned(SHGetFolderPathA) then begin
      if ShFolderDLLHandle <> 0 then
        FreeLibrary(ShFolderDLLHandle);
      ShFolderDLLHandle := LoadLibrary(ShFolderDLLName);
      if ShFolderDLLHandle <> 0 then begin
        SHGetFolderPathA := GetProcAddress(ShFolderDLLHandle, SHGetFolderPathNameA);
        SHGetFolderPathW := GetProcAddress(ShFolderDLLHandle, SHGetFolderPathNameW);
      end
      else begin
        SHGetFolderPathA := Nil;
        SHGetFolderPathW := Nil;
      end;
      if not Assigned(SHGetFolderPathA) and (ShFolderDLLHandle <> 0) then begin
        FreeLibrary(ShFolderDLLHandle);
        ShFolderDLLHandle := INVALID_HANDLE_VALUE;
      end;
    end;
  end;
end;

const
  Flags: array[Boolean] of Integer = (SHGFP_TYPE_CURRENT, SHGFP_TYPE_DEFAULT);

function ShellGetFolderPathA(CSIDL: Integer; out Path: AnsiString; Default: Boolean = False): Boolean;
var
  Buffer: array[0..MAX_PATH] of AnsiChar;
  Res: HResult;
begin
  Path := '';
  InitSHGetFolderPath;
  Result := Assigned(SHGetFolderPathA);
  if not Result then begin
    SetLastError(ERROR_CALL_NOT_IMPLEMENTED);
    Exit;
  end;
  FillChar(Buffer, SizeOf(Buffer), 0);
  Res := SHGetFolderPathA(0, CSIDL, 0, Flags[Default], Buffer);
  SetLastError(Res);
  Result := Res = S_OK;
  if Result then
    Path := Buffer;
end;

function ShellGetFolderPathW(CSIDL: Integer; out Path: WideString; Default: Boolean = False): Boolean;
var
  Buffer: array[0..MAX_PATH] of WideChar;
  Res: HResult; 
begin
  Path := '';
  InitSHGetFolderPath;
  Result := Assigned(SHGetFolderPathW);
  if not Result then begin
    SetLastError(ERROR_CALL_NOT_IMPLEMENTED);
    Exit;
  end;
  FillChar(Buffer, SizeOf(Buffer), 0);
  Res := SHGetFolderPathW(0, CSIDL, 0, Flags[Default], Buffer);
  SetLastError(Res);
  Result := Res = S_OK;
  if Result then
    Path := Buffer;
end;

function ShellGetFolderPath(CSIDL: Integer; out Path: String; Default: Boolean = False): Boolean;
begin
  Result := ShellGetFolderPathA(CSIDL, Path, Default);
end;

initialization
finalization
  SHGetFolderPathA := Nil;
  SHGetFolderPathW := Nil;
  YclFreeLibrary(ShFolderDLLHandle);
  ShFolderDLLHandle := 0;

// *******************************************************************************************

//  History:
//  2005-08-26, Peter J. Haas
//   - YCL version
//   - use YwalShFolder
//   - use YclWinRtdl
//
//  2003-08-05, Peter J. Haas
//   - own, up-to-date SHFolder.pas
//   - Default parameter
//   - ANSI and Unicode                                                      
//
//  2001-07-27, Peter J. Haas
//   - rename to SHFolders.pas                                               
//   - use external ShFolder interface unit
//
//  2000-04-09, Peter J. Haas
//   - first version (SHFolder.pas)

end.
