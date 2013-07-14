{**************************************************************************************************}
{                                                                                                  }
{  Y Windows Api library                                                                           }
{                                                                                                  }
{  ShFolder interface unit                                                                         }
{                                                                                                  }
{  The contents of this file are subject to the Y Library Public License Version 1.0 (the          }
{  "License"); you may not use this file except in compliance with the License. You may obtain a   }
{  copy of the License at http://delphi.pjh2.de/                                                   }
{                                                                                                  }
{  Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF  }
{  ANY KIND, either express or implied. See the License for the specific language governing        }
{  rights and limitations under the License.                                                       }
{                                                                                                  }
{  The Original Code is: ShFolder.h, 2005-04-14.                                                   }
{  The Initial Developer of the Original Code is Microsoft. Portions created by Microsoft are      }
{  Copyright (C) Microsoft Corporation. All Rights Reserved.                                       }
{                                                                                                  }
{  The Original Pascal code is: ShFolder.pas.                                                      }
{  The Initial Developer of the Original Pascal code is Peter J. Haas (libs@pjh2.de). Portions     }
{  created by Peter J. Haas are Copyright (C) 2000-2005 Peter J. Haas. All Rights Reserved.        }
{                                                                                                  }
{  Contributor(s):                                                                                 }
{                                                                                                  }
{  You may retrieve the latest version of this file at the homepage of Peter J. Haas, located at   }
{  http://delphi.pjh2.de/                                                                          }
{                                                                                                  }
{**************************************************************************************************}

// For history see end of file

{$I YlWinApi.inc}

unit YwalShFolder;

interface
uses
  Windows;

(*$HPPEMIT '' *)
(*$HPPEMIT '#include <ShFolder.h>' *)
(*$HPPEMIT '' *)

// functions to get shell special folders/
// shfolder.dll supports these on all platforms including Win95, Win98, NT4 and IE4 shell

// all CSIDL values referred to here are supported natively by shfolder.dll, that is they
// will work on all platforms.

const
  {$EXTERNALSYM CSIDL_PERSONAL}
  CSIDL_PERSONAL             = $0005;  // My Documents
  {$EXTERNALSYM CSIDL_MYMUSIC}
  CSIDL_MYMUSIC              = $000d;  // "My Music" folder
  {$EXTERNALSYM CSIDL_APPDATA}
  CSIDL_APPDATA              = $001A;  // Application Data, new for NT4
  {$EXTERNALSYM CSIDL_LOCAL_APPDATA}
  CSIDL_LOCAL_APPDATA        = $001C;  // non roaming, user\Local Settings\Application Data
  {$EXTERNALSYM CSIDL_INTERNET_CACHE}
  CSIDL_INTERNET_CACHE       = $0020;
  {$EXTERNALSYM CSIDL_COOKIES}
  CSIDL_COOKIES              = $0021;
  {$EXTERNALSYM CSIDL_HISTORY}
  CSIDL_HISTORY              = $0022;
  {$EXTERNALSYM CSIDL_COMMON_APPDATA}
  CSIDL_COMMON_APPDATA       = $0023;  // All Users\Application Data
  {$EXTERNALSYM CSIDL_WINDOWS}
  CSIDL_WINDOWS              = $0024;  // GetWindowsDirectory()
  {$EXTERNALSYM CSIDL_SYSTEM}
  CSIDL_SYSTEM               = $0025;  // GetSystemDirectory()
  {$EXTERNALSYM CSIDL_PROGRAM_FILES}
  CSIDL_PROGRAM_FILES        = $0026;  // C:\Program Files
  {$EXTERNALSYM CSIDL_MYPICTURES}
  CSIDL_MYPICTURES           = $0027;  // My Pictures, new for Win2K
  {$EXTERNALSYM CSIDL_PROGRAM_FILES_COMMON}
  CSIDL_PROGRAM_FILES_COMMON = $002b;  // C:\Program Files\Common
  {$EXTERNALSYM CSIDL_COMMON_DOCUMENTS}
  CSIDL_COMMON_DOCUMENTS     = $002e;  // All Users\Documents
  {$EXTERNALSYM CSIDL_RESOURCES}
  CSIDL_RESOURCES            = $0038;  // %windir%\Resources\, For theme and other windows resources.
  {$EXTERNALSYM CSIDL_RESOURCES_LOCALIZED}
  CSIDL_RESOURCES_LOCALIZED  = $0039;  // %windir%\Resources\<LangID>, for theme and other windows specific resources.

  {$EXTERNALSYM CSIDL_FLAG_CREATE}
  CSIDL_FLAG_CREATE          = $8000;  // new for Win2K, or this in to force creation of folder

  {$EXTERNALSYM CSIDL_COMMON_ADMINTOOLS}
  CSIDL_COMMON_ADMINTOOLS    = $002f;  // All Users\Start Menu\Programs\Administrative Tools
  {$EXTERNALSYM CSIDL_ADMINTOOLS}
  CSIDL_ADMINTOOLS           = $0030;  // <user name>\Start Menu\Programs\Administrative Tools

{$EXTERNALSYM SHGetFolderPathA}
function SHGetFolderPathA(hwnd: HWND; csidl: Integer; hToken: THandle; dwFlags: DWORD; pszPath: PAnsiChar): HResult; stdcall;
{$EXTERNALSYM SHGetFolderPathW}
function SHGetFolderPathW(hwnd: HWND; csidl: Integer; hToken: THandle; dwFlags: DWORD; pszPath: PWideChar): HResult; stdcall;
{$EXTERNALSYM SHGetFolderPath}
function SHGetFolderPath(hwnd: HWND; csidl: Integer; hToken: THandle; dwFlags: DWORD; pszPath: PChar): HResult; stdcall;

// protos so callers can GetProcAddress() from shfolder.dll
type
  {$EXTERNALSYM PFNSHGETFOLDERPATHA}
  PFNSHGETFOLDERPATHA = function(hwnd: HWND; csidl: Integer; hToken: THandle; dwFlags: DWORD; pszPath: PAnsiChar): HResult; stdcall;
  {$EXTERNALSYM PFNSHGETFOLDERPATHW}
  PFNSHGETFOLDERPATHW = function(hwnd: HWND; csidl: Integer; hToken: THandle; dwFlags: DWORD; pszPath: PWideChar): HResult; stdcall;
  {$EXTERNALSYM PFNSHGETFOLDERPATH}
  PFNSHGETFOLDERPATH = PFNSHGETFOLDERPATHA;
  TSHGetFolderPathFuncA = PFNSHGETFOLDERPATHA;
  TSHGetFolderPathFuncW = PFNSHGETFOLDERPATHW;
  TSHGetFolderPathFunc = TSHGetFolderPathFuncA;

implementation

const
  ShFolderDLL = 'ShFolder.dll';

function SHGetFolderPathA; external ShFolderDLL name 'SHGetFolderPathA';
function SHGetFolderPathW; external ShFolderDLL name 'SHGetFolderPathW';
function SHGetFolderPath; external ShFolderDLL name 'SHGetFolderPathA';

// *******************************************************************************************

//  History:
//  2005-08-26, Peter J. Haas
//   - SDK 2005-04, Windows Server 2003 SP1 (unchanged)
//
//  2003-08-05, Peter J. Haas
//   - SDK 2003-02, Windows Server 2003
//
//  2000-04-09, Peter J. Haas
//   - first version

end.
