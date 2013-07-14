{**************************************************************************************************}
{                                                                                                  }
{  Y core library (Ycl)                                                                            }
{                                                                                                  }
{  Windows - Runtime dynamic linking support                                                       }
{                                                                                                  }
{  The contents of this file are subject to the Y Library Public License Version 1.0 (the          }
{  "License"); you may not use this file except in compliance with the License. You may obtain a   }
{  copy of the License at http://delphi.pjh2.de/                                                   }
{                                                                                                  }
{  Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF  }
{  ANY KIND, either express or implied. See the License for the specific language governing rights }
{  and limitations under the License.                                                              }
{                                                                                                  }
{  The Original Code is: YclWinRtdl.pas.                                                           }
{  The Initial Developer of the Original Code is Peter J. Haas (libs@pjh2.de). Portions created    }
{  by Peter J. Haas are Copyright (C) 2003-2005 Peter J. Haas. All Rights Reserved.                }
{                                                                                                  }
{  Contributor(s):                                                                                 }
{                                                                                                  }
{  You may retrieve the latest version of this file at the homepage of Peter J. Haas, located at   }
{  http://delphi.pjh2.de/                                                                          }
{                                                                                                  }
{**************************************************************************************************}

{$I Ycl.inc}

unit YclWinRtdl;

interface
uses
  Windows;

function YclLoadLibrary(LibFileName: LPCTSTR; var LibHandle: HModule): HModule;
function YclGetModuleHandle(LibFileName: LPCTSTR; var LibHandle: HModule): HModule;
procedure YclFreeLibrary(LibHandle: HModule);
function YclGetProcAddress(LibHandle: HModule; ProcName: LPCSTR; var Call: Pointer): Boolean;
function YclGetProcAddressResult(LibHandle: HModule; ProcName: LPCSTR; var Call: Pointer): HResult;
function YclGetProcAddressInt(LibHandle: HModule; ProcName: LPCSTR; var Call: Pointer): Integer;

implementation

const
  CallNotImplemented = Pointer(-1);

function YclLoadLibrary(LibFileName: LPCTSTR; var LibHandle: HModule): HModule;
begin
  if LibHandle = 0 then begin
    LibHandle := LoadLibrary(LibFileName);
    if LibHandle = 0 then
      LibHandle := INVALID_HANDLE_VALUE;
  end;
  Result := LibHandle;
end;

function YclGetModuleHandle(LibFileName: LPCTSTR; var LibHandle: HModule): HModule;
begin
  if LibHandle = 0 then begin
    LibHandle := GetModuleHandle(LibFileName);
    if LibHandle = 0 then
      LibHandle := INVALID_HANDLE_VALUE;
  end;
  Result := LibHandle;
end;

procedure YclFreeLibrary(LibHandle: HModule);
begin
  case LibHandle of
    0, INVALID_HANDLE_VALUE: ;
  else
    FreeLibrary(LibHandle);
  end;
end;

function YclGetProcAddress(LibHandle: HModule; ProcName: LPCSTR; var Call: Pointer): Boolean;
begin
  if not Assigned(Call) then begin
    case LibHandle of
      0, INVALID_HANDLE_VALUE:   // 0 to be on the safe side
        Call := CallNotImplemented;
    else
      Call := GetProcAddress(LibHandle, ProcName);
      if not Assigned(Call) then
        Call := CallNotImplemented;
    end;
  end;
  Result := Call <> CallNotImplemented;
end;

function YclGetProcAddressResult(LibHandle: HModule; ProcName: LPCSTR; var Call: Pointer): HResult;
begin
  if YclGetProcAddress(LibHandle, ProcName, Call) then
    Result := ERROR_SUCCESS
  else
    Result := ERROR_CALL_NOT_IMPLEMENTED;
end;

function YclGetProcAddressBool(LibHandle: HModule; ProcName: LPCSTR; var Call: Pointer): BOOL;
begin
  Result := YclGetProcAddress(LibHandle, ProcName, Call);
  if Result then
    SetLastError(ERROR_SUCCESS)
  else
    SetLastError(ERROR_CALL_NOT_IMPLEMENTED);
end;

function YclGetProcAddressInt(LibHandle: HModule; ProcName: LPCSTR; var Call: Pointer): Integer;
begin
  Result := Integer(YclGetProcAddressBool(LibHandle, ProcName, Call));
end;

end.
