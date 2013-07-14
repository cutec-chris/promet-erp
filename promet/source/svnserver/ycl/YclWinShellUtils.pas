{**************************************************************************************************}
{                                                                                                  }
{  Y core library (Ycl)                                                                            }
{                                                                                                  }
{  YclWinShellUtils - Windows shell utility functions                                              }
{                                                                                                  }
{  The contents of this file are subject to the Y Library Public License Version 1.0 (the          }
{  "License"); you may not use this file except in compliance with the License. You may obtain a   }
{  copy of the License at http://delphi.pjh2.de/                                                   }
{                                                                                                  }
{  Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF  }
{  ANY KIND, either express or implied. See the License for the specific language governing rights }
{  and limitations under the License.                                                              }
{                                                                                                  }
{  The Original Code is: YclWinShellUtils.pas.                                                     }
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

unit YclWinShellUtils;

interface
uses
  Windows, Messages, ShellAPI,
  SysUtils, Classes;

// alle Verzeichnisangaben ohne abschließenden Backslash

// kopiert alle Dateien von SrcDir -> DstDir
function ShellCopyDirContents(
    Handle: HWnd; const SrcDir, DstDir: String; SubDirs: Boolean; Flags: Word): Boolean;
// kopiert die in FileList angegebenen Dateien von SrcDir -> DstDir
function ShellCopyFileList(
    Handle: HWnd; const SrcDir, DstDir: String; FileList: array of String; Flags: Word): Boolean;

// verschiebt alle Dateien von SrcDir -> DstDir
function ShellMoveDirContents(
    Handle: HWnd; const SrcDir, DstDir: String; SubDirs: Boolean; Flags: Word): Boolean;
// verschiebt die in FileList angegebenen Dateien von SrcDir -> DstDir
function ShellMoveFileList(
    Handle: HWnd; const SrcDir, DstDir: String; FileList: array of String; Flags: Word): Boolean;
                           
// benennt Verzeichnis um
function ShellRenameDir(
    Handle: HWnd; const OldName, NewName: String; Flags: Word): Boolean;

// Leert ein Verzeichnis
function ShellDeleteDirContents(
    Handle: HWnd; const Dir: String; SubDirs: Boolean; Flags: Word): Boolean;
// Löscht Files (Maske) aus Verzeichnis Dir
function ShellDeleteFiles(
    Handle: HWnd; const Dir, Files: String; SubDirs: Boolean; Flags: Word): Boolean;
// löscht die in FileList angegebenen Dateien in Dir
function ShellDeleteFileList(
    Handle: HWnd; const Dir: String; FileList: array of String; Flags: Word): Boolean;
// löscht ein Verzeichnis komplett
function ShellDeleteDir(
    Handle: HWnd; const Dir: String; Flags: Word): Boolean;

// ****************************************  Process  ****************************************

// return False, if error
function WaitForEndOfProcess(ProcessHandle: THandle): Boolean;
// return the ExitCode
function ExecuteFile(const Filename: String; Parameter, CurrentDir: String; Wait: Boolean): Cardinal;

// *************************************  Kommandozeile  *************************************

// existiert single char option?
function CommandlineSingleCharOptionExists(Option: Char): Boolean;
// Index einer single char option; gibt 0 zurück, wenn sie nicht existiert
function CommandlineSingleCharOptionIndex(Option: Char): Integer;
// Wert einer single char option
// gibt False zurück, wenn sie nicht existiert, Value dann ''
function CommandlineSingleCharOptionValue(Option: Char; out Value: String): Boolean;
// Wert einer single char option, bei Index 0 bleibt Value unverändert
procedure CommandlineSingleCharOptionValueByIndex(Index: Integer; var Value: String);

implementation
uses
  YclStrings, YclFileUtils;

function WildCardFileExists(const AFilename: String): Boolean;
var
  Handle: THandle;
  FindData: TWin32FindData;
begin
  Handle := FindFirstFile(PChar(AFilename), FindData);
  Result := Handle <> INVALID_HANDLE_VALUE;
  if Result then
    Windows.FindClose(Handle);
end;

function ShellFileOperation(
    Handle: HWnd; Func: Integer; FromPath, ToPath: PChar; Flags: Word; Size: Integer): Boolean; overload;
var
  Info: TSHFileOpStruct;
begin
  Result := False;
  if FromPath = Nil then begin
    if Size < 0 then
      Result := True;
  end
  else try
    FillChar(Info, SizeOf(Info), 0);
    Info.Wnd := Handle;
    Info.wFunc := Func;
    Info.pFrom := FromPath;
    Info.pTo := ToPath;
    Info.fFlags := Flags;
    Info.fAnyOperationsAborted := False;
    Info.hNameMappings := Nil;
    Info.lpszProgressTitle := Nil;  // only used if FOF_SIMPLEPROGRESS
    Result := (SHFileOperation(Info) = 0) and not Info.fAnyOperationsAborted;
  finally
    FreeMem(FromPath, Size);
  end;
end;

function ShellFileOperation(
    Handle: HWnd; Func: Integer; const FromList, ToList: String; Flags: Word): Boolean; overload;
var
  Info: TSHFileOpStruct;
begin
  Result := FromList = '';
  if not Result then begin
    FillChar(Info, SizeOf(Info), 0);
    Info.Wnd := Handle;
    Info.wFunc := Func;
    Info.pFrom := PChar(FromList);
    Info.pTo := PChar(ToList);
    Info.fFlags := Flags;
    Info.fAnyOperationsAborted := False;
    Info.hNameMappings := Nil;
    Info.lpszProgressTitle := Nil;  // only used if FOF_SIMPLEPROGRESS
    Result := (SHFileOperation(Info) = 0) and not Info.fAnyOperationsAborted;
  end;
end;

// kopiert alle Dateien von SrcDir -> DstDir
function ShellCopyDirContents(
    Handle: HWnd; const SrcDir, DstDir: String; SubDirs: Boolean; Flags: Word): Boolean;
var
  FromList: String;
begin
  if not SubDirs then
    Flags := Flags or FOF_FILESONLY;
  FromList := SrcDir + '\*.*'#0;
  Result := ShellFileOperation(Handle, FO_COPY, FromList, DstDir, Flags);
end;

// kopiert die in FileList angegebenen Dateien von SrcDir -> DstDir
function ShellCopyFileList(
    Handle: HWnd; const SrcDir, DstDir: String; FileList: array of String; Flags: Word): Boolean;
var
  FromList: String;
begin
  FromList := StringListToMultiAnsiStringPrefix(SrcDir + '\', FileList);
  Result := ShellFileOperation(Handle, FO_COPY, FromList, DstDir, Flags);
end;

// verschiebt alle Dateien von SrcDir -> DstDir
function ShellMoveDirContents(
    Handle: HWnd; const SrcDir, DstDir: String; SubDirs: Boolean; Flags: Word): Boolean;
var
  FromList: String;
begin
  if not SubDirs then
    Flags := Flags or FOF_FILESONLY;
  FromList := SrcDir + '\*.*'#0;
  Result := ShellFileOperation(Handle, FO_MOVE, FromList, DstDir, Flags);
end;

// verschiebt die in FileList angegebenen Dateien von SrcDir -> DstDir
function ShellMoveFileList(
    Handle: HWnd; const SrcDir, DstDir: String; FileList: array of String; Flags: Word): Boolean;
var
  FromList: String;
begin
  FromList := StringListToMultiAnsiStringPrefix(IncludeTrailingPathDelimiter(SrcDir), FileList);
  Result := ShellFileOperation(Handle, FO_MOVE, FromList, DstDir, Flags);
end;

// benennt Verzeichnis um
function ShellRenameDir(
    Handle: HWnd; const OldName, NewName: String; Flags: Word): Boolean;
var
  FromList, ToList: String;
begin
  FromList := OldName + #0;
  ToList := NewName + #0;   { TODO : untersuchen, ob notwendig }
  Result := ShellFileOperation(Handle, FO_MOVE, FromList, ToList, Flags);
end;

// Leert ein Verzeichnis
function ShellDeleteDirContents(
    Handle: HWnd; const Dir: String; SubDirs: Boolean; Flags: Word): Boolean;
begin
  Result := ShellDeleteFiles(Handle, Dir, '*.*', SubDirs, Flags);
end;

// Löscht Files (Maske) aus Verzeichnis Dir
function ShellDeleteFiles(
    Handle: HWnd; const Dir, Files: String; SubDirs: Boolean; Flags: Word): Boolean;
var
  FromList: String;
begin
  FromList := Dir + '\' + Files;
  Result := not WildCardFileExists(FromList);
  if Result then
    Exit;
  FromList := FromList + #0;
  if not SubDirs then
    Flags := Flags or FOF_FILESONLY;
  Result := ShellFileOperation(Handle, FO_DELETE, FromList, '', Flags);
end;

// löscht die in FileList angegebenen Dateien in Dir
function ShellDeleteFileList(
    Handle: HWnd; const Dir: String; FileList: array of String; Flags: Word): Boolean;
var
  FromList: String;
begin
  FromList := StringListToMultiAnsiStringPrefix(Dir + '\', FileList);
  Result := ShellFileOperation(Handle, FO_DELETE, FromList, '', Flags);
end;

// löscht ein Verzeichnis komplett
function ShellDeleteDir(
    Handle: HWnd; const Dir: String; Flags: Word): Boolean;
var
  FromList: String;
begin
  Result := not WildCardFileExists(Dir);
  if Result then
    Exit;
  FromList := Dir + #0;
  Result := ShellFileOperation(Handle, FO_DELETE, FromList, '', Flags);
end;

// *******************************************************************************************

// False, if error
function WaitForEndOfProcess(ProcessHandle: THandle): Boolean;
{$IFDEF WIN32}
var
  Msg: TMsg;
  Res: DWORD;
begin
  Result := False;
  while True do begin
    Res := MsgWaitForMultipleObjects(
        1,                          // wait for 1 process
        ProcessHandle,              // wait for process
        False,                      // return when the state of any one of the objects is set to signaled
        INFINITE,                   // no time-out
        QS_PAINT or QS_SENDMESSAGE  // A WM_PAINT message or a message sent by another thread or application is in the queue.
      );
    case Res of
      WAIT_OBJECT_0: begin          // The process is terminated
        Result := True;
        Break;
      end;
      WAIT_OBJECT_0 + 1: begin      // New input is available in the thread's input queue
        while PeekMessage(Msg, 0, WM_PAINT, WM_PAINT, PM_REMOVE) do
          DispatchMessage(Msg);
      end;
    else
      Break;   // Error
    end;
  end;
end;
{$ENDIF WIN32}
{$IFDEF UNIX}
begin
  { TODO : implementieren }
end;
{$ENDIF UNIX}

function ExecuteFile(const Filename: String; Parameter, CurrentDir: String; Wait: Boolean): Cardinal;
var
  si: TStartupInfo;
  pi: TProcessInformation;
  ExitCode: DWord;
begin
  Result := 0;
  if Length(CurrentDir) = 0 then
    CurrentDir := ExcludeTrailingPathDelimiter(ExtractFilePath(Filename));
  FillChar(si, SizeOf(si), 0);
  si.cb := SizeOf(si);
  si.dwFlags := STARTF_USESHOWWINDOW;
  si.wShowWindow := SW_NORMAL;
  FillChar(pi, SizeOf(pi), 0);
  if Length(Parameter) = 0 then
    Parameter := Format('"%s"', [Filename])
  else
    Parameter := Format('"%s" %s', [Filename, Parameter]);
  if CreateProcess(Nil, PChar(Parameter), Nil, Nil, False,
                   CREATE_DEFAULT_ERROR_MODE or CREATE_NEW_CONSOLE or
                   NORMAL_PRIORITY_CLASS, Nil, PChar(CurrentDir), si, pi) then begin
    try
      if Wait then begin
        if WaitForEndOfProcess(pi.hProcess) then begin  // kein Fehler
          if GetExitCodeProcess(pi.hProcess, ExitCode) then
            Result := ExitCode;
        end;
      end;
    finally
      CloseHandle(pi.hProcess);
      CloseHandle(pi.hThread);
    end;
  end
  else
    RaiseLastWin32Error;
end;

// *************************************  Kommandozeile  *************************************

// existiert single char option?
function CommandlineSingleCharOptionExists(Option: Char): Boolean;
begin
  Result := CommandlineSingleCharOptionIndex(Option) <> 0;
end;

// Index einer single char option; gibt 0 zurück, wenn sie nicht existiert
function CommandlineSingleCharOptionIndex(Option: Char): Integer;
var
  C: Char;
begin
  CharUpperBuff(@Option, 1);
  Result := ParamCount;
  while Result > 0 do begin
    if (Length(ParamStr(Result)) >= 2) and (ParamStr(Result)[1] in ['-', '/']) then begin
      C := ParamStr(Result)[2];
      CharUpperBuff(@C, 1);
      if C = Option then
        Break;
    end;
    Dec(Result);
  end;
end;

// Wert einer single char option
// gibt False zurück, wenn sie nicht existiert, Value dann ''
function CommandlineSingleCharOptionValue(Option: Char; out Value: String): Boolean;
var
  Index: Integer;
begin
  Index := CommandlineSingleCharOptionIndex(Option);
  Result := Index > 0;
  if Result then
    CommandlineSingleCharOptionValueByIndex(Index, Value)
  else
    Value := '';
end;

// Wert einer single char option, bei Index 0 bleibt Value unverändert
procedure CommandlineSingleCharOptionValueByIndex(Index: Integer; var Value: String);
begin
  if Index > 0 then
    Value := System.Copy(ParamStr(Index), 3, Length(ParamStr(Index)) - 2);
end;

// *******************************************************************************************

//  History:
//  2005-07-05, Peter J. Haas
//   - YCL version

end.
