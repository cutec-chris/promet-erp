{*******************************************************************************
  Copyright (C) Christian Ulrich info@cu-tec.de

  This source is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 2 of the License, or commercial alternative
  contact us for more information

  This code is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
  to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
  MA 02111-1307, USA.
Created 01.06.2006
*******************************************************************************}
unit uProcessManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Process, UTF8Process, FileUtil, uBaseApplication, ProcessUtils
  {$IFDEF WINDOWS}
  ,Windows,jwatlhelp32
  {$ENDIF}
  ;

function StartMessageManager(Mandant : string;User : string = '') : TExtendedProcess;
function StartProcessManager(Mandant : string;User : string = '';aProcess : string = 'processmanager') : TExtendedProcess;
function ProcessExists(cmd: string): Boolean;

implementation

{$IFDEF WINDOWS}
function ProcessExists(cmd: string): Boolean;
var
  ContinueLoop: BOOL;
  FSnapshotHandle: THandle;
  FProcessEntry32: TProcessEntry32;
begin
  FSnapshotHandle := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
  FProcessEntry32.dwSize := SizeOf(FProcessEntry32);
  ContinueLoop := Process32First(FSnapshotHandle, FProcessEntry32);
  Result := False;
  while Integer(ContinueLoop) <> 0 do
  begin
    if ((UpperCase(ExtractFileName(FProcessEntry32.szExeFile)) =
      UpperCase(cmd)) or (UpperCase(FProcessEntry32.szExeFile) =
      UpperCase(cmd))) then
    begin
      Result := True;
    end;
    ContinueLoop := Process32Next(FSnapshotHandle, FProcessEntry32);
  end;
  CloseHandle(FSnapshotHandle);
end;
{$ELSE}
function ProcessExists(cmd:String):Boolean;
var
  t:TProcess;
  s:TStringList;
  i: Integer;
  tmp: String;
begin
  Result:=false;
  t:=tprocess.create(nil);
  t.CommandLine:='ps -C '+cmd;
  t.Options:=[poUsePipes,poWaitonexit];
  try
    t.Execute;
    s:=tstringlist.Create;
    try
     s.LoadFromStream(t.Output);
     tmp := s.Text;
     for i := 0 to s.Count-1 do
       if copy(trim(s[i]),0,pos(' ',trim(s[i]))-1) = IntToStr(GetProcessID) then
         begin
           s.Delete(i);
           break;
         end;
     Result:=Pos(cmd,s.Text)>0;
    finally
    s.free;
    end;
  finally
    t.Free;
  end;
end;
{$ENDIF}
function StartMessageManager(Mandant : string;User : string = '') : TExtendedProcess;
begin
  Result := StartProcessManager(Mandant,User,'messagemanager');
end;
function StartProcessManager(Mandant : string;User : string = '';aProcess : string = 'processmanager') : TExtendedProcess;
var
  cmd: String;
  tmp: String;
  aDir: String;
begin
  Result := nil;
  cmd := aProcess+ExtractFileExt(BaseApplication.ExeName);
  if ProcessExists(aProcess+ExtractFileExt(BaseApplication.ExeName)) then exit;
  aDir := AppendPathDelim(AppendPathDelim(AppendPathDelim(BaseApplication.Location)+'tools'));
  if (not FileExistsUTF8(cmd)) and (not FileExistsUTF8(aDir+cmd)) then
    begin
      aDir := AppendPathDelim(AppendPathDelim(GetCurrentDirUTF8)+'tools');
      if not FileExistsUTF8(aDir+cmd) then exit;
    end;
  cmd := cmd+' "--mandant='+Mandant+'"';
  if User <> '' then
    cmd := cmd+' "--user='+User+'"';
  Result := TExtendedProcess.Create(aDir+cmd,True,aDir);
end;

end.
