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
  Classes, SysUtils, Process, Utils, uBaseApplication,usimpleprocess
  {$IFDEF WINDOWS}
  ,Windows,jwatlhelp32
  {$ENDIF}
  ;

function StartMessageManager(Mandant : string;User : string = '') : TProcess;
function StartProcessManager(Mandant : string;User : string = '';aProcess : string = 'processmanager') : TProcess;
function ProcessExists(cmd,cmdln: string): Boolean;
var
  ProcessMandant : string;
  ProcessUser : string;

implementation

{$IFDEF WINDOWS}
function ProcessExists(cmd,cmdln: string): Boolean;
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
function ProcessExists(cmd,cmdln:String):Boolean;
var
  s:TStringList;
  i: Integer;
  tmp: String;
  tmp1: String;
begin
  Result:=false;
  s:=tstringlist.Create;
  try
   s.Text:=ExecProcessEx('ps -f --width=10000 -C '+cmd);
   tmp := s.Text;
   for i := 0 to s.Count-1 do
     if copy(trim(s[i]),0,pos(' ',trim(s[i]))-1) = IntToStr(GetProcessID) then
       begin
         s.Delete(i);
         break;
       end;
   tmp1 := StringReplace(cmd+' '+cmdln,'"','',[rfReplaceAll]);
   tmp1 := StringReplace(tmp1,' ','',[rfReplaceAll]);
   tmp := StringReplace(tmp,' ','',[rfReplaceAll]);
   Result:=(Pos(tmp1,tmp)>0);
  finally
    s.free;
  end;
end;
{$ENDIF}
function StartMessageManager(Mandant : string;User : string = '') : TProcess;
begin
  Result := StartProcessManager(Mandant,User,'messagemanager');
end;
function StartProcessManager(Mandant : string;User : string = '';aProcess : string = 'processmanager') : TProcess;
var
  tmp: String;
  aDir: String;
  cmdln: String;
begin
  ProcessMandant := Mandant;
  ProcessUser := User;
  Result := nil;
  cmdln := ' "--mandant='+Mandant+'"';
  if User <> '' then
    cmdln := cmdln+' "--user='+User+'"';
  if BaseApplication.HasOption('debug-log') then
    cmdln := cmdln+' "--debug-log=msg.'+BaseApplication.GetOptionValue('debug-log')+'"';
  if BaseApplication.HasOption('config-path') then
    cmdln := cmdln+' "--config-path='+BaseApplication.GetOptionValue('config-path')+'"';
  if ProcessExists(aProcess+ExtractFileExt(BaseApplication.ExeName),cmdln) then exit;
  aDir := BaseApplication.Location+'tools'+DirectorySeparator;
  if (not FileExists(UniToSys(aProcess+ExtractFileExt(BaseApplication.ExeName)))) and (not FileExists(UniToSys(aDir+aProcess+ExtractFileExt(BaseApplication.ExeName)))) then
    begin
      aDir := GetCurrentDir+'tools'+DirectorySeparator;
      if not FileExists(UniToSys(aDir+aProcess+ExtractFileExt(BaseApplication.ExeName))) then exit;
    end;
  Result := TProcess.Create(nil);
  Result.Options:=[poUsePipes,poStderrToOutPut,poNoConsole];
  Result.CommandLine:=aDir+aProcess+ExtractFileExt(BaseApplication.ExeName)+' '+cmdln;
  try
    Result.Execute;
  except
    begin
      Result.Free;
      Result := nil;
    end;
  end;
end;

end.
