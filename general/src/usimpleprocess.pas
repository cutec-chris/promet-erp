unit usimpleprocess;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,UTF8Process,process,FileUtil
  {$IFDEF MSWINDOWS}
  ,Windows
  {$ENDIF}
  ;

function ExecProcessEx(CommandLine : string;CurDir : string = '') : string;
procedure ExecProcess(CommandLine : string;CurDir : string = '';Waitfor : Boolean = True);

implementation

function ExecProcessEx(CommandLine : string;CurDir : string = '') : string;
var
  process : TProcessUTF8;
  tmps: tstringlist;
  err : string = '';
begin
  Process := TProcessUTF8.Create(nil);
  Process.Options:= [poUsePipes, poWaitOnExit, poStdErrToOutPut, poNewProcessGroup];
  Process.CommandLine := CommandLine;
  if CurDir <> '' then
    Process.CurrentDirectory := CurDir;
  try
    Process.Execute;
  except
    on e : exception do
      err := err+#13+e.Message;
  end;
  tmps := TStringList.Create;
  tmps.LoadFromStream(Process.Output);
  Process.Free;
  Result := tmps.Text;
  tmps.Free;
  if err <> '' then
    Result := 'errors:'+err+#13+Result;
end;
procedure ExecProcess(CommandLine : string;CurDir : string = '';Waitfor : Boolean = True);
var
{$IFDEF MSWINDOWS}
  SUInfo: TStartupInfo;
  ProcInfo: TProcessInformation;
  Res: Boolean;
{$ELSE}
  process : TProcessUTF8;
{$ENDIF}
  aDir: String;
begin
  aDir := GetCurrentDirUTF8;
  if CurDir <> '' then
    ChDir(CurDir);
{$IFDEF MSWINDOWS}
  FillChar(SUInfo, SizeOf(SUInfo), #0);
  with SUInfo do begin
    cb := SizeOf(SUInfo);
    dwFlags := STARTF_USESHOWWINDOW;
    wShowWindow := SW_HIDE
  end;
  Res := CreateProcess(NIL, PChar(UTF8ToSys(CommandLine)), NIL, NIL, FALSE,
                          CREATE_NEW_CONSOLE or
                          NORMAL_PRIORITY_CLASS, NIL,
                          PChar(UTF8ToSys(CurDir)),
                          SUInfo, ProcInfo);
  { Wait for it to finish. }
//  Clipboard.AsText:=CommandLine;
  if Res and Waitfor then
    WaitForSingleObject(ProcInfo.hProcess, INFINITE);
{$ELSE}
  Process := TProcessUTF8.Create(nil);
  if CurDir <> '' then
    Process.CurrentDirectory := CurDir;
  Process.CommandLine := CommandLine;
  if Waitfor then
    Process.Options := [poNoConsole,poWaitOnExit]
  else
    Process.Options := [poNoConsole];
//  Process.ShowWindow := swoHide;
  Process.Execute;
  if Waitfor then Process.Free;
{$ENDIF}
  ChDir(aDir);
end;


end.

