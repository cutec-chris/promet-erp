unit usimpleprocess;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,process,Utils
  {$IFDEF MSWINDOWS}
  ,Windows
  {$ENDIF}
  ;

function ExecProcessEx(CommandLine : string;CurDir : string = '') : string;
procedure ExecProcess(CommandLine : string;CurDir : string = '';Waitfor : Boolean = True);

implementation

function ExecProcessEx(CommandLine : string;CurDir : string = '') : string;
var
  process : TProcess;
  tmps: tstringlist;
  err : string = '';
begin
  Process := TProcess.Create(nil);
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
  process : TProcess;
{$ENDIF}
  aDir: String;
begin
  aDir := GetCurrentDir;
  if CurDir <> '' then
    ChDir(CurDir);
{$IFDEF MSWINDOWS}
  FillChar(SUInfo, SizeOf(SUInfo), #0);
  with SUInfo do begin
    cb := SizeOf(SUInfo);
    dwFlags := STARTF_USESHOWWINDOW;
    wShowWindow := SW_HIDE
  end;
  Res := CreateProcess(NIL, PChar(UniToSys(CommandLine)), NIL, NIL, FALSE,
                          CREATE_NEW_CONSOLE or
                          NORMAL_PRIORITY_CLASS, NIL,
                          PChar(UniToSys(CurDir)),
                          SUInfo, ProcInfo);
  { Wait for it to finish. }
//  Clipboard.AsText:=CommandLine;
  if Res and Waitfor then
    WaitForSingleObject(ProcInfo.hProcess, INFINITE);
{$ELSE}
  Process := TProcess.Create(nil);
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

