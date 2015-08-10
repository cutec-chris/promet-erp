program pstarter;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms,
  Process,UTF8Process,
  SysUtils,
  Dialogs,
  Utils,
  Classes,
  {$IFDEF WINDOWS}
  Windows,
  {$ENDIF}
  FileUtil
  { add your units here }, uprogramended, general,uLanguageUtils;

var
  Proc : TProcessUTF8;
  tmp: string;

{$R pstarter.res}


{$IFDEF WINDOWS}
function IsFileOpen(FileName: string): Boolean;
var
  HFileRes: HFILE;
begin
  Result := False;
  if not FileExists(FileName) then Exit;
  HFileRes := CreateFile(PChar(FileName),
                         GENERIC_READ or GENERIC_WRITE,
                         0,
                         nil,
                         OPEN_EXISTING,
                         FILE_ATTRIBUTE_NORMAL,
                         0);
  Result := (HFileRes = INVALID_HANDLE_VALUE);
  if not Result then
    CloseHandle(HFileRes);
end;{$ELSE}
function IsFileOpen(const FileName: string): Boolean;
var Stream: TFileStream;
begin
  Result := false;
  if not FileExists(FileName) then exit;
  try
    Stream := TFileStream.Create(FileName,fmOpenRead or fmShareExclusive);
  except
    Result := true;
    exit;
  end;
  Stream.Free;
end;
{$ENDIF}

begin
  Application.Initialize;
  Proc := TProcessUTF8.Create(nil);
  Proc.Options := [poNoConsole, poNewProcessGroup, poWaitOnExit];
  tmp := SysToUTF8(CmdLine);
  tmp := copy(tmp,pos(' ',tmp)+1,length(tmp));
  LoadLanguage(copy(tmp,0,pos(' ',tmp)));
  tmp := trim(copy(tmp,pos(' ',tmp)+1,length(tmp)));
  if length(tmp)>0 then
    if byte(tmp[length(tmp)])>128 then
      tmp := copy(tmp,0,length(tmp)-1);
  if (copy(tmp,0,1)='"') and (copy(tmp,length(tmp),1) = '"') then
    tmp := copy(tmp,2,length(tmp)-2);
  Proc.CommandLine := tmp;
  if Proc.CommandLine = '' then exit;
  Proc.Execute;
  while pos(' ',tmp)>0 do
    begin
      if (copy(tmp,0,1)='"') and (copy(tmp,length(tmp),1) = '"') then
        tmp := copy(tmp,2,length(tmp)-2);
      if FileExistsUTF8(tmp) then break;
      tmp := copy(tmp,pos(' ',tmp)+1,length(tmp));
      if (copy(tmp,0,1)='"') and (copy(tmp,length(tmp),1) = '"') then
        tmp := copy(tmp,2,length(tmp)-2);
      if FileExistsUTF8(tmp) then break;
    end;
  if FileExistsUTF8(tmp) then
    begin
      while IsFileOpen(tmp) do sleep(100);
    end;
  Application.CreateForm(TfProgramEnded, fProgramEnded);
  fProgramEnded.Filename := ExtractFilename(tmp);
  Application.Run;
  if fProgramEnded.cbDontShowthisDialogAgain.Checked then
    ExitCode := 1
  else
    ExitCode := 0;
end.

