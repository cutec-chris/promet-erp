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
  FileUtil,LCLIntf
  { add your units here }, uprogramended, general,uLanguageUtils;

var
  Proc : TProcessUTF8;
  tmp: string;
  Elapsed: Int64;
  aStartTime: TDateTime;

{$R pstarter.res}

begin
  aStartTime := Now();
  Application.Initialize;
  Proc := TProcessUTF8.Create(nil);
  Proc.Options := [poNoConsole, poNewProcessGroup, poWaitOnExit];
  tmp := SysToUni(CmdLine);
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
  try
    Proc.Execute;
  except
    begin
      if not OpenDocument(tmp) then
        raise;
    end;
  end;
  while pos(' ',tmp)>0 do
    begin
      if (copy(tmp,0,1)='"') and (copy(tmp,length(tmp),1) = '"') then
        tmp := copy(tmp,2,length(tmp)-2);
      if FileExists(UniToSys(tmp)) then break;
      tmp := copy(tmp,pos(' ',tmp)+1,length(tmp));
      if (copy(tmp,0,1)='"') and (copy(tmp,length(tmp),1) = '"') then
        tmp := copy(tmp,2,length(tmp)-2);
      if FileExists(UniToSys(tmp)) then break;
    end;
  Elapsed := round((Now()-aStartTime)*SecsPerDay*1000);
  Elapsed := 6000-Elapsed;
  if Elapsed<0 then Elapsed := 0;
  sleep(Elapsed);
  if FileExists(UniToSys(tmp)) then
    begin
      while IsFileOpen(tmp) do sleep(1000);
    end;
  Application.CreateForm(TfProgramEnded, fProgramEnded);
  fProgramEnded.Filename := ExtractFilename(tmp);
  Application.Run;
  if fProgramEnded.cbDontShowthisDialogAgain.Checked then
    ExitCode := 1
  else
    ExitCode := 0;
end.

