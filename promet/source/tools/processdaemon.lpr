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
Created 01.10.2013
*******************************************************************************}
program processdaemon;

{$mode objfpc}{$H+}

uses
{$IFDEF UNIX}
  CThreads,
  Cmem,
{$ELSE}
  Windows,
{$ENDIF}
  Classes, SysUtils, EventLog, DaemonApp,process,IniFiles;

type

  { TTheThread }

  TTheThread = class(TThread)
  private
    FString : string;
    procedure DoInfo;
  public
    procedure Info(aInfo : string);
    procedure Execute; override;
  end;

  TTheDaemon = class(TCustomDaemon)
  private
    FThread: TTheThread;
  public
    function Install: boolean; override;
    function UnInstall: boolean; override;
    function Start: boolean; override;
    function Stop: boolean; override;
    function Pause: boolean; override;
    function Continue: boolean; override;
    function Execute: boolean; override;
    function ShutDown: boolean; override;
  end;

  TTheDaemonMapper = class(TCustomDaemonMapper)
  public
    constructor Create(AOwner: TComponent); override;
    procedure ToDoOnInstall(Sender: TObject);
    procedure ToDoOnRun(Sender: TObject);
    procedure ToDoOnUninstall(Sender: TObject);
    procedure ToDoOnDestroy(Sender: TObject);
  end;
  {$ifdef WINDOWS}
  PFNSHGetFolderPath = Function(Ahwnd: HWND; Csidl: Integer; Token: THandle; Flags: DWord; Path: PChar): HRESULT; stdcall;
  {$endif}

function BoolToStr(AVal: Boolean): String;
begin
  if AVal = True then result := 'true' else result := 'false';
end;

procedure TTheThread.DoInfo;
begin
  Application.Log(etDebug,FString);
end;

procedure TTheThread.Info(aInfo: string);
begin
  FString := aInfo;
  Synchronize(@DoInfo);
end;

function GetGlobalConfigDir(app : string;Global : Boolean = True) : string;
{$IFDEF MSWINDOWS}
const
  CSIDL_COMMON_APPDATA  = $0023; // All Users\Application Data
  CSIDL_LOCAL_APPDATA   = $001c;
  CSIDL_FLAG_CREATE     = $8000; { (force creation of requested folder if it doesn't exist yet)     }
var
  Path: array [0..1024] of char;
  P : Pointer;
  SHGetFolderPath : PFNSHGetFolderPath = Nil;
  CFGDLLHandle : THandle = 0;
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
  CFGDLLHandle:=LoadLibrary('shell32.dll');
  if (CFGDLLHandle<>0) then
    begin
    P:=GetProcAddress(CFGDLLHandle,'SHGetFolderPathA');
    If (P=Nil) then
      begin
      FreeLibrary(CFGDLLHandle);
      CFGDllHandle:=0;
      end
    else
      SHGetFolderPath:=PFNSHGetFolderPath(P);
    end;
  If (P=Nil) then
    begin
    CFGDLLHandle:=LoadLibrary('shfolder.dll');
    if (CFGDLLHandle<>0) then
      begin
      P:=GetProcAddress(CFGDLLHandle,'SHGetFolderPathA');
      If (P=Nil) then
        begin
        FreeLibrary(CFGDLLHandle);
        CFGDllHandle:=0;
        end
      else
        ShGetFolderPath:=PFNSHGetFolderPath(P);
      end;
    end;
  Result := ExtractFilePath(Paramstr(0));
  If (@ShGetFolderPath<>Nil) then
    begin
      if Global then
        begin
          if SHGetFolderPath(0,CSIDL_COMMON_APPDATA or CSIDL_FLAG_CREATE,0,0,@PATH[0])=S_OK then
            Result:=IncludeTrailingPathDelimiter(StrPas(@Path[0]))+app;
        end
      else
        begin
          if SHGetFolderPath(0,CSIDL_LOCAL_APPDATA or CSIDL_FLAG_CREATE,0,0,@PATH[0])=S_OK then
            Result:=IncludeTrailingPathDelimiter(StrPas(@Path[0]))+app;
        end;
    end;
{$ELSE}
  Result:=GetEnvironmentVariable('HOME');
  If (Result<>'') then
    Result:=IncludeTrailingPathDelimiter(Result)+'.'+app;
{$ENDIF}
  Result := IncludeTrailingPathDelimiter(result);
end;

procedure TTheThread.Execute;
var
  aProcess: TProcess;
  aInfo: TSearchRec;
  aMandant: String;
  aFileDir: String;
  Output,Buffer,LogOutput : string;
  aIni: TIniFile;
  aPath: String;

  procedure ProcessData;
  var
    aLine: String;
    BytesAvailable: System.DWord;
    BytesRead: Integer;
  begin
    BytesAvailable := aProcess.Output.NumBytesAvailable;
    BytesRead := 0;
    while BytesAvailable>0 do
      begin
        SetLength(Buffer, BytesAvailable);
        BytesRead := aProcess.OutPut.Read(Buffer[1], BytesAvailable+1);
        Output := Output+copy(Buffer,0, BytesRead);
        LogOutput := Logoutput+copy(Buffer,1, BytesRead);
        while pos(#10,Logoutput) > 0 do
          begin
            aLine := copy(Logoutput,0,pos(#10,Logoutput)-1);
            Application.Log(etInfo, aLine);
            Logoutput := copy(Logoutput,pos(#10,Logoutput)+1,length(Logoutput));
            if copy(Logoutput,0,1) = #13 then
              Logoutput := copy(Logoutput,2,length(Logoutput));
          end;
        sleep(1);
        BytesAvailable := aProcess.Output.NumBytesAvailable;
      end;
  end;

begin
  Application.Log(etDebug, 'Thread.Execute');
  aFileDir := GetGlobalConfigDir('prometerp',False);
  Application.Log(etDebug, 'Thread.FileDir:'+aFileDir);
  If FindFirst (aFileDir+'*.perml',faAnyFile,aInfo)=0 then
    begin
      aMandant := copy(aInfo.Name,0,length(aInfo.Name)-6);
    end;
  if aMandant='' then
    begin
      Application.Log(etDebug, 'Using ini File '+ExtractFileDir(Application.Location)+DirectorySeparator+'processdaemon.ini');
      aIni := TIniFile.Create(ExtractFileDir(Application.Location)+DirectorySeparator+'processdaemon.ini');
      aMandant := aIni.ReadString('mandant','name','');
      aPath := aIni.ReadString('mandant','path','');
      aIni.free;
    end;
  Application.Log(etDebug, 'Thread.Mandant:'+aMandant);
  FindClose(aInfo);
  aProcess := TProcess.Create(nil);
  ChDir(Application.Location);
  aProcess.CurrentDirectory:=Application.Location;
  if aPath = '' then
    aProcess.CommandLine:=Application.Location+'processmanager'+ExtractFileExt(Application.ExeName)+' --mandant='+aMandant
  else
    aProcess.CommandLine:=Application.Location+'processmanager'+ExtractFileExt(Application.ExeName)+' --mandant='+aMandant+' --config-path='+aPath;
  aProcess.Options:=[poUsePipes,poNoConsole];
  Application.Log(etDebug, 'Executing:'+aProcess.CommandLine);
  while not Terminated do
    begin
      try
        aProcess.Execute;
        while aProcess.Active and (not Terminated) do
          begin
            sleep(100);
            ProcessData;
          end;
        ProcessData;
      except
        on e : Exception do
          Application.Log(etDebug, 'Error: '+e.Message);
      end;
      Application.Log(etDebug, 'Exitted: Resultcode '+IntToStr(aProcess.ExitStatus));
      sleep(5000);
    end;
  aProcess.Free;
end;

{$REGION ' - Daemon - '}
function TTheDaemon.Install: boolean;
begin
  result := inherited Install;
  Application.Log(etDebug, 'Daemon.installed: ' + BoolToStr(result));
end;

function TTheDaemon.UnInstall: boolean;
begin
  result := inherited UnInstall;
  Application.Log(etDebug, 'Daemon.Uninstall: ' + BoolToStr(result));
end;

function TTheDaemon.Start: boolean;
begin
  result := inherited Start;
  Application.Log(etDebug, 'Daemon.Start: ' + BoolToStr(result));
  if not(assigned(FThread)) then
  begin
    FThread := TTheThread.Create(true);
    FThread.FreeOnTerminate := true;
    FThread.Resume;
  end;
end;

function TTheDaemon.Stop: boolean;
begin
  Application.Log(etDebug, 'Daemon.Stop: ' + BoolToStr(result));
  if assigned(FThread) then
  begin
    FThread.Terminate;
    Application.Log(etDebug, 'Thread.WaitFor');
    FThread.WaitFor;
    Application.Log(etDebug, 'Thread.Free');
    FThread := nil;
  end;
  result := inherited Stop;
end;

function TTheDaemon.Pause: boolean;
begin
  result := inherited Pause;
  Application.Log(etDebug, 'Daemon.Pause: ' + BoolToStr(result));
  if assigned(FThread) then
    FThread.Suspend;
end;

function TTheDaemon.Continue: boolean;
begin
  result := inherited Continue;
  Application.Log(etDebug, 'Daemon.Continue: ' + BoolToStr(result));
  if assigned(FThread) then
    FThread.Resume;
end;

function TTheDaemon.Execute: boolean;
begin
  result := inherited Execute;
  Application.Log(etDebug, 'Daemon.Execute: ' + BoolToStr(result));
end;

function TTheDaemon.ShutDown: boolean;
begin
  result := inherited ShutDown;
  Application.Log(etDebug, 'Daemon.ShutDown: ' + BoolToStr(result));
end;
{$ENDREGION}

{$REGION ' - DaemonMapper - '}
constructor TTheDaemonMapper.Create(AOwner: TComponent);
begin
  Application.Log(etDebug, 'DaemonMapper.Create');
  inherited Create(AOwner);
  with DaemonDefs.Add as TDaemonDef do
  begin
    DaemonClassName := 'TTheDaemon';
    Name := 'processdaemon';
    Description := 'Promet Processmanager Daemon';
    DisplayName := 'Process Daemon';
    Options := [doAllowStop,doAllowPause];
    Enabled := true;
    with WinBindings do
    begin
      StartType := stBoot;
      WaitHint := 0;
      IDTag := 0;
      ServiceType := stWin32;
      ErrorSeverity := esNormal;//esIgnore;
    end;
    LogStatusReport := false;
  end;
  OnInstall := @Self.ToDoOnInstall;
  OnRun := @Self.ToDoOnRun;
  OnUnInstall := @Self.ToDoOnUninstall;
  OnDestroy := @Self.ToDoOnDestroy;
  Application.Log(etDebug, 'DaemonMapper.Createted');
end;

procedure TTheDaemonMapper.ToDoOnInstall(Sender: TObject);
begin
  Application.Log(etDebug, 'DaemonMapper.Install');
end;

procedure TTheDaemonMapper.ToDoOnRun(Sender: TObject);
begin
  Application.Log(etDebug, 'DaemonMapper.Run');
end;

procedure TTheDaemonMapper.ToDoOnUnInstall(Sender: TObject);
begin
  Application.Log(etDebug, 'DaemonMapper.Uninstall');
end;

procedure TTheDaemonMapper.ToDoOnDestroy(Sender: TObject);
begin
  //doesn't comes here
  Application.Log(etDebug, 'DaemonMapper.Destroy');
end;
{$ENDREGION}

begin
  RegisterDaemonClass(TTheDaemon);
  RegisterDaemonMapper(TTheDaemonMapper);
  with Application do
  begin
    Title := 'Processdaemon';
    EventLog.DefaultEventType := etDebug;
    EventLog.AppendContent := false;
    EventLog.LogType := ltFile;
    {$ifndef unix}
    EventLog.FileName := ChangeFileExt(ParamStr(0), '.log');
    {$else}
    EventLog.FileName := '/var/log/'+ExtractFileName(ChangeFileExt(ParamStr(0), '.log'));
    {$endif}
    Initialize;
    Run;
  end;
end.
