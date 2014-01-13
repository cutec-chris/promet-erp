program processdaemon;

{$mode objfpc}{$H+}

uses
{$IFDEF UNIX}
  CThreads,
  Cmem,{$ENDIF}
  Classes, SysUtils, EventLog, DaemonApp,process;

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

procedure TTheThread.Execute;
var
  aProcess: TProcess;
begin
  Application.Log(etDebug, 'Thread.Execute');
  aProcess := TProcess.Create(nil);
  aProcess.CurrentDirectory:=Application.Location;
  aProcess.CommandLine:='processmanager';
  aProcess.Options:=[poUsePipes,poNoConsole];
  while not Terminated do
    begin
      aProcess.Execute;
      while aProcess.Active and (not Terminated) do
        sleep(100);
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
    DaemonClassName := 'TProcessDaemon';
    Name := 'theDaemon.exe';
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
    EventLog.LogType := ltFile;
    EventLog.DefaultEventType := etDebug;
    EventLog.AppendContent := true;
    EventLog.FileName := ChangeFileExt(ParamStr(0), '.log');
    Initialize;
    Run;
  end;
end.
