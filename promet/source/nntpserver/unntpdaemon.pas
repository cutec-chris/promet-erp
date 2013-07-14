unit unntpdaemon;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, DaemonApp,eventlog,uBaseApplication,
  uBaseCustomApplication,uLNNTP;

type
  TNNTPThread = class(TThread)
    procedure ServerLog(aSocket: TLNNTPSocket; DirectionIn: Boolean;
      aMessage: string);
  private
    Server: TLNNTPServer;
    aLog : string;
    procedure DoLog;
  public
    constructor Create;
    destructor Destroy;
    procedure Execute; override;
  end;
  TTNNTPDaemon = class(TDaemon)
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
    procedure DataModuleStart(Sender: TCustomDaemon; var OK: Boolean);
    procedure DataModuleStop(Sender: TCustomDaemon; var OK: Boolean);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  TNNTPDaemon: TTNNTPDaemon;

implementation
procedure RegisterDaemon;
begin
  RegisterDaemonClass(TTNNTPDaemon)
end;

procedure TNNTPThread.ServerLog(aSocket: TLNNTPSocket; DirectionIn: Boolean;
  aMessage: string);
begin
  aLog := aMessage;
  Synchronize(@DoLog);
end;

procedure TNNTPThread.DoLog;
begin
  TNNTPDaemon.Logger.Info(aLog);
end;

constructor TNNTPThread.Create;
begin
  Server := TLNNTPServer.Create(nil);
  Server.OnLog:=@ServerLog;
  inherited Create(False);
end;

destructor TNNTPThread.Destroy;
begin
  Server.Free;
  inherited;
end;

procedure TNNTPThread.Execute;
begin
  while not Terminated do
    begin
      Server.CallAction;
    end;
end;
procedure TTNNTPDaemon.DataModuleCreate(Sender: TObject);
begin
  BaseApplication := TBaseCustomApplication.Create(nil);
  TBaseCustomApplication(BaseApplication).SetLog(Logger);
  Logger.LogType:=ltFile;
  Logger.FileName:='c:\nntpdaemon.log';
end;

procedure TTNNTPDaemon.DataModuleDestroy(Sender: TObject);
begin
  BaseApplication.Free;
end;

procedure TTNNTPDaemon.DataModuleStart(Sender: TCustomDaemon; var OK: Boolean);
begin
  OK := True;
end;

procedure TTNNTPDaemon.DataModuleStop(Sender: TCustomDaemon; var OK: Boolean);
begin
  OK := True;
end;

{$R *.lfm}


initialization
  RegisterDaemon;
end.

