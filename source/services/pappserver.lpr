program pappserver;
uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils, CustApp, general_nogui,
  uBaseDatasetInterfaces2, pprometdbintfs,LazFileUtils,ubasedbclasses,uPrometORM;

type
  { TProcessManager }
  TProcessManager = class(TCustomApplication)
  private
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;
var
  Application: TProcessManager;

{ TProcessManager }

procedure TProcessManager.DoRun;
begin
  inherited DoRun;
  sleep(1);
end;
constructor TProcessManager.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  writeln('connecting...');
  Data.ConfigPath := GetOptionValue('config-path');
  if Data.ConfigPath = '' then Data.ConfigPath:=AppendPathDelim(GetAppConfigDir(True))+'prometerp';
  Data.ConfigPath := AppendPathDelim(Data.ConfigPath);
  Data.Mandant := GetOptionValue('mandant');
  if Data.Mandant = '' then Data.Mandant := 'Standard';
  try
    Data.Connect;
  except
    on e : exception do
      begin
        writeln(e.Message);
        exit;
      end;
  end;
  writeln('...done.');
end;

destructor TProcessManager.Destroy;
begin
  inherited Destroy;
end;

begin
  Application := TProcessManager.Create(nil);
  Application.Run;
  Application.Free;
end.

