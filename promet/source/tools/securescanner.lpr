program securescanner;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp,
  Windows
  , uProcsses;

type

  { TSecureScanner }

  TSecureScanner = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

{ TSecureScanner }

procedure TSecureScanner.DoRun;
var
  ErrorMsg: String;
  ProcessList: TStringList;
begin
  // quick check parameters
  ErrorMsg:=CheckOptions('h','help');
  if ErrorMsg<>'' then begin
    ShowException(Exception.Create(ErrorMsg));
    Halt;
  end;

  // parse parameters
  if HasOption('h','help') then begin
    WriteHelp;
    Halt;
  end;
  { add your program here }
  ProcessList := GetProcessList;
  for i := 0 to Processlist.Count-1 do
    writeln(processList[i]);
  while (True);

  // stop program loop
  Terminate;
end;

constructor TSecureScanner.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor TSecureScanner.Destroy;
begin
  inherited Destroy;
end;

procedure TSecureScanner.WriteHelp;
begin
  { add your help code here }
  writeln('Usage: ',ExeName,' -h');
end;

var
  Application: TSecureScanner;

begin
  Application:=TSecureScanner.Create(nil);
  Application.Run;
  Application.Free;
end.

