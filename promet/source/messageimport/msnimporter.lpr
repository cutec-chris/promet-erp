program msnimporter;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp, pcmdprometapp, uBaseCustomApplication
  { you can add units after this };

type

  { TMSNImporter }

  TMSNImporter = class(TBaseCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

{ TMSNImporter }

procedure TMSNImporter.DoRun;
begin
  inherited;
  // stop program loop
  Terminate;
end;

constructor TMSNImporter.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor TMSNImporter.Destroy;
begin
  inherited Destroy;
end;

var
  Application: TMSNImporter;

begin
  Application:=TMSNImporter.Create(nil);
  Application.Run;
  Application.Free;
end.

