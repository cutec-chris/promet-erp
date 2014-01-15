 program excel_import;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp,
  Interfaces
  { you can add units after this },db, Utils, FileUtil, Forms, uData,
  uIntfStrConsts, pcmdprometapp, uBaseCustomApplication, laz_fpspreadsheet,
  uBaseApplication;

type

  { PrometCmdApp }

  PrometCmdApp = class(TBaseCustomApplication)
  private
    mailaccounts : string;
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

{ PrometCmdApp }

procedure PrometCmdApp.DoRun;
begin
  with BaseApplication as IBaseApplication do
    begin
      AppVersion:={$I ../base/version.inc};
      AppRevision:={$I ../base/revision.inc};
    end;
  if not Login then Terminate;
  //Your logged in here on promet DB


  // stop program loop
  Terminate;
end;

constructor PrometCmdApp.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor PrometCmdApp.Destroy;
begin
  inherited Destroy;
end;

var
  Application: PrometCmdApp;

begin
  Application:=PrometCmdApp.Create(nil);
  Application.Run;
  Application.Free;
end.

