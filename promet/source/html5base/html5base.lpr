program html5base;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}cwstring,cthreads,{$ENDIF}

  uBaseFCGIApplication, pfcgiprometapp,
  Interfaces,uBaseApplication,umain;
begin
  Application.DefaultModule:='main';
  with BaseApplication as IBaseApplication do
    begin
      with Application as IBaseApplication do
        begin
          AppVersion:={$I ../base/version.inc};
          AppRevision:={$I ../base/revision.inc};
        end;
      SetConfigName('appconfig');
      RestoreConfig;
      Login;
    end;
  Application.Initialize;
  Application.Run;
  Application.DoExit;
end.

