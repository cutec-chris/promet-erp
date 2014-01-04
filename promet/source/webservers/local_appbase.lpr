program local_appbase;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}cwstring,cthreads,{$ENDIF}
  pfcgiprometapp,
  ubasehttpapplication,
  Interfaces,uBaseApplication,umain, laz_synapse;
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
  Application.Port:=8086;
  Application.DefaultModule:='appbase';
  Application.Run;
  Application.DoExit;
end.

