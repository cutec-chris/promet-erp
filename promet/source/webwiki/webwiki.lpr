program webwiki;
{$mode objfpc}{$H+}
uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  cwstring,
  {$IFDEF HTTPAPP}
  ubasehttpapplication,
  {$ELSE}
  uBaseFCGIApplication,
  {$ENDIF}
  pfcgiprometapp,
  uWebWiki,
  Interfaces, uerror, GeoIP, uBaseApplication, udownloads,
  ushop, uforum, ulogin, laz_synapse;
{$R *.res}
begin
  writeln('starting...');
  Application.DefaultModule:='wiki';
  with BaseApplication as IBaseApplication do
    begin
      with Application as IBaseApplication do
        begin
          AppVersion:={$I ../base/version.inc};
          AppRevision:={$I ../base/revision.inc};
        end;
      SetConfigName('webconfig');
      RestoreConfig;
      writeln('login...');
      Login;
    end;
  Application.Initialize;
  writeln('run...');
  Application.Run;
  Application.DoExit;
end.
