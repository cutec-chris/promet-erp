{$mode objfpc}{$H+}
program fpmake;

uses fpmkunit;

Var
  P : TPackage;
  T : TTarget;

begin
  With Installer do
    begin
    P:=AddPackage('lnet');

    P.Version:='0.6.4-0';
    P.OSes:=AllUnixOSes+[Win32,Win64];
    P.Author := 'Aleš Katona';
    P.License := 'LGPL with modification, Examples: GPL2';
    P.HomepageURL := 'http://lnet.wordpress.com/';
    P.Email := 'almindor@gmail.com';
    P.Description := 'Collection of classes and components to enable event-driven TCP or UDP networking';
{$IFDEF VER_2_4_0}
    P.Options := '-Sm';
{$ELSE VER_2_4_0}
    P.Options.add('-Sm');
{$ENDIF VER_2_4_0}

    P.Dependencies.Add('fcl-net');
    P.Dependencies.Add('fcl-base');
    P.Dependencies.Add('fcl-process');
    p.Dependencies.Add('winunits-jedi',[win32,win64]);
//    P.NeedLibC:= true;  // true for headers that indirectly link to libc?

    T:=P.Targets.AddUnit('lib/lcommon.pp');
    T.Dependencies.AddInclude('lib/sys/osunits.inc');
    T:=P.Targets.AddUnit('lib/levents.pp');
    with T.Dependencies do
      begin
      AddInclude('lib/sys/lkqueueeventerh.inc');
      AddInclude('lib/sys/lepolleventerh.inc');
      AddInclude('lib/sys/lkqueueeventer.inc');
      AddInclude('lib/sys/lepolleventer.inc');
      end;
    T:=P.Targets.AddUnit('lib/lcontrolstack.pp');
    T:=P.Targets.AddUnit('lib/lmimetypes.pp');
    T:=P.Targets.AddUnit('lib/lmimestreams.pp');
    T:=P.Targets.AddUnit('lib/lmimewrapper.pp');
    T:=P.Targets.AddUnit('lib/lprocess.pp');
    T:=P.Targets.AddUnit('lib/lspawnfcgi.pp');
    with T.Dependencies do
      begin
      AddInclude('lib/sys/lspawnfcgiunix.inc',AllUnixOSes);
      AddInclude('lib/sys/lspawnfcgiwin.inc',AllOSes-AllUnixOSes);
      end;
    T:=P.Targets.AddUnit('lib/lfastcgi.pp');
    T:=P.Targets.AddUnit('lib/lstrbuffer.pp');
    T:=P.Targets.AddUnit('lib/lthreadevents.pp');
    T:=P.Targets.AddUnit('lib/ltimer.pp');
    T:=P.Targets.AddUnit('lib/lwebserver.pp');
    T:=P.Targets.AddUnit('lib/openssl.pas');
    T:=P.Targets.AddUnit('lib/lnet.pp');
    T:=P.Targets.AddUnit('lib/lnetssl.pp');
    T:=P.Targets.AddUnit('lib/ltelnet.pp');
    T:=P.Targets.AddUnit('lib/lftp.pp');
    with T.Dependencies do
      begin
      AddInclude('lib/lcontainers.inc');
      AddInclude('lib/lcontainersh.inc');
      end;
    T:=P.Targets.AddUnit('lib/lsmtp.pp');
    T:=P.Targets.AddUnit('lib/lhttp.pp');
    T:=P.Targets.AddUnit('lib/lhttputil.pp');
    T:=P.Targets.AddUnit('lib/fastcgi_base.pp');

    T:=P.Targets.AddExampleProgram('examples/console/ltelnet/ltclient.pp');
    T:=P.Targets.AddExampleProgram('examples/console/lftp/lftpclient.pp');
    T:=P.Targets.AddExampleProgram('examples/console/lsmtp/lsmtpclient.pp');
    T:=P.Targets.AddExampleProgram('examples/console/ltcp/lclient.pp');
    T:=P.Targets.AddExampleProgram('examples/console/ltcp/lserver.pp');
    T:=P.Targets.AddExampleProgram('examples/console/ludp/ludp.pp');
    T:=P.Targets.AddExampleProgram('examples/console/lhttp/fphttpd.pp');
    T:=P.Targets.AddExampleProgram('examples/console/lhttp/fpget.pp');
    P.Sources.AddExampleFiles('examples/console/lhttp/*');

    P.Sources.AddDoc('README');
    P.Sources.AddDoc('LICENSE.examples');
    P.Sources.AddDoc('CHANGELOG');
    P.Sources.AddDoc('INSTALL');
    P.Sources.AddDocFiles('doc/*');
    P.Sources.AddDocFiles('doc/en/*');

    Run;
    end;
end.

