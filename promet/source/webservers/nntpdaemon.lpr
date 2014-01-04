Program nntpdaemon;

Uses
  Interfaces,
{$IFDEF UNIX}{$IFDEF UseCThreads}
  CThreads,
{$ENDIF}{$ENDIF}
  DaemonApp, lazdaemonapp, lnetbase, unntpdaemonmapper, unntpdaemon, ulnntp,
  pcmdprometapp
  { add your units here };

begin
  Application.Title:='Daemon application';
  Application.Initialize;
  Application.Run;
end.
