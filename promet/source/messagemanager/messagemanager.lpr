program messagemanager;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, umain, umtimeline, richmemopackage, general,
  uBaseVisualApplication, ufollow, udetailview, wiki2html_pkg;

{$R *.res}

begin
  Application.Free;
  Application := TBaseVisualApplication.Create(nil);
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TfMain, fMain);
  Application.Run;
end.

