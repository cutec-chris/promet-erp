program messagemanager;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, umain, umtimeline, pvisualprometapp, richmemopackage, general,
  uBaseVisualApplication, ufollow, udetailview;

{$R *.res}

begin
  Application.Free;
  Application := TBaseVisualApplication.Create(nil);
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TfMain, fMain);
  Application.Run;
end.

