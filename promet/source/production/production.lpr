program production;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, uerror, general,
umain, pvisualprometapp, uBaseVisualApplication, uselectorder;

{$R *.res}

begin
  Application.Free;
  Application := TBaseVisualApplication.Create(nil);
  Application.Initialize;
  fMain.DoCreate;
  Application.CreateForm(TfMain, fMain);
  Application.CreateForm(TfSelectOrder, fSelectOrder);
  Application.Run;
end.

