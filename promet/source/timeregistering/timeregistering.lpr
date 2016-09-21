program timeregistering;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  clocale,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms
  { you can add units after this },
umain, lazcontrols, lazreport, pvisualprometapp,
uBaseVisualApplication, ptimes, uTimeOptions;

{$R *.res}

begin
  Application.Free;
  Application := TBaseVisualApplication.Create(nil);
  Application.Title:='Timeregistering';
  Application.Initialize;
  Application.CreateForm(TfMain, fMain);
  fMain.DoCreate;
  Application.Run;
end.

