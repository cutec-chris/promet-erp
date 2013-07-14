program helpviewer;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{.$IFDEF UseCThreads}
  cthreads,
  {.$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, richmemopackage, uhelpviewer, pvisualprometapp,
  uBaseVisualApplication;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Free;
  Application := TBaseVisualApplication.Create(nil);
  Application.Initialize;
  Application.CreateForm(TfMain, fMain);
  fMain.DoCreate;
  Application.Run;
end.
