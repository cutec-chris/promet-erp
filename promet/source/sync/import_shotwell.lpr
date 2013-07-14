program import_shotwell;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, uimportshotwell, pvisualprometapp, zcomponent, uBaseVisualApplication,
  richmemopackage
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Free;
  Application := TBaseVisualApplication.Create(nil);
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

