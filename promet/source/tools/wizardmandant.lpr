program wizardmandant;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, dbflaz, pvisualprometapp
  { you can add units after this }
  ,uBaseVisualApplication, uwizardnewmandant, richmemopackage;

{$R *.res}

begin
  Application.Free;
  Application := TBaseVisualApplication.Create(nil);
  Application.Initialize;
  Application.CreateForm(TfWizardNewMandant, fWizardNewMandant);
  Application.Run;
end.

