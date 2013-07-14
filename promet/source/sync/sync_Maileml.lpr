program sync_Maileml;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, richmemopackage, laz_synapse, usynceml, pvisualprometapp,
  uBaseVisualApplication, pmimemessages;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Free;
  Application := TBaseVisualApplication.Create(nil);
  Application.Initialize;
  Application.CreateForm(TfMailImport, fMailImport);
  Application.Run;
end.

