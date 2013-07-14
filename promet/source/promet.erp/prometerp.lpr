program prometerp;

{$mode objfpc}{$H+}

uses
//  heaptrc,
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, lazreport, turbopoweripro, uMain, pvisualprometapp
  { you can add units after this }
  ,uBaseVisualApplication, pphones, richmemopackage, zvdatetimectrls, general;

{$R *.res}

begin
//  SetHeapTraceOutput ('heaptrclog1.trc');
  Application.Free;
  Application := TBaseVisualApplication.Create(nil);
  Application.Title:='Promet-ERP';
  Application.Initialize;
  Application.CreateForm(TfMain, fMain);
  Application.Run;
end.
