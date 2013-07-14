program fpschart;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, tachartlazaruspkg, mainform, laz_fpspreadsheet_visual
  { you can add units after this };

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFPSChartForm, FPSChartForm);
  Application.Run;
end.

