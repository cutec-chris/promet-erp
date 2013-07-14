program updatedatabase;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms
  { add your units here }, uUpdateDatabase, DBFLaz, lazreport, uData,
  uAppconsts, richmemopackage, zcomponent, general;

{$R updatedatabase.res}

begin
  Application.Initialize;
  Application.CreateForm(TfMain, fMain);
  Application.CreateForm(TData, Data);
  Application.Run;
end.

