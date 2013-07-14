program fpsvisual;

{$mode objfpc}{$H+}

uses
  Interfaces, // this includes the LCL widgetset
  Forms, mainform, LResources, laz_fpspreadsheet_visual
  { you can add units after this };

//{$IFDEF WINDOWS}{$R fpsvisual.rc}{$ENDIF}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

