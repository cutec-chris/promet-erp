program backuptool;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms
  { you can add units after this }, ubackupmain, synapse, general;

{$IFDEF WINDOWS}{$R manifest.rc}{$ENDIF}

{$IFDEF WINDOWS}{$R backuptool.rc}{$ENDIF}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

