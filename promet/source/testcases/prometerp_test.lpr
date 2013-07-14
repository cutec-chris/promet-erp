program prometerp_test;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GuiTestRunner, lazmouseandkeyinput, visualnewmandant;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

