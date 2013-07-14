unit visualnewmandant;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, types, fpcunit, testutils, testregistry, uvisualtests,
  Process, Forms, FileUtil, mouseandkeyinput, LCLType;

type

  NewMandant= class(TTestCase)
  public
    aPoint: TPoint;
  published
    procedure StartApp;
    procedure WaitForWizardQuestion;
    procedure FillFirstPage;
    procedure NextPage1;
    procedure SelectGerman;
    procedure NextPage2;
    procedure NextPage3;
    procedure FillInDB;
    procedure Finalize;
  end; 
var
  aProcess : TProcess;
implementation
procedure NewMandant.StartApp;
var
  aProcess: TProcess;
begin
  MouseInput.Move([],Screen.Width-10,Screen.Height-10);
  aProcess := TProcess.Create(nil);
  aProcess.CommandLine := AppendPathDelim(GetCurrentDir)+'prometerp --config-path=test';
  aProcess.Options := [poNoConsole];
  aProcess.Execute;
end;

procedure NewMandant.WaitForWizardQuestion;
begin
  Check(WaitForImageOnScreen('../../source/testcases/images/ask_for_mandant_wizard.png',4000) = True,'Mandant Wizard not shown');
  KeyInput.Press(VK_RETURN);
end;

procedure NewMandant.FillFirstPage;
begin
  Check(WaitForImageOnScreen('../../source/testcases/images/mandant_wizard_1.png',4000) = True,'Mandant Wizard Page 1 not Shown');
  KeyInput.Press(VK_T);// Press('t');
  KeyInput.Press(VK_E);
  KeyInput.Press(VK_S);
  KeyInput.Press(VK_T);
end;
procedure NewMandant.NextPage1;
begin
  aPoint := FindImageOnScreen('../../source/testcases/images/mandant_wizard_weiter.png');
  LeftClick(aPoint);
end;
procedure NewMandant.SelectGerman;
begin
  KeyInput.Press(VK_TAB);
  KeyInput.Press(VK_TAB);
  KeyInput.Press(VK_TAB);
  KeyInput.Press(VK_TAB);
  KeyInput.Press(VK_TAB);
  KeyInput.Press(VK_DOWN);
  KeyInput.Press(VK_DOWN);
  KeyInput.Press(VK_RIGHT);
  KeyInput.Press(VK_DOWN);
end;
procedure NewMandant.NextPage2;
begin
  LeftClick(aPoint);
end;
procedure NewMandant.NextPage3;
begin
  LeftClick(aPoint);
end;
procedure NewMandant.FillInDB;
begin
  KeyInput.Press(VK_TAB);
  KeyInput.Press(VK_TAB);
  KeyInput.Press(VK_TAB);
  KeyInput.Press(VK_TAB);

end;
procedure NewMandant.Finalize;
begin
  aProcess.Free;
end;

initialization

  RegisterTest(NewMandant); 
end.

