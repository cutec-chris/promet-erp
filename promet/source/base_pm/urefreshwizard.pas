unit uRefreshWizard;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, ButtonPanel;

type
  TfRefreshWizard = class(TForm)
    ButtonPanel1: TButtonPanel;
    Image1: TImage;
    Image2: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    lReason: TLabel;
    mReason: TMemo;
    rbPlan1: TRadioButton;
    rbPlan2: TRadioButton;
    rbPlan3: TRadioButton;
    rbPlan4: TRadioButton;
  private
    { private declarations }
  public
    { public declarations }
    function Execute : Boolean;
  end;

var
  fRefreshWizard: TfRefreshWizard;

implementation

{$R *.lfm}

function TfRefreshWizard.Execute: Boolean;
begin
  if not Assigned(Self) then
    begin
      Application.CreateForm(TfRefreshWizard,fRefreshWizard);
      Self := fRefreshWizard;
    end;
  result := ShowModal=mrOK;
end;

end.

