unit uchangegantt;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ButtonPanel;

type
  TfChangeGantt = class(TForm)
    ButtonPanel1: TButtonPanel;
    cbSetAppt: TCheckBox;
    cbChangeMilestones: TCheckBox;
    cbAddToProject: TCheckBox;
    Label1: TLabel;
    mRule: TMemo;
  private
    { private declarations }
  public
    { public declarations }
    function Execute(var aRule: string): Boolean;
  end;

var
  fChangeGantt: TfChangeGantt;

implementation

{$R *.lfm}

function TfChangeGantt.Execute(var aRule: string): Boolean;
begin
  if not Assigned(Self) then
    begin
      Application.CreateForm(TfChangeGantt,fChangeGantt);
      Self := fChangeGantt;
    end;
  mRule.Text:=aRule;
  Result := Showmodal=mrOK;
  aRule := mRule.Text;
end;

end.

