unit uchangegantt;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ButtonPanel;

type
  TfChangeGantt = class(TForm)
    ButtonPanel1: TButtonPanel;
    cbMakeSnapshot: TCheckBox;
    cbSetAppt: TCheckBox;
    cbChangeMilestones: TCheckBox;
    cbAddToProject: TCheckBox;
    Label1: TLabel;
    mRule: TMemo;
    procedure cbSetApptChange(Sender: TObject);
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

procedure TfChangeGantt.cbSetApptChange(Sender: TObject);
begin
  if not cbSetAppt.Checked then
    cbMakeSnapshot.Checked:=False;
end;

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

