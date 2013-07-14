unit uWizard;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Buttons, ComCtrls, uIntfStrConsts;

type

  { TfWizard }

  TfWizard = class(TForm)
    bAbort0: TButton;
    bNext0: TButton;
    bPrev0: TButton;
    bvleft: TBevel;
    bvImage: TBevel;
    bvRight0: TBevel;
    imDialog: TImage;
    lDescription0: TLabel;
    pButtons0: TPanel;
    pCont0: TPanel;
    pLeft: TPanel;
    procedure bAbort0Click(Sender: TObject);
    procedure bNext0Click(Sender: TObject);
    procedure bPrev0Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    procedure SetLanguage;
    procedure DoSave;
    function DoExecStep(Step : Integer) : Integer;
    procedure InitWizard;
  end;

var
  fWizard: TfWizard;

implementation

{ TfWizard }

procedure TfWizard.bAbort0Click(Sender: TObject);
begin
  Close;
end;

procedure TfWizard.bNext0Click(Sender: TObject);
var
  i: LongInt;
begin
  i := Steps[length(Steps)-1];
  i := DoExecStep(i);
  Setlength(Steps,length(Steps)+1);
  Steps[length(Steps)-1] := i;
  TPanel(FindComponent('pCont'+IntToStr(Steps[length(Steps)-2]))).Visible := false;
  if FindComponent('pCont'+IntToStr(i)) <> nil then
    begin
      TPanel(FindComponent('pCont'+IntToStr(i))).Visible := True
    end
  else
    begin
      DoSave;
      Close;
    end;
end;

procedure TfWizard.bPrev0Click(Sender: TObject);
var
  i: LongInt;
begin
  TPanel(FindComponent('pCont'+IntToStr(Steps[length(Steps)-1]))).Visible := false;
  if FindComponent('pCont'+IntToStr(Steps[length(Steps)-2])) <> nil then
    TPanel(FindComponent('pCont'+IntToStr(Steps[length(Steps)-2]))).Visible := True;
  Setlength(Steps,length(Steps)-1);
end;

procedure TfWizard.SetLanguage;
var
  i: Integer;
begin
  i := 0;
  while FindComponent('bPrev'+IntToStr(i)) <> nil do
    begin
      TButton(FindComponent('bPrev'+IntToStr(i))).Caption := strPrev;
      if i = 0 then
        TButton(FindComponent('bPrev'+IntToStr(i))).Enabled := False;
      TButton(FindComponent('bNext'+IntToStr(i))).Caption := strNext;
      TButton(FindComponent('bAbort'+IntToStr(i))).Caption := strAbort;
      inc(i);
    end;
  TButton(FindComponent('bNext'+IntToStr(i))).Caption := strFinish;
  //Application depend Language strings
  lDescription0.Caption := '';
end;

procedure TfWizard.DoSave;
begin
  //Wizard finished, use the made settings
end;

function TfWizard.DoExecStep(Step: Integer): Integer;
begin
  //Do whatever to do in these Step and return the next step
end;

procedure TfWizard.InitWizard;
var
  i : Integer;
begin
  Setlength(Steps,1);
  Steps[0] := 0;
  i := 0;
  while FindComponent('pCont'+IntToStr(i)) <> nil do
    begin
      TPanel(FindComponent('pCont'+IntToStr(i))).Visible := False;
      inc(i);
    end;
  pCont0.Visible := True;
end;

initialization
  {$I uwizard.lrs}

end.

