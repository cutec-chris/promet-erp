unit uBankingDialog;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, ExtCtrls;

type
  TfbankingDialog = class(TForm)
    Bevel1: TBevel;
    Label1: TLabel;
    pButtons: TPanel;
    procedure aButtonClick(Sender: TObject);
  private
    { private declarations }
    Res : Integer;
  public
    { public declarations }
    procedure ClearButtons;
    procedure AddButton(Cap : string;aTag : Integer);
    procedure SetLabel(aText : string);
    function Execute : Integer;
  end; 

var
  fbankingDialog: TfBankingDialog;

implementation

{$R *.lfm}

procedure TfbankingDialog.aButtonClick(Sender: TObject);
begin
  Res := TButton(Sender).Tag;
  ModalResult := mrOK;
end;

procedure TfbankingDialog.ClearButtons;
begin
  if not Assigned(Self) then
    begin
      Application.CreateForm(TfBankingDialog,fBankingDialog);
      Self := fBankingDialog;
    end;
  while pButtons.ComponentCount > 0 do
    pButtons.Components[0].Free;
end;

procedure TfbankingDialog.AddButton(Cap: string; aTag: Integer);
var
  aButton: TButton;
begin
  if not Assigned(Self) then
    begin
      Application.CreateForm(TfBankingDialog,fBankingDialog);
      Self := fBankingDialog;
    end;
  aButton := TButton.Create(pButtons);
  aButton.Caption := Cap;
  aButton.Tag:=aTag;
  aButton.Parent := pButtons;
  aButton.Left:=0;
  aButton.Align:=alRight;
  aButton.BorderSpacing.Around:=4;
  aButton.AutoSize:=True;
  aButton.OnClick:=@aButtonClick;
end;

procedure TfbankingDialog.SetLabel(aText: string);
begin
  Label1.Caption:=aText;
  Application.ProcessMessages;
  Height := Label1.Height+50;
end;

function TfbankingDialog.Execute: Integer;
begin
  if not Assigned(Self) then
    begin
      Application.CreateForm(TfBankingDialog,fBankingDialog);
      Self := fBankingDialog;
    end;
  Result := -1;
  if Showmodal = mrOK then
    Result := res;
end;

end.

