unit upasswordgenerate;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Spin, ExtCtrls, Buttons, ButtonPanel;

type
  TfPWGenerate = class(TForm)
    Bevel1: TBevel;
    bShowPW: TBitBtn;
    ButtonPanel1: TButtonPanel;
    cbFixedSize: TCheckBox;
    cbLowercase: TCheckBox;
    cbUppercase: TCheckBox;
    cbNumbers: TCheckBox;
    cbUni: TCheckBox;
    cbSpecial: TCheckBox;
    cbOther: TCheckBox;
    ePassword: TEdit;
    Edit2: TEdit;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    Image1: TImage;
    Label1: TLabel;
    Label2: TLabel;
    seMinLength: TSpinEdit;
    seMaxLength: TSpinEdit;
    procedure bShowPWClick(Sender: TObject);
    procedure cbFixedSizeChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure DoGenerate(Sender: TObject);
    procedure seMinLengthChange(Sender: TObject);
  private
    { private declarations }
    procedure Generate;
  public
    { public declarations }
    procedure SetLanguage;
    function Execute : Boolean;
  end;

var
  fPWGenerate: TfPWGenerate;

implementation
uses pwgenerator;
{$R *.lfm}

procedure TfPWGenerate.FormCreate(Sender: TObject);
begin
  seMinLengthChange(nil);
  cbFixedSizeChange(nil);
end;

procedure TfPWGenerate.DoGenerate(Sender: TObject);
begin
  Generate;
end;

procedure TfPWGenerate.seMinLengthChange(Sender: TObject);
begin
  seMaxLength.MinValue:= seMinLength.Value;
  if cbFixedSize.checked AND (seMaxLength.value <> seMinLength.value) then begin
     seMaxLength.Value:= seMinLength.Value;
  end;

  if seMinLength.Value > seMaxLength.Value then
     seMaxLength.Value:= seMinLength.Value;

  if seMinLength.Value <= 7 then image1.Visible := true
  else image1.visible := false;
  Generate;
end;

procedure TfPWGenerate.Generate;
var
  chars:string;
begin

  chars :='';
  ApwGen.LowerCases:=false;
  ApwGen.UpperCases:=false;
  ApwGen.Numbers:=false;
  ApwGen.Specials:=false;
  ApwGen.CustomChars:='';

  Apwgen.MinSize:=seMinLength.value;
  Apwgen.MaxSize:=seMaxLength.value;

  if cbLowercase.Checked then
    ApwGen.LowerCases:=true;

  if cbUppercase.Checked then
    ApwGen.UpperCases:=true;

  if cbNumbers.Checked then
    ApwGen.Numbers:=true;

  if cbSpecial.Checked then
    ApwGen.Specials:=true;

  if cbOther.checked then
    ApwGen.CustomChars:=Edit2.Text;

  ePassword.text := GeneratePWD(Apwgen);
end;

procedure TfPWGenerate.SetLanguage;
begin
  if not Assigned(Self) then
    begin
      Application.CreateForm(TfPWGenerate,fPWGenerate);
      Self := fPWGenerate;
    end;
end;

function TfPWGenerate.Execute: Boolean;
begin
  if not Assigned(Self) then
    begin
      Application.CreateForm(TfPWGenerate,fPWGenerate);
      Self := fPWGenerate;
    end;
  Generate;
  Result := Showmodal = mrOK;
end;

procedure TfPWGenerate.cbFixedSizeChange(Sender: TObject);
begin
  if cbFixedSize.checked then begin
    Label2.enabled := false;
    seMaxLength.Enabled:= false;
    seMaxLength.Value:= seMinLength.Value;
  end else begin
    Label2.enabled := true;
    seMaxLength.Enabled:= true;
  end;

end;

procedure TfPWGenerate.bShowPWClick(Sender: TObject);
begin
  if ePassword.PasswordChar = #0 then
    ePassword.PasswordChar:= '*'
  else
    ePassword.PasswordChar:= #0;
end;

end.

