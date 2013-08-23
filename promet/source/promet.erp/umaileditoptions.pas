{*******************************************************************************
Dieser Sourcecode darf nicht ohne gültige Geheimhaltungsvereinbarung benutzt werden
und ohne gültigen Vertriebspartnervertrag weitergegeben oder kommerziell verwertet werden.
You have no permission to use this Source without valid NDA
and copy it without valid distribution partner agreement
CU-TEC Christian Ulrich
info@cu-tec.de
*******************************************************************************}

unit uMailEditoptions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Buttons, uIntfStrConsts,LCLType, ButtonPanel;

type

  { TfMailOptions }

  TfMailOptions = class(TForm)
    bpButtons: TButtonPanel;
    cbArchive: TCheckBox;
    cbDelete: TCheckBox;
    eName: TEdit;
    eMailAddr: TEdit;
    eServertype: TComboBox;
    eUsername: TEdit;
    eServer: TEdit;
    ePassword: TEdit;
    lName: TLabel;
    lMailAddr: TLabel;
    lServertype: TLabel;
    lUsername: TLabel;
    lServer: TLabel;
    lPassword: TLabel;
    pPOPOptions: TPanel;
    pSMTPOptions: TPanel;
    procedure eServertypeSelect(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    { private declarations }
  public
    { public declarations }
    function Execute : Boolean;
    procedure SetLanguage;
  end;

var
  fMailOptions: TfMailOptions;

implementation

resourcestring
  strFeed                       = 'Feed';


{ TfMailOptions }

procedure TfMailOptions.eServertypeSelect(Sender: TObject);
begin
  pSMTPOptions.Visible := eServertype.Text = 'SMTP';
  pPOPOptions.Visible := eServertype.Text = 'POP3';
  if eServertype.Text = 'FEED' then
    begin
      lServer.Caption := strFeed;
      eUsername.Visible:=False;
      ePassword.Visible:=False;
      lUsername.Visible:=False;
      lPassword.Visible:=False;
    end
  else
    begin
      lServer.Caption := strServer;
      eUsername.Visible:=True;
      ePassword.Visible:=True;
      lUsername.Visible:=True;
      lPassword.Visible:=True;
    end;
end;

procedure TfMailOptions.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
    begin
      Key := 0;
      Close;
    end;
end;

function TfMailOptions.Execute: Boolean;
var
  ActControl: TWinControl;
begin
  ActControl := Screen.ActiveControl;
  Result := Showmodal = mrOK;
  try
    if Assigned(ActControl) and ActControl.CanFocus then ActControl.SetFocus;
  except
  end;
end;

procedure TfMailOptions.SetLanguage;
begin
  if not Assigned(Self) then
    begin
      Application.CreateForm(TfMailOptions,fMailOptions);
      Self := fMailOptions;
    end;
end;

initialization
  {$I umaileditoptions.lrs}

end.

