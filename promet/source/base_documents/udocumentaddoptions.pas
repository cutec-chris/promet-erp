{*******************************************************************************
Dieser Sourcecode darf nicht ohne gültige Geheimhaltungsvereinbarung benutzt werden
und ohne gültigen Vertriebspartnervertrag weitergegeben oder kommerziell verwertet werden.
You have no permission to use this Source without valid NDA
and copy it without valid distribution partner agreement
Christian Ulrich
info@cu-tec.de
Created 01.06.2006
*******************************************************************************}
unit uDocumentAddOptions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, Calendar,
  StdCtrls, ExtCtrls, Buttons, uIntfStrConsts, EditBtn, LCLType, ButtonPanel,
  ZVDateTimePicker;

type

  { TfDocumentAddOptions }

  TfDocumentAddOptions = class(TForm)
    ButtonPanel1: TButtonPanel;
    cbDeletefromFilesystem: TCheckBox;
    cbAddToMessages: TCheckBox;
    ccCalendar: TZVDateTimePicker;
    eName: TEdit;
    Image1: TImage;
    lName: TLabel;
    lOptions: TLabel;
    lCheckindate: TLabel;
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    procedure SetLanguage;
    function Execute : Boolean;
  end; 

var
  fDocumentAddOptions: TfDocumentAddOptions;

implementation

{ TfDocumentAddOptions }

procedure TfDocumentAddOptions.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
    begin
      Key := 0;
      Close;
    end;
end;

procedure TfDocumentAddOptions.FormShow(Sender: TObject);
begin
  if eName.Enabled then
    begin
      eName.SetFocus;
      eName.SelectAll;
    end;
end;

procedure TfDocumentAddOptions.SetLanguage;
begin
  if not Assigned(Self) then
    begin
      Application.CreateForm(TfDocumentAddOptions,fDocumentAddOptions);
      Self := fDocumentAddOptions;
    end;
end;

function TfDocumentAddOptions.Execute: Boolean;
var
  ActControl: TWinControl;
begin
  if not Assigned(Self) then
    begin
      Application.CreateForm(TfDocumentAddOptions,fDocumentAddOptions);
      Self := fDocumentAddOptions;
    end;
  ActControl := Screen.ActiveControl;
  Result := Showmodal = mrOK;
  try
    if Assigned(ActControl) and ActControl.CanFocus then ActControl.SetFocus;
  except
  end;
end;

initialization
  {$I udocumentaddoptions.lrs}

end.

