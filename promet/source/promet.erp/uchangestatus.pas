{*******************************************************************************
Dieser Sourcecode darf nicht ohne gültige Geheimhaltungsvereinbarung benutzt werden
und ohne gültigen Vertriebspartnervertrag weitergegeben oder kommerziell verwertet werden.
You have no permission to use this Source without valid NDA
and copy it without valid distribution partner agreement
CU-TEC Christian Ulrich
info@cu-tec.de
*******************************************************************************}

unit uChangeStatus;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons,
  uIntfStrConsts,LCLType;

type

  { TfChangeStatus }

  TfChangeStatus = class(TForm)
    bAbort: TBitBtn;
    bOK: TBitBtn;
    lOrder: TLabel;
    lChange: TLabel;
    lTarget: TLabel;
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    { private declarations }
  public
    { public declarations }
    function Execute : Boolean;
    procedure SetLanguage;
  end; 

var
  fChangeStatus: TfChangeStatus;

implementation

{ TfChangeStatus }

procedure TfChangeStatus.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
    begin
      Key := 0;
      Close;
    end;
end;

function TfChangeStatus.Execute: Boolean;
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

procedure TfChangeStatus.SetLanguage;
begin
  if not Assigned(Self) then
    begin
      Application.CreateForm(TfChangeStatus,fChangeStatus);
      Self := fChangeStatus;
    end;
end;

initialization
  {$I uchangestatus.lrs}

end.
