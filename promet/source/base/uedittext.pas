{*******************************************************************************
Dieser Sourcecode darf nicht ohne gültige Geheimhaltungsvereinbarung benutzt werden
und ohne gültigen Vertriebspartnervertrag weitergegeben werden.
You have no permission to use this Source without valid NDA
and copy it without valid distribution partner agreement
Christian Ulrich
info@cu-tec.de
Created 13.01.2013
*******************************************************************************}
unit uEditText;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, db, FileUtil, Forms, Controls, Graphics, Dialogs,
  ButtonPanel, DbCtrls, StdCtrls;

type
  TfEditText = class(TForm)
    ButtonPanel1: TButtonPanel;
    mText: TMemo;
  private
    { private declarations }
  public
    { public declarations }
    procedure SetLanguage;
  end;

var
  fEditText: TfEditText;

implementation

{$R *.lfm}

procedure TfEditText.SetLanguage;
begin
  if not Assigned(Self) then
    begin
      Application.CreateForm(TfEditText,fEditText);
      Self := fEditText;
    end;
end;

end.

