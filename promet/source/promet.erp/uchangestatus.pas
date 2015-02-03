{*******************************************************************************
  Copyright (C) Christian Ulrich info@cu-tec.de

  This source is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 2 of the License, or commercial alternative
  contact us for more information

  This code is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
  to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
  MA 02111-1307, USA.
*******************************************************************************}

unit uChangeStatus;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,  Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons,
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
{$R *.lfm}
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

end.
