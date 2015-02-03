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
Created 01.06.2006
*******************************************************************************}
unit uDocumentAddOptions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,  Forms, Controls, Graphics, Dialogs, Calendar,
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
{$R *.lfm}
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

end.

