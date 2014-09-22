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
unit uLogWait;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,  Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons, ButtonPanel, uIntfStrConsts;

type

  { TfLogWaitForm }

  TfLogWaitForm = class(TForm)
    bAbort: TBitBtn;
    lbLog: TListBox;
    procedure FormShow(Sender: TObject);
    procedure ShowInfo(Info: string);
  private
    { private declarations }
  public
    { public declarations }
    procedure SetLanguage;
  end;

var
  fLogWaitForm: TfLogWaitForm;

implementation
{$R *.lfm}
procedure TfLogWaitForm.ShowInfo(Info: string);
begin
  if not Assigned(Self) then
    begin
      Application.CreateForm(TfLogWaitForm,fLogWaitform);
      Self := fLogWaitForm;
    end;
  if trim(info) = '' then exit;
  lbLog.Items.Add(StringReplace(StringReplace(Info,#10,'',[rfreplaceAll]),#13,'',[rfreplaceAll]));
  lbLog.ItemIndex := lbLog.Items.Count-1;
  lbLog.MakeCurrentVisible;
  Application.Processmessages;
end;

procedure TfLogWaitForm.SetLanguage;
begin
  if not Assigned(Self) then
    begin
      Application.CreateForm(TfLogWaitForm,fLogWaitform);
      Self := fLogWaitForm;
    end;
  bAbort.Kind:=bkAbort;
end;

procedure TfLogWaitForm.FormShow(Sender: TObject);
begin
  if not Assigned(Self) then
    begin
      Application.CreateForm(TfLogWaitForm,fLogWaitform);
      Self := fLogWaitForm;
    end;
  lbLog.Items.Clear;
end;

initialization

end.

