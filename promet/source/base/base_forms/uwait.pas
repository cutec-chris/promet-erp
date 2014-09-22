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

unit uwait;

{$mode objfpc}{$H+}

interface

uses
  classes, sysutils,  forms, controls, graphics, dialogs, StdCtrls,
  uIntfStrConsts, ExtCtrls, LMessages, Fileutil, ComCtrls;

type

  { TfWaitForm }

  TfWaitForm = class(tform)
    Image1: TImage;
    lStep: tlabel;
    lPleaseWait: tlabel;
    ProgressBar1: TProgressBar;
  private
    { private declarations }
    procedure WMCloseQuery(var message: TLMessage); message LM_CLOSEQUERY;
  public
    { public declarations }
    procedure ShowInfo(Info : string);
    procedure SetLanguage;
  end; 

var
  fWaitForm: TfWaitForm;

implementation
{$R *.lfm}
procedure TfWaitForm.WMCloseQuery(var message: TLMessage);
begin //Workaround for #0012552
  Close;
end;

procedure TfWaitForm.ShowInfo(Info: string);
begin
  if not Assigned(Self) then
    begin
      Application.CreateForm(TfWaitForm,fWaitForm);
      Self := fWaitForm;
      BringToFront;
    end;
  lStep.Caption := info;
  Application.Processmessages;
end;

procedure TfWaitForm.SetLanguage;
begin
  if not Assigned(Self) then
    begin
      Application.CreateForm(TfWaitForm,fWaitForm);
      Self := fWaitForm;
    end;
end;

initialization

end.
