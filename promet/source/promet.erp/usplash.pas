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
unit usplash;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,  Forms, Controls, Graphics, Dialogs, ExtCtrls,
  uIntfStrConsts, StdCtrls, ComCtrls;

type

  { TfSplash }

  TfSplash = class(TForm)
    Image3: TImage;
    lStartup: TLabel;
    lVersion: TLabel;
    Panel1: TPanel;
    pgProgress: TProgressBar;
    procedure FormCreate(Sender: TObject);
    procedure HideTimerTimer(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    HideTimer : TTimer;
    procedure SetLanguage;
    procedure AddText(AText : string);
    procedure SetPercent(Value : Integer);
  end;

var
  fSplash: TfSplash;
implementation
uses uBaseApplication;
{$R *.lfm}
procedure TfSplash.FormCreate(Sender: TObject);
begin
  HideTimer := TTimer.Create(Self);
  HideTimer.Enabled := False;
  HideTimer.OnTimer:=@HideTimerTimer;
end;

procedure TfSplash.HideTimerTimer(Sender: TObject);
begin
  Close;
end;

procedure TfSplash.SetLanguage;
begin
end;

procedure TfSplash.AddText(AText: string);
begin
  lStartup.Caption := lStartUp.Caption+lineending+AText;
  Update;
  with Application as IBaseApplication do
    Info(AText);
  Application.Processmessages;
end;

procedure TfSplash.SetPercent(Value: Integer);
begin
  PgProgress.Position := Value;
{  if (Value > 15) and (Image1.Visible) then
    begin
      Image1.Visible := False;
      Image2.Visible := True;
      Update;
      Application.Processmessages;
    end;
  if (Value > 60) and (Image2.Visible) then
    begin
      Image2.Visible := False;
      Image3.Visible := True;
      Update;
      Application.Processmessages;
    end;}
end;

initialization

end.

