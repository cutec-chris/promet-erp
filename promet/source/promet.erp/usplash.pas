{*******************************************************************************
Dieser Sourcecode darf nicht ohne gültige Geheimhaltungsvereinbarung benutzt werden
und ohne gültigen Vertriebspartnervertrag weitergegeben oder kommerziell verwertet werden.
You have no permission to use this Source without valid NDA
and copy it without valid distribution partner agreement
Christian Ulrich
info@cu-tec.de
Created 01.06.2006
*******************************************************************************}
unit usplash;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  uIntfStrConsts, StdCtrls, ComCtrls;

type

  { TfSplash }

  TfSplash = class(TForm)
    Image3: TImage;
    lRegistered: TLabel;
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
procedure TfSplash.FormCreate(Sender: TObject);
begin
  lregistered.Caption := '';
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
  {$I usplash.lrs}

end.

