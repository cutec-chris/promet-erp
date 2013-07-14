{*******************************************************************************
Dieser Sourcecode darf nicht ohne gültige Geheimhaltungsvereinbarung benutzt werden
und ohne gültigen Vertriebspartnervertrag weitergegeben werden.
You have no permission to use this Source without valid NDA
and copy it without valid distribution partner agreement
Christian Ulrich
info@cu-tec.de
Created 01.06.2006
*******************************************************************************}
unit uFormAnimate;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ExtCtrls, Forms, Controls;

type

  { TAnimationController }

  TAnimationController = class(TComponent)
  private
    FanimationTime: Integer;
    FSteps: LongWord;
    FStepHeight: Integer;
    FTargetHeight: LongInt;
    procedure aTimerTimer(Sender: TObject);
  public
    property AnimationTime : Integer read FanimationTime write FAnimationTime;
    constructor Create(AOwner : TComponent);override;
    procedure AnimateControlHeight(TargetHeight: Integer);
  end;

implementation

uses LCLIntf;

{ AnimationController }

procedure TAnimationController.aTimerTimer(Sender: TObject);
var
  Start: LongWord;
begin
  dec(FSteps);
  TControl(Owner).Height := TControl(Owner).Height + FStepHeight;
  if FSteps < 1 then
    begin
      TTimer(Sender).Enabled:=False;
      Start := GetTickCount;
      TWinControl(Owner).BeginUpdateBounds;
      TControl(Owner).Height := FTargetHeight;
      TWinControl(Owner).EndUpdateBounds;
      Start := GetTickCount - Start;
    end;
end;

constructor TAnimationController.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAnimationTime := 100;
end;

procedure TAnimationController.AnimateControlHeight(TargetHeight: Integer);
var
  aTimer: TTimer;
begin
  {.$IFDEF LCLgtk2}
  //TControl(Owner).Height := TargetHeight;
  {.$ELSE}
  aTimer := TTimer.Create(nil);
  aTimer.Interval:=20;
  FSteps := FAnimationTime div aTimer.Interval;
  FTargetHeight := TargetHeight;
  FStepHeight := (TargetHeight-TControl(Owner).Height) div FSteps;
  aTimer.OnTimer:=@aTimerTimer;
  aTimer.Enabled:=True;
  aTimer.Tag:=TargetHeight;
  while aTimer.Enabled do
    begin
      try
        Application.Processmessages;
      except
      end;
    end;
  aTimer.Free;
  {.$ENDIF}
end;

end.

