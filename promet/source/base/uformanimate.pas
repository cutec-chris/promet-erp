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
  aTimer := TTimer.Create(Self);
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

