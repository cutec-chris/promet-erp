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
Created 19.09.2013
*******************************************************************************}
unit uAttStatistic;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ButtonPanel,
  StdCtrls,gsGanttCalendar;

type
  TfAttStatistic = class(TForm)
    ButtonPanel1: TButtonPanel;
    lbItems: TListBox;
  private
    { private declarations }
  public
    { public declarations }
    procedure Execute(aInterval : TInterval);
  end;

var
  fAttStatistic: TfAttStatistic;

implementation
uses uTaskPlan;
{$R *.lfm}

function RemoveWeekends(aStart,aEnd : TDateTime) : Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := trunc(aStart) to Trunc(aEnd) do
    if not ((DayOfWeek(i)=1) or (DayOfWeek(i)=7)) then
      Result := Result+1;
end;

procedure TfAttStatistic.Execute(aInterval: TInterval);
var
  i: Integer;
  y: word;
  m: word;
  d: word;
  YS: TDateTime;
begin
  if not Assigned(Self) then
    begin
      Application.CreateForm(TfAttStatistic,fAttStatistic);
      Self := fAttStatistic;
    end;
  DecodeDate(Now(),y,m,d);
  YS := EncodeDate(y,1,1);
  lbItems.Clear;
  if aInterval.Color<>clRed then exit;
  Caption := aInterval.Task;
  if Assigned(aInterval.Pointer) then
    begin
      for i := 0 to TInterval(aInterval.Pointer).IntervalCount-1 do
        with TInterval(aInterval.Pointer).Interval[i] as tBackInterval do
          begin
            if (TInterval(aInterval.Pointer).Interval[i].StartDate>=YS) and (TInterval(aInterval.Pointer).Interval[i].Project<>'') then
              lbItems.Items.Values[TInterval(aInterval.Pointer).Interval[i].Project] := FloatToStr(StrToFloatDef(lbItems.Items.Values[TInterval(aInterval.Pointer).Interval[i].Project],0)+(RemoveWeekends(TInterval(aInterval.Pointer).Interval[i].StartDate,TInterval(aInterval.Pointer).Interval[i].FinishDate)));
          end;
    end;
  fAttStatistic.ShowModal;
end;

end.

