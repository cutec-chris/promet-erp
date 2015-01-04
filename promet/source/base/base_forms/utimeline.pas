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
unit uTimeLine;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Graphics, Utils;

type
  TTLOrientation = (toHorizontal,toVertical);

  { TTimeLine }

  TTimeLine = class(TCustomControl)
  private
    FInc: real;
    FMarkerDate: TDateTime;
    FOnSetMarker: TNotifyEvent;
    Forientation: TTLOrientation;
    FPDate: TDateTime;
    FTmpDate: TDateTime;
    FDate: TDateTime;
    FDownPos : Integer;
    FDayWidth : Integer;
    Bitmap : TBitmap;
    FUseLongMonth: Boolean;
    function GetDateRange: Integer;
    procedure SetDate(const AValue: TDateTime);
    procedure SetMarkerDate(const AValue: TDateTime);
    procedure DoRefreshImage;
    procedure SetPointerDate(AValue: TDateTime);
    procedure SetUseLongMonth(AValue: Boolean);
  protected
    procedure AllAutoSized; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
    procedure MouseDown(Button: TMouseButton; Shift:TShiftState; X,Y:Integer); override;
    procedure MouseMove(Shift: TShiftState; X,Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift:TShiftState; X,Y:Integer); override;
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean; override;
  published
    property StartDate : TDateTime read FDate write SetDate;
    property MarkerDate : TDateTime read FMarkerDate write SetMarkerDate;
    property PointerDate : TDateTime read FPDate write SetPointerDate;
    property Increment : real read FInc write FInc;
    property Orientation : TTLOrientation read Forientation write FOrientation;
    property OnSetMarker : TNotifyEvent read FOnSetMarker write FOnSetMarker;
    property UseLongMonth : Boolean read FUseLongMonth write SetUseLongMonth;
    property DateRange : Integer read GetDateRange;
  end;

implementation

{ TTimeLine }

procedure TTimeLine.SetDate(const AValue: TDateTime);
begin
  if FDate=AValue then exit;
  FDate:=AValue;
  FTmpDate := AValue;
  DoRefreshImage;
  Invalidate;
end;

function TTimeLine.GetDateRange: Integer;
var
  CtrlWidth: Integer;
begin
  if FOrientation = toHorizontal then
    CtrlWidth := Height
  else
    CtrlWidth := Width;
  Result := round(CtrlWidth/FDayWidth);
end;

procedure TTimeLine.SetMarkerDate(const AValue: TDateTime);
begin
  if FMarkerDate=AValue then exit;
  FMarkerDate:=AValue;
  DoRefreshImage;
  Invalidate;
end;

procedure TTimeLine.DoRefreshImage;
const
  FontColor = clGrayText;//clHighlightText;
var
  x: real = 0;
  y: Integer = 0;
  CtrlWidth: LongInt;
  Year: word = 0;
  Month: word = 0;
  ActDate: Double;
  aYear: word;
  aMonth: word;
  aDay: word;
  Markerthere : Boolean = False;
  Day: Word;
begin
//  if not Visible then exit;
  // Initializes the Bitmap Size
  Bitmap.Height := Height;
  Bitmap.Width := Width;
  // Draws the background
  with Bitmap.Canvas do
    begin
      Font.Height:=11;
      Font.Style:=[];
      fDayWidth := TextExtent('33').cx+4;
      Brush.Color := Color;
      Pen.Color := Color;
      Bitmap.Canvas.Rectangle(0, 0, Bitmap.Width, Bitmap.Height);
      if FOrientation = toHorizontal then
        Font.Orientation := 900;
      if FOrientation = toHorizontal then
        CtrlWidth := Height
      else
        CtrlWidth := Width;
      ActDate := FTmpDate;
      while x < CtrlWidth do
        begin
          y := 0;
          Font.Style:=[fsBold];
          Font.Color:=FontColor;
          DecodeDate(ActDate,aYear,aMonth,aDay);
          Font.Height:=20;
          if aYear <> Year then
            begin
              if FOrientation = toHorizontal then
                TextOut(y,round(x+TextExtent(IntToStr(aYear)).cx),IntToStr(aYear))
              else
                TextOut(round(x),y,IntToStr(aYear));
              Year := aYear;
            end;
          inc(y,TextExtent('HY').cy);
          Font.Height:=16;
          if aMonth <> Month then
            begin
              if (FOrientation = toHorizontal) then
                begin
                  if FUseLongMonth then
                    TextOut(y,round(x+TextExtent(SysToUni(LongMonthNames[aMonth])).cx),SysToUni(LongMonthNames[aMonth]))
                  else
                    TextOut(y,round(x+TextExtent(SysToUni(ShortMonthNames[aMonth])).cx),SysToUni(ShortMonthNames[aMonth]));
                end
              else
                TextOut(round(x),y,SysToUni(ShortMonthNames[aMonth]));
              Month := aMonth;
            end;
          inc(y,TextExtent('HY').cy);
          Font.Height:=11;
          Font.Color:=FontColor;
          Font.Style:=[];
          if aDay <> Day then
            begin
              if (aDay = 1)
              or (aDay = 10)
              or (aDay = 20)
              or (aDay = 30)
              then
                begin
                  if FOrientation = toHorizontal then
                    TextOut(y,round(x),IntToStr(aDay))
                  else
                    TextOut(round(x),y,IntToStr(aDay));
                end;
              if ((ActDate >= FMarkerDate) and (ActDate+FInc <= FMarkerDate)) and not MarkerThere then
                begin
                  Markerthere:=True;
                  Pen.Color := clHighlight;
                  if FOrientation = toHorizontal then
                    begin
                      MoveTo(0,round(x)-1);
                      LineTo(y+TextExtent('HY').cy,round(x)-1);
                      MoveTo(0,round(x));
                      LineTo(y+TextExtent('HY').cy,round(x));
                    end
                  else
                    begin
                      MoveTo(round(x)-1,0);
                      LineTo(round(x)-1,y+TextExtent('HY').cy);
                      MoveTo(round(x),0);
                      LineTo(round(x),y+TextExtent('HY').cy);
                    end;
                  Pen.Color := clBtnFace;
                end;
              Day := aDay;
            end;
          if (FDayWidth+FInc)>0 then
            x += FDayWidth+FInc
          else x+=1;
          if FOrientation = toHorizontal then
            begin
              MoveTo(y-3,round(x)-2);
              LineTo(y+TextExtent('HY').cy,round(x)-2);
            end
          else
            begin
              MoveTo(round(x)-2,y-3);
              LineTo(round(x)-2,y+TextExtent('HY').cy);
            end;
          if ActDate = FPDate then
            begin
              Brush.Color:=clBlack;
              Pen.Color:=clBtnFace;
              Polygon([Point(0, y-5), Point(8, y), Point(0, y+5)]);
              Brush.Color := Color;
            end;
          ActDate := ActDate-1;
        end;
    end;
end;

procedure TTimeLine.SetPointerDate(AValue: TDateTime);
begin
  if FPDate=AValue then Exit;
  FPDate:=AValue;
  DoRefreshImage;
  Invalidate;
end;

procedure TTimeLine.SetUseLongMonth(AValue: Boolean);
begin
  if FUseLongMonth=AValue then Exit;
  FUseLongMonth:=AValue;
end;

procedure TTimeLine.AllAutoSized;
begin
  inherited AllAutoSized;
  DoRefreshImage;
end;

constructor TTimeLine.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FInc := -4;
  Bitmap := TBitmap.Create;
end;

destructor TTimeLine.Destroy;
begin
  Bitmap.Free;
  inherited Destroy;
end;

procedure TTimeLine.Paint;
begin
  if Width <> Bitmap.Width then
    DorefreshImage;
  Canvas.Draw(0, 0, Bitmap);
  inherited Paint;
end;

procedure TTimeLine.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  aFact: Extended;
begin
  inherited MouseDown(Button, Shift, X, Y);
  if FOrientation = toHorizontal then
    FDownPos := y
  else
    FDownPos := x;
  if ssRight in Shift then
    begin
    end
  else if (ssLeft in Shift) then
    begin
      aFact := (FDayWidth+FInc);
      if aFact <= 0 then aFact := 1;
      FMarkerDate := FDate-round(FDownPos/aFact)-15;
      DoRefreshImage;
      Invalidate;
      if Assigned(FOnSetMarker) then
        FOnSetMarker(Self);
    end;
end;

procedure TTimeLine.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  if (ssRight in Shift) and (FDownPos > 0) then
    begin
      if FOrientation = toHorizontal then
        FTmpDate := FDate+(round((FDownPos-Y)/FDayWidth)*FInc)
      else
        FTmpDate := FDate+(round((FDownPos-X)/FDayWidth)*FInc);
      DoRefreshImage;
      Invalidate;
    end;
  inherited MouseMove(Shift, X, Y);
end;

procedure TTimeLine.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  if (ssRight in Shift) and (FDownPos > 0) then
    begin
      FDownPos := 0;
      FDate := FTmpDate;
    end;
  inherited MouseUp(Button, Shift, X, Y);
end;

function TTimeLine.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
  MousePos: TPoint): Boolean;
begin
  FTmpDate := FtmpDate+((WheelDelta div 15))*round(-(FInc / 4));
  FDate := FTmpDate;
  DoRefreshImage;
  Invalidate;
  Result := inherited;
end;

end.
