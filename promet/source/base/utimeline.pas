{*******************************************************************************
Dieser Sourcecode darf nicht ohne gültige Geheimhaltungsvereinbarung benutzt werden
und ohne gültigen Vertriebspartnervertrag weitergegeben werden.
You have no permission to use this Source without valid NDA
and copy it without valid distribution partner agreement
Christian Ulrich
info@cu-tec.de
Created 01.06.2006
*******************************************************************************}
unit uTimeLine;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Graphics, FileUtil;

type
  TTLOrientation = (toHorizontal,toVertical);

  { TTimeLine }

  TTimeLine = class(TCustomControl)
  private
    FInc: Integer;
    FMarkerDate: TDateTime;
    FOnSetMarker: TNotifyEvent;
    Forientation: TTLOrientation;
    FTmpDate: TDateTime;
    FDate: TDateTime;
    FDownPos : Integer;
    FDayWidth : Integer;
    Bitmap : TBitmap;
    FUseLongMonth: Boolean;
    procedure SetDate(const AValue: TDateTime);
    procedure SetMarkerDate(const AValue: TDateTime);
    procedure DoRefreshImage;
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
    property Increment : Integer read FInc write FInc;
    property Orientation : TTLOrientation read Forientation write FOrientation;
    property OnSetMarker : TNotifyEvent read FOnSetMarker write FOnSetMarker;
    property UseLongMonth : Boolean read FUseLongMonth write SetUseLongMonth;
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
  x: Integer = 0;
  y: Integer = 0;
  CtrlWidth: LongInt;
  Year: word = 0;
  Month: word = 0;
  ActDate: Double;
  aYear: word;
  aMonth: word;
  aDay: word;
  Markerthere : Boolean = False;
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
      fDayWidth := TextExtent('31').cx+4;
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
                TextOut(y,x+TextExtent(IntToStr(aYear)).cx,IntToStr(aYear))
              else
                TextOut(x,y,IntToStr(aYear));
              Year := aYear;
            end;
          inc(y,TextExtent('HY').cy);
          Font.Height:=16;
          if aMonth <> Month then
            begin
              if (FOrientation = toHorizontal) then
                begin
                  if FUseLongMonth then
                    TextOut(y,x+TextExtent(SysToUTF8(LongMonthNames[aMonth])).cx,SysToUTF8(LongMonthNames[aMonth]))
                  else
                    TextOut(y,x+TextExtent(SysToUTF8(ShortMonthNames[aMonth])).cx,SysToUTF8(ShortMonthNames[aMonth]));
                end
              else
                TextOut(x,y,SysToUTF8(ShortMonthNames[aMonth]));
              Month := aMonth;
            end;
          inc(y,TextExtent('HY').cy);
          Font.Height:=11;
          Font.Color:=FontColor;
          Font.Style:=[];
          inc(x,1);
          if FOrientation = toHorizontal then
            TextOut(y,x,IntToStr(aDay))
          else
            TextOut(x,y,IntToStr(aDay));
          if ((ActDate >= FMarkerDate) and (ActDate+FInc <= FMarkerDate)) and not MarkerThere then
            begin
              Markerthere:=True;
              Pen.Color := clHighlight;
              if FOrientation = toHorizontal then
                begin
                  MoveTo(0,x-1);
                  LineTo(y+TextExtent('HY').cy,x-1);
                  MoveTo(0,x);
                  LineTo(y+TextExtent('HY').cy,x);
                end
              else
                begin
                  MoveTo(x-1,0);
                  LineTo(x-1,y+TextExtent('HY').cy);
                  MoveTo(x,0);
                  LineTo(x,y+TextExtent('HY').cy);
                end;
              Pen.Color := clBtnFace;
            end;
          inc(x,fDayWidth);
          if FOrientation = toHorizontal then
            begin
              MoveTo(y-3,x-2);
              LineTo(y+TextExtent('HY').cy,x-2);
            end
          else
            begin
              MoveTo(x-2,y-3);
              LineTo(x-2,y+TextExtent('HY').cy);
            end;
          ActDate := ActDate+FInc;

        end;
    end;
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
begin
  inherited MouseDown(Button, Shift, X, Y);
  if ssRight in Shift then
    begin
      if FOrientation = toHorizontal then
        FDownPos := y
      else
        FDownPos := x;
    end
  else if (ssLeft in Shift) then
    begin
      if FOrientation = toHorizontal then
        FMarkerDate := FDate-(round((FDownPos-Y)/FDayWidth)*FInc)+1
      else
        FMarkerDate := FDate-(round((FDownPos-X)/FDayWidth)*FInc)+1;
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
  FTmpDate := FtmpDate+((WheelDelta div 15))*(-(FInc div 4));
  FDate := FTmpDate;
  DoRefreshImage;
  Invalidate;
  Result := inherited;
end;

end.
