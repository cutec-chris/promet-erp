unit uProjectFlow;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, types, Controls, Graphics, FileUtil,uColors,gsGanttCalendar,
  LCLproc,LCLIntf,LCLType,LMessages;
type

  { TPJSection }

  { TPFSection }

  TPFSection = class
  public
    Start : TDateTime;
    Ended : TDateTime;
    Name : string;
    Color : TColor;
    constructor Create(aStart, aEnd: TDateTime; aName: string;aColor : TColor);
  end;

  { TProjectFlow }

  TProjectFlow = class(TCustomControl)
  private
    FEndDate: TDateTime;
    FMajorScale: TTimeScale;
    FMinorScale: TTimeScale;
    FSections : TList;
    FStartDate: TDateTime;
    FDayWidth : Extended;
    FPointWidth: Integer;
    FTop : Integer;
    FCurrentDate: TDateTime;
    FRecalcNeeded : Boolean;
    procedure CalcParams;
    function GetCount: Integer;
    function GetDayOfWeek: Integer;
    function GetDays: Integer;
    function GetHalfYears: Integer;
    function GetHours: Integer;
    function GetMajorScale: Integer;
    function GetMinorScale: Integer;
    function GetMinutes: Integer;
    function GetMonthes: Integer;
    function getPixelsPerMinorScale: Integer;
    function GetQuarters: Integer;
    function GetSeconds: Integer;
    function GetSection(aIdx : Integer): TPFSection;
    procedure DrawSection(aSection : TPFSection);
    function GetWeeks: Integer;
    function GetYears: Integer;
    procedure DrawMinorScale;
    procedure DrawMajorScale;
    procedure DrawHeaderLines;
    function IsNewPeriod(AScale: TTimeScale; AUseSub: Boolean = False): Boolean;
    procedure SetEndDate(AValue: TDateTime);
    procedure SetMajorScale(AValue: TTimeScale);
    procedure SetMinorScale(AValue: TTimeScale);
    procedure SetStartDate(AValue: TDateTime);
  protected
    procedure BoundsChanged; override;
    property PixelsPerMinorScale: Integer read getPixelsPerMinorScale;
    property MajorScaleHeight: Integer read GetMajorScale;
    property MinorScaleHeight: Integer read GetMinorScale;
    property Seconds: Integer read GetSeconds;
    property Minutes: Integer read GetMinutes;
    property Hours: Integer read GetHours;
    property Days: Integer read GetDays;
    property DayOfWeek: Integer read GetDayOfWeek;
    property Weeks: Integer read GetWeeks;
    property Monthes: Integer read GetMonthes;
    property Quarters: Integer read GetQuarters;
    property HalfYears: Integer read GetHalfYears;
    property Years: Integer read GetYears;
  public
    procedure Paint; override;
    property Sections[aIdx : Integer] : TPFSection read GetSection;
    procedure AddSection(aStart,aEnd : TDateTime;aName : string;aColor : TColor);
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Count : Integer read GetCount;
    property StartDate : TDateTime read FStartDate write SetStartDate;
    property EndDate : TDateTime read FEndDate write SetEndDate;
    property MajorScale: TTimeScale read FMajorScale write SetMajorScale default tsWeekNum;
    property MinorScale: TTimeScale read FMinorScale write SetMinorScale default tsDay;
  end;

implementation

{ TPJSection }

constructor TPFSection.Create(aStart, aEnd: TDateTime; aName: string;
  aColor: TColor);
begin
  Start:=aStart;
  Ended:=aEnd;
  Name := aName;
  Color := aColor;
end;

{ TProjectFlow }

function TProjectFlow.GetSection(aIdx : Integer): TPFSection;
begin
  Result := TPFSection(FSections[aIdx]);
end;

procedure TProjectFlow.DrawSection(aSection: TPFSection);
var
  aLeft: Integer;
  aRight: Integer;
  te: TSize;
begin
  aLeft := round((aSection.Start-StartDate)*FDayWidth);
  aRight := round((aSection.Ended-StartDate)*FDayWidth);
  with Canvas do
    begin
      Pen.Color:=Ligthen(aSection.Color,0.2);

      Pen.Style:=psClear;

      Brush.Color:=Ligthen(aSection.Color,1);
      Polygon([
              Point(aLeft,FTop+1),
              Point(aRight-FPointWidth,FTop+1),
              Point(aRight,((Height-FTop) div 2)+FTop),
              Point(aLeft+FPointWidth,((Height-FTop) div 2)+FTop)
              ]);
      Brush.Color:=Ligthen(aSection.Color,0.8);
      Polygon([
              Point(aLeft,Height-1),
              Point(aRight-FPointWidth,Height-1),
              Point(aRight,((Height-FTop) div 2)+FTop),
              Point(aLeft+FPointWidth,((Height-FTop) div 2)+FTop)
              ]);

      Pen.Style:=psSolid;
      MoveTo(aLeft,FTop);
      LineTo(aLeft+FPointWidth,((Height-FTop) div 2)+FTop);
      LineTo(aLeft,Height);
      MoveTo(aLeft,FTop);
      LineTo(aRight-FPointWidth,FTop);
      LineTo(aRight,((Height-FTop) div 2)+FTop);
      LineTo(aRight-FPointWidth,Height);
      MoveTo(aLeft,Height-1);
      LineTo(aRight-FPointWidth,Height-1);
      te := TextExtent(aSection.Name);
      Brush.Style:=bsClear;
      TextOut((aLeft+FPointWidth)+((aRight-aLeft-FPointWidth-te.cx) div 2),
              ((Height-FTop-te.cy-1) div 2)+FTop,aSection.Name);
      Brush.Style:=bsSolid;
    end;
end;

function TProjectFlow.GetWeeks: Integer;
var
  S: TTimeStamp;
begin
  S := DateTimeToTimeStamp(FCurrentDate);
  Result := (S.Date - 1) div 7;
end;

function TProjectFlow.GetYears: Integer;
var
  Year, Month, Day: Word;
begin
  DecodeDate(FCurrentDate, Year, Month, Day);
  Result := Year;
end;

procedure TProjectFlow.DrawMinorScale;
var
  R, TextR: TRect;
  OldTransparent: Integer;
begin
  with Canvas do
  begin
    if IsNewPeriod(MinorScale) then
    begin
      OldTransparent := SetBKMode(Handle, TRANSPARENT);

      R.Left := {Trunc}Round
        (
          UnitsBetweenDates(StartDate, FCurrentDate, MinorScale)
            *
          PixelsPerMinorScale
        );
      R.Right := R.Left + PixelsPerMinorScale;

      R.Top := MajorScaleHeight + 1;
      R.Bottom := MajorScaleHeight + MinorScaleHeight - 1;

      TextR := Rect(R.Left + 2, R.Top + 2, R.Right - 2, R.Bottom - 2);

      DrawText
      (
        Handle,
        PChar(GetTimeScaleName(MinorScale, FCurrentDate)),
        -1,
        TextR,
        DT_LEFT or DT_VCENTER or DT_SINGLELINE
      );

      SetBKMode(Handle, OldTransparent);

      Canvas.Pen.Style := psSolid;
      Canvas.Pen.Color := clBlack;

      MoveTo(R.Left, MajorScaleHeight);
      LineTo(R.Left, MajorScaleHeight + MinorScaleHeight);
    end;
  end;
end;

procedure TProjectFlow.DrawMajorScale;
var
  OldTransparent: Integer;
  R: TRect;
  TextR: TRect;
begin
  with Canvas do
  begin
    OldTransparent := SetBKMode(Handle, TRANSPARENT);

    R.Left := {Trunc}Round
      (
        UnitsBetweenDates(StartDate, FCurrentDate, MinorScale)
          *
        PixelsPerMinorScale
      );
    R.Right := {Trunc}Round
      (
        UnitsBetweenDates(StartDate, IncTime(FCurrentDate, MajorScale, 1), MinorScale)
          *
        PixelsPerMinorScale
      );

    R.Top := 1;
    R.Bottom := MajorScaleHeight - 1+R.Top;

    TextR := Rect(R.Left + 2, R.Top + 2, R.Right - 2, R.Bottom - 2);

    DrawText
    (
      Handle,
      PChar(GetTimeScaleName(MajorScale, FCurrentDate)),
      -1,
      TextR,
      DT_LEFT or DT_VCENTER or DT_SINGLELINE or DT_LEFT
    );

    SetBKMode(Handle, OldTransparent);

    Canvas.Pen.Style := psSolid;
    Canvas.Pen.Color := clBlack;

    MoveTo(R.Left, R.Top);
    LineTo(R.Left, R.Bottom);
  end;
end;

procedure TProjectFlow.DrawHeaderLines;
begin
  with Canvas do
  begin
    Canvas.Pen.Style := psSolid;
    Canvas.Pen.Color := clBlack;

    MoveTo(0, MajorScaleHeight);
    LineTo(ClientWidth, MajorScaleHeight);

    MoveTo(0, MajorScaleHeight + MinorScaleHeight);
    LineTo(ClientWidth, MajorScaleHeight + MinorScaleHeight);
  end;
end;

function TProjectFlow.IsNewPeriod(AScale: TTimeScale; AUseSub: Boolean
  ): Boolean;
begin
  case AScale of
    tsMinute:
      Result := Seconds = 0;
    tsHour:
      Result := (Minutes = 0) and (not AUseSub or AUseSub and IsNewPeriod(tsMinute, True));
    tsDay:
      Result := (Hours = 0) and (not AUseSub or AUseSub and IsNewPeriod(tsHour, True));
    tsWeek,tsWeekNum,tsWeekNumPlain:
      Result := (DayOfWeek = 1) and (not AUseSub or AUseSub and IsNewPeriod(tsDay, True));
    tsMonth:
      Result := (Days = 1) and (not AUseSub or AUseSub and IsNewPeriod(tsDay, True));
    tsQuarter:
      Result := ((Monthes - 1) mod 3 = 0) and (not AUseSub or AUseSub and IsNewPeriod(tsMonth, True));
    tsHalfYear:
      Result := ((Monthes - 1) mod 6 = 0) and (not AUseSub or AUseSub and IsNewPeriod(tsMonth, True));
    tsYear:
      Result := (Monthes = 1) and (not AUseSub or AUseSub and IsNewPeriod(tsMonth, True));
    else
      Result := True;
  end;
end;

procedure TProjectFlow.SetEndDate(AValue: TDateTime);
begin
  if FEndDate=AValue then Exit;
  FEndDate:=AValue;
  FRecalcNeeded:=True;
end;

procedure TProjectFlow.SetMajorScale(AValue: TTimeScale);
begin
  if FMajorScale=AValue then Exit;
  FMajorScale:=AValue;
end;

procedure TProjectFlow.SetMinorScale(AValue: TTimeScale);
begin
  if FMinorScale=AValue then Exit;
  FMinorScale:=AValue;
end;

procedure TProjectFlow.SetStartDate(AValue: TDateTime);
begin
  if FStartDate=AValue then Exit;
  FStartDate:=AValue;
  FRecalcNeeded:=True;
end;

procedure TProjectFlow.BoundsChanged;
begin
  inherited BoundsChanged;
  FRecalcNeeded:=True;
end;

procedure TProjectFlow.CalcParams;
begin
  FDayWidth:=Width/(EndDate-StartDate);
  FPointWidth:=(Height-FTop) div 8;
  if FDayWidth > 20 then
    begin
      FMajorScale:=tsWeekNum;
      FMinorScale:=tsDay;
    end
  else
    begin
      FMajorScale:=tsMonth;
      FMinorScale:=tsWeekNum;
    end;

  FTop := MinorScaleHeight+MajorScaleHeight;
end;

function TProjectFlow.GetCount: Integer;
begin
  Result := FSections.Count;
end;

function TProjectFlow.GetDayOfWeek: Integer;
var
  S: TTimeStamp;
begin
  S := DateTimeToTimeStamp(FCurrentDate);
  Result := (S.Date - 1) mod 7 + 1;
end;

function TProjectFlow.GetDays: Integer;
var
  Year, Month, Day: Word;
begin
  DecodeDate(FCurrentDate, Year, Month, Day);
  Result := Day;
end;

function TProjectFlow.GetHalfYears: Integer;
var
  Year, Month, Day: Word;
begin
  DecodeDate(FCurrentDate, Year, Month, Day);
  Result := Month div 6 + 1;
end;

function TProjectFlow.GetHours: Integer;
var
  Hour, Min, Sec, MSec: Word;
begin
  DecodeTime(FCurrentDate, Hour, Min, Sec, MSec);
  Result := Hour;
end;

function TProjectFlow.GetMajorScale: Integer;
begin
  Canvas.Font := Font;
  Result := Trunc(Canvas.TextHeight('A') * 1.5);
end;

function TProjectFlow.GetMinorScale: Integer;
begin
  Canvas.Font := Font;
  Result := Trunc(Canvas.TextHeight('A') * 1.5);
end;

function TProjectFlow.GetMinutes: Integer;
var
  Hour, Min, Sec, MSec: Word;
begin
  DecodeTime(FCurrentDate, Hour, Min, Sec, MSec);
  Result := Min;
end;

function TProjectFlow.GetMonthes: Integer;
var
  Year, Month, Day: Word;
begin
  DecodeDate(FCurrentDate, Year, Month, Day);
  Result := Month;
end;

function TProjectFlow.getPixelsPerMinorScale: Integer;
begin
  Result := round(FDayWidth*IncTime(0,FMinorScale,1));
end;

function TProjectFlow.GetQuarters: Integer;
var
  Year, Month, Day: Word;
begin
  DecodeDate(FCurrentDate, Year, Month, Day);
  Result := Month div 3 + 1;
end;

function TProjectFlow.GetSeconds: Integer;
var
  Hour, Min, Sec, MSec: Word;
begin
  DecodeTime(FCurrentDate, Hour, Min, Sec, MSec);
  Result := Sec;
end;

procedure TProjectFlow.Paint;
var
  i: Integer;
  aLeft: Int64;
  aRight: Int64;
  Smallest: Integer;
  sName: String;
begin
  if FRecalcNeeded then
    begin
      CalcParams;
      FRecalcNeeded:=False;
    end;
  FCurrentDate := ClearToPeriodStart(MinorScale, StartDate);
  while FCurrentDate < EndDate do
  begin
    DrawMinorScale;
    FCurrentDate := IncTime(FCurrentDate, MinorScale, 1);
  end;
  FCurrentDate := ClearToPeriodStart(MajorScale, StartDate);
  while FCurrentDate < EndDate do
  begin
    DrawMajorScale;
    FCurrentDate := IncTime(FCurrentDate, MajorScale, 1);
  end;
  DrawHeaderLines;
  with Canvas do
    begin
      Smallest := Width;
      for i := 0 to FSections.Count-1 do
        begin
          aLeft := round((GetSection(i).Start-StartDate)*FDayWidth);
          aRight := round((GetSection(i).Ended-StartDate)*FDayWidth);
          if aRight-aLeft<Smallest then
            begin
              Smallest := aRight-aLeft;
              sName := GetSection(i).Name;
            end;
        end;
      {$ifdef WINDOWS}
      Canvas.Font.Height:=14;
      i := 0;
      while Canvas.TextExtent(sName).cx>round(Smallest*0.9) do
        begin
          Canvas.Font.Height:=Canvas.Font.Height-1;
          if i>4 then break;
        end;
      {$endif}
      for i := 0 to FSections.Count-1 do
        DrawSection(GetSection(i));
    end;
end;

constructor TProjectFlow.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSections := TList.Create;
  FTop := 20;
  FRecalcNeeded := True;
end;

destructor TProjectFlow.Destroy;
begin
  FSections.Free;
  inherited Destroy;
end;

procedure TProjectFlow.AddSection(aStart, aEnd: TDateTime; aName: string;
  aColor: TColor);
begin
  FSections.Add(TPFSection.Create(aStart,aEnd,aName,aColor));
end;

end.

