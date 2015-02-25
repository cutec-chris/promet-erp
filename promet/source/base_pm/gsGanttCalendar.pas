
{++

  Copyright (c) 1999 by Golden Software of Belarus

  Module

    gsGanttCalendar.pas

  Abstract

    A visual component. Calendar with marked periods of time.

  Author

    Romanovski Denis (04-Nov-1999)

  Contact address

    dionik@usa.net
    goldensoftware@usa.net

  Revisions history

    1.00   04-Nov-1999    dennis          Initial version.
    2.00   07-dec-1999    dennis          Beta1. Tree added and connected to gantt.

--}

unit gsGanttCalendar;

interface

uses
  LCLproc,LCLIntf,LCLType,LMessages, SysUtils,Classes, Graphics, Controls,
  Forms, Dialogs, ExtCtrls, StdCtrls, Grids, Spin, ComCtrls, Buttons,
  EditBtn,dateutils,Calendar;


const
  crGanttMiddle = 16345;
  crGanttRightMove = 16346;
  crGanttLeftMove = 16347;
  crGanttPercent = 16348;
  crGanttConnect = 16349;

const
  SCROLL_MAX = 100;
  SCROLL_MINSTEP = 1;
  SCROLL_MAXSTEP = 10;

type
  TTimeScale = (tsMinute, tsHour, tsDay, tsWeek,tsWeekNum,tsWeekNumPlain, tsMonth, tsQuarter, tsHalfYear, tsYear);


type
  TIntervalType = (itPeriod, itAction);
  TIntervalStyle = (isDefault,isNone);


const
  PrevScale: array[TTimeScale] of TTimeScale =
    (tsMinute, tsMinute, tsHour, tsDay, tsDay, tsDay,tsDay, tsMonth, tsMonth, tsMonth);

type
  TDragIntervalType = (ditMiddle, ditRightMove, ditLeftMove, ditPercent, ditConnect, ditNone);

type
  TGanttColumnType =
  (
    gctInfo, gctTask, gctDuration, gctStart,
    gctFinish, gctConnection, gctResources, gctNone
  );

type
  TPaitBackgroundEvent = procedure(Sender : TObject;Canvas : TCanvas;aRect : TRect;aStart,aEnd : TDateTime;aDayWidth : Double;aUnfinishedList : TList = nil) of object;
  TGanttCalendar = class;
  TGanttTree = class;
  TgsGantt = class;

  { TInterval }

  TInterval = class
  private
    FBP: TPaitBackgroundEvent;
    FBuffer: TDateTime;
    FChanged: Boolean;
    FColor: TColor;
    FDepDone: Boolean;
    FDontChange: Boolean;
    FFixed: Boolean;
    FGantt: TgsGantt;
    Fid: Variant;
    FInCriticalPath: Boolean;
    FIntervalStyle: TIntervalStyle;
    FMoved: Boolean;
    FMovedBack: Boolean;
    FMovedFwd: Boolean;
    FNetDuration: TDateTime;
    FNetTime: TDateTime;
    FOnChanged: TNotifyEvent;
    FOnExpand: TNotifyEvent;
    FPB: TPaitBackgroundEvent;
    FPointer: Pointer;
    FPointer2: Pointer;
    FProject: String;
    FRes: string;
    FResPerDay: Real;

    FStartDate: TDateTime;
    FFinishDate: TDateTime;
    FStarted: Boolean;
    FStyle: TFontStyles;
    FTask: String;
    FParent: TInterval;

    FDrawRect: TRect;
    FVisible: Boolean;
    FIntervalDone: TDateTime;
    FEarliestDate: TDateTime;

    FIntervals: TList;

    FConnections: TList;
    FDependencies: TList;
    FCanUpdate: Boolean;
    FUpdating : Integer;
    FUpdateCount : Integer;

    // property procedures and functions
    function GetDep(Index: Integer): TInterval;
    function GetDepCount: Integer;
    function GetEarliestDate: TDateTime;
    function GetIndex: Integer;
    function GetLatest: TDateTime;
    function GetStartDate: TDateTime;
    function GetFinishDate: TDateTime;
    procedure SetBuffer(AValue: TDateTime);
    procedure SetColor(AValue: TColor);
    procedure SetEarliestDate(AValue: TDateTime);
    procedure SetId(AValue: Variant);

    function GetDuration: TDateTime;
    procedure SetDuration(const Value: TDateTime);

    function GetStampDuration: TTimeStamp;

    procedure SetTask(const Value: String);

    function GetIntervalType: TIntervalType;
    function GetLevel: Integer;

    function GetIntervalCount: Integer;
    function GetInterval(AnIndex: Integer): TInterval;

    function GetConnectionCount: Integer;
    function GetConnection(AnIndex: Integer): TInterval;

    function GetIsCollection: Boolean;

    function GetIntervalDone: TDateTime;
    procedure SetIntervalDone(const Value: TDateTime);

    function GetIsDrawRectClear: Boolean;

    function GetDoneRect: TRect;

    function GetLeftMoveRect: TRect;
    function GetRightMoveRect: TRect;

    function GetVisible: Boolean;
    procedure SetVisible(const Value: Boolean);

    function GetOpened: Boolean;
    procedure SetOpened(const Value: Boolean);

    // other usefull procedures and functions
    function CountStartDate: TDateTime;
    function CountFinishDate: TDateTime;

  protected
    procedure SetStartDate(const Value: TDateTime);virtual;
    procedure SetFinishDate(const Value: TDateTime);virtual;
    procedure SetNetTime(AValue: TDateTime);virtual;
    function GetUsage: Extended;virtual;
    function GetPercentMoveRect: TRect;virtual;
    procedure PrepareDrawRect;virtual;
  public
    constructor Create(AGantt: TgsGantt);virtual;
    destructor Destroy; override;

    procedure AddConnection(AConnection: TInterval;DoUpdateIntervalStart : Boolean;DoChange : Boolean = True);
    procedure DeleteConnection(AnIndex: Integer);
    procedure RemoveConnection(AConnection: TInterval);

    procedure AddInterval(AnInterval: TInterval);
    procedure InsertInterval(AnIndex: Integer; AnInterval: TInterval);
    procedure DeleteInterval(AnIndex: Integer);
    procedure RemoveInterval(AnInterval: TInterval);
    procedure Sort;

    property IsDrawRectClear: Boolean read GetIsDrawRectClear;
    procedure ClearDrawRect;
    procedure MakeIntervalList(AList: TList);

    function ConnectionExists(AConnection: TInterval): Boolean;
    function IntervalExists(AnInterval: TInterval): Boolean;

    procedure UpdateIntervalStart(Delta: TDateTime);
    procedure PrepareToUpdate;
    property StartDate: TDateTime read GetStartDate write SetStartDate;
    property FinishDate: TDateTime read GetFinishDate write SetFinishDate;
    property Latest : TDateTime read GetLatest;
    property Earliest: TDateTime read GetEarliestDate write SetEarliestDate;
    property Duration: TDateTime read GetDuration write SetDuration;
    property NetDuration: TDateTime read FNetDuration write FNetDuration;
    property NetTime: TDateTime read FNetTime write SetNetTime;
    property WaitTime: TDateTime read FBuffer write SetBuffer;
    property StampDuration: TTimeStamp read GetStampDuration;
    property Task: String read FTask write SetTask;
    property Project: String read FProject write Fproject;
    property Started: Boolean read FStarted write FStarted;
    property Parent: TInterval read FParent write FParent;
    property PercentUsage : Extended read GetUsage;
    property DepDone : Boolean read FDepDone write FDepDone;

    property IntervalType: TIntervalType read GetIntervalType;
    property Style: TIntervalStyle read FIntervalStyle write FIntervalStyle;
    property Level: Integer read GetLevel;

    property DrawRect: TRect read FDrawRect write FDrawRect;
    property DoneRect: TRect read GetDoneRect;
    property PercentMoveRect: TRect read GetPercentMoveRect;
    property LeftMoveRect: TRect read GetLeftMoveRect;
    property RightMoveRect: TRect read GetRightMoveRect;

    property Left: Integer read FDrawRect.Left write FDrawRect.Left;
    property Right: Integer read FDrawRect.Right write FDrawRect.Right;
    property Top: Integer read FDrawRect.Top write FDrawRect.Top;
    property Bottom: Integer read FDrawRect.Bottom write FDrawRect.Bottom;

    property Visible: Boolean read GetVisible write SetVisible;
    property Opened: Boolean read GetOpened write SetOpened;
    property IntervalCount: Integer read GetIntervalCount;
    property Interval[Index: Integer]: TInterval read GetInterval;
    property ConnectionCount: Integer read GetConnectionCount;
    property Connection[Index: Integer]: TInterval read GetConnection;
    property DependencyCount: Integer read GetDepCount;
    property Dependencies[Index: Integer]: TInterval read GetDep;

    property IsCollection: Boolean read GetIsCollection;
    property IntervalDone: TDateTime read GetIntervalDone write SetIntervalDone;
    property InCriticalPath : Boolean read FInCriticalPath write FInCriticalPath;
    property Id : Variant read Fid write SetId;
    property Changed : Boolean read FChanged write FChanged;
    property MovedBack : Boolean read FMovedBack;
    property MovedFwd : Boolean read FMovedFwd;
    property Moved : Boolean read FMoved write FMoved;
    procedure ResetMovement;
    property DontChange : Boolean read FDontChange write FDontChange;
    procedure Change;
    procedure BeginUpdate;
    procedure EndUpdate(aDontchange : Boolean = False);
    property Pointer : Pointer read FPointer write FPointer;
    property Pointer2 : Pointer read FPointer2 write FPointer2;
    property Resource : string read FRes write FRes;
    property ResourceTimePerDay : Real read FResPerDay write FResPerDay;
    property OnChanged : TNotifyEvent read FOnChanged write FOnChanged;
    property OnDrawBackground : TPaitBackgroundEvent read FPB write FBP;
    property Color : TColor read FColor write SetColor;
    property Styles : TFontStyles read FStyle write FStyle;
    property Gantt : TgsGantt read FGantt;
    property OnExpand : TNotifyEvent read FOnExpand write FOnExpand;
    property Fixed : Boolean read FFixed write FFixed;
    property Index : Integer read GetIndex;
  end;
  TMouseOverInterval = procedure(Sender : TObject;aInterval : TInterval;X,Y : Integer) of object;

  { TGanttCalendar }

  TGanttCalendar = class(TCustomControl)
  private
    FOverInterval: TMouseOverInterval;
    FStartDateChanged: TNotifyEvent;
    FVertScrollBar: TScrollBar;
    FHorzScrollBar: TScrollBar;

    FMajorScale: TTimeScale;
    FMinorScale: TTimeScale;
    FPixelsPerMinorScale: Integer;
    FPixelsPerLine: Integer;

    FStartDate: TDateTime;
    FVisibleStart: TDateTime;

    FCurrentDate: TDateTime;

    FMajorColor: TColor;
    FMinorColor: TColor;

    FIntervals: TList;
    FGantt: TgsGantt;

    FBeforeStartDateCount: Integer;

    FDragRect: TRect;
    FDragType: TDragIntervalType;
    FDragInterval: TInterval;
    FConnectInterval: TInterval;
    FDragStarted: Boolean;
    FMoveStarted: Boolean;
    FFromDragPoint: Integer;
    FConnectFromPoint: TPoint;
    FConnectToPoint: TPoint;

    // Property Procedures and functions
    function GetEndDate: TDateTime;
    procedure SetMajorScale(const Value: TTimeScale);
    procedure SetMinorScale(const Value: TTimeScale);

    procedure SetPixelsPerMinorScale(const Value: Integer);

    procedure SetPixelsPerLine(const Value: Integer);

    procedure SetStartDate(const Value: TDateTime);

    function GetVisibleFinish: TDateTime;
    function GetMinorVisibleUnitsCount: Integer;

    function GetMajorScale: Integer;
    function GetMinorScale: Integer;

    function GetSeconds: Integer;
    function GetMinutes: Integer;
    function GetHours: Integer;
    function GetDays: Integer;
    function GetDayOfWeek: Integer;
    function GetWeeks: Integer;
    function GetMonthes: Integer;
    function GetQuarters: Integer;
    function GetHalfYears: Integer;
    function GetYears: Integer;
    function GetTimeUnitByScale(ATimeScale: TTimeScale): Integer;

    procedure SetMajorColor(const Value: TColor);
    procedure SetMinorColor(const Value: TColor);

    function GetIntervalCount: Integer;
    function GetInterval(AnIndex: Integer): TInterval;

    // Other usefull methods
    procedure DrawHeaderLines(aCanvas : TCanvas);
    procedure ConnectIntervals(aCanvas : TCanvas;FromInterval, ToInterval: TInterval);

    procedure DrawMinorScale(aCanvas : TCanvas);
    procedure DrawMajorScale(aCanvas : TCanvas);

    function IsNewPeriod(AScale: TTimeScale; AUseSub: Boolean = False): Boolean;

    procedure UpdateScrollbars;
    procedure DoOnHorzScroll(Sender: TObject; ScrollCode: TScrollCode; var ScrollPos: Integer);

    procedure WMSize(var Message: TLMSize);
      message LM_SIZE;
    procedure CMMouseEnter(var Message: TLMessage);
      message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TLMessage);
      message CM_MOUSELEAVE;
    procedure WMMouseMove(var Message: TLMMouseMove);
      message LM_MOUSEMOVE;
    procedure WMLButtonDown(var Message: TLMLButtonDown);
      message LM_LBUTTONDOWN;
    procedure WMLButtonUp(var Message: TLMRButtonDown);
      message LM_LBUTTONUP;

  protected
    procedure Paint; override;

  public
    constructor Create(AnOwner: TgsGantt); reintroduce;
    destructor Destroy; override;

    procedure PaintToCanvas(aCanvas: TCanvas; DoClip: Boolean=True);
    function GetStartDrawIntervals: Integer;
    function GetIntervalHeight: Integer;
    function GetIntervalWidth: Double;

    procedure AddInterval(AnInterval: TInterval);
    procedure InsertInterval(AnIndex: Integer; AnInterval: TInterval);
    procedure DeleteInterval(AnIndex: Integer);
    procedure RemoveInterval(AnInterval: TInterval);
    function IsDragging : Boolean;

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
    property TimeUnitByScale[ATimeScale: TTimeScale]: Integer read GetTimeUnitByScale;

    property StartDrawIntervals: Integer read GetStartDrawIntervals;
    property IntervalHeight: Integer read GetIntervalHeight;

    property MinorVisibleUnits: Integer read GetMinorVisibleUnitsCount;
    property MajorScaleHeight: Integer read GetMajorScale;
    property MinorScaleHeight: Integer read GetMinorScale;
    property VisibleStart: TDateTime read FVisibleStart;
    property VisibleFinish: TDateTime read GetVisibleFinish;
    property IntervalCount: Integer read GetIntervalCount;
    property Interval[Index: Integer]: TInterval read GetInterval;

  published
    property Font;
    property Color;
    property Align;
    property MajorColor: TColor read FMajorColor write SetMajorColor;
    property MinorColor: TColor read FMinorColor write SetMinorColor;

    property MajorScale: TTimeScale read FMajorScale write SetMajorScale default tsWeek;
    property MinorScale: TTimeScale read FMinorScale write SetMinorScale default tsDay;
    property PixelsPerMinorScale: Integer read FPixelsPerMinorScale write SetPixelsPerMinorScale default 30;
    property PixelsPerLine: Integer read FPixelsPerLine write SetPixelsPerLine default 24;

    property StartDate: TDateTime read FStartDate write SetStartDate;
    property EndDate : TDateTime read GetEndDate;

    property OnMouseMove;
    property OnMouseDown;
    property OnMouseUp;
    property OnDblClick;
    property OnClick;
    property OnMoveOverInterval : TMouseOverInterval read FOverInterval write FOverInterval;
    property OnStartDateChanged : TNotifyEvent read FStartDateChanged write FStartDateChanged;
  end;

  { TGanttTree }

  TGanttTree = class(TStringGrid)
  private
    FAfterUpdateCommonSettings: TNotifyEvent;
    FIntervals: TList;
    FGantt: TgsGantt;
    FIndent: Integer;
    FBranchFont: TFont;
    FTaskEditable: Boolean;

    FTextEdit: TEdit;
    FDurationEdit: TSpinEdit;
    FDateEdit: TDateEdit;
    FComboEdit: TComboBox;
    FUpDown: TUpDown;
    FMonthCalendar: TCalendar;
    FDownButton: TSpeedButton;
    FEditInterval: TInterval;

    // property procedures and functions
    function GetColumnType(AnIndex: Integer): TGanttColumnType;
    procedure SetAfterUpdateCommonSettings(AValue: TNotifyEvent);
    procedure SetIndent(const Value: Integer);

    procedure DrawMinus(X, Y: Integer);
    procedure DrawPlus(X, Y: Integer);

    function GetBrachFont: TFont;
    procedure SetBranchFont(const Value: TFont);

    // other usefull procedures and functions
    procedure OnEditKeyPress(Sender: TObject; var Key: Char);
    procedure OnEditExit(Sender: TObject);
    procedure OnDownButtonClick(Sender: TObject);
    procedure OnMonthCalendarClick(Sender: TObject);
    procedure OnUpDownButtonClick(Sender: TObject; Button: TUDBtnType);

    procedure UpdateCurrentControl(ACol, ARow: Integer);
    procedure ShowTaskEditor;

    procedure WMChar(var Message: TLMChar);
      message LM_CHAR;

  protected
    procedure CreateWnd; override;
    procedure DrawCell(ACol, ARow: Longint; ARect: TRect;
      AState: TGridDrawState); override;
    procedure MouseDown(Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); override;
    function SelectCell(ACol, ARow: Longint): Boolean; override;
    procedure TopLeftChanged; override;
    procedure ColWidthsChanged; override;
    procedure DoExit; override;

    procedure UpdateCommonSettings;
    procedure UpdateTreeList;

  public
    constructor Create(AnOwner: TgsGantt); reintroduce;
    destructor Destroy; override;

  published
    property Indent: Integer read FIndent write SetIndent;
    property Font;
    property BranchFont: TFont read GetBrachFont write SetBranchFont;
    property TaskEditable : Boolean read FTaskEditable write FTaskEditable;
    property AfterUpdateCommonSettings : TNotifyEvent read FAfterUpdateCommonSettings write SetAfterUpdateCommonSettings;
  end;
  TgsGantt = class(TCustomControl)
  private
    FIntervals: TList;

    FCalendar: TGanttCalendar;
    FStartDateChanged: TNotifyEvent;
    FTree: TGanttTree;
    FSplitter: TSplitter;
    FUpdateCount : Integer;

    function GetIntervalCount: Integer;
    function GetInterval(AnIndex: Integer): TInterval;

    function GetCalendarFont: TFont;
    procedure SetCalendarFont(const Value: TFont);
    function GetCalendarColor: TColor;
    procedure SetCalendarColor(const Value: TColor);
    function GetMajorColor: TColor;
    procedure SetMajorColor(const Value: TColor);
    function GetMinorColor: TColor;
    procedure SetMinorColor(const Value: TColor);
    function GetMajorScale: TTimeScale;
    procedure SetMajorScale(const Value: TTimeScale);
    function GetMinorScale: TTimeScale;
    procedure SetMinorScale(const Value: TTimeScale);
    function GetPixelsPerMinorScale: Integer;
    procedure SetPixelsPerMinorScale(const Value: Integer);
    function GetPixelsPerLine: Integer;
    procedure SetPixelsPerLine(const Value: Integer);
    function GetStartDate: TDateTime;
    procedure SetStartDate(const Value: TDateTime);

    function GetTreeIndent: Integer;
    procedure SetTreeIndent(const Value: Integer);
    function GetTreeFont: TFont;
    procedure SetTreeFont(const Value: TFont);
    function GetTreeBranchFont: TFont;
    procedure SetTreeBranchFont(const Value: TFont);
  protected

  public
    constructor Create(AnOwner: TComponent); override;
    destructor Destroy; override;

    procedure MakeIntervalList(AList: TList);
    procedure MakeIntervalListWithDeps(AList: TList);
    procedure AddInterval(AnInterval: TInterval);
    procedure InsertInterval(AnIndex: Integer; AnInterval: TInterval);
    procedure DeleteInterval(AnIndex: Integer);
    procedure RemoveInterval(AnInterval: TInterval);

    procedure UpdateInterval;

    property Tree: TGanttTree read FTree;
    property Calendar: TGanttCalendar read FCalendar;

    property IntervalCount: Integer read GetIntervalCount;
    property Interval[Index: Integer]: TInterval read GetInterval;
    procedure BeginUpdate;
    function IsUpdating : Boolean;
    procedure EndUpdate;
  published
    property Align;
    property CalendarFont: TFont read GetCalendarFont write SetCalendarFont;
    property CalendarColor: TColor read GetCalendarColor write SetCalendarColor;
    property MajorColor: TColor read GetMajorColor write SetMajorColor;
    property MinorColor: TColor read GetMinorColor write SetMinorColor;

    property MajorScale: TTimeScale read GetMajorScale write SetMajorScale default tsWeek;
    property MinorScale: TTimeScale read GetMinorScale write SetMinorScale default tsDay;
    property PixelsPerMinorScale: Integer read GetPixelsPerMinorScale write SetPixelsPerMinorScale default 30;
    property PixelsPerLine: Integer read GetPixelsPerLine write SetPixelsPerLine default 24;
    property StartDate: TDateTime read GetStartDate write SetStartDate;

    property TreeIndent: Integer read GetTreeIndent write SetTreeIndent;
    property TreeFont: TFont read GetTreeFont write SetTreeFont;
    property TreeBranchFont: TFont read GetTreeBranchFont write SetTreeBranchFont;

    property OnStartDateChangd : TNotifyEvent read FStartDateChanged write FStartDateChanged;
  end;
//procedure Register;
function UnitsBetweenDates(Start, Finish: TdateTime; TimeScale: TTimeScale): Double;
function GetTimeScaleName(TimeScale: TTimeScale; D: TDateTime): String;
function IncTime(D: TDateTime; TimeScale: TTimeScale; IncAmount: Integer): TDateTime;
function ClearToPeriodStart(MinorScale: TTimeScale; D: TDateTime): TDateTime;
implementation

{$R gsGantt.res}


uses Math;

resourcestring
  strDay                     = ' Tage';
  strNum                     = 'Pos';
  strInfo                    = 'Info';
  strTask                    = 'Aufgabe';
  strTime                    = 'Dauer';
  strStart                   = 'Start';
  strFinish                  = 'Ende';
  strTies                    = 'Ties';
  strResourses               = 'Resourcen';
  strKW                      = 'KW';
var
  DrawBitmap: TBitmap = nil;

{
  --------------------------------------------------------
  ------                                            ------
  ------      Usefull procedures and functions      ------
  ------                                            ------
  --------------------------------------------------------
}
function GetTimeScaleUnits(TimeScale: TTimeScale; Year, Month: Integer): Integer;
begin
  case TimeScale of
    tsMinute:
    begin
      Result := 60;
    end;
    tsHour:
    begin
      Result := 60;
    end;
    tsDay:
    begin
      Result := 24;
    end;
    tsWeek,tsWeekNum,tsWeekNumPlain:
    begin
      Result := 7;
    end;
    tsMonth:
    begin
      Result := MonthDays[IsLeapYear(Year)][Month];
    end;
    tsQuarter:
    begin
      Result := 3;
    end;
    tsHalfYear:
    begin
      Result := 2;
    end;
    tsYear:
    begin
      Result := 2;
    end;
    else
      Result := 60;
  end;
end;

function IncTime(D: TDateTime; TimeScale: TTimeScale; IncAmount: Integer): TDateTime;
var
  S: TTimeStamp;
begin
  S := DateTimeToTimeStamp(D);

  case TimeScale of
    tsMinute:
    begin
      if IncAmount > 24 * 60 then
      begin
        Inc(S.Date, IncAmount div 24 * 60);
        IncAmount := IncAmount - IncAmount div (24 * 60) * (24 * 60);
      end;

      Inc(S.Time, IncAmount * 60 * 1000);
      while S.Time < 0 do
      begin
        Dec(S.Date);
        S.Time := MSecsPerDay + S.Time;
      end;
    end;
    tsHour:
    begin
      if IncAmount > 24 then
      begin
        Inc(S.Date, IncAmount div 24);
        IncAmount := IncAmount - IncAmount div 24 * 24;
      end;

      Inc(S.Time, IncAmount * 60 * 60 * 1000);
      while S.Time < 0 do
      begin
        Dec(S.Date);
        S.Time := MSecsPerDay + S.Time;
      end;
    end;
    tsDay:
    begin
      Inc(S.Date, IncAmount);
    end;
    tsWeek,tsWeekNum,tsWeekNumPlain:
    begin
      Inc(S.Date, IncAmount * 7);
    end;
    tsMonth:
    begin
      S := DateTimeToTimeStamp(IncMonth(D, IncAmount));
    end;
    tsQuarter:
    begin
      S := DateTimeToTimeStamp(IncMonth(D, IncAmount * 3));
    end;
    tsHalfYear:
    begin
      S := DateTimeToTimeStamp(IncMonth(D, IncAmount * 6));
    end;
    tsYear:
    begin
      S := DateTimeToTimeStamp(IncMonth(D, IncAmount * 12));
    end;
    else begin
      if IncAmount > 24 * 60 * 60 then
      begin
        Inc(S.Date, IncAmount div 24 * 60 * 60);
        IncAmount := IncAmount - IncAmount div 24 * 60 * 60;
      end;

      Inc(S.Time, IncAmount * 1000);
      while S.Time < 0 do
      begin
        Dec(S.Date);
        S.Time := MSecsPerDay + S.Time;
      end;
    end;
  end;

  Result := TimeStampToDateTime(S);
end;

function IncTimeEx(D: TDateTime; TimeScale: TTimeScale; IncAmount: Double): TDateTime;
var
  S: TTimeStamp;
  Year, Month, Day: Word;
begin
  S := DateTimeToTimeStamp(D);

  case TimeScale of
    tsMinute:
    begin
      if IncAmount > 24 * 60 then
      begin
        Inc(S.Date, Trunc(IncAmount / 24 * 60));
        IncAmount := IncAmount - Trunc(IncAmount / 24 * 60) * (24 * 60);
      end;

      Inc(S.Time, Trunc(IncAmount * 60 * 1000));

      while S.Time < 0 do
      begin
        Dec(S.Date);
        S.Time := MSecsPerDay + S.Time;
      end;
    end;
    tsHour:
    begin
      if IncAmount > 24 then
      begin
        Inc(S.Date, Trunc(IncAmount / 24));
        IncAmount := IncAmount - Trunc(IncAmount / 24) * 24;
      end;

      Inc(S.Time, Trunc(IncAmount * 60 * 60 * 1000));

      while S.Time < 0 do
      begin
        Dec(S.Date);
        S.Time := MSecsPerDay + S.Time;
      end;
    end;
    tsDay:
    begin
      Inc(S.Date, Trunc(IncAmount));

      S :=
        DateTimeToTimeStamp
        (
          IncTimeEx
          (
            TimeStampToDateTime(S), tsHour, Frac(IncAmount) * 24
          )
        );
    end;
    tsWeek,tsWeekNum,tsWeekNumPlain:
    begin
      Inc(S.Date, Trunc(IncAmount) * 7);

      S :=
        DateTimeToTimeStamp
        (
          IncTimeEx
          (
            TimeStampToDateTime(S), tsDay, Frac(IncAmount) * 7
          )
        );
    end;
    tsMonth:
    begin
      S := DateTimeToTimeStamp(IncMonth(D, Trunc(IncAmount)));
      DecodeDate(TimeStampToDateTime(S), Year, Month, Day);

      S :=
        DateTimeToTimeStamp
        (
          IncTimeEx
          (
            TimeStampToDateTime(S), tsDay,
            Frac(IncAmount) * MonthDays[IsLeapYear(Year)][Month]
          )
        );
    end;
    tsQuarter:
    begin
      S := DateTimeToTimeStamp(IncMonth(D, Trunc(IncAmount) * 3));

      S :=
        DateTimeToTimeStamp
        (
          IncTimeEx
          (
            TimeStampToDateTime(S), tsMonth, Frac(IncAmount) * 3
          )
        );
    end;
    tsHalfYear:
    begin
      S := DateTimeToTimeStamp(IncMonth(D, Trunc(IncAmount) * 6));

      S :=
        DateTimeToTimeStamp
        (
          IncTimeEx
          (
            TimeStampToDateTime(S), tsMonth, Frac(IncAmount) * 6
          )
        );
    end;
    tsYear:
    begin
      S := DateTimeToTimeStamp(IncMonth(D, Trunc(IncAmount) * 12));

      S :=
        DateTimeToTimeStamp
        (
          IncTimeEx
          (
            TimeStampToDateTime(S), tsMonth, Frac(IncAmount) * 12
          )
        );
    end;
    else begin
      if IncAmount > 24 * 60 * 60 then
      begin
        Inc(S.Date, Trunc(IncAmount / 24 * 60 * 60));
        IncAmount := IncAmount - IncAmount / 24 * 60 * 60;
      end;

      Inc(S.Time, Trunc(IncAmount * 1000));
      while S.Time < 0 do
      begin
        Dec(S.Date);
        S.Time := MSecsPerDay + S.Time;
      end;
    end;
  end;

  Result := TimeStampToDateTime(S);
end;


function ClearToPeriodStart(MinorScale: TTimeScale; D: TDateTime): TDateTime;
var
  S: TTimeStamp;
  Year, Month, Day: Word;
begin
  S := DateTimeToTimeStamp(D);
  DecodeDate(D, Year, Month, Day);

  case MinorScale of
    tsMinute:
    begin
      S.Time := (S.Time div (60 * 1000)) * (60 * 1000);
    end;
    tsHour:
    begin
      S.Time := (S.Time div (60 * 60 * 1000)) * (60 * 60 * 1000);
    end;
    tsDay:
    begin
      S.Time := 0;
    end;
    tsWeek,tsWeekNum,tsWeekNumPlain:
    begin
      S.Date := (S.Date div 7) * 7 + 1;
      S.Time := 0;
    end;
    tsMonth:
    begin
      Day := 1;
      D := EncodeDate(Year, Month, Day);
      S := DateTimeToTimeStamp(D);
      S.Time := 0;
    end;
    tsQuarter:
    begin
      Day := 1;
      Month := (Month div 3) * 3 + 1;

      if Month > 12 then
      begin
        Month := 1;
        Inc(Year);
      end;

      D := EncodeDate(Year, Month, Day);
      S := DateTimeToTimeStamp(D);
      S.Time := 0;
    end;
    tsHalfYear:
    begin
      Day := 1;
      Month := (Month div 6) * 6 + 1;

      if Month > 12 then
      begin
        Month := 1;
        Inc(Year);
      end;
      
      D := EncodeDate(Year, Month, Day);
      S := DateTimeToTimeStamp(D);
      S.Time := 0;
    end;
    tsYear:
    begin
      Day := 1;
      Month := 1;
      D := EncodeDate(Year, Month, Day);
      S := DateTimeToTimeStamp(D);
      S.Time := 0;
    end;
    else begin
      S.Time := (S.Time div 1000) * 1000;
    end;
  end;

  Result := TimeStampToDateTime(S);
end;

function GetTimeScaleName(TimeScale: TTimeScale; D: TDateTime): String;
var
  Hour, Min, Sec, MSec: Word;
  Year, Month, Day: Word;
  S: TTimeStamp;
begin
  DecodeDate(D, Year, Month, Day);
  DecodeTime(D, Hour, Min, Sec, MSec);
  S := DateTimeToTimeStamp(D);

  case TimeScale of
    tsMinute:
    begin
      Result := IntToStr(Min);
    end;
    tsHour:
    begin
      Result := IntToStr(Hour);
    end;
    tsDay:
    begin
      Result := ShortDayNames[DayOfWeek(D)];
    end;
    tsWeek:
    begin
      Result := IntToStr(Day) + '.' + IntToStr(Month);
    end;
    tsWeekNum:
    begin
      Result := strKW+IntToStr(WeekOfTheYear(D));
    end;
    tsWeekNumPlain:
    begin
      Result := IntToStr(WeekOfTheYear(D));
    end;
    tsMonth:
    begin
      Result := ShortMonthNames[Month];
    end;
    tsQuarter:
    begin
      Result := IntToStr((Month) div 3 + 1);
    end;
    tsHalfYear:
    begin
      Result := IntToStr((Month) div 6 + 1);
    end;
    tsYear:
    begin
      Result := IntToStr(Year);
    end;
  end;
end;

function UnitsBetweenDates(Start, Finish: TdateTime; TimeScale: TTimeScale): Double;
var
  StartStamp, FinishStamp: TTimeStamp;
  StartDay, StartMonth, StartYear: Word;
  FinishDay, FinishMonth, FinishYear: Word;
begin
  StartStamp := DateTimeToTimeStamp(Start);
  FinishStamp := DateTimeToTimeStamp(Finish);
  
  DecodeDate(Start, StartYear, StartMonth, StartDay);
  DecodeDate(Finish, FinishYear, FinishMonth, FinishDay);

  case TimeScale of
    tsMinute:
    begin
      Result :=
        (FinishStamp.Time / 1000 / 60 + FinishStamp.Date * 24 * 60) -
        (StartStamp.Time / 1000 / 60 + StartStamp.Date * 24 * 60);
    end;
    tsHour:
    begin
      Result :=
        (FinishStamp.Time / 1000 / 60 / 60 + FinishStamp.Date * 24) -
        (StartStamp.Time / 1000 / 60 / 60 + StartStamp.Date * 24);
    end;
    tsDay:
    begin
      Result :=
        (FinishStamp.Time / 1000 / 60 / 60 / 24 + FinishStamp.Date) -
        (StartStamp.Time / 1000 / 60 / 60 / 24 + StartStamp.Date);
    end;
    tsWeek,tsWeekNum,tsWeekNumPlain:
    begin
      Result :=
        (FinishStamp.Time / 1000 / 60 / 60 / 24 / 7 + FinishStamp.Date / 7) -
        (StartStamp.Time / 1000 / 60 / 60 / 24 / 7 + StartStamp.Date / 7);
    end;
    tsMonth:
    begin
      Result :=
        (
          FinishMonth
            +
          (FinishDay + FinishStamp.Time / 1000 / 60 / 60 / 24)
            /
          MonthDays[IsLeapYear(FinishYear)][FinishMonth]
        )
          -
        (
          StartMonth
            +
          (StartDay + StartStamp.Time / 1000 / 60 / 60 / 24)
            /
          MonthDays[IsLeapYear(FinishYear)][FinishMonth]
        )
          +
        (FinishYear - StartYear)
          *
        12;
    end;
    tsQuarter:
    begin
      Result :=
        (
          (
            FinishMonth
              +
            (FinishDay + FinishStamp.Time / 1000 / 60 / 60 / 24)
              /
            MonthDays[IsLeapYear(FinishYear)][FinishMonth]
          )
            -
          (
            StartMonth
              +
            (StartDay + StartStamp.Time / 1000 / 60 / 60 / 24)
              /
            MonthDays[IsLeapYear(FinishYear)][FinishMonth]
          )
        )
          /
        3
          +
        (FinishYear - StartYear)
          *
        3;
    end;
    tsHalfYear:
    begin
      Result :=
        (
          (
            FinishMonth
              +
            (FinishDay + FinishStamp.Time / 1000 / 60 / 60 / 24)
              /
            MonthDays[IsLeapYear(FinishYear)][FinishMonth]
          )
            -
          (
            StartMonth
              +
            (StartDay + StartStamp.Time / 1000 / 60 / 60 / 24)
              /
            MonthDays[IsLeapYear(FinishYear)][FinishMonth]
          )
        )
          /
        6
          +
        (FinishYear - StartYear)
          *
        6;
    end;
    tsYear:
    begin
      Result :=
        (
          (
            FinishMonth
              +
            (FinishDay + FinishStamp.Time / 1000 / 60 / 60 / 24)
              /
            MonthDays[IsLeapYear(FinishYear)][FinishMonth]
          )
            -
          (
            StartMonth
              +
            (StartDay + StartStamp.Time / 1000 / 60 / 60 / 24)
              /
            MonthDays[IsLeapYear(FinishYear)][FinishMonth]
          )
          /
          12
        )
          +
        (FinishYear - StartYear);
    end;
    else begin
      Result :=
        FinishStamp.Time / 1000 + FinishStamp.Date * 24 * 60 * 60 -
        StartStamp.Time / 1000 + StartStamp.Date * 24 * 60 * 60;
    end;
  end;
end;


function IsInRect(X, Y: Integer; R: TRect): Boolean;
begin
  Result := (X >= R.Left) and (X <= R.Right) and (Y >= R.Top) and (Y <= R.Bottom);
end;

procedure WriteText(ACanvas: TCanvas; ARect: TRect; DX, DY: Integer;
  const Text: string; Alignment: TAlignment; ARightToLeft: Boolean);
const
  AlignFlags : array [TAlignment] of Integer =
    ( DT_LEFT or DT_EXPANDTABS or DT_NOPREFIX,
      DT_RIGHT or DT_EXPANDTABS or DT_NOPREFIX,
      DT_CENTER or DT_EXPANDTABS or DT_NOPREFIX );
  RTL: array [Boolean] of Integer = (0, DT_RTLREADING);
var
  B, R: TRect;
  Hold, Left: Integer;
  I: TColorRef;
begin
  I := ColorToRGB(ACanvas.Brush.Color);
  if {GetNearestColor(ACanvas.Handle, I) = I} False then
  begin                       { Use ExtTextOut for solid colors }
    { In BiDi, because we changed the window origin, the text that does not
      change alignment, actually gets its alignment changed. }
    case Alignment of
      taLeftJustify:
        Left := ARect.Left + DX;
      taRightJustify:
        Left := ARect.Right - ACanvas.TextWidth(Text) - 3;
    else { taCenter }
      Left := ARect.Left + (ARect.Right - ARect.Left) shr 1
        - (ACanvas.TextWidth(Text) shr 1);
    end;
    ACanvas.TextRect(ARect, Left, ARect.Top + DY, Text);
  end
  else begin                  { Use FillRect and Drawtext for dithered colors }
    DrawBitmap.Canvas.Lock;
    try
      with DrawBitmap, ARect do { Use offscreen bitmap to eliminate flicker and }
      begin                     { brush origin tics in painting / scrolling.    }
        Width := Max(Width, Right - Left);
        Height := Max(Height, Bottom - Top);
        R := Rect(DX, DY, Right - Left - 1, Bottom - Top - 1);
        B := Rect(0, 0, Right - Left, Bottom - Top);
      end;
      with DrawBitmap.Canvas do
      begin
        Font := ACanvas.Font;
        Font.Color := ACanvas.Font.Color;
        Brush := ACanvas.Brush;
        Brush.Style := bsSolid;
        FillRect(B);
        SetBkMode(Handle, TRANSPARENT);
        DrawText(Handle, PChar(Text), Length(Text), R,
          AlignFlags[Alignment] or RTL[ARightToLeft]);
      end;
      ACanvas.CopyRect(ARect, DrawBitmap.Canvas, B);
    finally
      DrawBitmap.Canvas.Unlock;
    end;
  end;
end;

{
  ---------------------------------------
  ------                           ------
  ------      TInterval Class      ------
  ------                           ------
  ---------------------------------------
}


{
  ***********************
  ***   Public Part   ***
  ***********************
}

constructor TInterval.Create(AGantt: TgsGantt);
begin
  FGantt := AGantt;
  FStartDate := 0;
  FDontChange:=False;
  FBuffer:=0;
  FFinishDate := 0;
  FDrawRect := Rect(-1, -1, -1, -1);
  FVisible := False;
  FParent := nil;
  FPointer:=nil;
  FColor:=clBlue;
  FUpdating:=0;
  FFixed:=False;

  FIntervals := TList.Create;
  FConnections := Tlist.Create;
  FDependencies := Tlist.Create;
  FCanUpdate := True;
  FInCriticalPath:=False;
  ResetMovement;
end;

destructor TInterval.Destroy;
var
  i: Integer;
begin
  if Assigned(FParent) then FParent.RemoveInterval(Self);
  FParent := nil;
  FConnections.Clear;
  FConnections.Free;
  try
    for i := 0 to FIntervals.Count-1 do
      begin
        TInterval(FIntervals.Items[i]).FParent:=nil;
        TInterval(FIntervals.Items[i]).Free;
      end;
  except
  end;
  FIntervals.Free;
  FDependencies.Clear;
  FDependencies.Free;

  inherited Destroy;
end;

procedure TInterval.AddConnection(AConnection: TInterval;
  DoUpdateIntervalStart: Boolean; DoChange: Boolean);
begin
  if FConnections.IndexOf(AConnection) = -1 then
  begin
    if AConnection.ConnectionExists(Self) then Exit;
    if ConnectionExists(AConnection) then Exit;

    if (AConnection.StartDate < FinishDate) and DoUpdateIntervalStart then
      AConnection.UpdateIntervalStart((FinishDate + WaitTime) - AConnection.StartDate);
    FConnections.Add(AConnection);
    AConnection.FDependencies.Add(Self);
    FGantt.UpdateInterval;
    if DoChange then
      Change;
  end;
end;

procedure TInterval.DeleteConnection(AnIndex: Integer);
begin
  if TInterval(FConnections[AnIndex]).FDependencies.IndexOf(Self)>-1 then
    TInterval(FConnections[AnIndex]).FDependencies.Delete(TInterval(FConnections[AnIndex]).FDependencies.IndexOf(Self));
  FConnections.Delete(AnIndex);
  FGantt.UpdateInterval;
end;

procedure TInterval.RemoveConnection(AConnection: TInterval);
begin
  if AConnection.FDependencies.IndexOf(Self)>-1 then
    AConnection.FDependencies.Delete(AConnection.FDependencies.IndexOf(Self));
  FConnections.Remove(AConnection);
  FGantt.UpdateInterval;
end;

procedure TInterval.AddInterval(AnInterval: TInterval);
begin
  FIntervals.Add(AnInterval);
  AnInterval.FParent := Self;
  if Assigned(FGantt) then
    FGantt.UpdateInterval;
end;

procedure TInterval.InsertInterval(AnIndex: Integer; AnInterval: TInterval);
begin
  FIntervals.Insert(AnIndex, AnInterval);
  AnInterval.FParent := Self;
  FGantt.UpdateInterval;
end;

procedure TInterval.DeleteInterval(AnIndex: Integer);
begin
  FIntervals.Delete(AnIndex);
  FGantt.UpdateInterval;
end;

procedure TInterval.RemoveInterval(AnInterval: TInterval);
begin
  FIntervals.Remove(AnInterval);
  if Assigned(FGantt) then FGantt.UpdateInterval;
end;

function CompareDate(Item1, Item2: Pointer): Integer;
begin
  Result := DateUtils.CompareDate(TInterval(Item1).StartDate,TInterval(Item2).StartDate);
end;
procedure TInterval.Sort;
begin
  FIntervals.Sort(@CompareDate);
end;

procedure TInterval.ClearDrawRect;
begin
  FDrawRect := Rect(-1, -1, -1, -1);
end;

procedure TInterval.MakeIntervalList(AList: TList);
var
  I: Integer;
begin
  for I := 0 to IntervalCount - 1 do
  begin
    if Interval[I].Visible then
    begin
      AList.Add(Interval[I]);
      Interval[I].MakeIntervalList(AList);
    end;
  end;
end;

function TInterval.ConnectionExists(AConnection: TInterval): Boolean;
var
  I: Integer;
begin
  Result := False;

  if IntervalExists(AConnection) then
  begin
    Result := True;
    Exit;
  end;

  for I := 0 to ConnectionCount - 1 do
    if AConnection = Connection[I] then
    begin
      Result := True;
      Exit;
    end else if Connection[I].ConnectionExists(AConnection) then
    begin
      Result := True;
      Exit;
    end;

  if IsCollection then
    for I := 0 to IntervalCount - 1 do
      if Interval[I].ConnectionExists(AConnection) then
      begin
        Result := True;
        Break; 
      end;
end;

function TInterval.IntervalExists(AnInterval: TInterval): Boolean;
var
  I: Integer;
begin
  Result := False;

  for I := 0 to IntervalCount - 1 do
    if AnInterval = Interval[I] then
    begin
      Result := True;
      Exit;
    end else if Interval[I].IntervalExists(AnInterval) then
    begin
      Result := True;
      Exit;
    end;
end;

procedure TInterval.UpdateIntervalStart(Delta: TDateTime);
var
  I: Integer;
begin
  for I := 0 to ConnectionCount - 1 do
    Connection[I].UpdateIntervalStart(Delta);

  if IsCollection then
  begin
    for I := 0 to IntervalCount - 1 do
      Interval[I].UpdateIntervalStart(Delta);
  end else begin
    if FCanUpdate then
    begin
      FStartDate := FStartDate + Delta;
      FFinishDate := FFinishDate + Delta;
      FIntervalDone := FIntervalDone + Delta;
      FCanUpdate := False;
      Change;
    end;  
  end;
end;

procedure TInterval.PrepareToUpdate;
var
  I: Integer;
begin
  FCanUpdate := True;

  for I := 0 to IntervalCount - 1 do
    Interval[I].PrepareToUpdate;

  for I := 0 to ConnectionCount - 1 do
    Connection[I].PrepareToUpdate;
end;

procedure TInterval.ResetMovement;
begin
  FMovedBack:=False;
  FMovedFwd :=False;
  FMoved:=False;
end;

{
  **************************
  ***   Protected Part   ***
  **************************
}

procedure TInterval.PrepareDrawRect;
begin
  if not Assigned(FGantt) then exit;
  FDrawRect.Left :=
    Round
    (
      UnitsBetweenDates
      (
        FGantt.Calendar.VisibleStart,
        StartDate,
        FGantt.Calendar.MinorScale
      )
        *
      FGantt.Calendar.PixelsPerMinorScale
    );

  FDrawRect.Right :=
    Round
    (
      UnitsBetweenDates(
        FGantt.Calendar.VisibleStart,
        FinishDate,
        FGantt.Calendar.MinorScale
      )
        *
      FGantt.Calendar.PixelsPerMinorScale
    );
  if FDrawRect.Left < 0 then ;
end;


{
  ************************
  ***   Private Part   ***
  ************************
}


function TInterval.GetStartDate: TDateTime;
begin
  Result := CountStartDate;
end;

function TInterval.GetEarliestDate: TDateTime;
begin
  Result := FEarliestDate;
end;

function TInterval.GetIndex: Integer;
begin
  Result := -1;
  if not Assigned(FParent) then exit;
  result := Parent.FIntervals.IndexOf(Self);
end;

function TInterval.GetDep(Index: Integer): TInterval;
begin
  if Index < FDependencies.Count then
    Result := TInterval(FDependencies[Index])
  else Result := nil;
end;

function TInterval.GetDepCount: Integer;
begin
  Result := FDependencies.Count;
end;

function TInterval.GetLatest: TDateTime;
begin
  if Assigned(Self) and (not IsCollection) then
    Result := FinishDate+WaitTime
  else Result := 0;
end;

function TInterval.GetFinishDate: TDateTime;
begin
  Result := CountFinishDate;
end;

function TInterval.GetUsage: Extended;
begin
  if FinishDate-StartDate > 0 then
    Result := (NetTime/(FinishDate-StartDate))
  else Result := 0;
end;

procedure TInterval.SetBuffer(AValue: TDateTime);
begin
  if FBuffer=AValue then Exit;
  FBuffer:=AValue;
end;

procedure TInterval.SetColor(AValue: TColor);
begin
  if FColor=AValue then Exit;
  FColor:=AValue;
end;

procedure TInterval.SetEarliestDate(AValue: TDateTime);
begin
  FEarliestDate := AValue;
end;

procedure TInterval.SetId(AValue: Variant);
begin
  if Fid=AValue then Exit;
  Fid:=AValue;
  FChanged := False;
end;

procedure TInterval.SetStartDate(const Value: TDateTime);
begin
  if FStartDate=Value then exit;
  if FStartDate<Value then
    FMovedFwd:=True;
  if FStartDate>Value then
    FMovedBack:=True;
  FStartDate := Value;
  if FIntervalDone < FStartDate then
    FIntervalDone := FStartDate;
  FChanged := True;
  Change;
end;

procedure TInterval.SetFinishDate(const Value: TDateTime);
begin
  if FFinishDate=Value then exit;
  FFinishDate := Value;
  if FIntervalDone > FFinishDate then
    FIntervalDone := FFinishDate;
  FChanged := True;
  Change;
end;

function TInterval.GetDuration: TDateTime;
begin
  Result := FinishDate - StartDate;
end;

procedure TInterval.SetDuration(const Value: TDateTime);
begin
  FinishDate := StartDate + Value;
  FChanged := True;
end;

function TInterval.GetStampDuration: TTimeStamp;
var
  StartStamp, FinishStamp: TTimeStamp;
begin
  StartStamp := DateTimeToTimeStamp(StartDate);
  FinishStamp := DateTimeToTimeStamp(FinishDate);

  Result.Date := FinishStamp.Date - StartStamp.Date;
  Result.Time := FinishStamp.Time - StartStamp.Time;

  if Result.Time < 0 then
  begin
    Dec(Result.Date);
    Result.Time := MSecsPerDay + Result.Time;
  end;
end;

procedure TInterval.SetNetTime(AValue: TDateTime);
begin
  if FNetTime=AValue then Exit;
  FNetTime:=AValue;
  if FNetDuration<AValue then
    FNetDuration:=AValue;
end;

procedure TInterval.SetTask(const Value: String);
begin
  FTask := Value;
  if Assigned(FGantt) then
    FGantt.UpdateInterval;
  FChanged:=True;
end;

function TInterval.GetIntervalType: TIntervalType;
begin
  if (IntervalCount = 0) and (FStartDate = FFinishDate) then
    Result := itAction
  else
    Result := itPeriod;
end;

function TInterval.GetLevel: Integer;
begin
  if Parent = nil then
    Result := 1
  else
    Result := FParent.GetLevel + 1;
end;

function TInterval.GetIntervalCount: Integer;
begin
  Result := FIntervals.Count;
end;

function TInterval.GetInterval(AnIndex: Integer): TInterval;
begin
  Result := TInterval(FIntervals[AnIndex]);
end;

function TInterval.GetConnectionCount: Integer;
begin
  Result := FConnections.Count;
end;

function TInterval.GetConnection(AnIndex: Integer): TInterval;
begin
  Result := TInterval(FConnections[AnIndex]);
end;

function TInterval.GetIsCollection: Boolean;
begin
  Result := FIntervals.Count > 0;
end;

function TInterval.GetIntervalDone: TDateTime;
begin
  Result := FIntervalDone;
end;

procedure TInterval.SetIntervalDone(const Value: TDateTime);
begin
  FIntervalDone := Value;
  
  if FIntervalDone > FinishDate then
    FIntervalDone := FinishDate
  else if FIntervalDone < StartDate then
    FIntervalDone := StartDate;
end;

function TInterval.GetIsDrawRectClear: Boolean;
begin
  Result :=
    (DrawRect.Left = -1)
      and
    (DrawRect.Right = -1)
      and
    (DrawRect.Top = -1)
      and
    (DrawRect.Bottom = -1);  
end;

function TInterval.GetDoneRect: TRect;
begin
  if IsDrawRectClear then
  begin
    Result := DrawRect;
    Exit;
  end;
  if not Assigned(FGantt) then exit;
  Result.Top := FDrawRect.Top + (FDrawRect.Bottom - FDrawRect.Top) div 3;
  Result.Bottom := FDrawRect.Bottom - (FDrawRect.Bottom - FDrawRect.Top) div 3;

  Result.Left := FDrawRect.Left;
  Result.Right :=
    Round
    (
      UnitsBetweenDates
      (
        FGantt.Calendar.VisibleStart,
        FIntervalDone,
        FGantt.Calendar.MinorScale
      )
        *
      FGantt.Calendar.PixelsPerMinorScale
    );

end;

function TInterval.GetPercentMoveRect: TRect;
begin
  Result := DoneRect;

  if (Result.Left >= 0) or (Result.Right >= 0) then
  begin
    if Result.Left = Result.Right then
      Result.Right := Result.Left + 8
    else
      Result.Left := Result.Right - 8;
  end;
end;

function TInterval.GetLeftMoveRect: TRect;
var
  aSize: integer;
begin
  if IsDrawRectClear then
  begin
    Result := DrawRect;
    Exit;
  end;

  aSize := abs(min(((FDrawRect.Left - FDrawRect.Right) div 10),1));

  Result := Rect
  (
    FDrawRect.Left,
    FDrawRect.Top,
    FDrawRect.Left + aSize,
    FDrawRect.Bottom
  );
end;

function TInterval.GetRightMoveRect: TRect;
var
  aSize: integer;
begin
  if IsDrawRectClear then
  begin
    Result := DrawRect;
    Exit;
  end;

  aSize := abs(min(((FDrawRect.Left - FDrawRect.Right) div 10),1));

  Result := Rect
  (
    FDrawRect.Right - aSize,
    FDrawRect.Top,
    FDrawRect.Right,
    FDrawRect.Bottom
  );
end;

function TInterval.GetVisible: Boolean;
begin
  Result :=
    FVisible and
    (
      (Parent = nil)
        or
      ((Parent <> nil) and Parent.Visible)
    );
end;

procedure TInterval.SetVisible(const Value: Boolean);
//var
//  I: Integer;
begin
  FVisible := Value;
{
  if not Value then
    for I := 0 to IntervalCount - 1 do
      Interval[I].Visible := Value;
}
end;

function TInterval.GetOpened: Boolean;
begin
  Result :=
    ((IntervalCount = 0) and FVisible)
      or
    ((IntervalCount > 0) and Interval[0].Visible);
end;

procedure TInterval.SetOpened(const Value: Boolean);
var
  I: Integer;
begin
  if Assigned(FOnExpand) and Value then
    FOnExpand(Self);
  for I := 0 to IntervalCount - 1 do
    Interval[I].Visible := Value;
end;

function TInterval.CountStartDate: TDateTime;
var
  I: Integer;
  CurrStartDate: TDateTime;
begin
  if IntervalType = itPeriod then
  begin
    if IsCollection then
    begin
      Result := Interval[0].CountStartDate;

      for I := 1 to IntervalCount - 1 do
      begin
        CurrStartDate := Interval[I].CountStartDate;

        if Result > CurrStartDate then Result := CurrStartDate;
      end;
    end else begin
      Result := FStartDate;
    end;
  end else begin
    Result := FStartDate;
  end;
end;

function TInterval.CountFinishDate: TDateTime;
var
  I: Integer;
begin
  if IntervalType = itPeriod then
  begin
    if IsCollection then
    begin
      Result := Interval[0].CountFinishDate;

      for I := 1 to IntervalCount - 1 do
        if
          Result < Interval[I].CountFinishDate
        then
          Result := Interval[I].CountFinishDate;
    end else begin
      Result := FFinishDate;
    end;
  end else begin
    Result := FFinishDate;
  end;
end;

procedure TInterval.Change;
begin
  inc(FUpdateCount);
  if FDontChange or (FUpdating>0) then exit;
  FChanged := True;
  if Assigned(FOnChanged) then
    FOnChanged(Self);
end;

procedure TInterval.BeginUpdate;
begin
  if FUpdating=0 then
    FUpdateCount:=0;
  inc(FUpdating);
end;

procedure TInterval.EndUpdate(aDontchange: Boolean);
begin
  dec(FUpdating);
  if FUpdating>0 then exit;
  if FUpdateCount>0 then
    begin
      if not aDontchange then
        Change;
      FUpdateCount:=0;
    end;
end;



{
  -------------------------------------------
  ------                               ------
  ------      TGanttCalendar Class      ------
  ------                               ------
  -------------------------------------------
}


{
  ***********************
  ***   Public Part   ***
  ***********************
}

{TGanttCalendar}

constructor TGanttCalendar.Create(AnOwner: TgsGantt);
begin
  inherited Create(AnOwner);

  FVertScrollBar := TScrollBar.Create(Self);
  FVertScrollBar.Kind := sbVertical;
  FVertScrollBar.Enabled := True;
  FVertScrollBar.Visible := True;
  FVertScrollBar.Align := alRight;
  FVertScrollBar.TabStop := False;
  InsertControl(FVertScrollBar);

  FHorzScrollBar := TScrollBar.Create(Self);
  FHorzScrollBar.Kind := sbHorizontal;
  FHorzScrollBar.Enabled := True;
  FHorzScrollBar.Visible := True;
  FHorzScrollBar.Align := alBottom;
  FHorzScrollBar.TabStop := False;
  InsertControl(FHorzScrollBar);

  FHorzScrollBar.SetParams(0, 0, SCROLL_MAX);
  FHorzScrollBar.LargeChange := SCROLL_MAXSTEP;
  FHorzScrollBar.PageSize := SCROLL_MAXSTEP;
  FHorzScrollBar.SmallChange := SCROLL_MINSTEP;

  FVertScrollBar.SetParams(0, 0, SCROLL_MAX);
  FVertScrollBar.LargeChange := SCROLL_MAXSTEP;
  FVertScrollBar.PageSize := SCROLL_MAXSTEP;
  FVertScrollBar.SmallChange := SCROLL_MINSTEP;

  FMajorScale := tsMonth;
  FMinorScale := tsWeek;

  FStartDate := Now;
  FCurrentDate := 0;
  FVisibleStart := ClearToPeriodStart(MinorScale, FStartDate);

  FPixelsPerMinorScale := 30;
  FPixelsPerLine := 20; {24}

  Color := clWhite;
  FMajorColor := clBtnFace;
  FMinorColor := clBtnFace;

  FIntervals := AnOwner.FIntervals;
  FGantt := AnOwner;

  FBeforeStartDateCount := 0;
  FDragRect := Rect(-1, -1, -1, -1);
  FDragType := ditNone;
  FDragInterval := nil;
  FDragStarted := False;
  FFromDragPoint := 0;
  FConnectInterval := nil;

  FConnectFromPoint := Point(0, 0);
  FConnectToPoint := Point(0, 0);
end;

destructor TGanttCalendar.Destroy;
begin
  inherited Destroy;
end;

procedure TGanttCalendar.AddInterval(AnInterval: TInterval);
begin
  FGantt.AddInterval(AnInterval)
end;

procedure TGanttCalendar.InsertInterval(AnIndex: Integer; AnInterval: TInterval);
begin
  FGantt.InsertInterval(AnIndex, AnInterval);
end;

procedure TGanttCalendar.DeleteInterval(AnIndex: Integer);
begin
  FGantt.DeleteInterval(AnIndex);
end;

procedure TGanttCalendar.RemoveInterval(AnInterval: TInterval);
begin
  FGantt.RemoveInterval(AnInterval);
end;

function TGanttCalendar.IsDragging: Boolean;
begin
  Result := FMoveStarted or FDragStarted;
end;

{
  **************************
  ***   Protected Part   ***
  **************************
}


{DONE 2 -oDenis -cDrawing: ????????? ?????????}
procedure TGanttCalendar.Paint;
begin
  PaintToCanvas(Canvas);
end;

procedure TGanttCalendar.PaintToCanvas(aCanvas: TCanvas;DoClip : Boolean = True);
var
  ClipRgn: hRgn;
  List: TList;
  CurrInterval: TInterval;
  I, K: Integer;
  BMP : array of TBitmap;
  DoneRect: TRect;
  aTop: Integer;
  bri: Integer;
  aTextStyle : TTextStyle = (Alignment:taLeftJustify;
                             Layout : tlTop;
                             SingleLine : True;
                             Clipping  : True;
                             ExpandTabs:False;
                             ShowPrefix:False;
                             Wordbreak:false;
                             Opaque:False;
                             SystemFont:False;
                             RightToLeft:False);
begin
  aTop := FGantt.Tree.TopRow-1;
  if DoClip then
    begin
      ClipRgn := CreateRectRgn
      (
        0,
        0,
        Width - FVertScrollBar.Width * Integer(FVertScrollBar.Visible),
        Height - FHorzScrollBar.Height * Integer(FHorzScrollBar.Visible)
      );
    end;

  List := TList.Create;

  try
    if DoClip then
      SelectClipRgn(aCanvas.Handle, ClipRgn);

    Brush.Color := Color;
    if Doclip then
      begin
        aCanvas.FillRect(Rect(
          0, Width - FVertScrollBar.Width * Integer(FVertScrollBar.Visible),
          0, Height - FHorzScrollBar.Height * Integer(FHorzScrollBar.Visible)));
      end
    else
    aCanvas.FillRect(Rect(
      0, aCanvas.Width,
      0, aCanvas.Height));

    aCanvas.Brush.Color := FMajorColor;
    aCanvas.FillRect(Rect(0, 0, aCanvas.Width, MajorScaleHeight));

    aCanvas.Brush.Color := FMajorColor;
    aCanvas.FillRect(Rect(0, MajorScaleHeight, aCanvas.Width, MajorScaleHeight + MinorScaleHeight));

    FGantt.MakeIntervalListwithDeps(List);

    for I := 0 to List.Count - 1 do
      begin
        CurrInterval := TInterval(List[I]);
        CurrInterval.ClearDrawRect;
      end;

    for I := aTop to List.Count - 1 do TInterval(List[I]).PrepareDrawRect;

    for I := aTop to List.Count - 1 do
      if Assigned(TInterval(List[I]).FBP) then
        TInterval(List[I]).FBP(TInterval(List[I]),aCanvas,Rect(0,(I-aTop) * PixelsPerLine + 1 + StartDrawIntervals,aCanvas.Width,((I-aTop) * PixelsPerLine)+PixelsPerLine+StartDrawIntervals+1),VisibleStart,VisibleFinish,UnitsBetweenDates(VisibleStart,VisibleStart+1,MinorScale)*PixelsPerMinorScale);

    DrawHeaderLines(aCanvas);

    for I := aTop to List.Count - 1 do
    begin
      CurrInterval := TInterval(List[I]);

      if not CurrInterval.IsDrawRectClear then
      begin
        with aCanvas do
        begin
          if (CurrInterval.IntervalType = itAction) and (CurrInterval.Style = isDefault) then
          begin
            CurrInterval.Top := (I-aTop) * PixelsPerLine + 1 + StartDrawIntervals + PixelsPerLine div 2 - PixelsPerLine div 4;
            CurrInterval.Bottom := CurrInterval.Top + PixelsPerLine div 4 * 2;

            CurrInterval.Left := CurrInterval.Left - PixelsPerLine div 4;
            CurrInterval.Right := CurrInterval.Right + PixelsPerLine div 4;

            Brush.Color := CurrInterval.Color;
            if CurrInterval.InCriticalPath then Brush.Color := clRed;
            with CurrInterval.DrawRect do
              Polygon
              (
                [
                  Point(Left + (Right - Left) div 2, Top),
                  Point(Right, Top + (Bottom - Top) div 2),
                  Point(Left + (Right - Left) div 2, Bottom),
                  Point(Left, Top + (Bottom - Top) div 2),
                  Point(Left + (Right - Left) div 2, Top)
                ]
              );

            Brush.Color := CurrInterval.Color;
            if CurrInterval.InCriticalPath then Brush.Color := clRed;
            Brush.Style := bsClear;

            Pen.Color := clBlack;
            Pen.Style := psSolid;

            //Ellipse(CurrInterval.DrawRect);
            with CurrInterval.DrawRect do
              Polygon
              (
                [
                  Point(Left + (Right - Left) div 2, Top),
                  Point(Right, Top + (Bottom - Top) div 2),
                  Point(Left + (Right - Left) div 2, Bottom),
                  Point(Left, Top + (Bottom - Top) div 2),
                  Point(Left + (Right - Left) div 2, Top)
                ]
              );
            Brush.Style := bsSolid;
            Brush.Color := clBlue;
          end else if (CurrInterval.IsCollection) and (CurrInterval.Style = isDefault) then
          begin
            CurrInterval.Top := (I-aTop) * PixelsPerLine + 1 + StartDrawIntervals;
            CurrInterval.Bottom := (I-aTop) * PixelsPerLine + 1 + StartDrawIntervals + 4;
            CurrInterval.Left := CurrInterval.Left - 4;
            CurrInterval.Right := CurrInterval.Right + 4;

            Brush.Color := clBlack;
            Brush.Style := bsSolid;
            FillRect(CurrInterval.DrawRect);

            with CurrInterval.DrawRect do
            begin
              Polygon
              (
                [
                  Point(Left, Bottom),
                  Point(Left + 4, Bottom + 4),
                  Point(Left + 8, Bottom)
                ]
              );

              Polygon
              (
                [
                  Point(Right - 1, Bottom),
                  Point(Right - 5, Bottom + 4),
                  Point(Right - 9, Bottom)
                ]
              );
            end;

          end else if (CurrInterval.Style = isDefault) then begin
            CurrInterval.Top := (I-aTop) * PixelsPerLine + StartDrawIntervals + IntervalHeight div 4;
            CurrInterval.Bottom := ((I-aTop) + 1) * PixelsPerLine + StartDrawIntervals - IntervalHeight div 4;

            Brush.Bitmap := nil;
            Brush.Style := bsSolid;
            Brush.Color := CurrInterval.Color;
            if CurrInterval.InCriticalPath then
              Brush.Color := clRed;
            {$IFDEF WINDOWS}
            for bri := low(BMP) to high(BMP) do
              if BMP[bri].Canvas.Pixels[0,0] = Brush.Color then
                Brush.Bitmap := BMP[bri];
            if (not Assigned(Brush.Bitmap)) then
              begin
                Setlength(BMP,length(BMP)+1);
                BMP[high(BMP)] := TBitmap.Create;
                BMP[high(BMP)].Width := 2;
                BMP[high(BMP)].Height := 2;
                BMP[high(BMP)].Canvas.Pixels[0, 1] := Color;
                BMP[high(BMP)].Canvas.Pixels[1, 0] := Color;
                BMP[high(BMP)].Canvas.Pixels[0, 0] := Brush.Color;
                BMP[high(BMP)].Canvas.Pixels[1, 1] := Brush.Color;
                Brush.Bitmap := BMP[high(BMP)];
              end;
            {$ENDIF}
            FillRect(CurrInterval.DrawRect);
            Brush.Bitmap := nil;
            Brush.Color := CurrInterval.Color;
              if CurrInterval.InCriticalPath then
                Brush.Color := clRed;
            Brush.Style := bsSolid;
            FrameRect(CurrInterval.DrawRect);

            if CurrInterval.IntervalDone > CurrInterval.StartDate then
            begin
              DoneRect := CurrInterval.DoneRect;

              Brush.Style := bsSolid;
              Brush.Color := clBlack;
              FillRect(DoneRect);
            end;

            Brush.Style:=bsClear;
            TextOut(CurrInterval.DrawRect.Right+10,CurrInterval.DrawRect.Top,CurrInterval.Task);
          end;
        end;
      end;
    end;

  for I := 0 to List.Count - 1 do
    begin
      CurrInterval := TInterval(List[I]);
      for K := 0 to CurrInterval.ConnectionCount - 1 do
        ConnectIntervals(aCanvas,CurrInterval, CurrInterval.Connection[K]);
    end;

  FCurrentDate := VisibleStart;

  while FCurrentDate < VisibleFinish do
  begin
    DrawMinorScale(aCanvas);
    FCurrentDate := IncTime(FCurrentDate, MinorScale, 1);
  end;

  FCurrentDate := ClearToPeriodStart(MajorScale, VisibleStart);

  while FCurrentDate < VisibleFinish do
  begin
    DrawMajorScale(aCanvas);
    FCurrentDate := IncTime(FCurrentDate, MajorScale, 1);
  end;

  finally
    for bri := low(BMP) to high(BMP) do
      BMP[bri].Free;
    Setlength(BMP,0);
    List.Free;
    if DoClip then
      begin
        SelectClipRgn(aCanvas.Handle, 0);
        DeleteObject(ClipRgn);
      end;
  end;
end;

{
  ************************
  ***   Private Part   ***
  ************************
}


procedure TGanttCalendar.SetMajorScale(const Value: TTimeScale);
begin
  if csReading in ComponentState then
    FMajorScale := Value
  else if Value < MinorScale then
    raise Exception.Create('MajorScale should by higher than MinorScale')
  else if Value = MinorScale then
    raise Exception.Create('MajorScale should by different from MinorScale')
  else begin
    FMajorScale := Value;
    Invalidate;
  end;
end;

function TGanttCalendar.GetEndDate: TDateTime;
begin
  Result := StartDate+FPixelsPerMinorScale;
end;

procedure TGanttCalendar.SetMinorScale(const Value: TTimeScale);
begin
  if csReading in ComponentState then
    FMinorScale := Value
  else if Value > MajorScale then
    raise Exception.Create('MinorScale should by lower than MajorScale')
  else if Value = MajorScale then
    raise Exception.Create('MinorScale should by different from MajorScale')
  else begin
    FMinorScale := Value;
    Invalidate;
  end;
end;

procedure TGanttCalendar.SetPixelsPerMinorScale(const Value: Integer);
begin
  if Value < 10 then
    FPixelsPerMinorScale := 10
  else
    FPixelsPerMinorScale := Value;

  FGantt.UpdateInterval;
end;

procedure TGanttCalendar.SetPixelsPerLine(const Value: Integer);
begin
  if Value < 1 then
    FPixelsPerLine := 1
  else
    FPixelsPerLine := Value;

  FGantt.UpdateInterval;
end;

procedure TGanttCalendar.SetStartDate(const Value: TDateTime);
begin
  FStartDate := Value;
  FVisibleStart := ClearToPeriodStart(MinorScale, FStartDate);
  Invalidate;
  if Assigned(FStartDateChanged) then
    FStartDateChanged(Self);
end;

function TGanttCalendar.GetVisibleFinish: TDateTime;
begin
  Result := IncTime(VisibleStart, MinorScale, MinorVisibleUnits + 1);
end;

function TGanttCalendar.GetMinorVisibleUnitsCount: Integer;
begin
  Result := ClientWidth div PixelsPerMinorScale;
  Inc(Result, Integer(ClientWidth mod PixelsPerMinorScale <> 0));
end;

function TGanttCalendar.GetMajorScale: Integer;
begin
  Canvas.Font := Font;
  Result := Trunc(Canvas.TextHeight('A') * 1.5);
end;

function TGanttCalendar.GetMinorScale: Integer;
begin
  Canvas.Font := Font;
  Result := Trunc(Canvas.TextHeight('A') * 1.5);
end;

function TGanttCalendar.GetSeconds: Integer;
var
  Hour, Min, Sec, MSec: Word;
begin
  DecodeTime(FCurrentDate, Hour, Min, Sec, MSec);
  Result := Sec;
end;

function TGanttCalendar.GetMinutes: Integer;
var
  Hour, Min, Sec, MSec: Word;
begin
  DecodeTime(FCurrentDate, Hour, Min, Sec, MSec);
  Result := Min;
end;

function TGanttCalendar.GetHours: Integer;
var
  Hour, Min, Sec, MSec: Word;
begin
  DecodeTime(FCurrentDate, Hour, Min, Sec, MSec);
  Result := Hour;
end;

function TGanttCalendar.GetDays: Integer;
var
  Year, Month, Day: Word;
begin
  DecodeDate(FCurrentDate, Year, Month, Day);
  Result := Day;
end;

function TGanttCalendar.GetDayOfWeek: Integer;
var
  S: TTimeStamp;
begin
  S := DateTimeToTimeStamp(FCurrentDate);
  Result := (S.Date - 1) mod 7 + 1;
end;

function TGanttCalendar.GetWeeks: Integer;
var
  S: TTimeStamp;
begin
  S := DateTimeToTimeStamp(FCurrentDate);
  Result := (S.Date - 1) div 7;
end;

function TGanttCalendar.GetMonthes: Integer;
var
  Year, Month, Day: Word;
begin
  DecodeDate(FCurrentDate, Year, Month, Day);
  Result := Month;
end;

function TGanttCalendar.GetQuarters: Integer;
var
  Year, Month, Day: Word;
begin
  DecodeDate(FCurrentDate, Year, Month, Day);
  Result := Month div 3 + 1;
end;

function TGanttCalendar.GetHalfYears: Integer;
var
  Year, Month, Day: Word;
begin
  DecodeDate(FCurrentDate, Year, Month, Day);
  Result := Month div 6 + 1;
end;

function TGanttCalendar.GetYears: Integer;
var
  Year, Month, Day: Word;
begin
  DecodeDate(FCurrentDate, Year, Month, Day);
  Result := Year;
end;

function TGanttCalendar.GetTimeUnitByScale(ATimeScale: TTimeScale): Integer;
begin
  case ATimeScale of
    tsMinute:
      Result := Minutes;
    tsHour:
      Result := Hours;
    tsDay:
      Result := Days;
    tsWeek,tsWeekNum,tsWeekNumPlain:
      Result := Weeks;
    tsMonth:
      Result := Monthes;
    tsQuarter:
      Result := Quarters;
    tsHalfYear:
      Result := HalfYears;
    tsYear:
      Result := Years;
    else
      Result := 0;
  end;
end;

procedure TGanttCalendar.SetMajorColor(const Value: TColor);
begin
  FMajorColor := Value;
  Repaint;
end;

procedure TGanttCalendar.SetMinorColor(const Value: TColor);
begin
  FMinorColor := Value;
  Repaint;
end;

function TGanttCalendar.GetIntervalCount: Integer;
begin
  Result := FIntervals.Count;
end;

function TGanttCalendar.GetInterval(AnIndex: Integer): TInterval;
begin
  Result := TInterval(Fintervals[AnIndex]);
end;

function TGanttCalendar.GetStartDrawIntervals: Integer;
begin
  Result := MajorScaleHeight + MinorScaleHeight;
end;

function TGanttCalendar.GetIntervalHeight: Integer;
begin
  Result := PixelsPerLine;
end;

function TGanttCalendar.GetIntervalWidth: Double;
begin
  Result := UnitsBetweenDates(VisibleStart,VisibleStart+1,MinorScale)*PixelsPerMinorScale;
end;

procedure TGanttCalendar.DrawHeaderLines(aCanvas: TCanvas);
begin
  with aCanvas do
  begin
    aCanvas.Pen.Style := psSolid;
    aCanvas.Pen.Color := clBlack;

    MoveTo(0, MajorScaleHeight);
    LineTo(aCanvas.Width, MajorScaleHeight);

    MoveTo(0, MajorScaleHeight + MinorScaleHeight);
    LineTo(aCanvas.Width, MajorScaleHeight + MinorScaleHeight);
  end;
end;

procedure TGanttCalendar.ConnectIntervals(aCanvas : TCanvas;FromInterval, ToInterval: TInterval);
var
  Plus: Integer;
  FromRect, ToRect: TRect;
begin
  with aCanvas do
  begin
    Brush.Style := bsSolid;
    Brush.Color := clBlack;
    if Assigned(FromInterval) and Assigned(ToInterval) and FromInterval.InCriticalPath and ToInterval.InCriticalPath then
      Brush.Color := clRed;
    Pen.Color := clWhite;
    Pen.Style := psDot;
    if FromInterval.WaitTime>0 then
      begin
        Pen.Color := clBlack;
        Pen.Style := psSolid;
      end;

    FromRect := FromInterval.DrawRect;
    ToRect := ToInterval.DrawRect;

    if ToInterval.IsCollection then
      ToRect.Left := ToRect.Left + 4;

    Plus := 3 * Integer((ToRect.Left - FromRect.Right) in [0, 1, 2]);

    MoveTo
    (
      FromRect.Right,
      FromRect.Top + (FromRect.Bottom - FromRect.Top) div 2
    );

    if FromRect.Right > ToRect.Left + Plus then
    begin
      LineTo
      (
        FromRect.Right + 10,
        FromRect.Top + (FromRect.Bottom - FromRect.Top) div 2
      );

      LineTo
      (
        FromRect.Right + 10,
        FromRect.Top
          +
        (
          ToRect.Top + (ToRect.Bottom - ToRect.Top) div 2
            -
          FromRect.Top + (FromRect.Bottom - FromRect.Top) div 2
        )
          div
        2
      );

      LineTo
      (
        ToRect.Left - 10,
        FromRect.Top
          +
        (
          ToRect.Top + (ToRect.Bottom - ToRect.Top) div 2
            -
          FromRect.Top + (FromRect.Bottom - FromRect.Top) div 2
        )
          div
        2
      );

      LineTo
      (
        ToRect.Left - 10,
        ToRect.Top + (ToRect.Bottom - ToRect.Top) div 2
      );

      LineTo
      (
        ToRect.Left,
        ToRect.Top + (ToRect.Bottom - ToRect.Top) div 2
      );

      Polygon
      (
        [
          Point(ToRect.Left, ToRect.Top + (ToRect.Bottom - ToRect.Top) div 2),
          Point(ToRect.Left - 5, ToRect.Top + (ToRect.Bottom - ToRect.Top) div 2 + 5),
          Point(ToRect.Left - 5, ToRect.Top + (ToRect.Bottom - ToRect.Top) div 2 - 5),
          Point(ToRect.Left, ToRect.Top + (ToRect.Bottom - ToRect.Top) div 2)
        ]
      );
    end else begin
      if FromInterval.WaitTime>0 then
        begin
          LineTo
          (
            FromRect.Right + Plus + round((UnitsBetweenDates(VisibleStart,VisibleStart+1,MinorScale)*PixelsPerMinorScale)*FromInterval.WaitTime),
            FromRect.Top + (FromRect.Bottom - FromRect.Top) div 2
          );
          Pen.Style := psDot;
          Pen.Color := clWhite;
          LineTo
          (
            ToRect.Left + Plus,
            FromRect.Top + (FromRect.Bottom - FromRect.Top) div 2
          );
        end
      else
        begin
          LineTo
          (
            ToRect.Left + Plus,
            FromRect.Top + (FromRect.Bottom - FromRect.Top) div 2
          );
        end;

      if FromRect.Top > ToRect.Top then
      begin
        LineTo
        (
          ToRect.Left + Plus,
          ToRect.Bottom + 6
        );

        Polygon
        (
          [
            Point(ToRect.Left + Plus, ToRect.Bottom + 6),
            Point(ToRect.Left + Plus - 5, ToRect.Bottom + 6),
            Point(ToRect.Left + Plus, ToRect.Bottom + 1),
            Point(ToRect.Left + Plus + 5, ToRect.Bottom + 6),
            Point(ToRect.Left + Plus, ToRect.Bottom + 6)
          ]
        );
      end else begin
        LineTo
        (
          ToRect.Left + Plus,
          ToRect.Top - 6
        );

        Polygon
        (
          [
            Point(ToRect.Left + Plus, ToRect.Top - 6),
            Point(ToRect.Left + Plus - 5, ToRect.Top - 6),
            Point(ToRect.Left + Plus, ToRect.Top - 1),
            Point(ToRect.Left + Plus + 5, ToRect.Top - 6),
            Point(ToRect.Left + Plus, ToRect.Top - 6)
          ]
        );
      end;
    end;
  end;
end;

procedure TGanttCalendar.DrawMinorScale(aCanvas: TCanvas);
var
  R, TextR: TRect;
  OldTransparent: Integer;
begin
  with aCanvas do
  begin
    if IsNewPeriod(MinorScale) then
    begin
      OldTransparent := SetBKMode(Handle, TRANSPARENT);

      R.Left := {Trunc}Round
        (
          UnitsBetweenDates(VisibleStart, FCurrentDate, MinorScale)
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

      aCanvas.Pen.Style := psSolid;
      aCanvas.Pen.Color := clBlack;

      MoveTo(R.Left, MajorScaleHeight);
      LineTo(R.Left, MajorScaleHeight + MinorScaleHeight);
    end;
  end;
end;

procedure TGanttCalendar.DrawMajorScale(aCanvas: TCanvas);
var
  OldTransparent: Integer;
  R: TRect;
  TextR: TRect;
begin
  with aCanvas do
  begin
    OldTransparent := SetBKMode(Handle, TRANSPARENT);

    R.Left := {Trunc}Round
      (
        UnitsBetweenDates(VisibleStart, FCurrentDate, MinorScale)
          *
        PixelsPerMinorScale
      );
    R.Right := {Trunc}Round
      (
        UnitsBetweenDates(VisibleStart, IncTime(FCurrentDate, MajorScale, 1), MinorScale)
          *
        PixelsPerMinorScale
      );

    R.Top := 1;
    R.Bottom := MajorScaleHeight - 1;

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

    aCanvas.Pen.Style := psSolid;
    aCanvas.Pen.Color := clBlack;

    MoveTo(R.Left, 0);
    LineTo(R.Left, MajorScaleHeight);

    aCanvas.Pen.Style := psDot;
    aCanvas.Pen.Color := clWhite;
    aCanvas.Brush.Color := clBlack;

    //MoveTo(R.Left, MajorScaleHeight + MinorScaleHeight);
    //LineTo(R.Left, Height);
  end;
end;

function TGanttCalendar.IsNewPeriod(AScale: TTimeScale; AUseSub: Boolean = False): Boolean;
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

procedure TGanttCalendar.UpdateScrollbars;
begin
  if FVertScrollBar.Visible then
  begin
    RemoveControl(FVertScrollBar);
    FVertScrollBar.Enabled := False;
    FVertScrollBar.Visible := False;
  end;

  FHorzScrollBar.Enabled := True;
  FHorzScrollBar.OnScroll := @DoOnHorzScroll;
end;

procedure TGanttCalendar.DoOnHorzScroll(Sender: TObject; ScrollCode: TScrollCode; var ScrollPos: Integer);
var
  R: TRect;
begin
  if (ScrollCode in [scLineUp, scPageUp, scEndScroll])
  and (ScrollPos = 0)
  and (FHorzScrollBar.Position = 0)
  then
    begin
      if (ScrollCode = scLineUp) then
        begin
          FVisibleStart := ClearToPeriodStart(MinorScale, IncTime(FVisibleStart, MinorScale, -1));
          Inc(FBeforeStartDateCount);
          R := Rect(
            0,
            0,
            Width - FVertScrollBar.Width * Integer(FVertScrollBar.Visible),
            Height - FHorzScrollBar.Height * Integer(FHorzScrollBar.Visible)
          );
          InvalidateRect(Handle, @R, True);
          if Assigned(FStartDateChanged) then
            FStartDateChanged(Self);
        end;
    end
  else
    begin
      if ClearToPeriodStart(MinorScale, FStartDate) > FVisibleStart then
        begin
          FVisibleStart := ClearToPeriodStart(MinorScale, IncTime(FVisibleStart, MinorScale, ScrollPos));
          ScrollPos := ScrollPos - FBeforeStartDateCount;

          if ScrollPos < 0 then
          begin
            FBeforeStartDateCount := Abs(ScrollPos);
            ScrollPos := 0;
          end;
          if Assigned(FStartDateChanged) then
            FStartDateChanged(Self);
        end
      else
        begin
          FVisibleStart := ClearToPeriodStart(MinorScale, IncTime(FStartDate, MinorScale, ScrollPos));
          FBeforeStartDateCount := 0;
          if Assigned(FStartDateChanged) then
            FStartDateChanged(Self);
        end;
      if (ScrollCode <> scEndScroll) then
        begin
          R := Rect
          (
            0,
            0,
            Width - FVertScrollBar.Width * Integer(FVertScrollBar.Visible),
            Height - FHorzScrollBar.Height * Integer(FHorzScrollBar.Visible)
          );
          InvalidateRect(Handle, @R, True);
        end;
      if ((ScrollCode = scPosition) and (FHorzScrollBar.Max - SCROLL_MAXSTEP < ScrollPos))
      or ((ScrollCode in [scLineDown, scPageDown, scBottom]) and (FHorzScrollBar.Max = ScrollPos))
      then
        begin
          FHorzScrollBar.Max := FHorzScrollBar.Max + SCROLL_MAXSTEP;
          ScrollPos := FHorzScrollBar.Max - SCROLL_MAXSTEP;
        end
      else if (ScrollPos <= FHorzScrollBar.Max - SCROLL_MAXSTEP)
           and (ScrollCode in [scLineUp, scPageUp, scPosition, scTop])
           then
        begin
          if ScrollPos > SCROLL_MAX then
            FHorzScrollBar.Max := FHorzScrollBar.Position
          else
            FHorzScrollBar.Max := SCROLL_MAX;
        end;
    end;
end;

procedure TGanttCalendar.WMSize(var Message: TLMSize);
begin
  UpdateScrollbars;

  if FVertScrollBar.Visible then
  begin
    if FHorzScrollBar.Visible then
      SetWindowPos
      (
        FVertScrollBar.Handle,
        0,
        Width - FVertScrollBar.Width,
        0,
        FVertScrollBar.Width,
        Height - FHorzScrollBar.Height,
        SWP_SHOWWINDOW or SWP_NOZORDER or  SWP_NOSENDCHANGING or SWP_NOACTIVATE
      )
    else
      SetWindowPos
      (
        FVertScrollBar.Handle,
        0,
        Width - FVertScrollBar.Width,
        0,
        FVertScrollBar.Width,
        Height,
        SWP_SHOWWINDOW or SWP_NOZORDER or SWP_NOSENDCHANGING or SWP_NOACTIVATE
      );
  end;

  if FHorzScrollBar.Visible then
  begin
    if FVertScrollBar.Visible then
      SetWindowPos
      (
        FHorzScrollBar.Handle,
        0,
        0,
        Height - FHorzScrollBar.Height,
        Width - FVertScrollBar.Width,
        FHorzScrollBar.Height,
        SWP_SHOWWINDOW or SWP_NOZORDER or SWP_NOSENDCHANGING or SWP_NOACTIVATE
      )
    else
      SetWindowPos
      (
        FHorzScrollBar.Handle,
        0,
        0,
        Height - FHorzScrollBar.Height,
        Width,
        FHorzScrollBar.Height,
        SWP_SHOWWINDOW or SWP_NOZORDER or SWP_NOSENDCHANGING or SWP_NOACTIVATE
      );
  end;

  inherited;
end;

procedure TGanttCalendar.CMMouseEnter(var Message: TLMessage);
begin
  inherited;
end;

procedure TGanttCalendar.CMMouseLeave(var Message: TLMessage);
begin
  FMoveStarted:=False;
  inherited;
end;

procedure TGanttCalendar.WMMouseMove(var Message: TLMMouseMove);
var
  List: TList;
  CurrInterval: TInterval;
  I: Integer;
  Found: Boolean;
  aTop: Integer;
  aIdx : Integer = 0;
  aCount : Integer = 0;
  R: TRect;
  NewDate: TDateTime;
  aDiff: Int64;
begin
  List := TList.Create;
  try
    FGantt.MakeIntervalList(List);
    aCount := List.Count;

    if not FDragStarted then
    begin
      Found := False;

      for I := 0 to List.Count - 1 do
      begin
        CurrInterval := TInterval(List[I]);

        if CurrInterval.IsCollection then Continue;

        if IsInRect(Message.XPos,  Message.YPos, CurrInterval.DrawRect) and not CurrInterval.Fixed then
        begin
          if (CurrInterval.IntervalType = itPeriod) and IsInRect(Message.XPos,  Message.YPos, CurrInterval.PercentMoveRect)
          then begin
            Cursor := crGanttPercent;
            FDragType := ditPercent;
            FDragRect := CurrInterval.DoneRect;
          end else if
            (CurrInterval.IntervalType = itPeriod)
              and
            IsInRect(Message.XPos,  Message.YPos, CurrInterval.LeftMoveRect)
          then begin
            Cursor := crGanttLeftMove;
            FDragType := ditLeftMove;
            FDragRect := CurrInterval.DrawRect;
          end else if
            (CurrInterval.IntervalType = itPeriod)
              and
            IsInRect(Message.XPos,  Message.YPos, CurrInterval.RightMoveRect)
          then begin
            Cursor := crGanttRightMove;
            FDragType := ditRightMove;
            FDragRect := CurrInterval.DrawRect;
          end else begin
            Cursor := crGanttMiddle;
            FDragType := ditMiddle;
            FDragRect := CurrInterval.DrawRect;
          end;

          Found := True;
          FDragInterval := CurrInterval;
          FDragStarted := False;
          if Assigned(FOverInterval) then
            FOverInterval(Self,CurrInterval,Message.XPos,Message.YPos);
          Break;
        end;
      end;

      if (Message.Keys and MK_LBUTTON)=MK_LBUTTON then
        FMoveStarted := True;
      if not Found then
      begin
        if Assigned(FOverInterval) then
          FOverInterval(Self,nil,Message.XPos,Message.YPos);
        Cursor := crDefault;
        if Assigned(FDragInterval) and FMoveStarted then
          FDragInterval.EndUpdate;
        FDragInterval := nil;
        FDragType := ditNone;
        FDragRect := Rect(-1, -1, -1, -1);
        FDragStarted := False;
      end;
    end else if Assigned(FDragInterval) and (FDragType <> ditNone) and FDragStarted and (abs(Message.XPos-FConnectFromPoint.X)>5) and (not FDragInterval.Fixed) then
    begin
      case FDragType of
        ditMiddle:
        begin
          if FDragInterval.IntervalType = itPeriod then
          begin
            {
            R := FDragInterval.DoneRect;

            FDragInterval.StartDate :=
              IncTimeEx(VisibleStart, MinorScale, FDragRect.Left / PixelsPerMinorScale);

            FDragInterval.FinishDate :=
              IncTimeEx(VisibleStart, MinorScale, FDragRect.Right / PixelsPerMinorScale);

            FDragInterval.IntervalDone :=
              IncTimeEx
              (
                VisibleStart,
                MinorScale,
                (FDragRect.Left + (R.Right - R.Left)) / PixelsPerMinorScale
              );
              }
          end else begin
            FDragInterval.StartDate :=
              IncTimeEx(
                VisibleStart,
                MinorScale,
                (FDragRect.Left + (FDragRect.Right - FDragRect.Left) div 2) / PixelsPerMinorScale);

            FDragInterval.FinishDate := FDragInterval.StartDate;
          end;
          FDragInterval.FMoved:=True;
        end;
        ditRightMove:
        begin
          {
          NewDate :=
            IncTimeEx(VisibleStart, MinorScale, FDragRect.Right / PixelsPerMinorScale);

          if FDragInterval.StartDate > NewDate then
            FDragInterval.FinishDate := FDragInterval.StartDate
          else
            FDragInterval.FinishDate := NewDate;
          }
        end;
        ditLeftMove:
        begin
          {
          NewDate := IncTimeEx(VisibleStart, MinorScale, FDragRect.Left / PixelsPerMinorScale);

          if FDragInterval.FinishDate < NewDate then
          begin
            FDragInterval.StartDate := FDragInterval.FinishDate;
          end else
            FDragInterval.StartDate := NewDate;
          }
        end;
        ditPercent:
        begin
          FDragInterval.IntervalDone :=
            IncTimeEx(VisibleStart, MinorScale, FDragRect.Right / PixelsPerMinorScale);
        end;
      end;

      aTop := FGantt.Tree.TopRow-1;
      Canvas.Brush.Color:=clWindow;
      Canvas.Rectangle(FDragInterval.DrawRect);
      aIdx := List.IndexOf(FDragInterval);
      if Assigned(FDragInterval.FBP) then
        FDragInterval.FBP(FDragInterval,Canvas,Rect(0,(aIdx-aTop) * PixelsPerLine + 1 + StartDrawIntervals,Width,((aIdx-aTop) * PixelsPerLine)+PixelsPerLine+StartDrawIntervals+1),VisibleStart,VisibleFinish,UnitsBetweenDates(VisibleStart,VisibleStart+1,MinorScale)*PixelsPerMinorScale);
      //Canvas.Brush.Color := clWhite;
      Canvas.DrawFocusRect(FDragRect);
      //Canvas.Brush.Color := clBlue;
      Canvas.DrawFocusRect(
        Rect(FDragRect.Left + 1, FDragRect.Top + 1, FDragRect.Right - 1, FDragRect.Bottom - 1));

      case FDragType of
        ditMiddle:
        begin
          if ((Message.YPos < FDragRect.Top) or (Message.YPos > FDragRect.Bottom)) and (abs(Message.XPos-FConnectFromPoint.X)<30) then
          begin
            FDragType := ditConnect;
            Screen.Cursor := crGanttConnect;
            //Cursor := crGanttConnect;
            FDragRect := FDragInterval.DrawRect;
          end else begin
            FDragRect := Rect
            (
              Message.XPos - FFromDragPoint,
              FDragRect.Top,
              Message.XPos - FFromDragPoint + FDragRect.Right - FDragRect.Left,
              FDragRect.Bottom
            );
          end;  
        end;
        ditRightMove:
          FDragRect.Right := Message.XPos;
        ditLeftMove:
          FDragRect.Left := Message.XPos;
        ditPercent:
        begin
          if FDragInterval.DrawRect.Right < Message.XPos then
            FDragRect.Right := FDragInterval.DrawRect.Right
          else
            FDragRect.Right := Message.XPos;
        end;
        ditConnect:
        begin
          Canvas.Brush.Color := clWhite;
          Canvas.Pen.Style := psSolid;
          Canvas.Pen.Mode := pmNotXor;
          Canvas.MoveTo(FConnectFromPoint.X, FConnectFromPoint.Y);
          Canvas.LineTo(FConnectToPoint.X, FConnectToPoint.Y);

          FConnectToPoint := Point(Message.XPos, Message.YPos);
          Canvas.Pen.Mode := pmNotXor;
          Canvas.MoveTo(FConnectFromPoint.X, FConnectFromPoint.Y);
          Canvas.LineTo(FConnectToPoint.X, FConnectToPoint.Y);
          Canvas.Pen.Mode := pmCopy;

          if FConnectInterval <> nil then
          begin
            Canvas.Brush.Color := clWhite;
            Canvas.DrawFocusRect(FConnectInterval.DrawRect);
            Canvas.Brush.Color := clBlue;
            Canvas.DrawFocusRect(
              Rect(
                FConnectInterval.DrawRect.Left + 1,
                FConnectInterval.DrawRect.Top + 1,
                FConnectInterval.DrawRect.Right - 1,
                FConnectInterval.DrawRect.Bottom - 1
              )
            );
          end;

          FConnectInterval := nil;

          for I := 0 to List.Count - 1 do
          begin
            CurrInterval := TInterval(List[I]);
            if CurrInterval = FDragInterval then Continue;

            if IsInRect(Message.XPos,  Message.YPos, CurrInterval.DrawRect) then
            begin
              FConnectInterval := CurrInterval;
              Break;
            end;
          end;

          if FConnectInterval <> nil then
          begin
            Canvas.Brush.Color := clWhite;
            Canvas.DrawFocusRect(FConnectInterval.DrawRect);
            Canvas.Brush.Color := clBlue;
            Canvas.DrawFocusRect(
              Rect(
                FConnectInterval.DrawRect.Left + 1,
                FConnectInterval.DrawRect.Top + 1,
                FConnectInterval.DrawRect.Right - 1,
                FConnectInterval.DrawRect.Bottom - 1
              ));
          end;
        end;
      end;

      Canvas.Brush.Color := clWhite;
      Canvas.DrawFocusRect(FDragRect);
      Canvas.Brush.Color := clBlue;
      Canvas.DrawFocusRect(
        Rect(FDragRect.Left + 1, FDragRect.Top + 1, FDragRect.Right - 1, FDragRect.Bottom - 1));
    end;
  finally
    List.Free;
  end;
  if FMoveStarted then
    begin
      aDiff := round((FConnectFromPoint.X-Message.XPos)/(Width/GetMinorVisibleUnitsCount));
      if (aDiff<>0) or (round((FConnectFromPoint.Y-Message.YPos)/FGantt.Tree.DefaultRowHeight) <> 0) then
        begin
          FVisibleStart := IncTime(FVisibleStart, MinorScale, aDiff);

          if aCount > (Height div FGantt.Tree.DefaultRowHeight) then
            FGantt.Tree.TopRow:=FGantt.Tree.TopRow+round((FConnectFromPoint.Y-Message.YPos)/FGantt.Tree.DefaultRowHeight)
          else FGantt.Tree.TopRow:=0;
          FConnectFromPoint := Point(Message.XPos, Message.YPos);
          if Assigned(FStartDateChanged) then
            FStartDateChanged(Self);
        end;
      Invalidate;
    end;
  inherited;
end;

procedure TGanttCalendar.WMLButtonDown(var Message: TLMLButtonDown);
begin
  FConnectFromPoint := Point(Message.XPos, Message.YPos);
  FMoveStarted := False;
  if Assigned(FDragInterval) and (FDragType <> ditNone) and not FDragStarted then
  begin
    FDragStarted := True;
    if Assigned(FDragInterval) then
      FDragInterval.beginUpdate;
    FFromDragPoint := Message.XPos - FDragRect.Left;

    FConnectFromPoint := Point(Message.XPos, Message.YPos);
    FConnectToPoint := Point(Message.XPos, Message.YPos);

    Canvas.Brush.Color := clWhite;
    Canvas.DrawFocusRect(FDragRect);
    Canvas.Brush.Color := clBlue;
    Canvas.DrawFocusRect(
      Rect(FDragRect.Left + 1, FDragRect.Top + 1, FDragRect.Right - 1, FDragRect.Bottom - 1));
  end;

  inherited;
end;

procedure TGanttCalendar.WMLButtonUp(var Message: TLMRButtonDown);
var
  NewDate: TDateTime;
  R: TRect;
  I: Integer;
begin
  FMoveStarted:=False;
  if Assigned(FDragInterval) and (FDragType <> ditNone) and FDragStarted then
  begin
    Canvas.Brush.Color := clWhite;
    Canvas.DrawFocusRect(FDragRect);
    Canvas.Brush.Color := clBlue;
    Canvas.DrawFocusRect(
      Rect(FDragRect.Left + 1, FDragRect.Top + 1, FDragRect.Right - 1, FDragRect.Bottom - 1));

    case FDragType of
      ditMiddle:
      begin
        if (abs(Message.XPos-FConnectFromPoint.X)>5) then
          begin
            if FDragInterval.IntervalType = itPeriod then
            begin
              R := FDragInterval.DoneRect;

              FDragInterval.StartDate :=
                IncTimeEx(VisibleStart, MinorScale, FDragRect.Left / PixelsPerMinorScale);

              FDragInterval.FinishDate :=
                IncTimeEx(VisibleStart, MinorScale, FDragRect.Right / PixelsPerMinorScale);

              FDragInterval.IntervalDone :=
                IncTimeEx
                (
                  VisibleStart,
                  MinorScale,
                  (FDragRect.Left + (R.Right - R.Left)) / PixelsPerMinorScale
                );
            end else begin
              FDragInterval.StartDate :=
                IncTimeEx(
                  VisibleStart,
                  MinorScale,
                  (FDragRect.Left + (FDragRect.Right - FDragRect.Left) div 2) / PixelsPerMinorScale);

              FDragInterval.FinishDate := FDragInterval.StartDate;
            end;
          end;
      end;
      ditRightMove:
      begin
        NewDate :=
          IncTimeEx(VisibleStart, MinorScale, FDragRect.Right / PixelsPerMinorScale);

        if FDragInterval.StartDate > NewDate then
          FDragInterval.FinishDate := FDragInterval.StartDate
        else
          begin
            FDragInterval.NetDuration:=FDragInterval.NetDuration+(NewDate-FDragInterval.FinishDate);
            FDragInterval.FinishDate := NewDate;
          end;
      end;
      ditLeftMove:
      begin
        NewDate := IncTimeEx(VisibleStart, MinorScale, FDragRect.Left / PixelsPerMinorScale);

        if FDragInterval.FinishDate < NewDate then
        begin
          FDragInterval.StartDate := FDragInterval.FinishDate;
        end else
          FDragInterval.StartDate := NewDate;
      end;
      ditPercent:
      begin
        FDragInterval.IntervalDone :=
          IncTimeEx(VisibleStart, MinorScale, FDragRect.Right / PixelsPerMinorScale);
      end;
      ditConnect:
      begin
        if FConnectInterval <> nil then
        begin
          for I := 0 to IntervalCount - 1 do
            Interval[I].PrepareToUpdate;
          FDragInterval.AddConnection(FConnectInterval,True,True);
        end;
      end;
    end;

    Cursor := crDefault;
    Screen.Cursor := crDefault;

    FConnectInterval := nil;
    FConnectFromPoint := Point(-1, -1);
    FConnectToPoint := Point(-1, -1);
    if Assigned(FDragInterval) then
      FDragInterval.EndUpdate;
    FDragInterval := nil;
    FDragType := ditNone;
    FDragStarted := False;
    FFromDragPoint := 0;
    FDragRect := Rect(-1, -1, -1, -1);

    FGantt.UpdateInterval;
  end;

  inherited;
end;

{
  ----------------------------------------
  ------                            ------
  ------      TGanttTree Class      ------
  ------                            ------
  ----------------------------------------
}

{TGanttTree}

{
  ***********************
  ***   Public Part   ***
  ***********************
}

{
  ?????? ????????? ?????????.
}

constructor TGanttTree.Create(AnOwner: TgsGantt);
begin
  inherited Create(AnOwner);

  FGantt := AnOwner;
  FIntervals := FGantt.FIntervals;
  FTaskEditable := True;

  //ParentCtl3d := False;
  //Ctl3d := False;
  BorderStyle := bsNone;
  DefaultDrawing := False;

  ColCount := 8;
  RowCount := 100;
  DefaultColWidth := 40;
  DefaultRowHeight := 24;

  FIndent := 10;

  Options :=
  [
    goFixedVertLine, goFixedHorzLine, goVertLine,
    goHorzLine, goColSizing
  ];

  FBranchFont := TFont.Create;
  FBranchFont.Style := [fsBold];

  Font.Style := [];

  FEditInterval := nil;

  if not (csDesigning in ComponentState) then
  begin
    FTextEdit := TEdit.Create(Owner);
    (Owner as TWinControl).InsertControl(FTextEdit);
    FTextEdit.BorderStyle := bsNone;
    FTextEdit.Visible := False;
    FTextEdit.OnKeyPress := @OnEditKeyPress;
    FTextEdit.OnExit := @OnEditExit;

    FDurationEdit := TSpinEdit.Create(Owner);
    (Owner as TWinControl).InsertControl(FDurationEdit);
    FDurationEdit.Visible := False;
    FDurationEdit.OnExit := @OnEditExit;

    FDateEdit := TDateEdit.Create(Owner);
    (Owner as TWinControl).InsertControl(FDateEdit);
    FDateEdit.Visible := False;
    FDateEdit.OnExit := @OnEditExit;

    FComboEdit := TComboBox.Create(Owner);
    (Owner as TWinControl).InsertControl(FComboEdit);
    FComboEdit.Visible := False;
    FComboEdit.OnExit := @OnEditExit;

    FUpDown := TUpDown.Create(Owner);
    (Owner as TWinControl).InsertControl(FUpDown);
    FUpDown.Visible := False;
    FUpDown.OnExit := @OnEditExit;
    FUpDown.OnClick := @OnUpDownButtonClick;
    FMonthCalendar := TCalendar.Create(Owner);
    (Owner as TWinControl).InsertControl(FMonthCalendar);
    FMonthCalendar.Visible := False;
    FMonthCalendar.AutoSize := True;
    FMonthCalendar.OnExit := @OnEditExit;
    FMonthCalendar.BorderWidth := 1;
    //FMonthCalendar.CalColors.BackColor := clBlack;
    FMonthCalendar.DisplaySettings:=[dsShowWeekNumbers,dsShowHeadings];
    FMonthCalendar.OnChange := @OnMonthCalendarClick;
    FDownButton := TSpeedButton.Create(Self);
    FDownButton.GroupIndex := 1;
    FDownButton.AllowAllUp := True;
    InsertControl(FDownButton);
    FDownButton.Visible := False;
    //FDownButton.Glyph.Handle := LoadBitmap(0, MAKEINTRESOURCE(OBM_COMBO));
    FDownButton.OnClick := @OnDownButtonClick;
  end else begin
    FTextEdit := nil;
    FDurationEdit := nil;
    FDateEdit := nil;
    FComboEdit := nil;
    FUpDown := nil;
    FMonthCalendar := nil;
    FDownButton := nil;
  end;
end;

destructor TGanttTree.Destroy;
begin
  FBranchFont.Free;

  inherited Destroy;
end;

{
  **************************
  ***   Protected Part   ***
  **************************
}
procedure TGanttTree.CreateWnd;
begin
  inherited CreateWnd;
  if not (csDesigning in ComponentState) then
    UpdateCommonSettings;
end;

procedure TGanttTree.DrawCell(ACol, ARow: Longint; ARect: TRect;
  AState: TGridDrawState);
var
  CurrInterval: TInterval;
  DeltaX: Integer;
  Stamp: TTimeStamp;
  S: String;
  aColor: TColor;
begin
  inherited DrawCell(ACol, ARow, ARect, AState);
  if csDesigning in ComponentState then Exit;
  with Canvas do
  begin
    if gdFixed in AState then
    begin
      Brush.Color := FixedColor;
      Canvas.Font := Self.Font;

      WriteText(Canvas, ARect, 2, (ARect.Bottom - ARect.Top - TextHeight('A')) div 2,
        Cells[ACol, ARow], taCenter, True);

    end else begin
      aColor := Color;
      if (AltColorStartNormal and Odd(ARow-FixedRows)) {(1)} or
         (not AltColorStartNormal and Odd(ARow)) {(2)} then
        AColor := AlternateColor;
      CurrInterval := TInterval(Objects[0, ARow]);

      if (CurrInterval <> nil) and CurrInterval.IsCollection then
        Canvas.Font := FBranchFont
      else
        Canvas.Font := Self.Font;

      if (gdSelected in AState) then
        begin
          aColor := SelectedColor;
          Canvas.Font.Color:=clHighlightText;
        end;
      Brush.Color := aColor;

      if GetColumnType(ACol) = gctTask then
      begin

        if CurrInterval <> nil then
        begin
          DeltaX := (CurrInterval.Level - 1) * FIndent + 14;
          if CurrInterval.Color<>clBlue then
            Canvas.Font.Color:=CurrInterval.Color;
          Canvas.Font.Style:=CurrInterval.Styles;
          WriteText
          (
            Canvas,
            ARect,
            DeltaX + 2,
            (ARect.Bottom - ARect.Top - TextHeight('A')) div 2,
            CurrInterval.Task,
            taLeftJustify,
            True
          );

          if CurrInterval.IsCollection then
          begin
            if CurrInterval.Opened then
              DrawMinus
              (
                ARect.Left + DeltaX - 9,
                ARect.Top + (ARect.Bottom - ARect.Top) div 2 - 5
              )
            else
              DrawPlus
              (
                ARect.Left + DeltaX - 9,
                ARect.Top + (ARect.Bottom - ARect.Top) div 2 - 5
              );
          end;
        end else // ???? ??? ??? - ?????? ?????? ???????
          FillRect(ARect);
      end else if
        (GetColumnType(ACol) = gctDuration)
          and
        (CurrInterval <> nil)
      then begin
        Stamp := CurrInterval.StampDuration;
        S := IntToStr(Stamp.Date);

        S := S + strDay;

        WriteText
        (
          Canvas,
          ARect,
          2,
          (ARect.Bottom - ARect.Top - TextHeight('A')) div 2,
          S,
          taLeftJustify,
          True
        );
      end else if
        (GetColumnType(ACol) = gctStart)
          and
        (CurrInterval <> nil)
      then begin
        WriteText
        (
          Canvas,
          ARect,
          2,
          (ARect.Bottom - ARect.Top - TextHeight('A')) div 2,
          DateToStr(CurrInterval.StartDate),
          taLeftJustify,
          True
        );
      end else if
        (GetColumnType(ACol) = gctFinish)
          and
        (CurrInterval <> nil)
      then begin
        WriteText
        (
          Canvas,
          ARect,
          2,
          (ARect.Bottom - ARect.Top - TextHeight('A')) div 2,
          DateToStr(CurrInterval.FinishDate),
          taLeftJustify,
          True
        );
      end else
        FillRect(ARect);

      //////////////////////////////////////////
      // ?????? ??????????, ???? ?????? ????????

      DrawCellGrid(aCol,aRow,aRect,aState);

      if gdSelected in AState then
      begin
        Brush.Color := clBlack;
        FrameRect(ARect);
        FrameRect(Rect(ARect.Left + 1, ARect.Top + 1, ARect.Right - 1, ARect.Bottom - 1));
      end;
    end;
  end;
end;

{
  ??????? ?????? ????. ?????????? ???????? ??? ??????? ????????.
}

procedure TGanttTree.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  CurrInterval: TInterval;
  R: TRect;
  ACol, ARow: Integer;
begin
  if csDesigning in ComponentState then Exit;

  MouseToCell(X, Y, ACol, ARow);
  R := CellRect(ACol, ARow);
  CurrInterval := TInterval(Objects[0, ARow]);

  if
    (Button = mbLeft)
      and
    (GetColumnType(ACol) = gctTask)
      and
    (CurrInterval <> nil)
  then begin
    R.Left := R.Left + (CurrInterval.Level - 1) * FIndent + 5;
    R.Right := R.Left + 9;
    R.Top := R.Top + (R.Bottom - R.Top) div 2 - 5;
    R.Bottom := R.Top + 9;

    if IsInRect(X, Y, R) then
    begin
      CurrInterval.Opened := not CurrInterval.Opened;
      FGantt.UpdateInterval;
    end else begin

      if (ACol = Col) and (ARow = Row) and FTaskEditable then
      begin
        ShowTaskEditor;
      end else
        inherited MouseDown(Button, Shift, X, Y);
    end;
  end else
    inherited MouseDown(Button, Shift, X, Y);
end;

function TGanttTree.SelectCell(ACol, ARow: Longint): Boolean;
begin
  Result := inherited SelectCell(ACol, ARow);

  if not (csDesigning in ComponentState) then
  begin
    if Result then
      UpdateCurrentControl(ACol, ARow)
    else
      UpdateCurrentControl(Col, Row);
  end;    
end;

procedure TGanttTree.TopLeftChanged;
var
  I: Integer;
begin
  inherited TopLeftChanged;

  if not (csDesigning in ComponentState) then
    UpdateCurrentControl(Col, Row);
  if not Assigned(FGantt) then exit;
  FGantt.Calendar.Invalidate;
end;

procedure TGanttTree.ColWidthsChanged;
begin
  inherited ColWidthsChanged;

  if not (csDesigning in ComponentState) then
  begin
    if Parent <> nil then
      UpdateCurrentControl(Col, Row);
  end;    
end;

procedure TGanttTree.DoExit;
begin
  if not (csDesigning in ComponentState) then
  begin
    FMonthCalendar.Visible := False;
    FDownButton.Down := False;
  end;  

  inherited DoExit;
end;

procedure TGanttTree.UpdateCommonSettings;
var
  I: Integer;
begin
  DefaultRowHeight := FGantt.Calendar.PixelsPerLine;

  RowHeights[0] :=
    FGantt.Calendar.MajorScaleHeight
      +
    FGantt.Calendar.MajorScaleHeight+1;

  Cells[0, 0] := strNum;
  ColWidths[0] := 28;
  Cells[1, 0] := strInfo;
  ColWidths[1] := 37;
  Cells[2, 0] := strTask;
  ColWidths[2] := 115;
  Cells[3, 0] := strTime;
  ColWidths[3] := 55;
  Cells[4, 0] := strStart;
  ColWidths[4] := 73;
  Cells[5, 0] := strFinish;
  ColWidths[5] := 73;
  Cells[6, 0] := strTies;
  ColWidths[6] := 85;
  Cells[7, 0] := strResourses;
  ColWidths[7] := 85;

  for I := 1 to RowCount - 1 do
    Cells[0, I] := IntToStr(I);

  UpdateTreeList;
  if Assigned(FAfterUpdateCommonSettings) then
    FAfterUpdateCommonSettings(Self);
end;

procedure TGanttTree.UpdateTreeList;
var
  List: TList;
  I: Integer;
begin
  List := TList.Create;

  try
    FGantt.MakeIntervalList(List);

    RowCount := List.Count + 1;

    for I := 0 to List.Count - 1 do
      Objects[0, I + 1] := TObject(List[I]);

    for I := List.Count + 1 to RowCount - 1 do
      Objects[0, I] := nil;
  finally
    List.Free;
  end;
end;

{
  ************************
  ***   Private Part   ***
  ************************
}

function TGanttTree.GetColumnType(AnIndex: Integer): TGanttColumnType;
begin
  case AnIndex of
    1: Result := gctInfo;
    2: Result := gctTask;
    3: Result := gctDuration;
    4: Result := gctStart;
    5: Result := gctFinish;
    6: Result := gctConnection;
    7: Result := gctResources;
  else
    Result := gctNone;
  end;
end;

procedure TGanttTree.SetAfterUpdateCommonSettings(AValue: TNotifyEvent);
begin
  if FAfterUpdateCommonSettings=AValue then Exit;
  FAfterUpdateCommonSettings:=AValue;
end;

procedure TGanttTree.SetIndent(const Value: Integer);
begin
  FIndent := Value;
  Repaint;
end;

procedure TGanttTree.DrawMinus(X, Y: Integer);
var
  OldBrushColor, OldPenColor: TColor;
begin
  with Canvas do
  begin
    OldBrushColor := Brush.Color;
    OldPenColor := Pen.Color;

    Pen.Style := psSolid;
    Pen.Color := clGray;
    Brush.Color := clGray;
    FrameRect(Rect(X, Y, X + 9, Y + 9));

    Pen.Color := clBlack;
    MoveTo(X + 2, Y + 4);
    LineTo(X + 7, Y + 4);

    Brush.Color := OldBrushColor;
    Pen.Color := OldPenColor;
  end;
end;

procedure TGanttTree.DrawPlus(X, Y: Integer);
var
  OldBrushColor, OldPenColor: TColor;
begin
  with Canvas do
  begin
    OldBrushColor := Brush.Color;
    OldPenColor := Pen.Color;

    Pen.Color := clGray;
    Brush.Color := clGray;

    FrameRect(Rect(X, Y, X + 9, Y + 9));

    Pen.Color := clBlack;
    MoveTo(X + 2, Y + 4);
    LineTo(X + 7, Y + 4);

    MoveTo(X + 4, Y + 2);
    LineTo(X + 4, Y + 7);

    Brush.Color := OldBrushColor;
    Pen.Color := OldPenColor;
  end;
end;

function TGanttTree.GetBrachFont: TFont;
begin
  Result := FBranchFont;
end;

procedure TGanttTree.SetBranchFont(const Value: TFont);
begin
  FBranchFont.Assign(Value);
end;

procedure TGanttTree.OnEditKeyPress(Sender: TObject; var Key: Char);
var
  R: TRect;
  CurrInterval: TInterval;
begin
  R := CellRect(Col, Row);
  CurrInterval := TInterval(Objects[0, Row]);

  case Ord(Key) of
    VK_ESCAPE:
    begin
      FTextEdit.Visible := False;
      Key := #0;
      SetFocus;
    end;
    VK_RETURN:
    begin
      CurrInterval.Task := FTextEdit.Text;
      FTextEdit.Visible := False;
      Key := #0;
      SetFocus;
    end;
  end;
end;

procedure TGanttTree.OnEditExit(Sender: TObject);
begin
  if Sender = FTextEdit then
  begin
    if Assigned(FEditInterval) then
      FEditInterval.Change;
    if FTextEdit.Visible and Assigned(FEditInterval) then
    begin
      FEditInterval.Task := FTextEdit.Text;
      FEditInterval := nil;
      FTextEdit.Visible := False;
    end;
  end;
end;

procedure TGanttTree.OnDownButtonClick(Sender: TObject);
var
  R: TRect;
  CurrInterval: TInterval;
begin
  SetFocus;
  R := CellRect(Col, Row);
  CurrInterval := TInterval(Objects[0, Row]);
  if FDownButton.Down then
  begin
    if GetColumnType(Col) = gctStart then
    begin
      //FMonthCalendar.MinDate := EncodeDate(1900, 01, 01);
      //FMonthCalendar.MaxDate := CurrInterval.FinishDate;
      FMonthCalendar.DateTime := CurrInterval.StartDate;
    end else begin
      //FMonthCalendar.MinDate := CurrInterval.StartDate;
      //FMonthCalendar.MaxDate := EncodeDate(9000, 01, 01);
      FMonthCalendar.DateTime := CurrInterval.FinishDate;
    end;

    FMonthCalendar.SendToBack;
    FMonthCalendar.Visible := True;
    FMonthCalendar.Left := R.Right - FMonthCalendar.Width;
    FMonthCalendar.Top := R.Bottom;
    FMonthCalendar.BringToFront;
  end else
    FMonthCalendar.Visible := False;
end;

procedure TGanttTree.OnMonthCalendarClick(Sender: TObject);
var
  CurrInterval: TInterval;
begin
  CurrInterval := TInterval(Objects[0, Row]);

  if CurrInterval <> nil then
  begin
    if GetColumnType(Col) = gctStart then
    begin
      CurrInterval.StartDate := FMonthCalendar.DateTime;
    end else begin
      CurrInterval.FinishDate := FMonthCalendar.DateTime;
    end;

    FGantt.UpdateInterval;
  end;

  FMonthCalendar.Visible := False;
  FDownButton.Down := False;
end;

procedure TGanttTree.OnUpDownButtonClick(Sender: TObject; Button: TUDBtnType);
var
  CurrInterval: TInterval;
begin
  CurrInterval := TInterval(Objects[0, Row]);

  if CurrInterval <> nil then
  begin
    if Button = btNext then
    begin
      CurrInterval.Duration := CurrInterval.Duration + 1;
    end else begin
      if CurrInterval.StampDuration.Date > 0 then
        CurrInterval.Duration := CurrInterval.Duration - 1;
    end;

    FGantt.UpdateInterval;
  end;
end;

procedure TGanttTree.UpdateCurrentControl(ACol, ARow: Integer);
var
  R: TRect;
  CurrInterval: TInterval;

  procedure HideAllControls;
  begin
    FTextEdit.Hide;
    FDurationEdit.Hide;
    FDateEdit.Hide;
    FComboEdit.Hide;
    FUpDown.Hide;
    FMonthCalendar.Hide;
    FDownButton.Hide;
    FDownButton.Down := False;
    //if CanFocus  then SetFocus;
  end;

begin
  R := CellRect(ACol, ARow);
  CurrInterval := TInterval(Objects[0, ARow]);
  if not Assigned(FTextEdit) then exit;

  if FTextEdit.Visible and Assigned(FEditInterval) then
    FEditInterval.Task := FTextEdit.Text;

  FEditInterval := nil;
  HideAllControls;

  case GetColumnType(ACol) of
    gctDuration:
    begin
      FUpDown.Min := 0;
      FUpDown.Max := 32767;

      if CurrInterval <> nil then
      begin
        FUpDown.Position := CurrInterval.StampDuration.Date;
        FUpDown.Enabled := not CurrInterval.IsCollection;
      end else begin
        FUpDown.Position := 0;
        FUpDown.Enabled := False;
      end;

      FUpDown.Height := R.Bottom - R.Top - 4;
      FUpDown.Width := 16;
      FUpDown.Left := R.Right - 16 - 2;
      FUpDown.Top := R.Top + 2;
      FUpDown.Visible := True;
      FUpDown.BringToFront;

      FEditInterval := CurrInterval;
    end;
    gctStart, gctFinish:
    begin
      if CurrInterval <> nil then
      begin
        FDownButton.Enabled := not CurrInterval.IsCollection;
        FEditInterval := CurrInterval;
      end else begin
        FDownButton.Enabled := False;
        FEditInterval := nil;
      end;

      FDownButton.Height := R.Bottom - R.Top - 4;
      FDownButton.Width := 16;
      FDownButton.Left := R.Right - 16 - 2;
      FDownButton.Top := R.Top + 2;
      FDownButton.Visible := True;
      FDownButton.BringToFront;
    end;
    else begin
      HideAllControls;
      FEditInterval := nil;
    end;
  end;
end;

procedure TGanttTree.ShowTaskEditor;
var
  R: TRect;
  CurrInterval: TInterval;
begin
  R := CellRect(Col, Row);
  CurrInterval := TInterval(Objects[0, Row]);

  if CurrInterval <> nil then
  begin
    FTextEdit.SendToBack;
    FTextEdit.Visible := True;
    FTextEdit.AutoSize := False;
    FTextEdit.Left := R.Left + 2;
    FTextEdit.Top := R.Top + 2;
    FTextEdit.Width := R.Right - R.Left - 4;
    FTextEdit.Height := R.Bottom - R.Top - 4;
    FTextEdit.Text := CurrInterval.Task;
    FTextEdit.SelectAll;
    FTextEdit.BringToFront;
    FTextEdit.SetFocus;
    FEditInterval := CurrInterval;
  end;  
end;

procedure TGanttTree.WMChar(var Message: TLMChar);
begin

  if GetColumnType(Col) = gctTask then
  begin
    if Message.CharCode = VK_ESCAPE then
    begin
      FTextEdit.Visible := False;
      SetFocus;
    end else if (not FTextEdit.Visible) and FTaskEditable then
    begin
      ShowTaskEditor;
    end;

    SendMessage(FTextEdit.HANDLE, LM_CHAR, Message.CharCode, Message.KeyData);
  end //else
    //inherited ;
end;

{
  --------------------------------------
  ------                          ------
  ------      TgsGantt Class      ------
  ------                          ------
  --------------------------------------
}

{
  ***********************
  ***   Public Part   ***
  ***********************
}

constructor TgsGantt.Create(AnOwner: TComponent);
begin
  inherited Create(AnOwner);

  Width := 300;
  Height := 150;

  FIntervals := TList.Create;

  FTree := TGanttTree.Create(Self);
  InsertControl(FTree);
  FTree.Align := alLeft;
  FTree.Width := 200;

  FSplitter := TSplitter.Create(Self);
  InsertControl(FSplitter);
  FSplitter.Left := FTree.Left + FTree.Width + 1;
  FSplitter.Align := alLeft;

  FCalendar := TGanttCalendar.Create(Self);
  InsertControl(FCalendar);
  FCalendar.Align := alClient;
end;

destructor TgsGantt.Destroy;
var
  i: Integer;
begin
  FCalendar.Free;
  FTree.Free;
  FSplitter.Free;
  try
    for i := 0 to FIntervals.Count-1 do
      TInterval(FIntervals.Items[i]).Free;
  except
  end;
  FreeAndNil(FIntervals);

  inherited Destroy;
end;

procedure TgsGantt.AddInterval(AnInterval: TInterval);
begin
  FIntervals.Add(AnInterval);
  UpdateInterval;
end;

procedure TgsGantt.InsertInterval(AnIndex: Integer; AnInterval: TInterval);
begin
  FIntervals.Insert(AnIndex, AnInterval);
  UpdateInterval;
end;

procedure TgsGantt.DeleteInterval(AnIndex: Integer);
begin
  FIntervals.Delete(AnIndex);
  UpdateInterval;
end;

procedure TgsGantt.RemoveInterval(AnInterval: TInterval);
begin
  FIntervals.Remove(AnInterval);
  UpdateInterval;
end;

{DONE 3 -oDenis -cMainUpdate: ??????????}
procedure TgsGantt.UpdateInterval;
begin
  if IsUpdating then exit;
  FTree.UpdateTreeList;
  FTree.Repaint;
  FCalendar.Repaint;
end;

procedure TgsGantt.BeginUpdate;
begin
  inc(FUpdateCount);
end;

function TgsGantt.IsUpdating: Boolean;
begin
  Result := FUpdateCount>0;
end;

procedure TgsGantt.EndUpdate;
begin
  dec(FUpdateCount);
  if FUpdateCount <=0 then
    UpdateInterval;
end;


{
  **************************
  ***   Protected Part   ***
  **************************
}

procedure TgsGantt.MakeIntervalList(AList: TList);
var
  I: Integer;
begin
  for I := 0 to IntervalCount - 1 do
  begin
    if Interval[I].Visible then
    begin
      AList.Add(Interval[I]);
      Interval[I].MakeIntervalList(AList);
    end;
  end;
end;

procedure TgsGantt.MakeIntervalListWithDeps(AList: TList);
var
  I: Integer;
  FirstVisible: Boolean;
begin
  FirstVisible := False;
  for I := 0 to IntervalCount - 1 do
  begin
    if Interval[I].Visible or (not FirstVisible) then
    begin
      FirstVisible:=FirstVisible or Interval[I].Visible;
      AList.Add(Interval[I]);
      Interval[I].MakeIntervalList(AList);
    end;
  end;
end;


{
  ************************
  ***   Private Part   ***
  ************************
}

function TgsGantt.GetIntervalCount: Integer;
begin
  Result := 0;
  if Assigned(FIntervals) then
    Result := FIntervals.Count;
end;

function TgsGantt.GetInterval(AnIndex: Integer): TInterval;
begin
  Result := TInterval(FIntervals[AnIndex]);
end;

function TgsGantt.GetCalendarFont: TFont;
begin
  Result := FCalendar.Font;
end;

procedure TgsGantt.SetCalendarFont(const Value: TFont);
begin
  FCalendar.Font := Value;
end;

function TgsGantt.GetCalendarColor: TColor;
begin
  Result := FCalendar.Color;
end;

procedure TgsGantt.SetCalendarColor(const Value: TColor);
begin
  FCalendar.Color := Value;
end;

function TgsGantt.GetMajorColor: TColor;
begin
  Result := FCalendar.MajorColor;
end;

procedure TgsGantt.SetMajorColor(const Value: TColor);
begin
  FCalendar.MajorColor := Value;
end;

function TgsGantt.GetMinorColor: TColor;
begin
  Result := FCalendar.MinorColor;
end;

procedure TgsGantt.SetMinorColor(const Value: TColor);
begin
  FCalendar.MinorColor := Value;
end;

function TgsGantt.GetMajorScale: TTimeScale;
begin
  Result := FCalendar.MajorScale;
end;

procedure TgsGantt.SetMajorScale(const Value: TTimeScale);
begin
  FCalendar.MajorScale := Value;
end;

function TgsGantt.GetMinorScale: TTimeScale;
begin
  Result := FCalendar.MinorScale;
end;

procedure TgsGantt.SetMinorScale(const Value: TTimeScale);
begin
  FCalendar.MinorScale := Value;
end;

function TgsGantt.GetPixelsPerMinorScale: Integer;
begin
  Result := FCalendar.PixelsPerMinorScale;
end;

procedure TgsGantt.SetPixelsPerMinorScale(const Value: Integer);
begin
  FCalendar.PixelsPerMinorScale := Value;
end;

function TgsGantt.GetPixelsPerLine: Integer;
begin
  Result := FCalendar.PixelsPerLine;
end;

procedure TgsGantt.SetPixelsPerLine(const Value: Integer);
begin
  FCalendar.PixelsPerLine := Value;
end;

function TgsGantt.GetStartDate: TDateTime;
begin
  Result := FCalendar.StartDate;
end;

procedure TgsGantt.SetStartDate(const Value: TDateTime);
begin
  FCalendar.StartDate := Value;
  FCalendar.FHorzScrollBar.Position:=trunc(StartDate-Value);
end;

function TgsGantt.GetTreeIndent: Integer;
begin
  Result := FTree.Indent;
end;

procedure TgsGantt.SetTreeIndent(const Value: Integer);
begin
  FTree.Indent := Value;
end;

function TgsGantt.GetTreeFont: TFont;
begin
  Result := FTree.Font;
end;

procedure TgsGantt.SetTreeFont(const Value: TFont);
begin
  FTree.Font := Value;
end;

function TgsGantt.GetTreeBranchFont: TFont;
begin
  Result := FTree.BranchFont;
end;

procedure TgsGantt.SetTreeBranchFont(const Value: TFont);
begin
  FTree.BranchFont := Value;
end;

{
   -----------------------------------------------------------
   ------                                               ------
   ------      Register TGanttCalendar as component      ------
   ------                                               ------
   -----------------------------------------------------------
}

{procedure Register;
begin
  RegisterComponents('gsNV', [TgsGantt]);
end;}


initialization
  Screen.Cursors[crGanttMiddle] := LoadCursor(hInstance, 'GANTT_MIDDLE');
  Screen.Cursors[crGanttRightMove] := LoadCursor(hInstance, 'GANTT_RIGHTMOVE');
  Screen.Cursors[crGanttLeftMove] := LoadCursor(hInstance, 'GANTT_LEFTMOVE');
  Screen.Cursors[crGanttPercent] := LoadCursor(hInstance, 'GANTT_PERCENT');
  Screen.Cursors[crGanttConnect] := LoadCursor(hInstance, 'GANTT_CONNECT');

  DrawBitmap := TBitmap.Create;

finalization

  DrawBitmap.Free;

end.

