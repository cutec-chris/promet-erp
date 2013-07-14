{
   TDatePicker control for Lazarus
  - - - - - - - - - - - - - - - - -
Author: Zoran Vučenović, January 2010
   Зоран Вученовић, јануара 2010.

   Delphi's Visual Component Library (VCL) has a control named TDateTimePicker,
which I find very useful for editing dates. Lazarus Component Library (LCL),
however, does not have this control, because VCL wraps native Windows control,
and it seems that such control does not exist on other platforms. Given that
LCL is designed to be platform independent, it could not use native Win control.
   Instead, for editing dates LCL has a control named TDateEdit, but I prefer
the VCL's TDateTimePicker.
   Therefore, I tried to create a custom control which would resemble VCL's
TDateTimePicker as much as possible, but not to rely on native Windows control.

   The TDatePicker control does not use native Win control. It has been written
and initially tested on Windows XP with win widgetset, but then tested and
adjusted on Ubuntu Linux 9.10 with gtk2 widgetset.
   Thanks to Željko Rikalo (http://wiki.lazarus.freepascal.org/User:Zeljan),
its tested on Qt widgetset and adjusted to be fully functional to Qt users.

   Unlike VCL's TDateTimePicker, this control lacks feature of time editing. It
can be used for date editing only. That's why it is named TDatePicker, not
TDateTimePicker.

-----------------------------------------------------------
LICENCE
- - - -
   Modified LGPL -- see COPYING.TXT.

-----------------------------------------------------------
NO WARRANTY
- - - - - -
   There is no warranty whatsoever.

-----------------------------------------------------------
BEST REGARDS TO LAZARUS COMMUNITY!
- - - - - - - - - - - - - - - - - -
   I do hope this control will be useful.
}
unit DatePicker;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLProc, Controls, LCLType, Graphics, Math, StdCtrls,
  Buttons, ExtCtrls, Forms, Calendar;

const
  NullDate = TDateTime(Math.NaN); // We will deal with this value the special
// way. It will be especially useful for dealing with null values from database.

  TheBiggestDate = TDateTime(2958465.0); // 31. dec. 9999.
//{$IFDEF WINDOWS}
// TCalendar does not accept smaller dates then 14. sep. 1752 on Windows
// platform (see TCustomCalendar.SetDateTime).
// In Delphi help it is documented that Windows controls act weird with dates
// older than 24. sep. 1752. Actually, TCalendar control has problems to show
// dates before 1. okt. 1752. (try putting one calendar on the form, run the
// application and see what september 1752. looks like). So, this will be the
// down limit:
  TheSmallestDate = TDateTime(-53780.0); // 1. okt. 1752.
//{$ELSE} -- I just commented this out. Let's behave uniformely as much as
// possible -- I won't allow dates before 1. okt. 1752. on any platform (who
// cares about those).
//  TheSmallestDate = TDateTime(-693593.0); // 1. jan. 0001.
//{$ENDIF}

type
  TYMD = record
    Year, Month, Day: Word;
  end;

  TDateDisplayOrder = (ddoDMY, ddoMDY, ddoYMD, ddoTryDefault);

  TDateTextPart = (dtpDay, dtpMonth, dtpYear);

  { TCustomDateEditor }

  TCustomDateEditor = class(TCustomControl)
  private
    FCenturyFrom, FEffectiveCenturyFrom: Word;
    FDateDisplayOrder: TDateDisplayOrder;
    FLeadingZeros: Boolean;
    FNullInputAllowed: Boolean;
    FOnConfirmChanges: TNotifyEvent;
    FTheDate: TDateTime;
    FConfirmedDate: TDateTime;
    FTheDateSeparator: UTF8String;
    FReadOnly: Boolean;
    FMaxDate, FMinDate: TDateTime;
    FTextForNullDate: UTF8String;
    FTrailingSeparator: Boolean;
    FUseDefaultDateSeparator: Boolean;
    FUserChangedText: Boolean;
    FTextPart: array[1..3] of UTF8String;
    FOnChange: TNotifyEvent;
    FStoredLockCount: Integer;
    FTwoDigitsWidth: Integer;
    FTextHeight: Integer;
    FSeparatorWidth: Integer;
    FSepNoSpaceWidth: Integer;
    FSelectedTextPart: 1..3;
    FRecalculatingTextSizesNeeded: Boolean;
    FJumpMinMax: Boolean;

    procedure RecalculateTextSizesIfNeeded;
    function GetDay: Word;
    function GetMonth: Word;
    function GetYear: Word;
    function GetYYYYMMDD(const TodayIfNull: Boolean = False): TYMD;
    procedure SetCenturyFrom(const AValue: Word);
    procedure SetLeadingZeros(const AValue: Boolean);
    procedure SetTheDate(const AValue: TDateTime);
    procedure SetDateDisplayOrder(const AValue: TDateDisplayOrder);
    procedure SetTheDateSeparator(const AValue: UTF8String);
    procedure SetDay(const AValue: Word);
    procedure SetMaxDate(const AValue: TDateTime);
    procedure SetMinDate(const AValue: TDateTime);
    procedure SetMonth(const AValue: Word);
    procedure SetTextForNullDate(const AValue: UTF8String);
    procedure SetTrailingSeparator(const AValue: Boolean);
    procedure SetUseDefaultDateSeparator(const AValue: Boolean);
    procedure SetYear(const AValue: Word);
    procedure SetReadOnly(Value: Boolean);
    procedure SetYYYYMMDD(const AValue: TYMD; const DoChangeIfChanged: Boolean = False);
    procedure UpdateIfUserChangedText;
    function GetSelectedText: UTF8String;
    procedure AdjustEffectiveCenturyFrom;
    procedure SelectDateTextPart(const DateTextPart: TDateTextPart);
  protected
    procedure ConfirmChanges;
    procedure UndoChanges;
    procedure SetTheDateJumpMinMax(const AValue: TDateTime);
    procedure ChangeDateInternally(const AValue: TDateTime);
    function GetEffectiveDateDisplayOrder: TDateDisplayOrder; virtual;
    function GetCurrentDateTextPart: TDateTextPart;
    procedure FontChanged(Sender: TObject); override;
    function GetTextOrigin: TPoint;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: char); override;
    procedure MouseDown(Button: TMouseButton; Shift:TShiftState; X,Y:Integer); override;
    procedure UpdateDate; virtual;
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure SetEnabled(Value: Boolean); override;
    procedure CalculatePreferredSize(var PreferredWidth,
                  PreferredHeight: integer; WithThemeSpace: Boolean); override;
    procedure IncreaseMonth;
    procedure IncreaseYear;
    procedure IncreaseDay;
    procedure DecreaseMonth;
    procedure DecreaseYear;
    procedure DecreaseDay;
    procedure Change; virtual;

    property ReadOnly: Boolean read FReadOnly write SetReadOnly default False;
    property AutoSize default True;
    property TabStop default True;
    property BorderStyle default bsSingle;

    property DateDisplayOrder: TDateDisplayOrder
             read FDateDisplayOrder write SetDateDisplayOrder default ddoTryDefault;
    property MaxDate: TDateTime read FMaxDate write SetMaxDate;
    property MinDate: TDateTime read FMinDate write SetMinDate;
    property TheDateSeparator: UTF8String
             read FTheDateSeparator write SetTheDateSeparator;
    property UseDefaultDateSeparator: Boolean
             read FUseDefaultDateSeparator write SetUseDefaultDateSeparator;
    property TrailingSeparator: Boolean
             read FTrailingSeparator write SetTrailingSeparator default False;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnConfirmChanges: TNotifyEvent
             read FOnConfirmChanges write FOnConfirmChanges;
    property TextForNullDate: UTF8String
             read FTextForNullDate write SetTextForNullDate;
    property LeadingZeros: Boolean
             read FLeadingZeros write SetLeadingZeros default True;
    property NullInputAllowed: Boolean
             read FNullInputAllowed write FNullInputAllowed default True;
  public
    constructor Create(AOwner: TComponent); override;

    function IsNullDate: Boolean;
    procedure SelectDay;
    procedure SelectMonth;
    procedure SelectYear;
    procedure Paint; override;
    procedure EditingDone; override;

    property TheDate: TDateTime read FTheDate write SetTheDate;
    property CenturyFrom: Word read FCenturyFrom write SetCenturyFrom default 1941;
    property Day: Word read GetDay write SetDay stored False;
    property Month: Word read GetMonth write SetMonth stored False;
    property Year: Word read GetYear write SetYear stored False;
  published
    //
  end;

  {TDateEditor}

  TDateEditor = class(TCustomDateEditor)
  published
    property UseDefaultDateSeparator;
    property CenturyFrom;
    property DateDisplayOrder;
    property MaxDate;
    property MinDate;
    property ReadOnly;
    property AutoSize;
    property Font;
    property ParentFont;
    property TabOrder;
    property TabStop;
    property BorderStyle;
    property BorderSpacing;
    property Enabled;
    property Color;
    property TheDateSeparator;
    property TrailingSeparator;
    property TextForNullDate;
    property ShowHint;
    property TheDate;
    property Align;
    property Anchors;
    property Constraints;
    property Cursor;
    property PopupMenu;
    property Visible;
    property LeadingZeros;
    property NullInputAllowed;
// events:
    property OnChange;
    property OnChangeBounds;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnEditingDone;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseLeave;
    property OnMouseWheel;
    property OnResize;
    property OnUTF8KeyPress;
  end;

  TArrowShape = (asClassicSmaller, asClassicLarger, asModernSmaller,
                                               asModernLarger, asYetAnotherShape);

  { TCustomDatePicker }

  TCustomDatePicker = class(TCustomControl)
  private
    FArrowShape: TArrowShape;
    FDateEditor: TCustomDateEditor;
    FCheckBox: TCheckBox;
    FOnChange: TNotifyEvent;
    FOnDropDown: TNotifyEvent;
    FOnCloseUp: TNotifyEvent;

    FArrowButton: TSpeedButton;
    FCalendarForm: TCustomForm;
    FCal: TCalendar;
    FShape: TShape;
    FDoNotArrangeControls: Boolean;
    FClosingCalendarForm: Boolean;
    FRememberedCalendarFormOrigin: TPoint;
    FCallFromDateEditorEnter: Boolean;
    FCallFromDateEditorExit: Boolean;
    FCallFromDateEditorEditingDone: Boolean;
    FCloseCalendarOnChange: Boolean;

    function GetCenturyFrom: Word;
    function GetChecked: Boolean;
    function GetDateDisplayOrder: TDateDisplayOrder;
    function GetLeadingZeros: Boolean;
    function GetNullInputAllowed: Boolean;
    function GetTheDate: TDateTime;
    function GetTheDateSeparator: UTF8String;
    function GetMaxDate: TDateTime;
    function GetMinDate: TDateTime;
    function GetReadOnly: Boolean;
    function GetShowCalendar: Boolean;
    function GetShowCheckBox: Boolean;
    function GetTextForNullDate: UTF8String;
    function GetTrailingSeparator: Boolean;
    function GetUseDefaultDateSeparator: Boolean;
    procedure SetArrowShape(const AValue: TArrowShape);
    procedure SetCenturyFrom(const AValue: Word);
    procedure SetChecked(const AValue: Boolean);
    procedure CheckIfDateEditorIsEnabled;
    procedure SetDateDisplayOrder(const AValue: TDateDisplayOrder);
    procedure SetLeadingZeros(const AValue: Boolean);
    procedure SetNullInputAllowed(const AValue: Boolean);
    procedure SetTheDate(const AValue: TDateTime);
    procedure SetTheDateSeparator(const AValue: UTF8String);
    procedure SetMaxDate(const AValue: TDateTime);
    procedure SetMinDate(const AValue: TDateTime);
    procedure SetReadOnly(const AValue: Boolean);
    procedure SetShowCalendar(const AValue: Boolean);
    procedure SetShowCheckBox(const AValue: Boolean);
    procedure SetTextForNullDate(const AValue: UTF8String);
    procedure SetTrailingSeparator(const AValue: Boolean);
    procedure SetUseDefaultDateSeparator(const AValue: Boolean);
    procedure DestroyTheCalendar;
    procedure AdjustCalendarFormSize;
    procedure AdjustCalendarFormScreenPosition;
    procedure CreateCalendarForm;
    procedure DestroyCalendarForm;
    procedure CloseCalendarForm(AndSetTheDate: Boolean = False);
    procedure DropDownCalendarForm;

    procedure DateEditorChange(Sender: TObject);
    procedure DateEditorEnter(Sender: TObject);
    procedure DateEditorExit(Sender: TObject);
    procedure DateEditorConfirmChanges(Sender: TObject);
    procedure DateEditorEditingDone(Sender: TObject);
    procedure DateEditorKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure DateEditorKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure DateEditorKeyPress(Sender: TObject; var Key: char);
    procedure DateEditorUTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
    procedure DateEditorMouseDown(Sender: TObject; Button: TMouseButton;
                                            Shift: TShiftState; X, Y: Integer);
    procedure DateEditorMouseMove(Sender: TObject; Shift: TShiftState;
                                                           X, Y: Integer);
    procedure DateEditorMouseUp(Sender: TObject; Button: TMouseButton;
                                            Shift: TShiftState; X, Y: Integer);
    procedure DateEditorClick(Sender: TObject);
    procedure DateEditorDblClick(Sender: TObject);
    procedure DateEditorTripleClick(Sender: TObject);
    procedure DateEditorQuadClick(Sender: TObject);

    procedure ArrowMouseDown(Sender: TObject; Button: TMouseButton;
                                            Shift: TShiftState; X, Y: Integer);
    procedure CheckBoxChange(Sender: TObject);

    procedure CalendarKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure CalendarResize(Sender: TObject);
    procedure CalendarMouseUp(Sender: TObject; Button: TMouseButton;
                                            Shift: TShiftState; X, Y: Integer);
    procedure CalendarChange(Sender: TObject);

    procedure CalendarFormDeactivate(Sender: TObject);
    procedure CalendarFormShow(Sender: TObject);
    procedure CalendarFormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure CalendarFormDestroy(Sender: TObject);

  protected
    class function GetControlClassDefaultSize: TPoint; override;
    procedure ConfirmChanges; virtual;
    procedure SetEnabled(Value: Boolean); override;
    procedure CreateWnd; override;
    procedure SetTheDateJumpMinMax(const AValue: TDateTime);
    procedure ArrangeCtrls; virtual;
    procedure Change; virtual;
    procedure DoDropDown; virtual;
    procedure DoCloseUp; virtual;
    procedure DrawArrowButtonGlyph; virtual;
    procedure CalculatePreferredSize(var PreferredWidth,
                  PreferredHeight: integer; WithThemeSpace: Boolean); override;

    procedure DoEnter; override;
    procedure DoExit; override;

    property BorderStyle default bsSingle;
    property AutoSize default True;
    property ParentColor default False;
    property Checked: Boolean read GetChecked write SetChecked;
    property UseDefaultDateSeparator: Boolean
             read GetUseDefaultDateSeparator write SetUseDefaultDateSeparator;
    property CenturyFrom: Word
             read GetCenturyFrom write SetCenturyFrom;
    property DateDisplayOrder: TDateDisplayOrder
             read GetDateDisplayOrder write SetDateDisplayOrder;
    property MaxDate: TDateTime read GetMaxDate write SetMaxDate;
    property MinDate: TDateTime read GetMinDate write SetMinDate;
    property TheDate: TDateTime read GetTheDate write SetTheDate;
    property TheDateSeparator: UTF8String
             read GetTheDateSeparator write SetTheDateSeparator;
    property TrailingSeparator: Boolean
             read GetTrailingSeparator write SetTrailingSeparator;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly;
    property LeadingZeros: Boolean read GetLeadingZeros write SetLeadingZeros;
    property TextForNullDate: UTF8String
             read GetTextForNullDate write SetTextForNullDate;
    property NullInputAllowed: Boolean
             read GetNullInputAllowed write SetNullInputAllowed;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnDropDown: TNotifyEvent read FOnDropDown write FOnDropDown;
    property OnCloseUp: TNotifyEvent read FOnCloseUp write FOnCloseUp;
    property ShowCheckBox: Boolean
             read GetShowCheckBox write SetShowCheckBox default False;
    property ShowCalendar: Boolean
             read GetShowCalendar write SetShowCalendar default True;
    property ArrowShape: TArrowShape
        read FArrowShape write SetArrowShape default asModernSmaller;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function IsNullDate: Boolean;
    procedure EditingDone; override;
  published
    //
  end;

  {TDatePicker}

  TDatePicker = class(TCustomDatePicker)
  published
    property ArrowShape;
    property ShowCalendar;
    property ShowCheckBox;
    property Checked;
    property UseDefaultDateSeparator;
    property CenturyFrom;
    property DateDisplayOrder;
    property MaxDate;
    property MinDate;
    property ReadOnly;
    property AutoSize;
    property Font;
    property ParentFont;
    property TabOrder;
    property TabStop;
    property BorderStyle;
    property BorderSpacing;
    property Enabled;
    property Color;
    property TheDateSeparator;
    property TrailingSeparator;
    property TextForNullDate;
    property LeadingZeros;
    property ShowHint;
    property TheDate;
    property Align;
    property Anchors;
    property Constraints;
    property Cursor;
    property PopupMenu;
    property Visible;
    property NullInputAllowed;
// events:
    property OnChange;
    property OnDropDown;
    property OnCloseUp;
    property OnChangeBounds;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnEditingDone;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseLeave;
    property OnMouseWheel;
    property OnResize;
    property OnUTF8KeyPress;
  end;

implementation

uses LCLVersion;

function NumberOfDaysInMonth(const Month, Year: Word): Word;
begin
  Result := 0;
  if Month in [1..12] then
    //try
      Result := MonthDays[IsLeapYear(Year), Month];
    //except
    //  raise Exception.Create('Invalid month');
    //end;
end;

function Equals(const A, B: Double): Boolean;
begin
//This was bad code -- if one of parameters is NaN and the other is not,
                              //     A = B evaluates to True!
  //Result := (IsNan(A) and IsNan(B))
  //                         or (A = B);

// So, this works:
  if IsNan(A) then
    Result := IsNan(B)
  else
    Result := (not IsNan(B)) and (A = B);
end;

procedure Exchange(var W1, W2: Word);
var
  W: Word;
begin
  W := W1;
  W1 := W2;
  W2 := W;
end;

{ TCustomDateEditor }

procedure TCustomDateEditor.SetTheDate(const AValue: TDateTime);
begin
  if IsInfinite(AValue) then Exit;

  if IsNan(AValue) then
    FTheDate := NullDate
  else
    FTheDate := Int(AValue);

  UpdateDate;
end;

procedure TCustomDateEditor.SetDateDisplayOrder(const AValue: TDateDisplayOrder);
var
  PreviousEffectiveDDO: TDateDisplayOrder;
begin
  if FDateDisplayOrder <> AValue then begin
    PreviousEffectiveDDO := GetEffectiveDateDisplayOrder;
    FDateDisplayOrder := AValue;
    if PreviousEffectiveDDO <> GetEffectiveDateDisplayOrder then
      UpdateDate;
  end;
end;

procedure TCustomDateEditor.SetTheDateSeparator(const AValue: UTF8String);
begin
  FUseDefaultDateSeparator := False;
  if FTheDateSeparator <> AValue then begin
    FTheDateSeparator := AValue;
    FRecalculatingTextSizesNeeded := True;
    Invalidate;
  end;
end;

procedure TCustomDateEditor.RecalculateTextSizesIfNeeded;
var
  C: Char;
  N: Integer;
  S: UTF8String;
begin
  if FRecalculatingTextSizesNeeded then begin
    FRecalculatingTextSizesNeeded := False;

    FSeparatorWidth := Canvas.GetTextWidth(FTheDateSeparator);

    FSepNoSpaceWidth := FSeparatorWidth;
    N := Length(FTheDateSeparator);
    if N > 1 then begin
      while (N > 1) and (FTheDateSeparator[N] in [#0..' ']) do
        Dec(N);

      S := Copy(FTheDateSeparator, 1, N);

      FSepNoSpaceWidth := Canvas.GetTextWidth(S)
    end;

    FTwoDigitsWidth := 0;
    for C := '0' to '9' do begin
      N := Canvas.GetTextWidth(C);
      if N > FTwoDigitsWidth then
        FTwoDigitsWidth := N;
    end;
    FTwoDigitsWidth := 2 * FTwoDigitsWidth;

    FTextHeight := Canvas.GetTextHeight('0123456789' + FTheDateSeparator);
  end;
end;

function TCustomDateEditor.GetDay: Word;
begin
  Result := GetYYYYMMDD.Day;
end;

function TCustomDateEditor.GetMonth: Word;
begin
  Result := GetYYYYMMDD.Month;
end;

function TCustomDateEditor.GetYear: Word;
begin
  Result := GetYYYYMMDD.Year;
end;

function TCustomDateEditor.GetYYYYMMDD(const TodayIfNull: Boolean): TYMD;
begin
  if IsNullDate then begin
    if TodayIfNull then
      DecodeDate(SysUtils.Date, Result.Year, Result.Month, Result.Day)
    else
      with Result do begin
        Day := 0;
        Month := 0;
        Year := 0;
      end;
  end else
    DecodeDate(FTheDate, Result.Year, Result.Month, Result.Day);
end;

procedure TCustomDateEditor.SetCenturyFrom(const AValue: Word);
begin
  if FCenturyFrom = AValue then Exit;

  FCenturyFrom := AValue;
  AdjustEffectiveCenturyFrom;
end;

procedure TCustomDateEditor.SetLeadingZeros(const AValue: Boolean);
begin
  if FLeadingZeros = AValue then Exit;

  FLeadingZeros := AValue;
  UpdateDate;
end;

procedure TCustomDateEditor.SetDay(const AValue: Word);
var
  YMD: TYMD;
begin
  SelectDay;
  YMD := GetYYYYMMDD(True);

  YMD.Day := AValue;
  SetYYYYMMDD(YMD);
end;

procedure TCustomDateEditor.SetMaxDate(const AValue: TDateTime);
begin
  if IsNan(AValue) or
     IsInfinite(AValue) then Exit;

  if AValue > TheBiggestDate then
    FMaxDate := TheBiggestDate
  else if AValue <= FMinDate then
    FMaxDate := FMinDate
  else
    FMaxDate := Int(AValue);

  if not IsNullDate then
    if FMaxDate <= FTheDate then
      SetTheDate(FMaxDate);

  AdjustEffectiveCenturyFrom;
end;

procedure TCustomDateEditor.SetMinDate(const AValue: TDateTime);
begin
  if IsNan(AValue) or
     IsInfinite(AValue) then Exit;

  if AValue < TheSmallestDate then
    FMinDate := TheSmallestDate
  else if AValue >= FMaxDate then
    FMinDate := FMaxDate
  else
    FMinDate := Int(AValue);

  if not IsNullDate then
    if FMinDate >= FTheDate then
      SetTheDate(FMinDate);

  AdjustEffectiveCenturyFrom;
end;

procedure TCustomDateEditor.SetMonth(const AValue: Word);
var
  YMD: TYMD;
  N: Word;
begin
  SelectMonth;
  YMD := GetYYYYMMDD(True);

  YMD.Month := AValue;

  N := NumberOfDaysInMonth(YMD.Month, YMD.Year);
  if YMD.Day > N then
    YMD.Day := N;

  SetYYYYMMDD(YMD);
end;

procedure TCustomDateEditor.SetTextForNullDate(const AValue: UTF8String);
begin
  if FTextForNullDate = AValue then Exit;

  FTextForNullDate := AValue;
  if IsNullDate then
    Invalidate;
end;

procedure TCustomDateEditor.SetTrailingSeparator(const AValue: Boolean);
begin
  if FTrailingSeparator = AValue then Exit;

  FTrailingSeparator := AValue;
  FRecalculatingTextSizesNeeded := True;
  UpdateIfUserChangedText;
  Invalidate;
end;

procedure TCustomDateEditor.SetUseDefaultDateSeparator(const AValue: Boolean);
begin
  if FUseDefaultDateSeparator <> AValue then begin
    if AValue then
      SetTheDateSeparator(SysUtils.DateSeparator); // -- Note that here, in
                              // SetTheDateSeparator procedure, the field
                              // FUseDefaultDateSeparator is set to False.
    // Therefore, the next line must NOT be moved above.
    FUseDefaultDateSeparator := AValue;
  end;
end;

procedure TCustomDateEditor.SetYear(const AValue: Word);
var
  YMD: TYMD;
begin
  SelectYear;

  YMD := GetYYYYMMDD(True);
  YMD.Year := AValue;
  if (YMD.Month = 2) and (YMD.Day > 28) and (not IsLeapYear(YMD.Year)) then
    YMD.Day := 28;

  SetYYYYMMDD(YMD);
end;

procedure TCustomDateEditor.SetYYYYMMDD(const AValue: TYMD;
  const DoChangeIfChanged: Boolean);
var
  D: TDateTime;
begin
  if TryEncodeDate(AValue.Year, AValue.Month, AValue.Day, D) then begin
    SetTheDate(D);
    if DoChangeIfChanged then
      Change;
  end else
    UpdateDate;
end;

{ GetEffectiveDateDisplayOrder function
 ----------------------------------
  If date display order ddoTryDefault is set, then we will decide which
  display order to use according to ShortDateFormat global variable. The
  function tries to achieve that by searching through short date format string,
  to see which letter comes first -- d, m or y. When it finds any of these
  characters, it assumes that date order should be d-m-y, m-d-y, or y-m-d
  respectively. If the search through ShortDateFormat is unsuccessful by any
  chance, we try the same with LongDateFormat global variable. If we don't
  succeed again, we'll assume y-m-d order.  }
function TCustomDateEditor.GetEffectiveDateDisplayOrder: TDateDisplayOrder;
var
  S: String;
  I: Integer;
begin
  if FDateDisplayOrder = ddoTryDefault then begin
    S := ShortDateFormat;
    Result := ddoTryDefault;

    repeat

      for I := 1 to Length(S) do begin
        case upCase(S[I]) of
          'D': begin
                 Result := ddoDMY;
                 Break;
               end;
          'M': begin
                 Result := ddoMDY;
                 Break;
               end;
          'Y': begin
                 Result := ddoYMD;
                 Break;
               end;
        end;
      end;

      if Result = ddoTryDefault then begin
        S := LongDateFormat; { We couldn't decide with ShortDateFormat, let's
                                             try with LongDateFormat now. }
        Result := ddoYMD; { -- But now we must set something to be default. This
                ensures that the repeat loop breaks next time. If we don't find
                anything in LongDateFormat, we'll leave with y-m-d order. }
      end else
        Break;

    until False;

  end else
    Result := FDateDisplayOrder;
end;

procedure TCustomDateEditor.UpdateIfUserChangedText;
var
  W: Word;
  S: UTF8String;
begin
  if FUserChangedText then begin
    Inc(FStoredLockCount);
    try
      FUserChangedText := False;
      S := Trim(GetSelectedText);
      W := StrToInt(S);
      case GetCurrentDateTextPart of
        dtpYear:
          begin
            if Length(S) <= 2 then begin
          // If user entered the year in two digit format (or even only one
          // digit), we will set the year according to the CenturyFrom property
          //  (We actually use FEffectiveCenturyFrom field, which is adjusted to
          //   take care of MinDate and MaxDate besides CenturyFrom properties).
              if W >= (FEffectiveCenturyFrom mod 100) then
                W := W + 100 * (FEffectiveCenturyFrom div 100)
              else
                W := W + 100 * (FEffectiveCenturyFrom div 100 + 1);

            end;
            SetYear(W);
          end;

        dtpDay:
          SetDay(W);

        dtpMonth:
          SetMonth(W);

      end;

    finally
      Dec(FStoredLockCount);
    end;
  end;
end;

function TCustomDateEditor.GetSelectedText: UTF8String;
begin
  Result := FTextPart[FSelectedTextPart];
end;

procedure TCustomDateEditor.AdjustEffectiveCenturyFrom;
var
  Y1, Y2, M, D: Word;
begin
  DecodeDate(FMinDate, Y1, M, D);

  if Y1 > FCenturyFrom then
    FEffectiveCenturyFrom := Y1 // If we use CenturyFrom which is set to value
         // below MinDate's year, then when user enters two digit year, the
         // TheDate would automatically be set to MinDate value, even though
         // we perhaps allow same two-digit year in following centuries. It
         // would be less user friendly.
         // This is therefore better.

  else begin
    DecodeDate(FMaxDate, Y2, M, D);

    if Y2 < 100 then
      Y2 := 0
    else
      Dec(Y2, 99); // -- We should not use CenturyFrom if it is set to value
       // greater then MaxDate's year minus 100 years.
       // For example:
       // if CenturyFrom = 1941 and MaxDate = 31.12.2025, then if user enters
       // Year 33, we could not set the year to 2033 anyway, because of MaxDate
       // limit. Note that if we just leave CenturyFrom to effectively remain as
       // is, then in case of our example the TheDate would be automatically
       // reduced to MaxDate value. Setting the year to 1933 is rather expected
       // behaviour, so our internal field FEffectiveCenturyFrom should be 1926.

    // Therefore:
    if Y2 < FCenturyFrom then
      FEffectiveCenturyFrom := Max(Y1, Y2)
    else
      FEffectiveCenturyFrom := FCenturyFrom; // -- FCenturyFrom has passed all
                   // our tests, so we'll really use it without any correction.
  end;
end;

procedure TCustomDateEditor.FontChanged(Sender: TObject);
begin
  FRecalculatingTextSizesNeeded := True;
  inherited FontChanged(Sender);
end;

function TCustomDateEditor.GetTextOrigin: TPoint;
begin
  Result.x := BorderSpacing.InnerBorder + BorderWidth;
  Result.y := Result.x;
end;

procedure TCustomDateEditor.SetReadOnly(Value: Boolean);
begin
  if FReadOnly <> Value then begin
    if Value then begin
      ConfirmChanges;
      UpdateDate;
    end;

    FReadOnly := Value;
  end;
end;

procedure TCustomDateEditor.KeyDown(var Key: Word; Shift: TShiftState);
begin
  Inc(FStoredLockCount);
  try
    inherited KeyDown(Key, Shift); // calls OnKeyDown event

    case Key of
      VK_LEFT, VK_RIGHT:
        begin
          UpdateIfUserChangedText;
          if Key = VK_LEFT then begin
            if FSelectedTextPart = Low(FSelectedTextPart) then
              FSelectedTextPart := High(FSelectedTextPart)
            else
              Dec(FSelectedTextPart);
          end else begin
            if FSelectedTextPart = High(FSelectedTextPart) then
              FSelectedTextPart := Low(FSelectedTextPart)
            else
              Inc(FSelectedTextPart);
          end;
          Invalidate;
        end;
      VK_UP:
        begin
          UpdateIfUserChangedText;
          if not FReadOnly then begin
            case GetCurrentDateTextPart of
              dtpDay: IncreaseDay;
              dtpMonth: IncreaseMonth;
              dtpYear: IncreaseYear;
            end;
          end;
        end;
      VK_DOWN:
        begin
          UpdateIfUserChangedText;
          if not FReadOnly then
            case GetCurrentDateTextPart of
              dtpDay: DecreaseDay;
              dtpMonth: DecreaseMonth;
              dtpYear: DecreaseYear;
            end;
        end;
      VK_RETURN:
        if not FReadOnly then begin
          ConfirmChanges;
          EditingDone;
        end;
      VK_ESCAPE:
        if not FReadOnly then
          UndoChanges;
      VK_N:
        if (not FReadOnly) and FNullInputAllowed then begin
          if FUserChangedText or (not IsNullDate) then begin
            SetTheDate(NullDate);
            Change;
          end else
            SetTheDate(NullDate);
        end;
    end;
  finally
    Dec(FStoredLockCount);
  end;
end;

procedure TCustomDateEditor.KeyPress(var Key: char);
var
  S: String;
  DTP: TDateTextPart;
  N, L, I: Integer;
  W: array[1..3] of Word;
  YMD: TYMD;
  D: TDateTime;
  Reverted: Boolean;
begin
  Inc(FStoredLockCount);
  try
    inherited KeyPress(Key);
    Reverted := False;

    if (not ReadOnly) and (Key in ['0'..'9']) then begin

      DTP := GetCurrentDateTextPart;
      if DTP = dtpYear then
        N := 4
      else
        N := 2;

      S := Trim(GetSelectedText);
      if FUserChangedText and (UTF8Length(S) < N) then begin
        S := S + Key;

        if not FLeadingZeros then
          while (UTF8Length(S) > 1) and (UTF8Copy(S, 1, 1) = '0') do
            UTF8Delete(S, 1, 1);

      end else begin
        S := Key;
      end;

      if (UTF8Length(S) >= N) then begin

        L := StrToInt(S);
        YMD := GetYYYYMMDD(True);
        case DTP of
          dtpDay: YMD.Day := L;
          dtpMonth: YMD.Month := L;
          dtpYear: YMD.Year := L;
        end;
        if not TryEncodeDate(YMD.Year, YMD.Month, YMD.Day, D) then begin
          D := MinDate - 1;
        end;
        if (D < MinDate) or (D > MaxDate) then begin
          if N = 4 then begin
            UpdateDate;
            Change;
            Reverted := True;
          end else
            S := Key;

        end;

      end else begin

        if IsNullDate then begin
          DecodeDate(Max(MinDate, Min(SysUtils.Date, MaxDate)), W[3], W[2], W[1]);

          L := 3;
          case GetEffectiveDateDisplayOrder of
            ddoMDY:
              Exchange(W[1], W[2]);
            ddoYMD:
              begin
                Exchange(W[1], W[3]);
                L := 1;
              end;
          end;

          for I := Low(FTextPart) to High(FTextPart) do
            if I <> FSelectedTextPart then begin
              if I = L then
                FTextPart[I] := RightStr('000' + IntToStr(W[I]), 4)
              else if FLeadingZeros then
                FTextPart[I] := RightStr('0' + IntToStr(W[I]), 2)
              else
                FTextPart[I] := IntToStr(W[I]);
            end;

        end;

      end;
      if (not Reverted) and (FTextPart[FSelectedTextPart] <> S) then begin
        FTextPart[FSelectedTextPart] := S;
        FUserChangedText := True;
        Change;
        Invalidate;
      end;
    end;

  finally
    Dec(FStoredLockCount);
  end;
end;

procedure TCustomDateEditor.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  M, NX: Integer;
begin
  UpdateIfUserChangedText;
  if CanFocus then
    SetFocus;

  if Focused then begin
// Calculating mouse position inside text
//       in order to select date part under mouse cursor.
    NX := X - GetTextOrigin.x;
    M := FTwoDigitsWidth;
    if GetEffectiveDateDisplayOrder = ddoYMD then
      M := 2 * M;
    Inc(M, FSeparatorWidth div 2);

    if M > NX then begin
      FSelectedTextPart := 1;
    end else begin
      M := M + FSeparatorWidth + FTwoDigitsWidth;
      if M > NX then begin
        FSelectedTextPart := 2;
      end else begin
        FSelectedTextPart := 3;
      end;
    end;
    Invalidate;
//-------------------------------------------------------
  end;

  inherited;
end;

procedure TCustomDateEditor.UpdateDate;
var
  W: Array[1..3] of Word;
  YearPos, I: Integer;
begin
  FUserChangedText := False;

  if IsNullDate then begin
    if GetEffectiveDateDisplayOrder = ddoYMD then begin
      FTextPart[1] := '0000';
      FTextPart[3] := '00';
    end else begin
      FTextPart[1] := '00';
      FTextPart[3] := '0000';
    end;
    FTextPart[2] := '00';
  end else begin

    if not FJumpMinMax then begin
      if FTheDate > MaxDate then
        FTheDate := MaxDate;

      if FTheDate < MinDate then
        FTheDate := MinDate;

    end;

    DecodeDate(FTheDate, W[3], W[2], W[1]);
    YearPos := 3;
    case GetEffectiveDateDisplayOrder of
      ddoMDY:
        Exchange(W[1], W[2]);

      ddoYMD:
        begin
          Exchange(W[1], W[3]);
          YearPos := 1;
        end;
    end;

    for I := Low(FTextPart) to High(FTextPart) do begin
      if I = YearPos then
        FTextPart[I] := RightStr('000' + IntToStr(W[I]), 4)
      else if FLeadingZeros then
        FTextPart[I] := RightStr('0' + IntToStr(W[I]), 2)
      else
        FTextPart[I] := IntToStr(W[I]);

    end;
  end;

  if FStoredLockCount = 0 then
    ConfirmChanges;

  Invalidate;
end;

procedure TCustomDateEditor.SelectDay;
begin
  SelectDateTextPart(dtpDay);
end;

procedure TCustomDateEditor.SelectMonth;
begin
  SelectDateTextPart(dtpMonth);
end;

procedure TCustomDateEditor.SelectYear;
begin
  SelectDateTextPart(dtpYear);
end;

procedure TCustomDateEditor.Paint;
var
  I: Integer;
  DD: Array[1..3] of Integer;
  R: TRect;

  SelectStep: 0..3;
  TextStyle: TTextStyle;
begin
  if FRecalculatingTextSizesNeeded then begin
    if AutoSize then begin
      InvalidatePreferredSize;
      AdjustSize;
    end else
      RecalculateTextSizesIfNeeded;
  end;
  TextStyle := Canvas.TextStyle;

  Canvas.Brush.Style := bsSolid;
  Canvas.Brush.Color := Color;
  Canvas.FillRect(ClientRect);

  R.TopLeft := GetTextOrigin;
  R.Bottom := R.Top + FTextHeight;

  {
  Canvas.TextStyle.Layout := tlCenter;
  Canvas.TextStyle.Wordbreak := False;
  Canvas.TextStyle.Opaque := False;
  }
  TextStyle.Layout := tlCenter;
  TextStyle.Wordbreak := False;
  TextStyle.Opaque := False;
  if IsNullDate and (FTextForNullDate > '')
                       and (not Focused) then begin

    R.Right := R.Left + 4 * FTwoDigitsWidth + 2 * FSeparatorWidth;
    if FTrailingSeparator then
      Inc(R.Right, FSepNoSpaceWidth);

    if R.Right - R.Left > Canvas.GetTextWidth(FTextForNullDate) then
      //Canvas.TextStyle.Alignment := taCenter
      TextStyle.Alignment := taCenter
    else
      //Canvas.TextStyle.Alignment := taLeftJustify;
      TextStyle.Alignment := taLeftJustify;

    if Enabled then
      Canvas.Font.Color := Font.Color
    else
      Canvas.Font.Color := clGrayText;

    Canvas.TextRect(R, R.Left, R.Top, FTextForNullDate, TextStyle);

  end else begin
    //Canvas.TextStyle.Alignment := taRightJustify;
    TextStyle.Alignment := taRightJustify;

    DD[2] := FTwoDigitsWidth;
    if GetEffectiveDateDisplayOrder = ddoYMD then begin
      DD[1] := 2 * FTwoDigitsWidth;
      DD[3] := FTwoDigitsWidth;
    end else begin
      DD[1] := FTwoDigitsWidth;
      DD[3] := 2 * FTwoDigitsWidth;
    end;

    SelectStep := 0;
    if Enabled then begin
      Canvas.Font.Color := Font.Color;
      if Focused then
        SelectStep := FSelectedTextPart;
    end else begin
      Canvas.Font.Color := clGrayText;
    end;

    for I := 1 to 3 do begin
      if SelectStep = I then begin
        //Canvas.TextStyle.Opaque := True;
        TextStyle.Opaque := True;
        Canvas.Brush.Color := clHighlight;
        Canvas.Font.Color := clHighlightText;
      end;

      R.Right := R.Left + DD[I];
      Canvas.TextRect(R, R.Left, R.Top, FTextPart[I], TextStyle);
      R.Left := R.Right; // + 1;

      if SelectStep = I then begin
        //Canvas.TextStyle.Opaque := False;
        TextStyle.Opaque := False;
        Canvas.Brush.Color := Color;
        Canvas.Font.Color := Self.Font.Color;
      end;
      if (I < 3) then begin
        R.Right := R.Left + FSeparatorWidth;
        Canvas.TextRect(R, Canvas.PenPos.x, canvas.PenPos.y,
                                      FTheDateSeparator, TextStyle);
        R.Left := R.Right; // + 1;
      end else if FTrailingSeparator then begin
        R.Right := R.Left + FSepNoSpaceWidth;
        Canvas.TextRect(R, Canvas.PenPos.x, canvas.PenPos.y,
                                      TrimRight(FTheDateSeparator), TextStyle);
      end;
    end;

  end;
  inherited Paint;
end;

function TCustomDateEditor.IsNullDate: Boolean;
begin
  Result := IsNan(FTheDate);
end;

function TCustomDateEditor.GetCurrentDateTextPart: TDateTextPart;
begin
  case FSelectedTextPart of
    1: Result := dtpDay;
    2: Result := dtpMonth;
    3: Result := dtpYear;
  end;

  case GetEffectiveDateDisplayOrder of
    ddoMDY: if Result = dtpDay then Result := dtpMonth
            else if Result = dtpMonth then Result := dtpDay;
    ddoYMD: if Result = dtpDay then Result := dtpYear
            else if Result = dtpYear then Result := dtpDay;
  end;
end;

procedure TCustomDateEditor.SelectDateTextPart(const DateTextPart: TDateTextPart);
begin
  case DateTextPart of
    dtpDay: //SelectDay;
      begin
        case GetEffectiveDateDisplayOrder of
          ddoDMY: FSelectedTextPart := 1;
          ddoMDY: FSelectedTextPart := 2;
          ddoYMD: FSelectedTextPart := 3;
        end;
      end;
    dtpMonth: //SelectMonth;
      begin
        if GetEffectiveDateDisplayOrder = ddoMDY then
          FSelectedTextPart := 1
        else
          FSelectedTextPart := 2;
      end;
  else
    //SelectYear;
    begin
      if GetEffectiveDateDisplayOrder = ddoYMD then
        FSelectedTextPart := 1
      else
        FSelectedTextPart := 3;
    end;
  end;

  Invalidate;
end;

procedure TCustomDateEditor.ConfirmChanges;
begin
  FConfirmedDate := FTheDate;
  if Assigned(FOnConfirmChanges) then
    FOnConfirmChanges(Self);
end;

procedure TCustomDateEditor.UndoChanges;
begin
  SetTheDate(FConfirmedDate);
end;

procedure TCustomDateEditor.SetTheDateJumpMinMax(const AValue: TDateTime);
begin
  FJumpMinMax := True;
  try
    SetTheDate(AValue);
  finally
    FJumpMinMax := False;
  end;
end;

procedure TCustomDateEditor.ChangeDateInternally(const AValue: TDateTime);
begin
  Inc(FStoredLockCount);
  try
    SetTheDate(AValue);
  finally
    Dec(FStoredLockCount);
  end;
end;

procedure TCustomDateEditor.DoEnter;
begin
  inherited DoEnter;

  Invalidate;
end;

procedure TCustomDateEditor.DoExit;
begin
  ConfirmChanges;
  inherited DoExit;
  Invalidate;
end;

procedure TCustomDateEditor.EditingDone;
begin
  UpdateIfUserChangedText;

  inherited EditingDone;
end;

procedure TCustomDateEditor.SetEnabled(Value: Boolean);
begin
  if GetEnabled <> Value then begin
    inherited SetEnabled(Value);
    Invalidate;
  end;
end;

procedure TCustomDateEditor.CalculatePreferredSize(var PreferredWidth,
  PreferredHeight: integer; WithThemeSpace: Boolean);
var
  TextOrigin: TPoint;
  Was0: Boolean;
begin
  inherited CalculatePreferredSize(PreferredWidth, PreferredHeight,
    WithThemeSpace);

  Was0 := PreferredHeight = 0;

  RecalculateTextSizesIfNeeded;
  TextOrigin := GetTextOrigin;

  PreferredWidth := PreferredWidth + 2 * TextOrigin.y
                   + 4 * FTwoDigitsWidth + 2 * FSeparatorWidth;
  PreferredHeight := Max(2 * TextOrigin.y + FTextHeight, PreferredHeight);

  if FTrailingSeparator then
    //Inc(PreferredWidth, FSeparatorWidth);
    Inc(PreferredWidth, FSepNoSpaceWidth);

  if Was0 then begin
    if BorderStyle = bsSingle then begin
{
Only by experimenting, I came to conclusion that BorderStyle bsSingle needs two
pixels on each side around. It has nothing to do with BorderWidth property,
that's apparently separate thing. Is there some property which gives this value?
For now, I just assume two pixels on each side. Therefore, I add 4 to width and
height:
This seems to work well on both Windows (win WS) and Linux (gtk2 WS).
}
      PreferredWidth := PreferredWidth + 4;
      PreferredHeight := PreferredHeight + 4;
    end;
  end;
end;

constructor TCustomDateEditor.Create(AOwner: TComponent);
var
  I: Integer;
begin
  inherited Create(AOwner);

  FNullInputAllowed := True;
  FTextForNullDate := 'NULL';
  FCenturyFrom := 1941;
  FRecalculatingTextSizesNeeded := True;
  FOnChange := nil;
  FSeparatorWidth := 0;
  FTwoDigitsWidth := 0;
  FTextHeight := 0;
  for I := Low(FTextPart) to High(FTextPart) do
    FTextPart[I] := '';

  FLeadingZeros := True;
  FStoredLockCount := 0;
  FReadOnly := False;
  FTheDate := SysUtils.Date;
  FConfirmedDate := FTheDate;
  FMinDate := TheSmallestDate;
  FMaxDate := TheBiggestDate;
  FTrailingSeparator := False;
  FDateDisplayOrder := ddoTryDefault;
  FSelectedTextPart := 1;
  FUseDefaultDateSeparator := True;
  FTheDateSeparator := DateSeparator;
  FEffectiveCenturyFrom := FCenturyFrom;
  FJumpMinMax := False;

  ParentColor := False;
  TabStop := True;
  BorderWidth := 2;
  BorderStyle := bsSingle;
  ParentFont := True;
  AutoSize := True;

  UpdateDate;
end;

procedure TCustomDateEditor.IncreaseMonth;
var
  YMD: TYMD;
  N: Word;
begin
  SelectMonth;
  YMD := GetYYYYMMDD(True);

  if YMD.Month >= 12 then
    YMD.Month := 1
  else
    Inc(YMD.Month);

  N := NumberOfDaysInMonth(YMD.Month, YMD.Year);
  if YMD.Day > N then
    YMD.Day := N;

  SetYYYYMMDD(YMD, True);
end;

procedure TCustomDateEditor.IncreaseYear;
var
  YMD: TYMD;
begin
  SelectYear;
  YMD := GetYYYYMMDD(True);

  Inc(YMD.Year);
  if (YMD.Month = 2) and (YMD.Day > 28) and (not IsLeapYear(YMD.Year)) then
    YMD.Day := 28;

  SetYYYYMMDD(YMD, True);
end;

procedure TCustomDateEditor.IncreaseDay;
var
  YMD: TYMD;
begin
  SelectDay;
  YMD := GetYYYYMMDD(True);

  if YMD.Day >= NumberOfDaysInMonth(YMD.Month, YMD.Year) then
    YMD.Day := 1
  else
    Inc(YMD.Day);

  SetYYYYMMDD(YMD, True);
end;

procedure TCustomDateEditor.DecreaseMonth;
var
  YMD: TYMD;
  N: Word;
begin
  SelectMonth;
  YMD := GetYYYYMMDD(True);

  if YMD.Month <= 1 then
    YMD.Month := 12
  else
    Dec(YMD.Month);

  N := NumberOfDaysInMonth(YMD.Month, YMD.Year);
  if YMD.Day > N then
    YMD.Day := N;

  SetYYYYMMDD(YMD, True);
end;

procedure TCustomDateEditor.DecreaseYear;
var
  YMD: TYMD;
begin
  SelectYear;
  YMD := GetYYYYMMDD(True);
  Dec(YMD.Year);
  if (YMD.Month = 2) and (YMD.Day > 28) and (not IsLeapYear(YMD.Year)) then
    YMD.Day := 28;
  SetYYYYMMDD(YMD, True);
end;

procedure TCustomDateEditor.DecreaseDay;
var
  YMD: TYMD;
begin
  SelectDay;
  YMD := GetYYYYMMDD(True);

  if YMD.Day <= 1 then
    YMD.Day := NumberOfDaysInMonth(YMD.Month, YMD.Year)
  else
    Dec(YMD.Day);

  SetYYYYMMDD(YMD, True);
end;

procedure TCustomDateEditor.Change;
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

{ TCustomDatePicker }

procedure TCustomDatePicker.SetChecked(const AValue: Boolean);
begin
  if Assigned(FCheckBox) then
    FCheckBox.Checked := AValue;
end;

procedure TCustomDatePicker.CheckIfDateEditorIsEnabled;
begin
  FDateEditor.SetEnabled(Self.Enabled and GetChecked);

  if GetShowCalendar then begin
    FArrowButton.Enabled := FDateEditor.Enabled;
  end;
end;

procedure TCustomDatePicker.SetDateDisplayOrder(const AValue: TDateDisplayOrder
  );
begin
  FDateEditor.DateDisplayOrder := AValue;
end;

procedure TCustomDatePicker.SetLeadingZeros(const AValue: Boolean);
begin
  FDateEditor.LeadingZeros := AValue;
end;

procedure TCustomDatePicker.SetNullInputAllowed(const AValue: Boolean);
begin
  FDateEditor.NullInputAllowed := AValue;
end;

procedure TCustomDatePicker.SetTheDate(const AValue: TDateTime);
begin
  if not Equals(FDateEditor.TheDate, AValue) then
    FDateEditor.TheDate := AValue;
end;

procedure TCustomDatePicker.SetTheDateSeparator(const AValue: UTF8String);
begin
  FDateEditor.TheDateSeparator := AValue;
end;

procedure TCustomDatePicker.SetMaxDate(const AValue: TDateTime);
begin
  FDateEditor.MaxDate := AValue;
end;

procedure TCustomDatePicker.SetMinDate(const AValue: TDateTime);
begin
  FDateEditor.MinDate := AValue;
end;

procedure TCustomDatePicker.SetReadOnly(const AValue: Boolean);
begin
  FDateEditor.ReadOnly := AValue;
end;

procedure TCustomDatePicker.SetShowCalendar(const AValue: Boolean);
begin
  if GetShowCalendar <> AValue then begin

    if AValue then begin
      FArrowButton := TSpeedButton.Create(Self);
      FArrowButton.ControlStyle := FArrowButton.ControlStyle + [csNoDesignSelectable];

      FArrowButton.SetBounds(0, 0, 17, 1);

      FArrowButton.Parent := Self;
      FArrowButton.Align := alRight;
      FArrowButton.BringToFront;

      DrawArrowButtonGlyph;

      FArrowButton.OnMouseDown := @ArrowMouseDown;
    end else begin
      FArrowButton.OnMouseDown := nil;
      DestroyCalendarForm;
      FreeAndNil(FArrowButton);
    end;

    ArrangeCtrls;
  end;
end;

procedure TCustomDatePicker.SetShowCheckBox(const AValue: Boolean);
var
  CB: TCheckBox;
begin
  if GetShowCheckBox <> AValue then begin
    if AValue then begin
      FCheckBox := TCheckBox.Create(Self);

      {$IFNDEF WINDOWS}
      {  On Windows, the following line seems to not have any effect, but I
         enclosed it in IFNDEF anyway. }
      FCheckBox.Color := clBtnFace; { This line is here because of CheckBox's
         strange behavior in Linux -- when parent's colour is white, which is
         the default in our case (actually, our default is clWindow, but it's
         usually white) and when the check box is on a form shown modally, if
         we close the form and then show it again, the check box refuses to
         paint it's "checker" shape.

         I spent a lot of time trying to solve this and this is the best I came
         up with -- setting the check box's colour to clBtnFace seems to be a
         workaround.

         Nice thing is that it seems not to really effect neither the checker's
         colour on the screen, nor the colour of check box's "box", so we didn't
         actually spoil the check box's default appearence on the screen.   }
      {$ENDIF}

      FCheckBox.ControlStyle := FCheckBox.ControlStyle + [csNoDesignSelectable];
      FCheckBox.AllowGrayed := False;
      FCheckBox.TabStop := False;

      FCheckBox.Parent := Self;

      FCheckBox.Checked := True;

      FCheckBox.OnChange := @CheckBoxChange;
      ArrangeCtrls;
    end else begin
      FCheckBox.OnChange := nil;

      CB := FCheckBox;
      FCheckBox := nil;

      ArrangeCtrls;

      if not CB.Checked then
        CheckIfDateEditorIsEnabled;

      if Self.Enabled and CB.Focused and FDateEditor.Enabled then
        FDateEditor.SetFocus;

      FreeAndNil(CB);
    end;
  end;
end;

procedure TCustomDatePicker.SetTextForNullDate(const AValue: UTF8String);
begin
  FDateEditor.TextForNullDate := AValue;
end;

procedure TCustomDatePicker.SetTrailingSeparator(const AValue: Boolean);
begin
  FDateEditor.TrailingSeparator := AValue;
end;

procedure TCustomDatePicker.SetUseDefaultDateSeparator(const AValue: Boolean);
begin
  FDateEditor.UseDefaultDateSeparator := AValue;
end;

procedure TCustomDatePicker.DestroyTheCalendar;
begin
  if Assigned(FCal) then begin
    FCal.OnChange := nil;
    FCal.OnResize := nil;
    FCal.OnMouseUp := nil;
    FCal.OnKeyDown := nil;
    FreeAndNil(FCal);
  end;
  FreeAndNil(FShape);
end;

procedure TCustomDatePicker.AdjustCalendarFormSize;
begin
  FCalendarForm.ClientWidth := FCal.Width + 2;
  FCalendarForm.ClientHeight := FCal.Height + 2;

  FShape.SetBounds(0, 0, FCalendarForm.ClientWidth, FCalendarForm.ClientHeight);

  AdjustCalendarFormScreenPosition;
end;

procedure TCustomDatePicker.CreateCalendarForm;
var
  P: TPoint;
begin
  if not (csDesigning in ComponentState) then begin
    DestroyCalendarForm;

    FCloseCalendarOnChange := False;

    P := Point(0, 0);
    FCal := TCalendar.Create(nil);
    FCal.GetPreferredSize(P.x, P.y);

    FCal.Align := alNone;

    FCal.SetBounds(1, 1, P.x, P.y);

    FCal.TabStop := True;

    FCalendarForm := TCustomForm.Create(nil);

    FCalendarForm.SetBounds(-8000, -8000, P.x + 2, P.y + 2);

    FRememberedCalendarFormOrigin := Point(-8000, -8000);

    FCalendarForm.ShowInTaskBar := stNever;
    FCalendarForm.BorderStyle := bsNone;

    FShape := TShape.Create(nil);
    FShape.Brush.Style := bsClear;

    FCal.Parent := FCalendarForm;
    FShape.Parent := FCalendarForm;

    FCal.OnResize := @CalendarResize;
    FCal.OnMouseUp := @CalendarMouseUp;
    FCal.OnKeyDown := @CalendarKeyDown;
    FCal.OnChange := @CalendarChange;

    FCalendarForm.OnDeactivate := @CalendarFormDeactivate;
    FCalendarForm.OnClose := @CalendarFormClose;
    FCalendarForm.OnShow := @CalendarFormShow;
    FCalendarForm.OnDestroy := @CalendarFormDestroy;

    AdjustCalendarFormSize;
  end;
end;

procedure TCustomDatePicker.DestroyCalendarForm;
begin
  if Assigned(FCalendarForm) then begin
    DestroyTheCalendar;
    FCalendarForm.Release;
    FCalendarForm := nil;
  end;
end;

procedure TCustomDatePicker.AdjustCalendarFormScreenPosition;
var
  R: TRect;
  P: TPoint;
  H, W: Integer;
begin
  H := FCalendarForm.Height;
  W := FCalendarForm.Width;

  P := ControlToScreen(Point(0, Height));

  R := Screen.MonitorFromWindow(Self.Handle).WorkareaRect;

  if P.y > R.Bottom - H then
    P.y := P.y - H - Height;

  if P.y < R.Top then
    P.y := R.Top;

  if P.x > R.Right - W then
    P.x := R.Right - W;

  if P.x < R.Left then
    P.x := R.Left;

  if (P.x <> FRememberedCalendarFormOrigin.x)
            or (P.y <> FRememberedCalendarFormOrigin.y) then begin
    FCalendarForm.SetBounds(P.x, P.y, W, H);
    FRememberedCalendarFormOrigin := P;
  end;

end;

class function TCustomDatePicker.GetControlClassDefaultSize: TPoint;
begin
  Result.x := 102;
  Result.y := 23;
end;

procedure TCustomDatePicker.ConfirmChanges;
begin
// Do nothing -- descendent classes can override this procedure to respond to
//  FDataEditor's OnConfirmChanges event. (we'll do so in data aware version
//  of this control.
end;

procedure TCustomDatePicker.SetEnabled(Value: Boolean);
begin
  inherited SetEnabled(Value);
  CheckIfDateEditorIsEnabled;
end;

// I had to override CreateWnd, because in design time on Linux Lazarus crashes
// if we try to do anchoring of child controls in constructor.
// Therefore, I needed to ensure that controls anchoring does not take place
// before CreateWnd has done. So, I moved all anchoring code to a procedure
// ArrangeCtrls and introduced a boolean field FDoNotArrangeControls which
// prevents that code from executing before CreateWnd.
//!!! Later, I simplified the arranging procedure, so maybe it can be done now
//    before window creation is done. It's better to leave this delay system,
//    anyway -- we might change anchoring code again for some reason.
procedure TCustomDatePicker.CreateWnd;
begin
  inherited CreateWnd;

  if FDoNotArrangeControls then begin { This field is set to True in constructor.
    Its purpose is to prevent control anchoring until this point. That's because
    on Linux Lazarus crashes when control is dropped on form in designer if
    particular anchoring code executes before CreateWnd has done its job. }
    FDoNotArrangeControls := False;
    ArrangeCtrls;
  end;
end;

procedure TCustomDatePicker.SetTheDateJumpMinMax(const AValue: TDateTime);
begin
  FDateEditor.SetTheDateJumpMinMax(AValue);
end;


procedure TCustomDatePicker.ArrangeCtrls;
begin
  if not FDoNotArrangeControls then begin //Read the note above CreateWnd procedure.
    DisableAutoSizing;
    DisableAlign;
    try
      FDateEditor.Align := alNone;
      if GetShowCheckBox then begin
        FCheckBox.Align := alLeft;
        FCheckBox.BorderSpacing.Left := 2;
        FDateEditor.AnchorToNeighbour(akLeft, 0, FCheckBox);
        FCheckBox.Constraints.MinHeight := FDateEditor.Height;
        FCheckBox.BringToFront;
      end else begin
        FDateEditor.AnchorSideLeft.Control := nil;
        FDateEditor.Align := alLeft;
      end;
      CheckIfDateEditorIsEnabled;
      InvalidatePreferredSize;
      AdjustSize;
      Invalidate;
    finally
      EnableAlign;
      EnableAutoSizing;
    end;
  end;
end;

procedure TCustomDatePicker.Change;
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TCustomDatePicker.CalculatePreferredSize(var PreferredWidth,
  PreferredHeight: integer; WithThemeSpace: Boolean);
begin
  inherited CalculatePreferredSize(PreferredWidth, PreferredHeight,
    WithThemeSpace);
  if GetShowCalendar then
    PreferredWidth := PreferredWidth + Min(FArrowButton.Width, FDateEditor.Width) + 3;
end;

procedure TCustomDatePicker.DateEditorChange(Sender: TObject);
begin
  Change;
end;

procedure TCustomDatePicker.DateEditorKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  KeyDown(Key, Shift); // We call our event handler first, so that handler can
    // prevent the default code from executing (by assigning 0 to Key; which
    // would also prevent FDateEditor's default key handling, as this code is
    // called from there first).

  if Key = VK_SPACE then begin
    // Pressing the space bar checks / unchecks the check box.
    if GetShowCheckBox then begin
    { On Linux, it seems to be enough to call FCheckBox.SetFocus. Check box
      gets checked or unchecked automatically. Maybe the check box receives
      this key event too, I don't know, but when I did testing on Linux,
      leaving both next lines acted as if the check box is clicked twice, so
      we need to isolate the first line from Linux.
      On Windows, however, both next lines should execute.  }
    {$IFDEF LCLWIN32}
      FCheckBox.Checked := not FCheckBox.Checked;
    {$ENDIF}
      FCheckBox.SetFocus;
    end;
  end;

end;

procedure TCustomDatePicker.DateEditorKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  KeyUp(Key, Shift);
end;

procedure TCustomDatePicker.DateEditorKeyPress(Sender: TObject; var Key: char);
begin
  KeyPress(Key);
end;

procedure TCustomDatePicker.DateEditorUTF8KeyPress(Sender: TObject;
  var UTF8Key: TUTF8Char);
begin
  UTF8KeyPress(UTF8Key);
end;

procedure TCustomDatePicker.DateEditorMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  P, P1: TPoint;
begin
  P := FDateEditor.ClientOrigin;
  P1 := ClientOrigin;
  X := X + P.x - P1.x;
  Y := Y + P.y - P1.y;

  MouseDown(Button, Shift, X, Y);
end;

procedure TCustomDatePicker.DateEditorMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  P, P1: TPoint;
begin
  P := FDateEditor.ClientOrigin;
  P1 := ClientOrigin;
  X := X + P.x - P1.x;
  Y := Y + P.y - P1.y;

  MouseMove(Shift, X, Y);
end;

procedure TCustomDatePicker.DateEditorMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  P, P1: TPoint;
begin
  P := FDateEditor.ClientOrigin;
  P1 := ClientOrigin;
  X := X + P.x - P1.x;
  Y := Y + P.y - P1.y;

  MouseUp(Button, Shift, X, Y);
end;

procedure TCustomDatePicker.DateEditorEnter(Sender: TObject);
begin
  FCallFromDateEditorEnter := True;
  try
    DoEnter;
  finally
    FCallFromDateEditorEnter := False;
  end;
end;

procedure TCustomDatePicker.DateEditorExit(Sender: TObject);
begin
  FCallFromDateEditorExit := True;
  try
    DoExit;
  finally
    FCallFromDateEditorExit := False;
  end;
end;

procedure TCustomDatePicker.DateEditorConfirmChanges(Sender: TObject);
begin
  ConfirmChanges;
end;

procedure TCustomDatePicker.DateEditorEditingDone(Sender: TObject);
begin
  FCallFromDateEditorEditingDone := True;
  try
    EditingDone;
  finally
    FCallFromDateEditorEditingDone := False;
  end;
end;

procedure TCustomDatePicker.ArrowMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  DropDownCalendarForm;
end;

procedure TCustomDatePicker.DoDropDown;
begin
  if Assigned(FOnDropDown) then
    FOnDropDown(Self);
end;

procedure TCustomDatePicker.DoCloseUp;
begin
  if Assigned(FOnCloseUp) then
    FOnCloseUp(Self);
end;

function TCustomDatePicker.GetChecked: Boolean;
begin
  Result := (not Assigned(FCheckBox)) or (FCheckBox.State = cbChecked);
end;

procedure TCustomDatePicker.DrawArrowButtonGlyph;
const
  ArrowColor = TColor($8D665A);
begin
// First I ment to put arrow images in a lrs file. In my opinion, however, that
// wouldn't be an elegant option for so simple shapes.

  if Assigned(FArrowButton) then begin
    FArrowButton.Glyph.TransparentColor := clRed;
    FArrowButton.Glyph.SetSize(9, 6);
    FArrowButton.Glyph.Canvas.Brush.Style := bsSolid;
    FArrowButton.Glyph.Canvas.Brush.Color := clSkyBlue;
    FArrowButton.Glyph.Canvas.FillRect(0, 0, 9, 6);
    FArrowButton.Glyph.Canvas.Pen.Color := ArrowColor;
    FArrowButton.Glyph.Canvas.Brush.Color := FArrowButton.Glyph.Canvas.Pen.Color;

{ Let's draw shape of the arrow on the button: }
    case FArrowShape of
      asClassicLarger:
        { triangle: }
        FArrowButton.Glyph.Canvas.Polygon([Point(0, 1), Point(8, 1), Point(4, 5)]);
      asClassicSmaller:
        { triangle -- smaller variant:  }
        FArrowButton.Glyph.Canvas.Polygon([Point(1, 2), Point(7, 2), Point(4, 5)]);
        { modern: }
      asModernLarger:
        FArrowButton.Glyph.Canvas.Polygon([Point(0, 1), Point(1, 0), Point(4, 3),
                                        Point(7, 0), Point(8, 1), Point(4, 5)]);
      asModernSmaller:
        { modern -- smaller variant:    }
        FArrowButton.Glyph.Canvas.Polygon([Point(1, 2), Point(2, 1), Point(4, 3),
                                        Point(6, 1), Point(7, 2), Point(4, 5)]);
      asYetAnotherShape:
        { something in between, not very pretty:  }
        FArrowButton.Glyph.Canvas.Polygon([Point(0, 1), Point(1, 0), Point(2, 1),
                            Point(6, 1),Point(7, 0), Point(8, 1), Point(4, 5)]);
    end;
    FArrowButton.Glyph.TransparentColor := clSkyBlue;
  end;
end;

function TCustomDatePicker.GetCenturyFrom: Word;
begin
  Result := FDateEditor.CenturyFrom;
end;

function TCustomDatePicker.GetDateDisplayOrder: TDateDisplayOrder;
begin
  Result := FDateEditor.DateDisplayOrder;
end;

function TCustomDatePicker.GetLeadingZeros: Boolean;
begin
  Result := FDateEditor.LeadingZeros;
end;

function TCustomDatePicker.GetNullInputAllowed: Boolean;
begin
  Result := FDateEditor.NullInputAllowed;
end;

function TCustomDatePicker.GetTheDate: TDateTime;
begin
  Result := FDateEditor.TheDate;
end;

function TCustomDatePicker.GetTheDateSeparator: UTF8String;
begin
  Result := FDateEditor.TheDateSeparator;
end;

function TCustomDatePicker.GetMaxDate: TDateTime;
begin
  Result := FDateEditor.MaxDate;
end;

function TCustomDatePicker.GetMinDate: TDateTime;
begin
  Result := FDateEditor.MinDate;
end;

function TCustomDatePicker.GetReadOnly: Boolean;
begin
  Result := FDateEditor.ReadOnly;
end;

function TCustomDatePicker.GetShowCalendar: Boolean;
begin
  Result := Assigned(FArrowButton);
end;

function TCustomDatePicker.GetShowCheckBox: Boolean;
begin
  Result := Assigned(FCheckBox);
end;

function TCustomDatePicker.GetTextForNullDate: UTF8String;
begin
  Result := FDateEditor.TextForNullDate;
end;

function TCustomDatePicker.GetTrailingSeparator: Boolean;
begin
  Result := FDateEditor.TrailingSeparator;
end;

function TCustomDatePicker.GetUseDefaultDateSeparator: Boolean;
begin
  Result := FDateEditor.UseDefaultDateSeparator;
end;

procedure TCustomDatePicker.SetArrowShape(const AValue: TArrowShape);
begin
  if FArrowShape = AValue then Exit;

  FArrowShape := AValue;
  DrawArrowButtonGlyph;
end;

procedure TCustomDatePicker.SetCenturyFrom(const AValue: Word);
begin
  FDateEditor.CenturyFrom := AValue;
end;

procedure TCustomDatePicker.CheckBoxChange(Sender: TObject);
begin
  CheckIfDateEditorIsEnabled;
  if Assigned(FCheckBox) then begin
    if FCheckBox.Focused and FDateEditor.Enabled then
      FDateEditor.SetFocus;
    FCheckBox.TabStop := not FDateEditor.Enabled;
  end;
end;

procedure TCustomDatePicker.CalendarMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  //debugln('cal. mouse up');
  if FCal.HitTest(Point(X, Y)) in [cpDate, cpNoWhere] then begin
    {$IFDEF LCLQt}
    // According to tests made by Željko Rikalo,
    // on qt widgetset the calendar's DateTime field does not get updated if
    // we close the calendar form here, because on Qt change is not made until
    // after the OnMouseUp event.

    // Let's then try something else, as proposed by Željko:
    // Closing the calendar form is moved to Calendar.OnChange.
      {$IF (lcl_major=0) AND (lcl_minor=9) AND (lcl_release<29)}
      //Željko changed the Qt behaviour since Lazarus 0.9.29, revision 23641.
         FCloseCalendarOnChange := True; // This is asked in
                                                       // CalendarChange procedure.
      {$ENDIF}
    {$ENDIF}
    // On the other hand, on other widgetsets, the previous wouldn't work
    // because OnChange gets called before OnMoueseUp event, so the OnChange
    // event is already executed when we are here, so it's too late to notify
    // it now.
    // But the calendar's date is already changed then and we can simply
    // call CloseCalendarForm immidiately.
    if not FCloseCalendarOnChange then
      CloseCalendarForm(True);

  end;
end;

procedure TCustomDatePicker.CalendarChange(Sender: TObject);
begin
  //debugln('calendar change');
  {$IFDEF LCLQt}
  // See the coments written in CalendarMouseUp procedure.
  if FCloseCalendarOnChange then
    CloseCalendarForm(True);
  {$ENDIF}
end;

procedure TCustomDatePicker.CalendarKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    VK_ESCAPE:
      begin
        CloseCalendarForm;
        //Key := 0;
      end;
    VK_RETURN, VK_SPACE:
      begin
        CloseCalendarForm(True);
        //Key := 0;
      end;
  end;
end;

procedure TCustomDatePicker.CalendarFormDeactivate(Sender: TObject);
begin
  if not FClosingCalendarForm then begin
    CloseCalendarForm;
  end;
end;

procedure TCustomDatePicker.CalendarFormShow(Sender: TObject);
begin
  FClosingCalendarForm := False;

  AdjustCalendarFormScreenPosition;

  DoDropDown; // calls OnDropDown event handler
end;

procedure TCustomDatePicker.CalendarFormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  FClosingCalendarForm := True;
  CloseAction := caFree;
end;

procedure TCustomDatePicker.CalendarFormDestroy(Sender: TObject);
begin
  DestroyTheCalendar;
  FCalendarForm := nil;
end;

procedure TCustomDatePicker.DateEditorClick(Sender: TObject);
begin
  Click;
end;

procedure TCustomDatePicker.DateEditorDblClick(Sender: TObject);
begin
  DblClick;
end;

procedure TCustomDatePicker.DateEditorTripleClick(Sender: TObject);
begin
  TripleClick;
end;

procedure TCustomDatePicker.DateEditorQuadClick(Sender: TObject);
begin
  QuadClick;
end;

procedure TCustomDatePicker.DoEnter;
begin
  if FCallFromDateEditorEnter then
    inherited DoEnter
  else if FDateEditor.Enabled then
    FDateEditor.SetFocus
  else if Assigned(FCheckBox) then
    FCheckBox.SetFocus;
end;

procedure TCustomDatePicker.DoExit;
begin
  if FCallFromDateEditorExit then
    inherited DoExit;
end;

procedure TCustomDatePicker.EditingDone;
begin
  if FCallFromDateEditorEditingDone then
    inherited EditingDone;
end;

procedure TCustomDatePicker.CloseCalendarForm(AndSetTheDate: Boolean);
begin
  if Assigned(FCalendarForm) and (not FClosingCalendarForm) then begin
    FClosingCalendarForm := True;
    if AndSetTheDate then begin
      Change;
      FDateEditor.ChangeDateInternally(FCal.DateTime);
    end;

    try
      FDateEditor.SetFocus;
    except
    end;

    FCalendarForm.Close;
    DoCloseUp;
  end;
end;

procedure TCustomDatePicker.DropDownCalendarForm;
{$IFNDEF LCLWin32}
 var
   F: TCustomForm;
{$ENDIF}
begin
  if not (ReadOnly or Assigned(FCalendarForm)) then begin
    try
      CreateCalendarForm;

      if IsNullDate then
        FCal.DateTime := Max(MinDate, Min(SysUtils.Date, MaxDate))

      else if TheDate < MinDate then // These "out of bounds" values can
        FCal.DateTime := MinDate     // happen when TheDate was set with
      else if TheDate > MaxDate then // "SetTheDateJumpMinMax" protected
        FCal.DateTime := MaxDate     // procedure (used in TDBDatePicker control).

      else
        FCal.DateTime := TheDate;

  {$IFNDEF LCLWin32}
      // On Linux, it seems that if a non-modal form is shown on top
      // of a modal one, it can't get user interaction. So it is useless then.
      // Therefore, if our parent is shown modally, we must show the calendar
      // on a modal form too.
      F := GetParentForm(Self);
      if (F <> nil) and (fsModal in F.FormState) then
        FCalendarForm.ShowModal
      else
  {$ENDIF}
        FCalendarForm.Show;

    finally
      if Assigned(FCalendarForm) and (not FCalendarForm.Visible) then
        DestroyCalendarForm;
    end;
  end;
end;

procedure TCustomDatePicker.CalendarResize(Sender: TObject);
begin
  AdjustCalendarFormSize;
end;

constructor TCustomDatePicker.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  with GetControlClassDefaultSize do begin
    Self.Width := x;
    Self.Height := y;
  end;
  FArrowShape := asModernSmaller;
  FCallFromDateEditorEnter := False;
  FCallFromDateEditorExit := False;
  FCallFromDateEditorEditingDone := False;
  FOnDropDown := nil;
  FOnCloseUp := nil;
  FOnChange := nil;

  ParentColor := False;
  FCheckBox := nil;
  FArrowButton := nil;
  FDateEditor := TCustomDateEditor.Create(Self);
  FDateEditor.ControlStyle := FDateEditor.ControlStyle + [csNoDesignSelectable];
  FDateEditor.BorderStyle := bsNone;
  FDateEditor.ParentColor := True;
  FDateEditor.Parent := Self;
  FDateEditor.Align := alLeft;

  BorderStyle := bsSingle;
  AutoSize := True;
  FDateEditor.OnChange := @DateEditorChange;
  FDateEditor.OnKeyDown := @DateEditorKeyDown;
  FDateEditor.OnEnter := @DateEditorEnter;
  FDateEditor.OnExit := @DateEditorExit;
  FDateEditor.OnEditingDone := @DateEditorEditingDone;
  FDateEditor.OnClick := @DateEditorClick;
  FDateEditor.OnDblClick := @DateEditorDblClick;
  FDateEditor.OnTripleClick := @DateEditorTripleClick;
  FDateEditor.OnQuadClick := @DateEditorQuadClick;
  FDateEditor.OnKeyUp := @DateEditorKeyUp;
  FDateEditor.OnKeyPress := @DateEditorKeyPress;
  FDateEditor.OnUTF8KeyPress := @DateEditorUTF8KeyPress;
  FDateEditor.OnMouseDown := @DateEditorMouseDown;
  FDateEditor.OnMouseMove := @DateEditorMouseMove;
  FDateEditor.OnMouseUp := @DateEditorMouseUp;
  FDateEditor.OnConfirmChanges := @DateEditorConfirmChanges;

  FShape := nil;
  FCal := nil;
  FCalendarForm := nil;
  FDoNotArrangeControls := True;
  ShowCalendar := True;
  UpdateBaseBounds(True, True, False);
end;

destructor TCustomDatePicker.Destroy;
begin
  FDoNotArrangeControls := True;

  SetShowCalendar(False);
  SetShowCheckBox(False);

  FDateEditor.OnConfirmChanges := nil;
  FDateEditor.OnMouseUp := nil;
  FDateEditor.OnMouseMove := nil;
  FDateEditor.OnMouseDown := nil;
  FDateEditor.OnUTF8KeyPress := nil;
  FDateEditor.OnKeyPress := nil;
  FDateEditor.OnKeyUp := nil;
  FDateEditor.OnQuadClick := nil;
  FDateEditor.OnTripleClick := nil;
  FDateEditor.OnDblClick := nil;
  FDateEditor.OnClick := nil;
  FDateEditor.OnEditingDone := nil;
  FDateEditor.OnExit := nil;
  FDateEditor.OnEnter := nil;
  FDateEditor.OnKeyDown := nil;
  FDateEditor.OnChange := nil;
  FreeThenNil(FDateEditor);

  inherited Destroy;
end;

function TCustomDatePicker.IsNullDate: Boolean;
begin
  Result := FDateEditor.IsNullDate;
end;

end.
