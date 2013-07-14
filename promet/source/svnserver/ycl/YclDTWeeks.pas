{**************************************************************************************************}
{                                                                                                  }
{  Y core library (Ycl)                                                                            }
{                                                                                                  }
{  YclDTWeeks - date / time - week functions                                                       }
{                                                                                                  }
{  The contents of this file are subject to the Y Library Public License Version 1.0 (the          }
{  "License"); you may not use this file except in compliance with the License. You may obtain a   }
{  copy of the License at http://delphi.pjh2.de/                                                   }
{                                                                                                  }
{  Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF  }
{  ANY KIND, either express or implied. See the License for the specific language governing rights }
{  and limitations under the License.                                                              }
{                                                                                                  }
{  The Original Code is: YclDTWeeks.pas.                                                           }
{  The Initial Developer of the Original Code is Peter J. Haas (libs@pjh2.de). Portions created    }
{  by Peter J. Haas are Copyright (C) 2000-2005 Peter J. Haas. All Rights Reserved.                }
{                                                                                                  }
{  Contributor(s):                                                                                 }
{    Dr. J. R. Stockton                                                                            }
{                                                                                                  }
{  You may retrieve the latest version of this file at the homepage of Peter J. Haas, located at   }
{  http://delphi.pjh2.de/                                                                          }
{                                                                                                  }
{**************************************************************************************************}

// For history see end of file

{$I Ycl.inc}

unit YclDTWeeks;

interface
uses
  {$IFDEF WIN32}
  Windows,  
  {$ENDIF WIN32}
  SysUtils;

type
  // TWeekDef - Week definition
  // Bit 31..16: reserverd
  // Bit 15.. 8: main definition
  // Bit  7.. 0: sub definition
  //
  // main definition
  //   0 = standard week definition
  //   1 = UK Inland Revenue Tax Weeks
  //
  // sub definition for standard week definition (0)
  //   Bit 3..0: first day of week
  //     1 = Monday
  //     2 = Tuesday
  //     3 = Wednesday
  //     4 = Thursday
  //     5 = Friday
  //     6 = Saturday
  //     7 = Sunday
  //   Bit 7..4: Number of days, thats must contained at least in the first week of the year
  //     1 = Week containing Jan 1st is the first week of that year.
  //     4 = Week containing Jan 4th is the first week of that year.
  //     7 = Week containing Jan 7th is the first week of that year.
  //
  // sub definition for UK Inland Revenue Tax Weeks (1)
  //   there is no sub definition
  TWeekDef = LongWord;

const
  WeekDefDefault = 0;  // Default settings

  WeekDef1stJan = $0010;  // Week containing Jan 1st is the first week of that year.
  WeekDef4thJan = $0040;  // Week containing Jan 4th is the first week of that year.
  WeekDef7thJan = $0070;  // Week containing Jan 7th is the first week of that year.

  WeekDefMon = $0001;  // Monday is the first day of a week.
  WeekDefTue = $0002;  // Tuesday is the first day of a week.
  WeekDefWed = $0003;  // Wednesday is the first day of a week.
  WeekDefThu = $0004;  // Thursday is the first day of a week.
  WeekDefFri = $0005;  // Friday is the first day of a week.
  WeekDefSat = $0006;  // Saturday is the first day of a week.
  WeekDefSun = $0007;  // Sunday is the first day of a week.

  WeekDefSun1stJan  = WeekDefSun + WeekDef1stJan;  // most America, Asia, Africa
  WeekDefMon1stJan  = WeekDefMon + WeekDef1stJan;  // Europe, old
  WeekDefMon4thJan  = WeekDefMon + WeekDef4thJan;  // Europe, ISO
  WeekDefMon7thJan  = WeekDefMon + WeekDef7thJan;
  WeekDefSat1stJan  = WeekDefSat + WeekDef1stJan;  // Arabic

  WeekDefISO8601    = WeekDefMon4thJan;

  WeekDefUKTaxWeeks = $00000100;  // UK Inland Revenue Tax Weeks

var
  // Default week definition = ISO 8601
  DefaultWeekDef: TWeekDef = WeekDefISO8601;

// Get standard week definition
//   the first week of that year must contain FirstWeekMinDays days (1, 4, 7)
//   FirstDayOfWeek: the first day of a week (1..7, 1 = Monday)
function StandardWeekDef(FirstWeekMinDays, FirstDayOfWeek: Integer): TWeekDef;

{$IFDEF WIN32}
// Get week definiton from windows locale setting
// ALCID: the LCID, 0 = Default locale setting

// You should note the informations on
//   http://www.pjh2.de/datetime/weeknumber/l10n.php (german and english)
function GetWinLocaleWeekDef(ALCID: LCID): TWeekDef;
{$ENDIF WIN32}

// Convert a week date to TDateTime
//   AYear: the calendar year to which the calendar week belongs
//   AWeek: the calendar week number of the calendar week within that year (1..52/53)
//   ADay:  the ordinal number within the calendar week (1..7)
//   AWeekDef: the week definition
function WeekToDate(
    WeekYear, Week, WeekDay: Integer;
    WeekDef: TWeekDef{$IFDEF SUPPORTS_DEFAULTPARAMS} = WeekDefDefault{$ENDIF}
  ): TDateTime;

// Convert TDateTime to a week date
//   ADate: the date
//   AYear: the calendar year to which the calendar week belongs
//   AWeek: the calendar week number of the calendar week within that year (1..52/53)
//   ADay:  the ordinal number within the calendar week (1..7)
//   AWeekDef: the week definition
procedure DateToWeek(
    Date: TDateTime; out WeekYear, Week, WeekDay: Integer;
    WeekDef: TWeekDef{$IFDEF SUPPORTS_DEFAULTPARAMS} = WeekDefDefault{$ENDIF}
  );

// Convert a week date to a ISO 8601 week date string
function WeekToISOWeekStr(
    WeekYear, Week: Integer;
    WeekDay: Integer{$IFDEF SUPPORTS_DEFAULTPARAMS} = 0{$ENDIF};
    ExtendedFormat: Boolean{$IFDEF SUPPORTS_DEFAULTPARAMS} = True{$ENDIF}
  ): String;

// Convert a date to a ISO 8601 week date string
function DateToISOWeekStr(
    Date: TDateTime;
    WithDay: Boolean{$IFDEF SUPPORTS_DEFAULTPARAMS} = True{$ENDIF};
    ExtendedFormat: Boolean{$IFDEF SUPPORTS_DEFAULTPARAMS} = True{$ENDIF}
  ): String;

implementation
uses
  YclDateTimes, YclDTConversions;

resourcestring
{$I YclDTWeeks.rs}

procedure ConvertErrorFmt(ResString: PResStringRec; const Args: array of const);
begin
  raise EConvertError.CreateFmt(LoadResString(ResString), Args);
end;

procedure GetStandardDefFromWeekDef(
    WeekDef: TWeekDef;
    out FirstWeekMinDays, FirstDayOfWeek: Integer
  );
begin
  FirstWeekMinDays := (WeekDef shr 4) and $F;
  FirstDayOfWeek := WeekDef and $F;
end;

// Check the definition
procedure CheckWeekDef(var WeekDef: TWeekDef);
var
  FirstWeekMinDays, FirstDayOfWeek: Integer;
begin
  // set default value
  if WeekDef = WeekDefDefault then
    WeekDef := DefaultWeekDef;
  // check definition
  case WeekDef shr 8 of
    0: begin  // standard week definition
      GetStandardDefFromWeekDef(WeekDef, FirstWeekMinDays, FirstDayOfWeek);
      if (1 <= FirstWeekMinDays) and (FirstWeekMinDays <= 7) and
         (1 <= FirstDayOfWeek) and (FirstDayOfWeek <= 7) then
        Exit;
    end;
    1:       // UK Inland Revenue Tax Weeks
      Exit;
  end;
  ConvertErrorFmt(@SInvalidWeekDef, [WeekDef]);
end;

// Get standard week definition
//   the first week of that year must contain FirstWeekMinDays days (1, 4, 7)
//   FirstDayOfWeek: the first day of a week (1..7, 1 = Monday)
function StandardWeekDef(FirstWeekMinDays, FirstDayOfWeek: Integer): TWeekDef;
begin
  Result := (FirstWeekMinDays shl 4) or FirstDayOfWeek;
  CheckWeekDef(Result);
end;

{$IFDEF WIN32}
// Get week definiton from windows locale setting
// ALCID: the LCID, 0 = Default locale setting

// You should note the informations on
//   http://www.pjh2.de/datetime/weeknumber/l10n.php (german and english)
function GetWinLocaleWeekDef(ALCID: LCID): TWeekDef;

{$IFDEF FPC}
{ TODO : entfernen, wenn in FPC veröffentlicht }
function GetLocaleChar(Locale, LocaleType: Integer; Default: Char): Char;
var
  Buffer: array[0..1] of Char;
begin
  Buffer[0] := #0;
  if GetLocaleInfo(Locale, LocaleType, Buffer, Length(Buffer)) > 0 then
    Result := Buffer[0]
  else
    Result := Default;
end;
{$ENDIF FPC}

const
  // 0 Week containing 1/1 is the first week of that year.
  // 1 First full week following 1/1 is the first week of that year.
  // 2 First week containing at least four days is the first week of that year.
  CFirstWeekMinDays: array['0'..'2'] of Integer = (1, 7, 4);
var
  FirstWeekMinDays, FirstDayOfWeek: Char;
begin
  FirstWeekMinDays := GetLocaleChar(ALCID, LOCALE_IFIRSTWEEKOFYEAR, '2');
  FirstDayOfWeek := GetLocaleChar(ALCID, LOCALE_IFIRSTDAYOFWEEK, '0');

  if (FirstWeekMinDays in ['0'..'2']) and (FirstDayOfWeek in ['0'..'6']) then
    Result := StandardWeekDef(
        CFirstWeekMinDays[FirstWeekMinDays], Ord(FirstDayOfWeek) - Ord('0') + 1)
  else
    Result := WeekDefDefault;
end;
{$ENDIF WIN32}

// Convert a week date to TDateTime
//   AYear: the calendar year to which the calendar week belongs
//   AWeek: the calendar week number of the calendar week within that year (1..52/53)
//   ADay:  the ordinal number within the calendar week (1..7)
//   AWeekDef: the week definition
function WeekToDate(
    WeekYear, Week, WeekDay: Integer;
    WeekDef: TWeekDef{$IFDEF SUPPORTS_DEFAULTPARAMS} = WeekDefDefault{$ENDIF}
  ): TDateTime;

  procedure StandardWeeks;
  var
    FirstWeekMinDays, FirstDayOfWeek: Integer;
    Offset: Integer;
  begin
    GetStandardDefFromWeekDef(WeekDef, FirstWeekMinDays, FirstDayOfWeek);
    Result := EncodeDate(Word(WeekYear), 1, Word(FirstWeekMinDays));
    Offset := (Week - 1) * 7 -
        ((Trunc(Result) + ChronologicalJulianDayDelta + 8 - FirstDayOfWeek) mod 7) + WeekDay - 1;
    Result := Result + Offset;
    // prüfen auf gültige Werte
    if (1 > Week) or (Week > 53) or (1 > WeekDay) or (WeekDay > 7) or
       (Offset >= YearDays(WeekYear)) then
      ConvertErrorFmt(@SInvalidWeekDate, [WeekYear, Week, WeekDay]);
  end;

  procedure UKTaxWeeks;
  begin
    Result := EncodeDate(Word(WeekYear), 4, 6) + (Week - 1) * 7 + WeekDay - 1;
    if (1 > Week) or (Week > 53) or (1 > WeekDay) or (WeekDay > 7) or
       (Result >= EncodeDate(WeekYear + 1, 4, 6)) then
      ConvertErrorFmt(@SInvalidUKTaxWeekDate, [WeekYear, Week, WeekDay]);
  end;

begin
  Result := 0;
  CheckWeekDef(WeekDef);
  case WeekDef shr 8 of
    0: StandardWeeks;
    1: UKTaxWeeks;
  end;
end;

// Convert TDateTime to a week date
//   ADate: the date
//   AYear: the calendar year to which the calendar week belongs
//   AWeek: the calendar week number of the calendar week within that year (1..52/53)
//   ADay:  the ordinal number within the calendar week (1..7)
//   AWeekDef: the week definition
procedure DateToWeek(
    Date: TDateTime; out WeekYear, Week, WeekDay: Integer;
    WeekDef: TWeekDef{$IFDEF SUPPORTS_DEFAULTPARAMS} = WeekDefDefault{$ENDIF}
  );

  procedure StandardWeeks;
  var
    FirstWeekMinDays, FirstDayOfWeek: Integer;
    CalendarYear, CalendarMonth, CalendarDay: Word;
  begin
    GetStandardDefFromWeekDef(WeekDef, FirstWeekMinDays, FirstDayOfWeek);
    WeekDay := ((Trunc(Date) + ChronologicalJulianDayDelta + 8 - FirstDayOfWeek) mod 7) + 1;
    Date := Date - WeekDay + 8 - FirstWeekMinDays;
    DecodeDate(Date, CalendarYear, CalendarMonth, CalendarDay);
    WeekYear := CalendarYear;
    Week := (Trunc(Date - EncodeDate(Word(WeekYear), 1, 1)) div 7) + 1;
  end;

  procedure UKTaxWeeks;
  var
    CalendarYear, CalendarMonth, CalendarDay: Word;
    Offset: Integer;
  begin
    DecodeDate(Date, CalendarYear, CalendarMonth, CalendarDay);
    if (CalendarMonth < 4) or ((CalendarMonth = 4) and (CalendarDay < 6)) then
      Dec(CalendarYear);
    Offset := Trunc(Date - EncodeDate(CalendarYear, 4, 6));
    WeekYear := CalendarYear;
    Week := Offset div 7 + 1;
    WeekDay := Offset mod 7 + 1;
  end;

begin
  CheckWeekDef(WeekDef);
  case WeekDef shr 8 of
    0: StandardWeeks;
    1: UKTaxWeeks;
  end;
end;

// Convert a week date to a ISO 8601 week date string
function WeekToISOWeekStr(
    WeekYear, Week: Integer;
    WeekDay: Integer{$IFDEF SUPPORTS_DEFAULTPARAMS} = 0{$ENDIF};
    ExtendedFormat: Boolean{$IFDEF SUPPORTS_DEFAULTPARAMS} = True{$ENDIF}
  ): String;
const
  CFormat: array[Boolean, Boolean] of String =
    (('%.4dW%.2d', '%.4d-W%.2d'), ('%.4dW%.2d%.1d', '%.4d-W%.2d-%.1d'));
begin
  Result := Format(CFormat[WeekDay <> 0, ExtendedFormat], [WeekYear, Week, WeekDay]);
end;

// Convert a date to a ISO 8601 week date string
function DateToISOWeekStr(
    Date: TDateTime;
    WithDay: Boolean{$IFDEF SUPPORTS_DEFAULTPARAMS} = True{$ENDIF};
    ExtendedFormat: Boolean{$IFDEF SUPPORTS_DEFAULTPARAMS} = True{$ENDIF}
  ): String;
var
  WeekYear, Week, WeekDay: Integer;
begin
  DateToWeek(Date, WeekYear, Week, WeekDay, WeekDefISO8601);
  if not WithDay then
    WeekDay := 0;
  Result := WeekToISOWeekStr(WeekYear, Week, WeekDay, ExtendedFormat);
end;

// *******************************************************************************************

//  History:
//  2005-02-19, Peter J. Haas
//   - YCL version
//   - use ChronologicalJulianDayDelta from YclDTConversions
//   - some cosmetic modifications
//
//  2002-01-25, Peter J. Haas
//   - DTWeeks.pas 
//   - add week definition
//   - add UK Inland Revenue Tax Weeks
//
//  2000-09-10, Peter J. Haas
//   - First public version (DTWeek.pas)

end.
