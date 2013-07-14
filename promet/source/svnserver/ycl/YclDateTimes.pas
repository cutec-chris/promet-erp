{**************************************************************************************************}
{                                                                                                  }
{  Y core library (Ycl)                                                                            }
{                                                                                                  }
{  YclDateTimes - date / time utility functions                                                    }
{                                                                                                  }
{  The contents of this file are subject to the Y Library Public License Version 1.0 (the          }
{  "License"); you may not use this file except in compliance with the License. You may obtain a   }
{  copy of the License at http://delphi.pjh2.de/                                                   }
{                                                                                                  }
{  Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF  }
{  ANY KIND, either express or implied. See the License for the specific language governing rights }
{  and limitations under the License.                                                              }
{                                                                                                  }
{  The Original Code is: YclDateTimes.pas.                                                         }
{  The Initial Developer of the Original Code is Peter J. Haas (libs@pjh2.de). Portions created    }
{  by Peter J. Haas are Copyright (C) 1998-2005 Peter J. Haas. All Rights Reserved.                }
{                                                                                                  }
{  Contributor(s):                                                                                 }
{                                                                                                  }
{  You may retrieve the latest version of this file at the homepage of Peter J. Haas, located at   }
{  http://delphi.pjh2.de/                                                                          }
{                                                                                                  }
{**************************************************************************************************}

// For history see end of file

{$I Ycl.inc}

unit YclDateTimes;

interface
uses
  {$IFDEF WIN32}
  Windows;
  {$ELSE WIN32~}
  YclBase;
  {$ENDIF ~WIN32}

type
  // Delphi 2, 3:  Unix Time (32 bit) valid in
  //   1970-01-01T00:00:00 (0) .. 2038-01-19T03:14:07 (2^31 - 1)
  // Delphi 4 up:  Unix Time (32 bit) valid in
  //   1970-01-01T00:00:00 (0) .. 2106-02-07T06:28:15 (2^32 - 1)
  TUnixTime32 = LongWord;
  // Delphi 4 up:  Unix Time (64 bit) valid in about 292 * 10^9 years in past and future 
  //   ?(-2^64) .. ?(2^64 - 1)  (1970-01-01T00:00:00 = 0)
  TUnixTime64 = Int64;

  {$IFDEF WIN32}
  TWinFileTime = Windows.FileTime;
  {$ELSE WIN32~}
  TWinFileTime = UInt64;
  {$ENDIF ~WIN32}

// Get the number of days in the specified year
function YearDays(Year: Integer): Integer;

const
  SecsPerMin  = 60;
  SecsPerHour = SecsPerMin * 60;
  SecsPerDay  = SecsPerHour * 24;
  MSecsPerDay = SecsPerDay * 1000;

// setzt vierstellige Jahreszahl in ShortDateFormat
procedure SetFourDigitYearFormat;

function DateAddMonth(Date: TDateTime; MonthCount: Integer): TDateTime;

// gibt True zurück, wenn Date2 >= Date1
function GetDateDifferenz(Date1, Date2: TDateTime; out Years, Months, Days: Cardinal): Boolean;

function CalcAge(Birthdate, Date: TDateTime): Integer;

implementation
uses
  SysUtils,
  YclUtils;
  
function YearDays(Year: Integer): Integer;
const
  CYearDays: array[Boolean] of Integer = (365, 366);
begin
  { TODO : durch eigene Methode ersetzen }
  Result := CYearDays[IsLeapYear(Word(Year))];
end;

procedure SetFourDigitYearFormat;
var
  i: Integer;
begin
  ShortDateFormat := AnsiUpperCase(ShortDateFormat);
  i := Pos('YYYY', ShortDateFormat);
  if i < 1 then begin
    i := Pos('YY', ShortDateFormat);
    if i > 0 then
      Insert('YY', ShortDateFormat, i);
  end;
end;

function DateAddMonth(Date: TDateTime; MonthCount: Integer): TDateTime;
var
  Year, Month, Day: Word;
  YearCount, MonthIndex: Cardinal;
begin
  DecodeDate(Date, Year, Month, Day);
  if MonthCount > 0 then
    DivMod(Month - 1 + MonthCount, 12, YearCount, MonthIndex)
  else
    IDivPMod(Month - 1 + MonthCount, 12, Integer(YearCount), MonthIndex);
  Result := EncodeDate(Year + YearCount, MonthIndex + 1, Day);
end;

// gibt True zurück, wenn Date2 >= Date1
function GetDateDifferenz(Date1, Date2: TDateTime; out Years, Months, Days: Cardinal): Boolean;
var
  YearFrom, MonthFrom, DayFrom: Word;
  YearTo, MonthTo, DayTo: Word;
  iYears, iMonths, iDays: Integer;
begin 
  Result := Date2 >= Date1;
  if not Result then
    SwapVar(Double(Date1), Double(Date2));
  DecodeDate(Date2, YearFrom, MonthFrom, DayFrom);
  DecodeDate(Date1, YearTo, MonthTo, DayTo);
  iYears := YearTo - YearFrom;
  iMonths := MonthTo - MonthFrom;
  iDays := DayTo - DayFrom;
  if iDays < 0 then begin
    Inc(iDays, MonthDays[IsLeapYear(YearTo), MonthFrom]);
    Dec(iMonths);
  end; 
  if iMonths < 0 then begin
    Inc(iMonths, 12);
    Dec(iYears);
  end;
  Years := iYears;
  Months := iMonths;
  Days := iDays;
end;

function CalcAge(Birthdate, Date: TDateTime): Integer;
var
  YearBD, MonthBD, DayBD: Word;
  Year, Month, Day: Word;
  Months, Days: Integer;
begin
  DecodeDate(Birthdate, YearBD, MonthBD, DayBD);
  DecodeDate(Date, Year, Month, Day);
  Result := Year - YearBD;
  Months := Month - MonthBD;
  Days := Day - DayBD;
  if Days < 0 then
    Dec(Months);
  if Months < 0 then
    Dec(Result);
end;

// *******************************************************************************************

//  History:
//  2005-08-17, Peter J. Haas
//   - add AddMonth, GetDateDifferenz
//
//  2005-07-08, Peter J. Haas
//   - add SetFourDigitYearFormat (from other unit)
//
//  2005-03-07, Peter J. Haas
//   - some modifications to remove FPC hints
//
//  2005-02-19, Peter J. Haas
//   - add YearDays from DTWeeks.pas
//
//  2005-02-09, Peter J. Haas
//   - YCL version
//
//  2000-04-11, Peter J. Haas
//   - original version of GetDateDifferenz
//
//  1998-10-11, Peter J. Haas
//   - SetFourDigitYearFormat

end.
