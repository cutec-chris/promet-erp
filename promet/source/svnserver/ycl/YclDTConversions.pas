{**************************************************************************************************}
{                                                                                                  }
{  Y core library (Ycl)                                                                            }
{                                                                                                  }
{  YclDTConversions - date / time conversion functions                                             }
{                                                                                                  }
{  The contents of this file are subject to the Y Library Public License Version 1.0 (the          }
{  "License"); you may not use this file except in compliance with the License. You may obtain a   }
{  copy of the License at http://delphi.pjh2.de/                                                   }
{                                                                                                  }
{  Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF  }
{  ANY KIND, either express or implied. See the License for the specific language governing rights }
{  and limitations under the License.                                                              }
{                                                                                                  }
{  The Original Code is: YclDTConversions.pas.                                                     }
{  The Initial Developer of the Original Code is Peter J. Haas (libs@pjh2.de). Portions created    }
{  by Peter J. Haas are Copyright (C) 2001-2005 Peter J. Haas. All Rights Reserved.                }
{                                                                                                  }
{  Contributor(s):                                                                                 }
{                                                                                                  }
{  You may retrieve the latest version of this file at the homepage of Peter J. Haas, located at   }
{  http://delphi.pjh2.de/                                                                          }
{                                                                                                  }
{**************************************************************************************************}

// For history see end of file

// Informations:
//   http://www.hermetic.ch/cal_stud/jdn.htm
//   http://www.decimaltime.hynes.net/dates.html
//   http://en.wikipedia.org/wiki/Julian_day

{$I Ycl.inc}

unit YclDTConversions;

interface
uses
  YclDateTimes;

// UnixTime <--> TDateTime
function UnixTimeToDateTime(const Value: TUnixTime32): TDateTime;
function DateTimeToUnixTime(const Value: TDateTime): TUnixTime32;
function UnixTime64ToDateTime(const Value: TUnixTime64): TDateTime;
function DateTimeToUnixTime64(const Value: TDateTime): TUnixTime64;

// UnixTime <--> FileTime
function UnixTimeToFileTime(const Value: TUnixTime32): TWinFileTime;
function FileTimeToUnixTime(const Value: TWinFileTime): TUnixTime32;
function UnixTime64ToFileTime(const Value: TUnixTime64): TWinFileTime;
function FileTimeToUnixTime64(const Value: TWinFileTime): TUnixTime64;

// FileTime <--> TDateTime
function LocalFileTimeToDateTime(const Value: TWinFileTime): TDateTime;
function DateTimeToLocalFileTime(const Value: TDateTime): TWinFileTime;


// (Astronomical) Julian Day (JD / AJD) <--> TDateTime
function DateTimeToJulianDay(const Value: TDateTime): Double;
function JulianDayToDateTime(const Value: Double): TDateTime;

// Chronological Julian Day (CJD) <--> TDateTime
function DateTimeToChronologicalJulianDay(const Value: TDateTime): Double;
function ChronologicalJulianDayToDateTime(const Value: Double): TDateTime;

// Modified Julian Day (MJD) <--> TDateTime
function DateTimeToModifiedJulianDay(const Value: TDateTime): Double;
function ModifiedJulianDayToDateTime(const Value: Double): TDateTime;

// Reduced Julian Day (RJD) <--> TDateTime
function DateTimeToReducedJulianDay(const Value: TDateTime): Double;
function ReducedJulianDayToDateTime(const Value: Double): TDateTime;

// Dublin Julian Day (DJD) <--> TDateTime
function DateTimeToDublinJulianDay(const Value: TDateTime): Double;
function DublinJulianDayToDateTime(const Value: Double): TDateTime;

// Truncated Julian Day (TJD) <--> TDateTime
function DateTimeToTruncatedJulianDay(const Value: TDateTime): Double;
function TruncatedJulianDayToDateTime(const Value: Double): TDateTime;

// INTEGRAL Julian Day (IJD) <--> TDateTime
function DateTimeToIntegralJulianDay(const Value: TDateTime): Double;
function IntegralJulianDayToDateTime(const Value: Double): TDateTime;

// Lilian Day (LD) <--> TDateTime
function DateTimeToLilianDay(const Value: TDateTime): Double;
function LilianDayToDateTime(const Value: Double): TDateTime;

// ANSI Date <--> TDateTime
function DateTimeToANSIDate(const Value: TDateTime): Double;
function ANSIDateToDateTime(const Value: Double): TDateTime;

// Rata Die <--> TDateTime
function DateTimeToRataDie(const Value: TDateTime): Double;
function RataDieToDateTime(const Value: Double): TDateTime;

// Excel Serial Day <--> TDateTime (since Delphi 2 identical to TDateTime)
function DateTimeToExcelSerialDay(const Value: TDateTime): Double;
function ExcelSerialDayToDateTime(const Value: Double): TDateTime;

// Time part of TDateTime <--> Milliseconds
function TimeToMilliseconds(const Value: TDateTime): LongWord;
function MillisecondsToTime(const Value: LongWord): TDateTime;

const
  // 1970-01-01T00:00:00 in TDateTime
  UnixTimeStart = 25569;

  // 1 second in FileTime resolution
  FileTimeSecond = 1000 * 1000 * 10;
  // 1 day in FileTime resolution: 24 * 60 * 60 * 1000 * 1000 * 10;
  {$IFDEF SUPPORTS_INT64}
  FileTimeDay = 864000000000;
  {$ELSE SUPPORTS_INT64~}
  FileTimeDay = 864000000000.0;
  {$ENDIF ~SUPPORTS_INT64}

  // 1601-01-01T00:00:00 in TDateTime
  FileTimeStart = -109205;
  // Time between 1601-01-01 and 1970-01-01 in FileTime resolution
  FileTimeUnixStart = (UnixTimeStart - FileTimeStart) * FileTimeDay;

  // Start of TDateTime in Chronological Julian Day (CJD)
  // -4712-01-01T00:00:00 (Julian) = 0;  -4712 = 4713 BC
  ChronologicalJulianDayDelta = 693594 {DateDelta} + 1721425;

  // Start of TDateTime in (Astronomical) Julian Day (JD / AJD)
  // -4712-01-01T12:00:00 (Julian) = 0;  -4712 = 4713 BC
  JulianDayDelta = ChronologicalJulianDayDelta - 0.5;

  // Start of TDateTime in Modified Julian Day (MJD)
  // 1858-11-17T00:00:00 = 0
  ModifiedJulianDayDelta = ChronologicalJulianDayDelta - 2400001;  // JD -2400000.5

  // Start of TDateTime in Reduced Julian Day (RJD)
  // 1858-11-16T12:00:00 = 0
  ReducedJulianDayDelta = JulianDayDelta - 2400000;

  // Start of TDateTime in Dublin Julian Day (DJD)
  // 1900-01-01T00:00:00 = 0
  DublinJulianDayDelta = ChronologicalJulianDayDelta - 2415021;  // JD -2415020.5

  // Start of TDateTime in Truncated Julian Day (TJD)
  // 1968-05-24T00:00:00 = 0
  TruncatedJulianDayDelta = ModifiedJulianDayDelta - 40000;  // JD -2440000.5

  // Start of TDateTime in Truncated Julian Day (TJD)
  // 1995-10-19T00:00:00 = 0
  // TruncatedJulianDay2Delta = ModifiedJulianDayDelta - 50000;  // JD -2450000.5

  // Start of TDateTime in INTEGRAL Julian Day (IJD)
  // INTErnational Gamma-Ray Astrophysics Laboratory
  // 2000-01-01T00:00:00 = 0 (TT)
  // 1999-12-31T23:58:55.817 = 0 (UTC)
  IntegralJulianDayDelta = ChronologicalJulianDayDelta - 2451545;  // JD -2451544.5

  // Start of TDateTime in Lilian Day (LD)
  // 1582-10-15T00:00:00 = 1
  LilianDayDelta = ChronologicalJulianDayDelta - 2299160;  // JD -2299159.5
                                                           
  // Start of TDateTime in ANSI Date
  // 1601-01-01T00:00:00 = 0
  AnsiDateDelta = ChronologicalJulianDayDelta - 2305814;  // JD -2305813.5

  // Start of TDateTime in Rata Die
  // 0001-01-01T00:00:00 = 1 = JD 1721425.5
  RataDieDelta = ChronologicalJulianDayDelta - 1721425;  // JD -1721424.5

  // Start of TDateTime in Excel Serial Day
  // 1899-12-31T00:00:00 = 1
  ExcelSerialDayDelta = 0;

implementation

// UnixTime <--> TDateTime
function UnixTimeToDateTime(const Value: TUnixTime32): TDateTime;
begin
  Result := Value;
  Result := Result / SecsPerDay + UnixTimeStart;
end;

function DateTimeToUnixTime(const Value: TDateTime): TUnixTime32;
begin
  Result := Round((Value - UnixTimeStart) * SecsPerDay);
end;

function UnixTime64ToDateTime(const Value: TUnixTime64): TDateTime;
begin
  Result := Value;
  Result := Result / SecsPerDay + UnixTimeStart;
end;

function DateTimeToUnixTime64(const Value: TDateTime): TUnixTime64;
begin
  Result := Round((Value - UnixTimeStart) * SecsPerDay);
end;

// *******************************************************************************************

// UnixTime <--> FileTime
function UnixTimeToFileTime(const Value: TUnixTime32): TWinFileTime;
begin
  {$IFDEF SUPPORTS_INT64}
  Int64(Result) := Int64(Value) * FileTimeSecond + FileTimeUnixStart;
  {$ELSE SUPPORTS_INT64~}
  Comp(Result) := Value;
  Comp(Result) := Comp(Result) * FileTimeSecond + FileTimeUnixStart;
  {$ENDIF ~SUPPORTS_INT64}
end;

function FileTimeToUnixTime(const Value: TWinFileTime): TUnixTime32;
begin
  {$IFDEF SUPPORTS_INT64}
  Result := (Int64(Value) - FileTimeUnixStart) div FileTimeSecond;
  {$ELSE SUPPORTS_INT64~}
  Result := Round((Comp(Value) - FileTimeUnixStart) / FileTimeSecond);
  {$ENDIF ~SUPPORTS_INT64}
end;

function UnixTime64ToFileTime(const Value: TUnixTime64): TWinFileTime;
begin
  {$IFDEF SUPPORTS_INT64}
  Int64(Result) := Value * FileTimeSecond + FileTimeUnixStart;
  {$ELSE SUPPORTS_INT64~}
  Comp(Result) := Value * FileTimeSecond + FileTimeUnixStart;
  {$ENDIF ~SUPPORTS_INT64}
end;

function FileTimeToUnixTime64(const Value: TWinFileTime): TUnixTime64;
begin
  {$IFDEF SUPPORTS_INT64}
  Result := (Int64(Value) - FileTimeUnixStart) div FileTimeSecond;
  {$ELSE SUPPORTS_INT64~}
  Result := Round((Comp(Value) - FileTimeUnixStart) / FileTimeSecond);
  {$ENDIF ~SUPPORTS_INT64}
end;

// *******************************************************************************************

// FileTime <--> TDateTime
function LocalFileTimeToDateTime(const Value: TWinFileTime): TDateTime;
begin
  {$IFDEF SUPPORTS_INT64}
  Result := Int64(Value);
  Result := (Result / FileTimeDay) + FileTimeStart;
  {$ELSE SUPPORTS_INT64~}
  Result := (Comp(Value) / FileTimeDay) + FileTimeStart;
  {$ENDIF ~SUPPORTS_INT64}
end;

function DateTimeToLocalFileTime(const Value: TDateTime): TWinFileTime;
begin
  {$IFDEF SUPPORTS_INT64}
  Int64(Result) := Round((Value - FileTimeStart) * FileTimeDay);
  {$ELSE SUPPORTS_INT64~}
  Comp(Result) := (Value - FileTimeStart) * FileTimeDay;
  {$ENDIF ~SUPPORTS_INT64}
end;

// *******************************************************************************************

// (Astronomical) Julian Day (JD) <--> TDateTime
function DateTimeToJulianDay(const Value: TDateTime): Double;
begin
  Result := Value + JulianDayDelta;
end;

function JulianDayToDateTime(const Value: Double): TDateTime;
begin
  Result := Value - JulianDayDelta;
end;

// *******************************************************************************************

// Chronological Julian Day (CJD) <--> TDateTime
function DateTimeToChronologicalJulianDay(const Value: TDateTime): Double;
begin
  Result := Value + ChronologicalJulianDayDelta;
end;

function ChronologicalJulianDayToDateTime(const Value: Double): TDateTime;
begin
  Result := Value - ChronologicalJulianDayDelta;
end;

// *******************************************************************************************

// Modified Julian Day (MJD) <--> TDateTime
function DateTimeToModifiedJulianDay(const Value: TDateTime): Double;
begin
  Result := Value + ModifiedJulianDayDelta;
end;

function ModifiedJulianDayToDateTime(const Value: Double): TDateTime;
begin
  Result := Value - ModifiedJulianDayDelta;
end;

// Reduced Julian Day (RJD) <--> TDateTime
function DateTimeToReducedJulianDay(const Value: TDateTime): Double;
begin
  Result := Value + ReducedJulianDayDelta;
end;

function ReducedJulianDayToDateTime(const Value: Double): TDateTime;
begin
  Result := Value - ReducedJulianDayDelta;
end;

// *******************************************************************************************

// Dublin Julian Day (DJD) <--> TDateTime
function DateTimeToDublinJulianDay(const Value: TDateTime): Double;
begin
  Result := Value + DublinJulianDayDelta;
end;

function DublinJulianDayToDateTime(const Value: Double): TDateTime;
begin
  Result := Value - DublinJulianDayDelta;
end;

// *******************************************************************************************

// Truncated Julian Day (TJD) <--> TDateTime
function DateTimeToTruncatedJulianDay(const Value: TDateTime): Double;
begin
  Result := Value + TruncatedJulianDayDelta;
end;

function TruncatedJulianDayToDateTime(const Value: Double): TDateTime;
begin
  Result := Value - TruncatedJulianDayDelta;
end;

// *******************************************************************************************

// INTEGRAL Julian Day (IJD) <--> TDateTime
function DateTimeToIntegralJulianDay(const Value: TDateTime): Double;
begin
  Result := Value + IntegralJulianDayDelta;
end;

function IntegralJulianDayToDateTime(const Value: Double): TDateTime;
begin
  Result := Value - IntegralJulianDayDelta;
end;

// *******************************************************************************************

// Lilian Day (LD) <--> TDateTime
function DateTimeToLilianDay(const Value: TDateTime): Double;
begin
  Result := Value + LilianDayDelta;
end;

function LilianDayToDateTime(const Value: Double): TDateTime;
begin
  Result := Value - LilianDayDelta;
end;

// *******************************************************************************************

// ANSI Date <--> TDateTime
function DateTimeToANSIDate(const Value: TDateTime): Double;
begin
  Result := Value + ANSIDateDelta;
end;

function ANSIDateToDateTime(const Value: Double): TDateTime;
begin
  Result := Value - ANSIDateDelta;
end;

// *******************************************************************************************

// Rata Die <--> TDateTime
function DateTimeToRataDie(const Value: TDateTime): Double;
begin
  Result := Value + RataDieDelta;
end;

function RataDieToDateTime(const Value: Double): TDateTime;
begin
  Result := Value - RataDieDelta;
end;

// *******************************************************************************************

// Excel Serial Day <--> TDateTime (since Delphi 2 identical to TDateTime)
function DateTimeToExcelSerialDay(const Value: TDateTime): Double;
begin
  Result := Value + ExcelSerialDayDelta;
end;

function ExcelSerialDayToDateTime(const Value: Double): TDateTime;
begin
  Result := Value - ExcelSerialDayDelta;
end;

// *******************************************************************************************

// Time part of TDateTime <--> Milliseconds
function TimeToMilliseconds(const Value: TDateTime): LongWord;
begin
  Result := Trunc(Frac(Value) * MSecsPerDay);
end;

function MillisecondsToTime(const Value: LongWord): TDateTime;
begin
  Result := Value;
  Result := Result / MSecsPerDay;
end;

// *******************************************************************************************

//  History:
//  2005-03-07, Peter J. Haas
//   - add Reduced Julian Day and Dublin Julian Day
//
//  2005-03-07, Peter J. Haas
//   - some modifications to remove FPC hints
//
//  2005-02-09, Peter J. Haas
//   - YCL version
//   - add TUnixTime32, TUnixTime64
//
//  2001-09-24 Version 1.11, Peter J. Haas
//   - BugFix: UnixTimeToFileTime
//
//  2001-09-23 Version 1.10, Peter J. Haas
//   - add UnixTime <--> TDateTime
//   - add UnixTime <--> TWinFileTime
//   - add Julian Day <--> TDateTime
//   - add Chronological Julian Day <--> TDateTime
//   - add Modified Julian Day <--> TDateTime
//   - add Truncated Julian Day <--> TDateTime
//   - add Lilian Day <--> TDateTime
//   - add TDateTime <--> Milliseconds
//
//  2001-09-17 Version 1.00, Peter J. Haas
//   - First public release

end.
