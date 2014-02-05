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
unit uimpvcal;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, uVTools, uCalendar,lHTTPUtil,LCLProc;
function VCalImport(Calendar : TCalendar;vIn : TStrings) : Boolean;
function VCalExport(Calendar : TCalendar;vOut : TStrings) : Boolean;
implementation
const
  WebFormatSettings : TFormatSettings = (
    CurrencyFormat: 1;
    NegCurrFormat: 5;
    ThousandSeparator: ',';
    DecimalSeparator: '.';
    CurrencyDecimals: 2;
    DateSeparator: '-';
    TimeSeparator: ':';
    ListSeparator: ',';
    CurrencyString: '$';
    ShortDateFormat: 'd/m/y';
    LongDateFormat: 'dd" "mmmm" "yyyy';
    TimeAMString: 'AM';
    TimePMString: 'PM';
    ShortTimeFormat: 'hh:nn';
    LongTimeFormat: 'hh:nn:ss';
    ShortMonthNames: ('Jan','Feb','Mar','Apr','May','Jun',
                      'Jul','Aug','Sep','Oct','Nov','Dec');
    LongMonthNames: ('January','February','March','April','May','June',
                     'July','August','September','October','November','December');
    ShortDayNames: ('Sun','Mon','Tue','Wed','Thu','Fri','Sat');
    LongDayNames:  ('Sunday','Monday','Tuesday','Wednesday','Thursday','Friday','Saturday');
    TwoDigitYearCenturyWindow: 50;
  );
function VCalImport(Calendar: TCalendar; vIn: TStrings): Boolean;
  function ConvertISODate(Str : string; ReturnUTC: Boolean = True) : TDateTime;
  var
    y, m, d: Word;
    h, n, s: Word;
    p: Integer;
    Sfrac: string;
    V: Double;
    Code: Integer;
  begin
    //Date
    if Copy(Str, 1, 1) = '-' then
      Delete(Str, 1, 1);
    y := StrToInt(Copy(Str, 1, 4));
    Str := copy(Str,5,length(Str));
    if Copy(Str, 1, 1) = '-' then
      Delete(Str, 1, 1);
    m := StrToInt(Copy(Str, 1, 2));
    Str := copy(Str,3,length(Str));
    if Copy(Str, 1, 1) = '-' then
      Delete(Str, 1, 1);
    d := StrToInt(Copy(Str, 1, 2));
    Result := EncodeDate(y, m, d);
    //Time
    p := Pos('T', Str);
    if p = 0 then
      exit;
    Delete(Str, 1, p); // '11:47:00.0000000+01:00'
    h := StrToInt(Copy(Str, 1, 2));
    Str := copy(Str,3,length(Str));
    if Copy(Str, 1, 1) = ':' then
      Delete(Str, 1, 1);
    n := StrToInt(Copy(Str, 1, 2));
    Str := copy(Str,3,length(Str));
    if Copy(Str, 1, 1) = ':' then
      Delete(Str, 1, 1);
    s := StrToInt(Copy(Str, 1, 2));
    Str := copy(Str,3,length(Str));
    if h = 24 then
      begin
        Result := Result + 1;
        h := 0;
      end;
    Result := Result + EncodeTime(h, n, s, 0);
    // fractional seconds // '.0000000+01:00'
    if Copy(Str, 1, 1) = '.' then
      begin
        Delete(Str, 1, 1); // '0000000+01:00'
        p := 0;
        while (p < Length(Str)) and (Str[p+1] in ['0'..'9']) do
          Inc(p);
        if p>0 then
          begin
            Sfrac := '0.' + Copy(Str, 1, p); // '0.0000000'
            Delete(Str, 1, p); // '+01:00' eller 'Z'
            Val(Sfrac, V, Code);
            if Code = 0 then
            Result := Result + (V / SysUtils.SecsPerDay);
          end;
      end;
    if ReturnUTC then
      begin
        //Hvis der skal bruges
        // timezone // '+01:00' eller 'Z'
        if Copy(Str, 1, 1) = 'Z' then
          Exit;
        if (Copy(Str, 1, 1) = '-') or (Copy(Str, 1, 1) = '+') then
          begin
            h := StrToInt(Copy(Str, 2, 2));
            n := StrToInt(Copy(Str, 5, 2));
            s := 0;
            if (Copy(Str, 1, 1) = '-') then
              Result := Result + EncodeTime(h, n, s, 0);
            if (Copy(Str, 1, 1) = '+') then
              Result := Result - EncodeTime(h, n, s, 0);
          end;
      end;
  end;
var
  i: Integer;
  tmp: String;
  InBody: Boolean = false;
  StartTimed: Boolean;
begin
  Result := False;
  i := 0;
  while i < vIn.Count do
    begin
      with Calendar do
        begin
          tmp := vIn[i];
          inc(i);
          if IsField('BEGIN',tmp) and (pos('VEVENT',Uppercase(tmp))>0) then
            begin
              Append;
              InBody := True
            end
          else if IsField('END',tmp) and InBody then
            begin
              Post;
              InBody := False
            end
          else if InBody then
            begin
              if IsField('UID',tmp) then
                FieldByName('ORIGID').AsString := GetValue(tmp)
              else if IsField('DTSTART',tmp) then
                begin
                  FieldByName('STARTDATE').AsDateTime := GMTToLocalTime(ConvertISODate(GetValue(tmp)));
                  StartTimed := ConvertISODate(GetValue(tmp))=trunc(ConvertISODate(GetValue(tmp)));
                end
              else if IsField('DTEND',tmp) then
                begin
                  FieldByName('ENDDATE').AsDateTime := GMTToLocalTime(ConvertISODate(GetValue(tmp)));
                  if StartTimed and (ConvertISODate(GetValue(tmp))=trunc(ConvertISODate(GetValue(tmp)))) then
                    FieldByName('ALLDAY').AsString:='Y';
                end
              else if IsField('SUMMARY',tmp) then
                FieldByName('SUMMARY').AsString := GetValue(tmp)
              else if IsField('LOCATION',tmp) then
                FieldByName('LOCATION').AsString := GetValue(tmp)
              else if IsField('LOCATION',tmp) then
                FieldByName('LOCATION').AsString := GetValue(tmp)
              else if IsField('DESCRIPTION',tmp) then
                FieldByName('DESCR').AsString := GetValue(tmp)
              else if IsField('CREATED',tmp) then
                FieldByName('CRDATE').AsDateTime := GMTToLocalTime(ConvertISODate(GetValue(tmp)))
              else if IsField('TRANSP',tmp) then
                begin
                  tmp := GetValue(tmp);
                  case tmp of
                  'TRANSPARENT':tmp := 'T';
                  'OPAQUE':tmp := 'O';
                  else tmp := '';
                  end;
                  FieldByName('BUSYTYPE').Asstring := tmp;
                end
              else if IsField('STATUS',tmp) then
                begin
                  tmp := GetValue(tmp);
                  case tmp of
                  'TENTATIVE':tmp := 'T';
                  'CONFIRMED':tmp := 'C';
                  'CANCELLED':tmp := 'X';
                  else tmp := '';
                  end;
                  FieldByName('STATUS').Asstring := tmp;
                end
              else debugln('Field Unknown:'+tmp);
            end;
        end;
    end;
  Result := True;
end;
function VCalExport(Calendar: TCalendar; vOut: TStrings): Boolean;
  function BuildISODate(aDate : TDateTime) : string;
  begin
    Result := FormatDateTime('yyyy-mm-dd',aDate)+'T'+FormatDateTime('hh:nn:ss',LocalTimeToGMT(aDate),WebFormatSettings)+'Z';
  end;
begin
  with Calendar.DataSet do
    begin
      vOut.Add('BEGIN:VCALENDAR');
      vOut.Add('VERSION:2.0');
      vOut.Add('PRODID:http://www.free-erp.de/');
      First;
      while not EOF do
        begin
          vOut.Add('BEGIN:VEVENT');
          if FieldByName('ORIGID').IsNull then
            vOut.Add('UID:'+FieldByName('SQL_ID').AsString)
          else
            vOut.Add('UID:'+FieldByName('ORIGID').AsString);
          vOut.Add('DTSTAMP:'+BuildISODate(FieldByName('TIMESTAMPD').AsDateTime));
          if FieldByName('ALLDAY').AsString='Y' then
            begin
              vOut.Add('DTSTART:'+BuildISODate(trunc(FieldByName('STARTDATE').AsDateTime)));
              vOut.Add('DTEND:'+BuildISODate(trunc(FieldByName('ENDDATE').AsDateTime)));
            end
          else
            begin
              vOut.Add('DTSTART:'+BuildISODate(FieldByName('STARTDATE').AsDateTime));
              vOut.Add('DTEND:'+BuildISODate(FieldByName('ENDDATE').AsDateTime));
            end;
          vOut.Add('SUMMARY:'+FieldByName('SUMMARY').AsString);
          if FieldByName('LOCATION').AsString <> '' then
            vOut.Add('LOCATION:'+FieldByName('LOCATION').AsString);
          if FieldByName('DESCR').AsString <> '' then
            vOut.Add('DESCRIPTION:'+FieldByName('DESCR').AsString);
          case trim(FieldByName('BUSYTYPE').AsString) of
          'T':vOut.Add('TRANSP:TRANSPARENT');
          'O':vOut.Add('TRANSP:OPAQUE');
          end;
          case trim(FieldByName('STATUS').AsString) of
          'T':vOut.Add('STATUS:TENTATIVE');
          'C':vOut.Add('STATUS:CONFIRMED');
          'X':vOut.Add('STATUS:CANCELLED');
          end;
          vOut.Add('END:VEVENT');
          Next;
        end;
      vOut.Add('END:VCALENDAR');
    end;
end;
end.

