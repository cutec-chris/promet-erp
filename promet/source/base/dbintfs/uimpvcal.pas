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
  {$ifdef WINDOWS}Windows,{$else}UnixUtil,{$endif}Classes, SysUtils, uVTools, uCalendar,
  utilsDate,utask,uBaseApplication;
function VCalImport(Calendar : TCalendar;vIn : TStrings;IsUTF8 : Boolean = False) : Boolean;
function VCalExport(Calendar : TCalendar;vOut : TStrings) : Boolean;
function VTodoImport(Task : TTaskList;vIn : TStrings;IsUTF8 : Boolean = False) : Boolean;
function VTodoExport(Task : TTaskList;vOut : TStrings) : Boolean;
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
function VCalImport(Calendar: TCalendar; vIn: TStrings;IsUTF8 : Boolean = False): Boolean;
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
              if not Calendar.CanEdit then
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
                  StartTimed := ConvertISODate(GetValue(tmp,IsUTF8))=trunc(ConvertISODate(GetValue(tmp)));
                end
              else if IsField('DTEND',tmp) then
                begin
                  FieldByName('ENDDATE').AsDateTime := GMTToLocalTime(ConvertISODate(GetValue(tmp)));
                  if StartTimed and (ConvertISODate(GetValue(tmp,IsUTF8))=trunc(ConvertISODate(GetValue(tmp)))) then
                    FieldByName('ALLDAY').AsString:='Y';
                end
              else if IsField('SUMMARY',tmp) then
                FieldByName('SUMMARY').AsString := GetValue(tmp,IsUTF8)
              else if IsField('LOCATION',tmp) then
                FieldByName('LOCATION').AsString := GetValue(tmp,IsUTF8)
              else if IsField('LOCATION',tmp) then
                FieldByName('LOCATION').AsString := GetValue(tmp,IsUTF8)
              else if IsField('DESCRIPTION',tmp) then
                FieldByName('DESCR').AsString := GetValue(tmp,IsUTF8)
              else if IsField('CREATED',tmp) then
                FieldByName('CRDATE').AsDateTime := GMTToLocalTime(ConvertISODate(GetValue(tmp,IsUTF8)))
              else if IsField('TRANSP',tmp) then
                begin
                  tmp := GetValue(tmp,IsUTF8);
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
              else if IsField('RRULE',tmp) then
                begin
                  //rtNone=0, rtDaily, rtWeekly, rtMonthlyByDay, rtMonthlyByDate,rtYearlyByDay, rtYearlyByDate, rtCustom
                  if pos('FREQ=DAILY',tmp)>0 then
                    FieldByName('ROTATION').AsInteger:=1
                  else if pos('FREQ=WEEKLY',tmp)>0 then
                    FieldByName('ROTATION').AsInteger:=2
                  else if pos('FREQ=MONTHLY',tmp)>0 then
                    FieldByName('ROTATION').AsInteger:=4
                  else if pos('FREQ=YEARLY',tmp)>0 then
                    FieldByName('ROTATION').AsInteger:=6
                  else FieldByName('ROTATION').AsInteger:=0;
                end
              else
                begin
                  with BaseApplication as IbaseApplication do
                    debug('Field Unknown:'+tmp)
                end;
              ;
            end;
        end;
    end;
  Result := True;
end;
function BuildISODate(aDate : TDateTime;DateOnly : Boolean = False) : string;
begin
  if not DateOnly then
    Result := FormatDateTime('yyyy-mm-dd',aDate)+'T'+FormatDateTime('hh:nn:ss',aDate,WebFormatSettings)+'Z'
  else
    Result := FormatDateTime('yyyy-mm-dd',aDate);
end;
function VCalExport(Calendar: TCalendar; vOut: TStrings): Boolean;
begin
  with Calendar.DataSet do
    begin
      vOut.Add('BEGIN:VCALENDAR');
      vOut.Add('VERSION:2.0');
      vOut.Add('PRODID:http://www.free-erp.de/');
      {
      vOut.Add('BEGIN:VTIMEZONE');
      vOut.Add('TZID:W. Europe Standard Time');
      //Sommerzeit
      vOut.Add('BEGIN:STANDARD');
      vOut.Add('DTSTART:16011028T030000');
      vOut.Add('RRULE:FREQ=YEARLY;BYDAY=-1SU;BYMONTH=10');
      vOut.Add('TZOFFSETFROM:+0200');
      vOut.Add('TZOFFSETTO:+0100');
      vOut.Add('END:STANDARD');
      //Winterzeit
      vOut.Add('BEGIN:DAYLIGHT');
      vOut.Add('DTSTART:16010325T020000');
      vOut.Add('RRULE:FREQ=YEARLY;BYDAY=-1SU;BYMONTH=3');
      vOut.Add('TZOFFSETFROM:+0100');
      vOut.Add('TZOFFSETTO:+0200');
      vOut.Add('END:DAYLIGHT');
      vOut.Add('END:VTIMEZONE');
      }
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
              vOut.Add('DTSTART;VALUE=DATE:'+BuildISODate(FieldByName('STARTDATE').AsDateTime,True));
              vOut.Add('DTEND;VALUE=DATE:'+BuildISODate(FieldByName('ENDDATE').AsDateTime,True));
            end
          else
            begin
              vOut.Add('DTSTART:'+BuildISODate(LocalTimeToGMT(FieldByName('STARTDATE').AsDateTime)));
              vOut.Add('DTEND:'+BuildISODate(LocalTimeToGMT(FieldByName('ENDDATE').AsDateTime)));
            end;
          vOut.Add('SUMMARY:'+SetValue(FieldByName('SUMMARY').AsString));
          if FieldByName('LOCATION').AsString <> '' then
            vOut.Add('LOCATION:'+SetValue(FieldByName('LOCATION').AsString));
          if FieldByName('DESCR').AsString <> '' then
            vOut.Add('DESCRIPTION:'+SetValue(FieldByName('DESCR').AsString));
          if FieldByName('ROTATION').AsInteger <> 0 then
            begin
              case FieldByName('ROTATION').AsInteger of
              1:vOut.Add('RRULE:FREQ=DAILY');
              2:vOut.Add('RRULE:FREQ=WEEKLY');
              4:vOut.Add('RRULE:FREQ=MONTHLY');
              6:vOut.Add('RRULE:FREQ=YEARLY');
              end;
            end;
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

function VTodoImport(Task: TTaskList; vIn: TStrings; IsUTF8: Boolean): Boolean;
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
      with Task do
        begin
          tmp := vIn[i];
          inc(i);
          if IsField('BEGIN',tmp) and (pos('VTODO',Uppercase(tmp))>0) then
            begin
              if not Task.CanEdit then
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
                FieldByName('ORIGIDS').AsString := GetValue(tmp)
              else if IsField('DTSTART',tmp) then
                begin
                  FieldByName('STARTDATE').AsDateTime := GMTToLocalTime(ConvertISODate(GetValue(tmp)));
                  StartTimed := ConvertISODate(GetValue(tmp,IsUTF8))=trunc(ConvertISODate(GetValue(tmp)));
                end
              else if IsField('DUE',tmp) then
                begin
                  FieldByName('DUEDATE').AsDateTime := GMTToLocalTime(ConvertISODate(GetValue(tmp)));
                end
              else if IsField('SUMMARY',tmp) then
                FieldByName('SUMMARY').AsString := GetValue(tmp,IsUTF8)
              else if IsField('DESCRIPTION',tmp) then
                FieldByName('DESC').AsString := GetValue(tmp,IsUTF8)
              else if IsField('CATEGORIES',tmp) then
                FieldByName('CATEGORY').AsString := GetValue(tmp,IsUTF8)
              else if IsField('PRIORITY',tmp) then
                FieldByName('PRIORITY').AsString := GetValue(tmp,IsUTF8)
              else if IsField('COMPLETED',tmp) then
                begin
                  if FieldByName('COMPLETED').AsString<>'Y' then
                    FieldByName('COMPLETED').AsString:='Y';
                  FieldByName('COMPLETEDAT').AsDateTime := GMTToLocalTime(ConvertISODate(GetValue(tmp)));
                end
              else if IsField('STATUS',tmp) then
                begin
                  case GetValue(tmp,IsUTF8) of
                  'IN-PROCESS':
                    begin
                      if FieldByName('STARTEDAT').IsNull then
                        FieldByName('STARTEDAT').AsDateTime:=Now();
                      if FieldByName('COMPLETED').AsString='Y' then
                        FieldByName('COMPLETED').AsString:='N';
                      if FieldByName('NEEDSACTION').AsString<>'N' then
                        FieldByName('NEEDSACTION').AsString:='N';
                    end;
                  'COMPLETED':
                    begin
                      if FieldByName('COMPLETED').AsString<>'Y' then
                        FieldByName('COMPLETED').AsString:='Y';
                      if FieldByName('NEEDSACTION').AsString<>'N' then
                        FieldByName('NEEDSACTION').AsString:='N';
                    end;
                  'NEEDS-ACTION':
                    begin
                      if FieldByName('NEEDSACTION').AsString<>'Y' then
                        FieldByName('NEEDSACTION').AsString:='Y';
                    end;
                  'CANCELLED':
                    begin
                      if FieldByName('COMPLETED').AsString<>'Y' then
                        FieldByName('COMPLETED').AsString:='Y';
                      if FieldByName('NEEDSACTION').AsString<>'N' then
                        FieldByName('NEEDSACTION').AsString:='N';
                    end;
                  end;
                end
              else if IsField('PERCENT-COMPLETE',tmp) then
                begin
                  FieldByName('PERCENT').AsInteger := StrToIntDef(GetValue(tmp,IsUTF8),0);
                  if FieldByName('STARTEDAT').IsNull then
                    FieldByName('STARTEDAT').AsDateTime:=Now();
                end
              else
                begin
                  with BaseApplication as IbaseApplication do
                    debug('Field Unknown:'+tmp)
                end;
            end;
        end;
    end;
  Result := True;
end;

function VTodoExport(Task: TTaskList; vOut: TStrings): Boolean;
begin
  {
  BEGIN:VTODO
  DTSTAMP:19980130T134500Z
  SEQUENCE:2
  UID:uid4@example.com
  ACTION:AUDIO
  TRIGGER:19980403T120000
  ATTACH;FMTTYPE=audio/basic:http://example.com/pub/audio-
   files/ssbanner.aud
  REPEAT:4
  DURATION:PT1H
  END:VTODO
  }
  with Task.DataSet do
    begin
      vOut.Add('BEGIN:VCALENDAR');
      vOut.Add('VERSION:2.0');
      vOut.Add('PRODID:http://www.free-erp.de/');
      First;
      while not EOF do
        begin
          vOut.Add('BEGIN:VTODO');
          if FieldByName('ORIGIDS').IsNull then
            vOut.Add('UID:'+FieldByName('SQL_ID').AsString)
          else
            vOut.Add('UID:'+FieldByName('ORIGIDS').AsString);
          vOut.Add('DTSTAMP:'+BuildISODate(FieldByName('TIMESTAMPD').AsDateTime));
          if FieldByName('STARTDATE').AsString<>'' then
            vOut.Add('DTSTART:'+BuildISODate(LocalTimeToGMT(FieldByName('STARTDATE').AsDateTime)));
          if FieldByName('DUEDATE').AsString<>'' then
            vOut.Add('DUE:'+BuildISODate(LocalTimeToGMT(FieldByName('DUEDATE').AsDateTime)));
          vOut.Add('SUMMARY:'+SetValue(FieldByName('SUMMARY').AsString));
          if FieldByName('DESC').AsString <> '' then
            vOut.Add('DESCRIPTION:'+SetValue(FieldByName('DESC').AsString));
          if FieldByName('CATEGORY').AsString <> '' then
            vOut.Add('CATEGORIES:'+SetValue(FieldByName('CATEGORY').AsString));
          if (FieldByName('COMPLETED').AsString<>'Y') and FieldByName('STARTEDAT').IsNull then
            begin
              if FieldByName('NEEDSACTION').AsString='Y' then
                vOut.Add('STATUS:'+SetValue('NEEDS-ACTION'));
              vOut.Add('PERCENT-COMPLETE:'+SetValue(FieldByName('PERCENT').AsString));
            end
          else if FieldByName('COMPLETED').AsString<>'Y' then
            begin
              if FieldByName('NEEDSACTION').AsString='Y' then
                vOut.Add('STATUS:'+SetValue('NEEDS-ACTION'))
              else
                vOut.Add('STATUS:'+SetValue('IN-PROCESS'));
              vOut.Add('PERCENT-COMPLETE:'+SetValue(FieldByName('PERCENT').AsString));
            end
          else if FieldByName('COMPLETED').AsString='Y' then
            begin
              vOut.Add('STATUS:'+SetValue('COMPLETED'));
              vOut.Add('PERCENT-COMPLETE:'+SetValue('100'));
            end
          else if FieldByName('NEEDSACTION').AsString='Y' then
            vOut.Add('STATUS:'+SetValue('NEEDS-ACTION'));
          vOut.Add('END:VTODO');
          Next;
        end;
      vOut.Add('END:VCALENDAR');
    end;

end;

end.

