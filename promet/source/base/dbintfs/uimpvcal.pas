{*******************************************************************************
Dieser Sourcecode darf nicht ohne gültige Geheimhaltungsvereinbarung benutzt werden
und ohne gültigen Vertriebspartnervertrag weitergegeben werden.
You have no permission to use this Source without valid NDA
and copy it without valid distribution partner agreement
Christian Ulrich
info@cu-tec.de
Created 01.06.2006
*******************************************************************************}
unit uimpvcal;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, uVTools, uCalendar,lHTTPUtil;
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
begin

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
          vOut.Add('UID:'+FieldByName('SQL_ID').AsString);
          vOut.Add('DTSTAMP:'+BuildISODate(FieldByName('TIMESTAMPD').AsDateTime));
          vOut.Add('DTSTART:'+BuildISODate(FieldByName('STARTDATE').AsDateTime+FieldByName('STARTTIME').AsFloat));
          vOut.Add('DTEND:'+BuildISODate(FieldByName('ENDDATE').AsDateTime+FieldByName('ENDTIME').AsFloat));
          vOut.Add('SUMMARY:'+FieldByName('SUMMARY').AsString);
          if FieldByName('LOCATION').AsString <> '' then
            vOut.Add('LOCATION:'+FieldByName('LOCATION').AsString);
          if FieldByName('DESCR').AsString <> '' then
            vOut.Add('DESCRIPTION:'+FieldByName('DESCR').AsString);
          vOut.Add('END:VEVENT');
          Next;
        end;
      vOut.Add('END:VCALENDAR');
    end;
end;
end.

