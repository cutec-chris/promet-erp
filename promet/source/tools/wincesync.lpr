program wincesync;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes
  { you can add units after this }
  ,SysUtils
  ,windows
  ,phone
  ,sms;

var
  h,hRead : THandle;
  Ce : CALLLOGENTRY;
  res: LongInt;
  f : textfile;
  sysStartTime : SYSTEMTIME;
  sysEndTime : SYSTEMTIME;
  CallType: String;

  smsH : SMS_HANDLE;

begin
  AssignFile(f,'calllist.txt');
  rewrite(f);
  if (PhoneOpenCallLog(@h) = S_OK) then
    while (true) do
      begin
        ce.cbSize:=sizeof(ce);
        res := PhoneGetCallLogEntry(h,@ce);
        FileTimeToSystemTime(@ce.ftStartTime, @sysStartTime);
        FileTimeToSystemTime(@ce.ftEndTime, @sysEndTime);
        case ce._iom of
        IOM_MISSED:CallType := 'Missed';
        IOM_INCOMING:CallType := 'Incomming';
        IOM_OUTGOING:callType := 'Outgoing';
        end;
        writeln(f,ce.pszName+';'
                 +ce.pszNumber+';'
                 +Format('%.4d-%.2d-%.2d %.2d:%.2d:%.2d',[sysStartTime.Year,sysStartTime.Month,sysStartTime.Day,sysStartTime.Hour,sysStartTime.Minute,sysStartTime.Second])+';'
                 +Format('%.4d-%.2d-%.2d %.2d:%.2d:%.2d',[sysEndTime.Year,sysEndTime.Month,sysEndTime.Day,sysEndTime.Hour,sysEndTime.Minute,sysStartTime.Second])+';'
                 +CallType
                 );
        if res <> S_OK then break;
      end;
  CloseFile(f);
  hRead := CreateEvent(nil, FALSE, FALSE, nil);
  h := SmsOpen (SMS_MSGTYPE_TEXT, SMS_MODE_RECEIVE,@smsH, @hRead);
end.

