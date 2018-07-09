program productioncmd;

uses laz_synapse,blcksock,sysutils,tlntsend, general_nogui,Utils
  {$ifdef WINDOWS}
  ,Windows
  {$endif}
  ;

var
  aSocket: TTCPBlockSocket;
  tmp: String;
  i: Integer;
  aTL: TTelnetSend;
  aRes: String;
begin
  ExitCode:=255;
  tmp := '';
  aTL := TTelnetSend.Create;
  aTL.Timeout:=40000;
  for i := 1 to Paramcount do
    begin
      if copy(ParamStr(i),0,10)='--timeout=' then
        begin
          aTL.Timeout:=StrToIntDef(copy(ParamStr(i),11,length(ParamStr(i))),40)*1000;
        end
      else
        tmp := tmp+' '+ParamStr(i);
    end;
  aTL.TargetHost:='localhost';
  aTL.TargetPort:='9874';
  if not aTL.Login then
    begin
      aTL.Free;
      ExitCode:=253;
      exit;
    end;
  aTL.Send(trim(tmp)+#13#10);
  aRes := aTL.RecvString;
  if IsNumeric(aRes) then
    ExitCode:=StrToInt(aRes)
  else
    begin
      {$ifdef WINDOWS}
      MessageBox(0,PChar(ares),'Fehler wärend Scriptausführung',0);
      {$endif}
      ExitCode:=254;
    end;
  aTL.Free;
end.

