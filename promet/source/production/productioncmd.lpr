program productioncmd;

uses laz_synapse,blcksock,sysutils,tlntsend, general_nogui,Utils;

var
  aSocket: TTCPBlockSocket;
  tmp: String;
  i: Integer;
  aTL: TTelnetSend;
  aRes: String;
begin
  ExitCode:=255;
  aTL := TTelnetSend.Create;
  aTL.TargetHost:='localhost';
  aTL.TargetPort:='9874';
  if not aTL.Login then
    begin
      aTL.Free;
      ExitCode:=253;
      exit;
    end;
  aTL.Timeout:=20000;
  tmp := '';
  for i := 1 to Paramcount do
    tmp := tmp+' '+ParamStr(i);
  aTL.Send(trim(tmp)+#13#10);
  aRes := aTL.RecvString;
  if IsNumeric(aRes) then
    ExitCode:=StrToInt(aRes)
  else ExitCode:=254;
  aTL.Free;
end.

