program productioncmd;

uses laz_synapse,blcksock,sysutils,tlntsend;

var
  aSocket: TTCPBlockSocket;
  tmp: String;
  i: Integer;
  aTL: TTelnetSend;
begin
  aTL := TTelnetSend.Create;
  aTL.TargetHost:='localhost';
  aTL.TargetPort:='9874';
  aTL.Login;
  tmp := '';
  for i := 1 to Paramcount do
    tmp := tmp+' '+ParamStr(i);
  aTL.Send(trim(tmp)+#13#10);
  aTL.Free;
end.

