program productioncmd;

uses laz_synapse,blcksock,sysutils,tlntsend, general_nogui,Utils,uprometmsgclient
  {$ifdef WINDOWS}
  ,Windows
  {$endif}
  ;

var
  Msg : TPrometMsgClient;
  tmp: String;
  i: Integer;

procedure OnPublish(Topic, Value: string);
begin

end;

begin
  ExitCode:=255;
  Msg := TPrometMsgClient.Create;
  try
    if not Msg.Connected then
      begin
        sleep(200);
        if not Msg.Connected then
          begin
            ExitCode := 252;
            exit;
          end;
      end;
    tmp := '';
    for i := 1 to Paramcount do
      tmp := tmp+' '+ParamStr(i);
    if not Msg.Sub('/'+GetSystemName+'/avad/*') then
      ExitCode:=253;
    Msg.OnPublish:=@OnPublish;
    if not Msg.Pub('/'+GetSystemName+'/avad/execute',tmp) then
      ExitCode:=253;

  finally
    Msg.Free;
  end;

{
  aTL.Timeout:=40000;
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
}
end.

