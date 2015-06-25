 program import_mqtt;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils, CustApp
  { you can add units after this },db,Utils,
  FileUtil,uData, uIntfStrConsts, pcmdprometapp,uBaseCustomApplication,
  MQTT,uBaseDbClasses,uMeasurement,
  uBaseApplication;

type

  { PrometCmdApp }

  PrometCmdApp = class(TBaseCustomApplication)
    procedure mqttcConnAck(Sender: TObject; ReturnCode: integer);
    procedure mqttcPingResp(Sender: TObject);
    procedure mqttcPublish(Sender: TObject; topic, payload: ansistring);
    procedure mqttcSubAck(Sender: TObject; MessageID: integer;
      GrantedQoS: integer);
    procedure mqttcUnSubAck(Sender: TObject; MessageID: integer);
  private
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

{ PrometCmdApp }

procedure PrometCmdApp.mqttcConnAck(Sender: TObject; ReturnCode: integer);
begin
  writeln('Connection Acknowledged, Return Code: ' + IntToStr(Ord(ReturnCode)));
end;

procedure PrometCmdApp.mqttcPingResp(Sender: TObject);
begin
  writeln('PING');
end;

procedure PrometCmdApp.mqttcPublish(Sender: TObject; topic, payload: ansistring
  );
var
  aProp: String;
  anObject: TObjects;
  aMeasurement: TMeasurement;
  PayloadFloat: Extended;
  aObject: String;
begin
  writeln(topic+':'+payload);
  aProp := copy(topic,rpos('/',topic)+1,length(topic));
  topic:=copy(topic,0,length(topic)-(length(aProp)+1));
  if pos(':',aProp)>0 then
    aProp := copy(aProp,0,pos(':',aProp)-1);
  if not TryStrToFloat(payload,PayloadFloat) then
    begin
      if (lowercase(payload)='ok')
      or (lowercase(payload)='true')
      or (lowercase(payload)='y')
      or (lowercase(payload)='on')
      then PayloadFloat:=1
      else
      if (lowercase(payload)='false')
      or (lowercase(payload)='n')
      or (lowercase(payload)='off')
      then PayloadFloat:=0
      else exit;
    end;
  aObject := copy(topic,rpos('/',topic)+1,length(topic));
  anObject := TObjects.Create(nil);
  anObject.SelectFromNumber(aObject);
  anObject.Open;
  if anObject.Count>0 then
    begin
      aMeasurement := TMeasurement.CreateEx(nil,Data,anObject.Connection,anObject.DataSet);
      aMeasurement.Open;
      if aMeasurement.Locate('ID',aProp,[loCaseInsensitive]) then
        begin
          if (aMeasurement.Current.AsFloat<>PayloadFloat) then
            begin
              aMeasurement.Edit;
              aMeasurement.Current.AsFloat:=PayloadFloat;
              aMeasurement.Post;
            end;
        end;
      aMeasurement.Free;
    end;
  anObject.Free;
end;

procedure PrometCmdApp.mqttcSubAck(Sender: TObject; MessageID: integer;
  GrantedQoS: integer);
begin
  writeln('Sub Ack Received');
end;

procedure PrometCmdApp.mqttcUnSubAck(Sender: TObject; MessageID: integer);
begin
  writeln('UnSub Ack Received');
end;

procedure PrometCmdApp.DoRun;
var
  Port: String;
  mqttc: TMQTTClient;
begin
  with BaseApplication as IBaseApplication do
    begin
      AppVersion:={$I ../base/version.inc};
      AppRevision:={$I ../base/revision.inc};
    end;
  if not Login then Terminate;
  //Your logged in here on promet DB
  Port := GetOptionValue('p','port');
  if Port = '' then
    Port := '1883';
  mqttc := TMQTTClient.Create(GetOptionValue('m','mqtt-server'),StrToInt(Port));
  writeln('Connecting ...');
  mqttc.OnPublish:=@mqttcPublish;
  mqttc.OnSubAck:=@mqttcSubAck;
  mqttc.OnConnAck:=@mqttcConnAck;
  mqttc.OnPingResp:=@mqttcPingResp;
  mqttc.OnUnSubAck:=@mqttcUnSubAck;
  if mqttc.Connect then
    begin
      writeln('Connected.');
      sleep(100);
      writeln('Subscribe to /#');
      mqttc.Subscribe('/#');
      while mqttc.isConnected do
        begin
          sleep(30000);
          if not mqttc.PingReq then break;
        end;
    end
  else writeln('Connection failed');
  // stop program loop
  Terminate;
end;

constructor PrometCmdApp.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor PrometCmdApp.Destroy;
begin
  inherited Destroy;
end;

var
  Application: PrometCmdApp;

begin
  Application:=PrometCmdApp.Create(nil);
  Application.Run;
  Application.Free;
end.

