unit ufhem;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,ssl_openssl,blcksock,httpsend;

type
  TInfoEvent = procedure(aInfo : string) of object;
  { TLogThread }

  { TFHEMLogThread }

  TFHEMLogThread = class(TThread)
    procedure FLogSockHeartbeat(Sender: TObject);
    procedure FLogSockStatus(Sender: TObject; Reason: THookSocketReason;
      const Value: String);
  private
    FConnType: string;
    FOnInfo: TInfoEvent;
    FPos : Integer;
    FBuffer : string;
    FInfo : string;
    FList: TStringList;
    FLog: THTTPSend;
    FServer: String;
    procedure Info;
  public
    constructor Create(aServer: string; CreateSuspended: Boolean=False);
    procedure Execute; override;
    procedure Abort;
    function BuildConnStr(aServer: string): string;
    property ConnType : string read FConnType write FConnType;
    property Server : string read FServer;
    property Log : THTTPSend read FLog;
    property OnInfo : TInfoEvent read FOnInfo write FOnInfo;
    property Terminated;
  end;

implementation

{ TLogThread }

procedure TFHEMLogThread.FLogSockHeartbeat(Sender: TObject);
var
  cnt: Integer;
  tmp : string;
begin
  if Terminated then
    begin
      FLog.Abort;
      exit;
    end;
  while FLog.Document.Size > FPos do
    begin
      FLog.Document.Position:=FPos;
      cnt := FLog.Document.Size-FPos;
      if cnt>255 then
        cnt := 255;
      Setlength(tmp,cnt);
      cnt := FLog.Document.Read(tmp[1],cnt);
      FPos := FPos+cnt;
      FBuffer:=FBuffer+copy(tmp,0,cnt);
    end;
  if pos(#10,FBuffer)>0 then
    Synchronize(@Info);
end;

procedure TFHEMLogThread.FLogSockStatus(Sender: TObject; Reason: THookSocketReason;
  const Value: String);
var
  aStr: String;
  tmp : string;
  cnt: Integer;
begin
  if Terminated then
    begin
      FLog.Abort;
      exit;
    end;
  if Reason=HR_CanRead then
    while FLog.Document.Size > FPos do
      begin
        FLog.Document.Position:=FPos;
        cnt := FLog.Document.Size-FPos;
        if cnt>255 then
          cnt := 255;
        Setlength(tmp,cnt);
        cnt := FLog.Document.Read(tmp[1],cnt);
        FPos := FPos+cnt;
        FBuffer:=FBuffer+copy(tmp,0,cnt);
      end;
  if pos(#10,FBuffer)>0 then
    Synchronize(@Info);
end;

procedure TFHEMLogThread.Info;
begin
  while pos(#10,FBuffer)>0 do
    begin
      FInfo := copy(FBuffer,0,pos(#10,FBuffer)-1);
      FBuffer := copy(FBuffer,pos(#10,FBuffer)+1,length(FBuffer));
      if Assigned(FOnInfo) then
        FOnInfo(FInfo);
    end;
end;

constructor TFHEMLogThread.Create(aServer: string;CreateSuspended : Boolean = False);
begin
  FServer := BuildConnStr(aServer);
  FPos := 0;
  FLog := THTTPSend.Create;
  inherited Create(CreateSuspended);
end;

function TFHEMLogThread.BuildConnStr(aServer: string): string;
var
  aConnType: String;
begin
  if pos(':',aServer)=0 then
    aServer := aServer+':8083';
  aConnType := 'http://';
  if pos('://',aServer)>0 then
    aConntype := '';
  Result := aConnType+aServer;
end;

procedure TFHEMLogThread.Execute;
var
  aStr: String;
  url: String;
begin
  url := FServer+'/fhem?XHR=1&inform=type=raw;filter=.*';
  FLog.Sock.OnStatus:=@FLogSockStatus;
  FLog.Sock.OnHeartbeat:=@FLogSockHeartbeat;
  FLog.Timeout:=120000;
  FLog.KeepAlive:=True;
  FLog.Sock.HeartbeatRate:=100;
  while not Terminated do
    begin
      try
        FLog.HTTPMethod('GET',url);
      except
      end;
      FLog.Clear;
    end;
  FLog.Free;
end;

procedure TFHEMLogThread.Abort;
begin
  FLog.Abort;
end;

end.

