unit usbaseserver;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, blcksock, synsock, SyncObjs;

type
  TSConnection= class
  private
    FPeerPort: Integer;
    FPeerName, FPeerIP: string;
  published
    property PeerIP: string read FPeerIP Write FPeerIP;
    property PeerName: string read FPeerName Write FPeerName;
    property PeerPort: Integer read FPeerPort Write FPeerPort;
  end;

  TSTcpThread= class;

  TSThreadEvent= procedure(AThread: TSTcpThread) of object;
  TSThreadExecuteEvent= procedure(AThread: TSTcpThread; const AData: string) of object;

  { TSTcpThread }

  TSTcpThread= class(TThread)
  private
    FId: Integer;
    FSocket: TTCPBlockSocket;
    FConnected, FReadTimedOut, FWriteTimedOut: Boolean;
    FOnDestroy: TNotifyEvent;
    FConnection: TSConnection;
    FOnExecute: TSThreadEvent;
  public
    constructor Create(ASocket: TSocket);
    destructor Destroy; override;
    function ReadLn(const ATimeout: Integer): string;
    procedure WriteLn(const AData: string);
    procedure Disconnect;
    procedure Execute; override;
  published
    property Connected: Boolean read FConnected;
    property Connection: TSConnection read FConnection;
    property Id : Integer read FId;
    property Terminated;
    property ReadTimedOut: Boolean read FReadTimedOut;
    property WriteTimedOut: Boolean read FWriteTimedOut;
    property OnDestroy: TNotifyEvent read FOnDestroy Write FOnDestroy;
    property OnExecute: TSThreadEvent read FOnExecute Write FOnExecute;
  end;

  TSLoginEvent = function(aSocket : TSTcpThread;aUser,aPasswort : string) : Boolean of object;
  TSLogEvent = procedure(aSocket : TSTcpThread;DirectionIn : Boolean;aMessage : string) of object;

  TSTcpListener= class(TThread)
  private
    FSocket: TTCPBlockSocket;
    FOnConnect: TSThreadEvent;
    FPort: Integer;
    FIP: string;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Execute; override;
  published
    property IP: string read FIP write FIP;
    property Port: Integer read FPort Write FPort;
    property OnConnect: TSThreadEvent read FOnConnect Write FOnConnect;
  end;

  { TSBaseServer }

  TSBaseServer= class(TComponent)
  private
    FListenInterface: string;
    FLog: TSLogEvent;
    FLogin: TSLoginEvent;
    FTimeout, FDefaultPort: Integer;
    procedure SetListenInterface(AValue: string);
  protected
    function GetActive: Boolean; virtual; abstract;
    procedure SetActive(const AValue: Boolean); virtual; abstract;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Active: Boolean read GetActive Write SetActive;
    procedure Start;
    property ListenInterface : string read FListenInterface write SetListenInterface;
    property ListenPort: Integer read FDefaultPort Write FDefaultPort;
    property Timeout: Integer read FTimeout Write FTimeout default 60000;
    property OnLogin : TSLoginEvent read FLogin write FLogin;
    property OnLog : TSLogEvent read FLog write FLog;
  end;

  TSTcpServer= class(TSBaseServer)
  private
    FListeners, FThreads: TList;
    FSection: TCriticalSection;
    FOnExecute: TSThreadExecuteEvent;
    FVersion: string;
    function CreateListener(const AIP: string; const APort: Integer): TSTcpListener;
    procedure DoClientCreate(AThread: TSTcpThread);
    procedure DoClientDestroy(ASender: TObject);
  protected
    function GetActive: Boolean; override;
    procedure SetActive(const AValue: Boolean); override;
    procedure Execute(AThread: TSTcpThread); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property OnExecute: TSThreadExecuteEvent read FOnExecute Write FOnExecute;
    property Version: string read FVersion write FVersion;
  end;

implementation

{ TSBaseServer }

procedure TSBaseServer.SetListenInterface(AValue: string);
begin
  if FListenInterface=AValue then Exit;
  FListenInterface:=AValue;
end;

constructor TSBaseServer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ListenInterface:='0.0.0.0';
end;

destructor TSBaseServer.Destroy;
begin
  inherited Destroy;
end;

procedure TSBaseServer.Start;
begin
  Active := True;
end;

{ TSBaseServer }

constructor TSTcpThread.Create(ASocket: TSocket);
begin
  inherited Create(True);
  FConnection:= TSConnection.Create;
  FSocket:= TTCPBlockSocket.Create;
  FSocket.Socket:= ASocket;
  FreeOnTerminate:= True;
  FConnected:= True;
  FReadTimedOut:= False;
  FWriteTimedOut:= False;
end;

destructor TSTcpThread.Destroy;
begin
  if ( Assigned(FOnDestroy) ) then
    FOnDestroy(Self);
  FreeAndNil(FSocket);
  FreeAndNil(FConnection);
  inherited;
end;

function TSTcpThread.ReadLn(const ATimeout: Integer): string;
begin
  if ( FSocket.socket=INVALID_SOCKET ) then begin
    FConnected:= False;
    FReadTimedOut:= False;
  end
  else begin
    FConnected:= True;
    FReadTimedOut:= False;
    Result:= FSocket.RecvString(ATimeout);
    if ( FSocket.socket=INVALID_SOCKET ) then begin
      FConnected:= False;
      FReadTimedOut:= False;
    end
    else begin
      case FSocket.LastError of
        0: ;
        WSAETIMEDOUT: FReadTimedOut:= True;
        else
          FConnected:= False;
      end;
    end;
  end;
end;

procedure TSTcpThread.WriteLn(const AData: string);
begin
  if ( FSocket.socket=INVALID_SOCKET ) then begin
    FConnected:= False;
    FWriteTimedOut:= False;
  end
  else begin
    FSocket.SendString(AData+CRLF);
    if ( FSocket.socket=INVALID_SOCKET ) then begin
      FConnected:= False;
      FWriteTimedOut:= False;
    end
    else begin
      case FSocket.LastError of
        0: ;
        WSAETIMEDOUT: FWriteTimedOut:= True;
      end;
    end;
  end;
end;

procedure TSTcpThread.Disconnect;
begin
  FSocket.CloseSocket;
  FConnected:= False;
  FReadTimedOut:= False;
  FWriteTimedOut:= False;
end;

procedure TSTcpThread.Execute;
begin
  FConnection.PeerIP:= FSocket.GetRemoteSinIP;
  FConnection.PeerName:= FSocket.ResolveIPToName(FSocket.GetRemoteSinIP);
  FConnection.PeerPort:= FSocket.GetRemoteSinPort;
  if ( Assigned(FOnExecute) ) then
    FOnExecute(Self);
end;

constructor TSTcpListener.Create;
begin
  inherited Create(True);
  FSocket:= TTCPBlockSocket.Create;
  FreeOnTerminate:= False;
end;

destructor TSTcpListener.Destroy;
begin
  if ( Suspended ) then
    Resume;
  FreeAndNil(FSocket);
  inherited;
end;

procedure TSTcpListener.Execute;
var
  LClient: TSocket;
  LSynaThread: TSTcpThread;
begin
  with FSocket do begin
    CreateSocket;
    RaiseExcept:= False;
    SetLinger(True, 10);
    Bind(FIP, IntToStr(FPort));
    Listen;
    repeat
      if ( Terminated ) then
        Break
      else if ( CanRead(1000) ) then begin
        if ( (not Terminated) and (not Suspended) ) then begin
          if ( Assigned(FOnConnect) ) then begin
            LClient:= Accept;
            if ( LastError=0 ) then begin
              LSynaThread:= TSTcpThread.Create(LClient);
              FOnConnect(LSynaThread);
            end;
          end;
        end;
      end;
    until false;
  end;
end;

constructor TSTcpServer.Create(AOwner: TComponent);
begin
  inherited;
  FSection:= TCriticalSection.Create;
  FThreads:= TList.Create;
  FListeners:= TList.Create;
end;

destructor TSTcpServer.Destroy;
begin
  if ( GetActive ) then
    SetActive(False);
  FreeAndNil(FThreads);
  FreeAndNil(FListeners);
  FreeAndNil(FSection);
  inherited;
end;

function TSTcpServer.GetActive: Boolean;
begin
  Result:= FListeners.Count>0;
end;

function TSTcpServer.CreateListener(const AIP: string; const APort: Integer): TSTcpListener;
begin
  Result:= TSTcpListener.Create;
  Result.OnConnect:= @DoClientCreate;
  Result.IP:= AIP;
  Result.Port:= APort;
end;

procedure TSTcpServer.SetActive(const AValue: Boolean);
var
  i: Integer;
  LListener: TSTcpListener;
begin
  if ( AValue<>GetActive ) then begin
    if ( AValue ) then begin
      FListeners.Clear;
    LListener:= CreateListener(FListenInterface, FDefaultPort);
    FListeners.Add(LListener);
    for i:= 0 to FListeners.Count-1 do
      TSTcpListener(FListeners[i]).Resume;
    end
    else begin
      for i:= FListeners.Count-1 downto 0 do
        TSTcpListener(FListeners[i]).Terminate;

      for i:= FThreads.Count-1 downto 0 do
        TSTcpThread(FThreads[i]).Disconnect;

      for i:= FListeners.Count-1 downto 0 do begin
        TSTcpThread(FListeners[i]).WaitFor;
        TSTcpThread(FListeners[i]).Free;
      end;

      FListeners.Clear;
    end;
  end;
end;

procedure TSTcpServer.DoClientCreate(AThread: TSTcpThread);
begin
  FSection.Create;
  try
    FThreads.Add(AThread);
  finally
    FSection.Leave;
  end;
  AThread.OnDestroy:= @DoClientDestroy;
  AThread.OnExecute:= @Execute;
  AThread.Resume;
end;

procedure TSTcpServer.DoClientDestroy(ASender: TObject);
var
  LIndex: Integer;
begin
  FSection.Create;
  try
    LIndex:= FThreads.IndexOf(ASender);
    if ( LIndex>=0 ) then
      FThreads.Delete(LIndex);
  finally
    FSection.Leave;
  end;
end;

procedure TSTcpServer.Execute(AThread: TSTcpThread);
var
  LData: string;
begin
  while ( not AThread.Terminated ) do begin
    LData:= AThread.ReadLn(Timeout);
    if ( not (AThread as TSTcpThread).Connected) then
      Break
    else if ( (AThread as TSTcpThread).ReadTimedOut ) then
      Break
    else if ( Assigned(FOnExecute) ) then
      FOnExecute(AThread, LData);
  end;
end;

end.

