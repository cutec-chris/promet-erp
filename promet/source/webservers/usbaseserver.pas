unit usbaseserver;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, blcksock, synsock, SyncObjs;

type
  TSConnection = class
  private
    FPeerPort: integer;
    FPeerName, FPeerIP: string;
  published
    property PeerIP: string read FPeerIP write FPeerIP;
    property PeerName: string read FPeerName write FPeerName;
    property PeerPort: integer read FPeerPort write FPeerPort;
  end;

  TSTcpThread = class;

  TSThreadEvent = procedure(AThread: TSTcpThread) of object;
  TSThreadExecuteEvent = procedure(AThread: TSTcpThread; const AData: string) of object;

  { TSTcpThread }

  TSTcpThread = class(TThread)
  private
    FId: integer;
    FOnException: TNotifyEvent;
    FSocket: TTCPBlockSocket;
    FConnected, FReadTimedOut, FWriteTimedOut: boolean;
    FUser: string;
    FOnDestroy: TNotifyEvent;
    FConnection: TSConnection;
    FOnExecute: TSThreadEvent;
  public
    constructor Create(ASocket: TSocket);
    destructor Destroy; override;
    function ReadLn(const ATimeout: integer): string;
    function ReadPacket(const ATimeout : integer) : string;
    procedure WriteLn(const AData: string);
    procedure Write(const AData: string);
    procedure Disconnect;
    procedure Execute; override;
  published
    property Connected: boolean read FConnected;
    property Connection: TSConnection read FConnection;
    property Id: integer read FId write Fid;
    property Terminated;
    property User : string read FUser write FUser;
    property Socket : TTCPBlockSocket read FSocket;
    property ReadTimedOut: boolean read FReadTimedOut;
    property WriteTimedOut: boolean read FWriteTimedOut;
    property OnDestroy: TNotifyEvent read FOnDestroy write FOnDestroy;
    property OnExecute: TSThreadEvent read FOnExecute write FOnExecute;
    property OnException : TNotifyEvent read FOnException write FOnException;
  end;
  TSTcpThreadClass = class of TSTcpThread;

  TSLoginEvent = function(aSocket: TSTcpThread; aUser, aPasswort: string): boolean of
    object;
  TSLogEvent = procedure(aSocket: TSTcpThread; DirectionIn: boolean;
    aMessage: string) of object;

  TSBaseServer = class;
  TSTcpListener = class(TThread)
  private
    FParent: TSBaseServer;
    FSocket: TTCPBlockSocket;
    FOnConnect: TSThreadEvent;
    FPort: integer;
    FIP: string;
  public
    constructor Create(aParent : TSBaseServer);
    destructor Destroy; override;
    procedure Execute; override;
  published
    property IP: string read FIP write FIP;
    property Port: integer read FPort write FPort;
    property OnConnect: TSThreadEvent read FOnConnect write FOnConnect;
    property Parent : TSBaseServer read FParent write fParent;
  end;

  { TSBaseServer }

  TSBaseServer = class(TComponent)
  private
    FClass: TSTcpThreadClass;
    FListenInterface: string;
    FLog: TSLogEvent;
    FLogin: TSLoginEvent;
    FTimeout, FDefaultPort: integer;
    procedure SetListenInterface(AValue: string);
  protected
    function GetActive: boolean; virtual; abstract;
    procedure SetActive(const AValue: boolean); virtual; abstract;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Active: boolean read GetActive write SetActive;
    procedure Start;
    property ListenInterface: string read FListenInterface write SetListenInterface;
    property ListenPort: integer read FDefaultPort write FDefaultPort;
    property Timeout: integer read FTimeout write FTimeout default 60000;
    property OnLogin: TSLoginEvent read FLogin write FLogin;
    property OnLog: TSLogEvent read FLog write FLog;
    property ClassType : TSTcpThreadClass read FClass write FClass;
  end;

  TSTcpServer = class(TSBaseServer)
  private
    FListeners, FThreads: TList;
    FSection: TCriticalSection;
    FOnExecute: TSThreadExecuteEvent;
    FVersion: string;
    function CreateListener(const AIP: string; const APort: integer): TSTcpListener;
  protected
    procedure DoClientCreate(AThread: TSTcpThread);virtual;
    procedure DoClientDestroy(ASender: TObject);virtual;
    function GetActive: boolean; override;
    procedure SetActive(const AValue: boolean); override;
    procedure Execute(AThread: TSTcpThread); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property OnExecute: TSThreadExecuteEvent read FOnExecute write FOnExecute;
    property Version: string read FVersion write FVersion;
  end;

implementation

{ TSBaseServer }

procedure TSBaseServer.SetListenInterface(AValue: string);
begin
  if FListenInterface = AValue then
    Exit;
  FListenInterface := AValue;
end;

constructor TSBaseServer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ListenInterface := '0.0.0.0';
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
  FConnection := TSConnection.Create;
  FSocket := TTCPBlockSocket.Create;
  FSocket.Socket := ASocket;
  FreeOnTerminate := True;
  FConnected := True;
  FReadTimedOut := False;
  FWriteTimedOut := False;
end;

destructor TSTcpThread.Destroy;
begin
  if (Assigned(FOnDestroy)) then
    FOnDestroy(Self);
  FreeAndNil(FSocket);
  FreeAndNil(FConnection);
  inherited;
end;

function TSTcpThread.ReadLn(const ATimeout: integer): string;
begin
  if (FSocket.socket = INVALID_SOCKET) then
  begin
    FConnected := False;
    FReadTimedOut := False;
  end
  else
  begin
    FConnected := True;
    FReadTimedOut := False;
    Result := FSocket.RecvString(ATimeout);
    if (FSocket.socket = INVALID_SOCKET) then
    begin
      FConnected := False;
      FReadTimedOut := False;
    end
    else
    begin
      case FSocket.LastError of
        0: ;
        WSAETIMEDOUT: FReadTimedOut := True;
        else
          FConnected := False;
      end;
    end;
  end;
end;

function TSTcpThread.ReadPacket(const ATimeout: integer): string;
begin
  if (FSocket.socket = INVALID_SOCKET) then
  begin
    FConnected := False;
    FReadTimedOut := False;
  end
  else
  begin
    FConnected := True;
    FReadTimedOut := False;
    Result := FSocket.RecvPacket(ATimeout);
    if (FSocket.socket = INVALID_SOCKET) then
    begin
      FConnected := False;
      FReadTimedOut := False;
    end
    else
    begin
      case FSocket.LastError of
        0: ;
        WSAETIMEDOUT: FReadTimedOut := True;
        else
          FConnected := False;
      end;
    end;
  end;
end;

procedure TSTcpThread.WriteLn(const AData: string);
begin
  if (FSocket.socket = INVALID_SOCKET) then
  begin
    FConnected := False;
    FWriteTimedOut := False;
  end
  else
  begin
    FSocket.SendString(AData + CRLF);
    if (FSocket.socket = INVALID_SOCKET) then
    begin
      FConnected := False;
      FWriteTimedOut := False;
    end
    else
    begin
      case FSocket.LastError of
        0: ;
        WSAETIMEDOUT: FWriteTimedOut := True;
      end;
    end;
  end;
end;

procedure TSTcpThread.Write(const AData: string);
begin
  if (FSocket.socket = INVALID_SOCKET) then
  begin
    FConnected := False;
    FWriteTimedOut := False;
  end
  else
  begin
    FSocket.SendString(AData);
    if (FSocket.socket = INVALID_SOCKET) then
    begin
      FConnected := False;
      FWriteTimedOut := False;
    end
    else
    begin
      case FSocket.LastError of
        0: ;
        WSAETIMEDOUT: FWriteTimedOut := True;
      end;
    end;
  end;
end;

procedure TSTcpThread.Disconnect;
begin
  FSocket.CloseSocket;
  FConnected := False;
  FReadTimedOut := False;
  FWriteTimedOut := False;
end;

procedure TSTcpThread.Execute;
begin
  FConnection.PeerIP := FSocket.GetRemoteSinIP;
  FConnection.PeerName := FSocket.ResolveIPToName(FSocket.GetRemoteSinIP);
  FConnection.PeerPort := FSocket.GetRemoteSinPort;
  try
    if (Assigned(FOnExecute)) then
      FOnExecute(Self);
  except
    if (Assigned(FOnException)) then
      FOnException(Self);
  end;
end;

constructor TSTcpListener.Create(aParent: TSBaseServer);
begin
  inherited Create(True);
  FSocket := TTCPBlockSocket.Create;
  FreeOnTerminate := False;
  FParent := aParent;
end;

destructor TSTcpListener.Destroy;
begin
  if (Suspended) then
    Resume;
  FreeAndNil(FSocket);
  inherited;
end;

procedure TSTcpListener.Execute;
var
  LClient: TSocket;
  LSynaThread: TSTcpThread;
begin
  with FSocket do
  begin
    CreateSocket;
    RaiseExcept := False;
    SetLinger(True, 10);
    Bind(FIP, IntToStr(FPort));
    Listen;
    repeat
      try
        if (Terminated) then
          Break
        else if (CanRead(1000)) then
          begin
            if ((not Terminated) and (not Suspended)) then
            begin
              if (Assigned(FOnConnect)) then
              begin
                LClient := Accept;
                if (LastError = 0) then
                begin
                  LSynaThread := Parent.ClassType.Create(LClient);
                  FOnConnect(LSynaThread);
                end;
              end;
            end;
          end;
      except
        Break;
      end;
    until False;
  end;
end;

constructor TSTcpServer.Create(AOwner: TComponent);
begin
  inherited;
  FSection := TCriticalSection.Create;
  FThreads := TList.Create;
  FListeners := TList.Create;
  FClass := TSTcpThread;
end;

destructor TSTcpServer.Destroy;
begin
  if (GetActive) then
    SetActive(False);
  FreeAndNil(FThreads);
  FreeAndNil(FListeners);
  FreeAndNil(FSection);
  inherited;
end;

function TSTcpServer.GetActive: boolean;
begin
  Result := FListeners.Count > 0;
end;

function TSTcpServer.CreateListener(const AIP: string;
  const APort: integer): TSTcpListener;
begin
  Result := TSTcpListener.Create(Self);
  Result.OnConnect := @DoClientCreate;
  Result.IP := AIP;
  Result.Port := APort;
end;

procedure TSTcpServer.SetActive(const AValue: boolean);
var
  i: integer;
  LListener: TSTcpListener;
begin
  if (AValue <> GetActive) then
  begin
    if (AValue) then
    begin
      FListeners.Clear;
      LListener := CreateListener(FListenInterface, FDefaultPort);
      FListeners.Add(LListener);
      for i := 0 to FListeners.Count - 1 do
        TSTcpListener(FListeners[i]).Resume;
    end
    else
    begin
      for i := FListeners.Count - 1 downto 0 do
        TSTcpListener(FListeners[i]).Terminate;

      for i := FThreads.Count - 1 downto 0 do
        TSTcpThread(FThreads[i]).Disconnect;

      for i := FListeners.Count - 1 downto 0 do
      begin
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
    FSection.Enter;
    FThreads.Add(AThread);
    AThread.Id := FThreads.Count;
  finally
    FSection.Leave;
  end;
  AThread.OnDestroy := @DoClientDestroy;
  AThread.OnExecute := @Execute;
  AThread.Resume;
end;

procedure TSTcpServer.DoClientDestroy(ASender: TObject);
var
  LIndex: integer;
begin
  FSection.Create;
  try
    FSection.Enter;
    LIndex := FThreads.IndexOf(ASender);
    if (LIndex >= 0) then
      FThreads.Delete(LIndex);
  finally
    FSection.Leave;
  end;
end;

procedure TSTcpServer.Execute(AThread: TSTcpThread);
var
  LData: string;
begin
  while (not AThread.Terminated) do
  begin
    LData := AThread.ReadLn(Timeout);
    if (not (AThread as TSTcpThread).Connected) then
      Break
    else if ((AThread as TSTcpThread).ReadTimedOut) then
        Break
    else if (Assigned(FOnExecute)) then
      FOnExecute(AThread, LData);
  end;
end;

end.
