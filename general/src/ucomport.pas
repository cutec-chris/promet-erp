unit ucomport;

{$mode objfpc}{$H+}

interface

uses
  Forms,Classes, SysUtils,LCLIntf, Dialogs,LCLProc,
  {$IFDEF DARWIN}
  serial_osx
  {$ELSE}
  {$IFDEF MSWINDOWS}
  Windows,serial_win
  {$ELSE}
  serial
  {$ENDIF}
  {$ENDIF}
  ;
  
type

  TComport = class;

  { TRecvThread }

  TRecvThread = class(TThread)
  private
    {$IFDEF WINDOWS}
    Overlapped: TOverlapped;
    ex: DWord;
    {$ENDIF}
    FPort : TComPort;
    FBuffer : string;
    aBuffer : string;
    procedure DirectDataReceived;
    procedure DataReceived(Data: PtrInt);
    procedure PutData;
    procedure DoExit;
  public
    function SendData(aData : string) : LongInt;
    procedure DoTerminate;
    constructor Create(Port : TComPort);
    procedure Execute;override;
    destructor Destroy; override;
  end;
  
  TCharNotification = procedure(Port : TComport;c : char) of object;
  TLineNotification = procedure(Port : TComport;line : string) of object;

  { TComPort }

  TComPort = class(TObject)
  private
    FActive: Boolean;
    FBaudrate: Integer;
    FCharReceived: TCharNotification;
    FDatabits: Integer;
    FHardflow: Boolean;
    FLineReceived: TLineNotification;
    FParity: char;
    hPort : THandle;
    FPortName : string;
    FSoftflow: Boolean;
    FStopbits: Integer;
    FRecvThread : TRecvThread;
    FTerminator: char;
    procedure SetActive(const AValue: Boolean);
    procedure SetDTR(const AValue: Boolean);
    procedure SetRTS(const AValue: Boolean);
  protected
    FBuffer : string;
  public
    constructor Create(APortName : string);
    destructor Destroy;override;
    property Port : string read FPortName write FPortname;
    property Baudrate : Integer read FBaudrate write FBaudrate;
    property Parity : char read FParity write FParity;
    property Stopbits : Integer read FStopbits write FStopbits;
    property Databits : Integer read FDatabits write FDataBits;
    property Hardflow : Boolean read FHardflow write FHardflow;
    property Softflow : Boolean read FSoftflow write FSoftflow;
    property DTR: Boolean write SetDTR;
    property RTS: Boolean write SetRTS;
    property Buffer : string read FBuffer;
    property Active : Boolean read FActive write SetActive;
    function SetParamsFromString(params : string) : Boolean;
    property LineTerminator : char read FTerminator write FTerminator;
    procedure Open;virtual;
    procedure Close;virtual;
    procedure ClearBuffer;
    function ATCommand(cmd : string) : string;
    function SendString(data : string) : Boolean;
    property OnCharReceived : TCharNotification read FCharReceived write FCharReceived;
    property OnLineReceived : TLineNotification read FLineReceived write FLineReceived;
    function CheckLineReceived(var Line : string) : Boolean;
    function CheckCharReceived(var c : char) : Boolean;
    function RecivLine(Timeout : Integer) : string;
  end;

implementation

{ TComPort }

procedure TComPort.SetActive(const AValue: Boolean);
begin
  if FActive=AValue then exit;
  FActive:=AValue;
  if FActive then
    Open
  else
    Close;
end;

procedure TComPort.SetDTR(const AValue: Boolean);
begin
  SerSetDTR(hPort,AValue);
end;

procedure TComPort.SetRTS(const AValue: Boolean);
begin
  SerSetRTS(hPort,AValue);
end;

constructor TComPort.Create(APortName: string);
begin
  FDatabits := 8;
  FStopbits := 1;
  FParity := 'N';
  FBaudrate := 9600;
  FPortname := APortName;
  FHardFlow := True;
  FSoftFlow := False;
  hPort := 0;
  FTerminator := #13;
end;

destructor TComPort.Destroy;
begin
  if Active then
    Close;
end;

function TComPort.SetParamsFromString(params: string): Boolean;
var
  tmp: String;
begin
  Result := True;
  try
    if pos(',',Params) = 0 then
      begin
        Result := False;
        exit;
      end;
    FBaudrate := StrToInt(copy(Params,0,pos(',',Params)-1));
    tmp := copy(Params,pos(',',Params)+1,length(Params));
    FDatabits := StrToInt(copy(tmp,0,1));
    FParity := copy(tmp,2,1)[1];
    FStopbits := StrToInt(copy(tmp,3,1));
  except
    result := False;
    exit;
  end;
end;

procedure TComPort.Open;
begin
  if not ((hPort = 0)
{$IFDEF WINDOWS}
  or (hport = INVALID_HANDLE_VALUE)
{$ENDIF}
  ) then
    begin
      FActive := True;
      exit;
    end;
  FActive := False;
{$IFDEF WINDOWS}
  hPort := CreateFile(PChar('\\.\' + UpperCase(FPortName)),
                       GENERIC_READ or GENERIC_WRITE,
                       0,
                       Nil,
                       OPEN_EXISTING,
                       FILE_FLAG_NO_BUFFERING or FILE_FLAG_OVERLAPPED,
                       0);
{$ELSE}
  hPort := SerOpen(FPortName);
{$ENDIF}
  if (hPort = 0)
{$IFDEF WINDOWS}
  or (hport = INVALID_HANDLE_VALUE)
{$ENDIF}
  then exit;
  FActive := True;
  if FParity = 'N' then
    SerSetParams(hPort,FBaudRate,FDataBits,NoneParity,FStopBits,[])
  else
    SerSetParams(hPort,FBaudRate,FDataBits,OddParity,FStopBits,[]);
  if FActive then
    FRecvThread := TRecvThread.Create(Self);
end;

procedure TComPort.Close;
var
  aPort: LongWord;
begin
  if not FActive then exit;
  aPort := hPort;
  hPort := 0;
  if not Assigned(FRecvThread) then exit;
  FRecvThread.DoTerminate;
  FActive := False;
  SerClose(aPort);
end;

procedure TComPort.ClearBuffer;
begin
  FBuffer := '';
end;

function TComPort.SendString(data : string) : Boolean;
begin
  Result := FRecvThread.SendData(data) = length(data);
end;

function TComPort.ATCommand(cmd: string): string;
var
  aBuffer: array[0..40] of char;
  tmp: String;
begin
//  Debugln('ATCommand:'+cmd);
  tmp := cmd+#13;
  SerWrite(hPort,PChar(tmp)^,length(tmp));
  aBuffer[0] := #0;
  aBuffer[SerRead(hPort,aBuffer, 40)] := #0;
  Result := aBuffer;
  Result := StringReplace(Result,#10,'',[rfReplaceAll]);
  Result := StringReplace(Result,#13,'',[rfReplaceAll]);
  if pos('OK',result) > 0 then
    Result := copy(Result,0,pos('OK',Result)-1);
end;

function TComPort.CheckLineReceived(var Line: string): Boolean;
begin
  Result := false;
  if pos(FTerminator,FBuffer) > 0 then
    begin
      Line := copy(FBuffer,0,pos(FTerminator,FBuffer));
      Fbuffer := copy(FBuffer,pos(FTerminator,FBuffer)+1,length(FBuffer));
      Result := True;
    end;
end;

function TComPort.CheckCharReceived(var c: char): Boolean;
begin
  Result := False;
  if length(FBuffer) > 0 then
    begin
      c := FBuffer[1];
      FBuffer := copy(Fbuffer,2,length(FBuffer));
      Result := True;
    end;
end;

function TComPort.RecivLine(Timeout: Integer): string;
var
  atm: Int64;
begin
  atm := GetTickCount;
  while (TimeOut) > (GetTickCount - atm) do
    begin
      if pos(FTerminator,FBuffer) > 0 then
        begin
          Result := copy(FBuffer,0,pos(FTerminator,FBuffer)-1);
          FBuffer := copy(FBuffer,pos(FTerminator,FBuffer)+1,length(Fbuffer));
          exit;
        end;
      Application.Processmessages;
    end;
end;

{ TRecvThread }
procedure TRecvThread.DirectDataReceived;
begin
  DataReceived(PtrInt(@FPort));
end;
procedure TRecvThread.DataReceived(Data: PtrInt);
var
  i: Integer;
  aPort : TComPort;
begin
  aPort := TComPort(Pointer(Data)^);
  if not Assigned(aPort) then exit;
  if (length(aPort.Fbuffer) > 0) or (length(FBuffer) > 0) then
    begin
      if Assigned(aPort.OnCharReceived) then
        for i := 0 to length(FBuffer) do
          aPort.OnCharReceived(aPort,FBuffer[i]);
      if Assigned(aPort.OnLineReceived) then
        begin
          while (pos(aPort.FTerminator,FBuffer) > 0) and Assigned(aPort.OnLineReceived) do
            begin
              aPort.OnLineReceived(aPort,StringReplace(copy(FBuffer,0,pos(aPort.FTerminator,FBuffer)),#10,'',[rfReplaceAll]));
              FBuffer := copy(FBuffer,pos(aPort.FTerminator,FBuffer)+1,length(FBuffer));
            end;
        end
      else
        begin
          if Assigned(aPort.OnCharReceived) then
            FBuffer := ''
          else
            begin
              aPort.FBuffer := aPort.FBuffer+FBuffer;
              FBuffer := '';
            end;
        end;
    end;
end;
procedure TRecvThread.PutData;
begin
  FBuffer := FBuffer+aBuffer;
end;
procedure TRecvThread.DoExit;
begin
  FPort.FRecvThread := nil;
end;
function TRecvThread.SendData(aData: string): LongInt;
begin
//  Debugln('SendString:'+aData);
  {$IFDEF WINDOWS}
  ResetEvent(Overlapped.hEvent);
  if not WriteFile(FPort.hPort, aData[1], length(aData), DWord(Result), @Overlapped) then Result := -1;
  {$ELSE}
  Result := SerWrite(FPort.hPort,aData[1],length(aData));
  {$ENDIF}
end;
procedure TRecvThread.DoTerminate;
begin
  Terminate;
  {$IFDEF WINDOWS}
  SetEvent(Overlapped.hEvent);
  {$ENDIF}
  WaitFor;
end;
constructor TRecvThread.Create(Port : TComPort);
begin
  FPort := Port;
  FBuffer := '';
  inherited Create(False);
  FreeOnTerminate := True;
end;

procedure TRecvThread.Execute;
var
  alen: LongInt;
begin
  {$IFDEF WINDOWS}
  FillChar(Overlapped, Sizeof(Overlapped), 0);
  Overlapped.hEvent := CreateEvent(nil, True, False, nil);
//  SetCommMask(FPort.hPort, EV_RXCHAR);
  {$ENDIF}
  while not Terminated do
    begin
      Setlength(aBuffer,40);
      {$IFDEF WINDOWS}
//      WaitCommEvent(FPort.hPort, ex, @Overlapped);
      ReadFile(FPort.hPort, aBuffer[1], 39, Dword(aLen), @Overlapped);
      WaitForSingleObject(Overlapped.hEvent, INFINITE);
      ResetEvent(Overlapped.hEvent);
      GetOverlappedResult(FPort.hPort, Overlapped, DWord(aLen), False);
      {$ELSE}
      alen := SerRead(FPort.hPort,aBuffer[1], 39);
      {$ENDIF}
      if aLen > 0 then
        begin
          Setlength(aBuffer,alen);
          PutData;
          Application.QueueAsyncCall(@DataReceived,PtrInt(@FPort));
        end
      else sleep(1);
    end;
end;

destructor TRecvThread.Destroy;
begin
  {$IFDEF WINDOWS}
//  SetCommMask(FPort.hPort,0);
  CloseHandle(Overlapped.hEvent);
  {$ENDIF}
  DoExit;
  inherited Destroy;
end;

end.

