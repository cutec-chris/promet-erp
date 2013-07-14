unit http;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, httpsend;
  
type

  { THTTPThread }

  THTTPThread = class(TThread)
  private
    Fhttp: THttpSend;
    FOnDone: TNotifyEvent;
    FUrl : string;
    FMethod : string;
    procedure DoOnDone;
  public
    property Http : THttpSend read Fhttp write Fhttp;
    procedure Execute;override;
    constructor Create(url : string;OnDone : TNotifyEvent;Method : string);
    destructor Destroy;override;
  end;
  
  { TTimeoutThread }

  TTimeoutThread = class(TThread)
  private
    FOnTimeout: TNotifyEvent;
    FTimeout: Integer;
    procedure DoOnTimeout;
  public
    constructor Create(Timeout: Integer;OnTimeout : TNotifyEvent);
    procedure Execute;override;
    property Timeout : Integer read FTimeout write FTimeout;
  end;
  
  THTTP = class
    procedure FHttpThreadDone(Sender: TObject);
    procedure FTimeoutThreadTimeout(Sender: TObject);
  private
    FActive: Boolean;
    fData: TMemoryStream;
    FHttpThread : THttpThread;
    FTimeout: Integer;
    FTimeoutThread : TTimeoutThread;
    Furl: string;
  public
    constructor Create;
    property url : string read Furl write Furl;
    property Data : TMemoryStream read fData write fData;
    property Active : Boolean read FActive;
    property TimeOut : Integer read FTimeout write FTimeout;
    procedure Get;
    procedure Post;
    destructor Destroy;override;
  end;

implementation

{ THTTPThread }

procedure THTTPThread.DoOnDone;
begin
  if Assigned(FOnDone) then
    FOnDone(Self);
end;

procedure THTTPThread.Execute;
begin
  FHttp.HTTPMethod(FMethod,furl);
  if not Terminated then
    Self.Synchronize(@DoOnDone);
end;

constructor THTTPThread.Create(url : string;OnDone : TNotifyEvent;Method : string);
begin
  FMethod := Method;
  FreeOnTerminate := True;
  FOnDone := OnDone;
  Self.Priority := tpLower;
  Http := THttpSend.Create;
  if http.Headers.IndexOf('Cache-Control: no-cache') = -1 then
    http.Headers.Add('Cache-Control: no-cache');
  Furl := url;
  inherited Create(False);
end;

destructor THTTPThread.Destroy;
begin
  Http.Free;
end;

{ TTimeoutThread }

procedure TTimeoutThread.DoOnTimeout;
begin
  if Assigned(FOnTimeout) then
    FOnTimeout(Self);
end;

constructor TTimeoutThread.Create(Timeout : Integer;OnTimeout : TNotifyEvent);
begin
  FTimeout := Timeout;
  Self.Priority := tpLower;
  FOnTimeout := OnTimeout;
  FreeOnTerminate := True;
  inherited Create(False);
end;

procedure TTimeoutThread.Execute;
begin
  sleep(FTimeout);
  if not Terminated then
    Self.Synchronize(@DoOnTimeout);
end;

{ THTTP }

procedure THTTP.FHttpThreadDone(Sender: TObject);
begin
  FTimeoutThread.FOnTimeout := nil;
  if Assigned(FHttpThread) then FHttpThread.FOnDone := nil;
  if Assigned(FHttpThread) and Assigned(FHttpThread.Http) then
    FData.CopyFrom(FHttpThread.Http.Document,FHttpThread.Http.Document.Size);
  FHttpThread := nil;
  if FTimeoutThread <> nil then
    FTimeoutThread.Terminate;
  FTimeoutThread := nil;
  FActive := false;
end;

procedure THTTP.FTimeoutThreadTimeout(Sender: TObject);
begin
  if Assigned(FTimeoutThread) then FTimeoutThread.FOnTimeout := nil;
  if Assigned(FHttpThread) then FHttpThread.FOnDone := nil;
  FTimeoutThread := nil;
  if Assigned(FHttpThread) then
    begin
      if Assigned(FHttpThread.Http) and Assigned(FHttpThread.Http.Sock) then
        FHttpThread.Http.Abort;
      FHttpThread.Http.Sock.AbortSocket;
      FHttpThread.Terminate;
      FHttpThread := nil;
    end;
  FActive := false;
end;

constructor THTTP.Create;
begin
  FTimeout := 1500;
  FData := TMemoryStream.Create;
end;

procedure THTTP.Get;
begin
  FData.Size := 0;
  FHttpthread := THttpThread.Create(url,@FHttpThreadDone,'GET');
  FTimeoutThread := TTimeoutThread.Create(FTimeout,@FTimeoutThreadTimeout);
  FActive := True;
end;

procedure THTTP.Post;
begin
  FData.Size := 0;
  FHttpthread := THttpThread.Create(url,@FHttpThreadDone,'POST');
  FTimeoutThread := TTimeoutThread.Create(FTimeout,@FTimeoutThreadTimeout);
  FActive := True;
end;

destructor THTTP.Destroy;
begin
  if FHttpThread <> nil then
    FHttpThread.Terminate;
  if FTimeoutThread <> nil then
    FTimeoutThread.Terminate;
  FData.Free;
  inherited;
end;

end.

