unit uhttpserver;

interface

uses
  Classes, blcksock, Synautil, SysUtils,Sockets,uAppServer;

type
  TTCPHttpThrd = class;
  THttpThrdClass = class of TTCPHttpThrd;

  { TTCPHttpDaemon }

  TTCPHttpDaemon = class(TThread)
  private
    FClass: THttpThrdClass;
    Sock:TTCPBlockSocket;
  public
    Constructor Create;virtual;
    Destructor Destroy; override;
    procedure Execute; override;
    property ThreadType :  THttpThrdClass read FClass write FClass;
    procedure InternalMessage(aMsg : string);virtual;
  end;

  { TTCPHttpThrd }

  TTCPHttpThrd = class(TThread)
  private
    FCreator: TTCPHttpDaemon;
    Sock:TTCPBlockSocket;
  public
    Headers: TStringList;
    Parameters : TStringList;
    InputData, OutputData: TMemoryStream;
    Constructor Create (hsock:tSocket);virtual;
    Destructor Destroy; override;
    procedure Execute; override;
    property Creator : TTCPHttpDaemon read FCreator write FCreator;
    function ProcessHttpRequest(Request, URI: string): integer;virtual;
  end;

implementation

{ TTCPHttpDaemon }

constructor TTCPHttpDaemon.Create;
begin
  inherited create(false);
  sock:=TTCPBlockSocket.create;
  FreeOnTerminate:=true;
  FClass := TTCPHttpThrd;
end;

destructor TTCPHttpDaemon.Destroy;
begin
  Sock.free;
  inherited Destroy;
end;

procedure TTCPHttpDaemon.Execute;
var
  ClientSock:TSocket;
  aNewSock: TTCPHttpThrd;
begin
  while not Terminated do
    begin
      with sock do
        begin
          CreateSocket;
          setLinger(true,10000);
          bind('0.0.0.0','8085');
          if LastError=0 then
            begin
              InternalMessage('Listening...');
              listen;
              repeat
                if terminated then break;
                if canread(1000) then
                  begin
                    ClientSock:=accept;
                    if lastError=0 then
                      begin
                        aNewSock := FClass.create(ClientSock);
                        aNewSock.Creator:=Self;
                      end;
                  end;
              until false;
            end;
        end;
      InternalMessage('Bind failed, retrying in 5 sek...');
      sleep(5000);
    end;
end;

procedure TTCPHttpDaemon.InternalMessage(aMsg: string);
begin

end;

{ TTCPHttpThrd }

constructor TTCPHttpThrd.Create(hsock: tSocket);
begin
  Headers := TStringList.Create;
  Parameters := TStringList.Create;
  Parameters.NameValueSeparator:=':';
  InputData := TMemoryStream.Create;
  OutputData := TMemoryStream.Create;
  sock:=TTCPBlockSocket.create;
  Sock.socket:=HSock;
  FreeOnTerminate:=true;
  Priority:=tpNormal;
  inherited create(false);
end;

destructor TTCPHttpThrd.Destroy;
begin
  Sock.free;
  Headers.Free;
  Parameters.Free;
  InputData.Free;
  OutputData.Free;
  inherited Destroy;
end;

procedure TTCPHttpThrd.Execute;
var
  timeout: integer;
  s: string;
  method, uri, protocol: string;
  size: integer;
  x, n: integer;
  resultcode: integer;
  close: boolean;
  tmp: String;
begin
  timeout := 120000;
  repeat
    //read request line
    s := sock.RecvString(timeout);
    if sock.lasterror <> 0 then
      Exit;
    if s = '' then
      Exit;
    method := fetch(s, ' ');
    if (s = '') or (method = '') then
      Exit;
    uri := fetch(s, ' ');
    if uri = '' then
      Exit;
    protocol := fetch(s, ' ');
    headers.Clear;
    Parameters.Clear;
    size := -1;
    close := false;
    //read request headers
    if protocol <> '' then
    begin
      if pos('HTTP/', protocol) <> 1 then
        Exit;
      if pos('HTTP/1.1', protocol) <> 1 then
        close := true;
      repeat
        s := sock.RecvString(Timeout);
        if sock.lasterror <> 0 then
          Exit;
        if s <> '' then
          begin
            Headers.add(s);
            tmp := copy(s,0,pos(':',s)-1);
            Parameters.Add(lowercase(tmp)+':'+trim(copy(s,pos(':',s)+1,length(s))));
          end;
        if Pos('CONTENT-LENGTH:', Uppercase(s)) = 1 then
          Size := StrToIntDef(SeparateRight(s, ' '), -1);
        if Pos('CONNECTION: CLOSE', Uppercase(s)) = 1 then
          close := true;
      until s = '';
    end;
    //recv document...
    InputData.Clear;
    if size >= 0 then
    begin
      InputData.SetSize(Size);
      x := Sock.RecvBufferEx(InputData.Memory, Size, Timeout);
      InputData.SetSize(x);
      if sock.lasterror <> 0 then
        Exit;
    end;
    OutputData.Clear;
    ResultCode := ProcessHttpRequest(method, uri);
    sock.SendString(protocol + ' ' + IntTostr(ResultCode) + CRLF);
    if protocol <> '' then
    begin
      if pos('CONTENT-LENGTH:',UpperCase(Headers.Text))=0 then
        headers.Add('Content-length: ' + IntTostr(OutputData.Size));
      if close then
        headers.Add('Connection: close');
      headers.Add('Date: ' + Rfc822DateTime(now));
      headers.Add('Server: Avamm');
      headers.Add('');
      for n := 0 to headers.count - 1 do
        sock.sendstring(headers[n] + CRLF);
    end;
    if sock.lasterror <> 0 then
      Exit;
    Sock.SendBuffer(OutputData.Memory, OutputData.Size);
    Headers.Clear;
    if close then
      Break;
  until Sock.LastError <> 0;
end;

function TTCPHttpThrd.ProcessHttpRequest(Request, URI: string): integer;
var
  l: TStringlist;
begin
  result := 504;
end;

end.
