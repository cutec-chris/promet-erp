library serialport;

{$mode objfpc}{$H+}

uses
  Classes,sysutils,synaser;

type
  TParityType = (NoneParity, OddParity, EvenParity);

var
  Ports : TList = nil;
  aData: String;

function SerOpen(const DeviceName: String): LongInt;
var
  aDev: TBlockSerial;
begin
  if not Assigned(Ports) then
    Ports := TList.Create;
  aDev := TBlockSerial.Create;
  aDev.Connect(DeviceName);
  Ports.Add(aDev);
  Result := aDev.Handle;
end;

procedure SerClose(Handle: LongInt);
var
  i: Integer;
  aDev: TBlockSerial;
begin
  for i := 0 to Ports.Count-1 do
    if TBlockSerial(Ports[i]).Handle=Handle then
      begin
        aDev := TBlockSerial(Ports[i]);
        Ports.Remove(aDev);
        aDev.Free;
        if Ports.Count=0 then
          FreeAndNil(Ports);
        exit;
      end;
end;

procedure SerFlush(Handle: LongInt);
var
  i: Integer;
begin
  for i := 0 to Ports.Count-1 do
    if TBlockSerial(Ports[i]).Handle=Handle then
      begin
        TBlockSerial(Ports[i]).Flush;
        exit;
      end;
end;

procedure SerParams(Handle: LongInt; BitsPerSec: LongInt; ByteSize: Integer; Parity: TParityType; StopBits: Integer);
var
  i: Integer;
begin
  for i := 0 to Ports.Count-1 do
    if TBlockSerial(Ports[i]).Handle=Handle then
      begin
        case Parity of
        NoneParity:TBlockSerial(Ports[i]).Config(BitsPerSec,ByteSize,'N',StopBits,false,true);
        OddParity:TBlockSerial(Ports[i]).Config(BitsPerSec,ByteSize,'O',StopBits,false,true);
        EvenParity:TBlockSerial(Ports[i]).Config(BitsPerSec,ByteSize,'E',StopBits,false,true);
        end;
        exit;
      end;
end;

function SerReadEx(Handle: LongInt;Count: LongInt) : PChar;
var
  Data: String;
  i: Integer;
begin
  for i := 0 to Ports.Count-1 do
    if TBlockSerial(Ports[i]).Handle=Handle then
      begin
        SetLength(Data,Count);
        TBlockSerial(Ports[i]).RecvBuffer(@Data[1],Count);
        Result := @Data[1];
        exit;
      end;
end;

function SerReadTimeoutEx(Handle: LongInt;var Data : PChar;Timeout: Integer;Count: LongInt) : Integer;
var
  i: Integer;
  iData: String;
  a: Integer;
begin
  for i := 0 to Ports.Count-1 do
    if TBlockSerial(Ports[i]).Handle=Handle then
      begin
        iData := TBlockSerial(Ports[i]).RecvPacket(Timeout);
        Result := length(iData);
        aData := '';
        for a := 1 to length(iData) do
          aData := aData+IntToHex(ord(iData[a]),2);
        Data := PChar(aData);
        exit;
      end;
end;

function SerGetCTS(Handle: LongInt) : Boolean;
var
  i: Integer;
begin
  for i := 0 to Ports.Count-1 do
    if TBlockSerial(Ports[i]).Handle=Handle then
      begin
        Result := TBlockSerial(Ports[i]).CTS;
        exit;
      end;
end;

function SerGetDSR(Handle: LongInt) : Boolean;
var
  i: Integer;
begin
  for i := 0 to Ports.Count-1 do
    if TBlockSerial(Ports[i]).Handle=Handle then
      begin
        Result := TBlockSerial(Ports[i]).DSR;
        exit;
      end;
end;

procedure SerSetRTS(Handle: LongInt;Value : Boolean);
var
  i: Integer;
begin
  for i := 0 to Ports.Count-1 do
    if TBlockSerial(Ports[i]).Handle=Handle then
      begin
        TBlockSerial(Ports[i]).RTS := Value;
        exit;
      end;
end;

procedure SerSetDTR(Handle: LongInt;Value : Boolean);
var
  i: Integer;
begin
  for i := 0 to Ports.Count-1 do
    if TBlockSerial(Ports[i]).Handle=Handle then
      begin
        TBlockSerial(Ports[i]).DTR := Value;
        exit;
      end;
end;

function SerWrite(Handle: LongInt; Data : PChar;Len : Integer): LongInt;
var
  i: Integer;
begin
  for i := 0 to Ports.Count-1 do
    if TBlockSerial(Ports[i]).Handle=Handle then
      begin
        TBlockSerial(Ports[i]).SendBuffer(Data,Len);
        Result := length(Data);
        exit;
      end;
end;

procedure ScriptCleanup;
var
  i: Integer;
begin
  if not Assigned(Ports) then exit;
  for i := 0 to Ports.Count-1 do
    TBlockSerial(Ports[i]).Free;
  Ports.Clear;
  FreeAndNil(Ports);
end;

function ScriptUnitDefinition : PChar;stdcall;
begin
  Result := 'unit SerialPort;'
       +#10+'interface'
       +#10+'type'
       +#10+'  TParityType = (NoneParity, OddParity, EvenParity);'
       +#10+'  function SerOpen(const DeviceName: String): LongInt;external ''SerOpen@serialport.dll'';'
       +#10+'  procedure SerClose(Handle: LongInt);external ''SerClose@serialport.dll'';'
       +#10+'  procedure SerFlush(Handle: LongInt);external ''SerFlush@serialport.dll'';'
       +#10+'  function SerRead(Handle: LongInt; Count: LongInt): string;'
       +#10+'  function SerReadTimeout(Handle: LongInt;Timeout: Integer;Count: LongInt) : string;'
       +#10+'  function SerWrite(Handle: LongInt; Data : PChar;Len : Integer): LongInt;external ''SerWrite@serialport.dll'';'
       +#10+'  procedure SerParams(Handle: LongInt; BitsPerSec: LongInt; ByteSize: Integer; Parity: TParityType; StopBits: Integer);external ''SerParams@serialport.dll'';'
       +#10+'  function SerGetCTS(Handle: LongInt) : Boolean;external ''SerGetCTS@serialport.dll'';'
       +#10+'  function SerGetDSR(Handle: LongInt) : Boolean;external ''SerGetDSR@serialport.dll'';'
       +#10+'  procedure SerSetRTS(Handle: LongInt;Value : Boolean);external ''SerSetRTS@serialport.dll'';'
       +#10+'  procedure SerSetDTR(Handle: LongInt;Value : Boolean);external ''SerSetDTR@serialport.dll'';'

       +#10+'  function SerReadEx(Handle: LongInt; Count: LongInt): PChar;external ''SerReadEx@serialport.dll'';'
       +#10+'  function SerReadTimeoutEx(Handle: LongInt;var Data : PChar;Timeout: Integer;Count: LongInt) : Integer;external ''SerReadTimeoutEx@serialport.dll'';'
       +#10+'implementation'
       +#10+'  function SerRead(Handle: LongInt; Count: LongInt): string;'
       +#10+'  var aOut : PChar;'
       +#10+'      bOut : string;'
       +#10+'      i : Integer;'
       +#10+'  begin'
       +#10+'    Result := '''';'
       +#10+'    aOut := SerReadEx(Handle,Count);'
       +#10+'    bOut := aOut;'
       +#10+'    SetLength(Result,Count);'
       +#10+'    for i := 0 to Count-1 do'
       +#10+'      begin'
       +#10+'        Result := Result+chr(StrToInt(''$''+copy(bOut,0,2)));'
       +#10+'        bOut := copy(bOut,3,length(bOut));'
       +#10+'        if bOut='''' then break;'
       +#10+'      end;'
       +#10+'  end;'
       +#10+'  function SerReadTimeout(Handle: LongInt;Timeout: Integer;Count: LongInt) : string;'
       +#10+'  var aOut : PChar;'
       +#10+'      bOut : string;'
       +#10+'      a : Integer;'
       +#10+'  begin'
       +#10+'    Result := '''';'
       +#10+'    a := SerReadTimeoutEx(Handle,aOut,Timeout,Count);'
       +#10+'    bOut := aOut;'
       +#10+'    SetLength(Result,a);'
       +#10+'    while a > 0 do'
       +#10+'      begin'
       +#10+'        Result := Result+chr(StrToInt(''$''+copy(bOut,0,2)));'
       +#10+'        bOut := copy(bOut,3,length(bOut));'
       +#10+'        if bOut='''' then break;'
       +#10+'        dec(a);'
       +#10+'      end;'
       +#10+'  end;'
       +#10+'end.'
            ;
end;

exports
  SerOpen,
  SerClose,
  SerFlush,
  SerReadEx,
  SerReadTimeoutEx,
  SerWrite,
  SerParams,
  SerGetCTS,
  SerGetDSR,
  SerSetRTS,
  SerSetDTR,

  ScriptUnitDefinition,
  ScriptCleanup;

end.
