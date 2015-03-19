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

function SerRead(Handle: LongInt;Count: LongInt) : PChar;
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

function SerReadTimeout(Handle: LongInt;var Data : PChar;Timeout: Integer;Count: LongInt) : Integer;
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
       +#10+'  function SerOpen(const DeviceName: String): LongInt;extern SerOpen@serialport.dll;'
       +#10+'  procedure SerClose(Handle: LongInt);extern SerClose@serialport.dll;'
       +#10+'  procedure SerFlush(Handle: LongInt);extern SerFlush@serialport.dll;'
       +#10+'  function SerRead(Handle: LongInt; Count: LongInt): PChar;extern SerRead@serialport.dll;'
       +#10+'  function SerReadTimeout(Handle: LongInt;var Data : PChar;Timeout: Integer;Count: LongInt) : Integer;extern SerReadTimeout@serialport.dll;'
       +#10+'  function SerWrite(Handle: LongInt; Data : PChar;Len : Integer): LongInt;extern SerWrite@serialport.dll;'
       +#10+'  procedure SerParams(Handle: LongInt; BitsPerSec: LongInt; ByteSize: Integer; Parity: TParityType; StopBits: Integer);extern SerParams@serialport.dll;'
       +#10+'  function SerGetCTS(Handle: LongInt) : Boolean;extern SerGetCTS@serialport.dll;'
       +#10+'  function SerGetDSR(Handle: LongInt) : Boolean;extern SerGetDSR@serialport.dll;'
       +#10+'  procedure SerSetRTS(Handle: LongInt;Value : Boolean);extern SerSetRTS@serialport.dll;'
       +#10+'  procedure SerSetDTR(Handle: LongInt;Value : Boolean);extern SerSetDTR@serialport.dll;'
       +#10+'implementation'
       +#10+'end.'
            ;
end;

exports
  SerOpen,
  SerClose,
  SerFlush,
  SerRead,
  SerReadTimeout,
  SerWrite,
  SerParams,
  SerGetCTS,
  SerGetDSR,
  SerSetRTS,
  SerSetDTR,

  ScriptUnitDefinition,
  ScriptCleanup;

end.
