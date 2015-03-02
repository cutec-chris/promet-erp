library serialport;

{$mode objfpc}{$H+}

uses
  Classes,sysutils,synaser;

type
  TParityType = (NoneParity, OddParity, EvenParity);

var
  Ports : TList = nil;

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
        Data := TBlockSerial(Ports[i]).RecvBlock(100);
        Result := PChar(Data);
        exit;
      end;
end;

function SerWrite(Handle: LongInt; Data : string): LongInt;
var
  i: Integer;
begin
  for i := 0 to Ports.Count-1 do
    if TBlockSerial(Ports[i]).Handle=Handle then
      begin
        TBlockSerial(Ports[i]).SendBlock(Data);
        Result := length(Data);
        exit;
      end;
end;

procedure ScriptCleanup;
var
  i: Integer;
begin
  for i := 0 to Ports.Count-1 do
    TBlockSerial(Ports[i]).Free;
  Ports.Clear;
  FreeAndNil(Ports);
end;

function ScriptDefinition : PChar;stdcall;
begin
  Result := 'TParityType = (NoneParity, OddParity, EvenParity);'
       +#10+'function SerOpen(const DeviceName: String): LongInt;'
       +#10+'procedure SerClose(Handle: LongInt);'
       +#10+'procedure SerFlush(Handle: LongInt);'
       +#10+'function SerRead(Handle: LongInt; Count: LongInt): PChar;'
       +#10+'function SerWrite(Handle: LongInt; Data : string): LongInt;'
       +#10+'procedure SerParams(Handle: LongInt; BitsPerSec: LongInt; ByteSize: Integer; Parity: TParityType; StopBits: Integer);'
            ;
end;

exports
  SerOpen,
  SerClose,
  SerFlush,
  SerRead,
  SerWrite,
  SerParams,

  ScriptDefinition,
  ScriptCleanup;

end.
