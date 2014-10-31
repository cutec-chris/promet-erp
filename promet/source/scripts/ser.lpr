library ser;

{$mode objfpc}{$H+}

uses
  Classes,sysutils,
  {$IFDEF DARWIN}
  serial_osx
  {$ELSE}
  {$IFDEF MSWINDOWS}
  serial_win
  {$ELSE}
  serial
  {$ENDIF}
  {$ENDIF}
  ;

procedure SerParams(Handle: LongInt; BitsPerSec: LongInt; ByteSize: Integer; Parity: TParityType; StopBits: Integer);
begin
  SerSetParams(Handle,BitsPerSec,ByteSize,Parity,StopBits,[]);
end;

function SerRead(Handle: LongInt;Count: LongInt) : PChar;
var
  s : string[255];
  buffer : string = '';
  Rest: LongInt;
  aRead: Integer;
begin
  Rest := Count;
  while Rest>0 do
    begin
      {$IFDEF DARWIN}
      aRead := serial_osx.SerRead(Handle,s[1],256);
      {$ELSE}
      {$IFDEF MSWINDOWS}
      aRead := serial_win.SerRead(Handle,s[1],256);
      {$ELSE}
      aRead := serial.SerRead(Handle,s[1],256);
      {$ENDIF}
      {$ENDIF}
      buffer := buffer+copy(s,0,aRead);
      Rest := Rest-aRead;
    end;
  Result := PChar(buffer);
end;

function SerWrite(Handle: LongInt; Data : string): LongInt;
begin
  {$IFDEF DARWIN}
  Result := serial_osx.SerWrite(Handle,Data[1],length(Data));
  {$ELSE}
  {$IFDEF MSWINDOWS}
  Result
  {$ELSE}
  Result := serial.SerWrite(Handle,Data[1],length(Data));
  {$ENDIF}
  {$ENDIF}
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

  ScriptDefinition;

end.
