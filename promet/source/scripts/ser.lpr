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

function ScriptDefinition : PChar;stdcall;
begin
  Result := 'TParityType = (NoneParity, OddParity, EvenParity);'
       +#10+'function SerOpen(const DeviceName: String): LongInt;'
       +#10+'procedure SerClose(Handle: LongInt);'
       +#10+'procedure SerFlush(Handle: LongInt);'
       +#10+'function SerRead(Handle: LongInt; var Buffer; Count: LongInt): LongInt;'
       +#10+'function SerWrite(Handle: LongInt; var Buffer; Count: LongInt): LongInt;'
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
