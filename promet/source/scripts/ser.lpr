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
  serial,BaseUnix,termio,unix
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
  aRest: Integer;
  i: Integer;
  {$ifdef UNIX}
  readSet: TFDSet;
  selectTimeout: TTimeVal;
  mSec : Integer = 150;
  aSize: BaseUnix.cint;
  {$endif}
begin
  Rest := Count;
  for i := 0 to (aRest div 256)+1 do
    begin
      if Rest>256 then
        aRest := 256
      else aRest := rest;
      {$IFDEF DARWIN}
      aRead := serial_osx.SerRead(Handle,s[1],aRest);
      {$ELSE}
      {$IFDEF MSWINDOWS}
      aRead := serial_win.SerRead(Handle,s[1],aRest);
      {$ELSE}
      aRead := 0;
      fpFD_ZERO(readSet);
      fpFD_SET(Handle, readSet);
      selectTimeout.tv_sec := mSec div 1000;
      selectTimeout.tv_usec := (mSec mod 1000) * 1000;
      aSize := fpSelect(Handle + 1, @readSet, nil, nil, @selectTimeout);
      if aSize>aRest then aSize := aRest;
      if aSize>0 then
        aRead := fpRead(Handle, s[1], aSize);
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
