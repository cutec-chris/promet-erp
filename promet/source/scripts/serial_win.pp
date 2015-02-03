{ Unit for handling the serial interfaces for Win32.
  (c) 2007 Luis R. Hilario B., luisdigital@gmail.com
}

unit serial_win;

{$MODE objfpc}
{$H+}
{$PACKRECORDS C}

interface

uses Windows, SysUtils;

type

  TSerialHandle = THandle;

  TParityType = (NoneParity, OddParity, EvenParity);

  TSerialFlags = set of (RtsCtsFlowControl);

  TSerialState = record
    LineState: LongWord;
    DCB: TDCB;
  end;


{ Open the serial device with the given device name, for example:
    COM1, COM2... for normal serial ports
    other device names are possible; refer to your OS documentation.
  Returns "INVALID_HANDLE_VALUE" if device could not be found }
function SerOpen(const DeviceName: String): TSerialHandle;

{ Closes a serial device previously opened with SerOpen. }
procedure SerClose(Handle: TSerialHandle);

{ Flushes the data queues of the given serial device. }
procedure SerFlush(Handle: TSerialHandle);

{ Reads a maximum of "Count" bytes of data into the specified buffer.
  Result: Number of bytes read. }
function SerRead(Handle: TSerialHandle; var Buffer; Count: LongInt): LongInt;

{ Tries to write "Count" bytes from "Buffer".
  Result: Number of bytes written. }
function SerWrite(Handle: TSerialHandle; var Buffer; Count: LongInt): LongInt;

procedure SerSetParams(Handle: TSerialHandle; BitsPerSec: LongInt;
  ByteSize: Integer; Parity: TParityType; StopBits: Integer;
  Flags: TSerialFlags);

{ Saves and restores the state of the serial device. }
function SerSaveState(Handle: TSerialHandle): TSerialState;
procedure SerRestoreState(Handle: TSerialHandle; State: TSerialState);

{ Getting and setting the line states directly. }
procedure SerSetDTR(Handle: TSerialHandle; State: Boolean);
procedure SerSetRTS(Handle: TSerialHandle; State: Boolean);
function SerGetCTS(Handle: TSerialHandle): Boolean;
function SerGetDSR(Handle: TSerialHandle): Boolean;
function SerGetRI(Handle: TSerialHandle): Boolean;


{ ************************************************************************** }

implementation


function SerOpen(const DeviceName: String): TSerialHandle;
begin
  Result := CreateFile(PChar('\\.\' + DeviceName),
                       GENERIC_READ or GENERIC_WRITE,
                       0,
                       Nil,
                       OPEN_EXISTING,
                       FILE_ATTRIBUTE_NORMAL,
                       0);
end;

procedure SerClose(Handle: TSerialHandle);
begin
  CloseHandle(Handle);
end;

procedure SerFlush(Handle: TSerialHandle);
begin
  FlushFileBuffers(Handle);
end;

function SerRead(Handle: TSerialHandle; var Buffer; Count: LongInt): LongInt;
begin
  if not ReadFile(Handle, Buffer, Count, DWord(Result), Nil) then Result := -1
end;

function SerWrite(Handle: TSerialHandle; var Buffer; Count: LongInt): LongInt;
begin
  if not WriteFile(Handle, Buffer, Count, DWord(Result), Nil) then Result := -1
end;

procedure SerSetParams(Handle: TSerialHandle; BitsPerSec: LongInt;
  ByteSize: Integer; Parity: TParityType; StopBits: Integer;
  Flags: TSerialFlags);
var
  DCB: TDCB;
  COMMTIMEOUTS: TCOMMTIMEOUTS;

begin
  FillChar(COMMTIMEOUTS, SizeOf(COMMTIMEOUTS), #0);
  COMMTIMEOUTS.ReadIntervalTimeout := MAXDWORD;

  FillChar(DCB, SizeOf(DCB), #0);
  DCB.DCBLength := SizeOf(DCB);

  DCB.Flags := bm_DCB_fBinary;

  case BitsPerSec of
    110: DCB.BaudRate := CBR_110;
    300: DCB.BaudRate := CBR_300;
    600: DCB.BaudRate := CBR_600;
    1200: DCB.BaudRate := CBR_1200;
    2400: DCB.BaudRate := CBR_2400;
    4800: DCB.BaudRate := CBR_4800;
    14400: DCB.BaudRate := CBR_14400;
    19200: DCB.BaudRate := CBR_19200;
    38400: DCB.BaudRate := CBR_38400;
    56000: DCB.BaudRate := CBR_56000;
    57600: DCB.BaudRate := CBR_57600;
    115200: DCB.BaudRate := CBR_115200;
    128000: DCB.BaudRate := CBR_128000;
    256000: DCB.BaudRate := CBR_256000;
    else DCB.BaudRate := CBR_9600;
  end;

  if ByteSize in[4..7] then DCB.ByteSize := ByteSize
   else
     DCB.ByteSize := 8;

  DCB.Parity := Ord(Parity);

  if StopBits = 2 then DCB.StopBits := TWOSTOPBITS;

  if RtsCtsFlowControl in Flags then
    DCB.Flags := DCB.Flags or bm_DCB_fOutxCtsFlow or (bm_DCB_fRtsControl -$1000);

  PurgeComm(Handle, PURGE_TXCLEAR or PURGE_RXCLEAR);
  if not SetCommState(Handle, DCB) then raise Exception.Create('SetCommState Failed!');
  if not SetCommTimeouts(Handle, COMMTIMEOUTS) then raise Exception.Create('SetCommTimeouts Failed!');
end;

function SerSaveState(Handle: TSerialHandle): TSerialState;
begin
  GetCommModemStatus(Handle, Result.LineState);
  GetCommState(Handle, Result.DCB);
end;

procedure SerRestoreState(Handle: TSerialHandle; State: TSerialState);
begin
  SetCommState(Handle, State.DCB);
end;

procedure SerSetDTR(Handle: TSerialHandle; State: Boolean);
begin
  if State then
    EscapeCommFunction(Handle, SETDTR)
  else
    EscapeCommFunction(Handle, CLRDTR);
end;

procedure SerSetRTS(Handle: TSerialHandle; State: Boolean);
begin
  if State then
    EscapeCommFunction(Handle, SETRTS)
  else
    EscapeCommFunction(Handle, CLRRTS);
end;

function SerGetCTS(Handle: TSerialHandle): Boolean;
var
  Flags: Cardinal;
begin
  GetCommModemStatus(Handle, Flags);
  Result := (Flags and MS_CTS_ON) <> 0;
end;

function SerGetDSR(Handle: TSerialHandle): Boolean;
var
  Flags: Cardinal;
begin
  GetCommModemStatus(Handle, Flags);
  Result := (Flags and MS_DSR_ON) <> 0;
end;

function SerGetRI(Handle: TSerialHandle): Boolean;
var
  Flags: Cardinal;
begin
  GetCommModemStatus(Handle, Flags);
  Result := (Flags and MS_RING_ON) <> 0;
end;


end.
