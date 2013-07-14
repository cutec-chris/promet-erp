{**************************************************************************************************}
{                                                                                                  }
{  Y core library (Ycl)                                                                            }
{                                                                                                  }
{  YclZlib - zlib, gzip stream classes                                                             }
{                                                                                                  }
{  The contents of this file are subject to the Y Library Public License Version 1.0 (the          }
{  "License"); you may not use this file except in compliance with the License. You may obtain a   }
{  copy of the License at http://delphi.pjh2.de/                                                   }
{                                                                                                  }
{  Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF  }
{  ANY KIND, either express or implied. See the License for the specific language governing        }
{  rights and limitations under the License.                                                       }
{                                                                                                  }
{  The Original Code is: YclZlib.pas.                                                              }
{  The Initial Developer of the Original Code is Peter J. Haas (libs@pjh2.de). Portions created    }
{  by Peter J. Haas are Copyright (C) 2002-2005 Peter J. Haas. All Rights Reserved.                }
{                                                                                                  }
{  Contributor(s):                                                                                 }
{                                                                                                  }
{  You may retrieve the latest version of this file at the homepage of Peter J. Haas, located at   }
{  http://delphi.pjh2.de/                                                                          }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{  Additional info:                                                                                }
{    RFC 1952: GZIP file format specification version 4.3, 1996, Peter Deutsch                     }
{    ftp://ftp.uu.net/graphics/png/documents/zlib/zdoc-index.html                                  }
{                                                                                                  }
{    The gzip file format, additional informations, Jean-loup Gailly                               }
{    http://www.gzip.org/format.txt                                                                }
{                                                                                                  }
{    gzip format                                                                                   }
{    http://www.onicos.com/staff/iz/formats/gzip.html                                              }
{                                                                                                  }
{**************************************************************************************************}

// For history see end of file

{$I Ycl.inc}

unit YclZlib;

interface
uses
  {$IFDEF WIN32}
  Windows,
  {$ENDIF WIN32}
  {$IFDEF UNIX}
  Types,
  {$ENDIF UNIX}
  {$IFDEF RTL_UNIT_LIBC}
  Libc,
  {$ENDIF RTL_UNIT_LIBC}
  SysUtils, Classes,
  YclBase, YclStrings, YclDateTimes, zlibh;

const
  ZLibStreamDefaultBufferSize = 32 * 1024;

type
  TZLibStream = class(TStream)
  protected
    FStream: TStream;
    FBufferSize: Integer;
    FBuffer: Pointer;
    FZLibStream: TZStreamRec;
    procedure SetSize(NewSize: Longint); override;
  public
    constructor Create(const Stream: TStream; const BufferSize: Integer);
    destructor Destroy; override;
    function Seek(Offset: Longint; Origin: Word): Longint; override;
  end;

  TZLibReader = class(TZLibStream)
  protected
    FEndOfStream: Boolean;
    procedure ReadNextBlock;
    procedure FinishZLibStream;
  public
    constructor Create(const Stream: TStream;
      const BufferSize: Integer{$IFDEF SUPPORTS_DEFAULTPARAMS} = ZLibStreamDefaultBufferSize{$ENDIF};
      const WindowBits: Integer{$IFDEF SUPPORTS_DEFAULTPARAMS} = DEF_WBITS{$ENDIF});
    constructor CreateDef2(const Stream: TStream;
      const BufferSize: Integer{$IFDEF SUPPORTS_DEFAULTPARAMS} = ZLibStreamDefaultBufferSize{$ENDIF});
    constructor CreateDef(const Stream: TStream);
    destructor Destroy; override;

    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(Offset: Longint; Origin: Word): Longint; override;

    procedure Reset;
    procedure SyncZLibStream;

    property EndOfStream: Boolean read FEndOfStream;
  end;

  TZLibWriter = class(TZLibStream)
  protected
    procedure WriteNextBlock;
    procedure FlushZLibStream(const Flush: Integer);
  public
    constructor Create(const Stream: TStream;
      const BufferSize: Integer{$IFDEF SUPPORTS_DEFAULTPARAMS} = ZLibStreamDefaultBufferSize{$ENDIF};
      const Level: Integer{$IFDEF SUPPORTS_DEFAULTPARAMS} = Z_DEFAULT_COMPRESSION{$ENDIF};
      const Strategy: Integer{$IFDEF SUPPORTS_DEFAULTPARAMS} = Z_DEFAULT_STRATEGY{$ENDIF};
      const WindowBits: Integer{$IFDEF SUPPORTS_DEFAULTPARAMS} = DEF_WBITS{$ENDIF});
    constructor CreateDef2(const Stream: TStream;
      const BufferSize: Integer{$IFDEF SUPPORTS_DEFAULTPARAMS} = ZLibStreamDefaultBufferSize{$ENDIF};
      const Level: Integer{$IFDEF SUPPORTS_DEFAULTPARAMS} = Z_DEFAULT_COMPRESSION{$ENDIF});
    constructor CreateDef(const Stream: TStream);
    destructor Destroy; override;

    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(Offset: Longint; Origin: Word): Longint; override;

    procedure Reset;
  end;

  EZLibError = class(EYclException);

// zlib error texts
function GetZlibErrorText(const ErrorCode: Integer): PResStringRec;

function ZLibCompressMem(const Src: Pointer; SrcLen: Integer;
  out Dst: Pointer; out DstLen: Integer; out DstCapacity: Integer;
  const Level: Integer{$IFDEF SUPPORTS_DEFAULTPARAMS} = Z_DEFAULT_COMPRESSION{$ENDIF}): Boolean;

// Flush:
//   Z_SYNC_FLUSH:  DstCapacity can be 0
//   Z_FINISH:      decompress with faster routine in a single step
//                  DstCapacity must be >= uncompressed size
function ZLibDecompressMem(const Src: Pointer; SrcLen: Integer;
  out Dst: Pointer; out DstLen: Integer; var DstCapacity: Integer;
  const Flush: Integer{$IFDEF SUPPORTS_DEFAULTPARAMS} = Z_SYNC_FLUSH{$ENDIF}): Boolean;

// *****************************************************************************

const
  ZLibDefaultLineSeparator = OSLineSeparator;

type
  TGZipStream = class(TStream)
  protected
    FStream: TStream;
    FCRC32: LongWord;
    FUncompressedSize: LongWord;
    procedure SetSize(NewSize: Longint); override;
  public
    constructor Create(const Stream: TStream);
    destructor Destroy; override;
    function Seek(Offset: Longint; Origin: Word): Longint; override;
  end;

  TGZipReader = class(TGZipStream)
  private
    FZLibReader: TZLibReader;
    FTextMode: Boolean;
    FFilename: String;
    FComment: String;
    FTimeStamp: TUnixTime32;
    FLevel: Integer;
    FOperatingSystem: Byte;
    FMultipartNumber: Word;
    FExtraField: Pointer;
    FExtraFieldSize: Integer;
    FEndOfStream: Boolean;
  public
    constructor Create(const Stream: TStream;
      const BufferSize: Integer{$IFDEF SUPPORTS_DEFAULTPARAMS} = ZLibStreamDefaultBufferSize{$ENDIF};
      const LineSeparator: String{$IFDEF SUPPORTS_DEFAULTPARAMS} = ZLibDefaultLineSeparator{$ENDIF});
    constructor CreateDef(const Stream: TStream);
    destructor Destroy; override;

    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;

    property TextMode: Boolean read FTextMode;
    property Filename: String read FFilename;
    property Comment: String read FComment;
    property TimeStamp: TUnixTime32 read FTimeStamp;
    property Level: Integer read FLevel;
    property OperatingSystem: Byte read FOperatingSystem;
    property MultipartNumber: Word read FMultipartNumber;  // 0 = first part
    property ExtraField: Pointer read FExtraField;
    property ExtraFieldSize: Integer read FExtraFieldSize;

    property EndOfStream: Boolean read FEndOfStream;
  end;

  TGZipWriter = class(TGZipStream)
  private
    FTextMode: Boolean;
    FZLibWriter: TZLibWriter;
  public
    constructor Create(const Stream: TStream;
      const BufferSize: Integer{$IFDEF SUPPORTS_DEFAULTPARAMS} = ZLibStreamDefaultBufferSize{$ENDIF};
      const Level: Integer{$IFDEF SUPPORTS_DEFAULTPARAMS} = Z_DEFAULT_COMPRESSION{$ENDIF};
      const Strategie: Integer{$IFDEF SUPPORTS_DEFAULTPARAMS} = Z_DEFAULT_STRATEGY{$ENDIF};
      const Filename: String{$IFDEF SUPPORTS_DEFAULTPARAMS} = ''{$ENDIF};
      const TimeStamp: TUnixTime32{$IFDEF SUPPORTS_DEFAULTPARAMS} = 0{$ENDIF};
      const Comment: String{$IFDEF SUPPORTS_DEFAULTPARAMS} = ''{$ENDIF};
      const TextMode: Boolean{$IFDEF SUPPORTS_DEFAULTPARAMS} = False{$ENDIF};
      const ExtraField: Pointer{$IFDEF SUPPORTS_DEFAULTPARAMS} = Nil{$ENDIF};
      const ExtraFieldSize: Word{$IFDEF SUPPORTS_DEFAULTPARAMS} = 0{$ENDIF});
    // less parameters
    constructor CreateDef2(const Stream: TStream;
      const BufferSize: Integer{$IFDEF SUPPORTS_DEFAULTPARAMS} = ZLibStreamDefaultBufferSize{$ENDIF};
      const Level: Integer{$IFDEF SUPPORTS_DEFAULTPARAMS} = Z_DEFAULT_COMPRESSION{$ENDIF});
    constructor CreateDef(const Stream: TStream);
    destructor Destroy; override;

    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
  end;

  EGZipError = class(EYclException);

// gzip file extension
const
  GZipDefaultFileExtension = '.gz';

// if DstFilename = '' -> DstFilename := SrcFilename + GZipDefaultFileExtension
procedure GZipCompressFile(const SrcFilename: String; DstFilename: String;
  const Level: Integer{$IFDEF SUPPORTS_DEFAULTPARAMS} = Z_DEFAULT_COMPRESSION{$ENDIF});
procedure GZipDecompressFile(const SrcFilename: String; DstFilename: String);

implementation
uses
  {$IFNDEF RTL_XPLATFORM ~}
  YclFileUtils,
  {$ENDIF ~RTL_XPLATFORM}
  YclDTConversions;

resourcestring
{$I YclZlib.rs}

function GetZlibErrorText(const ErrorCode: Integer): PResStringRec;
const
  ErrorTexts: array[-6..2] of PResStringRec = (
    @RsZlibVersionError,
    @RsZlibBufError,
    @RsZlibMemError,
    @RsZlibDataError,
    @RsZlibStreamError,
    @RsZlibErrNo,
    @RsZlibOK,
    @RsZlibStreamEnd,
    @RsZlibNeedDict
  );
begin
  case ErrorCode of
    Low(ErrorTexts)..High(ErrorTexts):
      Result := ErrorTexts[ErrorCode];
  else
    Result := @RsZlibUnknownError;
  end;
end;

// if error then raise exception
// but not for Z_OK, Z_STREAM_END and Z_NEED_DICT
procedure CheckZlib(const ErrorCode: Integer);
begin
  if ErrorCode < Z_OK then
    raise EZLibError.CreateRes(GetZlibErrorText(ErrorCode));
end;


// **************************************  TZLibStream  **************************************

constructor TZLibStream.Create(const Stream: TStream; const BufferSize: Integer);
begin
  inherited Create;

  FStream := Stream;
  // at least 1 byte buffer
  if BufferSize <= 0 then
    FBufferSize := ZLibStreamDefaultBufferSize
  else
    FBufferSize := BufferSize;
  GetMem(FBuffer, FBufferSize);
end;

destructor TZLibStream.Destroy;
begin
  FreeMem(FBuffer, FBufferSize);
  inherited Destroy;
end;

{$HINTS OFF}{$WARNINGS OFF}
procedure TZLibStream.SetSize(NewSize: Longint);
begin
  raise EZLibError.CreateRes(@RsZlibNoSetSize);
end;

function TZLibStream.Seek(Offset: Longint; Origin: Word): Longint;
begin
  raise EZLibError.CreateRes(@RsZlibNoSeek);
end;
{$WARNINGS ON}{$HINTS ON}

// **************************************  TZLibReader  **************************************

constructor TZLibReader.Create(const Stream: TStream;
  const BufferSize: Integer{$IFDEF SUPPORTS_DEFAULTPARAMS} = ZLibStreamDefaultBufferSize{$ENDIF};
  const WindowBits: Integer{$IFDEF SUPPORTS_DEFAULTPARAMS} = DEF_WBITS{$ENDIF});

begin
  inherited Create(Stream, BufferSize);
  FEndOfStream := False;
  ReadNextBlock;
  CheckZlib(inflateInit2(FZLibStream, WindowBits));
end;

constructor TZLibReader.CreateDef2(const Stream: TStream;
  const BufferSize: Integer{$IFDEF SUPPORTS_DEFAULTPARAMS} = ZLibStreamDefaultBufferSize{$ENDIF});
begin
  Create(Stream, BufferSize, DEF_WBITS);
end;

constructor TZLibReader.CreateDef(const Stream: TStream);
begin
  Create(Stream, ZLibStreamDefaultBufferSize, DEF_WBITS);
end;

destructor TZLibReader.Destroy;
begin
  try  // Stream.Seek can raise any Exception
    if not FEndOfStream then
      FinishZLibStream;
  finally
    inherited Destroy;
  end;
end;

procedure TZLibReader.ReadNextBlock;
begin
  if (FZLibStream.avail_in = 0) and (not FEndOfStream) then begin
    FZLibStream.avail_in := FStream.Read(FBuffer^, FBufferSize);
    FZLibStream.next_in := FBuffer;
  end;
end;

procedure TZLibReader.SyncZLibStream;
var
  Err: Integer;
  Buf: array[0..255] of Byte;
begin
  // Skips leaving compressed data
  while not FEndOfStream do begin
    FZLibStream.next_out := PByte(@Buf);
    FZLibStream.avail_out := SizeOf(Buf);
    ReadNextBlock;
    if FZLibStream.avail_in = 0 then
      Exit;  // End of source file
    Err := inflate(FZLibStream, Z_NO_FLUSH);
    FEndOfStream := Err = Z_STREAM_END;  // End of zlib stream
    CheckZlib(Err);
  end;
end;

procedure TZLibReader.FinishZLibStream;
begin
  CheckZlib(inflateEnd(FZLibStream));
  // set stream position to end of zlib stream
  if FZLibStream.avail_in > 0 then
    FStream.Seek(-FZLibStream.avail_in, soFromCurrent);
end;

procedure TZLibReader.Reset;
begin
  SyncZLibStream;
  FEndOfStream := False;
  FZLibStream.total_in := 0;
  FZLibStream.total_out := 0;
  CheckZlib(inflateReset(FZLibStream));
end;

function TZLibReader.Read(var Buffer; Count: Longint): Longint;
var
  Err: Integer;
begin
  Result := 0;
  if FEndOfStream then
    Exit;
  FZLibStream.next_out := @Buffer;
  FZLibStream.avail_out := Count;
  while FZLibStream.avail_out <> 0 do begin
    ReadNextBlock;
    if FZLibStream.avail_in = 0 then
      Exit;  // End of source file
    Err := inflate(FZLibStream, Z_NO_FLUSH);
    FEndOfStream := Err = Z_STREAM_END;  // End of zlib stream
    Result := Count - LongInt(FZLibStream.avail_out);
    if FEndOfStream then begin
      FinishZLibStream;
      Exit;
    end;
    CheckZlib(Err);
  end;
end;

{$HINTS OFF}{$WARNINGS OFF}
function TZLibReader.Write(const Buffer; Count: Longint): Longint;
begin
  raise EZLibError.CreateRes(@RsZlibNoWrite);
end;
{$WARNINGS ON}{$HINTS ON}

function TZLibReader.Seek(Offset: Longint; Origin: Word): Longint;
begin
  if (Offset = 0) and (Origin = soFromCurrent) then  // GetPosition
    Result := FZLibStream.total_out
  else
    Result := inherited Seek(Offset, Origin);
end;


// **************************************  TZLibWriter  **************************************

constructor TZLibWriter.Create(const Stream: TStream;
  const BufferSize: Integer{$IFDEF SUPPORTS_DEFAULTPARAMS} = ZLibStreamDefaultBufferSize{$ENDIF};
  const Level: Integer{$IFDEF SUPPORTS_DEFAULTPARAMS} = Z_DEFAULT_COMPRESSION{$ENDIF};
  const Strategy: Integer{$IFDEF SUPPORTS_DEFAULTPARAMS} = Z_DEFAULT_STRATEGY{$ENDIF};
  const WindowBits: Integer{$IFDEF SUPPORTS_DEFAULTPARAMS} = DEF_WBITS{$ENDIF});
begin
  inherited Create(Stream, BufferSize);
  FZLibStream.next_out := FBuffer;
  FZLibStream.avail_out := FBufferSize;
  CheckZlib(deflateInit2(FZLibStream, Level, Z_DEFLATED, WindowBits, DEF_MEM_LEVEL, Strategy));
end;

constructor TZLibWriter.CreateDef2(const Stream: TStream;
  const BufferSize: Integer{$IFDEF SUPPORTS_DEFAULTPARAMS} = ZLibStreamDefaultBufferSize{$ENDIF};
  const Level: Integer{$IFDEF SUPPORTS_DEFAULTPARAMS} = Z_DEFAULT_COMPRESSION{$ENDIF});
begin
  Create(Stream, BufferSize, Level, Z_DEFAULT_STRATEGY, DEF_WBITS);
end;

constructor TZLibWriter.CreateDef(const Stream: TStream);
begin
  Create(Stream, ZLibStreamDefaultBufferSize, Z_DEFAULT_COMPRESSION,
         Z_DEFAULT_STRATEGY, DEF_WBITS);
end;

destructor TZLibWriter.Destroy;
begin
  FlushZLibStream(Z_FINISH);
  WriteNextBlock;
  CheckZlib(deflateEnd(FZLibStream));
  inherited Destroy;
end;

procedure TZLibWriter.WriteNextBlock;
var
  Len: LongInt;
begin
  Len := FBufferSize - LongInt(FZLibStream.avail_out);
  if Len > 0 then
    FStream.WriteBuffer(FBuffer^, Len);
  FZLibStream.next_out := FBuffer;
  FZLibStream.avail_out := FBufferSize;
end;

procedure TZLibWriter.FlushZLibStream(const Flush: Integer);
var
  Err: Integer;
begin
  FZLibStream.next_in := Nil;
  FZLibStream.avail_in := 0;
  repeat
    if FZLibStream.avail_out = 0 then
      WriteNextBlock;
    Err := deflate(FZLibStream, Flush);
  until Err <> Z_OK;
  CheckZlib(Err);
end;

procedure TZLibWriter.Reset;
begin
  FlushZLibStream(Z_FINISH);
  FZLibStream.total_in := 0;
  FZLibStream.total_out := 0;
  CheckZlib(deflateReset(FZLibStream));
end;

{$HINTS OFF}{$WARNINGS OFF}
function TZLibWriter.Read(var Buffer; Count: Longint): Longint;
begin
  raise EZLibError.CreateRes(@RsZlibNoRead);
end;
{$WARNINGS ON}{$HINTS ON}

function TZLibWriter.Write(const Buffer; Count: Longint): Longint;
var
  Err: Integer;
begin
  Result := 0;
  Err := Z_OK;
  FZLibStream.next_in := @Buffer;
  FZLibStream.avail_in := Count;
  while FZLibStream.avail_in <> 0 do begin
    if FZLibStream.avail_out = 0 then
      WriteNextBlock;
    Err := deflate(FZLibStream, Z_NO_FLUSH);
    Result := Count - LongInt(FZLibStream.avail_in);
    if Err <> Z_OK then Break;
  end;
  CheckZlib(Err);
end;

function TZLibWriter.Seek(Offset: Longint; Origin: Word): Longint;
begin
  if (Offset = 0) and (Origin = soFromCurrent) then  // GetPosition
    Result := FZLibStream.total_in
  else
    Result := inherited Seek(Offset, Origin);
end;

// *******************************************************************************************

function ZLibCompressMem(const Src: Pointer; SrcLen: Integer;
  out Dst: Pointer; out DstLen: Integer; out DstCapacity: Integer;
  const Level: Integer{$IFDEF SUPPORTS_DEFAULTPARAMS} = Z_DEFAULT_COMPRESSION{$ENDIF}): Boolean;
var
  ZLibStream: TZStreamRec;
  Err: Integer;

// calculate DstCapacity, at least 100.1% * SrcLen + 12
procedure CalcDstCapacity(const Estimated: Double);
var
  i: Integer;
begin
  i := Round(Estimated * 1.002) + 12;
  i := (i + 15) and not 15;
  if DstCapacity < i then
    DstCapacity := i
  else
    DstCapacity := DstCapacity + 16;
end;

var
  Temp: Double;
  DstPtr: PByte;
begin
  Result := False;
  Dst := Nil;
  DstLen := 0;
  DstCapacity := 0;
  if (SrcLen = 0) or (not Assigned(Src)) then
    Exit;
    
  CalcDstCapacity(SrcLen);
  GetMem(Dst, DstCapacity);
  try
    FillChar(ZLibStream, SizeOf(ZLibStream), 0);

    ZLibStream.next_in := Src;
    ZLibStream.avail_in := SrcLen;
    ZLibStream.total_in := 0;

    ZLibStream.next_out := Dst;
    ZLibStream.avail_out := DstCapacity;
    ZLibStream.total_out := 0;

    CheckZlib(deflateInit(ZLibStream, Level));
    try
      repeat
        Err := deflate(ZLibStream, Z_FINISH);

        if Err = Z_OK then begin  // there was not enough output space
          Temp := ZLibStream.total_out;
          CalcDstCapacity(SrcLen * (Temp / ZLibStream.total_in));
          ReAllocMem(Dst, DstCapacity);

          DstPtr := Dst;
          Inc(DstPtr, ZLibStream.total_out);
          ZLibStream.next_out := DstPtr;
          ZLibStream.avail_out := LongWord(DstCapacity) - ZLibStream.total_out;
        end;
      until Err <> Z_OK;
      CheckZlib(Err);
    finally
      CheckZlib(deflateEnd(ZLibStream));
    end;
    DstLen := ZLibStream.total_out;
    Result := True;
  except
    FreeMem(Dst);
    Dst := Nil;
    DstLen := 0;
    DstCapacity := 0;
    raise;
  end;
end;

function ZLibDecompressMem(const Src: Pointer; SrcLen: Integer;
  out Dst: Pointer; out DstLen: Integer; var DstCapacity: Integer;
  const Flush: Integer{$IFDEF SUPPORTS_DEFAULTPARAMS} = Z_SYNC_FLUSH{$ENDIF}): Boolean;
var
  ZLibStream: TZStreamRec;
  Err: Integer;

// calculate DstCapacity, 120% of Estimated
procedure CalcDstCapacity(const Estimated: Double);
var
  i: Integer;
begin
  i := Round(Estimated * 1.2);
  i := (i + 15) and not 15;
  if DstCapacity < i then
    DstCapacity := i
  else
    DstCapacity := DstCapacity + 16;
end;

var
  Temp: Double;
  DstPtr: PByte;
begin
  Result := False;
  DstLen := 0;
  Dst := Nil;
  if (SrcLen = 0) or (not Assigned(Src)) then
    Exit;

  if DstCapacity <= 0 then
    CalcDstCapacity(SrcLen * 2);

  GetMem(Dst, DstCapacity);
  try
    FillChar(ZLibStream, SizeOf(ZLibStream), 0);

    ZLibStream.next_in := Src;
    ZLibStream.avail_in := SrcLen;
    ZLibStream.total_in := 0;

    ZLibStream.next_out := Dst;
    ZLibStream.avail_out := DstCapacity;
    ZLibStream.total_out := 0;

    CheckZlib(inflateInit(ZLibStream));
    try
      repeat
        Err := inflate(ZLibStream, Flush);

        if Err = Z_OK then begin  // there was not enough output space
          Temp := ZLibStream.total_out;
          CalcDstCapacity(SrcLen * (Temp / ZLibStream.total_in));
          ReAllocMem(Dst, DstCapacity);

          DstPtr := Dst;
          Inc(DstPtr, ZLibStream.total_out);
          ZLibStream.next_out := DstPtr;
          ZLibStream.avail_out := LongWord(DstCapacity) - ZLibStream.total_out;
        end;
      until Err <> Z_OK;
      CheckZlib(Err);
    finally
      CheckZlib(inflateEnd(ZLibStream));
    end;
    DstLen := ZLibStream.total_out;
    Result := True;
  except
    FreeMem(Dst);
    Dst := Nil;
    DstLen := 0;
    DstCapacity := 0;
    raise;
  end;
end;

// *******************************************************************************************

constructor TGZipStream.Create(const Stream: TStream);
begin
  inherited Create;
  FStream := Stream;
  FCRC32 := crc32(0, Nil, 0);  // get crc32 initial value
  FUncompressedSize := 0;
end;

destructor TGZipStream.Destroy;
begin
  inherited Destroy;
end;

{$HINTS OFF}{$WARNINGS OFF}
procedure TGZipStream.SetSize(NewSize: Longint);
begin
  raise EGZipError.CreateRes(@RsGzipNoSetSize);
end;

function TGZipStream.Seek(Offset: Longint; Origin: Word): Longint;
begin
  raise EGZipError.CreateRes(@RsGzipNoSeek);
end;
{$WARNINGS ON}{$HINTS ON}

const
  gzipMagic = $8B1F;

  gzipFlag_ASCII_FLAG   = $01;  // bit 0 set: file probably ascii text
  gzipFlag_CONTINUATION = $02;  // bit 1 set: continuation of multi-part gzip file
  gzipFlag_EXTRA_FIELD  = $04;  // bit 2 set: extra field present
  gzipFlag_ORIG_NAME    = $08;  // bit 3 set: original file name present
  gzipFlag_COMMENT      = $10;  // bit 4 set: file comment present
  gzipFlag_ENCRYPTED    = $20;  // bit 5 set: file is encrypted
  gzipFlag_RESERVED     = $C0;  // bits 5..7: reserved

constructor TGZipReader.Create(const Stream: TStream;
  const BufferSize: Integer{$IFDEF SUPPORTS_DEFAULTPARAMS} = ZLibStreamDefaultBufferSize{$ENDIF};
  const LineSeparator: String{$IFDEF SUPPORTS_DEFAULTPARAMS} = ZLibDefaultLineSeparator{$ENDIF});
var
  b: Byte;
  c: AnsiChar;
  w: Word;
  Flags: Byte;
  EncryptionHeader: array[0..11] of Byte;  // placeholder
begin
  inherited Create(Stream);

  // check ID
  Stream.ReadBuffer(w, SizeOf(w));
  if w <> gzipMagic then
    raise EGZipError.CreateRes(@RsGzipNoGZipStream);

  // check compression mode
  Stream.ReadBuffer(b, SizeOf(b));
  if not (b in [Z_DEFLATED]) then
    raise EGZipError.CreateRes(@RsGzipNoDeflate);

  // read flags
  Stream.ReadBuffer(Flags, SizeOf(Flags));
  if (Flags and gzipFlag_CONTINUATION) <> 0 then
    raise EGZipError.CreateRes(@RsGzipMultipartNotSupported);
  if (Flags and gzipFlag_ENCRYPTED) <> 0 then
    raise EGZipError.CreateRes(@RsGzipEncryptedNotSupported);
  if (Flags and gzipFlag_RESERVED) <> 0 then
    raise EGZipError.CreateRes(@RsGzipUnknownFlags);

  // get TextMode
  FTextMode := (Flags and gzipFlag_ASCII_FLAG) <> 0;

  // read Timestamp
  Stream.ReadBuffer(FTimeStamp, SizeOf(FTimeStamp));

  // read compression level (extra flags)
  Stream.ReadBuffer(b, SizeOf(b));
  case b of
    2: FLevel := Z_BEST_COMPRESSION;
    4: FLevel := Z_BEST_SPEED;
  else
    FLevel := Z_DEFAULT_COMPRESSION;
  end;

  // read operating system
  Stream.ReadBuffer(FOperatingSystem, SizeOf(FOperatingSystem));

  // read multi-part number (second part = 1)
  if (Flags and gzipFlag_CONTINUATION) <> 0 then
    Stream.ReadBuffer(FMultipartNumber, SizeOf(FMultipartNumber));

  // read ExtraField
  if (Flags and gzipFlag_EXTRA_FIELD) <> 0 then begin
    Stream.ReadBuffer(w, SizeOf(w));
    FExtraFieldSize := w;
    GetMem(FExtraField, FExtraFieldSize);
    Stream.ReadBuffer(FExtraField^, FExtraFieldSize);
  end;

  // read filename
  if (Flags and gzipFlag_ORIG_NAME) <> 0 then begin
    repeat
      Stream.ReadBuffer(c, SizeOf(c));
      if c <> #0 then
        FFilename := FFilename + c;
    until c = #0;
  end;

  // read comment
  if (Flags and gzipFlag_COMMENT) <> 0 then begin
    repeat
      Stream.ReadBuffer(c, SizeOf(c));
      case c of
        #0: ;
        #$0A: FComment := FComment + LineSeparator;  // replace newline
      else
        FComment := FComment + c;
      end;
    until c = #0;
  end;

  // read encryption header
  if (Flags and gzipFlag_ENCRYPTED) <> 0 then
    Stream.ReadBuffer(EncryptionHeader, SizeOf(EncryptionHeader));

  // windowBits is passed < 0 to tell that there is no zlib header
  FZLibReader := TZLibReader.Create(Stream, BufferSize, -MAX_WBITS);
end;

constructor TGZipReader.CreateDef(const Stream: TStream);
begin
  Create(Stream, ZLibStreamDefaultBufferSize);
end;

destructor TGZipReader.Destroy;
begin
  FZLibReader.Free;
  if Assigned(FExtraField) then
    FreeMem(FExtraField);
  inherited Destroy;
end;

function TGZipReader.Read(var Buffer; Count: Longint): Longint;
var
  xCRC32: LongWord;
  xUncompressedSize: LongWord;
begin
  Result := 0;
  if FEndOfStream then
    Exit;
  // read bytes from stream
  Result := FZLibReader.Read(Buffer, Count);
  // calculate CRC and Size
  FCRC32 := crc32(FCRC32, @Buffer, Result);
  FUncompressedSize := FUncompressedSize + LongWord(Result);
  // check end
  FEndOfStream := FZLibReader.EndOfStream;
  if FEndOfStream then
  begin
    FStream.ReadBuffer(xCRC32, SizeOf(xCRC32));
    FStream.ReadBuffer(xUncompressedSize, SizeOf(xUncompressedSize));
    if FCRC32 <> xCRC32 then
      raise EGZipError.CreateRes(@RsGzipCRCError);
    if FUncompressedSize <> xUncompressedSize then
      raise EGZipError.CreateRes(@RsGzipSizeError);
  end;
end;

{$HINTS OFF}{$WARNINGS OFF}
function TGZipReader.Write(const Buffer; Count: Longint): Longint;
begin
  raise EGZipError.CreateRes(@RsGzipNoWrite);
end;
{$WARNINGS ON}{$HINTS ON}

constructor TGZipWriter.Create(const Stream: TStream;
  const BufferSize: Integer{$IFDEF SUPPORTS_DEFAULTPARAMS} = ZLibStreamDefaultBufferSize{$ENDIF};
  const Level: Integer{$IFDEF SUPPORTS_DEFAULTPARAMS} = Z_DEFAULT_COMPRESSION{$ENDIF};
  const Strategie: Integer{$IFDEF SUPPORTS_DEFAULTPARAMS} = Z_DEFAULT_STRATEGY{$ENDIF};
  const Filename: String{$IFDEF SUPPORTS_DEFAULTPARAMS} = ''{$ENDIF};
  const TimeStamp: TUnixTime32{$IFDEF SUPPORTS_DEFAULTPARAMS} = 0{$ENDIF};
  const Comment: String{$IFDEF SUPPORTS_DEFAULTPARAMS} = ''{$ENDIF};
  const TextMode: Boolean{$IFDEF SUPPORTS_DEFAULTPARAMS} = False{$ENDIF};
  const ExtraField: Pointer{$IFDEF SUPPORTS_DEFAULTPARAMS} = Nil{$ENDIF};
  const ExtraFieldSize: Word{$IFDEF SUPPORTS_DEFAULTPARAMS} = 0{$ENDIF});
var
  b: Byte;
  w: Word;
  s: String;
  Flags: Byte;
begin
  inherited Create(Stream);
  FTextMode := TextMode;

  // write ID
  w := gzipMagic;
  Stream.WriteBuffer(w, SizeOf(w));

  // write compression mode
  b := Z_DEFLATED;
  Stream.WriteBuffer(b, SizeOf(b));

  // write flags
  Flags := 0;
  if TextMode then
    Flags := Flags or gzipFlag_ASCII_FLAG;
  if Assigned(ExtraField) and (ExtraFieldSize > 0) then
    Flags := Flags or gzipFlag_EXTRA_FIELD;
  if Length(Filename) > 0 then
    Flags := Flags or gzipFlag_ORIG_NAME;
  if Length(Comment) > 0 then
    Flags := Flags or gzipFlag_COMMENT;
  Stream.WriteBuffer(Flags, SizeOf(Flags));

  // write Timestamp
  Stream.WriteBuffer(TimeStamp, SizeOf(TimeStamp));

  // write compression level
  case Level of
    Z_BEST_COMPRESSION:
      b := 2;
    Z_BEST_SPEED:
      b := 4;
  else
    b := 0;
  end;
  Stream.WriteBuffer(b, SizeOf(b));

  // write operating system
  b := 14;  // VFAT file system (Win95, NT)
  Stream.WriteBuffer(b, SizeOf(b));

  // write ExtraField
  if (Flags and gzipFlag_EXTRA_FIELD) <> 0 then begin
    w := ExtraFieldSize;
    Stream.WriteBuffer(w, SizeOf(w));
    Stream.WriteBuffer(ExtraField^, w);
  end;

  // write filename
  if (Flags and gzipFlag_ORIG_NAME) <> 0 then begin
    s := ExtractFilename(Filename);
    {$IFDEF WIN32}
    // convert to lower case, because caseinsensitive filenames
    s := AnsiLowerCase(s);
    {$ENDIF WIN32}
    Stream.WriteBuffer(Pointer(s)^, StrLen(Pointer(s)) + 1);
  end;

  // write comment
  if (Flags and gzipFlag_COMMENT) <> 0 then begin
    s := ConvertNewLineToLF(Comment);
    Stream.WriteBuffer(Pointer(s)^, Length(s) + 1);
  end;

  // windowBits is passed < 0 to suppress zlib header
  FZLibWriter := TZLibWriter.Create(Stream, BufferSize, Level, Strategie, -MAX_WBITS);
end;

constructor TGZipWriter.CreateDef2(const Stream: TStream;
  const BufferSize: Integer{$IFDEF SUPPORTS_DEFAULTPARAMS} = ZLibStreamDefaultBufferSize{$ENDIF};
  const Level: Integer{$IFDEF SUPPORTS_DEFAULTPARAMS} = Z_DEFAULT_COMPRESSION{$ENDIF});
begin
  Create(Stream, BufferSize, Level,
         Z_DEFAULT_STRATEGY, '', 0, '', False, Nil, 0);
end;

constructor TGZipWriter.CreateDef(const Stream: TStream);
begin
  Create(Stream, ZLibStreamDefaultBufferSize, Z_DEFAULT_COMPRESSION,
         Z_DEFAULT_STRATEGY, '', 0, '', False, Nil, 0);
end;

destructor TGZipWriter.Destroy;
begin
  FZLibWriter.Free;
  FStream.WriteBuffer(FCRC32, SizeOf(FCRC32));
  FStream.WriteBuffer(FUncompressedSize, SizeOf(FUncompressedSize));
  inherited Destroy;
end;

{$HINTS OFF}{$WARNINGS OFF}
function TGZipWriter.Read(var Buffer; Count: Longint): Longint;
begin
  raise EGZipError.CreateRes(@RsGzipNoRead);
end;
{$WARNINGS ON}{$HINTS ON}

function TGZipWriter.Write(const Buffer; Count: Longint): Longint;
var
  s: String;
  p: Pointer;
begin
  if FTextMode then begin
    SetString(s, PChar(@Buffer), Count);
    s := ConvertNewLineToLF(s);
    p := Pointer(s);
    Count := Length(s);
  end
  else
    p := @Buffer;
  Result := FZLibWriter.Write(p^, Count);
  // calculate CRC and Size
  FCRC32 := crc32(FCRC32, p, Result);
  FUncompressedSize := FUncompressedSize + LongWord(Result);
end;

// ****************  gzip file support  *****************************

const
  MaxBufferSize = 1024 * 1024;  // 1 MByte
  BufferBlockSize = 32 * 1024;

procedure GZipCompressFile(const SrcFilename: String; DstFilename: String;
  const Level: Integer{$IFDEF SUPPORTS_DEFAULTPARAMS} = Z_DEFAULT_COMPRESSION{$ENDIF});
var
  Src, Dst: TFileStream;
  DstPath: String;
  gzip: TGZipWriter;
  {$IFDEF WIN32}
  FileInfo: TByHandleFileInformation;
  {$ENDIF WIN32}
  {$IFDEF UNIX}
  FileInfo: TStatBuf;
  TimeInfo: TUTimeBuffer;
  {$ENDIF UNIX}
  GZipTime: TUnixTime32;
  Buffer: Pointer;
  BufferSize: Integer;
  BlockSize: Integer;
begin
  Src := TFileStream.Create(SrcFilename, fmOpenRead or fmShareDenyWrite);
  try
    // Get file time stamp and attributes
    {$IFDEF WIN32}
    if not GetFileInformationByHandle(Src.Handle, FileInfo) then
      RaiseLastOSError;
    GZipTime := FileTimeToUnixTime(FileInfo.ftLastWriteTime);
    {$ENDIF WIN32}
    {$IFDEF UNIX}
    if stat(PChar(SrcFileName), FileInfo) <> 0 then
      RaiseLastOSError;
    GZipTime := FileInfo.st_mtime;
    {$ENDIF UNIX}
    // Allocate Buffer
    if Src.Size > MaxBufferSize then  // Limit the buffer size
      BufferSize := MaxBufferSize
    else
      BufferSize := Integer(Src.Size);
    DstPath := ExtractFilePath(DstFilename);
    DstFilename := ExtractFilename(DstFilename);

    if DstPath = '' then begin
      DstPath := ExtractFilePath(SrcFilename);  // path from SrcFilename
    end;

    if DstFilename = '' then begin
      DstFilename := ExtractFilename(SrcFilename) + GZipDefaultFileExtension;
    end;

    DstFilename := DstPath + DstFilename;

    Dst := TFileStream.Create(DstFilename, fmCreate);
    try
      gzip := TGZipWriter.Create(Dst, ZLibStreamDefaultBufferSize,
        Level, Z_DEFAULT_STRATEGY, SrcFilename, GZipTime, '', False, Nil, 0);
      try
        if BufferSize > 0 then begin
          GetMem(Buffer, BufferSize);
          try
            repeat
              BlockSize := Src.Read(Buffer^, BufferSize);
              gzip.WriteBuffer(Buffer^, BlockSize);
            until BlockSize <> BufferSize;
          finally
            FreeMem(Buffer, BufferSize);
          end;
        end;
      finally
        gzip.Free;
      end;
      // set file time stamp (without error check)
      {$IFDEF WIN32}
      Windows.SetFileTime(Dst.Handle, @FileInfo.ftCreationTime, @FileInfo.ftLastAccessTime,
          @FileInfo.ftLastWriteTime);
      {$ENDIF WIN32}
    finally
      Dst.Free;
    end;
    {$IFDEF UNIX}
    // set file time stamp (without error check)
    TimeInfo.actime := FileInfo.st_atime;   // Access time
    TimeInfo.modtime := FileInfo.st_mtime;  // Modification time
    utime(PChar(DstFilename), @TimeInfo);
    // set file attributes (without error check)
    chmod(PChar(DstFilename), FileInfo.st_mode and $00000FFF);
    chown(PChar(DstFilename), FileInfo.st_uid, FileInfo.st_gid);
    {$ENDIF UNIX}
    {$IFDEF WIN32}
    // set file attributes (without error check)
    SetFileAttributes(PChar(DstFilename), FileInfo.dwFileAttributes);
    {$ENDIF WIN32}
  finally
    Src.Free;
  end;
end;

procedure GZipDecompressFile(const SrcFilename: String; DstFilename: String);
var
  Src, Dst: TFileStream;
  DstPath: String;
  gzip: TGZipReader;
  {$IFDEF WIN32}
  FileInfo: TByHandleFileInformation;
  {$ENDIF WIN32}
  {$IFDEF UNIX}
  FileInfo: TStatBuf;
  TimeInfo: TUTimeBuffer;
  {$ENDIF UNIX}
  Buffer: Pointer;
  BufferSize: Integer;
  BlockSize: Integer;
begin
  Src := TFileStream.Create(SrcFilename, fmOpenRead or fmShareDenyWrite);
  try
    // Get file time stamp and attributes
    {$IFDEF WIN32}
    if not GetFileInformationByHandle(Src.Handle, FileInfo) then
      RaiseLastOSError;
    {$ENDIF WIN32}
    {$IFDEF UNIX}
    if stat(PChar(SrcFileName), FileInfo) <> 0 then
      RaiseLastOSError;
    {$ENDIF UNIX}
    // Allocate Buffer
    { TODO : Warum div 2? }
    if Src.Size > (MaxBufferSize div 2) then
      BufferSize := MaxBufferSize  // Limit the buffer size
    else
      BufferSize := ((Integer(Src.Size) * 2 + BufferBlockSize - 1) div BufferBlockSize) * BufferBlockSize;

    gzip := TGZipReader.Create(Src, ZLibStreamDefaultBufferSize);
    try
      DstPath := ExtractFilePath(DstFilename);
      DstFilename := ExtractFilename(DstFilename);

      if DstPath = '' then
        DstPath := ExtractFilePath(SrcFilename);  // path from SrcFilename

      if DstFilename = '' then begin  // no override destination filename
        if gzip.Filename = '' then  // no gzip name field
          // use the filename from SrcFilename without the last ext. part
          DstFilename := ChangeFileExt(ExtractFilename(SrcFilename), '')
        else
          DstFilename := gzip.Filename;  // use the gzip name field
      end;

      DstFilename := DstPath + DstFilename;

      Dst := TFileStream.Create(DstFilename, fmCreate);
      try
        GetMem(Buffer, BufferSize);
        try
          while not gzip.EndOfStream do begin
            BlockSize := gzip.Read(Buffer^, BufferSize);
            Dst.WriteBuffer(Buffer^, BlockSize);
            if BlockSize <> BufferSize then
              Break;
          end;
        finally
          FreeMem(Buffer, BufferSize);
        end;
        {$IFDEF WIN32}
        // set file time stamp (without error check)
        if gzip.TimeStamp <> 0 then
          FileInfo.ftLastWriteTime := UnixTimeToFileTime(gzip.TimeStamp);
        Windows.SetFileTime(Dst.Handle, @FileInfo.ftCreationTime, @FileInfo.ftLastAccessTime,
            @FileInfo.ftLastWriteTime);
        {$ENDIF WIN32}
      finally
        Dst.Free;
      end;
      {$IFDEF UNIX}
      // set file time stamp (without error check)
      TimeInfo.actime := FileInfo.st_atime;     // Access time
      if gzip.TimeStamp = 0 then                // Modification time
        TimeInfo.modtime := FileInfo.st_mtime
      else
        TimeInfo.modtime := gzip.TimeStamp;
      utime(PChar(DstFilename), @TimeInfo);
      // set file attributes (without error check)
      chmod(PChar(DstFilename), FileInfo.st_mode and $00000FFF);
      chown(PChar(DstFilename), FileInfo.st_uid, FileInfo.st_gid);
      {$ENDIF UNIX}
      {$IFDEF WIN32}
      // set file attributes (without error check)
      SetFileAttributes(PChar(DstFilename), FileInfo.dwFileAttributes);
      {$ENDIF WIN32}
    finally
      gzip.Free;
    end;
  finally
    Src.Free;
  end;
end;

// *******************************************************************************************

//  History:
//  2005-03-20, Peter J. Haas
//   - add compiler options to remove FPC hints and warnings
//
//  2005-03-07, Peter J. Haas
//   - some modifications to remove FPC hints
//
//  2005-02-09, Peter J. Haas
//   - YCL version, split zlib, gzip and tar 
// 
//  2005-02-06, Peter J. Haas
//   - use Yl.inc instead of pjh.inc
//
//  2004-05-25, Peter J. Haas
//   -
//
//  2004-05-09, Peter J. Haas
//   - change interface crc32 to avoid FPC compatibility problems
//   - change GetZlibErrorText implementation for FPC compatibility
//   - change OctalToInt implementation FPC compatibility
//   - change Char to AnsiChar in fixed structures
//
//  2004-03-20, Peter J. Haas
//   - Bugfix: TGZipReader.Create: read multi-part number
//
//  2004-03-17 Version 2.0, Peter J. Haas
//   - zlibs, gzips, tars -> zlibs
//
//  2003-04-22 Version 1.0.1, Peter J. Haas
//   - Interface GZipCompressFile and GZipDecompressFile changed
//   - GZipCompressFile, GZipDecompressFile for Linux
//
//  2003-04-19 Version 1.0.1, Peter J. Haas
//   - Interface ZLibCompressMem and ZLibDecompressMem changed
//   - Bugfix: ZLibDecompressMem
//
//  2003-04-14 Version 1.0, Peter J. Haas
//   - First public version
//
//  2002-04-07 Version 0.9, Peter J. Haas
//   - First public pre release

end.
