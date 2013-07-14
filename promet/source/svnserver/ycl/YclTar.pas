{**************************************************************************************************}
{                                                                                                  }
{  Y core library (Ycl)                                                                            }
{                                                                                                  }
{  YclTar - tar stream classes                                                                     }
{                                                                                                  }
{  The contents of this file are subject to the Y Library Public License Version 1.0 (the          }
{  "License"); you may not use this file except in compliance with the License. You may obtain a   }
{  copy of the License at http://delphi.pjh2.de/                                                   }
{                                                                                                  }
{  Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF  }
{  ANY KIND, either express or implied. See the License for the specific language governing        }
{  rights and limitations under the License.                                                       }
{                                                                                                  }
{  The Original Code is: YclTar.pas.                                                               }
{  The Initial Developer of the Original Code is Peter J. Haas (libs@pjh2.de). Portions created    }
{  by Peter J. Haas are Copyright (C) 2002-2005 Peter J. Haas. All Rights Reserved.                }
{                                                                                                  }
{  Contributor(s):                                                                                 }
{                                                                                                  }
{  You may retrieve the latest version of this file at the homepage of Peter J. Haas, located at   }
{  http://delphi.pjh2.de/                                                                          }
{                                                                                                  }
{**************************************************************************************************}

// For history see end of file

{$I Ycl.inc}

unit YclTar;

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
  YclBase, YclDateTimes, zlibh;

const
  TarBlockSize = 512;

type
  TTarArchiveFormat = (
    tafDefaultFormat,    // format to be decided later
    tafV7Format,         // old V7 tar format
    tafOldGnuFormat,     // GNU format as per before tar 1.12
    tafPosixFormat,      // restricted, pure POSIX format
    tafGnuFormat);       // POSIX format with GNU extensions

type
  PSparse = ^TSparse;
  TSparse = packed record                // offset
    Offset: array[0..11] of AnsiChar;    //  $00
    NumBytes: array[0..11] of AnsiChar;  //  $0C
  end;                                   //  $18

  PTarHeader = ^TTarHeader;
  TTarHeader = packed record       // offset
  case Integer of
    0: (Buffer: array[0..TarBlockSize - 1] of Byte);
    1: (
      // Old UNIX TAR format
      Name: array[0..99] of AnsiChar;          //  $000     Char + #0   / mit 0 gefüllt
      Mode: array[0..7] of AnsiChar;           //  $064     Octal + ' '#0   9 + 3 bits              20 34 30 37 35 35 20 00
      UID: array[0..7] of AnsiChar;            //  $06C     Octal + ' '#0   ignore on DOS           20 20 31 37 35 36 20 00
      GID: array[0..7] of AnsiChar;            //  $074     Octal + ' '#0   ignore on DOS           20 20 20 31 34 34 20 00
      Size: array[0..11] of AnsiChar;          //  $07C     Octal + ' '     size in bytes           20 20 20 20 20 20 20 20 20 20 30 20
      MTime: array[0..11] of AnsiChar;         //  $088     Octal + ' '     last modify Unix        20 36 37 32 32 34 34 36 31 30 37 20
      Chksum: array[0..7] of AnsiChar;         //  $094     Octal + ' '#0   >= 17 bit, init 0, add  20 20 37 35 37 32 00 20
      TypeFlag: AnsiChar;                      //  $09C     Octal           + ' '#0 ??              35
      Linkname: array[0..99] of AnsiChar;      //  $09D     Char + #0
      // Extension of POSIX P1003.1
      Magic: array[0..5] of AnsiChar;          //  $101     Char + #0                               75 73 74 61 72 20
      Version: array[0..1] of AnsiChar;        //  $107     Octal + ' '                             20 00
      UName: array[0..31] of AnsiChar;         //  $109     Char + #0                               72 63 64 00 ...
      GName: array[0..31] of AnsiChar;         //  $129     Char + #0                               75 73 65 72 73 00 ...
      DevMajor: array[0..7] of AnsiChar;       //  $149     Octal + ' '#0
      DevMinor: array[0..7] of AnsiChar;       //  $151     Octal + ' '#0
    case TTarArchiveFormat of
      tafV7Format: (
        FillV7: array[0..166] of AnsiChar);    //  $159
      tafPosixFormat: (
        Prefix: array[0..154] of AnsiChar;     //  $159         Prefix for name
        FillPosix: array[0..11] of AnsiChar);  //  $1F4
      tafOldGnuFormat: (
        ATime: array[0..11] of AnsiChar;       //  $159
        CTime: array[0..11] of AnsiChar;       //  $165
        Offset: array[0..11] of AnsiChar;      //  $171
        Longnames: array[0..3] of AnsiChar;    //  $17D
        Pad: AnsiChar;                         //  $181
        Sparses: array[0..3] of TSparse;       //  $182
        IsExtended: AnsiChar;                  //  $1E2
        RealSize: array[0..11] of AnsiChar;    //  $1E3
        FillGnu: array[0..16] of AnsiChar));   //  $1EF
  end;                                         //  $200

// ModeFlag Flags
type
  TTarMode = (
    tmOtherExec,   // execute/search by other
    tmOtherWrite,  // write by other
    tmOtherRead,   // read by other
    tmGroupExec,   // execute/search by group
    tmGroupWrite,  // write by group
    tmGroupRead,   // read by group
    tmOwnerExec,   // execute/search by owner
    tmOwnerWrite,  // write by owner
    tmOwnerRead,   // read by owner
    tmSaveText,    // reserved
    tmSetGID,      // set GID on execution
    tmSetUID);     // set UID on execution
  TTarModes = set of TTarMode;

// TypeFlag
type
  TTarTypeFlag = AnsiChar;

const                     //                     V7  Posix
  ttfRegFile      = '0';  // regular file         x    x
  ttfARegFile     = #0;   // regular file         x    x
  ttfLink         = '1';  // link                 x    x
  ttfSymbolicLink = '2';  // symbolic link        x
  ttfCharacter    = '3';  // character special         x
  ttfBlock        = '4';  // block special             x
  ttfDirectory    = '5';  // directory                 x
  ttfFIFO         = '6';  // FIFO special              x
  ttfContiguous   = '7';  // contiguous file

  // GNU extensions
  ttfGnuDumpDir   = 'D';
  ttfGnuLongLink  = 'K';  // next file have a long link name
  ttfGnuLongName  = 'L';  // next file have a long name
  ttfGnuMultiVol  = 'M';  // file began on another volume
  ttfGnuNames     = 'N';  // long filename
  ttfGnuSparse    = 'S';  // sparse files
  ttfGnuVolHeader = 'V';  // Volume label (must be the first file)

const
  TarOldGnuMagic = 'ustar  '#0;  // old GNU  Magic + Version
  TarPosixMagic  = 'ustar'#0;    // Posix or GNU
  TarGnuVersion  = '00';

  // other version for GNU-Magic:  'GNUtar '#0

// *****************************************************************************

type
  TTarFileType = (tftUnknown, tftEof, tftFile, tftDirectory);
  {$IFDEF SUPPORTS_INT64}
  TTarFileSize = Int64;
  {$ELSE SUPPORTS_INT64~}
  TTarFileSize = LongInt;
  {$ENDIF ~SUPPORTS_INT64}

  TTarReader = class(TObject)
  private
    function GetFileDateTime: TDateTime;
  protected
    FTarStream: TStream;
    FHeader: TTarHeader;
    FArchiveFormat: TTarArchiveFormat;
    FFileType: TTarFileType;
    FFilename: String;
    FFileSize: TTarFileSize;
    FFileTime: TUnixTime32;
    function ReadHeader: Boolean;  // False if Eof
    procedure ScanHeader;
  public
    constructor Create(const TarStream: TStream);
    procedure CopyToStream(const FileStream: TStream;
        CanSeek: Boolean{$IFDEF SUPPORTS_DEFAULTPARAMS} = False{$ENDIF});
    procedure CopyToFile(const FilePath: String);
    procedure SkipFile;
    procedure SkipFileSeek;
    property FileType: TTarFileType read FFileType;
    property Filename: String read FFilename;
    property FileSize: TTarFileSize read FFileSize;
    property FileTime: TUnixTime32 read FFileTime;
    property FileDateTime: TDateTime read GetFileDateTime;
  end;

  TTarWriter = class(TObject)
  protected
    FTarStream: TStream;
    procedure AddEof;
  public
    constructor Create(const TarStream: TStream);
    destructor Destroy; override;
    procedure AddFile(FileRoot, Filename: String);
    procedure AddStream(const Stream: TStream; Filename: String;
      FileSize: TTarFileSize; FileTime: TUnixTime32);
    procedure AddDirectory(DirName: String);
  end;

  ETarError = class(EYclException);

procedure TarAllFiles(const TarFilename, FileRoot: String);
procedure TarFileList(const TarFilename, FileRoot: String; List: TStrings);
procedure TarFileArray(const TarFilename, FileRoot: String; const Filenames: array of String);
procedure TarGZipAllFiles(const TgzFilename, FileRoot: String);
procedure TarGZipFileList(const TgzFilename, FileRoot: String; List: TStrings);
procedure TarGZipFileArray(const TgzFilename, FileRoot: String; const Filenames: array of String);

procedure UnTarAllFiles(const TarFilename: String; DstDir: String);
procedure UnGZipTarAllFiles(const TgzFilename: String; DstDir: String);

implementation
uses
  YclDTConversions, YclStrings, YclFileUtils, YclZlib;

resourcestring
{$I YclTar.rs}

function OctalToInt(const Value: array of AnsiChar; MaxValue: TTarFileSize): TTarFileSize;
var
  i: Integer;
  V: String;
  C: AnsiChar;
begin
  i := Low(Value);
  while (i <= High(Value)) and (Value[i] <> #0) do
    Inc(i);
  SetString(V, PChar(@Value), i);
  V := Trim(V);
  // convert
  Result := 0;
  for i := 1 to Length(V) do begin
    C := V[i];
    case C of
      '0'..'7':
        Result := (Result shl 3) or (Ord(C) - Ord('0'));
    else
      raise ETarError.CreateResFmt(@RsTarOctalToIntInvalidCharacters, [V]);
    end;
  end;
  // check range
  if Result > MaxValue then
    raise ETarError.CreateResFmt(@RsTarOctalToIntOutOfRange, [V]);
end;

function CalculateTarChecksum(Header: TTarHeader): Integer;
var
  i: Integer;
begin
  Result := ($09C - $094) * Ord(' ');
  for i := 0 to $093 do
    Result := Result + Header.Buffer[i];
  for i := $09C to $1FF do
    Result := Result + Header.Buffer[i];
end;

constructor TTarReader.Create(const TarStream: TStream);
begin
  inherited Create;
  FTarStream := TarStream;
  if ReadHeader then
    ScanHeader;
end;

function TTarReader.ReadHeader: Boolean;
var
  i: Integer;
  IsNullBlock: Boolean;
begin
  Result := FTarStream.Read(FHeader, SizeOf(FHeader)) = SizeOf(FHeader);
  if Result then begin
    // check for 0 block
    IsNullBlock := True;
    for i := Low(FHeader.Buffer) to High(FHeader.Buffer) do begin
      if FHeader.Buffer[i] <> 0 then begin
        IsNullBlock := False;
        Break;
      end;
    end;
    if IsNullBlock then
      Result := ReadHeader;
  end
  else begin
    FArchiveFormat := Low(FArchiveFormat);
    FFileType := tftEof;
  end;
end;

procedure TTarReader.ScanHeader;
var
  Prefix: String;
begin
  if FFileType = tftEof then
    Exit;
  // get archive format
  if StrLComp(FHeader.Magic, TarOldGnuMagic, SizeOf(TarOldGnuMagic)) = 0 then
    FArchiveFormat := tafOldGnuFormat
  else if StrLComp(FHeader.Magic, TarPosixMagic, SizeOf(TarPosixMagic)) = 0 then
    FArchiveFormat := tafPosixFormat  // or tafGnuFormat
  else
    FArchiveFormat := tafV7Format;
  // get file type
  case FHeader.TypeFlag of
    ttfRegFile, ttfARegFile:
      FFileType := tftFile;
    ttfDirectory:
      FFileType := tftDirectory;
  else
    FFileType := tftUnknown;
  end;
  // get file name
  FFilename := FHeader.Name;
  case FArchiveFormat of
    tafPosixFormat, tafGnuFormat: begin
      Prefix := FHeader.Prefix;
      if Prefix <> '' then
        FFilename := Prefix + UnixPathDelimiter + FFilename;
    end;
  end;
  {$IFNDEF UNIX}
  // correct path delimiter
  FFilename := UnixPathToOSPath(FFilename);
  {$ENDIF ~UNIX}
  // get file size
  FFileSize := OctalToInt(FHeader.Size, High(FFileSize));
  // get file date
  FFileTime := TUnixTime32(OctalToInt(FHeader.MTime, High(FFileTime)));
  if OctalToInt(FHeader.Chksum, High(Integer)) <> CalculateTarChecksum(FHeader) then
    raise ETarError.CreateRes(@RsTarChecksumError);
end;

procedure TTarReader.CopyToStream(const FileStream: TStream;
  CanSeek: Boolean{$IFDEF SUPPORTS_DEFAULTPARAMS} = False{$ENDIF});
var
  Buffer: array[0..TarBlockSize - 1] of Byte;
  Blocks, i: Integer;
  ReadedBytes: Integer;
  RestBytes: Integer;
begin
  Blocks := (FFileSize + (TarBlockSize - 1)) div TarBlockSize;
  if not Assigned(FileStream) and CanSeek then begin
    FTarStream.Seek(Blocks * TarBlockSize, soFromCurrent);
  end
  else begin
    RestBytes := FFileSize;
    for i := 0 to Blocks - 1 do begin
      ReadedBytes := FTarStream.Read(Buffer, TarBlockSize);
      // schreiben
      if Assigned(FileStream) then begin
        if ReadedBytes > RestBytes then
          ReadedBytes := RestBytes;
        FileStream.WriteBuffer(Buffer, ReadedBytes);
        RestBytes := RestBytes - ReadedBytes;
      end;
    end;
  end;
  if ReadHeader then
    ScanHeader;
end;

procedure TTarReader.CopyToFile(const FilePath: String);
var
  Filename: String;
  FileDir: String;
  FileStream: TFileStream;
  UnixFileTime: TUnixTime32;
  {$IFDEF WIN32}
  FileTime: TFileTime;
  {$ENDIF WIN32}
  {$IFDEF UNIX}
  TimeInfo: TUTimeBuffer;
  {$ENDIF UNIX}
begin
  Filename := IncludeTrailingPathDelimiter(FilePath) + FFilename;
  FileDir := ExcludeTrailingPathDelimiter(ExtractFilePath(Filename));
  if FileDir <> '' then
    ForceDirectories(FileDir);
  FileStream := TFileStream.Create(Filename, fmCreate);
  try
    UnixFileTime := FFileTime;
    CopyToStream(FileStream, False);
    {$IFDEF WIN32}
    FileTime := UnixTimeToFileTime(UnixFileTime);
    Windows.SetFileTime(FileStream.Handle, Nil, Nil, @FileTime);  // without error handling
    {$ENDIF WIN32}
    {$IFDEF UNIX}
    TimeInfo.actime := UnixFileTime;
    TimeInfo.modtime := UnixFileTime;
    utime(PChar(FileName), @TimeInfo);  // without error handling
    {$ENDIF UNIX}
  finally
    FileStream.Free;
  end;
end;

procedure TTarReader.SkipFile;
begin
  CopyToStream(Nil, False);
end;

procedure TTarReader.SkipFileSeek;
begin
  CopyToStream(Nil, True);
end;

function TTarReader.GetFileDateTime: TDateTime;
begin
  Result := UnixTimeToDateTime(FFileTime);
end;

procedure UnTarAllFiles(const TarFilename: String; DstDir: String);
var
  TarFile: TFileStream;
  TarReader: TTarReader;
  DstFilename: String;
begin
  if DstDir = '' then
    DstDir := ChangeFileExt(TarFilename, '')
  else
    DstDir := ExcludeTrailingPathDelimiter(DstDir);
  if DstDir = TarFilename then
    DstDir := DstDir + '.dir';
  DstDir := DstDir + OSPathDelimiter;

  TarFile := TFileStream.Create(TarFilename, fmOpenRead or fmShareDenyWrite);
  try
    TarReader := TTarReader.Create(TarFile);
    try
      while TarReader.FileType <> tftEof do begin
        DstFilename := DstDir + TarReader.Filename;
        case TarReader.FileType of
          tftFile:
            TarReader.CopyToFile(DstDir);
          tftDirectory: begin
            ForceDirectories(DstFilename);
            TarReader.SkipFile;
          end;
        else
          TarReader.SkipFile;
        end;
      end;
    finally
      TarReader.Free;
    end;
  finally
    TarFile.Free;
  end;
end;

constructor TTarWriter.Create(const TarStream: TStream);
begin
  inherited Create;
  FTarStream := TarStream;
end;

destructor TTarWriter.Destroy;
begin
  AddEof;
  inherited Destroy;
end;

procedure TTarWriter.AddFile(FileRoot, Filename: String);
var
  FileStream: TFileStream;
  {$IFDEF WIN32}
  FileInfo: TByHandleFileInformation;
  {$ENDIF WIN32}
  {$IFDEF UNIX}
  FileInfo: TStatBuf;
  {$ENDIF}
  FileUnixTime: TUnixTime32;
begin
  FileRoot := IncludeTrailingPathDelimiter(FileRoot);
  FileStream := TFileStream.Create(FileRoot + Filename, fmOpenRead or fmShareDenyWrite);
  try
    FillChar(FileInfo, SizeOf(FileInfo), 0);
    {$IFDEF WIN32}
    if GetFileInformationByHandle(FileStream.Handle, FileInfo) then
      FileUnixTime := FileTimeToUnixTime(FileInfo.ftLastWriteTime)
    else
      FileUnixTime := 0;
    {$ENDIF WIN32}
    {$IFDEF UNIX}
    if fstat(FileStream.Handle, FileInfo) = 0 then
      FileUnixTime := FileInfo.st_mtime
    else
      FileUnixTime := 0;
    {$ENDIF UNIX}
    AddStream(FileStream, Filename, FileStream.Size, FileUnixTime);
  finally
    FileStream.Free;
  end;
end;

procedure SetOctal(var Field: array of AnsiChar; Value: TTarFileSize);
var
  V: AnsiString;
  Delta: Integer;
  i: Integer;
begin
  V := '';
  repeat
    V := Chr((Value and $07) + Ord('0')) + V;
    Value := Value shr 3;
  until Value = 0;
  Delta := Length(Field) - Length(V);
  if Delta < 0 then
    raise ETarError.CreateRes(@RsTarSetOctalOutOfRange)
  else if Delta > 1 then begin
    // fill with 0
    for i := 2 to Delta do
      V := '0' + V;
  end;
  for i := 1 to Length(V) do
    Field[i - 1] := V[i];
end;

procedure TTarWriter.AddStream(const Stream: TStream; Filename: String;
  FileSize: TTarFileSize; FileTime: TUnixTime32);
var
  i: Integer;
  Header: TTarHeader;
  Blocks: Integer;
  RestBytes: Integer;
begin
  {$IFNDEF UNIX}
  // path delimiter -> UNIX
  Filename := OSPathToUnixPath(Filename);
  {$ENDIF ~UNIX}
  FillChar(Header, SizeOf(Header), 0);
  StrPLCopy(Header.Name, Filename, Length(Header.Name) - 1);
  StrCopy(Header.Mode, '0000777');
  StrCopy(Header.UID, '0000000');
  StrCopy(Header.GID, '0000000');
  SetOctal(Header.Size, FileSize);
  SetOctal(Header.MTime, FileTime);
  Header.TypeFlag := '0';
  SetOctal(Header.Chksum, CalculateTarChecksum(Header));
  FTarStream.WriteBuffer(Header, SizeOf(Header));
  Blocks := FileSize div TarBlockSize;
  RestBytes := FileSize mod TarBlockSize;
  for i := 0 to Blocks - 1 do begin
    Stream.ReadBuffer(Header.Buffer, SizeOf(Header.Buffer));
    FTarStream.WriteBuffer(Header.Buffer, SizeOf(Header.Buffer));
  end;
  if RestBytes > 0 then begin
    FillChar(Header.Buffer, SizeOf(Header.Buffer), 0);
    Stream.ReadBuffer(Header.Buffer, RestBytes);
    FTarStream.WriteBuffer(Header.Buffer, SizeOf(Header.Buffer));
  end;
end;

procedure TTarWriter.AddDirectory(DirName: String);
var
  Header: TTarHeader;
begin
  DirName := IncludeTrailingPathDelimiter(DirName);
  {$IFNDEF UNIX ~}
  // path delimiter -> UNIX
  DirName := OSPathToUnixPath(DirName);
  {$ENDIF ~UNIX}
  FillChar(Header, SizeOf(Header), 0);
  StrPLCopy(Header.Name, DirName, Length(Header.Name) - 1);
  StrCopy(Header.Mode, '0000777');
  StrCopy(Header.UID, '0000000');
  StrCopy(Header.GID, '0000000');
  SetOctal(Header.Size, 0);
  SetOctal(Header.MTime, 0);
  Header.TypeFlag := '5';
  SetOctal(Header.Chksum, CalculateTarChecksum(Header));
  FTarStream.WriteBuffer(Header, SizeOf(Header));
end;

procedure TTarWriter.AddEof;
var
  Buffer: array[0..TarBlockSize - 1] of Byte;
begin
  FillChar(Buffer, SizeOf(Buffer), 0);
  FTarStream.WriteBuffer(Buffer, SizeOf(Buffer));
end;

procedure UnGZipTarAllFiles(const TgzFilename: String; DstDir: String);
var
  TgzFile: TFileStream;
  GZipReader: TGZipReader;
  TarReader: TTarReader;
  DstFilename: String;
begin
  if DstDir = '' then
    DstDir := ChangeFileExt(TgzFilename, '')
  else
    DstDir := ExcludeTrailingPathDelimiter(DstDir);
  if DstDir = TgzFilename then
    DstDir := DstDir + '.dir';
  DstDir := DstDir + OSPathDelimiter;

  TgzFile := TFileStream.Create(TgzFilename, fmOpenRead or fmShareDenyWrite);
  try
    GZipReader := TGZipReader.Create(TgzFile, ZLibStreamDefaultBufferSize);
    try
      TarReader := TTarReader.Create(GZipReader);
      try
        while TarReader.FileType <> tftEof do begin
          DstFilename := DstDir + TarReader.Filename;
          case TarReader.FileType of
            tftFile:
              TarReader.CopyToFile(DstDir);
            tftDirectory: begin
              ForceDirectories(DstFilename);
              TarReader.SkipFile;
            end;
          else
            TarReader.SkipFile;
          end;
        end;
      finally
        TarReader.Free;
      end;
    finally
      GZipReader.Free;
    end;
  finally
    TgzFile.Free;
  end;
end;

procedure TarFileList(const TarFilename, FileRoot: String; List: TStrings);
var
  TarFile: TFileStream;
  TarWriter: TTarWriter;
  i: Integer;
  Filename: String;
begin
  TarFile := TFileStream.Create(TarFilename, fmCreate);
  try
    TarWriter := TTarWriter.Create(TarFile);
    try
      for i := 0 to List.Count - 1 do begin
        Filename := List[i];
        if Filename <> '' then begin
          if Filename[Length(Filename)] = OSPathDelimiter then
            TarWriter.AddDirectory(Filename)
          else
            TarWriter.AddFile(FileRoot, Filename);
        end;
      end;
    finally
      TarWriter.Free;
    end;
  finally
    TarFile.Free;
  end;
end;

procedure TarAllFiles(const TarFilename, FileRoot: String);
var
  List: TStringList;
begin
  List := TStringList.Create;
  try
    GetFileList(FileRoot, List);
    TarFileList(TarFilename, FileRoot, List);
  finally
    List.Free;
  end;
end;

procedure TarFileArray(const TarFilename, FileRoot: String; const Filenames: array of String);
var
  List: TStringList;
begin
  List := TStringList.Create;
  try
    StringArrayToStrings(Filenames, List);
    TarFileList(TarFilename, FileRoot, List);
  finally
    List.Free;
  end;
end;

procedure TarGZipFileList(const TgzFilename, FileRoot: String; List: TStrings);
var
  TgzFile: TFileStream;
  GZipWriter: TGZipWriter;
  TarWriter: TTarWriter;
  i: Integer;
  Filename: String;
begin
  TgzFile := TFileStream.Create(TgzFilename, fmCreate);
  try
    {$IFDEF SUPPORTS_DEFAULTPARAMS}
      GZipWriter := TGZipWriter.Create(TgzFile,
        ZLibStreamDefaultBufferSize, Z_BEST_COMPRESSION);
    {$ELSE SUPPORTS_DEFAULTPARAMS~}
      GZipWriter := TGZipWriter.CreateDef2(TgzFile,
        ZLibStreamDefaultBufferSize, Z_BEST_COMPRESSION);
    {$ENDIF ~SUPPORTS_DEFAULTPARAMS}
    try
      TarWriter := TTarWriter.Create(GZipWriter);
      try
        for i := 0 to List.Count - 1 do begin
          Filename := List[i];
          if Filename <> '' then begin
            if Filename[Length(Filename)] = OSPathDelimiter then
              TarWriter.AddDirectory(Filename)
            else
              TarWriter.AddFile(FileRoot, Filename);
          end;
        end;
      finally
        TarWriter.Free;
      end;
    finally
      GZipWriter.Free;
    end;
  finally
    TgzFile.Free;
  end;
end;

procedure TarGZipAllFiles(const TgzFilename, FileRoot: String);
var
  List: TStringList;
begin
  List := TStringList.Create;
  try
    GetFileList(FileRoot, List);
    TarGZipFileList(TgzFilename, FileRoot, List);
  finally
    List.Free;
  end;
end;

procedure TarGZipFileArray(const TgzFilename, FileRoot: String; const Filenames: array of String);
var
  List: TStringList;
begin
  List := TStringList.Create;
  try
    StringArrayToStrings(Filenames, List);
    TarGZipFileList(TgzFilename, FileRoot, List);
  finally
    List.Free;
  end;
end;

// *******************************************************************************************

//  History:
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
