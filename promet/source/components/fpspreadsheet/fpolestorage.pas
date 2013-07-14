{
fpolestorage.pas

Writes an OLE document

AUTHORS: Felipe Monteiro de Carvalho

Properties of this unit for creating OLE documents:

* Can only create documents with one stream. It's not possible
  to create real directory structures like the OLE format supports.
  This is no problem for most applications.
* No limitations interfere with the creation of multi-sheet Excel files

The Windows only code, which calls COM to write the documents
should work very well.

The cross-platform code at this moment has some limitations,
but should work for most documents. Some limitations are:

* Supports only 1 stream in the file
* Fixed sectors size of 512 bytes
* Fixed short sector size of 64 bytes
* Never allocates more space for the MSAT, limiting the SAT to 109 sectors,
  which means a total
* Never allocates more then 1 sector for the SAT, so the document may have
  only up to 512 / 4 = 128 sectors

}
unit fpolestorage;

{$ifdef fpc}
{$mode delphi}
{$endif}

interface

{$ifdef Windows}
  {$define FPOLESTORAGE_USE_COM}
{$endif}

uses
{$ifdef FPOLESTORAGE_USE_COM}
  ActiveX, ComObj,
{$endif}
  Classes, SysUtils, Math,
  fpsutils;

type

  { Describes an OLE Document }

  TOLEDocument = record
    // Information about the document
    Stream: TMemoryStream;
  end;


  { TOLEStorage }

  TOLEStorage = class
  private
    { Information filled by the write routines for the helper routines }
{$ifdef FPOLESTORAGE_USE_COM}
    FStorage: IStorage;
    FStream: IStream;
{$endif}
    { Fields for the read routines }
    FReadingStreamSize: Int64;
    { Fields for both }
    FOLEDocument: TOLEDocument;
    FUseShortSectors: Boolean;
    FNumSATSectors, FNumStreamSectors, FNumTotalSectors: Cardinal;
    FNumStreamShortSectors: Cardinal;
    { Writer Helper routines }
    procedure WriteOLEHeader(AStream: TStream);
    procedure WriteSectorAllocationTable(AStream: TStream);
    procedure WriteDirectoryStream(AStream: TStream;
      const ABookStreamName: string='Book');
    procedure WriteDirectoryEntry(AStream: TStream; AName: widestring;
      EntryType, EntryColor: Byte; AIsStorage: Boolean;
      AFirstSecID, AStreamSize: Cardinal);
    procedure WriteShortSectorAllocationTable(AStream: TStream);
    procedure WriteUserStream(ADest, ASource: TStream);
    { Reader helper routines }
    procedure ReadOLEHeader(AStream: TStream);
    procedure ReadSectorAllocationTable(AStream: TStream);
    procedure ReadDirectoryStream(AStream: TStream);
    procedure ReadDirectoryEntry(AStream: TStream; var AName: widestring;
      var EntryType, EntryColor: Byte; var AIsStorage: Boolean;
      var AFirstSecID, AStreamSize: Cardinal);
    procedure ReadShortSectorAllocationTable(AStream: TStream);
    procedure ReadUserStream(ADest, ASource: TStream);
  public
    constructor Create;
    destructor Destroy; override;
    procedure WriteOLEFile(AFileName: string; AOLEDocument: TOLEDocument;
     const AOverwriteExisting: Boolean = False; const AStreamName: UTF8String='Book');
    procedure ReadOLEFile(AFileName: string; AOLEDocument: TOLEDocument; const AStreamName: UTF8String='Book');
    procedure FreeOLEDocumentData(AOLEDocument: TOLEDocument);
  end;

implementation

const
  INT_OLE_SECTOR_SIZE = 512; // in bytes
  INT_OLE_SECTOR_DWORD_SIZE = 512 div 4; // in dwords
  INT_OLE_SHORT_SECTOR_SIZE = 64; // in bytes
  INT_OLE_MIN_SIZE_FOR_STANDARD_STREAMS = 4096;

  INT_OLE_DIR_ENTRY_TYPE_EMPTY = 0;
  INT_OLE_DIR_ENTRY_TYPE_USER_STREAM = 2;
  INT_OLE_DIR_ENTRY_TYPE_ROOT_STORAGE = 5;

  INT_OLE_DIR_COLOR_RED = 0;
  INT_OLE_DIR_COLOR_BLACK = 1;

{ TOLEStorage }

{
4.1 Compound Document Header Contents
The header is always located at the beginning of the file, and its size is exactly 512 bytes. This implies that the first
sector (with SecID 0) always starts at file offset 512.
}
procedure TOLEStorage.WriteOLEHeader(AStream: TStream);
var
  i: Integer;
begin
  {
  Contents of the compound document header structure:
  Offset Size Contents
  0 8 Compound document file identifier: D0H CFH 11H E0H A1H B1H 1AH E1H
  }
  AStream.WriteByte($D0);
  AStream.WriteByte($CF);
  AStream.WriteByte($11);
  AStream.WriteByte($E0);
  AStream.WriteByte($A1);
  AStream.WriteByte($B1);
  AStream.WriteByte($1A);
  AStream.WriteByte($E1);

  { 8 16 Unique identifier (UID) of this file (not of interest in the following, may be all 0) }
  AStream.WriteDWord(0);
  AStream.WriteDWord(0);
  AStream.WriteDWord(0);
  AStream.WriteDWord(0);

  { 24 2 Revision number of the file format (most used is 003EH) }
  AStream.WriteWord(WordToLE($003E));

  { 26 2 Version number of the file format (most used is 0003H) }
  AStream.WriteWord(WordToLE($0003));

  { 28 2 Byte order identifier (➜4.2): FEH FFH = Little-Endian
    FFH FEH = Big-Endian

    Real applications only use Little-Endian, so we follow that }
  AStream.WriteByte($FE);
  AStream.WriteByte($FF);

  { 30 2 Size of a sector in the compound document file (➜3.1) in power-of-two (ssz), real sector
    size is sec_size = 2ssz bytes (minimum value is 7 which means 128 bytes, most used
    value is 9 which means 512 bytes) }
  AStream.WriteWord(WordToLE($0009));

  { 32 2 Size of a short-sector in the short-stream container stream (➜6.1) in power-of-two (sssz),
    real short-sector size is short_sec_size = 2sssz bytes (maximum value is sector size
    ssz, see above, most used value is 6 which means 64 bytes) }
  AStream.WriteWord(WordToLE($0006));

  { 34 10 Not used }
  AStream.WriteDWord($0);
  AStream.WriteDWord($0);
  AStream.WriteWord($0);

  { 44 4 Total number of sectors used for the sector allocation table (➜5.2) }
  AStream.WriteDWord(DWordToLE(1));

  { 48 4 SecID of first sector of the directory stream (➜7) }
  AStream.WriteDWord(DWordToLE($01));

  { 52 4 Not used }
  AStream.WriteDWord($0);

  { 56 4 Minimum size of a standard stream (in bytes, minimum allowed and most used size is 4096
    bytes), streams with an actual size smaller than (and not equal to) this value are stored as
    short-streams (➜6) }
  AStream.WriteDWord(DWordToLE(4096));

  { 60 4 SecID of first sector of the short-sector allocation table (➜6.2), or –2 (End Of Chain
    SecID, ➜3.1) if not extant }
  AStream.WriteDWord(DWordToLE(2));

  { 64 4 Total number of sectors used for the short-sector allocation table (➜6.2) }
  AStream.WriteDWord(DWordToLE(1));

  { 68 4 SecID of first sector of the master sector allocation table (➜5.1), or –2 (End Of Chain
    SecID, ➜3.1) if no additional sectors used }
  AStream.WriteDWord(DWORD(IntegerToLE(-2)));

  { 72 4 Total number of sectors used for the master sector allocation table (➜5.1) }
  AStream.WriteDWord(0);

  { 76 436 First part of the master sector allocation table (➜5.1) containing 109 SecIDs }
  AStream.WriteDWord(0);

  for i := 1 to 108 do AStream.WriteDWord($FFFFFFFF);
end;

{
  The file is organized as following using Short Sectors:

  HEADER
  SECTOR 0 - SAT
  SECTOR 1 - Directory stream
  SECTOR 2 - Short SAT
  SECTOR 3 and on - User data

  And this SAT will describe that.

  This results in the following SecID array for the SAT:

  Array indexes 0  1  2  3 ...  N-1  N  ...
  SecID array  –3 –1 –2  4 ...  -2  -1  ...

  As expected, sector 0 is marked with the special SAT SecID (➜3.1).
  Sector 1 and all sectors starting with sector 5 are
  not used (special Free SecID with value –1).

  Without Short Sectors the file will be similar, but more compact:

  HEADER
  SECTOR 0 - SAT
  SECTOR 1 - Directory stream
  SECTOR 2 and on - User data
}
procedure TOLEStorage.WriteSectorAllocationTable(AStream: TStream);
var
  i, CurrentPos, NextSecID: Integer;
begin
  { Example values:

   00000200H  FD FF FF FF FF FF FF FF FE FF FF FF 04 00 00 00
   00000210H  FE FF FF FF FF FF FF FF FF FF FF FF FF FF FF FF

   And after that only $FFFFFFFF until $400 }

  AStream.WriteDWord(DWordToLE($FFFFFFFD)); // SAT
  AStream.WriteDWord($FFFFFFFE); // Empty, doesn't work with $FFFFFFFF

  // If we don't use short sectors we won't write a section for their SSAT
  if FUseShortSectors then
  begin
    AStream.WriteDWord(DWordToLE($FFFFFFFE)); // Start and End of Short SAT
    CurrentPos := $200 + 12;
    NextSecID := $00000004;
  end
  else
  begin
    CurrentPos := $200 + 8;
    NextSecID := $00000003;
  end;

  // Now write the user data

  for i := 2 to FNumStreamSectors do
  begin
    AStream.WriteDWord(DWordToLE(NextSecID));
    Inc(NextSecID);
    CurrentPos := CurrentPos + 4;
  end;

  AStream.WriteDWord(DWordToLE($FFFFFFFE)); // End of user data

  CurrentPos := CurrentPos + 4;

  // Fill the rest of the sector with $FF

  for i := 1 to ($400 - CurrentPos) do AStream.WriteByte($FF);
end;

{
7.2.1 Directory Entry Structure
The size of each directory entry is exactly 128 bytes. The formula to calculate an offset in the directory stream from a
DirID is as follows:
dir_entry_pos(DirID) = DirID ∙ 128
}
procedure TOLEStorage.WriteDirectoryEntry(AStream: TStream; AName: widestring;
  EntryType, EntryColor: Byte; AIsStorage: Boolean;
  AFirstSecID, AStreamSize: Cardinal);
var
  i: Integer;
  EntryName: array[0..31] of WideChar;
begin
  { Contents of the directory entry structure:
    Offset Size Contents
    0 64 Character array of the name of the entry, always 16-bit Unicode characters, with trailing
    zero character (results in a maximum name length of 31 characters)

   00000400H  52 00 6F 00 6F 00 74 00 20 00 45 00 6E 00 74 00 }

  EntryName := AName;

  AStream.WriteBuffer(EntryName, 64);

  {Root Storage #1
   00000440H  16 00 05 00 FF FF FF FF FF FF FF FF 01 00 00 00

   Book #2
   000004C0H  0A 00 02 01 FF FF FF FF FF FF FF FF FF FF FF FF

   Item #3 e #4
   00000540H  00 00 00 00 FF FF FF FF FF FF FF FF FF FF FF FF

    64 2 Size of the used area of the character buffer of the name (not character count), including
    the trailing zero character (e.g. 12 for a name with 5 characters: (5+1)∙2 = 12)
    66 1 Type of the entry: 00H = Empty 03H = LockBytes (unknown)
    01H = User storage 04H = Property (unknown)
    02H = User stream 05H = Root storage
    67 1 Node colour of the entry: 00H = Red 01H = Black
    68 4 DirID of the left child node inside the red-black tree of all direct members of the parent
    storage (if this entry is a user storage or stream, ➜7.1), –1 if there is no left child
    72 4 DirID of the right child node inside the red-black tree of all direct members of the parent
    storage (if this entry is a user storage or stream, ➜7.1), –1 if there is no right child
    76 4 DirID of the root node entry of the red-black tree of all storage members (if this entry is a
    storage, ➜7.1), –1 otherwise
  }

  if AName = #0 then AStream.WriteWord($0000)
  else AStream.WriteWord(WordToLE(Length(AName) * 2));

  AStream.WriteByte(EntryType);
  AStream.WriteByte(EntryColor);

  AStream.WriteDWord(DWordToLE($FFFFFFFF));
  AStream.WriteDWord(DWordToLE($FFFFFFFF));

  if AIsStorage then AStream.WriteDWord(DWordToLE($00000001))
  else AStream.WriteDWord(DWordToLE($FFFFFFFF));

  {00000450H  00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00

    80 16 Unique identifier, if this is a storage (not of interest in the following, may be all 0)

   00000460H  00 00 00 00 00 00 00 00 00 00 00 00 00 4E 67 0E
   00000470H  39 6F C9 01

    96 4 User flags (not of interest in the following, may be all 0)
    100 8 Time stamp of creation of this entry (➜7.2.3). Most implementations do not write a valid
    time stamp, but fill up this space with zero bytes.
    108 8 Time stamp of last modification of this entry (➜7.2.3). Most implementations do not write
    a valid time stamp, but fill up this space with zero bytes.
   }

  AStream.WriteDWord(0);
  AStream.WriteDWord(0);
  AStream.WriteDWord(0);
  AStream.WriteDWord(0);

  AStream.WriteDWord(0);
  AStream.WriteDWord(0);
  AStream.WriteDWord(0);

  AStream.WriteDWord(0);
  AStream.WriteDWord(0);

  {Root Storage #1
   00000470H  XX XX XX XX 03 00 00 00 40 03 00 00 00 00 00 00

   Book #2
   000004F0H  XX XX XX XX 00 00 00 00 3F 03 00 00 00 00 00 00

   Item #3 e #4
   00000570H  XX XX XX XX 00 00 00 00 00 00 00 00 00 00 00 00

    First 4 bytes still with the timestamp.

    116 4 SecID of first sector or short-sector, if this entry refers to a stream (➜7.2.2),
    SecID of first sector of the short-stream container stream (➜6.1),
    if this is the root storage entry, 0 otherwise

    120 4 Total stream size in bytes, if this entry refers to a stream (➜7.2.2),
    total size of the short-stream container stream (➜6.1),
    if this is the root storage entry, 0 otherwise

    124 4 Not used
   }

  AStream.WriteDWord(DWordToLE(AFirstSecID));

  AStream.WriteDWord(DWordToLE(AStreamSize));

  AStream.WriteDWord(DWordToLE($00000000));
end;

procedure TOLEStorage.WriteDirectoryStream(AStream: TStream; const ABookStreamName: string='Book');
var
  FContainerSize: Cardinal;
begin
  { Size of the container stream }

  FContainerSize := Ceil(FOLEDocument.Stream.Size / INT_OLE_SECTOR_SIZE) * INT_OLE_SECTOR_SIZE;

  if FUseShortSectors then
  begin
    WriteDirectoryEntry(AStream, 'Root Entry'#0,
     INT_OLE_DIR_ENTRY_TYPE_ROOT_STORAGE, INT_OLE_DIR_COLOR_RED,
     True, $00000003, FContainerSize);

    WriteDirectoryEntry(AStream, ABookStreamName+#0,
     INT_OLE_DIR_ENTRY_TYPE_USER_STREAM, INT_OLE_DIR_COLOR_BLACK,
     False, 0, FOLEDocument.Stream.Size);

    WriteDirectoryEntry(AStream, #0,
     INT_OLE_DIR_ENTRY_TYPE_EMPTY, INT_OLE_DIR_COLOR_RED,
     False, 0, $00000000);

    WriteDirectoryEntry(AStream, #0,
     INT_OLE_DIR_ENTRY_TYPE_EMPTY, INT_OLE_DIR_COLOR_RED,
     False, 0, $00000000);
  end
  else
  begin
    WriteDirectoryEntry(AStream, 'Root Entry'#0,
     INT_OLE_DIR_ENTRY_TYPE_ROOT_STORAGE, INT_OLE_DIR_COLOR_RED,
     True,  $FFFFFFFE, 0);

    WriteDirectoryEntry(AStream, ABookStreamName+#0,
     INT_OLE_DIR_ENTRY_TYPE_USER_STREAM, INT_OLE_DIR_COLOR_BLACK,
     False, $00000002, FOLEDocument.Stream.Size);

    WriteDirectoryEntry(AStream, #0,
     INT_OLE_DIR_ENTRY_TYPE_EMPTY, INT_OLE_DIR_COLOR_RED,
     False, 0, $00000000);

    WriteDirectoryEntry(AStream, #0,
     INT_OLE_DIR_ENTRY_TYPE_EMPTY, INT_OLE_DIR_COLOR_RED,
     False, 0, $00000000);
  end;
end;

{
8.4 Short-Sector Allocation Table

The short-sector allocation table (SSAT) is an array of SecIDs and contains the SecID chains (➜3.2) of all short-
streams, similar to the sector allocation table (➜5.2) that contains the SecID chains of standard streams.
The first SecID of the SSAT is contained in the header (➜4.1), the remaining SecID chain is contained in the SAT. The
SSAT is built by reading and concatenating the contents of all sectors.
Contents of a sector of the SSAT (sec_size is the size of a sector in bytes, see ➜4.1):
Offset Size Contents
0 sec_size Array of sec_size/4 SecIDs of the SSAT
The SSAT will be used similarly to the SAT (➜5.2) with the difference that the SecID chains refer to short-sectors in the
short-stream container stream (➜6.1).

This results in the following SecID array for the SSAT:
Array
indexes 0 1 2 3 4 5 6 7 8 9 1011...4142434445464748495051525354...
SecID array 1 2 3 4 5 6 7 8 9 101112...42434445–247–2–250515253–2–1...
All short-sectors starting with sector 54 are not used (special Free SecID with value –1).
}
procedure TOLEStorage.WriteShortSectorAllocationTable(AStream: TStream);
var
  i, NextShortSecID, CurrentPos: Integer;
begin
  CurrentPos := $800;
  NextShortSecID := $00000001;

  for i := 2 to FNumStreamShortSectors do
  begin
    AStream.WriteDWord(DWordToLE(NextShortSecID));
    Inc(NextShortSecID);
    Inc(CurrentPos, 4);
  end;

  AStream.WriteDWord(DWordToLE($FFFFFFFE));
  Inc(CurrentPos, 4);

  for i := 1 to ($A00 - CurrentPos) do AStream.WriteByte($FF);
end;

procedure TOLEStorage.WriteUserStream(ADest, ASource: TStream);
var
  i, PadSize: Integer;
begin
  ADest.CopyFrom(ASource, 0);

  // Pad the sector with zeroes
  PadSize := INT_OLE_SECTOR_SIZE - (ASource.Size mod INT_OLE_SECTOR_SIZE);

  for i := 1 to PadSize do ADest.WriteByte($00);
end;

procedure TOLEStorage.ReadOLEHeader(AStream: TStream);
var
  BaseAddr: Int64;
begin
  BaseAddr := AStream.Position;

  { 28 2 Byte order identifier (➜4.2): FEH FFH = Little-Endian
    FFH FEH = Big-Endian }

  // For now just assume little-endian

  { 30 2 Size of a sector in the compound document file (➜3.1) in power-of-two (ssz), real sector
    size is sec_size = 2ssz bytes (minimum value is 7 which means 128 bytes, most used
    value is 9 which means 512 bytes) }

  // Assume default value

  { 32 2 Size of a short-sector in the short-stream container stream (➜6.1) in power-of-two (sssz),
    real short-sector size is short_sec_size = 2sssz bytes (maximum value is sector size
    ssz, see above, most used value is 6 which means 64 bytes) }

  // Assume default value

  { 44 4 Total number of sectors used for the sector allocation table (➜5.2) }

  // Assume 1

  { 48 4 SecID of first sector of the directory stream (➜7) }

  // Assume 1

  { 56 4 Minimum size of a standard stream (in bytes, minimum allowed and most used size is 4096
    bytes), streams with an actual size smaller than (and not equal to) this value are stored as
    short-streams (➜6) }

  // Assume 4096

  { 60 4 SecID of first sector of the short-sector allocation table (➜6.2), or –2 (End Of Chain
    SecID, ➜3.1) if not extant }

  // Assume 2

  { 64 4 Total number of sectors used for the short-sector allocation table (➜6.2) }

  // Assume 1

  { 68 4 SecID of first sector of the master sector allocation table (➜5.1), or –2 (End Of Chain
    SecID, ➜3.1) if no additional sectors used }

  // Assume 2

  { 72 4 Total number of sectors used for the master sector allocation table (➜5.1) }

  // Assume 0

  { 76 436 First part of the master sector allocation table (➜5.1) containing 109 SecIDs }

  // Assume: 0, -1, -1, -1 ...
end;

procedure TOLEStorage.ReadSectorAllocationTable(AStream: TStream);
begin

end;

procedure TOLEStorage.ReadDirectoryStream(AStream: TStream);
var
  EntryName: widestring;
  EntryType, EntryColor: Byte;
  EntryIsStorage: Boolean;
  EntryFirstSecID, EntryStreamSize: Cardinal;
begin
  ReadDirectoryEntry(AStream, EntryName,
    EntryType, EntryColor, EntryIsStorage,
    EntryFirstSecID, EntryStreamSize);

  ReadDirectoryEntry(AStream, EntryName,
    EntryType, EntryColor, EntryIsStorage,
    EntryFirstSecID, EntryStreamSize);

  FReadingStreamSize := EntryStreamSize;
end;

procedure TOLEStorage.ReadDirectoryEntry(AStream: TStream;
  var AName: widestring; var EntryType, EntryColor: Byte;
  var AIsStorage: Boolean; var AFirstSecID, AStreamSize: Cardinal);
var
  BaseAddr: Int64;
  EntryName: array[0..63] of WideChar;
begin
  BaseAddr := AStream.Position;

  { Contents of the directory entry structure:
    Offset Size Contents
    0 64 Character array of the name of the entry, always 16-bit Unicode characters, with trailing
    zero character (results in a maximum name length of 31 characters)

   00000400H  52 00 6F 00 6F 00 74 00 20 00 45 00 6E 00 74 00 }

  AStream.ReadBuffer(EntryName, 64);

  AName := EntryName;

  {Root Storage #1
   00000440H  16 00 05 00 FF FF FF FF FF FF FF FF 01 00 00 00

   Book #2
   000004C0H  0A 00 02 01 FF FF FF FF FF FF FF FF FF FF FF FF

   Item #3 e #4
   00000540H  00 00 00 00 FF FF FF FF FF FF FF FF FF FF FF FF

    64 2 Size of the used area of the character buffer of the name (not character count), including
    the trailing zero character (e.g. 12 for a name with 5 characters: (5+1)∙2 = 12)
    66 1 Type of the entry: 00H = Empty 03H = LockBytes (unknown)
    01H = User storage 04H = Property (unknown)
    02H = User stream 05H = Root storage
    67 1 Node colour of the entry: 00H = Red 01H = Black
    68 4 DirID of the left child node inside the red-black tree of all direct members of the parent
    storage (if this entry is a user storage or stream, ➜7.1), –1 if there is no left child
    72 4 DirID of the right child node inside the red-black tree of all direct members of the parent
    storage (if this entry is a user storage or stream, ➜7.1), –1 if there is no right child
    76 4 DirID of the root node entry of the red-black tree of all storage members (if this entry is a
    storage, ➜7.1), –1 otherwise
  }

  AStream.ReadWord();

  EntryType := AStream.ReadByte();
  EntryColor := AStream.ReadByte();

  AStream.ReadDWord();
  AStream.ReadDWord();

  AIsStorage := DWordLEtoN(AStream.ReadDWord) <> $FFFFFFFF;

  {Root Storage #1
   00000470H  XX XX XX XX 03 00 00 00 40 03 00 00 00 00 00 00

   Book #2
   000004F0H  XX XX XX XX 00 00 00 00 3F 03 00 00 00 00 00 00

   Item #3 e #4
   00000570H  XX XX XX XX 00 00 00 00 00 00 00 00 00 00 00 00

    First 4 bytes still with the timestamp.

    116 4 SecID of first sector or short-sector, if this entry refers to a stream (➜7.2.2),
    SecID of first sector of the short-stream container stream (➜6.1),
    if this is the root storage entry, 0 otherwise

    120 4 Total stream size in bytes, if this entry refers to a stream (➜7.2.2),
    total size of the short-stream container stream (➜6.1),
    if this is the root storage entry, 0 otherwise

    124 4 Not used
   }

  AStream.Seek(BaseAddr + $74, soFromBeginning);

  AFirstSecID := DWordLEtoN(AStream.ReadDWord);

  AStreamSize := DWordLEtoN(AStream.ReadDWord);

  AStream.ReadDWord();
end;

procedure TOLEStorage.ReadShortSectorAllocationTable(AStream: TStream);
begin

end;

procedure TOLEStorage.ReadUserStream(ADest, ASource: TStream);
var
  i: Integer;
begin
//  ADest.CopyFrom(ASource, FReadingStreamSize);
  for i := 1 to FReadingStreamSize do
    ADest.WriteByte(ASource.ReadByte);
end;

constructor TOLEStorage.Create;
begin
  inherited Create;

end;

destructor TOLEStorage.Destroy;
begin

  inherited Destroy;
end;

{@@
  Writes the OLE document specified in AOLEDocument
  to the file with name AFileName. The routine will fail
  if the file already exists, or if the directory where
  it should be placed doesn't exist.
}
procedure TOLEStorage.WriteOLEFile(AFileName: string;
  AOLEDocument: TOLEDocument; const AOverwriteExisting: Boolean;
  const AStreamName: UTF8String);
var
  cbWritten: Cardinal;
  AFileStream: TFileStream;
  i, x: Cardinal;
  lMode: Word;
begin
  // The behavior of LCL classes is failling to write to existing file,
  // But here we make this settable
  if AOverwriteExisting then lMode := fmCreate or fmOpenWrite
  else lMode := fmCreate;

  if (not AOverwriteExisting) and FileExists(AFileName) then
   Raise EStreamError.Createfmt('File already exists "%s"',[AFileName]);

  { Fill information for helper routines }
  FOLEDocument := AOLEDocument;

  { Calculate the number of sectors necessary for the stream }

  FNumStreamSectors := Ceil(AOLEDocument.Stream.Size / INT_OLE_SECTOR_SIZE);

  { Calculates the number of short sectors for the stream, if it applies }

  FUseShortSectors := AOLEDocument.Stream.Size < INT_OLE_MIN_SIZE_FOR_STANDARD_STREAMS;

  FNumStreamShortSectors := Ceil(AOLEDocument.Stream.Size / INT_OLE_SHORT_SECTOR_SIZE);

  { Numbers of sectors necessary for the SAT }

  FNumSATSectors := 1; // Ceil(FNumStreamSectors / INT_OLE_SECTOR_DWORD_SIZE);

{$ifdef FPOLESTORAGE_USE_COM}

  { Initialize the Component Object Model (COM) before calling s functions }
  OleCheck(CoInitialize(nil));

  { Create a Storage Object }
  OleCheck(StgCreateDocfile(PWideChar(WideString(AFileName)),
   STGM_READWRITE or STGM_CREATE or STGM_SHARE_EXCLUSIVE or STGM_DIRECT,
   0, FStorage));

  { Create a workbook stream in the storage.  A BIFF5 file must
    have at least a workbook stream.  This stream *must* be named 'Book',
    in BIFF8 it is 'Workbook', so use AStreamName }
  OleCheck(FStorage.CreateStream(PWideChar(UTF8Decode(AStreamName)),
   STGM_READWRITE or STGM_SHARE_EXCLUSIVE or STGM_DIRECT, 0, 0, FStream));

  { Write all data }
  FStream.Write(FOLEDocument.Stream.Memory,
    FOLEDocument.Stream.Size, @cbWritten);

{$else}

  AFileStream := TFileStream.Create(AFileName, lMode);
  try
    // Header
    WriteOLEHeader(AFileStream);

    // Record 0, the SAT
    WriteSectorAllocationTable(AFileStream);

    // Record 1, the directory stream
    WriteDirectoryStream(AFileStream,AStreamName);

    // Record 2, the Short SAT
    if FUseShortSectors then WriteShortSectorAllocationTable(AFileStream);

    // Records 3 and on (or 2 and on without Short Sectors), the user data
    WriteUserStream(AFileStream, FOLEDocument.Stream);
  finally
    AFileStream.Free;
  end;

{$endif}
end;

{@@
  Reads an OLE file.
}
procedure TOLEStorage.ReadOLEFile(AFileName: string;
  AOLEDocument: TOLEDocument; const AStreamName: UTF8String='Book');
var
  AFileStream: TFileStream;
  CurrentSectorPos: Int64;
begin
  FOLEDocument := AOLEDocument;
  if not Assigned(AOLEDocument.Stream) then
    AOLEDocument.Stream := TMemoryStream.Create;
  AFileStream := TFileStream.Create(AFileName, fmOpenRead);
  try
    // Header
    ReadOLEHeader(AFileStream);

    // Record 0, the SAT
    CurrentSectorPos := $200;
    AFileStream.Seek(CurrentSectorPos, soFromBeginning);
    ReadSectorAllocationTable(AFileStream);

    // Record 1, the directory stream
    CurrentSectorPos := $400;
    AFileStream.Seek(CurrentSectorPos, soFromBeginning);
    ReadDirectoryStream(AFileStream);

    // FReadingStreamSize is filled by ReadDirectoryStream
    FUseShortSectors := FReadingStreamSize < INT_OLE_MIN_SIZE_FOR_STANDARD_STREAMS;

    CurrentSectorPos := $600;

    // Record 2, the Short SAT, if exists
    if FUseShortSectors then
    begin
      AFileStream.Seek(CurrentSectorPos, soFromBeginning);
      ReadShortSectorAllocationTable(AFileStream);
      CurrentSectorPos := $800;
    end
    else
      CurrentSectorPos := $600;

    // Records 3 and on (or 2 and on without Short Sectors), the user data
    AFileStream.Seek(CurrentSectorPos, soFromBeginning);
    ReadUserStream(FOLEDocument.Stream, AFileStream);
  finally
    AFileStream.Free;
  end;
end;

{@@
  Frees all internal objects storable in a TOLEDocument structure
}
procedure TOLEStorage.FreeOLEDocumentData(AOLEDocument: TOLEDocument);
begin
  if Assigned(AOLEDocument.Stream) then FreeAndNil(AOLEDocument.Stream);
end;

end.

