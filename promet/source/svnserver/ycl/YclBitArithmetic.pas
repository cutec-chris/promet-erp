{**************************************************************************************************}
{                                                                                                  }
{  Y core library (Ycl)                                                                            }
{                                                                                                  }
{  Bit arithmetic                                                                                  }
{                                                                                                  }
{  The contents of this file are subject to the Y Library Public License Version 1.0 (the          }
{  "License"); you may not use this file except in compliance with the License. You may obtain a   }
{  copy of the License at http://delphi.pjh2.de/                                                   }
{                                                                                                  }
{  Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF  }
{  ANY KIND, either express or implied. See the License for the specific language governing        }
{  rights and limitations under the License.                                                       }
{                                                                                                  }
{  The Original Code is: YclBitArithmetic.pas.                                                     }
{  The Initial Developer of the Original Code is Peter J. Haas (libs@pjh2.de). Portions created    }
{  by Peter J. Haas are Copyright (C) 2000-2005 Peter J. Haas. All Rights Reserved.                }
{                                                                                                  }
{  Contributor(s):                                                                                 }
{                                                                                                  }
{  You may retrieve the latest version of this file at the homepage of Peter J. Haas, located at   }
{  http://delphi.pjh2.de/                                                                          }
{                                                                                                  }
{**************************************************************************************************}

// For history see end of file

{$I Ycl.inc}

unit YclBitArithmetic;

interface
uses
  Classes, YclBase;

const
  // einzelne Bits
  BitMask0BitL: array[0..31] of LongWord = (
    $FFFFFFFE, $FFFFFFFD, $FFFFFFFB, $FFFFFFF7,  $FFFFFFEF, $FFFFFFDF, $FFFFFFBF, $FFFFFF7F,
    $FFFFFEFF, $FFFFFDFF, $FFFFFBFF, $FFFFF7FF,  $FFFFEFFF, $FFFFDFFF, $FFFFBFFF, $FFFF7FFF,
    $FFFEFFFF, $FFFDFFFF, $FFFBFFFF, $FFF7FFFF,  $FFEFFFFF, $FFDFFFFF, $FFBFFFFF, $FF7FFFFF,
    $FEFFFFFF, $FDFFFFFF, $FBFFFFFF, $F7FFFFFF,  $EFFFFFFF, $DFFFFFFF, $BFFFFFFF, $7FFFFFFF
  );

  BitMask1BitL: array[0..31] of LongWord = (
    $00000001, $00000002, $00000004, $00000008,  $00000010, $00000020, $00000040, $00000080,
    $00000100, $00000200, $00000400, $00000800,  $00001000, $00002000, $00004000, $00008000,
    $00010000, $00020000, $00040000, $00080000,  $00100000, $00200000, $00400000, $00800000,
    $01000000, $02000000, $04000000, $08000000,  $10000000, $20000000, $40000000, $80000000
  );

  BitMask0BitH: array[0..31] of LongWord = (
    $7FFFFFFF, $BFFFFFFF, $DFFFFFFF, $EFFFFFFF,  $F7FFFFFF, $FBFFFFFF, $FDFFFFFF, $FEFFFFFF,
    $FF7FFFFF, $FFBFFFFF, $FFDFFFFF, $FFEFFFFF,  $FFF7FFFF, $FFFBFFFF, $FFFDFFFF, $FFFEFFFF,
    $FFFF7FFF, $FFFFBFFF, $FFFFDFFF, $FFFFEFFF,  $FFFFF7FF, $FFFFFBFF, $FFFFFDFF, $FFFFFEFF,
    $FFFFFF7F, $FFFFFFBF, $FFFFFFDF, $FFFFFFEF,  $FFFFFFF7, $FFFFFFFB, $FFFFFFFD, $FFFFFFFE
  );

  BitMask1BitH: array[0..31] of LongWord = (
    $80000000, $40000000, $20000000, $10000000,  $08000000, $04000000, $02000000, $01000000,
    $00800000, $00400000, $00200000, $00100000,  $00080000, $00040000, $00020000, $00010000,
    $00008000, $00004000, $00002000, $00001000,  $00000800, $00000400, $00000200, $00000100,
    $00000080, $00000040, $00000020, $00000010,  $00000008, $00000004, $00000002, $00000001
  );

  // Bit-Bereiche
  BitMask0BitsL: array[0..32] of LongWord = (
    $FFFFFFFF, $FFFFFFFE, $FFFFFFFC, $FFFFFFF8,  $FFFFFFF0, $FFFFFFE0, $FFFFFFC0, $FFFFFF80,
    $FFFFFF00, $FFFFFE00, $FFFFFC00, $FFFFF800,  $FFFFF000, $FFFFE000, $FFFFC000, $FFFF8000,
    $FFFF0000, $FFFE0000, $FFFC0000, $FFF80000,  $FFF00000, $FFE00000, $FFC00000, $FF800000,
    $FF000000, $FE000000, $FC000000, $F8000000,  $F0000000, $E0000000, $C0000000, $80000000,
    $00000000
  );
  BitMask1BitsL: array[0..32] of LongWord = (
    $00000000, $00000001, $00000003, $00000007,  $0000000F, $0000001F, $0000003F, $0000007F,
    $000000FF, $000001FF, $000003FF, $000007FF,  $00000FFF, $00001FFF, $00003FFF, $00007FFF,
    $0000FFFF, $0001FFFF, $0003FFFF, $0007FFFF,  $000FFFFF, $001FFFFF, $003FFFFF, $007FFFFF,
    $00FFFFFF, $01FFFFFF, $03FFFFFF, $07FFFFFF,  $0FFFFFFF, $1FFFFFFF, $3FFFFFFF, $7FFFFFFF,
    $FFFFFFFF
  );
  BitMask0BitsH: array[0..32] of LongWord = (
    $FFFFFFFF, $7FFFFFFF, $3FFFFFFF, $1FFFFFFF,  $0FFFFFFF, $07FFFFFF, $03FFFFFF, $01FFFFFF,
    $00FFFFFF, $007FFFFF, $003FFFFF, $001FFFFF,  $000FFFFF, $0007FFFF, $0003FFFF, $0001FFFF,
    $0000FFFF, $00007FFF, $00003FFF, $00001FFF,  $00000FFF, $000007FF, $000003FF, $000001FF,
    $000000FF, $0000007F, $0000003F, $0000001F,  $0000000F, $00000007, $00000003, $00000001,
    $00000000
  );
  BitMask1BitsH: array[0..32] of LongWord = (
    $00000000, $80000000, $C0000000, $E0000000,  $F0000000, $F8000000, $FC000000, $FE000000,
    $FF000000, $FF800000, $FFC00000, $FFE00000,  $FFF00000, $FFF80000, $FFFC0000, $FFFE0000,
    $FFFF0000, $FFFF8000, $FFFFC000, $FFFFE000,  $FFFFF000, $FFFFF800, $FFFFFC00, $FFFFFE00,
    $FFFFFF00, $FFFFFF80, $FFFFFFC0, $FFFFFFE0,  $FFFFFFF0, $FFFFFFF8, $FFFFFFFC, $FFFFFFFE,
    $FFFFFFFF
  );

type
  TArrayBitMask8  = array[0.. 7] of LongWord;
  TArrayBitMask16 = array[0..15] of LongWord;
  TArrayBitMask32 = array[0..31] of LongWord;

  PArrayBitMask8  = ^TArrayBitMask8;
  PArrayBitMask16 = ^TArrayBitMask16;
  PArrayBitMask32 = ^TArrayBitMask32;

const
  // **********************  Bit order: First = LSB  (Index 0 = Bit0)  ***********************

  // Bit setzen
  BitMaskSetBitN8LPtr:  PArrayBitMask8  = @BitMask1BitL[0];
  BitMaskSetBitN16LPtr: PArrayBitMask16 = @BitMask1BitL[0];
  BitMaskSetBitN32LPtr: PArrayBitMask32 = @BitMask1BitL[0];

  // Bit löschen
  BitMaskClearBitN8LPtr:  PArrayBitMask8  = @BitMask0BitL[0];
  BitMaskClearBitN16LPtr: PArrayBitMask16 = @BitMask0BitL[0];
  BitMaskClearBitN32LPtr: PArrayBitMask32 = @BitMask0BitL[0];

  // Löschen Bits 0..Index,   $FE, $FC, $F8, ...
  BitMaskClear0ToN8LPtr:  PArrayBitMask8  = @BitMask0BitsL[1];
  BitMaskClear0ToN16LPtr: PArrayBitMask16 = @BitMask0BitsL[1];
  BitMaskClear0ToN32LPtr: PArrayBitMask32 = @BitMask0BitsL[1];

  // Setzen Bits 0..Index,   $01, $03, $07, ...
  BitMaskSet0ToN8LPtr:  PArrayBitMask8  = @BitMask1BitsL[1];
  BitMaskSet0ToN16LPtr: PArrayBitMask16 = @BitMask1BitsL[1];
  BitMaskSet0ToN32LPtr: PArrayBitMask32 = @BitMask1BitsL[1];

  // Löschen Bereich vor Bit Index, d.h. Bits 0..Index-1,   $FF, $FE, $FC, ...
  BitMaskClearLowerRange8LPtr:  PArrayBitMask8  = @BitMask0BitsL[0];
  BitMaskClearLowerRange16LPtr: PArrayBitMask16 = @BitMask0BitsL[0];
  BitMaskClearLowerRange32LPtr: PArrayBitMask32 = @BitMask0BitsL[0];

  // Löschen Bereich nach Bit Index, d.h. Bits Index+1..Max,   $01, $03, $07, ...
  BitMaskClearHigherRange8LPtr:  PArrayBitMask8  = @BitMask1BitsL[1];
  BitMaskClearHigherRange16LPtr: PArrayBitMask16 = @BitMask1BitsL[1];
  BitMaskClearHigherRange32LPtr: PArrayBitMask32 = @BitMask1BitsL[1];

  // Setzen Bereich vor Bit Index, d.h. Bits 0..Index-1,   $00, $01, $03, ...
  BitMaskSetLowerRange8LPtr:  PArrayBitMask8  = @BitMask1BitsL[0];
  BitMaskSetLowerRange16LPtr: PArrayBitMask16 = @BitMask1BitsL[0];
  BitMaskSetLowerRange32LPtr: PArrayBitMask32 = @BitMask1BitsL[0];

  // Setzen Bereich nach Bit Index, d.h. Bits Index+1..Max,   $FE, $FC, $F8, ...
  BitMaskSetHigherRange8LPtr:  PArrayBitMask8  = @BitMask0BitsL[1];
  BitMaskSetHigherRange16LPtr: PArrayBitMask16 = @BitMask0BitsL[1];
  BitMaskSetHigherRange32LPtr: PArrayBitMask32 = @BitMask0BitsL[1];

const
  // ******************  Bit order: First = MSB  (Index 0 = Bit7, 15, 31)  *******************

  // Bit setzen
  BitMaskSetBitN8HPtr:  PArrayBitMask8  = @BitMask1BitH[24];
  BitMaskSetBitN16HPtr: PArrayBitMask16 = @BitMask1BitH[16];
  BitMaskSetBitN32HPtr: PArrayBitMask32 = @BitMask1BitH[0];

  // Bit löschen
  BitMaskClearBitN8HPtr:  PArrayBitMask8  = @BitMask0BitH[24];
  BitMaskClearBitN16HPtr: PArrayBitMask16 = @BitMask0BitH[16];
  BitMaskClearBitN32HPtr: PArrayBitMask32 = @BitMask0BitH[0];

  // Löschen Bits 0..Index,   $7F, $3F, $1F, ...
  BitMaskClear0ToN8HPtr:  PArrayBitMask8  = @BitMask0BitsH[25];
  BitMaskClear0ToN16HPtr: PArrayBitMask16 = @BitMask0BitsH[17];
  BitMaskClear0ToN32HPtr: PArrayBitMask32 = @BitMask0BitsH[1];

  // Setzen Bits 0..Index,   $80, $C0, $E0, ...
  BitMaskSet0ToN8HPtr:  PArrayBitMask8  = @BitMask1BitsH[25];
  BitMaskSet0ToN16HPtr: PArrayBitMask16 = @BitMask1BitsH[17];
  BitMaskSet0ToN32HPtr: PArrayBitMask32 = @BitMask1BitsH[1];

  // Löschen Bereich vor Bit Index, d.h. Bits 0..Index-1,   $FF, $7F, $3F, ...
  BitMaskClearLowerRange8HPtr:  PArrayBitMask8  = @BitMask0BitsH[24];
  BitMaskClearLowerRange16HPtr: PArrayBitMask16 = @BitMask0BitsH[16];
  BitMaskClearLowerRange32HPtr: PArrayBitMask32 = @BitMask0BitsH[0];

  // Löschen Bereich nach Bit Index, d.h. Bits Index+1..Max,   $80, $C0, $E0, ...
  BitMaskClearHigherRange8HPtr:  PArrayBitMask8  = @BitMask1BitsH[25];
  BitMaskClearHigherRange16HPtr: PArrayBitMask16 = @BitMask1BitsH[17];
  BitMaskClearHigherRange32HPtr: PArrayBitMask32 = @BitMask1BitsH[1];

  // Setzen Bereich vor Bit Index, d.h. Bits 0..Index-1,   $00, $80, $C0, ...
  BitMaskSetLowerRange8HPtr:  PArrayBitMask8  = @BitMask1BitsH[24];
  BitMaskSetLowerRange16HPtr: PArrayBitMask16 = @BitMask1BitsH[16];
  BitMaskSetLowerRange32HPtr: PArrayBitMask32 = @BitMask1BitsH[0];

  // Setzen Bereich nach Bit Index, d.h. Bits Index+1..Max,   $7F, $3F, $1F, ...
  BitMaskSetHigherRange8HPtr:  PArrayBitMask8  = @BitMask0BitsH[25];
  BitMaskSetHigherRange16HPtr: PArrayBitMask16 = @BitMask0BitsH[17];
  BitMaskSetHigherRange32HPtr: PArrayBitMask32 = @BitMask0BitsH[1];

// *******************************  Arithmetische Operationen  *******************************

// Bit setzen/löschen, Index 0 = Bit 0
function SetBit8L(Data: Byte; Index: Cardinal; Value: Boolean): Byte;
// Bit setzen/löschen, Index 0 = Bit 7
function SetBit8H(Data: Byte; Index: Cardinal; Value: Boolean): Byte;

// Index 0 = Bit 0

// Bit setzen / löschen / invertieren / abfragen
function ClearBit32(Data: LongWord; Index: Integer): LongWord;
function ClearBit64(Data: UInt64; Index: Integer): UInt64;
procedure ClearBitBuffer(var Data; DataSize, Index: Integer);

function SetBit32(Data: LongWord; Index: Integer; Value: Boolean = True): LongWord;
function SetBit64(Data: UInt64; Index: Integer; Value: Boolean = True): UInt64;
procedure SetBitBuffer(var Data; DataSize, Index: Integer; Value: Boolean = True);

function ToogleBit32(Data: LongWord; Index: Integer): LongWord;
function ToogleBit64(Data: UInt64; Index: Integer): UInt64;
procedure ToogleBitBuffer(var Data; DataSize, Index: Integer);

function TestBit32(Data: LongWord; Index: Integer): Boolean;
function TestBit64(Data: UInt64; Index: Integer): Boolean;
function TestBitBuffer(var Data; DataSize, Index: Integer): Boolean;

// mehrere Bits gleichzeitig setzen / löschen / invertieren / abfragen
function ClearBits32(Data: LongWord; StartIndex, EndIndex: Integer): LongWord;
function ClearBits64(Data: UInt64; StartIndex, EndIndex: Integer): UInt64;
procedure ClearBitsBuffer(var Data; DataSize, StartIndex, EndIndex: Integer);

function SetBits32(Data: LongWord; StartIndex, EndIndex: Integer; Value: Boolean = True): LongWord;
function SetBits64(Data: UInt64; StartIndex, EndIndex: Integer; Value: Boolean = True): UInt64;
procedure SetBitsBuffer(var Data; DataSize, StartIndex, EndIndex: Integer; Value: Boolean = True);

function ToogleBits32(Data: LongWord; StartIndex, EndIndex: Integer): LongWord;
function ToogleBits64(Data: UInt64; StartIndex, EndIndex: Integer): UInt64;
procedure ToogleBitsBuffer(var Data; DataSize, StartIndex, EndIndex: Integer);

function TestBits32(Data: LongWord; StartIndex, EndIndex: Integer): Boolean;
function TestBits64(Data: UInt64; StartIndex, EndIndex: Integer): Boolean;
function TestBitsBuffer(const Data; DataSize, StartIndex, EndIndex: Integer): Boolean;

// *************************************  Count 0 bits  **************************************

function Count0Bits8(Value: Cardinal): Cardinal;
function Count0Bits16(Value: Cardinal): Cardinal;
function Count0Bits32(Value: LongWord): Cardinal;
function Count0Bits64(Value: UInt64): Cardinal;

// Bits außerhalb der eigentlichen Daten gelten als 0-Bit
function Count0Bits(Value: Cardinal; StartIndex, EndIndex: Integer): Cardinal;
function Count0BitsBuffer(const Data; DataSize, StartIndex, EndIndex: Integer): Cardinal;

// *************************************  Count 1 bits  **************************************

function Count1Bits8(Value: Cardinal): Cardinal;
function Count1Bits16(Value: Cardinal): Cardinal;
function Count1Bits32(Value: LongWord): Cardinal;
function Count1Bits64(Value: UInt64): Cardinal;

// Bits außerhalb der eigentlichen Daten gelten als 0-Bit
function Count1Bits(Value: Cardinal; StartIndex, EndIndex: Integer): Cardinal;
function Count1BitsBuffer(const Data; DataSize, StartIndex, EndIndex: Integer): Cardinal;

// *******************************  Bit-Order tauschen  **************************************

function BitSwap8(Value: Cardinal): Cardinal;
function BitSwap16(Value: Cardinal): Cardinal;
function BitSwap32(Value: LongWord): LongWord;
function BitSwap64(Value: UInt64): UInt64;
procedure BitSwapBuffer(var Data; DataSize: Integer);

function BitSwapN32(Value: LongWord; BitCount: Cardinal): LongWord;
function BitSwapN64(Value: UInt64; BitCount: Cardinal): UInt64;

// **********************  Index höchstes / niedrigstes Bit ermitteln  ***********************

// -1, wenn kein entsprechendes Bit vorhanden ist
function Lowest0Bit8(Value: Cardinal): Integer;
function Lowest0Bit16(Value: Cardinal): Integer;
function Lowest0Bit32(Value: LongWord): Integer;
function Lowest0Bit64(Value: UInt64): Integer;
function Lowest0BitBuffer(const Data; DataSize: Integer): Integer;

function Lowest1Bit8(Value: Cardinal): Integer;
function Lowest1Bit16(Value: Cardinal): Integer;
function Lowest1Bit32(Value: LongWord): Integer;
function Lowest1Bit64(Value: UInt64): Integer;
function Lowest1BitBuffer(const Data; DataSize: Integer): Integer;

function Highest0Bit8(Value: Cardinal): Integer;
function Highest0Bit16(Value: Cardinal): Integer;
function Highest0Bit32(Value: LongWord): Integer;
function Highest0Bit64(Value: UInt64): Integer;
function Highest0BitBuffer(const Data; DataSize: Integer): Integer;

function Highest1Bit8(Value: Cardinal): Integer;
function Highest1Bit16(Value: Cardinal): Integer;
function Highest1Bit32(Value: LongWord): Integer;
function Highest1Bit64(Value: UInt64): Integer;
function Highest1BitBuffer(const Data; DataSize: Integer): Integer;

// ***************************************  Rotation  ****************************************

// rotate right
function ROR8(Value: Cardinal; Count: Integer): Cardinal;
function ROR16(Value: Cardinal; Count: Integer): Cardinal;
function ROR32(Value: LongWord; Count: Integer): LongWord;
//function ROR64(Value: UInt64; Count: Integer): UInt64;
//procedure RORBuffer(var Data; DataSize: Integer; Count: Integer);

// rotate left
function ROL8(Value: Cardinal; Count: Integer): Cardinal;
function ROL16(Value: Cardinal; Count: Integer): Cardinal;
function ROL32(Value: LongWord; Count: Integer): LongWord;
//function ROL64(Value: UInt64; Count: Integer): UInt64;
//procedure ROLBuffer(var Data; DataSize: Integer; Count: Integer);

// shift right
function SHR8(Value: Cardinal; Count: Integer): Cardinal;
function SHR16(Value: Cardinal; Count: Integer): Cardinal;
function SHR32(Value: LongWord; Count: Integer): LongWord;
function SHR64(Value: UInt64; Count: Integer): UInt64;
//procedure SHRBuffer(var Data; DataSize: Integer; Count: Integer);

// shift left
function SHL8(Value: Cardinal; Count: Integer): Cardinal;
function SHL16(Value: Cardinal; Count: Integer): Cardinal;
function SHL32(Value: LongWord; Count: Integer): LongWord;
function SHL64(Value: UInt64; Count: Integer): UInt64;
//procedure SHLBuffer(var Data; DataSize: Integer; Count: Integer);

// shift arithmetic right
function SAR8(Value: Cardinal; Count: Integer): Cardinal;
function SAR16(Value: Cardinal; Count: Integer): Cardinal;
function SAR32(Value: LongWord; Count: Integer): LongWord;
//function SAR64(Value: UInt64; Count: Integer): UInt64;
//procedure SARBuffer(var Data; DataSize: Integer; Count: Integer);

// shift arithmetic left
function SAL8(Value: Cardinal; Count: Integer): Cardinal;
function SAL16(Value: Cardinal; Count: Integer): Cardinal;
function SAL32(Value: LongWord; Count: Integer): LongWord;
function SAL64(Value: UInt64; Count: Integer): UInt64;
//procedure SALBuffer(var Data; DataSize: Integer; Count: Integer);

// *******************************************************************************************

type
  TYclBitOrder = (boLeastSignificantBitFirst, boMostSignificantBitFirst);

// ***********************************  Bit-Stream Reader  ***********************************

type
  TYclBitStreamCustomSingleBitReader = class(TObject)
  private
    procedure SetPosition(Value: Cardinal);
  protected
    FBuffer: Pointer;         // Zeiger auf den Puffer, der Puffer wird nicht vom Objekt verwaltet
    FSize: Cardinal;          // Größe des Puffers in Bits
    FCurrentBytePtr: PByte;   // Zeiger auf aktuelles Byte
    FPosition: Cardinal;      // Index des aktuellen Bits,
                              // reale Position im Bit abhängig von Bit-Order
  public
    // Stream-Daten übergeben
    procedure Init(Buffer: Pointer; Size: Cardinal);
    // gibt aktuelles Bit zurück
    function CurrentBit: Boolean; virtual; abstract;
    // Stream auf nächstes Bit setzen
    // True, wenn nächstes Bit vorhanden; False, wenn Ende erreicht
    function NextBit: Boolean;
    // Bit lesen und Stream auf nächstes Bit setzen
    function ReadBit: Boolean;
    function IsEndOfStream: Boolean;
    procedure SeekToNextByteBoundary;
    property Position: Cardinal read FPosition write SetPosition;
    property Size: Cardinal read FSize;
  end;

  // Low-Order, erstes Bit im Byte = Bit 0
  TYclBitStreamSingleBitReaderL = class(TYclBitStreamCustomSingleBitReader)
  public
    function CurrentBit: Boolean; override;
  end;

  // High-Order, erstes Bit im Byte = Bit 7
  TYclBitStreamSingleBitReaderH = class(TYclBitStreamCustomSingleBitReader)
  public
    function CurrentBit: Boolean; override;
  end;

// *******************************************************************************************

type
  TYclBitStreamSingleBitReaderClass = class of TYclBitStreamCustomSingleBitReader;

function YclBitOrderToSingleBitReaderClass(BitOrder: TYclBitOrder): TYclBitStreamSingleBitReaderClass;

// ***********************************  Bit-Stream Writer  ***********************************

type
  TYclBitStreamToByteFlushEvent = procedure(Sender: TObject; Data: Byte) of object;

  // Bit-Stream-Klasse, welche jedes volle Byte über ein Ereignis weitergibt
  TYclBitStreamToByteCustomWriter = class(TObject)
  protected
    FBuffer: Byte;        // Zwischenpuffer für 8 Bit
    FBitIndex: Cardinal;  // 0 für erstes Bit als nächstes zu schreibendes Bit
                          // reale Bit-Position hängt von Bit-Stream-Order ab
    FOnFlush: TYclBitStreamToByteFlushEvent;
  public
    constructor Create;
    // ein Bit schreiben
    procedure WriteBit(Data: Boolean); overload; virtual; abstract;
    // ein Bit schreiben; Data = 0 => Bit := 0; Data <> 0 => Bit := 1
    procedure WriteBit(Data: Cardinal); overload;
    // Count Bits schreiben, erstes Bit ist Bit 0
    procedure WriteBits8L(Data: Byte; Count: Cardinal);
    procedure WriteBits16L(Data: Word; Count: Cardinal);
    procedure WriteBits32L(Data: LongWord; Count: Cardinal); virtual; abstract;
    // Count Bits schreiben, erstes Bit ist Bit 7, 15 oder 31
    procedure WriteBits8H(Data: Byte; Count: Cardinal);
    procedure WriteBits16H(Data: Word; Count: Cardinal);
    procedure WriteBits32H(Data: LongWord; Count: Cardinal); virtual; abstract;
    // gibt aktuelles Byte aus, Bitposition danach wieder an Bytegrenze
    procedure Flush; //virtual;
    // Bit-Puffer löschen und Bitposition auf Anfang, ohne auszugeben
    procedure Clear; //virtual;
  end;

  TYclBitStreamToByteCustomWriterL = class(TYclBitStreamToByteCustomWriter)
  public
    procedure WriteBit(Data: Boolean); override;
    procedure WriteBits32L(Data: LongWord; Count: Cardinal); override;
    procedure WriteBits32H(Data: LongWord; Count: Cardinal); override;
  end;

  TYclBitStreamToByteCustomWriterH = class(TYclBitStreamToByteCustomWriter)
  public
    procedure WriteBit(Data: Boolean); override;
    procedure WriteBits32L(Data: LongWord; Count: Cardinal); override;
    procedure WriteBits32H(Data: LongWord; Count: Cardinal); override;
  end;

// *******************************************************************************************

type
  TYclBitStreamToByteWriterClass = class of TYclBitStreamToByteCustomWriter;

function YclBitOrderToBitToByteWriterClass(BitOrder: TYclBitOrder): TYclBitStreamToByteWriterClass;

// *******************************************************************************************

type
  // Achtung!
  // keine Instanz von TJiclBitStreamToByteStreamCustomWriter erzeugen,
  // da FDstStream nicht zugewiesen
  TYclBitStreamToByteStreamCustomWriter = class(TObject)
  protected
    FBitStream: TYclBitStreamToByteCustomWriter;   // Bit-Stream
                                                   // wird von dieser Klasse verwaltet
    FDstStream: TStream;                           // wird nicht von dieser Klasse verwaltet
    procedure FlushEvent(Sender: TObject; Data: Byte);
  public
    constructor Create(BitStreamOrder: TYclBitOrder);
    destructor Destroy; override;
    property BitStream: TYclBitStreamToByteCustomWriter read FBitStream;
  end;

  TYclBitStreamToByteStreamWriter = class(TYclBitStreamToByteStreamCustomWriter)
  public
    // setzt DstStream -> FDstStream, DstStream wird nicht von dieser Klasse verwaltet
    constructor Create(BitStreamOrder: TYclBitOrder; DstStream: TStream);
  end;

  // Bit-Stream-Klasse, welche die geschriebenen Bytes in einen selbstverwalteten Puffer ausgibt
  TYclBitStreamToMemoryWriter = class(TYclBitStreamToByteStreamCustomWriter)
  private
    function GetMemoryStream: TMemoryStream;
    function GetDataSize: Cardinal;
    function GetDataCapacity: Cardinal;
    procedure SetDataCapacity(Value: Cardinal);
  protected
    property MemoryStream: TMemoryStream read GetMemoryStream;
  public
    constructor Create(BitStreamOrder: TYclBitOrder);
    destructor Destroy; override;
    // setzt MemoryStream auf Beginn, Inhalt und Größe bleiben erhalten
    procedure Reset;
    // schreibt DataSize Daten vom Anfang des MemoryStreams in einen Stream
    procedure WriteToStream(Stream: TStream);
    // DataSize entspricht aktueller Position im MemoryStream
    property DataSize: Cardinal read GetDataSize;
    // DataCapacity entspricht Size des MemoryStream
    // Streamgröße wächst automatisch mit der Ausgabe der Daten, schrumpft aber nicht wieder
    property DataCapacity: Cardinal read GetDataCapacity write SetDataCapacity;
  end;

implementation

// *******************************  Arithmetische Operationen  *******************************

// Bit setzen/löschen
function SetBit8L(Data: Byte; Index: Cardinal; Value: Boolean): Byte;
var
  BitNumber: Cardinal;
begin
  BitNumber := Index mod 8;
  if Value then
    Result := Data or BitMaskSetBitN8LPtr^[BitNumber]
  else
    Result := Data and BitMaskClearBitN8LPtr^[BitNumber];
end;

function SetBit8H(Data: Byte; Index: Cardinal; Value: Boolean): Byte;
var
  BitNumber: Cardinal;
begin
  BitNumber := Index mod 8;
  if Value then
    Result := Data or BitMaskSetBitN8HPtr^[BitNumber]
  else
    Result := Data and BitMaskClearBitN8HPtr^[BitNumber];
end;


// Bit löschen
function ClearBit32(Data: LongWord; Index: Integer): LongWord;
begin
  case Index of
    0..31:
      Result := Data and BitMaskClearBitN32LPtr^[Index];
  else
    Result := Data;
  end;
end;

function ClearBit64(Data: UInt64; Index: Integer): UInt64;
begin
  Result := Data;
  case Index of
    0..31:
      TArrayLongWord2(Result)[0] := TArrayLongWord2(Data)[0] and BitMaskClearBitN32LPtr^[Index];
    32..63:
      TArrayLongWord2(Result)[1] := TArrayLongWord2(Data)[1] and BitMaskClearBitN32LPtr^[Index - 32];
  end;
end;

procedure ClearBitBuffer(var Data; DataSize, Index: Integer);
var
  Ptr: PArrayByte;
  ByteIdx: Integer;
begin
  Ptr := @Data;
  if (0 <= Index) and (Index < DataSize * 8) then begin
    ByteIdx := Index div 8;
    Ptr^[ByteIdx] := Ptr^[ByteIdx] and Byte(BitMaskClearBitN8LPtr^[Index mod 8]);
  end;
end;

// Bit setzen oder löschen
function SetBit32(Data: LongWord; Index: Integer; Value: Boolean = True): LongWord;
begin
  case Index of
    0..31: begin
      if Value then
        Result := Data or BitMaskSetBitN32LPtr^[Index]
      else
        Result := Data and BitMaskClearBitN32LPtr^[Index];
    end;
  else
    Result := Data;
  end;
end;

function SetBit64(Data: UInt64; Index: Integer; Value: Boolean = True): UInt64;
begin
  Result := Data;
  case Index of
    0..31: begin
      if Value then
        TArrayLongWord2(Result)[0] := TArrayLongWord2(Data)[1] or BitMaskSetBitN32LPtr^[Index]
      else
        TArrayLongWord2(Result)[0] := TArrayLongWord2(Data)[1] and BitMaskClearBitN32LPtr^[Index - 32];
    end;
    32..63: begin
      if Value then
        TArrayLongWord2(Result)[1] := TArrayLongWord2(Data)[1] or BitMaskSetBitN32LPtr^[Index]
      else
        TArrayLongWord2(Result)[1] := TArrayLongWord2(Data)[1] and BitMaskClearBitN32LPtr^[Index - 32];
    end;
  end;
end;

procedure SetBitBuffer(var Data; DataSize, Index: Integer; Value: Boolean = True);
var
  Ptr: PArrayByte;
  ByteIdx: Integer;
begin
  Ptr := @Data;
  if (0 <= Index) and (Index < DataSize * 8) then begin
    ByteIdx := Index div 8;
    if Value then
      Ptr^[ByteIdx] := Ptr^[ByteIdx] or Byte(BitMaskSetBitN8LPtr^[Index mod 8])
    else
      Ptr^[ByteIdx] := Ptr^[ByteIdx] and Byte(BitMaskClearBitN8LPtr^[Index mod 8]);
  end;
end;

// Bit invertieren
function ToogleBit32(Data: LongWord; Index: Integer): LongWord;
begin
  case Index of
    0..31:
      Result := Data xor BitMaskSetBitN32LPtr^[Index];
  else
    Result := Data;
  end;
end;

function ToogleBit64(Data: UInt64; Index: Integer): UInt64;
begin
  Result := Data;
  case Index of
    0..31:
      TArrayLongWord2(Result)[0] := TArrayLongWord2(Data)[0] xor BitMaskSetBitN32LPtr^[Index];
    32..63:
      TArrayLongWord2(Result)[1] := TArrayLongWord2(Data)[1] xor BitMaskSetBitN32LPtr^[Index - 32];
  end;
end;

procedure ToogleBitBuffer(var Data; DataSize, Index: Integer);
var
  Ptr: PArrayByte;
  ByteIdx: Integer;
begin
  Ptr := @Data;
  if (0 <= Index) and (Index < DataSize * 8) then begin
    ByteIdx := Index div 8;
    Ptr^[ByteIdx] := Ptr^[ByteIdx] xor Byte(BitMaskSetBitN8LPtr^[Index mod 8]);
  end;
end;

function TestBit32(Data: LongWord; Index: Integer): Boolean;
begin
  case Index of
    0..31:
      Result := (Data and BitMaskSetBitN32LPtr^[Index]) <> 0;
  else
    Result := False;
  end;
end;

function TestBit64(Data: UInt64; Index: Integer): Boolean;
begin
  case Index of
    0..31:
      Result := (TArrayLongWord2(Data)[0] and BitMaskSetBitN32LPtr^[Index]) <> 0;
    32..63:
      Result := (TArrayLongWord2(Data)[1] and BitMaskSetBitN32LPtr^[Index - 32]) <> 0;
  else
    Result := False;
  end;
end;

function TestBitBuffer(var Data; DataSize, Index: Integer): Boolean;
var
  Ptr: PArrayByte;
begin
  Ptr := @Data;
  if (0 <= Index) and (Index < DataSize * 8) then
    Result := (Ptr^[Index div 8] and Byte(BitMaskSetBitN8LPtr^[Index mod 8])) <> 0
  else
    Result := False;
end;

// mehrere Bits gleichzeitig setzen / löschen / umkehren / abfragen
function ClearBits32(Data: LongWord; StartIndex, EndIndex: Integer): LongWord;
begin
  if StartIndex < 0 then
    StartIndex := 0;
  if EndIndex > 31 then
    EndIndex := 31;
  if StartIndex <= EndIndex then
    Result := Data and
        (BitMaskSetLowerRange32LPtr^[StartIndex] or BitMaskSetHigherRange32LPtr^[EndIndex])
  else
    Result := Data;
end;

function ClearBits64(Data: UInt64; StartIndex, EndIndex: Integer): UInt64;
begin
  if StartIndex < 0 then
    StartIndex := 0;
  if EndIndex > 63 then
    EndIndex := 63;
  if StartIndex <= EndIndex then begin
    if EndIndex < 32 then begin  // nur untere 32 Bit
      TArrayLongWord2(Result)[0] := TArrayLongWord2(Data)[0] and
          (BitMaskSetLowerRange32LPtr^[StartIndex] or BitMaskSetHigherRange32LPtr^[EndIndex]);
      TArrayLongWord2(Result)[1] := TArrayLongWord2(Data)[1];
    end
    else if StartIndex >= 32 then begin  // nur obere 32 Bit
      TArrayLongWord2(Result)[0] := TArrayLongWord2(Data)[0];
      TArrayLongWord2(Result)[1] := TArrayLongWord2(Data)[1] and
          (BitMaskSetLowerRange32LPtr^[StartIndex - 32] or BitMaskSetHigherRange32LPtr^[EndIndex - 32]);
    end
    else begin  // StartIndex untere 32 Bit, EndIndex obere 32 Bit
      TArrayLongWord2(Result)[0] := TArrayLongWord2(Data)[0] and BitMaskSetLowerRange32LPtr^[StartIndex];
      TArrayLongWord2(Result)[1] := TArrayLongWord2(Data)[1] and BitMaskSetHigherRange32LPtr^[EndIndex - 32];
    end;
  end
  else
    Result := Data;
end;

procedure ClearBitsBuffer(var Data; DataSize, StartIndex, EndIndex: Integer);
var
  Ptr: PArrayByte;
  StartByte, EndByte, i: Integer;
begin
  if StartIndex < 0 then
    StartIndex := 0;
  if EndIndex >= DataSize * 8 then
    EndIndex := DataSize * 8 - 1;
  if StartIndex > EndIndex then
    Exit;
  StartByte := StartIndex div 8;
  StartIndex := StartIndex mod 8;
  EndByte := EndIndex div 8;
  EndIndex := EndIndex mod 8;
  Ptr := @Data;
  if StartByte = EndByte then begin
    Ptr^[StartByte] := Ptr^[StartByte] and
        (BitMaskSetLowerRange8LPtr^[StartIndex] or BitMaskSetHigherRange8LPtr^[EndIndex]);
  end
  else begin
    Ptr^[StartByte] := Ptr^[StartByte] and BitMaskSetLowerRange8LPtr^[StartIndex];
    for i := StartByte + 1 to EndByte - 1 do
      Ptr^[i] := 0;
    Ptr^[EndByte] := Ptr^[EndByte] and BitMaskSetHigherRange8LPtr^[EndIndex];
  end;
end;

function SetBits32(Data: LongWord; StartIndex, EndIndex: Integer; Value: Boolean = True): LongWord;
begin
  if StartIndex < 0 then
    StartIndex := 0;
  if EndIndex > 31 then
    EndIndex := 31;
  if StartIndex <= EndIndex then begin
    if Value then
      Result := Data or
          (BitMaskClearLowerRange32LPtr^[StartIndex] and BitMaskClearHigherRange32LPtr^[EndIndex])
    else
      Result := Data and
          (BitMaskSetLowerRange32LPtr^[StartIndex] or BitMaskSetHigherRange32LPtr^[EndIndex]);
  end
  else
    Result := Data;
end;

function SetBits64(Data: UInt64; StartIndex, EndIndex: Integer; Value: Boolean = True): UInt64;
begin
  if StartIndex < 0 then
    StartIndex := 0;
  if EndIndex > 63 then
    EndIndex := 63;
  if StartIndex <= EndIndex then begin
    if EndIndex < 32 then begin  // nur untere 32 Bit
      if Value then
        TArrayLongWord2(Result)[0] := TArrayLongWord2(Data)[0] or
            (BitMaskClearLowerRange32LPtr^[StartIndex] and BitMaskClearHigherRange32LPtr^[EndIndex])
      else
        TArrayLongWord2(Result)[0] := TArrayLongWord2(Data)[0] and
            (BitMaskSetLowerRange32LPtr^[StartIndex] or BitMaskSetHigherRange32LPtr^[EndIndex]);
      TArrayLongWord2(Result)[1] := TArrayLongWord2(Data)[1];
    end
    else if StartIndex >= 32 then begin  // nur obere 32 Bit
      TArrayLongWord2(Result)[0] := TArrayLongWord2(Data)[0];
      if Value then
        TArrayLongWord2(Result)[1] := TArrayLongWord2(Data)[1] or
            (BitMaskClearLowerRange32LPtr^[StartIndex - 32] and BitMaskClearHigherRange32LPtr^[EndIndex - 32])
      else
        TArrayLongWord2(Result)[1] := TArrayLongWord2(Data)[1] and
            (BitMaskSetLowerRange32LPtr^[StartIndex - 32] or BitMaskSetHigherRange32LPtr^[EndIndex - 32]);
    end
    else begin  // StartIndex untere 32 Bit, EndIndex obere 32 Bit
      if Value then begin
        TArrayLongWord2(Result)[0] := TArrayLongWord2(Data)[0] or BitMaskClearLowerRange32LPtr^[StartIndex];
        TArrayLongWord2(Result)[1] := TArrayLongWord2(Data)[1] or BitMaskClearHigherRange32LPtr^[EndIndex - 32];
      end
      else begin
        TArrayLongWord2(Result)[0] := TArrayLongWord2(Data)[0] and BitMaskSetLowerRange32LPtr^[StartIndex];
        TArrayLongWord2(Result)[1] := TArrayLongWord2(Data)[1] and BitMaskSetHigherRange32LPtr^[EndIndex - 32];
      end;
    end;
  end
  else
    Result := Data;
end;

procedure SetBitsBuffer(var Data; DataSize, StartIndex, EndIndex: Integer; Value: Boolean = True);
var
  Ptr: PArrayByte;
  StartByte, EndByte, i: Integer;
begin
  if StartIndex < 0 then
    StartIndex := 0;
  if EndIndex >= DataSize * 8 then
    EndIndex := DataSize * 8 - 1;
  if StartIndex > EndIndex then
    Exit;
  StartByte := StartIndex div 8;
  StartIndex := StartIndex mod 8;
  EndByte := EndIndex div 8;
  EndIndex := EndIndex mod 8;
  Ptr := @Data;
  if StartByte = EndByte then begin
    if Value then
      Ptr^[StartByte] := Ptr^[StartByte] or
          (BitMaskClearLowerRange8LPtr^[StartIndex] and BitMaskClearHigherRange8LPtr^[EndIndex])
    else
      Ptr^[StartByte] := Ptr^[StartByte] and
          (BitMaskSetLowerRange8LPtr^[StartIndex] or BitMaskSetHigherRange8LPtr^[EndIndex]);
  end
  else begin
    if Value then begin
      Ptr^[StartByte] := Ptr^[StartByte] or BitMaskClearLowerRange8LPtr^[StartIndex];
      for i := StartByte + 1 to EndByte - 1 do
        Ptr^[i] := $FF;
      Ptr^[EndByte] := Ptr^[EndByte] or BitMaskClearHigherRange8LPtr^[EndIndex];
    end
    else begin
      Ptr^[StartByte] := Ptr^[StartByte] and BitMaskSetLowerRange8LPtr^[StartIndex];
      for i := StartByte + 1 to EndByte - 1 do
        Ptr^[i] := 0;
      Ptr^[EndByte] := Ptr^[EndByte] and BitMaskSetHigherRange8LPtr^[EndIndex];
    end;
  end;
end;

function ToogleBits32(Data: LongWord; StartIndex, EndIndex: Integer): LongWord;
begin
  if StartIndex < 0 then
    StartIndex := 0;
  if EndIndex > 31 then
    EndIndex := 31;
  if StartIndex <= EndIndex then begin
    Result := Data xor
        (BitMaskClearLowerRange32LPtr^[StartIndex] and BitMaskClearHigherRange32LPtr^[EndIndex]);
  end
  else
    Result := Data;
end;

function ToogleBits64(Data: UInt64; StartIndex, EndIndex: Integer): UInt64;
begin
  if StartIndex < 0 then
    StartIndex := 0;
  if EndIndex > 63 then
    EndIndex := 63;
  if StartIndex <= EndIndex then begin
    if EndIndex < 32 then begin  // nur untere 32 Bit
      TArrayLongWord2(Result)[0] := TArrayLongWord2(Data)[0] xor
          (BitMaskClearLowerRange32LPtr^[StartIndex] and BitMaskClearHigherRange32LPtr^[EndIndex]);
      TArrayLongWord2(Result)[1] := TArrayLongWord2(Data)[1];
    end
    else if StartIndex >= 32 then begin  // nur obere 32 Bit
      TArrayLongWord2(Result)[0] := TArrayLongWord2(Data)[0];
      TArrayLongWord2(Result)[1] := TArrayLongWord2(Data)[1] xor
          (BitMaskClearLowerRange32LPtr^[StartIndex - 32] and BitMaskClearHigherRange32LPtr^[EndIndex - 32]);
    end
    else begin  // StartIndex untere 32 Bit, EndIndex obere 32 Bit
      TArrayLongWord2(Result)[0] := TArrayLongWord2(Data)[0] xor BitMaskClearLowerRange32LPtr^[StartIndex];
      TArrayLongWord2(Result)[1] := TArrayLongWord2(Data)[1] xor BitMaskClearHigherRange32LPtr^[EndIndex - 32];
    end;
  end
  else
    Result := Data;
end;

procedure ToogleBitsBuffer(var Data; DataSize, StartIndex, EndIndex: Integer);
var
  Ptr: PArrayByte;
  StartByte, EndByte, i: Integer;
begin
  if StartIndex < 0 then
    StartIndex := 0;
  if EndIndex >= DataSize * 8 then
    EndIndex := DataSize * 8 - 1;
  if StartIndex > EndIndex then
    Exit;
  StartByte := StartIndex div 8;
  StartIndex := StartIndex mod 8;
  EndByte := EndIndex div 8;
  EndIndex := EndIndex mod 8;
  Ptr := @Data;
  if StartByte = EndByte then begin
    Ptr^[StartByte] := Ptr^[StartByte] xor
        (BitMaskClearLowerRange8LPtr^[StartIndex] and BitMaskClearHigherRange8LPtr^[EndIndex]);
  end
  else begin
    Ptr^[StartByte] := Ptr^[StartByte] xor BitMaskClearLowerRange8LPtr^[StartIndex];
    for i := StartByte + 1 to EndByte - 1 do
      Ptr^[i] := not Ptr^[i];
    Ptr^[EndByte] := Ptr^[EndByte] xor BitMaskClearHigherRange8LPtr^[EndIndex];
  end;
end;

function TestBits32(Data: LongWord; StartIndex, EndIndex: Integer): Boolean;
begin
  if StartIndex < 0 then
    StartIndex := 0;
  if EndIndex > 31 then
    EndIndex := 31;
  if StartIndex <= EndIndex then begin
    Result := (Data and
        (BitMaskClearLowerRange32LPtr^[StartIndex] and BitMaskClearHigherRange32LPtr^[EndIndex])) <> 0;
  end
  else
    Result := False;
end;

function TestBits64(Data: UInt64; StartIndex, EndIndex: Integer): Boolean;
begin
  if StartIndex < 0 then
    StartIndex := 0;
  if EndIndex > 63 then
    EndIndex := 63;
  if StartIndex <= EndIndex then begin
    if EndIndex < 32 then begin  // nur untere 32 Bit
      Result := (TArrayLongWord2(Data)[0] and
          (BitMaskClearLowerRange32LPtr^[StartIndex] and BitMaskClearHigherRange32LPtr^[EndIndex])) <> 0;
    end
    else if StartIndex >= 32 then begin  // nur obere 32 Bit
      Result := (TArrayLongWord2(Data)[1] and
          (BitMaskClearLowerRange32LPtr^[StartIndex - 32] and BitMaskClearHigherRange32LPtr^[EndIndex - 32])) <> 0;
    end
    else begin  // StartIndex untere 32 Bit, EndIndex obere 32 Bit
      Result := ((TArrayLongWord2(Data)[0] and BitMaskClearLowerRange32LPtr^[StartIndex]) or
                 (TArrayLongWord2(Data)[1] and BitMaskClearHigherRange32LPtr^[EndIndex - 32])) <> 0;
    end;
  end
  else
    Result := False;
end;

function TestBitsBuffer(const Data; DataSize, StartIndex, EndIndex: Integer): Boolean;
var
  Ptr: PArrayByte;
  StartByte, EndByte, i: Integer;
begin
  Result := False;
  if StartIndex < 0 then
    StartIndex := 0;
  if EndIndex >= DataSize * 8 then
    EndIndex := DataSize * 8 - 1;
  if StartIndex > EndIndex then
    Exit;
  Ptr := @Data;
  StartByte := StartIndex div 8;
  StartIndex := StartIndex mod 8;
  EndByte := EndIndex div 8;
  EndIndex := EndIndex mod 8;
  if StartByte = EndByte then begin
    Result := (Ptr^[StartByte] and
        (BitMaskClearLowerRange8LPtr^[StartIndex] and BitMaskClearHigherRange8LPtr^[EndIndex])) <> 0;
  end
  else begin
    Result := (Ptr^[StartByte] and BitMaskClearLowerRange8LPtr^[StartIndex]) <> 0;
    if not Result then begin
      for i := StartByte + 1 to EndByte - 1 do begin
        Result := Ptr[i] <> 0;
        if Result then
          Break;
      end;
      if not Result then 
        Result := (Ptr^[EndByte] and BitMaskClearHigherRange8LPtr^[EndIndex]) <> 0;
    end;
  end;
end;

// *************************************  Count 0 bits  **************************************

function Count0Bits8(Value: Cardinal): Cardinal;
begin
  Result := 8 - Count1Bits8(Value);
end;

function Count0Bits16(Value: Cardinal): Cardinal;
begin
  Result := 16 - Count1Bits16(Value);
end;

function Count0Bits32(Value: LongWord): Cardinal;
begin
  Result := 32 - Count1Bits32(Value);
end;

function Count0Bits64(Value: UInt64): Cardinal;
begin
  Result := 64 - Count1Bits64(Value);
end;

function Count0Bits(Value: Cardinal; StartIndex, EndIndex: Integer): Cardinal;
var
  TotalBitCount: Integer;
begin
  TotalBitCount := EndIndex - StartIndex + 1;
  if TotalBitCount > 0 then
    Result := Cardinal(TotalBitCount) - Count1Bits(Value, StartIndex, EndIndex)
  else
    Result := 0;
end;

function Count0BitsBuffer(const Data; DataSize, StartIndex, EndIndex: Integer): Cardinal;
var
  TotalBitCount: Integer;
begin
  TotalBitCount := EndIndex - StartIndex + 1;
  if TotalBitCount > 0 then
    Result := Cardinal(TotalBitCount) - Count0BitsBuffer(Data, DataSize, StartIndex, EndIndex)
  else
    Result := 0;
end;

// *************************************  Count 1 bits  **************************************

const
  BitCount1Nibbly: array[$00..$0F] of Byte = (
    0, 1, 1, 2,    // 0000 -> 0   0001 -> 1   0010 -> 1   0011 ->  2
    1, 2, 2, 3,    // 0100 -> 1   0101 -> 2   0110 -> 2   0111 ->  3
    1, 2, 2, 3,    // 1000 -> 1   1001 -> 2   1010 -> 2   1011 ->  3
    2, 3, 3, 4     // 1100 -> 2   1101 -> 3   1110 -> 3   1111 ->  4
  );

function Count1Bits8(Value: Cardinal): Cardinal;
begin
  Result := BitCount1Nibbly[Value and $0F];
  Result := Result + BitCount1Nibbly[(Value shr 4) and $0F];
end;

function Count1Bits16(Value: Cardinal): Cardinal;
begin
  Result := BitCount1Nibbly[Value and $0F];
  Value := Value shr 4;
  Result := Result + BitCount1Nibbly[Value and $0F];
  Value := Value shr 4;
  Result := Result + BitCount1Nibbly[Value and $0F];
  Value := Value shr 4;
  Result := Result + BitCount1Nibbly[Value and $0F];
end;

function Count1Bits32(Value: LongWord): Cardinal;
var
  W, X, Y: LongWord;
begin
  W := Value - ((Value shr 1) and $55555555);
  X := (W and $33333333) + ((W shr 2) and $33333333);
  Y := (X + (X shr 4)) and $0F0F0F0F;
  Result := (Y * $01010101) shr 24;
end;

function Count1Bits64(Value: UInt64): Cardinal;
begin
  Result := Count1Bits32(TArrayLongWord2(Value)[0]) +
            Count1Bits32(TArrayLongWord2(Value)[1]);
end;

function Count1Bits(Value: Cardinal; StartIndex, EndIndex: Integer): Cardinal;
begin
  if StartIndex < 0 then
    StartIndex := 0;
  if EndIndex >= SizeOf(Cardinal) * 8 then
    EndIndex := SizeOf(Cardinal) * 8 - 1;
  if StartIndex <= EndIndex then begin
    // nicht gefragte Bits auf 0 setzen
    Value := Value and BitMaskClearLowerRange32LPtr^[StartIndex];
    Value := Value and BitMaskClearHigherRange32LPtr^[EndIndex];
    Result := Count1Bits32(Value);
  end
  else
    Result := 0;
end;

function Count1BitsBufferInternal8(const Data;
    StartByte, StartBit, EndByte, EndBit: Integer): Cardinal;
var
  Ptr: PByte;
  V: Byte;
  i: Integer;
begin
  Result := 0;
  Ptr := @Data;
  Inc(Ptr, StartByte);
  V := Ptr^ and BitMaskClearLowerRange8LPtr^[StartBit];
  for i := StartByte + 1 to EndByte do begin
    Result := Result + Count1Bits8(V);
    Inc(Ptr);
    V := Ptr^;
  end;
  V := V and BitMaskClearHigherRange8LPtr^[EndBit];
  Result := Result + Count1Bits8(V);
end;

function Count1BitsBuffer(const Data; DataSize, StartIndex, EndIndex: Integer): Cardinal;
var
  MSB: Integer;
begin
  // Start begrenzen
  if StartIndex < 0 then
    StartIndex := 0;
  // Ende auf Anzahl Bytes begrenzen
  MSB := DataSize * 8 - 1;
  if EndIndex > MSB then
    EndIndex := MSB;
  // sind Bits zu zählen?
  if StartIndex <= EndIndex then begin
    { TODO : optimieren }
    Result := Count1BitsBufferInternal8(Data,
        StartIndex div 8, StartIndex mod 8, EndIndex div 8, EndIndex mod 8);
  end
  else
    Result := 0;
end;

// *******************************  Bit-Order tauschen  **************************************

const
  BitSwapLUT: array[Byte] of Byte = (
    $00, $80, $40, $C0,  $20, $A0, $60, $E0,  $10, $90, $50, $D0,  $30, $B0, $70, $F0,
    $08, $88, $48, $C8,  $28, $A8, $68, $E8,  $18, $98, $58, $D8,  $38, $B8, $78, $F8,
    $04, $84, $44, $C4,  $24, $A4, $64, $E4,  $14, $94, $54, $D4,  $34, $B4, $74, $F4,
    $0C, $8C, $4C, $CC,  $2C, $AC, $6C, $EC,  $1C, $9C, $5C, $DC,  $3C, $BC, $7C, $FC,

    $02, $82, $42, $C2,  $22, $A2, $62, $E2,  $12, $92, $52, $D2,  $32, $B2, $72, $F2,
    $0A, $8A, $4A, $CA,  $2A, $AA, $6A, $EA,  $1A, $9A, $5A, $DA,  $3A, $BA, $7A, $FA,
    $06, $86, $46, $C6,  $26, $A6, $66, $E6,  $16, $96, $56, $D6,  $36, $B6, $76, $F6,
    $0E, $8E, $4E, $CE,  $2E, $AE, $6E, $EE,  $1E, $9E, $5E, $DE,  $3E, $BE, $7E, $FE,

    $01, $81, $41, $C1,  $21, $A1, $61, $E1,  $11, $91, $51, $D1,  $31, $B1, $71, $F1,
    $09, $89, $49, $C9,  $29, $A9, $69, $E9,  $19, $99, $59, $D9,  $39, $B9, $79, $F9,
    $05, $85, $45, $C5,  $25, $A5, $65, $E5,  $15, $95, $55, $D5,  $35, $B5, $75, $F5,
    $0D, $8D, $4D, $CD,  $2D, $AD, $6D, $ED,  $1D, $9D, $5D, $DD,  $3D, $BD, $7D, $FD,

    $03, $83, $43, $C3,  $23, $A3, $63, $E3,  $13, $93, $53, $D3,  $33, $B3, $73, $F3,
    $0B, $8B, $4B, $CB,  $2B, $AB, $6B, $EB,  $1B, $9B, $5B, $DB,  $3B, $BB, $7B, $FB,
    $07, $87, $47, $C7,  $27, $A7, $67, $E7,  $17, $97, $57, $D7,  $37, $B7, $77, $F7,
    $0F, $8F, $4F, $CF,  $2F, $AF, $6F, $EF,  $1F, $9F, $5F, $DF,  $3F, $BF, $7F, $FF
  );

function BitSwap8(Value: Cardinal): Cardinal;
begin
  Result := BitSwapLUT[Value and $FF];
end;

function BitSwap16(Value: Cardinal): Cardinal;
begin
  Result := BitSwapLUT[Value and $FF] shl 8;
  Value := Value shr 8;
  Result := Result or BitSwapLUT[Value];
end;

function BitSwap32(Value: LongWord): LongWord;
begin
  Result := BitSwapLUT[Value and $FF] shl 24;
  Value := Value shr 8;
  Result := Result or (BitSwapLUT[Value] shl 16);
  Value := Value shr 8;
  Result := Result or (BitSwapLUT[Value] shl 8);
  Value := Value shr 8;
  Result := Result or BitSwapLUT[Value];
end;

function BitSwap64(Value: UInt64): UInt64;
begin
  TArrayLongWord2(Result)[0] := BitSwap32(TArrayLongWord2(Value)[1]);
  TArrayLongWord2(Result)[1] := BitSwap32(TArrayLongWord2(Value)[0]);
end;

procedure BitSwapBuffer(var Data; DataSize: Integer);
var
  Ptr: PArrayByte;
  Idx1, Idx2: Integer;
  Temp: Byte;
begin
  Ptr := @Data;
  Idx2 := DataSize - 1;
  for Idx1 := 0 to DataSize div 2 - 1 do begin
    Temp := BitSwapLUT[Ptr^[Idx1]];
    Ptr^[Idx1] := BitSwapLUT[Ptr^[Idx2]];
    Ptr^[Idx2] := Temp;
    Dec(Idx2);
  end;
  if Odd(DataSize) then
    Ptr^[Idx2] := BitSwapLUT[Ptr^[Idx2]];
end;

function BitSwapN32(Value: LongWord; BitCount: Cardinal): LongWord;
begin
  case BitCount of
    0, 1:
      Result := Value;
    2..7:
      Result := BitSwapLUT[Value and $FF] shr (8 - BitCount);
    8:
      Result := BitSwapLUT[Value and $FF];
    9..15:
      Result := BitSwap16(Value) shr (16 - BitCount);
    16:
      Result := BitSwap16(Value);
    17..31:
      Result := BitSwap32(Value) shr (32 - BitCount);
  else
    Result := BitSwap32(Value);
  end;
end;

function BitSwapN64(Value: UInt64; BitCount: Cardinal): UInt64;
begin
  case BitCount of
    0..32:
      TArrayLongWord2(Result)[0] := BitSwapN32(TArrayLongWord2(Value)[0], BitCount);
    33..63:
      Result := BitSwap64(Value) shr (64 - BitCount);
  else
    Result := BitSwap64(Value);
  end;
end;

// **********************  Index höchstes / niedrigstes Bit ermitteln  ***********************

const
  // Bit 7..4: Index unterstes 0 Bit
  // Bit 3..0: Index unterstes 1 Bit
  // Index 8, kein entsprechendes Bit
  LowestBitLUT: array[Byte] of Byte = (
    $08, $10, $01, $20,  $02, $10, $01, $30,   $03, $10, $01, $20,  $02, $10, $02, $40,
    $04, $10, $01, $20,  $02, $10, $01, $30,   $03, $10, $01, $20,  $02, $10, $02, $50,
    $05, $10, $01, $20,  $02, $10, $01, $30,   $03, $10, $01, $20,  $02, $10, $02, $40,
    $04, $10, $01, $20,  $02, $10, $01, $30,   $03, $10, $01, $20,  $02, $10, $02, $60,

    $06, $10, $01, $20,  $02, $10, $01, $30,   $03, $10, $01, $20,  $02, $10, $02, $40,
    $04, $10, $01, $20,  $02, $10, $01, $30,   $03, $10, $01, $20,  $02, $10, $02, $50,
    $05, $10, $01, $20,  $02, $10, $01, $30,   $03, $10, $01, $20,  $02, $10, $02, $40,
    $04, $10, $01, $20,  $02, $10, $01, $30,   $03, $10, $01, $20,  $02, $10, $02, $70,

    $07, $10, $01, $20,  $02, $10, $01, $30,   $03, $10, $01, $20,  $02, $10, $02, $40,
    $04, $10, $01, $20,  $02, $10, $01, $30,   $03, $10, $01, $20,  $02, $10, $02, $50,
    $05, $10, $01, $20,  $02, $10, $01, $30,   $03, $10, $01, $20,  $02, $10, $02, $40,
    $04, $10, $01, $20,  $02, $10, $01, $30,   $03, $10, $01, $20,  $02, $10, $02, $60,

    $06, $10, $01, $20,  $02, $10, $01, $30,   $03, $10, $01, $20,  $02, $10, $02, $40,
    $04, $10, $01, $20,  $02, $10, $01, $30,   $03, $10, $01, $20,  $02, $10, $02, $50,
    $05, $10, $01, $20,  $02, $10, $01, $30,   $03, $10, $01, $20,  $02, $10, $02, $40,
    $04, $10, $01, $20,  $02, $10, $01, $30,   $03, $10, $01, $20,  $02, $10, $02, $80
  );

// -1, wenn kein entsprechendes Bit vorhanden ist
function Lowest0Bit8(Value: Cardinal): Integer;
begin
  Value := Value and $FF;
  if Value = $FF then
    Result := -1
  else
    Result := LowestBitLUT[Value] shr 4;
end;

function Lowest0Bit16(Value: Cardinal): Integer;
begin
  Value := Value and $FFFF;
  if Value = $FFFF then
    Result := -1
  else begin
    Result := LowestBitLUT[Value and $FF] shr 4;
    if Result >= 8 then begin
      Value := Value shr 8;
      Result := (LowestBitLUT[Value and $FF] shr 4) + 8;
    end;
  end;
end;

function Lowest0Bit32(Value: LongWord): Integer;
begin
  if Value = $FFFFFFFF then
    Result := -1
  else begin
    Result := LowestBitLUT[Value and $FF] shr 4;
    if Result >= 8 then begin
      Value := Value shr 8;
      Result := (LowestBitLUT[Value and $FF] shr 4) + 8;
      if Result >= 16 then begin
        Value := Value shr 8;
        Result := (LowestBitLUT[Value and $FF] shr 4) + 16;
        if Result >= 24 then begin
          Value := Value shr 8;
          Result := (LowestBitLUT[Value and $FF] shr 4) + 24;
        end;
      end;
    end;
  end;
end;

function Lowest0Bit64(Value: UInt64): Integer;
begin
  Result := Lowest0Bit32(TArrayLongWord2(Value)[0]);
  if Result < 0 then
    Result := Lowest0Bit32(TArrayLongWord2(Value)[1]);
end;

function Lowest0BitBuffer(const Data; DataSize: Integer): Integer;
var
  Ptr: PArrayByte;
  i: Integer;
  Res: Integer; 
begin
  Ptr := @Data;
  Result := -1;
  for i := 0 to DataSize - 1 do begin
    Res := LowestBitLUT[Ptr^[i]] shr 4;
    if Res < 8 then begin
      Result := Res + i * 8;
      Break;
    end;
  end;
end;

function Lowest1Bit8(Value: Cardinal): Integer;
begin
  Value := Value and $FF;
  if Value = 0 then
    Result := -1
  else
    Result := LowestBitLUT[Value] and $0F;
end;

function Lowest1Bit16(Value: Cardinal): Integer;
begin
  Value := Value and $FFFF;
  if Value = 0 then
    Result := -1
  else begin
    Result := LowestBitLUT[Value and $FF] and $0F;
    if Result >= 8 then begin
      Value := Value shr 8;
      Result := (LowestBitLUT[Value and $FF] and $0F) + 8;
    end;
  end;
end;

function Lowest1Bit32(Value: LongWord): Integer;
begin
  if Value = 0 then
    Result := -1
  else begin
    Result := LowestBitLUT[Value and $FF] and $0F;
    if Result >= 8 then begin
      Value := Value shr 8;
      Result := (LowestBitLUT[Value and $FF] and $0F) + 8;
      if Result >= 16 then begin
        Value := Value shr 8;
        Result := (LowestBitLUT[Value and $FF] and $0F) + 16;
        if Result >= 24 then begin
          Value := Value shr 8;
          Result := (LowestBitLUT[Value and $FF] and $0F) + 24;
        end;
      end;
    end;
  end;
end;

function Lowest1Bit64(Value: UInt64): Integer;
begin
  Result := Lowest1Bit32(TArrayLongWord2(Value)[0]);
  if Result < 0 then
    Result := Lowest1Bit32(TArrayLongWord2(Value)[1]) + 32;
end;

function Lowest1BitBuffer(const Data; DataSize: Integer): Integer;
var
  Ptr: PArrayByte;
  i: Integer;
  Res: Integer; 
begin
  Ptr := @Data;
  Result := -1;
  for i := 0 to DataSize - 1 do begin
    Res := LowestBitLUT[Ptr^[i]] and $0F;
    if Res < 8 then begin
      Result := Res + i * 8;
      Break;
    end;
  end;
end;

const
  // Bit 7..4: Index oberstes 0 Bit
  // Bit 3..0: Index oberstes 1 Bit
  // Index 8, kein entsprechendes Bit
  HighestBitLUT: array[Byte] of Byte = (
    $08, $07, $06, $06,  $05, $05, $05, $05,   $04, $04, $04, $04,  $04, $04, $04, $04,
    $03, $03, $03, $03,  $03, $03, $03, $03,   $03, $03, $03, $03,  $03, $03, $03, $03,
    $02, $02, $02, $02,  $02, $02, $02, $02,   $02, $02, $02, $02,  $02, $02, $02, $02,
    $02, $02, $02, $02,  $02, $02, $02, $02,   $02, $02, $02, $02,  $02, $02, $02, $02,

    $01, $01, $01, $01,  $01, $01, $01, $01,   $01, $01, $01, $01,  $01, $01, $01, $01,
    $01, $01, $01, $01,  $01, $01, $01, $01,   $01, $01, $01, $01,  $01, $01, $01, $01,
    $01, $01, $01, $01,  $01, $01, $01, $01,   $01, $01, $01, $01,  $01, $01, $01, $01,
    $01, $01, $01, $01,  $01, $01, $01, $01,   $01, $01, $01, $01,  $01, $01, $01, $01,

    $10, $10, $10, $10,  $10, $10, $10, $10,   $10, $10, $10, $10,  $10, $10, $10, $10,
    $10, $10, $10, $10,  $10, $10, $10, $10,   $10, $10, $10, $10,  $10, $10, $10, $10,
    $10, $10, $10, $10,  $10, $10, $10, $10,   $10, $10, $10, $10,  $10, $10, $10, $10,
    $10, $10, $10, $10,  $10, $10, $10, $10,   $10, $10, $10, $10,  $10, $10, $10, $10,

    $20, $20, $20, $20,  $20, $20, $20, $20,   $20, $20, $20, $20,  $20, $20, $20, $20,
    $20, $20, $20, $20,  $20, $20, $20, $20,   $20, $20, $20, $20,  $20, $20, $20, $20,
    $30, $30, $30, $30,  $30, $30, $30, $30,   $30, $30, $30, $30,  $30, $30, $30, $30,
    $40, $40, $40, $40,  $40, $40, $40, $40,   $50, $50, $50, $50,  $60, $60, $70, $80
  );

function Highest0Bit8(Value: Cardinal): Integer;
begin
  Value := Value and $FF;
  if Value = $FF then
    Result := -1
  else
    Result := HighestBitLUT[Value] shr 4;
end;

function Highest0Bit16(Value: Cardinal): Integer;
begin
  Value := Value and $FFFF;
  if Value = $FFFF then
    Result := -1
  else begin
    Result := HighestBitLUT[(Value shr 8) and $FF] shr 4;
    if Result >= 8 then
      Result := (HighestBitLUT[Value and $FF] shr 4) + 8;
  end;
end;

function Highest0Bit32(Value: LongWord): Integer;
begin
  if Value = $FFFFFFFF then
    Result := -1
  else begin
    Result := HighestBitLUT[(Value shr 24) and $FF] shr 4;
    if Result >= 8 then begin
      Result := (HighestBitLUT[(Value shr 16) and $FF] shr 4) + 8;
      if Result >= 16 then begin
        Result := (HighestBitLUT[(Value shr 8) and $FF] shr 4) + 16;
        if Result >= 24 then begin
          Result := (HighestBitLUT[Value and $FF] shr 4) + 24;
        end;
      end;
    end;
  end;
end;

function Highest0Bit64(Value: UInt64): Integer;
begin
  Result := Highest0Bit32(TArrayLongWord2(Value)[1]);
  if Result < 0 then
    Result := Highest0Bit32(TArrayLongWord2(Value)[0]) + 32;
end;

function Highest0BitBuffer(const Data; DataSize: Integer): Integer;
var
  Ptr: PArrayByte;
  i: Integer;
  Res: Integer; 
begin
  Ptr := @Data;
  Result := -1;
  for i := DataSize - 1 downto 0 do begin
    Res := HighestBitLUT[Ptr^[i]] shr 4;
    if Res < 8 then begin
      Result := Res + (DataSize - 1 - i) * 8;
      Break;
    end;
  end;
end;

function Highest1Bit8(Value: Cardinal): Integer;
begin
  Value := Value and $FF;
  if Value = $FF then
    Result := -1
  else
    Result := HighestBitLUT[Value] and $0F;
end;

function Highest1Bit16(Value: Cardinal): Integer;
begin
  Value := Value and $FFFF;
  if Value = $FFFF then
    Result := -1
  else begin
    Result := HighestBitLUT[(Value shr 8) and $FF] and $0F;
    if Result >= 8 then
      Result := (HighestBitLUT[Value and $FF] and $0F) + 8;
  end;
end;

function Highest1Bit32(Value: LongWord): Integer;
begin
  if Value = $FFFFFFFF then
    Result := -1
  else begin
    Result := HighestBitLUT[(Value shr 24) and $FF] and $0F;
    if Result >= 8 then begin
      Result := (HighestBitLUT[(Value shr 16) and $FF] and $0F) + 8;
      if Result >= 16 then begin
        Result := (HighestBitLUT[(Value shr 8) and $FF] and $0F) + 16;
        if Result >= 24 then begin
          Result := (HighestBitLUT[Value and $FF] and $0F) + 24;
        end;
      end;
    end;
  end;
end;

function Highest1Bit64(Value: UInt64): Integer;
begin
  Result := Highest1Bit32(TArrayLongWord2(Value)[1]);
  if Result < 0 then
    Result := Highest1Bit32(TArrayLongWord2(Value)[0]) + 32;
end;

function Highest1BitBuffer(const Data; DataSize: Integer): Integer;
var
  Ptr: PArrayByte;
  i: Integer;
  Res: Integer; 
begin
  Ptr := @Data;
  Result := -1;
  for i := DataSize - 1 downto 0 do begin
    Res := HighestBitLUT[Ptr^[i]] and $0F;
    if Res < 8 then begin
      Result := Res + (DataSize - 1 - i) * 8;
      Break;
    end;
  end;
end;

// ***************************************  Rotation  ****************************************

// rotate right
{$IFDEF CPUI386}
function ROR8(Value: Cardinal; Count: Integer): Cardinal; register; assembler;
asm
        MOV     ECX, EDX
        ROR     AL, CL
end;
{$ELSE CPUI386~}
function ROR8(Value: Cardinal; Count: Integer): Cardinal;
begin
  Count := Count mod 8;
  if Count = 0 then
    Result := Value
  else 
    Result := (Value shr Count and BitMaskClearRange8HPtr^[Count]) or
              (Value shl (8 - Count) and BitMaskSetLowerRange8HPtr^[Count]);
end;
{$ENDIF ~CPUI386}

{$IFDEF CPUI386}
function ROR16(Value: Cardinal; Count: Integer): Cardinal; register; assembler;
asm
        MOV     ECX, EDX
        ROR     AX, CL
end;
{$ELSE CPUI386~}
function ROR16(Value: Cardinal; Count: Integer): Cardinal;
begin
  Count := Count mod 16;
  if Count = 0 then
    Result := Value
  else 
    Result := (Value shr Count and BitMaskClearRange16HPtr^[Count]) or
              (Value shl (16 - Count) and BitMaskSetLowerRange16HPtr^[Count]);
end;
{$ENDIF ~CPUI386}

{$IFDEF CPUI386}
function ROR32(Value: LongWord; Count: Integer): LongWord; register; assembler;
asm
        MOV     ECX, EDX
        ROR     EAX, CL
end;
{$ELSE CPUI386~}
function ROR32(Value: LongWord; Count: Integer): LongWord;
begin
  Count := Count mod 32;
  if Count = 0 then
    Result := Value
  else 
    Result := (Value shr Count and BitMaskClearRange32HPtr^[Count]) or
              (Value shl (32 - Count) and BitMaskSetLowerRange32HPtr^[Count]);
end;
{$ENDIF ~CPUI386}

(*
function ROR64(Value: UInt64; Count: Integer): UInt64;
begin
  { TODO : implementieren }
end;

procedure RORBuffer(var Data; DataSize: Integer; Count: Integer);
begin
  { TODO : implementieren }
end;
*)

// rotate left

{$IFDEF CPUI386}
function ROL8(Value: Cardinal; Count: Integer): Cardinal; register; assembler;
asm
        MOV     ECX, EDX
        ROL     AX, CL
end;
{$ELSE CPUI386~}
function ROL8(Value: Cardinal; Count: Integer): Cardinal;
begin
  Count := Count mod 8;
  if Count = 0 then
    Result := Value
  else
    Result := (Value shl Count and BitMaskClearRange8LPtr^[Count]) or
              (Value shr (8 - Count) and BitMaskSetLowerRange8LPtr^[Count]);
end;
{$ENDIF ~CPUI386}

{$IFDEF CPUI386}
function ROL16(Value: Cardinal; Count: Integer): Cardinal; register; assembler;
asm
        MOV     ECX, EDX
        ROL     AX, CL
end;
{$ELSE CPUI386~}
function ROL16(Value: Cardinal; Count: Integer): Cardinal;
begin
  Count := Count mod 16;
  if Count = 0 then
    Result := Value
  else
    Result := (Value shl Count and BitMaskClearRange16LPtr^[Count]) or
              (Value shr (16 - Count) and BitMaskSetLowerRange16LPtr^[Count]);
end;
{$ENDIF ~CPUI386}

{$IFDEF CPUI386}
function ROL32(Value: LongWord; Count: Integer): LongWord; register; assembler;
asm
        MOV     ECX, EDX
        ROL     EAX, CL
end;
{$ELSE CPUI386~}
function ROL32(Value: LongWord; Count: Integer): LongWord;
begin
  Count := Count mod 32;
  if Count = 0 then
    Result := Value
  else
    Result := (Value shl Count and BitMaskClearRange32LPtr^[Count]) or
              (Value shr (32 - Count) and BitMaskSetLowerRange32LPtr^[Count]);
end;
{$ENDIF ~CPUI386}

(*
function ROL64(Value: UInt64; Count: Integer): UInt64;
begin
  { TODO : implementieren }
end;

procedure ROLBuffer(var Data; DataSize: Integer; Count: Integer);
begin
  { TODO : implementieren }
end;
*)

// shift right
function SHR8(Value: Cardinal; Count: Integer): Cardinal;
begin
  Result := Value shr Count;
end;

function SHR16(Value: Cardinal; Count: Integer): Cardinal;
begin
  Result := Value shr Count;
end;

function SHR32(Value: LongWord; Count: Integer): LongWord;
begin
  Result := Value shr Count;
end;

function SHR64(Value: UInt64; Count: Integer): UInt64;
begin
  Result := Value shr Count;
end;

(*
procedure SHRBuffer(var Data; DataSize: Integer; Count: Integer);
begin
  { TODO : implementieren }
end;
*)

// shift left
function SHL8(Value: Cardinal; Count: Integer): Cardinal;
begin
  Result := Value shl Count;
end;

function SHL16(Value: Cardinal; Count: Integer): Cardinal;
begin
  Result := Value shl Count;
end;

function SHL32(Value: LongWord; Count: Integer): LongWord;
begin
  Result := Value shl Count;
end;

function SHL64(Value: UInt64; Count: Integer): UInt64;
begin
  Result := Value shl Count;
end;

(*
procedure SHLBuffer(var Data; DataSize: Integer; Count: Integer);
begin
  { TODO : implementieren }
end;
*)

// shift arithmetic right
{$IFDEF CPUI386}
function SAR8(Value: Cardinal; Count: Integer): Cardinal; register; assembler;
asm
        MOV     ECX, EDX
        SAR     AL, CL
end;
{$ELSE CPUI386~}
function SAR8(Value: Cardinal; Count: Integer): Cardinal;
begin
  Count := Count mod 8;
  Result := Value shr Count;
  if (Value and $80) <> 0 then
    Result := BitMaskSetLowerRange8HPtr^[Count];
end;
{$ENDIF ~CPUI386}

{$IFDEF CPUI386}
function SAR16(Value: Cardinal; Count: Integer): Cardinal; register; assembler;
asm
        MOV     ECX, EDX
        SAR     AX, CL
end;
{$ELSE CPUI386~}
function SAR16(Value: Cardinal; Count: Integer): Cardinal;
begin
  Count := Count mod 16;
  Result := Value shr Count;
  if (Value and $8000) <> 0 then
    Result := BitMaskSetLowerRange16HPtr^[Count];
end;
{$ENDIF ~CPUI386}

{$IFDEF CPUI386}
function SAR32(Value: LongWord; Count: Integer): LongWord; register; assembler;
asm
        MOV     ECX, EDX
        SAR     EAX, CL
end;
{$ELSE CPUI386~}
function SAR32(Value: LongWord; Count: Integer): LongWord;
begin
  Count := Count mod 32;
  Result := Value shr Count;
  if (Value and $80000000) <> 0 then
    Result := BitMaskSetLowerRange32HPtr^[Count];
end;
{$ENDIF ~CPUI386}

(*
function SAR64(Value: UInt64; Count: Integer): UInt64;
begin
  { TODO : implementieren }
end;

procedure SARBuffer(var Data; DataSize: Integer; Count: Integer);
begin
  { TODO : implementieren }
end;
*)

// shift arithmetic left
function SAL8(Value: Cardinal; Count: Integer): Cardinal;
begin
  Result := Value shl Count;
end;

function SAL16(Value: Cardinal; Count: Integer): Cardinal;
begin
  Result := Value shl Count;
end;

function SAL32(Value: LongWord; Count: Integer): LongWord;
begin
  Result := Value shl Count;
end;

function SAL64(Value: UInt64; Count: Integer): UInt64;
begin
  Result := Value shl Count;
end;

(*
procedure SALBuffer(var Data; DataSize: Integer; Count: Integer);
begin
  SHLBuffer(Data, DataSize, Count);
end;
*)

// ***********************************  Bit-Stream Reader  ***********************************

// Stream-Daten übergeben
procedure TYclBitStreamCustomSingleBitReader.Init(Buffer: Pointer; Size: Cardinal);
begin
  FBuffer := Buffer;
  FSize := Size;
  FCurrentBytePtr := FBuffer;
  FPosition := 0;
end;

// Stream auf nächstes Bit setzen
// True, wenn nächstes Bit vorhanden; False, wenn Ende erreicht
function TYclBitStreamCustomSingleBitReader.NextBit: Boolean;
begin
  Result := FPosition < FSize;  // ist bereits am Ende des Streams
  if Result then begin
    Inc(FPosition);
    if (FPosition and $00000007) = 0 then   // nächstes Byte
      Inc(FCurrentBytePtr);
    Result := FPosition < FSize;   // ist er jetzt am Ende des Streams?
  end;
end;

function TYclBitStreamCustomSingleBitReader.ReadBit: Boolean;
begin
  Result := CurrentBit;
  NextBit;
end;

function TYclBitStreamCustomSingleBitReader.IsEndOfStream: Boolean;
begin
  Result := FPosition >= FSize;
end;

procedure TYclBitStreamCustomSingleBitReader.SetPosition(Value: Cardinal);
begin
  if Value > FSize then
    Value := FSize;
  FPosition := Value;
end;

procedure TYclBitStreamCustomSingleBitReader.SeekToNextByteBoundary;
begin
  if (FPosition and $00000007) <> 0 then begin   // Index im Bit > 0 -> nächstes Byte
    FPosition := (FPosition and not $00000007) + 8;
    Inc(FCurrentBytePtr);
  end;
end;

// *******************************************************************************************

// konkrete Implementierung der Bit-Order
function TYclBitStreamSingleBitReaderL.CurrentBit: Boolean;
begin
  Result := FPosition < FSize;  // wenn Ende erreicht, False zurückgeben
  if Result then
    Result := (FCurrentBytePtr^ and BitMaskSetBitN8LPtr^[FPosition and $00000007]) <> 0;
end;

function TYclBitStreamSingleBitReaderH.CurrentBit: Boolean;
begin
  Result := FPosition < FSize;  // wenn Ende erreicht, False zurückgeben
  if Result then
    Result := (FCurrentBytePtr^ and BitMaskSetBitN8HPtr^[FPosition and $00000007]) <> 0;
end;

// *******************************************************************************************

function YclBitOrderToSingleBitReaderClass(BitOrder: TYclBitOrder): TYclBitStreamSingleBitReaderClass;
const
  BitStreamClasses: array[Boolean] of TYclBitStreamSingleBitReaderClass = (
    TYclBitStreamSingleBitReaderL, TYclBitStreamSingleBitReaderH
  );
begin
  Result := BitStreamClasses[BitOrder = boMostSignificantBitFirst];
end;

// ***********************************  Bit-Stream Writer  ***********************************

constructor TYclBitStreamToByteCustomWriter.Create;
begin
  inherited Create;
  Clear;
end;

// ein Bit schreiben
procedure TYclBitStreamToByteCustomWriter.WriteBit(Data: Cardinal);
begin
  WriteBit(Data <> 0);
end;

// Count Bits schreiben, erstes Bit ist Bit 0
procedure TYclBitStreamToByteCustomWriter.WriteBits8L(Data: Byte; Count: Cardinal);
begin
  WriteBits32L(Data, Count);
end;

procedure TYclBitStreamToByteCustomWriter.WriteBits16L(Data: Word; Count: Cardinal);
begin
  WriteBits32L(Data, Count);
end;

// Count Bits schreiben, erstes Bit ist Bit 7, 15 oder 31
procedure TYclBitStreamToByteCustomWriter.WriteBits8H(Data: Byte; Count: Cardinal);
var
  Data32: LongWord;
begin
  Data32 := Data;
  Data32 := Data32 shl 24;      // Bit 7 -> Bit 31 (erstes Bit)
  WriteBits32H(Data32, Count);
end;

procedure TYclBitStreamToByteCustomWriter.WriteBits16H(Data: Word; Count: Cardinal);
var
  Data32: LongWord;
begin
  Data32 := Data;
  Data32 := Data32 shl 16;      // Bit 15 -> Bit 31 (erstes Bit)
  WriteBits32H(Data32, Count);
end;

// gibt aktuelles Byte aus, Bitposition danach wieder an Bytegrenze
procedure TYclBitStreamToByteCustomWriter.Flush;
begin
  if FBitIndex > 0 then begin
    if Assigned(FOnFlush) then
      FOnFlush(Self, FBuffer);
  end;
  Clear;
end;

procedure TYclBitStreamToByteCustomWriter.Clear;
begin
  FBuffer := 0;    // alle Bits = 0
  FBitIndex := 0;
end;

// *******************************************************************************************

// ein Bit schreiben
procedure TYclBitStreamToByteCustomWriterL.WriteBit(Data: Boolean);
begin
  if Data then  // nur 1 muß gesetzt werden
    FBuffer := FBuffer or BitMaskSetBitN8LPtr^[FBitIndex];
  Inc(FBitIndex);
  if FBitIndex > 7 then
    Flush;
end;

// Count Bits schreiben, erstes Bit ist Bit 0
procedure TYclBitStreamToByteCustomWriterL.WriteBits32L(Data: LongWord; Count: Cardinal);
var
  i: Cardinal;
begin
  { TODO : optimierte Version erstellen }
  for i := 1 to Count do begin
    WriteBit(Data and $01);
    Data := Data shr 1;
  end;
end;

// Count Bits schreiben, erstes Bit ist Bit 31
procedure TYclBitStreamToByteCustomWriterL.WriteBits32H(Data: LongWord; Count: Cardinal);
begin
  WriteBits32L(BitSwap32(Data), Count);
end;

// *******************************************************************************************

// ein Bit schreiben
procedure TYclBitStreamToByteCustomWriterH.WriteBit(Data: Boolean);
begin
  if Data then  // nur 1 muß gesetzt werden
    FBuffer := FBuffer or BitMaskSetBitN8HPtr^[FBitIndex];
  Inc(FBitIndex);
  if FBitIndex > 7 then
    Flush;
end;

// Count Bits schreiben, erstes Bit ist Bit 0
procedure TYclBitStreamToByteCustomWriterH.WriteBits32L(Data: LongWord; Count: Cardinal);
begin
  WriteBits32H(BitSwap32(Data), Count);
end;

// Count Bits schreiben, erstes Bit ist Bit 31
procedure TYclBitStreamToByteCustomWriterH.WriteBits32H(Data: LongWord; Count: Cardinal);
var
  i: Cardinal;
begin
  { TODO : optimierte Version erstellen }
  for i := 1 to Count do begin
    WriteBit(Data and $80000000);
    Data := Data shl 1;
  end;
end;

// *******************************************************************************************

function YclBitOrderToBitToByteWriterClass(BitOrder: TYclBitOrder): TYclBitStreamToByteWriterClass;
const
  BitStreamClasses: array[Boolean] of TYclBitStreamToByteWriterClass = (
    TYclBitStreamToByteCustomWriterL, TYclBitStreamToByteCustomWriterH
  );
begin
  Result := BitStreamClasses[BitOrder = boMostSignificantBitFirst];
end;

// *******************************************************************************************

constructor TYclBitStreamToByteStreamCustomWriter.Create(BitStreamOrder: TYclBitOrder);
begin
  inherited Create;
  // BitStream mit gewünschter BitStreamOrder erzeugen
  FBitStream := YclBitOrderToBitToByteWriterClass(BitStreamOrder).Create;
  FBitStream.FOnFlush := FlushEvent;
end;

destructor TYclBitStreamToByteStreamCustomWriter.Destroy;
begin
  FBitStream.Free;
  inherited Destroy;
end;

procedure TYclBitStreamToByteStreamCustomWriter.FlushEvent(Sender: TObject; Data: Byte);
begin
  Assert(Assigned(FDstStream));
  if Assigned(FDstStream) then
    FDstStream.WriteBuffer(Data, SizeOf(Data));
end;

// *******************************************************************************************

// setzt DstStream -> FDstStream, DstStream wird nicht von dieser Klasse verwaltet
constructor TYclBitStreamToByteStreamWriter.Create(BitStreamOrder: TYclBitOrder;
    DstStream: TStream);
begin
  inherited Create(BitStreamOrder);
  FDstStream := DstStream;
end;

// *******************************************************************************************

// Bit-Stream-Klasse, welche die geschriebenen Bytes in einen selbstverwalteten Puffer ausgibt

constructor TYclBitStreamToMemoryWriter.Create(BitStreamOrder: TYclBitOrder);
begin
  inherited Create(BitStreamOrder);
  FDstStream := TMemoryStream.Create;
end;

destructor TYclBitStreamToMemoryWriter.Destroy;
begin
  FDstStream.Free;
  inherited Destroy;
end;

// setzt MemoryStream auf Beginn, Inhalt und Größe bleiben erhalten
procedure TYclBitStreamToMemoryWriter.Reset;
begin
  FDstStream.Position := 0;
end;

// schreibt DataSize Daten vom Anfang des MemoryStreams in einen Stream
procedure TYclBitStreamToMemoryWriter.WriteToStream(Stream: TStream);
begin
  if DataSize > 0 then
    Stream.WriteBuffer(MemoryStream.Memory^, DataSize);
end;

function TYclBitStreamToMemoryWriter.GetMemoryStream: TMemoryStream;
begin
  Assert(FDstStream is TMemoryStream);
  Result := TMemoryStream(FDstStream);
end;

function TYclBitStreamToMemoryWriter.GetDataSize: Cardinal;
begin
  Result := Cardinal(MemoryStream.Position);
end;

function TYclBitStreamToMemoryWriter.GetDataCapacity: Cardinal;
begin
  Result := Cardinal(MemoryStream.Size);
end;

procedure TYclBitStreamToMemoryWriter.SetDataCapacity(Value: Cardinal);
begin
  MemoryStream.Size := Value;
end;

// *******************************************************************************************

initialization
  Assert((Ord(False) = 0) and (Ord(True) = 1));
  
// *******************************************************************************************

//  History:
//  2005-05-17, Peter J. Haas
//   - Bugfix: BitMaskSetBitN8HPtr, BitMaskSetBitN16HPtr, BitMaskSetBitN32HPtr

end.
