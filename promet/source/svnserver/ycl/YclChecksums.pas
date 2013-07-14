{**************************************************************************************************}
{                                                                                                  }
{  Y core library (Ycl)                                                                            }
{                                                                                                  }
{  Checksum calculations                                                                           }
{                                                                                                  }
{  The contents of this file are subject to the Y Library Public License Version 1.0 (the          }
{  "License"); you may not use this file except in compliance with the License. You may obtain a   }
{  copy of the License at http://delphi.pjh2.de/                                                   }
{                                                                                                  }
{  Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF  }
{  ANY KIND, either express or implied. See the License for the specific language governing        }
{  rights and limitations under the License.                                                       }
{                                                                                                  }
{  The Original Code is: YclChecksums.pas.                                                         }
{  The Initial Developer of the Original Code is Peter J. Haas (libs@pjh2.de). Portions created    }
{  by Peter J. Haas are Copyright (C) 2001-2005 Peter J. Haas. All Rights Reserved.                }
{                                                                                                  }
{  Contributor(s):                                                                                 }
{                                                                                                  }
{  You may retrieve the latest version of this file at the homepage of Peter J. Haas, located at   }
{  http://delphi.pjh2.de/                                                                          }
{                                                                                                  }
{**************************************************************************************************}

// For history see end of file

{$I Ycl.inc}

unit YclChecksums;

interface
uses
  SysUtils, YclBase;

function Sum8(
    const Data; DataCount: Cardinal;
    const InitValue: Byte{$IFDEF SUPPORTS_DEFAULTPARAMS} = 0{$ENDIF}
  ): Byte;
function Sum16(
    const Data; DataCount: Cardinal;
    const InitValue: Word{$IFDEF SUPPORTS_DEFAULTPARAMS} = 0{$ENDIF}
  ): Word;
function Sum24(
    const Data; DataCount: Cardinal;
const InitValue: LongWord{$IFDEF SUPPORTS_DEFAULTPARAMS} = 0{$ENDIF}
  ): LongWord;
function Sum32(
    const Data; DataCount: Cardinal;
    const InitValue: LongWord{$IFDEF SUPPORTS_DEFAULTPARAMS} = 0{$ENDIF}
  ): LongWord;

function SumSystemV(
    const Data; DataCount: Cardinal;
    const InitValue: Word{$IFDEF SUPPORTS_DEFAULTPARAMS} = 0{$ENDIF}
  ): Word;

function SumBSD(
    const Data; DataCount: Cardinal;
    const InitValue: Word{$IFDEF SUPPORTS_DEFAULTPARAMS} = 0{$ENDIF}
  ): Word;

function Xor8(
    const Data; DataCount: Cardinal;
    const InitValue: Byte{$IFDEF SUPPORTS_DEFAULTPARAMS} = 0{$ENDIF}
  ): Byte;

// RFC 1950
function Adler32(
    const Data; DataCount: Cardinal;
    const InitValue: LongWord{$IFDEF SUPPORTS_DEFAULTPARAMS} = 1{$ENDIF}
  ): LongWord;

// ********************************  Cyclic Redundency Check  ********************************

type
  TCRCParameters = record
    Width: Integer;
    Polynom: LongWord;
    InitValue: LongWord;
    ReflectionIn: Boolean;
    ReflectionOut: Boolean;
    XorOut: LongWord;
  end;

  TCRCParameters64 = record
    Width: Integer;
    Polynom: UInt64;
    InitValue: UInt64;
    ReflectionIn: Boolean;
    ReflectionOut: Boolean;
    XorOut: UInt64;
  end;

function CRCCalc(
    const Parameters: TCRCParameters;    // describe the CRC parameters
    const Data;                          // data
    DataCount: Cardinal                  // length of Data in bytes
  ): LongWord;                           // return the checksum
function CRCCalc64(
    const Parameters: TCRCParameters64;  // describe the CRC parameters
    const Data;                          // data
    DataCount: Cardinal                  // length of Data in bytes
  ): UInt64;                             // return the checksum

function CRCCalcPtr(
    const Parameters: TCRCParameters;    // describe the CRC parameters
    DataPtr: Pointer;                    // Pointer to the byte of the block, that will be
                                         // calculate at first
    DataCount: Integer                   // length of Data in bytes
                                         // 0: no calculation,
                                         // negative values: DataPtr is the last byte of the data
                                         // block, the bytes will be calculated from end to start
  ): LongWord;                           // return the checksum
function CRCCalcPtr64(
    const Parameters: TCRCParameters64;  // describe the CRC parameters
    DataPtr: Pointer;                    // Pointer to the byte of the block, that will be
                                         // calculate at first
    DataCount: Integer                   // length of Data in bytes
                                         // 0: no calculation,
                                         // negative values: DataPtr is the last byte of the data
                                         // block, the bytes will be calculated from end to start
  ): UInt64;                             // return the checksum

// ********************************  Handle based functions  *********************************
type
  TCRCHandle = Pointer;
  
// Init of calculation
function CRCInit(
    const Parameters: TCRCParameters    // describe the CRC parameters
  ): TCRCHandle;                        // return a handle to the CRC object
function CRCInit64(
    const Parameters: TCRCParameters64  // describe the CRC parameters
  ): TCRCHandle;                        // return a handle to the CRC object

// Add 1 byte to Checksum
function CRCAddByte(
    Handle: TCRCHandle;    // handle to the CRC object
    Data: Byte             // byte 
  ): Boolean;              // success of calculation, False means Handle is not a valid CRC object

// Add a block to Checksum
function CRCAddDataBlock(
    Handle: TCRCHandle;    // handle to the CRC object
    var DataPtr: Pointer;  // in: Pointer to the byte of the block, that will be calculate at first
                           // out: Pointer to the next byte (first uncalculated byte)
    DataCount: Integer     // length of Data in bytes
                           // 0: no calculation,
                           // negative values: DataPtr is the last byte of the data block,
                           // the bytes will be calculated from end to start
  ): Boolean;              // success of calculation, False means Handle is not a valid CRC object

// Close the calculation and release the CRC object
function CRCClose(
    Handle: TCRCHandle;    // handle to the CRC object
    out Value: LongWord    // return the checksum
  ): Boolean;              // success of calculation, False means Handle is not a valid CRC object
function CRCClose64(
    Handle: TCRCHandle;    // handle to the CRC object
    out Value: UInt64      // return the checksum
  ): Boolean;              // success of calculation, False means Handle is not a valid CRC object

// **************************************  CRC classes  **************************************

type
  TCustomCRC = class(TObject)
  public
    // Add 1 byte to checksum
    procedure AddByte(
        Data: Byte                  // byte
      ); virtual; abstract;
    // Add a block to checksum
    procedure AddDataBlock(
        var DataPtr: Pointer;       // in: Pointer to the byte of the block, that will be calculate
                                    //     at first
                                    // out: Pointer to the next byte (first uncalculated byte)
        DataCount: Integer          // length of Data in bytes
                                    // 0: no calculation,
                                    // negative values: DataPtr is the last byte of the data block,
                                    // the bytes will be calculated from end to start
      );
  end;

  TCRC = class(TCustomCRC)
  private
    FParameters: TCRCParameters;    // CRC parameters
    FWidthMask: LongWord;           // mask for all valid bits
    FTopBit: LongWord;              // mask for top bit
    FCRCValue: LongWord;            // current checksum without finish
  public
    // Init of calculation
    constructor Create(
        const Parameters: TCRCParameters  // describe the CRC parameters
      );
    // Add 1 byte to checksum
    procedure AddByte(
        Data: Byte                  // byte
      ); override;
    // Close the calculation and reset the CRC object
    function Finish: LongWord;      // return the checksum
  end;

  TCRC64 = class(TCustomCRC)
  private
    FParameters: TCRCParameters64;  // CRC parameters
    FWidthMask: UInt64;             // mask for all valid bits
    FTopBit: UInt64;                // mask for top bit
    FCRCValue: UInt64;              // current checksum without finish
  public
    // Init of calculation
    constructor Create(
        const Parameters: TCRCParameters64  // describe the CRC parameters
      );
    // Add 1 byte to checksum
    procedure AddByte(
        Data: Byte                  // byte
      ); override;
    // Close the calculation and reset the CRC object
    function Finish: UInt64;        // return the checksum
  end;

// *************************************  Build-in CRCs  *************************************

// Standard CRC8
// x^8 + x^7 + x^6 + x^5   (?)
// Polynom:        E0
// Init value:     00
// Reflection in:  False
// Reflection out: False
// XOR Output:     00
function CRC8(
    const Data; DataCount: Cardinal;
    const InitValue: Byte{$IFDEF SUPPORTS_DEFAULTPARAMS} = 0{$ENDIF}
  ): Byte;

// Reversed CRC8
// eg. ATM HEC
// x^8 + x^7 + x^6 + x^5   (?)
// Polynom:        07
// Init value:     00
// Reflection in:  True
// Reflection out: True
// XOR Output:     00
function CRC8Reversed(
    const Data; DataCount: Cardinal;
    const InitValue: Byte{$IFDEF SUPPORTS_DEFAULTPARAMS} = 0{$ENDIF}
  ): Byte;

// Standard CRC16
// x^16 + x^15 + x^2 + x^0
// Polynom:        8005         1-1000-0000-0000-0101
// Init value:     0000
// Reflection in:  False
// Reflection out: False
// XOR Output:     0000
// Check:          BB3D
function CRC16(
    const Data; DataCount: Cardinal;
    const InitValue: Word{$IFDEF SUPPORTS_DEFAULTPARAMS} = 0{$ENDIF}
  ): Word;

// Reversed CRC16
// x^16 + x^15 + x^2 + x^0
// Polynom:        A001         1010-0000-0000-0001-1
// Init value:     0000
// Reflection in:  True
// Reflection out: True
// XOR Output:     0000
function CRC16Reversed(
    const Data; DataCount: Cardinal;
    const InitValue: Word{$IFDEF SUPPORTS_DEFAULTPARAMS} = 0{$ENDIF}
  ): Word;

// Standard CRC16 CCITT
// x^16 + x^12 + x^5 + x^0
// e.g. X.25, SDLC, and HDLC
// Polynom:        1021         1-0001-0000-0010-0001
// Init value:     FFFF
// Reflection in:  False
// Reflection out: False
// XOR Output:     0000
function CRC16CCITT(
    const Data; DataCount: Cardinal;
    const InitValue: Word{$IFDEF SUPPORTS_DEFAULTPARAMS} = $FFFF{$ENDIF}
  ): Word;

// Reversed CRC16 CCITT
// e.g. XMODEM and Kermit
// x^16 + x^12 + x^5 + x^0
// Polynom:        8408         1000-0100-0000-1000-1
// Init value:     0000
// Reflection in:  True
// Reflection out: True
// XOR Output:     0000
function CRC16CCITTReversed(
    const Data; DataCount: Cardinal;
    const InitValue: Word{$IFDEF SUPPORTS_DEFAULTPARAMS} = 0{$ENDIF}
  ): Word;

// Variation of CRC16
// e.g. ARC
// x^16 + x^15 + x^13 + x^0   (?)
// Polynom:        8005
// Init value:     0000
// Reflection in:  True
// Reflection out: True
// XOR Output:     0000
function CRC16ARC(
    const Data; DataCount: Cardinal;
    const InitValue: Word{$IFDEF SUPPORTS_DEFAULTPARAMS} = 0{$ENDIF}
  ): Word;

// Variation of CRC16
// e.g. ZMODEM
// x^16 + x^12 + x^5 + x^0
// Polynom:        1021
// Init value:     0000
// Reflection in:  False
// Reflection out: False
// XOR Output:     0000
function CRC16ZMODEM(
    const Data; DataCount: Cardinal;
    const InitValue: Word{$IFDEF SUPPORTS_DEFAULTPARAMS} = 0{$ENDIF}
  ): Word;

// Standard CRC24
// e.g. PGP
// x^24 + x^23 + x^18 + x^17 + x^14 + x^11 + x^10 + x^7 + x^6 + x^5 + x^4 + x^3 + x^1 + x^0  (?)
// Polynom:        1864CFB
// Init value:     B704CE
// Reflection in:  False
// Reflection out: False
// XOR Output:     000000
function CRC24(
    const Data; DataCount: Cardinal;
    const InitValue: LongWord{$IFDEF SUPPORTS_DEFAULTPARAMS} = $B704CE{$ENDIF}
  ): LongWord;

// Standard CRC32
// e.g. AUTODIN II, Ethernet, and FDDI
// x^32 + x^26 + x^23 + x^22 + x^16 + x^12 + x^11 + x^10 + x^8 + x^7 + x^5 + x^4 + x^2 + x^1 + x^0
// Polynom:        EDB88320
// Init value:     FFFFFFFF
// Reflection in:  False
// Reflection out: False
// XOR Output:     FFFFFFFF
function CRC32(
    const Data; DataCount: Cardinal;
    const InitValue: LongWord{$IFDEF SUPPORTS_DEFAULTPARAMS} = 0{$ENDIF}
  ): LongWord;

// Reversed CRC32
// e.g. PKZip, zlib and SFV
// x^32 + x^26 + x^23 + x^22 + x^16 + x^12 + x^11 + x^10 + x^8 + x^7 + x^5 + x^4 + x^2 + x^1 + x^0
// Polynom:        04C11DB7
// Init value:     FFFFFFFF
// Reflection in:  True
// Reflection out: True
// XOR Output:     FFFFFFFF
// Check:          CBF43926
function CRC32Reversed(
    const Data; DataCount: Cardinal;
    const InitValue: LongWord{$IFDEF SUPPORTS_DEFAULTPARAMS} = 0{$ENDIF}
  ): LongWord;

// Variation of CRC32
// e.g. JAMCRC
// Polynom:        04C11DB7
// Init value:     FFFFFFFF
// Reflection in:  True
// Reflection out: True
// XOR Output:     00000000
function CRC32JAMCRC(
    const Data; DataCount: Cardinal;
    const InitValue: LongWord{$IFDEF SUPPORTS_DEFAULTPARAMS} = $FFFFFFFF{$ENDIF}
  ): LongWord;

// Variation of CRC32
// e.g. BZip2
// Polynom:        04C11DB7
// Init value:     FFFFFFFF
// Reflection in:  False
// Reflection out: False
// XOR Output:     FFFFFFFF
function CRC32BZIP2(
    const Data; DataCount: Cardinal;
    const InitValue: LongWord{$IFDEF SUPPORTS_DEFAULTPARAMS} = 0{$ENDIF}
  ): LongWord;

implementation
uses
  YclBitArithmetic;

function Sum8(
    const Data; DataCount: Cardinal;
    const InitValue: Byte{$IFDEF SUPPORTS_DEFAULTPARAMS} = 0{$ENDIF}
  ): Byte;
begin
  Result := Byte(Sum32(Data, DataCount, InitValue));
end;

function Sum16(
    const Data; DataCount: Cardinal;
    const InitValue: Word{$IFDEF SUPPORTS_DEFAULTPARAMS} = 0{$ENDIF}
  ): Word;
begin
  Result := Word(Sum32(Data, DataCount, InitValue));
end;

function Sum24(
    const Data; DataCount: Cardinal;
    const InitValue: LongWord{$IFDEF SUPPORTS_DEFAULTPARAMS} = 0{$ENDIF}
  ): LongWord;
begin
  Result := Sum32(Data, DataCount, InitValue) and $00FFFFFF;
end;

function Sum32(
    const Data; DataCount: Cardinal;
    const InitValue: LongWord{$IFDEF SUPPORTS_DEFAULTPARAMS} = 0{$ENDIF}
  ): LongWord;
var
  Ptr: PByte;
  i: Cardinal;
begin
  Ptr := @Data;
  if not Assigned(Ptr) then
    DataCount := 0;
  Result := InitValue;
  for i := 1 to DataCount do begin
    Result := Result + Ptr^;
    Inc(Ptr);
  end;
end;

function SumSystemV(
    const Data; DataCount: Cardinal;
    const InitValue: Word{$IFDEF SUPPORTS_DEFAULTPARAMS} = 0{$ENDIF}
  ): Word;
var
  R: LongWord;
begin
  { TODO : Wird die Berechnung in 512-Byte - Blöcke unterteilt? }
  R := Sum32(Data, DataCount, InitValue);
  R := (R and $FFFF) + (R shr 16);
  Result := (R and $FFFF) + (R shr 16);
end;

function SumBSD(
    const Data; DataCount: Cardinal;
    const InitValue: Word{$IFDEF SUPPORTS_DEFAULTPARAMS} = 0{$ENDIF}
  ): Word;
var
  Ptr: PByte;
  i: Cardinal;
begin
  Ptr := @Data;
  if not Assigned(Ptr) then
    DataCount := 0;
  Result := InitValue;
  for i := 1 to DataCount do begin
    // 16 bit Wert um 1 bit nach rechts rotieren
    if Result and $01 = 0 then
      Result := Result shr 1
    else
      Result := (Result shr 1) or $8000;
    Result := Result + Ptr^;
    Inc(Ptr);
  end;
end;

function Xor8(
    const Data; DataCount: Cardinal;
    const InitValue: Byte{$IFDEF SUPPORTS_DEFAULTPARAMS} = 0{$ENDIF}
  ): Byte;
var
  Ptr: PByte;
  i: Cardinal;
begin
  Ptr := @Data;
  if not Assigned(Ptr) then
    DataCount := 0;
  Result := InitValue;
  for i := 1 to DataCount do begin
    Result := Result xor Ptr^;
    Inc(Ptr);
  end;
end;

// RFC 1950
function Adler32(
    const Data; DataCount: Cardinal;
    const InitValue: LongWord{$IFDEF SUPPORTS_DEFAULTPARAMS} = 1{$ENDIF}
  ): LongWord;
const
  Base = 65521;  // largest prime smaller than 65536
var
  Ptr: PByte;
  i: Cardinal;
  s1, s2: LongWord;
begin
  Ptr := @Data;
  if not Assigned(Ptr) then
    DataCount := 0;
  s1 := InitValue and $FFFF;
  s2 := InitValue shr 16;
  for i := 1 to DataCount do begin
    s1 := (s1 + Ptr^) mod Base;
    s2 := (s2 + s1) mod Base;
    Inc(Ptr);
  end;
  Result := (s2 shl 16) or s1;
end;

// ********************************  Cyclic Redundency Check  ********************************

function CRCCalc(
    const Parameters: TCRCParameters;    // describe the CRC parameters
    const Data;                          // data
    DataCount: Cardinal                  // length of Data in bytes
  ): LongWord;                           // return the checksum
var
  CRC: TCRC;
  DataPtr: Pointer;
begin
  CRC := TCRC.Create(Parameters);
  try
    DataPtr := @Data;
    if Assigned(DataPtr) then
      CRC.AddDataBlock(DataPtr, DataCount);
    Result := CRC.Finish;
  finally
    CRC.Free;
  end;
end;

function CRCCalc64(
    const Parameters: TCRCParameters64;  // describe the CRC parameters
    const Data;                          // data
    DataCount: Cardinal                  // length of Data in bytes
  ): UInt64;                             // return the checksum
var
  CRC: TCRC64;
  DataPtr: Pointer;
begin
  CRC := TCRC64.Create(Parameters);
  try
    DataPtr := @Data;
    if Assigned(DataPtr) then
      CRC.AddDataBlock(DataPtr, DataCount);
    Result := CRC.Finish;
  finally
    CRC.Free;
  end;
end;

function CRCCalcPtr(
    const Parameters: TCRCParameters;    // describe the CRC parameters
    DataPtr: Pointer;                    // Pointer to the byte of the block, that will be
                                         // calculate at first
    DataCount: Integer                   // length of Data in bytes
                                         // 0: no calculation,
                                         // negative values: DataPtr is the last byte of the data
                                         // block, the bytes will be calculated from end to start
  ): LongWord;                           // return the checksum
var
  CRC: TCRC;
begin
  CRC := TCRC.Create(Parameters);
  try
    if Assigned(DataPtr) then
      CRC.AddDataBlock(DataPtr, DataCount);
    Result := CRC.Finish;
  finally
    CRC.Free;
  end;
end;

function CRCCalcPtr64(
    const Parameters: TCRCParameters64;  // describe the CRC parameters
    DataPtr: Pointer;                    // Pointer to the byte of the block, that will be
                                         // calculate at first
    DataCount: Integer                   // length of Data in bytes
                                         // 0: no calculation,
                                         // negative values: DataPtr is the last byte of the data
                                         // block, the bytes will be calculated from end to start
  ): UInt64;                             // return the checksum
var
  CRC: TCRC64;
begin
  CRC := TCRC64.Create(Parameters);
  try
    if Assigned(DataPtr) then
      CRC.AddDataBlock(DataPtr, DataCount);
    Result := CRC.Finish;
  finally
    CRC.Free;
  end;
end;

// ********************************  Handle based functions  *********************************

// Init of calculation
function CRCInit(
    const Parameters: TCRCParameters    // describe the CRC parameters
  ): TCRCHandle;                        // return a handle to the CRC object
begin
  Result := TCRC.Create(Parameters);
end;

function CRCInit64(
    const Parameters: TCRCParameters64  // describe the CRC parameters
  ): TCRCHandle;                        // return a handle to the CRC object
begin
  Result := TCRC64.Create(Parameters);
end;

// Add 1 byte to Checksum
function CRCAddByte(
    Handle: TCRCHandle;    // handle to the CRC object
    Data: Byte             // byte 
  ): Boolean;              // success of calculation, False means Handle is not a valid CRC object
begin
  Result := TObject(Handle) is TCustomCRC;
  if Result then
    TCustomCRC(Handle).AddByte(Data);
end;

// Add a block to Checksum
function CRCAddDataBlock(
    Handle: TCRCHandle;    // handle to the CRC object
    var DataPtr: Pointer;  // in: Pointer to the byte of the block, that will be calculate at first
                           // out: Pointer to the next byte (first uncalculated byte)
    DataCount: Integer     // length of Data in bytes
                           // 0: no calculation,
                           // negative values: DataPtr is the last byte of the data block,
                           // the bytes will be calculated from end to start
  ): Boolean;              // success of calculation, False means Handle is not a valid CRC object
begin
  Result := TObject(Handle) is TCustomCRC;
  if Result then
    TCustomCRC(Handle).AddDataBlock(DataPtr, DataCount);
end;

// Close the calculation and release the CRC object
function CRCClose(
    Handle: TCRCHandle;    // handle to the CRC object
    out Value: LongWord    // return the checksum
  ): Boolean;              // success of calculation, False means Handle is not a valid CRC object
begin
  Result := TObject(Handle) is TCRC;
  if Result then begin
    Value := TCRC(Handle).Finish;
    TCRC(Handle).Free;
  end;
end;

function CRCClose64(
    Handle: TCRCHandle;    // handle to the CRC object
    out Value: UInt64      // return the checksum
  ): Boolean;              // success of calculation, False means Handle is not a valid CRC object
begin
  Result := TObject(Handle) is TCRC64;
  if Result then
  if Result then begin
    Value := TCRC64(Handle).Finish;
    TCRC64(Handle).Free;
  end;
end;

// **************************************  CRC classes  **************************************

// Add a block to checksum
procedure TCustomCRC.AddDataBlock(
    var DataPtr: Pointer;       // in: Pointer to the byte of the block, that will be calculate
                                //     at first
                                // out: Pointer to the next byte (first uncalculated byte)
    DataCount: Integer          // length of Data in bytes
                                // 0: no calculation,
                                // negative values: DataPtr is the last byte of the data block,
                                // the bytes will be calculated from end to start
  );
var
  i: Cardinal;
  Delta: Integer;
begin
  if not Assigned(DataPtr) then
    DataCount := 0;
  if DataCount >= 0 then
    Delta := 1
  else
    Delta := -1;
  for i := 1 to Abs(DataCount) do begin
    AddByte(PByte(DataPtr)^);
    Inc(PByte(DataPtr), Delta);
  end;
end;

// Init of calculation
constructor TCRC.Create(
    const Parameters: TCRCParameters  // describe the CRC parameters
  );
var
  V: LongInt;
begin
  inherited Create;
  FParameters := Parameters;
  V := 1;
  V := V shl (FParameters.Width - 1);
  FTopBit := V;
  V := V shl 1;
  FWidthMask := ((V and $FFFFFFFE) - 1) or 1;
  FCRCValue := FParameters.InitValue;
end;

// Add 1 byte to checksum
procedure TCRC.AddByte(
    Data: Byte                  // byte
  );
var
  V, CRCValue: LongWord;
  i: Integer;
begin
  if FParameters.ReflectionIn then
    V := BitSwap8(Data)
  else
    V := Data;
  CRCValue := FCRCValue;
  CRCValue := CRCValue xor (V shl (FParameters.Width - 8));
  for i := 0 to 7 do begin
    if (CRCValue and FTopBit) <> 0 then
      CRCValue := (CRCValue shl 1) xor FParameters.Polynom
    else
      CRCValue := CRCValue shl 1;
    CRCValue := CRCValue and FWidthMask;
  end;
  FCRCValue := CRCValue;
end;

// Close the calculation and reset the CRC object
function TCRC.Finish: LongWord;      // return the checksum
begin
  if FParameters.ReflectionOut then
    Result := FParameters.XorOut xor BitSwapN32(FCRCValue, FParameters.Width)
  else
    Result := FParameters.XorOut xor FCRCValue;
end;

// Init of calculation
constructor TCRC64.Create(
    const Parameters: TCRCParameters64  // describe the CRC parameters
  );
var
  V: UInt64;
begin
  inherited Create;
  FParameters := Parameters;
  V := 1;
  V := V shl (FParameters.Width - 1);
  FTopBit := V;
  V := V shl 1;
  FWidthMask := ((V and $FFFFFFFFFFFFFFFE) - 1) or 1;
  FCRCValue := FParameters.InitValue;
end;

// Add 1 byte to checksum
procedure TCRC64.AddByte(
    Data: Byte                  // byte
  );
var
  V, CRCValue: UInt64;
  i: Integer;
begin
  if FParameters.ReflectionIn then
    V := BitSwap8(Data)
  else
    V := Data;
  CRCValue := FCRCValue;
  CRCValue := CRCValue xor (V shl (FParameters.Width - 8));
  for i := 0 to 7 do begin
    if (CRCValue and FTopBit) <> 0 then
      CRCValue := (CRCValue shl 1) xor FParameters.Polynom
    else
      CRCValue := CRCValue shl 1;
    CRCValue := CRCValue and FWidthMask;
  end;
  FCRCValue := CRCValue;
end;

// Close the calculation and reset the CRC object
function TCRC64.Finish: UInt64;        // return the checksum
begin
  if FParameters.ReflectionOut then
    Result := FParameters.XorOut xor BitSwapN64(FCRCValue, FParameters.Width)
  else
    Result := FParameters.XorOut xor FCRCValue;
end;

// *************************************  Build-in CRCs  *************************************

type
  TCRC8LookupTable = array[Byte] of Byte;
  TCRC16LookupTable = array[Byte] of Word;
  TCRC32LookupTable = array[Byte] of LongWord;
//  TCRC64LookupTable = array[Byte] of UInt64;  // currently not used

const
  CRCLookupTableCRC8: TCRC8LookupTable = (
    $00, $E0, $20, $C0, $40, $A0, $60, $80, $80, $60, $A0, $40, $C0, $20, $E0, $00,
    $E0, $00, $C0, $20, $A0, $40, $80, $60, $60, $80, $40, $A0, $20, $C0, $00, $E0,
    $20, $C0, $00, $E0, $60, $80, $40, $A0, $A0, $40, $80, $60, $E0, $00, $C0, $20,
    $C0, $20, $E0, $00, $80, $60, $A0, $40, $40, $A0, $60, $80, $00, $E0, $20, $C0,
    $40, $A0, $60, $80, $00, $E0, $20, $C0, $C0, $20, $E0, $00, $80, $60, $A0, $40,
    $A0, $40, $80, $60, $E0, $00, $C0, $20, $20, $C0, $00, $E0, $60, $80, $40, $A0,
    $60, $80, $40, $A0, $20, $C0, $00, $E0, $E0, $00, $C0, $20, $A0, $40, $80, $60,
    $80, $60, $A0, $40, $C0, $20, $E0, $00, $00, $E0, $20, $C0, $40, $A0, $60, $80,
    $80, $60, $A0, $40, $C0, $20, $E0, $00, $00, $E0, $20, $C0, $40, $A0, $60, $80,
    $60, $80, $40, $A0, $20, $C0, $00, $E0, $E0, $00, $C0, $20, $A0, $40, $80, $60,
    $A0, $40, $80, $60, $E0, $00, $C0, $20, $20, $C0, $00, $E0, $60, $80, $40, $A0,
    $40, $A0, $60, $80, $00, $E0, $20, $C0, $C0, $20, $E0, $00, $80, $60, $A0, $40,
    $C0, $20, $E0, $00, $80, $60, $A0, $40, $40, $A0, $60, $80, $00, $E0, $20, $C0,
    $20, $C0, $00, $E0, $60, $80, $40, $A0, $A0, $40, $80, $60, $E0, $00, $C0, $20,
    $E0, $00, $C0, $20, $A0, $40, $80, $60, $60, $80, $40, $A0, $20, $C0, $00, $E0,
    $00, $E0, $20, $C0, $40, $A0, $60, $80, $80, $60, $A0, $40, $C0, $20, $E0, $00
  );

// Standard CRC8
// Polynom:        E0
// Init value:     00
// Reflection in:  False
// Reflection out: False
// XOR Output:     00
function CRC8(
    const Data; DataCount: Cardinal;
    const InitValue: Byte{$IFDEF SUPPORTS_DEFAULTPARAMS} = 0{$ENDIF}
  ): Byte;
var
  Ptr: PByte;
  i: Cardinal;
begin
  Ptr := @Data;
  if not Assigned(Ptr) then
    DataCount := 0;
  // Initialisierung
  Result := InitValue;
  for i := 0 to DataCount - 1 do begin
    Result := CRCLookupTableCRC8[Result xor Ptr^];
    Inc(Ptr);
  end;
end;

const
  CRCLookupTableCRC8Reversed: TCRC8LookupTable = (
    $00, $91, $E3, $72, $07, $96, $E4, $75, $0E, $9F, $ED, $7C, $09, $98, $EA, $7B,
    $1C, $8D, $FF, $6E, $1B, $8A, $F8, $69, $12, $83, $F1, $60, $15, $84, $F6, $67,
    $38, $A9, $DB, $4A, $3F, $AE, $DC, $4D, $36, $A7, $D5, $44, $31, $A0, $D2, $43,
    $24, $B5, $C7, $56, $23, $B2, $C0, $51, $2A, $BB, $C9, $58, $2D, $BC, $CE, $5F,
    $70, $E1, $93, $02, $77, $E6, $94, $05, $7E, $EF, $9D, $0C, $79, $E8, $9A, $0B,
    $6C, $FD, $8F, $1E, $6B, $FA, $88, $19, $62, $F3, $81, $10, $65, $F4, $86, $17,
    $48, $D9, $AB, $3A, $4F, $DE, $AC, $3D, $46, $D7, $A5, $34, $41, $D0, $A2, $33,
    $54, $C5, $B7, $26, $53, $C2, $B0, $21, $5A, $CB, $B9, $28, $5D, $CC, $BE, $2F,
    $E0, $71, $03, $92, $E7, $76, $04, $95, $EE, $7F, $0D, $9C, $E9, $78, $0A, $9B,
    $FC, $6D, $1F, $8E, $FB, $6A, $18, $89, $F2, $63, $11, $80, $F5, $64, $16, $87,
    $D8, $49, $3B, $AA, $DF, $4E, $3C, $AD, $D6, $47, $35, $A4, $D1, $40, $32, $A3,
    $C4, $55, $27, $B6, $C3, $52, $20, $B1, $CA, $5B, $29, $B8, $CD, $5C, $2E, $BF,
    $90, $01, $73, $E2, $97, $06, $74, $E5, $9E, $0F, $7D, $EC, $99, $08, $7A, $EB,
    $8C, $1D, $6F, $FE, $8B, $1A, $68, $F9, $82, $13, $61, $F0, $85, $14, $66, $F7,
    $A8, $39, $4B, $DA, $AF, $3E, $4C, $DD, $A6, $37, $45, $D4, $A1, $30, $42, $D3,
    $B4, $25, $57, $C6, $B3, $22, $50, $C1, $BA, $2B, $59, $C8, $BD, $2C, $5E, $CF
  );

// Reversed CRC8
// eg. ATM HEC
// Polynom:        07
// Init value:     00
// Reflection in:  True
// Reflection out: True
// XOR Output:     00
function CRC8Reversed(
    const Data; DataCount: Cardinal;
    const InitValue: Byte{$IFDEF SUPPORTS_DEFAULTPARAMS} = 0{$ENDIF}
  ): Byte;
var
  Ptr: PByte;
  i: Cardinal;
begin
  Ptr := @Data;
  if not Assigned(Ptr) then
    DataCount := 0;
  // Initialisierung
  Result := InitValue;
  for i := 0 to DataCount - 1 do begin
    Result := CRCLookupTableCRC8Reversed[Result xor Ptr^];
    Inc(Ptr);
  end;
end;

const
  CRCLookupTableCRC16: TCRC16LookupTable = (
    $0000, $8005, $800F, $000A, $801B, $001E, $0014, $8011,
    $8033, $0036, $003C, $8039, $0028, $802D, $8027, $0022,
    $8063, $0066, $006C, $8069, $0078, $807D, $8077, $0072,
    $0050, $8055, $805F, $005A, $804B, $004E, $0044, $8041,
    $80C3, $00C6, $00CC, $80C9, $00D8, $80DD, $80D7, $00D2,
    $00F0, $80F5, $80FF, $00FA, $80EB, $00EE, $00E4, $80E1,
    $00A0, $80A5, $80AF, $00AA, $80BB, $00BE, $00B4, $80B1,
    $8093, $0096, $009C, $8099, $0088, $808D, $8087, $0082,
    $8183, $0186, $018C, $8189, $0198, $819D, $8197, $0192,
    $01B0, $81B5, $81BF, $01BA, $81AB, $01AE, $01A4, $81A1,
    $01E0, $81E5, $81EF, $01EA, $81FB, $01FE, $01F4, $81F1,
    $81D3, $01D6, $01DC, $81D9, $01C8, $81CD, $81C7, $01C2,
    $0140, $8145, $814F, $014A, $815B, $015E, $0154, $8151,
    $8173, $0176, $017C, $8179, $0168, $816D, $8167, $0162,
    $8123, $0126, $012C, $8129, $0138, $813D, $8137, $0132,
    $0110, $8115, $811F, $011A, $810B, $010E, $0104, $8101,
    $8303, $0306, $030C, $8309, $0318, $831D, $8317, $0312,
    $0330, $8335, $833F, $033A, $832B, $032E, $0324, $8321,
    $0360, $8365, $836F, $036A, $837B, $037E, $0374, $8371,
    $8353, $0356, $035C, $8359, $0348, $834D, $8347, $0342,
    $03C0, $83C5, $83CF, $03CA, $83DB, $03DE, $03D4, $83D1,
    $83F3, $03F6, $03FC, $83F9, $03E8, $83ED, $83E7, $03E2,
    $83A3, $03A6, $03AC, $83A9, $03B8, $83BD, $83B7, $03B2,
    $0390, $8395, $839F, $039A, $838B, $038E, $0384, $8381,
    $0280, $8285, $828F, $028A, $829B, $029E, $0294, $8291,
    $82B3, $02B6, $02BC, $82B9, $02A8, $82AD, $82A7, $02A2,
    $82E3, $02E6, $02EC, $82E9, $02F8, $82FD, $82F7, $02F2,
    $02D0, $82D5, $82DF, $02DA, $82CB, $02CE, $02C4, $82C1,
    $8243, $0246, $024C, $8249, $0258, $825D, $8257, $0252,
    $0270, $8275, $827F, $027A, $826B, $026E, $0264, $8261,
    $0220, $8225, $822F, $022A, $823B, $023E, $0234, $8231,
    $8213, $0216, $021C, $8219, $0208, $820D, $8207, $0202
  );

// Standard CRC16
// Polynom:        8005
// Init value:     0000
// Reflection in:  False
// Reflection out: False
// XOR Output:     0000
function CRC16(
    const Data; DataCount: Cardinal;
    const InitValue: Word{$IFDEF SUPPORTS_DEFAULTPARAMS} = 0{$ENDIF}
  ): Word;
var
  Ptr: PByte;
  i: Cardinal;
begin
  Ptr := @Data;
  if not Assigned(Ptr) then
    DataCount := 0;
  // Initialisierung
  Result := InitValue;
  for i := 0 to DataCount - 1 do begin
    Result := CRCLookupTableCRC16[Byte((Result shr 8) xor Ptr^)] xor (Result shl 8);
    Inc(Ptr);
  end;
end;

const
  CRCLookupTableCRC16Reversed: TCRC16LookupTable = (
    $0000, $9705, $2E01, $B904, $5C02, $CB07, $7203, $E506,
    $B804, $2F01, $9605, $0100, $E406, $7303, $CA07, $5D02,
    $7003, $E706, $5E02, $C907, $2C01, $BB04, $0200, $9505,
    $C807, $5F02, $E606, $7103, $9405, $0300, $BA04, $2D01,
    $E006, $7703, $CE07, $5902, $BC04, $2B01, $9205, $0500,
    $5802, $CF07, $7603, $E106, $0400, $9305, $2A01, $BD04,
    $9005, $0700, $BE04, $2901, $CC07, $5B02, $E206, $7503,
    $2801, $BF04, $0600, $9105, $7403, $E306, $5A02, $CD07,
    $C007, $5702, $EE06, $7903, $9C05, $0B00, $B204, $2501,
    $7803, $EF06, $5602, $C107, $2401, $B304, $0A00, $9D05,
    $B004, $2701, $9E05, $0900, $EC06, $7B03, $C207, $5502,
    $0800, $9F05, $2601, $B104, $5402, $C307, $7A03, $ED06,
    $2001, $B704, $0E00, $9905, $7C03, $EB06, $5202, $C507,
    $9805, $0F00, $B604, $2101, $C407, $5302, $EA06, $7D03,
    $5002, $C707, $7E03, $E906, $0C00, $9B05, $2201, $B504,
    $E806, $7F03, $C607, $5102, $B404, $2301, $9A05, $0D00,
    $8005, $1700, $AE04, $3901, $DC07, $4B02, $F206, $6503,
    $3801, $AF04, $1600, $8105, $6403, $F306, $4A02, $DD07,
    $F006, $6703, $DE07, $4902, $AC04, $3B01, $8205, $1500,
    $4802, $DF07, $6603, $F106, $1400, $8305, $3A01, $AD04,
    $6003, $F706, $4E02, $D907, $3C01, $AB04, $1200, $8505,
    $D807, $4F02, $F606, $6103, $8405, $1300, $AA04, $3D01,
    $1000, $8705, $3E01, $A904, $4C02, $DB07, $6203, $F506,
    $A804, $3F01, $8605, $1100, $F406, $6303, $DA07, $4D02,
    $4002, $D707, $6E03, $F906, $1C00, $8B05, $3201, $A504,
    $F806, $6F03, $D607, $4102, $A404, $3301, $8A05, $1D00,
    $3001, $A704, $1E00, $8905, $6C03, $FB06, $4202, $D507,
    $8805, $1F00, $A604, $3101, $D407, $4302, $FA06, $6D03,
    $A004, $3701, $8E05, $1900, $FC06, $6B03, $D207, $4502,
    $1800, $8F05, $3601, $A104, $4402, $D307, $6A03, $FD06,
    $D007, $4702, $FE06, $6903, $8C05, $1B00, $A204, $3501,
    $6803, $FF06, $4602, $D107, $3401, $A304, $1A00, $8D05
  );

// Reversed CRC16
// Polynom:        A001
// Init value:     0000
// Reflection in:  True
// Reflection out: True
// XOR Output:     0000
function CRC16Reversed(
    const Data; DataCount: Cardinal;
    const InitValue: Word{$IFDEF SUPPORTS_DEFAULTPARAMS} = 0{$ENDIF}
  ): Word;
var
  Ptr: PByte;
  i: Cardinal;
begin
  Ptr := @Data;
  if not Assigned(Ptr) then
    DataCount := 0;
  // Initialisierung
  Result := InitValue;
  for i := 0 to DataCount - 1 do begin
    Result := CRCLookupTableCRC16Reversed[Byte(Result xor Ptr^)] xor ((Result shr 8) and $00FF);
    Inc(Ptr);
  end;
end;

const
  CRCLookupTableCRC16CCITT: TCRC16LookupTable = (
    $0000, $1021, $2042, $3063, $4084, $50A5, $60C6, $70E7,
    $8108, $9129, $A14A, $B16B, $C18C, $D1AD, $E1CE, $F1EF,
    $1231, $0210, $3273, $2252, $52B5, $4294, $72F7, $62D6,
    $9339, $8318, $B37B, $A35A, $D3BD, $C39C, $F3FF, $E3DE,
    $2462, $3443, $0420, $1401, $64E6, $74C7, $44A4, $5485,
    $A56A, $B54B, $8528, $9509, $E5EE, $F5CF, $C5AC, $D58D,
    $3653, $2672, $1611, $0630, $76D7, $66F6, $5695, $46B4,
    $B75B, $A77A, $9719, $8738, $F7DF, $E7FE, $D79D, $C7BC,
    $48C4, $58E5, $6886, $78A7, $0840, $1861, $2802, $3823,
    $C9CC, $D9ED, $E98E, $F9AF, $8948, $9969, $A90A, $B92B,
    $5AF5, $4AD4, $7AB7, $6A96, $1A71, $0A50, $3A33, $2A12,
    $DBFD, $CBDC, $FBBF, $EB9E, $9B79, $8B58, $BB3B, $AB1A,
    $6CA6, $7C87, $4CE4, $5CC5, $2C22, $3C03, $0C60, $1C41,
    $EDAE, $FD8F, $CDEC, $DDCD, $AD2A, $BD0B, $8D68, $9D49,
    $7E97, $6EB6, $5ED5, $4EF4, $3E13, $2E32, $1E51, $0E70,
    $FF9F, $EFBE, $DFDD, $CFFC, $BF1B, $AF3A, $9F59, $8F78,
    $9188, $81A9, $B1CA, $A1EB, $D10C, $C12D, $F14E, $E16F,
    $1080, $00A1, $30C2, $20E3, $5004, $4025, $7046, $6067,
    $83B9, $9398, $A3FB, $B3DA, $C33D, $D31C, $E37F, $F35E,
    $02B1, $1290, $22F3, $32D2, $4235, $5214, $6277, $7256,
    $B5EA, $A5CB, $95A8, $8589, $F56E, $E54F, $D52C, $C50D,
    $34E2, $24C3, $14A0, $0481, $7466, $6447, $5424, $4405,
    $A7DB, $B7FA, $8799, $97B8, $E75F, $F77E, $C71D, $D73C,
    $26D3, $36F2, $0691, $16B0, $6657, $7676, $4615, $5634,
    $D94C, $C96D, $F90E, $E92F, $99C8, $89E9, $B98A, $A9AB,
    $5844, $4865, $7806, $6827, $18C0, $08E1, $3882, $28A3,
    $CB7D, $DB5C, $EB3F, $FB1E, $8BF9, $9BD8, $ABBB, $BB9A,
    $4A75, $5A54, $6A37, $7A16, $0AF1, $1AD0, $2AB3, $3A92,
    $FD2E, $ED0F, $DD6C, $CD4D, $BDAA, $AD8B, $9DE8, $8DC9,
    $7C26, $6C07, $5C64, $4C45, $3CA2, $2C83, $1CE0, $0CC1,
    $EF1F, $FF3E, $CF5D, $DF7C, $AF9B, $BFBA, $8FD9, $9FF8,
    $6E17, $7E36, $4E55, $5E74, $2E93, $3EB2, $0ED1, $1EF0
  );

// Standard CRC16 CCITT
// e.g. X.25, SDLC, and HDLC
// Polynom:        1021
// Init value:     FFFF
// Reflection in:  False
// Reflection out: False
// XOR Output:     0000
function CRC16CCITT(
    const Data; DataCount: Cardinal;
    const InitValue: Word{$IFDEF SUPPORTS_DEFAULTPARAMS} = $FFFF{$ENDIF}
  ): Word;
var
  Ptr: PByte;
  i: Cardinal;
begin
  Ptr := @Data;
  if not Assigned(Ptr) then
    DataCount := 0;
  // Initialisierung
  Result := InitValue;
  for i := 0 to DataCount - 1 do begin
    Result := CRCLookupTableCRC16CCITT[Byte((Result shr 8) xor Ptr^)] xor (Result shl 8);
    Inc(Ptr);
  end;
end;

const
  CRCLookupTableCRC16CCITTReversed: TCRC16LookupTable = (
    $0000, $17CE, $0FDF, $1811, $1FBE, $0870, $1061, $07AF,
    $1F3F, $08F1, $10E0, $072E, $0081, $174F, $0F5E, $1890,
    $1E3D, $09F3, $11E2, $062C, $0183, $164D, $0E5C, $1992,
    $0102, $16CC, $0EDD, $1913, $1EBC, $0972, $1163, $06AD,
    $1C39, $0BF7, $13E6, $0428, $0387, $1449, $0C58, $1B96,
    $0306, $14C8, $0CD9, $1B17, $1CB8, $0B76, $1367, $04A9,
    $0204, $15CA, $0DDB, $1A15, $1DBA, $0A74, $1265, $05AB,
    $1D3B, $0AF5, $12E4, $052A, $0285, $154B, $0D5A, $1A94,
    $1831, $0FFF, $17EE, $0020, $078F, $1041, $0850, $1F9E,
    $070E, $10C0, $08D1, $1F1F, $18B0, $0F7E, $176F, $00A1,
    $060C, $11C2, $09D3, $1E1D, $19B2, $0E7C, $166D, $01A3,
    $1933, $0EFD, $16EC, $0122, $068D, $1143, $0952, $1E9C,
    $0408, $13C6, $0BD7, $1C19, $1BB6, $0C78, $1469, $03A7,
    $1B37, $0CF9, $14E8, $0326, $0489, $1347, $0B56, $1C98,
    $1A35, $0DFB, $15EA, $0224, $058B, $1245, $0A54, $1D9A,
    $050A, $12C4, $0AD5, $1D1B, $1AB4, $0D7A, $156B, $02A5,
    $1021, $07EF, $1FFE, $0830, $0F9F, $1851, $0040, $178E,
    $0F1E, $18D0, $00C1, $170F, $10A0, $076E, $1F7F, $08B1,
    $0E1C, $19D2, $01C3, $160D, $11A2, $066C, $1E7D, $09B3,
    $1123, $06ED, $1EFC, $0932, $0E9D, $1953, $0142, $168C,
    $0C18, $1BD6, $03C7, $1409, $13A6, $0468, $1C79, $0BB7,
    $1327, $04E9, $1CF8, $0B36, $0C99, $1B57, $0346, $1488,
    $1225, $05EB, $1DFA, $0A34, $0D9B, $1A55, $0244, $158A,
    $0D1A, $1AD4, $02C5, $150B, $12A4, $056A, $1D7B, $0AB5,
    $0810, $1FDE, $07CF, $1001, $17AE, $0060, $1871, $0FBF,
    $172F, $00E1, $18F0, $0F3E, $0891, $1F5F, $074E, $1080,
    $162D, $01E3, $19F2, $0E3C, $0993, $1E5D, $064C, $1182,
    $0912, $1EDC, $06CD, $1103, $16AC, $0162, $1973, $0EBD,
    $1429, $03E7, $1BF6, $0C38, $0B97, $1C59, $0448, $1386,
    $0B16, $1CD8, $04C9, $1307, $14A8, $0366, $1B77, $0CB9,
    $0A14, $1DDA, $05CB, $1205, $15AA, $0264, $1A75, $0DBB,
    $152B, $02E5, $1AF4, $0D3A, $0A95, $1D5B, $054A, $1284
  );

// Reversed CRC16 CCITT
// e.g. XMODEM and Kermit
// Polynom:        8408
// Init value:     0000
// Reflection in:  True
// Reflection out: True
// XOR Output:     0000
function CRC16CCITTReversed(
    const Data; DataCount: Cardinal;
    const InitValue: Word{$IFDEF SUPPORTS_DEFAULTPARAMS} = 0{$ENDIF}
  ): Word;
var
  Ptr: PByte;
  i: Cardinal;
begin
  Ptr := @Data;
  if not Assigned(Ptr) then
    DataCount := 0;
  // Initialisierung
  Result := InitValue;
  for i := 0 to DataCount - 1 do begin
    Result := CRCLookupTableCRC16CCITTReversed[Byte(Result xor Ptr^)] xor ((Result shr 8) and $00FF);
    Inc(Ptr);
  end;
end;

const
  CRCLookupTableCRC16ARC: TCRC16LookupTable = (
    $0000, $C0C1, $C181, $0140, $C301, $03C0, $0280, $C241,
    $C601, $06C0, $0780, $C741, $0500, $C5C1, $C481, $0440,
    $CC01, $0CC0, $0D80, $CD41, $0F00, $CFC1, $CE81, $0E40,
    $0A00, $CAC1, $CB81, $0B40, $C901, $09C0, $0880, $C841,
    $D801, $18C0, $1980, $D941, $1B00, $DBC1, $DA81, $1A40,
    $1E00, $DEC1, $DF81, $1F40, $DD01, $1DC0, $1C80, $DC41,
    $1400, $D4C1, $D581, $1540, $D701, $17C0, $1680, $D641,
    $D201, $12C0, $1380, $D341, $1100, $D1C1, $D081, $1040,
    $F001, $30C0, $3180, $F141, $3300, $F3C1, $F281, $3240,
    $3600, $F6C1, $F781, $3740, $F501, $35C0, $3480, $F441,
    $3C00, $FCC1, $FD81, $3D40, $FF01, $3FC0, $3E80, $FE41,
    $FA01, $3AC0, $3B80, $FB41, $3900, $F9C1, $F881, $3840,
    $2800, $E8C1, $E981, $2940, $EB01, $2BC0, $2A80, $EA41,
    $EE01, $2EC0, $2F80, $EF41, $2D00, $EDC1, $EC81, $2C40,
    $E401, $24C0, $2580, $E541, $2700, $E7C1, $E681, $2640,
    $2200, $E2C1, $E381, $2340, $E101, $21C0, $2080, $E041,
    $A001, $60C0, $6180, $A141, $6300, $A3C1, $A281, $6240,
    $6600, $A6C1, $A781, $6740, $A501, $65C0, $6480, $A441,
    $6C00, $ACC1, $AD81, $6D40, $AF01, $6FC0, $6E80, $AE41,
    $AA01, $6AC0, $6B80, $AB41, $6900, $A9C1, $A881, $6840,
    $7800, $B8C1, $B981, $7940, $BB01, $7BC0, $7A80, $BA41,
    $BE01, $7EC0, $7F80, $BF41, $7D00, $BDC1, $BC81, $7C40,
    $B401, $74C0, $7580, $B541, $7700, $B7C1, $B681, $7640,
    $7200, $B2C1, $B381, $7340, $B101, $71C0, $7080, $B041,
    $5000, $90C1, $9181, $5140, $9301, $53C0, $5280, $9241,
    $9601, $56C0, $5780, $9741, $5500, $95C1, $9481, $5440,
    $9C01, $5CC0, $5D80, $9D41, $5F00, $9FC1, $9E81, $5E40,
    $5A00, $9AC1, $9B81, $5B40, $9901, $59C0, $5880, $9841,
    $8801, $48C0, $4980, $8941, $4B00, $8BC1, $8A81, $4A40,
    $4E00, $8EC1, $8F81, $4F40, $8D01, $4DC0, $4C80, $8C41,
    $4400, $84C1, $8581, $4540, $8701, $47C0, $4680, $8641,
    $8201, $42C0, $4380, $8341, $4100, $81C1, $8081, $4040
  );

// Variation of CRC16
// e.g. ARC
// Polynom:        8005
// Init value:     0000
// Reflection in:  True
// Reflection out: True
// XOR Output:     0000
function CRC16ARC(
    const Data; DataCount: Cardinal;
    const InitValue: Word{$IFDEF SUPPORTS_DEFAULTPARAMS} = 0{$ENDIF}
  ): Word;
var
  Ptr: PByte;
  i: Cardinal;
begin
  Ptr := @Data;
  if not Assigned(Ptr) then
    DataCount := 0;
  // Initialisierung
  Result := InitValue;
  for i := 0 to DataCount - 1 do begin
    Result := CRCLookupTableCRC16ARC[Byte(Result xor Ptr^)] xor ((Result shr 8) and $00FF);
    Inc(Ptr);
  end;
end;

// Variation of CRC16
// e.g. ZMODEM
// Polynom:        1021
// Init value:     0000
// Reflection in:  False
// Reflection out: False
// XOR Output:     0000
function CRC16ZMODEM(
    const Data; DataCount: Cardinal;
    const InitValue: Word{$IFDEF SUPPORTS_DEFAULTPARAMS} = 0{$ENDIF}
  ): Word;
var
  Ptr: PByte;
  i: Cardinal;
begin
  Ptr := @Data;
  if not Assigned(Ptr) then
    DataCount := 0;
  // Initialisierung
  Result := InitValue;
  for i := 0 to DataCount - 1 do begin
    Result := CRCLookupTableCRC16CCITT[Byte((Result shr 8) xor Ptr^)] xor (Result shl 8);
    Inc(Ptr);
  end;
end;

const
  CRCLookupTableCRC24: TCRC32LookupTable = (
    $00000000, $00864CFB, $008AD50D, $000C99F6,  $0093E6E1, $0015AA1A, $001933EC, $009F7F17,
    $00A18139, $0027CDC2, $002B5434, $00AD18CF,  $003267D8, $00B42B23, $00B8B2D5, $003EFE2E,
    $00C54E89, $00430272, $004F9B84, $00C9D77F,  $0056A868, $00D0E493, $00DC7D65, $005A319E,
    $0064CFB0, $00E2834B, $00EE1ABD, $00685646,  $00F72951, $007165AA, $007DFC5C, $00FBB0A7,
    $000CD1E9, $008A9D12, $008604E4, $0000481F,  $009F3708, $00197BF3, $0015E205, $0093AEFE,
    $00AD50D0, $002B1C2B, $002785DD, $00A1C926,  $003EB631, $00B8FACA, $00B4633C, $00322FC7,
    $00C99F60, $004FD39B, $00434A6D, $00C50696,  $005A7981, $00DC357A, $00D0AC8C, $0056E077,
    $00681E59, $00EE52A2, $00E2CB54, $006487AF,  $00FBF8B8, $007DB443, $00712DB5, $00F7614E,

    $0019A3D2, $009FEF29, $009376DF, $00153A24,  $008A4533, $000C09C8, $0000903E, $0086DCC5,
    $00B822EB, $003E6E10, $0032F7E6, $00B4BB1D,  $002BC40A, $00AD88F1, $00A11107, $00275DFC,
    $00DCED5B, $005AA1A0, $00563856, $00D074AD,  $004F0BBA, $00C94741, $00C5DEB7, $0043924C,
    $007D6C62, $00FB2099, $00F7B96F, $0071F594,  $00EE8A83, $0068C678, $00645F8E, $00E21375,
    $0015723B, $00933EC0, $009FA736, $0019EBCD,  $008694DA, $0000D821, $000C41D7, $008A0D2C,
    $00B4F302, $0032BFF9, $003E260F, $00B86AF4,  $002715E3, $00A15918, $00ADC0EE, $002B8C15,
    $00D03CB2, $00567049, $005AE9BF, $00DCA544,  $0043DA53, $00C596A8, $00C90F5E, $004F43A5,
    $0071BD8B, $00F7F170, $00FB6886, $007D247D,  $00E25B6A, $00641791, $00688E67, $00EEC29C,

    $003347A4, $00B50B5F, $00B992A9, $003FDE52,  $00A0A145, $0026EDBE, $002A7448, $00AC38B3,
    $0092C69D, $00148A66, $00181390, $009E5F6B,  $0001207C, $00876C87, $008BF571, $000DB98A,
    $00F6092D, $007045D6, $007CDC20, $00FA90DB,  $0065EFCC, $00E3A337, $00EF3AC1, $0069763A,
    $00578814, $00D1C4EF, $00DD5D19, $005B11E2,  $00C46EF5, $0042220E, $004EBBF8, $00C8F703,
    $003F964D, $00B9DAB6, $00B54340, $00330FBB,  $00AC70AC, $002A3C57, $0026A5A1, $00A0E95A,
    $009E1774, $00185B8F, $0014C279, $00928E82,  $000DF195, $008BBD6E, $00872498, $00016863,
    $00FAD8C4, $007C943F, $00700DC9, $00F64132,  $00693E25, $00EF72DE, $00E3EB28, $0065A7D3,
    $005B59FD, $00DD1506, $00D18CF0, $0057C00B,  $00C8BF1C, $004EF3E7, $00426A11, $00C426EA,

    $002AE476, $00ACA88D, $00A0317B, $00267D80,  $00B90297, $003F4E6C, $0033D79A, $00B59B61,
    $008B654F, $000D29B4, $0001B042, $0087FCB9,  $001883AE, $009ECF55, $009256A3, $00141A58,
    $00EFAAFF, $0069E604, $00657FF2, $00E33309,  $007C4C1E, $00FA00E5, $00F69913, $0070D5E8,
    $004E2BC6, $00C8673D, $00C4FECB, $0042B230,  $00DDCD27, $005B81DC, $0057182A, $00D154D1,
    $0026359F, $00A07964, $00ACE092, $002AAC69,  $00B5D37E, $00339F85, $003F0673, $00B94A88,
    $0087B4A6, $0001F85D, $000D61AB, $008B2D50,  $00145247, $00921EBC, $009E874A, $0018CBB1,
    $00E37B16, $006537ED, $0069AE1B, $00EFE2E0,  $00709DF7, $00F6D10C, $00FA48FA, $007C0401,
    $0042FA2F, $00C4B6D4, $00C82F22, $004E63D9,  $00D11CCE, $00575035, $005BC9C3, $00DD8538
  );

// Standard CRC24
// e.g. PGP
// Polynom:        1864CFB
// Init value:     B704CE
// Reflection in:  False
// Reflection out: False
// XOR Output:     000000
function CRC24(
    const Data; DataCount: Cardinal;
    const InitValue: LongWord{$IFDEF SUPPORTS_DEFAULTPARAMS} = $B704CE{$ENDIF}
  ): LongWord;
var
  Ptr: PByte;
  i: Cardinal;
begin
  Ptr := @Data;
  if not Assigned(Ptr) then
    DataCount := 0;
  // Initialisierung
  Result := InitValue;
  for i := 0 to DataCount - 1 do begin
    Result := CRCLookupTableCRC24[Byte((Result shr 16) xor Ptr^)] xor (Result shl 8);
    Inc(Ptr);
  end;
end;

const
  CRCLookupTableCRC32: TCRC32LookupTable = (
    $00000000, $EDB88320, $36C98560, $DB710640,  $6D930AC0, $802B89E0, $5B5A8FA0, $B6E20C80,
    $DB261580, $369E96A0, $EDEF90E0, $005713C0,  $B6B51F40, $5B0D9C60, $807C9A20, $6DC41900,
    $5BF4A820, $B64C2B00, $6D3D2D40, $8085AE60,  $3667A2E0, $DBDF21C0, $00AE2780, $ED16A4A0,
    $80D2BDA0, $6D6A3E80, $B61B38C0, $5BA3BBE0,  $ED41B760, $00F93440, $DB883200, $3630B120,
    $B7E95040, $5A51D360, $8120D520, $6C985600,  $DA7A5A80, $37C2D9A0, $ECB3DFE0, $010B5CC0,
    $6CCF45C0, $8177C6E0, $5A06C0A0, $B7BE4380,  $015C4F00, $ECE4CC20, $3795CA60, $DA2D4940,
    $EC1DF860, $01A57B40, $DAD47D00, $376CFE20,  $818EF2A0, $6C367180, $B74777C0, $5AFFF4E0,
    $373BEDE0, $DA836EC0, $01F26880, $EC4AEBA0,  $5AA8E720, $B7106400, $6C616240, $81D9E160,
    
    $826A23A0, $6FD2A080, $B4A3A6C0, $591B25E0,  $EFF92960, $0241AA40, $D930AC00, $34882F20,
    $594C3620, $B4F4B500, $6F85B340, $823D3060,  $34DF3CE0, $D967BFC0, $0216B980, $EFAE3AA0,
    $D99E8B80, $342608A0, $EF570EE0, $02EF8DC0,  $B40D8140, $59B50260, $82C40420, $6F7C8700,
    $02B89E00, $EF001D20, $34711B60, $D9C99840,  $6F2B94C0, $829317E0, $59E211A0, $B45A9280,
    $358373E0, $D83BF0C0, $034AF680, $EEF275A0,  $58107920, $B5A8FA00, $6ED9FC40, $83617F60,
    $EEA56660, $031DE540, $D86CE300, $35D46020,  $83366CA0, $6E8EEF80, $B5FFE9C0, $58476AE0,
    $6E77DBC0, $83CF58E0, $58BE5EA0, $B506DD80,  $03E4D100, $EE5C5220, $352D5460, $D895D740,
    $B551CE40, $58E94D60, $83984B20, $6E20C800,  $D8C2C480, $357A47A0, $EE0B41E0, $03B3C2C0,

    $E96CC460, $04D44740, $DFA54100, $321DC220,  $84FFCEA0, $69474D80, $B2364BC0, $5F8EC8E0,
    $324AD1E0, $DFF252C0, $04835480, $E93BD7A0,  $5FD9DB20, $B2615800, $69105E40, $84A8DD60,
    $B2986C40, $5F20EF60, $8451E920, $69E96A00,  $DF0B6680, $32B3E5A0, $E9C2E3E0, $047A60C0,
    $69BE79C0, $8406FAE0, $5F77FCA0, $B2CF7F80,  $042D7300, $E995F020, $32E4F660, $DF5C7540,
    $5E859420, $B33D1700, $684C1140, $85F49260,  $33169EE0, $DEAE1DC0, $05DF1B80, $E86798A0,
    $85A381A0, $681B0280, $B36A04C0, $5ED287E0,  $E8308B60, $05880840, $DEF90E00, $33418D20,
    $05713C00, $E8C9BF20, $33B8B960, $DE003A40,  $68E236C0, $855AB5E0, $5E2BB3A0, $B3933080,
    $DE572980, $33EFAAA0, $E89EACE0, $05262FC0,  $B3C42340, $5E7CA060, $850DA620, $68B52500,

    $6B06E7C0, $86BE64E0, $5DCF62A0, $B077E180,  $0695ED00, $EB2D6E20, $305C6860, $DDE4EB40,
    $B020F240, $5D987160, $86E97720, $6B51F400,  $DDB3F880, $300B7BA0, $EB7A7DE0, $06C2FEC0,
    $30F24FE0, $DD4ACCC0, $063BCA80, $EB8349A0,  $5D614520, $B0D9C600, $6BA8C040, $86104360,
    $EBD45A60, $066CD940, $DD1DDF00, $30A55C20,  $864750A0, $6BFFD380, $B08ED5C0, $5D3656E0,
    $DCEFB780, $315734A0, $EA2632E0, $079EB1C0,  $B17CBD40, $5CC43E60, $87B53820, $6A0DBB00,
    $07C9A200, $EA712120, $31002760, $DCB8A440,  $6A5AA8C0, $87E22BE0, $5C932DA0, $B12BAE80,
    $871B1FA0, $6AA39C80, $B1D29AC0, $5C6A19E0,  $EA881560, $07309640, $DC419000, $31F91320,
    $5C3D0A20, $B1858900, $6AF48F40, $874C0C60,  $31AE00E0, $DC1683C0, $07678580, $EADF06A0
  );

// Standard CRC32
// e.g. AUTODIN II, Ethernet, and FDDI
// Polynom:        EDB88320
// Init value:     FFFFFFFF
// Reflection in:  False
// Reflection out: False
// XOR Output:     FFFFFFFF
function CRC32(
    const Data; DataCount: Cardinal;
    const InitValue: LongWord{$IFDEF SUPPORTS_DEFAULTPARAMS} = 0{$ENDIF}
  ): LongWord;
var
  Ptr: PByte;
  i: Cardinal;
begin
  Ptr := @Data;
  if not Assigned(Ptr) then
    DataCount := 0;
  // Initialisierung
  Result := not InitValue;
  for i := 0 to DataCount - 1 do begin
    Result := CRCLookupTableCRC32[Byte((Result shr 24) xor Ptr^)] xor (Result shl 8);
    Inc(Ptr);
  end;
  Result := not Result;
end;

const
  CRCLookupTableCRC32Reversed: TCRC32LookupTable = (
    $00000000, $77073096, $EE0E612C, $990951BA,  $076DC419, $706AF48F, $E963A535, $9E6495A3,
    $0EDB8832, $79DCB8A4, $E0D5E91E, $97D2D988,  $09B64C2B, $7EB17CBD, $E7B82D07, $90BF1D91,
    $1DB71064, $6AB020F2, $F3B97148, $84BE41DE,  $1ADAD47D, $6DDDE4EB, $F4D4B551, $83D385C7,
    $136C9856, $646BA8C0, $FD62F97A, $8A65C9EC,  $14015C4F, $63066CD9, $FA0F3D63, $8D080DF5,
    $3B6E20C8, $4C69105E, $D56041E4, $A2677172,  $3C03E4D1, $4B04D447, $D20D85FD, $A50AB56B,
    $35B5A8FA, $42B2986C, $DBBBC9D6, $ACBCF940,  $32D86CE3, $45DF5C75, $DCD60DCF, $ABD13D59,
    $26D930AC, $51DE003A, $C8D75180, $BFD06116,  $21B4F4B5, $56B3C423, $CFBA9599, $B8BDA50F,
    $2802B89E, $5F058808, $C60CD9B2, $B10BE924,  $2F6F7C87, $58684C11, $C1611DAB, $B6662D3D,

    $76DC4190, $01DB7106, $98D220BC, $EFD5102A,  $71B18589, $06B6B51F, $9FBFE4A5, $E8B8D433,
    $7807C9A2, $0F00F934, $9609A88E, $E10E9818,  $7F6A0DBB, $086D3D2D, $91646C97, $E6635C01,
    $6B6B51F4, $1C6C6162, $856530D8, $F262004E,  $6C0695ED, $1B01A57B, $8208F4C1, $F50FC457,
    $65B0D9C6, $12B7E950, $8BBEB8EA, $FCB9887C,  $62DD1DDF, $15DA2D49, $8CD37CF3, $FBD44C65,
    $4DB26158, $3AB551CE, $A3BC0074, $D4BB30E2,  $4ADFA541, $3DD895D7, $A4D1C46D, $D3D6F4FB,
    $4369E96A, $346ED9FC, $AD678846, $DA60B8D0,  $44042D73, $33031DE5, $AA0A4C5F, $DD0D7CC9,
    $5005713C, $270241AA, $BE0B1010, $C90C2086,  $5768B525, $206F85B3, $B966D409, $CE61E49F,
    $5EDEF90E, $29D9C998, $B0D09822, $C7D7A8B4,  $59B33D17, $2EB40D81, $B7BD5C3B, $C0BA6CAD,

    $EDB88320, $9ABFB3B6, $03B6E20C, $74B1D29A,  $EAD54739, $9DD277AF, $04DB2615, $73DC1683,
    $E3630B12, $94643B84, $0D6D6A3E, $7A6A5AA8,  $E40ECF0B, $9309FF9D, $0A00AE27, $7D079EB1,
    $F00F9344, $8708A3D2, $1E01F268, $6906C2FE,  $F762575D, $806567CB, $196C3671, $6E6B06E7,
    $FED41B76, $89D32BE0, $10DA7A5A, $67DD4ACC,  $F9B9DF6F, $8EBEEFF9, $17B7BE43, $60B08ED5,
    $D6D6A3E8, $A1D1937E, $38D8C2C4, $4FDFF252,  $D1BB67F1, $A6BC5767, $3FB506DD, $48B2364B,
    $D80D2BDA, $AF0A1B4C, $36034AF6, $41047A60,  $DF60EFC3, $A867DF55, $316E8EEF, $4669BE79,
    $CB61B38C, $BC66831A, $256FD2A0, $5268E236,  $CC0C7795, $BB0B4703, $220216B9, $5505262F,
    $C5BA3BBE, $B2BD0B28, $2BB45A92, $5CB36A04,  $C2D7FFA7, $B5D0CF31, $2CD99E8B, $5BDEAE1D,

    $9B64C2B0, $EC63F226, $756AA39C, $026D930A,  $9C0906A9, $EB0E363F, $72076785, $05005713,
    $95BF4A82, $E2B87A14, $7BB12BAE, $0CB61B38,  $92D28E9B, $E5D5BE0D, $7CDCEFB7, $0BDBDF21,
    $86D3D2D4, $F1D4E242, $68DDB3F8, $1FDA836E,  $81BE16CD, $F6B9265B, $6FB077E1, $18B74777,
    $88085AE6, $FF0F6A70, $66063BCA, $11010B5C,  $8F659EFF, $F862AE69, $616BFFD3, $166CCF45,
    $A00AE278, $D70DD2EE, $4E048354, $3903B3C2,  $A7672661, $D06016F7, $4969474D, $3E6E77DB,
    $AED16A4A, $D9D65ADC, $40DF0B66, $37D83BF0,  $A9BCAE53, $DEBB9EC5, $47B2CF7F, $30B5FFE9,
    $BDBDF21C, $CABAC28A, $53B39330, $24B4A3A6,  $BAD03605, $CDD70693, $54DE5729, $23D967BF,
    $B3667A2E, $C4614AB8, $5D681B02, $2A6F2B94,  $B40BBE37, $C30C8EA1, $5A05DF1B, $2D02EF8D
  );

// Reversed CRC32
// e.g. PKZip, zlib and SFV
// Polynom:        04C11DB7
// Init value:     FFFFFFFF
// Reflection in:  True
// Reflection out: True
// XOR Output:     FFFFFFFF
{$IFDEF CPUI386}
// Assembler version
function CRC32Reversed(
    const Data; DataCount: Cardinal;
    const InitValue: LongWord{$IFDEF SUPPORTS_DEFAULTPARAMS} = 0{$ENDIF}
  ): LongWord; register; assembler;
// EAX: Data
// EDX: DataCount
// ECX: InitValue
asm
        OR      EAX, EAX           // Assiged(Data)
        JE      @@Exit
        OR      EDX, EDX           // DataCount = 0
        JE      @@Exit
        PUSH    EBX
        NOT     ECX                // Initial value: $FFFFFFFF
@@m1:
        XOR     EBX, EBX           // EBX := Result and $000000FF
        MOV     BL, CL
        XOR     BL, BYTE [EAX]     // EBX := Data^ xor EBX
        SHR     ECX, 8             // Result := Result shr 8
        XOR     ECX, DWORD PTR CRCLookupTableCRC32Reversed[EBX * 4]  // Result := Result xor Table[EBX]
        INC     EAX                // Inc(Data)
        DEC     EDX
        JNZ     @@m1               // next byte
        POP     EBX
        NOT     ECX                // Xor output: $FFFFFFFF
@@exit:
        MOV     EAX, ECX
        RET
end;
{$ELSE CPUI386~}
function CRC32Reversed(
    const Data; DataCount: Cardinal;
    const InitValue: LongWord{$IFDEF SUPPORTS_DEFAULTPARAMS} = 0{$ENDIF}
  ): LongWord;
var
  Ptr: PByte;
  i: Cardinal;
  TableIndex: Cardinal;
begin
  Ptr := @Data;
  if not Assigned(Ptr) then
    DataCount := 0;
  Result := not InitValue;
  for i := 1 to DataCount do begin
    TableIndex := Ptr^ xor Byte(Result);
    Result := (Result shr 8);
    Result := Result xor CRCLookupTableCRC32Reversed[TableIndex - Low(CRCLookupTableCRC32Reversed)];
    Inc(Ptr);
  end;
  Result := not Result;
end;
{$ENDIF ~CPUI386}

// Variation of CRC32
// e.g. JAMCRC
// Polynom:        04C11DB7
// Init value:     FFFFFFFF
// Reflection in:  True
// Reflection out: True
// XOR Output:     00000000
function CRC32JAMCRC(
    const Data; DataCount: Cardinal;
    const InitValue: LongWord{$IFDEF SUPPORTS_DEFAULTPARAMS} = $FFFFFFFF{$ENDIF}
  ): LongWord;
var
  Ptr: PByte;
  i: Cardinal;
  TableIndex: Cardinal;
begin
  Ptr := @Data;
  if not Assigned(Ptr) then
    DataCount := 0;
  Result := InitValue;
  for i := 1 to DataCount do begin
    TableIndex := Ptr^ xor Byte(Result);
    Result := (Result shr 8);
    Result := Result xor CRCLookupTableCRC32Reversed[TableIndex - Low(CRCLookupTableCRC32Reversed)];
    Inc(Ptr);
  end;
end;

const
  CRCLookupTableCRC32BZIP2: TCRC32LookupTable = (
    $00000000, $04C11DB7, $09823B6E, $0D4326D9,  $130476DC, $17C56B6B, $1A864DB2, $1E475005,
    $2608EDB8, $22C9F00F, $2F8AD6D6, $2B4BCB61,  $350C9B64, $31CD86D3, $3C8EA00A, $384FBDBD,
    $4C11DB70, $48D0C6C7, $4593E01E, $4152FDA9,  $5F15ADAC, $5BD4B01B, $569796C2, $52568B75,
    $6A1936C8, $6ED82B7F, $639B0DA6, $675A1011,  $791D4014, $7DDC5DA3, $709F7B7A, $745E66CD,
    $9823B6E0, $9CE2AB57, $91A18D8E, $95609039,  $8B27C03C, $8FE6DD8B, $82A5FB52, $8664E6E5,
    $BE2B5B58, $BAEA46EF, $B7A96036, $B3687D81,  $AD2F2D84, $A9EE3033, $A4AD16EA, $A06C0B5D,
    $D4326D90, $D0F37027, $DDB056FE, $D9714B49,  $C7361B4C, $C3F706FB, $CEB42022, $CA753D95,
    $F23A8028, $F6FB9D9F, $FBB8BB46, $FF79A6F1,  $E13EF6F4, $E5FFEB43, $E8BCCD9A, $EC7DD02D,

    $34867077, $30476DC0, $3D044B19, $39C556AE,  $278206AB, $23431B1C, $2E003DC5, $2AC12072,
    $128E9DCF, $164F8078, $1B0CA6A1, $1FCDBB16,  $018AEB13, $054BF6A4, $0808D07D, $0CC9CDCA,
    $7897AB07, $7C56B6B0, $71159069, $75D48DDE,  $6B93DDDB, $6F52C06C, $6211E6B5, $66D0FB02,
    $5E9F46BF, $5A5E5B08, $571D7DD1, $53DC6066,  $4D9B3063, $495A2DD4, $44190B0D, $40D816BA,
    $ACA5C697, $A864DB20, $A527FDF9, $A1E6E04E,  $BFA1B04B, $BB60ADFC, $B6238B25, $B2E29692,
    $8AAD2B2F, $8E6C3698, $832F1041, $87EE0DF6,  $99A95DF3, $9D684044, $902B669D, $94EA7B2A,
    $E0B41DE7, $E4750050, $E9362689, $EDF73B3E,  $F3B06B3B, $F771768C, $FA325055, $FEF34DE2,
    $C6BCF05F, $C27DEDE8, $CF3ECB31, $CBFFD686,  $D5B88683, $D1799B34, $DC3ABDED, $D8FBA05A,

    $690CE0EE, $6DCDFD59, $608EDB80, $644FC637,  $7A089632, $7EC98B85, $738AAD5C, $774BB0EB,
    $4F040D56, $4BC510E1, $46863638, $42472B8F,  $5C007B8A, $58C1663D, $558240E4, $51435D53,
    $251D3B9E, $21DC2629, $2C9F00F0, $285E1D47,  $36194D42, $32D850F5, $3F9B762C, $3B5A6B9B,
    $0315D626, $07D4CB91, $0A97ED48, $0E56F0FF,  $1011A0FA, $14D0BD4D, $19939B94, $1D528623,
    $F12F560E, $F5EE4BB9, $F8AD6D60, $FC6C70D7,  $E22B20D2, $E6EA3D65, $EBA91BBC, $EF68060B,
    $D727BBB6, $D3E6A601, $DEA580D8, $DA649D6F,  $C423CD6A, $C0E2D0DD, $CDA1F604, $C960EBB3,
    $BD3E8D7E, $B9FF90C9, $B4BCB610, $B07DABA7,  $AE3AFBA2, $AAFBE615, $A7B8C0CC, $A379DD7B,
    $9B3660C6, $9FF77D71, $92B45BA8, $9675461F,  $8832161A, $8CF30BAD, $81B02D74, $857130C3,

    $5D8A9099, $594B8D2E, $5408ABF7, $50C9B640,  $4E8EE645, $4A4FFBF2, $470CDD2B, $43CDC09C,
    $7B827D21, $7F436096, $7200464F, $76C15BF8,  $68860BFD, $6C47164A, $61043093, $65C52D24,
    $119B4BE9, $155A565E, $18197087, $1CD86D30,  $029F3D35, $065E2082, $0B1D065B, $0FDC1BEC,
    $3793A651, $3352BBE6, $3E119D3F, $3AD08088,  $2497D08D, $2056CD3A, $2D15EBE3, $29D4F654,
    $C5A92679, $C1683BCE, $CC2B1D17, $C8EA00A0,  $D6AD50A5, $D26C4D12, $DF2F6BCB, $DBEE767C,
    $E3A1CBC1, $E760D676, $EA23F0AF, $EEE2ED18,  $F0A5BD1D, $F464A0AA, $F9278673, $FDE69BC4,
    $89B8FD09, $8D79E0BE, $803AC667, $84FBDBD0,  $9ABC8BD5, $9E7D9662, $933EB0BB, $97FFAD0C,
    $AFB010B1, $AB710D06, $A6322BDF, $A2F33668,  $BCB4666D, $B8757BDA, $B5365D03, $B1F740B4
  );

// Variation of CRC32
// e.g. BZip2
// Polynom:        04C11DB7
// Init value:     FFFFFFFF
// Reflection in:  False
// Reflection out: False
// XOR Output:     FFFFFFFF
function CRC32BZIP2(
    const Data; DataCount: Cardinal;
    const InitValue: LongWord{$IFDEF SUPPORTS_DEFAULTPARAMS} = 0{$ENDIF}
  ): LongWord;
var
  Ptr: PByte;
  i: Cardinal;
begin
  Ptr := @Data;
  if not Assigned(Ptr) then
    DataCount := 0;
  Result := not InitValue;
  for i := 1 to DataCount do begin
    Result := CRCLookupTableCRC32BZIP2[Byte((Result shr 24) xor Ptr^)] xor (Result shl 8);
    Inc(Ptr);
  end;
  Result := not Result;
end;

// *******************************************************************************************

//  History:
//  2005-03-23, Peter J. Haas
//   - CRCReflect durch BitSwapN32 / BitSwapN64 ersetzen
//
//  2005-03-05, Peter J. Haas
//   - some modifications to remove FPC hints
//
//   2005-02-19, Peter J. Haas
//    - Ycl version
//    - integrate CRC32.pas and parts of CRCs.pas
//    - add Sum8, Sum16, Sum24, Sum32, SumSystemV, SumBSD, Xor8, Adler32
//    - extend Parameter based CRC functions / classes to 64 bit polynom
//    - add native support for CRC8, CRC8Reversed, CRC16, CRC16Reversed, CRC16CCITT,
//      CRC16CCITTReversed, CRC16ARC, CRC16ZMODEM, CRC24, CRC32, CRC32JAMCRC, CRC32BZIP2,
//      CRC32Reversed was CRC32PKZip
//
//   2002-04-29, Peter J. Haas, Version 1.2 (CRC32.pas)
//    - change licence to MPL (not really a alteration :-)                    
//    - change assembler source code syntax for Delphi 6 compatibility   
//
//   2001-06-04, Peter J. Haas
//    - Parameter based CRC functions / classes (CRCs.pas)
//
//   2000-12-15, Peter J. Haas, Version 1.1 (CRC32.pas)
//    - CalcCRC32PKZIP -> Assembler
//
//   2000-06-26, Peter J. Haas, Version 1.0 (CRC32.pas) 
//    - first public version

end.
