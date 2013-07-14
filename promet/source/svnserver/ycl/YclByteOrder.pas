{**************************************************************************************************}
{                                                                                                  }
{  Y core library (Ycl)                                                                            }
{                                                                                                  }
{  Functions to convert between the byte orders little endian and big endian                       }
{                                                                                                  }
{  The contents of this file are subject to the Y Library Public License Version 1.0 (the          }
{  "License"); you may not use this file except in compliance with the License. You may obtain a   }
{  copy of the License at http://delphi.pjh2.de/                                                   }
{                                                                                                  }
{  Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF  }
{  ANY KIND, either express or implied. See the License for the specific language governing        }
{  rights and limitations under the License.                                                       }
{                                                                                                  }
{  The Original Code is: YclByteOrder.pas.                                                         }
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
{$I YclUserDefines.inc}

unit YclByteOrder;

interface
uses
  YclBase;
  
// *******************************  Byte-Order tauschen  *************************************

var
  ByteSwap32: function(const Value: LongWord): LongWord; register; 

function ByteSwapUInt16(const Value: Word): Word;
function ByteSwapUInt32(const Value: LongWord): LongWord;
function ByteSwapUInt64(const Value: UInt64): UInt64;
function ByteSwapInt16(const Value: SmallInt): SmallInt;
function ByteSwapInt32(const Value: LongInt): LongInt;
function ByteSwapInt64(const Value: Int64): Int64;

// tauscht die Byte-Order in der Variable Value
procedure ByteSwapVarUInt16(var Value: Word);
procedure ByteSwapVarUInt32(var Value: LongWord);
procedure ByteSwapVarUInt64(var Value: UInt64);
procedure ByteSwapVarInt16(var Value: SmallInt);
procedure ByteSwapVarInt32(var Value: LongInt);
procedure ByteSwapVarInt64(var Value: Int64); 

implementation

// *******************************  Byte-Order tauschen  *************************************

{$IFDEF CPUI386}
function ByteSwap32_386(const Value: LongWord): LongWord; register; assembler;
asm                             // 44332211
        MOV     EDX, EAX        // 44332211
        ROR     EAX, 8          // 11443322
        AND     EAX, $FF00FF00  // 11003300
        ROL     EDX, 8          // 33221144
        AND     EDX, $00FF00FF  // 00220044
        OR      EAX, EDX        // 11223344
end;

function ByteSwap32_486(const Value: LongWord): LongWord; register; assembler;
asm
        BSWAP   EAX
end;
{$ENDIF CPUI386}

function ByteSwap32_Pascal(const Value: LongWord): LongWord;
begin
  Result := ((Value shl 24) and $FF000000) or
            ((Value shl  8) and $00FF0000) or
            ((Value shr  8) and $0000FF00) or
            ((Value shr 24) and $000000FF);
end;

procedure InitBSwap;
begin
  {$IFDEF CPUI386}
  {$IFDEF USE_BSWAP}
  ByteSwap32 := @ByteSwap32_486;
  {$ELSE USE_BSWAP~}
  ByteSwap32 := @ByteSwap32_386;
  {$ENDIF ~USE_BSWAP}
  {$ELSE CPUI386~}
  ByteSwap32 := @ByteSwap32_Pascal;
  {$ENDIF ~CPUI386}
end;

function ByteSwapUInt16(const Value: Word): Word;
begin
  Result := Swap(Value);
end;

function ByteSwapUInt32(const Value: LongWord): LongWord;
begin
  Result := ByteSwap32(Value);
end;

function ByteSwapUInt64(const Value: UInt64): UInt64;
begin
  TArrayLongWord2(Result)[0] := ByteSwap32(TArrayLongWord2(Value)[1]);
  TArrayLongWord2(Result)[1] := ByteSwap32(TArrayLongWord2(Value)[0]);
end;

function ByteSwapInt16(const Value: SmallInt): SmallInt;
begin
  Result := Swap(Value);
end;

function ByteSwapInt32(const Value: LongInt): LongInt;
begin
  Result := ByteSwap32(Value);
end;

function ByteSwapInt64(const Value: Int64): Int64;
begin
  TArrayLongWord2(Result)[0] := ByteSwap32(TArrayLongWord2(Value)[1]);
  TArrayLongWord2(Result)[1] := ByteSwap32(TArrayLongWord2(Value)[0]);
end;

// tauscht die Byte-Order in der Variable Value
procedure ByteSwapVarUInt16(var Value: Word);
begin
  Value := Swap(Value);
end;

procedure ByteSwapVarUInt32(var Value: LongWord);
begin
  Value := ByteSwap32(Value);
end;

procedure ByteSwapVarUInt64(var Value: UInt64);
var
  Temp: LongWord;
begin
  Temp := ByteSwap32(TArrayLongWord2(Value)[0]);
  TArrayLongWord2(Value)[0] := ByteSwap32(TArrayLongWord2(Value)[1]);
  TArrayLongWord2(Value)[1] := Temp;
end;

procedure ByteSwapVarInt16(var Value: SmallInt);
begin
  Value := Swap(Value);
end;

procedure ByteSwapVarInt32(var Value: LongInt);
begin
  Value := ByteSwap32(Value);
end;

procedure ByteSwapVarInt64(var Value: Int64);
var
  Temp: LongWord;
begin
  Temp := ByteSwap32(TArrayLongWord2(Value)[0]);
  TArrayLongWord2(Value)[0] := ByteSwap32(TArrayLongWord2(Value)[1]);
  TArrayLongWord2(Value)[1] := Temp;
end;

initialization
  InitBSwap;
end.
