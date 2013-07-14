{**************************************************************************************************}
{                                                                                                  }
{  Y core library (Ycl)                                                                            }
{                                                                                                  }
{  Base declarations                                                                               }
{                                                                                                  }
{  The contents of this file are subject to the Y Library Public License Version 1.0 (the          }
{  "License"); you may not use this file except in compliance with the License. You may obtain a   }
{  copy of the License at http://delphi.pjh2.de/                                                   }
{                                                                                                  }
{  Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF  }
{  ANY KIND, either express or implied. See the License for the specific language governing rights }
{  and limitations under the License.                                                              }
{                                                                                                  }
{  The Original Code is: YclBase.pas.                                                              }
{  The Initial Developer of the Original Code is Peter J. Haas (libs@pjh2.de). Portions created    }
{  by Peter J. Haas are Copyright (C) 2004-2005 Peter J. Haas. All Rights Reserved.                }
{                                                                                                  }
{  Contributor(s):                                                                                 }
{                                                                                                  }
{  You may retrieve the latest version of this file at the homepage of Peter J. Haas, located at   }
{  http://delphi.pjh2.de/                                                                          }
{                                                                                                  }
{**************************************************************************************************}

// For history see end of file

{$I Ycl.inc}

unit YclBase;

interface
uses
  SysUtils;
                      
type
  // 64 bit unsigned integer
  {$IFNDEF SUPPORTS_UINT64 ~}
  UInt64 = Int64;   // 64-bit
  {$ENDIF ~SUPPORTS_UINT64}

  // pointer to a unsigned integer
  {$IFNDEF SUPPORTS_PREDECLARED_PTRS ~}
  PByte     = ^Byte;
  PWord     = ^Word;
  PLongWord = ^LongWord;
  {$ENDIF ~SUPPORTS_PREDECLARED_PTRS}
  PUInt64   = ^UInt64;

  // pointer to a signed integer
  {$IFNDEF SUPPORTS_PREDECLARED_PTRS ~}
  PShortInt = ^ShortInt;
  PSmallInt = ^SmallInt;
  PLongInt  = ^LongInt;
  PInt64    = ^Int64;
  {$ENDIF ~SUPPORTS_PREDECLARED_PTRS}

  // pointer to a generic unsigned integer
  {$IFNDEF SUPPORTS_PREDECLARED_PTRS ~}
  PCardinal = ^Cardinal;
  {$ENDIF ~SUPPORTS_PREDECLARED_PTRS}

  // pointer to a generic signed integer
  {$IFNDEF SUPPORTS_PREDECLARED_PTRS ~}
  PInteger  = ^Integer;
  {$ENDIF ~SUPPORTS_PREDECLARED_PTRS}

  {$IFNDEF SUPPORTS_PREDECLARED_PTRS ~}
  PBoolean  = ^Boolean;
  {$ENDIF ~SUPPORTS_PREDECLARED_PTRS}

  // pointer to float types
  {$IFNDEF SUPPORTS_PREDECLARED_PTRS ~}
  PSingle = ^Single;
  PDouble = ^Double;
  PExtended = ^Extended;
  {$ENDIF ~SUPPORTS_PREDECLARED_PTRS}

  // Integer / Pointer casts
  {$IFNDEF SUPPORTS_PTRINT ~}
  PtrInt = LongInt;
  PtrUInt = LongWord;
  {$ENDIF ~SUPPORTS_PTRINT}

  // Array-Typen
  TArrayByte      = packed array[0..High(Integer) div SizeOf(Byte     ) - 1] of Byte;
  TArrayWord      = packed array[0..High(Integer) div SizeOf(Word     ) - 1] of Word;
  TArrayLongWord  = packed array[0..High(Integer) div SizeOf(LongWord ) - 1] of LongWord;
  TArrayUInt64    = packed array[0..High(Integer) div SizeOf(UInt64   ) - 1] of UInt64;

  TArrayShortInt  = packed array[0..High(Integer) div SizeOf(ShortInt ) - 1] of ShortInt;
  TArraySmallInt  = packed array[0..High(Integer) div SizeOf(SmallInt ) - 1] of SmallInt;
  TArrayLongInt   = packed array[0..High(Integer) div SizeOf(LongInt  ) - 1] of LongInt;
  TArrayInt64     = packed array[0..High(Integer) div SizeOf(Int64    ) - 1] of Int64;

  TArrayCardinal  = packed array[0..High(Integer) div SizeOf(Cardinal ) - 1] of Cardinal;
  TArrayInteger   = packed array[0..High(Integer) div SizeOf(Integer  ) - 1] of Integer;

  TArrayBoolean   = packed array[0..High(Integer) div SizeOf(Boolean  ) - 1] of Boolean;

  TArraySingle    = packed array[0..High(Integer) div SizeOf(Single   ) - 1] of Single;
  TArrayDouble    = packed array[0..High(Integer) div SizeOf(Double   ) - 1] of Double;
  TArrayExtended  = packed array[0..High(Integer) div SizeOf(Extended ) - 1] of Extended;

  TArrayAnsiChar  = packed array[0..High(Integer) div SizeOf(AnsiChar ) - 1] of AnsiChar;
  TArrayWideChar  = packed array[0..High(Integer) div SizeOf(WideChar ) - 1] of WideChar;

  // Array-Typen mit Zeigern
  TArrayPointer   = packed array[0..High(Integer) div SizeOf(Pointer  ) - 1] of Pointer;

  TArrayPByte     = packed array[0..High(Integer) div SizeOf(PByte    ) - 1] of PByte;
  TArrayPWord     = packed array[0..High(Integer) div SizeOf(PWord    ) - 1] of PWord;
  TArrayPLongWord = packed array[0..High(Integer) div SizeOf(PLongWord) - 1] of PLongWord;
  TArrayPUInt64   = packed array[0..High(Integer) div SizeOf(PUInt64  ) - 1] of PUInt64;

  TArrayPShortInt = packed array[0..High(Integer) div SizeOf(PShortInt) - 1] of PShortInt;
  TArrayPSmallInt = packed array[0..High(Integer) div SizeOf(PSmallInt) - 1] of PSmallInt;
  TArrayPLongInt  = packed array[0..High(Integer) div SizeOf(PLongInt ) - 1] of PLongInt;
  TArrayPInt64    = packed array[0..High(Integer) div SizeOf(PInt64   ) - 1] of PInt64;

  TArrayPCardinal = packed array[0..High(Integer) div SizeOf(PCardinal) - 1] of PCardinal;
  TArrayPInteger  = packed array[0..High(Integer) div SizeOf(PInteger ) - 1] of PInteger;

  TArrayPBoolean  = packed array[0..High(Integer) div SizeOf(PBoolean ) - 1] of PBoolean;

  TArrayPSingle   = packed array[0..High(Integer) div SizeOf(PSingle  ) - 1] of PSingle;
  TArrayPDouble   = packed array[0..High(Integer) div SizeOf(PDouble  ) - 1] of PDouble;
  TArrayPExtended = packed array[0..High(Integer) div SizeOf(PExtended) - 1] of PExtended;

  TArrayPAnsiChar = packed array[0..High(Integer) div SizeOf(PAnsiChar) - 1] of PAnsiChar;
  TArrayPWideChar = packed array[0..High(Integer) div SizeOf(PWideChar) - 1] of PWideChar;

  // Zeiger auf Arrays
  PArrayByte      = ^TArrayByte    ;
  PArrayWord      = ^TArrayWord    ;
  PArrayLongWord  = ^TArrayLongWord;
  PArrayUInt64    = ^TArrayUInt64  ;

  PArrayShortInt  = ^TArrayShortInt;
  PArraySmallInt  = ^TArraySmallInt;
  PArrayLongInt   = ^TArrayLongInt ;
  PArrayInt64     = ^TArrayInt64   ;

  PArrayCardinal  = ^TArrayCardinal;
  PArrayInteger   = ^TArrayInteger ;

  PArrayBoolean   = ^TArrayBoolean ;

  PArraySingle    = ^TArraySingle  ;
  PArrayDouble    = ^TArrayDouble  ;
  PArrayExtended  = ^TArrayExtended;

  PArrayAnsiChar  = ^TArrayAnsiChar;
  PArrayWideChar  = ^TArrayWideChar;

  // Zeiger auf Pointer-Arrays
  PArrayPointer   = ^TArrayPointer  ;

  PArrayPByte     = ^TArrayPByte    ;
  PArrayPWord     = ^TArrayPWord    ;
  PArrayPLongWord = ^TArrayPLongWord;
  PArrayPUInt64   = ^TArrayPUInt64  ;

  PArrayPShortInt = ^TArrayPShortInt;
  PArrayPSmallInt = ^TArrayPSmallInt;
  PArrayPLongInt  = ^TArrayPLongInt ;
  PArrayPInt64    = ^TArrayPInt64   ;

  PArrayPCardinal = ^TArrayPCardinal;
  PArrayPInteger  = ^TArrayPInteger ;

  PArrayPBoolean  = ^TArrayPBoolean ;

  PArrayPSingle   = ^TArrayPSingle  ;
  PArrayPDouble   = ^TArrayPDouble  ;
  PArrayPExtended = ^TArrayPExtended;

  PArrayPAnsiChar = ^TArrayPAnsiChar;
  PArrayPWideChar = ^TArrayPWideChar;

  // Dynamische Array-Typen
  TDynArrayByte       = array of Byte;
  TDynArrayWord       = array of Word;
  TDynArrayLongWord   = array of LongWord;
  TDynArrayUInt64     = array of UInt64;

  TDynArrayShortInt   = array of ShortInt;
  TDynArraySmallInt   = array of SmallInt;
  TDynArrayLongInt    = array of LongInt;
  TDynArrayInt64      = array of Int64;

  TDynArrayCardinal   = array of Cardinal;
  TDynArrayInteger    = array of Integer;

  TDynArrayBoolean    = array of Boolean;

  TDynArraySingle     = array of Single;
  TDynArrayDouble     = array of Double;
  TDynArrayExtended   = array of Extended;

  TDynArrayAnsiChar   = array of AnsiChar;
  TDynArrayWideChar   = array of WideChar;

  TDynArrayAnsiString = array of AnsiString;
  TDynArrayWideString = array of WideString;

  // Dynamische Pointer Array-Typen
  TDynArrayPointer    = array of Pointer;

  TDynArrayPByte      = array of PByte;
  TDynArrayPWord      = array of PWord;
  TDynArrayPLongWord  = array of PLongWord;
  TDynArrayPUInt64    = array of PUInt64;

  TDynArrayPShortInt  = array of PShortInt;
  TDynArrayPSmallInt  = array of PSmallInt;
  TDynArrayPLongInt   = array of PLongInt;
  TDynArrayPInt64     = array of PInt64;

  TDynArrayPCardinal  = array of PCardinal;
  TDynArrayPInteger   = array of PInteger;

  TDynArrayPBoolean   = array of PBoolean;

  TDynArrayPSingle    = array of PSingle;
  TDynArrayPDouble    = array of PDouble;
  TDynArrayPExtended  = array of PExtended;

  TDynArrayPAnsiChar  = array of PAnsiChar;
  TDynArrayPWideChar  = array of PWideChar;

  // 2-dimensionale Dynamische Arrays
  TDyn2DArrayByte      = array of TDynArrayByte;
  TDyn2DArrayWord      = array of TDynArrayWord;
  TDyn2DArrayLongWord  = array of TDynArrayLongWord;
  TDyn2DArrayUInt64    = array of TDynArrayUInt64;

  TDyn2DArrayShortInt  = array of TDynArrayShortInt;
  TDyn2DArraySmallInt  = array of TDynArraySmallInt;
  TDyn2DArrayLongInt   = array of TDynArrayLongInt;
  TDyn2DArrayInt64     = array of TDynArrayInt64;

  TDyn2DArrayCardinal  = array of TDynArrayCardinal;
  TDyn2DArrayInteger   = array of TDynArrayInteger;

  TDyn2DArrayBoolean   = array of TDynArrayBoolean;

  TDyn2DArraySingle    = array of TDynArraySingle;
  TDyn2DArrayDouble    = array of TDynArrayDouble;
  TDyn2DArrayExtended  = array of TDynArrayExtended;

  TDyn2DArrayAnsiChar  = array of TDynArrayAnsiChar;
  TDyn2DArrayWideChar  = array of TDynArrayWideChar;

  // 2-dimensionale Dynamische Pointer Arrays
  TDyn2DArrayPointer   = array of TDynArrayPointer;
                       
  TDyn2DArrayPByte     = array of TDynArrayPByte;
  TDyn2DArrayPWord     = array of TDynArrayPWord;
  TDyn2DArrayPLongWord = array of TDynArrayPLongWord;
  TDyn2DArrayPUInt64   = array of TDynArrayPUInt64;

  TDyn2DArrayPShortInt = array of TDynArrayPShortInt;
  TDyn2DArrayPSmallInt = array of TDynArrayPSmallInt;
  TDyn2DArrayPLongInt  = array of TDynArrayPLongInt;
  TDyn2DArrayPInt64    = array of TDynArrayPInt64;

  TDyn2DArrayPCardinal = array of TDynArrayPCardinal;
  TDyn2DArrayPInteger  = array of TDynArrayPInteger;

  TDyn2DArrayPBoolean  = array of TDynArrayPBoolean;

  TDyn2DArrayPSingle   = array of TDynArrayPSingle;
  TDyn2DArrayPDouble   = array of TDynArrayPDouble;
  TDyn2DArrayPExtended = array of TDynArrayPExtended;

  TDyn2DArrayPAnsiChar = array of TDynArrayPAnsiChar;
  TDyn2DArrayPWideChar = array of TDynArrayPWideChar;


  // spezielle Array-Typen
  TArrayByte2          = packed array[0..1] of Byte;
  TArrayByte4          = packed array[0..3] of Byte;
  TArrayByte8          = packed array[0..7] of Byte;
  TArrayByte10         = packed array[0..9] of Byte;

  TArrayWord2          = packed array[0..1] of Word;
  TArrayWord4          = packed array[0..3] of Word;
  TArrayWord5          = packed array[0..4] of Word;

  TArrayLongWord1      = packed array[0..0] of LongWord;
  TArrayLongWord2      = packed array[0..1] of LongWord;

  PArrayByte2          = ^TArrayByte2;
  PArrayByte4          = ^TArrayByte4;
  PArrayByte8          = ^TArrayByte8;
  PArrayByte10         = ^TArrayByte10;

  PArrayWord2          = ^TArrayWord2;
  PArrayWord4          = ^TArrayWord4;
  PArrayWord5          = ^TArrayWord5;

  PArrayLongWord1      = ^TArrayLongWord1;
  PArrayLongWord2      = ^TArrayLongWord2;

type
  EYclException = class(Exception)
  public
    {$IFNDEF RTL_SUPPORTS_CREATERES ~}
    constructor CreateRes(ResStringRec: PResStringRec);
    constructor CreateResFmt(ResStringRec: PResStringRec; const Args: array of const);
    {$ENDIF ~RTL_SUPPORTS_CREATERES}
  end;

{$IFNDEF RTL_XPLATFORM ~}
procedure RaiseLastOSError;
{$ENDIF ~RTL_XPLATFORM}

implementation

{$IFNDEF RTL_SUPPORTS_CREATERES ~}
constructor CreateRes(ResStringRec: PResStringRec);
begin
  Message := LoadResString(ResStringRec);
end;

constructor CreateResFmt(ResStringRec: PResStringRec; const Args: array of const);
begin
  Message := Format(LoadResString(ResStringRec), Args);
end;
{$ENDIF ~RTL_SUPPORTS_CREATERES}

{$IFNDEF RTL_XPLATFORM}
procedure RaiseLastOSError;
begin
  RaiseLastWin32Error;
end;
{$ENDIF ~RTL_XPLATFORM}

// *******************************************************************************************

//  History:
//  2005-07-29, Peter J. Haas
//   - move RaiseLastOSError from YclFileUtils to YclBase
//
//  2005-03-05, Peter J. Haas
//   - some modifications to remove FPC hints

end.
