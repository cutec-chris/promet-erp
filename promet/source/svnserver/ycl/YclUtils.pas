{**************************************************************************************************}
{                                                                                                  }
{  Y core library (Ycl)                                                                            }
{                                                                                                  }
{  Useful utility functions                                                                        }
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

{$I Ycl.inc}

unit YclUtils;

interface

procedure SwapVar(var V1, V2: Integer); overload;
procedure SwapVar(var V1, V2: Single); overload;
procedure SwapVar(var V1, V2: Double); overload;

procedure Sort2Values(var V1, V2: Integer); overload;
procedure Sort2Values(var V1, V2: Single); overload;
procedure Sort2Values(var V1, V2: Double); overload;

// Quotient := Dividend div Divisor; Remainder := Dividend mod Divisor
procedure DivMod(const Dividend, Divisor: Cardinal; out Quotient, Remainder: Cardinal);

// Quotient := Dividend div Divisor; Remainder := Dividend mod Divisor
// Quotient zu Null hin gerundet, Remainder hat gleiches Vorzeichen wie Dividend
procedure IDivMod(const Dividend, Divisor: Integer; out Quotient, Remainder: Integer);

// Quotient := Dividend div Divisor; Remainder := Dividend mod Divisor
// Quotient abgerundet, Remainder immer positiv
procedure IDivPMod(const Dividend, Divisor: Integer; out Quotient: Integer; out Remainder: Cardinal);

// Result := Dividend mod Divisor
// Remainder immer positiv
function PMod(const Dividend, Divisor: Integer): Cardinal;

// Fill the remaining byte from Offset to DataSize
procedure FillRemainBytes(var Data; DataSize: Integer; Offset: Integer; Value: Byte);

// Copy Count bytes from Src to Dst and return a pointer to the end of dst buffer  
function CopyMemE(Src, Dst: Pointer; Count: Cardinal): Pointer;

implementation
uses
  YclBase;
  
procedure SwapVar(var V1, V2: Integer);
var
  Temp: Integer;
begin
  Temp := V1;
  V1 := V2;
  V2 := Temp;
end;

procedure SwapVar(var V1, V2: Single);
var
  Temp: Single;
begin
  Temp := V1;
  V1 := V2;
  V2 := Temp;
end;

procedure SwapVar(var V1, V2: Double);
var
  Temp: Double;
begin
  Temp := V1;
  V1 := V2;
  V2 := Temp;
end;

procedure Sort2Values(var V1, V2: Integer);
begin
  if V1 > V2 then
    SwapVar(V1, V2);
end;

procedure Sort2Values(var V1, V2: Single);
begin
  if V1 > V2 then
    SwapVar(V1, V2);
end;

procedure Sort2Values(var V1, V2: Double);
begin
  if V1 > V2 then
    SwapVar(V1, V2);
end;

// Quotient := Dividend div Divisor; Remainder := Dividend mod Divisor
{$IFDEF CPUI386}
// In: EAX Dividend, EDX Divisor, [ECX] Quotient
procedure DivMod(const Dividend, Divisor: Cardinal; out Quotient, Remainder: Cardinal); register; assembler;
asm
        PUSH    EBX
        MOV     EBX, EDX
        XOR     EDX, EDX
        DIV     EBX             // EAX := EDX:EAX div EBX, EDX := EDX:EAX mod EBX
        MOV     [ECX], EAX
        MOV     ECX, Remainder
        MOV     [ECX], EDX
        POP     EBX
end;
{$ELSE CPUI386~}
procedure DivMod(const Dividend, Divisor: Cardinal; out Quotient, Remainder: Cardinal);
begin
  Quotient := Dividend div Divisor;
  Remainder := Dividend mod Divisor;
end;
{$ENDIF ~CPUI386}

// Quotient := Dividend div Divisor; Remainder := Dividend mod Divisor
// Quotient zu Null hin gerundet, Remainder hat gleiches Vorzeichen wie Dividend
{$IFDEF CPUI386}
// In: EAX Dividend, EDX Divisor, [ECX] Quotient
procedure IDivMod(const Dividend, Divisor: Integer; out Quotient, Remainder: Integer); register; assembler;
asm
        PUSH    EBX
        MOV     EBX, EDX
        CDQ
        IDIV    EBX             // EAX := EDX:EAX div EBX, EDX := EDX:EAX mod EBX
        MOV     [ECX], EAX
        MOV     ECX, Remainder
        MOV     [ECX], EDX
        POP     EBX
end;
{$ELSE CPUI386~}
procedure IDivMod(const Dividend, Divisor: Integer; out Quotient, Remainder: Integer);
begin
  Quotient := Dividend div Divisor;
  Remainder := Dividend mod Divisor;
end;
{$ENDIF ~CPUI386}

// Quotient := Dividend div Divisor; Remainder := Dividend mod Divisor
// Quotient abgerundet, Remainder immer positiv
{$IFDEF CPUI386}
// In: EAX Dividend, EDX Divisor, [ECX] Quotient
procedure IDivPMod(const Dividend, Divisor: Integer; out Quotient: Integer; out Remainder: Cardinal); register; assembler;
asm
        PUSH    EBX
        MOV     EBX, EDX
        CDQ
        IDIV    EBX             // EAX := EDX:EAX div EBX, EDX := EDX:EAX mod EBX
        TEST    EDX, EDX
        JNS     @@01
        SUB     EAX, 1
        ADD     EDX, EBX
@@01:   MOV     [ECX], EAX
        MOV     ECX, Remainder
        MOV     [ECX], EDX
        POP     EBX
end;
{$ELSE CPUI386~}
procedure IDivPMod(const Dividend, Divisor: Integer; out Quotient: Integer; out Remainder: Cardinal);
begin
  Quotient := Dividend div Divisor;
  Remainder := Dividend mod Divisor;
  if Remainder < 0 then begin
    Remainder := Remainder + Divisor;
    Quotient := Quotient - 1;
  end;
end;
{$ENDIF ~CPUI386}

// Result := Dividend mod Divisor
// Remainder immer positiv
{$IFDEF CPUI386}
// in: EAX Dividend, EDX Divisor
// out: EAX Remainder
function PMod(const Dividend, Divisor: Integer): Cardinal; register; assembler;
asm
        MOV     ECX, EDX
        CDQ
        IDIV    ECX             // EAX := EDX:EAX div ECX, EDX := EDX:EAX mod ECX
        MOV     EAX, EDX
        TEST    EAX, EAX
        JNS     @@01
        ADD     EAX, ECX
@@01:
end;
{$ELSE CPUI386~}
function PMod(const Dividend, Divisor: Integer): Cardinal;
var
  Remainder: Integer;
begin
  Remainder := Dividend mod Divisor;
  if Remainder < 0 then
    Remainder := Remainder + Divisor;
  Result := Remainder;
end;
{$ENDIF ~CPUI386}

// Fill the remaining byte from Offset to DataSize
procedure FillRemainBytes(var Data; DataSize: Integer; Offset: Integer; Value: Byte);
var
  P: PByte;
  Count: Integer;
begin
  Count := DataSize - Offset;
  if Count > 0 then
  begin
    P := @Data;
    Inc(P, Offset);
    FillChar(P^, Count, Value);
  end;
end;

// Copy Count bytes from Src to Dst and return a pointer to the end of dst buffer
{$IFDEF CPUI386}

function CopyMemE(Src, Dst: Pointer; Count: Cardinal): Pointer; register; assembler;
asm
        PUSH    EDI
        PUSH    ESI
        MOV     ESI,EAX
        MOV     EDI,EDX
        MOV     EDX,ECX        // Anzahl retten
        SHR     ECX,2          // volle 32 Bit Gruppen kopieren
        REP     MOVSD
        MOV     ECX,EDX        // restliche Bytes kopieren
        AND     ECX,3
        REP     MOVSB
        MOV     EAX,EDI
        POP     ESI
        POP     EDI
end;
{$ELSE CPUI386~}
function CopyMemE(Src, Dst: Pointer; Count: Cardinal): Pointer;
begin
  Move(Src^, Dst^, Count);
  Result := Dst;
  Inc(PByte(Result), Count);
end;
{$ENDIF ~CPUI386}

// *******************************************************************************************

//  History:
//  2005-08-27, Peter J. Haas
//   - add FillRemainBytes, CopyMemE

end.
