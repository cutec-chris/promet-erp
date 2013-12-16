unit ubenchmark;

{$mode objfpc}{$H+}
{$IFDEF CPU86}
{$asmmode intel}
{$ENDIF}

interface

uses
  Forms,Classes, SysUtils, LCLIntf, Utils, uGeneralStrConsts
{$IFDEF WINDOWS}
  ,Windows
{$ENDIF}
  ;

function CalcCPUSpeed: Extended;
function GetOSVersion : string;
function GetMemoryTransferRate(MbToTest : Integer) : Extended;
procedure GetHardDiskTransferrates(MbToTest: Integer; var ReadRate,WriteRate: Extended);
function GetWhetstone(NLoops : Integer = 50) : Extended;
function GetDrystone(NLoops: Integer = 5000) : Int64;
function GetMemorySize : LongInt;

function GetProcessorRating(WKIPS : Extended;DryTime : Extended) : Extended;
function GetHardDiskRating(fReadTime,fWriteTime : Extended) : Extended;
function GetMemoryRating(TransferRate : Extended) : Extended;


TYPE ARRAY4 = ARRAY [1..4] OF DOUBLE;

  { TCPUBenchThread }

  TCPUBenchThread = class(TThread)
  private
    FDryStoneResultTime: Integer;
    FLoops : Integer;
    FWhetstoneResult: Extended;
  public
    property WhetstoneResult : Extended read FWhetstoneResult;
    property DrystoneResultTime : Integer read FDryStoneResultTime;
    constructor Create(Loops : Integer);
    procedure Execute;override;
  end;

VAR E1                  : ARRAY4;
    T, T1, T2           : DOUBLE;
    J, K, L             : LONGINT;
    ptime, time0, time1 : DOUBLE;

var
(* With loopcount NLoop=10, one million Whetstone instructions
   will be executed in each major loop.
   A major loop is executed 'II' times to increase wall-clock timing accuracy *)
   NLoopValue : Integer;

implementation

function GetMemoryTransferRate(MbToTest : Integer) : Extended;
var
  tm : Int64;
  td : array [0..1023] of byte;
  i: Integer;
begin
  tm := GetTickCount;
  for i := 0 to (MbToTest div 2)*1023 do
    begin
      fillchar(td,1024,0);
      fillchar(td,1024,$ff);
    end;
  tm := 1+GetTickCount-tm;
  Result := 1/(tm/(MbToTest*1000));
end;

procedure GetHardDiskTransferrates(MbToTest: Integer; var ReadRate,
  WriteRate: Extended);
var
  tm : Int64;
  td : array [0..1023] of byte;
  f: File;
  i: Integer;
begin
  fillchar(td,1024,$ff);
  AssignFile(f,GetTempDir+'ubench.tmp');
  Rewrite(f,1024);
  tm := GetTickCount;
  for i := 0 to MbToTest*1023 do
    blockwrite(f,td,1);
  Closefile(f);
  WriteRate := 1/((1+GetTickCount-tm)/(MbToTest*1000));
  Reset(f,1024);
  tm := GetTickCount;
  for i := 0 to MbToTest*1023 do
    blockread(f,td,1);
  ReadRate := 1/((1+GetTickCount-tm)/(MbTotest*1000));
  CloseFile(f);
  SysUtils.DeleteFile(GetTempDir+'ubench.tmp');
end;

function GetOSVersion : string;
{$IFDEF MSWINDOWS}
  function IsWindows64: Boolean;
  type
    TIsWow64Process = function( // Type of IsWow64Process API fn
      Handle: Windows.THandle; var Res: Windows.BOOL
    ): Windows.BOOL; stdcall;
  var
    IsWow64Result: Windows.BOOL;      // Result from IsWow64Process
    IsWow64Process: TIsWow64Process;  // IsWow64Process fn reference
  begin
    // Try to load required function from kernel32
    IsWow64Process := TIsWow64Process(Windows.GetProcAddress(
      Windows.GetModuleHandle('kernel32'), 'IsWow64Process'
    ));
    if Assigned(IsWow64Process) then
    begin
      // Function is implemented: call it
      if not IsWow64Process(
        Windows.GetCurrentProcess, IsWow64Result
      ) then
        raise SysUtils.Exception.Create('IsWindows64: bad process handle');
      // Return result of function
      Result := IsWow64Result;
    end
    else
      // Function not implemented: can't be running on Wow64
      Result := False;
  end;
{$ENDIF}
begin
  result:=strUnknownOS;
{$IFDEF MSWINDOWS}
  case Win32Platform of
    1:// 9x-Reihe
      If Win32MajorVersion=4 Then Begin
        Case Win32MinorVersion of
            0: result:='Windows 95';
            10: result:='Windows 98';
            90: result:='Windows Me';
        end;
      end;
  2: // NT-Reihe
     Case Win32MajorVersion of
         3:IF Win32MinorVersion=51 then
              result:='Windows NT 3.51';
         4:If Win32MinorVersion=0 then
             result:='Windows NT 4';
         5:Case Win32MinorVersion of
              0: result:='Windows 2000';
              1: result:='Windows XP';
              2: result:='Windows .NET Server';
           end;
         6:case Win32MinorVersion of
              0: result := 'Windows Vista';
              1: result := 'Windows 7';
           end;
     End;
  end;
  if IsWindows64 then
    result := result+' 64bit'
  else
    result := result+' 32bit';
  //Win32CSDVersion enthält Informationen zu Servicepacks
  if Win32CSDVersion<>'' then
    result:=result+' '+Win32CSDVersion;
{$ENDIF}
{$IFDEF LINUX}
    result:='Linux';
{$ENDIF}
{$IFDEF DARWIN}
    result:='MacOS(X)';
{$ENDIF}
end;

function CalcCPUSpeed: Extended;
const
  DelayTime = 500; // measure time in ms
var
  TimerHi, TimerLo: DWord;
  PriorityClass, Priority: Integer;
begin
  Result := -2;
{$IFDEF CPU86}
{$IFDEF WINDOWS}
  try
    PriorityClass := GetPriorityClass(GetCurrentProcess);
    Priority := GetThreadPriority(GetCurrentThread);

    SetPriorityClass(GetCurrentProcess, REALTIME_PRIORITY_CLASS);
    SetThreadPriority(GetCurrentThread,
                      THREAD_PRIORITY_TIME_CRITICAL);
    try
      Sleep(10);
      asm
        dw 310Fh // rdtsc
        mov TimerLo, eax
        mov TimerHi, edx
      end;
      Sleep(DelayTime);
      asm
        dw 310Fh // rdtsc
        sub eax, TimerLo
        sbb edx, TimerHi
        mov TimerLo, eax
        mov TimerHi, edx
      end;
    finally
      SetThreadPriority(GetCurrentThread, Priority);
      SetPriorityClass(GetCurrentProcess, PriorityClass);
    end;
    Result := TimerLo / (1000.0 * DelayTime);
  except
    Result := -2;
  end;
{$ENDIF}
{$ELSE}
  Result := -1;
{$ENDIF}
end;


//Whetstone Test

PROCEDURE PA (VAR E : ARRAY4);
VAR J1 : LONGINT;
BEGIN
        J1 := 0;
        REPEAT
                E [1] := ( E [1] + E [2] + E [3] - E [4]) * T;
                E [2] := ( E [1] + E [2] - E [3] + E [4]) * T;
                E [3] := ( E [1] - E [2] + E [3] + E [4]) * T;
                E [4] := (-E [1] + E [2] + E [3] + E [4]) / T2;
                J1 := J1 + 1;
        UNTIL J1 >= 6;
END;

PROCEDURE P0;
BEGIN
        E1 [J] := E1 [K]; E1 [K] := E1 [L]; E1 [L] := E1 [J];
END;

PROCEDURE P3 (X,Y : DOUBLE; VAR Z : DOUBLE);
VAR X1, Y1 : DOUBLE;
BEGIN
        X1 := X;
        Y1 := Y;
        X1 := T * (X1 + Y1);
        Y1 := T * (X1 + Y1);
        Z := (X1 + Y1)/T2;
END;

function DoWhetstone : Longint;
VAR NLoop, I, II, JJ : LONGINT;
    N1, N2, N3, N4, N5, N6, N7, N8, N9, N10, N11 : LONGINT;
    X1, X2, X3, X4, X, Y, Z : DOUBLE;
BEGIN
(* The actual benchmark starts here. *)
        T  := 0.499975;
        T1 := 0.50025;
        T2 := 2.0;
        NLoop := NLoopValue;
        II    := 400;
        FOR JJ:=1 TO II DO BEGIN
(* Establish the relative loop counts of each module. *)
                N1 := 0;
                N2 := 12 * NLoop;
                N3 := 14 * NLoop;
                N4 := 345 * NLoop;
                N5 := 0;
                N6 := 210 * NLoop;
                N7 := 32 * NLoop;
                N8 := 899 * NLoop;
                N9 := 616 * NLoop;
                N10 := 0;
                N11 := 93 * NLoop;
(* Module 1: Simple identifiers *)
                X1 := 1.0;
                X2 := -1.0;
                X3 := -1.0;
                X4 := -1.0;
                FOR I:=1 TO N1 DO BEGIN
                        X1 := (X1 + X2 + X3 - X4)*T;
                        X2 := (X1 + X2 - X3 + X4)*T;
                        X3 := (X1 - X2 + X3 + X4)*T;
                        X4 := (-X1 + X2 + X3 + X4)*T;
                END;
//                IF (JJ = II) THEN BEGIN
//                        POUT (N1, N1, N1, X1, X2, X3, X4);
//                END;
(* Module 2: Array elements *)
                E1 [1] :=  1.0;
                E1 [2] := -1.0;
                E1 [3] := -1.0;
                E1 [4] := -1.0;
                FOR I:=1 TO N2 DO BEGIN
                        E1 [1] := (E1 [1] + E1 [2] + E1 [3] - E1 [4])*T;
                        E1 [2] := (E1 [1] + E1 [2] - E1 [3] + E1 [4])*T;
                        E1 [3] := (E1 [1] - E1 [2] + E1 [3] + E1 [4])*T;
                        E1 [4] := (-E1 [1] + E1 [2] + E1 [3] + E1 [4])*T;
                END;
//                IF (JJ = II) THEN BEGIN
//                        POUT (N2, N3, N2, E1 [1], E1 [2], E1 [3], E1 [4]);
//                END;
(* Module 3: Array as parameter *)
                FOR I:=1 TO N3 DO BEGIN
                        PA (E1);
                END;
//                IF (JJ = II) THEN BEGIN
//                        POUT(N3, N2, N2, E1 [1], E1 [2], E1 [3], E1 [4]);
//                END;
(* Module 4: Conditional jumps *)
                J := 1;
                FOR I:=1 TO N4 DO BEGIN
                        IF (J <> 1) THEN J := 3 ELSE J := 2;
                        IF (J <= 2) THEN J := 1 ELSE J := 0;
                        IF (J >= 1) THEN J := 0 ELSE J := 1;
                END;
//                IF (JJ = II) THEN BEGIN
//                        POUT (N4, J, J, X1, X2, X3, X4)
//                END;
(* Module 5: Omitted; Module 6: Integer arithmetic *)
                J := 1;
                K := 2;
                L := 3;
                FOR I:=1 TO N6 DO BEGIN
                        J := J * (K-J) * (L-K);
                        K := L * K - (L-J) * K;
                        L := (L - K) * (K + J);
                        E1 [L-1] := (J + K + L);
                        E1 [K-1] := (J * K * L);
                END;
//                IF (JJ = II) THEN BEGIN
//                        POUT (N6, J, K, E1 [1], E1 [2], E1 [3], E1 [4]);
//                END;
(* Module 7: Trigonometric functions *)
                X := 0.5;
                Y := 0.5;
                FOR I:=1 TO N7 DO BEGIN
                        X:=T*arctan(T2*sin(X)*cos(X)/(cos(X+Y)+cos(X-Y)-1.0));
                        Y:=T*arctan(T2*sin(Y)*cos(Y)/(cos(X+Y)+cos(X-Y)-1.0));
                END;
//                IF (JJ = II) THEN BEGIN
//                        POUT (N7, J, K, X, X, Y, Y);
//                END;
(* Module 8: Procedure calls *)
                X := 1.0;
                Y := 1.0;
                Z := 1.0;
                FOR I:=1 TO N8 DO BEGIN
                        P3 (X,Y,Z);
                END;
//                IF (JJ = II) THEN BEGIN
//                        POUT (N8, J, K, X, Y, Z, Z);
//                END;
(* Module 9: Array references *)
                J := 1;
                K := 2;
                L := 3;
                E1 [1] := 1.0;
                E1 [2] := 2.0;
                E1 [3] := 3.0;
                FOR I:=1 TO N9 DO BEGIN
                        P0;
                END;
//                IF (JJ = II) THEN BEGIN
//                        POUT (N9, J, K, E1 [1], E1 [2], E1 [3], E1 [4])
//                END;
(* Module 10: Integer arithmetic *)
                J := 2;
                K := 3;
                FOR I:=1 TO N10 DO BEGIN
                        J := J + K;
                        K := J + K;
                        J := K - J;
                        K := K - J - J;
                END;
//                IF (JJ = II) THEN BEGIN
//                        POUT (N10, J, K, X1, X2, X3, X4)
//                END;
(* Module 11: Standard functions *)
                X := 0.75;
                FOR I:=1 TO N11 DO BEGIN
                  X := sqrt (exp (ln (X)/T1))
                  // x:=sqrt(x);
                END;
//                IF (JJ = II) THEN BEGIN
//                        POUT (N11, J, K, X, X, X, X)
//                END;
(* THIS IS THE END OF THE MAJOR LOOP. *)
        END;
  Result := II;
END;

function GetWhetstone(NLoops: Integer): Extended;
var
  tm : Int64;
  CPUTime : Int64;
  II: LongInt;
begin
  NLoopValue := NLoops;
  tm := GetTickCount;
  II := DoWhetstone;
  CPUTime := GetTickCount-tm;
  Result := 100.0*NLoopValue*II* 1000/CPUTime;
end;


//Drystone
{$R- range checking off}

var
  LOOPS : Integer;      { Use this for slow or 16 bit machines }


CONST

{    Set LOOPS to specify how many thousand drystones to perform.
      LOOPS = 50 will perforum 50,000 drystones. Choose longer for
      better precision and for fast machines.
}

  Ident1 = 1;
  Ident2 = 2;
  Ident3 = 3;
  Ident4 = 4;
  Ident5 = 5;

type integer = longint;
Type Enumeration = INTEGER;
{ TYPE Enumeration = (Ident1, Ident2, Ident3, Ident4, Ident5); }

TYPE   OneToThirty   = INTEGER;
TYPE   OneToFifty    = INTEGER;
TYPE   CapitalLetter = CHAR;
TYPE   String30      = STRING[30]; { ARRAY[0..30] OF CHAR; }
TYPE   Array1Dim     = ARRAY[0..50] OF INTEGER;
TYPE   Array2Dim     = ARRAY[0..50,0..50] OF INTEGER;

{ TYPE   RecordPtr     = ^RecordType; }
       RecordType    = RECORD
                         PtrComp    : integer;
                         Discr      : Enumeration;
                         EnumComp   : Enumeration;
                         IntComp    : OneToFifty;
                         StringComp : String30;
                       END;

{
 * Package 1
 }
VAR
  IntGlob    : INTEGER;
  BoolGlob   : BOOLEAN;
  Char1Glob  : CHAR;
  Char2Glob  : CHAR ;
  Array1Glob : Array1Dim;
  Array2Glob : Array2Dim;
  MyRec      : array[0..2] of RecordType;
{  PtrGlb     : RecordPtr; }
{  PtrGlbNext : RecordPtr; }

  Hour, Min, Sec, Hund : word;
  TStart, TEnd : real;

CONST
  PtrGlb     = 1;
  PtrGlbNext = 2;

PROCEDURE Proc7(IntParI1, IntParI2 : OneToFifty; VAR IntParOut : OneToFifty);
VAR
   IntLoc  : OneToFifty;
BEGIN
   IntLoc:= IntParI1 + 2;
   IntParOut:= IntParI2 + IntLoc;
END ;

PROCEDURE Proc3( var inRecIdx : integer );
BEGIN
   IF ( inRecIdx <> 0 ) THEN
      inRecIdx := MyRec[PtrGlb].PtrComp
   ELSE
      IntGlob:= 100;
   Proc7( 10, IntGlob, MyRec[PtrGlb].IntComp);
END ;

FUNCTION Func3(EnumParIn : Enumeration) : BOOLEAN;
  VAR EnumLoc: Enumeration;
BEGIN
   EnumLoc:= EnumParIn;
   Func3:= EnumLoc = Ident3;
END ;

PROCEDURE Proc6(EnumParIn : Enumeration; VAR EnumParOut : Enumeration);
BEGIN
   EnumParOut:= EnumParIn;
   IF (NOT Func3(EnumParIn) ) THEN
      EnumParOut:= Ident4;
   CASE EnumParIn OF
    Ident1:   EnumParOut:= Ident1 ;
    Ident2:   IF (IntGlob > 100) THEN EnumParOut:= Ident1
                                 ELSE EnumParOut:= Ident4;
    Ident3:   EnumParOut:= Ident2 ;
    Ident4:   ;
    Ident5:   EnumParOut:= Ident3;
   END;
END ;


PROCEDURE Proc1( inIdx : integer );
var
   i : integer;
BEGIN
   i := MyRec[inIdx].PtrComp;

   MyRec[i] := MyRec[PtrGlb];
   MyRec[inIdx].IntComp := 5;
   MyRec[i].IntComp:= MyRec[inIdx].IntComp;
   MyRec[i].PtrComp:= i;
   Proc3( MyRec[i].PtrComp );
   IF ( MyRec[i].Discr = Ident1 ) THEN
      BEGIN
         MyRec[i].IntComp:= 6;
         Proc6( MyRec[inIdx].EnumComp, MyRec[i].EnumComp );
         MyRec[i].PtrComp:= MyRec[PtrGlb].PtrComp;
         Proc7( MyRec[i].IntComp, 10, MyRec[i].IntComp );
      END
   ELSE
      MyRec[inIdx] := MyRec[i];
END;


PROCEDURE Proc2(VAR IntParIO : OneToFifty);
VAR
   IntLoc  : OneToFifty;
   EnumLoc : Enumeration;
BEGIN
   IntLoc:= IntParIO + 10;
   REPEAT
     IF (Char1Glob = 'A') THEN
      BEGIN
         IntLoc:= IntLoc - 1;
         IntParIO:= IntLoc - IntGlob;
         EnumLoc:= Ident1;
      END;
   UNTIL EnumLoc = Ident1;
END ;

PROCEDURE Proc4;
VAR
   BoolLoc : BOOLEAN;
BEGIN
   BoolLoc:= Char1Glob = 'A';
   BoolLoc:= BoolLoc OR BoolGlob;
   Char2Glob:= 'B';
END ;

PROCEDURE Proc5;
BEGIN
   Char1Glob:= 'A';
   BoolGlob:= FALSE;
END ;

PROCEDURE Proc8(VAR Array1Par : Array1Dim; VAR Array2Par : Array2Dim;
      IntParI1, IntParI2 : OneToFifty);
VAR
   IntLoc   : OneToFifty;
   IntIndex : OneToFifty;
BEGIN
   IntLoc:= IntParI1 + 5;
   Array1Par[IntLoc]:= IntParI2;
   Array1Par[IntLoc+1]:= Array1Par[IntLoc];
   Array1Par[IntLoc+30]:= IntLoc;
   FOR IntIndex:= IntLoc TO (IntLoc+1) DO
      Array2Par[IntLoc,IntIndex]:= IntLoc;
   { Array2Par[IntLoc,IntLoc-1]:= Array2Par[IntLoc,IntLoc-1] + 1; }
   Array2Par[IntLoc+20,IntLoc]:= Array1Par[IntLoc];
   IntGlob:= 5;
END ;

FUNCTION Func1(CharPar1, CharPar2 : CapitalLetter) : Enumeration;
VAR
   CharLoc1, CharLoc2 : CapitalLetter;
BEGIN
   CharLoc1:= CharPar1;
   CharLoc2:= CharLoc1;
   IF (CharLoc2 <> CharPar2) THEN
      Func1:= (Ident1)
   ELSE
      Func1:= (Ident2);
END ;

FUNCTION Func2(VAR StrParI1, StrParI2 : String30) : BOOLEAN;
VAR
   IntLoc   : OneToThirty;
   CharLoc  : CapitalLetter;
BEGIN
   IntLoc := 2;
   WHILE (IntLoc <= 2) DO
    BEGIN
     IF (Func1(StrParI1[IntLoc], StrParI2[IntLoc+1]) = Ident1) THEN
       BEGIN
         CharLoc := 'A';
         IntLoc:= IntLoc + 1;
       END;
    END;
   IF (CharLoc >= 'W') AND (CharLoc <= 'Z') THEN IntLoc:= 7;
   IF CharLoc = 'X' THEN
     Func2:= TRUE
   ELSE IF StrParI1 > StrParI2 THEN
    BEGIN
     IntLoc:= IntLoc + 7;
     Func2:= TRUE;
    END
   ELSE
     Func2:= FALSE;
END ;


PROCEDURE Proc0;
VAR
   IntLoc1    : OneToFifty;
   IntLoc2    : OneToFifty;
   IntLoc3    : OneToFifty;
   CharLoc    : CHAR;
   CharIndex  : CHAR;
   EnumLoc    : Enumeration;
   String1Loc,
   String2Loc : String30;
   i,
   j          : INTEGER;

BEGIN
{
   NEW(PtrGlbNext);
   NEW(PtrGlb);
}

   MyRec[PtrGlb].PtrComp:= PtrGlbNext;
   MyRec[PtrGlb].Discr:= Ident1;
   MyRec[PtrGlb].EnumComp:= Ident3;
   MyRec[PtrGlb].IntComp:= 40;
   MyRec[PtrGlb].StringComp := 'DHRYSTONE PROGRAM, SOME STRING';

   String1Loc := 'DHRYSTONE PROGRAM, 1''ST STRING';

FOR i := 1 TO LOOPS DO
  FOR j := 1 TO 1000 DO
  BEGIN
   Proc5;
   Proc4;
   IntLoc1:= 2;
   IntLoc2:= 3;
   String2Loc := 'DHRYSTONE PROGRAM, 2''ND STRING';
   EnumLoc:= Ident2;
   BoolGlob:= NOT Func2(String1Loc, String2Loc);
   WHILE (IntLoc1 < IntLoc2) DO
    BEGIN
      IntLoc3 := 5 * IntLoc1 - IntLoc2;
      Proc7(IntLoc1, IntLoc2, IntLoc3);
      IntLoc1:= IntLoc1 + 1;
    END;
   Proc8(Array1Glob, Array2Glob, IntLoc1, IntLoc3);
   Proc1(PtrGlb);
   CharIndex:= 'A';
   WHILE  CharIndex <= Char2Glob DO
     BEGIN
      IF (EnumLoc = Func1(CharIndex, 'C')) THEN
         Proc6(Ident1, EnumLoc);
      { CharIndex:= SUCC(CharIndex); }
      inc(byte(charindex));
     END;
   IntLoc3:= IntLoc2 * IntLoc1;
   IntLoc2:= IntLoc3 DIV IntLoc1;
   IntLoc2:= 7 * (IntLoc3 - IntLoc2) - IntLoc1;
   Proc2(IntLoc1);
 END;
END;

function GetDrystone(NLoops: Integer = 5000) : Int64;
var
  tm: Int64;
  CPUTime: Int64;
begin
  LOOPS := NLoops;
  tm := GetTickCount;
  Proc0;
  CPUTime := GetTickCount-tm;
  Result := CPUTime;
end;

function GetMemorySize: LongInt;
{$IFDEF WINDOWS}
var
  Memory: TMemoryStatus;
  r: Extended;
{$ENDIF}
begin
  Result := 0;
{$IFDEF WINDOWS}
  Memory.dwLength := SizeOf(Memory);
  GlobalMemoryStatus(Memory);
  Result := Memory.dwTotalPhys;
{$ENDIF}
end;

function GetProcessorRating(WKIPS: Extended; DryTime: Extended): Extended;
begin
  Result := ((((1/WKIPS)*1000000)/2.3)+((DryTime*1.4)*0.8))*0.5;
end;

function GetHardDiskRating(fReadTime, fWriteTime: Extended): Extended;
begin
  Result := ((fReadTime/400)+(fWriteTime/15))/2;
end;

function GetMemoryRating(TransferRate: Extended): Extended;
begin
  Result := TransferRate/5000;
end;

{ TCPUBenchThread }

constructor TCPUBenchThread.Create(Loops: Integer);
begin
  FLoops := Loops;
  FWhetstoneResult := -1;
  FDrystoneResultTime := -1;
  Priority := tpTimeCritical;
  inherited Create(False);
end;

procedure TCPUBenchThread.Execute;
begin
  FWhetstoneResult := GetWhetstone(FLoops);
  FDrystoneResultTime := GetDryStone(FLoops*20);
end;

end.

