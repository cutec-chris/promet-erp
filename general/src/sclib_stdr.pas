unit sclib_stdr;

{$MODE DELPHI}

interface
uses
  SC_Utils, SC_Comp, SC_Exec, SC_Parsers;

{ This function registers all standard functions. 
  Call this function before loading your script into the executer.
  } 
procedure RegisterStandardLibrary_R(S: TSCExec);

implementation

type
  TMYExec = class (TSCExec) end;

function Trim(const s: string): string;
begin
  Result := s;
  while (Length(result) > 0) and (Result[1] = #32) do Delete(Result, 1, 1);
  while (Length(result) > 0) and (Result[Length(Result)] = #32) do Delete(Result, Length(Result), 1);
end;
function FloatToStr(E: Extended): string;
var
  s: string;
begin
  Str(e:0:12, s);
  result := s;
end;
//-------------------------------------------------------------------

function Padl(s: string; i: longInt): string;
begin
  result := StringOfChar(' ', i - length(result)) + s;
end;
//-------------------------------------------------------------------

function Padz(s: string; i: longInt): string;
begin
  result := StringOfChar('0', i - length(result)) + s;
end;
//-------------------------------------------------------------------

function Padr(s: string; i: longInt): string;
begin
  result := s + StringOfChar(' ', i - Length(s));
end;
//-------------------------------------------------------------------

function VarProc(Caller: TSCExec; p: PIFProcRec; Global, Stack: TSCList): Boolean;
var
  PStart: Cardinal;
  Pp: PIFVariant;
begin
  if p^.Ext1 = Pointer(0) then
  begin
    PStart := Stack.Count -2;
    pp := Stack.GetItem(PStart);
    if (pp = nil) or (pp^.FType^.BaseType <> btVariant) then begin
      Result := False;
      Exit;
    end;
    Inc(PStart);
    if pp^.tvariant^.FType = nil then LSetInt(Stack, PStart, 0) else
    case pp^.TVariant^.FType^.BaseType of
      btU8: LSetInt(Stack, PStart, 8);
      btS8: LSetInt(Stack, PStart, 7);
      btU16: LSetInt(Stack, PStart, 6);
      btS16: LSetInt(Stack, PStart, 5);
      btU32: LSetInt(Stack, PStart, 4);
      btS32: LSetInt(Stack, PStart, 3);
      btSingle: LSetInt(Stack, PStart, 9);
      btDouble: LSetInt(Stack, PStart, 10);
      btExtended: LSetInt(Stack, PStart, 11);
      btPChar, btString: LSetInt(Stack, PStart, 1);
      btRecord: LSetInt(Stack, PStart, 14);
      btArray: LSetInt(Stack, PStart, 13);
      btResourcePointer: LSetInt(Stack, PStart, 12);
      {$IFNDEF NOINT64}
      btS64: LSetInt(Stack, PStart, 2);
      {$ENDIF}
    else
      LSetInt(Stack, PStart, 0);
    end;
    Result := True;
  end else if p^.Ext1 = Pointer(1) then
  begin
    Pp := Stack.GetItem(Stack.Count-1);
    if (pp = nil) or (pp^.FType^.BaseType <> btVariant) then
    begin
      Result := False;
      exit;
    end;
    ChangeVariantType({$IFNDEF NOSMARTMM}caller.MemoryManager, {$ENDIF} pp^.tVariant, nil);
    Result := True;
  end else begin
    Result := False;
  end;
end;


function DefProc(Caller: TSCExec; p: PIFProcRec; Global, Stack: TSCList): Boolean;
var
  PStart: Cardinal;
  temp: PIfVariant;
  I: Longint;
  b: Boolean;
  E: Extended;
begin
  case Longint(p^.Ext1) of
    0: // inttostr
      begin
        PStart := Stack.Count - 2;
        LSetStr(Stack, PStart + 1, IntToStr(LGetInt(Stack, PStart)));
        Result := True;
      end;
    1: // strtoint
      begin
        PStart := Stack.Count - 2;
        LSetInt(Stack, PStart+1, StrToInt(LGetStr(Stack, PStart)));
        Result := True;
      end;
    2: // strtointdef
      begin
        PStart := Stack.Count - 3;
        LSetInt(Stack, PStart+2, StrToIntDef(LGetStr(Stack, PStart + 1), LGetInt(Stack, PStart)));
        Result := True;
      end;
    3: // pos
      begin
        PStart := Stack.Count - 3;
        LSetInt(Stack, PStart+2,Pos(LGetStr(Stack, PStart+1), LGetStr(Stack, PStart)));
        Result := True;
      end;
    4: // copy
      begin
        PStart := Stack.Count - 4;
        LSetStr(Stack, PStart + 3,Copy(LGetStr(Stack, PStart+2), LGetInt(Stack, PStart + 1), LGetInt(Stack, PStart)));
        Result := True;
      end;
    5: //delete
      begin
        PStart := Stack.Count - 3;
        temp := Stack.GetItem(PStart + 2);
        if (temp = nil) or (temp^.FType^.BaseType <> btString) then begin
          Result := False;
          exit;
        end;
        Delete(string(temp^.tstring), LGetInt(Stack, PStart + 1), LGetInt(Stack, PStart));
        Result := True;
      end;
    6: // insert
      begin
        PStart := Stack.Count - 3;
        temp := Stack.GetItem(PStart + 1);
        if (temp = nil) or (temp^.FType^.BaseType <> btString) then begin
          Result := False;
          exit;
        end;
        Insert(LGetStr(Stack, PStart + 2), string(temp^.tstring), LGetInt(Stack, PStart + 0));
        Result := True;
      end;
    7: // StrGet
      begin
        PStart := Stack.Count - 3;
        temp := Stack.GetItem(PStart + 1);
        if (temp = nil) or (temp^.FType^.BaseType <> btString) then begin
          Result := False;
          exit;
        end;
        I := LGetInt(Stack, PStart);
        if (i<1) or (i>length(string(temp^.tstring))) then
        begin
          Caller.CMD_Err2(erCustomError, 'Out Of String Range');
          Result := False;
          exit;
        end;
        LSetInt(Stack, PStart +2, Ord(string(temp^.tstring)[i]));
        Result := True;
      end;
    8: // StrSet
      begin
        PStart := Stack.Count - 3;
        temp := Stack.GetItem(PStart);
        if (temp = nil) or (temp^.FType^.BaseType <> btString) then begin
          Result := False;
          Caller.CMD_Err2(erCustomError, 'Invalid Type');
          exit;
        end;
        I := LGetInt(Stack, PStart + 1);
        if (i<1) or (i>length(string(temp^.tstring))) then
        begin
          Caller.CMD_Err2(erCustomError, 'Out Of String Range');
          Result := True;
          exit;
        end;
        string(temp^.tstring)[i] := chr(LGetInt(Stack, PStart + 2));
        Result := True;
      end;
    10: // Uppercase
      begin
        PStart := STack.Count -2;
        LSetStr(Stack, PStart + 1, FastUpperCase(LGetStr(Stack, PStart)));
        Result := True;
      end;
    11: // LowerCase
      begin
        PStart := STack.Count -2;
        LSetStr(Stack, PStart + 1, FastLowercase(LGetStr(Stack, PStart)));
        Result := True;
      end;
    12: // Trim
      begin
        PStart := STack.Count -2;
        LSetStr(Stack, PStart + 1, Trim(LGetStr(Stack, PStart)));
        Result := True;
      end;
    13: // Length
      begin
        PStart := Stack.Count - 2;
        LSetInt(Stack, PStart + 1, Length(LGetStr(Stack, PStart)));
        Result := True;
      end;
    14: // SetLength
      begin
        PStart := Stack.Count - 2;
        temp := Stack.GetItem(PStart+1);
        if (temp = nil) or (temp^.FType^.BaseType <> btString) then begin
          Result := False;
          exit;
        end;
        SetLength(string(temp^.tstring), LGetInt(Stack, PStart));
        Result := True;
      end;
    15: // Sin
      begin
        PStart := Stack.Count - 2;
        try
          LSetReal(Stack, PStart + 1, Sin(LGetReal(Stack, PStart)));
        except
          Caller.CMD_Err2(erCustomError, 'Floating Point Exception');
        end;
        Result := True;
      end;
    16: // Cos
      begin
        PStart := Stack.Count - 2;
        try
          LSetReal(Stack, PStart + 1, Cos(LGetReal(Stack, PStart)));
        except
          Caller.CMD_Err2(erCustomError, 'Floating Point Exception');
        end;
        Result := True;
      end;
    17: // Sqrt
      begin
        PStart := Stack.Count - 2;
        try
          LSetReal(Stack, PStart + 1, Sqrt(LGetReal(Stack, PStart)));
        except
          Caller.CMD_Err2(erCustomError, 'Floating Point Exception');
        end;
        Result := True;
      end;
    18: // Round
      begin
        PStart := Stack.Count - 2;
        try
          LSetInt(Stack, PStart + 1, Round(LGetReal(Stack, PStart)));
        except
          Caller.CMD_Err2(erCustomError, 'Floating Point Exception');
        end;
        Result := True;
      end;
    19: // Trunc
      begin
        PStart := Stack.Count - 2;
        try
          LSetInt(Stack, PStart + 1, Trunc(LGetReal(Stack, PStart)));
        except
          Caller.CMD_Err2(erCustomError, 'Floating Point Exception');
        end;
        Result := True;
      end;
    20: // Int
      begin
        PStart := Stack.Count - 2;
        try
          LSetReal(Stack, PStart + 1, Int(LGetReal(Stack, PStart)));
        except
          Caller.CMD_Err2(erCustomError, 'Floating Point Exception');
        end;
        Result := True;
      end;
    21: // Pi
      begin
        PStart := Stack.Count - 1;
        try
          LSetReal(Stack, PStart, PI);
        except
          Caller.CMD_Err2(erCustomError, 'Floating Point Exception');
        end;
        Result := True;
      end;
    22: // Abs
      begin
        PStart := Stack.Count - 2;
        try
          LSetReal(Stack, PStart + 1, Abs(LGetReal(Stack, PStart)));
        except
          Caller.CMD_Err2(erCustomError, 'Floating Point Exception');
        end;
        Result := True;
      end;
    23: // StrToFloat
      begin
        PStart := Stack.Count - 2;
        try
          Val(LGetStr(Stack, PStart), E, I);
          LSetReal(Stack, PStart + 1, E);
        except
          Caller.CMD_Err2(erCustomError, 'Floating Point Exception');
        end;
        Result := True;
      end;
    24: // FloatToStr
      begin
        PStart := Stack.Count - 2;
        try
          LSetStr(Stack, PStart + 1, FloatToStr(LGetReal(Stack, PStart)));
        except
          Caller.CMD_Err2(erCustomError, 'Floating Point Exception');
        end;
        Result := True;
      end;
    25: //  PadL
      begin
        PStart := Stack.Count - 3;
        LSetStr(Stack, PStart + 2, Padl(LGetStr(Stack, PStart + 1), LGetUInt(Stack, PStart)));
        Result := True;
      end;
    26: // PadR
      begin
        PStart := Stack.Count - 3;
        LSetStr(Stack, PStart + 2, Padr(LGetStr(Stack, PStart + 1), LGetUInt(Stack, PStart)));
        Result := True;
      end;
    27: // PadZ
      begin
        PStart := Stack.Count - 3;
        LSetStr(Stack, PStart + 2, Padz(LGetStr(Stack, PStart + 1), LGetUInt(Stack, PStart)));
        Result := True;
      end;
    28: // Replicate/StrOfChar
      begin
        PSTart := Stack.Count - 3;
        LSetStr(Stack, PStart + 2, StringOfChar(Char(LGetInt(Stack, PStart + 1)), LGetInt(Stack, PStart)));
        Result := True;
      end;
    29: // Assigned
      begin
        temp := Stack.GetItem(Stack.Count -2);
        if Temp = nil then
        begin
          Result := False;
          exit;
        end;

        case temp^.FType^.BaseType of
          btU8, btS8: b := Temp^.tu8 <> 0;
          btU16, btS16: b := Temp^.tu16 <> 0;
          btU32, btS32: b := Temp^.tu32 <> 0;
          btString, btPChar: b := Temp^.tstring <> nil;
          btArray: b := Temp^.tarray <> nil;
          btPointer: b := Temp^.tpointer <> nil;
          btResourcePointer: b := @temp^.tResourceFreeProc <> nil;
        else
          Result := False;
          Exit;
        end;
        if b then
          LSetInt(Stack, Stack.Count -1, 1)
        else
          LSetInt(Stack, Stack.Count -1, 0);
        Result := True;
      end;
    30: begin {RaiseLastException}
        TMYExec(Caller).ExceptionProc(TMYExec(Caller).ExProc, TMYExec(Caller).ExPos, TMYExec(Caller).ExEx, TMYExec(Caller).ExParam);
        Result := True;
    end;
    31: begin {RaiseExeption}
        TMYExec(Caller).CMD_Err2(TIFError(LGetInt(Stack, Stack.Count -1)), LGetStr(Stack, Stack.Count -2));
        Result := True;
    end;
    32: begin {ExceptionType}
        LSetInt(Stack, Stack.Count -1, Ord(TMyExec(Caller).ExEx));
        Result := True;
    end;
    33: begin {ExceptionParam}
        LSetstr(Stack, Stack.Count -1, TMyExec(Caller).ExParam);
        Result := True;
    end;
    34: begin {ExceptionProc}
        LSetInt(Stack, Stack.Count -1, TMyExec(Caller).ExProc);
        Result := True;
    end;
    35: begin {ExceptionPos}
        LSetInt(Stack, Stack.Count -1, TMyExec(Caller).ExPos);
        Result := True;
    end;
    36:
        begin {ExceptionToString}
          LSetStr(Stack, Stack.Count -1, TIFErrorToString(TIFError(LGetInt(Stack, Stack.Count -2)), LGetStr(Stack, Stack.Count -3)));
          Result := True;
        end;

    else
      Result := False;
  end;
end;

function GetArrayLength(Caller: TSCExec; p: PIFProcRec; Global, Stack: TSCList): Boolean;
var
  PStart: Cardinal;
  n: PIfVariant;
begin
  PStart := Stack.Count - 2;
  n := Stack.GetItem(PStart);
  if n^.FType^.BaseType = btVariant then
  begin
    n := n^.tvariant;
    if n^.ftype = nil then
    begin
      result := false; exit;
    end;
  end;
  if n^.FType^.BaseType <>btArray then begin result := false; exit; end;

  if n^.tArray = nil then
    LSetInt(Stack, PStart + 1, 0)
  else
    LSetInt(Stack, PStart + 1, pbtrecord(n^.TArray)^.FieldCount);
  Result := True;
end;

function min(const x,y: integer): integer;
begin
  if x < y then result := x else result := y;
end;

function SetArrayLength(Caller: TSCExec; p: PIFProcRec; Global, Stack: TSCList): Boolean;
var
  PStart: Cardinal;
  n: PIfVariant;
  i, oldl: Integer;
  r: pbtRecord;
begin
  PStart := Stack.Count - 2;
  n := Stack.GetItem(PStart + 1);
  if n^.FType^.BaseType = btVariant then
  begin
    n := n^.tvariant;
    if n^.ftype = nil then
    begin
      result := false; exit;
    end;
  end;
  if n^.FType^.BaseType <>btArray then begin result := false; exit; end;
  if n^.tArray = nil then
  begin
    i := LGetInt(Stack, PStart);
    if  i > 0 then
    begin
      try
        GetMem(r, 4 + i * 4);
      except
        result := False;
        exit;
      end;
      r^.FieldCount := i;
      dec(i);
      while i >= 0 do
      begin
        r^.Fields[i] := CreateVariant({$IFNDEF NOSMARTMM}Caller.MemoryManager, {$ENDIF}Caller.GetTypeNo(Cardinal(n^.FType^.Ext)));
        if r^.Fields[i] = nil then
        begin
          while i < LGetInt(Stack, PStart) do
          begin
            DisposeVariant({$IFNDEF NOSMARTMM}Caller.MemoryManager, {$ENDIF}r.Fields[i]);
            inc(i);
          end;
          Result := False;
          exit;
        end;
        dec(i);
      end;
      n^.tArray := r;
    end;
  end else begin
    r := n^.TArray;
    oldl := LGetInt(Stack, PStart);
    for i := oldl to r^.FieldCount -1 do
    begin
      DisposeVariant({$IFNDEF NOSMARTMM}Caller.MemoryManager, {$ENDIF} r^.Fields[i]);
    end;
    if oldl = 0 then
    begin
      FreeMem(r, 4 + 4 * r^.FieldCount);
      n^.tArray := nil;
    end else begin
      i := oldl;
      oldl := r^.FieldCount;
      try
        ReallocMem(r, 4 + 4 * i);
      except
        for i := 0 to Min(LGetInt(Stack, PStart), oldl)-1 do
        begin
          DisposeVariant({$IFNDEF NOSMARTMM}Caller.MemoryManager, {$ENDIF} r^.Fields[i]);
        end;
        FreeMem(r, 4 + 4 * LGetInt(Stack, PStart));
        n^.tArray := nil;
        result := false;
        exit;
      end;
      r^.FieldCount := i;
      for i := r^.FieldCount -1 downto oldl do
      begin
        r^.Fields[i] := CreateVariant({$IFNDEF NOSMARTMM}Caller.MemoryManager, {$ENDIF}Caller.GetTypeNo(Cardinal(n^.FType^.Ext)));
        if r^.Fields[i] = nil then
        begin
          oldl := i;
          while oldl < LGetInt(Stack, PStart) do
          begin
            DisposeVariant({$IFNDEF NOSMARTMM}Caller.MemoryManager, {$ENDIF}r.Fields[oldl]);
            inc(oldl);
          end;
          Result := False;
          exit;
        end;
      end;
      n^.tArray := r;
    end;
  end;
  Result := True;
end;
{
Function StrGet(S : String; I : Integer) : Char;
procedure StrSet(c : Char; I : Integer; var s : String);
Function Uppercase(s : string) : string;
Function Lowercase(s : string) : string;
Function Trim(s : string) : string;
Function Length(s : String) : Longint;
procedure SetLength(var S: String; L: Longint);
Function Sin(e : Extended) : Extended;
Function Cos(e : Extended) : Extended;
Function Sqrt(e : Extended) : Extended;
Function Round(e : Extended) : Longint;
Function Trunc(e : Extended) : Longint;
Function Int(e : Extended) : Longint;
Function Pi : Extended;
Function Abs(e : Extended) : Extended;
Function Sqrt(e : Extended) : Extended;
function StrToFloat(s: string): Extended;
Function FloatToStr(e : Extended) : String;
Function Padl(s : string;I : longInt) : string;
Function Padr(s : string;I : longInt) : string;
Function Padz(s : string;I : longInt) : string;
Function Replicate(c : char;I : longInt) : string;
Function StringOfChar(c : char;I : longInt) : string;
}

procedure RegisterStandardLibrary_R(S: TSCExec);
begin
  s.RegisterFunctionName('INTTOSTR', DefProc, Pointer(0), nil);
  s.RegisterFunctionName('STRTOINT', DefProc, Pointer(1), nil);
  s.RegisterFunctionName('STRTOINTDEF', DefProc, Pointer(2), nil);
  s.RegisterFunctionName('POS', DefProc, Pointer(3), nil);
  s.RegisterFunctionName('COPY', DefProc, Pointer(4), nil);
  s.RegisterFunctionName('DELETE', DefProc, Pointer(5), nil);
  s.RegisterFunctionName('INSERT', DefProc, Pointer(6), nil);

  s.RegisterFunctionName('STRGET', DefProc, Pointer(7), nil);
  s.RegisterFunctionName('STRSET', DefProc, Pointer(8), nil);
  s.RegisterFunctionName('UPPERCASE', DefProc, Pointer(10), nil);
  s.RegisterFunctionName('LOWERCASE', DefProc, Pointer(11), nil);
  s.RegisterFunctionName('TRIM', DefProc, Pointer(12), nil);
  s.RegisterFunctionName('LENGTH', DefProc, Pointer(13), nil);
  s.RegisterFunctionName('SETLENGTH', DefProc, Pointer(14), nil);
  s.RegisterFunctionName('SIN', DefProc, Pointer(15), nil);
  s.RegisterFunctionName('COS', DefProc, Pointer(16), nil);
  s.RegisterFunctionName('SQRT', DefProc, Pointer(17), nil);
  s.RegisterFunctionName('ROUND', DefProc, Pointer(18), nil);
  s.RegisterFunctionName('TRUNC', DefProc, Pointer(19), nil);
  s.RegisterFunctionName('INT', DefProc, Pointer(20), nil);
  s.RegisterFunctionName('PI', DefProc, Pointer(21), nil);
  s.RegisterFunctionName('ABS', DefProc, Pointer(22), nil);
  s.RegisterFunctionName('STRTOFLOAT', DefProc, Pointer(23), nil);
  s.RegisterFunctionName('FLOATTOSTR', DefProc, Pointer(24), nil);
  s.RegisterFunctionName('PADL', DefProc, Pointer(25), nil);
  s.RegisterFunctionName('PADR', DefProc, Pointer(26), nil);
  s.RegisterFunctionName('PADZ', DefProc, Pointer(27), nil);
  s.RegisterFunctionName('REPLICATE', DefProc, Pointer(28), nil);
  s.RegisterFunctionName('STRINGOFCHAR', DefProc, Pointer(28), nil);
  s.RegisterFunctionName('!ASSIGNED', DefProc, Pointer(29), nil);
  s.RegisterFunctionName('VARGETTYPE', VarProc, Pointer(0), nil);
  s.RegisterFunctionName('NULL', VarProc, Pointer(1), nil);

  s.RegisterFunctionName('GETARRAYLENGTH', GetArrayLength, nil, nil);
  s.RegisterFunctionName('SETARRAYLENGTH', SetArrayLength, nil, nil);

  s.RegisterFunctionName('RAISELASTEXCEPTION', DefPRoc, Pointer(30), nil);
  s.RegisterFunctionName('RAISEEXCEPTION', DefPRoc, Pointer(31), nil);
  s.RegisterFunctionName('EXCEPTIONTYPE', DefPRoc, Pointer(32), nil);
  s.RegisterFunctionName('EXCEPTIONPARAM', DefPRoc, Pointer(33), nil);
  s.RegisterFunctionName('EXCEPTIONPROC', DefPRoc, Pointer(34), nil);
  s.RegisterFunctionName('EXCEPTIONPOS', DefPRoc, Pointer(35), nil);
  s.RegisterFunctionname('EXCEPTIONTOSTRING', DefProc, Pointer(36), nil);
end;

end.
