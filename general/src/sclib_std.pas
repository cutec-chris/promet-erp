unit SClib_std;
{

Innerfuse Pascal Script III
Copyright (C) 2000-2002 by Carlo Kok (ck@carlo-kok.com)

}
interface
uses
  SC_Utils, SC_Comp,SysUtils;

{
  In your Compilers OnUses proc:
  RegisterStandardLibrary_C registers the standard library.

function floattostr(e: extended): string;
function inttostr(i: Longint): string;
function strtoint(s: string): Longint;
function strtointdef(s: string; def: Longint): Longint;
function copy(s: string; ifrom, icount: Longint): string;
function pos(substr, s: string): Longint;
procedure delete(var s: string; ifrom, icount: Longint): string;
procedure insert(s: string; var s2: string; ipos: Longint): string;
function getarraylength(var v: array): Integer;
procedure setarraylength(var v: array; i: Integer);

Function StrGet(var S : String; I : Integer) : Char;
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
function StrToFloat(s: string): Extended;
Function FloatToStr(e : Extended) : String;
Function Padl(s : string;I : longInt) : string;
Function Padr(s : string;I : longInt) : string;
Function Padz(s : string;I : longInt) : string;
Function Replicate(c : char;I : longInt) : string;
Function StringOfChar(c : char;I : longInt) : string;

type
  TVarType = (vtNull, vtString, vtU64, vtS32, vtU32, vtS16, vtU16, vtS8, vtU8, vtSingle, vtDouble, vtExtended, vtResourcePointer, vtArray, vtRecord);
function VarGetType(x: Variant): TVarType;
function Null: Variant;

type
  TIFException = (ErNoError, erCannotImport, erInvalidType, ErInternalError,
    erInvalidHeader, erInvalidOpcode, erInvalidOpcodeParameter, erNoMainProc,
    erOutOfGlobalVarsRange, erOutOfProcRange, ErOutOfRange, erOutOfStackRange,
    ErTypeMismatch, erUnexpectedEof, erVersionError, ErDivideByZero, ErMathError,
    erCouldNotCallProc, erOutofRecordRange, erOutOfMemory, erException,
    erNullPointerException, erNullVariantError, erCustomError);


procedure RaiseLastException;
procedure RaiseException(Ex: TIFException; Param: string);
function ExceptionType: TIFException;
function ExceptionParam: string;
function ExceptionProc: Cardinal;
function ExceptionPos: Cardinal;
function ExceptionToString(er: TIFException; Param: string): string;
}
procedure RegisterStandardLibrary_C(S: TSCPascalCompiler);

implementation

procedure RegisterStandardLibrary_C(S: TSCPascalCompiler);
var
  p: PSCRegProc;
begin
  s.AddFunction('function inttostr(i: Longint): string;');
  s.AddFunction('function strtoint(s: string): Longint;');
  s.AddFunction('function strtointdef(s: string; def: Longint): Longint;');
  s.AddFunction('function copy(s: string; ifrom, icount: Longint): string;');
  s.AddFunction('function pos(substr, s: string): Longint;');
  s.AddFunction('procedure delete(var s: string; ifrom, icount: Longint): string;');
  s.AddFunction('procedure insert(s: string; var s2: string; ipos: Longint): string;');
  p := s.Addfunction('function getarraylength: integer;');
  p^.Decl := p^.Decl + ' !V -1';
  p := s.Addfunction('procedure setarraylength;');
  p^.Decl := p^.Decl + ' !V -1 @LENGTH '+IntToStr(Longint(s.FindType('INTEGER')));
  s.AddFunction('Function StrGet(var S : String; I : Integer) : Char;');
  s.AddFunction('procedure StrSet(c : Char; I : Integer; var s : String);');
  s.AddFunction('Function Uppercase(s : string) : string;');
  s.AddFunction('Function Lowercase(s : string) : string;');
  s.AddFunction('Function Trim(s : string) : string;');
  s.AddFunction('Function Length(s : String) : Longint;');
  s.AddFunction('procedure SetLength(var S: String; L: Longint);');
  s.AddFunction('Function Sin(e : Extended) : Extended;');
  s.AddFunction('Function Cos(e : Extended) : Extended;');
  s.AddFunction('Function Sqrt(e : Extended) : Extended;');
  s.AddFunction('Function Round(e : Extended) : Longint;');
  s.AddFunction('Function Trunc(e : Extended) : Longint;');
  s.AddFunction('Function Int(e : Extended) : Longint;');
  s.AddFunction('Function Pi : Extended;');
  s.AddFunction('Function Abs(e : Extended) : Extended;');
  s.AddFunction('function StrToFloat(s: string): Extended;');
  s.AddFunction('Function FloatToStr(e : Extended) : String;');
  s.AddFunction('Function Padl(s : string;I : longInt) : string;');
  s.AddFunction('Function Padr(s : string;I : longInt) : string;');
  s.AddFunction('Function Padz(s : string;I : longInt) : string;');
  s.AddFunction('Function Replicate(c : char;I : longInt) : string;');
  s.AddFunction('Function StringOfChar(c : char;I : longInt) : string;');
  s.AddTypeS('TVarType', '(vtNull, vtString, vtU64, vtS32, vtU32, vtS16, vtU16, vtS8, vtU8, vtSingle, vtDouble, vtExtended, vtResourcePointer, vtArray, vtRecord)');
  S.AddFunction('function VarGetType(x: Variant): TVarType;');
  s.AddFunction('function Null: Variant;');

  s.addTypeS('TIFException', '(ErNoError, erCannotImport, erInvalidType, ErInternalError, erInvalidHeader, erInvalidOpcode, erInvalidOpcodeParameter, erNoMainProc, erOutOfGlobalVarsRange, erOutOfProcRange, ErOutOfRange, erOutOfStackRange, '+
    'ErTypeMismatch, erUnexpectedEof, erVersionError, ErDivideByZero, ErMathError,erCouldNotCallProc, erOutofRecordRange, erOutOfMemory, erException, erNullPointerException, erNullVariantError, erCustomError)');

  s.AddFunction('procedure RaiseLastException;');
  s.AddFunction('procedure RaiseException(Ex: TIFException; Param: string);');
  s.AddFunction('function ExceptionType: TIFException;');
  s.AddFunction('function ExceptionParam: string;');
  s.AddFunction('function ExceptionProc: Cardinal;');
  s.AddFunction('function ExceptionPos: Cardinal;');
  s.Addfunction('function ExceptionToString(er: TIFException; Param: string): string;');


end;


end.
