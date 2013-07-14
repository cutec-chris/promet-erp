unit SC_Exec;

{$ifdef fpc}
{$mode delphi}{$H+}
{$endif}

interface
uses
  SysUtils, SC_Parsers, SC_Utils;

type
  TSCExec = class;
{ TIFError contains all possible errors }
  TIFError = (ErNoError, erCannotImport, erInvalidType, ErInternalError,
    erInvalidHeader, erInvalidOpcode, erInvalidOpcodeParameter, erNoMainProc,
    erOutOfGlobalVarsRange, erOutOfProcRange, ErOutOfRange, erOutOfStackRange,
    ErTypeMismatch, erUnexpectedEof, erVersionError, ErDivideByZero, ErMathError,
    erCouldNotCallProc, erOutofRecordRange, erOutOfMemory, erException,
    erNullPointerException, erNullVariantError, erCustomError);
{ The current status of the script }
  TIFStatus = (isNotLoaded, isLoaded, isRunning, isPaused);
{Pointer to array of bytes}
  PByteArray = ^TByteArray;
{Array of bytes}
  TByteArray = array[0..1023] of Byte;
{Pointer to array of words}
  PDWordArray = ^TDWordArray;
{Array of dwords}
  TDWordArray = array[0..1023] of Cardinal;
{ Pointer to @link(TIFTypeRec)}
  PIFTypeRec = ^TIFTypeRec;
{TIFTypeRec is used to store all types inside the script}
  TIFTypeRec = record
    {Ext is used in a typecopy or array to store more information}
    Ext: Pointer;
    BaseType: TSCBaseType;
    ExportName: string;
    ExportNameHash: Longint;
  end;
{TIFArrayType is a pointer to an other type}
  TIFArrayType = PIFTypeRec;
{PIFRecordType is a pointer to record information}
  PIFRecordType = ^TIFRecordType;
{TIFRecordType is used to store information about records}
  TIFRecordType = record
    Data: string;
  end;
{@link(TProcRec)
  PProcRec is pointer to a TProcRec record}
  PProcRec = ^TProcRec;
{@link(TIFProcRec)
  PIFProcRec is a pointer to a TIProcRec record}
  PIFProcRec = ^TIFProcRec;
{
@link(TSCExec)
@link(PIFProcRec)
@link(TIfList)
TIFProc is is the procedure definition of all external functions
}
  TIFProc = function(Caller: TSCExec; p: PIFProcRec; Global, Stack: TSCList): Boolean;
{
@link(PProcRec)
FreeProc is called when a PProcRec is freed}
  TIFFreeProc = procedure (Caller: TSCExec; p: PProcRec);
{TIFProcRec contains a currently used internal or external function}
  TIFProcRec = record
    {True means the procedure is external}
    ExternalProc: Boolean;
    {The exportname/decl used to identify the procedure}
    ExportName, ExportDecl: string;
    {ExportNameHash is used to quickly find an ExportName}
    ExportNameHash: Longint;
    case Boolean of
      False: (Data: PByteArray; Length: Cardinal);
      True: (ProcPtr: TIFProc; Name: ShortString; Ext1, Ext2: Pointer);
      // ExportDecl will contain Params in case of importing with Flags := 3;
  end;
{TProcrec is used to store an external function that could be used by the script executer}
  TProcRec = record
    Name: ShortString;
    Hash: Longint;
    ProcPtr: TIFProc;
    FreeProc: TIFFreeProc;
    Ext1, Ext2: Pointer;
  end;
{@link(TBTReturnAddress)
  PBTReturnAddress is a pointer to an TBTReturnAddress record}
  PBTReturnAddress = ^TBTReturnAddress;
{TBTReturnAddress is a record used to store return information}
  TBTReturnAddress = record
    ProcNo: PIFProcRec;
    Position, StackBase: Cardinal;
  end;
{@link(PIFVariant)
PPIFVariant is a pointer to a PIFVariant}
  PPIFVariant = ^PIfVariant;
{@link(TIFVariant)
PIFVariant is a pointer to a TIFVariant}
  PIFVariant = ^TIfVariant;
{@link(TVariantResourceFreeProc)
  TVRMode is used to when the scriptengine needs to free or duplicate a resourcepointer}
  TVRFMode = (vrfFree, vrfDuplicate);
{@link(TVRMode)
  TVariantResourceFreeProc is used when the scriptengine needs to free or duplicate a resourcepointer}
  TVariantResourceFreeProc = function (FMode: TVRFMode; P, IntoP: PIFVariant): Boolean;
  {PBTRecord is a pointer to a @link(TbtRecord) record}
  pbtrecord = ^TbtRecord;
{TIFvariant is variant used for storing all variables used by the script}
  TIFVariant = packed record
    {The type of the variant}
    FType: PIFTypeRec;
    {The number of pointers referencing this variant}
    RefCount: Cardinal; // 0 = Freeable
    case Byte of
      1: (tu8: TbtU8);
      2: (tS8: TbtS8);
      3: (tu16: TbtU16);
      4: (ts16: TbtS16);
      5: (tu32: TbtU32);
      6: (ts32: TbtS32);
      7: (tsingle: TbtSingle);
      8: (tdouble: TbtDouble);
      9: (textended: TbtExtended);
      10: (tstring: Pointer);
      11: (treturnaddress: TBTReturnAddress);
      12: (trecord: pbtrecord);
      13: (tArray: pbtrecord);
      14: (tPointer: PIfVariant);
      15: (tResourceP1, tResourceP2: Pointer; tResourceFreeProc: TVariantResourceFreeProc);
      16: (tvariant: PIFVariant);
      {$IFNDEF NOINT64}
      17: (ts64: Tbts64);
      {$ENDIF}
  end;
  {TbtRecord is used to store the fields in a record or array}
  TbtRecord = packed record
    FieldCount: Cardinal;
    Fields: array[0..10000] of PIfVariant;
  end;
  {TSCResourceFreeProc is called when a resource needs to be freed}
  TSCResourceFreeProc = procedure (Sender: TSCExec; P: Pointer);
  {@link(TSCResource)
    PSCResource is a pointer to a TSCResource record
  }
  PSCResource = ^TSCResource;
  { A resource in SC3 is stored as a pointer to the proc and a tag (p) }
  TSCResource = record
    Proc: Pointer;
    P: Pointer;
  end;
  {@link(pbtrecord)
    PBTRecord}
  PBTArray = pbtrecord;
  {@link(TbtRecord)
  tbtrecord}
  TBTArray = TbtRecord;
  {See TSCExec.OnRunLine}
  TSCOnLineEvent = procedure(Sender: TSCExec);
  {See TSCExec.AddSpecialProcImport}
  TSCOnSpecialProcImport = function (Sender: TSCExec; p: PIFProcRec; Tag: Pointer): Boolean;
  {TSCExec is the core of the script engine executer}
  TSCExec = class(TObject)
  Private
    FId: Pointer;
    FJumpFlag: Boolean;
    FCallCleanup: Boolean;
    FObject: TObject;
    function ReadData(var Data; Len: Cardinal): Boolean;
    function ReadByte(var b: Cardinal): Boolean;
    function ReadLong(var b: Cardinal): Boolean;
    function DoCalc(var1, Var2: PIfVariant; CalcType: Cardinal): Boolean;
    function DoBooleanCalc(var1, Var2: PIfVariant; Into: PIfVariant; Cmd: Cardinal): Boolean;
    function SetVariantValue(dest, Src: PIfVariant): Boolean;
    function ReadVariable(var NeedToFree: LongBool; UsePointer: LongBool): PIfVariant;
    procedure DoBooleanNot(Vd: PIfVariant);
    procedure DoMinus(Vd: PIfVariant);
    function BuildArray(Dest, Src: PIFVariant): boolean;
  Protected
    {MM is the memory manager used internally. It's needed to create and destroy variants}
{$IFNDEF NOSMARTMM}MM: Pointer;
{$ENDIF}
    {The exception stack}
    FExceptionStack: TSCList;
    {The list of resources}
    FResources: TSCList;
    {The list of exported variables}
    FExportedVars: TSCList;
    {FTypes contains all types used by the script}
    FTypes: TSCList; 
    {FProcs contains all script procedures}
    FProcs: TSCList; 
    {FGlobalVars contains the global variables of the current script}
    FGlobalVars: TSCList; 
    {The current stack}
    FStack: TSCList; 
    {The main proc no or -1 (no main proc)}
    FMainProc: Cardinal;
    {The current status of the script engine}
    FStatus: TIFStatus;
    {The current proc}
    FCurrProc: PIFProcRec;
    {The current position in the current proc}
    FCurrentPosition: Cardinal;
    {Current stack base}
    FCurrStackBase: Cardinal;
    {FOnRunLine event}
    FOnRunLine: TSCOnLineEvent;
    {List of SpecialProcs; See TSCExec.AddSpecialProc}
    FSpecialProcList: TSCList;
    {List of all registered external functions}
    FRegProcs: TSCList;
    {The proc where the last error occured}
    ExProc: Cardinal;
    {The position of the last error}
    ExPos: Cardinal;
    {The error code}
    ExEx: TIFError;
    {The optional parameter for the error}
    ExParam: string;
    {RunLine function}
    procedure RunLine; virtual;
    {ImportProc is called when the script needs to import an external function}
    function ImportProc(const Name: ShortString; var proc: TIFProcRec): Boolean; Virtual;
    {ExceptionProc is called when an error occurs}
    procedure ExceptionProc(proc, Position: Cardinal; Ex: TIFError; const s: string); Virtual;
  Public
    {Call CMD_Err to cause an error and stop the script}
    procedure CMD_Err(EC: TIFError);
    {Call CMD_Err2 to cause an error and stop the script}
    procedure CMD_Err2(EC: TIFError; const Param: string);
    {Optional tag of the script engine}
    property Id: Pointer read FID write FID;
    {The MemoryManager used when calling CreateVariant/DestroyVariant}
{$IFNDEF NOSMARTMM}property MemoryManager: Pointer Read MM;{$ENDIF}
    {This function will return about information}
    class function About: string;
    {Use RunProc to call a script function. The Params will not be freed after the call}
    function RunProc(Params: TSCList; ProcNo: Cardinal): Boolean;
    {Search for a type (l is the starting position)}
    function FindType(StartAt: Cardinal; BaseType: TSCBaseType; var l: Cardinal): PIFTypeRec;
    {Search for a type}
    function FindType2(BaseType: TSCBaseType): PIFTypeRec;
    {Return type no L}
    function GetTypeNo(l: Cardinal): PIFTypeRec;
    {Create an integer variant}
    function CreateIntegerVariant(FType: PIFTypeRec; Value: Longint): PIfVariant;
    {create a string variant}
    function CreateStringVariant(FType: PIFTypeRec; const Value: string): PIfVariant;
    {Create a float variant}
    function CreateFloatVariant(FType: PIFTypeRec; Value: Extended): PIfVariant;

    {Get Type that has been compiled with a name}
    function GetType(const Name: string): Cardinal;
    {Get function that has been compiled with a name}
    function GetProc(const Name: string): Cardinal;
    {Get variable that has been compiled with a name}
    function GetVar(const Name: string): Cardinal;
    {Get variable compiled with a name as a variant}
    function GetVar2(const Name: string): PIFVariant;
    {Get variable no (C)}
    function GetVarNo(C: Cardinal): PIFVariant;
    {Get Proc no (C)}
    function GetProcNo(C: Cardinal): PIFProcRec;

    {Create an instance of the executer}
    constructor Create;
	{Destroy this instance of the executer}
    destructor Destroy; Override;

	{Run the current script}
    function RunScript: Boolean;

	{Load data into the script engine}
    function LoadData(const s: string): Boolean; virtual;
	{Clear the currently loaded script}
    procedure Clear; Virtual;
	{Reset all variables in the script to zero}
    procedure Cleanup; Virtual;
    {Stop the script engine}
    procedure Stop; Virtual;
	{Pause the script engine}
    procedure Pause; Virtual;
    {Set CallCleanup to false when you don't want the script engine to cleanup all variables after RunScript}
    property CallCleanup: Boolean read FCallCleanup write FCallCleanup;
    {Status contains the current status of the scriptengine}
    property Status: TIFStatus Read FStatus;
	{The OnRunLine event is called after each executed script line}
    property OnRunLine: TSCOnLineEvent Read FOnRunLine Write FOnRunLine;
    {Add a special proc import; this is used for the dll and class library}
    procedure AddSpecialProcImport(const FName: string; P: TSCOnSpecialProcImport; Tag: Pointer);
    {Register a function by name}
    function RegisterFunctionName(const Name: ShortString; ProcPtr: TIFProc;
      Ext1, Ext2: Pointer): PProcRec;
	{Clear the function list}
    procedure ClearFunctionList;
    {Contains the last error proc}
    property ExceptionProcNo: Cardinal Read ExProc;
	{Contains the last error position}
    property ExceptionPos: Cardinal Read ExPos;
	{Contains the last error code}
    property ExceptionCode: TIFError Read ExEx;
	{Contains the last error string}
    property ExceptionString: string read ExParam;

    {Add a resource}
    procedure AddResource(Proc, P: Pointer);
	{Check if P is a valid resource for Proc}
    function IsValidResource(Proc, P: Pointer): Boolean;
	{Delete a resource}
    procedure DeleteResource(P: Pointer);
	{Find a resource}
    function FindProcResource(Proc: Pointer): Pointer;
	{Find a resource}
    function FindProcResource2(Proc: Pointer; var StartAt: Longint): Pointer;
    property Obj : TObject read FObject write FObject;
  end;
{Decrease the variant's refcount and free it if it's 0}
procedure DisposeVariant({$IFNDEF NOSMARTMM}MM: Pointer; {$ENDIF}p: PIfVariant);
{Create a variant}
function CreateVariant({$IFNDEF NOSMARTMM}MM: Pointer; {$ENDIF}n: PIFTypeRec): PIfVariant;
{Convert an error to a string}
function TIFErrorToString(x: TIFError; const Param: string): string;
{Get the value of a variant (as Cardinal/Longword)}
function GetUInt(Src: PIfVariant; var s: Boolean): Cardinal;
{Get the value of a variant (as Longint)}
function GetInt(Src: PIfVariant; var s: Boolean): Longint;
{Get the value of a variant (as Extended)}
function GetReal(Src: PIfVariant; var s: Boolean): Extended;
{Get the value of a variant (as String)}
function GetString(Src: PIfVariant; var s: Boolean): string;
{Set the value of an Integer variant in a list} 
procedure LSetInt(List: TSCList; Pos: Cardinal; Val: Longint);
{Set the value of an unsigned integer variant in a list} 
procedure LSetUInt(List: TSCList; Pos: Cardinal; Val: Cardinal);
{Get the value of an Integer variant in a list} 
function LGetInt(List: TSCList; Pos: Cardinal): Longint;
{Get the value of an unsigned integer variant in a list} 
function LGetUInt(List: TSCList; Pos: Cardinal): Cardinal;
{Set the value of a string variant in a list} 
procedure LSetStr(List: TSCList; Pos: Cardinal; const s: string);
{Get the value of a string variant in a list} 
function LGetStr(List: TSCList; Pos: Cardinal): string;
{Set the value of a real variant in a list}
procedure LSetReal(List: TSCList; Pos: Cardinal; const Val: Extended);
{Get the value of a real variant in a list} 
function LGetReal(List: TSCList; Pos: Cardinal): Extended;
{Get the length of a variant array}
function GetSCArrayLength(SE: TSCExec; p: PIfVariant): Cardinal;
{Set the length of a variant array}
procedure SetSCArrayLength(SE: TSCExec; p: PIfVariant; NewLength: Cardinal);

{Convert a variant to a string}
function SCVariantToString(p: PIfVariant): string;
{Free a list of variants and also the list}
procedure FreePIFVariantList({$IFNDEF NOSMARTMM}MM: Pointer; {$ENDIF}List: TSCList);

function VGetString(P: PIFVariant): string;
function VGetFloat(P: PIFVariant): Extended;
function VGetInt(P: PIFVariant): Longint;
{$IFNDEF NOINT64}
function VGetInt64(P: PIFVariant): Int64;
{$ENDIF}

procedure VSetString(P: PIFVariant; const d: string);
procedure VSetFloat(P: PIFVariant; const d: Extended);
procedure VSetInt(P: PIFVariant; const d: Longint);
{$IFNDEF NOINT64}
procedure VSetInt64(P: PIFVariant; const d: Int64);
{$ENDIF}

const
  ENoError = ERNoError;
  ecCustomError = erCustomError; 


procedure ChangeVariantType({$IFNDEF NOSMARTMM}MM: Pointer; {$ENDIF}p: PIFVariant; n: PIFTypeRec);
  
implementation

function VGetString(P: PIFVariant): string;
begin
  if p = nil then begin Result := ''; exit; end;
  case p^.FType^.BaseType of
    btu8: Result := chr(p^.tu8);
    btString: Result := TbtString(p^.tstring);
    else Result := '';
  end;
end;

function VGetFloat(P: PIFVariant): Extended;
begin
  if p = nil then begin Result := 0; exit; end;
  case p^.FType^.BaseType of
    btSingle: Result := p^.tsingle;
    btDouble: Result := p^.tdouble;
    btExtended: Result := p^.textended;
    else Result := 0;
  end;
end;
function VGetInt(P: PIFVariant): Longint;
begin
  if p = nil then begin Result := 0; exit; end;
  case p^.FType^.BaseType of
    btu8: Result := p^.tu8;
    bts8: Result := p^.ts8;
    btu16: Result := p^.tu16;
    bts16: Result := p^.ts16;
    btu32: Result := p^.tu32;
    bts32: Result := p^.ts32;
    else Result := 0;
  end;
end;
{$IFNDEF NOINT64}

function VGetInt64(P: PIFVariant): Int64;
begin
  if p = nil then begin Result := 0; exit; end;
  case p^.FType^.BaseType of
    btu8: Result := p^.tu8;
    bts8: Result := p^.ts8;
    btu16: Result := p^.tu16;
    bts16: Result := p^.ts16;
    btu32: Result := p^.tu32;
    bts32: Result := p^.ts32;
    btS64: Result := p^.ts64;
    else Result := 0;
  end;
end;
{$ENDIF}
procedure VSetString(P: PIFVariant; const d: string);
begin
  if p = nil then begin exit; end;
  case p^.FType^.BaseType of
    btString: TbtString(p^.tstring) := d;
  end;
end;
procedure VSetFloat(P: PIFVariant; const d: Extended);
begin
  if p = nil then begin exit; end;
  case p^.FType^.BaseType of
    btSingle: p^.tsingle := d;
    btDouble: p^.tdouble := d;
    btExtended: p^.textended := d;
  end;
end;
procedure VSetInt(P: PIFVariant; const d: Longint);
begin
  if p = nil then begin exit; end;
  case p^.FType^.BaseType of
    btu8: p^.tu8 := d;
    bts8: p^.ts8 := d;
    btu16: p^.tu16 := d;
    bts16: p^.ts16 := d;
    btu32: p^.tu32 := d;
    bts32: p^.ts32 := d;
  end;
end;
{$IFNDEF NOINT64}
procedure VSetInt64(P: PIFVariant; const d: Int64);
begin
  if p = nil then begin exit; end;
  case p^.FType^.BaseType of
    btu8: p^.tu8 := d;
    bts8: p^.ts8 := d;
    btu16: p^.tu16 := d;
    bts16: p^.ts16 := d;
    btu32: p^.tu32 := d;
    bts32: p^.ts32 := d;
    btS64: p^.ts64 := d;
  end;
end;
{$ENDIF}
function MakeString(const s: string): string;
var
  i: Longint;
  e: string;
  b: boolean;
begin
  Result := s;
  i := 1;
  b := false;
  while i <= length(result) do
  begin
    if Result[i] = '''' then
    begin
      if not b then
      begin
        b := true;
        Insert('''', Result, i);
        inc(i);
      end;
      Insert('''', Result, i);
      inc(i, 2);
    end else if (Result[i] < #32) then
    begin
      e := '#'+inttostr(ord(Result[i]));
      Delete(Result, i, 1);
      if b then
      begin
        b := false;
        Insert('''', Result, i);
        inc(i);
      end;
      Insert(e, Result, i);
      inc(i, length(e));
    end else begin
      if not b then
      begin
        b := true;
        Insert('''', Result, i);
        inc(i, 2);
      end else
        inc(i);
    end;  
  end;
  if b then
  begin
    Result := Result + '''';
  end;
  if Result = '' then
    Result := '''''';
end;

function SCVariantToString(p: PIfVariant): string;
var
  I: Longint;
begin
  while p^.FType^.BaseType = btPointer do
  begin
    if p^.tPointer <> nil then p := p^.tPointer else break;
  end;
  if p^.FType^.BaseType = btVariant then P := p^.tvariant;
  case p^.FType^.BaseType of
    btU8: str(p^.tu8, Result);
    btS8: str(p^.ts8, Result);
    btU16: str(p^.tu16, Result);
    btS16: str(p^.ts16, Result);
    btU32: str(p^.tu32, Result);
    btS32: str(p^.ts32, Result);
    btSingle: str(p^.tsingle, Result);
    btDouble: str(p^.tdouble, Result);
    btExtended: str(p^.textended, Result);
    btString, btPChar: Result := makestring(string(p^.tString));
    {$IFNDEF NOINT64}btS64: str(p^.ts64, Result);{$ENDIF}
    btRecord, btArray:
      begin
        Result := '[';
        if p^.tArray <>nil then
        begin
          for i := 0 to pbtRecord(p^.tarray)^.FieldCount -1 do
          begin
            if i <> 0 then
              Result := Result + ', ';
            Result := Result + SCVariantToString(pbtRecord(p^.tarray)^.Fields[i]);
          end;
        end;
        Result := Result + ']';
      end;
    btPointer: Result := 'Nil';
    btResourcePointer: Result := '[ResourcePointer]';
  else
    Result := '[Invalid]';
  end;
end;


function GetSCArrayLength(SE: TSCExec; p: PIfVariant): Cardinal;
begin
  if p^.FType^.BaseType <> btArray then begin
    Result := 0;
    exit;
  end;
  if p^.tArray = nil then
    Result := 0
  else
    Result := pbtrecord(p^.tArray)^.FieldCount;
end;

function Min(const x, Y: Integer): Integer;
begin
  if x < Y then Result := x else Result := Y;
end;

procedure SetSCArrayLength(SE: TSCExec; p: PIfVariant; NewLength: Cardinal);
var
  I, oldl: Integer;
  r: pbtrecord;
begin
  if p^.FType^.BaseType <> btArray then exit;
  if p^.tArray = nil then begin
    I := NewLength;
    if I > 0 then begin
      try
        GetMem(r, 4 + I * 4);
      except
        exit;
      end;
      r^.FieldCount := I;
      Dec(I);
      while I >= 0 do begin
        r^.Fields[I] := CreateVariant({$IFNDEF NOSMARTMM}SE.MemoryManager, {$ENDIF}SE.GetTypeNo(Cardinal(p^.FType^.Ext)));
        if r^.Fields[I] = nil then begin
          while I < Longint(NewLength) do begin
            DisposeVariant({$IFNDEF NOSMARTMM}SE.MemoryManager, {$ENDIF}r.Fields[I]);
            Inc(I);
          end;
          exit;
        end;
        Dec(I);
      end;
      p^.tArray := r;
    end;
  end else begin
    r := p^.tArray;
    oldl := NewLength;
    for I := oldl to r^.FieldCount - 1 do begin
      DisposeVariant({$IFNDEF NOSMARTMM}SE.MemoryManager, {$ENDIF}r^.Fields[I]);
    end;
    if oldl = 0 then begin
      FreeMem(r, 4 + 4 * r^.FieldCount);
      p^.tArray := nil;
    end else begin
      I := oldl;
      oldl := r^.FieldCount;
      try
        ReallocMem(r, 4 + 4 * I);
      except
        for I := 0 to Min(NewLength, oldl) - 1 do begin
          DisposeVariant({$IFNDEF NOSMARTMM}SE.MemoryManager, {$ENDIF}r^.Fields[I]);
        end;
        FreeMem(r, 4 + 4 * NewLength);
        p^.tArray := nil;
        exit;
      end;
      r^.FieldCount := I;
      for I := r^.FieldCount - 1 downto oldl do begin
        r^.Fields[I] := CreateVariant({$IFNDEF NOSMARTMM}SE.MemoryManager, {$ENDIF}SE.GetTypeNo(Cardinal(p^.FType^.Ext)));
        if r^.Fields[I] = nil then begin
          oldl := I;
          while oldl < Longint(NewLength) do begin
            DisposeVariant({$IFNDEF NOSMARTMM}SE.MemoryManager, {$ENDIF}r.Fields[oldl]);
            Inc(oldl);
          end;
          exit;
        end;
      end;
      p^.tArray := r;
    end;
  end;
end;

function SafeStr(const s: string): string;
var
 i : Longint;
begin
  Result := s;
  for i := 1 to length(s) do
  begin
    if s[i] in [#0..#31] then
    begin
      Result := Copy(s, 1, i-1);
      exit;
    end;
  end;

end;

function TIFErrorToString(x: TIFError; const Param: string): string;
begin
  case x of
    ErNoError: Result := 'No Error';
    erCannotImport: Result := 'Cannot Import '+Safestr(Param);
    erInvalidType: Result := 'Invalid Type';
    ErInternalError: Result := 'Internal error';
    erInvalidHeader: Result := 'Invalid Header';
    erInvalidOpcode: Result := 'Invalid Opcode';
    erInvalidOpcodeParameter: Result := 'Invalid Opcode Parameter';
    erNoMainProc: Result := 'no Main Proc';
    erOutOfGlobalVarsRange: Result := 'Out of Global Vars range';
    erOutOfProcRange: Result := 'Out of Proc Range';
    ErOutOfRange: Result := 'Out Of Range';
    erOutOfStackRange: Result := 'Out Of Stack Range';
    ErTypeMismatch: Result := 'Type Mismatch';
    erUnexpectedEof: Result := 'Unexpected End Of File';
    erVersionError: Result := 'Version error';
    ErDivideByZero: Result := 'divide by Zero';
    erMathError: Result := 'Math error';
    erCouldNotCallProc: Result := 'Could not call proc';
    erOutofRecordRange: Result := 'Out of Record Fields Range';
    erNullPointerException: Result := 'Null Pointer Exception';
    erNullVariantError: Result := 'Null variant error';
    erOutOfMemory: Result := 'Out Of Memory';
    erException: Result := 'Exception: '+ Param;
    erCustomError: Result := Param;
      else
    Result := 'Unknown error';
  end;
  //
end;

{$IFNDEF NOSMARTMM}
const
  Count = 50;

type
  TFreeIFVariant = packed record
    NextFreeItem: Longint;
    DummyData: array[0..SizeOf(TIfVariant) - SizeOf(Longint) - 1 +
    SizeOf(Pointer)] of Byte;
  end;
  PPageData = ^TPageData;
  TMyIFVariant = packed record
    Page: PPageData;
    p: TIfVariant;
  end;
  TPageData = packed record
    ItemCount, FirstFreeItem: Longint;
    PrevPage, NextPage,
      PrevFreeItemsPage, NextFreeItemsPage: PPageData;
    case Byte of
      0: (BLOCK: array[0..Count - 1] of TMyIFVariant);
      1: (FREELIST: array[0..Count - 1] of TFreeIFVariant);
  end;

type
  TIFVariantMemoryManager = class
  Private
    FFirstFreeItemsPage, FFirstPage: PPageData;
    procedure CleanItem(Page: PPageData);
    function AllocItem: Boolean;
  Public
    constructor Create;
    destructor Destroy; Override;
    procedure Clear;

    function Alloc: PIfVariant;
    procedure DisposeItem(p: PIfVariant);
  end;
type
  TPointingInteger = Longint; // same size as Pointer

function TIFVariantMemoryManager.Alloc: PIfVariant;
var
  CB: PPageData;
  I: Integer;
begin
  if FFirstFreeItemsPage = nil then begin
    if not AllocItem then begin
      Result := nil;
      exit;
    end;
  end;
  CB := FFirstFreeItemsPage;
  Inc(CB^.ItemCount);
  I := CB^.FirstFreeItem;
  CB^.FirstFreeItem := CB^.FREELIST[I].NextFreeItem;
  Result := @CB^.BLOCK[I].p;
  CB^.BLOCK[I].Page := CB;
  if CB^.FirstFreeItem = -1 then begin // remove from freeitemspage list
    if CB^.PrevFreeItemsPage <> nil then
      CB^.PrevFreeItemsPage^.NextFreeItemsPage := CB^.NextFreeItemsPage;
    if CB^.NextFreeItemsPage <> nil then
      CB^.NextFreeItemsPage^.PrevFreeItemsPage := CB^.PrevFreeItemsPage;
    if FFirstFreeItemsPage = CB then
      FFirstFreeItemsPage := CB^.NextFreeItemsPage;
  end;
end;

function TIFVariantMemoryManager.AllocItem: Boolean;
var
  NewItem: PPageData;
  I: Longint;

begin
  try
    New(NewItem);
  except
    Result := False;
    exit;
  end;

  NewItem^.ItemCount := 0;
  NewItem^.FirstFreeItem := Count - 1;
  NewItem^.PrevPage := nil;
  NewItem^.NextPage := FFirstPage;
  NewItem^.PrevFreeItemsPage := nil;
  NewItem^.NextFreeItemsPage := FFirstFreeItemsPage;

  for I := Count - 1 downto 0 do begin
    NewItem^.FREELIST[I].NextFreeItem := I - 1;
  end;

  if FFirstPage <> nil then
    FFirstPage^.PrevPage := NewItem;
  if FFirstFreeItemsPage <> nil then
    FFirstFreeItemsPage^.PrevPage := NewItem;

  FFirstPage := NewItem;
  FFirstFreeItemsPage := NewItem;
  Result := True;
end;

procedure TIFVariantMemoryManager.CleanItem(Page: PPageData);
begin
  if Page^.PrevPage <> nil then
    Page^.PrevPage^.NextPage := Page^.NextPage;
  if Page^.NextPage <> nil then
    Page^.NextPage^.PrevPage := Page^.PrevPage;

  if Page^.PrevFreeItemsPage <> nil then
    Page^.PrevFreeItemsPage^.NextFreeItemsPage := Page^.NextFreeItemsPage;
  if Page^.NextFreeItemsPage <> nil then
    Page^.NextFreeItemsPage^.PrevFreeItemsPage := Page^.PrevFreeItemsPage;
  if FFirstPage = Page then
    FFirstPage := Page^.NextPage;
  if FFirstFreeItemsPage = Page then
    FFirstFreeItemsPage := Page^.NextFreeItemsPage;
  Dispose(Page);
end;

procedure TIFVariantMemoryManager.Clear;
var
  CB, NB: PPageData;
begin
  CB := FFirstPage;
  while CB <> nil do begin
    NB := CB^.NextPage;
    Dispose(CB);
    CB := NB;
  end;
  FFirstPage := nil;
  FFirstFreeItemsPage := nil;
end;

constructor TIFVariantMemoryManager.Create;
begin
  inherited Create;
  FFirstFreeItemsPage := nil;
  FFirstPage := nil;
end;

destructor TIFVariantMemoryManager.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TIFVariantMemoryManager.DisposeItem(p: PIfVariant);
var
  Page: PPageData;
  I: Longint;
begin
  Page := PPageData(Pointer(TPointingInteger(p) - SizeOf(Pointer))^);
  I := (TPointingInteger(p) - TPointingInteger(@Page^.BLOCK) - SizeOf(Pointer)) div SizeOf(TMyIFVariant);
  Dec(Page^.ItemCount);
  Page^.FREELIST[I].NextFreeItem := Page^.FirstFreeItem;
  Page^.FirstFreeItem := I;
  if Page^.ItemCount = 0 then begin
    CleanItem(Page);
  end
  else if Page^.ItemCount = Count - 1 then begin // insert into list
    if FFirstFreeItemsPage <> nil then
      FFirstFreeItemsPage^.PrevFreeItemsPage := Page;
    Page^.PrevFreeItemsPage := nil;
    Page^.NextFreeItemsPage := FFirstFreeItemsPage;
    FFirstFreeItemsPage := Page;
  end;
end;

{$ENDIF}

const
  ReturnAddressType: TIFTypeRec = (Ext: nil; BaseType: btReturnAddress);

type
  PSCExceptionHandler =^TSCExceptionHandler;
  TSCExceptionHandler = packed record
    BasePtr, StackSize: Cardinal;
    FinallyOffset, ExceptOffset, Finally2Offset, EndOfBlock: Cardinal;
  end;
  TSCHeader = packed record
    HDR: Cardinal;
    SCBuildNo: Cardinal;
    TypeCount: Cardinal;
    ProcCount: Cardinal;
    VarCount: Cardinal;
    MainProcNo: Cardinal;
    ImportTableSize: Cardinal;
  end;

  TSCExportItem = packed record
    ProcNo: Cardinal;
    NameLength: Cardinal;
    DeclLength: Cardinal;
  end;

  TSCType = packed record
    BaseType: TSCBaseType;
  end;
  TSCProc = packed record
    Flags: Byte;
  end;

  TSCVar = packed record
    TypeNo: Cardinal;
    Flags: Byte;
  end;
  PSpecialProc = ^TSpecialProc;
  TSpecialProc = record
    P: TSCOnSpecialProcImport;
    namehash: Longint;
    Name: string;
    tag: pointer;
  end;

procedure DisposeType(p: PIFTypeRec);
var
  x: PIFRecordType;
begin
  if p^.BaseType = btRecord then begin
    x := p^.Ext;
    x^.Data := '';
    Dispose(x);
  end;
  Dispose(p);
end;

procedure DisposeProc(SE: TSCExec; p: PIFProcRec);
begin
  if not p^.ExternalProc then
    FreeMem(p^.Data, p^.Length);

  Dispose(p);
end;

function Initrecord({$IFNDEF NOSMARTMM}MM: Pointer; {$ENDIF}FType:
  PIFRecordType; var Rec: pbtrecord): Boolean;
var
  I, J: Longint;
begin
  I := (Length(FType^.Data) shr 2);
  try
    GetMem(Rec, 4 + 4 * I);
  except
    Result := False;
    exit;
  end;
  Rec.FieldCount := I;
  for I := 0 to Rec.FieldCount - 1 do begin
    Rec.Fields[I] := CreateVariant({$IFNDEF NOSMARTMM}MM,
{$ENDIF}PIFTypeRec((@FType^.Data[I shl 2 + 1])^));
    if Rec.Fields[I] = nil then begin
      for J := I - 1 downto 0 do begin
        DisposeVariant({$IFNDEF NOSMARTMM}MM, {$ENDIF}Rec.Fields[J]);
        FreeMem(Rec, 4 * 4 * (Length(FType^.Data) shr 2));
        Result := False;
        exit;
      end;
    end;
  end;
  Result := True;
end;

procedure FreeRecord({$IFNDEF NOSMARTMM}MM: Pointer; {$ENDIF}Rec: pbtrecord);
var
  I: Longint;
begin
  if Rec <> nil then begin
    for I := Rec.FieldCount - 1 downto 0 do
      DisposeVariant({$IFNDEF NOSMARTMM}MM, {$ENDIF}Rec.Fields[I]);
    FreeMem(Rec, Rec.FieldCount * 4 + 4);
  end;
end;

procedure DisposeVariant({$IFNDEF NOSMARTMM}MM: Pointer; {$ENDIF}p: PIfVariant);
begin
  if p <> nil then
  if p^.RefCount = 0 then begin
    if p^.FType <> nil then
    begin
      if (p^.FType^.BaseType = btRecord) or (p^.FType^.BaseType = btArray) then
        FreeRecord({$IFNDEF NOSMARTMM}MM, {$ENDIF}p^.trecord)
      else if p^.FType^.BaseType = btString then
        Finalize(TbtString((@p^.tstring)^))
      else if p^.FType^.BaseType = btResourcePointer then
      begin
        if (@p^.tResourceFreeProc <> nil) then
        begin
          p^.tResourceFreeProc(vrfFree, p, nil);
        end;
      end else if p^.FType^.BaseType = btvariant then
         DisposeVariant({$IFNDEF NOSMARTMM}MM, {$ENDIF}p^.tvariant);
    end;
    {$IFNDEF NOSMARTMM}
    TIFVariantMemoryManager(MM).DisposeItem(p);
    {$ELSE}
    Dispose(p);
    {$ENDIF}
  end
  else
    Dec(p^.RefCount);
end;

procedure ChangeVariantType({$IFNDEF NOSMARTMM}MM: Pointer; {$ENDIF}p: PIFVariant; n: PIFTypeRec);
begin
  if p^.FType <> nil then
  begin
    if (p^.FType^.BaseType = btRecord) or (p^.FType^.BaseType = btArray) then
      FreeRecord({$IFNDEF NOSMARTMM}MM, {$ENDIF}p^.trecord)
    else if p^.FType^.BaseType = btString then
      Finalize(TbtString((@p^.tstring)^))
    else if p^.FType^.BaseType = btResourcePointer then
    begin
      if (@p^.tResourceFreeProc <> nil) then
      begin
        p^.tResourceFreeProc(vrfFree, p, nil);
      end;
    end else if p^.FType^.BaseType = btvariant then
      DisposeVariant({$IFNDEF NOSMARTMM}MM, {$ENDIF}p^.tvariant);
  end;
  p^.FType := n;
  if n <> nil then
  begin
    if n^.BaseType = btVariant then
    begin
      {$IFDEF NOSMARTMM}
      try
        New(p^.tvariant);
      except
        p^.tvariant := nil;
        exit;
      end;
      {$ELSE}
      p^.TVariant := TIFVariantMemoryManager(MM).Alloc;
      {$ENDIF}
       p^.tVariant^.FType := nil;
       p^.tvariant^.refcount := 0;
    end else if (n^.BaseType = btRecord) then begin
      p^.RefCount := 0;
      if not Initrecord({$IFNDEF NOSMARTMM}MM, {$ENDIF}n^.Ext, pbtrecord(p^.trecord)) then begin
        p^.trecord := nil;
        DisposeVariant({$IFNDEF NOSMARTMM}MM, {$ENDIF}p);
      end;
    end
    else begin
      FillChar(p^.RefCount, SizeOf(TIfVariant) - SizeOf(Pointer), 0);
    end;
  end;
end;

function CreateVariant({$IFNDEF NOSMARTMM}MM: Pointer; {$ENDIF}n: PIFTypeRec): PIfVariant;
var
  p: PIfVariant;
begin
  if n = nil then begin
    Result := nil;
    exit;
  end;
{$IFNDEF NOSMARTMM}
  p := TIFVariantMemoryManager(MM).Alloc;
  if p = nil then begin
    Result := nil;
    exit;
  end;
{$ELSE}
  try
    New(p);
  except
    Result := nil;
    exit;
  end;
{$ENDIF}

  p^.FType := n;
  if n^.BaseType = btVariant then
  begin
    {$IFDEF NOSMARTMM}
    try
      New(p^.tvariant);
    except
      p^.tvariant := nil;
      exit;
    end;
    {$ELSE}
    p^.TVariant := TIFVariantMemoryManager(MM).Alloc;
    {$ENDIF}
     p^.tVariant^.FType := nil;
     p^.tvariant^.RefCount := 0;
  end else if (n^.BaseType = btRecord) then begin
    p^.RefCount := 0;
    if not Initrecord({$IFNDEF NOSMARTMM}MM, {$ENDIF}n^.Ext, pbtrecord(p^.trecord)) then begin
      p^.trecord := nil;
      DisposeVariant({$IFNDEF NOSMARTMM}MM, {$ENDIF}p);
    end;
  end
  else begin
    FillChar(p^.RefCount, SizeOf(TIfVariant) - SizeOf(Pointer), 0);
  end;
  CreateVariant := p;
end;
procedure LSetReal(List: TSCList; Pos: Cardinal; const Val: Extended);
var
  p: PIfVariant;
begin
  p := List.GetItem(Pos);
  if p = nil then exit;
  case p^.FType^.BaseType of
    btSingle: p^.tsingle := Val;
    btDouble: p^.tdouble := Val;
    btExtended: p^.textended := Val;
  end;
end;

function LGetReal(List: TSCList; Pos: Cardinal): Extended;
var
  p: PIfVariant;
begin
  p := List.GetItem(Pos);
  if p = nil then begin result := 0; exit; end;
  case p^.FType^.BaseType of
    btSingle: Result := p^.tsingle;
    btDouble: Result := p^.tdouble;
    btExtended: Result := p^.textended;
  else
    Result := 0;
  end;
end;

function LGetStr(List: TSCList; Pos: Cardinal): string;
var
  p: PIfVariant;
begin
  p := List.GetItem(Pos);
  if p = nil then begin result := ''; exit; end;
  case p^.FType^.BaseType of
    btstring: Result := TbtString(p^.tstring);
  end;
end;

procedure LSetStr(List: TSCList; Pos: Cardinal; const s: string);
var
  p: PIfVariant;
begin
  p := List.GetItem(Pos);
  if p = nil then exit;
  case p^.FType^.BaseType of
    btstring: TbtString(p^.tstring) := s;
  end;
end;

function LGetUInt(List: TSCList; Pos: Cardinal): Cardinal;
var
  p: PIfVariant;
begin
  p := List.GetItem(Pos);
  if p = nil then begin result := 0; exit; end;
  case p^.FType^.BaseType of
    btU8: Result := p^.tu8;
    btS8: Result := p^.tS8;
    btU16: Result := p^.tu16;
    btS16: Result := p^.ts16;
    btU32: Result := p^.tu32;
    btS32: Result := p^.ts32;
    {$IFNDEF NOINT64}btS64: Result := p^.ts64;{$ENDIF}
    btString: begin
      if Length(tbtstring(p^.tstring)) =1 then
      begin
        Result := ord(tbtstring(p^.tstring)[1]);
      end else Result := 0;
    end;
  else
    Result := 0;
  end;
end;

function LGetInt(List: TSCList; Pos: Cardinal): Longint;
var
  p: PIfVariant;
begin
  p := List.GetItem(Pos);
  if p = nil then begin result := 0; exit; end;
  case p^.FType^.BaseType of
    btU8: Result := p^.tu8;
    btS8: Result := p^.tS8;
    btU16: Result := p^.tu16;
    btS16: Result := p^.ts16;
    btU32: Result := p^.tu32;
    btS32: Result := p^.ts32;
    {$IFNDEF NOINT64}btS64: Result := p^.ts64;{$ENDIF}
    btString: begin
      if Length(tbtstring(p^.tstring)) =1 then
      begin
        Result := ord(tbtstring(p^.tstring)[1]);
      end else Result := 0;
    end;
  else
    Result := 0;
  end;
end;

procedure LSetUInt(List: TSCList; Pos: Cardinal; Val: Cardinal);
var
  Src: PIfVariant;
begin
  Src := List.GetItem(Pos);
  if Src = nil then exit;
  case Src^.FType^.BaseType of
    btU8: Src^.tu8 := Val;
    btS8: Src^.tS8 := Val;
    btU16: Src^.tu16 := Val;
    btS16: Src^.ts16 := Val;
    btU32: Src^.tu32 := Val;
    btS32: Src^.ts32 := Val;
    {$IFNDEF NOINT64}btS64: src^.ts64 := Val;{$ENDIF}
    btString: tbtstring(src^.tstring) := Chr(Val);
  end;
end;

procedure LSetInt(List: TSCList; Pos: Cardinal; Val: Longint);
var
  Src: PIfVariant;
begin
  Src := List.GetItem(Pos);
  if Src = nil then exit;
  case Src^.FType^.BaseType of
    btU8: Src^.tu8 := Val;
    btS8: Src^.tS8 := Val;
    btU16: Src^.tu16 := Val;
    btS16: Src^.ts16 := Val;
    btU32: Src^.tu32 := Val;
    btS32: Src^.ts32 := Val;
    {$IFNDEF NOINT64}btS64: src^.ts64 := Val;{$ENDIF}
    btString: tbtstring(src^.tstring) := chr(Val);
  end;
end;
{$IFNDEF NOINT64}
function GetInt64(Src: PIfVariant; var s: Boolean): Int64;
begin
  case Src^.FType^.BaseType of
    btVariant:
      begin
        if src^.TVariant^.FType <> nil then
          Result := GetInt64(Src^.TVariant, s)
        else
         Result := 0;
      end;
    btU8: Result := Src^.tu8;
    btS8: Result := Src^.tS8;
    btU16: Result := Src^.tu16;
    btS16: Result := Src^.ts16;
    btU32: Result := Src^.tu32;
    btS32: Result := Src^.ts32;
    btS64: Result := src^.ts64;
    btString: begin
      if Length(tbtstring(src^.tstring)) =1 then
      begin
        Result := ord(tbtstring(src^.tstring)[1]);
      end else begin Result := 0; s := False; end;
    end;
  else begin
      s := False;
      Result := 0;
    end;
  end;
end;
{$ENDIF}

function GetUInt(Src: PIfVariant; var s: Boolean): Cardinal;
begin
  case Src^.FType^.BaseType of
    btVariant:
      begin
        if src^.TVariant^.FType <> nil then
          Result := GetUINT(Src^.TVariant, s)
        else
         Result := 0;
      end;
    btU8: Result := Src^.tu8;
    btS8: Result := Src^.tS8;
    btU16: Result := Src^.tu16;
    btS16: Result := Src^.ts16;
    btU32: Result := Src^.tu32;
    btS32: Result := Src^.ts32;
    {$IFNDEF NOINT64}btS64: Result := src^.ts64;{$ENDIF}
    btString: begin
      if Length(tbtstring(src^.tstring)) =1 then
      begin
        Result := ord(tbtstring(src^.tstring)[1]);
      end else begin Result := 0; s := False; end;
    end;
  else begin
      s := False;
      Result := 0;
    end;
  end;
end;

function GetInt(Src: PIfVariant; var s: Boolean): Longint;
begin
  case Src^.FType^.BaseType of
    btVariant:
      begin
        if src^.TVariant^.FType <> nil then
          Result := GetInt(Src^.TVariant, s)
        else
         Result := 0;
      end;
    btU8: Result := Src^.tu8;
    btS8: Result := Src^.tS8;
    btU16: Result := Src^.tu16;
    btS16: Result := Src^.ts16;
    btU32: Result := Src^.tu32;
    btS32: Result := Src^.ts32;
    {$IFNDEF NOINT64}btS64: Result := src^.ts64;{$ENDIF}
    btString: begin
      if Length(tbtstring(src^.tstring)) =1 then
      begin
        Result := ord(tbtstring(src^.tstring)[1]);
      end else begin Result := 0; s := False; end;
    end;
  else begin
      s := False;
      Result := 0;
    end;
  end;
end;

function GetReal(Src: PIfVariant; var s: Boolean): Extended;
begin
  case Src^.FType^.BaseType of
    btVariant:
      begin
        if src^.TVariant^.FType <> nil then
          Result := GetReal(Src^.TVariant, s)
        else
         Result := 0;
      end;
    btU8: Result := Src^.tu8;
    btS8: Result := Src^.tS8;
    btU16: Result := Src^.tu16;
    btS16: Result := Src^.ts16;
    btU32: Result := Src^.tu32;
    btS32: Result := Src^.ts32;
    btSingle: Result := Src^.tsingle;
    btDouble: Result := Src^.tdouble;
    btExtended: Result := Src^.textended;
  else begin
      s := False;
      Result := 0;
    end;
  end;
end;

function GetString(Src: PIfVariant; var s: Boolean): string;
begin
  case Src^.FType^.BaseType of
    btVariant:
      begin
        if src^.TVariant^.FType <> nil then
          Result := GetString(Src^.TVariant, s)
        else
         Result := '';
      end;
    btU8, btS8: Result := Char(Src^.tu8);
    btPChar, btString: Result := TbtString((@Src^.tstring)^);
  else begin
      s := False;
      Result := '';
    end;
  end;
end;

function LookupProc(List: TSCList; const Name: ShortString): PProcRec;
var
  h, l: Longint;
begin
  h := MakeHash(Name);
  for l := 0 to List.Count - 1 do begin
    if (PProcRec(List.GetItem(l))^.Hash = h) and (PProcRec(List.GetItem(l))^.Name
      = Name) then begin
      Result := List.GetItem(l);
      exit;
    end;
  end;
  Result := nil;
end;

{ TSCExec }

procedure TSCExec.ClearFunctionList;
var
  x: PProcRec;
  l: Longint;
begin
  for l := 0 to FRegProcs.Count - 1 do begin
    x := FRegProcs.GetItem(l);
    if @x^.FreeProc <> nil then x^.FreeProc(Self, x);
    Dispose(x);
  end;
  FRegProcs.Clear;
end;

class function TSCExec.About: string;
begin
  Result := 'Innerfuse Pascal Script III ' + SCCurrentversion + '. Copyright (c) 2001-2002 by Carlo Kok';
end;

procedure TSCExec.Cleanup;
var
  I: Longint;
  p: PIfVariant;
begin
  if FStatus <> isLoaded then
    exit;
  for I := 0 to Longint(FGlobalVars.Count) - 1 do begin
    p := FGlobalVars.GetItem(I);
    FGlobalVars.SetItem(I, CreateVariant({$IFNDEF NOSMARTMM}MM,
{$ENDIF}p^.FType));
    DisposeVariant({$IFNDEF NOSMARTMM}MM, {$ENDIF}p);
  end;
end;
type
  PSCExportedVar = ^TSCExportedVar;
  TSCExportedVar = record
    FName: string;
    FNameHash: Longint;
    FVarNo: Cardinal;
  end;

procedure TSCExec.Clear;
var
  I: Longint;
  temp: PSCResource;
  Proc: TSCResourceFreeProc;
  pp: PSCExceptionHandler;
begin
  for i := Longint(FExceptionStack.Count) -1 downto 0 do
  begin
    pp := FExceptionStack.GetItem(i);
    Dispose(pp);
  end;
  for i := Longint(FResources.Count) -1 downto 0 do
  begin
    Temp := FResources.GetItem(i);
    Proc := Temp^.Proc;
    Proc(Self, Temp^.P);
    Dispose(Temp);
  end;
  for i := Longint(FExportedVars.Count) -1 downto 0 do
  begin
    Dispose(PSCExportedVar(FExportedVars.GetItem(I)));
  end;
  for I := 0 to Longint(FStack.Count) - 1 do begin
    DisposeVariant({$IFNDEF NOSMARTMM}MM, {$ENDIF}FStack.GetItem(I));
  end;
  for I := 0 to Longint(FProcs.Count) - 1 do begin
    DisposeProc(Self, FProcs.GetItem(I));
  end;
  for I := 0 to Longint(FGlobalVars.Count) - 1 do begin
    DisposeVariant({$IFNDEF NOSMARTMM}MM, {$ENDIF}FGlobalVars.GetItem(I));
  end;
  for I := 0 to Longint(FTypes.Count) - 1 do begin
    DisposeType(FTypes.GetItem(I));
  end;
  FStack.Clear;
  FProcs.Clear;
  FGlobalVars.Clear;
  FTypes.Clear;
  FStatus := isNotLoaded;
  FResources.Clear;
  FExportedVars.Clear;
  FExceptionStack.Clear;
end;

constructor TSCExec.Create;
begin
  inherited Create;
{$IFNDEF NOSMARTMM}MM := TIFVariantMemoryManager.Create;
{$ENDIF}
  FExceptionStack := TSCList.Create;
  FCallCleanup := False;
  FResources := TSCList.Create;
  FTypes := TSCList.Create;
  FProcs := TSCList.Create;
  FGlobalVars := TSCList.Create;
  FStack := TSCList.Create;
  FMainProc := 0;
  FStatus := isNotLoaded;
  FRegProcs := TSCList.Create;
  FExportedVars := TSCList.create;
  FSpecialProcList := TSCList.Create;
end;

destructor TSCExec.Destroy;
var
  I: Longint;
  P: PSpecialProc;
begin
  Clear;
  for I := FSpecialProcList.Count -1 downto 0 do
  begin
    P := FSpecialProcList.GetItem(I);
    Dispose(p);
  end;
  FStack.Free;
  FResources.Free;
  FExportedVars.Free;
  FGlobalVars.Free;
  FProcs.Free;
  FTypes.Free;
  FSpecialProcList.Free;
  ClearFunctionList;

  FRegProcs.Free;
  FExceptionStack.Free;
{$IFNDEF NOSMARTMM}TIFVariantMemoryManager(MM).Free;
{$ENDIF}
  inherited Destroy;
end;

procedure TSCExec.ExceptionProc(proc, Position: Cardinal; Ex: TIFError; const s: string);
var
  d, l: Longint;
  pp: PSCExceptionHandler;
begin
  ExProc := proc;
  ExPos := Position;
  ExEx := Ex;
  ExParam := s;
  if Ex = eNoError then Exit;
  for d := FExceptionStack.Count -1 downto 0 do
  begin
    pp := FExceptionStack.GetItem(d);
    if FStack.Count > pp^.StackSize then
    begin
      for l := Longint(FStack.count) -1 downto Longint(pp^.StackSize) do
      begin
        DisposeVariant({$IFNDEF SMARTMM}mm, {$ENDIF}FStack.GetItem(l));
        FStack.Delete(l);
      end;
    end;
    FCurrStackBase := pp^.BasePtr;
    if pp^.FinallyOffset <> cardinal(-1) then
    begin
      FCurrentPosition := pp^.FinallyOffset;
      pp^.FinallyOffset := cardinal(-1);
      Exit;
    end else if pp^.ExceptOffset <> cardinal(-1) then
    begin
      FCurrentPosition := pp^.ExceptOffset;
      pp^.ExceptOffset := cardinal(-1);
      Exit;
    end else if pp^.Finally2Offset <> Cardinal(-1) then
    begin
      FCurrentPosition := pp^.FinallyOffset;
      pp^.FinallyOffset := cardinal(-1);
      Exit;
    end;
    Dispose(pp);
    FExceptionStack.Delete(FExceptionStack.Count -1);
  end;
  FStatus := isPaused; 
end;

function TSCExec.ImportProc(const Name: ShortString; var proc: TIFProcRec): Boolean;
var
  u: PProcRec;
  fname: string;
  I, fnh: Longint;
  P: PSpecialProc;

begin
  if name = '' then
  begin
    fname := proc.ExportDecl;
    fname := copy(fname, 1, pos(':', fname)-1);
    fnh := MakeHash(fname);
    for I := FSpecialProcList.Count -1 downto 0 do
    begin
      p := FSpecialProcList.GetItem(I);
      IF (p^.name = '') or ((p^.namehash = fnh) and (p^.name = fname)) then
      begin
        if p^.P(Self, @Proc, p^.tag) then
        begin
          Result := True;
          exit;
        end;
      end;
    end;
    Result := FAlse;
    exit;
  end;
  u := LookupProc(FRegProcs, Name);
  if u = nil then begin
    Result := False;
    exit;
  end;
  proc.ProcPtr := u^.ProcPtr;
  proc.Ext1 := u^.Ext1;
  proc.Ext2 := u^.Ext2;
  Result := True;
end;

function TSCExec.RegisterFunctionName(const Name: ShortString; ProcPtr: TIFProc; Ext1, Ext2: Pointer): PProcRec;
var
  p: PProcRec;
begin
  if LookupProc(FRegProcs, Name) <> nil then begin
    Result :=  nil;
    exit;
  end;
  New(p);
  p^.Name := Name;
  p^.Hash := MakeHash(Name);
  p^.ProcPtr := ProcPtr;
  p^.FreeProc := nil;
  p^.Ext1 := Ext1;
  p^.Ext2 := Ext2;
  FRegProcs.Add(p);
  Result := P;
end;

function TSCExec.LoadData(const s: string): Boolean;
var
  HDR: TSCHeader;
  Pos: Cardinal;

  function read(var Data; Len: Cardinal): Boolean;
  begin
    if Longint(Pos + Len) <= Length(s) then begin
      Move(s[Pos + 1], Data, Len);
      Pos := Pos + Len;
      read := True;
    end
    else
      read := False;
  end;
{$WARNINGS OFF}

  function LoadTypes: Boolean;
  var
    currf: TSCType;
    Curr: PIFTypeRec;
    currr: PIFRecordType;
    fe: Boolean;
    l: Longint;
    d: Cardinal;

    function resolve(var s: string): Boolean;
    var
      l: Longint;
      p: PIFTypeRec;
    begin
      l := 1;
      while l < Length(s) do begin
        p := FTypes.GetItem(Cardinal(s[l]));
        if p = nil then begin
          Result := False;
          exit;
        end;
        PIFTypeRec((@s[l])^) := p;
        if p^.BaseType = btRecord then begin
          Delete(s, l, 4);
          insert(PIFRecordType(p^.Ext)^.Data, s, l);
        end;
        l := l + 4;
      end;
      Result := True;
    end;
  begin
    LoadTypes := True;
    for l := 0 to HDR.TypeCount - 1 do begin
      if not read(currf, SizeOf(currf)) then begin
        cmd_err(erUnexpectedEof);
        LoadTypes := False;
        exit;
      end;
      if (currf.BaseType and 128) <> 0 then begin
        fe := True;
        currf.BaseType := currf.BaseType - 128;
      end else
        fe := False;
      try
        New(Curr);
      except
        CMD_Err(erOutOfMemory);
        LoadTypes := False;
        exit;
      end;
      case currf.BaseType of
        {$IFNDEF NOINT64}bts64, {$ENDIF}
        btU8, btS8, btU16, btS16, btU32, btS32, btSingle, btDouble, btExtended, btString, btPointer, btPChar, btResourcePointer, btVariant: begin
            Curr^.BaseType := currf.BaseType;
            Curr^.Ext := nil;
            FTypes.Add(Curr);
          end;
        btArray: begin
            if not read(d, 4) then begin // Read type
              cmd_err(erUnexpectedEof);
              LoadTypes := False;
              exit;
            end;
            if (d >= FTypes.Count) then begin
              cmd_err(erTypeMismatch);
              LoadTypes := False;
              exit;
            end;
            Curr^.BaseType := currf.BaseType;
            Curr^.Ext := Pointer(d);
            FTypes.Add(Curr);
          end;
        btRecord: begin
            if not read(d, 4) or (d = 0) then begin
              cmd_err(erUnexpectedEof);
              LoadTypes := false;
              exit;
            end;
            try
              New(currr);
            except
              cmd_err(erOutOfMemory);
              LoadTypes := False;
              exit;
            end;
            SetLength(currr^.Data, d * 4);
            if not read(currr^.Data[1], d * 4) then begin
              currr^.Data := '';
              Dispose(currr);
              cmd_err(erUnexpectedEof);
              LoadTypes := False;
              exit;
            end;
            if not resolve(currr^.Data) then begin
              currr^.Data := '';
              Dispose(currr);
              cmd_err(erInvalidType);
              LoadTypes := False;
              exit;
            end;
            Curr^.BaseType := currf.BaseType;
            Curr^.Ext := currr;
            FTypes.Add(Curr);
          end;
      else begin
          LoadTypes := False;
          CMD_Err(erInvalidType);
          Dispose(Curr);
          exit;
        end;
      end;
      if fe then begin
        if not read(d, 4) then begin
          cmd_err(erUnexpectedEof);
          LoadTypes := False;
          exit;
        end;
        if d > SCAddrNegativeStackStart then begin
          cmd_err(erInvalidType);
          LoadTypes := False;
          exit;
        end;
        SetLength(Curr^.ExportName, d);
        if not read(Curr^.ExportName[1], d) then begin
          cmd_err(erUnexpectedEof);
          LoadTypes := False;
          exit;
        end;
        Curr^.ExportNameHash := MakeHash(Curr^.ExportName);
      end;
    end;
  end;

  function LoadProcs: Boolean;
  var
    Rec: TSCProc;
    n: string;
    b: Byte;
    l, L2, L3: Longint;
    Curr: PIFProcRec;
  begin
    LoadProcs := True;
    for l := 0 to HDR.ProcCount - 1 do begin
      if not read(Rec, SizeOf(Rec)) then begin
        cmd_err(erUnexpectedEof);
        LoadProcs := False;
        exit;
      end;
      try
        New(Curr);
      except
        cmd_err(erOutOfMemory);
        LoadProcs := False;
        exit;
      end;
      Curr^.ExternalProc := (Rec.Flags and 1) <> 0;
      if Curr^.ExternalProc then begin
        if not read(b, 1) then begin
          Dispose(Curr);
          cmd_err(erUnexpectedEof);
          LoadProcs := False;
          exit;
        end;
        SetLength(n, b);
        if not read(n[1], b) then begin
          Dispose(Curr);
          cmd_err(erUnexpectedEof);
          LoadProcs := False;
          exit;
        end;
        Curr^.Name := n;
        if (Rec.Flags and 3 = 3) then
        begin
          if (not Read(L2, 4)) or (L2 > Length(s) - Pos) then
          begin
            Dispose(Curr);
            cmd_err(erUnexpectedEof);
            LoadProcs := False;
            exit;
          end;
          SetLength(n, L2);
          Read(n[1], L2); // no check is needed
          Curr^.ExportDecl := n;
        end;
        if not ImportProc(Curr^.Name, Curr^) then begin
          if Curr^.Name <> '' then
            CMD_Err2(erCannotImport, Curr^.Name)
          else if Curr^.ExportDecl <> '' then
            CMD_Err2(erCannotImport, curr^.ExportDecl)
          else
            CMD_Err2(erCannotImport, curr^.ExportName);
          Dispose(Curr);
          LoadProcs := False;
          exit;
        end;
      end
      else begin
        if not read(L2, 4) then begin
          Dispose(Curr);
          cmd_err(erUnexpectedEof);
          LoadProcs := False;
          exit;
        end;
        if not read(L3, 4) then begin
          Dispose(Curr);
          cmd_err(erUnexpectedEof);
          LoadProcs := False;
          exit;
        end;
        if (L2 < 0) or (L2 >= Length(s)) or (L2 + L3 > Length(s)) then begin
          Dispose(Curr);
          cmd_err(erUnexpectedEof);
          LoadProcs := False;
          exit;
        end;
        GetMem(Curr^.Data, L3);
        Move(s[L2 + 1], Curr^.Data^, L3);
        Curr^.Length := L3;
        if (Rec.Flags and 2) <> 0 then begin // exported
          if not read(L3, 4) then begin
            Dispose(Curr);
            cmd_err(erUnexpectedEof);
            LoadProcs := False;
            exit;
          end;
          if L3 > SCAddrNegativeStackStart then begin
            Dispose(Curr);
            cmd_err(erUnexpectedEof);
            LoadProcs := False;
            exit;
          end;
          SetLength(Curr^.ExportName, L3);
          if not read(Curr^.ExportName[1], L3) then begin
            Dispose(Curr);
            cmd_err(erUnexpectedEof);
            LoadProcs := False;
            exit;
          end;
          if not read(L3, 4) then begin
            Dispose(Curr);
            cmd_err(erUnexpectedEof);
            LoadProcs := False;
            exit;
          end;
          if L3 > SCAddrNegativeStackStart then begin
            Dispose(Curr);
            cmd_err(erUnexpectedEof);
            LoadProcs := False;
            exit;
          end;
          SetLength(Curr^.ExportDecl, L3);
          if not read(Curr^.ExportDecl[1], L3) then begin
            Dispose(Curr);
            cmd_err(erUnexpectedEof);
            LoadProcs := False;
            exit;
          end;
          Curr^.ExportNameHash := MakeHash(Curr^.ExportName);
        end;
      end;
      FProcs.Add(Curr);
    end;
  end;
{$WARNINGS ON}

  function LoadVars: Boolean;
  var
    l, n: Longint;
    e: PSCExportedVar;
    Rec: TSCVar;
    Curr: PIfVariant;
  begin
    LoadVars := True;
    for l := 0 to HDR.VarCount - 1 do begin
      if not read(Rec, SizeOf(Rec)) then begin
        cmd_err(erUnexpectedEof);
        LoadVars := False;
        exit;
      end;
      if Rec.TypeNo >= HDR.TypeCount then begin
        cmd_err(erInvalidType);
        LoadVars := False;
        exit;
      end;
      Curr := CreateVariant({$IFNDEF NOSMARTMM}MM,
{$ENDIF}FTypes.GetItem(Rec.TypeNo));
      if Curr = nil then begin
        cmd_err(erInvalidType);
        LoadVars := False;
        exit;
      end;
      if (Rec.Flags and 1) <> 0then
      begin
        if not read(n, 4) then begin
          cmd_err(erUnexpectedEof);
          LoadVars := False;
          exit;
        end;
        new(e);
        try
          SetLength(e^.FName, n);
          if not Read(e^.FName[1], n) then
          begin
            dispose(e);
            cmd_err(erUnexpectedEof);
            LoadVars := False;
            exit;
          end;
          e^.FNameHash := MakeHash(e^.FName);
          e^.FVarNo := FGlobalVars.Count;
          FExportedVars.Add(E);
        except
          dispose(e);
          cmd_err(erInvalidType);
          LoadVars := False;
          exit;
        end;
      end;
      FGlobalVars.Add(Curr);
    end;
  end;

begin
  Clear;
  Pos := 0;
  LoadData := False;
  if not read(HDR, SizeOf(HDR)) then
  begin
    CMD_Err(erInvalidHeader);
    exit;
  end;
  if HDR.HDR <> SCValidHeader then
  begin
    CMD_Err(erInvalidHeader);
    exit;
  end;
  if (HDR.SCBuildNo > SCCurrentBuildNo) or (HDR.SCBuildNo < SCLowBuildSupport) then begin
    CMD_Err(erInvalidHeader);
    exit;
  end;
  if not LoadTypes then
  begin
    Clear;
    exit;
  end;
  if not LoadProcs then
  begin
    Clear;
    exit;
  end;
  if not LoadVars then
  begin
    Clear;
    exit;
  end;
  if (HDR.MainProcNo >= FProcs.Count) and (HDR.MainProcNo <> Cardinal(-1))then begin
    CMD_Err(erNoMainProc);
    Clear;
    exit;
  end;
  // Load Import Table
  FMainProc := HDR.MainProcNo;
  FStatus := isLoaded;
  Result := True;
end;

procedure TSCExec.Pause;
begin
  if FStatus = isRunning then
    FStatus := isPaused;
end;

function TSCExec.ReadData(var Data; Len: Cardinal): Boolean;
begin
  if FCurrentPosition + Len <= FCurrProc.Length then begin
    Move(FCurrProc.Data^[FCurrentPosition], Data, Len);
    FCurrentPosition := FCurrentPosition + Len;
    Result := True;
  end
  else
    Result := False;
end;

procedure TSCExec.CMD_Err(EC: TIFError); // Error
begin
  CMD_Err2(ec, '');
end;

function TSCExec.BuildArray(Dest, Src: PIFVariant): boolean;
var
  i, j: Longint;
  t: pbtrecord;
begin
  if (Src^.FType^.BaseType = btVariant) and (Src^.TVariant^.FType <> nil) and (Src^.TVariant^.FType^.BaseType = btArray) then
    Src := Src^.TVariant;
  if (Src^.FType^.BaseType <> btArray) and (Src^.FType^.BaseType <> btRecord) then
  begin
    Result := False;
    exit;
  end;
  if Dest^.TArray <> nil then
  begin
    for i := 0 to pbtrecord(Dest^.Tarray)^.FieldCount -1 do
    begin
      DisposeVariant({$IFNDEF NOSMARTMM}MM ,{$ENDIF}pbtrecord(Dest^.Tarray)^.fields[i]);
    end;
    FreeMem(pbtrecord(Dest^.Tarray), pbtrecord(Dest^.Tarray)^.FieldCount * 4 + 4);
  end;
  if src^.TArray = nil then
  begin
    Dest^.TArray := nil;
    Result := true;
    exit;
  end;
  try
    getmem(t, pbtRecord(src^.Tarray)^.FieldCount * 4 +4);
    t.FieldCount := pbtRecord(src^.Tarray)^.FieldCount;
  except
    Dest^.TArray := nil;
    Result := False;
    exit;
  end;
  for i := pbtRecord(src^.Tarray)^.FieldCount -1 downto 0 do
  begin
    t^.Fields[i] := CreateVariant({$IFNDEF NOSMARTMM}mm, {$ENDIF} pbtRecord(src^.Tarray)^.Fields[i]^.FType);
    if t^.Fields[i] = nil then
    begin
      Freemem(t, t^.FieldCount * 4 + 4);
      for j := 0 to i -1 do
      begin
        DisposeVariant({$IFNDEF NOSMARTMM}mm, {$ENDIF} t^.Fields[j]);
      end;
      Dest^.TArray := nil;
      Result := False;
      exit;
    end;
    if not SetVariantValue(t^.Fields[i], pbtRecord(src^.Tarray)^.Fields[i]) then
    begin
      for j := pbtRecord(src^.Tarray)^.FieldCount -1 downto i do
      begin
        DisposeVariant({$IFNDEF NOSMARTMM}mm, {$ENDIF} t^.Fields[j]);
      end;
      Freemem(t, t^.FieldCount * 4 + 4);
      Dest^.TArray := nil;
      Result := False;
      exit;
    end;
  end;
  dest^.TArray := t;

  Result := True;
end;

function TSCExec.SetVariantValue(dest, Src: PIfVariant): Boolean;
begin
  Result := True;
  case dest^.FType^.BaseType of
    btU8: dest^.tu8 := GetUInt(Src, Result);
    btS8: dest^.tS8 := GetInt(Src, Result);
    btU16: dest^.tu16 := GetUInt(Src, Result);
    btS16: dest^.ts16 := GetInt(Src, Result);
    btU32: dest^.tu32 := GetUInt(Src, Result);
    btS32: dest^.ts32 := GetInt(Src, Result);
    {$IFNDEF NOINT64}
    btS64: dest^.ts64 := GetInt64(Src, Result);
    {$ENDIF}
    btSingle: dest^.tsingle := GetReal(Src, Result);
    btDouble: dest^.tdouble := GetReal(Src, Result);
    btExtended: dest^.textended := GetReal(Src, Result);
    btPChar,btString: TbtString((@dest^.tstring)^) := GetString(Src, Result);
    btArray, btRecord: Result := BuildArray(Dest, Src);
    btVariant:
    begin
      if Src^.FType^.BaseType = btVariant then
        ChangeVariantType({$IFNDEF NOSMARTMM}mm, {$ENDIF}Dest^.tVariant, src^.TVariant^.FType)
      else
        ChangeVariantType({$IFNDEF NOSMARTMM}mm, {$ENDIF}Dest^.tVariant, src^.FType);
      if Dest^.tvariant = nil then
      begin
        Result := False;
      end else begin
        if Dest^.TVariant^.FType <> nil then
        begin
          if Src^.FType^.BaseType = btVariant then
            Result := SetVariantValue(Dest^.TVariant, Src^.tvariant)
          else
            Result := SetVariantValue(Dest^.TVariant, Src);
        end;
      end;
    end;
    btResourcePointer:
    begin
      if src^.Ftype^.BaseType = btvariant then
      begin
        Src := src^.tvariant;
        if src^.FType = nil then
        begin
          Result := False;
          exit;
        end;
      end;
      if Src^.FType^.BaseType <> btResourcePointer then
      begin
        Result := False;
        exit;
      end;
      if @Src^.tResourceFreeProc <> nil then
      begin
        Result := Src^.tResourceFreeProc(vrfDuplicate, Src, Dest);
      end else begin
        Dest^.TResourceFreeProc := nil;
        Dest^.TResourceP1 := nil;
        Dest^.TResourceP2 := nil;
      end;
    end;
  else begin
      Result := False;
    end;
  end;
  if Result = False then
    CMD_Err(ErTypeMismatch);
end;

function TSCExec.DoBooleanCalc(var1, Var2: PIfVariant; Into: PIfVariant; Cmd:
  Cardinal): Boolean;
var
  b: Boolean;

  procedure SetBoolean(b: Boolean; var Ok: Boolean);
  begin
    Ok := True;
    case Into^.FType^.BaseType of
      btU8: Into^.tu8 := Cardinal(b);
      btS8: Into^.tS8 := Longint(b);
      btU16: Into^.tu16 := Cardinal(b);
      btS16: Into^.ts16 := Longint(b);
      btU32: Into^.tu32 := Cardinal(b);
      btS32: Into^.ts32 := Longint(b);
    else begin
        CMD_Err(ErTypeMismatch);
        Ok := False;
      end;
    end;
  end;
begin
  Result := True;
  if (var1^.FType = nil) and (var1^.FType = nil) then {variants}
  begin
    case Cmd of
      0,1,2,3: Result := False; 
      4: SetBoolean(False, Result); { <> }
      5: SetBoolean(True, Result); { = }
    else begin
        Result := False;
        CMD_Err(erInvalidOpcodeParameter);
        exit;
      end;
    end;
    if not Result then begin
      CMD_Err(erTypeMismatch);
      exit;
    end;
  end else
  if (var1^.FType = nil) xor (var2^.FType = nil) then {variants}
  begin
    case Cmd of
      0,1,2,3: Result := False; 
      4: SetBoolean(True, Result); { <> }
      5: SetBoolean(False, Result); { = }
    else begin
        Result := False;
        CMD_Err(erInvalidOpcodeParameter);
        exit;
      end;
    end;
    if not Result then begin
      CMD_Err(erTypeMismatch);
      exit;
    end;
  end else
  case Cmd of
    0: begin { >= }
        case var1^.FType^.BaseType of
          btU8:
          if (var2^.FType^.BaseType = btString) or (Var2^.Ftype^.BaseType = btPChar) then
            b := char(var1^.tu8) >= GetString(Var2, Result)
          else
            b := var1^.tu8 >= GetUInt(Var2, Result);
          btS8: b := var1^.tS8 >= GetInt(Var2, Result);
          btU16: b := var1^.tu16 >= GetUInt(Var2, Result);
          btS16: b := var1^.ts16 >= GetInt(Var2, Result);
          btU32: b := var1^.tu32 >= GetUInt(Var2, Result);
          btS32: b := var1^.ts32 >= GetInt(Var2, Result);
          btSingle: b := var1^.tsingle >= GetReal(Var2, Result);
          btDouble: b := var1^.tdouble >= GetReal(Var2, Result);
          btExtended: b := var1^.textended >= GetReal(Var2, Result);
          {$IFNDEF NOINT64}
          btS64: b := var1^.ts64 >= GetInt64(Var2, Result);
          {$ENDIF}
          btPChar,btString: b := tbtstring(var1^.tstring) >= GetString(Var2, Result);
        else begin
            CMD_Err(ErTypeMismatch);
            exit;
          end;
        end;
        if not Result then begin
          CMD_Err(ErTypeMismatch);
          exit;
        end;
        SetBoolean(b, Result);
      end;
    1: begin { <= }
        case var1^.FType^.BaseType of
          btU8:
          if (var2^.FType^.BaseType = btString) or (Var2^.Ftype^.BaseType = btPChar) then
            b := char(var1^.tu8) <= GetString(Var2, Result)
          else
            b := var1^.tu8 <= GetUInt(Var2, Result);
          btS8: b := var1^.tS8 <= GetInt(Var2, Result);
          btU16: b := var1^.tu16 <= GetUInt(Var2, Result);
          btS16: b := var1^.ts16 <= GetInt(Var2, Result);
          btU32: b := var1^.tu32 <= GetUInt(Var2, Result);
          btS32: b := var1^.ts32 <= GetInt(Var2, Result);
          btSingle: b := var1^.tsingle <= GetReal(Var2, Result);
          btDouble: b := var1^.tdouble <= GetReal(Var2, Result);
          btExtended: b := var1^.textended <= GetReal(Var2, Result);
          {$IFNDEF NOINT64}
          btS64: b := var1^.ts64 <= GetInt64(Var2, Result);
          {$ENDIF}
          btPChar,btString: b := tbtstring(var1^.tstring) <= GetString(Var2, Result);
        else begin
            CMD_Err(ErTypeMismatch);
            exit;
          end;
        end;
        if not Result then begin
          CMD_Err(erTypeMismatch);
          exit;
        end;
        SetBoolean(b, Result);
      end;
    2: begin { > }
        case var1^.FType^.BaseType of
          btU8:
          if (var2^.FType^.BaseType = btString) or (Var2^.Ftype^.BaseType = btPChar) then
            b := char(var1^.tu8) > GetString(Var2, Result)
          else
            b := var1^.tu8 > GetUInt(Var2, Result);
          btS8: b := var1^.tS8 > GetInt(Var2, Result);
          btU16: b := var1^.tu16 > GetUInt(Var2, Result);
          btS16: b := var1^.ts16 > GetInt(Var2, Result);
          btU32: b := var1^.tu32 > GetUInt(Var2, Result);
          btS32: b := var1^.ts32 > GetInt(Var2, Result);
          btSingle: b := var1^.tsingle > GetReal(Var2, Result);
          btDouble: b := var1^.tdouble > GetReal(Var2, Result);
          btExtended: b := var1^.textended > GetReal(Var2, Result);
          {$IFNDEF NOINT64}
          btS64: b := var1^.ts64 > GetInt64(Var2, Result);
          {$ENDIF}
          btPChar,btString: b := tbtstring(var1^.tstring) > GetString(Var2, Result);
        else begin
            CMD_Err(erTypeMismatch);
            exit;
          end;
        end;
        if not Result then begin
          CMD_Err(erTypeMismatch);
          exit;
        end;
        SetBoolean(b, Result);
      end;
    3: begin { < }
        case var1^.FType^.BaseType of
          btU8:
          if (var2^.FType^.BaseType = btString) or (Var2^.Ftype^.BaseType = btPChar) then
            b := char(var1^.tu8) < GetString(Var2, Result)
          else
            b := var1^.tu8 < GetUInt(Var2, Result);
          btS8: b := var1^.tS8 < GetInt(Var2, Result);
          btU16: b := var1^.tu16 < GetUInt(Var2, Result);
          btS16: b := var1^.ts16 < GetInt(Var2, Result);
          btU32: b := var1^.tu32 < GetUInt(Var2, Result);
          btS32: b := var1^.ts32 < GetInt(Var2, Result);
          btSingle: b := var1^.tsingle < GetReal(Var2, Result);
          btDouble: b := var1^.tdouble < GetReal(Var2, Result);
          btExtended: b := var1^.textended < GetReal(Var2, Result);
          {$IFNDEF NOINT64}
          btS64: b := var1^.ts64 < GetInt64(Var2, Result);
          {$ENDIF}
          btPChar,btString: b := tbtstring(var1^.tstring) < GetString(Var2, Result);
        else begin
            CMD_Err(erTypeMismatch);
            exit;
          end;
        end;
        if not Result then begin
          CMD_Err(erTypeMismatch);
          exit;
        end;
        SetBoolean(b, Result);
      end;
    4: begin { <> }
        case var1^.FType^.BaseType of
          btU8:
          if (var2^.FType^.BaseType = btString) or (Var2^.Ftype^.BaseType = btPChar) then
            b := char(var1^.tu8) <> GetString(Var2, Result)
          else
            b := var1^.tu8 <> GetUInt(Var2, Result);
          btS8: b := var1^.tS8 <> GetInt(Var2, Result);
          btU16: b := var1^.tu16 <> GetUInt(Var2, Result);
          btS16: b := var1^.ts16 <> GetInt(Var2, Result);
          btU32: b := var1^.tu32 <> GetUInt(Var2, Result);
          btS32: b := var1^.ts32 <> GetInt(Var2, Result);
          btSingle: b := var1^.tsingle <> GetReal(Var2, Result);
          btDouble: b := var1^.tdouble <> GetReal(Var2, Result);
          btExtended: b := var1^.textended <> GetReal(Var2, Result);
          btPChar,btString: b := TbtString(var1^.tstring) <> GetString(Var2, Result);
          {$IFNDEF NOINT64}
          btS64: b := var1^.ts64 <> GetInt64(Var2, Result);
          {$ENDIF}
        else begin
            CMD_Err(erTypeMismatch);
            exit;
          end;
        end;
        if not Result then begin
          CMD_Err(erTypeMismatch);
          exit;
        end;
        SetBoolean(b, Result);
      end;
    5: begin { = }
        case var1^.FType^.BaseType of
          btU8:
          if (var2^.FType^.BaseType = btString) or (Var2^.Ftype^.BaseType = btPChar) then
            b := char(var1^.tu8) = GetString(Var2, Result)
          else
            b := var1^.tu8 = GetUInt(Var2, Result);
          btS8: b := var1^.tS8 = GetInt(Var2, Result);
          btU16: b := var1^.tu16 = GetUInt(Var2, Result);
          btS16: b := var1^.ts16 = GetInt(Var2, Result);
          btU32: b := var1^.tu32 = GetUInt(Var2, Result);
          btS32: b := var1^.ts32 = GetInt(Var2, Result);
          btSingle: b := var1^.tsingle = GetReal(Var2, Result);
          btDouble: b := var1^.tdouble = GetReal(Var2, Result);
          btExtended: b := var1^.textended = GetReal(Var2, Result);
          btPchar, btString: b := TbtString(var1^.tstring) = GetString(Var2, Result);
          {$IFNDEF NOINT64}
          btS64: b := var1^.ts64 = GetInt64(Var2, Result);
          {$ENDIF}
        else begin
            CMD_Err(erTypeMismatch);
            exit;
          end;
        end;
        if not Result then begin
          CMD_Err(erTypeMismatch);
          exit;
        end;
        SetBoolean(b, Result);
      end;
  else begin
      Result := False;
      CMD_Err(erInvalidOpcodeParameter);
      exit;
    end;
  end;
end;

function TSCExec.DoCalc(var1, Var2: PIfVariant; CalcType: Cardinal): Boolean;
    { var1=dest, var2=src }
begin
  try
    Result := True;
    case CalcType of
      0: begin { + }
          case var1^.FType^.BaseType of
            btU8: var1^.tu8 := var1^.tu8 + GetUInt(Var2, Result);
            btS8: var1^.tS8 := var1^.tS8 + GetInt(Var2, Result);
            btU16: var1^.tu16 := var1^.tu16 + GetUInt(Var2, Result);
            btS16: var1^.ts16 := var1^.ts16 + GetInt(Var2, Result);
            btU32: var1^.tu32 := var1^.tu32 + GetUInt(Var2, Result);
            btS32: var1^.ts32 := var1^.ts32 + GetInt(Var2, Result);
           {$IFNDEF NOINT64}
            btS64:  var1^.ts64 := var1^.ts64 + GetInt64(var2, Result);
           {$ENDIF}
            btSingle: var1^.tsingle := var1^.tsingle + GetReal(Var2, Result);
            btDouble: var1^.tdouble := var1^.tdouble + GetReal(Var2, Result);
            btExtended: var1^.textended := var1^.textended + GetReal(Var2,
                Result);
            btPchar, btString: TbtString((@var1^.tstring)^) :=
              TbtString((@var1^.tstring)^) + GetString(Var2, Result);
          else begin
              CMD_Err(erTypeMismatch);
              exit;
            end;
          end;
          if not Result then begin
            CMD_Err(erTypeMismatch);
            exit;
          end;
        end;
      1: begin { - }
          case var1^.FType^.BaseType of
            btU8: var1^.tu8 := var1^.tu8 - GetUInt(Var2, Result);
            btS8: var1^.tS8 := var1^.tS8 - GetInt(Var2, Result);
            btU16: var1^.tu16 := var1^.tu16 - GetUInt(Var2, Result);
            btS16: var1^.ts16 := var1^.ts16 - GetInt(Var2, Result);
            btU32: var1^.tu32 := var1^.tu32 - GetUInt(Var2, Result);
            btS32: var1^.ts32 := var1^.ts32 - GetInt(Var2, Result);
           {$IFNDEF NOINT64}
            btS64: var1^.ts64 := var1^.ts64 - GetInt64(var2, Result);
           {$ENDIF}
            btSingle: var1^.tsingle := var1^.tsingle - GetReal(Var2, Result);
            btDouble: var1^.tdouble := var1^.tdouble - GetReal(Var2, Result);
            btExtended: var1^.textended := var1^.textended - GetReal(Var2,
                Result);
          else begin
              CMD_Err(erTypeMismatch);
              exit;
            end;
          end;
          if not Result then begin
            CMD_Err(erTypeMismatch);
            exit;
          end;
        end;
      2: begin { * }
          case var1^.FType^.BaseType of
            btU8: var1^.tu8 := var1^.tu8 * GetUInt(Var2, Result);
            btS8: var1^.tS8 := var1^.tS8 * GetInt(Var2, Result);
            btU16: var1^.tu16 := var1^.tu16 * GetUInt(Var2, Result);
            btS16: var1^.ts16 := var1^.ts16 * GetInt(Var2, Result);
            btU32: var1^.tu32 := var1^.tu32 * GetUInt(Var2, Result);
            btS32: var1^.ts32 := var1^.ts32 * GetInt(Var2, Result);
           {$IFNDEF NOINT64}
            btS64: var1^.ts64 := var1^.ts64 * GetInt64(var2, Result);
           {$ENDIF}
            btSingle: var1^.tsingle := var1^.tsingle * GetReal(Var2, Result);
            btDouble: var1^.tdouble := var1^.tdouble * GetReal(Var2, Result);
            btExtended: var1^.textended := var1^.textended * GetReal(Var2,
                Result);
          else begin
              CMD_Err(erTypeMismatch);
              exit;
            end;
          end;
          if not Result then begin
            CMD_Err(erTypeMismatch);
            exit;
          end;
        end;
      3: begin { / }
          case var1^.FType^.BaseType of
            btU8: var1^.tu8 := var1^.tu8 div GetUInt(Var2, Result);
            btS8: var1^.tS8 := var1^.tS8 div GetInt(Var2, Result);
            btU16: var1^.tu16 := var1^.tu16 div GetUInt(Var2, Result);
            btS16: var1^.ts16 := var1^.ts16 div GetInt(Var2, Result);
            btU32: var1^.tu32 := var1^.tu32 div GetUInt(Var2, Result);
            btS32: var1^.ts32 := var1^.ts32 div GetInt(Var2, Result);
           {$IFNDEF NOINT64}
            btS64: var1^.ts64 := var1^.ts64 div GetInt64(var2, Result);
           {$ENDIF}
            btSingle: var1^.tsingle := var1^.tsingle / GetReal(Var2, Result);
            btDouble: var1^.tdouble := var1^.tdouble / GetReal(Var2, Result);
            btExtended: var1^.textended := var1^.textended / GetReal(Var2, Result);
          else begin
              CMD_Err(erTypeMismatch);
              exit;
            end;
          end;
          if not Result then begin
            CMD_Err(erTypeMismatch);
            exit;
          end;
        end;
      4: begin { MOD }
          case var1^.FType^.BaseType of
            btU8: var1^.tu8 := var1^.tu8 mod GetUInt(Var2, Result);
            btS8: var1^.tS8 := var1^.tS8 mod GetInt(Var2, Result);
            btU16: var1^.tu16 := var1^.tu16 mod GetUInt(Var2, Result);
            btS16: var1^.ts16 := var1^.ts16 mod GetInt(Var2, Result);
            btU32: var1^.tu32 := var1^.tu32 mod GetUInt(Var2, Result);
            btS32: var1^.ts32 := var1^.ts32 mod GetInt(Var2, Result);
           {$IFNDEF NOINT64}
            btS64: var1^.ts64 := var1^.ts64 mod GetInt64(var2, Result);
           {$ENDIF}
          else begin
              CMD_Err(erTypeMismatch);
              exit;
            end;
          end;
          if not Result then begin
            CMD_Err(erTypeMismatch);
            exit;
          end;
        end;
      5: begin { SHL }
          case var1^.FType^.BaseType of
            btU8: var1^.tu8 := var1^.tu8 shl GetUInt(Var2, Result);
            btS8: var1^.tS8 := var1^.tS8 shl GetInt(Var2, Result);
            btU16: var1^.tu16 := var1^.tu16 shl GetUInt(Var2, Result);
            btS16: var1^.ts16 := var1^.ts16 shl GetInt(Var2, Result);
            btU32: var1^.tu32 := var1^.tu32 shl GetUInt(Var2, Result);
            btS32: var1^.ts32 := var1^.ts32 shl GetInt(Var2, Result);
           {$IFNDEF NOINT64}
            btS64: var1^.ts64 := var1^.ts64 shl GetInt64(var2, Result);
           {$ENDIF}
          else begin
              CMD_Err(erTypeMismatch);
              exit;
            end;
          end;
          if not Result then begin
            CMD_Err(erTypeMismatch);
            exit;
          end;
        end;
      6: begin { SHR }
          case var1^.FType^.BaseType of
            btU8: var1^.tu8 := var1^.tu8 shr GetUInt(Var2, Result);
            btS8: var1^.tS8 := var1^.tS8 shr GetInt(Var2, Result);
            btU16: var1^.tu16 := var1^.tu16 shr GetUInt(Var2, Result);
            btS16: var1^.ts16 := var1^.ts16 shr GetInt(Var2, Result);
            btU32: var1^.tu32 := var1^.tu32 shr GetUInt(Var2, Result);
            btS32: var1^.ts32 := var1^.ts32 shr GetInt(Var2, Result);
           {$IFNDEF NOINT64}
            btS64: var1^.ts64 := var1^.ts64 shr GetInt64(var2, Result);
           {$ENDIF}
          else begin
              CMD_Err(erTypeMismatch);
              exit;
            end;
          end;
          if not Result then begin
            CMD_Err(erTypeMismatch);
            exit;
          end;
        end;
      7: begin { AND }
          case var1^.FType^.BaseType of
            btU8: var1^.tu8 := var1^.tu8 and GetUInt(Var2, Result);
            btS8: var1^.tS8 := var1^.tS8 and GetInt(Var2, Result);
            btU16: var1^.tu16 := var1^.tu16 and GetUInt(Var2, Result);
            btS16: var1^.ts16 := var1^.ts16 and GetInt(Var2, Result);
            btU32: var1^.tu32 := var1^.tu32 and GetUInt(Var2, Result);
            btS32: var1^.ts32 := var1^.ts32 and GetInt(Var2, Result);
           {$IFNDEF NOINT64}
            btS64: var1^.ts64 := var1^.ts64 and GetInt64(var2, Result);
           {$ENDIF}
          else begin
              CMD_Err(erTypeMismatch);
              exit;
            end;
          end;
          if not Result then begin
            CMD_Err(erTypeMismatch);
            exit;
          end;
        end;
      8: begin { OR }
          case var1^.FType^.BaseType of
            btU8: var1^.tu8 := var1^.tu8 or GetUInt(Var2, Result);
            btS8: var1^.tS8 := var1^.tS8 or GetInt(Var2, Result);
            btU16: var1^.tu16 := var1^.tu16 or GetUInt(Var2, Result);
            btS16: var1^.ts16 := var1^.ts16 or GetInt(Var2, Result);
            btU32: var1^.tu32 := var1^.tu32 or GetUInt(Var2, Result);
            btS32: var1^.ts32 := var1^.ts32 or GetInt(Var2, Result);
           {$IFNDEF NOINT64}
            btS64: var1^.ts64 := var1^.ts64 or GetInt64(var2, Result);
           {$ENDIF}
          else begin
              CMD_Err(erTypeMismatch);
              exit;
            end;
          end;
          if not Result then begin
            CMD_Err(erTypeMismatch);
            exit;
          end;
        end;
      9: begin { XOR }
          case var1^.FType^.BaseType of
            btU8: var1^.tu8 := var1^.tu8 xor GetUInt(Var2, Result);
            btS8: var1^.tS8 := var1^.tS8 xor GetInt(Var2, Result);
            btU16: var1^.tu16 := var1^.tu16 xor GetUInt(Var2, Result);
            btS16: var1^.ts16 := var1^.ts16 xor GetInt(Var2, Result);
            btU32: var1^.tu32 := var1^.tu32 xor GetUInt(Var2, Result);
            btS32: var1^.ts32 := var1^.ts32 xor GetInt(Var2, Result);
           {$IFNDEF NOINT64}
            btS64: var1^.ts64 := var1^.ts64 xor GetInt64(var2, Result);
           {$ENDIF}
          else begin
              CMD_Err(erTypeMismatch);
              exit;
            end;
          end;
          if not Result then begin
            CMD_Err(erTypeMismatch);
            exit;
          end;
        end;
    else begin
        Result := False;
        CMD_Err(erInvalidOpcodeParameter);
        exit;
      end;
    end;
  except
    on E: EDivByZero do
    begin
      Result := False;
      CMD_Err(erDivideByZero);
      Exit;
    end;
    on E: EZeroDivide do
    begin
      Result := False;
      CMD_Err(erDivideByZero);
      Exit;
    end;
    on E: EMathError do
    begin
      Result := False;
      CMD_Err(erMathError);
      Exit;
    end;
    on E: Exception do
    begin
      Result := False;
      CMD_Err2(erException, e.Message);
      exit;
    end;
  end;
end;

function TSCExec.ReadVariable(var NeedToFree: LongBool; UsePointer: LongBool): PIfVariant;
var
  VarType: Cardinal;
  Param: Cardinal;
  Tmp: PIfVariant;

begin
  if not (ReadByte(VarType) and ReadLong(Param)) then begin
    CMD_Err(erOutOfRange);
    Result := nil;
    exit;
  end;
  case VarType of
    0: begin
        NeedToFree := False;
        if Param < SCAddrNegativeStackStart then begin
          Result := FGlobalVars.GetItem(Param);
          if Result = nil then begin
            CMD_Err(erOutOfGlobalVarsRange);
            exit;
          end;
        end
        else begin
          Result := FStack.GetItem(Cardinal(Longint(-SCAddrStackStart) +
            Longint(FCurrStackBase) + Longint(Param)));
          if Result = nil then begin
            CMD_Err(erOutOfStackRange);
            exit;
          end;
        end;
        if UsePointer then
        begin
          if Result^.FType^.BaseType = btPointer then begin
            Result := Result^.tPointer;
            if Result = nil then begin
              CMD_Err(erNullPointerException);
              exit;
            end;
          end;
          if Result^.FType^.BaseType = btVariant then begin
            Result := Result^.tvariant;         
            if Result = nil then begin
              CMD_Err(erNullPointerException);
              exit;
            end;
            if Result^.FType = nil then
            begin
              Result := nil;
              CMD_Err(erNullVariantError);
              Exit;
            end;
          end;
        end;
      end;
    1: begin
        NeedToFree := True;
        Result := CreateVariant({$IFNDEF NOSMARTMM}MM,
{$ENDIF}FTypes.GetItem(Param));
        if Result = nil then begin
          CMD_Err(erInvalidType);
          exit;
        end;
        case Result^.FType^.BaseType of
          btU8: if not ReadData(Result^.tu8, 1) then begin
              CMD_Err(erOutOfRange);
              DisposeVariant({$IFNDEF NOSMARTMM}MM, {$ENDIF}Result);
              Result := nil;
              exit;
            end;
          btS8: if not ReadData(Result^.tS8, 1) then begin
              CMD_Err(erOutOfRange);
              DisposeVariant({$IFNDEF NOSMARTMM}MM, {$ENDIF}Result);
              Result := nil;
              exit;
            end;
          btU16: if not ReadData((@Result^.tu16)^, SizeOf(TbtU16)) then begin
              CMD_Err(ErOutOfRange);
              DisposeVariant({$IFNDEF NOSMARTMM}MM, {$ENDIF}Result);
              Result := nil;
              exit;
            end;
          btS16: if not ReadData((@Result^.ts16)^, SizeOf(TbtS16)) then begin
              CMD_Err(ErOutOfRange);
              DisposeVariant({$IFNDEF NOSMARTMM}MM, {$ENDIF}Result);
              Result := nil;
              exit;
            end;
          btU32: if not ReadLong(Result^.tu32) then begin
              CMD_Err(erOutOfRange);
              DisposeVariant({$IFNDEF NOSMARTMM}MM, {$ENDIF}Result);
              Result := nil;
              exit;
            end;
          btS32: if not ReadLong(Cardinal(Result^.ts32)) then begin
              CMD_Err(erOutOfRange);
              DisposeVariant({$IFNDEF NOSMARTMM}MM, {$ENDIF}Result);
              Result := nil;
              exit;
            end;
          {$IFNDEF NOINT64}
          bts64: if not ReadData(Result^.ts64, sizeof(tbts64)) then
            begin
              CMD_Err(erOutOfRange);
              DisposeVariant({$IFNDEF NOSMARTMM}MM, {$ENDIF}Result);
              Result := nil;
              exit;
            end;
          {$ENDIF}
          btSingle: if not ReadData((@Result^.tsingle)^, SizeOf(TbtSingle))
            then begin
              CMD_Err(erOutOfRange);
              DisposeVariant({$IFNDEF NOSMARTMM}MM, {$ENDIF}Result);
              Result := nil;
              exit;
            end;
          btDouble: if not ReadData((@Result^.tdouble)^, SizeOf(TbtDouble))
            then begin
              CMD_Err(erOutOfRange);
              DisposeVariant({$IFNDEF NOSMARTMM}MM, {$ENDIF}Result);
              Result := nil;
              exit;
            end;
          btExtended: if not ReadData((@Result^.textended)^,
              SizeOf(TbtExtended)) then begin
              CMD_Err(erOutOfRange);
              DisposeVariant({$IFNDEF NOSMARTMM}MM, {$ENDIF}Result);
              Result := nil;
              exit;
            end;
          btPchar, btString: begin
              if not ReadLong(Param) then begin
                CMD_Err(erOutOfRange);
                DisposeVariant({$IFNDEF NOSMARTMM}MM, {$ENDIF}Result);
                Result := nil;
                exit;
              end;
              SetLength(TbtString((@Result^.tstring)^), Param);
              if not ReadData(TbtString((@Result^.tstring)^)[1], Param) then begin
                CMD_Err(erOutOfRange);
                DisposeVariant({$IFNDEF NOSMARTMM}MM, {$ENDIF}Result);
                Result := nil;
                exit;
              end;
            end;
        else begin
            CMD_Err(erInvalidType);
            DisposeVariant({$IFNDEF NOSMARTMM}MM, {$ENDIF}Result);
            Result := nil;
            exit;
          end;
        end;
      end;
    2: begin
        NeedToFree := False;
        if Param < SCAddrNegativeStackStart then begin
          Result := FGlobalVars.GetItem(Param);
          if Result = nil then begin
            CMD_Err(erOutOfGlobalVarsRange);
            exit;
          end;
        end
        else begin
          Result := FStack.GetItem(Cardinal(Longint(-SCAddrStackStart) +
            Longint(FCurrStackBase) + Longint(Param)));
          if Result = nil then begin
            CMD_Err(erOutOfStackRange);
            exit;

          end;
        end;
        if (Result^.FType^.BaseType = btPointer) then begin
          Result := Result^.tPointer;
          if Result = nil then begin
            CMD_Err(erNullPointerException);
            exit;
          end;
        end;
        if Result^.FType^.BaseType = btVariant then begin
          Result := Result^.tvariant;
          if Result = nil then begin
            CMD_Err(erNullPointerException);
            exit;
          end;
          if Result^.FType = nil then
          begin
            Result := nil;
            CMD_Err(erNullVariantError);
            Exit;
          end;
        end;
        if (Result^.FType^.BaseType <> btRecord) and (Result^.FType^.BaseType <> btArray) then begin
          CMD_Err(erInvalidType);
          Result := nil;
          exit;
        end;
        if not ReadLong(Param) then begin
          CMD_Err(erOutOfRange);
          Result := nil;
          exit;
        end;
        if (Result^.trecord = nil) or (Param >= pbtrecord(Result^.trecord)^.FieldCount) then begin
          CMD_Err(erOutofRecordRange);
          Result := nil;
          exit;
        end;
        Result := pbtrecord(Result^.trecord)^.Fields[Param];
        if UsePointer then
        begin
          if Result^.FType^.BaseType = btPointer then begin
            Result := Result^.tPointer;
            if Result = nil then begin
              CMD_Err(erNullPointerException);
              exit;
            end;
          end;
          if Result^.FType^.BaseType = btVariant then begin
            Result := Result^.tvariant;
            if Result = nil then begin
              CMD_Err(erNullPointerException);
              exit;
            end;
            if Result^.FType = nil then
            begin
              Result := nil;
              CMD_Err(erNullVariantError);
              Exit;
            end;
          end;
        end;
      end;
    3: begin
        NeedToFree := False;
        if Param < SCAddrNegativeStackStart then begin
          Result := FGlobalVars.GetItem(Param);
          if Result = nil then begin
            CMD_Err(erOutOfGlobalVarsRange);
            exit;
          end;
        end
        else begin
          Result := FStack.GetItem(Cardinal(Longint(-SCAddrStackStart) +
            Longint(FCurrStackBase) + Longint(Param)));
          if Result = nil then begin
            CMD_Err(erOutOfStackRange);
            exit;
          end;
        end;
        if (Result^.FType^.BaseType = btPointer) then begin
          Result := Result^.tPointer;
          if Result = nil then begin
            CMD_Err(erNullPointerException);
            exit;
          end;
        end;
        if Result^.FType^.BaseType = btVariant then begin
          Result := Result^.tvariant;
          if Result = nil then begin
            CMD_Err(erNullPointerException);
            exit;
          end;
          if Result^.FType = nil then
          begin
            Result := nil;
            CMD_Err(erNullVariantError);
            Exit;
          end;
        end;
        if (Result^.FType^.BaseType <> btRecord) and (Result^.FType^.BaseType <> btArray) then begin
          CMD_Err(erInvalidType);
          Result := nil;
          exit;
        end;
        if not ReadLong(Param) then begin
          CMD_Err(erOutOfRange);
          Result := nil;
          exit;
        end;
        if Param < SCAddrNegativeStackStart then begin
          Tmp := FGlobalVars.GetItem(Param);
          if Tmp = nil then begin
            CMD_Err(erOutOfGlobalVarsRange);
            exit;
          end;
        end
        else begin
          Tmp := FStack.GetItem(Cardinal(Longint(-SCAddrStackStart) + Longint(FCurrStackBase) + Longint(Param)));
          if Tmp = nil then begin
            CMD_Err(erOutOfStackRange);
            exit;
          end;
        end;
        case Tmp^.FType^.BaseType of
          btu8: Param := Tmp^.tu8;
          bts8: Param := Tmp^.ts8;
          btu16: Param := Tmp^.tu16;
          bts16: Param := Tmp^.ts16;
          btu32: Param := Tmp^.tu32;
          bts32: Param := Tmp^.ts32;
        else
          CMD_Err(ErTypeMismatch);
          exit;
        end;

        if (Result^.trecord = nil) or (Param >= pbtrecord(Result^.trecord)^.FieldCount) then begin
          CMD_Err(erOutofRecordRange);
          Result := nil;
          exit;
        end;
        Result := pbtrecord(Result^.trecord)^.Fields[Param];
        if UsePointer then
        begin
          if Result^.FType^.BaseType = btPointer then begin
            Result := Result^.tPointer;
            if Result = nil then begin
              CMD_Err(erNullPointerException);
              exit;
            end;
          end;
          if Result^.FType^.BaseType = btVariant then begin
            if Result = nil then begin
              CMD_Err(erNullPointerException);
              exit;
            end;
            if Result^.FType = nil then
            begin
              Result := nil;
              CMD_Err(erNullVariantError);
              Exit;
            end;
          end;
        end;
      end;
  else
    Result := nil;
  end;
end;

procedure TSCExec.DoMinus(Vd: PIfVariant);
begin
  case Vd^.FType^.BaseType of
    btU8: Vd^.tu8 := -Vd^.tu8;
    btU16: Vd^.tu8 := -Vd^.tu16;
    btU32: Vd^.tu8 := -Vd^.tu32;
    btS8: Vd^.tS8 := -Vd^.tS8;
    btS16: Vd^.ts16 := -Vd^.ts16;
    btS32: Vd^.ts32 := -Vd^.ts32;
    btSingle: Vd^.tsingle := - vd^.tsingle;
    btDouble: Vd^.tdouble := -vd^.tdouble;
    btExtended: Vd^.textended := -vd^.textended;
  else
    CMD_Err(erTypeMismatch);
  end;
end;

procedure TSCExec.DoBooleanNot(Vd: PIfVariant);
begin
  case Vd^.FType^.BaseType of
    btU8: Vd^.tu8 := TbtU8(Vd^.tu8 = 0);
    btS8: Vd^.tS8 := TbtS8(Vd^.tS8 = 0);
    btU16: Vd^.tu16 := TbtU16(Vd^.tu16 = 0);
    btS16: Vd^.ts16 := TbtS16(Vd^.ts16 = 0);
    btU32: Vd^.tu32 := TbtU32(Vd^.tu32 = 0);
    btS32: Vd^.ts32 := TbtS32(Vd^.ts32 = 0);
  else
    CMD_Err(erTypeMismatch);
  end;
end;

function TSCExec.RunScript: Boolean;
var
  CalcType: Cardinal;
  Vd, Vs, v3: PIfVariant;
  vdFree, vsFree: LongBool;
  p: Cardinal;
  P2: Longint;
  u: PIFProcRec;
  Cmd: Cardinal;
  I: Longint;
  pp: PSCExceptionHandler;
  FExitPoint: Cardinal;

begin
  FExitPoint := Cardinal(-1);
  for i := FExceptionStack.Count -1 downto 0 do
  begin
    pp := FExceptionStack.GetItem(i);
    Dispose(pp);
  end;
  FExceptionStack.Clear;
  ExceptionProc(Cardinal(-1), Cardinal(-1), erNoError, '');
  RunScript := True;
  case FStatus of
    isLoaded: begin
        if FMainProc = Cardinal(-1) then
        begin
          RunScript := False;
          exit;
        end;
        FStatus := isRunning;
        FCurrProc := FProcs.GetItem(FMainProc);
        if FCurrProc^.ExternalProc then begin
          CMD_Err(erNoMainProc);
          FStatus := isLoaded;
          exit;
        end;
        FCurrStackBase := Cardinal(-1);
        FCurrentPosition := 0;
      end;
    isPaused: begin
        FStatus := isRunning;
      end;
  else begin
      RunScript := False;
      exit;
    end;
  end;
  RunLine;
  repeat
    FStatus := isRunning;
    while FStatus = isRunning do begin
      if not ReadByte(Cmd) then
        CMD_Err(erOutOfRange) // Error
      else begin
        if Cmd = CM_CA then begin // Calc and assigning are needed most and have priority
          if not ReadByte(CalcType) then begin
            CMD_Err(erOutOfRange);
            break;
          end;
          Vd := ReadVariable(vdFree, True);
          if Vd = nil then
            break;
          if vdFree then begin
            DisposeVariant({$IFNDEF NOSMARTMM}MM, {$ENDIF}Vd);
            CMD_Err(erInvalidOpcodeParameter);
            break;
          end;
          Vs := ReadVariable(vsFree, True);
          if Vs = nil then
            break;
          if not DoCalc(Vd, Vs, CalcType) then Break;
          if vsFree then
            DisposeVariant({$IFNDEF NOSMARTMM}MM, {$ENDIF}Vs);
        end
        else if Cmd = CM_A then begin // Calc and assigning are needed most and have priority
          Vd := ReadVariable(vdFree, False);
          if Vd = nil then
            break;
          if vdFree then begin
            DisposeVariant({$IFNDEF NOSMARTMM}MM, {$ENDIF}Vd);
            CMD_Err(erInvalidOpcodeParameter);
            break;
          end;
          if vd^.FType^.BaseType = btPointer then
          begin
            vd := vd^.tPointer;
            if vd = nil then
            begin
              CMD_Err(erNullPointerException);
              Break;
            end;
          end;
          Vs := ReadVariable(vsFree, False);
          if Vs = nil then
            break;
          if vs^.FType^.BaseType = btPointer then begin
            v3 := vs^.tPointer;
            if v3 = nil then begin
              if vsFree then
              begin
                DisposeVariant({$IFNDEF NOSMARTMM}MM, {$ENDIF}Vs);
              end;
              CMD_Err(erNullPointerException);
              Break;
            end;
            vs := v3;
          end;
          if not SetVariantValue(Vd, Vs) then
          begin
            if vsFree then
              DisposeVariant({$IFNDEF NOSMARTMM}MM, {$ENDIF}Vs);
            cmd_err(erTypeMismatch);
            Break;
          end;
          if vsFree then
            DisposeVariant({$IFNDEF NOSMARTMM}MM, {$ENDIF}Vs);
        end
        else
          case Cmd of
            CM_P: begin
                Vs := ReadVariable(vsFree, True);
                if Vs = nil then
                  break;
                if vsFree then begin
                  FStack.Add(Vs);
                end
                else begin
                  Vd := CreateVariant({$IFNDEF NOSMARTMM}MM, {$ENDIF}Vs^.FType);
                  SetVariantValue(Vd, Vs);
                  FStack.Add(Vd);
                end;
              end;
            CM_PV: begin
                Vs := ReadVariable(vsFree, False);
                if vs^.FType^.BaseType = btPointer then
                begin
                  vs := vs^.tPointer;
                  if vs = nil then
                  begin
                    CMD_Err(erNullPointerException);
                    break;
                  end;
                end;

                if Vs = nil then
                  break;
                if vsFree then begin
                  DisposeVariant({$IFNDEF NOSMARTMM}MM, {$ENDIF}Vs);
                  CMD_Err(erInvalidOpcodeParameter);
                  break;
                end
                else begin
                  Inc(Vs^.RefCount);
                  FStack.Add(Vs);
                end;
              end;
            CM_PO: begin
                if FStack.Count = 0 then begin
                  CMD_Err(erOutOfStackRange);
                  break;
                end;
                Vs := FStack.GetItem(FStack.Count - 1);
                FStack.Delete(FStack.Count - 1);
                DisposeVariant({$IFNDEF NOSMARTMM}MM, {$ENDIF}Vs);
              end;
            Cm_C: begin
                if not ReadLong(p) then begin
                  CMD_Err(erOutOfRange);
                  break;
                end;
                if p >= FProcs.Count then begin
                  CMD_Err(erOutOfProcRange);
                  break;
                end;
                u := FProcs.GetItem(p);
                if u^.ExternalProc then begin
                  if not u^.ProcPtr(Self, u, FGlobalVars, FStack) then
                    CMD_Err(erCouldNotCallProc);
                end
                else begin
                  Vd := CreateVariant({$IFNDEF NOSMARTMM}MM,{$ENDIF}@ReturnAddressType);
                  Vd^.treturnaddress.ProcNo := FCurrProc;
                  Vd^.treturnaddress.Position := FCurrentPosition;
                  Vd^.treturnaddress.StackBase := FCurrStackBase;
                  FStack.Add(Vd);

                  FCurrStackBase := FStack.Count - 1;
                  FCurrProc := u;
                  FCurrentPosition := 0;
                end;
              end;
            Cm_G: begin
                if not ReadLong(p) then begin
                  CMD_Err(erOutOfRange);
                  break;
                end;
                FCurrentPosition := FCurrentPosition + p;
              end;
            Cm_CG: begin
                if not ReadLong(p) then begin
                  CMD_Err(erOutOfRange);
                  break;
                end;
                Vs := ReadVariable(vsFree, True);
                if Vs = nil then
                  break;
                if vsFree then begin
                  CMD_Err(erInvalidOpcodeParameter);
                  break;
                end;
                case Vs^.FType^.BaseType of
                  btU8: vdFree := Vs^.tu8 <> 0;
                  btS8: vdFree := Vs^.tS8 <> 0;
                  btU16: vdFree := Vs^.tu16 <> 0;
                  btS16: vdFree := Vs^.ts16 <> 0;
                  btU32: vdFree := Vs^.tu32 <> 0;
                  btS32: vdFree := Vs^.ts32 <> 0;
                else begin
                    CMD_Err(erInvalidType);
                    break;
                  end;
                end;
                if vdFree then
                  FCurrentPosition := FCurrentPosition + p;
              end;
            Cm_CNG: begin
                if not ReadLong(p) then begin
                  CMD_Err(erOutOfRange);
                  break;
                end;
                Vs := ReadVariable(vsFree, True);
                if Vs = nil then
                  break;
                if vsFree then begin
                  CMD_Err(erInvalidOpcodeParameter);
                  break;
                end;
                case Vs^.FType^.BaseType of
                  btU8: vdFree := Vs^.tu8 = 0;
                  btS8: vdFree := Vs^.tS8 = 0;
                  btU16: vdFree := Vs^.tu16 = 0;
                  btS16: vdFree := Vs^.ts16 = 0;
                  btU32: vdFree := Vs^.tu32 = 0;
                  btS32: vdFree := Vs^.ts32 = 0;
                else begin
                    CMD_Err(erInvalidType);
                    break;
                  end;
                end;
                if vdFree then
                  FCurrentPosition := FCurrentPosition + p;
              end;
            Cm_R: begin
                FExitPoint := FCurrentPosition -1;
                P2 := 0;
                if FExceptionStack.Count > 0 then
                begin
                  pp := FExceptionStack.GetItem(FExceptionStack.Count -1);
                  if pp^.BasePtr >= FCurrStackBase then
                  begin
                    if pp^.StackSize < FStack.Count then
                    begin
                      for p := Longint(FStack.count) -1 downto Longint(pp^.StackSize) do
                      begin
                        DisposeVariant({$IFNDEF SMARTMM}mm, {$ENDIF}FStack.GetItem(p));
                        FStack.Delete(p);
                      end;
                    end;
                    FCurrStackBase := pp^.BasePtr;
                    if pp^.FinallyOffset <> Cardinal(-1) then
                    begin
                      FCurrentPosition := pp^.FinallyOffset;
                      pp^.FinallyOffset := cardinal(-1);
                      p2 := 1;
                    end else if pp^.Finally2Offset <> Cardinal(-1) then
                    begin
                      FCurrentPosition := pp^.Finally2Offset;
                      pp^.Finally2Offset := cardinal(-1);
                      p2 := 1;
                    end;
                  end;
                end;
                if p2 = 0 then
                begin
                  FExitPoint := Cardinal(-1);
                  Vs := FStack.GetItem(FCurrStackBase);
                  if Vs = nil then begin
                    FStatus := isLoaded;
                    break;
                  end;
                  for P2 := FStack.Count - 1 downto FCurrStackBase + 1 do begin
                    DisposeVariant({$IFNDEF NOSMARTMM}MM,
    {$ENDIF}FStack.GetItem(P2));
                    FStack.Delete(P2);
                  end;
                  FStack.Delete(FCurrStackBase);
                  FCurrProc := Vs^.treturnaddress.ProcNo;
                  FCurrentPosition := Vs^.treturnaddress.Position;
                  FCurrStackBase := Vs^.treturnaddress.StackBase;
                  DisposeVariant({$IFNDEF NOSMARTMM}MM, {$ENDIF}Vs);
                  if FCurrProc = nil then begin
                    FStatus := isPaused;
                    break;
                  end;
                end;
              end;
            Cm_ST: begin
                if not ReadLong(p) or not ReadLong(Cardinal(P2)) then begin
                  CMD_Err(erOutOfRange);
                  break;
                end;
                Cardinal(P2) := FCurrStackBase + Cardinal(P2);
                if p >= FTypes.Count then begin
                  CMD_Err(erInvalidType);
                  break;
                end;
                if Cardinal(P2) >= FStack.Count then begin
                  CMD_Err(erOutOfStackRange);
                  break;
                end;
                Vs := FStack.GetItem(Cardinal(P2));
                if Vs^.FType = @ReturnAddressType then begin
                  CMD_Err(erInvalidType);
                  break;
                end;
                DisposeVariant({$IFNDEF NOSMARTMM}MM, {$ENDIF}Vs);
                Vs := CreateVariant({$IFNDEF NOSMARTMM}MM,
  {$ENDIF}FTypes.GetItem(p));
                FStack.SetItem(Cardinal(P2), Vs);
              end;
            Cm_Pt: begin
                if not ReadLong(p) then begin
                  CMD_Err(erInvalidType);
                  break;
                end;
                Vs := CreateVariant({$IFNDEF NOSMARTMM}MM,
  {$ENDIF}FTypes.GetItem(p));
                if Vs = nil then begin
                  CMD_Err(erInvalidType);
                  break;
                end;
                FStack.Add(Vs);
              end;
            CM_CO: begin
                if not ReadByte(CalcType) then begin
                  CMD_Err(erOutOfRange);
                  break;
                end;
                v3 := ReadVariable(vsFree, True);
                if v3 = nil then
                  break;
                if vsFree then begin
                  DisposeVariant({$IFNDEF NOSMARTMM}MM, {$ENDIF}v3);
                  CMD_Err(erInvalidOpcodeParameter);
                  break;
                end;
                Vs := ReadVariable(vsFree, False);
                if Vs = nil then
                  break;
                if vs^.FType^.BaseType = btPointer then begin
                  vs := vs^.tPointer;
                  if vs = nil then begin
                    CMD_Err(erNullPointerException);
                    break;
                  end;
                end;
                if vs^.FType^.BaseType = btVariant then begin
                  vs := vs^.tvariant;
                end;
                Vd := ReadVariable(vdFree, False);
                if vd^.FType^.BaseType = btPointer then begin
                  vd := vs^.tPointer;
                  if vd = nil then begin
                    CMD_Err(erNullPointerException);
                    break;
                  end;
                end;
                if vd^.FType^.BaseType = btVariant then begin
                  vd := vd^.tvariant;
                end;
                if Vd = nil then begin
                  if vsFree then
                    DisposeVariant({$IFNDEF NOSMARTMM}MM, {$ENDIF}Vs);
                  break;
                end;
                DoBooleanCalc(Vs, Vd, v3, CalcType);
                if vsFree then
                  DisposeVariant({$IFNDEF NOSMARTMM}MM, {$ENDIF}Vs);
                if vdFree then
                  DisposeVariant({$IFNDEF NOSMARTMM}MM, {$ENDIF}Vd);
              end;
            Cm_cv: begin
                Vd := ReadVariable(vdFree, True);
                if Vd = nil then
                  break;
                if (Vd^.FType^.BaseType <> btU32) and (Vd^.FType^.BaseType <>
                  btS32) then begin
                  if vdFree then
                    DisposeVariant({$IFNDEF NOSMARTMM}MM, {$ENDIF}Vd);
                  CMD_Err(ErTypeMismatch);
                  break;
                end;
                p := Vd^.tu32;
                if vdFree then
                  DisposeVariant({$IFNDEF NOSMARTMM}MM, {$ENDIF}Vd);
                if (p >= FProcs.Count) or (p = FMainProc) then begin
                  CMD_Err(erOutOfProcRange);
                  break;
                end;
                u := FProcs.GetItem(p);
                if u^.ExternalProc then begin
                  if not u^.ProcPtr(Self, u, FGlobalVars, FStack) then
                    CMD_Err(erCouldNotCallProc);
                end
                else begin
                  Vs := CreateVariant({$IFNDEF NOSMARTMM}MM,
  {$ENDIF}@ReturnAddressType);
                  Vs^.treturnaddress.ProcNo := FCurrProc;
                  Vs^.treturnaddress.Position := FCurrentPosition;
                  Vs^.treturnaddress.StackBase := FCurrStackBase;
                  FStack.Add(Vs);
                  FCurrStackBase := FStack.Count - 1;
                  FCurrProc := u;
                  FCurrentPosition := 0;
                end;
              end;
            cm_sp: begin
                Vd := ReadVariable(vdFree, False);
                if Vd = nil then
                  break;
                if vdFree then begin
                  DisposeVariant({$IFNDEF NOSMARTMM}MM, {$ENDIF}Vd);
                  CMD_Err(erInvalidOpcodeParameter);
                  break;
                end;
                if Vd^.FType^.BaseType <> btPointer then begin
                  CMD_Err(erInvalidOpcodeParameter);
                  break;
                end;
                if (Vd^.tPointer <> nil) then
                  DisposeVariant({$IFNDEF NOSMARTMM}MM, {$ENDIF}Vd^.tPointer);
                Vs := ReadVariable(vsFree, False);
                if Vs = nil then begin
                  Vd^.tPointer := nil;
                end else if vsFree then begin
                  DisposeVariant({$IFNDEF NOSMARTMM}MM, {$ENDIF}Vs);
                  CMD_Err(erInvalidOpcodeParameter);
                  break;
                end else begin
                  Inc(Vs^.RefCount);
                  Vd^.tPointer := Vs;
                end;
              end;
            cm_bn: begin
                Vd := ReadVariable(vdFree, False);
                if Vd = nil then
                  break;
                if vdFree then begin
                  DisposeVariant({$IFNDEF NOSMARTMM}MM, {$ENDIF}Vd);
                  CMD_Err(erInvalidOpcodeParameter);
                  break;
                end;
                DoBooleanNot(Vd);
              end;
            cm_vm: begin
                Vd := ReadVariable(vdFree, False);
                if Vd = nil then
                  break;
                if vdFree then begin
                  DisposeVariant({$IFNDEF NOSMARTMM}MM, {$ENDIF}Vd);
                  CMD_Err(erInvalidOpcodeParameter);
                  break;
                end;
                DoMinus(Vd);
              end;
            cm_sf:
              begin
                vd := ReadVariable(vdFree, True);
                if vd = nil then
                  break;
                if vdFree then
                begin
                  DisposeVariant({$IFNDEF NOSMARTMM}MM, {$ENDIF}vd);
                  CMD_Err(erInvalidOpcodeParameter);
                  break;
                end;
                if not ReadByte(p) then
                begin
                  CMD_Err(erOutOfRange);
                  Break;
                end;
                case Vd^.FType^.BaseType of
                  btU8: vdFree := Vd^.tu8 <> 0;
                  btS8: vdFree := Vd^.tS8 <> 0;
                  btU16: vdFree := Vd^.tu16 <> 0;
                   btS16: vdFree := Vd^.ts16 <> 0;
                  btU32: vdFree := Vd^.tu32 <> 0;
                  btS32: vdFree := Vd^.ts32 <> 0;
                else begin
                    CMD_Err(erInvalidType);
                    break;
                  end;
                end;
                if p <> 0 then
                  FJumpFlag := not vdFree
                else
                 FJumpFlag := vdFree;
              end;
            cm_fg:
              begin
                if not ReadLong(p) then begin
                  CMD_Err(erOutOfRange);
                  break;
                end;
                if FJumpFlag then
                  FCurrentPosition := FCurrentPosition + p;
              end;
            cm_puexh:
              begin
                New(pp);
                pp^.BasePtr :=FCurrStackBase;
                pp^.StackSize := FStack.Count;
                if not ReadLong(pp^.FinallyOffset) then begin
                  CMD_Err(erOutOfRange);
                  Dispose(pp);
                  Break;
                end;
                if not ReadLong(pp^.ExceptOffset) then begin
                  CMD_Err(erOutOfRange);
                  Dispose(pp);
                  Break;
                end;
                if not ReadLong(pp^.Finally2Offset) then begin
                  CMD_Err(erOutOfRange);
                  Dispose(pp);
                  Break;
                end;
                if not ReadLong(pp^.EndOfBlock) then begin
                  CMD_Err(erOutOfRange);
                  Dispose(pp);
                  Break;
                end;
                if pp^.FinallyOffset <> Cardinal(-1) then
                  pp^.FinallyOffset := pp^.FinallyOffset + FCurrentPosition;
                if pp^.ExceptOffset <> Cardinal(-1) then
                  pp^.ExceptOffset := pp^.ExceptOffset + FCurrentPosition;
                if pp^.Finally2Offset <> Cardinal(-1) then
                  pp^.Finally2Offset := pp^.Finally2Offset + FCurrentPosition;
                if pp^.EndOfBlock <> Cardinal(-1) then
                  pp^.EndOfBlock := pp^.EndOfBlock + FCurrentPosition;
                if ((pp^.FinallyOffset <> cardinal(-1)) and (pp^.FinallyOffset >= FCurrProc^.Length)) or
                  ((pp^.ExceptOffset <> cardinal(-1)) and (pp^.ExceptOffset >= FCurrProc^.Length)) or
                  ((pp^.Finally2Offset <> cardinal(-1)) and (pp^.Finally2Offset >= FCurrProc^.Length)) or
                  ((pp^.EndOfBlock <> cardinal(-1)) and (pp^.EndOfBlock >= FCurrProc^.Length)) then
                  begin
                    CMD_Err(ErOutOfRange);
                    Dispose(pp);
                    Break;
                  end;
                  FExceptionStack.Add(pp);
              end;
            cmd_poexh:
              begin
                if not ReadByte(p) then
                begin
                  CMD_Err(ErOutOfRange);
                  Break;
                end;
                case p of
                  2:
                    begin
                      ExceptionProc(Cardinal(-1), Cardinal(-1), erNoError, '');
                      pp := FExceptionStack.GetItem(FExceptionStack.Count -1);
                      if pp = nil then begin
                        cmd_err(ErOutOfRange);
                        Break;
                      end;
                      if pp^.Finally2Offset <> cardinal(-1) then
                      begin
                        FCurrentPosition := pp^.Finally2Offset;
                        pp^.Finally2Offset := cardinal(-1);
                      end else begin
                        p := pp^.EndOfBlock;
                        Dispose(pp);
                        FExceptionStack.Delete(FExceptionStack.Count -1);
                        if FExitPoint <> Cardinal(-1) then
                        begin
                          FCurrentPosition := FExitPoint;
                        end else begin
                          FCurrentPosition := p;
                        end;
                      end;
                    end;
                  0:
                    begin
                      pp := FExceptionStack.GetItem(FExceptionStack.Count -1);
                      if pp = nil then begin
                        cmd_err(ErOutOfRange);
                        Break;
                      end;
                      if pp^.FinallyOffset <> cardinal(-1) then
                      begin
                        FCurrentPosition := pp^.FinallyOffset;
                        pp^.FinallyOffset := cardinal(-1);
                      end else if pp^.Finally2Offset <> cardinal(-1) then
                      begin
                         FCurrentPosition := pp^.Finally2Offset;
                         pp^.ExceptOffset := cardinal(-1);
                      end else begin
                        p := pp^.EndOfBlock;
                        Dispose(pp);
                        FExceptionStack.Delete(FExceptionStack.Count -1);
                        if ExEx <> eNoError then
                        begin
                          ExceptionProc(ExProc, ExPos, ExEx, ExParam);
                        end else
                        if FExitPoint <> Cardinal(-1) then
                        begin
                          FCurrentPosition := FExitPoint;
                        end else begin
                          FCurrentPosition := p;
                        end;
                      end;
                    end;
                  1:
                    begin
                      pp := FExceptionStack.GetItem(FExceptionStack.Count -1);
                      if pp = nil then begin
                        cmd_err(ErOutOfRange);
                        Break;
                      end;
                      if (ExEx <> ENoError) and (pp^.ExceptOffset <> cardinal(-1)) then
                      begin
                        FCurrentPosition := pp^.ExceptOffset;
                        pp^.ExceptOffset := cardinal(-1);
                      end else if (pp^.Finally2Offset <> cardinal(-1)) then
                      begin
                        FCurrentPosition := pp^.Finally2Offset;
                        pp^.Finally2Offset := cardinal(-1);
                      end else begin
                        p := pp^.EndOfBlock;
                        Dispose(pp);
                        FExceptionStack.Delete(FExceptionStack.Count -1);
                        if ExEx <> eNoError then
                        begin
                          ExceptionProc(ExProc, ExPos, ExEx, ExParam);
                        end else
                        if FExitPoint <> Cardinal(-1) then
                        begin
                          FCurrentPosition := FExitPoint;
                        end else begin
                          FCurrentPosition := p;
                        end;
                      end;
                    end;
                  3:
                    begin
                      pp := FExceptionStack.GetItem(FExceptionStack.Count -1);
                      if pp = nil then begin
                        cmd_err(ErOutOfRange);
                        Break;
                      end;
                      p := pp^.EndOfBlock;
                      Dispose(pp);
                      FExceptionStack.Delete(FExceptionStack.Count -1);
                      if ExEx <> eNoError then
                      begin
                          ExceptionProc(ExProc, ExPos, ExEx, ExParam);
                      end else
                      if FExitPoint <> Cardinal(-1) then
                      begin
                        FCurrentPosition := FExitPoint;
                      end else begin
                        FCurrentPosition := p;
                      end;
                   end;
                end;
              end;
          else
            CMD_Err(erInvalidOpcode); // Error
          end;
        RunLine;
      end;
    end;
  until (FExceptionStack.Count = 0) or (Fstatus <> IsRunning);
  if FStatus = isLoaded then begin
    for I := 0 to Longint(FStack.Count) - 1 do begin
      DisposeVariant({$IFNDEF NOSMARTMM}MM, {$ENDIF}FStack.GetItem(I));
    end;
    FStack.Clear;
    if FCallCleanup then Cleanup;
  end;
end;

procedure TSCExec.Stop;
var
  I: Longint;
begin
  if FStatus = isRunning then
    FStatus := isLoaded
  else if FStatus = isPaused then begin
    FStatus := isLoaded;
    for I := 0 to Longint(FStack.Count) - 1 do begin
      DisposeVariant({$IFNDEF NOSMARTMM}MM, {$ENDIF}FStack.GetItem(I));
    end;
    FStack.Clear;
  end;
end;

function TSCExec.ReadByte(var b: Cardinal): Boolean;
begin
  if FCurrentPosition < FCurrProc.Length then begin
    b := FCurrProc.Data^[FCurrentPosition];
    Inc(FCurrentPosition);
    Result := True;
  end
  else
    Result := False;
end;

function TSCExec.ReadLong(var b: Cardinal): Boolean;
begin
  if FCurrentPosition + 3 < FCurrProc.Length then begin
    b := Cardinal((@FCurrProc.Data^[FCurrentPosition])^);
    Inc(FCurrentPosition, 4);
    Result := True;
  end
  else
    Result := False;
end;

function TSCExec.RunProc(Params: TSCList; ProcNo: Cardinal): Boolean;
var
  I, I2: Integer;
  Vd: PIfVariant;
  Cp: PIFProcRec;
  oldStatus: TIFStatus;
begin
  if FStatus <> isNotLoaded then begin
    if ProcNo >= FProcs.Count then begin
      Result := False;
      exit;
    end;
    if PIFProcRec(FProcs.GetItem(ProcNo))^.ExternalProc then begin
      Result := False;
      exit;
    end;
    for I := 0 to Params.Count - 1 do begin
      vd := Params.GetItem(I);
      if vd = nil then
      begin
        Result := False;
        exit;
      end;
      FStack.Add(Params.GetItem(I));
    end;
    Vd := CreateVariant({$IFNDEF NOSMARTMM}MM, {$ENDIF}@ReturnAddressType);
    Cp := FCurrProc;
    Vd^.treturnaddress.ProcNo := nil;
    Vd^.treturnaddress.Position := FCurrentPosition;
    Vd^.treturnaddress.StackBase := FCurrStackBase;
    I := FStack.Count;
    FStack.Add(Vd);
    FCurrStackBase := FStack.Count - 1;
    FCurrProc := FProcs.GetItem(ProcNo);
    FCurrentPosition := 0;
    oldStatus := FStatus;
    FStatus := isPaused;
    Result := RunScript;
    if FStack.Count > Cardinal(I) then
    begin
      vd := FStack.GetItem(I);
      if (vd <> nil) and (vd^.FType = @ReturnAddressType) then begin
        for i2 := FStack.Count - 1 downto I + 1 do begin
          DisposeVariant({$IFNDEF NOSMARTMM}MM, {$ENDIF}FStack.GetItem(i2));
          FStack.Delete(i2);
        end;
        FStack.Delete(I);
        FCurrentPosition := Vd^.treturnaddress.Position;
        FCurrStackBase := Vd^.treturnaddress.StackBase;
        DisposeVariant({$IFNDEF NOSMARTMM}MM, {$ENDIF}Vd);
      end;
    end;
    for I := Params.Count - 1 downto 0 do begin
      FStack.Delete(FStack.Count - 1);
    end;
    FStatus := oldStatus;
    FCurrProc := Cp;
  end else begin
    Result := False;
  end;
end;

function TSCExec.CreateIntegerVariant(FType: PIFTypeRec; Value: Longint): PIfVariant;
begin
  Result := CreateVariant({$IFNDEF NOSMARTMM}MM, {$ENDIF}FType);
  if Result <> nil then begin
    case FType^.BaseType of
      btU8: Result^.tu8 := Value;
      btS8: Result^.tS8 := Value;
      btU16: Result^.tu16 := Value;
      btS16: Result^.ts16 := Value;
      btU32: Result^.tu32 := Value;
      btS32: Result^.ts32 := Value;
    end;
  end;
end;

function TSCExec.CreateStringVariant(FType: PIFTypeRec; const Value: string): PIfVariant;
begin
  Result := CreateVariant({$IFNDEF NOSMARTMM}MM, {$ENDIF}FType);
  if Result <> nil then begin
    case FType^.BaseType of
      btPChar, btString: begin
          TbtString(Result^.tstring) := Value;
        end;
    end;
  end;
end;

function TSCExec.CreateFloatVariant(FType: PIFTypeRec; Value: Extended): PIfVariant;
begin
  Result := CreateVariant({$IFNDEF NOSMARTMM}MM, {$ENDIF}FType);
  if Result <> nil then begin
    case FType^.BaseType of
      btSingle: Result^.tsingle := Value;
      btDouble: Result^.tdouble := Value;
      btExtended: Result^.textended := Value;
    end;
  end;
end;

function TSCExec.FindType2(BaseType: TSCBaseType): PIFTypeRec;
var
  l: Cardinal;
begin
  FindType2 := FindType(0, BaseType, l);

end;

function TSCExec.FindType(StartAt: Cardinal; BaseType: TSCBaseType; var l: Cardinal): PIFTypeRec;
var
  I: Integer;
  n: PIFTypeRec;
begin
  for I := StartAt to FTypes.Count - 1 do begin
    n := FTypes.GetItem(I);
    if n^.BaseType = BaseType then begin
      l := I;
      Result := n;
      exit;
    end;
  end;
  Result := nil;
end;

function TSCExec.GetTypeNo(l: Cardinal): PIFTypeRec;
begin
  Result := FTypes.GetItem(l);
end;

function TSCExec.GetProc(const Name: string): Cardinal;
var
  MM,
    I: Longint;
  n: PIFProcRec;
begin
  MM := MakeHash(Name);
  for I := 0 to FProcs.Count - 1 do begin
    n := FProcs.GetItem(I);
    if (not n^.ExternalProc) and (Length(n^.ExportName) <> 0) and (n^.ExportNameHash = MM) and (n^.ExportName = Name) then begin
      Result := I;
      exit;
    end;
  end;
  Result := Cardinal(-1);
end;

function TSCExec.GetType(const Name: string): Cardinal;
var
  MM,
    I: Longint;
  n: PIFTypeRec;
begin
  MM := MakeHash(Name);
  for I := 0 to FTypes.Count - 1 do begin
    n := FTypes.GetItem(I);
    if (Length(n^.ExportName) <> 0) and (n^.ExportNameHash = MM) and (n^.ExportName = Name) then begin
      Result := I;
      exit;
    end;
  end;
  Result := Cardinal(-1);
end;


procedure TSCExec.AddResource(Proc, P: Pointer);
var
  Temp: PSCResource;
begin
  New(Temp);
  Temp^.Proc := Proc;
  Temp^.P := p;
  FResources.Add(temp);
end;

procedure TSCExec.DeleteResource(P: Pointer);
var
  i: Longint;
begin
  for i := Longint(FResources.Count) -1 downto 0 do
  begin
    if PSCResource(FResources.GetItem(I))^.P = P then
    begin
      FResources.Delete(I);
      exit;
    end;
  end;
end;

function TSCExec.FindProcResource(Proc: Pointer): Pointer;
var
  I: Longint;
  temp: PSCResource;
begin
  for i := Longint(FResources.Count) -1 downto 0 do
  begin
    temp := FResources.GetItem(I);
    if temp^.Proc = proc then
    begin
      Result := Temp^.P;
      exit;
    end;
  end;
  Result := nil;
end;

function TSCExec.IsValidResource(Proc, P: Pointer): Boolean;
var
  i: Longint;
  temp: PSCResource;
begin
  for i := 0 to Longint(FResources.Count) -1 do
  begin
    temp := FResources.GetItem(i);
    if temp^.p = p then begin
      result := temp^.Proc = Proc;
      exit;
    end;
  end;
  result := false;
end;

function TSCExec.FindProcResource2(Proc: Pointer;
  var StartAt: Longint): Pointer;
var
  I: Longint;
  temp: PSCResource;
begin
  if StartAt > longint(FResources.Count) -1 then 
    StartAt := longint(FResources.Count) -1;
  for i := StartAt downto 0 do
  begin
    temp := FResources.GetItem(I);
    if temp^.Proc = proc then
    begin
      Result := Temp^.P;
      StartAt := i -1;
      exit;
    end;
  end;
  StartAt := -1;
  Result := nil;
end;

procedure TSCExec.RunLine;
begin
  if @FOnRunLine <> nil then
    FOnRunLine(Self);
end;

procedure TSCExec.CMD_Err2(EC: TIFError; const Param: string);
var
  l: Longint;
  C: Cardinal;
begin
  C := Cardinal(-1);
  for l := 0 to FProcs.Count - 1 do begin
    if FProcs.GetItem(l) = FCurrProc then begin
      C := l;
      break;
    end;
  end;
  ExceptionProc(C, FCurrentPosition, EC, Param);
end;

procedure FreePIFVariantList({$IFNDEF NOSMARTMM}MM: Pointer; {$ENDIF}List: TSCList);
var
  I: Longint;
begin
  for I := List.Count -1 downto 0 do
  begin
    DisposeVariant({$IFNDEF NOSMARTMM}MM, {$ENDIF}List.GetItem(I));
  end;
  List.Free;
end;
procedure TSCExec.AddSpecialProcImport(const FName: string;
  P: TSCOnSpecialProcImport; Tag: Pointer);
var
  N: PSpecialProc;
begin
  New(n);
  n^.P := P;
  N^.Name := FName;
  n^.namehash := MakeHash(FName);
  n^.Tag := Tag;
  FSpecialProcList.Add(n);
end;

function TSCExec.GetVar(const Name: string): Cardinal;
var
  l: Longint;
  h: longint;
begin
  h := makehash(Name);
  for l := FExportedVars.Count - 1 downto 0 do
  begin
    if (PSCExportedVar(FexportedVars.GetItem(L))^.FNameHash = h) and(PSCExportedVar(FexportedVars.GetItem(L))^.FName=Name) then
    begin
      Result := L;
      exit;
    end;
  end;
  Result := Cardinal(-1);
end;

function TSCExec.GetVarNo(C: Cardinal): PIFVariant;
begin
  Result := FGlobalVars.GetItem(c);
end;

function TSCExec.GetVar2(const Name: string): PIFVariant;
begin
  Result := GetVarNo(GetVar(Name));
end;

function TSCExec.GetProcNo(C: Cardinal): PIFProcRec;
begin
  Result := FProcs.GetItem(c);
end;

end.

