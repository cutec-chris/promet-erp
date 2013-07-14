{ifpscomp is the compiler part of the script engine}
UNIT SC_Comp;

{$ifdef fpc}
{$mode delphi}{$H+}
{$endif}
{

Innerfuse Pascal Script III
Copyright (C) 2000-2002 by Carlo Kok (ck@carlo-kok.com)

Features:

  - Constants
  - Variables
  - Procedures/Functions
  - Procedural Variables
  - If, While, Repeat, For, Case
  - Break/Continue
  - External/Integer Procedures/Functions
  - Arrays, Records
  - Ability to create compiled code that can be used later
  - Debugging Support
  - Importing Delphi Funtions and classes

}
INTERFACE
USES
  SysUtils, SC_Parsers, SC_Utils;
CONST
  {Internal constant: used when a value must be read from an address}
  CVAL_Addr = 0;
  {Internal constant: used when a value is plain data}
  CVAL_Data = 1;
  {Internal constant: used when a value must be read from an address and pushed}
  CVAL_PushAddr = 2;
  {Internal constant: used for function calls}
  CVAL_Proc = 3;
  {Internal constant: used when there are sub calculations}
  CVAL_Eval = 4;
  {Internal constant: same as address except that it has to be freed otherwise}
  CVAL_AllocatedStackReg = 5;
  {Internal constant: A method call}
  CVAL_ClassProcCall = 7;
  {Internal contant: A method call}
  CVAL_ClassMethodCall = 8;
  {Internal constant: Property set method}
  CVAL_ClassPropertyCallSet = 9;
  {Internal constant: Property get method}
  CVAL_ClassPropertyCallGet = 10;
  {Internal Constant: Procedural Call with variable procedure number}
  CVAL_VarProc = 11;
  {Internal Constant: Procedural Pointer}
  CVAL_VarProcPtr = 12;
  {Internal Constant: Array}
  CVAL_Array = 13;
  {Internal Constant: ArrayAllocatedStackRec same as @link(CVAL_AllocatedStackReg)}
  CVAL_ArrayAllocatedStackRec = 14;
  {Internal Constant: Nil}
  CVAL_Nil = 15;
  {Internal Constant; Casting}
  CVAL_Cast = 16;

TYPE
  TSCPascalCompiler = CLASS;
  {Internal type used to store the current block type}
  TSubOptType = (tMainBegin, tProcBegin, tSubBegin, tOneLiner, tifOneliner, tRepeat, tTry, tTryEnd);

  {TSCExternalClass is used when external classes need to be called}
  TSCExternalClass = CLASS;

  PSCRegProc = ^TSCRegProc;
  {TSCRegProc is used to store the registered procs}
  TSCRegProc = RECORD
    NameHash: Longint;
    Name, Decl: STRING;
    FExportName: Boolean;
    ImportDecl: STRING; // used for dlls
  END;
  {The compile time variant}
  PIfRVariant = ^TIfRVariant;
  {The compile time variant}
  TIfRVariant = RECORD
    FType: Cardinal;
    Value: STRING;
  END;
  {PSCRecordType is is used to store records}
  PSCRecordType = ^TSCRecordType;
  {TSCRecordType is is used to store records}
  TSCRecordType = RECORD
    FieldNameHash: Longint;
    FieldName: STRING;
    RealFieldOffset: Cardinal;
    FType: Cardinal;
  END;
  {PSCProceduralType is a pointer to @link(TSCProceduralType)}
  PSCProceduralType = ^TSCProceduralType;
  {TSCProceduralType contains information to store procedural variables}
  TSCProceduralType = RECORD
    ProcDef: STRING;
  END;
  {PSCType is a pointer to a @link(TSCType) record}
  PSCType = ^TSCType;
  {TSCType contains a type definition}
  TSCType = RECORD
    NameHash: Longint;
    Name: STRING;
    BaseType: TSCBaseType;
    DeclarePosition: Cardinal;
    Used: Boolean;
    TypeSize: Cardinal;
    RecordSubVals: TSCList;
    FExport: Boolean;
    CASE byte OF
      0: (Ext: Pointer);
      1: (Ex: TSCExternalClass);
  END;
  {@link(TSCProcVar)
  PSCProcVar is a pointer to a TSCProcVar record}
  PSCProcVar = ^TSCProcVar;
  {TSCProcVar is used to store procedural variables}
  TSCProcVar = RECORD
    NameHash: Longint;
    VarName: STRING;
    VarType: Cardinal; // only for calculation types
    Used, CurrentlyUsed: Boolean;
    DeclarePosition: Cardinal;
  END;
  {PSCUsedRegProc is a pointer to an TSCUsedRegProc}
  PSCUsedRegProc = ^TSCUsedRegProc;
  {TSCUsedRegProc is used to store used registered procs}
  TSCUsedRegProc = RECORD
    Internal: Boolean; { false }
    RP: PSCRegProc;
  END;
  {PSCProcedure is a pointer to a TSCProcedure}
  PSCProcedure = ^TSCProcedure;
  {TSCProcdure is used to store information about a procedure}
  TSCProcedure = RECORD
    Internal: Boolean; { true }
    Forwarded: Boolean;
    Data: STRING;
    NameHash: Longint;
    Decl, Name: STRING;
    {Decl: [RESULTTYPE] [PARAM1NAME] [PARAM1TYPE] [PARAM2NAME] ... }
    { @ = Normal Parameter  ! = Var parameter `}
    ProcVars: TSCList;
    Used: Boolean;
    DeclarePosition: Cardinal;
    OutputDeclPosition: Cardinal;
    ResUsed: Boolean;
    FExport: Byte; {1 = yes; 2 = also decl}
    FLabels: TSCStringList; // mi2s(position)+mi2s(namehash)+name   [position=$FFFFFFFF means position unknown]
    FGotos: TSCStringList; // mi2s(position)+mi2s(destinationnamehash)+destinationname
  END;
  {PSCVar is a pointer to a TSCVar record}
  PSCVar = ^TSCVar;
  {TSCVar is used to store global variables}
  TSCVar = RECORD
    NameHash: Longint;
    Name: STRING;
    FType: Cardinal;
    Used: Boolean;
    DeclarePosition: Cardinal;
    exportname: STRING;
  END;
  {PSCContant is a pointer to a TSCConstant}
  PSCConstant = ^TSCConstant;
  {TSCContant contains a constant}
  TSCConstant = RECORD
    NameHash: Longint;
    Name: STRING;
    Value: TIfRVariant;
  END;
  {Is used to store the type of a compiler error}
  TSCCompilerError = (
    ecUnknownIdentifier,
    ecIdentifierExpected,
    ecCommentError,
    ecStringError,
    ecCharError,
    ecSyntaxError,
    ecUnexpectedEndOfFile,
    ecSemicolonExpected,
    ecBeginExpected,
    ecPeriodExpected,
    ecDuplicateIdentifier,
    ecColonExpected,
    ecUnknownType,
    ecCloseRoundExpected,
    ecTypeMismatch,
    ecInternalError,
    ecAssignmentExpected,
    ecThenExpected,
    ecDoExpected,
    ecNoResult,
    ecOpenRoundExpected,
    ecCommaExpected,
    ecToExpected,
    ecIsExpected,
    ecOfExpected,
    ecCloseBlockExpected,
    ecVariableExpected,
    ecStringExpected,
    ecEndExpected,
    ecUnSetLabel,
    ecNotInLoop,
    ecInvalidJump,
    ecOpenBlockExpected,
    ecWriteOnlyProperty,
    ecReadOnlyProperty,
    ecClassTypeExpected,
    ecCustomError,
    ecDivideByZero,
    ecMathError,
    ecUnsatisfiedForward

    );
  {Used to store the type of a hint}
  TSCCompilerHint = (
    ehVariableNotUsed, {param = variable name}
    ehFunctionNotUsed, {param = function name}
    ehCustomHint
    );
  {Is used to store the type of a warning}
  TSCCompilerWarning = (
    ewCalculationAlwaysEvaluatesTo,
    ewIsNotNeeded,
    ewCustomWarning
    );
  {Is used to store the type of the messages}
  TSCCompilerMessageType = (ptWarning, ptError, ptHint);
  {Contains a pointer to an TSCPascalCompilerMessages record}
  PSCCompilerMessage = ^TSCCompilerMessage;
  {Contains compiler messages}
  TSCCompilerMessage = PACKED RECORD

    ModuleName: STRING;
    Param: STRING;
    Position: Cardinal;
    MessageType: TSCCompilerMessageType;
    CASE TSCCompilerMessageType OF
      ptError: (Error: TSCCompilerError);
      ptWarning: (Warning: TSCCompilerWarning);
      ptHint: (Hint: TSCCompilerHint);
  END;

  {See TSCPascalCompiler.OnUseVariable}
  TSCOnUseVariable = PROCEDURE(Sender: TSCPascalCompiler; VarType: TSCVariableType; VarNo: Longint; ProcNo, Position: Cardinal);
  {See TSCPascalCompiler.OnUses}
  TSCOnUses = FUNCTION(Sender: TSCPascalCompiler; CONST Name: STRING): Boolean;
  {See TSCPascalCompiler.OnExportCheck}
  TSCOnExportCheck = FUNCTION(Sender: TSCPascalCompiler; Proc: PSCProcedure; CONST ProcDecl: STRING): Boolean;
  {See TSCPascalCompiler.OnWriteLine}
  TSCOnWriteLineEvent = FUNCTION(Sender: TSCPascalCompiler; Position: Cardinal): Boolean;
  {See TSCPascalCompiler.OnExternalProc}
  TSCOnExternalProc = FUNCTION(Sender: TSCPascalCompiler; CONST Name, Decl, FExternal: STRING): PSCRegProc;
  
  TSCAbstractCompiler = CLASS
  private
    FParser: TSCAbstractParser;
    FMessages: TSCList;
    FOnUses: TSCOnUses;
    FOnExportCheck: TSCOnExportCheck;
    FOnExternalProc: TSCOnExternalProc;
    FOnUseVariable: TSCOnUseVariable;
    FOnWriteLine: TSCOnWriteLineEvent;
    FIsUnit: Boolean;
    FAllowNoBegin: Boolean;
    FAllowNoEnd: Boolean;
    FAllowUnit: Boolean;
    FAvailableTypes: TSCList;
    FUsedTypes: TSCList;
    FProcs: TSCList;
    FDebugOutput: STRING;
    FOutput: STRING;
    FAutoFreeList: TSCList;
    FUNCTION GetMsgCount: Longint;
    FUNCTION GetMsg(l: Longint): PSCCompilerMessage;
    FUNCTION AT2UT(L: Cardinal): Cardinal;
    FUNCTION GetTypeCopyLink(p: PSCType): PSCType;
    PROCEDURE UpdateRecordFields(r: Pointer);
  public
    {Add an error the messages}
    FUNCTION MakeError(CONST Module: STRING; E: TSCCompilerError; CONST
      Param: STRING): PSCCompilerMessage;
    {Add a warning to the messages}
    FUNCTION MakeWarning(CONST Module: STRING; E: TSCCompilerWarning;
      CONST Param: STRING): PSCCompilerMessage;
    {Add a hint to the messages}
    FUNCTION MakeHint(CONST Module: STRING; E: TSCCompilerHint;
      CONST Param: STRING): PSCCompilerMessage;
    PROCEDURE ReplaceTypes(VAR s: STRING);
    {Compile a script (s)}
    FUNCTION Compile(CONST s: STRING): Boolean;virtual;abstract;
    {Return the output}
    FUNCTION GetOutput(VAR s: STRING): Boolean;virtual;abstract;
 {Return the debugger output}
    FUNCTION GetDebugOutput(VAR s: STRING): Boolean;virtual;abstract;
    {Clear the current data}
    PROCEDURE Clear;

    {Search for a type}
    FUNCTION FindType(CONST Name: STRING): Cardinal;
    {contains the number of messages}
    PROPERTY MsgCount: Longint READ GetMsgCount;
 {The messages/warnings/errors}
    PROPERTY Msg[l: Longint]: PSCCompilerMessage READ GetMsg;
    {OnUses i scalled for each Uses and always first with 'SYSTEM' parameters}
    PROPERTY OnUses: TSCOnUses READ FOnUses WRITE FOnUses;
 {OnExportCheck is called for each function to check if it needs to be exported and has the correct parameters}
    PROPERTY OnExportCheck: TSCOnExportCheck READ FOnExportCheck WRITE FOnExportCheck;
 {OnWriteLine is called after each line}
    PROPERTY OnWriteLine: TSCOnWriteLineEvent READ FOnWriteLine WRITE FOnWriteLine;
 {OnExternalProc is called when an external token is found after a procedure header}
    PROPERTY OnExternalProc: TSCOnExternalProc READ FOnExternalProc WRITE FOnExternalProc;
 {The OnUseVariant event is called when a variable is used by the script engine}
    PROPERTY OnUseVariable: TSCOnUseVariable READ FOnUseVariable WRITE FOnUseVariable;
 {contains true if the current file is a unit}
    PROPERTY IsUnit: Boolean READ FIsUnit;
 {Allow no main begin/end}
    PROPERTY AllowNoBegin: Boolean READ FAllowNoBegin WRITE FAllowNoBegin;
 {Allow a unit instead of program}
    PROPERTY AllowUnit: Boolean READ FAllowUnit WRITE FAllowUnit;
 {Allow it to have no END on the script (only works when AllowNoBegin is true)}
    PROPERTY AllowNoEnd: Boolean READ FAllowNoEnd WRITE FAllowNoEnd;
  end;
  
  TSCPascalCompiler = CLASS(TSCAbstractCompiler)
  PROTECTED
    FID: Pointer;
    FBooleanType: Cardinal;
    FRegProcs: TSCList;
    FConstants: TSCList;
    FVars: TSCList;
    FContinueOffsets, FBreakOffsets: TSCList;
    FUNCTION GetType(BaseType: TSCBaseType): Cardinal;
    FUNCTION MakeDecl(decl: STRING): STRING;
    FUNCTION MakeExportDecl(decl: STRING): STRING;
    PROCEDURE DefineStandardTypes;
    FUNCTION IsIntBoolType(FTypeNo: Cardinal): Boolean;
    FUNCTION GetUInt(FUseTypes: TSCList; Src: PIfRVariant; VAR s: Boolean): Cardinal;
    FUNCTION GetInt(FUseTypes: TSCList; Src: PIfRVariant; VAR s: Boolean): Longint;
    FUNCTION GetReal(FUseTypes: TSCList; Src: PIfRVariant; VAR s: Boolean): Extended;
    FUNCTION GetString(FUseTypes: TSCList; Src: PIfRVariant; VAR s: Boolean): STRING;
    FUNCTION PreCalc(FUseTypes: TSCList; Var1Mod: Byte; var1: PIFRVariant; Var2Mod: Byte;
      Var2: PIfRVariant; Cmd: Byte; Pos: Cardinal): Boolean;
    FUNCTION ReadConstant(StopOn: TSCToken): PIfRVariant;
    PROCEDURE WriteDebugData(CONST s: STRING);
    FUNCTION ProcessFunction: Boolean;
    FUNCTION IsDuplicate(CONST s: STRING): Boolean;
    FUNCTION DoVarBlock(proc: PSCProcedure): Boolean;
    FUNCTION DoTypeBlock(FParser: TSCAbstractParser): Boolean;
    FUNCTION ReadType(CONST Name: STRING; FParser: TSCAbstractParser): Cardinal;
    FUNCTION NewProc(CONST Name: STRING): PSCProcedure;
    FUNCTION ProcessLabel(Proc: PSCProcedure): Boolean;
    PROCEDURE Debug_SavePosition(ProcNo: Cardinal; Proc: PSCProcedure);
    PROCEDURE Debug_WriteParams(ProcNo: Cardinal; Proc: PSCProcedure);
    FUNCTION ProcessSub(FType: TSubOptType; ProcNo: Cardinal;
      proc: PSCProcedure): Boolean;
    FUNCTION ProcessLabelForwards(Proc: PSCProcedure): Boolean;

    FUNCTION GetUsedType(No: Cardinal): PSCType;
    FUNCTION GetAvailableType(No: Cardinal): PSCType;
    FUNCTION GetUsedTypeCount: Cardinal;
    FUNCTION GetAvailableTypeCount: Cardinal;
    FUNCTION UseAvailableType(No: Cardinal): Cardinal;
    FUNCTION GetProc(No: Cardinal): PSCProcedure;
    FUNCTION GetProcCount: Cardinal;
    FUNCTION GetVariableCount: Cardinal;
    FUNCTION GetVariable(No: Cardinal): PSCVar;

    FUNCTION AddUsedFunction(VAR Proc: PSCProcedure): Cardinal;
    FUNCTION AddUsedFunction2(VAR Proc: PSCUsedRegProc): Cardinal;
    FUNCTION CheckCompatProc(FTypeNo, ProcNo: Cardinal): Boolean;
    PROCEDURE ParserError(Parser: TObject; Kind: TSCParserErrorKind; Position: Cardinal);
  PUBLIC
    {Add an object to the auto-free list}
    PROCEDURE AddToFreeList(Obj: TObject);
    {Tag}
    PROPERTY ID: Pointer READ FID WRITE FID;
    {Add a function}
    FUNCTION AddFunction(CONST Header: STRING): PSCRegProc;
    {add a type}
    FUNCTION AddType(CONST Name: STRING; CONST BaseType: TSCBaseType): PSCType;
    {Add a type declared in a string}
    FUNCTION AddTypeS(CONST Name, Decl: STRING): PSCType;
    {Add a type copy type}
    FUNCTION AddTypeCopy(CONST Name: STRING; TypeNo: Cardinal): PSCType;
    {Add a type copy type}
    FUNCTION AddTypeCopyN(CONST Name, FType: STRING): PSCType;
    {Add a constant}
    FUNCTION AddConstant(CONST Name: STRING; FType: Cardinal): PSCConstant;
    {Add a constant}
    FUNCTION AddConstantN(CONST Name, FType: STRING): PSCConstant;
    {Add a variable}
    FUNCTION AddVariable(CONST Name: STRING; FType: Cardinal): PSCVar;
    {Add a variable}
    FUNCTION AddVariableN(CONST Name, FType: STRING): PSCVar;
    {Add an used variable}
    FUNCTION AddUsedVariable(CONST Name: STRING; FType: Cardinal): PSCVar;
    {add an used variable (with named type)}
    FUNCTION AddUsedVariableN(CONST Name, FType: STRING): PSCVar;
    {Add a variable and export it}
    FUNCTION AddExportVariableN(CONST Name, FType: STRING): PSCVar;
    {Add an used variable and export it}
    FUNCTION AddUsedExportVariableN(CONST Name, FType: STRING): PSCVar;
    {Compile a script (s)}
    FUNCTION Compile(CONST s: STRING): Boolean;override;
    {Return the output}
    FUNCTION GetOutput(VAR s: STRING): Boolean;override;
 {Return the debugger output}
    FUNCTION GetDebugOutput(VAR s: STRING): Boolean;override;
    {Create}
    CONSTRUCTOR Create;
 {Destroy the current instance of the script compiler}
    DESTRUCTOR Destroy; OVERRIDE;
  END;
  {Pointer to @link(TSCValue) type}
  PSCValue = ^TSCValue;
  {Type containing types}
  TSCValue = PACKED RECORD
    FType: Byte;
    Modifiers: byte;
    {
      1 = not
      2 = minus
      4 = ignore types (casting)
      8 = override type
      128 = don't free
    }
    FNewTypeNo: Cardinal;
    DPos: Cardinal;
    CASE Byte OF
      CVAL_Nil: ();
      CVAL_Addr: (Address: Cardinal; RecField: TSCList); {i/o}
      CVAL_Data: (FData: PIfRVariant); {i}
      CVAL_PushAddr: (Address_: Cardinal; RecField__: TSCList);
      CVAL_Proc: (Parameters: TSCList; ProcNo: Cardinal);
      CVAL_VarProc: (_Parameters: TSCList; _ProcNo: PSCValue);
      CVAL_Eval: (SubItems: TSCList; frestype: Cardinal);
      CVAL_ClassPropertyCallGet,
        CVAL_ClassPropertyCallSet,
        CVAL_ClassMethodCall,
        CVAL_ClassProcCall: (Self: PSCValue; ClassProcNo: Cardinal; Params: TSCList);
      CVAL_Array: (ArrayItems: TSCList);
      CVAL_VarProcPtr: (VProcNo: Cardinal);
      CVAL_Cast: (NewTypeNo: Cardinal; Input: PSCValue);
  END;
  {Internal type: PCalc_Item}
  PCalc_Item = ^TCalc_Item;
  {Internal type: TCalc_Item}
  TCalc_Item = PACKED RECORD
    C: Boolean;
    CASE Boolean OF
      False: (OutRec: PSCValue);
      True: (calcCmd: Byte);
  END;
  {Internal type: PIFRecField}
  PIFRecField = ^TIFRecField;
  {Internal type: TIFRecField}
  TIFRecField = PACKED RECORD
    FKind: Byte;
    FType: Cardinal;
    CASE Byte OF
      0: (RecFieldNo: Cardinal);
      1: (ArrayFieldNo: Cardinal);
      2: (ReadArrayFieldNoFrom: PSCValue);
      3: (ResultRec: PSCValue);
  END;
  {TSCExternalClass is used when external classes need to be called}
  TSCExternalClass = CLASS
  PROTECTED
    SE: TSCPascalCompiler;
  PUBLIC
    {The type used as a class}
    FUNCTION SelfType: Cardinal; VIRTUAL;
 {Create}
    CONSTRUCTOR Create(Se: TSCPascalCompiler);
    {Find a class function}
    FUNCTION ClassFunc_Find(CONST Name: STRING; VAR Index: Cardinal): Boolean; VIRTUAL;
 {Call a class function}
    FUNCTION ClassFunc_Call(Index: Cardinal; VAR ProcNo: Cardinal): Boolean; VIRTUAL;
    {Find a function}
    FUNCTION Func_Find(CONST Name: STRING; VAR Index: Cardinal): Boolean; VIRTUAL;
 {Call a function}
    FUNCTION Func_Call(Index: Cardinal; VAR ProcNo: Cardinal): Boolean; VIRTUAL;
    {Find a variant}
    FUNCTION Property_Find(CONST Name: STRING; VAR Index: Cardinal): Boolean; VIRTUAL;
 {Return the header of an variant}
    FUNCTION Property_GetHeader(Index: Cardinal; VAR s: STRING): Boolean; VIRTUAL;
 {Get a variant value}
    FUNCTION Property_Get(Index: Cardinal; VAR ProcNo: Cardinal): Boolean; VIRTUAL;
 {Set a variant value}
    FUNCTION Property_Set(Index: Cardinal; VAR ProcNo: Cardinal): Boolean; VIRTUAL;
    {Check if the class is compatible}
    FUNCTION IsCompatibleWith(Cl: TSCExternalClass): Boolean; VIRTUAL;
    {Returns the ProcNo for setting a class variable to nil}
    FUNCTION SetNil(TypeNo: Cardinal; VAR ProcNo: Cardinal): Boolean; VIRTUAL;
    {Return the procno for casting}
    FUNCTION CastToType(TypeNo, IntoType: Cardinal; VAR ProcNo: Cardinal): Boolean; VIRTUAL;
    {Return the procno for comparing two classes}
    FUNCTION CompareClass(OtherTypeNo: Cardinal; VAR ProcNo: Cardinal): Boolean; VIRTUAL;
  END;
{Convert a message to a string}
FUNCTION SCMessageToString(x: PSCCompilerMessage): STRING;
{Set the name of an exported variable}
PROCEDURE SetVarExportName(P: PSCVar; CONST ExpName: STRING);
{Transform a double to a string}
FUNCTION TransDoubleToStr(D: Double): STRING;
{Transform a single to a string}
FUNCTION TransSingleToStr(D: Single): STRING;
{Transform a extended to a string}
FUNCTION TransExtendedToStr(D: Extended): STRING;
{Transform a longint to a string}
FUNCTION TransLongintToStr(D: Longint): STRING;
{Transform a cardinal to a string}
FUNCTION TransCardinalToStr(D: Cardinal): STRING;
{Transform a word to a string}
FUNCTION TransWordToStr(D: Word): STRING;
{Transform a smallint to a string}
FUNCTION TransSmallIntToStr(D: SmallInt): STRING;
{Transform a byte to a string}
FUNCTION TransByteToStr(D: Byte): STRING;
{Transform a shortint to a string}
FUNCTION TransShortIntToStr(D: ShortInt): STRING;


IMPLEMENTATION

PROCEDURE SetVarExportName(P: PSCVar; CONST ExpName: STRING);
BEGIN
  IF p <> NIL THEN
    p^.exportname := ExpName;
END;

FUNCTION TSCPascalCompiler.GetType(BaseType: TSCBaseType): Cardinal;
VAR
  l: Longint;
  x: PSCType;
BEGIN
  FOR l := 0 TO FUsedTypes.Count - 1 DO
    BEGIN
      x := FUsedTypes.GetItem(l);
      IF (x^.BaseType = BaseType) AND (x^.Ext = NIL) THEN
        BEGIN
          Result := l;
          exit;
        END;
    END;
  FOR l := 0 TO FAvailableTypes.Count - 1 DO
    BEGIN
      x := FAvailableTypes.GetItem(l);
      IF (x^.BaseType = BaseType) AND (x^.Ext = NIL) THEN
        BEGIN
          FUsedTypes.Add(x);
          Result := FUsedTypes.Count - 1;
          exit;
        END;
    END;
  New(x);
  x^.Name := '';
  x^.NameHash := MakeHash(x^.Name);
  x^.BaseType := BaseType;
  x^.TypeSize := 1;
  x^.DeclarePosition := Cardinal(-1);
  x^.Ext := NIL;
  x^.Used := True;
  FAvailableTypes.Add(x);
  FUsedTypes.Add(x);
  Result := FUsedTypes.Count - 1;
END;

FUNCTION TSCPascalCompiler.MakeDecl(decl: STRING): STRING;
VAR
  s: STRING;
  c: char;
BEGIN
  s := grfw(decl);
  IF s = '-1' THEN result := '0' ELSE
    result := PSCType(FUsedTypes.GetItem(StrToInt(s)))^.Name;

  WHILE length(decl) > 0 DO
    BEGIN
      s := grfw(decl);
      c := s[1];
      s := PSCType(FUsedTypes.GetItem(StrToInt(grfw(decl))))^.Name;
      result := result + ' ' + c + s;
    END;
END;


{ TSCPascalCompiler }

CONST
  BtTypeCopy = 255;
  btChar = 254;

FUNCTION SCMessageToString(x: PSCCompilerMessage): STRING;
BEGIN
  CASE x^.MessageType OF
    ptError:
      BEGIN
        CASE x^.Error OF
          ecUnknownIdentifier: Result := 'Unknown identifier ''' + x^.Param +
            '''';
          ecIdentifierExpected: Result := 'Identifier expected';
          ecCommentError: Result := 'Comment error';
          ecStringError: Result := 'String error';
          ecCharError: Result := 'Char error';
          ecSyntaxError: Result := 'Syntax error';
          ecUnexpectedEndOfFile: Result := 'Unexpected end of file';
          ecSemicolonExpected: Result := 'Semicolon ('';'') expected';
          ecBeginExpected: Result := '''BEGIN'' expected';
          ecPeriodExpected: Result := 'period (''.'') expected';
          ecDuplicateIdentifier: Result := 'Duplicate identifier ''' + x^.Param + '''';
          ecColonExpected: Result := 'colon ('':'') expected';
          ecUnknownType: Result := 'Unknown type ''' + x^.Param + '''';
          ecCloseRoundExpected: Result := 'Close round expected';
          ecTypeMismatch: Result := 'Type mismatch';
          ecInternalError: Result := 'Internal error (' + x^.Param + ')';
          ecAssignmentExpected: Result := 'Assignment expected';
          ecThenExpected: Result := '''THEN'' expected';
          ecDoExpected: Result := '''DO'' expected';
          ecNoResult: Result := 'No result';
          ecOpenRoundExpected: Result := 'open round (''('')expected';
          ecCommaExpected: Result := 'comma ('','') expected';
          ecToExpected: Result := '''TO'' expected';
          ecIsExpected: Result := 'is (''='') expected';
          ecOfExpected: Result := '''OF'' expected';
          ecCloseBlockExpected: Result := 'Close block('']'') expected';
          ecVariableExpected: Result := 'Variable Expected';
          ecStringExpected: result := 'String Expected';
          ecEndExpected: Result := '''END'' expected';
          ecUnSetLabel: Result := 'Label ''' + x^.Param + ''' not set';
          ecNotInLoop: Result := 'Not in a loop';
          ecInvalidJump: Result := 'Invalid jump';
          ecOpenBlockExpected: Result := 'Open Block (''['') expected';
          ecWriteOnlyProperty: Result := 'Write-only property';
          ecReadOnlyProperty: Result := 'Read-only property';
          ecClassTypeExpected: Result := 'Class type expected';
          ecCustomError: Result := x^.Param;
          ecDivideByZero: Result := 'Divide by Zero';
          ecMathError: Result := 'Math Error';
          ecUnsatisfiedForward: Result := 'Unsatisfied Forward ' + X^.Param;
        ELSE
          Result := 'Unknown error';
        END;
        Result := '[Error] ' + x^.ModuleName + ': ' + Result;
      END;
    ptHint:
      BEGIN
        CASE x^.Hint OF
          ehVariableNotUsed: Result := 'Variable ''' + x^.Param + ''' never used';
          ehFunctionNotUsed: Result := 'Function ''' + x^.Param + ''' never used';
          ehCustomHint: Result := x^.Param;
        ELSE
          Result := 'Unknown hint';
        END;
        Result := '[Hint] ' + x^.ModuleName + ': ' + Result;
      END;
    ptWarning:
      BEGIN
        CASE x^.Warning OF
          ewCustomWarning: Result := x^.Param;
          ewCalculationAlwaysEvaluatesTo: Result := 'Calculation always evaluates to ' + x^.Param;
          ewIsNotNeeded: Result := x^.Param + ' is not needed';
        END;
        Result := '[Warning] ' + x^.ModuleName + ': ' + Result;
      END;
  ELSE
    Result := 'Unknown message';
  END;
END;

TYPE
  TFuncType = (ftProc, ftFunc);

FUNCTION mi2s(i: Cardinal): STRING;
BEGIN
  Result := #0#0#0#0;
  Cardinal((@Result[1])^) := i;
END;




FUNCTION TSCPascalCompiler.AddType(CONST Name: STRING; CONST BaseType: TSCBaseType): PSCType;
BEGIN
  IF FProcs = NIL THEN BEGIN Result := NIL; exit;
    END;
  New(Result);
  Result^.Name := FastUppercase(Name);
  Result^.NameHash := MakeHash(Result^.Name);
  Result^.BaseType := BaseType;
  Result^.Used := False;
  Result^.TypeSize := 1;
  Result^.DeclarePosition := Cardinal(-1);
  Result^.RecordSubVals := NIL;
  Result^.FExport := False;
  Result^.Ext := NIL;
  FAvailableTypes.Add(Result);

END;


FUNCTION TSCPascalCompiler.AddFunction(CONST Header: STRING): PSCRegProc;

  FUNCTION FindType(CONST s: STRING): Cardinal;
  VAR
    h, l: Longint;
  BEGIN
    h := MakeHash(s);
    FOR l := 0 TO FAvailableTypes.Count - 1 DO
      BEGIN
        IF (PSCType(FAvailableTypes.GetItem(l))^.NameHash = h) AND
          (PSCType(FAvailableTypes.GetItem(l))^.Name = s) THEN
          BEGIN
            Result := l;
            exit;
          END;
      END;
    Result := Cardinal(-1);
  END;
VAR
  Parser: TSCPascalParser;
  IsFunction: Boolean;
  VNames, Name, Decl: STRING;
  modifier: Char;
  VCType: Cardinal;
  x: PSCRegProc;
BEGIN
  IF FProcs = NIL THEN BEGIN Result := NIL; exit;
    END;
  Parser := TSCPascalParser.Create;
  Parser.SetText(Header);
  IF Parser.CurrTokenId = CSTII_Function THEN
    IsFunction := True
  ELSE IF Parser.CurrTokenId = CSTII_Procedure THEN
    IsFunction := False
  ELSE
    BEGIN
      Parser.Free;
      Result := NIL;
      exit;
    END;
  Decl := '';
  Parser.Next;
  IF Parser.CurrTokenId <> CSTI_Identifier THEN
    BEGIN
      Parser.Free;
      Result := NIL;
      exit;
    END; {if}
  Name := Parser.GetToken;
  Parser.Next;
  IF Parser.CurrTokenId = CSTI_OpenRound THEN
    BEGIN
      Parser.Next;
      IF Parser.CurrTokenId <> CSTI_CloseRound THEN
        BEGIN
          WHILE True DO
            BEGIN
              IF Parser.CurrTokenId = CSTII_Const THEN
                BEGIN
                  Modifier := '@';
                  Parser.Next;
                END ELSE
                IF Parser.CurrTokenId = CSTII_Var THEN
                  BEGIN
                    modifier := '!';
                    Parser.Next;
                  END
                ELSE
                  modifier := '@';
              IF Parser.CurrTokenId <> CSTI_Identifier THEN
                BEGIN
                  Parser.Free;
                  Result := NIL;
                  exit;
                END;
              VNames := Parser.GetToken + '|';
              Parser.Next;
              WHILE Parser.CurrTokenId = CSTI_Comma DO
                BEGIN
                  Parser.Next;
                  IF Parser.CurrTokenId <> CSTI_Identifier THEN
                    BEGIN
                      Parser.Free;
                      Result := NIL;
                      exit;
                    END;
                  VNames := VNames + Parser.GetToken + '|';
                  Parser.Next;
                END;
              IF Parser.CurrTokenId <> CSTI_Colon THEN
                BEGIN
                  Parser.Free;
                  Result := NIL;
                  exit;
                END;
              Parser.Next;
              VCType := FindType(Parser.GetToken);
              IF VCType = Cardinal(-1) THEN
                BEGIN
                  Parser.Free;
                  Result := NIL;
                  exit;
                END;
              WHILE Pos('|', VNames) > 0 DO
                BEGIN
                  Decl := Decl + ' ' + modifier + copy(VNames, 1, Pos('|', VNames) - 1)
                    +
                    ' ' + inttostr(VCType);
                  Delete(VNames, 1, Pos('|', VNames));
                END;
              Parser.Next;
              IF Parser.CurrTokenId = CSTI_CloseRound THEN
                break;
              IF Parser.CurrTokenId <> CSTI_Semicolon THEN
                BEGIN
                  Parser.Free;
                  Result := NIL;
                  exit;
                END;
              Parser.Next;
            END; {while}
        END; {if}
      Parser.Next;
    END; {if}
  IF IsFunction THEN
    BEGIN
      IF Parser.CurrTokenId <> CSTI_Colon THEN
        BEGIN
          Parser.Free;
          Result := NIL;
          exit;
        END;

      Parser.Next;
      VCType := FindType(Parser.GetToken);
      IF VCType = Cardinal(-1) THEN
        BEGIN
          Parser.Free;
          Result := NIL;
          exit;
        END;
    END
  ELSE
    VCType := Cardinal(-1);
  Decl := inttostr(VCType) + Decl;
  Parser.Free;
  New(x);
  x^.Name := Name;
  x^.NameHash := MakeHash(Name);
  x^.FExportName := True;
  x^.Decl := Decl;
  Result := x;
  FRegProcs.Add(x);
END;

FUNCTION TSCAbstractCompiler.MakeHint(CONST Module: STRING; E: TSCCompilerHint; CONST Param: STRING): PSCCompilerMessage;
VAR
  n: PSCCompilerMessage;
BEGIN
  New(n);
  n^.ModuleName := Module;
  n^.Param := Param;
  n^.Position := FParser.CurrTokenPos;
  n^.MessageType := ptHint;
  n^.Hint := E;
  FMessages.Add(n);
  Result := n;
END;

FUNCTION TSCAbstractCompiler.MakeError(CONST Module: STRING; E:
  TSCCompilerError; CONST Param: STRING): PSCCompilerMessage;
VAR
  n: PSCCompilerMessage;
BEGIN
  New(n);
  n^.ModuleName := Module;
  n^.Param := Param;
  n^.Position := FParser.CurrTokenPos;
  n^.MessageType := ptError;
  n^.Error := E;
  FMessages.Add(n);
  Result := n;
END;

FUNCTION TSCAbstractCompiler.MakeWarning(CONST Module: STRING; E:
  TSCCompilerWarning; CONST Param: STRING): PSCCompilerMessage;
VAR
  n: PSCCompilerMessage;
BEGIN
  New(n);
  n^.ModuleName := Module;
  n^.Param := Param;
  n^.Position := FParser.CurrTokenPos;
  n^.MessageType := ptWarning;
  n^.Warning := E;
  FMessages.Add(n);
  Result := n;
END;

PROCEDURE TSCAbstractCompiler.Clear;
VAR
  l: Integer;
  p: PSCCompilerMessage;
BEGIN
  FDebugOutput := '';
  FOutput := '';
  FOR l := 0 TO FMessages.Count - 1 DO
    BEGIN
      p := FMessages.GetItem(l);
      Dispose(p);
    END;
  FMessages.Clear;
  FOR L := FAutoFreeList.Count - 1 DOWNTO 0 DO
    BEGIN
      TObject(FAutoFreeList.GetItem(l)).Free;
    END;
  FAutoFreeList.Clear;
END;


PROCEDURE DisposeVariant(p: PIfRVariant);
BEGIN
  IF p <> NIL THEN
    BEGIN
      p^.Value := '';
      Dispose(p);
    END;
END;

TYPE
  PParam = ^TParam;
  TParam = RECORD
    InReg, OutReg: PSCValue;
    FType: Cardinal;
    OutRegPos: Cardinal;
  END;

PROCEDURE DisposePValue(r: PSCValue); FORWARD;

PROCEDURE FreeRecFields(List: TSCList);
VAR
  i: Longint;
  p: PIFRecField;
BEGIN
  IF list = NIL THEN
    exit;
  FOR i := List.Count - 1 DOWNTO 0 DO
    BEGIN
      p := List.GetItem(i);
      IF p^.FKind >= 2 THEN
        BEGIN
          DisposePValue(p^.ReadArrayFieldNoFrom);
        END;
      Dispose(p);
    END;
  List.Free;
END;

PROCEDURE DisposePValue(r: PSCValue);
VAR
  l: Longint;
  p: PCalc_Item;
  P2: PParam;
BEGIN
  IF (r <> NIL) AND ((r^.Modifiers AND 128) = 0) THEN
    BEGIN
      IF (r^.FType = CVAL_Array) THEN
        BEGIN
          FOR l := 0 TO r.ArrayItems.Count - 1 DO
            BEGIN
              DisposePValue(R.ArrayItems.GetItem(l));
            END;
          r.ArrayItems.Free;
        END ELSE
        IF (r^.FType = CVAL_AllocatedStackReg) OR (r^.FType = CVAL_Addr) OR (r^.FType = CVAL_PushAddr) THEN
          BEGIN
            FreeRecFields(R^.RecField);
          END
        ELSE IF r.FType = CVAL_Data THEN
          DisposeVariant(r^.FData)
        ELSE IF r.FType = CVAL_Eval THEN
          BEGIN
            FOR l := 0 TO r.SubItems.Count - 1 DO
              BEGIN
                p := r.SubItems.GetItem(l);
                IF NOT p^.C THEN
                  DisposePValue(p^.OutRec);
                Dispose(p);
              END;
            r^.SubItems.Free;
          END
        ELSE IF (r.FType = CVAL_Proc) OR (r.FType = CVAL_varProc) THEN
          BEGIN
            FOR l := 0 TO r^.Parameters.Count - 1 DO
              BEGIN
                P2 := r^.Parameters.GetItem(l);
                IF P2^.InReg <> NIL THEN
                  DisposePValue(P2^.InReg);
                Dispose(P2);
              END;
            r.Parameters.Free;
            IF r.FType = CVAL_VarProc THEN
              DisposePValue(r._ProcNo);
          END ELSE IF (r.FType = CVAL_ClassPropertyCallGet) OR (r.FType = CVAL_ClassPropertyCallSet) OR (r.FType = CVAL_ClassMethodCall) OR (r.FType = CVAL_ClassProcCall) THEN
          BEGIN
            DisposePValue(r.Self);
            FOR l := 0 TO r^.Params.Count - 1 DO
              BEGIN
                P2 := r^.Params.GetItem(l);
                IF P2^.InReg <> NIL THEN
                  DisposePValue(P2^.InReg);
                Dispose(P2);
              END;
          END;
      Dispose(r);
    END;
END;

FUNCTION TSCAbstractCompiler.GetTypeCopyLink(p: PSCType): PSCType;
BEGIN
  IF p^.BaseType = BtTypeCopy THEN BEGIN
      IF p^.Ext <> NIL THEN
        Result := p^.Ext
      ELSE
        Result := NIL
    END ELSE Result := p;
END;

FUNCTION IsIntType(b: TSCBaseType): Boolean;
BEGIN
  CASE b OF
    btU8, btS8, btU16, btS16, btU32, btS32{$IFNDEF NOINT64}, btS64{$ENDIF}: Result := True;
  ELSE
    Result := False;
  END;
END;

FUNCTION IsRealType(b: TSCBaseType): Boolean;
BEGIN
  CASE b OF
    btSingle, btDouble, btExtended: Result := True;
  ELSE
    Result := False;
  END;
END;

FUNCTION IsIntRealType(b: TSCBaseType): Boolean;
BEGIN
  CASE b OF
    btSingle, btDouble, btExtended, btU8, btS8, btU16, btS16, btU32, btS32{$IFNDEF NOINT64}, btS64{$ENDIF}:
      Result := True;
  ELSE
    Result := False;
  END;

END;

FUNCTION DiffRec(p1, p2: PIFRecField): Boolean;
BEGIN
  Result :=
    (p1^.FKind <> p2^.FKind) OR
    (p1^.RecFieldNo <> p2^.RecFieldNo);
END;

FUNCTION SameReg(x1, x2: PSCValue): Boolean;
VAR
  I: Longint;
BEGIN
  IF x1^.FType = x2^.FType THEN
    BEGIN
      CASE x1^.FType OF
        CVAL_Addr, CVAL_PushAddr, CVAL_AllocatedStackReg, CVAL_AllocatedStackReg + 1:
          BEGIN
            IF x1^.Address = x2^.Address THEN
              BEGIN
                IF (x1^.RecField = NIL) AND (x2^.RecField = NIL) THEN
                  Result := True
                ELSE IF (x1^.RecField <> NIL) AND (x2^.RecField <> NIL) AND
                  (x1^.RecField.Count = x2^.RecField.Count) THEN
                  BEGIN
                    FOR I := x1^.RecField.Count - 1 DOWNTO 0 DO
                      BEGIN
                        IF DiffRec(x1^.RecField.GetItem(I), x2^.RecField.GetItem(I))
                          THEN
                          BEGIN
                            Result := False;
                            exit;
                          END;
                      END;
                    Result := True;
                  END
                ELSE
                  Result := False;
              END
            ELSE
              Result := False;
          END;
      ELSE
        Result := False;
      END;
    END
  ELSE
    Result := False;
END;

FUNCTION D1(CONST s: STRING): STRING;
BEGIN
  Result := copy(s, 2, Length(s) - 1);
END;

FUNCTION TSCAbstractCompiler.AT2UT(L: Cardinal): Cardinal;
VAR
  i: Longint;
  p: PSCType;
BEGIN
  IF L = Cardinal(-1) THEN BEGIN Result := Cardinal(-1); exit;
    END;
  p := FAvailableTypes.GetItem(L);
  p := GetTypeCopyLink(p);
  IF p^.Used THEN
    BEGIN
      FOR i := 0 TO FUsedTypes.Count - 1 DO
        BEGIN
          IF FUSedTypes.GetItem(I) = P THEN
            BEGIN
              Result := i;
              exit;
            END;
        END;
    END;
  UpdateRecordFields(p);
  p^.Used := True;
  FUsedTypes.Add(p);
  Result := FUsedTypes.Count - 1;
END;


PROCEDURE TSCAbstractCompiler.ReplaceTypes(VAR s: STRING);
VAR
  NewS: STRING;
  ts: STRING;
BEGIN
  ts := GRFW(s);
  IF ts <> '-1' THEN
    BEGIN
      NewS := IntToStr(AT2UT(StrToInt(ts)));
    END
  ELSE
    NewS := '-1';
  WHILE length(s) > 0 DO
    BEGIN
      NewS := NewS + ' ' + grfw(s);
      ts := grfw(s);
      NewS := NewS + ' ' + IntToStr(AT2UT(StrToInt(ts)));
    END;
  s := NewS;
END;


FUNCTION TSCPascalCompiler.GetUInt(FUseTypes: TSCList; Src: PIfRVariant; VAR s: Boolean): Cardinal;
BEGIN
  CASE PSCType(FUseTypes.GetItem(Src^.FType))^.BaseType OF
    btU8: Result := TbtU8((@Src^.Value[1])^);
    btS8: Result := TbtS8((@Src^.Value[1])^);
    btU16: Result := TbtU16((@Src^.Value[1])^);
    btS16: Result := TbtS16((@Src^.Value[1])^);
    btU32: Result := TbtU32((@Src^.Value[1])^);
    btS32: Result := TbtS32((@Src^.Value[1])^);
  ELSE
    BEGIN
      s := False;
      Result := 0;
    END;
  END;
END;

FUNCTION TSCPascalCompiler.GetInt(FUseTypes: TSCList; Src: PIfRVariant; VAR s: Boolean): Longint;
BEGIN
  CASE PSCType(FUseTypes.GetItem(Src^.FType))^.BaseType OF
    btU8: Result := TbtU8((@Src^.Value[1])^);
    btS8: Result := TbtS8((@Src^.Value[1])^);
    btU16: Result := TbtU16((@Src^.Value[1])^);
    btS16: Result := TbtS16((@Src^.Value[1])^);
    btU32: Result := TbtU32((@Src^.Value[1])^);
    btS32: Result := TbtS32((@Src^.Value[1])^);
  ELSE
    BEGIN
      s := False;
      Result := 0;
    END;
  END;
END;

FUNCTION TSCPascalCompiler.GetReal(FUseTypes: TSCList; Src: PIfRVariant; VAR s: Boolean): Extended;
BEGIN
  CASE PSCType(FUseTypes.GetItem(Src^.FType))^.BaseType OF
    btU8: Result := TbtU8((@Src^.Value[1])^);
    btS8: Result := TbtS8((@Src^.Value[1])^);
    btU16: Result := TbtU16((@Src^.Value[1])^);
    btS16: Result := TbtS16((@Src^.Value[1])^);
    btU32: Result := TbtU32((@Src^.Value[1])^);
    btS32: Result := TbtS32((@Src^.Value[1])^);
    btSingle: Result := TbtSingle((@Src^.Value[1])^);
    btDouble: Result := TbtDouble((@Src^.Value[1])^);
    btExtended: Result := TbtExtended((@Src^.Value[1])^);
  ELSE
    BEGIN
      s := False;
      Result := 0;
    END;
  END;
END;

FUNCTION TSCPascalCompiler.GetString(FUseTypes: TSCList; Src: PIfRVariant; VAR s: Boolean): STRING;
BEGIN
  CASE PSCType(FUseTypes.GetItem(Src^.FType))^.BaseType OF
    btChar: Result := Src^.Value;
    btString: Result := Src^.Value;
  ELSE
    BEGIN
      s := False;
      Result := '';
    END;
  END;
END;

FUNCTION TSCPascalCompiler.PreCalc(FUseTypes: TSCList; Var1Mod: Byte; var1: PIFRVariant; Var2Mod: Byte; Var2: PIfRVariant; Cmd: Byte; Pos: Cardinal): Boolean;
  { var1=dest, var2=src }
VAR
  b: Boolean;

  PROCEDURE SetBoolean(b: Boolean);
  BEGIN
    IF FUseTypes = FAvailableTypes THEN
      Var1^.FType := FBooleanType
    ELSE
      Var1^.FType := at2ut(FBooleanType);
    var1^.Value := Chr(Ord(b));
  END;
  FUNCTION ab(b: Longint): Longint;
  BEGIN
    ab := Longint(b = 0);
  END;

BEGIN
  Result := True;
  TRY
    CASE Cmd OF
      0:
        BEGIN { + }
          CASE PSCType(FUseTypes.GetItem(var1^.FType))^.BaseType OF
            btU8: TbtU8((@var1^.Value[1])^) := TbtU8((@var1^.Value[1])^) +
              GetUInt(FUseTypes, Var2, Result);
            btS8: TbtS8((@var1^.Value[1])^) := TbtS8((@var1^.Value[1])^) +
              GetInt(FUseTypes, Var2, Result);
            btU16: TbtU16((@var1^.Value[1])^) := TbtU16((@var1^.Value[1])^) +
              GetUInt(FUseTypes, Var2, Result);
            btS16: TbtS16((@var1^.Value[1])^) := TbtS16((@var1^.Value[1])^) +
              GetInt(FUseTypes, Var2, Result);
            btU32: TbtU32((@var1^.Value[1])^) := TbtU32((@var1^.Value[1])^) +
              GetUInt(FUseTypes, Var2, Result);
            btS32: TbtS32((@var1^.Value[1])^) := TbtS32((@var1^.Value[1])^) +
              GetInt(FUseTypes, Var2, Result);
            btSingle: TbtSingle((@var1^.Value[1])^) :=
              TbtSingle((@var1^.Value[1])^) + GetReal(FUseTypes, Var2, Result);
            btDouble: TbtDouble((@var1^.Value[1])^) :=
              TbtDouble((@var1^.Value[1])^) + GetReal(FUseTypes, Var2, Result);
            btExtended: TbtExtended((@var1^.Value[1])^) :=
              TbtExtended((@var1^.Value[1])^) + GetReal(FUseTypes, Var2, Result);
            btString: var1^.Value := var1^.Value + GetString(FUseTypes, Var2, Result);
          END;
        END;
      1:
        BEGIN { - }
          CASE PSCType(FUseTypes.GetItem(var1^.FType))^.BaseType OF
            btU8: TbtU8((@var1^.Value[1])^) := TbtU8((@var1^.Value[1])^) -
              GetUInt(FUseTypes, Var2, Result);
            btS8: TbtS8((@var1^.Value[1])^) := TbtS8((@var1^.Value[1])^) -
              GetInt(FUseTypes, Var2, Result);
            btU16: TbtU16((@var1^.Value[1])^) := TbtU16((@var1^.Value[1])^) -
              GetUInt(FUseTypes, Var2, Result);
            btS16: TbtS16((@var1^.Value[1])^) := TbtS16((@var1^.Value[1])^) -
              GetInt(FUseTypes, Var2, Result);
            btU32: TbtU32((@var1^.Value[1])^) := TbtU32((@var1^.Value[1])^) -
              GetUInt(FUseTypes, Var2, Result);
            btS32: TbtS32((@var1^.Value[1])^) := TbtS32((@var1^.Value[1])^) -
              GetInt(FUseTypes, Var2, Result);
            btSingle: TbtSingle((@var1^.Value[1])^) :=
              TbtSingle((@var1^.Value[1])^) - GetReal(FUseTypes, Var2, Result);
            btDouble: TbtDouble((@var1^.Value[1])^) :=
              TbtDouble((@var1^.Value[1])^) - GetReal(FUseTypes, Var2, Result);
            btExtended: TbtExtended((@var1^.Value[1])^) :=
              TbtExtended((@var1^.Value[1])^) - GetReal(FUseTypes, Var2, Result);
          END;
        END;
      2:
        BEGIN { * }
          CASE PSCType(FUseTypes.GetItem(var1^.FType))^.BaseType OF
            btU8: TbtU8((@var1^.Value[1])^) := TbtU8((@var1^.Value[1])^) *
              GetUInt(FUseTypes, Var2, Result);
            btS8: TbtS8((@var1^.Value[1])^) := TbtS8((@var1^.Value[1])^) *
              GetInt(FUseTypes, Var2, Result);
            btU16: TbtU16((@var1^.Value[1])^) := TbtU16((@var1^.Value[1])^) *
              GetUInt(FUseTypes, Var2, Result);
            btS16: TbtS16((@var1^.Value[1])^) := TbtS16((@var1^.Value[1])^) *
              GetInt(FUseTypes, Var2, Result);
            btU32: TbtU32((@var1^.Value[1])^) := TbtU32((@var1^.Value[1])^) *
              GetUInt(FUseTypes, Var2, Result);
            btS32: TbtS32((@var1^.Value[1])^) := TbtS32((@var1^.Value[1])^) *
              GetInt(FUseTypes, Var2, Result);
            btSingle: TbtSingle((@var1^.Value[1])^) :=
              TbtSingle((@var1^.Value[1])^) * GetReal(FUseTypes, Var2, Result);
            btDouble: TbtDouble((@var1^.Value[1])^) :=
              TbtDouble((@var1^.Value[1])^) * GetReal(FUseTypes, Var2, Result);
            btExtended: TbtExtended((@var1^.Value[1])^) :=
              TbtExtended((@var1^.Value[1])^) * GetReal(FUseTypes, Var2, Result);
          END;
        END;
      3:
        BEGIN { / }
          CASE PSCType(FUseTypes.GetItem(var1^.FType))^.BaseType OF
            btU8:
              TbtU8((@var1^.Value[1])^) := TbtU8((@var1^.Value[1])^) DIV
                GetUInt(FUseTypes, Var2, Result);
            btS8:
              TbtS8((@var1^.Value[1])^) := TbtS8((@var1^.Value[1])^) DIV
                GetInt(FUseTypes, Var2, Result);
            btU16:
              TbtU16((@var1^.Value[1])^) := TbtU16((@var1^.Value[1])^) DIV
                GetUInt(FUseTypes, Var2, Result);
            btS16:
              TbtS16((@var1^.Value[1])^) := TbtS16((@var1^.Value[1])^) DIV
                GetInt(FUseTypes, Var2, Result);
            btU32:
              TbtU32((@var1^.Value[1])^) := TbtU32((@var1^.Value[1])^) DIV
                GetUInt(FUseTypes, Var2, Result);
            btS32:
              TbtS32((@var1^.Value[1])^) := TbtS32((@var1^.Value[1])^) DIV
                GetInt(FUseTypes, Var2, Result);
            btSingle:
              TbtSingle((@var1^.Value[1])^) := TbtSingle((@var1^.Value[1])^)
                / GetReal(FUseTypes, Var2, Result);
            btDouble:
              TbtDouble((@var1^.Value[1])^) := TbtDouble((@var1^.Value[1])^)
                / GetReal(FUseTypes, Var2, Result);
            btExtended:
              TbtExtended((@var1^.Value[1])^) :=
                TbtExtended((@var1^.Value[1])^) / GetReal(FUseTypes, Var2, Result);
          END;
        END;
      4:
        BEGIN { MOD }
          CASE PSCType(FUseTypes.GetItem(var1^.FType))^.BaseType OF
            btU8:
              TbtU8((@var1^.Value[1])^) := TbtU8((@var1^.Value[1])^) MOD
                GetUInt(FUseTypes, Var2, Result);
            btS8:
              TbtS8((@var1^.Value[1])^) := TbtS8((@var1^.Value[1])^) MOD
                GetInt(FUseTypes, Var2, Result);
            btU16:
              TbtU16((@var1^.Value[1])^) := TbtU16((@var1^.Value[1])^) MOD
                GetUInt(FUseTypes, Var2, Result);
            btS16:
              TbtS16((@var1^.Value[1])^) := TbtS16((@var1^.Value[1])^) MOD
                GetInt(FUseTypes, Var2, Result);
            btU32:
              TbtU32((@var1^.Value[1])^) := TbtU32((@var1^.Value[1])^) MOD
                GetUInt(FUseTypes, Var2, Result);
            btS32:
              TbtS32((@var1^.Value[1])^) := TbtS32((@var1^.Value[1])^) MOD
                GetInt(FUseTypes, Var2, Result);
          END;
        END;
      5:
        BEGIN { SHL }
          CASE PSCType(FUseTypes.GetItem(var1^.FType))^.BaseType OF
            btU8: TbtU8((@var1^.Value[1])^) := TbtU8((@var1^.Value[1])^) SHL
              GetUInt(FUseTypes, Var2, Result);
            btS8: TbtS8((@var1^.Value[1])^) := TbtS8((@var1^.Value[1])^) SHL
              GetInt(FUseTypes, Var2, Result);
            btU16: TbtU16((@var1^.Value[1])^) := TbtU16((@var1^.Value[1])^)
              SHL
                GetUInt(FUseTypes, Var2, Result);
            btS16: TbtS16((@var1^.Value[1])^) := TbtS16((@var1^.Value[1])^)
              SHL
                GetInt(FUseTypes, Var2, Result);
            btU32: TbtU32((@var1^.Value[1])^) := TbtU32((@var1^.Value[1])^)
              SHL
                GetUInt(FUseTypes, Var2, Result);
            btS32: TbtS32((@var1^.Value[1])^) := TbtS32((@var1^.Value[1])^)
              SHL
                GetInt(FUseTypes, Var2, Result);
          END;
        END;
      6:
        BEGIN { SHR }
          CASE PSCType(FUseTypes.GetItem(var1^.FType))^.BaseType OF
            btU8: TbtU8((@var1^.Value[1])^) := TbtU8((@var1^.Value[1])^) SHR
              GetUInt(FUseTypes, Var2, Result);
            btS8: TbtS8((@var1^.Value[1])^) := TbtS8((@var1^.Value[1])^) SHR
              GetInt(FUseTypes, Var2, Result);
            btU16: TbtU16((@var1^.Value[1])^) := TbtU16((@var1^.Value[1])^)
              SHR
                GetUInt(FUseTypes, Var2, Result);
            btS16: TbtS16((@var1^.Value[1])^) := TbtS16((@var1^.Value[1])^)
              SHR
                GetInt(FUseTypes, Var2, Result);
            btU32: TbtU32((@var1^.Value[1])^) := TbtU32((@var1^.Value[1])^)
              SHR
                GetUInt(FUseTypes, Var2, Result);
            btS32: TbtS32((@var1^.Value[1])^) := TbtS32((@var1^.Value[1])^)
              SHR
                GetInt(FUseTypes, Var2, Result);
          END;
        END;
      7:
        BEGIN { AND }
          CASE PSCType(FUseTypes.GetItem(var1^.FType))^.BaseType OF
            btU8: TbtU8((@var1^.Value[1])^) := TbtU8((@var1^.Value[1])^) AND
              GetUInt(FUseTypes, Var2, Result);
            btS8: TbtS8((@var1^.Value[1])^) := TbtS8((@var1^.Value[1])^) AND
              GetInt(FUseTypes, Var2, Result);
            btU16: TbtU16((@var1^.Value[1])^) := TbtU16((@var1^.Value[1])^)
              AND
                GetUInt(FUseTypes, Var2, Result);
            btS16: TbtS16((@var1^.Value[1])^) := TbtS16((@var1^.Value[1])^)
              AND
                GetInt(FUseTypes, Var2, Result);
            btU32: TbtU32((@var1^.Value[1])^) := TbtU32((@var1^.Value[1])^)
              AND
                GetUInt(FUseTypes, Var2, Result);
            btS32: TbtS32((@var1^.Value[1])^) := TbtS32((@var1^.Value[1])^)
              AND
                GetInt(FUseTypes, Var2, Result);
          END;
        END;
      8:
        BEGIN { OR }
          CASE PSCType(FUseTypes.GetItem(var1^.FType))^.BaseType OF
            btU8: TbtU8((@var1^.Value[1])^) := TbtU8((@var1^.Value[1])^) OR
              GetUInt(FUseTypes, Var2, Result);
            btS8: TbtS8((@var1^.Value[1])^) := TbtS8((@var1^.Value[1])^) OR
              GetInt(FUseTypes, Var2, Result);
            btU16: TbtU16((@var1^.Value[1])^) := TbtU16((@var1^.Value[1])^) OR
              GetUInt(FUseTypes, Var2, Result);
            btS16: TbtS16((@var1^.Value[1])^) := TbtS16((@var1^.Value[1])^) OR
              GetInt(FUseTypes, Var2, Result);
            btU32: TbtU32((@var1^.Value[1])^) := TbtU32((@var1^.Value[1])^) OR
              GetUInt(FUseTypes, Var2, Result);
            btS32: TbtS32((@var1^.Value[1])^) := TbtS32((@var1^.Value[1])^) OR
              GetInt(FUseTypes, Var2, Result);
          END;
        END;
      9:
        BEGIN { XOR }
          CASE PSCType(FUseTypes.GetItem(var1^.FType))^.BaseType OF
            btU8: TbtU8((@var1^.Value[1])^) := TbtU8((@var1^.Value[1])^) XOR
              GetUInt(FUseTypes, Var2, Result);
            btS8: TbtS8((@var1^.Value[1])^) := TbtS8((@var1^.Value[1])^) XOR
              GetInt(FUseTypes, Var2, Result);
            btU16: TbtU16((@var1^.Value[1])^) := TbtU16((@var1^.Value[1])^)
              XOR
                GetUInt(FUseTypes, Var2, Result);
            btS16: TbtS16((@var1^.Value[1])^) := TbtS16((@var1^.Value[1])^)
              XOR
                GetInt(FUseTypes, Var2, Result);
            btU32: TbtU32((@var1^.Value[1])^) := TbtU32((@var1^.Value[1])^)
              XOR
                GetUInt(FUseTypes, Var2, Result);
            btS32: TbtS32((@var1^.Value[1])^) := TbtS32((@var1^.Value[1])^)
              XOR
                GetInt(FUseTypes, Var2, Result);
          END;
        END;
      10:
        BEGIN { >= }
          CASE PSCType(FUseTypes.GetItem(var1^.FType))^.BaseType OF
            btU8: b := TbtU8((@var1^.Value[1])^) >= GetUInt(FUseTypes, Var2, Result);
            btS8: b := TbtS8((@var1^.Value[1])^) >= GetInt(FUseTypes, Var2, Result);
            btU16: b := TbtU16((@var1^.Value[1])^) >= GetUInt(FUseTypes, Var2, Result);
            btS16: b := TbtS16((@var1^.Value[1])^) >= GetInt(FUseTypes, Var2, Result);
            btU32: b := TbtU32((@var1^.Value[1])^) >= GetUInt(FUseTypes, Var2, Result);
            btS32: b := TbtS32((@var1^.Value[1])^) >= GetInt(FUseTypes, Var2, Result);
            btSingle: b := TbtSingle((@var1^.Value[1])^) >= GetReal(FUseTypes, Var2,
                Result);
            btDouble: b := TbtDouble((@var1^.Value[1])^) >= GetReal(FUseTypes, Var2,
                Result);
            btExtended: b := TbtExtended((@var1^.Value[1])^) >= GetReal(FUseTypes, Var2,
                Result);
          ELSE
            b := False;
          END;
          SetBoolean(b);
        END;
      11:
        BEGIN { <= }
          CASE PSCType(FUseTypes.GetItem(var1^.FType))^.BaseType OF
            btU8: b := TbtU8((@var1^.Value[1])^) <= GetUInt(FUseTypes, Var2, Result);
            btS8: b := TbtS8((@var1^.Value[1])^) <= GetInt(FUseTypes, Var2, Result);
            btU16: b := TbtU16((@var1^.Value[1])^) <= GetUInt(FUseTypes, Var2, Result);
            btS16: b := TbtS16((@var1^.Value[1])^) <= GetInt(FUseTypes, Var2, Result);
            btU32: b := TbtU32((@var1^.Value[1])^) <= GetUInt(FUseTypes, Var2, Result);
            btS32: b := TbtS32((@var1^.Value[1])^) <= GetInt(FUseTypes, Var2, Result);
            btSingle: b := TbtSingle((@var1^.Value[1])^) <= GetReal(FUseTypes, Var2,
                Result);
            btDouble: b := TbtDouble((@var1^.Value[1])^) <= GetReal(FUseTypes, Var2,
                Result);
            btExtended: b := TbtExtended((@var1^.Value[1])^) <= GetReal(FUseTypes, Var2,
                Result);
          ELSE
            b := False;
          END;
          SetBoolean(b);
        END;
      12:
        BEGIN { > }
          CASE PSCType(FUseTypes.GetItem(var1^.FType))^.BaseType OF
            btU8: b := TbtU8((@var1^.Value[1])^) > GetUInt(FUseTypes, Var2, Result);
            btS8: b := TbtS8((@var1^.Value[1])^) > GetInt(FUseTypes, Var2, Result);
            btU16: b := TbtU16((@var1^.Value[1])^) > GetUInt(FUseTypes, Var2, Result);
            btS16: b := TbtS16((@var1^.Value[1])^) > GetInt(FUseTypes, Var2, Result);
            btU32: b := TbtU32((@var1^.Value[1])^) > GetUInt(FUseTypes, Var2, Result);
            btS32: b := TbtS32((@var1^.Value[1])^) > GetInt(FUseTypes, Var2, Result);
            btSingle: b := TbtSingle((@var1^.Value[1])^) > GetReal(FUseTypes, Var2,
                Result);
            btDouble: b := TbtDouble((@var1^.Value[1])^) > GetReal(FUseTypes, Var2,
                Result);
            btExtended: b := TbtExtended((@var1^.Value[1])^) > GetReal(FUseTypes, Var2,
                Result);
          ELSE
            b := False;
          END;
          SetBoolean(b);
        END;
      13:
        BEGIN { < }
          CASE PSCType(FUseTypes.GetItem(var1^.FType))^.BaseType OF
            btU8: b := TbtU8((@var1^.Value[1])^) < GetUInt(FUseTypes, Var2, Result);
            btS8: b := TbtS8((@var1^.Value[1])^) < GetInt(FUseTypes, Var2, Result);
            btU16: b := TbtU16((@var1^.Value[1])^) < GetUInt(FUseTypes, Var2, Result);
            btS16: b := TbtS16((@var1^.Value[1])^) < GetInt(FUseTypes, Var2, Result);
            btU32: b := TbtU32((@var1^.Value[1])^) < GetUInt(FUseTypes, Var2, Result);
            btS32: b := TbtS32((@var1^.Value[1])^) < GetInt(FUseTypes, Var2, Result);
            btSingle: b := TbtSingle((@var1^.Value[1])^) < GetReal(FUseTypes, Var2,
                Result);
            btDouble: b := TbtDouble((@var1^.Value[1])^) < GetReal(FUseTypes, Var2,
                Result);
            btExtended: b := TbtExtended((@var1^.Value[1])^) < GetReal(FUseTypes, Var2,
                Result);
          ELSE
            b := False;
          END;
          SetBoolean(b);
        END;
      14:
        BEGIN { <> }
          CASE PSCType(FUseTypes.GetItem(var1^.FType))^.BaseType OF
            btU8: b := TbtU8((@var1^.Value[1])^) <> GetUInt(FUseTypes, Var2, Result);
            btS8: b := TbtS8((@var1^.Value[1])^) <> GetInt(FUseTypes, Var2, Result);
            btU16: b := TbtU16((@var1^.Value[1])^) <> GetUInt(FUseTypes, Var2, Result);
            btS16: b := TbtS16((@var1^.Value[1])^) <> GetInt(FUseTypes, Var2, Result);
            btU32: b := TbtU32((@var1^.Value[1])^) <> GetUInt(FUseTypes, Var2, Result);
            btS32: b := TbtS32((@var1^.Value[1])^) <> GetInt(FUseTypes, Var2, Result);
            btSingle: b := TbtSingle((@var1^.Value[1])^) <> GetReal(FUseTypes, Var2,
                Result);
            btDouble: b := TbtDouble((@var1^.Value[1])^) <> GetReal(FUseTypes, Var2,
                Result);
            btExtended: b := TbtExtended((@var1^.Value[1])^) <> GetReal(FUseTypes, Var2,
                Result);
          ELSE
            b := False;
          END;
          SetBoolean(b);
        END;
      15:
        BEGIN { = }
          CASE PSCType(FUseTypes.GetItem(var1^.FType))^.BaseType OF
            btU8: b := TbtU8((@var1^.Value[1])^) = GetUInt(FUseTypes, Var2, Result);
            btS8: b := TbtS8((@var1^.Value[1])^) = GetInt(FUseTypes, Var2, Result);
            btU16: b := TbtU16((@var1^.Value[1])^) = GetUInt(FUseTypes, Var2, Result);
            btS16: b := TbtS16((@var1^.Value[1])^) = GetInt(FUseTypes, Var2, Result);
            btU32: b := TbtU32((@var1^.Value[1])^) = GetUInt(FUseTypes, Var2, Result);
            btS32: b := TbtS32((@var1^.Value[1])^) = GetInt(FUseTypes, Var2, Result);
            btSingle: b := TbtSingle((@var1^.Value[1])^) = GetReal(FUseTypes, Var2,
                Result);
            btDouble: b := TbtDouble((@var1^.Value[1])^) = GetReal(FUseTypes, Var2,
                Result);
            btExtended: b := TbtExtended((@var1^.Value[1])^) = GetReal(FUseTypes, Var2,
                Result);
          ELSE
            b := False;
          END;
          SetBoolean(b);
        END;
    END;
  EXCEPT
    ON E: EDivByZero DO
      BEGIN
        Result := False;
        MakeError('', ecDivideByZero, '');
        Exit;
      END;
    ON E: EZeroDivide DO
      BEGIN
        Result := False;
        MakeError('', ecDivideByZero, '');
        Exit;
      END;
    ON E: EMathError DO
      BEGIN
        Result := False;
        MakeError('', ecMathError, e.Message);
        Exit;
      END;
    ON E: Exception DO
      BEGIN
        Result := False;
        MakeError('', ecInternalError, E.Message);
        Exit;
      END;
  END;
  IF NOT Result THEN
    MakeError('', ecTypeMismatch, '')^.Position := Pos;
END;

FUNCTION TSCPascalCompiler.IsDuplicate(CONST s: STRING): Boolean;
VAR
  h, l: Longint;
  x: PSCProcedure;
BEGIN
  h := MakeHash(s);
  IF (s = 'RESULT') THEN
    BEGIN
      Result := True;
      exit;
    END;

  FOR l := 0 TO FAvailableTypes.Count - 1 DO
    BEGIN
      IF (PSCType(FAvailableTypes.GetItem(l))^.NameHash = h) AND
        (PSCType(FAvailableTypes.GetItem(l))^.Name = s) THEN
        BEGIN
          Result := True;
          exit;
        END;
    END;

  FOR l := 0 TO FProcs.Count - 1 DO
    BEGIN
      x := FProcs.GetItem(l);
      IF x^.Internal THEN
        BEGIN
          IF (h = x^.NameHash) AND (s = x^.Name) THEN
            BEGIN
              Result := True;
              exit;
            END;
        END
      ELSE
        BEGIN
          IF (PSCUsedRegProc(x)^.RP^.NameHash = h) AND
            (PSCUsedRegProc(x)^.RP^.Name = s) THEN
            BEGIN
              Result := True;
              exit;
            END;
        END;
    END;
  FOR l := 0 TO FVars.Count - 1 DO
    BEGIN
      IF (PSCVar(FVars.GetItem(l))^.NameHash = h) AND
        (PSCVar(FVars.GetItem(l))^.Name = s) THEN
        BEGIN
          Result := True;
          exit;
        END;
    END;
  FOR l := 0 TO FConstants.Count - 1 DO
    BEGIN
      IF (PSCConstant(FConstants.GetItem(l))^.NameHash = h) AND
        (PSCConstant(FConstants.GetItem(l))^.Name = s) THEN
        BEGIN
          Result := TRue;
          exit;
        END;
    END;
  Result := False;
END;


FUNCTION TSCPascalCompiler.ReadType(CONST Name: STRING; FParser: TSCAbstractParser): Cardinal; // Cardinal(-1) = Invalid
VAR
  h, l, TypeNo: Longint;
  fieldname, s: STRING;
  RecSubVals: TSCList;
  rvv: PSCRecordType;
  p, p2: PSCType;
  FUNCTION ATNUT(C: Cardinal): Cardinal;
  VAR
    i: Longint;
    P: PSCType;
  BEGIN
    p := FAvailableTypes.GetItem(C);
    FOR i := 0 TO FUsedTypes.Count - 1 DO
      BEGIN
        IF FUsedTypes.GetItem(I) = P THEN
          BEGIN
            Result := I;
            exit;
          END;
      END;
    result := Cardinal(-1);
  END;
  PROCEDURE ClearRecSubVals;
  VAR
    I: Longint;
    p: PSCRecordType;
  BEGIN
    FOR I := 0 TO RecSubVals.Count - 1 DO
      BEGIN
        p := RecSubVals.GetItem(I);
        Dispose(p);
      END;
    RecSubVals.Free;
  END;

  PROCEDURE MakeRealFieldOffsets;
  VAR
    I: Longint;
    O: Cardinal;
    rvv: PSCRecordType;
  BEGIN
    O := 0;
    FOR I := 0 TO RecSubVals.Count - 1 DO
      BEGIN
        rvv := RecSubVals.GetItem(I);
        rvv^.RealFieldOffset := O;
        O := O + PSCType(FAvailableTypes.GetItem(rvv^.FType))^.TypeSize;
      END;
    p^.TypeSize := O;
  END;
  FUNCTION GetTypeCopy(i: Cardinal): Cardinal;
  BEGIN
    IF PSCType(FAvailableTypes.GetItem(I))^.BaseType = btTypeCopy THEN
      Result := GetTypeCopy(Cardinal(PSCType(FAvailableTypes.GetItem(I))^.Ext))
    ELSE
      Result := i;
  END;

  FUNCTION AddProcedure: Cardinal;
  VAR
    IsFunction: Boolean;
    VNames, Decl: STRING;
    modifier: Char;
    VCType: Cardinal;
    x: PSCType;
    xp: PSCProceduralType;
  BEGIN
    IF FParser.CurrTokenId = CSTII_Function THEN
      IsFunction := True
    ELSE
      IsFunction := False;
    Decl := '';
    FParser.Next;
    IF FParser.CurrTokenId = CSTI_OpenRound THEN
      BEGIN
        FParser.Next;
        IF FParser.CurrTokenId <> CSTI_CloseRound THEN
          BEGIN
            WHILE True DO
              BEGIN
                IF FParser.CurrTokenId = CSTII_Const THEN
                  BEGIN
                    Modifier := '@';
                    FParser.Next;
                  END ELSE
                  IF FParser.CurrTokenId = CSTII_Var THEN
                    BEGIN
                      modifier := '!';
                      FParser.Next;
                    END
                  ELSE
                    modifier := '@';
                IF FParser.CurrTokenId <> CSTI_Identifier THEN
                  BEGIN
                    Result := Cardinal(-1);
                    IF FParser = Self.FParser THEN
                      MakeError('', ecIdentifierExpected, '');
                    exit;
                  END;
                VNames := FParser.GetToken + '|';
                FParser.Next;
                WHILE FParser.CurrTokenId = CSTI_Comma DO
                  BEGIN
                    FParser.Next;
                    IF FParser.CurrTokenId <> CSTI_Identifier THEN
                      BEGIN
                        Result := Cardinal(-1);
                        IF FParser = Self.FParser THEN
                          MakeError('', ecIdentifierExpected, '');
                        exit;
                      END;
                    VNames := VNames + FParser.GetToken + '|';
                    FParser.Next;
                  END;
                IF FParser.CurrTokenId <> CSTI_Colon THEN
                  BEGIN
                    Result := Cardinal(-1);
                    IF FParser = Self.FParser THEN
                      MakeError('', ecColonExpected, '');
                    exit;
                  END;
                FParser.Next;
                IF FParser.CurrTokenId <> CSTI_Identifier THEN
                  BEGIN
                    Result := Cardinal(-1);
                    IF FParser = Self.FParser THEN
                      MakeError('', ecIdentifierExpected, '');
                    exit;
                  END;
                VCType := FindType(FParser.GetToken);
                IF VCType = Cardinal(-1) THEN
                  BEGIN
                    IF FParser = Self.FParser THEN
                      MakeError('', ecUnknownIdentifier, FParser.OriginalToken);
                    Result := Cardinal(-1);
                    exit;
                  END;
                WHILE Pos('|', VNames) > 0 DO
                  BEGIN
                    Decl := Decl + ' ' + modifier + copy(VNames, 1, Pos('|', VNames) - 1) +
                      ' ' + inttostr(VCType);
                    Delete(VNames, 1, Pos('|', VNames));
                  END;
                FParser.Next;
                IF FParser.CurrTokenId = CSTI_CloseRound THEN
                  break;
                IF FParser.CurrTokenId <> CSTI_Semicolon THEN
                  BEGIN
                    IF FParser = Self.FParser THEN
                      MakeError('', ecSemicolonExpected, '');
                    Result := Cardinal(-1);
                    exit;
                  END;
                FParser.Next;
              END; {while}
          END; {if}
        FParser.Next;
      END; {if}
    IF IsFunction THEN
      BEGIN
        IF FParser.CurrTokenId <> CSTI_Colon THEN
          BEGIN
            IF FParser = Self.FParser THEN
              MakeError('', ecColonExpected, '');
            Result := Cardinal(-1);
            exit;
          END;
        FParser.Next;
        IF FParser.CurrTokenId <> CSTI_Identifier THEN
          BEGIN
            Result := Cardinal(-1);
            IF FParser = Self.FParser THEN
              MakeError('', ecIdentifierExpected, '');
            exit;
          END;
        VCType := FindType(FParser.GetToken);
        IF VCType = Cardinal(-1) THEN
          BEGIN
            IF FParser = Self.FParser THEN
              MakeError('', ecUnknownIdentifier, FParser.OriginalToken);
            Result := Cardinal(-1);
            exit;
          END;
        FParser.Next;
      END
    ELSE
      VCType := Cardinal(-1);
    Decl := inttostr(VCType) + Decl;
    New(x);
    x^.FExport := False;
    x^.Name := Name;
    x^.NameHash := MakeHash(x^.Name);
    x^.BaseType := btProcPtr;
    x^.DeclarePosition := FParser.CurrTokenPos;
    x^.Used := False;
    x^.TypeSize := 1;
    x^.RecordSubVals := NIL;
    New(xp);
    x^.Ext := xp;
    xp^.ProcDef := Decl;
    FAvailableTypes.Add(X);
    Result := FAvailableTypes.Count - 1;
  END; {AddProcedure}

BEGIN
  IF (FParser.CurrTokenID = CSTII_Function) OR (FParser.CurrTokenID = CSTII_Procedure) THEN
    BEGIN
      Result := AddProcedure;
      Exit;
    END ELSE IF FParser.CurrTokenId = CSTI_OpenRound THEN
    BEGIN
      FParser.Next;
      L := 0;
      New(P);
      p^.NameHash := MakeHash(Name);
      p^.Name := Name;
      p^.BaseType := btEnum;
      p^.Used := False;
      p^.TypeSize := 1;
      p^.DeclarePosition := FParser.CurrTokenPos;
      p^.RecordSubVals := NIL;
      p^.FExport := False;
      p^.Ext := NIL;
      FAvailableTypes.Add(p);

      TypeNo := FAvailableTypes.Count - 1;
      REPEAT
        IF FParser.CurrTokenId <> CSTI_Identifier THEN
          BEGIN
            IF FParser = Self.FParser THEN
              MakeError('', ecIdentifierExpected, '');
            Result := Cardinal(-1);
            exit;
          END;
        s := FParser.GetToken;
        IF IsDuplicate(s) THEN
          BEGIN
            IF FParser = Self.FParser THEN
              MakeError('', ecDuplicateIdentifier, s);
            Result := Cardinal(-1);
            Exit;
          END;
        AddConstant(s, TypeNo)^.Value.Value := TransCardinalToStr(L);
        Inc(L);
        FParser.Next;
        IF FParser.CurrTokenId = CSTI_CloseRound THEN
          Break
        ELSE IF FParser.CurrTokenId <> CSTI_Comma THEN
          BEGIN
            IF FParser = Self.FParser THEN
              MakeError('', ecCloseRoundExpected, '');
            Result := Cardinal(-1);
            Exit;
          END;
        FParser.Next;
      UNTIL False;
      FParser.Next;
      p^.Ext := Pointer(L - 1);
      Result := TypeNo;
      exit;
    END ELSE
    IF FParser.CurrTokenId = CSTII_Array THEN
      BEGIN
        FParser.Next;
        IF FParser.CurrTokenId <> CSTII_Of THEN
          BEGIN
            IF FParser = Self.FParser THEN
              MakeError('', ecOfExpected, '');
            Result := Cardinal(-1);
            exit;
          END;
        FParser.Next;
        L := ReadType('', FParser);
        IF L = -1 THEN
          BEGIN
            IF FParser = Self.FParser THEN
              MakeError('', ecUnknownIdentifier, '');
            Result := Cardinal(-1);
            exit;
          END;
        IF Name = '' THEN
          BEGIN
            TypeNo := ATNUT(l);
            IF TypeNo <> -1 THEN
              BEGIN
                FOR h := 0 TO FUsedTypes.Count - 1 DO
                  BEGIN
                    p := FUsedTypes.GetItem(H);
                    IF (p^.BaseType = btArray) AND (p^.Ext = Pointer(TypeNo)) THEN
                      BEGIN
                        FOR l := 0 TO FAvailableTypes.Count - 1 DO
                          BEGIN
                            IF FAvailableTypes.GetItem(L) = P THEN
                              BEGIN
                                Result := l;
                                exit;
                              END;
                          END;
                        IF FParser = Self.FParser THEN
                          MakeError('', ecInternalError, '0001C');
                        Result := Cardinal(-1);
                        Exit;
                      END;
                  END;
              END;
            FOR h := 0 TO FAvailableTypes.Count - 1 DO
              BEGIN
                p := FAvailableTypes.GetItem(H);
                IF (p^.BaseType = btArray) AND (p^.Ext = Pointer(L)) AND (NOT p^.Used) THEN
                  BEGIN
                    Result := H;
                    Exit;
                  END;
              END;
          END;
        New(p);
        p^.NameHash := MakeHash(Name);
        p^.Name := Name;
        p^.Used := False;
        p^.BaseType := btArray;
        p^.TypeSize := 1;
        p^.DeclarePosition := FParser.CurrTokenPos;
        p^.RecordSubVals := NIL;
        p^.FExport := False;
        p^.Ext := Pointer(L);
        FAvailableTypes.Add(p);
        Result := Cardinal(FAvailableTypes.Count - 1);
        Exit;
      END
    ELSE IF FParser.CurrTokenId = CSTII_Record THEN
      BEGIN
        FParser.Next;
        RecSubVals := TSCList.Create;
        REPEAT
          REPEAT
            IF FParser.CurrTokenId <> CSTI_Identifier THEN
              BEGIN
                ClearRecSubVals;
                IF FParser = Self.FParser THEN
                  MakeError('', ecIdentifierExpected, '');
                Result := Cardinal(-1);
                exit;
              END;
            FieldName := FParser.GetToken;
            s := S + FieldName + '|';
            FParser.Next;
            TypeNo := MakeHash(S);
            FOR l := 0 TO RecSubVals.Count - 1 DO
              BEGIN
                IF (PSCRecordType(RecSubVals.GetItem(l))^.FieldNameHash = TypeNo) AND
                  (PSCRecordType(RecSubVals.GetItem(l))^.FieldName = s) THEN
                  BEGIN
                    IF FParser = Self.FParser THEN
                      MakeError('', ecDuplicateIdentifier, FParser.OriginalToken);
                    ClearRecSubVals;
                    Result := Cardinal(-1);
                    exit;
                  END;
              END;
            IF FParser.CurrTokenID = CSTI_Colon THEN Break ELSE
              IF FParser.CurrTokenID <> CSTI_Comma THEN
                BEGIN
                  IF FParser = Self.FParser THEN
                    MakeError('', ecColonExpected, '');
                  ClearRecSubVals;
                  Result := Cardinal(-1);
                  exit;
                END;
            FParser.Next;
          UNTIL False;
          FParser.Next;
          l := ReadType('', FParser);
          IF L = -1 THEN
            BEGIN
              ClearRecSubVals;
              Result := Cardinal(-1);
              exit;
            END;
          P := FAvailableTypes.GetItem(L);
          IF p^.BaseType = BtTypeCopy THEN
            BEGIN
              L := 0;
              FOR TypeNo := 0 TO FAvailableTypes.Count - 1 DO
                BEGIN
                  IF FAvailableTypes.GetItem(TypeNo) = p^.Ext THEN
                    BEGIN
                      L := TypeNo;
                      Break;
                    END;
                END;
            END;
          IF FParser.CurrTokenId <> CSTI_Semicolon THEN
            BEGIN
              ClearRecSubVals;
              IF FParser = Self.FParser THEN
                MakeError('', ecSemicolonExpected, '');
              Result := Cardinal(-1);
              exit;
            END; {if}
          FParser.Next;
          WHILE Pos('|', s) > 0 DO
            BEGIN
              fieldname := copy(s, 1, pos('|', s) - 1);
              Delete(s, 1, length(FieldName) + 1);
              New(rvv);
              rvv^.FieldName := fieldname;
              rvv^.FieldNameHash := MakeHash(fieldname);
              rvv^.FType := l;
              RecSubVals.Add(rvv);
            END;
        UNTIL FParser.CurrTokenId = CSTII_End;
        FParser.Next; // skip CSTII_End
        New(p);
        p^.NameHash := MakeHash(Name);
        p^.Name := Name;
        p^.BaseType := btRecord;
        p^.DeclarePosition := FParser.CurrTokenPos;
        p^.RecordSubVals := RecSubVals;
        p^.Used := False;
        p^.FExport := False;
        p^.Ext := NIL;
        FAvailableTypes.Add(p);
        Result := FAvailableTypes.Count - 1;
        MakeRealFieldOffsets;
        Exit;
      END ELSE IF FParser.CurrTokenId = CSTI_Identifier THEN
      BEGIN
        s := FParser.GetToken;
        h := MakeHash(s);
        p2 := NIL;
        FOR l := 0 TO FAvailableTypes.Count - 1 DO
          BEGIN
            IF (PSCType(FAvailableTypes.GetItem(l))^.NameHash = h) AND
              (PSCType(FAvailableTypes.GetItem(l))^.Name = s) THEN
              BEGIN
                FParser.Next;
                p2 := FAvailableTypes.GetItem(l);
                IF p2^.BaseType = BtTypeCopy THEN
                  BEGIN
                    P2 := p2^.Ext;
                  END;
                Break;
              END;
          END;
        IF p2 = NIL THEN
          BEGIN
            Result := Cardinal(-1);
            IF FParser = Self.FParser THEN
              MakeError('', ecUnknownType, FParser.OriginalToken);
            exit;
          END;
        IF Name <> '' THEN
          BEGIN
            New(p);
            p^.NameHash := MakeHash(Name);
            p^.Name := Name;
            p^.BaseType := BtTypeCopy;
            p^.TypeSize := 0;
            p^.DeclarePosition := FParser.CurrTokenPos;
            p^.RecordSubVals := NIL;
            p^.FExport := FAlse;
            p^.Used := False;
            p^.Ext := p2;
            FAvailableTypes.Add(p);
            Result := FAvailableTypes.Count - 1;
            Exit;
          END ELSE BEGIN
            FOR h := 0 TO FAvailableTypes.Count - 1 DO
              BEGIN
                IF FAvailableTypes.GetItem(h) = P2 THEN
                  BEGIN
                    Result := h;
                    Exit;
                  END;
              END;
          END;
      END;
  Result := Cardinal(-1);
  IF FParser = Self.FParser THEN
    MakeError('', ecIdentifierExpected, '');
  Exit;
END;


FUNCTION TSCPascalCompiler.DoVarBlock(proc: PSCProcedure): Boolean;
VAR
  VarName, s: STRING;
  VarType: Cardinal;
  VarNo: Cardinal;
  v: PSCVar;
  vp: PSCProcVar;

  FUNCTION VarIsDuplicate(CONST s: STRING): Boolean;
  VAR
    h, l: Longint;
    x: PSCProcedure;
    v: STRING;
  BEGIN
    h := MakeHash(s);
    IF (s = 'RESULT') THEN
      BEGIN
        Result := True;
        exit;
      END;

    FOR l := 0 TO FAvailableTypes.Count - 1 DO
      BEGIN
        IF (PSCType(FAvailableTypes.GetItem(l))^.NameHash = h) AND
          (PSCType(FAvailableTypes.GetItem(l))^.Name = s) THEN
          BEGIN
            Result := True;
            exit;
          END;
      END;

    FOR l := 0 TO FProcs.Count - 1 DO
      BEGIN
        x := FProcs.GetItem(l);
        IF x^.Internal THEN
          BEGIN
            IF (h = x^.NameHash) AND (s = x^.Name) THEN
              BEGIN
                Result := True;
                exit;
              END;
          END
        ELSE
          BEGIN
            IF (PSCUsedRegProc(x)^.RP^.NameHash = h) AND
              (PSCUsedRegProc(x)^.RP^.Name = s) THEN
              BEGIN
                Result := True;
                exit;
              END;
          END;
      END;
    IF proc <> NIL THEN
      BEGIN
        FOR l := 0 TO proc^.ProcVars.Count - 1 DO
          BEGIN
            IF (PSCProcVar(proc^.ProcVars.GetItem(l))^.NameHash = h) AND
              (PSCVar(proc^.ProcVars.GetItem(l))^.Name = s) THEN
              BEGIN
                Result := True;
                exit;
              END;
          END;
      END
    ELSE
      BEGIN
        FOR l := 0 TO FVars.Count - 1 DO
          BEGIN
            IF (PSCVar(FVars.GetItem(l))^.NameHash = h) AND
              (PSCVar(FVars.GetItem(l))^.Name = s) THEN
              BEGIN
                Result := True;
                exit;
              END;
          END;
      END;
    v := VarName;
    WHILE Pos('|', v) > 0 DO
      BEGIN
        IF copy(v, 1, Pos('|', v) - 1) = s THEN
          BEGIN
            Result := True;
            exit;
          END;
        Delete(v, 1, Pos('|', v));
      END;
    FOR l := 0 TO FConstants.Count - 1 DO
      BEGIN
        IF (PSCConstant(FConstants.GetItem(l))^.NameHash = h) AND
          (PSCConstant(FConstants.GetItem(l))^.Name = s) THEN
          BEGIN
            Result := TRue;
            exit;
          END;
      END;
    Result := False;
  END;

BEGIN
  Result := False;
  FParser.Next; // skip CSTII_Var
  IF FParser.CurrTokenId <> CSTI_Identifier THEN
    BEGIN
      MakeError('', ecIdentifierExpected, '');
      exit;
    END;
  REPEAT
    IF VarIsDuplicate(FParser.GetToken) THEN
      BEGIN
        MakeError('', ecDuplicateIdentifier, FParser.OriginalToken);
        exit;
      END;
    VarName := FParser.GetToken + '|';
    Varno := 0;
    IF @FOnUseVariable <> NIL THEN
      BEGIN
        IF Proc <> NIL THEN
          FOnUseVariable(Self, ivtVariable, Proc^.ProcVars.Count + VarNo, FProcs.Count - 1, FParser.CurrTokenPos)
        ELSE
          FOnUseVariable(Self, ivtGlobal, FVars.Count + VarNo, Cardinal(-1), FParser.CurrTokenPos)
      END;
    FParser.Next;
    WHILE FParser.CurrTokenId = CSTI_Comma DO
      BEGIN
        FParser.Next;
        IF FParser.CurrTokenId <> CSTI_Identifier THEN
          BEGIN
            MakeError('', ecIdentifierExpected, '');
          END;
        IF VarIsDuplicate(FParser.GetToken) THEN
          BEGIN
            MakeError('', ecDuplicateIdentifier, FParser.OriginalToken);
            exit;
          END;
        VarName := VarName + FParser.GetToken + '|';
        Inc(varno);
        IF @FOnUseVariable <> NIL THEN
          BEGIN
            IF Proc <> NIL THEN
              FOnUseVariable(Self, ivtVariable, Proc^.ProcVars.Count + VarNo, FProcs.Count - 1, FParser.CurrTokenPos)
            ELSE
              FOnUseVariable(Self, ivtGlobal, FVars.Count + VarNo, Cardinal(-1), FParser.CurrTokenPos)
          END;
        FParser.Next;
      END;
    IF FParser.CurrTokenId <> CSTI_Colon THEN
      BEGIN
        MakeError('', ecColonExpected, '');
        exit;
      END;
    FParser.Next;
    VarType := at2ut(ReadType('', FParser));
    IF VarType = Cardinal(-1) THEN
      BEGIN
        exit;
      END;
    WHILE Pos('|', VarName) > 0 DO
      BEGIN
        s := copy(VarName, 1, Pos('|', VarName) - 1);
        Delete(VarName, 1, Pos('|', VarName));
        IF proc = NIL THEN
          BEGIN
            New(v);
            v^.Used := False;
            v^.Name := s;
            v^.NameHash := MakeHash(v^.Name);
            v^.DeclarePosition := FParser.CurrTokenPos;
            v^.FType := VarType;
            FVars.Add(v);
          END
        ELSE
          BEGIN
            New(vp);
            vp^.Used := False;
            vp^.VarName := s;
            vp^.NameHash := MakeHash(vp^.VarName);
            vp^.VarType := VarType;
            vp^.CurrentlyUsed := False;
            vp^.Used := False;
            vp^.DeclarePosition := FParser.CurrTokenPos;
            proc.ProcVars.Add(vp);
          END;
      END;
    IF FParser.CurrTokenId <> CSTI_Semicolon THEN
      BEGIN
        MakeError('', ecSemicolonExpected, '');
        exit;
      END;
    FParser.Next;
  UNTIL FParser.CurrTokenId <> CSTI_Identifier;
  Result := True;
END;

FUNCTION TSCPascalCompiler.NewProc(CONST Name: STRING): PSCProcedure;
BEGIN
  New(Result);
  Result^.Forwarded := False;
  Result^.FExport := 0;
  Result^.Data := '';
  Result^.Decl := '-1';
  Result^.Name := Name;
  Result^.NameHash := MakeHash(Result^.Name);
  Result^.Used := False;
  Result^.Internal := True;
  Result^.ProcVars := TSCList.Create;
  Result^.ResUsed := False;
  Result^.DeclarePosition := FParser.CurrTokenPos;
  Result^.FLabels := TSCStringList.Create;
  Result^.FGotos := TSCStringList.Create;
  FProcs.Add(Result);
END;

FUNCTION TSCPascalCompiler.ProcessLabel(Proc: PSCProcedure): Boolean;
VAR
  CurrLabel: STRING;
  FUNCTION IsProcDuplic(CONST s: STRING): Boolean;
  VAR
    i: Longint;
    h: Longint;
    u: STRING;
  BEGIN
    h := MakeHash(s);
    IF s = 'RESULT' THEN
      Result := True
    ELSE IF Proc^.Name = s THEN
      Result := True
    ELSE IF IsDuplicate(s) THEN
      Result := True
    ELSE
      BEGIN
        u := Proc^.Decl;
        WHILE Length(u) > 0 DO
          BEGIN
            IF D1(GRFW(u)) = s THEN
              BEGIN
                Result := True;
                exit;
              END;
            GRFW(u);
          END;
        FOR i := 0 TO Proc^.ProcVars.Count - 1 DO
          BEGIN
            IF (PSCProcVar(Proc^.ProcVars.GetItem(I))^.NameHash = h) AND (PSCProcVar(Proc^.ProcVars.GetItem(I))^.VarName = s) THEN
              BEGIN
                Result := True;
                exit;
              END;
          END;
        FOR i := 0 TO Proc^.FLabels.Count - 1 DO
          BEGIN
            u := Proc^.FLabels.GetItem(i);
            delete(u, 1, 4);
            IF Longint((@u[1])^) = h THEN
              BEGIN
                delete(u, 1, 4);
                IF u = s THEN
                  BEGIN
                    Result := True;
                    exit;
                  END;
              END;
          END;
        Result := False;
      END;
  END;
BEGIN
  FParser.Next;
  WHILE true DO
    BEGIN
      IF FParser.CurrTokenId <> CSTI_Identifier THEN
        BEGIN
          MakeError('', ecIdentifierExpected, '');
          Result := False;
          exit;
        END;
      CurrLabel := FParser.GetToken;
      IF IsDuplicate(CurrLabel) OR IsProcDuplic(CurrLabel) THEN
        BEGIN
          MakeError('', ecDuplicateIdentifier, '');
          Result := False;
          exit;
        END;
      FParser.Next;
      Proc^.FLabels.Add(#$FF#$FF#$FF#$FF + mi2s(MakeHash(CurrLabel)) + CurrLabel);
      IF FParser.CurrTokenId = CSTI_Semicolon THEN
        BEGIN
          FParser.Next;
          Break;
        END;
      IF FParser.CurrTokenId <> CSTI_Comma THEN
        BEGIN
          MakeError('', ecCommaExpected, '');
          Result := False;
          exit;
        END;
      FParser.Next;
    END;
  Result := True;
END;

PROCEDURE TSCPascalCompiler.Debug_SavePosition(ProcNo: Cardinal; Proc: PSCProcedure);
BEGIN
  WriteDebugData(#4 + mi2s(ProcNo) + mi2s(Length(Proc^.Data)) + mi2s(FParser.CurrTokenPos));
END;

PROCEDURE TSCPascalCompiler.Debug_WriteParams(ProcNo: Cardinal; Proc: PSCProcedure);
VAR
  I: Longint;
  s, d: STRING;
BEGIN
  s := #2 + mi2s(ProcNo);
  d := proc^.Decl;
  IF GRFW(d) <> '-1' THEN
    BEGIN
      s := s + 'RESULT' + #1;
    END;
  WHILE Length(d) > 0 DO
    BEGIN
      s := s + D1(GRFW(d)) + #1;
      GRFW(d);
    END;
  s := s + #0#3 + mi2s(ProcNo);
  FOR I := 0 TO proc^.ProcVars.Count - 1 DO
    BEGIN
      s := s + PSCProcVar(proc^.ProcVars.GetItem(I))^.VarName + #1;
    END;
  s := s + #0;
  WriteDebugData(s);
END;



FUNCTION TSCPascalCompiler.ProcessFunction: Boolean;
VAR
  FunctionType: TFuncType;
  FunctionName: STRING;
  FunctionParamNames: STRING;
  FunctionTempType: Cardinal;
  ParamNo: Cardinal;
  FunctionDecl: STRING;
  modifier: Char;
  F2, Func: PSCProcedure;
  EPos: Cardinal;
  pp: PSCRegProc;
  pp2: PSCUsedRegProc;
  FuncNo, I: Longint;
  PROCEDURE CheckVars(Func: PSCProcedure);
  VAR
    i: Integer;
    p: PSCProcVar;
  BEGIN
    FOR i := 0 TO Func^.ProcVars.Count - 1 DO
      BEGIN
        p := Func^.ProcVars.GetItem(I);
        IF NOT p^.Used THEN
          BEGIN
            MakeHint('', ehVariableNotUsed, p^.VarName)^.Position := p^.DeclarePosition;
          END;
      END;
    IF (NOT Func^.ResUsed) AND (Fw(Func^.Decl) <> '-1') THEN
      BEGIN
        MakeHint('', ehVariableNotUsed, 'RESULT')^.Position := Func^.DeclarePosition;
      END;
  END;

  FUNCTION IsDuplic(CONST s: STRING): Boolean;
  VAR
    i: Longint;
    u: STRING;
  BEGIN
    IF s = 'RESULT' THEN
      Result := True
    ELSE IF FunctionName = s THEN
      Result := True
    ELSE IF IsDuplicate(s) THEN
      Result := True
    ELSE
      BEGIN
        u := FunctionDecl;
        WHILE Length(u) > 0 DO
          BEGIN
            IF D1(GRFW(u)) = s THEN
              BEGIN
                Result := True;
                exit;
              END;
            GRFW(u);
          END;
        u := FunctionParamNames;
        WHILE Pos('|', u) > 0 DO
          BEGIN
            IF copy(u, 1, Pos('|', u) - 1) = s THEN
              BEGIN
                Result := True;
                exit;
              END;
            Delete(u, 1, Pos('|', u));
          END;
        IF Func = NIL THEN
          BEGIN
            result := False;
            exit;
          END;
        FOR i := 0 TO Func^.ProcVars.Count - 1 DO
          BEGIN
            IF s = PSCProcVar(Func^.ProcVars.GetItem(I))^.VarName THEN
              BEGIN
                Result := True;
                exit;
              END;
          END;
        FOR i := 0 TO Func^.FLabels.Count - 1 DO
          BEGIN
            u := Func^.FLabels.GetItem(i);
            delete(u, 1, 4);
            IF u = s THEN
              BEGIN
                Result := True;
                exit;
              END;
          END;
        Result := False;
      END;
  END;
  PROCEDURE WriteProcVars(t: TSCList);
  VAR
    l: Longint;
    v: PSCProcVar;
  BEGIN
    FOR l := 0 TO t.Count - 1 DO
      BEGIN
        v := t.GetItem(l);
        Func^.Data := Func^.Data + chr(cm_pt) + mi2s(v^.VarType);
      END;
  END;

BEGIN
  IF FParser.CurrTokenId = CSTII_Procedure THEN
    FunctionType := ftProc
  ELSE
    FunctionType := ftFunc;
  Func := NIL;
  FParser.Next;
  Result := False;
  IF FParser.CurrTokenId <> CSTI_Identifier THEN
    BEGIN
      MakeError('', ecIdentifierExpected, '');
      exit;
    END;
  EPos := FParser.CurrTokenPos;
  FunctionName := FParser.GetToken;
  FuncNo := -1;
  FOR i := 0 TO FProcs.Count - 1 DO
    BEGIN
      f2 := FProcs.GetItem(i);
      IF (f2^.Internal) AND (f2^.Name = FunctionName) AND (f2^.Forwarded) THEN
        BEGIN
          Func := FProcs.GetItem(i);
          FuncNo := i;
          Break;
        END;
    END;
  IF (Func = NIL) AND IsDuplicate(FunctionName) THEN
    BEGIN
      MakeError('', ecDuplicateIdentifier, FunctionName);
      exit;
    END;
  FParser.Next;
  FunctionDecl := '';
  IF FParser.CurrTokenId = CSTI_OpenRound THEN
    BEGIN
      FParser.Next;
      IF FParser.CurrTokenId = CSTI_CloseRound THEN
        BEGIN
          FParser.Next;
        END
      ELSE
        BEGIN
          IF FunctionType = ftFunc THEN
            ParamNo := 1
          ELSE
            ParamNo := 0;
          WHILE True DO
            BEGIN
              IF FParser.CurrTokenId = CSTII_Var THEN
                BEGIN
                  modifier := '!';
                  FParser.Next;
                END
              ELSE
                modifier := '@';
              IF FParser.CurrTokenId <> CSTI_Identifier THEN
                BEGIN
                  MakeError('', ecIdentifierExpected, '');
                  exit;
                END;
              IF IsDuplic(FParser.GetToken) THEN
                BEGIN
                  MakeError('', ecDuplicateIdentifier, FParser.OriginalToken);
                  exit;
                END;
              FunctionParamNames := FParser.GetToken + '|';
              IF @FOnUseVariable <> NIL THEN
                BEGIN
                  FOnUseVariable(Self, ivtParam, ParamNo, FProcs.Count, FParser.CurrTokenPos);
                END;
              inc(ParamNo);
              FParser.Next;
              WHILE FParser.CurrTokenId = CSTI_Comma DO
                BEGIN
                  FParser.Next;
                  IF FParser.CurrTokenId <> CSTI_Identifier THEN
                    BEGIN
                      MakeError('', ecIdentifierExpected, '');
                      exit;
                    END;
                  IF IsDuplic(FParser.GetToken) THEN
                    BEGIN
                      MakeError('', ecDuplicateIdentifier, '');
                      exit;
                    END;
                  IF @FOnUseVariable <> NIL THEN
                    BEGIN
                      FOnUseVariable(Self, ivtParam, ParamNo, FProcs.Count, FParser.CurrTokenPos);
                    END;
                  inc(ParamNo);
                  FunctionParamNames := FunctionParamNames + FParser.GetToken +
                    '|';
                  FParser.Next;
                END;
              IF FParser.CurrTokenId <> CSTI_Colon THEN
                BEGIN
                  MakeError('', ecColonExpected, '');
                  exit;
                END;
              FParser.Next;
              FunctionTempType := at2ut(ReadType('', FParser));
              IF FunctionTempType = Cardinal(-1) THEN
                BEGIN
                  exit;
                END;
              WHILE Pos('|', FunctionParamNames) > 0 DO
                BEGIN
                  FunctionDecl := FunctionDecl + ' ' + modifier +
                    copy(FunctionParamNames, 1, Pos('|', FunctionParamNames) - 1)
                    + ' '
                    + inttostr(Longint(FunctionTempType));
                  Delete(FunctionParamNames, 1, Pos('|', FunctionParamNames));
                END;
              IF FParser.CurrTokenId = CSTI_CloseRound THEN
                break;
              IF FParser.CurrTokenId <> CSTI_Semicolon THEN
                BEGIN
                  MakeError('', ecSemicolonExpected, '');
                  exit;
                END;
              FParser.Next;
            END;
          FParser.Next;
        END;
    END;
  IF FunctionType = ftFunc THEN
    BEGIN
      IF FParser.CurrTokenId <> CSTI_Colon THEN
        BEGIN
          MakeError('', ecColonExpected, '');
          exit;
        END;
      FParser.Next;
      FunctionTempType := at2ut(ReadType('', FParser));
      IF FunctionTempType = Cardinal(-1) THEN
        exit;
      FunctionDecl := inttostr(Longint(FunctionTempType)) + FunctionDecl;
    END
  ELSE
    FunctionDecl := '-1' + FunctionDecl;
  IF FParser.CurrTokenId <> CSTI_Semicolon THEN
    BEGIN
      MakeError('', ecSemicolonExpected, '');
      exit;
    END;
  FParser.Next;
  IF (Func = NIL) AND (FParser.CurrTokenID = CSTII_External) THEN
    BEGIN
      FParser.Next;
      IF FParser.CurrTokenID <> CSTI_String THEN
        BEGIN
          MakeError('', ecStringExpected, '');
          exit;
        END;
      FunctionParamNames := FParser.GetToken;
      FunctionParamNames := copy(FunctionParamNames, 2, length(FunctionParamNames) - 2);
      FParser.Next;
      IF FParser.CurrTokenID <> CSTI_Semicolon THEN
        BEGIN
          MakeError('', ecSemicolonExpected, '');
          exit;
        END;
      FParser.Next;
      pp := FOnExternalProc(Self, FunctionName, FunctionDecl, FunctionParamNames);
      IF pp = NIL THEN
        BEGIN
          MakeError('', ecCustomError, '');
          exit;
        END;
      new(pp2);
      pp2^.Internal := false;
      pp2^.RP := pp;
      FProcs.Add(pp2);
      FRegProcs.Add(pp);
      Result := True;
      Exit;
    END ELSE IF (Func = NIL) AND (FParser.CurrTokenID = CSTII_Forward) THEN
    BEGIN
      FParser.Next;
      IF FParser.CurrTokenID <> CSTI_Semicolon THEN
        BEGIN
          MakeError('', ecSemicolonExpected, '');
          Exit;
        END;
      FParser.Next;
      Func := NewProc(FunctionName);
      Func^.Forwarded := True;
      Func^.DeclarePosition := EPos;
      Result := True;
      exit;
    END;
  IF (Func = NIL) THEN
    BEGIN
      Func := NewProc(FunctionName);
      Func^.Decl := FunctionDecl;
      Func^.DeclarePosition := EPos;
      FuncNo := FProcs.Count - 1;
    END ELSE BEGIN
      Func^.Forwarded := False;
    END;
  IF FParser.CurrTokenID = CSTII_Export THEN
    BEGIN
      FParser.Next;
      IF FParser.CurrTokenID <> CSTI_Semicolon THEN
        BEGIN
          MakeError('', ecSemicolonExpected, '');
          exit;
        END;
      FParser.Next;
      Func^.FExport := 1;
    END;
  WHILE FParser.CurrTokenId <> CSTII_Begin DO
    BEGIN
      IF FParser.CurrTokenId = CSTII_Var THEN
        BEGIN
          IF NOT DoVarBlock(Func) THEN
            exit;
        END ELSE IF FParser.CurrTokenId = CSTII_Label THEN
        BEGIN
          IF NOT ProcessLabel(Func) THEN
            Exit;
        END ELSE
        BEGIN
          MakeError('', ecBeginExpected, '');
          exit;
        END;
    END;
  Debug_WriteParams(FuncNo, Func);
  WriteProcVars(Func^.ProcVars);
  IF NOT ProcessSub(tProcBegin, FuncNo, Func) THEN
    BEGIN
      exit;
    END;
  CheckVars(Func);
  ProcessLabelForwards(Func);
  Result := True;
END;

FUNCTION TSCPascalCompiler.DoTypeBlock(FParser: TSCAbstractParser): Boolean;
VAR
  VName: STRING;
BEGIN
  Result := False;
  FParser.Next;
  IF FParser.CurrTokenId <> CSTI_Identifier THEN
    BEGIN
      MakeError('', ecIdentifierExpected, '');
      exit;
    END;
  REPEAT
    VName := FParser.GetToken;
    IF IsDuplicate(VName) THEN
      BEGIN
        MakeError('', ecDuplicateIdentifier, FParser.OriginalToken);
        exit;
      END;

    FParser.Next;
    IF FParser.CurrTokenId <> CSTI_Equal THEN
      BEGIN
        MakeError('', ecIsExpected, '');
        exit;
      END;
    FParser.Next;
    IF ReadType(VName, FParser) = Cardinal(-1) THEN
      BEGIN
        Exit;
      END;
    IF FParser.CurrTokenID <> CSTI_Semicolon THEN
      BEGIN
        MakeError('', ecSemicolonExpected, '');
        Exit;
      END;
    FParser.Next;
  UNTIL FParser.CurrTokenId <> CSTI_Identifier;
  Result := True;
END;


FUNCTION TSCPascalCompiler.ProcessSub(FType: TSubOptType; ProcNo: Cardinal; proc: PSCProcedure): Boolean;


  PROCEDURE Debug_WriteLine;
  VAR
    b: Boolean;
  BEGIN
    IF @FOnWriteLine <> NIL THEN BEGIN
        b := FOnWriteLine(Self, FParser.CurrTokenPos);
      END ELSE
      b := true;
    IF b THEN Debug_SavePosition(ProcNo, proc);
  END;

  PROCEDURE WriteCommand(b: Byte);
  BEGIN
    proc^.Data := proc^.Data + Char(b);
  END;

  PROCEDURE WriteByte(b: Byte);
  BEGIN
    proc^.Data := proc^.Data + Char(b);
  END;

  PROCEDURE WriteData(CONST Data; Len: Longint);
  BEGIN
    SetLength(proc^.Data, Length(proc^.Data) + Len);
    Move(Data, proc^.Data[Length(proc^.Data) - Len + 1], Len);
  END;

  FUNCTION ReadReal(CONST s: STRING): PIfRVariant;
  VAR
    C: Integer;
  BEGIN
    New(Result);
    Result^.FType := GetType(btExtended);
    SetLength(Result^.Value, SizeOf(TbtExtended));
    Val(s, TbtExtended((@Result^.Value[1])^), C);
  END;

  FUNCTION ReadString: PIfRVariant;

    FUNCTION ParseString: STRING;
    VAR
      temp3: STRING;

      FUNCTION ChrToStr(s: STRING): Char;
      BEGIN
        Delete(s, 1, 1); {First char : #}
        ChrToStr := Chr(StrToInt(s));
      END;

      FUNCTION PString(s: STRING): STRING;
      BEGIN
        s := copy(s, 2, Length(s) - 2);
        PString := s;
      END;
    BEGIN
      temp3 := '';
      WHILE (FParser.CurrTokenId = CSTI_String) OR (FParser.CurrTokenId =
        CSTI_Char) DO
        BEGIN
          IF FParser.CurrTokenId = CSTI_String THEN
            BEGIN
              temp3 := temp3 + PString(FParser.GetToken);
              FParser.Next;
              IF FParser.CurrTokenId = CSTI_String THEN
                temp3 := temp3 + #39;
            END {if}
          ELSE
            BEGIN
              temp3 := temp3 + ChrToStr(FParser.GetToken);
              FParser.Next;
            END; {else if}
        END; {while}
      ParseString := temp3;
    END;

  BEGIN
    New(Result);
    Result^.FType := GetType(btString);
    Result^.Value := ParseString;
  END;

  FUNCTION ReadInteger(CONST s: STRING): PIfRVariant;
  VAR
    C: Integer;
  {$IFNDEF NOINT64}e: Int64; {$ENDIF}
  BEGIN
{$IFNDEF NOINT64}
    e := StrToInt64Def(s, -1);
    IF (e AND $FFFFFFFF) = E THEN
      BEGIN
        New(Result);
        Result^.FType := GetType(btS32);
        SetLength(Result^.Value, SizeOf(TbtS32));
        Val(s, TbtS32((@Result^.Value[1])^), C);
        IF TbtS32((@Result^.Value[1])^) < 0 THEN
          BEGIN
            Val(s, TbtU32((@Result^.Value[1])^), C);
          END;
      END ELSE BEGIN
        New(Result);
        Result^.FType := GetType(btS64);
        SetLength(Result^.Value, SizeOf(TbtS64));
        tbts64((@Result^.Value[1])^) := e;
      END;
{$ELSE}
    New(Result);
    Result^.FType := GetType(btS32);
    SetLength(Result^.Value, SizeOf(TbtS32));
    Val(s, TbtS32((@Result^.Value[1])^), C);
    IF TbtS32((@Result^.Value[1])^) < 0 THEN
      BEGIN
        Val(s, TbtU32((@Result^.Value[1])^), C);
      END;
{$ENDIF}
  END;

  PROCEDURE WriteLong(l: Cardinal);
  BEGIN
    WriteData(l, 4);
  END;

  PROCEDURE WriteVariant(p: PIfRVariant);
  VAR
    px: PSCType;
  BEGIN
    WriteLong(p^.FType);
    px := FUsedTypes.GetItem(p^.FType);
    IF px^.BaseType = btString THEN
      BEGIN
        WriteLong(Length(p^.Value));
        WriteData(p^.Value[1], Length(p^.Value));
      END
    ELSE IF px^.BaseType = btenum THEN
      BEGIN
        IF Longint(px^.Ext) <= 256 THEN
          WriteData(p^.Value[1], 1)
        ELSE IF Longint(px^.Ext) <= 65536 THEN
          WriteData(p^.Value[1], 2)
        ELSE
          WriteData(p^.Value[1], 4);
      END ELSE
      BEGIN
        WriteData(p^.Value[1], Length(p^.Value));
      END;
  END;

  FUNCTION GetParamType(I: Longint): Cardinal;
  VAR
    u, n: STRING;
  BEGIN
    u := proc^.Decl;
    Inc(I);
    n := GRFW(u);
    IF (I = 0) AND (n <> '-1') THEN
      BEGIN
        Result := StrToIntDef(n, -1);
        exit;
      END
    ELSE IF n <> '-1' THEN
      Inc(I);
    WHILE I < 0 DO
      BEGIN
        GRFW(u);
        GRFW(u);
        Inc(I);
      END;
    GRFW(u);
    Result := StrToIntDef(GRFW(u), -1);
  END;
  FUNCTION GetRecordTypeNo(x: PSCValue): Cardinal;
  VAR
    rr: PIFRecField;
  BEGIN
    rr := x^.RecField.GetItem(x^.RecField.Count - 1);
    Result := rr^.FType;
  END;
  FUNCTION AllocStackReg(FType: Cardinal): PSCValue;
  VAR
    x: PSCProcVar;
  BEGIN
    New(x);
    x^.DeclarePosition := FParser.CurrTokenPos;
    x^.VarName := '';
    x^.NameHash := MakeHash(x^.VarName);
    x^.VarType := FType;
    proc^.ProcVars.Add(x);
    New(Result);
    Result^.FType := CVAL_AllocatedStackReg;
    Result^.DPos := FParser.CurrTokenPos;
    Result^.Address := SCAddrStackStart + proc^.ProcVars.Count;
    Result^.RecField := NIL;
    Result^.Modifiers := 0;
    WriteCommand(Cm_Pt);
    WriteLong(FType);
  END;

  FUNCTION AllocStackReg2(FType: Cardinal): PSCValue;
  VAR
    x: PSCProcVar;
  BEGIN
    New(x);
    x^.VarName := '';
    x^.NameHash := MakeHash(x^.VarName);
    x^.VarType := FType;
    proc^.ProcVars.Add(x);
    New(Result);
    Result^.FType := CVAL_AllocatedStackReg;
    Result^.RecField := NIL;
    Result^.Modifiers := 0;
    Result^.Address := SCAddrStackStart + proc^.ProcVars.Count;
  END;
  FUNCTION WriteCalculation(InData, OutReg: PSCValue): Boolean; FORWARD;

  PROCEDURE DisposeStackReg(p: PSCValue);
  BEGIN
    Dispose(
      PSCProcVar(proc^.ProcVars.GetItem(p^.Address - SCAddrStackStart - 1))
      );
    proc^.ProcVars.Delete(proc^.ProcVars.Count - 1);
    DisposePValue(p);
    WriteCommand(CM_PO);
  END;
  FUNCTION GetTypeNo(p: PSCValue): Cardinal; FORWARD;

  FUNCTION WriteOutRec(x: PSCValue; AllowData: Boolean): Boolean; FORWARD;
  PROCEDURE AfterWriteOutRec(VAR x: PSCValue); FORWARD;
  FUNCTION FindProc(CONST Name: STRING): Cardinal; FORWARD;
  FUNCTION checkCompatType2(p1, p2: PSCType): Boolean;
  BEGIN
    IF
      ((p1^.BaseType = btProcPtr) AND (p2 = p1)) OR
      (p1^.BaseType = btVariant) OR
      (p2^.baseType = btVariant) OR
      (IsIntType(p1^.BaseType) AND IsIntType(P2^.BaseType)) OR
      (IsRealType(p1^.BaseType) AND IsIntRealType(P2^.BaseType)) OR
      ((p1^.BaseType = btString) AND (P2^.BaseType = btString)) OR
      ((p1^.BaseType = btString) AND (P2^.BaseType = btChar)) OR
      ((p1^.BaseType = btArray) AND (p2^.BaseType = btArray)) OR
      ((p1^.BaseType = btChar) AND (p2^.BaseType = btChar)) OR
      ((p1^.BaseType = btRecord) AND (p2^.BaseType = btrecord)) OR
      ((p1^.BaseType = btEnum) AND (p2^.BaseType = btEnum))
      THEN
      Result := True
    ELSE IF ((P1^.BaseType = btclass) AND (p2^.Basetype = btClass)) THEN
      BEGIN
        Result := p1^.Ex.IsCompatibleWith(p2^.Ex);
      END ELSE

      Result := False;
  END;

  FUNCTION CheckCompatType(V1, v2: PSCValue): Boolean;
  VAR
    p1, P2: PSCType;
  BEGIN
    IF (v1^.Modifiers AND 4) <> 0 THEN
      BEGIN
        Result := True;
        exit;
      END;
    p1 := FUsedTypes.GetItem(GetTypeNo(V1));
    P2 := FUsedTypes.GetItem(GetTypeNo(v2));
    IF (p1^.BaseType = btChar) AND (p2^.BaseType = btString) AND (v2^.FType = CVAL_Data) AND (length(V2^.FData^.Value) = 1) THEN
      BEGIN
        v2^.FData^.FType := GetType(btChar);
        P2 := FUsedTypes.GetItem(GetTypeNo(v2));
      END;
    Result := CheckCompatType2(p1, p2);
  END;

  FUNCTION ProcessFunctionX(ResModifiers: Byte; ProcNo: Cardinal; InData: TSCList;
    ResultRegister:
    PSCValue): Boolean; FORWARD;
  FUNCTION ProcessVarFunction(ResModifiers: Byte; ProcNo: PSCValue; InData: TSCList;
    ResultRegister:
    PSCValue): Boolean; FORWARD;

  FUNCTION MakeNil(NilPos: Cardinal; ivar: PSCValue): Boolean;
  VAR
    Procno: Cardinal;
    PF: PSCType;
    Par: TSCList;
    pp: PParam;
  BEGIN
    Pf := FUsedTypes.GetItem(GetTypeNo(IVar));
    IF (pf^.BaseType <> btClass) OR (NOT pf.Ex.SetNil(GetTypeno(IVar), ProcNo)) OR ((Ivar.FType <> CVAL_Addr) AND (Ivar.FType <> CVAL_AllocatedStackReg)) THEN
      BEGIN
        MakeError('', ecTypeMismatch, '')^.Position := nilPos;
        Result := False;
        exit;
      END;
    ivar.FType := CVAL_PushAddr;
    ivar.Modifiers := ivar.modifiers OR 128;
    Par := TSCList.Create;
    new(pp);
    pp^.InReg := ivar;
    pp^.OutReg := NIL;
    pp^.FType := GetTypeNo(ivar);
    pp^.OutRegPos := NilPos;
    par.add(pp);
    Result := ProcessFunctionX(0, ProcNo, Par, NIL);
    Dispose(pp);
    Par.Free;
    ivar.Modifiers := ivar.modifiers AND NOT 128;
  END;

  FUNCTION PreWriteOutRec(VAR X: PSCValue; FArrType: Cardinal): Boolean;
  VAR
    rr: PIFRecField;
    tmpp,
      tmpc: PSCValue;
    i: Longint;
  BEGIN
    Result := True;
    IF x^.FType = CVAL_NIL THEN
      BEGIN
        IF FArrType = Cardinal(-1) THEN
          BEGIN
            MakeError('', ecTypeMismatch, '');
            Result := False;
            Exit;
          END;
        tmpp := AllocStackReg(FArrType);
        IF NOT MakeNil(x^.DPos, tmpp) THEN
          BEGIN
            Result := False;
            exit;
          END;
        tmpp^.FType := CVAL_ArrayAllocatedStackRec;
        x := tmpp;
      END ELSE
      IF x^.FType = CVAL_Array THEN
        BEGIN
          IF FArrType = Cardinal(-1) THEN
            BEGIN
              MakeError('', ecTypeMismatch, '');
              Result := False;
              Exit;
            END;
          tmpp := AllocStackReg(FArrType);
          tmpp^.FType := CVAL_ArrayAllocatedStackRec;
          tmpc := AllocStackReg(GetType(bts32));
          WriteCommand(CM_A);
          WriteOutrec(tmpc, False);
          WriteByte(1);
          WriteLong(GetType(bts32));
          WriteLong(x^.ArrayItems.Count);
          WriteCommand(CM_PV);
          WriteOutrec(tmpp, False);
          WriteCommand(CM_C);
          WriteLong(FindProc('SETARRAYLENGTH'));
          WriteCommand(CM_PO);
          DisposeStackReg(tmpc);
          new(tmpc);
          tmpc^.FType := CVAL_Addr;
          tmpc^.Modifiers := 0;
          tmpc^.DPos := tmpp^.DPos;
          tmpc^.Address := tmpp^.Address;
          tmpc^.RecField := TSCList.Create;
          new(rr);
          rr^.FKind := 1;
          rr^.FType := Cardinal(PSCType(FUsedTypes.GetItem(FArrType))^.Ext);
          tmpc^.RecField.Add(rr);
          FOR i := 0 TO x^.ArrayItems.Count - 1 DO
            BEGIN
              rr^.ArrayFieldNo := i;
              IF NOT WriteCalculation(x^.ArrayItems.GetItem(i), tmpc) THEN
                BEGIN
                  DisposePValue(tmpc);
                  DisposeStackReg(tmpp);
                  Result := False;
                  Exit;
                END;
            END;
          x := tmpp;
        END ELSE IF (x^.FType = CVAL_Eval) THEN
        BEGIN
          tmpp := AllocStackReg(x^.frestype);
          WriteCalculation(x, tmpp);
          IF x^.Modifiers = 1 THEN
            BEGIN
              WriteCommand(cm_bn);
              WriteByte(0);
              WriteLong(Tmpp^.Address);
            END ELSE
            IF x^.Modifiers = 2 THEN
              BEGIN
                WriteCommand(cm_vm);
                WriteByte(0);
                WriteLong(Tmpp^.Address);
              END;
          tmpp^.DPos := cardinal(x);
          x := tmpp;
          x^.FType := CVAL_AllocatedStackReg + 1;
        END ELSE IF (x^.FType = CVAL_Proc) OR (x^.FType = CVAL_VarProc) THEN
        BEGIN
          IF x^.FType = CVAL_VarProc THEN
            BEGIN
              tmpp := AllocStackReg(StrToIntDef(Fw(PSCProceduralType(PSCType(FUsedTypes.GetItem(GetTypeNo(x^._ProcNo)))^.Ext)^.ProcDef), -1));
            END ELSE IF PSCProcedure(FProcs.GetItem(x^.ProcNo))^.Internal THEN
            tmpp := AllocStackReg(StrToIntDef(Fw(PSCProcedure(FProcs.GetItem(x^.ProcNo))^.Decl), -1))
          ELSE
            tmpp := AllocStackReg(StrToIntDef(Fw(PSCUsedRegProc(FPRocs.GetItem(x^.ProcNo))^.RP^.Decl), -1));
          WriteCalculation(x, tmpp);
          IF x^.Modifiers = 1 THEN
            BEGIN
              WriteCommand(cm_bn);
              WriteByte(0);
              WriteLong(Tmpp^.Address);
            END ELSE
            IF x^.Modifiers = 2 THEN
              BEGIN
                WriteCommand(cm_vm);
                WriteByte(0);
                WriteLong(Tmpp^.Address);
              END;
          tmpp^.DPos := cardinal(x);
          x := tmpp;
          x^.FType := CVAL_AllocatedStackReg + 1;
        END ELSE
        IF ((x^.FType = CVAL_Addr) OR (x^.FType = CVAL_PushAddr)) AND (x^.RecField <> NIL) THEN
          BEGIN
            IF x^.RecField.Count = 1 THEN
              BEGIN
                rr := x^.RecField.GetItem(0);
                IF rr^.FKind < 2 THEN
                  exit; // there is no need pre-calculate anything
                IF rr^.ReadArrayFieldNoFrom^.FType = CVAL_Addr THEN
                  exit;
              END; //if
            tmpp := AllocStackReg(GetType(btPointer));
            WriteCommand(cm_sp);
            WriteOutRec(tmpp, True);
            WriteByte(0);
            WriteLong(x^.Address);

            FOR i := 0 TO x^.RecField.Count - 1 DO
              BEGIN
                rr := x^.RecField.GetItem(I);
                CASE rr^.FKind OF
                  0, 1:
                    BEGIN
                      WriteCommand(cm_sp);
                      WriteOutRec(tmpp, false);
                      WriteByte(2);
                      WriteLong(tmpp^.Address);
                      WriteLong(rr^.RecFieldNo);
                    END; // case 0,1
                  2:
                    BEGIN
                      tmpc := AllocStackReg(GetType(btU32));
                      IF NOT WriteCalculation(rr^.ReadArrayFieldNoFrom, tmpc) THEN
                        BEGIN
                          DisposeStackReg(tmpc);
                          DisposeStackReg(tmpp);
                          Result := False;
                          exit;
                        END; //if
                      WriteCommand(cm_sp);
                      WriteOutRec(tmpp, false);
                      WriteByte(3);
                      WriteData(tmpp^.Address, 4);
                      WriteData(tmpc^.Address, 4);
                      DisposeStackReg(tmpc);
                    END; // case 2
                END; // case
                Dispose(rr);
              END; // for
            IF x^.Modifiers = 1 THEN
              BEGIN
                WriteCommand(cm_bn);
                WriteByte(0);
                WriteLong(Tmpp^.Address);
              END ELSE
              IF x^.Modifiers = 2 THEN
                BEGIN
                  WriteCommand(cm_vm);
                  WriteByte(0);
                  WriteLong(Tmpp^.Address);
                END;
            x^.RecField.Clear;
            new(rr);
            rr^.FKind := 3;
            rr^.ResultRec := tmpp;
            x^.RecField.Add(rr);
          END ELSE IF (x^.Modifiers AND 3) <> 0 THEN
          BEGIN
            IF x^.FType = CVAL_Addr THEN
              BEGIN
                tmpp := AllocStackReg(GetTypeNo(x));
                tmpp^.FType := CVAL_AllocatedStackReg + 1;
                WriteCommand(CM_A);
                WriteByte(0);
                WriteLong(tmpp^.Address);
                WriteByte(0);
                WriteLong(x^.Address);
                IF x^.Modifiers = 1 THEN
                  BEGIN
                    WriteCommand(cm_bn);
                    WriteByte(0);
                    WriteLong(Tmpp^.Address);
                  END ELSE
                  IF x^.Modifiers = 2 THEN
                    BEGIN
                      WriteCommand(cm_vm);
                      WriteByte(0);
                      WriteLong(Tmpp^.Address);
                    END;
                tmpp^.DPos := cardinal(x);
                x := tmpp;
              END ELSE IF x^.FType = CVAL_PushAddr THEN
              BEGIN
                IF x^.Modifiers = 1 THEN
                  BEGIN
                    WriteCommand(cm_bn);
                    WriteByte(0);
                    WriteLong(x^.Address);
                  END ELSE
                  IF x^.Modifiers = 2 THEN
                    BEGIN
                      WriteCommand(cm_vm);
                      WriteByte(0);
                      WriteLong(x^.Address);
                    END;
              END;
          END;
  END;

  PROCEDURE AfterWriteOutRec(VAR x: PSCValue);
  VAR
    rr: PIFRecField;
    p: Pointer;
  BEGIN
    IF x^.FType = CVAL_ArrayAllocatedStackRec THEN
      BEGIN
        DisposeStackReg(x);
      END ELSE
      IF x^.FType = CVAL_AllocatedStackReg + 1 THEN
        BEGIN
          p := Pointer(x^.DPos);
          DisposeStackReg(x);
          x := p;
        END ELSE IF ((x^.FType = CVAL_Addr) OR (x^.FType = CVAL_PushAddr)) AND (x^.RecField <> NIL) THEN
        BEGIN
          rr := x^.RecField.GetItem(0);
          IF (rr^.FKind = 3) THEN
            BEGIN
              DisposeStackReg(rr^.ResultRec);
              Dispose(Rr);
              x^.RecField.Free;
              x^.RecField := NIL;
            END;
        END;
  END; //afterwriteoutrec

  FUNCTION WriteOutRec(x: PSCValue; AllowData: Boolean): Boolean;
  VAR
    rr: PIFRecField;
  BEGIN
    Result := True;
    CASE x^.FType OF
      CVAL_ArrayAllocatedStackRec, CVAL_Addr, CVAL_PushAddr, CVAL_AllocatedStackReg, CVAL_AllocatedStackReg + 1:
        BEGIN
          IF x^.RecField = NIL THEN
            BEGIN
              WriteByte(0);
              WriteData(x^.Address, 4);
            END
          ELSE
            BEGIN
              rr := x^.RecField.GetItem(0);
              CASE rr^.FKind OF
                0, 1:
                  BEGIN
                    WriteByte(2);
                    WriteLong(x^.Address);
                    WriteLong(rr^.RecFieldNo);
                  END;
                2:
                  BEGIN
                    WriteByte(3);
                    WriteLong(x^.Address);
                    WriteLong(rr^.ReadArrayFieldNoFrom^.Address);
                  END;
                3:
                  BEGIN
                    WriteByte(0);
                    WriteLong(rr^.ResultRec^.Address);
                  END;
              END;
            END;
        END;
      CVAL_Data:
        IF AllowData THEN
          BEGIN
            WriteByte(1);
            WriteVariant(x^.FData)
          END
        ELSE
          BEGIN
            Result := False;
            exit;
          END;
    ELSE
      Result := False;
    END;
  END;

  FUNCTION GetTypeNo(p: PSCValue): Cardinal;
  VAR
    n: PSCProcedure;
  BEGIN
    IF (p^.Modifiers AND 8) <> 0 THEN
      BEGIN
        Result := p^.FNewTypeNo;
        exit;
      END;
    IF (p^.RecField <> NIL) AND (p^.FType = CVAL_Addr) THEN
      BEGIN
        Result := GetRecordTypeNo(p);
      END
    ELSE
      CASE p^.FType OF
        CVAL_Cast:
          BEGIN
            Result := p^.NewTypeNo;
          END;
        CVAL_Array:
          BEGIN
            Result := at2ut(FindType('TVariantArray'));
          END;
        CVAL_ArrayAllocatedStackRec, CVAL_Addr, CVAL_AllocatedStackReg, CVAL_PushAddr:
          BEGIN
            IF p^.Address < SCAddrNegativeStackStart THEN
              BEGIN
                IF p^.Address < FVars.Count THEN
                  BEGIN
                    Result := PSCVar(FVars.GetItem(p^.Address))^.FType;
                  END
                ELSE
                  Result := Cardinal(-1);
              END
            ELSE
              BEGIN
                IF p^.Address < SCAddrStackStart THEN
                  BEGIN
                    Result := GetParamType(p^.Address - SCAddrStackStart);
                  END
                ELSE
                  Result := PSCProcVar(proc^.ProcVars.GetItem(p^.Address - 1 -
                    SCAddrStackStart))^.VarType;
              END;
          END;
        CVAL_Data: Result := p^.FData^.FType;
        CVAL_VarProc:
          BEGIN
            Result := StrToIntDef(Fw(PSCProceduralType(PSCType(FUsedTypes.GetItem(GetTypeNo(p^._ProcNo)))^.Ext)^.ProcDef), -1);
          END;
        CVAL_Proc:
          BEGIN
            n := PSCProcedure(FProcs.GetItem(p^.ProcNo));

            IF n^.Internal THEN
              Result := StrToIntDef(Fw(n^.Decl), -1)
            ELSE
              Result := StrToIntDef(Fw(PSCUsedRegProc(n)^.RP^.Decl), -1);
          END;
        CVAL_Eval: Result := p^.frestype;
      ELSE
        Result := Cardinal(-1);
      END;
  END;

  FUNCTION ReadParameters(ProcNo: Cardinal; FSelf: PSCValue): PSCValue; FORWARD;

  FUNCTION FindProc(CONST Name: STRING): Cardinal;
  VAR
    l, h: Longint;
    x: PSCUsedRegProc;

  BEGIN
    h := MakeHash(Name);
    FOR l := 0 TO FProcs.Count - 1 DO
      BEGIN
        IF PSCProcedure(FProcs.GetItem(l))^.Internal THEN
          BEGIN
            IF (PSCProcedure(FProcs.GetItem(l))^.NameHash = h) AND
              (PSCProcedure(FProcs.GetItem(l))^.Name = Name) THEN
              BEGIN
                Result := l;
                exit;
              END;
          END
        ELSE
          BEGIN
            IF (PSCUsedRegProc(FProcs.GetItem(l))^.RP^.NameHash = h) AND
              (PSCUsedRegProc(FProcs.GetItem(l))^.RP^.Name = Name) THEN
              BEGIN
                Result := l;
                exit;
              END;
          END;
      END;
    FOR l := 0 TO FRegProcs.Count - 1 DO
      BEGIN
        IF (PSCRegProc(FRegProcs.GetItem(l))^.NameHash = h) AND
          (PSCRegProc(FRegProcs.GetItem(l))^.Name = Name) THEN
          BEGIN
            New(x);
            x^.Internal := False;
            x^.RP := FRegProcs.GetItem(l);
            ReplaceTypes(x^.Rp^.Decl);
            FProcs.Add(x);
            Result := FProcs.Count - 1;
            exit;
          END;
      END;
    Result := Cardinal(-1);
  END; {findfunc}

  FUNCTION calc(endOn: TSCToken): PSCValue; FORWARD;

  FUNCTION ReadVarParameters(ProcNoVar: PSCValue): PSCValue; FORWARD;

  FUNCTION GetIdentifier(CONST FType: Byte): PSCValue;
    {
      FType:
        0 = Anything
        1 = Only variables
        2 = Not constants
    }
  VAR
    Temp: PSCValue;
    l, h: Longint;
    s, u: STRING;
    t: PSCConstant;
    Temp1: Cardinal;

    PROCEDURE CheckProcCall(VAR x: PSCValue);
    BEGIN
      IF FParser.CurrTokenId = CSTI_Dereference THEN
        BEGIN
          IF PSCType(FUsedTypes.GetItem(GetTypeNo(x)))^.BaseType <> btProcPtr THEN
            BEGIN
              MakeError('', ecTypeMismatch, '');
              DisposePValue(x);
              x := NIL;
              Exit;
            END;
          FParser.Next;
          x := ReadVarParameters(x);
        END;
    END;

    PROCEDURE CheckFurther(VAR x: PSCValue);
    VAR
      t: Cardinal;
      rr: PIFRecField;
      LastRecType, I, LL: Longint;
      u: PSCType;
      Param: PParam;
      NewRecFields: TSCList;
      tmp, tmp3: PSCValue;
      tmp2: Boolean;

      FUNCTION FindSubR(CONST n: STRING; FType: PSCType): Cardinal;
      VAR
        h, I: Longint;
      BEGIN
        h := MakeHash(n);
        FOR I := 0 TO FType.RecordSubVals.Count - 1 DO
          BEGIN
            IF
              (PSCRecordType(FType.RecordSubVals.GetItem(I))^.FieldNameHash =
              h) AND
              (PSCRecordType(FType.RecordSubVals.GetItem(I))^.FieldName = n)
              THEN
              BEGIN
                Result := I;
                exit;
              END;
          END;
        Result := Cardinal(-1);
      END;

    BEGIN
      IF (x^.FType <> CVAL_Addr) AND (x^.FType <> CVAL_PushAddr) AND (x^.FType <> CVAL_AllocatedStackReg) THEN
        Exit;
      x.RecField := NIL;
      t := GetTypeNo(x);
      u := FUsedTypes.GetItem(t);
      IF u^.BaseType = btClass THEN exit;
      WHILE True DO
        BEGIN
          IF FParser.CurrTokenId = CSTI_OpenBlock THEN
            BEGIN
              IF (u^.BaseType = btString) AND (x^.FType = CVAL_Addr) THEN
                BEGIN
                  x^.FType := CVAL_PushAddr;
                  FParser.Next;
                  tmp := Calc(CSTI_CloseBlock);
                  IF tmp = NIL THEN
                    BEGIN
                      DisposePValue(x);
                      x := NIL;
                      exit;
                    END;
                  IF NOT IsIntType(PSCType(FUSedTypes.GetItem(GetTypeNo(tmp)))^.BaseType) THEN
                    BEGIN
                      MakeError('', ecTypeMismatch, '');
                      DisposePValue(tmp);
                      DisposePValue(x);
                      x := NIL;
                      exit;
                    END;
                  FParser.Next;
                  IF FParser.CurrTokenId = CSTI_Assignment THEN
                    BEGIN
                      l := FindProc('STRSET');
                      IF l = -1 THEN
                        BEGIN
                          MakeError('', ecUnknownIdentifier, 'StrGet');
                          DisposePValue(tmp);
                          DisposePValue(x);
                          x := NIL;
                          exit;
                        END;
                      New(tmp3);
                      tmp3^.FType := CVAL_Proc;
                      tmp3^.Modifiers := 0;
                      tmp3^.DPos := FParser.CurrTokenPos;
                      tmp3^.ProcNo := L;
                      tmp3^.Parameters := TSCList.Create;
                      new(Param);
                      tmp3^.Parameters.Add(Param);
                      new(Param);
                      param^.InReg := tmp;
                      Param^.FType := GetTypeNo(tmp);
                      Param^.OutReg := NIL;
                      Param^.OutRegPos := tmp^.DPos;
                      tmp3^.Parameters.Add(Param);
                      new(Param);
                      param^.InReg := x;
                      Param^.FType := GetTypeNo(x);
                      Param^.OutReg := NIL;
                      Param^.OutRegPos := tmp^.DPos;
                      tmp3^.Parameters.Add(Param);
                      Param := tmp3^.Parameters.GetItem(0);
                      x := tmp3;
                      FParser.Next;
                      tmp := Calc(CSTI_SemiColon);
                      IF (Tmp^.FType = CVAL_DATA) AND (PSCType(FUSedTypes.GetItem(Tmp^.FData^.FType))^.BaseType = btString) THEN
                        BEGIN
                          IF Length(Tmp^.FData^.Value) <> 1 THEN
                            BEGIN
                              MakeError('', ecTypeMismatch, '');
                              DisposePValue(Tmp);
                              x^.Parameters.Delete(0);
                              DisposePValue(x);
                              x := NIL;
                              exit;
                            END;
                          Tmp^.FData^.FType := GetType(btChar);
                        END;
                      IF PSCType(FUSedTypes.GetItem(Tmp^.FData^.FType))^.BaseType <> btChar THEN
                        BEGIN
                          MakeError('', ecTypeMismatch, '');
                          DisposePValue(Tmp);
                          x^.Parameters.Delete(0);
                          DisposePValue(x);
                          x := NIL;
                          exit;
                        END;
                      IF tmp = NIL THEN
                        BEGIN
                          x^.Parameters.Delete(0);
                          DisposePValue(x);
                          x := NIL;
                          exit;
                        END;
                      Param^.InReg := tmp;
                      Param^.OutReg := NIL;
                      param^.FType := GetTypeNo(tmp);
                      Param^.OutRegPos := tmp^.DPos;
                    END ELSE BEGIN
                      l := FindProc('STRGET');
                      IF l = -1 THEN
                        BEGIN
                          MakeError('', ecUnknownIdentifier, 'StrGet');
                          DisposePValue(tmp);
                          DisposePValue(x);
                          x := NIL;
                          exit;
                        END;
                      New(tmp3);
                      tmp3^.FType := CVAL_Proc;
                      tmp3^.Modifiers := 0;
                      tmp3^.DPos := FParser.CurrTokenPos;
                      tmp3^.ProcNo := L;
                      tmp3^.Parameters := TSCList.Create;
                      new(Param);
                      param^.InReg := x;
                      Param^.FType := GetTypeNo(x);
                      Param^.OutReg := NIL;
                      Param^.OutRegPos := tmp^.DPos;
                      tmp3^.Parameters.Add(Param);
                      new(Param);
                      param^.InReg := tmp;
                      Param^.FType := GetTypeNo(tmp);
                      Param^.OutReg := NIL;
                      Param^.OutRegPos := tmp^.DPos;
                      tmp3^.Parameters.Add(Param);
                      x := tmp3;
                    END;
                  Break;
                END ELSE IF u^.BaseType = btArray THEN
                BEGIN
                  FParser.Next;
                  tmp := calc(CSTI_CloseBlock);
                  IF tmp = NIL THEN
                    BEGIN
                      DisposePValue(x);
                      x := NIL;
                      exit;
                    END;
                  IF NOT IsIntType(PSCType(FUSedTypes.GetItem(GetTypeNo(tmp)))^.BaseType) THEN
                    BEGIN
                      MakeError('', ecTypeMismatch, '');
                      DisposePValue(tmp);
                      DisposePValue(x);
                      x := NIL;
                      exit;
                    END;
                  IF tmp^.FType = CVAL_Data THEN
                    BEGIN
                      IF x.RecField = NIL THEN
                        x.RecField := TSCList.Create;
                      new(rr);
                      rr^.FKind := 1;
                      rr^.ArrayFieldNo := GetUInt(FUsedTypes, tmp^.FData, tmp2);
                      rr^.FType := Cardinal(u^.Ext);
                      u := FUsedTypes.GetItem(Cardinal(u^.Ext));
                      x^.RecField.Add(rr);
                    END
                  ELSE
                    BEGIN
                      IF x.RecField = NIL THEN
                        x.RecField := TSCList.Create;
                      new(rr);
                      rr^.FKind := 2;
                      rr^.ReadArrayFieldNoFrom := tmp;
                      rr^.FType := Cardinal(u^.Ext);
                      u := FUsedTypes.GetItem(Cardinal(u^.Ext));
                      x^.RecField.Add(rr);
                    END;
                  IF FParser.CurrTokenId <> CSTI_CloseBlock THEN
                    BEGIN
                      MakeError('', ecCloseBlockExpected, '');
                      DisposePValue(x);
                      x := NIL;
                      exit;
                    END;
                  Fparser.Next;
                END ELSE BEGIN
                  MakeError('', ecSemicolonExpected, '');
                  DisposePValue(x);
                  x := NIL;
                  exit;
                END;
            END
          ELSE IF FParser.CurrTokenId = CSTI_Period THEN
            BEGIN
              FParser.Next;
              IF u^.BaseType = btRecord THEN
                BEGIN
                  t := FindSubR(FParser.GetToken, u);
                  IF t = Cardinal(-1) THEN
                    BEGIN
                      MakeError('', ecUnknownIdentifier, '');
                      DisposePValue(x);
                      x := NIL;
                      exit;
                    END;
                  FParser.Next;
                  IF x.RecField = NIL THEN
                    x.RecField := TSCList.Create;
                  new(rr);
                  rr^.FKind := 0;
                  rr^.FType := PSCRecordType(u^.RecordSubVals.GetItem(t))^.FType;
                  rr^.RecFieldNo := t;
                  u := FUsedTypes.GetItem(rr^.FType);
                  x^.RecField.Add(rr);
                END
              ELSE
                BEGIN
                  DisposePValue(x);
                  MakeError('', ecSemicolonExpected, '');
                  x := NIL;
                  exit;
                END;
            END
          ELSE
            break;
        END;
      IF x^.RecField = NIL THEN
        exit;
      LL := -1;
      NewRecFields := TSCList.Create;
      IF x^.FType = 0 THEN
        BEGIN
          IF x^.Address < SCAddrNegativeStackStart THEN
            LastRecType := PSCVar(FVars.GetItem(x^.Address))^.FType
          ELSE IF x^.Address < SCAddrStackStart THEN
            BEGIN
              LastRecType := GetParamType(Longint(x^.Address - SCAddrStackStart));
            END
          ELSE
            LastRecType := PSCProcVar(proc^.ProcVars.GetItem(x^.Address - 1 - SCAddrStackStart))^.VarType;
          i := 0;
          u := FUsedTypes.GetItem(LastRecType);

          WHILE i < Longint(x^.RecField.Count) DO
            BEGIN
              rr := x^.RecField.GetItem(I);
              CASE rr^.FKind OF
                0:
                  BEGIN
                    IF LL = -1 THEN
                      inc(ll);
                    LastRecType := rr^.FType;
                    LL := LL + Longint(PSCRecordType(u^.RecordSubVals.GetItem(rr^.RecFieldNo))^.RealFieldOffset);
                    u := FUsedTypes.GetItem(LastRecType);
                    dispose(rr);
                  END;
                1:
                  BEGIN
                    IF LL <> -1 THEN
                      BEGIN
                        new(rr);
                        rr^.FKind := 0;
                        rr^.RecFieldNo := LL;
                        rr^.FType := LastRecType;
                        newRecFields.Add(Rr);
                        rr := x^.RecField.GetItem(I);
                      END;
                    u := FUsedTypes.GetItem(rr^.FType);
                    newRecFields.Add(rr);
                    LL := -1;
                  END;
                2:
                  BEGIN
                    IF LL <> -1 THEN
                      BEGIN
                        new(rr);
                        rr^.FKind := 0;
                        rr^.FType := LastRecType;
                        rr^.RecFieldNo := LL;
                        newRecFields.Add(Rr);
                        rr := x^.RecField.GetItem(I);
                      END;
                    u := FUsedTypes.GetItem(rr^.FType);
                    newRecFields.Add(rr);
                    LL := -1;
                  END;

              END;
              inc(i);
            END;
          IF LL <> -1 THEN
            BEGIN
              new(rr);
              rr^.FKind := 0;
              rr^.RecFieldNo := LL;
              rr^.FType := LastRecType;
              newRecFields.Add(Rr);
            END;
          x^.RecField.Free;
          x^.RecField := NewRecFields;
        END;
    END;
    FUNCTION ReadPropertyParameters(Params: TSCList; ParamTypes: STRING): Boolean;
    VAR
      CurrParamType: Cardinal;
      Temp: PSCValue;
      P: PParam;
    BEGIN
      Delete(ParamTypes, 1, pos(' ', ParamTypes)); // Remove property type
      IF FParser.CurrTokenID <> CSTI_OpenBlock THEN
        BEGIN
          MakeError('', ecOpenBlockExpected, '');
          Result := False;
          exit;
        END;
      FParser.Next;
      WHILE ParamTypes <> '' DO
        BEGIN
          CurrParamType := at2ut(StrToIntDef(GRFW(ParamTypes), -1));
          Temp := Calc(CSTI_CloseBlock);
          IF temp = NIL THEN
            BEGIN
              Result := False;
              Exit;
            END;
          New(P);
          p^.InReg := Temp;
          p^.OutReg := NIL;
          p^.FType := CurrParamType;
          p^.OutRegPos := FParser.CurrTokenPos;
          Params.Add(p);
          IF ParamTypes = '' THEN
            BEGIN
              IF FParser.CurrTokenID <> CSTI_CloseBlock THEN
                BEGIN
                  MakeError('', ecCloseBlockExpected, '');
                  Result := False;
                  Exit;
                END;
              FParser.Next;
            END ELSE BEGIN
              IF FParser.CurrTokenId <> CSTI_Comma THEN
                BEGIN
                  MakeError('', ecCommaExpected, '');
                  Result := False;
                  exit;
                END;
              FParser.Next;
            END;
        END;
      Result := True;
    END;
    PROCEDURE CheckClass(VAR P: PSCValue);
    VAR
      Idx, FTypeNo: Cardinal;
      FType: PSCType;
      TempP: PSCValue;
      Param: PParam;
      s: STRING;

    BEGIN
      FTypeNo := GetTypeNo(p);
      IF FTypeNo = Cardinal(-1) THEN Exit;
      FType := FUsedTypes.GetItem(FTypeNo);
      IF FType.BaseType <> btClass THEN Exit;
      WHILE FParser.CurrTokenID = CSTI_Period DO
        BEGIN
          FParser.Next;
          IF FParser.CurrTokenID <> CSTI_Identifier THEN
            BEGIN
              MakeError('', ecIdentifierExpected, '');
              DisposePValue(p);
              P := NIL;
              Exit;
            END;
          s := FParser.GetToken;
          FParser.Next;
          IF FType.Ex.Func_Find(s, Idx) THEN
            BEGIN
              FType.Ex.Func_Call(Idx, FTypeNo);
              P := ReadParameters(FTypeNo, P);
              IF p = NIL THEN
                BEGIN
                  Exit;
                END;
            END ELSE IF FType.Ex.Property_Find(s, Idx) THEN
            BEGIN
              FType.Ex.Property_GetHeader(Idx, s);
              TempP := P;
              New(P);
              P^.FType := CVAL_Proc;
              p^.Modifiers := 0;
              p^.DPos := FParser.CurrTokenPos;
              P^.Parameters := TSCList.Create;
              new(param);
              Param^.InReg := TempP;
              Param^.OutReg := NIL;
              Param^.FType := GetTypeNo(TempP);
              P^.Parameters.Add(Param);
              IF pos(' ', s) <> 0 THEN
                BEGIN
                  IF NOT ReadPropertyParameters(P^.Parameters, s) THEN
                    BEGIN
                      DisposePValue(P);
                      P := NIL;
                      exit;
                    END;
                END; // if
              IF FParser.CurrTokenId = CSTI_Assignment THEN
                BEGIN
                  FParser.Next;
                  TempP := Calc(CSTI_SemiColon);
                  IF TempP = NIL THEN
                    BEGIN
                      DisposePValue(P);
                      p := NIL;
                      exit;
                    END;
                  new(param);
                  Param^.InReg := tempp;
                  Param^.OutReg := NIL;
                  Param^.FType := at2ut(StrToIntDef(fw(s), -1));
                  P^.Parameters.Add(Param);
                  IF NOT FType.Ex.Property_Set(Idx, p^.ProcNo) THEN
                    BEGIN
                      MakeError('', ecReadOnlyProperty, '');
                      DisposePValue(p);
                      p := NIL;
                      exit;
                    END;
                  Exit;
                END ELSE BEGIN
                  IF NOT FType.Ex.Property_Get(Idx, p^.ProcNo) THEN
                    BEGIN
                      MakeError('', ecWriteOnlyProperty, '');
                      DisposePValue(p);
                      p := NIL;
                      exit;
                    END;
                END; // if FParser.CurrTokenId = CSTI_Assign
            END ELSE
            BEGIN
              MakeError('', ecUnknownIdentifier, s);
              DisposePValue(p);
              P := NIL;
              Exit;
            END;
          FTypeNo := GetTypeNo(p);
          FType := FUsedTypes.GetItem(FTypeNo);
          IF (FType = NIL) OR (FType.BaseType <> btClass) THEN Exit;
        END; {while}
    END;
    FUNCTION CheckClassType(CONST TypeNo, ParserPos: Cardinal): PSCValue;
    VAR
      FType, FType2: PSCType;
      ProcNo, Idx: Cardinal;
      PP: PParam;
      Temp: PSCValue;
    BEGIN
      FType := FAvailableTypes.GetItem(TypeNo);
      IF FParser.CurrTokenID = CSTI_OpenRound THEN
        BEGIN
          FParser.Next;
          Temp := Calc(CSTI_CloseRound);
          IF Temp = NIL THEN
            BEGIN
              Result := NIL;
              exit;
            END;
          IF FParser.CurrTokenID <> CSTI_CloseRound THEN
            BEGIN
              DisposePValue(temp);
              MakeError('', ecCloseRoundExpected, '');
              Result := NIL;
              exit;
            END;
          FType2 := FUsedTypes.GetItem(GetTypeNo(Temp));
          IF (FType^.basetype = BtClass) AND (ftype2^.BaseType = btClass) AND (ftype <> ftype2) THEN
            BEGIN
              IF NOT FType2^.Ex.CastToType(GetTypeNo(Temp), AT2UT(TypeNo), ProcNo) THEN
                BEGIN
                  DisposePValue(Temp);
                  MakeError('', ecTypeMismatch, '');
                  Result := NIL;
                  exit;
                END;
              New(Result);
              Result^.FType := CVAL_Proc;
              Result^.Modifiers := 8;
              Result^.FNewTypeNo := at2ut(TypeNo);
              Result^.DPos := FParser.CurrTokenPos;
              Result^.Parameters := TSCList.Create;
              Result^.ProcNo := ProcNo;
              New(pp);
              pp^.InReg := Temp;
              pp^.OutReg := NIL;
              pp^.FType := GetTypeNo(Temp);
              Result^.Parameters.Add(pp);
              New(pp);
              pp^.OutReg := NIL;
              pp^.FType := GetType(btu32);
              New(pp^.InReg);
              pp^.InReg^.FType := CVAL_Data;
              pp^.InReg^.Modifiers := 0;
              pp^.InReg^.DPos := FParser.CurrTokenPos;
              New(pp^.InReg^.FData);
              pp^.InReg^.FData^.FType := pp^.FType;
              pp^.InReg^.FData^.Value := mi2s(at2ut(TypeNo));
              Result^.Parameters.Add(pp);
              FParser.Next;
              Exit;
            END;
          IF NOT checkCompatType2(FType, FType2) THEN
            BEGIN
              DisposePValue(Temp);
              MakeError('', ecTypeMismatch, '');
              Result := NIL;
              exit;
            END;
          FParser.Next;
          New(Result);
          Result^.FType := CVAL_Cast;
          Result^.Modifiers := 0;
          Result^.DPos := FParser.CurrTokenPos;
          Result^.Input := Temp;
          Result^.NewTypeNo := AT2UT(TypeNo);
          exit;
        END;
      IF FParser.CurrTokenId <> CSTI_Period THEN
        BEGIN
          Result := NIL;
          MakeError('', ecPeriodExpected, '');
          Exit;
        END;
      IF FType^.BaseType <> btClass THEN
        BEGIN
          Result := NIL;
          MakeError('', ecClassTypeExpected, '');
          Exit;
        END;
      FParser.Next;
      IF NOT FType^.Ex.ClassFunc_Find(FParser.GetToken, Idx) THEN
        BEGIN
          Result := NIL;
          MakeError('', ecUnknownIdentifier, FParser.OriginalToken);
          Exit;
        END;
      FParser.Next;
      FType^.Ex.ClassFunc_Call(Idx, ProcNo);
      New(Temp);
      Temp^.FType := CVAL_Data;
      Temp^.Modifiers := 0;
      New(Temp^.FData);
      Temp^.FData^.FType := GetType(btU32);
      SetLength(Temp^.FData^.Value, 4);
      Cardinal((@Temp^.FData^.Value[1])^) := AT2UT(TypeNo);
      Result := ReadParameters(ProcNo, Temp);
      IF Result <> NIL THEN
        BEGIN
          Result^.Modifiers := Result^.Modifiers OR 8;
          Result^.FNewTypeNo := AT2UT(TypeNo);
        END;
    END;

  BEGIN
    s := FParser.GetToken;
    h := MakeHash(s);
    u := proc.Decl;
    IF s = 'RESULT' THEN
      BEGIN
        IF GRFW(u) = '-1' THEN
          BEGIN
            Result := NIL;
            MakeError('', ecUnknownIdentifier, FParser.OriginalToken);
          END
        ELSE
          BEGIN
            proc^.ResUsed := True;
            New(Result);
            Result^.FType := CVAL_Addr;
            Result^.Modifiers := 0;
            Result^.Address := SCAddrStackStart - 1;
            Result^.DPos := FParser.CurrTokenPos;
            Result^.RecField := NIL;
            IF @FOnUseVariable <> NIL THEN
              FOnUseVariable(Self, ivtParam, 0, ProcNo, FParser.CurrTokenPos);
            FParser.Next;
            REPEAT
              Temp := Result;
              IF Result <> NIL THEN CheckFurther(Result);
              IF Result <> NIL THEN CheckClass(Result);
              IF Result <> NIL THEN CheckProcCall(Result);
            UNTIL (Result = NIL) OR (Temp = Result);
          END;
        exit;
      END;
    IF GRFW(u) <> '-1' THEN
      l := -2
    ELSE
      l := -1;
    WHILE Length(u) > 0 DO
      BEGIN
        IF D1(GRFW(u)) = s THEN
          BEGIN
            New(Result);
            Result^.FType := CVAL_Addr;
            Result^.Modifiers := 0;
            Result^.Address := SCAddrStackStart + Cardinal(l);
            Result^.RecField := NIL;
            IF @FOnUseVariable <> NIL THEN
              FOnUseVariable(Self, ivtParam, -1 - L, ProcNo, FParser.CurrTokenPos);
            FParser.Next;
            Result^.DPos := FParser.CurrTokenPos;
            REPEAT
              Temp := Result;
              IF Result <> NIL THEN CheckFurther(Result);
              IF Result <> NIL THEN CheckClass(Result);
              IF Result <> NIL THEN CheckProcCall(Result);
            UNTIL (Result = NIL) OR (Temp = Result);
            exit;
          END;
        Dec(l);
        GRFW(u);
      END;

    FOR l := 0 TO proc^.ProcVars.Count - 1 DO
      BEGIN
        IF (PSCProcVar(proc^.ProcVars.GetItem(l))^.NameHash = h) AND
          (PSCProcVar(proc^.ProcVars.GetItem(l))^.VarName = s) THEN
          BEGIN
            PSCProcVar(proc^.ProcVars.GetItem(l))^.Used := True;
            IF @FOnUseVariable <> NIL THEN
              FOnUseVariable(Self, ivtVariable, L, ProcNo, FParser.CurrTokenPos);
            New(Result);
            Result^.FType := CVAL_Addr;
            Result^.Modifiers := 0;
            Result^.Address := SCAddrStackStart + Cardinal(l) + 1;
            Result^.DPos := FParser.CurrTokenPos;
            Result^.RecField := NIL;

            FParser.Next;
            REPEAT
              Temp := Result;
              IF Result <> NIL THEN CheckFurther(Result);
              IF Result <> NIL THEN CheckClass(Result);
              IF Result <> NIL THEN CheckProcCall(Result);
            UNTIL (Result = NIL) OR (Temp = Result);

            exit;
          END;
      END;

    FOR l := 0 TO FVars.Count - 1 DO
      BEGIN
        IF (PSCVar(FVars.GetItem(l))^.NameHash = h) AND
          (PSCVar(FVars.GetItem(l))^.Name = s) THEN
          BEGIN
            PSCVar(FVars.GetItem(l))^.Used := True;
            New(Result);
            Result^.FType := CVAL_Addr;
            Result^.Modifiers := 0;
            Result^.Address := l;
            Result^.RecField := NIL;
            Result^.DPos := FParser.CurrTokenPos;
            IF @FOnUseVariable <> NIL THEN
              FOnUseVariable(Self, ivtGlobal, l, ProcNo, FParser.CurrTokenPos);
            FParser.Next;
            REPEAT
              Temp := Result;
              IF Result <> NIL THEN CheckFurther(Result);
              IF Result <> NIL THEN CheckClass(Result);
              IF Result <> NIL THEN CheckProcCall(Result);
            UNTIL (Result = NIL) OR (Temp = Result);
            exit;
          END;
      END;
    Temp1 := FindType(FParser.GetToken);
    IF Temp1 <> Cardinal(-1) THEN
      BEGIN
        l := FParser.CurrTokenPos;
        IF FType = 1 THEN
          BEGIN
            Result := NIL;
            MakeError('', ecVariableExpected, FParser.OriginalToken);
            exit;
          END;
        FParser.Next;
        Result := CheckClassType(Temp1, l);
        REPEAT
          Temp := Result;
          IF Result <> NIL THEN CheckFurther(Result);
          IF Result <> NIL THEN CheckClass(Result);
          IF Result <> NIL THEN CheckProcCall(Result);
        UNTIL (Result = NIL) OR (Temp = Result);

        exit;
      END;
    Temp1 := FindProc(FParser.GetToken);
    IF Temp1 <> Cardinal(-1) THEN
      BEGIN
        l := FParser.CurrTokenPos;
        IF FType = 1 THEN
          BEGIN
            Result := NIL;
            MakeError('', ecVariableExpected, FParser.OriginalToken);
            exit;
          END;
        FParser.Next;
        Result := ReadParameters(Temp1, NIL);
        IF Result = NIL THEN
          exit;
        Result^.DPos := l;
        REPEAT
          Temp := Result;
          IF Result <> NIL THEN CheckFurther(Result);
          IF Result <> NIL THEN CheckClass(Result);
          IF Result <> NIL THEN CheckProcCall(Result);
        UNTIL (Result = NIL) OR (Temp = Result);
        exit;
      END;
    FOR l := 0 TO FConstants.Count - 1 DO
      BEGIN
        t := PSCConstant(FConstants.GetItem(l));
        IF (t^.NameHash = h) AND (t^.Name = s) THEN
          BEGIN
            IF FType <> 0 THEN
              BEGIN
                Result := NIL;
                MakeError('', ecVariableExpected, FParser.OriginalToken);
                exit;
              END;
            fparser.next;
            new(result);
            Result^.FType := CVAL_Data;
            Result^.DPos := FParser.CurrTokenPos;
            Result^.Modifiers := 0;
            new(Result^.FData);
            Result^.FData^.FType := at2ut(t^.Value.FType);
            Result^.FData^.Value := t^.Value.Value;
            exit;
          END;
      END;
    Result := NIL;
    MakeError('', ecUnknownIdentifier, FParser.OriginalToken);
  END;
  FUNCTION ReadVarParameters(ProcNoVar: PSCValue): PSCValue;
  VAR
    Decl: STRING;
    p: PParam;
    Tmp: PSCValue;
    FType: Cardinal;
    modifier: Char;

    FUNCTION IsVarInCompatible(ft1, ft2: PSCType): Boolean;
    BEGIN
      ft1 := GetTypeCopyLink(ft1);
      ft2 := GetTypeCopyLink(ft2);
      Result := (ft1 <> ft2);
    END;

    FUNCTION getfc(CONST s: STRING): Char;
    BEGIN
      IF Length(s) > 0 THEN
        Result := s[1]
      ELSE
        Result := #0
    END;
  BEGIN
    Decl := PSCProceduralType(PSCType(FUsedTypes.GetItem(GetTypeNo(ProcnoVar)))^.Ext)^.ProcDef;
    GRFW(Decl);
    New(Result);
    Result^.FType := CVAL_VarProc;
    Result^.Modifiers := 0;
    Result^._ProcNo := ProcNoVar;
    Result^._Parameters := TSCList.Create;
    IF Length(Decl) = 0 THEN
      BEGIN
        IF FParser.CurrTokenId = CSTI_OpenRound THEN
          BEGIN
            FParser.Next;
            IF FParser.CurrTokenId <> CSTI_CloseRound THEN
              BEGIN
                DisposePValue(Result);
                Result := NIL;
                MakeError('', ecCloseRoundExpected, '');
                exit;
              END;
            FParser.Next;
          END;
      END
    ELSE
      BEGIN
        IF FParser.CurrTokenId <> CSTI_OpenRound THEN
          BEGIN
            DisposePValue(Result);
            MakeError('', ecOpenRoundExpected, '');
            Result := NIL;
            exit;
          END;
        FParser.Next;
        WHILE Length(Decl) > 0 DO
          BEGIN
            modifier := getfc(GRFW(Decl));
            FType := StrToInt(GRFW(Decl));
            IF (modifier = '@') THEN
              BEGIN
                Tmp := calc(CSTI_CloseRound);
                IF Tmp = NIL THEN
                  BEGIN
                    DisposePValue(Result);
                    Result := NIL;
                    exit;
                  END;
              END
            ELSE
              BEGIN
                IF FParser.CurrTokenId <> CSTI_Identifier THEN
                  BEGIN
                    MakeError('', ecIdentifierExpected, '');
                    DisposePValue(Result);
                    Result := NIL;
                    exit;
                  END;
                Tmp := GetIdentifier(1); // only variables
                IF Tmp = NIL THEN
                  BEGIN
                    DisposePValue(Result);
                    Result := NIL;
                    exit;
                  END;
                IF ((FType = Cardinal(-1)) AND (PSCType(FUsedTypes.GetItem(GetTypeNo(Tmp)))^.BaseType = btArray)) THEN
                  BEGIN
            {nothing}
                  END ELSE IF IsVarInCompatible(FUsedTypes.GetItem(FType), FUsedTypes.GetItem(GetTypeNo(Tmp))) THEN
                  BEGIN
                    MakeError('', ecTypeMismatch, '');
                    DisposePValue(Result);
                    DisposePValue(Tmp);
                    Result := NIL;
                    exit;
                  END;
                Tmp^.FType := Tmp^.FType + CVAL_PushAddr;
              END;
            New(p);
            p^.InReg := Tmp;
            p^.OutReg := NIL;
            p^.FType := FType;
            Result._Parameters.Add(p);
            IF Length(Decl) = 0 THEN
              BEGIN
                IF FParser.CurrTokenId <> CSTI_CloseRound THEN
                  BEGIN
                    MakeError('', ecCloseRoundExpected, '');
                    DisposePValue(Result);
                    Result := NIL;
                    exit;
                  END; {if}
                FParser.Next;
              END
            ELSE
              BEGIN
                IF FParser.CurrTokenId <> CSTI_Comma THEN
                  BEGIN
                    MakeError('', ecCommaExpected, '');
                    DisposePValue(Result);
                    Result := NIL;
                    exit;
                  END; {if}
                FParser.Next;
              END; {else if}
          END; {for}
      END; {else if}
  END;

  FUNCTION calc(endOn: TSCToken): PSCValue;
  VAR
    Items: TSCList;
    p: PCalc_Item;
    x: PParam;
    v, vc: PSCValue;
    Pt: PSCType;
    C: Byte;
    modifiers: byte;
    L: Cardinal;

    PROCEDURE Cleanup;
    VAR
      p: PCalc_Item;
      l: Longint;
    BEGIN
      FOR l := 0 TO Items.Count - 1 DO
        BEGIN
          p := Items.GetItem(l);
          IF NOT p^.C THEN
            BEGIN
              DisposePValue(p^.OutRec);
            END;
          Dispose(p);
        END;
      Items.Free;
    END;

    FUNCTION SortItems: Boolean;
    VAR
      l: Longint;
      tt: Cardinal;
      p, p1, P2, ptemp: PCalc_Item;
      tempt: PSCType;
      pp: PParam;
      temps: STRING;

      FUNCTION GetResultType(p1, P2: PSCValue; Cmd: Byte): Cardinal;
      VAR
        t1, t2: PSCType;
        tt1, tt2: Cardinal;
      BEGIN
        tt1 := GetTypeNo(p1);
        t1 := FUsedTypes.GetItem(tt1);
        tt2 := GetTypeNo(P2);
        t2 := FUsedTypes.GetItem(tt2);
        IF (t1 = NIL) OR (t2 = NIL) THEN
          BEGIN
            Result := Cardinal(-1);
            exit;
          END;
        CASE Cmd OF
          0: {plus}
            BEGIN
              IF (t1^.BaseType = btVariant) AND (
                (t2^.BaseType = btVariant) OR
                (t2^.BaseType = btString) OR
                (t2^.BaseType = btPchar) OR
                (t2^.BaseType = btChar) OR
                (isIntRealType(t2^.BaseType))) THEN
                Result := tt1
              ELSE
                IF (t2^.BaseType = btVariant) AND (
                  (t1^.BaseType = btVariant) OR
                  (t1^.BaseType = btString) OR
                  (t1^.BaseType = btPchar) OR
                  (t1^.BaseType = btChar) OR
                  (isIntRealType(t1^.BaseType))) THEN
                  Result := tt2
                ELSE IF IsIntType(t1^.BaseType) AND IsIntType(t2^.BaseType) THEN
                  Result := tt1
                ELSE IF IsIntRealType(t1^.BaseType) AND
                  IsIntRealType(t2^.BaseType) THEN
                  BEGIN
                    IF IsRealType(t1^.BaseType) THEN
                      Result := tt1
                    ELSE
                      Result := tt2;
                  END
                ELSE IF (t1^.BaseType = btString) AND (t2^.BaseType = btChar) THEN
                  Result := tt1
                ELSE IF (t1^.BaseType = btChar) AND (t2^.BaseType = btString) THEN
                  Result := tt2
                ELSE IF (t1^.BaseType = btChar) AND (t2^.BaseType = btChar) THEN
                  Result := GetType(btString)
                ELSE IF (t1^.BaseType = btString) AND (t2^.BaseType =
                  btString) THEN
                  Result := tt1
                ELSE IF (t1^.BaseType = btString) AND (t2^.BaseType = btU8) THEN
                  Result := tt1
                ELSE IF (t1^.BaseType = btU8) AND (t2^.BaseType = btString) THEN
                  Result := tt2
                ELSE
                  Result := Cardinal(-1);
            END;
          1, 2, 3: { -  * / }
            BEGIN
              IF (t1^.BaseType = btVariant) AND (
                (t2^.BaseType = btVariant) OR
                (isIntRealType(t2^.BaseType))) THEN
                Result := tt1
              ELSE
                IF (t2^.BaseType = btVariant) AND (
                  (t1^.BaseType = btVariant) OR
                  (isIntRealType(t1^.BaseType))) THEN
                  Result := tt2
                ELSE IF IsIntType(t1^.BaseType) AND IsIntType(t2^.BaseType) THEN
                  Result := tt1
                ELSE IF IsIntRealType(t1^.BaseType) AND
                  IsIntRealType(t2^.BaseType) THEN
                  BEGIN
                    IF IsRealType(t1^.BaseType) THEN
                      Result := tt1
                    ELSE
                      Result := tt2;
                  END
                ELSE
                  Result := Cardinal(-1);
            END;
          7, 8, 9: {and,or,xor}
            BEGIN
              IF (t1^.BaseType = btVariant) AND (
                (t2^.BaseType = btVariant) OR
                (isIntType(t2^.BaseType))) THEN
                Result := tt1
              ELSE
                IF (t2^.BaseType = btVariant) AND (
                  (t1^.BaseType = btVariant) OR
                  (isIntType(t1^.BaseType))) THEN
                  Result := tt2
                ELSE IF IsIntType(t1^.BaseType) AND IsIntType(t2^.BaseType) THEN
                  Result := tt1
                ELSE IF (tt1 = at2ut(FBooleanType)) AND (tt2 = tt1) THEN
                  BEGIN
                    Result := tt1;
                    IF ((p1^.FType = CVAL_Data) OR (p2^.FType = CVAL_Data)) THEN
                      BEGIN
                        IF cmd = 7 THEN {and}
                          BEGIN
                            IF p1^.FType = CVAL_Data THEN
                              BEGIN
                                IF (p1^.FData^.Value[1] = #1) THEN
                                  MakeWarning('', ewIsNotNeeded, '"True and"')^.Position := p1^.DPos
                                ELSE
                                  MakeWarning('', ewCalculationAlwaysEvaluatesTo, 'False')^.Position := p1^.DPos
                              END ELSE BEGIN
                                IF (p2^.FData^.Value[1] = #1) THEN
                                  MakeWarning('', ewIsNotNeeded, '"and True"')^.Position := p2^.DPos
                                ELSE
                                  MakeWarning('', ewCalculationAlwaysEvaluatesTo, 'False')^.Position := p2^.DPos;
                              END;
                          END ELSE IF cmd = 8 THEN {or}
                          BEGIN
                            IF p1^.FType = CVAL_Data THEN
                              BEGIN
                                IF (p1^.FData^.Value[1] = #1) THEN
                                  MakeWarning('', ewCalculationAlwaysEvaluatesTo, 'True')^.Position := p1^.DPos
                                ELSE
                                  MakeWarning('', ewIsNotNeeded, '"False or"')^.Position := p1^.DPos
                              END ELSE BEGIN
                                IF (p2^.FData^.Value[1] = #1) THEN
                                  MakeWarning('', ewCalculationAlwaysEvaluatesTo, 'True')^.Position := p2^.DPos
                                ELSE
                                  MakeWarning('', ewIsNotNeeded, '"or False"')^.Position := p2^.DPos;
                              END;
                          END;
                      END;
                  END ELSE
                  Result := Cardinal(-1);
            END;
          4, 5, 6: {mod,shl,shr}
            BEGIN
              IF (t1^.BaseType = btVariant) AND (
                (t2^.BaseType = btVariant) OR
                (isIntType(t2^.BaseType))) THEN
                Result := tt1
              ELSE
                IF (t2^.BaseType = btVariant) AND (
                  (t1^.BaseType = btVariant) OR
                  (isIntType(t1^.BaseType))) THEN
                  Result := tt2
                ELSE IF IsIntType(t1^.BaseType) AND IsIntType(t2^.BaseType) THEN
                  Result := tt1
                ELSE
                  Result := Cardinal(-1);
            END;
          10, 11, 12, 13: { >=, <=, >, <}
            BEGIN
              IF (t1^.BaseType = btVariant) AND (
                (t2^.BaseType = btVariant) OR
                (t2^.BaseType = btString) OR
                (t2^.BaseType = btPchar) OR
                (t2^.BaseType = btChar) OR
                (isIntRealType(t2^.BaseType))) THEN
                Result := tt1
              ELSE
                IF (t2^.BaseType = btVariant) AND (
                  (t1^.BaseType = btVariant) OR
                  (t1^.BaseType = btString) OR
                  (t1^.BaseType = btPchar) OR
                  (t1^.BaseType = btChar) OR
                  (isIntRealType(t1^.BaseType))) THEN
                  Result := tt2
                ELSE IF IsIntType(t1^.BaseType) AND IsIntType(t2^.BaseType) THEN
                  Result := at2ut(FBooleanType)
                ELSE IF IsIntRealType(t1^.BaseType) AND
                  IsIntRealType(t2^.BaseType) THEN
                  Result := at2ut(FBooleanType)
                ELSE IF (t1^.BaseType = btString) AND (t2^.BaseType = btString) THEN
                  Result := at2ut(FBooleanType)
                ELSE IF (t1^.BaseType = btChar) AND (t2^.BaseType = btString) THEN
                  Result := at2ut(FBooleanType)
                ELSE IF (t1^.BaseType = btString) AND (t2^.BaseType = btChar) THEN
                  Result := at2ut(FBooleanType)
                ELSE IF (t1^.BaseType = btVariant) OR (t2^.BaseType = btVariant) THEN
                  Result := at2ut(FBooleanType)
                ELSE
                  Result := Cardinal(-1);
            END;
          14, 15: {=, <>}
            BEGIN
              IF (t1^.BaseType = btVariant) AND (
                (t2^.BaseType = btVariant) OR
                (t2^.BaseType = btString) OR
                (t2^.BaseType = btPchar) OR
                (t2^.BaseType = btChar) OR
                (isIntRealType(t2^.BaseType))) THEN
                Result := tt1
              ELSE
                IF (t2^.BaseType = btVariant) AND (
                  (t1^.BaseType = btVariant) OR
                  (t1^.BaseType = btString) OR
                  (t1^.BaseType = btPchar) OR
                  (t1^.BaseType = btChar) OR
                  (isIntRealType(t1^.BaseType))) THEN
                  Result := tt2
                ELSE IF IsIntType(t1^.BaseType) AND IsIntType(t2^.BaseType) THEN
                  Result := at2ut(FBooleanType)
                ELSE IF IsIntRealType(t1^.BaseType) AND
                  IsIntRealType(t2^.BaseType) THEN
                  Result := at2ut(FBooleanType)
                ELSE IF (t1^.BaseType = btChar) AND (t2^.BaseType = btChar) THEN
                  Result := at2ut(FBooleanType)
                ELSE IF (t1^.BaseType = btChar) AND (t2^.BaseType = btString) THEN
                  Result := at2ut(FBooleanType)
                ELSE IF (t1^.BaseType = btString) AND (t2^.BaseType = btChar) THEN
                  Result := at2ut(FBooleanType)
                ELSE IF (t1^.BaseType = btString) AND (t2^.BaseType = btString) THEN
                  Result := at2ut(FBooleanType)
                ELSE IF (t1^.BaseType = btEnum) AND (t1 = t2) THEN
                  Result := at2ut(FBooleanType)
                ELSE IF (t1^.BaseType = btVariant) OR (t2^.BaseType = btVariant) THEN
                  Result := at2ut(FBooleanType)
                ELSE Result := Cardinal(-1);
            END;
        ELSE
          Result := Cardinal(-1);
        END;
      END;
      PROCEDURE ApplyModifiers(FData: PSCValue);
      BEGIN
        IF (FData^.FType = CVAL_Data) THEN
          BEGIN
            IF FData^.Modifiers = 1 THEN // not
              BEGIN
                FData^.Modifiers := FData^.Modifiers AND NOT 1;
                CASE PSCType(FUsedTypes.GetItem(FData.FData^.FType))^.BaseType OF
                  btEnum: TbtU32((@FData^.FData^.Value[1])^) := tbtu32(TbtU32((@FData^.FData^.Value[1])^) = 0);
                  btU8: TbtU8((@FData.FData^.Value[1])^) := tbtu8(TbtU8((@FData.FData^.Value[1])^) = 0);
                  btS8: TbtS8((@FData^.FData^.Value[1])^) := tbts8(TbtS8((@FData^.FData^.Value[1])^) = 0);
                  btU16: TbtU16((@FData^.FData^.Value[1])^) := tbtu16(TbtU16((@FData^.FData^.Value[1])^) = 0);
                  btS16: TbtS16((@FData^.FData^.Value[1])^) := tbts16(TbtS16((@FData^.FData^.Value[1])^) = 0);
                  btU32: TbtU32((@FData^.FData^.Value[1])^) := tbtu32(TbtU32((@FData^.FData^.Value[1])^) = 0);
                  btS32: TbtS32((@FData^.FData^.Value[1])^) := tbts32(TbtS32((@FData^.FData^.Value[1])^) = 0);
                END;
              END ELSE
              IF FData^.Modifiers = 2 THEN // minus
                BEGIN
                  FData^.Modifiers := FData^.Modifiers AND NOT 2;
                  CASE PSCType(FUsedTypes.GetItem(FData^.FData^.FType))^.BaseType OF
                    btU8: TbtU8((@FData^.FData^.Value[1])^) := -TbtU8((@FData^.FData^.Value[1])^);
                    btS8: TbtS8((@FData^.FData^.Value[1])^) := -TbtS8((@FData^.FData^.Value[1])^);
                    btU16: TbtU16((@FData^.FData^.Value[1])^) := -TbtU16((@FData^.FData^.Value[1])^);
                    btS16: TbtS16((@FData^.FData^.Value[1])^) := -TbtS16((@FData^.FData^.Value[1])^);
                    btU32: TbtU32((@FData^.FData^.Value[1])^) := -TbtU32((@FData^.FData^.Value[1])^);
                    btS32: TbtS32((@FData^.FData^.Value[1])^) := -TbtS32((@FData^.FData^.Value[1])^);
                    btSingle: TbtSingle((@FData^.FData^.Value[1])^) := -TbtSingle((@FData^.FData^.Value[1])^);
                    btDouble: TbtDouble((@FData^.FData^.Value[1])^) := -TbtDouble((@FData^.FData^.Value[1])^);
                    btExtended: TbtExtended((@FData^.FData^.Value[1])^) := -tbtExtended((@FData^.FData^.Value[1])^);
                  END;
                END;
          END;
      END;
    BEGIN
      SortItems := False;
      IF Items.Count = 1 THEN
        BEGIN
          p1 := Items.GetItem(0);
          ApplyModifiers(p1^.OutRec);
          SortItems := True;
          exit;
        END;
      FOR l := 0 TO (Longint(Items.Count) DIV 2) DO
        BEGIN
          p1 := Items.GetItem(l SHL 1);
          IF p1^.OutRec^.FType = CVAL_Data THEN
            ApplyModifiers(P1^.OutRec);
        END;
      l := 0;
      WHILE l < Longint(Items.Count - 1) DIV 2 DO
        BEGIN
          p := Items.GetItem((l SHL 1) + 1);
          p1 := Items.GetItem((l SHL 1));
          P2 := Items.GetItem((l SHL 1) + 2);
          CASE p^.calcCmd OF
            2, 3, 4, 5, 6, 7: {*}
              BEGIN
                IF (p1^.OutRec^.FType = CVAL_Data) AND (P2^.OutRec^.FType =
                  CVAL_Data) THEN
                  BEGIN
                    IF NOT PreCalc(FUsedTypes, p1^.OutRec^.Modifiers, p1^.OutRec^.FData, p2^.OutRec^.Modifiers, P2^.OutRec^.FData,
                      p^.calcCmd, P2^.OutRec^.DPos) THEN
                      BEGIN
                        exit;
                      END;
                    Items.Delete((l SHL 1) + 1);
                    Items.Delete((l SHL 1) + 1);
                    DisposePValue(P2^.OutRec);
                    Dispose(P2);
                    Dispose(p);
                  END
                ELSE
                  BEGIN
                    tt := GetResultType(p1^.OutRec, P2^.OutRec, p^.calcCmd);
                    IF tt = Cardinal(-1) THEN
                      BEGIN
                        MakeError('', ecTypeMismatch, '')^.Position :=
                          P2^.OutRec^.DPos;
                        exit;
                      END;
                    New(ptemp);
                    ptemp^.C := False;
                    New(ptemp^.OutRec);
                    ptemp^.OutRec^.Modifiers := 0;
                    ptemp^.OutRec^.FType := CVAL_Eval;
                    ptemp^.OutRec^.SubItems := TSCList.Create;
                    ptemp^.OutRec^.SubItems.Add(p1);
                    ptemp^.OutRec^.SubItems.Add(p);
                    ptemp^.OutRec^.SubItems.Add(P2);
                    ptemp^.OutRec^.frestype := tt;
                    Items.SetItem((l SHL 1), ptemp);
                    Items.Delete((l SHL 1) + 1);
                    Items.Delete((l SHL 1) + 1);
                  END;
              END;
          ELSE
            Inc(l);
          END;
        END;
      l := 0;
      WHILE l < Longint(Items.Count - 1) DIV 2 DO
        BEGIN
          p := Items.GetItem((l SHL 1) + 1);
          p1 := Items.GetItem((l SHL 1));
          P2 := Items.GetItem((l SHL 1) + 2);
          CASE p^.calcCmd OF
            0, 1, 8, 9:
              BEGIN
                IF (p1^.OutRec^.FType = CVAL_Data) AND (P2^.OutRec^.FType =
                  CVAL_Data) THEN
                  BEGIN
                    IF NOT PreCalc(FUsedTypes, p1^.OutRec^.Modifiers, p1^.OutRec^.FData, p2^.OutRec^.Modifiers, P2^.OutRec^.FData,
                      p^.calcCmd, P2^.OutRec^.DPos) THEN
                      BEGIN
                        exit;
                      END;
                    Items.Delete((l SHL 1) + 1);
                    Items.Delete((l SHL 1) + 1);
                    DisposePValue(P2^.OutRec);
                    Dispose(P2);
                    Dispose(p);
                  END
                ELSE
                  BEGIN
                    tt := GetResultType(p1^.OutRec, P2^.OutRec, p^.calcCmd);
                    IF tt = Cardinal(-1) THEN
                      BEGIN
                        MakeError('', ecTypeMismatch, '')^.Position :=
                          P2^.OutRec^.DPos;
                        exit;
                      END;
                    New(ptemp);
                    ptemp^.C := False;
                    New(ptemp^.OutRec);
                    ptemp^.OutRec^.Modifiers := 0;
                    ptemp^.OutRec^.FType := CVAL_Eval;
                    ptemp^.OutRec^.SubItems := TSCList.Create;
                    ptemp^.OutRec^.SubItems.Add(p1);
                    ptemp^.OutRec^.SubItems.Add(p);
                    ptemp^.OutRec^.SubItems.Add(P2);
                    ptemp^.OutRec^.frestype := tt;
                    Items.SetItem((l SHL 1), ptemp);
                    Items.Delete((l SHL 1) + 1);
                    Items.Delete((l SHL 1) + 1);
                  END;
              END;
          ELSE
            Inc(l);
          END;
        END;
      l := 0;
      WHILE l < Longint(Items.Count - 1) DIV 2 DO
        BEGIN
          p := Items.GetItem((l SHL 1) + 1);
          p1 := Items.GetItem((l SHL 1));
          P2 := Items.GetItem((l SHL 1) + 2);
          CASE p^.calcCmd OF
            10, 11, 12, 13, 14, 15:
              BEGIN
                IF (p1^.OutRec^.FType <> CVAL_VarProcPtr) AND (p2^.OutRec^.FType <> CVAL_VarProcPtr) AND
                  ((PSCType(FUsedTypes.GetItem(GetTypeNo(p1^.OutRec)))^.BaseType = btclass) OR
                  (PSCType(FUsedTypes.GetItem(GetTypeNo(p2^.OutRec)))^.BaseType = btclass)) AND
                  ((p^.CalcCmd = 14) OR (p^.CalcCmd = 15)) THEN
                  BEGIN
                    tempt := FUsedTypes.GetItem(GetTypeNo(p1^.OutRec));
                    IF NOT tempt^.Ex.CompareClass(GetTypeNo(p2^.OutRec), tt) THEN
                      BEGIN
                        exit;
                      END;
                    new(ptemp);
                    ptemp^.C := False;
                    new(ptemp^.outrec);
                    WITH ptemp^.outrec^ DO
                      BEGIN
                        FType := CVAL_Proc;
                        IF p^.calcCmd = 14 THEN
                          Modifiers := 1
                        ELSE
                          Modifiers := 0;
                        ProcNo := tt;
                        Parameters := TSCList.Create;
                        new(pp);
                        IF PSCProcedure(FProcs.GetItem(tt))^.Internal THEN
                          temps := PSCProcedure(FProcs.GetItem(tt))^.Decl
                        ELSE
                          temps := PSCUsedRegProc(FProcs.GetItem(tt))^.rp^.Decl;
                        GRFW(temps);
                        pp^.InReg := p1^.OutRec;
                        pp^.OutReg := NIL;
                        grfw(temps);
                        pp^.FType := StrToIntDef(grfw(temps), -1);
                        pp^.OutRegPos := p1^.OutRec^.DPos;
                        Parameters.add(pp);
                        new(pp);
                        pp^.InReg := p2^.OutRec;
                        pp^.OutReg := NIL;
                        grfw(temps);
                        pp^.FType := StrToIntDef(grfw(temps), -1);
                        pp^.OutRegPos := p2^.OutRec^.DPos;
                        Parameters.add(pp);
                      END;
                    Items.SetItem((l SHL 1), ptemp);
                    Items.Delete((l SHL 1) + 1);
                    Items.Delete((l SHL 1) + 1);
                    Dispose(P2);
                    dispose(p1);
                    Dispose(p);
                  END ELSE
                  IF (p1^.OutRec^.FType = CVAL_Data) AND (P2^.OutRec^.FType =
                    CVAL_Data) THEN
                    BEGIN
                      IF NOT PreCalc(FUsedTypes, p1^.OutRec^.Modifiers, p1^.OutRec^.FData, p2^.OutRec^.Modifiers, P2^.OutRec^.FData,
                        p^.calcCmd, P2^.OutRec^.DPos) THEN
                        BEGIN
                          exit;
                        END;
                      Items.Delete((l SHL 1) + 1);
                      Items.Delete((l SHL 1) + 1);
                      DisposePValue(P2^.OutRec);
                      Dispose(P2);
                      Dispose(p);
                    END
                  ELSE
                    BEGIN
                      tt := GetResultType(p1^.OutRec, P2^.OutRec, p^.calcCmd);
                      IF tt = Cardinal(-1) THEN
                        BEGIN
                          MakeError('', ecTypeMismatch, '')^.Position :=
                            P2^.OutRec^.DPos;
                          exit;
                        END;
                      New(ptemp);
                      ptemp^.C := False;
                      New(ptemp^.OutRec);
                      ptemp^.OutRec^.Modifiers := 0;
                      ptemp^.OutRec^.FType := CVAL_Eval;
                      ptemp^.OutRec^.SubItems := TSCList.Create;
                      ptemp^.OutRec^.SubItems.Add(p1);
                      ptemp^.OutRec^.SubItems.Add(p);
                      ptemp^.OutRec^.SubItems.Add(P2);
                      ptemp^.OutRec^.frestype := tt;
                      Items.SetItem((l SHL 1), ptemp);
                      Items.Delete((l SHL 1) + 1);
                      Items.Delete((l SHL 1) + 1);
                    END;
              END;
          ELSE
            Inc(l);
          END;
        END;
      SortItems := True;
    END;
  BEGIN
    Items := TSCList.Create;
    calc := NIL;
    WHILE True DO
      BEGIN
        modifiers := 0;
        IF Items.Count AND 1 = 0 THEN
          BEGIN
            IF fParser.CurrTokenID = CSTII_Not THEN
              BEGIN
                FParser.Next;
                modifiers := 1;
              END ELSE // only allow one of these two
              IF fParser.CurrTokenID = CSTI_Minus THEN
                BEGIN
                  FParser.Next;
                  modifiers := 2;
                END;
            CASE FParser.CurrTokenId OF
              CSTI_AddressOf:
                BEGIN
                  IF (Modifiers <> 0) THEN
                    BEGIN
                      MakeError('', ecTypeMismatch, '');
                      Cleanup;
                      exit;
                    END;
                  FParser.Next;
                  IF FParser.CurrTokenId <> CSTI_Identifier THEN
                    BEGIN
                      MakeError('', ecIdentifierExpected, '');
                      Cleanup;
                      Exit;
                    END;
                  L := FindProc(FParser.GetToken);
                  IF L = Cardinal(-1) THEN
                    BEGIN
                      MakeError('', ecUnknownIdentifier, FParser.OriginalToken);
                      Cleanup;
                      Exit;
                    END;
                  PSCProcedure(FProcs.GetItem(L))^.FExport := 2;
                  FParser.Next;
                  New(v);
                  v^.FType := CVAL_VarProcPtr;
                  v^.Modifiers := 0;
                  v^.DPos := FParser.CurrTokenPos;
                  v^.VProcNo := L;
                  New(p);
                  p^.C := False;
                  p^.OutRec := v;
                  Items.Add(p);
                END;
              CSTI_OpenBlock:
                BEGIN
                  IF (Modifiers <> 0) THEN
                    BEGIN
                      MakeError('', ecTypeMismatch, '');
                      Cleanup;
                      exit;
                    END;
                  New(v);
                  v^.FType := CVAL_Array;
                  v^.Modifiers := 0;
                  v^.DPos := FParser.CurrTokenPos;
                  v^.ArrayItems := TSCList.Create;
                  New(p);
                  p^.C := False;
                  p^.OutRec := v;
                  Items.Add(p);
                  FParser.Next;
                  WHILE FParser.CurrTokenId <> CSTI_CloseBlock DO
                    BEGIN
                      vc := calc(CSTI_CloseBlock);
                      IF vc = NIL THEN
                        BEGIN
                          Cleanup;
                          exit;
                        END; {if}
                      IF vc^.FType = CVAL_Array THEN
                        BEGIN
                          MakeError('', ecIdentifierExpected, '')^.Position := v^.DPos;
                          Cleanup;
                          Exit;
                        END;
                      v^.ArrayItems.Add(vc);
                      IF FParser.CurrTokenId = CSTI_Comma THEN
                        BEGIN
                          FParser.Next;
                          Continue;
                        END;
                    END; {while}
                  FParser.Next;
                END; {csti_openblock}
              CSTI_EOF:
                BEGIN
                  MakeError('', ecUnexpectedEndOfFile, '');
                  Cleanup;
                  exit;
                END;
              CSTI_OpenRound:
                BEGIN
                  FParser.Next;
                  v := calc(CSTI_CloseRound);
                  IF v = NIL THEN
                    BEGIN
                      Cleanup;
                      exit;
                    END;
                  IF FParser.CurrTokenId <> CSTI_CloseRound THEN
                    BEGIN
                      DisposePValue(v);
                      MakeError('', ecCloseRoundExpected, '');
                      Cleanup;
                      exit;
                    END;
                  IF ((Modifiers AND 1) <> 0) AND (NOT IsIntBoolType(GetTypeNo(v))) OR ((Modifiers AND 2) <> 0) AND (NOT IsRealType(PSCType(FUsedTypes.GetItem(GetTypeNo(v)))^.BaseType)) THEN
                    BEGIN
                      DisposePValue(v);
                      MakeError('', ecTypeMismatch, '');
                      Cleanup;
                      exit;
                    END;

                  New(p);
                  p^.C := False;
                  IF ((v^.Modifiers AND 1) <> 0) OR ((modifiers AND 1) <> 0) THEN
                    BEGIN
                      v^.modifiers := v^.modifiers XOR (modifiers AND 1);
                    END;
                  IF ((v^.Modifiers AND 2) <> 0) OR ((modifiers AND 2) <> 0) THEN
                    BEGIN
                      v^.modifiers := v^.modifiers XOR (modifiers AND 2);
                    END;
                  p^.OutRec := v;
                  Items.Add(p);

                  FParser.Next;
                END;
              CSTII_Chr:
                BEGIN
                  IF modifiers <> 0 THEN
                    BEGIN
                      MakeError('', ecTypeMismatch, '');
                      Cleanup;
                      exit;
                    END;
                  FParser.Next;
                  IF FParser.CurrTokenID <> CSTI_OpenRound THEN
                    BEGIN
                      MakeError('', ecOpenRoundExpected, '');
                      Cleanup;
                      exit;
                    END;
                  FParser.Next;
                  v := calc(CSTI_CloseRound);
                  IF v = NIL THEN
                    BEGIN
                      Cleanup;
                      exit;
                    END;
                  IF FParser.CurrTokenId <> CSTI_CloseRound THEN
                    BEGIN
                      DisposePValue(v);
                      MakeError('', ecCloseRoundExpected, '');
                      Cleanup;
                      exit;
                    END;
                  IF NOT IsIntType(PSCType(FUsedTypes.GetItem(GetTypeNo(v)))^.BaseType) THEN
                    BEGIN
                      DisposePValue(v);
                      MakeError('', ecTypeMismatch, '');
                      Cleanup;
                      exit;
                    END;
                  New(p);
                  p^.c := False;
                  New(p^.OutRec);
                  p^.OutRec^.FType := CVAL_Cast;
                  p^.OutRec^.Modifiers := 0;
                  p^.OutRec^.DPos := FParser.CurrTokenPos;
                  p^.OutRec^.Input := v;
                  p^.OutRec^.NewTypeNo := GetType(btChar);
                  Items.Add(p);
                  FParser.Next;
                END;
              CSTII_Ord:
                BEGIN
                  FParser.Next;
                  IF FParser.CurrTokenID <> CSTI_OpenRound THEN
                    BEGIN
                      MakeError('', ecOpenRoundExpected, '');
                      Cleanup;
                      exit;
                    END;
                  FParser.Next;
                  v := calc(CSTI_CloseRound);
                  IF v = NIL THEN
                    BEGIN
                      Cleanup;
                      exit;
                    END;
                  IF FParser.CurrTokenId <> CSTI_CloseRound THEN
                    BEGIN
                      DisposePValue(v);
                      MakeError('', ecCloseRoundExpected, '');
                      Cleanup;
                      exit;
                    END;
                  Pt := FUsedTypes.GetItem(GetTypeNo(v));
                  IF (pt^.BaseType = btString) AND (v^.FType = CVAL_Data) AND (Length(v^.FData.Value) = 1) THEN
                    BEGIN
                      v^.FData.FType := GetType(btChar);
                      Pt := FUsedTypes.GetItem(GetTypeNo(v));
                    END;
                  New(p);
                  p^.c := False;
                  IF ((v^.Modifiers AND 1) <> 0) OR ((modifiers AND 1) <> 0) THEN
                    BEGIN
                      v^.modifiers := v^.modifiers XOR (modifiers AND 1);
                    END;
                  IF ((v^.Modifiers AND 2) <> 0) OR ((modifiers AND 2) <> 0) THEN
                    BEGIN
                      v^.modifiers := v^.modifiers XOR (modifiers AND 2);
                    END;
                  New(p^.OutRec);
                  p^.OutRec^.FType := CVAL_Cast;
                  p^.OutRec^.Modifiers := 0;
                  p^.OutRec^.DPos := FParser.CurrTokenPos;
                  p^.OutRec^.Input := v;
                  IF (pt^.BaseType = btChar) THEN
                    BEGIN
                      p^.OutRec^.NewTypeNo := GetType(btU8);
                    END ELSE IF (pt^.BaseType = btEnum) THEN
                    BEGIN
                      IF Longint(pt^.Ext) <= 256 THEN
                        p^.OutRec^.NewTypeNo := GetType(btU8)
                      ELSE IF Longint(pt^.Ext) <= 65536 THEN
                        p^.OutRec^.NewTypeNo := GetType(btU16)
                      ELSE
                        p^.OutRec^.NewTypeNo := GetType(btU32);
                    END ELSE
                    BEGIN
                      Dispose(P^.OutRec);
                      Dispose(p);
                      DisposePValue(v);
                      MakeError('', ecTypeMismatch, '');
                      Cleanup;
                      exit;
                    END;
                  Items.Add(p);
                  FParser.Next;
                END;
              CSTI_String, CSTI_Char:
                BEGIN
                  IF (Modifiers <> 0) THEN
                    BEGIN
                      MakeError('', ecTypeMismatch, '');
                      Cleanup;
                      exit;
                    END;
                  New(v);
                  v^.FType := CVAL_Data;
                  v^.DPos := FParser.CurrTokenPos;
                  v^.FData := ReadString;
                  v^.Modifiers := modifiers;
                  v^.RecField := NIL;
                  New(p);
                  p^.C := False;
                  p^.OutRec := v;
                  Items.Add(p);

                END;
              CSTI_HexInt, CSTI_Integer:
                BEGIN
                  New(v);
                  v^.FType := CVAL_Data;
                  v^.DPos := FParser.CurrTokenPos;
                  v^.FData := ReadInteger(FParser.GetToken);
                  v^.Modifiers := modifiers;
                  New(p);
                  p^.C := False;
                  p^.OutRec := v;
                  Items.Add(p);

                  FParser.Next;
                END;
              CSTI_Real:
                BEGIN
                  IF ((Modifiers AND 1) <> 0) THEN
                    BEGIN
                      MakeError('', ecTypeMismatch, '');
                      Cleanup;
                      exit;
                    END;
                  New(v);
                  v^.FType := CVAL_Data;
                  v^.DPos := FParser.CurrTokenPos;
                  v^.FData := ReadReal(FParser.GetToken);
                  v^.Modifiers := modifiers;
                  New(p);
                  p^.C := False;
                  p^.OutRec := v;
                  Items.Add(p);
                  FParser.Next;
                END;
              CSTI_Identifier:
                BEGIN
                  IF FParser.GetToken = 'LOW' THEN
                    c := 1
                  ELSE
                    c := 0;
                  IF (FParser.GetToken = 'HIGH') OR (c <> 0) THEN
                    BEGIN
                      FParser.Next;
                      IF FParser.CurrTokenId <> CSTI_OpenRound THEN
                        BEGIN
                          MakeError('', ecOpenRoundExpected, '');
                          Cleanup;
                          Exit;
                        END;
                      FParser.Next;
                      L := FindType(FParser.GetToken);
                      IF L = Cardinal(-1) THEN
                        BEGIN
                          v := GetIdentifier(1);
                          IF v = NIL THEN
                            BEGIN
                              Cleanup;
                              Exit;
                            END;
                          L := GetTypeNo(v);
                          DisposePValue(v);
                        END ELSE FParser.Next;
                      pt := FAvailableTypes.GetItem(L);
                      IF pt^.BaseType <> btEnum THEN
                        BEGIN
                          MakeError('', ecTypeMismatch, '');
                          Cleanup;
                          Exit;
                        END;
                      New(v);
                      new(v^.FData);
                      v^.FType := CVAL_Data;
                      v^.DPos := FParser.CurrTokenPos;
                      v^.FData^.FType := AT2UT(L);
                      IF c = 1 THEN
                        v^.FData^.Value := #0#0#0#0
                      ELSE
                        v^.FData^.Value := TransCardinalToStr(Cardinal(pt^.Ex));
                      v^.Modifiers := modifiers;
                      New(p);
                      p^.C := False;
                      p^.OutRec := v;
                      Items.Add(p);
                      IF FParser.CurrTokenId <> CSTI_CloseRound THEN
                        BEGIN
                          MakeError('', ecCloseRoundExpected, '');
                          Cleanup;
                          Exit;
                        END;
                    END ELSE IF FParser.GetToken = 'ASSIGNED' THEN
                    BEGIN
                      IF (Modifiers AND 2) <> 0 THEN
                        BEGIN
                          MakeError('', ecTypeMismatch, '');
                          cleanup;
                          exit;
                        END;
                      FParser.Next;
                      IF FParser.CurrTokenId <> CSTI_OpenRound THEN
                        BEGIN
                          MakeError('', ecOpenRoundExpected, '');
                          Cleanup;
                          Exit;
                        END;
                      FParser.Next;
                      vc := calc(CSTI_CloseRound);
                      IF vc = NIL THEN
                        BEGIN
                          Cleanup;
                          Exit;
                        END;
                      Pt := FUsedTypes.GetItem(GetTypeNo(vc));
                      IF (pt^.BaseType <> btProcPtr) AND (pt^.BaseType <> btClass) AND (pt^.BaseType <> btPChar) AND (pt^.BaseType <> btString) THEN
                        BEGIN
                          DisposePValue(vc);
                          MakeError('', ecTypeMismatch, '');
                          Cleanup;
                          exit;
                        END;
                      IF FParser.CurrTokenId <> CSTI_CloseRound THEN
                        BEGIN
                          MakeError('', ecCloseRoundExpected, '');
                          Cleanup;
                          Exit;
                        END;
                      FParser.Next;
                      new(v);
                      V^.FType := CVAL_Proc;
                      v^.Modifiers := 0;
                      v^.ProcNo := FindProc('!ASSIGNED');
                      V^.Parameters := TSCList.Create;
                      new(x);
                      X^.InReg := vc;
                      x^.OutReg := NIL;
                      x^.FType := GetTypeNo(vc);
                      X^.OutRegPos := FParser.CurrTokenPos;
                      v^.Parameters.Add(x);
                      new(p);
                      p^.C := False;
                      p^.OutRec := v;
                      Items.Add(p);
                    END ELSE IF FParser.GetToken = 'NIL' THEN
                    BEGIN
                      IF modifiers <> 0 THEN
                        BEGIN
                          MakeError('', ecTypeMismatch, '');
                          cleanup;
                          exit;
                        END;
                      New(v);
                      v^.FType := CVAL_Nil;
                      v^.DPos := FParser.CurrTokenPos;
                      v^.Modifiers := 0;
                      New(p);
                      p^.C := False;
                      p^.OutRec := v;
                      Items.Add(p);
                      FParser.Next;
                    END ELSE BEGIN
                      v := GetIdentifier(0);
                      IF v = NIL THEN
                        BEGIN
                          Cleanup;
                          exit;
                        END
                      ELSE IF (GetTypeNo(v) = Cardinal(-1)) THEN
                        BEGIN
                          MakeError('', ecTypeMismatch, '')^.Position := v^.DPos;
                          DisposePValue(v);
                          Cleanup;
                          Exit;
                        END ELSE
                        BEGIN
                          IF ((Modifiers AND 1) <> 0) AND (NOT IsIntBoolType(GetTypeNo(v))) OR ((Modifiers
                            AND 2) <> 0) AND (NOT IsIntRealType(PSCType(
                            FUsedTypes.GetItem(GetTypeNo(v)))^.BaseType))
                            THEN
                            BEGIN
                              DisposePValue(v);
                              MakeError('', ecTypeMismatch, '');
                              Cleanup;
                              exit;
                            END;
                          v^.Modifiers := v^.modifiers OR modifiers;
                          New(p);
                          p^.C := False;
                          p^.OutRec := v;
                          Items.Add(p);
                        END;
                    END;
                END;
            ELSE
              BEGIN
                MakeError('', ecSyntaxError, '');
                Cleanup;
                exit;
              END;
            END; {case}
          END
        ELSE {Items.Count and 1 = 1}
          BEGIN
            IF FParser.CurrTokenId = endOn THEN
              break;
            C := 0;
            CASE FParser.CurrTokenId OF
              CSTI_EOF:
                BEGIN
                  MakeError('', ecUnexpectedEndOfFile, '');
                  Cleanup;
                  exit;
                END;
              CSTI_CloseBlock,
                CSTII_To,
                CSTI_CloseRound,
                CSTI_Semicolon,
                CSTII_Else,
                CSTII_End,
                CSTI_Comma: break;
              CSTI_Plus: ;
              CSTI_Minus: C := 1;
              CSTI_Multiply: C := 2;
              CSTII_div, CSTI_Divide: C := 3;
              CSTII_mod: C := 4;
              CSTII_shl: C := 5;
              CSTII_shr: C := 6;
              CSTII_and: C := 7;
              CSTII_or: C := 8;
              CSTII_xor: C := 9;
              CSTI_GreaterEqual: C := 10;
              CSTI_LessEqual: C := 11;
              CSTI_Greater: C := 12;
              CSTI_Less: C := 13;
              CSTI_NotEqual: C := 14;
              CSTI_Equal: C := 15;
            ELSE
              BEGIN
                MakeError('', ecSyntaxError, '');
                Cleanup;
                exit;
              END;
            END; {case}
            New(p);
            p^.C := True;
            p^.calcCmd := C;
            Items.Add(p);
            FParser.Next;
          END;
      END;
    IF NOT SortItems THEN
      BEGIN
        Cleanup;
        exit;
      END;
    IF Items.Count = 1 THEN
      BEGIN
        p := Items.GetItem(0);
        Result := p^.OutRec;
        Dispose(p);
        Items.Free;
      END
    ELSE
      BEGIN
        New(Result);
        Result^.FType := CVAL_Eval;
        Result^.DPos := 0;
        result^.Modifiers := 0;
        Result^.SubItems := Items;
      END;
  END;

  FUNCTION ReadParameters(ProcNo: Cardinal; fSelf: PSCValue): PSCValue;
  VAR
    Decl: STRING;
    p: PParam;
    Tmp: PSCValue;
    FType: Cardinal;
    modifier: Char;

    FUNCTION IsVarInCompatible(ft1, ft2: PSCType): Boolean;
    BEGIN
      ft1 := GetTypeCopyLink(ft1);
      ft2 := GetTypeCopyLink(ft2);
      Result := (ft1 <> ft2);
    END;

    FUNCTION getfc(CONST s: STRING): Char;
    BEGIN
      IF Length(s) > 0 THEN
        Result := s[1]
      ELSE
        Result := #0
    END;
  BEGIN
    IF PSCProcedure(FProcs.GetItem(ProcNo))^.Internal THEN
      Decl := PSCProcedure(FProcs.GetItem(ProcNo))^.Decl
    ELSE
      Decl := PSCUsedRegProc(FProcs.GetItem(ProcNo))^.RP^.Decl;
    GRFW(Decl);
    New(Result);
    Result^.FType := CVAL_Proc;
    Result^.DPos := FParser.CurrTokenPos;
    Result^.Modifiers := 0;
    Result^.ProcNo := ProcNo;
    Result^.Parameters := TSCList.Create;
    IF FSelf <> NIL THEN BEGIN
        new(p);
        p^.InReg := fself;
        p^.OutReg := NIL;
        p^.FType := GetTypeNo(fself);
        Result^.Parameters.Add(p);
      END;
    IF Length(Decl) = 0 THEN
      BEGIN
        IF FParser.CurrTokenId = CSTI_OpenRound THEN
          BEGIN
            FParser.Next;
            IF FParser.CurrTokenId <> CSTI_CloseRound THEN
              BEGIN
                DisposePValue(Result);
                Result := NIL;
                MakeError('', ecCloseRoundExpected, '');
                exit;
              END;
            FParser.Next;
          END;
      END
    ELSE
      BEGIN
        IF FParser.CurrTokenId <> CSTI_OpenRound THEN
          BEGIN
            DisposePValue(Result);
            MakeError('', ecOpenRoundExpected, '');
            Result := NIL;
            exit;
          END;
        FParser.Next;
        WHILE Length(Decl) > 0 DO
          BEGIN
            modifier := getfc(GRFW(Decl));
            FType := StrToInt(GRFW(Decl));
            IF (modifier = '@') THEN
              BEGIN
                Tmp := calc(CSTI_CloseRound);
                IF Tmp = NIL THEN
                  BEGIN
                    DisposePValue(Result);
                    Result := NIL;
                    exit;
                  END;
              END
            ELSE
              BEGIN
                IF FParser.CurrTokenId <> CSTI_Identifier THEN
                  BEGIN
                    MakeError('', ecIdentifierExpected, '');
                    DisposePValue(Result);
                    Result := NIL;
                    exit;
                  END;
                Tmp := GetIdentifier(1); // only variables
                IF Tmp = NIL THEN
                  BEGIN
                    DisposePValue(Result);
                    Result := NIL;
                    exit;
                  END;
                IF ((FType = Cardinal(-1)) OR (PSCType(FUsedTypes.GetItem(GetTypeNo(Tmp)))^.BaseType = btArray)) THEN
                  BEGIN
            {nothing}
                  END ELSE IF IsVarInCompatible(FUsedTypes.GetItem(FType), FUsedTypes.GetItem(GetTypeNo(Tmp))) THEN
                  BEGIN
                    MakeError('', ecTypeMismatch, '');
                    DisposePValue(Result);
                    DisposePValue(Tmp);
                    Result := NIL;
                    exit;
                  END;
                Tmp^.FType := Tmp^.FType + CVAL_PushAddr;
              END;
            New(p);
            p^.InReg := Tmp;
            p^.OutReg := NIL;
            p^.FType := FType;
            Result.Parameters.Add(p);
            IF Length(Decl) = 0 THEN
              BEGIN
                IF FParser.CurrTokenId <> CSTI_CloseRound THEN
                  BEGIN
                    MakeError('', ecCloseRoundExpected, '');
                    DisposePValue(Result);
                    Result := NIL;
                    exit;
                  END; {if}
                FParser.Next;
              END
            ELSE
              BEGIN
                IF FParser.CurrTokenId <> CSTI_Comma THEN
                  BEGIN
                    MakeError('', ecCommaExpected, '');
                    DisposePValue(Result);
                    Result := NIL;
                    exit;
                  END; {if}
                FParser.Next;
              END; {else if}
          END; {for}
      END; {else if}
  END;


  FUNCTION WriteCalculation(InData, OutReg: PSCValue): Boolean;
  VAR
    l: Longint;
    tmpcalc, p, PT, pt2: PSCValue;
    bmodsave: byte;
    C: Byte;

    FUNCTION CheckOutreg(Where, Outreg: PSCValue): Boolean;
    VAR
      i: Longint;
      P: PCalc_Item;
    BEGIN
      CASE Where^.FType OF
        CVAL_Cast:
          BEGIN
            IF CheckOutreg(Where^.Input, Outreg) THEN
              BEGIN
                Result := True;
                exit;
              END;
          END;
        CVAL_Addr, CVAL_PushAddr, CVAL_AllocatedStackReg:
          BEGIN
            IF SameReg(Where, OutReg) THEN
              BEGIN
                Result := True;
                exit;
              END;
          END;
        CVAL_Eval:
          FOR i := 0 TO Where.SubItems.Count - 1 DO
            BEGIN
              p := Where.SubItems.GetItem(i);
              IF NOT p^.C THEN
                IF CheckOutreg(p^.OutRec, Outreg) THEN
                  BEGIN
                    Result := True;
                    Exit;
                  END;
            END;
        CVAL_Proc, CVAL_VarProc:
          FOR i := 0 TO Where^.Parameters.Count - 1 DO
            BEGIN
              IF CheckOutreg(PParam(Where^.Parameters.GetItem(i))^.InReg, Outreg) THEN
                BEGIN
                  Result := True;
                  Exit;
                END;
            END;
        CVAL_ClassProcCall,
          CVAL_ClassMethodCall,
          CVAL_ClassPropertyCallSet,
          CVAL_ClassPropertyCallGet:
          BEGIN
            IF CheckOutreg(Where^.Self, Outreg) THEN
              BEGIN
                Result := True;
                exit;
              END;
            FOR i := 0 TO Where^.Params.Count - 1 DO
              BEGIN
                IF CheckOutreg(PParam(Where^.Params.GetItem(i))^.InReg, Outreg) THEN
                  BEGIN
                    Result := True;
                    Exit;
                  END;
              END;

          END;
      END;
      Result := False; ;
    END;
  BEGIN
    IF indata^.FType = CVAL_Cast THEN
      BEGIN
        IF GetTypeNo(OutReg) = Indata^.NewTypeNo THEN
          BEGIN
            OutReg^.Modifiers := outreg^.modifiers OR 4;
            Result := WriteCalculation(Indata^.Input, OutReg);
            OutReg^.Modifiers := outreg^.modifiers AND NOT 4;
            Exit;
          END ELSE BEGIN
            p := AllocStackReg(Indata^.NewTypeNo);
            p^.DPos := InData^.DPos;
            p^.Modifiers := p^.modifiers OR 4;
            IF NOT WriteCalculation(Indata^.Input, p) THEN
              BEGIN
                DisposeStackReg(p);
                Result := False;
                Exit;
              END;
            Result := WriteCalculation(p, outreg);
            DisposeStackReg(p);
            exit;
          END;
      END ELSE
      IF InData^.FType = CVAL_VarProcPtr THEN
        BEGIN
          IF NOT CheckCompatProc(GetTypeNo(OutReg), InData^.VProcNo) THEN
            BEGIN
              MakeError('', ecTypeMismatch, '')^.Position := InData^.DPos;
              Result := False;
              exit;
            END;
          New(p);
          p^.FType := CVAL_Data;
          p^.Modifiers := 0;
          p^.DPos := Indata^.DPos;
          New(p^.FData);
          p^.FData.FType := GetTypeNo(OutReg);
          p^.FData.Value := mi2s(Indata^.VProcNo);
          WriteCommand(CM_A);
          WriteOutRec(OutReg, False);
          WriteOutRec(p, True);
          DisposePValue(p);
        END ELSE
        IF (InData^.FType = CVAL_Proc) OR (InData^.FType = CVAL_VarProc) THEN
          BEGIN
            IF NOT CheckCompatType(OutReg, InData) THEN
              BEGIN
                MakeError('', ecTypeMismatch, '')^.Position := InData^.DPos;
                Result := False;
                exit;
              END;

            IF InData^.FType = CVAL_VarProc THEN
              BEGIN
                IF NOT ProcessVarFunction(InData^.Modifiers, InData^._ProcNo, InData^._Parameters, OutReg) THEN
                  BEGIN
                    Result := False;
                    exit;
                  END;
              END ELSE BEGIN
                IF NOT ProcessFunctionX(InData^.Modifiers, InData^.ProcNo, InData^.Parameters, OutReg) THEN
                  BEGIN
                    Result := False;
                    exit;
                  END;
              END;
            IF Indata^.Modifiers = 1 THEN BEGIN
                PreWriteOutRec(OutReg, Cardinal(-1));
                WriteCommand(cm_bn);
                WriteOutRec(OutReg, False);
                AfterWriteOutRec(OutReg);
              END ELSE IF Indata^.Modifiers = 2 THEN BEGIN
                PreWriteOutRec(OutReg, Cardinal(-1));
                WriteCommand(cm_vm);
                WriteOutRec(OutReg, False);
                AfterWriteOutRec(OutReg);
              END;
          END
        ELSE IF InData^.FType = CVAL_Eval THEN
          BEGIN
            IF CheckOutreg(InData, OutReg) THEN
              BEGIN
                tmpcalc := AllocStackReg(GetTypeNo(OutReg));
                IF NOT WriteCalculation(InData, TmpCalc) THEN
                  BEGIN
                    DisposeStackReg(tmpcalc);
                    Result := False;
                    exit;
                  END;
                IF NOT WriteCalculation(TmpCalc, OutReg) THEN
                  BEGIN
                    DisposeStackReg(tmpcalc);
                    Result := False;
                    exit;
                  END;
                DisposeStackReg(tmpcalc);
              END ELSE BEGIN
                bmodsave := Indata^.Modifiers AND 15;
                p := PCalc_Item(InData^.SubItems.GetItem(0))^.OutRec;
                C := PCalc_Item(InData^.SubItems.GetItem(1))^.calcCmd;
                IF c >= 10 THEN
                  BEGIN
                    tmpcalc := p;
                  END ELSE BEGIN
                    IF NOT WriteCalculation(p, OutReg) THEN
                      BEGIN
                        Result := False;
                        exit;
                      END; {if}
                    tmpcalc := NIL;
                  END;
                FOR l := 0 TO ((InData^.SubItems.Count - 1) DIV 2) - 1 DO
                  BEGIN
                    p := PCalc_Item(InData^.SubItems.GetItem((l SHL 1) + 2))^.OutRec;
                    C := PCalc_Item(InData^.SubItems.GetItem((l SHL 1) + 1))^.calcCmd;
                    IF C < 10 THEN
                      BEGIN
                        IF p^.FType = CVAL_Eval THEN
                          BEGIN
                            PT := AllocStackReg(GetTypeNo(OutReg));
                            IF NOT WriteCalculation(p, PT) THEN
                              BEGIN
                                DisposeStackReg(PT);
                                Result := False;
                                exit;
                              END; {if}
                            PreWriteOutRec(OutReg, Cardinal(-1)); {error}
                            WriteCommand(CM_CA);
                            WriteData(C, 1);
                            IF NOT WriteOutRec(OutReg, False) THEN
                              BEGIN
                                MakeError('', ecInternalError, '00001');
                                DisposeStackReg(pt);
                                Result := False;
                                exit;
                              END; {if}
                            IF NOT WriteOutRec(PT, True) THEN
                              BEGIN
                                MakeError('', ecInternalError, '00002');
                                DisposeStackReg(pt);
                                Result := False;
                                exit;
                              END; {if}
                            AfterWriteOutRec(Pt);
                            DisposeStackReg(PT);
                          END
                        ELSE IF (p^.FType = CVAL_Proc) OR (P^.Ftype = CVAL_VarProc) OR (p^.FType = CVAL_Cast) THEN
                          BEGIN
                            PT := AllocStackReg(GetTypeNo(OutReg));
                            IF NOT WriteCalculation(p, Pt) THEN
                              BEGIN
                                DisposeStackReg(Pt);
                                Result := False;
                                exit;
                              END;
                            PreWriteOutRec(OutReg, Cardinal(-1)); {error}
                            PreWriteOutRec(pt, Cardinal(-1)); {error}
                            WriteCommand(CM_CA);
                            WriteData(C, 1);
                            IF NOT WriteOutRec(OutReg, False) THEN
                              BEGIN
                                MakeError('', ecInternalError, '00005');
                                Result := False;
                                exit;
                              END; {if}
                            IF NOT WriteOutRec(pt, True) THEN
                              BEGIN
                                MakeError('', ecInternalError, '00006');
                                Result := False;
                                exit;
                              END; {if}
                            AfterWriteOutRec(p);
                            AfterWriteOutRec(OutReg);
                            DisposeStackReg(Pt);
                          END ELSE BEGIN
                            PreWriteOutRec(OutReg, Cardinal(-1)); {error}
                            PreWriteOutRec(p, GetTypeNo(Outreg)); {error}
                            WriteCommand(CM_CA);
                            WriteData(C, 1);
                            IF NOT WriteOutRec(OutReg, False) THEN
                              BEGIN
                                MakeError('', ecInternalError, '00005');
                                Result := False;
                                exit;
                              END; {if}
                            IF NOT WriteOutRec(p, True) THEN
                              BEGIN
                                MakeError('', ecInternalError, '00006');
                                Result := False;
                                exit;
                              END; {if}
                            AfterWriteOutRec(p);
                            AfterWriteOutRec(OutReg);

                          END; {else if}
                      END
                    ELSE
                      BEGIN
                        C := C - 10;
                        IF p^.FType = CVAL_Eval THEN
                          BEGIN

                            PT := AllocStackReg(p^.frestype);
                            IF NOT WriteCalculation(p, PT) THEN
                              BEGIN
                                DisposeStackReg(PT);
                                Result := False;
                                exit;
                              END; {if}
                            IF GetTypeNo(OutReg) <> at2ut(FBooleanType) THEN
                              BEGIN
                                PT2 := AllocStackReg(at2ut(FBooleanType));
                              END
                            ELSE
                              PT2 := OutReg;
                            PreWriteOutRec(OutReg, Cardinal(-1));
                            IF tmpcalc <> NIL THEN PreWriteOutRec(Tmpcalc, Cardinal(-1));
                            WriteCommand(CM_CO);
                            WriteByte(C);
                            IF (pt2 = OutReg) THEN
                              BEGIN
                                IF NOT WriteOutRec(OutReg, False) THEN
                                  BEGIN
                                    MakeError('', ecInternalError, '00007');
                                    Result := False;
                                    exit;
                                  END; {if}
                              END
                            ELSE
                              BEGIN
                                IF NOT WriteOutRec(pt2, False) THEN
                                  BEGIN
                                    MakeError('', ecInternalError, '00007');
                                    Result := False;
                                    exit;
                                  END; {if}
                              END;
                            IF tmpcalc <> NIL THEN
                              BEGIN
                                IF NOT WriteOutRec(tmpcalc, True) THEN
                                  BEGIN
                                    MakeError('', ecInternalError, '00008');
                                    Result := False;
                                    exit;
                                  END; {if}
                              END ELSE BEGIN
                                IF NOT WriteOutRec(OutReg, False) THEN
                                  BEGIN
                                    MakeError('', ecInternalError, '00008');
                                    Result := False;
                                    exit;
                                  END; {if}
                              END;
                            IF NOT WriteOutRec(PT, True) THEN
                              BEGIN
                                MakeError('', ecInternalError, '00009');
                                Result := False;
                                exit;
                              END; {if}
                            IF tmpcalc <> NIL THEN BEGIN
                                AfterWriteOutRec(Tmpcalc);
                                tmpcalc := NIL;
                              END;
                            AfterWriteOutRec(OutReg);
                            DisposeStackReg(PT);
                            IF pt2 <> OutReg THEN
                              BEGIN
                                IF (OutReg^.FType <> CVAL_Addr) OR (OutReg^.Address <
                                  SCAddrNegativeStackStart) THEN
                                  BEGIN
                                    MakeError('', ecTypeMismatch, '')^.Position :=
                                      OutReg^.DPos;
                                    DisposeStackReg(PT);
                                    Result := False;
                                    exit;
                                  END;
                                PSCProcVar(proc^.ProcVars.GetItem(OutReg^.Address - 1 -
                                  SCAddrStackStart))^.VarType := GetType(btS32);
                                WriteCommand(Cm_ST); // set stack type
                                WriteLong(PSCProcVar(proc^.ProcVars.GetItem(OutReg^.Address
                                  - 1 - SCAddrStackStart))^.VarType);
                                WriteLong(OutReg^.Address - SCAddrStackStart);
                                WriteCommand(CM_A); // stack assignment
                                WriteCommand(CVAL_Addr);
                                WriteLong(OutReg^.Address);
                                IF NOT WriteOutRec(pt2, False) THEN
                                  BEGIN
                                    MakeError('', ecInternalError, '0000A');
                                    DisposeStackReg(PT);
                                    Result := False;
                                    exit;
                                  END;
                                DisposeStackReg(pt2);
                              END;

                          END
                        ELSE IF p^.FType = CVAL_Proc THEN
                          BEGIN
                            IF GetTypeNo(OutReg) <> at2ut(FBooleanType) THEN
                              BEGIN
                                PT2 := AllocStackReg(at2ut(FBooleanType));
                              END
                            ELSE
                              PT2 := OutReg;
                            IF PSCProcedure(FProcs.GetItem(p^.ProcNo))^.Internal THEN
                              PT := AllocStackReg(StrToIntDef(Fw(PSCProcedure(FProcs.GetItem(p^.ProcNo))^.Decl), -1))
                            ELSE
                              PT := AllocStackReg(StrToIntDef(Fw(PSCUSedRegProc(FProcs.GetItem(p^.ProcNo))^.rp^.Decl), -1));
                            IF NOT ProcessFunctionX(p^.Modifiers, p^.ProcNo, p^.Parameters, PT) THEN
                              BEGIN
                                Result := False;
                                exit;
                              END;
                            pt^.Modifiers := p^.modifiers;
                            WriteCalculation(pt, pt);
                            pt^.Modifiers := 0;
                            PreWriteOutRec(OutReg, Cardinal(-1));
                            IF tmpcalc <> NIL THEN PreWriteOutRec(tmpcalc, Cardinal(-1));
                            WriteCommand(CM_CO);
                            WriteByte(C);
                            IF pt2 = Outreg THEN
                              BEGIN
                                IF NOT WriteOutRec(OutReg, False) THEN
                                  BEGIN
                                    MakeError('', ecInternalError, '0000B');
                                    Result := False;
                                    exit;
                                  END; {if}
                              END
                            ELSE
                              BEGIN
                                IF NOT WriteOutRec(pt2, False) THEN
                                  BEGIN
                                    MakeError('', ecInternalError, '0000B');
                                    Result := False;
                                    exit;
                                  END; {if}
                              END;
                            IF tmpcalc <> NIL THEN
                              BEGIN
                                IF NOT WriteOutRec(tmpcalc, true) THEN
                                  BEGIN
                                    MakeError('', ecInternalError, '0000C');
                                    Result := False;
                                    exit;
                                  END; {if}
                              END ELSE BEGIN
                                IF NOT WriteOutRec(OutReg, False) THEN
                                  BEGIN
                                    MakeError('', ecInternalError, '0000C');
                                    Result := False;
                                    exit;
                                  END; {if}
                              END;
                            IF NOT WriteOutRec(PT, True) THEN
                              BEGIN
                                MakeError('', ecInternalError, '0000D');
                                Result := False;
                                exit;
                              END; {if}
                            IF TmpCalc <> NIL THEN
                              BEGIN
                                AfterWriteOutRec(TmpCalc);
                                tmpcalc := NIL;
                              END;
                            AfterWriteOutRec(OutReg);
                            DisposeStackReg(PT);
                            IF pt2 <> OutReg THEN
                              BEGIN
                                IF (OutReg^.FType <> CVAL_Addr) OR (OutReg^.Address <
                                  SCAddrNegativeStackStart) THEN
                                  BEGIN
                                    MakeError('', ecTypeMismatch, '')^.Position :=
                                      InData^.DPos;
                                    DisposeStackReg(pt2);
                                    Result := False;
                                    exit;
                                  END;
                                PSCProcVar(proc^.ProcVars.GetItem(OutReg^.Address - 1 -
                                  SCAddrStackStart))^.VarType := GetType(btS32);
                                WriteCommand(Cm_ST); // set stack type
                                WriteLong(PSCProcVar(proc^.ProcVars.GetItem(OutReg^.Address
                                  - 1
                                  - SCAddrStackStart))^.VarType);
                                WriteLong(OutReg^.Address - SCAddrStackStart);

                                WriteCommand(CM_A); // stack assignment
                                WriteCommand(CVAL_Addr);
                                WriteLong(OutReg^.Address);
                                IF NOT WriteOutRec(pt2, False) THEN
                                  BEGIN
                                    MakeError('', ecInternalError, '0000E');
                                    DisposeStackReg(pt2);
                                    Result := False;
                                    exit;
                                  END; {if}
                                DisposeStackReg(pt2);
                              END;
                          END
                        ELSE
                          BEGIN
                            IF GetTypeNo(OutReg) <> at2ut(FBooleanType) THEN
                              BEGIN
                                PT := AllocStackReg(at2ut(FBooleanType));
                              END
                            ELSE
                              PT := OutReg;
                            PreWriteOutRec(OutReg, Cardinal(-1));
                            PreWriteOutRec(P, GetTypeNo(Outreg));
                            IF TmpCalc <> NIL THEN PreWriteOutRec(tmpcalc, Cardinal(-1));

                            WriteCommand(CM_CO);
                            WriteData(C, 1);
                            IF Pt = OutReg THEN
                              BEGIN
                                IF NOT WriteOutRec(OutReg, False) THEN
                                  BEGIN
                                    MakeError('', ecInternalError, '0000F');
                                    Result := False;
                                    exit;
                                  END; {if}
                              END
                            ELSE
                              BEGIN
                                IF NOT WriteOutRec(PT, False) THEN
                                  BEGIN
                                    MakeError('', ecInternalError, '0000F');
                                    Result := False;
                                    exit;
                                  END; {if}
                              END;
                            IF tmpcalc <> NIL THEN
                              BEGIN
                                IF NOT WriteOutRec(tmpcalc, True) THEN
                                  BEGIN
                                    MakeError('', ecInternalError, '00010');
                                    DisposeStackReg(PT);
                                    Result := False;
                                    exit;
                                  END; {if}
                              END ELSE BEGIN
                                IF NOT WriteOutRec(OutReg, False) THEN
                                  BEGIN
                                    MakeError('', ecInternalError, '00010');
                                    DisposeStackReg(PT);
                                    Result := False;
                                    exit;
                                  END; {if}
                              END;
                            IF NOT WriteOutRec(p, True) THEN
                              BEGIN
                                MakeError('', ecInternalError, '00011');
                                DisposeStackReg(PT);
                                Result := False;
                                exit;
                              END; {case}
                            IF TmpCalc <> NIL THEN BEGIN
                                AfterWriteOutRec(tmpcalc);
                                tmpcalc := NIL;
                              END;
                            AfterWriteOutRec(P);
                            AfterWriteOutRec(OutReg);
                            IF PT <> OutReg THEN
                              BEGIN
                                IF (OutReg^.FType <> CVAL_Addr) OR (OutReg^.Address < SCAddrNegativeStackStart) THEN
                                  BEGIN
                                    MakeError('', ecTypeMismatch, '')^.Position :=
                                      InData^.DPos;
                                    DisposeStackReg(PT);
                                    Result := False;
                                    exit;
                                  END;
                                PSCProcVar(proc^.ProcVars.GetItem(OutReg^.Address - 1 -
                                  SCAddrStackStart))^.VarType := GetType(btS32);
                                WriteCommand(Cm_ST); // set stack type
                                WriteLong(PSCProcVar(proc^.ProcVars.GetItem(OutReg^.Address
                                  - 1
                                  - SCAddrStackStart))^.VarType);
                                WriteLong(OutReg^.Address - SCAddrStackStart);

                                WriteCommand(CM_A); // stack assignment
                                WriteCommand(CVAL_Addr);
                                WriteLong(OutReg^.Address);
                                IF NOT WriteOutRec(PT, False) THEN
                                  BEGIN
                                    MakeError('', ecInternalError, '00012');
                                    DisposeStackReg(PT);
                                    Result := False;
                                    exit;
                                  END; {if}
                                DisposeStackReg(PT);
                              END;
                          END; {else if}
                      END;
                  END; {for}
                l := outreg^.modifiers;
                OutReg^.Modifiers := outreg^.Modifiers OR bmodsave;
                WriteCalculation(OutReg, OutReg);
                outreg^.modifiers := l;
              END; {if}
          END
        ELSE
          BEGIN
            IF NOT SameReg(OutReg, InData) THEN
              BEGIN
                IF (indata^.FType <> CVAL_NIL) AND NOT CheckCompatType(OutReg, InData) THEN
                  BEGIN
                    MakeError('', ecTypeMismatch, '')^.Position := InData^.DPos;
                    Result := False;
                    exit;
                  END;
                IF NOT PreWriteOutRec(InData, GetTypeNo(Outreg)) THEN
                  BEGIN
                    Result := False;
                    exit;
                  END;
                IF NOT PreWriteOutRec(OutReg, Cardinal(-1)) THEN
                  BEGIN
                    Result := False;
                    Exit;
                  END;
                WriteCommand(CM_A);
                IF NOT WriteOutRec(OutReg, False) THEN
                  BEGIN
                    MakeError('', ecInternalError, '00013');
                    AfterWriteOutRec(OutReg);
                    AfterWriteOutRec(InData);
                    Result := False;
                    exit;
                  END; {if}
                IF NOT WriteOutRec(InData, True) THEN
                  BEGIN
                    MakeError('', ecInternalError, '00014');
                    AfterWriteOutRec(OutReg);
                    AfterWriteOutRec(InData);
                    Result := False;
                    exit;
                  END; {if}
                AfterWriteOutRec(OutReg);
                AfterWriteOutRec(InData);
              END ELSE IF InData^.Modifiers = 1 THEN BEGIN
                InData^.Modifiers := 0;
                PreWriteOutRec(InData, GetTypeNo(Outreg));
                WriteCommand(cm_bn);
                WriteOutRec(InData, False);
                AfterWriteOutRec(InData);
              END ELSE IF InData^.Modifiers = 2 THEN BEGIN
                InData^.Modifiers := 0;
                PreWriteOutRec(InData, GetTypeNo(Outreg));
                WriteCommand(cm_vm);
                WriteOutRec(InData, False);
                AfterWriteOutRec(InData);
              END;
          END; {if}
    Result := True;
  END; {WriteCalculation}

  FUNCTION ProcessFunctionX(ResModifiers: Byte; ProcNo: Cardinal; InData: TSCList;
    ResultRegister:
    PSCValue): Boolean;
  VAR
    res: STRING;
    Tmp: PParam;
    resreg: PSCValue;
    l: Longint;

    PROCEDURE CleanParams;
    VAR
      l: Longint;
      x: PSCValue;
    BEGIN
      FOR l := 0 TO InData.Count - 1 DO
        BEGIN
          x := PParam(InData.GetItem(l))^.OutReg;
          IF x <> NIL THEN
            BEGIN
              DisposeStackReg(x);
            END;
        END;
      IF resreg <> NIL THEN
        BEGIN
          IF Cardinal(StrTointDef(Res, -1)) <> GetTypeNo(resreg) THEN
            BEGIN
              ResultRegister^.Modifiers := ResModifiers;
              IF NOT WriteCalculation(ResultRegister, resreg) THEN
                BEGIN
                  Result := False;
                END;

              DisposeStackReg(ResultRegister);
            END ELSE DisposeStackReg(resreg);
        END;
    END;
  BEGIN
    IF PSCProcedure(FProcs.GetItem(ProcNo))^.Internal THEN
      res := PSCProcedure(FProcs.GetItem(ProcNo))^.Decl
    ELSE
      res := PSCUsedRegProc(FProcs.GetItem(ProcNo))^.RP^.Decl;
    IF Pos(' ', res) > 0 THEN
      res := copy(res, 1, Pos(' ', res) - 1);
    Result := False;
    IF (ResModifiers AND 8 <> 0) THEN
      BEGIN
        IF (ResultRegister = NIL) THEN
          BEGIN
            MakeError('', ecNoResult, '');
            Exit;
          END ELSE resreg := NIL;
      END ELSE
      IF (res = '-1') AND (ResultRegister <> NIL) THEN
        BEGIN
          MakeError('', ecNoResult, '');
          exit;
        END
      ELSE IF (res <> '-1') THEN
        BEGIN
          IF (ResultRegister = NIL) THEN
            BEGIN
              resreg := AllocStackReg(StrToInt(res));
              ResultRegister := resreg;
            END ELSE IF Cardinal(StrTointDef(Res, -1)) <> GetTypeNo(ResultRegister) THEN
            BEGIN
              resreg := ResultRegister;
              ResultRegister := AllocStackReg(StrToInt(res));
            END ELSE resreg := NIL;
        END
      ELSE
        resreg := NIL;

    FOR l := InData.Count - 1 DOWNTO 0 DO
      BEGIN
        Tmp := InData.GetItem(l);
        IF (Tmp^.InReg^.FType = CVAL_PushAddr) THEN
          BEGIN
            Tmp^.OutReg := AllocStackReg2(Tmp^.FType);
            PreWriteOutRec(Tmp^.InReg, Cardinal(-1));
            WriteCommand(CM_PV);
            WriteOutRec(Tmp^.InReg, False);
            AfterWriteOutRec(Tmp^.InReg);
          END
        ELSE
          BEGIN
            Tmp^.OutReg := AllocStackReg(Tmp^.FType);
            IF NOT WriteCalculation(Tmp^.InReg, Tmp^.OutReg) THEN
              BEGIN
                CleanParams;
                exit;
              END;
          END;
        DisposePValue(Tmp^.InReg);
        Tmp^.InReg := NIL;
      END; {for}
    IF (res <> '-1') OR (ResModifiers AND 8 <> 0) THEN
      BEGIN
        WriteCommand(CM_PV);
        IF NOT WriteOutRec(ResultRegister, False) THEN
          BEGIN
            CleanParams;
            MakeError('', ecInternalError, '00015');
            exit;
          END;
      END;
    WriteCommand(Cm_C);
    WriteLong(ProcNo);
    IF (res <> '-1') OR (ResModifiers AND 8 <> 0) THEN
      WriteCommand(CM_PO);
    Result := True;
    CleanParams;
  END; {ProcessFunction}

  FUNCTION ProcessVarFunction(ResModifiers: Byte; ProcNo: PSCValue; InData: TSCList;
    ResultRegister: PSCValue): Boolean;
  VAR
    res: STRING;
    Tmp: PParam;
    resreg: PSCValue;
    l: Longint;

    PROCEDURE CleanParams;
    VAR
      l: Longint;
      x: PSCValue;
    BEGIN
      FOR l := 0 TO InData.Count - 1 DO
        BEGIN
          x := PParam(InData.GetItem(l))^.OutReg;
          IF x <> NIL THEN
            BEGIN
              DisposeStackReg(x);
            END;
        END;
      AfterWriteOutRec(ProcNo);
      IF resreg <> NIL THEN
        BEGIN
          IF Cardinal(StrTointDef(Res, -1)) <> GetTypeNo(resreg) THEN
            BEGIN
              ResultRegister^.Modifiers := ResModifiers;
              WriteCalculation(ResultRegister, resreg);
              DisposeStackReg(ResultRegister);
            END ELSE DisposeStackReg(resreg);
        END;
    END;
  BEGIN
    res := PSCProceduralType(PSCType(FUsedTypes.GetItem(GetTypeNo(ProcNo)))^.Ext)^.ProcDef;
    IF Pos(' ', res) > 0 THEN
      res := copy(res, 1, Pos(' ', res) - 1);
    Result := False;
    IF (res = '-1') AND (ResultRegister <> NIL) THEN
      BEGIN
        MakeError('', ecNoResult, '');
        exit;
      END
    ELSE IF (res <> '-1') THEN
      BEGIN
        IF (ResultRegister = NIL) THEN
          BEGIN
            resreg := AllocStackReg(StrToInt(res));
            ResultRegister := resreg;
          END ELSE IF Cardinal(StrTointDef(Res, -1)) <> GetTypeNo(ResultRegister) THEN
          BEGIN
            resreg := ResultRegister;
            ResultRegister := AllocStackReg(StrToInt(res));
          END ELSE resreg := NIL;
      END
    ELSE
      resreg := NIL;

    PreWriteOutRec(ProcNo, Cardinal(-1));
    FOR l := InData.Count - 1 DOWNTO 0 DO
      BEGIN
        Tmp := InData.GetItem(l);
        IF (Tmp^.InReg^.FType = CVAL_PushAddr) THEN
          BEGIN
            Tmp^.OutReg := AllocStackReg2(Tmp^.FType);
            PreWriteOutRec(Tmp^.InReg, Cardinal(-1));
            WriteCommand(CM_PV);
            WriteOutRec(Tmp^.InReg, False);
            AfterWriteOutRec(Tmp^.InReg);
          END
        ELSE
          BEGIN
            Tmp^.OutReg := AllocStackReg(Tmp^.FType);
            IF NOT WriteCalculation(Tmp^.InReg, Tmp^.OutReg) THEN
              BEGIN
                CleanParams;
                exit;
              END;
          END;
        DisposePValue(Tmp^.InReg);
        Tmp^.InReg := NIL;
      END; {for}
    IF res <> '-1' THEN
      BEGIN
        WriteCommand(CM_PV);
        IF NOT WriteOutRec(ResultRegister, False) THEN
          BEGIN
            CleanParams;
            MakeError('', ecInternalError, '00015');
            exit;
          END;
      END;
    WriteCommand(Cm_cv);
    WriteOutRec(ProcNo, True);
    IF res <> '-1' THEN
      WriteCommand(CM_PO);
    Result := True;
    CleanParams;
  END; {ProcessVarFunction}

  FUNCTION HasInvalidJumps(StartPos, EndPos: Cardinal): Boolean;
  VAR
    I, J: Longint;
    Ok: LongBool;
    FLabelsInBlock: TSCStringList;
    s: STRING;
  BEGIN
    FLabelsInBlock := TSCStringList.Create;
    FOR i := 0 TO Proc^.FLabels.Count - 1 DO
      BEGIN
        s := Proc^.FLabels.GetItem(I);
        IF (Cardinal((@s[1])^) >= StartPos) AND (Cardinal((@s[1])^) <= EndPos) THEN
          BEGIN
            Delete(s, 1, 8);
            FLabelsInBlock.Add(s);
          END;
      END;
    FOR i := 0 TO Proc^.FGotos.Count - 1 DO
      BEGIN
        s := Proc^.FGotos.GetItem(I);
        IF (Cardinal((@s[1])^) >= StartPos) AND (Cardinal((@s[1])^) <= EndPos) THEN
          BEGIN
            Delete(s, 1, 8);
            OK := False;
            FOR J := 0 TO FLabelsInBlock.Count - 1 DO
              BEGIN
                IF FLabelsInBlock.GetItem(J) = s THEN
                  BEGIN
                    Ok := True;
                    Break;
                  END;
              END;
            IF NOT Ok THEN
              BEGIN
                MakeError('', ecInvalidJump, '');
                Result := True;
                FLabelsInBlock.Free;
                exit;
              END;
          END ELSE BEGIN
            Delete(s, 1, 4);
            OK := True;
            FOR J := 0 TO FLabelsInBlock.Count - 1 DO
              BEGIN
                IF FLabelsInBlock.GetItem(J) = s THEN
                  BEGIN
                    Ok := False;
                    Break;
                  END;
              END;
            IF NOT Ok THEN
              BEGIN
                MakeError('', ecInvalidJump, '');
                Result := True;
                FLabelsInBlock.Free;
                exit;
              END;
          END;
      END;
    FLabelsInBlock.Free;
    Result := False;
  END;

  FUNCTION ProcessFor: Boolean;
    { Process a for x := y to z do }
  VAR
    VVar: PSCValue;
    TempVar,
      InitialVal,
      finalVal: PSCValue;
    Backwards: Boolean;
    FPos, NPos, EPos, RPos: Longint;
    OldCO, OldBO: TSCList;
    I: Longint;
  BEGIN
    Debug_WriteLine;
    Result := False;
    FParser.Next;
    IF FParser.CurrTokenId <> CSTI_Identifier THEN
      BEGIN
        MakeError('', ecIdentifierExpected, '');
        exit;
      END;
    VVar := GetIdentifier(1);
    IF VVar = NIL THEN
      exit;
    CASE PSCType(FUsedTypes.GetItem(GetTypeNo(VVar)))^.BaseType OF
      btU8, btS8, btU16, btS16, btU32, btS32: ;
    ELSE
      BEGIN
        MakeError('', ecTypeMismatch, '');
        DisposePValue(VVar);
        exit;
      END;
    END;
    IF FParser.CurrTokenId <> CSTI_Assignment THEN
      BEGIN
        MakeError('', ecAssignmentExpected, '');
        DisposePValue(VVar);
        exit;
      END;
    FParser.Next;
    InitialVal := calc(CSTII_DownTo);
    IF InitialVal = NIL THEN
      BEGIN
        DisposePValue(VVar);
        exit;
      END;
    IF FParser.CurrTokenId = CSTII_To THEN
      Backwards := False
    ELSE IF FParser.CurrTokenId = CSTII_DownTo THEN
      Backwards := True
    ELSE
      BEGIN
        MakeError('', ecToExpected, '');
        DisposePValue(VVar);
        DisposePValue(InitialVal);
        exit;
      END;
    FParser.Next;
    finalVal := calc(CSTII_do);
    IF finalVal = NIL THEN
      BEGIN
        DisposePValue(VVar);
        DisposePValue(InitialVal);
        exit;
      END;
    IF FParser.CurrTokenId <> CSTII_do THEN
      BEGIN
        MakeError('', ecDoExpected, '');
        DisposePValue(VVar);
        DisposePValue(InitialVal);
        DisposePValue(finalVal);
        exit;
      END;
    FParser.Next;
    IF NOT WriteCalculation(InitialVal, VVar) THEN
      BEGIN
        DisposePValue(VVar);
        DisposePValue(InitialVal);
        DisposePValue(finalVal);
        exit;
      END;
    DisposePValue(InitialVal);
    TempVar := AllocStackReg(at2ut(FBooleanType));
    NPos := Length(proc^.Data);
    PreWriteOutRec(VVar, Cardinal(-1));
    PreWriteOutRec(finalVal, Cardinal(-1));
    WriteCommand(CM_CO);
    IF Backwards THEN
      BEGIN
        WriteByte(0); { >= }
      END
    ELSE
      BEGIN
        WriteByte(1); { <= }
      END;
    IF NOT WriteOutRec(TempVar, False) THEN
      BEGIN
        DisposePValue(TempVar);
        DisposePValue(VVar);
        DisposePValue(finalVal);
        exit;
      END;
    WriteOutRec(VVar, False);
    WriteOutRec(finalVal, True);
    AfterWriteOutRec(finalVal);
    AfterWriteOutRec(VVar);
    WriteCommand(Cm_CNG);
    EPos := Length(proc^.Data);
    WriteLong($12345678);
    WriteOutRec(TempVar, False);
    RPos := Length(proc^.Data);
    OldCO := FContinueOffsets;
    FContinueOffsets := TSCList.Create;
    OldBO := FBreakOffsets;
    FBreakOffsets := TSCList.Create;
    IF NOT ProcessSub(tOneliner, ProcNo, proc) THEN
      BEGIN
        DisposePValue(TempVar);
        DisposePValue(VVar);
        DisposePValue(finalVal);
        FBreakOffsets.Free;
        FContinueOffsets.Free;
        FContinueOffsets := OldCO;
        FBreakOffsets := OldBo;
        exit;
      END;
    New(InitialVal);
    InitialVal^.FType := CVAL_Data;
    New(InitialVal^.FData);
    InitialVal^.FData^.FType := GetTypeNo(VVar);
    CASE PSCType(FUsedTypes.GetItem(InitialVal^.FData^.FType))^.BaseType
      OF
      btU8, btS8: InitialVal^.FData^.Value := #1;
      btU16, btS16: InitialVal^.FData^.Value := #1#0;
      btU32, btS32: InitialVal^.FData^.Value := #1#0#0#0;
    ELSE
      BEGIN
        MakeError('', ecInternalError, '00019');
        DisposePValue(TempVar);
        DisposePValue(VVar);
        DisposePValue(finalVal);
        DisposePValue(InitialVal);
        FBreakOffsets.Free;
        FContinueOffsets.Free;
        FContinueOffsets := OldCO;
        FBreakOffsets := OldBo;
        exit;
      END;
    END;
    FPos := Length(Proc^.Data);
    PreWriteOutRec(InitialVal, Cardinal(-1));
    PreWriteOutRec(VVar, Cardinal(-1));
    WriteCommand(CM_CA);
    IF Backwards THEN
      WriteByte(1) {-}
    ELSE
      WriteByte(0); {+}
    WriteOutRec(VVar, False);
    WriteOutRec(InitialVal, True);
    AfterWriteOutRec(VVar);
    AfterWriteOutRec(InitialVal);
    DisposePValue(InitialVal);
    WriteCommand(Cm_G);
    WriteLong(Longint(NPos - Length(proc^.Data) - 4));
    Longint((@proc^.Data[EPos + 1])^) := Length(proc^.Data) - RPos;
    FOR i := 0 TO FBreakOffsets.Count - 1 DO
      BEGIN
        EPos := Cardinal(FBreakOffsets.GetItem(I));
        Longint((@proc^.Data[EPos - 3])^) := Length(proc^.Data) - Longint(EPos);
      END;
    FOR i := 0 TO FContinueOffsets.Count - 1 DO
      BEGIN
        EPos := Cardinal(FContinueOffsets.GetItem(I));
        Longint((@proc^.Data[EPos - 3])^) := Longint(FPos) - Longint(EPos);
      END;
    FBreakOffsets.Free;
    FContinueOffsets.Free;
    FContinueOffsets := OldCO;
    FBreakOffsets := OldBo;
    DisposeStackReg(TempVar);
    DisposePValue(VVar);
    DisposePValue(finalVal);
    IF HasInvalidJumps(RPos, Length(Proc^.Data)) THEN
      BEGIN
        Result := False;
        exit;
      END;
    Result := True;
  END; {ProcessFor}

  FUNCTION ProcessWhile: Boolean;
  VAR
    vin, vout: PSCValue;
    SPos, EPos: Cardinal;
    OldCo, OldBO: TSCList;
    I: Longint;
  BEGIN
    Result := False;
    Debug_WriteLine;
    FParser.Next;
    vout := calc(CSTII_do);
    IF vout = NIL THEN
      exit;
    IF FParser.CurrTokenId <> CSTII_do THEN
      BEGIN
        DisposePValue(vout);
        MakeError('', ecDoExpected, '');
        exit;
      END;
    vin := AllocStackReg(at2ut(FBooleanType));
    SPos := Length(proc^.Data); // start position
    OldCo := FContinueOffsets;
    FContinueOffsets := TSCList.Create;
    OldBO := FBreakOffsets;
    FBreakOffsets := TSCList.Create;
    IF NOT WriteCalculation(vout, vin) THEN
      BEGIN
        DisposePValue(vout);
        DisposeStackReg(vin);
        FBreakOffsets.Free;
        FContinueOffsets.Free;
        FContinueOffsets := OldCO;
        FBreakOffsets := OldBo;
        exit;
      END;
    DisposePValue(vout);
    FParser.Next; // skip DO
    WriteCommand(Cm_CNG); // only goto if expression is false
    WriteLong($12345678);
    EPos := Length(proc^.Data);
    IF NOT WriteOutRec(vin, False) THEN
      BEGIN
        MakeError('', ecInternalError, '00017');
        DisposeStackReg(vin);
        FBreakOffsets.Free;
        FContinueOffsets.Free;
        FContinueOffsets := OldCO;
        FBreakOffsets := OldBo;
        exit;
      END;
    IF NOT ProcessSub(tOneliner, ProcNo, proc) THEN
      BEGIN
        DisposeStackReg(vin);
        FBreakOffsets.Free;
        FContinueOffsets.Free;
        FContinueOffsets := OldCO;
        FBreakOffsets := OldBo;
        exit;
      END;
    Debug_WriteLine;
    WriteCommand(Cm_G);
    WriteLong(Longint(SPos) - Length(proc^.Data) - 4);
    Longint((@proc^.Data[EPos - 3])^) := Length(proc^.Data) - Longint(EPos) - 5;
    FOR i := 0 TO FBreakOffsets.Count - 1 DO
      BEGIN
        EPos := Cardinal(FBreakOffsets.GetItem(I));
        Longint((@proc^.Data[EPos - 3])^) := Length(proc^.Data) - Longint(EPos);
      END;
    FOR i := 0 TO FContinueOffsets.Count - 1 DO
      BEGIN
        EPos := Cardinal(FContinueOffsets.GetItem(I));
        Longint((@proc^.Data[EPos - 3])^) := Longint(SPos) - Longint(EPos);
      END;
    FBreakOffsets.Free;
    FContinueOffsets.Free;
    FContinueOffsets := OldCO;
    FBreakOffsets := OldBo;
    DisposeStackReg(vin);
    IF HasInvalidJumps(EPos, Length(Proc^.Data)) THEN
      BEGIN
        Result := False;
        exit;
      END;
    Result := True;
  END;

  FUNCTION ProcessRepeat: Boolean;
  VAR
    vin, vout: PSCValue;
    SPos, EPos: Cardinal;
    I: Longint;
    OldCo, OldBO: TSCList;
  BEGIN
    Result := False;
    Debug_WriteLine;
    FParser.Next;
    OldCo := FContinueOffsets;
    FContinueOffsets := TSCList.Create;
    OldBO := FBreakOffsets;
    FBreakOffsets := TSCList.Create;
    vin := AllocStackReg(at2ut(FBooleanType));
    SPos := Length(proc^.Data);
    IF NOT ProcessSub(tRepeat, ProcNo, proc) THEN
      BEGIN
        FBreakOffsets.Free;
        FContinueOffsets.Free;
        FContinueOffsets := OldCO;
        FBreakOffsets := OldBo;
        DisposeStackReg(vin);
        exit;
      END;
    FParser.Next; //cstii_until
    vout := calc(CSTI_Semicolon);
    IF vout = NIL THEN
      BEGIN
        FBreakOffsets.Free;
        FContinueOffsets.Free;
        FContinueOffsets := OldCO;
        FBreakOffsets := OldBo;
        DisposeStackReg(vin);
        exit;
      END;
    IF NOT WriteCalculation(vout, vin) THEN
      BEGIN
        DisposePValue(vout);
        DisposeStackReg(vin);
        FBreakOffsets.Free;
        FContinueOffsets.Free;
        FContinueOffsets := OldCO;
        FBreakOffsets := OldBo;
        exit;
      END;
    DisposePValue(vout);
    WriteCommand(Cm_CNG);
    WriteLong($12345678);
    EPos := Length(proc^.Data);
    IF NOT WriteOutRec(vin, False) THEN
      BEGIN
        MakeError('', ecInternalError, '00016');
        DisposeStackReg(vin);
        FBreakOffsets.Free;
        FContinueOffsets.Free;
        FContinueOffsets := OldCO;
        FBreakOffsets := OldBo;
        exit;
      END;
    Longint((@proc^.Data[EPos - 3])^) := Longint(SPos) -
      Length(proc^.Data);
    FOR i := 0 TO FBreakOffsets.Count - 1 DO
      BEGIN
        EPos := Cardinal(FBreakOffsets.GetItem(I));
        Longint((@proc^.Data[EPos - 3])^) := Length(proc^.Data) - Longint(EPos);
      END;
    FOR i := 0 TO FContinueOffsets.Count - 1 DO
      BEGIN
        EPos := Cardinal(FContinueOffsets.GetItem(I));
        Longint((@proc^.Data[EPos - 3])^) := Longint(SPos) - Longint(EPos);
      END;
    FBreakOffsets.Free;
    FContinueOffsets.Free;
    FContinueOffsets := OldCO;
    FBreakOffsets := OldBo;
    DisposeStackReg(vin);
    IF HasInvalidJumps(SPos, Length(Proc^.Data)) THEN
      BEGIN
        Result := False;
        exit;
      END;
    Result := True;
  END; {ProcessRepeat}

  FUNCTION ProcessIf: Boolean;
  VAR
    vout, vin: PSCValue;
    SPos, EPos: Cardinal;
  BEGIN
    Result := False;
    Debug_WriteLine;
    FParser.Next;
    vout := calc(CSTII_Then);
    IF vout = NIL THEN
      exit;
    IF FParser.CurrTokenId <> CSTII_Then THEN
      BEGIN
        DisposePValue(vout);
        MakeError('', ecThenExpected, '');
        exit;
      END;
    vin := AllocStackReg(at2ut(FBooleanType));
    IF NOT WriteCalculation(vout, vin) THEN
      BEGIN
        DisposePValue(vout);
        DisposeStackReg(vin);
        exit;
      END;
    DisposePValue(vout);
    WriteCommand(cm_sf);
    IF NOT WriteOutRec(vin, False) THEN
      BEGIN
        MakeError('', ecInternalError, '00018');
        DisposeStackReg(vin);
        exit;
      END;
    WriteByte(1);
    DisposeStackReg(vin);
    WriteCommand(cm_fg);
    WriteLong($12345678);
    SPos := Length(proc^.Data);
    FParser.Next; // skip then
    IF NOT ProcessSub(tifOneliner, Procno, proc) THEN
      BEGIN
        exit;
      END;
    IF FParser.CurrTokenId = CSTII_Else THEN
      BEGIN
        WriteCommand(Cm_G);
        WriteLong($12345678);
        EPos := Length(proc^.Data);
        Longint((@proc^.Data[SPos - 3])^) := Length(proc^.Data) -
          Longint(SPos);
        FParser.Next;
        IF NOT ProcessSub(tOneliner, ProcNo, proc) THEN
          BEGIN
            exit;
          END;
        Longint((@proc^.Data[EPos - 3])^) := Length(proc^.Data) - Longint(EPos);
      END
    ELSE
      BEGIN
        Longint((@proc^.Data[SPos - 3])^) := Length(proc^.Data) -
          Longint(SPos) + 5
          - 5;
      END;
    Result := True;
  END; {ProcessIf}

  FUNCTION ProcessLabelX: Longint; {0 = failed; 1 = successful; 2 = no label}
  VAR
    I, H: Longint;
    s: STRING;
  BEGIN
    h := MakeHash(FParser.GetToken);
    FOR i := 0 TO Proc^.FLabels.Count - 1 DO
      BEGIN
        s := proc^.FLabels.GetItem(I);
        delete(s, 1, 4);
        IF Longint((@s[1])^) = h THEN
          BEGIN
            delete(s, 1, 4);
            IF s = FParser.GetToken THEN
              BEGIN
                s := proc^.FLabels.GetItem(I);
                Cardinal((@s[1])^) := Length(Proc^.Data);
                Proc^.FLabels.SetItem(i, s);
                FParser.Next;
                IF fParser.CurrTokenId = CSTI_Colon THEN
                  BEGIN
                    Result := 1;
                    FParser.Next;
                    exit;
                  END ELSE BEGIN
                    MakeError('', ecColonExpected, '');
                    Result := 0;
                    Exit;
                  END;
              END;
          END;
      END;
    result := 2;
  END;

  FUNCTION ProcessIdentifier: Boolean;
  VAR
    vin, vout: PSCValue;
  BEGIN
    Result := False;
    Debug_WriteLine;
    vin := GetIdentifier(2);
    IF vin <> NIL THEN
      BEGIN
        IF vin^.FType < CVAL_Proc THEN
          BEGIN // assignment needed
            IF FParser.CurrTokenId <> CSTI_Assignment THEN
              BEGIN
                MakeError('', ecAssignmentExpected, '');
                DisposePValue(vin);
                exit;
              END;
            FParser.Next;
            vout := calc(CSTI_Semicolon);
            IF vout = NIL THEN
              BEGIN
                DisposePValue(vin);
                exit;
              END;
            IF NOT WriteCalculation(vout, vin) THEN
              BEGIN
                DisposePValue(vin);
                DisposePValue(vout);
                exit;
              END;
            DisposePValue(vin);
            DisposePValue(vout);
          END
        ELSE IF vin^.FType = CVAL_VarProc THEN
          BEGIN
            Result := ProcessVarFunction(0, Vin^._ProcNo, vin^._Parameters, NIL);
            DisposePValue(vin);
            Exit;
          END ELSE
          BEGIN
            Result := ProcessFunctionX(0, vin^.ProcNo, vin^.Parameters, NIL);
            DisposePValue(vin);
            exit;
          END;
      END
    ELSE
      BEGIN
        Result := False;
        exit;
      END;
    Result := True;
  END; {ProcessIdentifier}

  FUNCTION ProcessCase: Boolean;
  VAR
    TempRec, CV, Val, CalcItem: PSCValue;
    p: PCalc_Item;
    SPos, CurrP: Cardinal;
    I: Longint;
    EndReloc: TSCList;
  BEGIN
    Debug_WriteLine;
    FParser.Next;
    Val := calc(CSTII_of);
    IF Val = NIL THEN
      BEGIN
        ProcessCase := False;
        exit;
      END; {if}
    IF FParser.CurrTokenId <> CSTII_Of THEN
      BEGIN
        MakeError('', ecOfExpected, '');
        DisposePValue(Val);
        ProcessCase := False;
        exit;
      END; {if}
    FParser.Next;
    TempRec := AllocStackReg(GetTypeNo(Val));
    IF NOT WriteCalculation(Val, TempRec) THEN
      BEGIN
        DisposeStackReg(TempRec);
        DisposePValue(Val);
        ProcessCase := False;
        exit;
      END; {if}
    DisposePValue(Val);
    EndReloc := TSCList.Create;
    CalcItem := AllocStackReg(at2ut(FBooleanType));
    SPos := Length(Proc^.Data);
    REPEAT
      Val := calc(CSTI_Colon);
      IF (Val = NIL) OR (FParser.CurrTokenID <> CSTI_Colon) THEN
        BEGIN
          IF FParser.CurrTokenID <> CSTI_Colon THEN
            MakeError('', ecColonExpected, '');
          DisposeStackReg(CalcItem);
          DisposeStackReg(TempRec);
          EndReloc.Free;
          ProcessCase := False;
          exit;
        END; {if}
      FParser.Next;
      New(cv);
      cv^.DPos := FParser.CurrTokenPos;
      cv^.FType := CVAL_Eval;
      cv^.SubItems := TSCList.Create;
      cv^.Modifiers := 0;
      new(p);
      p^.C := False;
      p^.OutRec := Val;
      cv^.SubItems.Add(p);
      new(p);
      p^.C := True;
      p^.calcCmd := 15;
      cv^.SubItems.Add(p);
      new(p);
      p^.C := False;
      p^.OutRec := TempRec;
      cv^.SubItems.Add(p);
      IF NOT WriteCalculation(CV, CalcItem) THEN
        BEGIN
          DisposeStackReg(CalcItem);
          DisposePValue(CV);
          EndReloc.Free;
          ProcessCase := False;
          exit;
        END;
      Cv.SubItems.Delete(2);
      Dispose(p);
      DisposePValue(CV);
      WriteByte(Cm_CNG);
      WriteLong($12345678);
      CurrP := Length(Proc^.Data);
      WriteOutRec(CalcItem, False);
      IF NOT ProcessSub(tifOneliner, Procno, proc) THEN
        BEGIN
          DisposeStackReg(CalcItem);
          DisposeStackReg(TempRec);
          EndReloc.Free;
          ProcessCase := False;
          exit;
        END;
      WriteByte(Cm_G);
      WriteLong($12345678);
      EndReloc.Add(Pointer(Length(Proc^.Data)));
      Cardinal((@Proc^.Data[CurrP - 3])^) := Cardinal(Length(Proc^.Data)) - CurrP - 5;
      IF FParser.CurrTokenID = CSTI_Semicolon THEN FParser.Next;
      IF FParser.CurrTokenID = CSTII_Else THEN
        BEGIN
          FParser.Next;
          IF NOT ProcessSub(tOneliner, Procno, proc) THEN
            BEGIN
              DisposeStackReg(CalcItem);
              DisposeStackReg(TempRec);
              EndReloc.Free;
              ProcessCase := False;
              exit;
            END;
          IF FParser.CurrTokenID = CSTI_Semicolon THEN FParser.Next;
          IF FParser.CurrtokenId <> CSTII_End THEN
            BEGIN
              MakeError('', ecEndExpected, '');
              DisposeStackReg(CalcItem);
              DisposeStackReg(TempRec);
              EndReloc.Free;
              ProcessCase := False;
              exit;
            END;
        END;
    UNTIL FParser.CurrTokenID = CSTII_End;
    FParser.Next;
    FOR i := 0 TO EndReloc.Count - 1 DO
      BEGIN
        Cardinal((@Proc^.Data[Cardinal(EndReloc.GetItem(I)) - 3])^) := Cardinal(Length(Proc^.Data)) - Cardinal(EndReloc.GetItem(I));
      END;
    DisposeStackReg(CalcItem);
    DisposeStackReg(TempRec);
    EndReloc.Free;
    IF HasInvalidJumps(SPos, Length(Proc^.Data)) THEN
      BEGIN
        Result := False;
        exit;
      END;
    Result := True;
  END; {ProcessCase}
  FUNCTION ProcessGoto: Boolean;
  VAR
    I, H: Longint;
    s: STRING;
  BEGIN
    Debug_WriteLine;
    FParser.Next;
    h := MakeHash(FParser.GetToken);
    FOR i := 0 TO Proc^.FLabels.Count - 1 DO
      BEGIN
        s := proc^.FLabels.GetItem(I);
        delete(s, 1, 4);
        IF Longint((@s[1])^) = h THEN
          BEGIN
            delete(s, 1, 4);
            IF s = FParser.GetToken THEN
              BEGIN
                FParser.Next;
                WriteCommand(Cm_G);
                WriteLong($12345678);
                Proc^.FGotos.Add(mi2s(length(Proc^.Data)) + mi2s(i));
                Result := True;
                exit;
              END;
          END;
      END;
    MakeError('', ecUnknownIdentifier, FParser.OriginalToken);
    Result := False;
  END; {ProcessGoto}
  FUNCTION ProcessTry: Boolean;
  VAR
    FStartOffset: Cardinal;
  BEGIN
    FParser.Next;
    WriteCommand(cm_puexh);
    FStartOffset := Length(Proc^.Data) + 1;
    WriteLong(Cardinal(-1));
    WriteLong(Cardinal(-1));
    WriteLong(Cardinal(-1));
    WriteLong(Cardinal(-1));
    IF ProcessSub(tTry, ProcNo, Proc) THEN
      BEGIN
        WriteCommand(cmd_poexh);
        WriteByte(0);
        IF FParser.CurrTokenID = CSTII_Except THEN
          BEGIN
            FParser.Next;
            Cardinal((@Proc^.Data[FStartOffset + 4])^) := Cardinal(Length(Proc^.Data)) - FStartOffset - 15;
            IF ProcessSub(tTryEnd, ProcNo, Proc) THEN
              BEGIN
                WriteCommand(cmd_poexh);
                writeByte(2);
                IF FParser.CurrTokenId = CSTII_Finally THEN
                  BEGIN
                    Cardinal((@Proc^.Data[FStartOffset + 8])^) := Cardinal(Length(Proc^.Data)) - FStartOffset - 15;
                    FParser.Next;
                    IF ProcessSub(tTryEnd, ProcNo, Proc) THEN
                      BEGIN
                        IF FParser.CurrTokenId = CSTII_End THEN
                          BEGIN
                            WriteCommand(cmd_poexh);
                            writeByte(3);
                          END ELSE BEGIN
                            MakeError('', ecEndExpected, '');
                            Result := False;
                            exit;
                          END;
                      END ELSE BEGIN Result := False; exit;
                      END;
                  END ELSE IF FParser.CurrTokenID <> CSTII_End THEN
                  BEGIN
                    MakeError('', ecEndExpected, '');
                    Result := False;
                    exit;
                  END;
                FParser.Next;
              END ELSE BEGIN Result := False; exit;
              END;
          END ELSE IF FParser.CurrTokenId = CSTII_Finally THEN
          BEGIN
            FParser.Next;
            Cardinal((@Proc^.Data[FStartOffset])^) := Cardinal(Length(Proc^.Data)) - FStartOffset - 15;
            IF ProcessSub(tTryEnd, ProcNo, Proc) THEN
              BEGIN
                WriteCommand(cmd_poexh);
                writeByte(1);
                IF FParser.CurrTokenId = CSTII_Except THEN
                  BEGIN
                    Cardinal((@Proc^.Data[FStartOffset + 4])^) := Cardinal(Length(Proc^.Data)) - FStartOffset - 15;
                    FParser.Next;
                    IF ProcessSub(tTryEnd, ProcNo, Proc) THEN
                      BEGIN
                        IF FParser.CurrTokenId = CSTII_End THEN
                          BEGIN
                            WriteCommand(cmd_poexh);
                            writeByte(2);
                          END ELSE BEGIN
                            MakeError('', ecEndExpected, '');
                            Result := False;
                            exit;
                          END;
                      END ELSE BEGIN Result := False; exit;
                      END;
                  END ELSE IF FParser.CurrTokenID <> CSTII_End THEN
                  BEGIN
                    MakeError('', ecEndExpected, '');
                    Result := False;
                    exit;
                  END;
                FParser.Next;
              END ELSE BEGIN Result := False; exit;
              END;
          END;
      END ELSE BEGIN Result := False; exit;
      END;
    Cardinal((@Proc^.Data[FStartOffset + 12])^) := Cardinal(Length(Proc^.Data)) - FStartOffset - 15;
    Result := True;
  END; {ProcessTry}
BEGIN
  ProcessSub := False;
  IF (FType = tProcBegin) OR (FType = tMainBegin) OR (FType = tSubBegin) THEN
    BEGIN
      FParser.Next; // skip CSTII_Begin
    END;
  WHILE True DO
    BEGIN
      CASE FParser.CurrTokenId OF
        CSTII_break:
          BEGIN
            IF FBreakOffsets = NIL THEN
              BEGIN
                MakeError('', ecNotInLoop, '');
                exit;
              END;
            WriteCommand(Cm_G);
            WriteLong($12345678);
            FBreakOffsets.Add(Pointer(Length(Proc^.Data)));
            FParser.Next;
            IF (FType = tifOneliner) OR (FType = TOneLiner) THEN
              break;
          END;
        CSTII_Continue:
          BEGIN
            IF FBreakOffsets = NIL THEN
              BEGIN
                MakeError('', ecNotInLoop, '');
                exit;
              END;
            WriteCommand(Cm_G);
            WriteLong($12345678);
            FContinueOffsets.Add(Pointer(Length(Proc^.Data)));
            FParser.Next;
            IF (FType = tifOneliner) OR (FType = TOneLiner) THEN
              break;
          END;
        CSTII_Goto:
          BEGIN
            IF NOT ProcessGoto THEN
              Exit;
            IF (FType = tifOneliner) OR (FType = TOneLiner) THEN
              break;
          END;
        CSTII_Try:
          BEGIN
            IF NOT ProcessTry THEN
              Exit;
            IF (FType = tifOneliner) OR (FType = TOneLiner) THEN
              break;
          END;
        CSTII_Finally, CSTII_Except:
          BEGIN
            IF (FType = tTry) OR (FType = tTryEnd) THEN
              Break
            ELSE
              BEGIN
                MakeError('', ecEndExpected, '');
                Exit;
              END;
          END;
        CSTII_Begin:
          BEGIN
            IF NOT ProcessSub(tSubBegin, ProcNo, proc) THEN
              Exit;
            FParser.Next; // skip END
            IF (FType = tifOneliner) OR (FType = TOneLiner) THEN
              break;
          END;
        CSTI_Semicolon:
          BEGIN
            FParser.Next;
            IF (FType = tifOneliner) OR (FType = TOneLiner) THEN
              break;
          END;
        CSTII_until:
          BEGIN
            Debug_WriteLine;
            IF FType = tRepeat THEN
              BEGIN
                break;
              END
            ELSE
              BEGIN
                MakeError('', ecIdentifierExpected, '');
                exit;
              END;
            IF (FType = tifOneliner) OR (FType = TOneLiner) THEN
              break;
          END;
        CSTII_Else:
          BEGIN
            IF FType = tifOneliner THEN
              break
            ELSE
              BEGIN
                MakeError('', ecIdentifierExpected, '');
                exit;
              END;
          END;
        CSTII_repeat:
          BEGIN
            IF NOT ProcessRepeat THEN
              exit;
            IF (FType = tifOneliner) OR (FType = TOneLiner) THEN
              break;
          END;
        CSTII_For:
          BEGIN
            IF NOT ProcessFor THEN
              exit;
            IF (FType = tifOneliner) OR (FType = TOneLiner) THEN
              break;
          END;
        CSTII_While:
          BEGIN
            IF NOT ProcessWhile THEN
              exit;
            IF (FType = tifOneliner) OR (FType = TOneLiner) THEN
              break;
          END;
        CSTII_Exit:
          BEGIN
            Debug_WriteLine;
            WriteCommand(Cm_R);
            FParser.Next;
          END;
        CSTII_Case:
          BEGIN
            IF NOT ProcessCase THEN
              exit;
            IF (FType = tifOneliner) OR (FType = TOneLiner) THEN
              break;
          END;
        CSTII_If:
          BEGIN
            IF NOT ProcessIf THEN
              exit;
            IF (FType = tifOneliner) OR (FType = TOneLiner) THEN
              break;
          END;
        CSTI_Identifier:
          BEGIN
            CASE ProcessLabelX OF
              0: Exit;
              1: ;
            ELSE
              BEGIN
                IF NOT ProcessIdentifier THEN
                  exit;
                IF (FType = tifOneliner) OR (FType = TOneLiner) THEN
                  break;
              END;
            END; {case}
          END;
        CSTII_End:
          BEGIN
            IF (FType = tTryEnd) OR (FType = tMainBegin) OR (FType = tSubBegin) OR (FType =
              tifOneliner) OR (FType = tProcBegin) OR (FType = TOneLiner) THEN
              BEGIN
                break;
              END
            ELSE
              BEGIN
                MakeError('', ecIdentifierExpected, '');
                exit;
              END;
          END;
        CSTI_EOF:
          BEGIN
            MakeError('', ecUnexpectedEndOfFile, '');
            exit;
          END;
      ELSE
        BEGIN
          MakeError('', ecIdentifierExpected, '');
          exit;
        END;
      END;
    END;
  IF (FType = tMainBegin) OR (FType = tProcBegin) THEN
    BEGIN
      WriteCommand(Cm_R);
      FParser.Next; // skip end
      IF (FType = tMainBegin) AND (FParser.CurrTokenId <> CSTI_Period) THEN
        BEGIN
          MakeError('', ecPeriodExpected, '');
          exit;
        END;
      IF (FType = tProcBegin) AND (FParser.CurrTokenId <> CSTI_Semicolon) THEN
        BEGIN
          MakeError('', ecSemicolonExpected, '');
          exit;
        END;
      FParser.Next;
    END;
  ProcessSub := True;
END;


FUNCTION TSCPascalCompiler.ProcessLabelForwards(Proc: PSCProcedure): Boolean;
VAR
  i: Longint;
  s, s2: STRING;
BEGIN
  FOR i := 0 TO Proc^.FLabels.Count - 1 DO
    BEGIN
      s := Proc^.FLabels.GetItem(i);
      IF Longint((@s[1])^) = -1 THEN
        BEGIN
          delete(s, 1, 8);
          MakeError('', ecUnSetLabel, s);
          Result := False;
          exit;
        END;
    END;
  FOR i := proc^.FGotos.Count - 1 DOWNTO 0 DO
    BEGIN
      s := proc^.FGotos.GetItem(I);
      s2 := Proc^.FLabels.GetItem(Cardinal((@s[5])^));
      Cardinal((@Proc^.Data[Cardinal((@s[1])^) - 3])^) := Cardinal((@s2[1])^) - Cardinal((@s[1])^);
    END;
  Result := True;
END;



FUNCTION TSCPascalCompiler.Compile(CONST s: STRING): Boolean;
VAR
  Position: Byte;
  i: Longint;

  PROCEDURE FreeAll;
  VAR
    I, I2: Longint;
    PPV: PSCProcVar;
    PT: PSCType;
    pp: PSCProcedure;
    pc: PSCConstant;
    ppe: PSCUsedRegProc;
    pv: PSCVar;
    pr: PSCRegProc;
    pn: PSCProceduralType;

    PROCEDURE FreeRecord(v: TSCList);
    VAR
      I: Longint;
      p: PSCRecordType;
    BEGIN
      FOR I := 0 TO v.Count - 1 DO
        BEGIN
          p := v.GetItem(I);
          p^.FieldName := '';
          Dispose(p);
        END;
      v.Free;
    END;
  BEGIN
    FOR I := 0 TO FRegProcs.Count - 1 DO
      BEGIN
        pr := FRegProcs.GetItem(I);
        Dispose(pr);
      END;
    FRegProcs.Free;
    FOR i := 0 TO FConstants.Count - 1 DO
      BEGIN
        pc := FConstants.GetItem(I);
        Dispose(pc);
      END;
    Fconstants.Free;
    FOR I := 0 TO FVars.Count - 1 DO
      BEGIN
        pv := FVars.GetItem(I);
        Dispose(pv);
      END;
    FVars.Free;
    FOR I := 0 TO FProcs.Count - 1 DO
      BEGIN
        ppe := FProcs.GetItem(I);
        IF ppe^.Internal THEN
          BEGIN
            pp := Pointer(ppe);
            FOR I2 := 0 TO pp^.ProcVars.Count - 1 DO
              BEGIN
                PPV := pp^.ProcVars.GetItem(I2);
                Dispose(PPV);
              END;
            pp^.ProcVars.Free;
            pp^.FGotos.Free;
            pp^.FLabels.Free;
            Dispose(pp);
          END
        ELSE
          BEGIN
            Dispose(ppe);
          END;

      END;
    FProcs.Free;
    FProcs := NIL;
    FOR I := 0 TO FAvailableTypes.Count - 1 DO
      BEGIN
        PT := FAvailableTypes.GetItem(I);
        IF pt^.BaseType = btProcPtr THEN
          BEGIN
            pn := pt^.Ext;
            Dispose(pn);
          END ELSE IF PT^.BaseType = btRecord THEN
          FreeRecord(PT^.RecordSubVals)
        ELSE IF (pt^.BaseType = btClass) AND (pt^.Ext <> NIL) THEN
          BEGIN
            TSCExternalClass(pt^.Ext).Free;
          END;
        Dispose(PT);
      END;
    FAvailableTypes.Free;
    FUsedTypes.Free;
  END;



  PROCEDURE MakeOutput;

    PROCEDURE WriteByte(b: Byte);
    BEGIN
      FOutput := FOutput + Char(b);
    END;

    PROCEDURE WriteData(CONST Data; Len: Longint);
    BEGIN
      SetLength(FOutput, Length(FOutput) + Len);
      Move(Data, FOutput[Length(FOutput) - Len + 1], Len);
    END;

    PROCEDURE WriteLong(l: Cardinal);
    BEGIN
      WriteData(l, 4);
    END;


    PROCEDURE WriteTypes;
    VAR
      l, n: Longint;
      Tmp: Cardinal;
      x: PSCType;
      xxp: PSCProceduralType;
      FExportName: STRING;
    BEGIN
      FOR l := 0 TO FUsedTypes.Count - 1 DO
        BEGIN
          x := FUsedTypes.GetItem(l);
          IF x^.BaseType = btChar THEN x^.BaseType := btu8;
          IF x^.FExport THEN
            FExportName := x^.Name
          ELSE
            FExportName := '';
          IF x^.BaseType = btClass THEN
            BEGIN
              x := GetTypeCopyLink(FAvailableTypes.GetItem(TSCExternalClass(x^.Ext).SelfType));
            END;
          IF (x^.BaseType = btString) AND (x^.Ext = Pointer(1)) THEN x^.BaseType := btPChar;
          IF (x^.BaseType = btEnum) THEN BEGIN
              IF Longint(x^.Ext) <= 256 THEN
                x^.BaseType := btU8
              ELSE IF Longint(x^.Ext) <= 65536 THEN
                x^.BaseType := btU16
              ELSE
                x^.BaseType := btU32;
            END;
          IF x^.BaseType = btProcPtr THEN BEGIN
              xxp := x^.Ext;
              Dispose(xxp);
              x^.Ext := NIL;
              x^.BaseType := btu32;
            END;
          IF FExportName <> '' THEN
            BEGIN
              WriteByte(x^.BaseType + 128);
            END
          ELSE
            WriteByte(X^.BaseType);
          IF x^.BaseType = btArray THEN
            BEGIN
              WriteLong(Longint(x^.Ext));
            END
          ELSE IF x^.BaseType = btRecord THEN
            BEGIN
              n := x^.RecordSubVals.Count;
              WriteData(n, 4);
              FOR n := 0 TO x^.RecordSubVals.Count - 1 DO
                BEGIN
                  Tmp := PSCRecordType(x^.RecordSubVals.GetItem(n))^.FType;
                  WriteData(Tmp, 4);
                END;
            END;
          IF FExportName <> '' THEN
            BEGIN
              WriteLong(Length(FExportName));
              WriteData(FExportName[1], length(FExportName));
            END;
        END;
    END;

    PROCEDURE WriteVars;
    VAR
      l: Longint;
      x: PSCVar;
    BEGIN
      FOR l := 0 TO FVars.Count - 1 DO
        BEGIN
          x := FVars.GetItem(l);
          WriteLong(x^.FType);
          IF x^.exportname <> '' THEN
            BEGIN
              WriteByte(1);
              WriteLong(Length(X^.ExportName));
              WriteData(X^.ExportName[1], length(X^.ExportName));
            END ELSE
            WriteByte(0);
        END;
    END;

    PROCEDURE WriteProcs;
    VAR
      l: Longint;
      x: PSCProcedure;
      xp: PSCUsedRegProc;
      s: STRING;
    BEGIN
      FOR l := 0 TO FProcs.Count - 1 DO
        BEGIN
          x := FProcs.GetItem(l);
          IF x^.Internal THEN
            BEGIN
              x^.OutputDeclPosition := Length(FOutput);
              IF x^.FExport <> 0 THEN
                WriteByte(2) // exported
              ELSE
                WriteByte(0); // not imported
              WriteLong(0); // offset is unknown at this time
              WriteLong(0); // length is also unknown at this time
              IF x^.FExport <> 0 THEN
                BEGIN
                  WriteLong(Length(x^.Name));
                  WriteData(x^.Name[1], length(x^.Name));
                  IF x^.FExport = 1 THEN
                    BEGIN
                      WriteLong(0);
                    END ELSE BEGIN
                      s := MakeExportDecl(x^.Decl);
                      WriteLong(Length(s));
                      WriteData(s[1], length(S));
                    END;
                END;
            END
          ELSE
            BEGIN
              xp := Pointer(x);
              IF xp^.RP^.ImportDecl <> '' THEN
                BEGIN
                  WriteByte(3); // imported
                  IF xp^.Rp^.FExportName THEN
                    BEGIN
                      WriteByte(Length(xp^.RP^.Name));
                      WriteData(xp^.RP^.Name[1], Length(xp^.RP^.Name) AND $FF);
                    END ELSE BEGIN
                      WriteByte(0);
                    END;
                  WriteLong(Length(xp^.RP^.ImportDecl));
                  WriteData(xp^.RP^.ImportDecl[1], Length(xp^.RP^.ImportDecl));
                END ELSE BEGIN
                  WriteByte(1); // imported
                  WriteByte(Length(xp^.RP^.Name));
                  WriteData(xp^.RP^.Name[1], Length(xp^.RP^.Name) AND $FF);
                END;
            END;
        END;
    END;

    PROCEDURE WriteProcs2;
    VAR
      l: Longint;
      L2: Cardinal;
      x: PSCProcedure;
    BEGIN
      FOR l := 0 TO FProcs.Count - 1 DO
        BEGIN
          x := FProcs.GetItem(l);
          IF x^.Internal THEN
            BEGIN
              L2 := Length(FOutput);
              Move(L2, FOutput[x^.OutputDeclPosition + 2], 4);
          // write position
              WriteData(x^.Data[1], Length(x^.Data));
              L2 := Cardinal(Length(FOutput)) - L2;
              Move(L2, FOutput[x^.OutputDeclPosition + 6], 4); // write length
            END;
        END;
    END;

    FUNCTION FindMainProc: Cardinal;
    VAR
      l: Longint;
    BEGIN
      FOR l := 0 TO FProcs.Count - 1 DO
        BEGIN
          IF (PSCProcedure(FProcs.GetItem(l))^.Internal) AND
            (PSCProcedure(FProcs.GetItem(l))^.Name = SCMainProcName) THEN
            BEGIN
              Result := l;
              exit;
            END;
        END;
      Result := Cardinal(-1);
    END;
    PROCEDURE CreateDebugData;
    VAR
      I: Longint;
      p: PSCProcedure;
      pv: PSCVar;
      s: STRING;
    BEGIN
      s := #0;
      FOR I := 0 TO FProcs.Count - 1 DO
        BEGIN
          p := FProcs.GetItem(I);
          IF p^.Internal THEN
            BEGIN
              IF p^.Name = SCMainProcName THEN
                s := s + #1
              ELSE
                s := s + p^.Name + #1;
            END
          ELSE
            BEGIN
              s := s + PSCUsedRegProc(p)^.RP^.Name + #1;
            END;
        END;
      s := s + #0#1;
      FOR I := 0 TO FVars.Count - 1 DO
        BEGIN
          pv := FVars.GetItem(I);
          s := s + pv.Name + #1;
        END;
      s := s + #0;
      WriteDebugData(s);
    END;
  BEGIN
    CreateDebugData;
    WriteLong(SCValidHeader);
    WriteLong(SCCurrentBuildNo);
    WriteLong(FUsedTypes.Count);
    WriteLong(FProcs.Count);
    WriteLong(FVars.Count);
    WriteLong(FindMainProc);
    WriteLong(0);
    WriteTypes;
    WriteProcs;
    WriteVars;
    WriteProcs2;
  END;

  FUNCTION CheckExports: Boolean;
  VAR
    i: Longint;
    p: PSCProcedure;
  BEGIN
    IF @FOnExportCheck = NIL THEN
      BEGIN
        result := true;
        exit;
      END;
    FOR i := 0 TO FProcs.Count - 1 DO
      BEGIN
        p := FProcs.GetItem(i);
        IF p^.Internal THEN
          BEGIN
            IF NOT FOnExportCheck(Self, p, MakeDecl(p^.Decl)) THEN
              BEGIN
                Result := false;
                exit;
              END;
          END;
      END;
    Result := True;
  END;
  FUNCTION DoConstBlock: Boolean;
  VAR
    CName: STRING;
    CValue: PIFRVariant;
    Cp: PSCConstant;
  BEGIN
    FParser.Next;
    REPEAT
      IF FParser.CurrTokenID <> CSTI_Identifier THEN
        BEGIN
          MakeError('', ecIdentifierExpected, '');
          Result := False;
          Exit;
        END;
      CName := FParser.GetToken;
      IF IsDuplicate(CName) THEN
        BEGIN
          MakeError('', ecDuplicateIdentifier, '');
          Result := False;
          exit;
        END;
      FParser.Next;
      IF FParser.CurrTokenID <> CSTI_Equal THEN
        BEGIN
          MakeError('', ecIsExpected, '');
          Result := False;
          Exit;
        END;
      FParser.Next;
      CValue := ReadConstant(CSTI_SemiColon);
      IF CValue = NIL THEN
        BEGIN
          Result := False;
          Exit;
        END;
      IF FParser.CurrTokenID <> CSTI_Semicolon THEN
        BEGIN
          MakeError('', ecSemicolonExpected, '');
          Result := False;
          exit;
        END;
      New(cp);
      cp^.NameHash := MakeHash(CName);
      cp^.Name := CName;
      cp^.Value.FType := CValue^.FType;
      cp^.Value.Value := CValue^.Value;
      FConstants.Add(cp);
      DisposeVariant(CValue);
      FParser.Next;
    UNTIL FParser.CurrTokenId <> CSTI_Identifier;
    Result := True;
  END;
  FUNCTION ProcessUses: Boolean;
  VAR
    FUses: TSCStringList;
    I: Longint;
    s: STRING;
  BEGIN
    FParser.Next;
    FUses := TSCStringList.Create;
    REPEAT
      IF FParser.CurrTokenID <> CSTI_Identifier THEN
        BEGIN
          MakeError('', ecIdentifierExpected, '');
          FUses.Free;
          Result := False;
          exit;
        END;
      s := FParser.GetToken;
      FOR i := 0 TO FUses.Count - 1 DO
        BEGIN
          IF FUses.GetItem(I) = s THEN
            BEGIN
              MakeError('', ecDuplicateIdentifier, s);
              FUses.Free;
              Result := False;
              exit;
            END;
        END;
      FUses.Add(s);
      IF @FOnUses <> NIL THEN
        BEGIN
          TRY
            IF NOT OnUses(Self, FParser.GetToken) THEN
              BEGIN
                FUses.Free;
                Result := False;
                exit;
              END;
          EXCEPT
            ON e: Exception DO
              BEGIN
                MakeError('', ecCustomError, e.Message);
                FUses.Free;
                Result := False;
                exit;
              END;
          END;
        END;
      FParser.Next;
      IF FParser.CurrTokenID = CSTI_Semicolon THEN break
      ELSE IF FParser.CurrTokenId <> CSTI_Comma THEN
        BEGIN
          MakeError('', ecSemicolonExpected, '');
          Result := False;
          FUses.Free;
          exit;
        END;
      FParser.Next;
    UNTIL False;
    FParser.next;
    Result := True;
  END;
VAR
  Proc: PSCProcedure;

BEGIN
  FIsUnit := False;
  Result := False;
  Clear;
  FParser.SetText(s);

  FProcs := TSCList.Create;
  FConstants := TSCList.Create;
  FVars := TSCList.Create;
  FAvailableTypes := TSCList.Create;
  FUsedTypes := TSCList.Create;
  FRegProcs := TSCList.Create;
  DefineStandardTypes;
  IF @FOnUses <> NIL THEN
    BEGIN
      TRY
        IF NOT OnUses(Self, 'SYSTEM') THEN
          BEGIN
            FreeAll;
            exit;
          END;
      EXCEPT
        ON e: Exception DO
          BEGIN
            MakeError('', ecCustomError, e.Message);
            FreeAll;
            exit;
          END;
      END;
    END;
  Position := 0;
  Proc := NewProc(SCMainProcName);
  REPEAT
    IF FParser.CurrTokenId = CSTI_EOF THEN
      BEGIN
        IF FAllowNoEnd THEN
          Break
        ELSE
          BEGIN
            MakeError('', ecUnexpectedEndOfFile, '');
            FreeAll;
            exit;
          END;
      END;
    IF (FParser.CurrTokenId = CSTII_Program) AND (Position = 0) THEN
      BEGIN
        Position := 1;
        FParser.Next;
        IF FParser.CurrTokenId <> CSTI_Identifier THEN
          BEGIN
            MakeError('', ecIdentifierExpected, '');
            FreeAll;
            exit;
          END;
        FParser.Next;
        IF FParser.CurrTokenId <> CSTI_Semicolon THEN
          BEGIN
            MakeError('', ecSemicolonExpected, '');
            FreeAll;
            exit;
          END;
        FParser.Next;
      END ELSE
      IF (FParser.CurrTokenId = CSTII_Unit) AND (Position = 0) AND (FAllowUnit) THEN
        BEGIN
          Position := 1;
          FIsUnit := True;
          FParser.Next;
          IF FParser.CurrTokenId <> CSTI_Identifier THEN
            BEGIN
              MakeError('', ecIdentifierExpected, '');
              FreeAll;
              exit;
            END;
          FParser.Next;
          IF FParser.CurrTokenId <> CSTI_Semicolon THEN
            BEGIN
              MakeError('', ecSemicolonExpected, '');
              FreeAll;
              exit;
            END;
          FParser.Next;
        END
      ELSE IF (FParser.CurrTokenID = CSTII_Uses) AND (Position < 2) THEN
        BEGIN
          Position := 2;
          IF NOT ProcessUses THEN
            BEGIN
              FreeAll;
              exit;
            END;
        END ELSE IF (FParser.CurrTokenId = CSTII_Procedure) OR
        (FParser.CurrTokenId = CSTII_Function) THEN
        BEGIN
          Position := 2;
          IF NOT ProcessFunction THEN
            BEGIN
              FreeAll;
              exit;
            END;
        END
      ELSE IF (FParser.CurrTokenId = CSTII_Label) THEN
        BEGIN
          Position := 2;
          IF NOT ProcessLabel(Proc) THEN
            BEGIN
              FreeAll;
              exit;
            END;
        END
      ELSE IF (FParser.CurrTokenId = CSTII_Var) THEN
        BEGIN
          Position := 2;
          IF NOT DoVarBlock(NIL) THEN
            BEGIN
              FreeAll;
              exit;
            END;
        END
      ELSE IF (FParser.CurrTokenId = CSTII_Const) THEN
        BEGIN
          Position := 2;
          IF NOT DoConstBlock THEN
            BEGIN
              FreeAll;
              exit;
            END;
        END
      ELSE IF (FParser.CurrTokenId = CSTII_Type) THEN
        BEGIN
          Position := 2;
          IF NOT DoTypeBlock(FParser) THEN
            BEGIN
              FreeAll;
              exit;
            END;
        END
      ELSE IF (FParser.CurrTokenId = CSTII_Begin) THEN
        BEGIN
          IF ProcessSub(tMainBegin, 0, Proc) THEN
            BEGIN
              break;
            END
          ELSE
            BEGIN
              FreeAll;
              exit;
            END;
        END
      ELSE IF (Fparser.CurrTokenId = CSTII_End) AND (FAllowNoBegin OR FIsUnit) THEN
        BEGIN
          FParser.Next;
          IF FParser.CurrTokenID <> CSTI_Period THEN
            BEGIN
              MakeError('', ecPeriodExpected, '');
              FreeAll;
              exit;
            END;
          break;
        END ELSE
        BEGIN
          MakeError('', ecBeginExpected, '');
          FreeAll;
          exit;
        END;
  UNTIL False;
  IF NOT ProcessLabelForwards(Proc) THEN
    BEGIN
      FreeAll;
      exit;
    END;
  FOR i := 0 TO FProcs.Count - 1 DO
    BEGIN
      Proc := FProcs.GetItem(i);
      IF Proc^.Internal AND Proc^.Forwarded THEN
        BEGIN
          MakeError('', ecUnsatisfiedForward, Proc^.Name)^.Position := Proc^.DeclarePosition;
          FreeAll;
          Exit;
        END;
    END;
  IF NOT CheckExports THEN
    BEGIN
      FreeAll;
      exit;
    END;
  FOR i := 0 TO FVars.Count - 1 DO
    BEGIN
      IF NOT PSCVar(FVars.GetItem(I))^.Used THEN
        BEGIN
          MakeHint('', ehVariableNotUsed, PSCVar(FVars.GetItem(I))^.Name)^.Position := PSCVar(FVars.GetItem(I))^.DeclarePosition;
        END;
    END;
  MakeOutput;
  FreeAll;
  Result := True;
END;

CONSTRUCTOR TSCPascalCompiler.Create;
BEGIN
  INHERITED Create;
  FParser := TSCPascalParser.Create;
  FParser.OnParserError := ParserError;
  FAutoFreeList := TSCList.Create;
  FOutput := '';
  FMessages := TSCList.Create;
END;

DESTRUCTOR TSCPascalCompiler.Destroy;
BEGIN
  Clear;
  FAutoFreeList.Free;

  FMessages.Free;
  FParser.Free;
  INHERITED Destroy;
END;

FUNCTION TSCPascalCompiler.GetOutput(VAR s: STRING): Boolean;
BEGIN
  IF Length(FOutput) <> 0 THEN
    BEGIN
      s := FOutput;
      Result := True;
    END
  ELSE
    Result := False;
END;

FUNCTION TSCAbstractCompiler.GetMsg(l: Longint):
  PSCCompilerMessage;
BEGIN
  Result := FMessages.GetItem(l);
END;

FUNCTION TSCAbstractCompiler.GetMsgCount: Longint;
BEGIN
  Result := FMessages.Count;
END;

PROCEDURE TSCPascalCompiler.DefineStandardTypes;
BEGIN
  AddType('BYTE', btU8);
  AddTypeS('BOOLEAN', '(False, True)');
  FBooleanType := FAvailableTypes.Count - 1;
  AddType('CHAR', btChar);
  AddType('SHORTINT', btS8);
  AddType('WORD', btU16);
  AddType('SMALLINT', btS16);
  AddType('INTEGER', BtTypeCopy)^.Ext := AddType('LONGINT', btS32);
  AddType('CARDINAL', BtTypeCopy)^.Ext := AddType('LONGWORD', btU32);
  AddType('STRING', btString);
  AddType('PCHAR', btString)^.Ext := Pointer(1);
  AddType('SINGLE', btSingle);
  AddType('DOUBLE', btDouble);
  AddType('EXTENDED', btExtended);
  AddType('VARIANT', btVariant);
  AddType('TVARIANTARRAY', btArray)^.Ext := Pointer(FindType('VARIANT'));
{$IFNDEF NOINT64}
  AddType('INT64', btS64);
{$ENDIF}
  WITH AddFunction('function Assigned(I: Longint): Boolean;')^ DO
    BEGIN
      Name := '!ASSIGNED';
      NameHash := MakeHash(Name);
    END;
END;

PROCEDURE TSCAbstractCompiler.UpdateRecordFields(r: Pointer);
VAR
  I: Longint;

BEGIN
  IF PSCType(r)^.BaseType = btProcPtr THEN
    BEGIN
      ReplaceTypes(PSCProceduralType(PSCType(r)^.Ext)^.ProcDef);

    END ELSE IF PSCType(r)^.BaseType = btRecord THEN
    BEGIN
      FOR I := 0 TO PSCType(r)^.RecordSubVals.Count - 1 DO
        BEGIN
          PSCRecordType(PSCType(r)^.RecordSubVals.GetItem(I))^.FType := at2ut(PSCRecordType(PSCType(r)^.RecordSubVals.GetItem(I))^.FType);
        END;
    END
  ELSE IF PSCType(r)^.BaseType = btArray THEN
    BEGIN
      IF PSCType(r)^.Ext <> Pointer(Cardinal(-1)) THEN
        PSCType(r)^.Ext := Pointer(at2ut(Cardinal(PSCType(r)^.Ext)));
    END;
END;


FUNCTION TSCAbstractCompiler.FindType(CONST Name: STRING): Cardinal;
VAR
  i, n: Longint;
  p: PSCType;
  RName: STRING;
BEGIN
  IF FProcs = NIL THEN BEGIN Result := Cardinal(-1); exit;
    END;
  RName := Fastuppercase(Name);
  n := makehash(rname);
  FOR i := 0 TO FAvailableTypes.Count - 1 DO
    BEGIN
      p := FAvailableTypes.GetItem(I);
      IF (p^.NameHash = n) AND (p^.name = rname) THEN
        BEGIN
          result := I;
          exit;
        END;
    END;
  result := Cardinal(-1);
END;

FUNCTION TSCPascalCompiler.AddConstant(CONST Name: STRING; FType: Cardinal): PSCConstant;
VAR
  pc: PSCConstant;
BEGIN
  IF FProcs = NIL THEN BEGIN Result := NIL; exit;
    END;
  IF FType = Cardinal(-1) THEN BEGIN Result := NIL; exit;
    END;
  new(pc);
  pc^.Name := FastUppercase(name);
  pc^.NameHash := makehash(pc^.name);
  pc^.Value.FType := FType;
  FConstants.Add(pc);
  result := pc;
END;

TYPE
  PConstantVal = ^TConstantVal;
  TConstantVal = RECORD
    b: Boolean;
    CASE boolean OF
      true: (Rec: PIfRVariant; DeclPos: Cardinal; Modifiers: Byte);
      false: (c: byte);
  END;

FUNCTION TSCPascalCompiler.ReadConstant(StopOn: TSCToken): PIfRVariant;
VAR
  Items: TSCList;
  tmp: PConstantVal;
  Val: PIfRVariant;
  c,
    modifiers: byte;

  FUNCTION GetTypeX(BaseType: TSCBaseType): Cardinal;
  VAR
    l: Longint;
    x: PSCType;
  BEGIN
    FOR l := 0 TO FAvailableTypes.Count - 1 DO
      BEGIN
        IF PSCType(FAvailableTypes.GetItem(l))^.BaseType = BaseType THEN
          BEGIN
            Result := l;
            exit;
          END;
      END;
    New(x);
    x^.Name := '';
    x^.NameHash := MakeHash(x^.Name);
    x^.BaseType := BaseType;
    x^.TypeSize := 1;
    x^.DeclarePosition := Cardinal(-1);
    FAvailableTypes.Add(x);
    FUsedTypes.Add(x);
    Result := FUsedTypes.Count - 1;
  END;

  PROCEDURE Cleanup;
  VAR
    p: PConstantVal;
    l: Longint;
  BEGIN
    FOR l := 0 TO Items.Count - 1 DO
      BEGIN
        p := Items.GetItem(l);
        IF NOT p^.b THEN
          BEGIN
            DisposeVariant(p^.Rec);
          END;
        Dispose(p);
      END;
    Items.Free;
  END;

  FUNCTION SortItems: Boolean;
  VAR
    l: Longint;
    p, p1, P2: PConstantVal;
  BEGIN
    SortItems := False;
    IF Items.Count = 1 THEN
      BEGIN
        p1 := Items.GetItem(0);

        IF (p1^.Rec^.FType = CVAL_Data) THEN
          BEGIN

            IF p1^.Modifiers = 1 THEN // not
              BEGIN
                CASE PSCType(FUsedTypes.GetItem(p1^.Rec^.FType))^.BaseType OF
                  btU8: TbtU8((@p1^.Rec^.Value[1])^) := tbtu8(TbtU8((@p1^.Rec^.Value[1])^) = 0);
                  btS8: TbtS8((@p1^.Rec^.Value[1])^) := tbts8(TbtS8((@p1^.Rec^.Value[1])^) = 0);
                  btU16: TbtU16((@p1^.Rec^.Value[1])^) := tbtu16(TbtU16((@p1^.Rec^.Value[1])^) = 0);
                  btS16: TbtS16((@p1^.Rec^.Value[1])^) := tbts16(TbtS16((@p1^.Rec^.Value[1])^) = 0);
                  btU32: TbtU32((@p1^.Rec^.Value[1])^) := tbtu32(TbtU32((@p1^.Rec^.Value[1])^) = 0);
                  btS32: TbtS32((@p1^.Rec^.Value[1])^) := tbts32(TbtS32((@p1^.Rec^.Value[1])^) = 0);
                END;
              END ELSE
              IF p1^.Modifiers = 2 THEN // minus
                BEGIN
                  CASE PSCType(FUsedTypes.GetItem(p1^.Rec^.FType))^.BaseType OF
                    btU8: TbtU8((@p1^.Rec^.Value[1])^) := -TbtU8((@p1^.Rec^.Value[1])^);
                    btS8: TbtS8((@p1^.Rec^.Value[1])^) := -TbtS8((@p1^.Rec^.Value[1])^);
                    btU16: TbtU16((@p1^.Rec^.Value[1])^) := -TbtU16((@p1^.Rec^.Value[1])^);
                    btS16: TbtS16((@p1^.Rec^.Value[1])^) := -TbtS16((@p1^.Rec^.Value[1])^);
                    btU32: TbtU32((@p1^.Rec^.Value[1])^) := -TbtU32((@p1^.Rec^.Value[1])^);
                    btS32: TbtS32((@p1^.Rec^.Value[1])^) := -TbtS32((@p1^.Rec^.Value[1])^);
                    btSingle: TbtSingle((@p1^.Rec^.Value[1])^) := -TbtSingle((@p1^.Rec^.Value[1])^);
                    btDouble: TbtDouble((@p1^.Rec^.Value[1])^) := -TbtDouble((@p1^.Rec^.Value[1])^);
                    btExtended: TbtExtended((@p1^.Rec^.Value[1])^) := -tbtExtended((@p1^.Rec^.Value[1])^);
                  END;
                END;
          END;

        SortItems := True;
        exit;
      END;
    l := 0;
    WHILE l < Longint(Items.Count - 1) DIV 2 DO
      BEGIN
        p := Items.GetItem((l SHL 1) + 1);
        p1 := Items.GetItem((l SHL 1));
        P2 := Items.GetItem((l SHL 1) + 2);
        CASE p^.c OF
          2, 3, 4, 5, 6, 7: {*}
            BEGIN
              IF NOT PreCalc(FAvailableTypes, p1^.Modifiers, p1^.Rec, p2^.Modifiers, P2^.Rec, p^.c, p2^.DeclPos) THEN
                BEGIN
                  exit;
                END;
              Items.Delete((l SHL 1) + 1);
              Items.Delete((l SHL 1) + 1);
              DisposeVariant(p2^.Rec);
              Dispose(P2);
              Dispose(p);
            END;
        ELSE
          Inc(l);
        END;
      END;
    l := 0;
    WHILE l < Longint(Items.Count - 1) DIV 2 DO
      BEGIN
        p := Items.GetItem((l SHL 1) + 1);
        p1 := Items.GetItem((l SHL 1));
        P2 := Items.GetItem((l SHL 1) + 2);
        CASE p^.c OF
          0, 1, 8, 9:
            BEGIN
              IF NOT PreCalc(FAvailableTypes, p1^.Modifiers, p1^.Rec, p2^.Modifiers, P2^.Rec, p^.c, p2^.DeclPos) THEN
                BEGIN
                  exit;
                END;
              Items.Delete((l SHL 1) + 1);
              Items.Delete((l SHL 1) + 1);
              DisposeVariant(p2^.Rec);
              Dispose(P2);
              Dispose(p);
            END;
        ELSE
          Inc(l);
        END;
      END;
    l := 0;
    WHILE l < Longint(Items.Count - 1) DIV 2 DO
      BEGIN
        p := Items.GetItem((l SHL 1) + 1);
        p1 := Items.GetItem((l SHL 1));
        P2 := Items.GetItem((l SHL 1) + 2);
        CASE p^.c OF
          10, 11, 12, 13, 14, 15:
            BEGIN
              IF NOT PreCalc(FAvailableTypes, p1^.Modifiers, p1^.Rec, p2^.Modifiers, P2^.Rec, p^.c, p2^.DeclPos) THEN
                BEGIN
                  exit;
                END;
              Items.Delete((l SHL 1) + 1);
              Items.Delete((l SHL 1) + 1);
              DisposeVariant(p2^.Rec);
              Dispose(P2);
              Dispose(p);
            END;
        ELSE
          Inc(l);
        END;
      END;
    SortItems := True;
  END;
  FUNCTION ReadReal(CONST s: STRING): PIfRVariant;
  VAR
    C: Integer;
  BEGIN
    New(Result);
    Result^.FType := GetTypeX(btExtended);
    SetLength(Result^.Value, SizeOf(TbtExtended));
    System.Val(s, TbtExtended((@Result^.Value[1])^), C);
  END;
  FUNCTION ReadInteger(CONST s: STRING): PIfRVariant;
  VAR
    C: Integer;
  BEGIN
    New(Result);
    Result^.FType := GetTypeX(btS32);
    SetLength(Result^.Value, SizeOf(TbtS32));
    System.Val(s, TbtS32((@Result^.Value[1])^), C);
    IF TbtS32((@Result^.Value[1])^) < 0 THEN
      BEGIN
        System.Val(s, TbtU32((@Result^.Value[1])^), C);
      END;
  END;
  FUNCTION ReadString: PIfRVariant;

    FUNCTION ParseString: STRING;
    VAR
      temp3: STRING;

      FUNCTION ChrToStr(s: STRING): Char;
      BEGIN
        Delete(s, 1, 1); {First char : #}
        ChrToStr := Chr(StrToInt(s));
      END;

      FUNCTION PString(s: STRING): STRING;
      BEGIN
        s := copy(s, 2, Length(s) - 2);
        PString := s;
      END;
    BEGIN
      temp3 := '';
      WHILE (FParser.CurrTokenId = CSTI_String) OR (FParser.CurrTokenId =
        CSTI_Char) DO
        BEGIN
          IF FParser.CurrTokenId = CSTI_String THEN
            BEGIN
              temp3 := temp3 + PString(FParser.GetToken);
              FParser.Next;
              IF FParser.CurrTokenId = CSTI_String THEN
                temp3 := temp3 + #39;
            END {if}
          ELSE
            BEGIN
              temp3 := temp3 + ChrToStr(FParser.GetToken);
              FParser.Next;
            END; {else if}
        END; {while}
      ParseString := temp3;
    END;
  BEGIN
    New(Result);
    Result^.FType := GetTypeX(btString);
    Result^.Value := ParseString;
  END;
  FUNCTION GetConstantIdentifier: PIfRVariant;
  VAR
    s: STRING;
    sh: Longint;
    i: Longint;
    p: PSCConstant;
  BEGIN
    s := FParser.GetToken;
    sh := MakeHash(s);
    FOR i := FConstants.Count - 1 DOWNTO 0 DO
      BEGIN
        p := FConstants.GetItem(I);
        IF (p^.NameHash = sh) AND (p^.Name = s) THEN
          BEGIN
            New(Result);
            Result^.FType := p^.Value.FType;
            Result^.Value := p^.Value.Value;
            FParser.Next;
            exit;
          END;
      END;
    MakeError('', ecUnknownIdentifier, '');
    Result := NIL;

  END;
BEGIN
  Items := TSCList.Create;
  ReadConstant := NIL;
  WHILE True DO
    BEGIN
      modifiers := 0;
      IF Items.Count AND 1 = 0 THEN
        BEGIN
          IF fParser.CurrTokenID = CSTII_Not THEN
            BEGIN
              FParser.Next;
              modifiers := 1;
            END ELSE // only allow one of these two
            IF fParser.CurrTokenID = CSTI_Minus THEN
              BEGIN
                FParser.Next;
                modifiers := 2;
              END;
          CASE FParser.CurrTokenId OF
            CSTI_EOF:
              BEGIN
                MakeError('', ecUnexpectedEndOfFile, '');
                Cleanup;
                exit;
              END;
            CSTI_OpenRound:
              BEGIN
                FParser.Next;

                val := ReadConstant(CSTI_CloseRound);
                IF val = NIL THEN
                  BEGIN
                    Cleanup;
                    exit;
                  END;
                IF FParser.CurrTokenId <> CSTI_CloseRound THEN
                  BEGIN
                    MakeError('', ecCloseRoundExpected, '');
                    Cleanup;
                    exit;
                  END;
                IF ((Modifiers AND 1) <> 0) AND (NOT IsIntType(PSCType(FUsedTypes.GetItem(val^.FType))^.BaseType)) OR ((Modifiers AND 2) <> 0) AND (NOT IsRealType(PSCType(FUsedTypes.GetItem(val^.FType))^.BaseType)) THEN
                  BEGIN
                    DisposeVariant(val);
                    MakeError('', ecTypeMismatch, '');
                    Cleanup;
                    exit;
                  END;
                new(tmp);
                tmp^.b := False;
                tmp^.Rec := Val;
                tmp^.DeclPos := FParser.CurrTokenPos;
                tmp^.Modifiers := modifiers;
                Items.Add(tmp);
                FParser.Next;
              END;
            CSTI_Char, CSTI_String:
              BEGIN
                IF (Modifiers <> 0) THEN
                  BEGIN
                    MakeError('', ecTypeMismatch, '');
                    Cleanup;
                    exit;
                  END;
                val := ReadString;
                new(tmp);
                tmp^.b := False;
                tmp^.Rec := Val;
                tmp^.DeclPos := FParser.CurrTokenPos;
                tmp^.Modifiers := modifiers;
                Items.Add(tmp);
              END;
            CSTI_HexInt, CSTI_Integer:
              BEGIN
                Val := ReadInteger(FParser.GetToken);
                new(tmp);
                tmp^.b := False;
                tmp^.Rec := Val;
                tmp^.DeclPos := FParser.CurrTokenPos;
                tmp^.Modifiers := modifiers;
                Items.Add(tmp);
                FParser.Next;
              END;
            CSTI_Real:
              BEGIN
                IF ((Modifiers AND 1) <> 0) THEN
                  BEGIN
                    MakeError('', ecTypeMismatch, '');
                    Cleanup;
                    exit;
                  END;
                Val := ReadReal(FParser.GetToken);
                new(tmp);
                tmp^.b := False;
                tmp^.Rec := Val;
                tmp^.DeclPos := FParser.CurrTokenPos;
                tmp^.Modifiers := modifiers;
                Items.Add(tmp);
                FParser.Next;
              END;
            CSTI_Identifier:
              BEGIN
                val := GetConstantIdentifier;
                IF val = NIL THEN
                  BEGIN
                    Cleanup;
                    exit;
                  END
                ELSE
                  BEGIN
                    IF ((Modifiers AND 1) <> 0) AND (NOT IsIntType(PSCType(FUsedTypes.GetItem(val^.FType))^.BaseType)) OR ((Modifiers AND 2) <> 0) AND (NOT IsIntRealType(PSCType(FUsedTypes.GetItem(val^.FType))^.BaseType))
                      THEN
                      BEGIN
                        DisposeVariant(val);
                        MakeError('', ecTypeMismatch, '');
                        Cleanup;
                        exit;
                      END;
                    new(tmp);
                    tmp^.b := False;
                    tmp^.Rec := Val;
                    tmp^.DeclPos := FParser.CurrTokenPos;
                    tmp^.Modifiers := modifiers;
                    Items.Add(tmp);
                  END;
              END;
          ELSE
            BEGIN
              MakeError('', ecSyntaxError, '');
              Cleanup;
              exit;
            END;
          END; {case}
        END
      ELSE {Items.Count and 1 = 1}
        BEGIN
          IF FParser.CurrTokenId = StopOn THEN
            break;
          C := 0;
          CASE FParser.CurrTokenId OF
            CSTI_EOF:
              BEGIN
                MakeError('', ecUnexpectedEndOfFile, '');
                Cleanup;
                exit;
              END;
            CSTI_CloseBlock,
              CSTII_To,
              CSTI_CloseRound,
              CSTI_Semicolon,
              CSTII_Else,
              CSTII_End,
              CSTI_Comma: break;
            CSTI_Plus: ;
            CSTI_Minus: C := 1;
            CSTI_Multiply: C := 2;
            CSTI_Divide: C := 3;
            CSTII_mod: C := 4;
            CSTII_shl: C := 5;
            CSTII_shr: C := 6;
            CSTII_and: C := 7;
            CSTII_or: C := 8;
            CSTII_xor: C := 9;
            CSTI_GreaterEqual: C := 10;
            CSTI_LessEqual: C := 11;
            CSTI_Greater: C := 12;
            CSTI_Less: C := 13;
            CSTI_NotEqual: C := 14;
            CSTI_Equal: C := 15;
          ELSE
            BEGIN
              MakeError('', ecSyntaxError, '');
              Cleanup;
              exit;
            END;
          END; {case}
          new(tmp);
          tmp^.b := True;
          tmp^.c := C;
          Items.Add(tmp);
          FParser.Next;
        END;
    END;
  IF NOT SortItems THEN
    BEGIN
      Cleanup;
      exit;
    END;
  IF Items.Count = 1 THEN
    BEGIN
      tmp := Items.GetItem(0);
      Result := tmp^.Rec;
      Dispose(tmp);
      Items.Free;
    END
  ELSE
    BEGIN
      MakeError('', ecInternalError, '0001B');
      Cleanup;
      Exit;
    END;
END;

PROCEDURE TSCPascalCompiler.WriteDebugData(CONST s: STRING);
BEGIN
  FDebugOutput := FDebugOutput + s;
END;

FUNCTION TSCPascalCompiler.GetDebugOutput(VAR s: STRING): Boolean;
BEGIN
  IF Length(FDebugOutput) <> 0 THEN
    BEGIN
      s := FDebugOutput;
      Result := True;
    END
  ELSE
    Result := False;
END;

FUNCTION TSCPascalCompiler.AddUsedFunction(VAR Proc: PSCProcedure): Cardinal;
BEGIN
  IF FProcs = NIL THEN BEGIN Result := Cardinal(-1); exit;
    END;
  New(Proc);
  FProcs.Add(Proc);
  Result := FProcs.Count - 1;
END;

FUNCTION TSCPascalCompiler.GetAvailableType(No: Cardinal): PSCType;
BEGIN
  IF FProcs = NIL THEN BEGIN Result := NIL; exit;
    END;
  Result := FAvailableTypes.GetItem(No);
END;

FUNCTION TSCPascalCompiler.GetAvailableTypeCount: Cardinal;
BEGIN
  IF FProcs = NIL THEN BEGIN Result := Cardinal(-1); exit;
    END;
  Result := FAvailableTypes.Count;
END;

FUNCTION TSCPascalCompiler.GetProc(No: Cardinal): PSCProcedure;
BEGIN
  IF FProcs = NIL THEN BEGIN Result := NIL; exit;
    END;
  Result := FProcs.GetItem(No);
END;

FUNCTION TSCPascalCompiler.GetProcCount: Cardinal;
BEGIN
  IF FProcs = NIL THEN BEGIN Result := Cardinal(-1); exit;
    END;
  Result := FProcs.Count;
END;

FUNCTION TSCPascalCompiler.GetUsedType(No: Cardinal): PSCType;
BEGIN
  IF FProcs = NIL THEN BEGIN Result := NIL; exit;
    END;
  Result := FUsedTypes.GetItem(No);
END;

FUNCTION TSCPascalCompiler.GetUsedTypeCount: Cardinal;
BEGIN
  IF FProcs = NIL THEN BEGIN Result := Cardinal(-1); exit;
    END;
  Result := FUsedTypes.Count;
END;

FUNCTION TSCPascalCompiler.UseAvailableType(No: Cardinal): Cardinal;
VAR
  I: Longint;
  p: PSCType;
BEGIN
  IF FProcs = NIL THEN BEGIN Result := Cardinal(-1); exit;
    END;
  p := FAvailableTypes.GetItem(No);
  IF p = NIL THEN
    BEGIN
      Result := Cardinal(-1);
      Exit;
    END;

  FOR I := 0 TO FUsedTypes.Count - 1 DO
    BEGIN
      IF FUsedTypes.GetItem(I) = p THEN
        BEGIN
          Result := I;
          exit;
        END;
    END;
  UpdateRecordFields(p);
  FUsedTypes.Add(p);
  Result := FUsedTypes.Count - 1;
END;

FUNCTION TSCPascalCompiler.AddUsedFunction2(VAR Proc: PSCUsedRegProc): Cardinal;
BEGIN
  IF FProcs = NIL THEN BEGIN Result := Cardinal(-1); exit;
    END;
  New(Proc);
  Proc^.Internal := False;
  FProcs.Add(Proc);
  Result := FProcs.Count - 1;
END;

FUNCTION TSCPascalCompiler.AddVariable(CONST Name: STRING; FType: Cardinal): PSCVar;
VAR
  P: PSCVar;
BEGIN
  IF FProcs = NIL THEN BEGIN Result := NIL; exit;
    END;
  IF FType = Cardinal(-1) THEN BEGIN Result := NIL; exit;
    END;
  New(p);
  p^.Name := Fastuppercase(Name);
  p^.Namehash := MakeHash(p^.Name);
  p^.FType := AT2UT(FType);
  p^.Used := False;
  p^.DeclarePosition := 0;
  FVars.Add(p);
  Result := P;
END;

FUNCTION TSCPascalCompiler.GetVariable(No: Cardinal): PSCVar;
BEGIN
  IF FProcs = NIL THEN BEGIN Result := NIL; exit;
    END;
  Result := FVars.GetItem(No);
END;

FUNCTION TSCPascalCompiler.GetVariableCount: Cardinal;
BEGIN
  IF FProcs = NIL THEN BEGIN Result := 0; exit;
    END;
  Result := FVars.Count;
END;


PROCEDURE TSCPascalCompiler.AddToFreeList(Obj: TObject);
BEGIN
  FAutoFreeList.Add(Obj);
END;

FUNCTION TSCPascalCompiler.AddConstantN(CONST Name,
  FType: STRING): PSCConstant;
VAR
  L: Cardinal;
BEGIN
  L := FindType(FType);
  IF l = Cardinal(-1) THEN
    Result := NIL
  ELSE
    Result := AddConstant(Name, L);
END;

FUNCTION TSCPascalCompiler.AddTypeCopy(CONST Name: STRING;
  TypeNo: Cardinal): PSCType;
BEGIN
  Result := AddType(Name, BtTypeCopy);
  Result.Ext := Pointer(TypeNo);
END;

FUNCTION TSCPascalCompiler.AddTypeCopyN(CONST Name,
  FType: STRING): PSCType;
VAR
  L: Cardinal;
BEGIN
  L := FindType(FType);
  IF L = Cardinal(-1) THEN
    Result := NIL
  ELSE
    Result := AddTypeCopy(Name, L);
END;


FUNCTION TSCPascalCompiler.AddUsedVariable(CONST Name: STRING;
  FType: Cardinal): PSCVar;
BEGIN
  Result := AddVariable(Name, FType);
  IF Result <> NIL THEN
    Result^.Used := True;
END;

FUNCTION TSCPascalCompiler.AddUsedVariableN(CONST Name,
  FType: STRING): PSCVar;
BEGIN
  Result := AddVariable(Name, FindType(FType));
  IF Result <> NIL THEN
    Result^.Used := True;
END;

FUNCTION TSCPascalCompiler.AddVariableN(CONST Name,
  FType: STRING): PSCVar;
BEGIN
  Result := AddVariable(Name, FindType(FType));
END;

FUNCTION TSCPascalCompiler.AddTypeS(CONST Name, Decl: STRING): PSCType;
VAR
  Parser: TSCPascalParser;
BEGIN
  Parser := TSCPascalParser.Create;
  Parser.SetText(Decl);
  Result := FAvailableTypes.GetItem(ReadType(FastUppercase(Name), Parser));
  Parser.Free;
END;


FUNCTION TSCPascalCompiler.CheckCompatProc(FTypeNo,
  ProcNo: Cardinal): Boolean;
VAR
  s1, s2: STRING;
  P: PSCType;

  FUNCTION c(CONST e1, e2: STRING): Boolean;
  BEGIN
    Result := (Length(e1) = 0) OR (Length(e2) = 0) OR (e1[1] <> e2[1]);
  END;
BEGIN
  P := FUsedTypes.GetItem(FTypeNo);
  IF p^.BaseType <> btProcPtr THEN BEGIN
      Result := False;
      Exit;
    END;

  S1 := PSCProceduralType(p^.Ext)^.ProcDef;

  IF PSCProcedure(FProcs.GetItem(ProcNo))^.Internal THEN
    s2 := PSCProcedure(FProcs.GetItem(ProcNo))^.Decl
  ELSE
    s2 := PSCUsedRegProc(FProcs.GetItem(ProcNo))^.RP^.Decl;
  IF GRFW(s1) <> GRFW(s2) THEN BEGIN
      Result := False;
      Exit;
    END;
  WHILE Length(s1) > 0 DO
    BEGIN
      IF c(GRFW(s1), GRFW(s2)) OR (GRFW(s1) <> GRFW(s2)) THEN BEGIN
          Result := False;
          Exit;
        END;
    END;
  Result := True;
END;

FUNCTION TSCPascalCompiler.MakeExportDecl(decl: STRING): STRING;
VAR
  c: char;
BEGIN
  result := grfw(decl);
  WHILE length(decl) > 0 DO
    BEGIN
      c := grfw(decl)[1];
      result := result + ' ' + c + grfw(decl);
    END;
END;


FUNCTION TSCPascalCompiler.IsIntBoolType(FTypeNo: Cardinal): Boolean;
VAR
  f: PSCType;
BEGIN
  IF FTypeNo = at2ut(FBooleanType) THEN BEGIN Result := True; exit;
    END;
  f := FUsedTypes.GetItem(FTypeNo);

  CASE f^.BaseType OF
    btU8, btS8, btU16, btS16, btU32, btS32{$IFNDEF NOINT64}, btS64{$ENDIF}: Result := True;
  ELSE
    Result := False;
  END;
END;

FUNCTION TSCPascalCompiler.AddExportVariableN(CONST Name,
  FType: STRING): PSCVar;
BEGIN
  Result := AddVariableN(Name, FType);
  IF Result <> NIL THEN
    Result^.exportname := FastUppercase(Name);
END;

FUNCTION TSCPascalCompiler.AddUsedExportVariableN(CONST Name,
  FType: STRING): PSCVar;
BEGIN
  Result := AddUsedVariableN(Name, FType);
  IF Result <> NIL THEN
    Result^.exportname := FastUppercase(Name);
END;

PROCEDURE TSCPascalCompiler.ParserError(Parser: TObject;
  Kind: TSCParserErrorKind; Position: Cardinal);
BEGIN
  CASE Kind OF
    ICOMMENTERROR: MakeError('', ecCommentError, '')^.Position := Position;
    ISTRINGERROR: MakeError('', ecStringError, '')^.Position := Position;
    ICHARERROR: MakeError('', ecCharError, '')^.Position := Position;
  ELSE
    MakeError('', ecSyntaxError, '')^.Position := Position;
  END;
END;

{ TSCExternalClass }

FUNCTION TSCExternalClass.SetNil(TypeNo: Cardinal; VAR ProcNo: Cardinal): Boolean;
BEGIN
  Result := False;
END;

FUNCTION TSCExternalClass.ClassFunc_Call(Index: Cardinal;
  VAR ProcNo: Cardinal): Boolean;
BEGIN
  Result := False;
END;

FUNCTION TSCExternalClass.ClassFunc_Find(CONST Name: STRING;
  VAR Index: Cardinal): Boolean;
BEGIN
  Result := False;
END;

CONSTRUCTOR TSCExternalClass.Create(Se: TSCPascalCompiler);
BEGIN
  INHERITED Create;
  Self.SE := se;
END;

FUNCTION TSCExternalClass.Func_Call(Index: Cardinal;
  VAR ProcNo: Cardinal): Boolean;
BEGIN
  Result := False;
END;

FUNCTION TSCExternalClass.Func_Find(CONST Name: STRING;
  VAR Index: Cardinal): Boolean;
BEGIN
  Result := False;
END;


FUNCTION TSCExternalClass.IsCompatibleWith(
  Cl: TSCExternalClass): Boolean;
BEGIN
  Result := False;
END;

FUNCTION TSCExternalClass.Property_Find(CONST Name: STRING;
  VAR Index: Cardinal): Boolean;
BEGIN
  Result := False;
END;

FUNCTION TSCExternalClass.Property_Get(Index: Cardinal;
  VAR ProcNo: Cardinal): Boolean;
BEGIN
  Result := False;
END;


FUNCTION TSCExternalClass.Property_GetHeader(Index: Cardinal;
  VAR s: STRING): Boolean;
BEGIN
  Result := False;
END;

FUNCTION TSCExternalClass.Property_Set(Index: Cardinal;
  VAR ProcNo: Cardinal): Boolean;
BEGIN
  Result := False;
END;

FUNCTION TSCExternalClass.SelfType: Cardinal;
BEGIN
  Result := Cardinal(-1);
END;

FUNCTION TSCExternalClass.CastToType(TypeNo, IntoType: Cardinal;
  VAR ProcNo: Cardinal): Boolean;
BEGIN
  Result := False;
END;

FUNCTION TSCExternalClass.CompareClass(OtherTypeNo: Cardinal;
  VAR ProcNo: Cardinal): Boolean;
BEGIN
  Result := false;
END;

{  }

FUNCTION TransDoubleToStr(D: Double): STRING;
BEGIN
  SetLength(Result, SizeOf(Double));
  Double((@Result[1])^) := D;
END;

FUNCTION TransSingleToStr(D: Single): STRING;
BEGIN
  SetLength(Result, SizeOf(Single));
  Single((@Result[1])^) := D;
END;

FUNCTION TransExtendedToStr(D: Extended): STRING;
BEGIN
  SetLength(Result, SizeOf(Extended));
  Extended((@Result[1])^) := D;
END;

FUNCTION TransLongintToStr(D: Longint): STRING;
BEGIN
  SetLength(Result, SizeOf(Longint));
  Longint((@Result[1])^) := D;
END;

FUNCTION TransCardinalToStr(D: Cardinal): STRING;
BEGIN
  SetLength(Result, SizeOf(Cardinal));
  Cardinal((@Result[1])^) := D;
END;

FUNCTION TransWordToStr(D: Word): STRING;
BEGIN
  SetLength(Result, SizeOf(Word));
  Word((@Result[1])^) := D;
END;

FUNCTION TransSmallIntToStr(D: SmallInt): STRING;
BEGIN
  SetLength(Result, SizeOf(SmallInt));
  SmallInt((@Result[1])^) := D;
END;

FUNCTION TransByteToStr(D: Byte): STRING;
BEGIN
  SetLength(Result, SizeOf(Byte));
  Byte((@Result[1])^) := D;
END;

FUNCTION TransShortIntToStr(D: ShortInt): STRING;
BEGIN
  SetLength(Result, SizeOf(ShortInt));
  ShortInt((@Result[1])^) := D;
END;

{

Internal error counter: 0001D (increase and then use)

}



END.

