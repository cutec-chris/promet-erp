UNIT SC_Parsers;

{$ifdef fpc}
{$mode delphi}{$H+}
{$endif}


INTERFACE

CONST
  MaxListSize = Maxint DIV 16;

TYPE
  PPointerList = ^TPointerList;
  TPointerList = ARRAY[0..MaxListSize - 1] OF Pointer;

  TSCList = CLASS(TObject)
  PRIVATE
    FCapacity: Cardinal;
    FCount: Cardinal;
    FData: PPointerList;
{$IFNDEF NOSMARTLIST}
    FCheckCount: Cardinal;
{$ENDIF}
  PUBLIC
{$IFNDEF NOSMARTLIST}
    PROCEDURE Recreate;
{$ENDIF}
    CONSTRUCTOR Create;
    DESTRUCTOR Destroy; OVERRIDE;
    PROPERTY Count: Cardinal READ FCount;
    FUNCTION GetItem(Nr: Cardinal): Pointer;
    PROCEDURE SetItem(Nr: Cardinal; P: Pointer);
    PROCEDURE Add(P: Pointer);
    PROCEDURE AddBlock(List: PPointerList; Count: Longint);
    PROCEDURE Remove(P: Pointer);
    PROCEDURE Delete(Nr: Cardinal);
    PROCEDURE Clear; VIRTUAL;
  END;

  TSCStringList = CLASS(TObject)
  PRIVATE
    List: TSCList;
  PUBLIC
    FUNCTION Count: LongInt;
    FUNCTION GetItem(Nr: LongInt): STRING;
    PROCEDURE SetItem(Nr: LongInt; CONST s: STRING);
    PROCEDURE Add(CONST P: STRING);
    PROCEDURE Delete(NR: LongInt);
    PROCEDURE Clear;
    CONSTRUCTOR Create;
    DESTRUCTOR Destroy; OVERRIDE;
  END;


TYPE
  TSCToken = (
    CSTI_EOF,
    CSTIINT_Comment,
    CSTIINT_WhiteSpace,

    CSTI_Identifier,
    CSTI_SemiColon,
    CSTI_Comma,
    CSTI_Period,
    CSTI_Colon,
    CSTI_OpenRound,
    CSTI_CloseRound,
    CSTI_OpenBlock,
    CSTI_CloseBlock,
    CSTI_Assignment,
    CSTI_Equal,
    CSTI_NotEqual,
    CSTI_Greater,
    CSTI_GreaterEqual,
    CSTI_Less,
    CSTI_LessEqual,
    CSTI_Plus,
    CSTI_Minus,
    CSTI_Divide,
    CSTI_Multiply,
    CSTI_Integer,
    CSTI_Real,
    CSTI_String,
    CSTI_Char,
    CSTI_HexInt,
    CSTI_AddressOf,
    CSTI_Dereference,

    CSTII_and,
    CSTII_array,
    CSTII_begin,
    CSTII_case,
    CSTII_const,
    CSTII_div,
    CSTII_do,
    CSTII_downto,
    CSTII_else,
    CSTII_end,
    CSTII_for,
    CSTII_function,
    CSTII_if,
    CSTII_in,
    CSTII_mod,
    CSTII_not,
    CSTII_of,
    CSTII_or,
    CSTII_procedure,
    CSTII_program,
    CSTII_repeat,
    CSTII_record,
    CSTII_set,
    CSTII_shl,
    CSTII_shr,
    CSTII_then,
    CSTII_to,
    CSTII_type,
    CSTII_until,
    CSTII_uses,
    CSTII_var,
    CSTII_while,
    CSTII_with,
    CSTII_xor,
    CSTII_exit,
    CSTII_break,
    CSTII_class,
    CSTII_constructor,
    CSTII_destructor,
    CSTII_inherited,
    CSTII_private,
    CSTII_public,
    CSTII_published,
    CSTII_protected,
    CSTII_property,
    CSTII_virtual,
    CSTII_override,
    CSTII_As,
    CSTII_Is,
    CSTII_Unit,
    CSTII_Continue,
    CSTII_Try,
    CSTII_Except,
    CSTII_Finally,
    CSTII_External,
    CSTII_Forward,
    CSTII_Export,
    CSTII_Label,
    CSTII_Goto,
    CSTII_Chr,
    CSTII_Ord,
    CSTII_Interface,
    CSTII_Implementation
    );
  TSCParserErrorKind = (iNoError, iCommentError, iStringError, iCharError, iSyntaxError);
  TSCParserErrorEvent = PROCEDURE(Parser: TObject; Kind: TSCParserErrorKind; Position: Cardinal) OF OBJECT;

  TSCAbstractParser = CLASS(TObject)
  private
    FToken: STRING;
    FOriginalToken: STRING;
    FTokenId: TSCToken;
    FRealPosition : Cardinal;
    FParserError: TSCParserErrorEvent;
  public
    PROCEDURE Next;virtual;abstract;
    PROPERTY GetToken: STRING READ FToken;
    PROPERTY OriginalToken: STRING READ FOriginalToken;
    PROPERTY CurrTokenPos: Cardinal READ FRealPosition;
    PROPERTY CurrTokenID: TSCToken READ FTokenId;
    PROCEDURE SetText(CONST Data: STRING);virtual;abstract;
    PROPERTY OnParserError: TSCParserErrorEvent READ FParserError WRITE FParserError;
  end;

  TSCPascalParser = CLASS(TSCAbstractParser)
  PRIVATE
    FData: STRING;
    FText: PChar;
    FTokenLength: Cardinal;
    // only applicable when Token in [CSTI_Identifier, CSTI_Integer, CSTI_Real, CSTI_String, CSTI_Char, CSTI_HexInt]
  PUBLIC
    PROCEDURE Next;override;
    PROCEDURE SetText(CONST Data: STRING);override;
    PROPERTY OnParserError: TSCParserErrorEvent READ FParserError WRITE FParserError;
  END;

FUNCTION FloatToStr(E: Extended): STRING;
FUNCTION FastLowerCase(CONST s: STRING): STRING;
{Return the first word of a string}
FUNCTION Fw(CONST S: STRING): STRING;
FUNCTION IntToStr(I: LongInt): STRING;
FUNCTION StrToIntDef(CONST S: STRING; Def: LongInt): LongInt;
FUNCTION StrToInt(CONST S: STRING): LongInt;
FUNCTION FastUpperCase(CONST s: STRING): STRING;
{Get the first word and remove it}
FUNCTION GRFW(VAR s: STRING): STRING;

IMPLEMENTATION

FUNCTION GRFW(VAR s: STRING): STRING;
VAR
  l: Longint;
BEGIN
  l := 1;
  WHILE l <= Length(s) DO
    BEGIN
      IF s[l] = ' ' THEN
        BEGIN
          Result := copy(s, 1, l - 1);
          Delete(s, 1, l);
          exit;
        END;
      l := l + 1;
    END;
  Result := s;
  s := '';
END;

FUNCTION IntToStr(I: LongInt): STRING;
VAR
  s: STRING;
BEGIN
  Str(i, s);
  IntToStr := s;
END;

FUNCTION FloatToStr(E: Extended): STRING;
VAR
  s: STRING;
BEGIN
  Str(e: 0: 12, s);
  result := s;
END;

FUNCTION StrToInt(CONST S: STRING): LongInt;
VAR
  e: Integer;
  Res: LongInt;
BEGIN
  Val(S, Res, e);
  IF e <> 0 THEN
    StrToInt := -1
  ELSE
    StrToInt := Res;
END;

FUNCTION StrToIntDef(CONST S: STRING; Def: LongInt): LongInt;
VAR
  e: Integer;
  Res: LongInt;
BEGIN
  Val(S, Res, e);
  IF e <> 0 THEN
    StrToIntDef := Def
  ELSE
    StrToIntDef := Res;
END;

CONSTRUCTOR TSCList.Create;
BEGIN
  INHERITED Create;
  FCount := 0;
  FCapacity := 16;
{$IFNDEF NOSMARTLIST}
  FCheckCount := 0;
{$ENDIF}
  GetMem(FData, 64);
END;

CONST
  FCapacityInc = 32;
{$IFNDEF NOSMARTLIST}
  FMaxCheckCount = (FCapacityInc DIV 4) * 16;
{$ENDIF}

FUNCTION MM(i1, i2: Integer): Integer;
BEGIN
  IF ((i1 DIV i2) * i2) < i1 THEN
    mm := (i1 DIV i2 + 1) * i2
  ELSE
    mm := (i1 DIV i2) * i2;
END;

{$IFNDEF NOSMARTLIST}

PROCEDURE TSCList.Recreate;
VAR
  NewData: PPointerList;
  NewCapacity: Cardinal;
  I: Longint;

BEGIN

  FCheckCount := 0;
  NewCapacity := mm(FCount, FCapacityInc);
  IF NewCapacity < 64 THEN NewCapacity := 64;
  GetMem(NewData, NewCapacity * 4);
  FOR I := 0 TO Longint(FCount) - 1 DO
    BEGIN
      NewData^[i] := FData^[I];
    END;
  FreeMem(FData, FCapacity * 4);
  FData := NewData;
  FCapacity := NewCapacity;
END;
{$ENDIF}

PROCEDURE TSCList.Add(P: Pointer);
BEGIN
  IF FCount >= FCapacity THEN
    BEGIN
      Inc(FCapacity, FCapacityInc); // := FCount + 1;
      ReAllocMem(FData, FCapacity SHL 2);
    END;
  FData[FCount] := P; // Instead of SetItem
  Inc(FCount);
  Inc(FCheckCount);
{$IFNDEF NOSMARTLIST}
  IF FCheckCount > FMaxCheckCount THEN Recreate;
{$ENDIF}
END;

PROCEDURE TSCList.AddBlock(List: PPointerList; Count: Longint);
VAR
  L: Longint;

BEGIN
  IF Longint(FCount) + Count > Longint(FCapacity) THEN
    BEGIN
      Inc(FCapacity, mm(Count, FCapacityInc));
      ReAllocMem(FData, FCapacity SHL 2);
    END;
  FOR L := 0 TO Count - 1 DO
    BEGIN
      FData^[FCount] := List^[L];
      Inc(FCount);
    END;
{$IFNDEF NOSMARTLIST}
  Inc(FCheckCount);
  IF FCheckCount > FMaxCheckCount THEN Recreate;
{$ENDIF}
END;

PROCEDURE TSCList.Delete(Nr: Cardinal);
BEGIN
  IF FCount = 0 THEN Exit;
  IF Nr < FCount THEN
    BEGIN
      Move(FData[Nr + 1], FData[Nr], (FCount - Nr) * 4);
      Dec(FCount);
{$IFNDEF NOSMARTLIST}
      Inc(FCheckCount);
      IF FCheckCount > FMaxCheckCount THEN Recreate;
{$ENDIF}
    END;
END;

PROCEDURE TSCList.Remove(P: Pointer);
VAR
  I: Cardinal;
BEGIN
  IF FCount = 0 THEN Exit;
  I := 0;
  WHILE I < FCount DO
    BEGIN
      IF FData[I] = P THEN
        BEGIN
          Delete(I);
          Exit;
        END;
      Inc(I);
    END;
END;

PROCEDURE TSCList.Clear;
BEGIN
  FCount := 0;
{$IFNDEF NOSMARTLIST}
  Recreate;
{$ENDIF}
END;

DESTRUCTOR TSCList.Destroy;
BEGIN
  FreeMem(FData, FCapacity * 4);
  INHERITED Destroy;
END;

PROCEDURE TSCList.SetItem(Nr: Cardinal; P: Pointer);
BEGIN
  IF (FCount = 0) OR (Nr >= FCount) THEN
    Exit;
  FData[Nr] := P;
END;

FUNCTION TSCList.GetItem(Nr: Cardinal): Pointer; {12}
BEGIN
  IF Nr < FCount THEN
    GetItem := FData[Nr]
  ELSE
    GetItem := NIL;
END;

FUNCTION TSCStringList.Count: LongInt;
BEGIN
  count := List.count;
END;

TYPE
  pStr = ^STRING;

FUNCTION TSCStringList.GetItem(Nr: LongInt): STRING;
VAR
  S: PStr;
BEGIN
  s := List.GetItem(Nr);
  IF s = NIL THEN
    Result := ''
  ELSE

    Result := s^;
END;

PROCEDURE TSCStringList.SetItem(Nr: LongInt; CONST s: STRING);
VAR
  p: PStr;
BEGIN
  p := List.GetItem(Nr);
  IF p = NIL
    THEN
    Exit;
  p^ := s;
END;

PROCEDURE TSCStringList.Add(CONST P: STRING);
VAR
  w: PStr;
BEGIN
  new(w);
  w^ := p;
  List.Add(w);
END;

PROCEDURE TSCStringList.Delete(NR: LongInt);
VAR
  W: PStr;
BEGIN
  W := list.getitem(nr);
  IF w <> NIL THEN
    BEGIN
      dispose(w);
    END;
  list.Delete(Nr);
END;

PROCEDURE TSCStringList.Clear;
BEGIN
  WHILE List.Count > 0 DO Delete(0);
END;

CONSTRUCTOR TSCStringList.Create;
BEGIN
  INHERITED Create;
  List := TSCList.Create;
END;

DESTRUCTOR TSCStringList.Destroy;
BEGIN
  WHILE List.Count > 0 DO
    Delete(0);
  List.Destroy;
  INHERITED Destroy;
END;

FUNCTION Fw(CONST S: STRING): STRING; //  First word
VAR
  x: integer;
BEGIN
  x := pos(' ', s);
  IF x > 0
    THEN Fw := Copy(S, 1, x - 1)
  ELSE Fw := S;
END;

FUNCTION FastUpperCase(CONST s: STRING): STRING;
VAR
  I: Integer;
  C: Char;
BEGIN
  Result := S;
  I := Length(Result);
  WHILE I > 0 DO
    BEGIN
      C := Result[I];
      IF C IN [#97..#122] THEN
        Dec(Byte(Result[I]), 32);
      Dec(I);
    END;
END;

FUNCTION FastLowerCase(CONST s: STRING): STRING;
VAR
  I: Integer;
  C: Char;
BEGIN
  Result := S;
  I := Length(Result);
  WHILE I > 0 DO
    BEGIN
      C := Result[I];
      IF C IN [#65..#90] THEN
        Inc(Byte(Result[I]), 32);
      Dec(I);
    END;
END;

TYPE
  TRTab = RECORD
    name: STRING;
    c: TSCToken;
  END;

CONST
  PAS_KEYWORD_COUNT = 63;
  PasLookupTable: ARRAY[0..PAS_KEYWORD_COUNT - 1] OF TRTab = (
    (name: 'AND'; c: CSTII_and),
    (name: 'ARRAY'; c: CSTII_array),
    (name: 'AS'; c: CSTII_as),
    (name: 'BEGIN'; c: CSTII_begin),
    (name: 'BREAK'; c: CSTII_break),
    (name: 'CASE'; c: CSTII_case),
    (name: 'CHR'; c: CSTII_chr),
    (name: 'CLASS'; c: CSTII_class),
    (name: 'CONST'; c: CSTII_const),
    (name: 'CONSTRUCTOR'; c: CSTII_constructor),
    (name: 'CONTINUE'; c: CSTII_Continue),
    (name: 'DESTRUCTOR'; c: CSTII_destructor),
    (name: 'DIV'; c: CSTII_div),
    (name: 'DO'; c: CSTII_do),
    (name: 'DOWNTO'; c: CSTII_downto),
    (name: 'ELSE'; c: CSTII_else),
    (name: 'END'; c: CSTII_end),
    (name: 'EXCEPT'; c: CSTII_except),
    (name: 'EXIT'; c: CSTII_exit),
    (name: 'EXPORT'; c: CSTII_Export),
    (name: 'EXTERNAL'; c: CSTII_External),
    (name: 'FINALLY'; c: CSTII_finally),
    (name: 'FOR'; c: CSTII_for),
    (name: 'FORWARD'; c: CSTII_Forward),
    (name: 'FUNCTION'; c: CSTII_function),
    (name: 'GOTO'; c: CSTII_Goto),
    (name: 'IF'; c: CSTII_if),
    (name: 'IMPLEMENTATION'; c: CSTII_Implementation),
    (name: 'IN'; c: CSTII_in),
    (name: 'INHERITED'; c: CSTII_inherited),
    (name: 'INTERFACE'; c: CSTII_Interface),
    (name: 'IS'; c: CSTII_is),
    (name: 'LABEL'; c: CSTII_Label),
    (name: 'MOD'; c: CSTII_mod),
    (name: 'NOT'; c: CSTII_not),
    (name: 'OF'; c: CSTII_of),
    (name: 'OR'; c: CSTII_or),
    (name: 'ORD'; c: CSTII_ord),
    (name: 'OVERRIDE'; c: CSTII_override),
    (name: 'PRIVATE'; c: CSTII_private),
    (name: 'PROCEDURE'; c: CSTII_procedure),
    (name: 'PROGRAM'; c: CSTII_program),
    (name: 'PROPERTY'; c: CSTII_property),
    (name: 'PROTECTED'; c: CSTII_protected),
    (name: 'PUBLIC'; c: CSTII_public),
    (name: 'PUBLISHED'; c: CSTII_published),
    (name: 'RECORD'; c: CSTII_record),
    (name: 'REPEAT'; c: CSTII_repeat),
    (name: 'SET'; c: CSTII_set),
    (name: 'SHL'; c: CSTII_shl),
    (name: 'SHR'; c: CSTII_shr),
    (name: 'THEN'; c: CSTII_then),
    (name: 'TO'; c: CSTII_to),
    (name: 'TRY'; c: CSTII_try),
    (name: 'TYPE'; c: CSTII_type),
    (name: 'UNIT'; c: CSTII_Unit),
    (name: 'UNTIL'; c: CSTII_until),
    (name: 'USES'; c: CSTII_uses),
    (name: 'VAR'; c: CSTII_var),
    (name: 'VIRTUAL'; c: CSTII_virtual),
    (name: 'WHILE'; c: CSTII_while),
    (name: 'WITH'; c: CSTII_with),
    (name: 'XOR'; c: CSTII_xor));

PROCEDURE TSCPascalParser.Next;
VAR
  Err: TSCParserErrorKind;
  FUNCTION CheckReserved(CONST S: ShortString; VAR CurrTokenId: TSCToken): Boolean;
  VAR
    L, H, I: LongInt;
    J: Char;
    SName: ShortString;
  BEGIN
    L := 0;
    J := S[0];
    H := PAS_KEYWORD_COUNT - 1;
    WHILE L <= H DO
      BEGIN
        I := (L + H) SHR 1;
        SName := PasLookupTable[i].Name;
        IF J = SName[0] THEN
          BEGIN
            IF S = SName THEN
              BEGIN
                CheckReserved := True;
                CurrTokenId := PasLookupTable[I].c;
                Exit;
              END;
            IF S > SName THEN
              L := I + 1
            ELSE
              H := I - 1;
          END ELSE
          IF S > SName THEN
            L := I + 1
          ELSE
            H := I - 1;
      END;
    CheckReserved := False;
  END;

  FUNCTION GetToken(CurrTokenPos, CurrTokenLen: Cardinal): STRING;
  VAR
    s: STRING;
  BEGIN
    SetLength(s, CurrTokenLen);
    Move(FText[CurrTokenPos], S[1], CurrtokenLen);
    GetToken := s;
  END;

  FUNCTION ParseToken(VAR CurrTokenPos, CurrTokenLen: Cardinal; VAR CurrTokenId: TSCToken): TSCParserErrorKind;
  VAR
    ct, ci: Cardinal;
    hs: Boolean;
  BEGIN
    ParseToken := iNoError;
    ct := CurrTokenPos;
    CASE FText[ct] OF
      #0:
        BEGIN
          CurrTokenId := CSTI_EOF;
          CurrTokenLen := 0;
        END;
      'A'..'Z', 'a'..'z', '_':
        BEGIN
          ci := ct + 1;
          WHILE (FText[ci] IN ['_', '0'..'9', 'a'..'z', 'A'..'Z']) DO BEGIN
              Inc(ci);
            END;
          CurrTokenLen := ci - ct;
          IF NOT CheckReserved(FastUppercase(GetToken(CurrTokenPos, CurrtokenLen)), CurrTokenId) THEN
            BEGIN
              CurrTokenId := CSTI_Identifier;
            END;
        END;
      '$':
        BEGIN
          ci := ct + 1;

          WHILE (FText[ci] IN ['0'..'9', 'a'..'f', 'A'..'F'])
            DO Inc(ci);

          CurrTokenId := CSTI_HexInt;
          CurrTokenLen := ci - ct;
        END;

      '0'..'9':
        BEGIN
          hs := False;
          ci := ct;
          WHILE (FText[ci] IN ['0'..'9']) DO
            BEGIN
              Inc(ci);
              IF (FText[ci] = '.') AND (NOT hs) THEN
                BEGIN
                  IF FText[ci + 1] = '.' THEN break;
                  hs := True;
                  Inc(ci);
                END;
            END;

          IF hs
            THEN CurrTokenId := CSTI_Real
          ELSE CurrTokenId := CSTI_Integer;

          CurrTokenLen := ci - ct;
        END;


      #39:
        BEGIN
          ci := ct + 1;
          WHILE (FText[ci] <> #0) AND (FText[ci] <> #13) AND
            (FText[ci] <> #10) AND (FText[ci] <> #39)
            DO BEGIN
              Inc(ci);
            END;
          IF FText[ci] = #39 THEN
            CurrTokenId := CSTI_String
          ELSE
            BEGIN
              CurrTokenId := CSTI_String;
              ParseToken := iStringError;
            END;
          CurrTokenLen := ci - ct + 1;
        END;
      '#':
        BEGIN
          ci := ct + 1;
          IF FText[ci] = '$' THEN
            BEGIN
              inc(ci);
              WHILE (FText[ci] IN ['A'..'F', 'a'..'f', '0'..'9']) DO BEGIN
                  Inc(ci);
                END;
              CurrTokenId := CSTI_Char;
              CurrTokenLen := ci - ct;
            END ELSE
            BEGIN
              WHILE (FText[ci] IN ['0'..'9']) DO BEGIN
                  Inc(ci);
                END;
              IF FText[ci] IN ['A'..'Z', 'a'..'z', '_'] THEN
                BEGIN
                  ParseToken := iCharError;
                  CurrTokenId := CSTI_Char;
                END ELSE
                CurrTokenId := CSTI_Char;
              CurrTokenLen := ci - ct;
            END;
        END;
      '=':
        BEGIN
          CurrTokenId := CSTI_Equal;
          CurrTokenLen := 1;
        END;
      '>':
        BEGIN
          IF FText[ct + 1] = '=' THEN
            BEGIN
              CurrTokenid := CSTI_GreaterEqual;
              CurrTokenLen := 2;
            END ELSE
            BEGIN
              CurrTokenid := CSTI_Greater;
              CurrTokenLen := 1;
            END;
        END;
      '<':
        BEGIN
          IF FText[ct + 1] = '=' THEN
            BEGIN
              CurrTokenId := CSTI_LessEqual;
              CurrTokenLen := 2;
            END ELSE
            IF FText[ct + 1] = '>' THEN
              BEGIN
                CurrTokenId := CSTI_NotEqual;
                CurrTokenLen := 2;
              END ELSE
              BEGIN
                CurrTokenId := CSTI_Less;
                CurrTokenLen := 1;
              END;
        END;
      ')':
        BEGIN
          CurrTokenId := CSTI_CloseRound;
          CurrTokenLen := 1;
        END;
      '(':
        BEGIN
          IF FText[ct + 1] = '*' THEN
            BEGIN
              ci := ct + 1;
              WHILE (FText[ci] <> #0) DO BEGIN
                  IF (FText[ci] = '*') AND (FText[ci + 1] = ')') THEN
                    Break;
                  Inc(ci);
                END;
              IF (FText[ci] = #0) THEN
                BEGIN
                  CurrTokenId := CSTIINT_Comment;
                  ParseToken := iCommentError;
                END ELSE
                BEGIN
                  CurrTokenId := CSTIINT_Comment;
                  Inc(ci, 2);
                END;
              CurrTokenLen := ci - ct;
            END
          ELSE
            BEGIN
              CurrTokenId := CSTI_OpenRound;
              CurrTokenLen := 1;
            END;
        END;
      '[':
        BEGIN
          CurrTokenId := CSTI_OpenBlock;
          CurrTokenLen := 1;
        END;
      ']':
        BEGIN
          CurrTokenId := CSTI_CloseBlock;
          CurrTokenLen := 1;
        END;
      ',':
        BEGIN
          CurrTokenId := CSTI_Comma;
          CurrTokenLen := 1;
        END;
      '.':
        BEGIN
          CurrTokenId := CSTI_Period;
          CurrTokenLen := 1;
        END;
      '@':
        BEGIN
          CurrTokenId := CSTI_AddressOf;
          CurrTokenLen := 1;
        END;
      '^':
        BEGIN
          CurrTokenId := CSTI_Dereference;
          CurrTokenLen := 1;
        END;
      ';':
        BEGIN
          CurrTokenId := CSTI_Semicolon;
          CurrTokenLen := 1;
        END;
      ':':
        BEGIN
          IF FText[ct + 1] = '=' THEN
            BEGIN
              CurrTokenId := CSTI_Assignment;
              CurrTokenLen := 2;
            END ELSE
            BEGIN
              CurrTokenId := CSTI_Colon;
              CurrTokenLen := 1;
            END;
        END;
      '+':
        BEGIN
          CurrTokenId := CSTI_Plus;
          CurrTokenLen := 1;
        END;
      '-':
        BEGIN
          CurrTokenId := CSTI_Minus;
          CurrTokenLen := 1;
        END;
      '*':
        BEGIN
          CurrTokenId := CSTI_Multiply;
          CurrTokenLen := 1;
        END;
      '/':
        BEGIN
          IF FText[ct + 1] = '/' THEN
            BEGIN
              ci := ct + 1;
              WHILE (FText[ci] <> #0) AND (FText[ci] <> #13) AND
                (FText[ci] <> #10) DO BEGIN
                  Inc(ci);
                END;
              IF (FText[ci] = #0) THEN
                BEGIN
                  CurrTokenId := CSTIINT_Comment;
                  ParseToken := iCommentError;
                END ELSE
                BEGIN
                  IF FText[ci + 1] = #10 THEN
                    Inc(ci) ELSE

                    IF FText[ci + 1] = #13 THEN
                      Inc(ci);
                  CurrTokenId := CSTIINT_Comment;
                END;
              CurrTokenLen := ci - ct + 1;
            END ELSE
            BEGIN
              CurrTokenId := CSTI_Divide;
              CurrTokenLen := 1;
            END;
        END;
      #32, #9, #13, #10:
        BEGIN
          ci := ct + 1;
          WHILE (FText[ci] IN [#32, #9, #13, #10]) DO BEGIN
              Inc(ci);
            END;
          CurrTokenId := CSTIINT_WhiteSpace;
          CurrTokenLen := ci - ct;
        END;
      '{':
        BEGIN
          ci := ct + 1;
          WHILE (FText[ci] <> #0) AND (FText[ci] <> '}') DO BEGIN
              Inc(ci);
            END;
          IF (FText[ci] = #0) THEN
            BEGIN
              CurrTokenId := CSTIINT_Comment;
              ParseToken := iCommentError;
            END ELSE
            CurrTokenId := CSTIINT_Comment;
          CurrTokenLen := ci - ct + 1;
        END;
    ELSE
      BEGIN
        ParseToken := iSyntaxError;
        CurrTokenId := CSTIINT_Comment;
        CurrTokenLen := 1;
      END;
    END;
  END;

BEGIN
  IF FText = NIL THEN
    BEGIN
      FTokenLength := 0;
      FRealPosition := 0;
      FTokenId := CSTI_EOF;
      Exit;
    END;
  REPEAT
    FRealPosition := FRealPosition + FTokenLength;
    Err := ParseToken(FRealPosition, FTokenLength, FTokenID);
    IF Err <> iNoError THEN
      BEGIN
        FTokenLength := 0;
        FTokenId := CSTI_EOF;
        FToken := '';
        FOriginalToken := '';
        IF @FParserError <> NIL THEN FParserError(Self, Err, FRealPosition);
        exit;
      END;
    CASE FTokenID OF
      CSTIINT_Comment, CSTIINT_WhiteSpace: Continue;
      CSTI_Integer, CSTI_Real, CSTI_String, CSTI_Char, CSTI_HexInt:
        BEGIN
          FOriginalToken := GetToken(FRealPosition, FTokenLength);
          FToken := FOriginalToken;
        END;
      CSTI_Identifier:
        BEGIN
          FOriginalToken := GetToken(FRealPosition, FTokenLength);
          FToken := FastUppercase(FOriginalToken);
        END;
    ELSE
      BEGIN
        FOriginalToken := '';
        FToken := '';
      END;
    END;
    Break;
  UNTIL False;
END;

PROCEDURE TSCPascalParser.SetText(CONST Data: STRING);
BEGIN
  FData := Data;
  FText := Pointer(FData);
  FTokenLength := 0;
  FRealPosition := 0;
  FTokenId := CSTI_EOF;
  Next;
END;

END.

