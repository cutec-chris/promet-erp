{
xlsbiff2.pas

Writes an Excel 2.x file

Excel 2.x files support only one Worksheet per Workbook, so only the first
will be written.

An Excel file consists of a number of subsequent records.
To ensure a properly formed file, the following order must be respected:

1st record:        BOF
2nd to Nth record: Any record
Last record:       EOF

The row and column numbering in BIFF files is zero-based.

Excel file format specification obtained from:

http://sc.openoffice.org/excelfileformat.pdf

Encoding information: ISO_8859_1 is used, to have support to
other characters, please use a format which support unicode

AUTHORS: Felipe Monteiro de Carvalho
}
unit xlsbiff2;

{$ifdef fpc}
  {$mode delphi}
{$endif}

interface

uses
  Classes, SysUtils,
  fpspreadsheet, xlscommon, fpsutils, lconvencoding;
  
type

  { TsSpreadBIFF2Reader }

  TsSpreadBIFF2Reader = class(TsCustomSpreadReader)
  private
    WorkBookEncoding: TsEncoding;
    RecordSize: Word;
    FWorksheet: TsWorksheet;
  public
    { General reading methods }
    procedure ReadFromStream(AStream: TStream; AData: TsWorkbook); override;
    { Record writing methods }
    procedure ReadFormula(AStream: TStream); override;
    procedure ReadLabel(AStream: TStream); override;
    procedure ReadNumber(AStream: TStream); override;
    procedure ReadInteger(AStream: TStream);
  end;

  { TsSpreadBIFF2Writer }

  TsSpreadBIFF2Writer = class(TsCustomSpreadWriter)
  private
    function  FEKindToExcelID(AElement: TFEKind; var AParamsNum, AFuncNum: Byte): Byte;
    procedure WriteCellFormatting(AStream: TStream; ACell: PCell);
  public
    { General writing methods }
    procedure WriteToStream(AStream: TStream; AData: TsWorkbook); override;
    { Record writing methods }
    procedure WriteBOF(AStream: TStream);
    procedure WriteEOF(AStream: TStream);
    procedure WriteRPNFormula(AStream: TStream; const ARow, ACol: Word; const AFormula: TsRPNFormula; ACell: PCell); override;
    procedure WriteLabel(AStream: TStream; const ARow, ACol: Word; const AValue: string; ACell: PCell); override;
    procedure WriteNumber(AStream: TStream; const ARow, ACol: Cardinal; const AValue: double; ACell: PCell); override;
  end;

implementation

const
  { Excel record IDs }
  INT_EXCEL_ID_INTEGER    = $0002;
  INT_EXCEL_ID_NUMBER     = $0003;
  INT_EXCEL_ID_LABEL      = $0004;
  INT_EXCEL_ID_FORMULA    = $0006;
  INT_EXCEL_ID_BOF        = $0009;
  INT_EXCEL_ID_EOF        = $000A;

  { Cell Addresses constants }
  MASK_EXCEL_ROW          = $3FFF;
  MASK_EXCEL_RELATIVE_ROW = $4000;
  MASK_EXCEL_RELATIVE_COL = $8000;

  { BOF record constants }
  INT_EXCEL_SHEET         = $0010;
  INT_EXCEL_CHART         = $0020;
  INT_EXCEL_MACRO_SHEET   = $0040;

{ TsSpreadBIFF2Writer }

function TsSpreadBIFF2Writer.FEKindToExcelID(AElement: TFEKind; var AParamsNum, AFuncNum: Byte): Byte;
begin
  AFuncNum := 0;

  case AElement of
  { Operands }
  fekCell: Result := INT_EXCEL_TOKEN_TREFV;
  fekNum: Result := INT_EXCEL_TOKEN_TNUM;
  { Operators }
  fekAdd:  Result := INT_EXCEL_TOKEN_TADD;
  fekSub:  Result := INT_EXCEL_TOKEN_TSUB;
  fekDiv:  Result := INT_EXCEL_TOKEN_TDIV;
  fekMul:  Result := INT_EXCEL_TOKEN_TMUL;
  { Build-in functions }
  fekABS:
  begin
    Result := INT_EXCEL_TOKEN_FUNCVAR_V;
    AParamsNum := 1;
    AFuncNum := INT_EXCEL_SHEET_FUNC_ABS;
  end;
  fekROUND:
  begin
    Result := INT_EXCEL_TOKEN_FUNCVAR_V;
    AParamsNum := 2;
    AFuncNum := INT_EXCEL_SHEET_FUNC_ROUND;
  end;
  end;
end;

procedure TsSpreadBIFF2Writer.WriteCellFormatting(AStream: TStream; ACell: PCell);
var
  BorderByte: Byte = 0;
begin
  if ACell^.UsedFormattingFields = [] then
  begin
    AStream.WriteByte($0);
    AStream.WriteByte($0);
    AStream.WriteByte($0);
    Exit;
  end;

  AStream.WriteByte($0);
  AStream.WriteByte($0);

  // The Border and Background

  BorderByte := 0;

  if uffBorder in ACell^.UsedFormattingFields then
  begin
    if cbNorth in ACell^.Border then BorderByte := BorderByte or $20;
    if cbWest in ACell^.Border then BorderByte := BorderByte or $08;
    if cbEast in ACell^.Border then BorderByte := BorderByte or $10;
    if cbSouth in ACell^.Border then BorderByte := BorderByte or $40;
  end;

  // BIFF2 does not support a background color, just a "shaded" option
  if uffBackgroundColor in ACell^.UsedFormattingFields then
    BorderByte := BorderByte or $80;

  AStream.WriteByte(BorderByte);
end;

{
  Writes an Excel 2 file to a stream

  Excel 2.x files support only one Worksheet per Workbook,
  so only the first will be written.
}
procedure TsSpreadBIFF2Writer.WriteToStream(AStream: TStream; AData: TsWorkbook);
begin
  WriteBOF(AStream);

  WriteCellsToStream(AStream, AData.GetFirstWorksheet.Cells);

  WriteEOF(AStream);
end;

{
  Writes an Excel 2 BOF record

  This must be the first record on an Excel 2 stream
}
procedure TsSpreadBIFF2Writer.WriteBOF(AStream: TStream);
begin
  { BIFF Record header }
  AStream.WriteWord(WordToLE(INT_EXCEL_ID_BOF));
  AStream.WriteWord(WordToLE($0004));

  { Unused }
  AStream.WriteWord($0000);

  { Data type }
  AStream.WriteWord(WordToLE(INT_EXCEL_SHEET));
end;

{
  Writes an Excel 2 EOF record

  This must be the last record on an Excel 2 stream
}
procedure TsSpreadBIFF2Writer.WriteEOF(AStream: TStream);
begin
  { BIFF Record header }
  AStream.WriteWord(WordToLE(INT_EXCEL_ID_EOF));
  AStream.WriteWord($0000);
end;

{
  Writes an Excel 2 FORMULA record

  The formula needs to be converted from usual user-readable string
  to an RPN array

  // or, in RPN: A1, B1, +
  SetLength(MyFormula, 3);
  MyFormula[0].TokenID := INT_EXCEL_TOKEN_TREFV; A1
  MyFormula[0].Col := 0;
  MyFormula[0].Row := 0;
  MyFormula[1].TokenID := INT_EXCEL_TOKEN_TREFV; B1
  MyFormula[1].Col := 1;
  MyFormula[1].Row := 0;
  MyFormula[2].TokenID := INT_EXCEL_TOKEN_TADD;  +
}
procedure TsSpreadBIFF2Writer.WriteRPNFormula(AStream: TStream; const ARow,
  ACol: Word; const AFormula: TsRPNFormula; ACell: PCell);
var
  FormulaResult: double;
  i: Integer;
  RPNLength: Word;
  TokenArraySizePos, RecordSizePos, FinalPos: Cardinal;
  FormulaKind, ParamsNum, ExtraInfo: Byte;
begin
  RPNLength := 0;
  FormulaResult := 0.0;

  { BIFF Record header }
  AStream.WriteWord(WordToLE(INT_EXCEL_ID_FORMULA));
  RecordSizePos := AStream.Position;
  AStream.WriteWord(WordToLE(17 + RPNLength));

  { BIFF Record data }
  AStream.WriteWord(WordToLE(ARow));
  AStream.WriteWord(WordToLE(ACol));

  { BIFF2 Attributes }
  AStream.WriteByte($0);
  AStream.WriteByte($0);
  AStream.WriteByte($0);

  { Result of the formula in IEE 754 floating-point value }
  AStream.WriteBuffer(FormulaResult, 8);

  { 0 = Do not recalculate
    1 = Always recalculate }
  AStream.WriteByte($1);

  { Formula }

  { The size of the token array is written later,
    because it's necessary to calculate if first,
    and this is done at the same time it is written }
  TokenArraySizePos := AStream.Position;
  AStream.WriteByte(RPNLength);

  { Formula data (RPN token array) }
  for i := 0 to Length(AFormula) - 1 do
  begin
    { Token identifier }
    FormulaKind := FEKindToExcelID(AFormula[i].ElementKind, ParamsNum, ExtraInfo);
    AStream.WriteByte(FormulaKind);
    Inc(RPNLength);

    { Additional data }
    case FormulaKind of

    { binary operation tokens }

    INT_EXCEL_TOKEN_TADD, INT_EXCEL_TOKEN_TSUB, INT_EXCEL_TOKEN_TMUL,
     INT_EXCEL_TOKEN_TDIV, INT_EXCEL_TOKEN_TPOWER: begin end;

    INT_EXCEL_TOKEN_TNUM:
    begin
      AStream.WriteBuffer(AFormula[i].DoubleValue, 8);
      Inc(RPNLength, 8);
    end;

    INT_EXCEL_TOKEN_TREFR, INT_EXCEL_TOKEN_TREFV, INT_EXCEL_TOKEN_TREFA:
    begin
      AStream.WriteWord(AFormula[i].Row and MASK_EXCEL_ROW);
      AStream.WriteByte(AFormula[i].Col);
      Inc(RPNLength, 3);
    end;

    INT_EXCEL_TOKEN_FUNCVAR_V:
    begin
      AStream.WriteByte(ParamsNum);
      AStream.WriteByte(ExtraInfo);
      Inc(RPNLength, 2);
    end;

    end;
  end;

  { Write sizes in the end, after we known them }
  FinalPos := AStream.Position;
  AStream.position := TokenArraySizePos;
  AStream.WriteByte(RPNLength);
  AStream.Position := RecordSizePos;
  AStream.WriteWord(WordToLE(17 + RPNLength));
  AStream.position := FinalPos;
end;

{*******************************************************************
*  TsSpreadBIFF2Writer.WriteLabel ()
*
*  DESCRIPTION:    Writes an Excel 2 LABEL record
*
*                  Writes a string to the sheet
*
*******************************************************************}
procedure TsSpreadBIFF2Writer.WriteLabel(AStream: TStream; const ARow,
  ACol: Word; const AValue: string; ACell: PCell);
var
  L: Byte;
  AnsiText: ansistring;
begin
  if AValue = '' then Exit; // Writing an empty text doesn't work

  AnsiText := UTF8ToISO_8859_1(AValue);
  L := Length(AnsiText);

  { BIFF Record header }
  AStream.WriteWord(WordToLE(INT_EXCEL_ID_LABEL));
  AStream.WriteWord(WordToLE(8 + L));

  { BIFF Record data }
  AStream.WriteWord(WordToLE(ARow));
  AStream.WriteWord(WordToLE(ACol));

  { BIFF2 Attributes }
  WriteCellFormatting(AStream, ACell);

  { String with 8-bit size }
  AStream.WriteByte(L);
  AStream.WriteBuffer(AnsiText[1], L);
end;

{*******************************************************************
*  TsSpreadBIFF2Writer.WriteNumber ()
*
*  DESCRIPTION:    Writes an Excel 2 NUMBER record
*
*                  Writes a number (64-bit IEE 754 floating point) to the sheet
*
*******************************************************************}
procedure TsSpreadBIFF2Writer.WriteNumber(AStream: TStream; const ARow,
  ACol: Cardinal; const AValue: double; ACell: PCell);
begin
  { BIFF Record header }
  AStream.WriteWord(WordToLE(INT_EXCEL_ID_NUMBER));
  AStream.WriteWord(WordToLE(15));

  { BIFF Record data }
  AStream.WriteWord(WordToLE(ARow));
  AStream.WriteWord(WordToLE(ACol));

  { BIFF2 Attributes }
  AStream.WriteByte($0);
  AStream.WriteByte($0);
  AStream.WriteByte($0);

  { IEE 754 floating-point value }
  AStream.WriteBuffer(AValue, 8);
end;

{ TsSpreadBIFF2Reader }

procedure TsSpreadBIFF2Reader.ReadFromStream(AStream: TStream; AData: TsWorkbook);
var
  BIFF2EOF: Boolean;
  RecordType: Word;
  CurStreamPos: Int64;
begin
  { Store some data about the workbook that other routines need }
  WorkBookEncoding := AData.Encoding;

  BIFF2EOF := False;

  { In BIFF2 files there is only one worksheet, let's create it }
  FWorksheet := AData.AddWorksheet('');

  { Read all records in a loop }
  while not BIFF2EOF do
  begin
    { Read the record header }
    RecordType := WordLEToN(AStream.ReadWord);
    RecordSize := WordLEToN(AStream.ReadWord);

    CurStreamPos := AStream.Position;

    case RecordType of

    INT_EXCEL_ID_INTEGER: ReadInteger(AStream);
    INT_EXCEL_ID_NUMBER:  ReadNumber(AStream);
    INT_EXCEL_ID_LABEL:   ReadLabel(AStream);
    INT_EXCEL_ID_FORMULA: ReadFormula(AStream);
    INT_EXCEL_ID_BOF:     ;
    INT_EXCEL_ID_EOF:     BIFF2EOF := True;

    else
      // nothing
    end;

    // Make sure we are in the right position for the next record
    AStream.Seek(CurStreamPos + RecordSize, soFromBeginning);

    if AStream.Position >= AStream.Size then BIFF2EOF := True;
  end;
end;

procedure TsSpreadBIFF2Reader.ReadFormula(AStream: TStream);
begin

end;

procedure TsSpreadBIFF2Reader.ReadLabel(AStream: TStream);
var
  L: Byte;
  ARow, ACol: Word;
  AValue: array[0..255] of Char;
  AStrValue: UTF8String;
begin
  { BIFF Record data }
  ARow := WordLEToN(AStream.ReadWord);
  ACol := WordLEToN(AStream.ReadWord);

  { BIFF2 Attributes }
  AStream.ReadByte();
  AStream.ReadByte();
  AStream.ReadByte();

  { String with 8-bit size }
  L := AStream.ReadByte();
  AStream.ReadBuffer(AValue, L);
  AValue[L] := #0;

  { Save the data }
  case WorkBookEncoding of
  seLatin2:   AStrValue := CP1250ToUTF8(AValue);
  seCyrillic: AStrValue := CP1251ToUTF8(AValue);
  seGreek:    AStrValue := CP1253ToUTF8(AValue);
  seTurkish:  AStrValue := CP1254ToUTF8(AValue);
  seHebrew:   AStrValue := CP1255ToUTF8(AValue);
  seArabic:   AStrValue := CP1256ToUTF8(AValue);
  else
    // Latin 1 is the default
    AStrValue := CP1252ToUTF8(AValue);
  end;
  FWorksheet.WriteUTF8Text(ARow, ACol, AStrValue);
end;

procedure TsSpreadBIFF2Reader.ReadNumber(AStream: TStream);
var
  ARow, ACol: Word;
  AValue: Double;
begin
  { BIFF Record data }
  ARow := WordLEToN(AStream.ReadWord);
  ACol := WordLEToN(AStream.ReadWord);

  { BIFF2 Attributes }
  AStream.ReadByte();
  AStream.ReadByte();
  AStream.ReadByte();

  { IEE 754 floating-point value }
  AStream.ReadBuffer(AValue, 8);

  { Save the data }
  FWorksheet.WriteNumber(ARow, ACol, AValue);
end;

procedure TsSpreadBIFF2Reader.ReadInteger(AStream: TStream);
var
  ARow, ACol: Word;
  AWord  : Word;
begin
  { BIFF Record data }
  ARow := WordLEToN(AStream.ReadWord);
  ACol := WordLEToN(AStream.ReadWord);

  { BIFF2 Attributes }
  AStream.ReadByte();
  AStream.ReadByte();
  AStream.ReadByte();

  { 16 bit unsigned integer }
  AStream.ReadBuffer(AWord, 2);

  { Save the data }
  FWorksheet.WriteNumber(ARow, ACol, AWord);
end;

{*******************************************************************
*  Initialization section
*
*  Registers this reader / writer on fpSpreadsheet
*
*******************************************************************}

initialization

  RegisterSpreadFormat(TsSpreadBIFF2Reader, TsSpreadBIFF2Writer, sfExcel2);

end.
