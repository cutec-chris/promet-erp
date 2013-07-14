{
xlsbiff5.pas

Writes an Excel 5 file

An Excel worksheet stream consists of a number of subsequent records.
To ensure a properly formed file, the following order must be respected:

1st record:        BOF
2nd to Nth record: Any record
Last record:       EOF

Excel 5 files are OLE compound document files, and must be written using the
fpOLE library.

Records Needed to Make a BIFF5 File Microsoft Excel Can Use:

Required Records:

BOF - Set the 6 byte offset to 0x0005 (workbook globals)
Window1
FONT - At least five of these records must be included
XF - At least 15 Style XF records and 1 Cell XF record must be included
STYLE
BOUNDSHEET - Include one BOUNDSHEET record per worksheet
EOF

BOF - Set the 6 byte offset to 0x0010 (worksheet)
INDEX
DIMENSIONS
WINDOW2
EOF

The row and column numbering in BIFF files is zero-based.

Excel file format specification obtained from:

http://sc.openoffice.org/excelfileformat.pdf

Records Needed to Make a BIFF5 File Microsoft Excel Can Use obtained from:

http://support.microsoft.com/default.aspx?scid=KB;EN-US;Q147732&ID=KB;EN-US;Q147732&LN=EN-US&rnk=2&SD=msdn&FR=0&qry=BIFF&src=DHCS_MSPSS_msdn_SRCH&SPR=MSALL&

Microsoft BIFF 5 writer example:

http://support.microsoft.com/kb/150447/en-us

Encoding information: ISO_8859_1 is used, to have support to
other characters, please use a format which support unicode

AUTHORS: Felipe Monteiro de Carvalho
}
unit xlsbiff5;

{$ifdef fpc}
  {$mode delphi}
{$endif}

{$define USE_NEW_OLE}

interface

uses
  Classes, SysUtils, fpcanvas,
  fpspreadsheet,
  xlscommon,
  {$ifdef USE_NEW_OLE}
  fpolebasic,
  {$else}
  fpolestorage,
  {$endif}
  fpsutils, lconvencoding;

type

  { TsSpreadBIFF5Reader }

  TsSpreadBIFF5Reader = class(TsSpreadBIFFReader)
  private
    RecordSize: Word;
    FWorksheet: TsWorksheet;
    FWorksheetNames: TStringList;
    FCurrentWorksheet: Integer;
    procedure ReadWorkbookGlobals(AStream: TStream; AData: TsWorkbook);
    procedure ReadWorksheet(AStream: TStream; AData: TsWorkbook);
    procedure ReadBoundsheet(AStream: TStream);
    procedure ReadRichString(AStream: TStream);
    procedure ReadRKValue(AStream: TStream);
    procedure ReadMulRKValues(AStream: TStream);
    procedure ReadFormulaExcel(AStream: TStream);
  protected
    function DecodeRKValue(const ARK: DWORD): Double;
    procedure ReadRowColXF(const AStream: TStream; out ARow,ACol,AXF: WORD); virtual;
  public
    { General reading methods }
    procedure ReadFromFile(AFileName: string; AData: TsWorkbook); override;
    procedure ReadFromStream(AStream: TStream; AData: TsWorkbook); override;
    { Record writing methods }
    procedure ReadFormula(AStream: TStream); override;
    procedure ReadLabel(AStream: TStream); override;
    procedure ReadNumber(AStream: TStream); override;
  end;

  { TsSpreadBIFF5Writer }

  TsSpreadBIFF5Writer = class(TsSpreadBIFFWriter)
  private
    WorkBookEncoding: TsEncoding;
    function  FEKindToExcelID(AElement: TFEKind; var AParamsNum: Byte; var AExtra: Word): Byte;
  public
//    constructor Create;
//    destructor Destroy; override;
    { General writing methods }
    procedure WriteToFile(const AFileName: string; AData: TsWorkbook;
      const AOverwriteExisting: Boolean = False); override;
    procedure WriteToStream(AStream: TStream; AData: TsWorkbook); override;
    { Record writing methods }
    procedure WriteBOF(AStream: TStream; ADataType: Word);
    function  WriteBoundsheet(AStream: TStream; ASheetName: string): Int64;
    //procedure WriteCodepage(AStream: TStream; AEncoding: TsEncoding); this is in xlscommon
    procedure WriteDimensions(AStream: TStream; AWorksheet: TsWorksheet);
    procedure WriteEOF(AStream: TStream);
    procedure WriteFont(AStream: TStream;  AFont: TFPCustomFont);
    procedure WriteRPNFormula(AStream: TStream; const ARow, ACol: Word; const AFormula: TsRPNFormula; ACell: PCell); override;
    procedure WriteIndex(AStream: TStream);
    procedure WriteLabel(AStream: TStream; const ARow, ACol: Word; const AValue: string; ACell: PCell); override;
    procedure WriteNumber(AStream: TStream; const ARow, ACol: Cardinal; const AValue: double; ACell: PCell); override;
    procedure WriteStyle(AStream: TStream);
    procedure WriteWindow1(AStream: TStream);
    procedure WriteWindow2(AStream: TStream; ASheetSelected: Boolean);
    procedure WriteXF(AStream: TStream; AFontIndex: Word; AXF_TYPE_PROT: Byte);
  end;

implementation

const
  { Excel record IDs }
  INT_EXCEL_ID_BOF        = $0809;
  INT_EXCEL_ID_BOUNDSHEET = $0085; // Renamed to SHEET in the latest OpenOffice docs
  INT_EXCEL_ID_EOF        = $000A;
  INT_EXCEL_ID_DIMENSIONS = $0200;
  INT_EXCEL_ID_FONT       = $0031;
  INT_EXCEL_ID_FORMULA    = $0006;
  INT_EXCEL_ID_INDEX      = $020B;
  INT_EXCEL_ID_LABEL      = $0204;
  INT_EXCEL_ID_NUMBER     = $0203;
  INT_EXCEL_ID_STYLE      = $0293;
  INT_EXCEL_ID_WINDOW1    = $003D;
  INT_EXCEL_ID_WINDOW2    = $023E;
  INT_EXCEL_ID_XF         = $00E0;
  INT_EXCEL_ID_RSTRING    = $00D6;
  INT_EXCEL_ID_RK         = $027E;
  INT_EXCEL_ID_MULRK      = $00BD;
  INT_EXCEL_ID_CODEPAGE   = xlscommon.INT_EXCEL_ID_CODEPAGE;

  { Cell Addresses constants }
  MASK_EXCEL_ROW          = $3FFF;
  MASK_EXCEL_RELATIVE_ROW = $4000;
  MASK_EXCEL_RELATIVE_COL = $8000;

  { BOF record constants }
  INT_BOF_BIFF5_VER       = $0500;
  INT_BOF_WORKBOOK_GLOBALS= $0005;
  INT_BOF_VB_MODULE       = $0006;
  INT_BOF_SHEET           = $0010;
  INT_BOF_CHART           = $0020;
  INT_BOF_MACRO_SHEET     = $0040;
  INT_BOF_WORKSPACE       = $0100;
  INT_BOF_BUILD_ID        = $1FD2;
  INT_BOF_BUILD_YEAR      = $07CD;

  { FONT record constants }
  INT_FONT_WEIGHT_NORMAL  = $0190;

  BYTE_ANSILatin1         = $00;
  BYTE_SYSTEM_DEFAULT     = $01;
  BYTE_SYMBOL             = $02;
  BYTE_Apple_Roman        = $4D;
  BYTE_ANSI_Japanese_Shift_JIS = $80;
  BYTE_ANSI_Korean_Hangul = $81;
  BYTE_ANSI_Korean_Johab  = $81;
  BYTE_ANSI_Chinese_Simplified_GBK = $86;
  BYTE_ANSI_Chinese_Traditional_BIG5 = $88;
  BYTE_ANSI_Greek         = $A1;
  BYTE_ANSI_Turkish       = $A2;
  BYTE_ANSI_Vietnamese    = $A3;
  BYTE_ANSI_Hebrew        = $B1;
  BYTE_ANSI_Arabic        = $B2;
  BYTE_ANSI_Baltic        = $BA;
  BYTE_ANSI_Cyrillic      = $CC;
  BYTE_ANSI_Thai          = $DE;
  BYTE_ANSI_Latin2        = $EE;
  BYTE_OEM_Latin1         = $FF;

  { FORMULA record constants }
  MASK_FORMULA_RECALCULATE_ALWAYS  = $0001;
  MASK_FORMULA_RECALCULATE_ON_OPEN = $0002;
  MASK_FORMULA_SHARED_FORMULA      = $0008;

  { STYLE record constants }
  MASK_STYLE_BUILT_IN     = $8000;

  { WINDOW1 record constants }
  MASK_WINDOW1_OPTION_WINDOW_HIDDEN             = $0001;
  MASK_WINDOW1_OPTION_WINDOW_MINIMISED          = $0002;
  MASK_WINDOW1_OPTION_HORZ_SCROLL_VISIBLE       = $0008;
  MASK_WINDOW1_OPTION_VERT_SCROLL_VISIBLE       = $0010;
  MASK_WINDOW1_OPTION_WORKSHEET_TAB_VISIBLE     = $0020;

  { WINDOW2 record constants }
  MASK_WINDOW2_OPTION_SHOW_FORMULAS             = $0001;
  MASK_WINDOW2_OPTION_SHOW_GRID_LINES           = $0002;
  MASK_WINDOW2_OPTION_SHOW_SHEET_HEADERS        = $0004;
  MASK_WINDOW2_OPTION_PANES_ARE_FROZEN          = $0008;
  MASK_WINDOW2_OPTION_SHOW_ZERO_VALUES          = $0010;
  MASK_WINDOW2_OPTION_AUTO_GRIDLINE_COLOR       = $0020;
  MASK_WINDOW2_OPTION_COLUMNS_RIGHT_TO_LEFT     = $0040;
  MASK_WINDOW2_OPTION_SHOW_OUTLINE_SYMBOLS      = $0080;
  MASK_WINDOW2_OPTION_REMOVE_SPLITS_ON_UNFREEZE = $0100;
  MASK_WINDOW2_OPTION_SHEET_SELECTED            = $0200;
  MASK_WINDOW2_OPTION_SHEET_ACTIVE              = $0400;

  { XF substructures }

  { XF_TYPE_PROT - XF Type and Cell protection (3 Bits) - BIFF3-BIFF8 }
  MASK_XF_TYPE_PROT_LOCKED            = $1;
  MASK_XF_TYPE_PROT_FORMULA_HIDDEN    = $2;
  MASK_XF_TYPE_PROT_STYLE_XF          = $4; // 0 = CELL XF

  { XF_USED_ATTRIB - Attributes from parent Style XF (6 Bits) - BIFF3-BIFF8
  
    In a CELL XF a cleared bit means that the parent attribute is used,
    while a set bit indicates that the data in this XF is used

    In a STYLE XF a cleared bit means that the data in this XF is used,
    while a set bit indicates that the attribute should be ignored }
  MASK_XF_USED_ATTRIB_NUMBER_FORMAT   = $04;
  MASK_XF_USED_ATTRIB_FONT            = $08;
  MASK_XF_USED_ATTRIB_TEXT            = $10;
  MASK_XF_USED_ATTRIB_BORDER_LINES    = $20;
  MASK_XF_USED_ATTRIB_BACKGROUND      = $40;
  MASK_XF_USED_ATTRIB_CELL_PROTECTION = $80;

  { XF_VERT_ALIGN }
  MASK_XF_VERT_ALIGN_TOP              = $00;
  MASK_XF_VERT_ALIGN_CENTRED          = $10;
  MASK_XF_VERT_ALIGN_BOTTOM           = $20;
  MASK_XF_VERT_ALIGN_JUSTIFIED        = $30;

  { XF record constants }
  MASK_XF_TYPE_PROT                   = $0007;
  MASK_XF_TYPE_PROT_PARENT            = $FFF0;

  MASK_XF_VERT_ALIGN                  = $70;

{
  Exported functions
}

{ TsSpreadBIFF5Writer }

function TsSpreadBIFF5Writer.FEKindToExcelID(AElement: TFEKind;
  var AParamsNum: Byte; var AExtra: Word): Byte;
begin
  AExtra := 0;

  case AElement of
  { Operands }
  fekCell: Result := INT_EXCEL_TOKEN_TREFV;
  fekNum: Result := INT_EXCEL_TOKEN_TNUM;
  { Operators }
  fekAdd:  Result := INT_EXCEL_TOKEN_TADD;
  fekSub:  Result := INT_EXCEL_TOKEN_TSUB;
  fekDiv:  Result := INT_EXCEL_TOKEN_TDIV;
  fekMul:  Result := INT_EXCEL_TOKEN_TMUL;
  { Build-in Function }
  fekABS:
  begin
    Result := INT_EXCEL_TOKEN_FUNCVAR_V;
    AParamsNum := 1;
    AExtra := INT_EXCEL_SHEET_FUNC_ABS;
  end;
  fekROUND:
  begin
    Result := INT_EXCEL_TOKEN_FUNCVAR_V;
    AParamsNum := 2;
    AExtra := INT_EXCEL_SHEET_FUNC_ROUND;
  end;
  end;
end;

{*******************************************************************
*  TsSpreadBIFF5Writer.WriteToFile ()
*
*  DESCRIPTION:    Writes an Excel BIFF5 file to the disc
*
*                  The BIFF 5 writer overrides this method because
*                  BIFF 5 is written as an OLE document, and our
*                  current OLE document writing method involves:
*
*                  1 - Writing the BIFF data to a memory stream
*
*                  2 - Write the memory stream data to disk using
*                      COM functions
*
*******************************************************************}
procedure TsSpreadBIFF5Writer.WriteToFile(const AFileName: string;
  AData: TsWorkbook; const AOverwriteExisting: Boolean);
var
  MemStream: TMemoryStream;
  OutputStorage: TOLEStorage;
  OLEDocument: TOLEDocument;
begin
  MemStream := TMemoryStream.Create;
  OutputStorage := TOLEStorage.Create;
  try
    WriteToStream(MemStream, AData);

    // Only one stream is necessary for any number of worksheets
    OLEDocument.Stream := MemStream;

    OutputStorage.WriteOLEFile(AFileName, OLEDocument, AOverwriteExisting);
  finally
    MemStream.Free;
    OutputStorage.Free;
  end;
end;

{*******************************************************************
*  TsSpreadBIFF5Writer.WriteToStream ()
*
*  DESCRIPTION:    Writes an Excel BIFF5 record structure
*
*                  Be careful as this method doesn't write the OLE
*                  part of the document, just the BIFF records
*
*******************************************************************}
procedure TsSpreadBIFF5Writer.WriteToStream(AStream: TStream; AData: TsWorkbook);
var
  FontData: TFPCustomFont;
  MyData: TMemoryStream;
  CurrentPos: Int64;
  Boundsheets: array of Int64;
  i, len: Integer;
begin
  { Store some data about the workbook that other routines need }
  WorkBookEncoding := AData.Encoding;

  { Write workbook globals }

  WriteBOF(AStream, INT_BOF_WORKBOOK_GLOBALS);

  WriteCodepage(AStream, WorkBookEncoding);

  WriteWindow1(AStream);

  FontData := TFPCustomFont.Create;
  try
    FontData.Name := 'Arial';

    // FONT0
    WriteFont(AStream, FontData);
    // FONT1
    WriteFont(AStream, FontData);
    // FONT2
    WriteFont(AStream, FontData);
    // FONT3
    WriteFont(AStream, FontData);
    // FONT5
    WriteFont(AStream, FontData);
  finally
   FontData.Free;
  end;
  
  // XF0
  WriteXF(AStream, 0, MASK_XF_TYPE_PROT_STYLE_XF);
  // XF1
  WriteXF(AStream, 0, MASK_XF_TYPE_PROT_STYLE_XF);
  // XF2
  WriteXF(AStream, 0, MASK_XF_TYPE_PROT_STYLE_XF);
  // XF3
  WriteXF(AStream, 0, MASK_XF_TYPE_PROT_STYLE_XF);
  // XF4
  WriteXF(AStream, 0, MASK_XF_TYPE_PROT_STYLE_XF);
  // XF5
  WriteXF(AStream, 0, MASK_XF_TYPE_PROT_STYLE_XF);
  // XF6
  WriteXF(AStream, 0, MASK_XF_TYPE_PROT_STYLE_XF);
  // XF7
  WriteXF(AStream, 0, MASK_XF_TYPE_PROT_STYLE_XF);
  // XF8
  WriteXF(AStream, 0, MASK_XF_TYPE_PROT_STYLE_XF);
  // XF9
  WriteXF(AStream, 0, MASK_XF_TYPE_PROT_STYLE_XF);
  // XF10
  WriteXF(AStream, 0, MASK_XF_TYPE_PROT_STYLE_XF);
  // XF11
  WriteXF(AStream, 0, MASK_XF_TYPE_PROT_STYLE_XF);
  // XF12
  WriteXF(AStream, 0, MASK_XF_TYPE_PROT_STYLE_XF);
  // XF13
  WriteXF(AStream, 0, MASK_XF_TYPE_PROT_STYLE_XF);
  // XF14
  WriteXF(AStream, 0, MASK_XF_TYPE_PROT_STYLE_XF);
  // XF15
  WriteXF(AStream, 0, 0);

  WriteStyle(AStream);

  // A BOUNDSHEET for each worksheet
  for i := 0 to AData.GetWorksheetCount - 1 do
  begin
    len := Length(Boundsheets);
    SetLength(Boundsheets, len + 1);
    Boundsheets[len] := WriteBoundsheet(AStream, AData.GetWorksheetByIndex(i).Name);
  end;
  
  WriteEOF(AStream);

  { Write each worksheet }

  for i := 0 to AData.GetWorksheetCount - 1 do
  begin
    { First goes back and writes the position of the BOF of the
      sheet on the respective BOUNDSHEET record }
    CurrentPos := AStream.Position;
    AStream.Position := Boundsheets[i];
    AStream.WriteDWord(CurrentPos);
    AStream.Position := CurrentPos;

    WriteBOF(AStream, INT_BOF_SHEET);

    WriteIndex(AStream);

    WriteDimensions(AStream, AData.GetWorksheetByIndex(i));

    WriteWindow2(AStream, True);

    WriteCellsToStream(AStream, AData.GetWorksheetByIndex(i).Cells);

    WriteEOF(AStream);
  end;
  
  { Cleanup }
  
  SetLength(Boundsheets, 0);
end;

{*******************************************************************
*  TsSpreadBIFF5Writer.WriteBOF ()
*
*  DESCRIPTION:    Writes an Excel 5 BOF record
*
*                  This must be the first record on an Excel 5 stream
*
*******************************************************************}
procedure TsSpreadBIFF5Writer.WriteBOF(AStream: TStream; ADataType: Word);
begin
  { BIFF Record header }
  AStream.WriteWord(WordToLE(INT_EXCEL_ID_BOF));
  AStream.WriteWord(WordToLE(8));

  { BIFF version. Should only be used if this BOF is for the workbook globals }
  if ADataType = INT_BOF_WORKBOOK_GLOBALS then
   AStream.WriteWord(WordToLE(INT_BOF_BIFF5_VER))
  else AStream.WriteWord(0);

  { Data type }
  AStream.WriteWord(WordToLE(ADataType));

  { Build identifier, must not be 0 }
  AStream.WriteWord(WordToLE(INT_BOF_BUILD_ID));

  { Build year, must not be 0 }
  AStream.WriteWord(WordToLE(INT_BOF_BUILD_YEAR));
end;

{*******************************************************************
*  TsSpreadBIFF5Writer.WriteBoundsheet ()
*
*  DESCRIPTION:    Writes an Excel 5 BOUNDSHEET record
*
*                  Always located on the workbook globals substream.
*
*                  One BOUNDSHEET is written for each worksheet.
*
*  RETURNS:        The stream position where the absolute stream position
*                  of the BOF of this sheet should be written (4 bytes size).
*
*******************************************************************}
function TsSpreadBIFF5Writer.WriteBoundsheet(AStream: TStream; ASheetName: string): Int64;
var
  Len: Byte;
  LatinSheetName: string;
begin
  LatinSheetName := UTF8ToISO_8859_1(ASheetName);
  Len := Length(LatinSheetName);

  { BIFF Record header }
  AStream.WriteWord(WordToLE(INT_EXCEL_ID_BOUNDSHEET));
  AStream.WriteWord(WordToLE(6 + 1 + Len));

  { Absolute stream position of the BOF record of the sheet represented
    by this record }
  Result := AStream.Position;
  AStream.WriteDWord(WordToLE(0));

  { Visibility }
  AStream.WriteByte(0);

  { Sheet type }
  AStream.WriteByte(0);

  { Sheet name: Byte string, 8-bit length }
  AStream.WriteByte(Len);
  AStream.WriteBuffer(LatinSheetName[1], Len);
end;

{
  Writes an Excel 5 DIMENSIONS record

  nm = (rl - rf - 1) / 32 + 1 (using integer division)

  Excel, OpenOffice and FPSpreadsheet ignore the dimensions written in this record,
  but some other applications really use them, so they need to be correct.

  See bug 18886: excel5 files are truncated when imported
}
procedure TsSpreadBIFF5Writer.WriteDimensions(AStream: TStream; AWorksheet: TsWorksheet);
var
  lLastCol, lLastRow: Word;
begin
  { BIFF Record header }
  AStream.WriteWord(WordToLE(INT_EXCEL_ID_DIMENSIONS));
  AStream.WriteWord(WordToLE(10));

  { Index to first used row }
  AStream.WriteWord(0);

  { Index to last used row, increased by 1 }
  lLastRow := Word(GetLastRowIndex(AWorksheet)+1);
  AStream.WriteWord(WordToLE(lLastRow)); // Old dummy value: 33

  { Index to first used column }
  AStream.WriteWord(0);

  { Index to last used column, increased by 1 }
  lLastCol := Word(GetLastColIndex(AWorksheet)+1);
  AStream.WriteWord(WordToLE(lLastCol)); // Old dummy value: 10

  { Not used }
  AStream.WriteWord(0);
end;

{*******************************************************************
*  TsSpreadBIFF5Writer.WriteEOF ()
*
*  DESCRIPTION:    Writes an Excel 5 EOF record
*
*                  This must be the last record on an Excel 5 stream
*
*******************************************************************}
procedure TsSpreadBIFF5Writer.WriteEOF(AStream: TStream);
begin
  { BIFF Record header }
  AStream.WriteWord(WordToLE(INT_EXCEL_ID_EOF));
  AStream.WriteWord($0000);
end;

{*******************************************************************
*  TsSpreadBIFF5Writer.WriteFont ()
*
*  DESCRIPTION:    Writes an Excel 5 FONT record
*
*                  The font data is passed in an instance of TFPCustomFont
*
*******************************************************************}
procedure TsSpreadBIFF5Writer.WriteFont(AStream: TStream; AFont: TFPCustomFont);
var
  Len: Byte;
begin
  Len := Length(AFont.Name);

  { BIFF Record header }
  AStream.WriteWord(WordToLE(INT_EXCEL_ID_FONT));
  AStream.WriteWord(WordToLE(14 + 1 + Len));

  { Height of the font in twips = 1/20 of a point }
  AStream.WriteWord(WordToLE(200));

  { Option flags }
  AStream.WriteWord(0);

  { Colour index }
  AStream.WriteWord($7FFF);

  { Font weight }
  AStream.WriteWord(WordToLE(INT_FONT_WEIGHT_NORMAL));

  { Escapement type }
  AStream.WriteWord(0);

  { Underline type }
  AStream.WriteByte(0);

  { Font family }
  AStream.WriteByte(0);

  { Character set }
  AStream.WriteByte(0);

  { Not used }
  AStream.WriteByte(0);

  { Font name: Byte string, 8-bit length }
  AStream.WriteByte(Len);
  AStream.WriteBuffer(AFont.Name[1], Len);
end;

{*******************************************************************
*  TsSpreadBIFF5Writer.WriteFormula ()
*
*  DESCRIPTION:    Writes an Excel 5 FORMULA record
*
*                  To input a formula to this method, first convert it
*                  to RPN, and then list all it's members in the
*                  AFormula array
*
*******************************************************************}
procedure TsSpreadBIFF5Writer.WriteRPNFormula(AStream: TStream; const ARow,
  ACol: Word; const AFormula: TsRPNFormula; ACell: PCell);
var
  FormulaResult: double;
  i: Integer;
  RPNLength: Word;
  TokenArraySizePos, RecordSizePos, FinalPos: Int64;
  FormulaKind, ParamsNum: Byte;
  ExtraInfo: Word;
begin
  RPNLength := 0;
  FormulaResult := 0.0;

  { BIFF Record header }
  AStream.WriteWord(WordToLE(INT_EXCEL_ID_FORMULA));
  RecordSizePos := AStream.Position;
  AStream.WriteWord(WordToLE(22 + RPNLength));

  { BIFF Record data }
  AStream.WriteWord(WordToLE(ARow));
  AStream.WriteWord(WordToLE(ACol));

  { Index to XF Record }
  AStream.WriteWord($0000);

  { Result of the formula in IEE 754 floating-point value }
  AStream.WriteBuffer(FormulaResult, 8);

  { Options flags }
  AStream.WriteWord(WordToLE(MASK_FORMULA_RECALCULATE_ALWAYS));

  { Not used }
  AStream.WriteDWord(0);

  { Formula }

  { The size of the token array is written later,
    because it's necessary to calculate if first,
    and this is done at the same time it is written }
  TokenArraySizePos := AStream.Position;
  AStream.WriteWord(RPNLength);

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

    {
    sOffset Size Contents
    0 1 22H (tFuncVarR), 42H (tFuncVarV), 62H (tFuncVarA)
    1 1 Number of arguments
    Bit Mask Contents
    6-0 7FH Number of arguments
    7 80H 1 = User prompt for macro commands (shown by a question mark
    following the command name)
    2 2 Index to a sheet function
    Bit Mask Contents
    14-0 7FFFH Index to a built-in sheet function (➜3.11) or a macro command
    15 8000H 0 = Built-in function; 1 = Macro command
    }
    INT_EXCEL_TOKEN_FUNCVAR_V:
    begin
      AStream.WriteByte(ParamsNum);
      AStream.WriteWord(WordToLE(ExtraInfo));
      Inc(RPNLength, 3);
    end;

    end;
  end;

  { Write sizes in the end, after we known them }
  FinalPos := AStream.Position;
  AStream.position := TokenArraySizePos;
  AStream.WriteByte(RPNLength);
  AStream.Position := RecordSizePos;
  AStream.WriteWord(WordToLE(22 + RPNLength));
  AStream.position := FinalPos;
end;

{*******************************************************************
*  TsSpreadBIFF5Writer.WriteIndex ()
*
*  DESCRIPTION:    Writes an Excel 5 INDEX record
*
*                  nm = (rl - rf - 1) / 32 + 1 (using integer division)
*
*******************************************************************}
procedure TsSpreadBIFF5Writer.WriteIndex(AStream: TStream);
begin
  { BIFF Record header }
  AStream.WriteWord(WordToLE(INT_EXCEL_ID_INDEX));
  AStream.WriteWord(WordToLE(12));

  { Not used }
  AStream.WriteDWord(0);

  { Index to first used row, rf, 0 based }
  AStream.WriteWord(0);

  { Index to first row of unused tail of sheet, rl, last used row + 1, 0 based }
  AStream.WriteWord(33);

  { Absolute stream position of the DEFCOLWIDTH record of the current sheet.
    If it doesn't exist, the offset points to where it would occur. }
  AStream.WriteDWord($00);

  { Array of nm absolute stream positions of the DBCELL record of each Row Block }
  
  { OBS: It seams to be no problem just ignoring this part of the record }
end;

{*******************************************************************
*  TsSpreadBIFF5Writer.WriteLabel ()
*
*  DESCRIPTION:    Writes an Excel 8 LABEL record
*
*                  Writes a string to the sheet
*
*******************************************************************}
procedure TsSpreadBIFF5Writer.WriteLabel(AStream: TStream; const ARow,
  ACol: Word; const AValue: string; ACell: PCell);
var
  L: Word;
  AnsiValue: ansistring;
begin
  case WorkBookEncoding of
  seLatin2:   AnsiValue := UTF8ToCP1250(AValue);
  seCyrillic: AnsiValue := UTF8ToCP1251(AValue);
  seGreek:    AnsiValue := UTF8ToCP1253(AValue);
  seTurkish:  AnsiValue := UTF8ToCP1254(AValue);
  seHebrew:   AnsiValue := UTF8ToCP1255(AValue);
  seArabic:   AnsiValue := UTF8ToCP1256(AValue);
  else
    // Latin 1 is the default
    AnsiValue := UTF8ToCP1252(AValue);
  end;

  if AnsiValue = '' then
  begin
    // Bad formatted UTF8String (maybe ANSI?)
    if Length(AValue)<>0 then begin
      //It was an ANSI string written as UTF8 quite sure, so raise exception.
      Raise Exception.CreateFmt('Expected UTF8 text but probably ANSI text found in cell [%d,%d]',[ARow,ACol]);
    end;
    Exit;
  end;
  L := Length(AnsiValue);
  if L>255 then begin
    //BIFF 5 does not support labels/text bigger than 255 chars.
    L:=255;
  end;

  { BIFF Record header }
  AStream.WriteWord(WordToLE(INT_EXCEL_ID_LABEL));
  AStream.WriteWord(WordToLE(8 + L));

  { BIFF Record data }
  AStream.WriteWord(WordToLE(ARow));
  AStream.WriteWord(WordToLE(ACol));

  { Index to XF record }
  AStream.WriteWord(WordToLE(15));

  { Byte String with 16-bit size }
  AStream.WriteWord(WordToLE(L));
  AStream.WriteBuffer(AnsiValue[1], L);
end;

{*******************************************************************
*  TsSpreadBIFF5Writer.WriteNumber ()
*
*  DESCRIPTION:    Writes an Excel 5 NUMBER record
*
*                  Writes a number (64-bit floating point) to the sheet
*
*******************************************************************}
procedure TsSpreadBIFF5Writer.WriteNumber(AStream: TStream; const ARow,
  ACol: Cardinal; const AValue: double; ACell: PCell);
begin
  { BIFF Record header }
  AStream.WriteWord(WordToLE(INT_EXCEL_ID_NUMBER));
  AStream.WriteWord(WordToLE(14));

  { BIFF Record data }
  AStream.WriteWord(WordToLE(ARow));
  AStream.WriteWord(WordToLE(ACol));

  { Index to XF record }
  AStream.WriteWord($0);

  { IEE 754 floating-point value }
  AStream.WriteBuffer(AValue, 8);
end;

{*******************************************************************
*  TsSpreadBIFF5Writer.WriteStyle ()
*
*  DESCRIPTION:    Writes an Excel 5 STYLE record
*
*                  Registers the name of a user-defined style or
*                  specific options for a built-in cell style.
*
*******************************************************************}
procedure TsSpreadBIFF5Writer.WriteStyle(AStream: TStream);
begin
  { BIFF Record header }
  AStream.WriteWord(WordToLE(INT_EXCEL_ID_STYLE));
  AStream.WriteWord(WordToLE(4));

  { Index to style XF and defines if it's a built-in or used defined style }
  AStream.WriteWord(WordToLE(MASK_STYLE_BUILT_IN));

  { Built-in cell style identifier }
  AStream.WriteByte($00);

  { Level if the identifier for a built-in style is RowLevel or ColLevel, $FF otherwise }
  AStream.WriteByte(WordToLE($FF));
end;

{*******************************************************************
*  TsSpreadBIFF5Writer.WriteWindow1 ()
*
*  DESCRIPTION:    Writes an Excel 5 WINDOW1 record
*
*                  This record contains general settings for the
*                  document window and global workbook settings.
*
*                  The values written here are reasonable defaults,
*                  which should work for most sheets.
*
*******************************************************************}
procedure TsSpreadBIFF5Writer.WriteWindow1(AStream: TStream);
begin
  { BIFF Record header }
  AStream.WriteWord(WordToLE(INT_EXCEL_ID_WINDOW1));
  AStream.WriteWord(WordToLE(18));

  { Horizontal position of the document window, in twips = 1 / 20 of a point }
  AStream.WriteWord(0);

  { Vertical position of the document window, in twips = 1 / 20 of a point }
  AStream.WriteWord(WordToLE($0069));

  { Width of the document window, in twips = 1 / 20 of a point }
  AStream.WriteWord(WordToLE($339F));

  { Height of the document window, in twips = 1 / 20 of a point }
  AStream.WriteWord(WordToLE($1B5D));

  { Option flags }
  AStream.WriteWord(WordToLE(
   MASK_WINDOW1_OPTION_HORZ_SCROLL_VISIBLE or
   MASK_WINDOW1_OPTION_VERT_SCROLL_VISIBLE or
   MASK_WINDOW1_OPTION_WORKSHEET_TAB_VISIBLE));

  { Index to active (displayed) worksheet }
  AStream.WriteWord($00);

  { Index of first visible tab in the worksheet tab bar }
  AStream.WriteWord($00);

  { Number of selected worksheets }
  AStream.WriteWord(WordToLE(1));

  { Width of worksheet tab bar (in 1/1000 of window width).
    The remaining space is used by the horizontal scroll bar }
  AStream.WriteWord(WordToLE(600));
end;

{*******************************************************************
*  TsSpreadBIFF5Writer.WriteWindow1 ()
*
*  DESCRIPTION:    Writes an Excel 5 WINDOW1 record
*
*                  This record contains aditional settings for the
*                  document window (BIFF2-BIFF4) or for a specific
*                  worksheet (BIFF5-BIFF8).
*
*                  The values written here are reasonable defaults,
*                  which should work for most sheets.
*
*******************************************************************}
procedure TsSpreadBIFF5Writer.WriteWindow2(AStream: TStream;
 ASheetSelected: Boolean);
var
  Options: Word;
begin
  { BIFF Record header }
  AStream.WriteWord(WordToLE(INT_EXCEL_ID_WINDOW2));
  AStream.WriteWord(WordToLE(10));

  { Options flags }
  Options := MASK_WINDOW2_OPTION_SHOW_GRID_LINES or
   MASK_WINDOW2_OPTION_SHOW_SHEET_HEADERS or
   MASK_WINDOW2_OPTION_SHOW_ZERO_VALUES or
   MASK_WINDOW2_OPTION_AUTO_GRIDLINE_COLOR or
   MASK_WINDOW2_OPTION_SHOW_OUTLINE_SYMBOLS or
   MASK_WINDOW2_OPTION_SHEET_ACTIVE;

  if ASheetSelected then Options := Options or MASK_WINDOW2_OPTION_SHEET_SELECTED;

  AStream.WriteWord(WordToLE(Options));

  { Index to first visible row }
  AStream.WriteWord(WordToLE(0));

  { Index to first visible column }
  AStream.WriteWord(WordToLE(0));

  { Grid line RGB colour }
  AStream.WriteDWord(WordToLE(0));
end;

{*******************************************************************
*  TsSpreadBIFF5Writer.WriteXF ()
*
*  DESCRIPTION:    Writes an Excel 5 XF record
*
*                  Writes a number (64-bit floating point) to the sheet
*
*******************************************************************}
procedure TsSpreadBIFF5Writer.WriteXF(AStream: TStream; AFontIndex: Word;
 AXF_TYPE_PROT: Byte);
var
  XFOptions: Word;
  XFAlignment, XFOrientationAttrib: Byte;
begin
  { BIFF Record header }
  AStream.WriteWord(WordToLE(INT_EXCEL_ID_XF));
  AStream.WriteWord(WordToLE(16));

  { Index to FONT record }
  AStream.WriteWord(WordToLE(AFontIndex));

  { Index to FORMAT record }
  AStream.WriteWord($00);

  { XF type, cell protection and parent style XF }
  XFOptions := AXF_TYPE_PROT and MASK_XF_TYPE_PROT;

  if AXF_TYPE_PROT and MASK_XF_TYPE_PROT_STYLE_XF <> 0 then
   XFOptions := XFOptions or MASK_XF_TYPE_PROT_PARENT;
   
  AStream.WriteWord(WordToLE(XFOptions));

  { Alignment and text break }
  XFAlignment := MASK_XF_VERT_ALIGN_BOTTOM;

  AStream.WriteByte(WordToLE(XFAlignment));

  { Text orientation and flags for used attribute groups }
  XFOrientationAttrib :=
   MASK_XF_USED_ATTRIB_NUMBER_FORMAT or
   MASK_XF_USED_ATTRIB_FONT or
   MASK_XF_USED_ATTRIB_TEXT or
   MASK_XF_USED_ATTRIB_BORDER_LINES or
   MASK_XF_USED_ATTRIB_BACKGROUND or
   MASK_XF_USED_ATTRIB_CELL_PROTECTION;

  AStream.WriteByte(WordToLE(XFOrientationAttrib));

  { Cell border lines and background area }
  AStream.WriteDWord($000020C0);
  AStream.WriteDWord($00000000);
end;

{*******************************************************************
*  Initialization section
*
*  Registers this reader / writer on fpSpreadsheet
*
*******************************************************************}

{ TsSpreadBIFF5Reader }

procedure TsSpreadBIFF5Reader.ReadWorkbookGlobals(AStream: TStream;
  AData: TsWorkbook);
var
  SectionEOF: Boolean = False;
  RecordType: Word;
  CurStreamPos: Int64;
begin
  while (not SectionEOF) do
  begin
    { Read the record header }
    RecordType := WordLEToN(AStream.ReadWord);
    RecordSize := WordLEToN(AStream.ReadWord);

    CurStreamPos := AStream.Position;

    case RecordType of
     INT_EXCEL_ID_BOF:        ;
     INT_EXCEL_ID_BOUNDSHEET: ReadBoundSheet(AStream);
     INT_EXCEL_ID_EOF:        SectionEOF := True;
    else
      // nothing
    end;

    // Make sure we are in the right position for the next record
    AStream.Seek(CurStreamPos + RecordSize, soFromBeginning);

    // Check for the end of the file
    if AStream.Position >= AStream.Size then SectionEOF := True;
  end;
end;

procedure TsSpreadBIFF5Reader.ReadWorksheet(AStream: TStream; AData: TsWorkbook);
var
  SectionEOF: Boolean = False;
  RecordType: Word;
  CurStreamPos: Int64;
begin
  FWorksheet := AData.AddWorksheet(FWorksheetNames.Strings[FCurrentWorksheet]);

  while (not SectionEOF) do
  begin
    { Read the record header }
    RecordType := WordLEToN(AStream.ReadWord);
    RecordSize := WordLEToN(AStream.ReadWord);

    CurStreamPos := AStream.Position;

    case RecordType of

    INT_EXCEL_ID_NUMBER:  ReadNumber(AStream);
    INT_EXCEL_ID_LABEL:   ReadLabel(AStream);
//    INT_EXCEL_ID_FORMULA: ReadFormula(AStream);
    INT_EXCEL_ID_RSTRING: ReadRichString(AStream); //(RSTRING) This record stores a formatted text cell (Rich-Text). In BIFF8 it is usually replaced by the LABELSST record. Excel still uses this record, if it copies formatted text cells to the clipboard.
    INT_EXCEL_ID_RK:      ReadRKValue(AStream); //(RK) This record represents a cell that contains an RK value (encoded integer or floating-point value). If a floating-point value cannot be encoded to an RK value, a NUMBER record will be written. This record replaces the record INTEGER written in BIFF2.
    INT_EXCEL_ID_MULRK:   ReadMulRKValues(AStream);
    INT_EXCEL_ID_FORMULA: ReadFormulaExcel(AStream);
    INT_EXCEL_ID_BOF:     ;
    INT_EXCEL_ID_EOF:     SectionEOF := True;
      // Show unsupported record types to console.
{.$DEFINE SHOWUNSUPPORTED}
{$IFDEF SHOWUNSUPPORTED}
    else
      case RecordType of
        $000C: ; //(CALCCOUNT) This record is part of the Calculation Settings Block. It specifies the maximum number of times the formulas should be iteratively calculated. This is a fail-safe against mutually recursive formulas locking up a spreadsheet application.
        $000D: ; //(CALCMODE) This record is part of the Calculation Settings Block. It specifies whether to calculate formulas manually, automatically or automatically except for multiple table operations.
        $000F: ; //(REFMODE) This record is part of the Calculation Settings Block. It stores which method is used to show cell addresses in formulas.
        $0010: ; //(DELTA) This record is part of the Calculation Settings Block. It stores the maximum change of the result to exit an iteration.
        $0011: ; //(ITERATION) This record is part of the Calculation Settings Block. It stores if iterations are allowed while calculating recursive formulas.
        $0014: ; //(HEADER) This record is part of the Page Settings Block. It specifies the page header string for the current worksheet. If this record is not present or completely empty (record size is 0), the sheet does not contain a page header.
        $0015: ; //(FOOTER) This record is part of the Page Settings Block. It specifies the page footer string for the current worksheet. If this record is not present or completely empty (record size is 0), the sheet does not contain a page footer.
        $001D: ; //(SELECTION) This record contains the addresses of all selected cell ranges and the position of the active cell for a pane in the current sheet.
        $0026: ; //(LEFTMARGIN) This record is part of the Page Settings Block. It contains the left page margin of the current worksheet.
        $0027: ; //(RIGHTMARGIN) This record is part of the Page Settings Block. It contains the right page margin of the current worksheet.
        $0028: ; //(TOPMARGIN) This record is part of the Page Settings Block. It contains the top page margin of the current worksheet.
        $0029: ; //(BOTTOMMARGIN) This record is part of the Page Settings Block. It contains the bottom page margin of the current worksheet.
        $002A: ; //(PRINTHEADERS) This record stores if the row and column headers (the areas with row numbers and column letters) will be printed.
        $002B: ; //(PRINTGRIDLINES) This record stores if sheet grid lines will be printed.
        $0055: ; //(DEFCOLWIDTH) This record specifies the default column width for columns that do not have a specific width set using the records COLWIDTH (BIFF2), COLINFO (BIFF3-BIFF8), or STANDARDWIDTH.
        $005F: ; //(SAVERECALC) This record is part of the Calculation Settings Block. It contains the “Recalculate before save” option in Excel's calculation settings dialogue.
        $007D: ; //(COLINFO) This record specifies the width and default cell formatting for a given range of columns.
        $0080: ; //(GUTS) This record contains information about the layout of outline symbols.
        $0081: ; //(SHEETPR) This record stores a 16-bit value with Boolean options for the current sheet. From BIFF5 on the “Save external linked values” option is moved to the record BOOKBOOL. This record is also used to distinguish standard sheets from dialogue sheets.
        $0082: ; //(GRIDSET) This record specifies if the option to print sheet grid lines (record PRINTGRIDLINES) has ever been changed.
        $0083: ; //(HCENTER) This record is part of the Page Settings Block. It specifies if the sheet is centred horizontally when printed.
        $0084: ; //(VCENTER) This record is part of the Page Settings Block. It specifies if the sheet is centred vertically when printed.
        $008C: ; //(COUNTRY) This record stores two Windows country identifiers. The first represents the user interface language of the Excel version that has saved the file, and the second represents the system regional settings at the time the file was saved.
        $00A1: ; //(PAGESETUP) This record is part of the Page Settings Block. It stores the page format settings of the current sheet. The pages may be scaled in percent or by using an absolute number of pages.
        $00BE: ; //(MULBLANK) This record represents a cell range of empty cells. All cells are located in the same row.
        $0200: ; //(DIMENSION) This record contains the range address of the used area in the current sheet.
        $0201: ; //(BLANK) This record represents an empty cell. It contains the cell address and formatting information.
        $0208: ; //(ROW) This record contains the properties of a single row in a sheet. Rows and cells in a sheet are divided into blocks of 32 rows.
        $0225: ; //(DEFAULTROWHEIGHT) This record specifies the default height and default flags for rows that do not have a corresponding ROW record.
        $023E: ; //(WINDOW2) This record contains the range address of the used area in the current sheet.
      else
        WriteLn(format('Record type: %.4X Record Size: %.4X',[RecordType,RecordSize]));
      end;
{$ENDIF}
    end;

    // Make sure we are in the right position for the next record
    AStream.Seek(CurStreamPos + RecordSize, soFromBeginning);

    // Check for the end of the file
    if AStream.Position >= AStream.Size then SectionEOF := True;
  end;
end;

procedure TsSpreadBIFF5Reader.ReadBoundsheet(AStream: TStream);
var
  Len: Byte;
  Str: array[0..255] of Char;
begin
  { Absolute stream position of the BOF record of the sheet represented
    by this record }
  // Just assume that they are in order
  AStream.ReadDWord();

  { Visibility }
  AStream.ReadByte();

  { Sheet type }
  AStream.ReadByte();

  { Sheet name: Byte string, 8-bit length }
  Len := AStream.ReadByte();
  AStream.ReadBuffer(Str, Len);
  Str[Len] := #0;

  FWorksheetNames.Add(Str);
end;

procedure TsSpreadBIFF5Reader.ReadRichString(AStream: TStream);
var
  L: Word;
  B: BYTE;
  ARow, ACol, XF: Word;
  AStrValue: ansistring;
begin
  ReadRowColXF(AStream,ARow,ACol,XF);

  { Byte String with 16-bit size }
  L := WordLEtoN(AStream.ReadWord());
  SetLength(AStrValue,L);
  AStream.ReadBuffer(AStrValue[1], L);

  { Save the data }
  FWorksheet.WriteUTF8Text(ARow, ACol, ISO_8859_1ToUTF8(AStrValue));
  //Read formatting runs (not supported)
  B:=AStream.ReadByte;
  for L := 0 to B-1 do begin
    AStream.ReadByte; // First formatted character
    AStream.ReadByte; // Index to FONT record
  end;
end;

procedure TsSpreadBIFF5Reader.ReadRKValue(AStream: TStream);
var
  L: DWORD;
  ARow, ACol, XF: WORD;
  Number: Double;
begin
  ReadRowColXF(AStream,ARow,ACol,XF);

  {Encoded RK value}
  L:=DWordLEtoN(AStream.ReadDWord);

  {Check RK codes}
  Number:=DecodeRKValue(L);
  FWorksheet.WriteNumber(ARow,ACol,Number);
end;

procedure TsSpreadBIFF5Reader.ReadMulRKValues(AStream: TStream);
var
  ARow, fc,lc,XF: Word;
  Pending: integer;
  RK: DWORD;
  Number: Double;
begin
  ARow:=WordLEtoN(AStream.ReadWord);
  fc:=WordLEtoN(AStream.ReadWord);
  Pending:=RecordSize-sizeof(fc)-Sizeof(ARow);
  while Pending > (sizeof(XF)+sizeof(RK)) do begin
    XF:=AStream.ReadWord; //XF record (not used)
    RK:=DWordLEtoN(AStream.ReadDWord);
    Number:=DecodeRKValue(RK);
    FWorksheet.WriteNumber(ARow,fc,Number);
    inc(fc);
    dec(Pending,(sizeof(XF)+sizeof(RK)));
  end;
  if Pending=2 then begin
    //Just for completeness
    lc:=WordLEtoN(AStream.ReadWord);
    if lc+1<>fc then begin
      //Stream error... bypass by now
    end;
  end;
end;

procedure TsSpreadBIFF5Reader.ReadFormulaExcel(AStream: TStream);
var
  ARow, ACol, XF: WORD;
  ResultFormula: Double;
  Data: array [0..7] of BYTE;
  Flags: WORD;
  FormulaSize: BYTE;
begin
  ReadRowColXF(AStream,ARow,ACol,XF);

  AStream.ReadBuffer(Data,Sizeof(Data));
  Flags:=WordLEtoN(AStream.ReadWord);
  AStream.ReadDWord; //Not used.
  FormulaSize:=AStream.ReadByte;
  //RPN data not used by now
  AStream.Position:=AStream.Position+FormulaSize;

  if SizeOf(Double)<>8 then Raise Exception.Create('Double is not 8 bytes');
  Move(Data[0],ResultFormula,sizeof(Data));
  FWorksheet.WriteNumber(ARow,ACol,ResultFormula);
end;

function TsSpreadBIFF5Reader.DecodeRKValue(const ARK: DWORD): Double;
var
  Number: Double;
  Tmp: LongInt;
begin
  if ARK and 2 = 2 then begin
    // Signed integer value
    if LongInt(ARK)<0 then begin
      //Simulates a sar
      Tmp:=LongInt(ARK)*-1;
      Tmp:=Tmp shr 2;
      Tmp:=Tmp*-1;
      Number:=Tmp-1;
    end else begin
      Number:=ARK shr 2;
    end;
  end else begin
    // Floating point value
    // NOTE: This is endian dependent and IEEE dependent (Not checked, working win-i386)
    PDWORD(@Number)^:= $00000000;
    (PDWORD(@Number)+1)^:=ARK and $FFFFFFFC;
  end;
  if ARK and 1 = 1 then begin
    // Encoded value is multiplied by 100
    Number:=Number / 100;
  end;
  Result:=Number;
end;

procedure TsSpreadBIFF5Reader.ReadRowColXF(const AStream: TStream; out ARow,
  ACol, AXF: WORD);
begin
  { BIFF Record data }
  ARow := WordLEToN(AStream.ReadWord);
  ACol := WordLEToN(AStream.ReadWord);

  { Index to XF record }
  AXF:=WordLEtoN(AStream.ReadWord);
end;

procedure TsSpreadBIFF5Reader.ReadFromFile(AFileName: string; AData: TsWorkbook);
var
  MemStream: TMemoryStream;
  OLEStorage: TOLEStorage;
  OLEDocument: TOLEDocument;
begin
  MemStream := TMemoryStream.Create;
  OLEStorage := TOLEStorage.Create;
  try
    // Only one stream is necessary for any number of worksheets
    OLEDocument.Stream := MemStream;
    OLEStorage.ReadOLEFile(AFileName, OLEDocument);

    // Check if the operation succeded
    if MemStream.Size = 0 then raise Exception.Create('FPSpreadsheet: Reading the OLE document failed');

    // Rewind the stream and read from it
    MemStream.Position := 0;
    ReadFromStream(MemStream, AData);

//    Uncomment to verify if the data was correctly optained from the OLE file
//    MemStream.SaveToFile(SysUtils.ChangeFileExt(AFileName, 'bin.xls'));
  finally
    MemStream.Free;
    OLEStorage.Free;
  end;
end;

procedure TsSpreadBIFF5Reader.ReadFromStream(AStream: TStream; AData: TsWorkbook);
var
  BIFF5EOF: Boolean;
begin
  { Initializations }

  FWorksheetNames := TStringList.Create;
  FWorksheetNames.Clear;
  FCurrentWorksheet := 0;
  BIFF5EOF := False;

  { Read workbook globals }

  ReadWorkbookGlobals(AStream, AData);

  // Check for the end of the file
  if AStream.Position >= AStream.Size then BIFF5EOF := True;

  { Now read all worksheets }

  while (not BIFF5EOF) do
  begin
    ReadWorksheet(AStream, AData);

    // Check for the end of the file
    if AStream.Position >= AStream.Size then BIFF5EOF := True;

    // Final preparations
    Inc(FCurrentWorksheet);
  end;

  { Finalizations }

  FWorksheetNames.Free;
end;

procedure TsSpreadBIFF5Reader.ReadFormula(AStream: TStream);
begin

end;

procedure TsSpreadBIFF5Reader.ReadLabel(AStream: TStream);
var
  L: Word;
  ARow, ACol, XF: WORD;
  AValue: array[0..255] of Char;
  AStrValue: ansistring;
begin
  ReadRowColXF(AStream,ARow,ACol,XF);

  { Byte String with 16-bit size }
  L := AStream.ReadWord();
  AStream.ReadBuffer(AValue, L);
  AValue[L] := #0;
  AStrValue := AValue;

  { Save the data }
  FWorksheet.WriteUTF8Text(ARow, ACol, ISO_8859_1ToUTF8(AStrValue));
end;

procedure TsSpreadBIFF5Reader.ReadNumber(AStream: TStream);
var
  ARow, ACol, XF: WORD;
  AValue: Double;
begin
  ReadRowColXF(AStream,ARow,ACol,XF);

  { IEE 754 floating-point value }
  AStream.ReadBuffer(AValue, 8);

  { Save the data }
  FWorksheet.WriteNumber(ARow, ACol, AValue);
end;

initialization

  RegisterSpreadFormat(TsSpreadBIFF5Reader, TsSpreadBIFF5Writer, sfExcel5);

end.

