{
xlsbiff8.pas

Writes an Excel 8 file

An Excel worksheet stream consists of a number of subsequent records.
To ensure a properly formed file, the following order must be respected:

1st record:        BOF
2nd to Nth record: Any record
Last record:       EOF

Excel 8 files are OLE compound document files, and must be written using the
fpOLE library.

Records Needed to Make a BIFF8 File Microsoft Excel Can Use:

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

AUTHORS:  Felipe Monteiro de Carvalho
          Jose Mejuto
}
unit xlsbiff8;

{$ifdef fpc}
  {$mode delphi}
{$endif}

// The new OLE code is much better, so always use it
{$define USE_NEW_OLE}

interface

uses
  Classes, SysUtils, fpcanvas,
  fpspreadsheet,
  {$ifdef USE_NEW_OLE}
  fpolebasic,
  {$else}
  fpolestorage,
  {$endif}
  fpsutils;

type

  { TsSpreadBIFF8Reader }

  TsSpreadBIFF8Reader = class(TsCustomSpreadReader)
  private
    RecordSize: Word;
    PendingRecordSize: SizeInt;
    FWorksheet: TsWorksheet;
    FWorksheetNames: TStringList;
    FCurrentWorksheet: Integer;
    FSharedStringTable: TStringList;
    function DecodeRKValue(const ARK: DWORD): Double;
    function ReadWideString(const AStream: TStream;const ALength: WORD): WideString;
    procedure ReadWorkbookGlobals(AStream: TStream; AData: TsWorkbook);
    procedure ReadWorksheet(AStream: TStream; AData: TsWorkbook);
    procedure ReadBoundsheet(AStream: TStream);

    procedure ReadRKValue(const AStream: TStream);
    procedure ReadMulRKValues(const AStream: TStream);
    procedure ReadFormulaExcel(AStream: TStream);
    procedure ReadRowColXF(const AStream: TStream; out ARow,ACol,AXF: WORD);
    function ReadString(const AStream: TStream; const ALength: WORD): UTF8String;
    procedure ReadRichString(const AStream: TStream);
    procedure ReadSST(const AStream: TStream);
    procedure ReadLabelSST(const AStream: TStream);
  public
    { General reading methods }
    procedure ReadFromFile(AFileName: string; AData: TsWorkbook); override;
    procedure ReadFromStream(AStream: TStream; AData: TsWorkbook); override;
    { Record writing methods }
    procedure ReadFormula(AStream: TStream); override;
    procedure ReadLabel(AStream: TStream); override;
    procedure ReadNumber(AStream: TStream); override;

    destructor Destroy; override;
  end;

  { TsSpreadBIFF8Writer }

  TsSpreadBIFF8Writer = class(TsCustomSpreadWriter)
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
    procedure WriteDimensions(AStream: TStream);
    procedure WriteEOF(AStream: TStream);
    procedure WriteFont(AStream: TStream; AFont: TFPCustomFont);
    procedure WriteFormula(AStream: TStream; const ARow, ACol: Word; const AFormula: TsFormula); override;
    procedure WriteIndex(AStream: TStream);
    procedure WriteLabel(AStream: TStream; const ARow, ACol: Word; const AValue: string; ACell: PCell); override;
    procedure WriteNumber(AStream: TStream; const ARow, ACol: Cardinal; const AValue: double); override;
    procedure WriteStyle(AStream: TStream);
    procedure WriteWindow1(AStream: TStream);
    procedure WriteWindow2(AStream: TStream; ASheetSelected: Boolean);
    procedure WriteXF(AStream: TStream; AFontIndex: Word;
      AXF_TYPE_PROT, ATextRotation: Byte);
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
  INT_EXCEL_ID_SST        = $00FC; //BIFF8 only
  INT_EXCEL_ID_CONTINUE   = $003C;
  INT_EXCEL_ID_LABELSST   = $00FD; //BIFF8 only

  { Cell Addresses constants }
  MASK_EXCEL_ROW          = $3FFF;
  MASK_EXCEL_RELATIVE_ROW = $4000;
  MASK_EXCEL_RELATIVE_COL = $8000;

  { BOF record constants }
  INT_BOF_BIFF8_VER       = $0600;
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

  { XF_ROTATION }

  XF_ROTATION_HORIZONTAL                 = 0;
  XF_ROTATION_90_DEGREE_COUNTERCLOCKWISE = 90;
  XF_ROTATION_90_DEGREE_CLOCKWISE        = 180;

  { XF record constants }
  MASK_XF_TYPE_PROT                   = $0007;
  MASK_XF_TYPE_PROT_PARENT            = $FFF0;

  MASK_XF_VERT_ALIGN                  = $70;

{
  Exported functions
}

{ TsSpreadBIFF8Writer }

{*******************************************************************
*  TsSpreadBIFF8Writer.WriteToFile ()
*
*  DESCRIPTION:    Writes an Excel BIFF8 file to the disc
*
*                  The BIFF 8 writer overrides this method because
*                  BIFF 8 is written as an OLE document, and our
*                  current OLE document writing method involves:
*
*                  1 - Writing the BIFF data to a memory stream
*
*                  2 - Write the memory stream data to disk using
*                      COM functions
*
*******************************************************************}
procedure TsSpreadBIFF8Writer.WriteToFile(const AFileName: string;
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

    OutputStorage.WriteOLEFile(AFileName, OLEDocument, AOverwriteExisting, 'Workbook');
  finally
    MemStream.Free;
    OutputStorage.Free;
  end;
end;

{*******************************************************************
*  TsSpreadBIFF8Writer.WriteToStream ()
*
*  DESCRIPTION:    Writes an Excel BIFF8 record structure
*
*                  Be careful as this method doesn't write the OLE
*                  part of the document, just the BIFF records
*
*******************************************************************}
procedure TsSpreadBIFF8Writer.WriteToStream(AStream: TStream; AData: TsWorkbook);
var
  FontData: TFPCustomFont;
  MyData: TMemoryStream;
  CurrentPos: Int64;
  Boundsheets: array of Int64;
  i, len: Integer;
begin
  { Write workbook globals }

  WriteBOF(AStream, INT_BOF_WORKBOOK_GLOBALS);

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
  WriteXF(AStream, 0, MASK_XF_TYPE_PROT_STYLE_XF, XF_ROTATION_HORIZONTAL);
  // XF1
  WriteXF(AStream, 0, MASK_XF_TYPE_PROT_STYLE_XF, XF_ROTATION_HORIZONTAL);
  // XF2
  WriteXF(AStream, 0, MASK_XF_TYPE_PROT_STYLE_XF, XF_ROTATION_HORIZONTAL);
  // XF3
  WriteXF(AStream, 0, MASK_XF_TYPE_PROT_STYLE_XF, XF_ROTATION_HORIZONTAL);
  // XF4
  WriteXF(AStream, 0, MASK_XF_TYPE_PROT_STYLE_XF, XF_ROTATION_HORIZONTAL);
  // XF5
  WriteXF(AStream, 0, MASK_XF_TYPE_PROT_STYLE_XF, XF_ROTATION_HORIZONTAL);
  // XF6
  WriteXF(AStream, 0, MASK_XF_TYPE_PROT_STYLE_XF, XF_ROTATION_HORIZONTAL);
  // XF7
  WriteXF(AStream, 0, MASK_XF_TYPE_PROT_STYLE_XF, XF_ROTATION_HORIZONTAL);
  // XF8
  WriteXF(AStream, 0, MASK_XF_TYPE_PROT_STYLE_XF, XF_ROTATION_HORIZONTAL);
  // XF9
  WriteXF(AStream, 0, MASK_XF_TYPE_PROT_STYLE_XF, XF_ROTATION_HORIZONTAL);
  // XF10
  WriteXF(AStream, 0, MASK_XF_TYPE_PROT_STYLE_XF, XF_ROTATION_HORIZONTAL);
  // XF11
  WriteXF(AStream, 0, MASK_XF_TYPE_PROT_STYLE_XF, XF_ROTATION_HORIZONTAL);
  // XF12
  WriteXF(AStream, 0, MASK_XF_TYPE_PROT_STYLE_XF, XF_ROTATION_HORIZONTAL);
  // XF13
  WriteXF(AStream, 0, MASK_XF_TYPE_PROT_STYLE_XF, XF_ROTATION_HORIZONTAL);
  // XF14
  WriteXF(AStream, 0, MASK_XF_TYPE_PROT_STYLE_XF, XF_ROTATION_HORIZONTAL);
  // XF15
  WriteXF(AStream, 0, 0, XF_ROTATION_HORIZONTAL);
  // XF16
  WriteXF(AStream, 0, 0, XF_ROTATION_90_DEGREE_COUNTERCLOCKWISE);
  // XF17
  WriteXF(AStream, 0, 0, XF_ROTATION_90_DEGREE_CLOCKWISE);

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
    AStream.WriteDWord(DWordToLE(DWORD(CurrentPos)));
    AStream.Position := CurrentPos;

    WriteBOF(AStream, INT_BOF_SHEET);

    WriteIndex(AStream);

    WriteDimensions(AStream);

    WriteWindow2(AStream, True);

    WriteCellsToStream(AStream, AData.GetWorksheetByIndex(i).Cells);

    WriteEOF(AStream);
  end;
  
  { Cleanup }
  
  SetLength(Boundsheets, 0);
end;

{*******************************************************************
*  TsSpreadBIFF8Writer.WriteBOF ()
*
*  DESCRIPTION:    Writes an Excel 8 BOF record
*
*                  This must be the first record on an Excel 8 stream
*
*******************************************************************}
procedure TsSpreadBIFF8Writer.WriteBOF(AStream: TStream; ADataType: Word);
begin
  { BIFF Record header }
  AStream.WriteWord(WordToLE(INT_EXCEL_ID_BOF));
  AStream.WriteWord(WordToLE(16));

  { BIFF version. Should only be used if this BOF is for the workbook globals }
  { OpenOffice rejects to correctly read xls files if this field is
    omitted as docs. says, or even if it is being written to zero value,
    Not tested with Excel, but MSExcel reader opens it as expected }
  AStream.WriteWord(WordToLE(INT_BOF_BIFF8_VER));

  { Data type }
  AStream.WriteWord(WordToLE(ADataType));

  { Build identifier, must not be 0 }
  AStream.WriteWord(WordToLE(INT_BOF_BUILD_ID));

  { Build year, must not be 0 }
  AStream.WriteWord(WordToLE(INT_BOF_BUILD_YEAR));

  { File history flags }
  AStream.WriteDWord(DWordToLE(0));

  { Lowest Excel version that can read all records in this file 5?}
  AStream.WriteDWord(DWordToLE(0)); //?????????
end;

{*******************************************************************
*  TsSpreadBIFF8Writer.WriteBoundsheet ()
*
*  DESCRIPTION:    Writes an Excel 8 BOUNDSHEET record
*
*                  Always located on the workbook globals substream.
*
*                  One BOUNDSHEET is written for each worksheet.
*
*  RETURNS:        The stream position where the absolute stream position
*                  of the BOF of this sheet should be written (4 bytes size).
*
*******************************************************************}
function TsSpreadBIFF8Writer.WriteBoundsheet(AStream: TStream; ASheetName: string): Int64;
var
  Len: Byte;
  WideSheetName: WideString;
begin
  WideSheetName:=UTF8Decode(ASheetName);
  Len := Length(WideSheetName);

  { BIFF Record header }
  AStream.WriteWord(WordToLE(INT_EXCEL_ID_BOUNDSHEET));
  AStream.WriteWord(WordToLE(6 + 1 + 1 + Len * Sizeof(WideChar)));

  { Absolute stream position of the BOF record of the sheet represented
    by this record }
  Result := AStream.Position;
  AStream.WriteDWord(DWordToLE(0));

  { Visibility }
  AStream.WriteByte(0);

  { Sheet type }
  AStream.WriteByte(0);

  { Sheet name: Unicode string char count 1 byte }
  AStream.WriteByte(Len);
  {String flags}
  AStream.WriteByte(1);
  AStream.WriteBuffer(WideStringToLE(WideSheetName)[1], Len * Sizeof(WideChar));
end;

{*******************************************************************
*  TsSpreadBIFF8Writer.WriteIndex ()
*
*  DESCRIPTION:    Writes an Excel 8 DIMENSIONS record
*
*                  nm = (rl - rf - 1) / 32 + 1 (using integer division)
*
*******************************************************************}
procedure TsSpreadBIFF8Writer.WriteDimensions(AStream: TStream);
begin
  { BIFF Record header }
  AStream.WriteWord(WordToLE(INT_EXCEL_ID_DIMENSIONS));
  AStream.WriteWord(WordToLE(14));

  { Index to first used row }
  AStream.WriteDWord(DWordToLE(0));

  { Index to last used row, increased by 1 }
  AStream.WriteDWord(DWordToLE(33));

  { Index to first used column }
  AStream.WriteWord(WordToLE(0));

  { Index to last used column, increased by 1 }
  AStream.WriteWord(WordToLE(10));

  { Not used }
  AStream.WriteWord(WordToLE(0));
end;

{*******************************************************************
*  TsSpreadBIFF8Writer.WriteEOF ()
*
*  DESCRIPTION:    Writes an Excel 8 EOF record
*
*                  This must be the last record on an Excel 8 stream
*
*******************************************************************}
procedure TsSpreadBIFF8Writer.WriteEOF(AStream: TStream);
begin
  { BIFF Record header }
  AStream.WriteWord(WordToLE(INT_EXCEL_ID_EOF));
  AStream.WriteWord(WordToLE($0000));
end;

{*******************************************************************
*  TsSpreadBIFF8Writer.WriteFont ()
*
*  DESCRIPTION:    Writes an Excel 8 FONT record
*
*                  The font data is passed in an instance of TFPCustomFont
*
*******************************************************************}
procedure TsSpreadBIFF8Writer.WriteFont(AStream: TStream; AFont: TFPCustomFont);
var
  Len: Byte;
  WideFontName: WideString;
begin
  WideFontName:=AFont.Name;
  Len := Length(WideFontName);

  { BIFF Record header }
  AStream.WriteWord(WordToLE(INT_EXCEL_ID_FONT));
  AStream.WriteWord(WordToLE(14 + 1 + 1 + Len * Sizeof(WideChar)));

  { Height of the font in twips = 1/20 of a point }
  AStream.WriteWord(WordToLE(200));

  { Option flags }
  AStream.WriteWord(WordToLE(0));

  { Colour index }
  AStream.WriteWord(WordToLE($7FFF));

  { Font weight }
  AStream.WriteWord(WordToLE(INT_FONT_WEIGHT_NORMAL));

  { Escapement type }
  AStream.WriteWord(WordToLE(0));

  { Underline type }
  AStream.WriteByte(0);

  { Font family }
  AStream.WriteByte(0);

  { Character set }
  AStream.WriteByte(0);

  { Not used }
  AStream.WriteByte(0);

  { Font name: Unicodestring, char count in 1 byte }
  AStream.WriteByte(Len);
  { Widestring flags, 1=regular unicode LE string }
  AStream.WriteByte(1);
  AStream.WriteBuffer(WideStringToLE(WideFontName)[1], Len * Sizeof(WideChar));
end;

{*******************************************************************
*  TsSpreadBIFF8Writer.WriteFormula ()
*
*  DESCRIPTION:    Writes an Excel 5 FORMULA record
*
*                  To input a formula to this method, first convert it
*                  to RPN, and then list all it's members in the
*                  AFormula array
*
*******************************************************************}
procedure TsSpreadBIFF8Writer.WriteFormula(AStream: TStream; const ARow,
  ACol: Word; const AFormula: TsFormula);
{var
  FormulaResult: double;
  i: Integer;
  RPNLength: Word;
  TokenArraySizePos, RecordSizePos, FinalPos: Int64;}
begin
(*  RPNLength := 0;
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
    AStream.WriteByte(AFormula[i].TokenID);
    Inc(RPNLength);

    { Additional data }
    case AFormula[i].TokenID of

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

    end;
  end;

  { Write sizes in the end, after we known them }
  FinalPos := AStream.Position;
  AStream.position := TokenArraySizePos;
  AStream.WriteByte(RPNLength);
  AStream.Position := RecordSizePos;
  AStream.WriteWord(WordToLE(22 + RPNLength));
  AStream.position := FinalPos;*)
end;

{*******************************************************************
*  TsSpreadBIFF8Writer.WriteIndex ()
*
*  DESCRIPTION:    Writes an Excel 8 INDEX record
*
*                  nm = (rl - rf - 1) / 32 + 1 (using integer division)
*
*******************************************************************}
procedure TsSpreadBIFF8Writer.WriteIndex(AStream: TStream);
begin
  { BIFF Record header }
  AStream.WriteWord(WordToLE(INT_EXCEL_ID_INDEX));
  AStream.WriteWord(WordToLE(16));

  { Not used }
  AStream.WriteDWord(DWordToLE(0));

  { Index to first used row, rf, 0 based }
  AStream.WriteDWord(DWordToLE(0));

  { Index to first row of unused tail of sheet, rl, last used row + 1, 0 based }
  AStream.WriteDWord(DWordToLE(0));

  { Absolute stream position of the DEFCOLWIDTH record of the current sheet.
    If it doesn't exist, the offset points to where it would occur. }
  AStream.WriteDWord(DWordToLE($00));

  { Array of nm absolute stream positions of the DBCELL record of each Row Block }
  
  { OBS: It seams to be no problem just ignoring this part of the record }
end;

{*******************************************************************
*  TsSpreadBIFF8Writer.WriteLabel ()
*
*  DESCRIPTION:    Writes an Excel 8 LABEL record
*
*                  Writes a string to the sheet
*
*******************************************************************}
procedure TsSpreadBIFF8Writer.WriteLabel(AStream: TStream; const ARow,
  ACol: Word; const AValue: string; ACell: PCell);
var
  L: Word;
  WideValue: WideString;
begin
  WideValue := UTF8Decode(AValue);
  if WideValue = '' then
  begin
    // Bad formatted UTF8String (maybe ANSI?)
    if Length(AValue)<>0 then begin
      //It was an ANSI string written as UTF8 quite sure, so raise exception.
      Raise Exception.CreateFmt('Expected UTF8 text but probably ANSI text found in cell [%d,%d]',[ARow,ACol]);
    end;
    Exit;
  end;
  L := Length(WideValue);

  { BIFF Record header }
  AStream.WriteWord(WordToLE(INT_EXCEL_ID_LABEL));
  AStream.WriteWord(WordToLE(8 + 1 + L * Sizeof(WideChar)));

  { BIFF Record data }
  AStream.WriteWord(WordToLE(ARow));
  AStream.WriteWord(WordToLE(ACol));

  { Index to XF record, according to formatting }
  if ACell^.UsedFormattingFields = [uffTextRotation] then
  begin
    case ACell^.TextRotation of
      rt90DegreeCounterClockwiseRotation: AStream.WriteWord(WordToLE(16));
      rt90DegreeClockwiseRotation: AStream.WriteWord(WordToLE(17));
    else
      AStream.WriteWord(WordToLE(15));
    end;
  end
  else
    AStream.WriteWord(WordToLE(15));

  { Byte String with 16-bit size }
  AStream.WriteWord(WordToLE(L));

  { Byte flags. 1 means regular Unicode LE encoding}
  AStream.WriteByte(1);
  AStream.WriteBuffer(WideStringToLE(WideValue)[1], L * Sizeof(WideChar));
end;

{*******************************************************************
*  TsSpreadBIFF8Writer.WriteNumber ()
*
*  DESCRIPTION:    Writes an Excel 8 NUMBER record
*
*                  Writes a number (64-bit floating point) to the sheet
*
*******************************************************************}
procedure TsSpreadBIFF8Writer.WriteNumber(AStream: TStream; const ARow,
  ACol: Cardinal; const AValue: double);
begin
  { BIFF Record header }
  AStream.WriteWord(WordToLE(INT_EXCEL_ID_NUMBER));
  AStream.WriteWord(WordToLE(14));

  { BIFF Record data }
  AStream.WriteWord(WordToLE(ARow));
  AStream.WriteWord(WordToLE(ACol));

  { Index to XF record }
  AStream.WriteWord(WordToLE($0));

  { IEE 754 floating-point value (is different in BIGENDIAN???) }
  AStream.WriteBuffer(AValue, 8);
end;

{*******************************************************************
*  TsSpreadBIFF8Writer.WriteStyle ()
*
*  DESCRIPTION:    Writes an Excel 8 STYLE record
*
*                  Registers the name of a user-defined style or
*                  specific options for a built-in cell style.
*
*******************************************************************}
procedure TsSpreadBIFF8Writer.WriteStyle(AStream: TStream);
begin
  { BIFF Record header }
  AStream.WriteWord(WordToLE(INT_EXCEL_ID_STYLE));
  AStream.WriteWord(WordToLE(4));

  { Index to style XF and defines if it's a built-in or used defined style }
  AStream.WriteWord(WordToLE(MASK_STYLE_BUILT_IN));

  { Built-in cell style identifier }
  AStream.WriteByte($00);

  { Level if the identifier for a built-in style is RowLevel or ColLevel, $FF otherwise }
  AStream.WriteByte($FF);
end;

{*******************************************************************
*  TsSpreadBIFF8Writer.WriteWindow1 ()
*
*  DESCRIPTION:    Writes an Excel 8 WINDOW1 record
*
*                  This record contains general settings for the
*                  document window and global workbook settings.
*
*                  The values written here are reasonable defaults,
*                  which should work for most sheets.
*
*******************************************************************}
procedure TsSpreadBIFF8Writer.WriteWindow1(AStream: TStream);
begin
  { BIFF Record header }
  AStream.WriteWord(WordToLE(INT_EXCEL_ID_WINDOW1));
  AStream.WriteWord(WordToLE(18));

  { Horizontal position of the document window, in twips = 1 / 20 of a point }
  AStream.WriteWord(WordToLE(0));

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
  AStream.WriteWord(WordToLE($00));

  { Index of first visible tab in the worksheet tab bar }
  AStream.WriteWord(WordToLE($00));

  { Number of selected worksheets }
  AStream.WriteWord(WordToLE(1));

  { Width of worksheet tab bar (in 1/1000 of window width).
    The remaining space is used by the horizontal scroll bar }
  AStream.WriteWord(WordToLE(600));
end;

{*******************************************************************
*  TsSpreadBIFF8Writer.WriteWindow2 ()
*
*  DESCRIPTION:    Writes an Excel 8 WINDOW2 record
*
*                  This record contains aditional settings for the
*                  document window (BIFF2-BIFF4) or for a specific
*                  worksheet (BIFF5-BIFF8).
*
*                  The values written here are reasonable defaults,
*                  which should work for most sheets.
*
*******************************************************************}
procedure TsSpreadBIFF8Writer.WriteWindow2(AStream: TStream;
 ASheetSelected: Boolean);
var
  Options: Word;
begin
  { BIFF Record header }
  AStream.WriteWord(WordToLE(INT_EXCEL_ID_WINDOW2));
  AStream.WriteWord(WordToLE(18));

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

  { Grid line index colour }
  AStream.WriteWord(WordToLE(0));

  { Not used }
  AStream.WriteWord(WordToLE(0));

  { Cached magnification factor in page break preview (in percent); 0 = Default (60%) }
  AStream.WriteWord(WordToLE(0));

  { Cached magnification factor in normal view (in percent); 0 = Default (100%) }
  AStream.WriteWord(WordToLE(0));

  { Not used }
  AStream.WriteDWord(DWordToLE(0));
end;

{*******************************************************************
*  TsSpreadBIFF8Writer.WriteXF ()
*
*  DESCRIPTION:    Writes an Excel 8 XF record
*
*
*
*******************************************************************}
procedure TsSpreadBIFF8Writer.WriteXF(AStream: TStream; AFontIndex: Word;
 AXF_TYPE_PROT, ATextRotation: Byte);
var
  XFOptions: Word;
  XFAlignment, XFOrientationAttrib: Byte;
begin
  { BIFF Record header }
  AStream.WriteWord(WordToLE(INT_EXCEL_ID_XF));
  AStream.WriteWord(WordToLE(20));

  { Index to FONT record }
  AStream.WriteWord(WordToLE(AFontIndex));

  { Index to FORMAT record }
  AStream.WriteWord(WordToLE($00));

  { XF type, cell protection and parent style XF }
  XFOptions := AXF_TYPE_PROT and MASK_XF_TYPE_PROT;

  if AXF_TYPE_PROT and MASK_XF_TYPE_PROT_STYLE_XF <> 0 then
   XFOptions := XFOptions or MASK_XF_TYPE_PROT_PARENT;
   
  AStream.WriteWord(WordToLE(XFOptions));

  { Alignment and text break }
  XFAlignment := MASK_XF_VERT_ALIGN_BOTTOM;

  AStream.WriteByte(XFAlignment);

  { Text rotation }
  AStream.WriteByte(ATextRotation); // 0 is horizontal / normal

  { Indentation, shrink and text direction }
  AStream.WriteByte(0);

  { Used attributes }
  XFOrientationAttrib :=
   MASK_XF_USED_ATTRIB_NUMBER_FORMAT or
   MASK_XF_USED_ATTRIB_FONT or
   MASK_XF_USED_ATTRIB_TEXT or
   MASK_XF_USED_ATTRIB_BORDER_LINES or
   MASK_XF_USED_ATTRIB_BACKGROUND or
   MASK_XF_USED_ATTRIB_CELL_PROTECTION;

  AStream.WriteByte(XFOrientationAttrib);

  { Cell border lines and background area }
  AStream.WriteDWord(DWordToLE($00000000));
  AStream.WriteDWord(DWordToLE($00000000));
  AStream.WriteByte(0);
  AStream.WriteByte(0);
end;

{ TsSpreadBIFF8Reader }

function TsSpreadBIFF8Reader.DecodeRKValue(const ARK: DWORD): Double;
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
    // NOTE: This is endian dependent and IEEE dependent (Not checked) (working win-i386)
    (PDWORD(@Number))^:= $00000000;
    (PDWORD(@Number)+1)^:=(ARK and $FFFFFFFC);
  end;
  if ARK and 1 = 1 then begin
    // Encoded value is multiplied by 100
    Number:=Number / 100;
  end;
  Result:=Number;
end;

function TsSpreadBIFF8Reader.ReadWideString(const AStream: TStream;
  const ALength: WORD): WideString;
var
  StringFlags: BYTE;
  AnsiStrValue: AnsiString;
  RunsCounter: WORD;
  AsianPhoneticBytes: DWORD;
  j: SizeUInt;
begin
  StringFlags:=AStream.ReadByte;
  Dec(PendingRecordSize);
  if StringFlags and 4 = 4 then begin
    //Asian phonetics
    //Read Asian phonetics Length (not used)
    AsianPhoneticBytes:=DWordLEtoN(AStream.ReadDWord);
  end;
  if StringFlags and 8 = 8 then begin
    //Rich string
    RunsCounter:=WordLEtoN(AStream.ReadWord);
    dec(PendingRecordSize,2);
  end;
  if StringFlags and 1 = 1 Then begin
    //String is WideStringLE
    if (ALength*SizeOf(WideChar)) > PendingRecordSize then begin
      SetLength(Result,PendingRecordSize);
      AStream.ReadBuffer(Result[1],PendingRecordSize * SizeOf(WideChar));
      Dec(PendingRecordSize,PendingRecordSize * SizeOf(WideChar));
    end else begin
      SetLength(Result,ALength);
      AStream.ReadBuffer(Result[1],ALength * SizeOf(WideChar));
      Dec(PendingRecordSize,ALength * SizeOf(WideChar));
    end;
    Result:=WideStringLEToN(Result);
  end else begin
    //String is 1 byte per char, maybe ANSI ?
    if ALength > PendingRecordSize then begin
      SetLength(AnsiStrValue,PendingRecordSize);
      AStream.ReadBuffer(AnsiStrValue[1],PendingRecordSize);
      dec(PendingRecordSize,PendingRecordSize);
    end else begin
      SetLength(AnsiStrValue,ALength);
      AStream.ReadBuffer(AnsiStrValue[1],ALength);
      dec(PendingRecordSize,ALength);
    end;
    Result:=AnsiStrValue; //implicit conversion.
  end;
  if StringFlags and 8 = 8 then begin
    //Rich string (This only happend in BIFF8)
    for j := 1 to RunsCounter do begin
      AStream.ReadWord;
      AStream.ReadWord;
      dec(PendingRecordSize,2*2);
    end;
  end;
  if StringFlags and 4 = 4 then begin
    //Asian phonetics
    //Read Asian phonetics, discarded as not used.
    SetLength(AnsiStrValue,AsianPhoneticBytes);
    AStream.ReadBuffer(AnsiStrValue[1],AsianPhoneticBytes);
  end;
end;

procedure TsSpreadBIFF8Reader.ReadWorkbookGlobals(AStream: TStream;
  AData: TsWorkbook);
var
  SectionEOF: Boolean = False;
  RecordType: Word;
  CurStreamPos: Int64;
begin
  if Assigned(FSharedStringTable) then FreeAndNIL(FSharedStringTable);
  while (not SectionEOF) do
  begin
    { Read the record header }
    RecordType := WordLEToN(AStream.ReadWord);
    RecordSize := WordLEToN(AStream.ReadWord);
    PendingRecordSize:=RecordSize;

    CurStreamPos := AStream.Position;

    if RecordType<>INT_EXCEL_ID_CONTINUE then begin
      case RecordType of
       INT_EXCEL_ID_BOF:        ;
       INT_EXCEL_ID_BOUNDSHEET: ReadBoundSheet(AStream);
       INT_EXCEL_ID_EOF:        SectionEOF := True;
       INT_EXCEL_ID_SST:        ReadSST(AStream);
      else
        // nothing
      end;
    end;

    // Make sure we are in the right position for the next record
    AStream.Seek(CurStreamPos + RecordSize, soFromBeginning);

    // Check for the end of the file
    if AStream.Position >= AStream.Size then SectionEOF := True;
  end;
end;

procedure TsSpreadBIFF8Reader.ReadWorksheet(AStream: TStream; AData: TsWorkbook);
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
    INT_EXCEL_ID_LABELSST:ReadLabelSST(AStream); //BIFF8 only
    INT_EXCEL_ID_FORMULA: ReadFormulaExcel(AStream);
    INT_EXCEL_ID_BOF:     ;
    INT_EXCEL_ID_EOF:     SectionEOF := True;
    else
      // nothing
    end;

    // Make sure we are in the right position for the next record
    AStream.Seek(CurStreamPos + RecordSize, soFromBeginning);

    // Check for the end of the file
    if AStream.Position >= AStream.Size then SectionEOF := True;
  end;
end;

procedure TsSpreadBIFF8Reader.ReadBoundsheet(AStream: TStream);
var
  Len: Byte;
  WideName: WideString;
begin
  { Absolute stream position of the BOF record of the sheet represented
    by this record }
  // Just assume that they are in order
  AStream.ReadDWord();

  { Visibility }
  AStream.ReadByte();

  { Sheet type }
  AStream.ReadByte();

  { Sheet name: 8-bit length }
  Len := AStream.ReadByte();

  { Read string with flags }
  WideName:=ReadWideString(AStream,Len);

  FWorksheetNames.Add(UTF8Encode(WideName));
end;

procedure TsSpreadBIFF8Reader.ReadRKValue(const AStream: TStream);
var
  RK: DWORD;
  ARow, ACol, XF: WORD;
  Number: Double;
begin
  ReadRowColXF(AStream,ARow,ACol,XF);

  {Encoded RK value}
  RK:=DWordLEtoN(AStream.ReadDWord);

  {Check RK codes}
  Number:=DecodeRKValue(RK);
  FWorksheet.WriteNumber(ARow,ACol,Number);
end;

procedure TsSpreadBIFF8Reader.ReadMulRKValues(const AStream: TStream);
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

procedure TsSpreadBIFF8Reader.ReadFormulaExcel(AStream: TStream);
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

procedure TsSpreadBIFF8Reader.ReadRowColXF(const AStream: TStream; out ARow,
  ACol, AXF: WORD);
begin
  { BIFF Record data }
  ARow := WordLEToN(AStream.ReadWord);
  ACol := WordLEToN(AStream.ReadWord);

  { Index to XF record }
  AXF:=WordLEtoN(AStream.ReadWord);
end;

function TsSpreadBIFF8Reader.ReadString(const AStream: TStream;
  const ALength: WORD): UTF8String;
begin
  Result:=UTF8Encode(ReadWideString(AStream, ALength));
end;

procedure TsSpreadBIFF8Reader.ReadFromFile(AFileName: string; AData: TsWorkbook);
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
    OLEStorage.ReadOLEFile(AFileName, OLEDocument,'Workbook');

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

procedure TsSpreadBIFF8Reader.ReadFromStream(AStream: TStream; AData: TsWorkbook);
var
  BIFF8EOF: Boolean;
begin
  { Initializations }

  FWorksheetNames := TStringList.Create;
  FWorksheetNames.Clear;
  FCurrentWorksheet := 0;
  BIFF8EOF := False;

  { Read workbook globals }

  ReadWorkbookGlobals(AStream, AData);

  // Check for the end of the file
  if AStream.Position >= AStream.Size then BIFF8EOF := True;

  { Now read all worksheets }

  while (not BIFF8EOF) do
  begin
    //Safe to not read beyond assigned worksheet names.
    if FCurrentWorksheet>FWorksheetNames.Count-1 then break;

    ReadWorksheet(AStream, AData);

    // Check for the end of the file
    if AStream.Position >= AStream.Size then BIFF8EOF := True;

    // Final preparations
    Inc(FCurrentWorksheet);
  end;

  { Finalizations }

  FWorksheetNames.Free;
end;

procedure TsSpreadBIFF8Reader.ReadFormula(AStream: TStream);
begin

end;

procedure TsSpreadBIFF8Reader.ReadLabel(AStream: TStream);
var
  L: Word;
  StringFlags: BYTE;
  ARow, ACol: Word;
  WideStrValue: WideString;
  AnsiStrValue: AnsiString;
begin
  { BIFF Record data }
  ARow := WordLEToN(AStream.ReadWord);
  ACol := WordLEToN(AStream.ReadWord);

  { Index to XF record, not used }
  AStream.ReadWord();

  { Byte String with 16-bit size }
  L := WordLEtoN(AStream.ReadWord());

  { Read string with flags }
  WideStrValue:=ReadWideString(AStream,L);

  { Save the data }
  FWorksheet.WriteUTF8Text(ARow, ACol, UTF8Encode(WideStrValue));
end;

procedure TsSpreadBIFF8Reader.ReadNumber(AStream: TStream);
var
  ARow, ACol: Word;
  AValue: Double;
begin
  { BIFF Record data }
  ARow := WordLEToN(AStream.ReadWord);
  ACol := WordLEToN(AStream.ReadWord);

  { Index to XF record, not used }
  AStream.ReadWord();

  { IEE 754 floating-point value }
  AStream.ReadBuffer(AValue, 8);

  { Save the data }
  FWorksheet.WriteNumber(ARow, ACol, AValue);
end;

destructor TsSpreadBIFF8Reader.Destroy;
begin
  if Assigned(FSharedStringTable) then FSharedStringTable.Free;
end;

procedure TsSpreadBIFF8Reader.ReadRichString(const AStream: TStream);
var
  L: Word;
  B: WORD;
  ARow, ACol, XF: Word;
  AStrValue: ansistring;
begin
  ReadRowColXF(AStream,ARow,ACol,XF);

  { Byte String with 16-bit size }
  L := WordLEtoN(AStream.ReadWord());
  AStrValue:=ReadString(AStream,L);

  { Save the data }
  FWorksheet.WriteUTF8Text(ARow, ACol, AStrValue);
  //Read formatting runs (not supported)
  B:=WordLEtoN(AStream.ReadWord);
  for L := 0 to B-1 do begin
    AStream.ReadWord; // First formatted character
    AStream.ReadWord; // Index to FONT record
  end;
end;

procedure TsSpreadBIFF8Reader.ReadSST(const AStream: TStream);
var
  Items: DWORD;
  StringLength: WORD;
  LString: String;
  Continue: WORD;
begin
  //Reads the shared string table, only compatible with BIFF8
  if not Assigned(FSharedStringTable) then begin
    //First time SST creation
    FSharedStringTable:=TStringList.Create;

    DWordLEtoN(AStream.ReadDWord); //Apparences not used
    Items:=DWordLEtoN(AStream.ReadDWord);
    Dec(PendingRecordSize,8);
  end else begin
    //A second record must not happend. Garbage so skip.
    Exit;
  end;
  while Items>0 do begin
    StringLength:=0;
    StringLength:=WordLEtoN(AStream.ReadWord);
    Dec(PendingRecordSize,2);
    LString:='';
    while PendingRecordSize>0 do begin
      if StringLength>0 then begin
        //Read a stream of zero length reads all the stream.
        LString:=LString+ReadString(AStream,StringLength);
      end;
      if (PendingRecordSize=0) and (Items>1) then begin
        //A continue will happend, read the continue
        //tag and continue linking...
        Continue:=WordLEtoN(AStream.ReadWord);
        if Continue<>INT_EXCEL_ID_CONTINUE then begin
          Raise Exception.Create('Expected CONTINUE not found.');
        end;
        PendingRecordSize:=WordLEtoN(AStream.ReadWord);
        Dec(StringLength,Length(UTF8Decode(LString))); //Dec the used chars
      end else begin
        break;
      end;
    end;
    FSharedStringTable.Add(LString);
    dec(Items);
  end;
end;

procedure TsSpreadBIFF8Reader.ReadLabelSST(const AStream: TStream);
var
  ACol,ARow,XF: WORD;
  SSTIndex: DWORD;
begin
  ReadRowColXF(AStream,ARow,ACol,XF);
  SSTIndex:=DWordLEtoN(AStream.ReadDWord);
  if SizeInt(SSTIndex)>=FSharedStringTable.Count then begin
    Raise Exception.CreateFmt('Index %d in SST out of range (0-%d)',[Integer(SSTIndex),FSharedStringTable.Count-1]);
  end;
  FWorksheet.WriteUTF8Text(ARow, ACol, FSharedStringTable[SSTIndex]);
end;

{*******************************************************************
*  Initialization section
*
*  Registers this reader / writer on fpSpreadsheet
*
*******************************************************************}

initialization

  RegisterSpreadFormat(TsSpreadBIFF8Reader, TsSpreadBIFF8Writer, sfExcel8);

end.

