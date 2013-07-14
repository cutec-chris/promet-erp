{
fpspreadsheet.pas

Writes an spreadsheet document

AUTHORS: Felipe Monteiro de Carvalho
}
unit fpspreadsheet;

{$ifdef fpc}
  {$mode delphi}
{$endif}

interface

uses
  Classes, SysUtils, AVL_Tree, fpsconvencoding;

type
  TsSpreadsheetFormat = (sfExcel2, sfExcel3, sfExcel4, sfExcel5, sfExcel8,
   sfOOXML, sfOpenDocument, sfCSV);

const
  { Default extensions }
  STR_EXCEL_EXTENSION = '.xls';
  STR_OOXML_EXCEL_EXTENSION = '.xlsx';
  STR_OPENDOCUMENT_CALC_EXTENSION = '.ods';

type

  {@@ Possible encodings for a non-unicode encoded text }
  TsEncoding = (
    seLatin1,
    seLatin2,
    seCyrillic,
    seGreek,
    seTurkish,
    seHebrew,
    seArabic
    );

  {@@ Describes a formula

    Supported syntax:

    =A1+B1+C1/D2...  - Array with simple mathematical operations

    =SUM(A1:D1)      - SUM operation in a interval
  }

  TsFormula = record
    FormulaStr: string;
    DoubleValue: double;
  end;

  {@@ Expanded formula. Used by backend modules. Provides more information then the text only }

  TFEKind = (
    { Basic operands }
    fekCell, fekNum,
    { Basic operations }
    fekAdd, fekSub, fekDiv, fekMul,
    { Build-in Functions}
    fekABS, fekROUND,
    { Other operations }
    fekOpSUM
    );

  TsFormulaElement = record
    ElementKind: TFEKind;
    Row, Row2: Word; // zero-based
    Col, Col2: Byte; // zero-based
    Param1, Param2: Word; // Extra parameters
    DoubleValue: double;
  end;

  TsExpandedFormula = array of TsFormulaElement;

  {@@ RPN formula. Similar to the expanded formula, but in RPN notation.
      Simplifies the task of format writers which need RPN }

  TsRPNFormula = array of TsFormulaElement;

  {@@ Describes the type of content of a cell on a TsWorksheet }

  TCellContentType = (cctEmpty, cctFormula, cctRPNFormula, cctNumber,
    cctUTF8String);

  {@@ List of possible formatting fields }

  TsUsedFormattingField = (uffTextRotation);

  {@@ Describes which formatting fields are active }

  TsUsedFormattingFields = set of TsUsedFormattingField;

  {@@ Text rotation formatting}

  TsTextRotation = (trHorizontal, rt90DegreeClockwiseRotation,
    rt90DegreeCounterClockwiseRotation);

  {@@ Cell structure for TsWorksheet

      Never suppose that all *Value fields are valid,
      only one of the ContentTypes is valid. For other fields
      use TWorksheet.ReadAsUTF8Text and similar methods

      @see TWorksheet.ReadAsUTF8Text
  }

  TCell = record
    Col: Byte; // zero-based
    Row: Word; // zero-based
    ContentType: TCellContentType;
    { Possible values for the cells }
    FormulaValue: TsFormula;
    RPNFormulaValue: TsRPNFormula;
    NumberValue: double;
    UTF8StringValue: ansistring;
    { Formatting fields }
    UsedFormattingFields: TsUsedFormattingFields;
    TextRotation: TsTextRotation;
  end;

  PCell = ^TCell;

type

  TsCustomSpreadReader = class;
  TsCustomSpreadWriter = class;

  { TsWorksheet }

  TsWorksheet = class
  private
    FCells: TAvlTree;
    FCurrentNode: TAVLTreeNode; // For GetFirstCell and GetNextCell
    procedure RemoveCallback(data, arg: pointer);
  public
    Name: string;
    { Base methods }
    constructor Create;
    destructor Destroy; override;
    { Data manipulation methods }
    function  FindCell(ARow, ACol: Cardinal): PCell;
    function  GetCell(ARow, ACol: Cardinal): PCell;
    function  GetCellCount: Cardinal;
    function  GetFirstCell(): PCell;
    function  GetNextCell(): PCell;
    function  GetLastColNumber: Cardinal;
    function  GetLastRowNumber: Cardinal;
    function  ReadAsUTF8Text(ARow, ACol: Cardinal): ansistring;
    function  ReadAsNumber(ARow, ACol: Cardinal): Double;
    procedure RemoveAllCells;
    procedure WriteUTF8Text(ARow, ACol: Cardinal; AText: ansistring);
    procedure WriteNumber(ARow, ACol: Cardinal; ANumber: double);
    procedure WriteFormula(ARow, ACol: Cardinal; AFormula: TsFormula);
    procedure WriteRPNFormula(ARow, ACol: Cardinal; AFormula: TsRPNFormula);
    procedure WriteTextRotation(ARow, ACol: Cardinal; ARotation: TsTextRotation);
    property  Cells: TAVLTree read FCells;
  end;

  { TsWorkbook }

  TsWorkbook = class
  private
    { Internal data }
    FWorksheets: TFPList;
    FEncoding: TsEncoding;
    { Internal methods }
    procedure RemoveCallback(data, arg: pointer);
  public
    { Base methods }
    constructor Create;
    destructor Destroy; override;
    function  CreateSpreadReader(AFormat: TsSpreadsheetFormat): TsCustomSpreadReader;
    function  CreateSpreadWriter(AFormat: TsSpreadsheetFormat): TsCustomSpreadWriter;
    procedure ReadFromFile(AFileName: string; AFormat: TsSpreadsheetFormat);
    procedure ReadFromStream(AStream: TStream; AFormat: TsSpreadsheetFormat);
    procedure WriteToFile(const AFileName: string;
      const AFormat: TsSpreadsheetFormat;
      const AOverwriteExisting: Boolean = False);
    procedure WriteToStream(AStream: TStream; AFormat: TsSpreadsheetFormat);
    { Worksheet list handling methods }
    function  AddWorksheet(AName: string): TsWorksheet;
    function  GetFirstWorksheet: TsWorksheet;
    function  GetWorksheetByIndex(AIndex: Cardinal): TsWorksheet;
    function  GetWorksheetCount: Cardinal;
    procedure RemoveAllWorksheets;
    {@@ This property is only used for formats which don't support unicode
      and support a single encoding for the whole document, like Excel 2 to 5 }
    property Encoding: TsEncoding read FEncoding write FEncoding;
  end;

  {@@ TsSpreadReader class reference type }

  TsSpreadReaderClass = class of TsCustomSpreadReader;
  
  { TsCustomSpreadReader }

  TsCustomSpreadReader = class
  protected
    FWorkbook: TsWorkbook;
    FCurrentWorksheet: TsWorksheet;
  public
    { General writing methods }
    procedure ReadFromFile(AFileName: string; AData: TsWorkbook); virtual;
    procedure ReadFromStream(AStream: TStream; AData: TsWorkbook); virtual;
    { Record reading methods }
    procedure ReadFormula(AStream: TStream); virtual; abstract;
    procedure ReadLabel(AStream: TStream); virtual; abstract;
    procedure ReadNumber(AStream: TStream); virtual; abstract;
  end;

  {@@ TsSpreadWriter class reference type }

  TsSpreadWriterClass = class of TsCustomSpreadWriter;

  { TsCustomSpreadWriter }

  TsCustomSpreadWriter = class
  public
    { Helper routines }
    function  ExpandFormula(AFormula: TsFormula): TsExpandedFormula;
    { General writing methods }
    procedure WriteCellCallback(data, arg: pointer);
    procedure WriteCellsToStream(AStream: TStream; ACells: TAVLTree);
    procedure WriteToFile(const AFileName: string; AData: TsWorkbook;
      const AOverwriteExisting: Boolean = False); virtual;
    procedure WriteToStream(AStream: TStream; AData: TsWorkbook); virtual;
    { Record writing methods }
    procedure WriteFormula(AStream: TStream; const ARow, ACol: Word; const AFormula: TsFormula); virtual;
    procedure WriteRPNFormula(AStream: TStream; const ARow, ACol: Word; const AFormula: TsRPNFormula); virtual;
    procedure WriteLabel(AStream: TStream; const ARow, ACol: Word; const AValue: string; ACell: PCell); virtual; abstract;
    procedure WriteNumber(AStream: TStream; const ARow, ACol: Cardinal; const AValue: double); virtual; abstract;
  end;

  {@@ List of registered formats }

  TsSpreadFormatData = record
    ReaderClass: TsSpreadReaderClass;
    WriterClass: TsSpreadWriterClass;
    Format: TsSpreadsheetFormat;
  end;
  
var
  GsSpreadFormats: array of TsSpreadFormatData;

procedure RegisterSpreadFormat(
  AReaderClass: TsSpreadReaderClass;
  AWriterClass: TsSpreadWriterClass;
  AFormat: TsSpreadsheetFormat);



implementation

uses
  Math;

var
  { Translatable strings }
  lpUnsupportedReadFormat, lpUnsupportedWriteFormat: string;

{@@
  Registers a new reader/writer pair for a format
}
procedure RegisterSpreadFormat(
  AReaderClass: TsSpreadReaderClass;
  AWriterClass: TsSpreadWriterClass;
  AFormat: TsSpreadsheetFormat);
var
  len: Integer;
begin
  len := Length(GsSpreadFormats);
  SetLength(GsSpreadFormats, len + 1);
  
  GsSpreadFormats[len].ReaderClass := AReaderClass;
  GsSpreadFormats[len].WriterClass := AWriterClass;
  GsSpreadFormats[len].Format := AFormat;
end;

{ TsWorksheet }

{@@
  Helper method for clearing the records in a spreadsheet.
}
procedure TsWorksheet.RemoveCallback(data, arg: pointer);
begin
  { The UTF8STring must be manually reseted to nil content, because
    FreeMem only frees the record mem, without checking its content }
  PCell(data).UTF8StringValue:='';
  FreeMem(data);
end;

function CompareCells(Item1, Item2: Pointer): Integer;
begin
  result := PCell(Item1).Row - PCell(Item2).Row;
  if Result = 0 then
    Result := PCell(Item1).Col - PCell(Item2).Col;
end;

{@@
  Constructor.
}
constructor TsWorksheet.Create;
begin
  inherited Create;

  FCells := TAVLTree.Create(@CompareCells);
end;

{@@
  Destructor.
}
destructor TsWorksheet.Destroy;
begin
  RemoveAllCells;

  FCells.Free;

  inherited Destroy;
end;

{@@
  Tryes to locate a Cell in the list of already
  written Cells

  @param  ARow      The row of the cell
  @param  ACol      The column of the cell

  @return Nil if no existing cell was found,
          otherwise a pointer to the desired Cell

  @see    TCell
}
function TsWorksheet.FindCell(ARow, ACol: Cardinal): PCell;
var
  LCell: TCell;
  AVLNode: TAVLTreeNode;
begin
  Result := nil;

  LCell.Row := ARow;
  LCell.Col := ACol;
  AVLNode := FCells.Find(@LCell);
  if Assigned(AVLNode) then
    result := PCell(AVLNode.Data);
end;

{@@
  Obtains an allocated cell at the desired location.

  If the Cell already exists, a pointer to it will
  be returned.

  If not, then new memory for the cell will be allocated,
  a pointer to it will be returned and it will be added
  to the list of Cells.

  @param  ARow      The row of the cell
  @param  ACol      The column of the cell

  @return A pointer to the Cell on the desired location.

  @see    TCell
}
function TsWorksheet.GetCell(ARow, ACol: Cardinal): PCell;
begin
  Result := FindCell(ARow, ACol);
  
  if (Result = nil) then
  begin
    Result := GetMem(SizeOf(TCell));
    FillChar(Result^, SizeOf(TCell), #0);

    Result^.Row := ARow;
    Result^.Col := ACol;

    Cells.Add(Result);
  end;
end;

{@@
  Returns the number of cells in the worksheet with contents.

  This routine is used together with GetFirstCell and GetNextCell
  to iterate througth all cells in a worksheet efficiently.

  @return The number of cells with contents in the worksheet

  @see    TCell
  @see    GetFirstCell
  @see    GetNextCell
}
function TsWorksheet.GetCellCount: Cardinal;
begin
  Result := FCells.Count;
end;

{@@
  Returns the first Cell.

  Use together with GetCellCount and GetNextCell
  to iterate througth all cells in a worksheet efficiently.

  @return The first cell if any exists, nil otherwise

  @see    TCell
  @see    GetCellCount
  @see    GetNextCell
}
function TsWorksheet.GetFirstCell(): PCell;
begin
  FCurrentNode := FCells.FindLowest();
  if FCurrentNode <> nil then
    Result := PCell(FCurrentNode.Data)
  else Result := nil;
end;

{@@
  Returns the next Cell.

  Should always be used either after GetFirstCell or
  after GetNextCell.

  Use together with GetCellCount and GetFirstCell
  to iterate througth all cells in a worksheet efficiently.

  @return The first cell if any exists, nil otherwise

  @see    TCell
  @see    GetCellCount
  @see    GetFirstCell
}
function TsWorksheet.GetNextCell(): PCell;
begin
  FCurrentNode := FCells.FindSuccessor(FCurrentNode);
  if FCurrentNode <> nil then
    Result := PCell(FCurrentNode.Data)
  else Result := nil;
end;

{@@
  Returns the 0-based number of the last column with a cell with contents.

  If no cells have contents, zero will be returned, which is also a valid value.

  Use GetCellCount to verify if there is at least one cell with contents in the
  worksheet.

  @see GetCellCount
}
function TsWorksheet.GetLastColNumber: Cardinal;
var
  AVLNode: TAVLTreeNode;
begin
  Result := 0;

  // Traverse the tree from lowest to highest.
  // Since tree primary sort order is on Row
  // highest Col could exist anywhere.
  AVLNode := FCells.FindLowest;
  While Assigned(AVLNode) do
  begin
    Result := Math.Max(Result, PCell(AVLNode.Data)^.Col);
    AVLNode := FCells.FindSuccessor(AVLNode);
  end;
end;

{@@
  Returns the 0-based number of the last row with a cell with contents.

  If no cells have contents, zero will be returned, which is also a valid value.

  Use GetCellCount to verify if there is at least one cell with contents in the
  worksheet.

  @see GetCellCount
}
function TsWorksheet.GetLastRowNumber: Cardinal;
var
  AVLNode: TAVLTreeNode;
begin
  Result := 0;

  AVLNode := FCells.FindHighest;
  if Assigned(AVLNode) then
    Result := PCell(AVLNode.Data).Row;
end;

{@@
  Reads the contents of a cell and returns an user readable text
  representing the contents of the cell.

  The resulting ansistring is UTF-8 encoded.

  @param  ARow      The row of the cell
  @param  ACol      The column of the cell

  @return The text representation of the cell
}
function TsWorksheet.ReadAsUTF8Text(ARow, ACol: Cardinal): ansistring;
var
  ACell: PCell;
begin
  ACell := FindCell(ARow, ACol);

  if ACell = nil then
  begin
    Result := '';
    Exit;
  end;

  case ACell^.ContentType of

  //cctFormula
  cctNumber:     Result := FloatToStr(ACell^.NumberValue);
  cctUTF8String:     Result := ACell^.UTF8StringValue;
  else
    Result := '';
  end;
end;

function TsWorksheet.ReadAsNumber(ARow, ACol: Cardinal): Double;
var
  ACell: PCell;
  Str: string;
begin
  ACell := FindCell(ARow, ACol);

  if ACell = nil then
  begin
    Result := 0.0;
    Exit;
  end;

  case ACell^.ContentType of

  //cctFormula
  cctNumber:     Result := ACell^.NumberValue;
  cctUTF8String:
  begin
    // The try is necessary to catch errors while converting the string
    // to a number, an operation which may fail
    try
      Str := ACell^.UTF8StringValue;
      Result := StrToFloat(Str);
    except
      Result := 0.0;
    end;
  end;

  else
    Result := 0.0;
  end;
end;

{@@
  Clears the list of Cells and releases their memory.
}
procedure TsWorksheet.RemoveAllCells;
var
  Node: TAVLTreeNode;
begin
  Node:=FCells.FindLowest;
  while Assigned(Node) do begin
    RemoveCallback(Node.Data,nil);
    Node.Data:=nil;
    Node:=FCells.FindSuccessor(Node);
  end;
  FCells.Clear;
end;

{@@
  Writes UTF-8 encoded text to a determined cell.

  On formats that don't support unicode, the text will be converted
  to ISO Latin 1.

  @param  ARow      The row of the cell
  @param  ACol      The column of the cell
  @param  AText     The text to be written encoded in utf-8
}
procedure TsWorksheet.WriteUTF8Text(ARow, ACol: Cardinal; AText: ansistring);
var
  ACell: PCell;
begin
  ACell := GetCell(ARow, ACol);

  ACell^.ContentType := cctUTF8String;
  ACell^.UTF8StringValue := AText;
end;

{@@
  Writes a floating-point number to a determined cell

  @param  ARow      The row of the cell
  @param  ACol      The column of the cell
  @param  ANumber   The number to be written
}
procedure TsWorksheet.WriteNumber(ARow, ACol: Cardinal; ANumber: double);
var
  ACell: PCell;
begin
  ACell := GetCell(ARow, ACol);

  ACell^.ContentType := cctNumber;
  ACell^.NumberValue := ANumber;
end;

{@@
  Writes a formula to a determined cell

  @param  ARow      The row of the cell
  @param  ACol      The column of the cell
  @param  AFormula  The formula to be written
}
procedure TsWorksheet.WriteFormula(ARow, ACol: Cardinal; AFormula: TsFormula);
var
  ACell: PCell;
begin
  ACell := GetCell(ARow, ACol);

  ACell^.ContentType := cctFormula;
  ACell^.FormulaValue := AFormula;
end;

procedure TsWorksheet.WriteRPNFormula(ARow, ACol: Cardinal;
  AFormula: TsRPNFormula);
var
  ACell: PCell;
begin
  ACell := GetCell(ARow, ACol);

  ACell^.ContentType := cctRPNFormula;
  ACell^.RPNFormulaValue := AFormula;
end;

{@@
  Adds text rotation to the formatting of a cell

  @param  ARow      The row of the cell
  @param  ACol      The column of the cell
  @param  ARotation How to rotate the text

  @see    TsTextRotation
}
procedure TsWorksheet.WriteTextRotation(ARow, ACol: Cardinal;
  ARotation: TsTextRotation);
var
  ACell: PCell;
begin
  ACell := GetCell(ARow, ACol);

  Include(ACell^.UsedFormattingFields, uffTextRotation);
  ACell^.TextRotation := ARotation;
end;

{ TsWorkbook }

{@@
  Helper method for clearing the spreadsheet list.
}
procedure TsWorkbook.RemoveCallback(data, arg: pointer);
begin
  TsWorksheet(data).Free;
end;

{@@
  Constructor.
}
constructor TsWorkbook.Create;
begin
  inherited Create;
  
  FWorksheets := TFPList.Create;

  // In the future: add support for translations
  lpUnsupportedReadFormat := 'Tryed to read a spreadsheet using an unsupported format';
  lpUnsupportedWriteFormat := 'Tryed to write a spreadsheet using an unsupported format';
end;

{@@
  Destructor.
}
destructor TsWorkbook.Destroy;
begin
  RemoveAllWorksheets;

  FWorksheets.Free;

  inherited Destroy;
end;

{@@
  Convenience method which creates the correct
  reader object for a given spreadsheet format.
}
function TsWorkbook.CreateSpreadReader(AFormat: TsSpreadsheetFormat): TsCustomSpreadReader;
var
  i: Integer;
begin
  Result := nil;

  for i := 0 to Length(GsSpreadFormats) - 1 do
    if GsSpreadFormats[i].Format = AFormat then
    begin
      Result := GsSpreadFormats[i].ReaderClass.Create;

      Break;
    end;

  if Result = nil then raise Exception.Create(lpUnsupportedReadFormat);
end;

{@@
  Convenience method which creates the correct
  writer object for a given spreadsheet format.
}
function TsWorkbook.CreateSpreadWriter(AFormat: TsSpreadsheetFormat): TsCustomSpreadWriter;
var
  i: Integer;
begin
  Result := nil;

  for i := 0 to Length(GsSpreadFormats) - 1 do
    if GsSpreadFormats[i].Format = AFormat then
    begin
      Result := GsSpreadFormats[i].WriterClass.Create;
    
      Break;
    end;
    
  if Result = nil then raise Exception.Create(lpUnsupportedWriteFormat);
end;

{@@
  Reads the document from a file.
}
procedure TsWorkbook.ReadFromFile(AFileName: string;
  AFormat: TsSpreadsheetFormat);
var
  AReader: TsCustomSpreadReader;
begin
  AReader := CreateSpreadReader(AFormat);

  try
    AReader.ReadFromFile(AFileName, Self);
  finally
    AReader.Free;
  end;
end;

{@@
  Reads the document from a seekable stream.
}
procedure TsWorkbook.ReadFromStream(AStream: TStream;
  AFormat: TsSpreadsheetFormat);
var
  AReader: TsCustomSpreadReader;
begin
  AReader := CreateSpreadReader(AFormat);

  try
    AReader.ReadFromStream(AStream, Self);
  finally
    AReader.Free;
  end;
end;

{@@
  Writes the document to a file.

  If the file doesn't exist, it will be created.
}
procedure TsWorkbook.WriteToFile(const AFileName: string;
 const AFormat: TsSpreadsheetFormat; const AOverwriteExisting: Boolean = False);
var
  AWriter: TsCustomSpreadWriter;
begin
  AWriter := CreateSpreadWriter(AFormat);

  try
    AWriter.WriteToFile(AFileName, Self, AOverwriteExisting);
  finally
    AWriter.Free;
  end;
end;

{@@
  Writes the document to a stream
}
procedure TsWorkbook.WriteToStream(AStream: TStream; AFormat: TsSpreadsheetFormat);
var
  AWriter: TsCustomSpreadWriter;
begin
  AWriter := CreateSpreadWriter(AFormat);

  try
    AWriter.WriteToStream(AStream, Self);
  finally
    AWriter.Free;
  end;
end;

{@@
  Adds a new worksheet to the workbook

  It is added to the end of the list of worksheets

  @param  AName     The name of the new worksheet

  @return The instace of the newly created worksheet

  @see    TsWorkbook
}
function TsWorkbook.AddWorksheet(AName: string): TsWorksheet;
begin
  Result := TsWorksheet.Create;

  Result.Name := AName;

  FWorksheets.Add(Pointer(Result));
end;

{@@
  Quick helper routine which returns the first worksheet

  @return A TsWorksheet instance if at least one is present.
          nil otherwise.

  @see    TsWorkbook.GetWorksheetByIndex
  @see    TsWorksheet
}
function TsWorkbook.GetFirstWorksheet: TsWorksheet;
begin
  Result := TsWorksheet(FWorksheets.First);
end;

{@@
  Gets the worksheet with a given index

  The index is zero-based, so the first worksheet
  added has index 0, the second 1, etc.

  @param  AIndex    The index of the worksheet (0-based)

  @return A TsWorksheet instance if one is present at that index.
          nil otherwise.

  @see    TsWorkbook.GetFirstWorksheet
  @see    TsWorksheet
}
function TsWorkbook.GetWorksheetByIndex(AIndex: Cardinal): TsWorksheet;
begin
  if (integer(AIndex) < FWorksheets.Count) and (integer(AIndex)>=0) then Result := TsWorksheet(FWorksheets.Items[AIndex])
  else Result := nil;
end;

{@@
  The number of worksheets on the workbook

  @see    TsWorksheet
}
function TsWorkbook.GetWorksheetCount: Cardinal;
begin
  Result := FWorksheets.Count;
end;

{@@
  Clears the list of Worksheets and releases their memory.
}
procedure TsWorkbook.RemoveAllWorksheets;
begin
  FWorksheets.ForEachCall(RemoveCallback, nil);
end;

{ TsCustomSpreadReader }

{@@
  Default file reading method.

  Opens the file and calls ReadFromStream

  @param  AFileName The input file name.
  @param  AData     The Workbook to be filled with information from the file.

  @see    TsWorkbook
}
procedure TsCustomSpreadReader.ReadFromFile(AFileName: string; AData: TsWorkbook);
var
  InputFile: TFileStream;
begin
  InputFile := TFileStream.Create(AFileName, fmOpenRead);
  try
    ReadFromStream(InputFile, AData);
  finally
    InputFile.Free;
  end;
end;

{@@
  This routine should be overriden in descendent classes.
}
procedure TsCustomSpreadReader.ReadFromStream(AStream: TStream; AData: TsWorkbook);
begin
  raise Exception.Create(lpUnsupportedReadFormat);
end;

{ TsCustomSpreadWriter }

{@@
  Expands a formula, separating it in it's constituent parts,
  so that it is already partially parsed and it is easier to
  convert it into the format supported by the writer module
}
function TsCustomSpreadWriter.ExpandFormula(AFormula: TsFormula): TsExpandedFormula;
var
  StrPos: Integer;
  ResPos: Integer;
begin
  ResPos := -1;
  SetLength(Result, 0);

  // The formula needs to start with a =
  if AFormula.FormulaStr[1] <> '=' then raise Exception.Create('Formula doesn''t start with =');

  StrPos := 2;

  while Length(AFormula.FormulaStr) <= StrPos do
  begin
    // Checks for cell with the format [Letter][Number]
{    if (AFormula.FormulaStr[StrPos] in [a..zA..Z]) and
       (AFormula.FormulaStr[StrPos + 1] in [0..9]) then
    begin
      Inc(ResPos);
      SetLength(Result, ResPos + 1);
      Result[ResPos].ElementKind := fekCell;
//      Result[ResPos].Col1 := fekCell;
      Result[ResPos].Row1 := AFormula.FormulaStr[StrPos + 1];

      Inc(StrPos);
    end
    // Checks for arithmetical operations
    else} if AFormula.FormulaStr[StrPos] = '+' then
    begin
      Inc(ResPos);
      SetLength(Result, ResPos + 1);
      Result[ResPos].ElementKind := fekAdd;
    end;

    Inc(StrPos);
  end;
end;

{@@
  Helper function for the spreadsheet writers.

  @see    TsCustomSpreadWriter.WriteCellsToStream
}
procedure TsCustomSpreadWriter.WriteCellCallback(data, arg: pointer);
var
  ACell: PCell;
  AStream: TStream;
begin
  ACell := PCell(data);
  AStream := TStream(arg);

  case ACell.ContentType of
    cctNumber:  WriteNumber(AStream, ACell^.Row, ACell^.Col, ACell^.NumberValue);
    cctUTF8String:  WriteLabel(AStream, ACell^.Row, ACell^.Col, ACell^.UTF8StringValue, ACell);
    cctFormula: WriteFormula(AStream, ACell^.Row, ACell^.Col, ACell^.FormulaValue);
    cctRPNFormula: WriteRPNFormula(AStream, ACell^.Row, ACell^.Col, ACell^.RPNFormulaValue);
  end;
end;

{@@
  Helper function for the spreadsheet writers.

  Iterates all cells on a list, calling the appropriate write method for them.

  @param  AStream The output stream.
  @param  ACells  List of cells to be writeen
}
procedure TsCustomSpreadWriter.WriteCellsToStream(AStream: TStream; ACells: TAVLTree);
var
  AVLNode: TAVLTreeNode;
begin
  AVLNode := ACells.FindLowest;
  While Assigned(AVLNode) do
  begin
    WriteCellCallback(AVLNode.Data, Pointer(AStream));
    AVLNode := ACells.FindSuccessor(AVLNode);
  end;
end;

{@@
  Default file writting method.

  Opens the file and calls WriteToStream

  @param  AFileName The output file name.
                   If the file already exists it will be replaced.
  @param  AData     The Workbook to be saved.

  @see    TsWorkbook
}
procedure TsCustomSpreadWriter.WriteToFile(const AFileName: string;
  AData: TsWorkbook; const AOverwriteExisting: Boolean = False);
var
  OutputFile: TFileStream;
  lMode: Word;
begin
  if AOverwriteExisting then lMode := fmCreate or fmOpenWrite
  else lMode := fmCreate;

  OutputFile := TFileStream.Create(AFileName, lMode);
  try
    WriteToStream(OutputFile, AData);
  finally
    OutputFile.Free;
  end;
end;

{@@
  This routine should be overriden in descendent classes.
}
procedure TsCustomSpreadWriter.WriteToStream(AStream: TStream; AData: TsWorkbook);
begin
  raise Exception.Create(lpUnsupportedWriteFormat);

end;

procedure TsCustomSpreadWriter.WriteFormula(AStream: TStream; const ARow,
  ACol: Word; const AFormula: TsFormula);
begin

end;

procedure TsCustomSpreadWriter.WriteRPNFormula(AStream: TStream; const ARow,
  ACol: Word; const AFormula: TsRPNFormula);
begin

end;

finalization

  SetLength(GsSpreadFormats, 0);

end.

