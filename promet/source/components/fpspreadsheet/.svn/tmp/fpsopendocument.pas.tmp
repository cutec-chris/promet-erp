{
fpsopendocument.pas

Writes an OpenDocument 1.0 Spreadsheet document

An OpenDocument document is a compressed ZIP file with the following files inside:

content.xml     - Actual contents
meta.xml        - Authoring data
settings.xml    - User persistent viewing information, such as zoom, cursor position, etc.
styles.xml      - Styles, which are the only way to do formatting
mimetype        - application/vnd.oasis.opendocument.spreadsheet
META-INF\manifest.xml  - Describes the other files in the archive

Specifications obtained from:

http://docs.oasis-open.org/office/v1.1/OS/OpenDocument-v1.1.pdf

AUTHORS: Felipe Monteiro de Carvalho / Jose Luis Jurado Rincon
}
unit fpsopendocument;

{$ifdef fpc}
  {$mode delphi}
{$endif}

interface

uses
  Classes, SysUtils,
  fpszipper, {NOTE: fpszipper is the latest zipper.pp Change to standard zipper when FPC 2.8 is released}
  fpspreadsheet,
  xmlread, DOM, AVL_Tree,
  math,
  fpsutils;
  
type

  { TsSpreadOpenDocReader }

  TsSpreadOpenDocReader = class(TsCustomSpreadReader)
  private
    FWorksheet: TsWorksheet;
    function GetAttrValue(ANode : TDOMNode; AAttrName : string) : string;
  public
    { General reading methods }
    procedure ReadFromFile(AFileName: string; AData: TsWorkbook); override;
    { Record writing methods }
    procedure ReadFormula(ARow : Word; ACol : Word; ACellNode: TDOMNode);
    procedure ReadLabel(ARow : Word; ACol : Word; ACellNode: TDOMNode);
    procedure ReadNumber(ARow : Word; ACol : Word; ACellNode: TDOMNode);
  end;

  { TsSpreadOpenDocWriter }

  TsSpreadOpenDocWriter = class(TsCustomSpreadWriter)
  protected
    FPointSeparatorSettings: TFormatSettings;
    // Strings with the contents of files
    FMeta, FSettings, FStyles, FContent, FMimetype: string;
    FMetaInfManifest: string;
    // Streams with the contents of files
    FSMeta, FSSettings, FSStyles, FSContent, FSMimetype: TStringStream;
    FSMetaInfManifest: TStringStream;
    // Routines to write those files
    procedure WriteMimetype;
    procedure WriteMetaInfManifest;
    procedure WriteMeta;
    procedure WriteSettings;
    procedure WriteStyles;
    procedure WriteContent(AData: TsWorkbook);
    procedure WriteWorksheet(CurSheet: TsWorksheet);
    // Routines to write parts of those files
    function WriteStylesXMLAsString: string;
  public
    constructor Create; override;
    { General writing methods }
    procedure WriteStringToFile(AString, AFileName: string);
    procedure WriteToFile(const AFileName: string; AData: TsWorkbook;
      const AOverwriteExisting: Boolean = False); override;
    procedure WriteToStream(AStream: TStream; AData: TsWorkbook); override;
    { Record writing methods }
    procedure WriteFormula(AStream: TStream; const ARow, ACol: Word; const AFormula: TsFormula); override;
    procedure WriteLabel(AStream: TStream; const ARow, ACol: Word; const AValue: string; ACell: PCell); override;
    procedure WriteNumber(AStream: TStream; const ARow, ACol: Cardinal; const AValue: double; ACell: PCell); override;
  end;

implementation

const
  { OpenDocument general XML constants }
  XML_HEADER           = '<?xml version="1.0" encoding="utf-8" ?>';

  { OpenDocument Directory structure constants }
  OPENDOC_PATH_CONTENT   = 'content.xml';
  OPENDOC_PATH_META      = 'meta.xml';
  OPENDOC_PATH_SETTINGS  = 'settings.xml';
  OPENDOC_PATH_STYLES    = 'styles.xml';
  OPENDOC_PATH_MIMETYPE  = 'mimetype';
  OPENDOC_PATH_METAINF = 'META-INF' + '/';
  OPENDOC_PATH_METAINF_MANIFEST = 'META-INF' + '/' + 'manifest.xml';

  { OpenDocument schemas constants }
  SCHEMAS_XMLNS_OFFICE   = 'urn:oasis:names:tc:opendocument:xmlns:office:1.0';
  SCHEMAS_XMLNS_DCTERMS  = 'http://purl.org/dc/terms/';
  SCHEMAS_XMLNS_META     = 'urn:oasis:names:tc:opendocument:xmlns:meta:1.0';
  SCHEMAS_XMLNS          = 'http://schemas.openxmlformats.org/officeDocument/2006/extended-properties';
  SCHEMAS_XMLNS_CONFIG   = 'urn:oasis:names:tc:opendocument:xmlns:config:1.0';
  SCHEMAS_XMLNS_OOO      = 'http://openoffice.org/2004/office';
  SCHEMAS_XMLNS_MANIFEST = 'urn:oasis:names:tc:opendocument:xmlns:manifest:1.0';
  SCHEMAS_XMLNS_FO       = 'urn:oasis:names:tc:opendocument:xmlns:xsl-fo-compatible:1.0';
  SCHEMAS_XMLNS_STYLE    = 'urn:oasis:names:tc:opendocument:xmlns:style:1.0';
  SCHEMAS_XMLNS_SVG      = 'urn:oasis:names:tc:opendocument:xmlns:svg-compatible:1.0';
  SCHEMAS_XMLNS_TABLE    = 'urn:oasis:names:tc:opendocument:xmlns:table:1.0';
  SCHEMAS_XMLNS_TEXT     = 'urn:oasis:names:tc:opendocument:xmlns:text:1.0';
  SCHEMAS_XMLNS_V        = 'urn:schemas-microsoft-com:vml';
  SCHEMAS_XMLNS_NUMBER   = 'urn:oasis:names:tc:opendocument:xmlns:datastyle:1.0';
  SCHEMAS_XMLNS_CHART    = 'urn:oasis:names:tc:opendocument:xmlns:chart:1.0';
  SCHEMAS_XMLNS_DR3D     = 'urn:oasis:names:tc:opendocument:xmlns:dr3d:1.0';
  SCHEMAS_XMLNS_MATH     = 'http://www.w3.org/1998/Math/MathML';
  SCHEMAS_XMLNS_FORM     = 'urn:oasis:names:tc:opendocument:xmlns:form:1.0';
  SCHEMAS_XMLNS_SCRIPT   = 'urn:oasis:names:tc:opendocument:xmlns:script:1.0';
  SCHEMAS_XMLNS_OOOW     = 'http://openoffice.org/2004/writer';
  SCHEMAS_XMLNS_OOOC     = 'http://openoffice.org/2004/calc';
  SCHEMAS_XMLNS_DOM      = 'http://www.w3.org/2001/xml-events';
  SCHEMAS_XMLNS_XFORMS   = 'http://www.w3.org/2002/xforms';
  SCHEMAS_XMLNS_XSD      = 'http://www.w3.org/2001/XMLSchema';
  SCHEMAS_XMLNS_XSI      = 'http://www.w3.org/2001/XMLSchema-instance';

{ TsSpreadOpenDocReader }

function TsSpreadOpenDocReader.GetAttrValue(ANode : TDOMNode; AAttrName : string) : string;
var
  i : integer;
  Found : Boolean;
begin
  Found:=false;
  i:=0;
  Result:='';
  while not Found and (i<ANode.Attributes.Length) do begin
    if ANode.Attributes.Item[i].NodeName=AAttrName then begin
      Found:=true;
      Result:=ANode.Attributes.Item[i].NodeValue;
    end;
    inc(i);
  end;
end;

procedure TsSpreadOpenDocReader.ReadFromFile(AFileName: string; AData: TsWorkbook);
var
  Col, Row : integer;
  FilePath : string;
  UnZip : TUnZipper;
  FileList : TStringList;
  Doc : TXMLDocument;
  BodyNode, SpreadSheetNode, TableNode, RowNode, CellNode : TDOMNode;
  ParamRowsRepeated, ParamColsRepeated, ParamValueType, ParamFormula : string;
  RowsCount, ColsCount : integer;
begin
  //unzip content.xml into AFileName path
  FilePath:=GetTempDir(false);
  UnZip:=TUnZipper.Create;
  UnZip.OutputPath:=FilePath;
  FileList:=TStringList.Create;
  FileList.Add('content.xml');
  try
    Unzip.UnZipFiles(AFileName,FileList);
  finally
    FreeAndNil(FileList);
    FreeAndNil(UnZip);
  end; //try

  Doc:=nil;
  try
    //process the xml file
    ReadXMLFile(Doc,FilePath+'content.xml');
    DeleteFile(FilePath+'content.xml');

    BodyNode:= Doc.DocumentElement.FindNode('office:body');
    if not Assigned(BodyNode) then Exit;

    SpreadSheetNode:=BodyNode.FindNode('office:spreadsheet');
    if not Assigned(SpreadSheetNode) then Exit;

    //process each table (sheet)
    TableNode:=SpreadSheetNode.FindNode('table:table');
    while Assigned(TableNode) do begin
      FWorkSheet:=aData.AddWorksheet(GetAttrValue(TableNode,'table:name'));
      Row:=0;

      //process each row inside the sheet
      RowNode:=TableNode.FindNode('table:table-row');
      while Assigned(RowNode) do begin

        Col:=0;

        ParamRowsRepeated:=GetAttrValue(RowNode,'table:number-rows-repeated');
        if ParamRowsRepeated='' then ParamRowsRepeated:='1';

        //process each cell of the row
        CellNode:=RowNode.FindNode('table:table-cell');
        while Assigned(CellNode) do
        begin
          ParamColsRepeated:=GetAttrValue(CellNode,'table:number-columns-repeated');
          if ParamColsRepeated='' then ParamColsRepeated:='1';

          //select this cell value's type
          ParamValueType:=GetAttrValue(CellNode,'office:value-type');
          ParamFormula:=GetAttrValue(CellNode,'table:formula');
          for RowsCount:=0 to StrToInt(ParamRowsRepeated)-1 do begin
            for ColsCount:=0 to StrToInt(ParamColsRepeated)-1 do begin
              if ParamValueType='string' then
                ReadLabel(Row+RowsCount,Col+ColsCount,CellNode)
              else if ParamFormula<>'' then
                ReadFormula(Row+RowsCount,Col+ColsCount,CellNode)
              else if ParamValueType='float' then
                ReadNumber(Row+RowsCount,Col+ColsCount,CellNode);
            end; //for ColsCount
          end; //for RowsCount

          Inc(Col,ColsCount+1);
          CellNode:=CellNode.NextSibling;
        end; //while Assigned(CellNode)

        Inc(Row,RowsCount+1);
        RowNode:=RowNode.NextSibling;
      end; // while Assigned(RowNode)

      TableNode:=TableNode.NextSibling;
    end; //while Assigned(TableNode)
  finally
    Doc.Free;
  end;
end;

procedure TsSpreadOpenDocReader.ReadFormula(ARow: Word; ACol : Word; ACellNode : TDOMNode);
begin
  // For now just read the number
  ReadNumber(ARow, ACol, ACellNode);
end;

procedure TsSpreadOpenDocReader.ReadLabel(ARow: Word; ACol : Word; ACellNode : TDOMNode);
begin
  FWorkSheet.WriteUTF8Text(ARow,ACol,UTF8Encode(ACellNode.TextContent));
end;

procedure TsSpreadOpenDocReader.ReadNumber(ARow: Word; ACol : Word; ACellNode : TDOMNode);
var
  FSettings: TFormatSettings;
  Value, Str: String;
  lNumber: Double;
begin
  FSettings.DecimalSeparator:='.';
  Value:=GetAttrValue(ACellNode,'office:value');
  if UpperCase(Value)='1.#INF' then
  begin
    FWorkSheet.WriteNumber(Arow,ACol,1.0/0.0);
  end
  else
  begin
    // Don't merge, or else we can't debug
    Str := GetAttrValue(ACellNode,'office:value');
    lNumber := StrToFloat(Str,FSettings);
    FWorkSheet.WriteNumber(Arow,ACol,lNumber);
  end;
end;

{ TsSpreadOpenDocWriter }

procedure TsSpreadOpenDocWriter.WriteMimetype;
begin
  FMimetype := 'application/vnd.oasis.opendocument.spreadsheet';
end;

procedure TsSpreadOpenDocWriter.WriteMetaInfManifest;
begin
  FMetaInfManifest :=
   XML_HEADER + LineEnding +
   '<manifest:manifest xmlns:manifest="' + SCHEMAS_XMLNS_MANIFEST + '">' + LineEnding +
   '  <manifest:file-entry manifest:media-type="application/vnd.oasis.opendocument.spreadsheet" manifest:full-path="/" />' + LineEnding +
   '  <manifest:file-entry manifest:media-type="text/xml" manifest:full-path="content.xml" />' + LineEnding +
   '  <manifest:file-entry manifest:media-type="text/xml" manifest:full-path="styles.xml" />' + LineEnding +
   '  <manifest:file-entry manifest:media-type="text/xml" manifest:full-path="meta.xml" />' + LineEnding +
   '  <manifest:file-entry manifest:media-type="text/xml" manifest:full-path="settings.xml" />' + LineEnding +
   '</manifest:manifest>';
end;

procedure TsSpreadOpenDocWriter.WriteMeta;
begin
  FMeta :=
   XML_HEADER + LineEnding +
   '<office:document-meta xmlns:office="' + SCHEMAS_XMLNS_OFFICE +
     '" xmlns:dcterms="' + SCHEMAS_XMLNS_DCTERMS +
     '" xmlns:meta="' + SCHEMAS_XMLNS_META +
     '" xmlns="' + SCHEMAS_XMLNS +
     '" xmlns:ex="' + SCHEMAS_XMLNS + '">' + LineEnding +
   '  <office:meta>' + LineEnding +
   '    <meta:generator>FPSpreadsheet Library</meta:generator>' + LineEnding +
   '    <meta:document-statistic />' + LineEnding +
   '  </office:meta>' + LineEnding +
   '</office:document-meta>';
end;

procedure TsSpreadOpenDocWriter.WriteSettings;
begin
  FSettings :=
   XML_HEADER + LineEnding +
   '<office:document-settings xmlns:office="' + SCHEMAS_XMLNS_OFFICE +
     '" xmlns:config="' + SCHEMAS_XMLNS_CONFIG +
     '" xmlns:ooo="' + SCHEMAS_XMLNS_OOO + '">' + LineEnding +
   '<office:settings>' + LineEnding +
   '  <config:config-item-set config:name="ooo:view-settings">' + LineEnding +
   '    <config:config-item-map-indexed config:name="Views">' + LineEnding +
   '      <config:config-item-map-entry>' + LineEnding +
   '        <config:config-item config:name="ActiveTable" config:type="string">Tabelle1</config:config-item>' + LineEnding +
   '        <config:config-item config:name="ZoomValue" config:type="int">100</config:config-item>' + LineEnding +
   '        <config:config-item config:name="PageViewZoomValue" config:type="int">100</config:config-item>' + LineEnding +
   '        <config:config-item config:name="ShowPageBreakPreview" config:type="boolean">false</config:config-item>' + LineEnding +
   '        <config:config-item config:name="HasColumnRowHeaders" config:type="boolean">true</config:config-item>' + LineEnding +
   '          <config:config-item-map-named config:name="Tables">' + LineEnding +
   '            <config:config-item-map-entry config:name="Tabelle1">' + LineEnding +
   '              <config:config-item config:name="CursorPositionX" config:type="int">3</config:config-item>' + LineEnding +
   '              <config:config-item config:name="CursorPositionY" config:type="int">2</config:config-item>' + LineEnding +
   '            </config:config-item-map-entry>' + LineEnding +
   '          </config:config-item-map-named>' + LineEnding +
   '        </config:config-item-map-entry>' + LineEnding +
   '      </config:config-item-map-indexed>' + LineEnding +
   '    </config:config-item-set>' + LineEnding +
   '  </office:settings>' + LineEnding +
   '</office:document-settings>';
end;

procedure TsSpreadOpenDocWriter.WriteStyles;
begin
  FStyles :=
   XML_HEADER + LineEnding +
   '<office:document-styles xmlns:office="' + SCHEMAS_XMLNS_OFFICE +
     '" xmlns:fo="' + SCHEMAS_XMLNS_FO +
     '" xmlns:style="' + SCHEMAS_XMLNS_STYLE +
     '" xmlns:svg="' + SCHEMAS_XMLNS_SVG +
     '" xmlns:table="' + SCHEMAS_XMLNS_TABLE +
     '" xmlns:text="' + SCHEMAS_XMLNS_TEXT +
     '" xmlns:v="' + SCHEMAS_XMLNS_V + '">' + LineEnding +
   '<office:font-face-decls>' + LineEnding +
   '  <style:font-face style:name="Arial" svg:font-family="Arial" />' + LineEnding +
   '</office:font-face-decls>' + LineEnding +
   '<office:styles>' + LineEnding +
   '  <style:style style:name="Default" style:family="table-cell">' + LineEnding +
   '    <style:text-properties fo:font-size="10" style:font-name="Arial" />' + LineEnding +
   '  </style:style>' + LineEnding +
   '</office:styles>' + LineEnding +
   '<office:automatic-styles>' + LineEnding +
   '  <style:page-layout style:name="pm1">' + LineEnding +
   '    <style:page-layout-properties fo:margin-top="1.25cm" fo:margin-bottom="1.25cm" fo:margin-left="1.905cm" fo:margin-right="1.905cm" />' + LineEnding +
   '    <style:header-style>' + LineEnding +
   '    <style:header-footer-properties fo:min-height="0.751cm" fo:margin-left="0cm" fo:margin-right="0cm" fo:margin-bottom="0.25cm" fo:margin-top="0cm" />' + LineEnding +
   '    </style:header-style>' + LineEnding +
   '    <style:footer-style>' + LineEnding +
   '    <style:header-footer-properties fo:min-height="0.751cm" fo:margin-left="0cm" fo:margin-right="0cm" fo:margin-top="0.25cm" fo:margin-bottom="0cm" />' + LineEnding +
   '    </style:footer-style>' + LineEnding +
   '  </style:page-layout>' + LineEnding +
   '</office:automatic-styles>' + LineEnding +
   '<office:master-styles>' + LineEnding +
   '  <style:master-page style:name="Default" style:page-layout-name="pm1">' + LineEnding +
   '    <style:header />' + LineEnding +
   '    <style:header-left style:display="false" />' + LineEnding +
   '    <style:footer />' + LineEnding +
   '    <style:footer-left style:display="false" />' + LineEnding +
   '  </style:master-page>' + LineEnding +
   '</office:master-styles>' + LineEnding +
   '</office:document-styles>';
end;

procedure TsSpreadOpenDocWriter.WriteContent(AData: TsWorkbook);
var
  i: Integer;
  lStylesCode: string;
begin
  ListAllFormattingStyles(AData);

  lStylesCode := WriteStylesXMLAsString();

  FContent :=
   XML_HEADER + LineEnding +
   '<office:document-content xmlns:office="' + SCHEMAS_XMLNS_OFFICE +
     '" xmlns:fo="'     + SCHEMAS_XMLNS_FO +
     '" xmlns:style="'  + SCHEMAS_XMLNS_STYLE +
     '" xmlns:text="'   + SCHEMAS_XMLNS_TEXT +
     '" xmlns:table="'  + SCHEMAS_XMLNS_TABLE +
     '" xmlns:svg="'    + SCHEMAS_XMLNS_SVG +
     '" xmlns:number="' + SCHEMAS_XMLNS_NUMBER +
     '" xmlns:meta="'   + SCHEMAS_XMLNS_META +
     '" xmlns:chart="'  + SCHEMAS_XMLNS_CHART +
     '" xmlns:dr3d="'   + SCHEMAS_XMLNS_DR3D +
     '" xmlns:math="'   + SCHEMAS_XMLNS_MATH +
     '" xmlns:form="'   + SCHEMAS_XMLNS_FORM +
     '" xmlns:script="' + SCHEMAS_XMLNS_SCRIPT +
     '" xmlns:ooo="'    + SCHEMAS_XMLNS_OOO +
     '" xmlns:ooow="'   + SCHEMAS_XMLNS_OOOW +
     '" xmlns:oooc="'   + SCHEMAS_XMLNS_OOOC +
     '" xmlns:dom="'    + SCHEMAS_XMLNS_DOM +
     '" xmlns:xforms="' + SCHEMAS_XMLNS_XFORMS +
     '" xmlns:xsd="'    + SCHEMAS_XMLNS_XSD +
     '" xmlns:xsi="'    + SCHEMAS_XMLNS_XSI + '">' + LineEnding +
   '  <office:scripts />' + LineEnding +

   // Fonts
   '  <office:font-face-decls>' + LineEnding +
   '    <style:font-face style:name="Arial" svg:font-family="Arial" xmlns:v="urn:schemas-microsoft-com:vml" />' + LineEnding +
   '  </office:font-face-decls>' + LineEnding +

   // Automatic styles
  '  <office:automatic-styles>' + LineEnding +
  '    <style:style style:name="co1" style:family="table-column">' + LineEnding +
  '      <style:table-column-properties fo:break-before="auto" style:column-width="2.267cm"/>' + LineEnding +
  '    </style:style>' + LineEnding +
  '    <style:style style:name="ro1" style:family="table-row">' + LineEnding +
  '      <style:table-row-properties style:row-height="0.416cm" fo:break-before="auto" style:use-optimal-row-height="true"/>' + LineEnding +
  '    </style:style>' + LineEnding +
  '    <style:style style:name="ta1" style:family="table" style:master-page-name="Default">' + LineEnding +
  '      <style:table-properties table:display="true" style:writing-mode="lr-tb"/>' + LineEnding +
  '    </style:style>' + LineEnding +
  // Automatically Generated Styles
  lStylesCode +
  '  </office:automatic-styles>' + LineEnding +

  // Body
  '  <office:body>' + LineEnding +
  '    <office:spreadsheet>' + LineEnding;

  // Write all worksheets
  for i := 0 to AData.GetWorksheetCount - 1 do
  begin
    WriteWorksheet(Adata.GetWorksheetByIndex(i));
  end;

  FContent :=  FContent +
   '    </office:spreadsheet>' + LineEnding +
   '  </office:body>' + LineEnding +
   '</office:document-content>';
end;

procedure TsSpreadOpenDocWriter.WriteWorksheet(CurSheet: TsWorksheet);
var
  j, k: Integer;
  CurCell: PCell;
  CurRow: array of PCell;
  LastColNum: Cardinal;
  LCell: TCell;
  AVLNode: TAVLTreeNode;
begin
  LastColNum := CurSheet.GetLastColNumber;

  // Header
  FContent := FContent +
  '    <table:table table:name="' + CurSheet.Name + '" table:style-name="ta1">' + LineEnding +
  '      <table:table-column table:style-name="co1" table:number-columns-repeated="' +
  IntToStr(LastColNum + 1) + '" table:default-cell-style-name="Default"/>' + LineEnding;

  // The cells need to be written in order, row by row, cell by cell
  for j := 0 to CurSheet.GetLastRowNumber do
  begin
    FContent := FContent +
    '      <table:table-row table:style-name="ro1">' + LineEnding;

    // Write cells from this row.
    for k := 0 to LastColNum do
    begin
      LCell.Row := j;
      LCell.Col := k;
      AVLNode := CurSheet.Cells.Find(@LCell);
      if Assigned(AVLNode) then
        WriteCellCallback(PCell(AVLNode.Data), nil)
      else
        FContent := FContent + '<table:table-cell/>' + LineEnding;
    end;

    FContent := FContent +
    '      </table:table-row>' + LineEnding;
  end;

  // Footer
  FContent := FContent +
  '    </table:table>' + LineEnding;
end;

function TsSpreadOpenDocWriter.WriteStylesXMLAsString: string;
var
  i: Integer;
begin
  Result := '';

  for i := 0 to Length(FFormattingStyles) - 1 do
  begin
    // Start and Name
    Result := Result +
    '    <style:style style:name="ce' + IntToStr(i) + '" style:family="table-cell" style:parent-style-name="Default">' + LineEnding;

    // Fields
    if uffBold in FFormattingStyles[i].UsedFormattingFields then
      Result := Result +
    '      <style:text-properties fo:font-weight="bold" style:font-weight-asian="bold" style:font-weight-complex="bold"/>' + LineEnding;

    if (uffBorder in FFormattingStyles[i].UsedFormattingFields) or
     (uffBackgroundColor in FFormattingStyles[i].UsedFormattingFields) then
    begin
      Result := Result + '      <style:table-cell-properties ';

      if (uffBorder in FFormattingStyles[i].UsedFormattingFields) then
      begin
        if cbSouth in FFormattingStyles[i].Border then Result := Result + 'fo:border-bottom="0.002cm solid #000000" '
        else Result := Result + 'fo:border-bottom="none" ';

        if cbWest in FFormattingStyles[i].Border then Result := Result + 'fo:border-left="0.002cm solid #000000" '
        else Result := Result + 'fo:border-left="none" ';

        if cbEast in FFormattingStyles[i].Border then Result := Result + 'fo:border-right="0.002cm solid #000000" '
        else Result := Result + 'fo:border-right="none" ';

        if cbNorth in FFormattingStyles[i].Border then Result := Result + 'fo:border-top="0.002cm solid #000000" '
        else Result := Result + 'fo:border-top="none" ';
      end;

      if (uffBackgroundColor in FFormattingStyles[i].UsedFormattingFields) then
      begin
        Result := Result + 'fo:background-color="#'
          + FPSColorToHexString(FFormattingStyles[i].BackgroundColor) +'" ';
      end;

      Result := Result + '/>' + LineEnding;
    end;

    // End
    Result := Result +
    '    </style:style>' + LineEnding;
  end;
end;

constructor TsSpreadOpenDocWriter.Create;
begin
  inherited Create;

  FPointSeparatorSettings := SysUtils.DefaultFormatSettings;
  FPointSeparatorSettings.DecimalSeparator:='.';
end;

{
  Writes a string to a file. Helper convenience method.
}
procedure TsSpreadOpenDocWriter.WriteStringToFile(AString, AFileName: string);
var
  TheStream : TFileStream;
  S : String;
begin
  TheStream := TFileStream.Create(AFileName, fmCreate);
  S:=AString;
  TheStream.WriteBuffer(Pointer(S)^,Length(S));
  TheStream.Free;
end;

{
  Writes an OOXML document to the disc.
}
procedure TsSpreadOpenDocWriter.WriteToFile(const AFileName: string;
  AData: TsWorkbook; const AOverwriteExisting: Boolean);
var
  FZip: TZipper;
begin
  { Fill the strings with the contents of the files }

  WriteMimetype();
  WriteMetaInfManifest();
  WriteMeta();
  WriteSettings();
  WriteStyles();
  WriteContent(AData);

  { Write the data to streams }

  FSMeta := TStringStream.Create(FMeta);
  FSSettings := TStringStream.Create(FSettings);
  FSStyles := TStringStream.Create(FStyles);
  FSContent := TStringStream.Create(FContent);
  FSMimetype := TStringStream.Create(FMimetype);
  FSMetaInfManifest := TStringStream.Create(FMetaInfManifest);

  { Now compress the files }

  FZip := TZipper.Create;
  try
    FZip.FileName := AFileName;

    FZip.Entries.AddFileEntry(FSMeta, OPENDOC_PATH_META);
    FZip.Entries.AddFileEntry(FSSettings, OPENDOC_PATH_SETTINGS);
    FZip.Entries.AddFileEntry(FSStyles, OPENDOC_PATH_STYLES);
    FZip.Entries.AddFileEntry(FSContent, OPENDOC_PATH_CONTENT);
    FZip.Entries.AddFileEntry(FSMimetype, OPENDOC_PATH_MIMETYPE);
    FZip.Entries.AddFileEntry(FSMetaInfManifest, OPENDOC_PATH_METAINF_MANIFEST);

    FZip.ZipAllFiles;
  finally
    FZip.Free;
    FSMeta.Free;
    FSSettings.Free;
    FSStyles.Free;
    FSContent.Free;
    FSMimetype.Free;
    FSMetaInfManifest.Free;
  end;
end;


procedure TsSpreadOpenDocWriter.WriteToStream(AStream: TStream; AData: TsWorkbook);
begin
  // Not supported at the moment
  raise Exception.Create('TsSpreadOpenDocWriter.WriteToStream not supported');
end;

procedure TsSpreadOpenDocWriter.WriteFormula(AStream: TStream; const ARow,
  ACol: Word; const AFormula: TsFormula);
begin
{  // The row should already be the correct one
  FContent := FContent +
    '  <table:table-cell office:value-type="string">' + LineEnding +
    '    <text:p>' + AFormula.DoubleValue + '</text:p>' + LineEnding +
    '  </table:table-cell>' + LineEnding;
<table:table-cell table:formula="of:=[.A1]+[.B2]" office:value-type="float" office:value="1833">
<text:p>1833</text:p>
</table:table-cell>}
end;

{
  Writes a cell with text content

  The UTF8 Text needs to be converted, because some chars are invalid in XML
  See bug with patch 19422
}
procedure TsSpreadOpenDocWriter.WriteLabel(AStream: TStream; const ARow,
  ACol: Word; const AValue: string; ACell: PCell);
var
  lStyle: string = '';
  lIndex: Integer;
begin
  if ACell^.UsedFormattingFields <> [] then
  begin
    lIndex := FindFormattingInList(ACell);
    lStyle := ' table:style-name="ce' + IntToStr(lIndex) + '" ';
  end;

  // The row should already be the correct one
  FContent := FContent +
    '  <table:table-cell office:value-type="string"' + lStyle + '>' + LineEnding +
    '    <text:p>' + UTF8TextToXMLText(AValue) + '</text:p>' + LineEnding +
    '  </table:table-cell>' + LineEnding;
end;

procedure TsSpreadOpenDocWriter.WriteNumber(AStream: TStream; const ARow,
  ACol: Cardinal; const AValue: double; ACell: PCell);
var
  StrValue: string;
  DisplayStr: string;
  lStyle: string = '';
begin
  if uffBold in ACell^.UsedFormattingFields then
    lStyle := ' table:style-name="bold" ';

  // The row should already be the correct one
  if IsInfinite(AValue) then begin
    StrValue:='1.#INF';
    DisplayStr:='1.#INF';
  end else begin
    StrValue:=FloatToStr(AValue,FPointSeparatorSettings); //Uses '.' as decimal separator
    DisplayStr:=FloatToStr(AValue); // Uses locale decimal separator
  end;
  FContent := FContent +
    '  <table:table-cell office:value-type="float" office:value="' + StrValue + '"' + lStyle + '>' + LineEnding +
    '    <text:p>' + DisplayStr + '</text:p>' + LineEnding +
    '  </table:table-cell>' + LineEnding;
end;

{
  Registers this reader / writer on fpSpreadsheet
}
initialization

  RegisterSpreadFormat(TsSpreadOpenDocReader, TsSpreadOpenDocWriter, sfOpenDocument);

end.

