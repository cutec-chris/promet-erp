unit uspreetsheet;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, Menus, ExtCtrls, ComCtrls, ActnList, Spin, Grids,
  ColorBox, ValEdit,uPrometFrames,
  fpstypes, fpspalette, fpspreadsheetgrid, fpspreadsheet,
  {%H-}fpsallformats;

type

  { TfSpreetsheet }

  TfSpreetsheet = class(TPrometMainFrame)
    AcOpen: TAction;
    AcSaveAs: TAction;
    AcQuit: TAction;
    AcEdit: TAction;
    AcLeftAlign: TAction;
    AcHorCenterAlign: TAction;
    AcRightAlign: TAction;
    AcHorDefaultAlign: TAction;
    AcFontBold: TAction;
    AcFontItalic: TAction;
    AcFontStrikeout: TAction;
    AcFontUnderline: TAction;
    AcFont: TAction;
    AcBorderTop: TAction;
    AcBorderBottom: TAction;
    AcBorderBottomDbl: TAction;
    AcBorderBottomMedium: TAction;
    AcBorderLeft: TAction;
    AcBorderRight: TAction;
    AcBorderNone: TAction;
    AcBorderHCenter: TAction;
    AcBorderVCenter: TAction;
    AcBorderTopBottom: TAction;
    AcBorderTopBottomThick: TAction;
    AcBorderInner: TAction;
    AcBorderAll: TAction;
    AcBorderOuter: TAction;
    AcBorderOuterMedium: TAction;
    AcTextHoriz: TAction;
    AcTextVertCW: TAction;
    AcTextVertCCW: TAction;
    AcTextStacked: TAction;
    AcNFFixed: TAction;
    AcNFFixedTh: TAction;
    AcNFPercentage: TAction;
    AcIncDecimals: TAction;
    AcDecDecimals: TAction;
    AcNFGeneral: TAction;
    AcNFExp: TAction;
    AcCopyFormat: TAction;
    AcNFCurrency: TAction;
    AcNFCurrencyRed: TAction;
    AcNFShortDateTime: TAction;
    AcNFShortDate: TAction;
    AcNFLongDate: TAction;
    AcNFShortTime: TAction;
    AcNFLongTime: TAction;
    AcNFShortTimeAM: TAction;
    AcNFLongTimeAM: TAction;
    AcNFTimeInterval: TAction;
    AcNFDayMonth: TAction;
    AcNFMonthDay: TAction;
    AcNFCusstomMS: TAction;
    AcNFCustomMSZ: TAction;
    AcNew: TAction;
    AcAddColumn: TAction;
    AcAddRow: TAction;
    AcMergeCells: TAction;
    AcShowHeaders: TAction;
    AcShowGridlines: TAction;
    AcDeleteColumn: TAction;
    AcDeleteRow: TAction;
    AcCSVParams: TAction;
    AcFormatSettings: TAction;
    AcSortColAsc: TAction;
    AcSort: TAction;
    AcCurrencySymbols: TAction;
    AcCommentAdd: TAction;
    AcCommentDelete: TAction;
    AcCommentEdit: TAction;
    AcViewInspector: TAction;
    AcWordwrap: TAction;
    AcVAlignDefault: TAction;
    AcVAlignTop: TAction;
    AcVAlignCenter: TAction;
    AcVAlignBottom: TAction;
    ActionList: TActionList;
    CbBackgroundColor: TColorBox;
    CbReadFormulas: TCheckBox;
    CbHeaderStyle: TComboBox;
    CbAutoCalcFormulas: TCheckBox;
    CbTextOverflow: TCheckBox;
    EdCellAddress: TEdit;
    FontComboBox: TComboBox;
    EdFrozenRows: TSpinEdit;
    FontDialog: TFontDialog;
    FontSizeComboBox: TComboBox;
    ImageList: TImageList;
    Label1: TLabel;
    Label2: TLabel;
    FormulaMemo: TMemo;
    MenuItem1: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem13: TMenuItem;
    MenuItem14: TMenuItem;
    MenuItem15: TMenuItem;
    MenuItem16: TMenuItem;
    MenuItem17: TMenuItem;
    MenuItem18: TMenuItem;
    MenuItem19: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem20: TMenuItem;
    MenuItem21: TMenuItem;
    MenuItem22: TMenuItem;
    MenuItem23: TMenuItem;
    MenuItem24: TMenuItem;
    MenuItem25: TMenuItem;
    MenuItem26: TMenuItem;
    MenuItem27: TMenuItem;
    MenuItem28: TMenuItem;
    MenuItem29: TMenuItem;
    MenuItem30: TMenuItem;
    MenuItem31: TMenuItem;
    MenuItem32: TMenuItem;
    MenuItem33: TMenuItem;
    MenuItem34: TMenuItem;
    MenuItem35: TMenuItem;
    MenuItem36: TMenuItem;
    MenuItem37: TMenuItem;
    MenuItem38: TMenuItem;
    MenuItem39: TMenuItem;
    MenuItem40: TMenuItem;
    MenuItem41: TMenuItem;
    MenuItem42: TMenuItem;
    MenuItem43: TMenuItem;
    MenuItem44: TMenuItem;
    MenuItem45: TMenuItem;
    MenuItem46: TMenuItem;
    MenuItem47: TMenuItem;
    MenuItem48: TMenuItem;
    MenuItem49: TMenuItem;
    MenuItem50: TMenuItem;
    MenuItem51: TMenuItem;
    MenuItem52: TMenuItem;
    MenuItem53: TMenuItem;
    MenuItem54: TMenuItem;
    MenuItem55: TMenuItem;
    MenuItem56: TMenuItem;
    MenuItem57: TMenuItem;
    MenuItem58: TMenuItem;
    MenuItem59: TMenuItem;
    MenuItem60: TMenuItem;
    MenuItem61: TMenuItem;
    MenuItem62: TMenuItem;
    MenuItem63: TMenuItem;
    MenuItem64: TMenuItem;
    MenuItem65: TMenuItem;
    MenuItem66: TMenuItem;
    MenuItem67: TMenuItem;
    MenuItem68: TMenuItem;
    MenuItem69: TMenuItem;
    MenuItem70: TMenuItem;
    MenuItem71: TMenuItem;
    MenuItem72: TMenuItem;
    MenuItem73: TMenuItem;
    MenuItem74: TMenuItem;
    MnuNumberFormatSettings: TMenuItem;
    MenuItem76: TMenuItem;
    MenuItem77: TMenuItem;
    MenuItem78: TMenuItem;
    MenuItem79: TMenuItem;
    MnuCurrencySymbol: TMenuItem;
    MnuCSVParams: TMenuItem;
    MnuSettings: TMenuItem;
    mnuInspector: TMenuItem;
    mnuView: TMenuItem;
    MnuFmtDateTimeMSZ: TMenuItem;
    MnuTimeInterval: TMenuItem;
    MnuShortTimeAM: TMenuItem;
    MnuLongTimeAM: TMenuItem;
    MnuFmtDateTimeMY: TMenuItem;
    MnuFmtDateTimeDM: TMenuItem;
    MnuShortTime: TMenuItem;
    MnuShortDate: TMenuItem;
    MnuLongTime: TMenuItem;
    MnuLongDate: TMenuItem;
    MnuShortDateTime: TMenuItem;
    MnuCurrencyRed: TMenuItem;
    MnuCurrency: TMenuItem;
    MnuNumberFormat: TMenuItem;
    MnuNFFixed: TMenuItem;
    MnuNFFixedTh: TMenuItem;
    MnuNFPercentage: TMenuItem;
    MnuNFExp: TMenuItem;
    MnuNFGeneral: TMenuItem;
    MnuTextRotation: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    MnuWordwrap: TMenuItem;
    MnuVertBottom: TMenuItem;
    MnuVertCentered: TMenuItem;
    MnuVertTop: TMenuItem;
    MnuVertDefault: TMenuItem;
    MnuVertAlignment: TMenuItem;
    MnuFOnt: TMenuItem;
    MnuHorDefault: TMenuItem;
    MnuHorAlignment: TMenuItem;
    mnuFormat: TMenuItem;
    mnuEdit: TMenuItem;
    mnuFile: TMenuItem;
    mnuOpen: TMenuItem;
    mnuQuit: TMenuItem;
    mnuSaveAs: TMenuItem;
    OpenDialog: TOpenDialog;
    InspectorPageControl: TPageControl;
    Panel1: TPanel;
    BordersPopupMenu: TPopupMenu;
    NumFormatPopupMenu: TPopupMenu;
    AddressPanel: TPanel;
    SaveDialog: TSaveDialog;
    EdFrozenCols: TSpinEdit;
    FormulaToolBar: TToolBar;
    FormulaToolbarSplitter: TSplitter;
    InspectorSplitter: TSplitter;
    PgCellValue: TTabSheet;
    PgProperties: TTabSheet;
    Splitter1: TSplitter;
    TabControl: TTabControl;
    PgSheet: TTabSheet;
    ToolButton22: TToolButton;
    ToolButton23: TToolButton;
    ToolButton27: TToolButton;
    CellInspector: TValueListEditor;
    ToolButton28: TToolButton;
    ToolButton29: TToolButton;
    ToolButton30: TToolButton;
    ToolButton31: TToolButton;
    ToolButton32: TToolButton;
    ToolButton33: TToolButton;
    WorksheetGrid: TsWorksheetGrid;
    ToolBar1: TToolBar;
    FormatToolBar: TToolBar;
    ToolButton1: TToolButton;
    ToolButton10: TToolButton;
    ToolButton11: TToolButton;
    ToolButton12: TToolButton;
    ToolButton13: TToolButton;
    ToolButton14: TToolButton;
    ToolButton15: TToolButton;
    ToolButton16: TToolButton;
    ToolButton17: TToolButton;
    ToolButton18: TToolButton;
    ToolButton19: TToolButton;
    ToolButton2: TToolButton;
    TbBorders: TToolButton;
    TbNumFormats: TToolButton;
    ToolButton20: TToolButton;
    ToolButton21: TToolButton;
    ToolButton24: TToolButton;
    ToolButton25: TToolButton;
    ToolButton26: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
    procedure AcAddColumnExecute(Sender: TObject);
    procedure AcAddRowExecute(Sender: TObject);
    procedure AcBorderExecute(Sender: TObject);
    procedure AcCommentAddExecute(Sender: TObject);
    procedure AcCommentDeleteExecute(Sender: TObject);
    procedure AcCopyFormatExecute(Sender: TObject);
    procedure AcDeleteColumnExecute(Sender: TObject);
    procedure AcDeleteRowExecute(Sender: TObject);
    procedure AcFontExecute(Sender: TObject);
    procedure AcFontStyleExecute(Sender: TObject);
    procedure AcFormatSettingsExecute(Sender: TObject);
    procedure AcHorAlignmentExecute(Sender: TObject);
    procedure AcIncDecDecimalsExecute(Sender: TObject);
    procedure AcMergeCellsExecute(Sender: TObject);
    procedure AcNewExecute(Sender: TObject);
    procedure AcNumFormatExecute(Sender: TObject);
    procedure AcOpenExecute(Sender: TObject);
    procedure AcQuitExecute(Sender: TObject);
    procedure AcSaveAsExecute(Sender: TObject);
    procedure AcShowGridlinesExecute(Sender: TObject);
    procedure AcShowHeadersExecute(Sender: TObject);
    procedure AcSortColAscExecute(Sender: TObject);
    procedure AcSortExecute(Sender: TObject);
    procedure AcTextRotationExecute(Sender: TObject);
    procedure AcVertAlignmentExecute(Sender: TObject);
    procedure AcViewInspectorExecute(Sender: TObject);
    procedure AcWordwrapExecute(Sender: TObject);
    procedure CbBackgroundColorSelect(Sender: TObject);
    procedure CbHeaderStyleChange(Sender: TObject);
    procedure CbReadFormulasChange(Sender: TObject);
    procedure CbBackgroundColorGetColors(Sender: TCustomColorBox; Items: TStrings);
    procedure CbTextOverflowChange(Sender: TObject);
    procedure EdCellAddressEditingDone(Sender: TObject);
    procedure EdFrozenColsChange(Sender: TObject);
    procedure EdFrozenRowsChange(Sender: TObject);
    procedure FontComboBoxSelect(Sender: TObject);
    procedure FontSizeComboBoxSelect(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure MemoFormulaEditingDone(Sender: TObject);
    procedure TabControlChange(Sender: TObject);
    procedure WorksheetGridClick(Sender: TObject);
    procedure WorksheetGridHeaderClick(Sender: TObject; IsColumn: Boolean;
      Index: Integer);
    procedure WorksheetGridSelection(Sender: TObject; aCol, aRow: Integer);

  private
    FCopiedFormat: TCell;
    FPalette: TsPalette;
    FSpName: string;

    function EditComment(ACaption: String; var AText: String): Boolean;
    procedure LoadFile(const AFileName: String);
    procedure SetName(AValue: string);
    procedure SetupBackgroundColorBox;
    procedure UpdateBackgroundColorIndex;
    procedure UpdateCommentActions;
    procedure UpdateFontNameIndex;
    procedure UpdateFontSizeIndex;
    procedure UpdateFontStyleActions;
    procedure UpdateHorAlignmentActions;
    procedure UpdateNumFormatActions;
    procedure UpdateTextRotationActions;
    procedure UpdateVertAlignmentActions;
    procedure UpdateWordwraps;

    procedure DoOpen;
    procedure WorksheetGridWorkbookReadCellData(Sender: TObject; ARow,
      ACol: Cardinal; const ADataCell: PCell);
    procedure WorksheetGridWorkbookWriteCellData(Sender: TObject; ARow,
      ACol: Cardinal; var AValue: variant; var AStyleCell: PCell);
  public
    procedure BeforeRun;
    property Name : string read FSpName write SetName;
    procedure DefineMenuEntrys; override;
  end;

//  Excel 97-2003 spreadsheet (*.xls)|*.xls|Excel 5.0 spreadsheet (*.xls)|*.xls|Excel 2.1 spreadsheet (*.xls)|*.xls|Excel XML spreadsheet (*.xlsx)|*.xlsx|LibreOffice/OpenOffice spreadsheet (*.ods)|*.ods|Comma-delimited file (*.csv)|*.csv|Wikitable (wikimedia) (.wikitable_wikimedia)|*.wikitable_wikimedia
var
  fSpreetsheet: TfSpreetsheet;

implementation

uses
  TypInfo, LCLIntf, LCLType, LCLVersion, fpcanvas, Buttons,
  fpsutils, fpscsv, fpsNumFormat,usortparamsform,uformatsettingsform,
  uBaseVisualApplication,uData,uIntfStrConsts;

resourcestring
  strNewSpreetsheet                           = 'Neue Tabelle';

const
  DROPDOWN_COUNT = 24;

  HORALIGN_TAG   = 100;
  VERTALIGN_TAG  = 110;
  TEXTROT_TAG    = 130;
  NUMFMT_TAG     = 1000;  // difference 10 per format item

  LEFT_BORDER_THIN       = $0001;
  LEFT_BORDER_THICK      = $0002;
  LR_INNER_BORDER_THIN   = $0008;
  RIGHT_BORDER_THIN      = $0010;
  RIGHT_BORDER_THICK     = $0020;
  TOP_BORDER_THIN        = $0100;
  TOP_BORDER_THICK       = $0200;
  TB_INNER_BORDER_THIN   = $0800;
  BOTTOM_BORDER_THIN     = $1000;
  BOTTOM_BORDER_THICK    = $2000;
  BOTTOM_BORDER_DOUBLE   = $3000;
  LEFT_BORDER_MASK       = $0007;
  RIGHT_BORDER_MASK      = $0070;
  TOP_BORDER_MASK        = $0700;
  BOTTOM_BORDER_MASK     = $7000;
  LR_INNER_BORDER        = $0008;
  TB_INNER_BORDER        = $0800;
  // Use a combination of these bits for the "Tag" of the Border actions - see FormCreate.


{ TfSpreetsheet }

procedure TfSpreetsheet.AcBorderExecute(Sender: TObject);
const
  LINESTYLES: Array[1..3] of TsLinestyle = (lsThin, lsMedium, lsDouble);
var
  r,c: Integer;
  ls: integer;
  bs: TsCellBorderStyle;
begin
  bs.Color := scBlack;

  with WorksheetGrid do begin
    TbBorders.Action := TAction(Sender);

    BeginUpdate;
    try
      if TAction(Sender).Tag = 0 then begin
        CellBorders[Selection] := [];
        exit;
      end;
      // Top and bottom edges
      for c := Selection.Left to Selection.Right do begin
        ls := (TAction(Sender).Tag and TOP_BORDER_MASK) shr 8;
        if (ls <> 0) then begin
          CellBorder[c, Selection.Top] := CellBorder[c, Selection.Top] + [cbNorth];
          bs.LineStyle := LINESTYLES[ls];
          CellBorderStyle[c, Selection.Top, cbNorth] := bs;
        end;
        ls := (TAction(Sender).Tag and BOTTOM_BORDER_MASK) shr 12;
        if ls <> 0 then begin
          CellBorder[c, Selection.Bottom] := CellBorder[c, Selection.Bottom] + [cbSouth];
          bs.LineStyle := LINESTYLES[ls];
          CellBorderStyle[c, Selection.Bottom, cbSouth] := bs;
        end;
      end;
      // Left and right edges
      for r := Selection.Top to Selection.Bottom do begin
        ls := (TAction(Sender).Tag and LEFT_BORDER_MASK);
        if ls <> 0 then begin
          CellBorder[Selection.Left, r] := CellBorder[Selection.Left, r] + [cbWest];
          bs.LineStyle := LINESTYLES[ls];
          CellBorderStyle[Selection.Left, r, cbWest] := bs;
        end;
        ls := (TAction(Sender).Tag and RIGHT_BORDER_MASK) shr 4;
        if ls <> 0 then begin
          CellBorder[Selection.Right, r] := CellBorder[Selection.Right, r] + [cbEast];
          bs.LineStyle := LINESTYLES[ls];
          CellBorderStyle[Selection.Right, r, cbEast] := bs;
        end;
      end;
      // Inner edges along row (vertical border lines) - we assume only thin lines.
      bs.LineStyle := lsThin;
      if (TAction(Sender).Tag and LR_INNER_BORDER <> 0) and (Selection.Right > Selection.Left)
      then
        for r := Selection.Top to Selection.Bottom do begin
          CellBorder[Selection.Left, r] := CellBorder[Selection.Left, r] + [cbEast];
          CellBorderStyle[Selection.Left, r, cbEast] := bs;
          for c := Selection.Left+1 to Selection.Right-1 do begin
            CellBorder[c,r] := CellBorder[c, r] + [cbEast, cbWest];
            CellBorderStyle[c, r, cbEast] := bs;
            CellBorderStyle[c, r, cbWest] := bs;
          end;
          CellBorder[Selection.Right, r] := CellBorder[Selection.Right, r] + [cbWest];
          CellBorderStyle[Selection.Right, r, cbWest] := bs;
        end;
      // Inner edges along column (horizontal border lines)
      if (TAction(Sender).Tag and TB_INNER_BORDER <> 0) and (Selection.Bottom > Selection.Top)
      then
        for c := Selection.Left to Selection.Right do begin
          CellBorder[c, Selection.Top] := CellBorder[c, Selection.Top] + [cbSouth];
          CellBorderStyle[c, Selection.Top, cbSouth] := bs;
          for r := Selection.Top+1 to Selection.Bottom-1 do begin
            CellBorder[c, r] := CellBorder[c, r] + [cbNorth, cbSouth];
            CellBorderStyle[c, r, cbNorth] := bs;
            CellBorderStyle[c, r, cbSouth] := bs;
          end;
          CellBorder[c, Selection.Bottom] := CellBorder[c, Selection.Bottom] + [cbNorth];
          CellBorderStyle[c, Selection.Bottom, cbNorth] := bs;
        end;
    finally
      EndUpdate;
    end;
  end;
end;

procedure TfSpreetsheet.AcCommentAddExecute(Sender: TObject);
var
  r,c: Cardinal;
  cell: PCell;
  comment: String;
begin
  with WorksheetGrid do
  begin
    r := GetWorksheetRow(Row);
    c := GetWorksheetCol(Col);
    cell := Worksheet.FindCell(r, c);
    if Worksheet.HasComment(cell) then
      comment := Worksheet.ReadComment(cell)
    else
      comment := '';
    if EditComment(Format('Comment for cell %s', [GetCellString(r, c)]), comment)
    then
      Worksheet.WriteComment(r, c, comment);
  end;
end;

procedure TfSpreetsheet.AcCommentDeleteExecute(Sender: TObject);
var
  r, c: Cardinal;
  cell: PCell;
begin
  with WorksheetGrid do
  begin
    r := GetWorksheetRow(Row);
    c := GetWorksheetCol(Col);
    cell := Worksheet.FindCell(r, c);
    if Worksheet.HasComment(cell) then
      Worksheet.RemoveComment(cell);
  end;
end;

procedure TfSpreetsheet.AcAddColumnExecute(Sender: TObject);
begin
  WorksheetGrid.InsertCol(WorksheetGrid.Col);
  WorksheetGrid.Col := WorksheetGrid.Col + 1;
end;

procedure TfSpreetsheet.AcAddRowExecute(Sender: TObject);
begin
  WorksheetGrid.InsertRow(WorksheetGrid.Row);
  WorksheetGrid.Row := WorksheetGrid.Row + 1;
end;

procedure TfSpreetsheet.AcCopyFormatExecute(Sender: TObject);
var
  cell: PCell;
  r, c: Cardinal;
begin
  with WorksheetGrid do begin
    if Workbook = nil then
      exit;

    if AcCopyFormat.Checked then begin
      r := GetWorksheetRow(Row);
      c := GetWorksheetCol(Col);
      cell := Worksheet.FindCell(r, c);
      if cell <> nil then
        FCopiedFormat := cell^;
    end;
  end;
end;

procedure TfSpreetsheet.AcDeleteColumnExecute(Sender: TObject);
var
  c: Integer;
begin
  c := WorksheetGrid.Col;
  WorksheetGrid.DeleteCol(c);
  WorksheetGrid.Col := c;
end;

procedure TfSpreetsheet.AcDeleteRowExecute(Sender: TObject);
var
  r: Integer;
begin
  r := WorksheetGrid.Row;
  WorksheetGrid.DeleteRow(r);
  WorksheetGrid.Row := r;
end;

{ Changes the font of the selected cell by calling a standard font dialog. }
procedure TfSpreetsheet.AcFontExecute(Sender: TObject);
begin
  with WorksheetGrid do begin
    if Workbook = nil then
      exit;
    FontDialog.Font := CellFonts[Selection];
    if FontDialog.Execute then
      CellFonts[Selection] := FontDialog.Font;
  end;
end;

procedure TfSpreetsheet.AcFontStyleExecute(Sender: TObject);
var
  style: TsFontstyles;
begin
  with WorksheetGrid do begin
    if Workbook = nil then
      exit;
    style := [];
    if AcFontBold.Checked then Include(style, fssBold);
    if AcFontItalic.Checked then Include(style, fssItalic);
    if AcFontStrikeout.Checked then Include(style, fssStrikeout);
    if AcFontUnderline.Checked then Include(style, fssUnderline);
    CellFontStyles[Selection] := style;
  end;
end;

procedure TfSpreetsheet.AcFormatSettingsExecute(Sender: TObject);
var
  F: TFormatSettingsForm;
begin
  if WorksheetGrid.Workbook = nil then
    exit;

  F := TFormatSettingsForm.Create(nil);
  try
    F.FormatSettings := WorksheetGrid.Workbook.FormatSettings;
    if F.ShowModal = mrOK then
    begin
      WorksheetGrid.Workbook.FormatSettings := F.FormatSettings;
      WorksheetGrid.Invalidate;
    end;
  finally
    F.Free;
  end;
end;

procedure TfSpreetsheet.AcHorAlignmentExecute(Sender: TObject);
var
  hor_align: TsHorAlignment;
begin
  if TAction(Sender).Checked then
    hor_align := TsHorAlignment(TAction(Sender).Tag - HORALIGN_TAG)
  else
    hor_align := haDefault;
  with WorksheetGrid do HorAlignments[Selection] := hor_align;
  UpdateHorAlignmentActions;
end;

procedure TfSpreetsheet.AcIncDecDecimalsExecute(Sender: TObject);
var
  cell: PCell;
  decs: Byte;
  currsym: String;
  nf: TsNumberFormat;
  nfs: String;
begin
  currsym := Sender.ClassName;
  with WorksheetGrid do begin
    if Workbook = nil then
      exit;
    cell := Worksheet.FindCell(GetWorksheetRow(Row), GetWorksheetCol(Col));
    if (cell <> nil) then begin
      Worksheet.ReadNumFormat(cell, nf, nfs);
      if nf = nfGeneral then begin
        Worksheet.WriteNumberFormat(cell, nfFixed, '0.00');
        exit;
      end;
      Worksheet.GetNumberFormatAttributes(cell, decs, currSym);
      if (Sender = AcIncDecimals) then
        Worksheet.WriteDecimals(cell, decs+1)
      else
      if (Sender = AcDecDecimals) and (decs > 0) then
        Worksheet.WriteDecimals(cell, decs-1);
    end;
  end;
end;

procedure TfSpreetsheet.AcMergeCellsExecute(Sender: TObject);
begin
  AcMergeCells.Checked := not AcMergeCells.Checked;
  if AcMergeCells.Checked then
    WorksheetGrid.MergeCells
  else
    WorksheetGrid.UnmergeCells;
  WorksheetGridSelection(nil, WorksheetGrid.Col, WorksheetGrid.Row);
end;

procedure TfSpreetsheet.AcNewExecute(Sender: TObject);
begin
  WorksheetGrid.NewWorkbook(26, 100);

  WorksheetGrid.BeginUpdate;
  try
    WorksheetGrid.Col := WorksheetGrid.FixedCols;
    WorksheetGrid.Row := WorksheetGrid.FixedRows;
    SetupBackgroundColorBox;
    WorksheetGridSelection(nil, WorksheetGrid.Col, WorksheetGrid.Row);
  finally
    WorksheetGrid.EndUpdate;
  end;
end;

procedure TfSpreetsheet.AcNumFormatExecute(Sender: TObject);
const
  DATETIME_CUSTOM: array[0..2] of string = ('', 'nn:ss', 'nn:ss.zzz');
var
  c, r: Cardinal;
  cell: PCell;
  fmt: String;
  decs: Byte;
  cs: String;
  isDateTimeFmt: Boolean;
  nf, cell_nf: TsNumberFormat;
  cell_nfs: String;
begin
  if TAction(Sender).Checked then
    nf := TsNumberFormat((TAction(Sender).Tag - NUMFMT_TAG) div 10)
  else
    nf := nfGeneral;

  fmt := '';
  isDateTimeFmt := IsDateTimeFormat(nf);
  if nf = nfCustom then begin
    fmt := DATETIME_CUSTOM[TAction(Sender).Tag mod 10];
    isDateTimeFmt := true;
  end;

  with WorksheetGrid do begin
    c := GetWorksheetCol(Col);
    r := GetWorksheetRow(Row);
    cell := Worksheet.GetCell(r, c);
    Worksheet.ReadNumFormat(cell, cell_nf, cell_nfs);
    Worksheet.GetNumberFormatAttributes(cell, decs, cs);
    if cs = '' then cs := '?';
    case cell^.ContentType of
      cctNumber, cctDateTime:
        if isDateTimeFmt then begin
          if IsDateTimeFormat(cell_nf) then
            Worksheet.WriteDateTime(cell, cell^.DateTimeValue, nf, fmt)
          else
            Worksheet.WriteDateTime(cell, cell^.NumberValue, nf, fmt);
        end else
        if IsCurrencyFormat(nf) then begin
          if IsDateTimeFormat(cell_nf) then
            Worksheet.WriteCurrency(cell, cell^.DateTimeValue, nf, decs, cs)
          else
            Worksheet.WriteCurrency(cell, cell^.Numbervalue, nf, decs, cs);
        end else begin
          if IsDateTimeFormat(cell_nf) then
            Worksheet.WriteNumber(cell, cell^.DateTimeValue, nf, decs)
          else
            Worksheet.WriteNumber(cell, cell^.NumberValue, nf, decs)
        end;
      else
        Worksheet.WriteNumberformat(cell, nf, fmt);
    end;
  end;
  UpdateNumFormatActions;
end;


procedure TfSpreetsheet.AcOpenExecute(Sender: TObject);
begin
  if OpenDialog.Execute then
    LoadFile(OpenDialog.FileName);
end;

procedure TfSpreetsheet.AcQuitExecute(Sender: TObject);
begin

end;

procedure TfSpreetsheet.AcSaveAsExecute(Sender: TObject);
// Saves sheet in grid to file, overwriting existing file
var
  err: String = '';
  fmt: TsSpreadsheetFormat;
begin
  if WorksheetGrid.Workbook = nil then
    exit;

  if SaveDialog.Execute then
  begin
    Screen.Cursor := crHourglass;
    case SaveDialog.FilterIndex of
      1: fmt := sfExcel8;
      2: fmt := sfExcel5;
      3: fmt := sfExcel2;
      4: fmt := sfOOXML;
      5: fmt := sfOpenDocument;
      6: fmt := sfCSV;
      7: fmt := sfWikiTable_wikimedia;
    end;
    try
      WorksheetGrid.SaveToSpreadsheetFile(Utf8ToAnsi(SaveDialog.FileName), fmt);
    finally
      Screen.Cursor := crDefault;
      err := WorksheetGrid.Workbook.ErrorMsg;
      if err <> '' then
        MessageDlg(err, mtError, [mbOK], 0);
    end;
  end;
end;

procedure TfSpreetsheet.AcShowGridlinesExecute(Sender: TObject);
begin
  WorksheetGrid.ShowGridLines := AcShowGridLines.Checked;
end;

procedure TfSpreetsheet.AcShowHeadersExecute(Sender: TObject);
begin
  WorksheetGrid.ShowHeaders := AcShowHeaders.Checked;
end;

procedure TfSpreetsheet.AcSortColAscExecute(Sender: TObject);
var
  c: Cardinal;
  sortParams: TsSortParams;
begin
  c := WorksheetGrid.GetWorksheetCol(WorksheetGrid.Col);
  sortParams := InitSortParams;
  WorksheetGrid.BeginUpdate;
  try
    with WorksheetGrid.Worksheet do
      Sort(sortParams, 0, c, GetLastOccupiedRowIndex, c);
  finally
    WorksheetGrid.EndUpdate;
  end;
end;

procedure TfSpreetsheet.AcSortExecute(Sender: TObject);
var
  F: TSortParamsForm;
  r1,c1,r2,c2: Cardinal;
begin
  F := TSortParamsForm.Create(nil);
  try
    F.WorksheetGrid := WorksheetGrid;
    if F.ShowModal = mrOK then
    begin
      // Limits of the range to be sorted
      with WorksheetGrid do begin
        r1 := GetWorksheetRow(Selection.Top);
        c1 := GetWorksheetCol(Selection.Left);
        r2 := GetWorksheetRow(Selection.Bottom);
        c2 := GetWorksheetCol(Selection.Right);
      end;
      // Execute sorting. Use Begin/EndUpdate to avoid unnecessary redraws.
      WorksheetGrid.BeginUpdate;
      try
        WorksheetGrid.Worksheet.Sort(F.SortParams, r1, c1, r2, c2)
      finally
        WorksheetGrid.EndUpdate;
      end;
    end;
  finally
    F.Free;
  end;
end;

procedure TfSpreetsheet.AcTextRotationExecute(Sender: TObject);
var
  text_rot: TsTextRotation;
begin
  if TAction(Sender).Checked then
    text_rot := TsTextRotation(TAction(Sender).Tag - TEXTROT_TAG)
  else
    text_rot := trHorizontal;
  with WorksheetGrid do TextRotations[Selection] := text_rot;
  UpdateTextRotationActions;
end;

procedure TfSpreetsheet.AcVertAlignmentExecute(Sender: TObject);
var
  vert_align: TsVertAlignment;
begin
  if TAction(Sender).Checked then
    vert_align := TsVertAlignment(TAction(Sender).Tag - VERTALIGN_TAG)
  else
    vert_align := vaDefault;
  with WorksheetGrid do VertAlignments[Selection] := vert_align;
  UpdateVertAlignmentActions;
end;

procedure TfSpreetsheet.AcViewInspectorExecute(Sender: TObject);
begin
  InspectorPageControl.Visible := AcViewInspector.Checked;
  InspectorSplitter.Visible := AcViewInspector.Checked;
  InspectorSplitter.Left := 0;
end;

procedure TfSpreetsheet.AcWordwrapExecute(Sender: TObject);
begin
  with WorksheetGrid do Wordwraps[Selection] := TAction(Sender).Checked;
end;

procedure TfSpreetsheet.BeforeRun;
begin
  if ParamCount > 0 then
    LoadFile(ParamStr(1));
end;

procedure TfSpreetsheet.DefineMenuEntrys;
var
  aRoot : Variant;
begin
  aRoot := DefineMenuEntry(Null,strSpreetsheet,'spreetsheet',34);
  DefineMenuEntry(aRoot,strNewSpreetsheet,'spreetsheet/new',35);
end;

procedure TfSpreetsheet.CbBackgroundColorGetColors(Sender: TCustomColorBox; Items: TStrings);
var
  clr: TColor;
  clrName: String;
  i: Integer;
begin
  if WorksheetGrid.Workbook <> nil then begin
    Items.Clear;
    Items.AddObject('no fill', TObject(PtrInt(clNone)));
    for i:=0 to FPalette.Count-1 do begin
      clr := FPalette[i];
      clrName := GetColorName(clr);
      Items.AddObject(Format('%d: %s', [i, clrName]), TObject(PtrInt(clr)));
    end;
  end;
end;

procedure TfSpreetsheet.CbTextOverflowChange(Sender: TObject);
begin
  WorksheetGrid.TextOverflow := CbTextOverflow.Checked;
  WorksheetGrid.Invalidate;
end;

procedure TfSpreetsheet.CbBackgroundColorSelect(Sender: TObject);
begin
  if CbBackgroundColor.ItemIndex <= 0 then
    with WorksheetGrid do BackgroundColors[Selection] := scNotDefined
  else
    with WorksheetGrid do BackgroundColors[Selection] := PtrInt(CbBackgroundColor.Items.Objects[CbBackgroundColor.ItemIndex]);
end;

procedure TfSpreetsheet.CbHeaderStyleChange(Sender: TObject);
begin
  WorksheetGrid.TitleStyle := TTitleStyle(CbHeaderStyle.ItemIndex);
end;

procedure TfSpreetsheet.CbReadFormulasChange(Sender: TObject);
begin
  WorksheetGrid.ReadFormulas := CbReadFormulas.Checked;
end;

procedure TfSpreetsheet.EdCellAddressEditingDone(Sender: TObject);
var
  c, r: cardinal;
begin
  if ParseCellString(EdCellAddress.Text, r, c) then begin
    WorksheetGrid.Row := WorksheetGrid.GetGridRow(r);
    WorksheetGrid.Col := WorksheetGrid.GetGridCol(c);
  end;
end;

procedure TfSpreetsheet.EdFrozenColsChange(Sender: TObject);
begin
  WorksheetGrid.FrozenCols := EdFrozenCols.Value;
end;

procedure TfSpreetsheet.EdFrozenRowsChange(Sender: TObject);
begin
  WorksheetGrid.FrozenRows := EdFrozenRows.Value;
end;

function TfSpreetsheet.EditComment(ACaption: String; var AText: String): Boolean;
var
  F: TForm;
  memo: TMemo;
  panel: TPanel;
  btn: TBitBtn;
begin
  F := TForm.Create(nil);
  try
    F.Caption := ACaption;
    F.Width := 400;
    F.Height := 300;
    F.Position := poMainFormCenter;
    memo := TMemo.Create(F);
    memo.Parent := F;
    memo.Align := alClient;
    memo.BorderSpacing.Around := 4;
    memo.Lines.Text := AText;
    panel := TPanel.Create(F);
    panel.Parent := F;
    panel.Align := alBottom;
    panel.Height := 44;
    panel.BevelOuter := bvNone;
    panel.Caption := '';
    btn := TBitBtn.Create(F);
    btn.Parent := panel;
    btn.Kind := bkOK;
    btn.Left := panel.ClientWidth - 2*btn.Width - 2*8;
    btn.Top := 6;
    btn.Anchors := [akTop, akRight];
    btn := TBitBtn.Create(F);
    btn.Parent := panel;
    btn.Kind := bkCancel;
    btn.Left := panel.ClientWidth - btn.Width - 8;
    btn.Top := 6;
    btn.Anchors := [akTop, akRight];
    if F.ShowModal = mrOK then
    begin
      Result := true;
      AText := memo.Lines.Text;
    end else
      Result := false;
  finally
    F.Free;
  end;
end;

procedure TfSpreetsheet.FontComboBoxSelect(Sender: TObject);
var
  fname: String;
begin
  fname := FontCombobox.Items[FontCombobox.ItemIndex];
  if fname <> '' then
    with WorksheetGrid do CellFontNames[Selection] := fName;
end;

procedure TfSpreetsheet.FontSizeComboBoxSelect(Sender: TObject);
var
  sz: Integer;
begin
  sz := StrToInt(FontSizeCombobox.Items[FontSizeCombobox.ItemIndex]);
  if sz > 0 then
    with WorksheetGrid do CellFontSizes[Selection] := sz;
end;

procedure TfSpreetsheet.FormActivate(Sender: TObject);
begin
  WorksheetGridSelection(nil, WorksheetGrid.Col, WorksheetGrid.Row);
end;

procedure TfSpreetsheet.FormCreate(Sender: TObject);
begin
  CbBackgroundColor.ItemHeight := FontCombobox.ItemHeight;
 {$IF LCL_FullVersion >= 1020000}
  CbBackgroundColor.ColorRectWidth := CbBackgroundColor.ItemHeight - 6; // to get a square box...
 {$ENDIF}

  InspectorPageControl.ActivePageIndex := 0;

  // Populate font combobox
  FontCombobox.Items.Assign(Screen.Fonts);

  // Set the Tags of the Border actions
  AcBorderNone.Tag := 0;
  AcBorderLeft.Tag := LEFT_BORDER_THIN;
  AcBorderHCenter.Tag := LR_INNER_BORDER_THIN;
  AcBorderRight.Tag := RIGHT_BORDER_THIN;
  AcBorderTop.Tag := TOP_BORDER_THIN;
  AcBorderVCenter.Tag := TB_INNER_BORDER_THIN;
  AcBorderBottom.Tag := BOTTOM_BORDER_THIN;
  AcBorderBottomDbl.Tag := BOTTOM_BORDER_DOUBLE;
  AcBorderBottomMedium.Tag := BOTTOM_BORDER_THICK;
  AcBorderTopBottom.Tag := TOP_BORDER_THIN + BOTTOM_BORDER_THIN;
  AcBorderTopBottomThick.Tag := TOP_BORDER_THIN + BOTTOM_BORDER_THICK;
  AcBorderInner.Tag := LR_INNER_BORDER_THIN + TB_INNER_BORDER_THIN;
  AcBorderOuter.Tag := LEFT_BORDER_THIN + RIGHT_BORDER_THIN + TOP_BORDER_THIN + BOTTOM_BORDER_THIN;
  AcBorderOuterMedium.Tag := LEFT_BORDER_THICK + RIGHT_BORDER_THICK + TOP_BORDER_THICK + BOTTOM_BORDER_THICK;
  AcBorderAll.Tag := AcBorderOuter.Tag + AcBorderInner.Tag;

  FontCombobox.DropDownCount := DROPDOWN_COUNT;
  FontSizeCombobox.DropDownCount := DROPDOWN_COUNT;
  CbBackgroundColor.DropDownCount := DROPDOWN_COUNT;

  FPalette := TsPalette.Create;
  FPalette.AddExcelColors;

  // Initialize a new empty workbook
  AcNewExecute(nil);
end;

procedure TfSpreetsheet.FormDestroy(Sender: TObject);
begin
  FPalette.Free;
end;

procedure TfSpreetsheet.LoadFile(const AFileName: String);
// Loads first worksheet from file into grid. File name is UTF8.
var
  err: String;
begin
  // Load file
  Screen.Cursor := crHourglass;
  try
    try
      WorksheetGrid.LoadFromSpreadsheetFile(utf8ToAnsi(AFileName));
    except
      on E: Exception do begin
        // In an error occurs show at least an empty valid worksheet
        AcNewExecute(nil);
        MessageDlg(E.Message, mtError, [mbOk], 0);
        exit;
      end;
    end;

    // Update user interface
    Caption := Format('spready - %s (%s)', [
      AFilename,
      GetFileFormatName(WorksheetGrid.Workbook.FileFormat)
    ]);
    AcShowGridLines.Checked := WorksheetGrid.ShowGridLines;
    AcShowHeaders.Checked := WorksheetGrid.ShowHeaders;
    EdFrozenCols.Value := WorksheetGrid.FrozenCols;
    EdFrozenRows.Value := WorksheetGrid.FrozenRows;
    WorksheetGrid.TextOverflow := CbTextOverflow.Checked;
    SetupBackgroundColorBox;

    // Load names of worksheets into tabcontrol and show first sheet
    WorksheetGrid.GetSheets(TabControl.Tabs);
    TabControl.TabIndex := 0;
    // Update display
    WorksheetGridSelection(nil, WorksheetGrid.Col, WorksheetGrid.Row);

  finally
    Screen.Cursor := crDefault;

    err := WorksheetGrid.Workbook.ErrorMsg;
    if err <> '' then
      MessageDlg(err, mtError, [mbOK], 0);
  end;
end;

procedure TfSpreetsheet.SetName(AValue: string);
begin
  if FSpName=AValue then Exit;
  FSpName:=AValue;
end;

procedure TfSpreetsheet.MemoFormulaEditingDone(Sender: TObject);
var
  r, c: Cardinal;
  s: String;
begin
  r := WorksheetGrid.GetWorksheetRow(WorksheetGrid.Row);
  c := WorksheetGrid.GetWorksheetCol(WorksheetGrid.Col);
  s := FormulaMemo.Lines.Text;
  if (s <> '') and (s[1] = '=') then
    WorksheetGrid.Worksheet.WriteFormula(r, c, Copy(s, 2, Length(s)), true)
  else
    WorksheetGrid.Worksheet.WriteCellValueAsString(r, c, s);
end;

procedure TfSpreetsheet.SetupBackgroundColorBox;
begin
  // This change triggers re-reading of the workbooks palette by the OnGetColors
  // event of the ColorBox.
  CbBackgroundColor.Style := CbBackgroundColor.Style - [cbCustomColors];
  CbBackgroundColor.Style := CbBackgroundColor.Style + [cbCustomColors];
  Application.ProcessMessages;
end;

procedure TfSpreetsheet.TabControlChange(Sender: TObject);
begin
  WorksheetGrid.SelectSheetByIndex(TabControl.TabIndex);
  WorksheetGridSelection(self, WorksheetGrid.Col, WorksheetGrid.Row);
end;

procedure TfSpreetsheet.WorksheetGridClick(Sender: TObject);
begin

end;

procedure TfSpreetsheet.WorksheetGridHeaderClick(Sender: TObject; IsColumn: Boolean;
  Index: Integer);
begin
  Unused(Sender);
  Unused(IsColumn, Index);
  //ShowMessage('Header click');
end;

procedure TfSpreetsheet.UpdateBackgroundColorIndex;
var
  clr: TsColor;
begin
  with WorksheetGrid do
    clr := BackgroundColors[Selection];
  if (clr = scNotDefined) or (clr = scTransparent) then
    CbBackgroundColor.ItemIndex := 0 // no fill
  else
    CbBackgroundColor.ItemIndex := CbBackgroundColor.Items.IndexOfObject(TObject(PtrInt(clr)));
end;

procedure TfSpreetsheet.UpdateHorAlignmentActions;
var
  i: Integer;
  ac: TAction;
  hor_align: TsHorAlignment;
begin
  with WorksheetGrid do hor_align := HorAlignments[Selection];
  for i:=0 to ActionList.ActionCount-1 do
  begin
    ac := TAction(ActionList.Actions[i]);
    if (ac.Tag >= HORALIGN_TAG) and (ac.Tag < HORALIGN_TAG+10) then
      ac.Checked := ((ac.Tag - HORALIGN_TAG) = ord(hor_align));
  end;
end;

procedure TfSpreetsheet.UpdateCommentActions;
var
  r, c: Cardinal;
  cell: PCell;
  hasCmnt: Boolean;
begin
  with WorksheetGrid do
  begin
    r := GetWorksheetRow(Row);
    c := GetWorksheetCol(Col);
    cell := Worksheet.FindCell(r, c);
    hasCmnt := Worksheet.HasComment(cell);
  end;
  AcCommentAdd.Enabled := not hasCmnt;
  AcCommentEdit.Enabled := hasCmnt;
  AcCommentDelete.Enabled := hasCmnt;
end;

procedure TfSpreetsheet.UpdateFontNameIndex;
var
  fname: String;
begin
  with WorksheetGrid do fname := CellFontNames[Selection];
  if fname = '' then
    FontCombobox.ItemIndex := -1
  else
    FontCombobox.ItemIndex := FontCombobox.Items.IndexOf(fname);
end;

procedure TfSpreetsheet.UpdateFontSizeIndex;
var
  sz: Single;
begin
  with WorksheetGrid do sz := CellFontSizes[Selection];
  if sz < 0 then
    FontSizeCombobox.ItemIndex := -1
  else
    FontSizeCombobox.ItemIndex := FontSizeCombobox.Items.IndexOf(IntToStr(Round(sz)));
end;

procedure TfSpreetsheet.UpdateFontStyleActions;
var
  style: TsFontStyles;
begin
  with WorksheetGrid do style := CellFontStyles[Selection];
  AcFontBold.Checked := fssBold in style;
  AcFontItalic.Checked := fssItalic in style;
  AcFontUnderline.Checked := fssUnderline in style;
  AcFontStrikeout.Checked := fssStrikeOut in style;
end;

procedure TfSpreetsheet.UpdateNumFormatActions;
var
  i: Integer;
  ac: TAction;
  nf: TsNumberFormat;
  nfs: String;
  cell: PCell;
  r,c: Cardinal;
  found: Boolean;
begin
  with WorksheetGrid do begin
    r := GetWorksheetRow(Row);
    c := GetWorksheetCol(Col);
    cell := Worksheet.FindCell(r, c);
    if (cell = nil) or not (cell^.ContentType in [cctNumber, cctDateTime]) then
      nf := nfGeneral
    else
      Worksheet.ReadNumFormat(cell, nf, nfs);
    for i:=0 to ActionList.ActionCount-1 do begin
      ac := TAction(ActionList.Actions[i]);
      if (ac.Tag >= NUMFMT_TAG) and (ac.Tag < NUMFMT_TAG + 300) then begin
        found := ((ac.Tag - NUMFMT_TAG) div 10 = ord(nf));
        if nf = nfCustom then
          case (ac.Tag - NUMFMT_TAG) mod 10 of
            1: found := nfs = 'nn:ss';
            2: found := nfs = 'nn:ss.z';
          end;
        ac.Checked := found;
      end;
    end;
    Invalidate;
  end;
end;

procedure TfSpreetsheet.UpdateTextRotationActions;
var
  i: Integer;
  ac: TAction;
  text_rot: TsTextRotation;
begin
  with WorksheetGrid do text_rot := TextRotations[Selection];
  for i:=0 to ActionList.ActionCount-1 do begin
    ac := TAction(ActionList.Actions[i]);
    if (ac.Tag >= TEXTROT_TAG) and (ac.Tag < TEXTROT_TAG+10) then
      ac.Checked := ((ac.Tag - TEXTROT_TAG) = ord(text_rot));
  end;
end;

procedure TfSpreetsheet.UpdateVertAlignmentActions;
var
  i: Integer;
  ac: TAction;
  vert_align: TsVertAlignment;
begin
  with WorksheetGrid do vert_align := VertAlignments[Selection];
  for i:=0 to ActionList.ActionCount-1 do begin
    ac := TAction(ActionList.Actions[i]);
    if (ac.Tag >= VERTALIGN_TAG) and (ac.Tag < VERTALIGN_TAG+10) then
      ac.Checked := ((ac.Tag - VERTALIGN_TAG) = ord(vert_align));
  end;
end;

procedure TfSpreetsheet.UpdateWordwraps;
var
  wrapped: Boolean;
begin
  with WorksheetGrid do wrapped := Wordwraps[Selection];
  AcWordwrap.Checked := wrapped;
end;

procedure TfSpreetsheet.DoOpen;
begin
  WorksheetGrid.Workbook.OnReadCellData:=@WorksheetGridWorkbookReadCellData;
  WorksheetGrid.Workbook.OnWriteCellData:=@WorksheetGridWorkbookWriteCellData;
  // Enter virtual mode
  WorksheetGrid.Workbook.Options := MyWorkbook.Options + [boVirtualMode];

  // Define number of columns - we want a column for each field
  WorksheetGrid.Workbook.VirtualColCount := MyDatabase.FieldCount;

  // Define number of rows - we want every record, plus 1 row for the title row
  WorksheetGrid.Workbook.VirtualRowCount := MyDatabase.RecordCount + 1;
end;

procedure TfSpreetsheet.WorksheetGridWorkbookReadCellData(Sender: TObject;
  ARow, ACol: Cardinal; const ADataCell: PCell);
begin

end;

procedure TfSpreetsheet.WorksheetGridWorkbookWriteCellData(Sender: TObject;
  ARow, ACol: Cardinal; var AValue: variant; var AStyleCell: PCell);
begin
{
MyWorksheet.WriteFontStyle(0, 0, [fssBold]);
MyWorksheet.WriteBackgroundColor(0, 0, scGray);


if ARow = 0 then begin
   // The value to be written to the spreadsheet is the field name.
   AValue := MyDatabase.Fields[ACol].FieldName;
   // Formatting is defined in the HeaderTemplateCell.
   AStyleCell := MyHeaderTemplateCell;
   // Move to first record
   MyDatabase.First;
 end else begin
   // The value to be written to the spreadsheet is the record value in the field corresponding to the column.
   // No special requirements on formatting --> leave AStyleCell at its default (nil).
   AValue := MyDatabase.Fields[ACol].AsVariant;
   // Advance database cursor if last field of record has been written
   if ACol = MyDatabase.FieldCount-1 then MyDatabase.Next;
 end;
 }
end;

procedure TfSpreetsheet.WorksheetGridSelection(Sender: TObject; aCol,
  aRow: Integer);
var
  r, c: Cardinal;
  cell: PCell;
  s: String;
begin
  if WorksheetGrid.Workbook = nil then
    exit;

  r := WorksheetGrid.GetWorksheetRow(ARow);
  c := WorksheetGrid.GetWorksheetCol(ACol);

  if AcCopyFormat.Checked then begin
    WorksheetGrid.Worksheet.CopyFormat(@FCopiedFormat, r, c);
    AcCopyFormat.Checked := false;
  end;

  cell := WorksheetGrid.Worksheet.FindCell(r, c);
  if cell <> nil then begin
    s := WorksheetGrid.Worksheet.ReadFormulaAsString(cell, true);
    if s <> '' then begin
      if s[1] <> '=' then s := '=' + s;
      FormulaMemo.Lines.Text := s;
    end else
    begin
      case cell^.ContentType of
        cctNumber:
          s := FloatToStr(cell^.NumberValue);
        cctDateTime:
          if cell^.DateTimeValue < 1.0 then
            s := FormatDateTime('tt', cell^.DateTimeValue)
          else
            s := FormatDateTime('c', cell^.DateTimeValue);
        cctUTF8String:
          s := cell^.UTF8StringValue;
        else
          s := WorksheetGrid.Worksheet.ReadAsUTF8Text(cell);
      end;
      FormulaMemo.Lines.Text := s;
    end;
  end else
    FormulaMemo.Text := '';

  EdCellAddress.Text := GetCellString(r, c, [rfRelRow, rfRelCol]);
  AcMergeCells.Checked := WorksheetGrid.Worksheet.IsMerged(cell);

  UpdateHorAlignmentActions;
  UpdateVertAlignmentActions;
  UpdateWordwraps;
  UpdateBackgroundColorIndex;
//  UpdateFontActions;
  UpdateFontNameIndex;
  UpdateFontSizeIndex;
  UpdateFontStyleActions;
  UpdateTextRotationActions;
  UpdateNumFormatActions;
  UpdateCommentActions;

end;


initialization
  {$I uspreetsheet.lrs}
  TBaseVisualApplication(Application).RegisterForm(TfSpreetsheet);

end.

