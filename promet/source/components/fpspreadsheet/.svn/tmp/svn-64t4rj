{
fpspreadsheetgrid.pas

Grid component which can load and write data from / to FPSpreadsheet documents

AUTHORS: Felipe Monteiro de Carvalho
}
unit fpspreadsheetgrid;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, Grids,
  fpspreadsheet;

type

  { TsCustomWorksheetGrid }

  TsCustomWorksheetGrid = class(TCustomStringGrid)
  private
    FWorksheet: TsWorksheet;
    FDisplayFixedColRow: Boolean;
    procedure SetDisplayFixedColRow(const AValue: Boolean);
    { Private declarations }
  protected
    { Protected declarations }
  public
    { methods }
    constructor Create(AOwner: TComponent); override;
    procedure LoadFromWorksheet(AWorksheet: TsWorksheet);
    procedure LoadFromSpreadsheetFile(AFileName: string; AFormat: TsSpreadsheetFormat; AWorksheetIndex: Integer = 0);
    procedure SaveToWorksheet(AWorksheet: TsWorksheet);
    property DisplayFixedColRow: Boolean read FDisplayFixedColRow write SetDisplayFixedColRow;
  end;

  { TsWorksheetGrid }

  TsWorksheetGrid = class(TsCustomWorksheetGrid)
  published
    property Align;
    property AlternateColor;
    property Anchors;
    property AutoAdvance;
    property AutoEdit;
    property AutoFillColumns;
    //property BiDiMode;
    property BorderSpacing;
    property BorderStyle;
    property Color;
    property ColCount;
    property Columns;
    property Constraints;
    property DefaultColWidth;
    property DefaultDrawing;
    property DefaultRowHeight;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property ExtendedSelect;
    property FixedColor;
    property Flat;
    property Font;
    property GridLineWidth;
    property HeaderHotZones;
    property HeaderPushZones;
    property MouseWheelOption;
    property Options;
    //property ParentBiDiMode;
    property ParentColor default false;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property RowCount;
    property ScrollBars;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property TitleFont;
    property TitleImageList;
    property TitleStyle;
    property UseXORFeatures;
    property Visible;
    property VisibleColCount;
    property VisibleRowCount;


    property OnBeforeSelection;
    property OnChangeBounds;
    property OnClick;
    property OnColRowDeleted;
    property OnColRowExchanged;
    property OnColRowInserted;
    property OnColRowMoved;
    property OnCompareCells;
    property OnDragDrop;
    property OnDragOver;
    property OnDblClick;
    property OnDrawCell;
    property OnEditButtonClick;
    property OnEditingDone;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetEditMask;
    property OnGetEditText;
    property OnHeaderClick;
    property OnHeaderSized;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnPickListSelect;
    property OnPrepareCanvas;
    property OnResize;
    property OnSelectEditor;
    property OnSelection;
    property OnSelectCell;
    property OnSetEditText;
    property OnShowHint;
    property OnStartDock;
    property OnStartDrag;
    property OnTopLeftChanged;
    property OnUTF8KeyPress;
    property OnValidateEntry;
    property OnContextPopup;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Additional',[TsWorksheetGrid]);
end;

const
  INT_FPSCOLROW_TO_GRIDCOLROW_WITH_FIXEDCOLROW = 2;
  INT_FPSCOLROW_TO_GRIDCOLROW = 1;

{ TsCustomWorksheetGrid }

procedure TsCustomWorksheetGrid.SetDisplayFixedColRow(const AValue: Boolean);
var
  x: Integer;
begin
  if AValue = FDisplayFixedColRow then Exit;

  FDisplayFixedColRow := AValue;

  if AValue then
  begin
    for x := 1 to ColCount - 1 do
      SetCells(x, 0, 'A');
    for x := 1 to RowCount - 1 do
      SetCells(0, x, IntToStr(x));
  end;
end;

constructor TsCustomWorksheetGrid.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FDisplayFixedColRow := False;
  FixedCols := 0;
  FixedRows := 0;
end;

procedure TsCustomWorksheetGrid.LoadFromWorksheet(AWorksheet: TsWorksheet);
var
  x, lRow, lCol: Integer;
  lStr: string;
  lCell: PCell;
begin
  FWorksheet := AWorksheet;

  { First get the size of the table }
  if FWorksheet.GetCellCount = 0 then
  begin
    ColCount := 0;
    RowCount := 0;
  end
  else
  begin
    if DisplayFixedColRow then
    begin
      ColCount := FWorksheet.GetLastColNumber() + INT_FPSCOLROW_TO_GRIDCOLROW_WITH_FIXEDCOLROW;
      RowCount := FWorksheet.GetLastRowNumber() + INT_FPSCOLROW_TO_GRIDCOLROW_WITH_FIXEDCOLROW;
    end
    else
    begin
      ColCount := FWorksheet.GetLastColNumber() + INT_FPSCOLROW_TO_GRIDCOLROW;
      RowCount := FWorksheet.GetLastRowNumber() + INT_FPSCOLROW_TO_GRIDCOLROW;
    end;
  end;

  { Now copy the contents }

  lCell := FWorksheet.GetFirstCell();
  for x := 0 to FWorksheet.GetCellCount() - 1 do
  begin
    lCol := lCell^.Col;
    lRow := lCell^.Row;
    lStr := FWorksheet.ReadAsUTF8Text(lRow, lCol);

    if DisplayFixedColRow then
      SetCells(lCol + 1, lRow + 1, lStr)
    else
      SetCells(lCol, lRow, lStr);

    lCell := FWorksheet.GetNextCell();
  end;
end;

procedure TsCustomWorksheetGrid.LoadFromSpreadsheetFile(AFileName: string;
  AFormat: TsSpreadsheetFormat; AWorksheetIndex: Integer);
var
  lWorkbook: TsWorkbook;
begin
  lWorkbook := TsWorkbook.Create;
  try
    lWorkbook.ReadFromFile(AFileName, AFormat);
    LoadFromWorksheet(lWorkbook.GetWorksheetByIndex(AWorksheetIndex));
  finally
    lWorkbook.Free;
  end;
end;

procedure TsCustomWorksheetGrid.SaveToWorksheet(AWorksheet: TsWorksheet);
var
  x, y: Integer;
  Str: string;
begin
  if AWorksheet = nil then Exit;

  { Copy the contents }

  for x := 0 to ColCount - 1 do
    for y := 0 to RowCount - 1 do
    begin
      Str := GetCells(x, y);
      if Str <> '' then AWorksheet.WriteUTF8Text(y, x, Str);
    end;
end;

end.
