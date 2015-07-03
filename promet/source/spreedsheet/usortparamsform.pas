unit usortparamsform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ButtonPanel, Grids, ExtCtrls, Buttons, StdCtrls,
  fpstypes, fpspreadsheet, fpspreadsheetgrid;

type

  { TSortParamsForm }

  TSortParamsForm = class(TForm)
    BtnAdd: TBitBtn;
    BtnDelete: TBitBtn;
    ButtonPanel: TButtonPanel;
    CbSortColsRows: TComboBox;
    CbPriority: TComboBox;
    TopPanel: TPanel;
    Grid: TStringGrid;
    procedure BtnAddClick(Sender: TObject);
    procedure BtnDeleteClick(Sender: TObject);
    procedure CbSortColsRowsChange(Sender: TObject);
    procedure GridSelectEditor(Sender: TObject; aCol, aRow: Integer;
      var Editor: TWinControl);
    procedure OKButtonClick(Sender: TObject);
  private
    { private declarations }
    FWorksheetGrid: TsWorksheetGrid;
    function GetSortParams: TsSortParams;
    procedure SetWorksheetGrid(AValue: TsWorksheetGrid);
    procedure UpdateColRowList;
    procedure UpdateCmds;
    function ValidParams(out AMsg: String): Boolean;
  public
    { public declarations }
    property SortParams: TsSortParams read GetSortParams;
    property WorksheetGrid: TsWorksheetGrid read FWorksheetGrid write SetWorksheetGrid;
  end;

var
  SortParamsForm: TSortParamsForm;

implementation

{$R *.lfm}

uses
  fpsutils;

procedure TSortParamsForm.CbSortColsRowsChange(Sender: TObject);
begin
  UpdateColRowList;
  UpdateCmds;
end;

procedure TSortParamsForm.GridSelectEditor(Sender: TObject;
  aCol, aRow: Integer; var Editor: TWinControl);
begin
  Unused(aCol, aRow);
  if (Editor is TCustomComboBox) then
    (Editor as TCustomComboBox).Style := csDropDownList;
end;

procedure TSortParamsForm.OKButtonClick(Sender: TObject);
var
  msg: String;
begin
  if not ValidParams(msg) then begin
    MessageDlg(msg, mtError, [mbOK], 0);
    ModalResult := mrNone;
  end;
end;

procedure TSortParamsForm.BtnAddClick(Sender: TObject);
var
  numConditions: Integer;
begin
  case CbSortColsRows.ItemIndex of
    0: numConditions := FWorksheetGrid.Selection.Right - FWorksheetGrid.Selection.Left + 1;
    1: numConditions := FWorksheetGrid.Selection.Bottom - FWorksheetGrid.Selection.Top + 1;
  end;
  if Grid.RowCount - Grid.FixedRows >= numConditions then
    exit;  // there can't be more conditions than defined by the worksheetgrid selection
  Grid.RowCount := Grid.RowCount + 1;
  Grid.Cells[0, Grid.RowCount-1] := 'Then by';
  Grid.Cells[1, Grid.RowCount-1] := '';
  Grid.Cells[2, Grid.RowCount-1] := '0';
  Grid.Cells[3, Grid.RowCount-1] := '0';
  UpdateCmds;
end;

procedure TSortParamsForm.BtnDeleteClick(Sender: TObject);
begin
  if Grid.RowCount = Grid.FixedRows + 1 then
    exit;  // 1 condition must remain
  Grid.DeleteRow(Grid.Row);
  Grid.Cells[0, 1] := 'Sort by';
  UpdateCmds;
end;

function TSortParamsForm.GetSortParams: TsSortParams;
var
  i, p: Integer;
  n: Cardinal;
  sortOptions: TsSortOptions;
  s: String;
begin
  // Sort by column or rows?
  Result := InitSortParams(CbSortColsRows.ItemIndex = 0, 0);

  // Number before Text, or reversed?
  Result.Priority := TsSortPriority(CbPriority.ItemIndex);

  for i:=Grid.FixedRows to Grid.RowCount-1 do
  begin
    sortOptions := [];

    // Sort index column
    s := Grid.Cells[1, i];   // the cell text is "Column A" or "Row A"
    if s = '' then
      raise Exception.Create('[TSortParamsForm.GetSortParams] No sort index selected.');
      // This case should have been detected already by the ValidParams method.

    p := pos(' ', s);     // we look for the space and extract column/row index
    if p = 0 then
      raise Exception.Create('[TSortParamsForm.GetSortParams] Unexpected string in grid.');
    s := copy(s, p+1, Length(s));
    case CbSortColsRows.ItemIndex of
      0: if not ParseCellColString(s, n) then
           raise Exception.CreateFmt('[TSortParamsForm.GetSortParams] '+
             'Unexpected column identifier in row %d', [i]);
      1: if TryStrToInt(s, LongInt(n)) then
           dec(n)
         else
           raise Exception.CreateFmt('[TSortParamsForm.GetSortParams] ' +
             'Unexpected row identifier in row %s', [i]);
    end;

    // Sort order column
    s := Grid.Cells[2, i];
    if s = '' then
      raise Exception.Create('[TSortParamsForm.GetSortParams] No sort direction selected.');
    if s = '1' then
      Include(sortOptions, ssoDescending);

    // Case sensitivity column
    s := Grid.Cells[3, i];
    if s = '1' then
      Include(sortOptions, ssoCaseInsensitive);

    SetLength(Result.Keys, Length(Result.Keys) + 1);
    with Result.Keys[Length(Result.Keys)-1] do
    begin
      Options := sortOptions;
      ColRowIndex := n;
    end;
  end; // for
end;

procedure TSortParamsForm.SetWorksheetGrid(AValue: TsWorksheetGrid);
begin
  FWorksheetGrid := AValue;
  UpdateColRowList;
  UpdateCmds;
  Grid.Cells[1, 1] := Grid.Columns[0].PickList[0];    // Sorting index
  Grid.Cells[2, 1] := '0';  // Ascending sort order   Grid.Columns[1].CheckedPickList[0];
  Grid.Cells[3, 1] := '0';  // case-sensitive comparisons
end;

procedure TSortParamsForm.UpdateColRowList;
var
  L: TStrings;
  r,c: LongInt;
  r1,c1, r2,c2: Cardinal;
begin
  with FWorksheetGrid do begin
    r1 := GetWorksheetRow(Selection.Top);
    c1 := GetWorksheetCol(Selection.Left);
    r2 := GetWorksheetRow(Selection.Bottom);
    c2 := GetWorksheetCol(Selection.Right);
  end;
  L := TStringList.Create;
  try
    case CbSortColsRows.ItemIndex of
      0: begin
           Grid.RowCount := Grid.FixedRows + 1;
           Grid.Columns[0].Title.Caption := 'Columns';
           for c := c1 to c2 do
             L.Add('Column ' + GetColString(c));
         end;
      1: begin
           Grid.RowCount := Grid.FixedRows + 1;
           Grid.Columns[0].Title.Caption := 'Rows';
           for r := r1 to r2 do
             L.Add('Row ' + IntToStr(r+1));
         end;
    end;
    Grid.Columns[0].PickList.Assign(L);
    for r := Grid.FixedRows to Grid.RowCount-1 do
    begin
      Grid.Cells[1, r] := '';
      Grid.Cells[2, r] := ''
    end;
  finally
    L.Free;
  end;
end;

procedure TSortParamsForm.UpdateCmds;
var
  r1,c1,r2,c2: Cardinal;
  numConditions: Integer;
begin
  with FWorksheetGrid do begin
    r1 := GetWorksheetRow(Selection.Top);
    c1 := GetWorksheetCol(Selection.Left);
    r2 := GetWorksheetRow(Selection.Bottom);
    c2 := GetWorksheetCol(Selection.Right);
  end;
  numConditions := Grid.RowCount - Grid.FixedRows;
  case CbSortColsRows.ItemIndex of
    0: BtnAdd.Enabled := numConditions < c2-c1+1;
    1: BtnAdd.Enabled := numConditions < r2-r1+1;
  end;
  BtnDelete.Enabled := numConditions > 1;
end;

function TSortParamsForm.ValidParams(out AMsg: String): Boolean;
var
  i: Integer;
begin
  Result := false;
  for i:=Grid.FixedRows to Grid.RowCount-1 do
  begin
    if Grid.Cells[1, i] = '' then
    begin
       AMsg := Format('No sorting criteria selected in row %d.', [i]);
       Grid.SetFocus;
       exit;
    end;
    if Grid.Cells[2, i] = '' then
    begin
       AMsg := Format('No sort order specified in row %d.', [i]);
       Grid.SetFocus;
       exit;
    end;
  end;
  Result := true;
end;


end.

