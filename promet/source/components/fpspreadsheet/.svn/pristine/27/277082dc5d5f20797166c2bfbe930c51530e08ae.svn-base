{
fpspreadsheetgrid.pas

Chart data source designed to work together with TChart from Lazarus to display the data
and with TsWorksheetGrid from FPSpreadsheet to load data from a grid.

AUTHORS: Felipe Monteiro de Carvalho
}
unit fpspreadsheetchart;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, Grids,
  // TChart
  {tasources,} TACustomSource,
  // FPSpreadsheet Visual
  fpspreadsheetgrid,
  // FPSpreadsheet
  fpspreadsheet, fpsutils;

type

  {@@ Chart data source designed to work together with TChart from Lazarus
    to display the data.

    The data can be loaded from a TsWorksheetGrid Grid component or
    directly from a TsWorksheet FPSpreadsheet Worksheet }

  { TsWorksheetChartSource }

  TsWorksheetChartSource = class(TCustomChartSource)
  private
    FInternalWorksheet: TsWorksheet;
    FPointsNumber: Integer;
    FXSelectionDirection: TsSelectionDirection;
    FYSelectionDirection: TsSelectionDirection;
//    FWorksheetGrid: TsWorksheetGrid;
    FXFirstCellCol: Integer;
    FXFirstCellRow: Integer;
    FYFirstCellCol: Integer;
    FYFirstCellRow: Integer;
    procedure SetPointsNumber(const AValue: Integer);
    procedure SetXSelectionDirection(const AValue: TsSelectionDirection);
    procedure SetYSelectionDirection(const AValue: TsSelectionDirection);
    procedure SetXFirstCellCol(const AValue: Integer);
    procedure SetXFirstCellRow(const AValue: Integer);
    procedure SetYFirstCellCol(const AValue: Integer);
    procedure SetYFirstCellRow(const AValue: Integer);
  protected
    FDataWorksheet: TsWorksheet;
    FCurItem: TChartDataItem;
    function GetCount: Integer; override;
    function GetItem(AIndex: Integer): PChartDataItem; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure LoadFromWorksheetGrid(const AValue: TsWorksheetGrid);
    procedure LoadPropertiesFromStrings(AXInterval, AYInterval, AXTitle, AYTitle, ATitle: string);
  public
  published
//    property WorksheetGrid: TsWorksheetGrid read FWorksheetGrid write SetWorksheetGrid;
    property PointsNumber: Integer read FPointsNumber write SetPointsNumber default 0;
    property XFirstCellCol: Integer read FXFirstCellCol write SetXFirstCellCol default 0;
    property XFirstCellRow: Integer read FXFirstCellRow write SetXFirstCellRow default 0;
    property YFirstCellCol: Integer read FYFirstCellCol write SetYFirstCellCol default 0;
    property YFirstCellRow: Integer read FYFirstCellRow write SetYFirstCellRow default 0;
    property XSelectionDirection: TsSelectionDirection read FXSelectionDirection write SetXSelectionDirection;
    property YSelectionDirection: TsSelectionDirection read FYSelectionDirection write SetYSelectionDirection;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Chart',[TsWorksheetChartSource]);
end;

{ TsWorksheetChartSource }

procedure TsWorksheetChartSource.SetPointsNumber(const AValue: Integer);
begin
  if FPointsNumber = AValue then exit;
  FPointsNumber := AValue;
  InvalidateCaches;
  Notify;
end;

procedure TsWorksheetChartSource.SetXSelectionDirection(
  const AValue: TsSelectionDirection);
begin
  if FXSelectionDirection=AValue then exit;
  FXSelectionDirection:=AValue;
  InvalidateCaches;
  Notify;
end;

procedure TsWorksheetChartSource.SetYSelectionDirection(
  const AValue: TsSelectionDirection);
begin
  if FYSelectionDirection=AValue then exit;
  FYSelectionDirection:=AValue;
  InvalidateCaches;
  Notify;
end;

procedure TsWorksheetChartSource.SetXFirstCellCol(const AValue: Integer);
begin
  if FXFirstCellCol=AValue then exit;
  FXFirstCellCol:=AValue;
  InvalidateCaches;
  Notify;
end;

procedure TsWorksheetChartSource.SetXFirstCellRow(const AValue: Integer);
begin
  if FXFirstCellRow=AValue then exit;
  FXFirstCellRow:=AValue;
  InvalidateCaches;
  Notify;
end;

procedure TsWorksheetChartSource.SetYFirstCellCol(const AValue: Integer);
begin
  if FYFirstCellCol=AValue then exit;
  FYFirstCellCol:=AValue;
  InvalidateCaches;
  Notify;
end;

procedure TsWorksheetChartSource.SetYFirstCellRow(const AValue: Integer);
begin
  if FYFirstCellRow=AValue then exit;
  FYFirstCellRow:=AValue;
  InvalidateCaches;
  Notify;
end;

function TsWorksheetChartSource.GetCount: Integer;
begin
  Result := FPointsNumber;
end;

function TsWorksheetChartSource.GetItem(AIndex: Integer): PChartDataItem;
var
  XRow, XCol, YRow, YCol: Integer;
begin
  // First calculate the cell position
  if XSelectionDirection = fpsVerticalSelection then
  begin
    XRow := FXFirstCellRow + AIndex;
    XCol := FXFirstCellCol;
  end
  else
  begin
    XRow := FXFirstCellRow;
    XCol := FXFirstCellCol + AIndex;
  end;

  if YSelectionDirection = fpsVerticalSelection then
  begin
    YRow := FYFirstCellRow + AIndex;
    YCol := FYFirstCellCol;
  end
  else
  begin
    YRow := FYFirstCellRow;
    YCol := FYFirstCellCol + AIndex;
  end;

  // Check the corresponding cell, if it is empty, use zero
  // If not, then get a number value

  FCurItem.X := FDataWorksheet.ReadAsNumber(XRow, XCol);
  FCurItem.Y := FDataWorksheet.ReadAsNumber(YRow, YCol);

  Result := @FCurItem;
end;

constructor TsWorksheetChartSource.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FInternalWorksheet := TsWorksheet.Create;
  FDataWorksheet := FInternalWorksheet;
end;

destructor TsWorksheetChartSource.Destroy;
begin
  if FInternalWorksheet <> nil then FInternalWorksheet.Free;
  inherited Destroy;
end;

procedure TsWorksheetChartSource.LoadFromWorksheetGrid(const AValue: TsWorksheetGrid);
begin
  if AValue = nil then Exit;

  AValue.SaveToWorksheet(FDataWorksheet);

  InvalidateCaches;
  Notify;
end;

procedure TsWorksheetChartSource.LoadPropertiesFromStrings(AXInterval,
  AYInterval, AXTitle, AYTitle, ATitle: string);
var
  lXCount, lYCount: Integer;
begin
  ParseIntervalString(AXInterval, FXFirstCellRow, FXFirstCellCol, lXCount, FXSelectionDirection);
  ParseIntervalString(AYInterval, FYFirstCellRow, FYFirstCellCol, lYCount, FYSelectionDirection);
  if lXCount <> lYCount then raise Exception.Create(
    'TsWorksheetChartSource.LoadPropertiesFromStrings: Interval sizes don''t match');
  FPointsNumber := lXCount;
end;

end.
