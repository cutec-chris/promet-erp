{*******************************************************************************
  Copyright (C) Christian Ulrich info@cu-tec.de

  This source is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 2 of the License, or commercial alternative
  contact us for more information

  This code is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
  to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
  MA 02111-1307, USA.
*******************************************************************************}
unit umeasurements;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, FileUtil, TAGraph, TASources, TAStyles, TALegendPanel,
  TANavigation, TAIntervalSources, TASeries, TADbSource, Forms, Controls,
  ExtCtrls, DbCtrls, DBGrids, Buttons, StdCtrls, db, uPrometFramesInplaceDB,
  uExtControls, uBaseDbClasses, Clipbrd, ActnList, StdActns, ComCtrls, DBActns,
  uMeasurement, types, uBaseVisualControls,TACustomSeries;
type

  { TfMeasurementFrame }

  TfMeasurementFrame = class(TPrometInplaceDBFrame)
    acRefresh: TAction;
    acZoomIn: TAction;
    acZoomOut: TAction;
    ActionList1: TActionList;
    Chart1: TChart;
    Chart1LineSeries1: TLineSeries;
    ChartNavScrollBar1: TChartNavScrollBar;
    DataSetNext1: TDataSetNext;
    DataSetPrior1: TDataSetPrior;
    DateTimeIntervalChartSource2: TDateTimeIntervalChartSource;
    MeasurementData: TDatasource;
    Measurements: TDatasource;
    EditCopy1: TEditCopy;
    EditPaste1: TEditPaste;
    gAdresses: TDBGrid;
    gAdresses1: TDBGrid;
    PageControl1: TPageControl;
    Diagramm: TTabSheet;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    tsData: TTabSheet;
    procedure acRefreshExecute(Sender: TObject);
  private
    { private declarations }
    Zoom : TDateTime;
    Position : TDateTime;
  public
    { public declarations }
    constructor Create(AOwner : TComponent);override;
    destructor Destroy;override;
    procedure SetDataSet(const AValue: TBaseDBDataSet);override;
    procedure SetRights(Editable : Boolean);override;
  end;

implementation
{$R *.lfm}
uses uData,Utils,Graphics;

procedure TfMeasurementFrame.acRefreshExecute(Sender: TObject);
var
  aSeries: TLineSeries;
begin
  with FDataSet as TMeasurement do
    begin
      First;
      Chart1.Series.Clear;
      while not EOF do
        begin
          if FieldByName('CHART').AsString='Y' then
            begin
              aSeries := TLineSeries.Create(Chart1);
              aSeries.LinePen.Color:=StringToColorDef(FieldByName('COLOR').AsString,clRed);
              Data.First;
              while not Data.EOF do
                begin
                  aSeries.AddXY(Data.FieldByName('DATE').AsFloat,Data.FieldByName('DATA').AsFloat);
                  Data.Next;
                end;
              Chart1.AddSeries(aSeries);
              if FieldByName('POSITION').AsString='R' then
                begin
                  Chart1.AxisList[2].Visible:=True;
                  aSeries.AxisIndexY:=2;
                end;
            end;
          Next;
        end;
      //Chart1.LogicalExtent AxisList[1].Range.Min:=Position;
      //Chart1.AxisList[1].Range.Max:=Position+Zoom;
    end;
end;

constructor TfMeasurementFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Zoom := 1;
  Position := trunc(Now());
end;

destructor TfMeasurementFrame.Destroy;
begin
  inherited Destroy;
end;

procedure TfMeasurementFrame.SetDataSet(const AValue: TBaseDBDataSet);
begin
  inherited SetDataSet(AValue);
  Measurements.DataSet := FDataSet.DataSet;
  MeasurementData.DataSet := TMeasurement(FDataSet).Data.DataSet;
  TMeasurement(FDataSet).Data.Open;
end;

procedure TfMeasurementFrame.SetRights(Editable: Boolean);
begin
  Enabled := Editable;
end;

end.

