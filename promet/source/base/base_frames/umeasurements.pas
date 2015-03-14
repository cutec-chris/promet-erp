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
  uMeasurement, types, uBaseVisualControls,TACustomSeries, TAChartUtils;
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
    DataSetNext1: TAction;
    DataSetPrior1: TAction;
    DateTimeIntervalChartSource2: TDateTimeIntervalChartSource;
    gData1: TDBGrid;
    MeasurementData: TDatasource;
    Measurements: TDatasource;
    EditCopy1: TEditCopy;
    EditPaste1: TEditPaste;
    gData: TDBGrid;
    PageControl1: TPageControl;
    Panel1: TPanel;
    Splitter1: TSplitter;
    tsChart: TTabSheet;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    tsData: TTabSheet;
    procedure acRefreshExecute(Sender: TObject);
    procedure DataSetNext1Execute(Sender: TObject);
    procedure DataSetPrior1Execute(Sender: TObject);
    procedure tsChartShow(Sender: TObject);
  private
    { private declarations }
    Zoom : TDateTime;
    Position : TDateTime;
    IsFirstShow: Boolean;
    procedure AddData(aSeries : TBasicPointSeries;aStart,aEnd : TDateTime);
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
  aExtent: TDoubleRect;
  Found: Boolean;
begin
  Found := False;
  PageControl1.ActivePage:=tsData;
  if not Assigned(FDataSet) then exit;
  with FDataSet as TMeasurement do
    begin
      Open;
      First;
      Chart1.Series.Clear;
      while not EOF do
        begin
          if FieldByName('CHART').AsString='Y' then
            begin
              aSeries := TLineSeries.Create(Chart1);
              aSeries.LinePen.Color:=StringToColorDef(FieldByName('COLOR').AsString,clRed);
              AddData(aSeries,trunc(Now()),trunc(Now())+Zoom);
              Chart1.AddSeries(aSeries);
              if FieldByName('POSITION').AsString='R' then
                begin
                  Chart1.AxisList[2].Visible:=True;
                  aSeries.AxisIndexY:=2;
                end;
              Found := True;
            end;
          Next;
        end;
      aExtent := Chart1.GetFullExtent;
      aExtent.b.X:=Position+Zoom;
      aExtent.a.X:=Position;
      Chart1.LogicalExtent:=aExtent;
    end;
  if Found then
    PageControl1.ActivePage:=tsChart;
end;

procedure TfMeasurementFrame.DataSetNext1Execute(Sender: TObject);
var
  i: Integer;
  aExtent: TDoubleRect;
begin
  Position:=Position+Zoom;
  with FDataSet as TMeasurement do
    begin
      First;
      i := 0;
      while not EOF do
        begin
          if FieldByName('CHART').AsString='Y' then
            begin
              AddData(TBasicPointSeries(Chart1.Series[i]),Position,Position+Zoom);
              inc(i);
            end;
          Next;
        end;
      aExtent := Chart1.GetFullExtent;
      aExtent.b.X:=Position+Zoom;
      aExtent.a.X:=Position;
      Chart1.LogicalExtent:=aExtent;
    end;
end;

procedure TfMeasurementFrame.DataSetPrior1Execute(Sender: TObject);
var
  i: Integer;
  aExtent: TDoubleRect;
begin
  Position:=Position-Zoom;
  with FDataSet as TMeasurement do
    begin
      First;
      i := 0;
      while not EOF do
        begin
          if FieldByName('CHART').AsString='Y' then
            begin
              AddData(TBasicPointSeries(Chart1.Series[i]),Position,Position+Zoom);
              inc(i);
            end;
          Next;
        end;
      aExtent := Chart1.GetFullExtent;
      aExtent.b.X:=Position+Zoom;
      aExtent.a.X:=Position;
      Chart1.LogicalExtent:=aExtent;
    end;
end;

procedure TfMeasurementFrame.tsChartShow(Sender: TObject);
begin
  if IsFirstShow then
    acRefresh.Execute;
  IsFirstShow := False;
end;

procedure TfMeasurementFrame.AddData(aSeries: TBasicPointSeries; aStart,
  aEnd: TDateTime);
var
  aLastEntry : Double = -99999;
begin
  if (aStart>aSeries.GetXMin) and (aStart<aSeries.GetXMax) then
    aStart := aSeries.GetXMax;
  if (aEnd>aSeries.GetXMin) and (aEnd<aSeries.GetXMax) then
    aEnd := aSeries.GetXMin;
  TMeasurement(FDataSet).Data.Filter(Data.QuoteField('DATE')+'>'+Data.DateTimeToFilter(aStart)+' AND '+Data.QuoteField('DATE')+'<'+Data.DateTimeToFilter(aEnd));
  TMeasurement(FDataSet).Data.First;
  while (not TMeasurement(FDataSet).Data.EOF) do
    begin
      if (aLastEntry<> -99999) and (TMeasurement(FDataSet).FieldByName('INTERPOLATE').AsString='N') then
        aSeries.AddXY(TMeasurement(FDataSet).Data.FieldByName('DATE').AsFloat,aLastEntry);
      aSeries.AddXY(TMeasurement(FDataSet).Data.FieldByName('DATE').AsFloat,TMeasurement(FDataSet).Data.FieldByName('DATA').AsFloat);
      aLastEntry:=TMeasurement(FDataSet).Data.FieldByName('DATA').AsFloat;
      TMeasurement(FDataSet).Data.Next;
    end;
end;

constructor TfMeasurementFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Zoom := 1;
  Position := trunc(Now());
  IsFirstShow := True;
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

