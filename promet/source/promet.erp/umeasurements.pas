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
  Classes, SysUtils, FileUtil, Forms, Controls, ExtCtrls, DbCtrls, DBGrids,
  Buttons, StdCtrls, db, uPrometFramesInplaceDB, uExtControls, uBaseDbClasses,
  Clipbrd, ActnList, StdActns, ComCtrls,uMeasurement, types;
type

  { TfMeasurementFrame }

  TfMeasurementFrame = class(TPrometInplaceDBFrame)
    MeasurementData: TDatasource;
    Measurements: TDatasource;
    EditCopy1: TEditCopy;
    EditPaste1: TEditPaste;
    gAdresses: TDBGrid;
    gAdresses1: TDBGrid;
    PageControl1: TPageControl;
    tsData: TTabSheet;
  private
    { private declarations }
  public
    { public declarations }
    constructor Create(AOwner : TComponent);override;
    destructor Destroy;override;
    procedure SetDataSet(const AValue: TBaseDBDataSet);override;
    procedure SetRights(Editable : Boolean);override;
  end;

implementation
{$R *.lfm}
uses uData,Utils;

constructor TfMeasurementFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
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

