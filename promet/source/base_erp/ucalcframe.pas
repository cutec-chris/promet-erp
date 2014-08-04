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
Created 01.06.2006
*******************************************************************************}
unit ucalcframe;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, FileUtil, Forms, Controls, ExtCtrls, StdCtrls, DbCtrls,
  DBGrids, Menus, db, uPrometFramesInplace,uBaseDbClasses;

type

  { TfCalcPositionFrame }

  TfCalcPositionFrame = class(TPrometInplaceFrame)
    PosCalc: TDataSource;
    eCommonPrice: TDBEdit;
    eDiscont: TDBEdit;
    ePosPrice: TDBEdit;
    ePurchase: TDBEdit;
    eQuantity: TDBEdit;
    eSellPrice: TDBEdit;
    eWholePrice: TDBEdit;
    gPrices: TDBGrid;
    Label3: TLabel;
    Label4: TLabel;
    Label6: TLabel;
    lAddVAT: TLabel;
    lBP: TLabel;
    lCommonPrice: TLabel;
    lDiscont: TLabel;
    lPosPrice: TLabel;
    lQuantity: TLabel;
    lSEP: TLabel;
    lSP: TLabel;
    lWholePrice: TLabel;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    pmImage: TPopupMenu;
    Position: TDatasource;
  private
    FDataSet: TBaseDBDataset;
    procedure SetDataSet(AValue: TBaseDBDataset);
    { private declarations }
  public
    { public declarations }
    procedure SetRights(Editable : Boolean);override;
    property DataSet : TBaseDBDataset read FDataSet write SetDataSet;
  end; 

resourcestring
  strCalc                             = 'Berechnung';

implementation
{$R *.lfm}
uses uPositionFrame,uBaseERPDBClasses;

procedure TfCalcPositionFrame.SetDataSet(AValue: TBaseDBDataset);
begin
  if FDataSet=AValue then Exit;
  FDataSet:=AValue;
  Position.DataSet := TBaseDBPosition(FDataSet).DataSet;
  PosCalc.DataSet := TBaseDBPosition(FDataSet).PosCalc.DataSet;
end;

procedure TfCalcPositionFrame.SetRights(Editable: Boolean);
begin
  Enabled := Editable;
end;

end.

