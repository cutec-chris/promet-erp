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
unit uSystemOptions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, DBGrids, db,
  uOptionsFrame, uBaseDbClasses, uBaseERPDbClasses;
type
  TfSystemOptions = class(TOptionsFrame)
    gNumbers: TDBGrid;
    gPaymentTargets: TDBGrid;
    gUnits: TDBGrid;
    gVat: TDBGrid;
    lNumbers: TLabel;
    lPaymentTargets: TLabel;
    lUnits: TLabel;
    lVAT: TLabel;
    NumbersDS: TDatasource;
    PaymentTargetsDS: TDatasource;
    UnitsDS: TDatasource;
    VatDS: TDatasource;
  private
    { private declarations }
    aConnection: TComponent;
    aNumbers: TNumbersets;
    aVat: TVat;
    aUnits: TUnits;
    aPaymentTargets: TPaymentTargets;
  public
    { public declarations }
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy;override;
    procedure StartTransaction;override;
    procedure CommitTransaction;override;
    procedure RollbackTransaction;override;
  end;
implementation
{$R *.lfm}
uses uData;
constructor TfSystemOptions.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  aConnection := Data.GetNewConnection;
  aVat := TVat.CreateEx(Self,Data,aConnection);
  VatDS.DataSet := aVat.DataSet;
  aNumbers := TNumbersets.CreateEx(Self,Data,aConnection);
  NumbersDS.DataSet := aNumbers.DataSet;
  aUnits := TUnits.CreateEx(Self,Data,aConnection);
  UnitsDS.DataSet := aUnits.DataSet;
  aPaymentTargets := TPaymentTargets.CreateEx(Self,Data,aConnection);
  PaymentTargetsDS.DataSet := aPaymentTargets.DataSet;
end;
destructor TfSystemOptions.Destroy;
begin
  aNumbers.Destroy;
  aVat.Destroy;
  aUnits.Destroy;
  aPaymentTargets.Destroy;
  aConnection.Destroy;
  inherited Destroy;
end;
procedure TfSystemOptions.StartTransaction;
begin
  inherited StartTransaction;
  Data.StartTransaction(aConnection);
  aVat.Open;
  aUnits.Open;
  aPaymentTargets.Open;
  aNumbers.Open;
end;
procedure TfSystemOptions.CommitTransaction;
begin
  inherited CommitTransaction;
  Data.CommitTransaction(aConnection);
end;
procedure TfSystemOptions.RollbackTransaction;
begin
  Data.RollbackTransaction(aConnection);
  inherited RollbackTransaction;
end;
end.

