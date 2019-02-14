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
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, DBGrids, Buttons,
  ActnList, db, uOptionsFrame, uBaseDbClasses, uBaseERPDbClasses;
type

  { TfSystemOptions }

  TfSystemOptions = class(TOptionsFrame)
    acNumberpools: TAction;
    acNumbersets: TAction;
    acNewNumberset: TAction;
    ActionList1: TActionList;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    TextsDS: TDataSource;
    gNumbers: TDBGrid;
    gPaymentTargets: TDBGrid;
    gUnits: TDBGrid;
    gUnits1: TDBGrid;
    gVat: TDBGrid;
    lNumbers: TLabel;
    lPaymentTargets: TLabel;
    lUnits: TLabel;
    lUnits1: TLabel;
    lVAT: TLabel;
    NumbersDS: TDatasource;
    PaymentTargetsDS: TDatasource;
    UnitsDS: TDatasource;
    VatDS: TDatasource;
    procedure acNewNumbersetExecute(Sender: TObject);
  private
    { private declarations }
    aConnection: TComponent;
    aNumbers: TNumbersets;
    aVat: TVat;
    aUnits: TUnits;
    aPaymentTargets: TPaymentTargets;
    aTexts: TTextTypes;
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
uses uData,unewnumberset;

procedure TfSystemOptions.acNewNumbersetExecute(Sender: TObject);
begin
  if fNewNumberset.Execute then
    begin

    end;
end;

constructor TfSystemOptions.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  aConnection := Data.GetNewConnection;
  aVat := TVat.CreateEx(Self,Data,aConnection);
  VatDS.DataSet := aVat.DataSet;
  aNumbers := TNumbersets.CreateEx(nil,Data,aConnection);
  NumbersDS.DataSet := aNumbers.DataSet;
  aUnits := TUnits.CreateEx(Self,Data,aConnection);
  UnitsDS.DataSet := aUnits.DataSet;
  aPaymentTargets := TPaymentTargets.CreateEx(Self,Data,aConnection);
  PaymentTargetsDS.DataSet := aPaymentTargets.DataSet;
  aTexts := TTextTypes.CreateEx(nil,Data,aConnection);
  TextsDS.DataSet := aTexts.DataSet;
end;
destructor TfSystemOptions.Destroy;
begin
  try
    aTexts.Destroy;
    aNumbers.Destroy;
    aConnection.Destroy;
    inherited Destroy;
  except
  end;
end;
procedure TfSystemOptions.StartTransaction;
begin
  inherited StartTransaction;
  Data.StartTransaction(aConnection);
  aVat.Open;
  aUnits.Open;
  aPaymentTargets.Open;
  aNumbers.Open;
  aTexts.Open;
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

