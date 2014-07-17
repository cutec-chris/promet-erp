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
unit ufinance;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, db, FileUtil, Forms, Controls, EditBtn, StdCtrls, DbCtrls,
  uPrometFramesInplaceDB, uProjects,uBaseDbClasses;

type
  TfFinance = class(TPrometInplaceDBFrame)
    DataSource: TDataSource;
    cbAccount: TDBComboBox;
    cbCostcentre: TDBComboBox;
    DBMemo1: TDBMemo;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
  private
    { private declarations }
  public
    { public declarations }
    procedure SetDataSet(const AValue: TBaseDBDataSet);override;
    procedure SetRights(Editable : Boolean);override;
  end;

implementation

{$R *.lfm}

procedure TfFinance.SetDataSet(const AValue: TBaseDBDataSet);
begin
  inherited SetDataSet(AValue);
  DataSource.DataSet := FDataSet.DataSet;
end;

procedure TfFinance.SetRights(Editable: Boolean);
begin
  Enabled := Editable;
end;

end.

