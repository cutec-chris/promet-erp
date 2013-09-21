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
unit uStorageTypeOptions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, DBGrids, StdCtrls, db,
  uOptionsFrame, uBaseDBClasses, uBaseERPDBClasses, uMasterdata;

type
  TfStorageTypeOptions = class(TOptionsFrame)
    gStorage: TDBGrid;
    lStorage: TLabel;
    StorageTypeDS: TDatasource;
  private
    { private declarations }
    aConnection: TComponent;
    aStorageType: TStorageTypes;
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
constructor TfStorageTypeOptions.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  aConnection := Data.GetNewConnection;
  aStorageType := TStorageTypes.Create(Self,Data,aConnection);
  StorageTypeDS.DataSet := aStorageType.DataSet;
end;

destructor TfStorageTypeOptions.Destroy;
begin
  aStorageType.Destroy;
  aConnection.Destroy;
  inherited Destroy;
end;

procedure TfStorageTypeOptions.StartTransaction;
begin
  inherited StartTransaction;
  Data.StartTransaction(aConnection);
  aStorageType.Open;
end;

procedure TfStorageTypeOptions.CommitTransaction;
begin
  Data.CommitTransaction(aConnection);
  inherited CommitTransaction;
end;

procedure TfStorageTypeOptions.RollbackTransaction;
begin
  Data.RollbackTransaction(aConnection);
  inherited RollbackTransaction;
end;

end.

