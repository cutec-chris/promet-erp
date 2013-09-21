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
Created 10.08.2013
*******************************************************************************}
unit ucategoryoptions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, ComCtrls, StdCtrls, ExtCtrls,
  DbCtrls, DBGrids, Buttons, db, uOptionsFrame, uBaseDBClasses, uBaseERPDBClasses;

type
  TfCategoryOptions = class(TOptionsFrame)
    gCategory: TDBGrid;
    CategoryDS: TDatasource;
  private
    { private declarations }
    aConnection: TComponent;
    aCategory: TCategory;
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
constructor TfCategoryOptions.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  aConnection := Data.GetNewConnection;
  aCategory := TCategory.Create(Self,Data,aConnection);
  CategoryDS.DataSet := aCategory.DataSet;
end;

destructor TfCategoryOptions.Destroy;
begin
  aCategory.Destroy;
  aConnection.Destroy;
  inherited Destroy;
end;

procedure TfCategoryOptions.StartTransaction;
begin
  inherited StartTransaction;
  Data.StartTransaction(aConnection);
  aCategory.Open;
end;

procedure TfCategoryOptions.CommitTransaction;
begin
  inherited CommitTransaction;
  Data.CommitTransaction(aConnection);
end;

procedure TfCategoryOptions.RollbackTransaction;
begin
  Data.RollbackTransaction(aConnection);
  inherited RollbackTransaction;
end;

end.

