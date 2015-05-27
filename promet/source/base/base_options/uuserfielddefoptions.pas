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
Created 23.10.2014
*******************************************************************************}
unit uUserfieldDefOptions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, DBGrids, StdCtrls, db,
  uOptionsFrame, uBaseDBClasses;

type
  TfUserFieldOptions = class(TOptionsFrame)
    gUserfields: TDBGrid;
    lOnlyOnNextStart: TLabel;
    UserfielddefsDS: TDatasource;
    procedure UserfielddefsDSDataChange(Sender: TObject; Field: TField);
    procedure UserfielddefsDSUpdateData(Sender: TObject);
  private
    { private declarations }
    aConnection: TComponent;
    aUserFieldDefs: TUserfielddefs;
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

procedure TfUserFieldOptions.UserfielddefsDSDataChange(Sender: TObject;
  Field: TField);
begin
end;

procedure TfUserFieldOptions.UserfielddefsDSUpdateData(Sender: TObject);
var
  aTable: String;
begin
  aTable := UserfielddefsDS.DataSet.FieldByName('TTABLE').AsString;
  Data.TableVersions.DataSet.Filter:=Data.QuoteField('NAME')+'='+Data.QuoteValue(aTable);
  Data.TableVersions.DataSet.Filtered:=True;
  while not Data.TableVersions.EOF do
    Data.TableVersions.DataSet.Delete;
  Data.TableVersions.DataSet.Filtered:=false;
  while Data.CheckedTables.IndexOf(aTable)>-1 do
    Data.CheckedTables.Delete(Data.CheckedTables.IndexOf(aTable));
end;

constructor TfUserFieldOptions.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  aConnection := Data.GetNewConnection;
  aUserFieldDefs := TUserFieldDefs.CreateEx(Self,Data,aConnection);
  UserFieldDefsDS.DataSet := aUserFieldDefs.DataSet;
end;

destructor TfUserFieldOptions.Destroy;
begin
  aUserFieldDefs.Destroy;
  try
    aConnection.Destroy;
  except
  end;
  inherited Destroy;
end;

procedure TfUserFieldOptions.StartTransaction;
begin
  inherited StartTransaction;
  Data.StartTransaction(aConnection);
  aUserFieldDefs.Open;
end;

procedure TfUserFieldOptions.CommitTransaction;
begin
  Data.CommitTransaction(aConnection);
  inherited CommitTransaction;
end;

procedure TfUserFieldOptions.RollbackTransaction;
begin
  Data.RollbackTransaction(aConnection);
  inherited RollbackTransaction;
end;

end.

