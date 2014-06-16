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
Created 26.05.2014
*******************************************************************************}
unit uimport;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,uBaseDbClasses,db,uBaseDBInterface;

type

  { TImportTypes }

  TImportTypes = class(TBaseDBDataset)
  public
    procedure DefineFields(aDataSet: TDataSet); override;
    procedure SelectByName(aName : string);
    function Export(aFilename : string) : Boolean;
  end;

implementation
uses uBaseApplication,uMasterdata,uPerson,uData;

procedure TImportTypes.DefineFields(aDataSet: TDataSet);
begin
  with aDataSet as IBaseManageDB do
    begin
      TableName := 'IMPORTTYPES';
      if Assigned(ManagedFieldDefs) then
        with ManagedFieldDefs do
          begin
            Add('CLASS',ftString,200,True);
            Add('TYPE',ftString,200,True);
            Add('FILTER',ftMemo,0,False);
            Add('TEMPLATE',ftMemo,0,False);
          end;
    end;
end;

procedure TImportTypes.SelectByName(aName: string);
var
  aFilter: String;
begin
  with BaseApplication as IBaseDBInterface do
    with DataSet as IBaseDBFilter do
      begin
        aFilter := TBaseDBModule(DataModule).QuoteField('TYPE')+'='+TBaseDBModule(DataModule).QuoteValue(aName);
        Filter := aFilter;
      end;
end;

function TImportTypes.Export(aFilename: string): Boolean;
var
  aDataSetClass: TBaseDBDatasetClass;
  aData: TDataSet = nil;
  aDataSet: TBaseDBDataset = nil;
begin
  case FieldByName('CLASS').AsString of
  'SQL':
    begin
      aData := Data.GetNewDataSet(FieldByName('FILTER').AsString);
      aData.Open;
    end
  else if Data.DataSetFromLink(lowercase(FieldByName('CLASS').AsString)+'@',aDataSetClass) then
    begin
      aDataSet := aDataSetClass.Create(nil,Data);
      aDataSet.Filter(FieldByName('FILTER').AsString,0);
      aData := aDataSet.DataSet;
    end
  else raise Exception.Create('Class of Import Type not found !');
  end;
  if Assigned(aData) then
    begin
      with BaseApplication as IBaseApplication do
        Info(Format('%d records to export',[aData.RecordCount]));
    end;
  if Assigned(aDataSet) then
    aDataSet.Free
  else if Assigned(aData) then
    aData.Free;
end;

end.

