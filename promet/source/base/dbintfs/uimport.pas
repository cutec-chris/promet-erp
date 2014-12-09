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
  aStream: TFileStream;
  aHeader: String = '';
  aRow: String = '';
  aFooter: String = '';
  ActRow: String;
  aOut: String;
  aModifier: String;
  i: Integer;
  aTemplate: String;
begin
  case FieldByName('CLASS').AsString of
  'SQL':
    begin
      aData := Data.GetNewDataSet(FieldByName('FILTER').AsString);
      aData.Open;
    end
  else if Data.DataSetFromLink(lowercase(FieldByName('CLASS').AsString)+'@',aDataSetClass) then
    begin
      aDataSet := aDataSetClass.Create(nil);
      aDataSet.Filter(FieldByName('FILTER').AsString,0);
      aData := aDataSet.DataSet;
    end
  else raise Exception.Create('Class of Import Type not found !');
  end;
  aTemplate := FieldByName('TEMPLATE').AsString;
  aHeader := copy(aTemplate,pos('[head]',lowercase(aTemplate))+6,length(aTemplate));
  aHeader := copy(aHeader,0,pos('[/head]',lowercase(aHeader))-1);
  aRow := copy(aTemplate,pos('[row]',lowercase(aTemplate))+5,length(aTemplate));
  aRow := copy(aRow,0,pos('[/row]',lowercase(aRow))-1);
  aFooter := copy(aTemplate,pos('[footer]',lowercase(aTemplate))+8,length(aTemplate));
  aFooter := copy(aFooter,0,pos('[/footer]',lowercase(aFooter))-1);
  if Assigned(aData) then
    begin
      with BaseApplication as IBaseApplication do
        Info(Format('%d records to export',[aData.RecordCount]));
      aStream := TFileStream.Create(aFilename,fmCreate);
      aStream.WriteBuffer(aHeader[1],length(aHeader));
      while not aData.EOF do
        begin
          ActRow := aRow;
          aOut := '';
          while pos('[[',lowercase(ActRow))>0 do
            begin
              aOut := aOut+copy(ActRow,0,pos('[[',lowercase(ActRow))-1);
              ActRow:=copy(ActRow,pos('[[',lowercase(ActRow))+2,length(ActRow));
              aModifier := copy(ActRow,0,pos(']]',ActRow)-1);
              ActRow:=copy(ActRow,pos(']]',lowercase(ActRow))+2,length(ActRow));
              //Replace Fields aModifier and add to aOut
              for i := 0 to aData.FieldCount-1 do
                aModifier:=StringReplace(aModifier,'field:'+lowercase(aData.Fields[i].FieldName),aData.Fields[i].AsString,[rfIgnoreCase,rfReplaceAll]);
              aOut := aOut+aModifier;
            end;
          aOut := aOut+ActRow;
          aStream.WriteBuffer(aOut[1],length(aOut));
          aData.Next;
        end;
      aStream.WriteBuffer(aFooter[1],length(aFooter));
      aStream.Free;
    end;
  if Assigned(aDataSet) then
    aDataSet.Free
  else if Assigned(aData) then
    aData.Free;
end;

end.

