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
  Created 23.05.2014
*******************************************************************************}
unit uimportoptions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, db, FileUtil, SynMemo, SynHighlighterSQL,
  SynHighlighterXML, Forms, Controls, StdCtrls, DbCtrls, DBGrids, Dialogs,
  uOptionsFrame, uimport;

type
  TfImportOptions = class(TOptionsFrame)
    Button1: TButton;
    cbClass: TComboBox;
    Datasource: TDatasource;
    DBGrid1: TDBGrid;
    DBNavigator1: TDBNavigator;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    mFilter: TSynMemo;
    mTemplate: TSynMemo;
    SaveDialog1: TSaveDialog;
    SynSQLSyn1: TSynSQLSyn;
    SynXMLSyn1: TSynXMLSyn;
    procedure Button1Click(Sender: TObject);
    procedure cbClassSelect(Sender: TObject);
    procedure FImportDataSetAfterInsert(DataSet: TDataSet);
    procedure FImportDataSetAfterScroll(DataSet: TDataSet);
    procedure mFilterChange(Sender: TObject);
    procedure mTemplateChange(Sender: TObject);
  private
    { private declarations }
    FImport: TImportTypes;
  public
    { public declarations }
    procedure StartTransaction;override;
  end;

implementation
  uses uData;
{$R *.lfm}
resourcestring
  strImportFailed                       = 'Import fehlgeschlagen !';
  strExportFailed                       = 'Export fehlgeschlagen !';

procedure TfImportOptions.cbClassSelect(Sender: TObject);
begin
  FImport.Filter(Data.QuoteField('CLASS')+'='+Data.QuoteValue(cbClass.Text));
  FImport.Open;
end;

procedure TfImportOptions.Button1Click(Sender: TObject);
begin
  if SaveDialog1.Execute then
    if not FImport.Export(SaveDialog1.FileName) then Showmessage(strExportFailed);
end;

procedure TfImportOptions.FImportDataSetAfterInsert(DataSet: TDataSet);
begin
  FImport.FieldByName('CLASS').AsString:=cbClass.Text;
end;

procedure TfImportOptions.FImportDataSetAfterScroll(DataSet: TDataSet);
begin
  mFilter.Text:=FImport.DataSet.FieldByName('FILTER').AsString;
  mTemplate.Text:=FImport.DataSet.FieldByName('TEMPLATE').AsString;
end;

procedure TfImportOptions.mFilterChange(Sender: TObject);
begin
  FImport.Edit;
  FImport.DataSet.FieldByName('FILTER').AsString := mFilter.Text;
end;

procedure TfImportOptions.mTemplateChange(Sender: TObject);
begin
  FImport.Edit;
  FImport.DataSet.FieldByName('TEMPLATE').AsString := mTemplate.Text;
end;

procedure TfImportOptions.StartTransaction;
begin
  inherited StartTransaction;
  FImport := TImportTypes.Create(nil);
  FImport.CreateTable;
  FImport.DataSet.AfterScroll:=@FImportDataSetAfterScroll;
  FImport.DataSet.AfterInsert:=@FImportDataSetAfterInsert;
  Datasource.DataSet := FImport.DataSet;
end;

end.

