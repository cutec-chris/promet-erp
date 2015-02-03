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
Created 07.07.2012
*******************************************************************************}
unit uarticlesupplierframe;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, FileUtil, Forms, Controls, ExtCtrls, DbCtrls, db,
  uFilterFrame,uPrometFramesInplace, uExtControls,DBGrids, StdCtrls;
type

  { TfArticleSupplierFrame }

  TfArticleSupplierFrame = class(TPrometInplaceFrame)
    Bevel1: TBevel;
    cbSupplierTransportCurrency: TDBComboBox;
    eArticleNumberSupplier: TDBEdit;
    eDelivertime: TDBEdit;
    eSupplierTransportPrice: TDBEdit;
    gSupplier: TDBGrid;
    gSupplierPrices: TDBGrid;
    lArticleNumberSupplier: TLabel;
    lDelivertime: TLabel;
    lDelivertime1: TLabel;
    dnNavigator: TDBNavigator;
    ExtRotatedLabel1: TExtRotatedLabel;
    lPrices: TLabel;
    lTransportSupplier: TLabel;
    Panel1: TPanel;
    pToolbar: TPanel;
    Panel2: TPanel;
    Supplier: TDatasource;
    SupplierPrices: TDatasource;
    procedure gSupplierDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure gSupplierDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
  private
    { private declarations }
  public
    { public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy;override;
    procedure SetRights(Editable : Boolean);override;
  end;
implementation
{$R *.lfm}
uses uMainTreeFrame,uSearch,uPerson,uData;
procedure TfArticleSupplierFrame.gSupplierDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Accept := False;
  if Assigned(uMainTreeFrame.fMainTreeFrame)
  and (Source = uMainTreeFrame.fMainTreeFrame.tvMain)
  and ((TTreeEntry(uMainTreeFrame.fMainTreeFrame.tvMain.Selected.Data).Typ = etSupplier)) then
    Accept := True;
  if Assigned(fSearch) and (Source = fSearch.sgResults) then
    begin
      with fSearch.sgResults do
        if copy(fSearch.GetLink,0,9) = 'CUSTOMERS' then
          Accept := True;
    end;
 end;
procedure TfArticleSupplierFrame.gSupplierDragDrop(Sender, Source: TObject; X,
  Y: Integer);
var
  nData: TTreeEntry;
  aAccountno,aName : string;
  aRec: String;
  OldFilter: String;
  aPersons: TPersonList;
begin
  if (Source = fMainTreeFrame.tvMain) then
    begin
      nData := TTreeEntry(fMainTreeFrame.tvMain.Selected.Data);
      aPersons := TPersonList.CreateEx(Self,Data);
      Data.SetFilter(aPersons,nData.Filter);
      aAccountno := aPersons.FieldByName('ACCOUNTNO').AsString;
      aName := aPersons.FieldByName('NAME').AsString;
      aPersons.free;
    end
  else if (Source = fSearch.sgResults) then
    begin
      aPersons := TPersonList.CreateEx(Self,Data);
      Data.SetFilter(aPersons,Data.QuoteField('ACCOUNTNO')+'='+Data.QuoteValue(fSearch.sgResults.Cells[1,fSearch.sgResults.Row]));
      if aPersons.DataSet.Locate('ACCOUNTNO',fSearch.sgResults.Cells[1,fSearch.sgResults.Row],[loCaseInsensitive,loPartialKey]) then
        begin
          aAccountno := aPersons.FieldByName('ACCOUNTNO').AsString;
          aName := aPersons.FieldByName('NAME').AsString;
        end;
      aPersons.Free;
    end
  else exit;
  Supplier.DataSet.Append;
  Supplier.DataSet.FieldByName('ACCOUNTNO').AsString := aAccountno;
  Supplier.DataSet.FieldByName('NAME').AsString := aName;
  Supplier.DataSet.Post;
end;
constructor TfArticleSupplierFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;
destructor TfArticleSupplierFrame.Destroy;
begin
  inherited Destroy;
end;
procedure TfArticleSupplierFrame.SetRights(Editable: Boolean);
begin
  Enabled := Editable;
  ArrangeToolBar(pToolbar,nil,'Supplier');
end;
end.

