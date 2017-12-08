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
Created 14.11.2017
*******************************************************************************}
unit uselectorder;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, db, FileUtil, LResources, Forms, Controls, Graphics,
  Dialogs, DBGrids, StdCtrls, ButtonPanel;

type

  { TfSelectOrder }

  TfSelectOrder = class(TForm)
    ButtonPanel1: TButtonPanel;
    eStatus: TEdit;
    Label2: TLabel;
    Order: TDataSource;
    gOrder: TDBGrid;
    eFilter: TEdit;
    Label1: TLabel;
    procedure eFilterChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure gOrderDblClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    function Execute : Boolean;
    procedure RefreshSQL;
  end;

var
  fSelectOrder: TfSelectOrder;

implementation

uses uData,uBaseDatasetInterfaces,ubaseconfig,uStatistic;

{ TfSelectOrder }

procedure TfSelectOrder.eFilterChange(Sender: TObject);
begin
  RefreshSQL;
  with Application as IBaseConfig do
    Config.WriteString('Workplace',eFilter.Text);
  with Application as IBaseConfig do
    Config.WriteString('WorkplaceStatus',eStatus.Text);
end;

procedure TfSelectOrder.FormCreate(Sender: TObject);
begin
  with Application as IBaseConfig do
    eFilter.Text:=Config.ReadString('Workplace','');
  with Application as IBaseConfig do
    eStatus.Text:=Config.ReadString('WorkplaceStatus','');
end;

procedure TfSelectOrder.FormDestroy(Sender: TObject);
begin
  Order.DataSet.Free;
end;

procedure TfSelectOrder.gOrderDblClick(Sender: TObject);
begin
  ModalResult:=mrOK;
end;

function TfSelectOrder.Execute: Boolean;
begin
  Result := False;
  if Order.DataSet = nil then
    Order.DataSet := Data.GetNewDataSet('');
  RefreshSQL;
  Result := ShowModal = mrOK;
  if Result then
    begin

    end;
end;

procedure TfSelectOrder.RefreshSQL;
var
  aSql: string;
begin
  if not Assigned(Order.DataSet) then exit;
  aSql :=
   'select distinct '+Data.QuoteField('ORDERS')+'.'+Data.QuoteField('STATUS')+','+Data.QuoteField('ORDERS')+'.'+Data.QuoteField('COMMISSION')+','+Data.QuoteField('ORDERNO')+','+Data.QuoteField('PID')+',OP2.'+Data.QuoteField('QUANTITY')+','+Data.QuoteField('DAPPR')+','+Data.QuoteField('ORDERS')+'.'+Data.QuoteField('TIMESTAMPD')+' from '+Data.QuoteField('ORDERS')
  +' inner join '+Data.QuoteField('ORDERTYPE')+' on '+Data.QuoteField('ORDERTYPE')+'.'+Data.QuoteField('STATUS')+'='+Data.QuoteField('ORDERS')+'.'+Data.QuoteField('STATUS')+' and '+Data.QuoteField('ORDERTYPE')+'.'+Data.QuoteField('TYPE')+'=7'
  +' inner join '+Data.QuoteField('ORDERPOS')+' as OP1 on OP1.'+Data.QuoteField('REF_ID')+'='+Data.QuoteField('ORDERS')+'.'+Data.QuoteField('SQL_ID')+' and OP1.'+Data.QuoteField('IDENT')+' like '''+Data.ProcessTerm(eFilter.Text+'*')+''' and '+Data.QuoteField('ORDERS')+'.'+Data.QuoteField('STATUS')+' like '''+Data.ProcessTerm(eStatus.Text+'*')+''''
  +' left  join '+Data.QuoteField('ORDERPOS')+' as OP2 on OP2.'+Data.QuoteField('REF_ID')+'='+Data.QuoteField('ORDERS')+'.'+Data.QuoteField('SQL_ID')+' and OP2.'+Data.QuoteField('PARENT')+' is NULL'
  +' order by '+Data.QuoteField('ORDERS')+'.'+Data.QuoteField('DAPPR');
  AddSQLLimit(aSQL,50);
  with Order.DataSet as IBaseDbFilter do
    begin
      if FullSQL <> aSQL then
        FullSQL := aSQL
      else Order.DataSet.Refresh;
    end;
  Order.DataSet.Open;
end;

initialization
  {$I uselectorder.lrs}

end.

