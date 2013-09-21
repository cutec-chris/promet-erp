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
Created 01.06.2006
*******************************************************************************}
unit uOrderAdditionalFrame;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, DbCtrls, DBGrids, db,
  uExtControls, uOrder, uPrometFramesInplace;
type
  TfOrderAdditionalFrame = class(TPrometInplaceFrame)
    cbShipping: TExtDBCombobox;
    cbStorage: TExtDBCombobox;
    eCommission: TDBEdit;
    Order: TDatasource;
    lCommission: TLabel;
    lShipping: TLabel;
    lStorage: TLabel;
  private
    { private declarations }
  public
    { public declarations }
    function IsNeeded(DataSet : TDataSet) : Boolean;
    procedure InitOrder(aOrder : TOrder);
    procedure Setrights(Editable : Boolean);override;
  end; 
implementation
uses uData;
{$R *.lfm}
function TfOrderAdditionalFrame.IsNeeded(DataSet: TDataSet): Boolean;
begin
  Result := not (DataSet.FieldByName('COMMISSION').IsNull
             and DataSet.FieldByName('SHIPPING').IsNull
             and DataSet.FieldByName('STORAGE').IsNull);
end;

procedure TfOrderAdditionalFrame.InitOrder(aOrder: TOrder);
begin
  Order.DataSet := nil;
  with Data.Dispatchtypes.DataSet do
    begin
      Data.SetFilter(Data.Dispatchtypes,'UPPER("COUNTRY")=Upper('''+trim(copy(aOrder.Address.FieldByName('COUNTRY').AsString,0,3))+''')');
      cbShipping.Items.Clear;
      First;
      while not EOF do
        begin
          cbShipping.Items.Add(FieldbyName('ID').AsString+' '+FieldbyName('NAME').AsString);
          Next;
        end;
    end;
  cbStorage.Clear;
  Data.StorageType.Open;
  with Data.StorageType.DataSet do
    begin
      First;
      while not Eof do
        begin
          cbStorage.Items.Add(Format('%-3s %s',[FieldByName('ID').AsString,FieldByName('NAME').AsString]));
          next;
        end;
    end;
  Order.DataSet := aOrder.DataSet;
end;

procedure TfOrderAdditionalFrame.Setrights(Editable: Boolean);
begin
  Self.Enabled:=Editable;
end;

end.

