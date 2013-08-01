{*******************************************************************************
Dieser Sourcecode darf nicht ohne gültige Geheimhaltungsvereinbarung benutzt werden
und ohne gültigen Vertriebspartnervertrag weitergegeben oder kommerziell verwertet werden.
You have no permission to use this Source without valid NDA
and copy it without valid distribution partner agreement
Christian Ulrich
info@cu-tec.de
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

