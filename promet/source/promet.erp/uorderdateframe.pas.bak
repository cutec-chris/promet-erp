{*******************************************************************************
Dieser Sourcecode darf nicht ohne gültige Geheimhaltungsvereinbarung benutzt werden
und ohne gültigen Vertriebspartnervertrag weitergegeben werden.
You have no permission to use this Source without valid NDA
and copy it without valid distribution partner agreement
Christian Ulrich
info@cu-tec.de
Created 01.06.2006
*******************************************************************************}
unit uOrderDateFrame;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, EditBtn, DbCtrls,
  DBZVDateTimePicker, db, uOrder,uPrometFramesInplace;
type
  TfOrderDateFrame = class(TPrometInplaceFrame)
    deApproved: TDBZVDateTimePicker;
    deOrigDate: TDBZVDateTimePicker;
    deRequest: TDBZVDateTimePicker;
    deWishDate: TDBZVDateTimePicker;
    Orders: TDatasource;
    dblDelivered: TDBText;
    dblPayed: TDBText;
    lApproved: TLabel;
    lDelivered: TLabel;
    lOrigDate: TLabel;
    lPayed: TLabel;
    lRequest: TLabel;
    lWishDate: TLabel;
  private
    { private declarations }
  public
    { public declarations }
    function IsNeeded(DataSet : TDataSet) : Boolean;
    procedure SetRights(Editable : Boolean);override;
  end;

implementation
{$R *.lfm}
function TfOrderDateFrame.IsNeeded(DataSet: TDataSet): Boolean;
begin
  Result := not (DataSet.FieldByName('DWISH').IsNull
             and DataSet.FieldByName('DAPPR').IsNull
             and DataSet.FieldByName('PAYEDON').IsNull
             and DataSet.FieldByName('DELIVEREDON').IsNull
             and DataSet.FieldByName('STORAGE').IsNull);
  Orders.DataSet := DataSet;
end;

procedure TfOrderDateFrame.SetRights(Editable: Boolean);
begin
  deRequest.Enabled := Editable;
  deOrigdate.Enabled := Editable;
  deApproved.Enabled := Editable;
end;

end.

