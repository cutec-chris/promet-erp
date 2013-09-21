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

