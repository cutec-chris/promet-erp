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
Created 13.02.2014
*******************************************************************************}
unit uBookSerial;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ButtonPanel,uMasterdata,uOrder;

type
  TfBookSerial = class(TForm)
    ButtonPanel1: TButtonPanel;
    eSerial: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    mComment: TMemo;
  private
    { private declarations }
  public
    { public declarations }
    function Execute(aOrder : TOrder;aMasterdata : TMasterdata;aQuantity : Integer) : Boolean;
  end;

var
  fBookSerial: TfBookSerial;

implementation
resourcestring
  strBookSerial         =   'Scannen Sie die Seriennummer für Position %s, Artikel %s';
{$R *.lfm}

function TfBookSerial.Execute(aOrder: TOrder; aMasterdata: TMasterdata;
  aQuantity: Integer): Boolean;
begin
  if not Assigned(Self) then
    begin
      Application.CreateForm(TfBookSerial,fBookSerial);
      Self := fBookSerial;
    end;
  eSerial.Clear;
  eSerial.SetFocus;
  Label2.Caption:=Format(strBookSerial,[aOrder.Positions.PosNo.AsString,aOrder.Positions.Ident.AsString]);
  result := ShowModal=mrOK;
  if Result then
    begin
      if aQuantity>0 then
        begin
          aMasterdata.Serials.Insert;
          aMasterdata.Serials.FieldByName('SERIAL').AsString:=eSerial.Text;
          aMasterdata.Serials.FieldByName('COMMENT').AsString:=mComment.Text;
          aMasterdata.Serials.Post;
        end
      else
        Result := aMasterdata.Serials.Locate('SERIAL',eSerial.Text,[]);
    end;
end;

end.

