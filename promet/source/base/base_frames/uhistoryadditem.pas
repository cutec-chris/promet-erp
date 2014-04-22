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

info@cu-tec.de
*******************************************************************************}
unit uhistoryadditem;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ButtonPanel,uBaseDbClasses;
type
  TfHistoryAddItem = class(TForm)
    ButtonPanel1: TButtonPanel;
    eAction: TMemo;
    eReference: TEdit;
    lReference: TLabel;
    lAction: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    function Execute(aDataSet : TBaseDBDataSet = nil) : Boolean;
  end; 
var
  fHistoryAddItem: TfHistoryAddItem;
implementation
{$R *.lfm}
procedure TfHistoryAddItem.FormCreate(Sender: TObject);
begin
  eReference.Clear;
end;

procedure TfHistoryAddItem.FormShow(Sender: TObject);
begin
  eAction.SetFocus;
end;

function TfHistoryAddItem.Execute(aDataSet : TBaseDBDataSet = nil): Boolean;
begin
  if not Assigned(Self) then
    begin
      Application.CreateForm(TfHistoryAddItem,fHistoryAddItem);
      Self := fHistoryAddItem;
    end;
  if aDataSet = nil then
    eAction.Clear
  else eAction.Text:=aDataSet.FieldByName('ACTION').AsString;
  Result := Showmodal = mrOK;
end;
end.

