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

unit uAddSerial;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,  Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons, ExtCtrls,
  uData,db,LCLtype, ButtonPanel;

type

  { TfAddSerial }

  TfAddSerial = class(TForm)
    ButtonPanel1: TButtonPanel;
    lArticle: TLabel;
    cbSerial: TComboBox;
    procedure bAbortClick(Sender: TObject);
    procedure bOKClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    function Execute(AddSerial : Boolean) : Boolean;
  end;

var
  fAddSerial: TfAddSerial;

implementation
{$R *.lfm}
resourcestring
  strAddSerialToArticle         = 'Please select Serial for %s Version %s Name %s';

procedure TfAddSerial.bAbortClick(Sender: TObject);
begin

end;

procedure TfAddSerial.bOKClick(Sender: TObject);
begin

end;


{ TfAddSerial }

procedure TfAddSerial.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
    begin
      Key := 0;
      Close;
    end;
end;

procedure TfAddSerial.FormShow(Sender: TObject);
begin
  cbSerial.Text:='';
end;

function TfAddSerial.Execute(AddSerial: Boolean): Boolean;
var
  ActControl: TWinControl;
  OK: Boolean;
begin
  cbSerial.Items.Clear;
  //Add Serials to List
  if not AddSerial then
    begin
      Data.Serials.DataSet.First;
      while not Data.Serials.DataSet.EOF do
        begin
          cbSerial.Items.Add(Data.Serials.FieldByName('SERIAL').AsString);
          Data.Serials.DataSet.Next;
        end;
    end;
  lArticle.Caption := Format(strAddSerialToArticle,[Data.Masterdata.FieldByName('ID').AsString,Data.Masterdata.FieldByName('VERSION').AsString,Data.Masterdata.FieldByName('SHORTTEXT').AsString]);
  if Data.OrderPos.FieldByName('SERIAL').IsNull then
    begin
      ActControl := Screen.ActiveControl;
      OK := Showmodal = mrOK;
      try
        if Assigned(ActControl) and ActControl.CanFocus then ActControl.SetFocus;
      except
      end;
      Result := OK;
      if not Result then exit;
      if AddSerial then
        begin
          Data.Serials.DataSet.Append;
          Data.Serials.FieldByName('SERIAL').AsString := cbSerial.Text;
          Data.Serials.DataSet.Post;
        end
      else
        if Data.Locate(Data.Serials,'SERIAL',cbSerial.Text,[loCaseInsensitive]) then
          begin
            if Data.OrderPos.dataSet.State <> dsEdit then
              Data.OrderPos.DataSet.Edit;
            Data.OrderPos.FieldByName('SERIAL').AsString := Data.Serials.FieldByName('SERIAL').AsString;
            Data.OrderPos.DataSet.Post;
            Data.Serials.DataSet.Delete;
          end
        else
          Result := False;
    end
  else
    begin
      Result := True;
      if AddSerial then
        begin
          Data.Serials.DataSet.Append;
          Data.Serials.FieldByName('SERIAL').AsString := Data.OrderPos.FieldByName('SERIAL').AsString;
          Data.Serials.DataSet.Post;
        end
      else
        if Data.Locate(Data.Serials,'SERIAL',Data.OrderPos.FieldByName('SERIAL').AsString,[loCaseInsensitive]) then
          begin
            if Data.OrderPos.dataSet.State <> dsEdit then
              Data.OrderPos.DataSet.Edit;
            Data.OrderPos.FieldByName('SERIAL').AsString := Data.Serials.FieldByName('SERIAL').AsString;
            Data.OrderPos.DataSet.Post;
            Data.Serials.DataSet.Delete;
          end
        else
          Result := False;
    end;
end;

initialization

end.
