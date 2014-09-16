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
  ButtonPanel, Buttons, ActnList,uBaseDbClasses, types;
type
  TfHistoryAddItem = class(TForm)
    acAdd: TAction;
    acDelete: TAction;
    acClose: TAction;
    ActionList1: TActionList;
    ButtonPanel1: TButtonPanel;
    eAction: TMemo;
    eReference: TEdit;
    Label1: TLabel;
    lbAdditional: TListBox;
    lReference: TLabel;
    lAction: TLabel;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    procedure acAddExecute(Sender: TObject);
    procedure acCloseExecute(Sender: TObject);
    procedure acDeleteExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    function fSearchOpenItem(aLink: string): Boolean;
    procedure lbAdditionalDrawItem(Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
    procedure lbAdditionalSelectionChange(Sender: TObject; User: boolean);
  private
    { private declarations }
  public
    { public declarations }
    function Execute(aDataSet : TBaseDBDataSet = nil) : Boolean;
    procedure ExecuteUnmodal(aDataSet : TBaseDBDataSet = nil);
  end;
var
  fHistoryAddItem: TfHistoryAddItem;
implementation
uses uSearch,uData;
{$R *.lfm}
procedure TfHistoryAddItem.FormCreate(Sender: TObject);
begin
  eReference.Clear;
end;

procedure TfHistoryAddItem.acAddExecute(Sender: TObject);
begin
  fSearch.SetLanguage;
  fSearch.OnOpenItem:=@fSearchOpenItem;
  fSearch.Execute(False,'HISTORYADD','');
end;

procedure TfHistoryAddItem.acCloseExecute(Sender: TObject);
begin
  Close;
end;

procedure TfHistoryAddItem.acDeleteExecute(Sender: TObject);
begin
  lbAdditional.Items.Delete(lbAdditional.ItemIndex);
  acDelete.Enabled:=False;
end;

procedure TfHistoryAddItem.FormShow(Sender: TObject);
begin
  eAction.SetFocus;
end;

function TfHistoryAddItem.fSearchOpenItem(aLink: string): Boolean;
var
  aLinks: String;
begin
  aLinks := fSearch.GetLink(true);
  while pos(';',aLinks)>0 do
    begin
      aLink := copy(aLinks,0,pos(';',aLinks)-1);
      aLinks := copy(aLinks,pos(';',aLinks)+1,length(aLinks));
      lbAdditional.Items.Add(aLink);
    end;
end;

procedure TfHistoryAddItem.lbAdditionalDrawItem(Control: TWinControl;
  Index: Integer; ARect: TRect; State: TOwnerDrawState);
begin
  with TListBox(Control).Canvas do
    begin
      FillRect(aRect);
      TextOut(aRect.Left + 1, aRect.Top + 1, Data.GetLinkDesc(TListBox(Control).Items[Index]));
    end;
end;

procedure TfHistoryAddItem.lbAdditionalSelectionChange(Sender: TObject;
  User: boolean);
begin
  acDelete.Enabled:=lbAdditional.ItemIndex>-1;
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

procedure TfHistoryAddItem.ExecuteUnmodal(aDataSet: TBaseDBDataSet);
begin
  if not Assigned(Self) then
    begin
      Application.CreateForm(TfHistoryAddItem,fHistoryAddItem);
      Self := fHistoryAddItem;
    end;
  if aDataSet = nil then
    eAction.Clear
  else eAction.Text:=aDataSet.FieldByName('ACTION').AsString;
  Show;
end;

end.

