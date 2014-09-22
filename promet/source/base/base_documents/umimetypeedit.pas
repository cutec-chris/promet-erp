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
unit uMimeTypeEdit;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils,  Forms, Controls, Graphics, Dialogs, StdCtrls,
  EditBtn, ExtCtrls, Buttons, uIntfStrConsts, DBGrids, DBCtrls, uData, LCLType,
  ButtonPanel, db,uDocuments;
type

  { TfMimeTypeEdit }

  TfMimeTypeEdit = class(TForm)
    ButtonPanel1: TButtonPanel;
    DBNavigator1: TDBNavigator;
    Label1: TLabel;
    MimeTypes: TDatasource;
    eOpenWith: TDBEdit;
    gMimes: TDBGrid;
    lOpenWith: TLabel;
    lMimeTypes: TLabel;
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
    FDocuments : TDocument;
  public
    { public declarations }
    function Execute(aDocuments : TDocument) : Boolean;
    procedure SetupDB;
    procedure SetLanguage;
  end; 

var
  fMimeTypeEdit: TfMimeTypeEdit;

implementation
{$R *.lfm}
procedure TfMimeTypeEdit.FormShow(Sender: TObject);
begin
  SetupDB;
end;
procedure TfMimeTypeEdit.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
    begin
      Key := 0;
      Close;
    end;
end;
function TfMimeTypeEdit.Execute(aDocuments : TDocument): Boolean;
var
  ActControl: TWinControl;
begin
  FDocuments := aDocuments;
  MimeTypes.DataSet := FDocuments.MimeTypes.DataSet;
  ActControl := Screen.ActiveControl;
  Result := Showmodal = mrOK;
  try
    if Assigned(ActControl) and ActControl.CanFocus then ActControl.SetFocus;
  except
  end;
end;
procedure TfMimeTypeEdit.SetupDB;
begin
end;
procedure TfMimeTypeEdit.SetLanguage;
begin
  if not Assigned(Self) then
    begin
      Application.CreateForm(TfMimeTypeEdit,fMimeTypeEdit);
      Self := fMimeTypeEdit;
    end;
end;
initialization
end.
