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

unit uDocProperties;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,  Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, ComCtrls, Grids, Menus, uIntfStrConsts,LCLType, ButtonPanel,
  uDocuments;

type

  { TfDocProperties }

  TfDocProperties = class(TForm)
    Bevel1: TBevel;
    Bevel3: TBevel;
    ButtonPanel1: TButtonPanel;
    lLastChangedValue: TLabel;
    llastChanged: TLabel;
    lMimeTypeValue: TLabel;
    lMimeType: TLabel;
    lFilenameValue: TLabel;
    lFilename: TLabel;
    lDate: TLabel;
    lDateValue: TLabel;
    miCheckoutToThisRevision: TMenuItem;
    pcTabs: TPageControl;
    pmLog: TPopupMenu;
    tsLog: TTabSheet;
    tsCommon: TTabSheet;
    sgLog: TStringGrid;
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure miCheckoutToThisRevisionClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    function Execute(aDoc : TDocument) : Boolean;
  end;

var
  fDocProperties: TfDocProperties;

implementation
{$R *.lfm}
{ TfDocProperties }

procedure TfDocProperties.miCheckoutToThisRevisionClick(Sender: TObject);
begin

end;

function TfDocProperties.Execute(aDoc: TDocument): Boolean;
begin
  if not Assigned(Self) then
    begin
      Application.CreateForm(TfDocProperties,fDocProperties);
      Self := fDocProperties;
    end;
  lFilenameValue.Caption:=aDoc.FileName;
  lMimeTypeValue.Caption:='';
  lDateValue.Caption:=DateTimeToStr(aDoc.CreationDate);
  lLastChangedValue.Caption:=DateTimeToStr(aDoc.LastModified);
  with aDoc.DataSet do
    begin
      First;
      sgLog.RowCount:=1;
      repeat
        begin
          sgLog.RowCount:=sgLog.RowCount+1;
          sgLog.Cells[0,sgLog.RowCount-1] := FieldByName('TIMESTAMPD').AsString;
          sgLog.Cells[1,sgLog.RowCount-1] := FieldByName('REVISION').AsString;
          sgLog.Cells[2,sgLog.RowCount-1] := FieldByName('MESSAGE').AsString;
          Next;
        end
      until EOF;
      if sgLog.RowCount>2 then
        sgLog.RowCount:=sgLog.RowCount-1;
    end;
  Result := Showmodal = mrOK;
end;

procedure TfDocProperties.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
    begin
      Key := 0;
      Close;
    end;
end;

initialization

end.
