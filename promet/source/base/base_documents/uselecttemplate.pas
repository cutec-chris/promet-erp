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

unit uSelectTemplate;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,  Forms, Controls, Graphics, Dialogs, DBGrids,
  StdCtrls, Buttons, Utils, db, uIntfStrConsts,FileUtil,LCLType, uBaseDbClasses,
  uDocuments;

type

  { TfSelectTemplate }

  TfSelectTemplate = class(TForm)
    cbReplacePlaceholders: TCheckBox;
    DocumentActions: TDatasource;
    gTemplate: TDBGrid;
    bSelect: TBitBtn;
    bAbort: TBitBtn;
    bNew: TBitBtn;
    bDelete: TBitBtn;
    OpenDialog: TOpenDialog;
    procedure bSelectClick(Sender: TObject);
    procedure bAbortClick(Sender: TObject);
    procedure bNewClick(Sender: TObject);
    procedure bDeleteClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    FDataSet: TBaseDbDataSet;
    { private declarations }
    fType : string;
    FDocuments : TDocument;
    OK : Boolean;
    procedure SetDataSet(AValue: TBaseDbDataSet);
  public
    { public declarations }
    property DataSet : TBaseDbDataSet read FDataSet write SetDataSet;
    function Execute(Typ : string;aDocuments : TDocument) : Boolean;
  end; 

var
  fSelectTemplate: TfSelectTemplate;

implementation
{$R *.lfm}
uses uData;

{ TfSelectTemplate }

procedure TfSelectTemplate.bSelectClick(Sender: TObject);
begin
  OK := True;
  Close;
end;

procedure TfSelectTemplate.bAbortClick(Sender: TObject);
begin
  Close;
end;

procedure TfSelectTemplate.bNewClick(Sender: TObject);
var
  Stream: TFileStream;
begin
  if OpenDialog.Execute then
    with DataSet.DataSet do
      begin
        Insert;
        FieldByName('TYPE').AsString := fType;
        FieldByName('NAME').AsString := ExtractFileName(copy(ExtractFileName(OpenDialog.Filename),0,rpos('.',ExtractFileName(OpenDialog.Filename))-1));
        FieldByName('EXTENSION').AsString := copy(ExtractFileName(OpenDialog.Filename),rpos('.',ExtractFileName(OpenDialog.Filename))+1,length(ExtractFileName(OpenDialog.Filename)));
        if Data.IsSQLDb then Post;
        Stream := TFileStream.Create(UniToSys(Opendialog.Filename),fmOpenread);
        Data.StreamToBlobField(Stream,DataSet.DataSet,'DOCUMENT');
        Stream.Free;
      end;
end;

procedure TfSelectTemplate.bDeleteClick(Sender: TObject);
begin
  if MessageDlg(strRealdelete,mtInformation,[mbYes,mbNo],0) = mrYes then
    DataSet.Delete;
end;

procedure TfSelectTemplate.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
    begin
      Key := 0;
      Close;
    end;
end;
procedure TfSelectTemplate.SetDataSet(AValue: TBaseDbDataSet);
begin
  if not Assigned(fSelectTemplate) then
    begin
      Application.CreateForm(TfSelectTemplate,fSelectTemplate);
      Self := fSelectTemplate;
      AValue.CreateTable;
    end;
  if FDataSet=AValue then Exit;
  FDataSet:=AValue;
end;
function TfSelectTemplate.Execute(Typ : string;aDocuments : TDocument): Boolean;
var
  ActControl: TWinControl;
begin
  FDocuments := aDocuments;
  fType := Typ;
  Data.SetFilter(FDataSet,Data.QuoteField('TYPE')+'='+Data.QuoteValue(Typ));
  DocumentActions.DataSet := FDataSet.DataSet;
  FDataSet.Open;
  OK := false;
  ActControl := Screen.ActiveControl;
  Showmodal;
  try
    if Assigned(ActControl) and ActControl.CanFocus then ActControl.SetFocus;
  except
  end;
  Result := OK;
end;

initialization

end.

