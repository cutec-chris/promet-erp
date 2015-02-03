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
*******************************************************************************}
unit uNewStorage;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils,  Forms, Controls, Graphics, Dialogs, DBGrids, StdCtrls, ExtCtrls, Buttons,
  uData, db,LCLType, ButtonPanel,uOrder,uMasterdata,uBaseERPDBClasses,LCLProc;
type
  TfNewStorage = class(TForm)
    ButtonPanel1: TButtonPanel;
    Label1: TLabel;
    Storage: TDatasource;
    gStorage1: TDBGrid;
    StorageType: TDatasource;
    gStorage: TDBGrid;
    lArticlewithoutStorage: TLabel;
    procedure aStorageTypeDataSetAfterScroll(DataSet: TDataSet);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    { private declarations }
    fStorage : TStorage;
  public
    { public declarations }
    function Execute(aOrder : TOrder;aStorage : TStorage) : Boolean;
    procedure SetLanguage;
  end;
var
  fNewStorage: TfNewStorage;
implementation
{$R *.lfm}
resourcestring
  strSelectanStorage            = 'wählen Sie ein Lager für Artikel %s Version %s Name %s';
procedure TfNewStorage.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
    begin
      Key := 0;
      Close;
    end;
end;

procedure TfNewStorage.aStorageTypeDataSetAfterScroll(DataSet: TDataSet);
begin
  fStorage.Filter(Data.QuoteField('STORAGEID')+'='+Data.QuoteValue(StorageType.DataSet.FieldByName('ID').AsString));
end;

procedure TfNewStorage.SetLanguage;
begin
  if not Assigned(Self) then
    begin
      Application.CreateForm(TfNewStorage,fNewStorage);
      Self := fNewStorage;
    end;
end;
function TfNewStorage.Execute(aOrder : TOrder;aStorage : TStorage): Boolean;
var
  CtrDisabled: Boolean;
  ActControl: TWinControl;
  aStorageType : TStorageTyp;
begin
  if not Assigned(Self) then
    begin
      Application.CreateForm(TfNewStorage,fNewStorage);
      Self := fNewStorage;
    end;
  fStorage := aStorage;
  aStorageType := TStorageTyp.CreateEx(Self,Data,aStorage.Connection);
  StorageType.DataSet := aStorageType.DataSet;
  aStorageType.DataSet.AfterScroll:=@aStorageTypeDataSetAfterScroll;
  CtrDisabled := aStorageType.DataSet.ControlsDisabled;
  aStorageType.Open;
  Storage.DataSet := aStorage.DataSet;
  lArticlewithoutStorage.Caption := Format(strSelectanStorage,[aOrder.Positions.FieldByName('IDENT').AsString,
                                                               aOrder.Positions.FieldByName('VERSION').AsString,
                                                               aOrder.Positions.FieldByName('SHORTTEXT').AsString]);
  Result := ShowModal = mrOK;
  if Result then
    begin
      if (aStorageType.FieldByName('ID').AsString<>fStorage.FieldByName('STORAGEID').AsString) and (not aStorage.Locate('STORAGEID',aStorageType.FieldByName('ID').AsString,[loCaseInsensitive])) then
        with aStorage do
          begin
            Insert;
            if aStorage.DataSet.FieldDefs.IndexOf('TYPE') > -1 then
              begin
                FieldByName('TYPE').AsVariant := aOrder.Positions.FieldByName('TYPE').AsVariant;
                FieldByName('ID').AsVariant := aOrder.Positions.FieldByName('ID').AsVariant;
                FieldByName('VERSION').AsVariant := aOrder.Positions.FieldByName('VERSION').Asvariant;
                FieldByName('LANGUAGE').AsVariant := aOrder.Positions.FieldByName('LANGUAGE').AsVariant;
              end;
            FieldByName('STORAGEID').AsVariant := aStorageType.FieldByName('ID').AsVariant;
            FieldByName('STORNAME').AsVariant := aStorageType.FieldByName('NAME').AsVariant;
            FieldByName('QUANTITY').AsFloat := 0;
            FieldByName('QUANTITYU').AsString := aOrder.Positions.FieldByName('QUANTITYU').AsString;
            Post;
          end;
    end;
  aStorageType.DataSet.AfterScroll:=nil;
  StorageType.DataSet := nil;
  aStorageType.Free;
  FreeAndNil(fNewStorage);
end;
initialization
end.

