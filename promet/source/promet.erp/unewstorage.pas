{*******************************************************************************
Dieser Sourcecode darf nicht ohne gültige Geheimhaltungsvereinbarung benutzt werden
und ohne gültigen Vertriebspartnervertrag weitergegeben werden.
You have no permission to use this Source without valid NDA
and copy it without valid distribution partner agreement
CU-TEC Christian Ulrich
info@cu-tec.de
*******************************************************************************}
unit uNewStorage;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, DBGrids, StdCtrls, ExtCtrls, Buttons,
  uData, db,LCLType, ButtonPanel,uOrder,uMasterdata,uBaseERPDBClasses;
type
  TfNewStorage = class(TForm)
    ButtonPanel1: TButtonPanel;
    StorageType: TDatasource;
    gStorage: TDBGrid;
    lArticlewithoutStorage: TLabel;
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    { private declarations }
  public
    { public declarations }
    function Execute(aOrder : TOrder;aStorage : TStorage) : Boolean;
    procedure SetLanguage;
  end;
var
  fNewStorage: TfNewStorage;
implementation
resourcestring
  strSelectanStorage            = 'Please select an Storage for the Article %s Version %s Name %s';
procedure TfNewStorage.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
    begin
      Key := 0;
      Close;
    end;
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
  aStorageType := TStorageTyp.Create(Self,Data,aStorage.Connection);
  CtrDisabled := aStorageType.DataSet.ControlsDisabled;
  aStorageType.Open;
  StorageType.DataSet := aStorageType.DataSet;
  lArticlewithoutStorage.Caption := Format(strSelectanStorage,[aOrder.Positions.FieldByName('IDENT').AsString,
                                                               aOrder.Positions.FieldByName('VERSION').AsString,
                                                               aOrder.Positions.FieldByName('SHORTTEXT').AsString]);
  Result := ShowModal = mrOK;
  if Result then
    if not aStorage.DataSet.Locate('STORAGEID',aStorageType.FieldByName('ID').AsString,[loCaseInsensitive]) then
      with aStorage.DataSet do
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
  StorageType.DataSet := nil;
  aStorageType.Free;
  FreeAndNil(fNewStorage);
end;
initialization
  {$I unewstorage.lrs}
end.

