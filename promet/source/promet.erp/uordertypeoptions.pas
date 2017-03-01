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
unit uOrderTypeOptions;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, FileUtil, DividerBevel, Forms, Controls, StdCtrls, DBGrids,
  Buttons, DbCtrls, ExtCtrls, ComCtrls, uExtControls, db, uBaseDBClasses,
  uBaseERPDBClasses, uOptionsFrame, uOrder;
type

  { TforderTypeOptions }

  TforderTypeOptions = class(TOptionsFrame)
    cbIsDerivate: TDBCheckBox;
    cbTextTyp: TComboBox;
    CheckBox1: TDBCheckBox;
    CheckBox2: TDBCheckBox;
    CheckBox3: TDBCheckBox;
    CheckBox4: TDBCheckBox;
    CheckBox5: TDBCheckBox;
    DBNavigator1: TDBNavigator;
    eDefaultPosTyp: TDBEdit;
    eDerivates: TDBEdit;
    eNumberset: TDBEdit;
    eOrderType: TExtDBCombobox;
    gOrderStatus: TDBGrid;
    Label2: TLabel;
    lDefaultPosTyp: TLabel;
    lDerivates: TLabel;
    lNumberset: TLabel;
    lType: TLabel;
    OrderTypeDS: TDatasource;
    PageControl1: TPageControl;
    rgAddCHist: TDBRadioGroup;
    rgAddDunning: TDBRadioGroup;
    rgAddINVO: TDBRadioGroup;
    rgAddINVR: TDBRadioGroup;
    rgAddJournal: TDBRadioGroup;
    rgAddreserved: TDBRadioGroup;
    rgAddSerials: TDBRadioGroup;
    rgAddStorage: TDBRadioGroup;
    rgAddToMainorder: TDBRadioGroup;
    tsDetails1: TTabSheet;
    tsDetails2: TTabSheet;
    tsDetails3: TTabSheet;
    tsPost: TTabSheet;
    tsDetails: TTabSheet;
    tsCommon: TTabSheet;
    procedure cbTextTypChange(Sender: TObject);
    procedure lvImagesSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
  private
    { private declarations }
    aConnection: TComponent;
    aStates: TOrderTyp;
  public
    { public declarations }
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy;override;
    procedure StartTransaction;override;
    procedure CommitTransaction;override;
    procedure RollbackTransaction;override;
  end;

implementation
{$R *.lfm}
uses uData,uBaseVisualControls;
procedure TforderTypeOptions.cbTextTypChange(Sender: TObject);
begin
  if not aStates.CanEdit then aStates.DataSet.Edit;
  aStates.FieldByName('TEXTTYP').AsInteger := cbTextTyp.ItemIndex;
  if cbTextTyp.ItemIndex = -1 then
    aStates.FieldByName('TEXTTYP').Clear;
end;

procedure TforderTypeOptions.lvImagesSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
begin
  if not Selected then exit;
  if not (OrderTypeDS.DataSet.State=dsEdit) then
    OrderTypeDS.DataSet.Edit;
  OrderTypeDS.DataSet.FieldByName('ICON').AsInteger:=lvImages.Selected.ImageIndex;
end;

constructor TforderTypeOptions.Create(TheOwner: TComponent);
var
  aItem: TListItem;
  i: Integer;
begin
  inherited Create(TheOwner);
  aConnection := Data.GetNewConnection;
  aStates := TOrderTyp.CreateEx(Self,Data,aConnection);
  OrderTypeDS.DataSet := aStates.DataSet;
  if not Assigned(TextTyp) then TextTyp := TTextTypes.Create(Data);
  TextTyp.CreateTable;
  Texttyp.Open;
  TextTyp.DataSet.First;
  while not TextTyp.DataSet.EOF do
    begin
      cbTextTyp.Items.Add(TextTyp.FieldByName('NAME').AsString);
      TextTyp.DataSet.Next;
    end;
  with fVisualControls.StatusImages do
    begin
      for i := 0 to fVisualControls.StatusImages.Count-1 do
        begin
          aItem := lvImages.Items.Add;
          aItem.Caption:='';
          aItem.ImageIndex:=i;
        end;
    end;
end;
destructor TforderTypeOptions.Destroy;
begin
  aStates.Destroy;
  aConnection.Destroy;
  inherited Destroy;
end;
procedure TforderTypeOptions.StartTransaction;
begin
  inherited StartTransaction;
  Data.StartTransaction(aConnection);
  aStates.Open;
end;
procedure TforderTypeOptions.CommitTransaction;
begin
  aStates.CascadicPost;
  Data.CommitTransaction(aConnection);
  inherited CommitTransaction;
end;
procedure TforderTypeOptions.RollbackTransaction;
begin
  aStates.CascadicCancel;
  Data.RollbackTransaction(aConnection);
  inherited RollbackTransaction;
end;
end.

