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
unit uArticleText;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, DbCtrls, ExtCtrls,
  Buttons, DBGrids, db, uFilterFrame, uMasterdata, uPrometFramesInplace,
  uEditorToolbar, kmemo, uExtControls;
type

  { TfArticleTextFrame }

  TfArticleTextFrame = class(TPrometInplaceFrame)
    Bevel1: TBevel;
    Bevel2: TBevel;
    cbTextTyp: TComboBox;
    DBNavigator1: TDBNavigator;
    dsTextTypes: TDataSource;
    ExtRotatedLabel1: TExtRotatedLabel;
    ExtRotatedLabel2: TExtRotatedLabel;
    fEditorToolbar1: TfEditorToolbar;
    KMemo1: TKMemo;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    pToolbar: TPanel;
    TextTypes: TDatasource;
    Texts: TDatasource;
    lTexttyp: TLabel;
    procedure cbTextTypSelect(Sender: TObject);
    procedure mTextChange(Sender: TObject);
    procedure TextsDataChange(Sender: TObject; Field: TField);
  private
    FMasterdata: TMasterdata;
    DontUpdate: Boolean;
    FEditable : Boolean;
    procedure SetMasterdata(const AValue: TMasterdata);
    { private declarations }
  public
    { public declarations }
    constructor Create(AOwner: TComponent); override;
    property Masterdata : TMasterdata read FMasterdata write SetMasterdata;
    procedure SetRights(Editable : Boolean);override;
  end;
implementation
{$R *.lfm}
uses uData,uRTFtoTXT,uBaseERPDBClasses;
procedure TfArticleTextFrame.cbTextTypSelect(Sender: TObject);
begin
  if DontUpdate then exit;
  DontUpdate := True;
  KMemo1.Clear;
  if not Texts.DataSet.Active then exit;
  if Texts.DataSet.State=dsInsert then
    Texts.DataSet.Cancel;
  if not Texts.DataSet.Locate('TEXTTYPE',cbTextTyp.ItemIndex,[]) then
    begin
      if not FEditable then
        begin
          DontUpDate := False;
          exit;
        end;
      Texts.DataSet.Insert;
      Texts.DataSet.FieldByName('TEXTTYPE').AsInteger:=cbTextTyp.ItemIndex;
    end;
  //-TODO:Support RTF
  DontUpDate := False;
  TextsDataChange(nil,Texts.DataSet.FieldByName('TEXT'));
end;

procedure TfArticleTextFrame.mTextChange(Sender: TObject);
begin
  if DontUpdate then exit;
  if not ((Texts.DataSet.State=dsEdit) or (Texts.DataSet.State=dsInsert)) then
    Texts.DataSet.Edit;
  //TODO:check if its real RTF else write just in Text
  DontUpdate:=True;
  if Texts.DataSet.FieldByName('TEXTTYPE').AsInteger<>cbTextTyp.ItemIndex then
    begin
      Texts.DataSet.Insert;
      Texts.DataSet.FieldByName('TEXTTYPE').AsInteger:=cbTextTyp.ItemIndex;
    end;
  Texts.DataSet.FieldByName('TEXT').AsString := KMemo1.RTF;
  DontUpdate:=False;
end;

procedure TfArticleTextFrame.TextsDataChange(Sender: TObject; Field: TField);
var
  ss: TStringStream;
begin
  if DontUpdate then exit;
  if not Assigned(Field) then exit;
  if Field.FieldName='TEXT' then
    begin
      DontUpdate:=True;
      ss := TStringStream.Create('');
      Data.BlobFieldToStream(Field.DataSet,'TEXT',ss);
      ss.Position:=0;
      KMemo1.LoadFromRTFStream(ss);
      ss.Free;
      DontUpdate:=False;
    end;
end;

procedure TfArticleTextFrame.SetMasterdata(const AValue: TMasterdata);
begin
  if FMasterdata=AValue then exit;
  FMasterdata:=AValue;
  Texts.DataSet := FMasterdata.Texts.DataSet;
  if Texts.DataSet.RecordCount > 0 then
    cbTextTyp.ItemIndex := TExts.DataSet.FieldByName('TEXTTYPE').AsInteger
  else
    cbTextTyp.Text := '';
  if Fmasterdata.DataSet.Active and (not Masterdata.Texts.DataSet.Active) then
    FMasterdata.Texts.Open;
  cbTextTypSelect(nil);
end;
constructor TfArticleTextFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  if not Assigned(uBaseERPDBClasses.TextTyp) then
    uBaseERPDBClasses.TextTyp := TTextTypes.Create(nil);
  Texttyp.Open;
  TextTyp.DataSet.First;
  while not TextTyp.DataSet.EOF do
    begin
      cbTextTyp.Items.Add(TextTyp.FieldByName('NAME').AsString);
      TextTyp.DataSet.Next;
    end;
  fEditorToolbar1.Editor := KMemo1;
end;

procedure TfArticleTextFrame.SetRights(Editable: Boolean);
begin
  FEditable := Editable;
  KMemo1.ReadOnly:=not Editable;
  DBNavigator1.Enabled:=Editable;
  //cbTextTypSelect(nil);
  ArrangeToolBar(pToolbar,nil,'Text');
end;

end.

