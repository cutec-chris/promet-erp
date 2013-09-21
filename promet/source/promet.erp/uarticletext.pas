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
  Buttons, db, uFilterFrame, uMasterdata, uPrometFramesInplace, uExtControls;
type
  TfArticleTextFrame = class(TPrometInplaceFrame)
    Bevel1: TBevel;
    cbTextTyp: TComboBox;
    DBNavigator1: TDBNavigator;
    ExtRotatedLabel1: TExtRotatedLabel;
    Panel2: TPanel;
    pLeft: TPanel;
    TextTypes: TDatasource;
    Texts: TDatasource;
    mText: TDBMemo;
    lTexttyp: TLabel;
    procedure cbTextTypSelect(Sender: TObject);
  private
    FMasterdata: TMasterdata;
    FEditable : Boolean;
    procedure SetMasterdata(const AValue: TMasterdata);
    { private declarations }
  public
    { public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy;override;
    property Masterdata : TMasterdata read FMasterdata write SetMasterdata;
    procedure SetRights(Editable : Boolean);override;
  end;
implementation
{$R *.lfm}
uses uData;
procedure TfArticleTextFrame.cbTextTypSelect(Sender: TObject);
begin
  if not Texts.DataSet.Locate('TEXTTYPE',cbTextTyp.ItemIndex,[]) then
    begin
      if not FEditable then exit;
      Texts.DataSet.Insert;
      Texts.DataSet.FieldByName('TEXTTYPE').AsInteger:=cbTextTyp.ItemIndex;
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
end;
constructor TfArticleTextFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Data.Texttyp.Open;
  TextTypes.DataSet := Data.TextTyp.DataSet;
  Data.TextTyp.DataSet.First;
  while not Data.TextTyp.DataSet.EOF do
    begin
      cbTextTyp.Items.Add(Data.TextTyp.FieldByName('NAME').AsString);
      Data.TextTyp.DataSet.Next;
    end;
end;
destructor TfArticleTextFrame.Destroy;
begin
  inherited Destroy;
end;

procedure TfArticleTextFrame.SetRights(Editable: Boolean);
begin
  FEditable := Editable;
  mText.ReadOnly:=not Editable;
  DBNavigator1.Enabled:=Editable;
end;

end.

