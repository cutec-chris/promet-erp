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
unit uCopyArticleData;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils,  Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Buttons, ButtonPanel,uIntfStrConsts,uMasterdata;
type
  TfCopyArticleData = class(TForm)
    ButtonPanel1: TButtonPanel;
    cbCopyPrices: TCheckBox;
    cbCopyProperties: TCheckBox;
    cbCopyTexts: TCheckBox;
    cbCopySupplier: TCheckBox;
    cbActivate: TCheckBox;
    lFeaturesforNewArticleversion: TLabel;
  private
    { private declarations }
    FMasterdata : TMasterdata;
  public
    { public declarations }
    function Execute(aMasterdata : TMasterdata;aNewVersion : Variant;aNewLanguage : Variant) : Boolean;
  end;
var
  fCopyArticleData: TfCopyArticleData;
implementation
{$R *.lfm}
function TfCopyArticleData.Execute(aMasterdata : TMasterdata;aNewVersion : Variant;aNewLanguage : Variant): Boolean;
begin
  if not Assigned(Self) then
    begin
      Application.CreateForm(TfCopyArticleData,fCopyArticleData);
      Self := fCopyArticleData;
    end;
  cbActivate.Visible:=(aNewLanguage=Null);
  fCopyArticleData.cbCopySupplier.Checked:=False;
  fCopyArticleData.cbCopySupplier.Enabled:=True;
  FMasterdata := aMasterdata;
  Result := Showmodal = mrOk;
  if Result then
    begin
      if aNewLanguage<>Null then
        Result := FMasterdata.Copy(aNewVersion,aNewLanguage,cbCopyPrices.Checked,cbCopyproperties.Checked,cbCopyTexts.Checked,cbCopySupplier.Checked)
      else
        Result := FMasterdata.Versionate(aNewVersion,cbActivate.Checked,cbCopyPrices.Checked,cbCopyproperties.Checked,cbCopyTexts.Checked,cbCopySupplier.Checked)
    end;
end;
initialization
end.

