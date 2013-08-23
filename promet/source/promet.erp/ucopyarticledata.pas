{*******************************************************************************
Dieser Sourcecode darf nicht ohne gültige Geheimhaltungsvereinbarung benutzt werden
und ohne gültigen Vertriebspartnervertrag weitergegeben oder kommerziell verwertet werden.
You have no permission to use this Source without valid NDA
and copy it without valid distribution partner agreement
CU-TEC Christian Ulrich
info@cu-tec.de
*******************************************************************************}
unit uCopyArticleData;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Buttons, ButtonPanel,uIntfStrConsts,uMasterdata;
type
  TfCopyArticleData = class(TForm)
    ButtonPanel1: TButtonPanel;
    cbCopyPrices: TCheckBox;
    cbCopyProperties: TCheckBox;
    cbCopyTexts: TCheckBox;
    cbCopySupplier: TCheckBox;
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
function TfCopyArticleData.Execute(aMasterdata : TMasterdata;aNewVersion : Variant;aNewLanguage : Variant): Boolean;
begin
  if not Assigned(Self) then
    begin
      Application.CreateForm(TfCopyArticleData,fCopyArticleData);
      Self := fCopyArticleData;
    end;
  fCopyArticleData.cbCopySupplier.Checked:=False;
  fCopyArticleData.cbCopySupplier.Enabled:=True;
  FMasterdata := aMasterdata;
  Result := Showmodal = mrOk;
  if Result then
    Result := FMasterdata.Copy(aNewVersion,aNewLanguage,cbCopyPrices.Checked,cbCopyproperties.Checked,cbCopyTexts.Checked,cbCopySupplier.Checked);
end;
initialization
  {$I ucopyarticledata.lrs}
end.
