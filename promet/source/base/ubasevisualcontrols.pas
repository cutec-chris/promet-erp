{*******************************************************************************
Dieser Sourcecode darf nicht ohne gültige Geheimhaltungsvereinbarung benutzt werden
und ohne gültigen Vertriebspartnervertrag weitergegeben werden.
You have no permission to use this Source without valid NDA
and copy it without valid distribution partner agreement
Christian Ulrich
info@cu-tec.de
Created 01.06.2006
*******************************************************************************}
unit uBaseVisualControls;
{$mode objfpc}{$H+}
interface
uses
  Classes, Controls;
type
  TfVisualControls = class(TDataModule)
    HistoryImages: TImageList;
    ImageListBig: TImageList;
    Images: TImageList;
  private
    { private declarations }
  public
    { public declarations }
  end;
var
  fVisualControls: TfVisualControls;

implementation

{$R *.lfm}

end.
