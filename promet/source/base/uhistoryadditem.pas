{*******************************************************************************
Dieser Sourcecode darf nicht ohne gültige Geheimhaltungsvereinbarung benutzt werden
und ohne gültigen Vertriebspartnervertrag weitergegeben werden.
You have no permission to use this Source without valid NDA
and copy it without valid distribution partner agreement
CU-TEC Christian Ulrich
info@cu-tec.de
*******************************************************************************}
unit uhistoryadditem;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ButtonPanel,uBaseDbClasses;
type
  TfHistoryAddItem = class(TForm)
    ButtonPanel1: TButtonPanel;
    eAction: TMemo;
    eReference: TEdit;
    lReference: TLabel;
    lAction: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    function Execute(aDataSet : TBaseDBDataSet = nil) : Boolean;
  end; 
var
  fHistoryAddItem: TfHistoryAddItem;
implementation
{$R *.lfm}
procedure TfHistoryAddItem.FormCreate(Sender: TObject);
begin
  eReference.Clear;
end;

procedure TfHistoryAddItem.FormShow(Sender: TObject);
begin
  eAction.SetFocus;
end;

function TfHistoryAddItem.Execute(aDataSet : TBaseDBDataSet = nil): Boolean;
begin
  if not Assigned(Self) then
    begin
      Application.CreateForm(TfHistoryAddItem,fHistoryAddItem);
      Self := fHistoryAddItem;
    end;
  if aDataSet = nil then
    eAction.Clear
  else eAction.Text:=aDataSet.FieldByName('ACTION').AsString;
  Result := Showmodal = mrOK;
end;
end.

