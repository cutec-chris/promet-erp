{*******************************************************************************
Dieser Sourcecode darf nicht ohne gültige Geheimhaltungsvereinbarung benutzt werden
und ohne gültigen Vertriebspartnervertrag weitergegeben oder kommerziell verwertet werden.
You have no permission to use this Source without valid NDA
and copy it without valid distribution partner agreement
CU-TEC Christian Ulrich
info@cu-tec.de
*******************************************************************************}
unit uMimeTypeEdit;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  EditBtn, ExtCtrls, Buttons, uIntfStrConsts, DBGrids, DBCtrls, uData, LCLType,
  ButtonPanel, db,uDocuments;
type

  { TfMimeTypeEdit }

  TfMimeTypeEdit = class(TForm)
    ButtonPanel1: TButtonPanel;
    DBNavigator1: TDBNavigator;
    Label1: TLabel;
    MimeTypes: TDatasource;
    eOpenWith: TDBEdit;
    gMimes: TDBGrid;
    lOpenWith: TLabel;
    lMimeTypes: TLabel;
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
    FDocuments : TDocument;
  public
    { public declarations }
    function Execute(aDocuments : TDocument) : Boolean;
    procedure SetupDB;
    procedure SetLanguage;
  end; 

var
  fMimeTypeEdit: TfMimeTypeEdit;

implementation
procedure TfMimeTypeEdit.FormShow(Sender: TObject);
begin
  SetupDB;
end;
procedure TfMimeTypeEdit.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
    begin
      Key := 0;
      Close;
    end;
end;
function TfMimeTypeEdit.Execute(aDocuments : TDocument): Boolean;
var
  ActControl: TWinControl;
begin
  FDocuments := aDocuments;
  MimeTypes.DataSet := FDocuments.MimeTypes.DataSet;
  ActControl := Screen.ActiveControl;
  Result := Showmodal = mrOK;
  try
    if Assigned(ActControl) and ActControl.CanFocus then ActControl.SetFocus;
  except
  end;
end;
procedure TfMimeTypeEdit.SetupDB;
begin
end;
procedure TfMimeTypeEdit.SetLanguage;
begin
  if not Assigned(Self) then
    begin
      Application.CreateForm(TfMimeTypeEdit,fMimeTypeEdit);
      Self := fMimeTypeEdit;
    end;
end;
initialization
  {$I umimetypeedit.lrs}
end.
