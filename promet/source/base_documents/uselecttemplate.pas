{*******************************************************************************
Dieser Sourcecode darf nicht ohne gültige Geheimhaltungsvereinbarung benutzt werden
und ohne gültigen Vertriebspartnervertrag weitergegeben oder kommerziell verwertet werden.
You have no permission to use this Source without valid NDA
and copy it without valid distribution partner agreement
CU-TEC Christian Ulrich
info@cu-tec.de
*******************************************************************************}

unit uSelectTemplate;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, DBGrids,
  StdCtrls, Buttons, Utils, db, uIntfStrConsts,FileUtil,LCLType, uBaseDbClasses,
  uDocuments;

type

  { TfSelectTemplate }

  TfSelectTemplate = class(TForm)
    cbReplacePlaceholders: TCheckBox;
    DocumentActions: TDatasource;
    gTemplate: TDBGrid;
    bSelect: TBitBtn;
    bAbort: TBitBtn;
    bNew: TBitBtn;
    bDelete: TBitBtn;
    OpenDialog: TOpenDialog;
    procedure bSelectClick(Sender: TObject);
    procedure bAbortClick(Sender: TObject);
    procedure bNewClick(Sender: TObject);
    procedure bDeleteClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    FDataSet: TBaseDbDataSet;
    { private declarations }
    fType : string;
    FDocuments : TDocument;
    OK : Boolean;
    procedure SetDataSet(AValue: TBaseDbDataSet);
  public
    { public declarations }
    property DataSet : TBaseDbDataSet read FDataSet write SetDataSet;
    function Execute(Typ : string;aDocuments : TDocument) : Boolean;
  end; 

var
  fSelectTemplate: TfSelectTemplate;

implementation

uses uData;

{ TfSelectTemplate }

procedure TfSelectTemplate.bSelectClick(Sender: TObject);
begin
  OK := True;
  Close;
end;

procedure TfSelectTemplate.bAbortClick(Sender: TObject);
begin
  Close;
end;

procedure TfSelectTemplate.bNewClick(Sender: TObject);
var
  Stream: TFileStream;
begin
  if OpenDialog.Execute then
    with DataSet.DataSet do
      begin
        Insert;
        FieldByName('TYPE').AsString := fType;
        FieldByName('NAME').AsString := ExtractFileName(copy(ExtractFileName(OpenDialog.Filename),0,rpos('.',ExtractFileName(OpenDialog.Filename))-1));
        FieldByName('EXTENSION').AsString := copy(ExtractFileName(OpenDialog.Filename),rpos('.',ExtractFileName(OpenDialog.Filename))+1,length(ExtractFileName(OpenDialog.Filename)));
        if Data.IsSQLDb then Post;
        Stream := TFileStream.Create(UTF8ToSys(Opendialog.Filename),fmOpenread);
        Data.StreamToBlobField(Stream,DataSet.DataSet,'DOCUMENT');
        Stream.Free;
      end;
end;

procedure TfSelectTemplate.bDeleteClick(Sender: TObject);
begin
  if MessageDlg(strRealdelete,mtInformation,[mbYes,mbNo],0) = mrYes then
    DataSet.Delete;
end;

procedure TfSelectTemplate.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
    begin
      Key := 0;
      Close;
    end;
end;
procedure TfSelectTemplate.SetDataSet(AValue: TBaseDbDataSet);
begin
  if not Assigned(fSelectTemplate) then
    begin
      Application.CreateForm(TfSelectTemplate,fSelectTemplate);
      Self := fSelectTemplate;
      AValue.CreateTable;
    end;
  if FDataSet=AValue then Exit;
  FDataSet:=AValue;
end;
function TfSelectTemplate.Execute(Typ : string;aDocuments : TDocument): Boolean;
var
  ActControl: TWinControl;
begin
  FDocuments := aDocuments;
  fType := Typ;
  Data.SetFilter(FDataSet,Data.QuoteField('TYPE')+'='+Data.QuoteValue(Typ));
  DocumentActions.DataSet := FDataSet.DataSet;
  FDataSet.Open;
  OK := false;
  ActControl := Screen.ActiveControl;
  Showmodal;
  try
    if Assigned(ActControl) and ActControl.CanFocus then ActControl.SetFocus;
  except
  end;
  Result := OK;
end;

initialization
  {$I uselecttemplate.lrs}

end.

