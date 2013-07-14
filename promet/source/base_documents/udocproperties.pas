{*******************************************************************************
Dieser Sourcecode darf nicht ohne gültige Geheimhaltungsvereinbarung benutzt werden
und ohne gültigen Vertriebspartnervertrag weitergegeben werden.
You have no permission to use this Source without valid NDA
and copy it without valid distribution partner agreement
CU-TEC Christian Ulrich
info@cu-tec.de
*******************************************************************************}

unit uDocProperties;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, ComCtrls, Grids, Menus, uIntfStrConsts,LCLType, ButtonPanel,
  uDocuments;

type

  { TfDocProperties }

  TfDocProperties = class(TForm)
    Bevel1: TBevel;
    Bevel3: TBevel;
    ButtonPanel1: TButtonPanel;
    lLastChangedValue: TLabel;
    llastChanged: TLabel;
    lMimeTypeValue: TLabel;
    lMimeType: TLabel;
    lFilenameValue: TLabel;
    lFilename: TLabel;
    lDate: TLabel;
    lDateValue: TLabel;
    miCheckoutToThisRevision: TMenuItem;
    pcTabs: TPageControl;
    pmLog: TPopupMenu;
    tsLog: TTabSheet;
    tsCommon: TTabSheet;
    sgLog: TStringGrid;
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure miCheckoutToThisRevisionClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    function Execute(aDoc : TDocument) : Boolean;
  end;

var
  fDocProperties: TfDocProperties;

implementation

{ TfDocProperties }

procedure TfDocProperties.miCheckoutToThisRevisionClick(Sender: TObject);
begin

end;

function TfDocProperties.Execute(aDoc: TDocument): Boolean;
begin
  if not Assigned(Self) then
    begin
      Application.CreateForm(TfDocProperties,fDocProperties);
      Self := fDocProperties;
    end;
  lFilenameValue.Caption:=aDoc.FileName;
  lMimeTypeValue.Caption:='';
  lDateValue.Caption:=DateTimeToStr(aDoc.CreationDate);
  lLastChangedValue.Caption:=DateTimeToStr(aDoc.LastModified);
  with aDoc.DataSet do
    begin
      First;
      sgLog.RowCount:=1;
      repeat
        begin
          sgLog.RowCount:=sgLog.RowCount+1;
          sgLog.Cells[0,sgLog.RowCount-1] := FieldByName('TIMESTAMPD').AsString;
          sgLog.Cells[1,sgLog.RowCount-1] := FieldByName('REVISION').AsString;
          sgLog.Cells[2,sgLog.RowCount-1] := FieldByName('MESSAGE').AsString;
          Next;
        end
      until EOF;
      if sgLog.RowCount>2 then
        sgLog.RowCount:=sgLog.RowCount-1;
    end;
  Result := Showmodal = mrOK;
end;

procedure TfDocProperties.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
    begin
      Key := 0;
      Close;
    end;
end;

initialization
  {$I udocproperties.lrs}

end.
