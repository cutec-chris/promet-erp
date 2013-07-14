{*******************************************************************************
Dieser Sourcecode darf nicht ohne gültige Geheimhaltungsvereinbarung benutzt werden
und ohne gültigen Vertriebspartnervertrag weitergegeben werden.
You have no permission to use this Source without valid NDA
and copy it without valid distribution partner agreement
Christian Ulrich
info@cu-tec.de
Created 01.06.2006
*******************************************************************************}
unit uQSPositionFrame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, ExtCtrls, DBGrids,
  uPositionFrame, db, Grids, Graphics, DbCtrls, Buttons, Clipbrd, uPrometFramesInplace;

type
 { TfQSPositionFrame }

  TfQSPositionFrame = class(TPrometInplaceFrame)
    lTesttime: TDBText;
    lSerial: TDBText;
    lPosNo: TDBText;
    lIdent: TDBText;
    dgSteps: TDBGrid;
    dgTest: TDBGrid;
    OrderPos: TDatasource;
    OrderQMTest: TDatasource;
    OrderQMtestDetails: TDatasource;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    Splitter1: TSplitter;
    procedure dgStepsDrawColumnCell(Sender: TObject; const Rect: TRect;
      DataCol: Integer; Column: TColumn; State: TGridDrawState);
    procedure FrameEnter(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    procedure SetupDB;
    procedure SetRights(Editable : Boolean);override;
  end;

implementation

{$R *.lfm}

uses uRawData,uOrder;

{ TfQSPositionFrame }

procedure TfQSPositionFrame.FrameEnter(Sender: TObject);
begin
  SetupDB;
end;
procedure TfQSPositionFrame.dgStepsDrawColumnCell(Sender: TObject;
  const Rect: TRect; DataCol: Integer; Column: TColumn; State: TGridDrawState);
const
  memowidth = 200;
var
  s: string;
begin
  with (Sender as TDBGrid) do begin
    if Column.Field.IsBlob then begin
      Canvas.FillRect(Rect);
      s := copy(Column.Field.AsString, 1, memowidth);
      Canvas.TextOut(Rect.Left+2, Rect.Top+2, s);
    end else begin
      DefaultDrawColumnCell(Rect, DataCol, Column, State);
    end;
  end;
end;

procedure TfQSPositionFrame.SpeedButton1Click(Sender: TObject);
begin
  if TfPosition(Owner).Dataset is TOrderPos then
    begin
      if not Assigned(fRawData) then Application.CreateForm(TfRawData,fRawData);
      fRawData.Memo1.Text:=TOrderPos(TfPosition(Owner).Dataset).QMTest.FieldByName('RAWDATA').AsString;
      fRawData.ShowModal;
    end;
end;

procedure TfQSPositionFrame.SpeedButton2Click(Sender: TObject);
begin
  if TfPosition(Owner).Dataset is TOrderPos then
    Clipboard.AsText:=TOrderPos(TfPosition(Owner).Dataset).QMTest.FieldByName('RAWDATA').AsString;
end;

procedure TfQSPositionFrame.SetupDB;
begin
  if TfPosition(Owner).Dataset is TOrderPos then
    with TfPosition(Owner).DataSet as TorderPos do
      begin
        OrderPos.DataSet := DataSet;
        OrderQMTest.DataSet := QMTest.DataSet;
        QMTest.Open;
        OrderQMTestDetails.DataSet := QMTest.Details.DataSet;
        QMTest.Details.Open;
      end;
end;

procedure TfQSPositionFrame.SetRights(Editable: Boolean);
begin
  dgTest.ReadOnly := not Editable;
  dgSteps.ReadOnly := not Editable;
end;

end.

