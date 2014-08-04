unit urepairimages;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, db, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  DbCtrls, StdCtrls, DBGrids, ButtonPanel,uOrder;

type

  { TfRepairImages }

  TfRepairImages = class(TForm)
    ButtonPanel1: TButtonPanel;
    Datasource1: TDatasource;
    DBNavigator1: TDBNavigator;
    eName: TDBEdit;
    gList: TDBGrid;
    mErrordesc: TDBMemo;
    mSolve: TDBMemo;
    eFilter: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    procedure Datasource1StateChange(Sender: TObject);
    procedure eFilterEnter(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FDataSet: TOrderRepairImages;
    procedure SetDataSet(AValue: TOrderRepairImages);
    { private declarations }
  public
    { public declarations }
    procedure SetLanguage;
    function Execute : Boolean;
    property DataSet : TOrderRepairImages read FDataSet write SetDataSet;
  end;

var
  fRepairImages: TfRepairImages;

implementation
uses uData;
{$R *.lfm}

{ TfRepairImages }

procedure TfRepairImages.FormCreate(Sender: TObject);
begin
  DataSet := TOrderRepairImages.Create(nil,Data);
  DataSet.CreateTable;
  DataSet.Open;
end;

procedure TfRepairImages.Datasource1StateChange(Sender: TObject);
begin
  if DataSet.State=dsInsert then
    eName.SetFocus;
end;

procedure TfRepairImages.eFilterEnter(Sender: TObject);
begin
  DataSet.Filter(Data.ProcessTerm('UPPER('+Data.QuoteField('NAME')+')=UPPER('+Data.QuoteValue('*'+fRepairImages.eFilter.Text+'*'))+') OR UPPER('+Data.ProcessTerm(Data.QuoteField('DESC')+')=UPPER('+Data.QuoteValue('*'+fRepairImages.eFilter.Text+'*'))+')');
end;

procedure TfRepairImages.SetDataSet(AValue: TOrderRepairImages);
begin
  if FDataSet=AValue then Exit;
  FDataSet:=AValue;
  Datasource1.DataSet := AValue.DataSet;
end;

procedure TfRepairImages.SetLanguage;
begin
  if not Assigned(fRepairImages) then
    begin
      Application.CreateForm(TfRepairImages,fRepairImages);
      Self := fRepairImages;
    end;

end;

function TfRepairImages.Execute: Boolean;
begin
  if not Assigned(fRepairImages) then
    begin
      Application.CreateForm(TfRepairImages,fRepairImages);
      Self := fRepairImages;
    end;
  Result := fRepairImages.ShowModal = mrOK;
  if Result then
    DataSet.Post;
end;

end.

