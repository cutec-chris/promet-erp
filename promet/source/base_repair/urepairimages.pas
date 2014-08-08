unit urepairimages;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, db, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  DbCtrls, StdCtrls, DBGrids, ButtonPanel, ComCtrls, ExtCtrls, uExtControls,
  uOrder;

type

  { TfRepairImages }

  TfRepairImages = class(TForm)
    ButtonPanel1: TButtonPanel;
    Datasource1: TDatasource;
    DBNavigator1: TDBNavigator;
    eName: TDBEdit;
    gList: TDBGrid;
    eFilter: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    mErrordesc: TDBMemo;
    mSolve: TDBMemo;
    pCommon: TPanel;
    pcPages: TExtMenuPageControl;
    tsCommon: TTabSheet;
    procedure AddLinks(Sender: TObject);
    procedure AddDocuments(Sender: TObject);
    procedure AddImages(Sender: TObject);
    procedure AddHistory(Sender: TObject);
    procedure DataSetDataSetAfterScroll(DataSet: TDataSet);
    procedure Datasource1StateChange(Sender: TObject);
    procedure eFilterEnter(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
    FDataSet: TOrderRepairImages;
    procedure SetDataSet(AValue: TOrderRepairImages);
    procedure DoOpen;
  public
    { public declarations }
    procedure SetLanguage;
    function Execute : Boolean;
    property DataSet : TOrderRepairImages read FDataSet write SetDataSet;
  end;

var
  fRepairImages: TfRepairImages;

implementation
uses uData,uHistoryFrame,uImageFrame,uIntfStrConsts,uDocuments,uDocumentFrame,uLinkFrame;
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
procedure TfRepairImages.AddImages(Sender: TObject);
begin
  TfImageFrame(Sender).DataSet := TOrderRepairImages(FDataSet).Images;
  TOrderRepairImages(FDataSet).Images.Open;
end;

procedure TfRepairImages.AddLinks(Sender: TObject);
begin
  TfLinkFrame(Sender).BaseName:='REPI';
  TfLinkFrame(Sender).DataSet := TOrderRepairImages(FDataSet).Links;
end;

procedure TfRepairImages.AddHistory(Sender: TObject);
begin
  TfHistoryFrame(Sender).BaseName:='REPI';
  TfHistoryFrame(Sender).DataSet := TOrderRepairImages(FDataSet).History;
  TfHistoryFrame(Sender).SetRights(True);
end;

procedure TfRepairImages.AddDocuments(Sender: TObject);
var
  aDocuments: TDocuments;
begin
  if not Assigned(TfDocumentFrame(Sender).DataSet) then
    begin
      aDocuments := TDocuments.Create(Self,Data);
      TfDocumentFrame(Sender).DataSet := aDocuments;
      TfDocumentFrame(Sender).Refresh(DataSet.Id.AsInteger,'R');
    end;
  TfDocumentFrame(Sender).BaseElement := FDataSet;
end;

procedure TfRepairImages.DataSetDataSetAfterScroll(DataSet: TDataSet);
begin
  DoOpen;
end;

procedure TfRepairImages.eFilterEnter(Sender: TObject);
begin
  DataSet.Filter(Data.ProcessTerm('UPPER('+Data.QuoteField('NAME')+')=UPPER('+Data.QuoteValue('*'+fRepairImages.eFilter.Text+'*'))+') OR UPPER('+Data.ProcessTerm(Data.QuoteField('SYMTOMS')+')=UPPER('+Data.QuoteValue('*'+fRepairImages.eFilter.Text+'*'))+')');
end;

procedure TfRepairImages.SetDataSet(AValue: TOrderRepairImages);
begin
  if FDataSet=AValue then Exit;
  FDataSet:=AValue;
  Datasource1.DataSet := AValue.DataSet;
end;

procedure TfRepairImages.DoOpen;
var
  aDocuments: TDocuments;
  aDocFrame: TfDocumentFrame;
begin
  pcPages.CloseAll;
  pcPages.AddTabClass(TfHistoryFrame,strHistory,@AddHistory);
  TOrderRepairImages(DataSet).History.Open;
  if TOrderRepairImages(DataSet).History.Count > 0 then
    pcPages.AddTab(TfHistoryFrame.Create(Self),False);
  if not TOrderRepairImages(DataSet).Images.DataSet.Active then
    TOrderRepairImages(DataSet).Images.DataSet.Open;
   pcPages.AddTabClass(TfImageFrame,strImages,@AddImages);
  if (TOrderRepairImages(DataSet).Images.Count > 0) then
    pcPages.AddTab(TfImageFrame.Create(Self),False);
  TOrderRepairImages(DataSet).Images.DataSet.Close;
  pcPages.AddTabClass(TfDocumentFrame,strFiles,@AddDocuments);
  if (FDataSet.State <> dsInsert) and (fDataSet.Count > 0) then
    begin
      aDocuments := TDocuments.Create(Self,Data);
      aDocuments.CreateTable;
      aDocuments.Select(DataSet.Id.AsLargeInt,'R');
      aDocuments.Open;
      if aDocuments.Count = 0 then
        aDocuments.Free
      else
        begin
          aDocFrame := TfDocumentFrame.Create(Self);
          pcPages.AddTab(aDocFrame,False);
          aDocFrame.DataSet := aDocuments;
          aDocFrame.BaseElement := FDataSet;
        end;
    end;
   pcPages.AddTabClass(TfLinkFrame,strLinks,@AddLinks);
  TOrderRepairImages(DataSet).Links.Open;
  if TOrderRepairImages(DataSet).Links.Count > 0 then
    pcPages.AddTab(TfLinkFrame.Create(Self),False);
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
  DataSet.DataSet.AfterScroll:=@DataSetDataSetAfterScroll;
  DoOpen;
  Result := fRepairImages.ShowModal = mrOK;
  if Result and DataSet.CanEdit then
    DataSet.Post;
end;

end.

