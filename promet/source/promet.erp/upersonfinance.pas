unit uPersonFinance;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, DbCtrls, DBGrids,
  Buttons, ExtCtrls, db, uExtControls, uBaseDbClasses, uPrometFramesInplaceDB;
type
  TfPersonFinance = class(TPrometInplaceDBFrame)
    Bevel1: TBevel;
    Bevel2: TBevel;
    bTransfer: TSpeedButton;
    cbCurrency: TExtDBCombobox;
    cbDiscontGroup: TExtDBCombobox;
    cbPaymenttarget: TExtDBCombobox;
    Customer: TDatasource;
    Banking: TDatasource;
    dnAccounts: TDBNavigator;
    eDiscont: TDBEdit;
    ExtRotatedLabel1: TExtRotatedLabel;
    ExtRotatedLabel2: TExtRotatedLabel;
    gBanking: TDBGrid;
    lBanking: TLabel;
    lCurrency: TLabel;
    lDiscont: TLabel;
    lDiscontGroup: TLabel;
    lPaymenttarget: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    procedure bTransferClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    procedure SetDataSet(const AValue: TBaseDBDataSet);override;
    procedure SetRights(Editable : Boolean);override;
  end;
implementation
uses uPerson,uAccountingTransfer;
{$R *.lfm}
procedure TfPersonFinance.bTransferClick(Sender: TObject);
begin
  fTransfer.SetLanguage;
  fTransfer.eName.Text := TPerson(FDataSet).Text.AsString;
  fTransfer.eRSortCode.Text := TPerson(FDataSet).Banking.FieldByName('SORTCODE').AsString;
  fTransfer.eRAccount.Text := TPerson(FDataSet).Banking.FieldByName('ACCOUNT').AsString;
  fTransfer.Show;
end;

procedure TfPersonFinance.SetDataSet(const AValue: TBaseDBDataSet);
begin
  inherited SetDataSet(AValue);
  Customer.DataSet := FDataSet.DataSet;
  Banking.DataSet := TPerson(FDataSet).Banking.DataSet;
end;
procedure TfPersonFinance.SetRights(Editable: Boolean);
begin
  Enabled := Editable;
end;

end.

