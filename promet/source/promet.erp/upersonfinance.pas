{*******************************************************************************
  Copyright (C) Christian Ulrich info@cu-tec.de

  This source is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 2 of the License, or commercial alternative
  contact us for more information

  This code is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
  to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
  MA 02111-1307, USA.
*******************************************************************************}
unit uPersonFinance;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, DbCtrls, DBGrids,
  Buttons, ExtCtrls, db, uExtControls, uBaseDbClasses, uPrometFramesInplaceDB,
  Dialogs;
type

  { TfPersonFinance }

  TfPersonFinance = class(TPrometInplaceDBFrame)
    Bevel1: TBevel;
    Bevel2: TBevel;
    bTransfer: TSpeedButton;
    bTransfer1: TSpeedButton;
    cbCurrency: TExtDBCombobox;
    cbDiscontGroup: TExtDBCombobox;
    cbPaymenttarget: TExtDBCombobox;
    Customer: TDatasource;
    Banking: TDatasource;
    dnAccounts: TDBNavigator;
    eDiscont: TDBEdit;
    eCustNumber: TDBEdit;
    ExtRotatedLabel1: TExtRotatedLabel;
    ExtRotatedLabel2: TExtRotatedLabel;
    gBanking: TDBGrid;
    lBanking: TLabel;
    lCurrency: TLabel;
    lDiscont: TLabel;
    lDiscont1: TLabel;
    lDiscontGroup: TLabel;
    lPaymenttarget: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    procedure bTransfer1Click(Sender: TObject);
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
resourcestring
  strIBANValid                   = 'Die IBAN Nummer scheint GÜLTIG zu sein oder keine IBAN Nummer !';
  strIBANNotValid                = 'Die IBAN Nummer ist UNGÜLTIG !';
procedure TfPersonFinance.bTransferClick(Sender: TObject);
begin
  fTransfer.SetLanguage;
  fTransfer.eName.Text := TPerson(FDataSet).Text.AsString;
  fTransfer.eRSortCode.Text := TPerson(FDataSet).Banking.FieldByName('SORTCODE').AsString;
  fTransfer.eRAccount.Text := TPerson(FDataSet).Banking.FieldByName('ACCOUNT').AsString;
  fTransfer.Show;
end;

procedure TfPersonFinance.bTransfer1Click(Sender: TObject);
begin
  if TPerson(FDataSet).Banking.CheckAccount then
    ShowMessage(strIBANValid)
  else
    Showmessage(strIBANnotValid);
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

