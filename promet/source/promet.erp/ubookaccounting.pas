{*******************************************************************************
Dieser Sourcecode darf nicht ohne gültige Geheimhaltungsvereinbarung benutzt werden
und ohne gültigen Vertriebspartnervertrag weitergegeben werden.
You have no permission to use this Source without valid NDA
and copy it without valid distribution partner agreement
CU-TEC Christian Ulrich
info@cu-tec.de
*******************************************************************************}
unit uBookAccounting;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ButtonPanel, ExtCtrls, EditBtn, MaskEdit, ZVDateTimePicker,
  uBaseERPDBClasses, uAccounting, uOrder;
type
  TfBookAccounting = class(TForm)
    ButtonPanel1: TButtonPanel;
    DateEdit1: TDateEdit;
    dePayed: TZVDateTimePicker;
    eDifference: TEdit;
    iAccount: TImage;
    iOrder: TImage;
    lAmount: TLabel;
    lPayed: TLabel;
    lrest: TLabel;
    meAmount: TEdit;
    pBank: TPanel;
    pBank1: TPanel;
    procedure meAmountChange(Sender: TObject);
  private
    { private declarations }
    FAccount: TAccountExchange;
    FJournal: TAccountingjournal;
  public
    { public declarations }
    property Accountingjournal : TAccountingjournal read FJournal write FJournal;
    property AccountExchange : TAccountExchange read FAccount write FAccount;
    procedure SetLanguage;
    function Execute(Value : real;Date : TDateTime;AccountingLink : string = '') : Boolean;
  end; 
var
  fBookAccounting: TfBookAccounting;

implementation
uses uData;
procedure TfBookAccounting.meAmountChange(Sender: TObject);
begin
  eDifference.Text:=FormatFloat('0.00',Accountingjournal.FieldByName('GROSSPRICE').AsFloat-Accountingjournal.FieldByName('PAYPRICE').AsFloat-StrToFloatDef(meAmount.Text,0));
end;
procedure TfBookAccounting.SetLanguage;
begin
  if not Assigned(Self) then
    begin
      Application.CreateForm(TfBookAccounting,fBookAccounting);
      Self := fBookAccounting;
    end;
end;
function TfBookAccounting.Execute(Value : real;Date : TDateTime;AccountingLink : string): Boolean;
var
  aOrder: TOrder;
begin
  iAccount.Visible := AccountingLink <> '';
//  iOrder.Visible := OrderLink <> '';
  eDifference.Text:=FormatFloat('0.00',Accountingjournal.FieldByName('GROSSPRICE').AsFloat-Accountingjournal.FieldByName('PAYPRICE').AsFloat-Value);
  dePayed.Date:=Date;
  meAmount.Text:=FormatFloat('0.00',Value);
  Result := Showmodal = mrOK;
  if Result then
    begin
      if Accountingjournal.FieldByName('PAYMENT').AsString <> 'Y' then
        begin
          with Accountingjournal.DataSet do
            begin
              Edit;
              FieldByName('PAYPRICE').AsFloat := FieldByName('PAYPRICE').AsFloat+StrToFloat(meAmount.Text);
              FieldByName('PAYMENT').AsString := 'Y';
              FieldByName('PAYEDON').AsDateTime := dePayed.Date;
              Post;
              aOrder := TOrder.Create(nil,Data);
              Data.SetFilter(aOrder,Data.QuoteField('ORDERNO')+'='+Data.QuoteValue(Accountingjournal.FieldByName('ORDERNO').AsString));
              if Data.Locate(aOrder,'ORDERNO',Accountingjournal.FieldByName('ORDERNO').AsString,[]) then
                begin
                  aOrder.DataSet.Edit;
                  aOrder.FieldByName('PAYEDON').AsDateTime := dePayed.Date;
                  aOrder.DataSet.Post;
                  if AccountingLink <> '' then
                    begin
                      aOrder.Links.Open;
                      aOrder.Links.DataSet.Insert;
                      aOrder.Links.FieldByName('LINK').AsString := AccountingLink;
                      aOrder.Links.FieldByName('NAME').AsString := Data.GetLinkDesc(AccountingLink);
                      aOrder.Links.FieldByName('ICON').AsInteger := Data.GetLinkIcon(AccountingLink);
                      aOrder.Links.FieldByName('CHANGEDBY').AsString := Data.Users.FieldByName('IDCODE').AsString;
                      aOrder.Links.DataSet.Post;
                      with AccountExchange.Dataset do
                        begin
                          Edit;
                          FieldByName('VOUCHER').AsString := Data.BuildLink(aOrder.DataSet);
                          Post;
                        end;
                    end;
                end;
            end;
        end;
    end;
end;
initialization
  {$I ubookaccounting.lrs}
end.

