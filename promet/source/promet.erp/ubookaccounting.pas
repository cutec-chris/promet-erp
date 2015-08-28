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
unit uBookAccounting;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, FileUtil,  Forms, Controls, Graphics, Dialogs,
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
{$R *.lfm}
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
              aOrder := TOrder.Create(nil);
              Data.SetFilter(aOrder,Data.QuoteField('ORDERNO')+'='+Data.QuoteValue(Accountingjournal.FieldByName('ORDERNO').AsString));
              if aOrder.Locate('ORDERNO',Accountingjournal.FieldByName('ORDERNO').AsString,[]) then
                begin
                  if FieldByName('PAYPRICE').AsFloat>=FieldByName('GROSSPRICE').AsFloat then
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
                          aOrder.Links.FieldByName('ICON').AsInteger := Data.GetLinkIcon(AccountingLink,True);
                          aOrder.Links.FieldByName('CHANGEDBY').AsString := Data.Users.IDCode.AsString;
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
end;
initialization
end.

