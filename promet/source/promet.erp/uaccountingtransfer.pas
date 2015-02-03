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
unit uAccountingTransfer;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils,  Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons,Utils,uAccountingQue,uIntfStrConsts;
type
  TfTransfer = class(TForm)
    bAbort: TBitBtn;
    bOK: TBitBtn;
    cbAccount: TComboBox;
    cbCurrency: TComboBox;
    cbtextKey: TComboBox;
    eRAccount: TEdit;
    eRSortCode: TEdit;
    eAmount: TEdit;
    eName: TEdit;
    gbReciver: TGroupBox;
    lTextKey: TLabel;
    lPurpose: TLabel;
    lAmount: TLabel;
    lInstitute: TLabel;
    lRAccount: TLabel;
    lRSortCode: TLabel;
    lName: TLabel;
    lAccount: TLabel;
    mPurpose: TMemo;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure bAbortClick(Sender: TObject);
    procedure bOKClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    procedure SetupDB;
    procedure SetLanguage;
  end;
var
  fTransfer: TfTransfer;

implementation
{$R *.lfm}
uses uData,uError,uAccounting;
resourcestring
  strAmountcannotbe0            = 'Bitte geben Sie einen Wert > 0 an';
  strPurposeLinetoLong          = 'Eine Zahlungsgrundzeile ist zu lang';
  strPurposeLinetomuchLines     = 'Der Zahlungsgrund besteht aus zu vielen Zeilen';
procedure TfTransfer.FormShow(Sender: TObject);
begin
  SetupDB;
end;
procedure TfTransfer.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  eName.Text := '';
  eRAccount.Text := '';
  eRSortCode.Text := '';
  eAmount.Text := FormatFloat('0.00',0);
  mPurpose.Lines.Clear;
end;
procedure TfTransfer.FormCreate(Sender: TObject);
begin
  eName.Text := '';
  eRAccount.Text := '';
  eRSortCode.Text := '';
  eAmount.Text := FormatFloat('0.00',0);
  mPurpose.Lines.Clear;
end;
procedure TfTransfer.bAbortClick(Sender: TObject);
begin
  Close;
end;
procedure TfTransfer.bOKClick(Sender: TObject);
var
  CmdLn : string;
  tmp : string;
  i: Integer;
  acc: String;
  sort: String;
begin
  if StrToFloat(eAmount.Text) = 0 then
    begin
      fError.ShowWarning(strAmountcannotbe0);
      exit;
    end;
  for i := 0 to mPurpose.Lines.Count-1 do
    begin
      if length(mPurpose.Lines[i]) > 27 then
        begin
          fError.ShowWarning(strPurposeLinetoLong);
          exit;
        end;
    end;
  if mPurpose.Lines.Count > 2 then
    begin
      fError.ShowWarning(strPurposeLinetomuchLines);
      exit;
    end;
  tmp := cbAccount.Text;
  acc := copy(tmp,rpos(' ',tmp)+1,length(tmp));
  tmp := copy(tmp,0,rpos(' ',tmp)-1);
  sort := copy(tmp,rpos(' ',tmp)+1,length(tmp));
  fAccountingQue.Setlanguage;
  fAccountingQue.Intf.AddTransfer(Sort,acc,eRSortCode.Text,eRAccount.Text,eName.Text,eAmount.Text,cbCurrency.Text,copy(cbTextKey.Text,0,pos(' ',cbTextKey.Text)),mPurpose.Lines);
  Close;
end;
procedure TfTransfer.SetupDB;
var
  Accounts: TAccounts;
begin
  cbAccount.Items.Clear;
  Accounts := TAccounts.CreateEx(Self,Data);
  Accounts.Open;
  Accounts.DataSet.First;
  while not Accounts.DataSet.EOF do
    begin
      cbAccount.Items.Add(Accounts.FieldByName('NAME').AsString+' '+Accounts.FieldByName('SORTCODE').AsString+' '+Accounts.FieldByName('ACCOUNTNO').AsString);
      Accounts.DataSet.Next;
    end;
  Accounts.Free;
end;
procedure TfTransfer.SetLanguage;
begin
  if not Assigned(Self) then
    begin
      Application.CreateForm(TfTransfer,fTransfer);
      Self := fTransfer;
    end;
end;
initialization
end.

