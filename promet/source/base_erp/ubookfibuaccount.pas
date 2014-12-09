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
unit ubookfibuaccount;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, db, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  DBGrids, ButtonPanel,uBaseERPDBClasses;

type

  { TfBookFibuAccount }

  TfBookFibuAccount = class(TForm)
    ButtonPanel1: TButtonPanel;
    cbWholeList: TCheckBox;
    Datasource1: TDatasource;
    gAccounts: TDBGrid;
    eAccountno: TEdit;
    eName: TEdit;
    Label1: TLabel;
    procedure eAccountnoChange(Sender: TObject);
  private
    { private declarations }
    FDataSet : TFinancialAccounts;
  public
    { public declarations }
    property DataSet : TFinancialAccounts read FDataSet;
    function Execute : Boolean;
  end;

var
  fBookFibuAccount: TfBookFibuAccount;

implementation
uses uData;
{$R *.lfm}

{ TfBookFibuAccount }

procedure TfBookFibuAccount.eAccountnoChange(Sender: TObject);
begin
  FDataSet.Filter('('+Data.ProcessTerm(Data.QuoteField('ACCOUNTNO')+'='+Data.QuoteValue('*'+eAccountno.Text+'*'))+') AND (UPPER('+Data.ProcessTerm(Data.QuoteField('NAME')+')=UPPER('+Data.QuoteValue('*'+eName.Text+'*'))+'))');
end;

function TfBookFibuAccount.Execute: Boolean;
begin
  if not Assigned(Self) then
    begin
      Application.CreateForm(TfBookFibuAccount,fBookFibuAccount);
      Self := fBookFibuAccount;
      FDataSet := TFinancialAccounts.CreateEx(Data,Data);
      Datasource1.DataSet := FDataSet.DataSet;
    end;
  FDataSet.Open;
  cbWholeList.Checked:=False;
  Result := Showmodal = mrOK;
end;

end.

