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
Created 23.09.2013
*******************************************************************************}
unit uWizardnewaccount;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Buttons, ComCtrls, uIntfStrConsts, ProcessUtils, FileUtil,uBaseERPDBClasses,
  uAccounting;

type

  { TfWizardNewAccount }

  TfWizardNewAccount = class(TForm)
    bAbort0: TButton;
    bAbort3: TButton;
    bAbort1: TButton;
    bNext0: TButton;
    bNext3: TButton;
    bNext1: TButton;
    bPrev0: TButton;
    bPrev3: TButton;
    bPrev1: TButton;
    bvleft: TBevel;
    bvImage: TBevel;
    bvRight0: TBevel;
    bvRight1: TBevel;
    bvRight2: TBevel;
    cbAccount: TComboBox;
    eAccountname: TEdit;
    eAccountNo: TEdit;
    eCustomerID: TEdit;
    eWebAddress: TEdit;
    eSortcode: TEdit;
    eName: TEdit;
    imDialog: TImage;
    lAccountNo1: TLabel;
    lFinTsName: TLabel;
    lSortcode: TLabel;
    lAccountNo: TLabel;
    lAccountName: TLabel;
    lDescription2: TLabel;
    lName: TLabel;
    lAccount: TLabel;
    lDescription0: TLabel;
    lDescription1: TLabel;
    pButtons0: TPanel;
    pButtons1: TPanel;
    pButtons2: TPanel;
    pCont0: TPanel;
    pCont3: TPanel;
    pCont1: TPanel;
    pLeft: TPanel;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    RadioButton3: TRadioButton;
    rbPINTAN: TRadioButton;
    rbImportManualAccounts1: TRadioButton;
    procedure bAbort0Click(Sender: TObject);
    procedure bNext0Click(Sender: TObject);
    procedure bPrev0Click(Sender: TObject);
  private
    { private declarations }
    Steps : array of Integer;
  public
    { public declarations }
    procedure SetLanguage;
    procedure DoSave;
    function  DoExecStep(Step : Integer) : Integer;
    procedure InitWizard;
  end;

var
  fWizardNewAccount: TfWizardNewAccount;

implementation

uses uData,uMain;

{ TfWizardNewAccount }

procedure TfWizardNewAccount.bAbort0Click(Sender: TObject);
begin
  Close;
end;

procedure TfWizardNewAccount.bNext0Click(Sender: TObject);
var
  i: LongInt;
begin
  i := Steps[length(Steps)-1];
  i := DoExecStep(i);
  Setlength(Steps,length(Steps)+1);
  Steps[length(Steps)-1] := i;
  TPanel(FindComponent('pCont'+IntToStr(Steps[length(Steps)-2]))).Visible := false;
  if (FindComponent('pCont'+IntToStr(i)) <> nil) and (TButton(Sender).Caption <> strFinish) then
    begin
      TPanel(FindComponent('pCont'+IntToStr(i))).Visible := True
    end
  else
    begin
      DoSave;
      Close;
    end;
end;

procedure TfWizardNewAccount.bPrev0Click(Sender: TObject);
var
  i: LongInt;
begin
  TPanel(FindComponent('pCont'+IntToStr(Steps[length(Steps)-1]))).Visible := false;
  if FindComponent('pCont'+IntToStr(Steps[length(Steps)-2])) <> nil then
    TPanel(FindComponent('pCont'+IntToStr(Steps[length(Steps)-2]))).Visible := True;
  Setlength(Steps,length(Steps)-1);
end;

procedure TfWizardNewAccount.SetLanguage;
var
  i: Integer;
  NextButton: TButton;
begin
  if not Assigned(Self) then
    begin
      Application.CreateForm(TfWizardNewAccount,fWizardNewAccount);
      Self := fWizardNewAccount;
    end;
  i := 0;
  while FindComponent('bPrev'+IntToStr(i)) <> nil do
    begin
      TButton(FindComponent('bPrev'+IntToStr(i))).Caption := strPrev;
      if i = 0 then
        TButton(FindComponent('bPrev'+IntToStr(i))).Enabled := False;
      TButton(FindComponent('bNext'+IntToStr(i))).Caption := strNext;
      NextButton := TButton(FindComponent('bNext'+IntToStr(i)));
      TButton(FindComponent('bAbort'+IntToStr(i))).Caption := strAbort;
      inc(i);
    end;
  NextButton.Caption := strFinish;
  //Application depend Language strings
  //lDescription0.Caption := strWoteAccountType;
  //rbImportFinTSToolsAccount.Caption := strImportFinTSToolsAccount;
  //rbPINTAN.Caption := strImportManualAQAccounts;
  lAccount.Caption := strAccount;
  lName.Caption := strName;
  //lDescription1.Caption := strWoteanAccountToImport;
  //Caption := strNewAccount;
end;

procedure TfWizardNewAccount.DoSave;
var
  TN: TTreeNode;
  tmp: String;
  aAccounts: TAccounts;
begin
  //Wizard finished, use the made settings
  aAccounts := TAccounts.Create(nil,Data);
  aAccounts.CreateTable;
  if Steps[length(Steps)-2] = 1 then
    with aAccounts.DataSet do
      begin
        Insert;
        FieldByName('TYPE').AsString := 'BNC';
        tmp := cbAccount.text;
        tmp := copy(tmp,pos(' ',tmp)+1,length(tmp));
        FieldByName('SORTCODE').AsString := copy(tmp,0,pos(' ',tmp)-1);
        tmp := copy(tmp,pos(' ',tmp)+1,length(tmp));
        FieldByName('ACCOUNTNO').AsString := copy(tmp,0,pos(' ',tmp)-1);
        FieldByName('NAME').AsString := eName.text;
        Post;
        {
        TN := fMain.tvMain.Items.AddChildObject(fMain.tvMain.Selected.Parent,'',TTreeEntry);
        TTreeEntry(TN.Data).Rec := Data.GetBookmark(Data.Accounts);
        TTreeEntry(TN.Data).Text[0] := eName.text;
        TTreeEntry(TN.Data).Typ := etAccount;
        }
      end;
end;

function TfWizardNewAccount.DoExecStep(Step: Integer): Integer;
var
  tmp: String;
  atmp: String;
  btmp : string;
  CmdLn: String;
begin
  //Do whatever to do in these Step and return the next step
  case Step of
  0:
    begin
      if rbPINTAN.Checked then
        begin
          Result := 1;
        end
      {
      else if rbImportFinTSToolsAccount.Checked then
        begin
          Result := 2;
        end;
      }
    end;
  1:
    begin
      Result := 3;
      try
        CmdLn := 'aqhbci-tool4 adduser -t pintan -u ';
        {$IFDEF MSWINDOWS}
        CmdLn := AppendPathDelim(AppendPathDelim(ExtractFilePath(Application.Exename))+'tools')+CmdLn;
        {$ENDIF}
        tmp := ExecProcessEx(CmdLn,AppendPathDelim(ExtractFileDir(Application.Exename))+'tools');
      except
        tmp := '';
      end;
      cbAccount.Items.Clear;
      try
        CmdLn := 'aqbanking-cli listaccs';
        {$IFDEF MSWINDOWS}
        CmdLn := AppendPathDelim(AppendPathDelim(ExtractFilePath(Application.Exename))+'tools')+CmdLn;
        {$ENDIF}
        tmp := ExecProcessEx(CmdLn,AppendPathDelim(ExtractFileDir(Application.Exename))+'tools');
      except
        tmp := '';
      end;
      while length(tmp) > 0 do
        begin
          if pos(#13,tmp) > 0 then
            begin
              atmp := copy(tmp,0,pos(#13,tmp)-1);
              tmp := copy(tmp,pos(#13,tmp)+1,length(tmp));
            end
          else
            begin
              atmp := tmp;
              tmp := '';
            end;
          atmp := copy(atmp,pos(#9,atmp)+1,length(atmp));
          btmp := copy(atmp,0,pos(#9,atmp)-1);
          btmp := btmp+' ';
          atmp := trim(copy(atmp,pos(#9,atmp)+1,length(atmp)));

          btmp := btmp+copy(atmp,0,pos(#9,atmp)-1);
          btmp := btmp;
          atmp := trim(copy(atmp,pos(#9,atmp)+1,length(atmp)));
          atmp := trim(copy(atmp,pos(#9,atmp)+1,length(atmp)));

          btmp := atmp+' '+btmp;
          cbAccount.Items.Add(btmp);
        end;
    end;
  2:Result := 3;
  end;
end;

procedure TfWizardNewAccount.InitWizard;
var
  i : Integer;
begin
  if not Assigned(Self) then
    begin
      Application.CreateForm(TfWizardNewAccount,fWizardNewAccount);
      Self := fWizardNewAccount;
    end;
  Setlength(Steps,1);
  Steps[0] := 0;
  i := 0;
  while FindComponent('pCont'+IntToStr(i)) <> nil do
    begin
      TPanel(FindComponent('pCont'+IntToStr(i))).Visible := False;
      inc(i);
    end;
  pCont0.Visible := True;
  //bNext3.Caption := strFinish;
end;

initialization
  {$I uwizardnewaccount.lrs}

end.
