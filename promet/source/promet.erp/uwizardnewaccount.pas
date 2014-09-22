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
  StdCtrls, Buttons, ComCtrls, uIntfStrConsts, ProcessUtils, FileUtil,
  DividerBevel, uBaseERPDBClasses, uAccounting;

type

  { TfWizardNewAccount }

  TfWizardNewAccount = class(TForm)
    bAbort0: TButton;
    bAbort3: TButton;
    bAbort1: TButton;
    bAbort4: TButton;
    bNext0: TButton;
    bNext3: TButton;
    bNext1: TButton;
    bNext4: TButton;
    bPrev0: TButton;
    bPrev3: TButton;
    bPrev1: TButton;
    bPrev4: TButton;
    bvleft: TBevel;
    bvRight0: TBevel;
    bvRight1: TBevel;
    bvRight2: TBevel;
    bvRight3: TBevel;
    cbAccount: TComboBox;
    CheckBox1: TCheckBox;
    cbExisting: TComboBox;
    cbExistingUser: TComboBox;
    DividerBevel1: TDividerBevel;
    eName1: TEdit;
    eUsername: TEdit;
    eAccountNo: TEdit;
    eCustomerID: TEdit;
    eWebAddress: TEdit;
    eSortcode: TEdit;
    eName: TEdit;
    imDialog: TImage;
    lAccountNo1: TLabel;
    lDescription3: TLabel;
    lFinTsName: TLabel;
    lName1: TLabel;
    lSortcode: TLabel;
    lAccountNo: TLabel;
    lAccountName: TLabel;
    lDescription2: TLabel;
    lName: TLabel;
    lAccount: TLabel;
    lDescription0: TLabel;
    lDescription1: TLabel;
    Panel1: TPanel;
    pButtons0: TPanel;
    pButtons1: TPanel;
    pButtons2: TPanel;
    pButtons3: TPanel;
    pCont0: TPanel;
    pCont3: TPanel;
    pCont1: TPanel;
    pCont4: TPanel;
    pLeft: TPanel;
    rbManualAccount: TRadioButton;
    rbMobileTan: TRadioButton;
    rbExistingUser: TRadioButton;
    rbHBCI: TRadioButton;
    rbImportManualAccounts1: TRadioButton;
    rbPINTAN: TRadioButton;
    procedure bAbort0Click(Sender: TObject);
    procedure bNext0Click(Sender: TObject);
    procedure bPrev0Click(Sender: TObject);
    procedure cbExistingSelect(Sender: TObject);
    procedure CheckBox1Change(Sender: TObject);
    procedure ProcLineWritten(Line: string);
    procedure rbExistingUserChange(Sender: TObject);
  private
    { private declarations }
    Steps : array of Integer;
    Dialog1,Dialog2 : string;
    LastDialogTime : LongWord;
    DontHide: Boolean;
    procedure ListAccounts;
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
{$R *.lfm}
uses uData,uMain,uLogWait,LCLIntf,uAccountingque,uMainTreeFrame,usimpleprocess;
resourcestring
  strAQBankingisnotinstalled                     = 'aqBanking ist nicht installiert ! aqBanking muss installiert sein um Online Banking zu bretreiben';
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

procedure TfWizardNewAccount.cbExistingSelect(Sender: TObject);
var
  aAcc: TAccounts;
begin
  aAcc := TAccounts.Create(nil,Data);
  aAcc.Open;
  if aAcc.DataSet.Locate('NAME',cbExisting.Text,[]) then
    begin
      eAccountNo.Text:=aAcc.FieldByName('ACCOUNTNO').AsString;
      eSortcode.Text:=aAcc.FieldByName('SORTCODE').AsString;
    end;
  aAcc.Free;
end;

procedure TfWizardNewAccount.CheckBox1Change(Sender: TObject);
var
  aAcc: TAccounts;
begin
  cbExisting.Enabled:=CheckBox1.Checked;
  if cbExisting.Enabled then
    begin
      cbExisting.Clear;
      aAcc := TAccounts.Create(nil,Data);
      aAcc.Open;
      with aAcc.DataSet do
        begin
          First;
          while not EOF do
            begin
              cbExisting.Items.Add(FieldByName('NAME').AsString);
              Next;
            end;
        end;
      aAcc.Free;
    end;
end;

procedure TfWizardNewAccount.ProcLineWritten(Line: string);
var
  Value : string;
begin
  if copy(Line,0,6) = '===== ' then
    begin
      Dialog1 := copy(Line,7,length(Line));
      Dialog1 := copy(Dialog1,0,pos(' =====',Dialog1)-1);
      fLogWaitForm.ShowInfo(Line);
      LastDialogTime := GetTickCount;
    end
  else if (Dialog1 <> '') then
    begin
      Dialog2 := Dialog2+lineending+Line;
      fLogWaitForm.ShowInfo(Line);
      LastDialogTime := GetTickCount;
    end
  else if pos(':',copy(line,0,3)) = 2 then
    begin
      if pos('Creating crypttoken (DDV)',line) > 0 then
        fLogWaitForm.ShowInfo(strWaitingforCard)
      else if pos('Error performing queue',line) > 0 then
        DontHide := True
      else if pos('Input',line) > 0 then
      else
        begin
          line := copy(line,pos(':',line)+1,length(line));
          line := copy(line,pos(':',line)+1,length(line));
          line := copy(line,pos(':',line)+1,length(line));
          line := copy(line,pos(':',line)+1,length(line));
          line := copy(line,pos(':',line)+1,length(line));
          fLogWaitForm.ShowInfo(trim(Line))
        end;
    end
  else if trim(line) <> '' then
    fLogWaitForm.ShowInfo(Line);
  //Output.Add(line);
end;

procedure TfWizardNewAccount.rbExistingUserChange(Sender: TObject);
begin
  ListAccounts;
  cbExistingUser.Items.Assign(cbAccount.Items);
end;

procedure TfWizardNewAccount.ListAccounts;
var
  CmdLn: String;
  tmp: String;
  atmp: String;
  btmp: String;
begin
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
      if pos(#10,tmp) > 0 then
        begin
          atmp := copy(tmp,0,pos(#10,tmp)-1);
          tmp := copy(tmp,pos(#10,tmp)+1,length(tmp));
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

      btmp := StringReplace(atmp,' ','-',[rfReplaceAll])+' '+btmp;
      cbAccount.Items.Add(btmp);
    end;
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
  aName: String;
begin
  //Wizard finished, use the made settings
  aAccounts := TAccounts.Create(nil,Data);
  aAccounts.CreateTable;
  if Steps[length(Steps)-2] = 1 then
    begin
      with aAccounts do
        begin
          Insert;
          FieldByName('TYPE').AsString := 'BNC';
          tmp := cbAccount.text;
          aName := copy(tmp,0,pos(' ',tmp)-1);
          tmp := copy(tmp,pos(' ',tmp)+1,length(tmp));
          FieldByName('SORTCODE').AsString := copy(tmp,0,pos(' ',tmp)-1);
          tmp := copy(tmp,pos(' ',tmp)+1,length(tmp));
          FieldByName('ACCOUNTNO').AsString := tmp;
          FieldByName('NAME').AsString := trim(aName);
          Post;
          TN := fMainTreeFrame.tvMain.Items.AddChildObject(fMainTreeFrame.tvMain.Selected.Parent,'',TTreeEntry.Create);
          TTreeEntry(TN.Data).Rec := aAccounts.GetBookmark;
          TTreeEntry(TN.Data).Text[0] := trim(aName);
          TTreeEntry(TN.Data).Typ := etAccount;
        end;
    end
  else if rbExistingUser.Checked then
    begin
      with aAccounts do
        begin
          Insert;
          FieldByName('TYPE').AsString := 'BNC';
          tmp := cbExistingUser.text;
          aName := copy(tmp,0,pos(' ',tmp)-1);
          tmp := copy(tmp,pos(' ',tmp)+1,length(tmp));
          FieldByName('SORTCODE').AsString := copy(tmp,0,pos(' ',tmp)-1);
          tmp := copy(tmp,pos(' ',tmp)+1,length(tmp));
          FieldByName('ACCOUNTNO').AsString := tmp;
          FieldByName('NAME').AsString := trim(aName);
          Post;
          TN := fMainTreeFrame.tvMain.Items.AddChildObject(fMainTreeFrame.tvMain.Selected.Parent,'',TTreeEntry.Create);
          TTreeEntry(TN.Data).Rec := aAccounts.GetBookmark;
          TTreeEntry(TN.Data).Text[0] := trim(aName);
          TTreeEntry(TN.Data).Typ := etAccount;
        end;
    end;
  aAccounts.Free;
end;

function TfWizardNewAccount.DoExecStep(Step: Integer): Integer;
var
  tmp: String;
  atmp: String;
  btmp : string;
  CmdLn: String;
  Proc: TExtendedProcess;
begin
  //Do whatever to do in these Step and return the next step
  case Step of
  0:
    begin
      if rbPINTAN.Checked
      or rbHBCI.Checked
      then
        begin
          Result := 1;
        end
      else if rbExistingUser.Checked then
        begin
          Result := 4;
        end
      else if rbManualAccount.Checked then
        Result := 5;
    end;
  1:
    begin
      Result := 3;
      try
        //TODO: packages sollten installiert sein: libccid,pcscd,libifd-cyberjack6,libchipcard-libgwenhywfar60-plugins in
        //TODO: HBCI Adressen: http://www.hbci-zka.de/
        fLogWaitForm.Show;
        if rbPINTAN.Checked then
          CmdLn := 'aqhbci-tool4 adduser -t pintan --context=1 -u '+eAccountNo.Text+' -b '+eSortcode.Text+' --username="'+eUsername.Text+'"';
        if rbHBCI.Checked then
          CmdLn := 'aqhbci-tool4 adduser -t pintan --context=1 -u '+eAccountNo.Text+' -b '+eSortcode.Text+' --username="'+eUsername.Text+'"';
        Proc := TExtendedProcess.Create(Cmdln);
        Proc.OnLineWritten:=@ProcLineWritten;
        if eCustomerID.Text<>'' then
          CmdLn += ' -c '+eCustomerID.Text;
        //--hbciversion=300
        if eWebAddress.Text<>'' then
          CmdLn += ' -s '+eWebAddress.Text;

        {$IFDEF MSWINDOWS}
        CmdLn := AppendPathDelim(AppendPathDelim(ExtractFilePath(Application.Exename))+'tools')+CmdLn;
        {$ENDIF}
        Proc.Execute;
        while Proc.Active do
          begin
            Application.ProcessMessages;
            sleep(100);
          end;
        Proc.Free;
        fLogWaitForm.Hide;
        //tmp := ExecProcessEx(CmdLn,AppendPathDelim(ExtractFileDir(Application.Exename))+'tools');
      except
        fLogWaitForm.Hide;
        tmp := '';
      end;
      if pos(':',tmp)>0 then
        begin
          Showmessage(tmp) ;
          Result := 1;
          exit;
        end;
      ListAccounts;
    end;
  2:Result := 3;
  end;
end;

procedure TfWizardNewAccount.InitWizard;
var
  i : Integer;
  CmdLn: String;
  tmp: String;
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
  SetLanguage;
  pCont0.Visible := True;
  //bNext3.Caption := strFinish;
  CmdLn := 'aqbanking-cli';
  {$IFDEF MSWINDOWS}
  CmdLn := AppendPathDelim(AppendPathDelim(ExtractFilePath(Application.Exename))+'tools')+CmdLn;
  {$ENDIF}
  tmp := ExecProcessEx(CmdLn,AppendPathDelim(ExtractFileDir(Application.Exename))+'tools');
  if tmp = '' then
    begin
      rbExistingUser.Enabled:=False;
      rbPINTAN.Enabled:=False;
      rbMobileTan.Enabled:=False;
      rbHBCI.Enabled:=False;
      Showmessage(strAQBankingisnotinstalled);
    end;
end;

initialization

end.
