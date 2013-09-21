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
unit uMailOptions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, CheckLst, Spin,
  Buttons, uOptionsFrame, uRichFrame;

type
  TfMessageOptions = class(TOptionsFrame)
    bAdd: TBitBtn;
    bDelete: TBitBtn;
    clbMailAccounts: TCheckListBox;
    fSignature: TfRichFrame;
    lCheckForMailevery: TLabel;
    lSignature: TLabel;
    seMailCheckTime: TSpinEdit;
    procedure bAddClick(Sender: TObject);
    procedure bDeleteClick(Sender: TObject);
    procedure clbMailAccountsDblClick(Sender: TObject);
  private
    { private declarations }
    FMailAccounts : string;
  public
    { public declarations }
    procedure StartTransaction;override;
    procedure CommitTransaction;override;
    procedure RollbackTransaction;override;
  end;

implementation
{$R *.lfm}
uses uBaseDBInterface, uMailEditOptions;

procedure TfMessageOptions.bAddClick(Sender: TObject);
var
  tmp: TCaption;
begin
  fMailOptions.SetLanguage;
  fMailOptions.eServertype.Text := 'POP3';
  fMailOptions.eServertypeSelect(nil);
  fMailOptions.eServer.text := '';
  fMailOptions.ePassword.text := '';
  fMailOptions.eUsername.Text := '';
  if fMailOptions.Execute then
    with fMailOptions do
      begin
        tmp := eServertype.Text+';'+eServer.Text+';'+eUsername.Text+';'+ePassword.Text+';YES;';
        if eServertype.text = 'SMTP' then
          tmp := tmp+eName.Text+';'+eMailAddr.Text+';';
        if eServertype.text = 'FEED' then
          tmp := eServertype.Text+';'+eServer.Text+';;;YES;';
        FMailAccounts := FMailAccounts+tmp+'|';
        clbMailAccounts.Items.Add(eUsername.Text+'@'+eServer.Text);
      end;
end;

procedure TfMessageOptions.bDeleteClick(Sender: TObject);
var
  tmp: String;
  tmpout : string;
  i : Integer;
begin
  tmp := FMailAccounts;
  i := 0;
  tmpout := '';
  while pos('|',tmp) > 0 do
    begin  //Servertype;Server;Username;Password;Active
      if i <> clbMailAccounts.ItemIndex then
        tmpout := tmpout+copy(tmp,0,pos('|',tmp));
      tmp := copy(tmp,pos('|',tmp)+1,length(tmp));
      inc(i);
    end;
  FMailAccounts := tmpOut;
  clbMailAccounts.Items.Delete(clbMailAccounts.ItemIndex);
end;

procedure TfMessageOptions.clbMailAccountsDblClick(Sender: TObject);
var
  i: Integer;
  tmp: String;
  tmpn : string = '';
  tmpa: String;
begin
  fMailOptions.SetLanguage;
  i := 0;
  tmp := FMailAccounts;
  while i < clbMailAccounts.ItemIndex do
    begin
      if pos('|',tmp) = 0 then break;
      tmpn := tmpn+copy(tmp,0,pos('|',tmp));
      tmp := copy(tmp,pos('|',tmp)+1,length(tmp));
      inc(i);
    end;
  fMailOptions.eServertype.Text := copy(tmp,0,pos(';',tmp)-1);
  tmp := copy(tmp,pos(';',tmp)+1,length(tmp));
  fMailOptions.eServer.Text := copy(tmp,0,pos(';',tmp)-1);
  tmp := copy(tmp,pos(';',tmp)+1,length(tmp));
  fMailOptions.eUsername.Text := copy(tmp,0,pos(';',tmp)-1);
  tmp := copy(tmp,pos(';',tmp)+1,length(tmp));
  fMailOptions.ePassword.Text := copy(tmp,0,pos(';',tmp)-1);
  tmp := copy(tmp,pos(';',tmp)+1,length(tmp));
  if fMailOptions.eServertype.Text = 'SMTP' then
    begin
      tmp := copy(tmp,pos(';',tmp)+1,length(tmp));
      fMailOptions.eName.Text := copy(tmp,0,pos(';',tmp)-1);
      tmp := copy(tmp,pos(';',tmp)+1,length(tmp));
      fMailOptions.eMailAddr.Text := copy(tmp,0,pos(';',tmp)-1);
    end
  else if fMailOptions.eServertype.Text = 'POP3' then
    begin
      tmp := copy(tmp,pos(';',tmp)+1,length(tmp));
      if copy(tmp,0,2) <> 'L:' then
        begin
          fMailOptions.cbArchive.Checked := copy(tmp,0,pos(';',tmp)-1) = 'YES';
          tmp := copy(tmp,pos(';',tmp)+1,length(tmp));
          fMailOptions.cbDelete.Checked := copy(tmp,0,pos(';',tmp)-1) = 'YES';
          tmp := copy(tmp,pos(';',tmp)+1,length(tmp));
        end;
    end;
  tmp := copy(tmp,pos('|',tmp),length(tmp));
  fMailOptions.eServertypeSelect(nil);
  if fMailOptions.Execute then
    with fMailOptions do
      begin
        tmpa := eServertype.Text+';'+eServer.Text+';'+eUsername.Text+';'+ePassword.Text+';YES;';
        if eServertype.text = 'SMTP' then
          tmpa := tmpa+eName.Text+';'+eMailAddr.Text+';';
        if eServertype.text = 'POP3' then
          begin
            if cbArchive.Checked then
              tmpa := tmpa+'YES;'
            else
              tmpa := tmpa+'NO;';
            if cbDelete.Checked then
              tmpa := tmpa+'YES;'
            else
              tmpa := tmpa+'NO;';
          end;
        tmpn := tmpn+tmpa+tmp;
        FMailAccounts := tmpn;
      end;
end;

procedure TfMessageOptions.StartTransaction;
var
  tmp: String;
  atmp: String;
begin
  inherited StartTransaction;
  fSignature.rmText.HandleNeeded;
  fSignature.AsText:='%MESSAGE%';
  with Application as IBaseDBInterface do
    begin
      fSignature.AsRichText := DBConfig.ReadString('SIGNATURE','%MESSAGE%');
      FMailAccounts := DBConfig.ReadString('MAILACCOUNTS','');
    end;
  clbMailAccounts.Items.Clear;
  tmp := FMailAccounts;
  while pos('|',tmp) > 0 do
    begin  //Servertype;Server;Username;Password;Active
      tmp := copy(tmp,pos(';',tmp)+1,length(tmp));
      atmp := copy(tmp,0,pos(';',tmp)-1);
      tmp := copy(tmp,pos(';',tmp)+1,length(tmp));
      atmp := copy(tmp,0,pos(';',tmp)-1)+'@'+atmp;
      clbMailAccounts.Items.Add(atmp);
      tmp := copy(tmp,pos(';',tmp)+1,length(tmp));
      tmp := copy(tmp,pos(';',tmp)+1,length(tmp));
      if copy(tmp,0,pos(';',tmp)-1) = 'YES' then
        clbMailAccounts.Checked[clbMailAccounts.Count-1] := True;
      tmp := copy(tmp,pos('|',tmp)+1,length(tmp));
    end;
end;

procedure TfMessageOptions.CommitTransaction;
begin
  inherited CommitTransaction;
  with Application as IBaseDBInterface do
    begin
      DBConfig.WriteString('MAILACCOUNTS',FMailAccounts);
      DBConfig.WriteString('SIGNATURE',fSignature.AsRichText);
    end;
end;

procedure TfMessageOptions.RollbackTransaction;
begin
  inherited RollbackTransaction;
end;

end.

