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
Created 01.06.2006
*******************************************************************************}

unit uPersonalOptions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons, EditBtn, StdCtrls, uIntfStrConsts, uRichFrame, uColorFrame, ComCtrls,
  CheckLst, Spin, db, LCLType, ColorBox, Grids, DBgrids, ButtonPanel,uBaseApplication,
  uBaseDBInterface;

type

  { TfPersonalOptions }

  TfPersonalOptions = class(TForm)
    bAdd: TBitBtn;
    bAddJob: TSpeedButton;
    bDelete: TBitBtn;
    bDeleteJob: TSpeedButton;
    ButtonPanel1: TButtonPanel;
    cbCategory: TComboBox;
    cbMinimizeonStartTime: TCheckBox;
    cbStartwithStandart: TCheckBox;
    clbMailAccounts: TCheckListBox;
    clbAllowedPhoneLines: TCheckListBox;
    eJob: TComboBox;
    eLink: TEditButton;
    eProject: TEditButton;
    eTempDirectory: TDirectoryEdit;
    fAlternativeRowColor: TfColorFrame;
    fCustomerBackground: TfColorFrame;
    fInfoColor: TfColorFrame;
    fMasterdataBackground: TfColorFrame;
    fMessageBackground: TfColorFrame;
    fOrderBackground: TfColorFrame;
    fSignature: TfRichFrame;
    GroupBox1: TGroupBox;
    iLink: TImage;
    ImageList1: TImageList;
    Label1: TLabel;
    lActiveEntry: TLabel;
    lCheckForMailevery: TLabel;
    lAllowedPhoneLines: TLabel;
    lEntry: TLabel;
    lJob1: TLabel;
    lNotes: TLabel;
    lProject: TLabel;
    lSignature: TLabel;
    ltempDirectory: TLabel;
    mNotes: TMemo;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    pcTabs: TPageControl;
    pgColorTabs: TPageControl;
    rbDeleteDoD522022: TRadioButton;
    rbDeleteNormal: TRadioButton;
    rbDeleteSecure: TRadioButton;
    rbOverride: TRadioButton;
    seMailCheckTime: TSpinEdit;
    tsTimeregistering: TTabSheet;
    tsForms: TTabSheet;
    tsCommon: TTabSheet;
    tsColors: TTabSheet;
    tsMail: TTabSheet;
    tsTelephonie: TTabSheet;
    tsDocumentmanagement: TTabSheet;
    procedure bAddClick(Sender: TObject);
    procedure bDeleteClick(Sender: TObject);
    procedure clbAllowedPhoneLinesClick(Sender: TObject);
    procedure clbMailAccountsDblClick(Sender: TObject);
    procedure eLinkButtonClick(Sender: TObject);
    procedure eLinkEditingDone(Sender: TObject);
    procedure eProjectButtonClick(Sender: TObject);
    procedure eProjectEditingDone(Sender: TObject);
    procedure fAlternativeRowColorChangeColor(Sender: TObject);
    procedure fCustomerBackgroundChangeColor(Sender: TObject);
    procedure fMasterdataBackgroundChangeColor(Sender: TObject);
    procedure fMessageBackgroundChangeColor(Sender: TObject);
    procedure fOrderBackgroundChangeColor(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    function fSearchOpenItem(aLink: string): Boolean;
    function fSearchOpenEntry(aLink: string): Boolean;
  private
    { private declarations }
    FMailAccounts : string;
    FStandartProject : string;
    FStandartLink : string;
    FStandartCategory : string;
  public
    { public declarations }
    procedure DoSetup;
    function Execute : Boolean;
    procedure SetLanguage;
  end; 

var
  fPersonalOptions: TfPersonalOptions;

implementation

uses
{$IFDEF MAINAPP}
  uPhones,uOrders,uArticle,uCustomer,uMessages,uMailOptions,
{$ENDIF}
  uSearch,uData
  ;

{ TfPersonalOptions }

procedure TfPersonalOptions.clbAllowedPhoneLinesClick(Sender: TObject);
var
  i: Integer;
  tmp: String;
begin
  tmp := '';
  for i := 0 to clbAllowedPhoneLines.Count-1 do
    if not clbAllowedPhoneLines.Checked[i] then
      tmp := tmp+clbAllowedPhoneLines.Items[i]+';';
  with Application as IBaseApplication do
    Config.WriteString('PHONELINES',tmp);
end;

procedure TfPersonalOptions.clbMailAccountsDblClick(Sender: TObject);
var
  i: Integer;
  tmp: String;
  tmpn : string = '';
  tmpa: String;
begin
  {$IFDEF MAINAPP}
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
    end;
  tmp := copy(tmp,pos('|',tmp),length(tmp));
  fMailOptions.eServertypeSelect(nil);
  if fMailOptions.Execute then
    with fMailOptions do
      begin
        tmpa := eServertype.Text+';'+eServer.Text+';'+eUsername.Text+';'+ePassword.Text+';YES;';
        if eServertype.text = 'SMTP' then
          tmpa := tmpa+eName.Text+';'+eMailAddr.Text+';';
        tmpn := tmpn+tmpa+tmp;
        FMailAccounts := tmpn;
      end;
  {$ENDIF}
end;

procedure TfPersonalOptions.eLinkButtonClick(Sender: TObject);
begin
  fSearch.SetLanguage;
  fSearch.OnOpenItem:=@fSearchOpenEntry;
  fSearch.Execute(True,'TIMELINK',strSearchfromTimeregisteringMode);
end;

procedure TfPersonalOptions.eLinkEditingDone(Sender: TObject);
begin
  FStandartLink := eLink.Text;
end;

procedure TfPersonalOptions.eProjectButtonClick(Sender: TObject);
begin
  fSearch.SetLanguage;
  fSearch.OnOpenItem:=@fSearchOpenItem;
  fSearch.Execute(True,'TIMEPROJ',strSearchfromTimeregisteringMode);
end;

procedure TfPersonalOptions.eProjectEditingDone(Sender: TObject);
begin
  FStandartproject := eProject.Text;
end;

procedure TfPersonalOptions.fAlternativeRowColorChangeColor(Sender: TObject);
var
  aForm: Integer;

  procedure SetGridsColor(Control : TWinControl);
  var
    aControl: Integer;
  begin
    for aControl := 0 to Control.ControlCount-1 do
      begin
        if Control.Controls[aControl] is TDBGrid then
          TDBGrid(Control.Controls[aControl]).AlternateColor := fAlternativeRowColor.Color
        else if Control.Controls[aControl] is TWinControl then
          SetGridsColor(TWinControl(Control.Controls[aControl]));
      end;
  end;

begin
  for aForm := 0 to Screen.FormCount-1 do
    SetGridsColor(Screen.Forms[aForm]);
end;

procedure TfPersonalOptions.fCustomerBackgroundChangeColor(Sender: TObject);
begin
  {$IFDEF MAINAPP}
  fCustomer.Color:=fCustomerBackground.Color;
  {$ENDIF}
end;

procedure TfPersonalOptions.fMasterdataBackgroundChangeColor(Sender: TObject);
begin
  {$IFDEF MAINAPP}
  fMasterdata.Color:=fMasterdataBackground.Color;
  {$ENDIF}
end;

procedure TfPersonalOptions.fMessageBackgroundChangeColor(Sender: TObject);
begin
  {$IFDEF MAINAPP}
  fMessages.Color:=fMessageBackground.Color;
  {$ENDIF}
end;

procedure TfPersonalOptions.fOrderBackgroundChangeColor(Sender: TObject);
begin
  {$IFDEF MAINAPP}
  fOrders.Color:=fOrderBackground.Color;
  {$ENDIF}
end;

procedure TfPersonalOptions.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
    begin
      Key := 0;
      Close;
    end;
end;

procedure TfPersonalOptions.FormShow(Sender: TObject);
begin
  {$IFDEF MAINAPP}
  pcTabs.ActivePage := tsMail;
  {$ENDIF}
end;

function TfPersonalOptions.fSearchOpenItem(aLink: string): Boolean;
begin
  eProject.Text:=Data.GetLinkDesc(aLink);
  FStandartProject := aLink;
  Result := True;
end;

function TfPersonalOptions.fSearchOpenEntry(aLink: string): Boolean;
begin
  eLink.Text:=Data.GetLinkDesc(aLink);
  FStandartLink := aLink;
  Result := True;
end;

procedure TfPersonalOptions.DoSetup;
begin
  fInfocolor.DefaultColor := clInfoBK;
  fInfoColor.ColorName:='INFOCOLOR';
  fAlternativeRowColor.DefaultColor := $00FFE6E6;
  fAlternativeRowColor.OnChangeColor:=@fAlternativeRowColorChangeColor;
  fAlternativeRowColor.ColorName:='ALTROWCOLOR';
  fOrderBackground.OnChangeColor:=@fOrderBackgroundChangeColor;
  fOrderBackground.DefaultColor := $00FEF9F8;
  fOrderBackground.ColorName:='ORDERS';
  fMasterdataBackground.OnChangeColor:=@fMasterdataBackgroundChangeColor;
  fMasterdataBackground.DefaultColor := $00F5FFF4;
  fMasterdataBackground.ColorName:='MDBK';
  fCustomerBackground.OnChangeColor:=@fCustomerBackgroundChangeColor;
  fCustomerBackground.DefaultColor := $00F5FEFE;
  fCustomerBackground.ColorName:='CUSTBK';
  fMessageBackground.OnChangeColor:=@fMessageBackgroundChangeColor;
  fMessageBackground.DefaultColor := clWindow;
  fMessageBackground.ColorName:='MSGBK';
end;

procedure TfPersonalOptions.bAddClick(Sender: TObject);
var
  tmp: String;
begin
  {$IFDEF MAINAPP}
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
  {$ENDIF}
end;

procedure TfPersonalOptions.bDeleteClick(Sender: TObject);
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

function TfPersonalOptions.Execute: Boolean;
var
  i: Integer;
  tmp,atmp: String;
  ActControl: TWinControl;
begin
  {$IFDEF MAINAPP}
  FmailAccounts := '';
  fSignature.rmText.HandleNeeded;
  fSignature.AsText:='%MESSAGE%';
  if Data.Locate(Data.Options,'OPTION','SIGNATURE',[loCaseInsensitive,loPartialKey]) then
    fSignature.AsRichText := Data.Options.FieldByName('VALUE').AsString;
  if Data.Locate(Data.Options,'OPTION','MAILACCOUNTS',[loCaseInsensitive,loPartialKey]) then
    FMailAccounts := Data.Options.FieldByName('VALUE').AsString;
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
  clbAllowedPhoneLines.Clear;
  for i := 0 to uPhones.Phones.Count-1 do
    clbAllowedPhoneLines.Checked[clbAllowedPhoneLines.Items.Add(uPhones.Phones.Phones[i].Name)] := True;
  tmp := fMain.Properties.StoredValue['PHONELINES'];
  while pos(';',tmp) > 0 do
    begin
      if clbAllowedPhoneLines.Items.IndexOf(copy(tmp,0,pos(';',tmp)-1)) > -1 then
        clbAllowedPhoneLines.Checked[clbAllowedPhoneLines.Items.IndexOf(copy(tmp,0,pos(';',tmp)-1))] := false;
      tmp := copy(tmp,pos(';',tmp)+1,length(tmp));
    end;
  eTempDirectory.text := fMain.Properties.StoredValue['TEMPPATH'];
  case StrToIntDef(fMain.Properties.StoredValue['DELETEMETHOD'],0) of
  0:rbDeleteNormal.Checked := True;
  1:rbDeleteSecure.Checked := True;
  2:rbDeleteDoD522022.Checked := True;
  3:rbOverride.Checked := True;
  end;
  {$ENDIF}
  with Application as IBaseDBInterface do
    begin
      cbMinimizeonStartTime.Checked := DBConfig.ReadString('TIMEREGCLOSEONSTART','Y') <> 'N';
      cbStartwithStandart.Checked := DBConfig.ReadString('TIMESTANDARTSTART','Y') = 'Y';
      eJob.Items.Text:=DBConfig.ReadString('JOBS','');
      eProject.Text:=Data.GetLinkDesc(DBConfig.ReadString('TIMEPROJECT',''));
      FStandartProject := DBConfig.ReadString('TIMEPROJECT','');
      eLink.Text:=Data.GetLinkDesc(DBConfig.ReadString('TIMELINK',''));
      FStandartLink := DBConfig.ReadString('TIMELINK','');
      eJob.Text:=DBConfig.ReadString('TIMEJOB','');
      mNotes.Text:=DBConfig.ReadString('TIMENOTES','');
    end;
  ActControl := Screen.ActiveControl;
  Result := Showmodal = mrOK;
  try
    if Assigned(ActControl) and ActControl.CanFocus then ActControl.SetFocus;
  except
  end;
  if Result then
    begin
      {$IFDEF MAINAPP}
      fMain.Properties.StoredValue['TEMPPATH'] := eTempDirectory.Text;
      if rbDeleteNormal.Checked then
        fMain.Properties.StoredValue['DELETEMETHOD'] := '0'
      else if rbDeleteSecure.Checked then
        fMain.Properties.StoredValue['DELETEMETHOD'] := '1'
      else if rbDeleteDoD522022.Checked then
        fMain.Properties.StoredValue['DELETEMETHOD'] := '2'
      else if rbOverride.Checked then
        fMain.Properties.StoredValue['DELETEMETHOD'] := '3';
      Data.SetUserOptionValue('MAILACCOUNTS',FMailAccounts);
      Data.SetUserOptionValue('SIGNATURE',fSignature.AsRichText);
      {$ENDIF}
      with Application as IBaseDBInterface do
        begin
          if not cbMinimizeonStartTime.Checked then
            DBConfig.WriteString('TIMEREGCLOSEONSTART','N')
          else
            DBConfig.WriteString('TIMEREGCLOSEONSTART','');
          if not cbStartwithStandart.Checked then
            DBConfig.WriteString('TIMESTANDARTSTART','N')
          else
            DBConfig.WriteString('TIMESTANDARTSTART','');
          DBConfig.WriteString('TIMEPROJECT',FStandartProject);
          DBConfig.WriteString('TIMELINK',FStandartLink);
          DBConfig.WriteString('TIMEJOB',eJob.Text);
          DBConfig.WriteString('TIMENOTES',mNotes.Text);
        end;
    end;
end;

procedure TfPersonalOptions.SetLanguage;
begin
  fInfoColor.InfoCaption:=strInfoColor;
  fAlternativeRowColor.InfoCaption:=strAlternativeRowcolor;
  fOrderBackground.InfoCaption:=strOrders;
  fMasterdataBackground.InfoCaption:=strMasterdata;
  fCustomerBackground.InfoCaption:=strCustomers;
  fMessageBackground.InfoCaption:=strMessages;
end;

initialization
  {$I upersonaloptions.lrs}

end.
