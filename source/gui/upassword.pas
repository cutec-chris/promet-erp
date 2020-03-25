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
unit uPassword;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils,  Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Buttons,ComCtrls,LCLType, ButtonPanel, Menus,
  Utils;
resourcestring
  strNoMandants         = 'keine Mandanten gefunden !';
  strStartmandantWizard = 'Es wurde kein Mandant gefunden.'+lineending+'Möchten Sie jetzt einen anlegen ?';
  strFirstLogin         = 'Sie müssen ein neues Passwort vergeben, das Passwort welches Sie eingeben wird ab nun Ihr Passwort sein.';
type

  { TfPassword }

  TfPassword = class(TForm)
    ButtonPanel1: TButtonPanel;
    cbAutomaticLogin: TCheckBox;
    cbMandant: TComboBox;
    cbUser: TComboBox;
    ePasswort: TEdit;
    IdleTimer1: TIdleTimer;
    lFirstLogin: TLabel;
    lPassword: TLabel;
    lUser: TLabel;
    lMandant: TLabel;
    SpeedButton1: TSpeedButton;
    procedure cbMandantSelect(Sender: TObject);
    procedure cbUserExit(Sender: TObject);
    procedure cbUserSelect(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure IdleTimer1Timer(Sender: TObject);
    procedure lFirstLoginResize(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
  private
    { private declarations }
    FUserSelectable: Boolean;
    FSHint: String;
    procedure StartWizardMandant;
    procedure RefreshList;
  public
    { public declarations }
    function Execute(aHint: string=''; aUserSelectable: Boolean=True): Integer;
  end; 

var
  fPassword: TfPassword;

implementation
{$R *.lfm}
uses
  uMashineID,uData,UTF8Process;
{ TfPassword }

procedure TfPassword.cbMandantSelect(Sender: TObject);
var
  aUser: TCaption;
  function DoOpenMandant(aMandantPath : string) : Boolean;
  var
    mSettings: TStringList;
  begin
    Result := False;
    if FileExists(UniToSys(aMandantPath)) then
      begin
        mSettings := TStringList.Create;
        mSettings.LoadFromFile(UniToSys(aMandantPath));
        if mSettings.Count = 2 then
          begin
            aUser := cbUser.Text;
            cbUser.Clear;
            //with Application as IBaseDBInterface do
              begin
                try
                  Data.Connect;
                except
                  on e : exception do
                    begin
                      lFirstLogin.AutoSize:=True;
                      lFirstLogin.Caption:=e.Message;
                      lFirstLogin.Visible:=True;
                      mSettings.Free;
                      exit;
                    end;
                end;
              end;
          end
        else
          begin
            lFirstLogin.AutoSize:=True;
            lFirstLogin.Caption:='Invalid Config file';
            lFirstLogin.Visible:=True;
            exit;
          end;
        mSettings.Free;
      end
    else
      begin
        lFirstLogin.AutoSize:=True;
        lFirstLogin.Caption:='Config File dosend exists';
        lFirstLogin.Visible:=True;
        mSettings.Free;
        exit;
      end;
    Result := True;
  end;
begin
  lFirstLogin.Caption := '';
  lFirstLogin.AutoSize:=False;
  lFirstLogin.Height := 0;
  lFirstLogin.Visible:=False;
  //with Application as IBaseDBInterface do
    begin
      if not DirectoryExists(UniToSys(Data.ConfigPath)) then
        begin
          if not DoOpenMandant(Data.ConfigPath) then exit;
        end
      else if DirectoryExists(UniToSys(Data.ConfigPath)) then
        begin
          if not DoOpenMandant(AppendPathDelim(Data.ConfigPath)+cbMandant.Text) then exit;
        end;
      {
      uData.DataM := GetDB;
      GetDB.Users.CreateTable;
      GetDB.Users.Open;
      GetDB.Users.First;
      while not GetDB.Users.DataSet.EOF do
        begin
          if GetDB.Users.Leaved.IsNull and (GetDB.Users.FieldByName('TYPE').AsString <> 'G') and ((not Assigned(GetDB.Users.FieldByName('LOGINACTIVE'))) or (GetDB.Users.FieldByName('LOGINACTIVE').AsString<>'N')) then
            cbUser.Items.Add(GetDB.Users.UserName.AsString);
          GetDB.Users.DataSet.Next;
        end;
      if cbUSer.Items.IndexOf(aUSer) > 0 then
        begin
          cbUser.ItemIndex:=cbUSer.Items.IndexOf(aUSer);
          cbUserSelect(nil);
        end;
      }
    end;
  lFirstLoginResize(nil);
end;

procedure TfPassword.cbUserExit(Sender: TObject);
begin
  ePasswort.Enabled := cbUser.Text<>'';
  if Visible and ePasswort.Enabled and ePasswort.CanFocus then
    ePasswort.SetFocus;
end;

procedure TfPassword.cbUserSelect(Sender: TObject);
begin
  ePasswort.Enabled := True;
  if ePasswort.CanFocus and Assigned(Sender) then
    ePasswort.SetFocus;
  {
    lFirstLogin.Visible:=Data.Users.DataSet.Locate('NAME',cbUser.text,[])
                         and (Data.Users.Passwort.IsNull or (Data.Users.Passwort.AsString = ''));
  }
  if lFirstLogin.Visible then
    begin
      lFirstLogin.AutoSize:=True;
      lFirstLogin.Caption:=strFirstLogin;
    end;
end;
procedure TfPassword.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
    begin
      Key := 0;
      Close;
    end;
end;
procedure TfPassword.FormShow(Sender: TObject);
begin
  if Visible and (cbMandant.Text <> '') and (cbUser.Text <> '') then
    begin
      if ePasswort.CanFocus then
        ePasswort.SetFocus;
      //Self.BringToFront;
      //IdleTimer1.Enabled:=True;
    end;
end;

procedure TfPassword.IdleTimer1Timer(Sender: TObject);
begin
  IdleTimer1.Enabled:=False;
  SetFocus;
end;

procedure TfPassword.lFirstLoginResize(Sender: TObject);
begin
  Height := ((ePasswort.Height+8)*3)+ButtonPanel1.Height+lFirstLogin.Height+36{$ifdef LCLGTK2}+15{$endif};
end;

procedure TfPassword.MenuItem1Click(Sender: TObject);
begin
  StartWizardMandant;
end;

procedure TfPassword.OKButtonClick(Sender: TObject);
begin
  {
  if Supports(Application, IBaseConfig, BaseApplication) then
    begin
      if cbAutomaticLogin.Checked then
        BaseApplication.Config.WriteInteger('AUTOMATICLOGIN',CreateUserID)
      else
        BaseApplication.Config.WriteString('AUTOMATICLOGIN','NO');
      BaseApplication.Config.WriteString('LOGINMANDANT',cbMandant.Text);
      BaseApplication.Config.WriteString('LOGINUSER',cbUser.Text);
    end;
  }
end;

procedure TfPassword.SpeedButton1Click(Sender: TObject);
begin
  StartWizardMandant;
end;

procedure TfPassword.StartWizardMandant;
begin
  {
  with TfWizardNewMandant.Create(Application) do
    begin
      if Application.HasOption('database') then
        begin
          IsSilent:=True;
        end;
      ShowModal;
      RefreshList;
    end;
  }
end;

procedure TfPassword.RefreshList;
var
  aInfo : TSearchRec;
begin
  lFirstLogin.Caption:='';
  lFirstLogin.Visible:=False;
  lFirstLogin.Height:=0;
  lFirstLoginResize(nil);
  cbMandant.Clear;
  {
  with Application as IBaseApplication,Application as IBaseConfig do
    begin
      if cbMandant.Items.Count = 0 then
        begin
          with Application as IBaseDbInterface do
            begin
              If FindFirst(UniToSys(AppendPathDelim(MandantPath)+'*'+MandantExtension),faAnyFile,AInfo)=0 then
                Repeat
                  With aInfo do
                    begin
                      If (Attr and faDirectory) <> faDirectory then
                        cbMandant.Items.Add(SysToUni(copy(Name,0,length(Name)-length(MandantExtension))));
                    end;
                Until FindNext(ainfo)<>0;
              FindClose(aInfo);
              if cbMandant.Items.Count = 0 then
                begin
                  if MessageDlg(strStartmandantWizard,mtInformation,[mbYes,mbNo],0) = mrYes then
                    begin
                      StartWizardMandant;
                    end
                  else  raise Exception.Create(strNoMandants);
                  exit;
                end;
            end;
        end;
      if (cbMandant.Text = '') then
        begin
          cbUser.Text:='';
          cbMandant.Text := '';
          if cbMandant.Items.IndexOf(Config.ReadString('LOGINMANDANT','')) > -1 then
            begin
              cbMandant.ItemIndex:=cbMandant.Items.IndexOf(Config.ReadString('LOGINMANDANT',''));
              cbMandantSelect(nil);
            end;
          if (cbMandant.Text = '') and (cbMandant.Items.Count = 1) then
            begin
              cbMandant.ItemIndex := 0;
              cbMandantSelect(nil);
            end;
          if cbUser.Items.IndexOf(Config.ReadString('LOGINUSER','')) > -1 then
            begin
              cbUser.Text := Config.ReadString('LOGINUSER','');
              cbUserSelect(nil);
            end;
        end;
    end;
  }
  cbMandant.Enabled:=FUserSelectable;
  cbUser.Enabled:=True;
  ePasswort.Text:='';
  if FSHint <> '' then
    begin
      lFirstLogin.AutoSize:=True;
      lFirstLogin.Visible:=True;
      lFirstLogin.Caption:=FSHint;
      lFirstLoginResize(nil);
    end;
end;

function TfPassword.Execute(aHint : string = '';aUserSelectable : Boolean = True): Integer;
begin
  Result := mrCancel;
  if not Assigned(Self) then
    begin
      Application.CreateForm(TfPassword,fPassword);
      Self := fPassword;
    end;
  FSHint := aHint;
  FUserSelectable := aUserSelectable;
  RefreshList;
  Result := Showmodal;
end;
initialization
end.

