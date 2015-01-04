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
  ExtCtrls, Buttons,ComCtrls,LCLType, ButtonPanel, Menus, uBaseApplication,
  uBaseDBInterface,Utils;
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
    procedure StartWizardMandant;
  public
    { public declarations }
    function Execute(aHint : string = '';UserSelectable : Boolean = True) : Boolean;
  end; 

var
  fPassword: TfPassword;

implementation
{$R *.lfm}
uses
  uMashineID,uData,UTF8Process,Process,ubaseconfig;
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
            with Application as IBaseDBInterface do
              begin
                if not OpenMandant(mSettings[0],mSettings[1]) then
                  begin
                    lFirstLogin.AutoSize:=True;
                    lFirstLogin.Caption:=LastError;
                    lFirstLogin.Visible:=True;
                    mSettings.Free;
                    exit;
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
  with Application as IBaseDBInterface do
    begin
      if not DirectoryExists(UniToSys(MandantPath)) then
        begin
          if not DoOpenMandant(MandantPath) then exit;
        end
      else if DirectoryExists(UniToSys(MandantPath)) then
        begin
          if not DoOpenMandant(AppendPathDelim(MandantPath)+cbMandant.Text+MandantExtension) then exit;
        end;
      Data.Users.CreateTable;
      Data.Users.Open;
      while not Data.Users.DataSet.EOF do
        begin
          if Data.Users.Leaved.IsNull and (Data.Users.FieldByName('TYPE').AsString <> 'G') and ((not Assigned(Data.Users.FieldByName('LOGINACTIVE'))) or (Data.Users.FieldByName('LOGINACTIVE').AsString<>'N')) then
            cbUser.Items.Add(Data.Users.UserName.AsString);
          Data.Users.DataSet.Next;
        end;
      cbUser.Enabled:=cbUser.Items.Count > 0;
      if cbUSer.Items.IndexOf(aUSer) > 0 then
        begin
          cbUser.ItemIndex:=cbUSer.Items.IndexOf(aUSer);
          cbUserSelect(nil);
        end;
    end;
  lFirstLoginResize(nil);
end;
procedure TfPassword.cbUserSelect(Sender: TObject);
begin
  ePasswort.Enabled := True;
  if Visible then
    ePasswort.SetFocus;
  with Application as IBaseDBInterface do
    lFirstLogin.Visible:=Data.Users.DataSet.Locate('NAME',cbUser.text,[])
                         and (Data.Users.Passwort.IsNull or (Data.Users.Passwort.AsString = ''));
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
  Height := ((ePasswort.Height+8)*3)+ButtonPanel1.Height+lFirstLogin.Height+35{$ifdef LCLGTK2}+5{$endif};
end;

procedure TfPassword.MenuItem1Click(Sender: TObject);
begin
  StartWizardMandant;
end;

procedure TfPassword.OKButtonClick(Sender: TObject);
var
  BaseApplication : IBaseConfig;
begin
  if Supports(Application, IBaseConfig, BaseApplication) then
    begin
      if cbAutomaticLogin.Checked then
        BaseApplication.Config.WriteInteger('AUTOMATICLOGIN',CreateUserID)
      else
        BaseApplication.Config.WriteString('AUTOMATICLOGIN','NO');
      BaseApplication.Config.WriteString('LOGINMANDANT',cbMandant.Text);
      BaseApplication.Config.WriteString('LOGINUSER',cbUser.Text);
    end;
end;

procedure TfPassword.SpeedButton1Click(Sender: TObject);
begin
  StartWizardMandant;
end;

procedure TfPassword.StartWizardMandant;
var
  aProcess: TProcessUTF8;
begin
  aProcess := TProcessUTF8.Create(Self);
  aProcess.CommandLine:=AppendPathDelim(Application.Location)+'wizardmandant'+ExtractFileExt(Application.ExeName);
  if Application.HasOption('c','config-path') then
    aProcess.CommandLine:=aProcess.CommandLine+' "--config-path='+Application.GetOptionValue('c','config-path')+'"';
  if Application.HasOption('database') then
    begin
      aProcess.CommandLine:=aProcess.CommandLine+' "--database='+Application.GetOptionValue('database')+'"';
      aProcess.CommandLine:=aProcess.CommandLine+' --silent';
    end;
  if Application.HasOption('firebird') then
    begin
      aProcess.CommandLine:=aProcess.CommandLine+' --firebird';
    end;
  aProcess.CommandLine:=aProcess.CommandLine+' "--execute='+Application.ExeName+'"';
  aProcess.Options := [poNoConsole];
  try
    aProcess.Execute;
    Application.Terminate;
  except
    aProcess.Free;
    raise Exception.Create(strNoMandants);
  end;
end;

function TfPassword.Execute(aHint : string = '';UserSelectable : Boolean = True): Boolean;
var
  AInfo: TSearchRec;
begin
  Result := False;
  if not Assigned(Self) then
    begin
      Application.CreateForm(TfPassword,fPassword);
      Self := fPassword;
    end;
  lFirstLogin.Caption:='';
  lFirstLogin.Visible:=False;
  lFirstLogin.Height:=0;
  lFirstLoginResize(nil);
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
                        cbMandant.Items.Add(copy(Name,0,length(Name)-length(MandantExtension)));
                    end;
                Until FindNext(ainfo)<>0;
              FindClose(aInfo);
              if cbMandant.Items.Count = 0 then
                begin
                  if FileExists(UniToSys(AppendPathDelim(Application.Location)+'wizardmandant'+ExtractFileExt(Application.ExeName))) then
                    begin
                      if MessageDlg(strStartmandantWizard,mtInformation,[mbYes,mbNo],0) = mrYes then
                        begin
                          StartWizardMandant;
                        end
                      else  raise Exception.Create(strNoMandants);
                    end
                  else raise Exception.Create(strNoMandants);
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
  cbMandant.Enabled:=UserSelectable;
  cbUser.Enabled:=UserSelectable;
  ePasswort.Text:='';
  if aHint <> '' then
    begin
      lFirstLogin.AutoSize:=True;
      lFirstLogin.Visible:=True;
      lFirstLogin.Caption:=aHint;
      lFirstLoginResize(nil);
    end;
  Result := Showmodal = mrOK;
end;
initialization
end.
