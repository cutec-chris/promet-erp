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
Created 04.04.2011
*******************************************************************************}
unit umain;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, DBGrids,
  Buttons, Menus, ActnList, XMLPropStorage, StdCtrls, Utils,
  uIntfStrConsts, db, memds, FileUtil, Translations, md5, simpleipc,
  ComCtrls, ExtCtrls, DbCtrls, Grids, LMessages;
type

  { TfMain }

  TfMain = class(TForm)
    acAddDirectory: TAction;
    acBack: TAction;
    acDeleteDirectory: TAction;
    acDeleteMandant: TAction;
    acDeleteWholeMessageDir: TAction;
    acForward: TAction;
    acHelpIndex: TAction;
    acNewCustomer: TAction;
    acNewMandant: TAction;
    acNewOrder: TAction;
    acNewTask: TAction;
    acNewTermin: TAction;
    acPersonalOptions: TAction;
    acProperties: TAction;
    acRegister: TAction;
    acTimeRegistering: TAction;
    acLogin: TAction;
    acLogout: TAction;
    acOptions: TAction;
    acHelp: TAction;
    acChangePasswort: TAction;
    acInfo: TAction;
    ActionList1: TActionList;
    ApplicationProperties1: TApplicationProperties;
    IdleTimer1: TIdleTimer;
    MainMenu: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    miHelpIndex: TMenuItem;
    miHelp: TMenuItem;
    MenuItem7: TMenuItem;
    miBugtracker: TMenuItem;
    miDeletemandant: TMenuItem;
    miLanguage: TMenuItem;
    miMandant: TMenuItem;
    miNewMandant: TMenuItem;
    miOptions: TMenuItem;
    miProperties: TMenuItem;
    miRegister: TMenuItem;
    pmTray: TPopupMenu;
    SimpleIPCServer1: TSimpleIPCServer;
    procedure acChangePasswortExecute(Sender: TObject);
    procedure acHelpExecute(Sender: TObject);
    procedure acInfoExecute(Sender: TObject);
    procedure acLoginExecute(Sender: TObject);
    procedure acLogoutExecute(Sender: TObject);
    procedure acOptionsExecute(Sender: TObject);
    procedure ApplicationProperties1ShowHint(var HintStr: string;
      var CanShow: Boolean; var HintInfo: THintInfo);
    procedure fEnterTimeMinimize(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure IdleTimer1Timer(Sender: TObject);
    procedure LanguageItemClick(Sender: TObject);
    procedure SimpleIPCServer1Message(Sender: TObject);
    procedure TrayIcon1Click(Sender: TObject);
    procedure IntSetLanguage(aLang : string);
  private
    { private declarations }
    MenueRegistered : Boolean;
    function CommandReceived(Sender : TObject;aCommand : string) : Boolean;
    procedure RegisterMenue;
  public
    { public declarations }
    History : TStringList; //Fake
    procedure DoCreate;
    procedure SetLanguage;
    procedure SetRights;
    procedure SetupDB;
  end;
var
  fMain: TfMain;
implementation
uses
  uPassword,uError,uMashineID,uEnterTime,uprometipc,
  uBaseDBInterface, uBaseApplication, uSearch, uOptions, uTimeOptions,
  uHelpContainer,uData,uBaseSearch,uProjects,uMasterdata,uPerson,uBaseVisualApplication,
  uInfo;
resourcestring
  strNotLoggedin                        = 'Sie sind nicht angemeldet, melden Sie sich an um Zeiten erfassen zu können !';

procedure TfMain.FormShow(Sender: TObject);
var
  sl: TStringList;
  i: Integer;
  aNewItem: TMenuItem;
begin
  with Application as IBaseApplication do
    RestoreConfig; //Must be called when Mainform is Visible
  with BaseApplication as IBaseApplication do
    begin
      if Language = '' then
        Language := 'deutsch';
      IntSetLanguage(Language);
      miLanguage.Clear;
      sl := TStringList.Create;
      if FileExistsUTF8(AppendPathDelim(AppendPathDelim(ProgramDirectory) + 'languages')+'languages.txt') then
        sl.LoadFromFile(UTF8ToSys(AppendPathDelim(AppendPathDelim(ProgramDirectory) + 'languages')+'languages.txt'));
      for i := 0 to sl.Count-1 do
        begin
          aNewItem := TMenuItem.Create(miLanguage);
          aNewItem.Caption := sl[i];
          aNewItem.AutoCheck := True;
          aNewItem.OnClick :=@LanguageItemClick;
          aNewItem.GroupIndex := 11;
          miLanguage.Add(aNewItem);
          if UpperCase(aNewItem.Caption) = UpperCase(Language) then
            begin
              aNewItem.Checked := True;
            end;
        end;
      sl.Free;
    end;
  if not acLogin.Enabled then exit;
  with Application as IBaseDBInterface do
    if not Assigned(Data) then
      begin
        acLogin.Execute;
      end;
  fEnterTime.acGotoID.Visible:=False;
  fEnterTime.Timer.Enabled:=True;
end;
procedure TfMain.IdleTimer1Timer(Sender: TObject);
var
  sl: TStringList;
  i: Integer;
  achanged: Boolean;
begin
  if SimpleIPCServer1.Active then
    SimpleIPCServer1.PeekMessage(100,True);
  PeekIPCMessages;
  if not MenueRegistered then RegisterMenue;
end;
procedure TfMain.LanguageItemClick(Sender: TObject);
var
  i: Integer;
begin
  with BaseApplication as IBaseApplication do
    begin
      for i := 0 to miLanguage.Count-1 do
        if miLanguage[i].Caption = Language then
          miLanguage[i].Checked := false;
      TmenuItem(Sender).Checked := True;
      Language := TmenuItem(Sender).Caption;
      IntSetLanguage(Language);
    end;
end;
procedure TfMain.SimpleIPCServer1Message(Sender: TObject);
begin
  CommandReceived(nil,TSimpleIPCServer(Sender).StringMessage);
end;
function MessageReceived(aMessage: string): Boolean;
begin
  Result := fMain.CommandReceived(nil,aMessage);
end;
procedure TfMain.RegisterMenue;
begin
  SendIPCMessage('AddMenue(Zeiterfassung/zeigen\verstecken)');
  if SendIPCMessage('AddMenue(Zeiterfassung/Standardeintrag starten)') then
    begin
      fEnterTime.acMinimizeToTray.Enabled:=True;
      fEnterTime.acMinimizeToTray.Visible:=True;
      MenueRegistered:=True;
      uprometipc.OnMessageReceived:=@MessageReceived;
    end;
end;
procedure TfMain.TrayIcon1Click(Sender: TObject);
begin
end;
procedure TfMain.IntSetLanguage(aLang: string);
begin
  LoadLanguage(aLang);
end;
function TfMain.CommandReceived(Sender: TObject; aCommand: string): Boolean;
begin
  Result := fEnterTime.CommandReceived(Sender,aCommand);
  if not Result then
    begin
      if aCommand = 'OnClick(/Zeiterfassung/zeigen\verstecken)' then
        begin
          if FMain.Visible then
            begin
              if fMain.WindowState=wsMinimized then fMain.WindowState:=wsNormal;
              fMain.Hide
            end
          else
            begin
              fMain.Show;
              if fMain.WindowState=wsMinimized then fMain.WindowState:=wsNormal;
            end;
          Result := True;
        end
    end;
end;
procedure TfMain.acLoginExecute(Sender: TObject);
begin
  with Application as IBaseApplication do
    if not Login then
      begin
        Application.Terminate;
        exit;
      end;
  fEnterTime.DoSetup;
  SetupDB;
  acLogin.Enabled:=false;
  acLogout.Enabled:=True;
  fOptions.RegisterOptionsFrame(TfTimeOptions.Create(fOptions),Caption,strPersonalOptions);
  if Data.Users.Rights.Right('PROJECTS') > RIGHT_NONE then
    begin
      AddSearchAbleDataSet(TProjectList);
    end;
  if Data.Users.Rights.Right('MASTERDATA') > RIGHT_NONE then
    begin
      AddSearchAbleDataSet(TMasterdataList);
    end;
  if Data.Users.Rights.Right('CUSTOMERS') > RIGHT_NONE then
    begin
      AddSearchAbleDataSet(TPersonList);
      AddSearchAbleDataSet(TPersonContactData);
      AddSearchAbleDataSet(TPersonAddress);
    end;
  SetRights;
  SimpleIPCServer1.Active:=True;
  RegisterMenue;
end;

procedure TfMain.acHelpExecute(Sender: TObject);
var
  aCreated: Boolean = False;
begin
  if not Assigned(fHelpContainer) then
    begin
      fHelpContainer.SetLanguage;
      aCreated := True;
    end;
  fHelpContainer.Show;
  if aCreated then
    fHelpContainer.WikiFrame.OpenWikiPage('TimeRegistering-Help/index',True);
end;

procedure TfMain.acChangePasswortExecute(Sender: TObject);
begin
  with BaseApplication as IBaseApplication do
    begin
      fEnterTime.StopActualTime;
      ChangePasswort;
    end;
  fEnterTime.DoSetup;
end;

procedure TfMain.acInfoExecute(Sender: TObject);
begin
  fInfo.SetLanguage;
  with Application as IBaseApplication do
    begin
     fInfo.Version:=AppVersion;
     fInfo.Revision:=AppRevision;
     fInfo.ProgramName:='Promet-ERP-Timeregistering';
     fInfo.InfoText:=vInfo;
     fInfo.Copyright:='2006-2013 C. Ulrich';
    end;
  fInfo.SetLanguage;
  fInfo.Execute;
end;

procedure TfMain.acLogoutExecute(Sender: TObject);
begin
  if Assigned(fOptions) then
    FreeAndNil(fOptions);
  if Assigned(fHelpContainer) then
    FreeAndNil(fHelpContainer);
  fEnterTime.StopActualTime;
  with Application as IBaseApplication do
    Logout;
  Close;
end;
procedure TfMain.acOptionsExecute(Sender: TObject);
begin
  fOptions.Showmodal;
end;
procedure TfMain.ApplicationProperties1ShowHint(var HintStr: string;
  var CanShow: Boolean; var HintInfo: THintInfo);
begin
  if fSearch.ShowHint(HintStr,CanShow,HintInfo) then exit;
end;
procedure TfMain.fEnterTimeMinimize(Sender: TObject);
begin
  if fEnterTime.acMinimizeToTray.Enabled then
    fMain.Hide
  else
    Application.Minimize;
end;
procedure TfMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  SendIPCMessage('RemoveMenue(Zeiterfassung)');
  if Assigned(fHelpContainer) then
    FreeAndNil(fHelpContainer);
  with Application as IBaseApplication do
    SaveConfig;
  with Application as IBaseDbInterface do
   if Assigned(Data) then
     begin
       if Data.Users.Rights.Right('Mustentertimes') > 0 then
         begin
           CloseAction := caNone;
           exit;
         end;
       fEnterTime.StopActualTime;
     end;
end;

procedure TfMain.DoCreate;
begin
  if Assigned(TBaseVisualApplication(Application).MessageHandler) then
    TBaseVisualApplication(Application).MessageHandler.RegisterCommandHandler(@CommandReceived);
  Application.CreateForm(TfEnterTime,fEnterTime);
  MenueRegistered:=False;
  fEnterTime.BorderStyle:=bsNone;
  fEnterTime.Parent := Self;
  fEnterTime.Align := alClient;
  fENterTime.Show;
  fEnterTime.Color:=Self.Color;
  fEnterTime.OnMinimize :=@fEnterTimeMinimize;
  with Application as IBaseApplication do
    begin
      AppVersion:={$I ../base/version.inc};
      AppRevision:={$I ../base/revision.inc};
      SetConfigName('Timeregistering');
    end;
  with Application as IBaseDbInterface do
    LoadMandants;
end;
procedure TfMain.SetLanguage;
begin
  with Application as IBaseApplication do
    if FileExistsUTF8(ProgramDirectory+'languages'+Directoryseparator+'lazreport.'+copy(Language,0,2)+'.po') then
      TranslateUnitResourceStrings('lr_const',ProgramDirectory+'languages'+Directoryseparator+'lazreport.'+copy(Language,0,2)+'.po');
  fError.SetLanguage;
  fEnterTime.SetLanguage;
end;
procedure TfMain.SetRights;
begin
  fEnterTime.SetRights;
end;
procedure TfMain.SetupDB;
begin
  fEnterTime.SetupDB;
end;
initialization
  {$I umain.lrs}

end.
