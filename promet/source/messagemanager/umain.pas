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
unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, types, FileUtil, ExtCtrls, Menus, Controls, ActnList,
  uProcessManagement,process, XMLConf,uSystemMessage,uBaseDbClasses,uBaseERPDBClasses,
  Graphics, LCLType, ImgList, db,umashineid,uBaseVisualApplication;

type

  { TfMain }

  TfMain = class(TDataModule)
    acHistory: TAction;
    acExit: TAction;
    acMarkRead: TAction;
    ActionList1: TActionList;
    ImageList1: TImageList;
    ImageList2: TImageList;
    ImageList3: TImageList;
    IPCTimer: TTimer;
    MenuItem1: TMenuItem;
    miExit: TMenuItem;
    miHistory: TMenuItem;
    pmTray: TPopupMenu;
    ProgTimer: TTimer;
    TrayIcon: TTrayIcon;
    procedure acExitExecute(Sender: TObject);
    procedure acHistoryExecute(Sender: TObject);
    procedure acMarkReadExecute(Sender: TObject);
    procedure aItemClick(Sender: TObject);
    procedure ApplicationEndSession(Sender: TObject);
    procedure DataModuleCreate(Sender: TObject);
    procedure IPCTimerTimer(Sender: TObject);
    function OpenLink(aLink: string; Sender: TObject): Boolean;
    procedure ProgTimerTimer(Sender: TObject);
    procedure TrayIconClick(Sender: TObject);
  private
    { private declarations }
    FUser: String;
    FMandant: String;
    aNow: TDateTime;
    aRefresh : Integer;
    FBaseRef: LargeInt;
    FFilter: string;
    FFilter2: string;
    InformRecTime : TDateTime;
    FHistory : TBaseHistory;
    fTimelineDataSet: TBaseHistory;
    function CommandReceived(Sender : TObject;aCommand : string) : Boolean;
    procedure SetBaseref(AValue: LargeInt);
    procedure SetFilter(AValue: string);
    procedure SetFilter2(AValue: string);
    procedure SwitchAnimationOff;
    procedure DoExit;
  public
    { public declarations }
    procedure RefreshFilter2;
    property History : TBaseHistory read FHistory;
    property Filter : string read FFilter write SetFilter;
    property Filter2 : string read FFilter2 write SetFilter2;
    property BaseRef : LargeInt read FBaseRef write SetBaseref;
  end;
var
  fMain: TfMain;
implementation
uses {$ifdef WINDOWS}Windows,{$endif}
  uData,Utils,Forms,uBaseApplication,uIntfStrConsts,math,eventlog,uBaseDBInterface,
  umTimeLine,XMLPropStorage,LCLProc,uprometipc,wikitohtml,uMessages,uBaseSearch,
  utask,uOrder,uPerson,uMasterdata,uProjects,uWiki,uDocuments,umeeting,uStatistic,
  uprometscripts,LCLIntf,uProcessManager,Dialogs,synautil;
{$R *.lfm}
const
  RefreshAll = 30;//5 mins refresh
resourcestring
  strNewEntrys        = 'Neue Einträge in Verlauf';
  strNewEntrysC       = 'Sie haben %d neue Einträge in Ihrem Verlauf';
  strHint             = 'Promet Nachrichtenzentrale';

function OnMessageReceived(aMessage: string): Boolean;
begin
  Result := fMain.CommandReceived(nil,aMessage);
end;
procedure TfMain.ProgTimerTimer(Sender: TObject);
var
  aProcess: String;
  Found: Boolean;
  tmp: String;
  i: Integer;
  bProcess: TProcProcess;
  cmd : string;
  sl: TStringList;
  a: Integer;
  Process: TProcProcess;
  aLog: TStringList;
  aRec: LargeInt;
  aTime: types.DWORD;
  s: TStringStream;
  bTime: String;
begin
  if not Assigned(Data) then exit;
  if not Data.Ping(Data.MainConnection) then exit;
  if Assigned(fmTimeline) and fmTimeline.Visible then exit;
  if Assigned(BaseApplication) then with BaseApplication as IBaseApplication do
    Debug('ProgTimer:Enter');
  aTime:=GetTickCount;
  ProgTimer.Enabled:=False;
  if acHistory.Enabled then
    begin
      //Show new History Entrys
      if (not FHistory.DataSet.Active) or (FHistory.DataSet.EOF) then //all shown, refresh list
        begin
          try
            with Application as IBaseDBInterface do
              InformRecTime := DecodeRfcDateTime(DBConfig.ReadString('INFORMRECTIME',''));
            if (InformRecTime=0) or (InformRecTime<Now()-5) then
             InformRecTime := Now()-5;
          except
            on e : Exception do
              begin
                InformRecTime:=Now()-5;
              end;
          end;
          Data.SetFilter(FHistory,'('+FFilter+' '+FFilter2+') AND ('+Data.QuoteField('TIMESTAMPD')+'>'+Data.DateTimeToFilter(InformRecTime)+')',0,'TIMESTAMPD');
          History.DataSet.Refresh;
          History.DataSet.First;
        end;
      if (not FHistory.EOF) then
        begin
          if Assigned(fmTimeline) and fmTimeline.Visible then
            begin
              if (fmTimeline.WindowState=wsMinimized) then
                begin
                  //debugln('Refresh visible Timeline');
                  fmTimeline.acRefresh.Execute;
                  InformRecTime := Now()+(1/(MSecsPerDay/MSecsPerSec));
                  {$IFDEF WINDOWS}
                  FlashWindow(fmTimeline.Handle,True);
                  {$ENDIF}
                end;
            end
          else
            begin
              TrayIcon.BalloonTitle:=strNewEntrys;
              tmp := '';
              while not FHistory.DataSet.EOF do
                begin
                  if (FHistory.FieldByName('CHANGEDBY').AsString <> Data.Users.IDCode.AsString)
                  and (FHistory.FieldByName('READ').AsString <> 'Y')
                  then
                    begin
                      tmp:=tmp+StripWikiText(FHistory.FieldByName('ACTION').AsString)+' - '+FHistory.FieldByName('REFERENCE').AsString+lineending;
                    end;
                  bTime := TimeToStr(FHistory.FieldByName('TIMESTAMPD').AsDateTime);
                  if FHistory.FieldByName('TIMESTAMPD').AsDateTime>InformRecTime then
                    InformRecTime:=FHistory.FieldByName('TIMESTAMPD').AsDateTime+(1/MSecsPerSec);
                  FHistory.DataSet.Next;
                end;
              if tmp <> '' then
                begin
                  TrayIcon.BalloonHint:=UniToSys(tmp);
                  TrayIcon.ShowBalloonHint;
                  TrayIcon.Icons := ImageList2;
                  TrayIcon.Animate:=True;
                  with Application as IBaseDBInterface do
                    DBConfig.WriteString('INFORMRECTIME',Rfc822DateTime(InformRecTime));
                  TrayIcon.Tag := 0;
                  if Assigned(fmTimeline) then
                    begin
                      //debugln('Refresh invisible Timeline');
                      fmTimeline.acRefresh.Execute;
                      fmTimeline.fTimeline.DataSet.First;
                      fmTimeline.fTimeline.GotoDataSetRow;
                    end;
                end;
            end;
        end;
    end;
  {$IFDEF LINUX}
  //Trayicon.visible := False;
  //TrayIcon.Visible:=True;
  {$ENDIF}
  ProgTimer.Enabled:=True;
  if Assigned(BaseApplication) then with BaseApplication as IBaseApplication do
    Debug('ProgTimer:Exit - '+IntToStr(GetTickCount-aTime));
end;
procedure TfMain.TrayIconClick(Sender: TObject);
begin
  if (not Assigned(fmTimeline)) or (not fmTimeline.Visible) then
    begin
      fmTimeline.Execute(fTimelineDataSet);
      SwitchAnimationOff;
    end
  else fmTimeline.Close;
end;
function TfMain.CommandReceived(Sender: TObject; aCommand: string
  ): Boolean;
var
  tmp: String;
  aMenue: TMenuItem;
  Found: Boolean = False;
  aItem: TMenuItem;
  tmp1: String;
  i: Integer;
  aMessage: String;
begin
  //debugln('CommdReceived:'+aCommand);
  Result := False;
  aMessage := aCommand;
  if copy(aMessage,0,9) = 'AddMenue(' then
    begin
      tmp := copy(aMessage,10,length(aMessage));
      tmp := copy(tmp,0,length(tmp)-1);
      aMenue := pmTray.Items;
      while length(tmp)>0 do
        begin
          tmp1 := copy(tmp,0,pos('/',tmp)-1);
          if pos('/',tmp) = 0 then
            begin
              tmp1 := tmp;
              tmp := ''
            end
          else  tmp := copy(tmp,pos('/',tmp)+1,length(tmp));
          Found := False;
          for i := 0 to aMenue.Count-1 do
            if aMenue.Items[i].Caption = tmp1 then
              begin
                Found := True;
                aMenue := aMenue.Items[i];
                break;
              end;
          if not Found then
            begin
              aItem := TMenuItem.Create(Self);
              aItem.Caption:=tmp1;
              aItem.OnClick:=@aItemClick;
              aMenue.Add(aItem);
              aMenue := aItem;
              acExit.Visible:=False;
            end;
        end;
      Result := True;
    end
  else if copy(aMessage,0,12) = 'RemoveMenue(' then
    begin
      tmp := copy(aMessage,13,length(aMessage));
      tmp := copy(tmp,0,length(tmp)-1);
      aMenue := pmTray.Items;
      while length(tmp)>0 do
        begin
          tmp1 := copy(tmp,0,pos('/',tmp)-1);
          if pos('/',tmp) = 0 then
            begin
              tmp1 := tmp;
              tmp := ''
            end
          else  tmp := copy(tmp,pos('/',tmp)+1,length(tmp));
          Found := False;
          for i := 0 to aMenue.Count-1 do
            if aMenue.Items[i].Caption = tmp1 then
              begin
                Found := True;
                aMenue := aMenue.Items[i];
                aMenue.Free;
                Result := True;
                if pmTray.Items.Count=2 then
                  acExit.Visible:=True;
                exit;
              end;
        end;
      Result := True;
    end;
end;
procedure TfMain.SetBaseref(AValue: LargeInt);
begin
  if FBaseRef=AValue then Exit;
  FBaseRef:=AValue;
end;
procedure TfMain.SetFilter(AValue: string);
begin
  if FFilter=AValue then Exit;
  FFilter:=AValue;
end;
procedure TfMain.SetFilter2(AValue: string);
begin
  if FFilter2=AValue then Exit;
  FFilter2:=AValue;
end;
procedure TfMain.SwitchAnimationOff;
begin
  if TrayIcon.Tag=0 then
    begin
      TrayIcon.Animate:=False;
      TrayIcon.Icons := ImageList3;
      TrayIcon.Animate:=True;
      TrayIcon.Tag := 1;
    end;
end;
procedure TfMain.DoExit;
begin
  try
    Data.ProcessClient.ShutDown;
    if Data.ProcessClient.DataSet.Locate('NAME',GetSystemName,[]) then
      begin
        Data.ProcessClient.DataSet.Edit;
        Data.ProcessClient.FieldByName('STATUS').AsString:='N';
        Data.ProcessClient.DataSet.Post;
      end;
  except
  end;
end;
procedure TfMain.RefreshFilter2;
var
  i: Integer;
  nLink: String;
begin
  Data.Users.Follows.ActualLimit:=0;
  Data.Users.Follows.Open;
  FFilter2:='';
  with Data.Users.Follows do
    begin
      FFilter2:=BuildFilter;
    end;
end;
procedure TfMain.DataModuleCreate(Sender: TObject);
var
  XMLConfig: TXMLPropStorage;
  aUsers: TUser;
  aUId : Variant;
  function FindSettings(bMandant,aConfig : string) : Boolean;
  var
    aID: LongInt;
  begin
    Result := False;
    with Application as IBaseApplication do
      if FileExists(GetOurConfigDir+aConfig+'.xml') then
        begin
          XMLConfig := TXMLPropStorage.Create(Self);
          XMLConfig.Filename:=GetOurConfigDir+aConfig+'.xml';
          XMLConfig.RootNodePath:='Config';
          XMLConfig.Restore;
          if copy(XMLConfig.ReadString('LOGINMANDANT',''),0,length(bMandant)) = bMandant then
            begin
              Result := True;
              aID := CreateUserID;
              if ((XMLConfig.ReadString('AUTOMATICLOGIN','') = IntToStr(aId)) and (aId <> 0)) then
                FUser := XMLConfig.ReadString('LOGINUSER','');
              FMandant := XMLConfig.ReadString('LOGINMANDANT','');
            end;
          XMLConfig.Free;
        end;
  end;
begin
  TrayIcon.AnimateInterval:=200;
  aRefresh:=0;
  Application.OnEndSession:=@ApplicationEndSession;
  with Application as IBaseApplication do
    begin
      with Application as IBaseApplication do
        begin
          SetConfigName('MessageManager');
          AppVersion:={$I ../base/version.inc};
          AppRevision:={$I ../base/revision.inc};
        end;
      with BaseApplication as IBaseDbInterface do
        begin
          Info('loading mandants...');
          if not LoadMandants then
            begin
              Error(strFailedtoLoadMandants);
              raise Exception.Create(strFailedtoLoadMandants);
              Application.Terminate;
            end;
          Info('search user...');
          FindSettings(FMandant,'PrometERP');
          if Application.GetOptionValue('m','mandant') <> '' then
            FMandant := Application.GetOptionValue('m','mandant');
          if Application.GetOptionValue('u','user') <> '' then
            FUser := Application.GetOptionValue('u','user');
          if FUser = '' then FindSettings(FMandant,'Timeregistering');
          if FUser = '' then FindSettings(FMandant,'Statistics');
          Info('User:'+FUser);
          Info('Mandant:'+FMandant);
          Info('login...');
          if FMandant = '' then
            begin
              //debugln(strMandantnotSelected);
              acHistory.Enabled:=False;
              exit;
            end;
          if not DBLogin(FMandant,FUser,False,False) then
            begin
              //debugln(strLoginFailed+' '+LastError);
              acHistory.Enabled:=False;
              FUser := '';
              FMandant := '';
              FindSettings(FMandant,'PrometERP');
              if FUser = '' then FindSettings(FMandant,'Timeregistering');
              if FUser = '' then FindSettings(FMandant,'Statistics');
              Info('User:'+FUser);
              Info('Mandant:'+FMandant);
              Info('relogin...');
              if not DBLogin(FMandant,FUser,False,False) then
                begin
                  //debugln(strLoginFailed+' '+LastError);
                  acHistory.Enabled:=False;
                  exit;
                end;
            end;
          uData.DataM := Data;
        end;
      Info('messagemanager login successful');
      TrayIcon.Hint:=strHint+LineEnding+FMandant+' '+FUser;

      Data.RegisterLinkHandler('ALLOBJECTS',@OpenLink,TObjects);
      //Messages
      Data.RegisterLinkHandler('HISTORY',@OpenLink,TBaseHistory);
      //Messages
      if Data.Users.Rights.Right('MESSAGES') > RIGHT_NONE then
        begin
          try
            Data.RegisterLinkHandler('MESSAGEIDX',@OpenLink,TMessage);
            Data.RegisterLinkHandler('MESSAGES',@OpenLink,TMessage);
            AddSearchAbleDataSet(TMessageList);
          except
          end;
        end;
      //Tasks
      if (Data.Users.Rights.Right('TASKS') > RIGHT_NONE) then
        begin
          try
          Data.RegisterLinkHandler('TASKS',@OpenLink,TTask,TTaskList);
          except
          end;
        end;
      //Add PIM Entrys
      if Data.Users.Rights.Right('CALENDAR') > RIGHT_NONE then
        begin
          try
            Data.RegisterLinkHandler('CALENDAR',@OpenLink,TTask,TTaskList);
          except
          end;
        end;
      //Orders
      if Data.Users.Rights.Right('ORDERS') > RIGHT_NONE then
        begin
          try
          Data.RegisterLinkHandler('ORDERS',@OpenLink,Torder);
          AddSearchAbleDataSet(TOrderList);
          except
          end;
        end;
      //Add Contacts
      if Data.Users.Rights.Right('CUSTOMERS') > RIGHT_NONE then
        begin
          try
          Data.RegisterLinkHandler('CUSTOMERS',@OpenLink,TPerson);
          AddSearchAbleDataSet(TPersonList);
          AddSearchAbleDataSet(TPersonContactData);
          AddSearchAbleDataSet(TPersonAddress);
          except
          end;
        end;
      //Add Masterdata stuff
      if (Data.Users.Rights.Right('MASTERDATA') > RIGHT_NONE) then
        begin
          try
          Data.RegisterLinkHandler('MASTERDATA',@OpenLink,TMasterdata);
          AddSearchAbleDataSet(TMasterdataList);
          except
          end;
        end;
      //Projects
      if (Data.Users.Rights.Right('PROJECTS') > RIGHT_NONE) then
        begin
          try
          Data.RegisterLinkHandler('PROJECT',@OpenLink,TProject);
          AddSearchAbleDataSet(TProjectList);
          except
          end;
        end;
      //Wiki
      Data.RegisterLinkHandler('WIKI',@OpenLink,TWikiList);
      if (Data.Users.Rights.Right('WIKI') > RIGHT_NONE) then
        begin
          try
          AddSearchAbleDataSet(TWikiList);
          except
          end;
        end;
      //Documents
      if (Data.Users.Rights.Right('DOCUMENTS') > RIGHT_NONE) then
        begin
          try
          Data.RegisterLinkHandler('DOCUMENTS',@OpenLink,TDocument);
          //Data.RegisterLinkHandler('DOCPAGES',@OpenLink,TDocPages);
          except
          end;
        end;
      //Lists
      if (Data.Users.Rights.Right('LISTS') > RIGHT_NONE) then
        begin
          try
          Data.RegisterLinkHandler('LISTS',@OpenLink,TLists);
          AddSearchAbleDataSet(TLists);
          except
          end;
        end;
      //Meetings
      if (Data.Users.Rights.Right('MEETINGS') > RIGHT_NONE) then
        begin
          try
          Data.RegisterLinkHandler('MEETINGS',@OpenLink,TMeetings);
          AddSearchAbleDataSet(TMeetings);
          except
          end;
        end;
      //Inventory
      if (Data.Users.Rights.Right('INVENTORY') > RIGHT_NONE) then
        begin
          try
          Data.RegisterLinkHandler('INVENTORY',@OpenLink,TInventorys);
          except
          end;
        end;
      //Statistics
      if (Data.Users.Rights.Right('STATISTICS') > RIGHT_NONE) then
        begin
          try
          Data.RegisterLinkHandler('STATISTICS',@OpenLink,TStatistic);
          AddSearchAbleDataSet(TStatistic);
          except
          end;
        end;
      //Timeregistering
      AddSearchAbleDataSet(TUser);
      Data.RegisterLinkHandler('USERS',@OpenLink,TUser);
      //History
      if Data.Users.Rights.Right('DOCUMENTS') > RIGHT_NONE then
        begin
          try
          AddSearchAbleDataSet(TBaseHistory);
          Data.RegisterLinkHandler('HISTORY',@OpenLink,TBaseHistory);
          except
          end;
        end;
    end;
  acHistory.Enabled:=FUser <> '';
  FHistory := TBaseHistory.CreateEx(Self,Data);
  FHistory.CreateTable;
  if FUser <> '' then
    begin
      if Data.Users.IDCode.AsString<>'' then
        FFilter := '('+Data.QuoteField('REFERENCE')+'='+Data.QuoteValue(Data.Users.IDCode.AsString)+')'
      else
        FFilter := '('+Data.QuoteField('REFERENCE')+'='+Data.QuoteValue('BLDS')+')';
      aUsers := TUser.Create(nil);
      aUsers.Select(Data.Users.Id.AsString);
      aUsers.Open;
      while aUsers.Count>0 do
        begin
          FFilter := FFilter+' OR ('+Data.QuoteField('REF_ID')+'='+Data.QuoteValue(aUsers.Id.AsString)+')';
          aUId := aUsers.FieldByName('PARENT').AsVariant;
          aUsers.Select(aUId);
          aUsers.Open;
        end;
      aUsers.Free;
      RefreshFilter2;
    end;
  ProgTimer.Enabled:=True;
  Data.ProcessClient.Startup;
  uprometipc.OnMessageReceived:=@OnMessageReceived;
  fTimelineDataSet := TBaseHistory.Create(nil);
  fTimelineDataSet.CreateTable;
  Data.SetFilter(fTimelineDataSet,trim(fMain.Filter+' '+fMain.Filter2),200);
  ProgTimerTimer(nil);
  SwitchAnimationOff;
end;

procedure TfMain.acHistoryExecute(Sender: TObject);
begin
  fmTimeline.Execute(fTimelineDataSet);
  SwitchAnimationOff;
end;

procedure TfMain.acMarkReadExecute(Sender: TObject);
begin
  SwitchAnimationOff;
end;

procedure TfMain.acExitExecute(Sender: TObject);
begin
  DoExit;
  FreeAndNil(FHistory);
  Application.Terminate;
end;

procedure TfMain.aItemClick(Sender: TObject);
var
  aItem: TMenuItem;
  tmp: string;
  i: Integer;
  sl: TStringList;
begin
  aItem := TMenuItem(Sender);
  tmp := aItem.Caption;
  while Assigned(aItem.Parent) do
    begin
      aItem := aItem.Parent;
      tmp := aItem.Caption+'/'+tmp;
    end;

  SendIPCMessage('OnClick('+tmp+')');
end;

procedure TfMain.ApplicationEndSession(Sender: TObject);
begin
  try
    if Data.ProcessClient.DataSet.Locate('NAME',GetSystemName,[]) then
      begin
        Data.ProcessClient.DataSet.Edit;
        Data.ProcessClient.DataSet.FieldByName('STATUS').AsString:='N';
        Data.ProcessClient.DataSet.Post;
      end;
  except
  end;
  DoExit;
end;

procedure TfMain.IPCTimerTimer(Sender: TObject);
begin
  PeekIPCMessages;
end;
function TfMain.OpenLink(aLink: string; Sender: TObject): Boolean;
begin
end;
end.

