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
Created 16.06.2015
*******************************************************************************}
 program message_xmpp;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils, CustApp
  { you can add units after this },db, Utils, general_nogui, uData,
  uIntfStrConsts, pcmdprometapp, uBaseCustomApplication, uBaseApplication,
  uxmpp, synautil, uPerson, uBaseDbClasses, uspeakinginterface, wikitohtml,
  uBaseDBInterface, uprometcscript, uprometpascalscript, uprometpythonscript;

type
  { PrometXMPPMessanger }
  PrometXMPPMessanger = class(TBaseCustomApplication)
    procedure SpeakerWriteln(const s: string);
    procedure xmppDebugXML(Sender: TObject; Value: string);
    procedure xmppError(Sender: TObject; ErrMsg: string);
    procedure xmppFileAccept(Sender: TObject; JID, Session, Filename: string;
      FileSize: Integer; var Accept: Boolean);
    procedure xmppIqVcard(Sender: TObject; from_, to_, fn_, photo_type_,
      photo_bin_: string);
    procedure xmppLogin(Sender: TObject);
    procedure xmppLogout(Sender: TObject);
    procedure xmppMessage(Sender: TObject; From: string; MsgText: string;
      MsgHTML: string; TimeStamp: TDateTime; MsgType: TMessageType);
    procedure xmppPresence(Sender: TObject; Presence_Type, JID, Resource,
      Status, Photo: string);
    procedure xmppRoster(Sender: TObject; JID, aName, Subscription, Group: string
      );
  private
    FActive : Boolean;
    FBaseRef: LargeInt;
    FFilter: string;
    FFilter2: string;
    FHistory: TBaseHistory;
    FUsers : TStringList;
    Speaker: TSpeakingInterface;
    xmpp: TXmpp;
    FJID : string;
    InformRecTime : TDateTime;
    FMessages : TStringList;
    procedure SetBaseref(AValue: LargeInt);
    procedure SetFilter(AValue: string);
    procedure SetFilter2(AValue: string);
  protected
    procedure DoRun; override;
    function CheckUser(JID : string) : Boolean;
    procedure RefreshFilter2;
    property History : TBaseHistory read FHistory;
    property Filter : string read FFilter write SetFilter;
    property Filter2 : string read FFilter2 write SetFilter2;
    property BaseRef : LargeInt read FBaseRef write SetBaseref;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

procedure PrometXMPPMessanger.SpeakerWriteln(const s: string);
begin
  if FJID<>'' then
    begin
      xmpp.SendPersonalMessage(FJID,s);
      writeln('answ:'+s);
    end;
end;
procedure PrometXMPPMessanger.xmppDebugXML(Sender: TObject; Value: string);
begin
  writeln('Debug:'+Value);
end;
procedure PrometXMPPMessanger.xmppError(Sender: TObject; ErrMsg: string);
begin
  FActive := False;
  writeln('error:'+ErrMsg);
end;

procedure PrometXMPPMessanger.xmppFileAccept(Sender: TObject; JID, Session,
  Filename: string; FileSize: Integer; var Accept: Boolean);
begin
  Accept := True;
end;

procedure PrometXMPPMessanger.xmppIqVcard(Sender: TObject; from_, to_, fn_,
  photo_type_, photo_bin_: string);
begin
  writeln('vcard:'+from_);
end;
procedure PrometXMPPMessanger.xmppLogin(Sender: TObject);
begin
  FActive := True;
  writeln('login ok');
end;
procedure PrometXMPPMessanger.xmppLogout(Sender: TObject);
begin
  FActive:=False;
  writeln('logout');
end;
procedure PrometXMPPMessanger.xmppMessage(Sender: TObject; From: string;
  MsgText: string; MsgHTML: string; TimeStamp: TDateTime; MsgType: TMessageType
  );
var
  ID: String;
begin
  writeln('msg:'+From+':'+MsgText);
  if CheckUser(From) then
    begin
      FJID:=From;
      try
        if not Speaker.CheckSentence(MsgText) then
          xmpp.SendPersonalMessage(From,strSentenceNotValid);
      except
      end;
      FJID:='';
    end
  else writeln('user unknown !')
end;
procedure PrometXMPPMessanger.xmppPresence(Sender: TObject; Presence_Type, JID,
  Resource, Status, Photo: string);
var
  UserIdx: Integer;
begin
  writeln('presence:'+Presence_Type+','+JID+','+Resource+','+Status+',',Photo);
  if CheckUser(JID) then
    begin
      if Presence_Type='subscribe' then
        begin
          xmpp.SendPresence(JID,'subscribed',xmpp.JabberID,xmpp.Resource,'');
        end
      else if Presence_Type='unavailable' then
        begin
          UserIdx :=FUsers.IndexOfName(JID);
          if UserIdx>-1 then
            FUsers.Delete(UserIdx);
        end;
    end
  else writeln('user unknown !')
end;

procedure PrometXMPPMessanger.xmppRoster(Sender: TObject; JID, aName,
  Subscription, Group: string);
begin
  writeln('roaster:'+JID+','+aName+','+Subscription+',',Group);
  if CheckUser(JID) then
    begin
      xmpp.SendPresence(JID,'subscribe',xmpp.JabberID,xmpp.Resource,'');
    end
  else writeln('user unknown !')
end;

procedure PrometXMPPMessanger.SetBaseref(AValue: LargeInt);
begin
  if FBaseRef=AValue then Exit;
  FBaseRef:=AValue;
end;

procedure PrometXMPPMessanger.SetFilter(AValue: string);
begin
  if FFilter=AValue then Exit;
  FFilter:=AValue;
end;

procedure PrometXMPPMessanger.SetFilter2(AValue: string);
begin
  if FFilter2=AValue then Exit;
  FFilter2:=AValue;
end;

procedure PrometXMPPMessanger.DoRun;
var
  tmp : string;
  i: Integer;
  aUsers: TUser;
  aUID : Variant;
  a: Integer;
  StartTime: TDateTime;
begin
  FActive:=True;
  with BaseApplication as IBaseApplication do
    begin
      AppVersion:={$I ../base/version.inc};
      AppRevision:={$I ../base/revision.inc};
    end;
  if not Login then Terminate;
  //Your logged in here on promet DB
  FHistory := TBaseHistory.Create(nil);
  Speaker := TSpeakingInterface.Create(nil);
  Speaker.Writeln:=@SpeakerWriteln;
  xmpp := TXmpp.Create;
  xmpp.Host := synautil.SeparateRight(GetOptionValue('jid'), '@');
  xmpp.JabberID := GetOptionValue('jid');
  xmpp.Password := GetOptionValue('pw');
  xmpp.Port:='5222';
  xmpp.OnLogin:=@xmppLogin;
  xmpp.OnError:=@xmppError;
  xmpp.OnPresence:=@xmppPresence;
  xmpp.OnMessage:=@xmppMessage;
  xmpp.OnLogout:=@xmppLogout;
  xmpp.OnRoster:=@xmppRoster;
  xmpp.OnFileAccept:=@xmppFileAccept;
  if HasOption('server-log') then
    xmpp.OnDebugXML:=@xmppDebugXML;
  xmpp.OnIqVcard:=@xmppIqVcard;
  writeln('logging in ...');
  xmpp.Login;
  StartTime := Now();
  while FActive and not Terminated do
    begin
      sleep(10000);
      for i := 0 to FUsers.Count-1 do
        begin
          if Data.Users.Locate('SQL_ID',FUsers.ValueFromIndex[i],[]) then
            begin
              try
                with BaseApplication as IBaseDBInterface do
                  InformRecTime := DecodeRfcDateTime(DBConfig.ReadString('INFORMRECTIME',''));
                if (InformRecTime=0) or (InformRecTime<Now()-5) then
                 InformRecTime := Now()-5;
              except
                on e : Exception do
                  begin
                    InformRecTime:=Now()-5;
                  end;
              end;
              try
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
                //Show new History Entrys
                if (not FHistory.DataSet.Active) or (FHistory.DataSet.EOF) then //all shown, refresh list
                  begin
                    Data.SetFilter(FHistory,'('+FFilter+' '+FFilter2+') AND ('+Data.QuoteField('TIMESTAMPD')+'>'+Data.DateTimeToFilter(InformRecTime)+')',0,'TIMESTAMPD');
                    History.DataSet.Refresh;
                    History.DataSet.First;
                  end;
                while (not FHistory.EOF) do
                  begin
                    try
                      if (FHistory.FieldByName('READ').AsString <> 'Y')
                      then
                        begin
                          tmp:=FHistory.FieldByName('DATE').AsString+' '+StripWikiText(FHistory.FieldByName('ACTION').AsString)+' - '+FHistory.FieldByName('REFERENCE').AsString+lineending;
                          xmpp.SendPersonalMessage(FUsers.Names[i],tmp);
                        end;
                      if FHistory.FieldByName('TIMESTAMPD').AsDateTime>InformRecTime then
                        InformRecTime:=FHistory.FieldByName('TIMESTAMPD').AsDateTime+(1/MSecsPerSec);
                      if tmp<>'' then
                        with BaseApplication as IBaseDBInterface do
                          DBConfig.WriteString('INFORMRECTIME',Rfc822DateTime(InformRecTime));
                    finally
                      FHistory.DataSet.Next;
                    end;
                  end;
                a := 0;
              except
              end;
            end;
          if Now()-StartTime > ((1/MinsPerDay)*20) then
            begin
              FActive:=False;
              break;
            end;
        end;
    end;
  writeln('exitting ...');
  //xmpp.Free;
  Speaker.Free;
  FHistory.Free;
  // stop program loop
  Terminate;
end;
function PrometXMPPMessanger.CheckUser(JID: string): Boolean;
var
  aCont: TPersonContactData;
  aUser: TUser;
begin
  if pos('/',JID)>0 then
    JID := copy(JID,0,pos('/',JID)-1);
  Result := FUsers.Values[JID]<>'False';
  if FUsers.Values[JID]='' then
    begin
      FUsers.Values[JID]:='False';
      aCont := TPersonContactData.Create(nil);
      aCont.Filter(Data.QuoteField('DATA')+'='+Data.QuoteValue(JID));
      if aCont.Count>0 then
        begin
          aUser := TUser.Create(nil);
          aUser.Filter(Data.QuoteField('CUSTOMERNO')+'='+Data.QuoteValue(aCont.FieldByName('ACCOUNTNO').AsString));
          if aUser.Count>0 then
            begin
              FUsers.Values[JID]:=aUser.Id.AsString;
              Result := True;
            end;
          aUser.Free;
        end;
      aCont.Free;
    end;
end;

procedure PrometXMPPMessanger.RefreshFilter2;
begin
  Data.Users.Follows.ActualLimit:=0;
  Data.Users.Follows.Open;
  FFilter2:=Data.Users.Follows.BuildFilter;
end;

constructor PrometXMPPMessanger.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
  FUsers := TStringList.Create;
  FMessages := TStringList.Create;
end;
destructor PrometXMPPMessanger.Destroy;
begin
  FUsers.Free;
  FMessages.Free;
  inherited Destroy;
end;
var
  Application: PrometXMPPMessanger;
begin
  Application:=PrometXMPPMessanger.Create(nil);
  Application.Run;
  Application.Free;
end.

