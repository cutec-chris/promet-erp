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
//TODO:Error on 7 UID fetch 92440905:92479961,92492667,92565735:92565739,92565747,92565755:92565767,92565775:92565787,92565795,92565811,92565827,92565839 (UID RFC822.SIZE FLAGS BODY.PEEK[HEADER.FIELDS (From To Cc Bcc Subject Date Message-ID Priority X-Priority References Newsgroups In-Reply-To Content-Type Reply-To)])
program imapserver;
{$mode objfpc}{$H+}
uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, pcmdprometapp, CustApp, uBaseCustomApplication, lnetbase,
  lNet, laz_synapse, ulimap, uBaseDBInterface, md5,uData,eventlog,uprometimap,
  pmimemessages, fileutil,lconvencoding,uBaseApplication;
type
  TPIMAPServer = class(TBaseCustomApplication)
    procedure ServerLog(aSocket: TLIMAPSocket; DirectionIn: Boolean;
      aMessage: string);
    function ServerLogin(aSocket: TLIMAPSocket; aUser, aPasswort: string
      ): Boolean;
  private
    Server: TLIMAPServer;
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

procedure TPIMAPServer.ServerLog(aSocket: TLIMAPSocket; DirectionIn: Boolean;
  aMessage: string);
begin
  with Self as IBaseApplication do
    begin
      if DirectionIn then
        begin
          Info(IntToStr(aSocket.Id)+':>'+aMessage);
//          writeln(IntToStr(aSocket.Id)+':>'+aMessage);
        end
      else
        begin
          Info(IntToStr(aSocket.Id)+':<'+aMessage);
//          writeln(IntToStr(aSocket.Id)+':<'+aMessage);
        end;
    end;
end;

function TPIMAPServer.ServerLogin(aSocket: TLIMAPSocket; aUser,
  aPasswort: string): Boolean;
begin
  Result := False;
  with Self as IBaseDBInterface do
    begin
      if Data.Users.DataSet.Locate('LOGINNAME',aUser,[]) then
        begin
          if (Data.Users.CheckPasswort(aPasswort)) then
            Result := True;
        end;
    end;
  with Self as IBaseApplication do
    begin
      if Result then
        Log('Login:'+aUser)
      else
        Error('Login failed:'+aUser);
    end;
end;
procedure TPIMAPServer.DoRun;
var
  y,m,d,h,mm,s,ss: word;
  aGroup: TIMAPFolder;
begin
  with Self as IBaseDBInterface do
    begin
      DBLogout;
      if not Login then exit;
    end;
  with Self as IBaseApplication do
    begin
      DecodeDate(Now(),y,m,d);
      DecodeTime(Now(),h,mm,s,ss);
      GetLog.Active := False;
      GetLog.FileName := Format('nntp_server_log_%.4d-%.2d-%.2d %.2d_%.2d_%.2d_%.4d.log',[y,m,d,h,mm,s,ss]);
      getLog.LogType := ltFile;
      GetLog.Active:=True;
    end;
  Data.SetFilter(Data.Tree,Data.QuoteField('TYPE')+'='+Data.QuoteValue('B')+' or '+Data.QuoteField('TYPE')+'='+Data.QuoteValue('N'),0,'','ASC',False,True,True);
  with Data.Tree.DataSet do
    begin
      First;
      while not EOF do
        begin
          if Data.Tree.Id.AsVariant = TREE_ID_MESSAGES then
            aGroup := TPIMAPFolder.Create('INBOX',Data.Tree.Id.AsString)
          else  if Data.Tree.Id.AsVariant = TREE_ID_DELETED_MESSAGES then
            aGroup := TPIMAPFolder.Create('Trash',Data.Tree.Id.AsString)
          else  if Data.Tree.Id.AsVariant = TREE_ID_SEND_MESSAGES then
            aGroup := TPIMAPFolder.Create('Send',Data.Tree.Id.AsString)
          else
            aGroup := TPIMAPFolder.Create(FieldByName('NAME').AsString,Data.Tree.Id.AsString);
          Server.Folders.Add(aGroup);
          next;
        end;
    end;
  Server.OnLogin :=@ServerLogin;
  Server.OnLog:=@ServerLog;
  while not Terminated do
    begin
      Server.CallAction;
      sleep(100);
    end;
  // stop program loop
  Terminate;
end;

constructor TPIMAPServer.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=False;
  Server := TLIMAPServer.Create(Self);
  Server.Port := 143;
  if HasOption('port') then
    Server.Port := StrToInt(GetOptionValue('port'));
  Server.Start;
end;

destructor TPIMAPServer.Destroy;
begin
  Server.Free;
  inherited Destroy;
end;

var
  Application: TPIMAPServer;

{$R *.res}

begin
  Application:=TPIMAPServer.Create(nil);
  Application.Run;
  Application.Free;
end.
