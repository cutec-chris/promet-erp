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
program nntpserver;
{$mode objfpc}{$H+}
uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, types, pcmdprometapp, CustApp, uBaseCustomApplication,
  lNet, uBaseDBInterface, md5, uData, eventlog, pmimemessages,
  uBaseApplication;
type
  TPNNTPServer = class(TBaseCustomApplication)
    procedure ServerLog(aSocket: TLNNTPSocket; DirectionIn: Boolean;
      aMessage: string);
    function ServerLogin(aSocket: TLNNTPSocket; aUser, aPasswort: string
      ): Boolean;
  private
    Server: TLNNTPServer;
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

procedure TPNNTPServer.ServerLog(aSocket: TLNNTPSocket; DirectionIn: Boolean;
  aMessage: string);
begin
  with Self as IBaseApplication do
    begin
      if DirectionIn then
        begin
          Info(IntToStr(aSocket.Id)+':>'+aMessage);
        end
      else
        begin
          Info(IntToStr(aSocket.Id)+':<'+aMessage);
        end;
    end;
end;

function TPNNTPServer.ServerLogin(aSocket: TLNNTPSocket; aUser,
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
procedure TPNNTPServer.DoRun;
var
  aGroup: TPNNTPGroup;
  y,m,d,h,mm,s,ss: word;
  aTime: TDateTime;
begin
  writeln('starting...');
  with Self as IBaseDBInterface do
    begin
      DBLogout;
      if not Login then
        begin
          writeln('login failed');
          exit;
        end;
    end;
  writeln('login ok');
  with Self as IBaseApplication do
    begin
      DecodeDate(Now(),y,m,d);
      DecodeTime(Now(),h,mm,s,ss);
      GetLog.Active := False;
      GetLog.FileName := Format('nntp_server_log_%.4d-%.2d-%.2d %.2d_%.2d_%.2d_%.4d.log',[y,m,d,h,mm,s,ss]);
      getLog.LogType := ltFile;
      GetLog.Active:=True;
    end;
  Data.SetFilter(Data.Tree,Data.QuoteField('TYPE')+'='+Data.QuoteValue('B'));
  with Data.Tree.DataSet do
    begin
      First;
      while not EOF do
        begin
          aGroup := TPNNTPGroup.Create(FieldByName('NAME').AsString,0);
          Server.Groups.Add(aGroup);
          next;
        end;
    end;
  Server.OnLogin :=@ServerLogin;
  Server.OnLog:=@ServerLog;
  writeln('server running...');
  aTime := Now();
  while not Terminated do
    begin
      Server.CallAction;
      sleep(10);
      if (Now()-aTime)>(1) then break;//stop once a day
    end;
  // stop program loop
  Terminate;
end;

constructor TPNNTPServer.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
  Server := TLNNTPServer.Create(Self);
end;

destructor TPNNTPServer.Destroy;
begin
  Server.Free;
  inherited Destroy;
end;

var
  Application: TPNNTPServer;

{$R *.res}

begin
  Application:=TPNNTPServer.Create(nil);
  Application.Run;
  Application.Free;
end.

