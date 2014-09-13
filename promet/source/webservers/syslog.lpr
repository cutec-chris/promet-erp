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
Created 07.09.2014
*******************************************************************************}
 program syslog;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp
  { you can add units after this },db, Utils, FileUtil, uData, uIntfStrConsts,
  pcmdprometapp, uBaseCustomApplication, laz_synapse, uBaseApplication,slogsend,
  blcksock,uBaseDbClasses;

type

  { PrometCmdApp }

  PrometCmdApp = class(TBaseCustomApplication)
  private
    Server: TBlockSocket;
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

{ PrometCmdApp }

procedure PrometCmdApp.DoRun;
var
  Buffer: String;
  aMsg: TSyslogMessage;
  aObject: TObjects;
  aIP: String;
begin
  with BaseApplication as IBaseApplication do
    begin
      AppVersion:={$I ../base/version.inc};
      AppRevision:={$I ../base/revision.inc};
    end;
  if not Login then Terminate;
  //Your logged in here on promet DB
  aObject := TObjects.Create(nil,Data);
  Server.Bind(cAnyHost,'1514');
  if Server.LastError = 0 then
    begin
      while not Terminated do
        begin
          Buffer := Server.RecvPacket(1000);
          if Server.LastError = 0 then
            begin
              // just send the same packet back
              writeln(Buffer);
              aMsg := TSyslogMessage.Create;
              aMsg.PacketBuf:=Buffer;
              aIP := aMsg.LocalIP;
              if aIP='' then
                aIP := Server.GetRemoteSinIP;
              aObject.SelectFromNumber(aIP);
              aObject.Open;
              if aObject.Count=0 then
                begin
                  aObject.Insert;
                  aObject.Number.AsString:=aIP;
                  aObject.Text.AsString:=aIP;
                end;
              aObject.History.AddItem(aObject.DataSet,aMsg.LogMessage,'',aMsg.Tag,nil,0,'',True,False,False);
              aObject.History.FieldByName('DATE').AsDateTime:=aMsg.DateTime;
              aObject.History.FieldByName('SOURCE').AsString:='SysLog';
              aObject.History.Post;
              aMsg.Free;
            end;

          // minimal sleep
          if Buffer = '' then
            Sleep(10);
        end;
    end;
  // stop program loop
  Terminate;
end;

constructor PrometCmdApp.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
  Server:=TUDPBlockSocket.Create;
end;

destructor PrometCmdApp.Destroy;
begin
  Server.Free;
  inherited Destroy;
end;

var
  Application: PrometCmdApp;

begin
  Application:=PrometCmdApp.Create(nil);
  Application.Run;
  Application.Free;
end.

