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
program davserver;
{$mode objfpc}{$H+}
uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils, pcmdprometapp, CustApp, uBaseCustomApplication,
  uBaseDBInterface, md5, uData, pmimemessages, uBaseApplication,
  uBaseDatasetInterfaces, uDocuments, Utils,
  laz_synapse, udavserver,
  uhttputil, uprometdavserver;
type

  { TSVNServer }

  TSVNServer = class(TBaseCustomApplication)
    procedure ServerAccess(aSocket : TDAVSession;AMessage: string);
  private
    Server : TWebDAVServer;
    ServerFunctions: TPrometServerFunctions;
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;
procedure TSVNServer.ServerAccess(aSocket: TDAVSession; AMessage: string);
begin
  writeln(AMessage);
end;
procedure TSVNServer.DoRun;
var
  y,m,d,h,mm,s,ss: word;
  aTime: TDateTime;
begin
  with Self as IBaseDBInterface do
    DBLogout;
  aTime := Now();
  while not Terminated do
    begin
      CheckSynchronize(100);
      if (Now()-aTime) > (1/HoursPerDay) then
        begin
          break;
        end;
    end;
  // stop program loop
  Terminate;
end;
constructor TSVNServer.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
  if not Login then exit;
  Server := TWebDAVServer.Create;
  ServerFunctions := TPrometServerFunctions.Create;
  Server.Master.OnAccess:=@ServerAccess;
  Server.Master.OnGetDirectoryList:=@ServerFunctions.ServerGetDirectoryList;
  Server.Master.OnMkCol:=@ServerFunctions.ServerMkCol;
  Server.Master.OnDelete:=@ServerFunctions.ServerDelete;
  Server.Master.OnPutFile:=@ServerFunctions.ServerPutFile;
  Server.Master.OnGetFile:=@ServerFunctions.ServerGetFile;
  Server.Master.OnReadAllowed:=@ServerFunctions.ServerReadAllowed;
  Server.Master.OnWriteAllowed:=@ServerFunctions.ServerReadAllowed;
  Server.Master.OnUserLogin:=@ServerFunctions.ServerUserLogin;
  writeln('Login to Database OK');
  Server.Start;
  Data.Users.DataSet.Close;
end;
destructor TSVNServer.Destroy;
begin
  Server.Free;
  inherited Destroy;
end;
var
  Application: TSVNServer;
{$R *.res}
begin
  Application:=TSVNServer.Create(nil);
  Application.Run;
  Application.Free;
end.

