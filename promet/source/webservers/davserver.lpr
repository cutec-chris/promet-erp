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
    procedure ServerAccess(aSocket : TDAVSocket;AMessage: string);
  private
    Server : TWebDAVServer;
    ServerFunctions: TPrometServerFunctions;
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;
procedure TSVNServer.ServerAccess(aSocket: TDAVSocket; AMessage: string);
begin
  writeln(AMessage);
end;
procedure TSVNServer.DoRun;
var
  y,m,d,h,mm,s,ss: word;
begin
  with Self as IBaseDBInterface do
    DBLogout;
  while not Terminated do
    begin
      CheckSynchronize(100);
    end;
  // stop program loop
  Terminate;
end;
constructor TSVNServer.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
  Server := TWebDAVServer.Create;
  ServerFunctions := TPrometServerFunctions.Create;
  Server.OnAccess:=@ServerAccess;
  Server.OnGetDirectoryList:=@ServerFunctions.ServerGetDirectoryList;
  Server.OnMkCol:=@ServerFunctions.ServerMkCol;
  Server.OnDelete:=@ServerFunctions.ServerDelete;
  Server.OnPutFile:=@ServerFunctions.ServerPutFile;
  Server.OnGetFile:=@ServerFunctions.ServerGetFile;
  Server.OnReadAllowed:=@ServerFunctions.ServerReadAllowed;
  Server.OnWriteAllowed:=@ServerFunctions.ServerReadAllowed;
  Server.OnUserLogin:=@ServerFunctions.ServerUserLogin;
  if not Login then exit;
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

