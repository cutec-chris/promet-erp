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
unit ubasehttpapplication;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, CustFCGI, uBaseApplication, uBaseDBInterface,
  uData, uSystemMessage, HTTPDefs,fpHTTP,custhttpapp,
  uBaseDbClasses,db,md5,uSessionDBClasses,eventlog;
type

  { TBaseHTTPApplication }

  TBaseHTTPApplication = class(TCustomHTTPApplication, IBaseApplication, IBaseDbInterface)
    procedure BaseHTTPApplicationException(Sender: TObject; E: Exception);
    procedure BaseHTTPApplicationGetModule(Sender: TObject; ARequest: TRequest;
      var ModuleClass: TCustomHTTPModuleClass);
  private
    FDBInterface: IBaseDbInterface;
    FDefaultModule: string;
    FMessageHandler: TMessageHandler;
    FLogger : TEventLog;
    FAppName : string;
    FAppRevsion : Integer;
    FAppVersion : Real;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Initialize; override;

    function GetOurConfigDir: string;
    function GetAppName: string;
    function GetApprevision: Integer;
    function GetAppVersion: real;
    procedure SetConfigName(aName : string);
    procedure RestoreConfig;
    procedure SaveConfig;
    function GetLanguage: string;
    procedure SetLanguage(const AValue: string);
    procedure SetAppname(AValue: string);virtual;
    procedure SetAppRevision(AValue: Integer);virtual;
    procedure SetAppVersion(AValue: real);virtual;
    function GetQuickHelp: Boolean;
    procedure SetQuickhelp(AValue: Boolean);
    function GetInternalTempDir: string;
    function GetMessageManager: TThread;

    function GetLog: TEventLog;
    procedure Log(aType : string;aMsg : string);virtual;
    procedure Log(aMsg : string);
    procedure Info(aMsg : string);
    procedure Warning(aMsg : string);
    procedure Error(aMsg : string);
    procedure Debug(aMsg : string);

    function ChangePasswort : Boolean;
    function GetSingleInstance : Boolean;
    function Login : Boolean;
    procedure Logout;
    procedure DoExit;
    property IData : IBaseDbInterface read FDBInterface implements IBaseDBInterface;
    property MessageHandler : TMessageHandler read FMessageHandler;
    property DefaultModule : string read FDefaultModule write FDefaultModule;
  end;
Var
  Application : TBaseHTTPApplication;
implementation
uses Utils,BlckSock, uUserAgents, ubasewebsession;
resourcestring
  strFailedtoLoadMandants    = 'Mandanten konnten nicht gelanden werden !';
  strLoginFailed             = 'Anmeldung fehlgeschlagen !';
procedure TBaseHTTPApplication.BaseHTTPApplicationException(Sender: TObject;
  E: Exception);
begin
  writeln('Error:'+e.Message);
  FLogger.Error(e.Message);
end;
procedure TBaseHTTPApplication.BaseHTTPApplicationGetModule(Sender: TObject;
  ARequest: TRequest; var ModuleClass: TCustomHTTPModuleClass);
var
  MN: String;
  MI: TModuleItem;
  S: String;
  I: Integer;
begin
  S:=ARequest.PathInfo;
  If (Length(S)>0) and (S[1]='/') then
    Delete(S,1,1);                      //Delete the leading '/' if exists
  I:=Length(S);
  If (I>0) and (S[I]='/') then
    Delete(S,I,1);                      //Delete the trailing '/' if exists
  I:=Pos('/',S);
  if (I>0) then
    MN:=ARequest.GetNextPathInfo;
  if S = '' then
    MI := ModuleFactory.FindModule(DefaultModule)
  else
    MI:=ModuleFactory.FindModule(MN);
  if (MI=Nil) then
    MI:=ModuleFactory.FindModule(S);
  if (MI=Nil) then
    MI:=ModuleFactory.FindModule('error');
  if Assigned(MI) then
    ModuleClass := MI.ModuleClass;
end;
constructor TBaseHTTPApplication.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAppName:='Promet-ERP';
  FAppVersion := 7.0;
  FAppRevsion := 0;
  BaseApplication := Self;
  FLogger := TEventLog.Create(Self);
  FLogger.Active:=false;
  if HasOption('l','logfile') then
    begin
      FLogger.FileName := GetOptionValue('l','logfile');
      FLogger.Active:=True;
    end;
  {.$Warnings Off}
  FDBInterface := TBaseDBInterface.Create;
  FDBInterface.SetOwner(Self);
  {.$Warnings On}
  AllowDefaultModule := True;
  Self.OnGetModule:=@BaseHTTPApplicationGetModule;
  Self.OnException:=@BaseHTTPApplicationException;
  Port := 8080;
  Threaded:=False;
end;
destructor TBaseHTTPApplication.Destroy;
begin
  DoExit;
  if Assigned(FmessageHandler) then
    begin
      FMessagehandler.Terminate;
      sleep(20);
    end;
  FDBInterface.Data.Free;
  FLogger.Free;
  inherited Destroy;
end;

procedure TBaseHTTPApplication.Initialize;
begin
  inherited Initialize;
  //StopOnException:=False;
end;

function TBaseHTTPApplication.GetOurConfigDir: string;
begin
  Result := GetConfigDir(StringReplace(lowercase(GetAppname),'-','',[rfReplaceAll]));
  if HasOption('c','config-path') then
    Result := GetOptionValue('c','config-path');
  result := AppendPathDelim(Result);
end;
function TBaseHTTPApplication.GetAppName: string;
begin
  Result := FAppName;
end;
function TBaseHTTPApplication.GetApprevision: Integer;
begin
  Result := FAppRevsion;
end;
function TBaseHTTPApplication.GetAppVersion: real;
begin
  Result := FAppVersion;
end;
procedure TBaseHTTPApplication.SetConfigName(aName: string);
var
  aDir: String;
begin
  aDir := GetOurConfigDir;
  aDir := Application.Location;
  if aDir <> '' then
    begin
      if not DirectoryExists(UniToSys(aDir)) then
        ForceDirectories(UniToSys(aDir));
    end;
end;
procedure TBaseHTTPApplication.RestoreConfig;
begin
end;

procedure TBaseHTTPApplication.SaveConfig;
begin
end;
function TBaseHTTPApplication.GetLanguage: string;
begin

end;
procedure TBaseHTTPApplication.SetLanguage(const AValue: string);
begin
end;
procedure TBaseHTTPApplication.SetAppname(AValue: string);
begin
  FAppName:=AValue;
end;
procedure TBaseHTTPApplication.SetAppRevision(AValue: Integer);
begin
  FAppRevsion:=AValue;
end;
procedure TBaseHTTPApplication.SetAppVersion(AValue: real);
begin
  FAppVersion:=AValue;
end;

function TBaseHTTPApplication.GetQuickHelp: Boolean;
begin

end;

procedure TBaseHTTPApplication.SetQuickhelp(AValue: Boolean);
begin

end;

function TBaseHTTPApplication.GetInternalTempDir: string;
begin
  Result := AppendPathDelim(GetTempPath);
end;

function TBaseHTTPApplication.GetMessageManager: TThread;
begin
  Result := nil;
end;

function TBaseHTTPApplication.GetLog: TEventLog;
begin
  Result := FLogger;
end;
procedure TBaseHTTPApplication.Log(aType: string; aMsg: string);
begin
  writeln(aType+':'+aMsg);
end;
procedure TBaseHTTPApplication.Log(aMsg: string);
begin
  Log('INFO',aMsg);
end;
procedure TBaseHTTPApplication.Info(aMsg: string);
begin
  Log(aMsg)
end;
procedure TBaseHTTPApplication.Warning(aMsg: string);
begin
  Log('WARNING',aMsg);
end;
procedure TBaseHTTPApplication.Error(aMsg: string);
begin
  Log('ERROR',aMsg);
end;
procedure TBaseHTTPApplication.Debug(aMsg: string);
begin
  if HasOption('debug') then
    //debugln('DEBUG:'+aMsg)
    ;
end;
function TBaseHTTPApplication.ChangePasswort: Boolean;
begin
  Result := False;
end;
function TBaseHTTPApplication.GetSingleInstance: Boolean;
begin
  Result := False;
end;
function TBaseHTTPApplication.Login: Boolean;
var
  aMandant: String;
begin
  Result := False;
  with Self as IBaseDbInterface do
    begin
      if not LoadMandants('') then
        raise Exception.Create(strFailedtoLoadMandants);
      aMandant := GetOptionValue('m','mandant');
      if aMandant = '' then
        aMandant := 'Default';
      if not DBLogin(aMandant,'') then
        begin
          FLogger.Error(strLoginFailed+':'+LastError);
          raise Exception.Create(strLoginFailed+':'+LastError);
        end;
      uData.Data := Data;
    end;
  SessionFactory.CleanupSessions;
  Result := True;
end;
procedure TBaseHTTPApplication.Logout;
begin
end;
procedure TBaseHTTPApplication.DoExit;
begin
  with Self as IBaseDbInterface do
    DBLogout;
end;
initialization
  Application := TBaseHTTPApplication.Create(nil);
  SessionFactoryClass:=TBaseSessionFactory;
finalization
  Application.Destroy;
end.

