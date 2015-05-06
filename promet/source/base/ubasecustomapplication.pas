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
unit uBaseCustomApplication;
{$mode objfpc}{$H+}
interface
uses
  {$ifdef UNIX}cwstring,{$endif}Classes, SysUtils, CustApp, uBaseApplication, uBaseDBInterface,
  uData, uSystemMessage, eventlog,Utils;
resourcestring
  strFailedtoLoadMandants    = 'Mandanten konnten nicht gelanden werden !';
  strMandantnotSelected      = 'kein Mandant gewählt (--mandant) !';
  strLoginFailed             = 'Login fehlgeschlagen';
  strStackTrace                 = 'Stackverfolgung:';
  strError                      = 'Error';
  strExceptionclass             = 'Errorclass: ';
  strOriginalException          = 'Original exception: ';
  strExceptObject               = 'Errorobject: ';
  strExceptObjectclass          = 'Errorobjectclass: ';
{$ifdef CPU32}
  strExceptPointer              = 'Error adress: %.4x';
{$endif}
{$ifdef CPU64}
  strExceptPointer              = 'Error adress: %.8x';
{$endif}

type

  { TBaseCustomApplication }

  TBaseCustomApplication = class(TCustomApplication, IBaseApplication, IBaseDbInterface)
    procedure BaseCustomApplicationException(Sender: TObject; E: Exception);
  private
    FDBInterface: IBaseDbInterface;
    FMessageHandler: TMessageHandler;
    FLogger : TEventLog;
    DoDestroy : Boolean;
    FAppName : string;
    FAppRevsion : Integer;
    FAppVersion : Real;
    function HandleSystemCommand(Sender : TObject;aCommand : string) : Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function GetOurConfigDir : string;
    procedure SetConfigName(aName : string);
    procedure RestoreConfig;
    procedure SaveConfig;
    function GetLanguage: string;
    procedure SetLanguage(const AValue: string);
    function GetSingleInstance : Boolean; virtual;
    function GetInternalTempDir: string;

    function GetAppName: string;virtual;
    function GetApprevision: Integer;virtual;
    function GetAppVersion: real;virtual;
    procedure SetAppname(AValue: string);virtual;
    procedure SetAppRevision(AValue: Integer);virtual;
    procedure SetAppVersion(AValue: real);virtual;
    function GetQuickHelp: Boolean;
    procedure SetQuickhelp(AValue: Boolean);
    function GetMessageManager : TThread;

    procedure Log(aType : string;aMsg : string);virtual;
    procedure Log(aMsg : string);
    procedure Info(aMsg : string);
    procedure Warning(aMsg : string);
    procedure Error(aMsg : string);
    procedure Debug(aMsg : string);
    function GetLog : TEventLog;

    function Login : Boolean;
    function ChangePasswort : Boolean;
    procedure RegisterMessageHandler;
    procedure Logout;
    procedure DoExit;
    property IData : IBaseDbInterface read FDBInterface implements IBaseDBInterface;
    property MessageHandler : TMessageHandler read FMessageHandler;
    property ShouldExit : Boolean read DoDestroy;
  end;

implementation
uses variants;
procedure TBaseCustomApplication.BaseCustomApplicationException(
  Sender: TObject; E: Exception);
var
  aMsg : string = '';
  FrameCount: LongInt;
  Frames: PPointer;
  FrameNumber: Integer;
begin
  try
    if ExceptAddr <> nil then
      begin
        aMsg := aMsg+lineending+strStackTrace;
        aMsg := aMsg+lineending+BackTraceStrFunc(ExceptAddr);
        FrameCount:=ExceptFrameCount;
        Frames:=ExceptFrames;
        for FrameNumber := 0 to FrameCount-1 do
          aMsg := aMsg+lineending+BackTraceStrFunc(Frames[FrameNumber]);
      end;
  except
  end;
  FLogger.Error(e.Message+aMsg);
  Terminate;
  halt(1);
end;
function TBaseCustomApplication.HandleSystemCommand(Sender: TObject;
  aCommand: string): Boolean;
var
  bCommand: String;
  cCommand: String;
begin
  Result := False;
  bCommand := copy(aCommand,0,pos('(',aCommand)-1);
  cCommand := copy(aCommand,length(bCommand)+1,length(aCommand));
  if bCommand = '' then bCommand := aCommand;
  if bCommand = 'Shutdown' then
    begin
      Terminate;
      DoDestroy := True;
      Result := True;
    end
  else if bCommand = 'Ping' then
    begin
      Result := True;
    end
  else if bCommand = 'ExecuteScript' then
    begin
      //TODO:execute script with pscripts
      Result := False;
    end
  ;
end;
constructor TBaseCustomApplication.Create(AOwner: TComponent);
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
  Self.OnException:=@BaseCustomApplicationException;
  //GetConsoleTextEncoding;
  DoDestroy := False;
  BaseApplication := Self;
  FMessageHandler := nil;
  {.$Warnings Off}
  FDBInterface := TBaseDBInterface.Create;
  FDBInterface.SetOwner(Self);
  {.$Warnings On}
end;
destructor TBaseCustomApplication.Destroy;
begin
  DoExit;
  if Assigned(FmessageHandler) then
    begin
      FMessagehandler.Terminate;
      sleep(20);
    end;
  FDBInterface.Data.Free;
  FLogger.Free;
  BaseApplication:=nil;
  inherited Destroy;
end;

function TBaseCustomApplication.GetOurConfigDir: string;
begin
  Result := GetConfigDir(StringReplace(lowercase(GetAppname),'-','',[rfReplaceAll]));
  if HasOption('c','config-path') then
    Result := GetOptionValue('c','config-path');
  result := AppendPathDelim(Result);
end;

procedure TBaseCustomApplication.SetConfigName(aName: string);
begin
//  Properties.FileName := GetOurConfigDir+aName+'.xml';
end;
procedure TBaseCustomApplication.RestoreConfig;
begin
  with Self as IBaseApplication do
    begin
//      Properties.Restore;
    end;
end;
procedure TBaseCustomApplication.SaveConfig;
begin
end;
function TBaseCustomApplication.GetLanguage: string;
begin

end;
procedure TBaseCustomApplication.SetLanguage(const AValue: string);
begin

end;
function TBaseCustomApplication.GetSingleInstance: Boolean;
begin
  Result := False;
end;

function TBaseCustomApplication.GetInternalTempDir: string;
begin
  Result := AppendPathDelim(GetTempPath);
end;

procedure TBaseCustomApplication.Debug(aMsg: string);
begin
  if HasOption('debug') then
    writeln('DEBUG:'+aMsg)
    ;
end;
function TBaseCustomApplication.GetLog: TEventLog;
begin
  Result := FLogger;
end;
function TBaseCustomApplication.GetAppName: string;
begin
  Result := FAppName;
end;
function TBaseCustomApplication.GetApprevision: Integer;
begin
  Result := FAppRevsion;
end;
function TBaseCustomApplication.GetAppVersion: real;
begin
  Result := FAppVersion;
end;
procedure TBaseCustomApplication.SetAppname(AValue: string);
begin
  FAppName := AValue;
end;
procedure TBaseCustomApplication.SetAppRevision(AValue: Integer);
begin
  FAppRevsion := AValue;
end;
procedure TBaseCustomApplication.SetAppVersion(AValue: real);
begin
  FAppVersion := AValue;
end;

function TBaseCustomApplication.GetQuickHelp: Boolean;
begin

end;

procedure TBaseCustomApplication.SetQuickhelp(AValue: Boolean);
begin

end;

function TBaseCustomApplication.GetMessageManager: TThread;
begin
  Result := FMessageHandler;
end;

procedure TBaseCustomApplication.Log(aType: string; aMsg: string);
begin
  try
    if Assigned(FLogger) then
      begin
        if aType = 'INFO' then
          begin
            if FLogger.LogType<>ltSystem then
              FLogger.Info(aMsg);
          end
        else if aType = 'WARNING' then
          FLogger.Warning(aMsg)
        else if aType = 'ERROR' then
          FLogger.Error(aMsg);
      end;
  except
  end;
end;
procedure TBaseCustomApplication.Log(aMsg: string);
begin
  Log('INFO',aMsg);
end;
procedure TBaseCustomApplication.Info(aMsg: string);
begin
  Log(aMsg)
end;
procedure TBaseCustomApplication.Warning(aMsg: string);
begin
  Log('WARNING',aMsg);
end;
procedure TBaseCustomApplication.Error(aMsg: string);
begin
  Log('ERROR',aMsg);
end;
function TBaseCustomApplication.Login: Boolean;
begin
  Result := True;
  try
  with Self as IBaseDbInterface do
    begin
      if not LoadMandants then
        raise Exception.Create(strFailedtoLoadMandants);
      if not HasOption('m','mandant') then
        raise Exception.Create(strMandantnotSelected);
      if not DBLogin(GetOptionValue('m','mandant'),GetOptionValue('u','user'),False,True) then
        raise Exception.Create(strLoginFailed+' '+LastError);
      uData.Data := Data;
    end;

  except
    on e : Exception do
      begin
        Result := False;
        raise;
      end;
  end;
end;
function TBaseCustomApplication.ChangePasswort: Boolean;
begin
  Result := False;
end;
procedure TBaseCustomApplication.RegisterMessageHandler;
begin
  FMessagehandler := TMessageHandler.Create(Data);
  FMessageHandler.RegisterCommandHandler(@HandleSystemCommand);
end;
procedure TBaseCustomApplication.Logout;
begin
  with Self as IBaseDbInterface do
    DBLogout;
end;
procedure TBaseCustomApplication.DoExit;
begin
  if Assigned(FMessageHandler) then FreeAndNil(FMessageHandler);
  with Self as IBaseDbInterface do
    DBLogout;
end;
end.

