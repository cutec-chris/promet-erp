{*******************************************************************************
Dieser Sourcecode darf nicht ohne gültige Geheimhaltungsvereinbarung benutzt werden
und ohne gültigen Vertriebspartnervertrag weitergegeben oder kommerziell verwertet werden.
You have no permission to use this Source without valid NDA
and copy it without valid distribution partner agreement
Christian Ulrich
info@cu-tec.de
Created 01.06.2006
*******************************************************************************}
unit ubasehttpapplication;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, CustFCGI, uBaseApplication, uBaseDBInterface,
  PropertyStorage, uData, uSystemMessage, XMLPropStorage,HTTPDefs,fpHTTP,custhttpapp,
  uBaseDbClasses,db,md5,uSessionDBClasses,eventlog;
type
  TBaseHTTPApplication = class(TCustomHTTPApplication, IBaseApplication, IBaseDbInterface)
    procedure BaseHTTPApplicationException(Sender: TObject; E: Exception);
    procedure BaseHTTPApplicationGetModule(Sender: TObject; ARequest: TRequest;
      var ModuleClass: TCustomHTTPModuleClass);
  private
    FDBInterface: IBaseDbInterface;
    FDefaultModule: string;
    FMessageHandler: TMessageHandler;
    Properties: TXMLPropStorage;
    FLogger : TEventLog;
    FAppName : string;
    FAppRevsion : Integer;
    FAppVersion : Real;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function GetOurConfigDir: string;
    function GetAppName: string;
    function GetApprevision: Integer;
    function GetAppVersion: real;
    procedure SetConfigName(aName : string);
    procedure RestoreConfig;
    procedure SaveConfig;
    function GetConfig: TCustomPropertyStorage;
    function GetLanguage: string;
    procedure SetLanguage(const AValue: string);
    procedure SetAppname(AValue: string);virtual;
    procedure SetAppRevision(AValue: Integer);virtual;
    procedure SetAppVersion(AValue: real);virtual;
    function GetQuickHelp: Boolean;
    procedure SetQuickhelp(AValue: Boolean);

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
uses FileUtil,Utils,BlckSock, uUserAgents, LCLProc,ubasewebsession;
resourcestring
  strFailedtoLoadMandants    = 'Mandanten konnten nicht gelanden werden !';
  strLoginFailed             = 'Anmeldung fehlgeschlagen !';
procedure TBaseHTTPApplication.BaseHTTPApplicationException(Sender: TObject;
  E: Exception);
begin
  writeln('Error:'+e.Message);
  FLogger.Error(e.Message);
  Application.Terminate;
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
  Properties := TXMLPropStorage.Create(AOwner);
  Properties.FileName := GetOurConfigDir+'config.xml';
  Properties.RootNodePath := 'Config';
  AllowDefaultModule := True;
  Self.OnGetModule:=@BaseHTTPApplicationGetModule;
  Self.OnException:=@BaseHTTPApplicationException;
  Port := 8080;
  if Properties.ReadInteger('PORT',-1) <> -1 then
    Port:=Properties.ReadInteger('PORT',8080);
end;
destructor TBaseHTTPApplication.Destroy;
begin
  Properties.Free;
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
function TBaseHTTPApplication.GetOurConfigDir: string;
begin
  Result := GetConfigDir(StringReplace(lowercase(GetAppname),'-','',[rfReplaceAll]));
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
  if aDir <> '' then
    begin
      if not DirectoryExistsUTF8(aDir) then
        ForceDirectoriesUTF8(aDir);
    end;
  Properties.FileName := aDir+aName+'.xml';
  Properties.RootNodePath := 'Config';
end;
procedure TBaseHTTPApplication.RestoreConfig;
begin
  Properties.Restore;
  DefaultModule := Properties.ReadString('DEFAULTMODULE',DefaultModule);
end;
procedure TBaseHTTPApplication.SaveConfig;
begin
  Properties.Save;
end;
function TBaseHTTPApplication.GetConfig: TCustomPropertyStorage;
begin
  Result := Properties;
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
    debugln('DEBUG:'+aMsg);
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
  writeln('Login...');
  Result := False;
  with Self as IBaseDbInterface do
    begin
      if not LoadMandants('') then
        raise Exception.Create(strFailedtoLoadMandants);
      aMandant := GetOptionValue('m','mandant');
      if aMandant = '' then
        aMandant := Properties.ReadString('MANDANT','');
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

