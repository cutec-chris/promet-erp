unit ubasedaemonapplication;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, daemonapp, uBaseApplication, uBaseDBInterface,
  PropertyStorage, uData, uSystemMessage;
type
  TBaseDaemonApplication = class(TCustomDaemonApplication, IBaseApplication, IBaseDbInterface)
  private
    FDBInterface: IBaseDbInterface;
    FMessageHandler: TMessageHandler;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function GetSingleInstance : Boolean;
    procedure SetConfigName(aName : string);
    procedure RestoreConfig;
    procedure SaveConfig;
    function GetConfig: TCustomPropertyStorage;
    function GetLanguage: string;
    procedure SetLanguage(const AValue: string);

    procedure DoLog(aMsg : string);
    function Login : Boolean;
    procedure Logout;
    procedure DoExit;
    property IData : IBaseDbInterface read FDBInterface implements IBaseDBInterface;
    property MessageHandler : TMessageHandler read FMessageHandler;
  end;
implementation
resourcestring
  strFailedtoLoadMandants    = 'Mandanten konnten nicht gelanden werden !';
constructor TBaseDaemonApplication.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  BaseApplication := Self;
  {.$Warnings Off}
  FDBInterface := TBaseDBInterface.Create;
  FDBInterface.SetOwner(Self);
  {.$Warnings On}
  with Self as IBaseDbInterface do
    begin
      if not LoadMandants then
        raise Exception.Create(strFailedtoLoadMandants);
    end;
end;
destructor TBaseDaemonApplication.Destroy;
begin
  DoExit;
  if Assigned(FmessageHandler) then
    begin
      FMessagehandler.Terminate;
      sleep(20);
    end;
  FDBInterface.Data.Free;
  inherited Destroy;
end;
function TBaseDaemonApplication.GetSingleInstance: Boolean;
begin
  result := False;
end;
procedure TBaseDaemonApplication.SetConfigName(aName: string);
begin

end;
procedure TBaseDaemonApplication.RestoreConfig;
begin

end;
procedure TBaseDaemonApplication.SaveConfig;
begin

end;
function TBaseDaemonApplication.GetConfig: TCustomPropertyStorage;
begin

end;
function TBaseDaemonApplication.GetLanguage: string;
begin

end;
procedure TBaseDaemonApplication.SetLanguage(const AValue: string);
begin

end;
procedure TBaseDaemonApplication.DoLog(aMsg: string);
begin

end;
function TBaseDaemonApplication.Login: Boolean;
begin

end;
procedure TBaseDaemonApplication.Logout;
begin

end;
procedure TBaseDaemonApplication.DoExit;
begin
  with Self as IBaseDbInterface do
    DBLogout;
end;
initialization
  AppClass := TBaseDaemonApplication;
end.

