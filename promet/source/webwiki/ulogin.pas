unit ulogin;
{$mode objfpc}{$H+}
interface
uses
  SysUtils, Classes, httpdefs, fpHTTP, fpWeb, sha1, md5;
type
  TfmLogin = class(TFPWebModule)
    procedure DataModuleGetAction(Sender: TObject; ARequest: TRequest;
      var ActionName: String);
  private
    { private declarations }
  public
    { public declarations }
  end;
var
  fmLogin: TfmLogin;
implementation
{$R *.lfm}
procedure TfmLogin.DataModuleGetAction(Sender: TObject;
  ARequest: TRequest; var ActionName: String);
var
  Result: String;
  Path: String;
begin
  Path := '';
  If (ActionVar<>'') then
    Result:=ARequest.QueryFields.Values[ActionVar];
  If (Result='') then
    begin
      Result := copy(ARequest.PathInfo,2,length(ARequest.PathInfo));
      Result := copy(Result,0,pos('/',Result)-1);
    end;
  if Result = '' then
    Result:=ARequest.GetNextPathInfo;
  if (ARequest.PathInfo = '') or (ARequest.PathInfo = '/') or (Result = 'show') or (Result = 'forum') then
    begin
      Result := 'new';
      Path := copy(ARequest.PathInfo,2,length(ARequest.PathInfo));
      Path := copy(Path,pos('/',Path)+1,length(Path));
      Path := trim(Path);
    end;
  if Actions.FindAction(path) <> nil then
    Result := path;
  ActionName := Result;
end;
initialization
  RegisterHTTPModule('login', TfmLogin);
end.

