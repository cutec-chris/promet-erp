unit uapiv2handling;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, HTTPDefs, websession,iniwebsession, fpHTTP, fpWeb, fpjson,
  ubasedbclasses, uPrometORM,fpjsonrtti, memds, uData, uMasterdata;

type
  TAPIV2Session = class(TIniWebSession)
  public
    User : TUser;
  end;

  { TAPIV2SessionFactory }

  TAPIV2SessionFactory = class(TIniSessionFactory)
  protected
    Function DoCreateSession(ARequest : TRequest) : TCustomSession; override;
  end;

  { TAPIV2Module }

  TAPIV2Module = class(TFPWebModule)
    constructor CreateNew(AOwner: TComponent; CreateMode: Integer); override;
  private
    procedure DoHandleRequest(Sender: TObject; ARequest: TRequest;
      AResponse: TResponse; var Handled: Boolean);
    procedure NewSession(Sender: TObject);
  end;

implementation

uses base64;

{ TAPIV2SessionFactory }

function TAPIV2SessionFactory.DoCreateSession(ARequest: TRequest
  ): TCustomSession;
begin
  Result := TAPIV2Session.Create(nil);
end;

{ TAPIV2Module }

constructor TAPIV2Module.CreateNew(AOwner: TComponent; CreateMode: Integer);
begin
  inherited CreateNew(AOwner, CreateMode);
  OnNewSession:=@NewSession;
  OnRequest:=@DoHandleRequest;
  CreateSession := True;
end;

procedure TAPIV2Module.DoHandleRequest(Sender: TObject; ARequest: TRequest;
  AResponse: TResponse; var Handled: Boolean);
var
  aUser: TUser;
  aStreamer, Streamer: TJSONStreamer;
  aOpt: TOption;
  aUsers: TMemDataset;
  tmp: String;
  aData : TBaseDBDataset;
  i: Integer;
begin
  Handled := False;
  if (not Assigned(TAPIV2Session(Session).User)) and (not Assigned(GlobalUser)) then
    begin
      if lowercase(copy(ARequest.Authorization,0,6))='basic ' then
        begin
          aUser := TUser.Create;
          try
            with aUser do
              begin
                tmp := DecodeStringBase64(copy(ARequest.Authorization,7,length(ARequest.Authorization)));
                if (not Data.Load(aUser,'NAME='+copy(tmp,0,pos(':',tmp)-1)+' OR LOGINNAME='+copy(tmp,0,pos(':',tmp)-1)+' OR EMAIL='+copy(tmp,0,pos(':',tmp)-1)))
                or (not CheckUserPasswort(copy(tmp,pos(':',tmp)+1,length(tmp))))
                then
                  begin
                    FreeAndNil(TAPIV2Session(Session).User);
                    Handled:=True;
                    Response.Code:=403;
                    Response.CodeText:='Access denied';
                    Response.SendContent;
                    exit;
                  end
                else TAPIV2Session(Session).User := aUser;
              end;
          finally
            if TAPIV2Session(Session).User <> aUser then
              FreeAndNil(aUser);
          end;
        end
      else
        begin
          Handled:=True;
          Response.Code:=401;
          Response.CodeText:='You must login first';
          Response.WWWAuthenticate:='Basic realm="Login first"';
          Response.SendContent;
          exit;
        end;
    end;
  Response.Code:=404;
  Response.CodeText:='Not found';
  Handled:=True;
  if ARequest.GetNextPathInfo = 'v2' then
    begin  //api/v2
      tmp := ARequest.GetNextPathInfo;
      for i := 0 to length(DatasetClasses)-1 do
        if DatasetClasses[i].aClass.InheritsFrom(TBaseDBDataset)
        and (lowercase(tmp) = lowercase(DatasetClasses[i].aClass.GetRealTableName))
        then
          begin
            Streamer := TJSONStreamer.Create(Self);
            aData := DatasetClasses[i].aClass.CreateEx(Self,Data);
            case aRequest.Method of
            'GET':
              begin
                Response.Code:=200;
                Response.CodeText:='OK';
                Response.Content:=Streamer.ObjectToJSON(aData).AsString;
                exit;
              end;
            end;
          end;
    end;
end;

procedure TAPIV2Module.NewSession(Sender: TObject);
begin
end;

initialization
  RegisterHTTPModule('api/v2', TAPIV2Module, True);
  SessionFactoryClass:=TAPIV2SessionFactory;
end.

