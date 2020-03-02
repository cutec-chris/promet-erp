unit uapiv2handling;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, HTTPDefs, websession,iniwebsession, fpHTTP, fpWeb, fpjson,
  ubasedbclasses, uPrometORM,fpjsonrtti, memds, uData, db, uorder;

type
  TAPIV2Session = class(TIniWebSession)
  public
    User : TUser;
  end;

  TDatasetToJSONOption = (djoSetNull, djoCurrentRecord, djoPreserveCase);
  TDatasetToJSONOptions = set of TDatasetToJSONOption;

  { TAPIV2Module }

  TAPIV2Module = class(TFPWebModule)
    constructor CreateNew(AOwner: TComponent; CreateMode: Integer); override;
  private
    procedure DoHandleRequest(Sender: TObject; ARequest: TRequest;
      AResponse: TResponse; var Handled: Boolean);
  end;

implementation

uses base64;

{ TAPIV2Module }

constructor TAPIV2Module.CreateNew(AOwner: TComponent; CreateMode: Integer);
begin
  inherited CreateNew(AOwner, CreateMode);
  OnRequest:=@DoHandleRequest;
  CreateSession := True;
end;

procedure TAPIV2Module.DoHandleRequest(Sender: TObject; ARequest: TRequest;
  AResponse: TResponse; var Handled: Boolean);
var
  aUser: TUser;
  aStreamer, Streamer: TJSONStreamer;
  aOpt: TOption;
  aUsers, aDS: TMemDataset;
  tmp: String;
  aData : TBaseDBDataset;
  i: Integer;
  arr: TJSONArray;

  procedure DatasetToJSON(Dataset: TDataset; JSONArray: TJSONArray; Options: TDatasetToJSONOptions);
  var
    OldRecNo: Integer;
    RecordData: TJSONArray;
  begin
    if Dataset.IsEmpty then
      Exit;
    if djoCurrentRecord in Options then
    begin
      RecordData := TJSONArray.Create;
      DatasetToJSON(Dataset, RecordData, Options);
      JSONArray.Add(RecordData);
    end
    else
    begin
      Dataset.DisableControls;
      OldRecNo := Dataset.RecNo;
      try
        Dataset.First;
        while not Dataset.EOF do
        begin
          RecordData := TJSONArray.Create;
          DatasetToJSON(Dataset, RecordData, Options);
          JSONArray.Add(RecordData);
          Dataset.Next;
        end;
      finally
        Dataset.RecNo := OldRecNo;
        Dataset.EnableControls;
      end;
    end;
  end;
begin
  Handled := False;
  if (not Assigned(TAPIV2Session(Session).User)) and (not Assigned(GlobalUser)) then
    begin
      if lowercase(copy(ARequest.Authorization,0,6))='basic ' then
        begin
          aUser := TUser.Create(nil);
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
            Streamer.Options:=[jsoDateTimeAsString,jsoLowerPropertyNames,jsoSetAsString,jsoCheckEmptyDateTime];
            tmp := ARequest.GetNextPathInfo;
            case tmp of
            'by-id':
              begin
                aData := DatasetClasses[i].aClass.Create(Self);
                case aRequest.Method of
                'GET':
                  begin
                    if Data.Load(aData,ARequest.GetNextPathInfo) then
                      begin
                        Response.Code:=200;
                        Response.CodeText:='OK';
                        Response.Content:=Streamer.ObjectToJSON(aData).FormatJSON;
                        Response.ContentType:='application/json';
                        exit;
                      end;
                  end;
                end;
              end;
            'index.json':
              begin
                if Data.Select(DatasetClasses[i].aClass,aDS,0,100,'','SQL_ID') then
                  begin
                    Response.Code:=200;
                    Response.CodeText:='OK';
                    Response.ContentType:='application/json';
                    arr := TJSONArray.Create;
                    DatasetToJSON(aDS,arr,[]);
                    Response.Content:=Streamer.ObjectToJSON(arr).FormatJSON;
                  end;
              end;
            end;
          end;
    end;
end;

initialization
  RegisterHTTPModule('api/v2', TAPIV2Module, True);
  IniWebSessionClass:=TAPIV2Session;
end.

