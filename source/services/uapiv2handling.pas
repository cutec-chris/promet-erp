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

  function DatasetToJSON(aDataset:TDataset):string;
   function fieldToJSON(thisField:TField):string;
   begin
     result := '"'+thisField.fieldName+'":';
     case thisField.DataType of
     ftInteger,
     ftSmallint,
     ftCurrency,
     ftFloat,
     ftLargeInt:
       result := result+thisField.AsString+',';
     ftDateTime:
       result := result+'"'+thisField.AsString+'"'+',';
     else
       result := result+'"'+thisField.AsString+'"'+',';
     end; // case
   end; // of fieldToJSON
    function rowToJSON(ds:TDataset):string;
    var
      fieldIx : integer;
    begin
      Result := '';
      for fieldIx := 0 to ds.fieldcount-1 do
        result := result + fieldToJSON(ds.Fields[fieldIx]);
      // trim comma after last col
      result := '{'+copy(result,0,length(result)-1)+'},'+#13;
    end; // of rowToJSON
  begin
    result := '';
    with aDataset do
    begin
      if not bof then first;
      while not eof do
      begin
        result := result + rowToJSON(aDataset);
        next;
      end;
    end;
    //strip last comma and add
    if length(result)>0 then
      result := copy(result,0,length(result)-2);
    result := '['+result+']';
  end; // of DSToJSON
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
                aDs := TMemDataSet.Create(nil);
                if Data.Select(DatasetClasses[i].aClass,aDS,0,10,'','SQL_ID,TIMESTAMPD') then
                  begin
                    Response.Code:=200;
                    Response.CodeText:='OK';
                    Response.ContentType:='application/json';
                    Response.Content:=DatasetToJSON(aDS);
                    FreeAndNil(aDS);
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

