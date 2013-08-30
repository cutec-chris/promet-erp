unit uS3Storage;

{$mode objfpc}{$H+}

//code by Tim Anderson; used for tutorial in PC Plus http://www.pcplus.co.uk.

//The lastest version of this code can be found at:

// http://www.itwriting.com/s3.php

interface

uses
  Sysutils, Classes, strutils, blcksock, httpsend, synautil, synacode, ssl_openssl;

type
  TS3Storage = class(TObject)

  private
    { Private declarations }
    FError: TstringList;
    Fpublickey: string;
    FHttpPrefix: string;
    mHttp: THttpSend;
    FOnStatusChange: THookSocketStatus;

    procedure FSetUseSSL(value: boolean);
    procedure InitHttp;

    function FGetUseSSL: boolean;
    function GetAuthString(verb: string; MD5: string; ContentType: string; sDate: string; Request: string): string;


  public
    { Public declarations }
    property UseSSL: Boolean read FGetUseSSL write FSetUseSSL;
    property Error: TStringList read FError;
    property PublicKey: string read Fpublickey;
    property OnStatusChange: THookSocketStatus read FOnStatusChange write FOnStatusChange;

    procedure Abort(); //cancel the data transfer
    procedure OnSocketStatus(Sender: TObject; Reason: THookSocketReason; const Value: string);

    function GetS3Object(BucketName: string; ObjectName: string; DestStream: TStream): boolean;
    function PutS3Object(BucketName: string; ObjectName: string; theStream: TStream; isBinary: boolean): boolean;
    function DeleteS3Object(BucketName: string; ObjectName: string): boolean;
    function CreateBucket(BucketName: string): boolean;
    function DeleteBucket(BucketName: string): boolean;
    function ListBucketItems(BucketName: string; DestStream: TMemoryStream): boolean;
    function ListBuckets(DestStream: TMemoryStream): boolean;

    function GetUniqueBucketName(BucketName: string): string;
    function StripKeyFromBucketName(BucketName: string): string;
    function EncodeString(s: string): string;
    function DecodeString(s: string): string;

    constructor Create(apublickey: string; aprivatekey: string);
    destructor Destroy; override;

  end;


implementation

var
  Fprivatekey: string;

destructor TS3STorage.Destroy;
begin
  FError.free;
  inherited;
end;

constructor TS3Storage.Create(apublickey: string; aprivatekey: string);
begin
  FpublicKey := apublickey;
  FprivateKey := aprivatekey;
  FError := TStringList.Create;
  FHttpPrefix := 'http://';
end;

procedure TS3Storage.Abort;
begin

  if (mHTTP <> nil) then
  begin
    mHttp.Sock.AbortSocket;
  end;

end;

function TS3Storage.EncodeString(s: string): string;
begin
  Result := EncodeURLElement(trim(s));
end;

function TS3Storage.DecodeString(s: string): string;
begin
  Result := DecodeURL(s);
end;

procedure TS3Storage.FSetUSESSL(value: boolean);
begin

  if value then
    FHttpPrefix := 'https://'
  else
    FHttpPrefix := 'http://';

end;

function TS3Storage.FGetUseSSL: boolean;
begin

  result := (FHttpPrefix = 'https://');

end;


function TS3Storage.GetUniqueBucketName(BucketName: string): string;
begin

  result := FPublicKey + '-' + trim(BucketName);
end;

function TS3STorage.StripKeyFromBucketName(BucketName: string): string;
begin
  result := AnsiReplaceStr(BucketName, GetUniqueBucketName(''), '');
end;

function TS3Storage.CreateBucket(BucketName: string): boolean;
var
  sRequest: string;
  theResponse: TMemoryStream;
  sDate: string;
  sFinalAuth: string;

  begin

  inithttp;

  try

    sRequest := '/' + BucketName;

    sDate := RFC822DateTime(now);

    mhttp.Headers.Add('Date: ' + sDate);

    sFinalAuth := GetAuthString('PUT', '', '', sDate, sRequest);

    mhttp.Headers.Add(sFinalAuth);

    mhttp.HTTPMethod('PUT', FHttpPrefix + 's3.amazonaws.com' + sRequest);
    theResponse := mhttp.Document;
    theResponse.Position := 0;
    Result := uppercase(mhttp.ResultString) = 'OK';

    if not Result then begin
      self.FError.LoadFromStream(theResponse);
      self.FError.Insert(0,'Http Result: ' + mhttp.ResultString);
    end
    else
      self.FError.Clear;

  finally
    Freeandnil(mhttp);
  end;


end;

function TS3Storage.DeleteBucket(BucketName: string): boolean;

begin

  result := DeleteS3Object(BucketName, '');

end;

function TS3Storage.DeleteS3Object(BucketName: string; ObjectName: string): boolean;
var
  sRequest: string;
  theResponse: TMemoryStream;
  sDate: string;
  sFinalAuth: string;
begin

  inithttp;

  try

    if ObjectName <> '' then
      sRequest := '/' + BucketName + '/' + ObjectName
    else
      sRequest := '/' + BucketName;

    sDate := RFC822DateTime(now);

    mhttp.Headers.Add('Date: ' + sDate);

    sFinalAuth := GetAuthString('DELETE', '', '', sDate, sRequest);

    mhttp.Headers.Add(sFinalAuth);

    mhttp.HTTPMethod('DELETE', FHttpPrefix + 's3.amazonaws.com' + sRequest);
    theResponse := mhttp.Document;
    theResponse.Position := 0;
    result := (mhttp.ResultCode = 204); //does this prove success?

    if not Result then begin
      self.FError.LoadFromStream(theResponse);
      self.FError.Insert(0,'Http Result: ' + mhttp.ResultString);
    end
    else
      self.FError.Clear;

  finally
    Freeandnil(mhttp);
  end;


end;


function Ts3Storage.PutS3Object(BucketName: string; ObjectName: string; theStream: TStream; isBinary: boolean): boolean;
var
  sRequest: string;
  theResponse: TMemoryStream;
  sDate: string;
  sContentType: string;
  sFinalAuth: string;
begin

  inithttp;

  try

    sRequest := '/' + BucketName + '/' + ObjectName;

    sDate := RFC822DateTime(now);

    if isBinary then
      sContentType := 'binary/octet-stream'
    else
      sContentType := 'text/html';

    mhttp.MimeType := sContentType; //becomes content type
    mhttp.Headers.Add('Date: ' + sDate);

    mhttp.Document.LoadFromStream(theStream);

    sFinalAuth := self.GetAuthString('PUT', '', sContentType, sDate, sRequest);

    mhttp.Headers.Add(sFinalAuth);

    mhttp.HTTPMethod('PUT', FHttpPrefix + 's3.amazonaws.com' + sRequest);
    theResponse := mhttp.Document;
    theResponse.Position := 0;

    result := uppercase(mhttp.resultstring) = 'OK';

    if not Result then begin
      self.FError.LoadFromStream(theResponse);
      self.FError.Insert(0,'Http Result: ' + mhttp.ResultString);
    end;
    FError.Clear;


  finally
    Freeandnil(mhttp);
  end;

end;

function TS3Storage.GetS3Object(BucketName: string; ObjectName: string; DestStream: TStream): boolean;
var
  sRequest: string;
  theResponse: TMemoryStream;

  sDate: string;
  sFinalAuth: string;
begin

  InitHttp;

  try

    sRequest := '/' + BucketName + '/' + ObjectName;

    sDate := RFC822DateTime(now);

    mhttp.Headers.Add('Date: ' + sDate);

    sFinalAuth := GetAuthString('GET', '', '', sDate, sRequest);

    mhttp.Headers.Add(sFinalAuth);

    mhttp.HTTPMethod('GET', FHttpPrefix + 's3.amazonaws.com' + sRequest);
    theResponse := mhttp.Document;
    theResponse.Position := 0;

    result := uppercase(mhttp.resultstring) = 'OK';

    if result then begin

      theResponse.SaveToStream(DestStream);

      FError.clear;
    end
    else
    begin
      self.FError.LoadFromStream(theResponse);
      self.FError.Insert(0,'Http Result: ' + mhttp.ResultString);
    end;

  finally
    Freeandnil(mhttp);
  end;

end;

function TS3Storage.ListBuckets(DestStream: TMemoryStream): boolean;
var
  sRequest: string;
  theResponse: TMemoryStream;
  sDate: string;
  sFinalAuth: string;
begin

  InitHttp;

  try

    sRequest := '/';

    sDate := RFC822DateTime(now);

    mhttp.Headers.Add('Date: ' + sDate);

    sFinalAuth := GetAuthString('GET', '', '', sDate, sRequest);

    mhttp.Headers.Add(sFinalAuth);

    mhttp.HTTPMethod('GET', FHttpPrefix + 's3.amazonaws.com' + sRequest);
    theResponse := mhttp.Document;
    theResponse.Position := 0;

    result := uppercase(mhttp.resultstring) = 'OK';

    if result then begin
      DestStream.LoadFromStream(theResponse);
      FError.clear;
    end
    else
         begin
      self.FError.LoadFromStream(theResponse);
      self.FError.Insert(0,'Http Result: ' + mhttp.ResultString);
    end;

  finally
    Freeandnil(mhttp);
  end;



end;

function TS3Storage.ListBucketItems(BucketName: string; DestStream: TMemoryStream): boolean;
var
  sRequest: string;
  theResponse: TMemoryStream;
  sDate: string;
  sFinalAuth: string;
begin

  initHttp;

  try

    sRequest := '/' + BucketName;

    sDate := RFC822DateTime(now);

    mhttp.Headers.Add('Date: ' + sDate);

    sFinalAuth := GetAuthString('GET', '', '', sDate, sRequest);

    mhttp.Headers.Add(sFinalAuth);

    mhttp.HTTPMethod('GET', FHttpPrefix + 's3.amazonaws.com' + sRequest);
    theResponse := mhttp.Document;
    theResponse.Position := 0;

    result := uppercase(mhttp.resultstring) = 'OK';

    if result then begin
      DestStream.LoadFromStream(theResponse);
      FError.clear;
    end
    else
        begin
      self.FError.LoadFromStream(theResponse);
      self.FError.Insert(0,'Http Result: ' + mhttp.ResultString);
    end;

  finally
    Freeandnil(mhttp);
  end;

end;


function TS3Storage.GetAuthString(verb: string; MD5: string; ContentType: string; sDate: string; Request: string): string;
var
  sAuth: string;
  sHMac: string;
  sBase64: string;
begin

  sAuth := verb + chr(10) + MD5 + chr(10) + ContentType + chr(10) + sDate + chr(10) + Request;

//concert sAuth to hmac-sha1
  sHMac := HMAC_SHA1(sAuth, FPrivateKey);

//convert binary string to base 64
  sBase64 := EncodeBase64(sHMac);

//return auth string
  Result := 'Authorization: AWS ' + FPublicKey + ':' + sBase64;

end;

procedure TS3Storage.InitHttp;
begin

  if mHttp <> nil then
  begin
    mHttp.Sock.AbortSocket;
    Freeandnil(mhttp);
  end;

  mhttp := THttpSend.Create;
  mhttp.Clear;
 // mhttp.Protocol := '1.1';

  mhttp.Sock.OnStatus := @OnSocketStatus;

end;

procedure TS3Storage.OnSocketStatus(Sender: TObject; Reason: THookSocketReason; const Value: string);

begin
//could do progress report here
If Assigned(FOnStatusChange) then
 FOnStatusChange(Sender,Reason,Value);

end;


end.
