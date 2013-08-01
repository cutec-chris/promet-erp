{*******************************************************************************
Dieser Sourcecode darf nicht ohne gültige Geheimhaltungsvereinbarung benutzt werden
und ohne gültigen Vertriebspartnervertrag weitergegeben oder kommerziell verwertet werden.
You have no permission to use this Source without valid NDA
and copy it without valid distribution partner agreement
Christian Ulrich
info@cu-tec.de
Created 01.06.2006
*******************************************************************************}
unit uerror;
{$mode objfpc}{$H+}
interface
uses
  SysUtils, Classes, httpdefs, fpHTTP, fpWeb, GeoIP, BlckSock;
type

  { TfmError }

  TfmError = class(TFPWebModule)
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
    procedure DataModuleNewSession(Sender: TObject);
    procedure DataModuleRequest(Sender: TObject; ARequest: TRequest;
      AResponse: TResponse; var Handled: Boolean);
  private
    { private declarations }
    Redirects: TStringList;
  public
    { public declarations }
    procedure GetGeoData(aSession : TCustomSession;Arequest : TRequest);
  end; 
var
  fmError: TfmError;
function HTTPDate(aDate : TDateTime) : string;
implementation
{$R *.lfm}
uses uBaseApplication,Fileutil;
function HTTPDate(aDate : TDateTime) : string;
var
  Y: word;
  M: word;
  D: word;
begin
  DecodeDate(aDate,Y,M,D);
  Result := Format(FormatDateTime(HTTPDateFmt,aDate),
                      [HTTPDays[DayOfWeek(aDate)],HTTPMonths[M]]);
end;
procedure TfmError.DataModuleCreate(Sender: TObject);
begin
  Redirects := TStringList.Create;
  if FileExists('redirects.list') then
    Redirects.LoadFromFile('redirects.list');
end;
procedure TfmError.DataModuleDestroy(Sender: TObject);
begin
  Redirects.Destroy;
end;
procedure TfmError.DataModuleNewSession(Sender: TObject);
begin
//  Session.Variables['SpamPoints'] := '5';
end;
procedure TfmError.DataModuleRequest(Sender: TObject; ARequest: TRequest;
  AResponse: TResponse; var Handled: Boolean);
var
  aPath: String;
  aExt: String;
  aFile: TFileStream;
  Sock: TBlockSocket;
begin
  with BaseApplication as IBaseApplication do
    begin
      aPath := ARequest.PathInfo;
      if copy(aPath,0,1) = '/' then
        aPath := copy(aPath,2,length(aPath));
      aPath := CleanAndExpandDirectory(Config.ReadString('DOCROOTPATH','')+aPath);
      if copy(aPath,length(aPath),1) = '/' then
        aPath := copy(aPath,0,length(aPath)-1);
      if pos('?',aPath) > 0 then
        aPath := copy(aPath,0,pos('?',aPath)-1);
      aExt := ExtractFileExt(aPath);
    end;
  aExt := lowercase(ExtractFileExt(aPath));
  if FileExists(aPath)
  and ((aExt = '.css')
  or (aExt = '.js')
  or (aExt = '.jpg')
  or (aExt = '.jpeg')
  or (aExt = '.gif')
  or (aExt = '.png')
  or (aExt = '.txt')
  or (aExt = '.ico')
  ) then
    begin
      AResponse.LastModified := HTTPDate(FileDateTodateTime(FileAgeUTF8(aPath)));
      if (aExt = '.css')
      or (aExt = '.js')
      or (aExt = '.txt')
      then
        begin
          AResponse.Contents.LoadFromFile(aPath);
          if (aExt = '.js') then
            AResponse.ContentType := 'text/javascript'
          else
            AResponse.ContentType := 'text/'+copy(aExt,2,length(aExt));
          AResponse.Expires := HTTPDate(Now()+60);
          AResponse.SendContent;
        end
      else
        begin
          aFile := TFileStream.Create(aPath,fmOpenRead);
          AResponse.ContentType := 'image/'+copy(aExt,2,length(aExt));
          AResponse.Code := 200;
          AResponse.ContentStream := aFile;
          Aresponse.ContentLength := aFile.Size;
          AResponse.Expires := HTTPDate(Now()+365);
          AResponse.SendContent;
          aFile.Free;
        end;
      AResponse.CustomHeaders.Add('Cache-Control: public,max-age=3600'); //1 hour
    end
  else if lowercase(ARequest.PathInfo) = '/favicon.ico' then
    begin
      AResponse.Code := 404;
      AResponse.CodeText := 'Not found '+aPath;
    end
  else if Redirects.Values[ARequest.PathInfo] <> '' then
    begin
//      AResponse.SendRedirect(Redirects.Values[ARequest.PathInfo]);
      AResponse.Location := Redirects.Values[ARequest.PathInfo];
      AResponse.Code := 301;
      AResponse.CodeText := 'Moved Permanently';
    end
  else if FileExistsUTF8(aPath) and not DirectoryExistsUTF8(aPath) then
    begin
      aFile := TFileStream.Create(UTF8ToSys(aPath),fmOpenRead,fmShareDenyNone);
      AResponse.ContentType := 'application/'+copy(aExt,2,length(aExt));
      AResponse.Code := 200;
      AResponse.ContentStream := aFile;
      Aresponse.ContentLength := aFile.Size;
      AResponse.Expires := HTTPDate(Now()+31);
      AResponse.SendContent;
      aFile.Free;
    end
  else
    begin
      Session.Variables['SpamPoints'] := IntToStr(StrToIntDef(Session.Variables['SpamPoints'],0)+1);
      if lowercase(copy(ARequest.PathInfo,0,7)) = '/webdav' then
        Session.Variables['SpamPoints'] := IntToStr(StrToIntDef(Session.Variables['SpamPoints'],0)+20);
      if pos('proxyheader',lowercase(ARequest.PathInfo)) > 0 then
        Session.Variables['SpamPoints'] := IntToStr(StrToIntDef(Session.Variables['SpamPoints'],0)+20);
      GetGeoData(Session,ARequest);
      if Session.Variables['HostName'] = '' then
        begin
          Sock := TBlockSocket.Create;
          Session.Variables['HostName'] := AnsiToUTF8(Sock.ResolveIPToName(ARequest.RemoteAddress));
          Sock.Free;
        end;
      if Session.Variables['UserAgent'] = '' then
        Session.Variables['UserAgent'] := AnsiToUTF8(ARequest.UserAgent);

      AResponse.Code := 404;
      AResponse.CodeText := 'Not found';
    end;
  Handled := True;
end;
procedure TfmError.GetGeoData(aSession : TCustomSession;Arequest : TRequest);
var
  aGeoIP: TGeoIP;
  GeoIPCountry: TGeoIPCountry;
  GeoIPCity: TGeoIPCity;
begin
  try
    if (aSession.Variables['City'] = '') then
      begin
        with BaseApplication as IBaseApplication do
          aGeoIP := TGeoIP.Create(Config.ReadString('DOCROOTPATH','')+'GeoLiteCity.dat');
        if aGeoIP.GetCity(Arequest.RemoteAddress, GeoIPCity) = GEOIP_SUCCESS then
          begin
            aSession.Variables['City'] := AnsiToUTF8(GeoIPCity.City);
            aSession.Variables['Country'] := AnsiToUTF8(GeoIPCity.CountryName);
            aSession.Variables['AreaCode'] := IntToStr(GeoIPCity.AreaCode);
            aSession.Variables['DmaCode'] := IntToStr(GeoIPCity.DmaCode);
            aSession.Variables['Latitude'] := FloatToStr(GeoIPCity.Latitude);
            aSession.Variables['Lonitude'] := FloatToStr(GeoIPCity.Longitude);
            aSession.Variables['PostalCode'] := GeoIPCity.PostalCode;
            aSession.Variables['Region'] := AnsiToUTF8(GeoIPCity.Region);
          end
        else if (aSession.Variables['Country'] = '') and (aGeoIP.GetCountry(Arequest.RemoteAddress, GeoIPCountry) = GEOIP_SUCCESS) then
          begin
            aSession.Variables['Country'] := AnsiToUTF8(GeoIPCountry.CountryName);
          end;
        aGeoIP.Free;
      end;
  except
  end;
end;

initialization
  RegisterHTTPModule('error', TfmError);
end.

