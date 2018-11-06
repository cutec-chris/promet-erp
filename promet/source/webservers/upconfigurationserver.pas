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
Created 28.03.2017
*******************************************************************************}
unit upconfigurationserver;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, uappserverhttp,uAppServer, uData;

type

  { TConfigSession }

  TConfigSession = class
  private
    FSocket: TAppNetworkThrd;
  public
    Url : string;
    Headers : TStringList;
    Method : string;
    Result : Integer;
    Input,Output : TMemoryStream;
    property Socket : TAppNetworkThrd read FSocket write FSocket;
    procedure ProcessConfigRequest;
  end;

implementation

uses uBaseApplication,uBaseDBInterface,uIntfStrConsts,uEncrypt,base64,uBaseDbClasses,
  uWiki;

function HandleConfigRequest(Sender : TAppNetworkThrd;Method, URL: string;SID : string;Parameters,Headers : TStringList;Input,Output : TMemoryStream;ResultStatusText : string): Integer;
var
  aSock: TConfigSession = nil;
  i: Integer;
begin
  Result := 404;
  ResultStatusText := '';
  for i := 0 to Sender.Objects.Count-1 do
    if TObject(Sender.Objects[i]) is TConfigSession then
      aSock := TConfigSession(Sender.Objects[i]);
  if not Assigned(aSock) then
    begin
      aSock := TConfigSession.Create;
      aSock.Socket := Sender;
      Sender.Objects.Add(aSock);
    end;
  aSock.Url:=Url;
  aSock.Headers := Headers;
  aSock.Method:=Method;
  aSock.Input := Input;
  aSock.Output := Output;
  Sender.InternalSynchronize(Sender,@aSock.ProcessConfigRequest,pos('dbstatus',lowercase(url))=0);
  Result := aSock.Result;
end;

{ TConfigSession }

procedure TConfigSession.ProcessConfigRequest;
var
  i: Integer;
  aParameters: TStringList;
  s: String;
  tmp: String;
  aResult: TStringList;
  aType: String;
  aServer: String;
  aPW: String;
  aUser: String;
  aOptions: String;
  aDB: String;
  aPassword: String;
  sl: TStringList;
  oldOrigin: String;
  aWiki: TWikiList;
  bUser: TUser;
  aStartPage: String;
  tmpData: TBaseDBModule;
  function BuildRight(aRight : string) : string;
  begin
    Result := '{"'+aRight+'": '+IntToStr(tmpData.Users.Rights.Right(aRight))+'}';
  end;
begin
  Result := 404;
  if copy(lowercase(url),0,15)='/configuration/' then
    begin
      aParameters := TStringList.Create;
      aParameters.Delimiter:=':';
      aParameters.NameValueSeparator:=':';
      aResult := TStringList.Create;
      if pos('?',Url)>0 then
        Url := copy(URL,0,pos('?',Url)-1);
      try
        aParameters.Clear;
        for i := 0 to Headers.Count-1 do
          begin
            s := Headers[i];
            tmp := copy(s,0,pos(':',s)-1);
            aParameters.Add(lowercase(tmp)+':'+trim(copy(s,pos(':',s)+1,length(s))));
          end;
        oldOrigin := Headers.Values['Access-Control-Allow-Origin'];
        headers.Clear;
        if oldOrigin<>'' then
          Headers.Add('Access-Control-Allow-Origin: '+oldOrigin)
        else
          Headers.Add('Access-Control-Allow-Origin: *');
        Headers.Add('Access-Control-Allow-Methods: GET, OPTIONS, POST');
        Headers.Add('Access-Control-Allow-Headers: Authorization,X-Requested-With');
        Headers.Add('Cache-Control: no-cache');
        if lowercase(Method) = 'options' then
          begin
            Result := 200;
            exit;
          end;
        if copy(lowercase(url),0,15)='/configuration/' then
          begin
            Url := copy(url,16,length(url));
            if lowercase(url) = 'add' then
              begin
                Result := 400;
                Input.Position:=0;
                aResult.LoadFromStream(Input);
                if pos(':',aResult.Text)>0 then
                  begin
                    tmp := copy(aResult.Text,pos(':',aResult.Text)+1,length(aResult.Text));
                    aType := copy(tmp,0,pos(';',tmp)-1);
                    tmp := copy(tmp,pos(';',tmp)+1,length(tmp));
                    aServer := copy(tmp,0,pos(';',tmp)-1);
                    tmp := copy(tmp,pos(';',tmp)+1,length(tmp));
                    aDB := copy(tmp,0,pos(';',tmp)-1);
                    tmp := copy(tmp,pos(';',tmp)+1,length(tmp));
                    aUser := copy(tmp,0,pos(';',tmp)-1);
                    tmp := copy(tmp,pos(';',tmp)+1,length(tmp));
                    aPW := copy(tmp,0,pos(';',tmp)-1);
                    tmp := copy(tmp,pos(';',tmp)+1,length(tmp));
                    aOptions := tmp;
                    Result := 503;
                    aResult.Clear;
                    //TODO:check if DB Connection works
                    with BaseApplication as IBaseDbInterface,BaseApplication as  IBaseApplication do
                      begin
                        aResult.Clear;
                        aResult.Add('SQL');
                        aResult.Add(aType+';'+aServer+';'+aDB+';'+aUser+';'+Encrypt(aPW,word(99998)));
                        aResult.SaveToFile(GetOurConfigDir+'standard.perml');
                        aResult.Clear;
                        Info('loading mandants...');
                        if not LoadMandants then
                          begin
                            Error(strFailedtoLoadMandants);
                            DeleteFile(GetOurConfigDir+'standard.perml');
                          end
                        else
                          begin
                            if DBLogin('standard','') then
                              begin
                                Result := 200;
                                uData.DataM := GetDB;
                              end
                            else
                              begin
                                Result := 403;
                                aResult.Text := LastError;
                                aresult.SaveToStream(Output);
                                Output.Position:=0;
                                DeleteFile(GetOurConfigDir+'standard.perml');
                              end;
                          end;
                      end;
                  end;
              end
            else if lowercase(url) = 'status' then
              begin
                if Assigned(uData.DataM) then
                  begin
                    if ((BaseApplication.HasOption('u','user') and (BaseApplication.HasOption('p','password')))) then
                      Result := 403
                    else
                      Result := 401;
                  end
                else
                  Result := 200;
              end
            else if lowercase(url)='userstatus' then
              begin
                tmpData := uData.Data;
                tmpData.CriticalSection.Enter;
                try
                  if ((BaseApplication.HasOption('u','user') and (BaseApplication.HasOption('p','password'))
                  and TBaseDBModule(tmpData).Authenticate(BaseApplication.GetOptionValue('u','user'),BaseApplication.GetOptionValue('p','password'))))
                  then
                    begin
                      Result := 200;
                    end
                  else
                    begin
                      aUser := aParameters.Values['authorization'];
                      if lowercase(copy(aUser,0,pos(' ',aUser)-1))<>'basic' then
                        begin
                          aResult.Free;
                          aParameters.Free;
                          Result:=401;
                          exit;
                        end;
                      Result:=401;
                      aUser := DecodeStringBase64(copy(aUser,pos(' ',aUser)+1,length(aUser)));
                      aPassword := copy(aUser,pos(':',aUser)+1,length(aUser));
                      aUser := copy(aUser,0,pos(':',aUser)-1);
                      if (Result = 401) then
                        if TBaseDBModule(tmpData).Authenticate(aUser,aPassword) then
                          Result := 200;
                    end;
                  if Result = 200 then
                    begin
                      aWiki := TWikiList.CreateEx(nil,tmpData);
                      aWiki.CreateTable;
                      try
                        bUser := TUser.CreateEx(nil,tmpData);
                        bUser.Open;
                        bUser.Locate('SQL_ID',tmpData.Users.Id.AsVariant,[]);
                        while (not aWiki.FindWikiPage('Promet-ERP-Help/users/'+StringReplace(bUser.UserName.AsString,' ','_',[rfReplaceAll]))) and (not bUser.FieldByName('PARENT').IsNull) do
                          begin
                            bUser.Locate('SQL_ID',bUser.FieldByName('PARENT').AsVariant,[]);
                          end;
                        aStartPage := 'Promet-ERP-Help/users/'+StringReplace(bUser.UserName.AsString,' ','_',[rfReplaceAll]);
                        if not aWiki.FindWikiPage(aStartPage) then
                          begin
                            aStartPage:='Promet-ERP-Help/users/Administrator';
                            if not aWiki.FindWikiPage(aStartPage) then
                              aStartPage:='';
                          end;
                      except
                      end;
                      aWiki.Free;
                      bUser.Free;
                      sl := TStringList.Create;
                      sl.Add('{"username": "'+tmpData.Users.FieldByName('NAME').AsString+'"');
                      if aStartPage<>'' then;
                        sl.Add(',"startpage": "'+aStartPage+'"');
                      sl.Add(',"rights": [');
                      sl.Add(BuildRight('DOCUMENTS')+',');
                      sl.Add(BuildRight('HISTORY')+',');
                      sl.Add(BuildRight('LISTS')+',');
                      sl.Add(BuildRight('TREE')+',');
                      sl.Add(BuildRight('MESSAGES')+',');
                      sl.Add(BuildRight('CALENDAR')+',');
                      sl.Add(BuildRight('ORDERS')+',');
                      sl.Add(BuildRight('PRODUCTION')+',');
                      sl.Add(BuildRight('CUSTOMERS')+',');
                      sl.Add(BuildRight('MASTERDATA')+',');
                      sl.Add(BuildRight('TASKS')+',');
                      sl.Add(BuildRight('WIKI')+',');
                      sl.Add(BuildRight('PROJECTS')+',');
                      sl.Add(BuildRight('SHEME')+',');
                      sl.Add(BuildRight('REPORTS')+',');
                      sl.Add(BuildRight('STORAGE')+',');
                      sl.Add(BuildRight('DISPOSITION')+',');
                      sl.Add(BuildRight('WEBSHOP')+',');
                      sl.Add(BuildRight('INVENTORY')+',');
                      sl.Add(BuildRight('ACCOUNTING')+',');
                      sl.Add(BuildRight('TIMEREG')+',');
                      sl.Add(BuildRight('STATISTICS')+',');
                      sl.Add(BuildRight('OPTIONS')+',');
                      sl.Add(BuildRight('MEETINGS'));
                      sl.Add(']');
                      sl.Add('}');
                      sl.SaveToStream(Output);
                      sl.Free;
                      Output.Position:=0;
                    end;
                  finally
                    tmpData.CriticalSection.Leave;
                  end;
                end
              else if lowercase(url)='dbstatus' then
                begin
                  Result := 500;
                  tmpData := uData.Data;//GetData(50);
                  if tmpData <> uData.DataM then
                    begin
                      Result := 200;
                      tmpData.CriticalSection.Leave;
                    end
                  else if tmpData.CriticalSection.TryEnter then
                    begin
                      Result := 200;
                      tmpData.CriticalSection.Leave;
                    end;
                  if Result = 500 then
                    BaseApplication.Terminate;
                end;
          end
      except
        Result:=500;
      end;
      aResult.Free;
      aParameters.Free;
    end;
end;

{ TPrometWebDAVMaster }

initialization
  uappserverhttp.RegisterHTTPHandler(@HandleConfigRequest);

end.

