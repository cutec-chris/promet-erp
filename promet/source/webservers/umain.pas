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

info@cu-tec.de
*******************************************************************************}
unit umain;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, LR_Class, httpdefs, fpHTTP, fpWeb, fpdatasetform, db,fpjson,
  LCLproc,uBaseDBInterface,FileUtil,LConvEncoding,uBaseDbClasses,fpsqlparser,
  fpsqlscanner, fpsqltree,httpsend,OpenSSL,jsonparser;

type

  { Tappbase }

  Tappbase = class(TFPWebModule)
    procedure checkloginRequest(Sender: TObject; ARequest: TRequest;
      AResponse: TResponse; var Handled: Boolean);
    procedure connectionavalibeRequest(Sender: TObject; ARequest: TRequest;
      AResponse: TResponse; var Handled: Boolean);
    procedure DataModuleBeforeRequest(Sender: TObject; ARequest: TRequest);
    procedure getstatisticRequest(Sender: TObject; ARequest: TRequest;
      AResponse: TResponse; var Handled: Boolean);
    procedure listRequest(Sender: TObject; ARequest: TRequest;
      AResponse: TResponse; var Handled: Boolean);
    procedure loginRequest(Sender: TObject; ARequest: TRequest;
      AResponse: TResponse; var Handled: Boolean);
    procedure logoutRequest(Sender: TObject; ARequest: TRequest;
      AResponse: TResponse; var Handled: Boolean);
    procedure objectRequest(Sender: TObject; ARequest: TRequest;
      AResponse: TResponse; var Handled: Boolean);
    procedure setobjectRequest(Sender: TObject; ARequest: TRequest;
      AResponse: TResponse; var Handled: Boolean);
    procedure syncRequest(Sender: TObject; ARequest: TRequest;
      AResponse: TResponse; var Handled: Boolean);
  private
    { private declarations }
    procedure DataSetToJSON(ADataSet: TDataSet; AJSON: TJSONArray; const ADateAsString: Boolean; Fields: TSQLElementList = nil);
    procedure ObjectToJSON(AObject : TBaseDBDataSet; AJSON: TJSONObject;const ADateAsString: Boolean);
  public
    { public declarations }
  end;

var
  appbase: Tappbase;

implementation
uses uStatistic,uData,uBaseWebSession,uPerson,uOrder,uMasterdata,utask,uProjects,
  uBaseDbDataSet,usync;
{$R *.lfm}
procedure Tappbase.checkloginRequest(Sender: TObject; ARequest: TRequest;
  AResponse: TResponse; var Handled: Boolean);
begin
  Handled:=True;
  if TBaseWebSession(Session).CheckLogin(ARequest,AResponse,True,False) then
    begin
      AResponse.Code:=200;
      AResponse.ContentType:='text/javascript;charset=utf-8';
      AResponse.CustomHeaders.Add('Access-Control-Allow-Origin: *');
      AResponse.Contents.Text := 'OnLoggedIn();';
      AResponse.SendContent;
    end
  else
    begin
      AResponse.Code:=200;
      AResponse.ContentType:='text/javascript;charset=utf-8';
      AResponse.CustomHeaders.Add('Access-Control-Allow-Origin: *');
      AResponse.Contents.Text := '';
      AResponse.SendContent;
    end;
end;
procedure Tappbase.connectionavalibeRequest(Sender: TObject;
  ARequest: TRequest; AResponse: TResponse; var Handled: Boolean);
begin
  TBaseWebSession(Session).ConnectionAvalible(ARequest,AResponse);
  AResponse.SendContent;
  Handled:=True;
end;

procedure Tappbase.DataModuleBeforeRequest(Sender: TObject; ARequest: TRequest);
begin
  debugln('Request:'+ARequest.URL);
end;

procedure Tappbase.getstatisticRequest(Sender: TObject; ARequest: TRequest;
  AResponse: TResponse; var Handled: Boolean);
var
  aStatistic: TStatistic;
  aDS: TDataSet;
  Json: TJSONArray;
  aStat: String;
  aQuerry: String;
  aState: Integer;
  bQuerry: String;
  aName: String;
  aType: String;
begin
  Handled:=True;
  if not TBaseWebSession(Session).CheckLogin(ARequest,AResponse,True) then exit;
  aStatistic := TStatistic.Create(nil,Data);
  aStatistic.Open;
  aStat := ARequest.QueryFields.Values['name'];
  if aStatistic.DataSet.Locate('NAME',aStat,[loCaseInsensitive]) then
    begin
      aDS := Data.GetNewDataSet(aStatistic.BuildQuerry(Arequest.QueryFields));
      aDS.Open;
      Json := TJSONArray.Create;
      DataSetToJSON(aDs,Json,True);
      Response.Contents.Text := 'handleData('+Json.AsJSON+');';
      Json.Free;
      aDS.Free;
      AResponse.Code:=200;
      AResponse.ContentType:='text/javascript;charset=utf-8';
      AResponse.CustomHeaders.Add('Access-Control-Allow-Origin: *');
      AResponse.SendContent;
    end
  else
    begin
      AResponse.Code:=404;
      AResponse.CodeText:='Document not found';
      AResponse.SendContent;
    end;
  aStatistic.Free;
end;
procedure Tappbase.listRequest(Sender: TObject; ARequest: TRequest;
  AResponse: TResponse; var Handled: Boolean);
var
  aList: String;
  aRight: String;
  Json: TJSONArray;
  aDs: TBaseDbList = nil;
  FSQLStream: TStringStream;
  FSQLScanner: TSQLScanner;
  FSQLParser: TSQLParser;
  aStmt: TSQLElement;
  aFilter: String;
  a: Integer;
  aSeq: String;
  http: THTTPSend;
  aClass: TBaseDBDatasetClass;
begin
  Handled:=True;
  if not TBaseWebSession(Session).CheckLogin(ARequest,AResponse,True,False) then exit;
  aList := lowercase(ARequest.QueryFields.Values['name']);
  aFilter := ARequest.QueryFields.Values['filter'];
  if aList <> '' then
    begin
      if aFilter<>'' then
        aList := aList+' where '+aFilter;
      FSQLStream := TStringStream.Create('select * from '+aList);
    end
  else
    FSQLStream := TStringStream.Create(ARequest.QueryFields.Values['ql']);
  FSQLScanner := TSQLScanner.Create(FSQLStream);
  Json := TJSONArray.Create;
  AResponse.Code:=200;
  AResponse.ContentType:='text/javascript;charset=utf-8';
  AResponse.CustomHeaders.Add('Access-Control-Allow-Origin: *');
  FSQLParser := TSQLParser.Create(FSQLScanner);
  try
    aStmt := FSQLParser.Parse;
    for a := 0 to TSQLSelectStatement(aStmt).Tables.Count-1 do
      begin
        aList := TSQLSimpleTableReference(TSQLSelectStatement(aStmt).Tables[a]).ObjectName.Name;
        if Data.DataSetFromLink(aList+'@',aClass) then
          begin
            aDs := TBaseDbList(aClass.Create(nil,Data));
            aRight := UpperCase(aList);
            if Assigned(TSQLSelectStatement(aStmt).Where) then
              aFilter:=TSQLSelectStatement(aStmt).Where.GetAsSQL([sfoDoubleQuoteIdentifier]);
            if (data.Users.Rights.Right(aRight)>RIGHT_READ) and (Assigned(aDS)) then
              begin
                if (aDs.ActualFilter<>'') and (aFilter<>'') then
                  aDs.Filter('('+aDs.ActualFilter+') AND ('+aFilter+')')
                else if (aFilter<>'') then
                  aDs.Filter(ARequest.QueryFields.Values['filter'])
                else
                  aDs.Open;
                DataSetToJSON(aDs.DataSet,Json,True,TSQLSelectStatement(aStmt).Fields);
              end
            else if (not Assigned(aDs)) and (pos('.',aList)>0) then
              begin
                //TODO:YQL Querys
                //http://query.yahooapis.com/v1/public/yql?q=select%20*%20from%20html%20where%20url%3D%27http%3A%2F%2Fmashable.com%27
                http := THTTPSend.Create;
                http.UserAgent:='Mozilla/5.0 (Windows NT 5.1; rv:6.0.2)';
                http.HTTPMethod('GET','https://query.yahooapis.com/v1/public/yql?q='+HTTPEncode(FSQLStream.DataString));
                if http.ResultCode=200 then
                  http.Document.SaveToFile('document.xml');
                http.Free;
              end
            else
              AResponse.Code:=403;
            if Assigned(aDs) then                                                           if Assigned(TSQLSelectStatement(aStmt).Where) then

              aDS.Free;
          end
        else
          AResponse.Code:=403;
      end;
  except
    on e : ESQLParser do
      begin
        if e.Col > 0 then
          begin
            AResponse.Code := 401;
            AResponse.Contents.Text:='['+IntToStr(e.Line)+':'+IntToStr(e.Col)+'] '+e.Message+','+FSQLParser.CurSource;
          end;
      end;
  end;

  FSQLParser.Free;
  FSQLScanner.Free;

  aSeq := ARequest.QueryFields.Values['sequence'];
  if aSeq='' then aSeq := '0';
  Response.Contents.Text := 'DoHandleObject('+aSeq+','+Json.AsJSON+');';
  if AResponse.Code=200 then
    Response.Contents.Text := 'DoHandleList('+aSeq+','+Json.AsJSON+');';
  Json.Free;

  AResponse.SendContent;
end;
procedure Tappbase.loginRequest(Sender: TObject; ARequest: TRequest;
  AResponse: TResponse; var Handled: Boolean);
begin
  TBaseWebSession(Session).DoLogin(Arequest,AResponse);
  AResponse.SendContent;
  Handled:=True;
end;
procedure Tappbase.logoutRequest(Sender: TObject; ARequest: TRequest;
  AResponse: TResponse; var Handled: Boolean);
begin
  Handled:=True;
  TBaseWebSession(Session).DoLogout(ARequest,AResponse);
  AResponse.SendContent;
end;
procedure Tappbase.objectRequest(Sender: TObject; ARequest: TRequest;
  AResponse: TResponse; var Handled: Boolean);
var
  Json: TJSONObject;
  aList: String;
  aDs: TBaseDBDataset;
  aRight: String;
  aSeq: String;
  aClass: TBaseDBDatasetClass;
begin
  Handled:=True;
  if not TBaseWebSession(Session).CheckLogin(ARequest,AResponse,True,False) then exit;
  aList := lowercase(ARequest.QueryFields.Values['name']);
  if Data.DataSetFromLink(aList+'@',aClass) then
    begin
      aDs := TBaseDBDataset(aClass.Create(nil,Data));
      aRight := UpperCase(aList);
      if (data.Users.Rights.Right(aRight)>RIGHT_READ) and (Assigned(aDS)) then
        begin
          aDs.Select(ARequest.QueryFields.Values['id']);
          aDs.Open;
          Json := TJSONObject.Create;
          ObjectToJSON(aDs,Json,True);
          aSeq := ARequest.QueryFields.Values['sequence'];
          if aSeq='' then aSeq := '0';
          Response.Contents.Text := 'DoHandleObject('+aSeq+','+Json.AsJSON+');';
          Json.Free;
          AResponse.Code:=200;
          AResponse.ContentType:='text/javascript;charset=utf-8';
          AResponse.CustomHeaders.Add('Access-Control-Allow-Origin: *');
        end
      else
        AResponse.Code:=403;
      if Assigned(aDs) then
        aDS.Free;
    end
  else
    AResponse.Code:=403;
  AResponse.SendContent;
end;
procedure Tappbase.setobjectRequest(Sender: TObject; ARequest: TRequest;
  AResponse: TResponse; var Handled: Boolean);
var
  aList: String;
  aDs: TBaseDBDataset;
  aRight: String;
  aClass: TBaseDBDatasetClass;
begin
  Handled:=True;
  AResponse.Code:=500;
  if not TBaseWebSession(Session).CheckLogin(ARequest,AResponse,True,False) then exit;
  aList := lowercase(ARequest.QueryFields.Values['name']);
  aRight := UpperCase(aList);
  if Data.DataSetFromLink(aList+'@',aClass) then
    begin
      aDs := TBaseDBDataset(aClass.Create(nil,Data));
      if (data.Users.Rights.Right(aRight)>RIGHT_WRITE) and (Assigned(aDS)) then
        begin
        end
      else
        AResponse.Code:=403;
      if Assigned(aDs) then
        aDS.Free;
    end
  else
    AResponse.Code:=403;
  AResponse.SendContent;
end;
procedure Tappbase.syncRequest(Sender: TObject; ARequest: TRequest;
  AResponse: TResponse; var Handled: Boolean);
var
  aList: String;
  aDs: TBaseDBDataset;
  aRight: String;
  aClass: TBaseDBDatasetClass;
  Sync: TSyncItems;
  Json: TJSONArray;
  JsonIn: TJSONArray;
  aFilter: String;
  FSQLStream: TStringStream;
  FSQLScanner: TSQLScanner;
  FSQLParser: TSQLParser;
  aStmt: TSQLElement;
  a: Integer;
  aSyncType: String;
  aSeq: String;
  aParser: TJSONParser;
begin
  Handled:=True;
  AResponse.Code:=500;
  if not TBaseWebSession(Session).CheckLogin(ARequest,AResponse,True,False) then exit;
  aList := lowercase(ARequest.QueryFields.Values['name']);
  aFilter := ARequest.QueryFields.Values['filter'];
  if aList <> '' then
    begin
      if aFilter<>'' then
        aList := aList+' where '+aFilter;
      FSQLStream := TStringStream.Create('select * from '+aList);
    end
  else
    FSQLStream := TStringStream.Create(ARequest.QueryFields.Values['ql']);
  FSQLScanner := TSQLScanner.Create(FSQLStream);
  Json := TJSONArray.Create;
  AResponse.Code:=200;
  AResponse.ContentType:='text/javascript;charset=utf-8';
  AResponse.CustomHeaders.Add('Access-Control-Allow-Origin: *');
  FSQLParser := TSQLParser.Create(FSQLScanner);
  try
    aStmt := FSQLParser.Parse;
    for a := 0 to TSQLSelectStatement(aStmt).Tables.Count-1 do
      begin
        aList := TSQLSimpleTableReference(TSQLSelectStatement(aStmt).Tables[a]).ObjectName.Name;
        if Data.DataSetFromLink(aList+'@',aClass) then
          begin
            aDs := TBaseDbList(aClass.Create(nil,Data));
            aRight := UpperCase(aList);
            if Assigned(TSQLSelectStatement(aStmt).Where) then
              aFilter:=TSQLSelectStatement(aStmt).Where.GetAsSQL([sfoDoubleQuoteIdentifier]);
            if (data.Users.Rights.Right(aRight)>RIGHT_READ) and (Assigned(aDS)) then
              begin
                if (aDs.ActualFilter<>'') and (aFilter<>'') then
                  aDs.Filter('('+aDs.ActualFilter+') AND ('+aFilter+')')
                else if (aFilter<>'') then
                  aDs.Filter(ARequest.QueryFields.Values['filter'])
                else
                  aDs.Open;
                Sync := TSyncItems.Create(nil,Data);
                JsonIn := TJSONArray.Create;
                aSyncType := ARequest.QueryFields.Values['synctype'];
                aParser := TJSONParser.Create(ARequest.QueryFields.Values['data']);
                if aSyncType = '' then
                  aSyncType:='JS.'+aRight;
                Json := Sync.SyncDataSet(aDs,JsonIn,aSyncType);
                JsonIn.Free;
                Sync.Free;
              end
            else
              AResponse.Code:=403;
            aDS.Free;
          end
        else
          AResponse.Code:=403;
      end;
  except
    on e : ESQLParser do
      begin
        if e.Col > 0 then
          begin
            AResponse.Code := 401;
            AResponse.Contents.Text:='['+IntToStr(e.Line)+':'+IntToStr(e.Col)+'] '+e.Message+','+FSQLParser.CurSource;
          end;
      end;
  end;

  FSQLParser.Free;
  FSQLScanner.Free;

  aSeq := ARequest.QueryFields.Values['sequence'];
  if aSeq='' then aSeq := '0';
  Response.Contents.Text := 'DoHandleObject('+aSeq+','+Json.AsJSON+');';
  if AResponse.Code=200 then
    Response.Contents.Text := 'DoHandleList('+aSeq+','+Json.AsJSON+');';
  Json.Free;

  AResponse.SendContent;
end;
procedure Tappbase.DataSetToJSON(ADataSet: TDataSet; AJSON: TJSONArray;
  const ADateAsString: Boolean; Fields: TSQLElementList);
var
  VJSON: TJSONObject;
begin
  ADataSet.First;
  while not ADataSet.EOF do
  begin
    VJSON := TJSONObject.Create;
    FieldsToJSON(ADataSet.Fields, VJSON, ADateAsString, Fields);
    AJSON.Add(VJSON);
    ADataSet.Next;
  end;
end;
procedure Tappbase.ObjectToJSON(AObject: TBaseDBDataSet; AJSON: TJSONObject;
  const ADateAsString: Boolean);
var
  aArray: TJSONArray;
  aNewObj: TJSONObject;
  i: Integer;
begin
  aArray := TJSONArray.Create;
  DataSetToJSON(AObject.DataSet,aArray,ADateAsString);
  AJSON.Add('Fields',aArray);
  with AObject as IBaseSubDataSets do
    for i := 0 to GetCount-1 do
      begin
        aNewObj := TJSONObject.Create;
        ObjectToJSON(SubDataSet[i],aNewObj,ADateAsString);
        AJSON.Add(SubDataSet[i].Caption,aNewObj);
      end;
end;
initialization
  RegisterHTTPModule('main', Tappbase);
end.

