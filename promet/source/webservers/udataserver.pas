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
unit udataserver;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, httpdefs, fpHTTP, fpWeb, fpdatasetform, fphtml, db,fpjson,
  uBaseDBInterface,FileUtil,LConvEncoding,uBaseDbClasses,fpsqlparser,
  fpsqlscanner, fpsqltree,httpsend,OpenSSL, jsonparser;

type
  { TAppDataServer }

  TAppDataServer = class(TFPWebModule)
    procedure checkloginRequest(Sender: TObject; ARequest: TRequest;
      AResponse: TResponse; var Handled: Boolean);
    procedure connectionavalibeRequest(Sender: TObject; ARequest: TRequest;
      AResponse: TResponse; var Handled: Boolean);
    procedure DataModuleAfterInitModule(Sender: TObject; ARequest: TRequest);
    procedure DataModuleBeforeRequest(Sender: TObject; ARequest: TRequest);
    procedure getstatisticRequest(Sender: TObject; ARequest: TRequest;
      AResponse: TResponse; var Handled: Boolean);
    procedure ServerAccess(AMessage: string);
    procedure listRequest(Sender: TObject; ARequest: TRequest;
      AResponse: TResponse; var Handled: Boolean);
    procedure loginRequest(Sender: TObject; ARequest: TRequest;
      AResponse: TResponse; var Handled: Boolean);
    procedure logoutRequest(Sender: TObject; ARequest: TRequest;
      AResponse: TResponse; var Handled: Boolean);
    procedure objectRequest(Sender: TObject; ARequest: TRequest;
      AResponse: TResponse; var Handled: Boolean);
    function OpenLink(aLink: string; Sender: TObject): Boolean;
    procedure setobjectRequest(Sender: TObject; ARequest: TRequest;
      AResponse: TResponse; var Handled: Boolean);
    procedure syncRequest(Sender: TObject; ARequest: TRequest;
      AResponse: TResponse; var Handled: Boolean);
  private
    { private declarations }
    FirstUse : Boolean;
    procedure DataSetToJSON(ADataSet: TDataSet; AJSON: TJSONArray; const ADateAsString: Boolean; Fields: TSQLElementList = nil);
    procedure ObjectToJSON(AObject : TBaseDBDataSet; AJSON: TJSONObject;const ADateAsString: Boolean);
    procedure RegisterContent;
  public
    { public declarations }
  end;

var
  AppDataServer: TAppDataServer;

implementation
{$R *.lfm}
uses uStatistic,uData,uBaseWebSession,uPerson,uOrder,uMasterdata,utask,uProjects,
  uBaseDbDataSet,usync,uMessages,uBaseSearch,uWiki,uDocuments,umeeting,uBaseERPDBClasses,
  uBaseApplication;

procedure TAppDataServer.checkloginRequest(Sender: TObject; ARequest: TRequest;
  AResponse: TResponse; var Handled: Boolean);
begin
  Handled:=True;
  if TBaseWebSession(Session).CheckLogin(ARequest,AResponse,True,False) then
    begin
      if FirstUse then
        begin
          FirstUse := False;
          RegisterContent;
        end;
      AResponse.Contents.Text := 'OnLoggedIn();';
    end
  else
    begin
      AResponse.Contents.Text := '';
    end;
  AResponse.Code:=200;
  AResponse.ContentType:='text/javascript;charset=utf-8';
  AResponse.CustomHeaders.Add('Access-Control-Allow-Origin: *');
  AResponse.SendContent;
end;
procedure TAppDataServer.connectionavalibeRequest(Sender: TObject;
  ARequest: TRequest; AResponse: TResponse; var Handled: Boolean);
begin
  TBaseWebSession(Session).ConnectionAvalible(ARequest,AResponse);
  AResponse.SendContent;
  Handled:=True;
end;
procedure TAppDataServer.DataModuleAfterInitModule(Sender: TObject; ARequest: TRequest
  );
begin
  FirstUse := True;
end;
procedure TAppDataServer.DataModuleBeforeRequest(Sender: TObject; ARequest: TRequest);
begin
end;

procedure TAppDataServer.getstatisticRequest(Sender: TObject; ARequest: TRequest;
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
  AResponse.Code:=403;
  if TBaseWebSession(Session).CheckLogin(ARequest,AResponse,True) then
    begin
      AResponse.Code:=500;
      aStatistic := TStatistic.Create(nil);
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
        end
      else
        begin
          AResponse.Code:=404;
          AResponse.CodeText:='Document not found';
        end;
      aStatistic.Free;
    end;
  AResponse.SendContent;
end;

procedure TAppDataServer.ServerAccess(AMessage: string);
begin
end;

procedure TAppDataServer.listRequest(Sender: TObject; ARequest: TRequest;
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
  AResponse.Code:=403;
  if TBaseWebSession(Session).CheckLogin(ARequest,AResponse,True,False) then
    begin
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
                if aClass = TBaseHistory then
                  aDs := TBaseDbList(aClass.CreateEx(nil,Data,nil,Data.Users.DataSet)) //Use User History when History
                else
                  aDs := TBaseDbList(aClass.Create(nil));
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
                if Assigned(aDs) then
                  aDS.Free;
              end
            else
              AResponse.Code:=403;
          end;
        aStmt.Free
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
    end;
  AResponse.SendContent;
end;
procedure TAppDataServer.loginRequest(Sender: TObject; ARequest: TRequest;
  AResponse: TResponse; var Handled: Boolean);
begin
  TBaseWebSession(Session).DoLogin(Arequest,AResponse);
  if AResponse.Code=200 then
    begin
      RegisterContent;
    end;
  AResponse.SendContent;
  Handled:=True;
end;
procedure TAppDataServer.logoutRequest(Sender: TObject; ARequest: TRequest;
  AResponse: TResponse; var Handled: Boolean);
begin
  Handled:=True;
  TBaseWebSession(Session).DoLogout(ARequest,AResponse);
  AResponse.SendContent;
  {$ifdef DEBUG}
  BaseApplication.Terminate;
  {$endif}
end;
procedure TAppDataServer.objectRequest(Sender: TObject; ARequest: TRequest;
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
  AResponse.Code:=403;
  if not TBaseWebSession(Session).CheckLogin(ARequest,AResponse,True,False) then
    begin
      AResponse.Code:=500;
      aList := lowercase(ARequest.QueryFields.Values['name']);
      if Data.DataSetFromLink(aList+'@',aClass) then
        begin
          aDs := TBaseDBDataset(aClass.Create(nil));
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
    end;
  AResponse.SendContent;
end;
function TAppDataServer.OpenLink(aLink: string; Sender: TObject): Boolean;
begin

end;

procedure TAppDataServer.setobjectRequest(Sender: TObject; ARequest: TRequest;
  AResponse: TResponse; var Handled: Boolean);
var
  aList: String;
  aDs: TBaseDBDataset;
  aRight: String;
  aClass: TBaseDBDatasetClass;
begin
  Handled:=True;
  AResponse.Code:=403;
  if TBaseWebSession(Session).CheckLogin(ARequest,AResponse,True,False) then
    begin
      AResponse.Code:=500;
      aList := lowercase(ARequest.QueryFields.Values['name']);
      aRight := UpperCase(aList);
      if Data.DataSetFromLink(aList+'@',aClass) then
        begin
          aDs := TBaseDBDataset(aClass.Create(nil));
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
    end;
  AResponse.SendContent;
end;
procedure TAppDataServer.syncRequest(Sender: TObject; ARequest: TRequest;
  AResponse: TResponse; var Handled: Boolean);
var
  aList: String;
  aDs: TBaseDBDataset;
  aRight: String;
  aClass: TBaseDBDatasetClass;
  Sync: TSyncItems;
  Json: TJSONArray = nil;
  JsonIn: TJSONData;
  aFilter: String;
  FSQLStream: TStringStream;
  FSQLScanner: TSQLScanner;
  FSQLParser: TSQLParser;
  aStmt: TSQLElement;
  a: Integer;
  aSyncType: String;
  aSeq: String;
  aParser: TJSONParser;
  aInpData: String;
begin
  Handled:=True;
  AResponse.Code:=403;
  if TBaseWebSession(Session).CheckLogin(ARequest,AResponse,True,False) then
    begin
      AResponse.Code:=500;
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
                if aClass = TBaseHistory then
                  aDs := TBaseDbList(aClass.CreateEx(nil,Data,nil,Data.Users.DataSet)) //Use User History when History
                else
                  aDs := TBaseDbList(aClass.Create(nil));
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
                    Sync := TSyncItems.Create(nil);
                    aSyncType := ARequest.QueryFields.Values['synctype'];
                    aInpData := ARequest.QueryFields.Values['data'];
                    aParser := TJSONParser.Create(aInpData);
                    JsonIn := aParser.Parse;
                    aParser.Free;
                    if aSyncType = '' then
                      aSyncType:='App';
                    Json := Sync.SyncDataSet(aDs,JsonIn,aSyncType);
                    if not Assigned(Json) then
                      AResponse.Code:=500;
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
        aStmt.Free;
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
      FSQLStream.Free;

      aSeq := ARequest.QueryFields.Values['sequence'];
      if aSeq='' then aSeq := '0';
      if not Assigned(Json) then Json := TJSONArray.Create;
      Response.Contents.Text := 'DoHandleObject('+aSeq+','+Json.AsJSON+');';
      if AResponse.Code=200 then
        Response.Contents.Text := 'DoHandleList('+aSeq+','+Json.AsJSON+');';
      Json.Free;
    end;
  AResponse.SendContent;
end;
procedure TAppDataServer.DataSetToJSON(ADataSet: TDataSet; AJSON: TJSONArray;
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
procedure TAppDataServer.ObjectToJSON(AObject: TBaseDBDataSet; AJSON: TJSONObject;
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

procedure TAppDataServer.RegisterContent;
begin
  Data.RegisterLinkHandler('ALLOBJECTS',@OpenLink,TObjects);
  //Messages
  Data.RegisterLinkHandler('HISTORY',@OpenLink,TBaseHistory);
  //Messages
  if Data.Users.Rights.Right('MESSAGES') > RIGHT_NONE then
    begin
      try
        Data.RegisterLinkHandler('MESSAGEIDX',@OpenLink,TMessage);
        AddSearchAbleDataSet(TMessageList);
      except
      end;
    end;
  //Tasks
  if (Data.Users.Rights.Right('TASKS') > RIGHT_NONE) then
    begin
      try
      Data.RegisterLinkHandler('TASKS',@OpenLink,TTask,TTaskList);
      except
      end;
    end;
  //Add PIM Entrys
  if Data.Users.Rights.Right('CALENDAR') > RIGHT_NONE then
    begin
      try
        Data.RegisterLinkHandler('CALENDAR',@OpenLink,TTask,TTaskList);
      except
      end;
    end;
  //Orders
  if Data.Users.Rights.Right('ORDERS') > RIGHT_NONE then
    begin
      try
      Data.RegisterLinkHandler('ORDERS',@OpenLink,Torder);
      AddSearchAbleDataSet(TOrderList);
      except
      end;
    end;
  //Add Contacts
  if Data.Users.Rights.Right('CUSTOMERS') > RIGHT_NONE then
    begin
      try
      Data.RegisterLinkHandler('CUSTOMERS',@OpenLink,TPerson);
      AddSearchAbleDataSet(TPersonList);
      AddSearchAbleDataSet(TPersonContactData);
      AddSearchAbleDataSet(TPersonAddress);
      except
      end;
    end;
  //Add Masterdata stuff
  if (Data.Users.Rights.Right('MASTERDATA') > RIGHT_NONE) then
    begin
      try
      Data.RegisterLinkHandler('MASTERDATA',@OpenLink,TMasterdata);
      AddSearchAbleDataSet(TMasterdataList);
      except
      end;
    end;
  //Projects
  if (Data.Users.Rights.Right('PROJECTS') > RIGHT_NONE) then
    begin
      try
      Data.RegisterLinkHandler('PROJECT',@OpenLink,TProject);
      AddSearchAbleDataSet(TProjectList);
      except
      end;
    end;
  //Wiki
  Data.RegisterLinkHandler('WIKI',@OpenLink,TWikiList);
  if (Data.Users.Rights.Right('WIKI') > RIGHT_NONE) then
    begin
      try
      AddSearchAbleDataSet(TWikiList);
      except
      end;
    end;
  //Documents
  if (Data.Users.Rights.Right('DOCUMENTS') > RIGHT_NONE) then
    begin
      try
      Data.RegisterLinkHandler('DOCUMENTS',@OpenLink,TDocument);
      //Data.RegisterLinkHandler('DOCPAGES',@OpenLink,TDocPages);
      except
      end;
    end;
  //Lists
  if (Data.Users.Rights.Right('LISTS') > RIGHT_NONE) then
    begin
      try
      Data.RegisterLinkHandler('LISTS',@OpenLink,TLists);
      AddSearchAbleDataSet(TLists);
      except
      end;
    end;
  //Meetings
  if (Data.Users.Rights.Right('MEETINGS') > RIGHT_NONE) then
    begin
      try
      Data.RegisterLinkHandler('MEETINGS',@OpenLink,TMeetings);
      AddSearchAbleDataSet(TMeetings);
      except
      end;
    end;
  //Inventory
  if (Data.Users.Rights.Right('INVENTORY') > RIGHT_NONE) then
    begin
      try
      Data.RegisterLinkHandler('INVENTORY',@OpenLink,TInventorys);
      except
      end;
    end;
  //Statistics
  if (Data.Users.Rights.Right('STATISTICS') > RIGHT_NONE) then
    begin
      try
      Data.RegisterLinkHandler('STATISTICS',@OpenLink,TStatistic);
      AddSearchAbleDataSet(TStatistic);
      except
      end;
    end;
  //Timeregistering
  AddSearchAbleDataSet(TUser);
  //History
  if Data.Users.Rights.Right('DOCUMENTS') > RIGHT_NONE then
    begin
      try
      AddSearchAbleDataSet(TBaseHistory);
      Data.RegisterLinkHandler('HISTORY',@OpenLink,TBaseHistory);
      except
      end;
    end;
end;

initialization
  RegisterHTTPModule('data', TAppDataServer);
end.

