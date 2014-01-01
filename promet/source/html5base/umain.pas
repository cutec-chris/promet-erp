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
  fpsqlscanner, fpsqltree,httpsend,OpenSSL;

type

  { Tappbase }

  Tappbase = class(TFPWebModule)
    procedure checkloginRequest(Sender: TObject; ARequest: TRequest;
      AResponse: TResponse; var Handled: Boolean);
    procedure connectionavalibeRequest(Sender: TObject; ARequest: TRequest;
      AResponse: TResponse; var Handled: Boolean);
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
  private
    { private declarations }
    procedure FieldsToJSON(AFields: TFields; AJSON: TJSONObject; const ADateAsString: Boolean; bFields: TSQLElementList = nil);
    procedure DataSetToJSON(ADataSet: TDataSet; AJSON: TJSONArray; const ADateAsString: Boolean; Fields: TSQLElementList = nil);
    procedure JSONToFields(AJSON: TJSONObject; AFields: TFields; const ADateAsString: Boolean);
    procedure ObjectToJSON(AObject : TBaseDBDataSet; AJSON: TJSONObject;const ADateAsString: Boolean);
    function GetListObject(aName : string) : TBaseDBList;
    function GetObject(aName : string) : TBaseDBDataset;
  public
    { public declarations }
  end;

var
  appbase: Tappbase;

implementation
uses uStatistic,uData,uBaseWebSession,uPerson,uOrder,uMasterdata,utask,uProjects,
  uBaseDbDataSet;
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
const
  ST_NEXTCHAR = 1;
  ST_NAME = 2;
  ST_TYPE=3;
begin
  Handled:=True;
  if not TBaseWebSession(Session).CheckLogin(ARequest,AResponse,True) then exit;
  aStatistic := TStatistic.Create(nil,Data);
  aStatistic.Open;
  aStat := ARequest.QueryFields.Values['name'];
  if aStatistic.DataSet.Locate('NAME',aStat,[loCaseInsensitive]) then
    begin
      aQuerry := aStatistic.FieldByName('QUERRY').AsString;
      bQuerry := '';
      aState := 1;
      while length(aQuerry)>0 do
        begin
          case aState of
          ST_NEXTCHAR:
            begin
              if copy(aQuerry,0,1)='@' then
                begin
                  aState:=ST_NAME;
                  aName := '';
                end
              else
                begin
                  bQuerry:=bQuerry+copy(aQuerry,0,1);
                end;
            end;
          ST_NAME:
            begin
              if copy(aQuerry,0,1)[1] in [#10,#13] then
                aState:=ST_NEXTCHAR
              else if copy(aQuerry,0,1) =':' then
                begin
                  aState:=ST_TYPE;
                  aType := '';
                end
              else
                begin
                  aName:=aName+copy(aQuerry,0,1);
                end;
            end;
          ST_TYPE:
            begin
              if copy(aQuerry,0,1)[1] in [#10,#13,'@'] then
                begin
                  aState:=ST_NEXTCHAR;
                  if copy(aQuerry,0,1)='@' then
                    begin
                      //Auswertung
                      if ARequest.QueryFields.Values[aName] <> '' then
                        bQuerry:=bQuerry+ARequest.QueryFields.Values[aName];
                    end;
                end
              else
                begin
                  aType:=aType+copy(aQuerry,0,1);
                end;
            end;
          else aState := ST_NEXTCHAR;
          end;
          aQuerry:=copy(aQuerry,2,length(aQuerry));
        end;
      aDS := Data.GetNewDataSet(bQuerry);
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
  //FSQLScanner.ExcludeKeywords := FExcludeKeywords;
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
        aDs := GetListObject(aList);
        aRight := UpperCase(aList);
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
begin
  Handled:=True;
  if not TBaseWebSession(Session).CheckLogin(ARequest,AResponse,True,False) then exit;
  aList := lowercase(ARequest.QueryFields.Values['name']);
  aDs := GetObject(aList);
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
  AResponse.SendContent;
end;
procedure Tappbase.setobjectRequest(Sender: TObject; ARequest: TRequest;
  AResponse: TResponse; var Handled: Boolean);
var
  aList: String;
  aDs: TBaseDBDataset;
  aRight: String;
begin
  Handled:=True;
  AResponse.Code:=500;
  if not TBaseWebSession(Session).CheckLogin(ARequest,AResponse,True,False) then exit;
  aList := lowercase(ARequest.QueryFields.Values['name']);
  aRight := UpperCase(aList);
  aDs := GetObject(aList);
  if (data.Users.Rights.Right(aRight)>RIGHT_WRITE) and (Assigned(aDS)) then
    begin

    end
  else
    AResponse.Code:=403;
  if Assigned(aDs) then
    aDS.Free;
  AResponse.SendContent;
end;
procedure Tappbase.FieldsToJSON(AFields: TFields; AJSON: TJSONObject;
  const ADateAsString: Boolean; bFields: TSQLElementList);
var
  I: Integer;
  VField: TField;
  VFieldName: ShortString;
  function FindField(aName : string) : Boolean;
  var
    a: Integer;
    aFName: string;
    aF: TSQLElement;
  begin
    Result := False;
    if not Assigned(aFields) then
      begin
        Result := True;
        exit;
      end;
    for a := 0 to bFields.Count-1 do
      begin
        aF := bFields[a];
        if af is TSQLSelectAsterisk then
          begin
            aFName:='*';
            Result := True;
            exit;
          end
        else if af is TSQLSelectField then
          aFName := aF.GetAsSQL([],0);
        if (UpperCase(aName) = Uppercase(aFName))
        or (UpperCase(aName) = 'ID') and (Uppercase(aFName)='SQL_ID')
        then
          begin
            if aName <> '*' then
              VFieldName:=aFName;
            Result := True;
            exit;
          end;
      end;
  end;

begin
  for I := 0 to Pred(AFields.Count) do
  begin
    VField := AFields[I];
    VFieldName := VField.FieldName;
    if (FindField(VFieldName) or (FindField('*'))) then
      begin
        if VField.DataType = ftBoolean then
          AJSON.Add(lowercase(VFieldName), VField.AsBoolean)
        else if VField.DataType = ftDateTime then
          begin
          if ADateAsString then
            AJSON.Add(lowercase(VFieldName), VField.AsString)
          else
            AJSON.Add(lowercase(VFieldName), VField.AsFloat);
          end
        else if VField.DataType = ftFloat then
          AJSON.Add(lowercase(VFieldName), VField.AsFloat)
        else if (VField.DataType = ftInteger) or (VField.DataType = ftLargeint) then
          AJSON.Add(lowercase(VFieldName), VField.AsInteger)
        else
          AJSON.Add(lowercase(VFieldName), ConvertEncoding(VField.AsString,guessEncoding(VField.AsString),EncodingUTF8))
      end;
  end;
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
procedure Tappbase.JSONToFields(AJSON: TJSONObject; AFields: TFields;
  const ADateAsString: Boolean);
var
  I: Integer;
  VName: string;
  VField: TField;
  VData: TJSONData;
begin
  for I := 0 to Pred(AJSON.Count) do
  begin
    VName := AJSON.Names[I];
    VField := AFields.FindField(uppercase(VName));
    if not Assigned(VField) then
      Continue;
    VData := AJSON.Items[I];
    VField.Clear;
    if VData.IsNull then
      Exit;
    if (VField is TStringField) or (VField is TBinaryField) or
      (VField is TBlobField) or (VField is TVariantField) then
      VField.AsString := VData.AsString;
    if (VField is TLongintField) or (VField is TLargeintField) then
      VField.AsInteger := VData.AsInteger;
    if (VField is TFloatField) or (VField is TBCDField) or
      (VField is TFMTBCDField) then
      VField.AsFloat := VData.AsFloat;
    if VField is TBooleanField then
      VField.AsBoolean := VData.AsBoolean;
    if VField is TDateTimeField then
      if ADateAsString then
        VField.AsDateTime := StrToDateTime(VData.AsString)
      else
        VField.AsDateTime := VData.AsFloat;
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
function Tappbase.GetListObject(aName: string): TBaseDBList;
var
  aList: String;
begin
  aList := lowercase(aName);
  case aList of
  'contacts','customers':
     begin
       aList := 'CUSTOMERS';
       Result := TPersonList.Create(nil,Data);
     end;
  'masterdata':
     begin
       Result := TMasterdataList.Create(nil,Data);
     end;
  'tasks':
     begin
       Result := TTaskList.Create(nil,Data);
       TTaskList(result).SelectActiveByUser(Data.Users.FieldByName('ACCOUNTNO').AsString);
     end;
  'projects':
     begin
       result := TProjectList.Create(nil,Data);
     end;
  'orders':
     begin
       result := TOrderList.Create(nil,Data);
     end;
  end;
end;
function Tappbase.GetObject(aName: string): TBaseDBDataset;
var
  aList: String;
begin
  aList := lowercase(aName);
  case aList of
  'contacts','customers':
     begin
       aList := 'CUSTOMERS';
       Result := TPerson.Create(nil,Data);
     end;
  'masterdata':
     begin
       Result := TMasterdata.Create(nil,Data);
     end;
  'tasks':
     begin
       Result := TTask.Create(nil,Data);
     end;
  'projects':
     begin
       result := TProject.Create(nil,Data);
     end;
  'orders':
     begin
       result := TOrder.Create(nil,Data);
     end;
  end;
end;
initialization
  RegisterHTTPModule('main', Tappbase);
end.

