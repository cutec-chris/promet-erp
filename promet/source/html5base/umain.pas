unit umain;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, LR_Class, httpdefs, fpHTTP, fpWeb, fpdatasetform, db,fpjson,
  LCLproc;

type
  Tappbase = class(TFPWebModule)
    procedure connectionavalibeRequest(Sender: TObject; ARequest: TRequest;
      AResponse: TResponse; var Handled: Boolean);
    procedure getstatisticRequest(Sender: TObject; ARequest: TRequest;
      AResponse: TResponse; var Handled: Boolean);
    procedure loginRequest(Sender: TObject; ARequest: TRequest;
      AResponse: TResponse; var Handled: Boolean);
  private
    { private declarations }
    procedure FieldsToJSON(AFields: TFields; AJSON: TJSONObject; const ADateAsString: Boolean);
    procedure DataSetToJSON(ADataSet: TDataSet; AJSON: TJSONArray; const ADateAsString: Boolean);
    procedure JSONToFields(AJSON: TJSONObject; AFields: TFields; const ADateAsString: Boolean);
  public
    { public declarations }
  end;

var
  appbase: Tappbase;

implementation
uses uStatistic,uData,uBaseWebSession;
{$R *.lfm}

procedure Tappbase.connectionavalibeRequest(Sender: TObject;
  ARequest: TRequest; AResponse: TResponse; var Handled: Boolean);
begin
  TBaseWebSession(Session).ConnectionAvalible(ARequest,AResponse);
  Handled:=True;
end;
procedure Tappbase.getstatisticRequest(Sender: TObject; ARequest: TRequest;
  AResponse: TResponse; var Handled: Boolean);
var
  aStatistic: TStatistic;
  aDS: TDataSet;
  Json: TJSONArray;
  aStat: String;
  i: Integer;
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
  i :=  ARequest.QueryFields.Count;
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
    end
  else
    begin
      AResponse.Code:=404;
      AResponse.CodeText:='Document not found';
    end;
  aStatistic.Free;
end;
procedure Tappbase.loginRequest(Sender: TObject; ARequest: TRequest;
  AResponse: TResponse; var Handled: Boolean);
begin
  TBaseWebSession(Session).DoLogin(Arequest,AResponse);
  Handled:=True;
end;
procedure Tappbase.FieldsToJSON(AFields: TFields; AJSON: TJSONObject;
  const ADateAsString: Boolean);
var
  I: Integer;
  VField: TField;
  VFieldName: ShortString;
begin
  for I := 0 to Pred(AFields.Count) do
  begin
    VField := AFields[I];
    VFieldName := VField.FieldName;
    if VField.DataType = ftString then
      AJSON.Add(VFieldName, VField.AsString);
    if VField.DataType = ftBoolean then
      AJSON.Add(VFieldName, VField.AsBoolean);
    if VField.DataType = ftDateTime then
      if ADateAsString then
        AJSON.Add(VFieldName, VField.AsString)
      else
        AJSON.Add(VFieldName, VField.AsFloat);
    if VField.DataType = ftFloat then
      AJSON.Add(VFieldName, VField.AsFloat);
    if VField.DataType = ftInteger then
      AJSON.Add(VFieldName, VField.AsInteger);
  end;
end;
procedure Tappbase.DataSetToJSON(ADataSet: TDataSet; AJSON: TJSONArray;
  const ADateAsString: Boolean);
var
  VJSON: TJSONObject;
begin
  ADataSet.First;
  while not ADataSet.EOF do
  begin
    VJSON := TJSONObject.Create;
    FieldsToJSON(ADataSet.Fields, VJSON, ADateAsString);
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
    VField := AFields.FindField(VName);
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

initialization
  RegisterHTTPModule('main', Tappbase);
end.

