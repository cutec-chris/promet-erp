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
unit usync;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, uBaseDbClasses, db, uBaseDbInterface,uBaseApplication,
  fpjson,fpsqltree,LConvEncoding,synautil;
type
  TSyncTable = class(TBaseDBDataSet)
  public
    constructor Create(aOwner : TComponent;DM : TComponent;aConnection : TComponent = nil;aMasterdata : TDataSet = nil);override;
    procedure DefineFields(aDataSet : TDataSet);override;
  end;
  TSyncDB = class(TBaseDBDataSet)
  private
    FTables: TSyncTable;
  public
    constructor Create(aOwner : TComponent;DM : TComponent;aConnection : TComponent = nil;aMasterdata : TDataSet = nil);override;
    destructor Destroy;override;
    procedure DefineFields(aDataSet : TDataSet);override;
    function CreateTable : Boolean;override;
    property Tables : TSyncTable read FTables;
  end;

  { TSyncItems }

  TSyncItems = class(TBaseDBDataSet)
  private
    function GetLocalID: TField;
    function GetRemoteID: TField;
    function GetSyncTime: TField;
    function GetType: TField;
  public
    procedure DefineFields(aDataSet : TDataSet);override;
    property SyncTime : TField read GetSyncTime;
    property LocalID : TField read GetLocalID;
    property RemoteID : TField read GetRemoteID;
    property Typ : TField read GetType;
    procedure SelectByReference(aID : Variant);
    procedure SelectByRemoteReference(aID : Variant);
    function SyncDataSet(aInternal : TBaseDBDataset;aExternal : TJSONArray;SyncType : string) : TJSONArray;
    function GetField(aObject: TJSONData; aName: string): TJSONData;
    function LastSync(SyncType : string) : TDateTime;
  end;
  TSyncStamps = class(TBaseDbDataSet)
    procedure DefineFields(aDataSet: TDataSet); override;
  end;
procedure FieldsToJSON(AFields: TFields; AJSON: TJSONObject; const ADateAsString: Boolean; bFields: TSQLElementList = nil);
procedure JSONToFields(AJSON: TJSONObject; AFields: TFields; const ADateAsString: Boolean; const AddFields: Boolean = True);
resourcestring
  strSynchedOut                                      = 'Synchronisation ausgehend %s';
  strSynchedIn                                       = 'Synchronisation eingehend %s';
implementation
uses Variants;
procedure FieldsToJSON(AFields: TFields; AJSON: TJSONObject;
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
    if Assigned(bFields) then
      begin
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
            then
              begin
                if aName <> '*' then
                  VFieldName:=aFName;
                Result := True;
                exit;
              end;
          end;
      end
    else
      begin
        Result := True;
        exit;
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
procedure JSONToFields(AJSON: TJSONObject; AFields: TFields;
  const ADateAsString: Boolean; const AddFields: Boolean);
var
  I: Integer;
  VName: string;
  VField: TField;
  VData: TJSONData;
  VdataStr: TJSONStringType;
begin
  for I := 0 to Pred(AJSON.Count) do
  begin
    VName := AJSON.Names[I];
    VField := AFields.FindField(uppercase(VName));
    if not Assigned(VField) then
      Continue;
    VData := AJSON.Items[I];
    if VData is TJSONArray then Continue;
    if VData is TJSONObject then Continue;
    VField.Clear;
    if VData.IsNull then
      Exit;
    VdataStr := VData.AsString;
    if (VField is TStringField) or (VField is TBinaryField) or
      (VField is TBlobField) or (VField is TVariantField)
    then
      begin
        if VData.JSONType=jtBoolean then
          begin
            if VData.AsBoolean then
              VField.AsString := 'Y'
            else
              VField.AsString := 'Y';
          end
        else
          VField.AsString := VdataStr
      end
    else if (VField is TLongintField) or (VField is TLargeintField) then
      VField.AsInteger := VData.AsInteger
    else if (VField is TFloatField) or (VField is TBCDField) or
      (VField is TFMTBCDField) then
      VField.AsFloat := VData.AsFloat
    else if VField is TBooleanField then
      VField.AsBoolean := VData.AsBoolean
    else if VField is TDateTimeField then
      begin
        if ADateAsString then
          VField.AsDateTime := DecodeRfcDateTime(VdataStr)
        else
          VField.AsDateTime := VData.AsFloat;
      end;
  end;
end;

{ TSyncStamps }

procedure TSyncStamps.DefineFields(aDataSet: TDataSet);
begin
  with aDataSet as IBaseManageDB do
    begin
      TableName := 'SYNCSTAMPS';
      if Assigned(ManagedFieldDefs) then
        with ManagedFieldDefs do
          begin
            Add('FROM',ftString,30,True);
            Add('NAME',ftString,30,True);
            Add('LTIMESTAMP',ftDateTime,0,False);
          end;
    end;
end;

function TSyncItems.GetLocalID: TField;
begin
  Result := FieldByName('LOCAL_ID');
end;

function TSyncItems.GetRemoteID: TField;
begin
  Result := FieldByName('REMOTE_ID');
end;

function TSyncItems.GetSyncTime: TField;
begin
  Result := FieldByName('SYNC_TIME');
end;

function TSyncItems.GetType: TField;
begin
  Result := FieldByName('SYNCTYPE');
end;

procedure TSyncItems.DefineFields(aDataSet: TDataSet);
begin
  with aDataSet as IBaseManageDB do
    begin
      TableName := 'SYNCITEMS';
      if Assigned(ManagedFieldDefs) then
        with ManagedFieldDefs do
          begin
            Add('SYNCTYPE',ftString,100,True);
            Add('SYNCTABLE',ftString,100,False);
            Add('LOCAL_ID',ftLargeInt,0,True);
            Add('REMOTE_ID',ftString,200,True);
            Add('USER_ID',ftLargeint,0,False);
            Add('REMOTE_TIME',ftDateTime,0,False);
            Add('SYNC_TIME',ftDateTime,0,False);
            Add('ERROR',ftString,1,False);
          end;
    end;
end;

procedure TSyncItems.SelectByReference(aID: Variant);
var
  aField: String = '';
begin
  with BaseApplication as IBaseDBInterface do
    with DataSet as IBaseDBFilter do
      begin
        aField := 'LOCAL_ID';
        if (VarIsNumeric(aID) and (aID = 0))
        or (VarIsStr(aID) and (aID = ''))
        or (aID = Null)  then
          begin
            with DataSet as IBaseManageDb do
              Filter := Data.QuoteField(TableName)+'.'+Data.QuoteField(aField)+'='+Data.QuoteValue('0');
          end
        else
          begin
            with DataSet as IBaseManageDb do
              Filter := Data.QuoteField(TableName)+'.'+Data.QuoteField(aField)+'='+Data.QuoteValue(Format('%d',[Int64(aID)]));
          end;
      end;
end;

procedure TSyncItems.SelectByRemoteReference(aID: Variant);
var
  aField: String = '';
begin
  with BaseApplication as IBaseDBInterface do
    with DataSet as IBaseDBFilter do
      begin
        Filter := Data.QuoteField('REMOTE_ID')+'='+Data.QuoteValue(aID);
      end;
end;

function TSyncItems.GetField(aObject : TJSONData;aName : string) : TJSONData;
begin
  Result := nil;
  if aObject is TJSONObject then
    begin
      if (not Assigned(result)) and (TJSONObject(aObject).IndexOfName(aName,True) > -1) then
        Result := aObject.Items[TJSONObject(aObject).IndexOfName(aName,True)];
    end;
end;
{
You have to push an JSON List with Data in aExternal with EXTERNAL_ID,TIMESTAMPD or TIMESTAMP filled
and get an List with changed Data since Lastsync with EXTERNAL_ID and TIMESTAMPD
}
function TSyncItems.SyncDataSet(aInternal: TBaseDBDataset;
  aExternal: TJSONArray; SyncType: string): TJSONArray;
var
  aLastSync: TDateTime;
  VJSON: TJSONObject;
  aObj: TJSONObject;
  aTime: TJSONData;
  DoSync: Boolean;
  i: Integer;
  aID: TJSONData;
  aSyncTime: TDateTime;
  tmp: TJSONStringType;
  Hist : IBaseHistory;
  function RoundToSecond(aDate : TDateTime) : TDateTime;
  begin
    Result := Round(aDate * SecsPerDay) / SecsPerDay;
  end;

begin
  Result := TJSONArray.Create;
  //Find Last Sync Time
  Filter(TBaseDBModule(DataModule).QuoteField('SYNCTYPE')+'='+TBaseDBModule(DataModule).QuoteValue(SyncType),0,'SYNC_TIME');
  Last;
  aLastSync := SyncTime.AsDateTime;
  //Sync internal items that are newer than last sync out
  aInternal.First;
  while not aInternal.EOF do
    begin
      if aInternal.TimeStamp.AsDateTime>aLastSync then
        begin
          DoSync := True;
          //check if newer version of row is in aExternal
          for i := 0 to aExternal.Count-1 do
            begin
              aObj := aExternal.Items[i] as TJSONObject;
              aID := GetField(aObj,'sql_id');
              aTime := GetField(aObj,'timestampd');
              if not Assigned(aTime) then
                aTime := GetField(aObj,'timestamp');
              if Assigned(aID) and Assigned(aTime)
              and (aID.AsString=IntToStr(aInternal.Id.AsLargeInt))
              and (DecodeRfcDateTime(aTime.AsString)>aInternal.TimeStamp.AsDateTime)
              then
                DoSync := False;
            end;
          if DoSync then
            begin
              SelectByReference(aInternal.Id.AsVariant);
              Open;
              if Count = 0 then
                Insert
              else Edit;
              VJSON := TJSONObject.Create;
              FieldsToJSON(aInternal.DataSet.Fields, VJSON, True);
              if RemoteID.AsString <> '' then
                VJSON.Add('EXTERNAL_ID',RemoteID.AsString)
              else ResmoteID := '';
              Result.Add(VJSON);
              if Supports(aInternal, IBaseHistory, Hist) then
                Hist.History.AddItem(aInternal.DataSet,Format(strSynchedOut,['Remote:'+DateTimeToStr(RoundToSecond(DecodeRfcDateTime(aTime.AsString)))+' Internal:'+DateTimeToStr(RoundToSecond(aInternal.TimeStamp.AsDateTime))+' Sync:'+DateTimeToStr(RoundToSecond(SyncTime.AsDateTime))]));
              LocalID.AsVariant:=aInternal.Id.AsVariant;
              Typ.AsString:=SyncType;
              SyncTime.AsDateTime:=Now();
              try
                Post;
              except
              end;
            end;
        end;
      aInternal.Next;
    end;
  //Sync external Items that are newer than internal in
  for i := 0 to aExternal.Count-1 do
    begin
      aObj := aExternal.Items[i] as TJSONObject;
      aID := GetField(aObj,'external_id');
      aTime := GetField(aObj,'timestampd');
      if not Assigned(aTime) then
        aTime := GetField(aObj,'timestamp');
      tmp := aTime.AsString;
      aSyncTime := DecodeRfcDateTime(tmp);
      DoSync := Assigned(aID) and Assigned(aTime) and (aSyncTime>aLastSync);
      if DoSync then
        begin
          SelectByRemoteReference(aID.AsString);
          Open;
          if Count = 0 then
            begin
              aID := GetField(aObj,'sql_id');
              if Assigned(aID) then
                begin
                  SelectByReference(aID.Value);
                  Open;
                  if Count = 0 then
                    Insert;
                end
              else
                Insert;
            end
          else Edit;
          aInternal.Select(LocalID.AsVariant);
          aInternal.Open;
          if aInternal.Count=0 then
            aInternal.Insert
          else aInternal.Edit;
          JSONToFields(aObj,aInternal.DataSet.Fields,True);
          aInternal.Post;
          LocalID.AsVariant:=aInternal.Id.AsVariant;
          RemoteID.AsVariant:=aID.AsString;
          Typ.AsString:=SyncType;
          SyncTime.AsDateTime:=Now();
          Post;
          if Supports(aInternal, IBaseHistory, Hist) then
            Hist.History.AddItem(aInternal.DataSet,Format(strSynchedIn,['Remote:'+DateTimeToStr(RoundToSecond(DecodeRfcDateTime(aTime.AsString)))+' Internal:'+DateTimeToStr(RoundToSecond(aInternal.TimeStamp.AsDateTime))+' Sync:'+DateTimeToStr(RoundToSecond(SyncTime.AsDateTime))]));
        end;
    end;
end;

function TSyncItems.LastSync(SyncType: string): TDateTime;
begin
  Filter(TBaseDBModule(DataModule).QuoteField('SYNCTYPE')+'='+TBaseDBModule(DataModule).QuoteValue(SyncType),0,'SYNC_TIME');
  Last;
  Result := SyncTime.AsDateTime;
end;

constructor TSyncTable.Create(aOwner: TComponent; DM: TComponent;
  aConnection: TComponent; aMasterdata: TDataSet);
begin
  inherited Create(aOwner, DM, aConnection, aMasterdata);
  with BaseApplication as IBaseDbInterface do
    begin
      with DataSet as IBaseDBFilter do
        begin
          SortFields := 'SQL_ID';
          SortDirection := sdAscending;
        end;
    end;
end;
procedure TSyncTable.DefineFields(aDataSet: TDataSet);
begin
  with aDataSet as IBaseManageDB do
    begin
      TableName := 'SYNCTABLE';
      if Assigned(ManagedFieldDefs) then
        with ManagedFieldDefs do
          begin
            Add('ACTIVE',ftString,1,False);
            Add('ACTIVEOUT',ftString,1,False);
            Add('NAME',ftString,30,True);
            Add('LTIMESTAMP',ftDateTime,0,False);
            Add('FILTERIN',ftMemo,0,False);
            Add('FILTEROUT',ftMemo,0,False);
          end;
    end;
end;
constructor TSyncDB.Create(aOwner: TComponent; DM: TComponent;
  aConnection: TComponent; aMasterdata: TDataSet);
begin
  inherited Create(aOwner, DM, aConnection, aMasterdata);
  FTables := TSyncTable.Create(Self,DM,aConnection,DataSet);
  with BaseApplication as IBaseDbInterface do
    begin
      with DataSet as IBaseDBFilter do
        begin
          BaseSortFields := 'SQL_ID';
          SortFields := 'SQL_ID';
          SortDirection := sdAscending;
        end;
    end;
end;
destructor TSyncDB.Destroy;
begin
  FTables.Destroy;
  inherited Destroy;
end;
procedure TSyncDB.DefineFields(aDataSet: TDataSet);
begin
  with aDataSet as IBaseManageDB do
    begin
      TableName := 'SYNCDB';
      if Assigned(ManagedFieldDefs) then
        with ManagedFieldDefs do
          begin
            Add('NAME',ftString,30,True);
            Add('ACTIVE',ftString,1,False);
            Add('PROPERTIES',ftMemo,0,False);
            Add('SYNCOFFS',ftInteger,0,False);
            Add('INPROGRESS',ftString,1,False);
          end;
    end;
end;
function TSyncDB.CreateTable : Boolean;
begin
  Result := inherited CreateTable;
  FTables.CreateTable;
end;

end.
