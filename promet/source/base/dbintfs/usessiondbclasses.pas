unit uSessionDBClasses;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,uBaseDbClasses, db, uBaseDbInterface, uBaseApplication, md5,uBaseDatasetInterfaces;
type
  TSessionVariables = class(TBaseDBDataSet)
  public
    procedure DefineFields(aDataSet : TDataSet);override;
    procedure Select(aName : string);overload;
  end;
  TSessionHistory = class(TBaseDBDataSet)
  public
    procedure DefineFields(aDataSet : TDataSet);override;
  end;

  { TSessions }

  TSessions = class(TBaseDBDataSet)
  private
    FHistory: TSessionHistory;
    FVariables: TSessionVariables;
  public
    constructor CreateEx(aOwner : TComponent;DM : TComponent=nil;aConnection : TComponent = nil;aMasterdata : TDataSet = nil);override;
    destructor Destroy;override;
    function CreateTable : Boolean;override;
    procedure DefineFields(aDataSet : TDataSet);override;
    procedure Select(SID : string);overload;
    procedure Select(IP,UserAgent : string);overload;
    property Variables : TSessionVariables read FVariables;
    property History : TSessionHistory read FHistory;
  end;
implementation
procedure TSessionHistory.DefineFields(aDataSet: TDataSet);
begin
  with aDataSet as IBaseManageDB do
    begin
      TableName := 'SESSIONHISTORY';
      if Assigned(ManagedFieldDefs) then
        with ManagedFieldDefs do
          begin
            Add('AUTO_ID',ftLargeint,0,True);
            Add('URL',ftMemo,0,True);
          end;
    end;
end;
procedure TSessionVariables.DefineFields(aDataSet: TDataSet);
begin
  with aDataSet as IBaseManageDB do
    begin
      TableName := 'SESSIONVARIABLES';
      if Assigned(ManagedFieldDefs) then
        with ManagedFieldDefs do
          begin
            Add('AUTO_ID',ftLargeInt,0,True);
            Add('NAME',ftString,60,True);
            Add('VALUE',ftMemo,0,False);
          end;
    end;
end;
procedure TSessionVariables.Select(aName: string);
begin
  with BaseApplication as IBaseDbInterface do
    begin
      with DataSet as IBaseDBFilter do
        begin
          Filter := Data.ProcessTerm(Data.QuoteField('NAME')+'='+Data.QuoteValue(aName));
          Limit := 1;
        end;
    end;
end;
constructor TSessions.CreateEx(aOwner: TComponent; DM: TComponent;
  aConnection: TComponent; aMasterdata: TDataSet);
begin
  inherited CreateEx(aOwner, DM, aConnection, aMasterdata);
  FHistory := TSessionHistory.CreateEx(Owner,DM,aConnection,DataSet);
  FVariables := TSessionVariables.CreateEx(Owner,DM,aConnection,DataSet);
  with DataSet as IBaseDBFilter do
    begin
      SortFields := 'AUTO_ID';
      SortDirection := sdDescending;
    end;
end;

destructor TSessions.Destroy;
begin
  try
    FreeAndNil(FHistory);
    FreeAndNil(FVariables);
  except
  end;
  inherited Destroy;
end;

function TSessions.CreateTable : Boolean;
begin
  Result := inherited CreateTable;
  FVariables.CreateTable;
  FHistory.CreateTable;
end;

procedure TSessions.DefineFields(aDataSet: TDataSet);
begin
  with aDataSet as IBaseManageDB do
    begin
      TableName := 'SESSIONS';
      if Assigned(ManagedFieldDefs) then
        with ManagedFieldDefs do
          begin
            Add('AUTO_ID',ftLargeint,0,True);
            Add('SID',ftString,40,True);
            Add('HOST',ftString,50,False);
            Add('CLIENT',ftString,32,false);
            Add('STARTED',ftDateTime,0,False);
            Add('LASTACCESS',ftDateTime,0,False);
            Add('TIMEOUT',ftInteger,0,False);
            Add('ISACTIVE',ftString,1,False);
          end;
    end;
end;
procedure TSessions.Select(SID: string);
begin
  with BaseApplication as IBaseDbInterface do
    begin
      with DataSet as IBaseDBFilter do
        begin
          Filter := Data.ProcessTerm(Data.QuoteField('SID')+'='+Data.QuoteValue(SID))+' and '+Data.ProcessTerm(Data.QuoteField('ISACTIVE')+'='+Data.QuoteValue(''));
          Limit := 1;
        end;
    end;
end;
procedure TSessions.Select(IP, UserAgent: string);
begin
  with BaseApplication as IBaseDbInterface do
    begin
      with DataSet as IBaseDBFilter do
        begin
          Filter := Data.ProcessTerm(Data.QuoteField('HOST')+'='+Data.QuoteValue(IP))+' and '+Data.ProcessTerm(Data.QuoteField('CLIENT')+'='+Data.QuoteValue(MD5Print(MD5String(UserAgent))))+' and '+Data.ProcessTerm(Data.QuoteField('ISACTIVE')+'='+Data.QuoteValue(''));
          Limit := 1;
        end;
    end;
end;
end.

