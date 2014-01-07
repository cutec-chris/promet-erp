unit usync;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, uBaseDbClasses, db, uBaseDbInterface,uBaseApplication;
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
  TSyncItems = class(TBaseDBDataSet)
  private
    function GetSyncTime: TField;
  public
    procedure DefineFields(aDataSet : TDataSet);override;
    property SyncTime : TField read GetSyncTime;
  end;
  TSyncStamps = class(TBaseDbDataSet)
    procedure DefineFields(aDataSet : TDataSet);override;
  end;

implementation

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

function TSyncItems.GetSyncTime: TField;
begin
  Result := FieldByName('SYNC_TIME');
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

