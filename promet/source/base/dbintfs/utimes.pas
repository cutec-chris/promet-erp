{*******************************************************************************
Dieser Sourcecode darf nicht ohne gültige Geheimhaltungsvereinbarung benutzt werden
und ohne gültigen Vertriebspartnervertrag weitergegeben oder kommerziell verwertet werden.
You have no permission to use this Source without valid NDA
and copy it without valid distribution partner agreement
Christian Ulrich
info@cu-tec.de
Created 01.06.2006
*******************************************************************************}
unit uTimes;
{$mode objfpc}
interface
uses
  Classes, SysUtils, uBaseDBClasses, db, uBaseDBInterface;
type
  TTimes = class(TBaseDBDataSet)
  public
    constructor Create(aOwner : TComponent;DM : TComponent;aConnection : TComponent = nil;aMasterdata : TDataSet = nil);override;
    procedure DefineFields(aDataSet : TDataSet);override;
    procedure FillDefaults(aDataSet : TDataSet);override;
    procedure SetDisplayLabels(aDataSet : TDataSet);override;
  end;
implementation
resourcestring
  strEntry                              = 'Eintrag';
constructor TTimes.Create(aOwner: TComponent;DM : TComponent; aConnection: TComponent;
  aMasterdata: TDataSet);
begin
  inherited Create(aOwner,DM, aConnection, aMasterdata);
  with DataSet as IBaseDBFilter do
    begin
      SortFields := 'START';
      BaseSortFields := 'START';
      SortDirection := sdDescending;
      Limit := 500;
    end;
end;
procedure TTimes.DefineFields(aDataSet: TDataSet);
begin
  with aDataSet as IBaseManageDB do
    begin
      TableName := 'TIMES';
      if Assigned(ManagedFieldDefs) then
        with ManagedFieldDefs do
          begin
            Add('START',ftDateTime,0,True);
            Add('END',ftDateTime,0,False);
            Add('LINK',ftString,200,False);
            Add('PROJECT',ftString,260,False);
            Add('CATEGORY',ftString,60,False);
            Add('JOB',ftString,150,False);
            Add('NOTE',ftMemo,0,False);
            Add('ISPAUSE',ftString,1,True);
          end;
    end;
end;
procedure TTimes.FillDefaults(aDataSet: TDataSet);
begin
  aDataSet.FieldByName('ISPAUSE').AsString := 'N';
end;
procedure TTimes.SetDisplayLabels(aDataSet: TDataSet);
begin
  inherited SetDisplayLabels(aDataSet);
  SetDisplayLabelName(aDataSet,'LINK',strEntry);
end;
end.

