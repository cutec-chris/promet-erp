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
*******************************************************************************}
unit uTimes;
{$mode objfpc}
{$H+}
interface
uses
  Classes, SysUtils, uBaseDBClasses, db, uBaseDBInterface,uBaseDatasetInterfaces;
type

  { TTimes }

  TTimes = class(TBaseDbList)
    procedure aFieldGetText(Sender: TField; var aText: string;
      DisplayText: Boolean);
    procedure aFieldSetText(Sender: TField; const aText: string);
    procedure aFieldValidate(Sender: TField);
    procedure FDSDataChange(Sender: TObject; Field: TField);
  private
    FDS: TDataSource;
  public
    constructor CreateEx(aOwner : TComponent;DM : TComponent=nil;aConnection : TComponent = nil;aMasterdata : TDataSet = nil);override;
    destructor Destroy; override;
    procedure Open; override;
    procedure DefineFields(aDataSet : TDataSet);override;
    procedure FillDefaults(aDataSet : TDataSet);override;
    procedure SetDisplayLabels(aDataSet : TDataSet);override;
  end;
implementation
uses uProjects,utask;
resourcestring
  strEntry                              = 'Eintrag';

procedure TTimes.aFieldGetText(Sender: TField; var aText: string;
  DisplayText: Boolean);
begin
  aText := DayTimeToStr(Sender.AsFloat);
end;

procedure TTimes.aFieldSetText(Sender: TField; const aText: string);
begin
  Sender.AsFloat:=StrToDayTime(aText);
end;

procedure TTimes.aFieldValidate(Sender: TField);
begin
end;

procedure TTimes.FDSDataChange(Sender: TObject; Field: TField);
var
  aProject: TProject;
begin
  if Assigned(Field) then
    begin
      if (Field.FieldName='PROJECT') then
        begin
          aProject := TProject.Create(nil);
          aProject.SelectFromLink(Field.AsString);
          aProject.Open;
          if aProject.Count>0 then
            DataSet.FieldByName('PROJECTID').AsVariant:=aProject.Id.AsVariant
          else
            DataSet.FieldByName('PROJECTID').Clear;
          aProject.Free;
        end
      else if (Field.FieldName='END') or (Field.FieldName='START') then
        begin
          if DataSet.FieldByName('END').AsFloat-DataSet.FieldByName('START').AsFloat>0 then
            DataSet.FieldByName('DURATION').AsFloat:=((DataSet.FieldByName('END').AsFloat-DataSet.FieldByName('START').AsFloat)/8)*24;
        end;
    end;
end;

constructor TTimes.CreateEx(aOwner: TComponent;DM : TComponent; aConnection: TComponent;
  aMasterdata: TDataSet);
begin
  inherited CreateEx(aOwner,DM, aConnection, aMasterdata);
  with DataSet as IBaseDBFilter do
    begin
      SortFields := 'START';
      BaseSortFields := 'START';
      SortDirection := sdDescending;
      Limit := 500;
    end;
  FDS := TDataSource.Create(nil);
  FDS.DataSet := DataSet;
  FDS.OnDataChange:=@FDSDataChange;
end;

destructor TTimes.Destroy;
begin
  FDS.Free;
  inherited Destroy;
end;

procedure TTimes.Open;
var
  aField: TDateTimeField;
begin
  inherited Open;
  if Assigned(FieldByName('DURATION')) then
    begin
      aField := TDateTimeField(FieldByName('DURATION'));
      aField.OnGetText:=@aFieldGetText;
      aField.OnSetText:=@aFieldSetText;
      aField.EditMask:='';
      aField.OnValidate:=@aFieldValidate;
      {$IF FPC_FULLVERSION>20600}
      aField.ValidChars:=aField.ValidChars+['h','m','i','n','s'];
      {$endif}
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
            Add('DURATION',ftFloat,0,False);
            Add('LINK',ftString,400,False);
            Add('PROJECT',ftString,260,False);
            Add('PROJECTID',ftLargeint,0,False);
            Add('TASKID',ftLargeint,0,False);
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
initialization
  RegisterdataSetClass('TIMES',TTimes);
end.

