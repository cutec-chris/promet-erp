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
unit uMeasurement;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,uBaseDbClasses,uBaseDBInterface,db;


type

  { TMeasurementData }

  TMeasurementData = class(TBaseDBDataset)
  public
    procedure DefineFields(aDataSet: TDataSet); override;
    constructor CreateEx(aOwner: TComponent; DM: TComponent;
      aConnection: TComponent=nil; aMasterdata: TDataSet=nil); override;
  end;

  { TMeasurement }

  TMeasurement = class(TBaseDBDataset)
  private
    FMesdata: TMeasurementData;
  public
    constructor CreateEx(aOwner: TComponent; DM: TComponent; aConnection: TComponent=nil; aMasterdata: TDataSet=nil); override;
    destructor Destroy; override;
    procedure DefineFields(aDataSet: TDataSet); override;
    function CreateTable: Boolean; override;
    property Data : TMeasurementData read FMesdata;
  end;

implementation

{ TMeasurement }

constructor TMeasurement.CreateEx(aOwner: TComponent; DM: TComponent;
  aConnection: TComponent; aMasterdata: TDataSet);
begin
  inherited CreateEx(aOwner, DM, aConnection, aMasterdata);
  FMesdata := TMeasurementData.CreateEx(Self,DM,aConnection,DataSet);
end;

destructor TMeasurement.Destroy;
begin
  FMesdata.Free;
  inherited Destroy;
end;

procedure TMeasurement.DefineFields(aDataSet: TDataSet);
begin
  with aDataSet as IBaseManageDB do
    begin
      TableName := 'MEASUREMENTS';
      if Assigned(ManagedFieldDefs) then
        with ManagedFieldDefs do
          begin
            Add('NAME',ftString,100,True);
            Add('ID',ftString,100,False);
            Add('TYPE',ftString,100,False);
            Add('CHART',ftString,1,False);
            Add('COLOR',ftString,30,False);
            Add('RANGE',ftString,20,False);
            Add('POSITION',ftString,1,False);
            Add('INTERPOLATE',ftString,1,False);
          end;
      if Assigned(ManagedIndexdefs) then
        with ManagedIndexDefs do
          begin
            Add('ID','ID',[]);
          end;
    end;
end;

function TMeasurement.CreateTable: Boolean;
begin
  Result:=inherited CreateTable;
  FMesdata.CreateTable;
end;

{ TMeasurementData }

procedure TMeasurementData.DefineFields(aDataSet: TDataSet);
begin
  with aDataSet as IBaseManageDB do
    begin
      TableName := 'MEASDATA';
      if Assigned(ManagedFieldDefs) then
        with ManagedFieldDefs do
          begin
            Add('DATA',ftFloat,0,True);
            Add('DATE',ftDate,0,True);
          end;
      if Assigned(ManagedIndexdefs) then
        with ManagedIndexDefs do
          begin
            Add('DATE','DATE',[]);
          end;
    end;
end;

constructor TMeasurementData.CreateEx(aOwner: TComponent; DM: TComponent;
  aConnection: TComponent; aMasterdata: TDataSet);
begin
  inherited;
  with DataSet as IBaseDBFilter do
    begin
      SortFields := 'DATE';
      SortDirection := sdDescending;
      UpdateFloatFields:=True;
      Limit:=5000;
    end;
end;

end.

