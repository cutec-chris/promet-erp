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
interface
uses
  Classes, SysUtils, uBaseDBClasses, db, uBaseDBInterface;
type
  TTimes = class(TBaseDBDataSet)
  public
    constructor CreateEx(aOwner : TComponent;DM : TComponent=nil;aConnection : TComponent = nil;aMasterdata : TDataSet = nil);override;
    procedure DefineFields(aDataSet : TDataSet);override;
    procedure FillDefaults(aDataSet : TDataSet);override;
    procedure SetDisplayLabels(aDataSet : TDataSet);override;
  end;
implementation
resourcestring
  strEntry                              = 'Eintrag';
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
end.

