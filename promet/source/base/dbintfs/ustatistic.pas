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
Created 01.06.2006
*******************************************************************************}
unit uStatistic;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uBaseDbClasses, db, uBaseDbInterface,uIntfStrConsts;

type

  { TStatistic }

  TStatistic = class(TBaseDbList)
  public
    function GetTextFieldName: string;override;
    function GetNumberFieldName : string;override;
    function GetDescriptionFieldName: string; override;
    procedure DefineFields(aDataSet : TDataSet);override;
    constructor Create(aOwner: TComponent; DM: TComponent;
       aConnection: TComponent=nil; aMasterdata: TDataSet=nil); override;
    function BuildQuerry(aVariables : TStrings) : string;
  end;

implementation
uses uBaseApplication;
{ TStatistic }

function TStatistic.GetTextFieldName: string;
begin
  Result := 'NAME';
end;

function TStatistic.GetNumberFieldName: string;
begin
  Result := 'SQL_ID';
end;

function TStatistic.GetDescriptionFieldName: string;
begin
  Result:='DESCRIPTION';
end;

procedure TStatistic.DefineFields(aDataSet: TDataSet);
begin
  with aDataSet as IBaseManageDB do
    begin
      TableName := 'STATISTICS';
      TableCaption:=strStatistics;
      if Assigned(ManagedFieldDefs) then
        with ManagedFieldDefs do
          begin
            Add('NAME',ftString,40,True);
            Add('DESCRIPTION',ftMemo,0,False);
            Add('QUERRY',ftMemo,0,False);
            Add('DETAIL',ftMemo,0,False);
            Add('SUBDETAIL',ftMemo,0,False);
            Add('STATFIELD',ftString,20,False);
            Add('STATNFIELD',ftString,20,False);
            Add('CHARTTYPE',ftString,1,False);
            Add('TREEENTRY',ftLargeint,0,false);
          end;
    end;
end;

constructor TStatistic.Create(aOwner: TComponent; DM: TComponent;
  aConnection: TComponent; aMasterdata: TDataSet);
begin
  inherited Create(aOwner, DM, aConnection, aMasterdata);
  with BaseApplication as IBaseDbInterface do
    begin
      with DataSet as IBaseDBFilter do
        begin
          BaseSortFields := 'SQL_ID';
          SortFields := 'SQL_ID';
          SortDirection := sdAscending;
          Limit := 0;
        end;
    end;
end;
const
  ST_NEXTCHAR = 1;
  ST_NAME = 2;
  ST_TYPE=3;
function TStatistic.BuildQuerry(aVariables: TStrings): string;
var
  aQuerry: String;
  bQuerry : string = '';
  aState: Integer;
  aName: String;
  aType: String;
begin
  aQuerry := FieldByName('QUERRY').AsString;
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
                  if aVariables.Values[aName] <> '' then
                    bQuerry:=bQuerry+aVariables.Values[aName];
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
  Result := bQuerry;
end;

end.

