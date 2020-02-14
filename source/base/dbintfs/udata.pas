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
unit uData;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ubasedbclasses,memds;

type
  TBaseDBModule = class(TComponent)
  public
    ConfigPath : string;
    Mandant : string;
    procedure Connect;virtual;abstract;
    function Load(Obj: TPersistent; Selector: Variant; Cascadic: Boolean = True) : Boolean;virtual;abstract;
    function Save(Obj: TPersistent; Selector: Variant; Cascadic: Boolean = True) : Boolean;virtual;abstract;
    function Select(Obj: TClass; aFilter: string; aFields: string): TMemDataset;virtual;abstract;
    function Delete(Selector: Variant) : Boolean;virtual;abstract;
    function GetID : Int64;virtual;abstract;
  end;
  DatasetClass = record
    aName : string;
    aClass : TBaseDBDatasetClass;
  end;

var
  DataM : TBaseDBModule = nil;
  GlobalUser : TUser;
  DatasetClasses : array of DatasetClass;

function Data : TBaseDBModule;
procedure RegisterdataSetClass(aName: string;aClass : TBaseDBDatasetClass);

implementation

function Data: TBaseDBModule;
begin
  if not Assigned(DataM) then
    DataM := TBaseDBModule.Create(nil);
  Result := DataM;
end;

procedure RegisterdataSetClass(aName: string; aClass: TBaseDBDatasetClass);
var
  i: Integer;
  Found: Boolean;
begin
  Found := False;
  for i := low(DatasetClasses) to High(DatasetClasses) do
    if DatasetClasses[i].aName=aName then
      begin
        Found := True;
        break;
      end;
  if not Found then
    begin
      SetLength(DatasetClasses,length(DatasetClasses)+1);
      DatasetClasses[length(DatasetClasses)-1].aName:=aName;
      DatasetClasses[length(DatasetClasses)-1].aClass:=aClass;
    end;
end;

finalization
  FreeAndNil(GlobalUser);
  //FreeAndNil(DataM);
end.


