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
Created 08.08.2014
*******************************************************************************}
unit uprometscripts;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,uBaseDbClasses,uBaseDBInterface,db;

type
  TBaseScript = class(TBaseDBDataset)
  public
    procedure DefineFields(aDataSet: TDataSet); override;
    procedure FillDefaults(aDataSet: TDataSet); override;
  end;

implementation

procedure TBaseScript.DefineFields(aDataSet: TDataSet);
begin
  with aDataSet as IBaseManageDB do
    begin
      TableName := 'SCRIPTS';
      if Assigned(ManagedFieldDefs) then
        with ManagedFieldDefs do
          begin
            Add('TYPE',ftString,1,False);
            Add('PARENT',ftLargeint,0,False);
            Add('NAME',ftString,60,True);
            Add('SYNTAX',ftString,15,True);
            Add('SCRIPT',ftMemo,0,false);
          end;
      if Assigned(ManagedIndexdefs) then
        with ManagedIndexDefs do
          Add('NAME','NAME',[ixUnique]);
    end;
end;

procedure TBaseScript.FillDefaults(aDataSet: TDataSet);
begin
  FieldByName('SYNTAX').AsString:='Pascal';
  FieldByName('SCRIPT').AsString:='begin'+LineEnding+'  '+LineEnding+'end.';
  inherited FillDefaults(aDataSet);
end;

end.

