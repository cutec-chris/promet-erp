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
  Created 07.06.2014
*******************************************************************************}
unit uPasswordSave;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,uBaseDbClasses,db,uBaseDBInterface;

type

  { TPasswordSave }

  TPasswordSave = class(TBaseDBDataset)
  public
    procedure DefineFields(aDataSet: TDataSet); override;
    procedure Generate(aLen : Integer);
  end;

implementation

{ TPasswordSave }

procedure TPasswordSave.DefineFields(aDataSet: TDataSet);
begin
  with aDataSet as IBaseManageDB do
    begin
      TableName := 'PWSAVE';
      if Assigned(ManagedFieldDefs) then
        with ManagedFieldDefs do
          begin
            Add('NAME',ftString,200,False);
            Add('SITE',ftString,500,False);
            Add('USERNAME',ftString,400,False);
            Add('PASSWORD',ftString,400,False);
            Add('DATE',ftDateTime,0,False);
          end;
    end;
end;

procedure TPasswordSave.Generate(aLen: Integer);
begin

end;

end.

