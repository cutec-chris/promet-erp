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
  TWritelnFunc = procedure(const s: string) of object;
  TReadlnFunc = procedure(var s: string) of object;
  TBaseScript = class(TBaseDBDataset)
  private
    FRlFunc: TReadlnFunc;
    FWrFunc: TWritelnFunc;
  public
    procedure DefineFields(aDataSet: TDataSet); override;
    procedure FillDefaults(aDataSet: TDataSet); override;
    property Writeln : TWritelnFunc read FWrFunc write FWRFunc;
    property Readln : TReadlnFunc read FRlFunc write FRlFunc;
    function Execute : Boolean;
  end;

  function ProcessScripts : Boolean;//process Scripts that must be runned cyclic

implementation
uses uStatistic,uData;

function ProcessScripts : Boolean;//process Scripts that must be runned cyclic
var
  aScript: TBaseScript;
begin
  aScript := TBaseScript.Create(nil,Data);
  aScript.Filter(Data.QuoteField('RUNEVERY')+'>'+Data.QuoteField('0'));
  while not aScript.EOF do
    begin
      if aScript.FieldByName('STATUS').AsString<>'E' then
        if aScript.FieldByName('LASTRUN').AsDateTime+(aScript.FieldByName('RUNEVERY').AsInteger/MinsPerDay)<Now() then
          aScript.Execute;
      aScript.Next;
    end;
  aScript.Free;
end;

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
            Add('STATUS',ftString,3,false);
            Add('SYNTAX',ftString,15,True);
            Add('RUNEVERY',ftInteger,0,False);
            Add('LASTRUN',ftDateTime,0,False);
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

function TBaseScript.Execute: Boolean;
var
  aDS: TDataSet;
begin
  try
    result := False;
    Edit;
    FieldByName('STATUS').AsString:='R';
    Post;
    if lowercase(FieldByName('SYNTAX').AsString) = 'sql' then
      begin
        aDS := TBaseDBModule(DataModule).GetNewDataSet(ReplaceSQLFunctions(FieldByName('SCRIPT').AsString));
        try
          with aDS as IBaseDbFilter do
            DoExecSQL;
          Result := True;
          with aDS as IBaseDbFilter do
          if Assigned(Writeln) then Writeln('Num Rows Affected: '+IntToStr(NumRowsAffected));
        except
          on e : Exception do
            begin
              if Assigned(Writeln) then Writeln(e.Message);
              Result := False;
            end;
        end;
        aDS.Free;
      end;
    Edit;
    if Result then
      begin
        FieldByName('STATUS').AsString:='N';
        FieldByName('LASTRUN').AsDateTime:=Now();
      end
    else
      FieldByName('STATUS').AsString:='E';
    Post;
  except
    result := False;
  end;
end;

end.

