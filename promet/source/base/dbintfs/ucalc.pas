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
Created 22.02.2014
*******************************************************************************}
unit ucalc;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,uBaseDbClasses, db, uBaseDbInterface,uIntfStrConsts,uStatistic,
  MathParser,Utils;
type

  { TCalcVariables }

  TCalcVariables = class(TBaseDBDataset)
  public
    procedure DefineFields(aDataSet: TDataSet); override;
  end;

  { TCalcEnviroments }

  TCalcEnviroments = class(TBaseDBDataset)
  private
    FType: string;
    FVariables: TCalcVariables;
    procedure Settype(AValue: string);
  public
    constructor Create(aOwner : TComponent;DM : TComponent;aConnection : TComponent = nil;aMasterdata : TDataSet = nil);override;
    destructor Destroy; override;
    function CreateTable: Boolean; override;
    procedure DefineFields(aDataSet: TDataSet); override;
    property Variables : TCalcVariables read FVariables;
    procedure FillDefaults(aDataSet: TDataSet); override;
    property Typ : string read FType write Settype;
    function Calculate(aIn : string;aOut : TStrings) : Boolean;
  end;


implementation

{ TCalcVariables }

function ConvertTausend(LeftArg, RightArg: Extended;
  AssignedArguments: TMPArguments; var Error: TMPError;
  var ErrorDescription: string; var InvalidArguments: TMPArguments): Extended;
begin
  if (AssignedArguments=[]) then Begin
    InvalidArguments:=[];
    Error:=mpeMissingArgument;
    ErrorDescription:='left argument is missing';
  end else result:=LeftArg*1000;
end;

function ConvertHundert(LeftArg, RightArg: Extended;
  AssignedArguments: TMPArguments; var Error: TMPError;
  var ErrorDescription: string; var InvalidArguments: TMPArguments): Extended;
begin
  if (AssignedArguments=[]) then Begin
    InvalidArguments:=[];
    Error:=mpeMissingArgument;
    ErrorDescription:='left argument is missing';
  end else result:=LeftArg*100;
end;

procedure TCalcVariables.DefineFields(aDataSet: TDataSet);
begin
  with aDataSet as IBaseManageDB do
    begin
      TableName := 'CALCVARIABLES';
      if Assigned(ManagedFieldDefs) then
        with ManagedFieldDefs do
          begin
            Add('NAME',ftString,60,True);
            Add('FORMULA',ftMemo,0,True);
            Add('RESULT',ftFloat,0,False);
            Add('CALCALWAYS',ftString,1,False);
          end;
    end;
end;

{ TCalcEnviroments }

procedure TCalcEnviroments.Settype(AValue: string);
begin
  if FType=AValue then Exit;
  FType:=AValue;
  with  DataSet as IBaseDBFilter, DataSet as IBaseManageDB do
    begin
      Filter := '('+TBaseDBModule(DataModule).QuoteField('TYPE')+'='+TBaseDBModule(DataModule).QuoteValue(FType)+')';
    end;
end;

constructor TCalcEnviroments.Create(aOwner: TComponent; DM: TComponent;
  aConnection: TComponent; aMasterdata: TDataSet);
begin
  inherited Create(aOwner, DM, aConnection, aMasterdata);
  FVariables := TCalcVariables.Create(Owner,DM,aConnection,DataSet);
end;

destructor TCalcEnviroments.Destroy;
begin
  FVariables.Free;
  inherited Destroy;
end;

function TCalcEnviroments.CreateTable: Boolean;
begin
  Result:=inherited CreateTable;
  FVariables.CreateTable;
end;

procedure TCalcEnviroments.DefineFields(aDataSet: TDataSet);
begin
  with aDataSet as IBaseManageDB do
    begin
      TableName := 'CALCENVIROMENT';
      if Assigned(ManagedFieldDefs) then
        with ManagedFieldDefs do
          begin
            Add('NAME',ftString,60,True);
            Add('TYPE',ftString,4,False);
          end;
    end;
end;

procedure TCalcEnviroments.FillDefaults(aDataSet: TDataSet);
begin
  inherited FillDefaults(aDataSet);
  FieldByName('TYPE').AsString:=FType;
end;

function TCalcEnviroments.Calculate(aIn: string; aOut: TStrings): Boolean;
var
  aParser: TMathParser;
  aTree: PTTermTreeNode;
  Stmt: TSQLStatemnt;
  aVar: String;
  bOut: String;
  aDS: TDataSet;
begin
  if copy(aIn,0,2)='--' then
    begin
      aOut.Add(aIn);
      exit;
    end;
  if RPos('=',aIn)>0 then
    begin
      aVar := copy(aIn,RPos('=',aIn)+1,length(aIn));
      aIn := copy(aIn,0,RPos('=',aIn)-1);
    end;
  aParser := TMathParser.Create;
  aParser.AddOperatorEx('kilo',@ConvertKilo,[mpaLeft],MPOP_OPERATOR_LEFTONLY);
  aParser.AddOperatorEx('tausend',@ConvertTausend,[mpaLeft],MPOP_OPERATOR_LEFTONLY);
  aParser.AddOperatorEx('hundert',@ConvertHundert,[mpaLeft],MPOP_OPERATOR_LEFTONLY);
  with Variables do
    begin
      Active:=True;
      First;
      while not EOF do
        begin
          aParser.AddConstant(Variables.FieldByName('NAME').AsString,Variables.FieldByName('RESULT').AsFloat);
          Next;
        end;
    end;
  try
    aTree := aParser.ParseTerm(aIn);
  except
    aTree := nil;
  end;
  if Assigned(aTree) then
    begin
      try
        aOut.Add(aParser.FormatTerm(aTree));
        bOut := FloatToStr(aParser.CalcTree(aTree));
        if aVar <> '' then
          begin
            if Variables.Locate('NAME',aVar,[]) then
              Variables.Edit
            else Variables.Insert;
            Variables.FieldByName('NAME').AsString:=aVar;
            Variables.FieldByName('FORMULA').AsString:=aIn;
            Variables.FieldByName('RESULT').AsFloat:=aParser.CalcTree(aTree);
            Variables.Post;
            aOut.Add('='+bOut+'='+aVar)
          end
        else
          aOut.Add('='+bOut);
      except
        on e : Exception do
          aOut.Add('='+e.Message);
      end;
    end
  else
    begin
      Stmt := TSQLStatemnt.Create;
      Stmt.SQL:=aIn;
      if Stmt.Parse then
        begin
          aDS := TBaseDBModule(DataModule).GetNewDataSet(Stmt.SQL);
          try
            aDS.Open;
            aOut.Add(Stmt.SQL);
            bOut := aDS.Fields[0].AsString;
            if (aVar <> '') and (aDs.Fields.Count=1) and (aDS.RecordCount=1) and (aDS.Fields[0].ClassType = TFloatField) then
              begin
                if Variables.Locate('NAME',aVar,[]) then
                  Variables.Edit
                else Variables.Insert;
                Variables.FieldByName('NAME').AsString:=aVar;
                Variables.FieldByName('FORMULA').AsString:=aIn;
                Variables.FieldByName('RESULT').AsFloat:=aDS.Fields[0].AsFloat;
                Variables.Post;
                aOut.Add('='+bOut+'='+aVar)
              end
            else
              aOut.Add('='+bOut);
          except
          end;
          aDS.Free;
        end;
      Stmt.Free;
    end;
  aParser.Free;
end;

end.

