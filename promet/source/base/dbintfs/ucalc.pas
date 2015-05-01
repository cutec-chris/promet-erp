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
  MathParser,Utils,uBaseDatasetInterfaces;
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
    constructor CreateEx(aOwner : TComponent;DM : TComponent=nil;aConnection : TComponent = nil;aMasterdata : TDataSet = nil);override;
    destructor Destroy; override;
    function CreateTable: Boolean; override;
    procedure DefineFields(aDataSet: TDataSet); override;
    property Variables : TCalcVariables read FVariables;
    procedure FillDefaults(aDataSet: TDataSet); override;
    property Typ : string read FType write Settype;
    function Calculate(aIn: string; aOut: TStrings; var aData: TDataSet;
      var aValue: Extended): Boolean;
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

function ConvertDzehn(LeftArg, RightArg: Extended;
  AssignedArguments: TMPArguments; var Error: TMPError;
  var ErrorDescription: string; var InvalidArguments: TMPArguments): Extended;
begin
  if (AssignedArguments=[]) then Begin
    InvalidArguments:=[];
    Error:=mpeMissingArgument;
    ErrorDescription:='left argument is missing';
  end else result:=LeftArg/10;
end;

function ConvertDTausend(LeftArg, RightArg: Extended;
  AssignedArguments: TMPArguments; var Error: TMPError;
  var ErrorDescription: string; var InvalidArguments: TMPArguments): Extended;
begin
  if (AssignedArguments=[]) then Begin
    InvalidArguments:=[];
    Error:=mpeMissingArgument;
    ErrorDescription:='left argument is missing';
  end else result:=LeftArg/1000;
end;

function ConvertDHundert(LeftArg, RightArg: Extended;
  AssignedArguments: TMPArguments; var Error: TMPError;
  var ErrorDescription: string; var InvalidArguments: TMPArguments): Extended;
begin
  if (AssignedArguments=[]) then Begin
    InvalidArguments:=[];
    Error:=mpeMissingArgument;
    ErrorDescription:='left argument is missing';
  end else result:=LeftArg/100;
end;

function ConvertMillion(LeftArg, RightArg: Extended;
  AssignedArguments: TMPArguments; var Error: TMPError;
  var ErrorDescription: string; var InvalidArguments: TMPArguments): Extended;
begin
  if (AssignedArguments=[]) then Begin
    InvalidArguments:=[];
    Error:=mpeMissingArgument;
    ErrorDescription:='left argument is missing';
  end else result:=LeftArg*1000000;
end;

function ConvertMilliarde(LeftArg, RightArg: Extended;
  AssignedArguments: TMPArguments; var Error: TMPError;
  var ErrorDescription: string; var InvalidArguments: TMPArguments): Extended;
begin
  if (AssignedArguments=[]) then Begin
    InvalidArguments:=[];
    Error:=mpeMissingArgument;
    ErrorDescription:='left argument is missing';
  end else result:=LeftArg*1000000000;
end;

function ConvertBillion(LeftArg, RightArg: Extended;
  AssignedArguments: TMPArguments; var Error: TMPError;
  var ErrorDescription: string; var InvalidArguments: TMPArguments): Extended;
begin
  if (AssignedArguments=[]) then Begin
    InvalidArguments:=[];
    Error:=mpeMissingArgument;
    ErrorDescription:='left argument is missing';
  end else result:=LeftArg*1000000000000;
end;

function ConvertBilliarde(LeftArg, RightArg: Extended;
  AssignedArguments: TMPArguments; var Error: TMPError;
  var ErrorDescription: string; var InvalidArguments: TMPArguments): Extended;
begin
  if (AssignedArguments=[]) then Begin
    InvalidArguments:=[];
    Error:=mpeMissingArgument;
    ErrorDescription:='left argument is missing';
  end else result:=LeftArg*1000000000000000;
end;

function ConvertTrillion(LeftArg, RightArg: Extended;
  AssignedArguments: TMPArguments; var Error: TMPError;
  var ErrorDescription: string; var InvalidArguments: TMPArguments): Extended;
begin
  if (AssignedArguments=[]) then Begin
    InvalidArguments:=[];
    Error:=mpeMissingArgument;
    ErrorDescription:='left argument is missing';
  end else result:=LeftArg*1000000000000000000;
end;

function ConvertTrilliarde(LeftArg, RightArg: Extended;
  AssignedArguments: TMPArguments; var Error: TMPError;
  var ErrorDescription: string; var InvalidArguments: TMPArguments): Extended;
begin
  if (AssignedArguments=[]) then Begin
    InvalidArguments:=[];
    Error:=mpeMissingArgument;
    ErrorDescription:='left argument is missing';
  end else result:=LeftArg*1000000000000000000000;
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

constructor TCalcEnviroments.CreateEx(aOwner: TComponent; DM: TComponent;
  aConnection: TComponent; aMasterdata: TDataSet);
begin
  inherited CreateEx(aOwner, DM, aConnection, aMasterdata);
  FVariables := TCalcVariables.CreateEx(Owner,DM,aConnection,DataSet);
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

function TCalcEnviroments.Calculate(aIn: string; aOut: TStrings;var aData : TDataSet;var aValue : Extended): Boolean;
var
  aParser: TMathParser;
  aTree: PTTermTreeNode;
  Stmt: TSQLStatemnt;
  aVar: String = '';
  bOut: String;
  aDS: TDataSet;
  aFStart: String;
  aTmp2: String;
  aTmp: String;
  toCalc: String;
  aSQL: string;
  field: Integer;
  aRec: Integer;
begin
  aData:=nil;
  aValue:=0;
  Result := True;
  if copy(aIn,0,2)='--' then
    begin
      aOut.Add(aIn);
      exit;
    end;
  if lowercase(copy(aIn,0,7))='delete ' then
    begin
      aIn := copy(aIn,8,length(aIn));
      with Variables do
        begin
          Active:=True;
          First;
          while not EOF do
            begin
              aFStart := Variables.FieldByName('NAME').AsString;
              if pos(aFStart,aIn)>0 then
                begin
                  aOut.Add(Variables.FieldByName('NAME').AsString+' deleted');
                  Variables.Delete;
                  Result := True;
                end
              else
                Next;
            end;
        end;
      exit;
    end;
  if (Pos('=',aIn)>0) and ((pos('select',lowercase(aIn))>Pos('=',aIn)) or (pos('select',lowercase(aIn))=0))
  then
    begin
      aVar := copy(aIn,0,Pos('=',aIn)-1);
      aVar := StringReplace(aVar,#10,'',[rfReplaceAll]);
      aVar := trim(StringReplace(aVar,#13,'',[rfReplaceAll]));
      aVar := StringReplace(aVar,'.','_',[rfReplaceAll]);
      aIn := copy(aIn,Pos('=',aIn)+1,length(aIn));
    end;
  if pos('(',aVar)>0 then
    begin
      if Variables.Locate('NAME',aVar,[]) then
        Variables.Edit
      else Variables.Insert;
      Variables.FieldByName('NAME').AsString:=aVar;
      Variables.FieldByName('FORMULA').AsString:=aIn;
      Variables.FieldByName('RESULT').AsFloat:=aParser.CalcTree(aTree);
      Variables.Post;
      aOut.Add(aVar+'='+aIn+' saved');
      Result := True;
      exit;
    end;
  aParser := TMathParser.Create;
  aParser.AddOperatorEx('milli',@ConvertDTausend,[mpaLeft],MPOP_OPERATOR_LEFTONLY);
  aParser.AddOperatorEx('zenti',@ConvertDHundert,[mpaLeft],MPOP_OPERATOR_LEFTONLY);
  aParser.AddOperatorEx('dezi',@ConvertDzehn,[mpaLeft],MPOP_OPERATOR_LEFTONLY);
  aParser.AddOperatorEx('kilo',@ConvertTausend,[mpaLeft],MPOP_OPERATOR_LEFTONLY);
  aParser.AddOperatorEx('hundert',@ConvertHundert,[mpaLeft],MPOP_OPERATOR_LEFTONLY);
  aParser.AddOperatorEx('tausend',@ConvertTausend,[mpaLeft],MPOP_OPERATOR_LEFTONLY);
  aParser.AddOperatorEx('million',@ConvertMillion,[mpaLeft],MPOP_OPERATOR_LEFTONLY);
  aParser.AddOperatorEx('millionen',@ConvertMillion,[mpaLeft],MPOP_OPERATOR_LEFTONLY);
  aParser.AddOperatorEx('milliarde',@ConvertMilliarde,[mpaLeft],MPOP_OPERATOR_LEFTONLY);
  aParser.AddOperatorEx('milliarden',@ConvertMilliarde,[mpaLeft],MPOP_OPERATOR_LEFTONLY);
  aParser.AddOperatorEx('billion',@ConvertBillion,[mpaLeft],MPOP_OPERATOR_LEFTONLY);
  aParser.AddOperatorEx('billionen',@ConvertBillion,[mpaLeft],MPOP_OPERATOR_LEFTONLY);
  aParser.AddOperatorEx('billiarde',@ConvertBilliarde,[mpaLeft],MPOP_OPERATOR_LEFTONLY);
  aParser.AddOperatorEx('billiarden',@ConvertBilliarde,[mpaLeft],MPOP_OPERATOR_LEFTONLY);
  aParser.AddOperatorEx('trillion',@ConvertTrillion,[mpaLeft],MPOP_OPERATOR_LEFTONLY);
  aParser.AddOperatorEx('trillionen',@ConvertTrillion,[mpaLeft],MPOP_OPERATOR_LEFTONLY);
  aParser.AddOperatorEx('trilliarde',@ConvertTrilliarde,[mpaLeft],MPOP_OPERATOR_LEFTONLY);
  aParser.AddOperatorEx('trilliarden',@ConvertTrilliarde,[mpaLeft],MPOP_OPERATOR_LEFTONLY);
  with Variables do
    begin
      Active:=True;
      First;
      while not EOF do
        begin
          aParser.AddConstant(Variables.FieldByName('NAME').AsString,Variables.FieldByName('RESULT').AsFloat);
          aFStart := copy(Variables.FieldByName('NAME').AsString,0,pos('(',Variables.FieldByName('NAME').AsString));
          while pos(aFStart,aIn)>0 do
            begin
              aTmp := aIn;
              aTmp2 := copy(Variables.FieldByName('NAME').AsString,length(aFStart)+1,length(Variables.FieldByName('NAME').AsString));
              aTmp2 := StringReplace(aTmp2,')',',',[rfReplaceAll]);
              aIn := copy(aTmp,0,pos(aFStart,aTmp)-1);
              aTmp := copy(aTmp,pos(aFStart,aTmp)+length(aFStart),length(aTmp));
              bOut := copy(atmp,pos(')',aTmp)+1,length(aTmp));
              aTmp := copy(aTmp,0,pos(')',aTmp));
              aTmp := StringReplace(aTmp,')',',',[rfReplaceAll]);
              toCalc := Variables.FieldByName('FORMULA').AsString;
              while pos(',',aTmp2)>0 do
                begin
                  toCalc := StringReplace(toCalc,'@'+copy(aTmp2,0,pos(',',aTmp2)-1)+'@',copy(aTmp,0,pos(',',aTmp)-1),[rfReplaceAll]);
                  aTmp2 := copy(aTmp2,pos(',',aTmp2)+1,length(aTmp2));
                  aTmp :=  copy(aTmp ,pos(',',aTmp )+1,length(aTmp ));
                end;
              if not Calculate(toCalc,aOut,aData,aValue) then
                begin
                  Result := False;
                  exit;
                end
              else bOut := StringReplace(FloatToStr(aValue),DecimalSeparator,'.',[]);
              aIn+=bOut;
            end;
          Next;
        end;
    end;
  try
    aTree := aParser.ParseTerm(aIn);
  except
    on e : Exception do
      begin
        aTree := nil;
        if trim(e.Message)<>'' then
          aOut.add(trim(e.Message)+' ('+aIn+')')
        else aOut.add(e.ClassName+' ('+aIn+')');
        Result:=False;
      end;
  end;
  if Assigned(aTree) then
    begin
      try
        aOut.Add(aParser.FormatTerm(aTree));
        aValue := aParser.CalcTree(aTree);
        bOut := StringReplace(FloatToStr(aValue),DecimalSeparator,'.',[]);
        if aVar <> '' then
          begin
            if Variables.Locate('NAME',aVar,[]) then
              Variables.Edit
            else Variables.Insert;
            Variables.FieldByName('NAME').AsString:=aVar;
            Variables.FieldByName('FORMULA').AsString:=aIn;
            Variables.FieldByName('RESULT').AsFloat:=aParser.CalcTree(aTree);
            Variables.Post;
            aOut.Add('='+aVar);
          end;
      except
        on e : EMathParserException do
          begin
            aTmp := e.ErrorDescription;
            aOut.Add(aTmp);
            Result:=False;
          end;
      end;
    end
  else if (pos('select',lowercase(aIn))>0) then
    begin
      Result := True;
      Stmt := TSQLStatemnt.Create;
      Stmt.SQL:=aIn;
      try
        aOut.Clear;
        aDS := Stmt.GetDataSet(aSQL);
        aOut.Add(aSQL);
        if Assigned(aDS) then
          begin
            if (aVar <> '') and (aDs.Fields.Count=1) and (aDS.RecordCount=1) and (TryStrToFloat(aDS.Fields[0].AsString,aValue)) then
              begin
                aValue := aDS.Fields[0].AsFloat;
                bOut := StringReplace(FloatToStr(aValue),DecimalSeparator,'.',[]);
                if Variables.Locate('NAME',aVar,[]) then
                  Variables.Edit
                else Variables.Insert;
                Variables.FieldByName('NAME').AsString:=aVar;
                Variables.FieldByName('FORMULA').AsString:=aIn;
                Variables.FieldByName('RESULT').AsFloat:=aDS.Fields[0].AsFloat;
                Variables.Post;
                aOut.Add('='+aVar+'='+bOut);
              end
            else
              begin
                if  (aDs.Fields.Count=1)
                and (aDS.RecordCount=1)
                and (TryStrToFloat(aDS.Fields[0].AsString,aValue)) then
                  begin
                    aValue := aDS.Fields[0].AsFloat;
                    bOut := StringReplace(FloatToStr(aValue),DecimalSeparator,'.',[]);
                  end
                else
                  begin
                    atmp := '';
                    for field := 0 to aDs.Fields.Count-1 do
                      atmp += aDs.FieldDefs[field].Name+#9;
                    aOut.Add(atmp);
                    atmp := '';
                    aRec := 0;
                    while not aDS.EOF do
                      begin
                        for field := 0 to aDs.Fields.Count-1 do
                          atmp += aDs.Fields[field].AsString+#9;
                        aOut.Add(atmp);
                        inc(aRec);
                        if arec > 0 then
                          begin
                            aOut.Add('... '+IntToStr(aDS.RecordCount-5)+' records follows');
                            break;
                          end;
                        aDs.Next;
                      end;

                  end;
              end;
            if Assigned(aDs) then aData := aDS;
          end;
      except
        on e : Exception do
          begin
            aOut.Add(e.Message);
            Result:=False;
          end;
      end;
      Stmt.Free;
    end;
  aParser.Free;
end;

end.

