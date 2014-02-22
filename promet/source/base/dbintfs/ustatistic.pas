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
  Classes, SysUtils, uBaseDbClasses, db, uBaseDbInterface,uIntfStrConsts,
  fpsqlparser,fpsqlscanner,fpsqltree;

type
  TOwnSQLParser = class(TSQLParser)
  end;
  TSQLExceptionEvent = procedure(e : Exception;aCol,aRow : Integer) of object;

  { TSQLStatemnt }

  TSQLStatemnt = class
  private
    FExcept: TSQLExceptionEvent;
    FFormated: string;
    FSQL: string;
    FStatememt: TSQLElementList;
    procedure SetSQL(AValue: string);
  public
    property SQL : string read FSQL write SetSQL;
    property FormatedSQL : string read FFormated;
    property Statements : TSQLElementList read FStatememt;
    property OnException : TSQLExceptionEvent read FExcept write FExcept;
    function Parse : Boolean;
    constructor Create;
    destructor Destroy; override;
  end;

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
uses uBaseApplication,uData;

{ TSQLStatement }

procedure TSQLStatemnt.SetSQL(AValue: string);
begin
  FSQL := AValue;
end;

function TSQLStatemnt.Parse: Boolean;
var
  FStatementList: TSQLElementList;
  i: Integer;
  aStmt: TSQLElement = nil;
  FExcludeKeywords: TStringList;
  FToken: TSQLToken;
  FSQLStream : TStringStream;
  FSQLParser : TOwnSQLParser;
  FSQLScanner : TSQLScanner;
  FStmtCount : Integer = 0;
  ErrVisible : Boolean = False;
  aOut : string = '';
  FTables: TStringList;

  function CheckStmtTables : Boolean;
  var
    aList: TSQLElementList;
    a: Integer;
    aName: TSQLStringType;
  begin
    Result := True;
    FTables.Clear;
    if aStmt is TSQLSelectStatement then
      begin
        aList := TSQLSelectStatement(aStmt).Tables;
        for a := 0 to aList.Count-1 do
          if aList[a] is TSQLSimpleTableReference then
            begin
              aName := TSQLSimpleTableReference(aList[a]).ObjectName.Name;
              FTables.Add(aName);
              if Assigned(Data) then
                if Data.Users.Rights.Right(aName)<RIGHT_READ then
                  Result := False;
            end;
      end;
  end;
begin
  Result := True;
  FSQLStream := TStringStream.Create(FSQL);
  FTables := TStringList.Create;
  FExcludeKeywords := TStringList.Create;
  FExcludeKeywords.Add('SUBSTRING');
  FExcludeKeywords.Add('REPLACE');
  FExcludeKeywords.Add('CHARINDEX');
  FExcludeKeywords.Add('CHARINDEX');
  FExcludeKeywords.Add('CHARINDEX');
  FSQLScanner := TSQLScanner.Create(FSQLStream);
  FSQLScanner.ExcludeKeywords := FExcludeKeywords;
  FToken := FSQLScanner.FetchToken;
  if trim(FSQLStream.DataString) <> '' then inc(FStmtCount);
  //Scan
  try
    while (FToken <> tsqlEOF) do
      begin
        if FToken = tsqlSEMICOLON then inc(FStmtCount);
        FToken := FSQLScanner.FetchToken;
      end;
  except
    on e : Exception do
      begin
        if Assigned(OnException) then
          OnException(e,FSQLScanner.CurColumn,FSQLScanner.CurRow);
        Result := False;
      end;
  end;
  FSQLScanner.Free;
  //Parse
  FSQLStream.Position:=0;
  FSQLScanner := TSQLScanner.Create(FSQLStream);
  FSQLScanner.ExcludeKeywords := FExcludeKeywords;
  FSQLParser := TOwnSQLParser.Create(FSQLScanner);
  try
    while (FStmtCount>0) and (FSQLScanner.CurToken <> tsqlEOF) do
      begin
        aStmt := FSQLParser.Parse;
        FStatememt.Add(aStmt);
        dec(FStmtCount);
        if not CheckStmtTables then
          begin
            if Assigned(OnException) then
              begin
                OnException(Exception.Create('Table Access not allowed or Table not found'),FSQLScanner.CurColumn,FSQLScanner.CurRow);
              end;
          end;
      end;
  except
    on e : Exception do
      begin
        if Assigned(OnException) then
          OnException(e,FSQLScanner.CurColumn,FSQLScanner.CurRow);
        Result := False;
      end;
  end;
  FreeAndNil(FSQLParser);
  FreeAndNil(FSQLScanner);
  FreeAndNil(FSQLStream);
end;

constructor TSQLStatemnt.Create;
begin
  FStatememt := TSQLElementList.create(true);
end;

destructor TSQLStatemnt.Destroy;
begin
  FStatememt.Free;
  inherited Destroy;
end;

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
            begin
              aState:=ST_NEXTCHAR;
              bQuerry:=bQuerry+'@'+aName+copy(aQuerry,0,1)[1];
              aName := '';
            end
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
  result := StringReplace(Result,'@USERID@',TBaseDBModule(DataModule).Users.Id.AsString,[rfReplaceAll]);
end;

end.
