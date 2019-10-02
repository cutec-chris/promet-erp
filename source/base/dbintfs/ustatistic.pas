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
  sqlparser,sqlscanner,sqltree,Utils,jsonparser,fpjson,
  memds,uBaseDatasetInterfaces;

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
    FTables: TStringList;
    FStatememt: TSQLElementList;
    FParseException : string;
    Data : TBaseDBModule;
    procedure SetSQL(AValue: string);
  public
    property SQL : string read FSQL write SetSQL;
    property FormatedSQL : string read FFormated;
    property Statements : TSQLElementList read FStatememt;
    property OnException : TSQLExceptionEvent read FExcept write FExcept;
    function Parse : Boolean;
    function BuildFormatedOuput(aStatement : TSQLStatement) : string;
    function GetDataSet(var aSQL: string): TDataSet;
    constructor Create;
    destructor Destroy; override;
  end;

  { TStatistic }

  TStatistic = class(TBaseDbList,IBaseHistory)
    procedure FDSDataChange(Sender: TObject; Field: TField);
  private
    FHistory: TBaseHistory;
    FInternalScript : string;
    FStateChange: TNotifyEvent;
    FStatus : string;
    FDS : TDataSource;
    procedure aScriptWriteln(const s: string);
    function GetHistory: TBaseHistory;
  protected
    function GetTextFieldName: string;override;
    function GetStatusFieldName: string; override;
    function GetNumberFieldName : string;override;
    function GetDescriptionFieldName: string; override;
  public
    procedure Open; override;
    procedure DefineFields(aDataSet : TDataSet);override;
    function GetTyp: string; override;
    constructor CreateEx(aOwner: TComponent; DM: TComponent;
       aConnection: TComponent=nil; aMasterdata: TDataSet=nil); override;
    destructor Destroy; override;
    function BuildQuerry(aVariables : TStrings) : string;
    function BuildSQL(aSQL : string) : string;
    property History : TBaseHistory read FHistory;
    property OnStateChange : TNotifyEvent read FStateChange write FStateChange;
  end;

  function ReplaceSQLFunctions(Str : string) : string;
  function AddSQLLimit(Str : string;aLimit : Integer;DM : TBaseDBModule = nil) : string;

implementation
uses RegExpr;
resourcestring
  strYQLFail                = 'YQL Abfrage fehlgeschlagen:';
{ TSQLStatement }


function ReplaceSQLFunctions(DBType,Str : string) : string;
var
  aLmt: String;
  tmp: String;
  tmp1: String;
  inKl: Integer;
begin
  Result := Str;
  if DBType='postgres' then
    begin
      Result := StringReplace(Result,'JULIANDAY(','cast(''17 may 1970'' as timestamp)+(',[rfReplaceAll,rfIgnoreCase]);
      while pos('CHARINDEX(',Uppercase(Result))>0 do
        begin
          tmp := Result;
          Result := copy(tmp,0,pos('CHARINDEX(',Uppercase(Result))-1);
          Result := Result+'position(';
          tmp := copy(tmp,pos('CHARINDEX(',Uppercase(tmp))+10,length(tmp));
          while ((copy(tmp,0,1) <> ',') or (inKl > 0)) do
            begin
              if copy(tmp,0,1)='(' then inc(inKl);
              if copy(tmp,0,1)=')' then dec(inKl);
              Result := Result+copy(tmp,0,1);
              tmp := copy(tmp,2,length(tmp));
            end;
          tmp := copy(tmp,pos(',',tmp)+1,length(tmp));
          Result := Result+' in ';
          inKl := 0;
          while (copy(tmp,0,1) <> ')') or (inKl > 0) do
            begin
              if copy(tmp,0,1)='(' then inc(inKl);
              if copy(tmp,0,1)=')' then dec(inKl);
              Result := Result+copy(tmp,0,1);
              tmp := copy(tmp,2,length(tmp));
            end;
          Result := Result+tmp;
        end;
      Result := StringReplace(Result,'CHARINDEX(','position(',[rfReplaceAll,rfIgnoreCase]);
      Result := StringReplace(Result,'MONTH(','EXTRACT(MONTH FROM ',[rfReplaceAll,rfIgnoreCase]);
      Result := StringReplace(Result,'YEAR(','EXTRACT(YEAR FROM ',[rfReplaceAll,rfIgnoreCase]);
      Result := StringReplace(Result,'DAY(','EXTRACT(DAY FROM ',[rfReplaceAll,rfIgnoreCase]);
      Result := StringReplace(Result,'DATEPART(HOUR,','EXTRACT(HOUR FROM ',[rfReplaceAll,rfIgnoreCase]);
      Result := StringReplace(Result,'DATEPART(MINUTE,','EXTRACT(MINUTE FROM ',[rfReplaceAll,rfIgnoreCase]);
      Result := StringReplace(Result,'DATEPART(SECOND,','EXTRACT(SECOND FROM ',[rfReplaceAll,rfIgnoreCase]);
      Result := StringReplace(Result,'DAYOFWEEK(','EXTRACT(DOW FROM ',[rfReplaceAll,rfIgnoreCase]);
      Result := StringReplace(Result,'GETDATE()','CURRENT_DATE',[rfReplaceAll,rfIgnoreCase]);
      Result := StringReplace(Result,'LENGTH(','LEN(',[rfReplaceAll,rfIgnoreCase]);
      Result := StringReplace(Result,'LEN(','CHAR_LENGTH(',[rfReplaceAll,rfIgnoreCase]);
      Result := StringReplace(Result,'varchar(max)','text',[rfReplaceAll,rfIgnoreCase]);
      Result := StringReplace(Result,'(max)','',[rfReplaceAll,rfIgnoreCase]);
      Result := StringReplace(Result,'IFNULL(','coalesce(',[rfReplaceAll,rfIgnoreCase]);
      Result := StringReplace(Result,'ISNULL(','coalesce(',[rfReplaceAll,rfIgnoreCase]);
    end
  else if DBType='mssql' then
    begin
      Result := StringReplace(Result,'LENGTH(','LEN(',[rfReplaceAll,rfIgnoreCase]);
      Result := StringReplace(Result,'JULIANDAY(','2415020.5+CONVERT(FLOAT,',[rfReplaceAll,rfIgnoreCase]);
      Result := StringReplace(Result,'IFNULL(','ISNULL(',[rfReplaceAll,rfIgnoreCase]);
      if pos('limit ',lowercase(Str))>0 then
        begin
          aLmt := copy(Str,pos('limit ',lowercase(Str))+6,length(Str));
          if pos(' ',aLmt)>0 then
            aLmt := copy(aLmt,0,pos(' ',aLmt));
          if pos(';',aLmt)>0 then
            aLmt := copy(aLmt,0,pos(';',aLmt));
          aLmt :=  trim(aLmt);
          Result := copy(Result,0,pos('limit ',lowercase(Result))-1);
          Result := StringReplace(Result,'select ','select top '+aLmt+' ',[rfReplaceAll,rfIgnoreCase]);
        end;
    end
  else if DBType='sqlite' then
    begin
      Result := StringReplace(Result,'(max)','',[rfReplaceAll,rfIgnoreCase]);
      Result := StringReplace(Result,'LEN(','LENGTH(',[rfReplaceAll,rfIgnoreCase]);
      Result := StringReplace(Result,'CHARINDEX(','instr(',[rfReplaceAll,rfIgnoreCase]);
      Result := StringReplace(Result,'MONTH(','strftime("%m",',[rfReplaceAll,rfIgnoreCase]);
      Result := StringReplace(Result,'YEAR(','strftime("%Y",',[rfReplaceAll,rfIgnoreCase]);
      Result := StringReplace(Result,' DAY(','strftime("%d",',[rfReplaceAll,rfIgnoreCase]);
      Result := StringReplace(Result,',DAY(','strftime("%d",',[rfReplaceAll,rfIgnoreCase]);
      Result := StringReplace(Result,'DATEPART(HOUR,','strftime("%h",',[rfReplaceAll,rfIgnoreCase]);
      Result := StringReplace(Result,'DATEPART(MINUTE,','strftime("%m",',[rfReplaceAll,rfIgnoreCase]);
      Result := StringReplace(Result,'DATEPART(SECOND,','strftime("%f",',[rfReplaceAll,rfIgnoreCase]);
      Result := StringReplace(Result,'DAYOFWEEK(','strftime("%w",',[rfReplaceAll,rfIgnoreCase]);
      Result := StringReplace(Result,'GETDATE()','date("now")',[rfReplaceAll,rfIgnoreCase]);
      Result := StringReplace(Result,'SUBSTRING(','substr(',[rfReplaceAll,rfIgnoreCase]);
    end;
end;

function AddSQLLimit(Str: string; aLimit: Integer;DM : TBaseDBModule = nil): string;
begin
  if DM.LimitAfterSelect then
    Result := StringReplace(Str,'select','SELECT '+Format(DM.LimitSTMT,[IntToStr(aLimit)]),[rfReplaceAll,rfIgnoreCase])
  else
    Result := StringReplace(Str,';','',[rfReplaceAll])+' '+Format(DM.LimitSTMT,[IntToStr(aLimit)]);
end;

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
  aSQL: String;
  yqlQ: Boolean;

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
                if (Data.Users.Rights.Right(aName)>-1) and (Data.Users.Rights.Right(aName)<RIGHT_READ) then
                  Result := False
                else if (Data.Users.Rights.Right(aName)=-1) then
                  begin //try to check yql and local file selects

                  end;
            end;
      end;
  end;
begin
  FParseException:='';
  Result := True;
  aSQL:=FSQL;
  if copy(lowercase(trim(aSQL)),0,4)='yql ' then
    begin
      yqlQ := True;
      aSQL:=copy(trim(aSQL),5,length(aSQL));
    end;
  FSQLStream := TStringStream.Create(aSQL);
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
        FParseException:=e.Message;
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
  FFormated:='';
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
                OnException(Exception.Create('Table Access not allowed'),FSQLScanner.CurColumn,FSQLScanner.CurRow);
              end;
          end
        else FFormated := FFormated+BuildFormatedOuput(TSQLStatement(aStmt));
      end;
  except
    on e : Exception do
      begin
        FParseException:=e.Message;
        if Assigned(OnException) then
          OnException(e,FSQLScanner.CurColumn,FSQLScanner.CurRow);
        Result := False;
      end;
  end;
  FreeAndNil(FSQLParser);
  FreeAndNil(FSQLScanner);
  FreeAndNil(FSQLStream);
end;

function TSQLStatemnt.BuildFormatedOuput(aStatement: TSQLStatement): string;
var
  Options: TSQLFormatOptions;
  NewLinePending: Boolean;
  Procedure AddList(Const AKeyWord : String; List : TSQLElementList; UseNewLine,UseIndent : boolean);

  Var
    S,Pref,Sep : TSQLStringType;
    I,Ind : Integer;
    aElem: TSQLElement;
    aXPr: TSQLExpression;

  begin
    S:='';
    if Not Assigned(List) or (List.Count=0) then
      exit;
    If (AkeyWord<>'') then
      If NewlinePending then
        Result:=Result+sLinebreak+SQLKeyWord(AKeyWord,Options)
      else
        Result:=Result+' '+SQLKeyWord(AKeyWord,Options);
    GetSepPrefixIndent(UseNewLine,UseIndent,Sep,Pref,Ind);
    For I:=0 to List.Count-1 do
      begin
        aElem := List[I];
        if (aElem is TSQLSelectField)
        then
          begin //Fields are always upper cased in promet
            If (S<>'') then
              S:=S+Sep;
            if (pos('''',TSQLSelectField(aElem).Expression.GetAsSQL([]))=0)
            and (pos('(',TSQLSelectField(aElem).Expression.GetAsSQL([]))=0)
            and (not Assigned(TSQLSelectField(aElem).AliasName))
            then
              S:=S+Pref+Data.QuoteField(aElem.GetAsSQL([],2+Ind))
            else
              S:=S+Pref+aElem.GetAsSQL(Options,2+Ind);
          end
        else if (aElem is TSQLSimpleTableReference) then
          begin //Tables are always upper cased in promet
            If (S<>'') then
              S:=S+Sep;
            if (pos('''',TSQLSimpleTableReference(aElem).ObjectName.Name)=0)
            and (not Assigned(TSQLSimpleTableReference(aElem).AliasName))
            then
              S:=S+Pref+Data.QuoteField(aElem.GetAsSQL([],2+Ind))
            else
              S:=S+Pref+aElem.GetAsSQL(Options,2+Ind);
          end
        else if (aElem is TSQLExpression)
             or (aElem is TSQLJoinTableReference)
        then
          begin
            S:=S+Pref+aElem.GetAsSQL(Options,2+Ind);
          end
        else if (aElem is TSQLStatement) then
          begin
            If (S<>'') then
              S:=S+Sep;
            S := S+Pref+BuildFormatedOuput(TSQLStatement(aElem));
          end
        else if (aElem is TSQLElement) then
          begin
            If (S<>'') then
              S:=S+Sep;
            S := S+Pref+'*';
          end;
      end;
    NewLinePending:=UseNewLine;
    If UseNewline then
      Result:=Result+sLinebreak+S
    else
      Result:=Result+' '+S;
  end;

  Procedure AddExpression(Const AKeyWord : TSQLStringType;AExpression  : TSQLElement; UseNewLine,UseIndent : boolean);

  Var
    S,Pref,Sep : TSQLStringType;
    Ind : Integer;

  begin
    If Not Assigned(AExpression) then
      Exit;
    If NewlinePending then
      S:=slineBreak
    else
      S:=' ';
    Result:=Result+S;
    If UseNewline then
      S:=slineBreak
    else
      S:=' ';
    Result:=Result+SQLKeyWord(AKeyword,Options)+S;
    GetSepPrefixIndent(UseNewLine,UseIndent,Sep,Pref,Ind);
    Result:=Result+Pref+AExpression.GetAsSQL(Options,0{AIndent+Ind});
    NewLinePending:=UseNewLine;
  end;
begin
  Result := '';
  Options := [sfoDoubleQuoteIdentifier];
  try
    if aStatement is TSQLSelectStatement then
      begin
        with aStatement as TSQLSelectStatement do
          begin
            Result:=SQLKeyWord('SELECT',Options);
            If Distinct then
              Result:=Result+' '+SQLKeyword('DISTINCT',Options);
            NewLinePending:=(sfoOneFieldPerLine in Options);
            AddList('',Fields,(sfoOneFieldPerLine in Options),(sfoIndentFields in Options));
            AddList('FROM',Tables,(sfoOneTablePerLine in Options),(sfoIndentTables in Options));
            AddExpression('WHERE',Where,(sfoWhereOnSeparateLine in Options),(sfoIndentWhere in Options));
            AddList('GROUP BY',GroupBy,(sfoOneGroupByFieldPerLine in Options),(sfoIndentGroupByFields in Options));
            AddExpression('HAVING',Having,(sfoHavingOnSeparateLine in Options),(sfoIndentHaving in Options));
            If Assigned(Union) then
              NewLinePending:=NewLinePending or (sfoUnionOnSeparateLine in Options);
            AddExpression('UNION',Union,(sfoUnionOnSeparateLine in Options),False);
            If Assigned(Plan) then
              NewLinePending:=NewLinePending or (sfoPlanOnSeparateLine in Options);
            AddExpression('PLAN',Plan,(sfoPlanOnSeparateLine in Options),(sfoIndentPlan in Options));
            AddList('ORDER BY',OrderBy,(sfoOneOrderByFieldPerLine in Options),(sfoIndentOrderByFields in Options));
          end;
      end
    else if Assigned(aStatement) then
      Result := aStatement.GetAsSQL(Options);
  except
  end;
end;

function TSQLStatemnt.GetDataSet(var aSQL : string): TDataSet;
var
  eMsg: String = 'not enougth rights to access these tables';
  //http: THTTPSend;
  yqlQ: Boolean = false;
  aParser: TJSONParser;
  aData: TJSONData;
  i: Integer;
  aItem: TJSONObject;
  aTable: TJSONArray;
  aObj: TJSONObject;
  aDat: TJSONData;
  FieldSizes : array of Integer;
  a: Integer;
begin
  Result := nil;
  aSQL := FSQL;
  {
  if copy(lowercase(trim(aSQL)),0,4)='yql ' then
    begin
      yqlQ := True;
      aSQL:=copy(trim(aSQL),5,length(aSQL));
    end;
  }
  if Parse and (not TBaseDBModule(Data).CheckForInjection(SQL)) then
    begin
      Result := TBaseDBModule(Data).GetNewDataSet(FormatedSQL);
      aSQL := FormatedSQL;
    end
  else if (TBaseDBModule(Data).Users.Rights.Right('STATISTIC')>=RIGHT_READ)
       and (not TBaseDBModule(Data).CheckForInjection(SQL))
  then
    begin
      Result := TBaseDBModule(Data).GetNewDataSet(SQL);
      aSQL := SQL;
    end
  else if FParseException<>'' then eMsg:=FParseException;
  if Assigned(Result) then
    begin
      try
        Result.Open;
      except
        on e : Exception do
          begin
            eMsg := e.Message;
            FreeAndNil(Result);
          end;
      end;
    end;
  if not Assigned(Result) then
    begin
      if (FTables.Count=1) and (aSQL = FormatedSQL) and FileExists(FTables[0]) then
        begin //local file
          eMsg:='not implemented';
        end
      {else if ((FTables.Count<2) and yqlQ) then//yql??
        begin
          //http://query.yahooapis.com/v1/public/yql?q=select%20*%20from%20html%20where%20url%3D%27http%3A%2F%2Fmashable.com%27
          http := THTTPSend.Create;
          http.UserAgent:='Mozilla/5.0 (Windows NT 5.1; rv:6.0.2)';
          http.HTTPMethod('GET','https://query.yahooapis.com/v1/public/yql?q='+HTTPEncode(aSQL)+'&format=json');
          if http.ResultCode=200 then
            begin
              http.Document.SaveToFile('document.json');
              http.Document.Position:=0;
              aParser := TJSONParser.Create(http.Document);
              aData := aParser.Parse;
              aParser.Free;
              if aData.Count=1 then
                begin
                  aItem := TJSONObject(aData.Items[0]);
                  aTable := TJSONArray(aItem.Elements['results']);
                  Result := TMemDataset.Create(BaseApplication);
                  if aTable.Count>0 then
                    begin
                      aDat := aTable.Items[0];
                      if aDat is TJSONArray then
                        begin
                          aTable := TJsonArray(aDat);
                          if aTable.Count>0 then
                            aDat := aTable.Items[0];
                        end;
                      if aDat is TJSONObject then
                        begin
                          aObj := aTable.Items[0] as TJSONObject;
                          for I := 0 to Pred(aObj.Count) do
                            Result.FieldDefs.Add(Uppercase(aObj.Names[I]),ftString,500);
                          setlength(FieldSizes,aObj.Count);
                          for i := low(FieldSizes) to High(FieldSizes) do
                            FieldSizes[i] := 0;
                          TMemDataset(Result).CreateTable;
                          Result.Open;
                          for i := 0 to aTable.Count-1 do
                            begin
                              aObj := TJSONObject(aTable.Items[i]);
                              Result.Append;
                              JSONToFields(aObj,Result.Fields,True);
                              for a := 0 to Pred(aObj.Count) do
                                if length(Result.Fields[a].AsString)>FieldSizes[a] then
                                  FieldSizes[a] := length(Result.Fields[a].AsString);
                              Result.Post;
                              aObj := nil;
                            end;
                          for I := 0 to Result.FieldDefs.Count-1 do
                            Result.FieldDefs[i].Size:=FieldSizes[i];
                          Result.Close;
                          TMemDataset(Result).CreateTable;
                          Result.Open;
                          for i := 0 to aTable.Count-1 do
                            begin
                              aObj := TJSONObject(aTable.Items[i]);
                              Result.Append;
                              JSONToFields(aObj,Result.Fields,True);
                              Result.Post;
                              aObj := nil;
                            end;
                          Result.First;
                        end;
                    end;
                end;
            end
          else eMsg:=strYQLFail+http.ResultString;
          http.Free;
        end};
    end;
  if not Assigned(Result) then
    raise Exception.Create(eMsg);
end;

constructor TSQLStatemnt.Create;
begin
  FStatememt := TSQLElementList.create(true);
  FTables := TStringList.Create;
end;

destructor TSQLStatemnt.Destroy;
begin
  FTables.Free;
  FStatememt.Free;
  inherited Destroy;
end;

procedure TStatistic.FDSDataChange(Sender: TObject; Field: TField);
begin
  if not Assigned(Field) then exit;
  if DataSet.ControlsDisabled then exit;
  if Field.FieldName = 'STATUS' then
    begin
      History.Open;
      History.AddItem(Self.DataSet,Format(strStatusChanged,[FStatus,Field.AsString]),'','',nil,ACICON_STATUSCH);
      FStatus := Field.AsString;
      if Assigned(FStateChange) then
        FStateChange(Self);
    end;
end;

procedure TStatistic.aScriptWriteln(const s: string);
begin
  FInternalScript := FInternalScript+s+#10;
end;

function TStatistic.GetHistory: TBaseHistory;
begin
  Result := FHistory;
end;

function TStatistic.GetTextFieldName: string;
begin
  Result := 'NAME';
end;

function TStatistic.GetStatusFieldName: string;
begin
  Result:='STATUS';
end;

procedure TStatistic.Open;
var
  i: Integer;
begin
  inherited Open;
  if not Active then exit;
  if Assigned(Status) then
    FStatus:=Status.AsString;
end;

function TStatistic.GetNumberFieldName: string;
begin
  Result := 'SQL_ID';
end;

function TStatistic.GetDescriptionFieldName: string;
begin
  Result:='DESC';
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
            Add('DESC',ftMemo,0,False);
            Add('STATUS',ftString,4,false);
            Add('QUERRY',ftMemo,0,False);
            Add('DETAIL',ftMemo,0,False);
            Add('SUBDETAIL',ftMemo,0,False);
            Add('STATFIELD',ftString,20,False);
            Add('ISSCRIPT',ftString,1,False);
            Add('QUERRYBLD',ftBlob,0,False);
            Add('STATNFIELD',ftString,20,False);
            Add('CHARTTYPE',ftString,1,False);
            Add('TREEENTRY',ftLargeint,0,false);
            Add('CHANGEDBY',ftString,4,false);
          end;
    end;
end;

function TStatistic.GetTyp: string;
begin
  Result:='S';
end;

constructor TStatistic.CreateEx(aOwner: TComponent; DM: TComponent;
  aConnection: TComponent; aMasterdata: TDataSet);
begin
  inherited CreateEx(aOwner, DM, aConnection, aMasterdata);
  //with BaseApplication as IBaseDbInterface do
    begin
      with DataSet as IBaseDBFilter do
        begin
          BaseSortFields := 'SQL_ID';
          SortFields := 'SQL_ID';
          SortDirection := sdAscending;
          UsePermissions:=True;
          Limit := 0;
        end;
    end;
  FHistory := TBaseHistory.CreateEx(Self,DataModule,aConnection,DataSet);
  FDS := TDataSource.Create(Self);
  FDS.DataSet := DataSet;
  FDS.OnDataChange:=@FDSDataChange;
end;

destructor TStatistic.Destroy;
begin
  inherited Destroy;
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
  aScript: TBaseScript;
  function CheckWildgards(Str : string) : string;
  begin
    Result := Str;
    Result := Stringreplace(Result,'*','%',[rfreplaceAll]);
    result := Stringreplace(Result,'?','_',[rfreplaceAll]);
  end;

begin
  aQuerry := FieldByName('QUERRY').AsString;

  if FieldByName('ISSCRIPT').AsString='Y' then
    begin
      {
      aScript := TBaseScript.CreateEx(nil,Data);
      aScript.Script.Source:=aQuerry;
      FInternalScript:='';
      aScript.Writeln:=@aScriptWriteln;
      aScript.Execute(Null);
      aQuerry:=FInternalScript;
      aScript.Free;
      ]}
    end;

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
                  if aVariables.Values[lowercase(aName)] <> '' then
                    bQuerry:=bQuerry+CheckWildgards(aVariables.Values[lowercase(aName)]);
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
  Result := ReplaceSQLFunctions(bQuerry);
  result := StringReplace(Result,'@USERID@',TBaseDBModule(DataModule).Users.Id.AsString,[rfReplaceAll]);
end;

function TStatistic.BuildSQL(aSQL: string): string;
begin
  Result := ReplaceSQLFunctions(aSQL);
end;

end.

