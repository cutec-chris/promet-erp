unit uPrometORM;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TypInfo, SQLDB, Rtti,Contnrs,memds,fpExprPars,uData,
  MSSQLConn,
  SQLite3Conn,
  PQConnection,
  ubasedatasetinterfaces2;

type
  TContext = record
    Transaction : TSQLTransaction;
    Id : TThreadID;
  end;
  TLockedQuery = class
  private
    cs : TRTLCriticalSection;
  public
    Query : TSQLQuery;
    constructor Create(aQuery : TSQLQuery);
    destructor Destroy; override;
    function Lock : Boolean;
    procedure Unlock;
  end;
  TQueryTable = class(TObjectList)
  private
    FFields: TStrings;
    FTableName: string;
    function GetTable(Table : Integer): TQueryTable;
    function RecoursiveFindField(Table: TQueryTable; Field: string;
      var FullFieldName, FieldTableName, JoinTableName: string): Boolean;
    procedure SetTableName(AValue: string);
    function QuoteField(aField : string) : string;
    function QuoteValue(aField : string) : string;
    function BuildInternalSelect(aFilter: Variant; FoundFields, JoinedTables,
      Params: TStrings): string;
  protected
    Selected : Boolean;
  public
    constructor Create(aClass : TClass);
    destructor Destroy; override;
    property Fields : TStrings read FFields;
    property TableName : string read FTableName write SetTableName;
    property SubTables[Table : Integer] : TQueryTable read GetTable;
    function BuildSelect(aFilter,aFields : string;Params : TStringList) : string;
    function BuildLoad(aSelector : Variant;CascadicIndex : Integer = 0;aParams : TStringList = nil) : string;
  end;

  { TSQLDBDataModule }

  TSQLDBDataModule = class(TBaseDBModule)
  private
    FMandants: TStringList;
    FTables : array of TQueryTable;
    MainConnection : TSQLConnection;
    function GetMandants: TStringList;
    function GetTable(aClass: TClass): TQueryTable;
  public
    //Globally used Querylist to hold all Update/Insert Querys globally prepared avalible
    Querys : array of TLockedQuery;
    //Globally used Contzext List to hold an Transaction per Thread
    Contexts : array of TContext;
    procedure Connect;override;
    function GetConnection(ConnectString : string) : TSQLConnection;
    function FindDataSet(aThreadId : TThreadID;SQL : string) : TLockedQuery;
    //generates SQL to Fill all Published properties and Gerneric TFPGList Types
    //(generates recursive joined Query for default TFPGList Type (or if only one is avalible) and separate Querys for all other)
    //only when this query fails the table structure for all sub-tables is checked so without changes of the table structure we dont have overhead
    function Load(Obj: TPersistent; Selector: Variant; Cascadic: Boolean = True) : Boolean;override;
    //Generates recursive an update Statement per record if SQL_ID is filled or n insert stetement if not
    function Save(Obj: TPersistent; Selector: Variant; Cascadic: Boolean = True) : Boolean;override;
    function Select(Obj: TClass; aFilter: string; aFields: string): TMemDataset;override;
    function Delete(Selector: Variant): Boolean; override;
    function GetID: Int64; override;
    property Mandants : TStringList read GetMandants;
    destructor Destroy; override;
  end;

implementation

uses uEncrypt,db,math;

procedure TQueryTable.SetTableName(AValue: string);
begin
  if FTableName=AValue then Exit;
  FTableName:=AValue;
end;
function TQueryTable.QuoteField(aField: string): string;
begin
  Result := '"'+aField+'"';
end;
function TQueryTable.QuoteValue(aField: string): string;
begin
  Result := ''''+aField+'''';
end;
function TQueryTable.RecoursiveFindField(Table : TQueryTable;Field : string;var FullFieldName,FieldTableName,JoinTableName : string) : Boolean;
var
  bTableName , aField: string;
  i: Integer;
begin
  Result := False;
  if pos('.',Field)>0 then
    begin
      bTableName := copy(Field,0,pos('.',Field)-1);
      aField := copy(Field,pos('.',Field)+1,length(Field));
    end
  else
    begin
      aField := Field;
      bTableName:=TableName;
    end;
  if ((bTableName='') or (lowercase(bTableName)=lowercase(TableName))) and (Table.Fields.IndexOf(aField)>=0) then
    begin
      JoinTableName:=bTableName;
      FieldTableName:=TableName;
      FullFieldName:=QuoteField(Uppercase(bTableName))+'.'+QuoteField(Uppercase(aField));
      Result := True;
      exit;
    end;
  for i := 0 to Table.Count-1 do
    begin
      if RecoursiveFindField(TQueryTable(Table.Items[i]),Field,FullFieldName,FieldTableName,JoinTableName) then
        begin
          Result := True;
          exit;
        end;
    end;
end;
function TQueryTable.BuildInternalSelect(aFilter: Variant; FoundFields,
  JoinedTables, Params: TStrings): string;
var
  tmp, aWhere, cFullFieldName, cFieldTableName, cJoinTableName,
    aParName: String;
  i: Integer;
  scanner: TFPExpressionScanner;
  aToken: TTokenType;
  aVal: int64;
begin
  Result := '';
  FoundFields.Delimiter:=',';
  tmp := '';
  for i := 0 to FoundFields.Count-1 do
    tmp += FoundFields[i]+',';
  if tmp <> '' then
    begin
      Result := 'select '+copy(tmp,0,length(tmp)-1)+' from '+JoinedTables.ValueFromIndex[0];
      JoinedTables.Delete(0);
      while JoinedTables.Count>0 do
        begin
          Result := Result+' left join '+JoinedTables.ValueFromIndex[0]+' on '+JoinedTables.Names[0]+'.'+QuoteField('SQL_ID')+'='+JoinedTables.ValueFromIndex[0]+'.'+QuoteField('REF_ID');
          JoinedTables.Delete(0);
        end;
      scanner := TFPExpressionScanner.Create;
      try
        if TryStrToInt64(aFilter,aVal) then
          scanner.source := 'SQL_ID='+IntToStr(aVal)
        else
          scanner.Source:=aFilter;
        aToken := scanner.GetToken;
        while aToken <> ttEOF do
          begin
            case aToken of
            ttString,ttIdentifier:
              begin
                if RecoursiveFindField(Self,scanner.Token,cFullFieldName,cFieldTableName,cJoinTableName) then
                  aWhere+=cFullFieldName
                else
                  begin
                    if (aParname = '') or (copy(aWhere,length(aWhere)-length(aParName)+1,length(aParName)) <> aParName) then
                      begin
                        aParName := 'PARAM'+IntToStr(Params.Count);
                        Params.Values[aParName]:=scanner.Token;
                        aWhere+= ':'+aParName;
                      end
                    else Params.Values[aParName]:=Params.Values[aParName]+' '+scanner.Token
                  end;
              end
            else
              aWhere+=' '+scanner.Token+' ';
            end;
            aToken := scanner.GetToken;
          end;
      finally
        scanner.Free;
      end;
      if aWhere <> '' then
        Result := Result+' where '+aWhere;
    end;
end;
function TQueryTable.GetTable(Table : Integer): TQueryTable;
begin
  Result := Items[Table] as TQueryTable;
end;
constructor TQueryTable.Create(aClass: TClass);
  procedure ListClassProperties(Obj: TClass;Prefix : string);
  var
    ctx: TRttiContext;
    objType: TRttiType;
    Prop: TRttiProperty;
    aTyp, bTyp: TClass;
  begin
    ctx := TRttiContext.Create;
    objType := ctx.GetType(Obj.ClassInfo);
     for Prop in objType.GetProperties do
       begin
         if (Prop.PropertyType.TypeKind<>tkClass) then
           begin
             if Obj.InheritsFrom(TAbstractDBDataset2) then
               Fields.Add(TAbstractDBDataset2(Obj).MapField(Prop.Name))
             else
               Fields.Add(Prop.Name);
           end
         else if (Prop.PropertyType.TypeKind=tkClass) then
           begin
             if TRttiInstanceType(Prop.PropertyType.BaseType).MetaClassType.InheritsFrom(TAbstractMasterDetail) then
               begin
                 aTyp := TRttiInstanceType(Prop.PropertyType).MetaClassType;
                 try
                   bTyp := TAbstractMasterDetail(aTyp).GetObjectTyp;
                   Add(TQueryTable.Create(bTyp))
                 except
                   //on e : exception do
                   //  debugln(e.message);
                 end;
               end
             else
               writeln('MetaClassType not found: '+TRttiInstanceType(Prop.PropertyType.BaseType).MetaClassType.ClassName);
           end;
       end;
  end;
begin
  inherited Create(True);
  Selected:=False;
  FFields := TStringList.Create;
  if aClass.InheritsFrom(TAbstractDBDataset2) then
    FTableName:=TAbstractDBDataset2(aClass).GetRealTableName
  else FTableName:=copy(aClass.ClassName,2,length(aClass.ClassName));
  ListClassProperties(aClass,copy(aClass.ClassName,2,length(aClass.ClassName)));
end;
destructor TQueryTable.Destroy;
begin
  FFields.Free;
  inherited Destroy;
end;
function TQueryTable.BuildSelect(aFilter, aFields: string; Params: TStringList
  ): string;
var
  ToFindFields,FoundFields,JoinedTables : TStringlist;
  tmp, aWhere, cJoinTableName, cFullFieldName, cFieldTableName,
    aParName: String;
  i: Integer;
  parser: TFPExpressionParser;
  scanner: TFPExpressionScanner;
  aToken: TTokenType;
begin
  ToFindFields := TStringList.Create;
  FoundFields := TStringList.Create;
  JoinedTables := TStringList.Create;
  try
    ToFindFields.Delimiter:=',';
    ToFindFields.DelimitedText:=aFields;
    while ToFindFields.Count>0 do
      if RecoursiveFindField(Self,ToFindFields[0],cFullFieldName,cFieldTableName,cJoinTableName) then
        begin
          FoundFields.Add(cFullFieldName);
          ToFindFields.Delete(0);
          if JoinedTables.IndexOf(QuoteField(Uppercase(cFieldTableName))+JoinedTables.NameValueSeparator+QuoteField(Uppercase(cJoinTableName)))=-1 then
            JoinedTables.AddPair(QuoteField(Uppercase(cFieldTableName)),QuoteField(Uppercase(cJoinTableName)));
        end
      else
        raise Exception.Create('Unable to build Select, Field "'+ToFindFields[0]+'" not found');
    Result := BuildInternalSelect(aFilter,FoundFields,JoinedTables,Params);
  finally
    ToFindFields.Free;
    FoundFields.Free;
    JoinedTables.Free;
  end;
  writeln(Result);
end;
function TQueryTable.BuildLoad(aSelector: Variant; CascadicIndex: Integer;
  aParams: TStringList): string;
var
  FoundFields, JoinedTables: TStringList;
  tmp, aWhere, cFullFieldName, cFieldTableName, cJoinTableName,
    aParName: String;
  i: Integer;
  scanner: TFPExpressionScanner;
  aVal: int64;
  aToken: TTokenType;
  procedure CollectFields(Table : TQueryTable);
  var
    i: Integer;
  begin
    if (CascadicIndex=0) or (not Table.Selected) then
      begin
        for i := 0 to Table.Fields.Count-1 do
          FoundFields.Add(QuoteField(Uppercase(Table.TableName))+'.'+QuoteField(Uppercase(Table.Fields[i]))+' as '+lowercase(Table.Tablename)+'_'+lowercase(Table.Fields[i]));
        Table.Selected:=True;
      end;
    for i := 0 to Table.Count-1 do
      begin
        if not TQueryTable(Table.Items[i]).Selected then
          JoinedTables.AddPair(QuoteField(Uppercase(Table.TableName)),QuoteField(Uppercase(TQueryTable(Table.Items[i]).TableName)));
        CollectFields(TQueryTable(Table.Items[i]));
      end;
  end;
begin
  Result := '';
  FoundFields := TStringList.Create;
  JoinedTables := TStringList.Create;
  try
    JoinedTables.AddPair(QuoteField(Uppercase(Self.TableName)),QuoteField(Uppercase(Self.TableName)));
    CollectFields(Self);
    Result := BuildInternalSelect(aSelector,FoundFields,JoinedTables,aParams);
    writeln(Result);
  finally
    FoundFields.Free;
    JoinedTables.Free;
  end;
end;
function TSQLDBDataModule.GetMandants: TStringList;
begin
  if not Assigned(FMandants) then
    FMandants := TStringList.Create;
  if FMandants.Count=0 then
    begin

    end;
end;
function TSQLDBDataModule.GetTable(aClass : TClass): TQueryTable;
var
  i: Integer;
begin
  for i := 0 to length(FTables)-1 do
    if FTables[i].TableName=copy(aClass.ClassName,2,length(aClass.ClassName)) then
      begin
        Result := FTables[i];
        exit;
      end;
  Result := TQueryTable.Create(aClass);
  Setlength(FTables,length(FTables)+1);
  FTables[length(Ftables)-1] := Result;
end;
procedure TSQLDBDataModule.Connect;
var
  ConfigFile: TStringList;
  Port: Integer;
  Properties : TStringList;
  Protocol, User, Password, HostName, Database, tmp, FDatabaseDir: String;
  FEData: Boolean;
begin
  ConfigFile := TStringList.Create;
  ConfigFile.LoadFromFile(ConfigPath+Mandant+'.perml');
  try
    MainConnection := GetConnection(ConfigFile[1]);
    MainConnection.Connected:=True;
  finally
    ConfigFile.Free;
  end;
end;
function TSQLDBDataModule.GetConnection(ConnectString: string): TSQLConnection;
var
  Port: Integer;
  Properties : TStringList;
  Protocol, User, Password, HostName, Database, tmp, FDatabaseDir: String;
  FEData: Boolean;
begin
  Properties := TStringList.Create;
  Port:=0;
  Properties.Clear;
  Protocol:='';
  User:='';
  Password:='';
  HostName:='';
  Database:='';
  tmp := ConnectString;
  if copy(tmp,0,pos(';',tmp)-1) <> 'sqlite-3-edata' then
    Protocol:=copy(tmp,0,pos(';',tmp)-1)
  else
    begin
      Protocol:='sqlite-3';
      FEData:=True;
    end;
  //Assert(Protocol<>'',strUnknownDbType);
  tmp := copy(tmp,pos(';',tmp)+1,length(tmp));
  HostName := copy(tmp,0,pos(';',tmp)-1);
  if pos(':',HostName) > 0 then
    begin
      Port:=StrToInt(copy(HostName,pos(':',HostName)+1,length(HostName)));
      HostName:=copy(HostName,0,pos(':',HostName)-1);
    end
  else if pos('/',HostName) > 0 then
    begin
      Port:=StrToInt(copy(HostName,pos('/',HostName)+1,length(HostName)));
      HostName:=copy(HostName,0,pos('/',HostName)-1);
    end;
  tmp := copy(tmp,pos(';',tmp)+1,length(tmp));
  Database:=copy(tmp,0,pos(';',tmp)-1);
  FDatabaseDir:=ExtractFileDir(ExpandFileName(Database));
  tmp := copy(tmp,pos(';',tmp)+1,length(tmp));
  User := copy(tmp,0,pos(';',tmp)-1);
  tmp := copy(tmp,pos(';',tmp)+1,length(tmp));
  if copy(tmp,0,1) = 'x' then
    Password := Decrypt(copy(tmp,2,length(tmp)),word(99998))
  else
    Password := tmp;
  Result := TSQLConnector.Create(Self);
  if copy(Protocol,0,10)='postgresql' then
    TSQLConnector(Result).ConnectorType:='PostgreSQL'
  else if copy(Protocol,0,6)='sqlite' then
    TSQLConnector(Result).ConnectorType:='SQLite3'
  else if copy(Protocol,0,5)='mssql' then
    TSQLConnector(Result).ConnectorType:='MSSQLServer'
  else
    raise Exception.Create('Unknown Database Server Type');
  TSQLConnector(Result).HostName:=HostName;
  TSQLConnector(Result).DatabaseName:=Database;
  TSQLConnector(Result).UserName:=User;
  TSQLConnector(Result).Password:=Password;
  TSQLConnector(Result).Params.Assign(Properties);
  TSQLConnector(Result).Params.Add('port='+IntToStr(Port));
  TSQLConnector(Result).Params.Add('application_name=''Avamm''');;
  Properties.Free;
end;
function TSQLDBDataModule.FindDataSet(aThreadId: TThreadID; SQL: string
  ): TLockedQuery;
var
  aQuery: TLockedQuery;
  i: Integer;
  procedure SetupTransaction(Query : TSQLQuery);
  var
    aContext: ^TContext;
    a: Integer;
  begin
    for a := 0 to length(Contexts)-1 do
      if Contexts[a].Id = aThreadId then
        begin
          Query.Transaction := Contexts[a].Transaction;
          Query.DataBase := Query.Transaction.DataBase;
          exit;
        end;
    Setlength(Contexts,length(Contexts)+1);
    aContext := @Contexts[length(Contexts)-1];
    aContext^.Transaction := TSQLTransaction.Create(MainConnection);
    aContext^.Id:=aThreadId;
    aContext^.Transaction.DataBase := MainConnection;
    Query.DataBase := MainConnection;
  end;

begin
  Result := nil;
  for i := 0 to length(Querys)-1 do
    if (Querys[i].Query.SQL.Text = SQL+LineEnding)
    and (Querys[i].Lock) then
      begin
        Result := Querys[i];
        SetupTransaction(Result.Query);
        exit;
      end;
  aQuery := TLockedQuery.Create(TSQLQuery.Create(MainConnection));
  aQuery.Lock;
  aQuery.Query.SQL.Text:=SQL;
  Setlength(Querys,length(Querys)+1);
  Querys[length(Querys)-1] := aQuery;
  SetupTransaction(aQuery.Query);
  Result := aQuery;
end;
function TSQLDBDataModule.Load(Obj: TPersistent; Selector: Variant;
  Cascadic: Boolean): Boolean;
var
  aTable: TQueryTable;
  aDataSet: TLockedQuery;
  actCascade : Integer = 1;
  bParams: TStringList;
  actLoad: String;
  i: Integer;
  procedure FillDataSet(aObj : TPersistent;aDataSet : TDataset);
  var
    a, b: Integer;
    ctx: TRttiContext;
    objType: TRttiType;
    Prop: TRttiProperty;
    aFieldName, aTablename: String;
    aField: TField;
    aVal: TValue;
    aTyp, bTyp : TClass;
    aObjTyp: TAbstractDBDataset2Class;
    aDetail, nObj: TObject;
    SubClassFilled : Boolean = False;
    aPerf: QWord;
  begin
    aPerf := GetTickCount64;
    writeln('FillDataSet('+aObj.ClassName+')');
    repeat
      //Fill aObj Fields
      ctx := TRttiContext.Create;
      aTablename := lowercase(aObj.ClassName);
      objType := ctx.GetType(aObj.ClassInfo);
      if aObj.InheritsFrom(TAbstractDBDataset2) then
        aTableName:=lowercase(TAbstractDBDataset2(aObj).GetRealTableName);
      if aDataSet.FieldDefs.IndexOf(aTablename+'_sql_id')>-1 then
        aField := aDataSet.FieldByName(aTablename+'_sql_id');
      if Assigned(aField) then
        for Prop in objType.GetProperties do
          begin
            if (Prop.PropertyType.TypeKind<>tkClass) then
              begin
                if aObj.InheritsFrom(TAbstractDBDataset2) then
                  aFieldName := lowercase(TAbstractDBDataset2(aObj).MapField(Prop.Name))
                else
                  aFieldName := lowercase(Prop.Name);
                aField := aDataSet.FieldByName(aTablename+'_'+aFieldName);
                if Assigned(aField) then
                  begin
                    try
                      case Prop.PropertyType.TypeKind of
                      tkFloat:               // Also for TDateTime !
                        SetFloatProp(aObj,PPropInfo(Prop.Handle),aField.AsFloat);
                      tkInteger,tkInt64,tkQWord,tkChar,tkWChar:
                        Prop.SetValue(aObj, aField.AsLargeInt);
                      tkUString,tkAString:
                        Prop.SetValue(aObj, aField.AsString);
                      tkBool:
                        begin
                          if aField.DataType=ftString then
                            begin
                              if aField.AsString='Y' then
                                Prop.SetValue(aObj,1)
                              else
                                Prop.SetValue(aObj,0);
                            end
                          else
                            Prop.SetValue(aObj, aField.AsLargeInt);
                        end;
                      // You should add other types as well
                      end;
                    except
                      // Ignore any exception here. Likely to be caused by
                      // invalid value format
                    end;
                  end
                else
                  raise Exception.Create('Property not Found !');
              end;
          end;
      for Prop in objType.GetProperties do
        begin
          if (Prop.PropertyType.TypeKind=tkClass) then
            begin
              if TRttiInstanceType(Prop.PropertyType.BaseType).MetaClassType=TAbstractMasterDetail then
                begin
                  aDetail := GetObjectProp(aObj,PPropInfo(Prop.Handle));
                  if Assigned(aDetail) then
                  with TAbstractMasterDetail(aDetail) do
                    begin
                      for b := Count-1 downto max(Count-2,0) do
                        if Items[b] is TAbstractDBDataset2 then
                          begin
                            if aTableName = '' then
                              begin
                                if Items[b].InheritsFrom(TAbstractDBDataset2) then
                                  aTableName:=lowercase(TAbstractDBDataset2(Items[b]).GetRealTableName)
                                else aTablename := lowercase(Items[b].ClassName);
                              end;
                            aField := aDataSet.FieldByName(aTablename+'_sql_id');
                            if Assigned(aField) and (aField.AsLargeInt=TAbstractDBDataset2(Items[b]).SQL_ID) then
                              begin
                                //we already have our row with that sql_id
                                FillDataSet(TPersistent(aDetail),aDataSet);
                                SubClassFilled := True;
                                break;
                              end;
                          end;
                      //not found we have to add an new object
                      aObjTyp := TAbstractDBDataset2Class(GetObjectTyp);
                      //writeln('Creating Object:'+nObj.ClassName+' (sql_id:'+aField.AsString+')');
                      nObj := aObjTyp.Create(aObj);
                      aTableName := TAbstractDBDataset2(nObj).GetRealTableName;
                      aField := aDataSet.FieldByName(aTablename+'_sql_id');
                      if Assigned(aField) and (aField.AsLargeInt<>0) then
                        begin
                          Add(nObj);
                          FillDataSet(TPersistent(nObj),aDataSet);
                          SubClassFilled := True;
                          break;
                        end
                      else
                        begin
                          //writeln('Destroying Object:'+nObj.ClassName+' (sql_id:'+aField.AsString+')');
                          FreeAndNil(nObj);
                          SubClassFilled:=False;
                        end;
                    end;
                end;
            end;
        end;
      ctx.Free;
      if not SubClassFilled then
        begin
          aDataSet.Next;
          //writeln('GetTickCount:'+IntToStr(GetTickCount64-aPerf));
          exit;
        end;
    until aDataSet.EOF;
    writeln('GetTickCounte:'+IntToStr(GetTickCount64-aPerf)+' Size:'+IntToStr(aObj.InstanceSize));
  end;

begin
  Result := False;
  aTable := GetTable(Obj.ClassType);
  bParams := TStringList.Create;
  actLoad := aTable.BuildLoad(Selector,0,bParams);
  while actLoad <> '' do
    begin
      aDataSet := FindDataSet(ThreadID,actLoad);
      if Assigned(aDataSet) then
        begin
          if not aDataSet.Query.Prepared then
            begin
              writeln('Preparing DataSet ',IntToHex(Qword(@aDataSet),8));
              aDataSet.Query.Prepare;
            end;
          for i := 0 to bParams.Count-1 do
            aDataSet.Query.Params.ParamValues[bParams.Names[i]]:=bParams.ValueFromIndex[i];
          aDataSet.Query.Open;
          //Fill in Class
          writeln(aDataSet.Query.RecordCount);
          aDataSet.Query.First;
          if not aDataSet.Query.EOF then Result := True;
          FillDataSet(Obj,aDataSet.Query);
          aDataSet.Query.Close;
          aDataSet.Unlock;
          bParams.Clear;
          actLoad := aTable.BuildLoad(Selector,actCascade,bParams);
          inc(actCascade);
        end;
    end;
  bParams.Free;
end;
function TSQLDBDataModule.Save(Obj: TPersistent; Selector: Variant;
  Cascadic: Boolean): Boolean;
var
  aTable: TQueryTable;
begin
  Result := False;
  aTable := GetTable(Obj.ClassType);
end;
function TSQLDBDataModule.Select(Obj: TClass; aFilter: string; aFields: string
  ): TMemDataset;
var
  aTable: TQueryTable;
  aDataSet: TLockedQuery;
  aParams: TStringList;
  i: Integer;
begin
  Result := TMemDataset.Create(nil);
  aTable := GetTable(Obj);
  aParams := TStringList.Create;
  aDataSet := FindDataSet(ThreadID,aTable.BuildSelect(aFilter,aFields,aParams));
  if Assigned(aDataSet) then
    begin
      for i := 0 to aParams.Count-1 do
        aDataSet.Query.Params.ParamValues[aParams.Names[i]]:=aParams.ValueFromIndex[i];
      aDataSet.Query.Open;
      Result.CopyFromDataset(aDataSet.Query);
      aDataSet.Query.Close;
      aDataSet.Unlock;
      aParams.Free;
    end;
end;
function TSQLDBDataModule.Delete(Selector: Variant): Boolean;
begin
  Result := False;
end;
function TSQLDBDataModule.GetID: Int64;
begin
  Result := -1;
end;
destructor TSQLDBDataModule.Destroy;
begin
  //MainConnection.Destroy;
  inherited Destroy;
end;
constructor TLockedQuery.Create(aQuery: TSQLQuery);
begin
  InitCriticalSection(cs);
  Query := aQuery;
  Query.PacketRecords:=-1;
end;
destructor TLockedQuery.Destroy;
begin
  DoneCriticalSection(cs);
  inherited Destroy;
end;
function TLockedQuery.Lock: Boolean;
begin
  Result := TryEnterCriticalSection(cs) <> 0;
end;
procedure TLockedQuery.Unlock;
begin
  LeaveCriticalSection(cs);
end;
initialization
  DataM := TSQLDBDataModule.Create(nil);
end.

