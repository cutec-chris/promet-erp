unit uPrometORM;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TypInfo, SQLDB, Rtti,Contnrs,memds,fpExprPars,
  MSSQLConn,
  SQLite3Conn,
  PQConnection,
  ubasedatasetinterfaces2;

type
  TContext = record
    Transaction : TSQLTransaction;
    Id : TThreadID;
  end;

  { TLockedQuery }

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

  { TQueryTable }

  TQueryTable = class(TObjectList)
  private
    FFields: TStrings;
    FTableName: string;
    function GetTable(Table : Integer): TQueryTable;
    procedure SetTableName(AValue: string);
    function QuoteField(aField : string) : string;
    function QuoteValue(aField : string) : string;
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

  TSQLDBDataModule = class(TComponent)
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
    ConfigPath : string;
    Mandant : string;
    procedure Connect;
    function GetConnection(ConnectString : string) : TSQLConnection;
    function FindDataSet(aThreadId : TThreadID;SQL : string) : TLockedQuery;
    //generates SQL to Fill all Published properties and Gerneric TFPGList Types
    //(generates recursive joined Query for default TFPGList Type (or if only one is avalible) and separate Querys for all other)
    //only when this query fails the table structure for all sub-tables is checked so without changes of the table structure we dont have overhead
    procedure Load(Obj: TPersistent; Selector: Variant; Cascadic: Boolean = True);
    //Generates recursive an update Statement per record if SQL_ID is filled or n insert stetement if not
    procedure Save(Obj: TPersistent; Selector: Variant; Cascadic: Boolean = True);
    function Select(Obj: TClass; aFilter: string; aFields: string): TMemDataset;

    property Mandants : TStringList read GetMandants;
    destructor Destroy; override;
  end;

var
  Data : TSQLDBDataModule;

implementation

uses uEncrypt;

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
         Fields.Add(Prop.Name);
         if (Prop.PropertyType.TypeKind=tkClass) then
           begin
             if TRttiInstanceType(Prop.PropertyType.BaseType).MetaClassType=TAbstractMasterDetail then
               begin
                 aTyp := TRttiInstanceType(Prop.PropertyType).MetaClassType;
                 try
                   bTyp := TAbstractMasterDetail(aTyp).GetObjectTyp;
                   Add(TQueryTable.Create(bTyp))
                 except
                   //on e : exception do
                   //  debugln(e.message);
                 end;
               end;
           end;
       end;
  end;
begin
  inherited Create(True);
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
  function RecoursiveFindField(Table : TQueryTable;Field : string;var FullFieldName,FieldTableName,JoinTableName : string) : Boolean;
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
    if ((bTableName='') or (lowercase(bTableName)=lowercase(TableName))) and (Table.Fields.IndexOf(aField)>0) then
      begin
        JoinTableName:=bTableName;
        FieldTableName:=TableName;
        FullFieldName:=QuoteField(bTableName)+'.'+QuoteField(aField);
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
          JoinedTables.AddPair(QuoteField(cFieldTableName),QuoteField(cJoinTableName));
        end
      else
        raise Exception.Create('Unable to build Select, Field "'+ToFindFields[0]+'" not found');
    FoundFields.Delimiter:=',';
    tmp := '';
    for i := 0 to FoundFields.Count-1 do
      tmp := FoundFields[i]+',';
    Result := 'select '+copy(tmp,0,length(tmp)-1)+' from '+JoinedTables.ValueFromIndex[0];
    JoinedTables.Delete(0);
    while JoinedTables.Count>0 do
      begin
        Result := Result+' left join '+JoinedTables.ValueFromIndex[0]+' on '+JoinedTables.Names[0]+'.'+QuoteField('SQL_ID')+'='+JoinedTables.ValueFromIndex[0]+'.'+QuoteField('REF_ID');
        JoinedTables.Delete(0);
      end;
    scanner := TFPExpressionScanner.Create;
    try
      aWhere := ' where ';
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
                  aParName := 'PARAM'+IntToStr(Params.Count);
                  Params.Values[aParName]:=scanner.Token;
                  aWhere+= ':'+aParName;

                end;
            end
          else
            aWhere+=scanner.Token;
          end;
          aToken := scanner.GetToken;
        end;
    finally
      scanner.Free;
    end;
    Result := Result+aWhere;
  finally
    ToFindFields.Free;
    FoundFields.Free;
    JoinedTables.Free;
  end;
  writeln(Result);
end;

function TQueryTable.BuildLoad(aSelector: Variant; CascadicIndex: Integer;
  aParams: TStringList): string;
begin
  Result := '';
end;

{ TSQLDBDataModule }

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
procedure TSQLDBDataModule.Load(Obj: TPersistent;Selector : Variant; Cascadic: Boolean);
var
  aTable: TQueryTable;
  aDataSet: TLockedQuery;
  actCascade : Integer = 1;
  bParams: TStringList;
  actLoad: String;
  i: Integer;
begin
  aTable := GetTable(Obj.ClassType);
  bParams := TStringList.Create;
  actLoad := aTable.BuildLoad(Selector,0,bParams);
  while actLoad <> '' do
    begin
      aDataSet := FindDataSet(ThreadID,actLoad);
      if Assigned(aDataSet) then
        begin
          for i := 0 to bParams.Count-1 do
            aDataSet.Query.Params.ParamValues[bParams.Names[i]]:=bParams.ValueFromIndex[i];
          aDataSet.Query.Open;
          //Fill in Class


          aDataSet.Query.Close;
          aDataSet.Unlock;
          actLoad := aTable.BuildLoad(Selector,actCascade,bParams);
          inc(actCascade);
        end;
    end;
  bParams.Free;
end;
procedure TSQLDBDataModule.Save(Obj: TPersistent;Selector : Variant; Cascadic: Boolean);
var
  aTable: TQueryTable;
begin
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

destructor TSQLDBDataModule.Destroy;
begin
  FreeAndNil(MainConnection);
  inherited Destroy;
end;

{ TLockedQuery }

constructor TLockedQuery.Create(aQuery: TSQLQuery);
begin
  InitCriticalSection(cs);
  Query := aQuery;
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
  Data := TSQLDBDataModule.Create(nil);
finalization
  FreeAndNil(Data);
end.

