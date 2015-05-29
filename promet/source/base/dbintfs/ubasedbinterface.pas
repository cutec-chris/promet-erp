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
unit uBaseDBInterface;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, DB, Typinfo, CustApp, Utils , memds,
  uBaseDbClasses, uIntfStrConsts,
  uBaseSearch,uBaseERPDbClasses,uDocuments,uOrder,Variants,uProcessManagement,
  rttiutils,uBaseDatasetInterfaces
  ;
const
  MandantExtension = '.perml';
type

  { TDBConfig }

  TDBConfig = class(TPropsStorage)
  public
    function  ReadString(const ASection, Ident, DefaultValue: string): string; override;
    procedure WriteString(const ASection, Ident, Value: string); override;
    function  ReadString(Ident, DefaultValue: string): string;overload;
    procedure WriteString(Ident, Value: string);overload;
    procedure ReadRect(const Ident: string; out ARect: TRect;
                       const Default: TRect);
    procedure WriteRect(const Ident: string; const Value: TRect);
    function  ReadInteger(const Ident: string; DefaultValue: Longint): Longint;
    procedure WriteInteger(const Ident: string; Value: Longint);
    function  ReadBoolean(const Ident: string; DefaultValue: Boolean): Boolean;
    procedure WriteBoolean(const Ident: string; Value: Boolean);
  end;
  TInternalDBDataSet = class
  private
    FDataSet: TDataSet;
  public
    destructor Destroy;override;
    property DataSet : TDataSet read FDataSet write FDataSet;
  end;
  TOpenLinkEvent = function(aLink : string;Sender : TObject) : Boolean of object;
  TCreateFromLinkEvent = function(aLink : string;Sender : TObject) : TBaseDbDataSet of object;
  LinkHandler = record
    aLinkType : string;
    aEvent : TOpenLinkEvent;
    aClass : TBaseDBDatasetClass;
    aListClass : TBaseDBDatasetClass;
  end;

  { TBaseDBModule }

  TBaseDBModule = class(TComponent)
  private
    FConnect: TNotifyEvent;
    FConnectionLost: TNotifyEvent;
    FKeepAlive: TNotifyEvent;
    FLastStmt: string;
    FLastTime: Int64;
    FSessionID: LargeInt;
    FTables: TStrings;
    FTriggers: TStrings;
    FUsersFilter: string;
    FCheckedTables : TStringList;
    FLinkHandlers : array of LinkHandler;
    FIgnoreOpenRequests : Boolean;
  protected
    FDataSetClass : TDataSetClass;
    function GetSyncOffset: Integer;virtual;abstract;
    procedure SetSyncOffset(const AValue: Integer);virtual;abstract;
    function GetLimitAfterSelect: Boolean;virtual;
    function GetLimitSTMT: string;virtual;
  public
    Users : TUser;
    ActiveUsers : TActiveUsers;
    Numbers : TNumberSets;
    MandantDetails : TMandantDetails;
    Tree : TTree;
    Forms : TForms;
    Filters : TFilters;
    Reports : TReports;
    Permissions : TPermissions;
    StorageTypes : TStorageTypes;
    Currency : TCurrency;
    PaymentTargets : TPaymentTargets;
    StorageType : TStorageTyp;
    StorageJournal : TStorageJournal;
    Countries : TCountries;
    Languages : TLanguages;
    Userfielddefs : TUserFielddefs;
    States : TStates;
    Categories : TCategory;
    DeletedItems : TDeletedItems;
    TableVersions : TBaseDBDataset;
    //_DocumentActions : TInternalDBDataSet;
    //_MimeTypes : TInternalDBDataSet;
    ProcessClient : TProcessClient;
    constructor Create(AOwner : TComponent);virtual;
    destructor Destroy;override;
    function GetConnection: TComponent;virtual;abstract;
    property SessionID : LargeInt read FSessionID write FSessionID;
    property MainConnection : TComponent read GetConnection;
    property UsersFilter : string read FUsersFilter;
    function GetNewConnection: TComponent;virtual;abstract;
    procedure Disconnect(aConnection : TComponent);virtual;abstract;
    function StartTransaction(aConnection : TComponent;ForceTransaction : Boolean = False): Boolean;virtual;abstract;
    function CommitTransaction(aConnection : TComponent): Boolean;virtual;abstract;
    function RollbackTransaction(aConnection : TComponent): Boolean;virtual;abstract;
//    function IsTransactionActive(aConnection : TComponent): Boolean;virtual;abstract;
    procedure DeleteExpiredSessions;virtual;
    function SetProperties(aProp : string;Connection : TComponent = nil) : Boolean;virtual;
    function CreateDBFromProperties(aProp : string) : Boolean;virtual;
    property LastStatement : string read FLastStmt write FLastStmt;
    property LastTime : Int64 read FLastTime write FLastTime;
    function IsSQLDB : Boolean;virtual;abstract;
    function ProcessTerm(aTerm : string) : string;virtual;
    function GetUniID(aConnection : TComponent = nil;Generator : string = 'GEN_SQL_ID';AutoInc : Boolean = True) : Variant;virtual;abstract;
    function GetNewDataSet(aTable : TBaseDBDataSet;aConnection : TComponent = nil;MasterData : TDataSet = nil;aTables : string = '') : TDataSet;virtual;abstract;
    function GetNewDataSet(aSQL : string;aConnection : TComponent = nil;MasterData : TDataSet = nil;aOrigtable : TBaseDBDataSet = nil) : TDataSet;virtual;
    procedure DestroyDataSet(DataSet : TDataSet);virtual;abstract;
    function Ping(aConnection : TComponent) : Boolean;virtual;abstract;
    procedure BlobFieldToFile(DataSet : TDataSet;Fieldname : string;Filename : string);virtual;
    procedure FileToBlobField(Filename : string;DataSet : TDataSet;Fieldname : string);virtual;
    procedure StreamToBlobField(Stream : TStream;DataSet : TDataSet;Fieldname : string);virtual;
    procedure BlobFieldToStream(DataSet: TDataSet; Fieldname: string;
      dStream: TStream); virtual;
    function QuoteField(aField : string) : string;virtual;
    function QuoteValue(aValue : string) : string;virtual;
    function EscapeString(aValue : string) : string;virtual;
    function DateToFilter(aValue : TDateTime) : string;virtual;
    function DateTimeToFilter(aValue : TDateTime) : string;virtual;
    function GetLinkDesc(aLink : string) : string;virtual;
    function GetLinkLongDesc(aLink : string) : string;virtual;
    function GetLinkIcon(aLink: string; GetRealIcon: Boolean=False): Integer;
      virtual;
    function BuildLink(aDataSet : TDataSet) : string;virtual;
    function GotoLink(const aLink : string) : Boolean;virtual;
    function DataSetFromLink(aLink: string;var aClass : TBaseDBDatasetClass): Boolean;
    function ListDataSetFromLink(aLink: string;var aClass : TBaseDBDatasetClass): Boolean;
    procedure RegisterLinkHandler(aLink : string;aOpenHandler : TOpenLinkEvent;DataSetClass : TBaseDBDatasetClass;DataSetListClass : TBaseDBDatasetClass = nil);
    function GetBookmark(aDataSet : TBaseDbDataSet) : Variant;
    function GotoBookmark(aDataSet : TBaseDbDataSet;aRec : Variant) : Boolean;
    function Locate(aDataSet : TBaseDbDataSet;const keyfields: string; const keyvalues: Variant; aoptions: TLocateOptions) : boolean;
    function GetErrorNum(e : EDatabaseError) : Integer;virtual;
    function RecordCount(aDataSet : TBaseDbDataSet) : Integer;
    function DeleteItem(aDataSet : TBaseDBDataSet) : Boolean;
    function ShouldCheckTable(aTableName : string;SetChecked : Boolean = True) : Boolean;
    procedure UpdateTableVersion(aTableName: string);
    function RemoveCheckTable(aTableName : string) : Boolean;
    function TableExists(aTableName : string;aConnection : TComponent = nil;AllowLowercase: Boolean = False) : Boolean;virtual;abstract;
    function TriggerExists(aTriggerName : string;aConnection : TComponent = nil;AllowLowercase: Boolean = False) : Boolean;virtual;
    function CreateTrigger(aTriggerName : string;aTableName : string;aUpdateOn : string;aSQL : string;aField : string = '';aConnection : TComponent = nil) : Boolean;virtual;
    function DropTable(aTableName : string) : Boolean;virtual;abstract;
    function GetColumns(TableName : string) : TStrings;virtual;abstract;
    function CheckForInjection(aFilter : string) : Boolean;
    function GetDBType : string;virtual;
    procedure SetFilter(DataSet : TbaseDBDataSet;aFilter : string;aLimit : Integer = 0;aOrderBy : string = '';aSortDirection : string = 'ASC';aLocalSorting : Boolean = False;aGlobalFilter : Boolean = True;aUsePermissions : Boolean = False;aFilterIn : string = '');
    procedure AppendUserToActiveList;
    procedure RefreshUsersFilter;
    procedure RemoveUserFromActiveList;
    procedure RegisterLinkHandlers;
    property IgnoreOpenRequests : Boolean read FIgnoreOpenrequests write FIgnoreOpenrequests;
    property Tables : TStrings read FTables;
    property CheckedTables : TStringList read FCheckedTables;
    property Triggers : TStrings read FTriggers;
    property LimitAfterSelect : Boolean read GetLimitAfterSelect;
    property LimitSTMT : string read GetLimitSTMT;
    property SyncOffset : Integer read GetSyncOffset write SetSyncOffset;
    property OnConnectionLost : TNotifyEvent read FConnectionLost write FConnectionLost;
    property OnConnect : TNotifyEvent read FConnect write FConnect;
    property OnDisconnectKeepAlive : TNotifyEvent read FKeepAlive write FKeepAlive;
  end;
  IBaseDBInterface = interface['{A2AB4BAB-38DF-4D4E-BCE5-B7D57E115ED5}']
    function GetConfig: TDBConfig;
    function GetDB: TBaseDBModule;
    function GetLastError: string;
    function GetMandantName: string;
    function GetMandantPath: string;
    function LoadMandants(aConfigPath : string = '') : Boolean;
    function OpenMandant(aDBTyp : string = '';aDBProp : string = ''): Boolean;
    function QuoteField(aField : string) : string;
    function QuoteValue(aValue : string) : string;
    function DBLogin(aMandant,aUser : string; HideStatus: Boolean=False;AppendToActiveList : Boolean = True) : Boolean;
    procedure DBLogout;
    procedure SetDB(const AValue: TBaseDBModule);
    procedure SetDBTyp(const AValue: string);
    procedure SetLastError(const AValue: string);
    procedure SetMandantPath(AValue: string);
    procedure SetOwner(aOwner : TObject);
    property MandantPath : string read GetMandantPath write SetMandantPath;
    property DBTyp : string write SetDBTyp;
    property Data : TBaseDBModule read GetDB write SetDB;
    property DBConfig : TDBConfig read GetConfig;
    property LastError : string read GetLastError write SetLastError;
    property MandantName : string read GetMandantName;
  end;

  { TBaseDBInterface }

  TBaseDBInterface = class(TInterfacedObject,IBaseDBInterface)
  private
    FDB : TBaseDBModule;
    FConfigPath : string;
    FMandantFile : string;
    FOwner: TObject;
    FConfig : TDBConfig;
    FDbTyp : string;
    FLastError : string;
  protected
    function GetMandantPath: string;
    procedure SetMandantPath(AValue: string);
    procedure SetDBTyp(const AValue: string);
    function GetConfig : TDBConfig;
    function GetLastError: string;
    procedure SetLastError(const AValue: string);
  public
    constructor Create;
    procedure SetOwner(aOwner : TObject);
    function GetDB: TBaseDBModule;
    procedure SetDB(const AValue: TBaseDBModule);
    destructor Destroy;override;
    procedure DBLogout;
    function GetMandantName: string;
    function DBLogin(aMandant,aUser : string; HideStatus: Boolean=False;AppendToActiveList : Boolean = True) : Boolean;
    function LoadMandants(aConfigPath : string = '') : Boolean;
    function OpenMandant(aDBTyp : string = '';aDBProp : string = ''): Boolean;
    function QuoteField(aField : string) : string;
    function QuoteValue(aValue : string) : string;
  end;

const
  RIGHT_NONE  = 0;
  RIGHT_READ  = 1;
  RIGHT_WRITE = 2;
  RIGHT_DELETE= 3;
  RIGHT_PERMIT= 4;

  IMAGE_PERSON             = 1;
  IMAGE_TASK               = 2;
  IMAGE_SUPPLIER           = 20;
  IMAGE_SEARCH             = 8;
  IMAGE_FOLDER             = 19;
  IMAGE_STATISTIC          = 58;
  IMAGE_MESSAGEOPEN        = 5;
  IMAGE_TABLEDIR           = 98;
  IMAGE_MESSAGE            = 36;
  IMAGE_FEED               = 61;
  IMAGE_SCRIPT             = 62;
  IMAGE_MASTERDATA         = 0;
  IMAGE_DOCUMENTS          = 25;
  IMAGE_ORDERS             = 7;
  IMAGE_CALLS              = 51;
  IMAGE_CALENDAR           = 4;
  IMAGE_FINANCIAL          = 9;
  IMAGE_PROJECT            = 13;
  IMAGE_BANKING            = 32;// 23;  32
  IMAGE_ACCOUNTS           = 33;// 24;  33
  IMAGE_ACCOUNT            = 34;// 25;  34
  IMAGE_NEWACCOUNT         = 35;// 26; 35
  IMAGE_NEWTRANSFER        = 38;// 28; 38
  IMAGE_ACCOUNTINGQUE      = 37;// 27; 37
  IMAGE_ORDERPAGE          = 25;
  IMAGE_REDORDERPAGE       = 26;
  IMAGE_ORDERLIST          = 24;
  IMAGE_WIKI               = 11;
  IMAGE_PROJECTS           = 2;
  IMAGE_WEBSITE            = 94;
  IMAGE_FAVOURITES         = 14;
  IMAGE_STATISTICS         = 15;
  IMAGE_TIME               = 6;

  TREE_ID_CUSTOMER_UNSORTED   = 32999;
  TREE_ID_MASTERDATA_UNSORTED = 32998;
  TREE_ID_PROJECT_UNSORTED    = 329997;
  TREE_ID_MESSAGES            = 329996;
  TREE_ID_UNKNOWN_MESSAGES    = 329995;
  TREE_ID_SPAM_MESSAGES       = 329994;
  TREE_ID_ARCHIVE_MESSAGES    = 329993;
  TREE_ID_SEND_MESSAGES       = 329992;
  TREE_ID_DELETED_MESSAGES    = 329991;
  TREE_ID_WIKI_UNSORTED       = 329990;
  TREE_ID_LOG_MESSAGES        = 329989;

  ACICON_EDITED   = 0;
  ACICON_MAILNEW  = 1;
  ACICON_MAILANSWERED = 2;
  ACICON_CALL     = 3;
  ACICON_NEWORDER = 4;
  ACICON_ORDERPOSTED = 5;
  ACICON_STATUSCH = 6;
  ACICON_DOCUMENTADDED = 7;
  ACICON_USEREDITED = 8;
  ACICON_RENAMED = 12;
  ACICON_ORERSTATUSCH = 6;
  ACICON_TASKADDED     = 10;
  ACICON_TASKCLOSED    = 9;
  ACICON_DATECHANGED   = 11;
  ACICON_OFFICECHANGED   = 13;
  ACICON_EXTERNALCHANGED   = 14;
resourcestring
  strGuest                       = 'Gast';
  strSQLInjection                = 'Versuchte SQL Injection !';
  strWebsite                     = 'Webseite';
  strScreenshotName              = 'Screenshot Name';
  strEnterAnName                 = 'enter an Name';
  strProjectProcess              = 'Projekt/Prozess';
  strFor                         = 'für';
implementation
uses uZeosDBDM, uBaseApplication, uWiki, uMessages, uprocessmanager,uRTFtoTXT,
  utask,uPerson,uMasterdata,uProjects,umeeting,uStatistic,usync;

{ TDBConfig }

destructor TInternalDBDataSet.Destroy;
begin
  if Assigned(FDataSet) then
    FDataSet.Free;
  inherited Destroy;
end;
function TDBConfig.ReadString(const ASection, Ident, DefaultValue: string
  ): string;
begin
  Result := DefaultValue;
  with BaseApplication as IBaseDBInterface do
    begin
      if not Data.Users.DataSet.Active then Data.Users.Open;
      Result := Data.Users.Options.GetOption(ASection,Ident,DefaultValue);
    end;
end;
procedure TDBConfig.WriteString(const ASection, Ident, Value: string);
begin
  with BaseApplication as IBaseDBInterface do
    Data.Users.Options.SetOption(ASection, Ident, Value);
end;

function TDBConfig.ReadString(Ident, DefaultValue: string): string;
begin
  if Assigned(Self) then
    Result := ReadString('',Ident,DefaultValue)
  else Result := DefaultValue;
end;

procedure TDBConfig.WriteString(Ident, Value: string);
begin
  WriteString('',Ident,Value);
end;

procedure TDBConfig.ReadRect(const Ident: string; out ARect: TRect;
  const Default: TRect);
begin
  ARect.Left:=ReadInteger(Ident+'Left',Default.Left);
  ARect.Top:=ReadInteger(Ident+'Top',Default.Top);
  ARect.Right:=ReadInteger(Ident+'Right',Default.Right);
  ARect.Bottom:=ReadInteger(Ident+'Bottom',Default.Bottom);
end;

procedure TDBConfig.WriteRect(const Ident: string; const Value: TRect);
begin
  WriteInteger(Ident+'Left',Value.Left);
  WriteInteger(Ident+'Top',Value.Top);
  WriteInteger(Ident+'Right',Value.Right);
  WriteInteger(Ident+'Bottom',Value.Bottom);
end;

function TDBConfig.ReadInteger(const Ident: string; DefaultValue: Longint
  ): Longint;
begin
  Result:=StrToIntDef(ReadString(Section,Ident,IntToStr(DefaultValue)),DefaultValue);
end;

procedure TDBConfig.WriteInteger(const Ident: string; Value: Longint);
begin
  WriteString(Section,Ident,IntToStr(Value))
end;

function TDBConfig.ReadBoolean(const Ident: string; DefaultValue: Boolean
  ): Boolean;
begin
  Result := ReadInteger(Ident, Ord(DefaultValue)) <> Ord(False);
end;

procedure TDBConfig.WriteBoolean(const Ident: string; Value: Boolean);
begin
  WriteInteger(Ident, Ord(Value));
end;

procedure TBaseDBModule.DeleteExpiredSessions;
begin
end;
function TBaseDBModule.SetProperties(aProp: string; Connection: TComponent
  ): Boolean;
begin
  FTables.Clear;
end;
function TBaseDBModule.CreateDBFromProperties(aProp: string): Boolean;
begin
  Result := False;
end;
function TBaseDBModule.ProcessTerm(aTerm: string): string;
var
  UseLike: Boolean;
  aFilter: String;
begin
  aFilter := aTerm;
  UseLike := (pos('*',aFilter) > 0) or (pos('?',aFilter) > 0);
  aFilter := StringReplace(aFilter,'*','%',[rfReplaceAll]);
  aFilter := StringReplace(aFilter,'?','_',[rfReplaceAll]);
  aFilter := StringReplace(aFilter,'= ''''','=''''',[rfReplaceAll]);
  aFilter := StringReplace(aFilter,'=''''',' is NULL',[rfReplaceAll]);
  if UseLike then
    aFilter := StringReplace(aFilter,'=',' like ',[rfReplaceAll]);
  Result := aFilter;
end;

function TBaseDBModule.GetLimitAfterSelect: Boolean;
begin
  Result := False;
end;

function TBaseDBModule.GetLimitSTMT: string;
begin
  Result := '';
end;

constructor TBaseDBModule.Create(AOwner: TComponent);
begin
  FIgnoreOpenrequests := False;
  FCheckedTables := TStringList.Create;
  FTables := TStringList.Create;
  FTriggers := TStringList.Create;
  Users := TUser.CreateEx(nil,Self);
  Numbers := TNumberSets.CreateEx(nil,Self);
  MandantDetails := TMandantDetails.CreateEx(nil,Self);
  Tree := TTree.CreateEx(nil,Self);
  Forms := TForms.CreateEx(nil,Self);
  UserFieldDefs := TUserFieldDefs.CreateEx(nil,Self);
  Filters := TFilters.CreateEx(nil,Self);
  Reports := TReports.CreateEx(nil,Self);
  ActiveUsers := TActiveUsers.CreateEx(nil,Self);
  Permissions := TPermissions.CreateEx(nil,Self);
  StorageTypes := TStorageTypes.CreateEx(nil,Self);
  Currency := TCurrency.CreateEx(nil,Self);
  PaymentTargets := TPaymentTargets.CreateEx(nil,Self);
  StorageType := TStorageTyp.CreateEx(nil,Self);
  StorageJournal := TStorageJournal.CreateEx(nil,Self);
  Countries := TCountries.CreateEx(nil,Self);
  Languages := TLanguages.CreateEx(nil,Self);
  States := TStates.CreateEx(nil,Self);
  Categories := TCategory.CreateEx(nil,Self);
  DeletedItems := TDeletedItems.CreateEx(nil,Self);
  ProcessClient := TProcessClient.CreateEx(nil,Self);
  TableVersions := TTableVersions.CreateEx(nil,Self);
end;
destructor TBaseDBModule.Destroy;
begin
  TableVersions.Destroy;
  FCheckedTables.Destroy;
  FTables.Free;
  FTriggers.Free;
  Users.Destroy;
  Numbers.Destroy;
  MandantDetails.Destroy;
  Tree.Destroy;
  Forms.Destroy;
  UserFieldDefs.Destroy;
  Filters.Destroy;
  Reports.Destroy;
  Permissions.Destroy;
  StorageTypes.Destroy;
  Currency.Destroy;
  StorageType.Destroy;
  StorageJournal.Destroy;
  States.Destroy;
  Categories.Destroy;
  DeletedItems.Destroy;
  Languages.Destroy;
  Countries.Destroy;
  PaymentTargets.Destroy;
  ProcessClient.Destroy;
  ActiveUsers.Destroy;
  inherited Destroy;
end;
function TBaseDBModule.GetNewDataSet(aSQL: string; aConnection: TComponent;
  MasterData : TDataSet = nil;aOrigtable : TBaseDBDataSet = nil): TDataSet;
begin
  raise Exception.Create(strNotSupported);
end;
procedure TBaseDBModule.BlobFieldToFile(DataSet: TDataSet; Fieldname: string;
  Filename: string);
var
  fstream: TFileStream;
begin
  fstream := TFileStream.Create(UniToSys(Filename),fmCreate);
  try
    BlobFieldToStream(DataSet,Fieldname,fstream);
  except
    fStream.Free;
    raise;
  end;
  fstream.Free;
end;
procedure TBaseDBModule.FileToBlobField(Filename: string; DataSet: TDataSet;
  Fieldname: string);
var
  fstream: TFileStream;
begin
  fstream := TFileStream.Create(UniToSys(Filename),fmOpenRead);
  try
    StreamToBlobField(fstream,DataSet,Fieldname);
  except
    fstream.Free;
    raise;
  end;
  fstream.Free;
end;
const
  ChunkSize: Longint = 16384; { copy in 8K chunks }
procedure TBaseDBModule.StreamToBlobField(Stream: TStream; DataSet: TDataSet;
  Fieldname: string);
var
  Edited: Boolean;
  dStream: TStream;
  pBuf    : Pointer;
  cnt: LongInt;
  totCnt: LongInt=0;
begin
  Edited := False;
  if (DataSet.State <> dsEdit) and (DataSet.State <> dsInsert) then
    begin
      DataSet.Edit;
      Edited := True;
    end;
  dStream := DataSet.CreateBlobStream(DataSet.FieldByName(Fieldname),bmWrite);
  try
    GetMem(pBuf, ChunkSize);
    try
      cnt := Stream.Read(pBuf^, ChunkSize);
      cnt := dStream.Write(pBuf^, cnt);
      totCnt := totCnt + cnt;
      {Loop the process of reading and writing}
      while (cnt > 0) do
        begin
          {Read bufSize bytes from source into the buffer}
          cnt := Stream.Read(pBuf^, ChunkSize);
          {Now write those bytes into destination}
          cnt := dStream.Write(pBuf^, cnt);
          {Increment totCnt for progress and do arithmetic to update the gauge}
          totcnt := totcnt + cnt;
        end;
    finally
      FreeMem(pBuf, ChunkSize);
    end;
  finally
    dStream.Free;
  end;
  if Edited then
    DataSet.Post;
end;
procedure TBaseDBModule.BlobFieldToStream(DataSet: TDataSet; Fieldname: string;
  dStream: TStream);
var
  pBuf    : Pointer;
  cnt: LongInt;
  totCnt: LongInt=0;
  Stream: TStream;
begin
  Stream := DataSet.CreateBlobStream(DataSet.FieldByName(Fieldname),bmRead);
  try
    GetMem(pBuf, ChunkSize);
    try
      cnt := Stream.Read(pBuf^, ChunkSize);
      cnt := dStream.Write(pBuf^, cnt);
      totCnt := totCnt + cnt;
      {Loop the process of reading and writing}
      while (cnt > 0) do
        begin
          {Read bufSize bytes from source into the buffer}
          cnt := Stream.Read(pBuf^, ChunkSize);
          {Now write those bytes into destination}
          cnt := dStream.Write(pBuf^, cnt);
          {Increment totCnt for progress and do arithmetic to update the gauge}
          totcnt := totcnt + cnt;
        end;
    finally
      FreeMem(pBuf, ChunkSize);
    end;
  finally
    Stream.Free;
  end;
end;
function TBaseDBModule.QuoteField(aField: string): string;
begin
  Result := '"'+aField+'"';
end;
function TBaseDBModule.QuoteValue(aValue: string): string;
begin
  Result := ''''+StringReplace(aValue,'''','''''',[rfReplaceAll])+'''';
end;
function TBaseDBModule.EscapeString(aValue: string): string;
begin
  Result := StringReplace(aValue,'''','',[rfReplaceAll]);
end;
function TBaseDBModule.DateToFilter(aValue: TDateTime): string;
begin
  Result := QuoteValue(FormatDateTime('YYYY-MM-DD',aValue));
end;
function TBaseDBModule.DateTimeToFilter(aValue: TDateTime): string;
begin
  Result := QuoteValue(FormatDateTime('YYYY-MM-DD HH:MM:SS',aValue));
end;
function TBaseDBModule.GetLinkDesc(aLink: string): string;
var
  tmp1: String;
  tmp2: String;
  tmp3: String;
  tmp4: String;
  Desc: String = '';
  aTable: TDataSet;
  aTmp: String;
  aTmp1: String;
  aTmp2: String;
begin
  if (pos('@',aLink) = 0) and (pos('://',aLink) = 0) then
    begin
      Result := aLink;
      exit;
    end;
  if rpos('{',aLink) > 0 then
    begin
      Desc := copy(aLink,rpos('{',aLink)+1,length(aLink));
      if rpos('}',Desc) > 0 then
        Desc := copy(Desc,0,rpos('}',Desc)-1);
    end
  else if rpos('(',aLink) > 0 then
    begin
      Desc := copy(aLink,rpos('(',aLink)+1,length(aLink));
      if rpos(')',Desc) > 0 then
        Desc := copy(Desc,0,rpos(')',Desc)-1);
    end;
  if rpos('{',aLink) > 0 then
    aLink := copy(aLink,0,rpos('{',aLink)-1)
  else if rpos('(',aLink) > 0 then
    aLink := copy(aLink,0,rpos('(',aLink)-1);
  if pos('://',aLink) > 0 then
    begin
      Result := strWebsite;
    end
  else if copy(aLink, 0, pos('@', aLink) - 1) = 'MASTERDATA' then
    begin
      aLink   := copy(aLink, pos('@', aLink) + 1, length(aLink));
      tmp1 := copy(aLink, 0, pos('&&', aLink) - 1);
      aLink   := copy(aLink, pos('&&', aLink) + 2, length(aLink));
      tmp2 := copy(aLink, 0, pos('&&', aLink) - 1);
      aLink   := copy(aLink, pos('&&', aLink) + 2, length(aLink));
      tmp3 := aLink;
      Result := strMasterdata+' '+tmp1;
      if tmp2 <> '' then
        Result := Result+' '+tmp2;
    end
  else if copy(aLink, 0, pos('@', aLink) - 1) = 'CUSTOMERS' then
    begin
      Result := strContact+' '+copy(aLink, pos('@', aLink) + 1, length(aLink));
    end
  else if copy(aLink, 0, pos('@', aLink) - 1) = 'DOCUMENTS' then
    begin
      aLink   := copy(aLink, pos('@', aLink) + 1, length(aLink));
      tmp1 := trim(copy(aLink, 0, pos('&&', aLink) - 1));
      aLink   := copy(aLink, pos('&&', aLink) + 2, length(aLink));
      tmp2 := trim(copy(aLink, 0, pos('&&', aLink) - 1));
      aLink   := copy(aLink, pos('&&', aLink) + 2, length(aLink));
      tmp3 := trim(copy(aLink, 0, pos('&&', aLink) - 1));
      aLink   := copy(aLink, pos('&&', aLink) + 2, length(aLink));
      tmp4 := trim(aLink);
      Result := strFile+' '+tmp4;
    end
  else if copy(aLink, 0, pos('@', aLink) - 1) = 'ORDERS' then
    begin
      if IsSQLDB then
        begin
          aTable := GetNewDataSet('select "SQL_ID","STATUS","CUSTNAME" from "ORDERS" where "ORDERNO"='+QuoteValue(copy(aLink, pos('@', aLink) + 1, length(aLink))));
          aTable.Open;
          aTmp := aTable.FieldByName('SQL_ID').AsString;
          aTmp1 := aTable.FieldByName('STATUS').AsString;
          aTmp2 := trim(aTable.FieldByName('CUSTNAME').AsString);
          FreeAndNil(aTable);
          aTable := GetNewDataSet('select "STATUSNAME" from "ORDERTYPE" where "STATUS"='+QuoteValue(aTmp1));
          aTable.Open;
          Result := aTable.FieldByName('STATUSNAME').AsString+' '+copy(aLink, pos('@', aLink) + 1, length(aLink));
          if aTmp2<>'' then
            result := result+' '+strFor+' '+aTmp2;
          FreeAndNil(aTable);
        end
      else
        Result := strOrder+' '+copy(aLink, pos('@', aLink) + 1, length(aLink));
    end
  else if copy(aLink, 0, pos('@', aLink) - 1) = 'CALLS' then
    begin
      Result := strCall+' '+copy(aLink, pos('@', aLink) + 1, length(aLink));
    end
  else if copy(aLink, 0, pos('@', aLink) - 1) = 'MESSAGEIDX' then
    begin
      Result := strMessage+' '+copy(aLink, pos('@', aLink) + 1, length(aLink));
    end
  else if copy(aLink, 0, pos('@', aLink) - 1) = 'WIKI' then
    begin
      Result := strWikiPage+' '+copy(aLink, pos('@', aLink) + 1, length(aLink));
    end
  else if copy(aLink, 0, pos('@', aLink) - 1) = 'PROJECTS' then
    begin
      {
      with BaseApplication as IBaseDbInterface do
        aTable := Data.GetNewDataSet('select "ID" from "'+copy(aLink, 0, pos('@', aLink) - 1)+'" where "SQL_ID"='+Data.QuoteValue(copy(aLink, pos('@', aLink) + 1, length(aLink))));
      aTable.Open;
      Result := aTable.FieldByName('ID').AsString;
      FreeAndNil(aTable);
      }
    end
  else if copy(aLink, 0, pos('@', aLink) - 1) = 'PROJECTS.ID' then
    begin
      Result := strProjectProcess+' '+copy(aLink, pos('@', aLink) + 1, length(aLink));
    end;
  if (Desc <> '') and (Result <> '') then
    Result := Desc+' ('+Result+')'
  else if (Desc <> '') then
    Result := Desc;
  if Result = '' then result := aLink;
end;
function TBaseDBModule.GetLinkLongDesc(aLink: string): string;
var
  aTable: TDataSet;
  i: Integer;
  aTmp: String;
  aWiki: TWikiList;
  aID: String;
  aTmp1: String;
  aBaseHist: TBaseHistory;
begin
  Result := '';
  with BaseApplication as IBaseDbInterface do
    begin
      if not Data.IsSQLDb then exit;
      if rpos('{',aLink) > 0 then
        aLink := copy(aLink,0,rpos('{',aLink)-1)
      else if rpos('(',aLink) > 0 then
        aLink := copy(aLink,0,rpos('(',aLink)-1);
      if copy(aLink, 0, pos('@', aLink) - 1) = 'MASTERDATA' then
        begin
          aID := copy(aLink, pos('@', aLink) + 1, length(aLink));
          if pos('&',aID) > 0 then
            aID := copy(aID,0,pos('&',aID)-1);
          aTable := Data.GetNewDataSet('select '+Data.QuoteField('SQL_ID')+' from '+Data.QuoteField('MASTERDATA')+' where '+Data.QuoteField('ID')+'='+Data.QuoteValue(aID));
          aTable.Open;
          aTmp := aTable.FieldByName('SQL_ID').AsString;
          FreeAndNil(aTable);
          if aTmp <> '' then
            begin
              aTable := Data.GetNewDataSet('select "TEXT" from "TEXTS" where "REF_ID"='+Data.QuoteValue(aTmp));
              aTable.Open;
              if aTable.RecordCount > 0 then
                begin
                  Result := RTF2Plain(aTable.FieldByName('TEXT').AsString);
                end;
            end;
          FreeAndNil(aTable);
        end
      else if (copy(aLink, 0, pos('@', aLink) - 1) = 'CUSTOMERS')
           or (copy(aLink, 0, pos('@', aLink) - 1) = 'CUSTOMERS.ID') then
        begin
          if (copy(aLink, 0, pos('@', aLink) - 1) = 'CUSTOMERS') then
            begin
              aTable := Data.GetNewDataSet('select "SQL_ID" from "CUSTOMERS" where "ACCOUNTNO"='+Data.QuoteValue(copy(aLink, pos('@', aLink) + 1, length(aLink))));
              aTable.Open;
              aTmp := aTable.FieldByName('SQL_ID').AsString;
              FreeAndNil(aTable);
            end
          else aTmp := copy(aLink, pos('@', aLink) + 1, length(aLink));
          aTable := Data.GetNewDataSet('select "NAME","ADDRESS","ZIP","CITY" from "ADDRESSES" where "REF_ID"='+Data.QuoteValue(aTmp));
          aTable.Open;
          if aTable.RecordCount > 0 then
            begin
              Result := lineending+aTable.FieldByName('NAME').AsString+lineending+aTable.FieldByName('ADDRESS').AsString+lineending+aTable.FieldByName('ZIP').AsString+' '+aTable.FieldByName('CITY').AsString;
            end;
          FreeAndNil(aTable);
        end
      else if copy(aLink, 0, pos('@', aLink) - 1) = 'DOCUMENTS' then
      else if copy(aLink, 0, pos('@', aLink) - 1) = 'ORDERS' then
        begin
          aTable := Data.GetNewDataSet('select "SQL_ID","STATUS" from "ORDERS" where "ORDERNO"='+Data.QuoteValue(copy(aLink, pos('@', aLink) + 1, length(aLink))));
          aTable.Open;
          aTmp := aTable.FieldByName('SQL_ID').AsString;
          aTmp1 := aTable.FieldByName('STATUS').AsString;
          FreeAndNil(aTable);
          aTable := Data.GetNewDataSet('select "NAME","ADDRESS","ZIP","CITY" from "ORDERADDR" where "REF_ID"='+Data.QuoteValue(aTmp));
          aTable.Open;
          if aTable.RecordCount > 0 then
            begin
              Result := lineending+aTable.FieldByName('NAME').AsString+','+aTable.FieldByName('ADDRESS').AsString+','+aTable.FieldByName('ZIP').AsString+' '+aTable.FieldByName('CITY').AsString;
            end;
          FreeAndNil(aTable);
          aTable := Data.GetNewDataSet('select "POSNO","IDENT","SHORTTEXT","QUANTITY","QUANTITYU" from "ORDERPOS" where "REF_ID"='+Data.QuoteValue(aTmp));
          aTable.Open;
          i := 0;
          aTable.First;
          if aTable.RecordCount > 0 then
            Result := Result+lineending;
          while not aTable.EOF do
            begin
              if aTable.FieldByName('QUANTITYU').AsString = '' then
                Result := Result+lineending+aTable.FieldByName('QUANTITY').AsString+' x '+aTable.FieldByName('IDENT').AsString+' '+aTable.FieldByName('SHORTTEXT').AsString
              else
                Result := Result+lineending+aTable.FieldByName('QUANTITY').AsString+' '+aTable.FieldByName('QUANTITYU').AsString+' '+aTable.FieldByName('IDENT').AsString+' '+aTable.FieldByName('SHORTTEXT').AsString;
              aTable.Next;
              inc(i);
              if i > 3 then
                begin
                  Result := Result+lineending+'...';
                  break;
                end;
            end;
          FreeAndNil(aTable);
        end
      else if copy(aLink, 0, pos('@', aLink) - 1) = 'CALLS' then
      else if copy(aLink, 0, pos('@', aLink) - 1) = 'MESSAGEIDX' then
      else if copy(aLink, 0, pos('@', aLink) - 1) = 'CALENDAR' then
      else if copy(aLink, 0, pos('@', aLink) - 1) = 'ACCOUNTEXCHANGE' then
      else if copy(aLink, 0, pos('@', aLink) - 1) = 'PROJECTS' then
        begin
          aTable := Data.GetNewDataSet('select "NAME","DESCRIPTION" from "'+copy(aLink, 0, pos('@', aLink) - 1)+'" where "SQL_ID"='+Data.QuoteValue(copy(aLink, pos('@', aLink) + 1, length(aLink))));
          aTable.Open;
          Result := aTable.FieldByName('NAME').AsString+LineEnding+LineEnding+aTable.FieldByName('DESCRIPTION').AsString;
          FreeAndNil(aTable);
        end
      else if copy(aLink, 0, pos('@', aLink) - 1) = 'PROJECTS.ID' then
        begin
          aTable := Data.GetNewDataSet('select "NAME","DESCRIPTION" from "PROJECTS" where "ID"='+Data.QuoteValue(copy(aLink, pos('@', aLink) + 1, length(aLink))));
          aTable.Open;
          Result := aTable.FieldByName('NAME').AsString+LineEnding+LineEnding+aTable.FieldByName('DESCRIPTION').AsString;
          FreeAndNil(aTable);
        end
      else if copy(aLink, 0, pos('@', aLink) - 1) = 'TASKS' then
        begin
          aTable := Data.GetNewDataSet('select "DESC" from "'+copy(aLink, 0, pos('@', aLink) - 1)+'" where "SQL_ID"='+Data.QuoteValue(copy(aLink, pos('@', aLink) + 1, length(aLink))));
          aTable.Open;
          Result := aTable.FieldByName('DESC').AsString;
          FreeAndNil(aTable);
        end
      else if copy(aLink, 0, pos('@', aLink) - 1) = 'WIKI' then
        begin
          if IsNumeric(copy(aLink, pos('@', aLink) + 1, length(aLink))) then
            begin
              aTable := Data.GetNewDataSet('select "DATA" from "'+copy(aLink, 0, pos('@', aLink) - 1)+'" where "SQL_ID"='+Data.QuoteValue(copy(aLink, pos('@', aLink) + 1, length(aLink))));
              aTable.Open;
              Result := aTable.FieldByName('DATA').AsString;
            end
          else
            begin
              aTable := nil;
              aWiki := TWikiList.CreateEx(Self,Data);
              if aWiki.FindWikiPage(copy(aLink, pos('@', aLink) + 1, length(aLink))) then
                Result := aWiki.PageAsText;
              aWiki.Free;
            end;
          FreeAndNil(aTable);
        end
      else if copy(aLink, 0, pos('@', aLink) - 1) = 'HISTORY' then
        begin
          aBaseHist := TBaseHistory.CreateEx(nil,Data);
          aBaseHist.SelectFromLink(aLink);
          aBaseHist.Open;
          Result := GetLinkLongDesc(aBaseHist.FieldByName('OBJECT').AsString);
          aBaseHist.Free;
        end;
      if length(Result) > 300 then
        result := copy(Result,0,300)+lineending+' ...';
    end;
end;
function TBaseDBModule.GetLinkIcon(aLink: string;GetRealIcon : Boolean = False): Integer;
var
  aObjs: TObjects;
begin
  Result := -1;
  if pos('://',aLink) > 0 then
    Result := IMAGE_WEBSITE
  else if copy(aLink, 0, pos('@', aLink) - 1) = 'MASTERDATA' then
    Result := IMAGE_MASTERDATA
  else if copy(aLink, 0, pos('@', aLink) - 1) = 'ALLOBJECTS' then
    begin
      Result := 121;
      if GetRealIcon then
        begin
          aObjs := TObjects.Create(nil);
          aObjs.SelectByLink(aLink);
          aObjs.Open;
          if aObjs.Count>0 then
            Result := aObjs.FieldByName('ICON').AsInteger;
          aObjs.Free;
        end;
    end
  else if (copy(aLink, 0, pos('@', aLink) - 1) = 'CUSTOMERS')
       or (copy(aLink, 0, pos('@', aLink) - 1) = 'CUSTOMERS.ID') then
    Result := IMAGE_PERSON
  else if copy(aLink, 0, pos('@', aLink) - 1) = 'DOCUMENTS' then
    Result := IMAGE_DOCUMENTS
  else if copy(aLink, 0, pos('@', aLink) - 1) = 'ORDERS' then
    Result := IMAGE_ORDERS
  else if copy(aLink, 0, pos('@', aLink) - 1) = 'CALLS' then
    Result := IMAGE_CALLS
  else if copy(aLink, 0, pos('@', aLink) - 1) = 'MESSAGEIDX' then
    Result := IMAGE_MESSAGE
  else if copy(aLink, 0, pos('@', aLink) - 1) = 'CALENDAR' then
    Result := IMAGE_CALENDAR
  else if copy(aLink, 0, pos('@', aLink) - 1) = 'ACCOUNTEXCHANGE' then
    Result := IMAGE_FINANCIAL
  else if (copy(aLink, 0, pos('@', aLink) - 1) = 'PROJECTS')
       or (copy(aLink, 0, pos('@', aLink) - 1) = 'PROJECTS.ID') then
    Result := IMAGE_PROJECT
  else if (copy(aLink, 0, pos('@', aLink) - 1) = 'TASKS') then
    Result := IMAGE_TASK
  else if (copy(aLink, 0, pos('@', aLink) - 1) = 'WIKI') then
    Result := IMAGE_WIKI
  else if (copy(aLink, 0, pos('@', aLink) - 1) = 'STATISTICS') then
    Result := IMAGE_STATISTIC
  else if (copy(aLink, 0, pos('@', aLink) - 1) = 'SCRIPTS') then
    Result := 62
  ;
end;
function TBaseDBModule.BuildLink(aDataSet: TDataSet): string;
var
  aTable : TDataSet;
begin
  with aDataSet as IBaseMAnageDB do
    Result := TableName + '@';
  if not aDataSet.Active then exit;
  if (Result = 'MASTERDATA@') then
    begin
      if (aDataSet.FieldDefs.IndexOf('ID')>0)
      and (aDataSet.FieldDefs.IndexOf('VERSION')>0)
      and (aDataSet.FieldDefs.IndexOf('LANGUAGE')>0)
      and (aDataSet.FieldDefs.IndexOf('SHORTTEXT')>0)
      then
        begin
          Result := Result + aDataSet.FieldByName('ID').AsString + '&&';
          Result := Result + aDataSet.FieldByName('VERSION').AsString + '&&';
          Result := Result + aDataSet.FieldByName('LANGUAGE').AsString;
        end
      else
        Result := 'MASTERDATA.ID@'+ aDataSet.FieldByName('SQL_ID').AsString;
      if (aDataSet.FieldDefs.IndexOf('SHORTTEXT')>0) then
        Result := result+'{'+aDataSet.FieldByName('SHORTTEXT').AsString+'}';
    end
  else  if (Result = 'MDPOSITIONS@') then
    begin
      Result := 'MASTERDATA@';
      Result := Result + aDataSet.FieldByName('IDENT').AsString + '&&';
      Result := Result + aDataSet.FieldByName('VERSION').AsString + '&&';
      Result := Result + aDataSet.FieldByName('LANGUAGE').AsString;
      Result := Result+'{'+aDataSet.FieldByName('SHORTTEXT').AsString+'}';
    end
  else  if (Result = 'CUSTOMERS@') then
    begin
      if aDataSet.FieldDefs.IndexOf('ACCOUNTNO') = -1 then
        begin
          Result := 'CUSTOMERS.ID@'+ aDataSet.FieldByName('SQL_ID').AsString;
        end
      else
        Result := Result + aDataSet.FieldByName('ACCOUNTNO').AsString;
      Result := result+'{'+aDataSet.FieldByName('NAME').AsString+'}';
    end
  else  if (Result = 'CUSTOMERCONT@') then
    begin
      Result := 'CUSTOMERS.ID@'+ aDataSet.FieldByName('REF_ID').AsString;
      if IsSQLDb then
        begin
          aTable := GetNewDataSet('select "NAME" from "CUSTOMERS" where "SQL_ID"='+QuoteValue(aDataSet.FieldByName('REF_ID').AsString));
          aTable.Open;
          if aTable.RecordCount > 0 then
            Result := result+'{'+aTable.FieldByName('NAME').AsString+'}';
          aTable.Free;
        end;
    end
  else  if (Result = 'ADDRESSES@') then
    begin
      Result := 'CUSTOMERS.ID@'+ aDataSet.FieldByName('REF_ID').AsString;
      if IsSQLDb then
        begin
          aTable := GetNewDataSet('select "NAME" from "CUSTOMERS" where "SQL_ID"='+QuoteValue(aDataSet.FieldByName('REF_ID').AsString));
          aTable.Open;
          if aTable.RecordCount > 0 then
            Result := result+'{'+aTable.FieldByName('NAME').AsString+'}';
          aTable.Free;
        end;
    end
  else  if (Result = 'DOCUMENTS@') then
    begin
      if aDataSet.FieldDefs.IndexOf('ID') > -1 then
        begin
          Result := Result + aDataSet.FieldByName('TYPE').AsString + '&&';
          Result := Result + aDataSet.FieldByName('ID').AsString + '&&';
          Result := Result + aDataSet.FieldByName('VERSION').AsString + '&&';
          Result := Result + aDataSet.FieldByName('NUMBER').AsString;
          Result := result+'{'+aDataSet.FieldByName('NAME').AsString+'}';
        end
      else
        begin
          Result := 'DOCUMENTS.ID@' + aDataSet.FieldByName('NUMBER').AsString;
          Result := result+'{'+aDataSet.FieldByName('NAME').AsString+'}';
        end;
    end
  else  if (Result = 'ORDERS@')
  then
    begin
      Result := Result + aDataSet.FieldByName('ORDERNO').AsString;
//      if aDataSet is TOrder then
//        Result := Result+'{'+TOrder(aDataSet).OrderType.FieldByName('STATUSNAME').AsString+' '+aDataSet.FieldByName('NUMBER').AsString+'}'
//      else
        Result := result+'{'+aDataSet.FieldByName('NUMBER').AsString+'}';
    end
  else if (Result = 'CALLS@') then
    begin
      Result := Result + aDataSet.FieldByName('ID').AsString;
    end
  else  if (Result = 'MESSAGEIDX@') then
    begin
      if copy(aDataSet.FieldByName('ID').AsString,0,10) = 'DOCUMENTS@' then
        Result := aDataSet.FieldByName('ID').AsString
      else
        begin
          Result := Result + aDataSet.FieldByName('ID').AsString;
          Result := result+'{'+aDataSet.FieldByName('SUBJECT').AsString+'}';
        end;
    end
  else if (Result = 'CALENDAR@') then
    begin
      Result := Result + aDataSet.FieldByName('ID').AsString;
    end
  else  if (Result = 'ACCOUNTEXCHANGE@') then
    begin
      Result := Result + aDataSet.FieldByName('REF_ID').AsString + '&&';
      Result := Result + aDataSet.FieldByName('SQL_ID').AsString;
      Result := result+'{'+strAccountexchange+' '+aDataSet.FieldByName('NAME').AsString+'}';
    end
  else  if (Result = 'PROJECTS@') then
    begin
      if (aDataSet.FieldByName('ID').AsString <> '') and (aDataSet.FieldByName('ID').AsString <> '0') then
        begin
          Result := 'PROJECTS.ID@';
          Result := Result + aDataSet.FieldByName('ID').AsString;
          Result := result+'{'+aDataSet.FieldByName('NAME').AsString+'}';
        end
      else
        begin
          Result := Result + aDataSet.FieldByName('SQL_ID').AsString;
          Result := result+'{'+aDataSet.FieldByName('NAME').AsString+'}';
        end;
    end
  else  if (Result = 'WIKI@') then
    begin
      Result := aDataSet.FieldByName('NAME').AsString;
      with BaseApplication as IBaseDBInterface do
        begin
          Data.SetFilter(Data.Tree,'',0,'','ASC',False,True,True);
          if Data.Tree.DataSet.Locate(Data.Tree.Id.FieldName,aDataSet.FieldByName('TREEENTRY').AsVariant,[]) then
            begin
              Result := Data.Tree.FieldByName('NAME').AsString+'/'+Result;
              while Data.Tree.DataSet.Locate(Data.Tree.Id.FieldName,Data.Tree.FieldByName('PARENT').AsVariant,[]) do
                Result := Data.Tree.FieldByName('NAME').AsString+'/'+Result;
            end;
        end;
      if trim(aDataSet.FieldByName('CAPTION').AsString) <> '' then
        Result := 'WIKI@'+Result+'{'+aDataSet.FieldByName('CAPTION').AsString+'}'
      else
        Result := 'WIKI@'+Result+'{'+aDataSet.FieldByName('NAME').AsString+'}'
    end
  else if aDataSet.FieldDefs.IndexOf('SQL_ID')>-1 then
    begin
      Result := Result+aDataSet.FieldByName('SQL_ID').AsString;
      if aDataSet.FieldDefs.IndexOf('SUBJECT') > -1 then
        Result := Result+'{'+aDataSet.FieldByName('SUBJECT').AsString+'}'
      else  if aDataSet.FieldDefs.IndexOf('NAME') > -1 then
        Result := Result+'{'+aDataSet.FieldByName('NAME').AsString+'}'
      else  if aDataSet.FieldDefs.IndexOf('SUMMARY') > -1 then
        Result := Result+'{'+aDataSet.FieldByName('SUMMARY').AsString+'}'
      ;
    end;
  if (length(Result)>200) and (pos('{',Result)>0) then
    Result := copy(Result,0,190)+'}';
  Result := StringReplace(Result,'{}','',[]);
  if copy(Result,length(Result),1)='@' then
    Result := '';
end;
function TBaseDBModule.GotoLink(const aLink: string): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to length(FLinkHandlers)-1 do
    if copy(aLink,0,length(FLinkHandlers[i].aLinkType)) = FLinkHandlers[i].aLinkType then
      begin
        if Assigned(FLinkHandlers[i].aEvent) then
          Result := FLinkHandlers[i].aEvent(aLink,Self);
        break;
      end;
end;
function TBaseDBModule.DataSetFromLink(aLink: string;
  var aClass: TBaseDBDatasetClass): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to length(FLinkHandlers)-1 do
    if copy(Uppercase(aLink),0,length(FLinkHandlers[i].aLinkType)) = Uppercase(FLinkHandlers[i].aLinkType) then
      begin
        if Assigned(FLinkHandlers[i].aClass) then
          begin
            aClass := FLinkHandlers[i].aClass;
            Result := True;
          end;
        break;
      end;
end;

function TBaseDBModule.ListDataSetFromLink(aLink: string;
  var aClass: TBaseDBDatasetClass): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to length(FLinkHandlers)-1 do
    if copy(aLink,0,length(FLinkHandlers[i].aLinkType)) = FLinkHandlers[i].aLinkType then
      begin
        if Assigned(FLinkHandlers[i].aClass) then
          begin
            aClass := FLinkHandlers[i].aClass;
            if Assigned(FLinkHandlers[i].aListClass) then
              aClass := FLinkHandlers[i].aListClass;
            Result := True;
          end;
        break;
      end;
end;

procedure TBaseDBModule.RegisterLinkHandler(aLink: string;
  aOpenHandler: TOpenLinkEvent; DataSetClass: TBaseDBDatasetClass;
  DataSetListClass: TBaseDBDatasetClass);
var
  i: Integer;
begin
  for i := 0 to length(FLinkHandlers)-1 do
    if FLinkHandlers[i].aLinkType=aLink then
      with FLinkHandlers[i] do
        begin
          aEvent := aOpenHandler;
          aClass := DatasetClass;
          aListClass := DataSetListClass;
          exit;
        end;
  Setlength(FLinkHandlers,length(FLinkHandlers)+1);
  with FLinkHandlers[length(FLinkHandlers)-1] do
    begin
      aLinkType :=aLink;
      aEvent := aOpenHandler;
      aClass := DatasetClass;
      aListClass := DataSetListClass;
    end;
end;
function TBaseDBModule.GetBookmark(aDataSet: TBaseDbDataSet): Variant;
begin
  Result := aDataSet.GetBookmark;
end;
function TBaseDBModule.GotoBookmark(aDataSet: TBaseDbDataSet; aRec: Variant
  ): Boolean;
begin
  Result := aDataSet.GotoBookmark(aRec);
end;
function TBaseDBModule.Locate(aDataSet: TBaseDbDataSet;
  const keyfields: string; const keyvalues: Variant; aoptions: TLocateOptions
  ): boolean;
begin
  Result := aDataSet.DataSet.Locate(keyfields,keyvalues,aoptions);
end;

function TBaseDBModule.GetErrorNum(e: EDatabaseError): Integer;
begin
  Result := -1;
end;

function TBaseDBModule.RecordCount(aDataSet: TBaseDbDataSet): Integer;
begin
  Result := aDataSet.Count;
end;
function TBaseDBModule.DeleteItem(aDataSet: TBaseDBDataSet): Boolean;
begin
  if not Assigned(aDataSet) then exit;
  if aDataSet.DataSet.FieldDefs.IndexOf('SQL_ID') > -1 then
    begin
      DeletedItems.Select(aDataSet.Id.AsVariant);
      DeletedItems.Open;
      if DeletedItems.Count = 0 then
        begin
          DeletedItems.DataSet.Append;
          DeletedItems.FieldByName('REF_ID_ID').AsVariant:=aDataSet.Id.AsVariant;
          DeletedItems.FieldByName('LINK').AsString := Self.BuildLink(aDataSet.DataSet);
          DeletedItems.DataSet.Post;
        end;
      DeletedItems.DataSet.Close;
    end;
end;
function TBaseDBModule.ShouldCheckTable(aTableName : string;SetChecked : Boolean = True): Boolean;
begin
  Result := FCheckedTables.IndexOf(aTableName) = -1;
  try
  if Result then
    begin
      TableVersions.Filter('');
      with BaseApplication as IBaseApplication do
        begin
          if TableVersions.Locate('NAME',aTableName,[]) then
            if (TableVersions.FieldByName('DBVERSION').AsInteger>=round((AppVersion*10000)+AppRevision)) and (not BaseApplication.HasOption('debug')) and (TableExists(aTableName)) then
              begin
                Result := False;
              end
            else if (not BaseApplication.HasOption('debug')) then
              begin
                with BaseApplication as IBaseApplication do
                  Debug('Table "'+aTableName+'" DBVersion '+TableVersions.FieldByName('DBVERSION').AsString+'<'+IntToStr(round((AppVersion*10000)+AppRevision)));
              end;
        end;
    end;
  except
  end;
  if (not Result) and SetChecked and TableExists(aTableName) then
    FCheckedTables.Add(aTableName);
end;

procedure TBaseDBModule.UpdateTableVersion(aTableName: string);
var
  i: Integer;
begin
  try
    TableVersions.Filter('');
    with BaseApplication as IBaseApplication do
      begin
        if not TableVersions.Locate('NAME',aTableName,[]) then
          begin
            TableVersions.Insert;
            TableVersions.FieldByName('NAME').AsString:=aTableName;
          end;
        with BaseApplication as IBaseApplication do
          begin
            TableVersions.Edit;
            TableVersions.FieldByName('DBVERSION').AsInteger:=round(AppVersion*10000+AppRevision);
            TableVersions.Post;
          end;
      end;
  except
  end;
end;

function TBaseDBModule.RemoveCheckTable(aTableName: string): Boolean;
begin
  if FCheckedTables.IndexOf(aTableName) > -1 then
    FCheckedTables.Delete(FCheckedTables.IndexOf(aTableName));
  Tables.Clear;
end;
function TBaseDBModule.TriggerExists(aTriggerName: string;
  aConnection: TComponent; AllowLowercase: Boolean): Boolean;
begin
  Result := False;
end;
function TBaseDBModule.CreateTrigger(aTriggerName: string; aTableName: string;
  aUpdateOn: string; aSQL: string;aField : string = ''; aConnection: TComponent=nil): Boolean;
begin
  Result := False;
end;
function TBaseDBModule.CheckForInjection(aFilter: string): Boolean;
begin
  Result := False;
  if (pos('insert into',lowercase(aFilter)) > 0)
//  or (pos('update ',lowercase(aFilter)) > 0)
  or (pos('delete table',lowercase(aFilter)) > 0)
  or (pos('delete from',lowercase(aFilter)) > 0)
  or (pos('alter table',lowercase(aFilter)) > 0)
  or (pos('union select ',lowercase(aFilter)) > 0)
  or (pos('select if ',lowercase(aFilter)) > 0)
  or (pos(' into outfile',lowercase(aFilter)) > 0)
  or (pos(' into dumpfile',lowercase(aFilter)) > 0)
  then
    begin
      raise Exception.Create(strSQLInjection);
      Result := True;
    end;
end;

function TBaseDBModule.GetDBType: string;
begin
  Result := '';
end;

procedure TBaseDBModule.SetFilter(DataSet: TbaseDBDataSet; aFilter: string;
  aLimit: Integer; aOrderBy: string; aSortDirection: string;
  aLocalSorting: Boolean; aGlobalFilter: Boolean; aUsePermissions: Boolean;
  aFilterIn: string);
begin
  if CheckForInjection(aFilter) then exit;
  DataSet.FilterEx(aFilter,aLimit,aOrderBy,aSortDirection,aLocalSorting,aGlobalFilter,aUsePermissions,aFilterIn);
end;
procedure TBaseDBModule.AppendUserToActiveList;
begin
  ActiveUsers.Select(FSessionID);
  ActiveUsers.Open;
  FUsersFilter:='';
  try
    if ActiveUsers.GotoBookmark(FSessionID) then
      begin
        with ActiveUsers.DataSet do
          begin
            Edit;
            FieldByName('TIMESTAMPD').AsDateTime := Now();
            if FieldDefs.IndexOf('TIMESTAMPT') <> -1 then
              FieldByName('TIMESTAMPT').AsFloat    := Frac(Now());
            Post;
          end;
      end
    else
      begin
        with ActiveUsers.DataSet do
          begin
            Insert;
            if Users.DataSet.Active then
              begin
                FieldByName('ACCOUNTNO').AsString:=Users.FieldByName('ACCOUNTNO').AsString;
                FieldByName('NAME').AsString:=Users.FieldByName('NAME').AsString;
              end
            else
              FieldByName('NAME').AsString:=ExtractFileName(Paramstr(0));
            FieldByName('CLIENT').AsString:=ExtractFileName(Paramstr(0));
            FieldByName('HOST').AsString:=GetSystemName;
            with BaseApplication as IBaseApplication do
              FieldByName('VERSION').AsString:=StringReplace(Format('Version %f Build %d',[AppVersion,AppRevision]),',','.',[rfReplaceAll]);
            FieldByName('TIMESTAMPD').AsDateTime := Now();
            FieldByName('EXPIRES').AsDateTime := Now()+0.5;
            if FieldDefs.IndexOf('TIMESTAMPT') <> -1 then
              FieldByName('TIMESTAMPT').AsFloat    := Frac(Now());
            Post;
            FSessionID := ActiveUsers.Id.AsVariant;
          end;
      end;
    RefreshUsersFilter;
  except
  end;
  ActiveUsers.DataSet.Close;
  RegisterLinkHandlers;
end;

procedure TBaseDBModule.RefreshUsersFilter;
var
  aUser : Int64;
  aUsers : string;

  procedure RecursiveGetRight;
  begin
    aUsers := aUsers+' or '+QuoteField('PERMISSIONS')+'.'+QuoteField('USER')+'='+QuoteValue(Users.FieldByName('SQL_ID').AsString);
    if not Users.FieldByName('PARENT').IsNull then
      begin
        if Users.GotoBookmark(Users.FieldByName('PARENT').AsInteger) then
          RecursiveGetRight
      end;
  end;
begin
  if Users.DataSet.Active then
    begin
      aUser := Users.GetBookmark;
      aUsers := '';
      RecursiveGetRight;
      FUsersFilter:=copy(aUsers,4,length(aUsers));
      Users.GotoBookmark(aUser);
    end;
end;

procedure TBaseDBModule.RemoveUserFromActiveList;
begin
  if FIgnoreOpenRequests then exit;
  try

    with ActiveUsers.DataSet as IBaseManageDB do
      UpdateStdFields := False;
    if ActiveUsers.DataSet.Active then
      ActiveUsers.DataSet.Refresh;
    if (not ActiveUsers.DataSet.Active) or (not ActiveUsers.GotoBookmark(FSessionID)) then
      begin
        ActiveUsers.Select(FSessionID);
        ActiveUsers.Open;
      end;
    if ActiveUsers.DataSet.Active and ActiveUsers.GotoBookmark(FSessionID) then
      ActiveUsers.DataSet.Delete;
  except
  end;
end;

procedure TBaseDBModule.RegisterLinkHandlers;
begin
  RegisterLinkHandler('ALLOBJECTS',nil,TObjects);
  //Messages
  RegisterLinkHandler('HISTORY',nil,TBaseHistory);
  //Messages
  if Users.Rights.Right('MESSAGES') > RIGHT_NONE then
    begin
      try
        RegisterLinkHandler('MESSAGEIDX',nil,TMessage);
        AddSearchAbleDataSet(TMessageList);
      except
      end;
    end;
  //Tasks
  if (Users.Rights.Right('TASKS') > RIGHT_NONE) then
    begin
      try
      RegisterLinkHandler('TASKS',nil,TTask,TTaskList);
      except
      end;
    end;
  //Add PIM Entrys
  if Users.Rights.Right('CALENDAR') > RIGHT_NONE then
    begin
      try
        RegisterLinkHandler('CALENDAR',nil,TTask,TTaskList);
      except
      end;
    end;
  //Orders
  if Users.Rights.Right('ORDERS') > RIGHT_NONE then
    begin
      try
      RegisterLinkHandler('ORDERS',nil,Torder);
      AddSearchAbleDataSet(TOrderList);
      except
      end;
    end;
  //Add Contacts
  if Users.Rights.Right('CUSTOMERS') > RIGHT_NONE then
    begin
      try
      RegisterLinkHandler('CUSTOMERS',nil,TPerson);
      AddSearchAbleDataSet(TPersonList);
      AddSearchAbleDataSet(TPersonContactData);
      AddSearchAbleDataSet(TPersonAddress);
      except
      end;
    end;
  //Add Masterdata stuff
  if (Users.Rights.Right('MASTERDATA') > RIGHT_NONE) then
    begin
      try
      RegisterLinkHandler('MASTERDATA',nil,TMasterdata);
      AddSearchAbleDataSet(TMasterdataList);
      except
      end;
    end;
  //Projects
  if (Users.Rights.Right('PROJECTS') > RIGHT_NONE) then
    begin
      try
      RegisterLinkHandler('PROJECT',nil,TProject);
      AddSearchAbleDataSet(TProjectList);
      except
      end;
    end;
  //Wiki
  RegisterLinkHandler('WIKI',nil,TWikiList);
  if (Users.Rights.Right('WIKI') > RIGHT_NONE) then
    begin
      try
      AddSearchAbleDataSet(TWikiList);
      except
      end;
    end;
  //Documents
  if (Users.Rights.Right('DOCUMENTS') > RIGHT_NONE) then
    begin
      try
      RegisterLinkHandler('DOCUMENTS',nil,TDocument);
      //RegisterLinkHandler('DOCPAGES',nil,TDocPages);
      except
      end;
    end;
  //Lists
  if (Users.Rights.Right('LISTS') > RIGHT_NONE) then
    begin
      try
      RegisterLinkHandler('LISTS',nil,TLists);
      AddSearchAbleDataSet(TLists);
      except
      end;
    end;
  //Meetings
  if (Users.Rights.Right('MEETINGS') > RIGHT_NONE) then
    begin
      try
      RegisterLinkHandler('MEETINGS',nil,TMeetings);
      AddSearchAbleDataSet(TMeetings);
      except
      end;
    end;
  //Inventory
  if (Users.Rights.Right('INVENTORY') > RIGHT_NONE) then
    begin
      try
      RegisterLinkHandler('INVENTORY',nil,TInventorys);
      except
      end;
    end;
  //Statistics
  if (Users.Rights.Right('STATISTICS') > RIGHT_NONE) then
    begin
      try
      RegisterLinkHandler('STATISTICS',nil,TStatistic);
      AddSearchAbleDataSet(TStatistic);
      except
      end;
    end;
  //Timeregistering
  AddSearchAbleDataSet(TUser);
  //History
  if Users.Rights.Right('DOCUMENTS') > RIGHT_NONE then
    begin
      try
      AddSearchAbleDataSet(TBaseHistory);
      RegisterLinkHandler('HISTORY',nil,TBaseHistory);
      except
      end;
    end;
end;

function TBaseDBInterface.GetMandantPath: string;
begin
  Result := FConfigPath
end;
procedure TBaseDBInterface.SetMandantPath(AValue: string);
begin
end;
procedure TBaseDBInterface.SetDBTyp(const AValue: string);
begin
  if (FDbTyp = AValue) and Assigned(FDB) then exit;
  FreeAndNil(FDB);
  if Uppercase(trim(AValue)) = 'SQL' then
    FDB := TZeosDBDM.Create(BaseApplication);
  FDbTyp := AValue;
end;
function TBaseDBInterface.GetDB: TBaseDBModule;
begin
  Result := FDB;
end;
procedure TBaseDBInterface.SetDB(const AValue: TBaseDBModule);
begin
  FDB := AValue;
end;
function TBaseDBInterface.GetConfig: TDBConfig;
begin
  Result := FConfig;
end;
function TBaseDBInterface.GetLastError: string;
begin
  Result := FLastError;
end;
procedure TBaseDBInterface.SetLastError(const AValue: string);
begin
  FLastError := AValue;
end;
constructor TBaseDBInterface.Create;
begin
end;
destructor TBaseDBInterface.Destroy;
begin
  if Assigned(FConfig) then
    FConfig.Free;
  inherited Destroy;
end;
procedure TBaseDBInterface.DBLogout;
begin
  if Assigned(FDB) then
    FDB.RemoveUserFromActiveList;
end;
function TBaseDBInterface.GetMandantName: string;
begin
  Result := copy(ExtractFileName(FMandantFile),0,length(ExtractFileName(FMandantFile))-length(MandantExtension));
end;
function TBaseDBInterface.DBLogin(aMandant, aUser: string; HideStatus: Boolean;AppendToActiveList : Boolean = True): Boolean;
var
  DocTemp: TDocuments;
  FImages: TImages;
  FLinks: TLinks;
  FArchiveStore: TArchivedMessage;
  FHistory: TBaseHistory;
  mSettings: TStringList;
  FCategory: TCategory;
begin
  Result := False;
  //Check if FDB already is our Mandant
  if FMandantFile <> AppendPathDelim(FConfigPath)+aMandant+MandantExtension then
    if not FileExists(UniToSys(AppendPathDelim(FConfigPath)+aMandant+MandantExtension)) then
      begin
        FLastError := 'Not such Mandant ('+aMandant+',Config:'+ExtractFilePath(FConfigPath)+') !';
        exit;
      end;
  mSettings := TStringList.Create;
  FMandantFile:=AppendPathDelim(FConfigPath)+aMandant+MandantExtension;
  mSettings.LoadFromFile(UniToSys(FMandantFile));
  if (mSettings.Count <> 2) or (not OpenMandant(mSettings[0],mSettings[1])) then
    begin
      exit;
    end;
  mSettings.Free;
  FDB.TableVersions.CreateTable;
  FDB.MandantDetails.CreateTable;
  FDB.MandantDetails.Open;
  FDB.Users.CreateTable;
  FDB.Numbers.CreateTable;
  FDB.ActiveUsers.CreateTable;
  if aUser <> '' then
    begin
      with FDB.Users.DataSet do
        begin
          Open;
          if not Locate('NAME',aUser,[]) then
            begin
              FLastError := 'User not found ('+aUser+') !';
              exit;
            end;
        end;
    end
  else
    with FDB.Users.DataSet do
      begin
        FDB.Users.Open;
        try
          if (not Locate('NAME',strGuest,[])) and (FDB.Numbers.HasNumberSet('USERS')) then
            begin
              Insert;
              FieldByName('NAME').AsString:=strGuest;
              Post;
            end;
        except
        end;
      end;
  FDB.ActiveUsers.Open;
  with BaseApplication as IBaseApplication do
    if SingleInstance
    and FDB.ActiveUsers.DataSet.Locate('CLIENT',ExtractFileName(ParamStr(0)),[])
    and ((FDB.ActiveUsers.FieldByName('HOST').AsString = GetSystemName) and (ProcessExists(ExtractFileName(ParamStr(0)),'')))
    then
      begin
        Result := False;
        DoExit;
        BaseApplication.Terminate;
        FLastError := 'already started';
        exit;
      end
    else if FDB.ActiveUsers.DataSet.Locate('CLIENT;HOST',VarArrayOf([ExtractFileName(ParamStr(0)),GetSystemName]),[]) then
      begin
        while FDB.ActiveUsers.DataSet.Locate('CLIENT;HOST',VarArrayOf([ExtractFileName(ParamStr(0)),GetSystemName]),[]) do
          FDB.ActiveUsers.Delete;
      end;

  FCategory := TCategory.CreateEx(nil,FDB,FDB.MainConnection);
  FCategory.CreateTable;
  FCategory.Free;
  FImages := TImages.CreateEx(nil,FDB,FDB.MainConnection);
  FImages.CreateTable;
  FImages.Free;
  FLinks := TLinks.CreateEx(nil,FDB,FDB.MainConnection);
  FLinks.CreateTable;
  FLinks.Free;
  FHistory := TBaseHistory.CreateEx(nil,FDB,FDB.MainConnection);
  FHistory.CreateTable;
  FHistory.Free;
  FArchiveStore := TArchivedMessage.CreateEx(nil,FDB,FDB.MainConnection);
  FArchiveStore.CreateTable;
  FArchiveStore.Free;
  FDB.Permissions.CreateTable;
  FDB.DeletedItems.CreateTable;
  FDB.Forms.CreateTable;
  FDB.StorageType.CreateTable;
  FDB.Users.Options.Open;
  if AppendToActiveList then
    FDB.AppendUserToActiveList;
  FDB.Tree.CreateTable;
  FDB.Users.LoginWasOK;
  Result := True;
end;
function TBaseDBInterface.LoadMandants(aConfigPath: string): Boolean;
var
  FilePath: String;
  mSettings: TStringList;
  aInfo: TSearchRec;
  bInfo: TSearchRec;
  sl: TStringList;
begin
  Result := False;
  try
    if aConfigPath <> '' then
      FilePath := aConfigpath
    else
      begin
        if Assigned(BaseApplication) then
          begin
            with BaseApplication as IBaseApplication do
              FilePath := GetOurConfigDir;
          end
        else FilePath := GetConfigDir(StringReplace(lowercase('prometerp'),'-','',[rfReplaceAll]));
      end;
    if not DirectoryExists(FilePath) then ForceDirectories(FilePath);
    FConfigPath:=FilePath;
    Result := True;
  except
    on e : Exception do
      begin
        with BaseApplication as IBaseApplication do
          Warning('LoadMandants:'+e.Message);
        Result := False;
      end;
  end;
  if not FindFirst(AppendPathDelim(FilePath)+'*'+MandantExtension,faAnyFile and faDirectory,aInfo)=0 then
    begin
      if FindFirst(UniToSys(AppendPathDelim(SysToUni(BaseApplication.Location))+'*'+MandantExtension),faAnyFile and faDirectory,bInfo)=0 then
        begin
          sl := TStringList.Create;
          sl.LoadFromFile(AppendPathDelim(SysToUni(BaseApplication.Location))+bInfo.Name);
          sl.SaveToFile(AppendPathDelim(FilePath)+bInfo.Name);
          sl.Free;
          FindClose(bInfo);
        end;
    end
  else FindClose(aInfo);
end;
function TBaseDBInterface.OpenMandant(aDBTyp : string;aDBProp : string): Boolean;
begin
  with Self as IBaseDbInterface do
    begin
      FreeAndNil(FConfig);
      DBTyp := aDBTyp;
      Result := Assigned(FDB);
      if not Result then
        begin
          FLastError := 'failed setting DB Typ failed !';
          Result := False;
          exit;
        end;
      if not FDB.SetProperties(aDBProp) then
        begin
          Result := False;
          FreeAndNil(FDB);
          exit;
        end;
      FConfig := TDBConfig.Create;
      Result := True;
    end;
end;
function TBaseDBInterface.QuoteField(aField: string): string;
begin
  Result := aField;
  if not Assigned(FDB) then Exit;
  Result := FDb.QuoteField(Result);
end;
function TBaseDBInterface.QuoteValue(aValue: string): string;
begin
  Result := aValue;
  if not Assigned(FDB) then Exit;
  Result := FDb.QuoteValue(Result);
end;
procedure TBaseDBInterface.SetOwner(aOwner: TObject);
begin
  FOwner := aOwner;
end;
end.

