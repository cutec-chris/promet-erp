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
  uBaseDbClasses, uIntfStrConsts,syncobjs,
  Variants,
  rttiutils,uBaseDatasetInterfaces,contnrs,uAbstractDBLayer
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
  TOpenLinkEvent = function(aLink : string;Sender : TObject) : Boolean of object;
  TCreateFromLinkEvent = function(aLink : string;Sender : TObject) : TBaseDbDataSet of object;
  LinkHandler = record
    aLinkType : string;
    aEvent : TOpenLinkEvent;
    aClass : TBaseDBDatasetClass;
    aListClass : TBaseDBDatasetClass;
  end;

  DatasetClass = record
    aName : string;
    aClass : TBaseDBDatasetClass;
  end;

  { TBaseDBModule }

  TBaseDBModule = class(TAbstractDBModule)
  private
    Fmandant: string;
    FProperies: string;
    FSessionID: LargeInt;
    FTables: TStrings;
    FFullTables : TStrings;
    FTriggers: TStrings;
    FLinkHandlers : array of LinkHandler;
    FUsers : TUser;
    FLoggedInUser : Variant;
    function GetUsers: TUser;
  protected
    FDataSetClass : TDataSetClass;
  public
    //DBTables : TBaseDBTables;
    ActiveUsers : TActiveUsers;
    Numbers : TNumberSets;
    NumberPools: TNumberPools;
    NumberRanges: TNumberRanges;
    MandantDetails : TMandantDetails;
    //Tree : TTree;
    //Forms : TForms;
    //Filters : TFilters;
    //Reports : TReports;
    //Permissions : TPermissions;
    //StorageTypes : TStorageTypes;
    //Currency : TCurrency;
    //PaymentTargets : TPaymentTargets;
    //StorageType : TStorageTyp;
    //StorageJournal : TStorageJournal;
    //Countries : TCountries;
    //Languages : TLanguages;
    Userfielddefs : TUserFielddefs;
    //States : TStates;
    //Categories : TCategory;
    //DeletedItems : TDeletedItems;
    //_DocumentActions : TInternalDBDataSet;
    //_MimeTypes : TInternalDBDataSet;
    constructor Create(AOwner : TComponent);override;
    destructor Destroy;override;
    property SessionID : LargeInt read FSessionID write FSessionID;
    property Users : TUser read GetUsers;
    procedure CleanupSession;
    procedure Connect(aConnection : TComponent);virtual;abstract;
    procedure Disconnect(aConnection : TComponent);virtual;abstract;
    procedure DeleteExpiredSessions;virtual;
    function SetProperties(aProp : string;Connection : TAbstractDBConnection = nil) : Boolean;override;
    function ProcessTerm(aTerm : string;ForceLike : Boolean = False) : string;virtual;
    function GetLinkDesc(aLink: string; Fast: Boolean=false): string; virtual;
    function GetLinkLongDesc(aLink : string) : string;virtual;
    function GetLinkIcon(aLink: string; GetRealIcon: Boolean=False): Integer;
      virtual;
    function GetUSerCode: string; override;
    function BuildLink(aDataSet : TDataSet) : string;virtual;
    function GotoLink(const aLink : string) : Boolean;virtual;
    function DataSetFromName(aName: string;var aClass : TBaseDBDatasetClass): Boolean;
    function DataSetFromLink(aLink: string;var aClass : TBaseDBDatasetClass): Boolean;
    function ListDataSetFromLink(aLink: string;var aClass : TBaseDBDatasetClass): Boolean;
    procedure RegisterLinkHandler(aLink : string;aOpenHandler : TOpenLinkEvent;DataSetClass : TBaseDBDatasetClass;DataSetListClass : TBaseDBDatasetClass = nil);
    function GetBookmark(aDataSet : TBaseDbDataSet) : Variant;
    function GotoBookmark(aDataSet : TBaseDbDataSet;aRec : Variant) : Boolean;
    function GetErrorNum(e : EDatabaseError) : Integer;virtual;
    function RecordCount(aDataSet : TBaseDbDataSet) : Integer;
    function DeleteItem(aDataSet : TBaseDBDataSet) : Boolean;
    procedure UpdateTableVersion(aTableName: string);
    //function GetFullTableName(aTable: string;DoLookup : Boolean = True): string; override;
    //function CreateTrigger(aTriggerName : string;aTableName : string;aUpdateOn : string;aSQL : string;aField : string = '';aConnection : TComponent = nil) : Boolean;virtual;
    //function Preprocess(aLine : string) : string;
    //procedure SetFilter(DataSet : TbaseDBDataSet;aFilter : string;aLimit : Integer = 0;aOrderBy : string = '';aSortDirection : string = 'ASC';aLocalSorting : Boolean = False;aGlobalFilter : Boolean = True;aUsePermissions : Boolean = False;aFilterIn : string = '');
    procedure AppendUserToActiveList;
    //procedure RefreshUsersFilter;
    //procedure ModifyUsersFilter(aNewFilter : string);
    procedure RemoveUserFromActiveList;
    procedure RegisterLinkHandlers(IgnoreRights : Boolean = False);
    function Authenticate(aUser,aPassword : string) : Boolean;
    property Mandant : string read Fmandant;
    property Properties : string read FProperies;
    property LoggedInUser : Variant read FLoggedInUser;
  end;
  TBaseDBModuleClass = class of TBaseDBModule;
  IBaseDBInterface = interface['{A2AB4BAB-38DF-4D4E-BCE5-B7D57E115ED5}']
    function GetConfig: TDBConfig;
    function GetDB: TBaseDBModule;
    function GetLastError: string;
    function GetTypeName: string;
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
    procedure FDBLog(Sender: TComponent; aLog: string);
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
    function GetTypeName: string;
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
  RIGHT_NONE  = -1;
  RIGHT_VIEW  = 0;
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
  IMAGE_PRODUCTION         = 18;
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
  strWebsite                     = 'Webseite';
  strScreenshotName              = 'Screenshot Name';
  strEnterAnName                 = 'enter an Name';
  strProjectProcess              = 'Projekt/Prozess';
  strFor                         = 'für';

procedure RegisterdataSetClass(aName: string;aClass : TBaseDBDatasetClass);

var
  DatabaseLayers : TClassList;
  DatasetClasses : array of DatasetClass;

implementation

{ TDBConfig }

function TDBConfig.ReadString(const ASection, Ident, DefaultValue: string
  ): string;
begin
  Result := DefaultValue;
end;
procedure TDBConfig.WriteString(const ASection, Ident, Value: string);
begin
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
function TBaseDBModule.SetProperties(aProp: string;Connection: TAbstractDBConnection
  ): Boolean;
var
  actDir: String;
  actVer: LongInt;
  sl: TStringList;
begin
  Result := inherited;
  if not Result then exit;
  if Connection=nil then
    Connection := MainConnection;
  FProperies := aProp;
  if not Assigned(Users) then
    begin
      //DBTables :=  TBaseDBTables.CreateEx(nil,Self);
      FUsers := TUser.CreateEx(nil,Self);
      Numbers := TNumberSets.CreateEx(nil,Self);
      NumberPools := TNumberPools.CreateEx(nil,Self);
      NumberRanges := TNumberRanges.CreateEx(nil,Self);
      MandantDetails := TMandantDetails.CreateEx(nil,Self);
      //Tree := TTree.CreateEx(nil,Self);
      //Forms := TForms.CreateEx(nil,Self);
      UserFieldDefs := TUserFieldDefs.CreateEx(nil,Self);
      //Filters := TFilters.CreateEx(nil,Self);
      //Reports := TReports.CreateEx(nil,Self);
      ActiveUsers := TActiveUsers.CreateEx(nil,Self);
      //Permissions := TPermissions.CreateEx(nil,Self);
      //StorageTypes := TStorageTypes.CreateEx(nil,Self);
      //Currency := TCurrency.CreateEx(nil,Self);
      //PaymentTargets := TPaymentTargets.CreateEx(nil,Self);
      //StorageType := TStorageTyp.CreateEx(nil,Self);
      //StorageJournal := TStorageJournal.CreateEx(nil,Self);
      //Countries := TCountries.CreateEx(nil,Self);
      //Languages := TLanguages.CreateEx(nil,Self);
      //States := TStates.CreateEx(nil,Self);
      //Categories := TCategory.CreateEx(nil,Self);
      //DeletedItems := TDeletedItems.CreateEx(nil,Self);
      //ProcessClient := TProcessClient.CreateEx(nil,Self);
    end
  else
    begin
      Users.Close;
    end;

  if Result then
    begin
      if not DBExists then //Create generators
        begin
          try
            if (GetDBType = 'firebird') then
              begin
                ExecuteDirect('EXECUTE BLOCK AS BEGIN'+lineending
                                         +'if (not exists(select 1 from rdb$generators where rdb$generator_name = ''GEN_SQL_ID'')) then'+lineending
                                         +'execute statement ''CREATE SEQUENCE GEN_SQL_ID;'';'+lineending
                                         +'END;');
                ExecuteDirect('EXECUTE BLOCK AS BEGIN'+lineending
                                         +'if (not exists(select 1 from rdb$generators where rdb$generator_name = ''GEN_AUTO_ID'')) then'+lineending
                                         +'execute statement ''CREATE SEQUENCE GEN_AUTO_ID;'';'+lineending
                                         +'END;');
              end
            else if GetDBType = 'sqlite' then
              begin
                ExecuteDirect('CREATE TABLE IF NOT EXISTS "GEN_SQL_ID"("SQL_ID" BIGINT NOT NULL PRIMARY KEY,ID BIGINT);');
                ExecuteDirect('CREATE TABLE IF NOT EXISTS "GEN_AUTO_ID"("SQL_ID" BIGINT NOT NULL PRIMARY KEY,ID BIGINT);');
              end
            else
              begin
                if not TableExists('GEN_SQL_ID') then
                  ExecuteDirect('CREATE TABLE '+QuoteField('GEN_SQL_ID')+'('+QuoteField('SQL_ID')+' BIGINT NOT NULL PRIMARY KEY,'+QuoteField('ID')+' BIGINT);');
                if not TableExists('GEN_AUTO_ID') then
                  ExecuteDirect('CREATE TABLE '+QuoteField('GEN_AUTO_ID')+'('+QuoteField('SQL_ID')+' BIGINT NOT NULL PRIMARY KEY,'+QuoteField('ID')+' BIGINT);');
              end
          except on e : Exception do
            begin
//              if Assigned(BaseApplication) then
//                with BaseApplication as IBaseDBInterface do
//                  LastError := e.Message;
              Result := False;
            end;
          end;
        end;
      if Assigned(MandantDetails) then
        begin
          try
            MandantDetails.Open;
            //DBTables.Open;
            actDir := GetCurrentDir;
            SetCurrentDir(ExtractFileDir((Connection as IBaseDBConnection).GetDatabaseName));
            if Assigned(MandantDetails.FieldByName('DBSTATEMENTS')) and (MandantDetails.FieldByName('DBSTATEMENTS').AsString<>'') then
              ExecuteDirect(MandantDetails.FieldByName('DBSTATEMENTS').AsString);
            SetCurrentDir(actDir);
          except
          end;
          if MandantDetails.Active and Assigned(MandantDetails.FieldByName('DBVER')) then
            begin
              actVer := MandantDetails.FieldByName('DBVER').AsInteger;
              if actVer<(7*10000+437) then
                actVer:=7*10000+437;
            end;
      end;
    end;
end;

function TBaseDBModule.ProcessTerm(aTerm: string; ForceLike: Boolean): string;
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
  if UseLike or ForceLike then
    aFilter := StringReplace(aFilter,'=',' like ',[rfReplaceAll]);
  Result := aFilter;
end;

function TBaseDBModule.GetUsers: TUser;
begin
  try
    if not Assigned(Self) then exit;
    Result := FUsers;
    if Assigned(FUsers) and (not Result.Active) then
      begin
        Result.Open;
        Result.Locate('SQL_ID',FLoggedInUser,[]);
      end;
  except
    Result := nil;
  end;
end;

constructor TBaseDBModule.Create(AOwner: TComponent);
begin
  inherited;
  FUsers := nil;
  FFullTables := TStringList.Create;
end;
destructor TBaseDBModule.Destroy;
begin
  SetLength(FLinkHandlers,0);
  FreeAndNil(FFullTables);
  //FreeAndNil(DBTables);
  FreeAndNil(FUsers);
  //FreeAndNil(Numbers);
  FreeAndNil(MandantDetails);
  //FreeAndNil(Tree);
  //FreeAndNil(Forms);
  FreeAndNil(UserFieldDefs);
  //FreeAndNil(Filters);
  //FreeAndNil(Reports);
  //FreeAndNil(Permissions);
  //FreeAndNil(StorageTypes);
  //FreeAndNil(Currency);
  //FreeAndNil(StorageType);
  //FreeAndNil(StorageJournal);
  //FreeAndNil(States);
  //FreeAndNil(Categories);
  //FreeAndNil(DeletedItems);
  //FreeAndNil(Languages);
  //FreeAndNil(Countries);
  //FreeAndNil(PaymentTargets);
  //FreeAndNil(ProcessClient);
  FreeAndNil(ActiveUsers);
  inherited Destroy;
end;

procedure TBaseDBModule.CleanupSession;
begin
  FreeAndNil(FUsers);
end;

function TBaseDBModule.GetLinkDesc(aLink: string;Fast : Boolean = false): string;
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
  else if copy(aLink, 0, 10) = 'MASTERDATA' then
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
  else if copy(aLink, 0, 9) = 'CUSTOMERS' then
    begin
      Result := strContact;
      if pos('.ID',aLink)=0 then
        Result+=' '+copy(aLink, pos('@', aLink) + 1, length(aLink));
    end
  else if copy(aLink, 0, 9) = 'DOCUMENTS' then
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
  else if copy(aLink, 0, 6) = 'ORDERS' then
    begin
      if IsSQLDB and (not Fast) then
        begin
          aTable := GetNewDataSet('select '+QuoteField('SQL_ID')+','+QuoteField('STATUS')+','+QuoteField('CUSTNAME')+' from '+QuoteField('ORDERS')+' where '+QuoteField('ORDERNO')+'='+QuoteValue(copy(aLink, pos('@', aLink) + 1, length(aLink)))+' OR '+QuoteField('SQL_ID')+'='+QuoteValue(copy(aLink, pos('@', aLink) + 1, length(aLink))));
          aTable.Open;
          aTmp := aTable.FieldByName('SQL_ID').AsString;
          aTmp1 := aTable.FieldByName('STATUS').AsString;
          aTmp2 := trim(aTable.FieldByName('CUSTNAME').AsString);
          FreeAndNil(aTable);
          aTable := GetNewDataSet('select '+QuoteField('STATUSNAME')+' from '+QuoteField('ORDERTYPE')+' where '+QuoteField('STATUS')+'='+QuoteValue(aTmp1));
          aTable.Open;
          Result := aTable.FieldByName('STATUSNAME').AsString+' '+copy(aLink, pos('@', aLink) + 1, length(aLink));
          if aTmp2<>'' then
            result := result+' '+strFor+' '+aTmp2;
          FreeAndNil(aTable);
        end
      else
        begin
          Result := strOrder+' '+copy(aLink, pos('@', aLink) + 1, length(aLink));
        end;
    end
  else if copy(aLink, 0, 5) = 'CALLS' then
    begin
      Result := strCall+' '+copy(aLink, pos('@', aLink) + 1, length(aLink));
    end
  else if copy(aLink, 0, 10) = 'MESSAGES' then
    begin
      Result := strMessage+' '+copy(aLink, pos('@', aLink) + 1, length(aLink));
    end
  else if copy(aLink, 0, 4) = 'WIKI' then
    begin
      Result := strWikiPage+' '+copy(aLink, pos('@', aLink) + 1, length(aLink));
    end
  else if copy(aLink, 0, 8) = 'PROJECTS' then
    begin
      Result := strProjectProcess;
      if pos('.ID',aLink)=0 then
        Result+=' '+copy(aLink, pos('@', aLink) + 1, length(aLink));
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
  //aWiki: TWikiList;
  aID: String;
  aTmp1: String;
  //aBaseHist: TBaseHistory;
begin
  Result := '';
  {
  with BaseApplication as IBaseDbInterface do
    begin
      if not IsSQLDb then exit;
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
              aTable := Data.GetNewDataSet('select '+Data.QuoteField('TEXT')+' from '+Data.QuoteField('TEXTS')+' where '+Data.QuoteField('REF_ID')+'='+Data.QuoteValue(aTmp));
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
              aTable := Data.GetNewDataSet('select '+Data.QuoteField('SQL_ID')+' from '+Data.QuoteField('CUSTOMERS')+' where '+Data.QuoteField('ACCOUNTNO')+'='+Data.QuoteValue(copy(aLink, pos('@', aLink) + 1, length(aLink))));
              aTable.Open;
              aTmp := aTable.FieldByName('SQL_ID').AsString;
              FreeAndNil(aTable);
            end
          else aTmp := copy(aLink, pos('@', aLink) + 1, length(aLink));
          aTable := Data.GetNewDataSet('select '+Data.QuoteField('NAME')+','+Data.QuoteField('ADDRESS')+','+Data.QuoteField('ZIP')+','+Data.QuoteField('CITY')+' from '+Data.QuoteField('ADDRESSES')+' where '+Data.QuoteField('REF_ID')+'='+Data.QuoteValue(aTmp));
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
          aTable := Data.GetNewDataSet('select '+Data.QuoteField('SQL_ID')+','+Data.QuoteField('STATUS')+' from '+Data.QuoteField('ORDERS')+' where '+Data.QuoteField('ORDERNO')+'='+Data.QuoteValue(copy(aLink, pos('@', aLink) + 1, length(aLink))));
          aTable.Open;
          aTmp := aTable.FieldByName('SQL_ID').AsString;
          aTmp1 := aTable.FieldByName('STATUS').AsString;
          FreeAndNil(aTable);
          aTable := Data.GetNewDataSet('select '+Data.QuoteField('NAME')+','+Data.QuoteField('ADDRESS')+','+Data.QuoteField('ZIP')+','+Data.QuoteField('CITY')+' from '+Data.QuoteField('ORDERADDR')+' where '+Data.QuoteField('REF_ID')+'='+Data.QuoteValue(aTmp));
          aTable.Open;
          if aTable.RecordCount > 0 then
            begin
              Result := lineending+aTable.FieldByName('NAME').AsString+','+aTable.FieldByName('ADDRESS').AsString+','+aTable.FieldByName('ZIP').AsString+' '+aTable.FieldByName('CITY').AsString;
            end;
          FreeAndNil(aTable);
          aTable := Data.GetNewDataSet('select '+Data.QuoteField('POSNO')+','+Data.QuoteField('IDENT')+','+Data.QuoteField('SHORTTEXT')+','+Data.QuoteField('QUANTITY')+','+Data.QuoteField('QUANTITYU')+' from '+Data.QuoteField('ORDERPOS')+' where '+Data.QuoteField('REF_ID')+'='+Data.QuoteValue(aTmp));
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
      else if copy(aLink, 0, pos('@', aLink) - 1) = 'PROJECTS.ID' then
        begin
          aTable := Data.GetNewDataSet('select '+Data.QuoteField('NAME')+','+Data.QuoteField('DESCRIPTION')+' from '+Data.QuoteField(StringReplace(copy(aLink, 0, pos('@', aLink) - 1),'.ID','',[rfReplaceAll]))+' where '+Data.QuoteField('SQL_ID')+'='+Data.QuoteValue(copy(aLink, pos('@', aLink) + 1, length(aLink))));
          aTable.Open;
          Result := aTable.FieldByName('NAME').AsString+LineEnding+LineEnding+aTable.FieldByName('DESCRIPTION').AsString;
          FreeAndNil(aTable);
        end
      else if copy(aLink, 0, pos('@', aLink) - 1) = 'PROJECTS' then
        begin
          aTable := Data.GetNewDataSet('select '+Data.QuoteField('NAME')+','+Data.QuoteField('DESCRIPTION')+' from '+Data.QuoteField('PROJECTS')+' where '+Data.QuoteField('ID')+'='+Data.QuoteValue(copy(aLink, pos('@', aLink) + 1, length(aLink))));
          aTable.Open;
          Result := aTable.FieldByName('NAME').AsString+LineEnding+LineEnding+aTable.FieldByName('DESCRIPTION').AsString;
          FreeAndNil(aTable);
        end
      else if copy(aLink, 0, pos('@', aLink) - 1) = 'TASKS' then
        begin
          aTable := Data.GetNewDataSet('select '+Data.QuoteField('DESC')+' from '+Data.QuoteField(copy(aLink, 0, pos('@', aLink) - 1)+'')+' where '+Data.QuoteField('SQL_ID')+'='+Data.QuoteValue(copy(aLink, pos('@', aLink) + 1, length(aLink))));
          aTable.Open;
          Result := aTable.FieldByName('DESC').AsString;
          FreeAndNil(aTable);
        end
      else if copy(aLink, 0, pos('@', aLink) - 1) = 'WIKI' then
        begin
          if IsNumeric(copy(aLink, pos('@', aLink) + 1, length(aLink))) then
            begin
              aTable := Data.GetNewDataSet('select '+Data.QuoteField('DATA')+' from '+Data.QuoteField(copy(aLink, 0, pos('@', aLink) - 1)+'')+' where '+Data.QuoteField('SQL_ID')+'='+Data.QuoteValue(copy(aLink, pos('@', aLink) + 1, length(aLink))));
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
  }}}
end;
function TBaseDBModule.GetLinkIcon(aLink: string;GetRealIcon : Boolean = False): Integer;
//var
//  aObjs: TObjects;
begin
  Result := -1;
  {
  if pos('://',aLink) > 0 then Result := IMAGE_WEBSITE
  else if copy(aLink, 0, 10) = 'MASTERDATA' then Result := IMAGE_MASTERDATA
  else if copy(aLink, 0, 10) = 'ALLOBJECTS' then
    begin
      Result := 121;
      try
        if GetRealIcon then
          begin
            aObjs := TObjects.Create(nil);
            aObjs.SelectByLink(aLink);
            aObjs.Open;
            if aObjs.Count>0 then
              Result := aObjs.FieldByName('ICON').AsInteger;
            aObjs.Free;
          end;
      except
      end;
    end
  else if (copy(aLink, 0, 9) = 'CUSTOMERS') then Result := IMAGE_PERSON
  else if copy(aLink, 0, 9) = 'DOCUMENTS' then   Result := IMAGE_DOCUMENTS
  else if copy(aLink, 0, 6) = 'ORDERS' then      Result := IMAGE_ORDERS
  else if copy(aLink, 0, 5) = 'CALLS' then       Result := IMAGE_CALLS
  else if copy(aLink, 0, 10) = 'MESSAGES' then Result := IMAGE_MESSAGE
  else if copy(aLink, 0, 8) = 'CALENDAR' then    Result := IMAGE_CALENDAR
  else if copy(aLink, 0, 15) = 'ACCOUNTEXCHANGE' then Result := IMAGE_FINANCIAL
  else if (copy(aLink, 0, 8) = 'PROJECTS') then  Result := IMAGE_PROJECT
  else if (copy(aLink, 0, 5) = 'TASKS') then     Result := IMAGE_TASK
  else if (copy(aLink, 0, 4) = 'WIKI') then      Result := IMAGE_WIKI
  else if (copy(aLink, 0, 10) = 'STATISTICS') then  Result := IMAGE_STATISTIC
  else if (copy(aLink, 0, 7) = 'SCRIPTS') then   Result := 62
  else if (copy(aLink, 0, 6) = 'SCHEME') then   Result := 130
  ;
  }
end;

function TBaseDBModule.GetUSerCode: string;
begin
  Result:=inherited GetUSerCode;
  if Users.Active then
    Result := USers.FieldByName('IDCODE').AsString;
end;

function TBaseDBModule.BuildLink(aDataSet: TDataSet): string;
var
  aTable : TDataSet;
begin
  {
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
          aTable := GetNewDataSet('select '+QuoteField('NAME')+' from '+QuoteField('CUSTOMERS')+' where '+QuoteField('SQL_ID')+'='+QuoteValue(aDataSet.FieldByName('REF_ID').AsString));
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
          aTable := GetNewDataSet('select '+QuoteField('NAME')+' from '+QuoteField('CUSTOMERS')+' where '+QuoteField('SQL_ID')+'='+QuoteValue(aDataSet.FieldByName('REF_ID').AsString));
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
      Result := result+'{'+aDataSet.FieldByName('NUMBER').AsString+'}';
    end
  else if (Result = 'CALLS@') then
    begin
      Result := Result + aDataSet.FieldByName('ID').AsString;
    end
  else  if (Result = 'MESSAGES@') then
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
          Result := Result + aDataSet.FieldByName('ID').AsString;
          Result := result+'{'+aDataSet.FieldByName('NAME').AsString+'}';
        end
      else
        begin
          Result := 'PROJECTS.ID@';
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
  }
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

function TBaseDBModule.DataSetFromName(aName: string;
  var aClass: TBaseDBDatasetClass): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := low(DatasetClasses) to High(DatasetClasses) do
    if DatasetClasses[i].aName=aName then
      begin
        Result := True;
        aClass:=DatasetClasses[i].aClass;
        break;
      end;
end;

procedure RegisterdataSetClass(aName: string;
  aClass: TBaseDBDatasetClass);
var
  i: Integer;
  Found: Boolean;
begin
  Found := False;
  for i := low(DatasetClasses) to High(DatasetClasses) do
    if DatasetClasses[i].aName=aName then
      begin
        Found := True;
        break;
      end;
  if not Found then
    begin
      SetLength(DatasetClasses,length(DatasetClasses)+1);
      DatasetClasses[length(DatasetClasses)-1].aName:=aName;
      DatasetClasses[length(DatasetClasses)-1].aClass:=aClass;
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
  //Result := aDataSet.GetBookmark;
end;
function TBaseDBModule.GotoBookmark(aDataSet: TBaseDbDataSet; aRec: Variant
  ): Boolean;
begin
  //Result := aDataSet.GotoBookmark(aRec);
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
  {
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
  }
end;
procedure TBaseDBModule.UpdateTableVersion(aTableName: string);
begin
end;
procedure TBaseDBModule.AppendUserToActiveList;
begin
  {
  try
    ActiveUsers.Select(FSessionID);
    ActiveUsers.Open;
    if ActiveUsers.IsReadOnly then exit;
    UsersFilter:='';
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
            try
              Insert;
              if Users.DataSet.Active and (Users.Count>0) then
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
            except
              Cancel;
            end;
          end;
      end;
    RefreshUsersFilter;
  except
  end;
  ActiveUsers.Close;
  RegisterLinkHandlers;
  }
end;

procedure TBaseDBModule.RemoveUserFromActiveList;
begin
end;

procedure TBaseDBModule.RegisterLinkHandlers(IgnoreRights: Boolean);
begin

end;

function TBaseDBModule.Authenticate(aUser, aPassword: string): Boolean;
begin
  Result := False;
  Result := (Users.Active)
  and ((Users.FieldByName('NAME').AsString=aUser) or (Users.FieldByName('LOGINNAME').AsString=aUser))
  and (Users.FieldByName('AUTHSOURCE').AsString='')
  and (Users.CheckUserPasswort(aPassword));
  if not Result then
    begin
      if not Users.Active then
        Users.Open;
      if Users.Locate('NAME',aUser,[loCaseInsensitive])
      or Users.Locate('LOGINNAME',aUser,[loCaseInsensitive])
      then
        Result := Users.CheckUserPasswort(aPassword);
    end;
  if not Result then
    begin
      with MandantDetails.AuthSources do
        begin
          Open;
          First;
          while not EOF do
            begin
              if Authenticate(aUser,aPassword) then
                begin
                  Result := True;
                  break;
                end;
              Next;
            end;
        end;
    end;
  Result := Result
        and (Users.Leaved=0)
        and (Users.FieldByName('TYPE').AsString <> 'G')
        and ((not Assigned(Users.FieldByName('LOGINACTIVE'))) or (Users.FieldByName('LOGINACTIVE').AsString<>'N'));
end;
procedure TBaseDBInterface.FDBLog(Sender: TComponent; aLog: string);
begin
end;

function TBaseDBInterface.GetMandantPath: string;
begin
  Result := FConfigPath
end;
procedure TBaseDBInterface.SetMandantPath(AValue: string);
begin
end;
procedure TBaseDBInterface.SetDBTyp(const AValue: string);
var
  i: Integer;
  tmp: String;
begin
  //!!Warning dont compare if AValue has changed
  //in sqlite unpreparing dont work in Zeos so we cleanup Database Layer to unprepare all Datasets
  FreeAndNil(FDB);
  FDB := TBaseDBModule.Create(nil);
  if not Assigned(FDB) then Exception.Create('Database Layer not supported !');
  FDB.OnLog:=@FDBLog;
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

function TBaseDBInterface.GetTypeName: string;
begin
  Result := '';
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
  Found: Boolean;
begin
  Result := False;
  {
  if Assigned(BaseApplication) then with BaseApplication as IBaseApplication do
    Info('Login to Mandant '+aMandant+' as '+aUser);
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
  FDB.MandantDetails.Open;
  if aUser <> '' then
    begin
      with FDB.Users do
        begin
          if (FDB.Users.FieldByName('NAME').AsString=aUser)
          or (FDB.Users.FieldByName('LOGINNAME').AsString=aUser)
          or (FDB.Users.FieldByName('EMAIL').AsString=aUser) then
          else if (not Locate('NAME',aUser,[])) and (not Locate('LOGINNAME',aUser,[])) and (not Locate('EMAIL',aUser,[])) then
            begin
              FLastError := 'User not found ('+aUser+') !';
              exit;
            end;
        end;
    end
  else
    with FDB.Users do
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
  FDB.FLoggedInUser := FDB.Users.Id.AsVariant;
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

  if AppendToActiveList then
    FDB.AppendUserToActiveList;
  FDB.Users.CreateTable;
  FDB.Permissions.CreateTable;
  FDB.DeletedItems.CreateTable;
  FDB.Forms.CreateTable;
  FDB.StorageType.CreateTable;
  FDB.Users.LoginWasOK;
  Result := True;
  }
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
  {
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
    if not DirectoryExists(UniToSys(FilePath)) then ForceDirectories(UniToSys(FilePath));
    FConfigPath:=FilePath;
    Result := True;
  except
    on e : Exception do
      begin
        if Assigned(BaseApplication) then with BaseApplication as IBaseApplication do
          Warning('LoadMandants:'+e.Message);
        Result := False;
      end;
  end;
  if FindFirst(AppendPathDelim(FilePath)+'*'+MandantExtension,faAnyFile and faDirectory,aInfo)<>0 then
    begin
      if FindFirst(UniToSys(AppendPathDelim(SysToUni(BaseApplication.Location))+'*'+MandantExtension),faAnyFile and faDirectory,bInfo)=0 then
        begin
          sl := TStringList.Create;
          sl.LoadFromFile(AppendPathDelim(SysToUni(BaseApplication.Location))+bInfo.Name);
          sl.SaveToFile(AppendPathDelim(FilePath)+bInfo.Name);
          sl.Free;
        end;
      FindClose(bInfo);
    end;
  FindClose(aInfo);
  }
end;
function TBaseDBInterface.OpenMandant(aDBTyp : string;aDBProp : string): Boolean;
begin
  with Self as IBaseDbInterface do
    begin
      try
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
      except
        Result:=False;
      end;
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

initialization
  DataBaseLayers := TClassList.Create;
finalization
  DatabaseLayers.Free;
  SetLength(DatasetClasses,0);
end.

