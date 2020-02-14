unit ubasedbclasses;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uBaseDatasetInterfaces2, db, Contnrs;

type
  TRawBlob = class
  end;

  { TBaseDBDataset }

  TBaseDBDataset = class(TAbstractDBDataset2)
  public
    constructor CreateEx(Module : TComponent;Owner : TComponent);virtual;
  end;
  generic TList<T> = class
    Items: array of T;
    procedure Add(Value: T);
  end;
  TBaseDbList = class(TBaseDBDataSet)
  private
    function GetActive: Boolean;
    function GetBookNumber: TField;
    function GetMatchcode: TField;
    function GetBarcode: TField;
    function GetCommission: TField;
    function GetDescription: TField;
    function GetStatus: TField;
    function GetText: TField;
    function GetNumber : TField;
  protected
    FStatusCache: TStringList;
  public
    //constructor CreateEx(aOwner : TComponent;DM : TComponent=nil;aConnection : TComponent = nil;aMasterdata : TDataSet = nil);override;
    destructor Destroy; override;
    function GetStatusIcon : Integer;virtual;
    function GetTyp: string;virtual;
    function GetMatchcodeFieldName: string;virtual;
    function GetBarcodeFieldName: string;virtual;
    function GetCommissionFieldName: string;virtual;
    function GetDescriptionFieldName: string;virtual;
    function GetStatusFieldName: string;virtual;
    function GetTextFieldName: string;virtual;abstract;
    function GetNumberFieldName : string;virtual;abstract;
    function GetBookNumberFieldName : string;virtual;
    function Delete : Boolean; override;
    function Find(aIdent : string;Unsharp : Boolean = False) : Boolean;virtual;
    procedure OpenItem(AccHistory: Boolean=True);virtual;
    procedure BuildSearchIndexes;virtual;
    procedure CascadicPost; override;
    procedure GenerateThumbnail;virtual;
  published
    property Text : TField read GetText;
    property Number : TField read GetNumber;
    property BookNumber : TField read GetBookNumber;
    property Barcode : TField read GetBarcode;
    property Description : TField read GetDescription;
    property Commission : TField read GetCommission;
    property Status : TField read GetStatus;
    property Typ : string read GetTyp;
    property IsActive : Boolean read GetActive;
    property Matchcode: TField read GetMatchcode;
    function SelectFromLink(aLink : string) : Boolean;virtual;
    function SelectFromNumber(aNumber : string) : Boolean;virtual;
    function SelectFromTreeEntry(aParent : LargeInt) : Boolean;virtual;
    function ChangeStatus(aNewStatus : string) : Boolean;virtual;
    function Duplicate : Boolean;virtual;
  end;
  TBaseDbDataSetClass = class of TBaseDbDataSet;
  TBaseDbListClass = class of TBaseDbList;

  { TOption }

  TOption = class(TBaseDBDataset)
  private
    FOption,FValue : string;
  public
    class function GetRealTableName: string; override;
  published
    property OPTION : string index 60 read FOption write FOption;
    property VALUE : string read FValue write FValue;
  end;
  TOptions = class(TAbstractMasterDetail)
  public
    class function GetObjectTyp: TClass; override;
  end;

  { TUser }

  TUser = class(TBaseDBDataset)
  private
    FAcc: string;
    FAuthSource: string;
    FCustomerNo: string;
    FDep: string;
    FEmployment: TDateTime;
    FIDCode: string;
    FLastLogin: TDateTime;
    Fleaved: TDateTime;
    FLogin: string;
    FLoginActive: Boolean;
    FMail: string;
    FName: string;
    FOptions: TOptions;
    FParent: Int64;
    FPaygroup: Int64;
    fPersNo: string;
    FPosition: string;
    FRemoteAcc: Boolean;
    FSalt: string;
    FType: string;
    FUseWorktime: Integer;
    FWeekWorktime: Integer;
    FWorktime: Integer;
    FPassword : string;
    function MergeSalt(apasswort,aSalt : string) : string;
    function GetRandomSalt : string;
  public
    function CheckUserPasswort(aPasswort: string): Boolean;
    procedure SetPasswort(aPasswort : string);
    constructor Create;
    constructor CreateEx(Module: TComponent; Owner: TComponent);override;
    class function GetRealTableName: string; override;
    class function MapField(aField: string): string; override;
  published
    property TYP : string index 1 read FType write FType;
    property PARENT : Int64 read FParent write FParent;
    property ACCOUNTNO : string index 20 read FAcc write FAcc;
    property NAME : string index 30 read FName write Fname;
    property PASSWORD : string index 45 read FPassword write FPassword;
    property SALT : string index 105 read FSalt write FSalt;
    property IDCODE : string index 4 read FIDCode write FIdCode;
    property EMPLOYMENT : TDateTime read FEmployment write FEmployment;
    property LEAVED : TDateTime read Fleaved write Fleaved;
    property CUSTOMERNO : string index 20 read FCustomerNo write FCustomerno;
    property PERSONNELNO : string index 20 read fPersNo write FPersNo;
    property DEPARTMENT : string index 30 read FDep write FDep;
    property POSITION : string index 30 read FPosition write FPosition;
    property LOGINNAME : string index 30 read FLogin write FLogin;
    property EMAIL : string index 100 read FMail write FMail;
    property PAYGROUP : Int64 read FPaygroup write FPaygroup;
    property WORKTIME: Integer read FWorktime write FWorktime; //8 wenn NULL
    property WEEKWORKTIME : Integer read FWeekWorktime write FWeekworktime;//40 wenn NULL
    property USEWORKTIME : Integer read FUseWorktime write FUseWorktime;
    property LOGINACTIVE : Boolean read FLoginActive write FLoginActive;
    property REMOTEACCESS : Boolean read FRemoteAcc write FRemoteAcc;
    property LASTLOGIN : TDateTime read FLastLogin write FLastLogin;
    property AUTHSOURCE : string index 10 read FAuthSource write FAuthSource;
    property Options : TOptions read FOptions write FOptions;
  end;
  TActiveUsers = class(TBaseDBDataSet)
  private
    FCOMMAND,FAccountNo,FName,FClient,FHost,FVersion: string;
    FEXPIRES: TDateTime;
  public
    function Delete : Boolean; override;
  published
    property ACCOUNTNO : string index 20 read FAccountNo write FAccountNo;
    property NAME : string index 30 read FName write FName;
    property CLIENT : string index 50 read FClient write FClient;
    property HOST : string index 50 read FHost write FHost;
    property VERSION : string index 25 read FVersion write FVersion;
    property COMMAND : string read FCOMMAND write FCOMMAND;
    property EXPIRES : TDateTime read FEXPIRES write FEXPIRES;
  end;
  TUserfielddefs = class(TBaseDBDataSet)
  private
    FTTable,FTField,FTyp : string;
    FSize : Integer;
  public
  published
    property TTABLE : string index 25 read FTTable write FTTable;
    property TFIELD : string index 10 read FTField write FTField;
    property TYP : string index 10 read FTyp write FTyp;
    property SIZE: Integer read FSize write FSize;
  end;
  TNumbersets = class(TBaseDBDataSet)
  private
    FTablename,FTyp,FPool : string;
    FIncr,fActual,FStop : Integer;
  public
    function GetNewNumber(Numberset : string) : string;
    function HasNumberSet(Numberset : string) : Boolean;
  published
    property TABLENAME : string index 25 read FTablename write FTablename;
    property TYP : string index 1 read FTyp write FTyp;
    property INCR: Integer read FIncr write FIncr;
    property ACTUAL: Integer read fActual write FActual;
    property STOP: Integer read FStop write FStop;
    property POOL : string index 25 read FPool write FPool;//NumberPool
  end;
  TNumberRanges = class(TBaseDBDataSet)
  private
    FTablename,FTyp,FPool,FUse,FNotice,FCreatedBy : string;
    FIncr,FStop,FStart : Integer;
  public
    function NewRangefromPool(aPool, aName: string; aCount: Integer; aUse,
      aNotice: string): Boolean;
    function NewRangewithoutPool(aName: string; aFrom, aCount: Integer; aUse,
      aNotice: string; aPool: string=''): Boolean;
  published
    property TABLENAME : string index 25 read FTablename write FTablename;//Numberset
    property POOL : string index 25 read FPool write FPool;//NumberPool
    property START: Integer read FStart write FStart;
    property STOP: Integer read FStop write FStop;
    property USE : string index 200 read FUse write FUse;
    property NOTICE : string read FNotice write FNotice;
    property CREATEDBY : string index 4 read FCreatedBy write FCreatedBy;
  end;
  TNumberPools = class(TBaseDBDataSet)
  private
    FName,FTyp,FPool : string;
    FStart,fActual,FStop : Integer;
  public
  published
    property NAME : string index 25 read FName write FName;//Numberset
    property START: Integer read FStart write FStart;
    property ACTUAL: Integer read fActual write FActual;
    property STOP: Integer read FStop write FStop;
  end;
  TPayGroups = class(TBaseDBDataSet)
  private
    FName : string;
    FCosts,FValue : Double;
  public
  published
    property NAME : string index 60 read FName write Fname;
    property COSTS: Double read FCosts write FCosts;
    property VALUE: Double read FValue write FValue;
  end;
  TAuthSources = class(TBaseDBDataset)
  private
    FType,FName,FServer,FUser,FPassword,FFilter,FBase : string;
  public
    function Authenticate(aUser,aPassword : string) : Boolean;
  published
    property TYP : string index 4 read FType write FType;//LDAP
    property NAME : string index 255 read FName write FName;
    property SERVER : string index 255 read FServer write FServer;
    property USER : string index 255 read FUser write FUser;
    property PASSWORD : string index 255 read FPassword write FPassword;
    property FILTER : string index 255 read FFilter write FFilter;
    property BASE : string index 255 read FBase write FBase;
  end;
  TMandantDetails = class(TBaseDBDataSet)
  private
    FName,FAdress,FSortcode,FAccount,FInstitute,FTel2,FTel3,FTel4,FTel1,
    FFax,FMail,FAddition1,FAddition2,FAddition3,FAddition4,FAddition5,FAddition6,FAddition7,FAddition8,
    FDBStatements,FInternet : string;
    FDBVersion,FDbVer : Integer;
    FStamp : Int64;
    FImage : TRawBlob;
  public
    AuthSources : TAuthSources;
  published
    property NAME : string index 160 read FName write FName;
    property ADRESS : string read FAdress write FAdress;
    property SORTCODE : string index 20 read FSortcode write FSortcode;
    property ACCOUNT : string index 200 read FAccount write FAccount;
    property INSTITUTE : string index 60 read FInstitute write FInstitute;
    property TEL1 : string index 30 read FTel1 write FTel1;
    property TEL2 : string index 30 read FTel2 write FTel2;
    property TEL3 : string index 30 read FTel3 write FTel3;
    property TEL4 : string index 30 read FTel4 write FTel4;
    property FAX : string index 30 read FFax write FFax;
    property MAIL : string index 50 read FMail write FMail;
    property INTERNET : string index 50 read FInternet write FInternet;
    property ADDITION1 : string index 200 read FAddition1 write FAddition1;
    property ADDITION2 : string index 200 read FAddition2 write FAddition2;
    property ADDITION3 : string index 200 read FAddition3 write FAddition3;
    property ADDITION4 : string index 200 read FAddition4 write FAddition4;
    property ADDITION5 : string index 200 read FAddition5 write FAddition5;
    property ADDITION6 : string index 200 read FAddition6 write FAddition6;
    property ADDITION7 : string index 200 read FAddition7 write FAddition7;
    property ADDITION8 : string index 200 read FAddition8 write FAddition8;
    property DBVERSION: Integer read FDBVersion write FDBVersion;
    property STAMP: Int64 read FStamp write FStamp;
    property IMAGE: TRawBlob read FImage write FImage;
    property DBSTATEMENTS : string read FDBStatements write FDBStatements;
    property DBVER: Integer read FDbVer write FDBVer;
  end;
  TRights = class(TBaseDBDataSet)
  private
    FRightName : string;
    fRights : SmallInt;
  public
    function Right(Element: string;Recursive : Boolean = True;UseCache : Boolean = True) : Integer;
  published
    property RIGHTNAME : string index 20 read FRightName write FRightName;
    property RIGHTS : SmallInt read FRights write FRights;
  end;
{
  TPermissions = class(TBaseDBDataSet)
  public
    constructor Create(aOwner: TComponent); override;
    procedure DefineFields(aDataSet : TDataSet);override;
  published
    property REF_ID_ID: Int64;
    property USER: Int64;
    property RIGHT : SmallInt read FRight write FRight;
  end;
  TTree = class(TBaseDBDataSet)
  private
    function GetText: TField;
  public
    constructor CreateEx(aOwner: TComponent; DM: TComponent;
     aConnection: TComponent=nil; aMasterdata: TDataSet=nil); override;
    procedure Open;override;
    procedure ImportStandartEntrys;
    procedure DefineFields(aDataSet : TDataSet);override;
    property Text : TField read GetText;
  published
    property PARENT: Int64;
    property TYP : string index 1;
    property NAME : string index 60;
    property LINK : string index 400;
    property ICON: Integer;
    property DESC : string index 200;
  end;
  TForms = class(TBaseDBDataSet)
  public
    procedure DefineFields(aDataSet : TDataSet);override;
  published
    property TYP : string index 3;
    property NAME : string index 60;
    property FORM: TSQLRawBlob;
  end;
  TReports = class(TBaseDBDataSet)
  public
    procedure DefineFields(aDataSet : TDataSet);override;
    procedure Open; override;
  published
    property TYP : string index 6;
    property NAME : string index 60;
    property STANDARD : string index 1;
    property CHANGEDBY : string index 4;
    property LANGUAGE : string index 3;
    property EMAIL : string index 200;
    property EMAILCC : string index 200;
    property EMAILBCC : string index 200;
    property REPORT: TSQLRawBlob;
    property TEXT : string;
  end;
  TFollowers = class(TBaseDBDataSet)
  private
    function GetLink: TField;
  public
    procedure DefineFields(aDataSet : TDataSet);override;
    procedure Open; override;
    function BuildFilter : string;
    property Link : TField read GetLink;
  published
    property LINK : string index 400;
  end;
  TFilters = class(TBaseDBDataSet)
  public
    constructor Create(aOwner: TComponent); override;
    procedure DefineFields(aDataSet : TDataSet);override;
    procedure FillDefaults(aDataSet : TDataSet);override;
  published
    property TYP : string index 1;
    property NAME : string index 60;
    property FILTER : string;
    property FILTERIN : string index 100;
    property STANDART : string index 1;
    property SORTDIR : string index 4;
    property SORTFIELD : string index 20;
    property USER : string index 20;
  end;
  TLinks = class(TBaseDBDataSet)
  private
    FOrigFilter : string;
  public
    constructor CreateEx(aOwner : TComponent;DM : TComponent=nil;aConnection : TComponent = nil;aMasterdata : TDataSet = nil);override;
    procedure Open;override;
    procedure DefineFields(aDataSet : TDataSet);override;
    function Add(aLink : string) : Boolean;
  published
    property RREF_ID: Int64;
    property LINK : string index 400;
    property LINK_REF_ID: Int64;
    property ICON: Integer;
    property NAME : string index 400;
    property REFERENCE : string index 30;
    property CHANGEDBY : string index 4;
    property CREATEDBY : string index 4;
  end;
  TListEntrys = class(TBaseDBDataSet)
  private
    FList: TBaseDbList;
  public
    procedure DefineFields(aDataSet : TDataSet);override;
    property List : TBaseDbList read FList write FList;
  published
    property ACTIVE : string index 1;
    property NAME : string index 60;
    property LINK : string index 400;
    property ICON: Integer;
  end;
  TLists = class(TBaseDBList)
  private
    FEntrys: TListEntrys;
  public
    constructor CreateEx(aOwner : TComponent;DM : TComponent=nil;aConnection : TComponent = nil;aMasterdata : TDataSet = nil);override;
    destructor Destroy;override;
    procedure DefineFields(aDataSet : TDataSet);override;
    function CreateTable : Boolean;override;
    function GetTextFieldName: string;override;
    function GetNumberFieldName : string;override;
    property Entrys : TListEntrys read FEntrys;
  published
    property NAME : string index 60;
  end;
  TBoilerplate = class(TBaseDBDataSet)
  public
    procedure DefineFields(aDataSet : TDataSet);override;
  published
    property NAME : string index 100;
    property TEXT: TSQLRawBlob;
  end;
  TImages = class(TBaseDBDataSet)
  public
    constructor CreateEx(aOwner: TComponent; DM: TComponent;
      aConnection: TComponent=nil; aMasterdata: TDataSet=nil); override;
    procedure DefineFields(aDataSet : TDataSet);override;
    procedure GenerateThumbnail(aThumbnail : TBaseDbDataSet);
  published
    property REF_ID: Int64;
    property IMAGE: TSQLRawBlob;
  end;
  TDeletedItems = class(TBaseDBDataSet)
  public
    procedure DefineFields(aDataSet : TDataSet);override;
  published
    property REF_ID_ID: Int64;
    property LINK : string index 400;
  end;
  TMeasurementData = class(TBaseDBDataset)
  public
    procedure DefineFields(aDataSet: TDataSet); override;
    constructor CreateEx(aOwner: TComponent; DM: TComponent;
      aConnection: TComponent=nil; aMasterdata: TDataSet=nil); override;
    procedure FillDefaults(aDataSet: TDataSet); override;
  published
    property DATA: Double;
    property DATE : TDateTime;
  end;
  TMeasurement = class(TBaseDBDataset)
    procedure DataSetAfterPost(aDataSet: TDataSet);
    procedure DataSetBeforeEdit(aDataSet: TDataSet);
    procedure FDSDataChange(Sender: TObject; Field: TField);
  private
    CurrentChanged : Boolean;
    CurrentValue : real;
    FMesdata: TMeasurementData;
    FDS: TDataSource;
    function GetCurrent: TField;
  public
    constructor CreateEx(aOwner: TComponent; DM: TComponent; aConnection: TComponent=nil; aMasterdata: TDataSet=nil); override;
    destructor Destroy; override;
    procedure DefineFields(aDataSet: TDataSet); override;
    function CreateTable: Boolean; override;
    property Data : TMeasurementData read FMesdata;
    property Current : TField read GetCurrent;
  published
    property NAME : string index 100;
    property ID : string index 100;
    property TYP : string index 100;
    property CURRENT: Double;
    property MUNIT : string index 15;
    property CHART : string index 1;
    property COLOR : string index 30;
    property RANGE : string index 20;
    property POSITION : string index 1;
    property INTERPOLATE : string index 1;
    property TOLLERANCE: Double;
  end;
}
implementation

uses md5,sha1;

{ TOption }

class function TOption.GetRealTableName: string;
begin
  Result:='OPTIONS';
end;

{ TOptions }

class function TOptions.GetObjectTyp: TClass;
begin
  Result := TOption;
end;

procedure TList.Add(Value: T);
begin
  SetLength(Items, Length(Items) + 1);
  Items[Length(Items) - 1] := Value;
end;

{ TBaseDBDataset }

constructor TBaseDBDataset.CreateEx(Module: TComponent; Owner: TComponent);
begin
end;

function TRights.Right(Element: string; Recursive: Boolean; UseCache: Boolean
  ): Integer;
begin

end;

function TAuthSources.Authenticate(aUser, aPassword: string): Boolean;
begin

end;

function TNumberRanges.NewRangefromPool(aPool, aName: string; aCount: Integer;
  aUse, aNotice: string): Boolean;
begin

end;

function TNumberRanges.NewRangewithoutPool(aName: string; aFrom,
  aCount: Integer; aUse, aNotice: string; aPool: string): Boolean;
begin

end;

function TNumbersets.GetNewNumber(Numberset: string): string;
begin

end;

function TNumbersets.HasNumberSet(Numberset: string): Boolean;
begin

end;

function TActiveUsers.Delete: Boolean;
begin
  Result:=inherited Delete;
end;

function TUser.MergeSalt(apasswort, aSalt: string): string;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to length(aPasswort)-1 do
    begin
      Result += copy(aSalt,0,5);
      aSalt := copy(aSalt,6,length(aSalt));
      result += copy(aPasswort,0,1);
      aPasswort := copy(aPasswort,2,length(aPasswort));
    end;
end;

function TUser.GetRandomSalt: string;
var
  aSalt: String;
  aGUID: TGUID;
begin
  CreateGUID(aGUID);
  aSalt := md5Print(md5String(GUIDToString(aGUID)+NAME));
  CreateGUID(aGUID);
  aSalt += md5Print(md5String(GUIDToString(aGUID)+aSalt));
  CreateGUID(aGUID);
  aSalt += md5Print(md5String(GUIDToString(aGUID)));
  aSalt :=copy(aSalt,0,104);
  Result := aSalt;
end;

function TUser.CheckUserPasswort(aPasswort: string): Boolean;
var
  aSalt, aRes: String;
begin
  Result := False;
  if copy(PASSWORD,0,1) <> '$' then
    Result := md5print(MD5String(aPasswort)) = PASSWORD
  else
    begin
      aSalt := SALT;
      aRes := '$'+SHA1Print(SHA1String(SHA1Print(SHA1String(MergeSalt(aPasswort,aSalt)))));
      Result := (copy(aRes,0,length(PASSWORD)) = PASSWORD) and (length(PASSWORD) > 30);
    end;
end;

procedure TUser.SetPasswort(aPasswort: string);
var
  aGUID: TGUID;
  aSalt: String;
  aRes: String;
begin
  SALT:=GetRandomSalt;
  aRes := '$'+SHA1Print(SHA1String(SHA1Print(SHA1String(MergeSalt(aPasswort,SALT)))));
  PASSWORD:=aRes;
end;

constructor TUser.Create;
begin
  FOptions := TOptions.Create;
end;

constructor TUser.CreateEx(Module: TComponent; Owner: TComponent);
begin
  inherited CreateEx(Module, Owner);
end;

class function TUser.GetRealTableName: string;
begin
  Result:='USERS';
end;

class function TUser.MapField(aField: string): string;
begin
  Result:=inherited MapField(aField);
  case Result of
  'TYP':Result := 'TYPE';
  end;
end;

{ TBaseDbList }

function TBaseDbList.GetActive: Boolean;
begin

end;

function TBaseDbList.GetBookNumber: TField;
begin

end;

function TBaseDbList.GetMatchcode: TField;
begin

end;

function TBaseDbList.GetBarcode: TField;
begin

end;

function TBaseDbList.GetCommission: TField;
begin

end;

function TBaseDbList.GetDescription: TField;
begin

end;

function TBaseDbList.GetStatus: TField;
begin

end;

function TBaseDbList.GetText: TField;
begin

end;

function TBaseDbList.GetNumber: TField;
begin

end;

destructor TBaseDbList.Destroy;
begin
  inherited Destroy;
end;

function TBaseDbList.GetStatusIcon: Integer;
begin

end;

function TBaseDbList.GetTyp: string;
begin

end;

function TBaseDbList.GetMatchcodeFieldName: string;
begin

end;

function TBaseDbList.GetBarcodeFieldName: string;
begin

end;

function TBaseDbList.GetCommissionFieldName: string;
begin

end;

function TBaseDbList.GetDescriptionFieldName: string;
begin

end;

function TBaseDbList.GetStatusFieldName: string;
begin

end;

function TBaseDbList.GetBookNumberFieldName: string;
begin

end;

function TBaseDbList.Delete: Boolean;
begin
  Result:=inherited Delete;
end;

function TBaseDbList.Find(aIdent: string; Unsharp: Boolean): Boolean;
begin

end;

procedure TBaseDbList.OpenItem(AccHistory: Boolean);
begin

end;

procedure TBaseDbList.BuildSearchIndexes;
begin

end;

procedure TBaseDbList.CascadicPost;
begin
  inherited CascadicPost;
end;

procedure TBaseDbList.GenerateThumbnail;
begin

end;

function TBaseDbList.SelectFromLink(aLink: string): Boolean;
begin

end;

function TBaseDbList.SelectFromNumber(aNumber: string): Boolean;
begin

end;

function TBaseDbList.SelectFromTreeEntry(aParent: LargeInt): Boolean;
begin

end;

function TBaseDbList.ChangeStatus(aNewStatus: string): Boolean;
begin

end;

function TBaseDbList.Duplicate: Boolean;
begin

end;

end.

