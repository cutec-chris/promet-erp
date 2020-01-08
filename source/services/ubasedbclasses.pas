unit ubasedbclasses;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uBaseDatasetInterfaces, db, SynCommons, mORMot;

type
  TBaseDBDataset = class(TAbstractDBDataset)
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
  TOptions = class(TBaseDBDataSet)
  private
    FOption,FValue : RawUTF8;
    fREF_ID : TRecordReference;
  public
    class procedure DefineFields(aDataSet : TDataSet);override;
    function GetOption(aSection, aIdent, DefaultValue: string): string;
    procedure SetOption(aSection,aIdent, Value : string);
  published
    property REF_ID: TRecordReference read fREF_ID write fREF_ID;
    property OPTION : RawUTF8 index 60 read FOption;
    property VALUE : RawUTF8 write FValue;
  end;
  TUser = class(TBaseDBDataset)
  private
    FAcc: RawUTF8;
    FAuthSource: RawUTF8;
    FCustomerNo: RawUTF8;
    FDep: RawUTF8;
    FEmployment: TDateTime;
    FIDCode: RawUTF8;
    FLastLogin: TDateTime;
    Fleaved: TDateTime;
    FLogin: RawUTF8;
    FLoginActive: Boolean;
    FMail: RawUTF8;
    FName: RawUTF8;
    FParent: Int64;
    FPaygroup: Int64;
    fPersNo: RawUTF8;
    FPosition: RawUTF8;
    FRemoteAcc: Boolean;
    FSalt: RawUTF8;
    FType: RawUTF8;
    FUseWorktime: Integer;
    FWeekWorktime: Integer;
    FWorktime: Integer;
    FPassword : RawUTF8;
    Foptions : TOptions;
  public
    procedure FillDefaults(aDataSet : TDataSet);override;
    class procedure DefineFields(aDataSet: TDataSet); override;
    class procedure MapFields(Mapping: PSQLRecordPropertiesMapping); override;
  published
    property TYP : RawUTF8 index 1 read FType write FType;
    property PARENT : Int64 read FParent write FParent;
    property ACCOUNTNO : RawUTF8 index 20 read FAcc write FAcc;
    property NAME : RawUTF8 index 30 read FName write Fname;
    property PASSWORD : RawUTF8 index 45 read FPassword write FPassword;
    property SALT : RawUTF8 index 105 read FSalt write FSalt;
    property IDCODE : RawUTF8 index 4 read FIDCode write FIdCode;
    property EMPLOYMENT : TDateTime read FEmployment write FEmployment;
    property LEAVED : TDateTime read Fleaved write Fleaved;
    property CUSTOMERNO : RawUTF8 index 20 read FCustomerNo write FCustomerno;
    property PERSONNELNO : RawUTF8 index 20 read fPersNo write FPersNo;
    property DEPARTMENT : RawUTF8 index 30 read FDep write FDep;
    property POSITION : RawUTF8 index 30 read FPosition write FPosition;
    property LOGINNAME : RawUTF8 index 30 read FLogin write FLogin;
    property EMAIL : RawUTF8 index 100 read FMail write FMail;
    property PAYGROUP : Int64 read FPaygroup write FPaygroup;
    property WORKTIME: Integer read FWorktime write FWorktime; //8 wenn NULL
    property WEEKWORKTIME : Integer read FWeekWorktime write FWeekworktime;//40 wenn NULL
    property USEWORKTIME : Integer read FUseWorktime write FUseWorktime;
    property LOGINACTIVE : Boolean read FLoginActive write FLoginActive;
    property REMOTEACCESS : Boolean read FRemoteAcc write FRemoteAcc;
    property LASTLOGIN : TDateTime read FLastLogin write FLastLogin;
    property AUTHSOURCE : RawUTF8 index 10 read FAuthSource write FAuthSource;
    property Options : TOptions read FOptions;
  end;
  TActiveUsers = class(TBaseDBDataSet)
  private
    FCOMMAND,FAccountNo,FName,FClient,FHost,FVersion: RawUTF8;
    FEXPIRES: TDateTime;
  public
    class procedure DefineFields(aDataSet : TDataSet);override;
    function Delete : Boolean; override;
  published
    property ACCOUNTNO : RawUTF8 index 20 read FAccountNo write FAccountNo;
    property NAME : RawUTF8 index 30 read FName write FName;
    property CLIENT : RawUTF8 index 50 read FClient write FClient;
    property HOST : RawUTF8 index 50 read FHost write FHost;
    property VERSION : RawUTF8 index 25 read FVersion write FVersion;
    property COMMAND : RawUTF8 read FCOMMAND write FCOMMAND;
    property EXPIRES : TDateTime read FEXPIRES write FEXPIRES;
  end;
  TUserfielddefs = class(TBaseDBDataSet)
  private
    FTTable,FTField,FTyp : RawUTF8;
    FSize : Integer;
  public
    class procedure DefineFields(aDataSet : TDataSet);override;
  published
    property TTABLE : RawUTF8 index 25 read FTTable write FTTable;
    property TFIELD : RawUTF8 index 10 read FTField write FTField;
    property TYP : RawUTF8 index 10 read FTyp write FTyp;
    property SIZE: Integer read FSize write FSize;
  end;
  TNumbersets = class(TBaseDBDataSet)
  private
    FTablename,FTyp,FPool : RawUTF8;
    FIncr,fActual,FStop : Integer;
  public
    class procedure DefineFields(aDataSet : TDataSet);override;
    function GetNewNumber(Numberset : string) : string;
    function HasNumberSet(Numberset : string) : Boolean;
  published
    property TABLENAME : RawUTF8 index 25 read FTablename write FTablename;
    property TYP : RawUTF8 index 1 read FTyp write FTyp;
    property INCR: Integer read FIncr write FIncr;
    property ACTUAL: Integer read fActual write FActual;
    property STOP: Integer read FStop write FStop;
    property POOL : RawUTF8 index 25 read FPool write FPool;//NumberPool
  end;
  TNumberRanges = class(TBaseDBDataSet)
  private
    FTablename,FTyp,FPool,FUse,FNotice,FCreatedBy : RawUTF8;
    FIncr,FStop,FStart : Integer;
  public
    class procedure DefineFields(aDataSet : TDataSet);override;
    function NewRangefromPool(aPool, aName: string; aCount: Integer; aUse,
      aNotice: string): Boolean;
    function NewRangewithoutPool(aName: string; aFrom, aCount: Integer; aUse,
      aNotice: string; aPool: string=''): Boolean;
  published
    property TABLENAME : RawUTF8 index 25 read FTablename write FTablename;//Numberset
    property POOL : RawUTF8 index 25 read FPool write FPool;//NumberPool
    property START: Integer read FStart write FStart;
    property STOP: Integer read FStop write FStop;
    property USE : RawUTF8 index 200 read FUse write FUse;
    property NOTICE : RawUTF8 read FNotice write FNotice;
    property CREATEDBY : RawUTF8 index 4 read FCreatedBy write FCreatedBy;
  end;
  TNumberPools = class(TBaseDBDataSet)
  private
    FName,FTyp,FPool : RawUTF8;
    FStart,fActual,FStop : Integer;
  public
    class procedure DefineFields(aDataSet : TDataSet);override;
  published
    property NAME : RawUTF8 index 25 read FName write FName;//Numberset
    property START: Integer read FStart write FStart;
    property ACTUAL: Integer read fActual write FActual;
    property STOP: Integer read FStop write FStop;
  end;
  TPayGroups = class(TBaseDBDataSet)
  private
    FName : RawUTF8;
    FCosts,FValue : Double;
  public
    class procedure DefineFields(aDataSet : TDataSet);override;
  published
    property NAME : RawUTF8 index 60 read FName write Fname;
    property COSTS: Double read FCosts write FCosts;
    property VALUE: Double read FValue write FValue;
  end;
  TAuthSources = class(TBaseDBDataset)
  private
    FType,FName,FServer,FUser,FPassword,FFilter,FBase : RawUTF8;
  public
    class procedure DefineFields(aDataSet : TDataSet);override;
    function Authenticate(aUser,aPassword : string) : Boolean;
  published
    property TYP : RawUTF8 index 4 read FType write FType;//LDAP
    property NAME : RawUTF8 index 255 read FName write FName;
    property SERVER : RawUTF8 index 255 read FServer write FServer;
    property USER : RawUTF8 index 255 read FUser write FUser;
    property PASSWORD : RawUTF8 index 255 read FPassword write FPassword;
    property FILTER : RawUTF8 index 255 read FFilter write FFilter;
    property BASE : RawUTF8 index 255 read FBase write FBase;
  end;
  TMandantDetails = class(TBaseDBDataSet)
  private
    FName,FAdress,FSortcode,FAccount,FInstitute,FTel2,FTel3,FTel4,FTel1,
    FFax,FMail,FAddition1,FAddition2,FAddition3,FAddition4,FAddition5,FAddition6,FAddition7,FAddition8,
    FDBStatements,FInternet : RawUTF8;
    FDBVersion,FDbVer : Integer;
    FStamp : Int64;
    FImage : TSQLRawBlob;
  public
    AuthSources : array of TAuthSources;
    class procedure DefineFields(aDataSet : TDataSet);override;
  published
    property NAME : RawUTF8 index 160 read FName write FName;
    property ADRESS : RawUTF8 read FAdress write FAdress;
    property SORTCODE : RawUTF8 index 20 read FSortcode write FSortcode;
    property ACCOUNT : RawUTF8 index 200 read FAccount write FAccount;
    property INSTITUTE : RawUTF8 index 60 read FInstitute write FInstitute;
    property TEL1 : RawUTF8 index 30 read FTel1 write FTel1;
    property TEL2 : RawUTF8 index 30 read FTel2 write FTel2;
    property TEL3 : RawUTF8 index 30 read FTel3 write FTel3;
    property TEL4 : RawUTF8 index 30 read FTel4 write FTel4;
    property FAX : RawUTF8 index 30 read FFax write FFax;
    property MAIL : RawUTF8 index 50 read FMail write FMail;
    property INTERNET : RawUTF8 index 50 read FInternet write FInternet;
    property ADDITION1 : RawUTF8 index 200 read FAddition1 write FAddition1;
    property ADDITION2 : RawUTF8 index 200 read FAddition2 write FAddition2;
    property ADDITION3 : RawUTF8 index 200 read FAddition3 write FAddition3;
    property ADDITION4 : RawUTF8 index 200 read FAddition4 write FAddition4;
    property ADDITION5 : RawUTF8 index 200 read FAddition5 write FAddition5;
    property ADDITION6 : RawUTF8 index 200 read FAddition6 write FAddition6;
    property ADDITION7 : RawUTF8 index 200 read FAddition7 write FAddition7;
    property ADDITION8 : RawUTF8 index 200 read FAddition8 write FAddition8;
    property DBVERSION: Integer read FDBVersion write FDBVersion;
    property STAMP: Int64 read FStamp write FStamp;
    property IMAGE: TSQLRawBlob read FImage write FImage;
    property DBSTATEMENTS : RawUTF8 read FDBStatements write FDBStatements;
    property DBVER: Integer read FDbVer write FDBVer;
  end;
  TRights = class(TBaseDBDataSet)
  private
    FRightName : RawUTF8;
    fRights : SmallInt;
  public
    class procedure DefineFields(aDataSet : TDataSet);override;
    procedure ResetCache;
    function Right(Element: string;Recursive : Boolean = True;UseCache : Boolean = True) : Integer;
  published
    property RIGHTNAME : RawUTF8 index 20 read FRightName write FRightName;
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
    property TYP : RawUTF8 index 1;
    property NAME : RawUTF8 index 60;
    property LINK : RawUTF8 index 400;
    property ICON: Integer;
    property DESC : RawUTF8 index 200;
  end;
  TForms = class(TBaseDBDataSet)
  public
    procedure DefineFields(aDataSet : TDataSet);override;
  published
    property TYP : RawUTF8 index 3;
    property NAME : RawUTF8 index 60;
    property FORM: TSQLRawBlob;
  end;
  TReports = class(TBaseDBDataSet)
  public
    procedure DefineFields(aDataSet : TDataSet);override;
    procedure Open; override;
  published
    property TYP : RawUTF8 index 6;
    property NAME : RawUTF8 index 60;
    property STANDARD : RawUTF8 index 1;
    property CHANGEDBY : RawUTF8 index 4;
    property LANGUAGE : RawUTF8 index 3;
    property EMAIL : RawUTF8 index 200;
    property EMAILCC : RawUTF8 index 200;
    property EMAILBCC : RawUTF8 index 200;
    property REPORT: TSQLRawBlob;
    property TEXT : RawUTF8;
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
    property LINK : RawUTF8 index 400;
  end;
  TFilters = class(TBaseDBDataSet)
  public
    constructor Create(aOwner: TComponent); override;
    procedure DefineFields(aDataSet : TDataSet);override;
    procedure FillDefaults(aDataSet : TDataSet);override;
  published
    property TYP : RawUTF8 index 1;
    property NAME : RawUTF8 index 60;
    property FILTER : RawUTF8;
    property FILTERIN : RawUTF8 index 100;
    property STANDART : RawUTF8 index 1;
    property SORTDIR : RawUTF8 index 4;
    property SORTFIELD : RawUTF8 index 20;
    property USER : RawUTF8 index 20;
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
    property LINK : RawUTF8 index 400;
    property LINK_REF_ID: Int64;
    property ICON: Integer;
    property NAME : RawUTF8 index 400;
    property REFERENCE : RawUTF8 index 30;
    property CHANGEDBY : RawUTF8 index 4;
    property CREATEDBY : RawUTF8 index 4;
  end;
  TListEntrys = class(TBaseDBDataSet)
  private
    FList: TBaseDbList;
  public
    procedure DefineFields(aDataSet : TDataSet);override;
    property List : TBaseDbList read FList write FList;
  published
    property ACTIVE : RawUTF8 index 1;
    property NAME : RawUTF8 index 60;
    property LINK : RawUTF8 index 400;
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
    property NAME : RawUTF8 index 60;
  end;
  TBoilerplate = class(TBaseDBDataSet)
  public
    procedure DefineFields(aDataSet : TDataSet);override;
  published
    property NAME : RawUTF8 index 100;
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
    property LINK : RawUTF8 index 400;
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
    property NAME : RawUTF8 index 100;
    property ID : RawUTF8 index 100;
    property TYP : RawUTF8 index 100;
    property CURRENT: Double;
    property MUNIT : RawUTF8 index 15;
    property CHART : RawUTF8 index 1;
    property COLOR : RawUTF8 index 30;
    property RANGE : RawUTF8 index 20;
    property POSITION : RawUTF8 index 1;
    property INTERPOLATE : RawUTF8 index 1;
    property TOLLERANCE: Double;
  end;
}
implementation

{ TOptions }

class procedure TOptions.DefineFields(aDataSet: TDataSet);
begin
  with aDataSet as IBaseManageDB do
    TableName:='OPTIONS';
end;

function TOptions.GetOption(aSection, aIdent, DefaultValue: string): string;
begin
end;

procedure TOptions.SetOption(aSection, aIdent, Value: string);
begin
end;

{ TRights }

class procedure TRights.DefineFields(aDataSet: TDataSet);
begin
  with aDataSet as IBaseManageDB do
    TableName:='RIGHTS';
end;

procedure TRights.ResetCache;
begin

end;

function TRights.Right(Element: string; Recursive: Boolean; UseCache: Boolean
  ): Integer;
begin

end;

{ TMandantDetails }

class procedure TMandantDetails.DefineFields(aDataSet: TDataSet);
begin
  with aDataSet as IBaseManageDB do
    TableName:='MANDANTDETAILS';
end;

{ TAuthSources }

class procedure TAuthSources.DefineFields(aDataSet: TDataSet);
begin
  with aDataSet as IBaseManageDB do
    TableName:='AUTHSOURCES';
end;

function TAuthSources.Authenticate(aUser, aPassword: string): Boolean;
begin

end;

{ TPayGroups }

class procedure TPayGroups.DefineFields(aDataSet: TDataSet);
begin
  with aDataSet as IBaseManageDB do
    TableName:='PAYGROUPS';
end;

{ TNumberPools }

class procedure TNumberPools.DefineFields(aDataSet: TDataSet);
begin
  with aDataSet as IBaseManageDB do
    TableName:='NUMBERPOOLS';
end;

{ TNumberRanges }

class procedure TNumberRanges.DefineFields(aDataSet: TDataSet);
begin
  with aDataSet as IBaseManageDB do
    TableName:='NUMBERRANGES';
end;

function TNumberRanges.NewRangefromPool(aPool, aName: string; aCount: Integer;
  aUse, aNotice: string): Boolean;
begin

end;

function TNumberRanges.NewRangewithoutPool(aName: string; aFrom,
  aCount: Integer; aUse, aNotice: string; aPool: string): Boolean;
begin

end;

{ TNumbersets }

class procedure TNumbersets.DefineFields(aDataSet: TDataSet);
begin
  with aDataSet as IBaseManageDB do
    TableName:='NUMBERS';
end;

function TNumbersets.GetNewNumber(Numberset: string): string;
begin

end;

function TNumbersets.HasNumberSet(Numberset: string): Boolean;
begin

end;

{ TUserfielddefs }

class procedure TUserfielddefs.DefineFields(aDataSet: TDataSet);
begin
  with aDataSet as IBaseManageDB do
    TableName:='USERFIELDS';
end;

{ TActiveUsers }

class procedure TActiveUsers.DefineFields(aDataSet: TDataSet);
begin
  with aDataSet as IBaseManageDB do
    TableName:='ACTIVEUSERS';
end;

function TActiveUsers.Delete: Boolean;
begin
  Result:=inherited Delete;
end;

procedure TUser.FillDefaults(aDataSet: TDataSet);
begin
  inherited FillDefaults(aDataSet);
end;

class procedure TUser.DefineFields(aDataSet: TDataSet);
begin
  with aDataSet as IBaseManageDB do
    TableName:='USERS';
end;

class procedure TUser.MapFields(Mapping: PSQLRecordPropertiesMapping);
var
  i: Integer;
begin
  inherited MapFields(Mapping);
  i := Mapping^.Properties.Fields.IndexByNameOrExcept('TYP');
  Mapping^.MapFields([
                     'TYP','TYPE'
                     ]);
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

