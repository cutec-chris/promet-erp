unit ubasedbclasses;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uBaseDatasetInterfaces, db, SynCommons, mORMot;

type
  TBaseDBDataset = class(TAbstractDBDataset)
  end;

  { TBaseDbList }

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

  { TUser }

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
  end;

  { TActiveUsers }

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

  { TUserfielddefs }

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
  public
    procedure DefineFields(aDataSet : TDataSet);override;
    function GetNewNumber(Numberset : string) : string;
    function HasNumberSet(Numberset : string) : Boolean;
    destructor Destroy; override;
  published
    property TABLENAME : RawUTF8 index 25;
    property TYP : RawUTF8 index 1;
    property INCR: Integer;
    property ACTUAL: Integer;
    property STOP: Integer;
    property POOL : RawUTF8 index 25;//NumberPool
  end;
  TNumberRanges = class(TBaseDBDataSet)
  public
    procedure DefineFields(aDataSet : TDataSet);override;
    function NewRangefromPool(aPool, aName: string; aCount: Integer; aUse,
      aNotice: string): Boolean;
    function NewRangewithoutPool(aName: string; aFrom, aCount: Integer; aUse,
      aNotice: string; aPool: string=''): Boolean;
  published
    property TABLENAME : RawUTF8 index 25;//Numberset
    property POOL : RawUTF8 index 25;//NumberPool
    property START: Integer;
    property STOP: Integer;
    property USE : RawUTF8 index 200;
    property NOTICE : RawUTF8;
    property CREATEDBY : RawUTF8 index 4;
  end;
  TNumberPools = class(TBaseDBDataSet)
  public
    procedure DefineFields(aDataSet : TDataSet);override;
  published
    property NAME : RawUTF8 index 25;//Numberset
    property START: Integer;
    property ACTUAL: Integer;
    property STOP: Integer;
  end;
  TPayGroups = class(TBaseDBDataSet)
  public
    procedure DefineFields(aDataSet : TDataSet);override;
  published
    property NAME : RawUTF8 index 60;
    property COSTS: TSynExtended;
    property VALUE: TSynExtended;
  end;
  TAuthSources = class(TBaseDBDataset)
  public
    procedure DefineFields(aDataSet : TDataSet);override;
    function Authenticate(aUser,aPassword : string) : Boolean;
  published
    property TYP : RawUTF8 index 4;//LDAP
    property NAME : RawUTF8 index 255;
    property SERVER : RawUTF8 index 255;
    property USER : RawUTF8 index 255;
    property PASSWORD : RawUTF8 index 255;
    property FILTER : RawUTF8 index 255;
    property BASE : RawUTF8 index 255;
  end;
  TMandantDetails = class(TBaseDBDataSet)
  private
    FAuthSources: TAuthSources;
  public
    constructor CreateEx(aOwner : TComponent;DM : TComponent=nil;aConnection : TComponent = nil;aMasterdata : TDataSet = nil);override;
    function CreateTable: Boolean; override;
    destructor Destroy; override;
    procedure DefineFields(aDataSet : TDataSet);override;
    property AuthSources : TAuthSources read FAuthSources;
  published
    property NAME : RawUTF8 index 160;
    property ADRESS : RawUTF8;
    property SORTCODE : RawUTF8 index 20;
    property ACCOUNT : RawUTF8 index 200;
    property INSTITUTE : RawUTF8 index 60;
    property TEL1 : RawUTF8 index 30;
    property TEL2 : RawUTF8 index 30;
    property TEL3 : RawUTF8 index 30;
    property TEL4 : RawUTF8 index 30;
    property FAX : RawUTF8 index 30;
    property MAIL : RawUTF8 index 50;
    property INTERNET : RawUTF8 index 50;
    property ADDITION1 : RawUTF8 index 200;
    property ADDITION2 : RawUTF8 index 200;
    property ADDITION3 : RawUTF8 index 200;
    property ADDITION4 : RawUTF8 index 200;
    property ADDITION5 : RawUTF8 index 200;
    property ADDITION6 : RawUTF8 index 200;
    property ADDITION7 : RawUTF8 index 200;
    property ADDITION8 : RawUTF8 index 200;
    property DBVERSION: Integer;
    property STAMP: Int64;
    property IMAGE: TSQLRawBlob;
    property DBSTATEMENTS : RawUTF8;
    property DBVER: Integer;
  end;
  TRights = class(TBaseDBDataSet)
  private
    FCachedRights : TStringList;
    UserTable: TUser;
  public
    constructor CreateEx(aOwner : TComponent;DM : TComponent=nil;aConnection : TComponent = nil;aMasterdata : TDataSet = nil);override;
    destructor Destroy;override;
    procedure Open; override;
    procedure DefineFields(aDataSet : TDataSet);override;
    property Users : TUser read UserTable write UserTable;
    procedure ResetCache;
    function Right(Element: string;Recursive : Boolean = True;UseCache : Boolean = True) : Integer;
  published
    property RIGHTNAME : RawUTF8 index 20;
    property RIGHTS',ftSmallInt,0;
  end;
  TPermissions = class(TBaseDBDataSet)
  public
    constructor Create(aOwner: TComponent); override;
    procedure DefineFields(aDataSet : TDataSet);override;
  published
    property REF_ID_ID: Int64;
    property USER: Int64;
    property RIGHT',ftSmallInt,0;
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
  TOptions = class(TBaseDBDataSet)
  public
    constructor Create(aOwner: TComponent); override;
    procedure DefineFields(aDataSet : TDataSet);override;
    procedure Open; override;
    function GetOption(aSection, aIdent, DefaultValue: string): string;
    procedure SetOption(aSection,aIdent, Value : string);
  published
    property OPTION : RawUTF8 index 60;
    property VALUE : RawUTF8;
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
    property DATA: TSynExtended;
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
    property CURRENT: TSynExtended;
    property MUNIT : RawUTF8 index 15;
    property CHART : RawUTF8 index 1;
    property COLOR : RawUTF8 index 30;
    property RANGE : RawUTF8 index 20;
    property POSITION : RawUTF8 index 1;
    property INTERPOLATE : RawUTF8 index 1;
    property TOLLERANCE: TSynExtended;
  end;


implementation

{ TUserfielddefs }

class procedure TUserfielddefs.DefineFields(aDataSet: TDataSet);
begin
  with aDataSet as IBaseManageDB do
    TableName:='USERFIELDS';
end;

{ TActiveUsers }

procedure TActiveUsers.DefineFields(aDataSet: TDataSet);
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

