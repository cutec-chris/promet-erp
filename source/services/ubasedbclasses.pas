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

implementation

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
  Mapping^.ExtFieldNames[i] := 'TYPE';
  //Mapping^.SetOptions(Mapping^.Options+[rpmNoCreateMissingField]);
  //Mapping^.MapFields([
                     //'TYP','"TYPE"'//,
                     //'PASSWORD','"PASSWORD"',
                     //'POSITION','"POSITION"'
  //                   ]);
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

