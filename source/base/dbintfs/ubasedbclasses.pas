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
unit uBaseDbClasses;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, db, Variants, uIntfStrConsts, DOM,
  Contnrs,uBaseDatasetInterfaces,fpjson,jsonparser
  ;
type
  { TBaseDBDataset }
  TJSONExportMode = (emStandard,emDhtmlX,emExtJS);
  TReplaceFieldFunc = procedure(aField : TField;aOldValue : string;var aNewValue : string) of object;

  TBaseDBDataset = class(TSQLRecord)
  private
    FOnChanged: TNotifyEvent;
    FOnRemoved: TNotifyEvent;
    FDisplayLabelsWasSet : Boolean;
    function GetTimestamp: TField;
    function GetID: TField;
    procedure DataSetToJSON(ADataSet: TDataSet; AJSON: TJSONArray;
      const ADateAsString: Boolean; Fields: TStringList);
  public
    constructor Create(aOwner: TComponent); override;
    constructor CreateEx(aOwner: TComponent; DM: TComponent;
      aConnection: TComponent=nil; aMasterdata: TDataSet=nil); override;
    procedure Open; override;
    procedure DefineFields(aDataSet: TDataSet); override;
    procedure DefineDefaultFields(aDataSet : TDataSet;HasMasterSource : Boolean);override;
    procedure DefineUserFields(aDataSet: TDataSet);override;
    procedure FillDefaults(aDataSet : TDataSet);override;
    procedure Select(aID : Variant);virtual;
    procedure SelectChangedSince(aDate : TDateTime);virtual;
    procedure Append;override;
    procedure Insert;override;
    procedure SetDisplayLabelName(aDataSet: TDataSet;aField, aName: string);
    procedure SetDisplayLabels(aDataSet : TDataSet);override;
    function GetBookmark: variant;
    function GotoBookmark(aRec : Variant) : Boolean;
    procedure FreeBookmark(aRec : Variant);
    property Id : TField read GetID;
    property TimeStamp : TField read GetTimestamp;
    function GetLink : string;
    function ExportToXML: string;virtual;
    function ExportToJSON(mode : TJSONExportMode = emStandard): string;virtual;
    procedure ImportFromXML(XML : string;OverrideFields : Boolean = False;ReplaceFieldFunc : TReplaceFieldFunc = nil);virtual;
    procedure ImportFromJSON(JSON : string;OverrideFields : Boolean = False;ReplaceFieldFunc : TReplaceFieldFunc = nil);virtual;
    procedure ObjectToJSON(AObject: TBaseDBDataSet; AJSON: TJSONObject;
      const ADateAsString: Boolean; mode: TJSONExportMode); virtual;
    procedure DuplicateRecord(DoPost : Boolean = False);virtual;
    procedure Assign(Source: TPersistent); override;
    procedure DirectAssign(Source : TPersistent);virtual;
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
    constructor CreateEx(aOwner : TComponent;DM : TComponent=nil;aConnection : TComponent = nil;aMasterdata : TDataSet = nil);override;
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
  TBaseDBDatasetClass = class of TBaseDBDataset;
  TBaseDBListClass = class of TBaseDBList;

  TBaseDBFields = class(TBaseDBDataset)
  public
    procedure DefineFields(aDataSet : TDataSet);override;
  end;

  { TBaseDBTables }

  TBaseDBTables = class(TBaseDBDataset)
  private
    FFields: TBaseDBFields;
  public
    constructor CreateEx(aOwner: TComponent; DM: TComponent;
      aConnection: TComponent=nil; aMasterdata: TDataSet=nil); override;
    procedure DefineFields(aDataSet : TDataSet);override;
    property Fields : TBaseDBFields read FFields;
  end;

  { TBaseHistory }

  TBaseHistory = class(TBaseDBList)
  private
    FHChanged: Boolean;
    FShouldChange : Boolean;
  protected
    procedure OpenItem(AccHistory: Boolean=True); override;
  public
    constructor CreateEx(aOwner : TComponent;DM : TComponent=nil;aConnection : TComponent = nil;aMasterdata : TDataSet = nil);override;
    procedure SetDisplayLabels(aDataSet: TDataSet); override;
    property ChangedDuringSession : Boolean read FHChanged write FHChanged;
    procedure DefineFields(aDataSet : TDataSet);override;
    procedure Change; override;

    procedure SelectByParent(aParent: Variant);
    procedure SelectByParentStr(aParent: string);
    procedure SelectByRoot(aParent: Variant);

    function AddItem(aObject: TDataSet; aAction: string; aLink: string=''; aReference: string=''; aRefObject: TDataSet=nil; aIcon: Integer=0;aComission: string=''; CheckDouble: Boolean=True; DoPost: Boolean=True; DoChange: Boolean=False) : Boolean; virtual;
    function AddItemSR(aObject: TDataSet; aAction: string; aLink: string=''; aReference: string=''; aRefObject: string=''; aIcon: Integer=0;aComission: string=''; CheckDouble: Boolean=True; DoPost: Boolean=True; DoChange: Boolean=False) : Boolean; virtual;
    function AddItemPlain(aObject: string; aAction: string; aLink: string=''; aReference: string=''; aRefObject: string=''; aIcon: Integer=0;aComission: string=''; CheckDouble: Boolean=True; DoPost: Boolean=True; DoChange: Boolean=False) : Boolean; virtual;
    procedure AddParentedItem(aObject: TDataSet; aAction: string;aParent : Variant; aLink: string=''; aReference: string=''; aRefObject: TDataSet=nil; aIcon: Integer=0; aComission: string=''; CheckDouble: Boolean=True; DoPost: Boolean=True; DoChange: Boolean=False); virtual;
    procedure AddParentedItemPlain(aObject: string; aAction: string;aParent: Variant; aLink: string; aReference: string; aRefObject: string;aIcon: Integer; aComission: string; CheckDouble: Boolean; DoPost: Boolean;DoChange: Boolean); virtual;
    procedure AddItemWithoutUser(aObject : TDataSet;aAction : string;aLink : string = '';aReference : string = '';aRefObject : TDataSet = nil;aIcon : Integer = 0;aComission : string = '';CheckDouble: Boolean=True;DoPost : Boolean = True;DoChange : Boolean = False);virtual;

    procedure AddMessageItem(aObject: TDataSet; aMessage, aSubject, aSource, aLink: string; aParent: LargeInt = 0);
    procedure AddAnsweredMessageItem(aObject: TDataSet; aMessage, aSubject, aSource, aLink: string; aParent: LargeInt = 0);

    function GetTextFieldName: string;override;
    function GetNumberFieldName : string;override;
  end;
  IBaseHistory = interface['{8BA16E96-1A06-49E2-88B1-301CF9E5C8FC}']
    function GetHistory: TBaseHistory;
    property History : TBaseHistory read GetHistory;
  end;
  IBaseStructure = interface['{5CC59C2C-3A70-48F1-82BB-68A358017938}']
    function GetParentField : string;
    function GetStructureElements(aIndex : Integer) : TBaseDbDataSet;
  end;
  TImages = class;
  TLinks = class;
  TMeasurement = class;

  { TObjects }

  TObjects = class(TBaseDbList)
  private
    FHistory: TBaseHistory;
    FImages: TImages;
    FLinks: TLinks;
    FMeasurement: TMeasurement;
  protected
    function GetNumberFieldName: string; override;
    function GetMatchcodeFieldName: string; override;
    function GetTextFieldName: string; override;
    function GetStatusFieldName: string; override;
  public
    constructor CreateEx(aOwner: TComponent; DM: TComponent;
      aConnection: TComponent=nil; aMasterdata: TDataSet=nil); override;
    destructor Destroy; override;
    function CreateTable: Boolean; override;
    procedure DefineFields(aDataSet : TDataSet);override;
    procedure FillDefaults(aDataSet: TDataSet); override;
    function SelectByLink(aLink: string): Boolean;
    procedure SelectByRefId(aId : Variant);
    property History : TBaseHistory read FHistory;
    property Images : TImages read FImages;
    property Links : TLinks read FLinks;
    property Measurements : TMeasurement read FMeasurement;
  end;

  { TVariables }

  TVariables = class(TBaseDBDataset)
  private
    function GetString(aName : string): string;
    procedure SetString(aName : string; AValue: string);
  protected
  public
    procedure DefineFields(aDataSet : TDataSet);override;
    procedure Add(aName,aId : string;aValue : Double);
    property StringValue[aName : string] : string read GetString write SetString;
  end;
  TOptions = class;
  TFollowers = class;
  TRights = class;
  TPayGroups = class;

  { TUser }

  TUser = class(TBaseDbList,IBaseHistory)
  private
    FFollows: TFollowers;
    FOptions: TOptions;
    FRights: TRights;
    FHistory: TBaseHistory;
    FPayGroups: TPayGroups;
    function GetAcc: TField;
    function GetIDCode: TField;
    function GetLeaved: TField;
    function GetPasswort: TField;
    function GetSalt: TField;
    function GetUser: TField;
    function GetTextFieldName: string;override;
    function GetNumberFieldName : string;override;
    function GetWorktime: Extended;
    function MergeSalt(apasswort,aSalt : string) : string;
    function GetHistory: TBaseHistory;
  public
    constructor CreateEx(aOwner : TComponent;DM : TComponent=nil;aConnection : TComponent = nil;aMasterdata : TDataSet = nil);overload;override;
    destructor Destroy;override;
    procedure Open; override;
    function GetTyp: string; override;
    procedure DefineFields(aDataSet : TDataSet);override;
    procedure FillDefaults(aDataSet : TDataSet);override;
    procedure SelectByParent(aParent : Variant);
    function CreateTable : Boolean;override;
    property UserName : TField read GetUser;
    property Accountno : TField read GetAcc;
    property Leaved : TField read GetLeaved;
    property Passwort : TField read GetPasswort;
    property Salt : TField read GetSalt;
    property Rights : TRights read FRights;
    property Options : TOptions read FOptions;
    property Follows : TFollowers read FFollows;
    procedure SetPasswort(aPasswort : string);
    function GetRandomSalt : string;
    function CheckUserPasswort(aPasswort : string) : Boolean;
    procedure LoginWasOK;
    procedure SelectByAccountno(aAccountno : string);virtual;
    function GetLeaderAccountno : string;
    property History : TBaseHistory read FHistory;
    property WorkTime : Extended read GetWorktime;
    property IDCode : TField read GetIDCode;
  end;

  { TDatabaseTables }

  TActiveUsers = class(TBaseDBDataSet)
  public
    procedure DefineFields(aDataSet : TDataSet);override;
    function Delete : Boolean; override;
  end;
  TUserfielddefs = class(TBaseDBDataSet)
  public
    procedure DefineFields(aDataSet : TDataSet);override;
  end;

  { TNumbersets }

  TNumbersets = class(TBaseDBDataSet)
  public
    procedure DefineFields(aDataSet : TDataSet);override;
    function GetNewNumber(Numberset : string) : string;
    function HasNumberSet(Numberset : string) : Boolean;
    destructor Destroy; override;
  end;

  { TNumberRanges }

  TNumberRanges = class(TBaseDBDataSet)
  public
    procedure DefineFields(aDataSet : TDataSet);override;
    function NewRangefromPool(aPool, aName: string; aCount: Integer; aUse,
      aNotice: string): Boolean;
    function NewRangewithoutPool(aName: string; aFrom, aCount: Integer; aUse,
      aNotice: string; aPool: string=''): Boolean;
  end;

  { TNumberPools }

  TNumberPools = class(TBaseDBDataSet)
  public
    procedure DefineFields(aDataSet : TDataSet);override;
  end;
  TPayGroups = class(TBaseDBDataSet)
  public
    procedure DefineFields(aDataSet : TDataSet);override;
  end;

  { TAuthSources }

  TAuthSources = class(TBaseDBDataset)
  public
    procedure DefineFields(aDataSet : TDataSet);override;
    function Authenticate(aUser,aPassword : string) : Boolean;
  end;

  { TMandantDetails }

  TMandantDetails = class(TBaseDBDataSet)
  private
    FAuthSources: TAuthSources;
  public
    constructor CreateEx(aOwner : TComponent;DM : TComponent=nil;aConnection : TComponent = nil;aMasterdata : TDataSet = nil);override;
    function CreateTable: Boolean; override;
    destructor Destroy; override;
    procedure DefineFields(aDataSet : TDataSet);override;
    property AuthSources : TAuthSources read FAuthSources;
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
  end;

  { TPermissions }

  TPermissions = class(TBaseDBDataSet)
  public
    constructor Create(aOwner: TComponent); override;
    procedure DefineFields(aDataSet : TDataSet);override;
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
  end;
  TForms = class(TBaseDBDataSet)
  public
    procedure DefineFields(aDataSet : TDataSet);override;
  end;

  { TReports }

  TReports = class(TBaseDBDataSet)
  public
    procedure DefineFields(aDataSet : TDataSet);override;
    procedure Open; override;
  end;

  { TOptions }

  TOptions = class(TBaseDBDataSet)
  public
    constructor Create(aOwner: TComponent); override;
    procedure DefineFields(aDataSet : TDataSet);override;
    procedure Open; override;
    function GetOption(aSection, aIdent, DefaultValue: string): string;
    procedure SetOption(aSection,aIdent, Value : string);
  end;

  { TFollowers }

  TFollowers = class(TBaseDBDataSet)
  private
    function GetLink: TField;
  public
    procedure DefineFields(aDataSet : TDataSet);override;
    procedure Open; override;
    function BuildFilter : string;
    property Link : TField read GetLink;
  end;

  { TFilters }

  TFilters = class(TBaseDBDataSet)
  public
    constructor Create(aOwner: TComponent); override;
    procedure DefineFields(aDataSet : TDataSet);override;
    procedure FillDefaults(aDataSet : TDataSet);override;
  end;
  TLinks = class(TBaseDBDataSet)
  private
    FOrigFilter : string;
  public
    constructor CreateEx(aOwner : TComponent;DM : TComponent=nil;aConnection : TComponent = nil;aMasterdata : TDataSet = nil);override;
    procedure Open;override;
    procedure DefineFields(aDataSet : TDataSet);override;
    function Add(aLink : string) : Boolean;
  end;
  TListEntrys = class(TBaseDBDataSet)
  private
    FList: TBaseDbList;
  public
    procedure DefineFields(aDataSet : TDataSet);override;
    property List : TBaseDbList read FList write FList;
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
  end;
  TBoilerplate = class(TBaseDBDataSet)
  public
    procedure DefineFields(aDataSet : TDataSet);override;
  end;

  { TImages }

  TImages = class(TBaseDBDataSet)
  public
    constructor CreateEx(aOwner: TComponent; DM: TComponent;
      aConnection: TComponent=nil; aMasterdata: TDataSet=nil); override;
    procedure DefineFields(aDataSet : TDataSet);override;
    procedure GenerateThumbnail(aThumbnail : TBaseDbDataSet);
  end;
  TDeletedItems = class(TBaseDBDataSet)
  public
    procedure DefineFields(aDataSet : TDataSet);override;
  end;
  TMeasurementData = class(TBaseDBDataset)
  public
    procedure DefineFields(aDataSet: TDataSet); override;
    constructor CreateEx(aOwner: TComponent; DM: TComponent;
      aConnection: TComponent=nil; aMasterdata: TDataSet=nil); override;
    procedure FillDefaults(aDataSet: TDataSet); override;
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
  end;
  TNumberHelper = class(TBaseDBDataSet)
  public
    procedure DefineFields(aDataSet : TDataSet);override;
    function CreateTable: Boolean; override;
  end;
var ImportAble : TClassList;
implementation
uses uBaseDBInterface, uBaseApplication, uBaseSearch,XMLRead,XMLWrite,Utils,
  md5,sha1,uData,uthumbnails,base64,uMeasurement,usync,ldapsend,uimpvcal;
resourcestring
  strNumbersetDontExists        = 'Nummernkreis "%s" existiert nicht !';
  strNumbersetEmpty             = 'Nummernkreis "%s" ist leer !';
  strDeletedmessages            = 'gelöschte Narichten';
  strlogmessages                = 'Logs';
  strSendMessages               = 'gesendete Narichten';
  strArchive                    = 'Archiv';
  strUnknownMessages            = 'Unbekannte Narichten';
  strSpam                       = 'Spam';
  strUnsorted                   = 'Unsortiert';
  strCheckingtable              = 'Prüfe Tabelle '+lineending+'%s';
  strPasteToanWrongDataSet      = 'Diese Daten können nicht in diese Art Eintrag eingefügt werden';
  strOpeningtable               = 'öffne Tabelle '+lineending+'%s';
  strFailedShowingDataFrom      = 'konnten Daten von %s nicht anzeigen';
  strTablename                  = 'Tabellenname';
  strAccountNo                  = 'Kontonummer';
  strPassword                   = 'Passwort';
  strLanguage                   = 'Sprache';
  strStatus                     = 'Status';
  strAction                     = 'Aktion';
  strChangedby                  = 'geändert von';
  strUser                       = 'Benutzer';
  strRead                       = 'gelesen';
  strSender                     = 'Absender';
  strDate                       = 'Datum';
  strTime                       = 'Zeit';
  strSubject                    = 'Betreff';
  strMatchcode                  = 'Suchbegriff';
  strCurrency                   = 'Währung';
  strPaymentTarget              = 'Zahlungsziel';
  strCreatedDate                = 'erstellt am';
  strChangedDate                = 'geändert am';
  strInstitute                  = 'Institut';
  strDescription                = 'Beschreibung';
  strData                       = 'Daten';
  strEmployee                   = 'Mitarbeiter';
  strDepartment                 = 'Abteilung';
  strPosition                   = 'Position';
  strShorttext                  = 'Kurztext';
  strQuantityUnit               = 'Mengeneinheit';
  strVat                        = 'MwSt';
  strUnit                       = 'Einheit';
  strPriceType                  = 'Preistyp';
  strPrice                      = 'Preis';
  strMinCount                   = 'Min.Anzahl';
  strMaxCount                   = 'Max.Anzahl';
  strValidfrom                  = 'gültig ab';
  strValidTo                    = 'gültig bis';
  strProblem                    = 'Problem';
  strAssembly                   = 'Baugruppe';
  strPart                       = 'Bauteil';
  strPlace                      = 'Ort';
  strNumber                     = 'Nummer';
  strCustomerNumber             = 'Kundennummer';
  strHalfVat                    = 'Halbe MwSt';
  strFullVat                    = 'Volle MwSt';
  strNetprice                   = 'Nettopreis';
  strDiscount                   = 'Rabatt';
  strGrossPrice                 = 'Bruttopreis';
  strDone                       = 'erledigt';
  strorderNo                    = 'Vorgangsnummer';
  strTitle                      = 'Titel';
  strAdditional                 = 'Zusätzlich';
  strAdress                     = 'Adresse';
  strCity                       = 'Stadt';
  strPostalcode                 = 'Postleitzahl';
  strState                      = 'Bundesland';
  strLand                       = 'Land';
  strPostBox                    = 'Postfach';
  strPosNo                      = 'Pos.Nr.';
  strTenderPosNo                = 'Aus.Nr.';
  strIdent                      = 'Nummer';
  strTexttyp                    = 'Text Typ';
  strText                       = 'Text';
  strReference                  = 'Referenz';
  strStorage                    = 'Lager';
  strReserved                   = 'Reserviert';
  strProperty                   = 'Eigenschaft';
  strValue                      = 'Wert';
  strQuantityDelivered          = 'Menge geliefert';
  strQuantityCalculated         = 'Menge berechnet';
  strPurchasePrice              = 'Einkaufspreis';
  strSellPrice                  = 'Verkaufspreis';
  strCommonprice                = 'Allgemeinpreis';
  strBallance                   = 'Kontostand';
  strPurpose                    = 'Zahlungsgrund';
  strChecked                    = 'geprüft';
  strCategory                   = 'Kategorie';
  strPaid                       = 'bezahlt';
  strDelivered                  = 'geliefert';
  strActive                     = 'Aktiv';
  strLink                       = 'Verweis';
  strTask                       = 'Aufgabe';
  strEnd                        = 'Ende';
  strPause                      = 'Pause';
  strOriginaldate               = 'Originaldatum';
  strNotes                      = 'Notizen';
  strOwner                      = 'Eigentümer';
  strAvalible                   = 'Verfügbar';
  strNeedsAction                = 'benötigt Hilfe';
  strCostCentre                 = 'Kostenstelle';

{ TNumberPools }

procedure TNumberPools.DefineFields(aDataSet: TDataSet);
begin
  with aDataSet as IBaseManageDB do
    begin
      TableName := 'NUMBERPOOLS';
      if Assigned(ManagedFieldDefs) then
        with ManagedFieldDefs do
          begin
            Add('NAME',ftString,25,True);//Numberset
            Add('START',ftInteger,0,False);
            Add('ACTUAL',ftInteger,0,False);
            Add('STOP',ftInteger,0,False);
          end;
    end;
end;

{ TNumberRanges }

procedure TNumberRanges.DefineFields(aDataSet: TDataSet);
begin
  with aDataSet as IBaseManageDB do
    begin
      TableName := 'NUMBERRANGES';
      if Assigned(ManagedFieldDefs) then
        with ManagedFieldDefs do
          begin
            Add('TABLENAME',ftString,25,True);//Numberset
            Add('POOL',ftString,25,True);//NumberPool
            Add('START',ftInteger,0,True);
            Add('STOP',ftInteger,0,True);
            Add('USE',ftString,200,False);
            Add('NOTICE',ftmemo,0,False);
            Add('CREATEDBY',ftString,4,False);
          end;
    end;
end;

function TNumberRanges.NewRangefromPool(aPool, aName: string;aCount : Integer; aUse,
  aNotice: string): Boolean;
begin
  Data.NumberPools.Open;
  Result := Data.NumberPools.Locate('NAME',aPool,[]);
  if not Result then exit;
  Result := NewRangewithoutPool(aName,Data.NumberPools.FieldByName('ACTUAL').AsInteger,aCount,aUse,aNotice,aPool);
  if not Result then exit;
  try
    Data.NumberPools.Edit;
    Data.NumberPools.FieldByName('ACTUAL').AsInteger:=Data.NumberPools.FieldByName('ACTUAL').AsInteger+aCount;
    Data.NumberPools.Post;
  except
    Result := False;
  end;
end;

function TNumberRanges.NewRangewithoutPool(aName: string; aFrom,
  aCount: Integer; aUse, aNotice: string;aPool : string = ''): Boolean;
begin
  Result := False;
  Filter(TBaseDBModule(DataModule).QuoteField('START')+'<'+IntToStr(aFrom)
+' AND '+TBaseDBModule(DataModule).QuoteField('STOP')+'>'+IntToStr(aFrom)
+' AND '+TBaseDBModule(DataModule).QuoteField('POOL')+'='+TBaseDBModule(DataModule).QuoteValue(aPool)
+' AND '+TBaseDBModule(DataModule).QuoteField('NAME')+'='+TBaseDBModule(DataModule).QuoteValue(aName)
  );
  if Count>0 then exit;//Start is already in an existing Range
  Filter('');
  Insert;
  FieldByName('TABLENAME').AsString := aName;
  FieldByName('POOL').AsString:=aPool;//NumberPool
  FieldByName('START').AsInteger:=aFrom;
  FieldByName('STOP').AsInteger:=aFrom+aCount;
  FieldByName('USE').AsString := aUse;
  FieldByName('NOTICE').AsString:=aNotice;
  Post;
  result := True;
end;

{ TAuthSources }

procedure TAuthSources.DefineFields(aDataSet: TDataSet);
begin
  with aDataSet as IBaseManageDB do
    begin
      TableName := 'AUTHSOURCES';
      if Assigned(ManagedFieldDefs) then
        with ManagedFieldDefs do
          begin
            Add('TYPE',ftString,4,True);//LDAP
            Add('NAME',ftString,255,True);
            Add('SERVER',ftString,255,True);
            Add('USER',ftString,255,False);
            Add('PASSWORD',ftString,255,False);
            Add('FILTER',ftString,255,False);
            Add('BASE',ftString,255,False);
          end;
    end;
end;

{ TDatabaseTables }

procedure TBaseDBFields.DefineFields(aDataSet: TDataSet);
begin
  with aDataSet as IBaseManageDB do
    begin
      TableName := 'TABLEFIELDS';
      if Assigned(ManagedFieldDefs) then
        with ManagedFieldDefs do
          begin
            Add('NAME',ftString,50,True);
            Add('ALIAS',ftString,150,False);
            Add('TYPE',ftString,1,True);
            {
              C:NVARCHAR
              T:TEXT
              I:INTEGER
              D:DATE
              B:BLOB
            }
            Add('SIZE',ftInteger,0,True);
          end;
      if Assigned(ManagedIndexdefs) then
        with ManagedIndexDefs do
          begin
            Add('NAME','NAME',[ixUnique]);
            Add('ALIAS','ALIAS',[]);
          end;
    end;
end;

constructor TBaseDBTables.CreateEx(aOwner: TComponent; DM: TComponent;
  aConnection: TComponent; aMasterdata: TDataSet);
begin
  inherited CreateEx(aOwner, DM, aConnection, aMasterdata);
  with BaseApplication as IBaseDbInterface do
    begin
      with DataSet as IBaseDBFilter do
        begin
          Limit := 0;
          FetchRows:=0;
        end;
    end;
end;

procedure TBaseDBTables.DefineFields(aDataSet: TDataSet);
begin
  with aDataSet as IBaseManageDB do
    begin
      TableName := 'DBTABLES';
      if Assigned(ManagedFieldDefs) then
        with ManagedFieldDefs do
          begin
            Add('NAME',ftString,50,True);
            Add('SHEMA',ftString,150,False);
            Add('SHEMAFILE',ftString,150,False);
            Add('ALIAS',ftString,150,False);
            Add('DBVERSION',ftInteger,0,False);
          end;
      if Assigned(ManagedIndexdefs) then
        with ManagedIndexDefs do
          begin
            Add('NAME','NAME',[ixUnique]);
            Add('ALIAS','ALIAS',[]);
          end;
    end;
end;

procedure TBoilerplate.DefineFields(aDataSet: TDataSet);
begin
  with aDataSet as IBaseManageDB do
    begin
      TableName := 'BOILERPLATE';
      if Assigned(ManagedFieldDefs) then
        with ManagedFieldDefs do
          begin
            Add('NAME',ftString,100,True);
            Add('TEXT',ftBlob,0,False);
          end;
      if Assigned(ManagedIndexdefs) then
        with ManagedIndexDefs do
          begin
            Add('NAME','NAME',[ixUnique]);
          end;
    end;
end;

{ TNumberHelper }

procedure TNumberHelper.DefineFields(aDataSet: TDataSet);
begin
  with aDataSet as IBaseManageDB do
    begin
      TableName := 'HELPER_NUMBER';
      if Assigned(ManagedFieldDefs) then
        with ManagedFieldDefs do
          begin
            Add('NUMBER',ftInteger,0,True);
          end;
    end;
end;

function TNumberHelper.CreateTable: Boolean;
var
  i: Integer;
begin
  with DataSet as IBaseManageDB do
    SetUpStdFields(False);
  if not TBaseDBModule(DataModule).TableExists(Self.TableName) then
    begin
      Result:=inherited CreateTable;
      if Result then
        begin
          Open;
          for i := 0 to 1024 do
            begin
              Append;
              FieldByName('NUMBER').AsInteger:=i;
            end;
          Post;
        end;
    end;
end;

{ TVariables }

function TVariables.GetString(aName : string): string;
begin
  Filter(TBaseDBModule(DataModule).QuoteField('NAME')+'='+TBaseDBModule(DataModule).QuoteValue(aName));
  if Count>0 then
    Result := FieldByName('VALUES').AsString
  else Result := '';
end;

procedure TVariables.SetString(aName : string; AValue: string);
begin
  Filter(TBaseDBModule(DataModule).QuoteField('NAME')+'='+TBaseDBModule(DataModule).QuoteValue(aName));
  if Count>0 then
    Edit
  else Append;
  FieldByName('NAME').AsString:=aName;
  FieldByName('VALUES').AsString:=AValue;
  Post;
end;

procedure TVariables.DefineFields(aDataSet: TDataSet);
begin
  with aDataSet as IBaseManageDB do
    begin
      TableName := 'VARIABLES';
      if Assigned(ManagedFieldDefs) then
        with ManagedFieldDefs do
          begin
            Add('NAME',ftString,100,True);
            Add('ID',ftString,100,False);
            Add('VALUE',ftFloat,0,True);
            Add('VALUES',ftString,100,True);
          end;
      if Assigned(ManagedIndexdefs) then
        with ManagedIndexDefs do
          begin
            Add('NAME','NAME',[]);
            Add('TIMESTAMPD','TIMESTAMPD',[]);
          end;
    end;
end;

procedure TVariables.Add(aName, aId: string; aValue: Double);
begin
  Insert;
  FieldByName('NAME').AsString:=aName;
  FieldByName('ID').AsString:=aId;
  FieldByName('VALUE').AsFloat := aValue;
  Post;
end;

function TObjects.GetNumberFieldName: string;
begin
  Result := 'NUMBER';
end;
function TObjects.GetMatchcodeFieldName: string;
begin
  Result := 'MATCHCODE';
end;
function TObjects.GetTextFieldName: string;
begin
  Result := 'NAME';
end;

function TObjects.GetStatusFieldName: string;
begin
  Result:='STATUS';
end;

constructor TObjects.CreateEx(aOwner: TComponent; DM: TComponent;
  aConnection: TComponent; aMasterdata: TDataSet);
begin
  inherited CreateEx(aOwner, DM, aConnection, aMasterdata);
  FHistory := TBaseHistory.CreateEx(Self,DataModule,aConnection,DataSet);
  FImages := TImages.CreateEx(Self,DataModule,aConnection,DataSet);
  FLinks := TLinks.CreateEx(Self,DataModule,aConnection);
  FMeasurement := TMeasurement.CreateEx(Self,DataModule,aConnection,DataSet);
  with BaseApplication as IBaseDbInterface do
    begin
      with DataSet as IBaseDBFilter do
        begin
          UsePermissions:=True;
        end;
    end;
end;

destructor TObjects.Destroy;
begin
  FMeasurement.Destroy;
  FImages.Destroy;
  FLinks.Destroy;
  FHistory.Destroy;
  inherited Destroy;
end;

function TObjects.CreateTable: Boolean;
begin
  Result:=inherited CreateTable;
  Result := Result and FHistory.CreateTable;
end;

procedure TObjects.DefineFields(aDataSet: TDataSet);
begin
  with aDataSet as IBaseManageDB do
    begin
      TableName := 'ALLOBJECTS';
      if Assigned(ManagedFieldDefs) then
        with ManagedFieldDefs do
          begin
            Add('NUMBER',ftString,60,False);
            Add('NAME',ftString,200,False);
            Add('MATCHCODE',ftString,200,False);
            Add('VERSION',ftString,25,False);
            Add('LINK',ftString,400,False);
            Add('STATUS',ftString,4,False);
            Add('ICON',ftInteger,0,False);
            Add('NOTICE',ftMemo,0,False);
            Add('TREEENTRY',ftLargeint,0,False);
            Add('CHANGEDBY',ftString,4,False);
            Add('CREATEDBY',ftString,4,False);
          end;
    end;
end;

procedure TObjects.FillDefaults(aDataSet: TDataSet);
begin
  inherited FillDefaults(aDataSet);
  try
    FieldByName('ICON').AsInteger:=TBaseDBModule(DataModule).GetLinkIcon('ALLOBJECTS@',True);
  except
  end;
end;

function TObjects.SelectByLink(aLink: string): Boolean;
var
  tmp: String;
begin
  with BaseApplication as IBaseDBInterface do
    with DataSet as IBaseDBFilter do
      begin
        if copy(aLink,0,11)='ALLOBJECTS@' then
          begin
            tmp := copy(aLink,12,length(aLink));
            if pos('{',aLink)>0 then tmp := copy(tmp,0,pos('{',tmp)-1);
            Filter := TBaseDBModule(DataModule).QuoteField(TableName)+'.'+TBaseDBModule(DataModule).QuoteField('SQL_ID')+'='+TBaseDBModule(DataModule).QuoteValue(tmp);
          end
        else if copy(aLink,0,14)='ALLOBJECTS.ID@' then
          begin
            tmp := copy(aLink,15,length(aLink));
            if pos('{',aLink)>0 then tmp := copy(tmp,0,pos('{',tmp)-1);
            Filter := TBaseDBModule(DataModule).QuoteField(TableName)+'.'+TBaseDBModule(DataModule).QuoteField('SQL_ID')+'='+TBaseDBModule(DataModule).QuoteValue(tmp);
          end
        else
          Filter := TBaseDBModule(DataModule).QuoteField('LINK')+'='+TBaseDBModule(DataModule).QuoteValue(aLink);
      end;
end;

procedure TObjects.SelectByRefId(aId: Variant);
begin
  with BaseApplication as IBaseDbInterface do
    begin
      with Self.DataSet as IBaseDBFilter do
        begin
          if aId <> Null then
            Filter := TBaseDBModule(DataModule).QuoteField(TableName)+'.'+TBaseDBModule(DataModule).QuoteField('SQL_ID')+'='+TBaseDBModule(DataModule).QuoteValue(Format('%d',[Int64(aID)]))
          else
            Filter := TBaseDBModule(DataModule).QuoteField(TableName)+'.'+TBaseDBModule(DataModule).QuoteField('SQL_ID')+'='+TBaseDBModule(DataModule).QuoteValue('0');
          Limit := 0;
        end;
    end;
end;

function TFollowers.GetLink: TField;
begin
  result := FieldByName('LINK');
end;

procedure TFollowers.DefineFields(aDataSet: TDataSet);
begin
  with aDataSet as IBaseManageDB do
    begin
      TableName := 'FOLLOWERS';
      if Assigned(ManagedFieldDefs) then
        with ManagedFieldDefs do
          begin
            Add('LINK',ftString,400,True);
          end;
    end;
end;

procedure TFollowers.Open;
begin
  inherited Open;
end;

function TFollowers.BuildFilter: string;
var
  i: Integer;
  nLink, FFilter2: String;
begin
  First;
  i := 0;
  FFilter2:='';
  while not EOF do
    begin
      nLink := FieldByName('LINK').AsString;
      if pos('{',nLink)>0 then
        begin
          nLink := copy(nLink,0,pos('{',nLink)-1);
          FFilter2:=FFilter2+' OR ('+TBaseDBModule(DataModule).ProcessTerm(TBaseDBModule(DataModule).QuoteField('OBJECT')+'='+TBaseDBModule(DataModule).QuoteValue(nLink+'*'))+')';
        end
      else
        FFilter2:=FFilter2+' OR ('+TBaseDBModule(DataModule).QuoteField('OBJECT')+'='+TBaseDBModule(DataModule).QuoteValue(nLink)+')';
      inc(i);
      Next;
    end;
  Result:=FFilter2;
end;

procedure TPayGroups.DefineFields(aDataSet: TDataSet);
begin
  with aDataSet as IBaseManageDB do
    begin
      TableName := 'PAYGROUPS';
      TableCaption:=strPayGroups;
      if Assigned(ManagedFieldDefs) then
        with ManagedFieldDefs do
          begin
            Add('NAME',ftString,60,False);
            Add('COSTS',ftFloat,0,False);
            Add('VALUE',ftFloat,0,False);
          end;
    end;
end;
procedure TListEntrys.DefineFields(aDataSet: TDataSet);
begin
  with aDataSet as IBaseManageDB do
    begin
      TableName := 'LISTENTRYS';
      if Assigned(ManagedFieldDefs) then
        with ManagedFieldDefs do
          begin
            Add('ACTIVE',ftString,1,False);
            Add('NAME',ftString,60,False);
            Add('LINK',ftString,400,False);
            Add('ICON',ftInteger,0,False);
          end;
    end;
end;
constructor TLists.CreateEx(aOwner: TComponent; DM: TComponent;
  aConnection: TComponent; aMasterdata: TDataSet);
begin
  inherited CreateEx(aOwner, DM, aConnection, aMasterdata);
  FEntrys := TListEntrys.CreateEx(Owner,DM,aConnection,DataSet);
  FEntrys.List := Self;
  with BaseApplication as IBaseDbInterface do
    begin
      with DataSet as IBaseDBFilter do
        begin
          UsePermissions:=True;
        end;
    end;
end;

destructor TLists.Destroy;
begin
  FEntrys.Free;
  inherited Destroy;
end;

procedure TLists.DefineFields(aDataSet: TDataSet);
begin
  with aDataSet as IBaseManageDB do
    begin
      TableName := 'LISTS';
      TableCaption := strLists;
      if Assigned(ManagedFieldDefs) then
        with ManagedFieldDefs do
          begin
            Add('NAME',ftString,60,False);
          end;
    end;
end;
function TLists.CreateTable : Boolean;
begin
  Result := inherited CreateTable;
  FEntrys.CreateTable;
end;
function TLists.GetTextFieldName: string;
begin
  Result := 'NAME';
end;
function TLists.GetNumberFieldName: string;
begin
  Result := 'SQL_ID';
end;
procedure TDeletedItems.DefineFields(aDataSet: TDataSet);
begin
  with aDataSet as IBaseManageDB do
    begin
      TableName := 'DELETEDITEMS';
      if Assigned(ManagedFieldDefs) then
        with ManagedFieldDefs do
          begin
            Add('REF_ID_ID',ftLargeInt,0,False);
            Add('LINK',ftString,400,False);
          end;
      if Assigned(ManagedIndexdefs) then
        with ManagedIndexDefs do
          begin
            Add('REF_ID_ID','REF_ID_ID',[]);
            Add('LINK','LINK',[]);
          end;
    end;
end;

constructor TImages.CreateEx(aOwner: TComponent; DM: TComponent;
  aConnection: TComponent; aMasterdata: TDataSet);
begin
  inherited CreateEx(aOwner, DM, aConnection, aMasterdata);
  with DataSet as IBaseManageDB do
    UseIntegrity:=False;
end;

procedure TImages.DefineFields(aDataSet: TDataSet);
begin
  with aDataSet as IBaseManageDB do
    begin
      TableName := 'IMAGES';
      TableCaption:=strImages;
      if Assigned(ManagedFieldDefs) then
        with ManagedFieldDefs do
          begin
            Add('REF_ID',ftLargeInt,0,True);
            Add('IMAGE',ftBlob,0,False);
          end;
    end;
end;

procedure TImages.GenerateThumbnail(aThumbnail: TBaseDbDataSet);
var
  s: TStream;
  GraphExt: String;
  aStream: TMemoryStream;
begin
  if not Self.Active then Self.Open;
  if Self.Count>0 then
    begin
      aThumbnail.Insert;
      aThumbnail.FieldByName('REF_ID_ID').AsVariant:=Self.FieldByName('REF_ID').AsVariant;
      s := DataSet.CreateBlobStream(FieldByName('IMAGE'),bmRead);
      if (S=Nil) or (s.Size = 0) then
      else
        begin
          GraphExt :=  s.ReadAnsiString;
          aStream := TMemoryStream.Create;
          if uthumbnails.GenerateThumbNail('.'+GraphExt,s,aStream,'') then
            begin
              if aStream.Size>0 then
                TBaseDBModule(DataModule).StreamToBlobField(aStream,aThumbnail.DataSet,'THUMBNAIL');
              aThumbnail.Post;
            end;
          aStream.Free;
        end;
      if aThumbnail.State=dsInsert then
        aThumbnail.Cancel;
    end;
end;

constructor TLinks.CreateEx(aOwner: TComponent; DM: TComponent;
  aConnection: TComponent; aMasterdata: TDataSet);
begin
  inherited CreateEx(aOwner, DM, aConnection, aMasterdata);
  with DataSet as IBaseManageDB do
    UseIntegrity:=False;
end;

procedure TLinks.Open;
begin
  with BaseApplication as IBaseDbInterface do
    begin
      with DataSet as IBaseDBFilter do
        begin
        if Assigned(Parent) and Assigned(TBaseDBDataSet(Parent).Id) then
          begin
            if not TBaseDBDataSet(Parent).Id.IsNull then
              BaseFilter := TBaseDBModule(DataModule).QuoteField('RREF_ID')+'='+TBaseDBModule(DataModule).QuoteValue(TBaseDBDataSet(Parent).Id.AsString)
            else
              BaseFilter := TBaseDBModule(DataModule).QuoteField('RREF_ID')+'= 0';
          end;
        end;
    end;
  inherited Open;
end;

procedure TLinks.DefineFields(aDataSet: TDataSet);
begin
  with aDataSet as IBaseManageDB do
    begin
      TableName := 'LINKS';
      TableCaption:=strLinks;
      if Assigned(ManagedFieldDefs) then
        with ManagedFieldDefs do
          begin
            Add('RREF_ID',ftLargeInt,0,true);
            Add('LINK',ftString,400,true);
            Add('LINK_REF_ID',ftLargeInt,0,false);
            Add('ICON',ftInteger,0,False);
            Add('NAME',ftString,400,True);
            Add('REFERENCE',ftString,30,False);
            Add('CHANGEDBY',ftString,4,False);
            Add('CREATEDBY',ftString,4,False);
          end;
    end;
end;

function TLinks.Add(aLink: string): Boolean;
var
  aLinkDesc: String;
  aIcon: Integer;
begin
  Result := False;
  aLinkDesc := TBaseDBModule(DataModule).GetLinkDesc(aLink);
  aIcon := TBaseDBModule(DataModule).GetLinkIcon(aLink);
  if not Active then  Open;
  if Locate('LINK',aLink,[]) then exit;
  Append;
  with DataSet do
    begin
      FieldByName('LINK').AsString := aLink;
      FieldByName('NAME').AsString := aLinkDesc;
      FieldByName('ICON').AsInteger := aIcon;
      FieldByName('CHANGEDBY').AsString := TBaseDBModule(DataModule).Users.Idcode.AsString;
      Post;
      Result := True;
    end;
end;

procedure TUserfielddefs.DefineFields(aDataSet: TDataSet);
begin
  with aDataSet as IBaseManageDB do
    begin
      TableName := 'USERFIELDDEFS';
      if Assigned(ManagedFieldDefs) then
        with ManagedFieldDefs do
          begin
            Add('TTABLE',ftString,25,True);
            Add('TFIELD',ftString,10,True);
            Add('TYPE',ftString,10,True);
            Add('SIZE',ftInteger,0,false);
          end;
    end;
end;

function TBaseDbList.GetActive: Boolean;
var
  aFound: Boolean = False;
begin
  Result := True;
  if not Assigned(Status) then exit;
  if not TBaseDBModule(DataModule).States.DataSet.Locate('TYPE;STATUS',VarArrayOf([GetTyp,DataSet.FieldByName('STATUS').AsString]),[loCaseInsensitive]) then
    begin
      TBaseDBModule(DataModule).SetFilter(TBaseDBModule(DataModule).States,'');
      aFound := TBaseDBModule(DataModule).States.DataSet.Locate('TYPE;STATUS',VarArrayOf([GetTyp,DataSet.FieldByName('STATUS').AsString]),[loCaseInsensitive]);
    end
  else aFound := True;
  if aFound then Result := TBaseDBModule(DataModule).States.FieldByName('ACTIVE').AsBoolean;
end;

function TBaseDbList.GetBookNumber: TField;
var
  aField: String;
begin
  Result := nil;
  aField := GetBookNumberFieldName;
  if aField <> '' then
    Result := DataSet.FieldByName(aField);
end;

function TBaseDbList.GetMatchcode: TField;
var
  aField: String;
begin
  Result := nil;
  aField := GetMatchcodeFieldName;
  if aField <> '' then
    Result := DataSet.FieldByName(aField);
end;
function TBaseDbList.GetBarcode: TField;
var
  aField: String;
begin
  Result := nil;
  aField := GetBarcodeFieldName;
  if aField <> '' then
    Result := DataSet.FieldByName(aField);
end;
function TBaseDbList.GetCommission: TField;
var
  aField: String;
begin
  Result := nil;
  aField := GetCommissionFieldName;
  if aField <> '' then
    Result := DataSet.FieldByName(aField);
end;
function TBaseDbList.GetDescription: TField;
var
  aField: String;
begin
  Result := nil;
  if not Assigned(Self) then exit;
  if not Active then exit;
  aField := GetDescriptionFieldName;
  if aField <> '' then
    Result := DataSet.FieldByName(aField);
end;
function TBaseDbList.GetStatus: TField;
var
  aField: String;
begin
  Result := nil;
  if not Assigned(Self) then exit;
  if not Active then exit;
  aField := GetStatusFieldName;
  if (aField <> '') and (DataSet.FieldDefs.IndexOf(aField)>-1) then
    Result := DataSet.FieldByName(aField);
end;
function TBaseDbList.GetText: TField;
var
  aField: String;
begin
  Result := nil;
  if not Assigned(Self) then exit;
  if not Active then exit;
  aField := GetTextFieldName;
  if aField <> '' then
    Result := DataSet.FieldByName(aField);
end;
function TBaseDbList.GetNumber: TField;
var
  aField: String;
begin
  Result := nil;
  aField := GetNumberFieldName;
  if aField <> '' then
    Result := DataSet.FieldByName(aField);
end;
constructor TBaseDbList.CreateEx(aOwner: TComponent; DM: TComponent;
  aConnection: TComponent; aMasterdata: TDataSet);
begin
  inherited CreateEx(aOwner, DM, aConnection, aMasterdata);
  with BaseApplication as IBaseDbInterface do
    begin
      with DataSet as IBaseDBFilter do
        begin
          BaseSortFields := 'TIMESTAMPD';
          SortFields := 'TIMESTAMPD';
          SortDirection := sdDescending;
          Limit:=500;
        end;
    end;
  FStatusCache := TStringList.Create;
end;

destructor TBaseDbList.Destroy;
begin
  FStatusCache.Free;
  inherited Destroy;
end;

function TBaseDbList.GetStatusIcon: Integer;
var
  aStat: String;
begin
  Result := -1;
  if GetStatusFieldName='' then exit;
  aStat := FStatusCache.Values[FieldByName(GetStatusFieldName).AsString];
  if aStat <> '' then Result := StrToIntDef(aStat,-1)
  else
    begin
      if TBaseDBModule(DataModule).States.DataSet.Locate('TYPE;STATUS',VarArrayOf([GetTyp,FieldByName(GetStatusFieldName).AsString]),[]) then
        Result := StrToIntDef(TBaseDBModule(DataModule).States.DataSet.FieldByName('ICON').AsString,-1)
      else
        begin
          TBaseDBModule(DataModule).SetFilter(TBaseDBModule(DataModule).States,TBaseDBModule(DataModule).QuoteField('TYPE')+'='+TBaseDBModule(DataModule).QuoteValue(GetTyp));
          if TBaseDBModule(DataModule).States.DataSet.Locate('TYPE;STATUS',VarArrayOf([GetTyp,FieldByName(GetStatusFieldName).AsString]),[]) then
            Result := StrToIntDef(TBaseDBModule(DataModule).States.DataSet.FieldByName('ICON').AsString,-1)
        end;
      FStatusCache.Values[FieldByName(GetStatusFieldName).AsString] := IntToStr(Result);
    end;
end;

function TBaseDbList.GetTyp: string;
begin
  Result := '';
end;

function TBaseDbList.GetMatchcodeFieldName: string;
begin
  Result := '';
end;
function TBaseDbList.GetBarcodeFieldName: string;
begin
  Result := '';
end;
function TBaseDbList.GetCommissionFieldName: string;
begin
  Result := '';
end;
function TBaseDbList.GetDescriptionFieldName: string;
begin
  Result := '';
end;
function TBaseDbList.GetStatusFieldName: string;
begin
  Result := '';
end;
function TBaseDbList.GetBookNumberFieldName: string;
begin
  Result := '';
end;

function TBaseDbList.Delete: Boolean;
var
  aObj: TObjects;
begin
  Result:=False;
  try
    aObj := TObjects.CreateEx(nil,DataModule);
    if not TBaseDBModule(DataModule).TableExists(aObj.TableName) then
      begin
        aObj.CreateTable;
        aObj.Free;
        aObj := TObjects.CreateEx(nil,DataModule,nil,DataSet);
      end;
    aObj.SelectByRefId(Id.AsVariant);
    aObj.Open;
    while aObj.Count>0 do
      begin
        aObj.DataSet.Delete;
        Result:=True;
      end;
    aObj.Free;
  except
  end;
  Result := inherited Delete;
end;

procedure TBaseDBDataset.Select(aID: Variant);
var
  aField: String = '';
begin
  with BaseApplication as IBaseDBInterface do
    with DataSet as IBaseDBFilter do
      begin
        with DataSet as IBaseManageDb do
          if ManagedFieldDefs.IndexOf('AUTO_ID') > -1 then
            aField := 'AUTO_ID';
        if aField = '' then aField := 'SQL_ID';
        if (VarIsNumeric(aID) and (aID = 0))
        or (VarIsStr(aID) and (aID = ''))
        or (aID = Null)  then
          begin
            with DataSet as IBaseManageDb do
              Filter := TBaseDBModule(DataModule).QuoteField(TableName)+'.'+TBaseDBModule(DataModule).QuoteField(aField)+'='+TBaseDBModule(DataModule).QuoteValue('0');
          end
        else
          begin
            with DataSet as IBaseManageDb do
              Filter := TBaseDBModule(DataModule).QuoteField(TableName)+'.'+TBaseDBModule(DataModule).QuoteField(aField)+'='+TBaseDBModule(DataModule).QuoteValue(Format('%d',[Int64(aID)]));
          end;
      end;
end;

function TBaseDBDataset.GetBookmark: variant;
begin
  if (not Assigned(Id)) or Id.IsNull then
    Result := 0
  else
    Result := Id.AsVariant;
end;
function TBaseDBDataset.GotoBookmark(aRec: Variant): Boolean;
begin
  Result := DataSet.Active;
  if Result then
    begin
      Result := DataSet.FieldByName('SQL_ID').AsVariant = aRec;
      if not Result then
        Result := DataSet.Locate('SQL_ID',aRec,[]);
    end;
end;
procedure TBaseDBDataset.FreeBookmark(aRec: Variant);
begin
end;

procedure TBaseDBDataset.Insert;
begin
  if not DataSet.Active then
    begin
      Select(0);
      Open;
    end;
  DataSet.Insert;
end;
procedure TBaseDBDataset.Append;
begin
  if not DataSet.Active then
    begin
      Select(0);
      Open;
    end;
  DataSet.Append;
end;

procedure TBaseDBDataset.SelectChangedSince(aDate: TDateTime);
var
  aFilter: String;
begin
  with BaseApplication as IBaseDBInterface do
    with DataSet as IBaseDBFilter do
      begin
        aFilter := '('+TBaseDBModule(DataModule).QuoteField('TIMESTAMPD')+'>';
        aFilter := aFilter+TBaseDBModule(DataModule).DateTimeToFilter(aDate);
        aFilter := aFilter+')';
        Filter := aFilter;
      end;
end;

function TBaseDbList.Find(aIdent: string;Unsharp : Boolean = False): Boolean;
begin
  Result := False;
end;
function TBaseDBDataset.ExportToXML : string;
var
  Stream: TStringStream;
  Doc: TXMLDocument;
  RootNode: TDOMElement;

  procedure RecourseTables(aNode : TDOMNode;aDataSet : TBaseDBDataset);
  var
    i: Integer;
    aData: TDOMElement;
    DataNode: TDOMElement;
    a: Integer;
    Row: TDOMElement;
    tmp: String;
    tmp1: String;
  begin
    if Assigned(aDataSet) then
      begin
        try
          if not (aDataSet is TBaseDBDataset) then exit;
          if not Supports(aDataset.DataSet,IBaseManageDB) then exit;
          with aDataSet.DataSet as IBaseManageDB do
            begin
              if pos('HISTORY',uppercase(TableName)) > 0 then exit;
              if pos('MEASDATA',uppercase(TableName)) > 0 then exit;
              if pos('TASKSNAPSHOTS',uppercase(TableName)) > 0 then exit;
              if Uppercase(TableName) = 'STORAGE' then exit;
              aData := Doc.CreateElement('TABLE');
              aNode.AppendChild(aData);
              with aDataSet.DataSet as IBaseManageDB do
                aData.SetAttribute('NAME',TableName);
            end;
          DataNode := Doc.CreateElement('DATA');
          aData.AppendChild(DataNode);
          aDataSet.Open;
          //aDataSet.Refresh;
          aDataSet.First;
          a := 0;
          while not aDataSet.EOF do
            begin
              inc(a);
              Row := Doc.CreateElement('ROW.'+IntToStr(a));
              DataNode.AppendChild(Row);
              for i := 0 to aDataSet.DataSet.Fields.Count-1 do
                begin
                  tmp := aDataSet.DataSet.Fields[i].FieldName;
                  if (tmp <> '')
                  and (tmp <> 'REF_ID')
                  then
                    begin
                      tmp1 :=  aDataSet.DataSet.Fields[i].AsString;
                      if (not aDataSet.DataSet.Fields[i].IsNull) then
                        begin
                          if aDataSet.DataSet.Fields[i].IsBlob then
                            Row.SetAttribute(tmp,EncodeStringBase64(aDataSet.DataSet.Fields[i].AsString))
                          else
                            Row.SetAttribute(tmp,tmp1);
                        end;
                    end;
                end;
              with aDataSet.DataSet as IBaseSubDataSets do
                begin
                  for i := 0 to (aDataSet.DataSet as IBaseSubDataSets).GetCount-1 do
                    RecourseTables(Row,TBaseDBDataset(SubDataSet[i]));
                end;
              aDataSet.Next;
            end;
        except
        end;
      end;
  end;
begin
  Doc := TXMLDocument.Create;
  RootNode := Doc.CreateElement('TABLES');
  Doc.AppendChild(RootNode);
  RecourseTables(RootNode,Self);
  Stream := TStringStream.Create('');
  WriteXMLFile(Doc,Stream);
  Result := Stream.DataString;
  Doc.Free;
  Stream.Free;
end;

procedure TBaseDBDataset.DataSetToJSON(ADataSet: TDataSet; AJSON: TJSONArray;
  const ADateAsString: Boolean; Fields: TStringList);
var
  VJSON: TJSONObject;
begin
  ADataSet.First;
  while not ADataSet.EOF do
  begin
    VJSON := TJSONObject.Create;
    FieldsToJSON(ADataSet.Fields, VJSON, ADateAsString, Fields);
    AJSON.Add(VJSON);
    ADataSet.Next;
  end;
end;

constructor TBaseDBDataset.Create(aOwner: TComponent);
begin
  CreateEx(aOwner,uData.Data,nil,nil);
end;

constructor TBaseDBDataset.CreateEx(aOwner: TComponent; DM: TComponent;
  aConnection: TComponent; aMasterdata: TDataSet);
begin
  FDisplayLabelsWasSet:=False;
  if DM = nil then
    DM := uData.Data;
  inherited CreateEx(aOwner, DM, aConnection, aMasterdata);
end;

procedure TBaseDBDataset.Open;
begin
  inherited Open;
  if DataSet.Active and FDisplayLabelsWasSet then
    SetDisplayLabels(DataSet);
end;

procedure TBaseDBDataset.ObjectToJSON(AObject: TBaseDBDataSet;
  AJSON: TJSONObject; const ADateAsString: Boolean; mode: TJSONExportMode);
var
  aArray: TJSONArray;
  aNewObj, VJSON: TJSONObject;
  i: Integer;
  aSubObj: TJSONObject;
  aMetadata: TJSONObject;
  WasOpen: Boolean;
begin
  if mode = emStandard then
    begin
      VJSON := TJSONObject.Create;
      FieldsToJSON(AObject.DataSet.Fields, VJSON, ADateAsString);
      AJSON.Add('Fields',VJSON);
      if not Supports(AObject.DataSet,IBaseManageDB) then exit;
      with AObject.DataSet as IBaseSubDataSets do
        for i := 0 to GetCount-1 do
          begin
            WasOpen := TBaseDBDataSet(SubDataSet[i]).Active;
            if WasOpen
            and (TBaseDBDataSet(SubDataSet[i]).TableName<>'TASKSNAPSHOTS')
            and (TBaseDBDataSet(SubDataSet[i]).TableName<>'TASKWORKFLOW')
            and (not ((AObject.TableName='TASKS') and (TBaseDBDataSet(SubDataSet[i]).TableName='HISTORY')))
            then
              begin
                TBaseDBDataSet(SubDataSet[i]).Open;
                TBaseDBDataSet(SubDataSet[i]).First;
                if not TBaseDBDataSet(SubDataSet[i]).EOF then
                  begin
                    aArray := TJSONArray.Create;
                    while not TBaseDBDataSet(SubDataSet[i]).EOF do
                      begin
                        aNewObj := TJSONObject.Create;
                        ObjectToJSON(TBaseDBDataSet(SubDataSet[i]),aNewObj,ADateAsString,mode);
                        aArray.Add(aNewObj);
                        TBaseDBDataSet(SubDataSet[i]).Next;
                      end;
                    AJSON.Add(TBaseDBDataSet(SubDataSet[i]).TableName,aArray);
                  end;
              end;
            if not WasOpen then
              TBaseDBDataSet(SubDataSet[i]).Close;
          end;
    end
  else if mode = emExtJS then
    begin
      FieldsToJSON(AObject.DataSet.Fields, AJSON, ADateAsString);
      if not Supports(AObject.DataSet,IBaseManageDB) then exit;
      with AObject.DataSet as IBaseSubDataSets do
        for i := 0 to GetCount-1 do
          begin
            WasOpen := TBaseDBDataSet(SubDataSet[i]).Active;
            if WasOpen
            and (TBaseDBDataSet(SubDataSet[i]).TableName<>'TASKSNAPSHOTS')
            and (TBaseDBDataSet(SubDataSet[i]).TableName<>'TASKWORKFLOW')
            and (not ((AObject.TableName='TASKS') and (TBaseDBDataSet(SubDataSet[i]).TableName='HISTORY')))
            then
              begin
                TBaseDBDataSet(SubDataSet[i]).Open;
                TBaseDBDataSet(SubDataSet[i]).First;
                aSubObj := TJSONObject.Create;
                aArray := TJSONArray.Create;
                MetadataToJSON(TBaseDBDataSet(SubDataSet[i]).DataSet,aArray);
                aMetadata := TJSONObject.Create();
                aMetadata.Add('fields',aArray);
                aSubObj.Add('Metadata',aMetadata);
                aArray := TJSONArray.Create;
                if not TBaseDBDataSet(SubDataSet[i]).EOF then
                  begin
                    while not TBaseDBDataSet(SubDataSet[i]).EOF do
                      begin
                        aNewObj := TJSONObject.Create;
                        ObjectToJSON(TBaseDBDataSet(SubDataSet[i]),aNewObj,ADateAsString,mode);
                        aArray.Add(aNewObj);
                        TBaseDBDataSet(SubDataSet[i]).Next;
                      end;
                  end;
                aSubObj.Add('Data',aArray);
                AJSON.Add(TBaseDBDataSet(SubDataSet[i]).TableName,aSubObj);
              end;
            if not WasOpen then
              TBaseDBDataSet(SubDataSet[i]).Close;
          end;
    end;
end;

function TBaseDBDataset.ExportToJSON(mode: TJSONExportMode): string;
var
  aObj: TJSONObject;
  aData: TJSONData;
  aSubObj: TJSONObject;
  aArray: TJSONArray;
  aSSubObj: TJSONObject;
  aMetadata: TJSONObject;
  aArray1: TJSONArray;
begin
  aObj := TJSONObject.Create;
  try
    if mode= emExtJS then
      begin
        aSubObj := TJSONObject.Create;
        aSSubObj := TJSONObject.Create;
        aArray := TJSONArray.Create;
        aArray1 := TJSONArray.Create;
        MetadataToJSON(DataSet,aArray);
        aMetadata := TJSONObject.Create();
        aMetadata.Add('fields',aArray);
        aSubObj.Add('Metadata',aMetadata);
        ObjectToJSON(Self,aSSubObj,True,mode);
        aArray1.Add(aSSubObj);
        aSubObj.Add('Data',aArray1);
        aObj.Add(TableName,aSubObj);
      end
    else
      ObjectToJSON(Self,aObj,True,mode);
    Result := aObj.FormatJSON;
  finally
    aObj.Free;
  end;
end;

procedure TBaseDBDataset.ImportFromXML(XML: string;OverrideFields : Boolean = False;ReplaceFieldFunc : TReplaceFieldFunc = nil);
var
  Doc : TXMLDocument;
  Stream : TStringStream;
  RootNode: TDOMNode;
  i: Integer;

  procedure RecourseTables(aNode : TDOMNode;aDataSet : TBaseDBDataset);
  var
    i: Integer;
    a: Integer;
    bNode: TDOMNode;
    cNode: TDOMNode;
    ThisDataSet: TDataSet;
    function ProcessDataSet(ThisDataSet : TBaseDBDataset) : Boolean;
    var
      c,d: Integer;
      b: Integer;
      aNewValue: String;
      aFieldName: DOMString;
      aRec,aNRec : Variant;
    begin
      Result := False;
      with ThisDataSet.DataSet as IBaseManageDB do
        if (TableName = aNode.Attributes.GetNamedItem('NAME').NodeValue) then
          begin
            if Assigned(BaseApplication) then with BaseApplication as IBaseApplication do
              debug('Importing from: '+TableName);
            ThisDataSet.Open;
            ThisDataSet.Tag:=111;
            cNode := aNode.ChildNodes[i];
            for c := 0 to cNode.ChildNodes.Count-1 do
              if copy(cNode.ChildNodes[c].NodeName,0,4) = 'ROW.' then
                begin
                  bNode := cNode.ChildNodes[c];
                  if (ThisDataSet.State = dsEdit) then
                    ThisDataSet.Post;
                  if (ThisDataSet.State <> dsInsert) then
                    ThisDataSet.Append;
                  for d := 0 to bNode.Attributes.Length-1 do
                    begin
                      aFieldName := bNode.Attributes.Item[d].NodeName;
                    if ((ThisDataSet.dataSet.FieldDefs.IndexOf(aFieldName) <> -1) or (aFieldName='SQL_ID')) then
                      if (ThisDataSet.dataSet.FieldByName(aFieldName).IsNull or (OverrideFields) or (aFieldName='SQL_ID'))
                      or (bNode.Attributes.Item[d].NodeName = 'QUANTITY')
                      or (bNode.Attributes.Item[d].NodeName = 'POSNO')
                      or (bNode.Attributes.Item[d].NodeName = 'VAT')
                      or (bNode.Attributes.Item[d].NodeName = 'WARRENTY')
                      or (bNode.Attributes.Item[d].NodeName = 'PARENT')
                      or (bNode.Attributes.Item[d].NodeName = 'TYPE')
                      or (bNode.Attributes.Item[d].NodeName = 'DEPDONE')
                      or (bNode.Attributes.Item[d].NodeName = 'CHECKED')
                      or (bNode.Attributes.Item[d].NodeName = 'HASCHILDS')
                      or (bNode.Attributes.Item[d].NodeName = 'OWNER')
                      or (bNode.Attributes.Item[d].NodeName = 'ACTIVE')
                      then
                        begin
                          aNewValue := bNode.Attributes.Item[d].NodeValue;
                          if Assigned(ReplaceFieldFunc) then
                            ReplaceFieldFunc(ThisDataSet.FieldByName(aFieldName),bNode.Attributes.Item[d].NodeValue,aNewValue);
                          if aFieldName='SQL_ID' then
                            begin
                              if ThisDataSet.dataSet.FieldDefs.IndexOf('OLD_ID')>-1 then
                                ThisDataSet.dataSet.FieldByName('OLD_ID').AsString := aNewValue;
                            end
                          else if ThisDataSet.FieldByName(aFieldName).IsBlob then
                            begin
                              if aNewValue<>'' then
                                ThisDataSet.FieldByName(aFieldName).AsString := DecodeStringBase64(aNewValue)
                            end
                          else if ThisDataSet.dataSet.FieldDefs.IndexOf(aFieldName)>-1 then
                            ThisDataSet.FieldByName(aFieldName).AsString := aNewValue;
                        end;
                    end;
                  ThisDataSet.Post;
                  for b := 0 to bNode.ChildNodes.Count-1 do
                    if bNode.ChildNodes[b].NodeName = 'TABLE' then
                      begin
                        RecourseTables(bNode.ChildNodes[b],ThisDataSet);
                      end;
                  if (ThisDataSet.State = dsEdit) then
                    ThisDataSet.Post;
                end;
            //change Parent
            aNRec := ThisDataSet.GetBookmark;
            ThisDataSet.First;
            while not ThisDataSet.EOF do
              begin
                aRec := ThisDataSet.GetBookmark;
                if ThisDataSet.dataSet.FieldDefs.IndexOf('OLD_ID')>-1 then
                  begin
                    if ThisDataSet.Locate('OLD_ID',ThisDataSet.FieldByName('PARENT').AsVariant,[]) then
                      begin
                        aNewValue:=ThisDataSet.FieldByName('SQL_ID').AsString;
                        if ThisDataSet.GotoBookmark(aRec) then
                          begin
                            ThisDataSet.Edit;
                            ThisDataSet.FieldByName('PARENT').AsString:=aNewValue;
                            ThisDataSet.Post;
                          end;
                      end;
                  end
                else break;
                ThisDataSet.GotoBookmark(aRec);
                ThisDataSet.Next;
              end;
            ThisDataSet.GotoBookmark(aNRec);
            Result := True;
            ThisDataSet.Tag:=0;
          end;
    end;
  begin
    for i := 0 to aNode.ChildNodes.Count-1 do
      begin
        if aNode.ChildNodes[i].NodeName = 'DATA' then
          begin
            if not ProcessDataSet(aDataSet) then
              with aDataSet.DataSet as IBaseSubDataSets do
                begin
                  for a := 0 to GetCount-1 do
                    begin
                      if ProcessDataSet(TBaseDbDataSet(SubDataSet[a])) then break;
                    end;
                end;
          end;
      end;
  end;
begin
  Stream := TStringStream.Create(XML);
  Stream.Position:=0;
  ReadXMLFile(Doc,Stream);
  RootNode := Doc.FindNode('TABLES');
  for i := 0 to RootNode.ChildNodes.Count-1 do
    if RootNode.ChildNodes[i].NodeName = 'TABLE' then
      with DataSet as IBaseManageDB do
        begin
         if RootNode.ChildNodes[i].Attributes.GetNamedItem('NAME') <> nil then
           if RootNode.ChildNodes[i].Attributes.GetNamedItem('NAME').NodeValue <> TableName then
             raise Exception.Create(strPasteToanWrongDataSet)
           else
             RecourseTables(RootNode.ChildNodes[i],Self);
        end;
  Stream.Free;
  Doc.Free;
end;

procedure TBaseDBDataset.ImportFromJSON(JSON: string; OverrideFields: Boolean;
  ReplaceFieldFunc: TReplaceFieldFunc);
var
  aJParser: TJSONParser;
  aData: TJSONData;
  i: Integer;

  procedure RecourseTables(aNode : TJSONData;aDataSet : TBaseDBDataset);
  var
    i: Integer;
    a: Integer;
    bNode: TDOMNode;
    cNode: TDOMNode;
    ThisDataSet: TDataSet;
    aField: TField;
    Found: Boolean;
    begin
      if aNode.JSONType = jtArray then
        begin
          for i := 0 to aNode.Count-1 do
            RecourseTables(aNode.Items[i],aDataSet);
          exit;
        end
      else if aNode.JSONType = jtObject then
        begin
          for i := 0 to aNode.Count-1 do
            begin
              if TJSONObject(aNode).Names[i]='Fields' then
                begin
                  if TJSONObject(TJSONObject(aNode).Items[i]).FindPath('id') <> nil then
                    begin
                      aDataSet.Select(TJSONObject(TJSONObject(aNode).Items[i]).FindPath('id').AsInt64);
                      aDataSet.Open;
                      adataSet.Edit;
                    end
                  else
                    begin
                      aDataSet.Append;
                    end;
                  for a := 0 to TJSONObject(TJSONObject(aNode).Items[i]).Count-1 do
                    begin
                      aField := aDataSet.FieldByName(TJSONObject(TJSONObject(aNode).Items[i]).Names[a]);
                      if Assigned(aField) then
                        begin
                          if TJSONObject(TJSONObject(aNode).Items[i]).Items[a].IsNull then
                            aField.Clear
                          else if aField.DataType = ftDateTime then
                            aField.AsDateTime:=ConvertISODate(TJSONObject(TJSONObject(aNode).Items[i]).Items[a].AsString)
                          else
                            aField.AsString:=TJSONObject(TJSONObject(aNode).Items[i]).Items[a].AsString;
                        end
                      else
                        begin
                          raise Exception.Create('Field "'+TJSONObject(TJSONObject(aNode).Items[i]).Names[a]+'" not found !');
                        end;
                    end;
                  aDataSet.Post;
                end
              else
                begin
                  with aDataSet.DataSet as IBaseSubDataSets do
                    begin
                      Found := False;
                      for a := 0 to (aDataSet.DataSet as IBaseSubDataSets).GetCount-1 do
                        begin
                          if lowercase(TBaseDbDataSet(SubDataSet[a]).TableName) = lowercase(TJSONObject(aNode).Names[i]) then
                            begin
                              RecourseTables(aNode.Items[i],TBaseDbDataSet(SubDataSet[a]));
                              Found := True;
                              break;
                            end;
                        end;
                      if not Found then
                        raise Exception.Create('Table "'+TJSONObject(aNode).Names[i]+'" not found !');
                    end;
                end;
            end;
        end;
    end;
begin
  aJParser := TJSONParser.Create(JSON);
  aData := aJParser.Parse;
  RecourseTables(aData,Self);
  aData.Free;
  aJParser.Free;
end;

procedure TBaseDbList.OpenItem(AccHistory : Boolean = True);
var
  aObj: TObjects;
begin
  if ((Self.Count=0) and (State<>dsInsert)) or (not Assigned(Id)) then exit;
  try
    try
      aObj := TObjects.CreateEx(nil,DataModule);
      with aObj.DataSet as IBaseDbFilter do
        UsePermissions:= False;
      if DataSet.State<>dsInsert then
        begin
        if not TBaseDBModule(DataModule).TableExists(aObj.TableName) then
          begin
            aObj.CreateTable;
            aObj.Free;
            aObj := TObjects.CreateEx(nil,DataModule,nil,DataSet);
          end;
        aObj.SelectByRefId(Id.AsVariant);
        aObj.Open;
        if aObj.Count=0 then
          begin
            aObj.Insert;
            aObj.Text.AsString := Self.Text.AsString;
            aObj.FieldByName('SQL_ID').AsVariant:=Self.Id.AsVariant;
            if Assigned(Self.Matchcode) then
              aObj.Matchcode.AsString := Self.Matchcode.AsString;
            if Assigned(Self.Status) then
              aObj.Status.AsString := Self.Status.AsString;
            aObj.Number.AsVariant:=Self.Number.AsVariant;
            aObj.FieldByName('LINK').AsString:=TBaseDBModule(DataModule).BuildLink(Self.DataSet);
            aObj.FieldByName('ICON').AsInteger:=TBaseDBModule(DataModule).GetLinkIcon(TBaseDBModule(DataModule).BuildLink(Self.DataSet),True);
            aObj.Post;
            Self.GenerateThumbnail;
          end
        else //Modify existing
          begin
            if CanEdit then
              aObj.Edit;
            if aObj.Text.AsString<>Self.Text.AsString then
              begin
                aObj.Edit;
                aObj.Text.AsString := Self.Text.AsString;
                aObj.FieldByName('LINK').AsString:=TBaseDBModule(DataModule).BuildLink(Self.DataSet);
              end;
            if aObj.FieldByName('ICON').AsVariant<>TBaseDBModule(DataModule).GetLinkIcon(TBaseDBModule(DataModule).BuildLink(Self.DataSet),True) then
              begin
                aObj.Edit;
                aObj.FieldByName('ICON').AsInteger:=TBaseDBModule(DataModule).GetLinkIcon(TBaseDBModule(DataModule).BuildLink(Self.DataSet),True);
              end;
            if aObj.Number.AsString<>Self.Number.AsString then
              begin
                aObj.Edit;
                aObj.Number.AsString := Self.Number.AsString;
                aObj.FieldByName('LINK').AsString:=TBaseDBModule(DataModule).BuildLink(Self.DataSet);
              end;
            if Assigned(Self.Status) and (aObj.Status.AsString<>Self.Status.AsString) then
              begin
                aObj.Edit;
                aObj.Status.AsString := Self.Status.AsString;
              end;
            if aObj.CanEdit then
              aObj.Post;
          end;
        end;
    finally
      aObj.Free;
    end;
  except
  end;
end;

procedure TBaseDbList.BuildSearchIndexes;
begin

end;

procedure TBaseDbList.CascadicPost;
begin
  inherited CascadicPost;
  OpenItem(False);//modify object properties
end;

procedure TBaseDbList.GenerateThumbnail;
begin
end;
function TBaseDbList.SelectFromLink(aLink: string): Boolean;
begin
  Result := False;
  Select(0);
  if pos('{',aLink) > 0 then
    aLink := copy(aLink,0,pos('{',aLink)-1)
  else if rpos('(',aLink) > 0 then
    aLink := copy(aLink,0,rpos('(',aLink)-1);
  with DataSet as IBaseManageDB do
    begin
      if copy(aLink,0,pos('@',aLink)-1) = TableName then
        begin
          if IsNumeric(copy(aLink,pos('@',aLink)+1,length(aLink))) then
            begin
              Select(StrToInt64(copy(aLink,pos('@',aLink)+1,length(aLink))));
              result := True;
            end;
        end
      else if copy(aLink,0,pos('.ID@',aLink)-1) = TableName then
        begin
          if IsNumeric(copy(aLink,pos('.ID@',aLink)+4,length(aLink))) then
            begin
              Select(StrToInt64(copy(aLink,pos('.ID@',aLink)+4,length(aLink))));
              Result := True;
            end;
        end;
    end;
end;

function TBaseDbList.SelectFromNumber(aNumber: string): Boolean;
begin
  with BaseApplication as IBaseDBInterface do
    with DataSet as IBaseDBFilter do
      begin
        Filter := TBaseDBModule(DataModule).QuoteField(TableName)+'.'+TBaseDBModule(DataModule).QuoteField(GetNumberFieldName)+'='+TBaseDBModule(DataModule).QuoteValue(aNumber);
      end;
end;

function TBaseDbList.SelectFromTreeEntry(aParent: LargeInt): Boolean;
begin
  with BaseApplication as IBaseDBInterface do
    with DataSet as IBaseDBFilter do
      begin
        Filter := TBaseDBModule(DataModule).QuoteField('TREEENTRY')+'='+TBaseDBModule(DataModule).QuoteValue(IntToStr(aParent));
      end;
end;

function TBaseDbList.ChangeStatus(aNewStatus: string): Boolean;
begin
  if GetStatusFieldName<>'' then
    begin
      Edit;
      Status.AsString:=aNewStatus;
      Post;
    end;
end;

function TBaseDbList.Duplicate: Boolean;
var
  Copied: String;
begin
  Copied := ExportToXML;
  ImportFromXML(Copied,False);
  with DataSet do
    begin
      Edit;
      if Assigned(Number) and (Number <> Id) then
        Number.Clear;
      Post;
    end;
end;

procedure TBaseDBDataset.Assign(Source: TPersistent);
begin
  if Source is Self.ClassType then
    DirectAssign(Source)
  else
    inherited Assign(Source);
end;
procedure TBaseDBDataset.DirectAssign(Source: TPersistent);
var
  i: Integer;
begin
  if (not DataSet.Active) or ((DataSet.RecordCount = 0) and (DataSet.State<>dsInsert)) then exit;
  if not (Source is TBaseDBDataSet) then exit;
  if (not TBaseDBDataSet(Source).DataSet.Active) or (TBaseDBDataSet(Source).DataSet.RecordCount = 0) then exit;
  for i := 0 to TBaseDBDataSet(Source).DataSet.Fields.Count-1 do
    if DataSet.FieldDefs.IndexOf(TBaseDBDataSet(Source).DataSet.Fields[i].FieldName) <> -1 then
      if  (TBaseDBDataSet(Source).DataSet.Fields[i].FieldName <> 'SQL_ID')
      and (TBaseDBDataSet(Source).DataSet.Fields[i].FieldName <> 'AUTO_ID')
      and (TBaseDBDataSet(Source).DataSet.Fields[i].FieldName <> 'TIMESTAMP') then
        DataSet.FieldByName(TBaseDBDataSet(Source).DataSet.Fields[i].FieldName).AsVariant:=TBaseDBDataSet(Source).DataSet.Fields[i].AsVariant;
end;

procedure TBaseHistory.OpenItem(AccHistory: Boolean);
begin
  //Dont add Item
end;

constructor TBaseHistory.CreateEx(aOwner: TComponent; DM: TComponent;
  aConnection: TComponent; aMasterdata: TDataSet);
begin
  inherited CreateEx(aOwner, DM, aConnection, aMasterdata);
  FHChanged := False;
  FShouldChange:=False;
  with BaseApplication as IBaseDbInterface do
    begin
      with DataSet as IBaseManageDB do
        UseIntegrity:=False;
      with DataSet as IBaseDBFilter do
        begin
          Limit := 50;
          SortFields:='DATE';
        end;
    end;
end;
procedure TBaseHistory.SetDisplayLabels(aDataSet: TDataSet);
begin
  inherited SetDisplayLabels(aDataSet);
  SetDisplayLabelName(aDataSet,'CHANGEDBY',strCreatedby);
  SetDisplayLabelName(aDataSet,'OBJECT',strObject);
  SetDisplayLabelName(aDataSet,'REFOBJECT',strRefObject);
end;
procedure TBaseHistory.DefineFields(aDataSet: TDataSet);
begin
  with aDataSet as IBaseManageDB do
    begin
      TableName := 'HISTORY';
      TableCaption:=strHistory;
      if Assigned(ManagedFieldDefs) then
        with ManagedFieldDefs do
          begin
            Add('REF_ID',ftLargeInt,0,true);
            Add('ACTIONICON',ftInteger,0,False);
            Add('SUMMARY',ftString,220,False);
            Add('ACTION',ftMemo,0,True);
            Add('REFERENCE', ftString,150,False);
            Add('LINK',ftString,400,False);
            Add('OBJECT',ftString,200,False);
            Add('REFOBJECT',ftString,200,False);
            Add('COMMISSION',ftString,60,False);
            Add('SOURCE',ftString,60,False);
            Add('READ',ftString,1,False);
            Add('IGNORE',ftString,1,False);//ignore item in reports
            Add('TAGS',ftString,200,False);
            Add('CHANGEDBY',ftString,4,False);
            Add('PARENT',ftLargeInt,0,False);
            Add('PARENTSTR',ftString,30,False);
            Add('ROOT',ftLargeInt,0,False);
            Add('DATE',ftDateTime,0,False);
          end;
      if Assigned(ManagedIndexdefs) then
        with ManagedIndexDefs do
          begin
            Add('REF_ID','REF_ID',[]);
            Add('PARENT','PARENT',[]);
            Add('SUMMARY','SUMMARY',[]);
            Add('REFERENCE','REFERENCE',[]);
            Add('REFOBJECT','REFOBJECT',[]);
            Add('COMMISSION','COMMISSION',[]);
            Add('READ','READ',[]);
            Add('SOURCE','SOURCE',[]);
            Add('TAGS','TAGS',[]);
            Add('CHANGEDBY','CHANGEDBY',[]);
            Add('TIMESTAMPD','TIMESTAMPD',[]);
            Add('DATE','DATE',[]);
          end;
    end;
end;
procedure TBaseHistory.SelectByParent(aParent: Variant);
begin
  with  DataSet as IBaseDBFilter, BaseApplication as IBaseDBInterface, DataSet as IBaseManageDB do
    begin
      if aParent=Null then
        Filter := TBaseDBModule(DataModule).ProcessTerm('('+QuoteField('PARENT')+'='+TBaseDBModule(DataModule).QuoteValue('')+')')
      else
        Filter := '('+QuoteField('PARENT')+'='+QuoteValue(aParent)+')';
    end;
end;

procedure TBaseHistory.SelectByParentStr(aParent: string);
begin
  with  DataSet as IBaseDBFilter, BaseApplication as IBaseDBInterface, DataSet as IBaseManageDB do
    begin
      Filter := '('+QuoteField('PARENTSTR')+'='+QuoteValue(aParent)+')';
    end;
end;

procedure TBaseHistory.SelectByRoot(aParent: Variant);
begin
  with  DataSet as IBaseDBFilter, BaseApplication as IBaseDBInterface, DataSet as IBaseManageDB do
    begin
      if aParent=Null then
        Filter := TBaseDBModule(DataModule).ProcessTerm('('+QuoteField('ROOT')+'='+TBaseDBModule(DataModule).QuoteValue('')+')')
      else
        Filter := '('+QuoteField('ROOT')+'='+QuoteValue(aParent)+')';
    end;
end;
procedure TBaseHistory.Change;
begin
  inherited;
  if FShouldChange then
    begin
      FHChanged := True;
      if Owner is TBaseDBDataSet then TBaseDBDataSet(Owner).Change;
      FShouldCHange := False;
    end;
end;

function TBaseHistory.AddItem(aObject: TDataSet; aAction: string;
  aLink: string; aReference: string; aRefObject: TDataSet; aIcon: Integer;
  aComission: string; CheckDouble: Boolean; DoPost: Boolean; DoChange: Boolean) : Boolean;
begin
  if Assigned(aRefObject) then
    Result := AddItemSR(aObject,aAction,aLink,aReference,TBaseDBModule(DataModule).BuildLink(aRefObject),aIcon,aComission,CheckDouble,DoPost,DoChange)
  else
    Result := AddItemSR(aObject,aAction,aLink,aReference,'',aIcon,aComission,CheckDouble,DoPost,DoChange);
end;

function TBaseHistory.AddItemSR(aObject: TDataSet; aAction: string;
  aLink: string; aReference: string; aRefObject: string; aIcon: Integer;
  aComission: string; CheckDouble: Boolean; DoPost: Boolean; DoChange: Boolean
  ): Boolean;
begin
  with BaseApplication as IBaseDbInterface do
    if Assigned(aObject) then
      Result := AddItemPlain(TBaseDBModule(DataModule).BuildLink(aObject),aAction,aLink,aReference,aRefObject,aIcon,aComission,CheckDouble,DoPost,DoChange);
end;

function TBaseHistory.AddItemPlain(aObject: string; aAction: string;
  aLink: string; aReference: string; aRefObject: string; aIcon: Integer;
  aComission: string; CheckDouble: Boolean; DoPost: Boolean; DoChange: Boolean
  ): Boolean;
var
  tmp: String;
begin
  Result := False;
  if DataSet.Tag=111 then exit; //Copy XML
  if not DataSet.Active then
    Open;
  Last;
  tmp := FieldByName('ACTION').AsString;
  with BaseApplication as IBaseDbInterface do
    begin
      if  (FieldByName('ACTIONICON').AsInteger = aIcon)
      and (aIcon<>ACICON_USEREDITED)
      and (FieldByName('LINK').AsString = aLink)
      and (trunc(FieldByName('TIMESTAMPD').AsDatetime) = trunc(Now()))
      and (FieldByName('CHANGEDBY').AsString = TBaseDBModule(DataModule).Users.Idcode.AsString)
      and ((FieldByName('REFERENCE').AsString = aReference) or (FieldByName('REFERENCE').AsString=TBaseDBModule(DataModule).Users.IDCode.Asstring))
      and ((aIcon=11{Termin}))
      and (CheckDouble)
      then
        Delete;
    end;
  Append;
  if aLink <> '' then
    FieldByName('LINK').AsString      := aLink;
  if aObject<>'' then
    FieldByName('OBJECT').AsString := aObject;
  FieldByName('ACTIONICON').AsInteger := aIcon;
  FieldByName('ACTION').AsString    := aAction;
  FieldByName('REFERENCE').AsString := aReference;
  if aRefObject<>'' then
    FieldByName('REFOBJECT').AsString := aRefObject;
  with BaseApplication as IBaseDbInterface do
    FieldByName('CHANGEDBY').AsString := TBaseDBModule(DataModule).Users.IDCode.AsString;
  DataSet.FieldByName('COMMISSION').AsString := aComission;
  DataSet.FieldByName('DATE').AsDateTime:=Now();
  if DoPost then
    Post;
  result := True;
  if DoChange or (not DoPost) then
    begin
      FShouldChange := True;
      Change;
    end;
end;

procedure TBaseHistory.AddParentedItem(aObject: TDataSet; aAction: string;
  aParent: Variant; aLink: string; aReference: string; aRefObject: TDataSet;
  aIcon: Integer; aComission: string; CheckDouble: Boolean; DoPost: Boolean;
  DoChange: Boolean);
  function GetRoot(bParent : Variant) : Variant;
  var
    aFind: TBaseHistory;
  begin
    Result := Null;
    aFind := TBaseHistory.CreateEx(nil,DataModule);
    aFind.Select(bParent);
    aFind.Open;
    if aFind.Count>0 then
      begin
        if (not aFind.FieldByName('PARENT').IsNull) and (aFind.FieldByName('PARENT').AsVariant<>bParent) then
          Result := GetRoot(aFind.FieldByName('PARENT').AsVariant)
        else
          Result := aFind.Id.AsVariant;
      end;
    aFind.Free;
  end;

begin
  if AddItem(aObject,aAction,aLink,aReference,aRefObject,aIcon,aComission,CheckDouble,False,DoChange) then
    begin
      DataSet.FieldByName('PARENT').AsVariant := aParent;
      DataSet.FieldByName('ROOT').AsVariant := GetRoot(aParent);
      if DoPost then
        Post;
    end;
end;

procedure TBaseHistory.AddParentedItemPlain(aObject: string; aAction: string;
  aParent: Variant; aLink: string; aReference: string; aRefObject: string;
  aIcon: Integer; aComission: string; CheckDouble: Boolean; DoPost: Boolean;
  DoChange: Boolean);
function GetRoot(bParent : Variant) : Variant;
var
  aFind: TBaseHistory;
begin
  Result := Null;
  aFind := TBaseHistory.CreateEx(nil,DataModule);
  aFind.Select(bParent);
  aFind.Open;
  if aFind.Count>0 then
    begin
      if (not aFind.FieldByName('PARENT').IsNull) and (aFind.FieldByName('PARENT').AsVariant<>bParent) then
        Result := GetRoot(aFind.FieldByName('PARENT').AsVariant)
      else
        Result := aFind.Id.AsVariant;
    end;
  aFind.Free;
end;

begin
  if AddItemPlain(aObject,aAction,aLink,aReference,aRefObject,aIcon,aComission,CheckDouble,False,DoChange) then
    begin
      DataSet.FieldByName('PARENT').AsVariant := aParent;
      DataSet.FieldByName('ROOT').AsVariant := GetRoot(aParent);
      if DoPost then
        Post;
    end;
end;

procedure TBaseHistory.AddItemWithoutUser(aObject: TDataSet; aAction: string;
  aLink: string; aReference: string; aRefObject: TDataSet; aIcon: Integer;
  aComission: string; CheckDouble: Boolean; DoPost: Boolean; DoChange: Boolean);
var
  tmp: String;
begin
  if not DataSet.Active then
    Open;
  with DataSet do
    begin
      Last;
      tmp := FieldByName('ACTION').AsString;
      with BaseApplication as IBaseDbInterface do
        begin
          if (tmp = aAction)
          and (FieldByName('ACTIONICON').AsInteger = aIcon)
          and (FieldByName('LINK').AsString = aLink)
          and (trunc(FieldByName('TIMESTAMPD').AsDatetime) = trunc(Now()))
          and (FieldByName('REFERENCE').AsString = aReference)
          and (CheckDouble)
          then
            exit; //Ignore Add when Action is equal
        end;
      Append;
      if aLink <> '' then
        FieldByName('LINK').AsString      := aLink;
      with BaseApplication as IBaseDbInterface do
        FieldByName('OBJECT').AsString := TBaseDBModule(DataModule).BuildLink(aObject);
      FieldByName('ACTIONICON').AsInteger := aIcon;
      FieldByName('ACTION').AsString    := aAction;
      FieldByName('REFERENCE').AsString := aReference;
      if Assigned(aRefObject) then
        begin
          with BaseApplication as IBaseDbInterface do
            FieldByName('REFOBJECT').AsString := TBaseDBModule(DataModule).BuildLink(aRefObject);
        end;
      DataSet.FieldByName('COMMISSION').AsString := aComission;
      if DoPost then
        Post;
      if DoChange or (not DoPost) then
        begin
          FShouldChange := True;
          Change;
        end;
    end;
end;

procedure TBaseHistory.AddMessageItem(aObject: TDataSet; aMessage, aSubject,
  aSource, aLink: string; aParent: LargeInt);
begin
  if copy(aMessage,0,length(aSubject)) = aSubject then
    aMessage:=copy(aMessage,length(aSubject),length(aMessage));
  if aParent=0 then
    AddItem(aObject,aSubject+LineEnding+aMessage,aLink,'',nil,ACICON_MAILNEW,'',True,False)
  else
    AddParentedItem(aObject,aMessage,aParent,aLink,'',nil,ACICON_MAILNEW,'',True,False);
  FieldByName('CHANGEDBY').Clear;
  Post;
end;

procedure TBaseHistory.AddAnsweredMessageItem(aObject: TDataSet; aMessage,
  aSubject, aSource, aLink: string; aParent: LargeInt);
begin
  if copy(aMessage,0,length(aSubject)) = aSubject then
    aMessage:=copy(aMessage,length(aSubject),length(aMessage));
  if aParent=0 then
    AddItem(aObject,aSubject+LineEnding+aMessage,aLink,'',nil,ACICON_MAILANSWERED,'',True,False)
  else
    AddParentedItem(aObject,aMessage,aParent,aLink,'',nil,ACICON_MAILANSWERED,'',True,False);
  FieldByName('CHANGEDBY').Clear;
  Post;
end;

function TBaseHistory.GetTextFieldName: string;
begin
  Result := 'ACTION';
end;

function TBaseHistory.GetNumberFieldName: string;
begin
  Result := 'REFERENCE';
end;

procedure TActiveUsers.DefineFields(aDataSet: TDataSet);
begin
  with aDataSet as IBaseManageDB do
    begin
      TableName := 'ACTIVEUSERS';
      if Assigned(ManagedFieldDefs) then
        with ManagedFieldDefs do
          begin
            Add('ACCOUNTNO',ftString,20,False);
            Add('NAME',ftString,30,True);
            Add('CLIENT',ftString,50,True);
            Add('HOST',ftString,50,False);
            Add('VERSION',ftString,25,False);
            Add('COMMAND',ftMemo,0,False);
            Add('EXPIRES',ftDateTime,0,False);
          end;
    end;
end;

function TActiveUsers.Delete: Boolean;
begin
  with DataSet as IBaseManageDB do
    UpdateStdFields := False;
  Result := True;
  DataSet.Delete;
  with DataSet as IBaseManageDB do
    UpdateStdFields := True;
end;

constructor TPermissions.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  IsReadOnly:=True;
end;

procedure TPermissions.DefineFields(aDataSet: TDataSet);
begin
  with aDataSet as IBaseManageDB do
    begin
      TableName := 'PERMISSIONS';
      TableCaption:=strPermissions;
      if Assigned(ManagedFieldDefs) then
        with ManagedFieldDefs do
          begin
            Add('REF_ID_ID',ftLargeInt,0,True);
            Add('USER',ftLargeInt,0,True);
            Add('RIGHT',ftSmallInt,0,True);
          end;
      if Assigned(ManagedIndexdefs) then
        with ManagedIndexDefs do
          Add('REF_ID_ID','REF_ID_ID',[]);
    end;
end;
procedure TReports.DefineFields(aDataSet: TDataSet);
begin
  with aDataSet as IBaseManageDB do
    begin
      TableName := 'REPORTS';
      TableCaption:=strReports;
      if Assigned(ManagedFieldDefs) then
        with ManagedFieldDefs do
          begin
            Add('TYPE',ftString,6,True);
            Add('NAME',ftString,60,True);
            Add('STANDARD',ftString,1,False);
            Add('CHANGEDBY',ftString,4,False);
            Add('LANGUAGE',ftString,3,True);
            Add('EMAIL',ftString,200,False);
            Add('EMAILCC',ftString,200,False);
            Add('EMAILBCC',ftString,200,False);
            Add('REPORT',ftBlob,0,False);
            Add('TEXT',ftMemo,0,False);
          end;
    end;
end;

procedure TReports.Open;
begin
  inherited Open;
end;

constructor TFilters.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  IsReadOnly:=True;
end;

procedure TFilters.DefineFields(aDataSet: TDataSet);
begin
  with aDataSet as IBaseManageDB do
    begin
      TableName := 'FILTERS';
      if Assigned(ManagedFieldDefs) then
        with ManagedFieldDefs do
          begin
            Add('TYPE',ftString,1,True);
            Add('NAME',ftString,60,True);
            Add('FILTER',ftMemo,0,True);
            Add('FILTERIN',ftString,100,False);
            Add('STANDART',ftString,1,True);
            Add('SORTDIR',ftString,4,False);
            Add('SORTFIELD',ftString,20,False);
            Add('USER',ftString,20,False);
          end;
    end;
end;
procedure TFilters.FillDefaults(aDataSet: TDataSet);
begin
  aDataSet.FieldByName('STANDART').AsString := 'N';
end;
procedure TForms.DefineFields(aDataSet : TDataSet);
begin
  with aDataSet as IBaseManageDB do
    begin
      TableName := 'FORMS';
      if Assigned(ManagedFieldDefs) then
        with ManagedFieldDefs do
          begin
            Add('TYPE',ftString,3,True);
            Add('NAME',ftString,60,True);
            Add('FORM',ftBlob,0,False);
          end;
    end;
end;

function TTree.GetText: TField;
begin
  Result := FieldByName('NAME');
end;

constructor TTree.CreateEx(aOwner: TComponent; DM: TComponent;
  aConnection: TComponent; aMasterdata: TDataSet);
begin
  inherited CreateEx(aOwner, DM, aConnection, aMasterdata);
  with BaseApplication as IBaseDbInterface do
    begin
      with DataSet as IBaseDBFilter do
        begin
          UsePermissions:=True;
          Distinct:=True;
          BaseSortFields := 'SQL_ID';
          SortFields := 'SQL_ID';
          SortDirection := sdAscending;
          Limit:=0;
        end;
    end;
end;
procedure TTree.Open;
begin
  inherited Open;
end;
procedure TTree.ImportStandartEntrys;
begin
  Open;
  if not DataSet.Locate('SQL_ID',TREE_ID_CUSTOMER_UNSORTED,[]) then
    begin
      DataSet.Append;
      DataSet.FieldByName('PARENT').AsString := '0';
      DataSet.FieldByName('TYPE').AsString := 'C';
      DataSet.FieldByName('NAME').AsString := strUnsorted;
      Id.AsVariant:=TREE_ID_CUSTOMER_UNSORTED;
      Post;
    end;
  if not DataSet.Locate('SQL_ID',TREE_ID_MASTERDATA_UNSORTED,[]) then
    begin
      DataSet.Append;
      DataSet.FieldByName('PARENT').AsString := '0';
      DataSet.FieldByName('TYPE').AsString := 'M';
      DataSet.FieldByName('NAME').AsString := strUnsorted;
      id.AsVariant:=TREE_ID_MASTERDATA_UNSORTED;
      Post;
    end;
  if not DataSet.Locate('SQL_ID',TREE_ID_PROJECT_UNSORTED,[]) then
    begin
      DataSet.Append;
      DataSet.FieldByName('PARENT').AsString := '0';
      DataSet.FieldByName('TYPE').AsString := 'P';
      DataSet.FieldByName('NAME').AsString := strUnsorted;
      DataSet.Post;
      DataSet.Edit;
      id.AsVariant:=TREE_ID_PROJECT_UNSORTED;
      Post;
    end;
  if not DataSet.Locate('SQL_ID',TREE_ID_WIKI_UNSORTED,[]) then
    begin
      DataSet.Append;
      DataSet.FieldByName('PARENT').AsString := '0';
      DataSet.FieldByName('TYPE').AsString := 'W';
      DataSet.FieldByName('NAME').AsString := strUnsorted;
      id.AsVariant:=TREE_ID_WIKI_UNSORTED;
      Post;
    end;
  if not DataSet.Locate('SQL_ID',TREE_ID_MESSAGES,[]) then
    begin
      DataSet.Append;
      DataSet.FieldByName('PARENT').AsString := '0';
      DataSet.FieldByName('TYPE').AsString := 'N';
      DataSet.FieldByName('NAME').AsString := strMessages;
      id.AsVariant:=TREE_ID_MESSAGES;
      Post;
    end;
  if not DataSet.Locate('SQL_ID',TREE_ID_UNKNOWN_MESSAGES,[]) then
    begin
      DataSet.Append;
      DataSet.FieldByName('PARENT').AsString := '0';
      DataSet.FieldByName('TYPE').AsString := 'N';
      DataSet.FieldByName('NAME').AsString := strUnknownMessages;
      id.AsVariant:=TREE_ID_UNKNOWN_MESSAGES;
      DataSet.Post;
    end;
  if not DataSet.Locate('SQL_ID',IntToStr(TREE_ID_SEND_MESSAGES),[]) then
    begin
      DataSet.Append;
      DataSet.FieldByName('PARENT').AsString := '0';
      DataSet.FieldByName('TYPE').AsString := 'N';
      DataSet.FieldByName('NAME').AsString := strSendMessages;
      id.AsVariant:=TREE_ID_SEND_MESSAGES;
      Post;
    end;
  if not DataSet.Locate('SQL_ID',TREE_ID_SPAM_MESSAGES,[]) then
    begin
      DataSet.Append;
      DataSet.FieldByName('PARENT').AsString := '0';
      DataSet.FieldByName('TYPE').AsString := 'N';
      DataSet.FieldByName('NAME').AsString := strSpam;
      id.AsVariant:=TREE_ID_SPAM_MESSAGES;
      Post;
    end;
  if not DataSet.Locate('SQL_ID',TREE_ID_ARCHIVE_MESSAGES,[]) then
    begin
      DataSet.Append;
      DataSet.FieldByName('PARENT').AsString := '0';
      DataSet.FieldByName('TYPE').AsString := 'N';
      DataSet.FieldByName('NAME').AsString := strArchive;
      id.AsVariant:=TREE_ID_ARCHIVE_MESSAGES;
      Post;
    end;
  if not DataSet.Locate('SQL_ID',TREE_ID_DELETED_MESSAGES,[]) then
    begin
      DataSet.Append;
      DataSet.FieldByName('PARENT').AsString := '0';
      DataSet.FieldByName('TYPE').AsString := 'N';
      DataSet.FieldByName('NAME').AsString := strDeletedMessages;
      id.AsVariant:=TREE_ID_DELETED_MESSAGES;
      Post;
    end;
  if not DataSet.Locate('SQL_ID',TREE_ID_LOG_MESSAGES,[]) then
    begin
      DataSet.Append;
      DataSet.FieldByName('PARENT').AsString := '0';
      DataSet.FieldByName('TYPE').AsString := 'N';
      DataSet.FieldByName('NAME').AsString := strLogMessages;
      id.AsVariant:=TREE_ID_LOG_MESSAGES;
      Post;
    end;
end;
procedure TTree.DefineFields(aDataSet : TDataSet);
begin
  with aDataSet as IBaseManageDB do
    begin
      TableName := 'TREE';
      TableCaption:=strTree;
      if Assigned(ManagedFieldDefs) then
        with ManagedFieldDefs do
          begin
            Add('PARENT',ftLargeint,0,False);
            Add('TYPE',ftString,1,True);
            Add('NAME',ftString,60,True);
            Add('LINK',ftString,400,False);
            Add('ICON',ftInteger,0,False);
            Add('DESC',ftString,200,False);
          end;
      if Assigned(ManagedIndexdefs) then
        with ManagedIndexDefs do
          Add('NAME','TYPE;NAME;PARENT',[ixUnique]);
    end;
end;

constructor TOptions.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  IsReadOnly:=True;
end;

procedure TOptions.DefineFields(aDataSet : TDataSet);
begin
  with aDataSet as IBaseManageDB do
    begin
      TableName := 'OPTIONS';
      if Assigned(ManagedFieldDefs) then
        with ManagedFieldDefs do
          begin
            Add('OPTION',ftString,60,True);
            Add('VALUE',ftMemo,0,True);
          end;
    end;
end;

procedure TOptions.Open;
begin
  with BaseApplication as IBaseDbInterface do
    begin
      with DataSet as IBaseDBFilter do
        begin
          Limit := 0;
        end;
    end;
  inherited Open;
end;

function TOptions.GetOption(aSection, aIdent, DefaultValue: string): string;
begin
  if not DataSet.Active then Open;
  if not DataSet.Locate('OPTION',aIdent,[]) then
    begin
      with DataSet as IBaseDBFilter do
        begin
          SetFilter('');
        end;
    end;
  if Locate('OPTION',aIdent,[]) then
    Result := FieldByName('VALUE').AsString;
end;

procedure TOptions.SetOption(aSection, aIdent, Value: string);
begin
  if IsReadOnly then
    begin
      Close;
      IsReadOnly:=False;
    end;
  with BaseApplication as IBaseDBInterface do
    begin
      if not Active then Open;
      if not Locate('OPTION',aIdent,[]) then
        TBaseDBModule(DataModule).SetFilter(Self,'',0);
      if not Locate('OPTION',aIdent,[]) then
        begin
          if Value <> '' then
            begin
              Insert;
              FieldByName('OPTION').AsString:=aIdent;
            end;
        end
      else if Value <> '' then
        Edit
      else if Value = '' then
        Delete;
      if Value <> '' then
        begin
          FieldByName('VALUE').AsString := Value;
          Post;
        end;
    end;
end;

constructor TRights.CreateEx(aOwner: TComponent; DM: TComponent;
  aConnection: TComponent; aMasterdata: TDataSet);
begin
  inherited CreateEx(aOwner,DM, aConnection, aMasterdata);
  FCachedRights := TStringList.Create;
end;
destructor TRights.Destroy;
begin
  FCachedRights.Free;
  inherited Destroy;
end;

procedure TRights.Open;
begin
  ResetCache;
  with BaseApplication as IBaseDbInterface do
    begin
      with DataSet as IBaseDBFilter do
        begin
          Limit := 0;
          FetchRows:=0;
        end;
    end;
  inherited Open;
end;

procedure TRights.DefineFields(aDataSet : TDataSet);
begin
  with aDataSet as IBaseManageDB do
    begin
      TableName := 'RIGHTS';
      TableCaption:=strUserRights;
      if Assigned(ManagedFieldDefs) then
        with ManagedFieldDefs do
          begin
            Add('RIGHTNAME',ftString,20,true);
            Add('RIGHTS',ftSmallInt,0,true);
          end;
    end;
end;

procedure TRights.ResetCache;
begin
  FCachedRights.Clear;
end;

function TRights.Right(Element: string; Recursive: Boolean; UseCache: Boolean): Integer;
var
  aUser : Variant;

  procedure RecursiveGetRight(aRec : Integer = 0);
  begin
    if aRec>20 then
      begin
        Result := -2;
        exit;
      end;
    if DataSet.Locate('RIGHTNAME',VarArrayOf([Element]),[loCaseInsensitive]) then
      Result := DataSet.FieldByName('RIGHTS').AsInteger
    else
      begin
        with BaseApplication as IBaseDbInterface do
          begin
            if not UserTable.FieldByName('PARENT').IsNull then
              begin
                if (Recursive) and UserTable.GotoBookmark(UserTable.FieldByName('PARENT').AsInteger) then
                  RecursiveGetRight(aRec+1)
                else
                  Result := -2;
              end
            else
              Result := -2;
          end;
      end;
  end;
begin
  try
    Result := -1;
    if not Assigned(FCachedRights) then exit;
    if (FCachedRights.Values[UpperCase(Element)] <> '') and UseCache then
      Result := StrToInt(FCachedRights.Values[UpperCase(Element)])
    else
      begin
        with BaseApplication as IBaseDBInterface do
          begin
            if not UserTable.Active then UserTable.Open;
            aUser := UserTable.GetBookmark;
            with Self.DataSet as IBaseDBFilter do
              Filter := TBaseDBModule(DataModule).QuoteField('RIGHTNAME')+'='+TBaseDBModule(DataModule).QuoteValue(UpperCase(Element));
            Open;
            RecursiveGetRight;
            UserTable.GotoBookmark(aUser);
          end;
        FCachedRights.Values[UpperCase(Element)] := IntToStr(Result);
      end;
  except
    Result := -1;
  end;
end;

constructor TMandantDetails.CreateEx(aOwner: TComponent; DM: TComponent;
  aConnection: TComponent; aMasterdata: TDataSet);
begin
  inherited CreateEx(aOwner, DM, aConnection, aMasterdata);
  FAuthSources := TAuthSources.CreateEx(nil,DataModule,aConnection,DataSet);
end;

function TMandantDetails.CreateTable: Boolean;
begin
  Result:=inherited CreateTable;
  FAuthSources.CreateTable;
end;

destructor TMandantDetails.Destroy;
begin
  FAuthSources.Free;
  inherited Destroy;
end;

procedure TMandantDetails.DefineFields(aDataSet : TDataSet);
begin
  with aDataSet as IBaseManageDB do
    begin
      TableName := 'MANDANTDETAILS';
      TableCaption:=strMandantDetails;
      if Assigned(ManagedFieldDefs) then
        with ManagedFieldDefs do
          begin
            Add('NAME',ftString,160,False);
            Add('ADRESS',ftMemo,0,False);
            Add('SORTCODE',ftString,20,False);
            Add('ACCOUNT',ftString,200,False);
            Add('INSTITUTE',ftString,60,false);
            Add('TEL1',ftString,30,false);
            Add('TEL2',ftString,30,false);
            Add('TEL3',ftString,30,false);
            Add('TEL4',ftString,30,false);
            Add('FAX',ftString,30,false);
            Add('MAIL',ftString,50,false);
            Add('INTERNET',ftString,50,false);
            Add('ADDITION1',ftString,200,False);
            Add('ADDITION2',ftString,200,False);
            Add('ADDITION3',ftString,200,False);
            Add('ADDITION4',ftString,200,False);
            Add('ADDITION5',ftString,200,False);
            Add('ADDITION6',ftString,200,False);
            Add('ADDITION7',ftString,200,False);
            Add('ADDITION8',ftString,200,False);
            Add('DBVERSION',ftInteger,0,False);
            Add('STAMP',ftLargeInt,0,False);
            Add('IMAGE',ftBlob,0,False);
            Add('DBSTATEMENTS',ftMemo,0,False);
            Add('DBVER',ftInteger,0,False);
          end;
    end;
end;

function TAuthSources.Authenticate(aUser, aPassword: string): Boolean;
var
  ldap: TLDAPSend;
  l: TStringList;
  tmp: String;
begin
  Result := False;
  if FieldByName('TYPE').AsString='LDAP' then
    begin
      ldap := TLDAPSend.Create;
      try
        if pos(':',FieldByName('SERVER').AsString)>0 then
          begin
            ldap.TargetHost:=copy(FieldByName('SERVER').AsString,0,pos(':',FieldByName('SERVER').AsString)-1);
            ldap.TargetPort:=copy(FieldByName('SERVER').AsString,pos(':',FieldByName('SERVER').AsString)+1,length(FieldByName('SERVER').AsString));
          end
        else
          ldap.TargetHost:=FieldByName('SERVER').AsString;
        ldap.UserName:=Stringreplace(FieldByName('USER').AsString,'%s',aUser,[rfReplaceAll]); //cn=admin,dc=tcsapps,dc=de
        if FieldByName('PASSWORD').AsString <> '' then
          ldap.Password:=Stringreplace(FieldByName('PASSWORD').AsString,'%s',aPassword,[rfReplaceAll])
        else
          ldap.Password:=aPassword;
        if ldap.Login then
          begin
            if ldap.BindSasl or ldap.Bind then
              begin
                l := TStringList.Create;
                l.Add('displayname');
                l.Add('description');
                l.Add('givenName');
                l.Add('*');
                tmp := '(|(uid=%s))';
                if FieldByName('FILTER').AsString<>'' then
                  tmp := FieldByName('FILTER').AsString;
                tmp := StringReplace(tmp,'%s',aUser,[rfReplaceAll]);
                if ldap.Search(FieldByName('BASE').AsString, False,tmp , l) then
                  if ldap.SearchResult.Count=1 then
                    begin
                      if (TBaseDBModule(DataModule).Users.Locate('NAME',ldap.SearchResult.Items[0].Attributes.Get('cn'),[]))
                      or ((ldap.SearchResult.Items[0].Attributes.Get('uid')<>'') and (TBaseDBModule(DataModule).Users.Locate('LOGINNAME',ldap.SearchResult.Items[0].Attributes.Get('uid'),[])))
                      or ((ldap.SearchResult.Items[0].Attributes.Get('sAMAccountName')<>'') and (TBaseDBModule(DataModule).Users.Locate('LOGINNAME',ldap.SearchResult.Items[0].Attributes.Get('sAMAccountName'),[])))
                      or ((ldap.SearchResult.Items[0].Attributes.Get('mail')<>'') and (TBaseDBModule(DataModule).Users.Locate('EMAIL',ldap.SearchResult.Items[0].Attributes.Get('mail'),[])))
                      then
                        TBaseDBModule(DataModule).Users.Edit
                      else
                        begin
                          TBaseDBModule(DataModule).Users.Append;
                          TBaseDBModule(DataModule).Users.FieldByName('NAME').AsString:=ldap.SearchResult.Items[0].Attributes.Get('cn');
                          if ldap.SearchResult.Items[0].Attributes.Get('uid')<>'' then
                            TBaseDBModule(DataModule).Users.FieldByName('LOGINNAME').AsString:=ldap.SearchResult.Items[0].Attributes.Get('uid')
                          else
                            TBaseDBModule(DataModule).Users.FieldByName('LOGINNAME').AsString:=ldap.SearchResult.Items[0].Attributes.Get('sAMAccountName');
                          TBaseDBModule(DataModule).Users.FieldByName('EMAIL').AsString := ldap.SearchResult.Items[0].Attributes.Get('mail');
                        end;
                      TBaseDBModule(DataModule).Users.FieldByName('AUTHSOURCE').AsString:=FieldByName('NAME').AsString;
                      TBaseDBModule(DataModule).Users.Post;
                      with BaseApplication as IBaseApplication do
                        Debug(LDAPResultDump(ldap.SearchResult));
                      Ldap.Logout;
                      ldap.UserName := ldap.SearchResult.Items[0].ObjectName;
                      ldap.Password:=aPassword;
                      Result := ldap.Login and (ldap.BindSasl or ldap.Bind);
                    end;
                if not Result then
                  begin
                    with BaseApplication as IBaseApplication do
                      Warning('LDAP User not found or Login failed with Filter "'+tmp+'"');
                  end;
                l.Free;
              end
            else with BaseApplication as IBaseApplication do
             Warning('LDAP Bind failed to Server "'+ldap.TargetHost+'"');
          end
        else with BaseApplication as IBaseApplication do
          Warning('LDAP Connection failed to Server "'+ldap.TargetHost+'"');
      finally
        ldap.Free;
      end;
    end;
end;

procedure TNumbersets.DefineFields(aDataSet : TDataSet);
begin
  with aDataSet as IBaseManageDB do
    begin
      TableName := 'NUMBERS';
      TableCaption:=strNumbersets;
      if Assigned(ManagedFieldDefs) then
        with ManagedFieldDefs do
          begin
            Add('TABLENAME',ftString,25,True);
            Add('TYPE',ftString,1,True);
            Add('INCR',ftInteger,0,False);
            Add('ACTUAL',ftInteger,0,False);
            Add('STOP',ftInteger,0,False);
            Add('POOL',ftString,25,True);//NumberPool
          end;
    end;
end;
function TNumbersets.GetNewNumber(Numberset: string): string;
var
  i: Integer;
begin
  Result := '';
  with BaseApplication as IBaseDbInterface do
    begin
      TBaseDBModule(DataModule).SetFilter(Self, TBaseDBModule(DataModule).QuoteField('TABLENAME')+'='+TBaseDBModule(DataModule).QuoteValue(numberset));
      DataSet.Refresh;//get sure data is actual
      if DataSet.Recordcount > 0 then
        begin
          if DataSet.FieldByName('TYPE').AsString = 'N' then
            begin
              if DataSet.FieldByName('ACTUAL').AsInteger + DataSet.FieldByName('INCR').AsInteger < DataSet.FieldByName('STOP').AsInteger then
                begin
                  DataSet.Edit;
                  DataSet.FieldByName('ACTUAL').AsInteger :=
                    DataSet.FieldByName('ACTUAL').AsInteger + DataSet.FieldByName('INCR').AsInteger;
                  Result := IntToStr(DataSet.FieldByName('ACTUAL').AsInteger);
                  DataSet.Post;
                end
              else raise Exception.Create(Format(strNumbersetEmpty,[Numberset]));
            end
          else if DataSet.FieldByName('TYPE').AsString = 'A' then
            begin
              Randomize;
              for i := 0 to 14 do
                Result := Result + chr($30 + random(36));
              StringReplace(Result, '=', '-', [rfreplaceAll]);
              StringReplace(Result, '?', '-', [rfreplaceAll]);
              StringReplace(Result, '@', '-', [rfreplaceAll]);
            end;
        end
      else raise Exception.Create(Format(strNumbersetDontExists,[Numberset]));
    end;
end;
function TNumbersets.HasNumberSet(Numberset: string): Boolean;
begin
  with BaseApplication as IBaseDbInterface do
    TBaseDBModule(DataModule).SetFilter(Self, TBaseDBModule(DataModule).QuoteField('TABLENAME')+'='+TBaseDBModule(DataModule).QuoteValue(numberset));
  Result := Count > 0;
end;

destructor TNumbersets.Destroy;
begin
end;

function TUser.GetLeaved: TField;
begin
  Result := DataSet.FieldByName('LEAVED');
end;

function TUser.GetIDCode: TField;
begin
  Result := FieldByName('IDCODE');
end;

function TUser.GetAcc: TField;
begin
  Result := DataSet.FieldByName('ACCOUNTNO');
end;

function TUser.GetPasswort: TField;
begin
  Result := DataSet.FieldByName('PASSWORD');
end;
function TUser.GetSalt: TField;
begin
  Result := DataSet.FieldByName('SALT');
end;
function TUser.GetUser: TField;
begin
  Result := DataSet.FieldByName('NAME');
end;
function TUser.GetTextFieldName: string;
begin
  Result := 'NAME';
end;
function TUser.GetNumberFieldName: string;
begin
  Result := 'CUSTOMERNO';
end;

function TUser.GetWorktime: Extended;
begin
  Result := FieldByName('WORKTIME').AsFloat;
  if Result=0 then Result := 8;
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
function TUser.GetHistory: TBaseHistory;
begin
  Result := FHistory;
end;
function TUser.GetTyp: string;
begin
  Result := 'U';
end;
constructor TUser.CreateEx(aOwner: TComponent; DM: TComponent;
  aConnection: TComponent; aMasterdata: TDataSet);
begin
  inherited CreateEx(aOwner, DM, aConnection, aMasterdata);
  FOptions := TOptions.CreateEx(Self,DataModule,aConnection,DataSet);
  FRights := TRights.CreateEx(Self,DataModule,aConnection,DataSet);
  FFollows := TFollowers.CreateEx(Self,DataModule,aConnection,DataSet);
  FRights.Users := Self;
  FHistory := TBaseHistory.CreateEx(Self,DataModule,aConnection,DataSet);
  FPayGroups := TPayGroups.CreateEx(Self,DataModule,aConnection);
  with BaseApplication as IBaseDbInterface do
    begin
      with DataSet as IBaseDBFilter do
        begin
          Limit := 0;
        end;
    end;
end;

destructor TUser.Destroy;
begin
  inherited Destroy;
end;

procedure TUser.Open;
begin
  with BaseApplication as IBaseDbInterface do
    begin
      with DataSet as IBaseDBFilter do
        begin
          Limit := 0;
          FetchRows:=0;
        end;
    end;
  inherited Open;
end;

procedure TUser.DefineFields(aDataSet : TDataSet);
begin
  with aDataSet as IBaseManageDB do
    begin
      TableName := 'USERS';
      TableCaption := strUsers;
      if Assigned(ManagedFieldDefs) then
        with ManagedFieldDefs do
          begin
            Add('TYPE',ftString,1,False);
            Add('PARENT',ftLargeint,0,False);
            Add('ACCOUNTNO',ftString,20,True);
            Add('NAME',ftString,30,True);
            Add('PASSWORD',ftString,45,False);
            Add('SALT',ftString,105,False);
            Add('IDCODE',ftString,4,False);
            Add('EMPLOYMENT',ftDate,0,False);
            Add('LEAVED',ftDate,0,false);
            Add('CUSTOMERNO',ftString,20,false);
            Add('PERSONNELNO',ftString,20,false);
            Add('DEPARTMENT',ftString,30,false);
            Add('POSITION',ftString,30,false);
            Add('LOGINNAME',ftString,30,false);
            Add('EMAIL',ftString,100,false);
            Add('PAYGROUP',ftLargeint,0,false);
            Add('WORKTIME',ftInteger,0,false); //8 wenn NULL
            Add('WEEKWORKTIME',ftInteger,0,false);//40 wenn NULL
            Add('USEWORKTIME',ftInteger,0,false);
            Add('LOGINACTIVE',ftString,1,false);
            Add('REMOTEACCESS',ftString,1,false);
            Add('LASTLOGIN',ftDateTime,0,false);
            Add('AUTHSOURCE',ftString,10,false);
          end;
      if Assigned(ManagedIndexdefs) then
        with ManagedIndexDefs do
          Add('ACCOUNTNO','ACCOUNTNO',[ixUnique]);
    end;
end;
procedure TUser.FillDefaults(aDataSet: TDataSet);
begin
  with BaseApplication as IBaseDbInterface do
    aDataSet.FieldByName('ACCOUNTNO').AsString:=TBaseDBModule(DataModule).Numbers.GetNewNumber('USERS');
end;

procedure TUser.SelectByParent(aParent: Variant);
begin
  with  DataSet as IBaseDBFilter, BaseApplication as IBaseDBInterface, DataSet as IBaseManageDB do
    begin
      if aParent=Null then
        Filter := TBaseDBModule(DataModule).ProcessTerm('('+QuoteField('PARENT')+'='+TBaseDBModule(DataModule).QuoteValue('')+')')
      else
        Filter := '('+QuoteField('PARENT')+'='+QuoteValue(aParent)+')';
    end;
end;

function TUser.CreateTable : Boolean;
begin
  Result := inherited CreateTable;
  FOptions.CreateTable;
  FRights.CreateTable;
  FFollows.CreateTable;
  FPayGroups.CreateTable;
end;
procedure TUser.SetPasswort(aPasswort: string);
var
  aGUID: TGUID;
  aSalt: String;
  aRes: String;
begin
  if not CanEdit then
    DataSet.Edit;
  Salt.AsString:=GetRandomSalt;
  aRes := '$'+SHA1Print(SHA1String(SHA1Print(SHA1String(MergeSalt(aPasswort,Salt.AsString)))));
  Passwort.AsString:=aRes;
  DataSet.Post;
end;

function TUser.GetRandomSalt: string;
var
  aSalt: String;
  aGUID: TGUID;
begin
  CreateGUID(aGUID);
  aSalt := md5Print(md5String(GUIDToString(aGUID)+UserName.AsString));
  CreateGUID(aGUID);
  aSalt += md5Print(md5String(GUIDToString(aGUID)+aSalt));
  CreateGUID(aGUID);
  aSalt += md5Print(md5String(GUIDToString(aGUID)));
  aSalt :=copy(aSalt,0,104);
  Result := aSalt;
end;

function TUser.CheckUserPasswort(aPasswort: string): Boolean;
var
  aRes: String;
  aSalt: String;
begin
  if copy(Passwort.AsString,0,1) <> '$' then
    Result := md5print(MD5String(aPasswort)) = Passwort.AsString
  else
    begin
      aSalt := Salt.AsString;
      aRes := '$'+SHA1Print(SHA1String(SHA1Print(SHA1String(MergeSalt(aPasswort,aSalt)))));
      Result := (copy(aRes,0,length(Passwort.AsString)) = Passwort.AsString) and (length(Passwort.AsString) > 30);
    end;
end;

procedure TUser.LoginWasOK;
begin
  with DataSet as IBaseManageDB do
  if (Count>0) and (FieldByName('LASTLOGIN') <> nil) and (FieldByName('LASTLOGIN').ReadOnly = False) then
    begin
      UpdateStdFields := False;
      try
        DataSet.Edit;
        FieldByName('LASTLOGIN').AsDateTime:=Now();
        DataSet.Post;
      except
        DataSet.Cancel;
      end;
    end;
  with DataSet as IBaseManageDB do
    UpdateStdFields := True;
end;

procedure TUser.SelectByAccountno(aAccountno: string);
var
  aField: String = '';
begin
  with BaseApplication as IBaseDBInterface do
    with DataSet as IBaseDBFilter do
      begin
        Filter := TBaseDBModule(DataModule).QuoteField('ACCOUNTNO')+'='+TBaseDBModule(DataModule).QuoteValue(aAccountno);
      end;
end;

function TUser.GetLeaderAccountno: string;
var
  aUsers: TUser;
begin
  Result := '';
  aUsers := Tuser.CreateEx(nil,DataModule);
  aUsers.SelectByParent(FieldbyName('PARENT').AsVariant);
  aUsers.Open;
  if aUsers.DataSet.Locate('POSITION','LEADER',[loCaseInsensitive]) then
    Result := aUsers.FieldByName('ACCOUNTNO').AsString;
  aUsers.Free;
end;

function TBaseDBDataset.GetID: TField;
begin
  Result := nil;
  if DataSet.Active and (DataSet.FieldDefs.IndexOf('SQL_ID')>-1) then
    Result := DataSet.FieldByName('SQL_ID');
end;

function TBaseDBDataset.GetTimestamp: TField;
begin
  Result := DataSet.FieldByName('TIMESTAMPD');
end;

procedure TBaseDBDataset.DefineFields(aDataSet: TDataSet);
begin
  with aDataSet as IBaseManageDB do
    begin
      if Assigned(ManagedFieldDefs) then
        begin
          ManagedFieldDefs.Add('DELETED',ftString,1);
        end;
      if Assigned(ManagedIndexdefs) then
        begin
          ManagedIndexDefs.Add('DELETED','DELETED',[]);
        end;
    end;
end;

procedure TBaseDBDataset.DefineDefaultFields(aDataSet: TDataSet;
  HasMasterSource: Boolean);
begin
  with aDataSet as IBaseManageDB do
    begin
      if Assigned(ManagedFieldDefs) then
        begin
          if ManagedFieldDefs.IndexOf('LOCKEDAT')=-1 then
            ManagedFieldDefs.Add('LOCKEDAT',ftDateTime,0,False);
        end;
      if Assigned(ManagedIndexdefs) then
        begin
          if (ManagedFieldDefs.IndexOf('REF_ID') > -1) or (HasMasterSource {and (ManagedFieldDefs.IndexOf('REF_ID')>-1)}) then
            ManagedIndexDefs.Add('REF_ID','REF_ID',[]);
          ManagedIndexDefs.Add('TIMESTAMPD','TIMESTAMPD',[]);
        end;
    end;
end;
procedure TBaseDBDataset.DefineUserFields(aDataSet: TDataSet);
var
  UserFields: TUserfielddefs;
begin exit;
  with BaseApplication as IBaseDbInterface do
    begin
      UserFields := TBaseDBModule(DataModule).Userfielddefs;
      if Assigned(UserFields) then
        begin
          if not UserFields.DataSet.Active then
            begin
              UserFields.CreateTable;
              UserFields.Open;
            end;
          with aDataSet as IBaseManageDB do
            UserFields.DataSet.Filter := TBaseDBModule(DataModule).QuoteField('TTABLE')+'='+TBaseDBModule(DataModule).QuoteValue(GetTableName);
          UserFields.DataSet.Filtered:=True;
          UserFields.DataSet.First;
          while not UserFields.DataSet.EOF do
            begin
              with aDataSet as IBaseManageDB do
                if Assigned(ManagedFieldDefs) then
                  with ManagedFieldDefs do
                    begin
                      if ManagedFieldDefs.IndexOf('U'+UserFields.FieldByName('TFIELD').AsString)=-1 then
                        begin
                          if UserFields.FieldByName('TYPE').AsString = 'STRING' then
                            Add('U'+UserFields.FieldByName('TFIELD').AsString,ftString,UserFields.FieldByName('SIZE').AsInteger)
                          else if UserFields.FieldByName('TYPE').AsString = 'DATETIME' then
                            Add('U'+UserFields.FieldByName('TFIELD').AsString,ftDateTime,0)
                          else if UserFields.FieldByName('TYPE').AsString = 'FLOAT' then
                            Add('U'+UserFields.FieldByName('TFIELD').AsString,ftFloat,0)
                          else if UserFields.FieldByName('TYPE').AsString = 'TEXT' then
                            Add('U'+UserFields.FieldByName('TFIELD').AsString,ftMemo,0)
                          else if UserFields.FieldByName('TYPE').AsString = 'INTEGER' then
                            Add('U'+UserFields.FieldByName('TFIELD').AsString,ftInteger,0)
                          ;
                        end;
                    end;
              UserFields.DataSet.Next;
            end;
        end;
    end;
end;
procedure TBaseDBDataset.FillDefaults(aDataSet: TDataSet);
begin
  with BaseApplication as IBaseDbInterface do
    begin
      if aDataSet.FieldDefs.IndexOf('TIMESTAMPT') <> -1 then
        aDataSet.FieldByName('TIMESTAMPT').AsFloat:=frac(Now());
    end;
end;
procedure TBaseDBDataset.SetDisplayLabelName(aDataSet: TDataSet; aField,
  aName: string);
var
  Idx: LongInt;
begin
  Idx := aDataSet.FieldDefs.IndexOf(aField);
  if (Idx <> -1) and (aDataSet.Fields.Count>Idx) then
    aDataSet.Fields[Idx].DisplayLabel := aName
  else if (Idx <> -1) then
    aDataSet.FieldDefs[Idx].DisplayName:=aName;
end;
procedure TBaseDBDataset.SetDisplayLabels(aDataSet: TDataSet);
begin
  FDisplayLabelsWasSet := True;
  SetDisplayLabelName(aDataSet,'LOCATION',strLocation);
  SetDisplayLabelName(aDataSet,'STARTDATE',strStart);
  SetDisplayLabelName(aDataSet,'DUEDATE',strDue);
  SetDisplayLabelName(aDataSet,'ACCOUNTNO',strAccountNo);
  SetDisplayLabelName(aDataSet,'NAME',strName);
  SetDisplayLabelName(aDataSet,'PASSWORD',strPassword);
  SetDisplayLabelName(aDataSet,'TABLENAME',strTablename);
  SetDisplayLabelName(aDataSet,'TYPE',strType);
  SetDisplayLabelName(aDataSet,'LANGUAGE',strLanguage);
  SetDisplayLabelName(aDataSet,'ID',strID);
  SetDisplayLabelName(aDataSet,'STATUS',strStatus);
  SetDisplayLabelName(aDataSet,'ACTION',strAction);
  SetDisplayLabelName(aDataSet,'CHANGEDBY',strChangedBy);
  SetDisplayLabelName(aDataSet,'USER',strUser);
  SetDisplayLabelName(aDataSet,'READ',strRead);
  SetDisplayLabelName(aDataSet,'SENDER',strSender);
  SetDisplayLabelName(aDataSet,'SENDDATE',strDate);
  SetDisplayLabelName(aDataSet,'SENDTIME',strTime);
  SetDisplayLabelName(aDataSet,'SUBJECT',strSubject);
  SetDisplayLabelName(aDataSet,'MATCHCODE',strMatchCode);
  SetDisplayLabelName(aDataSet,'CURRENCY',strCurrency);
  SetDisplayLabelName(aDataSet,'PAYMENTTAR',strPaymentTarget);
  SetDisplayLabelName(aDataSet,'CRDATE',strCreatedDate);
  SetDisplayLabelName(aDataSet,'CHDATE',strChangedDate);
  SetDisplayLabelName(aDataSet,'CREATEDBY',strCreatedBy);
  SetDisplayLabelName(aDataSet,'CHANGEDBY',strChangedBy);
  SetDisplayLabelName(aDataSet,'SORTCODE',strSortCode);
  SetDisplayLabelName(aDataSet,'ACCOUNT',strAccount);
  SetDisplayLabelName(aDataSet,'INSTITUTE',strInstitute);
  SetDisplayLabelName(aDataSet,'DESCR',strDescription);
  SetDisplayLabelName(aDataSet,'DESC',strDescription);
  SetDisplayLabelName(aDataSet,'DATA',strData);
  SetDisplayLabelName(aDataSet,'EMPLOYEE',strEmployee);
  SetDisplayLabelName(aDataSet,'DEPARTMENT',strDepartment);
  SetDisplayLabelName(aDataSet,'POSITION',strPosition);
  SetDisplayLabelName(aDataSet,'VERSION',strVersion);
  SetDisplayLabelName(aDataSet,'BARCODE',strBarcode);
  SetDisplayLabelName(aDataSet,'SHORTTEXT',strShorttext);
  SetDisplayLabelName(aDataSet,'QUANTITYU',strQuantityUnit);
  SetDisplayLabelName(aDataSet,'VAT',strVat);
  SetDisplayLabelName(aDataSet,'UNIT',strUnit);
  SetDisplayLabelName(aDataSet,'VERSION',strVersion);
  SetDisplayLabelName(aDataSet,'PTYPE',strPriceType);
  SetDisplayLabelName(aDataSet,'PRICE',strPrice);
  SetDisplayLabelName(aDataSet,'MINCOUNT',strMinCount);
  SetDisplayLabelName(aDataSet,'MAXCOUNT',strMaxCount);
  SetDisplayLabelName(aDataSet,'VALIDFROM',strValidFrom);
  SetDisplayLabelName(aDataSet,'VALIDTO',strValidTo);
  SetDisplayLabelName(aDataSet,'PROBLEM',strProblem);
  SetDisplayLabelName(aDataSet,'ASSEMBLY',strAssembly);
  SetDisplayLabelName(aDataSet,'PART',strPart);
  SetDisplayLabelName(aDataSet,'STORAGEID',strID);
  SetDisplayLabelName(aDataSet,'STORNAME',strName);
  SetDisplayLabelName(aDataSet,'PLACE',strPlace);
  SetDisplayLabelName(aDataSet,'QUANTITY',strQuantity);
  SetDisplayLabelName(aDataSet,'SERIAL',strSerial);
  SetDisplayLabelName(aDataSet,'DATE',strDate);
  SetDisplayLabelName(aDataSet,'COMMISSION',strCommission);
  SetDisplayLabelName(aDataSet,'NUMBER',strNumber);
  SetDisplayLabelName(aDataSet,'ADDRNO',strNumber);
  SetDisplayLabelName(aDataSet,'CUSTNO',strCustomerNumber);
  SetDisplayLabelName(aDataSet,'CUSTNAME',strName);
  SetDisplayLabelName(aDataSet,'VATH',strHalfVat);                   //Halbe MwSt
  SetDisplayLabelName(aDataSet,'VATF',strFullVat);                   //Volle MwSt
  SetDisplayLabelName(aDataSet,'NETPRICE',strNetPrice);                //Nettopreis
  SetDisplayLabelName(aDataSet,'DISCOUNT',strDiscount);                //Rabatt
  SetDisplayLabelName(aDataSet,'GROSSPRICE',strGrossPrice);              //Bruttoprice
  SetDisplayLabelName(aDataSet,'DONE',strDone);
  SetDisplayLabelName(aDataSet,'ORDERNO',strOrderNo);
  SetDisplayLabelName(aDataSet,'TITLE',strTitle);
  SetDisplayLabelName(aDataSet,'ADDITIONAL',strAdditional);
  SetDisplayLabelName(aDataSet,'ADDRESS',strAdress);
  SetDisplayLabelName(aDataSet,'CITY',strCity);
  SetDisplayLabelName(aDataSet,'ZIP',strPostalCode);
  SetDisplayLabelName(aDataSet,'STATE',strState);
  SetDisplayLabelName(aDataSet,'COUNTRY',strLand);
  SetDisplayLabelName(aDataSet,'POBOX',strPostBox);
  SetDisplayLabelName(aDataSet,'POSNO',strPosNo);
  SetDisplayLabelName(aDataSet,'POSTYP',strType);
  SetDisplayLabelName(aDataSet,'TPOSNO',strTenderPosNo);                //Auschreibungsnummer
  SetDisplayLabelName(aDataSet,'IDENT',strIdent);
  SetDisplayLabelName(aDataSet,'TIDENT',strIdent);
  SetDisplayLabelName(aDataSet,'TVERSION',strVersion);
  SetDisplayLabelName(aDataSet,'TEXTTYPE',strTextTyp);
  SetDisplayLabelName(aDataSet,'TEXT',strText);
  SetDisplayLabelName(aDataSet,'REFERENCE',strReference);
  SetDisplayLabelName(aDataSet,'STORAGE',strStorage);
  SetDisplayLabelName(aDataSet,'ACTIONICON',' ');
  SetDisplayLabelName(aDataSet,'TIMESTAMPD',strDate);
  SetDisplayLabelName(aDataSet,'RESERVED',strReserved);
  SetDisplayLabelName(aDataSet,'PROPERTY',strProperty);
  SetDisplayLabelName(aDataSet,'VALUE',strValue);
  SetDisplayLabelName(aDataSet,'QUANTITYD',strQuantityDelivered);               //Menge Geliefert
  SetDisplayLabelName(aDataSet,'QUANTITYC',strQuantityCalculated);               //Menge berechnet
  SetDisplayLabelName(aDataSet,'PURCHASE',strPurchasePrice);                //Einkaufspreis
  SetDisplayLabelName(aDataSet,'SELLPRICE',strSellPrice);               //Verkaufspreis
  SetDisplayLabelName(aDataSet,'COMPRICE',strCommonPrice);                //Common Price
  SetDisplayLabelName(aDataSet,'POSPRICE',strPrice);                //Gesamtpreis
  SetDisplayLabelName(aDataSet,'GROSSPRICE',strGrossPrice);              //Bruttoprice
  SetDisplayLabelName(aDataSet,'ERROR',strProblem);
  SetDisplayLabelName(aDataSet,'BALLANCE',strBallance);
  SetDisplayLabelName(aDataSet,'VALUEDATE',strDate);
  SetDisplayLabelName(aDataSet,'PURPOSE',strPurpose);
  SetDisplayLabelName(aDataSet,'CHECKED',strChecked);
  SetDisplayLabelName(aDataSet,'CATEGORY',strCategory);
  SetDisplayLabelName(aDataSet,'CUSTOMER',strContact);
  SetDisplayLabelName(aDataSet,'PAYEDON',strPaid);
  SetDisplayLabelName(aDataSet,'DELIVERED',strDelivered);
  SetDisplayLabelName(aDataSet,'ACTIVE',strActive);
  SetDisplayLabelName(aDataSet,'START',strStart);
  SetDisplayLabelName(aDataSet,'END',strEnd);
  SetDisplayLabelName(aDataSet,'LINK',strLink);
  SetDisplayLabelName(aDataSet,'JOB',strTask);
  SetDisplayLabelName(aDataSet,'ISPAUSE',strPause);
  SetDisplayLabelName(aDataSet,'ODATE',strOriginaldate);
  SetDisplayLabelName(aDataSet,'NOTE',strNotes);
  SetDisplayLabelName(aDataSet,'PROJECT',strProject);
  SetDisplayLabelName(aDataSet,'PROJECTNR',strProjectNumber);
  SetDisplayLabelName(aDataSet,'SUMMARY',strSummary);
  SetDisplayLabelName(aDataSet,'OWNER',strOwner);
  SetDisplayLabelName(aDataSet,'AVALIBLE',strAvalible);
  SetDisplayLabelName(aDataSet,'ICON',' ');
  SetDisplayLabelName(aDataSet,'NEEDSACTION',strNeedsAction);
  SetDisplayLabelName(aDataSet,'COSTCENTRE',strCostcentre);
end;

function TBaseDBDataset.GetLink: string;
begin
  Result := TBaseDBModule(DataModule).BuildLink(DataSet);
end;

procedure TBaseDBDataset.DuplicateRecord(DoPost: Boolean);
var
  Data : array of string;
  i : integer;
  max : integer;
begin
  max := DataSet.fields.count -1;
  SetLength(data,max+1);

  // Copy the Record to the Array
  for i := 0 to max do
    Data[i] := DataSet.fields[i].AsString;

  DataSet.Append;
  for i := 0 to max do
    if  (DataSet.fields[i].DataType <> ftAutoInc)
    and (DataSet.FieldDefs[i].Name <> 'SQLITE_ID')
    and (DataSet.FieldDefs[i].Name <> 'SQL_ID')
    and (Data[i] <> '') then
      DataSet.fields[i].AsString := Data[i];
  if DoPost then
    DataSet.Post;
end;
procedure TMeasurement.DataSetAfterPost(aDataSet: TDataSet);
var
  aDiff: ValReal;
  aOld,aNew : real;
begin
  if CurrentChanged then
    begin
      CurrentChanged:=False;
      if DataSet.ControlsDisabled then exit;
      aOld := CurrentValue;
      aNew := FieldByName('CURRENT').AsFloat;
      aDiff := Abs(Abs(aNew)-Abs(aOld));
      if (FieldByName('TOLLERANCE').AsFloat >0) and (aDiff < FieldByName('TOLLERANCE').AsFloat) then
        begin
          DataSet.DisableControls;
          Edit;
          FieldByName('CURRENT').AsFloat:=aOld;
          Post;
          DataSet.EnableControls;
          exit;
        end;
      Data.Append;
      Data.FieldByName('DATA').AsFloat:=FieldByName('CURRENT').AsFloat;
      Data.Post;
    end;
end;
procedure TMeasurement.DataSetBeforeEdit(aDataSet: TDataSet);
begin
  CurrentValue:=FieldByName('CURRENT').AsFloat;
end;
procedure TMeasurement.FDSDataChange(Sender: TObject; Field: TField);
begin
  if not Assigned(Field) then exit;
  if DataSet.ControlsDisabled then exit;
  if (Dataset.State <> dsInsert)
  then
    begin
      if (Field.FieldName = 'CURRENT') then
        begin
          CurrentChanged := TRue;
        end;
    end;
end;
function TMeasurement.GetCurrent: TField;
begin
  Result := FieldByName('CURRENT');
end;
constructor TMeasurement.CreateEx(aOwner: TComponent; DM: TComponent;
  aConnection: TComponent; aMasterdata: TDataSet);
begin
  inherited CreateEx(aOwner, DM, aConnection, aMasterdata);
  (DataSet as IBaseManageDB).UseIntegrity:=False;
  FMesdata := TMeasurementData.CreateEx(Self,DataModule,aConnection,DataSet);
  FDS := TDataSource.Create(Self);
  FDS.DataSet := DataSet;
  FDS.OnDataChange:=@FDSDataChange;
  CurrentChanged:=False;
  DataSet.AfterPost:=@DataSetAfterPost;
  DataSet.BeforeEdit:=@DataSetBeforeEdit;
end;
destructor TMeasurement.Destroy;
begin
  FMesdata.Free;
  FDS.Free;
  inherited Destroy;
end;
procedure TMeasurement.DefineFields(aDataSet: TDataSet);
begin
  with aDataSet as IBaseManageDB do
    begin
      TableName := 'MEASUREMENTS';
      if Assigned(ManagedFieldDefs) then
        with ManagedFieldDefs do
          begin
            Add('NAME',ftString,100,True);
            Add('ID',ftString,100,False);
            Add('TYPE',ftString,100,False);
            Add('CURRENT',ftFloat,0,False);
            Add('UNIT',ftString,15,False);
            Add('CHART',ftString,1,False);
            Add('COLOR',ftString,30,False);
            Add('RANGE',ftString,20,False);
            Add('POSITION',ftString,1,False);
            Add('INTERPOLATE',ftString,1,False);
            Add('TOLLERANCE',ftFloat,0,False);
          end;
      if Assigned(ManagedIndexdefs) then
        with ManagedIndexDefs do
          begin
            Add('ID','ID',[]);
            Add('NAME','NAME',[]);
          end;
    end;
end;
function TMeasurement.CreateTable: Boolean;
begin
  Result:=inherited CreateTable;
  FMesdata.CreateTable;
end;
procedure TMeasurementData.DefineFields(aDataSet: TDataSet);
begin
  with aDataSet as IBaseManageDB do
    begin
      TableName := 'MEASDATA';
      if Assigned(ManagedFieldDefs) then
        with ManagedFieldDefs do
          begin
            Add('DATA',ftFloat,0,True);
            Add('DATE',ftDateTime,0,True);
          end;
      if Assigned(ManagedIndexdefs) then
        with ManagedIndexDefs do
          begin
            Add('DATE','DATE',[]);
          end;
    end;
end;
constructor TMeasurementData.CreateEx(aOwner: TComponent; DM: TComponent;
  aConnection: TComponent; aMasterdata: TDataSet);
begin
  inherited;
  with DataSet as IBaseDBFilter do
    begin
      SortFields := 'DATE';
      SortDirection := sdDescending;
      UpdateFloatFields:=True;
      Limit:=100;
    end;
end;
procedure TMeasurementData.FillDefaults(aDataSet: TDataSet);
begin
  inherited FillDefaults(aDataSet);
  FieldByName('DATE').AsDateTime:=Now();
end;

initialization
  RegisterdataSetClass('USERS',TUser);
  RegisterdataSetClass('ALLOBJECTS',TObjects);
  RegisterdataSetClass('BOILERPLATE',TBoilerplate);
  RegisterdataSetClass('FILTERS',TFilters);
  RegisterdataSetClass('FORMS',TForms);
  RegisterdataSetClass('REPORTS',TReports);
  RegisterdataSetClass('OPTIONS',TOptions);
  RegisterdataSetClass('PERMISSIONS',TPermissions);
  RegisterdataSetClass('MEASUREMENTS',TMeasurement);
  RegisterdataSetClass('HISTORY',TBaseHistory);
end.

