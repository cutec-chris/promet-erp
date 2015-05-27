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
  Classes, SysUtils, db, uBaseDbDataSet, Variants, uIntfStrConsts, DOM,
  Contnrs,uBaseDatasetInterfaces
  {$IFDEF LCL}
  ,Graphics
  {$ENDIF}
  ;
type
  { TBaseDBDataset }

  TBaseDBDataset = class(TComponent)
  private
    fChanged: Boolean;
    FDataSet: TDataSet;
    FDisplayLabelsWasSet : Boolean;
    FDataModule : TComponent;
    FOnChanged: TNotifyEvent;
    FOnRemoved: TNotifyEvent;
    FParent: TBaseDBDataset;
    FUpdateFloatFields: Boolean;
    FSecModified: Boolean;
    FDoChange:Integer;
    FUseIntegrity : Boolean;
    function GetActive: Boolean;
    function GetCanEdit: Boolean;
    function GetCaption: string;
    function GetConnection: TComponent;
    function GetCount: Integer;
    function GetFilter: string;
    function GetFRows: Integer;
    function GetFullCount: Integer;
    function GetID: TField;
    function GetLimit: Integer;
    function GetSortDirection: TSortDirection;
    function GetSortFields: string;
    function GetState: TDataSetState;
    function GetTableName: string;
    function GetTimestamp: TField;
    procedure SetActive(AValue: Boolean);
    procedure SetFilter(AValue: string);
    procedure SetFRows(AValue: Integer);
    procedure SetLimit(AValue: Integer);
    procedure SetSortDirection(AValue: TSortDirection);
    procedure SetSortFields(AValue: string);
  public
    constructor CreateExIntegrity(aOwner : TComponent;DM : TComponent;aUseIntegrity : Boolean;aConnection : TComponent = nil;aMasterdata : TDataSet = nil);virtual;
    constructor CreateEx(aOwner : TComponent;DM : TComponent;aConnection : TComponent = nil;aMasterdata : TDataSet = nil);virtual;
    constructor Create(aOwner : TComponent);override;
    destructor Destroy;override;
    property DataSet : TDataSet read FDataSet write FDataSet;
    property DataModule : TComponent read FDataModule;
    procedure Open;virtual;
    procedure Close;virtual;
    function CreateTable : Boolean;virtual;
    procedure DefineFields(aDataSet : TDataSet);virtual;abstract;
    procedure DefineDefaultFields(aDataSet : TDataSet;HasMasterSource : Boolean);
    procedure DefineUserFields(aDataSet: TDataSet);
    procedure FillDefaults(aDataSet : TDataSet);virtual;
    procedure Select(aID : Variant);virtual;
    procedure SelectChangedSince(aDate : TDateTime);virtual;
    procedure SetDisplayLabelName(aDataSet: TDataSet;aField, aName: string);
    procedure SetDisplayLabels(aDataSet : TDataSet);virtual;
    property Id : TField read GetID;
    property TimeStamp : TField read GetTimestamp;
    property Count : Integer read GetCount;
    property FullCount : Integer read GetFullCount;
    function GetBookmark: LargeInt;
    function GotoBookmark(aRec : Variant) : Boolean;
    function GetLink : string;
    procedure FreeBookmark(aRec : Variant);
    procedure DuplicateRecord(DoPost : Boolean = False);
    property Connection : TComponent read GetConnection;
    property State : TDataSetState read GetState;
    property Caption : string read GetCaption;
    property TableName : string read GetTableName;
    property Changed : Boolean read FChanged;
    procedure DisableChanges;
    procedure EnableChanges;
    procedure Change;virtual;
    procedure UnChange;virtual;
    procedure CascadicPost;virtual;
    procedure CascadicCancel;virtual;
    procedure Delete;virtual;
    procedure Insert;virtual;
    procedure Append;virtual;
    procedure First;virtual;
    procedure Last;virtual;
    procedure Next;virtual;
    procedure Prior;virtual;
    procedure Post;virtual;
    procedure Edit;virtual;
    procedure Cancel;virtual;
    function Locate(const keyfields: string; const keyvalues: Variant; options: TLocateOptions) : boolean; virtual;
    function EOF : Boolean;virtual;
    function FieldByName(const aFieldName : string) : TField;virtual;
    procedure Assign(Source: TPersistent); override;
    procedure DirectAssign(Source : TPersistent);
    procedure Filter(aFilter : string;aLimit : Integer = 0);virtual;
    procedure FilterEx(aFilter : string;aLimit : Integer = 0;aOrderBy : string = '';aSortDirection : string = 'ASC';aLocalSorting : Boolean = False;aGlobalFilter : Boolean = True;aUsePermissions : Boolean = False;aFilterIn : string = '');virtual;
    property ActualFilter : string read GetFilter write SetFilter;
    property ActualLimit : Integer read GetLimit write SetLimit;
    property SortFields : string read GetSortFields write SetSortFields;
    property SortDirection : TSortDirection read GetSortDirection write SetSortDirection;
    property FetchRows : Integer read GetFRows write SetFRows;
    property Parent : TBaseDBDataSet read FParent;
    property UpdateFloatFields : Boolean read FUpdateFloatFields write FUpdateFloatFields;
    property CanEdit : Boolean read GetCanEdit;
    property Active : Boolean read GetActive write SetActive;
    property OnChange : TNotifyEvent read FOnChanged write FOnChanged;
    property OnRemove : TNotifyEvent read FOnRemoved write FOnRemoved;
  end;

  TReplaceFieldFunc = procedure(aField : TField;aOldValue : string;var aNewValue : string);

  { TBaseDbList }

  TBaseDbList = class(TBaseDBDataSet)
  private
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
    procedure Delete; override;
    function Find(aIdent : string;Unsharp : Boolean = False) : Boolean;virtual;
    function  ExportToXML : string;virtual;
    procedure ImportFromXML(XML : string;OverrideFields : Boolean = False;ReplaceFieldFunc : TReplaceFieldFunc = nil);virtual;
    procedure OpenItem(AccHistory: Boolean=True);virtual;
    procedure BuildSearchIndexes;virtual;
    procedure CascadicPost; override;
    procedure GenerateThumbnail;virtual;
    property Text : TField read GetText;
    property Number : TField read GetNumber;
    property BookNumber : TField read GetBookNumber;
    property Barcode : TField read GetBarcode;
    property Description : TField read GetDescription;
    property Commission : TField read GetCommission;
    property Status : TField read GetStatus;
    property Typ : string read GetTyp;
    property Matchcode : TField read GetMatchcode;
    function SelectFromLink(aLink : string) : Boolean;virtual;
    function SelectFromNumber(aNumber : string) : Boolean;virtual;
  end;
  TBaseDBDatasetClass = class of TBaseDBDataset;
  TBaseDBListClass = class of TBaseDBList;

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
  TAccessHistory = class(TBaseHistory)
  public
    procedure DefineFields(aDataSet : TDataSet);override;
  end;
  TImages = class;
  TLinks = class;

  { TObjects }

  TObjects = class(TBaseDbList)
  private
    FHistory: TBaseHistory;
    FImages: TImages;
    FLinks: TLinks;
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
  end;

  { TVariables }

  TVariables = class(TBaseDBDataset)
  protected
  public
    procedure DefineFields(aDataSet : TDataSet);override;
    procedure Add(aName,aId : string;aValue : Double);
  end;
  TOptions = class;
  TFollowers = class;
  TRights = class;
  TPayGroups = class;
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
    function CheckPasswort(aPasswort : string) : Boolean;
    function CheckSHA1Passwort(aPasswort : string) : Boolean;
    procedure LoginWasOK;
    procedure SelectByAccountno(aAccountno : string);virtual;
    function GetLeaderAccountno : string;
    property History : TBaseHistory read FHistory;
    property WorkTime : Extended read GetWorktime;
    property IDCode : TField read GetIDCode;
  end;
  TActiveUsers = class(TBaseDBDataSet)
  public
    procedure DefineFields(aDataSet : TDataSet);override;
    procedure Delete; override;
  end;
  TUserfielddefs = class(TBaseDBDataSet)
  public
    procedure DefineFields(aDataSet : TDataSet);override;
  end;
  TNumbersets = class(TBaseDBDataSet)
  public
    procedure DefineFields(aDataSet : TDataSet);override;
    function GetNewNumber(Numberset : string) : string;
    function HasNumberSet(Numberset : string) : Boolean;
  end;
  TPayGroups = class(TBaseDBDataSet)
  public
    procedure DefineFields(aDataSet : TDataSet);override;
  end;
  TMandantDetails = class(TBaseDBDataSet)
  public
    procedure DefineFields(aDataSet : TDataSet);override;
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
  TPermissions = class(TBaseDBDataSet)
  public
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
  TReports = class(TBaseDBDataSet)
  public
    procedure DefineFields(aDataSet : TDataSet);override;
  end;

  { TOptions }

  TOptions = class(TBaseDBDataSet)
  public
    procedure DefineFields(aDataSet : TDataSet);override;
    procedure Open; override;
    function GetOption(aSection, aIdent, DefaultValue: string): string;
    procedure SetOption(aSection,aIdent, Value : string);
  end;
  TFollowers = class(TBaseDBDataSet)
  private
    function GetLink: TField;
  public
    procedure DefineFields(aDataSet : TDataSet);override;
    procedure Open; override;
    property Link : TField read GetLink;
  end;
  TFilters = class(TBaseDBDataSet)
  public
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
  TImages = class(TBaseDBDataSet)
  public
    procedure DefineFields(aDataSet : TDataSet);override;
    {$IFDEF LCL}
    function AddFromFile(aFile : string) : Boolean;
    {$ENDIF}
    procedure GenerateThumbnail(aThumbnail : TBaseDbDataSet);
  end;
  TDeletedItems = class(TBaseDBDataSet)
  public
    procedure DefineFields(aDataSet : TDataSet);override;
  end;
var ImportAble : TClassList;
implementation
uses uBaseDBInterface, uBaseApplication, uBaseSearch,XMLRead,XMLWrite,Utils,
  md5,sha1,uData,uthumbnails,base64;
resourcestring
  strNumbersetDontExists        = 'Nummernkreis "%s" existiert nicht !';
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

{ TVariables }

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
  FHistory := TBaseHistory.CreateEx(Self,DM,aConnection,DataSet);
  FImages := TImages.CreateEx(Self,DM,aConnection,DataSet);
  FLinks := TLinks.CreateEx(Self,DM,aConnection);
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
            Add('VERSION',ftString,20,False);
            Add('LINK',ftString,400,False);
            Add('STATUS',ftString,4,False);
            Add('ICON',ftInteger,0,False);
            Add('NOTICE',ftMemo,0,False);
            Add('TREEENTRY',ftLargeint,0,False);
            Add('CHANGEDBY',ftString,4,False);
          end;
    end;
end;

procedure TObjects.FillDefaults(aDataSet: TDataSet);
begin
  inherited FillDefaults(aDataSet);
  FieldByName('ICON').AsInteger:=Data.GetLinkIcon('ALLOBJECTS@',True);
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
            Filter := TBaseDBModule(DataModule).QuoteField(TableName)+'.'+Data.QuoteField('SQL_ID')+'='+Data.QuoteValue(tmp);
          end
        else
          Filter := Data.QuoteField('LINK')+'='+Data.QuoteValue(aLink);
      end;
end;

procedure TObjects.SelectByRefId(aId: Variant);
begin
  with BaseApplication as IBaseDbInterface do
    begin
      with Self.DataSet as IBaseDBFilter do
        begin
          if aId <> Null then
            Filter := TBaseDBModule(DataModule).QuoteField(TableName)+'.'+Data.QuoteField('SQL_ID')+'='+TBaseDBModule(DataModule).QuoteValue(Format('%d',[Int64(aID)]))
          else
            Filter := TBaseDBModule(DataModule).QuoteField(TableName)+'.'+Data.QuoteField('SQL_ID')+'='+Data.QuoteValue('0');
          Limit := 0;
        end;
    end;
end;

procedure TAccessHistory.DefineFields(aDataSet: TDataSet);
begin
  inherited DefineFields(aDataSet);
  with aDataSet as IBaseManageDB do
    begin
      TableName := 'ACCHISTORY';
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
          end;
    end;
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

procedure TImages.GenerateThumbnail(aThumbnail: TBaseDBDataset);
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
                Data.StreamToBlobField(aStream,aThumbnail.DataSet,'THUMBNAIL');
              aThumbnail.Post;
            end;
          aStream.Free;
        end;
      if aThumbnail.State=dsInsert then
        aThumbnail.Cancel;
    end;
end;

{$IFDEF LCL}
function TImages.AddFromFile(aFile: string): Boolean;
var
  aPicture: TPicture;
  fe: String;
  s: TStream;
  i: SizeInt;
begin
  Insert;
  aPicture := TPicture.Create;
  aPicture.LoadFromFile(aFile);

  fe := aPicture.Graphic.GetFileExtensions;
  s := DataSet.CreateBlobStream(FieldByName('IMAGE'),bmwrite);
  try
    i := pos(';',fe);
    if i > 0 then fe := copy(fe,1,i-1);
      begin
        s.WriteAnsiString(fe);  //otherwise write file extension to stream
      end;
    aPicture.Graphic.SaveToStream(s);
  finally
    s.Free;
  end;
  Post;
end;
{$endif}
constructor TLinks.CreateEx(aOwner: TComponent; DM: TComponent;
  aConnection: TComponent; aMasterdata: TDataSet);
begin
  inherited CreateEx(aOwner, DM, aConnection, aMasterdata);
end;

procedure TLinks.Open;
begin
  with BaseApplication as IBaseDbInterface do
    begin
      with DataSet as IBaseDBFilter do
        begin
        if Assigned(FParent) then
          begin
            if not FParent.Id.IsNull then
              BaseFilter := Data.QuoteField('RREF_ID')+'='+Data.QuoteValue(FParent.Id.AsString)
            else
              BaseFilter := Data.QuoteField('RREF_ID')+'= 0';
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
  aLinkDesc := Data.GetLinkDesc(aLink);
  aIcon := Data.GetLinkIcon(aLink);
  if not Active then  Open;
  if Locate('LINK',aLink,[]) then exit;
  Append;
  with DataSet do
    begin
      FieldByName('LINK').AsString := aLink;
      FieldByName('NAME').AsString := aLinkDesc;
      FieldByName('ICON').AsInteger := aIcon;
      FieldByName('CHANGEDBY').AsString := Data.Users.Idcode.AsString;
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
function TBaseDbList.GetBookNumber: TField;
var
  aField: String;
begin
  aField := GetBookNumberFieldName;
  if aField <> '' then
    Result := DataSet.FieldByName(aField);
end;

function TBaseDbList.GetMatchcode: TField;
var
  aField: String;
begin
  aField := GetMatchcodeFieldName;
  if aField <> '' then
    Result := DataSet.FieldByName(aField);
end;
function TBaseDbList.GetBarcode: TField;
var
  aField: String;
begin
  aField := GetBarcodeFieldName;
  if aField <> '' then
    Result := DataSet.FieldByName(aField);
end;
function TBaseDbList.GetCommission: TField;
var
  aField: String;
begin
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
  if aField <> '' then
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
      with FDataSet as IBaseDBFilter do
        begin
          BaseSortFields := 'TIMESTAMPD';
          SortFields := 'TIMESTAMPD';
          SortDirection := sdDescending;
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
      if Data.States.DataSet.Locate('TYPE;STATUS',VarArrayOf([GetTyp,FieldByName(GetStatusFieldName).AsString]),[]) then
        Result := StrToIntDef(Data.States.DataSet.FieldByName('ICON').AsString,-1)
      else
        begin
          Data.SetFilter(Data.States,Data.QuoteField('TYPE')+'='+Data.QuoteValue(GetTyp));
          if Data.States.DataSet.Locate('TYPE;STATUS',VarArrayOf([GetTyp,FieldByName(GetStatusFieldName).AsString]),[]) then
            Result := StrToIntDef(Data.States.DataSet.FieldByName('ICON').AsString,-1)
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

procedure TBaseDbList.Delete;
var
  aObj: TObjects;
begin
  try
    aObj := TObjects.Create(nil);
    if not Data.TableExists(aObj.TableName) then
      begin
        aObj.CreateTable;
        aObj.Free;
        aObj := TObjects.CreateEx(nil,Data,nil,DataSet);
      end;
    aObj.SelectByRefId(Id.AsVariant);
    aObj.Open;
    while aObj.Count>0 do
      begin
        aObj.DataSet.Delete;
      end;
    aObj.Free;
  except
  end;
  inherited Delete;
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
function TBaseDbList.ExportToXML : string;
var
  Stream: TStringStream;
  Doc: TXMLDocument;
  RootNode: TDOMElement;

  procedure RecourseTables(aNode : TDOMNode;aDataSet : TDataSet);
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
          with aDataSet as IBaseManageDB do
            begin
              if pos('HISTORY',uppercase(TableName)) > 0 then exit;
              if Uppercase(TableName) = 'STORAGE' then exit;
            end;
          aData := Doc.CreateElement('TABLE');
          aNode.AppendChild(aData);
          with aDataSet as IBaseManageDB do
            aData.SetAttribute('NAME',TableName);
          DataNode := Doc.CreateElement('DATA');
          aData.AppendChild(DataNode);
          aDataSet.Open;
          aDataSet.Refresh;
          aDataSet.First;
          a := 0;
          while not aDataSet.EOF do
            begin
              inc(a);
              Row := Doc.CreateElement('ROW.'+IntToStr(a));
              DataNode.AppendChild(Row);
              for i := 1 to aDataSet.Fields.Count-1 do
                begin
                  tmp := aDataSet.Fields[i].FieldName;
                  if (tmp <> '')
                  and (tmp <> 'SQL_ID')
                  and (tmp <> 'REF_ID')
                  then
                    begin
                      tmp1 :=  aDataSet.Fields[i].AsString;
                      if (not aDataSet.Fields[i].IsNull) then
                        begin
                          if aDataSet.Fields[i].IsBlob then
                            Row.SetAttribute(tmp,EncodeStringBase64(aDataSet.Fields[i].AsString))
                          else
                            Row.SetAttribute(tmp,tmp1);
                        end;
                    end;
                end;
              with aDataSet as IBaseSubDataSets do
                begin
                  for i := 0 to GetCount-1 do
                    RecourseTables(Row,TBaseDBDataset(SubDataSet[i]).DataSet);
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
  RecourseTables(RootNode,DataSet);
  Stream := TStringStream.Create('');
  WriteXMLFile(Doc,Stream);
  Result := Stream.DataString;
  Doc.Free;
  Stream.Free;
end;
procedure TBaseDbList.ImportFromXML(XML: string;OverrideFields : Boolean = False;ReplaceFieldFunc : TReplaceFieldFunc = nil);
var
  Doc : TXMLDocument;
  Stream : TStringStream;
  RootNode: TDOMNode;
  i: Integer;

  procedure RecourseTables(aNode : TDOMNode;aDataSet : TDataSet);
  var
    i: Integer;
    a: Integer;
    bNode: TDOMNode;
    cNode: TDOMNode;
    ThisDataSet: TDataSet;
    function ProcessDataSet(ThisDataSet : TDataSet) : Boolean;
    var
      c,d: Integer;
      b: Integer;
      aNewValue: String;
    begin
      Result := False;
      with ThisDataSet as IBaseManageDB do
        if (TableName = aNode.Attributes.GetNamedItem('NAME').NodeValue) then
          begin
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
                    if ThisDataSet.FieldDefs.IndexOf(bNode.Attributes.Item[d].NodeName) <> -1 then
                      if (ThisDataSet.FieldByName(bNode.Attributes.Item[d].NodeName).IsNull or (OverrideFields))
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
                            ReplaceFieldFunc(ThisDataSet.FieldByName(bNode.Attributes.Item[d].NodeName),bNode.Attributes.Item[d].NodeValue,aNewValue);
                          if ThisDataSet.FieldByName(bNode.Attributes.Item[d].NodeName).IsBlob then
                            ThisDataSet.FieldByName(bNode.Attributes.Item[d].NodeName).AsString := DecodeStringBase64(aNewValue)
                          else
                            ThisDataSet.FieldByName(bNode.Attributes.Item[d].NodeName).AsString := aNewValue;
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
              with aDataSet as IBaseSubDataSets do
                begin
                  for a := 0 to GetCount-1 do
                    begin
                      if ProcessDataSet(TBaseDbDataSet(SubDataSet[a]).DataSet) then break;
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
             RecourseTables(RootNode.ChildNodes[i],DataSet);
        end;
  Stream.Free;
  Doc.Free;
end;

procedure TBaseDbList.OpenItem(AccHistory : Boolean = True);
var
  aHistory: TAccessHistory;
  aObj: TObjects;
begin
  if ((Self.Count=0) and (State<>dsInsert)) or (not Assigned(Id)) then exit;
  try
    try
      aHistory := TAccessHistory.Create(nil);
      aObj := TObjects.Create(nil);
      if AccHistory then
        begin
          if DataSet.State<>dsInsert then
            begin
              if not Data.TableExists(aHistory.TableName) then
                aHistory.CreateTable;
              aHistory.Free;
              aHistory := TAccessHistory.CreateEx(nil,Data,nil,DataSet);
              aHistory.AddItem(DataSet,Format(strItemOpened,[Data.GetLinkDesc(Data.BuildLink(DataSet))]),Data.BuildLink(DataSet));
            end;
        end;
      if DataSet.State<>dsInsert then
        begin
        if not Data.TableExists(aObj.TableName) then
          begin
            aObj.CreateTable;
            aObj.Free;
            aObj := TObjects.CreateEx(nil,Data,nil,DataSet);
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
            aObj.FieldByName('LINK').AsString:=Data.BuildLink(Self.DataSet);
            aObj.FieldByName('ICON').AsInteger:=Data.GetLinkIcon(Data.BuildLink(Self.DataSet),True);
            aObj.Post;
            Self.GenerateThumbnail;
          end
        else //Modify existing
          begin
            if aObj.Text.AsString<>Self.Text.AsString then
              begin
                aObj.Edit;
                aObj.Text.AsString := Self.Text.AsString;
                aObj.FieldByName('LINK').AsString:=Data.BuildLink(Self.DataSet);
              end;
            if aObj.Number.AsString<>Self.Number.AsString then
              begin
                aObj.Edit;
                aObj.Number.AsString := Self.Number.AsString;
                aObj.FieldByName('LINK').AsString:=Data.BuildLink(Self.DataSet);
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
      aHistory.Free;
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
        Filter := Data.QuoteField(GetNumberFieldName)+'='+Data.QuoteValue(aNumber);
      end;
end;

procedure TBaseDBDataset.Delete;
begin
  Change;
  if FDataSet.Active and (Count > 0) then
    FDataSet.Delete;
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

procedure TBaseDBDataset.First;
begin
  DataSet.First;
end;

procedure TBaseDBDataset.Last;
begin
  DataSet.Last;
end;

procedure TBaseDBDataset.Next;
begin
  DataSet.Next;
end;
procedure TBaseDBDataset.Prior;
begin
  DataSet.Prior;
end;

procedure TBaseDBDataset.Post;
begin
  if CanEdit then
    FDataSet.Post;
end;

procedure TBaseDBDataset.Edit;
begin
  if not CanEdit then
    DataSet.Edit;
end;

procedure TBaseDBDataset.Cancel;
begin
  if Assigned(FDataSet) and (FDataSet.Active) then
    FDataSet.Cancel;
end;

function TBaseDBDataset.Locate(const keyfields: string;
  const keyvalues: Variant; options: TLocateOptions): boolean;
begin
  Result := False;
  if DataSet.Active then
    Result := DataSet.Locate(keyfields,keyvalues,options);
end;

function TBaseDBDataset.EOF: Boolean;
begin
  Result := True;
  if Assigned(FDataSet) and (FDataSet.Active) then
    Result := FDataSet.EOF;
end;

function TBaseDBDataset.FieldByName(const aFieldName: string): TField;
begin
  Result := nil;
  if Assigned(DataSet) and DataSet.Active then
    if DataSet.FieldDefs.IndexOf(aFieldName)>=0 then
      Result := DataSet.FieldByName(aFieldname);
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

procedure TBaseDBDataset.FilterEx(aFilter: string; aLimit: Integer;
  aOrderBy: string; aSortDirection: string; aLocalSorting: Boolean;
  aGlobalFilter: Boolean; aUsePermissions: Boolean; aFilterIn: string);
begin
  with DataSet as IBaseDbFilter do
    begin
      UsePermissions := aUsePermissions;
      if (aOrderBy <> '') then
        begin
          SortFields:=aOrderBy;
          if aSortDirection = 'DESC' then
            SortDirection := sdDescending
          else
            SortDirection := sdAscending;
        end;
      Limit := aLimit;
      Filter := aFilter;
    end;
  Open;
end;

procedure TBaseDBDataset.Filter(aFilter: string; aLimit: Integer);
begin
  if (not ((ActualFilter=aFilter) and (aLimit=ActualLimit))) then
    FilterEx(aFilter,aLimit);
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
      with FDataSet as IBaseDBFilter do
        Limit := 50;
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
            Add('SUMMARY',ftString,120,False);
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
        Filter := Data.ProcessTerm('('+QuoteField('PARENT')+'='+Data.QuoteValue('')+')')
      else
        Filter := '('+QuoteField('PARENT')+'='+QuoteValue(aParent)+')';
    end;
end;
procedure TBaseHistory.SelectByRoot(aParent: Variant);
begin
  with  DataSet as IBaseDBFilter, BaseApplication as IBaseDBInterface, DataSet as IBaseManageDB do
    begin
      if aParent=Null then
        Filter := Data.ProcessTerm('('+QuoteField('ROOT')+'='+Data.QuoteValue('')+')')
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
    Result := AddItemSR(aObject,aAction,aLink,aReference,Data.BuildLink(aRefObject),aIcon,aComission,CheckDouble,DoPost,DoChange)
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
      Result := AddItemPlain(Data.BuildLink(aObject),aAction,aLink,aReference,aRefObject,aIcon,aComission,CheckDouble,DoPost,DoChange);
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
      and (FieldByName('CHANGEDBY').AsString = Data.Users.Idcode.AsString)
      and ((FieldByName('REFERENCE').AsString = aReference) or (FieldByName('REFERENCE').AsString=Data.Users.IDCode.Asstring))
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
    FieldByName('CHANGEDBY').AsString := Data.Users.IDCode.AsString;
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
        FieldByName('OBJECT').AsString := Data.BuildLink(aObject);
      FieldByName('ACTIONICON').AsInteger := aIcon;
      FieldByName('ACTION').AsString    := aAction;
      FieldByName('REFERENCE').AsString := aReference;
      if Assigned(aRefObject) then
        begin
          with BaseApplication as IBaseDbInterface do
            FieldByName('REFOBJECT').AsString := Data.BuildLink(aRefObject);
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
  if aParent=0 then
    AddItem(aObject,aSubject+LineEnding+aMessage,aLink,'',nil,ACICON_MAILNEW)
  else
    AddParentedItem(aObject,aMessage,aParent,aLink,'',nil,ACICON_MAILNEW);
end;

procedure TBaseHistory.AddAnsweredMessageItem(aObject: TDataSet; aMessage,
  aSubject, aSource, aLink: string; aParent: LargeInt);
begin
  if aParent=0 then
    AddItem(aObject,aSubject+LineEnding+aMessage,aLink,'',nil,ACICON_MAILANSWERED)
  else
    AddParentedItem(aObject,aMessage,aParent,aLink,'',nil,ACICON_MAILANSWERED);
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

procedure TActiveUsers.Delete;
begin
  with DataSet as IBaseManageDB do
    UpdateStdFields := False;
  DataSet.Delete;
  with DataSet as IBaseManageDB do
    UpdateStdFields := True;
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
procedure Treports.DefineFields(aDataSet: TDataSet);
begin
  with aDataSet as IBaseManageDB do
    begin
      TableName := 'REPORTS';
      TableCaption:=strReports;
      if Assigned(ManagedFieldDefs) then
        with ManagedFieldDefs do
          begin
            Add('TYPE',ftString,4,True);
            Add('NAME',ftString,60,True);
            Add('CHANGEDBY',ftString,4,False);
            Add('LANGUAGE',ftString,3,True);
            Add('REPORT',ftBlob,0,False);
            Add('TEXT',ftMemo,0,False);
          end;
    end;
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
            Add('FILTER',ftString,150,True);
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
    end;
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
  with BaseApplication as IBaseDBInterface do
    begin
      if not Locate('OPTION',aIdent,[]) then
        Data.SetFilter(Self,'',0);
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
  with BaseApplication as IBaseDbInterface do
    begin
      with DataSet as IBaseDBFilter do
        begin
          Limit := 0;
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
  aUser : Int64;

  procedure RecursiveGetRight(aRec : Integer = 0);
  begin
    if aRec>10 then
      begin
        Result := -1;
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
                  Result := -1;
              end
            else
              Result := -1;
          end;
      end;
  end;
begin
  try
    Result := -1;
    if not Assigned(FCachedRights) then exit;
    if (FCachedRights.Values[Element] <> '') and UseCache then
      Result := StrToInt(FCachedRights.Values[Element])
    else
      begin
        with BaseApplication as IBaseDBInterface do
          begin
            aUser := UserTable.GetBookmark;
            with Self.DataSet as IBaseDBFilter do
              Filter := Data.QuoteField('RIGHTNAME')+'='+Data.QuoteValue(UpperCase(Element));
            Open;
            RecursiveGetRight;
            UserTable.GotoBookmark(aUser);
          end;
        FCachedRights.Values[Element] := IntToStr(Result);
      end;
  except
    Result := -1;
  end;
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
      Data.SetFilter(Self, Data.QuoteField('TABLENAME')+'='+Data.QuoteValue(numberset));
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
                end;
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
    Data.SetFilter(Self, Data.QuoteField('TABLENAME')+'='+Data.QuoteValue(numberset));
  Result := Count > 0;
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
  FOptions := TOptions.CreateEx(Owner,DM,aConnection,DataSet);
  FRights := TRights.CreateEx(Owner,DM,aConnection,DataSet);
  FFollows := TFollowers.CreateEx(Owner,DM,aConnection,DataSet);
  FRights.Users := Self;
  FHistory := TBaseHistory.CreateEx(Self,DM,aConnection,DataSet);
  FPayGroups := TPayGroups.CreateEx(Self,DM,aConnection);
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
  FPayGroups.Destroy;
  Options.Destroy;
  Rights.Destroy;
  FFollows.Destroy;
  FHistory.Destroy;
  inherited Destroy;
end;

procedure TUser.Open;
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
          end;
      if Assigned(ManagedIndexdefs) then
        with ManagedIndexDefs do
          Add('ACCOUNTNO','ACCOUNTNO',[ixUnique]);
    end;
end;
procedure TUser.FillDefaults(aDataSet: TDataSet);
begin
  with BaseApplication as IBaseDbInterface do
    aDataSet.FieldByName('ACCOUNTNO').AsString:=Data.Numbers.GetNewNumber('USERS');
end;

procedure TUser.SelectByParent(aParent: Variant);
begin
  with  DataSet as IBaseDBFilter, BaseApplication as IBaseDBInterface, DataSet as IBaseManageDB do
    begin
      if aParent=Null then
        Filter := Data.ProcessTerm('('+QuoteField('PARENT')+'='+Data.QuoteValue('')+')')
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

function TUser.CheckPasswort(aPasswort: string): Boolean;
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

function TUser.CheckSHA1Passwort(aPasswort : string): Boolean;
var
  aRes: String;
begin
  aRes := '$'+SHA1Print(SHA1String(aPasswort));
  Result := (copy(aRes,0,length(Passwort.AsString)) = Passwort.AsString) and (length(Passwort.AsString) > 30);
end;

procedure TUser.LoginWasOK;
begin
  with DataSet as IBaseManageDB do
  if (Count>0) and (FieldByName('LASTLOGIN') <> nil) then
    begin
      UpdateStdFields := False;
      DataSet.Edit;
      FieldByName('LASTLOGIN').AsDateTime:=Now();
      DataSet.Post;
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
        Filter := Data.QuoteField('ACCOUNTNO')+'='+Data.QuoteValue(aAccountno);
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

function TBaseDBDataset.GetLimit: Integer;
begin
  result := -1;
  if not Assigned(DataSet) then exit;
  with DataSet as IBaseDbFilter do
    Result := Limit;
end;

function TBaseDBDataset.GetSortDirection: TSortDirection;
begin
  if not Assigned(DataSet) then exit;
  with DataSet as IBaseDbFilter do
    Result := GetSortDirection;
end;

function TBaseDBDataset.GetSortFields: string;
begin
  if not Assigned(DataSet) then exit;
  with DataSet as IBaseDbFilter do
    Result := GetSortFields;
end;

function TBaseDBDataset.GetState: TDataSetState;
begin
  if Assigned(FDataSet) then
    Result := FDataSet.State;
end;

function TBaseDBDataset.GetTableName: string;
begin
  with FDataSet as IBaseManageDB do
    Result := GetTableName;
end;

function TBaseDBDataset.GetConnection: TComponent;
begin
  with FDataSet as IBaseManageDB do
    Result := GetConnection;
end;
function TBaseDBDataset.GetCaption: string;
begin
  with FDataSet as IBaseManageDB do
    Result := GetTableCaption;
end;
function TBaseDBDataset.GetCanEdit: Boolean;
begin
  Result := Assigned(Self) and (Self is TBaseDBDataSet) and Assigned(fdataSet) and (FDataSet.State = dsEdit) or (FDataSet.State = dsInsert);
end;

function TBaseDBDataset.GetActive: Boolean;
begin
  Result := False;
  if Assigned(FDataSet) then
    Result := FDataSet.Active;
end;

function TBaseDBDataset.GetCount: Integer;
begin
  if DataSet.Active then
    Result := DataSet.RecordCount
  else
    Result := -1;
end;

function TBaseDBDataset.GetFilter: string;
begin
  result := '';
  if not Assigned(DataSet) then exit;
  with DataSet as IBaseDbFilter do
    Result := Filter;
end;
function TBaseDBDataset.GetFRows: Integer;
begin
  result := -1;
  if not Assigned(DataSet) then exit;
  with DataSet as IBaseDbFilter do
    Result := FetchRows;
end;
function TBaseDBDataset.GetFullCount: Integer;
var
  aDS: TDataSet;
  aFilter: String;
begin
  if TBaseDBModule(DataModule).IsSQLDB then
    begin
      with FDataSet as IBaseManageDB,FDataSet as IBaseDbFilter do
        begin
          aFilter := Filter;
          if aFilter <> '' then
            aDS := TBaseDBModule(DataModule).GetNewDataSet('select count(*) from '+TBaseDBModule(DataModule).QuoteField(GetTableName)+' where '+Filter,Connection)
          else
            aDS := TBaseDBModule(DataModule).GetNewDataSet('select count(*) from '+TBaseDBModule(DataModule).QuoteField(GetTableName),Connection);
        end;
      aDS.Open;
      if aDS.RecordCount>0 then
        Result := aDS.Fields[0].AsInteger;
      aDS.Free;
    end
  else
    Result := Count;
end;
function TBaseDBDataset.GetTimestamp: TField;
begin
  Result := DataSet.FieldByName('TIMESTAMPD');
end;

procedure TBaseDBDataset.SetActive(AValue: Boolean);
begin
  if (not AValue) and Active then
    Close
  else if AValue and (not Active) then
    Open;
end;
procedure TBaseDBDataset.SetFilter(AValue: string);
begin
  with DataSet as IBaseDbFilter do
    Filter := AValue;
end;
procedure TBaseDBDataset.SetFRows(AValue: Integer);
begin
  with DataSet as IBaseDbFilter do
    FetchRows := aValue;
end;
procedure TBaseDBDataset.SetLimit(AValue: Integer);
begin
  with DataSet as IBaseDbFilter do
    Limit := aValue;
end;

procedure TBaseDBDataset.SetSortDirection(AValue: TSortDirection);
begin
  if not Assigned(DataSet) then exit;
  with DataSet as IBaseDbFilter do
    SetSortDirection(AValue);
end;

procedure TBaseDBDataset.SetSortFields(AValue: string);
begin
  if not Assigned(DataSet) then exit;
  with DataSet as IBaseDbFilter do
    SetSortFields(AValue);
end;

constructor TBaseDBDataset.CreateExIntegrity(aOwner: TComponent; DM: TComponent;
  aUseIntegrity: Boolean; aConnection: TComponent; aMasterdata: TDataSet);
begin
  inherited Create(aOwner);
  FUpdateFloatFields := false;
  Fparent := nil;
  FDataModule := DM;
  if FDataModule=nil then
    FDataModule:=Data;
  FSecModified := True;
  FDisplayLabelsWasSet:=False;
  FUseIntegrity:=aUseIntegrity;
  FOnChanged := nil;
  if Assigned(aOwner) and (aOwner is TBaseDBDataSet) then
    FParent := TBaseDBDataSet(aOwner);
  with BaseApplication as IBaseDbInterface do
    begin
      with FDataModule as TBaseDbModule do
        FDataSet := GetNewDataSet(Self,aConnection,aMasterdata);
      with FDataSet as IBaseManageDB do
        UseIntegrity := FUseIntegrity;
      with FDataSet as IBaseDBFilter do
        Limit := 100;
    end;
end;
constructor TBaseDBDataset.CreateEx(aOwner: TComponent; DM: TComponent;
  aConnection: TComponent; aMasterdata: TDataSet);
begin
  CreateExIntegrity(aOwner,DM,True,aConnection,aMasterdata);
end;

constructor TBaseDBDataset.Create(aOwner: TComponent);
begin
  CreateEx(aOwner,Data,nil,nil);
end;

destructor TBaseDBDataset.Destroy;
begin
  if not TBaseDBModule(DataModule).IgnoreOpenRequests then
    begin
      try
        TBaseDBModule(DataModule).DestroyDataSet(FDataSet);
      except
      end;
      inherited Destroy;
    end;
end;
procedure TBaseDBDataset.Open;
var
  Retry: Boolean = False;
  aCreated: Boolean = False;
  aOldFilter: String = '';
  aOldLimit: Integer;
  aErr: String;
begin
  if not Assigned(FDataSet) then exit;
  if FDataSet.Active then
    begin
      FDataSet.Refresh;
      exit;
    end;
  with FDataSet as IBaseManageDB do
    if (Assigned(Data)) and (Data.ShouldCheckTable(TableName,True)) then
      begin
        if not Self.CreateTable then
          begin
            with BaseApplication as IBaseApplication do
              Info('Table "'+TableName+'" will be altered');
            with DataSet as IBaseManageDB do
              FDataSet.Open;
            if AlterTable then
              FDataSet.Close
            else
              begin
                with BaseApplication as IBaseApplication do
                  Info('Table "'+TableName+'" altering failed');
              end;
          end;
      end;
  with DataSet as IBaseManageDB do
    FDataSet.Open;
  if DataSet.Active and FDisplayLabelsWasSet then
    SetDisplayLabels(DataSet);
end;

procedure TBaseDBDataset.Close;
begin
  if not Assigned(FDataSet) then exit;
  FDataSet.Close;
end;

function TBaseDBDataset.CreateTable: Boolean;
var
  aOldFilter: String;
  aOldLimit: Integer;
  aTableName: String;
  aField: String;
begin
  with FDataSet as IBaseManageDB do
    begin
      Result := CreateTable;
      if not Result then
        begin
          aTableName:=TableName;
          if (not Assigned(Data)) or (Data.ShouldCheckTable(aTableName,True)) then
            begin
              with DataSet as IBaseDbFilter do
                begin
                  aOldFilter := Filter;
                  with DataSet as IBaseManageDb do
                    if ManagedFieldDefs.IndexOf('AUTO_ID') > -1 then
                      aField := 'AUTO_ID';
                  if aField = '' then aField := 'SQL_ID';
                  Filter := TBaseDBModule(DataModule).QuoteField(TableName)+'.'+TBaseDBModule(DataModule).QuoteField(aField)+'='+TBaseDBModule(DataModule).QuoteValue('0');
                  aOldLimit := Limit;
                  Limit := 1;
                end;
              FDataSet.Open;
              with DataSet as IBaseDbFilter do
                begin
                  Limit := aOldLimit;
                  Filter := aOldFilter;
                end;
            end;
        end;
    end;
end;
procedure TBaseDBDataset.DefineDefaultFields(aDataSet: TDataSet;
  HasMasterSource: Boolean);
begin
  with aDataSet as IBaseManageDB do
    begin
      if Assigned(ManagedIndexdefs) then
        begin
          if (ManagedFieldDefs.IndexOf('REF_ID') > -1) or HasMasterSource then
            ManagedIndexDefs.Add('REF_ID','REF_ID',[]);
          ManagedIndexDefs.Add('TIMESTAMPD','TIMESTAMPD',[]);
        end;
    end;
end;
procedure TBaseDBDataset.DefineUserFields(aDataSet: TDataSet);
var
  UserFields: TUserfielddefs;
begin
  with BaseApplication as IBaseDbInterface do
    begin
      UserFields := GetDB.Userfielddefs;
      if Assigned(UserFields) then
        begin
          if not UserFields.DataSet.Active then
            begin
              UserFields.CreateTable;
              UserFields.Open;
            end;
          with aDataSet as IBaseManageDB do
            UserFields.DataSet.Filter := Data.QuoteField('TTABLE')+'='+Data.QuoteValue(GetTableName);
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
  SetDisplayLabelName(aDataSet,'SUMMARY',strSummary);
  SetDisplayLabelName(aDataSet,'OWNER',strOwner);
  SetDisplayLabelName(aDataSet,'AVALIBLE',strAvalible);
  SetDisplayLabelName(aDataSet,'ICON',' ');
  SetDisplayLabelName(aDataSet,'NEEDSACTION',strNeedsAction);
end;
function TBaseDBDataset.GetBookmark: LargeInt;
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

function TBaseDBDataset.GetLink: string;
begin
  Result := TBaseDBModule(DataModule).BuildLink(FDataSet);
end;

procedure TBaseDBDataset.FreeBookmark(aRec: Variant);
begin
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
procedure TBaseDBDataset.DisableChanges;
begin
  inc(FDoChange);
end;
procedure TBaseDBDataset.EnableChanges;
begin
  if FDoChange > 0 then
    dec(FDoChange);
end;
procedure TBaseDBDataset.Change;
begin
  if FDoChange > 0 then exit;
  if fChanged then exit;
  FChanged := True;
  if Owner is TBaseDBDataSet then TBaseDBDataSet(Owner).Change;
  if Assigned(FOnChanged) then
    FOnChanged(Self);
end;
procedure TBaseDBDataset.UnChange;
begin
  fChanged:=False;
end;
procedure TBaseDBDataset.CascadicPost;
begin
  if CanEdit then
    Post;
  FChanged := False;
  if Assigned(FOnChanged) then
    FOnChanged(Self);
end;
procedure TBaseDBDataset.CascadicCancel;
begin
  if (FDataSet.State = dsEdit) or (FDataSet.State = dsInsert) then
    FDataSet.Cancel;
  FChanged := False;
  if Assigned(FOnChanged) then
    FOnChanged(Self);
end;

initialization
end.

