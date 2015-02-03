{ The contents of this file are subject to the Mozilla Public License  }
{ Version 1.1 (the "License"); you may not use this file except in     }
{ compliance with the License. You may obtain a copy of the License at }
{ http://www.mozilla.org/MPL/                                          }
{                                                                      }
{ Software distributed under the License is distributed on an "AS IS"  }
{ basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See  }
{ the License for the specific language governing rights and           }
{ limitations under the License.                                       }
{                                                                      }
{ The Original Code is MapiServices.pas                                }
{                                                                      }
{ The Initial Developer of the Original Code is Ashley Godfrey, all    }
{ Portions created by these individuals are Copyright (C) of Ashley    }
{ Godfrey.                                                             }
{                                                                      }
{**********************************************************************}
{                                                                      }
{ This unit contains object oriented wrapper code for the Extended     }
{ MAPI headers as developed by Alexander Staubo and updated by Dmitry  }
{ Streblechenko. These classes make using Extended MAPI a little       }
{ easier than directly accessing the Extended MAPI system itself.      }
{                                                                      }
{ The original MAPI headers (required by this unit) can be downloaded  }
{ here:                                                                }
{ http://www.dimastr.com/outspy/download/MAPI_headers.zip              }
{                                                                      }
{ Unit owner: Ashley Godfrey.                                          }
{ Last modified: March 5, 2007.                                        }
{ Updates available from http://www.evocorp.com                        }
{                                                                      }
{**********************************************************************}

{$RANGECHECKS OFF}

{$IFDEF FPC}
{$MODE DELPHI}
{$ENDIF}

unit MapiServices;

interface
uses
  SysUtils, Windows, Classes, ComObj, Contnrs, MapiDefs, MapiTags, MapiX,
  MapiGuid, MapiUtil, MapiCode, ActiveX;

type
{$IFDEF VER130}
  // Delphi 5 conversion, thanks to Dan Cunningham
  IInterface = IUnknown;
{$ENDIF}

  EMapiError = class(Exception);

  // I've redefined IMapiSession to fix a couple issues with
  // interface ID method declarations.
  IMapiSession = interface(IUnknown)
  [strIID_IMapiSession]
    function GetLastError (hResult : HResult; ulFlags : ULONG;
      var lppMAPIError : PMAPIERROR) : HResult; stdcall;
    function GetMsgStoresTable (ulFlags : ULONG;
      out lppTable : IMAPITable) : HResult; stdcall;
    function OpenMsgStore (ulUIParam : ULONG; cbEntryID : ULONG;
      lpEntryID : PENTRYID; const lpInterface : TIID; ulFlags : ULONG;
      out lppMDB : IMsgStore) : HResult; stdcall;
    function OpenAddressBook (ulUIParam : ULONG; lpInterface : pIID;
      ulFlags : ULONG; out lppAdrBook : IAddrBook) : HResult; stdcall;
    function OpenProfileSection (lpUID : PMAPIUID; const lpInterface : TIID;
      ulFlags : ULONG; out lppProfSect : IProfSect) : HResult; stdcall;
    function GetStatusTable (ulFlags : ULONG;
      out lppTable : IMAPITable) : HResult; stdcall;
    function OpenEntry (cbEntryID : ULONG; lpEntryID : PENTRYID;
      const lpInterface : TIID; ulFlags : ULONG; var lpulObjType : ULONG;
      out lppUnk : IUnknown) : HResult; stdcall;
    function CompareEntryIDs (cbEntryID1 : ULONG; lpEntryID1 : PENTRYID;
      cbEntryID2 : ULONG; lpEntryID2 : PENTRYID; ulFlags : ULONG;
      var lpulResult : ULONG) : HResult; stdcall;
    function Advise (cbEntryID : ULONG; lpEntryID : PENTRYID;
      ulEventMask : ULONG; lpAdviseSink : IMAPIAdviseSink;
      var lpulConnection : ULONG) : HResult; stdcall;
    function Unadvise (ulConnection : ULONG) : HResult; stdcall;
    function MessageOptions (ulUIParam : ULONG; ulFlags : ULONG;
      lpszAdrType : PChar; lpMessage : MapiDefs.IMessage) : HResult; stdcall;
    function QueryDefaultMessageOpt (lpszAdrType : PChar; ulFlags : ULONG;
      var lpcValues : ULONG; var lppOptions : PSPropValue) : HResult; stdcall;
    function EnumAdrTypes (ulFlags : ULONG; var lpcAdrTypes : ULONG;
      var lpppszAdrTypes : PChar) : HResult; stdcall;
    function QueryIdentity (var lpcbEntryID : ULONG;
      var lppEntryID : PENTRYID) : HResult; stdcall;
    function Logoff (ulUIParam : ULONG; ulFlags : ULONG;
      ulReserved : ULONG) : HResult; stdcall;
    function SetDefaultStore (ulFlags : ULONG; cbEntryID : ULONG;
      lpEntryID : PENTRYID) : HResult; stdcall;
    function AdminServices (ulFlags : ULONG;
      out lppServiceAdmin : IMsgServiceAdmin) : HResult; stdcall;
    function ShowForm (ulUIParam : ULONG; lpMsgStore : IMsgStore;
      lpParentFolder : IMAPIFolder; const lpInterface : TIID;
      ulMessageToken : ULONG; lpMessageSent : MapiDefs.IMessage; ulFlags : ULONG;
      ulMessageStatus : ULONG; ulMessageFlags : ULONG; ulAccess : ULONG;
      lpszMessageClass : LPSTR) : HResult; stdcall;
    function PrepareForm (const lpInterface : TIID; lpMessage : MapiDefs.IMessage;
      var lpulMessageToken : ULONG) : HResult; stdcall;
  end;

  EMapiProperty = class(EMapiError)
  public
    constructor Create(const LeadInMessage: string;
      MapiPropAtFault: IMapiProp; ErrorResult: HRESULT); overload;
    constructor Create(const LeadInMessage: string;
      MapiSession: IMapiSession; ErrorResult: HRESULT); overload;
    constructor Create(const LeadInMessage: string;
      Problems: PSPropProblemArray); overload;
  end;

  TTagArray = array of ULONG;
  TSPropTagArrayEx = packed record
    aulPropTag: TTagArray;
    SPropTagArray: TSPropTagArray;
  end;
  pSPropTagArrayEx = ^TSPropTagArrayEx;

  // To differentiate between Outlook and MAPI objects (when
  // using both), we'll redefine MAPI types with the "MAPI" suffix.
  IMapiAddressBook = MapiX.IAddrBook;
  IMapiAttachment = MapiDefs.IAttach;
  IMapiMessage = MapiDefs.IMessage;

  TMapiConnection = class
  private
    FAddressBook: IMapiAddressBook;
    FProfileName: string;
    FSession: IMapiSession;
    FSharedSession: Boolean;
  protected
    procedure MapiServicesInitialise; virtual;
    procedure MapiServicesLogOn; virtual;
    procedure MapiServicesLogOff; virtual;
    procedure MapiServicesUninitialise; virtual;

    property AddressBook: IMapiAddressBook read FAddressBook write FAddressBook;
    property Session: IMapiSession read FSession write FSession;
  public
    constructor Create(const AProfileName: string;
      const SharedSession: Boolean = False); virtual;
    destructor Destroy; override;

    property ProfileName: string read FProfileName;
    // SharedSession identifies whether or not the active MAPI session
    // is to be disconnected when this object closes. Set this to true
    // if you are binding to an active instance of Microsoft Outlook
    // when obtaining your MAPI information (for example, from within
    // an Outlook add-in).
    property SharedSession: Boolean read FSharedSession;
  end;

  TAddressEntryFormat = (aefDisplayName, aefEntryId);

  TMapiMailAddress = class
  private
    FDisplayName: string;
    FEntryId: TSBINARY;
  protected
    function GetEntryIdAsString: string; virtual;
  public
    constructor Create(const DisplayName, EntryID: string); overload; virtual;
    constructor Create(const DisplayName: string;
      const EntryID: TSBINARY); overload; virtual;
    destructor Destroy; override;

    property DisplayName: string read FDisplayName;
    property EntryID: TSBINARY read FEntryId;
    property EntryIDAsString: string read GetEntryIdAsString;
  end;

  TMapiMailAddresses = class
  private
    FItems: TList;
  protected
    function GetCount: Integer; virtual;
    function GetItem(Index: Integer): TMapiMailAddress; virtual;

    property _Items: TList read FItems write FItems;
  public
    procedure Add(const AddressItem: TMapiMailAddress); overload; virtual;
    function Add(const DisplayName, EntryID: string): TMapiMailAddress; overload; virtual;
    function Add(const DisplayName: string;
      const EntryID: TSBINARY): TMapiMailAddress; overload; virtual;
    procedure Assign(const SourceAddresses: TMapiMailAddresses); virtual; 
    procedure Clear; virtual;
    constructor Create; virtual;
    procedure Delete(Index: Integer); virtual;
    destructor Destroy; override;
    function IndexOfDisplayName(const DisplayName: string): Integer; virtual;
    function IndexOfEntryID(const EntryID: string): Integer; virtual;

    property Count: Integer read GetCount;
    property Items[Index: Integer]: TMapiMailAddress read GetItem; default;
  end;

  TMapiAddressBook = class(TMapiConnection)
  private
    FButtonCaption: string;
    FDialogTitle: string;
    FInputCaption: string;
    FRecipientType: Integer;
  protected
    procedure AddAddressToList(const Address: TMapiMailAddress;
      var AddressList: PADRLIST; AddressListIndex: Integer); virtual;
    function GetAddress(var AddressList: PADRLIST;
      AddressListIndex: Integer): TMapiMailAddress; virtual;
    function GetDisplayName(var AddressList: PADRLIST;
      AddressListIndex: Integer): string; virtual;
    function GetEntryID(var AddressList: PADRLIST;
      AddressListIndex: Integer): TSBINARY; virtual;
    procedure ReleaseAddressList(var AddressList: PADRLIST); virtual;
  public
    constructor Create(const AProfileName: string;
      const SharedSession: Boolean = False); override;
    // Addresses represent both the incoming (for display of addresses
    // already selected) and outgoing (selected by the user, and user
    // selects "OK") addresses. 
    function Execute(Addresses: TMapiMailAddresses;
      AllowMultipleSelection: Boolean): Boolean;

    property AddressBook;
    property ButtonCaption: string read FButtonCaption write FButtonCaption;
    property InputCaption: string read FInputCaption write FInputCaption;
    property RecipientType: Integer read FRecipientType write FRecipientType;
    property Session;
    property Title: string read FDialogTitle write FDialogTitle;
  end;

  TMapiMailAttachment = class
  private
    FAttachment: IMapiAttachment;

    procedure BindToStream(const SourceStream: TStream);
    function GetFileName: string;
  public
    function Copy(TargetStream: TStream): Integer;
    constructor Create(const AAttachment: IMapiAttachment); overload;
    constructor Create(const SourceMessage: IMapiMessage;
      const StreamName: string; const SourceStream: TStream; Hidden : Boolean = False); overload;
    destructor Destroy; override;

    property FileName: string read GetFileName;
    property CoAttatchemnt : IMapiAttachment read FAttachment;
  end;

  TMapiMailAttachments = class
  private
    FAttachments: IMapiTable;
    FItems: TObjectList;
    FMessage: IMapiMessage;

    function GetCount: Cardinal;
    function GetItem(Index: Integer): TMapiMailAttachment;
    procedure ReadAttachments;
  public
    function Add(const StreamName: string;
      const SourceStream: TStream;Hidden : Boolean = False): TMapiMailAttachment;
    constructor Create(const AMessage: IMapiMessage);
    destructor Destroy; override;

    property Count: Cardinal read GetCount;
    property Items[Index: Integer]: TMapiMailAttachment read GetItem; default;
  end;

  TMapiPropertyType = (ptBoolean, ptInteger, ptString, ptTime);
  TMapiRecipientRole = (mrrBcc, mrrCc, mrrTo);
  TCustomMapiFolder = class;

  { TMapiMailItem }

  TMapiMailItem = class
  private
    FAttachments: TMapiMailAttachments;
    FDeleted: Boolean;
    FEntryID: TSBinary;
    FFolder: TCustomMapiFolder;
    FReadOnly: Boolean;
    FMessage: IMapiMessage;

    procedure AddRecipients(var AddressList: PADRLIST;
      ShowResolveDialog: Boolean);
    function GetBody: string;
    function GetCreateTime: TDateTime;
    function GetImportance: Integer;
    function GetMessageClass: string;
    function GetPropertyDirect(const aPropType: LONG; PropType: TMapiPropertyType
      ): Variant;
    function GetProperty(const PropName: string;
      PropType: TMapiPropertyType): Variant;
    function GetReadReceiptRequested: Boolean;
    function GetSentTime: TDateTime;
    function GetLastModificationTime : TDateTime;
    function GetSender: string;
    function GetSubject: string;
    procedure ReleaseRecipients(AddressList: PADRLIST);
    procedure SetBody(const Value: string);
    procedure SetImportance(const Value: Integer);
    procedure SetLastModificationTime(const AValue: TDateTime);
    procedure SetMessageClass(const Value: string);
    procedure SetProperty(const PropName: string;
      PropType: TMapiPropertyType; Value: Variant);
    procedure SetPropertyDirect(const aPropType: LONG;
      PropType: TMapiPropertyType; const AValue: Variant);
    procedure SetReadReceiptRequested(const Value: Boolean);
    procedure SetRecipient(RecipientType: TMapiRecipientRole;
      const Recipient: string; AddressEntry: PADRENTRY); overload;
    procedure SetRecipient(RecipientType: TMapiRecipientRole;
      const DisplayName, EntryID: string; AddressEntry: PADRENTRY); overload;
    procedure SetSender(const Value: string);
    procedure SetSubject(const Value: string);
  public
    function GetPropertyId(const PropertyName: WideString;
      ulPropType: Cardinal; CreateProperty: Boolean;aGUID : PGUID): Cardinal;
    function GetPropertyDispId(const PropertyId: LongInt;
      ulPropType: Cardinal; CreateProperty: Boolean;aGUID : PGUID): Cardinal;
    procedure AddRecipient(RecipientType: TMapiRecipientRole;
      const Recipient: string; ShowResolveDialog: Boolean); overload;
    procedure AddRecipient(RecipientType: TMapiRecipientRole;
      const DisplayName, EntryID: string; ShowResolveDialog: Boolean); overload;
    constructor Create(const Folder: TCustomMapiFolder;
      const MapiMessage: IMapiMessage; const IsReadOnly: Boolean);
    procedure Delete; virtual;
    destructor Destroy; override;
    function HasProperty(const PropName: string;aGUID : PGUID): Boolean;
    procedure SetRecipients(RecipientType: TMapiRecipientRole;
      const Recipients: TStrings; ShowResolveDialog: Boolean);
    function Submit(const DeleteOnSubmit: Boolean): Boolean;

    property Attachments: TMapiMailAttachments read FAttachments;
    property Body: string read GetBody write SetBody;
    property CoMessage: IMapiMessage read FMessage;
    property CreateTime: TDateTime read GetCreateTime;
    property EntryID: TSBinary read FEntryID;
    property Importance: Integer read GetImportance write SetImportance;
    property MessageClass: string read GetMessageClass write SetMessageClass;
    property Properties[const PropName: string;PropType: TMapiPropertyType]: Variant read GetProperty write SetProperty;
    property PropertiesDirect[const PropType: LONG;aPropType: TMapiPropertyType]: Variant read GetPropertyDirect write SetPropertyDirect;
    property ReadReceiptRequested: Boolean read GetReadReceiptRequested write SetReadReceiptRequested;
    property SentTime: TDateTime read GetSentTime;
    property LastModificationTime: TDateTime read GetlastModificationTime write SetLastModificationTime;
    property Sender: string read GetSender write SetSender;
    property Subject: string read GetSubject write SetSubject;
  end;

  TCustomMapiFolder = class
  private
    FConnection: TMapiConnection;
    FContentItems: PSRowSet;
    FContentsTable: IMapiTable;
    FItemIndex: Integer;

    function GetSession: IMapiSession;
  protected
    FFolder: IMapiFolder;
    procedure ClearContentItems; virtual;
    procedure ConnectToMailbox; virtual; abstract;
    function GetMailItem(Index: Cardinal): TMapiMailItem; virtual;
    procedure ConnectToContents; virtual;
    procedure PopulateContentItems; virtual;

    property ContentItems: PSRowSet read FContentItems write FContentItems;
    property Contents: IMapiTable read FContentsTable write FContentsTable;
    property Folder: IMapiFolder read FFolder write FFolder;
  public
    property Session: IMapiSession read GetSession;
    constructor Create(const Connection: TMapiConnection); virtual;
    procedure DeleteItem(const MailItem: TMapiMailItem); virtual;
    destructor Destroy; override;
    function GetFirst: TMapiMailItem; virtual;
    function GetNext: TMapiMailItem; virtual;
  end;
  TCustomMapiFolderClass = class of TCustomMapiFolder;

  TInbox = class(TCustomMapiFolder)
  protected
    procedure ConnectToMailbox; override;
    function GetDefaultStoreEID(MessageStoreRows: PSRowSet;
      var DefaultStoreEID: TSBinary): Boolean; virtual;
    function OpenDefaultStoreEID(
      const DefaultStoreEID: TSBinary): Boolean; virtual;
  public
    property Folder;
  end;

  TOutbox = class(TInBox)
  private
    FSentItems: IMapiFolder;
  protected
    function GetOutbox: IMapiFolder;
    function OpenDefaultStoreEID(
      const DefaultStoreEID: TSBinary): Boolean; override;
  public
    destructor Destroy; override;
    function NewMailItem: TMapiMailItem;
    property Outbox: IMapiFolder read GetOutbox;
    property SentItems: IMapiFolder read FSentItems;
  end;

// General Mapi Functions
function CloneEntryId(const EntryId: TSBINARY): TSBINARY;   // Caller must release buffer
procedure CopyMapiString(const InputString: string;
  var OutputStringLocation: pointer);
function EntryIdToString(const EntryId: TSBINARY): string;
function StringToEntryId(const EntryId: string): TSBINARY;  // Caller must release buffer

// MAPI property access
function ClearMapiProperty(
  const MapiProp: IMapiProp; PropertyId: ULONG): Boolean;
function GetMapiBinaryProperty(
  const MapiProp: IMapiProp; PropertyId: ULONG): TSBINARY;
function GetMapiBooleanProperty(
  const MapiProp: IMapiProp; PropertyId: ULONG): Boolean;
function GetMapiIntProperty(
  const MapiProp: IMapiProp; PropertyId: ULONG): Integer;
function GetMapiStringProperty(
  const MapiProp: IMapiProp; PropertyId: ULONG): string;
function GetMapiTimeProperty(
  const MapiProp: IMapiProp; PropertyId: ULONG): TDateTime;
procedure SetMapiBinaryProperty(const MapiProp: IMapiProp;
  PropertyId: ULONG; const Value: TSBinary);
procedure SetMapiBooleanProperty(const MapiProp: IMapiProp;
  PropertyId: ULONG; const Value: Boolean);
procedure SetMapiIntProperty(const MapiProp: IMapiProp;
  PropertyId: ULONG; const Value: Integer);
procedure SetMapiStringProperty(const MapiProp: IMapiProp;
  PropertyId: ULONG; const Value: string);
procedure SetMapiTimeProperty(const MapiProp: IMapiProp;
  PropertyId: ULONG; const Value: TDateTime);
function PROP_ID(ulPropTag: ULONG): ULONG;
function PROP_TAG(ulPropType, ulPropID: ULONG): ULONG;
function PROP_TYPE(ulPropTag: ULONG): ULONG;
function SizedSPropTagArray(cValues: ULONG): pSPropTagArrayEx;

const
  PROP_INDEX_ENTRYID = 0;
  PROP_INDEX_ATTACH_NUM = 0;
  PROP_INDEX_DEFAULTSTORE = 1;

  // Mapi Specific
  sExchangeDest = 'EX';
  sExchangeSmtpDest = 'SMTP';
  sMapiIpmNote = 'IPM.NOTE';
  sMapiQuotedIpmNote = '"' + sMapiIpmNote + '"';

  // Other
  sSecureMailExternAddrIdentifier = '@';

implementation

//uses
//  AxCtrls;

resourcestring
  sMapiInitFailed =
    'An unknown error occurred initializing the extended MAPI subsystem.';
  sMapiItemDeleted =
    'The properties of this mail item cannot be retrieved as this item ' +
    'has been deleted.';
  sMapiItemReadOnly =
    'An attempt has been made to update the properties of a mail item ' +
    'which has been marked as "read only".';
  sMapiItemReadOnlySend =
    'An attempt has been made to send a mail item which has been marked ' +
    'as "read only" and therefore is not allowed to be resent.';
  sMapiLogonFailed =
    'An unknown error (0x%.8x) occurred logging into the extended MAPI ' +
    'subsystem via the Microsoft Exchange server for "%s".';
  sMapiMailboxContentsNotFound =
    'A connection was successfully made to the MAPI subsystem, however ' +
    'an unknown error occurred connecting to the table of contents ' +
    'associated with this mailbox.';
  sMapiMailboxNotFound =
    'A connection was successfully made to the MAPI subsystem, however ' +
    'an unknown error occurred locating one of the mailboxes for profile "%s".';
  sMapiNamedRecipientResolutionFail =
    'Name resolution for one or more of the following recipients failed:' +
    #13#10' '#13#10'"%s"'#13#10' '#13#10'"%s"';
  sMapiEntryIDResolutionFail =
    'An internal entry ID resolution error has occurred for the following recipient:' +
    #13#10' '#13#10'"%s"'#13#10' '#13#10'"%s"';
  sMapiPropertyFailure =
    'The requested message property "%s" could not be located in the ' +
    'associated message item.';
  sMapiRecipientResolutionFail =
    'An error occurred attempting to resolve one or more of the recipients ' +
    'associated with this mail item.';
  sMapiRecipientsTableFail =
    'An unknown error occurred retrieving the recipients table for this ' +
    'mail item. No TO, CC or BCC destinations could be added to this item.';
  sMapiAttachAccessError =
    'One or more of the attachments associated with the current ' +
    'Exchange mail item could not be accessed.';
  sAddressBookConnectionFailure =
    'An unexpected error occurred accessing the global address list.';

  GetMapiPropertyError = 'Unable to get property %.8x';

function CloneEntryId(const EntryId: TSBINARY): TSBINARY;
begin
  Result.cb := EntryId.cb;
  MAPIAllocateBuffer(Result.cb, pointer(Result.lpb));
  Move(EntryId.lpb^, Result.lpb^, Result.cb);
end;

procedure CopyMapiString(const InputString: string;
  var OutputStringLocation: pointer);
var InputStringLen: Integer;
begin
  InputStringLen := Length(InputString);
  MAPIAllocateBuffer(InputStringLen + 1, OutputStringLocation);
  FillChar(OutputStringLocation^, InputStringLen + 1, 0);
  StrLCopy(OutputStringLocation, pChar(InputString), InputStringLen);
end;

function EntryIdToString(const EntryId: TSBINARY): string;
var i: Integer;
    CurByte: pByte;
begin
  // Converts a MAPI style entry ID to an OLE compatible entry ID.
  Result := '';
  if EntryId.cb > 0 then
  begin
    CurByte := EntryId.lpb;
    for i := 0 to EntryId.cb - 1 do
    begin
      Result := Result + IntToHex(CurByte^, 2);
      Inc(CurByte);
    end;
  end;
end;

procedure SetPropertyTagValue(
  var Properties: TSPropTagArray; Index, Value: Integer);
begin
  Properties.aulPropTag[Index] := Value;
end;

function SizedSPropTagArray(cValues: ULONG): pSPropTagArrayEx;
begin
  Result := AllocMem(SizeOf(TSPropTagArrayEx) - SizeOf(ULONG) + (cValues * SizeOf(ULONG)));
  Result^.SPropTagArray.cValues := cValues;
  pointer(Result^.aulPropTag) := @Result^.SPropTagArray.aulPropTag;
end;

function StringToEntryId(const EntryId: string): TSBINARY;
type TChar2Array = array [0..1] of Char;
     pChar2Array = ^TChar2Array;
var i: Integer;
    CurByte: pByte;
    CurChars: pChar2Array;
begin
  Result.cb := Length(EntryId) div 2;
  MAPIAllocateBuffer(Result.cb, pointer(Result.lpb));
  FillChar(Result.lpb^, Result.cb, 0);
  CurByte := Result.lpb;
  if Result.cb > 0 then
  begin
    CurChars := @EntryId[1];
    for i := 0 to Result.cb - 1 do
    begin
      CurByte^ := StrToIntDef('$' + CurChars^, 0);
      Inc(CurByte);
      Inc(CurChars);
    end;
  end;
end;

function PROP_ID(ulPropTag: ULONG): ULONG;
begin
  Result := ulPropTag shr 16;
end;

function PROP_TAG(ulPropType, ulPropID: ULONG): ULONG;
begin
  Result := (ulPropID shl 16) or ulPropType;
end;

function PROP_TYPE(ulPropTag: ULONG): ULONG;
begin
  Result := ulPropTag and $FFFF;
end;

function ClearMapiProperty(const MapiProp: IMapiProp; PropertyId: ULONG
  ): Boolean;
var
    Problems: PSPropProblemArray;
    SetPropResult: HRESULT;
var Properties: TSPropTagArray;
begin
  Problems := nil;

  // Get the actual property value
  Properties.cValues := 1;
  Properties.aulPropTag[0] := PropertyId;

  // Update said property
  SetPropResult := MapiProp.DeleteProps(@Properties, Problems);
  try
    // Check for errors
    if SetPropResult <> S_OK then
      raise EMapiProperty.Create(
        Format('error deleting property %d', [PropertyId]), MapiProp, SetPropResult)
    else if Assigned(Problems) then
      raise EMapiProperty.Create(
        Format('error deleting property %d', [PropertyId]), Problems);
  finally
    if Assigned(Problems) then
      MapiFreeBuffer(Problems);
  end;
end;

function GetMapiBinaryProperty(
  const MapiProp: IMapiProp; PropertyId: ULONG): TSBINARY;
var Properties: TSPropTagArray;
    GetPropResult: HRESULT;
    lpcValues: Cardinal;
    lppPropArray: PSPropValue;
begin
  // Default output value
  FillChar(Result, SizeOf(TSBINARY), 0);
  lppPropArray := nil;

  // Get the actual property value
  Properties.cValues := 1;
  Properties.aulPropTag[0] := PropertyId;
  GetPropResult := MapiProp.GetProps(@Properties, 0, lpcValues, lppPropArray);

  // MAPI_W_ERRORS_RETURNED
  // The call succeeded overall, but one or more properties could not
  // be accessed. The ulPropTag member of the property value for each
  // unaccessible property has a property type of PT_ERROR and an
  // identifier of zero. When this warning is returned, the call should
  // be handled as successful.
  try
    if GetPropResult = S_OK then
    begin
      if Assigned(lppPropArray) then
        Result := lppPropArray^.Value.bin
    end else if GetPropResult <> MAPI_W_ERRORS_RETURNED then
      raise EMapiProperty.Create(Format(GetMapiPropertyError, [PropertyID]));
  finally
    if Assigned(lppPropArray) then
      MapiFreeBuffer(lppPropArray);
  end;
end;

function GetMapiBooleanProperty(
  const MapiProp: IMapiProp; PropertyId: ULONG): Boolean;
var Properties: TSPropTagArray;
    GetPropResult: HRESULT;
    lpcValues: Cardinal;
    lppPropArray: PSPropValue;
begin
  // Default output value
  Result := False;
  lppPropArray := nil;

  // Get the actual property value
  Properties.cValues := 1;
  Properties.aulPropTag[0] := PropertyId;
  GetPropResult := MapiProp.GetProps(@Properties, 0, lpcValues, lppPropArray);

  // MAPI_W_ERRORS_RETURNED
  // The call succeeded overall, but one or more properties could not
  // be accessed. The ulPropTag member of the property value for each
  // unaccessible property has a property type of PT_ERROR and an
  // identifier of zero. When this warning is returned, the call should
  // be handled as successful.
  try
    if GetPropResult = S_OK then
    begin
      if Assigned(lppPropArray) then
        Result := Boolean(lppPropArray^.Value.i);
    end else if GetPropResult <> MAPI_W_ERRORS_RETURNED then
      raise EMapiProperty.Create(Format(GetMapiPropertyError, [PropertyID]));
  finally
    if Assigned(lppPropArray) then
      MapiFreeBuffer(lppPropArray);
  end;
end;

function GetMapiIntProperty(const MapiProp: IMapiProp;
  PropertyId: ULONG): Integer;
var Properties: TSPropTagArray;
    GetPropResult: HRESULT;
    lpcValues: Cardinal;
    lppPropArray: PSPropValue;
begin
  // Default output value
  Result := -1;
  lppPropArray := nil;

  // Get the actual property value
  Properties.cValues := 1;
  Properties.aulPropTag[0] := PropertyId;
  GetPropResult := MapiProp.GetProps(@Properties, 0, lpcValues, lppPropArray);

  // MAPI_W_ERRORS_RETURNED
  // The call succeeded overall, but one or more properties could not
  // be accessed. The ulPropTag member of the property value for each
  // unaccessible property has a property type of PT_ERROR and an
  // identifier of zero. When this warning is returned, the call should
  // be handled as successful.
  try
    if GetPropResult = S_OK then
    begin
      if Assigned(lppPropArray) then
        Result := lppPropArray^.Value.l
    end else if GetPropResult <> MAPI_W_ERRORS_RETURNED then
      raise EMapiProperty.Create(Format(GetMapiPropertyError, [PropertyID]));
  finally
    if Assigned(lppPropArray) then
      MapiFreeBuffer(lppPropArray);
  end;
end;

function GetMapiStringProperty(
  const MapiProp: IMapiProp; PropertyId: ULONG): string;
var Properties: TSPropTagArray;
    GetPropResult: HRESULT;
    lpcValues: Cardinal;
    lppPropArray: PSPropValue;
begin
  // Default output value
  Result := '';
  lppPropArray := nil;

  // Get the actual property value
  Properties.cValues := 1;
  Properties.aulPropTag[0] := PropertyId;
  GetPropResult := MapiProp.GetProps(@Properties, 0, lpcValues, lppPropArray);

  // MAPI_W_ERRORS_RETURNED
  // The call succeeded overall, but one or more properties could not
  // be accessed. The ulPropTag member of the property value for each
  // unaccessible property has a property type of PT_ERROR and an
  // identifier of zero. When this warning is returned, the call should
  // be handled as successful.
  try
    if GetPropResult = S_OK then
    begin
      if Assigned(lppPropArray) then
        Result := lppPropArray^.Value.lpszA;
    end else if GetPropResult <> MAPI_W_ERRORS_RETURNED then
      raise EMapiProperty.Create(Format(GetMapiPropertyError, [PropertyID]));
  finally
    if Assigned(lppPropArray) then
      MapiFreeBuffer(lppPropArray);
  end;
end;

function GetMapiTimeProperty(
  const MapiProp: IMapiProp; PropertyId: ULONG): TDateTime;
var Properties: TSPropTagArray;
    GetPropResult: HRESULT;
    SysTime: TSystemTime;
    lpcValues: Cardinal;
    lppPropArray: PSPropValue;
begin
  // Default output value
  Result := -1;
  lppPropArray := nil;

  // Get the actual property value
  Properties.cValues := 1;
  Properties.aulPropTag[0] := PropertyId;
  GetPropResult := MapiProp.GetProps(@Properties, 0, lpcValues, lppPropArray);

  // MAPI_W_ERRORS_RETURNED
  // The call succeeded overall, but one or more properties could not
  // be accessed. The ulPropTag member of the property value for each
  // unaccessible property has a property type of PT_ERROR and an
  // identifier of zero. When this warning is returned, the call should
  // be handled as successful.
  try
    if GetPropResult = S_OK then
    begin
      if Assigned(lppPropArray) then
      begin
        FileTimeToSystemTime(Windows.TFileTime(lppPropArray^.Value.ft), SysTime);
        Result := SystemTimeToDateTime(SysTime);
      end;
    end else if GetPropResult <> MAPI_W_ERRORS_RETURNED then
      raise EMapiProperty.Create(Format(GetMapiPropertyError, [PropertyID]));
  finally
    if Assigned(lppPropArray) then
      MapiFreeBuffer(lppPropArray);
  end;
end;

procedure SetMapiBinaryProperty(const MapiProp: IMapiProp;
  PropertyId: ULONG; const Value: TSBinary);
const SetBinaryError = 'Unable to set "binary" property %.8x';
var NewProperty: TSPropValue;
    Problems: PSPropProblemArray;
    SetPropResult: HRESULT;
begin
  Problems := nil;

  // Initialise and set the details of the property we want to modify
  FillChar(NewProperty, SizeOf(TSPropValue), 0);
  NewProperty.ulPropTag := PropertyId;
  NewProperty.Value.bin := Value;

  // Update said property
  SetPropResult := MapiProp.SetProps(1, @NewProperty, Problems);
  try
    // Check for errors
    if SetPropResult <> S_OK then
      raise EMapiProperty.Create(
        Format(SetBinaryError, [PropertyId]), MapiProp, SetPropResult)
    else if Assigned(Problems) then
      raise EMapiProperty.Create(
        Format(SetBinaryError, [PropertyId]), Problems);
  finally
    if Assigned(Problems) then
      MapiFreeBuffer(Problems);
  end;
end;

procedure SetMapiBooleanProperty(const MapiProp: IMapiProp;
  PropertyId: ULONG; const Value: Boolean);
const SetBoolError = 'Unable to set "boolean" property %.8x';
var NewProperty: TSPropValue;
    Problems: PSPropProblemArray;
    SetPropResult: HRESULT;
begin
  Problems := nil;

  // Initialise and set the details of the property we want to modify
  FillChar(NewProperty, SizeOf(TSPropValue), 0);
  NewProperty.ulPropTag := PropertyId;
  NewProperty.Value.b := SmallInt(Value);

  // Update said property
  SetPropResult := MapiProp.SetProps(1, @NewProperty, Problems);
  try
    // Check for errors
    if SetPropResult <> S_OK then
      raise EMapiProperty.Create(
        Format(SetBoolError, [PropertyId]), MapiProp, SetPropResult)
    else if Assigned(Problems) then
      raise EMapiProperty.Create(
        Format(SetBoolError, [PropertyId]), Problems);
  finally
    if Assigned(Problems) then
      MapiFreeBuffer(Problems);
  end;
end;

procedure SetMapiIntProperty(const MapiProp: IMapiProp;
  PropertyId: ULONG; const Value: Integer);
const SetIntError = 'Unable to set property %.8x to %d [0x%.8x]';
var NewProperty: TSPropValue;
    Problems: PSPropProblemArray;
    SetPropResult: HRESULT;
begin
  Problems := nil;

  // Initialise and set the details of the property we want to modify
  FillChar(NewProperty, SizeOf(TSPropValue), 0);
  NewProperty.ulPropTag := PropertyId;
  NewProperty.Value.l := Value;

  // Update said property
  SetPropResult := MapiProp.SetProps(1, @NewProperty, Problems);
  try
    // Check for errors
    if SetPropResult <> S_OK then
      raise EMapiProperty.Create(
        Format(SetIntError, [PropertyId, Value, Value]), MapiProp, SetPropResult)
    else if Assigned(Problems) then
      raise EMapiProperty.Create(
        Format(SetIntError, [PropertyId, Value, Value]), Problems);
  finally
    if Assigned(Problems) then
      MapiFreeBuffer(Problems);
  end;
end;

procedure SetMapiStringProperty(const MapiProp: IMapiProp;
  PropertyId: ULONG; const Value: string);
const SetStringError = 'Unable to set property %.8x to %s';
var NewProperty: TSPropValue;
    Problems: PSPropProblemArray;
    SetPropResult: HRESULT;
begin
  Problems := nil;

  // Initialise and set the details of the property we want to modify
  FillChar(NewProperty, SizeOf(TSPropValue), 0);
  NewProperty.ulPropTag := PropertyId;
  NewProperty.Value.lpszA := pChar(Value);

  // Update said property
  SetPropResult := MapiProp.SetProps(1, @NewProperty, Problems);
  try
    // Check for errors
    if SetPropResult <> S_OK then
      raise EMapiProperty.Create(
        Format(SetStringError, [PropertyId, Value]), MapiProp, SetPropResult)
    else if Assigned(Problems) then
      raise EMapiProperty.Create(
        Format(SetStringError, [PropertyId, Value]), Problems);
  finally
    if Assigned(Problems) then
      MapiFreeBuffer(Problems);
  end;
end;

procedure SetMapiTimeProperty(const MapiProp: IMapiProp;
  PropertyId: ULONG; const Value: TDateTime);
const SetTimeError = 'Unable to set "time" property %.8x';
var NewProperty: TSPropValue;
    Problems: PSPropProblemArray;
    SetPropResult: HRESULT;
    FileTime: TFileTime;
    SystemTime: TSystemTime;
begin
  Problems := nil;

  // Initialise and set the details of the property we want to modify
  FillChar(NewProperty, SizeOf(TSPropValue), 0);
  NewProperty.ulPropTag := PropertyId;
  DateTimeToSystemTime(Value, SystemTime);
  SystemTimeToFileTime(SystemTime, _FileTime(FileTime));
  NewProperty.Value.ft := FileTime;

  // Update said property
  SetPropResult := MapiProp.SetProps(1, @NewProperty, Problems);
  try
    // Check for errors
    if SetPropResult <> S_OK then
      raise EMapiProperty.Create(
        Format(SetTimeError, [PropertyId]), MapiProp, SetPropResult)
    else if Assigned(Problems) then
      raise EMapiProperty.Create(
        Format(SetTimeError, [PropertyId]), Problems);
  finally
    if Assigned(Problems) then
      MapiFreeBuffer(Problems);
  end;
end;

{ EMapiProperty }

constructor EMapiProperty.Create(const LeadInMessage: string;
  MapiPropAtFault: IMapiProp; ErrorResult: HRESULT);
var ErrorInfo: PMAPIERROR;
    ErrorMessage: string;
begin
  ErrorMessage := 'MAPI Error:'#13#10#13#10'"' + LeadInMessage + '"'#13#10;
  if MapiPropAtFault = nil then
    ErrorMessage := ErrorMessage + 'No MAPI property available for interrogation.'
  else begin
    ErrorInfo := nil;
    try
      MapiPropAtFault.GetLastError(ErrorResult, 0, ErrorInfo);
      if not Assigned(ErrorInfo) then
        ErrorMessage := ErrorMessage + 'No MAPI error information available.'
      else begin
        if ErrorInfo^.lpszComponent <> nil then
          ErrorMessage := ErrorMessage + 'Component "' +
            ErrorInfo^.lpszComponent + '" failed.'#13#10;
        if ErrorInfo^.lpszError <> nil then
          ErrorMessage := ErrorMessage + ErrorInfo^.lpszError;
      end;
    finally
      if Assigned(ErrorInfo) then
        MapiFreeBuffer(ErrorInfo);
    end;
  end;

  inherited Create(ErrorMessage);
end;

constructor EMapiProperty.Create(const LeadInMessage: string;
  Problems: PSPropProblemArray);
var ErrorMessage: string;
    i: Integer;
begin
  ErrorMessage := 'MAPI Error:'#13#10#13#10'"' + LeadInMessage + '"'#13#10;
  if (Problems = nil) or (Problems^.cProblem <= 0) then
    ErrorMessage := ErrorMessage + 'No error information available for interrogation.'
  else begin
    for i := 0 to Problems^.cProblem - 1 do
      ErrorMessage := ErrorMessage +
        'Error 0x' + IntToHex(Problems^.aProblem[i].scode, 8) + ' ' +
        '(' + IntToStr(Problems^.aProblem[i].scode) + ') for property 0x' +
        IntToHex(Problems^.aProblem[i].ulPropTag, 8) + #13#10;
  end;

  inherited Create(ErrorMessage);
end;

constructor EMapiProperty.Create(const LeadInMessage: string;
  MapiSession: IMapiSession; ErrorResult: HRESULT);
var ErrorInfo: PMAPIERROR;
    ErrorMessage: string;
begin
  ErrorMessage := 'MAPI Error:'#13#10#13#10'"' + LeadInMessage + '"'#13#10;
  if MapiSession = nil then
    ErrorMessage := ErrorMessage + 'No MAPI session available for interrogation.'
  else begin
    ErrorInfo := nil;
    try
      MapiSession.GetLastError(ErrorResult, 0, ErrorInfo);
      if not Assigned(ErrorInfo) then
        ErrorMessage := ErrorMessage + 'No MAPI error information available.'
      else begin
        if ErrorInfo^.lpszComponent <> nil then
          ErrorMessage := ErrorMessage + 'Component "' +
            ErrorInfo^.lpszComponent + '" failed.'#13#10;
        if ErrorInfo^.lpszError <> nil then
          ErrorMessage := ErrorMessage + ErrorInfo^.lpszError;
      end;
    finally
      if Assigned(ErrorInfo) then
        MapiFreeBuffer(ErrorInfo);
    end;
  end;

  inherited Create(ErrorMessage);
end;

{ TMapiConnection }

constructor TMapiConnection.Create(const AProfileName: string;
  const SharedSession: Boolean = False);
begin
  pointer(FSession) := nil;
  FSharedSession := SharedSession;
  FProfileName := AProfileName;
  MapiServicesInitialise;
end;

destructor TMapiConnection.Destroy;
begin
  MapiServicesUninitialise;
  inherited;
end;

procedure TMapiConnection.MapiServicesInitialise;
var MapiInit: TMapiInit_0;
begin
  FillChar(MapiInit, SizeOf(TMapiInit_0), 0);
  MapiInit.ulVersion := MAPI_INIT_VERSION;  // Default Mapi Version
  MapiInit.ulFlags := MAPI_NT_SERVICE;      // Start as non-UI NT service
  if MapiInitialize(@MapiInit) <> S_OK then
    raise EMapiError.Create(sMapiInitFailed);
  MapiServicesLogOn;
end;

procedure TMapiConnection.MapiServicesLogOff;
begin
  if FSession <> nil then
  try
    FAddressBook := nil;
    FSession.Logoff(0, 0, 0);
  finally
    // This throws an AV somewhere in the session release code
    // that prevents this service from shutting down properly.
    // Unfortunately this occurs in the MAPI server and as such
    // is out of our control.
    try
      FSession := nil;
    except
      // Something really out of our control. Prevent Delphi from
      // handling this code by erasing the reference without counting
      // (because we tried reference counting above, and it failed). 
      pointer(FSession) := nil;
    end;
  end;
end;

procedure TMapiConnection.MapiServicesLogOn;
var LogonResult: HRESULT;
    LogonFlags: DWORD;
begin
  MapiServicesLogOff;

  LogonFlags :=
    MAPI_EXTENDED or       // Log on with extended capabilities. This flag
                           // should always be set. The older MAPILogon
                           // function is no longer available.

    MAPI_NO_MAIL or        // MAPI should not inform the MAPI spooler of the
                           // session's existence. The result is that
                           // no messages can be sent or received within the
                           // session except through a tightly coupled store
                           // and transport pair. A calling client sets this
                           // flag if it is acting as an agent, if
                           // configuration work must be done, or if the
                           // client is browsing the available message stores.
                           // Windows NT service clients written with the
                           // MAPI client interface must set the MAPI_NO_MAIL
                           // flag in the call to MAPILogonEx. Other types of
                           // Windows NT service clients need not set a flag
                           // for logon because it is automatically set by MAPI.

    MAPI_NT_SERVICE;       // The caller is running as a Windows NT service.
                           // Callers that are running as a service must set
                           // this flag.

  if not FSharedSession then
    // An attempt should be made to create a new MAPI session rather than
    // acquire the shared session. If the MAPI_NEW_SESSION flag is not set,
    // MAPILogonEx uses an existing shared session even if the profile name
    // parameter is not NULL.
    LogonFlags := LogonFlags or MAPI_NEW_SESSION;

  LogonResult := MapiLogonEx(0, pChar(FProfileName), nil, LogonFlags,
    MapiX.IMAPISession(FSession));

  if LogonResult <> S_OK then
    raise EMapiError.Create(Format(sMapiLogonFailed, [LogonResult, FProfileName]));

  LogonResult := FSession.OpenAddressBook(0, nil, AB_NO_DIALOG, FAddressBook);
  if (FAddressBook = nil) or ((LogonResult <> S_OK) and (LogonResult <> MAPI_W_ERRORS_RETURNED)) then
    raise EMapiError.Create(Format(sAddressBookConnectionFailure, [LogonResult,
        FProfileName]));
end;

procedure TMapiConnection.MapiServicesUninitialise;
begin
  MapiServicesLogOff;
  if not FSharedSession then
    MapiUninitialize;
end;

{ TMapiMailAddress }

constructor TMapiMailAddress.Create(const DisplayName, EntryID: string);
begin
  FDisplayName := DisplayName;
  FEntryId := StringToEntryId(EntryID);
end;

constructor TMapiMailAddress.Create(
  const DisplayName: string; const EntryID: TSBINARY);
begin
  FDisplayName := DisplayName;
  FEntryId := CloneEntryId(EntryID);  // As the caller may free lpb themselves.
end;

destructor TMapiMailAddress.Destroy;
begin
  if FEntryId.lpb <> nil then
    MAPIFreeBuffer(FEntryId.lpb);
  inherited;
end;

function TMapiMailAddress.GetEntryIdAsString: string;
begin
  Result := EntryIdToString(FEntryId);
end;

{ TMapiMailAddresses }

procedure TMapiMailAddresses.Add(const AddressItem: TMapiMailAddress);
begin
  if Assigned(AddressItem) then
    FItems.Add(AddressItem);
end;

function TMapiMailAddresses.Add(const DisplayName: string;
  const EntryID: TSBINARY): TMapiMailAddress;
begin
  Result := TMapiMailAddress.Create(DisplayName, EntryID);
  FItems.Add(Result);
end;

function TMapiMailAddresses.Add(const DisplayName,
  EntryID: string): TMapiMailAddress;
begin
  Result := TMapiMailAddress.Create(DisplayName, EntryID);
  FItems.Add(Result);
end;

procedure TMapiMailAddresses.Assign(
  const SourceAddresses: TMapiMailAddresses);
var i: Integer;
begin
  Clear;
  if Assigned(SourceAddresses) and (SourceAddresses.Count > 0) then
    for i := 0 to SourceAddresses.Count - 1 do
      if (IndexOfDisplayName(SourceAddresses[i].DisplayName) = -1) and
         (IndexOfEntryID(SourceAddresses[i].EntryIDAsString) = -1) then
        Add(SourceAddresses[i].DisplayName, SourceAddresses[i].EntryID);
end;

procedure TMapiMailAddresses.Clear;
var i: Integer;
begin
  if FItems.Count > 0 then
    for i := FItems.Count - 1 downto 0 do
      Delete(i); 
end;

constructor TMapiMailAddresses.Create;
begin
  FItems := TList.Create;
end;

procedure TMapiMailAddresses.Delete(Index: Integer);
var Item: TMapiMailAddress;
begin
  Item := TMapiMailAddress(FItems[Index]);
  Item.Free;
  FItems.Delete(Index);
end;

destructor TMapiMailAddresses.Destroy;
begin
  if Assigned(FItems) then
  try
    Clear;
  finally
    FreeAndNil(FItems);
  end;
  inherited;
end;

function TMapiMailAddresses.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TMapiMailAddresses.GetItem(Index: Integer): TMapiMailAddress;
begin
  Result := TMapiMailAddress(FItems[Index]);
end;

function TMapiMailAddresses.IndexOfDisplayName(
  const DisplayName: string): Integer;
begin
  if FItems.Count > 0 then
    for Result := 0 to FItems.Count - 1 do
      if CompareText(GetItem(Result).FDisplayName, DisplayName) = 0 then
        Exit;
  Result := -1;
end;

function TMapiMailAddresses.IndexOfEntryID(const EntryID: string): Integer;
begin
  if FItems.Count > 0 then
    for Result := 0 to FItems.Count - 1 do
      if CompareText(GetItem(Result).EntryIDAsString, EntryID) = 0 then
        Exit;
  Result := -1;
end;

{ TMapiAddressBook }

procedure TMapiAddressBook.AddAddressToList(
  const Address: TMapiMailAddress; var AddressList: PADRLIST;
  AddressListIndex: Integer);
var PropValues: PSPropValue;
begin
  AddressList^.aEntries[AddressListIndex].cValues := 4;
  MAPIAllocateBuffer(SizeOf(TSPropValue) * 4,
    pointer(AddressList^.aEntries[AddressListIndex].rgPropVals));
  PropValues := AddressList^.aEntries[AddressListIndex].rgPropVals;
  FillChar(PropValues^, SizeOf(TSPropValue) * 4, 0);
  PropValues^.ulPropTag := PR_RECIPIENT_TYPE;
  PropValues^.Value.ul := FRecipientType;
  Inc(PropValues);
  PropValues^.ulPropTag := PR_ADDRTYPE;
  CopyMapiString('EX', pointer(PropValues^.Value.lpszA));
  Inc(PropValues);
  PropValues^.ulPropTag := PR_ENTRYID;
  PropValues^.Value.bin := CloneEntryId(Address.EntryID);

  // Finally, add the display name. This can't be a Delphi
  // assigned pChar because the resulting block of memory
  // is actually released by the MAPI server during the display
  // of the address book.
  Inc(PropValues);
  PropValues^.ulPropTag := PR_DISPLAY_NAME;
  CopyMapiString(Address.DisplayName, pointer(PropValues^.Value.lpszA));
end;

constructor TMapiAddressBook.Create(const AProfileName: string;
  const SharedSession: Boolean = False);
begin
  inherited;
  FButtonCaption := '';
  FDialogTitle := '';
  FInputCaption := '';
  FRecipientType := MAPI_TO;
end;

function TMapiAddressBook.Execute(
  Addresses: TMapiMailAddresses; AllowMultipleSelection: Boolean): Boolean;
var AddressList: PADRLIST;
    AddressParm: TADRPARM;
    UIParam: ULONG;
    i, BufferSize: Integer;
    DestFieldTitle: pChar;
begin
  Result := False;

  // Populate any existing entries
  if Addresses.Count = 0 then
    AddressList := nil
  else begin
    BufferSize := (SizeOf(TADRENTRY) * Addresses.Count) + SizeOf(ULONG);
    MAPIAllocateBuffer(BufferSize, pointer(AddressList));
    FillChar(AddressList^, BufferSize, 0);
    AddressList^.cEntries := Addresses.Count;
    for i := 0 to Addresses.Count - 1 do
      AddAddressToList(Addresses[i], AddressList, i);
  end;

  try
    // Now set up the dialog box's appearance.
    FillChar(AddressParm, SizeOf(AddressParm), 0);
    AddressParm.ulFlags := AB_RESOLVE or AB_SELECTONLY or DIALOG_MODAL;
    if not AllowMultipleSelection then
      AddressParm.ulFlags := AddressParm.ulFlags or ADDRESS_ONE;
    AddressParm.lpszCaption := pChar(FDialogTitle);
    AddressParm.lpszDestWellsTitle := pChar(FInputCaption);
    if AllowMultipleSelection then
      AddressParm.cDestFields := 1
    else AddressParm.cDestFields := 0;
    AddressParm.nDestFieldFocus := 0;
    DestFieldTitle := pChar(FButtonCaption);
    AddressParm.lppszDestTitles := @DestFieldTitle;
    AddressParm.lpulDestComps := @Cardinal(FRecipientType);
    UIParam := 0;

    Addresses.Clear;
    if AddressBook.Address(UIParam, @AddressParm, AddressList) = S_OK then
    begin
      // If the user hasn't selected any addresses, then
      // AddressList comes back unassigned.
      if Assigned(AddressList) and (AddressList^.cEntries > 0) then
        for i := 0 to AddressList^.cEntries - 1 do
          Addresses.Add(GetAddress(AddressList, i));
      Result := True;
    end;
  finally
    if AddressList <> nil then
      ReleaseAddressList(AddressList);
  end;
end;

function TMapiAddressBook.GetAddress(
  var AddressList: PADRLIST; AddressListIndex: Integer): TMapiMailAddress;
begin
  Result := TMapiMailAddress.Create(
    GetDisplayName(AddressList, AddressListIndex),
    GetEntryID(AddressList, AddressListIndex));
end;

function TMapiAddressBook.GetDisplayName(
  var AddressList: PADRLIST; AddressListIndex: Integer): string;
var i: Integer;
    PropValues: PSPropValue;
begin
  Result := '';
  if AddressList^.aEntries[AddressListIndex].cValues > 0 then
  begin
    PropValues := AddressList^.aEntries[AddressListIndex].rgPropVals;
    for i := 0 to AddressList^.aEntries[AddressListIndex].cValues - 1 do
    begin
      if PropValues^.ulPropTag = PR_DISPLAY_NAME then
      begin
        Result := PropValues^.Value.lpszA;
        Exit;
      end;
      Inc(PropValues);
    end;
  end;
end;

function TMapiAddressBook.GetEntryID(
  var AddressList: PADRLIST; AddressListIndex: Integer): TSBINARY;
var i: Integer;
    PropValues: PSPropValue;
begin
  FillChar(Result, SizeOf(TSBINARY), 0);
  if AddressList^.aEntries[AddressListIndex].cValues > 0 then
  begin
    PropValues := AddressList^.aEntries[AddressListIndex].rgPropVals;
    for i := 0 to AddressList^.aEntries[AddressListIndex].cValues - 1 do
    begin
      if PropValues^.ulPropTag = PR_ENTRYID then
      begin
        Result := PropValues^.Value.bin;
        Exit;
      end;
      Inc(PropValues);
    end;
  end;
end;

procedure TMapiAddressBook.ReleaseAddressList(var AddressList: PADRLIST);
var i: Integer;
begin
  if AddressList^.cEntries > 0 then
    for i := 0 to AddressList^.cEntries - 1 do
      MAPIFreeBuffer(AddressList^.aEntries[i].rgPropVals);
  MAPIFreeBuffer(AddressList);
end;

{ TMapiMailAttachment }

procedure TMapiMailAttachment.BindToStream(const SourceStream: TStream);
var StreamIntf: IStream;
    Stream: TOleStream;
    SourcePosition: Int64;
begin
  SourcePosition := SourceStream.Position;
  SourceStream.Position := 0;
  try
    if FAttachment.OpenProperty(PR_ATTACH_DATA_BIN, IStream, 0,
       MAPI_MODIFY or MAPI_CREATE, IInterface(StreamIntf)) = S_OK then
    try
      Stream := TOleStream.Create(StreamIntf);
      try
        Stream.CopyFrom(SourceStream, SourceStream.Size);
      finally
        Stream.Free;
      end;
    finally
      StreamIntf := nil;
    end;
  finally
    SourceStream.Position := SourcePosition;
  end;
end;

function TMapiMailAttachment.Copy(TargetStream: TStream): Integer;
var MapiStream: IStream;
    OleStream: TOleStream;
    StreamInfo: TStatStg;
begin
  if FAttachment.OpenProperty(PR_ATTACH_DATA_BIN,
      IStream, STGM_READ, 0, IInterface(MapiStream)) <> S_OK then
    raise EMapiError.Create(sMapiAttachAccessError)
  else try
    // Get information regarding this attachment
    MapiStream.Stat(StreamInfo, STATFLAG_NONAME);
    OleStream := TOleStream.Create(MapiStream);
    try
      Result := StreamInfo.cbSize;
      TargetStream.CopyFrom(OleStream, Result);
    finally
      // At this point the contents of OleStream have
      // been transferred into the target stream, so we
      // can safely destroy the object.
      OleStream.Free;
    end;
  finally
    MapiStream := nil;      // Release reference
  end;
end;

constructor TMapiMailAttachment.Create(const AAttachment: IMapiAttachment);
begin
  FAttachment := AAttachment;
end;

constructor TMapiMailAttachment.Create(const SourceMessage: IMapiMessage;
  const StreamName: string; const SourceStream: TStream; Hidden : Boolean = False);
type PropValueArray = array of TSPropValue;
var AttachmentNumber: Cardinal;
    AttachmentProperties: PropValueArray;
    PropertyIndex: Integer;
    Problems: PSPropProblemArray;
begin
  Assert(SourceMessage <> nil);
  AttachmentNumber := 0;
  FAttachment := nil;

  // *** Development Note ***
  // The following lines appear to work on some machines but not
  // on others. I have never had a problem with this code, but in
  // case you do, comment these two lines out....
  // if SourceMessage.CreateAttach(IMapiMessage, 0,
  //     AttachmentNumber, FAttachment) = S_OK then

  // ...and uncomment these lines...
  if SourceMessage.CreateAttach(TGuid(nil^), 0,   // OUTLOOK 2003 requires this version.
      AttachmentNumber, FAttachment) = S_OK then

  // ...this solves the problem, and note: Outlook 2003 requires this
  //    definition instead.
  // *** End Development Note ***


  begin
    BindToStream(SourceStream);

    // Looks bizarre, but this avoids the Delphi compiler throwing
    // a whammy with an "index out of bounds" panic attack.
    PropertyIndex := 0;
    SetLength(AttachmentProperties, 3);

    // FileName
    AttachmentProperties[PropertyIndex].ulPropTag := PR_ATTACH_FILENAME;
    CopyMapiString(StreamName, pointer(AttachmentProperties[PropertyIndex].Value.lpszA));
    Inc(PropertyIndex);

    // Attach Method
    AttachmentProperties[PropertyIndex].ulPropTag := PR_ATTACH_METHOD;
    AttachmentProperties[PropertyIndex].Value.l := ATTACH_BY_VALUE;
    Inc(PropertyIndex);

    // Rendering Information
    AttachmentProperties[PropertyIndex].ulPropTag := PR_RENDERING_POSITION;
    AttachmentProperties[PropertyIndex].Value.l := -1;
    Inc(PropertyIndex);

    if Hidden then
      begin
        SetLength(AttachmentProperties, 4);
        AttachmentProperties[PropertyIndex].ulPropTag := (PT_BOOLEAN) or ($7FFF shl 16);
        AttachmentProperties[PropertyIndex].Value.b := 1;
        Inc(PropertyIndex);
      end;

    if FAttachment.SetProps(PropertyIndex, @AttachmentProperties[0], Problems) = S_OK then
      FAttachment.SaveChanges(FORCE_SAVE or MAPI_DEFERRED_ERRORS);
    MapiFreeBuffer(Problems);
  end;
end;

destructor TMapiMailAttachment.Destroy;
begin
  FAttachment := nil;
  inherited;
end;

function TMapiMailAttachment.GetFileName: string;
begin
  Result := GetMapiStringProperty(FAttachment, PR_ATTACH_LONG_FILENAME);
  if Result = '' then
    Result := GetMapiStringProperty(FAttachment, PR_ATTACH_FILENAME);
end;

{ TMapiMailAttachments }

function TMapiMailAttachments.Add(
  const StreamName: string; const SourceStream: TStream;Hidden : Boolean = False): TMapiMailAttachment;
begin
  Result := TMapiMailAttachment.Create(FMessage, StreamName, SourceStream, Hidden);
  FItems.Add(Result);
end;

constructor TMapiMailAttachments.Create(const AMessage: IMapiMessage);
begin
  FItems := TObjectList.Create(True);
  FMessage := AMessage;
  if FMessage.GetAttachmentTable(0, FAttachments) <> S_OK then
    FAttachments := nil
  else ReadAttachments;
end;

destructor TMapiMailAttachments.Destroy;
begin
  if Assigned(FItems) then
    FreeAndNil(FItems);
  FAttachments := nil;
  FMessage := nil;
  inherited;
end;

function TMapiMailAttachments.GetCount: Cardinal;
begin
  Result := FItems.Count;
end;

function TMapiMailAttachments.GetItem(Index: Integer): TMapiMailAttachment;
begin
  Result := TMapiMailAttachment(FItems[Index]);
end;

procedure TMapiMailAttachments.ReadAttachments;
var AttachProps: TSPropTagArray;
    lpRows: PSRowSet;
    i, AttachmentNumber: Integer;
    Attachment: TMapiMailAttachment;
    AttachIntf: IMapiAttachment;
    AttachProp: PSPropValue;
    AttachCount: Cardinal;
begin
  FAttachments.GetRowCount(0, AttachCount);
  if AttachCount > 0 then
  begin
    AttachProps.cValues := 1;
    SetPropertyTagValue(AttachProps, PROP_INDEX_ATTACH_NUM, PR_ATTACH_NUM);
    if FAttachments.SetColumns(@AttachProps, 0) = S_OK then
      if FAttachments.QueryRows(AttachCount, 0, lpRows) = S_OK then
      try
        if lpRows^.cRows > 0 then
          for i := 0 to lpRows^.cRows - 1 do
          begin
            pointer(AttachIntf) := nil;
            // Unique Attachment ID
            AttachmentNumber := lpRows^.aRow[i].lpProps^[PROP_INDEX_ATTACH_NUM].Value.ul;
            if FMessage.OpenAttach(AttachmentNumber, IMapiAttachment,
                 MAPI_BEST_ACCESS, AttachIntf) = S_OK then
            try
              if HrGetOneProp(AttachIntf, PR_ATTACH_METHOD, AttachProp) = S_OK then
              try
                if AttachProp^.Value.ul = ATTACH_BY_VALUE then
                begin
                  Attachment := TMapiMailAttachment.Create(AttachIntf);
                  FItems.Add(Attachment);
                end;
              finally
                MAPIFreeBuffer(AttachProp);
              end;
            finally
              AttachIntf := nil;
            end;
          end;
      finally
        MAPIFreeBuffer(lpRows);
      end;
  end;
end;

{ TMapiMailItem }

procedure TMapiMailItem.AddRecipient(RecipientType: TMapiRecipientRole;
  const Recipient: string; ShowResolveDialog: Boolean);
var RecipientTable: IMapiTable;
    AddressEntry: PADRLIST;
begin
  try
    if FMessage.GetRecipientTable(0, RecipientTable) <> S_OK then
      raise EMapiError.Create(sMapiRecipientsTableFail);
    try
      MAPIAllocateBuffer(SizeOf(TADRLIST), pointer(AddressEntry));
      try
        // Identify the number of recipients
        AddressEntry^.cEntries := 1;
        SetRecipient(RecipientType, Recipient, @AddressEntry^.aEntries);
        AddRecipients(AddressEntry, ShowResolveDialog);
      finally
        MAPIFreeBuffer(AddressEntry);
      end;
    finally
      RecipientTable := nil;
    end;
  except
    on e: Exception do
      raise EMapiError.Create(Format(
        sMapiNamedRecipientResolutionFail, [Recipient]));
  end;
end;

procedure TMapiMailItem.AddRecipient(RecipientType: TMapiRecipientRole;
  const DisplayName, EntryID: string; ShowResolveDialog: Boolean);
var RecipientTable: IMapiTable;
    AddressEntry: PADRLIST;
begin
  // The display name is the same value as would be returned by the
  // Outlook Object Model DisplayName property for an address entry.
  try
    if FMessage.GetRecipientTable(0, RecipientTable) <> S_OK then
      raise EMapiError.Create(sMapiRecipientsTableFail);
    try
      MAPIAllocateBuffer(SizeOf(TADRLIST), pointer(AddressEntry));
      try
        // Identify the number of recipients
        AddressEntry^.cEntries := 1;
        SetRecipient(RecipientType, DisplayName, EntryID, @AddressEntry^.aEntries);
        AddRecipients(AddressEntry, ShowResolveDialog);
      finally
        MAPIFreeBuffer(AddressEntry);
      end;
    finally
      RecipientTable := nil;
    end;
  except
    on e: Exception do
      if DisplayName <> '' then
        raise EMapiError.Create(Format(
          sMapiNamedRecipientResolutionFail, [DisplayName, e.Message]))
      else
        raise EMapiError.Create(Format(
          sMapiEntryIDResolutionFail, [EntryID, e.Message]))
  end;
end;

procedure TMapiMailItem.AddRecipients(var AddressList: PADRLIST;
  ShowResolveDialog: Boolean);
var dwFlags: DWORD;
begin
  if ShowResolveDialog then
    dwFlags := MAPI_DIALOG
  else dwFlags := 0;
  
  if FFolder.FConnection.AddressBook.ResolveName(0, dwFlags, nil, AddressList) = S_OK then
    if FMessage.ModifyRecipients(MODRECIP_ADD, AddressList) = S_OK then
      Exit;
  raise EMapiError.Create(sMapiRecipientResolutionFail);
end;

constructor TMapiMailItem.Create(const Folder: TCustomMapiFolder;
  const MapiMessage: IMapiMessage; const IsReadOnly: Boolean);
begin
  FFolder := Folder;
  FMessage := MapiMessage;
  FReadOnly := IsReadOnly;
  FDeleted := False;
  FillChar(FEntryID, SizeOf(TSBinary), 0);
  FAttachments := TMapiMailAttachments.Create(FMessage);
end;

procedure TMapiMailItem.Delete;
begin
  if Assigned(FFolder) then
  begin
    FFolder.DeleteItem(Self);
    FDeleted := True;
    FReadOnly := True;
  end;
end;

destructor TMapiMailItem.Destroy;
begin
  if Assigned(FAttachments) then
    FreeAndNil(FAttachments);
  FMessage := nil;
  inherited;
end;

function TMapiMailItem.GetBody: string;
begin
  Assert(not FDeleted, sMapiItemDeleted);
  Result := GetMapiStringProperty(FMessage, PR_BODY);
end;

function TMapiMailItem.GetCreateTime: TDateTime;
begin
  Assert(not FDeleted, sMapiItemDeleted);
  Result := GetMapiTimeProperty(FMessage, PR_CREATION_TIME);
end;

function TMapiMailItem.GetImportance: Integer;
begin
  Assert(not FDeleted, sMapiItemDeleted);
  Result := GetMapiIntProperty(FMessage, PR_IMPORTANCE);
end;

function TMapiMailItem.GetMessageClass: string;
begin
  Assert(not FDeleted, sMapiItemDeleted);
  Result := GetMapiStringProperty(FMessage, PR_MESSAGE_CLASS);
end;

function TMapiMailItem.GetPropertyDirect(const aPropType: LONG;
  PropType: TMapiPropertyType): Variant;
begin
  Assert(not FDeleted, sMapiItemDeleted);
  try
    case PropType of
      ptBoolean: Result := GetMapiBooleanProperty(FMessage, aPropType);
      ptInteger: Result := GetMapiIntProperty(FMessage, aPropType);
      ptTime: Result := GetMapiTimeProperty(FMessage, aPropType);
    else
      Result := GetMapiStringProperty(FMessage, aPropType);
    end;
  except
    on e: Exception do
      raise EMapiError.Create(Format(sMapiPropertyFailure, [aPropType])+ e.Message);
  end;
end;

function TMapiMailItem.GetProperty(const PropName: string;
  PropType: TMapiPropertyType): Variant;
begin
  Assert(not FDeleted, sMapiItemDeleted);
  try
    case PropType of
      ptBoolean: Result := GetMapiBooleanProperty(
        FMessage, GetPropertyId(PropName, PT_BOOLEAN, False, @PS_PUBLIC_STRINGS));
      ptInteger: Result := GetMapiIntProperty(
        FMessage, GetPropertyId(PropName, PT_LONG, False, @PS_PUBLIC_STRINGS));
      ptTime: Result := GetMapiTimeProperty(
        FMessage, GetPropertyId(PropName, PT_SYSTIME, False, @PS_PUBLIC_STRINGS));
    else
      Result := GetMapiStringProperty(FMessage, GetPropertyId(PropName, PT_TSTRING, False, @PS_PUBLIC_STRINGS));
    end;
  except
    on e: Exception do
      raise EMapiError.Create(Format(sMapiPropertyFailure, [PropName])+ e.Message);
  end;
end;

function TMapiMailItem.GetPropertyId(const PropertyName: WideString;
  ulPropType: Cardinal; CreateProperty: Boolean;aGUID : PGUID): Cardinal;
var NameId: TMapiNameId;
    lpNameId: pMapiNameId = nil;
    PropTags: PSPropTagArray;
    ulFlags: Cardinal;
begin
  Assert(not FDeleted, sMapiItemDeleted);
  Result := ULONG(-1);

  NameId.lpguid := aGUID;
  NameId.ulKind := MNID_STRING;
  pWideChar(NameId.Kind.lpwstrName) := pWideChar(PropertyName);
  lpNameId := @NameId;

  PropTags := nil;
  try
    if CreateProperty then
      ulFlags := MAPI_CREATE
    else ulFlags := 0;
    if FMessage.GetIDsFromNames(1, lpNameId, ulFlags, PropTags) = S_OK then
      begin
        if  (PropTags^.cValues > 0)
        and (PROP_TYPE(PropTags^.aulPropTag[0]) <> PT_ERROR)
        and (PROP_ID(PropTags^.aulPropTag[0]) <> 0) then
        begin
          // Change the resulting PT_UNSPECIFIED to the type
          // required by the caller
          Result := PropTags^.aulPropTag[0];
          Result := PROP_TAG(ulPropType, PROP_ID(Result));
        end;
      end;
  finally
    if Assigned(PropTags) then
      MapiFreeBuffer(PropTags);
  end;
end;
function TMapiMailItem.GetPropertyDispId(const PropertyId: LongInt;
  ulPropType: Cardinal; CreateProperty: Boolean; aGUID: PGUID): Cardinal;
var NameId: TMapiNameId;
    lpNameId: pMapiNameId = nil;
    PropTags: PSPropTagArray;
    ulFlags: Cardinal;
begin
  Assert(not FDeleted, sMapiItemDeleted);
  Result := ULONG(-1);

  NameId.lpguid := aGUID;
  NameId.ulKind := MNID_ID;
  NameId.Kind.lID:=PropertyId;
  lpNameId := @NameId;

  PropTags := nil;
  try
    if CreateProperty then
      ulFlags := MAPI_CREATE
    else ulFlags := 0;
    if FMessage.GetIDsFromNames(1, lpNameId, ulFlags, PropTags) = S_OK then
      begin
        if  (PropTags^.cValues > 0)
        and (PROP_TYPE(PropTags^.aulPropTag[0]) <> PT_ERROR)
        and (PROP_ID(PropTags^.aulPropTag[0]) <> 0) then
        begin
          // Change the resulting PT_UNSPECIFIED to the type
          // required by the caller
          Result := PropTags^.aulPropTag[0];
          Result := PROP_TAG(ulPropType, PROP_ID(Result));
        end;
      end;
  finally
    if Assigned(PropTags) then
      MapiFreeBuffer(PropTags);
  end;
end;
function TMapiMailItem.GetReadReceiptRequested: Boolean;
begin
  Assert(not FDeleted, sMapiItemDeleted);
  Result := GetMapiBooleanProperty(FMessage, PR_READ_RECEIPT_REQUESTED);
end;

function TMapiMailItem.GetSender: string;
begin
  Assert(not FDeleted, sMapiItemDeleted);
  Result := GetMapiStringProperty(FMessage, PR_SENDER_NAME);
end;

function TMapiMailItem.GetSentTime: TDateTime;
begin
  Assert(not FDeleted, sMapiItemDeleted);
  Result := GetMapiTimeProperty(FMessage, PR_CLIENT_SUBMIT_TIME);
end;

function TMapiMailItem.GetLastModificationTime: TDateTime;
begin
 Assert(not FDeleted, sMapiItemDeleted);
 Result := GetMapiTimeProperty(FMessage, PR_LAST_MODIFICATION_TIME);
end;

function TMapiMailItem.GetSubject: string;
begin
  Assert(not FDeleted, sMapiItemDeleted);
  Result := GetMapiStringProperty(FMessage, PR_SUBJECT);
end;

function TMapiMailItem.HasProperty(const PropName: string;aGUID : PGUID): Boolean;
begin
  Assert(not FDeleted, sMapiItemDeleted);
  // Development Note: This function does not work, as GetPropertId
  //                   will ALWAYS return a property ID even when no
  //                   such named property exists.
  Result := GetPropertyId(PropName, PT_UNSPECIFIED, False, aGUID) <> ULONG(-1);
end;

procedure TMapiMailItem.ReleaseRecipients(AddressList: PADRLIST);
var AddressEntry: PADRENTRY;
    i: Integer;
begin
  if AddressList^.cEntries > 0 then
  begin
    AddressEntry := @AddressList^.aEntries[0];
    for i := 0 to AddressList^.cEntries - 1 do
    begin
      MapiFreeBuffer(AddressEntry.rgPropVals);
      Inc(AddressEntry);
    end;
  end;
end;

procedure TMapiMailItem.SetBody(const Value: string);
begin
  Assert(not FDeleted, sMapiItemDeleted);
  Assert(not FReadOnly, sMapiItemReadOnly);
  SetMapiStringProperty(FMessage, PR_BODY, Value);
end;

procedure TMapiMailItem.SetImportance(const Value: Integer);
begin
  Assert(not FDeleted, sMapiItemDeleted);
  Assert(not FReadOnly, sMapiItemReadOnly);
  SetMapiIntProperty(FMessage, PR_IMPORTANCE, Value);
end;

procedure TMapiMailItem.SetLastModificationTime(const AValue: TDateTime);
begin
  Assert(not FDeleted, sMapiItemDeleted);
  SetMapiTimeProperty(FMessage, PR_LAST_MODIFICATION_TIME, AValue);
end;

procedure TMapiMailItem.SetMessageClass(const Value: string);
begin
  Assert(not FDeleted, sMapiItemDeleted);
  Assert(not FReadOnly, sMapiItemReadOnly);
  SetMapiStringProperty(FMessage, PR_MESSAGE_CLASS, Value);
end;

procedure TMapiMailItem.SetProperty(const PropName: string;
  PropType: TMapiPropertyType; Value: Variant);
begin
  Assert(not FDeleted, sMapiItemDeleted);
  Assert(not FReadOnly, sMapiItemReadOnly);
  case PropType of
    ptBoolean: SetMapiBooleanProperty(FMessage,
      GetPropertyId(PropName, PT_BOOLEAN, True, @PS_PUBLIC_STRINGS), Value);
    ptInteger: SetMapiIntProperty(FMessage,
      GetPropertyId(PropName, PT_LONG, True, @PS_PUBLIC_STRINGS), Value);
    ptTime: SetMapiTimeProperty(FMessage,
      GetPropertyId(PropName, PT_SYSTIME, True, @PS_PUBLIC_STRINGS), Value);
  else
    SetMapiStringProperty(FMessage,GetPropertyId(PropName, PT_TSTRING, True, @PS_PUBLIC_STRINGS), Value);
  end;
end;

procedure TMapiMailItem.SetPropertyDirect(const aPropType: LONG;
  PropType: TMapiPropertyType; const AValue: Variant);
begin
  Assert(not FDeleted, sMapiItemDeleted);
  Assert(not FReadOnly, sMapiItemReadOnly);
  case PropType of
    ptBoolean: SetMapiBooleanProperty(FMessage,aPropType, aValue);
    ptInteger: SetMapiIntProperty(FMessage,aPropType, aValue);
    ptTime:
      begin
        if (AValue = -1) then
          begin
            if GetMapiTimeProperty(FMessage,aPropType)<>-1 then
              ClearMapiProperty(FMessage,aPropType)
          end
        else
          SetMapiTimeProperty(FMessage,aPropType, aValue);
      end
  else
    SetMapiStringProperty(FMessage,aPropType, aValue);
  end;
end;

procedure TMapiMailItem.SetReadReceiptRequested(const Value: Boolean);
begin
  Assert(not FDeleted, sMapiItemDeleted);
  Assert(not FReadOnly, sMapiItemReadOnly);
  SetMapiBooleanProperty(FMessage, PR_READ_RECEIPT_REQUESTED, Value);
end;

procedure TMapiMailItem.SetRecipient(RecipientType: TMapiRecipientRole;
  const Recipient: string; AddressEntry: PADRENTRY);
const RECIPIENT_TYPES: array [TMapiRecipientRole] of Cardinal =
        (MAPI_BCC, MAPI_CC, MAPI_TO);
var DestinationEntry: PSPropValue;
begin
  // Populate information for this recipient
  MapiAllocateBuffer(3 * SizeOf(TSPropValue), pointer(AddressEntry^.rgPropVals));
  AddressEntry^.ulReserved1 := 0;
  AddressEntry^.cValues := 3;
  DestinationEntry := AddressEntry^.rgPropVals;

  // Destination mailbox (recipient name)
  DestinationEntry^.ulPropTag := PR_DISPLAY_NAME;
  CopyMapiString(Recipient, pointer(DestinationEntry^.Value.lpszA));
  Inc(DestinationEntry);

  // Recipient role
  DestinationEntry^.ulPropTag := PR_RECIPIENT_TYPE;
  DestinationEntry^.Value.l := RECIPIENT_TYPES[RecipientType];
  Inc(DestinationEntry);

  // Address type (SMTP or Exchange)
  DestinationEntry^.ulPropTag := PR_ADDRTYPE;
  if Pos(sSecureMailExternAddrIdentifier, Recipient) <> 0 then
    CopyMapiString(sExchangeSmtpDest, pointer(DestinationEntry^.Value.lpszA))
  else CopyMapiString(sExchangeDest, pointer(DestinationEntry^.Value.lpszA));
end;

procedure TMapiMailItem.SetRecipient(RecipientType: TMapiRecipientRole;
  const DisplayName, EntryID: string; AddressEntry: PADRENTRY);
const RECIPIENT_TYPES: array [TMapiRecipientRole] of Cardinal =
        (MAPI_BCC, MAPI_CC, MAPI_TO);
var DestinationEntry: PSPropValue;
    cValuesCount: Integer;
begin
  // Populate information for this recipient
  cValuesCount := 2;
  if DisplayName <> '' then
    Inc(cValuesCount);
  if EntryID <> '' then
    Inc(cValuesCount);
  MapiAllocateBuffer(cValuesCount * SizeOf(TSPropValue),
    pointer(AddressEntry^.rgPropVals));
  AddressEntry^.ulReserved1 := 0;
  AddressEntry^.cValues := cValuesCount;
  DestinationEntry := AddressEntry^.rgPropVals;

  // Destination mailbox (recipient name)
  if DisplayName <> '' then
  begin
    DestinationEntry^.ulPropTag := PR_DISPLAY_NAME;
    CopyMapiString(DisplayName, pointer(DestinationEntry^.Value.lpszA));
    Inc(DestinationEntry);
    // Address type (i.e., SMTP or Exchange)
    DestinationEntry^.ulPropTag := PR_ADDRTYPE;
    if Pos(sSecureMailExternAddrIdentifier, DisplayName) <> 0 then
      CopyMapiString(sExchangeSmtpDest, pointer(DestinationEntry^.Value.lpszA))
    else CopyMapiString(sExchangeDest, pointer(DestinationEntry^.Value.lpszA));
    Inc(DestinationEntry);
  end;
  // Entry ID
  if EntryID <> '' then
  begin
    DestinationEntry^.ulPropTag := PR_ENTRYID;
    DestinationEntry^.Value.bin := StringToEntryId(EntryID);
    Inc(DestinationEntry);
  end;
  // Recipient Role
  DestinationEntry^.ulPropTag := PR_RECIPIENT_TYPE;
  DestinationEntry^.Value.l := RECIPIENT_TYPES[RecipientType];
  
end;

procedure TMapiMailItem.SetRecipients(RecipientType: TMapiRecipientRole;
  const Recipients: TStrings; ShowResolveDialog: Boolean);
var RecipientTable: IMapiTable;
    AddressList: PADRLIST;
    AddressListSize: Integer;
    AddressEntry: PADRENTRY;
    i: Integer;
begin
  if Recipients.Count > 0 then
  try
    if FMessage.GetRecipientTable(0, RecipientTable) <> S_OK then
      raise EMapiError.Create(sMapiRecipientsTableFail);
    try
      // Allocate a buffer that is big enough to hold all of our
      // destinations. We have to allocate one TADRLIST structure
      // and "Recipients.Count - 1" TADRENTRY structures (minus
      // one because the first entry's stored in the calculation
      // for the initial TADRLIST structure).
      AddressListSize :=
        SizeOf(TADRLIST) + ((Recipients.Count - 1) * SizeOf(TADRENTRY));
      MapiAllocateBuffer(AddressListSize, pointer(AddressList));
      try
        // Identify the number of recipients
        AddressList^.cEntries := Recipients.Count;
        AddressEntry := @AddressList^.aEntries[0];

        // And for each recipient in this role...
        for i := 0 to Recipients.Count - 1 do
        begin
          SetRecipient(RecipientType, Recipients[i], AddressEntry);
          // Proceed to the next address entry
          Inc(AddressEntry);
        end;
        AddRecipients(AddressList, ShowResolveDialog);
      finally
        ReleaseRecipients(AddressList);  // Release the address list entries
        MapiFreeBuffer(AddressList);     // before releasing the address list.
      end;
    finally
      RecipientTable := nil;
    end;
  except
    on e: Exception do
      raise EMapiError.Create(Format(
        sMapiNamedRecipientResolutionFail, [Recipients.Text, e.Message]));
  end;
end;

procedure TMapiMailItem.SetSender(const Value: string);
begin
  Assert(not FDeleted, sMapiItemDeleted);
  Assert(not FReadOnly, sMapiItemReadOnly);
  SetMapiStringProperty(FMessage, PR_SENDER_NAME, Value);
end;

procedure TMapiMailItem.SetSubject(const Value: string);
begin
  Assert(not FDeleted, sMapiItemDeleted);
  Assert(not FReadOnly, sMapiItemReadOnly);
  SetMapiStringProperty(FMessage, PR_SUBJECT, Value);
end;

function TMapiMailItem.Submit(const DeleteOnSubmit: Boolean): Boolean;
begin
  Assert(not FDeleted, sMapiItemDeleted);
  Assert(not FReadOnly, sMapiItemReadOnlySend);

  Result := False;
  if not FReadOnly then
    if FMessage.SubmitMessage(0) = S_OK then
    begin
      FReadOnly := True;
      Result := True;
      if DeleteOnSubmit then
        Delete;
      FMessage := nil;
    end;
end;

{ TCustomMapiFolder }

procedure TCustomMapiFolder.ClearContentItems;
begin
  if FContentItems <> nil then
  try
    MAPIFreeBuffer(FContentItems);
  finally
    FContentItems := nil;
  end;
end;

procedure TCustomMapiFolder.ConnectToContents;
begin
  if FFolder.GetContentsTable(0, FContentsTable) <> S_OK then
    raise EMapiError.Create(sMapiMailboxContentsNotFound);
end;

constructor TCustomMapiFolder.Create(const Connection: TMapiConnection);
begin
  FItemIndex := -1;
  FConnection := Connection;
  ConnectToMailbox;
  if FFolder = nil then
    raise EMapiError.Create(Format(sMapiMailboxNotFound, [Connection.ProfileName]));
  ConnectToContents;
end;

procedure TCustomMapiFolder.DeleteItem(const MailItem: TMapiMailItem);
var Entries: TENTRYLIST;
    EntryID: TSBinary;
    DeleteResult: DWORD;
begin
  if Assigned(MailItem) then
  begin
    EntryID := MailItem.FEntryID;
    Entries.cValues := 1;
    Entries.lpbin := @EntryID;
    DeleteResult := FFolder.DeleteMessages(@Entries, 0, nil, 0);
    if (DeleteResult = S_OK) or (DeleteResult = MAPI_W_PARTIAL_COMPLETION) then
    begin
      // Mark as deleted anyway, as MAPI destroys
      // all properties associated with this message
      // as soon as SubmitMessage is called.
      MailItem.FReadOnly := True;
      MailItem.FDeleted := True;
    end;
  end;
end;

destructor TCustomMapiFolder.Destroy;
begin
  ClearContentItems;
  FContentsTable := nil;
  FFolder := nil;
  inherited;
end;

function TCustomMapiFolder.GetFirst: TMapiMailItem;
begin
  PopulateContentItems;
  if Assigned(FContentItems) and (FContentItems^.cRows > 0) then
  begin
    FItemIndex := 0;
    Result := GetMailItem(FItemIndex);
  end else begin
    Result := nil;
    FItemIndex := -1;
  end;
end;

function TCustomMapiFolder.GetMailItem(Index: Cardinal): TMapiMailItem;
var BinaryInfo: TSBinary;
    MailMessage: IMapiMessage;
    ulObjectType: Cardinal;
begin
  Result := nil;
  pointer(MailMessage) := nil;
  if Assigned(FContentItems) then
  try
    // Make sure this is a valid item index in the list of
    // mail items for this folder.
    if Index >= FContentItems^.cRows then
      Result := nil
    else begin
      BinaryInfo := FContentItems^.aRow[Index].lpProps^[PROP_INDEX_ENTRYID].Value.bin;
      if FFolder.OpenEntry(BinaryInfo.cb, pEntryID(BinaryInfo.lpb),
           IMapiMessage, MAPI_BEST_ACCESS, ulObjectType,
           IInterface(MailMessage)) = S_OK then
      begin
        Result := TMapiMailItem.Create(Self, MailMessage, True);
        Result.FEntryID := BinaryInfo;
      end;
    end;
  except
    // If this index is out of bounds...
    Result := nil;
  end;
end;

function TCustomMapiFolder.GetNext: TMapiMailItem;
begin
  // We need to convert this to a signed value, so make sure it it not too large
  if Assigned(FContentItems) then
    assert(FContentItems^.cRows <= Cardinal(MAXINT));

  if Assigned(FContentItems) and (FItemIndex < Integer(FContentItems^.cRows) - 1) then
  begin
    Inc(FItemIndex);
    Result := GetMailItem(FItemIndex);
  end else
    Result := nil;
end;

function TCustomMapiFolder.GetSession: IMapiSession;
begin
  Result := FConnection.FSession;
end;

procedure TCustomMapiFolder.PopulateContentItems;
var ContentRows: Cardinal;
    ContentProperties: TSPropTagArray;
    RowsSought: Integer;
begin
  // Clear any existing items
  ClearContentItems;

  // Determine the number of items available
  FContentsTable.GetRowCount(0, ContentRows);
  if ContentRows > 0 then
  begin
    // Make sure we're on the first row of the table
    FContentsTable.SeekRow(0, 0, RowsSought);
    // And then obtain the list of ID's for each mail item.
    ContentProperties.cValues := 1;
    SetPropertyTagValue(ContentProperties, PROP_INDEX_ENTRYID, PR_ENTRYID);
    if FContentsTable.SetColumns(@ContentProperties, 0) = S_OK then
      if FContentsTable.QueryRows(ContentRows, 0, FContentItems) <> S_OK then
        ClearContentItems;
  end;
end;

{ TInbox }

procedure TInbox.ConnectToMailbox;
var MessageStoreTable: IMapiTable;
    MessageStoreCount: Cardinal;
    MessageStoreProperties: pSPropTagArrayEx;
    MessageStoreRows: PSRowSet;
    DefaultStoreEID: TSBinary;
begin
  pointer(MessageStoreTable) := nil;
  if (Session <> nil) and
     (Session.GetMsgStoresTable(0, MessageStoreTable) = S_OK) then
  try
    MessageStoreRows := nil;
    MessageStoreProperties := SizedSPropTagArray(2);  // Modified 15 Sep 2004,
    try                                               // Courtesy of Ian Hinson
      MessageStoreProperties^.aulPropTag[PROP_INDEX_ENTRYID] := PR_ENTRYID;
      MessageStoreProperties^.aulPropTag[PROP_INDEX_DEFAULTSTORE] := PR_DEFAULT_STORE;
      if MessageStoreTable.SetColumns(@MessageStoreProperties^.SPropTagArray, 0) = S_OK then
      begin
        MessageStoreTable.GetRowCount(0, MessageStoreCount);
        if (MessageStoreCount > 0) and
           (MessageStoreTable.QueryRows(MessageStoreCount, 0, MessageStoreRows) = S_OK) then
        try
          if GetDefaultStoreEID(MessageStoreRows, DefaultStoreEID) then
            OpenDefaultStoreEID(DefaultStoreEID);
        finally
          MAPIFreeBuffer(MessageStoreRows);
        end;
      end;
    finally
      FreeMem(MessageStoreProperties);
    end;
  finally
    MessageStoreTable := nil;
  end;
end;

function TInbox.GetDefaultStoreEID(MessageStoreRows: PSRowSet;
  var DefaultStoreEID: TSBinary): Boolean;
var DefaultStoreIndex: Integer;
    RowIndex: Integer;
begin
  Result := False;
  if MessageStoreRows^.cRows > 0 then
  begin
    // Determine indexes
    if MessageStoreRows^.aRow[0].lpProps^[0].ulPropTag = PR_DEFAULT_STORE then
      DefaultStoreIndex := 0
    else DefaultStoreIndex := 1;

    // Find the default store if there is one
    for RowIndex := 0 to MessageStoreRows^.cRows - 1 do
      if Boolean(MessageStoreRows^.aRow[RowIndex].
        lpProps^[DefaultStoreIndex].Value.b) then
      begin
        DefaultStoreEID := MessageStoreRows^.aRow[RowIndex].lpProps^[0].Value.bin;
        Result := True;
        Break;
      end;
  end;
end;

function TInbox.OpenDefaultStoreEID(
  const DefaultStoreEID: TSBinary): Boolean;
var cbEntryID: Cardinal;
    DefaultMessageStore: IMsgStore;
    lpEntryID: pEntryID;
    lpClass: pChar;
    ulObjectType: Cardinal;
begin
  Result := False;
  pointer(DefaultMessageStore) := nil;
  lpClass := nil;
  if Session.OpenMsgStore(0, DefaultStoreEID.cb,
       PEntryID(DefaultStoreEID.lpb), IMsgStore,
       MAPI_BEST_ACCESS or MDB_NO_DIALOG or MDB_NO_MAIL or MDB_WRITE,
       DefaultMessageStore) = S_OK then
  try
    if DefaultMessageStore.GetReceiveFolder(sMapiQuotedIpmNote,
      0, cbEntryID, lpEntryID, lpClass) = S_OK then
    try
      if DefaultMessageStore.OpenEntry(cbEntryID, lpEntryID, IMapiFolder,
           MAPI_MODIFY, ulObjectType, IInterface(FFolder)) <> S_OK then
        pointer(FFolder) := nil
      else Result := True;
    finally
      MAPIFreeBuffer(lpEntryID);
    end;
  finally
    DefaultMessageStore := nil;
  end;
end;

{ TOutbox }

destructor TOutbox.Destroy;
begin
  FSentItems := nil;  // Thanks to Dan Cunningham and Aramello Superina
  inherited;          // (dereferencing of FSentItems - commonsense really :)
end;

function TOutbox.GetOutbox: IMapiFolder;
begin
  Result := FFolder;
end;

function TOutbox.NewMailItem: TMapiMailItem;
var MapiMessage: IMapiMessage;
    SentItemsProps: TSPropTagArray;
    SentItemsValues: PSPropValue;
    lpcValues: Cardinal;
begin
  // This function is responsible for creating a new mail item
  Result := nil;
  if FFolder.CreateMessage(IMapiMessage, 0, MapiMessage) = S_OK then
  try
    // Do not create the new item as "read only".
    Result := TMapiMailItem.Create(Self, MapiMessage, False);

    // Ensure that post-submission, the new mail item
    // is put into the sent items folder.
    SentItemsProps.cValues := 1;
    SentItemsProps.aulPropTag[0] := PR_ENTRYID;
    if FSentItems.GetProps(@SentItemsProps, 0, lpcValues, SentItemsValues) = S_OK then
    try
      Result.FEntryID := SentItemsValues^.Value.bin;
      SetMapiBinaryProperty(MapiMessage,
        PR_SENTMAIL_ENTRYID, SentItemsValues^.Value.bin);
    finally
      MapiFreeBuffer(SentItemsValues);
    end;
  finally
    // Because the message interface is referenced in the call to
    // "TMapiMailItem.Create", we can safely release it here.
    MapiMessage := nil;
  end;
end;

function TOutbox.OpenDefaultStoreEID(
  const DefaultStoreEID: TSBinary): Boolean;
var DefaultMessageStore: IMsgStore;
    QueryProperties: TSPropTagArray;
    lppPropArray: PSPropValue;
    lpcValues: Cardinal;
    lpulObjectType: Cardinal;
    OpenResult: Cardinal;
begin
  Result := False;
  pointer(FSentItems) := nil;
  pointer(DefaultMessageStore) := nil;
  OpenResult := Session.OpenMsgStore(0, DefaultStoreEID.cb,
      PEntryID(DefaultStoreEID.lpb), IMsgStore,
      MAPI_BEST_ACCESS or MDB_NO_DIALOG or MDB_WRITE, DefaultMessageStore);
  if OpenResult <> S_OK then
    raise EMapiProperty.Create('Unable to open Outbox', Session, OpenResult)
  else try
    // Find the Outbox
    QueryProperties.cValues := 1;
    QueryProperties.aulPropTag[0] := PR_IPM_OUTBOX_ENTRYID;
    if DefaultMessageStore.GetProps(@QueryProperties, 0,
         lpcValues, lppPropArray) = S_OK then
    try
      if DefaultMessageStore.OpenEntry(lppPropArray^.Value.bin.cb,
           PENTRYID(lppPropArray^.Value.bin.lpb), IMapiFolder,
           MAPI_BEST_ACCESS or MAPI_MODIFY, lpulObjectType,
           IInterface(FFolder)) <> S_OK then
        FFolder := nil
      else Result := True;
    finally
      MapiFreeBuffer(lppPropArray);
    end;

    // Find the Sent Items folder
    QueryProperties.cValues := 1;
    QueryProperties.aulPropTag[0] := PR_IPM_SENTMAIL_ENTRYID;
    if DefaultMessageStore.GetProps(@QueryProperties, 0,
         lpcValues, lppPropArray) = S_OK then
    try
      if DefaultMessageStore.OpenEntry(lppPropArray^.Value.bin.cb,
           PENTRYID(lppPropArray^.Value.bin.lpb), IMapiFolder,
           MAPI_BEST_ACCESS or MAPI_MODIFY, lpulObjectType,
           IInterface(FSentItems)) <> S_OK then
        FSentItems := nil
      else Result := True;
    finally
      MapiFreeBuffer(lppPropArray);
    end;
  finally
    DefaultMessageStore := nil;
  end;
end;

end.
