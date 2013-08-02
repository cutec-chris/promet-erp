unit edkmdb;

interface

uses Windows, ActiveX, MAPIDefs, MapiTags, MapiCode,
     edkguid;

{$IFDEF CPU64}
  //64 bit - align at 8 bytes
  {$A8}
{$ELSE}
  //32 bit
  {$A+}
{$ENDIF }

const

{$IFDEF FPC}
_DELETE                  = $00010000; { Renamed from DELETE }
READ_CONTROL             = $00020000;
{$ENDIF}

{OPENSTORE_USE_ADMIN_PRIVILEGE		        =1;
OPENSTORE_PUBLIC				=2;
OPENSTORE_HOME_LOGON				=4;
OPENSTORE_TAKE_OWNERSHIP			=8;
OPENSTORE_OVERRIDE_HOME_MDB			=16;
OPENSTORE_TRANSPORT				=32;
OPENSTORE_REMOTE_TRANSPORT			=64;}

// Values for PR_PROFILE_TYPE

{PROFILE_PRIMARY_USER				=1;
PROFILE_DELEGATE				=2;
PROFILE_PUBLIC_STORE				=3;
PROFILE_SUBSCRIPTION				=4;}

//pidStoreMin = $6618;
//pidProfileMin = $6600;

//PR_STORE_OFFLINE = (PT_BOOLEAN) or ((pidStoreMin+$1A) shl 16);
//PR_SCHEDULE_FOLDER_ENTRYID = (PT_BINARY) or ((pidStoreMin+$06) shl 16);
//PR_SPLUS_FREE_BUSY_ENTRYID = (PT_BINARY) or ((pidStoreMin+$0A) shl 16);

//PR_PROFILE_HOME_SERVER_DN  =  (PT_STRING8) or ((pidProfileMin+$12) shl 16);

pbGlobalProfileSectionGuid  : TMAPIUID = (ab:($13,$DB,$B0,$C8,$AA,$05,$10,$1A,$9B,$B0,$00,$AA,$00,$2F,$C4,$5A));

type

  IExchangeManageStore = interface(IUnknown)
    [strIID_IExchangeManageStore]
    function CreateStoreEntryID	(lpszMsgStoreDN : PAnsiChar;
				 lpszMailboxDN : PAnsiChar;
				 ulFlags : ULONG;
				 out lpcbEntryID : ULONG;
				 out lppEntryID : PENTRYID):HResult;stdcall;
    function EntryIDFromSourceKey(cFolderKeySize : ULONG;
                                  lpFolderSourceKey : pointer;
                                  cMessageKeySize : ULONG;
                                  lpMessageSourceKey : pointer;
                                  out lpcbEntryID : ULONG;
                                  out lppEntryID : PENTRYID):HResult; stdcall;
    function GetRights(cbUserEntryID : ULONG;
                       lpUserEntryID : PENTRYID;
                       cbEntryID : ULONG;
                       lpEntryID : PENTRYID;
                       out lpulRights : ULONG):HResult; stdcall;
    function GetMailboxTable(lpszServerName : PAnsiChar;
                             out lppTable : IMAPITABLE;
                             ulFlags : ULONG):HResult; stdcall;
    function GetPublicFolderTable(lpszServerName : PAnsiChar;
                                  out lppTable : IMAPITABLE;
                                  ulFlags : ULONG):HResult; stdcall;
  end;

const

  ROW_ADD = 1;
  ROW_MODIFY = 2;
  ROW_REMOVE = 4;
  ROW_EMPTY = ROW_ADD or ROW_REMOVE;

type

  PROWENTRY = ^TROWENTRY;
  TROWENTRY = record
    ulRowFlags : ULONG;
    cValues : ULONG;
    rgPropVals : PSPropValueArray;
  end;

  PROWLIST = ^TROWLIST;
  TROWLIST = record
    cEntries : ULONG;
    aEntries : array[0..MAPI_DIM-1] of TROWENTRY;
  end;

  IExchangeModifyTable = interface(IUnknown)
  [strIID_IExchangeModifyTable]
    function GetLastError(ulFlags : ULONG; var LPMAPIERROR : PMAPIError):HResult;stdcall;
    function GetTable(ulFlags : ULONG; out LPMAPITABLE : IMAPITable):Hresult;stdcall;
    function ModifyTable(ulFlags : ULONG; lpMods : PROWLIST):HResult;stdcall;
  end;

//
//
//
//
//



(*
 *	WARNING: Many of the property id values contained within this
 *  file are subject to change.  For best results please use the
 *	literals declared here instead of the numerical values.
 *)

const pidExchangeXmitReservedMin		= $3FE0;
const pidExchangeNonXmitReservedMin	= $65E0;
const	pidProfileMin					= $6600;
const	pidStoreMin						= $6618;
const	pidFolderMin					= $6638;
const	pidMessageReadOnlyMin			= $6640;
const	pidMessageWriteableMin			= $6658;
const	pidAttachReadOnlyMin			= $666C;
const	pidSpecialMin					= $6670;
const	pidAdminMin						= $6690;
//const pidSecureProfileMin				= PROP_ID_SECURE_MIN;

(*------------------------------------------------------------------------
 *
 *	PROFILE properties
 *
 *	These are used in profiles which contain the Exchange Messaging
 *	Service.  These profiles contain a "global section" used to store
 *	common data, plus individual sections for the transport provider,
 *	one store provider for the user, one store provider for the public
 *	store, and one store provider for each additional mailbox the user
 *	has delegate access to.
 *
 *-----------------------------------------------------------------------*)

(* GUID of the global section *)

//const	pbGlobalProfileSectionGuid	"\x13\xDB\xB0\xC8\xAA\x05\x10\x1A\x9B\xB0\x00\xAA\x00\x2F\xC4\x5A"


(* Properties in the global section *)

//= (PT_TSTRING or ( ULONG($FFFE) shl 16 ));

const	PR_PROFILE_VERSION				= ( PT_LONG or ( ULONG(pidProfileMin+$00) shl 16));
const	PR_PROFILE_CONFIG_FLAGS			= ( PT_LONG or ( ULONG(pidProfileMin+$01) shl 16));
const	PR_PROFILE_HOME_SERVER			= ( PT_STRING8 or ( ULONG(pidProfileMin+$02) shl 16));
const	PR_PROFILE_HOME_SERVER_DN		= ( PT_STRING8 or ( ULONG(pidProfileMin+$12) shl 16));
const	PR_PROFILE_HOME_SERVER_ADDRS	= ( PT_MV_STRING8 or ( ULONG(pidProfileMin+$13) shl 16));
const	PR_PROFILE_USER					= ( PT_STRING8 or ( ULONG(pidProfileMin+$03) shl 16));
const	PR_PROFILE_CONNECT_FLAGS		= ( PT_LONG or ( ULONG(pidProfileMin+$04) shl 16));
const PR_PROFILE_TRANSPORT_FLAGS		= ( PT_LONG or ( ULONG(pidProfileMin+$05) shl 16));
const	PR_PROFILE_UI_STATE				= ( PT_LONG or ( ULONG(pidProfileMin+$06) shl 16));
const	PR_PROFILE_UNRESOLVED_NAME		= ( PT_STRING8 or ( ULONG(pidProfileMin+$07) shl 16));
const	PR_PROFILE_UNRESOLVED_SERVER	= ( PT_STRING8 or ( ULONG(pidProfileMin+$08) shl 16));
const PR_PROFILE_BINDING_ORDER		= ( PT_STRING8 or ( ULONG(pidProfileMin+$09) shl 16));
const PR_PROFILE_MAX_RESTRICT			= ( PT_LONG or ( ULONG(pidProfileMin+$0D) shl 16));
const	PR_PROFILE_AB_FILES_PATH		= ( PT_STRING8 or ( ULONG(pidProfileMin+$E) shl 16));
const PR_PROFILE_OFFLINE_STORE_PATH	= ( PT_STRING8 or ( ULONG(pidProfileMin+$10) shl 16));
const PR_PROFILE_OFFLINE_INFO			= ( PT_BINARY or ( ULONG(pidProfileMin+$11) shl 16));
const PR_PROFILE_ADDR_INFO			= ( PT_BINARY or ( ULONG(pidSpecialMin+$17) shl 16));
const PR_PROFILE_OPTIONS_DATA			= ( PT_BINARY or ( ULONG(pidSpecialMin+$19) shl 16));
//const PR_PROFILE_SECURE_MAILBOX		= ( PT_BINARY or ( ULONG(pidSecureProfileMin + 0) shl 16));
const PR_DISABLE_WINSOCK				= ( PT_LONG or ( ULONG(pidProfileMin+$18) shl 16));
const	PR_PROFILE_UNRESOLVED_NAME_W		= ( PT_UNICODE or ( ULONG(pidProfileMin+$07) shl 16));
const	PR_PROFILE_UNRESOLVED_SERVER_W	= ( PT_UNICODE or ( ULONG(pidProfileMin+$08) shl 16));

(* Properties passed through the Service Entry to the OST *)
const PR_OST_ENCRYPTION				= (PT_LONG or ( ULONG($6702) shl 16));

(* Values for PR_OST_ENCRYPTION *)
const OSTF_NO_ENCRYPTION              = $80000000;
const OSTF_COMPRESSABLE_ENCRYPTION    = $40000000;
const OSTF_BEST_ENCRYPTION            = $20000000;

(* Properties in each profile section *)

const	PR_PROFILE_OPEN_FLAGS			= ( PT_LONG or ( ULONG(pidProfileMin+$09) shl 16));
const	PR_PROFILE_TYPE					= ( PT_LONG or ( ULONG(pidProfileMin+$0A) shl 16));
const	PR_PROFILE_MAILBOX				= ( PT_STRING8 or ( ULONG(pidProfileMin+$0B) shl 16));
const	PR_PROFILE_SERVER				= ( PT_STRING8 or ( ULONG(pidProfileMin+$0C) shl 16));
const	PR_PROFILE_SERVER_DN			= ( PT_STRING8 or ( ULONG(pidProfileMin+$14) shl 16));

(* Properties in the Public Folders section *)

const PR_PROFILE_FAVFLD_DISPLAY_NAME	 = (PT_STRING8 or ( ULONG(pidProfileMin+$0F) shl 16));
const PR_PROFILE_FAVFLD_COMMENT		 = (PT_STRING8 or ( ULONG(pidProfileMin+$15) shl 16));
const PR_PROFILE_ALLPUB_DISPLAY_NAME	 = (PT_STRING8 or ( ULONG(pidProfileMin+$16) shl 16));
const PR_PROFILE_ALLPUB_COMMENT		 = (PT_STRING8 or ( ULONG(pidProfileMin+$17) shl 16));

// Current value for PR_PROFILE_VERSION
const	PROFILE_VERSION						= $501;

// Bit values for PR_PROFILE_CONFIG_FLAGS

const	CONFIG_SERVICE						= 1;
const	CONFIG_SHOW_STARTUP_UI				= 2;
const	CONFIG_SHOW_CONNECT_UI				= 4;
const	CONFIG_PROMPT_FOR_CREDENTIALS		= 8;
//by Jason Johnston in "How to determine if 'Outlook Cached Exchange Mode' is on?" newsgroup post
const CONFIG_OST_CACHE_PRIVATE    = $00000180;  //2 bits!
const CONFIG_OST_CACHE_PUBLIC     = $00000400;
const CONFIG_OST_CACHE_DELEGATE   = $00000800;


// Bit values for PR_PROFILE_CONNECT_FLAGS

const	CONNECT_USE_ADMIN_PRIVILEGE			= 1;
const	CONNECT_NO_RPC_ENCRYPTION			= 2;
//const   CONNECT_USE_SEPARATE_CONNECTION                 = 4; //http://support.microsoft.com/kb/q260141/

// Bit values for PR_PROFILE_TRANSPORT_FLAGS

const	TRANSPORT_DOWNLOAD					= 1;
const TRANSPORT_UPLOAD					= 2;

// Bit values for PR_PROFILE_OPEN_FLAGS

const	OPENSTORE_USE_ADMIN_PRIVILEGE		= 1;
const OPENSTORE_PUBLIC					= 2;
const	OPENSTORE_HOME_LOGON				= 4;
const OPENSTORE_TAKE_OWNERSHIP			= 8;
const OPENSTORE_OVERRIDE_HOME_MDB			= 16;
const OPENSTORE_TRANSPORT					= 32;
const OPENSTORE_REMOTE_TRANSPORT			= 64;
const OPENSTORE_INTERNET_ANONYMOUS		= 128;
const  OPENSTORE_ALTERNATE_SERVER			= 256;
const  OPENSTORE_IGNORE_HOME_MDB			= 512;
const  OPENSTORE_NO_MAIL				= 1024;
const  OPENSTORE_OVERRIDE_LAST_MODIFIER                 = 2048;
const OPENSTORE_RESTORE_DATABASE = $00100000;

// Values for PR_PROFILE_TYPE

const	PROFILE_PRIMARY_USER				= 1;
const	PROFILE_DELEGATE					= 2;
const	PROFILE_PUBLIC_STORE				= 3;
const	PROFILE_SUBSCRIPTION				= 4;


(*------------------------------------------------------------------------
 *
 *	MDB object properties
 *
 *-----------------------------------------------------------------------*)

(* PR_MDB_PROVIDER GUID in stores table *)

const pbExchangeProviderPrimaryUserGuid	: array[0..15] of byte = ($54,$94,$A1,$C0,$29,$7F,$10,$1B,$A5,$87,$08,$00,$2B,$2A,$25,$17);
const pbExchangeProviderDelegateGuid	: array[0..15] of byte = ($9e,$b4,$77,$00,$74,$e4,$11,$ce,$8c,$5e,$00,$aa,$00,$42,$54,$e2);
const pbExchangeProviderPublicGuid	: array[0..15] of byte = ($78,$b2,$fa,$70,$af,$f7,$11,$cd,$9b,$c8,$00,$aa,$00,$2f,$c4,$5a);
const pbExchangeProviderXportGuid	: array[0..15] of byte = ($a9,$06,$40,$e0,$d6,$93,$11,$cd,$af,$95,$00,$aa,$00,$4a,$35,$c3);
const pbExchangeProviderLocalStoreGuid	: array[0..15] of byte = ($2D,$E5,$6B,$A1,$64,$6E,$11,$d2,$8D,$4E,$00,$C0,$4F,$AE,$23,$71);
const pbExchangeProviderPersistStoreGuid	: array[0..15] of byte = ($98,$A2,$3D,$67,$62,$CF,$4d,$34,$82,$79,$DB,$FA,$6A,$50,$8B,$31);


// All properties in this section are readonly

// Identity of store
	// All stores
const	PR_USER_ENTRYID					= ( PT_BINARY or ( ULONG(pidStoreMin+$01) shl 16));
const	PR_USER_NAME					= ( PT_STRING8 or ( ULONG(pidStoreMin+$02) shl 16));

	// All mailbox stores
const	PR_MAILBOX_OWNER_ENTRYID		= ( PT_BINARY or ( ULONG(pidStoreMin+$03) shl 16));
const	PR_MAILBOX_OWNER_NAME			= ( PT_STRING8 or ( ULONG(pidStoreMin+$04) shl 16));
const PR_OOF_STATE					= ( PT_BOOLEAN or ( ULONG(pidStoreMin+$05) shl 16));

	// Public stores -- name of hierarchy server
const	PR_HIERARCHY_SERVER				= ( PT_TSTRING or ( ULONG(pidStoreMin+$1B) shl 16));

// Entryids of special folders
	// All mailbox stores
const	PR_SCHEDULE_FOLDER_ENTRYID		= ( PT_BINARY or ( ULONG(pidStoreMin+$06) shl 16));

	// All mailbox and gateway stores
const PR_IPM_DAF_ENTRYID				= ( PT_BINARY or ( ULONG(pidStoreMin+$07) shl 16));

	// Public store
const	PR_NON_IPM_SUBTREE_ENTRYID				= ( PT_BINARY or ( ULONG(pidStoreMin+$08) shl 16));
const	PR_EFORMS_REGISTRY_ENTRYID				= ( PT_BINARY or ( ULONG(pidStoreMin+$09) shl 16));
const	PR_SPLUS_FREE_BUSY_ENTRYID				= ( PT_BINARY or ( ULONG(pidStoreMin+$0A) shl 16));
const	PR_OFFLINE_ADDRBOOK_ENTRYID				= ( PT_BINARY or ( ULONG(pidStoreMin+$0B) shl 16));
const	PR_EFORMS_FOR_LOCALE_ENTRYID			= ( PT_BINARY or ( ULONG(pidStoreMin+$0C) shl 16));
const	PR_FREE_BUSY_FOR_LOCAL_SITE_ENTRYID		= ( PT_BINARY or ( ULONG(pidStoreMin+$0D) shl 16));
const	PR_ADDRBOOK_FOR_LOCAL_SITE_ENTRYID		= ( PT_BINARY or ( ULONG(pidStoreMin+$0E) shl 16));
const	PR_OFFLINE_MESSAGE_ENTRYID				= ( PT_BINARY or ( ULONG(pidStoreMin+$0F) shl 16));
const PR_IPM_FAVORITES_ENTRYID				= ( PT_BINARY or ( ULONG(pidStoreMin+$18) shl 16));
const PR_IPM_PUBLIC_FOLDERS_ENTRYID			= ( PT_BINARY or ( ULONG(pidStoreMin+$19) shl 16));

	// Gateway stores
const	PR_GW_MTSIN_ENTRYID				= ( PT_BINARY or ( ULONG(pidStoreMin+$10) shl 16));
const	PR_GW_MTSOUT_ENTRYID			= ( PT_BINARY or ( ULONG(pidStoreMin+$11) shl 16));
const	PR_TRANSFER_ENABLED				= ( PT_BOOLEAN or ( ULONG(pidStoreMin+$12) shl 16));

// This property is preinitialized to 256 bytes of zeros
// GetProp on this property is guaranteed to RPC.  May be used
// to determine line speed of connection to server.
const	PR_TEST_LINE_SPEED				= ( PT_BINARY or ( ULONG(pidStoreMin+$13) shl 16));

// Used with OpenProperty to get interface or ( ULONG(also on folders
const	PR_HIERARCHY_SYNCHRONIZER		= ( PT_OBJECT or ( ULONG(pidStoreMin+$14) shl 16));
const	PR_CONTENTS_SYNCHRONIZER		= ( PT_OBJECT or ( ULONG(pidStoreMin+$15) shl 16));
const	PR_COLLECTOR					= ( PT_OBJECT or ( ULONG(pidStoreMin+$16) shl 16));

// Used with OpenProperty to get interface for folders or ( ULONG(messages or ( ULONG(attachmentson
const	PR_FAST_TRANSFER				= ( PT_OBJECT or ( ULONG(pidStoreMin+$17) shl 16));

// This property is available on mailbox and public stores.  If it exists
// and its value is TRUE or ( ULONG(the store is connected to the offline store provider.
const PR_STORE_OFFLINE				= ( PT_BOOLEAN or ( ULONG(pidStoreMin+$1A) shl 16));

// In transit state for store object.  This state is
// set when mail is being moved and it pauses mail delivery
// to the mail box
const	PR_IN_TRANSIT					= ( PT_BOOLEAN or ( ULONG(pidStoreMin) shl 16));

// Writable only with Admin rights or ( ULONG(available on public stores and folders
const PR_REPLICATION_STYLE			= ( PT_LONG or ( ULONG(pidAdminMin) shl 16));
const PR_REPLICATION_SCHEDULE			= ( PT_BINARY or ( ULONG(pidAdminMin+$01) shl 16));
const PR_REPLICATION_MESSAGE_PRIORITY = ( PT_LONG or ( ULONG(pidAdminMin+$02) shl 16));

// Writable only with Admin rights or ( ULONG(available on public stores
const PR_OVERALL_MSG_AGE_LIMIT		= ( PT_LONG or ( ULONG(pidAdminMin+$03 ) shl 16));
const PR_REPLICATION_ALWAYS_INTERVAL	= ( PT_LONG or ( ULONG(pidAdminMin+$04 ) shl 16));
const PR_REPLICATION_MSG_SIZE			= ( PT_LONG or ( ULONG(pidAdminMin+$05 ) shl 16));

// default replication style=always interval (minutes) shl 16));
const STYLE_ALWAYS_INTERVAL_DEFAULT	= 15;

// default replication message size limit (KB) shl 16));
const REPLICATION_MESSAGE_SIZE_LIMIT_DEFAULT	= 100;

// Values for PR_REPLICATION_STYLE
const STYLE_NEVER				= 0;
const STYLE_NORMAL			= 1;
const STYLE_ALWAYS			= 2;
const STYLE_DEFAULT			= -1;

(*------------------------------------------------------------------------
 *
 *	INCREMENTAL CHANGE SYNCHRONIZATION
 *	folder and message properties
 *
 *-----------------------------------------------------------------------*)

const PR_SOURCE_KEY					= ( PT_BINARY or ( ULONG(pidExchangeNonXmitReservedMin+$0) shl 16));
const PR_PARENT_SOURCE_KEY			= ( PT_BINARY or ( ULONG(pidExchangeNonXmitReservedMin+$1) shl 16));
const PR_CHANGE_KEY					= ( PT_BINARY or ( ULONG(pidExchangeNonXmitReservedMin+$2) shl 16));
const PR_PREDECESSOR_CHANGE_LIST		= ( PT_BINARY or ( ULONG(pidExchangeNonXmitReservedMin+$3) shl 16));

(*------------------------------------------------------------------------
 *
 *	FOLDER object properties
 *
 *-----------------------------------------------------------------------*)

// Read only or ( ULONG(available on all folders
const	PR_FOLDER_CHILD_COUNT			= ( PT_LONG or ( ULONG(pidFolderMin) shl 16));
const	PR_RIGHTS						= ( PT_LONG or ( ULONG(pidFolderMin+$01) shl 16));
const	PR_ACL_TABLE					= ( PT_OBJECT or ( ULONG(pidExchangeXmitReservedMin) shl 16));
const	PR_RULES_TABLE					= ( PT_OBJECT or ( ULONG(pidExchangeXmitReservedMin+$1) shl 16));
const	PR_HAS_RULES				= ( PT_BOOLEAN or ( ULONG(pidFolderMin+$02) shl 16));

//Read only or ( ULONG(available only for public folders
const	PR_ADDRESS_BOOK_ENTRYID		= ( PT_BINARY or ( ULONG(pidFolderMin+$03) shl 16));

//Writable or ( ULONG(available on folders in all stores
const	PR_ACL_DATA					= ( PT_BINARY or ( ULONG(pidExchangeXmitReservedMin) shl 16));
const	PR_RULES_DATA				= ( PT_BINARY or ( ULONG(pidExchangeXmitReservedMin+$1) shl 16));
const	PR_FOLDER_DESIGN_FLAGS		= ( PT_LONG or ( ULONG(pidExchangeXmitReservedMin+$2) shl 16));
const	PR_DESIGN_IN_PROGRESS		= ( PT_BOOLEAN or ( ULONG(pidExchangeXmitReservedMin+$4) shl 16));
const	PR_SECURE_ORIGINATION		= ( PT_BOOLEAN or ( ULONG(pidExchangeXmitReservedMin+$5) shl 16));

//Writable or ( ULONG(available only for public folders
const	PR_PUBLISH_IN_ADDRESS_BOOK	= ( PT_BOOLEAN or ( ULONG(pidExchangeXmitReservedMin+$6) shl 16));
const	PR_RESOLVE_METHOD			= ( PT_LONG or ( ULONG( pidExchangeXmitReservedMin+$7) shl 16));
const	PR_ADDRESS_BOOK_DISPLAY_NAME	= ( PT_TSTRING or ( ULONG(pidExchangeXmitReservedMin+$8) shl 16));

//Writable or ( ULONG(used to indicate locale id for eforms registry subfolders
const	PR_EFORMS_LOCALE_ID			= ( PT_LONG or ( ULONG(pidExchangeXmitReservedMin+$9) shl 16));

// Writable only with Admin rights or ( ULONG(available only for public folders
const PR_REPLICA_LIST				= ( PT_BINARY or ( ULONG(pidAdminMin+$8) shl 16));
const PR_OVERALL_AGE_LIMIT		= ( PT_LONG or ( ULONG(pidAdminMin+$9) shl 16));

//PR_RESOLVE_METHOD values
const	RESOLVE_METHOD_DEFAULT			= 0 ;	// default handling attach conflicts
const	RESOLVE_METHOD_LAST_WRITER_WINS	= 1;	// the last writer will win conflict
const	RESOLVE_METHOD_NO_CONFLICT_NOTIFICATION = 2; // no conflict notif

//Read only or ( ULONG(available only for public folder favorites
const PR_PUBLIC_FOLDER_ENTRYID	= ( PT_BINARY or ( ULONG(pidFolderMin+$04) shl 16));

(*------------------------------------------------------------------------
 *
 *	MESSAGE object properties
 *
 *-----------------------------------------------------------------------*)

// Read only or ( ULONG(automatically set on all messages in all stores
const	PR_HAS_NAMED_PROPERTIES			 = (PT_BOOLEAN or ( ULONG(pidMessageReadOnlyMin+$0A) shl 16));

// Read only but outside the provider specific range for replication thru GDK-GWs
const	PR_CREATOR_NAME					 = (PT_TSTRING or ( ULONG(pidExchangeXmitReservedMin+$18) shl 16));
const	PR_CREATOR_NAME_W				 = (PT_UNICODE or ( ULONG(pidExchangeXmitReservedMin+$18) shl 16));
const	PR_CREATOR_ENTRYID				 = (PT_BINARY or ( ULONG(pidExchangeXmitReservedMin+$19) shl 16));
const	PR_LAST_MODIFIER_NAME			 = (PT_TSTRING or ( ULONG(pidExchangeXmitReservedMin+$1A) shl 16));
const	PR_LAST_MODIFIER_NAME_W			 = (PT_UNICODE or ( ULONG(pidExchangeXmitReservedMin+$1A) shl 16));
const	PR_LAST_MODIFIER_ENTRYID		 = (PT_BINARY or ( ULONG(pidExchangeXmitReservedMin+$1B) shl 16));

// Read only or ( ULONG(appears on messages which have DAM's pointing to them
const PR_HAS_DAMS						= ( PT_BOOLEAN or ( ULONG(pidExchangeXmitReservedMin+$A) shl 16));
const PR_RULE_TRIGGER_HISTORY			= ( PT_BINARY or ( ULONG(pidExchangeXmitReservedMin+$12) shl 16));
const	PR_MOVE_TO_STORE_ENTRYID		= ( PT_BINARY or ( ULONG(pidExchangeXmitReservedMin+$13) shl 16));
const	PR_MOVE_TO_FOLDER_ENTRYID		= ( PT_BINARY or ( ULONG(pidExchangeXmitReservedMin+$14) shl 16));

// Read only or ( ULONG(available only on messages in the public store
const	PR_REPLICA_SERVER				 = (PT_TSTRING or ( ULONG(pidMessageReadOnlyMin+$4) shl 16));
const	PR_REPLICA_SERVER_W				 = (PT_UNICODE or ( ULONG(pidMessageReadOnlyMin+$4) shl 16));

// Writeable or ( ULONG(used for recording send option dialog settings
const	PR_DEFERRED_SEND_NUMBER			= ( PT_LONG or ( ULONG(pidExchangeXmitReservedMin+$B) shl 16));
const	PR_DEFERRED_SEND_UNITS			= ( PT_LONG or ( ULONG(pidExchangeXmitReservedMin+$C) shl 16));
const	PR_EXPIRY_NUMBER				= ( PT_LONG or ( ULONG(pidExchangeXmitReservedMin+$D) shl 16));
const	PR_EXPIRY_UNITS					= ( PT_LONG or ( ULONG(pidExchangeXmitReservedMin+$E) shl 16));

// Writeable or ( ULONG(deferred send time
const PR_DEFERRED_SEND_TIME			= ( PT_SYSTIME or ( ULONG(pidExchangeXmitReservedMin+$F) shl 16));

//Writeable or ( ULONG(intended for both folders and messages in gateway mailbox
const	PR_GW_ADMIN_OPERATIONS			= ( PT_LONG or ( ULONG(pidMessageWriteableMin) shl 16));

//Writeable or ( ULONG(used for DMS messages
const PR_P1_CONTENT					= ( PT_BINARY or ( ULONG($1100) shl 16));
const PR_P1_CONTENT_TYPE				= ( PT_BINARY or ( ULONG($1101) shl 16));

// Properties on deferred action messages
const	PR_CLIENT_ACTIONS		  		 = (PT_BINARY or ( ULONG(pidMessageReadOnlyMin+$5) shl 16));
const	PR_DAM_ORIGINAL_ENTRYID			 = (PT_BINARY or ( ULONG(pidMessageReadOnlyMin+$6) shl 16));
const PR_DAM_BACK_PATCHED				= ( PT_BOOLEAN or ( ULONG(pidMessageReadOnlyMin+$7) shl 16));

// Properties on deferred action error messages
const	PR_RULE_ERROR					 = (PT_LONG or ( ULONG(pidMessageReadOnlyMin+$8) shl 16));
const	PR_RULE_ACTION_TYPE				 = (PT_LONG or ( ULONG(pidMessageReadOnlyMin+$9) shl 16));
const	PR_RULE_ACTION_NUMBER			 = (PT_LONG or ( ULONG(pidMessageReadOnlyMin+$10) shl 16));
const PR_RULE_FOLDER_ENTRYID			 = (PT_BINARY or ( ULONG(pidMessageReadOnlyMin+$11) shl 16));

// Property on conflict notification indicating entryid of conflicting object
const	PR_CONFLICT_ENTRYID				 = (PT_BINARY or ( ULONG(pidExchangeXmitReservedMin+$10) shl 16));

// Property on messages to indicate the language client used to create this message
const	PR_MESSAGE_LOCALE_ID			 = (PT_LONG or ( ULONG(pidExchangeXmitReservedMin+$11) shl 16));

// Properties on Quota warning messages to indicate Storage quota and Excess used
const	PR_STORAGE_QUOTA_LIMIT			 = (PT_LONG or ( ULONG(pidExchangeXmitReservedMin+$15) shl 16));
const	PR_EXCESS_STORAGE_USED			 = (PT_LONG or ( ULONG(pidExchangeXmitReservedMin+$16) shl 16));
const PR_SVR_GENERATING_QUOTA_MSG		 = (PT_TSTRING or ( ULONG(pidExchangeXmitReservedMin+$17) shl 16));

// Property affixed by delegation rule and deleted on forwards
const PR_DELEGATED_BY_RULE			= ( PT_BOOLEAN or ( ULONG(pidExchangeXmitReservedMin+$3) shl 16));

// Message status bit used to indicate message is in conflict
const	MSGSTATUS_IN_CONFLICT			= $800;
        //MSGSTATUS_ANSWERED                      =
        //MSGSTATUS_DRAFT                         =

(*------------------------------------------------------------------------
 *
 *	ATTACHMENT object properties
 *
 *-----------------------------------------------------------------------*)

// Appears on attachments to a message marked to be in conflict.  Identifies
// those attachments which are conflicting versions of the top level message
const	PR_IN_CONFLICT					 = (PT_BOOLEAN or ( ULONG(pidAttachReadOnlyMin) shl 16));


(*------------------------------------------------------------------------
 *
 *	TABLE object properties
 *
 *	Id Range: $662F-$662F
 *
 *-----------------------------------------------------------------------*)

//This property can be used in a contents table to get PR_ENTRYID returned
//as a long term entryid instead of a short term entryid.
const	PR_LONGTERM_ENTRYID_FROM_TABLE	 = (PT_BINARY or ( ULONG(pidSpecialMin) shl 16));


(*------------------------------------------------------------------------
 *
 *	Gateway "MTE" ENVELOPE properties
 *
 *	Id Range:  $66E0-$66FF
 *
 *-----------------------------------------------------------------------*)

const PR_ORIGINATOR_NAME				= ( PT_TSTRING or ( ULONG(pidMessageWriteableMin+$3) shl 16));
const PR_ORIGINATOR_ADDR				= ( PT_TSTRING or ( ULONG(pidMessageWriteableMin+$4) shl 16));
const PR_ORIGINATOR_ADDRTYPE			= ( PT_TSTRING or ( ULONG(pidMessageWriteableMin+$5) shl 16));
const PR_ORIGINATOR_ENTRYID			= ( PT_BINARY or ( ULONG(pidMessageWriteableMin+$6) shl 16));
const PR_ARRIVAL_TIME					= ( PT_SYSTIME or ( ULONG(pidMessageWriteableMin+$7) shl 16));
const PR_TRACE_INFO					= ( PT_BINARY or ( ULONG(pidMessageWriteableMin+$8) shl 16));
const PR_INTERNAL_TRACE_INFO 			= ( PT_BINARY or ( ULONG(pidMessageWriteableMin+$12) shl 16));
const PR_SUBJECT_TRACE_INFO			= ( PT_BINARY or ( ULONG(pidMessageWriteableMin+$9) shl 16));
const PR_RECIPIENT_NUMBER				= ( PT_LONG or ( ULONG(pidMessageWriteableMin+$A) shl 16));
const PR_MTS_SUBJECT_ID				 = (PT_BINARY or ( ULONG(pidMessageWriteableMin+$B) shl 16));
const PR_REPORT_DESTINATION_NAME		 = (PT_TSTRING or ( ULONG(pidMessageWriteableMin+$C) shl 16));
const PR_REPORT_DESTINATION_ENTRYID	 = (PT_BINARY or ( ULONG(pidMessageWriteableMin+$D) shl 16));
const PR_CONTENT_SEARCH_KEY			 = (PT_BINARY or ( ULONG(pidMessageWriteableMin+$E) shl 16));
const PR_FOREIGN_ID					 = (PT_BINARY or ( ULONG(pidMessageWriteableMin+$F) shl 16));
const PR_FOREIGN_REPORT_ID			 = (PT_BINARY or ( ULONG(pidMessageWriteableMin+$10) shl 16));
const PR_FOREIGN_SUBJECT_ID			 = (PT_BINARY or ( ULONG(pidMessageWriteableMin+$11) shl 16));
const PR_MTS_ID						= PR_MESSAGE_SUBMISSION_ID;
const PR_MTS_REPORT_ID				= PR_MESSAGE_SUBMISSION_ID;


(*------------------------------------------------------------------------
 *
 *	Trace properties format
 *		PR_TRACE_INFO
 *		PR_INTERNAL_TRACE_INFO
 *
 *-----------------------------------------------------------------------*)

const MAX_ADMD_NAME_SIZ       = 17;
const MAX_PRMD_NAME_SIZ       = 17;
const MAX_COUNTRY_NAME_SIZ    = 4;
const MAX_MTA_NAME_SIZ		= 33;

const	ADMN_PAD				= 3;
const	PRMD_PAD				= 3;
const	COUNTRY_PAD				= 0;
const	MTA_PAD					= 3;

(* Access Control Specifics *)

//Properties
const	PR_MEMBER_ID					= ( PT_I8 or ( ULONG(pidSpecialMin+$01) shl 16));
const	PR_MEMBER_NAME					= ( PT_TSTRING or ( ULONG(pidSpecialMin+$02) shl 16));
const	PR_MEMBER_ENTRYID				= PR_ENTRYID;
const	PR_MEMBER_RIGHTS				= ( PT_LONG or ( ULONG(pidSpecialMin+$03) shl 16));

//Security bits
//typedef DWORD RIGHTS;
const frightsReadAny			= $0000001;
const	frightsCreate			= $0000002;
const	frightsEditOwned		= $0000008;
const	frightsDeleteOwned		= $0000010;
const	frightsEditAny			= $0000020;
const	frightsDeleteAny		= $0000040;
const	frightsCreateSubfolder	= $0000080;
const	frightsOwner			= $0000100;
const	frightsContact			= $0000200;	// NOTE: not part of rightsAll
const	rightsNone				= $00000000;
const	rightsReadOnly			= frightsReadAny;
const	rightsReadWrite			= (frightsReadAny or frightsEditAny);
const	rightsAll				= $00001FB;
const frightsVisible			= $0000400; //new      

//Exchange 2000 - from http://blogs.msdn.com/exchange/archive/2004/04/22/118377.aspx

fsdrightReadBody{†}  = $00000001; //The ability to read the “body” of a message.
fsdrightListContents{‡}  = $00000001; // The ability to open a contents table.
fsdrightWriteBody{†}  = $00000002; // The ability to modify the “body” of a message.
fsdrightCreateItem{‡}  = $00000002; // The ability to create new messages
fsdrightAppendMsg{†}  = $00000004; // The ability to append data to a message
fsdrightCreateContainer{‡}  = $00000004; // The ability to create  a subfolder
fsdrightReadProperty  = $00000008; // The ability to read properties on the folder/message.
fsdrightWriteProperty  = $00000010; // the ability to write properties on the folder/message.
fsdrightExecute{†}  = $00000020; // The ability to “execute” the body of a message
fsdrightReserved1{‡}  = $00000040; // The ability to delete arbitrary items under this folder
fsdrightReadAttributes  = $00000080; // The ability to read the “attributes” of a folder/message.
fsdrightWriteAttributes  = $00000100; // The ability to modify the “attributes” of a folder/message.
fsdrightWriteOwnProperty{†}  = $00000200; // The ability to modify messages that the principal granted this right has created.
fsdrightDeleteOwnItem{†}  = $00000400; // The ability to delete messages that the principal granted this right has created.
fsdrightViewItem  = $00000800; // The ability to see the items in the folder.
fsdrightOwner{‡}  = $00004000; // No security semantics, used to indicate the “owner” of a folder.
fsdrightContact{‡}  = $00008000; // No security semantics, used to indicate “contacts” for a folder – people that are not the owner, but are responsible for the folder.
fsdrightWriteSD = WRITE_DAC; // Alias for NT “WRITE_DAC” right.
fsdrightDelete = _DELETE; //Alias for NT “DELETE” right.
fsdrightWriteOwner = WRITE_OWNER; // Alias for NT “WRITE_OWNER” right.
fsdrightReadControl = READ_CONTROL; // Alias for NT “READ_CONTROL” right.
fsdrightSynchronize = SYNCHRONIZE; // Alias for NT “SYNCHRONIZE” right.


(* Rules specifics *)

//Property types
const	PT_SRESTRICTION				= $00FD;
const	PT_ACTIONS					= $00FE;

//Properties in rule table
const	PR_RULE_ID						= ( PT_I8 or ( ULONG(pidSpecialMin+$04) shl 16));
const	PR_RULE_IDS						= ( PT_BINARY or ( ULONG(pidSpecialMin+$05) shl 16));
const	PR_RULE_SEQUENCE				= ( PT_LONG or ( ULONG(pidSpecialMin+$06) shl 16));
const	PR_RULE_STATE					= ( PT_LONG or ( ULONG(pidSpecialMin+$07) shl 16));
const	PR_RULE_USER_FLAGS				= ( PT_LONG or ( ULONG(pidSpecialMin+$08) shl 16));
const	PR_RULE_CONDITION				= ( PT_SRESTRICTION or ( ULONG(pidSpecialMin+$09) shl 16));
const	PR_RULE_ACTIONS					= ( PT_ACTIONS or ( ULONG(pidSpecialMin+$10) shl 16));
const	PR_RULE_PROVIDER				= ( PT_STRING8 or ( ULONG(pidSpecialMin+$11) shl 16));
const	PR_RULE_NAME_A					= ( PT_STRING8 or ( ULONG(pidSpecialMin+$12) shl 16));
const	PR_RULE_NAME_W					= ( PT_UNICODE or ( ULONG(pidSpecialMin+$12) shl 16));
const   PR_RULE_NAME = PR_RULE_NAME_A;
const	PR_RULE_LEVEL					= ( PT_LONG or ( ULONG(pidSpecialMin+$13) shl 16));
const	PR_RULE_PROVIDER_DATA			= ( PT_BINARY or ( ULONG(pidSpecialMin+$14) shl 16));

PR_EXTENDED_RULE_ACTIONS		  = $0e990102;  //PT_BINARY
PR_EXTENDED_RULE_CONDITION		= $0e9a0102;  //PT_BINARY
PR_EXTENDED_RULE_SIZE_LIMIT		= $0e9b0003;  //PT_LONG


//PR_STATE property values
const ST_DISABLED			= $0000;
const ST_ENABLED			= $0001;
const ST_ERROR			= $0002;
const ST_ONLY_WHEN_OOF	= $0004;
const ST_KEEP_OOF_HIST	= $0008;
const ST_EXIT_LEVEL		= $0010;
const ST_SKIP_IF_SCL_IS_SAFE = $0020;
const ST_RULE_PARSE_ERROR = $0040;

const ST_CLEAR_OOF_HIST	= $80000000;

//Empty restriction
const NULL_RESTRICTION	= $ff;

// special RELOP for Member of DL
const RELOP_MEMBER_OF_DL	= 100;

//ACTTYPE
const OP_MOVE = 1;
const OP_COPY = 2;
const OP_REPLY = 3;
const OP_OOF_REPLY = 4;
const OP_DEFER_ACTION = 5;
const OP_BOUNCE = 6;
const OP_FORWARD = 7;
const OP_DELEGATE = 8;
const OP_TAG = 9;
const OP_DELETE = 10;
const OP_MARK_AS_READ = 11;

// for OP_REPLY
   DO_NOT_SEND_TO_ORIGINATOR		= 1;
   STOCK_REPLY_TEMPLATE			= 2;

// for OP_FORWARD
   FWD_PRESERVE_SENDER				= 1;
   FWD_DO_NOT_MUNGE_MSG			= 2;
   FWD_AS_ATTACHMENT				= 4;

   RULE_ERR_TOO_MANY_RECIPS = 13;			//number of recips exceded upper limit

//scBounceCode values
BOUNCE_MESSAGE_SIZE_TOO_LARGE = MAPI_DIAG_LENGTH_CONSTRAINT_VIOLATD;
BOUNCE_FORMS_MISMATCH = MAPI_DIAG_RENDITION_UNSUPPORTED;
BOUNCE_ACCESS_DENIED = MAPI_DIAG_MAIL_REFUSED;

//Message class prefix for Reply and OOF Reply templates
szReplyTemplateMsgClassPrefix = 'IPM.Note.Rules.ReplyTemplate.';
szOofTemplateMsgClassPrefix   = 'IPM.Note.Rules.OofTemplate.';

//Action structure
type
  TAction = record
    acttype : ULONG; //ACTTYPE above
    // to indicate which flavour of the action.
    ulActionFlavor : ULONG;
    // Action restriction
    // currently unsed and must be set to NULL
    lpRes : PSRestriction;
    // currently unused, must be set to 0.
    lpPropTagArray : PSPropTagArray;
    // User defined flags
    ulFlags : ULONG;
    // padding to align the union on 8 byte boundary
    dwAlignPad : ULONG;
    case integer of
      OP_MOVE, OP_COPY :
        (
          actMoveCopy : record
            cbStoreEntryId : ULONG;
            lpStoreEntryId : PENTRYID;
            cbFldEntryId : ULONG;
            lpFldEntryId : PENTRYID;
          end;
        );
      OP_REPLY, OP_OOF_REPLY :
        (
          actReply : record
            cbEntryId : ULONG;
            lpEntryId : PENTRYID;
            guidReplyTemplate : TGUID;
          end;
        );
      OP_DEFER_ACTION :
        (
          actDeferAction : record
            cbData : ULONG;
            pbData : PBYTE;
          end;
        );
      OP_BOUNCE : ( scBounceCode : ULONG{SCODE}; );
      OP_FORWARD, OP_DELEGATE : ( lpadrlist : PADRLIST; );
      OP_TAG : (propTag : TSPropValue;);
end;

PACTION = ^TACTION;

// Rules version
const EDK_RULES_VERSION = 1;

//Array of actions
type
  TACTIONS = record
    ulVersion : ULONG;		// use the #define above
    cActions : UINT;
    lpAction : PACTION;
  end;
  PACTIONS = ^TACTIONS;

const
// message class definitions for Deferred Action and Deffered Error messages
szDamMsgClass = 'IPC.Microsoft Exchange 4.0.Deferred Action';
szDemMsgClass = 'IPC.Microsoft Exchange 4.0.Deferred Error';

(*
 *	Rule error codes
 *	Values for PR_RULE_ERROR
 *)
RULE_ERR_UNKNOWN = 1; //general catchall error
RULE_ERR_LOAD = 	2; //unable to load folder rules
RULE_ERR_DELIVERY = 3; //unable to deliver message temporarily
RULE_ERR_PARSING = 4; //error while parsing
RULE_ERR_CREATE_DAE = 5; //error creating DAE message
RULE_ERR_NO_FOLDER = 6; //folder to move/copy doesn't exist
RULE_ERR_NO_RIGHTS = 7; //no rights to move/copy into folder
RULE_ERR_CREATE_DAM = 8; //error creating DAM
RULE_ERR_NO_SENDAS = 9; //can not send as another user
RULE_ERR_NO_TEMPLATE = 10; //reply template is missing
RULE_ERR_EXECUTION = 11; //error in rule execution
RULE_ERR_QUOTA_EXCEEDED	= 12;

RULE_ERR_FIRST = RULE_ERR_UNKNOWN;
RULE_ERR_LAST = RULE_ERR_QUOTA_EXCEEDED;



// Properties for GetMailboxTable
const PR_NT_USER_NAME                         = ( PT_TSTRING or ( ULONG(pidAdminMin+$10) shl 16));
const PR_LOCALE_ID                            = ( PT_LONG or ( ULONG(pidAdminMin+$11 ) shl 16));
const PR_LAST_LOGON_TIME                      = ( PT_SYSTIME or ( ULONG(pidAdminMin+$12 ) shl 16));
const PR_LAST_LOGOFF_TIME                     = ( PT_SYSTIME or ( ULONG(pidAdminMin+$13 ) shl 16));
const PR_STORAGE_LIMIT_INFORMATION			= ( PT_LONG or ( ULONG(pidAdminMin+$14 ) shl 16));

// Properties for GetPublicFolderTable
const PR_FOLDER_FLAGS                         = ( PT_LONG or ( ULONG(pidAdminMin+$18 ) shl 16));
const	PR_LAST_ACCESS_TIME						= ( PT_SYSTIME or ( ULONG(pidAdminMin+$19 ) shl 16));
const PR_RESTRICTION_COUNT                    = ( PT_LONG or ( ULONG(pidAdminMin+$1A ) shl 16));
const PR_CATEG_COUNT                          = ( PT_LONG or ( ULONG(pidAdminMin+$1B ) shl 16));
const PR_CACHED_COLUMN_COUNT                  = ( PT_LONG or ( ULONG(pidAdminMin+$1C ) shl 16));
const PR_NORMAL_MSG_W_ATTACH_COUNT    		= ( PT_LONG or ( ULONG(pidAdminMin+$1D ) shl 16));
const PR_ASSOC_MSG_W_ATTACH_COUNT             = ( PT_LONG or ( ULONG(pidAdminMin+$1E ) shl 16));
const PR_RECIPIENT_ON_NORMAL_MSG_COUNT        = ( PT_LONG or ( ULONG(pidAdminMin+$1F ) shl 16));
const PR_RECIPIENT_ON_ASSOC_MSG_COUNT 		= ( PT_LONG or ( ULONG(pidAdminMin+$20 ) shl 16));
const PR_ATTACH_ON_NORMAL_MSG_COUNT   		= ( PT_LONG or ( ULONG(pidAdminMin+$21 ) shl 16));
const PR_ATTACH_ON_ASSOC_MSG_COUNT    		= ( PT_LONG or ( ULONG(pidAdminMin+$22 ) shl 16));
const PR_NORMAL_MESSAGE_SIZE                  = ( PT_LONG or ( ULONG(pidAdminMin+$23 ) shl 16));
const PR_NORMAL_MESSAGE_SIZE_EXTENDED         = ( PT_I8 or ( ULONG(pidAdminMin+$23 ) shl 16));
const PR_ASSOC_MESSAGE_SIZE                   = ( PT_LONG or ( ULONG(pidAdminMin+$24 ) shl 16));
const PR_ASSOC_MESSAGE_SIZE_EXTENDED          = ( PT_I8 or ( ULONG(pidAdminMin+$24 ) shl 16));
const PR_FOLDER_PATHNAME                       = (PT_TSTRING or ( ULONG(pidAdminMin+$25 ) shl 16));
const PR_OWNER_COUNT							= ( PT_LONG or ( ULONG(pidAdminMin+$26 ) shl 16));
const PR_CONTACT_COUNT						= ( PT_LONG or ( ULONG(pidAdminMin+$27 ) shl 16));

// PT_I8 version of PR_MESSAGE_SIZE defined in mapitags.h
//const	PR_MESSAGE_SIZE_EXTENDED			 = (PT_I8 or ( ULONG(PROP_ID(PR_MESSAGE_SIZE) shl 16));) shl 16));

(* Bits in PR_FOLDER_FLAGS *)
const MDB_FOLDER_IPM                  = $1;
const MDB_FOLDER_SEARCH               = $2;
const MDB_FOLDER_NORMAL               = $4;
const MDB_FOLDER_RULES                = $8;

(* Bits used in ulFlags in GetPublicFolderTable() shl 16)); *)
const MDB_NON_IPM                     = $10;
const MDB_IPM                         = $20;

(* Bits in PR_STORAGE_LIMIT_INFORMATION *)
const MDB_LIMIT_BELOW					= $1;
const MDB_LIMIT_ISSUE_WARNING			= $2;
const MDB_LIMIT_PROHIBIT_SEND			= $4;
const MDB_LIMIT_NO_CHECK				= $8;

(*------------------------------------------------------------------------
 *
 *	Properties used by the Offline Folders API
 *
 *-----------------------------------------------------------------------*)
											  
const PR_OFFLINE_FLAGS				= ( PT_LONG or ( ULONG(pidFolderMin + $5) shl 16));
const PR_SYNCHRONIZE_FLAGS			= ( PT_LONG or ( ULONG(pidExchangeNonXmitReservedMin + $4) shl 16));
							

(*------------------------------------------------------------------------
 *
 *	Flags used by the Offline Folders API
 *
 *-----------------------------------------------------------------------*)

const OF_AVAILABLE_OFFLINE					= $00000001;
const OF_FORCE								= $80000000;

const SF_DISABLE_STARTUP_SYNC					= $00000001;

//new stuff - Exchange 2000; from 2002 SDK

   pidRenMsgFldMin					 = $1080;

   PR_PROFILE_AUTH_PACKAGE = PT_LONG or  ((pidProfileMin+$19) shl 16);	// dup tag of PR_USER_ENTRYID
   PR_PROFILE_RECONNECT_INTERVAL = PT_LONG or ((pidProfileMin+$1a) shl 16);  // dup tag of PR_USER_NAME

//* Properties for Multiple Offline Address Book support (MOAB) */

   PR_PROFILE_MOAB					 =  PT_STRING8 or (pidSpecialMin + $0B );
   PR_PROFILE_MOAB_GUID			 =  PT_STRING8 or ((pidSpecialMin + $0C ) shl 16);
   PR_PROFILE_MOAB_SEQ				 =  PT_LONG or (( pidSpecialMin + $0D ) shl 16);

// Property for setting a list of prop_ids to be excluded
// from the GetProps(NULL) call.
   PR_GET_PROPS_EXCLUDE_PROP_ID_LIST	 =  PT_BINARY or ((pidSpecialMin + $0E ) shl 16);

   CONFIG_NO_AUTO_DETECT				= $00000010;
   CONFIG_OST_CACHE_ONLY				= $00000020;

   CONNECT_USE_SEPARATE_CONNECTION		= 4;
   CONNECT_NO_UNDER_COVER_CONNECTION	= 8;
   CONNECT_ANONYMOUS_ACCESS			= 16;
   CONNECT_NO_NOTIFICATIONS    		 = 32;
   CONNECT_NO_TABLE_NOTIFICATIONS		= 32;	//*	BUGBUG: TEMPORARY */
   CONNECT_NO_ADDRESS_RESOLUTION		= 64;
   CONNECT_IGNORE_NO_PF  = $8000;

   {OPENSTORE_INTERNET_ANONYMOUS		= 128;
   OPENSTORE_ALTERNATE_SERVER		= 256; //* reserved for internal use */
   OPENSTORE_IGNORE_HOME_MDB			= 512; //* reserved for internal use */
   OPENSTORE_NO_MAIL					= 1024;//* reserved for internal use */
   OPENSTORE_OVERRIDE_LAST_MODIFIER	= 2048;}

   PR_NNTP_CONTROL_FOLDER_ENTRYID			 =  PT_BINARY or ((pidSpecialMin+$1B)  shl 16);

   PR_NEWSGROUP_ROOT_FOLDER_ENTRYID		 =  PT_BINARY or ((pidSpecialMin+$1C) shl 16);

   PR_FAVORITES_DEFAULT_NAME				 =  PT_STRING8 or ((pidStoreMin+$1D) shl 16);
   PR_SYS_CONFIG_FOLDER_ENTRYID			 =  PT_BINARY or ((pidStoreMin+$1E) shl 16);
   PR_NNTP_ARTICLE_FOLDER_ENTRYID			 =  PT_BINARY or ((pidSpecialMin+$1A)  shl 16);
   PR_EVENTS_ROOT_FOLDER_ENTRYID			 =  PT_BINARY or ((pidSpecialMin+$A) shl 16);

// Used with OpenProperty to get interface for store object
   PR_CHANGE_ADVISOR				 =  PT_OBJECT or ((pidStoreMin+$1C) shl 16);

// used to set the ics notification suppression guid
   PR_CHANGE_NOTIFICATION_GUID		 =  PT_CLSID or ((pidStoreMin+$1F) shl 16);

   PR_HAS_MODERATOR_RULES		 =  PT_BOOLEAN or ((pidFolderMin+$07 ) shl 16);

   PR_EXTENDED_ACL_DATA		 =  PT_BINARY or ((pidExchangeXmitReservedMin+$1E) shl 16);

// Newsgroup related properties. Writable only with Admin rights.
   PR_IS_NEWSGROUP_ANCHOR		 =  PT_BOOLEAN or ((pidAdminMin+$06) shl 16);
   PR_IS_NEWSGROUP			 =  PT_BOOLEAN or ((pidAdminMin+$07) shl 16);
   PR_NEWSGROUP_COMPONENT		 =  PT_STRING8 or ((pidAdminMin+$15) shl 16);
   PR_INTERNET_NEWSGROUP_NAME	 =  PT_STRING8 or ((pidAdminMin+$17) shl 16);
   PR_NEWSFEED_INFO			 =  PT_BINARY or ((pidAdminMin+$16) shl 16);

// Newsgroup related property.
   PR_PREVENT_MSG_CREATE		 =  PT_BOOLEAN or ((pidExchangeNonXmitReservedMin + $14 ) shl 16);

// IMAP internal date
   PR_IMAP_INTERNAL_DATE		 =  PT_SYSTIME or ((pidExchangeNonXmitReservedMin + $15 ) shl 16);

// Virtual properties to refer to Newsfeed DNs. Cannot get/set these on
// any object. Supported currently only in specifying restrictions.
   PR_INBOUND_NEWSFEED_DN		 =  PT_STRING8 or ((pidSpecialMin+$1D) shl 16);
   PR_OUTBOUND_NEWSFEED_DN		 =  PT_STRING8 or ((pidSpecialMin+$1E) shl 16);

// Used for controlling content conversion in NNTP
   PR_INTERNET_CHARSET			 =  PT_TSTRING or ((pidAdminMin+$A) shl 16);

//Read only. changes everytime a subfolder is created or deleted
   PR_HIERARCHY_CHANGE_NUM		 =  PT_LONG or ((pidFolderMin+$06) shl 16);

   PR_REPLY_RECIPIENT_SMTP_PROXIES	 = PT_TSTRING or ((pidExchangeXmitReservedMin+$1C) shl 16);

   PR_REPLICA_VERSION				 = PT_I8 or ((pidMessageReadOnlyMin+$0B) shl 16);

// computed property used for moderated folder rule
// its an EntryId whose value is:
// ptagSenderEntryId on delivery
// LOGON::PbUserEntryId() for all other cases (move/copy/post)
   PR_ACTIVE_USER_ENTRYID			 = PT_BINARY or ((pidMessageReadOnlyMin+$12) shl 16);

   PR_MESSAGE_CODEPAGE				 =  PT_LONG or ((pidExchangeXmitReservedMin+$1D) shl 16);


// used to indicate how much X400 private extension data is present: none, just the
// message level, or both the message and recipient levels
// !!The high order byte of this ULONG is reserved.!!
   ENV_BLANK						= $00000000;
   ENV_RECIP_NUM					= $00000001;
   ENV_MSG_EXT  					= $00000002;
   ENV_RECIP_EXT					= $00000004;



   PR_X400_ENVELOPE_TYPE			 = PT_LONG or ((pidMessageReadOnlyMin+$13) shl 16);
   X400_ENV_PLAIN				 = (ENV_BLANK);	// no extension
   X400_ENV_VALID_RECIP			         = (ENV_RECIP_NUM or ENV_MSG_EXT);		// just the message level extension
   X400_ENV_FULL_EXT				 = (ENV_RECIP_NUM or ENV_MSG_EXT or ENV_RECIP_EXT);	// both message and recipient levels

//
// bitmask that indicates whether RN, NRN, DR, NDR, OOF, Auto-Reply should be suppressed
//
   AUTO_RESPONSE_SUPPRESS_DR			= $00000001;
   AUTO_RESPONSE_SUPPRESS_NDR			= $00000002;
   AUTO_RESPONSE_SUPPRESS_RN			= $00000004;
   AUTO_RESPONSE_SUPPRESS_NRN			= $00000008;
   AUTO_RESPONSE_SUPPRESS_OOF			= $00000010;
   AUTO_RESPONSE_SUPPRESS_AUTO_REPLY	        = $00000020;

   PR_AUTO_RESPONSE_SUPPRESS		 = PT_LONG or ((pidExchangeXmitReservedMin - $01) shl 16);
   PR_INTERNET_CPID				 = PT_LONG or ((pidExchangeXmitReservedMin - $02) shl 16);

(*------------------------------------------------------------------------
 *
 *	DUMPSTER properties
 *
 *-----------------------------------------------------------------------*)

// Indicates when a message, folder, or mailbox has been deleted.
// (Read only, non transmittable property).
  	PR_DELETED_ON					 = PT_SYSTIME or ((pidSpecialMin + $1F) shl 16);

// Read-only folder properties which indicate the number of messages, and child folders
// that have been "soft" deleted in this folder (and the time the first message was deleted).
   PR_DELETED_MSG_COUNT			 = PT_LONG or ((pidFolderMin + $08) shl 16);
   PR_DELETED_ASSOC_MSG_COUNT		 = PT_LONG or ((pidFolderMin + $0B) shl 16);
   PR_DELETED_FOLDER_COUNT		 = PT_LONG or ((pidFolderMin + $09) shl 16);
   PR_OLDEST_DELETED_ON			 = PT_SYSTIME or ((pidFolderMin + $0A) shl 16);

// Total size of all soft deleted messages
   PR_DELETED_MESSAGE_SIZE_EXTENDED	 = PT_I8 or ((pidAdminMin + $B) shl 16);

// Total size of all normal soft deleted messages
   PR_DELETED_NORMAL_MESSAGE_SIZE_EXTENDED	 = PT_I8 or ((pidAdminMin + $C) shl 16);

// Total size of all associated soft deleted messages
   PR_DELETED_ASSOC_MESSAGE_SIZE_EXTENDED	 = PT_I8 or ((pidAdminMin + $D) shl 16);

// This property controls the retention age limit (minutes) for the Private/Public MDB,
// Mailbox (private only), or Folder (public).
// Note - the Folder/Mailbox retention, if set, overrides the MDB retention.
   PR_RETENTION_AGE_LIMIT			 = PT_LONG or ((pidAdminMin + $34) shl 16);

// This property is set by JET after a full backup has occurred.
// It is used to determine whether or not messages and folders can be "hard" deleted
// before a full backup has captured the last modification to the object.
   PR_LAST_FULL_BACKUP				 = PT_SYSTIME or ((pidSpecialMin + $15) shl 16);


// Property that defines whether a folder is secure or not
   PR_SECURE_IN_SITE				 = PT_BOOLEAN or ((pidAdminMin + $E) shl 16);

   PR_PROMOTE_PROP_ID_LIST			 = PT_BINARY or ((pidMessageWriteableMin+$13) shl 16);

(* 	Special flag bit for GetContentsTable, GetHierarchyTable and
	OpenEntry.
	Supported by > 5.x servers
	If set in GetContentsTable and GetHierarchyTable
	we will show only items that are soft deleted, i.e deleted
	by user but not yet purged from the system. If set in OpenEntry
	we will open this item even if it is soft deleted *)
//* Flag bits must not collide by existing definitions in Mapi */
//****** MAPI_UNICODE			=  $80000000) above */
//****** MAPI_DEFERRED_ERRORS	=  $00000008) below */
//****** MAPI_ASSOCIATED			=  $00000040) below */
//****** CONVENIENT_DEPTH		=  $00000001)	   */
   SHOW_SOFT_DELETES		=  $00000002;

(* 	Special flag bit for DeleteFolder
	Supported by > 5.x servers
	If set the server will hard delete the folder (i.e it will not be
	retained for later recovery) *)
//* Flag bits must not collide by existing definitions in Mapi	*/
//*	DeleteFolder */
//*****	   DEL_MESSAGES			=  $00000001)	*/
//*****	   FOLDER_DIALOG			=  $00000002)	*/
//*****	   DEL_FOLDERS				=  $00000004)	*/
//* EmptyFolder */
//*****	   DEL_ASSOCIATED			=  $00000008)	*/

  	DELETE_HARD_DELETE				=  $00000010;

(*------------------------------------------------------------------------
 *
 *	"IExchangeManageStore2" Interface Declaration
 *
 *	Used for store management functions.
 *
 *-----------------------------------------------------------------------*)


type

  IExchangeManageStore2 = interface(IExchangeManageStore)
    [strIID_IExchangeManageStore2]
    function CreateNewsgroupNameEntryID(
		 lpszNewsgroupName : LPSTR;
                 var lpcbEntryID : ULONG;
                 var lppEntryID : PENTRYID):HResult;stdcall;
  end;

  IExchangeManageStore3 = interface(IExchangeManageStore2)
    [strIID_IExchangeManageStore3]
    function GetMailboxTableOffset(lpszServerName : PAnsiChar;
                                   var lppTable : IMAPITABLE;
                                   ulFlags : ULONG;
                                   Offset : UINT):HResult; stdcall;
  end;

  IExchangeManageStore4 = interface(IExchangeManageStore3)
    [strIID_IExchangeManageStore4]
    function GetPublicFolderTableOffset(lpszServerName : PAnsiChar;
                                        var lppTable : IMAPITABLE;
                                        ulFlags : ULONG;
                                        Offset : UINT):HResult; stdcall;
  end;

(*------------------------------------------------------------------------
 *
 *	"IExchangeNntpNewsfeed" Interface Declaration
 *
 *	Used for Nntp pull newsfeed.
 *
 *-----------------------------------------------------------------------*)

  IExchangeNntpNewsfeed = interface(IUnknown)
  [strIID_IExchangeNntpNewsfeed]
	function Configure(
		lpszNewsfeedDN : LPSTR;
                cValues : ULONG;
                lpIMailPropArray :PSPropValue):HResult;stdcall;
	function CheckMsgIds(
                lpszMsgIds :LPSTR;
                var lpcfWanted : ULONG;
                var lppfWanted : PByte):HResult;stdcall;
	function OpenArticleStream(
		var lppStream : IStream):HResult;stdcall;
   end;

const

// Locale-specific properties
   //PR_LOCALE_ID							 =  PT_LONG or ((pidAdminMin+$11 ) shl 16);
   PR_CODE_PAGE_ID                          =  PT_LONG or ((pidAdminMin+$33 ) shl 16);
   PR_SORT_LOCALE_ID			    =  PT_LONG or ((pidAdminMin+$75 ) shl 16);

   MDB_LIMIT_DISABLED			    = $10;

type

  IExchangeExportChanges = interface(IUnknown)
    [strIID_IExchangeExportChanges]
    function GetLastError(hResult : HResult; ulFlags : ULONG; out lppMAPIError : PMAPIError):HResult;stdcall;
    function Config(lpStream :IStream; ulFlags : ULONG; lpUnk : IUnknown;
                    lpRestriction : PSRestriction;
                    lpIncludeProps : PSPropTagArray; lpExcludeProps : PSPropTagArray;
                    ulBufferSize : ULONG):HResult;stdcall;
    function Synchronize(out lpulSteps : ULONG; out lpulProgress : ULONG):HResult;stdcall;
    function UpdateState(lpStream : IStream):HResult;stdcall;
  end;

  ReadState = record
    cbSourceKey : ULONG;
    pbSourceKey : pointer;
    ulFlags : ULONG;
  end;
  TReadState = ReadState;
  PReadState = ^TReadState;
  TReadStateArray = array[0..0] of TReadState;
  PReadStateArray = ^TReadStateArray;

{*------------------------------------------------------------------------
 *
 *	"IExchangeImportContentsChanges" Interface Declaration
 *
 *	Used for Incremental Synchronization of folder contents (i.e. messages)
 *
 *-----------------------------------------------------------------------*}

  IExchangeImportContentsChanges = interface(IUnknown)
  [strIID_IExchangeImportContentsChanges]
    function GetLastError(hResult : HResult; ulFlags : ULONG; var lppMAPIError : PMAPIError):HResult;stdcall;
    function Config(lpStream :IStream; ulFlags : ULONG):HResult;stdcall;
    function UpdateState(lpStream :IStream):HResult;stdcall;
    function ImportMessageChange(cpvalChanges : ULONG; ppvalChanges : PSPropValue;
                                 ulFlags : ULONG; out lppmessage : IMessage):HResult;stdcall;
    function ImportMessageDeletion(ulFlags : ULONG; lpSrcEntryList : PENTRYLIST):HResult;stdcall;
    function ImportPerUserReadStateChange(cElements : ULONG; lpReadState : PREADSTATE):HResult;stdcall;
    function ImportMessageMove(cbSourceKeySrcFolder : ULONG;
                               pbSourceKeySrcFolder : pointer;
                               cbSourceKeySrcMessage : ULONG;
                               pbSourceKeySrcMessage : pointer;
                               cbPCLMessage : ULONG;
                               pbPCLMessag : pointer;
                               cbSourceKeyDestMessage : ULONG;
                               pbSourceKeyDestMessage : pointer;
                               cbChangeNumDestMessage : ULONG;
                               pbChangeNumDestMessage : pointer):HResult;stdcall;
  end;

  IExchangeImportHierarchyChanges = interface(IUnknown)
  [strIID_IExchangeImportHierarchyChanges]
    function GetLastError(hResult : HResult; ulFlags : ULONG; var lppMAPIError : PMAPIError):HResult;stdcall;
    function Config(lpStream :IStream; ulFlags : ULONG):HResult;stdcall;
    function UpdateState(lpStream :IStream):HResult;stdcall;
    function ImportFolderChange(cpvalChanges : ULONG; ppvalChanges : PSPropValue):HResult;stdcall;
    function ImportFolderDeletion(ulFlags : ULONG; lpSrcEntryList : PENTRYLIST):HResult;stdcall;
  end;

const

{*------------------------------------------------------------------------
 *
 *	Errors returned by Exchange Incremental Change Synchronization Interface
 *
 *-----------------------------------------------------------------------*}


//#define MAKE_SYNC_E(err)	(MAKE_SCODE(SEVERITY_ERROR, FACILITY_ITF, err))
//#define MAKE_SYNC_W(warn)	(MAKE_SCODE(SEVERITY_SUCCESS, FACILITY_ITF, warn))

 SYNC_E_UNKNOWN_FLAGS		=	MAPI_E_UNKNOWN_FLAGS;
 SYNC_E_INVALID_PARAMETER	=	E_INVALIDARG;
 SYNC_E_ERROR		=			E_FAIL;
 SYNC_E_OBJECT_DELETED = (ULONG(SEVERITY_ERROR) shl 31) or (FACILITY_ITF shl 16) or $800;//MAKE_SYNC_E(0x800)
 SYNC_E_IGNORE				 =	(ULONG(SEVERITY_ERROR) shl 31) or (FACILITY_ITF shl 16) or $801;//MAKE_SYNC_E(0x801)
 SYNC_E_CONFLICT			 =	(ULONG(SEVERITY_ERROR) shl 31) or (FACILITY_ITF shl 16) or $802;//MAKE_SYNC_E(0x802)
 SYNC_E_NO_PARENT			 =	(ULONG(SEVERITY_ERROR) shl 31) or (FACILITY_ITF shl 16) or $803;//MAKE_SYNC_E(0x803)
 SYNC_E_INCEST				 =	(ULONG(SEVERITY_ERROR) shl 31) or (FACILITY_ITF shl 16) or $804;//MAKE_SYNC_E(0x804)
 SYNC_E_CYCLE				   =  SYNC_E_INCEST;
 SYNC_E_UNSYNCHRONIZED                   =	(ULONG(SEVERITY_ERROR) shl 31) or (FACILITY_ITF shl 16) or $805;//MAKE_SYNC_E(0x805)

 SYNC_W_PROGRESS			 =	(ULONG(SEVERITY_SUCCESS) shl 31) or (FACILITY_ITF shl 16) or $820;//MAKE_SYNC_W(0x820)
 SYNC_W_CLIENT_CHANGE_NEWER              =	(ULONG(SEVERITY_SUCCESS) shl 31) or (FACILITY_ITF shl 16) or $821;//MAKE_SYNC_W(0x821)


{*------------------------------------------------------------------------
 *
 *	Flags used by Exchange Incremental Change Synchronization Interface
 *
 *-----------------------------------------------------------------------*}

  SYNC_UNICODE				= $01;
  SYNC_NO_DELETIONS			= $02;
  SYNC_NO_SOFT_DELETIONS		= $04;
  SYNC_READ_STATE				= $08;
  SYNC_ASSOCIATED				= $10;
  SYNC_NORMAL					= $20;
  SYNC_NO_CONFLICTS			= $40;
  SYNC_ONLY_SPECIFIED_PROPS	= $80;
  SYNC_NO_FOREIGN_KEYS		= $100;
  SYNC_LIMITED_IMESSAGE		= $200;


{*------------------------------------------------------------------------
 *
 *	Flags used by ImportMessageDeletion and ImportFolderDeletion methods
 *
 *-----------------------------------------------------------------------*}

  SYNC_SOFT_DELETE			= $01;
  SYNC_EXPIRY					= $02;

{*------------------------------------------------------------------------
 *
 *	Flags used by ImportPerUserReadStateChange method
 *
 *-----------------------------------------------------------------------*}

  SYNC_READ					= $01;


(*------------------------------------------------------------------------
 *
 *	"IExchangeExportChanges2" Interface Declaration
 *
 *	Used for Incremental Synchronization
 *	Has the Config2 method for configuring for internet format conversion streams
 *
 *-----------------------------------------------------------------------*)

(*

type

   IExchangeExportChanges2 = interface(IExchangeExportChanges)
	MAPIMETHOD(ConfigForConversionStream)						\
		(THIS_	LPSTREAM			lpStream,				\
				ULONG				ulFlags,				\
				LPUNKNOWN			lpUnk,					\
		 		LPSRestriction		lpRestriction,			\
		 		ULONG				cValuesConversion,			\
		 		LPSPropValue		lpPropArrayConversion,		\
		 		ULONG				ulBufferSize) IPURE;

#undef		 INTERFACE
  		 INTERFACE  IExchangeExportChanges2
DECLARE_MAPI_INTERFACE_(IExchangeExportChanges2, IExchangeExportChanges)
{
	MAPI_IUNKNOWN_METHODS(PURE)
	EXCHANGE_IEXCHANGEEXPORTCHANGES_METHODS(PURE)
	EXCHANGE_IEXCHANGEEXPORTCHANGES2_METHODS(PURE)
};
#undef	IMPL
   IMPL

DECLARE_MAPI_INTERFACE_PTR(IExchangeExportChanges2, LPEXCHANGEEXPORTCHANGES2);

*)

(*

/*------------------------------------------------------------------------
 *
 *	"IExchangeExportChanges3" Interface Declaration
 *
 *	Used for Incremental Synchronization
 *	Has the Config3 method for configuring for selective message download
 *
 *-----------------------------------------------------------------------*/

   EXCHANGE_IEXCHANGEEXPORTCHANGES3_METHODS(IPURE)		\
	MAPIMETHOD(ConfigForSelectiveSync)						\
		(THIS_	LPSTREAM			lpStream,				\
				ULONG				ulFlags,				\
				LPUNKNOWN			lpUnk,					\
				LPENTRYLIST 		lpMsgList,				\
		 		LPSRestriction		lpRestriction,			\
		 	    LPSPropTagArray		lpIncludeProps,			\
		 	    LPSPropTagArray		lpExcludeProps,			\
		 		ULONG				ulBufferSize) IPURE;

#undef		 INTERFACE
  		 INTERFACE  IExchangeExportChanges3
DECLARE_MAPI_INTERFACE_(IExchangeExportChanges3, IExchangeExportChanges2)
{
	MAPI_IUNKNOWN_METHODS(PURE)
	EXCHANGE_IEXCHANGEEXPORTCHANGES_METHODS(PURE)
	EXCHANGE_IEXCHANGEEXPORTCHANGES2_METHODS(PURE)
	EXCHANGE_IEXCHANGEEXPORTCHANGES3_METHODS(PURE)
};
#undef	IMPL
   IMPL

DECLARE_MAPI_INTERFACE_PTR(IExchangeExportChanges3, LPEXCHANGEEXPORTCHANGES3);

*)

(*

/*------------------------------------------------------------------------
 *
 *	"IExchangeImportContentsChanges2" Interface Declaration
 *
 *	Used for Incremental Synchronization of folder contents (i.e. messages)
 *	This interface allows you to import message changes as an internet
 *	format conversion stream
 *
 *-----------------------------------------------------------------------*/


   EXCHANGE_IEXCHANGEIMPORTCONTENTSCHANGES2_METHODS(IPURE)		\
	MAPIMETHOD(ConfigForConversionStream)								\
		(THIS_	LPSTREAM				lpStream,					\
		 		ULONG					ulFlags,					\
		 		ULONG					cValuesConversion,				\
		 		LPSPropValue			lpPropArrayConversion) IPURE;	\
	MAPIMETHOD(ImportMessageChangeAsAStream)						\
		(THIS_	ULONG					cpvalChanges,				\
				LPSPropValue			ppvalChanges,				\
				ULONG					ulFlags,					\
				LPSTREAM				*lppstream) IPURE;			\


#undef		 INTERFACE
  		 INTERFACE  IExchangeImportContentsChanges2
DECLARE_MAPI_INTERFACE_(IExchangeImportContentsChanges2, IExchangeImportContentsChanges)
{
	MAPI_IUNKNOWN_METHODS(PURE)
	EXCHANGE_IEXCHANGEIMPORTCONTENTSCHANGES_METHODS(PURE)
	EXCHANGE_IEXCHANGEIMPORTCONTENTSCHANGES2_METHODS(PURE)
};
#undef	IMPL
   IMPL

DECLARE_MAPI_INTERFACE_PTR(IExchangeImportContentsChanges2,
						   LPEXCHANGEIMPORTCONTENTSCHANGES2);
*)

const
   	ulHierChanged		= ($01);

type

  IExchangeChangeAdviseSink = interface(IUnknown)
  //todo: GUID
	function OnNotify(
		ulFlags :ULONG;
                lpEntryList : PENTRYLIST):HResult;stdcall;
  end;

(*
   EXCHANGE_IEXCHANGECHANGEADVISOR_METHODS(IPURE)				\
	MAPIMETHOD(GetLastError)										\
		(THIS_	HRESULT				hResult,						\
				ULONG 				ulFlags,						\
				LPMAPIERROR FAR *	lppMAPIError) IPURE;			\
	MAPIMETHOD(Config)												\
		(THIS_	LPSTREAM					lpStream,				\
		 		LPGUID						lpGUID,					\
				LPEXCHANGECHANGEADVISESINK	lpAdviseSink,			\
		 		ULONG						ulFlags) IPURE;			\
	MAPIMETHOD(UpdateState)											\
		(THIS_	LPSTREAM			lpStream) IPURE;				\
	MAPIMETHOD(AddKeys)												\
		(THIS_	LPENTRYLIST			lpEntryList) IPURE;				\
	MAPIMETHOD(RemoveKeys)											\
		(THIS_	LPENTRYLIST			lpEntryList) IPURE;

#undef		 INTERFACE
  		 INTERFACE  IExchangeChangeAdvisor
DECLARE_MAPI_INTERFACE_(IExchangeChangeAdvisor, IUnknown)
{
	MAPI_IUNKNOWN_METHODS(PURE)
	EXCHANGE_IEXCHANGECHANGEADVISOR_METHODS(PURE)
};
#undef	IMPL
   IMPL

DECLARE_MAPI_INTERFACE_PTR(IExchangeChangeAdvisor,
						   LPEXCHANGECHANGEADVISOR);
*)

const

   SYNC_CATCHUP				=$400;
   SYNC_NEW_MESSAGE			=$800;	// only applicable to ImportMessageChange()
   SYNC_MSG_SELECTIVE			=$1000;	// Used internally.  Will reject if used by clients.

   SYNC_IMAIL_MIME_FORMAT		=$400;
   SYNC_IMAIL_UUENCODE_FORMAT	=$800;
  	SYNC_ONLY_HEADERS			=$1000;

(*------------------------------------------------------------------------
 *
 *	Properties used by the Favorites Folders API
 *
 *-----------------------------------------------------------------------*)

   PR_AUTO_ADD_NEW_SUBS			 =  PT_BOOLEAN or ((pidExchangeNonXmitReservedMin + $5) shl 16);
   PR_NEW_SUBS_GET_AUTO_ADD		 =  PT_BOOLEAN or ((pidExchangeNonXmitReservedMin + $6) shl 16);

(*------------------------------------------------------------------------
 *
 *	"IExchangeMessageConversion" Interface Declaration
 *
 *	Used to configure message conversion streams
 *
 *-----------------------------------------------------------------------*)
(*
   EXCHANGE_IEXCHANGEMESSAGECONVERSION_METHODS(IPURE)					\
	MAPIMETHOD(OpenStream)										\
		(THIS_	ULONG 						cValues,			\
				LPSPropValue				lpPropArray,		\
				LPSTREAM FAR *				lppStream) IPURE;
#undef		 INTERFACE
  		 INTERFACE  IExchangeMessageConversion
DECLARE_MAPI_INTERFACE_(IExchangeMessageConversion, IUnknown)
{
	MAPI_IUNKNOWN_METHODS(PURE)
	EXCHANGE_IEXCHANGEMESSAGECONVERSION_METHODS(PURE)
};
#undef	IMPL
   IMPL

DECLARE_MAPI_INTERFACE_PTR(IExchangeMessageConversion, LPEXCHANGEMESSAGECONVERSION);

*)

   PR_MESSAGE_SITE_NAME				 =  PT_TSTRING or ((pidExchangeNonXmitReservedMin + $7) shl 16);
   PR_MESSAGE_SITE_NAME_A				 =  PT_STRING8 or ((pidExchangeNonXmitReservedMin + $7) shl 16);
   PR_MESSAGE_SITE_NAME_W				 =  PT_UNICODE or ((pidExchangeNonXmitReservedMin + $7) shl 16);

   PR_MESSAGE_PROCESSED				 =  PT_BOOLEAN or ((pidExchangeNonXmitReservedMin + $8) shl 16);

   PR_MSG_BODY_ID						 =  PT_LONG or ((pidExchangeXmitReservedMin - $03) shl 16);


   PR_BILATERAL_INFO					 =  PT_BINARY or ((pidExchangeXmitReservedMin - $04) shl 16);
   PR_DL_REPORT_FLAGS					 =  PT_LONG or ((pidExchangeXmitReservedMin - $05) shl 16);

   PRIV_DL_HIDE_MEMBERS    =$00000001;
   PRIV_DL_REPORT_TO_ORIG  =$00000002;
   PRIV_DL_REPORT_TO_OWNER =$00000004;
   PRIV_DL_ALLOW_OOF       =$00000008;

(*---------------------------------------------------------------------------------
 *
 *  PR_PREVIEW is a folder contents property that is either PR_ABSTRACT
 *		or the first 255 characters of PR_BODY.
 *	PR_PREVIEW_UNREAD is a folder contents property that is either PR_PREVIEW
 *		if the message is not read, or NULL if it is read.
 *
 *---------------------------------------------------------------------------------*)
   PR_ABSTRACT								 =  PT_TSTRING or ((pidExchangeXmitReservedMin - $06) shl 16);
   PR_ABSTRACT_A							 =  PT_STRING8 or ((pidExchangeXmitReservedMin - $06) shl 16);
   PR_ABSTRACT_W							 =  PT_UNICODE or ((pidExchangeXmitReservedMin - $06) shl 16);

   PR_PREVIEW								 =  PT_TSTRING or ((pidExchangeXmitReservedMin - $07) shl 16);
   PR_PREVIEW_A								 =  PT_STRING8 or ((pidExchangeXmitReservedMin - $07) shl 16);
   PR_PREVIEW_W								 =  PT_UNICODE or ((pidExchangeXmitReservedMin - $07) shl 16);

   PR_PREVIEW_UNREAD							 =  PT_TSTRING or ((pidExchangeXmitReservedMin - $08) shl 16);
   PR_PREVIEW_UNREAD_A							 =  PT_STRING8 or ((pidExchangeXmitReservedMin - $08) shl 16);
   PR_PREVIEW_UNREAD_W							 =  PT_UNICODE or ((pidExchangeXmitReservedMin - $08) shl 16);

//
//	Informs IMAIL that full fidelity should be discarded for this message.
//
  	PR_DISABLE_FULL_FIDELITY		 =  PT_BOOLEAN or (($10f2) shl 16);


(*------------------------------------------------------------------------------------
*
*	OWA Info Property
*
*------------------------------------------------------------------------------------*)
   PR_OWA_URL							         =  PT_STRING8 or ((pidRenMsgFldMin+$71 ) shl 16);

//    SERVER_SLOW_LINK
//#ifdef	SERVER_SLOW_LINK
// The value of this property ID will change in the future.  Do not rely on
// its current value.  Rely on the define only.
   PR_STORE_SLOWLINK							 =  PT_BOOLEAN or (($7c0a) shl 16);
//#endif

(*
/*
 * Registry locations of settings
 */
#if defined(WIN32) && !defined(MAC)
   SZ_OUTL_OST_OPTIONS "Software\\Microsoft\\Office\\8.0\\Outlook\\OST"
   SZ_NO_OST "NoOST"
   NO_OST_FLAG_ALLOWED		0	// OST's are allowed on the machine
   NO_OST_FLAG_CACHE_ONLY	1	// OST can only be used as a cache
   NO_OST_FLAG_NOT_ALLOWED	2	// OST's are not allowed on the machine
   NO_OST_DEFAULT			NO_OST_FLAG_ALLOWED
#endif

*)

MAPI_REASON_TRANSFER_FAILED           =( 0 );
MAPI_REASON_TRANSFER_IMPOSSIBLE       =( 1 );
MAPI_REASON_CONVERSION_NOT_PERFORMED  =( 2 );
MAPI_REASON_PHYSICAL_RENDITN_NOT_DONE =( 3 );
MAPI_REASON_PHYSICAL_DELIV_NOT_DONE   =( 4 );
MAPI_REASON_RESTRICTED_DELIVERY       =( 5 );
MAPI_REASON_DIRECTORY_OPERATN_FAILED  =( 6 );

//* SE 233155 - MarkH: EMSABP DCR /*
//* Properties in the abp section - I got these values from AlecDun (Outlook team) */
PR_PROFILE_ABP_ALLOW_RECONNECT		= ( PT_LONG or ( ULONG(pidProfileMin+$39) shl 16));
PR_PROFILE_ABP_MTHREAD_TIMEOUT_SECS = ( PT_LONG or ( ULONG(pidProfileMin+$3A) shl 16));

//Exchange 2003 SP2 - sender id support
PR_SENDER_ID_STATUS = $40790003;
//Sender_ID Result Numeric Code
SENDER_ID_STATUS_NEUTRAL     = $1;
SENDER_ID_STATUS_PASS        = $2;
SENDER_ID_STATUS_FAIL        = $3;
SENDER_ID_STATUS_SOFT_FAIL   = $4;
SENDER_ID_STATUS_NONE        = $5;
SENDER_ID_STATUS_TEMP_ERROR  = $80000006;
SENDER_ID_STATUS_PERM_ERROR  = $80000007;
 
type

  IExchangeFavorites = interface(IUnknown)
    [strIID_IExchangeFavorites]
    function GetLastError(ulFlags : ULONG; var LPMAPIERROR : PMAPIError):HResult;stdcall;
    function AddFavorites(lpEntryList : PENTRYLIST):HResult;stdcall;
    function DelFavorites(lpEntryList : PENTRYLIST):HResult;stdcall;
  end;


const
  PR_SYNCEVENT_FIRED =  PT_BOOLEAN or ((pidExchangeXmitReservedMin + $0F) shl 16);

  EDK_PROFILEUISTATE_ENCRYPTNETWORK = $4000;

implementation

end.

