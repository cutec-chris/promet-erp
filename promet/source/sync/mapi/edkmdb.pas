unit edkmdb;

interface

uses Windows, MAPIDefs, MapiTags,
     edkguid;

const

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
    function CreateStoreEntryID	(lpszMsgStoreDN : PChar;
				 lpszMailboxDN : PChar;
				 ulFlags : ULONG;
				 var lpcbEntryID : ULONG;
				 var lppEntryID : PENTRYID):HResult;stdcall;
    function EntryIDFromSourceKey(cFolderKeySize : ULONG;
                                  lpFolderSourceKey : pointer;
                                  cMessageKeySize : ULONG;
                                  lpMessageSourceKey : pointer;
                                  var lpcbEntryID : ULONG;
                                  var lppEntryID : PENTRYID):HResult; stdcall;
    function GetRights(cbUserEntryID : ULONG;
                       lpUserEntryID : PENTRYID;
                       cbEntryID : ULONG;
                       lpEntryID : PENTRYID;
                       var lpulRights : ULONG):HResult; stdcall;
    function GetMailboxTable(lpszServerName : PChar;
                             var lppTable : IMAPITABLE;
                             ulFlags : ULONG):HResult; stdcall;
    function GetPublicFolderTable(lpszServerName : PChar;
                                  var lppTable : IMAPITABLE;
                                  ulFlags : ULONG):HResult; stdcall;
  end;

  PROWENTRY = ^TROWENTRY;
  TROWENTRY = packed record
    ulRowFlags : ULONG;
    cValues : ULONG;
    rgPropVals : array[0..MAPI_DIM-1] of PSPropValue;
  end;

  PROWLIST = ^TROWLIST;
  TROWLIST = packed record
    cEntries : ULONG;
    aEntries : array[0..MAPI_DIM-1] of TROWENTRY;
  end;

  IExchangeModifyTable = interface(IUnknown)
  [strIID_IExchangeModifyTable]
    function GetLastError(ulFlags : ULONG; var LPMAPIERROR : PMAPIError):HResult;stdcall;
    function GetTable(ulFlags : ULONG; var LPMAPITABLE : IMAPITable):Hresult;stdcall;
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

// Bit values for PR_PROFILE_CONNECT_FLAGS

const	CONNECT_USE_ADMIN_PRIVILEGE			= 1;
const	CONNECT_NO_RPC_ENCRYPTION			= 2;

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

//const pbExchangeProviderPrimaryUserGuid	"\x54\x94\xA1\xC0\x29\x7F\x10\x1B\xA5\x87\x08\x00\x2B\x2A\x25\x17"
//const pbExchangeProviderDelegateGuid		"\x9e\xb4\x77\x00\x74\xe4\x11\xce\x8c\x5e\x00\xaa\x00\x42\x54\xe2"
//const pbExchangeProviderPublicGuid		"\x78\xb2\xfa\x70\xaf\xf7\x11\xcd\x9b\xc8\x00\xaa\x00\x2f\xc4\x5a"
//const pbExchangeProviderXportGuid			"\xa9\x06\x40\xe0\xd6\x93\x11\xcd\xaf\x95\x00\xaa\x00\x4a\x35\xc3"

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
const	PR_CREATOR_ENTRYID				 = (PT_BINARY or ( ULONG(pidExchangeXmitReservedMin+$19) shl 16));
const	PR_LAST_MODIFIER_NAME			 = (PT_TSTRING or ( ULONG(pidExchangeXmitReservedMin+$1A) shl 16));
const	PR_LAST_MODIFIER_ENTRYID		 = (PT_BINARY or ( ULONG(pidExchangeXmitReservedMin+$1B) shl 16));

// Read only or ( ULONG(appears on messages which have DAM's pointing to them
const PR_HAS_DAMS						= ( PT_BOOLEAN or ( ULONG(pidExchangeXmitReservedMin+$A) shl 16));
const PR_RULE_TRIGGER_HISTORY			= ( PT_BINARY or ( ULONG(pidExchangeXmitReservedMin+$12) shl 16));
const	PR_MOVE_TO_STORE_ENTRYID		= ( PT_BINARY or ( ULONG(pidExchangeXmitReservedMin+$13) shl 16));
const	PR_MOVE_TO_FOLDER_ENTRYID		= ( PT_BINARY or ( ULONG(pidExchangeXmitReservedMin+$14) shl 16));

// Read only or ( ULONG(available only on messages in the public store
const	PR_REPLICA_SERVER				 = (PT_TSTRING or ( ULONG(pidMessageReadOnlyMin+$4) shl 16));

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
const	PR_RULE_NAME					= ( PT_TSTRING or ( ULONG(pidSpecialMin+$12) shl 16));
const	PR_RULE_LEVEL					= ( PT_LONG or ( ULONG(pidSpecialMin+$13) shl 16));
const	PR_RULE_PROVIDER_DATA			= ( PT_BINARY or ( ULONG(pidSpecialMin+$14) shl 16));

//PR_STATE property values
const ST_DISABLED			= $0000;
const ST_ENABLED			= $0001;
const ST_ERROR			= $0002;
const ST_ONLY_WHEN_OOF	= $0004;
const ST_KEEP_OOF_HIST	= $0008;
const ST_EXIT_LEVEL		= $0010;

const ST_CLEAR_OOF_HIST	= $80000000;

//Empty restriction
const NULL_RESTRICTION	= $ff;

// special RELOP for Member of DL
const RELOP_MEMBER_OF_DL	= 100;

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


implementation

end.
