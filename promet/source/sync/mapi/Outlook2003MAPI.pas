unit Outlook2003MAPI;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

{$IFDEF CPUX64}
  {$DEFINE CPU64} //Delphi 2010
{$ENDIF}

{$IFDEF CPU64}
  //64 bit - align at 8 bytes
  {$A8}
{$ELSE}
  //32 bit
  {$A+}
{$ENDIF }

interface

uses //{$IFDEF FPC}LCLIntf{$ELSE}Windows{$ENDIF},
    Windows,
    SysUtils, {ComObj,} Registry, ActiveX,
    MapiDefs, MAPIX, MapiTags, MapiCode, edkguid, MAPIUtil;


{-------------------------------------------

              FreeBudy stuff

-------------------------------------------}

const

  IID_IEnumFBBlock : TGUID = '{00067064-0000-0000-C000-000000000046}';
  IID_IFreeBusyData : TGUID = '{00067066-0000-0000-C000-000000000046}';
  IID_IFreeBusySupport : TGUID = '{00067067-0000-0000-C000-000000000046}';

type

  FBStatus = (fbFree	= 0, fbTentative, fbBusy, fbOutOfOffice);

  FBBlock_1 = record
    m_tmStart : ULONG;
    m_tmEnd : ULONG;
    m_fbstatus : FBStatus;
  end;

  FBUser = record
    m_cbEid : ULONG;
    m_lpEid : PENTRYID;
    m_ulReserved : ULONG;
    m_pwszReserved : LPWSTR;
  end;

  IEnumFBBlock = interface(IUnknown)
  ['{00067064-0000-0000-C000-000000000046}']
    function Next( 
	celt : ULONG;
        var pblk : FBBlock_1;
        var pcfetch : ULONG):HResult; stdcall;
    function Skip(celt : ULONG):HResult; stdcall;
    function Reset : HResult; stdcall;
    function Clone(var ppclone : IEnumFBBlock):HResult; stdcall;
    function Restrict(
      ftmStart : FILETIME;
      ftmEnd : FILETIME):HResult; stdcall;
  end;

  IFreeBusyData = interface(IUnknown)
  ['{00067066-0000-0000-C000-000000000046}']
    function Placeholder1 : HResult; stdcall;
    function EnumBlocks(
      var ppenumfb : IEnumFBBlock;
      ftmStart : FILETIME;
      ftmEnd : FILETIME):HResult; stdcall;
    function Placeholder2 : HResult; stdcall;
    function Placeholder3 : HResult; stdcall;
    function Placeholder4 : HResult; stdcall;
    function Placeholder5 : HResult; stdcall;
    function SetFBRange(rtmStart : ULONG; rtmEnd : ULONG):HResult;stdcall;
    function Placeholder6 : HResult; stdcall;
    function GetFBPublishRange(var prtmStart : ULONG; var prtmEnd : ULONG): HResult; stdcall;
  end;

  IFreeBusySupport = interface(IUnknown)
  ['{00067067-0000-0000-C000-000000000046}']
    function Placeholder1 : HResult; stdcall;
    function Placeholder2 : HResult; stdcall;
    function LoadFreeBusyData(
      cMax : ULONG;
      var rgfbuser : FBUser;
      var prgfbdata : IFreeBusyData;
      var phrStatus :HRESULT;
      var pcRead : ULONG) : HResult;stdcall;
    function Placeholder23 : HResult; stdcall;
    function Placeholder3 : HResult; stdcall;
    function Placeholder4 : HResult; stdcall;
    function Placeholder5 : HResult; stdcall;
    function Placeholder6 : HResult; stdcall;
    function Placeholder7 : HResult; stdcall;
    function Placeholder8 : HResult; stdcall;
    function Placeholder9 : HResult; stdcall;
    function Placeholder10 : HResult; stdcall;
    function Placeholder11 : HResult; stdcall;
    function Placeholder12 : HResult; stdcall;
    function Placeholder13 : HResult; stdcall;
    function Placeholder14 : HResult; stdcall;
    function Placeholder15 : HResult; stdcall;
    function Placeholder16 : HResult; stdcall;
    function Placeholder17 : HResult; stdcall;
    function Placeholder18 : HResult; stdcall;
    function Placeholder19 : HResult; stdcall;
    function Placeholder20 : HResult; stdcall;
    function Placeholder21 : HResult; stdcall;
    function Placeholder22 : HResult; stdcall;
  end;

  ///function GetFBProviderForAddressType(const AddrType : AnsiString): IFreeBusySupport;

{-------------------------------------------

           Account Manager stuff

-------------------------------------------}

const

  ACCT_INIT_NOSYNCH_MAPI_ACCTS  = $00000001;
  E_ACCT_NOT_FOUND  = $800C8101;
  E_ACCT_WRONG_SORT_ORDER  = $800C8105;
  E_OLK_ALREADY_INITIALIZED   = $800C8002;
  E_OLK_NOT_INITIALIZED  = $800C8005;
  E_OLK_PARAM_NOT_SUPPORTED  = $800C8003;
  E_OLK_PROP_READ_ONLY   = $800C800D;
  //E_OLK_REGISTRY   = $800C8002;
  NOTIFY_ACCT_CHANGED = 1;
  NOTIFY_ACCT_CREATED = 2;
  NOTIFY_ACCT_DELETED = 3;
  NOTIFY_ACCT_ORDER_CHANGED = 4;
  NOTIFY_ACCT_PREDELETED = 5;
  OLK_ACCOUNT_NO_FLAGS = 0;

  E_ACCT_UI_BUSY = $800C8102;
  ACCTUI_NO_WARNING = $0100;
  ACCTUI_SHOW_DATA_TAB   = $0200;
  ACCTUI_SHOW_ACCTWIZARD = $0400;


  //Class Identifiers
  CLSID_OlkAccountManager : TGUID = '{ed475410-b0d6-11d2-8c3b-00104b2a6676}';
  CLSID_OlkPOP3Account : TGUID = '{ed475411-b0d6-11d2-8c3b-00104b2a6676}';
  CLSID_OlkIMAP4Account: TGUID = '{ed475412-b0d6-11d2-8c3b-00104b2a6676}';
  CLSID_OlkMAPIAccount: TGUID = '{ed475414-b0d6-11d2-8c3b-00104b2a6676}';
  CLSID_OlkMail: TGUID = '{ed475418-b0d6-11d2-8c3b-00104b2a6676}';
  CLSID_OlkAddressBook : TGUID = '{ed475419-b0d6-11d2-8c3b-00104b2a6676}';
  CLSID_OlkStore: TGUID = '{ed475420-b0d6-11d2-8c3b-00104b2a6676}';
  CLSID_OlkHotmailAccount: TGUID = '{4db5cbf0-3b77-4852-bc8e-bb81908861f3}';
  CLSID_OlkLDAPAccount: TGUID = '{4db5cbf2-3b77-4852-bc8e-bb81908861f3}';

  //Interface Identifiers
  IID_IOlkErrorUnknown: TGUID = '{9240A6C0-AF41-11d2-8C3B-00104B2A6676}';
  IID_IOlkEnum: TGUID = '{9240A6C1-AF41-11d2-8C3B-00104B2A6676}';
  IID_IOlkAccountNotify: TGUID = '{9240a6c3-af41-11d2-8c3b-00104b2a6676}';
  IID_IOlkAccountHelper: TGUID = '{9240a6cb-af41-11d2-8c3b-00104b2a6676}';
  IID_IOlkAccountManager: TGUID = '{9240a6cd-af41-11d2-8c3b-00104b2a6676}';
  IID_IOlkAccount: TGUID = '{9240a6d2-af41-11d2-8c3b-00104b2a6676}';

  PR_NEXT_SEND_ACCT = $0E29001E;
  PR_NEXT_SEND_ACCT_W = $0E29001F;

  PR_PRIMARY_SEND_ACCT = $0E28001E;
  PR_PRIMARY_SEND_ACCT_W = $0E28001F;

  PROP_ACCT_ID               = $00010003;
  PROP_ACCT_IS_EXCH          = $00140003;
  PROP_ACCT_NAME             = $0002001F;
  PROP_ACCT_SEND_STAMP       = $000E001F;
  PROP_ACCT_STAMP            = $000D001F;
  PROP_ACCT_USER_DISPLAY_NAME= $000B001F;

  //my own hacks
  PROP_ACCT_TYPE              = $0004001F; //"POP3/SMTP", "Exchange", "MAPI", "HTTP"
  PROP_ACCT_PROFILE_NAME      = $0007001F;  //"SLX-LAN"
  PROP_ACCT_MAPI_SERVICE_NAME = $0008001F;  //"MSEMS", "CONTAB" - missing for POP3/SMTP
  PROP_ACCT_ADDRESS           = $000C001F;       //"dmitry@dimastr.com", missing for Exchange
  //0 - use LAN
  //1 - use IE
  //2 - use phone line
  //3 - connect via modem
  //9 - connect using phone line
  PROP_ACCT_CONNECT_FLAGS     = $000F0003;
  PROP_ACCT_DIALUP_NAME       = $0010001F;
  //hack - POP3/IMAP4/HTTP
  PROP_ACCT_POP3_SERVER       = $0100001F;
  PROP_ACCT_POP3_NAME         = $0101001F;
  PROP_ACCT_POP3_REPLY_ADDRESS= $0103001F;       //"dmitry@dimastr.com", missing for Exchange
  PROP_ACCT_POP3_PORT         = $01040003;
  PROP_ACCT_POP3_USE_SSL      = $01050003; //0 - None, 1 - SSL, 2 - TLS, 3 - Auto
  PROP_ACCT_POP3_REMEMBER_PASSWORD = $01060003;  //1 - yes, 0 - no
  PROP_ACCT_POP3_ORGANIZATION      = $0107001F;
  PROP_ACCT_POP3_USE_SPA           = $01080003;  //1 - yes, 0 - no
  //hack - SMTP
  PROP_ACCT_SMTP_SERVER       = $0200001F;
  PROP_ACCT_SMTP_PORT         = $02010003;
  PROP_ACCT_SMTP_USE_SSL      = $02020003;
  PROP_ACCT_SMTP_USE_AUTH     = $02030003;
  PROP_ACCT_SMTP_NAME         = $0204001F;
  PROP_ACCT_SMTP_USE_SPA      = $02070003;  //1 - yes, 0 - no
  PROP_ACCT_SMTP_LOGON_KIND   = $02080003;  //0 - same as POP3, 1 - use SMTP specific login, 2 - log to POP3 first
  PROP_ACCT_TIMEOUT           = $02090003;

  PROP_ACCT_SMTP_ENCRYPTION_TYPE   = $020A0003;  //0 - None, 1 - SSL, 2 - TLS, 3 - Auto

  //upper 2 bytes  - x days to keep on the server
  //lower 2 bytes - or with the following bits:
  // 1 - leave messages on server
  // 2 - delete after x days (x is given in the upper 2 bytes)
  // 4 - delete from server when deleted from the Deleted Items fodler
  PROP_ACCT_FLAGS             = $10000003;

  //hack - IMAP
  PROP_ACCT_IMAP_ROOT_PATH    = $1101001F;
  PROP_ACCT_IMAP_SENTITEMS_STORE = $01090102;

  //hack - MAPI
  PROP_MAPI_SERVICE_UID  = $20000102; //corresponds to PR_SERVICE_UID in IMsgServiceAdmin::GetMsgServiceTable
  PROP_MAIL_USER_ENTRYID = $20020102; //corresponds PR_ENTRYID of the owner (IMailUser)

  //Delivery location
  //http://blogs.msdn.com/stephen_griffin/archive/2007/07/12/what-to-do-what-to-do.aspx
  PROP_ACCT_DELIVERY_STORE  = $00180102;
  PROP_ACCT_DELIVERY_FOLDER = $00190102;
  PROP_ACCT_SENTITEMS_EID   = $00200102;

type

  ACCT_BIN = record
    cb : DWORD;
    pb : pointer;
  end;

  ACCT_VARIANT = record
    dwType :DWORD;
    case DWORD of
      PT_LONG    : (dw :DWORD);
      PT_UNICODE : (pwsz : PWCHAR);
      PT_BINARY  : (bin : ACCT_BIN);
  end;

  IOlkErrorUnknown = interface(IUnknown)
  ['{9240A6C0-AF41-11d2-8C3B-00104B2A6676}']
    function GetLastError(hr : HRESULT;	var ppwszError : LPWSTR):HResult;stdcall;
  end;

  IOlkAccountHelper = interface(IUnknown)
  ['{9240a6cb-af41-11d2-8c3b-00104b2a6676}']
    function Placeholder1 : HResult; stdcall;
    function GetIdentity (
      pwszIdentity : LPWSTR;
      var pcch : DWORD):HResult;stdcall;
    function GetMapiSession(var ppmsess : IUnknown):HResult;stdcall;
    function HandsOffSession:HResult;stdcall;
  end;

  IOlkAccount = interface(IOlkErrorUnknown)
  ['{9240a6d2-af41-11d2-8c3b-00104b2a6676}']
    function Placeholder1 : HResult; stdcall;
    function Placeholder2 : HResult; stdcall;
    function Placeholder3 : HResult; stdcall;
    function Placeholder4 : HResult; stdcall;
    function Placeholder5 : HResult; stdcall;
    function Placeholder6 : HResult; stdcall;
    function GetAccountInfo(
      var pclsidType : TGUID;
      var pcCategories : DWORD;
      var prgclsidCategory : PGUID): HResult; stdcall;
    function GetProp(
      dwProp : DWORD;
      var pVar : ACCT_VARIANT): HResult; stdcall;
    function SetProp(
      dwProp : DWORD;
      var pVar : ACCT_VARIANT): HResult; stdcall;
    function Placeholder7 : HResult; stdcall;
    function Placeholder8 : HResult; stdcall;
    function Placeholder9 : HResult; stdcall;
    function FreeMemory (pv : pointer): HResult; stdcall;
    function Placeholder10 : HResult; stdcall;
    function SaveChanges (dwFlags : DWORD): HResult; stdcall;
  end;

  IOlkEnum = interface(IUnknown)
  ['{9240A6C1-AF41-11d2-8C3B-00104B2A6676}']
    function GetCount (var pulCount : DWORD): HResult; stdcall;
    function Reset : HResult; stdcall;
    function GetNext(var ppunk : IUnknown): HResult; stdcall;
    function Skip(cSkip : DWORD): HResult; stdcall;
  end;

  IOlkAccountNotify = interface(IOlkErrorUnknown)
  ['{9240a6c3-af41-11d2-8c3b-00104b2a6676}']
    function Notify( 
      dwNotify,
      dwAcctID,
      dwFlags : DWORD): HResult; stdcall;
  end;

  IOlkAccountManager = interface(IOlkErrorUnknown)
  ['{9240a6cd-af41-11d2-8c3b-00104b2a6676}']
    function Init(pAcctHelper : IOlkAccountHelper; dwFlags : DWORD):HResult;stdcall;
    function DisplayAccountList (
      hwnd : HWND;
      dwFlags : DWORD;
      lpwszReserved : LPWSTR;
      dwReserved : DWORD;
      pclsidReserved1,
      pclsidReserved2 : PGUID):HResult;stdcall;
    function Placeholder2 : HResult; stdcall;
    function Placeholder3 : HResult; stdcall;
    function Placeholder4 : HResult; stdcall;
    function Placeholder5 : HResult; stdcall;
    function Placeholder6 : HResult; stdcall;
    function FindAccount ( 
      dwProp : DWORD;
      var pVar : ACCT_VARIANT;
      var ppAccount : IOlkAccount): HResult; stdcall;
    function Placeholder7 : HResult; stdcall;
    function Placeholder8 : HResult; stdcall;
    function Placeholder9 : HResult; stdcall;
    function DeleteAccount (dwAcctID : DWORD): HResult; stdcall;
    function Placeholder10 : HResult; stdcall;
    function SaveChanges (
      dwAcctID : DWORD;
      dwFlags : DWORD): HResult; stdcall;
    function GetOrder (
      const pclsidCategory : TGUID;
      var pcAccts : DWORD;
      var prgAccts : PDWORD): HResult; stdcall;
    function SetOrder (
      const pclsidCategory : TGUID;
      {var }pcAccts : DWORD;
      var prgAccts : DWORD{var prgAccts : DWORD}{prgAccts : PDWORD}): HResult; stdcall;
    function EnumerateAccounts ( 
      pclsidCategory : PGUID;
      pclsidType : PGUID;
      dwFlags : DWORD;
	    out ppEnum : IOlkEnum): HResult; stdcall;
    function Placeholder11 : HResult; stdcall;
    function Placeholder12 : HResult; stdcall;
    function FreeMemory (pv : pointer): HResult; stdcall;
    function Advise (
      pNotify : IOlkAccountNotify;
      var pdwCookie : DWORD): HResult; stdcall;
    function Unadvise (
      var pdwCookie : DWORD): HResult; stdcall;
    function Placeholder13 : HResult; stdcall;
    function Placeholder14 : HResult; stdcall;
    function Placeholder15 : HResult; stdcall;
  end;

{-------------------------------------------

              Connection State stuff

-------------------------------------------}

const

  MAPIOFFLINE_ADVISE_DEFAULT  = 0;
  MAPIOFFLINE_UNADVISE_DEFAULT = 0;
  MAPIOFFLINE_ADVISE_TYPE_STATECHANGE = 1;
  MAPIOFFLINE_CALLBACK_TYPE_NOTIFY = 0;
  MAPIOFFLINE_CAPABILITY_OFFLINE = 1;
  MAPIOFFLINE_CAPABILITY_ONLINE = 2;
  MAPIOFFLINE_NOTIFY_TYPE_STATECHANGE_START = 1; //not documented
  MAPIOFFLINE_NOTIFY_TYPE_STATECHANGE = 2;
  MAPIOFFLINE_NOTIFY_TYPE_STATECHANGE_DONE = 3;

  MAPIOFFLINE_STATE_ALL = $003f037f;

  //Online or offline
  MAPIOFFLINE_STATE_OFFLINE_MASK  = 3;
  MAPIOFFLINE_STATE_OFFLINE  = 1;
  MAPIOFFLINE_STATE_ONLINE  = 2;

  MAPIOFFLINE_FLAG_BLOCK   = $00002000;
  MAPIOFFLINE_FLAG_DEFAULT = $00000000;


  GUID_GlobalState : TGUID = '{fbeffd93-b11f-4094-842b-96dcd31e63d1}';
  IID_IMAPIOffline : TGUID = '{000672B5-0000-0000-c000-000000000046}';
  IID_IMAPIOfflineNotify : TGUID = '{0317bde5-fc29-44cd-8dcd-36125a3be9ec}';
  IID_IMAPIOfflineMgr : TGUID = '{42175607-ff3e-4790-bc18-66c8643e6424}';

type

  //MAPIOFFLINE_CALLBACK_TYPE = (MAPIOFFLINE_CALLBACK_TYPE_NOTIFY);

  MAPIOFFLINE_ADVISEINFO = record
    ulSize : ULONG;
    ulClientToken : ULONG;
    CallbackType : ULONG; //MAPIOFFLINE_CALLBACK_TYPE;
    pCallback : IUnknown;
    ulAdviseTypes : ULONG;
    ulStateMask : ULONG;
  end;
  PMAPIOFFLINE_ADVISEINFO = ^MAPIOFFLINE_ADVISEINFO;

  MAPIOFFLINE_NOTIFY = record
    ulSize : ULONG;
    NotifyType : ULONG;//MAPIOFFLINE_NOTIFY_TYPE;  MAPIOFFLINE_ADVISE_TYPE_STATECHANGE only
    ulClientToken : ULONG;
    case integer of
      0 : (StateChange : record
             ulMask : ULONG;
             ulStateOld : ULONG;
             ulStateNew : ULONG;
           end;)
  end;

  IMAPIOffline = interface(IUnknown)
  ['{000672B5-0000-0000-C000-000000000046}']
    function SetCurrentState(
                ulFlags : ULONG;
                ulMask : ULONG;
                ulState : ULONG;
                pReserved : pointer) : HResult; stdcall;
    function GetCapabilities(var pulCapabilities : ULONG): HResult; stdcall;
    function GetCurrentState(out pulState : ULONG): HResult; stdcall;
    function Placeholder3 : HResult; stdcall;
  end;

  IMAPIOfflineMgr = interface(IMAPIOffline)
  ['{42175607-ff3e-4790-bc18-66c8643e6424}']
    function Advise(
      ulFlags : ULONG;
      var pAdviseInfo : MAPIOFFLINE_ADVISEINFO;
      var pulAdviseToken : ULONG): HResult; stdcall;
    function Unadvise(
      ulFlags : ULONG;
      ulAdviseToken : ULONG): HResult; stdcall;
    function Placeholder1 : HResult; stdcall;
    function Placeholder2 : HResult; stdcall;
    function Placeholder33: HResult; stdcall;
    function Placeholder4 : HResult; stdcall;
    function Placeholder5 : HResult; stdcall;
    function Placeholder6 : HResult; stdcall;
    function Placeholder7 : HResult; stdcall;
  end;

  IMAPIOfflineNotify = interface(IUnknown)
  ['{0317bde5-fc29-44cd-8dcd-36125a3be9ec}']
    function Notify(var pNotifyInfo : MAPIOFFLINE_NOTIFY): HResult; stdcall;
  end;

var

  HrOpenOfflineObj : function(
    ulReserved : ULONG;
    pszProfileNameIn : LPCWSTR;
    var pGUID : TGUID;
    pReserved : pointer;
    out ppOfflineObj : IMAPIOfflineMgr
  ) : HResult; stdcall = nil;

{-------------------------------------------

              MAPI-MIME Conversion API

-------------------------------------------}

const

  CCSF_SMTP        = $0002;
  CCSF_NOHEADERS   = $0004;
  CCSF_INCLUDE_BCC = $0020;
  CCSF_USE_RTF     = $0080;
  CCSF_NO_MSGID    = $4000;

  //http://blogs.msdn.com/stephen_griffin/archive/2007/07/20/more-iconverter-for-ya.aspx
  CCSF_USE_TNEF    = $0010; // the converter should embed TNEF in the MIME message
  CCSF_8BITHEADERS   = $0040; // the converter should allow 8 bit headers
  CCSF_PLAIN_TEXT_ONLY = $1000; // the converter should just send plain text


  //CCSF_RTF       = $0C10; //$0800 or $0400 or $0010 ???? - Eugene Kandrasheu. This will force winmail.dat
  CCSF_UNKNOWN1    = $0010; //"X-MS-TNEF-Correlator" header
  CCSF_UNKNOWN2    = $0400;
  CCSF_UNKNOWN3    = $0800;

  CLSID_IConverterSession : TGUID = '{4e3a7680-b77a-11d0-9da5-00c04fd65685}';
  IID_IConverterSession : TGUID = '{4b401570-b77b-11d0-9da5-00c04fd65685}';

  //ENCODINGTYPE
  IET_BINARY = 0;
  IET_BASE64 = 1;
  IET_UUENCODE = 2;
  IET_QP = 3;
  IET_7BIT = 4;
  IET_8BIT = 5;
  IET_INETCSET = 6;
  IET_UNICODE = 7;
  IET_RFC1522 = 8;
  IET_ENCODED = 9;
  IET_CURRENT = 10;
  IET_UNKNOWN = 11;
  IET_BINHEX40 = 12;
  IET_LAST = 13;

  SAVE_RFC822 = 0;
  SAVE_RFC1521 = 1;

    CSET_APPLY_UNTAGGED = 0;
    CSET_APPLY_ALL = 1;
    CSET_APPLY_TAG_ALL = 2;


type

  MIMESAVETYPE = ULONG;
  CSETAPPLYTYPE = ULONG; //one of the CSET_APPLY_xxx constants above


  IConverterSession = interface(IUnknown)
  ['{4b401570-b77b-11d0-9da5-00c04fd65685}']
    function SetAdrBook(AddrBook : IAddrBook) : HResult; stdcall; //Eugene Kandrasheu
    function SetEncoding (et : ULONG {ENCODINGTYPE}): HResult; stdcall;
    function Placeholder1 : HResult; stdcall;
    function MIMEToMAPI(
      pstm : ISTREAM;
      pmsg : MAPIDefs.IMessage;
      pszSrcSrv : LPCSTR;
      ulFlags : ULONG): HResult; stdcall;
    function MAPIToMIMEStm(
      pmsg : MAPIDEfs.IMessage;
      pstm : IStream;
      ulFlags : ULONG): HResult; stdcall;
    function Placeholder2 : HResult; stdcall;
    function Placeholder3 : HResult; stdcall;
    function Placeholder4 : HResult; stdcall;
    function SetTextWrapping(fWrapText : BOOL; ulWrapWidth : ULONG) : HResult; stdcall;
    function SetSaveFormat(mstSaveFormat : MIMESAVETYPE) : HResult; stdcall;
    function Placeholder7 : HResult; stdcall;
    function SetCharset(fApply : BOOL; hcharset : THandle{HCHARSET}; csetapplytype : CSETAPPLYTYPE) : HResult; stdcall;
  end;

{-------------------------------------------

             Data Degradation Layer API

-------------------------------------------}

//HrCreateNewWrapperObject is defined in MAPIUtil.pas

const
  DDLWRAP_FLAG_ANSI = $00000001;
  DDLWRAP_FLAG_UNICODE = $00000002;


{-------------------------------------------

             Replication API

-------------------------------------------}


const

  pbNSTGlobalProfileSectionGuid : array[0..15] of byte = ($85,$ED,$14,$23,$9D,$F7,$42,$66,$8B,$F2,$FB,$D4,$A5,$21,$29,$41);

  //Use the two interface identifiers below with IMAPIContainer::OpenEntry,
  //IMAPISession::OpenEntry, or IMsgStore::OpenEntry to open and ignore any provider check
  //on a folder object and a message object respectively.
  IID_IMAPIFolderNoProvChk : TGUID = '{57D333A0-F589-4b23-A3F9-85F82FEC153C}';
  IID_IMessageNoProvChk : TGUID = '{C3505457-7B2E-4c3b-A8D6-6DD949BB97A1}';

  DNH_OK   = $00010000;
  DNT_OK   = $00010000;
  HSF_LOCAL   = $00000008;
  HSF_COPYDESTRUCTIVE   = $00000010;
  HSF_OK   = $00010000;
  MNID_ID = 0;
  MNID_STRING = 1;
  SS_ACTIVE = 0;
  SS_SUSPENDED = 1;
  SYNC_UPLOAD_HIERARCHY  = $00000001;
  SYNC_DOWNLOAD_HIERARCHY   = $00000002;
  SYNC_UPLOAD_CONTENTS   = $00000040;
  SYNC_DOWNLOAD_CONTENTS   = $00000080;
  SYNC_OUTGOING_MAIL   = $00000200;
  SYNC_BACKGROUND   = $00001000;
  SYNC_THESE_FOLDERS   = $00020000;
  SYNC_HEADERS   = $02000000;
  UPC_OK   = $00010000;
  UPD_ASSOC  = $00000001;
  UPD_MOV  = $00000002;
  UPD_OK  = $00010000;
  UPD_MOVED  = $00020000;
  UPD_UPDATE  = $00040000;
  UPD_COMMIT  = $00080000;
  UPF_NEW  = $00000001;
  UPF_MOD_PARENT  = $00000002;
  UPF_MOD_PROPS  = $00000004;
  UPF_DEL  = $00000008;
  UPF_OK  = $00010000;
  UPH_OK   = $00010000;
  UPM_ASSOC  = $00000001;
  UPM_NEW  = $00000002;
  UPM_MOV  = $00000004;
  UPM_MOD_PROPS  = $00000008;
  UPM_HEADER  = $00000010;
  UPM_OK  = $00010000;
  UPM_MOVED  = $00020000 ;
  UPM_COMMIT  = $00040000;
  UPM_DELETE  = $00080000;
  UPM_SAVE  = $00100000;
  UPR_ASSOC  = $00000001;
  UPR_READ  = $00000002;
  UPR_OK  = $00010000;
  UPR_COMMIT  = $00020000;
  UPS_UPLOAD_ONLY  = $00000001;
  UPS_DNLOAD_ONLY  = $00000002;
  UPS_THESE_FOLDERS  = $00000080;
  UPS_OK  = $00010000;
  UPT_PUBLIC   = $00000001;
  UPT_OK   = $00010000;
  UPV_ERROR   = $00010000;
  UPV_DIRTY   = $00020000;
  UPV_COMMIT  = $00040000 ;

  IID_IPSTX  : TGUID = '{4FDEEFF0-0319-11CF-B4CF-00AA0DBBB6E6}';
  IID_IPSTX2 : TGUID = '{2067A790-2A45-11D1-EB86-00A0C90DCA6D}';
  IID_IPSTX3 : TGUID = '{55f15320-111b-11d2-a999-006008b05aa7}';
  IID_IPSTX4 : TGUID = '{aa2e2092-ac08-11d2-a2f9-0060b0ec3d4f}';
  IID_IPSTX5 : TGUID = '{55f15322-111b-11d2-a999-006008b05aa7}';
  IID_IPSTX6 : TGUID = '{55f15323-111b-11d2-a999-006008b05aa7}';
  IID_IOSTX  : TGUID = '{d2d85db4-840f-49b8-9982-07d2405ec6b7}';

type

  IOSTX = interface(IUnknown)
  ['{d2d85db4-840f-49b8-9982-07d2405ec6b7}']
  //todo
  end;

  IPSTX = interface(IUnknown)
  ['{4FDEEFF0-0319-11CF-B4CF-00AA0DBBB6E6}']
    function GetLastError(hResult : HRESULT; ulFlags : ULONG;	var lppMAPIError : PMAPIERROR): HResult; stdcall;
    function GetSyncObject(out ppostx : IOSTX): HResult; stdcall;
    function Placeholder1 : HResult; stdcall;
    function Placeholder2 : HResult; stdcall;
    function Placeholder3 : HResult; stdcall;
    function SetFolderInfo(cbEntryID : ULONG; pbEntryID : PENTRYID;	fReserved1 : BOOL; fReserved2 : BOOL): HResult; stdcall;
    function EmulateSpooler(lpMDB : IMsgStore{???}; fEmulate : BOOL): HResult; stdcall;
    function Placeholder4 : HResult; stdcall;
    function Placeholder5 : HResult; stdcall;
    function SwitchToOffline(fOffline : BOOL): HResult; stdcall;
    function Placeholder6 : HResult; stdcall;
    function Placeholder7 : HResult; stdcall;
  end;

const

  //MAPI properties for RPC over HTTP settings in Outlook 2003 - Q898835
  //Note The following MAPI properties are stored in the MAPI Global Profile Section.
  // New MAPI properties for RPC over HTTP

  // This is the main bitmask that holds most of the settings.
  PR_ROH_FLAGS                = $66230003;

  // This is the AnsiString for "Use this URL to connect to my proxy server for Exchange".
  PR_ROH_PROXY_SERVER           = $6622001E;
  PR_ROH_PROXY_SERVER_A         = $6622001E;
  PR_ROH_PROXY_SERVER_W         = $6622001F;

  // This is the AnsiString for "Principal name for proxy server".
  PR_ROH_PROXY_PRINCIPAL_NAME   = $6625001E;
  PR_ROH_PROXY_PRINCIPAL_NAME_A = $6625001E;
  PR_ROH_PROXY_PRINCIPAL_NAME_W = $6625001F;

  // This is the property for the Basic or NTLM authentication method.
  PR_ROH_PROXY_AUTH_SCHEME    = $66270003;

  // Flags that are used in PR_ROH_FLAGS
  // Connect to my Exchange mailbox by using HTTP.
  ROHFLAGS_USE_ROH                = $1;
  // Connect by using SSL only.
  ROHFLAGS_SSL_ONLY               = $2;
  // Mutually authenticate the session when connecting by using SSL.
  ROHFLAGS_MUTUAL_AUTH            = $4;
  // On fast networks, connect by using HTTP first. Then, connect by using TCP/IP.
  ROHFLAGS_HTTP_FIRST_ON_FAST     = $8;
  // On slow networks, connect by using HTTP first. Then, connect by using TCP/IP.
  ROHFLAGS_HTTP_FIRST_ON_SLOW     = $20;

  // Values that are used in PR_ROH_PROXY_AUTH_SCHEME
  // Basic authentication
  ROHAUTH_BASIC                   = 1;
  // NTLM authentication
  ROHAUTH_NTLM                    = 2;


//fast shutdown
const
  IID_IMAPIClientShutdown   : TGUID = '{00020397-0000-0000-C000-000000000046}';
  IID_IMAPIProviderShutdown : TGUID = '{00020398-0000-0000-C000-000000000046}';


type
  IMAPIClientShutdown = interface(IUnknown)
  ['{00020397-0000-0000-C000-000000000046}']
    function QueryFastShutdown : HResult;stdcall;
    function NotifyProcessShutdown : HResult;stdcall;
    function DoFastShutdown : HResult;stdcall;
  end;

  IMAPIProviderShutdown = interface(IUnknown)
  ['{00020398-0000-0000-C000-000000000046}']
    function QueryFastShutdown : HResult;stdcall;
    function NotifyProcessShutdown : HResult;stdcall;
    function DoFastShutdown : HResult;stdcall;
  end;


const
  IID_IMAPISync                 : TGUID = '{5024a385-2d44-486a-81a8-8f0ecb6071dd}';
  IID_IMAPISyncProgressCallback : TGUID = '{5024a386-2d44-486a-81a8-8f0ecb6071dd}';
  IID_IMAPISecureMessage        : TGUID = '{253cc320-eab6-11d0-8222-0060979387ea}';
  IID_IMAPIGetSession           : TGUID = '{614ab435-491d-4f5b-a8b4-60eb031030c6}';



{// IMAPISync
DEFINE_GUID(IID_IMAPISync, 0x5024a385, 0x2d44, 0x486a,  0x81, 0xa8, 0x8f, 0xe, 0xcb, 0x60, 0x71, 0xdd);

// IMAPISyncProgressCallback 
DEFINE_GUID(IID_IMAPISyncProgressCallback, 0x5024a386, 0x2d44, 0x486a,  0x81, 0xa8, 0x8f, 0xe, 0xcb, 0x60, 0x71, 0xdd);

// IMAPISecureMessage 
DEFINE_GUID(IID_IMAPISecureMessage, 0x253cc320, 0xeab6, 0x11d0, 0x82, 0x22, 0, 0x60, 0x97, 0x93, 0x87, 0xea);

// IMAPIGetSession 
DEFINE_GUID(IID_IMAPIGetSession, 0x614ab435, 0x491d, 0x4f5b, 0xa8, 0xb4, 0x60, 0xeb, 0x3, 0x10, 0x30, 0xc6);
}


type

  MAPISIB = record
           ulSize : ULONG;
           ulFlags : ULONG;
           psesSync : IMAPISESSION;
           punkCallBack : IUNKNOWN;
          phSyncDoneEvent : ^THANDLE;
  end;
  PMAPISIB = ^MAPISIB;


  IMAPISync = interface(IUnknown)
  ['{5024a385-2d44-486a-81a8-8f0ecb6071dd}']
    function SynchronizeInBackground (psibpb : PMAPISIB) : HResult;stdcall;
  end;

  IMAPISecureMessage = interface(IUnknown)
  ['{253cc320-eab6-11d0-8222-0060979387ea}']
    function Placeholder0 : HResult;stdcall;
    function Placeholder1 : HResult;stdcall;
    function Placeholder2 : HResult;stdcall;
    function Placeholder3 : HResult;stdcall;
    function Placeholder4 : HResult;stdcall;
    function GetBaseMessage(out ppmsg : MAPIDefs.IMessage) : HResult;stdcall;
    function Placeholder5 : HResult;stdcall;
    function Placeholder6 : HResult;stdcall;
    function Placeholder7 : HResult;stdcall;
  end;

const

  IID_IProxyStoreObject : TGUID = '{29F3AB10-554D-11D0-A97C-00A0C911F50A}';

type

  IProxyStoreObject = interface(IUnknown)
  ['{29F3AB10-554D-11D0-A97C-00A0C911F50A}']
    function Placeholder1 : HResult;stdcall;
    //does not AddRef the returned object
    function UnwrapNoRef(out ppvObject : pointer) : HResult;stdcall;
    function Placeholder2 : HResult;stdcall;
  end;

const
  IID_CAPONE_PROF : TGUID = '{00020D0A-0000-0000-C000-000000000046}';



implementation

(*function GetFBProviderForAddressType(const AddrType : AnsiString): IFreeBusySupport;
var Reg : TRegistry;
    i : integer;
    strKeyName : AnsiString;
    strClassGUID : WideString;
    CLSID : TGUID;
begin
  Result:=nil;
  Reg := TRegistry.Create(KEY_READ);
  try
    Reg.RootKey:=HKEY_LOCAL_MACHINE;
    for i:=20 downto 8 do begin //start with the highest version and work down
      strKeyName:=Format('SOFTWARE\Microsoft\Office\%s.0\Outlook\SchedulingInformation\FreeBusySupport', [IntToStr(i)]);
      if Reg.KeyExists(strKeyName) then begin
        if Reg.OpenKey(strKeyName, false) then begin
          if Reg.ValueExists(AddrType) then begin
            strClassGUID:=Reg.ReadString(AddrType);
            if strClassGUID <> '' then begin
              if Succeeded(CLSIDFromString(PWideChar(strClassGUID), CLSID)) then begin
                CoCreateInstance(CLSID, nil, CLSCTX_INPROC_SERVER or CLSCTX_LOCAL_SERVER, IFreeBusySupport, Result);
              end;
            end;
          end;
        end;
        Break;
      end;
    end;
  finally
    Reg.Free;
  end;
end; *)

initialization
  if MAPIDLLHandle <> 0 then begin
    HrOpenOfflineObj:=GetProcAddress(MAPIDLLHandle, 'HrOpenOfflineObj@20');
    if @HrOpenOfflineObj = nil then HrOpenOfflineObj:=GetProcAddress(MAPIDLLHandle, 'HrOpenOfflineObj');

  end;
end.
