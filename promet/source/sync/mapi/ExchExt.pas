unit ExchExt;

interface

{$IFDEF CPU64}
  //64 bit - align at 8 bytes
  {$A8}
{$ELSE}
  //32 bit
  {$A+}
{$ENDIF }

uses
  SysUtils,
  Classes,
  Windows,
  ActiveX,
  Messages,
  //AxCtrls,
  ComObj,
  ComServ,
  //Commctrl,
  MAPIX,
  MAPIDefs,
  MAPIForm;

const

  EXCHEXT_UNICODE = $80000000;

type

  // Redefine Prop Sheet consts and types:
  // borrow the definition from D3 header file: Exchange Client
  // and OL 97 do not understand new page format in D4

  PPropSheetPage = ^TPropSheetPage;
  TFNPSPCallbackA = function(Wnd: HWnd; Msg: Integer; PPSP: PPropSheetPage): Integer stdcall;
  TPropSheetPage = record
    dwSize: Longint;
    dwFlags: Longint;
    hInstance: THandle;
    case Integer of
      0: (
	pszTemplate: PAnsiChar);
      1: (
	pResource: Pointer;
	case Integer of
	  0: (
	    hIcon: THandle);
	  1: (
	    pszIcon: PAnsiChar;
	    pszTitle: PAnsiChar;
	    pfnDlgProc: Pointer;
	    lParam: Longint;
	    pfnCallback: TFNPSPCallbackA;
	    pcRefParent: PInteger));
  end;


const
  MAXPROPPAGES = 100;

  PSP_DEFAULT             = $0000;
  PSP_DLGINDIRECT         = $0001;
  PSP_USEHICON            = $0002;
  PSP_USEICONID           = $0004;
  PSP_USETITLE            = $0008;

  PSP_HASHELP             = $0020;
  PSP_USEREFPARENT        = $0040;
  PSP_USECALLBACK         = $0080;

  PSPCB_RELEASE           = 1;
  PSPCB_CREATE            = 2;

  PSH_DEFAULT             = $0000;
  PSH_PROPTITLE           = $0001;
  PSH_USEHICON            = $0002;
  PSH_USEICONID           = $0004;
  PSH_PROPSHEETPAGE       = $0008;
  PSH_MULTILINETABS       = $0010;
  PSH_WIZARD              = $0020;
  PSH_USEPSTARTPAGE       = $0040;
  PSH_NOAPPLYNOW          = $0080;
  PSH_USECALLBACK         = $0100;
  PSH_HASHELP             = $0200;
  PSH_MODELESS            = $0400;

  PSCB_INITIALIZED  = 1;

  PSN_FIRST               = -200;
  PSN_LAST                = -299;

  PSN_SETACTIVE           = PSN_FIRST - 0;
  PSN_KILLACTIVE          = PSN_FIRST - 1;
  PSN_APPLY               = PSN_FIRST - 2;
  PSN_RESET               = PSN_FIRST - 3;
  PSN_HELP                = PSN_FIRST - 5;
  PSN_WIZBACK             = PSN_FIRST - 6;
  PSN_WIZNEXT             = PSN_FIRST - 7;
  PSN_WIZFINISH           = PSN_FIRST - 8;
  PSN_QUERYCANCEL         = PSN_FIRST - 9;

  PSNRET_NOERROR              = 0;
  PSNRET_INVALID              = 1;
  PSNRET_INVALID_NOCHANGEPAGE = 2;

  PSM_SETCURSEL           = WM_USER + 101;
  PSM_REMOVEPAGE          = WM_USER + 102;
  PSM_ADDPAGE             = WM_USER + 103;
  PSM_CHANGED             = WM_USER + 104;
  PSM_RESTARTWINDOWS      = WM_USER + 105;
  PSM_REBOOTSYSTEM        = WM_USER + 106;
  PSM_CANCELTOCLOSE       = WM_USER + 107;
  PSM_QUERYSIBLINGS       = WM_USER + 108;
  PSM_UNCHANGED           = WM_USER + 109;
  PSM_APPLY               = WM_USER + 110;
  PSM_SETTITLE            = WM_USER + 111;
  PSM_SETTITLEW           = WM_USER + 120;
  PSM_SETWIZBUTTONS       = WM_USER + 112;
  PSM_PRESSBUTTON         = WM_USER + 113;
  PSM_SETCURSELID         = WM_USER + 114;
  PSM_SETFINISHTEXT       = WM_USER + 115;
  PSM_SETFINISHTEXTW      = WM_USER + 121;
  PSM_GETTABCONTROL       = WM_USER + 116;
  PSM_ISDIALOGMESSAGE     = WM_USER + 117;

  PSWIZB_BACK             = $00000001;
  PSWIZB_NEXT             = $00000002;
  PSWIZB_FINISH           = $00000004;
  PSWIZB_DISABLEDFINISH   = $00000008;

  PSBTN_BACK              = 0;
  PSBTN_NEXT              = 1;
  PSBTN_FINISH            = 2;
  PSBTN_OK                = 3;
  PSBTN_APPLYNOW          = 4;
  PSBTN_CANCEL            = 5;
  PSBTN_HELP              = 6;
  PSBTN_MAX               = 6;

  ID_PSRESTARTWINDOWS     = 2;
  ID_PSREBOOTSYSTEM       = ID_PSRESTARTWINDOWS or 1;

  WIZ_CXDLG               = 276;
  WIZ_CYDLG               = 140;

  WIZ_CXBMP               = 80;

  WIZ_BODYX               = 92;
  WIZ_BODYCX              = 184;

  PROP_SM_CXDLG           = 212;
  PROP_SM_CYDLG           = 188;

  PROP_MED_CXDLG          = 227;
  PROP_MED_CYDLG          = 215;

  PROP_LG_CXDLG           = 252;
  PROP_LG_CYDLG           = 218;


type

//Exchange interfaces.

IExchExtAttachedFileEvents = interface(IUnknown)
  ['{00020D16-0000-0000-C000-000000000046}']
  function OnReadPattFromSzFile(lpAtt : IAttach; lpszFile : LPTSTR; ulFlags : ULONG):HResult;stdcall;
  function OnWritePattToSzFile(lpAtt : IAttach; lpszFile : LPTSTR; ulFlags : ULONG):HResult;stdcall;
  function OnQueryDisallowOpenPatt(lpAtt : IAttach):HResult;stdcall;
  function OnOpenPatt(lpAtt : IAttach):HResult;stdcall;
  function OnOpenSzFile(lpszFile : LPTSTR; ulFlags : ULONG):HResult;stdcall;
end;

IEXCHEXTMODELESS = interface(IUnknown)
  function TranslateAccelerator(var pmsg:TMsg):HResult;stdcall;
  function EnableModeless(hwnd : HWND; fEnable : BOOL):HResult;stdcall;
end;

IEXCHEXTMODELESSCALLBACK = interface(IUnknown)
  function EnableModeless(hwnd : HWND; fEnable : BOOL):HResult;stdcall;
  function AddWindow:HResult;stdcall;
  function ReleaseWindow:HResult;stdcall;
end;

PEECFHOOKPROC = ^TEECFHOOKPROC;
TEECFHOOKPROC = function (HWND, UINT, WPARAM, LPARAM : longint):longint;stdcall;

EXCHEXTCHOOSEFOLDER = record
  cbLength:UINT;
  hwnd:HWND;
  szCaption:PAnsiChar;
  szLabel:PAnsiChar;
  szHelpFile:PAnsiChar;
  ulHelpID:ULONG;
  hinst:HINST;
  uiDlgID:UINT;
  lpeecfhp:PEECFHOOKPROC;
  dwHookData:DWORD;
  ulFlags:ULONG;
  pmdb:IMsgStore;
  pfld:IMAPIFOLDER;
  szName:PAnsiChar;
  dwReserved1:DWORD;
  dwReserved2:DWORD;
  dwReserved3:DWORD;
end;

IOutlookExtCallback = interface(IUnknown)
  ['{0006720D-0000-0000-C000-000000000046}']
  function GetObject(out lppmp: IMAPIProp):HResult;stdcall;
  function GetOfficeCharacter(out ppmsotfc: pointer):HResult;stdcall;
end;

IExchExtCallback = interface(IUnknown)
  function GetVersion(var Version : longint; ulFlags : longint):HResult;stdcall;
  function GetWindow(var hwnd:HWND):HResult;stdcall;
  function GetMenu(var hmenu:HMenu):HResult;stdcall;
  function GetToolbar(tbid : longint;var lphwndTb:HWND):HResult;stdcall;
  function GetSession(var lppses : IMAPISession; var lppab : IADDRBOOK):HResult;stdcall;
  function GetObject(var lppmdb : IMsgStore;var lppmp: IMAPIProp):HResult;stdcall;
  function GetSelectionCount(var lpceid:longint):HResult;stdcall;
  function GetSelectionItem( ieid : longint; var lpcbeid:longint;
                             var lppeid : PENTRYID;  var lpulType:longint;
                             lpszMsgClass : PAnsiChar; cbMsgClass:longint;
                             var lpulMsgFlags : longint; ulFlags:longint):HResult;stdcall;
  function GetMenuPos(cmdid : UINT; var  lphmenu:HMenu; var lpmposMin:longint;
                      var lpmposMax:longint; ulFlags:longint):HResult;stdcall;
  function GetSharedExtsDir(lpszDir :PAnsiChar; cchDir:longint; var ulFlags:longint):HResult;stdcall;
  function GetRecipients( var lppal:TADRLIST):HResult;stdcall;
  function SetRecipients( var lppal:TADRLIST):HResult;stdcall;
  function GetNewMessageSite( fComposeInFolder : longint;var pfldFocus:IMAPIFolder;
                              var ppermsg: IPERSISTMESSAGE; var ppmsg:IMESSAGE;
                              var ppmms:IMAPIMESSAGESITE; var ppmvc:IMAPIVIEWCONTEXT;
                              ulFlags:longint):HResult;stdcall;
  function RegisterModeless(var peem:IEXCHEXTMODELESS;var ppeemcb:IEXCHEXTMODELESSCALLBACK):HResult;stdcall;
  function ChooseFolder(var peecf:EXCHEXTCHOOSEFOLDER):HResult;stdcall;
end;

LPTBENTRY = ^TBENTRY;
TBENTRY  = record
  hwnd : HWND;
  tbid : ULONG;
  ulFlags : ULONG;
  itbbBase : UINT;
end;

LPTBENTRY_Array = ^ TBENTRY_Array;
TBENTRY_Array = array[0..0] of TBENTRY;

const

EECMDID_FileSave      = 13;
EECMDID_FileSaveAs    = 14;


EECMDID_ToolsServices = 135;
EECMDID_ToolsOptions  = 136;
EECMDID_ComposeReplyToSender  = 154;


EETBID_STANDARD = 1;

type

  PTBButton = ^TTBButton;
  TTBButton = record
    iBitmap: Integer;
    idCommand: Integer;
    fsState: Byte;
    fsStyle: Byte;
    bReserved: array[1..2] of Byte;
    dwData: Longint;
    iString: Integer;
  end;

IExchExtCommands = interface(IUnknown)
  ['{00020D12-0000-0000-C000-000000000046}']
  function InstallCommands(lpeecb : IEXCHEXTCALLBACK;
                           hwnd : HWND; hmenu : HMENU;
                           var lpcmdidBase:UINT;
                           lptbeArray : LPTBENTRY_Array;
                           ctbe : UINT; ulFlags : ULONG):HRESULT;stdcall;
  procedure InitMenu(lpeecb : IEXCHEXTCALLBACK );stdcall;
  function DoCommand(lpeecb : IEXCHEXTCALLBACK; cmdid : UINT):HRESULT;stdcall;
  function Help(lpeecb : IEXCHEXTCALLBACK; cmdid : UINT):HRESULT;stdcall;
  function QueryHelpText(cmdid : UINT; ulFlags : ULONG; lpsz :PAnsiChar; cch : UINT) : HRESULT;stdcall;
  function QueryButtonInfo(tbid : ULONG; itbb : UINT; ptbb : PTBButton;
                           lpsz : PAnsiChar; cch : UINT; ulFlags :ULONG) : HRESULT;stdcall;
  function ResetToolbar(tbid : UINT; ulFlags :ULONG) : HRESULT;stdcall;
end;

IExchExtUserEvents = interface(IUnknown)
  ['{00020D13-0000-0000-C000-000000000046}']
  procedure OnSelectionChange(lpeecb : IEXCHEXTCALLBACK );stdcall;
  procedure OnObjectChange(lpeecb : IEXCHEXTCALLBACK );stdcall;
end;

const Class_IExchExt:TGUID='{00020D11-0000-0000-C000-000000000046}';

type
IExchExt = interface(IUnknown)
  ['{00020D11-0000-0000-C000-000000000046}']
  function Install(lpeecb : IExchExtCallback; eecontext : longint; ulFlags : longint):HRESULT;stdcall;
end;

const Class_IExchExtSessionEvents:TGUID='{00020D14-0000-0000-C000-000000000046}';
      IID_IOutlookExtCallback :TGUID='{0006720D-0000-0000-C000-000000000046}';

const
 // Flag values for IExchExtMessageEvents::OnXComplete
 EEME_FAILED          = $00000001;
 EEME_COMPLETE_FAILED = $00000002;
 // Extensibility contexts used with IExchExt::Install 
  EECONTEXT_SESSION = $00000001;
  EECONTEXT_VIEWER = $00000002;
  EECONTEXT_REMOTEVIEWER = $00000003;
  EECONTEXT_SEARCHVIEWER = $00000004;
  EECONTEXT_ADDRBOOK = $00000005;
  EECONTEXT_SENDNOTEMESSAGE = $00000006;
  EECONTEXT_READNOTEMESSAGE = $00000007;
  EECONTEXT_SENDPOSTMESSAGE = $00000008;
  EECONTEXT_READPOSTMESSAGE = $00000009;
  EECONTEXT_READREPORTMESSAGE = $0000000A;
  EECONTEXT_SENDRESENDMESSAGE = $0000000B;
  EECONTEXT_PROPERTYSHEETS = $0000000C;
  EECONTEXT_ADVANCEDCRITERIA = $0000000D;
  EECONTEXT_TASK = $0000000E;


type
 IExchExtMessageEvents = interface(IUnknown)
  ['{00020D15-0000-0000-C000-000000000046}']
  function OnRead(lpeecb : IExchExtCallback):HRESULT;stdcall;
  function OnReadComplete(lpeecb : IExchExtCallback; ulFlags:ULONG):HRESULT;stdcall;
  function OnWrite(lpeecb : IExchExtCallback):HRESULT;stdcall;
  function OnWriteComplete(lpeecb : IExchExtCallback; ulFlags:ULONG):HRESULT;stdcall;
  function OnCheckNames(lpeecb : IExchExtCallback):HRESULT;stdcall;
  function OnCheckNamesComplete(lpeecb : IExchExtCallback; ulFlags:ULONG):HRESULT;stdcall;
  function OnSubmit(lpeecb : IExchExtCallback):HRESULT;stdcall;
  procedure OnSubmitComplete(lpeecb : IExchExtCallback; ulFlags:ULONG);stdcall;
 end;

type
IExchExtSessionEvents = interface(IUnknown)
  ['{00020D14-0000-0000-C000-000000000046}']
  function OnDelivery(lpeecb : IEXCHEXTCALLBACK):HResult;stdcall;
end;

const

  // Flag values for IExchExtPropertySheets methods
  EEPS_MESSAGE = 1;
  EEPS_FOLDER  = 2;
  EEPS_STORE   = 3;
  EEPS_TOOLSOPTIONS = 4;
  EEPS_TOOLSOPTIONS_W = EEPS_TOOLSOPTIONS or EXCHEXT_UNICODE;

type
IExchExtPropertySheets = interface(IUnknown)
  ['{00020D17-0000-0000-C000-000000000046}']
  function GetMaxPageCount(ulFlags : ULONG):ULONG;stdcall;
  function GetPages(lpeecb : IEXCHEXTCALLBACK; ulFlags : ULONG;
                    lppsp : PPROPSHEETPAGE;  var lpcpsp : ULONG):HResult;stdcall;
  procedure FreePages(lppsp : PPROPSHEETPAGE; ulFlags : ULONG; cpsp : ULONG);stdcall;
end;

implementation

end.
