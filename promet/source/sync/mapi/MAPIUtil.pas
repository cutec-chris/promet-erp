{

THIS FILE MUST BE THE LAST AMONG THE MAPI HEADERS IN THE "USES" CLAUSE
IF YOU WANT TO DYNAMICALLY LINK TO THE MAPI32.DLL !!!!

}

unit MAPIUtil;

interface
{$IFDEF FPC}
 {$MODE DELPHI}
{$ENDIF}
{$IFDEF CPU64}
  //64 bit - align at 8 bytes
  {$A8}
{$ELSE}
  //32 bit
  {$A+}
{$ENDIF }

uses MAPIDefs, Classes, Windows, SysUtils, ActiveX, MAPIX, MAPIGUID, IMessage, TNEF, Registry, MAPIForm;

const
  //Mapi32Dll = 'mapi32.dll';

  // RTF Sync Utilities
  RTF_SYNC_RTF_CHANGED  = $00000001;
  RTF_SYNC_BODY_CHANGED = $00000002;

  //
  STGSTRM_RESET   = 0;
  STGSTRM_CURRENT = $10000000;
  STGSTRM_MODIFY  = $00000002;
  STGSTRM_CREATE  = $00001000;


type

  ITableData = interface;

  TCallerRelease = procedure(ulCallerData : ULONG_PTR; lpTblData : ITableData; lpVue : IMAPITable);stdcall;

  ITableData = interface(IUnknown)
    [strIID_IMAPITableData]
    function HrGetView(lpSSortOrderSet : PSSortOrderSet; lpfCallerRelease : TCallerRelease;
                       uCallerData : ULONG_PTR; out lppMAPITable : IMAPITable):HResult;stdcall;
    function HrModifyRow(lpSRow : PSRow):HResult;stdcall;
    function HrDeleteRow(lpSPropValue : PSPropValue):HResult;stdcall;
    function HrQueryRow(lpSPropValue : PSPropValue; var lppSRow : PSRow; var lpuliRow : ULONG):HResult;stdcall;
    function HrEnumRow(ulRowNumber : ULONG; var lppSRow : PSRow):HResult;stdcall;
    function HrNotify(ulFlags : ULONG; cValues : ULONG; lpSPropValue : PSPropValue):HResult;stdcall;
    function HrInsertRow(uliRow : ULONG; lpSRow : PSRow):HResult;stdcall;
    function HrModifyRows(ulFlags : ULONG; lpSRowSet : PSRowSet):HResult;stdcall;
    function HrDeleteRows(ulFlags : ULONG; lprowsetToDelete : PSRowSet; var cRowsDeleted : ULONG):HResult;stdcall;
  end;

  PDTCTL = ^TDTCTL;
  TDTCTL = record
    ulCtlType : ULONG;
    ulCtlFlags : ULONG;
    lpbNotif : PBYTE;
    cbNotif : ULONG;
    lpszFilter : LPTSTR;
    ulItemID : ULONG;
    case integer of
      0 : ( lpv : pointer );
      1 : ( lplabel : PDTBLLABEL);
      2 : ( lpedit : PDTBLEDIT);
      3 : ( lplbx : PDTBLLBX);
      4 : ( lpcombobox : PDTBLCOMBOBOX);
      5 : ( lpddlbx : PDTBLDDLBX);
      6 : ( lpcheckbox : PDTBLCHECKBOX);
      7 : ( lpgroupbox : PDTBLGROUPBOX);
      8 : ( lpbutton : PDTBLBUTTON);
      9 : ( lpradiobutton : PDTBLRADIOBUTTON);
      10: ( lpmvlbx : PDTBLMVLISTBOX);
      11: ( lpmvddlbx : PDTBLMVDDLBX);
      12: ( lppage : PDTBLPAGE);
  end;

  PDTPage = ^TDTPage;
  TDTPage = record
    cctl : ULONG;
    lpszResourceName : LPTSTR;
    case integer of
      0 : (
        lpszComponent : LPTSTR;
          );
      1 : (
        ulItemID : ULONG;
        lpctl : PDTCTL;
          );
  end;

//*****************************************************
//        MAPIUtil function
//*****************************************************

TBuildDisplayTable = function(lpAllocateBuffer : PALLOCATEBUFFER;
                              lpAllocateMore : PALLOCATEMORE;
                              lpFreeBuffer : PFREEBUFFER;
                              lpMalloc : IMalloc;
                              HInstance : Longint;
                              cPages : UINT;
                              lpPage : PDTPage;
                              ulFlags : ULONG;
                              var lppTable : IMAPITable;
                              var lppTblData : ITableData):HResult;stdcall;

TCreateTable = function (const lpInterface: TGUID; lpAllocateBuffer : PALLOCATEBUFFER;
                         lpAllocateMore : PALLOCATEMORE; lpFreeBuffer : PFREEBUFFER;
                         lpvReserved : pointer; ulTableType : ULONG;
                         ulPropTagIndexColumn : ULONG;
                         lpSPropTagArrayColumns : PSPropTagArray;
                         out lppTableData : ITableData):HResult;stdcall;

THrQueryAllRows = function (ptable : IMAPITABLE;
                        ptaga:PSPropTagArray;
                        pres:PSRestriction;
                        psos:PSSortOrderSet;
                        crowsMax:longint;
                        var pprows:PSRowSet):HResult;stdcall;

TOpenStreamOnFile = function (lpAllocateBuffer:PALLOCATEBUFFER;
                          lpFreeBuffer:PFREEBUFFER;
                          ulFlags:ULONG;
                          lpszFileName:PAnsiChar;
                          lpszPrefix:PAnsiChar;
                          out Stream:IStream):HRESULT;stdcall;

TFreeProws = function (prows : PSRowSet):HResult; stdcall;

TFreePAdrList = function (prows : PAdrList):HResult; stdcall;

THrSzFromEntryID = function (cb : ULONG; pentry : PEntryID; var psz: PAnsiChar):HResult; stdcall;

THrGetOneProp = function (pmp : IMAPIPROP; ulPropTag:ULONG; out ppprop : PSPropValue):HResult; stdcall;

THrSetOneProp = function (pmp : IMAPIPROP; pprop : PSPropValue):HResult; stdcall;

TWrapCompressedRTFStream = function (lpCompressedRTFStream : IStream; ulflags : ULONG; out lpUncompressedRTFStream : IStream): HResult; stdcall;
TRTFSync = function(lpMessage : MAPIDefs.IMessage; ulFlags : ULONG; out lpfMessageUpdated : BOOL):HResult; stdcall;

THrEntryIDFromSz = function (sz : PAnsiChar; var pcb: ULONG; var ppentry : PENTRYID):HResult; stdcall;

TOpenTnefStreamEx = function (lpvSupport : pointer; lpStreamName : IStream;
                              lpszStreamName : PAnsiChar; ulFlags : ULONG; lpMessage : MAPIDefs.IMessage;
                              wKeyVal : ULONG {?}; lpAdressBook : IADDRBOOK; var lppTNEF : ITNEF) :HResult; stdcall;

TScCopyProps = function (cprop : integer; pgprop : PSPropValue; pvDest : pointer; var pcb : ULONG):HResult; stdcall;
TPropCopyMore = function(lpSPropValueDest, lpSPropValueSrc : PSPropValue; lpfAllocMore : PALLOCATEMORE; lpvObject : pointer):HResult;stdcall;
TScDupPropset = function(cprop : integer; rgprop : PSPropValue; lpAllocateBuffer : PALLOCATEBUFFER; var prgprop : PSPropValue):HResult;stdcall;

TMAPIInitIdle = function(lpvReserved : pointer):HResult; stdcall;
TMAPIDeinitIdle = procedure;stdcall;
TFtgRegisterIdleRoutine = function (lpfnIdle : pointer; lpvIdleParam : pointer;
            priIdle : integer; csecIdle : ULONG; iroIdle : ULONG):HResult; stdcall;
TDeregisterIdleRoutine  = procedure(ftg : ULONG);stdcall;

TWrapCompressedRTFStreamEx = function(lpCompressedRTFStream : IStream; const pWCSInfo : RTF_WCSINFO;
                                      out lppUncompressedRTFStream : IStream; out pRetInfo : RTF_WCSRETINFO):HResult;stdcall;


//*****************************************************
//        IMessage function
//*****************************************************

TOpenIMsgSession = function  (
  lpMalloc : IMalloc;                          { -> Co malloc object          }
  ulFlags : ULONG;                             { reserved. Must be zero.      }
  var lppMsgSess : PMSGSESS) : SCODE; stdcall; { <- message session object    }

TCloseIMsgSession = procedure  (
  lpMsgSess : PMSGSESS); stdcall;              { -> message session object    }

TOpenIMsgOnIStg = function  (
  lpMsgSess : PMSGSESS;                { -> message session obj (optional) }
  lpAllocateBuffer : PALLOCATEBUFFER;  { -> AllocateBuffer memory routine  }
  lpAllocateMore : PALLOCATEMORE;      { -> AllocateMore memory routine    }
  lpFreeBuffer : PFREEBUFFER;          { -> FreeBuffer memory routine      }
  lpMalloc : IMalloc;                  { -> Co malloc object               }
  lpMapiSup : Pointer;                 { -> MAPI Support Obj (optional)    }
  lpStg : IStorage;                    { -> open IStorage containing msg   }
  lpfMsgCallRelease : PMSGCALLRELEASE;{var lpfMsgCallRelease : TMSGCALLRELEASE;}{ -> release callback rtn (opt) }
  ulCallerData : ULONG;                { caller data returned in callback  }
  ulFlags : ULONG;                     { -> flags (controls istg commit)   }
  out lppMsg : MapiDefs.IMessage) : SCODE; stdcall;

TGetAttribIMsgOnIStg = function  (lpObject : Pointer;
  lpPropTagArray : PSPropTagArray;
  var lppPropAttrArray : PSPropAttrArray) : HResult; stdcall;

TSetAttribIMsgOnIStg = function  (lpObject : Pointer;
  lpPropTags : PSPropTagArray; lpPropAttrs : PSPropAttrArray;
  var lppPropProblems : PSPropProblemArray) : HResult; stdcall;

TMapStorageSCode = function  (StgSCode : SCODE) : SCODE; stdcall;

//*****************************************************
//        MAPIX function
//*****************************************************

TMAPIInitialize = function  (lpMapiInit : Pointer) : HResult; stdcall;

TMAPIUninitialize = procedure ; stdcall;

TMAPILogonEx = function  (ulUIParam : ULONG_PTR; lpszProfileName : PAnsiChar;
  lpszPassword : PAnsiChar; ulFlags : ULONG; {  ulFlags takes all that SimpleMAPI does + MAPI_UNICODE }
  out lppSession : IMAPISession) : HResult; stdcall;

TMAPIAllocateBuffer = function  (cbSize : ULONG; out lppBuffer : Pointer) : SCODE; stdcall;

TMAPIAllocateMore = function  (cbSize : ULONG; lpObject : Pointer; out lppBuffer : Pointer) : SCODE; stdcall;

TMAPIFreeBuffer = function  (lpBuffer : Pointer) : ULONG; stdcall;

TMAPIAdminProfiles = function  (ulFlags : ULONG;
  out lppProfAdmin : IProfAdmin) : HResult; stdcall;

TMAPIGetDefaultMalloc = function:pointer;stdcall;

THrIStorageFromStream = function (lpUnkIn : IUnknown;
                                  const lpInterface : TGUID;
                                  ulFlags : ULONG;
                                  var lppStorageOut : IStorage):HResult;stdcall;

TWrapStoreEntryID = function(ulFlags : ULONG; szDLLName : PAnsiChar; cbOrigEntry : ULONG;
                             lpOrigEntry : PEntryID; var lpcbWrappedEntry : ULONG;
                             var lppWrappedEntry : PEntryID):HResult;stdcall;

THrTextFromCompressedRTFStreamEx = function(pstmCompressed : IStream;
                                            var ppstmText : IStream;
                                            var psi : RTFSYNCINFO;
                                            cpid : ULONG;
                                            cAttach : ULONG;
                                            var rgAttachPos : ULONG) : HResult; stdcall;

TScCreateConversationIndex = function (cbParent : ULONG;
                                       lpbParent : pointer;
                                       var lpcbIndex : ULONG;
                                       var lppbIndex : pointer): HResult; stdcall;

THrCreateNewWrappedObject = function (pvUnwrapped : pointer;
                                      ulUnwrappedFlags : ULONG;
                                      ulWrappedFlags : ULONG;
                                      var pIID : tGUID;
                                      var pulReserved : ULONG;
                                      fCheckWrap : BOOL;
                                      var ppvWrapped : pointer): HResult; stdcall;

TMAPIOpenFormMgr = function  (pSession : IMAPISession; out ppmgr : IMAPIFormMgr) : HResult; stdcall;

TMAPIOpenLocalFormContainer = function  (out ppfcnt : IMAPIFormContainer) : HResult; stdcall;

THrValidateIPMSubtree = function(lpMDB : IMsgStore; ulFlags : ULONG; var lpcValues : ULONG; var lppProps : PSPropValue; var lppMapiError : PMAPIERROR) : HResult; stdcall;

THrDispatchNotifications = function(ulFlags : ULONG):HResult; stdcall;

THrGetAutoDiscoverXML = function(pwzAddress : PWideChar;
                                 pwzPassword : PWideChar;
                                 hCancelEvent : THandle;
                                 ulFlags : ULONG;
                                 out ppXmlStream : IStream):HResult; stdcall;


var HrQueryAllRows:THrQueryAllRows = nil;
    OpenStreamOnFile:TOpenStreamOnFile = nil;
    FreeProws:TFreeProws = nil;
    FreePAdrList:TFreePAdrList = nil;
    HrSzFromEntryID:THrSzFromEntryID = nil;
    HrEntryIDFromSz:THrEntryIDFromSz = nil;
    HrGetOneProp:THrGetOneProp = nil;
    HrSetOneProp:THrSetOneProp = nil;
    WrapCompressedRTFStream:TWrapCompressedRTFStream = nil;
    WrapCompressedRTFStreamEx : TWrapCompressedRTFStreamEx = nil;
    RTFSync:TRTFSync = nil;
    CreateTable : TCreateTable = nil;
    BuildDisplayTable : TBuildDisplayTable = nil;

    OpenIMsgSession:TOpenIMsgSession = nil;
    CloseIMsgSession:TCloseIMsgSession= nil;
    OpenIMsgOnIStg:TOpenIMsgOnIStg= nil;
    GetAttribIMsgOnIStg:TGetAttribIMsgOnIStg= nil;
    SetAttribIMsgOnIStg:TSetAttribIMsgOnIStg= nil;
    MapStorageSCode:TMapStorageSCode= nil;

    MAPIInitialize:TMAPIInitialize = nil;
    MAPIUninitialize:TMAPIUninitialize = nil;
    MAPILogonEx:TMAPILogonEx = nil;
    MAPIAllocateBuffer:TMAPIAllocateBuffer = nil;
    MAPIAllocateMore:TMAPIAllocateMore = nil;
    MAPIFreeBuffer:TMAPIFreeBuffer = nil;
    MAPIAdminProfiles:TMAPIAdminProfiles = nil;
    MAPIGetDefaultMalloc:TMAPIGetDefaultMalloc = nil;
    OpenTnefStreamEx:TOpenTnefStreamEx = nil;

    MAPIInitIdle : TMAPIInitIdle= nil;
    MAPIDeinitIdle : TMAPIDeinitIdle = nil;
    FtgRegisterIdleRoutine :TFtgRegisterIdleRoutine = nil;
    DeregisterIdleRoutine : TDeregisterIdleRoutine = nil;

    HrIStorageFromStream  : THrIStorageFromStream = nil;

    WrapStoreEntryID : TWrapStoreEntryID = nil;

    ScCopyProps : TScCopyProps = nil;
    PropCopyMore : TPropCopyMore = nil;
    ScDupPropset : TScDupPropset = nil;

    HrTextFromCompressedRTFStreamEx : THrTextFromCompressedRTFStreamEx = nil;

    ScCreateConversationIndex : TScCreateConversationIndex = nil;

    HrCreateNewWrappedObject : THrCreateNewWrappedObject = nil;

    MAPIOpenFormMgr : TMAPIOpenFormMgr = nil;
    MAPIOpenLocalFormContainer : TMAPIOpenLocalFormContainer = nil;

    HrValidateIPMSubtree : THrValidateIPMSubtree = nil;
    HrDispatchNotifications : THrDispatchNotifications = nil;
    HrGetAutoDiscoverXML : THrGetAutoDiscoverXML = nil;


const
     IRONULL          = $0000;
     FIROWAIT         = $0001;
     FIROINTERVAL     = $0002;
     FIROPERBLOCK     = $0004;
     FIRODISABLED     = $0020;
     FIROONCEONLY     = $0040;

var MAPIDLLHandle:THandle;
    IsBadMAPIDll : boolean = false;

implementation


function GetCOMObjPath(ClassName : WideString):AnsiString;
var CLSID : TGUID;
    Reg : TRegistry;
begin
  Result:='';
  if S_OK = CLSIDFromProgID(PWideChar(ClassName), CLSID) then begin
    Reg:=TRegistry.Create(KEY_READ);
    try
      Reg.RootKey:=HKEY_CLASSES_ROOT;
      if Reg.OpenKey('CLSID\'+GUIDToString(CLSID)+'\LocalServer32', false) then begin
        Result:=Reg.ReadString('');
        Reg.CloseKey;
      end;
    finally
      Reg.Free;
    end;
  end;
end;

procedure GetBuildInfo(const FileName : AnsiString; var V1, V2, V3, V4: DWord);
var
   VerInfoSize,
   VerValueSize,
   Dummy       : DWORD;
   VerInfo	   : Pointer;
   VerValue	   : PVSFixedFileInfo;
begin
  V1:=0; V2:=0; V3:=0; V4:=0;
  VerInfoSize := GetFileVersionInfoSizeA(PAnsiChar(FileName), Dummy);
  if VerInfoSize > 0 then begin
    GetMem(VerInfo, VerInfoSize);
    GetFileVersionInfoA(PAnsiChar(FileName), 0, VerInfoSize, VerInfo);
    if VerQueryValue(VerInfo, '\', Pointer(VerValue), VerValueSize) then begin
      with VerValue^ do begin
        V1 := dwFileVersionMS shr 16;
        V2 := dwFileVersionMS and $FFFF;
        V3 := dwFileVersionLS shr 16;
        V4 := dwFileVersionLS and $FFFF;
      end;
    end;
    FreeMem(VerInfo, VerInfoSize);
  end;
end;


function GetOutlookVersion(out V1, V2, V3, V4 : DWORD):LongBool;stdcall;
var Reg:TRegistry;
    strOLPath:AnsiString;
begin
  V1:=0;
  V2:=0;
  V3:=0;
  V4:=0;
  Result:=false;
  strOLPath:='';
  //try the registry
  Reg:=TRegistry.Create(KEY_READ);
  try
    Reg.RootKey:=HKEY_LOCAL_MACHINE;
    if Reg.OpenKey('Software\Microsoft\Windows\CurrentVersion\App Paths\OUTLOOK.EXE', false) then begin
      strOLPath:=Reg.ReadString('');
      Reg.CloseKey;
    end;
    try
    finally
      Reg.Free;
    end;
  except
  end;
  //try the location of the Outlook.Application COM object
  if strOLPath = '' then begin
    strOLPath:=GetCOMObjPath('Outlook.Application');
    strOLPath:=ExtractFileDir(strOLPath);
  end;
  //get the version info
  if (Length(strOLPath) > 0) and FileExists(strOLPath) then begin
    GetBuildInfo(strOLPath, V1, V2, V3, V4);
    Result:=true;
  end;
end;




//*****************************************************************
//
//   GetMAPIDir - returns the directory where mapisvc.inf file is located
//
//*****************************************************************

type TFGETCOMPONENTPATH  = function(
        szComponent : LPSTR;
        szQualifier : LPSTR;
        szDllPath : LPSTR;
        cchBufferSize : DWORD;
        fInstall : BOOL):BOOL; stdcall;

function GetMAPIDllPath : AnsiString;
const s_szMSIApplicationLCID : AnsiString = 'Microsoft\Office\%s\Outlook'#0'LastUILanguage'#0;  //DNL
      s_szMSIOfficeLCID      : AnsiString = 'Microsoft\Office\%s\Common\LanguageResources'#0'UILanguage'#0'InstallLanguage'#0;  //DNL

var hlib : THandle;
    FGETCOMPONENTPATH : TFGETCOMPONENTPATH;
    szPrivateMAPIDir, strOutlookPath : AnsiString;
    strVersion : AnsiString;
    V1, V2, V3, V4 : DWORD;
begin
  Result:='';
  //Outlook 2007 and up - find the office path and see if olmapi323.dll exists
  if GetOutlookVersion(V1, V2, V3, V4) and (V1 >= 12) then begin
    strOutlookPath:=GetCOMObjPath('Outlook.Application');
    if strOutlookPath <> '' then begin
      strOutlookPath:=ExtractFileDir(strOutlookPath)+'\olmapi32.dll';
      if FileExists(strOutlookPath) then Result:=strOutlookPath;
    end;
  end; 

  if Result = '' then begin
    //default value - system directory
    SetLength(Result, MAX_PATH);
    GetSystemDirectoryA(PAnsiChar(Result), MAX_PATH);
    Result:=PAnsiChar(Result);
    hlib:=LoadLibrary('mapistub.dll');
    if hlib = 0 then hlib:=LoadLibrary('mapi32.dll');
    if hlib <> 0 then begin
      @FGETCOMPONENTPATH:=GetProcAddress(hlib, 'FGetComponentPath');  
      if @FGETCOMPONENTPATH <> nil then begin
        //get the right version number
        strVersion:='9.0';
        if GetOutlookVersion(V1, V2, V3, V4) then begin
          if V1 >= 9 then begin
            strVersion:=IntToStr(V1)+'.'+IntToStr(V2);
            s_szMSIApplicationLCID:=Format(s_szMSIApplicationLCID, [strVersion]);
            s_szMSIOfficeLCID     :=Format(s_szMSIOfficeLCID, [strVersion]);
            //read the directory
            {$IFDEF DEBUG}
            OutputDebugString('Will now call FGETCOMPONENTPATH');
            {$ENDIF}
            SetLength(szPrivateMAPIDir, MAX_PATH);
            if (FGETCOMPONENTPATH('{FF1D0740-D227-11D1-A4B0-006008AF820E}',
                        PAnsiChar(s_szMSIApplicationLCID), PAnsiChar(szPrivateMAPIDir), MAX_PATH, TRUE) or
                FGETCOMPONENTPATH('{FF1D0740-D227-11D1-A4B0-006008AF820E}',
                        PAnsiChar(s_szMSIOfficeLCID), PAnsiChar(szPrivateMAPIDir), MAX_PATH, TRUE) or
                FGETCOMPONENTPATH('{FF1D0740-D227-11D1-A4B0-006008AF820E}',
                        nil, PAnsiChar(szPrivateMAPIDir), MAX_PATH, TRUE))
                and
                ((Length(szPrivateMAPIDir) > 0) and (szPrivateMAPIDir[1] <> #0))
            then begin
              Result:=PAnsiChar(szPrivateMAPIDir);
            end;
            {$IFDEF DEBUG}
            OutputDebugString('Called FGETCOMPONENTPATH');
            {$ENDIF}
          end;
        end;
      end;
      FreeLibrary(hlib);
    end;
  end;
end;

function GetMAPIDir : AnsiString;
begin
  Result:=ExtractFileDir(GetMAPIDllPath);
  if (Length(Result) > 0) and (Result[length(Result)] = '\')  //DNL
    then Delete(Result, Length(Result), 1);
end;

function IsIMO : boolean;
var V1, V2, V3, V4 : DWORD;
    Reg:TRegistry;
begin
  Result:=false;
  try
    if GetOutlookVersion(V1, V2, V3, V4) then begin
      if (V1 = 9) or ((V1 = 8) and (V2 = 5)) then begin //Outlook 2000 or 98
        Reg:=TRegistry.Create(KEY_READ);
        try
          Reg.RootKey:=HKEY_LOCAL_MACHINE;
          if Reg.OpenKey('Software\Microsoft\Office\9.0\Outlook\Setup', false) or
             Reg.OpenKey('Software\Microsoft\Office\8.0\Outlook\Setup', false)
          then begin
            if Reg.ValueExists('MailSupport') then begin
              Result:=(Reg.ReadInteger('MailSupport') = 0);
            end;
            Reg.CloseKey;
          end;
        finally
          Reg.Free;
        end;
      end;
    end;
  except
    Result:=false;;
  end;
end;

//returns th numbre off profiles for the current user
function GetNumProfiles : integer;
var Profiles : TStringList;
begin
  Result := 0;
  with TRegistry.Create(KEY_READ) do begin
    try
      Profiles := TStringList.Create;
      RootKey := HKEY_CURRENT_USER;
      if OpenKeyReadOnly('Software\Microsoft\Windows NT\CurrentVersion\Windows Messaging Subsystem\Profiles') then begin
        GetKeyNames(profiles);
        Result := Profiles.Count;
      end;
    finally
      Free;
      Profiles.Free;
    end;
  end;
  {$IFDEF DEBUG}
  OutputDebugStringA(PAnsiChar(AnsiString('Number of profiles for the current user: '+IntToStr(Result))));
  {$ENDIF}
end;


var DLLPath : AnsiString;
    Key:HKey;
    cb:DWORD;
    rType:DWORD;
    Buffer : array[0..MAX_PATH] of AnsiChar;
    Dir, TempStr : AnsiString;
    DummyMAPI32Handle : THandle;
initialization
  MAPIDLLHandle:=0;
  DummyMAPI32Handle:=0;
  //see if msmapi32.dll / olmapi32.dll / exmapi32.dll
  //has already been loaded and use it.
  //the exact path to msmapi32.dll depends on the currently
  //selected language, that's why we need to get the path to the
  //currently loaded version of msmapi32.dll

  //Outlook 2007 and up uses olmapi32.dll
  MAPIDLLHandle:=GetModuleHandle('olmapi32.dll');
  {$IFDEF DEBUG}
  if MAPIDLLHandle = 0
    then OutputDebugString('olmapi32.dll is not yet loaded')
    else OutputDebugString('olmapi32.dll is already loaded');
  {$ENDIF}
  //Ootlook 2003 and below - check for msmapi32.dll
  if MAPIDLLHandle = 0 then begin
    MAPIDLLHandle:=GetModuleHandle('msmapi32.dll');
    {$IFDEF DEBUG}
    if MAPIDLLHandle = 0
      then OutputDebugString('msmapi32.dll is not yet loaded')
      else OutputDebugString('msmapi32.dll is already loaded');
    {$ENDIF}
  end;
  //new Exchange version of MAPI
  if MAPIDLLHandle = 0 then begin
    MAPIDLLHandle:=GetModuleHandle('exmapi32.dll');
    {$IFDEF DEBUG}
    if MAPIDLLHandle = 0
      then OutputDebugString('exmapi32.dll is not yet loaded')
      else OutputDebugString('exmapi32.dll is already loaded');
    {$ENDIF}
  end;
  //
  if MAPIDLLHandle <> 0 then begin
    {$IFDEF DEBUG}
    OutputDebugString('MAPI is already loaded, figure out the dll path');
    {$ENDIF}
    if GetModuleFileNameA(MAPIDLLHandle, Buffer, SizeOf(Buffer)) <> 0 then begin
      DLLPath:=PAnsiChar(@Buffer);
      {$IFDEF DEBUG}
      TempStr := AnsiString('MAPI dll is at: ')+DLLPath;
      OutputDebugStringA(PAnsiChar(TempStr));
      {$ENDIF}
      MAPIDLLHandle:=LoadLibraryA(PAnsiChar(DLLPath));
      //MAPI bug: GAL provider in the standalone version of MAPI
      //expects mapi32.dll (stub) to be loaded in the current process
      if CompareText('exmapi32.dll', ExtractFileName(DLLPath)) = 0 then begin
        DummyMAPI32Handle := LoadLibrary('mapi32.dll');
      end;
    end
    else begin
      MAPIDLLHandle:=0;
    end;
  end;

  //is mapi32.dll already loaded?
  //it is possible that mapi32.dll is used rather than msmapi32.dll
  //in the server environment (no Outlook)
  if (MAPIDLLHandle = 0) then begin
    MAPIDLLHandle:=GetModuleHandle('mapi32.dll');
    {$IFDEF DEBUG}
    if MAPIDLLHandle = 0
      then OutputDebugString('mapi32.dll is not yet loaded')
      else OutputDebugString('mapi32.dll is already loaded');
    {$ENDIF}
    if MAPIDLLHandle <> 0 then begin
      if GetModuleFileNameA(MAPIDLLHandle, Buffer, SizeOf(Buffer)) <> 0 then begin
        DLLPath:=PAnsiChar(@Buffer);
        {$IFDEF DEBUG}
        TempStr := 'MAPI dll is at: '+DLLPath;
        OutputDebugStringA(PAnsiChar(AnsiString(TempStr)));
        {$ENDIF}
        MAPIDLLHandle:=LoadLibraryA(PAnsiChar(DLLPath));
      end
      else begin
        MAPIDLLHandle:=0;
      end;
    end;
  end;
  if (MAPIDLLHandle = 0) and
     (not IsIMO) and      //IMO mode brings the installer window when FGETCOMPONENTPATH is called
     (GetNumProfiles > 0) //if there are no profiles, FGETCOMPONENTPATH will prompt the user to configure
  then begin
    //use FGETCOMPONENTPATH function which takes into account LCID
    DLLPath:=GetMAPIDllPath;
    if DLLPath <> '' then begin
      {$IFDEF DEBUG}
      TempStr := 'Called FGETCOMPONENTPATH to retrive MAPI path: '+DLLPath;
      OutputDebugStringA(PAnsiChar(AnsiString(TempStr)));
      {$ENDIF}
      Dir:=GetCurrentDir;
      //temporarily change the current dir
      SetCurrentDir(ExtractFileDir(DLLPath));
      //load the mapi dll
      MAPIDLLHandle:=LoadLibraryA(PAnsiChar(DLLPath));
      //check mismatched bitness (32 vs 64)
      if (MAPIDLLHandle = 0) and (GetLastError = ERROR_BAD_EXE_FORMAT) then IsBadMAPIDll := true;
      //restore the file dir
      SetCurrentDir(Dir);
    end;
  end;
  if MAPIDLLHandle = 0 then begin
    //see if Exchange version of MAPI is installed
    {$IFDEF DEBUG}
    OutputDebugStringA(PAnsiChar(AnsiString('Will try to load the Exchange version of MAPI')));
    {$ENDIF}
    cb:=MAX_PATH;
    Setlength(DLLPath, MAX_PATH);
    if ERROR_SUCCESS = RegOpenKeyExA(HKEY_LOCAL_MACHINE, 'SOFTWARE\Clients\Mail\ExchangeMAPI',0, KEY_READ, Key) then begin
      {$IFDEF DEBUG}
      OutputDebugStringA(PAnsiChar(AnsiString('Successfully opened HKEY_LOCAL_MACHINE\SOFTWARE\Clients\Mail\ExchangeMAPI')));
      {$ENDIF}
      rType := REG_NONE;
      if ERROR_SUCCESS = RegQueryValueExA(Key, 'DllPathEx', nil, @rType, Windows.PByte(PAnsiChar(DLLPath)), @cb) then begin
        {$IFDEF DEBUG}
        OutputDebugStringA(PAnsiChar(AnsiString('Successfully opened DllPathEx key')));
        {$ENDIF}
        if rType = REG_SZ then begin
          SetLength(DLLPath, strlen(PAnsiChar(DLLPath)));
          if Length(DLLPath) > 0 then begin
            {$IFDEF DEBUG}
            TempStr := 'Exchange version of MAPI is at: '+DLLPath;
            OutputDebugStringA(PAnsiChar(AnsiString(TempStr)));
            {$ENDIF}
            Dir:=GetCurrentDir;
            //temporarily change the current dir
            SetCurrentDir(ExtractFileDir(DLLPath));
            //load the mapi dll
            MAPIDLLHandle:=LoadLibraryA(PAnsiChar(DLLPath));
            //MAPI bug: GAL provider expects mapi32.dll (stub) to be loaded in the current process
            DummyMAPI32Handle := LoadLibrary('mapi32.dll');
            //restore the file dir
            SetCurrentDir(Dir);
          end;
        end
        else begin
          {$IFDEF DEBUG}
          OutputDebugStringA(PAnsiChar(AnsiString('Unexpected DllPathEx type')));
          {$ENDIF}
        end;
      end;
      RegCloseKey(Key);
    end
    else begin
      {$IFDEF DEBUG}
      OutputDebugStringA(PAnsiChar(AnsiString('Exchange version of MAPI is not installed')));
      {$ENDIF}
    end;
  end;
  if MAPIDLLHandle = 0 then begin
    //first try to read the path to the MAPI dll from the registry
    // HKEY_LOCAL_MACHINE\SOFTWARE\Clients\Mail\Microsoft Outlook
    cb:=MAX_PATH;
    Setlength(DLLPath, MAX_PATH);
    if ERROR_SUCCESS = RegOpenKeyExA(HKEY_LOCAL_MACHINE, 'SOFTWARE\Clients\Mail\Microsoft Outlook',0, KEY_READ, Key) then begin
      {$IFDEF DEBUG}
      OutputDebugStringA(PAnsiChar(AnsiString('Successfully opened HKEY_LOCAL_MACHINE\SOFTWARE\Clients\Mail\Microsoft Outlook')));
      {$ENDIF}
      if ERROR_SUCCESS = RegQueryValueExA(Key,'DllPathEx',nil, @rType, Windows.PByte(PAnsiChar(DLLPath)), @cb) then begin
        {$IFDEF DEBUG}
        OutputDebugStringA(PAnsiChar(AnsiString('Successfully opened DllPathEx key')));
        {$ENDIF}
        if rType = REG_SZ then begin
          SetLength(DLLPath, strlen(PAnsiChar(DLLPath)));
          if Length(DLLPath) > 0 then begin
            {$IFDEF DEBUG}
            TempStr := 'Outlook version of MAPI is at: '+DLLPath;
            OutputDebugStringA(PAnsiChar(AnsiString(TempStr)));
            {$ENDIF}
            Dir:=GetCurrentDir;
            //temporarily change the current dir
            SetCurrentDir(ExtractFileDir(DLLPath));
            //load the mapi dll
            MAPIDLLHandle:=LoadLibraryA(PAnsiChar(DLLPath));
            //check mismatched bitness (32 vs 64)
            if (MAPIDLLHandle = 0) and (GetLastError = ERROR_BAD_EXE_FORMAT) then IsBadMAPIDll := true;
            //restore the file dir
            SetCurrentDir(Dir);
          end;
        end
        else begin
          {$IFDEF DEBUG}
          OutputDebugStringA(PAnsiChar(AnsiString('Unexpected DllPathEx type')));
          {$ENDIF}
        end;
      end;
      RegCloseKey(Key);
    end;
  end;
  //no luck, use the default
  if MAPIDLLHandle = 0 then begin
    MAPIDLLHandle:=LoadLibrary('MAPI32.DLL');
    {$IFDEF DEBUG}
    OutputDebugStringA(PAnsiChar(AnsiString('Loaded the default MAPI dll (mapi32.dll)')));
    {$ENDIF}
  end;

  if MAPIDLLHandle <> 0 then begin

    HrQueryAllRows:=GetProcAddress(MAPIDLLHandle,'HrQueryAllRows@24');
    if @HrQueryAllRows = nil then HrQueryAllRows:=GetProcAddress(MAPIDLLHandle,'HrQueryAllRows');

    OpenStreamOnFile:=GetProcAddress(MAPIDLLHandle,'OpenStreamOnFile@24');
    if @OpenStreamOnFile = nil then OpenStreamOnFile:=GetProcAddress(MAPIDLLHandle,'OpenStreamOnFile');

    FreeProws:=GetProcAddress(MAPIDLLHandle,'FreeProws@4');
    if @FreeProws = nil then FreeProws:=GetProcAddress(MAPIDLLHandle,'FreeProws');

    FreePAdrList:=GetProcAddress(MAPIDLLHandle,'FreePadrlist@4');
    if @FreePAdrList = nil then FreePAdrList:=GetProcAddress(MAPIDLLHandle,'FreePadrlist');

    HrSzFromEntryID:=GetProcAddress(MAPIDLLHandle,'HrSzFromEntryID@12');
    if @HrSzFromEntryID = nil then HrSzFromEntryID:=GetProcAddress(MAPIDLLHandle,'HrSzFromEntryID');

    HrEntryIDFromSz:=GetProcAddress(MAPIDLLHandle,'HrEntryIDFromSz@12');
    if @HrEntryIDFromSz = nil then HrEntryIDFromSz:=GetProcAddress(MAPIDLLHandle,'HrEntryIDFromSz');

    HrGetOneProp:=GetProcAddress(MAPIDLLHandle,'HrGetOneProp@12');
    if @HrGetOneProp = nil then HrGetOneProp:=GetProcAddress(MAPIDLLHandle,'HrGetOneProp');

    HrSetOneProp:=GetProcAddress(MAPIDLLHandle,'HrSetOneProp@8');
    if @HrSetOneProp = nil then HrSetOneProp:=GetProcAddress(MAPIDLLHandle,'HrSetOneProp');

    WrapCompressedRTFStream:=GetProcAddress(MAPIDLLHandle,'WrapCompressedRTFStream');
    if @WrapCompressedRTFStream = nil then WrapCompressedRTFStream:=GetProcAddress(MAPIDLLHandle,'WrapCompressedRTFStream');

    WrapCompressedRTFStreamEx:=GetProcAddress(MAPIDLLHandle,'WrapCompressedRTFStreamEx');

    RTFSync:=GetProcAddress(MAPIDLLHandle,'RTFSync');

    CreateTable:=GetProcAddress(MAPIDLLHandle,'CreateTable@36');
    if @CreateTable = nil then CreateTable:=GetProcAddress(MAPIDLLHandle,'CreateTable');

    BuildDisplayTable:=GetProcAddress(MAPIDLLHandle,'BuildDisplayTable@40');
    if @BuildDisplayTable = nil then BuildDisplayTable:=GetProcAddress(MAPIDLLHandle,'BuildDisplayTable');

    OpenIMsgSession:=GetProcAddress(MAPIDLLHandle,'OpenIMsgSession@12');
    if @OpenIMsgSession = nil then OpenIMsgSession:=GetProcAddress(MAPIDLLHandle,'OpenIMsgSession');

    CloseIMsgSession:=GetProcAddress(MAPIDLLHandle,'CloseIMsgSession@4');
    if @CloseIMsgSession = nil then CloseIMsgSession:=GetProcAddress(MAPIDLLHandle,'CloseIMsgSession');

    OpenIMsgOnIStg:=GetProcAddress(MAPIDLLHandle,'OpenIMsgOnIStg@44');
    if @OpenIMsgOnIStg = nil then OpenIMsgOnIStg:=GetProcAddress(MAPIDLLHandle,'OpenIMsgOnIStg');

    GetAttribIMsgOnIStg:=GetProcAddress(MAPIDLLHandle,'GetAttribIMsgOnIStg@12');
    if @GetAttribIMsgOnIStg = nil then GetAttribIMsgOnIStg:=GetProcAddress(MAPIDLLHandle,'GetAttribIMsgOnIStg');

    SetAttribIMsgOnIStg:=GetProcAddress(MAPIDLLHandle,'SetAttribIMsgOnIStg@16');
    if @SetAttribIMsgOnIStg = nil then SetAttribIMsgOnIStg:=GetProcAddress(MAPIDLLHandle,'SetAttribIMsgOnIStg');

    MapStorageSCode:=GetProcAddress(MAPIDLLHandle,'MapStorageSCode@4');
    if @MapStorageSCode = nil then MapStorageSCode:=GetProcAddress(MAPIDLLHandle,'MapStorageSCode');

    MAPIInitialize:=GetProcAddress(MAPIDLLHandle,'MAPIInitialize');

    MAPIUninitialize:=GetProcAddress(MAPIDLLHandle,'MAPIUninitialize');

    MAPILogonEx:=GetProcAddress(MAPIDLLHandle,'MAPILogonEx');

    MAPIAllocateBuffer:=GetProcAddress(MAPIDLLHandle,'MAPIAllocateBuffer');

    MAPIAllocateMore:=GetProcAddress(MAPIDLLHandle,'MAPIAllocateMore');

    MAPIFreeBuffer:=GetProcAddress(MAPIDLLHandle,'MAPIFreeBuffer');

    MAPIAdminProfiles:=GetProcAddress(MAPIDLLHandle,'MAPIAdminProfiles');

    MAPIGetDefaultMalloc:=GetProcAddress(MAPIDLLHandle,'MAPIGetDefaultMalloc@0');
    if @MAPIGetDefaultMalloc = nil then MAPIGetDefaultMalloc:=GetProcAddress(MAPIDLLHandle,'MAPIGetDefaultMalloc');

    OpenTnefStreamEx:=GetProcAddress(MAPIDLLHandle,'OpenTnefStreamEx');

    MAPIInitIdle:=GetProcAddress(MAPIDLLHandle,'MAPIInitIdle@4');
    if @MAPIInitIdle = nil then MAPIInitIdle:=GetProcAddress(MAPIDLLHandle,'MAPIInitIdle');

    MAPIDeinitIdle:=GetProcAddress(MAPIDLLHandle,'MAPIDeinitIdle@0');
    if @MAPIDeinitIdle = nil then MAPIDeinitIdle:=GetProcAddress(MAPIDLLHandle,'MAPIDeinitIdle');

    FtgRegisterIdleRoutine:=GetProcAddress(MAPIDLLHandle,'FtgRegisterIdleRoutine@20');
    if @FtgRegisterIdleRoutine = nil then FtgRegisterIdleRoutine:=GetProcAddress(MAPIDLLHandle,'FtgRegisterIdleRoutine');

    DeregisterIdleRoutine:=GetProcAddress(MAPIDLLHandle,'DeregisterIdleRoutine@4');
    if @DeregisterIdleRoutine = nil then DeregisterIdleRoutine:=GetProcAddress(MAPIDLLHandle,'DeregisterIdleRoutine');

    HrIStorageFromStream:=GetProcAddress(MAPIDLLHandle,'HrIStorageFromStream@16');
    if @HrIStorageFromStream = nil then HrIStorageFromStream:=GetProcAddress(MAPIDLLHandle,'HrIStorageFromStream');

    WrapStoreEntryID:=GetProcAddress(MAPIDLLHandle,'WrapStoreEntryID@24');
    if @WrapStoreEntryID = nil then WrapStoreEntryID:=GetProcAddress(MAPIDLLHandle,'WrapStoreEntryID');

    ScCopyProps:=GetProcAddress(MAPIDLLHandle,'ScCopyProps@16');
    if @ScCopyProps = nil then ScCopyProps:=GetProcAddress(MAPIDLLHandle,'ScCopyProps');

    PropCopyMore:=GetProcAddress(MAPIDLLHandle,'PropCopyMore@16');
    if @PropCopyMore = nil then PropCopyMore:=GetProcAddress(MAPIDLLHandle,'PropCopyMore');

    ScDupPropset:=GetProcAddress(MAPIDLLHandle,'ScDupPropset@16');
    if @ScDupPropset = nil then ScDupPropset:=GetProcAddress(MAPIDLLHandle,'ScDupPropset');

    HrTextFromCompressedRTFStreamEx:=GetProcAddress(MAPIDLLHandle,'HrTextFromCompressedRTFStreamEx@24');
    if @HrTextFromCompressedRTFStreamEx = nil then HrTextFromCompressedRTFStreamEx:=GetProcAddress(MAPIDLLHandle,'HrTextFromCompressedRTFStreamEx');

    ScCreateConversationIndex:=GetProcAddress(MAPIDLLHandle,'ScCreateConversationIndex@16');
    if @ScCreateConversationIndex = nil then ScCreateConversationIndex:=GetProcAddress(MAPIDLLHandle,'ScCreateConversationIndex');

    HrCreateNewWrappedObject:=GetProcAddress(MAPIDLLHandle,'HrCreateNewWrappedObject@28');
    if @HrCreateNewWrappedObject = nil then HrCreateNewWrappedObject:=GetProcAddress(MAPIDLLHandle,'HrCreateNewWrappedObject');

    MAPIOpenFormMgr := GetProcAddress(MAPIDLLHandle,'MAPIOpenFormMgr');

    MAPIOpenLocalFormContainer := GetProcAddress(MAPIDLLHandle,'MAPIOpenLocalFormContainer');

    HrValidateIPMSubtree := GetProcAddress(MAPIDLLHandle,'HrValidateIPMSubtree@20');
    if @HrValidateIPMSubtree = nil then HrValidateIPMSubtree:=GetProcAddress(MAPIDLLHandle,'HrValidateIPMSubtree');

    HrDispatchNotifications := GetProcAddress(MAPIDLLHandle,'HrDispatchNotifications@4');
    if @HrDispatchNotifications = nil then HrDispatchNotifications:=GetProcAddress(MAPIDLLHandle,'HrDispatchNotifications');

    HrGetAutoDiscoverXML := GetProcAddress(MAPIDLLHandle,'HrGetAutoDiscoverXML@20');
    if @HrGetAutoDiscoverXML = nil then HrGetAutoDiscoverXML:=GetProcAddress(MAPIDLLHandle,'HrGetAutoDiscoverXML');


  end;

  //free the strings now!
  DLLPath := '';
  Dir := '';
  TempStr := '';

finalization
  if MAPIDLLHandle <> INVALID_HANDLE_VALUE then FreeLibrary(MAPIDLLHandle);
  if DummyMAPI32Handle <> INVALID_HANDLE_VALUE then FreeLibrary(DummyMAPI32Handle);
end.
