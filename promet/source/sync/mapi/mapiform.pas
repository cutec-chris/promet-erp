{++

  m a p i f o r m . p a s

  Abstract:

    Automatic conversion of mapiform.h.

  Comments:

    This source file automatically converted by
    htrans 0.91 beta 1 Copyright (c) 1997 Alexander Staubo

  Revision history:

    18-06-1997 20:53 alex  [Autogenerated]
    18-06-1997 20:53 alex  Retouched for release

--}

unit MapiForm;

{$IFDEF CPU64}
  //64 bit - align at 8 bytes
  {$A8}
{$ELSE}
  //32 bit
  {$A+}
{$ENDIF }
{$MINENUMSIZE 4}

interface

uses
  Windows, SysUtils, ActiveX,
  MapiGuid, MapiDefs, MapiX;

(*
 *  M A P I F O R M . H
 *
 *  Declarations of interfaces for clients and providers of MAPI
 *  forms and form registries.
 *
 *  Copyright 1986-1996 Microsoft Corporation. All Rights Reserved.
 *)

 (*
 *  V e r b s
 *)


const
  // Interpersonal messaging verbs
  EXCHIVERB_OPEN               = 0;
  EXCHIVERB_RESERVED_COMPOSE   = 100;
  EXCHIVERB_RESERVED_OPEN      = 101;
  EXCHIVERB_REPLYTOSENDER      = 102;
  EXCHIVERB_REPLYTOALL         = 103;
  EXCHIVERB_FORWARD            = 104;
  EXCHIVERB_PRINT              = 105;
  EXCHIVERB_SAVEAS             = 106;
  EXCHIVERB_RESERVED_DELIVERY  = 107;
  EXCHIVERB_REPLYTOFOLDER      = 108;


type
  PCRECT = ^TRECT;

(* HFRMREG is an enumeration which represents a registry container.
 * Microsoft reserves the values from 0 to 0x3FFF for its own use.
 *)

  THFRMREG = ULONG;
  
const
  HFRMREG_DEFAULT = 0;
  HFRMREG_LOCAL = 1;
  HFRMREG_PERSONAL = 2;
  HFRMREG_FOLDER = 3;
  HFRMREG_ENTERPRISE = 4;

  MAPIFORM_EXACTMATCH = $0020;

MAPIFORM_SELECT_ALL_REGISTRIES           = 0;
MAPIFORM_SELECT_FOLDER_REGISTRY_ONLY     = 1;
MAPIFORM_SELECT_NON_FOLDER_REGISTRY_ONLY = 2;



type
  PPCSTR = ^PAnsiChar;

  TSAVEOPTS =
    (
      SAVEOPTS_SAVEIFDIRTY,
      SAVEOPTS_NOSAVE,
      SAVEOPTS_PROMPTSAVE
    );

(* Implemented by viewers to support next/previous in forms.
 *)

{ Structure passed in GetPrintSetup  }

type
  TFORMPRINTSETUP =     
    record
      ulFlags : ULONG;                 { MAPI_UNICODE }
      hDevMode : HGLOBAL;
      hDevNames : HGLOBAL;
      ulFirstPageNumber : ULONG;
      fPrintAttachments : ULONG;
    end;

  PFORMPRINTSETUP = ^TFORMPRINTSETUP;

{ Values for pulFormat in GetSaveStream }

type
  TFORMPROPSPECIALTYPE = ULONG;

const
  SAVE_FORMAT_TEXT = 1;
  SAVE_FORMAT_RICHTEXT = 2;

  { Values from 0 to 0x3fff are reserved for future definition by Microsoft }

const
  VCSTATUS_NEXT = $00000001;
  VCSTATUS_PREV = $00000002;
  VCSTATUS_MODAL = $00000004;
  VCSTATUS_INTERACTIVE = $00000008;
  VCSTATUS_READONLY = $00000010;
  VCSTATUS_DELETE = $00010000;
  VCSTATUS_COPY = $00020000;
  VCSTATUS_MOVE = $00040000;
  VCSTATUS_SUBMIT = $00080000;
  VCSTATUS_DELETE_IS_MOVE = $00100000;
  VCSTATUS_SAVE = $00200000;
  VCSTATUS_NEW_MESSAGE = $00400000;
  VCDIR_NEXT = VCSTATUS_NEXT;
  VCDIR_PREV = VCSTATUS_PREV;
  VCDIR_DELETE = VCSTATUS_DELETE;
  VCDIR_MOVE = VCSTATUS_MOVE;

  //Steve G.
  VCSTATUS_UNREAD       = $00000020;
  VCSTATUS_CATEGORY      = $00000040;

{ MAPI Form property descriptor }

(*
 * Values for the tag in the SMAPIFormProp structure
 *
 * Microsoft reserves the range from 0 to 0x3FFF for future use in its other
 * forms registry implementations.
 *)

  FPST_VANILLA = 0;
  FPST_ENUM_PROP = 1;

{ Enumeration of permissible values for PR_FORM_MESSAGE_BEHAVIOR }

  MAPI_MESSAGE_BEHAVIOR_IPM = 0;
  MAPI_MESSAGE_BEHAVIOR_FOLDER = 1;

{ Platform numbers (used in .CFG files for forms) }

const
  MAPIFORM_CPU_X86 = 1;
  MAPIFORM_CPU_MIP = 2;
  MAPIFORM_CPU_AXP = 3;
  MAPIFORM_CPU_PPC = 4;
  MAPIFORM_CPU_M68 = 5;
  MAPIFORM_CPU_X64 = 6;

  MAPIFORM_OS_WIN_31 = 1;
  MAPIFORM_OS_WINNT_35 = 2;
  MAPIFORM_OS_WIN_95 = 3;
  MAPIFORM_OS_MAC_7x = 4;
  MAPIFORM_OS_WINNT_40 = 5;
  MAPIFORM_OS_WINNT_50 = 6;
  MAPIFORM_OS_WINNT_60 = 7;


{  Flags for IMAPIFormMgr::CalcFormPropSet }

{ #define FORMPROPSET_UNION            0   }

{ #define FORMPROPSET_INTERSECTION     1   }

{  Flags for IMAPIFormMgr::InstallForm     }

  MAPIFORM_INSTALL_DIALOG = MAPI_DIALOG;
  MAPIFORM_INSTALL_OVERWRITEONCONFLICT = $0010;

{  Flags for IMAPIFormContainer::ResolveMessageClass and
      IMAPIFormContainer::ResolveMultipleMessageClasses }

{ #define MAPIFORM_EXACTIMATCH    0x0020   }

{!! Interface forwards }

type
  IMAPIMessageSite = interface;
  IMAPIFormInfo = interface;
  IMAPIFormMgr = interface;
  IMAPIViewContext = interface;
  IMAPIViewAdviseSink = interface;
  IMAPIFormAdviseSink = interface;
  IMAPIFormContainer = interface;

{ Single enum value }

  TSMAPIFormPropEnumVal =
    record                             { fpev }
      pszDisplayName : PAnsiChar;          { carries the display AnsiString }
      nVal : ULONG;                    { the value for the above enumeration }
    end;

  PMAPIFORMPROPENUMVAL = ^TSMAPIFormPropEnumVal;

  TSMAPIFormProp =
    record
      ulFlags : ULONG;                 { Contains MAPI_UNICODE if strings are UNICODE }
      nPropType : ULONG;               { type of the property, hiword is 0 }
      nmid : TMAPINAMEID;              { id of the property }
      pszDisplayName : PAnsiChar;
      nSpecialType : TFORMPROPSPECIALTYPE;
                                       { tag for the following union }
      u :
        record
          case Integer of
            1 :
              (
                s1 : { Property AnsiString/Number association Enumeration }
                  record
                    nmidIdx : TMAPINAMEID;
                    cfpevAvailable : ULONG; { # of enums }
                    pfpevAvailable : PMAPIFORMPROPENUMVAL;
                  end;
              );
        end;
    end;

  PMAPIFORMPROP = ^TSMAPIFormProp;

{ Array of form properties }

  TSMAPIFormPropArray =
    record
      cProps : ULONG;
      ulPad : ULONG;                   { Pad to 8-byte alignment for insurance }
      aFormProp : array[0..MAPI_DIM - 1] of TSMAPIFormProp;
    end;

  PMAPIFORMPROPARRAY = ^TSMAPIFormPropArray;

{ Structure defining the layout of an mapi verb description }

  TSMAPIVerb =
    record
      lVerb : Longint;
      szVerbname : PAnsiChar;
      fuFlags : DWORD;
      grfAttribs : DWORD;
      ulFlags : ULONG;                 { Either 0 or MAPI_UNICODE }
    end;

  PMAPIVERB = ^TSMAPIVerb;

{ Structure used for returning arrays of mapi verbs }

  TSMAPIVerbArray =
    record
      cMAPIVerb : ULONG;               { Number of verbs in the structure }
      aMAPIVerb : array[0..MAPI_DIM - 1] of TSMAPIVerb;
    end;

  PMAPIVERBARRAY = ^TSMAPIVerbArray;

{ Structure containing an array of message class strings }

  TSMessageClassArray =
    record
      cValues : ULONG;
      aMessageClass : array[0..MAPI_DIM - 1] of LPCSTR;
    end;

  PSMESSAGECLASSARRAY = ^TSMessageClassArray;

{ Structure containing an array of IMAPIFormInfo interfaces }

  TSMAPIFormInfoArray =
    record
      cForms : ULONG;
      aFormInfo : array[0..MAPI_DIM - 1] of IMAPIFormInfo;
    end;

  PSMAPIFORMINFOARRAY = ^TSMAPIFormInfoArray;

(* This interface is implemented by forms and is used to save,
 * initialize and load forms to and from messages.
 *)

  IPersistMessage =
    interface(IUnknown)
    [strIID_IPersistMessage]
      function GetLastError (hResult : HResult; ulFlags : ULONG;
        var lppMAPIError : PMAPIERROR) : HResult; stdcall;
      function GetClassID (var lpClassID : TCLSID) : HResult; stdcall;
      function IsDirty : HResult; stdcall;
      function InitNew (pMessageSite : IMAPIMessageSite; pMessage : IMessage) : HResult; stdcall;
      function Load (pMessageSite : IMAPIMessageSite; pMessage : IMessage; 
        ulMessageStatus : ULONG; ulMessageFlags : ULONG) : HResult; stdcall;
      function Save (pMessage : IMessage; fSameAsLoad : ULONG) : HResult; stdcall;
      function SaveCompleted (pMessage : IMessage) : HResult; stdcall;
      function HandsOffMessage : HResult; stdcall;
    end;

  IMAPIMessageSite =
    interface(IUnknown)
    [strIID_IMAPIMessageSite]
      function GetLastError (hResult : HResult; ulFlags : ULONG;
        var lppMAPIError : PMAPIERROR) : HResult; stdcall;
      function GetSession (out ppSession : IMAPISession) : HResult; stdcall;
      function GetStore (out ppStore : IMsgStore) : HResult; stdcall;
      function GetFolder (out ppFolder : IMAPIFolder) : HResult; stdcall;
      function GetMessage (out ppmsg : IMessage) : HResult; stdcall;
      function GetFormManager (out ppFormMgr : IMAPIFormMgr) : HResult; stdcall;
      function NewMessage (fComposeInFolder : ULONG; pFolderFocus : IMAPIFolder; 
        pPersistMessage : IPersistMessage; out ppMessage : IMessage; 
        out ppMessageSite : IMAPIMessageSite;
        out ppViewContext : IMAPIViewContext) : HResult; stdcall;
      function CopyMessage (pFolderDestination : IMAPIFolder) : HResult; stdcall;
      function MoveMessage (pFolderDestination : IMAPIFolder; 
        pViewContext : IMAPIViewContext; prcPosRect : PCRECT) : HResult; stdcall;
      function DeleteMessage (pViewContext : IMAPIViewContext;
        prcPosRect : PCRECT) : HResult; stdcall;
      function SaveMessage : HResult; stdcall;
      function SubmitMessage (ulFlags : ULONG) : HResult; stdcall;
      function GetSiteStatus (lpulStatus : PULONG) : HResult; stdcall;
    end;

(* This interface is implemented by forms for the benefit of viewers.
 * One method (ShutdownForm) is provided such that simple forms implementing
 * only IMAPIForm and IPersistMessage have reasonable embedding behavior.
 *)

  IMAPIForm =
    interface(IUnknown)
    [strIID_IMAPIForm]
      function GetLastError (hResult : HResult; ulFlags : ULONG;
        var lppMAPIError : PMAPIERROR) : HResult; stdcall;
      function SetViewContext (
        pViewContext : IMAPIViewContext) : HResult; stdcall;
      function GetViewContext (
        out ppViewContext : IMAPIViewContext) : HResult; stdcall;
      function ShutdownForm (ulSaveOptions : ULONG) : HResult; stdcall;
      function DoVerb (iVerb : Longint; lpViewContext : IMAPIViewContext; { can be null }
        hwndParent : ULONG_PTR; lprcPosRect : PCRECT) : HResult; stdcall;
      function Advise (pAdvise : IMAPIViewAdviseSink;
        out pdwStatus : ULONG_PTR) : HResult; stdcall;
      function Unadvise (ulConnection : ULONG_PTR) : HResult; stdcall;
    end;

  IMAPIViewContext =
    interface(IUnknown)
    [strIID_IMAPIViewContext]
      function GetLastError (hResult : HResult; ulFlags : ULONG;
        var lppMAPIError : PMAPIERROR) : HResult; stdcall;
      function SetAdviseSink (pmvns : IMAPIFormAdviseSink) : HResult; stdcall;
      function ActivateNext (ulDir : ULONG;
        prcPosRect : PCRECT) : HResult; stdcall;
      function GetPrintSetup (ulFlags : ULONG;
        var lppFormPrintSetup : PFORMPRINTSETUP) : HResult; stdcall;
      function GetSaveStream (var pulFlags : ULONG; var pulFormat : ULONG;
        out ppstm : IStream) : HResult; stdcall;
      function GetViewStatus (lpulStatus : PULONG) : HResult; stdcall;
    end;

(* Part of form server, held by view; receives notifications from the view.
 *
 * This part of the form server, but is not an interface on the form
 * object.  This means that clients should not expect to QueryInterface
 * from an IMAPIForm* or IOleObject* to this interface, or vice versa.
 *)

  IMAPIFormAdviseSink =
    interface(IUnknown)
    [strIID_IMAPIFormAdviseSink]
      function OnChange (ulDir : ULONG) : HResult; stdcall;
      function OnActivateNext (lpszMessageClass : LPCSTR;
        ulMessageStatus : ULONG; ulMessageFlags : ULONG;
        out ppPersistMessage : IPersistMessage) : HResult; stdcall;
    end;

(* Part of view context, held by form; receives notifications from the form.
 *)

  IMAPIViewAdviseSink =
    interface(IUnknown)
    [strIID_IMAPIViewAdviseSink]
      function OnShutdown : HResult; stdcall;
      function OnNewMessage : HResult; stdcall;
      function OnPrint (dwPageNumber : ULONG;
        hrStatus : HResult) : HResult; stdcall;
      function OnSubmitted : HResult; stdcall;
      function OnSaved : HResult; stdcall;
    end;

(* Is implemented by registries.  Describes the form.
 *)

  PMAPIFormInfo = ^IMAPIFormInfo;
  IMAPIFormInfo =
    interface(IMAPIProp)
    [strIID_IMAPIFormInfo]
      { note: subsumes getlasterror }
      function CalcFormPropSet (ulFlags : ULONG;
        var ppFormPropArray : PMAPIFORMPROPARRAY) : HResult; stdcall;
      function CalcVerbSet (ulFlags : ULONG;
        var ppMAPIVerbArray : PMAPIVERBARRAY) : HResult; stdcall;
      function MakeIconFromBinary (nPropID : ULONG;
        var phicon : HICON) : HResult; stdcall;
      function SaveForm (szFileName : PAnsiChar) : HResult; stdcall;
      function OpenFormContainer (
        out ppformcontainer : IMAPIFormContainer) : HResult; stdcall;
    end;

(* The client-visible interface for form resolution and dispatch.
 *)

  IMAPIFormMgr =
    interface(IUnknown)
    [strIID_IMAPIFormMgr]
      function GetLastError (hResult : HResult; ulFlags : ULONG;
        var lppMAPIError : PMAPIERROR) : HResult; stdcall;
      function LoadForm (ulUIParam : ULONG_PTR; ulFlags : ULONG;
        lpszMessageClass : LPCSTR; ulMessageStatus : ULONG; ulMessageFlags : ULONG;
        pFolderFocus : IMAPIFolder; pMessageSite : IMAPIMessageSite;
        pmsg : IMessage; pViewContext : IMAPIViewContext; riid : PIID;
        out ppvObj : IUnknown) : HResult; stdcall;
      function ResolveMessageClass (szMsgClass : LPCSTR; ulFlags : ULONG;
        pFolderFocus : IMAPIFolder;  { can be null }
        out ppResult : IMAPIFormInfo) : HResult; stdcall;
      function ResolveMultipleMessageClasses (pMsgClasses : PSMESSAGECLASSARRAY;
        ulFlags : ULONG; pFolderFocus : IMAPIFolder; { can be null }
      var pfrminfoarray : PSMAPIFORMINFOARRAY) : HResult; stdcall;
      function CalcFormPropSet (pfrminfoarray : PSMAPIFORMINFOARRAY; ulFlags : ULONG;
        var ppResults : PMAPIFORMPROPARRAY) : HResult; stdcall;
      function CreateForm (ulUIParam : ULONG_PTR; ulFlags : ULONG;
        pfrminfoToActivate : IMAPIFormInfo; refiidToAsk : PIID;
        out ppvObj : IUnknown) : HResult; stdcall;
      function SelectForm (ulUIParam : ULONG_PTR; ulFlags : ULONG;
        pszTitle : PAnsiChar; pfld : IMAPIFolder;
        out ppfrminfoReturned : IMAPIFormInfo) : HResult; stdcall;
      function SelectMultipleForms (ulUIParam : ULONG_PTR; ulFlags : ULONG;
        pszTitle : PAnsiChar; pfld : IMAPIFolder; pfrminfoarray : PSMAPIFORMINFOARRAY;
        var ppfrminfoarray : PSMAPIFORMINFOARRAY) : HResult; stdcall;
      function SelectFormContainer (ulUIParam : ULONG_PTR; ulFlags : ULONG;
        out lppfcnt : IMAPIFormContainer) : HResult; stdcall;
      function OpenFormContainer (hfrmreg : THFRMREG; lpunk : IUnknown; 
        out lppfcnt : IMAPIFormContainer) : HResult; stdcall;
      function PrepareForm (ulUIParam : ULONG_PTR; ulFlags : ULONG;
        pfrminfo : IMAPIFormInfo) : HResult; stdcall;
      function IsInConflict (ulMessageFlags : ULONG; ulMessageStatus : ULONG; 
        szMessageClass : LPCSTR; pFolderFocus : IMAPIFolder) : HResult; stdcall;
    end;

  IMAPIFormContainer =
    interface(IUnknown)
    [strIID_IMAPIFormContainer]
      function GetLastError (hResult : HResult; ulFlags : ULONG;
        var lppMAPIError : PMAPIERROR) : HResult; stdcall;
      function InstallForm (ulUIParam : ULONG_PTR; ulFlags : ULONG;
        szCfgPathName : PAnsiChar) : HResult; stdcall;
      function RemoveForm (szMessageClass : LPCSTR) : HResult; stdcall;
      function ResolveMessageClass (szMessageClass : LPCSTR; ulFlags : ULONG; 
        var pforminfo : IMAPIFormInfo) : HResult; stdcall;
      function ResolveMultipleMessageClasses (
        pMsgClassArray : PSMESSAGECLASSARRAY; ulFlags : ULONG;
        var ppfrminfoarray : PSMAPIFORMINFOARRAY) : HResult; stdcall;
      function CalcFormPropSet (ulFlags : ULONG;
        var ppResults : PMAPIFORMPROPARRAY) : HResult; stdcall;
      function GetDisplay (ulFlags : ULONG;
        var pszDisplayName : PAnsiChar) : HResult; stdcall;
    end;

  IMAPIFormFactory =
    interface(IUnknown)
    [strIID_IMAPIFormFactory]
      function GetLastError (hResult : HResult; ulFlags : ULONG;
        var lppMAPIError : PMAPIERROR) : HResult; stdcall;
      function CreateClassFactory (const clsidForm : TIID{!! was: REFCLSID };
        ulFlags : ULONG; out lppClassFactory : IClassFactory) : HResult; stdcall;
      function LockServer (ulFlags : ULONG; fLockServer : ULONG) : HResult; stdcall;
    end;
{
function MAPIOpenFormMgr (pSession : IMAPISession;
  out ppmgr : IMAPIFormMgr) : HResult; stdcall;

function MAPIOpenLocalFormContainer (
  out ppfcnt : IMAPIFormContainer) : HResult; stdcall;
}
implementation
{
const
  Mapi32Dll = 'mapi32.dll';

function MAPIOpenFormMgr; external Mapi32Dll name 'MAPIOpenFormMgr';
function MAPIOpenLocalFormContainer; external Mapi32Dll name 'MAPIOpenLocalFormContainer';
}
end.
