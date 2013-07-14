// Outlext.h
//
// Copyright (C) Microsoft Corp. 1986-1999.  All Rights Reserved.
//---------------------------------------------------------------------------
//	contains Outlook interface definitions for IOutlookExtCallback and IOutlookExtItemEvents
//
(*
 *	IOutlookExtCallback
 *
 *	Purpose:
 *		Resource interface that may be used by Outlook client extensions.
 *)

unit OutlExt;

interface

uses Windows, ActiveX, MAPIDefs, ExchExt;

type

  IOutlookExtCallback = interface(IUnknown)
    ['{0006720D-0000-0000-C000-000000000046}']
    function GetObject(out lppmp: IUnknown):HResult;stdcall;
    function GetOfficeCharacter(out ppmsotfc: pointer):HResult;stdcall;
  end;

  IOutlookExtItemEvents = interface(IUnknown)
    ['{0006723A-0000-0000-C000-000000000046}']
    function OnOpen(var peecb : IExchExtCallback):HResult;stdcall;
    function OnOpenComplete(var peecb : IExchExtCallback; ulFlags : ULONG):HResult;stdcall;
    function OnClose(var peecb : IExchExtCallback; ulSaveOptions : ULONG):HResult;stdcall;
    function OnCloseComplete(var peecb : IExchExtCallback; ulFlags : ULONG):HResult;stdcall;
  end;

implementation

end.
