{******************************************************************************}
{*        Copyright 1999-2001 by J.Friebel all rights reserved.               *}
{*        Autor           :  Jörg Friebel                                     *}
{*        Compiler        :  Delphi 4 / 5 / 6 / 7                             *}
{*        System          :  Windows NT / 2000                                *}
{*        Projekt         :  TAPI Komponenten (TAPI Version 1.4 bis 3.0)      *}
{*        Last Update     :  10.11.2002                                       *}
{*        Version         :  0.1                                              *}
{*        EMail           :  tapi@delphiclub.de                               *}
{******************************************************************************}
{*                                                                            *}
{*    This File is free software; You can redistribute it and/or modify it    *}
{*    under the term of GNU Library General Public License as published by    *}
{*    the Free Software Foundation. This File is distribute in the hope       *}
{*    it will be useful "as is", but WITHOUT ANY WARRANTY OF ANY KIND;        *}
{*    See the GNU Library Public Licence for more details.                    *}
{*                                                                            *}
{******************************************************************************}
{*                                                                            *}
{*    Diese Datei ist Freie-Software. Sie können sie weitervertreiben         *}
{*    und/oder verändern im Sinne der Bestimmungen der "GNU Library GPL"      *}
{*    der Free Software Foundation. Diese Datei wird,"wie sie ist",           *}
{*    zur Verfügung gestellt, ohne irgendeine GEWÄHRLEISTUNG                  *}
{*                                                                            *}
{******************************************************************************}
{*                          www.delphiclub.de                                 *}
{*                         tapi.sourceforge.net                               *}
{******************************************************************************}
unit TAPISystem;

interface
{$IFDEF WINDOWS}
{$IFDEF CPU32}
uses
  Windows, Messages ,Classes, Controls, TAPIHelpfunc;

{$INCLUDE TAPI.INC}

const
  CM_TAPI=WM_APP+1000;

type
  TAsyncFunc=(afMakeCall,afDial,afAnswer,afDrop,afAccept,afRedirect,afHold,
       afUnHold,afSecureCall,afSetupTransfer,afCompleteTransfer,afBlindTransfer,
       afSwapHold,afForward,afPark,afUnPark,afSetupConference,
       afPrepareAddToConference,afAddToConference,afRemoveFromConference,
       afPickup,afUnknown,afSetCallParams,
       afSetHookSwitch,afSetGain,afCompleteCall
       {$IFDEF TAPI20},afGetAgentCaps{$ENDIF});

  TAsyncFunctions= Set of TAsyncFunc;

  TAsyncFuncList= Class(TObject)
  private
    FList:TThreadList;
    function GetOperations: TAsyncFunctions;
  protected
  public
    constructor Create;
    destructor Destroy;override;
    function IsInList(Func:TAsyncFunc):Boolean;
    procedure Add(Func:TAsyncFunc;ID:DWord;Obj:TObject);
    procedure Delete(ID: DWord);
    function GetObject(ID:DWord):TObject;
    function GetFunction(ID:DWord):TAsyncFunc;
  published
    property Operations:TAsyncFunctions read GetOperations;
  end;

  PAsyncItem = ^TAsyncItem;
  TAsyncItem =record
    ID:DWord;
    AFunc:TAsyncFunc;
    Obj:TObject;
  end;

  TTAPIMgr = class(TComponent)
  private
    FTAPIObjects: TList;
    FAsyncList: TAsyncFuncList;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property AsyncList: TAsyncFuncList read FAsyncList;
    property TAPIObjects: TList read FTAPIObjects;
  end;


  PTAPIRec = ^TTAPIRec;
  TTAPIRec = packed record
    hDevice,
    dwMsg,
    dwCallbackInstance,
    dwParam1,
    dwParam2,
    dwParam3: DWORD;
  end;

  TCMTAPI = packed record
    Msg: Cardinal;
    AsyncFunc: ^TAsyncFunc;
    TAPIRec: PTAPIRec;
    Result: Longint;
  end;


  TTAPIComponent=class(TComponent)
  private
    function GetVersion: Real;
  protected
    
  public
    constructor Create(AOwner:TComponent); override;
    destructor Destroy;override;
    procedure PerformMsg(Msg: TCMTAPI);dynamic;
    property Version: Real read GetVersion;
  end;

var AppTAPIMgr:TTAPIMgr;

{$ENDIF}
{$ENDIF}

implementation

{$IFDEF WINDOWS}
{$IFDEF CPU32}

uses Forms, SysUtils;

{ TTAPIComponent }

constructor TTAPIComponent.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  if Assigned(AppTAPIMgr)=False then AppTAPIMgr := TTAPIMgr.Create(Application);
  AppTAPIMgr.TAPIObjects.Add(self);
end;

destructor TTAPIComponent.Destroy;
begin
  inherited;
end;


function TTAPIComponent.GetVersion: Real;
begin
  Result:=3.8;
end;

procedure TTAPIComponent.PerformMsg(Msg: TCMTAPI);
begin
  // nothing to do
end;

{ TAsyncFuncList }

procedure TAsyncFuncList.Add(Func: TAsyncFunc; ID: DWord; Obj: TObject);
var AItem:PAsyncItem;
begin
  New(AItem);
  AItem^.AFunc:=Func;
  AItem^.ID:=ID;
  AItem^.Obj:=Obj;
  FList.Add(AItem);
end;

constructor TAsyncFuncList.Create;
begin
  FList:=TThreadList.Create;
  {$IFDEF VER130}
  FList.Duplicates:=dupError;
  {$ENDIF}
end;

procedure TAsyncFuncList.Delete(ID: DWord);
var List:TList;
    Item:PAsyncItem;
    i:Integer;
begin
  List:=FList.LockList;
  for i:=0 to List.Count-1 do
  begin
    Item:=PAsyncItem(List.Items[i]);
    if  Item^.ID=ID then
    begin
      List.Delete(I);
      List.Pack;
      break;
    end;
  end;
  FList.UnlockList;
end;

destructor TAsyncFuncList.Destroy;
begin
  FList.Free;
  inherited;
end;

function TAsyncFuncList.GetFunction(ID: DWord): TAsyncFunc;
var List:TList;
    Item:PAsyncItem;
    i:Integer;
begin
  Result:=afUnknown;
  List:=FList.LockList;
  for i:=0 to List.Count-1 do
  begin
    Item:=PAsyncItem(List.Items[i]);
    if Item^.ID=ID then
    Result:=Item^.AFunc;
  end;
  FList.UnlockList;
end;

function TAsyncFuncList.GetObject(ID: DWord): TObject;
var List:TList;
    Item:PAsyncItem;
    i:Integer;
begin
  Result:=nil;
  List:=FList.LockList;
  for i:=0 to List.Count-1 do
  begin
    Item:=PAsyncItem(List.Items[i]);
    if Item^.ID=ID then
    Result:=Item^.Obj;
  end;
  FList.UnlockList;
end;

function TAsyncFuncList.GetOperations: TAsyncFunctions;
var List:TList;
    Item:PAsyncItem;
    i:Integer;
begin
  Result:=[];
  List:=FList.LockList;
  for i:=0 to List.Count-1 do
  begin
    Item:=PAsyncItem(List.Items[i]);
    Result:=Result + [Item^.AFunc];
  end;
  FList.UnlockList;
end;

function TAsyncFuncList.IsInList(Func: TAsyncFunc): Boolean;
var List:TList;
    Item:PAsyncItem;
    i:Integer;
begin
  Result:=False;
  List:=FList.LockList;
  for i:=0 to List.Count-1 do
  begin
    Item:=PAsyncItem(List.Items[i]);
    if  Item^.AFunc=Func then
    begin
      Result:=True;
      break;
    end;
  end;
  FList.UnlockList;
end;

{ TTAPIMgr }

constructor TTAPIMgr.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTAPIObjects:=TList.Create;
  FAsyncList:=TAsyncFuncList.Create;
end;

destructor TTAPIMgr.Destroy;
begin
  FAsyncList.Free;
  FTAPIObjects.Clear;
  FTAPIObjects.Free;
  inherited;
end;



{$ENDIF}
{$ENDIF}
end.