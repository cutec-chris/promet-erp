{******************************************************************************}
{*        Copyright 1999-2002 by J.Friebel all rights reserved.               *}
{*        Autor           :  Jörg Friebel                                     *}
{*        Compiler        :  Delphi 4 / 5 / 6 / 7                             *}
{*        System          :  Windows NT / 2000                                *}
{*        Projekt         :  TAPI Komponenten (TAPI Version 1.4 bis 3.0)      *}
{*        Last Update     :  10.11.2002                                       *}
{*        Version         :  2.0.3                                            *}
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
{******************************************************************************}

unit TAPIServices;

interface

{$IFDEF WINDOWS}
{$IFDEF CPU32}

{$INCLUDE TAPI.INC}

{$IFDEF TAPI20 }
uses Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
     TAPI,TAPISystem,TAPICallBack,TAPIThread;
{$ELSE}
uses Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
     TAPI,TAPISystem,TAPICallBack;
{$ENDIF}



type
  TTAPIExtensionID = packed record
    dwExtensionID0,
    dwExtensionID1,
    dwExtensionID2,
    dwExtensionID3: DWORD;
  end;
  {$IFDEF TAPI20}
  TLineInitializeExOption = (ieoUseHiddenWindow,ieoUseEvent,
     ieoUseCompletionPort{$IFDEF TAPI30},ieoCallHubTracking{$ENDIF});
  {$ENDIF}



  TLineRequestMode = (lrqmMakeCall,lrqmMediaCall,lrqmDrop);

  TLineRequestEvent=procedure(Sender:TObject;RequestMode:TLineRequestMode;
    WndHandle:hWnd;RequestID:DWord)of Object;
  TLineCreateEvent=procedure(Sender:TObject;NewDeviceID:Dword)of Object;
  TLineRemoveEvent=procedure(Sender:TObject;DeviceID:DWord)of Object;
  TPhoneCreateEvent=procedure(Sender:TObject;NewDeviceID:Dword)of Object;
  TNegotiateExtVerErrorEvent=procedure(Sender:TObject;DeviceID,
     ErrorCode:DWord)of Object;

{Prototype LineService PhoneService}

type
  TTAPIService = class(TTAPIComponent)
  private
    FExtLoVer:DWord;
    FExtHiVer:DWord;
    {$IFDEF TAPI20}
    FThread:TTAPIThread;
    FInitOpt:DWord;
    FEvent:THandle;
    {$ENDIF}
    FActive:Boolean;
    FAPIExtVersion:DWord;
    FAPIHiVer:DWord;
    FAPILoVer:DWord;
    FNumDev:DWord;
    FAppName:String;
    FAppHandle:THandle;
    function GetActive: Boolean;
    procedure SetActive(const Value: Boolean);
    function GetHandle: THandle;
    function GetCallBack: TTAPICallBack;virtual;abstract;
    function GetAPIHighVersion: ShortString;
    procedure SetAPIHighVersion(const Value: ShortString);
    function GetAPILoVersion: ShortString;
    procedure SetAPILoVersion(const Value: ShortString);
    procedure SetFAPIHiVer(const Value: DWord);virtual;
    {$IFDEF TAPI20}
    function GetInitOptions: TLineInitializeExOption;
    procedure SetInitOptions(const Value: TLineInitializeExOption);
    {$ENDIF}
    function GetExtHiVersion: ShortString;
    function GetExtLoVersion: ShortString;
    procedure SetExtHiVersion(const Value: ShortString);
    procedure SetExtLoVersion(const Value: ShortString);
  protected
    procedure Initalize; virtual;abstract;
    {$IFDEF TAPI20}
    procedure InitalizeEx;virtual;abstract;
    {$ENDIF}
    procedure Shutdown;virtual;abstract;
    property CallBackFunc:TLineCallBack read GetCallBack ;
    property Active:Boolean read GetActive write SetActive default False;
    property AppName:String read FAppName write FAppName;
    property NumDevice:DWord read FNumDev;
    property APILoVer:DWord read FAPILoVer write FAPILoVer;
    property APIHiVer:DWord read FAPIHiVer write SetFAPIHiVer;
    property APIHiVerApp:ShortString read GetAPIHighVersion write SetAPIHighVersion ;
    property APILoVerApp:ShortString read GetAPILoVersion write SetAPILoVersion ;
    property APIExtVersion:DWord read FAPIExtVersion write FAPIExtVersion default 0;
    property Handle:THandle read GetHandle;
    property ExtLoVersion:ShortString read GetExtLoVersion write SetExtLoVersion;
    property ExtHiVersion:ShortString read GetExtHiVersion write SetExtHiVersion;
    {$IFDEF TAPI20}
    property InitOptions:TLineInitializeExOption read GetInitOptions write SetInitOptions default ieoUseHiddenWindow ;
    property Event:THandle read FEvent;
    {$ENDIF}
  public
    constructor Create(AOwner:TComponent);override;
    destructor Destroy;override;
    function CheckAPIVersion(DeviceID:DWord;var APIVersion:DWord;var ExtID:TTAPIExtensionID):Boolean;virtual;abstract;
  end;

type TTAPICustomService = class(TTAPIService)
     private
       FOnNegotiateExtVerError:TNegotiateExtVerErrorEvent;
     public
       property APIExtVersion;
       property APIHiVer;
       property APILoVer;
       property NumDevice;
       property Active;
     published
       {$IFDEF TAPI20}
       property InitOptions;
       {$ENDIF}
       property ExtLoVersion;
       property ExtHiVersion;
       property Handle;
       property OnNegotiateExtVerError:TNegotiateExtVerErrorEvent read FOnNegotiateExtVerError write FOnNegotiateExtVerError;
     end;

{ LineService }

type
  TTAPILineService = class(TTAPICustomService)
  private
    FOnLineCreate:TLineCreateEvent;
    FOnLineRequest: TLineRequestEvent;
    FOnLineRemove:TLineRemoveEvent ;
    function GetCallBack: TTAPICallBack;override;
    procedure SetFAPIHiVer(const Value: DWord);override;
  protected
    procedure Initalize;override;
    {$IFDEF TAPI20}
    procedure InitalizeEx;override;
    {$ENDIF}
    procedure Shutdown;override;
  public
    constructor Create(AOwner: TComponent);override;
    destructor Destroy; override;
    function CheckAPIVersion(DeviceID:DWord;var APIVersion:DWord;var ExtID:TTAPIExtensionID):Boolean;override;
    procedure LineCreate(var NewDeviceID:DWord);virtual;
    procedure LineRequest(var RequestMode,Param2,Param3:DWord);virtual;
    procedure LineRemove(var DeviceID:DWord);virtual;
    function GetAPIVersion(DeviceID:DWord;var ExtID:TLineExtensionID):DWord;
    function GetExtVersion(DeviceID,APIVersion:DWord;var ExtID:TLineExtensionID):DWord;
    procedure PerformMsg(Msg: TCMTAPI);override;
 published
    property APIHiVerApp;
    property APILoVerApp;
    property OnLineCreate:TLineCreateEvent read FOnLineCreate write FOnLineCreate;
    property OnLineRequest:TLineRequestEvent read FOnLineRequest write FOnLineRequest;
    property OnLineRemove:TLineRemoveEvent read FOnLineRemove write FOnLineRemove;
    property AppName;
  end;
  
  {$IFDEF TAPI20}
  TPhoneRemoveEvent=procedure(Sender:TObject;DeviceID:DWord)of Object;
  {$ENDIF}

  TTAPIPhoneService = class(TTAPICustomService)
  private
    FOnPhoneCreate:TPhoneCreateEvent;
    {$IFDEF TAPI20}
    FOnPhoneRemove:TPhoneRemoveEvent;
    {$ENDIF}
    function GetCallBack: TTAPICallBack;override;
  protected
    procedure Initalize;override;
    {$IFDEF TAPI20}
    procedure InitalizeEx;override;
    procedure PhoneRemove(DeviceID:DWord);virtual;
    {$ENDIF}
    procedure Shutdown;override;
    procedure PhoneCreate(var hDevice,NewDeviceID:DWord);virtual;
  public
    constructor Create(AOwner: TComponent);override;
    destructor Destroy; override;
    function CheckAPIVersion(DeviceID:DWord;var APIVersion:DWord;var ExtID:TTAPIExtensionID):Boolean;override;
    function GetAPIVersion(DeviceID:DWord;var ExtID:TPhoneExtensionID):DWord;
    function GetExtVersion(DeviceID,APIVersion:DWord;var ExtID:TPhoneExtensionID):DWord;
    procedure PerformMsg(Msg: TCMTAPI);override;
    property Active;
  published
    property APIHiVerApp;
    property APILoVerApp;
    property OnPhoneCreate:TPhoneCreateEvent read FOnPhoneCreate write FOnPhoneCreate;
    {$IFDEF TAPI20}
    property OnPhoneRemove:TPhoneRemoveEvent read FOnPhoneRemove write FOnPhoneRemove;
    {$ENDIF}
    property AppName;
  end;

function IntToRequestMode(Value:LongWord):TLineRequestMode;
{$IFDEF TAPI20}
function IntToInitializeExOption(Value:LongWord):TLineInitializeExOption;
function InitializeExOptionToInt(Value: TLineInitializeExOption):LongWord;
{$ENDIF}

procedure Register;

{$ENDIF}
{$ENDIF}

implementation

{$IFDEF WINDOWS}
{$IFDEF CPU32}

uses TAPIErr,TAPICurVer,TAPIHelpFunc;

procedure Register;
begin
{$IFDEF TAPI30}
  RegisterComponents('TAPI30', [TTAPILineService]);
  RegisterComponents('TAPI30', [TTAPIPhoneService]);
{$ELSE}
{$IFDEF TAPI22}
  RegisterComponents('TAPI22', [TTAPILineService]);
  RegisterComponents('TAPI22', [TTAPIPhoneService]);
{$ELSE}
{$IFDEF TAPI21}
  RegisterComponents('TAPI21', [TTAPILineService]);
  RegisterComponents('TAPI21', [TTAPIPhoneService]);
{$ELSE}
{$IFDEF TAPI20}
  RegisterComponents('TAPI20', [TTAPILineService]);
  RegisterComponents('TAPI20', [TTAPIPhoneService]);
{$ELSE}
  RegisterComponents('TAPI', [TTAPILineService]);
  RegisterComponents('TAPI', [TTAPIPhoneService]);
{$ENDIF}
{$ENDIF}
{$ENDIF}
{$ENDIF}
end;

function IntToRequestMode(Value:LongWord):TLineRequestMode;
begin
  Result:=lrqmMediaCall; // Undefine in Win32
  case Value of
    LINEREQUESTMODE_MAKECALL:  Result:= lrqmMakeCall;
    LINEREQUESTMODE_MEDIACALL: Result:= lrqmMediaCall;
    LINEREQUESTMODE_DROP:      Result:= lrqmDrop;
  end;
end;

{$IFDEF TAPI20}
function IntToInitializeExOption(Value:LongWord):TLineInitializeExOption;
begin
  Result:=ieoUseHiddenWindow;
  if Value=LINEINITIALIZEEXOPTION_USEHIDDENWINDOW then Result:=ieoUseHiddenWindow;
  if Value=LINEINITIALIZEEXOPTION_USEEVENT then Result:=ieoUseEvent;
  if Value=LINEINITIALIZEEXOPTION_USECOMPLETIONPORT then Result:=ieoUseCompletionPort;
  {$IFDEF TAPI30}
  if Value=LINEINITIALIZEEXOPTION_CALLHUBTRACKING  then Result:=ieoCallHubTracking;
  {$ENDIF}
end;

function InitializeExOptionToInt(Value: TLineInitializeExOption):LongWord;
begin
  Result:=LINEINITIALIZEEXOPTION_USEHIDDENWINDOW;
  if Value=ieoUseHiddenWindow then Result:=LINEINITIALIZEEXOPTION_USEHIDDENWINDOW;
  if Value=ieoUseEvent  then Result:=LINEINITIALIZEEXOPTION_USEEVENT;
  if Value=ieoUseCompletionPort then Result:=LINEINITIALIZEEXOPTION_USECOMPLETIONPORT;
  {$IFDEF TAPI30}
  if Value=ieoCallHubTracking  then Result:=LINEINITIALIZEEXOPTION_CALLHUBTRACKING;
  {$ENDIF}
end;
{$ENDIF}



{ TTAPIService }

constructor TTAPIService.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  {$IFDEF TAPI20}
  FInitOpt:=LINEINITIALIZEEXOPTION_USEHIDDENWINDOW;
  {$ENDIF}
  FActive:=False;
  FAPIExtVersion:=0;
  FAPIHiVer:=TAPI_CURRENT_VERSION;
  FAPILoVer:=$000000;
  FAppName:=TForm(Owner).Caption;
  FExtHiVer:=$7FFFFFFF;
  FExtLoVer:=$00000;
end;

destructor TTAPIService.Destroy;
begin
  SetActive(False);
  inherited Destroy;
end;

function TTAPIService.GetActive: Boolean;
begin
  Result:=FActive;
end;

function TTAPIService.GetAPIHighVersion: ShortString;
begin
  result:='$'+IntToHex(FAPIHiVer,8);
end;

function TTAPIService.GetAPILoVersion: ShortString;
begin
  result:='$'+IntToHex(FAPILoVer,8);
end;

function TTAPIService.GetExtHiVersion: ShortString;
begin
  result:='$'+IntToHex(FExtHiVer,8);
end;

function TTAPIService.GetExtLoVersion: ShortString;
begin
  result:='$'+IntToHex(FExtLoVer,8);
end;

function TTAPIService.GetHandle: THandle;
begin
  Result:=FAppHandle;
end;

{$IFDEF TAPI20}

function TTAPIService.GetInitOptions: TLineInitializeExOption;
begin
  Result:=IntToInitializeExOption(FInitOpt);
end;
{$ENDIF}

procedure TTAPIService.SetActive(const Value: Boolean);
begin
  if Value<> FActive then
  begin
    IF Value then
    begin
      {$IFNDEF TAPI20}
      Initalize;
      {$ELSE}
      InitalizeEx;
      {$ENDIF}
      FActive:=True;
    end
    else
    begin
      ShutDown;
      FActive:=False;
    end;
  end;
end;

function SetVersion(const Value:ShortString;Min,Max:LongInt):LongInt;
var Dummy:LongInt;
begin
 Dummy:=StrToIntDef(Value,$00000000);
 if (Dummy < Min) or (Dummy > Max) then
    raise ERangeError.CreateFmt(
    '$%x liegt nicht im gültigen Bereich zwischen $%x..$%x',[Dummy, Min, Max]);
  Result:=Dummy;
end;

procedure TTAPIService.SetAPIHighVersion(const Value: ShortString);
begin
  FAPIHiVer:=SetVersion(Value,FAPILoVer,TAPI_CURRENT_VERSION);
end;

procedure TTAPIService.SetAPILoVersion(const Value: ShortString);
begin
  if FAPIHiVer <=FAPILoVer then FAPIHiVer:=TAPI_CURRENT_VERSION;
  FAPILoVer:=SetVersion(Value,$0010003,FAPIHiVer);
end;

procedure TTAPIService.SetExtHiVersion(const Value: ShortString);
begin
  FExtHiVer:=SetVersion(Value,$0000000,$7FFFFFFF);
end;

procedure TTAPIService.SetExtLoVersion(const Value: ShortString);
begin
  FExtLoVer:=SetVersion(Value,$0000000,FExtHiVer);
end;

procedure TTAPIService.SetFAPIHiVer(const Value: DWord);
begin
  FAPIHiVer := Value;
end;

{$IFDEF TAPI20}
procedure TTAPIService.SetInitOptions(const Value: TLineInitializeExOption);
begin
  FInitOpt:=InitializeExOptionToInt(Value);
end;
{$ENDIF}

{ TTAPILineService }

constructor TTAPILineService.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TTAPILineService.Destroy;
begin
  inherited Destroy;
end;

function TTAPILineService.GetAPIVersion(DeviceID:DWord;var ExtID:TLineExtensionID):DWord;
var R:Longint;
    APIVersion:DWord;
begin
  R:=LineNegotiateAPIVersion(Handle,DeviceID,APILoVer,APIHiVer,APIVersion,ExtID);
  if R<>0 then RaiseTAPILineError(R);
  Result:=APIVersion;
end;

function TTAPILineService.CheckAPIVersion(DeviceID:DWord;var APIVersion:DWord;var ExtID:TTAPIExtensionID):Boolean;
var R:Longint;
begin
  Result:=False;
  APIVersion:=GetAPIVersion(DeviceID,TLineExtensionID(ExtID));
  R:=LineNegotiateAPIVersion(Handle,DeviceID,APILoVer,APIHiVer,APIVersion,TLineExtensionID(ExtID));
  if R =0 then
  begin
    if (APIVersion<=APIHiVer)And(APIVersion >APILoVer) then Result:=True else Result:=False;
  end;
end;

function TTAPILineService.GetCallBack: TTAPICallBack;
begin
  {$IFDEF TAPI20}
  Result:=nil;
  if FInitOpt=LINEINITIALIZEEXOPTION_USEHIDDENWINDOW then
  {$ENDIF}
  Result:=@LineCallback;
end;

procedure TTAPILineService.Initalize;
var R:Longint;
begin
   R:=LineInitialize(@FAppHandle,hInstance,CallBackFunc,PChar(@FAppName),FNumDev);
   if R <>0 then  RaiseTAPILineError(R);
   //GetVersion; ??
end;

{$IFDEF TAPI20}
procedure TTAPILineService.InitalizeEx;
var R:Longint;
    ExParams:pLineInitializeExParams;
begin
  ExParams:=AllocMem(SizeOf(TLineInitializeExParams)+1000);
  ExParams^.dwTotalSize:=SizeOf(TLineInitializeExParams)+1000;
  ExParams^.dwOptions:=FInitOpt;
  try
    R:=LineInitializeEx(PHLINEAPP(@FAppHandle),hInstance,CallBackFunc,PChar(@FAppName),FNumDev,FAPIHiVer,ExParams^);
    if R <>0 then  RaiseTAPILineError(R);
    case ExParams^.dwOptions of
      LINEINITIALIZEEXOPTION_USEEVENT:
      begin
        FEvent:=ExParams^.Handles.hEvent;
        FThread:=TTAPIThread.Create(FAppHandle,FEvent);
      end;
    end;
  finally
    FreeMem(ExParams);
  end;
end;
{$ENDIF}

procedure TTAPILineService.LineCreate(var NewDeviceID: DWord);
begin
  if Assigned(FOnLineCreate) then FOnLineCreate(self,NewDeviceID);
end;

procedure TTAPILineService.LineRequest(var RequestMode, Param2,
  Param3: DWord);
begin
   if Assigned(FOnLineRequest) then FOnLineRequest(self,IntToRequestMode(RequestMode),Param2,Param3);
end;

procedure TTAPILineService.SetFAPIHiVer(const Value: DWord);
begin
  inherited SetFAPIHiVer(Value);
end;

procedure TTAPILineService.Shutdown;
var R:Longint;
begin
  {$IFDEF TAPI20}
  IF assigned(FThread) then FThread.Terminate;
  try
  {$ENDIF}
    R:=LineShutDown(FAppHandle);
    if R <>0 then RaiseTAPILineError(R);
  {$IFDEF TAPI20}
  finally
    FThread.Free;
  end;
  {$ENDIF}
end;

procedure TTAPILineService.LineRemove(var DeviceID: DWord);
begin
  if Assigned(FOnLineRemove) then FOnLineRemove(self,DeviceID);
end;

function TTAPILineService.GetExtVersion(DeviceID,APIVersion: DWord;
  var ExtID: TLineExtensionID): DWord;
var R:LongInt;
    ExtVersion:DWord;
begin
  ExtVersion:=0;
  IF not((ExtID.dwExtensionID0=0) and (ExtID.dwExtensionID1=0) and (ExtID.dwExtensionID2=0) and (ExtID.dwExtensionID3=0)) then
  begin
    if (FExtHiVer<>0)then
    begin
      R:=LineNegotiateExtVersion(Handle,DeviceID,APIVersion,FExtLoVer,FExtHiVer,ExtVersion);
      if R<>0 then
      begin
        if Assigned(OnNegotiateExtVerError)then OnNegotiateExtVerError(self,DeviceID,R);
      end;
    end;
  end;
  Result:=ExtVersion;
end;

procedure TTAPILineService.PerformMsg(Msg: TCMTAPI);
begin
  inherited;
  with Msg.TAPIRec^ do
  begin
    case dwMsg of
      {$IFDEF TAPI20}
      LINE_REMOVE:LineRemove(dwParam1);
      {$ENDIF}
      LINE_REQUEST:LineRequest(dwParam1,dwParam2,dwParam3);
      LINE_CREATE:LineCreate(dwParam1);
    end;
  end;
end;

{ TTAPIPhoneService }

function TTAPIPhoneService.CheckAPIVersion(DeviceID: DWord;
  var APIVersion: DWord; var ExtID: TTAPIExtensionID): Boolean;
var R:Longint;
begin
  Result:=False;
  APIVersion:=GetAPIVersion(DeviceID,TPhoneExtensionID(ExtID));
  R:=PhoneNegotiateAPIVersion(Handle,DeviceID,APILoVer,APIHiVer,APIVersion,TPhoneExtensionID(ExtID));
  if R =0 then
  begin
    if (APIVersion<=APIHiVer)And(APIVersion >APILoVer) then Result:=True else Result:=False;
  end;
end;

constructor TTAPIPhoneService.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TTAPIPhoneService.Destroy;
begin
  inherited Destroy;
end;

function TTAPIPhoneService.GetAPIVersion(DeviceID: DWord;
  var ExtID: TPhoneExtensionID): DWord;
var R:Longint;
    APIVersion:DWord;
begin
  R:=PhoneNegotiateAPIVersion(Handle,DeviceID,APILoVer,APIHiVer,APIVersion,ExtID);
  if R<>0 then RaiseTAPIPhoneError(R);
  Result:=APIVersion;
end;

function TTAPIPhoneService.GetCallBack: TTAPICallBack;
begin
  {$IFDEF TAPI20}
  Result:=nil;
  if FInitOpt=PHONEINITIALIZEEXOPTION_USEHIDDENWINDOW then
  {$ENDIF}
  Result:=@PhoneCallback;
end;

function TTAPIPhoneService.GetExtVersion(DeviceID, APIVersion: DWord;
  var ExtID: TPhoneExtensionID): DWord;
var R:LongInt;
    ExtVersion:DWord;
begin
  ExtVersion:=0;
  IF not((ExtID.dwExtensionID0=0) and (ExtID.dwExtensionID1=0) and (ExtID.dwExtensionID2=0) and (ExtID.dwExtensionID3=0)) then
  begin
    if (FExtHiVer<>0)then
    begin
      R:=PhoneNegotiateExtVersion(Handle,DeviceID,APIVersion,FExtLoVer,FExtHiVer,ExtVersion);
      if R<>0 then
      begin
        if Assigned(OnNegotiateExtVerError)then OnNegotiateExtVerError(self,DeviceID,R);
      end;
    end;
  end;
  Result:=ExtVersion;
end;

procedure TTAPIPhoneService.Initalize;
var R:Longint;
begin
  {$IFDEF CPU32}
  R:=PhoneInitialize(FAppHandle,hInstance,Callbackfunc,PChar(@FAppName),FNumDev);
  {$ENDIF}
  if R <>0 then  RaiseTAPIPhoneError(R);
end;

{$IFDEF TAPI20}
procedure TTAPIPhoneService.InitalizeEx;
var R:Longint;
    ExParams:pPhoneInitializeExParams;
begin
  FNumDev:=900;
  ExParams:=AllocMem(SizeOf(TPhoneInitializeExParams)+1000);
  ExParams^.dwTotalSize:=SizeOf(TPhoneInitializeExParams)+1000;
  ExParams^.dwOptions:=FInitOpt;
  try
    R:=PhoneInitializeEx(FAppHandle,hInstance,Callbackfunc,PChar(@FAppName),FNumDev,FAPIHiVer,ExParams^);
    if R <>0 then  RaiseTAPIPhoneError(R);
    case ExParams^.dwOptions of
      PHONEINITIALIZEEXOPTION_USEEVENT:
      begin
        FEvent:=ExParams^.Handles.hEvent;
        FThread:=TTAPIPhoneThread.Create(FAppHandle,FEvent);
      end;
    end;
  finally
    FreeMem(ExParams);
  end;
end;
{$ENDIF}

procedure TTAPIPhoneService.PerformMsg(Msg: TCMTAPI);
begin
  inherited;
  with Msg.TAPIRec^ do
  begin
    case dwMsg of
      PHONE_CREATE: PhoneCreate(hDevice,dwParam1);
      {$IFDEF TAPI20}
      PHONE_REMOVE: PhoneRemove(dwParam1);
      {$ENDIF}
    end;
  end;
end;

procedure TTAPIPhoneService.PhoneCreate(var hDevice, NewDeviceID: DWord);
begin
  if Assigned(FOnPhoneCreate) then FOnPhoneCreate(self,NewDeviceID);
end;

{$IFDEF TAPI20}
procedure TTAPIPhoneService.PhoneRemove(DeviceID: DWord);
begin
  if Assigned(FOnPhoneRemove) then FOnPhoneRemove(self,DeviceID);
end;
{$ENDIF}

procedure TTAPIPhoneService.Shutdown;
var R:Longint;
begin
  {$IFDEF TAPI20}
  if Assigned(FThread) then
  begin
    FThread.Terminate;
  end;
  try
  {$ENDIF}
    R:=PhoneShutDown(FAppHandle);
    if R <>0 then RaiseTAPIPhoneError(R);
  {$IFDEF TAPI20}
  finally
    FThread.Free;
  end;
  {$ENDIF}
end;

{$ENDIF}
{$ENDIF}
end.
