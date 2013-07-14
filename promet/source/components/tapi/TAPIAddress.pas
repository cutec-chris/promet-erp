{******************************************************************************}
{*        Copyright 1999-2001 by J.Friebel all rights reserved.               *}
{*        Autor           :  Jörg Friebel                                     *}
{*        Compiler        :  Delphi 4 / 5                                     *}
{*        System          :  Windows NT / 2000                                *}
{*        Projekt         :  TAPI Komponenten (TAPI Version 1.4 bis 3.0)      *}
{*        Last Update     :  04.05.2002                                       *}
{*        Version         :  1.60                                             *}
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
unit TAPIAddress;

interface

{$IFDEF WINDOWS}
{$IFDEF CPU32}

{$IFDEF FPC}
{$MODE DELPHI}
{$ENDIF}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  TAPI,TAPISystem,TAPIServices,TAPIDevices,TAPILines,TAPICall;

{$INCLUDE TAPI.INC}

type
  TLineAddressSharing = (lasPrivate,lasBridgedexcl,lasBridgednew,
    lasBridgedshared,lasMonitored);
  TLineAddressState = (asOther,asDevSpecific,asInUseZerro,asInUseOne,
    asInUseMany,asNumCalls,asForward,asTerminals,asCapSchanged);
  TLineAddressStates = Set of TLineAddressState;

  TLineCallInfoState = (cisOther,cisDevSpecific,cisBearerMode,cisRate,
    cisMediaMode,cisAppSpecific,cisCallId,cisRelatedCallId,cisOrigin,cisReason,
    cisCompletionId,cisNumOwnerIncr,cisNumOwnwerDecr,cisNumMonitors,cisTrunk,
    cisCallerId,cisCalledId,cisConnectedId,cisReDirectionId,cisReDirectingId,
    cisDisplay,cisUserUserInfo,cisHighLevelComp,cisLowLevelComp,cisChargingInfo,
    cisTerminal,cisDialParams,cisMonitormodes {$IFDEF TAPI20},cisTreatment,
    cisQOS,cisCallData {$ENDIF});
  TLineCallInfoStates = Set of TLineCallInfoState;

  TLineAddrCapsFlags = Set of (lacfFWDNumRings,lacfPickupGroupId,lacfSecure,
    lacfBlockIdDefault,lacfBlockIdOverride,lacfDialed,lacfOrigOffHook,
    lacfDestOffHook,lacfFWDConsult,lacfSetupConfNull,lacfAutoReconnect,
    lacfCompletionId,lacfTransferHeld,lacfTransferMake,lacfConferenceHeld,
    lacfConferenceMake,lacfPartialDial,lacfFWDStatusValid,lacfFWDInTextAddr,
    lacfFWDBusyNAAddr,lacfAcceptToAlert,lacfConfDrop,lacfPickupCallWait
    {$IFDEF TAPI20},lacfPredictiveDialer,lacfQUEUE,lacfRoutePoint,lacfHoldMakesNew,
    lacfNoInternalCalls,lacfNoExternalCalls,lacfSetCallingId{$ENDIF}
    {$IFDEF TAPI22},lacfACDGroup{$ENDIF});

  TLineRemoveFromConf = (rfcNone,rfcLast,rfcAny);

  TLineTransferMode = (ltmConference,ltmTransfer);
  TLineTransferModes =set of TLineTransferMode;

  TLineParkMode = (lpmDirected,lpmNoDirected);
  TLineParkModes =set of TLineParkMode;

  TLineForwardMode = (lfmUnCond,lfmUnCondInternal,lfmUnCondExternal,
    lfmUnCondSpecific,lfmBusy,lfmBusyInternal,lfmBusyExternal,lfmBusySpecific,
    lfmNoAnsw,lfmNoAnswInternal,lfmNoAnswExternal,lfmNoAnswSpecific,lfmBusyNA,
    lfmBusyNAInternal,lfmBusyNAExternal,lfmBusyNASpecific,lfmUnknown,
    lfmUnavail);
  TLineForwardModes = set of TLineForwardMode;

  TLineAddrFeature = (lafForward,lafMakeCall,lafPickup,lafSetMediaControl,
    lafSetTerminal,lafSetupConf,lafUncompleteCall,lafUnPark
    {$IFDEF TAPI20},lafPickupHeld,lafPickupGroup,lafPickupDirect,
    lafPickupwaiting,lafForwardFWD,lafForwardDND{$ENDIF});
  TLineAddrFeatures = set of TLineAddrFeature;

  TLineTranslateOption = (toCardOverride,toCancelCallWaiting,
    toForceLocal,toForceLd);
  TLineTranslateOptions = set of TLineTranslateOption;

  TLineCallSelect =(lcsAddress,lcsCall,lcsLine{$IFDEF TAPI21},lcsDeviceId{$ENDIF}
    {$IFDEF TAPI30},lcsCallId{$ENDIF});


  TAppNewCallEvent=procedure(Sender:TObject;Call:TTAPICall;AddressID:DWord;Privilege:TLineCallPrivilege)of Object;

type
  TAddressStatus=class(TPersistent)
  private
    FNumInUse,
    FNumActiveCalls,
    FNumOnHoldCalls,
    FNumOnHoldPendCalls:DWord;
    FAddressFeatures:TLineAddrFeatures;
    FNumRingsNoAnswer:DWord;
    FForwardNumEntries:DWord;
    FForward:Array of TLineForward;
    FTerminalModes:Array of TLineTermMode;
    FDevSpecific: Array of Char;
    FDevSpecificSize:DWord;
    procedure SetStatus(ALineAddressStatus:PLineAddressStatus);
    function GetTerminalModes(Index:Integer):TLineTermMode;
  public
    constructor Create(ALine:HLine;AAddressID:DWord);overload;virtual;
    constructor Create(ALineAddressStatus:PLineAddressStatus);overload;
    destructor Destroy;override;
    property TerminalModes[Index:integer]:TLineTermMode read GetTerminalModes;
    procedure GetDevSpecific(var AStr:String);overload;
    procedure GetDevSpecific(var AArray:Array of byte);overload;
  published
    property NumInUse:DWord read FNumInUse;
    property NumActiveCalls:DWord read FNumActiveCalls;
    property NumOnHoldCalls:DWord read FNumOnHoldCalls;
    property NumOnHoldPendCalls:DWord read FNumOnHoldPendCalls;
    property AddressFeatures:TLineAddrFeatures read FAddressFeatures;
    property NumRingsNoAnswer:DWord read FNumRingsNoAnswer;
    property ForwardNumEntries:DWord read FForwardNumEntries;
  end;

type
  PAddressCaps=^TAddressCaps;
  TAddressCaps=class(TPersistent)
  private
    FLineDeviceID: DWord;
    FAddress: String;
    FDevSpecific:String;
    FAddressSharing: TLineAddressSharing;
    FAddressStates: TLineAddressStates;
    FCallInfoStates: TLineCallInfoStates;
    FCalledIDFlags: TLineCallPartyID;
    FCallerIDFlags: TLineCallPartyID;
    FConnectedIDFlags: TLineCallPartyID;
    FRedirectingIDFlags: TLineCallPartyID;
    FRedirectionIDFlags: TLineCallPartyID;
    FCallStates: TLineCallStates;
    FDialToneModes: TLineDialtoneModes;
    FBusyModes: TLineBusyModes;
    FSpecialInfo: TLineSpecialInfo;
    FDisconnectModes :TLineDisconnectModes;
    FMaxNumActiveCalls,
    FMaxNumOnHoldCalls,
    FMaxNumOnHoldPentingCalls,
    FMaxNumConference,
    FMaxNumTransConf:DWord;
    FAddrCapFlags:TLineAddrCapsFlags;
    FCallFeatures:TLineCallFeatures;
    FRemoveFromConfCaps:TLineRemoveFromConf;
    FRemoveFromConfState:TLineCallStates;
    FTransferModes:TLineTransferModes;
    FParkModes:TLineParkModes;
    FForwardModes:TLineForwardModes;
    FMaxForwardEntries:Dword;
    FMaxSpecificEntries:DWord;
    FMinFwdNumRings:DWord;
    FMaxFwdNumRings:DWord;
    FMaxCallCompletions:DWord;
    FCallCompletionConds:TLineCallComplConds;
    FCallCompletionModes:TLineCallComplModes;
    FNumCompletionMessages,
    FCompletionMsgTextEntrySize:Dword;
    FCompletionMsgText:String;
    FAddressFeatures:TLineAddrFeatures;
    {$IFDEF TAPI20}
    FPredictiveAutoTransferStates:TLineCallStates;
    FNumCallTreatments:DWord;
    FCallTreatmentList:TLineCallTreatmentEntry;
    FDeviceClasses:TStringList;
    FMaxCallDataSize:DWord;
    FCallFeatures2:TLineCallFeatures2;
    FMaxNoAnswerTimeout:Dword;
    FConnectedModes:TLineConnectedModes;
    FOfferingModes:TLineOfferingModes;
    FAvailableMediaModes:TLineMediaModes;
    {$ENDIF}
    procedure SetCaps(var Caps:PLineAddresscaps);
  public
    constructor Create(ALineApp:HLineApp;ADeviceID,AAddressID,AAPIVersion,AExtVersion:DWord);overload;virtual;
    constructor Create(ALineAddressCaps:PLINEADDRESSCAPS);overload;
    destructor Destroy;override;
    property CallTreatmentList:TLineCallTreatmentEntry read FCallTreatmentList;
  published
    property LineDeviceID:DWord read FLineDeviceID ;
    property Address:String read FAddress ;
    property DevSpecific:String read FDevSpecific;
    property AddresSharing:TLineAddressSharing read FAddressSharing ;
    property AddressStates:TLineAddressStates read FAddressStates ;
    property CallInfoStates:TLineCallInfoStates read FCallInfoStates;
    property CallerIDFlags:TLineCallPartyID read FCallerIDFlags;
    property CalledIDFlags:TLineCallPartyID read FCalledIDFlags;
    property ConnectedIDFlags:TLineCallPartyID read FConnectedIDFlags;
    property RedirectionIDFlags:TLineCallPartyID read FRedirectionIDFlags;
    property RedirectingIDFlags:TLineCallPartyID read FRedirectingIDFlags;
    property CallStates:TLineCallStates read FCallStates;
    property DialToneModes:TLineDialtoneModes read FDialToneModes;
    property BusyModes:TLineBusyModes read FBusyModes;
    property SpecialInfo:TLineSpecialInfo read FSpecialInfo;
    property DisconnectModes :TLineDisconnectModes read FDisconnectModes;
    property MaxNumActiveCalls:DWord read FMaxNumActiveCalls;
    property MaxNumOnHoldCalls:DWord read FMaxNumOnHoldCalls;
    property MaxNumOnHoldPentingCalls:DWord read FMaxNumOnHoldPentingCalls;
    property MaxNumConference:DWord read FMaxNumConference;
    property MaxNumTransConf:DWord read FMaxNumTransConf;
    property AddrCapFlags:TLineAddrCapsFlags read FAddrCapFlags;
    property CallFeatures:TLineCallFeatures read FCallFeatures;
    property RemoveFromConfCaps:TLineRemoveFromConf read FRemoveFromConfCaps;
    property RemoveFromConfState:TLineCallStates read FRemoveFromConfState;
    property TransferModes:TLineTransferModes read FTransferModes;
    property ParkModes:TLineParkModes read FParkModes;
    property ForwardModes:TLineForwardModes read FForwardModes;
    property MaxForwardEntries:DWord read FMaxForwardEntries;
    property MaxSpecificEntries:DWord read FMaxSpecificEntries;
    property MinFwdNumRings:DWord read FMinFwdNumRings;
    property MaxFwdNumRings:DWord read FMaxFwdNumRings;
    property MaxCallCompletions:DWord read FMaxCallCompletions;
    property CallCompletionConds:TLineCallComplConds read FCallCompletionConds;
    property CallCompletionModes:TLineCallComplModes read FCallCompletionModes;
    property NumCompletionMessages:DWord read FNumCompletionMessages;
    property CompletionMsgTextEntrySize:DWord read FCompletionMsgTextEntrySize;
    property CompletionMsgText:String read FCompletionMsgText;
    property AddressFeatures:TLineAddrFeatures read FAddressFeatures;
    {$IFDEF TAPI20}
    property PredictiveAutoTransferStates:TLineCallStates read FPredictiveAutoTransferStates;
    property NumCallTreatments:DWord read FNumCallTreatments;
    property DeviceClasses:TStringList read FDeviceClasses;
    property MaxCallDataSize:DWord read FMaxCallDataSize;
    property CallFeatures2:TLineCallFeatures2 read FCallFeatures2;
    property MaxNoAnswerTimeout:Dword read FMaxNoAnswerTimeout;
    property ConnectedModes:TLineConnectedModes read FConnectedModes;
    property OfferingModes:TLineOfferingModes read FOfferingModes;
    property AvailableMediaModes:TLineMediaModes read FAvailableMediaModes;
    {$ENDIF}
  end;

type
  TCMLineOpen=packed record
    Msg: Cardinal;
    DevID: DWord;
    LineHandle: THandle;
    Result: Longint;
  end;

type
  TTAPIAddress = class(TTAPIComponent)
  private
    FAddressCaps:TAddressCaps;
    FAddressMode:TLineAddressMode;
    FNumRings:DWord;
    FLine:TTAPILine;
    FOutBoundCall:TTAPICall;
    FInBoundCall:TTAPICall;
    FMonitorCall:TTAPICall;
    FAddressStatus:TAddressStatus;
    FAddressID:DWord;
    FAddress:String;
    FTranslateOptions:DWord;
    FCountryCode:DWord;
    FAddressStateMessages:TLineAddressStates;
    FOnForward: TNotifyEvent;
    FOnInUseMany: TNotifyEvent;
    FOnDevSpecific: TNotifyEvent;
    FOnTerminals: TNotifyEvent;
    FOnOther: TNotifyEvent;
    FOnInUseOne: TNotifyEvent;
    FOnNumCalls: TNotifyEvent;
    FOnInUseZero: TNotifyEvent;
    FOnCapsChanged: TNotifyEvent;
    FOnAppNewCall:TAppNewCallEvent;
    FActive: Boolean;
    //FAgent: TTAPILineAgent;
    //FACDProxy:TACDProxy;
    function GetAddressCaps: TAddressCaps;
    procedure SetAddressID(const Value: DWord);
    function GetDialableAdress: String;
    function GetTranslateOptions: TLineTranslateOptions;
    procedure SetTranslateOptions(const Value: TLineTranslateOptions);
    function GetAddressStatus: TAddressStatus;
    function GetNumRings: DWord;
    procedure SetNumRings(const Value: DWord);
    procedure SetLine(const Value: TTAPILine);
    procedure SetActive(const Value: Boolean);
    //procedure SetAgent(const Value: TTAPILineAgent);
  protected
    procedure Notification(AComponent:TComponent; Operation :TOperation); override;
    procedure StateChange(AddressStatus:DWord);
    procedure AppNewCall(hDevice,dwParam1,dwParam2,dwParam3:LongWord);
  public
    constructor Create(AOwner:TComponent);override;
    destructor Destroy;override;
    function GetStateMsg:DWord;
    function GetMode:DWord;
    procedure MakeCall;
    procedure Dial;//(DestAddress:String;CountryCode:DWord);
    procedure SetStatusMessages;
    procedure GetID(var ACall:TTAPICall;var AddressID:Dword;Select:TLineCallSelect; var VarStr:TVarString);
    property Caps:TAddressCaps read GetAddressCaps ;
    property Status:TAddressStatus read GetAddressStatus;
    property DialableAddress:String read GetDialableAdress;
    property Active:Boolean read FActive write SetActive;
    procedure PerformMsg(Msg: TCMTAPI);override;
    procedure GetAddressID(var AddrID: PChar);
  published
    property OnCapsChanged:TNotifyEvent read FOnCapsChanged write FOnCapsChanged;
    property OnDevSpecific:TNotifyEvent read FOnDevSpecific write FOnDevSpecific;
    property OnForward:TNotifyEvent read FOnForward write FOnForward;
    property OnInUseMany:TNotifyEvent read FOnInUseMany write FOnInUseMany;
    property OnInUseOne:TNotifyEvent read FOnInUseOne  write FOnInUseOne;
    property OnInuseZero:TNotifyEvent read FOnInuseZero write FOnInuseZero;
    property OnNumCalls:TNotifyEvent read FOnNumCalls write FOnNumCalls;
    property OnOther:TNotifyEvent read FOnOther write FOnOther;
    property OnTerminals:TNotifyEvent read FOnTerminals write FOnTerminals;
    property OnAppNewCall:TAppNewCallEvent read FOnAppNewCall write FOnAppNewCall;
    property Line:TTAPILine read FLine write SetLine;
    {$IFDEF TAPI20}
    //property Agent:TTAPILineAgent read FAgent write SetAgent;
    //property ACDProxy:TACDProxy read FACDProxy write FACDProxy;
    {$ENDIF}
    property Address:String read FAddress write FAddress ;
    property TranslateOptions:TLineTranslateOptions read GetTranslateOptions write SetTranslateOptions;
    property CountryCode:DWord read  FCountryCode write  FCountryCode default 0;
    property Mode:TLineAddressMode read FAddressMode write FAddressMode default amDialableAddr;
    property NumRings:DWord read GetNumRings write SetNumRings default 0;
    property InboundCall:TTAPICall read FInBoundCall write FInBoundCall;
    property OutboundCall:TTAPICall read FOutBoundCall write FOutBoundCall;
    property MonitorCall:TTAPICall read FMonitorCall write FMonitorCall;
    property ID:DWord read FAddressID write SetAddressID default 0;
  end;


function CallSelectToInt(Value:TLineCallSelect):LongWord;
function TranslateOptionsToInt(Value:TLineTranslateOptions):LongWord;
function IntToTranslateOptions(Value:LongWord):TLineTranslateOptions;
function IntToAddrFeatures(Value:LongWord):TLineAddrFeatures;
function IntToForwardModes(Value:LongWord):TLineForwardModes;
function IntToParkModes(Value:LongWord):TLineParkModes;
function IntToTransferModes(Value:LongWord):TLineTransferModes;
function IntToAddressSharing(Value:LongWord):TLineAddressSharing;
function IntToAddressStates(Value:LongWord):TLineAddressStates;
function AddressStatesToInt(Value:TLineAddressStates):LongWord;
function IntToCallInfoStates(Value:LongWord):TLineCallInfoStates;
function IntToAddrCapsFlags(Value:LongWord):TLineAddrCapsFlags;
function IntToRemoveFromConf(Value:LongWord):TLineRemoveFromConf;
function ForwardModeToInt(Value:TLineForwardMode):LongWord;

procedure Register;

{$ENDIF}
{$ENDIF}

implementation

{$IFDEF WINDOWS}
{$IFDEF CPU32}

uses {$IFDEF VER120}D4Comp,{$ENDIF}TAPIErr,TAPIHelpFunc;

procedure Register;
begin
{$IFDEF TAPI30}
  RegisterComponents('TAPI30', [TTAPIAddress]);
{$ELSE}
{$IFDEF TAPI22}
  RegisterComponents('TAPI22', [TTAPIAddress]);
{$ELSE}
{$IFDEF TAPI21}
  RegisterComponents('TAPI21', [TTAPIAddress]);
{$ELSE}
{$IFDEF TAPI20}
  RegisterComponents('TAPI20', [TTAPIAddress]);
{$ELSE}
  RegisterComponents('TAPI', [TTAPIAddress]);
{$ENDIF}
{$ENDIF}
{$ENDIF}
{$ENDIF}
end;

function CallSelectToInt(Value:TLineCallSelect):LongWord;
begin
  Result:=0;
  case Value of
    lcsAddress:Result:=LINECALLSELECT_ADDRESS;
    lcsCall:Result:=LINECALLSELECT_CALL;
    lcsLine:Result:=LINECALLSELECT_LINE;
    {$IFDEF TAPI21}
    lcsDeviceId:Result:=LINECALLSELECT_DEVICEID;
    {$ENDIF}
    {$IFDEF TAPI30}
    lcsCallId:Result:=LINECALLSELECT_CALLID;
    {$ENDIF}
  end;
end;

function TranslateOptionsToInt(Value:TLineTranslateOptions):LongWord;
begin
  Result:=0;
  if toCardOverride in Value then Result:=Result or LINETRANSLATEOPTION_CARDOVERRIDE;
  if toCancelCallWaiting in Value then Result:=Result or  LINETRANSLATEOPTION_CANCELCALLWAITING;
  if toForceLocal in Value then Result:=Result or LINETRANSLATEOPTION_FORCELOCAL;
  if toForceLd in Value then Result:=Result or LINETRANSLATEOPTION_FORCELD;
end;

function IntToTranslateOptions(Value:LongWord):TLineTranslateOptions;
begin
  Result:=[];
  IF (Not((LINETRANSLATEOPTION_CARDOVERRIDE xor $FFFFFFFF) or Value ))=0 then Result:=Result+[toCardOverride];
  IF (Not((LINETRANSLATEOPTION_CANCELCALLWAITING xor $FFFFFFFF) or Value ))=0 then Result:=Result+[toCancelCallWaiting];
  IF (Not((LINETRANSLATEOPTION_FORCELOCAL xor $FFFFFFFF) or Value ))=0 then Result:=Result+[toForceLocal];
  IF (Not((LINETRANSLATEOPTION_FORCELD xor $FFFFFFFF) or Value ))=0 then Result:=Result+[toForceLd];
end;

function IntToAddrFeatures(Value:LongWord):TLineAddrFeatures;
begin
  Result:=[];
  IF (Not((LINEADDRFEATURE_FORWARD xor $FFFFFFFF) or Value ))=0 then Result:=Result+[lafForward];
  IF (Not((LINEADDRFEATURE_MAKECALL xor $FFFFFFFF) or Value ))=0 then Result:=Result+[lafMakeCall];
  IF (Not((LINEADDRFEATURE_PICKUP xor $FFFFFFFF) or Value ))=0 then Result:=Result+[lafPickup];
  IF (Not((LINEADDRFEATURE_SETMEDIACONTROL xor $FFFFFFFF) or Value ))=0 then Result:=Result+[lafSetMediaControl];
  IF (Not((LINEADDRFEATURE_SETTERMINAL xor $FFFFFFFF) or Value ))=0 then Result:=Result+[lafSetTerminal];
  IF (Not((LINEADDRFEATURE_SETUPCONF xor $FFFFFFFF) or Value ))=0 then Result:=Result+[lafSetupConf];
  IF (Not((LINEADDRFEATURE_UNCOMPLETECALL xor $FFFFFFFF) or Value ))=0 then Result:=Result+[lafUncompleteCall];
  IF (Not((LINEADDRFEATURE_UNPARK xor $FFFFFFFF) or Value ))=0 then Result:=Result+[lafUnPark];
  {$IFDEF TAPI20}
  IF (Not((LINEADDRFEATURE_PICKUPHELD xor $FFFFFFFF) or Value ))=0 then Result:=Result+[lafPickupHeld];
  IF (Not((LINEADDRFEATURE_PICKUPGROUP xor $FFFFFFFF) or Value ))=0 then Result:=Result+[lafPickupGroup];
  IF (Not((LINEADDRFEATURE_PICKUPDIRECT xor $FFFFFFFF) or Value ))=0 then Result:=Result+[lafPickupDirect];
  IF (Not((LINEADDRFEATURE_PICKUPWAITING xor $FFFFFFFF) or Value ))=0 then Result:=Result+[lafPickupwaiting];
  IF (Not((LINEADDRFEATURE_FORWARDFWD xor $FFFFFFFF) or Value ))=0 then Result:=Result+[lafForwardFWD];
  IF (Not((LINEADDRFEATURE_FORWARDDND xor $FFFFFFFF) or Value ))=0 then Result:=Result+[lafForwardDND];
  {$ENDIF}
end;

function IntToForwardModes(Value:LongWord):TLineForwardModes;
begin
  result:=[];
  IF (Not((LINEFORWARDMODE_UNCOND xor $FFFFFFFF) or Value ))=0 then Result:=Result+[lfmUnCond];
  IF (Not((LINEFORWARDMODE_UNCONDINTERNAL xor $FFFFFFFF) or Value ))=0 then Result:=Result+[lfmUnCondInternal];
  IF (Not((LINEFORWARDMODE_UNCONDEXTERNAL xor $FFFFFFFF) or Value ))=0 then Result:=Result+[lfmUnCondExternal];
  IF (Not((LINEFORWARDMODE_UNCONDSPECIFIC xor $FFFFFFFF) or Value ))=0 then Result:=Result+[lfmUnCondSpecific];
  IF (Not((LINEFORWARDMODE_BUSY xor $FFFFFFFF) or Value ))=0 then Result:=Result+[lfmBusy];
  IF (Not((LINEFORWARDMODE_BUSYINTERNAL xor $FFFFFFFF) or Value ))=0 then Result:=Result+[lfmBusyInternal];
  IF (Not((LINEFORWARDMODE_BUSYEXTERNAL xor $FFFFFFFF) or Value ))=0 then Result:=Result+[lfmBusyExternal];
  IF (Not((LINEFORWARDMODE_BUSYSPECIFIC xor $FFFFFFFF) or Value ))=0 then Result:=Result+[lfmBusySpecific];
  IF (Not((LINEFORWARDMODE_NOANSW xor $FFFFFFFF) or Value ))=0 then Result:=Result+[lfmNoAnsw];
  IF (Not((LINEFORWARDMODE_NOANSWINTERNAL xor $FFFFFFFF) or Value ))=0 then Result:=Result+[lfmNoAnswInternal];
  IF (Not((LINEFORWARDMODE_NOANSWEXTERNAL xor $FFFFFFFF) or Value ))=0 then Result:=Result+[lfmNoAnswExternal];
  IF (Not((LINEFORWARDMODE_NOANSWSPECIFIC xor $FFFFFFFF) or Value ))=0 then Result:=Result+[lfmNoAnswSpecific];
  IF (Not((LINEFORWARDMODE_BUSYNA xor $FFFFFFFF) or Value ))=0 then Result:=Result+[lfmBusyNA];
  IF (Not((LINEFORWARDMODE_BUSYNAINTERNAL xor $FFFFFFFF) or Value ))=0 then Result:=Result+[lfmBusyNAInternal];
  IF (Not((LINEFORWARDMODE_BUSYNAEXTERNAL xor $FFFFFFFF) or Value ))=0 then Result:=Result+[lfmBusyNAExternal];
  IF (Not((LINEFORWARDMODE_BUSYNASPECIFIC xor $FFFFFFFF) or Value ))=0 then Result:=Result+[lfmBusyNASpecific];
  IF (Not((LINEFORWARDMODE_UNKNOWN xor $FFFFFFFF) or Value ))=0 then Result:=Result+[lfmUnknown];
  IF (Not((LINEFORWARDMODE_UNAVAIL xor $FFFFFFFF) or Value ))=0 then Result:=Result+[lfmUnavail];
end;

function ForwardModeToInt(Value:TLineForwardMode):LongWord;
begin
  Result:=0;
  case Value of
    lfmUnCond : Result:=LINEFORWARDMODE_UNCOND;
    lfmUnCondInternal : Result:=LINEFORWARDMODE_UNCONDINTERNAL;
    lfmUnCondExternal : Result:=LINEFORWARDMODE_UNCONDEXTERNAL;
    lfmUnCondSpecific : Result:=LINEFORWARDMODE_UNCONDSPECIFIC;
    lfmBusy : Result:=LINEFORWARDMODE_BUSY;
    lfmBusyInternal : Result:=LINEFORWARDMODE_BUSYINTERNAL;
    lfmBusyExternal : Result:=LINEFORWARDMODE_BUSYEXTERNAL;
    lfmBusySpecific : Result:=LINEFORWARDMODE_BUSYSPECIFIC;
    lfmNoAnsw : Result:=LINEFORWARDMODE_NOANSW;
    lfmNoAnswInternal : Result:=LINEFORWARDMODE_NOANSWINTERNAL;
    lfmNoAnswExternal : Result:=LINEFORWARDMODE_NOANSWEXTERNAL;
    lfmNoAnswSpecific : Result:=LINEFORWARDMODE_NOANSWSPECIFIC;
    lfmBusyNA : Result:=LINEFORWARDMODE_BUSYNA;
    lfmBusyNAInternal : Result:=LINEFORWARDMODE_BUSYNAINTERNAL;
    lfmBusyNAExternal : Result:=LINEFORWARDMODE_BUSYNAEXTERNAL;
    lfmBusyNASpecific : Result:=LINEFORWARDMODE_BUSYNASPECIFIC;
    lfmUnknown : Result:=LINEFORWARDMODE_UNKNOWN;
    lfmUnavail : Result:=LINEFORWARDMODE_UNAVAIL;
  end;
end;


function IntToParkModes(Value:LongWord):TLineParkModes;
begin
  result:=[];
  IF (Not((LINEPARKMODE_DIRECTED xor $FFFFFFFF) or Value ))=0 then Result:=Result+[lpmDirected];
  IF (Not((LINEPARKMODE_NONDIRECTED xor $FFFFFFFF) or Value ))=0 then Result:=Result+[lpmNoDirected];
end;

function IntToTransferModes(Value:LongWord):TLineTransferModes;
begin
  result:=[];
  IF (Not((LINETRANSFERMODE_CONFERENCE xor $FFFFFFFF) or Value ))=0 then Result:=Result+[ltmConference];
  IF (Not((LINETRANSFERMODE_TRANSFER xor $FFFFFFFF) or Value ))=0 then Result:=Result+[ltmTransfer];
end;

function IntToAddressSharing(Value:LongWord):TLineAddressSharing;
begin
  Result:=lasPrivate;
  case Value of
    LINEADDRESSSHARING_PRIVATE:Result:=lasPrivate;
    LINEADDRESSSHARING_BRIDGEDEXCL:Result:=lasBridgedexcl;
    LINEADDRESSSHARING_BRIDGEDNEW:Result:=lasBridgednew;
    LINEADDRESSSHARING_BRIDGEDSHARED:Result:=lasBridgedshared;
    LINEADDRESSSHARING_MONITORED:Result:=lasMonitored;
  end;
end;

function IntToRemoveFromConf(Value:LongWord):TLineRemoveFromConf;
begin
  Result:=rfcNone;
  case Value of
    LINEREMOVEFROMCONF_NONE:Result:=rfcNone;
    LINEREMOVEFROMCONF_LAST:Result:=rfcLast;
    LINEREMOVEFROMCONF_ANY:Result:=rfcAny;
  end;
end;

function IntToAddressStates(Value:LongWord):TLineAddressStates;
begin
  Result:=[];
  IF (Not((LINEADDRESSSTATE_OTHER xor $FFFFFFFF) or Value ))=0 then Result:=Result+[asOther];
  IF (Not((LINEADDRESSSTATE_DEVSPECIFIC xor $FFFFFFFF) or Value ))=0 then Result:=Result+[asDevSpecific];
  IF (Not((LINEADDRESSSTATE_INUSEZERO xor $FFFFFFFF) or Value ))=0 then Result:=Result+[asInUseZerro];
  IF (Not((LINEADDRESSSTATE_INUSEONE xor $FFFFFFFF) or Value ))=0 then Result:=Result+[asInUseOne];
  IF (Not((LINEADDRESSSTATE_INUSEMANY  xor $FFFFFFFF) or Value ))=0 then Result:=Result+[asInUseMany];
  IF (Not((LINEADDRESSSTATE_NUMCALLS xor $FFFFFFFF) or Value ))=0 then Result:=Result+[asNumCalls];
  IF (Not((LINEADDRESSSTATE_FORWARD xor $FFFFFFFF) or Value ))=0 then Result:=Result+[asForward];
  IF (Not((LINEADDRESSSTATE_TERMINALS xor $FFFFFFFF) or Value ))=0 then Result:=Result+[asTerminals];
  IF (Not((LINEADDRESSSTATE_CAPSCHANGE  xor $FFFFFFFF) or Value ))=0 then Result:=Result+[asCapSchanged];
end;

function AddressStatesToInt(Value:TLineAddressStates):LongWord;
begin
  Result:=0;
  IF asOther in Value then Result:=Result or LINEADDRESSSTATE_OTHER;
  IF asDevSpecific in Value then Result:=Result or LINEADDRESSSTATE_DEVSPECIFIC;
  IF asInUseZerro in Value then Result:=Result or LINEADDRESSSTATE_INUSEZERO;
  IF asInUseOne in Value then Result:=Result or LINEADDRESSSTATE_INUSEONE;
  IF asInUseMany in Value then Result:=Result or LINEADDRESSSTATE_INUSEMANY;
  IF asNumCalls in Value then Result:=Result or LINEADDRESSSTATE_NUMCALLS;
  IF asForward in Value then Result:=Result or LINEADDRESSSTATE_FORWARD;
  IF asTerminals in Value then Result:=Result or LINEADDRESSSTATE_TERMINALS;
  IF asCapSchanged in Value then Result:=Result or LINEADDRESSSTATE_CAPSCHANGE;
end;

function IntToCallInfoStates(Value:LongWord):TLineCallInfoStates;
begin
  Result:=[];
  IF (Not((LINECALLINFOSTATE_OTHER xor $FFFFFFFF) or Value ))=0 then Result:=Result+[cisOTHER];
  IF (Not((LINECALLINFOSTATE_DEVSPECIFIC xor $FFFFFFFF) or Value ))=0 then Result:=Result+[cisDEVSPECIFIC];
  IF (Not((LINECALLINFOSTATE_BEARERMODE xor $FFFFFFFF) or Value ))=0 then Result:=Result+[cisBEARERMODE];
  IF (Not((LINECALLINFOSTATE_RATE xor $FFFFFFFF) or Value ))=0 then Result:=Result+[cisRATE];
  IF (Not((LINECALLINFOSTATE_MEDIAMODE xor $FFFFFFFF) or Value ))=0 then Result:=Result+[cisMEDIAMODE];
  IF (Not((LINECALLINFOSTATE_APPSPECIFIC xor $FFFFFFFF) or Value ))=0 then Result:=Result+[cisAPPSPECIFIC];
  IF (Not((LINECALLINFOSTATE_CALLID xor $FFFFFFFF) or Value ))=0 then Result:=Result+[cisCALLID];
  IF (Not((LINECALLINFOSTATE_RELATEDCALLID xor $FFFFFFFF) or Value ))=0 then Result:=Result+[cisRELATEDCALLID];
  IF (Not((LINECALLINFOSTATE_ORIGIN xor $FFFFFFFF) or Value ))=0 then Result:=Result+[cisORIGIN];
  IF (Not((LINECALLINFOSTATE_REASON xor $FFFFFFFF) or Value ))=0 then Result:=Result+[cisREASON];
  IF (Not((LINECALLINFOSTATE_COMPLETIONID xor $FFFFFFFF) or Value ))=0 then Result:=Result+[cisCOMPLETIONID];
  IF (Not((LINECALLINFOSTATE_NUMOWNERINCR xor $FFFFFFFF) or Value ))=0 then Result:=Result+[cisNUMOWNERINCR];
  IF (Not((LINECALLINFOSTATE_NUMOWNERDECR xor $FFFFFFFF) or Value ))=0 then Result:=Result+[cisNumOwnwerDecr];
  IF (Not((LINECALLINFOSTATE_NUMMONITORS xor $FFFFFFFF) or Value ))=0 then Result:=Result+[cisNUMMONITORS];
  IF (Not((LINECALLINFOSTATE_TRUNK xor $FFFFFFFF) or Value ))=0 then Result:=Result+[cisTRUNK];
  IF (Not((LINECALLINFOSTATE_CALLERID xor $FFFFFFFF) or Value ))=0 then Result:=Result+[cisCALLERID];
  IF (Not((LINECALLINFOSTATE_CALLEDID xor $FFFFFFFF) or Value ))=0 then Result:=Result+[cisCALLEDID];
  IF (Not((LINECALLINFOSTATE_CONNECTEDID xor $FFFFFFFF) or Value ))=0 then Result:=Result+[cisCONNECTEDID];
  IF (Not((LINECALLINFOSTATE_REDIRECTIONID xor $FFFFFFFF) or Value ))=0 then Result:=Result+[cisREDIRECTIONID];
  IF (Not((LINECALLINFOSTATE_REDIRECTINGID  xor $FFFFFFFF) or Value ))=0 then Result:=Result+[cisREDIRECTINGID];
  IF (Not((LINECALLINFOSTATE_DISPLAY xor $FFFFFFFF) or Value ))=0 then Result:=Result+[cisDISPLAY];
  IF (Not((LINECALLINFOSTATE_USERUSERINFO xor $FFFFFFFF) or Value ))=0 then Result:=Result+[cisUSERUSERINFO];
  IF (Not((LINECALLINFOSTATE_HIGHLEVELCOMP xor $FFFFFFFF) or Value ))=0 then Result:=Result+[cisHIGHLEVELCOMP];
  IF (Not((LINECALLINFOSTATE_LOWLEVELCOMP xor $FFFFFFFF) or Value ))=0 then Result:=Result+[cisLOWLEVELCOMP];
  IF (Not((LINECALLINFOSTATE_CHARGINGINFO  xor $FFFFFFFF) or Value ))=0 then Result:=Result+[cisCHARGINGINFO];
  IF (Not((LINECALLINFOSTATE_TERMINAL xor $FFFFFFFF) or Value ))=0 then Result:=Result+[cisTERMINAL];
  IF (Not((LINECALLINFOSTATE_DIALPARAMS xor $FFFFFFFF) or Value ))=0 then Result:=Result+[cisDIALPARAMS];
  IF (Not((LINECALLINFOSTATE_MONITORMODES xor $FFFFFFFF) or Value ))=0 then Result:=Result+[cisMONITORMODES];
  {$IFDEF TAPI20}
  IF (Not((LINECALLINFOSTATE_TREATMENT  xor $FFFFFFFF) or Value ))=0 then Result:=Result+[cisTREATMENT];
  IF (Not((LINECALLINFOSTATE_QOS xor $FFFFFFFF) or Value ))=0 then Result:=Result+[cisQOS];
  IF (Not((LINECALLINFOSTATE_CALLDATA xor $FFFFFFFF) or Value ))=0 then Result:=Result+[cisCALLDATA];
  {$ENDIF}
end;

function IntToAddrCapsFlags(Value:LongWord):TLineAddrCapsFlags;
begin
  Result:=[];
  IF (Not((LINEADDRCAPFLAGS_FWDNUMRINGS xor $FFFFFFFF) or Value ))=0 then Result:=Result+[lacfFWDNUMRINGS];
  IF (Not((LINEADDRCAPFLAGS_PICKUPGROUPID xor $FFFFFFFF) or Value ))=0 then Result:=Result+[lacfPICKUPGROUPID];
  IF (Not((LINEADDRCAPFLAGS_SECURE xor $FFFFFFFF) or Value ))=0 then Result:=Result+[lacfSECURE];
  IF (Not((LINEADDRCAPFLAGS_BLOCKIDDEFAULT xor $FFFFFFFF) or Value ))=0 then Result:=Result+[lacfBLOCKIDDEFAULT];
  IF (Not((LINEADDRCAPFLAGS_BLOCKIDOVERRIDE xor $FFFFFFFF) or Value ))=0 then Result:=Result+[lacfBLOCKIDOVERRIDE];
  IF (Not((LINEADDRCAPFLAGS_DIALED xor $FFFFFFFF) or Value ))=0 then Result:=Result+[lacfDIALED];
  IF (Not((LINEADDRCAPFLAGS_ORIGOFFHOOK xor $FFFFFFFF) or Value ))=0 then Result:=Result+[lacfORIGOFFHOOK];
  IF (Not((LINEADDRCAPFLAGS_DESTOFFHOOK xor $FFFFFFFF) or Value ))=0 then Result:=Result+[lacfDESTOFFHOOK];
  IF (Not((LINEADDRCAPFLAGS_FWDCONSULT xor $FFFFFFFF) or Value ))=0 then Result:=Result+[lacfFWDCONSULT];
  IF (Not((LINEADDRCAPFLAGS_SETUPCONFNULL xor $FFFFFFFF) or Value ))=0 then Result:=Result+[lacfSETUPCONFNULL];
  IF (Not((LINEADDRCAPFLAGS_AUTORECONNECT xor $FFFFFFFF) or Value ))=0 then Result:=Result+[lacfAUTORECONNECT];
  IF (Not((LINEADDRCAPFLAGS_COMPLETIONID xor $FFFFFFFF) or Value ))=0 then Result:=Result+[lacfCOMPLETIONID];
  IF (Not((LINEADDRCAPFLAGS_TRANSFERHELD xor $FFFFFFFF) or Value ))=0 then Result:=Result+[lacfTRANSFERHELD];
  IF (Not((LINEADDRCAPFLAGS_TRANSFERMAKE xor $FFFFFFFF) or Value ))=0 then Result:=Result+[lacfTRANSFERMAKE];
  IF (Not((LINEADDRCAPFLAGS_CONFERENCEHELD xor $FFFFFFFF) or Value ))=0 then Result:=Result+[lacfCONFERENCEHELD];
  IF (Not((LINEADDRCAPFLAGS_CONFERENCEMAKE xor $FFFFFFFF) or Value ))=0 then Result:=Result+[lacfCONFERENCEMAKE];
  IF (Not((LINEADDRCAPFLAGS_PARTIALDIAL xor $FFFFFFFF) or Value ))=0 then Result:=Result+[lacfPARTIALDIAL];
  IF (Not((LINEADDRCAPFLAGS_FWDSTATUSVALID xor $FFFFFFFF) or Value ))=0 then Result:=Result+[lacfFWDSTATUSVALID];
  IF (Not((LINEADDRCAPFLAGS_FWDINTEXTADDR xor $FFFFFFFF) or Value ))=0 then Result:=Result+[lacfFWDINTEXTADDR];
  IF (Not((LINEADDRCAPFLAGS_FWDBUSYNAADDR xor $FFFFFFFF) or Value ))=0 then Result:=Result+[lacfFWDBUSYNAADDR];
  IF (Not((LINEADDRCAPFLAGS_ACCEPTTOALERT xor $FFFFFFFF) or Value ))=0 then Result:=Result+[lacfACCEPTTOALERT];
  IF (Not((LINEADDRCAPFLAGS_CONFDROP xor $FFFFFFFF) or Value ))=0 then Result:=Result+[lacfCONFDROP];
  IF (Not((LINEADDRCAPFLAGS_PICKUPCALLWAIT xor $FFFFFFFF) or Value ))=0 then Result:=Result+[lacfPICKUPCALLWAIT];
{$IFDEF TAPI20}
  IF (Not((LINEADDRCAPFLAGS_PREDICTIVEDIALER xor $FFFFFFFF) or Value ))=0 then Result:=Result+[lacfPREDICTIVEDIALER];
  IF (Not((LINEADDRCAPFLAGS_QUEUE xor $FFFFFFFF) or Value ))=0 then Result:=Result+[lacfQUEUE];
  IF (Not((LINEADDRCAPFLAGS_ROUTEPOINT xor $FFFFFFFF) or Value ))=0 then Result:=Result+[lacfROUTEPOINT];
  IF (Not((LINEADDRCAPFLAGS_HOLDMAKESNEW xor $FFFFFFFF) or Value ))=0 then Result:=Result+[lacfHOLDMAKESNEW];
  IF (Not((LINEADDRCAPFLAGS_NOINTERNALCALLS xor $FFFFFFFF) or Value ))=0 then Result:=Result+[lacfNOINTERNALCALLS];
  IF (Not((LINEADDRCAPFLAGS_NOEXTERNALCALLS xor $FFFFFFFF) or Value ))=0 then Result:=Result+[lacfNOEXTERNALCALLS];
  IF (Not((LINEADDRCAPFLAGS_SETCALLINGID xor $FFFFFFFF) or Value ))=0 then Result:=Result+[lacfSETCALLINGID];
{$ENDIF}
{$IFDEF TAPI22}
  IF (Not((LINEADDRCAPFLAGS_ACDGROUP xor $FFFFFFFF) or Value ))=0 then Result:=Result+[lacfACDGROUP];
{$ENDIF}
end;

{ TAddressStatus }

constructor TAddressStatus.Create(ALine: HLine; AAddressID: DWord);
var FLineAddressStatus:PLineAddressStatus;
    R:LongWord;
begin
  GetMem(FLineAddressStatus,SizeOf(TLineAddressStatus)+1000);
  FLineAddressStatus^.dwTotalSize:=SizeOF(TLineAddressStatus)+1000;
  R:=LineGetAddressStatus(ALine,AAddressID,FLineAddressStatus);
  If R<>0 then RaiseTAPILineError(R);
  SetStatus(FLineAddressStatus);
  FreeMem(FLineAddressStatus);
end;

constructor TAddressStatus.Create(ALineAddressStatus: PLineAddressStatus);
begin
  SetStatus(ALineAddressStatus);
end;

destructor TAddressStatus.Destroy;
begin
  inherited;

end;

procedure TAddressStatus.GetDevSpecific(var AStr: String);
begin
  if FDevSpecificSize > 0 then
    AStr:=PChar(FDevSpecific);
end;

procedure TAddressStatus.GetDevSpecific(var AArray: array of byte);
begin

end;

function TAddressStatus.GetTerminalModes(Index: Integer): TLineTermMode;
begin
  try
    Result:=FTerminalModes[Index];
  except
    Result:=tmmNoDef;
  end;
end;

procedure TAddressStatus.SetStatus(ALineAddressStatus: PLineAddressStatus);
var i:Integer;
begin
  FNumInUse:=ALineAddressStatus^.dwNumInUse;
  FNumActiveCalls:=ALineAddressStatus^.dwNumActiveCalls;
  FNumOnHoldCalls:=ALineAddressStatus^.dwNumOnHoldCalls;
  FNumOnHoldPendCalls:=ALineAddressStatus^.dwNumOnHoldPendCalls;
  FAddressFeatures:=IntToAddrFeatures(ALineAddressStatus^.dwAddressFeatures);
  FNumRingsNoAnswer:=ALineAddressStatus^.dwNumRingsNoAnswer;
  FForwardNumEntries:=ALineAddressStatus^.dwForwardNumEntries;
  //GetDataFromTAPIStruct(
  if FForwardNumEntries > 0 then
  begin
    SetLength(FForward,SizeOf(TLineForward)*FForwardNumEntries);
    for i:=0 To FForwardNumEntries do
    begin

    end;
    //ALineAddressStatus^.dwForwardSize
  end;
  if ALineAddressStatus^.dwTerminalModesSize > 0 then
  begin
    SetLength(FTerminalModes,ALineAddressStatus^.dwTerminalModesSize);
    StrCopy(PChar(FTerminalModes),Pchar(ALineAddressStatus)+ALineAddressStatus^.dwTerminalModesOffset);
  end;
  if ALineAddressStatus^.dwDevSpecificOffset > 0 then
  begin
    FDevSpecificSize:= ALineAddressStatus^.dwDevSpecificSize;
    SetLength(FDevSpecific,FDevSpecificSize);
    StrCopy(PChar(FDevSpecific),Pchar(ALineAddressStatus)+ALineAddressStatus^.dwDevSpecificOffset);
  end
  else
  begin
    SetLength(FDevSpecific,0);
    FDevSpecificSize:=0;
  end;
end;



{TAddressCaps}

constructor TAddressCaps.Create(ALineApp:HLineApp;ADeviceID,AAddressID,AAPIVersion,AExtVersion:DWord);
var R:Longint;
    Size:DWord;
    FLineAddressCaps:PLineAddressCaps;
Label Init;
begin
  inherited Create;
  {$IFDEF TAPI20}
  FDeviceClasses:=TStringList.Create;
  {$ENDIF}
  Size:=SizeOf(TLineAddresscaps)+1000;

  try
  Init:
  GetMem(PLINEADDRESSCAPS(FLineAddressCaps),Size);
  FillChar(FLineAddressCaps^,Size,#0);
  FLineAddressCaps^.dwTotalSize:=Size;
  FLineAddressCaps^.dwUsedSize:=0;
  R:=LineGetAddressCaps(ALineApp,ADeviceID,AAddressID,AAPIVersion,AExtVersion,FLineAddressCaps);
  if DWord(R)<> 0 then
  begin
    if DWord(R)=LINEERR_STRUCTURETOOSMALL then
    begin
      Size:=FLineAddressCaps^.dwNeededSize;
      FreeAndNil(FLineAddressCaps);
      goto Init;
    end
    else RaiseTAPILineError(R);
  end;
  SetCaps(FLineAddressCaps);
  finally
    FreeMem(FLineAddressCaps);
  end;
end;

constructor TAddressCaps.Create(ALineAddressCaps: PLINEADDRESSCAPS);
begin
  inherited Create;
  {$IFDEF TAPI20}
  FDeviceClasses:=TStringList.Create;
  {$ENDIF}
  SetCaps(ALineAddressCaps);
  if Assigned(AppTAPIMgr)=False then AppTAPIMgr := TTAPIMgr.Create(Application);
  AppTAPIMgr.TAPIObjects.Add(self);
end;

destructor TAddressCaps.Destroy;
begin
  {$IFDEF TAPI20}
  FDeviceClasses.Free;
  {$ENDIF}
  inherited Destroy;
end;

procedure TAddressCaps.SetCaps;
{$IFDEF TAPI20}
var S:String;
    Offset:DWord;
{$ENDIF}
begin
  FAddress:=GetDataFromTAPIStruct(Caps,Caps.dwAddressOffset,Caps.dwAddressSize);
  FAddressSharing:=IntToAddressSharing(Caps.dwAddressSharing);
  FAddressStates:=IntToAddressStates(Caps.dwAddressStates);
  FLineDeviceID:=Caps.dwLineDeviceID;
  FDevSpecific:=GetDataFromTAPIStruct(Caps,Caps.dwDevSpecificOffset,Caps.dwDevSpecificSize);
  FCallInfoStates:=IntToCallInfoStates(Caps.dwCallInfoStates);
  FCalledIDFlags:=IntToCallPartyID(Caps.dwCalledIDFlags);
  FCallerIDFlags:=IntToCallPartyID(Caps.dwCallerIDFlags);
  FConnectedIDFlags:=IntToCallPartyID(Caps.dwConnectedIDFlags);
  FRedirectingIDFlags:=IntToCallPartyID(Caps.dwRedirectingIDFlags);
  FRedirectionIDFlags:=IntToCallPartyID(Caps.dwRedirectionIDFlags);
  FCallStates:=IntToCallStates(Caps.dwCallStates);
  FDialToneModes:=IntToDialToneModes(Caps.dwDialToneModes);
  FBusyModes:=IntToBusyModes(Caps.dwBusyModes);
  FSpecialInfo:=IntToSpecialInfo(Caps.dwSpecialInfo);
  FDisconnectModes:=IntToDisconnectModes(Caps.dwDisconnectModes);
  FMaxNumActiveCalls:=Caps.dwMaxNumActiveCalls;
  FMaxNumOnHoldCalls:=Caps.dwMaxNumOnHoldCalls;
  FMaxNumOnHoldPentingCalls:=Caps.dwMaxNumOnHoldPendingCalls;
  FMaxNumConference:=Caps.dwMaxNumConference;
  FMaxNumTransConf:=Caps.dwMaxNumTransConf;
  FAddrCapFlags:=IntToAddrCapsFlags(Caps.dwAddrCapFlags);
  FCallFeatures:=IntToCallFeatures(Caps.dwCallFeatures);
  FRemoveFromConfCaps:=IntToRemoveFromConf(Caps.dwRemoveFromConfCaps);
  FRemoveFromConfState:=IntToCallStates(Caps.dwRemoveFromConfState);
  FTransferModes:=IntToTransferModes(Caps.dwTransferModes);
  FParkModes:=IntToParkModes(Caps.dwParkModes);
  FForwardModes:=IntToForwardModes(Caps.dwForwardModes);
  FMaxForwardEntries:=Caps.dwMaxForwardEntries;
  FMaxSpecificEntries:=Caps.dwMaxSpecificEntries;
  FMinFwdNumRings:=Caps.dwMinFwdNumRings;
  FMaxFwdNumRings:=Caps.dwMaxFwdNumRings;
  FMaxCallCompletions:=Caps.dwMaxCallCompletions;
  FCallCompletionConds:=IntToCallComplConds(Caps.dwCallCompletionConds);
  FCallCompletionModes:=IntToCallComplModes(Caps.dwCallCompletionModes);
  FNumCompletionMessages:=Caps.dwNumCompletionMessages;
  FCompletionMsgTextEntrySize:=Caps.dwCompletionMsgTextEntrySize;
  FCompletionMsgText:=GetDataFromTAPIStruct(Caps,Caps.dwCompletionMsgTextOffset,Caps.dwCompletionMsgTextSize) ;
  FAddressFeatures:=IntToAddrFeatures(Caps.dwAddressFeatures);
  {$IFDEF TAPI20}
  FPredictiveAutoTransferStates:=IntToCallStates(Caps.dwPredictiveAutoTransferStates);
  FNumCallTreatments:=Caps.dwNumCallTreatments;
  //FCallTreatmentList:=IntToCallTreatmentEntry();
  if (Caps.dwDeviceClassesSize > 0) and (Caps.dwDeviceClassesOffset < Caps.dwUsedSize) then
  begin
    Offset:=Caps.dwDeviceClassesOffset;
    while (Offset- Caps.dwDeviceClassesOffset)< Caps.dwDeviceClassesSize-1 do
    begin
      S:=GetDataFromTAPIStruct(Caps,Offset,Caps.dwDeviceClassesSize);
      FDeviceClasses.Add(S);
      Offset:=Offset+DWord(Length(S))+1;
    end;
  end;
 { if caps.dwDeviceClassesSize=0 then FDeviceClasses:=''
  else
  begin
    try
      SetLength(Dummy,Caps.dwDeviceClassesSize+1);
      StrCopy(PChar(Dummy),PCHAR(Caps)+Caps.dwDeviceClassesOffset);
      FDeviceClasses:=PChar(Dummy);
    except
      Dummy:=nil;
      FDeviceClasses:='';
    end;
  end;  }
  //FDeviceClasses:=GetDataFromTAPIStruct(Caps,Caps.dwDeviceClassesOffset,Caps.dwDeviceClassesSize);
  FMaxCallDataSize:=Caps.dwMaxCallDataSize;
  FCallFeatures2:=IntToCallFeatures2(Caps.dwCallFeatures);
  FMaxNoAnswerTimeout:=Caps.dwMaxNoAnswerTimeout;
  FConnectedModes:=IntToConnectedModes(Caps.dwConnectedModes);
  FOfferingModes:=IntToOfferingModes(Caps.dwOfferingModes);
  FAvailableMediaModes:=IntToMediaModes(Caps.dwAvailableMediaModes);
  {$ENDIF}
end;


{ TTAPIAddress }

constructor TTAPIAddress.Create(AOwner: TComponent);
begin
  inherited;
  FCountryCode:=0;
  FAddressMode:=amDialableAddr;
  FAddressID:=0;
end;

destructor TTAPIAddress.Destroy;
begin
  inherited;
end;

function TTAPIAddress.GetAddressCaps: TAddressCaps;
begin
  Result:=nil;
  if Assigned(FLine.Device) then
  begin
    if Assigned(FAddressCaps)=False then
      FAddressCaps:=TAddressCaps.Create(FLine.Device.Service.Handle,FLine.Device.ID,FAddressID,FLine.Device.APIVersion,FLine.Device.ExtVersion);
    Result:=FAddressCaps;
  end;
end;

function TTAPIAddress.GetAddressStatus: TAddressStatus;
begin
  if Assigned(FAddressStatus) then FreeAndNil(FAddressStatus);
  FAddressStatus:=TAddressStatus.Create(FLine.Handle,FAddressID);
  Result:=FAddressStatus;
end;

function TTAPIAddress.GetDialableAdress: String;
var R:longint;
    LTO:PLineTranslateOutput;
    Dummy:Array of char;
begin
  Result:='';
  GetMem(LTO,SizeOf(TLineTranslateOutput)+1000);
  LTO^.dwTotalSize:=SizeOf(TLineTranslateOutput)+1000;
  try
    R:=LineTranslateAddress(FLine.Device.Service.Handle,FLine.Device.ID,FLine.Device.APIVersion,PChar(FAddress),0,FTranslateOptions,LTO);
    if R<>0 then RaiseTAPILineError(R)
    else
    begin
      if LTO^.dwDialableStringSize >0 then
      begin
        SetLength(Dummy,LTO^.dwDialableStringSize+1);
        StrCopy(PChar(Dummy),PCHAR(LTO)+LTO^.dwDialableStringOffset);
        Result:=Pchar(Dummy);
      end;
    end;
  finally
    FreeMem(LTO);
  end;
end;

function TTAPIAddress.GetMode: DWord;
begin
  Result:=AddressModeToInt(FAddressMode);
end;

function TTAPIAddress.GetStateMsg: DWord;
begin
  FAddressStateMessages:=[];
  if Assigned(FOnOther) then FAddressStateMessages:=FAddressStateMessages +[asOther];
  if Assigned(FOnDevSpecific) then FAddressStateMessages:=FAddressStateMessages +[asDevSpecific];
  if Assigned(FOnInUseZero) then FAddressStateMessages:=FAddressStateMessages +[asInUseZerro];
  if Assigned(FOnInUseOne) then FAddressStateMessages:=FAddressStateMessages +[asInUseOne];
  if Assigned(FOnInUseMany) then FAddressStateMessages:=FAddressStateMessages +[asInUseMany];
  if Assigned(FOnNumCalls) then FAddressStateMessages:=FAddressStateMessages +[asNumCalls];
  if Assigned(FOnForward) then FAddressStateMessages:=FAddressStateMessages +[asForward];
  if Assigned(FOnTerminals) then FAddressStateMessages:=FAddressStateMessages +[asTerminals];
  if Assigned(FOnCapsChanged) then FAddressStateMessages:=FAddressStateMessages +[asCapSchanged];
  Result:=AddressStatesToInt(FAddressStateMessages);  
end;

function TTAPIAddress.GetTranslateOptions: TLineTranslateOptions;
begin
  Result:=IntToTranslateOptions(FTranslateOptions);
end;

procedure TTAPIAddress.SetAddressID(const Value: DWord);
begin
  if Assigned(FAddressCaps) then FreeAndNil(FAddressCaps);
  FAddressID := Value;
end;

procedure TTAPIAddress.SetTranslateOptions(
  const Value: TLineTranslateOptions);
begin
  FTranslateOptions:=TranslateOptionsToInt(Value);
end;

procedure TTAPIAddress.StateChange(AddressStatus:DWord);
{$IFDEF DEBUG}
var LAS:String;
{$ENDIF}
begin
  if Assigned(FAddressStatus) then FreeAndNil(FAddressStatus);
  {$IFDEF DEBUG}
  case AddressStatus of
    LINEADDRESSSTATE_CAPSCHANGE:LAS:='LINEADDRESSSTATE_CAPSCHANGE';
    LINEADDRESSSTATE_DEVSPECIFIC:LAS:='LINEADDRESSSTATE_DEVSPECIFIC';
    LINEADDRESSSTATE_FORWARD:LAS:='LINEADDRESSSTATE_FORWARD';
    LINEADDRESSSTATE_INUSEMANY:LAS:='LINEADDRESSSTATE_INUSEMANY';
    LINEADDRESSSTATE_INUSEONE:LAS:='LINEADDRESSSTATE_INUSEONE';
    LINEADDRESSSTATE_INUSEZERO:LAS:='LINEADDRESSSTATE_INUSEZERO';
    LINEADDRESSSTATE_NUMCALLS:LAS:='LINEADDRESSSTATE_NUMCALLS';
    LINEADDRESSSTATE_OTHER:LAS:='LINEADDRESSSTATE_OTHER';
    LINEADDRESSSTATE_TERMINALS:LAS:='LINEADDRESSSTATE_TERMINALS';
  end;
  OutputDebugString(PChar(LAS));
  {$ENDIF}
  case AddressStatus of
    LINEADDRESSSTATE_CAPSCHANGE:
    begin
      if Assigned(FAddressCaps) then FreeAndNil(FAddressCaps);
      FAddressCaps:=TAddressCaps.Create(FLine.Device.Service.Handle,FLine.Device.ID,FAddressID,FLine.Device.APIVersion,FLine.Device.ExtVersion);
      if Assigned(FOnCapsChanged) then FOnCapsChanged(self);
    end;
    LINEADDRESSSTATE_DEVSPECIFIC:if Assigned(FOnDevSpecific) then FOnDevSpecific(self);
    LINEADDRESSSTATE_FORWARD:if Assigned(FOnForward) then FOnForward(self);
    LINEADDRESSSTATE_INUSEMANY:if Assigned(FOnInUseMany) then FOnInUseMany(self);
    LINEADDRESSSTATE_INUSEONE:if Assigned( FOnInUseOne) then  FOnInUseOne(self);
    LINEADDRESSSTATE_INUSEZERO:if Assigned(FOnInUseZero) then FOnInUseZero(self);
    LINEADDRESSSTATE_NUMCALLS:if Assigned(FOnNumCalls) then FOnNumCalls(self);
    LINEADDRESSSTATE_OTHER:if Assigned(FOnOther) then FOnOther(self);
    LINEADDRESSSTATE_TERMINALS:if Assigned(FOnTerminals) then FOnTerminals(self);
  end;
end;

procedure TTAPIAddress.MakeCall;
begin
  FOutBoundCall.MakeCall(FLine.Handle,DialableAddress,FCountryCode);
end;


function TTAPIAddress.GetNumRings: DWord;
var R:Longint;
    NumR:DWord;
begin
  Result:=FNumRings;
  if Assigned(FLine) then
  begin
    if FLine.Handle <> 0 then
    begin
      R:=LineGetNumRings(FLine.Handle,FAddressID,NumR);
      if R<>0 then RaiseTAPILineError(R);
      Result:=NumR;
    end;
  end;
end;



procedure TTAPIAddress.SetNumRings(const Value: DWord);
var R:Longint;
begin
  FNumRings:=Value;
  if Assigned(FLine) then
  begin
    if FLine.Handle <> 0 then
    begin
      R:=LineSetNumRings(FLine.Handle,FAddressID,Value);
      if R<>0 then RaiseTAPILineError(R);
    end;
  end;
end;

procedure TTAPIAddress.SetStatusMessages;
var R:Longint;
    DState,AState:DWord;
begin
  If FLine.Active then
  begin
    {$IFDEF DEBUG}
    R:=LineSetStatusMessages(FLine.Handle,$01ffffff,$1FF);
    {$ELSE}
    R:=LineSetStatusMessages(FLine.Handle,FLine.Device.States,GetStateMsg);
    {$ENDIF}
    if R<-1 then
    begin
      RaiseTAPILineError(R);
    end
    else
    begin
      R:=LineGetStatusMessages(FLine.Handle,DState,AState);
      if R<-1 then
      begin
        RaiseTAPILineError(R);
      end
      else
      begin
        R:=LineSetStatusMessages(FLine.Handle,DState,AState);
        if R<-1 then
        begin
          RaiseTAPILineError(R);
        end;
      end;
    end;
  end;
end;

procedure TTAPIAddress.SetLine(const Value: TTAPILine);
begin
  FLine := Value;
end;

procedure TTAPIAddress.AppNewCall(hDevice, dwParam1, dwParam2, dwParam3:LongWord);
begin
  if dwParam3=0 then
  begin
    if cpOwner in FLine.CallPrivilege then dwParam3:=LINECALLPRIVILEGE_OWNER else
      dwParam3:=LINECALLPRIVILEGE_Monitor;
  end;
  if dwParam3=LINECALLPRIVILEGE_OWNER then
  begin
    if FLine.Handle=hDevice then
    begin
      {if FAddressID=dwParam1 then
      begin }
        if Assigned(FInBoundCall)then
        begin
          FInBoundCall.Handle:=HCall(dwParam2);
          if Assigned(FOnAppNewCall) then FOnAppNewCall(self,FInBoundCall,dwParam1,IntToCallPrivilege(dwParam3));
        end;
      {end;}
    end;
  end;
  if dwParam3=LINECALLPRIVILEGE_Monitor then
  begin
    if FLine.Handle=hDevice then
    begin
      {if FAddressID=dwParam1 then
      begin }
        if Assigned(FMonitorCall)then
        begin
          FMonitorCall.Handle:=HCALL(dwParam2);
          if Assigned(FOnAppNewCall) then FOnAppNewCall(self,FMonitorCall,dwParam1,IntToCallPrivilege(dwParam3));
        end;
      {end; }
    end;
  end;
end;



procedure TTAPIAddress.Dial;//(DestAddress: String;CountryCode: DWord);
var R:LongInt;
begin
  //R:=LineDial(FOutBoundCall.Handle,PChar(DestAddress),CountryCode);
  R:=LineDial(FOutBoundCall.Handle,PChar(DialableAddress),FCountryCode);
  if R<-1 then
  begin
    RaiseTAPILineError(R);
  end
  else
  begin
    AppTAPIMgr.AsyncList.Add(afDial,R,self);
  end;
end;

procedure TTAPIAddress.GetID(var ACall: TTAPICall;
  var AddressID: Dword;Select:TLineCallSelect; var VarStr: TVarString);
var R:Longint;
begin
  R:=LineGetID(FLine.Handle,AddressID,ACall.Handle,CallSelectToInt(Select),@varstr,PChar(FLine.Device.DeviceClass));
  if R<-1 then
  begin
    RaiseTAPILineError(R);
  end;
end;

{$IFDEF TAPI20}
{procedure TTAPIAddress.SetAgent(const Value: TTAPILineAgent);
begin
  if Value<>FAgent then
  begin
    FAgent := Value;
    FAgent.AddressID:=FID;
  end;
end; }
{$ENDIF}

procedure TTAPIAddress.SetActive(const Value: Boolean);
begin
  if Value<> FActive then
  begin
    if Assigned(FLine) then
    begin
      Line.Active:=Value;
      if Line.Active then SetStatusMessages;
    end;
    FActive := Value;
  end;
end;

procedure TTAPIAddress.PerformMsg(Msg: TCMTAPI);
{$IFNDEF TAPI20}
var TempCall: TTAPICall;
{$ENDIF}
begin
  inherited;
  with Msg.TAPIRec^ do
  begin
    if Assigned(FLine) then
    begin
      if FLine.Handle=hDevice then
      begin
        case dwMsg of
          LINE_ADDRESSSTATE:if ID=dwParam1 then StateChange(dwParam2);
          {$IFNDEF TAPI20}
          LINE_CALLSTATE:
            begin
              if Assigned(FLine)and Assigned(FInboundCall) then
              begin
                if dwParam3=LINECALLPRIVILEGE_OWNER then
                begin
                  TempCall:=TTAPICall.Create(nil);
                  TempCall.Handle:=hdevice;
                  if (TempCall.Info.AddressID=ID)and (TempCall.Info.LineHandle=FLine.Handle) then
                  begin
                    //New Call ?
                    if not(InboundCall.Handle=hDevice) then
                    begin
                      AppNewCall(TempCall.Info.LineHandle,TempCall.Info.AddressID,hdevice,dwParam3);
                      InboundCall.LineCallState(dwParam1,dwParam2,dwParam3);
                    end;
                  end;
                  TempCall.Free;
                end;
              end;
            end;
          {$ENDIF}
          {$IFDEF TAPI20}
          LINE_APPNEWCALL: {if ID=dwParam1 then }AppNewCall(hDevice,dwParam1,dwParam2,dwParam3);
          {$ENDIF}
        end;
      end;
    end;
  end;
end;

procedure TTAPIAddress.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent,Operation);
  if Operation=opRemove then
  begin
    if AComponent=FInboundCall then
      FInboundCall:=nil;
    if AComponent=FOutboundCall then
      FOutboundCall:=nil;
    if AComponent=FMonitorCall then
      FMonitorCall:=nil;
    if AComponent=FLine then
      FLine:=nil;
  end;
end;

procedure TTAPIAddress.GetAddressID(var AddrID: PChar);
var R:Longint;
begin
  R:=LineGetAddressID(FLine.Handle,FAddressID,LINEADDRESSMODE_DIALABLEADDR,AddrID,SizeOf(AddrID));
  if R<-1 then
  begin
    RaiseTAPILineError(R);
  end;
end;

{$ENDIF}
{$ENDIF}

end.
