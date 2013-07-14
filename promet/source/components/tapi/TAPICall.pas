{******************************************************************************}
{*        Copyright 1999-2001 by J.Friebel all rights reserved.               *}
{*        Autor           :  Jörg Friebel                                     *}
{*        Compiler        :  Delphi 4 / 5                                     *}
{*        System          :  Windows NT / 2000                                *}
{*        Projekt         :  TAPI Komponenten (TAPI Version 1.4 bis 3.0)      *}
{*        Last Update     :  10.05.2002                                       *}
{*        Version         :  1.2                                              *}
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
unit TAPICall;

interface
{$IFDEF WINDOWS}
{$IFDEF CPU32}

{$IFDEF FPC}
{$MODE DELPHI}
{$ENDIF}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  TAPI,TAPISystem,TAPIHelpFunc,tapierr,TAPIServices,TAPITon,DevConf;

{$INCLUDE TAPI.INC}

type
  // Structs
  TLineCallPrivilege = set of (cpNone,cpOwner,cpMonitor);
  TLineGatherTerm = (lgtBufferFull,lgtTermDigit,lgtFirstTimeOut,
    lgtInterTimeOut,lgtCancel);
  TLineBusyMode = (bymStation,bymTrunk,bymUnknown,bymUnavail);
  TLineBusyModes = Set of TLineBusyMode;
  TLineDialToneMode = (dtmNormal,dtmSpecial,dtmInternal,dtmExternal,dtmUnknown,
       dtmUnavail);
  TLineDialToneModes = Set of TLineDialToneMode;
  TLineSpecialInfo =Set of (siNoCircuit,siCustirreg,siReorder,siUnknown,
       siUnavail);
  TLineDisconnectMode = (dmNormal,dmUnknown,dmReject,dmPickup,dmForwarded,
       dmBusy,dmNoAnswer,dmBadAddress,dmUnreachable,dmCongestion,
       dmIncompatible,dmUnavail,dmNoDialtone{$IFDEF TAPI20},dmNumberChanged,
       dmOutOfOrder,dmTempFailure,dmQOSUnavail,dmBlocked,dmDoNotDisturb,
       dmCancelled{$ENDIF});
  TLineDisconnectModes = set of TLineDisconnectMode;
  TLineMediaMode = (mmUnknown,mmInteractiveVoice,mmAutomatedVoice,mmDataModem,
       mmG3FAX,mmTDD,mmG4FAX,mmDigitalData,mmTelex,mmVideotex,mmTeletex,mmMIXED,
       mmADSI,mmVoiceView{$IFDEF TAPI21},mmVideo{$ENDIF});
  TLineMediaModes = set of TLineMediaMode;
  TLineConnectedMode = (lcmActive,lcmInactive{$IFDEF TAPI20},lcmActiveHeld,
       lcmInactiveHeld,lcmConfirmed{$ENDIF});
  TLineConnectedModes = set of TLineConnectedMode;
  TLineOfferingMode = (lomActive,lomInactive);
  TLineOfferingModes =set of TLineOfferingMode;

  TLineAddressMode = (amAddressID,amDialableAddr);
  TLineAddressModes = set of TLineAddressMode;

  TLineCallState = (csAccepted,csBusy,csConferenced,csConnected,csDialing,
    csDialtone,csDisconnected,csIDLE,csOffering,csOnHold,csOnHoldPendConf,
    csOnHoldPendTransfer,csProceeding,csRingBack,csSpecialInfo,csUnknown);
  TLineCallStates = set of TLineCallState;

  TLineCallFeature = (lcfAccept,lcfAddToConf,lcfAnswer,lcfBlindTransfer,
    lcfCompleteCall,lcfCompleteTransf,lcfDial,lcfDrop,lcfGatherDigits,
    lcfGenerateDigits,lcfGenerateTone,lcfHold,lcfMonitorDigits,lcfMonitorMedia,
    lcfMonitorTones,lcfPark,lcfPrepareAddConf,lcfReDirect,lcfRemoveFromConf,
    lcfSecureCall,lcfSendUserUser,lcfSetCallParams,lcfSetMediaControl,
    lcfSetTerminal,lcfSetupConf,lcfSetupTransfer,lcfSwapHold,lcfUnHold,
    lcfReleaseUserUserInfo{$IFDEF TAPI20},lcfSetTreatment,lcfSetQOS,
    lcfSetCallData{$ENDIF});
  TLineCallFeatures = set of TLineCallFeature;

{$IFDEF TAPI20}
  TLineCallFeature2 = (lcf2NoHoldConference,lcf2OneStepTransfer,lcf2ComlpCampOn,
    lcf2ComplCallback,lcf2ComplIntrude,lcf2ComplMessage,lcf2TransferNorm,
    lcf2TransferConf,lcf2ParkDirect,lcf2ParkNonDirect);
  TLineCallFeatures2 = set of TLineCallFeature2;
{$ENDIF}
  TLineCallComplCond = (lcccBusy,lcccNoAnswer,lcccNoDef);
  TLineCallComplConds = set of TLineCallComplCond;

  TLineCallComplMode = (lccmCallBack,lccmCampon,lccmIntrude,lccmMessage);
  TLineCallComplModes = set of TLineCallComplMode;

  TLineBearerMode = (bmVoice,bmSpeech,bmMultiuse,bmData,bmAltSpeechData,
    bmNonCallSignaling,bmPassthrough {$IFDEF TAPI20},bmRestrictedData{$ENDIF});
  TLineBearerModes = set of TLineBearerMode;

  TLineCallParamFlags = set of (cpfBlockId,cpfDestOffHook,cpfIdle,cpfOrigOffHook,
    cpfSecure{$IFDEF TAPI20},cpfNoHoldConference,cpfPredictiveDial,
    cpfOneStepTransfer{$ENDIF} );

  TLineTermMode = (tmmButtons,tmmLamps,tmmDisplay,tmmRinger,tmmHookSwitch,
       tmmMediaToLine,tmmMediaFromLine,tmmMediaBidirect,tmmNoDef);
  TLineTermModes = set of TLineTermMode;      

  TLineCallOrigin = (coOutBound,coInternal,coExternal,coUnknown,coUnavail,
    coConference,coInbound);

  TLineCallReason = (crDirect,crFWDBusy,crFWDNoAnswer,crFWDUnCond,crPickup,
    crUnPark,crRedirect,crCallCompletion,crTransfer,crReminder,crUnknown,
    crUnavail,crIntrude{$IFDEF TAPI20},crParked,crCampedon,crRouterRequest{$ENDIF});

  {$IFDEF TAPI20}
  TLineProxyRequest = (lprSetAgentGroup,lprSetAgentState,lprSetAgentActivity,
    lprGetAgentCaps,lprGetAgentStatus,lprAgentSpecific,lprGetAgentActivityList,
    lprGetAgentGroupList{$IFDEF TAPI22},lprCreateAgent,
    lprSetAgentMeasurementPeriod,lprGetAgentInfo,lprCreateAgentSession,
    lprGetAgentSessionList,lprSetAgentSessionState,lprGetAgentSessionInfo,
    lprGetQueueList,lprSetQueueMeasurementPeriod,lprGetQueueInfo,lprGetGroupList,
    lprSetAgentStateEx{$ENDIF});
  TLineProxyRequests = Set of TLineProxyRequest;
  {$ENDIF}

  TDialParams=class(TPersistent)
  private
    FDialPause,
    FDialSpeed,
    FDigitDuration,
    FWaitForDialtone:DWord;
  public
    constructor Create; virtual;
  published
    property DialPause:DWORD read FDialPause write FDialPause default 0;
    property DialSpeed:DWORD read FDialSpeed write FDialSpeed default 0;
    property DigitDuration:DWORD read FDigitDuration write FDigitDuration default 0;
    property WaitForDialtone:DWORD read FWaitForDialtone write FWaitForDialtone default 0;
  end;
  
  // Eventproc
  TCallEventOffering=procedure(Sender:TObject;OfferingMode:TLineOfferingModes;Rights:TLineCallPrivilege)of Object;
  TCallEvent=procedure(Sender:TObject;Rights:TLineCallPrivilege)of Object;
  TCallEventConnected=procedure(Sender:TObject;ConnectedMode:TLineConnectedModes;Rights:TLineCallPrivilege)of Object;
  TCallEventReply=procedure(Sender:TObject;AsyncFunc:TAsyncFunc;Error:Dword)of Object;
  TCallEventBusy=procedure(Sender:TObject;BusyMode:TLineBusyMode;Rights:TLineCallPrivilege)of Object;
  TCallEventSpecialInfo=procedure(Sender:TObject;SpecialInfo:TLineSpecialInfo;Rights:TLineCallPrivilege)of Object;
  TCallGatherDigitsEvent=procedure(Sender: TObject;GatherTermination:TLineGatherTerm;TickCount:Dword)of Object;
  TCallEventDialTone=procedure(Sender:TObject;DialTonMode:TLineDialToneMode;Rights:TLineCallPrivilege)of Object;
  TCallEventDisconnected=procedure(Sender:TObject;DisconnectedMode:TLineDisconnectMode;Rights:TLineCallPrivilege)of Object;
  TCallEventMonitorMedia=procedure(Sender:TObject;MediaMode:TLineMediaModes;TickCount:DWord)of Object;
  TCallEventDevSpecific=procedure(Sender:TObject;dwParam1,dwParam2,dwParam3:DWord)of Object;
  TCallEventDevSpecificFeature=procedure(Sender:TObject;dwParam1,dwParam2,dwParam3:DWord)of Object;

  TLineCallPartyID = Set of (cpidBlocked,cpidOutOfArea,cpidName,cpidAddress,cpidPartial,
       cpidUnknown,cpidUnavail);
  {$IFDEF TAPI30}
  TLineAddressType = (latPhoneNumber,latSDP,latEmailName,latDomainName,latIPAddress);
  TLineAddressTypes = set of TLineAddressType;
  {$ENDIF}


  TCallParams=class(TComponent)
  private
    FAddressID: DWord;
    FAddressMode: TLineAddressMode;
    FBearerMode: TLineBearerMode;
    FCallParamFlags: TLineCallParamFlags;
    FDialParams: TDialParams;
    FMaxRate: DWord;
    FMediaMode: TLineMediaModes;// Nur ein Wert !
    FMinRate: DWord;
    FDisplayableAddress: String;
    FLowLevelComp: String;
    FHighLevelComp: String;
    FDevSpecific: String;
    FOrigAddress: String;
    FCalledParty: String;
    FUserUserInfo: String;
    FComment: String;
    FUseProxy:Boolean;
    {$IFDEF TAPI20}
    FPredictiveAutoTransferStates: TLineCallStates;
    FTargetAddress: String;
    FProxyRequests:TLineProxyRequests;
    //FSendingFlowspec:TFlowSpec
    //FReceivingFlowspec:TFlowSpec - WinSock2 - ab W98
    FDeviceClass:String;
    FDeviceConfig:TTAPILineDeviceConfig;
    FCallData:String;
    FCallingPartyID: string;
    FNoAnswerTimeout: DWord;
    {$ENDIF}
    {$IFDEF TAPI30}
    FAddressType: TLineAddressType;
    {$ENDIF}
  public
    constructor Create(Owner:TComponent); override;
    destructor Destroy;override;
    procedure GetParamStruct(var Struct:PLineCallParams);
    property UseProxy:Boolean read FUseProxy write FUseProxy;
  published
    property BearerMode:TLineBearerMode read FBearerMode write FBearerMode default bmVoice;                    {// voice            }
    property MinRate:DWord read FMinRate write FMinRate default 0;
    property MaxRate:DWord read FMaxRate write FMaxRate default 0;
    property MediaMode:TLineMediaModes read FMediaMode write FMediaMode default [mmInteractiveVoice] ;
    property CallParamFlags:TLineCallParamFlags read FCallParamFlags write FCallParamFlags;
    property AddressMode:TLineAddressMode read FAddressMode write FAddressMode;
    property AddressID:DWord read FAddressID  write FAddressID default 0;
    property DialParams: TDialParams read FDialParams write FDialParams;
    property OrigAddress:String read FOrigAddress write FOrigAddress;
    property DisplayableAddress:String read FDisplayableAddress write FDisplayableAddress;
    property CalledParty:String read FCalledParty write FCalledParty;
    property Comment:String read FComment write FComment;
    property UserUserInfo:String read FUserUserInfo write FUserUserInfo;
    property HighLevelComp:String read FHighLevelComp write FHighLevelComp;
    property LowLevelComp:String read FLowLevelComp write FLowLevelComp;
    property DevSpecific:String  read FDevSpecific write FDevSpecific;
    {$IFDEF TAPI20}
    property PredictiveAutoTransferStates:TLineCallStates read FPredictiveAutoTransferStates write FPredictiveAutoTransferStates;
    property TargetAddress:String  read FTargetAddress write FTargetAddress;
    property ProxyRequests:TLineProxyRequests read FProxyRequests write FProxyRequests;
    //property SendingFlowspec:tFLOWSPEC;   winsock2.h
    //property ReceivingFlowspec
    property DeviceClass:String read FDeviceClass write FDeviceClass;                                   // TAPI v2.0
    property DeviceConfig:TTAPILineDeviceConfig read FDeviceConfig write FDeviceConfig;
    property CallData:String read FCallData write FCallData;
    property NoAnswerTimeout:DWord read FNoAnswerTimeout write FNoAnswerTimeout;
    property CallingPartyID:string read FCallingPartyID write FCallingPartyID;
    {$ENDIF}
    {$IFDEF TAPI30}
    property AddressType: TLineAddressType read FAddressType write FAddressType;                                // TAPI v3.0}
    {$ENDIF}
  end;


  TCallStatus=class(TPersistent)
  private
    FCallState:TLineCallStates;
    FCallStateMode:DWord;
    FCallPrivilege:TLineCallPrivilege;
    FCallFeatures:TLineCallFeatures;
    FDevSpecific: String;
    {$IFDEF TAPI20}
    FCallFeatures2: TLineCallFeatures2;
    {$IFDEF WIN32}
    FtStateEntryTime: TSystemTime;
    {$ELSE}
    FtStateEntryTime: array[0..7] of WORD;
    FHandle: HCall;
    {$ENDIF}
    {$ENDIF}
    function GetCallStateBusyMode: TLineBusyModes;
    function GetCallStateDialToneMode: TLineDialToneModes;
    function GetCallStateDisconnectMode: TLineDisConnectModes;
    function GetCallStateSpecialInfo: TLineSpecialInfo;
  protected
    procedure SetStatus(CStatus:PLineCallStatus);
  public
    constructor Create(ACall:HCall);
    destructor Destroy;override;
  published
    property State:TLineCallStates read FCallState;
    property CallStateBusyMode:TLineBusyModes read GetCallStateBusyMode;
    property CallStateDialToneMode:TLineDialToneModes read GetCallStateDialToneMode;
    property CallStateDisconnectMode:TLineDisConnectModes read GetCallStateDisconnectMode;
    property CallStateSpecialInfo:TLineSpecialInfo read GetCallStateSpecialInfo;
    property CallPrivilege:TLineCallPrivilege read FCallPrivilege;
    property Features:TLineCallFeatures read FCallFeatures;
    property DevSpecific: String read FDevSpecific;
    {$IFDEF TAPI20}
    property CallFeatures2:TLineCallFeatures2 read FCallFeatures2;
    {$IFDEF WIN32}
    {$IFNDEF FPC}
    property StateEntryTime:TSystemTime read FtStateEntryTime;
    {$ENDIF}
    {$ELSE}
    property StateEntryTime: array[0..7] of WORD read FtStateEntryTime;
    {$ENDIF}
    {$ENDIF}
  end;


type
   TCallInfo = class;

   TTAPICallObject = class(TTAPIComponent)
   private
     FHandle:HCall;
     FAccepted:Boolean;
     FAnswered:Boolean;
     FDroped:Boolean;
     FConnected:Boolean;
     FForwarded:Boolean;
     FCallStatus:TCallStatus;
     FCallInfo:TCallInfo;
     FCallParams:TCallParams;
     FOnStateIdle:TCallEvent;
     FOnStateOffering:TCallEventOffering;
     FOnStateAccepted:TCallEvent;
     FOnStateDialTone:TCallEventDialTone;
     FOnStateDialing:TCallEvent;
     FOnStateRingBack:TCallEvent;
     FOnStateBusy:TCallEventBusy;
     FOnStateSpecialInfo:TCallEventSpecialInfo;
     FOnStateConnected:TCallEventConnected;
     FOnStateProceeding:TCallEvent;
     FOnStateOnHold:TCallEvent;
     FOnStateConferenced:TCallEvent;
     FOnStateOnHoldPendConf:TCallEvent;
     FOnStateOnHoldPendTransF:TCallEvent;
     FOnStateDisconnected:TCallEventDisconnected;
     FOnStateUnknown:TCallEvent;
     FOnInfoStateOther:TNotifyEvent;
     FOnInfoStateDevSpecific:TNotifyEvent;
     FOnInfoStateBearerMode:TNotifyEvent;
     FOnInfoStateRate:TNotifyEvent;
     FOnInfoStateMediaMode:TNotifyEvent;
     FOnInfoStateAppSpecific:TNotifyEvent;
     FOnInfoStateCallID:TNotifyEvent;
     FOnInfoStateRelatedCallID:TNotifyEvent;
     FOnInfoStateOrigin:TNotifyEvent;
     FOnInfoStateReason:TNotifyEvent;
     FOnInfoStateCompletionID:TNotifyEvent;
     FOnInfoStateNumOwnerIncr:TNotifyEvent;
     FOnInfoStateNumOwnerDecr:TNotifyEvent;
     FOnInfoStateNumMonitors:TNotifyEvent;
     FOnInfoStateTrunk:TNotifyEvent;
     FOnInfoStateCallerID:TNotifyEvent;
     FOnInfoStateCalledID:TNotifyEvent;
     FOnInfoStateConnectedID:TNotifyEvent;
     FOnInfoStateRedirectionID:TNotifyEvent;
     FOnInfoStateRedirectingID:TNotifyEvent;
     FOnInfoStateDisplay:TNotifyEvent;
     FOnInfoStateUserUserInfo:TNotifyEvent;
     FOnInfoStateHighLevelComp:TNotifyEvent;
     FOnInfoStateLowLevelComp:TNotifyEvent;
     FOnInfoStateChargingInfo:TNotifyEvent;
     FOnInfoStateTerminal:TNotifyEvent;
     FOnInfoStateDialParams:TNotifyEvent;
     FOnInfoStateMonitorModes:TNotifyEvent;
     FOnReply: TCallEventReply;
     procedure SetHandle(const Value: THandle);virtual;
     function GetIsAnswered:Boolean;
   protected

   public
     procedure Reply(AsyncFunc:TAsyncFunc;Error:DWord);
     procedure DeallocateCall;
     function SetCallParams(BearerMode: TLineBearerMode;MinRate,MaxRate:DWord):LongWord;
     // Line_CallState Msg
     property OnStateIdle:TCallEvent read FOnStateIdle write FOnStateIdle;
     property OnStateOffering:TCallEventOffering read FOnStateOffering write FOnStateOffering;
     property OnStateAccepted:TCallEvent read FOnStateAccepted write FOnStateAccepted;
     property OnStateDialTone:TCallEventDialTone read FOnStateDialTone write FOnStateDialTone;
     property OnStateDialing:TCallEvent read FOnStateDialing write FOnStateDIALING;
     property OnStateRingBack:TCallEvent read FOnStateRingBack write FOnStateRingBack;
     property OnStateBusy:TCallEventBusy read FOnStateBusy write FOnStateBusy;
     property OnStateSpecialInfo:TCallEventSpecialInfo read FOnStateSpecialInfo write FOnStateSpecialInfo;
     property OnStateConnected:TCallEventConnected read FOnStateConnected write FOnStateConnected;
     property OnStateProceeding:TCallEvent read FOnStateProceeding write FOnStateProceeding;
     property OnStateOnHold:TCallEvent read FOnStateOnHold write FOnStateOnHold;
     property OnStateConferenced:TCallEvent read FOnStateConferenced write FOnStateConferenced;
     property OnStateOnHoldPendConf:TCallEvent read FOnStateOnHoldPendConf write FOnStateOnHoldPendConf;
     property OnStateOnHoldPendTransf:TCallEvent read FOnStateOnHoldPendTransf write FOnStateOnHoldPendTransf;
     property OnStateDisconnected:TCallEventDisconnected read FOnStateDisconnected write FOnStateDisconnected;
     property OnStateUnknown:TCallEvent read FOnStateUnknown write FOnStateUnknown;
     // Line_Reply for Calls
     property OnReply:TCallEventReply read FOnReply write FOnReply;
     //Line_CallInfo Msg
     property OnInfoOther:TNotifyEvent read FOnInfoStateOther write FOnInfoStateOther;
     property OnInfoDevSpecific:TNotifyEvent read FOnInfoStateDevSpecific write FOnInfoStateDevSpecific;
     property OnInfoBearerMode:TNotifyEvent read FOnInfoStateBearerMode write FOnInfoStateBearerMode;
     property OnInfoRate:TNotifyEvent read FOnInfoStateRate write FOnInfoStateRate;
     property OnInfoMediaMode:TNotifyEvent read FOnInfoStateMediaMode write FOnInfoStateMediaMode;
     property OnInfoAppSpecific:TNotifyEvent read FOnInfoStateAppSpecific write FOnInfoStateAppSpecific;
     property OnInfoCallId:TNotifyEvent read FOnInfoStateCallId write FOnInfoStateCallId;
     property OnInfoRelatedCallId:TNotifyEvent read FOnInfoStateRelatedCallId write FOnInfoStateRelatedCallId;
     property OnInfoOrigin:TNotifyEvent read FOnInfoStateOrigin write FOnInfoStateOrigin;
     property OnInfoReason:TNotifyEvent read FOnInfoStateReason write FOnInfoStateReason;
     property OnInfoCompletionId:TNotifyEvent read FOnInfoStateCompletionId write FOnInfoStateCompletionId;
     property OnInfoNumOwnerIncr:TNotifyEvent read FOnInfoStateNumOwnerIncr write FOnInfoStateNumOwnerIncr;
     property OnInfoNumOwnerDecr:TNotifyEvent read FOnInfoStateNumOwnerDecr write FOnInfoStateNumOwnerDecr;
     property OnInfoNumMonitors:TNotifyEvent read FOnInfoStateNumMonitors write FOnInfoStateNumMonitors;
     property OnInfoTrunk:TNotifyEvent read FOnInfoStateTrunk  write FOnInfoStateTrunk;
     property OnInfoCallerId:TNotifyEvent read FOnInfoStateCallerId write FOnInfoStateCallerId;
     property OnInfoCalledId:TNotifyEvent read FOnInfoStateCalledId write FOnInfoStateCalledId;
     property OnInfoConnectedId:TNotifyEvent read FOnInfoStateConnectedId write FOnInfoStateConnectedId;
     property OnInfoRedirectionId:TNotifyEvent read FOnInfoStateRedirectionId write FOnInfoStateRedirectionId;
     property OnInfoRedirectingId:TNotifyEvent read FOnInfoStateRedirectingId write FOnInfoStateRedirectingId;
     property OnInfoDisplay:TNotifyEvent read FOnInfoStateDisplay write FOnInfoStateDisplay;
     property OnInfoUserUserInfo:TNotifyEvent read FOnInfoStateUserUserInfo write FOnInfoStateUserUserInfo;
     property OnInfoHighLevelComp:TNotifyEvent read FOnInfoStateHighLevelComp write FOnInfoStateHighLevelComp;
     property OnInfoLowLevelComp:TNotifyEvent read FOnInfoStateLowLevelComp write FOnInfoStateLowLevelComp;
     property OnInfoChargingInfo:TNotifyEvent read FOnInfoStateChargingInfo write FOnInfoStateChargingInfo;
     property OnInfoTerminal:TNotifyEvent read FOnInfoStateTerminal write FOnInfoStateTerminal;
     property OnInfoDialParams:TNotifyEvent read FOnInfoStateDialParams write FOnInfoStateDialParams;
     property OnInfoMonitorModes:TNotifyEvent read FOnInfoStateMonitorModes write FOnInfoStateMonitorModes;
     property Handle:HCall read FHandle write SetHandle;
     property MakeCallParams:TCallParams read FCallParams write FCallParams;
     property IsAnswered:Boolean read GetIsAnswered;
   end;

   TTAPICustomCall=class(TTAPICallObject)
   published
     // Line_CallState Msg
     property OnStateIdle;
     property OnStateOffering;
     property OnStateAccepted;
     property OnStateDialTone;
     property OnStateDialing;
     property OnStateRingBack;
     property OnStateBusy;
     property OnStateSpecialInfo;
     property OnStateConnected;
     property OnStateProceeding;
     property OnStateOnHold;
     property OnStateConferenced;
     property OnStateOnHoldPendConf;
     property OnStateOnHoldPendTransf;
     property OnStateDisconnected;
     property OnStateUnknown;
     // Line_Reply for Calls
     property OnReply;
     //Line_CallInfo Msg
     property OnInfoOther;
     property OnInfoDevSpecific;
     property OnInfoBearerMode;
     property OnInfoRate;
     property OnInfoMediaMode;
     property OnInfoAppSpecific;
     //property OnInfoCallId;
     property OnInfoRelatedCallId;
     property OnInfoOrigin;
     property OnInfoReason;
     property OnInfoCompletionId;
     property OnInfoNumOwnerIncr;
     property OnInfoNumOwnerDecr;
     property OnInfoNumMonitors;
     property OnInfoTrunk;
     property OnInfoCallerId;
     property OnInfoCalledId;
     property OnInfoConnectedId;
     property OnInfoRedirectionId;
     property OnInfoRedirectingId;
     property OnInfoDisplay;
     property OnInfoUserUserInfo;
     property OnInfoHighLevelComp;
     property OnInfoLowLevelComp;
     property OnInfoChargingInfo;
     property OnInfoTerminal;
     property OnInfoDialParams;
     property OnInfoMonitorModes;
     property MakeCallParams;
   end;

   TTAPICall=class(TTAPICustomCall)
   private
     FDigits:TTAPIDigits;
     FTones:TTAPITones;
     FUserUserInfo:AnsiString;
     FMessageID:DWord;
     FCompletionMode:DWord;
     FCompletionID:DWord;
     FOnDevSpecific:TCallEventDevSpecific;
     FOnDevSpecificFeature:TCallEventDevSpecificFeature;
     FOnMonitorMedia:TCallEventMonitorMedia;

     FOnLineGatherDigits:TCallGatherDigitsEvent;
     function GetCallInfo: TCallInfo;
     function GetCompletionMode: TLineCallComplMode;
     procedure SetCompletionMode(const Value: TLineCallComplMode);
     function GetProcess: TAsyncFunctions;
     function GetStatus: TCallStatus;
     function GetDigits: TTAPIDigits;
     procedure SetDigits(const Value: TTAPIDigits);
     procedure SetHandle(const Value: THandle);override;
     //function GetCallHandle: THandle;
     function GetTones: TTAPITones;
     procedure SetTones(const Value: TTAPITones);
   protected
     procedure Notification(AComponent: TComponent;
       Operation: TOperation); override;
   public
     procedure MonitorMediaChange(MediaMode:TLineMediaModes;TickCount:DWord);
     procedure MonitorMedia(MediaMode:TLineMediaModes);
     procedure SetMediaMode(MediaMode: TLineMediaModes);
     procedure lineCallState(var State,Mode,Rights:DWord);
     procedure DevSpecificChange(dwParam1,dwParam2,dwParam3:DWord);
     procedure DevSpecificFeatureChange(dwParam1,dwParam2,dwParam3:DWord);
     procedure LineCallInfoChange(var CallInfo: Dword);
     //procedure LineGatherDigits(var GatherTermination,TickCount: DWord);
     procedure PerformMsg(Msg: TCMTAPI);override;
     function GetWaveID(WaveDeviceClass: String): DWord;
     function GetCommPort:THandle;
     function Drop: LongWord;
     function Accept: LongWord;
     function Answer: LongWord;
     function Hold: LongWord;
     function UnHold: LongWord;
     function BlindTransfer(TransferDestAddress:String;CountryCode:DWord):LongWord;
     function CompleteCall:LongWord;
     //procedure DeallocateCall;
     procedure ChangePrivilege(NewPriv: TLineCallPrivilege);
     procedure MakeCall(LineHandle:HLine;DialableAddress:String;FCountryCode:DWord);
     procedure Handoff(MediaMode:TLineMediaModes);
     constructor Create(Owner:TComponent);override;
     destructor Destroy; override;

     property Info:TCallInfo read GetCallInfo;
     property Status:TCallStatus read GetStatus;
     property Process:TAsyncFunctions read GetProcess ;
     property UserUserInfo:String read FUserUserInfo write FUserUserInfo;
   published
     property OnLineGatherDigits:TCallGatherDigitsEvent read FOnLineGatherDigits write FOnLineGatherDigits;
     property OnMonitorMedia:TCallEventMonitorMedia read FOnMonitorMedia write FOnMonitorMedia;
     property OnDevSpecific:TCallEventDevSpecific read FOnDevSpecific write FOnDevSpecific;
     property OnDevSpecificFeature:TCallEventDevSpecificFeature read FOnDevSpecificFeature write FOnDevSpecificFeature;
     property Digits:TTAPIDigits read GetDigits write SetDigits;
     property Tones:TTAPITones read GetTones write SetTones;
     property CompletionMode:TLineCallComplMode read GetCompletionMode write SetCompletionMode default lccmCallBack;

   end;

   TCallInfo=class(TPersistent)
   private
     FhLine: HLINE;
     FLineDeviceID:DWord;
     FAddressID:DWord;
     FBearerMode:TLineBearerMode;
     FRate:Dword;
     FMediaMode:TLineMediaModes;
     FAppSpecific,
     FCallID,
     FRelatedCallID:DWord;
     FCallParamFlags:TLineCallParamFlags;
     FCallStates:TLineCallStates;
     FMonitorDigitModes:TLineDigitModes;
     FMonitorMediaModes:TLineMediaModes;
     FDialParams: TLINEDIALPARAMS;
     FOrigin:TLineCallOrigin;
     FReason:TLineCallReason;
     FCompletionID,
     FNumOwners,
     FNumMonitors,
     FCountryCode,
     FTrunk:DWord;
     FCallerIDFlags:TLineCallPartyID;
     FCallerID,
     FCallerIDName:String;
     FCalledIDFlags:TLineCallPartyID;
     FCalledID:String;
     FCalledIDName:String;
     FConnectedIDFlags:TLineCallPartyID;
     FConnectedID:String;
     FConnectedIDName:String;
     FRedirectionIDFlags:TLineCallPartyID;
     FRedirectionID:String;
     FRedirectionIDName:String;
     FRedirectingIDFlags:TLineCallPartyID;
     FRedirectingID:String;
     FRedirectingIDName:String;
     FAppName:String;
     FDisplayableAddress:String;
     FCalledParty,
     FComment,
     FDisplay,
     FUserUserInfo:String;
     FHighLevelComp,
     FLowLevelComp,
     FChargingInfo:String;
     FTerminalModes:Array of TLineTermModes;
     FDevSpecific:String;
     {$IFDEF TAPI20}
     FCallTreatment:DWord;
     FCallData:String;
     //FSendingFlowspec,
     //FReceivingFlowspec,
     {$ENDIF}
     function GetTerminalModes(Index:Integer):TLineTermModes;
   public
     constructor Create(AHCall:hCall);virtual;
     destructor Destroy;override;
     property LineHandle: HLINE read FhLine;
     property LineDeviceID:DWord read FLineDeviceID;
     property AddressID:DWord read FAddressID;
     property BearerMode:TLineBearerMode read FBearerMode;
     property Rate:DWord read FRate;
     property MediaMode:TLineMediaModes read FMediaMode;
     property AppSpecific:DWord read FAppSpecific ;
     property CallID:DWord read FCallID;
     property RelatedCallID:DWord read FRelatedCallID;
     property CallParamFlags:TLineCallParamFlags read FCallParamFlags;
     property CallStates:TLineCallStates read FCallStates;
     property MonitorDigitModes:TLineDigitModes read FMonitorDigitModes;
     property MonitorMediaModes: TLineMediaModes read FMonitorMediaModes;
     property DialParams: TLINEDIALPARAMS read FDialParams;
     property Origin:TLineCallOrigin read FOrigin;
     property Reason:TLineCallReason read FReason;
     property CompletionID:DWord read FCompletionID;
     property NumOwners:DWord read FNumOwners;
     property NumMonitors:DWord read FNumMonitors;
     property CountryCode:DWord read FCountryCode;
     property Trunk:DWord read FTrunk;
     property CallerIDFlags:TLineCallPartyID read FCallerIDFlags;
     property CallerID:String read FCallerID;
     property CallerIDName:String read FCallerIDName;
     property CalledIDFlags:TLineCallPartyID read FCalledIDFlags;
     property CalledID:String read FCalledID;
     property CalledIDName:String read FCalledIDName;
     property ConnectedIDFlags:TLineCallPartyID read FConnectedIDFlags;
     property ConnectedID:String read FConnectedID;
     property ConnectedIDName:String read FConnectedIDName;
     property RedirectionIDFlags:TLineCallPartyID read FRedirectionIDFlags;
     property RedirectionID:String read FRedirectionID;
     property RedirectionIDName:String read FRedirectionIDName;
     property RedirectingIDFlags:TLineCallPartyID read FRedirectingIDFlags;
     property RedirectingID:String read FRedirectingID;
     property RedirectingIDName:String read FRedirectingIDName;
     property AppName:String read FAppName;
     property DisplayableAddress:String read FDisplayableAddress;
     property CalledParty:String read FCalledParty;
     property Comment:String read FComment;
     property Display:String read FDisplay;
     property UserUserInfo:String read FUserUserInfo;
     property HighLevelComp:String read FHighLevelComp;
     property LowLevelComp:String read FLowLevelComp;
     property ChargingInfo:String read FChargingInfo;
     property TerminalModes[Index:Integer]:TLineTermModes read GetTerminalModes;
     property DevSpecific:String read FDevSpecific;
     {$IFDEF TAPI20}
     property CallTreatment:DWord read FCallTreatment;
     property CallData:String read FCallData;
     //property SendingFlowspec:FLOWSPEC read F,
     //property ReceivingFlowspec:FLOWSPEC read F,
     {$ENDIF}
   end;

function IntToTermMode(Value:LongWord):TLineTermMode;   
function IntToCallReason(Value:LongWord):TLineCallReason;
function IntToCallOrigin(Value:LongWord):TLineCallOrigin;
function IntToCallParamFlags(Value:LongWord):TLineCallParamFlags;
function CallParamFlagsToInt(Value:TLineCallParamFlags):LongWord;
function IntToBearerModes(Value:LongWord):TLineBearerModes;
function IntToBearerMode(Value:LongWord):TLineBearerMode;
function BearerModeToInt(AMode:TLineBearerMode):DWord;
function AddressModeToInt(Value:TLineAddressMode):LongWord;
function IntToAddressModes(Value:LongWord):TLineAddressModes;   
{$IFDEF TAPI20}
function IntToCallFeatures2(Value:LongWord):TLineCallFeatures2;
{$ENDIF}
function IntToCallComplCond(Value:LongWord):TLineCallComplCond;
function IntToCallComplConds(Value:LongWord):TLineCallComplConds;
function IntToCallComplModes(Value:LongWord):TLineCallComplModes;
function IntToCallPartyID(Value:LongWord):TLineCallPartyID;
function IntToCallPrivilege(Value:LongWord):TLineCallPrivilege;
function CallPrivilegeToInt(Value:TLineCallPrivilege):LongWord;
function IntToGatherTerm(Value:LongWord):TLineGatherTerm;
function IntToBusyModes(Value:Longword):TLineBusyModes;
function IntToBusyMode(Value:Longword):TLineBusyMode;
function IntToDialToneModes(Value:LongWord):TLineDialToneModes;
function IntToDialToneMode(Value:LongWord):TLineDialToneMode;
function IntToSpecialInfo(Value:LongWord):TLineSpecialInfo;
function IntToDisconnectModes(Value:Longword):TLineDisconnectModes;
function IntToDisconnectMode(Value:Longword):TLineDisconnectMode;
function IntToMediaModes(Value:LongWord):TLineMediaModes;
function MediaModesToInt(MM:TLineMediaModes):LongWord;
function IntToConnectedModes(Value:LongWord):TLineConnectedModes;
function IntToOfferingModes(Value:LongWord):TLineOfferingModes;
function IntToCallState(Value:LongWord):TLineCallState;
function IntToCallStates(Value:LongWord):TLineCallStates;
function CallStatesToInt(Value:TLineCallStates):LongWord;
function IntToCallFeatures(Value:Longword):TLineCallFeatures;


procedure Register;

{$ENDIF}
{$ENDIF}

implementation

{$IFDEF WINDOWS}
{$IFDEF CPU32}

uses {$IFDEF VER120}D4Comp,{$ENDIF}TAPILines;


procedure Register;
begin
{$IFDEF TAPI30}
  RegisterComponents('TAPI30', [TTAPICall]);
  RegisterComponents('TAPI30', [TCallParams]);
{$ELSE}
{$IFDEF TAPI22}
  RegisterComponents('TAPI22', [TTAPICall]);
  RegisterComponents('TAPI22', [TCallParams]);
{$ELSE}
{$IFDEF TAPI21}
  RegisterComponents('TAPI21', [TTAPICall]);
  RegisterComponents('TAPI21', [TCallParams]);
{$ELSE}
{$IFDEF TAPI20}
  RegisterComponents('TAPI20', [TTAPICall]);
  RegisterComponents('TAPI20', [TCallParams]);
{$ELSE}
  RegisterComponents('TAPI', [TTAPICall]);
  RegisterComponents('TAPI', [TCallParams]);
{$ENDIF}
{$ENDIF}
{$ENDIF}
{$ENDIF}
end;

function IntToTermMode(Value:LongWord):TLineTermMode;
begin
  Result:=tmmNoDef;
  case Value of
    LINETERMMODE_BUTTONS:Result:=tmmButtons;
    LINETERMMODE_LAMPS:Result:=tmmLamps;
    LINETERMMODE_DISPLAY:Result:=tmmDisplay;
    LINETERMMODE_RINGER:Result:=tmmRinger;
    LINETERMMODE_HOOKSWITCH:Result:=tmmHookSwitch;
    LINETERMMODE_MEDIATOLINE:Result:=tmmMediaToLine;
    LINETERMMODE_MEDIAFROMLINE:Result:=tmmMediaFromLine;
    LINETERMMODE_MEDIABIDIRECT:Result:=tmmMediaBidirect;
  end;
end;

function IntToTermModes(Value:LongWord):TLineTermModes;
begin
  Result:=[];
  if Value and LINETERMMODE_BUTTONS = LINETERMMODE_BUTTONS then Result:=Result+[tmmButtons];
  if Value and LINETERMMODE_LAMPS = LINETERMMODE_LAMPS then Result:=Result+[tmmLamps];
  if Value and LINETERMMODE_DISPLAY = LINETERMMODE_DISPLAY then Result:=Result+[tmmDisplay];
  if Value and LINETERMMODE_RINGER = LINETERMMODE_RINGER then Result:=Result+[tmmRinger];
  if Value and LINETERMMODE_HOOKSWITCH = LINETERMMODE_HOOKSWITCH then Result:=Result+[tmmHookSwitch];
  if Value and LINETERMMODE_MEDIATOLINE = LINETERMMODE_MEDIATOLINE then Result:=Result+[tmmMediaToLine];
  if Value and LINETERMMODE_MEDIAFROMLINE = LINETERMMODE_MEDIAFROMLINE then Result:=Result+[tmmMediaFromLine];
  if Value and LINETERMMODE_MEDIABIDIRECT = LINETERMMODE_MEDIABIDIRECT then Result:=Result+[tmmMediaBidirect];
end;

function IntToCallReason(Value:LongWord):TLineCallReason;
begin
  result:=crUnknown;
  case Value of
    LINECALLREASON_DIRECT:Result:=crDirect;
    LINECALLREASON_FWDBUSY:Result:=crFWDBusy;
    LINECALLREASON_FWDNOANSWER:Result:=crFWDNoAnswer;
    LINECALLREASON_FWDUNCOND:Result:=crFWDUnCond;
    LINECALLREASON_PICKUP:Result:=crPickup;
    LINECALLREASON_UNPARK:Result:=crUnPark;
    LINECALLREASON_REDIRECT:Result:=crRedirect;
    LINECALLREASON_CALLCOMPLETION:Result:=crCallCompletion;
    LINECALLREASON_TRANSFER:Result:=crTransfer;
    LINECALLREASON_REMINDER:Result:=crReminder;
    LINECALLREASON_UNKNOWN:Result:=crUnknown;
    LINECALLREASON_UNAVAIL:Result:=crUnavail;
    LINECALLREASON_INTRUDE:Result:=crIntrude;
{$IFDEF TAPI20}
    LINECALLREASON_PARKED:Result:=crParked ;
    LINECALLREASON_CAMPEDON:Result:=crCampedon;
    LINECALLREASON_ROUTEREQUEST:Result:=crRouterRequest;
{$ENDIF}
  end;
end;

function IntToCallOrigin(Value:LongWord):TLineCallOrigin;
begin
  Result:=coUnknown;
  case Value of
    LINECALLORIGIN_OUTBOUND:Result:=coOutBound;
    LINECALLORIGIN_INTERNAL:Result:=coInternal;
    LINECALLORIGIN_EXTERNAL:Result:=coExternal;
    LINECALLORIGIN_UNKNOWN:Result:=coUnknown;
    LINECALLORIGIN_UNAVAIL:Result:=coUnavail;
    LINECALLORIGIN_CONFERENCE:Result:=coConference;
    LINECALLORIGIN_INBOUND:Result:=coInbound;
  end;
end;

function IntToCallParamFlags(Value:LongWord):TLineCallParamFlags;
begin
  Result:=[];
  if Value and LINECALLPARAMFLAGS_BLOCKID = LINECALLPARAMFLAGS_BLOCKID then Result:=Result+[cpfBlockId];
  if Value and LINECALLPARAMFLAGS_DESTOFFHOOK = LINECALLPARAMFLAGS_DESTOFFHOOK then Result:=Result+[cpfDestOffHook];
  if Value and LINECALLPARAMFLAGS_IDLE = LINECALLPARAMFLAGS_IDLE then Result:=Result+[cpfIdle];
  if Value and LINECALLPARAMFLAGS_ORIGOFFHOOK = LINECALLPARAMFLAGS_ORIGOFFHOOK then Result:=Result+[cpfOrigOffHook];
  if Value and LINECALLPARAMFLAGS_SECURE = LINECALLPARAMFLAGS_SECURE then Result:=Result+[cpfSecure];
  {$IFDEF TAPI20}
  if Value and LINECALLPARAMFLAGS_NOHOLDCONFERENCE = LINECALLPARAMFLAGS_NOHOLDCONFERENCE then Result:=Result+[cpfNoHoldConference];
  if Value and LINECALLPARAMFLAGS_PREDICTIVEDIAL = LINECALLPARAMFLAGS_PREDICTIVEDIAL then Result:=Result+[cpfPredictiveDial];
  if Value and LINECALLPARAMFLAGS_ONESTEPTRANSFER = LINECALLPARAMFLAGS_ONESTEPTRANSFER then Result:=Result+[cpfOneStepTransfer];
  {$ENDIF}
end;

function CallParamFlagsToInt(Value:TLineCallParamFlags):LongWord;
begin
  Result:=0;
  if cpfBlockId in Value then Result:=Result or LINECALLPARAMFLAGS_BLOCKID;
  if cpfDestOffHook in Value then Result:=Result or LINECALLPARAMFLAGS_DESTOFFHOOK;
  if cpfIdle in Value then Result:=Result or LINECALLPARAMFLAGS_IDLE;
  if cpfOrigOffHook in Value then Result:=Result or LINECALLPARAMFLAGS_ORIGOFFHOOK;
  if cpfSecure in Value then Result:=Result or LINECALLPARAMFLAGS_SECURE;
  {$IFDEF TAPI20}
  if cpfNoHoldConference in Value then Result:=Result or LINECALLPARAMFLAGS_NOHOLDCONFERENCE;
  if cpfPredictiveDial in Value then Result:=Result or LINECALLPARAMFLAGS_PREDICTIVEDIAL;
  if cpfOneStepTransfer in Value then Result:=Result or LINECALLPARAMFLAGS_ONESTEPTRANSFER;
  {$ENDIF}
end;


function BearerModeToInt(AMode:TLineBearerMode):DWord;
begin
  result:=LINEBEARERMODE_SPEECH;
  case AMode of
    bmVoice:Result:=LINEBEARERMODE_VOICE;
    bmSpeech:Result:=LINEBEARERMODE_SPEECH;
    bmMultiuse:Result:=LINEBEARERMODE_MULTIUSE;
    bmData:Result:=LINEBEARERMODE_DATA;
    bmAltSpeechData:Result:=LINEBEARERMODE_ALTSPEECHDATA;
    bmNonCallSignaling:Result:=LINEBEARERMODE_NONCALLSIGNALING;
    bmPassthrough:Result:=LINEBEARERMODE_PASSTHROUGH;
    {$IFDEF TAPI20}
    bmRestrictedData:Result:=LINEBEARERMODE_RESTRICTEDDATA;
    {$ENDIF}
  end;                 
end;


function IntToBearerMode(Value:LongWord):TLineBearerMode;
begin
  result:=bmSpeech;
  case Value of
    LINEBEARERMODE_VOICE:result:=bmVoice;
    LINEBEARERMODE_SPEECH:result:=bmSpeech;
    LINEBEARERMODE_MULTIUSE:result:=bmMultiuse;
    LINEBEARERMODE_DATA:result:=bmData;
    LINEBEARERMODE_ALTSPEECHDATA:result:=bmAltSpeechData;
    LINEBEARERMODE_NONCALLSIGNALING:result:=bmNonCallSignaling;
    LINEBEARERMODE_PASSTHROUGH:result:=bmPassthrough;
    {$IFDEF TAPI20}
    LINEBEARERMODE_RESTRICTEDDATA:result:=bmRestrictedData;
    {$ENDIF}
  end;
end;

function IntToBearerModes(Value:LongWord):TLineBearerModes;
begin
  Result:=[];
  if (Value and LINEBEARERMODE_VOICE) = LINEBEARERMODE_VOICE then
    Result:=Result+[bmVoice];
  if (Value and LINEBEARERMODE_SPEECH) = LINEBEARERMODE_SPEECH then
    Result:=Result+[bmSpeech];
  if (Value and LINEBEARERMODE_MULTIUSE) =LINEBEARERMODE_MULTIUSE  then
    Result:=Result+[bmMultiuse];
  if (Value and LINEBEARERMODE_DATA) = LINEBEARERMODE_DATA then
    Result:=Result+[bmData];
  if (Value and LINEBEARERMODE_ALTSPEECHDATA) = LINEBEARERMODE_ALTSPEECHDATA then
    Result:=Result+[bmAltSpeechData];
  if (Value and LINEBEARERMODE_NONCALLSIGNALING) = LINEBEARERMODE_NONCALLSIGNALING then
    Result:=Result+[bmNonCallSignaling];
  if (Value and LINEBEARERMODE_PASSTHROUGH) = LINEBEARERMODE_PASSTHROUGH then
    Result:=Result+[bmPassthrough];
  {$IFDEF TAPI20}
  if (Value and LINEBEARERMODE_RESTRICTEDDATA) = LINEBEARERMODE_RESTRICTEDDATA then
    Result:=Result+[bmRestrictedData];
  {$ENDIF}
end;


{$IFDEF TAPI20}
function IntToCallFeatures2(Value:LongWord):TLineCallFeatures2;
begin
  Result:=[];
  if Value and LINECALLFEATURE2_NOHOLDCONFERENCE = LINECALLFEATURE2_NOHOLDCONFERENCE then
    Result:=Result+[lcf2NoHoldConference];
  if Value and LINECALLFEATURE2_ONESTEPTRANSFER = LINECALLFEATURE2_ONESTEPTRANSFER then
    Result:=Result+[lcf2OneStepTransfer];
  if Value and LINECALLFEATURE2_COMPLCAMPON = LINECALLFEATURE2_COMPLCAMPON then
    Result:=Result+[lcf2ComlpCampOn];
  if Value and LINECALLFEATURE2_COMPLCALLBACK = LINECALLFEATURE2_COMPLCALLBACK then
    Result:=Result+[lcf2ComplCallback];
  if Value and LINECALLFEATURE2_COMPLINTRUDE = LINECALLFEATURE2_COMPLINTRUDE then
    Result:=Result+[lcf2ComplIntrude];
  if Value and LINECALLFEATURE2_COMPLMESSAGE = LINECALLFEATURE2_COMPLMESSAGE then
    Result:=Result+[lcf2ComplMessage];
  if Value and LINECALLFEATURE2_TRANSFERNORM = LINECALLFEATURE2_TRANSFERNORM then
    Result:=Result+[lcf2TransferNorm];
  if Value and LINECALLFEATURE2_TRANSFERCONF = LINECALLFEATURE2_TRANSFERCONF then
    Result:=Result+[lcf2TransferConf];
  if Value and LINECALLFEATURE2_PARKDIRECT = LINECALLFEATURE2_PARKDIRECT then
    Result:=Result+[lcf2ParkDirect];
  if Value and LINECALLFEATURE2_PARKNONDIRECT = LINECALLFEATURE2_PARKNONDIRECT then
    Result:=Result+[lcf2ParkNonDirect];
end;
{$ENDIF}

function AddressModeToInt(Value:TLineAddressMode):LongWord;
begin
  Result:=0;
  case Value of
    amDialableAddr:Result:=LINEADDRESSMODE_DIALABLEADDR;
    amAddressID:Result:=LINEADDRESSMODE_ADDRESSID;
  end;
end;

function IntToAddressModes(Value:LongWord):TLineAddressModes;
begin
  result:=[];
  if (Value and LINEADDRESSMODE_DIALABLEADDR) = LINEADDRESSMODE_DIALABLEADDR then
    Result:=Result+[amDialableAddr];
  if (Value and LINEADDRESSMODE_ADDRESSID) = LINEADDRESSMODE_ADDRESSID then
    Result:=Result+[amAddressID];
end;


function IntToCallComplCond(Value:LongWord):TLineCallComplCond;
begin
  Result:=lcccNoDef;
  case Value of
    LINECALLCOMPLCOND_BUSY :Result:=lcccBusy;
    LINECALLCOMPLCOND_NOANSWER:Result:=lcccNoAnswer;
  end;
end;

function IntToCallComplConds(Value:LongWord):TLineCallComplConds;
begin
  Result:=[];
  if Value and LINECALLCOMPLCOND_BUSY = LINECALLCOMPLCOND_BUSY then
    Result:=Result+[lcccBusy];
  if Value and LINECALLCOMPLCOND_NOANSWER = LINECALLCOMPLCOND_NOANSWER then
    Result:=Result+[lcccNoAnswer];
end;

function IntToCallComplModes(Value:LongWord):TLineCallComplModes;
begin
  Result:=[];
  if Value and LINECALLCOMPLMODE_CALLBACK = LINECALLCOMPLMODE_CALLBACK then
    Result:=Result+[lccmCallBack];
  if Value and LINECALLCOMPLMODE_CAMPON = LINECALLCOMPLMODE_CAMPON then
    Result:=Result+[lccmCampon];
  if Value and LINECALLCOMPLMODE_INTRUDE = LINECALLCOMPLMODE_INTRUDE then
    Result:=Result+[lccmIntrude];
  if Value and LINECALLCOMPLMODE_MESSAGE = LINECALLCOMPLMODE_MESSAGE then
    Result:=Result+[lccmMessage];
end;


function IntToCallPartyID(Value:LongWord):TLineCallPartyID;
begin
  Result:=[];
  if Value and LINECALLPARTYID_BLOCKED = LINECALLPARTYID_BLOCKED then
    Result:=Result+[cpidBlocked];
  if Value and LINECALLPARTYID_OUTOFAREA = LINECALLPARTYID_OUTOFAREA then
    Result:=Result+[cpidOutOfArea];
  if Value and LINECALLPARTYID_NAME = LINECALLPARTYID_NAME then
    Result:=Result+[cpidName];
  if Value and LINECALLPARTYID_ADDRESS = LINECALLPARTYID_ADDRESS then
    Result:=Result+[cpidAddress];
  if Value and LINECALLPARTYID_PARTIAL = LINECALLPARTYID_PARTIAL then
    Result:=Result+[cpidPartial];
  if Value and LINECALLPARTYID_UNKNOWN = LINECALLPARTYID_UNKNOWN then
    Result:=Result+[cpidUnknown];
  if Value and LINECALLPARTYID_UNAVAIL = LINECALLPARTYID_UNAVAIL then
    Result:=Result+[cpidUnavail];
end;

function IntToCallPrivilege(Value:LongWord):TLineCallPrivilege;
begin
  Result:=[];
  if Value and LINECALLPRIVILEGE_NONE = LINECALLPRIVILEGE_NONE then
    Result:=Result+[cpNone];
  if Value and LineCallPrivilege_Owner = LineCallPrivilege_Owner then
    Result:=Result+[cpOwner];
  if Value and LineCallPrivilege_Monitor = LineCallPrivilege_Monitor then
    Result:=Result+[cpMonitor];
end;

function CallPrivilegeToInt(Value:TLineCallPrivilege):LongWord;
var OResult,MResult:LongWord;
begin
  OResult:=0;
  MResult:=0;
  if cpNone in Value then
  begin
    result:=LineCallPrivilege_None;
    exit;
  end;
  if cpOwner in Value then OResult:=LineCallPrivilege_Owner;
  if cpMonitor in Value then MResult:=LineCallPrivilege_Monitor;
  result:=OResult or MResult;
end;

{$WARNINGS OFF}
function IntToGatherTerm(Value:LongWord):TLineGatherTerm;
begin
 case Value of
   LINEGATHERTERM_BUFFERFULL:Result:=lgtBufferFull;
   LINEGATHERTERM_TERMDIGIT:Result:=lgtTermDigit;
   LINEGATHERTERM_FIRSTTIMEOUT:Result:=lgtFirstTimeOut;
   LINEGATHERTERM_INTERTIMEOUT:Result:=lgtInterTimeOut;
   LINEGATHERTERM_CANCEL:Result:=lgtCancel;
  end;
end;
{$WARNINGS ON}

function IntToBusyModes(Value:Longword):TLineBusyModes;
begin
  Result:=[];
  if Value and LINEBUSYMODE_STATION = LINEBUSYMODE_STATION then
    Result:=Result+[bymSTATION];
  if Value and LINEBUSYMODE_TRUNK = LINEBUSYMODE_TRUNK then
    Result:=Result+[bymTRUNK];
  if Value and LINEBUSYMODE_UNKNOWN = LINEBUSYMODE_UNKNOWN then
    Result:=Result+[bymUNKNOWN];
  if Value and LINEBUSYMODE_UNAVAIL = LINEBUSYMODE_UNAVAIL then
    Result:=Result+[bymUNAVAIL];
end;

function IntToBusyMode(Value:Longword):TLineBusyMode;
begin
  Result:=bymUnavail;
  case Value of
    LINEBUSYMODE_STATION:Result:=bymStation;
    LINEBUSYMODE_TRUNK:Result:=bymTrunk;
    LINEBUSYMODE_UNKNOWN:Result:=bymUnknown;
    LINEBUSYMODE_UNAVAIL:Result:=bymUnavail;
  end;
end;

function IntToDialToneModes(Value:LongWord):TLineDialtoneModes;
begin
  Result:=[];
  if Value and LINEDIALTONEMODE_NORMAL = LINEDIALTONEMODE_NORMAL then
    Result:=Result+[dtmNORMAL];
  if Value and LINEDIALTONEMODE_SPECIAL = LINEDIALTONEMODE_SPECIAL then
    Result:=Result+[dtmSPECIAL];
  if Value and LINEDIALTONEMODE_INTERNAL = LINEDIALTONEMODE_INTERNAL then
    Result:=Result+[dtmINTERNAL];
  if Value and LINEDIALTONEMODE_EXTERNAL = LINEDIALTONEMODE_EXTERNAL then
    Result:=Result+[dtmEXTERNAL];
  if Value and LINEDIALTONEMODE_UNKNOWN = LINEDIALTONEMODE_UNKNOWN then
    Result:=Result+[dtmUNKNOWN];
  if Value and LINEDIALTONEMODE_UNAVAIL = LINEDIALTONEMODE_UNAVAIL then
    Result:=Result+[dtmUNAVAIL];
end;

function IntToDialToneMode(Value:LongWord):TLineDialtoneMode;
begin
  Result:=dtmUNAVAIL;
  case Value of
    LINEDIALTONEMODE_NORMAL:Result:=dtmNORMAL;
    LINEDIALTONEMODE_SPECIAL:Result:=dtmSPECIAL;
    LINEDIALTONEMODE_INTERNAL:Result:=dtmINTERNAL;
    LINEDIALTONEMODE_EXTERNAL:Result:=dtmEXTERNAL;
    LINEDIALTONEMODE_UNKNOWN:Result:=dtmUNKNOWN;
    LINEDIALTONEMODE_UNAVAIL:Result:=dtmUNAVAIL;
  end;
end;

function IntToSpecialInfo(Value:LongWord):TLineSpecialInfo;
begin
  Result:=[];
  if Value and LINESPECIALINFO_NOCIRCUIT = LINESPECIALINFO_NOCIRCUIT then
    Result:=Result+[siNOCIRCUIT];
  if Value and LINESPECIALINFO_CUSTIRREG = LINESPECIALINFO_CUSTIRREG then
    Result:=Result+[siCUSTIRREG];
  if Value and LINESPECIALINFO_REORDER = LINESPECIALINFO_REORDER then
    Result:=Result+[siREORDER];
  if Value and LINESPECIALINFO_UNKNOWN = LINESPECIALINFO_UNKNOWN then
    Result:=Result+[siUNKNOWN];
  if Value and LINESPECIALINFO_UNAVAIL = LINESPECIALINFO_UNAVAIL then
    Result:=Result+[siUNAVAIL];
end;

function IntToDisconnectModes(Value:Longword):TLineDisconnectModes;
begin
  Result:=[];
  if Value and LINEDISCONNECTMODE_NORMAL = LINEDISCONNECTMODE_NORMAL then
    Result:=Result+[dmNORMAL];
  if Value and LINEDISCONNECTMODE_UNKNOWN = LINEDISCONNECTMODE_UNKNOWN then
    Result:=Result+[dmUNKNOWN];
  if Value and LINEDISCONNECTMODE_REJECT = LINEDISCONNECTMODE_REJECT then
    Result:=Result+[dmREJECT];
  if Value and LINEDISCONNECTMODE_PICKUP = LINEDISCONNECTMODE_PICKUP then
    Result:=Result+[dmPICKUP];
  if Value and LINEDISCONNECTMODE_FORWARDED = LINEDISCONNECTMODE_FORWARDED then
    Result:=Result+[dmFORWARDED];
  if Value and LINEDISCONNECTMODE_BUSY = LINEDISCONNECTMODE_BUSY then
    Result:=Result+[dmBUSY];
  if Value and LINEDISCONNECTMODE_NOANSWER = LINEDISCONNECTMODE_NOANSWER then
    Result:=Result+[dmNOANSWER];
  if Value and LINEDISCONNECTMODE_BADADDRESS = LINEDISCONNECTMODE_BADADDRESS then
    Result:=Result+[dmBADADDRESS];
  if Value and LINEDISCONNECTMODE_UNREACHABLE = LINEDISCONNECTMODE_UNREACHABLE then
    Result:=Result+[dmUNREACHABLE];
  if Value and LINEDISCONNECTMODE_CONGESTION = LINEDISCONNECTMODE_CONGESTION then
    Result:=Result+[dmCONGESTION];
  if Value and LINEDISCONNECTMODE_INCOMPATIBLE = LINEDISCONNECTMODE_INCOMPATIBLE then
    Result:=Result+[dmINCOMPATIBLE];
  if Value and LINEDISCONNECTMODE_UNAVAIL = LINEDISCONNECTMODE_UNAVAIL then
    Result:=Result+[dmUNAVAIL];
  if Value and LINEDISCONNECTMODE_NODIALTONE = LINEDISCONNECTMODE_NODIALTONE then
    Result:=Result+[dmNODIALTONE];
  {$IFDEF TAPI20}
  if Value and LINEDISCONNECTMODE_NUMBERCHANGED = LINEDISCONNECTMODE_NUMBERCHANGED then
    Result:=Result+[dmNUMBERCHANGED];
  if Value and LINEDISCONNECTMODE_OUTOFORDER = LINEDISCONNECTMODE_OUTOFORDER then
    Result:=Result+[dmOUTOFORDER];
  if Value and LINEDISCONNECTMODE_TEMPFAILURE = LINEDISCONNECTMODE_TEMPFAILURE then
    Result:=Result+[dmTEMPFAILURE];
  if Value and LINEDISCONNECTMODE_QOSUNAVAIL = LINEDISCONNECTMODE_QOSUNAVAIL then
    Result:=Result+[dmQOSUNAVAIL];
  if Value and LINEDISCONNECTMODE_BLOCKED = LINEDISCONNECTMODE_BLOCKED then
    Result:=Result+[dmBLOCKED];
  if Value and LINEDISCONNECTMODE_DONOTDISTURB = LINEDISCONNECTMODE_DONOTDISTURB then
    Result:=Result+[dmDONOTDISTURB];
  if Value and LINEDISCONNECTMODE_CANCELLED = LINEDISCONNECTMODE_CANCELLED then
    Result:=Result+[dmCANCELLED];
  {$ENDIF}
end;

function IntToDisconnectMode(Value:Longword):TLineDisconnectMode;
begin
  Result:=dmUnknown;
  case Value of
    LINEDISCONNECTMODE_NORMAL:Result:=dmNormal;
    LINEDISCONNECTMODE_UNKNOWN:Result:=dmUnknown;
    LINEDISCONNECTMODE_REJECT:Result:=dmReject;
    LINEDISCONNECTMODE_PICKUP:Result:=dmPickup;
    LINEDISCONNECTMODE_FORWARDED:Result:=dmForwarded;
    LINEDISCONNECTMODE_BUSY:Result:=dmBusy;
    LINEDISCONNECTMODE_NOANSWER:Result:=dmNoAnswer;
    LINEDISCONNECTMODE_BADADDRESS:Result:=dmBadAddress;
    LINEDISCONNECTMODE_UNREACHABLE:Result:=dmUnreachable;
    LINEDISCONNECTMODE_CONGESTION:Result:=dmCongestion;
    LINEDISCONNECTMODE_INCOMPATIBLE:Result:=dmIncompatible;
    LINEDISCONNECTMODE_UNAVAIL:Result:=dmUnavail;
    LINEDISCONNECTMODE_NODIALTONE:Result:=dmNoDialtone;
    {$IFDEF TAPI20}
    LINEDISCONNECTMODE_NUMBERCHANGED:Result:=dmNumberChanged;
    LINEDISCONNECTMODE_OUTOFORDER:Result:=dmOutOfOrder;
    LINEDISCONNECTMODE_TEMPFAILURE:Result:=dmTempFailure;
    LINEDISCONNECTMODE_QOSUNAVAIL:Result:=dmQOSUnavail;
    LINEDISCONNECTMODE_BLOCKED:Result:=dmBlocked;
    LINEDISCONNECTMODE_DONOTDISTURB:Result:=dmDoNotDisturb;
    LINEDISCONNECTMODE_CANCELLED:Result:=dmCancelled;
    {$ENDIF}
  end;
end;

function IntToMediaModes(Value:LongWord):TLineMediaModes;
begin
  Result:=[];
  if Value and LINEMEDIAMODE_UNKNOWN = LINEMEDIAMODE_UNKNOWN then
    Result:=Result+[mmUnknown];
  if Value and LINEMEDIAMODE_INTERACTIVEVOICE = LINEMEDIAMODE_INTERACTIVEVOICE then
    Result:=Result+[mmInteractiveVoice];
  if Value and LINEMEDIAMODE_AUTOMATEDVOICE = LINEMEDIAMODE_AUTOMATEDVOICE then
    Result:=Result+[mmAutomatedVoice];
  if Value and LINEMEDIAMODE_DATAMODEM = LINEMEDIAMODE_DATAMODEM then
    Result:=Result+[mmDataModem];
  if Value and LINEMEDIAMODE_G3FAX = LINEMEDIAMODE_G3FAX then
    Result:=Result+[mmG3FAX];
  if Value and LINEMEDIAMODE_TDD = LINEMEDIAMODE_TDD then
    Result:=Result+[mmTDD];
  if Value and LINEMEDIAMODE_G4FAX = LINEMEDIAMODE_G4FAX then
    Result:=Result+[mmG4FAX];
  if Value and LINEMEDIAMODE_DIGITALDATA = LINEMEDIAMODE_DIGITALDATA then
    Result:=Result+[mmDigitalData];
  if Value and LINEMEDIAMODE_TELETEX = LINEMEDIAMODE_TELETEX then
    Result:=Result+[mmTeletex];
  if Value and LINEMEDIAMODE_VIDEOTEX = LINEMEDIAMODE_VIDEOTEX then
    Result:=Result+[mmVideotex];
  if Value and LINEMEDIAMODE_TELEX = LINEMEDIAMODE_TELEX then
    Result:=Result+[mmTelex];
  if Value and LINEMEDIAMODE_MIXED = LINEMEDIAMODE_MIXED then
    Result:=Result+[mmMIXED];
  if Value and LINEMEDIAMODE_ADSI = LINEMEDIAMODE_ADSI then
    Result:=Result+[mmADSI];
  if Value and LINEMEDIAMODE_VOICEVIEW = LINEMEDIAMODE_VOICEVIEW then
    Result:=Result+[mmVoiceView];
  {$IFDEF TAPI21}
  if Value and LINEMEDIAMODE_VIDEO = LINEMEDIAMODE_VIDEO then
    Result:=Result+[mmVideo];
  {$ENDIF}
end;

function MediaModesToInt(MM:TLineMediaModes):LongWord;
begin
  Result:=0;
  if mmUnknown in MM then Result:=Result + LINEMEDIAMODE_UNKNOWN;
  if mmInteractiveVoice in MM then Result:=Result +LINEMEDIAMODE_INTERACTIVEVOICE;
  if mmAutomatedVoice in MM then Result:=Result +LINEMEDIAMODE_AUTOMATEDVOICE;
  if mmDataModem in MM then Result:=Result +LINEMEDIAMODE_DATAMODEM;
  if mmG3FAX in MM then Result:=Result +LINEMEDIAMODE_G3FAX;
  if mmTDD in MM then Result:=Result +LINEMEDIAMODE_TDD ;
  if mmG4FAX in MM then Result:=Result +LINEMEDIAMODE_G4FAX ;
  if mmDigitalData in MM then Result:=Result +LINEMEDIAMODE_DIGITALDATA;
  if mmTeletex in MM then Result:=Result +LINEMEDIAMODE_TELETEX;
  if mmVideotex in MM then Result:=Result +LINEMEDIAMODE_VIDEOTEX;
  if mmTelex in MM then Result:=Result +LINEMEDIAMODE_TELEX;
  if mmMIXED in MM then Result:=Result +LINEMEDIAMODE_MIXED;
  if mmADSI in MM then Result:=Result +LINEMEDIAMODE_ADSI;
  if mmVoiceView in MM then Result:=Result +LINEMEDIAMODE_VOICEVIEW;
  {$IFDEF TAPI21}
  if mmVideo in MM then Result:=Result +LINEMEDIAMODE_VIDEO;
  {$ENDIF}
  if MM=[] then Result:=LINEMEDIAMODE_UNKNOWN;
end;

function IntToConnectedModes(Value:LongWord):TLineConnectedModes;
begin
  Result:=[];
  if Value and LINECONNECTEDMODE_ACTIVE = LINECONNECTEDMODE_ACTIVE then Result:=Result+[lcmActive];
  if Value and LINECONNECTEDMODE_INACTIVE = LINECONNECTEDMODE_INACTIVE then Result:=Result+[lcmInactive];
  {$IFDEF TAPI20}
  if Value and LINECONNECTEDMODE_ACTIVEHELD = LINECONNECTEDMODE_ACTIVEHELD then Result:=Result+[lcmActiveHeld];
  if Value and LINECONNECTEDMODE_INACTIVEHELD = LINECONNECTEDMODE_INACTIVEHELD then Result:=Result+[lcmInactiveHeld];
  if Value and LINECONNECTEDMODE_CONFIRMED = LINECONNECTEDMODE_CONFIRMED then Result:=Result+[lcmConfirmed];
  {$ENDIF}
end;

function IntToOfferingModes(Value:LongWord):TLineOfferingModes;
begin
  Result:=[];
  if Value and LINEOFFERINGMODE_ACTIVE = LINEOFFERINGMODE_ACTIVE then Result:=Result+[lomActive];
  if Value and LINEOFFERINGMODE_INACTIVE = LINEOFFERINGMODE_INACTIVE then Result:=Result+[lomInactive];
end;

function IntToCallState(Value:LongWord):TLineCallState;
begin
  result:=csUnKnown;
  case Value of
    LINECALLSTATE_IDLE:result:=csIDLE;
    LINECALLSTATE_OFFERING:result:=csOffering;
    LINECALLSTATE_ACCEPTED:result:=csAccepted;
    LINECALLSTATE_DIALTONE:result:=csDialtone;
    LINECALLSTATE_DIALING:result:=csDialing;
    LINECALLSTATE_RINGBACK:result:=csRingBack;
    LINECALLSTATE_BUSY:result:=csBusy;
    LINECALLSTATE_SPECIALINFO:result:=csSpecialInfo;
    LINECALLSTATE_CONNECTED:result:=csConnected;
    LINECALLSTATE_PROCEEDING:result:=csProceeding;
    LINECALLSTATE_ONHOLD:result:=csOnHold;
    LINECALLSTATE_CONFERENCED:result:=csConferenced;
    LINECALLSTATE_ONHOLDPENDCONF:result:=csOnHoldPendConf;
    LINECALLSTATE_ONHOLDPENDTRANSFER:result:=csOnHoldPendTransfer;
    LINECALLSTATE_DISCONNECTED:result:=csDisconnected;
    LINECALLSTATE_UNKNOWN:result:=csUnknown;
  end;
end;

function IntToCallStates(Value:LongWord):TLineCallStates;
begin
  result:=[];
  if Value and LINECALLSTATE_IDLE = LINECALLSTATE_IDLE then Result:=Result+[csIDLE];
  if Value and LINECALLSTATE_OFFERING = LINECALLSTATE_OFFERING then Result:=Result+[csOffering];
  if Value and LINECALLSTATE_ACCEPTED = LINECALLSTATE_ACCEPTED then Result:=Result+[csAccepted];
  if Value and LINECALLSTATE_DIALTONE = LINECALLSTATE_DIALTONE then Result:=Result+[csDialtone];
  if Value and LINECALLSTATE_DIALING = LINECALLSTATE_DIALING then Result:=Result+[csDialing];
  if Value and LINECALLSTATE_RINGBACK = LINECALLSTATE_RINGBACK then Result:=Result+[csRingBack];
  if Value and LINECALLSTATE_BUSY = LINECALLSTATE_BUSY then Result:=Result+[csBusy];
  if Value and LINECALLSTATE_SPECIALINFO = LINECALLSTATE_SPECIALINFO then Result:=Result+[csSpecialInfo];
  if Value and LINECALLSTATE_CONNECTED = LINECALLSTATE_CONNECTED then Result:=Result+[csConnected];
  if Value and LINECALLSTATE_PROCEEDING = LINECALLSTATE_PROCEEDING then Result:=Result+[csProceeding];
  if Value and LINECALLSTATE_ONHOLD = LINECALLSTATE_ONHOLD then Result:=Result+[csOnHold];
  if Value and LINECALLSTATE_CONFERENCED = LINECALLSTATE_CONFERENCED then Result:=Result+[csConferenced];
  if Value and LINECALLSTATE_ONHOLDPENDCONF = LINECALLSTATE_ONHOLDPENDCONF then Result:=Result+[csOnHoldPendConf];
  if Value and LINECALLSTATE_ONHOLDPENDTRANSFER = LINECALLSTATE_ONHOLDPENDTRANSFER then Result:=Result+[csOnHoldPendTransfer];
  if Value and LINECALLSTATE_DISCONNECTED = LINECALLSTATE_DISCONNECTED then Result:=Result+[csDisconnected];
  if Value and LINECALLSTATE_UNKNOWN = LINECALLSTATE_UNKNOWN then Result:=Result+[csUnknown];
end;

function CallStatesToInt(Value:TLineCallStates):LongWord;
begin
  result:=0;
  if csAccepted in Value then Result:=Result or LINECALLSTATE_ACCEPTED;
  if csBusy in Value then Result:=Result or LINECALLSTATE_BUSY;
  if csConferenced in Value then Result:=Result or LINECALLSTATE_CONFERENCED;
  if csConnected in Value then Result:=Result or LINECALLSTATE_CONNECTED;
  if csDialing in Value then Result:=Result or LINECALLSTATE_DIALING;
  if csDialtone in Value then Result:=Result or LINECALLSTATE_DIALTONE;
  if csDisconnected in Value then Result:=Result or LINECALLSTATE_DISCONNECTED;
  if csIDLE in Value then Result:=Result or LINECALLSTATE_IDLE;
  if csOffering in Value then Result:=Result or LINECALLSTATE_OFFERING;
  if csOnHold in Value then Result:=Result or LINECALLSTATE_ONHOLD;
  if csOnHoldPendConf in Value then Result:=Result or LINECALLSTATE_ONHOLDPENDCONF;
  if csOnHoldPendTransfer in Value then Result:=Result or LINECALLSTATE_ONHOLDPENDTRANSFER;
  if csProceeding in Value then Result:=Result or LINECALLSTATE_PROCEEDING;
  if csRingBack in Value then Result:=Result or LINECALLSTATE_RINGBACK;
  if csSpecialInfo in Value then Result:=Result or LINECALLSTATE_SPECIALINFO;
  if csUnknown in Value then Result:=Result or LINECALLSTATE_UNKNOWN;
end;

function IntToCallFeatures(Value:Longword):TLineCallFeatures;
begin
  Result:=[];
  if Value and LINECALLFEATURE_ACCEPT = LINECALLFEATURE_ACCEPT then Result:=Result+[lcfACCEPT];
  if Value and LINECALLFEATURE_ADDTOCONF = LINECALLFEATURE_ADDTOCONF then Result:=Result+[lcfADDTOCONF];
  if Value and LINECALLFEATURE_ANSWER = LINECALLFEATURE_ANSWER then Result:=Result+[lcfANSWER];
  if Value and LINECALLFEATURE_BLINDTRANSFER = LINECALLFEATURE_BLINDTRANSFER then Result:=Result+[lcfBLINDTRANSFER];
  if Value and LINECALLFEATURE_COMPLETECALL = LINECALLFEATURE_COMPLETECALL then Result:=Result+[lcfCOMPLETECALL];
  if Value and LINECALLFEATURE_COMPLETETRANSF = LINECALLFEATURE_COMPLETETRANSF then Result:=Result+[lcfCOMPLETETRANSF];
  if Value and LINECALLFEATURE_DIAL = LINECALLFEATURE_DIAL then Result:=Result+[lcfDIAL];
  if Value and LINECALLFEATURE_DROP = LINECALLFEATURE_DROP then Result:=Result+[lcfDROP];
  if Value and LINECALLFEATURE_GATHERDIGITS = LINECALLFEATURE_GATHERDIGITS then Result:=Result+[lcfGATHERDIGITS];
  if Value and LINECALLFEATURE_GENERATEDIGITS = LINECALLFEATURE_GENERATEDIGITS then Result:=Result+[lcfGENERATEDIGITS];
  if Value and LINECALLFEATURE_GENERATETONE = LINECALLFEATURE_GENERATETONE then Result:=Result+[lcfGENERATETONE];
  if Value and LINECALLFEATURE_HOLD = LINECALLFEATURE_HOLD then Result:=Result+[lcfHOLD];
  if Value and LINECALLFEATURE_MONITORDIGITS = LINECALLFEATURE_MONITORDIGITS then Result:=Result+[lcfMONITORDIGITS];
  if Value and LINECALLFEATURE_MONITORMEDIA = LINECALLFEATURE_MONITORMEDIA then Result:=Result+[lcfMONITORMEDIA];
  if Value and LINECALLFEATURE_MONITORTONES = LINECALLFEATURE_MONITORTONES then Result:=Result+[lcfMONITORTONES];
  if Value and LINECALLFEATURE_PARK = LINECALLFEATURE_PARK then Result:=Result+[lcfPARK];
  if Value and LINECALLFEATURE_PREPAREADDCONF = LINECALLFEATURE_PREPAREADDCONF then Result:=Result+[lcfPREPAREADDCONF];
  if Value and LINECALLFEATURE_REDIRECT = LINECALLFEATURE_REDIRECT then Result:=Result+[lcfREDIRECT];
  if Value and LINECALLFEATURE_REMOVEFROMCONF = LINECALLFEATURE_REMOVEFROMCONF then Result:=Result+[lcfREMOVEFROMCONF];
  if Value and LINECALLFEATURE_SECURECALL = LINECALLFEATURE_SECURECALL then Result:=Result+[lcfSECURECALL];
  if Value and LINECALLFEATURE_SENDUSERUSER = LINECALLFEATURE_SENDUSERUSER then Result:=Result+[lcfSENDUSERUSER];
  if Value and LINECALLFEATURE_SETCALLPARAMS = LINECALLFEATURE_SETCALLPARAMS then Result:=Result+[lcfSETCALLPARAMS];
  if Value and LINECALLFEATURE_SETMEDIACONTROL = LINECALLFEATURE_SETMEDIACONTROL then Result:=Result+[lcfSETMEDIACONTROL];
  if Value and LINECALLFEATURE_SETTERMINAL = LINECALLFEATURE_SETTERMINAL then Result:=Result+[lcfSETTERMINAL];
  if Value and LINECALLFEATURE_SETUPCONF = LINECALLFEATURE_SETUPCONF then Result:=Result+[lcfSETUPCONF];
  if Value and LINECALLFEATURE_SETUPTRANSFER = LINECALLFEATURE_SETUPTRANSFER then Result:=Result+[lcfSETUPTRANSFER];
  if Value and LINECALLFEATURE_SWAPHOLD = LINECALLFEATURE_SWAPHOLD then Result:=Result+[lcfSWAPHOLD];
  if Value and LINECALLFEATURE_UNHOLD = LINECALLFEATURE_UNHOLD then Result:=Result+[lcfUNHOLD];
  if Value and LINECALLFEATURE_RELEASEUSERUSERINFO = LINECALLFEATURE_RELEASEUSERUSERINFO then Result:=Result+[lcfRELEASEUSERUSERINFO];
  {$IFDEF TAPI20}
  if Value and LINECALLFEATURE_SETTREATMENT = LINECALLFEATURE_SETTREATMENT then Result:=Result+[lcfSETTREATMENT];
  if Value and LINECALLFEATURE_SETQOS = LINECALLFEATURE_SETQOS then Result:=Result+[lcfSETQOS];
  if Value and LINECALLFEATURE_SETCALLDATA = LINECALLFEATURE_SETCALLDATA then Result:=Result+[lcfSETCALLDATA];
  {$ENDIF}
end;



{ TCallParams }

constructor TCallParams.Create(Owner:TComponent);
begin
  inherited Create(Owner);
  FBearerMode:= bmVoice;
  FMinRate:= 0;
  FMaxRate:= 0;
  FMediaMode:= [mmInteractiveVoice] ;
  FAddressID:= 0;
  FDialParams:=TDialParams.Create;
  {$IFDEF TAPI20}
  FProxyRequests:= [];
  FUseProxy:=False;
  {$ENDIF}
end;

destructor TCallParams.Destroy;
begin
  FDialParams.Destroy;
  inherited;
end;

procedure TCallParams.GetParamStruct(var Struct:PLineCallParams);
var Offset:Integer;
    {$IFDEF TAPI20}
    ReqStruct:Array of DWord;
    i,y:Integer;
    {$ENDIF}
    //Dummy:Array[0..3] of Char;
begin
  Offset:=SizeOf(TLineCallParams)+1;
  Struct^.dwBearerMode:=BearerModeToInt(FBearerMode);
  Struct^.dwMinRate:=FMinRate;
  Struct^.dwMaxRate:=FMaxRate;
  Struct^.dwMediaMode:=MediaModesToInt(FMediaMode);
  Struct^.dwCallParamFlags:=CallParamFlagsToInt(FCallParamFlags);
  Struct^.dwAddressMode:=AddressModeToInt(FAddressMode);
  //if FAddressMode=amAddressID then
    Struct^.dwAddressID:=FAddressID;
  //else
  //  Struct^.dwAddressID:=$FFFFFFFF;
  Struct^.DialParams.dwDialPause:=FDialParams.DialPause;
  Struct^.DialParams.dwDialSpeed:=FDialParams.DialSpeed;
  Struct^.DialParams.dwDigitDuration:=FDialParams.DigitDuration;
  Struct^.DialParams.dwWaitForDialtone:=FDialParams.WaitForDialtone;
  if Length(FOrigAddress)> 0 then
  begin
    Struct^.dwOrigAddressSize:=Length(FOrigAddress)+1;
    //strCopy(PChar(Struct)+Offset,PChar(FOrigAddress));
    //strCopy(PChar(Struct)+Offset+Length(FOrigAddress)+1,#0);
    Struct^.dwOrigAddressOffset:=Offset;
    StrCat(Pchar(Struct)+Offset,PChar(FOrigAddress));
    Offset:=Offset+Length(FOrigAddress)+1; //+2
  end
  else
  begin
    Struct^.dwOrigAddressSize:=0;
    Struct^.dwOrigAddressOffset:=0;
  end;
  //
  if Length(FDisplayableAddress) > 0 then
  begin
    Struct^.dwDisplayableAddressSize:=Length(FDisplayableAddress)+1;
    //strCopy(PChar(Struct)+Offset,PChar(FDisplayableAddress));
    Struct^.dwDisplayableAddressOffset:=Offset;
    StrCat(Pchar(Struct)+Offset,PChar(FDisplayableAddress));
    Offset:=Offset+Length(FDisplayableAddress)+1;
  end
  else
  begin
    Struct^.dwDisplayableAddressSize:=0;
    Struct^.dwDisplayableAddressOffset:=0;
  end;
  if Length(FCalledParty) > 0 then
  begin
    Struct^.dwCalledPartySize:=Length(FCalledParty)+1;
    //StrCopy(PChar(Struct)+Offset,PChar(FCalledParty));
    Struct^.dwCalledPartyOffset:=Offset;
    StrCat(PChar(Struct)+Offset,PChar(FCalledParty));
    Offset:=Offset+Length(FCalledParty)+1;
    
  end
  else
  begin
    Struct^.dwCalledPartySize:=0;
    Struct^.dwCalledPartyOffset:=0;
  end;
  if Length(FComment) > 0 then
  begin
    Struct^.dwCommentSize:=Length(FComment)+1;
    //StrCopy(PChar(Struct)+Offset,PChar(FComment));
    Struct^.dwCommentOffset:=Offset;
    StrCat(PChar(Struct)+Offset,PChar(FComment));
    Offset:=Offset+Length(FComment)+1;
  end
  else
  begin
    Struct^.dwCommentSize:=0;
    Struct^.dwCommentOffset:=0;
  end;

  if Length(FUserUserInfo) > 0 then
  begin
    Struct^.dwUserUserInfoSize:=Length(FUserUserInfo)+1;
    //StrCopy(PChar(Struct)+Offset,PChar(FUserUserInfo));
    Struct^.dwUserUserInfoOffset:=Offset;
    StrCat(PChar(Struct)+Offset,PChar(FUserUserInfo));
    Offset:=Offset+Length(FUserUserInfo)+1;
  end
  else
  begin
    Struct^.dwUserUserInfoSize:=0;
    Struct^.dwUserUserInfoOffset:=0;
  end;

  if Length(FHighLevelComp) > 0 then
  begin
    Struct^.dwHighLevelCompSize:=Length(FHighLevelComp)+1;
    //StrCopy(PChar(Struct)+Offset,PChar(FHighLevelComp));
    Struct^.dwHighLevelCompOffset:=Offset;
    StrCat(PChar(Struct)+Offset,PChar(FHighLevelComp));
    Offset:=Offset+Length(FHighLevelComp)+1;

  end
  else
  begin
    Struct^.dwHighLevelCompSize:=0;
    Struct^.dwHighLevelCompOffset:=0;
  end;

  if Length(FLowLevelComp) > 0 then
  begin
    Struct^.dwLowLevelCompSize:=Length(FLowLevelComp)+1;
    //StrCopy(PChar(Struct)+Offset,PChar(FLowLevelComp));
    Struct^.dwLowLevelCompOffset:=Offset;
    StrCat(PChar(Struct)+Offset,PChar(FLowLevelComp));
    Offset:=Offset+Length(FLowLevelComp)+1;
  end
  else
  begin
    Struct^.dwLowLevelCompSize:=0;
    Struct^.dwLowLevelCompOffset:=0;
  end;

  if Length(FDevSpecific) > 0 then
  begin
    Struct^.dwDevSpecificSize:=Length(FDevSpecific)+1;
    //StrCopy(PChar(Struct)+Offset,PChar(FDevSpecific));
    Struct^.dwDevSpecificOffset:=Offset;
    StrCat(PChar(Struct)+Offset,PChar(FDevSpecific));
    {$IFDEF TAPI20}
    Offset:=Offset+Length(FDevSpecific)+1;
    {$ENDIF}
  end
  else
  begin
    Struct^.dwDevSpecificSize:=0;
    Struct^.dwDevSpecificOffset:=0;
  end;


  {$IFDEF TAPI20}
  if FUseProxy then
  begin
    i:=1;
    if lprSetAgentGroup in FProxyRequests then
    begin
      SetLength(ReqStruct,i);
      ReqStruct[i-1]:=LINEPROXYREQUEST_SETAGENTGROUP;
      inc(i);
    end;
    if lprSetAgentState in FProxyRequests then
    begin
      SetLength(ReqStruct,i);
      ReqStruct[i-1]:=LINEPROXYREQUEST_SETAGENTSTATE;
      inc(i);
    end;
    if lprSetAgentActivity in FProxyRequests then
    begin
      SetLength(ReqStruct,i);
      ReqStruct[i-1]:=LINEPROXYREQUEST_SETAGENTACTIVITY;
      inc(i);
    end;
    if lprGetAgentCaps in FProxyRequests then
    begin
      SetLength(ReqStruct,i);
      ReqStruct[i-1]:=LINEPROXYREQUEST_GETAGENTCAPS ;
      inc(i);
    end;
    if lprGetAgentStatus in FProxyRequests then
    begin
      SetLength(ReqStruct,i);
      ReqStruct[i-1]:=LINEPROXYREQUEST_GETAGENTSTATUS;
      inc(i);
    end;
    if lprAgentSpecific in FProxyRequests then
    begin
      SetLength(ReqStruct,i);
      ReqStruct[i-1]:=LINEPROXYREQUEST_AGENTSPECIFIC;
      inc(i);
    end;
    if lprGetAgentActivityList in FProxyRequests then
    begin
      SetLength(ReqStruct,i);
      ReqStruct[i-1]:=LINEPROXYREQUEST_GETAGENTACTIVITYLIST ;
      inc(i);
    end;
    if lprGetAgentGroupList in FProxyRequests then
    begin
      SetLength(ReqStruct,i);
      ReqStruct[i-1]:=LINEPROXYREQUEST_GETAGENTGROUPLIST;
      inc(i);
    end;
    {$IFDEF TAPI22}
    if lprCreateAgent in FProxyRequests then
    begin
      SetLength(ReqStruct,i);
      ReqStruct[i-1]:=LINEPROXYREQUEST_CREATEAGENT;
      inc(i);
    end;
    if lprSetAgentMeasurementPeriod in FProxyRequests then
    begin
      SetLength(ReqStruct,i);
      ReqStruct[i-1]:=LINEPROXYREQUEST_SETAGENTMEASUREMENTPERIOD;
      inc(i);
    end;
    if lprGetAgentInfo in FProxyRequests then
    begin
      SetLength(ReqStruct,i);
      ReqStruct[i-1]:=LINEPROXYREQUEST_GETAGENTINFO;
      inc(i);
    end;
    if lprCreateAgentSession in FProxyRequests then
    begin
      SetLength(ReqStruct,i);
      ReqStruct[i-1]:=LINEPROXYREQUEST_CREATEAGENTSESSION;
      inc(i);
    end;
    if lprGetAgentSessionList in FProxyRequests then
    begin
      SetLength(ReqStruct,i);
      ReqStruct[i-1]:=LINEPROXYREQUEST_GETAGENTSESSIONLIST;
      inc(i);
    end;
    if lprSetAgentSessionState in FProxyRequests then
    begin
      SetLength(ReqStruct,i);
      ReqStruct[i-1]:=LINEPROXYREQUEST_SETAGENTSESSIONSTATE;
      inc(i);
    end;
    if lprGetAgentSessionInfo in FProxyRequests then
    begin
      SetLength(ReqStruct,i);
      ReqStruct[i-1]:=LINEPROXYREQUEST_GETAGENTSESSIONINFO;
      inc(i);
    end;
    if lprGetQueueList in FProxyRequests then
    begin
      SetLength(ReqStruct,i);
      ReqStruct[i-1]:=LINEPROXYREQUEST_GETQUEUELIST;
      inc(i);
    end;
    if lprSetQueueMeasurementPeriod in FProxyRequests then
    begin
      SetLength(ReqStruct,i);
      ReqStruct[i-1]:=LINEPROXYREQUEST_SETQUEUEMEASUREMENTPERIOD;
      inc(i);
    end;
    if lprGetQueueInfo in FProxyRequests then
    begin
      SetLength(ReqStruct,i);
      ReqStruct[i-1]:=LINEPROXYREQUEST_GETQUEUEINFO;
      inc(i);
    end;
    if lprGetGroupList in FProxyRequests then
    begin
      SetLength(ReqStruct,i);
      ReqStruct[i-1]:=LINEPROXYREQUEST_GETGROUPLIST;
      inc(i);
    end;
    if lprSetAgentStateEx in FProxyRequests then
    begin
      SetLength(ReqStruct,i);
      ReqStruct[i-1]:=LINEPROXYREQUEST_SETAGENTSTATEEX;
      inc(i);
    end;
    {$ENDIF}
    Struct^.dwDevSpecificSize:=(i-1)*4;
    Struct^.dwDevSpecificOffset:=Offset;
    for y:=0 to i-1 do
    begin
      //Dummy[1]:=PChar(HiWord(ReqStruct[y]))^;
      StrCopy(PChar(Struct)+Offset,PChar(ReqStruct)+(y*4));
      StrCopy(PChar(Struct)+Offset+1,PChar(ReqStruct)+(y*4)+1);
      StrCopy(PChar(Struct)+Offset+2,PChar(ReqStruct)+(y*4)+2);
      StrCopy(PChar(Struct)+Offset+3,PChar(ReqStruct)+(y*4)+3);
      {StrCopy(PChar(Struct)+Offset,PChar(ReqStruct)+y+1);
      StrCopy(PChar(Struct)+Offset,PChar(ReqStruct)+y+2);
      StrCopy(PChar(Struct)+Offset,PChar(ReqStruct)+y+3);}
      //StrCat(PChar(Struct)+Offset,PChar(@ReqStruct[y]));
      //StrCat(PChar(Struct)+Offset,PChar(Dummy[2]));
      //StrCat(PChar(Struct)+Offset,PChar(Dummy[3]));
      inc(Offset,4);
    end;
  end;

  Struct^.dwPredictiveAutoTransferStates:=CallStatesToInt(FPredictiveAutoTransferStates);
  if Length(FTargetAddress) > 0 then
  begin
    Struct^.dwTargetAddressSize:=Length(FTargetAddress)+1;
    //StrCopy(PChar(Struct)+Offset,PChar(FTargetAddress));
    Struct^.dwTargetAddressOffset:=Offset;
    StrCat(PChar(Struct)+Offset,PChar(FTargetAddress));
    Offset:=Offset+Length(FTargetAddress)+1;
  end
  else
  begin
    Struct^.dwTargetAddressSize:=0;
    Struct^.dwTargetAddressOffset:=0;
  end;
  
  // WinSock2
  Struct^.dwSendingFlowspecSize:=0;
  Struct^.dwSendingFlowspecOffset:=0;
  Struct^.dwReceivingFlowspecSize:=0;
  Struct^.dwReceivingFlowspecOffset:=0;

  if Length(FDeviceClass) > 0 then
  begin
    Struct^.dwDeviceClassSize:=Length(FDeviceClass)+1;
    //StrCopy(PChar(Struct)+Offset,PChar(FDeviceClass));
    Struct^.dwDeviceClassOffset:=Offset;
    Offset:=Offset+Length(FDeviceClass)+1;
    StrCat(PChar(Struct)+Offset,PChar(FDeviceClass));
  end
  else
  begin
    Struct^.dwDeviceClassSize:=0;
    Struct^.dwDeviceClassOffset:=0;
  end;

  if Assigned(FDeviceConfig) then
  begin
    Struct^.dwDeviceConfigSize:=FDeviceConfig.Config.GetSize+1;
    //StrCopy(PChar(Struct)+Offset,PChar(FDeviceConfig.Config.GetStruct));
    Struct^.dwDeviceConfigOffset:=Offset;
    StrCat(PChar(Struct)+Offset,PChar(FDeviceConfig.Config.GetStruct));
    Offset:=Offset+FDeviceConfig.Config.GetSize+1;
  end
  else
  begin
    Struct^.dwDeviceConfigSize:=0;
    Struct^.dwDeviceConfigOffset:=0;
  end;

  if Length(FCallData) > 0 then
  begin
    Struct^.dwCallDataSize:=Length(FCallData)+1;
    StrCopy(PChar(Struct)+Offset,PChar(FCallData));
    //Struct^.dwCallDataOffset:=Offset;
    Offset:=Offset+Length(FCallData)+1;
    StrCat(PChar(Struct)+Offset,PChar(FCallData));
  end
  else
  begin
    Struct^.dwCallDataSize:=0;
    Struct^.dwCallDataOffset:=0;
  end;

  Struct^.dwNoAnswerTimeout:=FNoAnswerTimeout;

  if Length(FCallingPartyID) > 0 then
  begin
    Struct^.dwCallingPartyIDSize:=Length(FCallingPartyID)+1;
    //StrCopy(PChar(Struct)+Offset,PChar(FCallingPartyID));
    Struct^.dwCallingPartyIDOffset:=Offset;
    StrCat(PChar(Struct)+Offset,PChar(FCallingPartyID));
   // Offset:=Offset+Length(FCallingPartyID)+1;
  end
  else
  begin
    Struct^.dwCallingPartyIDSize:=0;
    Struct^.dwCallingPartyIDOffset:=0;
  end;
  {$ENDIF}

  {$IFDEF TAPI30}
  Struct^.dwAddressType:=0;
  {$ENDIF}

end;


{ TCallStatus }

constructor TCallStatus.Create(ACall: HCall);
var CallStatus:PLineCallStatus;
    Size:Integer;
    R:Longint;
begin
  inherited Create;
  Size:=SizeOf(TLineCallStatus)+1000;
  GetMem(PLINECALLSTATUS(CallStatus),Size);
  try
    CallStatus.dwTotalSize:=Size;
    R:=LineGetCallStatus(ACall,CallStatus);
    if DWord(R)<> 0 then RaiseTAPILineError(R);
    SetStatus(CallStatus);
  finally
    FreeMem(CallStatus);
  end;
end;

destructor TCallStatus.Destroy;
begin
  inherited;

end;

function TCallStatus.GetCallStateBusyMode: TLineBusyModes;
begin
  if csBusy in FCallState then
  result:=IntToBusyModes(FCallStateMode)
  else
  result:=[];
end;

function TCallStatus.GetCallStateDialToneMode: TLineDialToneModes;
begin
  if csDialTone in FCallState then
  result:=IntToDialToneModes(FCallStateMode)
  else
  result:=[];
end;

function TCallStatus.GetCallStateDisconnectMode: TLineDisConnectModes;
begin
  if csDisconnected in FCallState then
  result:=IntToDisconnectModes(FCallStateMode)
  else
  result:=[];
end;

function TCallStatus.GetCallStateSpecialInfo: TLineSpecialInfo;
begin
  if csSpecialInfo in FCallState then
  result:=IntToSpecialInfo(FCallStateMode)
  else
  result:=[];
end;


procedure TCallStatus.SetStatus(CStatus: PLineCallStatus);
begin
  FCallState:=IntToCallStates(CStatus^.dwCallState);
  FCallStateMode:=CStatus^.dwCallStateMode;
  FCallPrivilege:=IntToCallPrivilege(CStatus^.dwCallPrivilege);
  FCallFeatures:=IntToCallFeatures(CStatus^.dwCallFeatures);
  {$IFDEF TAPI20}
  FCallFeatures2:=IntToCallFeatures2(CStatus^.dwCallFeatures2);
    {$IFDEF WIN32}
      FtStateEntryTime:=CStatus^.tStateEntryTime;
    {$ELSE}
      FtStateEntryTime:=CStatus^.tStateEntryTime;
    {$ENDIF}
  {$ENDIF}
end;

{ TTAPICall }

function TTAPICall.Accept: LongWord;
var R:Longint;
begin
  Result:=0;
  if Handle <> DWORD(-1)then
  begin
    if FAccepted=False then
    begin
      R:=LineAccept(Handle,PChar(FUserUserInfo+#0),Length(FUserUserInfo));
      {$IFDEF DEBUG}
      OutputDebugString(PChar('Accept AsynID='+IntToStr(R)));
      {$ENDIF}
      if DWord(R)>DWord($80000000) then
      begin
        RaiseTAPILineError(R);
      end
      else
      begin
        AppTAPIMgr.AsyncList.Add(afAccept,R,self);
        Result:=R;
        FAccepted:=True;
      end;
    end
    else
      Result:=1;
  end;
end;

function TTAPICall.Answer: LongWord;
  var R:Longint;
begin
  Result:=0;
  if Handle <> DWORD(-1)then
  begin
    if FAnswered=False then
    begin
      R:=LineAnswer(Handle,Pchar(FUserUserInfo),Length(FUserUserInfo));
      {$IFDEF DEBUG}
      OutputDebugString(PChar('Answer AsynID='+IntToStr(R)));
      {$ENDIF}
      if DWord(R)>DWord($80000000) then
      begin
        RaiseTAPILineError(R);
      end
      else
      begin
        AppTAPIMgr.AsyncList.Add(afAnswer,R,self);
        FAnswered:=True;
        Result:=R;
      end;
    end;
  end;
end;

function TTAPICall.Hold: LongWord;
var R:Longint;
begin
  Result:=0;
  if Handle <> DWORD(-1)then
  begin
    R:=LineHold(Handle);
    {$IFDEF DEBUG}
    OutputDebugString(PChar('Hold AsynID='+IntToStr(R)));
    {$ENDIF}
    if DWord(R)>DWord($80000000) then
    begin
      RaiseTAPILineError(R);
    end
    else
    begin
      AppTAPIMgr.AsyncList.Add(afHold,R,self);
      Result:=R;
    end;
  end;
end;

function TTAPICall.UnHold: LongWord;
var R:Longint;
begin
  Result:=0;
  if Handle <> DWORD(-1)then
  begin
    R:=LineUnhold(Handle);
    {$IFDEF DEBUG}
    OutputDebugString(PChar('UnHold AsynID='+IntToStr(R)));
    {$ENDIF}
    if DWord(R)>DWord($80000000) then
    begin
      RaiseTAPILineError(R);
    end
    else
    begin
      AppTAPIMgr.AsyncList.Add(afAnswer,R,self);
      FAnswered:=True;
      Result:=R;
    end;
  end;
end;

function TTAPICall.BlindTransfer(TransferDestAddress:String;CountryCode:DWord):LongWord;
var R:Longint;
begin
  Result:=0;
  R:=LineBlindTransfer(Handle,PChar(TransferDestAddress),CountryCode);
  if R<-1 then
  begin
    RaiseTAPILineError(R);
  end
  else
  begin
   AppTAPIMgr.AsyncList.Add(afBlindTransfer,R,self);
   Result:=R;
  end;
end;

function TTAPICall.CompleteCall:LongWord;
var R:Longint;
begin
  Result:=0;
  R:=LineCompleteCall(Handle,FCompletionID,FCompletionMode,FMessageID);
  if R<-1 then
  begin
    RaiseTAPILineError(R);
  end
  else
  begin
   AppTAPIMgr.AsyncList.Add(afCompleteCall,R,self);
   Result:=R;
  end;
end;

constructor TTAPICall.Create(Owner: TComponent);
begin
  inherited Create(Owner);
  Handle:=DWORD(-1);
end;

{procedure TTAPICall.DeallocateCall;
  var R:Longint;
begin
  if Handle <> 0 then
  begin
    R:=LineDeallocateCall(Handle);
    if DWord(R)>DWord($80000000) then
    begin
      RaiseTAPILineError(R);
    end;
    Handle:=0;
    FAnswered:=False;
    FDroped:=False;
    FAccepted:=False;
    FConnected:=False;
  end;
end;}

destructor TTAPICall.Destroy;
begin
  //AppTAPIMgr.TAPIObjects.Delete(AppTAPIMgr.TAPIObjects.IndexOf(self));
  inherited;
end;

function TTAPICall.Drop: LongWord;
var R:Longint;
begin
  FreeAndNil(FCallStatus);
  Result:=0;
  if Handle <> DWord(-1)then
  begin
    if FDroped=False then
    begin
      R:=LineDrop(Handle,Pchar(FUserUserInfo),Length(FUserUserInfo));
      if DWord(R)>DWord($80000000) then
      begin
        RaiseTAPILineError(R);
      end
      else
      begin
        AppTAPIMgr.AsyncList.Add(afDrop,R,self);
        FDroped:=True;
        FConnected:=False;
        if Assigned(FDigits) then FDigits.Monitoring:=False;
      end;
      Result:=R;
    end
    else Result:=1;
  end;
end;

function TTAPICall.GetCallInfo: TCallInfo;
begin
  if Assigned(FCallInfo)=false then FCallInfo:=TCallInfo.Create(FHandle);
  result:= FCallInfo;
end;

function TTAPICall.GetCompletionMode: TLineCallComplMode;
begin
  Result:=lccmCallBack;
  case FCompletionMode of
    LINECALLCOMPLMODE_CAMPON: Result:= lccmCampon;
    LINECALLCOMPLMODE_CALLBACK:Result:=lccmCallBack;
    LINECALLCOMPLMODE_INTRUDE:Result:=lccmIntrude;
    LINECALLCOMPLMODE_MESSAGE:Result:=lccmMessage;
  end;
end;

function TTAPICall.GetDigits: TTAPIDigits;
begin
  result:=FDigits;
end;

function TTAPICall.GetProcess: TAsyncFunctions;
begin
  Result:=AppTAPIMgr.AsyncList.Operations;
end;

function TTAPICall.GetStatus: TCallStatus;
begin
  if Assigned(FCallStatus)=False then FCallStatus:=TCallStatus.Create(Handle);
  result:=FCallStatus;
end;

function TTAPICall.GetWaveID(WaveDeviceClass: String): DWord;
var varstr:PVARSTRING;
    Dummy:Array[0..3] of Char;
    id:DWord;
    R:LongInt;
begin
  id:=0;
  varstr:=PVARSTRING(AllocMem(SizeOf(TVarString)+1000));
  try
    varstr^.dwTotalSize:=SizeOf(TVARSTRING)+1000;
    R:=LineGetID(0,0,Handle,LINECALLSELECT_Call,varstr,PChar(WaveDeviceClass));
    if R<-1 then
    begin
      RaiseTAPILineError(R);
    end;
    if varstr^.dwStringFormat<>STRINGFORMAT_BINARY then
    begin

    end
    else
    begin
      Dummy:='';
      StrCopy(Dummy,PCHAR(VarStr)+VarStr^.dwStringOffset);
      id:=Byte(Dummy[0])+(256*(Byte(Dummy[1])));
    end;
    Result:=Cardinal(id);
  finally
     FreeMem(VarStr);
  end;
end;

function TTAPICall.GetCommPort: THandle;
var varstr:PVARSTRING;
    Dummy:Array[0..3] of Char;
    id:DWord;
    R:LongInt;
begin
  id:=0;
  varstr:=PVARSTRING(AllocMem(SizeOf(TVarString)+1000));
  try
    varstr^.dwTotalSize:=SizeOf(TVARSTRING)+1000;
    R:=LineGetID(0,0,Handle,LINECALLSELECT_Call,varstr,PChar('comm/datamodem'));
    if R<-1 then
    begin
      RaiseTAPILineError(R);
    end;
    if varstr^.dwStringFormat<>STRINGFORMAT_BINARY then
    begin

    end
    else
    begin
      Dummy:='';
      StrCopy(Dummy,PCHAR(VarStr)+VarStr^.dwStringOffset);
      id:=Byte(Dummy[0])+(256*(Byte(Dummy[1])));
    end;
    Result:=THandle(id);
  finally
     FreeMem(VarStr);
  end;
end;


procedure TTAPICall.Handoff(MediaMode:TLineMediaModes);
var R:Longint;
begin
  R:=LineHandoff(Handle,nil,MediaModesToInt(MediaMode));
  if R<> 0 then RaiseTAPILineError(R);
end;

procedure TTAPICall.lineCallInfoChange(var CallInfo: Dword);
begin
  if Assigned(FCallInfo) then FreeAndNil(FCallInfo);    
  if CallInfo and LINECALLINFOSTATE_OTHER = LINECALLINFOSTATE_OTHER then
    if Assigned(FOnInfoStateOTHER) then FOnInfoStateOTHER(self);
  if CallInfo and LINECALLINFOSTATE_DEVSPECIFIC = LINECALLINFOSTATE_DEVSPECIFIC then
    if Assigned(FOnInfoStateDEVSPECIFIC) then FOnInfoStateDEVSPECIFIC(self);
  if CallInfo and LINECALLINFOSTATE_BEARERMODE = LINECALLINFOSTATE_BEARERMODE then
    if Assigned(FOnInfoStateBEARERMODE) then FOnInfoStateBEARERMODE(self);
  if CallInfo and LINECALLINFOSTATE_RATE = LINECALLINFOSTATE_RATE then
    if Assigned(FOnInfoStateRATE) then FOnInfoStateRATE(self);
  if CallInfo and LINECALLINFOSTATE_MEDIAMODE = LINECALLINFOSTATE_MEDIAMODE then
    if Assigned(FOnInfoStateMEDIAMODE) then FOnInfoStateMEDIAMODE(self);
  if CallInfo and LINECALLINFOSTATE_APPSPECIFIC = LINECALLINFOSTATE_APPSPECIFIC then
    if Assigned(FOnInfoStateAPPSPECIFIC) then FOnInfoStateAPPSPECIFIC(self);
  if CallInfo and LINECALLINFOSTATE_CALLID = LINECALLINFOSTATE_CALLID then
    if Assigned(FOnInfoStateCALLID) then FOnInfoStateCALLID(self);
  if CallInfo and LINECALLINFOSTATE_RELATEDCALLID = LINECALLINFOSTATE_RELATEDCALLID then
    if Assigned(FOnInfoStateRELATEDCALLID) then FOnInfoStateRELATEDCALLID(self);
  if CallInfo and LINECALLINFOSTATE_ORIGIN = LINECALLINFOSTATE_ORIGIN then
    if Assigned(FOnInfoStateORIGIN) then FOnInfoStateORIGIN(self);
  if CallInfo and LINECALLINFOSTATE_REASON = LINECALLINFOSTATE_REASON then
    if Assigned(FOnInfoStateREASON) then FOnInfoStateREASON(self);
  if CallInfo and LINECALLINFOSTATE_COMPLETIONID = LINECALLINFOSTATE_COMPLETIONID then
    if Assigned(FOnInfoStateCOMPLETIONID) then FOnInfoStateCOMPLETIONID(self);
  if CallInfo and LINECALLINFOSTATE_NUMOWNERINCR = LINECALLINFOSTATE_NUMOWNERINCR then
    if Assigned(FOnInfoStateNUMOWNERINCR) then FOnInfoStateNUMOWNERINCR(self);
  if CallInfo and LINECALLINFOSTATE_NUMOWNERDECR = LINECALLINFOSTATE_NUMOWNERDECR then
    if Assigned(FOnInfoStateNUMOWNERDECR) then FOnInfoStateNUMOWNERDECR(self);
  if CallInfo and LINECALLINFOSTATE_NUMMONITORS = LINECALLINFOSTATE_NUMMONITORS then
    if Assigned(FOnInfoStateNUMMONITORS) then FOnInfoStateNUMMONITORS(self);
  if CallInfo and LINECALLINFOSTATE_TRUNK = LINECALLINFOSTATE_TRUNK then
    if Assigned(FOnInfoStateTRUNK) then FOnInfoStateTRUNK(self);
  if CallInfo and LINECALLINFOSTATE_CALLERID = LINECALLINFOSTATE_CALLERID then
    if Assigned(FOnInfoStateCALLERID) then FOnInfoStateCALLERID(self);
  if CallInfo and LINECALLINFOSTATE_CALLEDID = LINECALLINFOSTATE_CALLEDID then
    if Assigned(FOnInfoStateCALLEDID) then FOnInfoStateCALLEDID(self);
  if CallInfo and LINECALLINFOSTATE_CONNECTEDID = LINECALLINFOSTATE_CONNECTEDID then
    if Assigned(FOnInfoStateCONNECTEDID) then FOnInfoStateCONNECTEDID(self);
  if CallInfo and LINECALLINFOSTATE_REDIRECTIONID = LINECALLINFOSTATE_REDIRECTIONID then
    if Assigned(FOnInfoStateREDIRECTIONID) then FOnInfoStateREDIRECTIONID(self);
  if CallInfo and LINECALLINFOSTATE_REDIRECTINGID = LINECALLINFOSTATE_REDIRECTINGID then
    if Assigned(FOnInfoStateREDIRECTINGID) then FOnInfoStateREDIRECTINGID(self);
  if CallInfo and LINECALLINFOSTATE_DISPLAY = LINECALLINFOSTATE_DISPLAY then
    if Assigned(FOnInfoStateDISPLAY) then FOnInfoStateDISPLAY(self);
  if CallInfo and LINECALLINFOSTATE_USERUSERINFO = LINECALLINFOSTATE_USERUSERINFO then
    if Assigned(FOnInfoStateUSERUSERINFO) then FOnInfoStateUSERUSERINFO(self);
  if CallInfo and LINECALLINFOSTATE_HIGHLEVELCOMP = LINECALLINFOSTATE_HIGHLEVELCOMP then
    if Assigned(FOnInfoStateHIGHLEVELCOMP) then FOnInfoStateHIGHLEVELCOMP(self);
  if CallInfo and LINECALLINFOSTATE_LOWLEVELCOMP = LINECALLINFOSTATE_LOWLEVELCOMP then
    if Assigned(FOnInfoStateLOWLEVELCOMP) then FOnInfoStateLOWLEVELCOMP(self);
  if CallInfo and LINECALLINFOSTATE_CHARGINGINFO = LINECALLINFOSTATE_CHARGINGINFO then
    if Assigned(FOnInfoStateCHARGINGINFO) then FOnInfoStateCHARGINGINFO(self);
  if CallInfo and LINECALLINFOSTATE_TERMINAL = LINECALLINFOSTATE_TERMINAL then
    if Assigned(FOnInfoStateTERMINAL) then FOnInfoStateTERMINAL(self);
  if CallInfo and LINECALLINFOSTATE_DIALPARAMS = LINECALLINFOSTATE_DIALPARAMS then
    if Assigned(FOnInfoStateDIALPARAMS) then FOnInfoStateDIALPARAMS(self);
  if CallInfo and LINECALLINFOSTATE_MONITORMODES = LINECALLINFOSTATE_MONITORMODES then
    if Assigned(FOnInfoStateMONITORMODES) then FOnInfoStateMONITORMODES(self)

 { case CallInfo of
    LINECALLINFOSTATE_OTHER:if Assigned(FOnInfoStateOTHER) then FOnInfoStateOTHER(self);
    LINECALLINFOSTATE_DEVSPECIFIC:if Assigned(FOnInfoStateDEVSPECIFIC) then FOnInfoStateDEVSPECIFIC(self);
    LINECALLINFOSTATE_BEARERMODE:if Assigned(FOnInfoStateBEARERMODE) then FOnInfoStateBEARERMODE(self);
    LINECALLINFOSTATE_RATE:if Assigned(FOnInfoStateRATE) then FOnInfoStateRATE(self);
    LINECALLINFOSTATE_MEDIAMODE:if Assigned(FOnInfoStateMEDIAMODE) then FOnInfoStateMEDIAMODE(self);
    LINECALLINFOSTATE_APPSPECIFIC:if Assigned(FOnInfoStateAPPSPECIFIC) then FOnInfoStateAPPSPECIFIC(self);
    LINECALLINFOSTATE_CALLID:if Assigned(FOnInfoStateCALLID) then FOnInfoStateCALLID(self);
    LINECALLINFOSTATE_RELATEDCALLID:if Assigned(FOnInfoStateRELATEDCALLID) then FOnInfoStateRELATEDCALLID(self);
    LINECALLINFOSTATE_ORIGIN:if Assigned(FOnInfoStateORIGIN) then FOnInfoStateORIGIN(self);
    LINECALLINFOSTATE_REASON:if Assigned(FOnInfoStateREASON) then FOnInfoStateREASON(self);
    LINECALLINFOSTATE_COMPLETIONID:if Assigned(FOnInfoStateCOMPLETIONID) then FOnInfoStateCOMPLETIONID(self);
    LINECALLINFOSTATE_NUMOWNERINCR:if Assigned(FOnInfoStateNUMOWNERINCR) then FOnInfoStateNUMOWNERINCR(self);
    LINECALLINFOSTATE_NUMOWNERDECR:if Assigned(FOnInfoStateNUMOWNERDECR) then FOnInfoStateNUMOWNERDECR(self);
    LINECALLINFOSTATE_NUMMONITORS:if Assigned(FOnInfoStateNUMMONITORS) then FOnInfoStateNUMMONITORS(self);
    LINECALLINFOSTATE_TRUNK:if Assigned(FOnInfoStateTRUNK) then FOnInfoStateTRUNK(self);
    LINECALLINFOSTATE_CALLERID:if Assigned(FOnInfoStateCALLERID) then FOnInfoStateCALLERID(self);
    LINECALLINFOSTATE_CALLEDID:if Assigned(FOnInfoStateCALLEDID) then FOnInfoStateCALLEDID(self);
    LINECALLINFOSTATE_CONNECTEDID:if Assigned(FOnInfoStateCONNECTEDID) then FOnInfoStateCONNECTEDID(self);
    LINECALLINFOSTATE_REDIRECTIONID:if Assigned(FOnInfoStateREDIRECTIONID) then FOnInfoStateREDIRECTIONID(self);
    LINECALLINFOSTATE_REDIRECTINGID:if Assigned(FOnInfoStateREDIRECTINGID) then FOnInfoStateREDIRECTINGID(self);
    LINECALLINFOSTATE_DISPLAY:if Assigned(FOnInfoStateDISPLAY) then FOnInfoStateDISPLAY(self);
    LINECALLINFOSTATE_USERUSERINFO:if Assigned(FOnInfoStateUSERUSERINFO) then FOnInfoStateUSERUSERINFO(self);
    LINECALLINFOSTATE_HIGHLEVELCOMP:if Assigned(FOnInfoStateHIGHLEVELCOMP) then FOnInfoStateHIGHLEVELCOMP(self);
    LINECALLINFOSTATE_LOWLEVELCOMP:if Assigned(FOnInfoStateLOWLEVELCOMP) then FOnInfoStateLOWLEVELCOMP(self);
    LINECALLINFOSTATE_CHARGINGINFO:if Assigned(FOnInfoStateCHARGINGINFO) then FOnInfoStateCHARGINGINFO(self);
    LINECALLINFOSTATE_TERMINAL:if Assigned(FOnInfoStateTERMINAL) then FOnInfoStateTERMINAL(self);
    LINECALLINFOSTATE_DIALPARAMS:if Assigned(FOnInfoStateDIALPARAMS) then FOnInfoStateDIALPARAMS(self);
    LINECALLINFOSTATE_MONITORMODES:if Assigned(FOnInfoStateMONITORMODES) then FOnInfoStateMONITORMODES(self);
  end;  }

end;

procedure TTAPICall.DevSpecificChange(dwParam1,dwParam2,dwParam3:DWord);
begin
  if Assigned(FOnDevSpecific) then FOnDevSpecific(self,dwParam1,dwParam2,dwParam3);
end;

procedure TTAPICall.DevSpecificFeatureChange(dwParam1,dwParam2,dwParam3:DWord);
begin
  if Assigned(FOnDevSpecificFeature) then FOnDevSpecificFeature(self,dwParam1,dwParam2,dwParam3);
end;

procedure TTAPICall.LineCallState(var State, Mode, Rights: DWord);
var CRight:TLineCallPrivilege;
var MS:String;
begin
  if Assigned(FCallStatus) then FreeAndNil(FCallStatus);
  if Assigned(FCallInfo) then FreeAndNil(FCallInfo);  
  CRight:=IntToCallPrivilege(Rights);
  {$IFDEF DEBUG}
  case State of
    LINECALLSTATE_IDLE: MS:='csIDLE';
    LINECALLSTATE_OFFERING:MS:='csOffering';
    LINECALLSTATE_ACCEPTED:MS:='csAccepted';
    LINECALLSTATE_DIALTONE:MS:='csDialTone';
    LINECALLSTATE_DIALING: MS:='csDIALING';
    LINECALLSTATE_RINGBACK:MS:='csRINGBACK';
    LINECALLSTATE_BUSY:MS:='csBUSY';
    LINECALLSTATE_SPECIALINFO:MS:='csSPECIALINFO';
    LINECALLSTATE_CONNECTED:MS:='csCONNECTED';
    LINECALLSTATE_PROCEEDING:MS:='csPROCEEDING';
    LINECALLSTATE_ONHOLD: MS:='csONHOLD';
    LINECALLSTATE_CONFERENCED:MS:='csCONFERENCED';
    LINECALLSTATE_ONHOLDPENDCONF:MS:='csONHOLDPENDCONF';
    LINECALLSTATE_ONHOLDPENDTRANSFER:MS:='csONHOLDPENDTRANSFer';
    LINECALLSTATE_DISCONNECTED:MS:='csDISCONNECTED';
    LINECALLSTATE_UNKNOWN :MS:='csUNKNOWN';
    else MS:='cs-?? ****************************************************';
  end;
  OutputDebugString(PChar(MS+'('+IntToStr(Mode)+')--'+IntToStr(Rights) ));

  {$ENDIF}
  case State of
    LINECALLSTATE_IDLE:if Assigned(FOnStateIdle) then FOnStateIDLE(self,CRight);
    LINECALLSTATE_OFFERING:
       begin
         //Handle:=Device;
         if Assigned(FOnStateOffering) then FOnStateOffering(self,IntToOfferingModes(Mode),CRight);
       end;
    LINECALLSTATE_ACCEPTED:if Assigned(FOnStateAccepted) then FOnStateAccepted(self,CRight);
    LINECALLSTATE_DIALTONE:if Assigned(FOnStateDialTone) then FOnStateDialTone(self,IntToDialToneMode(Mode),CRight);
    LINECALLSTATE_DIALING: if Assigned(FOnSTATEDIALING) then FOnSTATEDIALING(self,CRight);
    LINECALLSTATE_RINGBACK:if Assigned(FOnSTATERINGBACK) then FOnSTATERINGBACK(self,CRight);
    LINECALLSTATE_BUSY:if Assigned(FOnSTATEBUSY) then FOnSTATEBUSY(self,IntToBusyMode(Mode),CRight);
    LINECALLSTATE_SPECIALINFO:if Assigned(FOnSTATESPECIALINFO) then FOnSTATESPECIALINFO(self,IntToSpecialInfo(Mode),CRight);
    LINECALLSTATE_CONNECTED:
      begin
        {if FConnected=False then
        begin}
          {$IFDEF DEBUG}
          OutputDebugString(PChar('ConnectMode=('+IntToStr(Mode)+')'));
          {$ENDIF}
          if Assigned(FOnSTATECONNECTED) then FOnSTATECONNECTED(self,IntToConnectedModes(Mode),CRight);
          {FConnected:=True;
        end;}
      end;
    LINECALLSTATE_PROCEEDING:if Assigned(FOnSTATEPROCEEDING) then FOnSTATEPROCEEDING(self,CRight);
    LINECALLSTATE_ONHOLD: if Assigned(FOnSTATEONHOLD) then FOnSTATEONHOLD(self,CRight);
    LINECALLSTATE_CONFERENCED:if Assigned(FOnSTATECONFERENCED) then FOnSTATECONFERENCED(self,CRight);
    LINECALLSTATE_ONHOLDPENDCONF: if Assigned(FOnSTATEONHOLDPENDCONF) then FOnSTATEONHOLDPENDCONF(self,CRight);
    LINECALLSTATE_ONHOLDPENDTRANSFER:if Assigned(FOnSTATEONHOLDPENDTRANSF) then FOnSTATEONHOLDPENDTRANSF(self,CRight);
    LINECALLSTATE_DISCONNECTED:if Assigned(FOnSTATEDISCONNECTED) then FOnSTATEDISCONNECTED(self,IntToDisconnectMode(Mode),CRight);
    LINECALLSTATE_UNKNOWN :if Assigned(FOnSTATEUNKNOWN) then FOnSTATEUNKNOWN(self,CRight);
  end;

end;

{procedure TTAPICall.LineGatherDigits(var GatherTermination,TickCount: DWord);
var GTerm:TLineGatherTerm;
begin
  GTerm:=IntToGatherTerm(GatherTermination);
  if Assigned(FOnLineGatherDigits) then FOnLineGatherDigits(self,GTerm,TickCount);
end;  }

procedure TTAPICall.MonitorMediaChange(MediaMode: TLineMediaModes;
  TickCount: DWord);
begin
  if Assigned(FOnMonitorMedia) then FOnMonitorMedia(self,MediaMode,TickCount);
end;

procedure TTAPICall.MonitorMedia(MediaMode: TLineMediaModes);
var R:Longint;
begin
  R:=LineMonitorMedia(Handle,MediaModesToInt(MediaMode));
  if R<> 0 then RaiseTAPILineError(R);
end;

{procedure TTAPICall.Reply(AsyncFunc: TAsyncFunc; Error: DWord);
begin
  if Assigned(FOnReply) then FOnReply(self,AsyncFunc,Error);
end;   }

procedure TTAPICall.ChangePrivilege(NewPriv: TLineCallPrivilege);
var R:Longint;
begin
  R:=LineSetCallPrivilege(Handle,CallPrivilegeToInt(NewPriv));
  if R<> 0 then RaiseTAPILineError(R);
end;

procedure TTAPICall.MakeCall(LineHandle:HLine;DialableAddress:String;FCountryCode:DWord);
var R:Longint;
    CParams:PLineCallParams;
begin
  if Assigned(FCallParams) then
  begin
    CParams:=AllocMem(SizeOf(TLineCallParams)+1000);
    CParams^.dwTotalSize:=SizeOf(TLineCallParams)+1000;
    FCallParams.GetParamStruct(CParams);
  end;

  try
    R:=LineMakeCall(LineHandle,@FHandle,PChar(DialableAddress),FCountryCode,CParams);
  if R<-1 then
  begin
    RaiseTAPILineError(R);
  end
  else
  begin
    AppTAPIMgr.AsyncList.Add(afMakeCall,R,self);
  end;
  finally
    FreeMem(CParams);
  end;
end;

procedure TTAPICall.SetCompletionMode(const Value: TLineCallComplMode);
begin
  if Value=lccmCampon then FCompletionMode:=LINECALLCOMPLMODE_CAMPON;
  if Value=lccmCallBack then FCompletionMode:=LINECALLCOMPLMODE_CALLBACK;
  if Value=lccmIntrude  then FCompletionMode:=LINECALLCOMPLMODE_INTRUDE;
  if Value=lccmMessage  then FCompletionMode:=LINECALLCOMPLMODE_MESSAGE;
end;

procedure TTAPICall.SetDigits(const Value: TTAPIDigits);
begin
  if FDigits<>Value then
  begin
    FDigits:=Value;
    FDigits.CallHandle:=Handle;
  end;
end;

procedure TTAPICall.SetHandle(const Value: THandle);
begin
  if (Value <> DWord(-1)) then
    if Assigned(FDigits) then FDigits.CallHandle:=Value;
  inherited SetHandle(Value);
end;

procedure TTAPICall.SetMediaMode(MediaMode: TLineMediaModes);
var R:Longint;
begin
  R:=LineSetMediaMode(Handle,MediaModesToInt(MediaMode));
  if R<> 0 then RaiseTAPILineError(R);
end;

function TTAPICall.GetTones: TTAPITones;
begin
  result:=FTones;
end;

procedure TTAPICall.SetTones(const Value: TTAPITones);
begin
  if FTones<>Value then
  begin
    FTones:=Value;
    FTones.CallHandle:=Handle;
  end;
end;

procedure TTAPICall.Notification(AComponent: TComponent;
      Operation: TOperation);
begin
  inherited Notification(AComponent,Operation);
  if (Operation = opRemove) and (AComponent=FTones) then
  begin
    FTones:=nil;
  end;
  if (Operation = opRemove) and (AComponent=FDigits) then
  begin
    FDigits:=nil;
  end;
  if (Operation = opRemove) and (AComponent=FCallParams) then
  begin
    FCallParams:=nil;
  end;
end;

procedure TTAPICall.PerformMsg(Msg: TCMTAPI);
begin
  inherited;
  with Msg.TAPIRec^ do
  begin
    if dwMsg = LINE_REPLY then Reply(Msg.AsyncFunc^,dwParam2);
    if Handle=hDevice then
    begin
      case dwMsg of
        LINE_CALLSTATE:LineCallState(dwParam1,dwParam2,dwParam3);
        LINE_CALLINFO:LineCallInfoChange(dwParam1);
        LINE_DEVSPECIFIC:DevSpecificChange(dwParam1,dwParam2,dwParam3);
        LINE_DEVSPECIFICFEATURE:DevSpecificFeatureChange(dwParam1,dwParam2,dwParam3);
        LINE_MONITORMEDIA:MonitorMediaChange(IntToMediaModes(dwParam1),dwParam3);
      end;
    end;
  end;
end;

{ TCallInfo }

constructor TCallInfo.Create(AHCall:hCall);
var R:Longint;
    FLineCallInfo:PLineCallInfo;
var Dummy:Array of char;
    I:Integer;
begin
  inherited Create;
  FLineCallInfo:=AllocMem(SizeOf(TLineCallInfo)+1000);
  FLineCallInfo.dwTotalSize:=SizeOf(TLineCallInfo)+1000;
  R:=LineGetCallInfo(AHCall,FLineCallInfo);
  if R<>0 then RaiseTAPILineError(R);
  FHLine:=FLineCallInfo^.hLine;
  FLineDeviceID:=FLineCallInfo^.dwLineDeviceID;
  FAddressID:=FLineCallInfo^.dwAddressID;
  FBearerMode:=IntToBearerMode(FLineCallInfo^.dwBearerMode);
  FRate:=FLineCallInfo^.dwRate;
  FMediaMode:=IntToMediaModes(FLineCallInfo^.dwMediaMode);
  FAppSpecific:=FLineCallInfo^.dwAppSpecific;
  FCallID:=FLineCallInfo^.dwCallID;
  FRelatedCallID:=FLineCallInfo^.dwRelatedCallID;
  FCallParamFlags:=IntToCallParamFlags(FLineCallInfo^.dwCallParamFlags);
  FCallStates:=IntToCallStates(FLineCallInfo^.dwCallStates);
  FMonitorDigitModes:=IntToDigitModes(FLineCallInfo^.dwMonitorDigitModes);
  FMonitorMediaModes:=IntToMediaModes(FLineCallInfo^.dwMonitorMediaModes);
  FDialParams:=FLineCallInfo^.DialParams;
  FOrigin:=IntToCallOrigin(FLineCallInfo^.dwOrigin);
  FReason:=IntToCallReason(FLineCallInfo^.dwReason);
  FCompletionID:=FLineCallInfo^.dwCompletionID;
  FNumOwners:=FLineCallInfo^.dwNumOwners;
  FNumMonitors:=FLineCallInfo^.dwNumMonitors;
  FCountryCode:=FLineCallInfo^.dwCountryCode;
  FTrunk:=FLineCallInfo^.dwTrunk;
  FCallerIDFlags:=IntToCallPartyID(FLineCallInfo^.dwCallerIDFlags);

  if FLineCallInfo^.dwCallerIDSize >0 then
  begin
    SetLength(Dummy,FLineCallInfo^.dwCallerIDSize+1);
    StrCopy(Pchar(Dummy),PChar(FLineCallInfo)+ FLineCallInfo^.dwCallerIDOffset);
    FCallerID:=PChar(Dummy);
    Dummy:=nil;
  end
  else
    FCallerID:='';
  if FLineCallInfo^.dwCallerIDNameSize >0 then
  begin
    SetLength(Dummy,FLineCallInfo^.dwCallerIDNameSize+1);
    StrCopy(PChar(Dummy),PChar(FLineCallInfo)+ FLineCallInfo^.dwCallerIDNameOffset);
    FCallerIDName:=PChar(Dummy);
    Dummy:=nil;
  end
  else
    FCallerIDName:='';
  FCalledIDFlags:=IntToCallPartyID(FLineCallInfo^.dwCalledIDFlags);
  if FLineCallInfo^.dwCalledIDSize >0 then
  begin
    SetLength(Dummy,FLineCallInfo^.dwCalledIDSize+1);
    StrCopy(Pchar(Dummy),PChar(FLineCallInfo)+ FLineCallInfo^.dwCalledIDOffset);
    FCalledID:=PChar(Dummy);
    Dummy:=nil;
  end
  else
    FCalledID:='';
  if FLineCallInfo^.dwCalledIDNameSize >0 then
  begin
    SetLength(Dummy,FLineCallInfo^.dwCalledIDNameSize+1);
    StrCopy(PChar(Dummy),PChar(FLineCallInfo)+ FLineCallInfo^.dwCalledIDNameOffset);
    FCalledIDName:=PChar(Dummy);
  end
  else
    FCalledIDName:='';
  FConnectedIDFlags:=IntToCallPartyID(FLineCallInfo^.dwConnectedIDFlags);
  if FLineCallInfo^.dwConnectedIDSize >0 then
  begin
    SetLength(Dummy,FLineCallInfo^.dwConnectedIDSize+1);
    StrCopy(Pchar(Dummy),PChar(FLineCallInfo)+ FLineCallInfo^.dwConnectedIDOffset);
    FConnectedID:=PChar(Dummy);
    Dummy:=nil;
  end
  else
    FConnectedID:='';
  if FLineCallInfo^.dwConnectedIDNameSize >0 then
  begin
    SetLength(Dummy,FLineCallInfo^.dwConnectedIDNameSize+1);
    StrCopy(PChar(Dummy),PChar(FLineCallInfo)+ FLineCallInfo^.dwConnectedIDNameOffset);
    FConnectedIDName:=Pchar(Dummy);
    Dummy:=nil;
  end
  else
    FConnectedIDName:='';
  FRedirectionIDFlags:=IntToCallPartyID(FLineCallInfo^.dwRedirectionIDFlags);
  if FLineCallInfo^.dwRedirectionIDSize >0 then
  begin
    SetLength(Dummy,FLineCallInfo^.dwRedirectionIDSize+1);
    StrCopy(Pchar(Dummy),PChar(FLineCallInfo)+ FLineCallInfo^.dwRedirectionIDOffset);
    FRedirectionID:=PChar(Dummy);
    Dummy:=nil;
  end
  else
   FRedirectionID:='';
  if FLineCallInfo^.dwRedirectionIDNameSize >0 then
  begin
    SetLength(Dummy,FLineCallInfo^.dwRedirectionIDNameSize+1);
    StrCopy(PChar(Dummy),PChar(FLineCallInfo)+ FLineCallInfo^.dwRedirectionIDNameOffset);
    FRedirectionIDName:=PChar(Dummy);
    Dummy:=nil;
  end
  else
    FRedirectionIDName:='';
  FRedirectingIDFlags:=IntToCallPartyID(FLineCallInfo^.dwRedirectingIDFlags);
  if FLineCallInfo^.dwRedirectingIDSize >0 then
  begin
    SetLength(Dummy,FLineCallInfo^.dwRedirectingIDSize+1);
    StrCopy(Pchar(Dummy),PChar(FLineCallInfo)+ FLineCallInfo^.dwRedirectingIDOffset);
    FRedirectingID:=PChar(Dummy);
    Dummy:=nil;
  end
  else
    FRedirectingID:='';
  if FLineCallInfo^.dwRedirectingIDNameSize >0 then
  begin
    SetLength(Dummy,FLineCallInfo^.dwRedirectingIDNameSize+1);
    StrCopy(PChar(Dummy),PChar(FLineCallInfo)+ FLineCallInfo^.dwRedirectingIDNameOffset);
    FRedirectingIDName:=PChar(Dummy);
    Dummy:=nil;
  end
  else
   FRedirectingIDName:='';
  if FLineCallInfo^.dwAppNameSize >0 then
  begin
    SetLength(Dummy,FLineCallInfo^.dwAppNameSize+1);
    StrCopy(PChar(Dummy),PChar(FLineCallInfo)+ FLineCallInfo^.dwAppNameOffset);
    FAppName:=PChar(Dummy);
  end
  else
    FAppName:='';
  if FLineCallInfo^.dwDisplayableAddressSize >0 then
  begin
    SetLength(Dummy,FLineCallInfo^.dwDisplayableAddressSize+1);
    StrCopy(PChar(Dummy),PChar(FLineCallInfo)+ FLineCallInfo^.dwDisplayableAddressOffset);
    FDisplayableAddress:=PChar(Dummy);
  end
  else FDisplayableAddress:='';

  //StrCopy(PChar(FCalledParty),PChar(FLineCallInfo)+ FLineCallInfo^.dwCalledPartyOffset);
  if FLineCallInfo^.dwCommentSize >0 then
  begin
    SetLength(Dummy,FLineCallInfo^.dwCommentSize+1);
    StrCopy(PChar(Dummy),PChar(FLineCallInfo)+ FLineCallInfo^.dwCommentOffset);
    FComment:=PChar(Dummy);
  end
  else
    FComment:='';
  //StrCopy(PChar(FHighLevelComp ),PChar(FLineCallInfo)+ FLineCallInfo^.dwHighLevelCompOffset);
  //StrCopy(PChar(FLowLevelComp),PChar(FLineCallInfo)+ FLineCallInfo^.dwLowLevelCompOffset);
  //StrCopy(PChar(FChargingInfo),PChar(FLineCallInfo)+ FLineCallInfo^.dwChargingInfoOffset);

  if FLineCallInfo^.dwTerminalModesSize >0 then
  begin
    // +++++++++++++++++++++++++++ BUGFIX 23.Aug. 2003  +++++++++++++++++++++++
    SetLength(FTerminalModes,SizeOf(TLineTermModes)*FLineCallInfo^.dwTerminalModesSize div 4);
    SetLength(Dummy,4);
    for i:=0 to (FLineCallInfo^.dwTerminalModesSize div 4)-1 do
    begin
      StrCopy(Pchar(Dummy),PChar(FLineCallInfo)+ FLineCallInfo^.dwTerminalModesOffset+(i*4));
      FTerminalModes[i]:=IntToTermModes(DWord(Dummy));
    end;
  end;
  //StrCopy(PChar(FDevSpecific),PChar(FLineCallInfo)+ FLineCallInfo^.dwDevSpecificOffset);
  {$IFDEF TAPI20}
  FCallTreatment:=FLineCallInfo^.dwCallTreatment;
    {FCallDataSize,
    FCallDataOffset,
    FSendingFlowspecSize,
    FSendingFlowspecOffset,
    FReceivingFlowspecSize,
    FReceivingFlowspecOffset: DWORD;  }
  {$ENDIF}
  FreeMem(FLineCallInfo);
end;

function TCallInfo.GetTerminalModes(Index:Integer):TLineTermModes;
begin
  Result:=FTerminalModes[Index];
end;

destructor TCallInfo.Destroy;
begin
  inherited;
end;


{ TTAPICallObject }

procedure TTAPICallObject.Reply(AsyncFunc: TAsyncFunc; Error: DWord);
begin
  if Assigned(FOnReply) then FOnReply(self,AsyncFunc,Error);
end;

procedure TTAPICallObject.SetHandle(const Value: THandle);
begin
  FHandle:=Value;
end;

procedure TTAPICallObject.DeallocateCall;
  var R:Longint;
begin
  if FHandle <> DWord(-1) then
  begin
    R:=LineDeallocateCall(Handle);
    if DWord(R)>DWord($80000000) then
    begin
      RaiseTAPILineError(R);
    end;
    Handle:=DWord(-1);
    FAnswered:=False;
    FDroped:=False;
    FAccepted:=False;
    FConnected:=False;
    FForwarded:=False;
  end;
end;

function TTAPICallObject.SetCallParams(BearerMode: TLineBearerMode;MinRate,MaxRate:DWord):LongWord;
var R:Longint;
begin
  Result:=0;
  if Handle <> DWord(-1) then
  begin
    R:=lineSetCallParams(Handle,BearerModeToInt(BearerMode),MinRate,MaxRate,nil{FDialParams});
    if R <-1 then
      RaiseTAPILineError(R)
    else
    begin
      AppTAPIMgr.AsyncList.Add(afSetCallParams,R,self);
      Result:=R;
    end;
  end;
end;

function TTAPICallObject.GetIsAnswered:Boolean;
begin
  Result:=False;
  if FAnswered then Result:=True;
end;

{ TDialParams }

constructor TDialParams.Create;
begin
  inherited Create;
end;
{$ENDIF}
{$ENDIF}

end.

