{******************************************************************************}
{*        Copyright 1999-2001 by J.Friebel all rights reserved.               *}
{*        Autor           :  Jörg Friebel                                     *}
{*        Compiler        :  Delphi 4 / 5                                     *}
{*        System          :  Windows NT / 2000                                *}
{*        Projekt         :  TAPI Komponenten (TAPI Version 1.4 bis 3.0)      *}
{*        Last Update     :  17.02.2001                                       *}
{*        Version         :  1.0                                              *}
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
unit TAPIDevices;

interface

{$IFDEF WINDOWS}
{$IFDEF CPU32}
{$IFDEF FPC}
{$MODE DELPHI}
{$ENDIF}

uses Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
     TAPI,TAPISystem,TAPIErr,TAPIServices,TAPITrans,TAPITon,TAPICall,
     DevConf;


{$INCLUDE TAPI.INC}

type
  TStringFormat=(sfASCII,sfDBCS,sfUNICODE,sfBINARY);

  TLineDevCapFlags = set of (ldcfCrossAddrConf,ldcfHighLevComp,ldcfLowLevComp,
    ldcfMediaControl,ldcfMultipleAddr,ldcfCloseDrop,ldcfDialBilling,ldcfDialQuiet,
    ldcfDialDialtone{$IFDEF TAPI30},ldcfMSP,ldcfCallHub,ldcfCallHubTracking,
    ldcfPrivateObjects{$ENDIF});

  TLineFeature = (lfDevSpecific,lfDevSpecificFeat,lfForward,lfMakeCall,
    lfSetMediaControl,lfSetTerminal{$IFDEF TAPI20},lfSetDevStatus,
    lfForwardFWD,lfForwardDND{$ENDIF});
  TLineFeatures = set of TLineFeature;

  TLineRoamMode = (lrmUnknown,lrmUnavail,lrmHome,lrmRoamA,lrmRoamB);

  TLineTermDev = (ltdHeadSet,ltdPhone,ltdSpeaker,ltdNoDef);

  TLineTermSharing = (ltsPrivate,ltsSharedExcl,ltsSharedConf,ltsNoDef);

  TTermCapsItem =class(TObject)
  private
    FTermDev:TLineTermDev;
    FTermMode:TLineTermMode;
    FTermSharing:TLineTermSharing;
    procedure GetCaps(ATermCaps:TLineTermCaps);
  public
    constructor Create(ATermDev,ATermMode,ATermSharing:DWord);
    destructor Destroy;override;
    property TermDev:TLineTermDev read FTermDev;
    property TermMode:TLineTermMode read FTermMode;
    property TermSharing :TLineTermSharing read FTermSharing;
  end;
  TTermCaps=class(TList);

  TLineDevStatusFlags = set of (ldsfConnected,ldsfMsgWait,ldsfInService,
    ldsfLocked);

  TLineAnswerMode = (lamUnkown,lamDrop,lamHold,lamNone);
  TLineAnswerModes = set of TLineAnswerMode;

  TLineDevState = (dsOther,dsRinging,dsConnected,dsDisconnected,
    dsMsgWaitOn,dsMsgWaitOff,dsInService,dsOutOfService,dsMaintenance,dsOpen,
    dsClose,dsNumCalls,dsNumCompletions,dsTerminals,dsRoamMode,dsBattery,
    dsSignal,dsDevSpecific,dsReInit,dsLock,dsReMoved,dsCapsChange,
    dsConfigChange,dsTranslateChange,dsComplCancel);
  TLineDevStates = set of TLineDevState;

  TDevStateRingingEvent=procedure(Sender:TObject;RingModeIndex,RingCounter:DWord)of Object;
  {$IFDEF TAPI30}
  TLineCallHubTracking = (lchtNone,lchtProviderLevel,lchtAllCalls);
  {$ENDIF}

  {$IFDEF TAPI20}
  TAppInfoItem=class (TCollectionItem)
  private
    FAddressID: DWord;
    FFriendlyName: String;
    FUserName: String;
    FMachineName: String;
    FModuleFilename: String;
    FMediaModes: TLineMediaModes;
    procedure SetInfo(AInfo:PLineAppInfo;Struct :PChar);
  public
    constructor Create(Collection: TCollection);override;
    destructor Destroy;override;
    property MachineName:String read FMachineName;
    property UserName:String read FUserName;
    property ModuleFilename:String read FModuleFilename;
    property FriendlyName: String read FFriendlyName;
    property MediaModes:TLineMediaModes read FMediaModes;
    property AddressID:DWord read FAddressID;
  end;

  TAppInfos = class(TCollection)
  private
    function GetItem(Index:Integer): TAppInfoItem;
    procedure SetItem(Index: Integer; const Value: TAppInfoItem);
  public
    constructor Create(ItemClass: TCollectionItemClass);
    destructor Destroy;override;
    function Add:TAppInfoItem;
    property Items[Index:Integer]:TAppInfoItem read GetItem write SetItem;
  end;
  {$ENDIF}

  TDeviceStatus=class(TPersistent)
  private
    FNumOpens:DWord;
    FOpenMediaModes:TLineMediaModes;
    FNumActiveCalls: DWord;
    FNumOnHoldCalls: DWord;
    FNumOnHoldPendCalls:DWord;
    FLineFeatures:TLineFeatures;
    FNumCallCompletions:DWord;
    FRingMode:DWord;
    FSignalLevel:DWord;
    FBatteryLevel:DWord;
    FRoamMode:TLineRoamMode;
    FDevStatusFlags:TLineDevStatusFlags;
    FTerminalModes:Array of TLineTermMode;
    FDevSpecific: String;
    FDevSpecificSize:DWord;
 {$IFDEF TAPI20}
    FAvailableMediaModes:TLineMediaModes;
    FAppInfo:TAppInfos;
 {$ENDIF}
    function GetTerminalModes(Index:Integer): TLineTermMode;

  protected
    procedure SetStatus(DStatus:PLineDevStatus);
  public
    constructor Create(ALine:hLine);virtual;
    destructor Destroy;override;
    //procedure GetDevSpecific(var AArray: Array of byte); overload;
    //procedure GetDevSpecific(var AStr: String ; UniModemFlag:Boolean); overload;
    property DevSpecific:String read FDevSpecific;
    property TerminalModes[Index:Integer]:TLineTermMode read GetTerminalModes;
    property NumOpens:DWord read FNumOpens;
    property OpenMediaModes:TLineMediaModes read FOpenMediaModes;
    property NumActiveCalls:DWord read FNumActiveCalls;
    property NumOnHoldCalls:DWord read FNumOnHoldCalls;
    property NumOnHoldPendCalls:DWord read FNumOnHoldPendCalls;
    property LineFeatures:TLineFeatures read FLineFeatures;
    property NumCallCompletions:DWord read FNumCallCompletions;
    property RingMode:DWord read FRingMode;
    property SignalLevel:DWord read FSignalLevel;
    property BatteryLevel:DWord read FBatteryLevel;
    property RoamMode:TLineRoamMode read FRoamMode;
    property DevStatusFlags:TLineDevStatusFlags read FDevStatusFlags;
  {$IFDEF TAPI20}
    property AvailableMediaModes:TLineMediaModes read FAvailableMediaModes;
    property AppInfo:TAppInfos read FAppInfo;
  {$ENDIF}
  end;


  PLineDeviceCaps=^TLineDeviceCaps;
  TLineDeviceCaps=class(TPersistent)
  private
    FDeviceID:DWord;
    FProviderInfo:String;
    FSwitchInfo:String;
    FPermanentLineID:DWORD;
    FLineName:String;
    FStringFormat:TStringFormat;
    FAddressModes:TLineAddressModes;
    FBearerModes:TLineBearerModes;
    FMaxRate:DWord;
    FMediaModes:TLineMediaModes;
    FNumAddresses: DWord;
    FGenerateToneModes:TLineToneModes;
    FGenerateToneMaxNumFreq: Dword;
    FGenerateDigitModes: TLineDigitModes;
    FMonitorToneMaxNumEntries: DWord;
    FMonitorToneMaxNumFreq: DWord;
    FMonitorDigitModes:TLineDigitModes;
    FMedCtlToneMaxListSize: Dword;
    FMedCtlDigitMaxListSize: DWord;
    FGaterDigitsMaxTimeout: DWord;
    FMedCtlCallStateMaxListSize: DWord;
    FGaterDigitsMinTimeout: DWord;
    FMedCtlMediaMaxListSize: DWord;
    FDevCapFlags:TLineDevCapFlags;
    FMaxNumActiveCalls:DWord;
    FAnswerMode:TLineAnswerMode;
    FRingModes:DWord;
    FLineStates:TLineDevStates;
    FUUIAcceptSize:DWord;
    FUUIAnswerSize:DWord;
    FUUIMakeCallSize:DWord;
    FUUIDropSize:DWord;
    FUUISendUserUserInfoSize:DWord;
    FUUICallInfoSize:DWord;
    FMinDialParams:TLineDialParams;
    FMaxDialParams:TLineDialParams;
    FDefaultDialParams:TLineDialParams;
    FNumTerminals:Dword;
    FTerminalCaps:TTermCaps;
    FTerminalText:TStringList;
    FDevSpecificSize:DWord;
    FDevSpecific:String;
    FLineFeatures:TLineFeatures;
    {$IFDEF TAPI20}
    FSettableDevStatus:TLineDevStatusFlags;
    FDeviceClasses:TStringList;
    {$ENDIF}
    {$IFDEF TAPI22}
    FPermanentLineGuid: TGUID;
    {$ENDIF}
    {$IFDEF TAPI30}
    FAddressTypes: TLineAddressTypes;
    FProtocolGuid: TGUID;
    FAvailableTracking:TLineCallHubTracking;
    {$ENDIF}
    procedure SetCaps(FLineDevCaps:PLineDevCaps);
    //function GetProviderID: DWord;
  public
    constructor Create(ALineApp:HLineApp;ADeviceID,AAPIVersion,AExtVersion:DWord);virtual;
    destructor Destroy;override;
    //procedure GetDevSpecific(var AStr:String);overload;
    //procedure GetDevSpecific(var AArray:Array of byte);overload;
  published
    property DevSpecific:String read FDevSpecific;
    property Name:String read FLineName;
    property PermanentLineID:DWord read FPermanentLineID;
    //property ProviderID:DWord read FProviderID;
    property ProviderInfo:String read FProviderInfo;
    property SwitchInfo:String read FSwitchInfo;
    property StringFormat:TStringFormat read FStringFormat;
    property AddressModes:TLineAddressModes read FAddressModes;
    property NumAddresses:DWord read FNumAddresses;
    property BearerModes:TLineBearerModes read FBearerModes;
    property MaxRate:DWord read FMaxRate;
    property MediaModes:TLineMediaModes read FMediaModes;
    property GenerateToneModes :TLineToneModes read FGenerateToneModes;
    property GenerateToneMaxNumFreq :DWord read FGenerateToneMaxNumFreq;
    property GenerateDigitModes :TLineDigitModes read FGenerateDigitModes;
    property MonitorToneMaxNumFreq :DWord read FMonitorToneMaxNumFreq;
    property MonitorToneMaxNumEntries :DWord read FMonitorToneMaxNumEntries;
    property MonitorDigitModes:TLineDigitModes read FMonitorDigitModes;
    property GaterDigitsMinTimeout:DWord read FGaterDigitsMinTimeout;
    property GaterDigitsMaxTimeout:DWord read FGaterDigitsMaxTimeout;
    property MedCtlDigitMaxListSize:DWord read FMedCtlDigitMaxListSize;
    property MedCtlMediaMaxListSize:DWord read FMedCtlMediaMaxListSize;
    property MedCtlToneMaxListSize:Dword read FMedCtlToneMaxListSize;
    property MedCtlCallStateMaxListSize:DWord read FMedCtlCallStateMaxListSize;
    property DevCapFlags:TLineDevCapFlags read FDevCapFlags;
    property MaxNumActiveCalls:DWord read FMaxNumActiveCalls;
    property AnswerMode:TLineAnswerMode read FAnswerMode;
    property RingModes:DWord read FRingModes;
    property LineStates:TLineDevStates read FLineStates;
    property UUIAcceptSize:DWord read FUUIAcceptSize;
    property UUIAnswerSize:DWord read FUUIAnswerSize;
    property UUIMakeCallSize:DWord read FUUIMakeCallSize;
    property UUIDropSize:DWord read FUUIDropSize;
    property UUISendUserUserInfoSize:Dword read FUUISendUserUserInfoSize;
    property UUICallInfoSize:DWord read FUUICallInfoSize;
    {$IFNDEF FPC}
    property MinDialParams:TLineDialParams read FMinDialParams;
    property MaxDialParams:TLineDialParams read FMaxDialParams;
    property DefaultDialParams:TLineDialParams read FDefaultDialParams;
    {$ENDIF}
    property NumTerminals:Dword read FNumTerminals;
    property TerminalCaps:TTermCaps read FTerminalCaps;
    property TerminalText:TStringList read FTerminalText;
    property LineFeatures:TLineFeatures read FLineFeatures;
    {$IFDEF TAPI20}
    property SettableDevStatus:TLineDevStatusFlags read FSettableDevStatus;
    property DeviceClasses:TStringList read FDeviceClasses;
    {$ENDIF}
    {$IFDEF TAPI22}
    {$IFNDEF FPC}
    property PermanentLineGuid: TGUID read FPermanentLineGuid;
    {$ENDIF}
    {$ENDIF}
    {$IFDEF TAPI30}
    property AddressTypes: TLineAddressTypes read FAddressTypes;
    {$IFNDEF FPC}
    property ProtocolGuid: TGUID read FProtocolGuid;
    {$ENDIF}
    property AvailableTracking:TLineCallHubTracking read FAvailableTracking;
    {$ENDIF}
    property DeviceID:DWord read FDeviceID ;
  end;


{Prototype für LineDevice - PhoneDevice}
type
  TTAPIDevice = class(TTAPIComponent)
  private
    //FCapsRead:Boolean;
    //FRingCounter:DWord;
    FNegotiateExtVersion:Boolean;
    FNegotiateAPIVersion:Boolean;
    FAPIVersion:DWord;
    FService:TTAPICustomService;
    FDeviceClass:String;
    FID:DWord;
    FExtVersion:DWord;
    FExtLoVer:DWord;
    FExtHiVer:DWord;
    function GetAPIIcon(DeviceClass:String;AIcon:TIcon):hIcon;virtual;abstract;
    procedure SetExtVersion;virtual;abstract;
    //function GetStates: LongInt; virtual; abstract;
    //function GetDeviceClass: String;
    //procedure SetDeviceClass(const Value: String);
    //function GetIcon:TIcon;
    procedure SetID(const Value: DWord);virtual;
    procedure SetService(const Value: TTAPICustomService);
    function GetAPIVersion: DWord;virtual; abstract;
    function GetExtVersion: DWord;virtual; abstract;
    procedure SetDeviceClass(const Value: String);virtual;
  protected
    property DeviceClass:String read FDeviceClass write SetDeviceClass;
    //property Icon:TIcon read GetIcon write FIcon;
    property APIVersion:DWord read GetAPIVersion ;
    property ExtVersion:DWord read GetExtVersion;
     property ID:DWord read FID write SetID;
    property Service:TTAPICustomService read FService write SetService;
    procedure Notification(AComponent:TComponent; Operation :TOperation); override;
  public
     //property RingCounter:DWord read FRingCounter write FRingCounter default 0;
    constructor Create(AOwner:TComponent);override;
    destructor Destroy;override;
  published
  end;

  TTAPILineDevice = class(TTAPIDevice)
  private
    FExtVersion:DWord;
    FExtID:TLineExtensionID;
    FLineMapper: Boolean;
    //FAddressCapsItems:Array of TAddressCaps;
    FDevCaps:TLineDeviceCaps;
    FDeviceConfig:TTAPILineDeviceConfig;
    FDevStateMessages:TLineDevStates;
    FOnStateConnected:TNotifyEvent;
    FOnStateDISCONNECTED:TNotifyEvent;
    FOnStateMsgWaitOn:TNotifyEvent;
    FOnStateMsgWaitOff:TNotifyEvent;
    FOnStateInService:TNotifyEvent;
    FOnStateOutOfService:TNotifyEvent;
    FOnStateMaintenance:TNotifyEvent;
    FOnStateOpen:TNotifyEvent;
    FOnStateClose:TNotifyEvent;
    FOnStateNumCalls:TNotifyEvent;
    FOnStateNumCompletions:TNotifyEvent;
    FOnStateTerminals:TNotifyEvent;
    FOnStateRoamMode:TNotifyEvent;
    FOnStateBattery:TNotifyEvent;
    FOnStateSignal:TNotifyEvent;
    FOnStateDevSpecific:TNotifyEvent;
    FOnStateReInit:TNotifyEvent;
    FOnStateLock:TNotifyEvent;
    FOnStateCapsChange:TNotifyEvent;
    FOnStateConfigChange:TNotifyEvent;
    FOnStateTranslateChange:TNotifyEvent;
    FOnStateComplCancel:TNotifyEvent;
    FOnStateReMoved:TNotifyEvent;
    FOnStateOther:TNotifyEvent;
    FOnStateRinging:TDevStateRingingEvent;
    function GetStates: LongInt;
    function GetCaps: TLineDeviceCaps;
    procedure SetID(const Value: DWord);override;
    procedure SetDeviceClass(const Value: String);override;
    //function GetStateMessages: TLineDevStates;
    //procedure SetStateMessages(const Value: TLineDevStates);
    function GetTranslateCaps: TTranslateCaps;
    function GetAPIVersion: DWord;override;
    function GetExtVersion: DWord;override;
    //function GetAddressCaps(Index: Integer):TAddressCaps;
    function GetDeviceConfig: TTAPILineDeviceConfig;
    procedure SetDeviceConfig(const Value: TTAPILineDeviceConfig);
  protected
     procedure SetExtVersion;override;
  public
    //function GetClass:PChar;
    property States:LongInt read GetStates;
    function GetAPIIcon(DeviceClass:String;AIcon:TIcon):hIcon;override;
    property APIVersion;
    property ExtVersion;
    property Caps:TLineDeviceCaps read GetCaps;
    procedure StateChange(hDevice,Param1,Param2,Param3:LongWord);
    procedure TranslateChange(hDevice:LongWord);
    property TranslateCaps:TTranslateCaps read GetTranslateCaps;
    //property AddressCaps[Index:Integer]:TAddressCaps read GetAddressCaps;
    constructor Create(AOwner:TComponent);override;
    destructor Destroy;override;
    procedure PerformMsg(Msg: TCMTAPI);override;
  published
    //DeviceStateEvents
    property OnStateOther:TNotifyEvent read FOnStateOther write FOnStateOther;
    property OnStateRinging:TDevStateRingingEvent read FOnStateRinging write FOnStateRinging;
    property OnStateConnected:TNotifyEvent read FOnStateConnected write FOnStateConnected;
    property OnStateDisConnected:TNotifyEvent read FOnStateDisConnected write FOnStateDisConnected;
    property OnStateMsgWaitOn:TNotifyEvent read FOnStateMsgWaitOn write FOnStateMsgWaitOn;
    property OnStateMsgWaitOff:TNotifyEvent read FOnStateMsgWaitOff write FOnStateMsgWaitOff;
    property OnStateInService:TNotifyEvent read FOnStateInService write FOnStateInService;
    property OnStateOutOfService:TNotifyEvent read FOnStateOutOfService write FOnStateOutOfService;
    property OnStateMaintenance:TNotifyEvent read FOnStateMaintenance write FOnStateMaintenance;
    property OnStateOpen:TNotifyEvent read FOnStateOpen write FOnStateOpen;
    property OnStateClose:TNotifyEvent read FOnStateClose write FOnStateClose;
    property OnStateNumCalls:TNotifyEvent read FOnStateNumCalls write FOnStateNumCalls;
    property OnStateNumCompletions:TNotifyEvent read FOnStateNumCompletions write FOnStateNumCompletions;
    property OnStateTerminals:TNotifyEvent read FOnStateTerminals write FOnStateTerminals;
    property OnStateRoamMode:TNotifyEvent read FOnStateRoamMode write FOnStateRoamMode;
    property OnStateBattery:TNotifyEvent read FOnStateBattery write FOnStateBattery;
    property OnStateSignal:TNotifyEvent read FOnStateSignal write FOnStateSignal;
    property OnStateDevSpecific:TNotifyEvent read FOnStateDevSpecific write FOnStateDevSpecific;
    property OnStateReinit:TNotifyEvent read FOnStateReinit write FOnStateReinit;
    property OnStateLock:TNotifyEvent read FOnStateLock write FOnStateLock;
    property OnStateCapsChange:TNotifyEvent read FOnStateCapsChange write FOnStateCapsChange;
    property OnStateConfigChange:TNotifyEvent read FOnStateConfigChange write FOnStateConfigChange;
    property OnStateTranslateChange:TNotifyEvent read FOnStateTranslateChange write FOnStateTranslateChange;
    property OnStateComplCancel:TNotifyEvent read FOnStateComplCancel write FOnStateComplCancel;
    property OnStateReMoved:TNotifyEvent read FOnStateReMoved write FOnStateReMoved;
    //property StateMessages:TLineDevStates read GetStateMessages write SetStateMessages;
    property DeviceClass;
    property Service;
    property ID;
    property DevConfig:TTAPILineDeviceConfig read GetDeviceConfig write SetDeviceConfig;
    property LineMapper: Boolean read FLineMapper write FLineMapper default False;
  end;


function IntToLineDevStates(Value:LongWord):TLineDevStates;
function DevStatesToInt(Value:TLineDevStates):LongWord;
function IntToAnswerMode(Value:LongWord):TLineAnswerMode;
function IntToLineRoamMode(Value:LongWord):TLineRoamMode;
function IntToLineFeatures(Value:LongWord): TLineFeatures;
function IntToStringFormat(Value:LongWord): TStringFormat;
function IntToDevCapFlags(Value:LongWord):TLineDevCapFlags;

type
  TPhoneState = (psOther,psConnected,psDisconnected,psOwner,psMonitors,
    psDisplay,psLamp,psRingMode,psRingVolume,psHandsetHookswitch,psHandsetVolume,
    psHandsetGain,psSpeakerHookswitch,psSpeakerVolume,psSpeakerGain,
    psHeadsetHookswitch,psHeadsetVolume,psHeadsetGain,psSuspend,psResume,
    psDevSpecific,psReInit,psCapsChange,psRemoved);
  TPhoneStates = set of TPhoneState;

  TPhoneButtonFunction = (pbfUnknown,pbfConferenc,pbfTransfer,pbfDrop,pbfHold,
    pbfReCall,pbfDisconnect,pbfConnect,pbfMsgWaitOn,pbfMsgWaitOff,
    pbfSelectRing,pbfAbbrevDial,pbfForward,pbfPickup,pbfRingAgain,pbfPark,
    pbfReject,pbfReDirect,pbfMute,pbfVolumeUp,pbfVolumeDown,pbfSpeakerOn,
    pbfSpeakerOff,pbfFlash,pbfDataOn,pbfDataOff,pbfDoNotDisturb,pbfInterCom,
    pbfBridgedApp,pbfBusy,pbfCallApp,pbfDateTime,pbfDirectory,pbfCover,
    pbfCallId,pbfLastNum,pbfNightSrv,pbfSendCalls,pbfMsgIndicator,pbfRepDial,
    pbfSetRepDial,pbfSystemSpeed,pbfStationSpeed,pbfCampOn,pbfSaveRepead,
    pbfQueueCall,pbfNone);


  TPhoneButtonMode = (pbmDummy,pbmCall,pbmFeature,pbmKeypad,pbmLocal,pbmDisplay,pbmUnknown);
  TPhoneButtonModes = Set of TPhoneButtonMode;

  TPhoneHookSwitchDev = (phsdHandset,phsdSpeaker,phsdHeadset);
  TPhoneHookSwitchDevs = Set of TPhoneHookSwitchDev;

  TPhoneHookSwitchMode = (phsmOnHook,phsmMic,phsmSpeaker,phsmMicSpeaker,
    phsmUnknown);
  TPhoneHookSwitchModes = Set of TPhoneHookSwitchMode;

  TPhoneLampMode = (plmDummy,plmOff,plmSteady,plmWink,plmFlash,plmFlutter,
    plmBrokenFlutter,plmUnknown);
  TPhoneLampModes = Set of TPhoneLampMode;

  TPhoneFeature = (pfGetButtonInfo,pfGetData,pfGetDispaly,pfGetGainHandset,
    pfGetGainSpeaker,pfGetGainHeadset,pfGetHookSwitchHandset,pfGetHookSwitchSpeaker,
    pfGetHookSwitchHeadset,pfGetLamp,pfGetRing,pfGetVolumeHandset,pfGetVolumeSpeaker,
    pfGetVolumeHeadset,pfSetButtonInfo,pfSetData,pfSetDisplay,pfSetGainHandset,
    pfSetGainSpeaker,pfSetGainHeadset,pfSetHookSwitchHandset,pfSetHookSwitchSpeaker,
    pfSetHookSwitchHeadset,pfSetLamp,pfSetRing,pfSetVolumeHandset,
    pfSetVolumeSpeaker,pfSetVolumeHeadset);
  TPhoneFeatures = Set of TPhoneFeature;

  TPhoneStatusFlags = (psfUnknown,psfConnected,psfSuspended);

  TPhoneButtonLamp=class(TCollectionItem)
  private
    FLampModes:TPhoneLampModes;
    FButtonMode: TPhoneButtonMode;
    FButtonFunction: TPhoneButtonFunction;
    FID:Integer;
  public
    constructor Create(Collection: TCollection);override;
    destructor Destroy;override;
    property LampModes:TPhoneLampModes read FLampModes write FLampModes;
    property ButtonMode: TPhoneButtonMode read FButtonMode write FButtonMode;
    property ButtonFunction: TPhoneButtonFunction read FButtonFunction write FButtonFunction;
    property ID:Integer read FID write FID;
  end;

  TPhoneButtonLamps=class(TCollection)
  private
    function GetItem(Index: Integer): TPhoneButtonLamp;
    procedure SetItem(Index: Integer; const Value: TPhoneButtonLamp);
  public
    constructor Create(ItemClass: TCollectionItemClass);
    destructor Destroy;override;
    function Add:TPhoneButtonLamp;
    property Items[Index:Integer]:TPhoneButtonLamp read GetItem write SetItem;
  end;

  TPhoneDataArea=class(TCollectionItem)
  private
    FData:Array of Byte;
    FSize:DWord;
    function GetData(Index: Integer): Byte;
    function GetSize: DWord;
    procedure SetData(Index: Integer; const Value: Byte);
    procedure SetSize(const Value: DWord);
  public
    constructor Create(Collection: TCollection);override;
    destructor Destroy;override;
    property Size:DWord read GetSize write SetSize default 0;
    property Data[Index:Integer]:Byte read GetData write SetData;
  end;

  TPhoneDataAreas=class(TCollection)
  private
    function GetItem(Index: Integer): TPhoneDataArea;
    procedure SetItem(Index: Integer; const Value: TPhoneDataArea);
  public
    constructor Create(ItemClass: TCollectionItemClass);
    destructor Destroy;override;
    function Add:TPhoneDataArea;
    property Items[Index:Integer]:TPhoneDataArea read GetItem write SetItem;
  end;

  PPhoneDeviceCaps=^TPhoneDeviceCaps;
  TPhoneDeviceCaps=class(TPersistent)
  private
    FPhoneApp:HPhoneApp;
    FAPIVersion,FExtVersion:DWord;
    FDeviceID:DWord;
    FProviderInfo:String;
    FPhoneInfo:String;
    FPermanentPhoneID:DWORD;
    FPhoneName:String;
    FStringFormat:TStringFormat;
    FPhoneStates:TPhoneStates;
    FHookSwitchDevs:TPhoneHookSwitchDevs;
    FHandsetHookSwitchModes:TPhoneHookSwitchModes;
    FSpeakerHookSwitchModes:TPhoneHookSwitchModes;
    FHeadsetHookSwitchModes:TPhoneHookSwitchModes;
    FHandsetHookSwitchVolumeFlag,
    FSpeakerHookSwitchVolumeFlag,
    FHeadsetHookSwitchVolumeFlag,
    FHandsetHookSwitchGainFlag,
    FSpeakerHookSwitchGainFlag,
    FHeadsetHookSwitchGainFlag:Boolean;
    FDisplayNumRows:DWORD;
    FDisplayNumColumns:DWORD;
    FNumRingModes:DWord;
    FNumButtonLamps:DWord;
    //FButtonModes:Array of TPhoneButtonMode;
    //FButtonFunctions:Array of TPhoneButtonFunction;
    FButtonLamps:TPhoneButtonLamps;
    FNumSetData:DWord;
    FSetData:TPhoneDataAreas;
    FNumGetData:DWord;
    FGetData:TPhoneDataAreas;
    FDevSpecific:String;
    {$IFDEF TAPI20}
    FDeviceClasses:TStringList;
    FPhoneFeatures:TPhoneFeatures;
    FSettableHandsetHookSwitchModes,
    FSettableSpeakerHookSwitchModes,
    FSettableHeadsetHookSwitchModes,
    FMonitoredHandsetHookSwitchModes,
    FMonitoredSpeakerHookSwitchModes,
    FMonitoredHeadsetHookSwitchModes: TPhoneHookSwitchModes;
    {$ENDIF}
    {$IFDEF TAPI22}
    FPermanentPhoneGuid: TGUID;
    {$ENDIF}
    function GetProviderID: DWord;
   //function GetStringFormat: TStringFormat;
    procedure SetCaps(FPhoneCaps:PPhoneCaps);
  protected
  public
    //property NewCaps:PPhoneCaps read FPhoneDevCaps write SetNewCaps;
    constructor Create(APhoneApp:HPhoneApp;ADeviceID,AAPIVersion,AExtVersion:DWord);virtual;
    destructor Destroy;override;
    procedure Update;
  published
    property Name:String read FPhoneName;
    property PermanentPhoneID:DWord read FPermanentPhoneID;
    property ProviderID:DWord read GetProviderID;
    property ProviderInfo:String read FProviderInfo;
    property PhoneInfo:String read FPhoneInfo;
    property StringFormat:TStringFormat read FStringFormat;
    property PhoneStates:TPhoneStates read FPhoneStates;
    property HookSwitchDevs:TPhoneHookSwitchDevs read FHookSwitchDevs;
    property HandsetHookSwitchModes:TPhoneHookSwitchModes read FHandsetHookSwitchModes;
    property SpeakerHookSwitchModes:TPhoneHookSwitchModes read FSpeakerHookSwitchModes;
    property HeadsetHookSwitchModes:TPhoneHookSwitchModes read FHeadsetHookSwitchModes;
    property HandsetHookSwitchVolumeFlag:Boolean read FHandsetHookSwitchVolumeFlag;
    property SpeakerHookSwitchVolumeFlag:Boolean read FSpeakerHookSwitchVolumeFlag;
    property HeadsetHookSwitchVolumeFlag:Boolean read FHeadsetHookSwitchVolumeFlag;
    property HandsetHookSwitchGainFlag:Boolean read FHandsetHookSwitchGainFlag;
    property SpeakerHookSwitchGainFlag:Boolean read FSpeakerHookSwitchGainFlag;
    property HeadsetHookSwitchGainFlag:Boolean read FHeadsetHookSwitchGainFlag;
    property DisplayNumRows:DWORD read FDisplayNumRows;
    property DisplayNumColumns:DWORD read FDisplayNumColumns;
    property NumRingModes:DWord read FNumRingModes;
    property NumButtonLamps:DWord read FNumButtonLamps;
    property ButtonLamps:TPhoneButtonLamps read FButtonLamps;
    property NumSetData:DWord read FNumSetData;
    //property SetData:Array of Byte read FSetData;
    property NumGetData:DWord read FNumGetData;
    //FGetData:Array of Byte;
    property DevSpecific:String read FDevSpecific;
    {$IFDEF TAPI20}
    property DeviceClasses:TStringList read FDeviceClasses;
    property PhoneFeatures:TPhoneFeatures read FPhoneFeatures;
    property SettableHandsetHookSwitchModes: TPhoneHookSwitchModes read FSettableHandsetHookSwitchModes;
    property SettableSpeakerHookSwitchModes: TPhoneHookSwitchModes read FSettableSpeakerHookSwitchModes;
    property SettableHeadsetHookSwitchModes: TPhoneHookSwitchModes read FSettableHeadsetHookSwitchModes;
    property MonitoredHandsetHookSwitchModes: TPhoneHookSwitchModes read FMonitoredHandsetHookSwitchModes;
    property MonitoredSpeakerHookSwitchModes: TPhoneHookSwitchModes read FMonitoredSpeakerHookSwitchModes;
    property MonitoredHeadsetHookSwitchModes: TPhoneHookSwitchModes read FMonitoredHeadsetHookSwitchModes;
    {$ENDIF}
    {$IFDEF TAPI22}
    {$IFNDEF FPC}
    property PermanentPhoneGuid: TGUID read FPermanentPhoneGuid;
    {$ENDIF}
    {$ENDIF}
    property DeviceID:DWord read FDeviceID ;
  end;

  TPhoneDeviceStatus=class(TPersistent)
  private
    FStatusFlags:TPhoneStatusFlags;
    FNumOwners:DWord;
    FNumMonitors:DWord;
    FRingMode:DWord;
    FRingVolume:DWord;
    FHandSetHookSwitchMode:TPhoneHookSwitchMode;
    FHandSetVolume:DWord;
    FHandSetGain:Dword;
    FSpeakerHookSwitchMode:TPhoneHookSwitchMode;
    FSpeakerVolume:DWord;
    FSpeakerGain:Dword;
    FHeadSetHookSwitchMode:TPhoneHookSwitchMode;
    FHeadSetVolume:DWord;
    FHeadSetGain:Dword;
    FDisplay:String;
    FButtonLamps:TPhoneButtonLamps;
    FOwnerName:String;
    FDevSpecific:String;
    {$IFDEF TAPI20}
    FFeatures:TPhoneFeatures;
    {$ENDIF}
  protected
    procedure SetStatus(DevStatus:PPhoneStatus);
  public
    constructor Create(APhone:hPhone);virtual;
    destructor Destroy;override;
  published
    property Flags:TPhoneStatusFlags read FStatusFlags;
    property NumOwners:DWord read FNumOwners;
    property NumMonitors:DWord read FNumMonitors;
    property RingMode:DWord read FRingMode;
    property RingVolume:DWord read FRingVolume;
    property HandSetHookSwitchMode:TPhoneHookSwitchMode read FHandSetHookSwitchMode;
    property HandSetVolume:DWord read FHandSetVolume;
    property HandSetGain:Dword read FHandSetGain;
    property SpeakerHookSwitchMode:TPhoneHookSwitchMode read FSpeakerHookSwitchMode;
    property SpeakerVolume:DWord read FSpeakerVolume;
    property SpeakerGain:Dword read FSpeakerGain;
    property HeadSetHookSwitchMode:TPhoneHookSwitchMode read FHeadSetHookSwitchMode;
    property HeadSetVolume:DWord read FHeadSetVolume;
    property HeadSetGain:Dword read FHeadSetGain;
    property Display:String read FDisplay;
    property ButtonLamps:TPhoneButtonLamps read FButtonLamps;
    property OwnerName:String read FOwnerName;
    property DevSpecific:String read FDevSpecific;
    {$IFDEF TAPI20}
    property Features:TPhoneFeatures read FFeatures;
    {$ENDIF}
  end;


  TTAPIPhoneDevice = class(TTAPIDevice)
  private
    FDevCaps:TPhoneDeviceCaps;
    FExtID:TPhoneExtensionID;
    function GetCaps: TPhoneDeviceCaps;
    function GetAPIVersion: DWord;override;
    function GetExtVersion: DWord;override;
    procedure SetID(const Value: DWord);override;
  protected
    procedure SetExtVersion;override;
  public
    property APIVersion;
    property ExtVersion;
    property Caps:TPhoneDeviceCaps read GetCaps;
    constructor Create(AOwner:TComponent);override;
    destructor Destroy;override;
    function GetAPIIcon(DeviceClass:String;AIcon:TIcon):hIcon;override;
    procedure PerformMsg(Msg: TCMTAPI);override;
  published
    property DeviceClass;
    property Service;
    property ID;
 end;



function PhoneStatesToInt(Value:TPhoneStates):LongWord;
function IntToPhoneStates(Value:LongWord):TPhoneStates;
function IntToPhoneButtonMode(Value:LongWord):TPhoneButtonMode;
function IntToPhoneButtonFunction(Value:LongWord):TPhoneButtonFunction;
function IntToPhoneHookSwitchMode(Value:LongWord):TPhoneHookSwitchMode;
function IntToPhoneStatusFlags(Value:LongWord):TPhoneStatusFlags;
function IntToPhoneLampModes(Value:LongWord):TPhoneLampModes;
function PhoneHookSwitchModeToInt(Value:TPhoneHookSwitchMode):LongWord;
{$IFDEF TAPI20}
function IntToPhoneFeatures(Value:LongWord):TPhoneFeatures;
{$ENDIF}
function IntToPhoneHookSwitchModes(Value:LongWord):TPhoneHookSwitchModes;
function IntToPhoneHookSwitchDevs(Value:LongWord):TPhoneHookSwitchDevs;
{$IFDEF TAPI30}
function IntToAddressTypes(Value:LongWord):TLineAddressTypes;
function IntToCallHubTracking(Value:LongWord):TLineCallHubTracking;
function AddressTypesToInt(Value:TLineAddressTypes):LongWord;
function AddressTypeToInt(Value:TLineAddressType):LongWord;
{$ENDIF}
function IntToTermDev(Value:LongWord):TLineTermDev;
function IntToTermSharing(Value:LongWord):TLineTermSharing;

procedure Register;

{$ENDIF}
{$ENDIF}
implementation

{$IFDEF WINDOWS}
{$IFDEF CPU32}
uses {$IFDEF VER120}D4Comp,{$ENDIF}TAPILists,TAPICurVer, TAPIHelpFunc;

procedure Register;
begin
{$IFDEF TAPI30}
  RegisterComponents('TAPI30', [TTAPILineDevice]);
  RegisterComponents('TAPI30', [TTAPIPhoneDevice]);
{$ELSE}
{$IFDEF TAPI22}
  RegisterComponents('TAPI22', [TTAPILineDevice]);
  RegisterComponents('TAPI22', [TTAPIPhoneDevice]);
{$ELSE}
{$IFDEF TAPI21}
  RegisterComponents('TAPI21', [TTAPILineDevice]);
  RegisterComponents('TAPI21', [TTAPIPhoneDevice]);
{$ELSE}
{$IFDEF TAPI20}
  RegisterComponents('TAPI20', [TTAPILineDevice]);
  RegisterComponents('TAPI20', [TTAPIPhoneDevice]);
{$ELSE}
  RegisterComponents('TAPI', [TTAPILineDevice]);
  RegisterComponents('TAPI', [TTAPIPhoneDevice]);
{$ENDIF}
{$ENDIF}
{$ENDIF}
{$ENDIF}
end;

function IntToTermDev(Value:LongWord):TLineTermDev;
begin
  Result:=ltdNoDef;
  case Value of
    LINETERMDEV_HEADSET:Result:=ltdHeadSet;
    LINETERMDEV_PHONE:Result:=ltdPhone;
    LINETERMDEV_SPEAKER:Result:=ltdSpeaker;
  end;
end;




function IntToTermSharing(Value:LongWord):TLineTermSharing;
begin
  Result:=ltsNoDef;
  case Value of
    LINETERMSHARING_PRIVATE:Result:=ltsPrivate;
    LINETERMSHARING_SHAREDEXCL:Result:=ltsSharedExcl;
    LINETERMSHARING_SHAREDCONF:Result:=ltsSharedConf;
  end;
end;




function IntToStringFormat(Value:LongWord): TStringFormat;
begin
  result:=sfAsCII;
  case Value of
    STRINGFORMAT_ASCII:result:=sfASCII;
    STRINGFORMAT_DBCS:result:=sfDBCS;
    STRINGFORMAT_UNICODE:result:=sfUNICODE;
    STRINGFORMAT_BINARY:result:=sfBinary;
  end;
end;

function IntToDevCapFlags(Value:LongWord):TLineDevCapFlags;
begin
  result:=[];
  if (Value and LINEDEVCAPFLAGS_CROSSADDRCONF) = LINEDEVCAPFLAGS_CROSSADDRCONF then
    Result:=Result+[ldcfCROSSADDRCONF];
  if (Value and LINEDEVCAPFLAGS_HIGHLEVCOMP) = LINEDEVCAPFLAGS_HIGHLEVCOMP then
    Result:=Result+[ldcfHIGHLEVCOMP];
  if (Value and LINEDEVCAPFLAGS_LOWLEVCOMP) = LINEDEVCAPFLAGS_LOWLEVCOMP then
    Result:=Result+[ldcfLOWLEVCOMP];
  if (Value and LINEDEVCAPFLAGS_MEDIACONTROL) = LINEDEVCAPFLAGS_MEDIACONTROL then
    Result:=Result+[ldcfMEDIACONTROL];
  if (Value and LINEDEVCAPFLAGS_MULTIPLEADDR) = LINEDEVCAPFLAGS_MULTIPLEADDR then
    Result:=Result+[ldcfMULTIPLEADDR];
  if (Value and LINEDEVCAPFLAGS_CLOSEDROP) = LINEDEVCAPFLAGS_CLOSEDROP then
    Result:=Result+[ldcfCLOSEDROP];
  if (Value and LINEDEVCAPFLAGS_DIALBILLING) = LINEDEVCAPFLAGS_DIALBILLING then
    Result:=Result+[ldcfDIALBILLING];
  if (Value and LINEDEVCAPFLAGS_DIALQUIET) = LINEDEVCAPFLAGS_DIALQUIET then
    Result:=Result+[ldcfDIALQUIET];
  if (Value and LINEDEVCAPFLAGS_DIALDIALTONE) = LINEDEVCAPFLAGS_DIALDIALTONE then
    Result:=Result+[ldcfDIALDIALTONE];
  {$IFDEF TAPI30}
  if (Value and LINEDEVCAPFLAGS_MSP) = LINEDEVCAPFLAGS_MSP then
    Result:=Result+[ldcfMSP];
  if (Value and LINEDEVCAPFLAGS_CALLHUB) = LINEDEVCAPFLAGS_CALLHUB then
    Result:=Result+[ldcfCallHub];
  if (Value and LINEDEVCAPFLAGS_CALLHUBTRACKING) = LINEDEVCAPFLAGS_CALLHUBTRACKING then
    Result:=Result+[ldcfCallHubTracking];
  if (Value and LINEDEVCAPFLAGS_PRIVATEOBJECTS) = LINEDEVCAPFLAGS_PRIVATEOBJECTS then
    Result:=Result+[ldcfPrivateObjects];
  {$ENDIF}
  {IF (Not((LINEDEVCAPFLAGS_CROSSADDRCONF xor $FFFFFFFF) or Value ))=0 then Result:=Result+[ldcfCROSSADDRCONF];
  IF (Not((LINEDEVCAPFLAGS_HIGHLEVCOMP xor $FFFFFFFF) or Value ))=0 then Result:=Result+[];
  IF (Not((LINEDEVCAPFLAGS_LOWLEVCOMP xor $FFFFFFFF) or Value ))=0 then Result:=Result+[];
  IF (Not((LINEDEVCAPFLAGS_MEDIACONTROL xor $FFFFFFFF) or Value ))=0 then Result:=Result+[];
  IF (Not((LINEDEVCAPFLAGS_MULTIPLEADDR xor $FFFFFFFF) or Value ))=0 then Result:=Result+[];
  IF (Not((LINEDEVCAPFLAGS_CLOSEDROP xor $FFFFFFFF) or Value ))=0 then Result:=Result+[];
  IF (Not((LINEDEVCAPFLAGS_DIALBILLING xor $FFFFFFFF) or Value ))=0 then Result:=Result+[];
  IF (Not((LINEDEVCAPFLAGS_DIALQUIET  xor $FFFFFFFF) or Value ))=0 then Result:=Result+[];
  IF (Not((LINEDEVCAPFLAGS_DIALDIALTONE xor $FFFFFFFF) or Value ))=0 then Result:=Result+[]; }
  //{$IFDEF TAPI30}
  {IF (Not((LINEDEVCAPFLAGS_MSP xor $FFFFFFFF) or Value ))=0 then Result:=Result+[];
  IF (Not((LINEDEVCAPFLAGS_CALLHUB xor $FFFFFFFF) or Value ))=0 then Result:=Result+[];
  IF (Not((LINEDEVCAPFLAGS_CALLHUBTRACKING xor $FFFFFFFF) or Value ))=0 then Result:=Result+[];
  IF (Not((LINEDEVCAPFLAGS_PRIVATEOBJECTS xor $FFFFFFFF) or Value ))=0 then Result:=Result+[];  }
  //{$ENDIF}
end;


function IntToLineDevStates(Value:LongWord):TLineDevStates;
begin
  Result:=[];
  IF (Not((LINEDEVSTATE_OTHER xor $FFFFFFFF) or Value ))=0 then Result:=Result+[dsOther];
  IF (Not((LINEDEVSTATE_RINGING xor $FFFFFFFF) or Value ))=0 then Result:=Result+[dsRinging];
  IF (Not((LINEDEVSTATE_CONNECTED xor $FFFFFFFF) or Value ))=0 then Result:=Result+[dsConnected];
  IF (Not((LINEDEVSTATE_DISCONNECTED xor $FFFFFFFF) or Value ))=0 then Result:=Result+[dsDisconnected];
  IF (Not((LINEDEVSTATE_MSGWAITON xor $FFFFFFFF) or Value ))=0 then Result:=Result+[dsMsgWaitOn];
  IF (Not((LINEDEVSTATE_MSGWAITOFF xor $FFFFFFFF) or Value ))=0 then Result:=Result+[dsMsgWaitOff];
  IF (Not((LINEDEVSTATE_INSERVICE xor $FFFFFFFF) or Value ))=0 then Result:=Result+[dsInService];
  IF (Not((LINEDEVSTATE_OUTOFSERVICE  xor $FFFFFFFF) or Value ))=0 then Result:=Result+[dsOutOfService];
  IF (Not((LINEDEVSTATE_MAINTENANCE xor $FFFFFFFF) or Value ))=0 then Result:=Result+[dsMaintenance];
  IF (Not((LINEDEVSTATE_OPEN xor $FFFFFFFF) or Value ))=0 then Result:=Result+[dsOpen];
  IF (Not((LINEDEVSTATE_CLOSE xor $FFFFFFFF) or Value ))=0 then Result:=Result+[dsClose];
  IF (Not((LINEDEVSTATE_NUMCALLS xor $FFFFFFFF) or Value ))=0 then Result:=Result+[dsNumCalls];
  IF (Not((LINEDEVSTATE_NUMCOMPLETIONS xor $FFFFFFFF) or Value ))=0 then Result:=Result+[dsNumCompletions];
  IF (Not((LINEDEVSTATE_TERMINALS xor $FFFFFFFF) or Value ))=0 then Result:=Result+[dsTerminals];
  IF (Not((LINEDEVSTATE_ROAMMODE xor $FFFFFFFF) or Value ))=0 then Result:=Result+[dsRoamMode];
  IF (Not((LINEDEVSTATE_BATTERY xor $FFFFFFFF) or Value ))=0 then Result:=Result+[dsBattery];
  IF (Not((LINEDEVSTATE_SIGNAL xor $FFFFFFFF) or Value ))=0 then Result:=Result+[dsSignal];
  IF (Not((LINEDEVSTATE_DEVSPECIFIC  xor $FFFFFFFF) or Value ))=0 then Result:=Result+[dsDevSpecific];
  IF (Not((LINEDEVSTATE_REINIT xor $FFFFFFFF) or Value ))=0 then Result:=Result+[dsReInit];
  IF (Not((LINEDEVSTATE_LOCK xor $FFFFFFFF) or Value ))=0 then Result:=Result+[dsLock];
  IF (Not((LINEDEVSTATE_CAPSCHANGE xor $FFFFFFFF) or Value ))=0 then Result:=Result+[dsCapsChange];
  IF (Not((LINEDEVSTATE_CONFIGCHANGE xor $FFFFFFFF) or Value ))=0 then Result:=Result+[dsConfigChange];
  IF (Not((LINEDEVSTATE_TRANSLATECHANGE xor $FFFFFFFF) or Value ))=0 then Result:=Result+[dsTranslateChange];
  IF (Not((LINEDEVSTATE_COMPLCANCEL xor $FFFFFFFF) or Value ))=0 then Result:=Result+[dsComplCancel];
  IF (Not((LINEDEVSTATE_REMOVED xor $FFFFFFFF) or Value ))=0 then Result:=Result+[dsReMoved];
end;

function DevStatesToInt(Value:TLineDevStates):LongWord;
begin
  result:=0;
  if  dsOther in Value then result:=result or LINEDEVSTATE_OTHER;
  if  dsRinging in Value then result:=result or LINEDEVSTATE_RINGING;
  if  dsConnected in Value then result:=result or LINEDEVSTATE_CONNECTED;
  if  dsDisconnected in Value then result:=result or LINEDEVSTATE_DISCONNECTED;
  if  dsMsgWaitOn in Value then result:=result or LINEDEVSTATE_MSGWAITON;
  if  dsMsgWaitOff in Value then result:=result or LINEDEVSTATE_MSGWAITOFF;
  if  dsInService in Value then result:=result or LINEDEVSTATE_INSERVICE;
  if  dsOutOfService in Value then result:=result or LINEDEVSTATE_OUTOFSERVICE;
  if  dsMaintenance in Value then result:=result or LINEDEVSTATE_MAINTENANCE;
  if  dsOpen in Value then result:=result or LINEDEVSTATE_OPEN;
  if  dsClose  in Value then result:=result or LINEDEVSTATE_CLOSE;
  if  dsNumCalls  in Value then result:=result or LINEDEVSTATE_NUMCALLS;
  if  dsNumCompletions in Value then result:=result or LINEDEVSTATE_NUMCOMPLETIONS;
  if  dsTerminals in Value then result:=result or LINEDEVSTATE_TERMINALS;
  if  dsRoamMode in Value then result:=result or LINEDEVSTATE_ROAMMODE;
  if  dsBattery in Value then result:=result or LINEDEVSTATE_BATTERY;
  if  dsSignal in Value then result:=result or  LINEDEVSTATE_SIGNAL;
  if  dsDevSpecific in Value then result:=result or LINEDEVSTATE_DEVSPECIFIC;
  if  dsReInit in Value then result:=result or LINEDEVSTATE_REINIT;
  if  dsLock in Value then result:=result or LINEDEVSTATE_LOCK;
  if  dsReMoved in Value then result:=result or LINEDEVSTATE_REMOVED;
  if  dsCapsChange in Value then result:=result or LINEDEVSTATE_CAPSCHANGE;
  if  dsConfigChange in Value then result:=result or LINEDEVSTATE_CONFIGCHANGE;
  if  dsTranslateChange in Value then result:=result or LINEDEVSTATE_TRANSLATECHANGE;
  if  dsComplCancel in Value then result:=result or LINEDEVSTATE_COMPLCANCEL;
end;


function IntToAnswerMode(Value:LongWord):TLineAnswerMode;
begin
  Result:=lamUnkown;
  case Value of
    LINEANSWERMODE_NONE:Result:=lamNone;
    LINEANSWERMODE_DROP:Result:=lamDrop;
    LINEANSWERMODE_HOLD:Result:=lamHold;
  end;
end;

function IntToLineRoamMode(Value:LongWord):TLineRoamMode;
begin
  Result:=lrmUnavail;
  case Value of
     LINEROAMMODE_UNKNOWN:Result:=lrmUnknown;
     LINEROAMMODE_UNAVAIL:Result:=lrmUnavail;
     LINEROAMMODE_HOME:Result:=lrmHome;
     LINEROAMMODE_ROAMA:Result:=lrmRoamA;
     LINEROAMMODE_ROAMB:Result:=lrmRoamB;
  end;
end;

function IntToDevStatusFlags(Value:LongWord):TLineDevStatusFlags;
begin
  Result:=[];
  IF (Not((LINEDEVSTATUSFLAGS_CONNECTED xor $FFFFFFFF) or Value ))=0 then Result:=Result+[ldsfConnected];
  IF (Not((LINEDEVSTATUSFLAGS_MSGWAIT xor $FFFFFFFF) or Value ))=0 then Result:=Result+[ldsfMsgWait];
  IF (Not((LINEDEVSTATUSFLAGS_INSERVICE xor $FFFFFFFF) or Value ))=0 then Result:=Result+[ldsfInService];
  IF (Not((LINEDEVSTATUSFLAGS_LOCKED xor $FFFFFFFF) or Value ))=0 then Result:=Result+[ldsfLocked];
end;

function IntToLineFeatures(Value:LongWord): TLineFeatures;
begin
  Result:=[];
  IF (Not((LINEFEATURE_DEVSPECIFIC xor $FFFFFFFF) or Value ))=0 then Result:=Result+[lfDevSpecific];
  IF (Not((LINEFEATURE_DEVSPECIFICFEAT xor $FFFFFFFF) or Value ))=0 then Result:=Result+[lfDevSpecificFeat];
  IF (Not((LINEFEATURE_FORWARD xor $FFFFFFFF) or Value ))=0 then Result:=Result+[lfForward];
  IF (Not((LINEFEATURE_MAKECALL xor $FFFFFFFF) or Value ))=0 then Result:=Result+[lfMakeCall];
  IF (Not((LINEFEATURE_SETMEDIACONTROL xor $FFFFFFFF) or Value ))=0 then Result:=Result+[lfSetMediaControl];
  IF (Not((LINEFEATURE_SETTERMINAL xor $FFFFFFFF) or Value ))=0 then Result:=Result+[lfSetTerminal];
{$IFDEF TAPI20}
  IF (Not((LINEFEATURE_SETDEVSTATUS xor $FFFFFFFF) or Value ))=0 then Result:=Result+[lfSetDevStatus];
  IF (Not((LINEFEATURE_FORWARDFWD xor $FFFFFFFF) or Value ))=0 then Result:=Result+[lfForwardFWD];
  IF (Not((LINEFEATURE_FORWARDDND xor $FFFFFFFF) or Value ))=0 then Result:=Result+[lfForwardDND];
{$ENDIF}
end;

function IntToPhoneLampModes(Value:LongWord):TPhoneLampModes;
begin
  Result:=[];
  IF (Not((PHONELAMPMODE_DUMMY xor $FFFFFFFF) or Value ))=0 then Result:=Result+[plmDummy];
  IF (Not((PHONELAMPMODE_OFF xor $FFFFFFFF) or Value ))=0 then Result:=Result+[plmOff];
  IF (Not((PHONELAMPMODE_STEADY xor $FFFFFFFF) or Value ))=0 then Result:=Result+[plmSteady];
  IF (Not((PHONELAMPMODE_WINK xor $FFFFFFFF) or Value ))=0 then Result:=Result+[plmWink];
  IF (Not((PHONELAMPMODE_FLASH xor $FFFFFFFF) or Value ))=0 then Result:=Result+[plmFlash];
  IF (Not((PHONELAMPMODE_FLUTTER xor $FFFFFFFF) or Value ))=0 then Result:=Result+[plmFlutter];
  IF (Not((PHONELAMPMODE_BROKENFLUTTER xor $FFFFFFFF) or Value ))=0 then Result:=Result+[plmBrokenFlutter];
  IF (Not((PHONELAMPMODE_UNKNOWN xor $FFFFFFFF) or Value ))=0 then Result:=Result+[plmUnknown];
end;

{$IFDEF TAPI30}
function IntToAddressTypes(Value:LongWord):TLineAddressTypes;
begin
  Result:=[];
  IF (Not((LINEADDRESSTYPE_PHONENUMBER xor $FFFFFFFF) or Value ))=0 then Result:=Result+[latPhoneNumber];
  IF (Not((LINEADDRESSTYPE_SDP xor $FFFFFFFF) or Value ))=0 then Result:=Result+[latSDP];
  IF (Not((LINEADDRESSTYPE_EMAILNAME xor $FFFFFFFF) or Value ))=0 then Result:=Result+[latEmailName];
  IF (Not((LINEADDRESSTYPE_DOMAINNAME xor $FFFFFFFF) or Value ))=0 then Result:=Result+[latDomainName];
  IF (Not((LINEADDRESSTYPE_IPADDRESS xor $FFFFFFFF) or Value ))=0 then Result:=Result+[latIPAddress];
end;

function IntToCallHubTracking(Value:LongWord):TLineCallHubTracking;
begin
  result:=lchtNone;
  case Value of
    LINECALLHUBTRACKING_NONE:Result:=lchtNone;
    LINECALLHUBTRACKING_PROVIDERLEVEL:Result:=lchtProviderLevel;
    LINECALLHUBTRACKING_ALLCALLS:Result:=lchtAllCalls;
  end;
end;
function AddressTypesToInt(Value:TLineAddressTypes):LongWord;
begin
 Result:=0;
 if latPhoneNumber in Value then Result:=Result or LINEADDRESSTYPE_PHONENUMBER;
 if latSDP in Value then Result:=Result or LINEADDRESSTYPE_SDP;
 if latEmailName in Value then Result:=Result or LINEADDRESSTYPE_EMAILNAME;
 if latDomainName in Value then Result:=Result or LINEADDRESSTYPE_DOMAINNAME;
 if latIPAddress in Value then Result:=Result or LINEADDRESSTYPE_IPADDRESS;
end;

function AddressTypeToInt(Value:TLineAddressType):Longword;
begin
 Result:=0;
 case Value of
   latPhoneNumber:Result:=LINEADDRESSTYPE_PHONENUMBER;
   latSDP:Result:=LINEADDRESSTYPE_SDP;
   latEmailName:Result:=LINEADDRESSTYPE_EMAILNAME;
   latDomainName:Result:=LINEADDRESSTYPE_DOMAINNAME;
   latIPAddress:Result:=LINEADDRESSTYPE_IPADDRESS;
 end;
end;
{$ENDIF}


{ TDeviceStatus }

constructor TDeviceStatus.Create(ALine: hLine);
var DevStatus:PLINEDEVSTATUS;
    Size:Integer;
    R:Longint;
begin
  inherited Create;
  {$IFDEF TAPI20}
  FAppInfo:=TAppInfos.Create(TAppInfoItem);
  {$ENDIF}
  Size:=SizeOf(TLineCallStatus)+1000;
  GetMem(PLINEDEVSTATUS(DevStatus),Size);
  try
    DevStatus.dwTotalSize:=Size;
    R:=LineGetLineDevStatus(ALine,DevStatus);
    if DWord(R)<> 0 then RaiseTAPILineError(R);
    SetStatus(DevStatus);
  finally
    FreeMem(DevStatus);
  end;
end;

destructor TDeviceStatus.Destroy;
begin
  {$IFDEF TAPI20}
  FAppInfo.Free;
  {$ENDIF}
  inherited;
end;

{procedure TDeviceStatus.GetDevSpecific(var AArray: array of byte);
begin
  if FDevSpecificSize >0 then
    StrCopy(PChar(@AArray),PChar(@FDevSpecific));
end;

procedure TDeviceStatus.GetDevSpecific(var AStr: String; UniModemFlag:Boolean);
begin
  if FDevSpecificSize >0 then
  begin
    if UniModemFlag=False then
      AStr:=PChar(FDevSpecific)
    else
      AStr:=PChar(@FDevSpecific)+8; // dwContens + dwKeyOffset
  end;
end;  }

function TDeviceStatus.GetTerminalModes(Index:Integer): TLineTermMode;
begin
  result:=FTerminalModes[Index];
end;

procedure TDeviceStatus.SetStatus(DStatus:PLINEDEVSTATUS);
var i:Integer;
    Dummy:Array of char;
    {$IFDEF TAPI20}
    AInfoItem: TAppInfoItem;
    {$ENDIF}
begin
  FNumOpens:=DStatus^.dwNumOpens;
  FOpenMediaModes:=IntToMediaModes(DStatus^.dwOpenMediaModes);
  FNumActiveCalls:=DStatus^.dwNumActiveCalls;
  FNumOnHoldCalls:=DStatus^.dwNumOnHoldCalls;
  FNumOnHoldPendCalls:=DStatus^.dwNumOnHoldPendCalls;
  FLineFeatures:=IntToLineFeatures(DStatus^.dwLineFeatures);
  FNumCallCompletions:=DStatus^.dwNumCallCompletions;
  FRingMode:=DStatus^.dwNumCallCompletions;
  FSignalLevel:=DStatus^.dwSignalLevel;
  FBatteryLevel:=DStatus^.dwBatteryLevel;
  FRoamMode:=IntToLineRoamMode(DStatus^.dwRoamMode);
  FDevStatusFlags:=IntToDevStatusFlags(DStatus^.dwDevStatusFlags);
  if DStatus^.dwTerminalModesSize > 0 then
  begin
    SetLength(Dummy,DStatus^.dwTerminalModesSize);
    SetLength(FTerminalModes,DStatus^.dwTerminalModesSize);
    StrCopy(PChar(Dummy),PCHAR(DStatus)+DStatus^.dwTerminalModesOffset);
    for i:=1 to DStatus^.dwTerminalModesSize do
    begin

      //FTerminalModes[i]:=IntToTermMode(DWord(Dummy[i]));
    end;
  end;
  FDevSpecificSize:=DStatus^.dwDevSpecificSize;
  if DStatus^.dwDevSpecificSize > 0 then
  begin
    SetLength(FDevSpecific,DStatus^.dwDevSpecificSize);
    StrCopy(PChar(FDevSpecific),PCHAR(DStatus)+DStatus^.dwDevSpecificOffset);
  end;
  {$IFDEF TAPI20}
  FAvailableMediaModes:=IntToMediaModes(DStatus^.dwAvailableMediaModes);
  if DStatus^.dwAppInfoSize >0 then
  begin
    if FAppInfo.Count > 0 then FAppInfo.Clear;
    for i:=1 to (DStatus^.dwAppInfoSize div SizeOf(TLineAppInfo)) do
    begin
      AInfoItem:=FAppInfo.Add;
      AInfoItem.SetInfo(PLineAppInfo(PChar(DStatus)+DStatus^.dwAppInfoOffset+(SizeOf(TLineAppInfo)* (i-1))),PChar(DStatus)) ;
    end;
  end;
  {$ENDIF}
end;


{ TLineDevCaps }

constructor TLineDeviceCaps.Create(ALineApp:HLineApp;ADeviceID,AAPIVersion,AExtVersion:DWord);
var R:LongInt;
    FLineDevCaps:PLineDevCaps;
begin
  inherited Create;
  FLineDevCaps:=AllocMem(SizeOf(TLineDevCaps)+1000);
  try
    FLineDevCaps^.dwTotalSize:=SizeOf(TLineDevcaps)+1000;
    R:=LineGetDevCaps(ALineApp,ADeviceID,AAPIVersion,AExtVersion,FLineDevCaps);
    if R<>0 then
    begin
      RaiseTAPILineError(R);
    end;
    SetCaps(FLineDevCaps);
    FDeviceID:=ADeviceID;
  finally
    FreeMem(FLineDevCaps);
  end;
end;

destructor TLineDeviceCaps.Destroy;
begin
  inherited Destroy;
end;

{procedure TLineDeviceCaps.GetDevSpecific(var AStr: String);
var Contents: DWord;
    KeyOffset: DWord;
begin
  if FDevSpecificSize > 0 then
  begin
    if bmPassthrough in FBearerModes then
    begin
      Contents:=StrToInt(PChar(@FDevSpecific));
      KeyOffset:=StrToInt(PChar(@FDevSpecific)+5);
      AStr:=PChar(@FDevSpecific)+KeyOffset;
    end
    else
      AStr:=PChar(FDevSpecific);
  end;            
end;

procedure TLineDeviceCaps.GetDevSpecific(var AArray: array of byte);
begin
  if FDevSpecificSize > 0 then
    StrCopy(PChar(@AArray),PChar(@FDevSpecific));
end;   }

procedure TLineDeviceCaps.SetCaps;
type
  PDevSpec=^TDevSpec;
  TDevSpec= packed record
    case Integer of
      0: (
        Item1: Word;
        Item2: Word;
        Item3: Word;
        Item4: Word;);
      1: (
        dwContetent:DWord;
        dwKeyOffset:DWord;)
   end;

var Dummy,Dummy1,Dummy2:Array of char;
    Dummy3: TDevSpec;
    Offset:DWord;
    {$IFDEF TAPI20}
    S:String;
    {$ENDIF}
    i:Integer;
    ATermItem:TTermCapsItem;
    

begin
  FAddressModes:=IntToAddressModes(FLineDevCaps^.dwAddressModes);
  {$IFDEF TAPI30}
  FAddressTypes:=IntToAddressTypes(FLineDevCaps^.dwAddressTypes);
  {$ENDIF}
  FAnswerMode:=IntToAnswerMode(FLineDevCaps^.dwAnswerMode);
  {$IFDEF TAPI30}
  FAvailableTracking:=IntToCallHubTracking(FLineDevCaps^.dwAvailableTracking);
  {$ENDIF}
  FBearerModes:=IntToBearerModes(FLineDevCaps^.dwBearerModes);
  FDefaultDialParams:=FLineDevCaps^.DefaultDialParams;
  FDevCapFlags:=IntToDevCapFlags(FLineDevCaps^.dwDevCapFlags);
  {$IFDEF TAPI20}
  FDeviceClasses:=TStringList.Create;
  if (FLineDevCaps^.dwDeviceClassesSize > 0) and (FLineDevCaps^.dwDeviceClassesOffset < FLineDevCaps^.dwUsedSize) then
  begin
    Offset:=FLineDevCaps^.dwDeviceClassesOffset;
    while (Offset- FLineDevCaps^.dwDeviceClassesOffset)< FLineDevCaps^.dwDeviceClassesSize-1 do
    begin
      S:=GetDataFromTAPIStruct(FLineDevCaps,Offset,FLineDevCaps^.dwDeviceClassesSize);
      FDeviceClasses.Add(S);
      Offset:=Offset+DWord(Length(S))+1;
      Dummy:=nil;
    end;
  end;
  {$EndIF}
  FGaterDigitsMaxTimeout:=FLineDevCaps^.dwGatherDigitsMaxTimeout;
  FGaterDigitsMinTimeout:=FLineDevCaps^.dwGatherDigitsMinTimeout;
  FGenerateDigitModes:=IntToDigitModes(FLineDevCaps^.dwGenerateDigitModes);
  FGenerateToneMaxNumFreq:=FLineDevCaps^.dwGenerateToneMaxNumFreq;
  FGenerateToneModes:=IntToToneModes(FLineDevCaps^.dwGenerateToneModes);
  FLineFeatures:=IntToLineFeatures(FLineDevCaps^.dwLineFeatures);
  FLineName:=GetDataFromTAPIStruct(FLineDevCaps,FLineDevCaps^.dwLineNameOffset,FLineDevCaps^.dwLineNameSize);
  FLineStates:=IntToLineDevStates(FLineDevCaps^.dwLineStates);
  FMaxDialParams:=FLineDevCaps^.MaxDialParams;
  FMaxNumActiveCalls:=FLineDevCaps^.dwMaxNumActiveCalls;
  FMaxRate:=FLineDevCaps^.dwMaxRate;
  FMedCtlCallStateMaxListSize:=FLineDevCaps^.dwMedCtlCallStateMaxListSize;
  FMedCtlDigitMaxListSize:=FLineDevCaps^.dwMedCtlDigitMaxListSize;
  FMedCtlMediaMaxListSize:=FLineDevCaps^.dwMedCtlMediaMaxListSize;
  FMedCtlToneMaxListSize:=FLineDevCaps^.dwMedCtlToneMaxListSize;
  FMediaModes:=IntToMediaModes(FLineDevCaps^.dwMediaModes);
  FMinDialParams:=FLineDevCaps^.MinDialParams;
  FMonitorDigitModes:=IntToDigitModes(FLineDevCaps^.dwMonitorDigitModes);
  FMonitorToneMaxNumEntries:=FLineDevCaps^.dwMonitorToneMaxNumEntries;
  FMonitorToneMaxNumFreq:=FLineDevCaps^.dwMonitorToneMaxNumFreq;
  FNumAddresses:=FLineDevCaps^.dwNumAddresses;
  FNumTerminals:=FLineDevCaps^.dwNumTerminals;
  if Assigned(FTerminalCaps)=False then FTerminalCaps:=TTermCaps.Create;
  if FLineDevCaps^.dwTerminalCapsSize >0 then
  begin
    for i:=0 to (FNumTerminals -1) do
    begin
      SetLength(Dummy,FLineDevCaps^.dwTerminalCapsSize+1);
      SetLength(Dummy1,FLineDevCaps^.dwTerminalCapsSize+1);
      SetLength(Dummy2,FLineDevCaps^.dwTerminalCapsSize+1);
      StrCopy(PChar(Dummy),PCHAR(FLineDevCaps)+FLineDevCaps^.dwTerminalCapsOffset+(i*12));
      StrCopy(PChar(Dummy1),PCHAR(FLineDevCaps)+FLineDevCaps^.dwTerminalCapsOffset+(i*12)+4);
      StrCopy(PChar(Dummy2),PCHAR(FLineDevCaps)+FLineDevCaps^.dwTerminalCapsOffset+(i*12)+8);
      ATermItem:=TTermCapsItem.Create(DWord(Dummy[i]),DWord(Dummy[i+1]),DWord(Dummy[i+2]));
      if Assigned(FTerminalCaps)=False then FTerminalCaps:=TTermCaps.Create;
      FTerminalCaps.Add(ATermItem);
      Dummy:=nil;
      Dummy1:=nil;
      Dummy2:=nil;
    end;
  end;

  //s1:=GetDataFromTAPIStruct(FLineDevCaps,FLineDevCaps^.dwDevSpecificOffset,FLineDevCaps^.dwDevSpecificSize);
  FDevSpecificSize:= FLineDevCaps^.dwDevSpecificSize;
  if FLineDevCaps^.dwDevSpecificSize > 0 then
  begin
    SetLength(FDevSpecific,FLineDevCaps^.dwDevSpecificSize+1);
    //SetLength(Dummy3,FLineDevCaps^.dwDevSpecificSize);
    //for i:=0 to (FDevSpecificSize div 4) do
    //  StrCopy(PChar(@Dummy3),PChar(FLineDevCaps)+FLineDevCaps^.dwDevSpecificOffset+(i*4));
    Dummy3:=PDevSpec(GetDataFromTAPIStructP(FLineDevCaps,FLineDevCaps^.dwDevSpecificOffset,FLineDevCaps^.dwDevSpecificSize))^;
    Offset:= FLineDevCaps^.dwDevSpecificOffset+Dummy3.dwKeyOffset;
    FDevSpecific:=GetDataFromTAPIStruct(FLineDevCaps,Offset,FLineDevCaps^.dwDevSpecificSize);
  end;
  {$IFDEF TAPI22}
  FPermanentLineGuid:= FLineDevCaps^.PermanentLineGuid;
  {$ENDIF}
  FPermanentLineID:=FLineDevCaps^.dwPermanentLineID;
  {$IFDEF TAPI30}
  FProtocolGuid:=FLineDevCaps^.ProtocolGuid;
  {$ENDIF}
  FProviderInfo:=GetDataFromTAPIStruct(FLineDevCaps,FLineDevCaps^.dwProviderInfoOffset,FLineDevCaps^.dwProviderInfoSize);
  FRingModes:=FLineDevCaps^.dwRingModes;
  {$IFDEF TAPI20}
  FSettableDevStatus:=IntToDevStatusFlags(FLineDevCaps^.dwSettableDevStatus);
  {$ENDIF}
  FStringFormat:=IntToStringFormat(FLineDevCaps^.dwStringFormat);
  //SetLength(Dummy,FLineDevCaps.dwSwitchInfoSize);
  //If FLineDevCaps^.dwSwitchInfoSize >0 then
  //StrCopy(PChar(Dummy),PCHAR(FLineDevCaps)+);
  FSwitchInfo:=GetDataFromTAPIStruct(FLineDevCaps,FLineDevCaps^.dwSwitchInfoOffset,FLineDevCaps.dwSwitchInfoSize);
  //Dummy:=nil;
  if Assigned(FTerminalText)=False then FTerminalText:=TStringList.Create;
  if FLineDevCaps^.dwTerminalTextSize > 0 then
  begin
    Offset:=0;
    while (Offset < FLineDevCaps^.dwTerminalTextSize)do
    begin
      SetLength(Dummy,FLineDevCaps^.dwTerminalTextSize);
      StrCopy(PChar(Dummy),PCHAR(FLineDevCaps)+FLineDevCaps^.dwTerminalTextOffset);
      FTerminalText.Add(PChar(Dummy));
      Offset:=Offset+StrLen(PChar(Dummy))+1;
    end;
  end;
  FUUIAcceptSize:=FLineDevCaps^.dwUUIAcceptSize;
  FUUIAnswerSize:=FLineDevCaps^.dwUUIAnswerSize;
  FUUICallInfoSize:=FLineDevCaps^.dwUUICallInfoSize;
  FUUIDropSize:=FLineDevCaps^.dwUUIDropSize;
  FUUIMakeCallSize:=FLineDevCaps^.dwUUIMakeCallSize;
  FUUISendUserUserInfoSize:=FLineDevCaps^.dwUUISendUserUserInfoSize;
end;


{ TTAPIDevice }

constructor TTAPIDevice.Create(AOwner: TComponent);
begin
  inherited;
  FNegotiateAPIVersion:=False;
  FNegotiateExtVersion:=False;
end;

destructor TTAPIDevice.Destroy;
begin
  inherited;

end;

procedure TTAPIDevice.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent,Operation);
  if (Operation=opRemove) and (AComponent=FService) then
    FService:=nil;
end;

procedure TTAPIDevice.SetDeviceClass(const Value: String);
begin
  FDeviceClass := Value;
end;

procedure TTAPIDevice.SetID(const Value: DWord);
begin
  if Value<>FID then
  begin
    If Assigned(FService) then
    begin
      if FService.Active then
      begin
        If Value > (FService.NumDevice -1)then
          RaiseTAPIError(TAPIERR_BADDEVICEID);
      end;
    end;
    FID := Value;
  end;
end;

procedure TTAPIDevice.SetService(const Value: TTAPICustomService);
begin
  FService := Value;
  if FService.Active=True then
  GetAPIVersion;
end;

{ TTAPILineDevice }

constructor TTAPILineDevice.Create(AOwner: TComponent);
begin
  inherited;
  DeviceClass:='tapi/line';
  FNegotiateAPIVersion:=False;
  FNegotiateExtVersion:=False;
end;

destructor TTAPILineDevice.Destroy;
begin
  FDevCaps.Free;
  inherited;
end;

function TTAPILineDevice.GetAPIIcon(DeviceClass:String;AIcon:TIcon):hIcon;
var R: Longint;
    Icon: hIcon;
    DClass: PChar;
begin
  Result:=0;
  if AIcon.Handle<>0 then AIcon.ReleaseHandle;
  if DeviceClass <> '' then
    DClass:=PChar(DeviceClass)
  else
    DClass:=nil;
  R:=LineGetIcon(FID,DClass,@Icon);
  if R <>0 then  RaiseTAPILineError(R)
  else
  begin
    Result:=Icon;
    AIcon.Handle:=CopyIcon(Icon);
  end;
end;

function TTAPILineDevice.GetAPIVersion: DWord;
begin
  If FNegotiateAPIVersion=False then
  begin
    if Assigned(FService) then
    begin
      FAPIVersion:=TTAPILineService(FService).GetAPIVersion(FID,FExtID);
      FNegotiateAPIVersion:=True;
    end;
  end;
  Result:=FAPIVersion;
end;

function TTAPILineDevice.GetCaps: TLineDeviceCaps;
begin
  try
    if (Assigned(FDevCaps) and (FDevCaps.DeviceID <> FID)) then
    begin
      FreeAndNil(FDevCaps);
    end;
    if Assigned(FDevCaps)=False then
    begin
      if Assigned(FService)=False then FService:=TTAPILineService.Create(Owner);
      FDevCaps:=TLineDeviceCaps.Create(FService.Handle,FID,APIVersion,ExtVersion);
   end;
  except
    On EAccessViolation do
    begin
      if not(csDesigning in ComponentState) then Application.MessageBox('TTAPILineDevice no Service','Error',IDOK);
    end;
  end;
  Result:=FDevCaps;
end;

function TTAPILineDevice.GetDeviceConfig: TTAPILineDeviceConfig;
begin
  Result:=FDeviceConfig;
end;

function TTAPILineDevice.GetExtVersion: DWord;
begin
  If FNegotiateExtVersion=False then
  begin
    if Assigned(FService) then
    begin
      try
        FExtVersion:=TTAPILineService(FService).GetExtVersion(FID,APIVersion,FExtID);
      except
        On E:ELineError do
        begin
          if DWord(E.ErrorCode)=LINEERR_INCOMPATIBLEEXTVERSION then FNegotiateExtVersion:=False;
        end
      end;
    end;
    FNegotiateExtVersion:=True;
  end;
  Result:=FExtVersion;
end;

function TTAPILineDevice.GetStates: LongInt;
begin
  FDevStateMessages:=[];
  if Assigned(FOnStateBattery) then FDevStateMessages:=FDevStateMessages +[dsBattery];
  if Assigned(FOnStateOther) then FDevStateMessages:=FDevStateMessages +[dsOther];
  if Assigned(FOnStateRinging) then FDevStateMessages:=FDevStateMessages +[dsRinging];
  if Assigned(FOnStateConnected) then FDevStateMessages:=FDevStateMessages +[dsConnected];
  if Assigned(FOnStateDisconnected) then FDevStateMessages:=FDevStateMessages +[dsDisconnected];
  if Assigned(FOnStateMsgWaitOn) then FDevStateMessages:=FDevStateMessages +[dsMsgWaitOn];
  if Assigned(FOnStateMsgWaitOff) then FDevStateMessages:=FDevStateMessages +[dsMsgWaitOff];
  if Assigned(FOnStateInService) then FDevStateMessages:=FDevStateMessages +[dsInService];
  if Assigned(FOnStateOutOfService) then FDevStateMessages:=FDevStateMessages +[dsOutOfService];
  if Assigned(FOnStateMaintenance) then FDevStateMessages:=FDevStateMessages +[dsMaintenance];
  if Assigned(FOnStateOpen) then FDevStateMessages:=FDevStateMessages +[dsOpen];
  if Assigned(FOnStateClose) then FDevStateMessages:=FDevStateMessages +[dsClose];
  if Assigned(FOnStateNumCalls) then FDevStateMessages:=FDevStateMessages +[dsNumCalls];
  if Assigned(FOnStateNumCompletions) then FDevStateMessages:=FDevStateMessages +[dsNumCompletions];
  if Assigned(FOnStateTerminals) then FDevStateMessages:=FDevStateMessages +[dsTerminals];
  if Assigned(FOnStateRoamMode) then FDevStateMessages:=FDevStateMessages +[dsRoamMode];
  if Assigned(FOnStateSignal) then FDevStateMessages:=FDevStateMessages +[dsSignal];
  if Assigned(FOnStateDevSpecific) then FDevStateMessages:=FDevStateMessages +[dsDevSpecific];
  if Assigned(FOnStateReInit) then FDevStateMessages:=FDevStateMessages +[dsReInit];
  if Assigned(FOnStateLock) then FDevStateMessages:=FDevStateMessages +[dsLock];
  if Assigned(FOnStateReMoved) then FDevStateMessages:=FDevStateMessages +[dsReMoved];
  if Assigned(FOnStateCapsChange) then FDevStateMessages:=FDevStateMessages +[dsCapsChange];
  if Assigned(FOnStateConfigChange) then FDevStateMessages:=FDevStateMessages +[dsConfigChange];
  if Assigned(FOnStateTranslateChange) then FDevStateMessages:=FDevStateMessages +[dsTranslateChange];
  if Assigned(FOnStateComplCancel) then FDevStateMessages:=FDevStateMessages +[dsComplCancel];
  result:=DevStatesToInt(FDevStateMessages);
end;

function TTAPILineDevice.GetTranslateCaps: TTranslateCaps;
var Caps:PLINETRANSLATECAPS;
    Size:DWord;
    R:Longint;
label Init;
begin
  Size:=SizeOf(TLINETRANSLATECAPS)+1000;
  Init:
  Caps:=AllocMem(Size);
  Caps^.dwTotalSize:=Size;
  R:=LineGetTranslateCaps(FService.Handle,APIVersion,Caps);
  if (DWORD(R)=LINEERR_STRUCTURETOOSMALL) or (Caps^.dwNeededSize >Caps^.dwTotalSize) then
  begin
    size:=Caps^.dwNeededSize;
    FreeMem(Caps);
    goto  Init;
  end
  else
  begin
    if R> 0 then RaiseTAPILineError(R);
  end;
  result:=TTranslateCaps.Create(Caps);
  FreeMem(caps);
end;

procedure TTAPILineDevice.PerformMsg(Msg: TCMTAPI);
begin
  inherited;
  with Msg.TAPIRec^ do
  begin
    case dwMsg of
      Line_LineDevState:
      begin
        If dwCallBackInstance=0 then
        begin
          if dwParam1=LINEDEVSTATE_TRANSLATECHANGE then TranslateChange(hDevice);
          if dwParam1=LINEDEVSTATE_REINIT then if Assigned(FOnStateReInit) then FOnStateReInit(self);
        end
     end;
   end
 end;
end;

procedure TTAPILineDevice.SetDeviceClass(const Value: String);
begin
  if FDeviceClass <> Value then
  begin
    if Assigned(FDeviceConfig) then FDeviceConfig.DeviceClass:=Value;
  end;
  inherited;
end;

procedure TTAPILineDevice.SetDeviceConfig(const Value: TTAPILineDeviceConfig);
begin
  FDeviceConfig:=Value;
  FDeviceConfig.DeviceID:=FID;
  FDeviceConfig.DeviceClass:=FDeviceClass;
end;

procedure TTAPILineDevice.SetExtVersion;
begin

end;

procedure TTAPILineDevice.SetID(const Value: DWord);
begin
  if FID <> Value then
  begin
    FreeAndNil(FDevCaps);
    FNegotiateAPIVersion:=False;
    FNegotiateExtVersion:=False;
    if Assigned(FDeviceConfig) then FDeviceConfig.DeviceID:=Value;
    FLineMapper:=False;
  end;
  inherited;
end;

procedure TTAPILineDevice.StateChange(hDevice,Param1,Param2,Param3:LongWord);
begin
   case Param1 of
    LINEDEVSTATE_OTHER:if Assigned(FOnStateOther) then FOnStateOther(self);
    LINEDEVSTATE_RINGING:if Assigned(FOnStateRINGING) then FOnStateRINGING(self,Param2,Param3);
    LINEDEVSTATE_CONNECTED:if Assigned(FOnStateConnected) then FOnStateConnected(self);
    LINEDEVSTATE_DISCONNECTED:if Assigned(FOnStateDISCONNECTED) then FOnStateDISCONNECTED(self);
    LINEDEVSTATE_MSGWAITON:if Assigned(FOnStateMSGWAITON) then FOnStateMSGWAITON(self);
    LINEDEVSTATE_MSGWAITOFF:if Assigned(FOnStateMSGWAITOFF) then FOnStateMSGWAITOFF(self);
    LINEDEVSTATE_INSERVICE:if Assigned(FOnStateINSERVICE) then FOnStateINSERVICE(self);
    LINEDEVSTATE_OUTOFSERVICE:if Assigned(FOnStateOUTOFSERVICE) then FOnStateOUTOFSERVICE(self);
    LINEDEVSTATE_MAINTENANCE:if Assigned(FOnStateMAINTENANCE) then FOnStateMAINTENANCE(self);
    LINEDEVSTATE_OPEN:if Assigned(FOnStateOpen) then FOnStateOpen(self);
    LINEDEVSTATE_CLOSE:if Assigned(FOnStateClose) then FOnStateClose(self);
    LINEDEVSTATE_NUMCALLS:if Assigned(FOnStateNUMCALLS) then FOnStateNUMCALLS(self);
    LINEDEVSTATE_NUMCOMPLETIONS:if Assigned(FOnStateNUMCOMPLETIONS) then FOnStateNUMCOMPLETIONS(self);
    LINEDEVSTATE_TERMINALS:if Assigned(FOnStateTERMINALS) then FOnStateTERMINALS(self);
    LINEDEVSTATE_ROAMMODE:if Assigned(FOnStateROAMMODE) then FOnStateROAMMODE(self);
    LINEDEVSTATE_BATTERY:if Assigned(FOnStateBATTERY) then FOnStateBATTERY(self);
    LINEDEVSTATE_SIGNAL:if Assigned(FOnStateSIGNAL) then FOnStateSIGNAL(self);
    LINEDEVSTATE_DEVSPECIFIC:if Assigned(FOnStateDEVSPECIFIC) then FOnStateDEVSPECIFIC(self);
    LINEDEVSTATE_REINIT:if Assigned(FOnStateReInit) then FOnStateReInit(self);
    LINEDEVSTATE_LOCK:if Assigned(FOnStateLOCK) then FOnStateLOCK(self);
    LINEDEVSTATE_CAPSCHANGE:begin
                              FreeAndNil(FDevCaps);
                              FDevCaps:=TLineDeviceCaps.Create(FService.Handle,FID,APIVersion,ExtVersion);
                              if Assigned(FOnStateCapsChange) then FOnStateCapsChange(self);
                            end;
    LINEDEVSTATE_CONFIGCHANGE:if Assigned(FOnStateCONFIGCHANGE) then FOnStateCONFIGCHANGE(self);
    LINEDEVSTATE_TRANSLATECHANGE:if Assigned(FOnStateTRANSLATECHANGE) then FOnStateTRANSLATECHANGE(self);
    LINEDEVSTATE_COMPLCANCEL:if Assigned(FOnStateCOMPLCANCEL) then FOnStateCOMPLCANCEL(self);
    LINEDEVSTATE_REMOVED:if Assigned(FOnStateREMOVED) then FOnStateREMOVED(self);
  end;
end;

procedure TTAPILineDevice.TranslateChange(hDevice: LongWord);
begin
  if Assigned(FOnStateTranslateChange) then FOnStateTranslateChange(self);
end;


function PhoneStatesToInt(Value:TPhoneStates):LongWord;
begin
  Result:=0;
  IF psOther in Value then Result:=Result or PHONESTATE_OTHER;
  IF psConnected in Value then Result:=Result or PHONESTATE_CONNECTED;
  IF psDisconnected in Value then Result:=Result or PHONESTATE_DISCONNECTED;
  IF psOwner in Value then Result:=Result or PHONESTATE_OWNER;
  IF psMonitors in Value then Result:=Result or PHONESTATE_MONITORS;
  IF psDisplay in Value then Result:=Result or PHONESTATE_DISPLAY;
  IF psLamp in Value then Result:=Result or PHONESTATE_LAMP;
  IF psRingMode in Value then Result:=Result or PHONESTATE_RINGMODE;
  IF psRingVolume in Value then Result:=Result or PHONESTATE_RINGVOLUME;
  IF psHandsetHookswitch in Value then Result:=Result or PHONESTATE_HANDSETHOOKSWITCH;
  IF psHandsetVolume in Value then Result:=Result or PHONESTATE_HANDSETVOLUME;
  IF psHandsetGain in Value then Result:=Result or PHONESTATE_HANDSETGAIN;
  IF psSpeakerHookSwitch in Value then Result:=Result or PHONESTATE_SPEAKERHOOKSWITCH;
  IF psSpeakerVolume in Value then Result:=Result or PHONESTATE_SPEAKERVOLUME;
  IF psSpeakerGain in Value then Result:=Result or PHONESTATE_SPEAKERGAIN;
  IF psHeadsetHookSwitch in Value then Result:=Result or PHONESTATE_HEADSETHOOKSWITCH;
  IF psHeadsetVolume in Value then Result:=Result or PHONESTATE_HEADSETVOLUME;
  IF psHeadsetGain in Value then Result:=Result or PHONESTATE_HEADSETGAIN;
  IF psSuspend in Value then Result:=Result or PHONESTATE_SUSPEND;
  IF psResume in Value then Result:=Result or PHONESTATE_RESUME;
  IF psReInit in Value then Result:=Result or PHONESTATE_REINIT;
  IF psCapsChange in Value then Result:=Result or PHONESTATE_CAPSCHANGE;
  IF psReMoved in Value then Result:=Result or PHONESTATE_REMOVED;
end;

function IntToPhoneStates(Value:LongWord):TPhoneStates;
begin
  Result:=[];
  IF (Not((PHONESTATE_OTHER xor $FFFFFFFF) or Value ))=0 then Result:=Result+[psOther];
  IF (Not((PHONESTATE_CONNECTED xor $FFFFFFFF) or Value ))=0 then Result:=Result+[psConnected];
  IF (Not((PHONESTATE_DISCONNECTED xor $FFFFFFFF) or Value ))=0 then Result:=Result+[psDisconnected];
  IF (Not((PHONESTATE_OWNER xor $FFFFFFFF) or Value ))=0 then Result:=Result+[psOwner];
  IF (Not((PHONESTATE_MONITORS xor $FFFFFFFF) or Value ))=0 then Result:=Result+[psMonitors];
  IF (Not((PHONESTATE_DISPLAY xor $FFFFFFFF) or Value ))=0 then Result:=Result+[psDisplay];
  IF (Not((PHONESTATE_LAMP xor $FFFFFFFF) or Value ))=0 then Result:=Result+[psLamp];
  IF (Not((PHONESTATE_RINGMODE xor $FFFFFFFF) or Value ))=0 then Result:=Result+[psRingMode];
  IF (Not((PHONESTATE_RINGVOLUME xor $FFFFFFFF) or Value ))=0 then Result:=Result+[psRingVolume];
  IF (Not((PHONESTATE_HANDSETHOOKSWITCH xor $FFFFFFFF) or Value ))=0 then Result:=Result+[psHandsetHookswitch];
  IF (Not((PHONESTATE_HANDSETVOLUME xor $FFFFFFFF) or Value ))=0 then Result:=Result+[psHandsetVolume];
  IF (Not((PHONESTATE_HANDSETGAIN xor $FFFFFFFF) or Value ))=0 then Result:=Result+[psHandsetGain];
  IF (Not((PHONESTATE_SPEAKERHOOKSWITCH xor $FFFFFFFF) or Value ))=0 then Result:=Result+[psSpeakerHookSwitch];
  IF (Not((PHONESTATE_SPEAKERVOLUME xor $FFFFFFFF) or Value ))=0 then Result:=Result+[psSpeakerVolume];
  IF (Not((PHONESTATE_SPEAKERGAIN xor $FFFFFFFF) or Value ))=0 then Result:=Result+[psSpeakerGain];
  IF (Not((PHONESTATE_HEADSETHOOKSWITCH xor $FFFFFFFF) or Value ))=0 then Result:=Result+[psHeadsetHookSwitch];
  IF (Not((PHONESTATE_HEADSETVOLUME xor $FFFFFFFF) or Value ))=0 then Result:=Result+[psHeadsetVolume];
  IF (Not((PHONESTATE_HEADSETGAIN xor $FFFFFFFF) or Value ))=0 then Result:=Result+[psHeadsetGain];
  IF (Not((PHONESTATE_SUSPEND xor $FFFFFFFF) or Value ))=0 then Result:=Result+[psSuspend];
  IF (Not((PHONESTATE_RESUME xor $FFFFFFFF) or Value ))=0 then Result:=Result+[psResume];
  IF (Not((PHONESTATE_REINIT xor $FFFFFFFF) or Value ))=0 then Result:=Result+[psReInit];
  IF (Not((PHONESTATE_CAPSCHANGE xor $FFFFFFFF) or Value ))=0 then Result:=Result+[psCapsChange];
  IF (Not((PHONESTATE_REMOVED xor $FFFFFFFF) or Value ))=0 then Result:=Result+[psReMoved];
end;


function IntToPhoneButtonMode(Value:LongWord):TPhoneButtonMode;
begin
  Result:=pbmUnknown;
  if Value= PHONEBUTTONMODE_DUMMY then Result:=pbmDummy;
  if Value= PHONEBUTTONMODE_CALL then Result:=pbmCall;
  if Value= PHONEBUTTONMODE_FEATURE then Result:=pbmFeature;
  if Value= PHONEBUTTONMODE_KEYPAD then Result:=pbmKeypad;
  if Value= PHONEBUTTONMODE_LOCAL then Result:=pbmLocal;
  if Value= PHONEBUTTONMODE_DISPLAY then Result:=pbmDisplay;
end;

function IntToPhoneButtonFunction(Value:LongWord):TPhoneButtonFunction;
begin
  Result:=pbfUnknown;
  case Value of
     PHONEBUTTONFUNCTION_UNKNOWN:Result:=pbfUnknown;
     PHONEBUTTONFUNCTION_CONFERENCE:Result:=pbfConferenc;
     PHONEBUTTONFUNCTION_TRANSFER:Result:=pbfTransfer;
     PHONEBUTTONFUNCTION_DROP:Result:=pbfDrop;
     PHONEBUTTONFUNCTION_HOLD:Result:=pbfHold;
     PHONEBUTTONFUNCTION_RECALL:Result:=pbfReCall;
     PHONEBUTTONFUNCTION_DISCONNECT:Result:=pbfDisconnect;
     PHONEBUTTONFUNCTION_CONNECT:Result:=pbfConnect;
     PHONEBUTTONFUNCTION_MSGWAITON:Result:=pbfMsgWaitOn;
     PHONEBUTTONFUNCTION_MSGWAITOFF:Result:=pbfMsgWaitOff;
     PHONEBUTTONFUNCTION_SELECTRING:Result:=pbfSelectRing;
     PHONEBUTTONFUNCTION_ABBREVDIAL:Result:=pbfAbbrevDial;
     PHONEBUTTONFUNCTION_FORWARD:Result:=pbfForward;
     PHONEBUTTONFUNCTION_PICKUP:Result:=pbfPickup;
     PHONEBUTTONFUNCTION_RINGAGAIN:Result:=pbfRingAgain;
     PHONEBUTTONFUNCTION_PARK:Result:=pbfPark;
     PHONEBUTTONFUNCTION_REJECT:Result:=pbfReject;
     PHONEBUTTONFUNCTION_REDIRECT:Result:=pbfReDirect;
     PHONEBUTTONFUNCTION_MUTE:Result:=pbfMute;
     PHONEBUTTONFUNCTION_VOLUMEUP:Result:=pbfVolumeUp;
     PHONEBUTTONFUNCTION_VOLUMEDOWN:Result:=pbfVolumeDown;
     PHONEBUTTONFUNCTION_SPEAKERON:Result:=pbfSpeakerOn;
     PHONEBUTTONFUNCTION_SPEAKEROFF:Result:=pbfSpeakerOff;
     PHONEBUTTONFUNCTION_FLASH:Result:=pbfFlash;
     PHONEBUTTONFUNCTION_DATAON:Result:=pbfDataOn;
     PHONEBUTTONFUNCTION_DATAOFF:Result:=pbfDataOff;
     PHONEBUTTONFUNCTION_DONOTDISTURB:Result:=pbfDoNotDisturb;
     PHONEBUTTONFUNCTION_INTERCOM:Result:=pbfInterCom;
     PHONEBUTTONFUNCTION_BRIDGEDAPP:Result:=pbfBridgedApp;
     PHONEBUTTONFUNCTION_BUSY:Result:=pbfBusy;
     PHONEBUTTONFUNCTION_CALLAPP:Result:=pbfCallApp;
     PHONEBUTTONFUNCTION_DATETIME:Result:=pbfDateTime;
     PHONEBUTTONFUNCTION_DIRECTORY:Result:=pbfDirectory;
     PHONEBUTTONFUNCTION_COVER:Result:=pbfCover;
     PHONEBUTTONFUNCTION_CALLID:Result:=pbfCallId;
     PHONEBUTTONFUNCTION_LASTNUM:Result:=pbfLastNum;
     PHONEBUTTONFUNCTION_NIGHTSRV:Result:=pbfNightSrv;
     PHONEBUTTONFUNCTION_SENDCALLS:Result:=pbfSendCalls;
     PHONEBUTTONFUNCTION_MSGINDICATOR:Result:=pbfMsgIndicator;
     PHONEBUTTONFUNCTION_REPDIAL:Result:=pbfRepDial;
     PHONEBUTTONFUNCTION_SETREPDIAL:Result:=pbfSetRepDial;
     PHONEBUTTONFUNCTION_SYSTEMSPEED:Result:=pbfSystemSpeed;
     PHONEBUTTONFUNCTION_STATIONSPEED:Result:=pbfStationSpeed;
     PHONEBUTTONFUNCTION_CAMPON:Result:=pbfCampOn;
     PHONEBUTTONFUNCTION_SAVEREPEAT:Result:=pbfSaveRepead;
     PHONEBUTTONFUNCTION_QUEUECALL:Result:=pbfQueueCall;
     PHONEBUTTONFUNCTION_NONE:Result:=pbfNone;
  end;
end;

function IntToPhoneHookSwitchMode(Value:LongWord):TPhoneHookSwitchMode;
begin
  Result:=phsmUnknown;
  case Value of
    PHONEHOOKSWITCHMODE_ONHOOK:Result:=phsmOnHook;
    PHONEHOOKSWITCHMODE_MIC:Result:=phsmMic;
    PHONEHOOKSWITCHMODE_SPEAKER:Result:=phsmSpeaker;
    PHONEHOOKSWITCHMODE_MICSPEAKER:Result:=phsmMicSpeaker;
    PHONEHOOKSWITCHMODE_UNKNOWN:Result:=phsmUnknown;
  end;
end;

function IntToPhoneStatusFlags(Value:LongWord):TPhoneStatusFlags;
begin
  Result:=psfUnknown;
  case Value of
    PHONESTATUSFLAGS_CONNECTED:Result:=psfConnected;
    PHONESTATUSFLAGS_SUSPENDED:Result:=psfSuspended;
  end;
end;

function PhoneHookSwitchModeToInt(Value:TPhoneHookSwitchMode):LongWord;
begin
  Result:=0;
  case Value of
    phsmOnHook:Result:=PHONEHOOKSWITCHMODE_ONHOOK;
    phsmMic:Result:=PHONEHOOKSWITCHMODE_MIC;
    phsmSpeaker:Result:=PHONEHOOKSWITCHMODE_SPEAKER;
    phsmMicSpeaker:Result:=PHONEHOOKSWITCHMODE_MICSPEAKER;
    phsmUnknown:Result:=PHONEHOOKSWITCHMODE_UNKNOWN;
  end;
end;

{$IFDEF TAPI20}
function IntToPhoneFeatures(Value:LongWord):TPhoneFeatures;
begin
  Result:=[];
  IF (Not((PHONEFEATURE_GETBUTTONINFO xor $FFFFFFFF) or Value ))=0 then Result:=Result+[pfGetButtonInfo];
  IF (Not((PHONEFEATURE_GETDATA xor $FFFFFFFF) or Value ))=0 then Result:=Result+[pfGetData];
  IF (Not((PHONEFEATURE_GETDISPLAY xor $FFFFFFFF) or Value ))=0 then Result:=Result+[pfGetDispaly];
  IF (Not((PHONEFEATURE_GETGAINHANDSET xor $FFFFFFFF) or Value ))=0 then Result:=Result+[pfGetGainHandset];
  IF (Not((PHONEFEATURE_GETGAINSPEAKER xor $FFFFFFFF) or Value ))=0 then Result:=Result+[pfGetGainSpeaker];
  IF (Not((PHONEFEATURE_GETGAINHEADSET xor $FFFFFFFF) or Value ))=0 then Result:=Result+[pfGetGainHeadset];
  IF (Not((PHONEFEATURE_GETHOOKSWITCHHANDSET xor $FFFFFFFF) or Value ))=0 then Result:=Result+[pfGetHookSwitchHandset];
  IF (Not((PHONEFEATURE_GETHOOKSWITCHSPEAKER xor $FFFFFFFF) or Value ))=0 then Result:=Result+[pfGetHookSwitchSpeaker];
  IF (Not((PHONEFEATURE_GETHOOKSWITCHHEADSET xor $FFFFFFFF) or Value ))=0 then Result:=Result+[pfGetHookSwitchHeadset];
  IF (Not((PHONEFEATURE_GETLAMP xor $FFFFFFFF) or Value ))=0 then Result:=Result+[pfGetLamp];
  IF (Not((PHONEFEATURE_GETRING  xor $FFFFFFFF) or Value ))=0 then Result:=Result+[pfGetRing];
  IF (Not((PHONEFEATURE_GETVOLUMEHANDSET xor $FFFFFFFF) or Value ))=0 then Result:=Result+[pfGetVolumeHandset];
  IF (Not((PHONEFEATURE_GETVOLUMESPEAKER xor $FFFFFFFF) or Value ))=0 then Result:=Result+[pfGetVolumeSpeaker];
  IF (Not((PHONEFEATURE_GETVOLUMEHEADSET xor $FFFFFFFF) or Value ))=0 then Result:=Result+[pfGetVolumeHeadset];
  IF (Not((PHONEFEATURE_SETBUTTONINFO xor $FFFFFFFF) or Value ))=0 then Result:=Result+[pfSetButtonInfo];
  IF (Not((PHONEFEATURE_SETDATA xor $FFFFFFFF) or Value ))=0 then Result:=Result+[pfSetData];
  IF (Not((PHONEFEATURE_SETDISPLAY xor $FFFFFFFF) or Value ))=0 then Result:=Result+[pfSetDisplay];
  IF (Not((PHONEFEATURE_SETGAINHANDSET xor $FFFFFFFF) or Value ))=0 then Result:=Result+[pfSetGainHandset];
  IF (Not((PHONEFEATURE_SETGAINSPEAKER xor $FFFFFFFF) or Value ))=0 then Result:=Result+[pfSetGainSpeaker];
  IF (Not((PHONEFEATURE_SETGAINHEADSET xor $FFFFFFFF) or Value ))=0 then Result:=Result+[pfSetGainHeadset];
  IF (Not((PHONEFEATURE_SETHOOKSWITCHHANDSET xor $FFFFFFFF) or Value ))=0 then Result:=Result+[pfSetHookSwitchHandset];
  IF (Not((PHONEFEATURE_SETHOOKSWITCHSPEAKER xor $FFFFFFFF) or Value ))=0 then Result:=Result+[pfSetHookSwitchSpeaker];
  IF (Not((PHONEFEATURE_SETHOOKSWITCHHEADSET xor $FFFFFFFF) or Value ))=0 then Result:=Result+[pfSetHookSwitchHeadset];
  IF (Not((PHONEFEATURE_SETLAMP xor $FFFFFFFF) or Value ))=0 then Result:=Result+[pfSetLamp];
  IF (Not((PHONEFEATURE_SETRING xor $FFFFFFFF) or Value ))=0 then Result:=Result+[pfSetRing];
  IF (Not((PHONEFEATURE_SETVOLUMEHANDSET xor $FFFFFFFF) or Value ))=0 then Result:=Result+[pfSetVolumeHandset];
  IF (Not((PHONEFEATURE_SETVOLUMESPEAKER xor $FFFFFFFF) or Value ))=0 then Result:=Result+[pfSetVolumeSpeaker];
  IF (Not((PHONEFEATURE_SETVOLUMEHEADSET xor $FFFFFFFF) or Value ))=0 then Result:=Result+[pfSetVolumeHeadset];
end;
{$ENDIF}

function IntToPhoneHookSwitchModes(Value:LongWord):TPhoneHookSwitchModes;
begin
  Result:=[];
  IF (Not((PHONEHOOKSWITCHMODE_ONHOOK xor $FFFFFFFF) or Value ))=0 then Result:=Result+[phsmOnHook];
  IF (Not((PHONEHOOKSWITCHMODE_MIC xor $FFFFFFFF) or Value ))=0 then Result:=Result+[phsmMic];
  IF (Not((PHONEHOOKSWITCHMODE_SPEAKER xor $FFFFFFFF) or Value ))=0 then Result:=Result+[phsmSpeaker];
  IF (Not((PHONEHOOKSWITCHMODE_MICSPEAKER xor $FFFFFFFF) or Value ))=0 then Result:=Result+[phsmMicSpeaker];
  IF (Not((PHONEHOOKSWITCHMODE_UNKNOWN xor $FFFFFFFF) or Value ))=0 then Result:=Result+[phsmUnknown];
end;

function IntToPhoneHookSwitchDevs(Value:LongWord):TPhoneHookSwitchDevs;
begin
  Result:=[];
  IF (Not((PHONEHOOKSWITCHDEV_HANDSET xor $FFFFFFFF) or Value ))=0 then Result:=Result+[phsdHandset];
  IF (Not((PHONEHOOKSWITCHDEV_SPEAKER xor $FFFFFFFF) or Value ))=0 then Result:=Result+[phsdSpeaker];
  IF (Not((PHONEHOOKSWITCHDEV_HEADSET xor $FFFFFFFF) or Value ))=0 then Result:=Result+[phsdHeadset];
end;

{ TPhoneDeviceCaps }

constructor TPhoneDeviceCaps.Create(APhoneApp: HPhoneApp; ADeviceID,
  AAPIVersion, AExtVersion: DWord);
begin
  inherited Create;
  FPhoneApp:=APhoneApp;
  FDeviceID:=ADeviceID;
  FAPIVersion:=AAPIVersion;
  FExtVersion:=AExtVersion;
  Update;
end;

destructor TPhoneDeviceCaps.Destroy;
begin
  FButtonLamps.Free;
  FSetData.Free;
  FGetData.Free;
  inherited;
end;

function TPhoneDeviceCaps.GetProviderID: DWord;
begin
  result:=DWord(FPermanentPhoneID shr 16);
end;

procedure TPhoneDeviceCaps.SetCaps(FPhoneCaps: PPhoneCaps);
var Dummy:Array of char;
    i:Integer;
    AButtonLamp:TPhoneButtonLamp;
    AData:TPhoneDataArea;
    Offset:DWord;
    {$IFDEF TAPI20}
    S:String;
    {$ENDIF}
    FVolumeFlages,FGainFlags:TPhoneHookSwitchDevs;
begin
  SetLength(Dummy,FPhoneCaps^.dwProviderInfoSize);
  StrCopy(PChar(Dummy),PChar(FPhoneCaps)+FPhoneCaps^.dwProviderInfoOffset);
  FProviderInfo:=PChar(Dummy);
  // The FProviderInfo property provides information about the provider hardware
  // and/or software, such as the vendor name and version numbers of hardware
  // and software. This information can be useful when a user needs to call
  // customer service with problems regarding the provider.
  If FPhoneCaps^.dwPhoneInfoSize >0 then
  begin
    SetLength(Dummy,FPhoneCaps^.dwPhoneInfoSize);
    StrCopy(Pchar(Dummy),PCHAR(FPhoneCaps)+FPhoneCaps^.dwPhoneInfoOffset);
    FPhoneInfo:=Pchar(Dummy);
  end
  else FPhoneInfo:='';
  // The FPhonInfo property member provides information about the attached
  // phone device, such as the phone device manufacturer, the model name,
  // the software version, and so on. This information can be useful when a user
  // needs to call customer service with problems regarding the phone.
  FPermanentPhoneID:=FPhoneCaps^.dwPermanentPhoneID;
  // The permanent DWORD identifier by which the phone device is known in
  // the system's configuration.
  if FPhoneCaps^.dwPhoneNameSize > 0 then
  begin
    SetLength(Dummy,FPhoneCaps^.dwPhoneNameSize);
    StrCopy(PChar(Dummy),PChar(FPhoneCaps)+FPhoneCaps^.dwPhoneNameOffset);
    FPhoneName:=Pchar(Dummy);
    if FPhoneName = '' then FPhoneName:='Phone #';
  end
  else FPhoneName:='';
  // The FPhoneName containing a user configurable name for this phone device.
  // This name can be configured by the user when configuring the
  // phone device's service provider and is provided for the user's convenience.
  FStringFormat:=IntToStringFormat(FPhoneCaps^.dwStringFormat);
  // The string format to be used with this phone device
  FPhoneStates:=IntToPhoneStates(FPhoneCaps^.dwPhoneStates);
  // The state changes for this phone device for which the application can be
  // notified in a TTAPIPhone.OnStatexxxx message.
  FHookSwitchDevs:=IntToPhoneHookSwitchDevs(FPhoneCaps^.dwHookSwitchDevs);
  //Specifies the phone's hookswitch devices.
  // This member uses one of the PHONEHOOKSWITCHDEV_ Constants.(MS-PSDK)
  // Some Serviceproviders uses one or more of THookSwitchDevs
  FHandsetHookSwitchModes:=IntToPhoneHookSwitchModes(FPhoneCaps^.dwHookSwitchDevs);
  FSpeakerHookSwitchModes:=IntToPhoneHookSwitchModes(FPhoneCaps^.dwSpeakerHookSwitchModes);
  FHeadsetHookSwitchModes:=IntToPhoneHookSwitchModes(FPhoneCaps^.dwHeadsetHookSwitchModes);
  // Specifies the phone's hookswitch mode capabilities of the handset, speaker,
  // or headset, respectively. The member is only meaningful if
  // the hookswitch device is listed in FHookSwitchDevs. !!
  FVolumeFlages:=IntToPhoneHookSwitchDevs(FPhoneCaps^.dwVolumeFlags);
  if phsdHandset in FVolumeFlages then  FHandsetHookSwitchVolumeFlag:=True else FHandsetHookSwitchVolumeFlag:=False;
  if phsdSpeaker in FVolumeFlages then  FSpeakerHookSwitchVolumeFlag:=True else FSpeakerHookSwitchVolumeFlag:=False;
  if phsdHeadset in FVolumeFlages then  FHeadsetHookSwitchVolumeFlag:=True else FHeadsetHookSwitchVolumeFlag:=False;
  // Specifies the volume-setting capabilities of the phone device's
  // speaker components. If the property is TRUE, the volume of the
  // corresponding hookswitch device's speaker component can be adjusted
  // with phoneSetVolume (API function ).
  FGainFlags:=IntToPhoneHookSwitchDevs(FPhoneCaps^.dwGainFlags);
  if phsdHandset in FGainFlags then  FHandsetHookSwitchGainFlag:=True else FHandsetHookSwitchGainFlag:=False;
  if phsdSpeaker in FGainFlags then  FSpeakerHookSwitchGainFlag:=True else FSpeakerHookSwitchGainFlag:=False;
  if phsdHeadset in FGainFlags then  FHeadsetHookSwitchGainFlag:=True else FHeadsetHookSwitchGainFlag:=False;
  // Specifies the gain-setting capabilities of the phone device's
  // microphone components. If the property is TRUE, the volume of the
  // corresponding hookswitch device's microphone component can be adjusted
  // with phoneSetGain (API function).
  FDisplayNumRows:=FPhoneCaps^.dwDisplayNumRows;
  FDisplayNumColumns:=FPhoneCaps^.dwDisplayNumColumns;
  // Specifies the display capabilities of the phone device by describing the
  // number of columns and the number of rows in the phone display.
  // The FDisplayNumRows and FDisplayNumColumns propertys are both zero for
  // a phone device without a display.
  FNumRingModes:=FPhoneCaps^.dwNumRingModes;
  // The ring capabilities of the phone device. The phone is able to ring with
  // FNumRingModes different ring patterns, identified as 1, 2, through
  // FNumRingModes minus one. If the value of this member is 0, applications
  // have no control over the ring mode of the phone. If the value of this
  // member is greater than 0, it indicates the number of ring modes in addition
  // to silence that are supported by the service provider.
  FNumButtonLamps:=FPhoneCaps^.dwNumButtonLamps;
  //Specifies the number of button/lamps on the phone device that are detectable
  // in TAPI. Button/lamps are identified by their identifier.
  // Valid button/lamp identifiers range from zero to FNumButtonLamps minus one.
  // The keypad buttons '0', through '9', '*', and '#' are assigned the
  // identifiers 0 through 12.
  if FPhoneCaps^.dwNumButtonLamps >0 then
  begin
    Offset:=0;
    for i:=0 to FNumButtonLamps-1 do
    begin
      AButtonLamp:=FButtonLamps.Add;
      // Create ButtonLamp Object
      if FPhoneCaps^.dwButtonModesSize >0 then
      begin
        SetLength(Dummy,FPhoneCaps^.dwButtonModesSize *4);
        StrCopy(PChar(Dummy),PCHAR(FPhoneCaps)+FPhoneCaps^.dwButtonModesOffset+Offset);
        AButtonLamp.ButtonMode:=IntToPhoneButtonMode(DWord(Dummy[0]));
      end;
      if FPhoneCaps^.dwButtonFunctionsSize >0 then
      begin
        SetLength(Dummy,FPhoneCaps^.dwButtonFunctionsSize);
        StrCopy(PChar(Dummy),PCHAR(FPhoneCaps)+FPhoneCaps^.dwButtonFunctionsOffset+Offset);
        AButtonLamp.ButtonFunction:=IntToPhoneButtonFunction(DWord(Dummy[0]));
      end;
      if FPhoneCaps^.dwLampModesSize >0 then
      begin
        SetLength(Dummy,1); //Max 1 Byte
        StrCopy(PChar(Dummy),PCHAR(FPhoneCaps)+FPhoneCaps^.dwLampModesOffset+Offset);
        AButtonLamp.LampModes:=IntToPhoneLampModes(DWord(Dummy[0]));
      end;
      Inc(Offset,4);
      AButtonLamp.ID:=i;
    end;
  end;
  // FButtonLamps containing the button/lamp modes/functions and Button/Lamp-ID
  // of the phone's buttons/lamps.
  // The collection is indexed by button/lamp identifier.
  FNumSetData:=FPhoneCaps^.dwNumSetData;
  // The number of different download areas in the phone device.
  // The different areas are referred to using
  // the data IDs 0, 1, , FNumSetData minus one.
  // If this member is zero, the phone does not support the download capability.
  if FPhoneCaps^.dwSetDataSize >0 then
  begin
    AData:=FSetData.Add;
    AData.Size:=FPhoneCaps^.dwSetDataSize div FNumSetData;
    for i:=0 to FPhoneCaps^.dwSetDataSize-1 do
    begin
      SetLength(Dummy,1);
      StrCopy(PChar(Dummy),PCHAR(FPhoneCaps)+FPhoneCaps^.dwSetDataOffset+i);
      AData.Data[i]:=Byte(Dummy[0]);
    end;
  end;
  //
  // This Feature ist now only tested with MS-ESP  (one Area)
  //
  FNumGetData:=FPhoneCaps^.dwNumGetData;
  // The number of different upload areas in the phone device.
  // The different areas are referred to using
  // the data IDs 0, 1, , FNumGetData minus one.
  // If this member is zero, the phone does not support the download capability.
  if FPhoneCaps^.dwGetDataSize >0 then
  begin
    AData:=FGetData.Add;
    AData.Size:=FPhoneCaps^.dwGetDataSize div FNumGetData;
    for i:=0 to FPhoneCaps^.dwGetDataSize-1 do
    begin
      SetLength(Dummy,1);
      StrCopy(PChar(Dummy),PCHAR(FPhoneCaps)+FPhoneCaps^.dwGetDataOffset+i);
      AData.Data[i]:=Byte(Dummy[0]);
    end;
  end;
  //
  // This Feature ist now only tested with MS-ESP  (one Area)
  //
  if FPhoneCaps^.dwDevSpecificSize > 0 then
  begin
    StrCopy(Pchar(Dummy),PCHAR(FPhoneCaps)+FPhoneCaps^.dwDevSpecificOffset);
    FDevSpecific:=PChar(Dummy);
  end else
  FDevSpecific:='';
   //
  {$IFDEF TAPI20}
  FDeviceClasses:=TStringList.Create;
  if FPhoneCaps^.dwDeviceClassesSize > 0 then
  begin
    Offset:=0;
    while Offset< FPhoneCaps^.dwDeviceClassesSize-1 do
    begin
      SetLength(Dummy,FPhoneCaps^.dwDeviceClassesSize);
      StrCopy(PChar(Dummy),PCHAR(FPhoneCaps)+FPhoneCaps^.dwDeviceClassesOffset+Offset);
      S:=PChar(Dummy);
      FDeviceClasses.Add(S);
      Offset:=Offset+StrLen(PChar(Dummy))+1;
      Dummy:=nil;
    end;
  end;
  FPhoneFeatures:=IntToPhoneFeatures(FPhoneCaps^.dwPhoneFeatures);
  FSettableHandsetHookSwitchModes:=IntToPhoneHookSwitchModes(FPhoneCaps^.dwSettableHandsetHookSwitchModes);
  FSettableSpeakerHookSwitchModes:=IntToPhoneHookSwitchModes(FPhoneCaps^.dwSettableSpeakerHookSwitchModes);
  FSettableHeadsetHookSwitchModes:=IntToPhoneHookSwitchModes(FPhoneCaps^.dwSettableHeadsetHookSwitchModes);
  FMonitoredHandsetHookSwitchModes:=IntToPhoneHookSwitchModes(FPhoneCaps^.dwMonitoredHandsetHookSwitchModes);
  FMonitoredSpeakerHookSwitchModes:=IntToPhoneHookSwitchModes(FPhoneCaps^.dwMonitoredSpeakerHookSwitchModes);
  FMonitoredHeadsetHookSwitchModes:=IntToPhoneHookSwitchModes(FPhoneCaps^.dwMonitoredHeadsetHookSwitchModes);

{$ENDIF}
{$IFDEF TAPI22}
  FPermanentPhoneGuid:=FPhoneCaps^.PermanentPhoneGuid;
{$ENDIF}
end;

procedure TPhoneDeviceCaps.Update;
var FPhoneDevCaps:PPhoneCaps;
    R:Longint;
begin
  if Assigned(FButtonLamps) then FButtonLamps.Clear
  else
    FButtonLamps:=TPhoneButtonLamps.Create(TPhoneButtonLamp);
  if Assigned(FSetData) then FSetData.Clear
  else
    FSetData:=TPhoneDataAreas.Create(TPhoneDataArea);
  if Assigned(FGetData) then FGetData.Clear
  else
    FGetData:=TPhoneDataAreas.Create(TPhoneDataArea);
  FPhoneDevCaps:=AllocMem(SizeOf(TPhoneCaps)+1000);
  try
    FPhoneDevCaps^.dwTotalSize:=SizeOf(TPhoneCaps)+1000;
    R:=PhoneGetDevCaps(FPhoneApp,FDeviceID,FAPIVersion,FExtVersion,FPhoneDevCaps);
    if R<>0 then
    begin
      RaiseTAPIPhoneError(R);
    end;
    SetCaps(FPhoneDevCaps);
  finally
    FreeMem(FPhoneDevCaps);
  end;
end;

{ TPhoneDeviceStatus }

constructor TPhoneDeviceStatus.Create(APhone: hPhone);
var Size:DWord;
    aDevStatus:PPhoneStatus;
    R:Longint;
begin
  inherited Create;
  FButtonLamps:=TPhoneButtonLamps.Create(TPhoneButtonLamp);
  Size:=SizeOf(TPhoneStatus)+1000;
  GetMem(aDevStatus,Size);
  try
    aDevStatus.dwTotalSize:=Size;
    R:=PhoneGetStatus(APhone,aDevStatus);
    if DWord(R)<> 0 then RaiseTAPIPhoneError(R);
    SetStatus(aDevStatus);
  finally
    FreeMem(aDevStatus);
  end;
end;

destructor TPhoneDeviceStatus.Destroy;
begin
  FButtonLamps.Free;
  inherited;
end;

procedure TPhoneDeviceStatus.SetStatus(DevStatus: PPhoneStatus);
var Dummy: Array of Char;
    AButtonLamp: TPhoneButtonLamp;
    i: DWord;
    Offset:DWord;
begin
  FStatusFlags:=IntToPhoneStatusFlags(DevStatus^.dwStatusFlags);
  FNumOwners:=DevStatus^.dwNumOwners;
  FNumMonitors:=DevStatus^.dwNumMonitors;
  FRingMode:=DevStatus^.dwRingMode;
  FRingVolume:=DevStatus^.dwRingVolume;
  FHandSetHookSwitchMode:=IntToPhoneHookSwitchMode(DevStatus^.dwHandsetHookSwitchMode);
  FHandSetVolume:=DevStatus^.dwHandsetVolume;
  FHandSetGain:=DevStatus^.dwHandsetGain;
  FSpeakerHookSwitchMode:=IntToPhoneHookSwitchMode(DevStatus^.dwSpeakerHookSwitchMode);
  FSpeakerVolume:=DevStatus^.dwSpeakerVolume;
  FSpeakerGain:=DevStatus^.dwSpeakerGain;
  FHeadSetHookSwitchMode:=IntToPhoneHookSwitchMode(DevStatus^.dwHeadsetHookSwitchMode);
  FHeadSetVolume:=DevStatus^.dwHeadsetVolume;
  FHeadSetGain:=DevStatus^.dwHeadsetGain;
  if DevStatus^.dwDisplaySize > 0 then
  begin
    SetLength(Dummy,DevStatus^.dwDisplaySize);
    StrCopy(PChar(Dummy),PChar(DevStatus)+DevStatus^.dwDisplayOffset);
    FDisplay:=PChar(Dummy);
  end
  else FDisplay:='';
  if DevStatus^.dwLampModesSize > 0 then
  begin
    Offset:=0;
    i:=0;
    while i <= (DevStatus^.dwLampModesSize div 4) do
    begin
      AButtonLamp:=FButtonLamps.Add;
      SetLength(Dummy,1);
      StrCopy(PChar(Dummy),PChar(DevStatus)+DevStatus^.dwLampModesOffset+Offset);
      AButtonLamp.ID:=i;
      AButtonLamp.LampModes:=IntToPhoneLampModes(DWord(Dummy[0]));
      Inc(i);
      Inc(Offset,4);
    end;
  end;
  
  if DevStatus^.dwOwnerNameSize > 0 then
  begin
    SetLength(Dummy,DevStatus^.dwOwnerNameSize);
    StrCopy(PChar(Dummy),PChar(DevStatus)+DevStatus^.dwOwnerNameOffset);
    FOwnerName:=PChar(Dummy);
  end
  else FOwnerName:='';
  if DevStatus^.dwDevSpecificSize > 0 then
  begin
    SetLength(Dummy,DevStatus^.dwDevSpecificSize);
    StrCopy(PChar(Dummy),PChar(DevStatus)+DevStatus^.dwDevSpecificOffset);
    FDevSpecific:=PChar(Dummy);
  end
  else FDevSpecific:='';
  {$IFDEF TAPI20}
  FFeatures:=IntToPhoneFeatures(DevStatus^.dwPhoneFeatures);
  {$ENDIF}
end;

{ TTAPIPhoneDevice }

constructor TTAPIPhoneDevice.Create(AOwner: TComponent);
begin
  inherited;
  DeviceClass:='tapi/phone';
end;

destructor TTAPIPhoneDevice.Destroy;
begin
  inherited;
end;

function TTAPIPhoneDevice.GetAPIIcon(DeviceClass:String;AIcon:TIcon):hIcon;
var R:Longint;
    Icon:hIcon;

begin
  Result:=0;
  if AIcon.Handle<>0 then AIcon.ReleaseHandle;
  R:=PhoneGetIcon(FID,PChar(DeviceClass),Icon);
  if R <>0 then  RaiseTAPIPhoneError(R)
  else
  begin
    Result:=Icon;
    AIcon.Handle:=Icon;
  end;
end;

function TTAPIPhoneDevice.GetAPIVersion: DWord;
begin
  If FNegotiateAPIVersion=False then
  begin
    if Assigned(FService) then
    begin
      FAPIVersion:=TTAPIPhoneService(FService).GetAPIVersion(FID,FExtID);
      FNegotiateAPIVersion:=True;
    end;
  end;
  Result:=FAPIVersion;
end;

function TTAPIPhoneDevice.GetCaps: TPhoneDeviceCaps;
begin
  try
    if (Assigned(FDevCaps) and (FDevCaps.DeviceID <> FID)) then
    begin
      FreeAndNil(FDevCaps);
    end;
    if Assigned(FDevCaps)=False then
    begin
      if Assigned(FService)=False then FService:=TTAPIPhoneService.Create(Owner);
      FDevCaps:=TPhoneDeviceCaps.Create(FService.Handle,FID,APIVersion,ExtVersion);
    end;
  except
    On EAccessViolation do
    begin
      if not(csDesigning in ComponentState) then Application.MessageBox('TTAPILineDevice no Service','Error',IDOK);
    end;
  end;
  Result:=FDevCaps;
end;

function TTAPIPhoneDevice.GetExtVersion: DWord;
begin
  If FNegotiateExtVersion=False then
  begin
    if Assigned(FService) then
    begin
      try
        FExtVersion:=TTAPIPhoneService(FService).GetExtVersion(FID,APIVersion,FExtID);
      except
        On E:ELineError do
        begin
          if DWord(E.ErrorCode)=LINEERR_INCOMPATIBLEEXTVERSION then FNegotiateExtVersion:=False;
        end
      end;
    end;
    FNegotiateExtVersion:=True;
  end;
  Result:=FExtVersion;
end;

procedure TTAPIPhoneDevice.PerformMsg(Msg: TCMTAPI);
begin
  // do nothing !!
end;

procedure TTAPIPhoneDevice.SetExtVersion;
var R:Longint;
begin
  R:=PhoneNegotiateExtVersion(TTAPIPhoneService(FService).Handle,FID,APIVersion,FExtLoVer,FExtHiVer,FExtVersion);
  if R <>0 then  RaiseTAPIPhoneError(R);
end;

procedure TTAPIPhoneDevice.SetID(const Value: DWord);
begin
  if FID <> Value then
  begin
    FreeAndNil(FDevCaps);
    FNegotiateAPIVersion:=False;
    FNegotiateExtVersion:=False;
  end;
  inherited;
end;


{ TPhoneLamps }

function TPhoneButtonLamps.Add: TPhoneButtonLamp;
begin
  result:=TPhoneButtonLamp(inherited Add);
end;

constructor TPhoneButtonLamps.Create(ItemClass: TCollectionItemClass);
begin
  inherited Create(ItemClass);
end;

destructor TPhoneButtonLamps.Destroy;
begin
  inherited;
end;

function TPhoneButtonLamps.GetItem(Index: Integer): TPhoneButtonLamp;
begin
  result:=TPhoneButtonLamp(inherited GetItem(Index));
end;

procedure TPhoneButtonLamps.SetItem(Index: Integer; const Value: TPhoneButtonLamp);
begin
  inherited SetItem(Index,Value);
end;

{ TPhoneLamp }

constructor TPhoneButtonLamp.Create(Collection: TCollection);
begin
  inherited Create(Collection);
end;

destructor TPhoneButtonLamp.Destroy;
begin
  inherited;
end;

{ TPhoneDataAreas }

function TPhoneDataAreas.Add: TPhoneDataArea;
begin
  result:=TPhoneDataArea(inherited Add);
end;

constructor TPhoneDataAreas.Create(ItemClass: TCollectionItemClass);
begin
  inherited Create(ItemClass);
end;

destructor TPhoneDataAreas.Destroy;
begin
  inherited;

end;

function TPhoneDataAreas.GetItem(Index: Integer): TPhoneDataArea;
begin
  result:=TPhoneDataArea(inherited GetItem(Index));
end;

procedure TPhoneDataAreas.SetItem(Index: Integer;
  const Value: TPhoneDataArea);
begin
  inherited SetItem(Index,Value);
end;

{ TPhoneDataArea }

constructor TPhoneDataArea.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FSize:=0;
end;

destructor TPhoneDataArea.Destroy;
begin
  inherited;
end;

function TPhoneDataArea.GetData(Index: Integer): Byte;
begin
  result:=FData[Index];
end;

function TPhoneDataArea.GetSize: DWord;
begin
  result:=FSize;
end;

procedure TPhoneDataArea.SetData(Index: Integer; const Value: Byte);
begin
  FData[Index]:=Value;
end;

procedure TPhoneDataArea.SetSize(const Value: DWord);
begin
  SetLength(FData,Value);
  FSize:=Value;
end;

{ TAppInfoItem }

{$IFDEF TAPI20}
constructor TAppInfoItem.Create(Collection: TCollection);
begin
  inherited Create(Collection);
end;

destructor TAppInfoItem.Destroy;
begin
  inherited;
end;

procedure TAppInfoItem.SetInfo(AInfo: PLineAppInfo;Struct :PChar);
var Dummy:Array of char;
begin
  FAddressID:=AInfo^.dwAddressID;
  if AInfo^.dwMachineNameSize >0 then
  begin
    SetLength(Dummy,AInfo^.dwMachineNameSize);
    StrCopy(PChar(Dummy),Struct+AInfo^.dwMachineNameOffset);
    FMachineName:=PChar(Dummy);
  end
  else FMachineName:='';
  if AInfo^.dwFriendlyNameSize >0 then
  begin
    SetLength(Dummy,AInfo^.dwFriendlyNameSize);
    StrCopy(PChar(Dummy),Struct+AInfo^.dwFriendlyNameOffset);
    FFriendlyName:=PChar(Dummy);
  end
  else FFriendlyName:='';
  if AInfo^.dwUserNameSize >0 then
  begin
    SetLength(Dummy,AInfo^.dwUserNameSize);
    StrCopy(PChar(Dummy),Struct+AInfo^.dwUserNameOffset);
    FUserName:=PChar(Dummy);
  end
  else FUserName:='';
  if AInfo^.dwModuleFilenameSize >0 then
  begin
    SetLength(Dummy,AInfo^.dwModuleFilenameSize);
    StrCopy(PChar(Dummy),Struct+AInfo^.dwModuleFilenameOffset);
    FModuleFilename:=PChar(Dummy);
  end
  else FModuleFilename:='';
  FMediaModes:=IntToMediaModes(AInfo^.dwMediaModes);
end;

{ TAppInfos }

function TAppInfos.Add: TAppInfoItem;
begin
  result:=TAppInfoItem(inherited Add);
end;

constructor TAppInfos.Create(ItemClass: TCollectionItemClass);
begin
  inherited Create(ItemClass);
end;

destructor TAppInfos.Destroy;
begin
  inherited;
end;

function TAppInfos.GetItem(Index: Integer): TAppInfoItem;
begin
  result:=TAppInfoItem(inherited GetItem(Index));
end;

procedure TAppInfos.SetItem(Index: Integer; const Value: TAppInfoItem);
begin
  inherited SetItem(Index,Value);
end;
{$ENDIF}

{ TTermCapsItem }

constructor TTermCapsItem.Create(ATermDev,ATermMode,ATermSharing:DWord);
var ATermCaps:TLineTermCaps;
begin
  inherited Create;
  ATermCaps.dwTermDev:=ATermDev;
  ATermCaps.dwTermModes:=ATermMode;
  ATermCaps.dwTermSharing:=ATermSharing;
  GetCaps(ATermCaps);
end;

destructor TTermCapsItem.Destroy;
begin
  inherited;
end;

procedure TTermCapsItem.GetCaps(ATermCaps: TLineTermCaps);
begin
  FTermDev:=IntToTermDev(ATermCaps.dwTermDev);
  FTermMode:=IntToTermMode(ATermCaps.dwTermModes);
  FTermSharing:=IntToTermSharing(ATermCaps.dwTermSharing);
end;

{$ENDIF}
{$ENDIF}

end.
