{******************************************************************************}
{*        Copyright 1999-2001 by J.Friebel all rights reserved.               *}
{*        Autor           :  Jörg Friebel                                     *}
{*        Compiler        :  Delphi 4 / 5                                     *}
{*        System          :  Windows NT / 2000                                *}
{*        Projekt         :  TAPI Komponenten (TAPI Version 1.4 bis 3.0)      *}
{*        Last Update     :  23.07.2003                                       *}
{*        Version         :  3.8                                              *}
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

unit TAPIPhone;
interface
{$IFDEF WINDOWS}
{$IFDEF CPU32}

{$IFDEF FPC}
{$MODE DELPHI}
{$ENDIF}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  TAPIDevices,TAPI,TAPIHelpFunc,TAPIServices,TAPISystem;
  

{$INCLUDE TAPI.INC}


type
  TPhoneButtonState = (pbsUp,pbsDown,pbsUnknown,pbsUnavail);
  TPhoneButtonStates = Set of TPhoneButtonState;
  TPhonePrivilege  =(ppOwner,ppMonitor);
  TPhonePrivileges = Set of TPhonePrivilege;
 {$IFDEF TAPI20}
  TPhoneInitalizeExOption =(pieoUseHiddenWindow,pieoUseEvents,pieoUseCompletionPort);
 {$ENDIF}


  TTAPIPhone = Class;

  TPhoneDisplay = class(TPersistent)
  private
    FNumRows:Integer;
    FNumColumn:Integer;
    FOwner:TComponent;
    function GetNumColumn: Integer;
    function GetNumRows: Integer;
  protected
    function UpdateValue:Boolean;
  public
    constructor Create(AOwner:TComponent);reintroduce;
    destructor Destroy;override;
    function GetDisplay: String;
    procedure SetDisplay(Row,Column:Integer;Value: String);
  published
    property NumRows:Integer read GetNumRows ;
    property NumColumn:Integer read GetNumColumn ;
  end;

  TPhoneButton = class(TCollectionItem)
  private
    FID:DWord;
    FMode:TPhoneButtonMode;
    FFunction:TPhoneButtonFunction;
    FText:String;
    FDevSpecific:String;
    FState:TPhoneButtonState;
    procedure SetButtonInfo(BInfo:PPhoneButtonInfo);
  public
    //constructor Create(AOwner:TComponent;AButtonLampID:DWord);
    constructor Create(Collection: TCollection);override;
    destructor Destroy;override;
    property Mode:TPhoneButtonMode read FMode;
    property Func:TPhoneButtonFunction read FFunction;
    property Text:String read FText;
    property DevSpecific:String read FDevSpecific;
    property State:TPhoneButtonState read FState;
    property ID:DWord read FID;
  end;

  TPhoneButtons = class(TCollection)
  private
    function GetItem(Index:Integer): TPhoneButton;
    procedure SetItem(Index: Integer; const Value: TPhoneButton);
  public
    constructor Create(ItemClass: TCollectionItemClass);
    destructor Destroy;override;
    function Add:TPhoneButton;
    property Items[Index:Integer]:TPhoneButton read GetItem write SetItem;
  end;

  TPhoneEventStateOwner=procedure(Sender: TObject;NumOwner:DWord)of Object;
  TPhoneEventStateMonitors=procedure(Sender: TObject;NumMonitors:DWord)of Object;
  TPhoneEventStateLamp=procedure(Sender: TObject;ButtonLampID:DWord)of Object;
  TPhoneEventReply=procedure(Sender:TObject;AsyncFunc:TAsyncFunc;Error:Dword)of Object;
  TPhoneEventDevSpecific=procedure(Sender:TObject;dwParam1,dwParam2,dwParam3:DWord)of Object;
  TPhoneEventButton=procedure(Sender:TObject;ButtonLampID:DWord;Mode:TPhoneButtonMode;State:TPhoneButtonState)of Object;


  TTAPIPhone = class(TTAPIComponent)
  private
    FActive:Boolean;
    FPhoneHandle:HPHONE;
    FStateMessages:TPhoneStates;
    FPhonePrivilege:TPhonePrivilege;
    FDevStatus:TPhoneDeviceStatus;
    FPhoneDevice:TTAPIPhoneDevice;
    FDisplay:TPhoneDisplay;
    FButtons:TPhoneButtons;
    FButtonModesMessages:TPhoneButtonModes;
    FButtonStatesMessages:TPhoneButtonStates;
    FHandSetHookSwitchMode,
    FSpeakerHookSwitchMode,
    FHeadSetHookSwitchMode:TPhoneHookSwitchMode;
    //  Events
    FOnPhoneReply:TPhoneEventReply;
    FOnStateOther:TNotifyEvent;
    FOnStateConnected:TNotifyEvent;
    FOnStateDisConnected:TNotifyEvent;
    FOnStateOwner:TPhoneEventStateOwner;
    FOnStateMonitor:TPhoneEventStateMonitors;
    FOnStateDisplay:TNotifyEvent;
    FOnStateLamp:TPhoneEventStateLamp;
    FOnStateRingMode:TNotifyEvent;
    FOnStateRingVolume:TNotifyEvent;
    FOnStateHandSetHookSwitch:TNotifyEvent;
    FOnStateHandSetVolume:TNotifyEvent;
    FOnStateHandSetGain:TNotifyEvent;
    FOnStateSpeakerHookSwitch:TNotifyEvent;
    FOnStateSpeakerVolume:TNotifyEvent;
    FOnStateSpeakerGain:TNotifyEvent;
    FOnStateHeadSetHookSwich:TNotifyEvent;
    FOnStateHeadSetVolume:TNotifyEvent;
    FOnStateHeadSetGain:TNotifyEvent;
    FOnStateSuspend:TNotifyEvent;
    FOnStateResume:TNotifyEvent;
    FOnStateDevSpecific:TNotifyEvent;
    FOnStateReInit:TNotifyEvent;
    FOnStateCapsChange:TNotifyEvent;
    FOnStateRemoved:TNotifyEvent;
    FOnPhoneClose:TNotifyEvent;
    FOnPhoneButton:TPhoneEventButton;
    FOnDevSpecific:TPhoneEventDevSpecific;
    FAfterOpen: TNotifyEvent;
    FBeforeOpen: TNotifyEvent;
    procedure Open;
    procedure Close;
    function GetActive: Boolean;
    procedure SetActive(const Value: Boolean);
    function GetPhonePrivilege:LongWord;
    function GetHookSwitchDevs:TPhoneHookSwitchDevs;
    procedure SetHookSwitch(Index:Integer;Value:TPhoneHookSwitchMode);
    function GetDisplay: TPhoneDisplay;
    function GetButtons: TPhoneButtons;
    function GetGain(Index:Integer): Integer;
    procedure SetGain(const Index, Value: Integer);
    function GetHookSwitch(const Index:Integer): Boolean;
    function GetHookSwitchMode(const Index:Integer): TPhoneHookSwitchMode;
    function GetStatus: TPhoneDeviceStatus;
    function GetStateMessages:DWord;
    function GetLineID:DWord;
    function GetVolume(const Index: Integer): Integer;
    procedure SetVolume(const Index, Value: Integer);
  protected
    { Protected-Deklarationen }
    procedure GetID(var VarStr:TVarString;DeviceClass:PChar);
  public
    { Public-Deklarationen }
    constructor Create(AOwner:TComponent);override;
    destructor Destroy;override;
    procedure SetStatusMessages;
    procedure PhoneReply(AsyncFunc: TAsyncFunc; Error: DWord);
    procedure DevSpecificChange(dwParam1,dwParam2,dwParam3:DWord);
    procedure ButtonChange(ButtonLampID:DWord;Mode:TPhoneButtonMode;State:TPhoneButtonState);
    procedure GetLineIDs(var IDs: Array of DWord);
    //function SetHandSetMode(const Value: TPhoneHookSwitchMode):LongWord;
    procedure PhoneState(dwParam1,dwParam2:DWord);
    procedure PerformMsg(Msg :TCMTAPI); override;
    property Handle:THandle read FPhoneHandle write FPhoneHandle;
    property Active:Boolean read GetActive write SetActive;
    property HookSwitchDevs:TPhoneHookSwitchDevs read GetHookSwitchDevs;
    property Display:TPhoneDisplay read GetDisplay write FDisplay;
    property Buttons:TPhoneButtons read GetButtons;
    property HandSetGain:Integer index 1 read GetGain write SetGain nodefault;
    property HeadSetGain:Integer index 4 read GetGain write SetGain nodefault;
    property SpeakerGain:Integer index 2 read GetGain write SetGain nodefault;

    property HandSetVolume:Integer index 1 read GetVolume write SetVolume nodefault;
    property HeadSetVolume:Integer index 4 read GetVolume write SetVolume nodefault;
    property SpeakerVolume:Integer index 2 read GetVolume write SetVolume nodefault;

    property HandSetHookSwitchMode:TPhoneHookSwitchMode index 1 read GetHookSwitchMode write SetHookSwitch;
    property HeadSetHookSwitchMode:TPhoneHookSwitchMode index 4 read GetHookSwitchMode write SetHookSwitch;
    property SpeakerHookSwitchMode:TPhoneHookSwitchMode index 2 read GetHookSwitchMode write SetHookSwitch;

    property HandSetHookSwitchOffHook:Boolean index 1 read GetHookSwitch;
    property HeadSetHookSwitchOffHook:Boolean index 4 read GetHookSwitch;
    property SpeakerHookSwitchOffHook:Boolean index 2 read GetHookSwitch;
    property Status:TPhoneDeviceStatus  read GetStatus;
    procedure PhoneIsClose(hDevice:LongWord);
    property LineID:DWord read GetLineID;
  published
    { Published-Deklarationen }
    property Device:TTAPIPhoneDevice read FPhoneDevice write FPhoneDevice;
    property Privilege:TPhonePrivilege read FPhonePrivilege write FPhonePrivilege default ppOwner;
    property OnReply:TPhoneEventReply read FOnPhoneReply write FOnPhoneReply;
    //property StateMessages:TPhoneStates read FStateMessages write FStateMessages default [psReInit];
    property ButtonModeMessages:TPhoneButtonModes read FButtonModesMessages write FButtonModesMessages default [];
    property ButtonStateMessages:TPhoneButtonStates read FButtonStatesMessages write FButtonStatesMessages default [];
    property BeforeOpen: TNotifyEvent read FBeforeOpen write FBeforeOpen;
    property AfterOpen: TNotifyEvent read FAfterOpen write FAfterOpen;
    property OnStateOther:TNotifyEvent read FOnStateOther write FOnStateOther;
    property OnStateConnected:TNotifyEvent read FOnStateConnected write FOnStateConnected;
    property OnStateDisConnected:TNotifyEvent read FOnStateDisConnected write FOnStateDisConnected;
    property OnStateOwner:TPhoneEventStateOwner read FOnStateOwner write FOnStateOwner;
    property OnStateMonitor:TPhoneEventStateMonitors read FOnStateMonitor write FOnStateMonitor;
    property OnStateDisplay:TNotifyEvent read FOnStateDisplay write FOnStateDisplay;
    property OnStateLamp:TPhoneEventStateLamp read FOnStateLamp write FOnStateLamp;
    property OnStateRingMode:TNotifyEvent read FOnStateRingMode write FOnStateRingMode;
    property OnStateRingVolume:TNotifyEvent read FOnStateRingVolume write FOnStateRingVolume;
    property OnStateHandSetHookSwitch:TNotifyEvent read FOnStateHandSetHookSwitch write FOnStateHandSetHookSwitch;
    property OnStateHandSetVolume:TNotifyEvent read FOnStateHandSetVolume write FOnStateHandSetVolume;
    property OnStateHandSetGain:TNotifyEvent read FOnStateHandSetGain write FOnStateHandSetGain;
    property OnStateSpeakerHookSwitch:TNotifyEvent read FOnStateSpeakerHookSwitch write FOnStateSpeakerHookSwitch;
    property OnStateSpeakerVolume:TNotifyEvent read FOnStateSpeakerVolume write FOnStateSpeakerVolume;
    property OnStateSpeakerGain:TNotifyEvent read FOnStateSpeakerGain write FOnStateSpeakerGain;
    property OnStateHeadSetHookSwich:TNotifyEvent read FOnStateHeadSetHookSwich write FOnStateHeadSetHookSwich;
    property OnStateHeadSetVolume:TNotifyEvent read FOnStateHeadSetVolume write FOnStateHeadSetVolume;
    property OnStateHeadSetGain:TNotifyEvent read FOnStateHeadSetGain write FOnStateHeadSetGain;
    property OnStateSuspend:TNotifyEvent read FOnStateSuspend write FOnStateSuspend;
    property OnStateResume:TNotifyEvent read FOnStateResume write FOnStateResume;
    property OnStateDevSpecific:TNotifyEvent read FOnStateDevSpecific write FOnStateDevSpecific;
    property OnStateReInit:TNotifyEvent read FOnStateReInit write FOnStateReInit;
    property OnStateCapsChange:TNotifyEvent read FOnStateCapsChange write FOnStateCapsChange;
    property OnStateRemoved:TNotifyEvent read FOnStateRemoved write FOnStateRemoved;
    property OnClose:TNotifyEvent read FOnPhoneClose write FOnPhoneClose;
    property OnDevSpecific:TPhoneEventDevSpecific read FOnDevSpecific write FOnDevSpecific;
  end;

function IntToPhoneButtonState(Value:LongWord):TPhoneButtonState;

procedure Register;

{$ENDIF}
{$ENDIF}

implementation

{$IFDEF WINDOWS}
{$IFDEF CPU32}

uses {$IFDEF VER120}D4Comp,{$ENDIF}TAPIErr;


procedure Register;
begin
{$IFDEF TAPI30}
  RegisterComponents('TAPI30', [TTAPIPhone]);
{$ELSE}
{$IFDEF TAPI22}
  RegisterComponents('TAPI22', [TTAPIPhone]);
{$ELSE}
{$IFDEF TAPI21}
  RegisterComponents('TAPI21', [TTAPIPhone]);
{$ELSE}
{$IFDEF TAPI20}
  RegisterComponents('TAPI20', [TTAPIPhone]);
{$ELSE}
  RegisterComponents('TAPI', [TTAPIPhone]);
{$ENDIF}
{$ENDIF}
{$ENDIF}
{$ENDIF}
end;

function IntToPhoneButtonState(Value:LongWord):TPhoneButtonState;
begin
  result:=pbsUnknown;
  case Value of
    PHONEBUTTONSTATE_UP:Result:=pbsUp;
    PHONEBUTTONSTATE_DOWN:Result:=pbsDown;
    PHONEBUTTONSTATE_UNKNOWN:Result:=pbsUnknown;
    PHONEBUTTONSTATE_UNAVAIL:Result:=pbsUnavail;
  end;
end;


{ TTAPIPhone }

procedure TTAPIPhone.ButtonChange(ButtonLampID: DWord;
  Mode: TPhoneButtonMode; State: TPhoneButtonState);
begin
  If Assigned(FPhoneDevice) then FPhoneDevice.Caps.Update;
  FreeAndNil(FButtons);
  If Assigned(FOnPhoneButton) then FOnPhoneButton(self,ButtonLampID,Mode,State);
end;

procedure TTAPIPhone.Close;
var R:Longint;
begin
  try
    If (FPhoneHandle > 0)and (FPhoneDevice.Service.Active) then
    begin
      R:=PhoneClose(FPhoneHandle);
      if R<-1 then
      begin
        RaiseTAPIPhoneError(R);
      end;
    end;
  finally
    FPhoneHandle:=0;
  end;
end;

constructor TTAPIPhone.Create(AOwner: TComponent);
begin
  inherited;
  FPhonePrivilege:=ppOwner;
  FStateMessages:=[psReInit];
  FPhoneHandle:=0;
  //FButtons:=TPhoneButtons.Create(self);
  if Assigned(AppTAPIMgr)= False then AppTAPIMgr := TTAPIMgr.Create(Application);
  AppTAPIMgr.TAPIObjects.Add(self);
end;

destructor TTAPIPhone.Destroy;
begin
  FButtons.Free;
  Active:=False;
  inherited;

end;

procedure TTAPIPhone.DevSpecificChange(dwParam1, dwParam2, dwParam3:DWord);
begin
  if Assigned(FOnDevSpecific) then FOnDevSpecific(self,dwParam1,dwParam2,dwParam3);
end;

function TTAPIPhone.GetActive: Boolean;
begin
   Result:=FActive;
end;

function TTAPIPhone.GetButtons: TPhoneButtons;
var R:Longint;
    ButtonInfo:PPhoneButtonInfo;
    Size:DWord;
    i:Integer;
    Button:TPhoneButton;
begin
  if Assigned(FButtons)=false then
  begin
    FButtons:=TPhoneButtons.Create(TPhoneButton);
    Size:=SizeOf(TPhoneButtonInfo)+1000;
    for i:=0 to Device.Caps.NumButtonLamps -1 do
    begin
      GetMem(ButtonInfo,Size);
      ButtonInfo.dwTotalSize:=Size;
      try
        R:=phoneGetButtonInfo(Handle,i,ButtonInfo);
        if R<>0 then RaiseTAPIPhoneError(R)
        else
        begin
          Button:=FButtons.Add;
          Button.SetButtonInfo(ButtonInfo);
        end;
      finally
        FreeMem(ButtonInfo);
      end;
    end;
  end;
  result:=FButtons;
end;

function TTAPIPhone.GetDisplay: TPhoneDisplay;
begin
  if Assigned(FDisplay)=False then FDisplay:=TPhoneDisplay.Create(self);
  Result := FDisplay;
end;



function TTAPIPhone.GetGain(Index:Integer): Integer;
var //R:LongInt;
    GainValue:DWord;
begin
  GainValue:=0;
  if Handle > 0 then
  begin
    //R:=
    PhoneGetGain(Handle,Index,GainValue);
    //if R<>0 then RaiseTAPIPhoneError(R);
  end;
  Result:=GainValue;
end;

function TTAPIPhone.GetHookSwitch(const Index:Integer): Boolean;
var HookSwitchValues:TPhoneHookSwitchDevs;
begin
  Result:=False;
  if Handle > 0 then
  begin
    HookSwitchValues:=GetHookSwitchDevs;
  end;
  if (Index=1) and (phsdHandset in HookSwitchValues) then Result:=True;
  if (Index=2) and (phsdSpeaker in HookSwitchValues) then Result:=True;
  if (Index=4) and (phsdHeadset in HookSwitchValues) then Result:=True;
end;

function TTAPIPhone.GetHookSwitchDevs:TPhoneHookSwitchDevs;
var R:Longint;
    HookSwitchDev:DWord;
begin
  Result:=[];
  R:=PhoneGetHookSwitch(FPhoneHandle,HookSwitchDev);
  if R<> 0 then RaiseTAPIPhoneError(R)
  else Result:=IntToPhoneHookSwitchDevs(HookSwitchDev);
end;

function TTAPIPhone.GetHookSwitchMode(const Index:Integer): TPhoneHookSwitchMode;
begin
  Result:=phsmUnknown;
  case Index of
    1:Result:=FHandSetHookSwitchMode;
    2:Result:=FSpeakerHookSwitchMode;
    4:Result:=FHeadSetHookSwitchMode;
  end;
end;



procedure TTAPIPhone.GetID(var VarStr: TVarString; DeviceClass: PChar);
var R:Longint;
begin
  R:=PhoneGetID(FPhoneHandle,@varstr,PChar(DeviceClass));
  if R<-1 then
  begin
    RaiseTAPIPhoneError(R);
  end;
end;

function TTAPIPhone.GetLineID: DWord;
var varstr:PVARSTRING;
    Dummy:Array[0..255] of Char;
    id:Cardinal;
begin
  id:=INVALID_Handle_Value;
  varstr:=PVARSTRING(AllocMem(SizeOf(TVarString)+45));
  try
    varstr^.dwTotalSize:=SizeOf(TVARSTRING)+45;
    GetID(varstr^,'tapi/line');
    if varstr^.dwStringFormat<>STRINGFORMAT_BINARY then
    begin

    end
    else
    begin
      StrCopy(Dummy,PCHAR(VarStr)+SizeOf(TVarString));
      id:=Byte(Dummy[0])+(256*(Byte(Dummy[1])));
    end;
    Result:=Cardinal(id);
  finally
    FreeMem(VarStr);
  end;
end;

procedure TTAPIPhone.GetLineIDs(var IDs: array of DWord);
var varstr:PVARSTRING;
    Dummy:Array of Char;
    id:Cardinal;
    i:Integer;
    Offset:DWord;
begin
  varstr:=PVARSTRING(AllocMem(SizeOf(TVarString)+45));
  try
    varstr^.dwTotalSize:=SizeOf(TVARSTRING)+45;
    GetID(varstr^,'tapi/line');
    Offset:=0;
    SetLength(Dummy,varstr^.dwStringSize+2);
    for i:=0 to High(IDs) do
    begin
      id:=INVALID_Handle_Value;
      if (Offset < varstr^.dwStringSize) and
        (varstr^.dwStringFormat = STRINGFORMAT_BINARY) then
      begin
        StrCopy(Pchar(Dummy),PCHAR(VarStr)+SizeOf(TVarString)+Offset);
        id:=Byte(Dummy[0])+(256*(Byte(Dummy[1])));
        Offset:=Offset+SizeOf(DWord);
      end;
      IDs[i]:=Cardinal(id);
    end;
  finally
    FreeMem(VarStr);
  end;
end;

function TTAPIPhone.GetPhonePrivilege: LongWord;
begin
  Result:=0;
  if FPhonePrivilege=ppOwner then Result:=PHONEPRIVILEGE_OWNER;
  if FPhonePrivilege=ppMonitor then Result:=Result or PHONEPRIVILEGE_MONITOR;
end;

function TTAPIPhone.GetStateMessages: DWord;
begin
  FStateMessages:=[];
  if Assigned(OnStateOther) then FStateMessages:=FStateMessages +[psOther];
  if Assigned(OnStateConnected) then FStateMessages:=FStateMessages +[psConnected];;
  if Assigned(OnStateDisConnected) then FStateMessages:=FStateMessages +[psDisconnected];
  if Assigned(OnStateOwner) then FStateMessages:=FStateMessages +[psOwner];
  if Assigned(OnStateMonitor) then FStateMessages:=FStateMessages +[psMonitors];
  if Assigned(OnStateDisplay) then FStateMessages:=FStateMessages +[psDisplay];
  if Assigned(OnStateLamp) then FStateMessages:=FStateMessages +[psLamp];
  if Assigned(OnStateRingMode) then FStateMessages:=FStateMessages +[psRingMode];
  if Assigned(OnStateRingVolume) then FStateMessages:=FStateMessages +[psRingVolume];
  if Assigned(OnStateHandSetHookSwitch) then FStateMessages:=FStateMessages +[psHandsetHookswitch];
  if Assigned(OnStateHandSetVolume) then FStateMessages:=FStateMessages +[psHandsetVolume];
  if Assigned(OnStateHandSetGain) then FStateMessages:=FStateMessages +[psHandsetGain];
  if Assigned(OnStateSpeakerHookSwitch) then FStateMessages:=FStateMessages +[psSpeakerHookswitch];
  if Assigned(OnStateSpeakerVolume) then FStateMessages:=FStateMessages +[psSpeakerVolume];
  if Assigned(OnStateSpeakerGain) then FStateMessages:=FStateMessages +[psSpeakerGain];
  if Assigned(OnStateHeadSetHookSwich) then FStateMessages:=FStateMessages +[psHeadsetHookswitch];
  if Assigned(OnStateHeadSetVolume) then FStateMessages:=FStateMessages +[psHeadsetVolume];
  if Assigned(OnStateHeadSetGain) then FStateMessages:=FStateMessages +[psHeadsetGain];
  if Assigned(OnStateSuspend) then FStateMessages:=FStateMessages +[psSuspend];
  if Assigned(OnStateResume) then FStateMessages:=FStateMessages +[psResume];
  if Assigned(OnStateDevSpecific) then FStateMessages:=FStateMessages +[psDevSpecific];
  if Assigned(OnStateReInit) then FStateMessages:=FStateMessages +[psReInit];
  if Assigned(OnStateCapsChange) then FStateMessages:=FStateMessages +[psCapsChange];
  if Assigned(OnStateRemoved) then FStateMessages:=FStateMessages +[psRemoved];
  Result:=PhoneStatesToInt(FStateMessages);
end;

function TTAPIPhone.GetStatus: TPhoneDeviceStatus;
begin
  if FActive then
  begin
    If Assigned(FDevStatus)=false then FDevStatus:=TPhoneDeviceStatus.Create(Handle);
  end
  else RaiseTAPIError(TAPIERR_NOTACTIVE);
  Result:=FDevStatus;
end;

function TTAPIPhone.GetVolume(const Index: Integer): Integer;
var VolumeValue:DWord;
begin
  VolumeValue:=0;
  if Handle > 0 then
  begin
    //R:=
    PhoneGetVolume(Handle,Index,VolumeValue);
    //if R<>0 then RaiseTAPIPhoneError(R);
  end;
  Result:=VolumeValue;
end;

procedure TTAPIPhone.Open;
var R:Longint;
begin
  FPhoneHandle:=0;
  R:=PhoneOpen(FPhoneDevice.Service.Handle,FPhoneDevice.ID,FPhoneHandle,FPhoneDevice.APIVersion,FPhoneDevice.Service.APIExtVersion,ComponentIndex,GetPhonePrivilege);
  if R<>0 then
  begin
    RaiseTAPIPhoneError(R);
  end;
end;

procedure TTAPIPhone.PerformMsg(Msg: TCMTAPI);
begin
  inherited;
  with Msg.TAPIRec^ do
  begin
    if dwMsg = PHONE_REPLY  then  PhoneReply(Msg.AsyncFunc^,dwParam2)
    else
    if FPhoneHandle=hDevice then
    begin
      case dwMsg of
        PHONE_BUTTON: ButtonChange(dwParam1,IntToPhoneButtonMode(dwParam2),IntToPhoneButtonState(dwParam3));
        PHONE_CLOSE:  PhoneIsClose(hDevice);
        PHONE_DEVSPECIFIC: DevSpecificChange(dwParam1,dwParam2,dwParam3);
        PHONE_STATE: PhoneState(dwParam1,dwParam2);
      end;
    end;
  end;
end;

procedure TTAPIPhone.PhoneIsClose(hDevice: LongWord);
begin
  FreeAndNil(FDevStatus);
  FActive:=False;
  if Assigned(FOnPhoneClose) then FOnPhoneClose(self);
  FPhoneHandle:=0;
end;

procedure TTAPIPhone.PhoneReply(AsyncFunc: TAsyncFunc; Error: DWord);
begin
  if Assigned(FOnPhoneReply) then FOnPhoneReply(self,AsyncFunc,Error);
end;

procedure TTAPIPhone.PhoneState(dwParam1, dwParam2:DWord);
{$IFDEF DEBUG}
var  PS:String;
{$ENDIF}
begin
  FreeAndNil(FDevStatus);
  {$IFDEF DEBUG}
  case dwParam1 of
    PHONESTATE_OTHER:PS:='PHONESTATE_OTHER';
    PHONESTATE_CONNECTED:PS:='PHONESTATE_CONNECTED';
    PHONESTATE_DISCONNECTED:PS:='PHONESTATE_DISCONNECTED';
    PHONESTATE_OWNER:PS:='PHONESTATE_OWNER';
    PHONESTATE_MONITORS:PS:='PHONESTATE_MONITORS';
    PHONESTATE_DISPLAY:PS:='PHONESTATE_DISPLAY';
    PHONESTATE_LAMP:PS:='PHONESTATE_LAMP';
    PHONESTATE_RINGMODE:PS:='PHONESTATE_RINGMODE';
    PHONESTATE_RINGVOLUME:PS:='PHONESTATE_RINGVOLUME';
    PHONESTATE_HANDSETHOOKSWITCH:PS:='PHONESTATE_HANDSETHOOKSWITCH';
    PHONESTATE_HANDSETVOLUME:PS:='PHONESTATE_HANDSETVOLUME';
    PHONESTATE_HANDSETGAIN:PS:='PHONESTATE_HANDSETGAIN';
    PHONESTATE_SPEAKERHOOKSWITCH:PS:='PHONESTATE_SPEAKERHOOKSWITCH';
    PHONESTATE_SPEAKERVOLUME:PS:='PHONESTATE_SPEAKERVOLUME';
    PHONESTATE_SPEAKERGAIN:PS:='PHONESTATE_SPEAKERGAIN';
    PHONESTATE_HEADSETHOOKSWITCH:PS:='PHONESTATE_HEADSETHOOKSWITCH';
    PHONESTATE_HEADSETVOLUME:PS:='PHONESTATE_HEADSETVOLUME';
    PHONESTATE_HEADSETGAIN:PS:='PHONESTATE_HEADSETGAIN';
    PHONESTATE_SUSPEND:PS:='PHONESTATE_SUSPEND';
    PHONESTATE_RESUME:PS:='PHONESTATE_RESUME';
    PHONESTATE_DEVSPECIFIC:PS:='PHONESTATE_DEVSPECIFIC';
    PHONESTATE_REINIT:PS:='PHONESTATE_REINIT';
    PHONESTATE_CAPSCHANGE:PS:='PHONESTATE_CAPSCHANGE';
    PHONESTATE_REMOVED:PS:='PHONESTATE_REMOVED';
  end;
  OutputDebugString(PChar(PS));
  {$ENDIF}
  case dwParam1 of
    PHONESTATE_OTHER:if Assigned(FOnStateOther) then FOnStateOther(self);
    PHONESTATE_CONNECTED:if Assigned(FOnStateConnected) then FOnStateConnected(self);
    PHONESTATE_DISCONNECTED:if Assigned(FOnStateDisConnected) then FOnStateDisConnected(self);
    PHONESTATE_OWNER:if Assigned(FOnStateOwner) then FOnStateOwner(self,dwParam2);
    PHONESTATE_MONITORS:if Assigned(FOnStateMonitor) then FOnStateMonitor(self,dwParam2);
    PHONESTATE_DISPLAY: if Assigned(FOnStateDisplay) then FOnStateDisplay(self);
    PHONESTATE_LAMP:if Assigned(FOnStateLamp) then FOnStateLamp(self,dwParam2);
    PHONESTATE_RINGMODE:if Assigned(FOnStateRingMode) then FOnStateRingMode(self);
    PHONESTATE_RINGVOLUME:if Assigned(FOnStateRingVolume) then FOnStateRingVolume(self);
    PHONESTATE_HANDSETHOOKSWITCH:if Assigned(FOnStateHandSetHookSwitch) then FOnStateHandSetHookSwitch(self);
    PHONESTATE_HANDSETVOLUME:if Assigned(FOnStateHandSetVolume) then FOnStateHandSetVolume(self);
    PHONESTATE_HANDSETGAIN:if Assigned(FOnStateHandSetGain) then FOnStateHandSetGain(self);
    PHONESTATE_SPEAKERHOOKSWITCH:if Assigned(FOnStateSpeakerHookSwitch) then FOnStateSpeakerHookSwitch(self);
    PHONESTATE_SPEAKERVOLUME:if Assigned(FOnStateSpeakerVolume) then FOnStateSpeakerVolume(self);
    PHONESTATE_SPEAKERGAIN:if Assigned(FOnStateSpeakerGain) then FOnStateSpeakerGain(self);
    PHONESTATE_HEADSETHOOKSWITCH:if Assigned(FOnStateHeadSetHookSwich) then FOnStateHeadSetHookSwich(self);
    PHONESTATE_HEADSETVOLUME:if Assigned(FOnStateHeadSetVolume) then FOnStateHeadSetVolume(self);
    PHONESTATE_HEADSETGAIN:if Assigned(FOnStateHeadSetGain) then FOnStateHeadSetGain(self);
    PHONESTATE_SUSPEND:if Assigned(FOnStateSuspend) then FOnStateSuspend(self);
    PHONESTATE_RESUME:if Assigned(FOnStateResume) then FOnStateResume(self);
    PHONESTATE_DEVSPECIFIC:if Assigned(FOnStateDevSpecific) then FOnStateDevSpecific(self);
    PHONESTATE_REINIT:if Assigned(FOnStateReInit) then FOnStateReInit(self);
    PHONESTATE_CAPSCHANGE:if Assigned(FOnStateCapsChange) then FOnStateCapsChange(self);
    PHONESTATE_REMOVED:if Assigned(FOnStateRemoved) then FOnStateRemoved(self);
  end;
end;

procedure TTAPIPhone.SetActive(const Value: Boolean);
begin
  if Value<> FActive then
  begin
    if Value=true then
    begin
      if Assigned(FPhoneDevice) then
      begin
        if Assigned(FBeforeOpen) then FBeforeOpen(self);
        Open;
      end
      else
          Raise(EPhoneError.CreateFmt('Zuweisung PhoneDevice von %s fehlt',[Name]));
      FActive:=Value;
      if Assigned(FAfterOpen) then FAfterOpen(self);
    end
    else
    begin
      Close;
      FActive:=Value;
    end;
  end;
  if Active then SetStatusMessages;
end;

procedure TTAPIPhone.SetGain(const Index, Value: Integer);
var R:LongInt;
    GainValue:DWord;
begin
  GainValue:=Value;
  if Handle > 0 then
  begin
    R:=PhoneSetGain(Handle,Index,GainValue);
    if R < 0 then RaiseTAPIPhoneError(R)
    else
    begin
      AppTAPIMgr.AsyncList.Add(afSetGain,R,self);
    end;
  end;
end;

procedure TTAPIPhone.SetHookSwitch(Index:Integer;Value:TPhoneHookSwitchMode);
var R:Longint;
begin
  R:=PhoneSetHookSwitch(FPhoneHandle,DWord(Index),PhoneHookSwitchModeToInt(Value));
  if R < -1 then RaiseTAPIPhoneError(R)
  else
  begin
    AppTAPIMgr.AsyncList.Add(afSetHookSwitch,R,self);
  end;
end;

procedure TTAPIPhone.SetStatusMessages;
var R:LongInt;
    State,BMState,BState:DWord;
begin
  if Active then
  begin
    State:=GetStateMessages;
    {$IFDEF DEBUG}
    R:=PhoneSetStatusMessages(Handle,$00ffffff,$3F,$F);
    {$ELSE}
    R:=PhoneSetStatusMessages(Handle,State,{ButtonModesToInt(),ButonStates()}0,0);
    {$ENDIF}
    if R < -1 then RaiseTAPIPhoneError(R)
    else
    begin
      R:=PhoneGetStatusMessages(Handle,State,BMState,BState);
      if R<-1 then
      begin
        RaiseTAPIPhoneError(R);
      end
      else
      begin
        R:=PhoneSetStatusMessages(Handle,State,BMState,BState);
        if R<-1 then
        begin
          RaiseTAPILineError(R);
        end;
      end;
    end;
  end;
end;

procedure TTAPIPhone.SetVolume(const Index, Value: Integer);
var R:LongInt;
    VolumeValue:DWord;
begin
  VolumeValue:=Value;
  if Handle > 0 then
  begin
    R:=PhoneSetVolume(Handle,Index,VolumeValue);
    if R < 0 then RaiseTAPIPhoneError(R)
    else
    begin
      AppTAPIMgr.AsyncList.Add(afSetGain,R,self);
    end;
  end;
end;

{ TPhoneDisplay }

constructor TPhoneDisplay.Create(AOwner: TComponent);
begin
  inherited Create;
  FOwner:=AOwner;
end;

destructor TPhoneDisplay.Destroy;
begin
  inherited;

end;

function TPhoneDisplay.GetDisplay: String;
var DisplayText:PVarString;
    R:Longint;
    Dummy:Array of Char;
begin
  Result:='';
  if UpdateValue then
  begin
    GetMem(DisplayText,SizeOf(TVarString)+1000);
    DisplayText.dwTotalSize:=SizeOf(TVarString)+1000;
    try
      R:=phoneGetDisplay(TTAPIPhone(FOwner).Handle,DisplayText);
      if R <> 0 then RaiseTAPIPhoneError(R)
      else
        SetLength(Dummy,DisplayText.dwStringSize);
        StrCopy(PChar(Dummy),PChar(DisplayText)+DisplayText.dwStringOffset);
    finally
      FreeMem(DisplayText);
    end;
    Result:=PChar(Dummy);
  end;
end;

function TPhoneDisplay.GetNumColumn: Integer;
begin
  UpdateValue;
  Result:=FNumColumn;
end;

function TPhoneDisplay.GetNumRows: Integer;
begin
  UpdateValue;
  Result:=FNumRows;
end;

procedure TPhoneDisplay.SetDisplay(Row,Column:Integer; Value: String);
var R:LongInt;
begin
  if Assigned(TTAPIPhone(FOwner).Device) then
  begin
    if (TTAPIPhone(FOwner).Device.Caps.DisplayNumRows >0) and (TTAPIPhone(FOwner).Device.Caps.DisplayNumColumns > 0) then
    begin
      R:=phoneSetDisplay(TTAPIPhone(FOwner).Handle,Row,Column,PChar(Value),StrLen(PChar(Value)));
      if R <> 0 then RaiseTAPIPhoneError(R);
    end;
  end;
end;



function TPhoneDisplay.UpdateValue: Boolean;
begin
  Result:=False;
  if Assigned(TTAPIPhone(FOwner).Device) then
  begin
    if (TTAPIPhone(FOwner).Device.Caps.DisplayNumRows >0) and (TTAPIPhone(FOwner).Device.Caps.DisplayNumColumns > 0) then
    begin
      FNumRows:=TTAPIPhone(FOwner).Device.Caps.DisplayNumRows;
      FNumColumn:=TTAPIPhone(FOwner).Device.Caps.DisplayNumColumns;
      Result:=True;
    end;
  end;
end;

{ TPhoneButtons }

function TPhoneButtons.Add: TPhoneButton;
begin
  result:=TPhoneButton(inherited Add);
end;

constructor TPhoneButtons.Create(ItemClass: TCollectionItemClass);
begin
  inherited Create(ItemClass);
end;

destructor TPhoneButtons.Destroy;
begin
 inherited;
end;

function TPhoneButtons.GetItem(Index: Integer): TPhoneButton;
begin
  result:=TPhoneButton(inherited GetItem(Index));
end;

procedure TPhoneButtons.SetItem(Index: Integer;
  const Value: TPhoneButton);
begin
  inherited SetItem(Index,Value);
end;

{ TPhoneButton }

constructor TPhoneButton.Create(Collection: TCollection);
begin
  inherited Create(Collection);
end;

destructor TPhoneButton.Destroy;
begin
  inherited;
end;

procedure TPhoneButton.SetButtonInfo(BInfo: PPhoneButtonInfo);
var Dummy:Array of char;
begin
  FMode:=IntToPhoneButtonMode(BInfo^.dwButtonMode);
  FFunction:=IntToPhoneButtonFunction(BInfo^.dwButtonFunction);
  if BInfo^.dwButtonTextSize >0 then
  begin
    SetLength(Dummy,BInfo^.dwButtonTextSize);
    StrCopy(PChar(Dummy),PChar(BInfo)+BInfo^.dwButtonTextOffset);
    FText:=PChar(Dummy);
  end
  else FText:='';
  if BInfo^.dwDevSpecificSize > 0 then
  begin
    SetLength(Dummy,BInfo^.dwDevSpecificSize);
    StrCopy(PChar(Dummy),PChar(BInfo)+BInfo^.dwDevSpecificOffset);
    FDevSpecific:=PChar(Dummy);
  end
  else FDevSpecific:='';
  FState:=IntToPhoneButtonState(BInfo^.dwButtonState);
end;

{$ENDIF}
{$ENDIF}
end.