{******************************************************************************}
{*        Copyright 1999-2001 by J.Friebel all rights reserved.               *}
{*        Autor           :  Jörg Friebel                                     *}
{*        Compiler        :  Delphi 4 / 5                                     *}
{*        System          :  Windows NT / 2000                                *}
{*        Projekt         :  TAPI Komponenten (TAPI Version 1.4 bis 3.0)      *}
{*        Last Update     :  08.05.2002                                       *}
{*        Version         :  1.3                                              *}
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

unit CallList;

interface

{$IFDEF WINDOWS}
{$IFDEF CPU32}
uses Windows,Classes,TAPI,TAPISystem,TAPILines,TAPICall,TAPIAddress;

{$INCLUDE TAPI.INC}

{$IFDEF FPC}
{$MODE DELPHI}
{$ENDIF}

type
  TCallItem = class(TCollectionItem)
  private
    { Private-Deklarationen }
    FCall:TTAPICall;
    function GetHandle: hCall;
    procedure SetHandle(const Value: hCall);
    function GetCall: TTAPICall;

  protected
    { Protected-Deklarationen }
  public
    { Public-Deklarationen }
    constructor Create(xCollection: TCollection);override;
    destructor Destroy;override;
  published
    { Published-Deklarationen }
    property Handle:hCall read GetHandle write SetHandle;
    property Call:TTAPICall read GetCall write FCall;
  end;


  TCallList=class(TCollection)
   private
     FOwner:TComponent;
   protected
     function GetOwner:TPersistent;override;
   public
     function GetCall(AHandle:hCall):TCallItem;
     function Add:TCallItem;
     constructor Create(xItemClass: TCollectionItemClass);
     destructor Destroy; override;
   end;

   TTAPICallList = class(TTAPIComponent)
   private
     FCallList:TCallList;
     FAddress: TTAPIAddress;
     FLine: TTAPILine;
     FOnDevSpecific:TCallEventDevSpecific;
     FOnDevSpecificFeature:TCallEventDevSpecificFeature;
     FOnMonitorMedia:TCallEventMonitorMedia;
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
     FOnINFOSTATEBEARERMODE:TNotifyEvent;
     FOnINFOSTATERATE:TNotifyEvent;
     FOnINFOSTATEMEDIAMODE:TNotifyEvent;
     FOnINFOSTATEAPPSPECIFIC:TNotifyEvent;
     FOnINFOSTATECALLID:TNotifyEvent;
     FOnINFOSTATERELATEDCALLID:TNotifyEvent;
     FOnINFOSTATEORIGIN:TNotifyEvent;
     FOnINFOSTATEREASON:TNotifyEvent;
     FOnINFOSTATECOMPLETIONID:TNotifyEvent;
     FOnINFOSTATENUMOWNERINCR:TNotifyEvent;
     FOnINFOSTATENUMOWNERDECR:TNotifyEvent;
     FOnINFOSTATENUMMONITORS:TNotifyEvent;
     FOnINFOSTATETRUNK:TNotifyEvent;
     FOnINFOSTATECALLERID:TNotifyEvent;
     FOnINFOSTATECALLEDID:TNotifyEvent;
     FOnINFOSTATECONNECTEDID:TNotifyEvent;
     FOnINFOSTATEREDIRECTIONID:TNotifyEvent;
     FOnINFOSTATEREDIRECTINGID:TNotifyEvent;
     FOnINFOSTATEDISPLAY:TNotifyEvent;
     FOnINFOSTATEUSERUSERINFO:TNotifyEvent;
     FOnINFOSTATEHIGHLEVELCOMP:TNotifyEvent;
     FOnINFOSTATELOWLEVELCOMP:TNotifyEvent;
     FOnINFOSTATECHARGINGINFO:TNotifyEvent;
     FOnINFOSTATETERMINAL:TNotifyEvent;
     FOnINFOSTATEDIALPARAMS:TNotifyEvent;
     FOnINFOSTATEMONITORMODES:TNotifyEvent;
     FOnReply: TCallEventReply;
    procedure SetAddress(const Value: TTAPIAddress);
    procedure SetLine(const Value: TTAPILine);
    function GetCallItem(Index: Integer): TCallItem;
   protected
     procedure Notification(AComponent:TComponent; Operation :TOperation); override;
   public
     constructor Create(AOwner: TComponent);override;
     destructor Destroy;override;
     procedure UpdateList;
     procedure Add(ACall:TTAPICall);
     property Items[Index:Integer]:TCallItem read GetCallItem;
   published
     property Line:TTAPILine read FLine write SetLine;
     property Address:TTAPIAddress read FAddress write SetAddress;
     property OnStateIdle:TCallEvent read FOnStateIdle write FOnStateIdle;
     property OnStateOffering:TCallEventOffering read FOnStateOffering write FOnStateOffering;
     property OnStateAccepted:TCallEvent read FOnStateAccepted write FOnStateAccepted;
     property OnStateDialTone:TCallEventDialTone read FOnStateDialTone write FOnStateDialTone;
     property OnStateDialing:TCallEvent read FOnSTATEDIALING write FOnSTATEDIALING;
     property OnStateRingBack:TCallEvent read FOnSTATERINGBACK write FOnSTATERINGBACK;
     property OnStateBusy:TCallEventBusy read FOnSTATEBUSY write FOnSTATEBUSY;
     property OnStateSpecialInfo:TCallEventSpecialInfo read FOnSTATESPECIALINFO write FOnSTATESPECIALINFO;
     property OnStateConnected:TCallEventConnected read FOnSTATECONNECTED write FOnSTATECONNECTED;
     property OnStateProceeding:TCallEvent read FOnSTATEPROCEEDING write FOnSTATEPROCEEDING;
     property OnStateOnHold:TCallEvent read FOnSTATEONHOLD write FOnSTATEONHOLD;
     property OnStateConferenced:TCallEvent read FOnSTATECONFERENCED write FOnSTATECONFERENCED;
     property OnStateOnHoldPendConf:TCallEvent read FOnSTATEONHOLDPENDCONF write FOnSTATEONHOLDPENDCONF;
     property OnStateOnHoldPendTransf:TCallEvent read FOnSTATEONHOLDPENDTRANSF write FOnSTATEONHOLDPENDTRANSF;
     property OnStateDisconnected:TCallEventDisconnected read FOnSTATEDISCONNECTED write FOnSTATEDISCONNECTED;
     property OnStateUnknown:TCallEvent read FOnSTATEUNKNOWN write FOnSTATEUNKNOWN;
     property OnReply:TCallEventReply read FOnReply write FOnReply;
     property OnInfoOther:TNotifyEvent read FOnInfoStateOTHER write FOnInfoStateOTHER;
     property OnInfoDevSpezific:TNotifyEvent read FOnInfoStateDEVSPECIFIC write FOnInfoStateDEVSPECIFIC;
     property OnInfoBearerMode:TNotifyEvent read FOnInfoStateBEARERMODE write FOnInfoStateBEARERMODE;
     property OnInfoRate:TNotifyEvent read FOnInfoStateRATE write FOnInfoStateRATE;
     property OnInfoMediaMode:TNotifyEvent read FOnInfoStateMEDIAMODE write FOnInfoStateMEDIAMODE;
     property OnInfoAppSpecific:TNotifyEvent read FOnInfoStateAPPSPECIFIC write FOnInfoStateAPPSPECIFIC;
     property OnInfoCallId:TNotifyEvent read FOnInfoStateCALLID write FOnInfoStateCALLID;
     property OnInfoRelatedCallId:TNotifyEvent read FOnInfoStateRELATEDCALLID write FOnInfoStateRELATEDCALLID;
     property OnInfoOrigin:TNotifyEvent read FOnInfoStateORIGIN write FOnInfoStateORIGIN;
     property OnInfoReason:TNotifyEvent read FOnInfoStateREASON write FOnInfoStateREASON;
     property OnInfoCompletionId:TNotifyEvent read FOnInfoStateCOMPLETIONID write FOnInfoStateCOMPLETIONID;
     property OnInfoNumOwnerIncr:TNotifyEvent read FOnInfoStateNUMOWNERINCR write FOnInfoStateNUMOWNERINCR;
     property OnInfoNumOwnerDecr:TNotifyEvent read FOnInfoStateNUMOWNERDECR write FOnInfoStateNUMOWNERDECR;
     property OnInfoNumMonitors:TNotifyEvent read FOnInfoStateNUMMONITORS write FOnInfoStateNUMMONITORS;
     property OnInfoTrunk:TNotifyEvent read FOnInfoStateNUMMONITORS  write FOnInfoStateTRUNK;
     property OnInfoCallerId:TNotifyEvent read FOnInfoStateCALLERID write FOnInfoStateCALLERID;
     property OnInfoCalledId:TNotifyEvent read FOnInfoStateCALLEDID write FOnInfoStateCALLEDID;
     property OnInfoConnectedId:TNotifyEvent read FOnInfoStateCONNECTEDID write FOnInfoStateCONNECTEDID;
     property OnInfoRedirectionId:TNotifyEvent read FOnInfoStateREDIRECTIONID write FOnInfoStateREDIRECTIONID;
     property OnInfoRedirectingId:TNotifyEvent read FOnInfoStateREDIRECTINGID write FOnInfoStateREDIRECTINGID;
     property OnInfoDisplay:TNotifyEvent read FOnInfoStateDISPLAY write FOnInfoStateDISPLAY;
     property OnInfoUserUserInfo:TNotifyEvent read FOnInfoStateUSERUSERINFO write FOnInfoStateUSERUSERINFO;
     property OnInfoHighLevelComp:TNotifyEvent read FOnInfoStateHIGHLEVELCOMP write FOnInfoStateHIGHLEVELCOMP;
     property OnInfoLowLevelComp:TNotifyEvent read FOnInfoStateLOWLEVELCOMP write FOnInfoStateLOWLEVELCOMP;
     property OnInfoChargingInfo:TNotifyEvent read FOnInfoStateCHARGINGINFO write FOnInfoStateCHARGINGINFO;
     property OnInfoTerminal:TNotifyEvent read FOnInfoStateTERMINAL write FOnInfoStateTERMINAL;
     property OnInfoDialParams:TNotifyEvent read FOnInfoStateDIALPARAMS write FOnInfoStateDIALPARAMS;
     property OnInfoMonitorModes:TNotifyEvent read FOnInfoStateMONITORMODES write FOnInfoStateMONITORMODES;
     //property OnLineGatherDigits:TCallGatherDigitsEvent read FOnLineGatherDigits write FOnLineGatherDigits;
     property OnMonitorMedia:TCallEventMonitorMedia read FOnMonitorMedia write FOnMonitorMedia;
     property OnDevSpecific:TCallEventDevSpecific read FOnDevSpecific write FOnDevSpecific;
     property OnDevSpecificFeature:TCallEventDevSpecificFeature read FOnDevSpecificFeature write FOnDevSpecificFeature;
   end;

procedure Register;

{$ENDIF}
{$ENDIF}

implementation

{$IFDEF WINDOWS}
{$IFDEF CPU32}

uses TAPIErr,SysUtils,TAPIHelpFunc;

procedure Register;
begin
{$IFDEF TAPI30}
  RegisterComponents('TAPI30', [TTAPICallList]);
{$ELSE}
{$IFDEF TAPI22}
  RegisterComponents('TAPI22', [TTAPICallList]);
{$ELSE}
{$IFDEF TAPI21}
  RegisterComponents('TAPI21', [TTAPICallList]);
{$ELSE}
{$IFDEF TAPI20}
  RegisterComponents('TAPI20', [TTAPICallList]);
{$ELSE}
  RegisterComponents('TAPI', [TTAPICallList]);
{$ENDIF}
{$ENDIF}
{$ENDIF}
{$ENDIF}
end;

{ TCallList }

function TCallList.Add: TCallItem;
begin
  Result:=TCallItem(inherited Add);
end;

constructor TCallList.Create(xItemClass: TCollectionItemClass);
begin
  inherited Create(xItemClass);
end;

destructor TCallList.Destroy;
begin
  inherited Destroy;
end;

function TCallList.GetCall(AHandle: hCall): TCallItem;
var i:Integer;
begin
  result:=nil;
  for i:=0 to Count-1 do
  begin
    if TCallItem(Items[i]).Handle=AHandle then Result:=TCallItem(Items[i]);
  end;
end;

function TCallList.GetOwner: TPersistent;
begin
  Result:=FOwner;
end;


{ TTAPICallList }

procedure TTAPICallList.Add(ACall: TTAPICall);
var CallItem:TCallItem;
begin
  CallItem:=FCallList.Add;
  CallItem.Call:=ACall;
end;

constructor TTAPICallList.Create(AOwner: TComponent);
begin
  inherited;
  FCallList:=TCallList.Create(TCallItem);
  {$IFNDEF VER140}
  FCallList.FOwner:=Owner;
  {$ENDIF}
end;

destructor TTAPICallList.Destroy;
begin
  FCallList.Destroy;
  inherited;
end;

function TTAPICallList.GetCallItem(Index: Integer): TCallItem;
begin
  Result:=TCallItem(FCallList.Items[Index]);
end;

procedure TTAPICallList.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent,Operation);
  if Operation=opRemove then
  begin
    if AComponent=FAddress then
      FAddress:=nil;
    if AComponent=FLine then
      FLine:=nil;
  end;    
end;

procedure TTAPICallList.SetAddress(const Value: TTAPIAddress);
begin
  FLine:=nil;
  FAddress := Value;
end;

procedure TTAPICallList.SetLine(const Value: TTAPILine);
begin
  FAddress:=nil;
  FLine := Value;
end;

procedure TTAPICallList.UpdateList;
var R:LongInt;
    LineHandle:hLine;
    AddressID:DWord;
    dwSelect:DWord;
    lpCallList:PLineCallList;
    Item:TCallItem;
    Dummy,Dummy1,Dummy2,Dummy3:Array of Byte;
    ahCall:HCall;
    i,ii:Integer;
begin
  LineHandle:=0;
  AddressID:=0;
  dwSelect:=0;
  if Assigned(FLine) then
  begin
    LineHandle:=FLine.Handle;
    AddressID:=0;
    dwSelect:=LINECALLSELECT_LINE;
  end;
  if Assigned(FAddress) then
  begin
    LineHandle:=FAddress.Line.Handle;
    AddressID:=FAddress.ID;
    dwSelect:=LINECALLSELECT_ADDRESS;
  end;
  GetMem(lpCallList,SizeOf(TLineCallList)+1000);
  lpCallList^.dwTotalSize:= SizeOf(TLineCallList)+1000;
  try
    R:=LineGetNewCalls(LineHandle,AddressID,dwSelect,lpCallList);
    if R<>0 then RaiseTAPILineError(R);
    if lpCallList^.dwCallsNumEntries > 0 then
    begin
      ii:=0;
      for i:=0 to lpCallList^.dwCallsNumEntries-1 do
      begin
        Item:=FCallList.Add;
        SetLength(Dummy,lpCallList^.dwCallsSize+1);
        SetLength(Dummy1,lpCallList^.dwCallsSize+1);
        SetLength(Dummy2,lpCallList^.dwCallsSize+1);
        SetLength(Dummy3,lpCallList^.dwCallsSize+1);
        StrCopy(PChar(Dummy),PCHAR(lpCallList)+lpCallList^.dwCallsOffset+ii);
        StrCopy(PChar(Dummy1),PCHAR(lpCallList)+lpCallList^.dwCallsOffset+1+ii);
        StrCopy(PChar(Dummy2),PCHAR(lpCallList)+lpCallList^.dwCallsOffset+2+ii);
        StrCopy(PChar(Dummy3),PCHAR(lpCallList)+lpCallList^.dwCallsOffset+3+ii);
        ahCall:=Dummy[0]+(Dummy1[0]*$100)+(Dummy2[0]*$10000)+(Dummy3[0]*$1000000);
        ii:=ii+4;
        Item.Call.Handle:=ahCall;
        Item.Call.OnDevSpecific:=FOnDevSpecific;
        Item.Call.OnDevSpecificFeature:=FOnDevSpecificFeature;
        Item.Call.OnMonitorMedia:=FOnMonitorMedia;
        Item.Call.OnStateIdle:=FOnStateIdle;
        Item.Call.OnStateOffering:=FOnStateOffering;
        Item.Call.OnStateAccepted:=FOnStateAccepted;
        Item.Call.OnStateDialTone:=FOnStateDialTone;
        Item.Call.OnStateDialing:=FOnStateDialing;
        Item.Call.OnStateRingBack:=FOnStateRingBack;
        Item.Call.OnStateBusy :=FOnStateBusy;
        Item.Call.OnStateSpecialInfo:=FOnStateSpecialInfo;
        Item.Call.OnStateConnected:=FOnStateConnected;
        Item.Call.OnStateProceeding:=FOnStateProceeding;
        Item.Call.OnStateOnHold:=FOnStateOnHold;
        Item.Call.OnStateConferenced:=FOnStateConferenced;
        Item.Call.OnStateOnHoldPendConf:=FOnStateOnHoldPendConf;
        Item.Call.OnStateOnHoldPendTransf:=FOnStateOnHoldPendTransf;
        Item.Call.OnStateDisconnected:=FOnStateDisconnected;
        Item.Call.OnStateUnknown:=FOnStateUnknown;
        Item.Call.OnInfoOther:=FOnInfoStateOther;
        Item.Call.OnInfoDevSpecific:=FOnInfoStateDevSpecific;
        Item.Call.OnInfoBearerMode:=FOnINFOSTATEBEARERMODE;
        Item.Call.OnInfoRate :=FOnINFOSTATERATE;
        Item.Call.OnInfoMediaMode :=FOnINFOSTATEMEDIAMODE;
        Item.Call.OnInfoAppSpecific:=FOnINFOSTATEAPPSPECIFIC;
        Item.Call.OnInfoCallId:=FOnINFOSTATECALLID;
        Item.Call.OnInfoRelatedCallId:=FOnINFOSTATERELATEDCALLID;
        Item.Call.OnInfoOrigin:=FOnINFOSTATEORIGIN;
        Item.Call.OnInfoReason:=FOnINFOSTATEREASON;
        Item.Call.OnInfoCompletionId:=FOnINFOSTATECOMPLETIONID;
        Item.Call.OnInfoNumOwnerIncr:=FOnINFOSTATENUMOWNERINCR;
        Item.Call.OnInfoNumOwnerDecr:=FOnINFOSTATENUMOWNERDECR;
        Item.Call.OnInfoNumMonitors:=FOnINFOSTATENUMMONITORS;
        Item.Call.OnInfoTrunk:=FOnINFOSTATETRUNK;
        Item.Call.OnInfoCallerId:=FOnINFOSTATECALLERID;
        Item.Call.OnInfoCalledId:=FOnINFOSTATECALLEDID;
        Item.Call.OnInfoConnectedId:=FOnINFOSTATECONNECTEDID;
        Item.Call.OnInfoRedirectionId:=FOnINFOSTATEREDIRECTIONID;
        Item.Call.OnInfoRedirectingId:=FOnINFOSTATEREDIRECTINGID;
        Item.Call.OnInfoDisplay:=FOnINFOSTATEDISPLAY;
        Item.Call.OnInfoDisplay:=FOnINFOSTATEUSERUSERINFO;
        Item.Call.OnInfoHighLevelComp:=FOnINFOSTATEHIGHLEVELCOMP;
        Item.Call.OnInfoLowLevelComp:=FOnINFOSTATELOWLEVELCOMP;
        Item.Call.OnInfoChargingInfo:=FOnINFOSTATECHARGINGINFO;
        Item.Call.OnInfoTerminal:=FOnINFOSTATETERMINAL;
        Item.Call.OnInfoDialParams:=FOnINFOSTATEDIALPARAMS;
        Item.Call.OnInfoMonitorModes:=FOnINFOSTATEMONITORMODES;
        Item.Call.OnReply:=FOnReply;
        SetLength(Dummy,0);
        SetLength(Dummy1,0);
        SetLength(Dummy2,0);
        SetLength(Dummy3,0);
      end;
    end;
  finally
    FreeMem(lpCallList);
  end;
end;

{ TCallItem }

constructor TCallItem.Create(xCollection: TCollection);
begin
  inherited;
  FCall:=TTAPICall.Create(TCallList(xCollection).FOwner);
end;

destructor TCallItem.Destroy;
begin
 inherited;
end;

function TCallItem.GetCall: TTAPICall;
begin
  if Assigned(FCall)=False then FCall:=TTAPICall.Create(TCallList(Collection).FOwner);
  Result := FCall;
end;

function TCallItem.GetHandle: hCall;
begin
  Result:=0;
  if Assigned(FCall) then Result:=FCall.Handle;
end;

procedure TCallItem.SetHandle(const Value: hCall);
begin
  if Assigned(FCall)=False then
  begin
    FCall:=TTAPICall.Create(TCallList(Collection).FOwner);
  end;
  if Value <> FCall.Handle then
  begin
    FCall.Handle:=Value;
  end;
end;
{$ENDIF}
{$ENDIF}

end.
 