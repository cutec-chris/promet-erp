{******************************************************************************}
{*        Copyright 1999-2001 by J.Friebel all rights reserved.               *}
{*        Autor           :  Jörg Friebel                                     *}
{*        Compiler        :  Delphi 4 / 5                                     *}
{*        System          :  Windows NT / 2000                                *}
{*        Projekt         :  TAPI Komponenten (TAPI Version 1.4 bis 3.0)      *}
{*        Last Update     :  27.01.2002                                       *}
{*        Version         :  2.0.2                                            *}
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

unit TAPILines;

interface
{$IFDEF WINDOWS}
{$IFDEF CPU32}
{$IFDEF FPC}
{$MODE DELPHI}
{$ENDIF}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  TAPI,TAPISystem,TAPIHelpFunc,TAPIErr,TAPIServices,TAPIDevices,TAPICall;

{$INCLUDE TAPI.INC}

type
  {$IFDEF TAPI20}
  TLineOpenOption = (looSingleAddress,looProxy);
  TLineOpenOptions = Set of TLineOpenOption;
  {$ENDIF}

  TLineEventDevSpecific=procedure(Sender:TObject;dwParam1,dwParam2,dwParam3:DWord)of Object;
  TLineEventDevSpecificFeature=procedure(Sender:TObject;dwParam1,dwParam2,dwParam3:DWord)of Object;
  TLineEventReply=procedure(Sender:TObject;AsyncFunc:TAsyncFunc;Error:DWord)of Object;

type
  TTAPILine = class(TTAPIComponent)
  private
    FDevice:TTAPILineDevice;
    FMediaModes:LongInt;
    FLineHandle:HLine;
    FDevStatus:TDeviceStatus;
    FActive:Boolean;
    FCallPrivilege:TLineCallPrivilege;
    FCallParams: TCallParams;
    {$IFDEF TAPI20}
    FOpenOptions:TLineOpenOptions;
    {$ENDIF}
    {Ereignise }
    FOnLineClose: TNotifyEvent;
    FOnDevSpecific: TLineEventDevSpecific;
    FOnDevSpecificFeature: TLineEventDevSpecificFeature;
    FOnLineReply: TLineEventReply;
    FBeforeOpen: TNotifyEvent;
    FAfterOpen: TNotifyEvent;
    function GetActive: Boolean;
    function GetLineCallPrivilege:LongWord;
    procedure SetActive(const Value: Boolean);
    function GetMediaModes: TLineMediaModes;
    procedure SetMediaModes(const Value: TLineMediaModes);
    function GetDevStatus: TDeviceStatus;
    function GetPhoneID: DWord;
    function GetProviderID: DWord;
    procedure SetCallParams(const Value: TCallParams);
  protected
    { Protected-Deklarationen }
    procedure Open;
    procedure Close;
    procedure LineReply(AsyncFunc:TAsyncFunc;Error:DWord);
    procedure Notification(AComponent: TComponent;
       Operation: TOperation); override;
  public
    { Public-Deklarationen }
    constructor Create(AOwner:TComponent);override;
    destructor Destroy;override;
    property Active:Boolean read GetActive write SetActive default False;
    property Handle:HLine read FLineHandle;
    procedure LineDevStateChange(hDevice,Param1,Param2,Param3:LongWord);
    procedure DevSpecificChange(dwParam1,dwParam2,dwParam3:DWord);
    procedure DevSpecificFeatureChange(dwParam1,dwParam2,dwParam3:DWord);
    procedure lineCallState(hDevice,State,Mode,Rights:LongWord);
    procedure LineIsClose(hDevice:LongWord);
    procedure PerformMsg(Msg: TCMTAPI);override;
    property Status:TDeviceStatus read GetDevStatus;
    property PhoneID:DWord read GetPhoneID;
    property ProviderID:DWord read GetProviderID;
  published
    { Published-Deklarationen }

    property Device:TTAPILineDevice read  FDevice write  FDevice;
    property CallPrivilege:TLineCallPrivilege  read FCallPrivilege write FCallPrivilege default [cpNone];
    {$IFDEF TAPI20}
    property OpenOptions:TLineOpenOptions read FOpenOptions write FOpenOptions default [];
    {$ENDIF}
    property BeforeOpen: TNotifyEvent read FBeforeOpen write FBeforeOpen;
    property AfterOpen: TNotifyEvent read FAfterOpen write FAfterOpen;
    property MediaModes:TLineMediaModes read GetMediaModes write SetMediaModes default [mmInteractiveVoice];
    property CallMapperParams: TCallParams read FCallParams write SetCallParams;
    property OnClose:TNotifyEvent read FOnLineClose write FOnLineClose;
    property OnReply:TLineEventReply read FOnLineReply write FOnLineReply;
    property OnDevSpecific:TLineEventDevSpecific read FOnDevSpecific write FOnDevSpecific;
    property OnDevSpecificFeature:TLineEventDevSpecificFeature read FOnDevSpecificFeature write FOnDevSpecificFeature;
  end;
{$IFDEF TAPI20}
function LineOpenOptionsToInt(Value:TLineOpenOptions):LongWord;

{$ENDIF}

procedure Register;

{$ENDIF}
{$ENDIF}

implementation

{$IFDEF WINDOWS}
{$IFDEF CPU32}

uses {$IFDEF VER120}D4Comp,{$ENDIF}TAPIAddress;

procedure Register;
begin
{$IFDEF TAPI30}
  RegisterComponents('TAPI30', [TTAPILine]);
{$ELSE}
{$IFDEF TAPI22}
  RegisterComponents('TAPI22', [TTAPILine]);
{$ELSE}
{$IFDEF TAPI21}
  RegisterComponents('TAPI21', [TTAPILine]);
{$ELSE}
{$IFDEF TAPI20}
  RegisterComponents('TAPI20', [TTAPILine]);
{$ELSE}
  RegisterComponents('TAPI', [TTAPILine]);
{$ENDIF}
{$ENDIF}
{$ENDIF}
{$ENDIF}
end;


{$IFDEF TAPI20}
function LineOpenOptionsToInt(Value:TLineOpenOptions):LongWord;
begin
  Result:=0;
  if looSingleAddress in Value then Result:=Result+LINEOPENOPTION_SINGLEADDRESS;
  if looProxy in Value then Result:=Result+LINEOPENOPTION_PROXY;
end;
{$ENDIF}




{ TTAPILine }

constructor TTAPILine.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLineHandle:=0;
  MediaModes:=[mmInteractiveVoice];
end;

destructor TTAPILine.Destroy;
begin
  Active:=False;
  inherited Destroy;
end;

function TTAPILine.GetActive: Boolean;
begin
  Result:=FActive;
end;

procedure TTAPILine.LineIsClose(hDevice:LongWord);
begin
  if Assigned(FOnLineClose) then FOnLineClose(self);
  FActive:=False;
  FLineHandle:=0;
end;

procedure TTAPILine.LineDevStateChange(hDevice, Param1, Param2,
  Param3: LongWord);
begin
  if Assigned(FDevStatus)then FreeAndNil(FDevStatus);
  if Assigned(FDevice) then FDevice.StateChange(hDevice,Param1,Param2,Param3);
end;

procedure TTAPILine.LineReply(AsyncFunc:TAsyncFunc;Error:DWord);
begin
  if Assigned(FOnLineReply) then FOnLineReply(self,AsyncFunc,Error);
end;

procedure TTAPILine.LineCallState(hDevice, State, Mode,
  Rights: DWord);
begin
  {$IFDEF DEBUG}
   OutputDebugString(PChar('TAPILine.LineCallState('+IntToStr(hDevice)+'),State:'+
    IntToStr(State)+' Mode:'+IntToStr(Mode)+'Rights: '+IntToStr(Rights)));
  {$ENDIF}
  {$IFDEF DEBUG}
  case Rights of
    0:                         OutputDebugString('ActCall');
    LINECALLPRIVILEGE_OWNER:   OutputDebugString('InBoundCall');
    LINECALLPRIVILEGE_MONITOR: OutputDebugString('MonitorCall');
  end;
  {$ENDIF}
end;

function TTAPILine.GetLineCallPrivilege: LongWord;
begin
  Result:=CallPrivilegeToInt(FCallPrivilege){$IFDEF TAPI20} or LineOpenOptionsToInt(FOpenOptions){$ENDIF};
end;

function TTAPILine.GetMediaModes: TLineMediaModes;
begin
  Result:=IntToMediaModes(FMediaModes);
end;

procedure TTAPILine.Open;
var R:Longint;
    CParams:PLineCallParams;
begin
  CParams:=nil;
  FLineHandle:=0;
  if Assigned(FCallParams) then
  begin
    GetMem(CParams,SizeOf(TLineCallParams)+1000);
    CParams^.dwTotalSize:=SizeOf(TLineCallParams)+1000;
    {$IFDEF TAPI20}
    if looProxy in FOpenOptions then FCallParams.UseProxy:=True
    else
      FCallParams.UseProxy:=False;
    {$ENDIF}
    FCallParams.GetParamStruct(CParams);
  end;
  try
    if FDevice.LineMapper then
      R:=LineOpen(FDevice.Service.Handle,LINEMAPPER,@FLineHandle,FDevice.APIVersion,FDevice.Service.APIExtVersion,ComponentIndex,GetLineCallPrivilege,FMediaModes,CParams)
    else
      R:=LineOpen(FDevice.Service.Handle,FDevice.ID,@FLineHandle,FDevice.APIVersion,FDevice.Service.APIExtVersion,ComponentIndex,GetLineCallPrivilege,FMediaModes,CParams);
    if R<>0 then
    begin
      RaiseTAPILineError(R);
    end;
  finally
    if Assigned(FCallParams) then FreeMem(CParams);
  end;
end;

procedure TTAPILine.SetActive(const Value: Boolean);
var Temp:TComponent;
    I:Integer;
begin
  if Value<> FActive then
  begin
    if Value=true then
    begin
      if Assigned(FDevice) then
      begin
        if Assigned(FBeforeOpen) then FBeforeOpen(self);
        Open;
        for I:=Owner.ComponentCount -1 downto 0 do
        begin
          Temp:=Owner.Components[i];
          if (Temp is TTAPIAddress) then
          begin
            if TTAPIAddress(Temp).Line=self then
            begin
              FActive:=Value;
              try
              TTAPIAddress(Temp).SetStatusMessages;
              //Exit;
              except
                // No Error Msg
              end;
            end;
          end;
        end;
        FActive:=Value;
        if Assigned(FAfterOpen) then FAfterOpen(self);
      end
      else
        Raise(ELineError.CreateFmt('Zuweisung LineDevice von %s fehlt',[Name]));
    end
    else
    begin
      Close;
    end;
  end;
  FActive:=Value;
end;

procedure TTAPILine.SetMediaModes(const Value: TLineMediaModes);
begin
  FMediaModes:=MediaModesToInt(Value);
end;

procedure TTAPILine.Close;
var R:LongInt;
begin
  try
    If (FLineHandle <> 0)then
    begin
      R:=LineClose(FLineHandle);
      if R<-1 then
      begin
        //RaiseTAPILineError(R);
      end;
    end;
  finally
    FLineHandle:=0;
    FreeAndNil(FDevStatus);
  end;
end;

function TTAPILine.GetDevStatus: TDeviceStatus;
begin
  FreeAndNil(FDevStatus);
  If Assigned(FDevStatus)=false then FDevStatus:=TDeviceStatus.Create(Handle);
  Result:=FDevStatus;
end;

procedure TTAPILine.DevSpecificChange(dwParam1, dwParam2, dwParam3: DWord);
begin
  if Assigned(FOnDevSpecific) then FOnDevSpecific(self,dwParam1,dwParam2,dwParam3);
end;

procedure TTAPILine.DevSpecificFeatureChange(dwParam1, dwParam2,
  dwParam3: DWord);
begin
  if Assigned(FOnDevSpecificFeature) then FOnDevSpecificFeature(self,dwParam1,
    dwParam2,dwParam3);
end;

function TTAPILine.GetPhoneID: DWord;
var R:LongInt;
    varstr:PVarString;
    id:DWORD;
    Dummy:Array[0..3] of Char;
begin
  id:=INVALID_Handle_Value;
  varstr:=PVARSTRING(AllocMem(SizeOf(TVarString)+1000));
  try
    varstr^.dwTotalSize:=SizeOf(TVARSTRING)+1000;
    R:=LineGetID(Handle,0,0,LINECALLSELECT_LINE,VarStr,'tapi/phone');
    if R<-1 then
    begin
      RaiseTAPILineError(R);
    end;
    if VarStr.dwStringFormat=STRINGFORMAT_BINARY then
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

function TTAPILine.GetProviderID: DWord;
var R:LongInt;
    varstr:PVarString;
    id:DWORD;
    Dummy:Array[0..3] of Char;
begin
  id:=INVALID_Handle_Value;
  varstr:=PVARSTRING(AllocMem(SizeOf(TVarString)+1000));
  try
    varstr^.dwTotalSize:=SizeOf(TVARSTRING)+1000;
    R:=LineGetID(Handle,0,0,LINECALLSELECT_LINE,VarStr,'tapi/providerid');
    if R<-1 then
    begin
      RaiseTAPILineError(R);
    end;
    if VarStr.dwStringFormat=STRINGFORMAT_BINARY then
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

procedure TTAPILine.PerformMsg(Msg: TCMTAPI);
begin
  inherited;
  with Msg.TAPIRec^ do
  begin

    if Handle=hDevice then
    begin
      case dwMsg of
        Line_Close:LineIsClose(Msg.TAPIRec.hDevice);
        Line_DevSpecific:DevSpecificChange(dwParam1,dwParam2,dwParam3);
        Line_DevSpecificFeature:DevSpecificFeatureChange(dwParam1,dwParam2,dwParam3);
        Line_Reply:LineReply(Msg.AsyncFunc^,dwParam2);
        Line_LineDevState:if dwCallBackInstance <> 0 then LineDevStateChange(hDevice,dwParam1,dwParam2,dwParam3);
      end;
    end;
  end;
end;

procedure TTAPILine.SetCallParams(const Value: TCallParams);
begin
  FCallParams := Value;
end;

procedure TTAPILine.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
 inherited Notification(AComponent,Operation);
  if (Operation = opRemove) and (AComponent=FDevice) then
  begin
    FDevice:=nil;
  end;
  if (Operation = opRemove) and (AComponent=FCallParams) then
  begin
    FCallParams:=nil;
  end;

end;

{$ENDIF}
{$ENDIF}
end.