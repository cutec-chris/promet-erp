unit uTAPIPhone;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uPhones
  {$IFDEF WINDOWS}
  ,ExtCtrls ,TAPI, TAPIServices, TAPIAddress, TAPIDevices, TAPICall, TAPILines, Dialogs, Forms
  {$ENDIF}
  ;

{$IFDEF WINDOWS}
{$IFDEF CPU32}
type
  { TTAPIPhoneLine }

  TTAPIPhoneLine = class(TPhoneLine)
    procedure FCallInfoCallerId(Sender: TObject);
    procedure FCallStateAccepted(Sender: TObject; Rights: TLineCallPrivilege);
    procedure FCallStateBusy(Sender: TObject; BusyMode: TLineBusyMode;
      Rights: TLineCallPrivilege);
    procedure FCallStateConnected(Sender: TObject;
      ConnectedMode: TLineConnectedModes; Rights: TLineCallPrivilege);
    procedure FCallStateDisconnected(Sender: TObject;
      DisconnectedMode: TLineDisconnectMode; Rights: TLineCallPrivilege);
    procedure FCallStateIdle(Sender: TObject; Rights: TLineCallPrivilege);
    procedure FCallStateOffering(Sender: TObject;
      OfferingMode: TLineOfferingModes; Rights: TLineCallPrivilege);
    procedure FCallStateOnHold(Sender: TObject; Rights: TLineCallPrivilege);
    procedure FCallStateUnknown(Sender: TObject; Rights: TLineCallPrivilege);
    procedure FTimerTimer(Sender: TObject);
  private
    FLineDevice: TTAPILineDevice;
    FLine : TTAPILine;
    FAddress : array of TTAPIAddress;
    FCall : array of TTAPICall;
    FNewCall : TCall;
    FTimer : TTimer;
    function GetName: string;override;
  protected
    procedure DoAnswer(cID : string);override;
    procedure DoHangup(cID : string);override;
    procedure DoHold(cID : string);override;
    procedure DoContinue(cID : string);override;
  public
    function Connect : Boolean;override;
    constructor Create(ID : Integer);
    destructor Destroy;override;
  end;
  
  { TTAPILineHandler }

  TTAPILineHandler = class(TComponent)
    procedure LineServiceLineCreate(Sender: TObject; NewDeviceID: Dword);
    procedure LineServiceLineRemove(Sender: TObject; DeviceID: DWord);
  private
  public
    LineService : TTAPILineService;
    constructor Create;
    procedure Init;
    destructor Destroy;override;
  end;
{$ENDIF}
{$ENDIF}
  procedure RegisterPhoneLines;

{$IFDEF WINDOWS}
{$IFDEF CPU32}
var
  LineServiceHandler : TTAPILineHandler;
{$ENDIF}
{$ENDIF}

implementation

procedure RegisterPhoneLines;
begin
  {$IFDEF WINDOWS}
  {$IFDEF CPU32}
  LineServiceHandler := TTAPILineHandler.Create;
  LineServiceHandler.Init;
  {$ENDIF}
  {$ENDIF}
end;

{ TTAPILineHandler }

{$IFDEF WINDOWS}
{$IFDEF CPU32}

procedure TTAPILineHandler.LineServiceLineCreate(Sender: TObject;NewDeviceID: Dword);
begin

end;

procedure TTAPILineHandler.LineServiceLineRemove(Sender: TObject;
  DeviceID: DWord);
begin

end;

constructor TTAPILineHandler.Create;
begin
  LineService := TTAPILineService.Create(Application.MainForm);
  LineService.OnLineCreate:=@LineServiceLineCreate;
  LineService.OnLineRemove:=@LineServiceLineRemove;
  LineService.Active:=True;
end;

procedure TTAPILineHandler.Init;
var
  i: Integer;
  TempLineDevice: TTAPILineDevice;
begin
  for i:=0 to LineService.NumDevice -1 do
  begin
    try
      TempLineDevice:=TTAPILineDevice.Create(self);
      TempLineDevice.Service:=LineService;
      TempLineDevice.ID:=i;
      if mmInteractiveVoice in TempLineDevice.Caps.MediaModes then
        begin
          uPhones.Phones.Add(TTAPIPhoneLine.Create(i));
        end;
      TempLineDevice.Free;
    except
    end;
  end;
end;

destructor TTAPILineHandler.Destroy;
begin
  LineService.Active:=False;
  LineService.Free;
  inherited Destroy;
end;

{ TTAPIPhoneLine }

procedure TTAPIPhoneLine.FCallInfoCallerId(Sender: TObject);
var
  aCall: TCall;
  i: Integer;
begin
  for i := 0 to length(FCall) do
    if FCall[i] = Sender then
      break;
  if FCall[i] <> Sender then exit;
  aCall := TCall.Create(IntToStr(i));
  aCall.Parent := Self;
  if (TTAPICall(Sender).Info.Origin = coOutBound)
  or (TTAPICall(Sender).Info.Origin = coInternal) then
    aCall.From:=TTAPICall(Sender).Info.CalledID
  else
    aCall.From:=TTAPICall(Sender).Info.CallerID;
  Add(aCall);
  FNewCall := aCall;
end;

procedure TTAPIPhoneLine.FCallStateAccepted(Sender: TObject;
  Rights: TLineCallPrivilege);
var
  aCall: TCall;
  i: Integer;
begin
  for i := 0 to length(FCall) do
    if FCall[i] = Sender then
      break;
  if FCall[i] <> Sender then exit;
  aCall := Calls[IntToStr(i)];
  if Assigned(aCall) then
    aCall.Status := csInprogress;
end;

procedure TTAPIPhoneLine.FCallStateBusy(Sender: TObject;
  BusyMode: TLineBusyMode; Rights: TLineCallPrivilege);
var
  aCall: TCall;
  i: Integer;
begin
  for i := 0 to length(FCall) do
    if FCall[i] = Sender then
      break;
  if FCall[i] <> Sender then exit;
  aCall := Calls[IntToStr(i)];
  if Assigned(aCall) then
    aCall.Status := uPhones.csBusy;
end;

procedure TTAPIPhoneLine.FCallStateConnected(Sender: TObject;
  ConnectedMode: TLineConnectedModes; Rights: TLineCallPrivilege);
var
  aCall: TCall;
  i: Integer;
begin
  for i := 0 to length(FCall) do
    if FCall[i] = Sender then
      break;
  if FCall[i] <> Sender then exit;
  aCall := Calls[IntToStr(i)];
  if Assigned(aCall) then
    aCall.Status := csInProgress;
end;

procedure TTAPIPhoneLine.FCallStateDisconnected(Sender: TObject;
  DisconnectedMode: TLineDisconnectMode; Rights: TLineCallPrivilege);
var
  aCall: TCall;
  i: Integer;
begin
  for i := 0 to length(FCall) do
    if FCall[i] = Sender then
      break;
  if FCall[i] <> Sender then exit;
  aCall := Calls[IntToStr(i)];
  if Assigned(aCall) then
    aCall.Status := csFinished;
end;

procedure TTAPIPhoneLine.FCallStateIdle(Sender: TObject;
  Rights: TLineCallPrivilege);
var
  aCall: TCall;
  i: Integer;
begin
  for i := 0 to length(FCall) do
    if FCall[i] = Sender then
      break;
  if FCall[i] <> Sender then exit;
  aCall := Calls[IntToStr(i)];
  if Assigned(aCall) then
    aCall.Status := csFinished;
end;

procedure TTAPIPhoneLine.FCallStateOffering(Sender: TObject;
  OfferingMode: TLineOfferingModes; Rights: TLineCallPrivilege);
var
  aCall: TCall;
  i: Integer;
begin
  for i := 0 to length(FCall) do
    if FCall[i] = Sender then
      break;
  if FCall[i] <> Sender then exit;
  aCall := Calls[IntToStr(i)];
  if Assigned(aCall) then
    aCall.Status := csRinging;
end;

procedure TTAPIPhoneLine.FCallStateOnHold(Sender: TObject;
  Rights: TLineCallPrivilege);
var
  aCall: TCall;
  i: Integer;
begin
  for i := 0 to length(FCall) do
    if FCall[i] = Sender then
      break;
  if FCall[i] <> Sender then exit;
  aCall := Calls[IntToStr(i)];
  if Assigned(aCall) then
    aCall.Status := uPhones.csHold;
end;

procedure TTAPIPhoneLine.FCallStateUnknown(Sender: TObject;
  Rights: TLineCallPrivilege);
var
  aCall: TCall;
  i: Integer;
begin
  for i := 0 to length(FCall) do
    if FCall[i] = Sender then
      break;
  if FCall[i] <> Sender then exit;
  aCall := Calls[IntToStr(i)];
  if Assigned(aCall) then
    aCall.Status := uPhones.csUnknown;
end;

procedure TTAPIPhoneLine.FTimerTimer(Sender: TObject);
begin
  if FNewCall <> nil then
    if Assigned(OnCall) then
      begin
        OnCall(Self,FNewCall);
        FNewCall := nil;
      end;
end;

function TTAPIPhoneLine.GetName: string;
begin
  Result := FLineDevice.Caps.Name;
end;

procedure TTAPIPhoneLine.DoAnswer(cID: string);
begin
end;

procedure TTAPIPhoneLine.DoHangup(cID: string);
begin
end;

procedure TTAPIPhoneLine.DoHold(cID: string);
begin
end;

procedure TTAPIPhoneLine.DoContinue(cID: string);
begin
end;

function TTAPIPhoneLine.Connect: Boolean;
begin
  Result := True;
end;

constructor TTAPIPhoneLine.Create(ID: Integer);
var
  i: Integer;
begin
  inherited Create;
  FTimer := TTimer.Create(nil);
  FTimer.Interval := 100;
  FTimer.OnTimer:=@FTimerTimer;
  FTimer.Enabled := True;
  FLineDevice := TTAPILineDevice.Create(nil);
  FLineDevice.Service := LineServiceHandler.LineService;
  FLineDevice.ID := ID;
  FLine := TTAPILine.Create(Application.Mainform);
  FLine.Device := FLineDevice;
  FLine.CallPrivilege:=[cpOwner,cpMonitor];
  FLine.Active:=True;
  SetLength(FAddress,FLineDevice.Caps.NumAddresses);
  SetLength(FCall,FLineDevice.Caps.NumAddresses);
  for i := 0 to FLineDevice.Caps.NumAddresses-1 do
    begin
      FAddress[i] := TTAPIAddress.Create(Application.Mainform);
      FAddress[i].ID:=i;
      FAddress[i].Line := FLine;
      FCall[i] := TTAPICall.Create(Application.Mainform);
      FAddress[i].MonitorCall := FCall[i];
      FAddress[i].InboundCall := FCall[i];
      FCall[i].OnInfoCallerId:=@FCallInfoCallerId;
      FCall[i].OnStateIdle:=@FCallStateIdle;
      FCall[i].OnStateAccepted:=@FCallStateAccepted;
      FCall[i].OnStateBusy:=@FCallStateBusy;
      FCall[i].OnStateConnected:=@FCallStateConnected;
      FCall[i].OnStateDisconnected:=@FCallStateDisconnected;
      FCall[i].OnStateOnHold:=@FCallStateOnHold;
      FCall[i].OnStateOffering:=@FCallStateOffering;
      FCall[i].OnStateUnknown:=@FCallStateUnknown;
      FAddress[i].Active:=True;
    end;
end;

destructor TTAPIPhoneLine.Destroy;
var
  i: Integer;
begin
  FTimer.Free;
  for i := 0 to length(FAddress)-1 do
    begin
      FAddress[i].Free;
      FCall[i].Free;
    end;
  Setlength(FAddress,0);
  Setlength(FCall,0);
  FLine.Free;
  FLineDevice.Free;
  inherited Destroy;
end;

{$ENDIF}
{$ENDIF}
finalization

end.

