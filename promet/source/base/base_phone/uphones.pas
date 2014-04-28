unit uPhones;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;
  
type
  TCallStatus = (csUnknown,csRinging,csInprogress,csFinished,csRouting,csFailed,csHold,csRefused,csBusy);
  TCallStatusChangeEvent = procedure(Sender : TObject;NewStatus : TCallStatus) of object;

  TPhoneLine = class;

  { TCall }

  TCall = class
  private
    FFrom : string;
    FIDX : string;
    FCallStatus: TCallStatus;
    FOnStatusChanged: TCallStatusChangeEvent;
    FStart: TDateTime;
    FDuration : TDateTime;
    FParent : TPhoneLine;
    function GetDuration: TDateTime;
    procedure SetCallStatus(const AValue: TCallStatus);
  public
    constructor Create(IDX : string);
    procedure Answer;
    procedure Hangup;
    procedure Hold;
    procedure Continue;
    property Status : TCallStatus read FCallStatus write SetCallStatus;
    property From : string read fFrom write fFrom;
    property Parent : TPhoneLine read FParent write FParent;
    property Start : TDateTime read FStart;
    property Duration : TDateTime read GetDuration;
    property OnStatusChanged : TCallStatusChangeEvent read FOnStatusChanged write FOnStatusChanged;
  end;
  
  TCallEvent = procedure(Sender : TObject;Call : TCall) of object;

  { TPhoneLine }

  TPhoneLine = class(TList)
  private
    FCallEvent: TCallEvent;
    function GetCall(ID : string): TCall;
  protected
    function GetName: string;virtual;abstract;
    procedure DoAnswer(cID : string);virtual;abstract;
    procedure DoHangup(cID : string);virtual;abstract;
    procedure DoHold(cID : string);virtual;abstract;
    procedure DoContinue(cID : string);virtual;abstract;
  public
    property Calls[ID : string] : TCall read GetCall;
    function Connect : Boolean;virtual;abstract;
    function PlaceCall(CallTo : string) : TCall;virtual;abstract;
    property OnCall : TCallEvent read FCallEvent write FCallEvent;
    property Name : string read GetName;
  end;

  { TPhones }

  TPhones = class(TList)
    procedure ItemCall(Sender: TObject; Call: TCall);
  private
    FCallEvent: TCallEvent;
    function GetLine(Index : Integer): TPhoneLine;
  public
    function Add(Item: TPhoneLine): Integer;
    property Phones[Index : Integer] : TPhoneLine read GetLine;
    property OnCall : TCallEvent read FCallEvent write FCallEvent;
    constructor Create;
    destructor Destroy; override;
  end;
  
var
  Phones : TPhones;

implementation

uses uSkypePhone
{$IFDEF WINDOWS}
    ,uTAPIPhone
{$ENDIF}
    ;

{ TPhones }

procedure TPhones.ItemCall(Sender: TObject; Call: TCall);
begin
  if Assigned(FCallEvent) then
    FCallEvent(Sender,Call);
end;

function TPhones.GetLine(Index : Integer): TPhoneLine;
begin
  Result := TPhoneLine(Items[Index]);
end;

function TPhones.Add(Item: TPhoneLine): Integer;
begin
  Result := inherited Add(Item);
  Item.OnCall:=@ItemCall;
end;

constructor TPhones.Create;
begin
  inherited Create;
end;

destructor TPhones.Destroy;
begin
  inherited Destroy;
end;

{ TCall }

procedure TCall.SetCallStatus(const AValue: TCallStatus);
begin
  if Assigned(FOnStatusChanged) then
    FOnStatusChanged(Self,AValue);
  if FCallStatus=AValue then exit;
  FCallStatus:=AValue;
  if AValue = csInprogress then FStart := Now();
  if AValue = csFinished then FDuration := Now()-FStart;
end;

function TCall.GetDuration: TDateTime;
begin
  if FCallStatus = csInProgress then
    Result := Now()-FStart
  else
    Result := FDuration;
end;

constructor TCall.Create(IDX: string);
begin
  FIDX := IDX;
  FCallStatus := csUnknown;
  FDuration := 0;
  FStart := 0;
end;

procedure TCall.Answer;
begin
  FParent.DoAnswer(FIDX);
end;

procedure TCall.Hangup;
begin
  FParent.DoHangup(FIDX);
end;

procedure TCall.Hold;
begin
  FParent.DoHold(FIDX);
end;

procedure TCall.Continue;
begin
  FParent.DoContinue(FIDX);
end;

{ TPhoneLine }

function TPhoneLine.GetCall(ID : string): TCall;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count-1 do
    if Assigned(Items[i])  and (TCall(Items[i]).FIDX = ID) then
      Result := TCall(Items[i]);
end;

initialization
  Phones := TPhones.Create;
  
finalization
  Phones.Free;

end.

