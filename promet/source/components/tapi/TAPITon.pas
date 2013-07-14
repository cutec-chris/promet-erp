{******************************************************************************}
{*        Copyright 1999-2001 by J.Friebel all rights reserved.               *}
{*        Autor           :  Jörg Friebel                                     *}
{*        Compiler        :  Delphi 4 / 5                                     *}
{*        System          :  Windows NT / 2000                                *}
{*        Projekt         :  TAPI Komponenten (TAPI Version 1.4 bis 3.0)      *}
{*        Last Update     :  19.05.2001                                       *}
{*        Version         :  0.01                                              *}
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

unit TAPITon;

interface
{$IFDEF WINDOWS}
{$IFDEF CPU32}

{$IFDEF FPC}
{$MODE DELPHI}
{$ENDIF}

uses Windows,Classes,TAPI, TAPISystem;

{$INCLUDE TAPI.INC}

type
  TLineGenerateTerm = (lgntDone,lgntCancel);
  
  TLineDigitMode = (ldmDTMF,ldmDTMFEnd,ldmPulse);
  TLineDigitmodes =set of TLineDigitMode;

  TLineToneMode = (tmCustom,tmRingback,tmBusy,tmBeep,tmBilling);
  TLineToneModes = set of TLineToneMode;

  TGenerateEvent=procedure(Sender: TObject;GenerateTermination:TLineGenerateTerm;TickCount:DWord)of Object;
  TMonitorDigitEvent=procedure(Sender: TObject;LastDigit:Char;Mode:TLineDigitModes;TickCount:DWord)of Object;
  TGatherDigitsEvent=procedure(Sender: TObject;GatherTermination,TickCount:DWord)of Object;

  TTAPIDigits=class(TTAPIComponent)
  private
    FCallHandle:THandle;
    FDigits:String;
    FMDigits:String;
    FDigitModes:TLineDigitModes;
    FTerminationDigits:String;
    FFirstDigitTimeOut:DWord;
    FInterDigitTimeOut:DWord;
    FDuration:DWord;
    FMonitoring:Boolean;
    FNumGatherDigits:Integer;
    FOnGenerate:TGenerateEvent;
    FOnMonitor:TMonitorDigitEvent;
    FOnGatherDigits:TGatherDigitsEvent;
    function GetMonitorDigits: Boolean;
    procedure SetMonitorDigits(const Value: Boolean);
  protected
    
  public
    constructor Create(AOwner: TComponent);override;
    destructor Destroy;override;
    procedure GenerateDigit;
    procedure GatherDigits;
    procedure Generate(hDevice:hCall;GenerateTermination,Tickcount:DWord);
    procedure GatherDigit(var GatherTermination,TickCount: DWord);
    procedure Monitor(LastDigit:Char;Mode:TLineDigitModes; TickCount: DWord);
    property Monitoring:Boolean read GetMonitorDigits write SetMonitorDigits default False;
    property CallHandle:THandle read FCallHandle write FCallHandle;
    procedure PerformMsg(Msg: TCMTAPI);override;
  published
    property Duration:DWord read FDuration write FDuration;
    property DigitModes:TLineDigitModes read FDigitModes write FDigitModes default [ldmDTMF];
    property GenerateDigits:String read FDigits write FDigits;
    property MonitorDigits:String read FMDigits write FMDigits;
    property OnGenerate:TGenerateEvent read FOnGenerate write FOnGenerate;
    property OnMonitor:TMonitorDigitEvent read FOnMonitor write FOnMonitor;
    property OnGatherDigits:TGatherDigitsEvent read FOnGatherDigits write FOnGatherDigits;
    property NumGatherDigits:Integer read FNumGatherDigits write FNumGatherDigits default 0;
    property FirstDigitTimeOut:DWord read FFirstDigitTimeOut write FFirstDigitTimeOut;
    property InterDigitTimeOut:DWord read FInterDigitTimeOut write FInterDigitTimeOut;
    property TerminationDigits:String read FTerminationDigits write FTerminationDigits;
  end;


  TGenerateTone=class(TCollectionItem)
  private
    FFrequency,
    FCadenceOn,
    FCadenceOff,
    FVolume:DWord;
  public
    constructor Create(Collection: TCollection);override;
    function GetNamePath:  string;override ;
  published
    property Frequency:DWord read FFrequency write FFrequency default 0;
    property CadenceOn:DWord read FCadenceOn write FCadenceOn default 0;
    property CadenceOff:DWord read FCadenceOff write FCadenceOff default 0;
    property Volume:DWord read FVolume write FVolume default 0;
  end;

  TMonitorTone=class(TCollectionItem)
  private
    FAppSpecific,
    FDuration,
    FFrequency1,
    FFrequency2,
    FFrequency3:DWord;
    FMonitorEvent:TNotifyEvent;//TMonitorToneEvent;
  public
    constructor Create(Collection: TCollection);override;
    function GetNamePath:  string;override ;
  published
    property AppSpecific:DWord  read FAppSpecific write FAppSpecific default 0;
    property Duration:DWord read FDuration write FDuration default 0;
    property Frequency1:DWord read FFrequency1 write FFrequency1 default 0;
    property Frequency2:DWord read FFrequency2 write FFrequency2 default 0;
    property Frequency3:DWord read FFrequency3 write FFrequency3 default 0;
    property OnMonitor:TNotifyEvent read FMonitorEvent write FMonitorEvent;
  end;

  TTAPITones =class;

  PGenerateTones=^TGenerateTones;
  TGenerateTones=Class(TCollection)
  private
    FOwner:TTAPITones;
    function GetItem(Index: Integer): TGenerateTone;
    procedure SetItem(Index: Integer; const Value: TGenerateTone);
    function GetStruct: PLineGenerateTone;
  protected
    function GetOwner: TPersistent; override;
  public
    constructor Create(AOwner:TTAPITones);
    property Items[Index:Integer]:TGenerateTone read GetItem write SetItem;default;
    property Struct:PLineGenerateTone read GetStruct;
  end;

  PMonitorTones=^TMonitorTones;
  TMonitorTones=Class(TCollection)
  private
      FOwner:TTAPITones;
      function GetItem(Index: Integer): TMonitorTone;
      procedure SetItem(Index: Integer; const Value: TMonitorTone);
    function GetStruct: PLineMonitorTone;
    protected
      function GetOwner: TPersistent; override;
    public
      constructor Create(AOwner:TTAPITones);
      property Items[Index:Integer]:TMonitorTone read GetItem write SetItem;default;
      property Struct:PLineMonitorTone read GetStruct;
   end;

  TTAPITones=class(TTAPIComponent)
  private
    FCallHandle:hCall;
    FTones:TGenerateTones;
    FMTones:TMonitorTones;
    FToneMode:DWord;
    FToneModes:TLineToneMode;
    FDuration:DWord;
    FMonitoring:Boolean;
    FOnGenerate:TGenerateEvent;
    function GetToneMode: TLineToneMode;
    procedure SetToneMode(const Value: TLineToneMode);
    function GetMonitorTones: Boolean;
    procedure SetMonitorTones(const Value: Boolean);
  protected
    
  public
    constructor Create(AOwner: TComponent);override;
    destructor Destroy;override;
    procedure GenerateTones;
    procedure Generate(hCall,GenerateTermination,Tickcount:DWord);
    procedure Monitor(hCall,AppSpecific,TickCount:DWord);
    procedure Break;
    property Monitoring:Boolean read GetMonitorTones write SetMonitorTones default False;
    property CallHandle:THandle read FCallHandle write FCallHandle;
    procedure PerformMsg(Msg: TCMTAPI);override;
  published
    property Duration:DWord read FDuration write FDuration;
    property ToneMode:TLineToneMode read GetToneMode write SetToneMode;
    property GenerateTone:TGenerateTones read FTones write FTones;
    property MonitorTone:TMonitorTones read FMTones write FMTones;
    property OnGenerate:TGenerateEvent read FOnGenerate write FOnGenerate;
  end;

function IntToGenerateTerm(Value:LongWord):TLineGenerateTerm;
function IntToDigitModes(Value:LongWord):TLineDigitModes;
function DigitModesToInt(Value:TLineDigitModes):LongWord;
function IntToToneModes(Value:LongWord):TLineToneModes;
function ToneModeToInt(Value:TLineToneMode):LongWord;

procedure Register;

{$ENDIF}
{$ENDIF}

implementation

{$IFDEF WINDOWS}
{$IFDEF CPU32}

uses TAPIErr;

procedure Register;
begin
{$IFDEF TAPI30}
  RegisterComponents('TAPI30', [TTAPITones]);
  RegisterComponents('TAPI30', [TTAPIDigits]);
{$ELSE}
{$IFDEF TAPI22}
  RegisterComponents('TAPI22', [TTAPITones]);
  RegisterComponents('TAPI22', [TTAPIDigits]);
{$ELSE}
{$IFDEF TAPI21}
  RegisterComponents('TAPI21', [TTAPITones]);
  RegisterComponents('TAPI21', [TTAPIDigits]);
{$ELSE}
{$IFDEF TAPI20}
  RegisterComponents('TAPI20', [TTAPITones]);
  RegisterComponents('TAPI20', [TTAPIDigits]);
{$ELSE}
  RegisterComponents('TAPI', [TTAPITones]);
  RegisterComponents('TAPI', [TTAPIDigits]);
{$ENDIF}
{$ENDIF}
{$ENDIF}
{$ENDIF}
end;

function IntToGenerateTerm(Value:LongWord):TLineGenerateTerm;
begin
  Result:=lgntCancel;
  case Value of
    LINEGENERATETERM_DONE:Result:=lgntDone;
    LINEGENERATETERM_CANCEL:Result:=lgntCancel;
  end;
end;

function IntToDigitModes(Value:LongWord):TLineDigitModes;
begin
  Result:=[];
  IF (Not((LINEDIGITMODE_PULSE xor $FFFFFFFF) or Value ))=0 then Result:=Result+[ldmPulse];
  IF (Not((LINEDIGITMODE_DTMF xor $FFFFFFFF) or Value))=0 then Result:=Result+[ldmDTMF];
  IF (Not((LINEDIGITMODE_DTMFEND xor $FFFFFFFF) or Value))=0 then Result:=Result+[ldmDTMFEnd];
end;



function DigitModesToInt(Value:TLineDigitModes):LongWord;
begin
  Result:=0;
  IF ldmPulse in Value then Result:=Result or LINEDIGITMODE_PULSE;
  IF ldmDTMF in Value then Result:=Result or LINEDIGITMODE_DTMF;
  IF ldmDTMFEnd in Value then Result:=Result or LINEDIGITMODE_DTMFEND;
end;

function IntToToneModes(Value:LongWord):TLineToneModes;
begin
  Result:=[];
  IF (Not((LINETONEMODE_BEEP xor $FFFFFFFF) or Value ))=0 then Result:=Result+[tmBeep];
  IF (Not((LINETONEMODE_CUSTOM xor $FFFFFFFF) or Value ))=0 then Result:=Result+[tmCustom];
  IF (Not((LINETONEMODE_RINGBACK xor $FFFFFFFF) or Value ))=0 then Result:=Result+[tmRingBack];
  IF (Not((LINETONEMODE_BUSY xor $FFFFFFFF) or Value ))=0 then Result:=Result+[tmBusy];
  IF (Not((LINETONEMODE_BILLING xor $FFFFFFFF) or Value ))=0 then Result:=Result+[tmBilling];
end;

function ToneModeToInt(Value:TLineToneMode):LongWord;
begin
  Result:=0;
  case Value of
    tmCUSTOM:Result:=LINETONEMODE_CUSTOM;
    tmRINGBACK:Result:=LINETONEMODE_RINGBACK;
    tmBUSY:Result:=LINETONEMODE_BUSY;
    tmBEEP:Result:=LINETONEMODE_BEEP;
    tmBILLING:Result:=LINETONEMODE_BILLING;
  end;
end;


{ TTAPIDigits }

constructor TTAPIDigits.Create(AOwner: TComponent);
begin
  inherited;
  FMonitoring:=False;
  NumGatherDigits:=0;
  FDigitModes:=[ldmDTMF];
end;

destructor TTAPIDigits.Destroy;
begin
  inherited;

end;

procedure TTAPIDigits.GatherDigits;
var R:Longint;
begin
  if FCallHandle <> DWORD(-1) then
  begin
    R:=LineGatherDigits(FCallHandle,DigitModesToInt(FDigitModes),PChar(FDigits),
    FNumGatherDigits,PChar(FTerminationDigits),FFirstDigitTimeout,FInterDigitTimeOut);
    if R<>0 then RaiseTAPILineError(R);
  end;
end;

procedure TTAPIDigits.Generate(hDevice: hCall; GenerateTermination,
  Tickcount: DWord);
begin
   if Assigned(FOnGenerate) then FOnGenerate(self,IntToGenerateTerm(GenerateTermination),TickCount);
end;

procedure TTAPIDigits.GenerateDigit;
var R:Longint;
begin
  if FCallHandle <> DWORD(-1) then
  begin
    R:=LineGenerateDigits(FCallHandle,DigitModesToInt(FDigitModes),PChar(FDigits),FDuration);
    if R<>0 then RaiseTAPILineError(R);
  end;
end;


function TTAPIDigits.GetMonitorDigits: Boolean;
begin
  Result:=FMonitoring;
end;



procedure TTAPIDigits.GatherDigit(var GatherTermination,
  TickCount: DWord);
begin
 if Assigned(FOnGatherDigits) then FOnGatherDigits(self,GatherTermination,TickCount);
end;

procedure TTAPIDigits.Monitor(LastDigit:Char;Mode:TLineDigitModes; TickCount: DWord);
begin
  if Assigned(FOnMonitor) then FOnMonitor(self,LastDigit,Mode,TickCount);
end;

procedure TTAPIDigits.SetMonitorDigits(const Value: Boolean);
var R:LongInt;
begin
  if Value <> FMonitoring then
  begin
    if Value then
    begin
      R:=LineMonitorDigits(FCallHandle,DigitModesToInt(FDigitModes));
      if R<>0 then
      begin
        FMonitoring:=False;
        RaiseTAPILineError(R)
      end
      else FMonitoring:=True;
    end
    else
    begin
      R:=LineMonitorDigits(FCallHandle,0);
      if R<>0 then
      begin
        FMonitoring:=True;
        RaiseTAPILineError(R)
      end
      else FMonitoring:=False;
    end;
  end;  
end;





procedure TTAPIDigits.PerformMsg(Msg: TCMTAPI);
begin
  inherited;
  with Msg.TAPIRec^ do
  begin
    if FCallHandle=hDevice then
    begin
      case dwMsg of
        LINE_GATHERDIGITS:GatherDigit(dwParam1,dwParam3);
        LINE_GENERATE:Generate(hDevice,dwParam1,dwParam3);
        LINE_MONITORDIGITS:Monitor(Char(Lo(dwParam1)),IntToDigitModes(dwParam2),dwParam3);
      end;
    end;
  end;
end;

{ TGenerateTone }

constructor TGenerateTone.Create;
begin
  inherited Create(Collection);
end;

function TGenerateTone.GetNamePath: string;
begin
  Result:= inherited GetNamePath;
end;


{ TGenerateTones }

constructor TGenerateTones.Create;
begin
  inherited Create(TGenerateTone);
  FOwner:=AOwner;
end;



function TGenerateTones.GetItem(Index: Integer): TGenerateTone;
begin
  Result:=TGenerateTone(inherited Items[Index]);
end;

function TGenerateTones.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

function TGenerateTones.GetStruct: PLineGenerateTone;
var TonesStruct:Array of TLINEGENERATETONE;
    i:Integer;
begin
  if (Count) > 0 then
  begin
    SetLength(TonesStruct,Count);
    for i:=0 to Count-1 do
    begin
      TonesStruct[i].dwFrequency :=Items[i].Frequency;
      TonesStruct[i].dwCadenceOn:=Items[i].CadenceOn;
      TonesStruct[i].dwCadenceOff:=Items[i].CadenceOff;
      TonesStruct[i].dwVolume:=Items[i].Volume;
    end;
    result:=@TonesStruct;
  end
  else result:=nil;
end;

procedure TGenerateTones.SetItem(Index: Integer;
  const Value: TGenerateTone);
begin
  inherited SetItem(Index, Value);
end;


{ TTAPITones }

procedure TTAPITones.Break;
var R:Longint;
begin
  if FCallHandle <> DWord(-1) then
  begin
    R:=lineGenerateTone(FCallHandle,0,FDuration,GenerateTone.Count,GenerateTone.Struct);
    if DWord(R)>DWord($80000000) then
    begin
      RaiseTAPILineError(R);
    end;
  end;
end;

constructor TTAPITones.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTones:=TGenerateTones.Create(self);
  FMTones:=TMonitorTones.Create(self);
  FMonitoring:=False;
end;

destructor TTAPITones.Destroy;
begin
  FMTones.Free;
  FTones.Free;
  inherited Destroy;
end;

procedure TTAPITones.GenerateTones;
var R:Longint;
begin
  if FCallHandle <> DWord(-1) then
  begin
    R:=lineGenerateTone(FCallHandle,FToneMode,FDuration,GenerateTone.Count,GenerateTone.Struct);
    if DWord(R)>DWord($80000000) then
    begin
      RaiseTAPILineError(R);
    end;
  end;
end;

procedure TTAPITones.Generate(hCall, GenerateTermination,
  Tickcount: DWord);
begin
  if Assigned(FOnGenerate) then FOnGenerate(self,IntToGenerateTerm(GenerateTermination),TickCount);
end;

function TTAPITones.GetToneMode: TLineToneMode;
begin
  Result:=FToneModes;
end;

procedure TTAPITones.SetToneMode(const Value: TLineToneMode);
begin
  FToneModes:=Value;
  FToneMode:=ToneModeToInt(FToneModes);
end;





function TTAPITones.GetMonitorTones: Boolean;
begin
  Result:=FMonitoring;
end;

procedure TTAPITones.SetMonitorTones(const Value: Boolean);
var R:Longint;
    Struct:PLineMonitorTone;
    Count:DWord;
begin
  if Value <> FMonitoring then
  begin
    if Value =True then
    begin
      Struct:=MonitorTone.Struct;
      Count:=MonitorTone.Count;
    end
    else
    begin
      Struct:=nil;
      Count:=0;
    end;
    FMonitoring:=False;
    if FCallHandle <> DWord(-1) then
    begin
      R:=LineMonitorTones(FCallHandle,Struct,Count);
      if DWord(R)>DWord($80000000) then
      begin
        RaiseTAPILineError(R);
      end
      else FMonitoring:=True;
    end;
  end;
end;

procedure TTAPITones.Monitor(hCall, AppSpecific, TickCount: DWord);
var i:Integer;
begin
  for i:=0 to FMTones.Count-1 do
  begin
    if FMTones.Items[i].AppSpecific=AppSpecific then
    begin
      if Assigned(FMTones.Items[i].OnMonitor) then FMTones.Items[i].OnMonitor(self);
    end;
  end;
end;

procedure TTAPITones.PerformMsg(Msg: TCMTAPI);
begin
  inherited;
  with Msg.TAPIRec^ do
  begin
    if FCallHandle=hDevice then
    begin
      case dwMsg of
        Line_Generate:Generate(hDevice,dwParam1,dwParam3);
        Line_MonitorTone:Monitor(hDevice,dwParam1,dwParam3);
      end;
    end;
  end;
end;

{ TMonitorTones }

constructor TMonitorTones.Create(AOwner: TTAPITones);
begin
  inherited Create(TMonitorTone);
  FOwner:=AOwner;
end;

function TMonitorTones.GetItem(Index: Integer): TMonitorTone;
begin
  Result:=TMonitorTone(inherited Items[Index]);
end;

function TMonitorTones.GetOwner: TPersistent;
begin
   Result := FOwner;
end;

function TMonitorTones.GetStruct: PLineMonitorTone;
var MonitorTonesStruct:Array of TLINEMONITORTONE;
    i:Integer;
begin
  SetLength(MonitorTonesStruct,Count-1);
  for i:=0 to Count-1 do
  begin
    MonitorTonesStruct[i].dwAppSpecific:=Items[i].FAppSpecific;
    MonitorTonesStruct[i].dwDuration:=Items[i].FDuration;
    MonitorTonesStruct[i].dwFrequency1:=Items[i].FFrequency1;
    MonitorTonesStruct[i].dwFrequency2:=Items[i].FFrequency2;
    MonitorTonesStruct[i].dwFrequency3:=Items[i].FFrequency3;
  end;
  result:=@MonitorTonesStruct;
end;

procedure TMonitorTones.SetItem(Index: Integer; const Value: TMonitorTone);
begin
  inherited SetItem(Index,Value);
end;

{ TMonitorTone }

constructor TMonitorTone.Create(Collection: TCollection);
begin
  inherited Create(Collection);;
end;

function TMonitorTone.GetNamePath: string;
begin
  Result:= inherited GetNamePath;
end;

{$ENDIF}
{$ENDIF}
end.