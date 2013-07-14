unit SaneTool;

{$H+}
{.$MODE DELPHI}

interface

{$IFDEF LINUX}
{$IFNDEF DARWIN}

uses
  {Libc,}BaseUnix, SysUtils, Classes, Types,
  //sanelib;
  sanelsane;

{$DEFINE OLDLIB}

type
  TSaneStats=set of TSANE_Status;

  TOptionChangedInfoItem = (ociInfoInexact, ociOptionReload, ociParamsChanged);
  TOptionChangedInfo = set of TOptionChangedInfoItem;

  TSaneClient=class;

  TSaneWordArray = array of TSANE_Word;

  TSaneCap = (scSoftSelect, scHardSelect, scSoftDetect, scEmulated,
     scAutomatic, scInactive, scAdvanced, scAlwaysSettable);
  TSaneCaps = set of TSaneCap;

  TSaneOption=class;
  PSaneOptionDArray = ^TSaneOptionDArray;
  TSaneOptionDArray = array of TSaneOption;

  TSaneOptionValueType =
    (ovtBool, ovtInt, ovtFloat, ovtString,
     ovtBoolArray, ovtIntArray, ovtFloatArray,
     ovtButton, ovtGroup, ovtUnknown);

  TCustomSaneOption = class(TObject)
  private
    FOwner:TSaneClient;
    FDescriptor : PSANE_Option_Descriptor;
    FIndex: integer;
    FStringItems:TStringList;
    FWordItems:TSaneWordArray;
    function GetDesc(Index:integer):string;
    function GetValueType : TSaneOptionValueType;
    function GetPhysUnit : TSANE_Unit;
    function GetSize : integer;
    function GetCapabilities : TSaneCaps;
    function GetConstraintType : TSANE_Constraint_Type;
    function GetStringItems : TStringList;
    function GetWordItems : TSaneWordArray;
    function GetRange : TSANE_Range;
    function GetIsEnabled:boolean;
    function GetIsReadOnly:boolean;
    function GetIsSettable:boolean;
  protected
    function GetModified:boolean; virtual; abstract;
    property Name : string index 1 read GetDesc;
    property Title : string index 2 read GetDesc;
    property Description : string index 3 read GetDesc;
    property ValueType : TSaneOptionValueType read GetValueType;
    property PhysUnit : TSANE_Unit read GetPhysUnit;
    property Size : integer read GetSize;
    property Capabilities : TSaneCaps read GetCapabilities;
    property ConstraintType : TSANE_Constraint_Type read GetConstraintType;
    property StringItems : TStringList read GetStringItems;
    property WordItems : TSaneWordArray read GetWordItems;
    property Range : TSANE_Range read GetRange;
    property IsEnabled:boolean read GetIsEnabled; //Option verfügbar?
    property IsReadOnly:boolean read GetIsReadOnly; //kann die Option nur gelesen werden?
    property IsSettable:boolean read GetIsSettable; //kann die Option JETZT verändert werden?
  public
    procedure ResetModified; dynamic; abstract;

    property Owner:TSaneClient read FOwner;
    property OptionDescriptor: PSANE_Option_Descriptor read FDescriptor;
    property Modified:boolean read GetModified;
  end;

  TSaneOptionGroup=class(TCustomSaneOption)
  private
    FOptions:TSaneOptionDArray;
    function GetOptionCount:integer;
    function GetOption(Index:integer):TSaneOption;
  protected
    function GetModified:boolean; override;
    procedure SetOwner(AOwner:TSaneClient);
    procedure BeginOptionUpdate(MaxOptions:integer);
    procedure SetOption(Descr:PSANE_Option_Descriptor; DescIndex, OptIndex:integer);
    procedure EndOptionUpdate(OptionCount:integer);
  public
    constructor Create(Descr:PSANE_Option_Descriptor; AOwner:TSaneClient; AIndex:integer);
    destructor Destroy; override;
    procedure ResetModified; override;

    property OptionCount:integer read GetOptionCount;
    property Option[Index:integer]:TSaneOption read GetOption; default;
    property Title;
  end;

  TSaneOption=class(TCustomSaneOption)
  private
    FOptionGroup:TSaneOptionGroup;
    FModified:boolean;
  protected
    function GetModified:boolean; override;
  public
    constructor Create(Descr:PSANE_Option_Descriptor; AGroup:TSaneOptionGroup; AIndex:integer);

    function ReadBool:boolean;
    function WriteBool(Value:boolean):TOptionChangedInfo;
    function ReadInteger:Integer;
    function ReadFixed:Integer;
    function WriteInteger(Value:Integer):TOptionChangedInfo;
    function WriteFixed(Value:Integer):TOptionChangedInfo;
    function ReadFloat:Double;
    function WriteFloat(Value:Double):TOptionChangedInfo;
    function ReadString:string;
    function WriteString(const Value:string):TOptionChangedInfo;

    function ReadData(var Buffer; BufferSize:integer):integer;
    function WriteData(const Data; DataSize:integer):TOptionChangedInfo;
    function SetToDefault:TOptionChangedInfo;
    function DoAction:TOptionChangedInfo;

    function ReadStoreValue:string;
    procedure WriteStoredValue(Value:string);

    procedure ResetModified; override;

    function IsArray:boolean;

    property OptionGroup:TSaneOptionGroup read FOptionGroup;
    property Name;
    property Title;
    property Description;
    property ValueType;
    property PhysUnit;
    property Size;
    property Capabilities;
    property ConstraintType;
    property StringItems;
    property WordItems;
    property Range;
    property IsEnabled;
    property IsSettable;
  end;

  PSaneDevice=^TSaneDevice;
  TSaneDevice= object
  private
     FName: string;          
     FVendor: string;        
     FModel: string;         
     FDeviceType: string;
  public
     property Name: string read FName;              // unique device name
     property Vendor: string read FVendor;          // device vendor string
     property Model: string read FModel;            // device model name
     property DeviceType: string read FDeviceType;  // device type (e.g., "flatbed scanner")
  end;

  TProofLevel=(plInitialized, plOpen);

  TFrameParams=record
     Format: TSANE_Frame;
     IsLastFrame: boolean;
     BytesPerLine: integer;
     PixelsPerLine: integer;
     Lines: integer;
     Depth, BitsPerPixel: integer;
     PixelSize , PixelLoss:integer;
     FrameSize, FrameRawSize:integer;
  end;

  TFrameType=(ftGray1Bit, ftGray8Bit, ftGray16Bit,
              ftRGB1Bit, ftRGB8Bit, ftRGB16Bit,
              ftRed1Bit, ftRed8Bit, ftRed16Bit,
              ftGreen1Bit, ftGreen8Bit, ftGreen16Bit,
              ftBlue1Bit, ftBlue8Bit, ftBlue16Bit,
              ftGray, ftRGB, ftRed, ftGreen, ftBlue, ftNone);

  TRGB=packed record
    R, G, B: byte;
  end;
  TWideRGB=packed record
    B, G, R: word;
  end;

  PByteArray=^TByteArray;
  TByteArray=packed array[0..0] of byte;
  PWordArray=^TWordArray;
  TWordArray=packed array[0..0] of word;
  PRGBArray=^TRGBArray;
  TRGBArray=packed array[0..0] of TRGB;
  PWideRGBArray=^TWideRGBArray;
  TWideRGBArray=packed array[0..0] of TWideRGB;

  TSaneSlice=record
    Pixels:integer;
    case FrameType: TFrameType of
      ftGray1Bit, ftGray8Bit, ftRed1Bit, ftRed8Bit,
      ftGreen1Bit, ftGreen8Bit, ftBlue1Bit, ftBlue8Bit :
        (GrayBytes: PByteArray);
      ftGray16Bit, ftRed16Bit, ftGreen16Bit, ftBlue16Bit :
        (GrayWords: PWordArray);
      ftRGB1Bit, ftRGB8Bit :
        (RGBPixel: PRGBArray);
      ftRGB16Bit :
        (WideRGBPixel: PWideRGBArray);
      ftNone :
        (Buffer : Pointer);
  end;

  TScanState=(ssNewScan, ssNewFrame, ssNewLine, ssLineCompleted, ssFrameCompleted, ssScanCompleted, ssIdle);
  TScanStates=set of TScanState;
  TScanMode=(smNormal, smAdvanced);

  TScanData=class(TObject)
  private
    FFrame: TFrameParams;
    FIsHandScanner:boolean;
    FFrameNr:integer;
    FLine:integer;
    FAcquiredData,          
    FEstimatedSize:integer; 
    FStatus: TSANE_Status;
    FScanStates, OldStates:TScanStates;
    FEndOfFrame, FFinished:boolean;
    FBufferSize, RemainingBuffer:integer;
    FBuffer, ReadBuffer:Pointer;
    FAcquiredFrameData:integer;
    FUserObject:TObject;
    FUserData:Pointer;
    FScanSlice:TSaneSlice;
    FScanMode:TScanMode;
    FFileDescriptor:integer;
    CallStartFrameFlag:boolean;
  public
    destructor Destroy; override;

    procedure AllocBuffer(ASize:integer);
    procedure FreeBuffer;

    property Frame:TFrameParams read FFrame;
    property FrameNr:integer read FFrameNr;
    property Line:integer read FLine;
    property AcquiredFrameData:integer read FAcquiredFrameData;  
    property AcquiredData:integer read FAcquiredData;        //Anzahl bisher übertragener Daten (Bytes)
    property EstimatedSize:integer read FEstimatedSize;      //Gesamtanzahl zu übertragender Daten
    property Status:TSANE_Status read FStatus;
    property ScanSlice:TSaneSlice read FScanSlice;
    property Buffer:Pointer read FBuffer;
    property BufferSize:integer read FBufferSize;
    property IsHandScanner:boolean read FIsHandScanner;

    property EndOfFrame:boolean read FEndOfFrame;
    property ScanStates:TScanStates read FScanStates;
    property ScanMode:TScanMode read FScanMode;

    property Finished:boolean read FFinished write FFinished;
    property UserObject:TObject read FUserObject write FUserObject;
    property UserData:Pointer read FUserData write FUserData;
  end;

  TScanProgressEvent=procedure(SaneClient:TSaneClient; Info:TScanData) of object;
  TSaveOptionEvent=procedure(SaneClient:TSaneClient; OptionNr:integer; Group, Name, Value:string) of object;

  TSaneStandardOptions=class;

  TSaneClient=class(TObject)
  private
    FDevStatus: TSANE_Status;
    FDevHandle:TSANE_Handle;
    FDevIndex:integer;

    FOptionGroups:array of TSaneOptionGroup;
    FIsScanning:boolean;
    FBlockingMode, FIsInBlockingMode:boolean;
    FStdOptions:TSaneStandardOptions;
    FSuccessfulScanned:boolean;

    ScanData:TScanData;

    FOnBeforeOpen:TNotifyEvent;
    FOnAfterOpen:TNotifyEvent;
    FOnBeforeClose:TNotifyEvent;
    FOnAfterClose:TNotifyEvent;

    FOnBeginScan:TNotifyEvent;
    FOnFinishedScan:TNotifyEvent;
    FOnScanProgress:TScanProgressEvent;
    FOnSaveOption:TSaveOptionEvent;
    function GetOptionGroupCount:integer;
    function GetOptionGroup(Index:Cardinal):TSaneOptionGroup;
    function GetFrameParams:TFrameParams;
    function GetDevice:PSaneDevice;
  protected
    class function Init:boolean;
    class procedure ProofInitialized;
    class procedure Release;
    procedure Proof(ProofLevel:TProofLevel);

    procedure QueryOptions;
    procedure ReleaseOptions;

    procedure DoBeforeOpen;
    procedure DoAfterOpen;
    procedure DoBeforeClose;
    procedure DoAfterClose;

    procedure DoBeginScan;
    procedure DoFinishedScan;

    procedure DoScanProgress(Info:TScanData);

    procedure InitBlockingMode;
    procedure ReadFileDescriptor;

    procedure StartScanning;
    procedure StartFrame;
    function ReadSlice:boolean;
    procedure CloseScanning;

    procedure DoSaveOption(OptionNr:integer; const Group, Name, Value:string);
  public
    //constructor + destructor
    constructor Create;
    destructor Destroy; override;
    // Klassenmethoden
    class function DeviceList:TStrings;
    class procedure UpdateDeviceList;
    class function Initialized:boolean;
    class function SaneVersion:integer;
    class function SaneVersionAsString:string;
    // Objektmethoden
    procedure Open(Index:integer=0); overload;
    procedure Open(DeviceName:string); overload;
    procedure Reset; //Gerät schließen und neue öffnen
    procedure Close;
    function IsOpen:boolean;

    procedure ScanImage;
    procedure Cancel;

    procedure StartScan;
    function ScanNext:boolean;
    procedure CloseScan;
    function HasData:boolean;

    function OptionByName(OptionName:string):TSaneOption;
    function OptionByNr(OptionNr:integer):TSaneOption;
    function OptionsModified:boolean;
    procedure ResetModified;
    procedure SaveOptions(SaveAll:boolean; ResetModifiedFlag:boolean=false);
    procedure SetOptionValue(OptionName, Value:string); overload;
    procedure SetOptionValue(OptionNr:integer; Value:string); overload;
    // Eigeneschaften
    property Handle:TSANE_Handle read FDevHandle;
    property Device:PSaneDevice read GetDevice;
    property DevIndex:integer read FDevIndex;

    property OptionGroupCount:integer read GetOptionGroupCount;
    property OptionGroup[Index:Cardinal]:TSaneOptionGroup read GetOptionGroup;
    property FrameParams:TFrameParams read GetFrameParams;

    property IsScanning:boolean read FIsScanning;
    property BlockingMode:boolean read FBlockingMode write FBlockingMode default false;
    property IsInBlockingMode:boolean read FIsInBlockingMode;
    property Status:TSANE_Status read FDevStatus;
    property SuccessfulScanned:boolean read FSuccessfulScanned;

    property StdOptions:TSaneStandardOptions read FStdOptions;
    //Ereignisse & Methodenzeiger
    property OnBeforeOpen:TNotifyEvent read FOnBeforeOpen write FOnBeforeOpen;
    property OnAfterOpen:TNotifyEvent read FOnAfterOpen write FOnAfterOpen;
    property OnBeforeClose:TNotifyEvent read FOnBeforeClose write FOnBeforeClose;
    property OnAfterClose:TNotifyEvent read FOnAfterClose write FOnAfterClose;

    property OnBeginScan:TNotifyEvent read FOnBeginScan write FOnBeginScan;
    property OnScanProgress:TScanProgressEvent read FOnScanProgress write FOnScanProgress;
    property OnFinishedScan:TNotifyEvent read FOnFinishedScan write FOnFinishedScan;

    property OnSaveOption:TSaveOptionEvent read FOnSaveOption write FOnSaveOption;
  end;

  TSaneStandardOptions=class(TObject)
  private
    FOwner:TSaneClient;
    function GetResolution:TSaneOption;
    function GetPreview:TSaneOption;
    function GetScanRectBound(Index:integer):TSaneOption;
  public
    constructor Create(AOwner:TSaneClient);

    procedure MaximizeScanRect;
    function ScanRectAvailable:boolean;
    function ScanRectModifiable:boolean;

    property Owner:TSaneClient read FOwner;
    property Resolution:TSaneOption read GetResolution;
    property Preview:TSaneOption read GetPreview;
    property ScanRectTop:TSaneOption index 1 read GetScanRectBound;
    property ScanRectLeft:TSaneOption index 2 read GetScanRectBound;
    property ScanRectBottom:TSaneOption index 3 read GetScanRectBound;
    property ScanRectRight:TSaneOption index 4 read GetScanRectBound;
  end;

  ESaneError=class(Exception);
  ESaneServerError=class(ESaneError);
  ESaneClientError=class(ESaneError);

const
  ValueSizes:array[SANE_TYPE_BOOL..SANE_TYPE_GROUP] of integer=
    (SizeOf(TSANE_Bool), SizeOf(TSANE_Word), SizeOf(TSANE_Fixed),
     High(integer), High(integer), High(integer));
var
  SaneDevArray: array of TSaneDevice;

//Fehlerbehandlung & Utilities
procedure ProofStatus(Status:TSANE_Status; GoodStats:TSaneStats=[SANE_STATUS_GOOD]);
function SaneInfoToOptionChangedInfo(SaneInfo:TSane_Int):TOptionChangedInfo;

function DataToString(const Data:string):string;
function StringToData(Value:string):string;

function UnitToText(Value:TSANE_Unit):string;

{$ENDIF}
{$ENDIF}

implementation

{$IFDEF LINUX}
{$IFNDEF DARWIN}

//Fehlerbehandlung & Utilities
procedure ProofStatus(Status:TSANE_Status; GoodStats:TSaneStats=[SANE_STATUS_GOOD]);
begin
  if not (Status in GoodStats) then begin
    raise ESaneServerError.Create('Sane-Error: '+PChar(sane_strstatus(Status)));
  end;
end;

function SaneInfoToOptionChangedInfo(SaneInfo:TSane_Int):TOptionChangedInfo;
begin
  result:=[];
  if (SaneInfo and SANE_INFO_INEXACT)<>0 then Include(result, ociInfoInexact);
  if (SaneInfo and SANE_INFO_RELOAD_OPTIONS)<>0 then Include(result, ociOptionReload);
  if (SaneInfo and SANE_INFO_RELOAD_PARAMS)<>0 then Include(result, ociParamsChanged);
end;

function DataToString(const Data:string):string;
begin
  SetLength(result, length(Data)*2);
  BinToHex(PChar(Data), PChar(result), length(Data));
end;

function StringToData(Value:string):string;
begin
  SetLength(result,length(Value) div 2);
  Value:=LowerCase(Value);
  HexToBin(PChar(Value), PChar(result), length(result));
end;

function UnitToText(Value:TSANE_Unit):string;
begin
  case Value of
    SANE_UNIT_PIXEL : result:='Pixel';
    SANE_UNIT_BIT : result:='Bits';
    SANE_UNIT_MM : result:='mm';
    SANE_UNIT_DPI : result:='dpi';
    SANE_UNIT_PERCENT : result:='%';
    SANE_UNIT_MICROSECOND : result:=#181+'s';
  else
    result:='';
  end;
end;

// TCustomSaneOption
function TCustomSaneOption.GetDesc(Index:integer):string;
begin
  case Index of
    1 : result := PChar(FDescriptor^.name);
    2 : result := PChar(FDescriptor^.title);
    3 : result := PChar(FDescriptor^.desc);
  else
    assert(false);
  end;
end;

function TCustomSaneOption.GetValueType : TSaneOptionValueType;
var
  ArrayFlag:boolean;
begin
  ArrayFlag:=Size>ValueSizes[FDescriptor^.option_type];
  case FDescriptor^.option_type of
     SANE_TYPE_BOOL :
      if ArrayFlag then
        result:=ovtBoolArray
      else
        result:=ovtBool;
     SANE_TYPE_INT:
      if ArrayFlag then
        result:=ovtIntArray
      else
        result:=ovtInt;
     SANE_TYPE_FIXED:
      if ArrayFlag then
        result:=ovtFloatArray
      else
        result:=ovtFloat;
     SANE_TYPE_STRING:
        result:=ovtString;
     SANE_TYPE_BUTTON:
        result:=ovtButton;
     SANE_TYPE_GROUP:
        result:=ovtGroup;
  else
    result:=ovtUnknown;
  end;
end;

function TCustomSaneOption.GetPhysUnit : TSANE_Unit;
begin
  result:=FDescriptor^.option_unit;
end;

function TCustomSaneOption.GetSize : integer;
begin
  result:=FDescriptor^.size;
end;

function TCustomSaneOption.GetCapabilities : TSaneCaps;
begin
  result:=[];
  if (FDescriptor^.cap and SANE_CAP_SOFT_SELECT)<>0 then Include(result, scSoftSelect);
  if (FDescriptor^.cap and SANE_CAP_HARD_SELECT)<>0 then Include(result, scHardSelect);
  if (FDescriptor^.cap and SANE_CAP_SOFT_DETECT)<>0 then Include(result, scSoftDetect);
  if (FDescriptor^.cap and SANE_CAP_EMULATED)<>0 then Include(result, scEmulated);
  if (FDescriptor^.cap and SANE_CAP_AUTOMATIC)<>0 then Include(result, scAutomatic);
  if (FDescriptor^.cap and SANE_CAP_INACTIVE)<>0 then Include(result, scInactive);
  if (FDescriptor^.cap and SANE_CAP_ADVANCED)<>0 then Include(result, scAdvanced);
//  if (FDescriptor^.cap and SANE_CAP_ALWAYS_SETTABLE)<>0 then Include(result, scAlwaysSettable);
end;

function TCustomSaneOption.GetConstraintType : TSANE_Constraint_Type;
begin
  {$IFNDEF OLDLIB}
  result:=FDescriptor^.constraint_type;
  {$ELSE}
  result:=FDescriptor^.constraint_type;
//  result:=FDescriptor^.constraint.constraint_type;
  {$ENDIF}
end;

function TCustomSaneOption.GetStringItems : TStringList;
var
  i : integer;
  itm_ptr: Pchar;
begin
  if not assigned(FStringItems) then begin
    FStringItems:=TStringList.Create;
    result:=FStringItems;
    {$IFNDEF OLDLIB}
    case FDescriptor^.constraint_type of
    {$ELSE}
    case FDescriptor^.constraint_type of
//    case FDescriptor^.constraint.constraint_type of
    {$ENDIF}
      SANE_CONSTRAINT_STRING_LIST :
      {$IFNDEF OLDLIB}
      if assigned(FDescriptor^.pstringlist) then begin
      {$ELSE}
      if assigned(FDescriptor^.constraint.string_list) then begin
      {$ENDIF}
        i:=0;
        {$IFNDEF OLDLIB}
        itm_ptr:=PChar(FDescriptor^.pstringlist[i]);
        {$ELSE}
        itm_ptr:=PChar(FDescriptor^.constraint.string_list[i]);
        {$ENDIF}
        while assigned(itm_ptr) do begin
          result.Add(PChar(itm_ptr));
          inc(i);
          {$IFNDEF OLDLIB}
          itm_ptr:=PChar(FDescriptor^.pstringlist[i]);
          {$ELSE}
          itm_ptr:=PChar(FDescriptor^.constraint.string_list[i]);
          {$ENDIF}
        end;
      end;
      SANE_CONSTRAINT_WORD_LIST :
      begin
        GetWordItems;
        if ValueType=ovtFloat then
          for i:=0 to High(FWordItems) do begin
            result.Add(FloatToStr(SANE_UNFIX(FWordItems[i])));
          end
        else
          for i:=0 to High(FWordItems) do begin
            result.Add(IntToStr(FWordItems[i]));
          end;
      end
    else
      raise ESaneClientError.Create('constraint-type mismatch');
    end;
  end else
    result:=FStringItems;
end;

function TCustomSaneOption.GetWordItems : TSaneWordArray;
var
  i:integer;
begin
  with FDescriptor^ do begin
    {$IFNDEF OLDLIB}
    if constraint_type=SANE_CONSTRAINT_WORD_LIST then begin
    {$ELSE}
    if constraint_type=SANE_CONSTRAINT_WORD_LIST then begin
//    if constraint.constraint_type=SANE_CONSTRAINT_WORD_LIST then begin
    {$ENDIF}
      {$IFNDEF OLDLIB}
      SetLength(FWordItems,FDescriptor^.pwordlist[0]-1);
      {$ELSE}
      SetLength(FWordItems,FDescriptor^.constraint.word_list[0]-1);
      {$ENDIF}
      {$IFNDEF OLDLIB}
      for i:=0 to pwordlist^[0]-1 do begin
      {$ELSE}
      for i:=0 to constraint.word_list[0]-1 do begin
      {$ENDIF}
        {$IFNDEF OLDLIB}
        FWordItems[i]:=pwordlist[i+1];
        {$ELSE}
        FWordItems[i]:=constraint.word_list[i+1];
        {$ENDIF}
      end;
    end else
      raise ESaneClientError.Create('constraint-type mismatch');
  end;
  result:=FWordItems;
end;

function TCustomSaneOption.GetRange : TSANE_Range;
begin
  with FDescriptor^ do begin
    {$IFNDEF OLDLIB}
    if constraint_type=SANE_CONSTRAINT_RANGE then
    {$ELSE}
    if constraint_type=SANE_CONSTRAINT_RANGE then
//    if constraint.constraint_type=SANE_CONSTRAINT_RANGE then
    {$ENDIF}
      {$IFNDEF OLDLIB}
      result:=prange^
      {$ELSE}
      result:=constraint.range^
      {$ENDIF}
    else
      raise ESaneClientError.Create('constraint-type mismatch');
  end;
end;

function TCustomSaneOption.GetIsEnabled:boolean;
begin
  //result:=SANE_OPTION_IS_ACTIVE(FDescriptor.cap);
  with FDescriptor^ do
    result:=((cap and SANE_CAP_INACTIVE)=0) and ((cap and SANE_CAP_SOFT_DETECT)<>0);
end;

function TCustomSaneOption.GetIsReadOnly:boolean;
begin
  result:=SANE_OPTION_IS_SETTABLE(FDescriptor^.cap);
end;

function TCustomSaneOption.GetIsSettable:boolean;
begin
  result:=SANE_OPTION_IS_SETTABLE(FDescriptor^.cap) and SANE_OPTION_IS_ACTIVE(FDescriptor^.cap);
end;

//TSaneOptionGroup: constructor + destructor
constructor TSaneOptionGroup.Create(Descr:PSANE_Option_Descriptor; AOwner:TSaneClient; AIndex:integer);
begin
  FDescriptor:=Descr;
  FOwner:=AOwner;
  FIndex:=AIndex;
end;

destructor TSaneOptionGroup.Destroy;
var
  i:integer;
begin
  for i:=0 to High(FOptions) do
    FOptions[i].Free;
end;

//TSaneOptionGroup: private-Methoden
function TSaneOptionGroup.GetOptionCount:integer;
begin
  result:=length(FOptions);
end;

function TSaneOptionGroup.GetOption(Index:integer):TSaneOption;
begin
  result:=FOptions[Index];
end;

//TSaneOptionGroup: protected-Methoden
procedure TSaneOptionGroup.SetOwner(AOwner:TSaneClient);
begin
  FOwner:=AOwner;
end;

procedure TSaneOptionGroup.BeginOptionUpdate(MaxOptions:integer);
begin
  SetLength(FOptions, MaxOptions);
end;

procedure TSaneOptionGroup.SetOption(Descr:PSANE_Option_Descriptor; DescIndex, OptIndex:integer);
begin
  FOptions[OptIndex]:=TSaneOption.Create(Descr, Self, DescIndex);
end;

procedure TSaneOptionGroup.EndOptionUpdate(OptionCount:integer);
begin
  SetLength(FOptions, OptionCount);
end;

function TSaneOptionGroup.GetModified:boolean;
var
  i:integer;
begin
  result:=false;
  for i:=0 to High(FOptions) do begin
    if FOptions[i].Modified then begin
      result:=true;
      Break;
    end;
  end;
end;

//TSaneOptionGroup: public-Methoden
procedure TSaneOptionGroup.ResetModified;
var
  i:integer;
begin
  for i:=0 to High(FOptions) do
    FOptions[i].ResetModified;
end;

//TSaneOption: constructor & destructor
constructor TSaneOption.Create(Descr:PSANE_Option_Descriptor; AGroup:TSaneOptionGroup; AIndex:integer);
begin
  FDescriptor:=Descr;
  FIndex:=AIndex;
  FOptionGroup:=AGroup;
  FOwner:=AGroup.Owner;
end;

//TSaneOption: protected-Methoden
function TSaneOption.GetModified:boolean;
begin
  result:=FModified;
end;

//TSaneOption: public-Methoden
function TSaneOption.ReadBool:boolean;
var
  ReturnedStatus: TSANE_Status;
  SaneResult: TSANE_Bool;
begin
  if ValueType=ovtBool then begin
    ReturnedStatus:=sane_control_option(FOwner.FDevHandle, FIndex,
      SANE_ACTION_GET_VALUE, @SaneResult, nil);
    ProofStatus(ReturnedStatus);
    result:=SaneResult<>0;
  end else
      raise ESaneClientError.Create('value-type mismatch');
end;

function TSaneOption.WriteBool(Value:boolean):TOptionChangedInfo;
var
  ReturnedStatus: TSANE_Status;
  SaneValue: TSANE_Bool;
  SaneInfo: TSANE_Int;
begin
  if ValueType=ovtBool then begin
    if IsSettable then begin
      if Value then
        SaneValue:=SANE_True
      else
        SaneValue:=SANE_False;
      ReturnedStatus:=sane_control_option(FOwner.FDevHandle, FIndex,
        SANE_ACTION_SET_VALUE, @SaneValue, @SaneInfo);
      ProofStatus(ReturnedStatus);
      FModified:=true;
      result:=SaneInfoToOptionChangedInfo(SaneInfo);
    end else
      raise ESaneClientError.Create('value is''nt writeable');
  end else
    raise ESaneClientError.Create('value-type mismatch');
end;

function TSaneOption.ReadInteger:Integer;
var
  ReturnedStatus:TSANE_Status;
  SaneResult:TSANE_Word;
begin
  if ValueType in [ovtInt, ovtFloat] then begin
    ReturnedStatus:=sane_control_option(FOwner.FDevHandle, FIndex,
      SANE_ACTION_GET_VALUE, @SaneResult, nil);
    ProofStatus(ReturnedStatus);
    if ValueType=ovtFloat then
      result:=round(SANE_UNFIX(SaneResult))
    else
      result:=SaneResult;
  end else
      raise ESaneClientError.Create('value-type mismatch');
end;

function TSaneOption.ReadFixed:Integer;
var
  ReturnedStatus:TSANE_Status;
  SaneResult:TSANE_Word;
begin
  if ValueType in [ovtInt, ovtFloat] then begin
    ReturnedStatus:=sane_control_option(FOwner.FDevHandle, FIndex,
      SANE_ACTION_GET_VALUE, @SaneResult, nil);
    ProofStatus(ReturnedStatus);
    result:=SaneResult;
  end else
      raise ESaneClientError.Create('value-type mismatch');
end;

function TSaneOption.WriteInteger(Value:Integer):TOptionChangedInfo;
var
  ReturnedStatus:TSANE_Status;
  SaneValue:TSANE_Bool;
  SaneInfo:TSANE_Int;
begin
  if ValueType in [ovtInt, ovtFloat] then begin
    if IsSettable then begin
      if ValueType=ovtFloat then
        SaneValue:=SANE_FIX(Value)
      else
        SaneValue:=Value;
      ReturnedStatus:=sane_control_option(FOwner.FDevHandle, FIndex,
        SANE_ACTION_SET_VALUE, @SaneValue, @SaneInfo);
      ProofStatus(ReturnedStatus);
      FModified:=true;
      result:=SaneInfoToOptionChangedInfo(SaneInfo);
    end else
      raise ESaneClientError.Create('value is''nt writeable');
  end else
    raise ESaneClientError.Create('value-type mismatch');
end;

function TSaneOption.WriteFixed(Value:Integer):TOptionChangedInfo;
var
  ReturnedStatus:TSANE_Status;
  SaneValue:TSANE_Bool;
  SaneInfo:TSANE_Int;
begin
  if ValueType in [ovtInt, ovtFloat] then begin
    if IsSettable then begin
      SaneValue:=Value;
      ReturnedStatus:=sane_control_option(FOwner.FDevHandle, FIndex,
        SANE_ACTION_SET_VALUE, @SaneValue, @SaneInfo);
      ProofStatus(ReturnedStatus);
      FModified:=true;
      result:=SaneInfoToOptionChangedInfo(SaneInfo);
    end else
      raise ESaneClientError.Create('value is''nt writeable');
  end else
    raise ESaneClientError.Create('value-type mismatch');
end;

function TSaneOption.ReadFloat:Double;
var
  ReturnedStatus: TSANE_Status;
  SaneResult: TSANE_Fixed;
begin
  if ValueType in [ovtInt, ovtFloat] then begin
    ReturnedStatus:=sane_control_option(FOwner.FDevHandle, FIndex,
      SANE_ACTION_GET_VALUE, @SaneResult, nil);
    ProofStatus(ReturnedStatus);
    if ValueType=ovtFloat then
      result:=SANE_UNFIX(SaneResult)
    else
      result:=SaneResult;
  end else
      raise ESaneClientError.Create('value-type mismatch');
end;

function TSaneOption.WriteFloat(Value:Double):TOptionChangedInfo;
var
  ReturnedStatus: TSANE_Status;
  SaneValue: TSANE_Fixed;
  SaneInfo: TSANE_Int;
begin
  if ValueType in [ovtInt, ovtFloat] then begin
    if IsSettable then begin
      if ValueType=ovtFloat then
        SaneValue:=SANE_FIX(Value)
      else
        SaneValue:=round(Value);
      ReturnedStatus:=sane_control_option(FOwner.FDevHandle, FIndex,
        SANE_ACTION_SET_VALUE, @SaneValue, @SaneInfo);
      ProofStatus(ReturnedStatus);
      FModified:=true;
      result:=SaneInfoToOptionChangedInfo(SaneInfo);
    end else
      raise ESaneClientError.Create('value is''nt writeable');
  end else
    raise ESaneClientError.Create('value-type mismatch');
end;

function TSaneOption.ReadString:string;
const
  MIN_SIZE=32;
var
  ReturnedStatus: TSANE_Status;
  SaneResult: TSANE_String;
  str_l, i:integer;
begin
  case ValueType of
    ovtString :
    begin
      if Size<MIN_SIZE then str_l:=MIN_SIZE else str_l:=Size;
      SetLength(result, str_l);
      SaneResult:=TSANE_String(PChar(result));
      ReturnedStatus:=sane_control_option(FOwner.FDevHandle, FIndex,
        SANE_ACTION_GET_VALUE, SaneResult, nil);
      ProofStatus(ReturnedStatus);
      for i:=1 to str_l do begin
        if result[i] in [#0..#31,',',';'] then begin
            str_l:=i;
          Break;
        end;
      end;
      SetLength(result, str_l-1);
    end;
    ovtInt :
      result:=IntToStr(ReadInteger);
    ovtFloat :
      result:=FloatToStr(ReadFloat);
  else
    raise ESaneClientError.Create('value-type mismatch');
  end;
end;

function TSaneOption.WriteString(const Value:string):TOptionChangedInfo;
var
  ReturnedStatus: TSANE_Status;
  SaneValue: TSANE_String_Const;
  SaneInfo: TSANE_Int;
begin
  if ValueType=ovtString then begin
    if IsSettable then begin
      SaneValue:=TSANE_String_Const(PChar(Value));
      ReturnedStatus:=sane_control_option(FOwner.FDevHandle, FIndex,
        SANE_ACTION_SET_VALUE, SaneValue, @SaneInfo);
      ProofStatus(ReturnedStatus);
      FModified:=true;
      result:=SaneInfoToOptionChangedInfo(SaneInfo);
    end else
      raise ESaneClientError.Create('value is''nt writeable');
  end else
    raise ESaneClientError.Create('value-type mismatch');
end;

function TSaneOption.ReadData(var Buffer; BufferSize:integer):integer;
var
  ReturnedStatus: TSANE_Status;
begin
  if not (ValueType in [ovtButton, ovtGroup]) then begin
    if BufferSize>=Size then begin
      ReturnedStatus:=sane_control_option(FOwner.FDevHandle, FIndex,
        SANE_ACTION_GET_VALUE, @Buffer, nil);
      ProofStatus(ReturnedStatus);
      result:=Size;
    end else
      raise ESaneClientError.Create('BufferSize is''nt big enough to hold the data');
  end else
    raise ESaneClientError.Create('value-type mismatch');
end;

function TSaneOption.WriteData(const Data; DataSize:integer):TOptionChangedInfo;
var
  ReturnedStatus: TSANE_Status;
  SaneInfo: TSANE_Int;
begin
  if not (ValueType in [ovtButton, ovtGroup]) then begin
    if IsSettable then begin
      if DataSize=Size then begin
        ReturnedStatus:=sane_control_option(FOwner.FDevHandle, FIndex,
          SANE_ACTION_SET_VALUE, @Data, @SaneInfo);
        ProofStatus(ReturnedStatus);
        FModified:=true;
        result:=SaneInfoToOptionChangedInfo(SaneInfo);
      end else
        if DataSize<Size then
          raise ESaneClientError.Create('To many data for this option')
        else
          raise ESaneClientError.Create('To few data for this option');
    end else
      raise ESaneClientError.Create('value is''nt writeable');
  end else
    raise ESaneClientError.Create('value-type mismatch');
end;

function TSaneOption.SetToDefault:TOptionChangedInfo;
var
  ReturnedStatus: TSANE_Status;
  SaneInfo: TSANE_Int;
begin
  if not (ValueType in [ovtButton, ovtGroup]) then begin
    ReturnedStatus:=sane_control_option(FOwner.FDevHandle, FIndex,
        SANE_ACTION_SET_Auto, nil, @SaneInfo);
    ProofStatus(ReturnedStatus);
    FModified:=true;
    result:=SaneInfoToOptionChangedInfo(SaneInfo);
  end else
    raise ESaneClientError.Create('value-type mismatch');
end;

function TSaneOption.DoAction:TOptionChangedInfo;
var
  ReturnedStatus: TSANE_Status;
  SaneInfo: TSANE_Int;
begin
  if ValueType=ovtButton then begin
    ReturnedStatus:=sane_control_option(FOwner.FDevHandle, FIndex,
        SANE_ACTION_SET_VALUE, nil, @SaneInfo);
    ProofStatus(ReturnedStatus);
    result:=SaneInfoToOptionChangedInfo(SaneInfo);
  end else
    raise ESaneClientError.Create('value-type mismatch');
end;

procedure TSaneOption.ResetModified;
begin
  FModified:=false;
end;

function TSaneOption.ReadStoreValue:string;
var
  Buf:string;
begin
  case ValueType of
    ovtBool :
      if ReadBool then result:='1' else result:='0';
    ovtInt :
      result:=IntToStr(ReadInteger);
    ovtFloat :
      result:=FloatToStr(ReadFloat);
    ovtString :
      result:=ReadString;
    ovtBoolArray, ovtIntArray, ovtFloatArray :
    begin
      SetLength(Buf, Size);
      ReadData(PChar(Buf)^, Size);
      result:=DataToString(Buf);
    end;
  else
    assert(false);
    result:='';
  end;
end;

procedure TSaneOption.WriteStoredValue(Value:string);
var
  Buf:string;
begin
  if IsSettable then
    case ValueType of
      ovtBool :
        WriteBool(Value='1');
      ovtInt :
        WriteInteger(StrToInt(Value));
      ovtFloat :
        WriteFloat(StrToFloat(Value));
      ovtString :
        WriteString(Value);
      ovtBoolArray, ovtIntArray, ovtFloatArray :
      begin
        Buf:=StringToData(Value);
        WriteData(PChar(Buf)^, length(Buf));
      end;
    else
      assert(false);
    end;
end;

function TSaneOption.IsArray:boolean;
begin
  result:=Size>ValueSizes[FDescriptor^.option_type];
end;

//TScanData
destructor TScanData.Destroy;
begin
  FreeBuffer;
  inherited;
end;

procedure TScanData.AllocBuffer(ASize:integer);
begin
  if FBufferSize<>ASize then begin
    ReAllocMem(FBuffer, ASize);
    ReadBuffer:=FBuffer;
    FBufferSize:=ASize;
    FScanSlice.Buffer:=FBuffer;
  end;
end;

procedure TScanData.FreeBuffer;
begin
  ReAllocMem(FBuffer, 0);
  ReadBuffer:=nil;
end;

//TSaneStandardOptions:
constructor TSaneStandardOptions.Create(AOwner:TSaneClient);
begin
  FOwner:=AOwner;
end;

procedure TSaneStandardOptions.MaximizeScanRect;
begin
  if assigned(ScanRectLeft) then
    with ScanRectLeft do
      case ConstraintType of
        SANE_CONSTRAINT_NONE : WriteFixed(0);
        SANE_CONSTRAINT_RANGE : WriteFixed(Range.min);
        SANE_CONSTRAINT_WORD_LIST : WriteFixed(WordItems[0]);
      end;
  if assigned(ScanRectRight) then
    with ScanRectRight do
      case ConstraintType of
        SANE_CONSTRAINT_NONE : WriteFixed(High(integer));
        SANE_CONSTRAINT_RANGE : WriteFixed(Range.max);
        SANE_CONSTRAINT_WORD_LIST : WriteFixed(WordItems[High(WordItems)]);
      end;
  if assigned(ScanRectTop) then
    with ScanRectTop do
      case ConstraintType of
        SANE_CONSTRAINT_NONE : WriteFixed(0);
        SANE_CONSTRAINT_RANGE : WriteFixed(Range.min);
        SANE_CONSTRAINT_WORD_LIST : WriteFixed(WordItems[0]);
      end;
  if assigned(ScanRectBottom) then
    with ScanRectBottom do
      case ConstraintType of
        SANE_CONSTRAINT_NONE : WriteFixed(High(integer));
        SANE_CONSTRAINT_RANGE : WriteFixed(Range.max);
        SANE_CONSTRAINT_WORD_LIST : WriteFixed(WordItems[High(WordItems)]);
      end;
end;

function TSaneStandardOptions.ScanRectAvailable:boolean;
begin
  result:=assigned(ScanRectLeft) and ScanRectLeft.IsEnabled and
      assigned(ScanRectRight) and ScanRectRight.IsEnabled and
      assigned(ScanRectTop) and ScanRectTop.IsEnabled and
      assigned(ScanRectBottom) and ScanRectBottom.IsEnabled;
end;

function TSaneStandardOptions.ScanRectModifiable:boolean;
begin
  result:=assigned(ScanRectLeft) and ScanRectLeft.IsSettable and
      assigned(ScanRectRight) and ScanRectRight.IsSettable and
      assigned(ScanRectTop) and ScanRectTop.IsSettable and
      assigned(ScanRectBottom) and ScanRectBottom.IsSettable;
end;

//TSaneStandardOptions
function TSaneStandardOptions.GetResolution:TSaneOption;
begin
  result:=Owner.OptionByName('resolution');
end;

function TSaneStandardOptions.GetPreview:TSaneOption;
begin
  result:=Owner.OptionByName('preview');
end;

function TSaneStandardOptions.GetScanRectBound(Index:integer):TSaneOption;
begin
  case Index of
    1 : result:=Owner.OptionByName('tl-y');
    2 : result:=Owner.OptionByName('tl-x');
    3 : result:=Owner.OptionByName('br-y');
    4 : result:=Owner.OptionByName('br-x');
  else
    result:=nil;
    assert(false);
  end;
end;

//Klassen-Variablen
var
  FInitialized: integer = 0;
  FSaneVersion: TSANE_Int;
  FDeviceList: TStringList;
  FInitStatus, FGetDevicesStatus: TSANE_Status;

// TSaneClient: Klassenmethoden
class function TSaneClient.DeviceList:TStrings;
begin
  if not assigned(FDeviceList) then
     UpdateDeviceList;
  result:=FDeviceList;
end;

class procedure TSaneClient.UpdateDeviceList;
var
  dev_list: PPSANE_Device;
  dev_ptr: PSANE_Device;
  i, j:integer;
begin
  if not assigned(FDeviceList) then
     FDeviceList:=TStringList.Create
  else
     FDeviceList.Clear;
  ProofInitialized;
  FGetDevicesStatus:=sane_get_devices(dev_list, SANE_true);
  try
     ProofStatus(FGetDevicesStatus);
     i:=0;
     dev_ptr:=dev_list[i];
     while assigned(dev_ptr) do begin
        FDeviceList.Add(PChar(dev_ptr^.name));
        inc(i);
        dev_ptr:=dev_list[i];
     end;
     SetLength(SaneDevArray,i);
     for j:=0 to i-1 do
       begin
         dev_ptr:=dev_list[j];
         SaneDevArray[j].FName:=PChar(dev_ptr^.name);
         SaneDevArray[j].FVendor:=PChar(dev_ptr^.vendor);
         SaneDevArray[j].FModel:=PChar(dev_ptr^.model);
         SaneDevArray[j].FDeviceType:=PChar(dev_ptr^.dev_type);
        FDeviceList.Objects[j]:=tObject(@SaneDevArray[j]);
       end;
  except
     SaneDevArray:=nil;
     raise;
  end;
end;

class function TSaneClient.Initialized:boolean;
begin
  result:=FInitialized>0;
end;

class procedure TSaneClient.ProofInitialized;
begin
  if FInitialized=0 then
     if not Init then
        ProofStatus(FInitStatus);
end;

class function TSaneClient.Init:boolean;
begin
  FInitStatus:=SANE_init(FSaneVersion,nil);
  result:=FInitStatus=SANE_STATUS_GOOD;
  if result then inc(FInitialized);
end;

class procedure TSaneClient.Release;
begin
  if FInitialized>0 then begin
     dec(FInitialized);
     if FInitialized=0 then
        SANE_Exit;
  end;
end;

class function TSaneClient.SaneVersion:integer;
begin
  ProofInitialized;
  result:=FSaneVersion;
end;

class function TSaneClient.SaneVersionAsString:string;
begin
  ProofInitialized;
  result:=IntToStr(Sane_Version_Major(FSaneVersion))+'.'+
    IntToStr(Sane_Version_Minor(FSaneVersion))+'.'+
    IntToStr(Sane_Version_Build(FSaneVersion));
end;

// TSaneClient: constructor + destructor
constructor TSaneClient.Create;
begin
  inherited;
  Init;
  FStdOptions:=TSaneStandardOptions.Create(Self);
end;

destructor TSaneClient.Destroy;
begin
  Close;
  FDeviceList.Free;
  Release;
  inherited;
end;

// TSaneClient: private-Methoden
function TSaneClient.GetOptionGroupCount:integer;
begin
  if not assigned(FOptionGroups) then
    QueryOptions;
  result:=Length(FOptionGroups);
end;

function TSaneClient.GetOptionGroup(Index:Cardinal):TSaneOptionGroup;
begin
  if not assigned(FOptionGroups) then
    QueryOptions;
  result:=FOptionGroups[Index];
end;

function TSaneClient.GetFrameParams:TFrameParams;
var
  ReturnedStatus: TSANE_Status;
  Params: SANE_Parameters;
begin
  ReturnedStatus:=sane_get_parameters(FDevHandle, Params);
  ProofStatus(ReturnedStatus);
  result.Format:=Params.format;
  result.IsLastFrame:=Params.last_frame=SANE_True;
  result.BytesPerLine:=Params.bytes_per_line;
  result.PixelsPerLine:=Params.pixels_per_line;
  result.Lines:=Params.lines;
  result.Depth:=Params.depth;
  with result do begin
    if Format=SANE_FRAME_GRAY then
      BitsPerPixel:=depth
    else
      BitsPerPixel:=depth*3;
    if Format=SANE_FRAME_RGB then begin
      PixelSize:=(depth div 16)*3+3;
    end else begin
      PixelSize:=(depth div 16)+1;
    end;
    PixelLoss:=BytesPerLine-PixelsPerLine*PixelSize;
    FrameRawSize:=BytesPerLine*Lines;
    FrameSize:=(BytesPerLine-PixelLoss)*Lines;

    if assigned(ScanData) then
      ScanData.FIsHandScanner:=Lines<0;
  end;
end;

function TSaneClient.GetDevice:PSaneDevice;
begin
  if assigned(FDevHandle) then begin
    result:=PSaneDevice(DeviceList.Objects[FDevIndex]);
  end else
    result:=nil;
end;

// TSaneClient: protected-Methoden
procedure TSaneClient.Proof(ProofLevel:TProofLevel);
begin
  case ProofLevel of
    plInitialized : ProofStatus(FInitStatus);
    plOpen : if not assigned(FDevHandle) then
      raise ESaneClientError.Create('Device not open');
  else
     assert(false);
  end;
end;

procedure TSaneClient.QueryOptions;
var
  ReturnedStatus: TSANE_Status;
  i, GroupIndex: integer;
  NumOptions, GroupedOptions: integer;
  OptionDescriptor: PSANE_Option_Descriptor;
  NewGroup: TSaneOptionGroup;
begin
  ReleaseOptions;
  Proof(plOpen);
  ReturnedStatus:=sane_control_option(FDevHandle, 0, SANE_ACTION_GET_VALUE,
    @NumOptions, nil);
  ProofStatus(ReturnedStatus);
  SetLength(FOptionGroups,NumOptions-1);
  if NumOptions>1 then begin
    GroupIndex:=0;
    OptionDescriptor:=sane_get_option_descriptor(FDevHandle, 1);
    assert(assigned(OptionDescriptor),'Wie, kein gültiger Index?');
    if OptionDescriptor^.option_type=SANE_TYPE_GROUP then begin
      NewGroup:=TSaneOptionGroup.Create(OptionDescriptor, Self, 1);
      NewGroup.BeginOptionUpdate(NumOptions-1);
      GroupedOptions:=0;
    end else begin
      NewGroup:=TSaneOptionGroup.Create(nil, Self, -1);
      NewGroup.BeginOptionUpdate(NumOptions-1);
      NewGroup.SetOption(OptionDescriptor, 1, 0);
      GroupedOptions:=1;
    end;
    for i:=2 to NumOptions-1 do begin
      OptionDescriptor:=sane_get_option_descriptor(FDevHandle, i);
      assert(assigned(OptionDescriptor),'Wie, kein gültiger Index?');
      if OptionDescriptor^.option_type=SANE_TYPE_GROUP then begin
        NewGroup.EndOptionUpdate(GroupedOptions);
        FOptionGroups[GroupIndex]:=NewGroup;
        Inc(GroupIndex);
        NewGroup:=TSaneOptionGroup.Create(OptionDescriptor, Self, i);
        NewGroup.BeginOptionUpdate(NumOptions-i);
        GroupedOptions:=0;
      end else begin
        NewGroup.SetOption(OptionDescriptor, i, GroupedOptions);
        Inc(GroupedOptions);
      end;
    end;
    if (GroupedOptions>0) or (OptionDescriptor^.option_type=SANE_TYPE_GROUP) then begin
        NewGroup.EndOptionUpdate(GroupedOptions);
        FOptionGroups[GroupIndex]:=NewGroup;
    end;
    SetLength(FOptionGroups,GroupIndex+1);
  end;
end;

procedure TSaneClient.ReleaseOptions;
var
  i:integer;
begin
  for i:=0 to High(FOptionGroups) do
    FOptionGroups[i].Free;
  FOptionGroups:=nil;
end;

//Event-Caller
procedure TSaneClient.DoBeforeOpen;
begin
  if assigned(FOnBeforeOpen) then
    FOnBeforeOpen(Self);
end;

procedure TSaneClient.DoAfterOpen;
begin
  if assigned(FOnAfterOpen) then
    FOnAfterOpen(Self);
end;

procedure TSaneClient.DoBeforeClose;
begin
  if assigned(FOnBeforeClose) then
    FOnBeforeClose(Self);
end;

procedure TSaneClient.DoAfterClose;
begin
  if assigned(FOnAfterClose) then
    FOnAfterClose(Self);
end;

procedure TSaneClient.DoBeginScan;
begin
  if assigned(FOnBeginScan) then
    FOnBeginScan(Self);
end;

procedure TSaneClient.DoScanProgress(Info:TScanData);
begin
  if assigned(FOnScanProgress) then
    FOnScanProgress(Self, Info);
end;

procedure TSaneClient.DoFinishedScan;
begin
  if assigned(FOnFinishedScan) then
    FOnFinishedScan(Self);
end;

procedure TSaneClient.DoSaveOption(OptionNr:integer; const Group, Name, Value:string);
begin
  if assigned(FOnSaveOption) then
    FOnSaveOption(Self, OptionNr, Group, Name, Value);
end;

//Scan-methods
procedure TSaneClient.InitBlockingMode;
var
  ReturnedStatus: TSANE_Status;
begin
  if not FBlockingMode then
    ReturnedStatus:=sane_set_io_mode(FDevHandle, SANE_True)
  else
    ReturnedStatus:=sane_set_io_mode(FDevHandle, SANE_False);
  FIsInBlockingMode:=FBlockingMode xor (ReturnedStatus<>SANE_STATUS_GOOD);
end;

procedure TSaneClient.ReadFileDescriptor;
var
  ReturnedStatus: TSANE_Status;
begin
  ReturnedStatus:=sane_get_select_fd(FDevHandle, ScanData.FFileDescriptor);
  if ReturnedStatus<>SANE_STATUS_GOOD then
    ScanData.FFileDescriptor:=0;
end;

procedure TSaneClient.StartScanning;
begin
  if not FIsScanning then begin
    FSuccessfulScanned:=false;
    Proof(plOpen);
    FIsScanning:=true;
    with ScanData do begin
      FScanStates:=[ssNewScan, ssNewFrame];
      CallStartFrameFlag:=true;
    end;
  end else
    raise ESaneClientError.Create('Scanning already started');
end;

procedure TSaneClient.StartFrame;
begin
  with ScanData do begin
    CallStartFrameFlag:=false;
    FEndOfFrame:=false;
    FAcquiredFrameData:=0;
    FLine:=0;
    FStatus:=sane_start(FDevHandle);
    ProofStatus(Status);
    FFrame:=FrameParams;
    Include(FScanStates, ssNewLine);
    InitBlockingMode;
    if ScanMode=smAdvanced then
      ReadFileDescriptor;
    with ScanData.Frame, ScanSlice do begin
      case Format of
        SANE_FRAME_Gray :
          case depth of
            1  : FrameType:=ftGray1Bit;
            8  : FrameType:=ftGray8Bit;
            16 : FrameType:=ftGray16Bit;
          else
            FrameType:=ftGray;
          end;
        SANE_FRAME_RGB :
          case depth of
            1  : FrameType:=ftRGB1Bit;
            8  : FrameType:=ftRGB8Bit;
            16 : FrameType:=ftRGB16Bit;
          else
            FrameType:=ftRGB;
          end;
        SANE_FRAME_Red :
          case depth of
            1  : FrameType:=ftRed1Bit;
            8  : FrameType:=ftRed8Bit;
            16 : FrameType:=ftRed16Bit;
          else
            FrameType:=ftRed;
          end;
        SANE_FRAME_Green :
          case depth of
            1  : FrameType:=ftGreen1Bit;
            8  : FrameType:=ftGreen8Bit;
            16 : FrameType:=ftGreen16Bit;
          else
            FrameType:=ftGreen;
          end;
        SANE_FRAME_Blue :
          case depth of
            1  : FrameType:=ftBlue1Bit;
            8  : FrameType:=ftBlue8Bit;
            16 : FrameType:=ftBlue16Bit;
          else
            FrameType:=ftBlue;
          end;
      else
        assert(false);
      end;
      if not IsHandScanner then begin
        if not (Frame.Format in [SANE_FRAME_GRAY, SANE_FRAME_RGB]) then
          FEstimatedSize:=BytesPerLine*Lines*3
        else
          FEstimatedSize:=BytesPerLine*Lines;
      end else
        FEstimatedSize:=0;
      FScanSlice.Pixels:=PixelsPerLine;
      AllocBuffer(FFrame.BytesPerLine);
      RemainingBuffer:=BufferSize;
    end;
  end;
end;

function TSaneClient.ReadSlice:boolean;
var
  ReadedData:integer;
begin
  with ScanData do begin
    FStatus:=sane_read(FDevHandle, ReadBuffer, RemainingBuffer, @ReadedData);
    result:=ReadedData<>0;
    FEndOfFrame:=FStatus=SANE_STATUS_EOF;
    if (EndOfFrame and Frame.IsLastFrame) or (FStatus=SANE_STATUS_CANCELLED) then
      FFinished:=true;
    if result then begin
      Dec(RemainingBuffer, ReadedData);
      Inc(PChar(ReadBuffer),ReadedData);
      Inc(FAcquiredData, ReadedData);
      Inc(FAcquiredFrameData, ReadedData);
    end;
  end;
end;

procedure TSaneClient.CloseScanning;
begin
  Cancel;
  FIsScanning:=false;
end;

// TSaneClient: public-Methoden
procedure TSaneClient.Open(Index:integer=0);
begin
  Open(DeviceList[Index]);
end;

procedure TSaneClient.Open(DeviceName:string);
begin
  Proof(plInitialized);
  if assigned(FDevHandle) then
     Close;
  DoBeforeOpen;
  FDevStatus:=sane_open(PChar(DeviceName), FDevHandle);
  ProofStatus(FDevStatus);
  FDevIndex:=DeviceList.IndexOf(DeviceName);
  DoAfterOpen;
end;

procedure TSaneClient.Reset;
var
  DeviceName:string;
  i, j:integer;
  Group:TSaneOptionGroup;
begin
  if assigned(FDevHandle) then begin
    Cancel;
    sane_close(FDevHandle);
    DeviceName:=DeviceList[FDevIndex];
    FDevStatus:=sane_open(TSANE_String_Const(PChar(DeviceName)), FDevHandle);
    try
      ProofStatus(FDevStatus);
    except
      DoBeforeClose;
      ReleaseOptions;
      FDevHandle:=nil;
      FDevIndex:=0;
      DoAfterClose;
      raise;
    end;
    for i:=0 to OptionGroupCount-1 do begin
      Group:=OptionGroup[i];
      Group.FDescriptor:=sane_get_option_descriptor(FDevHandle, Group.FIndex);
      for j:=0 to Group.OptionCount-1 do begin
        with Group.Option[j] do
          FDescriptor:=sane_get_option_descriptor(FDevHandle, FIndex);
      end;
    end;
    ResetModified;
  end;
end;

procedure TSaneClient.Close;
begin
  if assigned(FDevHandle) then begin
    Cancel;
    DoBeforeClose;
    ReleaseOptions;
    sane_close(FDevHandle);
    FDevHandle:=nil;
    FDevIndex:=0;
    DoAfterClose;
  end;
end;

function TSaneClient.IsOpen:boolean;
begin
  result:=assigned(FDevHandle);
end;

procedure TSaneClient.ScanImage;
begin
  ScanData:=TScanData.Create;
  try
    StartScanning;
    try
      DoBeginScan;
      with ScanData do begin
        FScanMode:=smNormal;
        repeat
          StartFrame;
          repeat
            if ReadSlice then begin
              if RemainingBuffer=0 then begin
                Include(FScanStates,ssLineCompleted);
                if EndOfFrame then begin
                  Include(FScanStates,ssFrameCompleted);
                  if Frame.IsLastFrame then
                    Include(FScanStates,ssScanCompleted);
                end;
              end;
              DoScanProgress(ScanData);
              if RemainingBuffer=0 then begin
                FScanStates:=[ssNewLine];
                RemainingBuffer:=BufferSize;
                ReadBuffer:=FBuffer;
                Inc(FLine);
              end else
                FScanStates:=[];
            end else begin
              OldStates:=FScanStates;
              Include(FScanStates, ssIdle);
              DoScanProgress(ScanData);
              FScanStates:=OldStates;
              Exclude(FScanStates, ssNewScan);
            end;
          until EndOfFrame or Finished;
          if EndOfFrame and not Finished then
            Include(FScanStates, ssNewFrame);
          Inc(FFrameNr);
        until Finished;
      end;
      FSuccessfulScanned:=true;
    finally
      CloseScanning;
      DoFinishedScan;
    end;
  finally
    ScanData.Free;
  end;
end;

procedure TSaneClient.StartScan;
begin
  ScanData:=TScanData.Create;
  try
    StartScanning;
    ScanData.FScanMode:=smAdvanced;
    DoBeginScan;
  except
    FreeAndNil(ScanData);
    raise;
  end;
end;

function TSaneClient.ScanNext:boolean;
begin
  if assigned(ScanData) then begin
    if ScanData.ScanMode=smAdvanced then begin
      with ScanData do begin
        if CallStartFrameFlag then
          StartFrame;
        if ReadSlice then begin
          if RemainingBuffer=0 then begin
            Include(FScanStates,ssLineCompleted);
            if EndOfFrame then begin
              Include(FScanStates,ssFrameCompleted);
              if Frame.IsLastFrame then
                Include(FScanStates,ssScanCompleted);
            end;
          end;
          DoScanProgress(ScanData);
          if RemainingBuffer=0 then begin
            FScanStates:=[ssNewLine];
            RemainingBuffer:=BufferSize;
            ReadBuffer:=FBuffer;
            Inc(FLine);
          end else
            FScanStates:=[];
        end else begin
          OldStates:=FScanStates;
          Include(FScanStates, ssIdle);
          DoScanProgress(ScanData);
          FScanStates:=OldStates;
        end;
        result:=not Finished;
        if result and EndOfFrame then begin
            Include(FScanStates, ssNewFrame);
            CallStartFrameFlag:=true;
        end;
      end;
    end else
      raise ESaneClientError.Create('Scanning is''nt started in Advanced Mode');
  end else
    raise ESaneClientError.Create('Scanning is''nt started');
end;

procedure TSaneClient.CloseScan;
begin
  if assigned(ScanData) then begin
    if ScanData.ScanMode=smAdvanced then begin
      CloseScanning;
      FreeAndNil(ScanData);
      DoFinishedScan;
    end else
      raise ESaneClientError.Create('Scanning is''nt started in Advanced Mode');
  end else
    raise ESaneClientError.Create('Scanning is''nt started');
end;

function TSaneClient.HasData:boolean;
var
  PollFD:TPollFD;
  r:integer;
begin
  if assigned(ScanData) then begin
    if ScanData.ScanMode=smAdvanced then begin
      with ScanData do begin
        if FFileDescriptor<>0 then begin
          PollFD.fd:=FFileDescriptor;
          PollFD.events:=POLLIN;
          r:=fpPoll(@PollFD, 1, 0);
          result:=r>0;
          //result:=(r>0) and ((PollFD.revents and POLLIN)<>0);
        end else
          result:=true;
      end;
    end else
      raise ESaneClientError.Create('Scanning is''nt started in Advanced Mode');
  end else
    raise ESaneClientError.Create('Scanning is''nt started');
end;

procedure TSaneClient.Cancel;
begin
  sane_cancel(FDevHandle);
end;

function TSaneClient.OptionByName(OptionName:string):TSaneOption;
var
  i,j:integer;
  Group:TSaneOptionGroup;
begin
  result:=nil;
  for i:=0 to OptionGroupCount-1 do begin
    Group:=OptionGroup[i];
    with Group do
      for j:=0 to OptionCount-1 do begin
        if CompareText(Option[j].Name,OptionName)=0 then begin
          result:=Option[j];
          Break;
        end;
      end;
      if assigned(result) then
        Break;
  end;
end;

function TSaneClient.OptionByNr(OptionNr:integer):TSaneOption;
var
  i,j:integer;
  Group:TSaneOptionGroup;
begin
  result:=nil;
  for i:=0 to OptionGroupCount-1 do begin
    Group:=OptionGroup[i];
    with Group do
      for j:=0 to OptionCount-1 do begin
        if Option[j].FIndex=OptionNr then begin
          result:=Option[j];
          Break;
        end;
      end;
      if assigned(result) then
        Break;
  end;
end;

function TSaneClient.OptionsModified:boolean;
var
  i:integer;
begin
  result:=false;
  for i:=0 to High(FOptionGroups) do begin
    if FOptionGroups[i].Modified then begin
      result:=true;
      Break;
    end;
  end;
end;

procedure TSaneClient.ResetModified;
var
  i:integer;
begin
  for i:=0 to High(FOptionGroups) do
    FOptionGroups[i].ResetModified;
end;

procedure TSaneClient.SaveOptions(SaveAll:boolean; ResetModifiedFlag:boolean=false);
var
  i, j:integer;
  Group:TSaneOptionGroup;
  GroupName, Value:string;
begin
  for i:=0 to High(FOptionGroups) do begin
    Group:=FOptionGroups[i];
    with Group do begin
      GroupName:=Title;
      for j:=0 to High(FOptions) do begin
        with FOptions[j] do
          if (SaveAll or Modified) and
              IsEnabled and
              IsSettable then
          begin
            Value:=ReadStoreValue;
            DoSaveOption(FIndex, GroupName, Title, Value);
          end;
      end;
    end;
  end;
  if ResetModifiedFlag then
    ResetModified;
end;

procedure TSaneClient.SetOptionValue(OptionName, Value:string);
begin
  OptionByName(OptionName).WriteStoredValue(Value);
end;

procedure TSaneClient.SetOptionValue(OptionNr:integer; Value:string);
begin
  OptionByNr(OptionNr).WriteStoredValue(Value);
end;

initialization

finalization
  SaneDevArray := nil;
  if FInitialized>0 then
     SANE_Exit;
{$ENDIF}
{$ENDIF}
end.

