unit uFileSystem; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Graphics, Utils
  {$IFDEF WINDOWS}
  ,Windows
  {$ENDIF}
  ;
  
  
const
  cDefaultListGrowthDelta = 16;
   
resourcestring
  strNamecannotbeclear                  = 'The name of an object cannot be clear';
  strComputer                           = 'Computer';

type
  TBaseInterface = class;
  
  TUser = class
  end;
  
  TUserGroup = class
  end;
  
  { TBaseAttribute }

  TBaseAttribute = class
  private
    FCanExecute: Boolean;
    FCanRead: Boolean;
    FCanWrite: Boolean;
    procedure SetCanExecute(const AValue: Boolean);
    procedure SetCanRead(const AValue: Boolean);
    procedure SetCanWrite(const AValue: Boolean);
  public
    property canread : Boolean read FCanRead write SetCanRead;
    property canwrite : Boolean read FCanWrite write SetCanWrite;
    property canexecute : Boolean read FCanExecute write SetCanExecute;
  end;
  
  { TBaseAttributes }

  TBaseAttributes = class
  private
    FGroup: TUserGroup;
    FGroupRights: TBaseAttribute;
    FOtherRights: TBaseAttribute;
    FUser: TUser;
    FUserRights: TBaseAttribute;
    procedure SetGroup(const AValue: TUserGroup);
    procedure SetGroupRights(const AValue: TBaseAttribute);
    procedure SetOtherRights(const AValue: TBaseAttribute);
    procedure SetUser(const AValue: TUser);
    procedure SetUserRights(const AValue: TBaseAttribute);
  public
    property User : TUser read FUser write SetUser;
    property Group : TUserGroup read FGroup write SetGroup;
    property UserRights : TBaseAttribute read FUserRights write SetUserRights;
    property GroupRights : TBaseAttribute read FGroupRights write SetGroupRights;
    property OtherRights : TBaseAttribute read FOtherRights write SetOtherRights;
  end;
  
  { TBaseEntry }

  PbaseEntry = ^TBaseEntry;
  TBaseEntry = class
  private
    FAttributes: TBaseAttributes;
    FChanged: TDateTime;
    FCreation: TDateTime;
    FName: string;
    FFullPath : string;
    FParent : TBaseEntry;
    function GetSize: LongInt;virtual;
    procedure SetAttributes(const AValue: TBaseAttributes);virtual;
    procedure SetName(const AValue: string);virtual;
  protected
    FInterface : TBaseInterface;
    procedure Select(Sender : TObject);
    procedure Deselect(Sender : TObject);
  public
    constructor Create(Creator : TBaseEntry;Path : string);
    procedure Open; virtual;
    procedure Close; virtual;
    property Parent : TBaseEntry read FParent;
    property Name : string read FName write SetName;
    property Size : LongInt read GetSize;
    property Created : TDateTime read FCreation;
    property Changed : TDateTime read FChanged;
    property Attributes : TBaseAttributes read FAttributes write SetAttributes;
    property FullPath : string read FFullPath;
  end;
  
  TBaseFile = class(TBaseEntry)
  public
  end;
  
  TDirectoryWatcher = class
  end;

  TPointerEntryList = array [0..MaxInt shr 3] of TbaseEntry;
  PPointerEntryList = ^TPointerEntryList;

  { TBaseDirectory }

  TBaseDirectory = class(TBaseEntry)
  private
    FList: TList;
    function Get(Index : Integer): TBaseEntry;
    function GetCount: Integer;
    procedure Put(Index : Integer; const AValue: TBaseEntry);
    function Add(const item : TBaseEntry) : Integer;virtual;
    function Remove(Item: TBaseEntry): Integer;
  public
    constructor Create(Creator : TBaseEntry;Path : string;Intf : TBaseInterface = nil);
    destructor Destroy;override;
    procedure Close;override;
    property Count: Integer read GetCount;
    function IndexOf(Item: TBaseEntry): Integer;
    property Items[Index : Integer] : TBaseEntry read Get write Put;
  end;
  
  { TDirectory }

  TDirectory = class(TBaseDirectory)
  private
  protected
  public
    procedure Open;override;
  end;
  
  { TComputerDirectory }

  TComputerDirectory = class(TBaseDirectory)
  public
    procedure Open;override;
    constructor Create;
  end;
  
  { TDrive }

  TDrive = class(TDirectory)
  public
    constructor Create(Creator : TBaseEntry;Path : string);
  end;
  
  TFile = class(TBaseFile)
  end;

  { TBaseInterface }

  TBaseInterface = CLASS
  private
    FFont : TFont;
    FFontSize : Integer;
  protected
    FLanguage : string;
  public
    Tag : LongInt;
    function GetLanguage : string;
    procedure SetLanguage(Lang : string);dynamic;abstract;
    procedure ShowInfo(Msg : String);dynamic;abstract;
    procedure ShowError(Msg : String);dynamic;abstract;
    procedure ShowDebug(Msg : String);dynamic;abstract;
    procedure GetMyInterfaces(Obj : TBaseEntry);dynamic;abstract;
    destructor Destroy;
  published
    property Font : TFont read FFont write FFont;
    property FontSize : Integer read FFontSize write FFontSize;
    property Language : string read FLanguage write SetLanguage;
  end;


implementation

{ TBaseEntry }

function TBaseEntry.GetSize: LongInt;
begin
  Result := 0;
end;

procedure TBaseEntry.SetAttributes(const AValue: TBaseAttributes);
begin
  if FAttributes=AValue then exit;
  FAttributes:=AValue;
end;

procedure TBaseEntry.SetName(const AValue: string);
begin
  if FName=AValue then exit;
  FName:=AValue;
end;

procedure TBaseEntry.Select(Sender : TObject);
begin
end;

procedure TBaseEntry.Deselect(Sender : TObject);
begin
end;

constructor TBaseEntry.Create(Creator: TBaseEntry; Path: string);
begin
  if Creator = nil then
    FFullPath := Path
  else
    FFullPath := Creator.FFullPath+DirectorySeparator+Path;
  if Rpos('\',FFullPath)>0 then
    FName := copy(FFullPath,RPos('\',FFullPath)+1,length(FFullPath))
  else if Rpos('/',FFullPath)>0 then
    FName := copy(FFullPath,RPos('/',FFullPath)+1,length(FFullPath))
  else
    Fname := FFullPath;
  FName := AnsiToUTF8(FName);
  FParent := Creator;
end;

procedure TBaseEntry.Open;
begin
end;

procedure TBaseEntry.Close;
begin
end;

{ TBaseAttribute }

procedure TBaseAttribute.SetCanExecute(const AValue: Boolean);
begin
  if FCanExecute=AValue then exit;
  FCanExecute:=AValue;
end;

procedure TBaseAttribute.SetCanRead(const AValue: Boolean);
begin
  if FCanRead=AValue then exit;
  FCanRead:=AValue;
end;

procedure TBaseAttribute.SetCanWrite(const AValue: Boolean);
begin
  if FCanWrite=AValue then exit;
  FCanWrite:=AValue;
end;

{ TBaseAttributes }

procedure TBaseAttributes.SetGroup(const AValue: TUserGroup);
begin
  if FGroup=AValue then exit;
  FGroup:=AValue;
end;

procedure TBaseAttributes.SetGroupRights(const AValue: TBaseAttribute);
begin
  if FGroupRights=AValue then exit;
  FGroupRights:=AValue;
end;

procedure TBaseAttributes.SetOtherRights(const AValue: TBaseAttribute);
begin
  if FOtherRights=AValue then exit;
  FOtherRights:=AValue;
end;

procedure TBaseAttributes.SetUser(const AValue: TUser);
begin
  if FUser=AValue then exit;
  FUser:=AValue;
end;

procedure TBaseAttributes.SetUserRights(const AValue: TBaseAttribute);
begin
  if FUserRights=AValue then exit;
  FUserRights:=AValue;
end;

{ TBaseDirectory }

function TBaseDirectory.Get(Index : Integer): TBaseEntry;
begin
  if Flist.Count > Index then
    Result := TBaseEntry(FList[Index]);
end;

function TBaseDirectory.GetCount: Integer;
begin
  Result := FList.Count;
end;

procedure TBaseDirectory.Put(Index : Integer; const AValue: TBaseEntry);
begin
  FList[Index] := AValue;
end;

constructor TBaseDirectory.Create(Creator: TBaseEntry; Path: string;
  Intf: TBaseInterface);
begin
  FList := TList.Create;
  inherited Create(Creator, Path);
end;

destructor TBaseDirectory.Destroy;
begin
  inherited Destroy;
  Close;
end;

procedure TBaseDirectory.Close;
var
  aItem: TBaseEntry;
begin
  inherited Close;
  while Count > 0 do
    begin
      aItem := Items[0];
      Remove(Items[0]);
      aItem.Free;
    end;
end;

function TBaseDirectory.Add(const item: TBaseEntry): Integer;
begin
  FList.Add(item);
end;

function TBaseDirectory.Remove(Item: TBaseEntry): Integer;
begin
  fList.Remove(Item);
end;

function TBaseDirectory.IndexOf(Item: TbaseEntry): Integer;
begin
  Result := FList.IndexOf(Item);
end;

{ TBaseInterface }

destructor TBaseInterface.Destroy;
begin
end;

function TBaseInterface.GetLanguage: string;
begin
  Result := FLanguage;
end;

{ TDirectory }

procedure TDirectory.Open;
var
  SR : TSearchRec;
begin
  if FindFirst(FFullPath+DirectorySeparator+'*',faAnyFile,sr)=0 then
    begin
      repeat
        If (sr.Attr and faDirectory) = faDirectory then
          begin
            if copy(sr.name,0,1) <> '.' then
              Add(TDirectory.Create(Self,sr.Name))
          end
        else
          Add(TFile.Create(Self,sr.Name));
      until FindNext(sr)<>0;
    end;
  SysUtils.FindClose(sr);
end;

{ TComputerDirectory }

procedure TComputerDirectory.Open;
var
{$IFDEF MSWINDOWS}
  DriveNum: Integer;
  DriveBits: set of 0..25;
{$ELSE}
  sr : TSearchRec;
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
  Integer(DriveBits) := GetLogicalDrives;
  for DriveNum := 0 to 25 do
    if (DriveNum in DriveBits) then
      Add(TDrive.Create(Self,Char(DriveNum + Ord('a')) + ':\'));
{$ELSE}
  if FindFirst('/',faAnyFile,sr)=0 then
    begin
      repeat
        If (sr.Attr and faDirectory) = faDirectory then
          begin
            if copy(sr.name,0,1) <> '.' then
              Add(TDirectory.Create(Self,sr.Name))
          end;
      until FindNext(sr)<>0;
    end;
  SysUtils.FindClose(sr);
{$ENDIF}
end;

constructor TComputerDirectory.Create;
begin
  FName := strComputer;
  FList := TList.Create;
end;

{ TDrive }

constructor TDrive.Create(Creator: TBaseEntry; Path: string);
begin
  FName := Path;
  FFullPath := Path;
  FParent := Creator;
  FList := TList.Create;
end;

end.

