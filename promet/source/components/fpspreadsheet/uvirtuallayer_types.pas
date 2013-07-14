unit uvirtuallayer_types;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils; 

const VL_ERROR_DISK_FULL=-1;
const VL_INVALID_HANDLE=nil;

type

TvlHandleRecord=record
  VirtualLayer: Pointer;
  Handle: Pointer;
end;

PvlHandleRecord=^TvlHandleRecord;
TvlHandle=Pointer;

TVirtualMountPoint=record
  MountPath: UTF8String;
  MountedVirtual: Pointer;
end;

{ TVirtualLayer_CustomAttributes }

TVirtualLayer_CustomAttributes=Class(TObject)
private
protected
  function GetReadOnly: Boolean; virtual; abstract;
  function GetHidden: Boolean; virtual; abstract;
  function GetSystem: Boolean; virtual; abstract;
  function GetLastModification: TDateTime; virtual; abstract;
//  procedure SetReadOnly(const AValue: Boolean); virtual; abstract;
//  procedure SetHidden(const AValue: Boolean); virtual; abstract;
//  procedure SetSystem(const AValue: Boolean); virtual; abstract;
//  procedure SetLastModification(const AValue: TDateTime); virtual; abstract;
public
  property IsReadOnly: Boolean read GetReadOnly; //write SetReadOnly;
  property IsHidden: Boolean read GetHidden; //write SetHidden;
  property IsSystem: Boolean read GetSystem; //write SetSystem;
  property LastModification: TDateTime read GetLastModification; //write SetLastModification;
end;

{ TVirtualLayer_Item }

TVirtualLayer_Item=class(TObject)
private
protected
  function GetAttributesHumanReadable: UTF8String; virtual;
  function GetAttributes: TVirtualLayer_CustomAttributes; virtual;
public
  Name: UTF8String;
  Size: int64;
  IsFolder: Boolean;
  IsMountPoint: Boolean;
  property Attributes: TVirtualLayer_CustomAttributes read GetAttributes;
  property AttributesHumanReadable: UTF8String read GetAttributesHumanReadable;
  destructor Destroy; override;
end;

{ TVirtualLayer_FolderList }

TVirtualLayer_FolderList=class(TList)
private
  FPath: UTF8String;
  function GetAttributes: TVirtualLayer_CustomAttributes; virtual;
  function GetItems(index: integer): TVirtualLayer_Item;
  procedure SetItems(index: integer; const AValue: TVirtualLayer_Item);
protected
public
  procedure AddInheritedPath(const APath: UTF8String);
  function Extract(index: integer): TVirtualLayer_Item;
  procedure Sort(const Ascending: Boolean=true;const FoldersFirst: Boolean=true);
  procedure Delete(const Index: integer);
  property Path: UTF8String read FPath;
  property Items[index: integer]:TVirtualLayer_Item read GetItems write SetItems; default;
  property Attributes: TVirtualLayer_CustomAttributes read GetAttributes;
  Constructor Create(const APath: UTF8String);
  Destructor Destroy(); override;
end;

implementation

function fSortAscendingFoldersFirst(item1,item2: Pointer): integer; forward;

{ TVirtualLayer_FolderList }

function TVirtualLayer_FolderList.GetItems(index: integer): TVirtualLayer_Item;
begin
  Result:=TVirtualLayer_Item(inherited items[Index]);
end;

function TVirtualLayer_FolderList.GetAttributes: TVirtualLayer_CustomAttributes;
begin
  Result:=nil;
end;

procedure TVirtualLayer_FolderList.SetItems(index: integer;
  const AValue: TVirtualLayer_Item);
begin
  inherited items[index]:=AValue;
end;

procedure TVirtualLayer_FolderList.AddInheritedPath(const APath: UTF8String);
begin
  FPath:=APath+RightStr(FPath,Length(FPath)-1);
end;

function TVirtualLayer_FolderList.Extract(index: integer): TVirtualLayer_Item;
begin
  Result:=Items[index];
  inherited Delete(index);
end;

procedure TVirtualLayer_FolderList.Sort(const Ascending: Boolean;
  const FoldersFirst: Boolean);
begin
  inherited Sort(@fSortAscendingFoldersFirst);
end;

procedure TVirtualLayer_FolderList.Delete(const Index: integer);
begin
  TObject(Get(Index)).Free;
  inherited Delete(Index);
end;

constructor TVirtualLayer_FolderList.Create(const APath: UTF8String);
begin
  inherited Create;
  FPath:=APath;
end;

destructor TVirtualLayer_FolderList.Destroy();
var
  j: integer;
begin
  for j := 0 to Count-1 do begin
    TObject(Items[j]).Free;
  end;
  inherited Destroy();
end;

{FORWARDS}
function fSortAscendingFoldersFirst(item1, item2: Pointer): integer;
var
  l1,l2: TVirtualLayer_Item;
begin
  l1:=TVirtualLayer_Item(item1);
  l2:=TVirtualLayer_Item(item2);
  if L1.IsFolder and L2.IsFolder then begin
    Result:=CompareText(L1.Name,L2.Name);
  end else begin
    if L1.IsFolder then begin
      Result:=-1;
    end else if L2.IsFolder then begin
      Result:=1;
    end else begin
      Result:=CompareText(L1.Name,L2.Name);
    end;
  end;
end;

{ TVirtualLayer_Item }

function TVirtualLayer_Item.GetAttributesHumanReadable: UTF8String;
var
  T: UTF8String;
  A: TVirtualLayer_CustomAttributes;
begin
  A:=Attributes;
  if A=nil then Exit;
  Result:=Result+'Regular attributes: ';
  if A.IsReadOnly then T:=T+'Read Only,';
  if A.IsHidden then T:=T+'Hidden,';
  if A.IsSystem then T:=T+'System file,';
  T:=LeftStr(T,Length(T)-1);
  Result:=Result+T+#13+#10;
  Result:=Result+'Last modification: '+FormatDateTime(LongDateFormat+' '+LongTimeFormat,A.LastModification)+#13+#10;
end;

function TVirtualLayer_Item.GetAttributes: TVirtualLayer_CustomAttributes;
begin
  Result:=nil;
end;

destructor TVirtualLayer_Item.Destroy;
begin
  if Attributes<>nil Then Attributes.Free;
  inherited Destroy;
end;

end.

