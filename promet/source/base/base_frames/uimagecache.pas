unit uImageCache;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, CacheCls;
type
  TGetFileEvent = function(Path : string;var NewPath : string) : TStream of object;
  TFileCache = class(TCache)
    procedure FileCacheFreeSlot(ACache: TCache; SlotIndex: Integer);
  private
    FGetFile: TGetFileEvent;
    FURLList: TStringList;
    FNewURLList: TStringList;
    function GetStream(Path : string;var NewPath: string) : TStream;
  public
    constructor Create(ASlotCount: Integer);
    destructor Destroy; override;
    function GetFile(Path : string;var NewPath : string) : TMemoryStream;
    property OnGetFile : TGetFileEvent read FGetFile write FGetFile;
  end;
implementation

procedure TFileCache.FileCacheFreeSlot(ACache: TCache; SlotIndex: Integer);
begin
//  TMemoryStream(ACache.Slots[SlotIndex]).Free;
end;
function TFileCache.GetStream(Path: string;var NewPath: string): TStream;
begin
  Result := nil;
  if Assigned(FGetFile) then
    Result := FGetFile(path,newpath);
end;
constructor TFileCache.Create(ASlotCount: Integer);
begin
  inherited Create(ASlotCount);
  FURLList := TStringList.Create;
  FNewURLList := TStringList.Create;
  Self.OnFreeSlot:=@FileCacheFreeSlot;
end;
destructor TFileCache.Destroy;
var
  i: Integer;
begin
  for i := 0 to FURLList.Count-1 do
    FURLList.Objects[i].Free;
  FURLList.Destroy;
  FNewURLList.Destroy;
  inherited Destroy;
end;
function TFileCache.GetFile(Path: string;var NewPath : string): TMemoryStream;
var
  aMem: TMemoryStream;
  aFile: TStream;
  aIdx: Integer;
begin
  NewPath:=Path;
  Result := nil;
  aIdx := FURLList.IndexOf(Path);
  if aIdx = -1 then
    begin
      aFile := GetStream(Path,NewPath);
      if Assigned(aFile) then
        begin
          aMem := TMemoryStream.Create;
          try
            aMem.CopyFrom(aFile,aFile.Size);
            FUrlList.AddObject(Path,aMem);
            FNewURLList.Add(NewPath);
//            Add(aMem);
            Result := aMem;
          except
            Result := nil;
          end;
          aFile.Free;
        end;
      while FUrlList.Count > Self.SlotCount do
        FUrlList.Delete(FUrlList.Count-1);
    end
  else
    begin
      NewPath:=FNewURLList[aIdx];
      FNewUrlList.Move(aIdx,0);
      Result := TmemoryStream(FURLList.Objects[aIdx]);//TMemoryStream(Self.Data[Self.FindSlot()^.Index]);
      FUrlList.Move(aIdx,0);
    end;
end;
end.

