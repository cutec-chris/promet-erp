unit uvirtuallayer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, strutils,
  uvirtuallayer_types, uvirtuallayer_stream;
  
type

  { TVirtualLayer }

  TVirtualLayer=class(TObject)
  private
    FMountPoints: array of TVirtualMountPoint;
    m_Critical: TRTLCriticalSection;
    FFreeLayerStreamOnFree: Boolean;
    function GetIsWritableMedia: Boolean;
    function GetMountPath: UTF8String;
    function GetRootLayer: TVirtualLayer;
    procedure SetParentLayer(const AValue: TVirtualLayer);
    function PathMountedInCurrentLayer(const APath: UTF8String): TVirtualLayer;
    function BuildPathFromIndex(const ASplittedPath: TStringList; const Index: integer): UTF8String;
    function FindMounted(const APath: UTF8String; out ARemainPath: UTF8String): TVirtualLayer; overload;
    function FindMounted(const APath: UTF8String): TVirtualLayer; overload;
    function PreviousPath(const APath: UTF8String): UTF8String;
    function StripMountPoints(const APath: UTF8String): UTF8String;
  protected
    FParentLayer: TVirtualLayer;
    FVirtualLayerStream: TStream;
    FMountedInPath: UTF8String;

    procedure SplitPath(const APath: UTF8String;const ASplittedPath: TStringList);
    function FileToVirtualLayer(const ATargetFileName: UTF8String;out TargetVL: TVirtualLayer): Boolean;
    function AcrossLayersCopy(const ASourceFileName,ATargetFileName: UTF8String): Boolean;
    function AcrossLayersMove(const ASourceFileName,ATargetFileName: UTF8String): Boolean;
    function NormalizePath(const APath: UTF8String): UTF8String;
    function RemoveRootPathDelimiter(const APath: UTF8String): UTF8String;
    function Initialize(): boolean; virtual;

    //Functions to be implemented in specializations
    function intfOpenFile(const AFileName: UTF8String; const AMode: cardinal): TvlHandle; virtual; abstract;
    function intfCloseFile(const Handle: TvlHandle): Boolean; virtual; abstract;
    function intfFindList(const APath: UTF8String; const AMask: UTF8String): TVirtualLayer_FolderList; virtual; abstract;
    function intfSeek(const AHandle: TvlHandle; const APosition: int64; const Origin: word): int64; virtual; abstract;
    function intfRead(const Handle: TvlHandle; const Buffer: PBYTE; const Size: int64): int64; virtual; abstract;
    function intfWrite(const Handle: TvlHandle; const Buffer: PBYTE; const Size: int64): int64; virtual; abstract;
    function intfGetFileSize(const AHandle: TvlHandle): int64; virtual; abstract;
    function intfSetFileSize(const AHandle: TvlHandle; const ANewFileSize: int64): Boolean; virtual; abstract;
    function intfDeleteFile(const AFileName: UTF8String): boolean; virtual; abstract;
    function intfGetFreeSpace(const APath: UTF8String): int64; virtual; abstract;
    function intfIsWritableMedia(): Boolean; virtual; abstract;
    function intfMakeFolder(const AFolder: UTF8String): Boolean; virtual; abstract;
    function intfRemoveFolder(const AFolder: UTF8String): Boolean; virtual; abstract;
    function intfCopy(const ASourceFileName,ATargetFileName: UTF8String): Boolean; virtual;
    function intfMove(const ASourceFileName,ATargetFileName: UTF8String): Boolean; virtual;

    procedure Lock(); virtual;
    procedure Unlock(); virtual;
  public

    function MakeFolder(const AFolder: UTF8String): Boolean;
    function RemoveFolder(const AFolder: UTF8String): Boolean;
    function DeleteFile(const AFileName: UTF8String): Boolean;
    function Read(const Handle: TvlHandle; const Buffer: PBYTE; const Size: int64): int64;
    function Write(const Handle: TvlHandle; const Buffer: PBYTE; const Size: int64): int64;
    function FindList(const APath: UTF8String; const AMask: UTF8String): TVirtualLayer_FolderList;
    function OpenFile(const FileName: UTF8String; const Mode: cardinal): TvlHandle;
    function CloseFile(const Handle: TvlHandle): Boolean;
    function GetFileSize(const AHandle: TvlHandle): int64;
    function SetFileSize(const AHandle: TvlHandle; const ANewSize: int64): Boolean;
    function Seek(const AHandle: TvlHandle; const APosition: int64; const Origin: Word): int64;
    function FileExists(const AFileName: UTF8String): Boolean; virtual;
    function MoveFile(const ASourceFileName,ATargetFileName: UTF8String): Boolean; virtual;
    function CopyFile(const ASourceFileName,ATargetFileName: UTF8String): Boolean; virtual;
    function GetFreeSpace(const APath: UTF8String): int64;
    function FindFirst(const APath: String;const Attr: LongInt;out Rlst: sysutils.TSearchRec): LongInt;
    function FindNext(var Rlst: sysutils.TSearchRec): LongInt;
    procedure FindClose(Rlst: sysutils.TSearchRec);

    procedure SplitFileNamePath(const AFullPath: UTF8String; out APath: UTF8String; out AFileName: UTF8String);
    function PathToVirtualLayer(const APath: UTF8String): TVirtualLayer;
    function CreateStream(const AFileName: UTF8String; const AMode: Cardinal): TVirtualLayer_Stream;
    function Mount(const AMountPath: UTF8String; const AVirtualLayer: TVirtualLayer): Boolean;
    function UnMount(const AMountPath: UTF8String; const FreeAssociatedVirtualLayer: Boolean=true): Boolean;

    property FreeLayerStreamOnFree: Boolean read FFreeLayerStreamOnFree write FFreeLayerStreamOnFree;
    property IsWritableMedia: Boolean read GetIsWritableMedia;
    property MountPath: UTF8String read GetMountPath;
    property RootLayer: TVirtualLayer read GetRootLayer;
    property ParentLayer: TVirtualLayer read FParentLayer write SetParentLayer;

    Constructor Create(const AVirtualLayerStream: TStream);
    procedure PrepareDestroy(); virtual;
    Destructor Destroy(); override;
  end;

implementation

type
TFileRecLocal=record
  FL: TVirtualLayer_FolderList;
  Attr: LongInt;
end;
PFileRecLocal=^TFileRecLocal;

{ TVirtualLayer }

procedure TVirtualLayer.SplitPath(const APath: UTF8String;const ASplittedPath: TStringList);
var
  j,k: integer;
  LB: integer;
  Chunk: UTF8String;
  ChunkC: integer;
begin
  ASplittedPath.Clear;
  LB:=1;
  for j := 1 to Length(APath) do begin
    if APath[j]='/' then begin
      SetLength(Chunk,j-LB);
      ChunkC:=1;
      for k := LB to j-1 do begin
        Chunk[ChunkC]:=APath[k];
        inc(ChunkC);
      end;
      if Chunk<>'' Then begin
        ASplittedPath.Add(Chunk);
      end;
      LB:=j+1;
    end;
  end;
  if LB<Length(APath) then begin
    ChunkC:=1;
    SetLength(Chunk,Length(APath)-LB+1);
    for k := LB to Length(APath) do begin
      Chunk[ChunkC]:=APath[k];
      inc(ChunkC);
    end;
    if Chunk<>'' Then begin
      ASplittedPath.Add(Chunk);
    end;
  end;
end;

procedure TVirtualLayer.SetParentLayer(const AValue: TVirtualLayer);
begin
  if AValue<>nil Then begin
    DoneCriticalsection(m_Critical);
  end;
  FParentLayer:=AValue;
end;

function TVirtualLayer.GetIsWritableMedia: Boolean;
begin
  Result:=false;
  if intfIsWritableMedia() Then begin
    //If this media is writable, all parents must be also.
    if Assigned(FParentLayer) Then begin
      if FParentLayer.IsWritableMedia Then begin
        Result:=true;
      end;
    end else begin
      Result:=true;
    end;
  end;
end;

function TVirtualLayer.FileToVirtualLayer(const ATargetFileName: UTF8String;
  out TargetVL: TVirtualLayer): Boolean;
var
  FileName: UTF8String;
  Path: UTF8String;
begin
  TargetVL:=nil;
  SplitFileNamePath(ATargetFileName, Path, FileName);
  if FileName='' then begin
    Result:=false;
    exit;
  end;
  TargetVL:=FindMounted(Path);
  if TargetVL=nil then begin
    result:=false;
    exit;
  end;
  Result:=true;
end;

function TVirtualLayer.GetMountPath: UTF8String;
begin
  if Assigned(FParentLayer) then begin
    Result:=FParentLayer.MountPath+copy(FMountedInPath,2,Length(FMountedInPath));
  end else begin
    Result:='/';
  end;
end;

function TVirtualLayer.GetRootLayer: TVirtualLayer;
var
  PL: TVirtualLayer;
begin
  if FParentLayer<>nil then begin
    PL:=FParentLayer.GetRootLayer;
    if PL=nil then begin
      Result:=Self;
    end else begin
      Result:=PL;
    end;
  end else begin
    Result:=Self;
  end;
end;

function TVirtualLayer.PathMountedInCurrentLayer(const APath: UTF8String): TVirtualLayer;
var
  j: integer;
  Count: integer;
begin
  Result:=nil;
  Lock();
  Count:=Length(FMountPoints);
  for j := 0 to Count-1 do begin
    with FMountPoints[j] do begin
      if MountPath=APath Then begin
        Result:=TVirtualLayer(MountedVirtual);
        break;
      end;
    end;
  end;
  Unlock();
end;

function TVirtualLayer.BuildPathFromIndex(const ASplittedPath: TStringList;
  const Index: integer): UTF8String;
var
  GluePath: UTF8String;
  j: integer;
begin
  GluePath:='';
  for j := Index to ASplittedPath.Count-1 do begin
    GluePath:=GluePath+'/'+ASplittedPath[j];
  end;
  Result:=GluePath;
end;

procedure TVirtualLayer.Lock();
begin
  if Assigned(FParentLayer) then begin
    FParentLayer.Lock();
  end else begin
    EnterCriticalsection(m_Critical);
  end;
end;

procedure TVirtualLayer.Unlock();
begin
  if Assigned(FParentLayer) then begin
    FParentLayer.Unlock();
  end else begin
    LeaveCriticalsection(m_Critical);
  end;
end;

function TVirtualLayer.MakeFolder(const AFolder: UTF8String): Boolean;
var
  VL: TVirtualLayer;
  RemainPath: UTF8String;
begin
  VL:=FindMounted(AFolder,RemainPath);
  if Assigned(VL) Then begin
    Result:=VL.MakeFolder(RemainPath);
  end else begin
    Result:=intfMakeFolder(AFolder);
  end;
end;

function TVirtualLayer.RemoveFolder(const AFolder: UTF8String): Boolean;
var
  VL: TVirtualLayer;
  RemainPath: UTF8String;
begin
  VL:=FindMounted(AFolder,RemainPath);
  if Assigned(VL) Then begin
    Result:=VL.RemoveFolder(RemainPath);
  end else begin
    Result:=intfRemoveFolder(AFolder);
  end;
end;

function TVirtualLayer.DeleteFile(const AFileName: UTF8String): Boolean;
var
  VL: TVirtualLayer;
  RemainPath: UTF8String;
begin
  VL:=FindMounted(AFileName,RemainPath);
  if Assigned(VL) Then begin
    Result:=VL.DeleteFile(RemainPath);
  end else begin
    Result:=intfDeleteFile(AFileName);
  end;
end;

function TVirtualLayer.Read(const Handle: TvlHandle; const Buffer: PBYTE;
  const Size: int64): int64;
var
  vlHandleRecord: PvlHandleRecord;
begin
  vlHandleRecord:=PvlHandleRecord(Handle);
  Result:=TVirtualLayer(vlHandleRecord^.VirtualLayer).intfRead(vlHandleRecord^.Handle,Buffer,Size);
end;

function TVirtualLayer.Write(const Handle: TvlHandle; const Buffer: PBYTE;
  const Size: int64): int64;
var
  vlHandleRecord: PvlHandleRecord;
begin
  vlHandleRecord:=PvlHandleRecord(Handle);
  Result:=TVirtualLayer(vlHandleRecord^.VirtualLayer).intfWrite(vlHandleRecord^.Handle,Buffer,Size);
end;

function TVirtualLayer.FindList(const APath: UTF8String; const AMask: UTF8String
  ): TVirtualLayer_FolderList;
var
  VL: TVirtualLayer;
  RemainPath: UTF8String;
  MyPath: UTF8String;
  FullPath: UTF8String;
  j: integer;
  VI: TVirtualLayer_Item;
  MountP: UTF8String;
  Found: Boolean;
begin
  FullPath:=NormalizePath(APath);
  VL:=FindMounted(FullPath,RemainPath);
  MyPath:=LeftStr(FullPath,Length(FullPath)-Length(RemainPath));
  if Assigned(VL) Then begin
    Result:=VL.FindList(NormalizePath(RemainPath),AMask);
    if Result<>nil Then begin
      Result.AddInheritedPath(MyPath);
    end;
  end else begin
    Result:=intfFindList(FullPath,AMask);
    //Add mount points for this folder
    for j := 0 to Length(FMountPoints)-1 do begin
      MountP:=PreviousPath(FMountPoints[j].MountPath);
      if MountP<>'' Then begin
        if MountP=MyPath Then begin
          VI:=TVirtualLayer_Item.Create;
          VI.Name:=RightStr(FMountPoints[j].MountPath,Length(FMountPoints[j].MountPath)-Length(MountP));
          VI.Name:=LeftStr(VI.Name,Length(VI.Name)-1);
          VI.Size:=0;
          VI.IsFolder:=true;
          Result.Add(VI);
        end;
      end;
    end;
    //Verify ".." folder exists (previous folder)
    //Unless the current folder is the Top one.
    Found:=false;
    for j := 0 to Result.Count-1 do begin
      VI:=TVirtualLayer_Item(Result[j]);
      if VI.Name='..' Then begin
        Found:=True;
        break;
      end;
    end;
    if (not Found) and (FParentLayer<>nil) Then begin
      VI:=TVirtualLayer_Item.Create;
      VI.Name:='..';
      VI.Size:=0;
      VI.IsFolder:=true;
      Result.Insert(0,VI);
    end;
  end;
end;

function TVirtualLayer.FindMounted(const APath: UTF8String; out ARemainPath: UTF8String): TVirtualLayer;
var
  PathList: TStringList;
  j: integer;
  CurPath: UTF8String;
  VirtualLayer: TVirtualLayer;
begin
  ARemainPath:='';
  Result:=nil;
  PathList:=TStringList.Create;
  SplitPath(NormalizePath(APath),PathList);
  //Remove empty strings
  for j := PathList.Count-1 downto 0 do begin
    if PathList[j]='' Then begin
      PathList.Delete(j);
    end;
  end;
  Lock();
  CurPath:='/';
  for j := 0 to PathList.Count-1 do begin
    CurPath:=NormalizePath(Curpath+PathList[j]);
    VirtualLayer:=PathMountedInCurrentLayer(CurPath);
    if VirtualLayer<>nil Then begin;
      Result:=VirtualLayer;
      ARemainPath:=BuildPathFromIndex(PathList,j+1);
      break;
    end;
  end;
  PathList.Free;
  Unlock();
end;

function TVirtualLayer.FindMounted(const APath: UTF8String): TVirtualLayer;
var
  Remain: UTF8String;
  VL: TVirtualLayer;
  Path: UTF8String;
begin
  Remain:=APath;
  VL:=nil;
  Repeat
    Result:=VL;
    Path:=Remain;
    VL:=FindMounted(Path,Remain);
  until VL=nil;
end;

function TVirtualLayer.NormalizePath(const APath: UTF8String): UTF8String;
//It should also resolve /../ path expressions.
var
  j: integer;
  PL: TStringList;
  Skip: Boolean;
begin
  if RightStr(APath,1)<>'/' then begin
    Result:=APath+'/';
  end else begin
    Result:=APath;
  end;
  if LeftStr(Result,1)<>'/' then begin
    Result:='/'+Result;
  end;
  Result:=StringReplace(Result,'/./','/',[]);
  if PosEx('../',Result)>0 then begin
    //Dissasemble path and reassemble it to remove the /../ path
    PL:=TStringList.Create;
    SplitPath(Result,PL);
    for j := PL.Count-1 downto 0 do begin
      if PL[j]='' Then PL.Delete(j);
    end;
    Skip:=False;
    for j := PL.Count-1 downto 0 do begin
      if Skip then begin
        PL.Delete(j);
        Skip:=false;
      end else begin
        if PL[j]='..' Then begin
          PL.Delete(j);
          Skip:=true;
        end;
      end;
    end;
    Result:='/';
    for j := 0 to PL.Count-1 do begin
      Result:=Result+PL[j]+'/';
    end;
    PL.Free;
  end;
end;

function TVirtualLayer.RemoveRootPathDelimiter(const APath: UTF8String
  ): UTF8String;
begin
  if Length(APath)>0 then begin
    if APath[1]='/' then begin
      Result:=Copy(APath,2,length(APath)-1);
    end;
  end;
end;

function TVirtualLayer.PreviousPath(const APath: UTF8String): UTF8String;
var
  PathParts: TStringList;
  j: integer;
begin
  PathParts:=TStringList.Create;
  SplitPath(APath,PathParts);
  for j := PathParts.Count-1 downto 0 do begin
    if PathParts[j]='' Then begin
      PathParts.Delete(j);
    end;
  end;
  Result:='/';
  for j := 0 to PathParts.Count-2 do begin
    Result:=Result+PathParts[j]+'/';
  end;
  PathParts.Free;
end;

function TVirtualLayer.StripMountPoints(const APath: UTF8String): UTF8String;
var
  PathList: TStringList;
  j: integer;
  CurPath: UTF8String;
  VirtualLayer: TVirtualLayer;
  ARemainPath: UTF8String;
  OtherPath: UTF8String;
begin
  ARemainPath:='';
  PathList:=TStringList.Create;
  SplitPath(NormalizePath(APath),PathList);
  //Remove empty strings
  for j := PathList.Count-1 downto 0 do begin
    if PathList[j]='' Then begin
      PathList.Delete(j);
    end;
  end;
  Lock();
  CurPath:='/';
  for j := 0 to PathList.Count-1 do begin
    CurPath:=NormalizePath(Curpath+PathList[j]);
    VirtualLayer:=PathMountedInCurrentLayer(CurPath);
    if VirtualLayer<>nil Then begin;
      ARemainPath:=BuildPathFromIndex(PathList,j+1);
      OtherPath:=VirtualLayer.StripMountPoints(ARemainPath);
      if OtherPath='' Then begin
        Result:=ARemainPath;
      end;
      break;
    end;
  end;
  PathList.Free;
  Unlock();
end;

function TVirtualLayer.Initialize(): boolean;
begin
  result:=true;
end;

function TVirtualLayer.intfCopy(const ASourceFileName,
  ATargetFileName: UTF8String): Boolean;
var
  L: TVirtualLayer;
begin
  L:=RootLayer;
  Result:=L.AcrossLayersCopy(MountPath+RemoveRootPathDelimiter(ASourceFileName),
                           MountPath+RemoveRootPathDelimiter(ATargetFileName));
end;

function TVirtualLayer.intfMove(const ASourceFileName,
  ATargetFileName: UTF8String): Boolean;
begin
  Result:=AcrossLayersMove(MountPath+RemoveRootPathDelimiter(ASourceFileName),
                           MountPath+RemoveRootPathDelimiter(ATargetFileName));
end;

function TVirtualLayer.AcrossLayersMove(const ASourceFileName,
  ATargetFileName: UTF8String): Boolean;
begin
  //By default move a file creating a new copy and deleting the old one.
  //This function should be specialized for each layer.
  if not intfCopy(ASourceFileName,ATargetFileName) then begin
    DeleteFile(ATargetFileName);
    result:=false;
  end else begin
    DeleteFile(ASourceFileName);
    result:=true;
  end;
end;

function TVirtualLayer.AcrossLayersCopy(const ASourceFileName,
  ATargetFileName: UTF8String): Boolean;
var
  SourceHandle, TargetHandle: TvlHandle;
  FileSize: int64;
  Buffer: array [0..1023] of BYTE;
  WriteBytes: int64;
begin
  //By default copy a file.
  //This function should be specialized for each layer.
  Result:=true;
  SourceHandle:=OpenFile(ASourceFileName,fmOpenRead);
  if SourceHandle=nil Then begin
    Result:=false;
    Exit;
  end;
  TargetHandle:=OpenFile(ATargetFileName,fmCreate);
  if TargetHandle=nil Then begin
    CloseFile(SourceHandle);
    Result:=false;
    Exit;
  end;
  FileSize:=GetFileSize(SourceHandle);
  while FileSize>0 do begin
    if FileSize>1024 Then begin
      if Read(SourceHandle,@Buffer[0],1024)<>1024 Then begin
        Result:=false;
        break;
      end else begin
        WriteBytes:=1024;
        dec(FileSize,1024);
      end;
    end else begin
      if Read(SourceHandle,@Buffer[0],FileSize)<>FileSize Then begin
        Result:=false;
        break;
      end else begin
        WriteBytes:=FileSize;
        FileSize:=0;
      end;
    end;
    if Write(TargetHandle,@Buffer[0],WriteBytes)<>WriteBytes Then begin
      Result:=false;
      break;
    end;
  end;
  CloseFile(SourceHandle);
  CloseFile(TargetHandle);
end;

function TVirtualLayer.MoveFile(const ASourceFileName,
  ATargetFileName: UTF8String): Boolean;
var
  SourceVL,TargetVL: TVirtualLayer;
begin
  if not FileToVirtualLayer(ATargetFileName,TargetVL) Then begin
    Result:=false;
    exit;
  end;
  if not FileToVirtualLayer(ASourceFileName,SourceVL) Then begin
    Result:=false;
    exit;
  end;
  if SourceVL=TargetVL then begin
    //Move in the same virtual layer. invoke intfmove.
    Result:=SourceVL.intfMove(StripMountPoints(ASourceFileName),StripMountPoints(ATargetFileName));
  end else begin
    Result:=intfMove(ASourceFileName,ATargetFileName);
  end;
end;

function TVirtualLayer.CopyFile(const ASourceFileName, ATargetFileName: UTF8String
  ): Boolean;
var
  SourceVL,TargetVL: TVirtualLayer;
begin
  if not FileToVirtualLayer(ATargetFileName,TargetVL) Then begin
    Result:=false;
    exit;
  end;
  if not FileToVirtualLayer(ASourceFileName,SourceVL) Then begin
    Result:=false;
    exit;
  end;
  if SourceVL=TargetVL then begin
    //Move in the same virtual layer. invoke intfmove.
    Result:=SourceVL.intfCopy(StripMountPoints(ASourceFileName),StripMountPoints(ATargetFileName));
  end else begin
    Result:=AcrossLayersCopy(ASourceFileName,ATargetFileName);
  end;
end;

function TVirtualLayer.OpenFile(const FileName: UTF8String; const Mode: cardinal): TvlHandle;
var
  VL: TVirtualLayer;
  RemainPath: UTF8String;
  vlHandleRecord: PvlHandleRecord;
  Handle: TvlHandle;
begin
  VL:=FindMounted(FileName,RemainPath);
  if Assigned(VL) Then begin
    Result:=VL.OpenFile(RemainPath,Mode);
  end else begin
    vlHandleRecord:=nil;
    Handle:=intfOpenFile(FileName,Mode);
    if Handle=VL_INVALID_HANDLE then begin
      Result:=VL_INVALID_HANDLE;
    end else begin
      GetMem(vlHandleRecord,sizeof(TvlHandleRecord));
      vlHandleRecord^.Handle:=Handle;
      vlHandleRecord^.VirtualLayer:=Self;
      Result:=TvlHandle(vlHandleRecord);
    end;
  end;
end;

function TVirtualLayer.CloseFile(const Handle: TvlHandle): Boolean;
var
  vlHandleRecord: PvlHandleRecord;
begin
  vlHandleRecord:=PvlHandleRecord(Handle);
  Result:=TVirtualLayer(vlHandleRecord^.VirtualLayer).intfCloseFile(vlHandleRecord^.Handle);
  FreeMem(vlHandleRecord);
end;

function TVirtualLayer.GetFileSize(const AHandle: TvlHandle): int64;
var
  vlHandleRecord: PvlHandleRecord;
begin
  vlHandleRecord:=PvlHandleRecord(AHandle);
  Result:=TVirtualLayer(vlHandleRecord^.VirtualLayer).intfGetFileSize(vlHandleRecord^.Handle);
end;

function TVirtualLayer.SetFileSize(const AHandle: TvlHandle;
  const ANewSize: int64): Boolean;
var
  vlHandleRecord: PvlHandleRecord;
begin
  vlHandleRecord:=PvlHandleRecord(AHandle);
  Result:=TVirtualLayer(vlHandleRecord^.VirtualLayer).intfSetFileSize(vlHandleRecord^.Handle,ANewSize);
end;

function TVirtualLayer.Seek(const AHandle: TvlHandle; const APosition: int64; const Origin: Word): int64;
var
  vlHandleRecord: PvlHandleRecord;
begin
  vlHandleRecord:=PvlHandleRecord(AHandle);
  Result:=TVirtualLayer(vlHandleRecord^.VirtualLayer).intfSeek(vlHandleRecord^.Handle,APosition,Origin);
end;

function TVirtualLayer.FileExists(const AFileName: UTF8String): Boolean;
var
  fl: TVirtualLayer_FolderList;
  Path: UTF8String;
  FileName: UTF8String;
  j: integer;
begin
  Result:=false;
  SplitFileNamePath(AFileName,Path,FileName);
  fl:=FindList(Path,FileName);
  for j := 0 to FL.Count-1 do begin
    if TVirtualLayer_Item(FL[j]).Name=FileName Then begin
      Result:=true;
      break;
    end;
  end;
  fl.Free;
end;

procedure TVirtualLayer.SplitFileNamePath(const AFullPath: UTF8String; out
  APath: UTF8String; out AFileName: UTF8String);
var
  j: integer;
begin
  APath:=AFullPath;
  AFileName:='';
  for j := Length(AFullPath) downto 1 do begin
    if AFullPath[j]='/' Then begin
      APath:=LeftStr(AFullPath,j);
      AFileName:=RightStr(AFullPath,Length(AFullPath)-Length(APath));
      break;
    end;
  end;
end;

function TVirtualLayer.Mount(const AMountPath: UTF8String;
  const AVirtualLayer: TVirtualLayer): Boolean;
var
  j: integer;
  VL: TVirtualLayer;
  RemainPath: UTF8String;
begin
  if AVirtualLayer.FParentLayer<>nil Then begin
    Result:=false;
    Exit;
  end;
  Lock();
  VL:=FindMounted(AMountPath,RemainPath);
  if Assigned(VL) Then begin
    Result:=VL.Mount(RemainPath,AVirtualLayer);
  end else begin
    j:=Length(FMountPoints);
    SetLength(FMountPoints,j+1);
    FMountPoints[j].MountPath:=NormalizePath(AMountPath);
    FMountPoints[j].MountedVirtual:=AVirtualLayer;
    AVirtualLayer.SetParentLayer(Self);
    AVirtualLayer.FMountedInPath:=FMountPoints[j].MountPath;
    Result:=true;
  end;
  Unlock();
end;

function TVirtualLayer.UnMount(const AMountPath: UTF8String;
  const FreeAssociatedVirtualLayer: Boolean): Boolean;
var
  j,k: integer;
  VirtualLayer: TVirtualLayer;
  RemainPath: UTF8String;
begin
  Result:=false;
  Lock();
  VirtualLayer:=FindMounted(AMountPath,RemainPath);
  if Assigned(VirtualLayer) Then begin
    Result:=VirtualLayer.UnMount(RemainPath,FreeAssociatedVirtualLayer);
  end else begin
    for j := 0 to Length(FMountPoints)-1 do begin
      if FMountPoints[j].MountPath=AMountPath Then begin
        VirtualLayer:=TVirtualLayer(FMountPoints[j].MountedVirtual);
        for k := j+1 to Length(FMountPoints)-1 do begin
          FMountPoints[k-1].MountPath:=FMountPoints[k].MountPath;
          FMountPoints[k-1].MountedVirtual:=FMountPoints[k].MountedVirtual;
        end;
        SetLength(FMountPoints,Length(FMountPoints)-1);
        if FreeAssociatedVirtualLayer Then begin
          VirtualLayer.Free;
        end;
        Result:=true;
        break;
      end;
    end;
  end;
  Unlock();
end;

function TVirtualLayer.PathToVirtualLayer(const APath: UTF8String
  ): TVirtualLayer;
var
  RemainPath: UTF8String;
begin
  Result:=FindMounted(APath,RemainPath);
  if Result=nil Then Result:=Self;
end;

function TVirtualLayer.CreateStream(const AFileName: UTF8String;
  const AMode: Cardinal): TVirtualLayer_Stream;
begin
  try
    result:=TVirtualLayer_Stream.Create(Self,AFileName,AMode);
  except
    on E: EStreamError do begin
        Result:=nil;
      end;
  end;
end;

function TVirtualLayer.GetFreeSpace(const APath: UTF8String): int64;
var
  VL: TVirtualLayer;
  RemainPath: UTF8String;
begin
  VL:=FindMounted(APath,RemainPath);
  if Assigned(VL) Then begin
    Result:=VL.GetFreeSpace(RemainPath);
  end else begin
    Result:=intfGetFreeSpace(APath);
  end;
end;

function TVirtualLayer.FindFirst(const APath: String; const Attr: LongInt; out
  Rlst: sysutils.TSearchRec): LongInt;
var
  FindL: TVirtualLayer_FolderList;
  Path: UTF8String;
  Mask: UTF8String;
  LHandle: PFileRecLocal;
begin
  Result:=-1;
  with Rlst do begin
    Time:=0;
    Size:=0;
    Attr:=0;
    Name:='';
    ExcludeAttr:=0;
  end;
  SplitFileNamePath(APath,Path,Mask);
  FindL:=FindList(APath,Mask);
  LHandle:=nil;
  GetMem(LHandle,sizeof(TFileRecLocal));
  LHandle^.FL:=FindL;
  LHandle^.Attr:=Attr;
  {$HINTS OFF}
  {$ifdef UNIX}
  Rlst.FindHandle:=LHandle;
  {$else}
  Rlst.FindHandle:=PtrUint(LHandle);
  {$endif}
  {$HINTS ON}
  if Assigned(FindL) Then begin
    if FindL.Count>0 Then begin
      With FindL[0] do begin
        Rlst.Name:=Name;
        Rlst.Size:=Size;
        if IsFolder or IsMountPoint Then begin
          Rlst.Attr:=Rlst.Attr+faDirectory;
        end;
      end;
      FindL.Delete(0);
      Result:=0;
    end;
  end;
end;

function TVirtualLayer.FindNext(var Rlst: sysutils.TSearchRec): LongInt;
var
  FindL: TVirtualLayer_FolderList;
  LHandle: PFileRecLocal;
begin
  Result:=-1;
  with Rlst do begin
    Time:=0;
    Size:=0;
    Attr:=0;
    Name:='';
    ExcludeAttr:=0;
  end;
  //Hint non portable conversion, it should work, but not tested.
  LHandle:=PFileRecLocal(Rlst.FindHandle);
  FindL:=TVirtualLayer_FolderList(LHandle^.FL);
  if Assigned(FindL) Then begin
    if FindL.Count>0 Then begin
      With FindL[0] do begin
        Rlst.Name:=Name;
        Rlst.Size:=Size;
        if IsFolder or IsMountPoint Then begin
          Rlst.Attr:=Rlst.Attr or faDirectory;
        end;
      end;
      FindL.Delete(0);
      Result:=0;
    end;
  end;
end;

procedure TVirtualLayer.FindClose(Rlst: sysutils.TSearchRec);
var
  FindL: TVirtualLayer_FolderList;
  LHandle: PFileRecLocal;
begin
  //Hint: non portable conversion. Not tested but it should work.
  LHandle:=PFileRecLocal(Rlst.FindHandle);
  FindL:=TVirtualLayer_FolderList(LHandle^.FL);
  if Assigned(FindL) Then FindL.Free;
  FreeMem(LHandle);
end;

constructor TVirtualLayer.Create(const AVirtualLayerStream: TStream);
begin
  InitCriticalSection(m_Critical);
  FVirtualLayerStream:=AVirtualLayerStream;
  if Assigned(FVirtualLayerStream) Then FVirtualLayerStream.Position:=0;
end;

procedure TVirtualLayer.PrepareDestroy();
begin
  //Do nothing by default.
end;

destructor TVirtualLayer.Destroy();
var
  j: integer;
begin
  //Notify mounted layers to write anything they need to.
  for j := Length(FMountPoints)-1 downto 0 do begin
    TVirtualLayer(FMountPoints[j].MountedVirtual).PrepareDestroy();
  end;
  //Destroy mounted layers, they must also close related handles.
  for j := Length(FMountPoints)-1 downto 0 do begin
    TVirtualLayer(FMountPoints[j].MountedVirtual).Free;
  end;
  if FFreeLayerStreamOnFree Then begin
    if Assigned(FVirtualLayerStream) Then begin
      FVirtualLayerStream.Free;
    end;
  end;
  if not Assigned(FParentLayer) then begin
    DoneCriticalsection(m_Critical);
  end;
  inherited Destroy();
end;

end.

