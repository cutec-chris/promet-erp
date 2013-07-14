{
uvirtuallayer_ole.pas

Creates a virtual layer over a stream to access an Microsoft OLE document
using similar functionality as a regular file in a disk. It can create streams
and storages and, read and enumerate them.

Based in the TVirtualLayer class, so it can be stacked (mounted).

Status of operations:
* Enumerate streams and storages: Operative.
* Create stream:                  Currently only on "Root Folder".
* Create storage:                 Not working.
* Read stream:                    Operative.
* Delete stream:                  Not working.
* Delete storage:                 Not working.
* Attributes read:                Not working.
* Attributes write:               Not working.
* Regular streams:                Operative.
* Mini streams:                   Operative.
* Streams read/write concurrence: Not fully operative.
* Root directory coloring:        All are black.
* 32/64 bits:                     Not tested (mostly compliant).
* Multithreading:                 Not working (*1).
* Little endian platform:         Operative.
* Big endian platform:            Operative (tests needed).
* Create LE OLE files:            Operative.
* Create BE OLE files:            Not supported by default (use format func.).
* OLE bigger than 2 GB:           Not working.
* Known bugs:                     Streams multiple of sector size will have
                                  one sector allocated in excess.
* Whole status:                   Alpha (9 May 2009)

*1: Multithreading is not implemented by design in TVirtualLayer and its
    descendants will not be multithreading safe. This does not means that
    you can not use them in a Multithreading environment, but concurrent
    access to any layer from different threads at the same time will give
    unpredictable results. Basic blocking is provided by TVirtualLayer but
    this means that accesses will not be truly concurrent. Different layers
    will expose different stability when called from several threads at the
    same time, so in other words do not access virtual layers from different
    threads at the same time or the world could KBOOM! :)

OLE comments: OLE files, aka "Windows Compound Binary Format", presents some
              limitations. In version 3 they can not be bigger than 2 GB even
              when the storage format allows more than 2 TB. Sector size even
              when defined in the header it is tied to the "DllVersion" and not
              to the "FileVersion" which means version 3 = 512 bytes per sector
              and version 4 = 4096 bytes per sector. Version 4 should be able to
              read sectors of 512 bytes and 4096 but this point has not been
              tested as no version 4 real files to test has been found.

Related files:  uvirtuallayer_ole
                uvirtuallayer_ole_helpers
                uvirtuallayer_ole_types

AUTHORS: José Mejuto Porral
}
unit uvirtuallayer_ole;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  uvirtuallayer_types, uvirtuallayer,
  uvirtuallayer_ole_helpers, uvirtuallayer_ole_types;

type

  { TVirtualLayer_wincompound }

  TVirtualLayer_OLE=class(TVirtualLayer)
  private
    OpenedStreams: array of TWCBFOpenStream;
    FFATIndirect: TFATIndirect;

    procedure UseParameter(const Parameter);
    procedure SwapEndian_Record(var D: TWCBFStructuredStorageHeader);
    procedure SwapEndian_Record(var D: TWCBFStructuredStorageDirectoryEntry);
    Procedure NotImplemented();
    function ReadData(const Index: integer; const Buffer: PBYTE; const Size: int64): int64;
    function GetStorageFirstSID(const APath: UTF8String): SID;
    function GetStreamSID(const APath: UTF8String): SID;
    function FindFreeOpenFile(): integer;
    procedure DeleteSIDData(const ASID: SID);
    function CreateNewSID(const AType: etagSTGTY): SID;
    procedure InsertInDirectoryTree(const ASID,AMasterSID: SID);
  protected
    FDirectory: array of TWCBFStructuredStorageDirectoryEntry;

    function intfGetFreeSpace(const APath: UTF8String): int64; override;
    function intfIsWritableMedia(): Boolean; override;
    function intfFindList(const APath: UTF8String; const AMask: UTF8String): TVirtualLayer_FolderList; override;

    function intfOpenFile(const AFileName: UTF8String; const AMode: cardinal): TvlHandle; override;
    function intfCloseFile(const Handle: TvlHandle): Boolean; override;
    function intfRead(const Handle: TvlHandle; const Buffer: PBYTE; const Size: int64): int64; override;
    function intfSeek(const AHandle: TvlHandle; const APosition: int64; const Origin: word): int64; override;
    function intfGetFileSize(const AHandle: TvlHandle): int64; override;
    //Not implemented....
    function intfWrite(const Handle: TvlHandle; const Buffer: PBYTE; const Size: int64): int64; override;
    function intfSetFileSize(const AHandle: TvlHandle; const ANewFileSize: int64): Boolean; override;
    function intfDeleteFile(const AFileName: UTF8String): boolean; override;
    function intfMakeFolder(const AFolder: UTF8String): Boolean; override;
    function intfRemoveFolder(const AFolder: UTF8String): Boolean; override;
    //..Not implemented

  public
    procedure Format();
    function Initialize():boolean; override;
    procedure AfterConstruction; override;
    destructor Destroy(); override;
  end;

implementation

function HandleToIndex(const Handle: TvlHandle; out Index: integer): Boolean;
var
  X: PtrUInt;
begin
{$HINTS OFF}
  X:=PtrUInt(Handle);
{$HINTS ON}
  index:=X-1;
  if (Index<0) Then begin
    Result:=false;
  end else begin
    Result:=true;
  end;
end;

function IndexToHandle(const Index: integer): TvlHandle;
begin
{$HINTS OFF}
  Result:=TvlHandle(Index+1);
{$HINTS ON}
end;

{ TVirtualLayer_OLE }

{$HINTS OFF}
procedure TVirtualLayer_OLE.UseParameter(const Parameter);
begin
  //Do nothing
end;
{$HINTS ON}

procedure TVirtualLayer_OLE.SwapEndian_Record(var D: TWCBFStructuredStorageHeader);
var
  j: integer;
begin
{$IFDEF FPC}
  {$IFDEF FPC_LITTLE_ENDIAN}
  UseParameter(D);
  j:=0;
  UseParameter(j);
  {$ELSE}
  d._csectDif:=SwapEndian(d._csectDif);
  d._csectFat:=SwapEndian(d._csectFat);
  d._csectMiniFat:=SwapEndian(d._csectMiniFat);
  d._sectDifStart:=SwapEndian(d._sectDifStart);
  d._sectDirStart:=SwapEndian(d._sectDirStart);
  d._uMinorVersion:=SwapEndian(d._uMinorVersion);
  d._uDllVersion:=SwapEndian(d._uDllVersion);
  d._uByteOrder:=SwapEndian(d._uByteOrder);
  d._uSectorShift:=SwapEndian(d._uSectorShift);
  d._uMiniSectorShift:=SwapEndian(d._uMiniSectorShift);
  d._signature:=SwapEndian(d._signature);
  d._ulMiniSectorCutoff:=SwapEndian(d._ulMiniSectorCutoff);
  d._sectMiniFatStart:=SwapEndian(d._sectMiniFatStart);
  for j := 0 to 108 do begin
    d._sectFat[j]:=SwapEndian(d._sectFat[j]);
  end;
  {$ENDIF}
{$ENDIF}
end;

procedure TVirtualLayer_OLE.SwapEndian_Record(
  var D: TWCBFStructuredStorageDirectoryEntry);
var
  j: integer;
begin
{$IFDEF FPC}
  {$IFDEF FPC_LITTLE_ENDIAN}
  j:=0;
  UseParameter(D);
  UseParameter(j);
  {$ELSE}
  for j := 0 to 31 do begin
    d._ab[j]:=WChar(SwapEndian(WORD(d._ab[j])));
  end;

  d._time[0].dwHighDateTime:=SwapEndian(d._time[0].dwHighDateTime);
  d._time[0].dwLowDateTime :=SwapEndian(d._time[0].dwLowDateTime);
  d._time[1].dwHighDateTime:=SwapEndian(d._time[1].dwHighDateTime);
  d._time[1].dwLowDateTime :=SwapEndian(d._time[1].dwLowDateTime);

  d._cb           :=SwapEndian(d._cb);
  d._mse          :=SwapEndian(d._mse);
  d._bflags       :=SwapEndian(d._bflags);
  d._sidLeftSib   :=SwapEndian(d._sidLeftSib);
  d._sidRightSib  :=SwapEndian(d._sidRightSib);
  d._sidChild     :=SwapEndian(d._sidChild);
  d._dwUserFlags  :=SwapEndian(d._dwUserFlags);
  d._sectStart    :=SwapEndian(d._sectStart);
  d._ulSize       :=SwapEndian(d._ulSize);
  d._dptPropType  :=SwapEndian(d._dptPropType);
  {$ENDIF}
{$ENDIF}
end;

procedure TVirtualLayer_OLE.NotImplemented();
begin
  Raise Exception.Create('Not implemented or not possible to be done.');
end;

function TVirtualLayer_OLE.ReadData(const Index: integer;
  const Buffer: PBYTE; const Size: int64): int64;
begin
  Result:=FFATIndirect.ReadData(OpenedStreams[Index].Context,Buffer,Size);
end;

function TVirtualLayer_OLE.GetStorageFirstSID(const APath: UTF8String
  ): SID;
var
  Splitted: TStringList;
  j: integer;
  SIDChild: SID;
  function FindSiblingWithName(const AName: WideString; const AStartSibling: integer): SID;
  begin
    if (FDirectory[AStartSibling]._ab=AName) and (FDirectory[AStartSibling]._mse<>BYTE(STGTY_INVALID)) then begin
      Result:=FDirectory[AStartSibling]._sidChild;
    end else begin
      Result:=WINCOMPOUND_NOSID;
      if FDirectory[AStartSibling]._sidLeftSib<>WINCOMPOUND_NOSID then begin
        Result:=FindSiblingWithName(AName,FDirectory[AStartSibling]._sidLeftSib);
      end;
      if Result<>WINCOMPOUND_NOSID then exit;
      if FDirectory[AStartSibling]._sidRightSib<>WINCOMPOUND_NOSID then begin
        Result:=FindSiblingWithName(AName,FDirectory[AStartSibling]._sidRightSib);
      end;
    end;
  end;
begin
  Splitted:=TStringList.Create;
  SplitPath(APath,Splitted);
  if Length(FDirectory)<=1 then begin
    Splitted.Free;
    Result:=WINCOMPOUND_NOSID;
    Exit;
  end;
  SIDChild:=FDirectory[0]._sidChild;
  for j := 0 to Splitted.Count-1 do begin
    SIDChild:=FindSiblingWithName(UTF8Decode(Splitted[j]),SIDChild);
    if SIDChild=WINCOMPOUND_NOSID then break;
  end;
  Splitted.Free;
  Result:=SIDChild;
end;

function TVirtualLayer_OLE.GetStreamSID(const APath: UTF8String): SID;
var
  Splitted: TStringList;
  j: integer;
  SIDChild: SID;
  function FindSiblingWithName(const AName: WideString; const AStartSibling: integer): SID;
  begin
    if (FDirectory[AStartSibling]._ab=AName) and (FDirectory[AStartSibling]._mse<>BYTE(STGTY_INVALID)) then begin
      Result:=AStartSibling;
    end else begin
      Result:=WINCOMPOUND_NOSID;
      if FDirectory[AStartSibling]._sidLeftSib<>WINCOMPOUND_NOSID then begin
        Result:=FindSiblingWithName(AName,FDirectory[AStartSibling]._sidLeftSib);
      end;
      if Result<>WINCOMPOUND_NOSID then exit;
      if FDirectory[AStartSibling]._sidRightSib<>WINCOMPOUND_NOSID then begin
        Result:=FindSiblingWithName(AName,FDirectory[AStartSibling]._sidRightSib);
      end;
    end;
  end;
begin
  Splitted:=TStringList.Create;
  SplitPath(APath,Splitted);
  if Length(FDirectory)<=1 then begin
    Splitted.Free;
    Result:=WINCOMPOUND_NOSID;
    Exit;
  end;
  SIDChild:=0;
  for j := 0 to Splitted.Count-1 do begin
    SIDChild:=FDirectory[SIDChild]._sidChild;
    SIDChild:=FindSiblingWithName(UTF8Decode(Splitted[j]),SIDChild);
    if SIDChild=WINCOMPOUND_NOSID then break;
  end;
  Splitted.Free;
  Result:=SIDChild;
end;

function TVirtualLayer_OLE.intfFindList(const APath: UTF8String;
  const AMask: UTF8String): TVirtualLayer_FolderList;
var
  LI: TVirtualLayer_FolderList;
  Mask: TMaskFile;
  SSID: SID;
  function AddNamesWithSID(const AStartSibling: SID): SID;
  var
    Name: WideString;
    VI: TVirtualLayer_Item;
  begin
    Name:=UTF8Encode(wideString(FDirectory[AStartSibling]._ab));
    if Mask.Matches(Name) Then begin
      VI:=TVirtualLayer_Item.Create;
      VI.Name:=Name;
      if FDirectory[AStartSibling]._mse=integer(STGTY_STORAGE) then begin
        //It is a "folder"
        VI.IsFolder:=true;
      end else begin
        VI.Size:=FDirectory[AStartSibling]._ulSize;
        VI.IsFolder:=false;
      end;
      LI.Add(VI);
    end;
    Result:=WINCOMPOUND_NOSID;
    if FDirectory[AStartSibling]._sidLeftSib<>WINCOMPOUND_NOSID then begin
      Result:=AddNamesWithSID(FDirectory[AStartSibling]._sidLeftSib);
    end;
    if FDirectory[AStartSibling]._sidRightSib<>WINCOMPOUND_NOSID then begin
      Result:=AddNamesWithSID(FDirectory[AStartSibling]._sidRightSib);
    end;
  end;

begin
  //Find the storage SID for the path...
  SSID:=GetStorageFirstSID(APath);
  if SSID=WINCOMPOUND_NOSID then begin
    LI:=TVirtualLayer_FolderList.Create(APath);
    Result:=LI; //Empty items list
    Exit;
  end;

  Mask:=TMaskFile.Create(AMask);
  LI:=TVirtualLayer_FolderList.Create(APath);

  AddNamesWithSID(SSID);

  Mask.Free;
  Result:=Li;
end;

function TVirtualLayer_OLE.intfOpenFile(const AFileName: UTF8String;
  const AMode: cardinal): TvlHandle;
var
  SSID,ParentSID: SID;
  CurHandle: TWCBFOpenStream;
  Index: integer;
  Path,StreamName: UTF8String;
begin
  SSID:=GetStreamSID(AFileName);
  if SSID=WINCOMPOUND_NOSID then begin
    //Stream not found
    if AMode<>fmCreate then begin
      //It should not be created
      Result:=nil;
      Exit;
    end;
  end;
  if AMode=fmCreate then begin
    FFATIndirect.DirtyMedia:=true;
    if SSID<>WINCOMPOUND_NOSID then begin
      //File already exists, so clear all the FAT links and adjust size to cero
      DeleteSIDData(SSID);
      with FDirectory[SSID] do begin
        _sectStart:=SECT_ENDOFCHAIN;
        _ulSize:=0;
      end;
    end else begin
      //Create a new SID and link it to the tree...
      SplitFileNamePath(AFileName,Path,StreamName);
      SSID:=CreateNewSID(STGTY_STREAM);
      FDirectory[SSID]._ab:=UTF8Decode(StreamName);
      FDirectory[SSID]._cb:=(Length(UTF8Decode(StreamName))+1)*SizeOf(WChar);
      ParentSID:=GetStreamSID(Path);
      if FDirectory[ParentSID]._sidChild=WINCOMPOUND_NOSID then begin
        //This one is the first entry in this storage.
        FDirectory[ParentSID]._sidChild:=SSID;
      end else begin
        //There are already some entries in this storage, explore the tree
        //and insert the new SID in the right position.
        InsertInDirectoryTree(SSID,FDirectory[ParentSID]._sidChild);
      end;
    end;
  end;

  CurHandle.Handle:=SSID;
  CurHandle.Context:=FFATIndirect.OpenStream(FDirectory[SSID]._sectStart,FDirectory[SSID]._ulSize,AMode);
  Index:=FindFreeOpenFile();
  if Index=integer(feInvalidHandle) then begin
    Index:=Length(OpenedStreams);
    SetLength(OpenedStreams,Index+1);
  end;
  OpenedStreams[Index]:=CurHandle;

  Result:=IndexToHandle(Index);
end;

function TVirtualLayer_OLE.intfCloseFile(const Handle: TvlHandle
  ): Boolean;
var
  Index: integer;
begin
  if not HandleToIndex(Handle,Index) Then begin
    Result:=false;
  end else begin
    if Index>High(OpenedStreams) then begin
      Result:=false;
    end else begin
      FFATIndirect.CloseStream(OpenedStreams[Index].Context);
      FDirectory[OpenedStreams[Index].Handle]._sectStart:=OpenedStreams[Index].Context.FATFirstIndex;
      FDirectory[OpenedStreams[Index].Handle]._ulSize:=OpenedStreams[Index].Context.Size;
      OpenedStreams[Index].Handle:=WINCOMPOUND_NOSID;
      OpenedStreams[Index].Context.FATFirstIndex:=SECT_ENDOFCHAIN;
      OpenedStreams[Index].Context.FATIndex:=SECT_ENDOFCHAIN;
      OpenedStreams[Index].Context.Size:=0;
      OpenedStreams[Index].Context.Position:=0;
      Result:=true;
    end;
  end;
end;

function TVirtualLayer_OLE.intfSeek(const AHandle: TvlHandle;
  const APosition: int64; const Origin: word): int64;
var
  index: Integer;
begin
  if not HandleToIndex(AHandle,Index) Then begin
    Result:=-1;
    Exit;
  end;
  Result:=FFATIndirect.StreamSeekPosition(OpenedStreams[Index].Context,APosition,TSeekOrigin(Origin));
end;

function TVirtualLayer_OLE.intfRead(const Handle: TvlHandle;
  const Buffer: PBYTE; const Size: int64): int64;
var
  index: integer;
begin
  if not HandleToIndex(Handle,Index) then begin
    Result:=0;
    Exit;
  end;
  result:=FFATIndirect.ReadData(OpenedStreams[index].Context,Buffer,Size);
end;

function TVirtualLayer_OLE.intfWrite(const Handle: TvlHandle;
  const Buffer: PBYTE; const Size: int64): int64;
var
  index: integer;
begin
  if not HandleToIndex(Handle,Index) then begin
    Result:=0;
    Exit;
  end;
  result:=FFATIndirect.WriteData(OpenedStreams[index].Context,Buffer,Size);
end;

function TVirtualLayer_OLE.intfGetFileSize(const AHandle: TvlHandle
  ): int64;
var
  Index: integer;
begin
  if not HandleToIndex(AHandle,Index) then begin
    Result:=0;
    Exit;
  end;
  Result:=FDirectory[OpenedStreams[index].Handle]._ulSize;
end;

function TVirtualLayer_OLE.intfSetFileSize(const AHandle: TvlHandle;
  const ANewFileSize: int64): Boolean;
begin
  UseParameter(AHandle);UseParameter(ANewFileSize);
  NotImplemented();
  Result:=false;
end;

function TVirtualLayer_OLE.intfDeleteFile(const AFileName: UTF8String
  ): boolean;
begin
  UseParameter(AFileName);
  NotImplemented();
  Result:=false;
end;

function TVirtualLayer_OLE.intfMakeFolder(const AFolder: UTF8String
  ): Boolean;
begin
  UseParameter(AFolder);
  NotImplemented();
  Result:=false;
end;

function TVirtualLayer_OLE.intfRemoveFolder(const AFolder: UTF8String
  ): Boolean;
begin
  UseParameter(AFolder);
  NotImplemented();
  Result:=false;
end;

procedure TVirtualLayer_OLE.AfterConstruction;
begin
  inherited AfterConstruction;
  FFATIndirect:=TFATIndirect.Create(FVirtualLayerStream);
end;

procedure TVirtualLayer_OLE.Format();
begin
  FFATIndirect.Initialize(true);
  FFATIndirect.Free;
  FFATIndirect:=TFATIndirect.Create(FVirtualLayerStream);
  Self.Initialize();
end;

function TVirtualLayer_OLE.intfGetFreeSpace(const APath: UTF8String
  ): int64;
begin
  //This is a quite large operation, all FAT sectors must be
  //loaded and look for USED ones and discount them for the
  //maximun theorical which is:
  // Sectors=4294967280
  // SectorsForFATs=Sectors div FATEntriesPerSect + 109
  // SectorsInDIFs=SectorsForFATs div (FATEntriesPerSect-1)
  // .......
  // Too complex, so use the maximun theorical which is
  // Sectors * SectorSize which taken default settings is
  // 2,199,023,247,360 bytes or roughly around 2.2 TeraBytes.
  // but due usual desing flaws version 3 files are limited
  // to 2 GB and version 4 (4 Kb sectors) are limited to 512 GB.
  UseParameter(APath);
  Result:=0;
end;

function TVirtualLayer_OLE.intfIsWritableMedia(): Boolean;
begin
  Result:=true;
end;

function TVirtualLayer_OLE.FindFreeOpenFile(): integer;
var
  j: integer;
begin
  for j := 0 to High(OpenedStreams) do begin
    if OpenedStreams[j].Handle=WINCOMPOUND_NOSID Then begin
      Result:=j;
      Exit;
    end;
  end;
  Result:=integer(feInvalidHandle);
end;

procedure TVirtualLayer_OLE.DeleteSIDData(const ASID: SID);
begin
  If FFATIndirect.IsSizeInMiniFAT(FDirectory[ASID]._ulSize) then begin
    FFATIndirect.ResetMiniFATLinkage(FDirectory[ASID]._sectStart,SECT_FREESECT);
  end else begin
    FFATIndirect.ResetFATLinkage(FDirectory[ASID]._sectStart,SECT_FREESECT);
  end;
  //TODO: Resync handles to this SID.
end;

function TVirtualLayer_OLE.CreateNewSID(const AType: etagSTGTY): SID;
var
  j: SizeUint;
  procedure SetDefaults(var D: TWCBFStructuredStorageDirectoryEntry);
  begin
    FillByte(D,Sizeof(D),0);
    D._mse:=BYTE(AType);
    D._sectStart:=SECT_ENDOFCHAIN;
    D._bflags:=BYTE(DE_BLACK); //All are blacks in this implement.
    D._cb:=2; //NULL string
    D._sidChild:=WINCOMPOUND_NOSID;
    D._sidLeftSib:=WINCOMPOUND_NOSID;
    D._sidRightSib:=WINCOMPOUND_NOSID;
    //Ths other fields are zero.
  end;
begin
  for j := 0 to High(FDirectory) do begin
    if FDirectory[j]._mse=BYTE(STGTY_INVALID) then begin
      //Reuse this entry and blank it
      SetDefaults(FDirectory[j]);
      Result:=j;
      Exit;
    end;
  end;
  j:=Length(FDirectory);
  SetLength(FDirectory,j+1);
  SetDefaults(FDirectory[j]);
  Result:=j;
end;

procedure TVirtualLayer_OLE.InsertInDirectoryTree(const ASID,
  AMasterSID: SID);
begin
  if FDirectory[ASID]._ab > FDirectory[AMasterSID]._ab then begin
    if FDirectory[AMasterSID]._sidLeftSib=WINCOMPOUND_NOSID then begin
      FDirectory[AMasterSID]._sidLeftSib:=ASID;
    end else begin
      InsertInDirectoryTree(ASID,FDirectory[AMasterSID]._sidLeftSib);
    end;
  end else begin
    if FDirectory[AMasterSID]._sidRightSib=WINCOMPOUND_NOSID then begin
      FDirectory[AMasterSID]._sidLeftSib:=ASID;
    end else begin
      InsertInDirectoryTree(ASID,FDirectory[AMasterSID]._sidRightSib);
    end;
  end;
end;

function TVirtualLayer_OLE.Initialize(): boolean;
var
  Dir: TWCBFStructuredStorageDirectoryEntry;
  DirEntry: integer;
  EffectiveRead: SizeInt;
begin
  if not FFATIndirect.Initialize(false) then begin
    //Unable to initialize component.
    Result:=false;
    exit;
  end;
  
  SetLength(OpenedStreams,1);
  OpenedStreams[0].Handle:=0;
  OpenedStreams[0].Context:=FFATIndirect.DirectoryContext;
  while true do begin
    EffectiveRead:=FFATIndirect.ReadData(FFATIndirect.DirectoryContext,@Dir,Sizeof(Dir));
    SwapEndian_Record(Dir);
    if EffectiveRead=Sizeof(Dir) Then begin
      if Dir._cb>0 then begin
        //Load all, as even "deleted" entries must be preserved as the
        //SID (index in dir) is constant for all the file life (of course
        //they can be renumbered, but better do it in the save process).
        DirEntry:=Length(FDirectory);
        SetLength(FDirectory,DirEntry+1);
        FDirectory[DirEntry]:=Dir;
      end else begin
        //Empty name means end of dir.
        Break;
      end;
    end else begin
      break;
    end;
  end;
  if Length(FDirectory)>0 then Result:=true else Result:=false;
end;

destructor TVirtualLayer_OLE.Destroy();
var
  j: SizeUint;
  EmptyDir: TWCBFStructuredStorageDirectoryEntry;
begin
  if FFATIndirect.DirtyMedia Then begin
    //Update Root entry values
    FDirectory[0]._sectStart:=FFATIndirect.MiniFATDataContext.FATFirstIndex;
    FDirectory[0]._ulSize:=FFATIndirect.MiniFATDataContext.Size;
    FFATIndirect.StreamSeekPosition(FFATIndirect.DirectoryContext,0,soBeginning);
    for j := 0 to High(FDirectory) do begin
      FFATIndirect.WriteData(FFATIndirect.DirectoryContext,@FDirectory[j],sizeof(FDirectory[j]));
    end;
    EmptyDir._sidRightSib:=0; //Avoid uninitialize hint.
    FillByte(EmptyDir,sizeof(EmptyDir),0);
    FFATIndirect.WriteData(FFATIndirect.DirectoryContext,@EmptyDir,sizeof(EmptyDir));
  end;
  FreeAndNIL(FFATIndirect);
  inherited Destroy();
end;

end.

