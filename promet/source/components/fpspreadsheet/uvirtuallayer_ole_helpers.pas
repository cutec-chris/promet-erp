{
uvirtuallayer_ole_helpers.pas

Part of "uvirtuallayer_ole".

Supports most of the logic of Indirect FAT and Double Indirect FAT accesses for
OLE files.

Presented as an unit to hide the class and definitions when using OLE virtual
layer. This class is not intented to general use, only by TVirtualLayer_OLE.

AUTHORS: José Mejuto Porral
}

unit uvirtuallayer_ole_helpers;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uvirtuallayer_ole_types;

type
  TFATCacheItem=record
    Sector: SECT;
    Cache: PSECT;
    Dirty: Boolean;
  end;

  TFATStreamContext=record
    FATFirstIndex: SizeUint;
    FATIndex: SizeUint;
    Position: int64;
    Size: int64;
    Dirty: Boolean;
    AccessMode: WORD;
    MiniStream: TMemoryStream;
  end;

  TWCBFOpenStream=record
    Handle: SID;
    Context: TFATStreamContext;
  end;

{ TMaskFile }

TMaskFile=class
private
protected
  FMask: UTF8String;
public
  function Matches(const AFileName: UTF8String): Boolean;
  Constructor Create(const AMask: UTF8String);
end;

{ TFATIndirect }

TFATIndirect=class
private
  procedure InitializeMiniDataStream();
protected
  FDirtyMedia: Boolean;
  FFATCache: TFATCacheItem;
  FMiscSectorBuffer: PBYTE;
  FStream: TStream;
  FHeader: TWCBFStructuredStorageHeader;
  FSectorSize: SizeUint;
  FEntriesShort: SizeUint;
  FDIFArray: array of PSECT;
  FFATEntriesPerSect: SizeUint;
  FNeedEndianChange: Boolean;
  FHeaderLength: SizeUint;
  procedure FlushCaches();
  function iLEToN(const AValue: DWORD): DWORD;
  function iNToLE(const AValue: DWORD): DWORD;
  procedure iDIFFATLEToN(var ADIF: PSECT);
  procedure iDIFFATNToLE(var ADIF: PSECT);
  procedure iHeaderLEToN(var AHeader: TWCBFStructuredStorageHeader);
  procedure iHeaderNToLE(var AHeader: TWCBFStructuredStorageHeader);
  function ComposeFATEntry(const ADIF,AFATInDIF,AOffSetInFAT: SECT): SECT; inline;
  procedure DecomposeFATEntry(const AFATEntry: SECT; out DIF,FATInDIF,OffsetInFAT: SECT); inline;
  function WriteToMiniFAT(var AContext: TFATStreamContext; const AStream: TStream): DWORD;
  function ReadFromMiniFAT(const AContext: TFATStreamContext; const AStream: TStream): DWORD;
  procedure ReadSector(const ASector: SECT; const ABuffer: Pointer); inline;
  procedure WriteSector(const ASector: SECT; const ABuffer: Pointer); inline;
  procedure ReadFATSector(const ASector: SECT);
  procedure WriteFATSector(const ASector: SECT;const ABuffer: PSECT); overload;
  procedure WriteFATSector(const ASector: SECT); overload;
  function IsReservedValue(const ASectorValue: SECT): Boolean;inline;
  function LoadDIFArray(): Boolean;
  function UnloadDIFArray(): Boolean;
  function SectorAbsolute(const ASector: SECT): int64; inline;
  function FindFreeFAT(const ABuffer: PSECT; const AStartIndex: SizeUint): SizeUint;
  function AllocateNewSector(const AStartingFATIndex: SECT; const ALinkFrom: SECT;const ALinkTo: SECT=SECT_ENDOFCHAIN): SizeUInt;
  function GetStreamSizeInSectors(const AContext: TFATStreamContext): int64;
  function GetFATOffsetForPosition(const APosition: int64): SizeUint; inline;
  function GetFATRemainForPosition(const APosition: int64): SizeUint; inline;
  function ReadFATEntryValue(const AFATEntryIndex: SizeUint): SECT; inline;
  procedure WriteFATEntryValue(const AFATEntryIndex: SizeUint;const ALinkTo: SECT); inline;
  procedure ResynchronizePosition(var AContext: TFATStreamContext);
  procedure ResynchronizePositionWithAllocation(var AContext: TFATStreamContext);
  function FollowFATLinkage(const AStartFATEntryIndex: SizeUint; const ABytes: int64): SECT;
  procedure Format(var AHeader: TWCBFStructuredStorageHeader); virtual;
public
  DirectoryContext: TFATStreamContext;
  MiniFATContext: TFATStreamContext;
  MiniFATDataContext: TFATStreamContext;
  property DirtyMedia: Boolean read FDirtyMedia write FDirtyMedia;
  property HeaderLength: SizeUint read FHeaderLength;
  function OpenStream(const AFirstFATSector: DWORD; const ASize: int64; const AAccessMode: WORD): TFATStreamContext;
  procedure CloseStream(var AContext: TFATStreamContext);
  function ReadData(var AContext: TFATStreamContext; const ABuffer: PBYTE; const ASize: SizeInt): SizeInt;
  function WriteData(var AContext: TFATStreamContext; const ABuffer: PBYTE; const ASize: SizeInt): SizeInt;
  function StreamSeekPosition(var AContext: TFATStreamContext; const AOffset: int64; const AOrigin: TSeekOrigin): int64;
  procedure ResetMiniFATLinkage(const AStartFATEntryIndex: SizeUint; const ANewFATValue: SECT);
  procedure ResetFATLinkage(const AStartFATEntryIndex: SizeUint; const ANewFATValue: SECT);
  function IsSizeInMiniFAT(const ASize: int64): Boolean;

  function Initialize(const ACreate: Boolean=false): Boolean;
  Constructor Create(const AOLEStream: TStream);
  Destructor Destroy; override;
end;

implementation

function MatchesMask(What, Mask: string): boolean; forward;

procedure TFATIndirect.InitializeMiniDataStream();
var
  RootDir: TWCBFStructuredStorageDirectoryEntry;
begin
  //Read the SID 0, RootEntry, or R or whichever name it uses.
  RootDir._cb:=0;
  FillByte(RootDir,sizeof(RootDir),0);
  StreamSeekPosition(DirectoryContext,0,soBeginning);
  ReadData(DirectoryContext,@RootDir,SizeOf(RootDir));
  StreamSeekPosition(DirectoryContext,0,soBeginning);
  MiniFATDataContext.FATFirstIndex:=RootDir._sectStart;
  MiniFATDataContext.FATIndex:=RootDir._sectStart;
  MiniFATDataContext.Size:=RootDir._ulSize;
  MiniFATDataContext.MiniStream:=nil;
end;

procedure TFATIndirect.FlushCaches();
begin
  if FFATCache.Dirty then begin
    WriteSector(FFATCache.Sector,FFATCache.Cache);
    FFATCache.Dirty:=false;
  end;
end;

function TFATIndirect.iLEToN(const AValue: DWORD): DWORD;
begin
  if FNeedEndianChange then begin
    Result:=LEToN(AValue);
  end else begin
    Result:=AValue;
  end;
end;

function TFATIndirect.iNToLE(const AValue: DWORD): DWORD;
begin
  if FNeedEndianChange then begin
    Result:=NToLE(AValue);
  end else begin
    Result:=AValue;
  end;
end;

procedure TFATIndirect.iDIFFATLEToN(var ADIF: PSECT);
var
  j: SizeUint;
begin
  if not FNeedEndianChange then Exit;
  for j := 0 to FFATEntriesPerSect-1 do begin
    ADIF[j]:=LEToN(ADIF[j]);
  end;
end;

procedure TFATIndirect.iDIFFATNToLE(var ADIF: PSECT);
var
  j: SizeUint;
begin
  if not FNeedEndianChange then Exit;
  for j := 0 to FFATEntriesPerSect-1 do begin
    ADIF[j]:=NToLE(ADIF[j]);
  end;
end;

procedure TFATIndirect.iHeaderLEToN(var AHeader: TWCBFStructuredStorageHeader);
var
  j: SizeUint;
begin
  if Not FNeedEndianChange then exit;
  AHeader._uMinorVersion:=iLEToN(FHeader._uMinorVersion);
  AHeader._uDllVersion:=iLEToN(FHeader._uDllVersion);
  AHeader._uSectorShift:=iLEToN(FHeader._uSectorShift);
  AHeader._uMiniSectorShift:=iLEToN(FHeader._uMiniSectorShift);
  AHeader._csectFat:=iLEToN(FHeader._csectFat);
  AHeader._sectDirStart:=iLEToN(FHeader._sectDirStart);
  AHeader._ulMiniSectorCutoff:=iLEToN(FHeader._ulMiniSectorCutoff);
  AHeader._sectMiniFatStart:=iLEToN(FHeader._sectMiniFatStart);
  AHeader._csectMiniFat:=iLEToN(FHeader._csectMiniFat);
  AHeader._sectDifStart:=iLEToN(FHeader._sectDifStart);
  AHeader._csectDif:=iLEToN(FHeader._csectDif);
  for j := Low(AHeader._sectFat) to High(AHeader._sectFat) do begin
    AHeader._sectFat[j]:=iLEToN(AHeader._sectFat[j]);
  end;
end;

procedure TFATIndirect.iHeaderNToLE(var AHeader: TWCBFStructuredStorageHeader);
var
  j: SizeUint;
begin
  if Not FNeedEndianChange then exit;
  AHeader._uMinorVersion:=iNToLE(FHeader._uMinorVersion);
  AHeader._uDllVersion:=iNToLE(FHeader._uDllVersion);
  AHeader._uSectorShift:=iNToLE(FHeader._uSectorShift);
  AHeader._uMiniSectorShift:=iNToLE(FHeader._uMiniSectorShift);
  AHeader._csectFat:=iNToLE(FHeader._csectFat);
  AHeader._sectDirStart:=iNToLE(FHeader._sectDirStart);
  AHeader._ulMiniSectorCutoff:=iNToLE(FHeader._ulMiniSectorCutoff);
  AHeader._sectMiniFatStart:=iNToLE(FHeader._sectMiniFatStart);
  AHeader._csectMiniFat:=iNToLE(FHeader._csectMiniFat);
  AHeader._sectDifStart:=iNToLE(FHeader._sectDifStart);
  AHeader._csectDif:=iNToLE(FHeader._csectDif);
  for j := Low(AHeader._sectFat) to High(AHeader._sectFat) do begin
    AHeader._sectFat[j]:=iNToLE(AHeader._sectFat[j]);
  end;
end;

function TFATIndirect.ComposeFATEntry(const ADIF, AFATInDIF, AOffSetInFAT: SECT
  ): SECT;
begin
  if ADIF=0 then begin
    Result:=AFATInDIF*FFATEntriesPerSect+AOffSetInFAT;
  end else begin
    Result:=FEntriesShort+
            int64((ADIF-1))*((FFATEntriesPerSect-1)*FFATEntriesPerSect)+
            int64(AFATInDIF)*FFATEntriesPerSect+
            AOffSetInFAT;
  end;
end;

procedure TFATIndirect.DecomposeFATEntry(const AFATEntry: SECT; out DIF,
  FATInDIF, OffsetInFAT: SECT);
var
  TmpEntry: SECT;
begin
  if AFATEntry<FEntriesShort then begin
    DIF:=0;
    FATInDIF:=AFATEntry div FFATEntriesPerSect;
    OffsetInFAT:=AFATEntry mod FFATEntriesPerSect;
  end else begin
    TmpEntry:=AFATEntry-FEntriesShort;
    DIF:=TmpEntry div ((FFATEntriesPerSect-1)*FFATEntriesPerSect);
    TmpEntry:=TmpEntry-DIF*((FFATEntriesPerSect-1)*FFATEntriesPerSect);
    FATInDIF:=TmpEntry div FFATEntriesPerSect;
    OffsetInFAT:=TmpEntry mod FFATEntriesPerSect;
    DIF:=DIF+1; //+1 the removed by substract FEntriesShort
  end;
end;

procedure TFATIndirect.ResetMiniFATLinkage(const AStartFATEntryIndex: SizeUint;
  const ANewFATValue: SECT);
var
  NextFAT: SECT;
  MiniFATBuffer: PSECT;
  Index,IndexLimit: SECT;
begin
  MiniFATBuffer:=nil;
  GetMem(MiniFATBuffer,MiniFATContext.Size);
  MiniFATContext.Position:=0;
  ReadData(MiniFATContext,PBYTE(MiniFATBuffer),MiniFATContext.Size);
  Index:=0;
  IndexLimit:=MiniFATContext.Size div BYTES_PER_FAT_ENTRY;
  NextFAT:=AStartFATEntryIndex;
  while not IsReservedValue(NextFAT) do begin
    Index:=MiniFATBuffer[NextFAT];
    if Index>=IndexLimit Then begin
      Raise Exception.Create('MiniFAT internal structure damaged');
    end;
    MiniFATBuffer[NextFAT]:=ANewFATValue;
    NextFAT:=Index;
  end;
  StreamSeekPosition(MiniFATContext,0,soBeginning);
  WriteData(MiniFATContext,PBYTE(MiniFATBuffer),IndexLimit*BYTES_PER_FAT_ENTRY);
  Freemem(MiniFATBuffer);
end;

function TFATIndirect.WriteToMiniFAT(var AContext: TFATStreamContext;
  const AStream: TStream): DWORD;
var
  MiniSectorSize: DWORD;
  MiniEntries: DWORD;
  FATArray: array of SECT;
  k: SizeUint;
  OldPos: int64;
  function FindFreeMiniFATArray(const AEntries: DWORD): SECT;
  var
    Pending: DWORD;
    MiniFATBuffer: PSECT;
    Index,IndexLimit: SECT;
    FATEntries: SizeUint;
    j: integer;
  begin
    Pending:=AEntries;
    SetLength(FATArray,0);
    MiniFATBuffer:=nil;
    GetMem(MiniFATBuffer,MiniFATContext.Size);
    MiniFATContext.Position:=0;
    ReadData(MiniFATContext,PBYTE(MiniFATBuffer),MiniFATContext.Size);
    FATEntries:=0;
    Index:=0;
    IndexLimit:=MiniFATContext.Size div BYTES_PER_FAT_ENTRY;
    while Pending>0 do begin
      while (Pending>0) and (Index<IndexLimit) do begin
        if MiniFATBuffer[Index]=SECT_FREESECT then begin
          SetLength(FATArray,FATEntries+1);
          FATArray[FATEntries]:=Index;
          if Index>=FHeader._csectMiniFat then begin
            FHeader._csectMiniFat:=Index+1;
          end;
          inc(FATEntries);
          dec(Pending);
        end;
        Inc(Index);
      end;
      if (Pending>0) then begin
        //Allocate a new FAT sector...
        MiniFATBuffer:=ReAllocMem(MiniFATBuffer,(IndexLimit+FFATEntriesPerSect)*BYTES_PER_FAT_ENTRY);
        IndexLimit:=IndexLimit+FFATEntriesPerSect;
        for j := Index to IndexLimit-1 do begin
          MiniFATBuffer[j]:=SECT_FREESECT;
        end;
      end;
    end;
    //Link the sectors....
    for j := 0 to High(FATArray)-1 do begin
      MiniFATBuffer[FATArray[j]]:=FATArray[j+1];
    end;
    //And mark the end
    MiniFATBuffer[FATArray[High(FATArray)]]:=SECT_ENDOFCHAIN;
    StreamSeekPosition(MiniFATContext,0,soBeginning);
    WriteData(MiniFATContext,PBYTE(MiniFATBuffer),IndexLimit*BYTES_PER_FAT_ENTRY);
    Freemem(MiniFATBuffer);
    Result:=FATArray[0];
  end;
begin
  if AStream.Size=0 then begin
    Result:=0;
    Exit;
  end;
  MiniSectorSize:= 1 shl FHeader._uMiniSectorShift;
  OldPos:=AStream.Position;
  AStream.Position:=0;
  MiniEntries:=AContext.Size div MiniSectorSize;
  if AContext.Size mod MiniSectorSize>0 then begin
    inc(MiniEntries);
  end;
  FindFreeMiniFATArray(MiniEntries);
  for k := 0 to MiniEntries-1 do begin
    StreamSeekPosition(MiniFATDataContext,int64(FATArray[k])*MiniSectorSize,soBeginning);
    AStream.Read(FMiscSectorBuffer^,MiniSectorSize);
    WriteData(MiniFATDataContext,FMiscSectorBuffer,MiniSectorSize);
  end;
  AContext.FATFirstIndex:=FATArray[0];
  AContext.FATIndex:=SECT_FREESECT;
  AStream.Position:=OldPos;
end;

function TFATIndirect.ReadFromMiniFAT(const AContext: TFATStreamContext;
  const AStream: TStream): DWORD;
var
  NextSector: SECT;
  MiniSectorSize: DWORD;
  ReadBytes,ThisRead: SizeUInt;
  EffectiveRead: SizeUint;
begin
  If AContext.Size=0 then begin
    Result:=0;
    Exit;
  end;
  MiniSectorSize:= 1 shl FHeader._uMiniSectorShift;
  AStream.Position:=0;
  AStream.Size:=0;
  ReadBytes:=AContext.Size;
  NextSector:=AContext.FATFirstIndex;
  while not IsReservedValue(NextSector) and (ReadBytes>0) do begin
    StreamSeekPosition(MiniFATDataContext,Int64(NextSector)*MiniSectorSize,soBeginning);
    if ReadBytes>MiniSectorSize then begin
      ThisRead:=MiniSectorSize;
    end else begin
      ThisRead:=ReadBytes;
    end;
    EffectiveRead:=ReadData(MiniFATDataContext,FMiscSectorBuffer,ThisRead);
    AStream.Write(FMiscSectorBuffer^,EffectiveRead);
    StreamSeekPosition(MiniFATContext,int64(NextSector)*BYTES_PER_FAT_ENTRY,soBeginning);
    ReadData(MiniFATContext,@NextSector,BYTES_PER_FAT_ENTRY);
    Dec(ReadBytes,EffectiveRead);
    if ThisRead<>EffectiveRead then exit;
  end;
  AStream.Size:=AContext.Size-ReadBytes;
  Result:=AStream.Size;
end;

procedure TFATIndirect.ReadSector(const ASector: SECT; const ABuffer: Pointer);
begin
  FStream.Position:=int64(ASector) * FSectorSize + FHeaderLength;
  FStream.ReadBuffer(PBYTE(ABuffer)^,FSectorSize);
end;

procedure TFATIndirect.WriteSector(const ASector: SECT; const ABuffer: Pointer);
begin
  FStream.Position:=int64(ASector) * FSectorSize + FHeaderLength;
  FStream.WriteBuffer(PBYTE(ABuffer)^,FSectorSize);
  FDirtyMedia:=true;
end;

procedure TFATIndirect.ReadFATSector(const ASector: SECT);
begin
  if ASector=FFATCache.Sector then exit;
  if (FFATCache.Dirty) Then begin
    //Write the dirty cached FAT sector
    iDIFFATNToLE(FFATCache.Cache);
    WriteSector(FFATCache.Sector,FFATCache.Cache);
  end;
  FFATCache.Sector:=ASector;
  FFATCache.Dirty:=false;
  ReadSector(ASector,FFATCache.Cache);
  iDIFFATLEToN(FFATCache.Cache);
end;

procedure TFATIndirect.WriteFATSector(const ASector: SECT; const ABuffer: PSECT
  );
begin
  if ASector<>FFATCache.Sector then begin
    if FFATCache.Dirty then begin
      //Write the dirty cached FAT sector
      if FHeader._csectFat<(FFATCache.Sector div FFATEntriesPerSect)+1 then begin
        FHeader._csectFat:=(FFATCache.Sector div FFATEntriesPerSect)+1;
      end;
      iDIFFATNToLE(FFATCache.Cache);
      WriteSector(FFATCache.Sector,FFATCache.Cache);
    end;
  end;
  move(PBYTE(ABuffer)^,PBYTE(FFATCache.Cache)^,FSectorSize);
  FFATCache.Sector:=ASector;
  FFATCache.Dirty:=true;
end;

procedure TFATIndirect.WriteFATSector(const ASector: SECT);
begin
  if ASector=FFATCache.Sector then begin
    FFATCache.Dirty:=true;
  end else begin
    if FHeader._csectFat<(FFATCache.Sector div FFATEntriesPerSect)+1 then begin
      FHeader._csectFat:=(FFATCache.Sector div FFATEntriesPerSect)+1;
    end;
    iDIFFATNToLE(FFATCache.Cache);
    //Writes the data
    WriteSector(FFATCache.Sector,FFATCache.Cache);
    FFATCache.Dirty:=false;
  end;
end;

function TFATIndirect.IsReservedValue(const ASectorValue: SECT): Boolean;
  inline;
begin
  if ASectorValue>=$FFFFFFF0 then
    Result:=true
  else
    Result:=false;
end;

function TFATIndirect.LoadDIFArray(): Boolean;
var
  NextDIF: SECT;
  Index: SizeUint;
begin
  SetLength(FDIFArray,1);
  //The first DIF is an special one...
  FDIFArray[0]:=PSECT(@FHeader._sectFat[0]);
  NextDIF:=FHeader._sectDifStart;
  Index:=0;
  while not IsReservedValue(NextDIF) do begin
    Inc(Index);
    SetLength(FDIFArray,Index+1);
    GetMem(FDIFArray[Index],FSectorSize);
    ReadSector(NextDIF,FDIFArray[Index]);
    iDIFFATLEToN(FDIFArray[Index]);
    NextDIF:=FDIFArray[Index][FFATEntriesPerSect-1];
  end;
  Result:=true;
end;

function TFATIndirect.UnloadDIFArray(): Boolean;
var
  NextDIF: SECT;
  NextDIFC: SECT;
  Index: SizeUint;
begin
  if not Assigned(FDIFArray) then begin
    //Nothing to be freed.
    Result:=true;
    exit;
  end;
  //The first DIF is an special one and do not need to be
  //written as it is written with the header updates.
  FDIFArray[0]:=PSECT(@FHeader._sectFat[0]);
  //It is in memory in machine endian format.
  NextDIF:=FHeader._sectDifStart;
  Index:=0;
  while not IsReservedValue(NextDIF) do begin
    Inc(Index);
    NextDIFC:=FDIFArray[Index][FFATEntriesPerSect-1];
    if FDirtyMedia then begin
      iDIFFATNToLE(FDIFArray[Index]);
      WriteSector(NextDIF,FDIFArray[Index]);
    end;
    FreeMem(FDIFArray[Index]);
    NextDIF:=NextDIFC;
  end;
  SetLength(FDIFArray,0);
  Result:=true;
end;

function TFATIndirect.SectorAbsolute(const ASector: SECT): int64;
begin
  Result:=int64(ASector)*FSectorSize+FHeaderLength;
end;

function TFATIndirect.FindFreeFAT(const ABuffer: PSECT; const AStartIndex: SizeUint): SizeUInt;
var
  j: SizeUint;
begin
  Result:=SECT_FREESECT;
  for j := AStartIndex to FFATEntriesPerSect-1 do begin
    if ABuffer[j]=SECT_FREESECT then begin
      Result:=j;
      break;
    end;
  end;
end;

function TFATIndirect.AllocateNewSector(const AStartingFATIndex: SECT;
  const ALinkFrom: SECT; const ALinkTo: SECT): SizeUInt;
var
  j: SizeUint;
  Sector: SECT;
  DIFEntries: SECT;
  DIFIndex,FATInDIF,OffsetInFAT: SECT;
  Index: SizeUint;
  DIFData: PSECT;
  procedure CreateEmptyFATSector(const ASector: SECT);
  var
    Buffer: PBYTE;
  begin
    inc(FHeader._csectFat); //increment the FAT sector counter
    Buffer:=nil;
    GetMem(Buffer,FSectorSize);
    FillByte(Buffer^,FSectorSize,$FF);
    WriteFATSector(ASector,PSECT(Buffer));
    FreeMem(Buffer);
  end;
begin
  //DIF sectors points to FAT sectors where the allocation happends.
  //The first DIF sector is an special one with 109 entries (fixed),
  //without next DIF sector link and it is allocated in the header.
  //------------
  DecomposeFATEntry(AStartingFATIndex+1,DIFIndex,FATInDIF,OffsetInFAT);
  if (AStartingFATIndex>0) and (SizeInt(DIFIndex)<=High(FDIFArray)) then begin
    //Due speed reasons, check next sector and all the sectors
    //in the same FAT, this improves linear write & read.
    //DIF is the special header one ?
    if DIFIndex=0 then begin
      DIFEntries:=0;
    end else begin
      DIFEntries:=FSectorSize div BYTES_PER_FAT_ENTRY;
    end;
    DIFData:=FDIFArray[DIFIndex];
    for j := 0 to 0 do begin
      Sector:=DIFData[FATInDIF];
      if Sector=SECT_FREESECT then begin
        //Create a new FAT sector just here
        Sector:=ComposeFATEntry(DIFIndex,FATInDIF,0);
        CreateEmptyFATSector(Sector);
        DIFData[FATInDIF]:=Sector;
        WriteFATEntryValue(Sector,SECT_FATSECT);
      end;
      ReadFATSector(Sector);
      if FFATCache.Cache[OffsetInFAT]=SECT_FREESECT then begin
        //Found sector as Free sector...
        FFATCache.Cache[OffsetInFAT]:=ALinkTo;
        WriteFATSector(Sector);
        Result:=ComposeFATEntry(DIFIndex,FATInDIF,OffsetInFAT);
      end else begin
        //Not found, so try the complete FAT sector
        Index:=FindFreeFAT(FFATCache.Cache,0);
        if Index<>SECT_FREESECT then begin
          //Found sector as free sector
          FFATCache.Cache[Index]:=ALinkTo;
          WriteFATSector(Sector);
          Result:=ComposeFATEntry(DIFIndex,FATInDIF,Index);
        end else begin
          Result:=SECT_FREESECT;
        end;
      end;
      if Result<>SECT_FREESECT then begin
        //Write the changes
        if ALinkFrom<>SECT_FREESECT then begin
          WriteFATEntryValue(ALinkFrom,Result);
        end;
      end else begin
        Result:=AllocateNewSector(0,ALinkFrom,ALinkTo);
      end;
    end;
    if Result<>SECT_FREESECT then exit;
  end;
  for DIFIndex := 0 to High(FDIFArray) do begin
    if DIFIndex=0 then begin
      DIFEntries:=Length(FHeader._sectFat);
    end else begin
      DIFEntries:=(FSectorSize div BYTES_PER_FAT_ENTRY)-1;
    end;
    FATInDIF:=0;
    OffsetInFAT:=0;
    DIFData:=FDIFArray[DIFIndex];
    for FATInDIF := 0 to DIFEntries-1 do begin
      Sector:=DIFData[FATInDIF];
      if Sector=SECT_FREESECT then begin
        //Create a new FAT sector just here
        Sector:=ComposeFATEntry(DIFIndex,FATInDIF,0);
        CreateEmptyFATSector(Sector);
        DIFData[FATInDIF]:=Sector;
        WriteFATEntryValue(Sector,SECT_FATSECT);
      end;
      ReadFATSector(Sector);
      for OffsetInFAT := 0 to FFATEntriesPerSect-1 do begin
        if FFATCache.Cache[OffsetInFAT]=SECT_FREESECT then begin
          //Found sector as Free sector...
          FFATCache.Cache[OffsetInFAT]:=ALinkTo;
          WriteFATSector(Sector);
          Result:=ComposeFATEntry(DIFIndex,FATInDIF,OffsetInFAT);
          if Result<>SECT_FREESECT then begin
            //Write the changes
            if ALinkFrom<>SECT_FREESECT then begin
              WriteFATEntryValue(ALinkFrom,Result);
            end;
            Exit;
          end;
        end;
      end;
    end;
  end;
  //There is no empty space in current FAT so, add a new DIF
  DIFIndex:=High(FDIFArray)+1;
  FATInDIF:=0;
  OffsetInFAT:=0;
  Sector:=ComposeFATEntry(DIFIndex,FATInDIF,OffsetInFAT);
  //Empty free sector
  GetMem(DIFData,FSectorSize); //This memblock will be freed by UnloadDIF
  FillByte(PBYTE(DIFData)^,FSectorSize,$FF);
  //With ENDOFCHAIN marker
  DIFData[FFATEntriesPerSect-1]:=SECT_ENDOFCHAIN;
  //Link in the previous DIF sector
  FDIFArray[High(FDIFArray)][FFATEntriesPerSect-1]:=Sector;
  //Update memory block
  SetLength(FDIFArray,DIFIndex+1);
  FDIFArray[DIFIndex]:=DIFData;
  FDirtyMedia:=true; //Forces a rewrite of DIF sectors when destroying the class.
  inc(FHeader._csectDif); //Increments the DIF counter.
  //Create a new FAT sector just here
  CreateEmptyFATSector(Sector+1);
  DIFData[FATInDIF]:=Sector+1;
  WriteFATEntryValue(Sector,SECT_DIFSECT);
  WriteFATEntryValue(Sector+1,SECT_FATSECT);
  Result:=AllocateNewSector(Sector+1,ALinkFrom,ALinkTo);
end;

function TFATIndirect.GetStreamSizeInSectors(const AContext: TFATStreamContext
  ): int64;
var
  Sector,LastSector: SECT;
begin
  Result:=0;
  LastSector:=AContext.FATFirstIndex;
  while (LastSector<$FFFFFFF0) do begin
    Sector:=LastSector;
    LastSector:=ReadFATEntryValue(Sector);
    inc(Result);
  end;
end;

function TFATIndirect.GetFATOffsetForPosition(const APosition: int64
  ): SizeUint;
begin
  Result:=APosition mod FSectorSize;
end;

function TFATIndirect.GetFATRemainForPosition(const APosition: int64
  ): SizeUint;
begin
  Result:=FSectorSize -(APosition mod FSectorSize);
end;

function TFATIndirect.ReadData(var AContext: TFATStreamContext;
  const ABuffer: PBYTE; const ASize: SizeInt): SizeInt;
var
  Position: int64;
  FAT,NextFAT: SECT;
  CanRead: SizeUint;
  RemainData: int64;
  TargetBuffer: PBYTE;
  EffectiveRead: int64;
begin
  if ASize<1 then begin
    Result:=0;
    Exit;
  end;
  if AContext.AccessMode=fmOpenWrite then begin
    if (@AContext<>@DirectoryContext) and
      (@AContext<>@MiniFATContext) and
      (@AContext<>@MiniFATDataContext) then begin
      Raise EStreamError.Create('Stream can not be read, open for write only');
    end;
  end;
  if AContext.Position>=AContext.Size Then begin
    //EOF condition, can not read nothing.
    Result:=0;
    Exit;
  end;
  if Assigned(AContext.MiniStream) then begin
    if AContext.Size<FHeader._ulMiniSectorCutoff then begin
      //Read it from MiniFAT
      AContext.MiniStream.Position:=AContext.Position;
      Result:=AContext.MiniStream.Read(ABuffer^,ASize);
      AContext.Position:=AContext.MiniStream.Position;
      Exit;
    end;
  end;
  if AContext.FATIndex=SECT_FREESECT then begin
    ResynchronizePosition(AContext);
  end;
  If AContext.FATIndex=SECT_FREESECT then begin
    Result:=0;
    Exit;
  end;
  Position:=AContext.Position;
  if Position+ASize>AContext.Size then begin
    RemainData:=AContext.Size-Position;
  end else begin
    RemainData:=ASize;
  end;
  TargetBuffer:=ABuffer;
  while RemainData>0 do begin
    FAT:=AContext.FATIndex;
    CanRead:=GetFATRemainForPosition(Position);
    if CanRead>RemainData then begin
      CanRead:=RemainData;
    end;
    if CanRead+AContext.Position>=AContext.Size then begin
      CanRead:=AContext.Size-AContext.Position;
    end;
    if IsReservedValue(FAT) then begin
      if FAT<>SECT_ENDOFCHAIN then begin
        Raise Exception.Create('Damaged stream');
      end;
    end;
    if CanRead>0 then begin
      FStream.Position:=SectorAbsolute(FAT)+GetFATOffsetForPosition(Position);
      EffectiveRead:=FStream.Read(TargetBuffer^,CanRead);
      dec(RemainData,EffectiveRead);
      inc(Position,EffectiveRead);
      inc(TargetBuffer,EffectiveRead);
      if (EffectiveRead<>CanRead) Then begin
        AContext.FATIndex:=SECT_FREESECT;
        Result:=ASize-RemainData;
        Exit;
      end;
    end;
    AContext.Position:=Position;
    if GetFATRemainForPosition(Position)=FSectorSize then begin
      //The stream is in sector boundary
      NextFAT:=ReadFATEntryValue(FAT);
      if NextFAT<>SECT_ENDOFCHAIN then begin
        AContext.FATIndex:=NextFAT;
      end else begin
        AContext.FATIndex:=SECT_FREESECT; //Force resynchronization
        Result:=ASize-RemainData;
        Exit;
      end;
    end;
  end;
  Result:=ASize;
end;

function TFATIndirect.WriteData(var AContext: TFATStreamContext;
  const ABuffer: PBYTE; const ASize: SizeInt): SizeInt;
var
  Position: int64;
  FAT,NextFAT: SECT;
  CanWrite: SizeUint;
  RemainData: int64;
  SourceBuffer: PBYTE;
  EffectiveWrite: int64;
  procedure MoveMiniStreamToRegularStream();
  var
    ContextBackup: TFATStreamContext;
  begin
    ContextBackup:=AContext;
    AContext.MiniStream:=nil;
    //Delete here the mini stream if needed.
    if not IsReservedValue(AContext.FATFirstIndex) then begin
      ResetMiniFATLinkage(AContext.FATFirstIndex,SECT_FREESECT);
    end;
    AContext.FATFirstIndex:=SECT_ENDOFCHAIN;
    AContext.FATIndex:=SECT_ENDOFCHAIN;
    AContext.Size:=0;
    AContext.Position:=0;
    WriteData(AContext,ContextBackup.MiniStream.Memory,ContextBackup.MiniStream.Size);
    ContextBackup.MiniStream.Free;
  end;
begin
  if ASize<1 then begin
    Result:=0;
    Exit;
  end;
  if AContext.AccessMode=fmOpenRead then begin
    Raise EStreamError.Create('Stream can not be written, open for read only');
  end;
  AContext.Dirty:=true;
  if Assigned(AContext.MiniStream) then begin
    //It could be a MiniFAT stream.
    if AContext.Size+ASize>=FHeader._ulMiniSectorCutoff then begin
      //Jump to regular FAT allocation
      MoveMiniStreamToRegularStream();
    end else begin
      //Write in the MiniFAT cache
      AContext.MiniStream.Position:=AContext.Position;
      Result:=AContext.MiniStream.Write(ABuffer^,ASize);
      AContext.Position:=AContext.MiniStream.Position;
      AContext.Size:=AContext.MiniStream.Size;
      Exit;
    end;
  end;
  if (AContext.FATIndex=SECT_FREESECT) then begin
    ResynchronizePositionWithAllocation(AContext);
  end;
  Position:=AContext.Position;
  RemainData:=ASize;
  SourceBuffer:=ABuffer;

  while RemainData>0 do begin
    FAT:=AContext.FATIndex;
    CanWrite:=GetFATRemainForPosition(Position);
    if CanWrite>RemainData then begin
      CanWrite:=RemainData;
    end;
    if IsReservedValue(FAT) then begin
      if FAT<>SECT_ENDOFCHAIN then begin
        Raise Exception.Create('Damaged stream');
      end;
      FAT:=AllocateNewSector(0,SECT_FREESECT,SECT_ENDOFCHAIN);
      AContext.FATIndex:=FAT;
      AContext.FATFirstIndex:=FAT;
    end;
    if CanWrite>0 then begin
      FStream.Position:=SectorAbsolute(FAT)+GetFATOffsetForPosition(Position);
      EffectiveWrite:=FStream.Write(SourceBuffer^,CanWrite);
      dec(RemainData,EffectiveWrite);
      inc(Position,EffectiveWrite);
      inc(SourceBuffer,EffectiveWrite);
      if (EffectiveWrite<>CanWrite) Then begin
        AContext.FATIndex:=SECT_FREESECT;
        Result:=ASize-RemainData;
        Exit;
      end;
    end;
    AContext.Position:=Position;
    if AContext.Position>AContext.Size then begin
      //The file is growing...
      AContext.Size:=AContext.Position;
    end;
    //--------------
    if GetFATRemainForPosition(Position)=FSectorSize then begin
      if FAT<>SECT_ENDOFCHAIN then begin
        NextFAT:=ReadFATEntryValue(FAT);
      end;
      if NextFAT<>SECT_ENDOFCHAIN then begin
        AContext.FATIndex:=NextFAT;
      end else begin
        //Add a new FAT allocation.
        NextFAT:=AllocateNewSector(FAT,FAT,SECT_ENDOFCHAIN);
        if NextFAT=SECT_FREESECT then begin
          //Failed to allocate new sector, Media full ?
          AContext.FATIndex:=SECT_FREESECT; //Force resyncronization
          Result:=ASize-RemainData;
          Exit;
        end;
        WriteFATEntryValue(FAT,NextFAT);
        AContext.FATIndex:=NextFAT;
        NextFAT:=SECT_ENDOFCHAIN;
      end;
    end;
  end;
  Result:=ASize;
end;

function TFATIndirect.StreamSeekPosition(var AContext: TFATStreamContext;
  const AOffset: int64; const AOrigin: TSeekOrigin): int64;
begin
  Case AOrigin of
    soBeginning:  begin
                    AContext.FATIndex:=SECT_FREESECT; //Resets FAT seek
                    AContext.Position:=AOffset;
                  end;
    soEnd:        begin
                    AContext.FATIndex:=SECT_FREESECT;
                    if AContext.Size<AOffset then begin
                      Raise EStreamError.Create('Trying to move to before beginning of stream.');
                    end;
                    AContext.Position:=AContext.Size+AOffset;
                  end;
    soCurrent:    begin
                    AContext.FATIndex:=SECT_FREESECT;
                    if AContext.Position+AOffset<0 then begin
                      Raise EStreamError.Create('Trying to move to before beginning of stream.');
                    end;
                    AContext.Position:=AContext.Position+AOffset;
                  end;
    else
      Raise Exception.Create('Invalid Seek mode');
  end;
  Result:=AContext.Position;
end;

function TFATIndirect.Initialize(const ACreate: Boolean): Boolean;
var
  RootEntry: TWCBFStructuredStorageDirectoryEntry;
begin
  Result:=true;
  if ACreate then begin
    FillByte(FHeader,SizeOf(FHeader),0);
    FHeader._abSig:=OLE_SIGTATURE;
    //FHeader._clid:=ALL ZEROS, as set by FillByte
    FHeader._uMinorVersion:=$003E;
    FHeader._uDllVersion:=$0003;
    FHeader._uByteOrder:=NToLE($FFFE);
    FHeader._uSectorShift:=9;
    FHeader._uMiniSectorShift:=6;
    //Some reserved, must be zero..., Reserved, Reserved1 and Reserved2.
    FHeader._csectFat:=0;
    FHeader._sectDirStart:=SECT_ENDOFCHAIN;
    //FHeader._signature:=0; //Transaction not used.
    FHeader._ulMiniSectorCutoff:=4096; //This is customizable
    FHeader._sectMiniFatStart:=SECT_ENDOFCHAIN;
    FHeader._csectMiniFat:=0;
    FHeader._sectDifStart:=SECT_ENDOFCHAIN;
    FHeader._csectDif:=0;

    Format(FHeader);

    FStream.Position:=0;

    FillByte(FHeader._sectFat[0],Sizeof(FHeader._sectFat),$FF);
    FStream.Write(FHeader,Sizeof(FHeader));
  end else begin
    FStream.Position:=0;
    if FStream.Size<SizeOf(FHeader) then begin
      Result:=false;
      Exit;
    end;
    if FStream.Read(FHeader,Sizeof(FHeader))<>Sizeof(FHeader) then begin
      Result:=false;
    end;
  end;

  if NToLE(FHeader._uByteOrder)=$FFFE then begin
    //Does not need to perform any endian change, the file endianess
    //matches the current machine endianess. This means big endian file
    //in big endian machine or little endian file in little endian machine.
    FNeedEndianChange:=false;
  end else begin
    FNeedEndianChange:=true;
  end;

  iHeaderLEToN(FHeader);

  //Check some structured details to detect file structure as expected
  if not CompareMem(@FHeader._abSig[0],@OLE_SIGTATURE[0],sizeof(OLE_SIGTATURE)) then begin
    Result:=false;
    Exit;
  end;

  //----------------------
  //This code is a placeholder for future compatibility updates when documentation
  //about different sector sizes in versions are found.
  if FHeader._uDllVersion=$0003 then begin
    //Sector size is fixed to 512 :-? really strange way. NOT DOCUMENTED
    FHeaderLength:=1 shl FHeader._uSectorShift;
  end else if FHeader._uDllVersion=$0004 then begin
    //Sector size is fixed to 4096 ????? NOT DOCUMENTED
    FHeaderLength:=1 shl FHeader._uSectorShift;
  end else begin
    Raise Exception.CreateFmt('OLE file version %d not supported.',[FHeader._uDllVersion]);
  end;
  if FHeaderLength<512 then begin
    //Header MUST be at least 512...
    FHeaderLength:=512;
  end;
  //-----------------------

  FSectorSize:=1 shl FHeader._uSectorShift;
  FFATEntriesPerSect:=(FSectorSize div BYTES_PER_FAT_ENTRY);

  FFATCache.Sector:=SECT_FREESECT;
  FFATCache.Dirty:=false;
  GetMem(FFATCache.Cache,FSectorSize);

  //Content of this buffer will not be catched at all, used
  //for some read/write operations to avoid the alloc/free
  //process. Its content is undetermined.
  GetMem(FMiscSectorBuffer,FSectorSize);

  //Loads the DIF structure in memory for fast access.
  LoadDIFArray();

  //FAT entries that fit in the header FAT indirect links.
  FEntriesShort:=FFATEntriesPerSect*Length(FHeader._sectFat);

  if ACreate then begin
    //The root directory entry must be created here just to
    //simplificate the design of creation files and Root Entry
    //must exists in a compound binary file.
    RootEntry._mse:=0;
    FillByte(RootEntry,sizeof(RootEntry),0);
    RootEntry._ab:='Root Entry';
    RootEntry._cb:=(Length('Root Entry')+1)*sizeof(WChar);
    RootEntry._mse:=BYTE(STGTY_ROOT);
    RootEntry._bflags:=BYTE(DE_BLACK);
    RootEntry._sidLeftSib:=WINCOMPOUND_NOSID;
    RootEntry._sidRightSib:=WINCOMPOUND_NOSID;
    RootEntry._sidChild:=WINCOMPOUND_NOSID;
    //RootEntry._clsid:=
    RootEntry._dwUserFlags:=0;
    RootEntry._time[0].dwHighDateTime:=0;
    RootEntry._time[0].dwLowDateTime:=0;
    RootEntry._time[1].dwHighDateTime:=0;
    RootEntry._time[1].dwLowDateTime:=0;
    RootEntry._sectStart:=SECT_ENDOFCHAIN;
    RootEntry._ulSize:=0;
    RootEntry._dptPropType:=0;

    DirectoryContext.FATFirstIndex:=FHeader._sectDirStart;
    DirectoryContext.FATIndex:=SECT_FREESECT;
    DirectoryContext.Position:=0;
    DirectoryContext.Size:=0;
    DirectoryContext.MiniStream:=nil;
    DirectoryContext.AccessMode:=fmOpenReadWrite;

    WriteData(DirectoryContext,@RootEntry,SizeOf(RootEntry));
    FillByte(RootEntry,sizeof(RootEntry),0);
    WriteData(DirectoryContext,@RootEntry,SizeOf(RootEntry));

    FHeader._sectDirStart:=DirectoryContext.FATFirstIndex;

  end;

  DirectoryContext.FATFirstIndex:=FHeader._sectDirStart;
  DirectoryContext.FATIndex:=DirectoryContext.FATFirstIndex;
  DirectoryContext.Position:=0;
  DirectoryContext.Size:=GetStreamSizeInSectors(DirectoryContext)*FSectorSize;
  DirectoryContext.MiniStream:=nil;
  DirectoryContext.AccessMode:=fmOpenReadWrite;

  MiniFATContext.FATFirstIndex:=FHeader._sectMiniFatStart;
  MiniFATContext.FATIndex:=FHeader._sectMiniFatStart;
  MiniFATContext.Position:=0;
  MiniFATContext.Size:=GetStreamSizeInSectors(MiniFATContext)*FSectorSize;
  MiniFATContext.MiniStream:=nil;
  MiniFATContext.AccessMode:=fmOpenReadWrite;

  InitializeMiniDataStream();
  MiniFATDataContext.AccessMode:=fmOpenReadWrite;
end;

function TFATIndirect.ReadFATEntryValue(const AFATEntryIndex: SizeUint): SECT;
var
  Sector: SECT;
  DIFIndex,FATInDIF,OffSetInFAT: SECT;
begin
try
  DecomposeFATEntry(AFATEntryIndex,DIFIndex,FATInDIF,OffSetInFAT);
  Sector:=FDIFArray[DIFIndex][FATInDIF];
except
beep;
end;
  ReadFATSector(Sector);
  Result:=FFATCache.Cache[OffSetInFAT];
end;

procedure TFATIndirect.WriteFATEntryValue(const AFATEntryIndex: SizeUint;
  const ALinkTo: SECT);
var
  DIF,FATInDIF,OffSetInFAT: SECT;
  Sector: SECT;
begin
  DecomposeFATEntry(AFATEntryIndex,DIF,FATInDIF,OffSetInFAT);
  Sector:=FDIFArray[DIF][FATInDIF];
  ReadFATSector(Sector);
  FFATCache.Cache[OffSetInFAT]:=ALinkTo;
  WriteFATSector(Sector);
end;

procedure TFATIndirect.ResynchronizePosition(var AContext: TFATStreamContext);
var
  LastSector: SECT;
begin
  LastSector:=FollowFATLinkage(AContext.FATFirstIndex,AContext.Position);
  if LastSector<>SECT_FREESECT then begin
    AContext.FATIndex:=LastSector;
  end else begin
    AContext.FATIndex:=SECT_FREESECT;
  end;
end;

procedure TFATIndirect.ResynchronizePositionWithAllocation(
  var AContext: TFATStreamContext);
var
  Sector,LastSector: SECT;
  Skip: int64;
begin
  Skip:=AContext.Position;
  LastSector:=AContext.FATFirstIndex;
  dec(Skip,FSectorSize);
  while (Skip>=0) and (LastSector<$FFFFFFF0) do begin
    Sector:=LastSector;
    LastSector:=ReadFATEntryValue(Sector);
    if LastSector=SECT_ENDOFCHAIN then begin
      LastSector:=AllocateNewSector(Sector,Sector,SECT_ENDOFCHAIN);
      if LastSector=SECT_FREESECT then begin
        //Unable to allocate new FAT sector, Media full ????
        AContext.FATIndex:=SECT_FREESECT;
        Exit;
      end;
    end;
    dec(Skip,FSectorSize);
  end;
  if (LastSector>=$FFFFFFF0) and (Skip>FSectorSize) then begin
    AContext.FATIndex:=SECT_FREESECT;
  end else begin
    AContext.FATIndex:=LastSector;
  end;
end;

function TFATIndirect.FollowFATLinkage(const AStartFATEntryIndex: SizeUint;
  const ABytes: int64): SECT;
var
  Sector,LastSector: SECT;
  Skip: int64;
begin
  Skip:=ABytes;
  LastSector:=AStartFATEntryIndex;
  dec(Skip,FSectorSize);
  while (Skip>=0) and (LastSector<$FFFFFFF0) do begin
    Sector:=LastSector;
    LastSector:=ReadFATEntryValue(Sector);
    dec(Skip,FSectorSize);
  end;
  if (LastSector>=$FFFFFFF0) and (Skip>FSectorSize) then begin
    Result:=SECT_FREESECT;
  end else begin
    Result:=LastSector;
  end;
end;

procedure TFATIndirect.ResetFATLinkage(const AStartFATEntryIndex: SizeUint;
  const ANewFATValue: SECT);
var
  Sector,LastSector: SECT;
begin
  LastSector:=AStartFATEntryIndex;
  while not IsReservedValue(LastSector) do begin
    Sector:=LastSector;
    LastSector:=ReadFATEntryValue(Sector);
    WriteFATEntryValue(Sector,ANewFATValue);
  end;
end;

procedure TFATIndirect.Format(var AHeader: TWCBFStructuredStorageHeader);
begin
  //Accept defaults.
  AHeader._csectDif:=AHeader._csectDif; //just to avoid hints...
end;

function TFATIndirect.IsSizeInMiniFAT(const ASize: int64): Boolean;
begin
  if ASize<FHeader._ulMiniSectorCutoff then begin
    Result:=true;
  end else begin
    Result:=false;
  end;
end;

function TFATIndirect.OpenStream(const AFirstFATSector: DWORD;
  const ASize: int64; const AAccessMode: WORD): TFATStreamContext;
begin
  Result.FATFirstIndex:=AFirstFATSector;
  Result.FATIndex:=AFirstFATSector;
  Result.Size:=ASize;
  Result.Position:=0;
  Result.Dirty:=false;
  Result.AccessMode:=AAccessMode;
  if ASize<FHeader._ulMiniSectorCutoff then begin
    Result.MiniStream:=TMemoryStream.Create;
    ReadFromMiniFAT(Result,Result.MiniStream);
    if Result.MiniStream.Size<>ASize then begin
      Result.FATFirstIndex:=SECT_ENDOFCHAIN;
      Result.FATIndex:=SECT_ENDOFCHAIN;
      Result.Size:=0;
      Result.MiniStream.Free;
      Result.MiniStream:=nil;
    end;
  end else begin
    Result.MiniStream:=nil;
  end;
end;

procedure TFATIndirect.CloseStream(var AContext: TFATStreamContext);
begin

  if Assigned(AContext.MiniStream) then begin
    if AContext.Dirty then begin
      WriteToMiniFAT(AContext,AContext.MiniStream);
    end;
    FreeAndNIL(AContext.MiniStream);
  end;
end;

constructor TFATIndirect.Create(const AOLEStream: TStream);
begin
  FStream:=AOLEStream;
  FStream.Position:=0; //Beginning
  FillByte(DirectoryContext,sizeof(DirectoryContext),0);
end;

destructor TFATIndirect.Destroy;
var
  FillUp: int64;
begin
  FlushCaches();
  UnloadDIFArray();
  if FDirtyMedia then begin
    //Rewrite the header...
    FHeader._sectDirStart:=DirectoryContext.FATFirstIndex;
    FHeader._sectMiniFatStart:=MiniFATContext.FATFirstIndex;
    FHeader._csectMiniFat:=MiniFATContext.Size div FSectorSize;
    iHeaderNToLE(FHeader);
    FStream.Position:=0;
    FStream.WriteBuffer(FHeader,Sizeof(FHeader));
    FillUp:=FStream.Size-FHeaderLength;
    if FillUp mod FSectorSize <> 0 then begin
      //Fillup to multiple of Sector Size excluding the header.
      FStream.Position:=FStream.Size;
      FillByte(FMiscSectorBuffer^,FSectorSize,$FF);
      FStream.WriteBuffer(FMiscSectorBuffer^,FSectorSize-(FillUp mod FSectorSize));
    end;
  end;
  if Assigned(FFATCache.Cache) then Freemem(FFATCache.Cache);
  if Assigned(FMiscSectorBuffer) then FreeMem(FMiscSectorBuffer);
  inherited Destroy;
end;

//Matches mask taken from FV package of FPC with little changes
function MatchesMask(What, Mask: string): boolean;
  Function CmpStr(const hstr1,hstr2:string):boolean;
  var
    found : boolean;
    i1,i2 : SizeInt;
  begin
    i1:=0;
    i2:=0;
    if hstr1='' then
      begin
        CmpStr:=(hstr2='');
        exit;
      end;
    found:=true;
    repeat
      inc(i1);
      if (i1>length(hstr1)) then
        break;
      inc(i2);
      if (i2>length(hstr2)) then
        break;
      case hstr1[i1] of
        '?' :
          found:=true;
        '*' :
          begin
            found:=true;
            if (i1=length(hstr1)) then
             i2:=length(hstr2)
            else
             if (i1<length(hstr1)) and (hstr1[i1+1]<>hstr2[i2]) then
              begin
                if i2<length(hstr2) then
                 dec(i1)
              end
            else
             if i2>1 then
              dec(i2);
          end;
        else
          found:=(hstr1[i1]=hstr2[i2]) or (hstr2[i2]='?');
      end;
    until not found;
    if found then
      begin
        found:=(i2>=length(hstr2)) and
               (
                (i1>length(hstr1)) or
                ((i1=length(hstr1)) and
                 (hstr1[i1]='*'))
               );
      end;
    CmpStr:=found;
  end;

begin
  MatchesMask:=CmpStr(Mask,What);
end;

{ TMaskFile }

function TMaskFile.Matches(const AFileName: UTF8String): Boolean;
begin
  Result:=MatchesMask(AFileName,FMask)
end;

constructor TMaskFile.Create(const AMask: UTF8String);
begin
  FMask:=AMask;
end;

end.

