unit uvirtuallayer_stream;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  uvirtuallayer_types;

type

{ TVirtualLayer_Stream }

TVirtualLayer_Stream=class(TStream)
private
  FRootVirtualLayer: Pointer;
  FHandle: TvlHandle;
protected
  FFilename: UTF8String;
  function  GetPosition: Int64; override;
  procedure SetPosition(const Pos: Int64); override;
{  function  GetSize: Int64; virtual;
  procedure SetSize64(const NewSize: Int64); virtual;
  procedure SetSize(NewSize: Longint); virtual;overload;
  procedure SetSize(const NewSize: Int64); virtual;overload;
}
public
  property Filename: UTF8String Read FFilename;
  function Read(var Buffer; Count: Longint): Longint; override;
  function Write(const Buffer; Count: Longint): Longint; override;
  function Seek(Offset: Longint; Origin: Word): Longint; override; overload;
  function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override; overload;
  Constructor Create(const AVirtualLayer: Pointer; const AFileName: UTF8String;const AMode: cardinal);
  Destructor Destroy(); override;
end;

implementation

uses uvirtuallayer;

function VL(L: Pointer): TVirtualLayer; inline;
begin
  Result:=TVirtualLayer(L);
end;

{ TVirtualLayer_Stream }

function TVirtualLayer_Stream.GetPosition: Int64;
begin
  Result:=VL(FRootVirtualLayer).Seek(FHandle,0,fsFromCurrent);
end;

procedure TVirtualLayer_Stream.SetPosition(const Pos: Int64);
var
  NewPos: int64;
begin
  NewPos:=VL(FRootVirtualLayer).Seek(FHandle,Pos,fsFromBeginning);
  if NewPos<0 Then begin
    //Raise exception ??? which one :-?
  end;
end;

function TVirtualLayer_Stream.Read(var Buffer; Count: Longint): Longint;
begin
  Result:=VL(FRootVirtualLayer).Read(FHandle,@Buffer,Count);
end;

function TVirtualLayer_Stream.Write(const Buffer; Count: Longint): Longint;
begin
  Result:=VL(FRootVirtualLayer).Write(FHandle,@Buffer,Count);
end;

function TVirtualLayer_Stream.Seek(Offset: Longint; Origin: Word): Longint;
begin
  Result:=VL(FRootVirtualLayer).Seek(FHandle,Offset,Origin);
end;

function TVirtualLayer_Stream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
var
  Org: Word;
begin
  Case Origin of
    soCurrent:  Org:=fsFromCurrent;
    soEnd:      Org:=fsFromEnd;
    soBeginning:Org:=fsFromBeginning;
  end;
  Result:=VL(FRootVirtualLayer).Seek(FHandle,Offset,Org);
end;

constructor TVirtualLayer_Stream.Create(const AVirtualLayer: Pointer;
  const AFileName: UTF8String; const AMode: cardinal);
begin
  FRootVirtualLayer:=AVirtualLayer;
  FHandle:=VL(FRootVirtualLayer).OpenFile(AFileName,AMode);
  if FHandle=nil Then begin
    Raise EStreamError.Create('Unable to open '+AFileName);
  end;
  FFilename:=AFileName;
end;

destructor TVirtualLayer_Stream.Destroy();
begin
  if FHandle<>nil Then begin
    VL(FRootVirtualLayer).CloseFile(FHandle);
  end;
  inherited Destroy();
end;

end.

