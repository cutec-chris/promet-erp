unit oledropfiles; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ActiveX, ShlObj, ComObj, Windows, Utils;

function GetFileDataObject(const Directory: string; Filename: String):IDataObject;

implementation

function SHGetMalloc (var ppMalloc : IMalloc) : hResult;stdcall; external 'SHELL32' name 'SHGetMalloc';
function SHGetDesktopFolder(var ppshf: IShellFolder):HResult; stdcall;external 'SHELL32' name 'SHGetDesktopFolder';

function GetFileDataObject(const Directory: string; Filename: String):IDataObject;
type
  PArrayOfPItemIDList = ^TArrayOfPItemIDList;
  TArrayOfPItemIDList = array[0..0] of PItemIDList;
var
  Malloc: IMalloc;
  Root: IShellFolder;
  FolderPidl: PItemIDList;
  Folder: IShellFolder;
  p: PArrayOfPItemIDList;
  chEaten: ULONG;
  dwAttributes: ULONG;
begin
  Result := nil;
  OleCheck(SHGetMalloc(Malloc));
  OleCheck(SHGetDesktopFolder(Root));
  OleCheck(Root.ParseDisplayName(0, nil, PWideChar(WideString(UniToSys(Directory))), chEaten, FolderPidl, dwAttributes));
  try
    OleCheck(Root.BindToObject(FolderPidl, nil, IShellFolder, Pointer(Folder)));
    p := AllocMem(SizeOf(PItemIDList) * 1);
    try
      OleCheck(Folder.ParseDisplayName(0, nil,PWideChar(WideString(UniToSys(Filename))), chEaten, p^[0],dwAttributes));
      OleCheck(Folder.GetUIObjectOf(0, 1, p^[0], IDataObject,nil,Pointer(Result)));
    finally
      if p^[0] <> nil then Malloc.Free(p^[0]);
      FreeMem(p);
    end;
  finally
    Malloc.Free(FolderPidl);
  end;
end;


end.

