{-------------------------------------------------------------------------------
 Unit Name: ShellObjHelper
 Author   : hans gulo (HG)
 Purpose  : Shell Object helper routines
-------------------------------------------------------------------------------}

unit ShellObjHelper;

{$MODE Delphi}

{$IFDEF VER100}{$DEFINE DELPHI3}{$ENDIF}

interface

uses
  ShlObj, ShObjIdlQuot, ActiveX, Comobj,Windows, Graphics;

{$IFDEF DELPHI3}
// Delphi 3 SysUtils does not have this function
function ExcludeTrailingBackslash(const Src: string): string;
{$ENDIF}

// IShellFolder methods helper
procedure ShellFolderBindToObject(const ShellFolder: IShellFolder;
  PIDL: PItemIDList; const riid: TGUID; out pv);
function ShellFolderGetUIObjectOf(const ShellFolder: IShellFolder;
  cidl: DWORD; var PIDL: PItemIDList; riid: TGUID; out pv): Boolean;
procedure ShellFolderParseDisplayName(const ShellFolder: IShellFolder;
  const DisplayName: string; out PIDL: PItemIDList);
function ShellFolderGetExtractImage(const ShellFolder: IShellFolder;
  const RelativeFileName: string; Malloc: IMalloc;
  out XtractImage: IExtractImage): Boolean;

function GetExtractImageItfPtr(const FileName: string;
  out XtractImage: IExtractImage): Boolean;
function GetFileLargeIcon(const FileName: string;
  out LargeIcon: TIcon): Boolean;
function ExtractImageGetFileThumbnail(const XtractImage: IExtractImage;
  ImgWidth, ImgHeight, ImgColorDepth: Integer; var Flags: DWORD;
  out RunnableTask: IRunnableTask; out Bmp: Graphics.TBitmap): Boolean;
function GetSysImgListIndex(const FileName: string): Integer;
procedure GetShellFolderItfPtr(const FolderName: string; Malloc: IMalloc;
  out TargetFolder: IShellFolder);

implementation

uses SysUtils;

{$IFDEF DELPHI3}
function ExcludeTrailingBackslash(const Src: string): string;
begin
  Result := Src;
  if AnsiLastChar(Result) = '\' then
    SetLength(Result, Pred(Length(Result)));
end;
{$ENDIF DELPHI3}

procedure ShellFolderBindToObject(const ShellFolder: IShellFolder;
  PIDL: PItemIDList; const riid: TGUID; out pv);
begin
  OleCheck(ShellFolder.BindToObject(PIDL, nil, riid,
    {$IFDEF DELPHI3}Pointer(pv){$ELSE}pv{$ENDIF}));
end;

function ShellFolderGetUIObjectOf(const ShellFolder: IShellFolder;
  cidl: DWORD; var PIDL: PItemIDList; riid: TGUID; out pv): Boolean;
begin
  Result := NOERROR = ShellFolder.GetUIObjectOf(0, cidl, PIDL,
    riid, nil, {$IFDEF DELPHI3}Pointer(pv){$ELSE}pv{$ENDIF});
end;

procedure ShellFolderParseDisplayName(const ShellFolder: IShellFolder;
  const DisplayName: string; out PIDL: PItemIDList);
var
  Attributes, Eaten: DWORD;
begin
  OleCheck(ShellFolder.ParseDisplayName(0, nil,
    PWideChar(WideString(DisplayName)), Eaten, PIDL, Attributes));
end;

function ShellFolderGetExtractImage(const ShellFolder: IShellFolder;
  const RelativeFileName: string; Malloc: IMalloc;
  out XtractImage: IExtractImage): Boolean;
var
  PIDL: PItemIDList;
begin
  ShellFolderParseDisplayName(ShellFolder, RelativeFileName, PIDL);
  Result := ShellFolderGetUIObjectOf(ShellFolder, 1, PIDL,
    IExtractImage, XtractImage);
  Malloc.Free(PIDL);
end;

function GetExtractImageItfPtr(const FileName: string;
  out XtractImage: IExtractImage): Boolean;
var
  TargetFolder: IShellFolder;
  FilePath: string;
  ItemIDList: PItemIDList;
  Malloc: IMalloc;
begin
  FilePath := ExcludeTrailingBackslash(ExtractFilePath(FileName));
  OleCheck(SHGetMalloc(Malloc));
  GetShellFolderItfPtr(FilePath, Malloc, TargetFolder);
  ShellFolderParseDisplayName(TargetFolder, ExtractFileName(FileName),
    ItemIDList);
  try
    Result := ShellFolderGetUIObjectOf(TargetFolder, 1, ItemIDList,
      IExtractImage, XtractImage);
  finally
    Malloc.Free(ItemIDList);
  end;
end;

function GetFileLargeIcon(const FileName: string; out LargeIcon: TIcon): Boolean;
var
  SFI: TSHFileInfo;
begin
  {
  if 0 <> SHGetFileInfo(PChar(FileName), FILE_ATTRIBUTE_ARCHIVE, SFI,
    sizeof(SFI), SHGFI_ICON or SHGFI_LARGEICON) then
  begin
    LargeIcon := TIcon.Create;
    LargeIcon.Handle := SFI.hIcon;
    Result := True;
  end else
    Result := False;
    }
end;

function ExtractImageGetFileThumbnail(const XtractImage: IExtractImage;
  ImgWidth, ImgHeight, ImgColorDepth: Integer; var Flags: DWORD;
  out RunnableTask: IRunnableTask; out Bmp: Graphics.TBitmap): Boolean;
var
  Size: TSize;
  Buf: array[0..MAX_PATH] of WideChar;
  BmpHandle: HBITMAP;
  Priority: DWORD;
  GetLocationRes: HRESULT;

  procedure FreeAndNilBitmap;
  begin
    {$IFNDEF DELPHI3}
    FreeAndNil(Bmp);
    {$ELSE}
    Bmp.Free;
    Bmp := nil;
    {$ENDIF}
  end;

begin
  Result := False;
  RunnableTask := nil;
  Size.cx := ImgWidth;
  Size.cy := ImgHeight;
  Priority := IEIT_PRIORITY_NORMAL;
  Flags := Flags or IEIFLAG_ASYNC;
  GetLocationRes := XtractImage.GetLocation(Buf, sizeof(Buf), Priority, Size,
    ImgColorDepth, Flags);

  if (GetLocationRes = NOERROR) or (GetLocationRes = E_PENDING) then
  begin
    if GetLocationRes = E_PENDING then
    begin
      { if QI for IRunnableTask succeed, we can use RunnableTask
        interface pointer later to kill running extraction process.
        We could spawn a new thread here to extract image. }
      if S_OK <> XtractImage.QueryInterface(IRunnableTask, RunnableTask) then
        RunnableTask := nil;
    end;
    Bmp := Graphics.TBitmap.Create;
    try
      OleCheck(XtractImage.Extract(BmpHandle)); // This could consume a long time.
                                                // If RunnableTask is available
                                                // then calling Kill() method
                                                // will immediately abort the process.
      Bmp.Handle := BmpHandle;
      Result := True;
    except
      on E: EOleSysError do
      begin
        //-------------
        OutputDebugString(PChar(string(E.ClassName) + ': ' + E.Message));
        //-------------
        FreeAndNilBitmap;
        Result := False;
      end;
      else
      begin
        FreeAndNilBitmap;
        raise;
      end;
    end; { try/except }
  end;
end;

function GetSysImgListIndex(const FileName: string): Integer;
var
  SFI: TSHFileInfo;
begin
  {SHGetFileInfo(PChar(FileName), 0, SFI, sizeof(TSHFileInfo),
    SHGFI_SYSICONINDEX);
  Result := SFI.iIcon;}
end;

procedure GetShellFolderItfPtr(const FolderName: string; Malloc: IMalloc;
  out TargetFolder: IShellFolder);
var
  DesktopFolder: IShellFolder;
  ItemIDList: PItemIDList;
begin
  OleCheck(SHGetDesktopFolder(DesktopFolder));
  ShellFolderParseDisplayName(DesktopFolder, FolderName, ItemIDList);
  try
    ShellFolderBindToObject(DesktopFolder, ItemIDList, IShellFolder, TargetFolder);
  finally
    Malloc.Free(ItemIDList);
  end;
end;

end.
