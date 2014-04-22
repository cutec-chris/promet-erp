unit uFavoritesIE;

{$mode objfpc}{$H+}

interface
uses
  Classes,sysutils;
function GetIEFavourites(favpath: string = ''): TStrings;

implementation
{$IFDEF WINDOWS}
uses
  ShlObj, ActiveX, Windows;

function GetIEFavourites(favpath: string): TStrings;
var
  searchrec: TSearchRec;
  str: TStrings;
  path, dir, FileName: string;
  Buffer: array[0..2047] of Char;
  found: Integer;
  aFavPath: array[0..2047] of char;
begin
  if FavPath = '' then
    begin
      SHGetSpecialFolderPath(0,aFavPath,CSIDL_FAVORITES,false);
      favpath:=aFavPath;
    end;
  str := TStringList.Create;
  // Get all file names in the favourites path
  path  := FavPath + '\*.url';
  dir   := ExtractFilepath(path);
  found := FindFirst(path, faAnyFile, searchrec);
  while found = 0 do
  begin
    // Get now URLs from files in variable files
    Setstring(FileName, Buffer, GetPrivateProfilestring('InternetShortcut',
      PChar('URL'), nil, Buffer, SizeOf(Buffer), PChar(dir + searchrec.Name)));
    str.Values[copy(searchrec.Name,0,length(searchrec.Name)-4)] := FileName;
    found := FindNext(searchrec);
  end;
  // find Subfolders
  found := FindFirst(dir + '\*.*', faAnyFile, searchrec);
  while found = 0 do
  begin
    if ((searchrec.Attr and faDirectory) > 0) and (searchrec.Name[1] <> '.') then
      str.Addstrings(GetIEFavourites(dir + '\' + searchrec.Name));
    found := FindNext(searchrec);
  end;
  SysUtils.FindClose(searchrec);
  Result := str;
end;

procedure FreePidl(pidl: PItemIDList);
var
  allocator: IMalloc;
begin
  if Succeeded(SHGetMalloc(allocator)) then
  begin
    allocator.Free(pidl);
    {$IFDEF VER100}
    allocator.Release;
    {$ENDIF}
  end;
end;
{$ELSE}
function GetIEFavourites(favpath: string): TStrings;
begin
end;
{$ENDIF}
end.

