unit ufavoriteschrome;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,FileUtil,fpjson,jsonparser,lclproc;

function GetChromeFavourites : TStrings;

implementation
{$IFDEF MSWINDOWS}
uses Windows;
{$ENDIF}
function GetChromeFavourites: TStrings;
{$IFDEF MSWINDOWS}
type
  PFNSHGetFolderPath = Function(Ahwnd: HWND; Csidl: Integer; Token: THandle; Flags: DWord; aPath: PChar): HRESULT; stdcall;
const
  CSIDL_COMMON_APPDATA  = $0023; // All Users\Application Data
  CSIDL_LOCAL_APPDATA   = $001c;
  CSIDL_FLAG_CREATE     = $8000; { (force creation of requested folder if it doesn't exist yet)     }
var
  bPath: array [0..1024] of char;
  P : Pointer;
  SHGetFolderPath : PFNSHGetFolderPath = Nil;
  CFGDLLHandle : THandle = 0;
{$ENDIF}
var
  Path : string;
  sl: TStringList;
  JsonData: TJSONData;
  procedure SetPath(apath : string);
  begin
    if DirectoryExists(CleanAndExpandDirectory(aPath)) then
      Path := CleanAndExpandDirectory(aPath);
  end;
  procedure RecurseURLs(Json : TJSONData);
  var
    i: Integer;
    a: Integer;
    b: Integer;
    procedure RecourseArray(ja : TJSONArray);
    var
      c: Integer;
      d: Integer;
    begin
      if ja is TJSONArray then
        begin
          {$IF FPC_FULLVERSION>20600}
          for c := 0 to  TJsonArray(ja).Count-1 do
            if TJsonArray(ja).Items[c].JSONType = jtObject then
              if Assigned(TJSONObject(ja.Items[c]).Find('type')) and (TJSONObject(ja.Items[c]).Elements['type'].AsString = 'url') then
                begin
                  Result.Values[TJSONObject(ja.Items[c]).Elements['name'].AsString]:=TJSONObject(ja.Items[c]).Elements['url'].AsString;
                end;
          {$ENDIF}
        end;
    end;
  begin
    if Json.JSONType=jtObject then
      begin
        {$IF FPC_FULLVERSION>20600}
        if Assigned(TJSONObject(json).Find('type')) and (TJSONObject(json).Elements['type'].AsString = 'folder') then
          begin
            for a := 0 to json.Count-1 do
              begin
                if json.Items[a] is TJSONArray then
                  RecourseArray(TJSONArray(json.Items[a]));
              end;
          end;
        {$ENDIF}
      end;
    for i := 0 to json.Count-1 do
      begin
        if json.Items[i].JSONType = jtObject then
          RecurseURLs(Json.Items[i]);
      end;
  end;

begin
  {$IFDEF MSWINDOWS}
  CFGDLLHandle:=LoadLibrary('shell32.dll');
  if (CFGDLLHandle<>0) then
    begin
    P:=GetProcAddress(CFGDLLHandle,'SHGetFolderPathA');
    If (P=Nil) then
      begin
      FreeLibrary(CFGDLLHandle);
      CFGDllHandle:=0;
      end
    else
      SHGetFolderPath:=PFNSHGetFolderPath(P);
    end;
  If (P=Nil) then
    begin
    CFGDLLHandle:=LoadLibrary('shfolder.dll');
    if (CFGDLLHandle<>0) then
      begin
      P:=GetProcAddress(CFGDLLHandle,'SHGetFolderPathA');
      If (P=Nil) then
        begin
        FreeLibrary(CFGDLLHandle);
        CFGDllHandle:=0;
        end
      else
        ShGetFolderPath:=PFNSHGetFolderPath(P);
      end;
    end;
  If (@ShGetFolderPath<>Nil) then
    begin
      if SHGetFolderPath(0,CSIDL_LOCAL_APPDATA or CSIDL_FLAG_CREATE,0,0,@bPATH[0])=S_OK then
        Path := StrPas(@bPath[0]);
    end;
  {$ENDIF}
  Result := TStringList.Create;
  {$IFDEF LINUX}
  SetPath('~/.config/google-chrome/Default');
  SetPath('~/.config/chromium/Default');
  {$ENDIF}
  {$IFDEF UNIX}
  SetPath('~/Library/Application Support/Google/Chrome/Default');
  {$ENDIF}
  {$IFDEF WINDOWS}
  SetPath(Path+'\Google\Chrome\User Data\');
  {$ENDIF}
  if FileExists(AppendPathDelim(Path)+'Bookmarks') then
    begin
      sl := TStringList.Create;
      sl.LoadFromFile(AppendPathDelim(Path)+'Bookmarks');
      with TJSONParser.Create(sl.text) do
        try
          JsonData := Parse;
          RecurseURLs(JsonData);
        finally
          FreeAndNil(JsonData);
          Free;
        end;
      sl.Free;
    end;
end;

end.

