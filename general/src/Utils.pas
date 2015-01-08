UNIT Utils;
INTERFACE
{$H+}
uses Classes,SysUtils
     {$IFDEF LCL}
     {$IFNDEF LCLnogui}
     ,Forms,Dialogs,Clipbrd,Translations,LCLProc,Graphics,LResources
     {$ENDIF}
     ,FileUtil,UTF8Process
     {$ENDIF}
     {$IFDEF MSWINDOWS}
     ,Registry,Windows
     {$ELSE}
     ,unix
     {$ENDIF}
;
{$IFNDEF FPC}
CONST
  DirectorySeparator = '\';
{$ENDIF}

type
 TRoundToRange = -37..37;
 {$ifdef WINDOWS}
 PFNSHGetFolderPath = Function(Ahwnd: HWND; Csidl: Integer; Token: THandle; Flags: DWord; Path: PChar): HRESULT; stdcall;
 {$endif}

function ClearDir (Path: string): boolean;
function RPos(const Substr: string; const S: string): Integer;
FUNCTION IsNumeric(s: STRING): boolean;
FUNCTION StrTimeToValue(val : string) : LongInt;
{$IFDEF LCL}
function TextCut(Len: Integer; Text: String): String;
{$ENDIF}
function InstallExt(Extension, ExtDescription, FileDescription,OpenWith, ParamString: string; IconIndex: Integer = 0): Boolean;
function SystemUserName : string;
function GetSystemName : string;
function HTTPEncode(const str : String) : string;
function ValidateFileName(old : string) : string;
function ValidateFileDir(old : string) : string;
function ValidateDate(D : string) : string;
function GetTempPath : string;
function GetConfigDir(app : string) : string;
function GetProgramDir : string;
function GetGlobalConfigDir(app : string;Global : Boolean = True) : string;
function GetHomeDir : string;
function SizeToText(size : Longint) : string;
function GetMimeTypeforExtension(Extension : string) : string;
function GetMainIconHandle(Resourcename : string) : Cardinal;
function CanWriteToProgramDir : Boolean;
function HexToBin(h: STRING): dword;
function RoundToSecond(aDate : TDateTime) : TDateTime;
function RoundToMinute(T : TDateTime): TDateTime;
function RoundTo(const AValue : extended ; const ADigit : TRoundToRange) : extended ;
function TimeTotext(Seconds : Integer) : string;
function GetSystemLang : string;
function DateTimeToHourString(DateTime : TDateTime) : string;
function DateTimeToIndustrialTime(dateTime : TDateTime) : string;
function ConvertUnknownStringdate(input : string) : TDateTime;
function HTMLEncode(const s : string)  : string;
function HTMLDecode(const si : string)  : string;
function UniToSys(const s: string): string; inline;
function SysToUni(const s: string): string; inline;
function AppendPathDelim(const Path: string): string; inline;
function FileSize(aFile : string) : Int64;
type
  TCopyFileFlag = (
    cffOverwriteFile,
    cffCreateDestDirectory,
    cffPreserveTime
    );
  TCopyFileFlags = set of TCopyFileFlag;
function CopyFile(const SrcFilename, DestFilename: string;
                  Flags: TCopyFileFlags=[cffOverwriteFile]): boolean;
IMPLEMENTATION
function GetMimeTypeforExtension(Extension : string) : string;
var
{$ifdef MSWINDOWS}
  reg : TRegistry;
{$else}
  f : TextFile;
  tmp : string;
{$endif}
begin
{$ifdef WINDOWS}
  Result := '';
  Reg := TRegistry.Create(KEY_READ);
  Reg.RootKey := HKEY_CLASSES_ROOT;
  if Reg.OpenKeyReadOnly(ExtractFileExt('.'+Extension)) then
  begin
    Result := Reg.ReadString('Content Type');
    Reg.CloseKey;
  end;
  Reg.Free;
{$ELSE}
  if FileExists('~/.local/share/mime/globs') then
    AssignFile(f,'~/.local/share/mime/globs')
  else if FileExists('/usr/local/share/mime/globs') then
    AssignFile(f,'/usr/local/share/mime/globs')
  else if FileExists('/usr/share/mime/globs') then
    AssignFile(f,'/usr/share/mime/globs')
  else
    exit;
  Reset(f);
  while not eof(f) do
    begin
      readln(f,tmp);
      if copy(tmp,pos(':*.',tmp)+3,length(tmp)) = Extension then
        result := copy(tmp,0,pos(':*.',tmp)-1);
    end;
  CloseFile(f);
{$endif}
end;
function TimeTotext(Seconds : Integer) : string;
var
  tmp : Integer;
begin
  if Seconds > 60*60 then
    begin
      Result := IntToStr(Trunc(Seconds/(60*60))) +' h';
      tmp := Seconds mod (60*60);
      Result := Result +' '+IntToStr(Trunc(tmp/(60))) +' m';
      tmp := Seconds mod 60;
      Result := Result +' '+IntToStr(tmp) +' s';
    end
  else if Seconds > 60 then
    begin
      Result := IntToStr(Trunc(Seconds/(60))) +' m';
      tmp := Seconds mod 60;
      Result := Result +' '+IntToStr(tmp) +' s';
    end
  else
    begin
      Result := IntToStr(Seconds)+' s';
    end
end;
function RoundToSecond(aDate : TDateTime) : TDateTime;
begin
  Result := Round(aDate * SecsPerDay) / SecsPerDay;
end;
function RoundToMinute(T : TDateTime): TDateTime;
Var
   H,M,S,ms : Word;
begin
   DecodeTime(T,H,M,S,ms);
   M := (M div 1) * 1;
   S := 0;
   Result := EncodeTime(H,M,S,ms);
end;
function RoundTo(const AValue : extended ; const ADigit : TRoundToRange) : extended ;
var X : extended ; i : integer ;
begin
  X := 1.0 ;
  for i := 1 to Abs(ADigit) do X := X * 10 ;
  if ADigit<0 then
    Result := Round(AValue * X) / X
  else
    Result := Round(AValue / X) * X;
end;
function HexToBin(h: STRING): dword;
  FUNCTION HexDigitToInt(c: Char): Integer;
  BEGIN
    IF (c >= '0') AND (c <= '9') THEN Result := Ord(c) - Ord('0')
    ELSE IF (c >= 'A') AND (c <= 'F') THEN Result := Ord(c) - Ord('A') + 10
    ELSE IF (c >= 'a') AND (c <= 'f') THEN Result := Ord(c) - Ord('a') + 10
    ELSE Result := -1;
  END;
VAR
  buf: ARRAY[0..16] OF Byte;
  digit1: Integer;
  bytes: Integer;
  index: Integer;
BEGIN
  bytes := 0;
  index := 0;
  result := 0;
  IF frac(length(h) / 2) = 0.5 THEN
    h := '0' + h;
  WHILE (bytes < 16) DO
    BEGIN
      if length(h) > index+1 then
        digit1 := HexDigitToInt(h[index + 1])
      else
        digit1 := -1;
      IF digit1 < 0 THEN
        break;
      buf[bytes] := (digit1 SHL 4) OR HexDigitToInt(h[index + 2]);
      Inc(index, 2);
      Inc(bytes);
    END;
  dec(bytes);
  FOR index := bytes DOWNTO 0 DO
    Result := Result + (buf[index] shl ((bytes-index)*8));
END;
{$IFDEF LCL}
{$ifndef ver2_0}
function Translate (Name,Value : AnsiString; Hash : Longint; arg:pointer) : AnsiString;
var
  po: TPOFile;
begin
  po:=TPOFile(arg);
  // get UTF8 string
  result := po.Translate(Name,Value);
  // convert UTF8 to current local
  if result<>'' then
    result:=UTF8ToSystemCharSet(result);
end;
{$ENDIF}
function TextCut(Len: Integer; Text: String): String;
begin
  if Len < length(Text) then
    Result := copy(Text,0,len-3)+'...'
  else
    Result := Text;
end;
{$ENDIF}
function GetSystemLang: string;
{$IFDEF WINDOWS}
var
  Ident: Integer;
  MyLang: PChar;
const
  Size: Integer = 250;
{$ENDIF}
begin
{$IFDEF WINDOWS}
  GetMem(MyLang, Size);
  Ident:=GetSystemDefaultLangID;
  VerLanguageName(Ident, MyLang, Size);
  Result:=StrPas(MyLang);
  FreeMem(MyLang);
{$ELSE}
  Result := GetEnvironmentVariable('LANG');
{$ENDIF}
end;
function DateTimeToHourString(DateTime: TDateTime): string;
var
  Hour,Minute,Second,Millisecond: word;
begin
  DecodeTime(DateTime,Hour,Minute,Second,Millisecond);
  Result := Format('%.2d:%.2d',[Trunc(DateTime)*HoursPerDay+Hour,Minute]);
end;
function DateTimeToIndustrialTime(dateTime: TDateTime): string;
var
  Hour,Minute,Second,Millisecond: word;
begin
  DecodeTime(DateTime,Hour,Minute,Second,Millisecond);
  Result := IntToStr(round((((Trunc(DateTime)*HoursperDay)+Hour)*100)+((Minute/60)*100)));
end;
function ConvertUnknownStringdate(input: string): TDateTime;
begin
  if (input <> '') and (pos('.',input) = 0) and (pos('-',input) = 0) and (pos('/',input) = 0) and (length(input) = 8) then
    Result := EncodeDate(StrToInt(copy(input,0,4)),StrToInt(copy(input,5,2)),StrToInt(copy(input,7,2)))
  else
    begin //decode date
      try
        if length(input) = 4 then //YYYY
          Result := EncodeDate(StrToInt(input),1,1)
        else if length(input) = 7 then
          begin
            if pos('-',input) = 5 then //YYYY-MM
              Result := EncodeDate(StrToInt(copy(input,0,4)),StrToInt(copy(input,6,2)),1)
          end
        else if length(input) = 10 then
          begin
            if pos('.',input) > 0 then
              begin
                if rpos('.',input) = 6  then //DD.MM.YYYY
                  Result := EncodeDate(StrToInt(copy(input,6,4)),StrToInt(copy(input,3,2)),StrToInt(copy(input,0,2)))
                else if pos('-',input) = 3  then //DD.MM.YY
                  Result := EncodeDate(1900+StrToInt(copy(input,6,2)),StrToInt(copy(input,3,2)),StrToInt(copy(input,0,2)))
              end
            else if pos('-',input) > 0 then
              begin
                if pos('-',input) = 5  then //YYYY-MM-DD
                  Result := EncodeDate(StrToInt(copy(input,0,4)),StrToInt(copy(input,6,2)),StrToInt(copy(input,9,2)))
                else if pos('-',input) = 3  then //YY-MM-DD
                  Result := EncodeDate(1900+StrToInt(copy(input,0,2)),StrToInt(copy(input,4,2)),StrToInt(copy(input,7,2)))
              end
            else if pos('/',input) > 0 then
              begin
                if pos('/',input) = 5  then //YYYY/MM/DD
                  Result := EncodeDate(StrToInt(copy(input,0,4)),StrToInt(copy(input,6,2)),StrToInt(copy(input,9,2)))
                else if pos('/',input) = 3  then //YY/MM/DD
                  Result := EncodeDate(1900+StrToInt(copy(input,0,2)),StrToInt(copy(input,4,2)),StrToInt(copy(input,7,2)))
              end;
          end;
        except
        end;
      end;
end;
function HTMLEncode(const s: string): string;
begin
  Result := StringReplace(s, '&', '&amp;', [rfreplaceall]);
  Result := StringReplace(Result, '"', '&quot;', [rfreplaceall]);
  Result := StringReplace(result, '<', '&lt;', [rfreplaceall]);
  Result := StringReplace(result, '>', '&gt;', [rfreplaceall]);
  Result := StringReplace(result, '''', '&auml', [rfreplaceall]);
  Result := StringReplace(result, 'ä', '&auml;', [rfreplaceall]);
  Result := StringReplace(result, 'ö', '&ouml;', [rfreplaceall]);
  Result := StringReplace(result, 'ü', '&uuml;', [rfreplaceall]);
  Result := StringReplace(result, 'Ä', '&Auml;', [rfreplaceall]);
  Result := StringReplace(result, 'Ö', '&Ouml;', [rfreplaceall]);
  Result := StringReplace(result, 'Ü', '&Uuml;', [rfreplaceall]);
  Result := StringReplace(result, 'ß', '&szlig;', [rfreplaceall]);
end;
function HTMLDecode(const si: string): string;
var
  Sp, Rp, Cp, Tp: PChar;
  S: String;
  I, Code: Integer;
  AStr: String;
begin
  Result := si;
  Result := StringReplace(Result, '&amp;' ,'&', [rfreplaceall]);
  Result := StringReplace(Result, '&quot;' ,'"', [rfreplaceall]);
  Result := StringReplace(Result, '&lt;' ,'<', [rfreplaceall]);
  Result := StringReplace(Result, '&gt;' ,'>', [rfreplaceall]);
  Result := StringReplace(Result, '&nbsp;' ,' ', [rfreplaceall]);
  Result := StringReplace(Result, '&auml;' ,'ä', [rfreplaceall]);
  Result := StringReplace(Result, '&ouml;' ,'ö', [rfreplaceall]);
  Result := StringReplace(Result, '&uuml;' ,'ü', [rfreplaceall]);
  Result := StringReplace(Result, '&Auml;' ,'Ä', [rfreplaceall]);
  Result := StringReplace(Result, '&Ouml;' ,'Ö', [rfreplaceall]);
  Result := StringReplace(Result, '&Uuml;' ,'Ü', [rfreplaceall]);
  Result := StringReplace(Result, '&szlig;','ß', [rfreplaceall]);
  AStr := UniToSys(Result);
  SetLength(Result, Length(AStr));
  Sp := PChar(AStr);
  Rp := PChar(Result);
  Cp := Sp;
  try
    while Sp^ <> #0 do
    begin
      case Sp^ of
        '&': begin
               Cp := Sp;
               Inc(Sp);
               case Sp^ of
                 '#': begin
                        Tp := Sp;
                        Inc(Tp);
                        while (Sp^ <> ';') and (Sp^ <> #0) do
                          Inc(Sp);
                        SetString(S, Tp, Sp - Tp);
                        Val(S, I, Code);
                        Rp^ := Chr((I));
                      end;
                 else
                   Exit;
               end;
           end
      else
        Rp^ := Sp^;
      end;
      Inc(Rp);
      Inc(Sp);
    end;
  except
  end;
  SetLength(Result, Rp - PChar(Result));
  Result := SysToUni(Result);
end;
var
  FNeedRTLAnsi: boolean = false;
  FNeedRTLAnsiValid: boolean = false;

function NeedRTLAnsi: boolean;
{$IFDEF WinCE}
// CP_UTF8 is missing in the windows unit of the Windows CE RTL
const
  CP_UTF8 = 65001;
{$ENDIF}
{$IFNDEF Windows}
var
  Lang: String;
  i: LongInt;
  Encoding: String;
{$ENDIF}
begin
  if FNeedRTLAnsiValid then
    exit(FNeedRTLAnsi);
  {$IFDEF Windows}
  FNeedRTLAnsi:=GetACP<>CP_UTF8;
  {$ELSE}
  FNeedRTLAnsi:=false;
  Lang := SysUtils.GetEnvironmentVariable('LC_ALL');
  if lang = '' then
  begin
    Lang := SysUtils.GetEnvironmentVariable('LC_MESSAGES');
    if Lang = '' then
    begin
      Lang := SysUtils.GetEnvironmentVariable('LANG');
    end;
  end;
  i:=System.Pos('.',Lang);
  if (i>0) then begin
    Encoding:=copy(Lang,i+1,length(Lang)-i);
    FNeedRTLAnsi:=(SysUtils.CompareText(Encoding,'UTF-8')<>0)
              and (SysUtils.CompareText(Encoding,'UTF8')<>0);
  end;
  {$ENDIF}
  FNeedRTLAnsiValid:=true;
  Result:=FNeedRTLAnsi;
end;

function IsASCII(const s: string): boolean; inline;
var
  i: Integer;
begin
  for i:=1 to length(s) do if ord(s[i])>127 then exit(false);
  Result:=true;
end;

function UniToSys(const s: string): string;
begin
  if NeedRTLAnsi and (not IsASCII(s)) then
    Result:=UTF8ToAnsi(s)
  else
    Result:=s;
end;

function SysToUni(const s: string): string;
begin
  if NeedRTLAnsi and (not IsASCII(s)) then
  begin
    Result:=AnsiToUTF8(s);
    {$ifdef FPC_HAS_CPSTRING}
    // prevent UTF8 codepage appear in the strings - we don't need codepage
    // conversion magic in LCL code
    SetCodePage(RawByteString(Result), StringCodePage(s), False);
    {$endif}
  end
  else
    Result:=s;
end;

function AppendPathDelim(const Path: string): string;
begin
  if (Path<>'') and not (Path[length(Path)] in AllowDirectorySeparators) then
    Result:=Path+PathDelim
  else
    Result:=Path;
end;

function FileSize(aFile: string): Int64;
var
  SR: TSearchRec;
begin
  Result := -1;
  if SysUtils.FindFirst(aFile,faAnyFile,SR)=0 then
    Result := sr.Size;
  SysUtils.FindClose(SR);
end;

function CopyFile(const SrcFilename, DestFilename: string; Flags: TCopyFileFlags
  ): boolean;
var
  SrcHandle: THandle;
  DestHandle: THandle;
  Buffer: array[1..4096] of byte;
  ReadCount, WriteCount, TryCount: LongInt;
begin
  Result := False;
  // check overwrite
  if (not (cffOverwriteFile in Flags)) and FileExists(UniToSys(DestFileName)) then
    exit;
  // check directory
  if (cffCreateDestDirectory in Flags)
  and (not DirectoryExists(UniToSys(ExtractFilePath(DestFileName))))
  and (not ForceDirectories(UniToSys(ExtractFilePath(DestFileName)))) then
    exit;
  TryCount := 0;
  While TryCount <> 3 Do Begin
    SrcHandle := FileOpen(UniToSys(SrcFilename), fmOpenRead or fmShareDenyWrite);
    if (THandle(SrcHandle)=feInvalidHandle) then Begin
      Inc(TryCount);
      Sleep(10);
    End
    Else Begin
      TryCount := 0;
      Break;
    End;
  End;
  If TryCount > 0 Then
    raise EFOpenError.Createfmt({SFOpenError}'Unable to open file "%s"', [SrcFilename]);
  try
    DestHandle := FileCreate(UniToSys(DestFileName));
    if (THandle(DestHandle)=feInvalidHandle) then
      raise EFCreateError.createfmt({SFCreateError}'Unable to create file "%s"',[DestFileName]);
    try
      repeat
        ReadCount:=FileRead(SrcHandle,Buffer[1],High(Buffer));
        if ReadCount<=0 then break;
        WriteCount:=FileWrite(DestHandle,Buffer[1],ReadCount);
        if WriteCount<ReadCount then
          raise EWriteError.createfmt({SFCreateError}'Unable to write to file "%s"',[DestFileName])
      until false;
    finally
      FileClose(DestHandle);
    end;
    if (cffPreserveTime in Flags) then
      FileSetDate(UnitoSys(DestFilename), FileGetDate(SrcHandle));
    Result := True;
  finally
    FileClose(SrcHandle);
  end;
end;

function CanWriteToProgramDir : Boolean;
var
  f : TextFile;
begin
  AssignFile(f,ExtractFilePath(Paramstr(0))+'writetest.tmp');
  try
    Rewrite(f);
  except
    Result := False;
    exit;
  end;
  CloseFile(f);
  SysUtils.DeleteFile(ExtractFilePath(Paramstr(0))+'writetest.tmp');
  Result := True;
end;

function GetHomeDir: string;
{$IFDEF MSWINDOWS}
const
  CSIDL_PERSONAL = $0005;
  CSIDL_FLAG_CREATE     = $8000; { (force creation of requested folder if it doesn't exist yet)     }
var
  Path: array [0..1024] of char;
  P : Pointer;
  SHGetFolderPath : PFNSHGetFolderPath = Nil;
  CFGDLLHandle : THandle = 0;
{$ENDIF}
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
  Result := ExtractFilePath(Paramstr(0));
  If (@ShGetFolderPath<>Nil) then
    begin
      if SHGetFolderPath(0,CSIDL_PERSONAL or CSIDL_FLAG_CREATE,0,0,@PATH[0])=S_OK then
        Result:=IncludeTrailingPathDelimiter(StrPas(@Path[0]));
    end;
{$ELSE}
  Result:=expandfilename('~/');
{$ENDIF}
end;

function SizeToText(size : Longint) : string;
begin
  if size > 1024*1024*1024 then
    Result := FormatFloat('0.00',size/(1024*1024*1024))+' Gb'
  else if size > 1024*1024 then
    Result := FormatFloat('0.00',size/(1024*1024))+' Mb'
  else if size > 1024 then
    Result := FormatFloat('0.00',size/(1024))+' Kb'
  else
    Result := IntToStr(size)+' byte'
end;
function GetMainIconHandle(Resourcename : string) : Cardinal;
begin
{$ifdef MSWINDOWS}
  Result := LoadIcon(hInstance,PChar(Resourcename));
{$else}
  Result := 0;
{$endif}
end;
function GetGlobalConfigDir(app : string;Global : Boolean = True) : string;
{$IFDEF MSWINDOWS}
const
  CSIDL_COMMON_APPDATA  = $0023; // All Users\Application Data
  CSIDL_LOCAL_APPDATA   = $001c;
  CSIDL_FLAG_CREATE     = $8000; { (force creation of requested folder if it doesn't exist yet)     }
var
  Path: array [0..1024] of char;
  P : Pointer;
  SHGetFolderPath : PFNSHGetFolderPath = Nil;
  CFGDLLHandle : THandle = 0;
{$ENDIF}
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
  Result := ExtractFilePath(Paramstr(0));
  If (@ShGetFolderPath<>Nil) then
    begin
      if Global then
        begin
          if SHGetFolderPath(0,CSIDL_COMMON_APPDATA or CSIDL_FLAG_CREATE,0,0,@PATH[0])=S_OK then
            Result:=IncludeTrailingPathDelimiter(StrPas(@Path[0]))+app;
        end
      else
        begin
          if SHGetFolderPath(0,CSIDL_LOCAL_APPDATA or CSIDL_FLAG_CREATE,0,0,@PATH[0])=S_OK then
            Result:=IncludeTrailingPathDelimiter(StrPas(@Path[0]))+app;
        end;
    end;
{$ELSE}
  Result:=GetEnvironmentVariable('HOME');
  If (Result<>'') then
    Result:=IncludeTrailingPathDelimiter(Result)+'.'+app;
{$ENDIF}
  Result := IncludeTrailingPathDelimiter(result);
end;
function GetConfigDir(app : string) : string;
begin
  Result := GetGlobalConfigDir(app,False);
  if Result = DirectorySeparator then
    Result := '';
end;
function GetProgramDir : string;
{$IFDEF MSWINDOWS}
const
    CSIDL_PROGRAM_FILES             = $0026;        // C:\Program Files
var
  Path: array [0..1024] of char;
  P : Pointer;
  SHGetFolderPath : PFNSHGetFolderPath = Nil;
  CFGDLLHandle : THandle = 0;
{$ENDIF}
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
  Result := ExtractFilePath(Paramstr(0));
  If (@ShGetFolderPath<>Nil) then
    if SHGetFolderPath(0,CSIDL_PROGRAM_FILES,0,0,@PATH[0])=S_OK then
      Result:=StrPas(@Path[0]);
{$ELSE}
{$ENDIF}
  Result := IncludeTrailingPathDelimiter(result);
end;
function GetTempPath : string;
{$IFDEF MSWINDOWS}
var
  TD                : PChar;
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
  GetMem(TD, 256);
  try
    FillChar(TD^, 256, 0);
    Windows.GetTempPath(256, TD);
    Result := TD;
  finally
    FreeMem(TD, 256);
  end;
{$ELSE}
  Result := '/tmp';
{$ENDIF}
end;
function ValidateFileDir(old: string): string;
begin
  Result := old;
  if DirectorySeparator <> '/' then
    Result := StringReplace(Result,'/','',[rfReplaceAll]);
  Result := StringReplace(Result,'@','',[rfReplaceAll]);
  Result := StringReplace(Result,';','',[rfReplaceAll]);
end;
function ValidateDate(D : string) : string;
begin
  if pos('.',D) > 0 then
    Result := StringReplace(D,'-','.',[rfReplaceAll]);
  if length(D) = 4 then
    Result := '01.01.'+D;
end;
function ValidateFileName(old : string) : string;
begin
  Result := StringReplace(old,'\','',[rfReplaceAll]);
  Result := StringReplace(Result,'/','',[rfReplaceAll]);
  Result := StringReplace(Result,'@','',[rfReplaceAll]);
  Result := StringReplace(Result,';','',[rfReplaceAll]);
  Result := StringReplace(Result,'#','_',[rfReplaceAll]);
  Result := StringReplace(Result,'>','_',[rfReplaceAll]);
  Result := StringReplace(Result,'<','_',[rfReplaceAll]);
  Result := StringReplace(Result,'|','_',[rfReplaceAll]);
  Result := StringReplace(Result,'"','_',[rfReplaceAll]);
  Result := StringReplace(Result,':','_',[rfReplaceAll]);
  Result := StringReplace(Result,'*','_',[rfReplaceAll]);
  Result := StringReplace(Result,'?','_',[rfReplaceAll]);
  Result := StringReplace(Result,'&','',[rfReplaceAll]);
  Result := StringReplace(Result,'(','_',[rfReplaceAll]);
  Result := StringReplace(Result,')','_',[rfReplaceAll]);
  Result := StringReplace(Result,'ö','oe',[rfReplaceAll]);
  Result := StringReplace(Result,'ä','ae',[rfReplaceAll]);
  Result := StringReplace(Result,'ü','ue',[rfReplaceAll]);
  Result := StringReplace(Result,'Ö','Oe',[rfReplaceAll]);
  Result := StringReplace(Result,'Ä','Ae',[rfReplaceAll]);
  Result := StringReplace(Result,'Ü','Ue',[rfReplaceAll]);
  Result := StringReplace(Result,'ß','ss',[rfReplaceAll]);
end;
function StripHTML(S: string): string;
var
  TagBegin, TagEnd, TagLength: integer;
begin
  S := Stringreplace(S,'<br>',#13,[rfReplaceAll]);
  TagBegin := Pos( '<', S);      // search position of first <

  while (TagBegin > 0) do begin  // while there is a < in S
    TagEnd := Pos('>', S);              // find the matching >
    TagLength := TagEnd - TagBegin + 1;
    if Taglength <= 0 then break;
    Delete(S, TagBegin, TagLength);     // delete the tag
    TagBegin:= Pos( '<', S);            // search for next <
  end;

  S := Stringreplace(S,'&nbsp;',' ',[rfReplaceAll]);
  S := Stringreplace(S,'&amp;','&',[rfReplaceAll]);
  S := Stringreplace(S,'&lt;','<',[rfReplaceAll]);
  S := Stringreplace(S,'&gt;','>',[rfReplaceAll]);
  S := Stringreplace(S,'&quot;','"',[rfReplaceAll]);
  Result := HTMLDecode(S);                   // give the result
end;
function GetSystemName: string;
{$IFDEF MSWINDOWS}
var
  ComputerNameBuffer: array[0..255] of char;
  sizeBuffer: DWord;
{$ENDIF}
begin
  {$IFDEF MSWINDOWS}
  SizeBuffer := 256;
  GetComputerName(ComputerNameBuffer, sizeBuffer);
  Result := string(ComputerNameBuffer);
  {$ELSE}
  Result := GetHostName;
  {$ENDIF}
end;
function HTTPEncode(const str : String) : string;
const
  noconvert = ['A'..'Z','a'..'z','*','@','.','_','-','0'..'9','$','!','''','(',')'];
  hex2str : array[0..15] of char = '0123456789ABCDEF';
var
  i : integer;
  c : char;
begin
  Result := '';
  for i:=1 to length(str) do
    begin
      c:=str[i];
      if c in noconvert then
        Result:=Result+c
      else
        Result:=Result+'%'+hex2str[ord(c) shr 4]+hex2str[ord(c) and $f];
    end;
end;
{$IFDEF MSWINDOWS}
function SystemUserName : string;
var
  buffer : array[0..MAX_PATH] of Char;
  Size: DWORD;
  FUserName: WideString;
begin
  Size := sizeof(buffer);
  GetUserName(buffer, Size);
  SetString(FUserName, buffer, lstrlen(buffer));
  Result := FUserName;
end;
{$ENDIF}
{$IFNDEF WINDOWS}
function SystemUserName : string;
begin
  Result := GetEnvironmentVariable('USERNAME');
  if Result = '' then
    Result := GetEnvironmentVariable('USER');
end;
{$ENDIF}
{$IFDEF MSWINDOWS}
function GetAdminSid: PSID;
const
  // bekannte SIDs ... (WinNT.h)
  SECURITYNTAUTHORITY: TSIDIdentifierAuthority = (Value: (0, 0, 0, 0, 0, 5));
  // bekannte RIDs ... (WinNT.h)
  SECURITYBUILTINDOMAINRID: DWORD = $00000020;
  DOMAINALIASRIDADMINS: DWORD = $00000220;
begin
  Result := nil;
  AllocateAndInitializeSid(SECURITYNTAUTHORITY,
    2,
    SECURITYBUILTINDOMAINRID,
    DOMAINALIASRIDADMINS,
    0,
    0,
    0,
    0,
    0,
    0,
    Result);
end;
//----von Mathias Simmacks "IsAdmin.inc" (TFileTypeRegistration.zip) geklaut:
function IsAdmin: LongBool;
var
  TokenHandle: THandle;
  ReturnLength: DWORD;
  TokenInformation: PTokenGroups;
  AdminSid: PSID;
  Loop: Integer;
  wv: TOSVersionInfo;
begin
  wv.dwOSVersionInfoSize := sizeof(TOSversionInfo);
  GetVersionEx(wv);

  Result := (wv.dwPlatformId = VER_PLATFORM_WIN32_WINDOWS);

  if (wv.dwPlatformId = VER_PLATFORM_WIN32_NT) then
  begin
    TokenHandle := 0;
    if OpenProcessToken(GetCurrentProcess, TOKEN_QUERY, TokenHandle) then
    try
      ReturnLength := 0;
      GetTokenInformation(TokenHandle, TokenGroups, nil, 0, ReturnLength);
      TokenInformation := GetMemory(ReturnLength);
      if Assigned(TokenInformation) then
      try
        if GetTokenInformation(TokenHandle, TokenGroups,
          TokenInformation, ReturnLength, ReturnLength) then
        begin
          AdminSid := GetAdminSid;
          for Loop := 0 to TokenInformation^.GroupCount - 1 do
          begin
            if EqualSid(TokenInformation^.Groups[Loop].Sid, AdminSid) then
            begin
              Result := True; break;
            end;
          end;
          FreeSid(AdminSid);
        end;
      finally
        FreeMemory(TokenInformation);
      end;
    finally
      CloseHandle(TokenHandle);
    end;
  end;
end;
{$ENDIF}
function InstallExt(Extension, ExtDescription, FileDescription,OpenWith, ParamString: string; IconIndex: Integer = 0): Boolean;
{$IFDEF MSWINDOWS}
const
  SHCNE_ASSOCCHANGED = $8000000;
  SHCNF_IDLIST       = $0000;
var
  Reg: TRegistry;
{$ENDIF}
begin
  Result := False;
  if Extension <> '' then
    begin
{$IFDEF MSWINDOWS}
      if Extension[1] <> '.' then
        Extension := '.' + Extension;
      Reg := TRegistry.Create;
      try
        Reg.RootKey := HKEY_CLASSES_ROOT;
        if Reg.OpenKey(Extension, True) then
          begin
            Reg.WriteString('', ExtDescription);
            if Reg.OpenKey('\' + ExtDescription, True) then
              begin
                Reg.WriteString('', FileDescription);
                if Reg.OpenKey('DefaultIcon', True) then
                  begin
                    Reg.WriteString('', Format('%s,%d', [OpenWith, IconIndex]));
                    if Reg.OpenKey('\' + ExtDescription + '\Shell\Open\Command', True) then
                      begin
                        Reg.WriteString('', Format('"%s" "%s"', [OpenWith, ParamString]));
                        Result:=True;
                      end;
                  end;
              end
            else
              begin
                Reg.RootKey:=HKEY_CURRENT_USER;
                if Reg.OpenKey('Software\Classes\', True) then
                  begin
                    if Reg.OpenKey(Extension, True) then
                      begin
                        Reg.WriteString('', ExtDescription);
                        if Reg.OpenKey('\' + ExtDescription, True) then
                          begin
                            Reg.WriteString('', FileDescription);
                            if Reg.OpenKey('DefaultIcon', True) then
                              begin
                                Reg.WriteString('', Format('%s,%d', [OpenWith, IconIndex]));
                                if Reg.OpenKey('\' + ExtDescription + '\Shell\Open\Command', True) then
                                  begin
                                    Reg.WriteString('', Format('"%s" "%s"', [OpenWith, ParamString]));
                                    Result:=True;
                                  end;
                              end;
                          end;
                      end
                  end;
              end;
          end;
      finally
        Reg.Free;
      end;
      SHChangeNotify(SHCNE_ASSOCCHANGED, SHCNF_IDLIST, nil, nil);
{$ENDIF}
    end;
end;
function StrTimeToValue(val: string): LongInt;
var
  i : Integer;
  un : string;
begin
  //TODO:replace ',' with system delemiter
  un := '';
  FOR i := 1 TO length(val) DO
    IF NOT ((Char(Val[i]) IN ['0'..'9']) or (Char(Val[i]) = DecimalSeparator)) THEN
      begin
        un := trim(copy(Val,i,length(Val)));
        break;
      end;
  if copy(Val,0,i-1) = '' then
    begin
      Result := -1;
      exit;
    end;
  if (UpperCase(un) = 'MS') or (un = '') then
    Result := Round(StrToFloat(copy(Val,0,i-1)))
  else if UpperCase(un) = 'S' then
    Result := Round(1000*StrToFloat(copy(Val,0,i-1)))
  else if UpperCase(un) = 'M' then
    Result := Round(60*1000*StrToFloat(copy(Val,0,i-1)))
  else
    Result := -1;
end;
function IsNumeric(s: STRING): boolean;
var
  i : integer;
begin
  if copy(s,0,1) = '-' then
    s := copy(s,2,length(s));
  Result := length(s) > 0;
 for i:= 0 to 47 do
   begin
     if (pos(chr(i),s) > 0) then
       begin
         result := false;
       end;
   end;
 for i := 58 to 255 do
   begin
     if (pos(chr(i),s)>0) then
       begin
          result := false;
       end
   end;
end;
function RPos(const Substr: string; const S: string): Integer;
var
  SL, i : Integer;
begin
  SL := Length(Substr);
  i := Length(S);
  if (Substr = '') or (S = '') or (SL > i) then begin
    Result := 0;
    Exit;
  end;

  while i >= SL do begin
    if S[i] = Substr[SL] then begin
      if Copy(S, i - SL + 1, SL) = Substr then begin
        Result := i - SL + 1;
        Exit;
      end;
    end;
    Dec(i);
  end;
  Result := i;
end;
 { Make sure given file path is ended with backslash ("\") }
 { Clears Directory: Removes all files and directories contained }
function ClearDir (Path: string): boolean;
var
  Res: integer;
  SRec: SysUtils.TSearchRec;
begin
  Result := false;
  try
    if copy(path,length(path)-1,1) <> DirectorySeparator then
      Path := Path+DirectorySeparator;
    Res := FindFirst (Path + '*.*', faAnyFile, SRec);
    while Res = 0 do
      begin
        if (SRec.Attr = faDirectory) and (SRec.Name[1] <> '.') then
          begin
            ClearDir (Path + SRec.Name); { Clear before removing }
            if not RemoveDir (pchar(Path + SRec.Name)) then
              exit;
          end
        else
          SysUtils.DeleteFile(Path + SRec.Name);
      Res := FindNext(SRec);
    end;
    SysUtils.FindClose(SRec);
    Result := true;
  except
  end;
end;
END.

 
