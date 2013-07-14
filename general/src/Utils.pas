UNIT Utils;
INTERFACE
{$H+}
uses Classes,SysUtils
     {$IFDEF LCL}
     {$IFNDEF LCLnogui}
     ,Forms,Dialogs,Clipbrd,Translations,TypInfo,LCLProc,Graphics
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
 TProcessinfoTyp = (piOpen,piPrint);
 {$ifdef WINDOWS}
 PFNSHGetFolderPath = Function(Ahwnd: HWND; Csidl: Integer; Token: THandle; Flags: DWord; Path: PChar): HRESULT; stdcall;
 {$endif}

function ClearDir (Path: string): boolean;
function RPos(const Substr: string; const S: string): Integer;
FUNCTION IsNumeric(s: STRING): boolean;
FUNCTION StrTimeToValue(val : string) : LongInt;
{$IFDEF LCL}
 {$IFNDEF LCLnogui}
 procedure LoadLanguage(lang : string);
 function GetProcessforExtension(InfoTyp : TProcessinfoTyp;Extension : string) : string;
 function TextCut(aCanvas: TCanvas; Len: Integer; Text: String): String;
 {$ELSE}
 function TextCut(Len: Integer; Text: String): String;
 {$ENDIF}
{$ENDIF}
function GetMimeTypeforExtension(Extension : string) : string;
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
function SizeToText(size : Longint) : string;
function GetMainIconHandle(Resourcename : string) : Cardinal;
function CanWriteToProgramDir : Boolean;
function HexToBin(h: STRING): dword;
function RoundTo(const AValue : extended ; const ADigit : TRoundToRange) : extended ;
function TimeTotext(Seconds : Integer) : string;
function GetSystemLang : string;
function DateTimeToHourString(DateTime : TDateTime) : string;
function DateTimeToIndustrialTime(dateTime : TDateTime) : string;
function ConvertUnknownStringdate(input : string) : TDateTime;
function HTMLEncode(s : string)  : string;
function HTMLDecode(s : string)  : string;
IMPLEMENTATION
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
{$IFNDEF LCLnogui}
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
{$endif ver2_0}
function TextCut(aCanvas: TCanvas; Len: Integer; Text: String): String;
var
  k: Integer;
begin
  Result := '';
  if Len < 0 then exit;
  if Len <= aCanvas.TextWidth(Copy(Text, 1, 1) + '...') then exit;
  Result := Text;
  with aCanvas do
    begin
      if TextWidth(Text) > Len then
        begin
          for k := Length(Text) downto 1 do
            if TextWidth(Copy(Text, 1, k) + '...') > Len then Continue
              else
            begin
              Result := Copy(Text, 1, k) + '...';
              Exit;
            end;
        end;
    end;
end;
procedure LoadLanguage(lang: string);
var
  Info : TSearchRec;
  po: TPOFile;
  units: TStringList;
  id: String;
  i: Integer;
  a: Integer;
  Comp: TComponent;
Begin
  If FindFirstUTF8(ProgramDirectory+'languages'+Directoryseparator+'*.'+lowercase(copy(lang,0,2))+'.po',faAnyFile,Info)=0 then
    repeat
      po := TPOFile.Create(ProgramDirectory+'languages'+Directoryseparator+Info.Name);
      units := TStringList.Create;
      for i := 0 to po.Items.Count-1 do
        begin
          id := copy(TPoFileItem(po.Items[i]).IdentifierLow,0,pos('.',TPoFileItem(po.Items[i]).IdentifierLow)-1);
          if units.IndexOf(id) = -1 then
            units.Add(id);
        end;
      for i := 0 to units.Count-1 do
        Translations.TranslateUnitResourceStrings(units[i],ProgramDirectory+'languages'+Directoryseparator+Info.Name);
      units.Free;
      for i := 0 to po.Items.Count-1 do
        begin
          id := copy(TPoFileItem(po.Items[i]).IdentifierLow,0,pos('.',TPoFileItem(po.Items[i]).IdentifierLow)-1);
          for a := 0 to Screen.FormCount-1 do
            if UTF8UpperCase(Screen.Forms[a].ClassName) = UTF8UpperCase(id) then
              begin
                id := copy(TPoFileItem(po.Items[i]).IdentifierLow,pos('.',TPoFileItem(po.Items[i]).IdentifierLow)+1,length(TPoFileItem(po.Items[i]).IdentifierLow));
                if Assigned(Screen.Forms[a].FindComponent(copy(id,0,pos('.',id)-1))) then
                  begin
                    Comp := Screen.Forms[a].FindComponent(copy(id,0,pos('.',id)-1));
                    id := copy(id,pos('.',id)+1,length(id));
                    SetStrProp(Comp,id,TPoFileItem(po.Items[i]).Translation);
                  end;
              end;
        end;
      po.Free;
    until FindNextUTF8(info)<>0;
  FindCloseUTF8(Info);
end;
function GetProcessforExtension(InfoTyp : TProcessinfoTyp;Extension : string) : string;
var
{$ifdef MSWINDOWS}
  reg : TRegistry;
  ot : string;
  FileClass: string;
  chrResult: array[0..1023] of Char;
  wrdReturn: DWORD;
{$else}
  SRec : TSearchRec;
  res : Integer;
  f : TextFile;
  tmp : string;
  mime : string;
  apps : string;
{$endif}
begin
{$ifdef WINDOWS}
  case InfoTyp of
  piOpen:ot := 'open';
  piPrint:ot := 'print';
  end;
  Result := '';
  Reg := TRegistry.Create(KEY_READ);
  Reg.RootKey := HKEY_CLASSES_ROOT;
  FileClass := '';
  if Reg.OpenKeyReadOnly(ExtractFileExt('.'+Extension)) then
  begin
    FileClass := Reg.ReadString('');
    Reg.CloseKey;
  end;
  if FileClass <> '' then begin
    if Reg.OpenKeyReadOnly(FileClass + '\Shell\'+ot+'\Command') then
    begin
      wrdReturn := ExpandEnvironmentStrings(PChar(StringReplace(Reg.ReadString(''),'%1','%s',[rfReplaceAll])), chrResult, 1024);
      if wrdReturn = 0 then
        Result := StringReplace(Reg.ReadString(''),'%1','%s',[rfReplaceAll])
      else
        Result := Trim(chrResult);
      Reg.CloseKey;
    end;
  end;
  Reg.Free;
{$ELSE}
  apps := '';
  mime := GetMimeTypeforExtension(Extension);
//  /usr/share/mime-info *.keys
  Res := FindFirst ('/usr/share/mime-info/*.keys', faAnyFile, SRec);
  while Res = 0 do
    begin
      AssignFile(f,'/usr/share/mime-info/'+SRec.Name);
      Reset(f);
      while not eof(f) do
        begin
          readln(f,tmp);
// nicht eingerueckt ist der mime typ
          if not ((copy(tmp,0,1) = ' ') or (copy(tmp,0,1) = #9)) then
//eingerÃckt die eigenschaften
            if ((copy(tmp,length(tmp)-2,1) = '*')
            and (copy(tmp,0,length(tmp)-2) = copy(mime,0,length(tmp)-2)))
            or (trim(tmp) = trim(mime)) then
              begin
                readln(f,tmp);
                while (not eof(f)) and ((copy(tmp,0,1) = ' ') or (copy(tmp,0,1) = #9)) do
                  begin
                    tmp := StringReplace(trim(tmp),#9,'',[rfReplaceAll]);
//open referenziert gleich das program
                    if lowercase(copy(tmp,0,5)) = 'open=' then
                      begin
                        Result := copy(tmp,6,length(tmp));
                        if pos('%f',Result) = 0 then
                          Result := Result+' "%s"'
                        else
                          Stringreplace(Result,'%f','%s',[rfReplaceAll]);
                        SysUtils.FindClose(SRec);
                        exit;
                      end
//das referenziert ein kÃrzel das isn der application registry steht
                    else if lowercase(copy(tmp,0,49)) = 'short_list_application_ids_for_novice_user_level=' then
                      begin
                        apps := copy(tmp,50,length(tmp));
                        break;
                      end;
                    readln(f,tmp);
                  end;
              end;
          if apps <> '' then break;
        end;
      CloseFile(f);
      Res := FindNext(SRec);
      if apps <> '' then break;
    end;
  SysUtils.FindClose(SRec);
  Result := apps;
  if apps <> '' then
    begin
      while pos(',',apps) > 0 do
        begin
          Res := FindFirst ('/usr/share/application-registry/*.applications', faAnyFile, SRec);
          while Res = 0 do
            begin
              AssignFile(f,'/usr/share/application-registry/'+SRec.Name);
              Reset(f);
              while not eof(f) do
                begin
                  readln(f,tmp);
                  if not ((copy(tmp,0,1) = ' ') or (copy(tmp,0,1) = #9)) then
    //eingerÃckt die eigenschaften
                    if trim(tmp) = copy(apps,0,pos(',',apps)-1) then
                      begin
                        readln(f,tmp);
                        while (not eof(f)) and ((copy(tmp,0,1) = ' ') or (copy(tmp,0,1) = #9)) do
                          begin
                            tmp := StringReplace(trim(tmp),#9,'',[rfReplaceAll]);
                            if lowercase(copy(tmp,0,8)) = 'command=' then
                              begin
                                Result := copy(tmp,9,length(tmp));
                                if FindFilenameOfCmd(Result) <> '' then
                                  begin
                                    if pos('%f',Result) = 0 then
                                      Result := Result+' "%s"'
                                    else
                                      Stringreplace(Result,'%f','%s',[rfReplaceAll]);

                                    CloseFile(f);
                                    exit;
                                  end;
                              end;
                            readln(f,tmp);
                          end;
                      end;
                end;
              CloseFile(f);
              Res := FindNext(SRec);
            end;
          apps := copy(apps,pos(',',apps)+1,length(apps));
        end;
    end;
  if Result='' then
    Result:=FindFilenameOfCmd('xdg-open')+' "%s"'; // Portland OSDL/FreeDesktop standard on Linux
  if Result='' then
    Result:=FindFilenameOfCmd('kfmclient')+' "%s"'; // KDE command
  if Result='' then
    Result:=FindFilenameOfCmd('gnome-open')+' "%s"'; // GNOME command
{$endif}
end;
{$ELSE}
function TextCut(Len: Integer; Text: String): String;
begin
  if Len < length(Text) then
    Result := copy(Text,0,len-3)+'...'
  else
    Result := Text;
end;
{$ENDIF}
{$ENDIF}
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
function DateTimeToIndustrialTime(DateTime: TDateTime): string;
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
function HTMLEncode(s: string): string;
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
function HTMLDecode(s: string): string;
begin
  Result := s;
  Result := StringReplace(Result, '&amp;'  ,'&', [rfreplaceall]);
  Result := StringReplace(Result, '&quot;' ,'"', [rfreplaceall]);
  Result := StringReplace(Result, '&lt;'   ,'<', [rfreplaceall]);
  Result := StringReplace(Result, '&gt;'   ,'>', [rfreplaceall]);
  Result := StringReplace(Result, '&nbsp;' ,' ', [rfreplaceall]);
  Result := StringReplace(Result, '&auml;' ,'ä', [rfreplaceall]);
  Result := StringReplace(Result, '&ouml;' ,'ö', [rfreplaceall]);
  Result := StringReplace(Result, '&uuml;' ,'ü', [rfreplaceall]);
  Result := StringReplace(Result, '&Auml;' ,'Ä', [rfreplaceall]);
  Result := StringReplace(Result, '&Ouml;' ,'Ö', [rfreplaceall]);
  Result := StringReplace(Result, '&Uuml;' ,'Ü', [rfreplaceall]);
  Result := StringReplace(Result, '&szlig;','ß', [rfreplaceall]);
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
FUNCTION StrTimeToValue(val : string) : LongInt;
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
FUNCTION IsNumeric(s: STRING): boolean;
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

 