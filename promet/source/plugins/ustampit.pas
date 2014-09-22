unit ustampit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,  Forms, Controls, Dialogs, ProcessUtils, Process,
  Windows,Strutils,Utils;

type

  { TDataModule1 }

  TDataModule1 = class(TDataModule)
    procedure DataModuleCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

  PFNSHGetFolderPath = Function(Ahwnd: HWND; Csidl: Integer; Token: THandle; Flags: DWord; Path: PChar): HRESULT; stdcall;

var
  DataModule1: TDataModule1; 
  SHGetFolderPath : PFNSHGetFolderPath = Nil;
  CFGDLLHandle : THandle = 0;

function EnumProc(wnd: HWND; x : Longint): BOOL; stdcall;

implementation
{$R *.lfm}
uses FileUtil;

Const
  CSIDL_PROGRAMS                = $0002; { %SYSTEMDRIVE%\Program Files                                      }
  CSIDL_APPDATA                 = $001A; { %USERPROFILE%\Application Data (roaming)                         }
  CSIDL_LOCAL_APPDATA           = $001C; { %USERPROFILE%\Local Settings\Application Data (non roaming)      }
  CSIDL_COMMON_APPDATA          = $0023; { %PROFILESPATH%\All Users\Application Data                        }
  CSIDL_PROGRAM_FILES           = $0026; { %SYSTEMDRIVE%\Program Files                                      }
  CSIDL_FLAG_CREATE             = $8000; { (force creation of requested folder if it doesn't exist yet)     }

function EnumProc1(wnd: HWND; x : Longint): BOOL; stdcall;
begin
  Result := EnumProc(wnd,x);
end;

function EnumProc(wnd: HWND; x : Longint): BOOL; stdcall;
var
  buf, Caption: array[0..255] of char;
begin
  Result := True;
  GetClassName(wnd, buf, SizeOf(buf) - 1);
  SendMessage(wnd, WM_GETTEXT, 256, Integer(@Caption));
  if StrComp(buf,'ComboBox') = 0 then
    begin
      if TStrings(x).IndexOf(IntToStr(wnd)) = -1 then
        TStrings(x).Add(IntToStr(wnd));
    end
  else EnumChildWindows(wnd, @EnumProc1, x);
end;

procedure InitSpecialDirs;
var
  P: Pointer;
begin
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
end;

Function GetSpecialDir(ID :  Integer) : String;
Var
  APath : Array[0..MAX_PATH] of char;
begin
  Result:='';
  if (CFGDLLHandle=0) then
    InitSpecialDirs;
  If (SHGetFolderPath<>Nil) then
    begin
    if SHGetFolderPath(0,ID or CSIDL_FLAG_CREATE,0,0,@APATH[0])=S_OK then
      Result:=IncludeTrailingPathDelimiter(StrPas(@APath[0]));
    end;
end;

{ TDataModule1 }

procedure TDataModule1.DataModuleCreate(Sender: TObject);
var
  Process : TProcess;
  StampItHandle: LongWord;
  CB1Handle: LongWord;
  ExplorerHandle: LongWord;
  WindowList : TStringList;
  i: Integer;
  tmp: String;
  ItemCount: LongInt;
  r : TRect;
  fs: TFileStream;
  ss: TStringStream;
  Address: TStringList;
  PrinterIndex: LongInt;
begin
  Address := TStringList.Create;
  tmp:=Application.GetOptionValue('address');
  while pos(',',tmp) > 0 do
    begin
      Address.Add(copy(tmp,0,pos(',',tmp)-1));
      tmp := copy(tmp,pos(',',tmp)+1,length(tmp));
    end;
  Address.Add(tmp);
  WindowList := TStringList.Create;
  fs := TFileStream.Create(ExtractFilePath(Application.Exename)+'TmpAddresses.TMP',fmOpenread);
  SetLength(tmp, fs.Size);
  fs.ReadBuffer(tmp[1], fs.Size);
  if Address.Count > 0 then
    tmp := StringReplace(tmp,'%ADDRESS1%',UTF8ToSys(Address[0]),[rfReplaceAll])
  else
    tmp := StringReplace(tmp,'%ADDRESS1%','',[rfReplaceAll]);
  if Address.Count > 1 then
    tmp := StringReplace(tmp,'%ADDRESS2%',UTF8ToSys(Address[1]),[rfReplaceAll])
  else
    tmp := StringReplace(tmp,'%ADDRESS2%','',[rfReplaceAll]);
  if Address.Count > 2 then
    tmp := StringReplace(tmp,'%ADDRESS3%',UTF8ToSys(Address[2]),[rfReplaceAll])
  else
    tmp := StringReplace(tmp,'%ADDRESS3%','',[rfReplaceAll]);
  if Address.Count > 3 then
    tmp := StringReplace(tmp,'%ADDRESS4%',UTF8ToSys(Address[3]),[rfReplaceAll])
  else
    tmp := StringReplace(tmp,'%ADDRESS4%','',[rfReplaceAll]);
  if Address.Count > 4 then
    tmp := StringReplace(tmp,'%ADDRESS5%',UTF8ToSys(Address[4]),[rfReplaceAll])
  else
    tmp := StringReplace(tmp,'%ADDRESS5%','',[rfReplaceAll]);
  if Address.Count > 5 then
    tmp := StringReplace(tmp,'%ADDRESS6%',UTF8ToSys(Address[5]),[rfReplaceAll])
  else
    tmp := StringReplace(tmp,'%ADDRESS6%','',[rfReplaceAll]);
  ss := TStringStream.Create(tmp);
  fs.Free;
  fs := TFileStream.Create(Utils.GetTempPath+'TmpAddresses.TMP',fmCreate);
  ss.Position:=0;
  fs.CopyFrom(ss,ss.Size);
  fs.Free;

  Process := TProcess.Create(nil);
  Process.ShowWindow := swoShowMinimized;
  Process.Options:= [poNoConsole,poNewProcessGroup];
  if Application.HasOption('packet') then
    Process.CommandLine := '"'+GetSpecialDir(CSIDL_PROGRAM_FILES)+'\STAMPIT\Binary\SBAPPNG.EXE" /p41'
  else
    Process.CommandLine := '"'+GetSpecialDir(CSIDL_PROGRAM_FILES)+'\STAMPIT\Binary\SBAPPNG.EXE" /p28';
  PrinterIndex := StrToIntDef(Application.GetOptionValue('p','printer'),0);
  Process.Execute;
  StampItHandle := 0;
  while StampItHandle = 0 do
    begin
      StampItHandle := FindWindow(nil,'STAMPIT');
      Application.ProcessMessages;
    end;
  // /p41 = Paket
  // /p28 = Brief
  //C:\Programme\STAMPIT\Binary\SBAPPNG.EXE /p4
  Application.Terminate;
end;

initialization
end.
