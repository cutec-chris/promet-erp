[Setup]
AppName=Promet-ERP
AppId=prometerp
AppVersion=7.1.0
AppPublisher=Christian Ulrich
AppPublisherURL=http://www.cu-tec.de
AppSupportURL=http://www.free-erp.de
AppUpdatesURL=http://www.free-erp.de
MinVersion=0,5.0
DefaultDirName={pf}\Promet-ERP
DefaultGroupName=Promet-ERP
AllowNoIcons=yes
Compression=lzma2/ultra
InternalCompressLevel=ultra
SolidCompression=yes
Uninstallable=True
UninstallDisplayIcon={app}\Compil32.exe
WizardImageFile=compiler:WizModernImage-IS.bmp
WizardSmallImageFile=compiler:WizModernSmallImage-IS.bmp

[Files]
Source: "isxdl.dll"; Flags: dontcopy
Source: "icons\*.ico"; Flags: dontcopy
Source: "..\..\resources\multi-icon.ico"; Flags: dontcopy
Source: "unzip.exe"; DestDir: "{tmp}"; Flags: deleteafterinstall

[Dirs]
Name: "{app}"; Flags: uninsalwaysuninstall

[Components]
Name: "program"; Description: "Programdateien"; ExtraDiskSpaceRequired: 14600000; Types: full compact custom
Name: "help"; Description: "Hilfe Datenbank"; ExtraDiskSpaceRequired: 10000000; Types: full compact custom
Name: "visualtools"; Description: "Werkzeuge"; ExtraDiskSpaceRequired: 29100000; Types: full compact custom
Name: "win32tools"; Description: "PDF und OCR Unterstütung"; ExtraDiskSpaceRequired: 4553871; Types: full compact custom
Name: "mail"; Description: "e-Mail Unterstützung"; ExtraDiskSpaceRequired: 6831654; Types: full custom
Name: "feed"; Description: "Feed Unterstützung (Atom,RSS,Twitter)"; ExtraDiskSpaceRequired: 3354131; Types: full custom
Name: "clientsqlite"; Description: "SQLite Unterstützung"; ExtraDiskSpaceRequired: 681097; Types: full compact custom
Name: "clientpostgres"; Description: "Postgres Unterstützung"; ExtraDiskSpaceRequired: 7053908; Types: custom
Name: "clientmysql"; Description: "MySQL Unterstützung"; ExtraDiskSpaceRequired: 4465152; Types: full custom
Name: "sync"; Description: "Datenbanksynchronisation/Backup"; ExtraDiskSpaceRequired: 3261459; Types: full custom
Name: "statistics"; Description: "Reporting/Statistik"; ExtraDiskSpaceRequired: 12260883; Types: custom
Name: "meeting"; Description: "Besprechungsprotokoll"; ExtraDiskSpaceRequired: 11000000; Types: custom
;Name: "archive"; Description: "Archivprogramm (Revisionssichere Archivierung)"; Types: custom
Name: "tools"; Description: "Kommandozeilenprogramme"; ExtraDiskSpaceRequired: 3721043; Types: custom
Name: "xmpp"; Description: "Jabber/XMPP Unterstützung"; ExtraDiskSpaceRequired: 3301907; Types: custom
;Name: "dav"; Description: "Kalender/Kontaktserver (CalDAV,CardDAV)"; Types: custom
Name: "imap"; Description: "e-Mail Server (IMAP)"; ExtraDiskSpaceRequired: 3567635; Types: custom
Name: "web"; Description: "Webserver/Applikationsserver"; ExtraDiskSpaceRequired: 3165203; Types: custom
Name: "mqtt"; Description: "MQTT Unterstützung (IoT)"; ExtraDiskSpaceRequired: 3301907; Types: custom

[Icons]
Name: "{group}\Internet"; Filename: "{app}\website.url"
Name: "{userdesktop}\Promet-ERP"; Filename: "{app}\prometerp.exe"; Flags: createonlyiffileexists; Components: program; Tasks: desktopicon
Name: "{group}\Promet-ERP"; Filename: "{app}\prometerp.exe"; WorkingDir: "{app}"; Flags: createonlyiffileexists; Components: program
Name: "{group}\Promet-ERP Reporting"; Filename: "{app}\statistics.exe"; WorkingDir: "{app}"; Flags: createonlyiffileexists; Components: statistics
Name: "{group}\Promet-ERP Besprechung"; Filename: "{app}\meetingminutes.exe"; WorkingDir: "{app}"; Flags: createonlyiffileexists; Components: meeting

[Tasks]
Name: desktopicon; Description: Create an Desktop Icon; GroupDescription: Additional Icons:; Languages: en
Name: desktopicon; Description: Ein Desktop Icon erstellen; GroupDescription: Zusätzliche Icons:; Languages: de

[Languages]
Name: en; MessagesFile: compiler:Default.isl
Name: de; MessagesFile: German.isl

[Run]
Filename: "{tmp}\unzip.exe"; Parameters: "-o {tmp}\*.zip -d ""{app}"""; Flags: waituntilterminated shellexec runhidden; StatusMsg: "Installiere Promet-ERP..."

[UninstallDelete]
Type: filesandordirs; Name: "{app}\*.*"
Type: filesandordirs; Name: "{app}"

[Code]
var
  Modifying, AllowInnoIDE: Boolean;

  PrometPage, PostgresPage: TWizardPage;
  PrometCheckBox, PostgresCheckBox, MySQLCheckBox, FirebirdCheckBox: TCheckBox;
  IDEOrg: Boolean;

  FilesDownloaded: Boolean;

  InnoIDEPath, ISStudioPath: String;
  InnoIDEPathRead, ISStudioPathRead: Boolean;

procedure isxdl_AddFile(URL, Filename: AnsiString);
external 'isxdl_AddFile@files:isxdl.dll stdcall';
function isxdl_DownloadFiles(hWnd: Integer): Integer;
external 'isxdl_DownloadFiles@files:isxdl.dll stdcall';
function isxdl_SetOption(Option, Value: AnsiString): Integer;
external 'isxdl_SetOption@files:isxdl.dll stdcall';

function GetModuleHandle(lpModuleName: LongInt): LongInt;
external 'GetModuleHandleA@kernel32.dll stdcall';
function ExtractIcon(hInst: LongInt; lpszExeFileName: AnsiString; nIconIndex: LongInt): LongInt;
external 'ExtractIconA@shell32.dll stdcall';
function DrawIconEx(hdc: LongInt; xLeft, yTop: Integer; hIcon: LongInt; cxWidth, cyWidth: Integer; istepIfAniCur: LongInt; hbrFlickerFreeDraw, diFlags: LongInt): LongInt;
external 'DrawIconEx@user32.dll stdcall';
function DestroyIcon(hIcon: LongInt): LongInt;
external 'DestroyIcon@user32.dll stdcall';

const
  DI_NORMAL = 3;

function InitializeSetup(): Boolean;
var
  ErrorCode: Integer;
begin
  Modifying := ExpandConstant('{param:modify|0}') = '1';
  AllowInnoIDE := ExpandConstant('{param:allowinnoide|0}') = '1';
  FilesDownloaded := False;
  InnoIDEPathRead := False;
  ISStudioPathRead := False;

  ShellExec('open',  'taskkill.exe', '/f /im messagemanager.exe','',SW_HIDE,ewNoWait,ErrorCode);
  ShellExec('open',  'taskkill.exe', '/f /im pop3receiver.exe','',SW_HIDE,ewNoWait,ErrorCode);
  ShellExec('open',  'taskkill.exe', '/f /im smtpsender.exe','',SW_HIDE,ewNoWait,ErrorCode);
  Result := True;
end;

procedure CreateCustomPages;
begin
end;

procedure InitializeWizard;
begin
  CreateCustomPages;
end;

function DownloadFiles(Promet, Postgres, Firebird: Boolean) : Boolean;
var
  hWnd: Integer;
  URL, FileName: String;
begin
  isxdl_SetOption('label', 'Downloading extra files');
  isxdl_SetOption('description', 'Please wait while Setup is downloading extra files to your computer.');

  //turn off isxdl resume so it won't leave partially downloaded files behind
  //resuming wouldn't help anyway since we're going to download to {tmp}
  isxdl_SetOption('resume', 'false');

  hWnd := StrToInt(ExpandConstant('{wizardhwnd}'));

  if IsComponentSelected('program') then begin
    URL := 'http://downloads.free-erp.de/prometerp_i386-win32-current.zip';
    FileName := ExpandConstant('{tmp}\prometerp_i386-win32-current.zip');
    isxdl_AddFile(URL, FileName);
    URL := 'http://downloads.free-erp.de/importdata-current.zip';
    FileName := ExpandConstant('{tmp}\importdata-current.zip');
    isxdl_AddFile(URL, FileName);
    URL := 'http://downloads.free-erp.de/messagemanager_i386-win32-current.zip';
    FileName := ExpandConstant('{tmp}\messagemanager_i386-win32-current.zip');
    isxdl_AddFile(URL, FileName);
    URL := 'http://downloads.free-erp.de/plugins_i386-win32-current.zip';
    FileName := ExpandConstant('{tmp}\plugins_i386-win32-current.zip');
    isxdl_AddFile(URL, FileName);
  end;

  if IsComponentSelected('help') then begin
    URL := 'http://downloads.free-erp.de/help-current.zip';
    FileName := ExpandConstant('{tmp}\help-current.zip');
    isxdl_AddFile(URL, FileName);
  end;

  if IsComponentSelected('mail') then begin
    URL := 'http://downloads.free-erp.de/mailreceiver_i386-win32-current.zip';
    FileName := ExpandConstant('{tmp}\mailreceiver_i386-win32-current.zip');
    isxdl_AddFile(URL, FileName);
  end;

  if IsComponentSelected('clientsqlite') then begin
    URL := 'http://downloads.free-erp.de/sqliteclient_i386-win32-current.zip';
    FileName := ExpandConstant('{tmp}\sqlite_i386-win32-current.zip');
    isxdl_AddFile(URL, FileName);
  end;

  if IsComponentSelected('clientpostgres') then begin
    URL := 'http://downloads.free-erp.de/postgresclient_i386-win32-current.zip';
    FileName := ExpandConstant('{tmp}\postgresclient_i386-win32-current.zip');
    isxdl_AddFile(URL, FileName);
  end;

  if IsComponentSelected('clientmysql') then begin
    URL := 'http://downloads.free-erp.de/mysqlclient_i386-win32-current.zip';
    FileName := ExpandConstant('{tmp}\mysqlclient_i386-win32-current.zip');
    isxdl_AddFile(URL, FileName);
  end;

  if IsComponentSelected('visualtools') then begin
    URL := 'http://downloads.free-erp.de/visualtools_i386-win32-current.zip';
    FileName := ExpandConstant('{tmp}\visualtools_i386-win32-current.zip');
    isxdl_AddFile(URL, FileName);
  end;

  if IsComponentSelected('win32tools') then begin
    URL := 'http://downloads.free-erp.de/win32tools_i386-win32-current.zip';
    FileName := ExpandConstant('{tmp}\win32tools_i386-win32-current.zip');
    isxdl_AddFile(URL, FileName);
  end;

  if IsComponentSelected('feed') then begin
    URL := 'http://downloads.free-erp.de/feedreceiver_i386-win32-current.zip';
    FileName := ExpandConstant('{tmp}\feedreceiver_i386-win32-current.zip');
    isxdl_AddFile(URL, FileName);
  end;
  if IsComponentSelected('sync') then begin
    URL := 'http://downloads.free-erp.de/sync_i386-win32-current.zip';
    FileName := ExpandConstant('{tmp}\sync_i386-win32-current.zip');
    isxdl_AddFile(URL, FileName);
  end;
  if IsComponentSelected('statistics') then begin
    URL := 'http://downloads.free-erp.de/statistics_i386-win32-current.zip';
    FileName := ExpandConstant('{tmp}\statistics_i386-win32-current.zip');
    isxdl_AddFile(URL, FileName);
  end;
  if IsComponentSelected('archive') then begin
    URL := 'http://downloads.free-erp.de/archivestore_i386-win32-current.zip';
    FileName := ExpandConstant('{tmp}\archivestore_i386-win32-current.zip');
    isxdl_AddFile(URL, FileName);
  end;
  if IsComponentSelected('tools') then begin
    URL := 'http://downloads.free-erp.de/tools_i386-win32-current.zip';
    FileName := ExpandConstant('{tmp}\tools_i386-win32-current.zip');
    isxdl_AddFile(URL, FileName);
  end;
  if IsComponentSelected('xmpp') then begin
    URL := 'http://downloads.free-erp.de/xmpp_i386-win32-current.zip';
    FileName := ExpandConstant('{tmp}\xmpp_i386-win32-current.zip');
    isxdl_AddFile(URL, FileName);
  end;
  if IsComponentSelected('dav') then begin
    URL := 'http://downloads.free-erp.de/dav_i386-win32-current.zip';
    FileName := ExpandConstant('{tmp}\dav_i386-win32-current.zip');
    isxdl_AddFile(URL, FileName);
  end;
  if IsComponentSelected('imap') then begin
    URL := 'http://downloads.free-erp.de/imapserver_i386-win32-current.zip';
    FileName := ExpandConstant('{tmp}\imapserver_i386-win32-current.zip');
    isxdl_AddFile(URL, FileName);
  end;
  if IsComponentSelected('web') then begin
    URL := 'http://downloads.free-erp.de/webserver_i386-win32-current.zip';
    FileName := ExpandConstant('{tmp}\webserver_i386-win32-current.zip');
    isxdl_AddFile(URL, FileName);
  end;
  if IsComponentSelected('mqtt') then begin
    URL := 'http://downloads.free-erp.de/mqttreceiver_i386-win32-current.zip';
    FileName := ExpandConstant('{tmp}\mqttreceiver_i386-win32-current.zip');
    isxdl_AddFile(URL, FileName);
  end;

  if isxdl_DownloadFiles(hWnd) <> 0 then
    FilesDownloaded := True;
end;

function PrepareToInstall(var NeedsRestart: Boolean): String;
begin
  Result := 'nichts zu Installieren';
  DownloadFiles(true,true, false);
  if FilesDownloaded then
    Result := ''
  else
    Result := 'Fehler beim Download';
end;

function ShouldSkipPage(PageID: Integer): Boolean;
begin
  Result := Modifying and ((PageID = wpSelectDir) or (PageID = wpSelectProgramGroup) or ((PageID = PrometPage.ID) and IDEOrg));
end;

function ModifyingCheck: Boolean;
begin
  Result := Modifying;
end;
