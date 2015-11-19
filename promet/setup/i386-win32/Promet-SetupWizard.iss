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

[Components]
Name: "program"; Description: "Programdateien"; Types: full compact custom;ExtraDiskSpaceRequired: 14600000
Name: "help"; Description: "Hilfe Datenbank"; Types: full compact custom;ExtraDiskSpaceRequired: 10000000
Name: "visualtools"; Description: "Werkzeuge"; Types: full compact custom;ExtraDiskSpaceRequired: 29100000
Name: "mail"; Description: "e-Mail Unterstützung"; Types: full custom
Name: "feed"; Description: "Feed Unterstützung (Atom,RSS,Twitter)"; Types: full custom
Name: "clientsqlite"; Description: "SQLite Unterstützung"; Types: full custom
Name: "clientpostgres"; Description: "Postgres Unterstützung"; Types: custom
Name: "clientmysql"; Description: "MySQL Unterstützung"; Types: full custom
Name: "sync"; Description: "Datenbanksynchronisation/Backup"; Types: full custom
Name: "statistics"; Description: "Reporting/Statistik"; Types: custom
Name: "archive"; Description: "Archivprogramm (Revisionssichere Archivierung)"; Types: custom
Name: "tools"; Description: "Kommandozeilenprogramme"; Types: custom
Name: "xmpp"; Description: "Jabber/XMPP Unterstützung"; Types: custom
Name: "dav"; Description: "Kalender/Kontaktserver (CalDAV,CardDAV)"; Types: custom
Name: "imap"; Description: "e-Mail Server (IMAP)"; Types: custom
Name: "web"; Description: "Webserver/Applikationsserver"; Types: custom
Name: "mqtt"; Description: "MQTT Unterstützung (IoT)"; Types: custom

[Run]
Filename: "{tmp}\unzip.exe"; Parameters: "{tmp}\*.zip -aoa -d ""{app}"""; Flags: waituntilterminated runhidden; StatusMsg: "Installiere Promet-ERP..."

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
begin
  Modifying := ExpandConstant('{param:modify|0}') = '1';
  AllowInnoIDE := ExpandConstant('{param:allowinnoide|0}') = '1';
  FilesDownloaded := False;
  InnoIDEPathRead := False;
  ISStudioPathRead := False;

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
    URL := 'http://downloads.free-erp.de/imap_i386-win32-current.zip';
    FileName := ExpandConstant('{tmp}\imap_i386-win32-current.zip');
    isxdl_AddFile(URL, FileName);
  end;
  if IsComponentSelected('web') then begin
    URL := 'http://downloads.free-erp.de/webserver_i386-win32-current.zip';
    FileName := ExpandConstant('{tmp}\webserver_i386-win32-current.zip');
    isxdl_AddFile(URL, FileName);
  end;
  if IsComponentSelected('mqtt') then begin
    URL := 'http://downloads.free-erp.de/mqtt_i386-win32-current.zip';
    FileName := ExpandConstant('{tmp}\mqtt_i386-win32-current.zip');
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
