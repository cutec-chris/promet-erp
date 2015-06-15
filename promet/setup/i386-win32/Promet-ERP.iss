[Defines]
#define AppName GetEnv('Progname')
#define AppVersion GetEnv('Version')
#define BaseAppVersion GetEnv('BaseVersion')
#define SetupDate GetEnv('DateStamp')
#define FullTarget GetEnv('FullTarget')
#define TargetCPU GetEnv('TARGETCPU')
[Setup]
AppID=CUPROMETERP7
AppName={#AppName}-Tools
AppVersion={#AppVersion}
AppVerName={#AppName} {#AppVersion}
DefaultDirName={code:DefDirRoot}\Promet-ERP
DefaultGroupName=Promet-ERP
UninstallDisplayIcon={app}\prometerp.exe
OutputBaseFilename=promet-erp_{#AppVersion}_{#FullTarget}
OutputDir=../output
InternalCompressLevel=ultra
PrivilegesRequired=none
TimeStampsInUTC=true
Encryption=false
Compression=bzip
VersionInfoCopyright=C.Ulrich
AppPublisher=C.Ulrich
AppPublisherURL=http://www.free-erp.de
AppSupportURL=http://mantis.free-erp.de/mantis/
AppUpdatesURL=http://www.free-erp.de
AppContact=http://www.free-erp.de
CloseApplications=yes

[Files]
Source: "..\executables\{#BaseAppVersion}\{#TargetCPU}\prometerp.exe"; DestDir: "{app}"; Components: main
Source: "..\executables\{#BaseAppVersion}\{#TargetCPU}\pstarter.exe"; DestDir: "{app}"; Components: main
Source: "..\executables\{#BaseAppVersion}\{#TargetCPU}\messagemanager.exe"; DestDir: "{app}\tools"; Components: main
Source: "..\executables\{#BaseAppVersion}\{#TargetCPU}\wizardmandant.exe"; DestDir: "{app}"; Components: main
Source: "..\executables\{#BaseAppVersion}\{#TargetCPU}\*receiver.exe"; DestDir: "{app}\tools"; Components: main
Source: "..\executables\{#BaseAppVersion}\{#TargetCPU}\*sender.exe"; DestDir: "{app}\tools"; Components: main
Source: "..\executables\{#BaseAppVersion}\{#TargetCPU}\pscript.exe"; DestDir: "{app}\tools"; Components: main
Source: "..\executables\{#BaseAppVersion}\{#TargetCPU}\sync_*.exe"; DestDir: "{app}\tools"; Components: main
Source: "sqlite3.dll"; DestDir: "{app}"; Components: main
Source: "sqlite3.dll"; DestDir: "{app}\tools"; Components: main
Source: "..\executables\{#BaseAppVersion}\{#TargetCPU}\linksender.exe"; DestDir: "{app}\tools"; Components: main
Source: "..\..\importdata\*.*"; DestDir: "{app}\importdata"; Flags: recursesubdirs; Components: main

Source: "..\errors.txt"; DestDir: "{app}"; Components: main
Source: "..\warnings.txt"; DestDir: "{app}"; Components: main

Source: "..\executables\{#BaseAppVersion}\{#TargetCPU}\helpviewer.exe"; DestDir: "{app}"; Components: help
Source: "..\help\help.db"; DestDir: "{app}"; Components: help

Source: "tools\*.*"; DestDir: "{app}\tools"; Components: main

Source: "..\..\source\base\changes.txt"; DestDir: "{app}"; Components: main

Source: "..\..\languages\*.po"; DestDir: "{app}\languages"; Components: main
Source: "..\..\languages\*.txt"; DestDir: "{app}\languages"; Components: main

Source: "plugins\*.*"; DestDir: "{app}\plugins"; Components: main

Source: "website.url"; DestDir: "{app}"

[Run]
Filename: "{app}\wizardmandant.exe"; Parameters: "--silent"; Flags: postinstall shellexec skipifsilent; Description: "Standarddatenbank erstellen"; Components: main

[Components]
Name: "main"; Description: "Main Program Components"; Types: full compact custom; Flags: fixed; Languages: en
Name: "help"; Description: "Help"; Types: full custom; Languages: en
Name: "admin"; Description: "Admin Tools"; Types: custom; Languages: en
Name: "main"; Description: "Hauptprogramm Komponenten"; Types: full compact custom; Flags: fixed; Languages: de
Name: "help"; Description: "Hilfe"; Types: full custom; Languages: de
Name: "admin"; Description: "Administrator Tools"; Types: full custom; Languages: de

[Tasks]
Name: desktopicon; Description: Create an Desktop Icon; GroupDescription: Additional Icons:; Languages: en
Name: desktopicon; Description: Ein Desktop Icon erstellen; GroupDescription: Zusätzliche Icons:; Languages: de

[Icons]
Name: {group}\{#AppName}; Filename: {app}\prometerp.exe; Workingdir: {app}; Flags: createonlyiffileexists
Name: {group}\Internet; Filename: {app}\website.url
Name: {userdesktop}\{#AppName}; Filename: {app}\prometerp.exe; Tasks: desktopicon; Flags: createonlyiffileexists

[Registry]
Root: HKCR; Subkey: ".plink"; ValueType: string; ValueName: ""; ValueData: "Promet-ERP-Link"; Flags: uninsdeletevalue
Root: HKCR; Subkey: "Promet-ERP-Link"; ValueType: string; ValueName: ""; ValueData: "Promet-ERP Verknüpfung"; Flags: uninsdeletekey
Root: HKCR; Subkey: "Promet-ERP-Link\DefaultIcon"; ValueType: string; ValueName: ""; ValueData: "{app}\tools\linksender.exe,0"
Root: HKCR; Subkey: "Promet-ERP-Link\shell\open\command"; ValueType: string; ValueName: ""; ValueData: """{app}\tools\linksender.exe"" ""%1"""

[UninstallDelete]
Type: filesandordirs; Name: {app}

[Languages]
Name: en; MessagesFile: compiler:Default.isl
Name: de; MessagesFile: German.isl

[Code]
#include "feedback.iss"

function IsRegularUser(): Boolean;
begin
  Result := not (IsAdminLoggedOn or IsPowerUserLoggedOn);
end;

function DefDirRoot(Param: String): String;
begin
  if IsRegularUser then
    Result := ExpandConstant('{localappdata}')
  else
    Result := ExpandConstant('{pf}')
end;

{
function InitializeSetup : Boolean;
var
ErrorCode: Integer;
begin
  ShellExec('open',  'taskkill.exe', '/f /im messagemanager.exe','',SW_HIDE,ewNoWait,ErrorCode);
  ShellExec('open',  'taskkill.exe', '/f /im pop3receiver.exe','',SW_HIDE,ewNoWait,ErrorCode);
  ShellExec('open',  'taskkill.exe', '/f /im smtpsender.exe','',SW_HIDE,ewNoWait,ErrorCode);
end;
}

procedure CurUninstallStepChanged(CurUninstallStep: TUninstallStep);
begin
if CurUninstallStep = usUninstall then
  begin
    UninstallFeedback('Rückmeldung', 'Senden', 'Abbrechen',
    'Um das Programm zu verbessern, wäre es schön wenn Sie uns ein paar Worte zu den Gründen der Deinstallation und Ihrer Erfahrung mit dem Programm schreiben würden.'#13#10'Danke.',
    'support@free-erp.de', 'Deinstallations Rückmeldung');
  end;
end;
