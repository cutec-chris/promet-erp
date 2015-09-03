[Defines]
#define AppName GetEnv('Progname')
#define AppVersion GetEnv('Version')
#define BaseAppVersion GetEnv('BaseVersion')
#define SetupDate GetEnv('DateStamp')
#define FullTarget GetEnv('FullTarget')
#define TargetCPU GetEnv('TARGETCPU')
[Setup]
AppID=CUPROMETHEUS7
AppName={#AppName}
AppVersion={#AppVersion}
AppVerName={#AppName} {#AppVersion}
DefaultDirName={code:DefDirRoot}\Promet-ERP
DefaultGroupName=Promet-ERP
UninstallDisplayIcon={app}\timeregistering.exe
OutputBaseFilename=promet-erp-timeregistering_{#AppVersion}_{#FullTarget}
OutputDir=../output
InternalCompressLevel=ultra
PrivilegesRequired=poweruser
TimeStampsInUTC=true
Encryption=false
Compression=bzip
VersionInfoCopyright=C.Ulrich
AppPublisher=C.Ulrich
AppPublisherURL=http://www.ullihome.de
AppSupportURL=http://www.ullihome.de
AppUpdatesURL=http://www.ullihome.de
AppContact=http://www.ullihome.de

[Files]
Source: ..\executables\{#BaseAppVersion}\{#TargetCPU}\messagemanager.exe; DestDir: {app}\tools; Components: main
Source: ..\executables\{#BaseAppVersion}\{#TargetCPU}\wizardmandant.exe; DestDir: {app}; Components: main
Source: ..\executables\{#BaseAppVersion}\{#TargetCPU}\timeregistering.exe; DestDir: {app}; Components: main
Source: ..\..\importdata\*.*; DestDir: {app}\importdata; Components: main; Flags: recursesubdirs
Source: ..\help.db; DestDir: {app}; Components: help
Source: ..\..\source\base\changes.txt; DestDir: {app}; Components: main
Source: website.url; DestDir: {app}

Source: ..\errors.txt; DestDir: {app}; Components: main
Source: ..\warnings.txt; DestDir: {app}; Components: main

Source: ..\..\languages\*.po; DestDir: {app}\languages; Components: main
Source: ..\..\languages\*.txt; DestDir: {app}\languages; Components: main

[Components]
Name: main; Description: Hauptprogramm Komponenten; Languages: de; Types: full compact custom; Flags: fixed
Name: main; Description: Main Program Components; Languages: en; Types: full compact custom; Flags: fixed
Name: help; Description: Help; Languages: en; Types: full custom
Name: help; Description: Hilfe; Languages: de; Types: full custom
[Tasks]
Name: "startupde"; Description: "Beim Systemstart starten"; GroupDescription: "{cm:AdditionalIcons}"; Languages: de
Name: "startupen"; Description: "Automatically start on login"; GroupDescription: "{cm:AdditionalIcons}"; Languages: en

[Icons]
Name: "{group}\Zeiterfassung"; Filename: "{app}\timeregistering.exe"; WorkingDir: "{app}"; Flags: createonlyiffileexists; Languages: de
Name: "{group}\Timeregistering"; Filename: "{app}\timeregistering.exe"; WorkingDir: "{app}"; Flags: createonlyiffileexists; Languages: en

Name: "{commonstartup}\Timeregistering"; Filename: "{app}\timeregistering.exe"; Tasks: startupen; Languages: en
Name: "{commonstartup}\Zeiterfassung"; Filename: "{app}\timeregistering.exe"; Tasks: startupde; Languages: de

[UninstallDelete]
Type: filesandordirs; Name: {app}

[Languages]
Name: en; MessagesFile: compiler:Default.isl
Name: de; MessagesFile: German.isl

[Code]
//#include "fixfonts.iss" // see http://www.gerixsoft.com/blog/delphi/system-font-innosetup
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

procedure CurUninstallStepChanged(CurUninstallStep: TUninstallStep);
begin
if CurUninstallStep = usUninstall then
  begin
    UninstallFeedback('Rückmeldung', 'Senden', 'Abbrechen',
    'Um das Programm zu verbessern, wäre es schön wenn Sie uns ein paar Worte zu den Gründen der Deinstallation und Ihrer Erfahrung mit dem Programm schreiben würden.'#13#10'Danke.',
    'support@free-erp.de', 'Deinstallations Rückmeldung');
  end;
end;
