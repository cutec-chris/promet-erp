[Defines]
#define AppName GetEnv('Progname')
#define AppVersion GetEnv('Version')
#define BaseAppVersion GetEnv('BaseVersion')
#define SetupDate GetEnv('DateStamp')
#define FullTarget GetEnv('FullTarget')
#define TargetCPU GetEnv('TARGETCPU')
[Setup]
AppID=CUPROMETERP7
AppName={#AppName}
AppVersion={#AppVersion}
AppVerName={#AppName} {#AppVersion}
DefaultDirName={code:DefDirRoot}\Promet-ERP
DefaultGroupName=Promet-ERP
UninstallDisplayIcon={app}\prometerp.exe
OutputBaseFilename=promet-erp-tools_{#AppVersion}_{#FullTarget}
OutputDir=../output
InternalCompressLevel=ultra
PrivilegesRequired=poweruser
TimeStampsInUTC=true
Encryption=false
Compression=bzip
VersionInfoCopyright=C.Ulrich
AppPublisher=C.Ulrich
AppPublisherURL=http://www.free-erp.de
AppSupportURL=http://www.free-erp.de
AppUpdatesURL=http://www.free-erp.de
AppContact=http://www.free-erp.de

[Files]
Source: "..\executables\{#BaseAppVersion}\{#TargetCPU}\cmdwizardmandant.exe"; DestDir: "{app}\tools"; Components: admin
Source: "..\executables\{#BaseAppVersion}\{#TargetCPU}\clientmanagement.exe"; DestDir: "{app}"; Components: admin
Source: "..\executables\{#BaseAppVersion}\{#TargetCPU}\messagemanager.exe"; DestDir: "{app}\tools"; Components: main
Source: "..\executables\{#BaseAppVersion}\{#TargetCPU}\*receiver.exe"; DestDir: "{app}\tools"; Components: main
Source: "..\executables\{#BaseAppVersion}\{#TargetCPU}\*sender.exe"; DestDir: "{app}\tools"; Components: main
Source: "..\executables\{#BaseAppVersion}\{#TargetCPU}\sync_*.exe"; DestDir: "{app}\tools"; Components: main
Source: "..\executables\{#BaseAppVersion}\{#TargetCPU}\pscript.exe"; DestDir: "{app}\tools"; Components: main
Source: "..\executables\{#BaseAppVersion}\{#TargetCPU}\checkout.exe"; DestDir: "{app}"; Components: admin
Source: "..\executables\{#BaseAppVersion}\{#TargetCPU}\checkin.exe"; DestDir: "{app}"; Components: admin
Source: "..\executables\{#BaseAppVersion}\{#TargetCPU}\statistics.exe"; DestDir: "{app}"; Components: statistics
Source: "..\executables\{#BaseAppVersion}\{#TargetCPU}\meetingminutes.exe"; DestDir: "{app}"; Components: meeting

Source: "tools\*.*"; DestDir: "{app}\tools"; Components: main

Source: "..\..\source\base\changes.txt"; DestDir: "{app}"; Components: main

[Components]
Name: "main"; Description: "Main Program Components"; Types: full compact custom; Flags: fixed; Languages: en
Name: "statistics"; Description: "Statistic Tool"; Types: custom; Languages: en
Name: "archive"; Description: "Archive Tool"; Types: custom; Languages: en
Name: "meeting"; Description: "Meeting Minutes"; Types: custom; Languages: en
Name: "admin"; Description: "Admin Tools"; Types: custom; Languages: en
Name: "main"; Description: "Hauptprogramm Komponenten"; Types: full compact custom; Flags: fixed; Languages: de
Name: "help"; Description: "Hilfe"; Types: full custom; Languages: de
Name: "statistics"; Description: "Statistik Tool"; Types: custom; Languages: de
Name: "archive"; Description: "Revisionssicheres Archiv"; Types: custom; Languages: de
Name: "meeting"; Description: "Besprechungsprotokoll"; Types: custom; Languages: de
Name: "admin"; Description: "Administrator Tools"; Types: full custom; Languages: de

[Tasks]
Name: desktopicon; Description: Create an Desktop Icon; GroupDescription: Additional Icons:; Languages: en
Name: desktopicon; Description: Ein Desktop Icon erstellen; GroupDescription: Zusätzliche Icons:; Languages: de

[Icons]
Name: {group}\Statistics; Filename: {app}\statistics.exe; Workingdir: {app}; Languages: en; Components: statistics; Flags: createonlyiffileexists
Name: {group}\Statistik; Filename: {app}\statistics.exe; Workingdir: {app}; Languages: de; Components: statistics; Flags: createonlyiffileexists
Name: {group}\Meeting Minutes; Filename: {app}\meetingminutes.exe; Workingdir: {app}; Languages: en; Components: meeting; Flags: createonlyiffileexists
Name: {group}\Besprechungsprotokoll; Filename: {app}\meetingminutes.exe; Workingdir: {app}; Languages: de; Components: meeting; Flags: createonlyiffileexists
Name: {group}\Clientmanagement; Filename: {app}\clientmanagement.exe; Workingdir: {app}; Languages: en; Components: admin; Flags: createonlyiffileexists
Name: {group}\Clientmanagement; Filename: {app}\clientmanagement.exe; Workingdir: {app}; Languages: de; Components: admin; Flags: createonlyiffileexists
Name: {group}\Archive; Filename: {app}\archivestore.exe; Workingdir: {app}; Languages: en; Components: admin; Flags: createonlyiffileexists
Name: {group}\Revisionssicheres Archiv; Filename: {app}\archivestore.exe; Workingdir: {app}; Languages: de; Components: admin; Flags: createonlyiffileexists
Name: {group}\Internet; Filename: {app}\website.url
Name: {userdesktop}\Statistics; Filename: {app}\statistics.exe; Tasks: desktopicon; Flags: createonlyiffileexists
Name: {userdesktop}\Besprechungsprotokoll; Filename: {app}\meetingminutes.exe; Tasks: desktopicon; Flags: createonlyiffileexists

[Languages]
Name: en; MessagesFile: compiler:Default.isl
Name: de; MessagesFile: German.isl

[Code]
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
