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
OutputBaseFilename=promet-erp-server_{#AppVersion}_{#FullTarget}
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
Source: "..\executables\{#BaseAppVersion}\{#TargetCPU}\processmanager.exe"; DestDir: "{app}\tools"; Components: main
Source: "..\executables\{#BaseAppVersion}\{#TargetCPU}\*receiver.exe"; DestDir: "{app}\tools"; Components: main
Source: "..\executables\{#BaseAppVersion}\{#TargetCPU}\*sender.exe"; DestDir: "{app}\tools"; Components: main
Source: "..\executables\{#BaseAppVersion}\{#TargetCPU}\sync_*.exe"; DestDir: "{app}\tools"; Components: main
Source: "..\executables\{#BaseAppVersion}\{#TargetCPU}\pscript.exe"; DestDir: "{app}\tools"; Components: main
Source: "..\executables\{#BaseAppVersion}\{#TargetCPU}\checkout.exe"; DestDir: "{app}"; Components: admin
Source: "..\executables\{#BaseAppVersion}\{#TargetCPU}\checkin.exe"; DestDir: "{app}"; Components: admin

Source: "tools\*.*"; DestDir: "{app}\tools"; Components: main

Source: "..\..\source\base\changes.txt"; DestDir: "{app}"; Components: main

[Components]
Name: "main"; Description: "Main Program Components"; Types: full compact custom; Flags: fixed; Languages: en

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
