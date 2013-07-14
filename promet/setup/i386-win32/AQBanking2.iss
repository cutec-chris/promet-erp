[Setup]
AppName=Promet-ERP AQBanking 2.x Support
AppVersion=2
AppVerName={#AppName} {#AppVersion}
DefaultDirName={pf}\Promet-ERP
OutputBaseFilename=Promet-ERP_AQBanking2
InternalCompressLevel=max
PrivilegesRequired=none
TimeStampsInUTC=true
Encryption=false
Compression=bzip
VersionInfoCopyright=C.Ulrich
MinVersion=4.1.2222,4.0.1381
AppPublisher=C.Ulrich
AppPublisherURL=http://www.ullihome.de
AppSupportURL=http://www.ullihome.de
AppUpdatesURL=http://www.ullihome.de
AppContact=http://www.ullihome.de

[Files]
Source: tools\aqbanking2\*.*; DestDir: {app}\tools\aqbanking2\; Flags: recursesubdirs

[Registry]
gwenhywfar Paths: 
Root: HKCU; Subkey: Software\Gwenhywfar; ValueType: none; Flags: uninsdeletekeyifempty
Root: HKCU; Subkey: Software\Gwenhywfar\Paths; ValueType: none; Flags: uninsdeletekeyifempty
Root: HKCU; Subkey: Software\Gwenhywfar\Paths; ValueType: string; ValueName: prefix; ValueData: {app}; Flags: uninsdeletevalue
Root: HKCU; Subkey: Software\Gwenhywfar\Paths; ValueType: string; ValueName: libdir; ValueData: {app}\tools\aqbanking2\lib; Flags: uninsdeletevalue
Root: HKCU; Subkey: Software\Gwenhywfar\Paths; ValueType: string; ValueName: plugindir; ValueData: {app}\tools\aqbanking2\lib\gwenhywfar\plugins; Flags: uninsdeletevalue
Root: HKCU; Subkey: Software\Gwenhywfar\Paths; ValueType: string; ValueName: sysconfdir; ValueData: {app}\tools\aqbanking2\etc; Flags: uninsdeletevalue
Root: HKCU; Subkey: Software\Gwenhywfar\Paths; ValueType: string; ValueName: localedir; ValueData: {app}\tools\aqbanking2\share\locale; Flags: uninsdeletevalue

;aqbanking Settings
Root: HKCU; Subkey: Software\Aqbanking; ValueType: none; Flags: uninsdeletekeyifempty
Root: HKCU; Subkey: Software\Aqbanking\Paths; ValueType: none; Flags: uninsdeletekeyifempty
Root: HKCU; Subkey: Software\Aqbanking\Paths; ValueType: string; ValueName: providerdir; ValueData: {app}\tools\lib\aqbanking2\plugins\providers; Flags: uninsdeletevalue
Root: HKCU; Subkey: Software\Aqbanking\Paths; ValueType: string; ValueName: bankinfodir; ValueData: {app}\tools\lib\aqbanking2\plugins\bankinfo; Flags: uninsdeletevalue
Root: HKCU; Subkey: Software\Aqbanking\Paths; ValueType: string; ValueName: importerdir; ValueData: {app}\tools\lib\aqbanking2\plugins\imexporters; Flags: uninsdeletevalue
Root: HKCU; Subkey: Software\Aqbanking\Paths; ValueType: string; ValueName: pkgdatadir; ValueData: {app}\tools\share\aqbanking2; Flags: uninsdeletevalue;; Root: HKCU; Subkey: Software\Aqbanking\Paths; ValueType: string; ValueName: sysconfdir; ValueData: {app}\tools\etc; Flags: uninsdeletevalue;

;aqhbci Settings
Root: HKCU; Subkey: Software\AqHbci; ValueType: none; Flags: uninsdeletekeyifempty
Root: HKCU; Subkey: Software\AqHbci\Paths; ValueType: none; Flags: uninsdeletekeyifempty
Root: HKCU; Subkey: Software\AqHbci\Paths; ValueType: string; ValueName: xmldatadir; ValueData: {app}\tools\aqbanking2\share\aqhbci\xml; Flags: uninsdeletevalue


[Languages]
Name: en; MessagesFile: compiler:Default.isl
Name: de; MessagesFile: German.isl
