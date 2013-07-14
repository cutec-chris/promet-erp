[Setup]
AppName=Promet-ERP AQBanking 4.x Support
AppVersion=4
AppVerName=Promet-ERP AQBanking 4.x Support
DefaultDirName={pf}\Promet-ERP
OutputBaseFilename=Promet-ERP_AQBanking4
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
OutputDir=output

[Files]
Source: tools\aqbanking4\*.*; DestDir: {app}\tools\aqbanking4\; Flags: recursesubdirs


[Registry]
; Additionally, we have to install the paths for gwenhywfar
Root: HKLM; Subkey: Software\Gwenhywfar; ValueType: none; Flags: uninsdeletekeyifempty
Root: HKLM; Subkey: Software\Gwenhywfar\Paths; ValueType: none; Flags: uninsdeletekeyifempty
Root: HKLM; Subkey: Software\Gwenhywfar\Paths; ValueType: string; ValueName: prefix; ValueData: {app}; Flags: uninsdeletevalue
Root: HKLM; Subkey: Software\Gwenhywfar\Paths; ValueType: string; ValueName: libdir; ValueData: {app}\tools\aqbanking4\lib; Flags: uninsdeletevalue
Root: HKLM; Subkey: Software\Gwenhywfar\Paths; ValueType: string; ValueName: plugindir; ValueData: {app}\tools\aqbanking4\lib\gwenhywfar\plugins\47; Flags: uninsdeletevalue
Root: HKLM; Subkey: Software\Gwenhywfar\Paths; ValueType: string; ValueName: sysconfdir; ValueData: {app}\tools\aqbanking4\etc; Flags: uninsdeletevalue
Root: HKLM; Subkey: Software\Gwenhywfar\Paths; ValueType: string; ValueName: localedir; ValueData: {app}\tools\aqbanking4\share\locale; Flags: uninsdeletevalue

; And we also need some registry keys for aqbanking
Root: HKLM; Subkey: Software\Aqbanking; ValueType: none; Flags: uninsdeletekeyifempty
Root: HKLM; Subkey: Software\Aqbanking\Paths; ValueType: none; Flags: uninsdeletekeyifempty
Root: HKLM; Subkey: Software\Aqbanking\Paths; ValueType: string; ValueName: providerdir; ValueData: {app}\tools\aqbanking4\lib\aqbanking\plugins\29\providers; Flags: uninsdeletevalue
Root: HKLM; Subkey: Software\Aqbanking\Paths; ValueType: string; ValueName: bankinfodir; ValueData: {app}\tools\aqbanking4\lib\aqbanking\plugins\29\bankinfo; Flags: uninsdeletevalue
Root: HKLM; Subkey: Software\Aqbanking\Paths; ValueType: string; ValueName: importerdir; ValueData: {app}\tools\aqbanking4\lib\aqbanking\plugins\29\imexporters; Flags: uninsdeletevalue
Root: HKLM; Subkey: Software\Aqbanking\Paths; ValueType: string; ValueName: wizarddir; ValueData: {app}\tools\aqbanking4\lib\aqbanking\plugins\29\wizards; Flags: uninsdeletevalue
Root: HKLM; Subkey: Software\Aqbanking\Paths; ValueType: string; ValueName: pkgdatadir; ValueData: {app}\tools\aqbanking4\share\aqbanking; Flags: uninsdeletevalue
Root: HKLM; Subkey: Software\Aqbanking\Paths; ValueType: string; ValueName: sysconfdir; ValueData: {app}\tools\aqbanking4\etc; Flags: uninsdeletevalue
Root: HKLM; Subkey: Software\Aqbanking\Paths; ValueType: string; ValueName: localedir; ValueData: {app}\tools\aqbanking4\share\locale; Flags: uninsdeletevalue
Root: HKLM; Subkey: Software\AqBanking\Paths; ValueType: string; ValueName: xmldatadir; ValueData: {app}\tools\aqbanking4\share\aqhbci\xml; Flags: uninsdeletevalue
Root: HKLM; Subkey: Software\AqBanking\Paths; ValueType: string; ValueName: cfgmoduledir; ValueData: {app}\tools\aqbanking4\lib\aqbanking\plugins\29\frontends\qbanking\cfgmodules; Flags: uninsdeletevalue

[Languages]
Name: en; MessagesFile: compiler:Default.isl
Name: de; MessagesFile: German.isl
