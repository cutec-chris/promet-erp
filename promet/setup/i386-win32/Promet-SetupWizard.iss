[Setup]
AppName=Promet-ERP
AppId=prometerp
AppVersion=0.0.0
AppPublisher=Christian Ulrich
AppPublisherURL=http://www.cu-tec.de
AppSupportURL=http://www.free-erp.de
AppUpdatesURL=http://www.free-erp.de
MinVersion=0,5.0
DefaultDirName={pf}\Promet-ERP1
DefaultGroupName=Promet-ERP
AllowNoIcons=yes
Compression=lzma2/ultra
InternalCompressLevel=ultra
SolidCompression=yes
Uninstallable=not PortableCheck
UninstallDisplayIcon={app}\Compil32.exe
WizardImageFile=compiler:WizModernImage-IS.bmp
WizardSmallImageFile=compiler:WizModernSmallImage-IS.bmp

[Files]
Source: "isxdl.dll"; Flags: dontcopy
Source: "icons\*.ico"; Flags: dontcopy
Source: "..\..\resources\multi-icon.ico"; Flags: dontcopy
Source: "unzip.exe"; DestDir: "{tmp}"; Flags: deleteafterinstall

[Run]
Filename: "{tmp}\unzip.exe"; Parameters: "{tmp}\promet-erp.zip -d {app}"; StatusMsg: "Installiere Promet-ERP...";
Filename: "{tmp}\unzip.exe";  Parameters: ; Check: PrometCheck
//http://pginstaller.projects.pgfoundry.org/silent.html
Filename: "{tmp}\postgres-server.exe"; StatusMsg: "Installiere PostgresSQL Server..."; Parameters: "/qn ADDLOCAL=server"; Check: PostgresCheck

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

procedure PrometSetCheckBoxChecked(Checked: Boolean);
begin
  if PrometCheckBox <> nil then
    PrometCheckBox.Checked := Checked;
end;

function PrometGetCheckBoxChecked: Boolean;
begin
  if PrometCheckBox <> nil then
    Result := PrometCheckBox.Checked
  else
    Result := False;
end;

function InitializeSetup(): Boolean;
begin
  Modifying := ExpandConstant('{param:modify|0}') = '1';
  AllowInnoIDE := ExpandConstant('{param:allowinnoide|0}') = '1';
  FilesDownloaded := False;
  InnoIDEPathRead := False;
  ISStudioPathRead := False;

  Result := True;
end;

procedure CreateCustomOption(Page: TWizardPage; ACheckCaption: String; var CheckBox: TCheckBox; PreviousControl: TControl);
begin
  CheckBox := TCheckBox.Create(Page);
  with CheckBox do begin
    Top := PreviousControl.Top + PreviousControl.Height + ScaleY(12);
    Width := Page.SurfaceWidth;
    Caption := ACheckCaption;
    Parent := Page.Surface;
  end;
end;

function CreateCustomOptionPage(AAfterId: Integer; ACaption, ASubCaption, AIconFileName, ALabel1Caption, ALabel2Caption,
  ACheckCaption: String; var CheckBox: TCheckBox): TWizardPage;
var
  Page: TWizardPage;
  Rect: TRect;
  hIcon: LongInt;
  Label1, Label2: TNewStaticText;
begin
  Page := CreateCustomPage(AAfterID, ACaption, ASubCaption);

  try
    AIconFileName := ExpandConstant('{tmp}\' + AIconFileName);
    if not FileExists(AIconFileName) then
      ExtractTemporaryFile(ExtractFileName(AIconFileName));

    Rect.Left := 0;
    Rect.Top := 0;
    Rect.Right := 32;
    Rect.Bottom := 32;

    hIcon := ExtractIcon(GetModuleHandle(0), AIconFileName, 0);
    try
      with TBitmapImage.Create(Page) do begin
        with Bitmap do begin
          Width := 32;
          Height := 32;
          Canvas.Brush.Color := WizardForm.Color;
          Canvas.FillRect(Rect);
          DrawIconEx(Canvas.Handle, 0, 0, hIcon, 32, 32, 0, 0, DI_NORMAL);
        end;
        Parent := Page.Surface;
      end;
    finally
      DestroyIcon(hIcon);
    end;
  except
  end;

  Label1 := TNewStaticText.Create(Page);
  with Label1 do begin
    AutoSize := False;
    Left := WizardForm.SelectDirLabel.Left;
    Width := Page.SurfaceWidth - Left;
    WordWrap := True;
    Caption := ALabel1Caption;
    Parent := Page.Surface;
  end;
  WizardForm.AdjustLabelHeight(Label1);

  Label2 := TNewStaticText.Create(Page);
  with Label2 do begin
    Top := Label1.Top + Label1.Height + ScaleY(12);
    Width := Page.SurfaceWidth;
    WordWrap := True;
    Caption := ALabel2Caption;
    Parent := Page.Surface;
  end;
  WizardForm.AdjustLabelHeight(Label2);

  CreateCustomOption(Page, ACheckCaption, CheckBox, Label2);

  Result := Page;
end;

procedure URLLabelOnClick(Sender: TObject);
var
  ErrorCode: Integer;
begin
  ShellExecAsOriginalUser('open', TNewStaticText(Sender).Caption, '', '', SW_SHOWNORMAL, ewNoWait, ErrorCode);
end;

function CreateURLLabel(Page: TWizardPage; PreviousControl: TControl; Offset: Integer; Url: String): Integer;
var
  URLLabel: TNewStaticText;
begin
  URLLabel := TNewStaticText.Create(Page);
  with URLLabel do begin
    Top := PreviousControl.Top + PreviousControl.Height + ScaleY(12);
    Left := Offset;
    Caption := Url;
    Cursor := crHand;
    OnClick := @UrlLabelOnClick;
    Parent := Page.Surface;
    { Alter Font *after* setting Parent so the correct defaults are inherited first }
    URLLabel.Font.Style := URLLabel.Font.Style + [fsUnderline];
    URLLabel.Font.Color := clBlue;
  end;
  WizardForm.AdjustLabelHeight(URLLabel);
  Result := URLLabel.Width;
end;

procedure CreateCustomPages;
var
  Caption, SubCaption1, IconFileName, Label1Caption, Label2Caption, CheckCaption: String;
  UrlSize: Integer;
begin
  Caption := 'Promet-ERP';
  SubCaption1 := 'Möchten Sie diesen Computer als Promet-ERP Client einrichten?';
  IconFileName := 'multi-icon.ico';
  Label1Caption :=
    'Wenn dies ein Arbeitscomputer auf dem Promet-ERP laufen soll ist, ' +
    'benutzen Sie diese Option.';
  Label2Caption := 'Wenn dieser Computer lediglich als Datenbank-Server laufen soll, oder Dienste wie e-Mail Server,Kalenderserver,Webapplikationsserver o.ä. hier laufen sollen wählen Sie die Option ab.';
  CheckCaption := '&Promet-ERP Clientsoftware herunterladen und installieren';

  PrometPage := CreateCustomOptionPage(wpSelectProgramGroup, Caption, SubCaption1, IconFileName, Label1Caption, Label2Caption, CheckCaption, PrometCheckBox);

  Caption := 'Postgres SQL Server';
  SubCaption1 := 'Möchten Sie auf diesem Computer einen Postgres SQL Server einrichten?';
  IconFileName := 'pgsql.ico';
  Label1Caption :=
    'Wenn dieser Computer als Datenbankserver laufen soll, um anderen Computern im (lokalen) Netzwerk zugriff auf die Daten zu erlauben wählen Sie diese Option';
  Label2Caption := 'Wenn Sie im Netzwerk keine Daten bereitstellen möchten und lediglich lokal arbeiten benötigen Sie diese Option nicht.';
  CheckCaption := '&Postgres-SQL Datenbankserver herunterladen und installieren';

  PostgresPage := CreateCustomOptionPage(PrometPage.ID, Caption, SubCaption1, IconFileName, Label1Caption, Label2Caption, CheckCaption, PostgresCheckBox);
end;

procedure InitializeWizard;
begin
  CreateCustomPages;

  PrometSetCheckBoxChecked(GetPreviousData('Promet' {don't change}, '1') = '1');
  PrometCheckBox.Checked := True;
  PostgresCheckBox.Checked := True;
  //MySQLCheckBox.Checked := GetPreviousData('Postgres', '1') = '1';

  IDEOrg := PrometGetCheckBoxChecked or PrometCheckBox.Checked;
end;

procedure RegisterPreviousData(PreviousDataKey: Integer);
begin
  SetPreviousData(PreviousDataKey, 'Promet' {don't change}, IntToStr(Ord(PrometGetCheckBoxChecked)));
  SetPreviousData(PreviousDataKey, 'Postgres', IntToStr(Ord(PostgresCheckBox.Checked)));
  //SetPreviousData(PreviousDataKey, 'MySQL', IntToStr(Ord(MySQLCheckBox.Checked)));
  //SetPreviousData(PreviousDataKey, 'Firebird', IntToStr(Ord(FirebirdCheckBox.Checked)));
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

  if Promet then begin
    URL := 'http://downloads.free-erp.de/promet-erp-7.0.414.i386-win32-portable.zip';
    FileName := ExpandConstant('{tmp}\promet-erp.zip');
    isxdl_AddFile(URL, FileName);
  end;

  if Postgres then begin
    URL := 'http://get.enterprisedb.com/postgresql/postgresql-9.4.4-3-windows.exe';
    FileName := ExpandConstant('{tmp}\postgres-server.exe');
    isxdl_AddFile(URL, FileName);
  end;

  if Firebird then begin
    URL := 'http://sourceforge.net/projects/firebird/files/firebird-win32/2.5.4-Release/Firebird-2.5.4.26856-0_Win32.zip/download';
    FileName := ExpandConstant('{tmp}\firebird-server.exe');
    isxdl_AddFile(URL, FileName);
  end;

  if isxdl_DownloadFiles(hWnd) <> 0 then
    FilesDownloaded := True;
end;

function PrepareToInstall(var NeedsRestart: Boolean): String;
begin
  Result := 'nichts zu Installieren';
  if PrometGetCheckBoxChecked or PrometCheckBox.Checked or FirebirdCheckBox.Checked then
    begin
      DownloadFiles(PrometGetCheckBoxChecked, PostgresCheckBox.Checked, false);
      if FilesDownloaded then
        Result := ''
      else
        Result := 'Fehler beim Download';
    end;
end;

function ShouldSkipPage(PageID: Integer): Boolean;
begin
  Result := Modifying and ((PageID = wpSelectDir) or (PageID = wpSelectProgramGroup) or ((PageID = PrometPage.ID) and IDEOrg));
end;

function ModifyingCheck: Boolean;
begin
  Result := Modifying;
end;

function InnoIDECheck: Boolean;
begin
  Result := PrometGetCheckBoxChecked and FilesDownloaded;
end;

function PrometCheck: Boolean;
begin
  Result := PrometCheckBox.Checked and FilesDownloaded;
end;

function PostgresCheck: Boolean;
begin
  Result := PostgresCheckBox.Checked and FilesDownloaded;
end;

function FirebirdCheck: Boolean;
begin
  Result := FirebirdCheckBox.Checked and FilesDownloaded;
end;

function PortableCheck: Boolean;
begin
  Result := ExpandConstant('{param:portable|0}') = '1';
end;
