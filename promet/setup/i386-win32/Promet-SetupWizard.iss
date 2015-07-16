[Setup]
AppName=Promet-ERP
AppId=prometerp
AppVersion=0.0.0
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
Uninstallable=not PortableCheck
UninstallDisplayIcon={app}\Compil32.exe
WizardImageFile=compiler:WizModernImage-IS.bmp
WizardSmallImageFile=compiler:WizModernSmallImage-IS.bmp

[Files]
Source: "isxdl.dll"; Flags: dontcopy
Source: "icons\*.ico"; Flags: dontcopy
Source: "..\..\resources\multi-icon.ico"; Flags: dontcopy
[Run]
Filename: "{tmp}\isstudio-setup.exe"; StatusMsg: "Installing Inno Script Studio..."; Parameters: {code:GetISStudioCmdLine}; Flags: skipifdoesntexist; Check: ISStudioCheck

[Code]
var
  Modifying, AllowInnoIDE: Boolean;

  PrometPage, PostgresPage: TWizardPage;
  InnoIDECheckBox, ISStudioCheckBox, ISPPCheckBox, ISCryptCheckBox: TCheckBox;
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

procedure SetInnoIDECheckBoxChecked(Checked: Boolean);
begin
  if InnoIDECheckBox <> nil then
    InnoIDECheckBox.Checked := Checked;
end;

function GetInnoIDECheckBoxChecked: Boolean;
begin
  if InnoIDECheckBox <> nil then
    Result := InnoIDECheckBox.Checked
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
    'Inno Setup supports encryption. However, because of encryption import/export laws in some countries, encryption support is not included in the main' +
    ' Inno Setup installer. Instead, it can be downloaded from a server located in the Netherlands now.';
  Label2Caption := 'Select whether you would like to download and install encryption support, then click Next.';
  CheckCaption := '&Download and install encryption support';

  PrometPage := CreateCustomOptionPage(ISPPPage.ID, Caption, SubCaption1, IconFileName, Label1Caption, Label2Caption, CheckCaption, ISCryptCheckBox);

  Caption := 'Postgres SQL Server';
  SubCaption1 := 'Möchten Sie auf diesem Computer einen Postgres SQL Server einrichten?';
  IconFileName := 'pqsql.ico';
  Label1Caption :=
    'Inno Setup Preprocessor (ISPP) is an official add-on for Inno Setup. ISPP allows' +
    ' you to conditionally compile parts of scripts, to use compile time variables in your scripts and to use built-in' +
    ' functions which for example can read from the registry or INI files at compile time.' + #13#10#13#10 +
    'ISPP also contains a special version of the ISCC command line compiler which can take variable definitions as command' +
    ' line parameters and use them during compilation.';
  Label2Caption := 'Select whether you would like to install ISPP, then click Next.';
  CheckCaption := '&Install Inno Setup Preprocessor';

  PostgresPage := CreateCustomOptionPage(IDEPage.ID, Caption, SubCaption1, IconFileName, Label1Caption, Label2Caption, CheckCaption, ISPPCheckBox);
end;

procedure InitializeWizard;
begin
  CreateCustomPages;

  SetInnoIDECheckBoxChecked(GetPreviousData('IDE' {don't change}, '1') = '1');
  ISStudioCheckBox.Checked := GetPreviousData('ISStudio', '1') = '1';
  ISPPCheckBox.Checked := GetPreviousData('ISPP', '1') = '1';
  ISCryptCheckBox.Checked := GetPreviousData('ISCrypt', '1') = '1';

  IDEOrg := GetInnoIDECheckBoxChecked or ISStudioCheckBox.Checked;
end;

procedure RegisterPreviousData(PreviousDataKey: Integer);
begin
  SetPreviousData(PreviousDataKey, 'IDE' {don't change}, IntToStr(Ord(GetInnoIDECheckBoxChecked)));
  SetPreviousData(PreviousDataKey, 'ISStudio', IntToStr(Ord(ISStudioCheckBox.Checked)));
  SetPreviousData(PreviousDataKey, 'ISPP', IntToStr(Ord(ISPPCheckBox.Checked)));
  SetPreviousData(PreviousDataKey, 'ISCrypt', IntToStr(Ord(ISCryptCheckBox.Checked)));
end;

procedure DownloadFiles(InnoIDE, ISStudio, ISCrypt: Boolean);
var
  hWnd: Integer;
  URL, FileName: String;
begin
  isxdl_SetOption('label', 'Downloading extra files');
  isxdl_SetOption('description', 'Please wait while Setup is downloading extra files to your computer.');

  try
    FileName := ExpandConstant('{tmp}\WizModernSmallImage-IS.bmp');
    if not FileExists(FileName) then
      ExtractTemporaryFile(ExtractFileName(FileName));
    isxdl_SetOption('smallwizardimage', FileName);
  except
  end;

  //turn off isxdl resume so it won't leave partially downloaded files behind
  //resuming wouldn't help anyway since we're going to download to {tmp}
  isxdl_SetOption('resume', 'false');

  hWnd := StrToInt(ExpandConstant('{wizardhwnd}'));

  if InnoIDE then begin
    URL := 'http://www.jrsoftware.org/download.php/innoide.exe';
    FileName := ExpandConstant('{tmp}\innoide-setup.exe');
    isxdl_AddFile(URL, FileName);
  end;

  if ISStudio then begin
    URL := 'http://www.jrsoftware.org/download.php/isstudio.exe';
    FileName := ExpandConstant('{tmp}\isstudio-setup.exe');
    isxdl_AddFile(URL, FileName);
  end;

  if ISCrypt then begin
    URL := 'http://www.jrsoftware.org/download.php/iscrypt.dll';
    FileName := ExpandConstant('{tmp}\ISCrypt.dll');
    isxdl_AddFile(URL, FileName);
  end;

  if isxdl_DownloadFiles(hWnd) <> 0 then
    FilesDownloaded := True
  else
    SuppressibleMsgBox('Setup could not download the extra files. Try again later or download and install the extra files manually.' + #13#13 + 'Setup will now continue installing normally.', mbError, mb_Ok, idOk);
end;

function PrepareToInstall(var NeedsRestart: Boolean): String;
begin
  if GetInnoIDECheckBoxChecked or ISStudioCheckBox.Checked or ISCryptCheckBox.Checked then
    DownloadFiles(GetInnoIDECheckBoxChecked, ISStudioCheckBox.Checked, ISCryptCheckBox.Checked);
  Result := '';
end;

function ShouldSkipPage(PageID: Integer): Boolean;
begin
  Result := Modifying and ((PageID = wpSelectDir) or (PageID = wpSelectProgramGroup) or ((PageID = IDEPage.ID) and IDEOrg));
end;

function ModifyingCheck: Boolean;
begin
  Result := Modifying;
end;

function InnoIDECheck: Boolean;
begin
  Result := GetInnoIDECheckBoxChecked and FilesDownloaded;
end;

function ISStudioCheck: Boolean;
begin
  Result := ISStudioCheckBox.Checked and FilesDownloaded;
end;

function AnyIDECheck: Boolean;
begin
  Result := InnoIDECheck or ISStudioCheck;
end;

function ISPPCheck: Boolean;
begin
  Result := ISPPCheckBox.Checked;
end;

function ISCryptCheck: Boolean;
begin
  Result := ISCryptCheckBox.Checked and FilesDownloaded;
end;

function GetIDEPath(Key, Name: String; var IDEPath: String; var IDEPathRead: Boolean): String;
var
  IDEPathKeyName, IDEPathValueName: String;
begin
  if not IDEPathRead then begin
    IDEPathKeyName := 'Software\Microsoft\Windows\CurrentVersion\Uninstall\' + Key;
    IDEPathValueName := 'Inno Setup: App Path';

    if not RegQueryStringValue(HKLM, IDEPathKeyName, IDEPathValueName, IDEPath) then begin
      if not RegQueryStringValue(HKCU, IDEPathKeyName, IDEPathValueName, IDEPath) then begin
        SuppressibleMsgBox('Error launching InnoIDE:'#13'Could not read InnoIDE path from registry.', mbError, mb_Ok, idOk);
        IDEPath := '';
      end;
    end;

    IDEPathRead := True;
  end;

  Result := IDEPath;
end;

function GetInnoIDEPath(S: String): String;
begin
  Result := GetIDEPath('{1E8BAA74-62A9-421D-A61F-164C7C3943E9}_is1', 'InnoIDE', InnoIDEPath, InnoIDEPathRead);
end;

function GetISStudioPath(S: String): String;
begin
  Result := GetIDEPath('{7C22BD69-9939-43CE-B16E-437DB2A39492}_is1', 'Inno Script Studio', ISStudioPath, ISStudioPathRead);
end;

function PortableCheck: Boolean;
begin
  Result := ExpandConstant('{param:portable|0}') = '1';
end;

function GetISStudioCmdLine(S: String): String;
begin
  Result := '/verysilent /group="' + ExpandConstant('{groupname}') + '\Inno Script Studio" /mergetasks="';
  if not IsTaskSelected('desktopicon') then
    Result := Result + '!';
  Result := Result + 'desktopicon,issfileassociation"';
  if PortableCheck then
    Result := Result + ' /portable=1';
end;
