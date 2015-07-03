unit uformatsettingsform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ButtonPanel, ComCtrls, StdCtrls, Spin, ExtCtrls, Buttons,sCtrls;

type
  { TFormatSettingsForm }

  TFormatSettingsForm = class(TForm)
    Bevel1: TBevel;
    Bevel2: TBevel;
    Bevel3: TBevel;
    BtnCurrency: TBitBtn;
    ButtonPanel: TButtonPanel;
    CbLongDateFormat: TComboBox;
    CbLongTimeFormat: TComboBox;
    CbPosCurrencyFormat: TComboBox;
    CbNegCurrencyFormat: TComboBox;
    CbShortDateFormat: TComboBox;
    CbShortTimeFormat: TComboBox;
    EdCurrencySymbol: TEdit;
    EdCurrencyDecimals: TSpinEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    LblCurrencySymbol: TLabel;
    LblCurrencySymbol1: TLabel;
    LblDateTimeSample: TLabel;
    LblDecimalSeparator: TLabel;
    LblDateSeparator: TLabel;
    LblTimeSeparator: TLabel;
    LblLongDayNames: TLabel;
    LblLongMonthNames: TLabel;
    LblNumFormat1: TLabel;
    LblNumFormat2: TLabel;
    LblNumFormat3: TLabel;
    LblNumFormat4: TLabel;
    LblPosCurrencyFormat: TLabel;
    LblNegCurrencyFormat: TLabel;
    LblShortDayNames: TLabel;
    LblShortMonthNames: TLabel;
    LblThousandSeparator: TLabel;
    PageControl: TPageControl;
    PgCurrency: TTabSheet;
    PgDateTime: TTabSheet;
    PgNumber: TTabSheet;
    procedure BtnCurrencyClick(Sender: TObject);
    procedure DateTimeFormatChange(Sender: TObject);
    procedure EdCurrencySymbolChange(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
    procedure PageControlChange(Sender: TObject);
  private
    FSampleDateTime: TDateTime;
    FDateFormatSample: String;
    FTimeFormatSample: String;
    FEdLongMonthNames: TMonthDayNamesEdit;
    FEdShortMonthNames: TMonthDayNamesEdit;
    FEdLongDayNames: TMonthDayNamesEdit;
    FEdShortDayNames: TMonthDayNamesEdit;
    FCbDecimalSeparator: TFormatSeparatorCombo;
    FCbThousandSeparator: TFormatSeparatorCombo;
    FCbDateSeparator: TFormatSeparatorCombo;
    FCbTimeSeparator: TFormatSeparatorCombo;
    function GetFormatSettings: TFormatSettings;
    procedure SetFormatSettings(const AValue: TFormatSettings);
    function ValidData(out AControl: TWinControl; out AMsg: String): Boolean;
  public
    { public declarations }
    property FormatSettings: TFormatSettings read GetFormatSettings write SetFormatSettings;
  end;

var
  FormatSettingsForm: TFormatSettingsForm;


implementation

{$R *.lfm}

uses
  fpsUtils, fpsNumFormat,
  uCurrencyForm;

const
  CURR_VALUE = 100.0;

var
  PageIndex: Integer = 0;  // stores the previously selected page index (to open the form always with previously used page)


{ TFormatSettingsForm }

procedure TFormatSettingsForm.DateTimeFormatChange(Sender: TObject);
var
  fs: TFormatSettings;
  ctrl: TWinControl;
  dt: TDateTime;
  s: String;
begin
  fs := GetFormatSettings;
  dt := FSampleDateTime;
  ctrl := ActiveControl;

  if (ctrl = CbLongDateFormat) then
  begin
    FDateFormatSample := fs.LongDateFormat;
    s := FormatDateTime(FDateFormatSample, dt, fs);
    LblDateTimeSample.Caption := 'Sample date:'#13 + s;
  end
  else
  if (ctrl = CbShortDateFormat) then
  begin
    FDateFormatSample := fs.ShortDateFormat;
    s := FormatDateTime(FDateFormatSample, dt, fs);
    LblDateTimeSample.Caption := 'Sample date:'#13 + s;
  end
  else
  if (ctrl = FCbDateSeparator) then begin
    s := FormatDateTime(FDateFormatSample, dt, fs);
    LblDateTimeSample.Caption := 'Sample date:'#13 + s;
  end
  else
  if (ctrl = CbLongTimeFormat) then
  begin
    FTimeFormatSample := fs.LongTimeFormat;
    s := FormatDateTime(FTimeFormatSample, dt, fs);
    LblDateTimeSample.Caption := 'Sample time:'#13 + s;
  end
  else
  if (ctrl = CbShortTimeFormat) then
  begin
    FTimeFormatSample := fs.ShortTimeFormat;
    s := FormatDateTime(FTimeFormatSample, dt, fs);
    LblDateTimeSample.Caption := 'Sample time:'#13 + s;
  end
  else
  if (ctrl = FCbTimeSeparator) then
  begin
    s := FormatDateTime(FTimeFormatSample, dt, fs);
    LblDateTimeSample.Caption := 'Sample time:'#13 + s;
    {
  end
  else
  begin
    s := AnsiToUTF8(FormatDateTime('c', dt, fs));
    LblDateTimeSample.Caption := 'Sample date/time:'#13 + s;
    }
  end;

  LblDateTimeSample.Visible := (PageControl.Activepage = PgDateTime) and
    ((FDateFormatSample <> '') or (FTimeFormatSample <> ''));
//  Application.ProcessMessages;
end;

procedure TFormatSettingsForm.BtnCurrencyClick(Sender: TObject);
var
  F: TCurrencyForm;
begin
  F := TCurrencyForm.Create(nil);
  try
    F.CurrencySymbol := EdCurrencySymbol.Text;
    if F.ShowModal = mrOK then
      EdCurrencySymbol.Text := F.CurrencySymbol;
  finally
    F.Free;
  end;
end;

procedure TFormatSettingsForm.EdCurrencySymbolChange(Sender: TObject);
var
  currSym: String;
begin
  currSym := EdCurrencySymbol.Text;
  BuildCurrencyFormatList(CbPosCurrencyFormat.Items, true, CURR_VALUE, currSym);
  BuildCurrencyFormatList(CbNegCurrencyFormat.Items, false, CURR_VALUE, currSym);
end;

procedure TFormatSettingsForm.FormCloseQuery(Sender: TObject;
  var CanClose: boolean);
begin
  Unused(Sender, CanClose);
  PageIndex := PageControl.ActivePageIndex;
end;

procedure TFormatSettingsForm.FormCreate(Sender: TObject);
const
  DROPDOWN_COUNT = 32;
var
  w: Integer;
begin
  PageControl.ActivePageIndex := PageIndex;

  CbLongDateFormat.DropdownCount := DROPDOWN_COUNT;
  CbShortDateFormat.DropdownCount := DROPDOWN_COUNT;
  CbLongTimeFormat.DropdownCount := DROPDOWN_COUNT;
  CbShortTimeFormat.DropdownCount := DROPDOWN_COUNT;
  CbPosCurrencyFormat.DropdownCount := DROPDOWN_COUNT;
  CbNegCurrencyFormat.DropdownCount := DROPDOWN_COUNT;

  w := CbLongDateFormat.Width;
  FCbDecimalSeparator := TFormatSeparatorCombo.Create(self);
  with FCbDecimalSeparator do
  begin
    Parent := PgNumber;
    Left := CbLongDateFormat.Left;
    Width := w;
    Top := CbLongDateFormat.Top;
    TabOrder := 0;
    SeparatorKind := skDecimal;
  end;
  LblDecimalSeparator.FocusControl := FCbDecimalSeparator;

  FCbThousandSeparator := TFormatSeparatorCombo.Create(self);
  with FCbThousandSeparator do
  begin
    Parent := PgNumber;
    Left := FCbDecimalSeparator.Left;
    Width := w;
    Top := FCBDecimalSeparator.Top + 32;
    TabOrder := FCbDecimalSeparator.TabOrder + 1;
    SeparatorKind := skThousand;
  end;
  LblThousandSeparator.FocusControl := FCbThousandSeparator;

  FCbDateSeparator := TFormatSeparatorCombo.Create(self);
  with FCbDateSeparator do
  begin
    Parent := PgDateTime;
    Left := CbShortDateFormat.Left;
    Width := w;
    Top := CbShortDateFormat.Top + 32;
    TabOrder := CbShortDateFormat.TabOrder + 1;
    SeparatorKind := skDate;
    OnChange := @DateTimeFormatChange;
    OnEnter := @DateTimeFormatChange;
  end;
  LblDateSeparator.FocusControl := FCbDateSeparator;

  FEdLongMonthNames := TMonthDayNamesEdit.Create(self);
  with FEdLongMonthNames do
  begin
    Parent := PgDateTime;
    Left :=  CbShortDateFormat.Left;
   {$IFDEF LCL_FULLVERSION AND LCL_FULLVERSION > 1020600}
    Width := w;
   {$ELSE}
    Width := w - Button.Width;
   {$ENDIF}
    Top := CbShortDateFormat.Top + 32*2;
    OnChange := @DateTimeFormatChange;
    OnEnter := @DateTimeFormatChange;
    TabOrder := CbShortDateFormat.TabOrder + 2;
  end;
  LblLongMonthNames.FocusControl := FEdLongMonthNames;

  FEdShortMonthNames := TMonthDayNamesEdit.Create(self);
  with FEdShortMonthNames do
  begin
    Parent := PgDateTime;
    Left :=  CbShortDateFormat.Left;
    Width := FEdLongMonthNames.Width;
    Top := CbShortDateFormat.Top + 32*3;
    TabOrder := CbShortDateFormat.TabOrder + 3;
    OnChange := @DateTimeFormatChange;
    OnEnter := @DateTimeFormatChange;
  end;
  LblShortMonthNames.FocusControl := FEdShortMonthNames;

  FEdLongDayNames := TMonthDayNamesEdit.Create(self);
  with FEdLongDayNames do
  begin
    Parent := PgDateTime;
    Left :=  CbShortDateformat.Left;
    Width := FEdLongMonthNames.Width;
    Top := CbShortDateFormat.Top + 32*4;
    TabOrder := CbShortDateFormat.TabOrder + 4;
    OnChange := @DateTimeFormatChange;
    OnEnter := @DateTimeFormatChange;
  end;
  LblLongDayNames.FocusControl := FEdLongDayNames;

  FEdShortDayNames := TMonthDayNamesEdit.Create(self);
  with FEdShortDayNames do
  begin
    Parent := PgDateTime;
    Left :=  CbShortDateFormat.Left;
    Width := FEdLongMonthNames.Width;
    Top := CbShortDateFormat.Top + 32*5;
    TabOrder := CbShortDateFormat.TabOrder + 5;
    OnChange := @DateTimeFormatChange;
    OnEnter := @DateTimeFormatChange;
  end;
  LblShortDayNames.FocusControl := FEdShortDayNames;

  FCbTimeSeparator := TFormatSeparatorCombo.Create(self);
  with FCbTimeSeparator do
  begin
    Parent := PgDateTime;
    Left := CbShortTimeFormat.Left;
    Width := w;
    Top := CbShortTimeFormat.Top + 32;
    TabOrder := CbShortTimeFormat.TabOrder + 1;
    SeparatorKind := skTime;
    OnChange := @DateTimeFormatChange;
    OnEnter := @DateTimeFormatChange;
  end;
  LblTimeSeparator.FocusControl := FCbTimeSeparator;

  FDateFormatSample := '';
  FTimeFormatSample := '';
  FSampleDateTime := now();

  LblDateTimeSample.Visible := false;

  // Published property not available in old Laz versions
  EdCurrencyDecimals.Alignment := taRightJustify;
end;

procedure TFormatSettingsForm.OKButtonClick(Sender: TObject);
var
  msg: String;
  C: TWinControl;
  cParent: TWinControl;
begin
  if not ValidData(C, msg) then
  begin
    cParent := C.Parent;
    while (cParent <> nil) and not (cParent is TTabSheet) do
      cParent := cParent.Parent;
    PageControl.ActivePage := cParent as TTabSheet;
    if C.CanFocus then C.SetFocus;
    MessageDlg(msg, mtError, [mbOK], 0);
    ModalResult := mrNone;
  end;
end;

procedure TFormatSettingsForm.PageControlChange(Sender: TObject);
begin
  LblDateTimeSample.Visible := (PageControl.Activepage = PgDateTime) and
    ((FDateFormatSample <> '') or (FTimeFormatSample <> ''));
end;

function TFormatSettingsForm.GetFormatSettings: TFormatSettings;
begin
  Result := DefaultFormatSettings;

  // --- Number format parameters --
  // Decimal separator
  Result.DecimalSeparator := FCbDecimalSeparator.Separator;
  // Thousand separator
  Result.ThousandSeparator := FCbThousandSeparator.Separator;

  // --- Currency format parameters ---
  // Currency symbol
  Result.CurrencyString := EdCurrencySymbol.Text;
  // Currency decimal places
  Result.CurrencyDecimals := EdCurrencyDecimals.Value;
  // Positive currency format
  Result.CurrencyFormat := CbPosCurrencyFormat.ItemIndex;
  // Negative currency format
  Result.NegCurrFormat := CbNegCurrencyFormat.ItemIndex;

  // --- Date format parameters ---
  // Long date format string
  Result.LongDateFormat := CbLongDateFormat.Text;
  // Short date format string
  Result.ShortDateFormat := CbShortDateFormat.Text;
  // Date separator
  Result.DateSeparator := FCbDateSeparator.Separator;
  // Long month names
  FEdLongMonthNames.GetNames(Result.LongMonthNames);
  // Short month names
  FEdShortMonthNames.GetNames(Result.ShortMonthNames);
  // Long day names
  FEdLongDayNames.GetNames(Result.LongDayNames);
  // Short day names
  FEdShortDayNames.GetNames(Result.ShortDayNames);

  // --- Time format parameters ---
  // Long time format string
  Result.LongTimeFormat := CbLongTimeFormat.Text;
  // Short time format string
  Result.ShortTimeFormat := CbShortTimeFormat.Text;
  // Time separator
  Result.TimeSeparator := FCbTimeSeparator.Separator;
end;

procedure TFormatSettingsForm.SetFormatSettings(const AValue: TFormatSettings);
var
  i: Integer;
begin
  // --- Number format parameters ---
  FCbDecimalSeparator.Separator := AValue.DecimalSeparator;
  FCbThousandSeparator.Separator := AValue.ThousandSeparator;

  // --- Currency format parameters ---
  // Currency symbol
  EdCurrencySymbol.Text := AValue.CurrencyString;
  // Currency decimal places
  EdCurrencyDecimals.Value := AValue.CurrencyDecimals;
  // Positive currency format
  CbPosCurrencyFormat.ItemIndex := AValue.CurrencyFormat;
  // Negative currency format
  CbNegCurrencyFormat.ItemIndex := AValue.NegCurrFormat;

  // --- Date format parameters ---
  // Long date format string
  i := CbLongDateFormat.Items.IndexOf(AValue.LongDateFormat);
  if i = -1 then
    CbLongDateFormat.ItemIndex := CbLongDateFormat.Items.Add(AValue.LongDateFormat)
  else
    CbLongDateFormat.ItemIndex := i;
  // Short date format string
  i := CbShortDateFormat.Items.IndexOf(AValue.ShortDateFormat);
  if i = -1 then
    CbShortDateFormat.ItemIndex := CbShortDateFormat.items.Add(AValue.ShortDateFormat)
  else
    CbShortDateFormat.ItemIndex := i;
  // Date separator
  FCbDateSeparator.Separator := AValue.DateSeparator;
  // Long month names
  FEdLongMonthNames.SetNames(AValue.LongMonthNames, 12, false, 'Error');
  // Short month names
  FEdShortMonthNames.SetNames(AValue.ShortMonthNames, 12, true, 'Error');
  // Long day names
  FEdLongDayNames.SetNames(AValue.LongDayNames, 7, false, 'Error');
  // Short month names
  FEdShortDayNames.SetNames(AValue.ShortDayNames, 7, true, 'Error');

  // --- Time format parameters ---

  // Long time format string
  i := CbLongTimeFormat.items.IndexOf(AValue.LongTimeFormat);
  if i = -1 then
    CbLongTimeFormat.ItemIndex := CbLongTimeFormat.Items.Add(AValue.LongTimeFormat)
  else
    CbLongTimeFormat.ItemIndex := i;
  // Short time format string
  i := cbShortTimeFormat.Items.IndexOf(AValue.ShortTimeFormat);
  if i = -1 then
    CbShortTimeFormat.itemIndex := CbShortTimeFormat.Items.Add(AValue.ShortTimeFormat);
  // Time separator
  FCbTimeSeparator.Separator := AValue.TimeSeparator;
end;

function TFormatSettingsForm.ValidData(out AControl: TWinControl;
  out AMsg: String): Boolean;
begin
  Result := false;
  if FCbDecimalSeparator.Separator = FCbThousandSeparator.Separator then
  begin
    AControl := FCbDecimalSeparator;
    AMsg := 'Decimal and thousand separators cannot be the same.';
    exit;
  end;
  Result := true;
end;

//initialization
//  {$I sformatsettingsform.lrs}

end.

