unit sCtrls;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, StdCtrls, Grids, EditBtn, Forms;

type
  { TMonthDayNamesEdit }
  TMonthDayNamesEdit = class(TEditButton)
  private
    FEmptyString: String;
    FCount: Integer;
    FShortnames: Boolean;
    procedure ButtonClickHandler(Sender: TObject);
    function CreateMonthDayNamesEditor(var AGrid: TStringGrid): TForm;
  protected
  public
    constructor Create(AOwner: TComponent); override;
    procedure GetNames(var ANamesArray);
    procedure SetNames(const ANamesArray; ACount: Integer; IsShortNames: Boolean;
      const AEmptyString: String);
  end;

  { TFormatSeparatorCombo }
  TFormatSeparatorKind = (skDecimal, skThousand, skDate, skTime, skList);

  TFormatSeparatorCombo = class(TCombobox)
  private
    FKind: TFormatSeparatorKind;
    function GetSeparator: Char;
    procedure SetSeparator(AValue: Char);
    procedure SetSeparatorKind(AValue: TFormatSeparatorKind);
  public
    property Separator: Char read GetSeparator write SetSeparator;
    property SeparatorKind: TFormatSeparatorKind read FKind write SetSeparatorKind;
  end;


implementation

uses
  Math, ButtonPanel, fpsUtils;

{@@ ----------------------------------------------------------------------------
  Concatenates the day names specified in ADayNames to a single string. If all
  daynames are empty AEmptyStr is returned

  @param   ADayNames   Array[1..7] of day names as used in the Formatsettings
  @param   AEmptyStr   Is returned if all day names are empty
  @return  String having all day names concatenated and separated by the
           DefaultFormatSettings.ListSeparator
-------------------------------------------------------------------------------}
function DayNamesToString(const ADayNames: TWeekNameArray;
  const AEmptyStr: String): String;
var
  i: Integer;
  isEmpty: Boolean;
begin
  isEmpty := true;
  for i:=1 to 7 do
    if ADayNames[i] <> '' then
    begin
      isEmpty := false;
      break;
    end;

  if isEmpty then
    Result := AEmptyStr
  else
  begin
    Result := ADayNames[1];
    for i:=2 to 7 do
      Result := Result + DefaultFormatSettings.ListSeparator + ' ' + ADayNames[i];
  end;
end;

{@@ ----------------------------------------------------------------------------
  Concatenates the month names specified in AMonthNames to a single string.
  If all month names are empty AEmptyStr is returned

  @param   AMonthNames  Array[1..12] of month names as used in the Formatsettings
  @param   AEmptyStr    Is returned if all month names are empty
  @return  String having all month names concatenated and separated by the
           DefaultFormatSettings.ListSeparator
-------------------------------------------------------------------------------}
function MonthNamesToString(const AMonthNames: TMonthNameArray;
  const AEmptyStr: String): String;
var
  i: Integer;
  isEmpty: Boolean;
begin
  isEmpty := true;
  for i:=1 to 12 do
    if AMonthNames[i] <> '' then
    begin
      isEmpty := false;
      break;
    end;

  if isEmpty then
    Result := AEmptyStr
  else
  begin
    Result := AMonthNames[1];
    for i:=2 to 12 do
      Result := Result + DefaultFormatSettings.ListSeparator + ' ' + AMonthNames[i];
  end;
end;

{ TMonthDayNamesEdit }

constructor TMonthDayNamesEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Button.Caption := '...';
  OnButtonClick := @ButtonClickHandler;
end;

procedure TMonthDayNamesEdit.ButtonClickHandler(Sender: TObject);
var
  F: TForm;
  i: Integer;
  grid: TStringGrid = nil;
  names: TMonthNameArray;  // can hold day and month names as well
begin
  F := CreateMonthDayNamesEditor(grid);
  try
    if F.ShowModal = mrOK then
    begin
      for i:=1 to 12 do
        names[i] := '';
      for i:=1 to grid.RowCount-1 do
        names[i] := grid.Cells[1, i];
      SetNames(names, FCount, FShortNames, FEmptyString);
    end;
  finally
    F.Free;
  end;
end;

function TMonthDayNamesEdit.CreateMonthDayNamesEditor(var AGrid: TStringGrid): TForm;
var
  btnPanel: TButtonPanel;
  i: Integer;
  R: TRect;
  Pt: TPoint;
  w: Integer;
  names: TMonthNameArray; // has space for both months and days...
begin
  Result := TForm.Create(nil);
  btnPanel := TButtonPanel.Create(Result);
  with btnPanel do begin
    Parent := Result;
    ShowButtons := [pbOK, pbCancel];
  end;
  AGrid := TStringGrid.Create(Result);
  with AGrid do begin
    Parent := Result;
    Align := alClient;
    BorderSpacing.Around := 8;
    TitleStyle := tsNative;
    Options := Options + [goEditing, goAlwaysShowEditor] - [goVertLine];
    DefaultColWidth := 150;
    AutoFillColumns := true;
    ColCount := 2;
    RowCount := FCount+1;
    if FCount = 12 then
    begin
      Cells[0, 1] := 'January';
      Cells[0, 2] := 'February';
      Cells[0, 3] := 'March';
      Cells[0, 4] := 'April';
      Cells[0, 5] := 'May';
      Cells[0, 6] := 'June';
      Cells[0, 7] := 'July';
      Cells[0, 8] := 'August';
      Cells[0, 9] := 'September';
      Cells[0,10] := 'October';
      Cells[0,11] := 'November';
      Cells[0,12] := 'December';
      if FShortNames then
        Cells[1, 0] := 'Short month names'
      else
        Cells[1, 0] := 'Long month names';
    end else
    begin
      Cells[0, 1] := 'Sunday';
      Cells[0, 2] := 'Monday';
      Cells[0, 3] := 'Tuesday';
      Cells[0, 4] := 'Wesdnesday';
      Cells[0, 5] := 'Thursday';
      Cells[0, 6] := 'Friday';
      Cells[0, 7] := 'Saturday';
      if FShortNames then
        Cells[1, 0] := 'Short day names'
      else
        Cells[1, 0] := 'Long day names';
    end;
    names[1] := '';  // to silence the compiler...
    GetNames(names);
    w := 0;
    for i:=1 to FCount do
    begin
      Cells[1, i] := TMonthNameArray(names)[i];
      w := Max(w, Canvas.TextWidth(Cells[0, i]));
    end;
    ColWidths[0] := w + 16;
    ColWidths[1] := 2*w;
    R := CellRect(ColCount-1, RowCount-1);
  end;
  Pt := Result.ScreenToClient(AGrid.ClientToScreen(R.BottomRight));
  Result.Width := AGrid.width + AGrid.BorderSpacing.Around*2 + 5;
  Result.Height := Pt.Y + btnPanel.Height + AGrid.BorderSpacing.Around*2 - 6;
  Result.Position := poMainFormCenter;
  Result.ActiveControl := AGrid;
end;

procedure TMonthDayNamesEdit.GetNames(var ANamesArray);
{ Not very nice code here: will crash if a TWeekNameArray is passed as ANameArray,
  but the edit stores month data! Watch out... }
var
  L: TStringList;
  i: Integer;
begin
  for i:=1 to FCount do
    TMonthNameArray(ANamesArray)[i] := '';
  if Text <> FEmptyString then
  begin
    L := TStringList.Create;
    try
      L.Delimiter := DefaultFormatSettings.ListSeparator;
      L.DelimitedText := Text;
      for i:=0 to L.Count-1 do
        if i < L.Count then
          TMonthNameArray(ANamesArray)[i+1] := L[i];
    finally
      L.Free;
    end;
  end;
end;

procedure TMonthDayNamesEdit.SetNames(const ANamesArray; ACount: Integer;
  IsShortNames: Boolean; const AEmptyString: String);
begin
  if not ACount in [7, 12] then
    raise Exception.Create('[TMonthDayNameEdit] Array length can only be 7 or 12.');

  FCount := ACount;
  FEmptyString := AEmptyString;
  FShortNames := IsShortNames;

  case FCount of
     7: Text := DayNamesToString(TWeekNameArray(ANamesArray), AEmptyString);
    12: Text := MonthNamesToString(TMonthNameArray(ANamesArray), AEmptyString);
    else raise Exception.Create('[TMonthDayNameEdit] Array length can only be 7 or 12.');
  end;
end;


{ TFormatSeparatorCombo }

function TFormatSeparatorCombo.GetSeparator: Char;
begin
  if ItemIndex = -1 then
  begin
    if Text = '' then
      Result := #0
    else
      Result := Text[1];
  end else
    Result := Char(PtrInt(items.Objects[ItemIndex]));
end;

procedure TFormatSeparatorCombo.SetSeparator(AValue: Char);
var
  i: Integer;
begin
  i := Items.IndexOfObject(TObject(PtrInt(ord(AValue))));
  if i = -1 then
    Text := AValue
  else
    ItemIndex := i;
end;

procedure TFormatSeparatorCombo.SetSeparatorKind(AValue: TFormatSeparatorKind);
begin
  FKind := AValue;
  Items.BeginUpdate;
  try
    case FKind of
      skDecimal, skThousand:
        begin
          Items.AddObject('Dot ( . )', TObject(PtrInt(ord('.'))));
          Items.AddObject('Comma ( , )', TObject(PtrInt(ord(','))));
          if FKind = skThousand then
            Items.AddObject('Space ( )', TObject(PtrInt(ord(' '))));
        end;
      skDate, skTime:
        begin
          Items.AddObject('Dot ( . )', TObject(PtrInt(ord('.'))));
          Items.AddObject('Dash ( - )', TObject(PtrInt(ord('-'))));
          Items.AddObject('Slash ( / )', TObject(PtrInt(ord('/'))));
          if FKind = skTime then
            Items.AddObject('Colon ( : )', TObject(PtrInt(ord(':'))));
        end;
      skList:
        begin
          Items.AddObject('Dot ( . )', TObject(PtrInt(ord('.'))));
          Items.AddObject('Comma ( , )', TObject(PtrInt(ord(','))));
          Items.AddObject('Semicolon ( ; )', TObject(PtrInt(ord(';'))));
          Items.AddObject('Colon ( : )', TObject(PtrInt(ord(':'))));
          Items.AddObject('Bar ( | )', TObject(PtrInt(ord('|'))));
          Items.AddObject('Slash ( / )', TObject(PtrInt(ord('/'))));
          Items.AddObject('Backslash ( \ )', TObject(PtrInt(ord('\'))));
        end;
    end;
  finally
    Items.EndUpdate;
  end;
end;

end.

