{
  Utility functions from FPSpreadsheet
}
unit fpsutils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StrUtils;

// Exported types
type
  TsSelectionDirection = (fpsVerticalSelection, fpsHorizontalSelection);

// Endianess helper functions
function WordToLE(AValue: Word): Word;
function DWordToLE(AValue: Cardinal): Cardinal;
function IntegerToLE(AValue: Integer): Integer;
function WideStringToLE(const AValue: WideString): WideString;

function WordLEtoN(AValue: Word): Word;
function DWordLEtoN(AValue: Cardinal): Cardinal;
function WideStringLEToN(const AValue: WideString): WideString;

// Other routines
function ParseIntervalString(const AStr: string;
  var AFirstCellRow, AFirstCellCol, ACount: Integer;
  var ADirection: TsSelectionDirection): Boolean;
function ParseCellString(const AStr: string;
  var ACellRow, ACellCol: Integer): Boolean;
function ParseCellRowString(const AStr: string;
  var AResult: Integer): Boolean;
function ParseCellColString(const AStr: string;
  var AResult: Integer): Boolean;

implementation

{
  Endianess helper functions

  Excel files are all written with Little Endian byte order,
  so it's necessary to swap the data to be able to build a
  correct file on big endian systems.

  These routines are preferable to System unit routines because they
  ensure that the correct overloaded version of the conversion routines
  will be used, avoiding typecasts which are less readable.

  They also guarantee delphi compatibility. For Delphi we just support
  big-endian isn't support, because Delphi doesn't support it.
}

function WordToLE(AValue: Word): Word;
begin
  {$IFDEF FPC}
    Result := NtoLE(AValue);
  {$ELSE}
    Result := AValue;
  {$ENDIF}
end;

function DWordToLE(AValue: Cardinal): Cardinal;
begin
  {$IFDEF FPC}
    Result := NtoLE(AValue);
  {$ELSE}
    Result := AValue;
  {$ENDIF}
end;

function IntegerToLE(AValue: Integer): Integer;
begin
  {$IFDEF FPC}
    Result := NtoLE(AValue);
  {$ELSE}
    Result := AValue;
  {$ENDIF}
end;

function WordLEtoN(AValue: Word): Word;
begin
  {$IFDEF FPC}
    Result := LEtoN(AValue);
  {$ELSE}
    Result := AValue;
  {$ENDIF}
end;

function DWordLEtoN(AValue: Cardinal): Cardinal;
begin
  {$IFDEF FPC}
    Result := LEtoN(AValue);
  {$ELSE}
    Result := AValue;
  {$ENDIF}
end;

function WideStringToLE(const AValue: WideString): WideString;
var
  j: integer;
begin
  {$IFDEF FPC}
    {$IFDEF FPC_LITTLE_ENDIAN}
      Result:=AValue;
    {$ELSE}
      Result:=AValue;
      for j := 1 to Length(AValue) do begin
        PWORD(@Result[j])^:=NToLE(PWORD(@Result[j])^);
      end;
    {$ENDIF}
  {$ELSE}
    Result:=AValue;
  {$ENDIF}
end;

function WideStringLEToN(const AValue: WideString): WideString;
var
  j: integer;
begin
  {$IFDEF FPC}
    {$IFDEF FPC_LITTLE_ENDIAN}
      Result:=AValue;
    {$ELSE}
      Result:=AValue;
      for j := 1 to Length(AValue) do begin
        PWORD(@Result[j])^:=LEToN(PWORD(@Result[j])^);
      end;
    {$ENDIF}
  {$ELSE}
    Result:=AValue;
  {$ENDIF}
end;

{@@
  Parses strings like A5:A10 into an selection interval information
}
function ParseIntervalString(const AStr: string;
  var AFirstCellRow, AFirstCellCol, ACount: Integer;
  var ADirection: TsSelectionDirection): Boolean;
var
  Cells: TStringList;
  LastCellRow, LastCellCol: Integer;
begin
  Result := True;

  // First get the cells
  Cells := TStringList.Create;
  ExtractStrings([':'],[], PChar(AStr), Cells);

  // Then parse each of them
  Result := ParseCellString(Cells[0], AFirstCellRow, AFirstCellCol);
  if not Result then Exit;
  Result := ParseCellString(Cells[1], LastCellRow, LastCellCol);
  if not Result then Exit;

  if AFirstCellRow = LastCellRow then
  begin
    ADirection := fpsHorizontalSelection;
    ACount := LastCellCol - AFirstCellCol + 1;
  end
  else if AFirstCellCol = LastCellCol then
  begin
    ADirection := fpsVerticalSelection;
    ACount := LastCellRow - AFirstCellRow + 1;
  end
  else Exit(False);
end;

{@@
  Parses a cell string, like 'A1' into zero-based column and row numbers

  The parser is a simple state machine, with the following states:

  0 - Reading Column part 1 (necesserely needs a letter)
  1 - Reading Column part 2, but could be the first number as well
  2 - Reading Row
}
function ParseCellString(const AStr: string; var ACellRow, ACellCol: Integer): Boolean;
var
  i: Integer;
  state: Integer;
  Col, Row: string;
  lChar: Char;
const
  cLetters = ['A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L',
   'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'W', 'X', 'Y', 'Z'];
  cDigits = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9'];
begin
  // Starting state
  Result := True;
  state := 0;
  Col := '';
  Row := '';

  // Separates the string into a row and a col
  for i := 0 to Length(AStr) - 1 do
  begin
    lChar := AStr[i + 1];

    case state of

    0:
    begin
      if lChar in cLetters then
      begin
        Col := lChar;
        state := 1;
      end
      else Exit(False);
    end;

    1:
    begin
      if lChar in cLetters then Col := Col + lChar
      else if lChar in cDigits then
      begin
        Row := lChar;
        state := 2;
      end
      else Exit(False);
    end;

    2:
    begin
      if lChar in cDigits then Row := Row + lChar
      else Exit(False);
    end;

    end;
  end;

  // Now parses each separetely
  ParseCellRowString(Row, ACellRow);
  ParseCellColString(Col, ACellCol);
end;

function ParseCellRowString(const AStr: string; var AResult: Integer): Boolean;
begin
  try
    AResult := StrToInt(AStr) - 1;
  except
    Result := False;
  end;
  Result := True;
end;

function ParseCellColString(const AStr: string; var AResult: Integer): Boolean;
const
  INT_NUM_LETTERS = 26;
begin
  Result := False;
  AResult := 0;

  if Length(AStr) = 1 then AResult := Ord(AStr[1]) - Ord('A')
  else if Length(AStr) = 2 then
  begin
    AResult := (Ord(AStr[1]) - Ord('A') + 1) * INT_NUM_LETTERS
     + Ord(AStr[2]) - Ord('A');
  end
  else Exit(False);

  Result := True;
end;

end.

