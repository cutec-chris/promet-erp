{**************************************************************************************************}
{                                                                                                  }
{  Y core library (Ycl)                                                                            }
{                                                                                                  }
{  YclFileUtils - file utility functions                                                           }
{                                                                                                  }
{  The contents of this file are subject to the Y Library Public License Version 1.0 (the          }
{  "License"); you may not use this file except in compliance with the License. You may obtain a   }
{  copy of the License at http://delphi.pjh2.de/                                                   }
{                                                                                                  }
{  Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF  }
{  ANY KIND, either express or implied. See the License for the specific language governing rights }
{  and limitations under the License.                                                              }
{                                                                                                  }
{  The Original Code is: YclStrings.pas.                                                           }
{  The Initial Developer of the Original Code is Peter J. Haas (libs@pjh2.de). Portions created    }
{  by Peter J. Haas are Copyright (C) 1996-2005 Peter J. Haas. All Rights Reserved.                }
{                                                                                                  }
{  Contributor(s):                                                                                 }
{                                                                                                  }
{  You may retrieve the latest version of this file at the homepage of Peter J. Haas, located at   }
{  http://delphi.pjh2.de/                                                                          }
{                                                                                                  }
{**************************************************************************************************}

// For history see end of file

{$I Ycl.inc}

unit YclStrings;

interface
uses
  {$IFDEF WIN32}
  Windows,
  {$ENDIF WIN32}
  SysUtils, Classes,
  YclBase, YclUtils;
  
const
  WindowsLineSeparator = #$0D#$0A;
  UnixLineSeparator = #$0A;
  {$IFDEF WIN32}
  OSLineSeparator = WindowsLineSeparator;
  {$ENDIF WIN32}
  {$IFDEF UNIX}
  OSLineSeparator = UnixLineSeparator;
  {$ENDIF UNIX}

function Fetch(var Value: String; const Separator: String): String; overload;
function Fetch(var Value: String; Separators: TSysCharSet): String; overload;

// beliebige Newlines zu einzelnem LF wandeln
function ConvertNewLineToLF(const Value: String): String;

// array of Char nach String kopieren, bis zum ersten #0, maximal Array-Länge
function CharArrayToStr(Value: array of Char) : String;

// array of String nach TString kopieren
procedure StringArrayToStrings(const Src: array of String; Dst: TStrings);

// wandelt erstes Zeichen in einen Großbuchstaben um
function UpperFirstChar(const Value: String): String;

// wandelt erstes Zeichen jedes Wortes in einen Großbuchstaben um
// ExceptionList: Liste von Ausnahmen
function UpperFirstCharInEveryWord(const Value: String): String;
function UpperFirstCharInWord(const Value: String; ExceptionList: array of String): String; 

// entfernt alle Zeichen, die nicht in ValidChars vorhanden sind
function RemoveInvalidCharacters(const Value: String; ValidChars: TSysCharSet): String;

// entfernt führendes bestimmtes Zeichen
function RemoveLeadingSpecificCharacter(const Value: String; SpecificCharacter: Char): String;

// Konvertierungen zwischen TString, array of String und nullterminierten Stringlisten
// Empty list entries will be deleted and a assertion created
function StringListToMultiAnsiString(const List: array of AnsiString): AnsiString; overload;
function StringListToMultiAnsiString(const List: TDynArrayAnsiString): AnsiString; overload;
function StringListToMultiAnsiString(const List: TStrings): AnsiString; overload;

function StringListToMultiAnsiStringPrefix(const Prefix: AnsiString; List: array of AnsiString): AnsiString;

procedure MultiAnsiStringToStringList(const Value: AnsiString; out DestList: TDynArrayAnsiString); overload;
procedure MultiAnsiStringToStringList(const Value: AnsiString; DestList: TStrings); overload;

function StringListToMultiWideString(const List: array of WideString): WideString; overload;
function StringListToMultiWideString(const List: TDynArrayWideString): WideString; overload;
function StringListToMultiWideString(const List: TStrings): WideString; overload;

procedure MultiWideStringToStringList(const Value: WideString; out DestList: TDynArrayWideString); overload;
procedure MultiWideStringToStringList(const Value: WideString; DestList: TStrings); overload;

implementation

function Fetch(var Value: String; const Separator: String): String;
var
  i: Integer;
begin
  i := Pos(Separator, Value);
  if i <= 0 then begin
    Result := Value;
    Value := '';
  end
  else begin
    Result := Copy(Value, 1, i - 1);
    Delete(Value, 1, i + Length(Separator) - 1);
  end;
end;

function Fetch(var Value: String; Separators: TSysCharSet): String;
var
  Idx: Integer;
begin
  for Idx := 1 to Length(Value) do begin
    if Value[Idx] in Separators then begin
      Result := Copy(Value, 1, Idx - 1);
      Delete(Value, 1, Idx);
      Exit;
    end;
  end;
  Result := Value;
  Value := '';
end;

function ConvertNewLineToLF(const Value: String): String;
var
  SrcPtr: PChar;
  DstIdx: Integer;

  procedure AddChar(c: Char);
  begin
    Result[DstIdx] := c;
    Inc(DstIdx);
  end;

begin
  SetLength(Result, Length(Value));
  DstIdx := 1;
  SrcPtr := Pointer(Value);
  while SrcPtr^ <> #0 do begin
    case SrcPtr^ of
      #$0D: begin
        Inc(SrcPtr);
        if SrcPtr^ = #$0A then
          Inc(SrcPtr);
        AddChar(#$0A);
      end;
      #$0A: begin
        Inc(SrcPtr);
        if SrcPtr^ = #$0D then
          Inc(SrcPtr);
        AddChar(#$0A);
      end;
    else
      AddChar(SrcPtr^);
      Inc(SrcPtr);
    end;
  end;
  SetLength(Result, DstIdx - 1);
end;

function CharArrayToStr(Value: array of Char) : String;
var
  Len, i: Integer;
begin
  Len := Length(Value);
  for i := Low(Value) to High(Value) do begin
    if Value[i] = #0 then begin
      Len := i - Low(Value);
      Break;
    end;
  end;
  if Len = 0 then
    Result := ''
  else
    SetString(Result, PChar(@Value[Low(Value)]), Len);
end;

procedure StringArrayToStrings(const Src: array of String; Dst: TStrings);
var
  i: Integer;
begin
  for i := Low(Src) to High(Src) do
    Dst.Add(Src[i]);
end;

// wandelt erstes Zeichen in einen Großbuchstaben um
function UpperFirstChar(const Value: String): String;
begin
  Result := Value;
  if Length(Result) >= 1 then
    CharUpperBuff(@Result[1], 1);
end;

// wandelt erstes Zeichen jedes Wortes in einen Großbuchstaben um
// ExceptionList: Liste von Ausnahmen
function UpperFirstCharInEveryWord(const Value: String): String;
var
  PtrStart, Ptr: PChar;
begin
  Result := Value;
  UniqueString(Result);
  PtrStart := PChar(Result);
  while PtrStart^ <> #0 do begin
    Ptr := PtrStart;
    while not (Ptr^ in [#0..'/', ':'..'?', '['..'_', '{'..#$7F]) do  // Trennzeichen suchen
      Inc(Ptr);
    if PtrStart <> Ptr then   // Wort gefunden
      CharUpperBuff(PtrStart, 1);  // erstes Zeichen wandeln
    PtrStart := Ptr;
    if PtrStart^ <> #0 then  // erstes Zeichen nach Trennzeichen
      Inc(PtrStart);
  end;
end;

function UpperFirstCharInWord(const Value: String; ExceptionList: array of String): String;
var
  PtrStart, Ptr: PChar;
  Word: String;
  i: Integer;
begin
  Result := Value;
  UniqueString(Result);
  PtrStart := PChar(Result);
  while PtrStart^ <> #0 do begin
    Ptr := PtrStart;
    while not (Ptr^ in [#0..'/', ':'..'?', '['..'_', '{'..#$7F]) do  // Trennzeichen suchen
      Inc(Ptr);
    if PtrStart <> Ptr then begin  // Wort gefunden
      SetString(Word, PtrStart, Ptr - PtrStart);
      // prüfen, ob in ExceptionList
      i := High(ExceptionList);
      while (i >= Low(ExceptionList)) and (CompareText(ExceptionList[i], Word) <> 0) do
        Dec(i);
      if i < 0 then  // nicht in Ausnahme-Liste
        CharUpperBuff(PtrStart, 1);  // erstes Zeichen wandeln
    end;
    PtrStart := Ptr;
    if PtrStart^ <> #0 then  // erstes Zeichen nach Trennzeichen
      Inc(PtrStart);
  end;
end;

function RemoveInvalidCharacters(const Value: String; ValidChars: TSysCharSet): String;
var
  Len, SrcIdx, DstIdx, ValidCount: Cardinal;
  Ch: Char;
begin
  Len := Length(Value);
  ValidCount := 0;
  // Anzahl gültiger Zeichen ermitteln
  for SrcIdx := 1 to Len do begin
    if Value[SrcIdx] in ValidChars then
      Inc(ValidCount);
  end;
  SetLength(Result, ValidCount);
  DstIdx := 1;
  for SrcIdx := 1 to Len do begin
    Ch := Value[SrcIdx];
    if Ch in ValidChars then begin
      Result[DstIdx] := Ch;
      Inc(DstIdx);
    end;
  end;
end;

// entfernt führendes bestimmtes Zeichen
function RemoveLeadingSpecificCharacter(const Value: String; SpecificCharacter: Char): String;
var
  Idx: Integer;
begin
  Result := '';
  for Idx := 1 to Length(Value) do begin
    if Value[Idx] <> SpecificCharacter then begin
      Result := Copy(Value, Idx, MaxInt);
      Break;
    end;
  end;
end;

// *******************************************************************************************

function StringListToMultiAnsiString(const List: array of AnsiString): AnsiString;
var
  LenSum, Len, i: Integer;
  DstPtr: PAnsiChar;
  ListItem: AnsiString;
begin
  Result := '';
  // calculate length
  LenSum := 0;
  for i := Low(List) to High(List) do begin
    Len := Length(List[i]);
    Assert(Len > 0, 'StringListToMultiAnsiString with empty item');
    if Len > 0 then
      Inc(LenSum, Len + 1);
  end;
  // copy the contents
  SetLength(Result, LenSum);
  DstPtr := PAnsiChar(Result);
  for i := Low(List) to High(List) do begin
    ListItem := List[i];
    if ListItem <> '' then
      DstPtr := CopyMemE(DstPtr, Pointer(ListItem), Length(ListItem) + 1);
  end;
end;

function StringListToMultiAnsiStringPrefix(const Prefix: AnsiString; List: array of AnsiString): AnsiString;
var
  LenSum, LenPrefix, Len, i: Integer;
  DstPtr: PAnsiChar;
  ListItem: AnsiString;
begin
  Result := '';
  // calculate length
  LenPrefix := Length(Prefix);
  LenSum := 0;
  for i := Low(List) to High(List) do begin
    Len := Length(List[i]);
    Assert(Len > 0, 'StringListToMultiAnsiString with empty item');
    if Len > 0 then
      Inc(LenSum, LenPrefix + Len + 1);
  end;
  // copy the contents
  SetLength(Result, LenSum);
  DstPtr := PAnsiChar(Result);
  for i := Low(List) to High(List) do begin
    ListItem := List[i];
    if ListItem <> '' then begin
      DstPtr := CopyMemE(DstPtr, Pointer(Prefix), LenPrefix);
      DstPtr := CopyMemE(DstPtr, Pointer(ListItem), Length(ListItem) + 1);
    end;
  end;
end;

function StringListToMultiWideString(const List: array of WideString): WideString;
var
  LenSum, Len, i: Integer;
  DstPtr: PWideChar;
  ListItem: WideString;
begin
  Result := '';
  // calculate length
  LenSum := 0;
  for i := Low(List) to High(List) do begin
    Len := Length(List[i]);
    Assert(Len > 0, 'StringListToMultiWideString with empty item');
    if Len > 0 then
      Inc(LenSum, Len + 1);
  end;
  // copy the contents
  SetLength(Result, LenSum);
  DstPtr := PWideChar(Result);
  for i := Low(List) to High(List) do begin
    ListItem := List[i];
    if ListItem <> '' then
      DstPtr := CopyMemE(DstPtr, Pointer(ListItem), (Length(ListItem) + 1) * SizeOf(WideChar));
  end;
end;

function StringListToMultiAnsiString(const List: TDynArrayAnsiString): AnsiString;
var
  LenSum, Len, i: Integer;
  DstPtr: PAnsiChar;
  ListItem: AnsiString;
begin
  Result := '';
  // calculate length
  LenSum := 0;
  for i := Low(List) to High(List) do begin
    Len := Length(List[i]);
    Assert(Len > 0, 'StringListToMultiAnsiString with empty item');
    if Len > 0 then
      Inc(LenSum, Len + 1);
  end;
  // Copy the contents
  SetLength(Result, LenSum);
  DstPtr := PAnsiChar(Result);
  for i := Low(List) to High(List) do begin
    ListItem := List[i];
    if ListItem <> '' then
      DstPtr := CopyMemE(DstPtr, Pointer(ListItem), Length(ListItem) + 1);
  end;
end;

function StringListToMultiWideString(const List: TDynArrayWideString): WideString;
var
  LenSum, Len, i: Integer;
  DstPtr: PWideChar;
  ListItem: WideString;
begin
  Result := '';
  // calculate length
  LenSum := 0;
  for i := Low(List) to High(List) do begin
    Len := Length(List[i]);
    Assert(Len > 0, 'StringListToMultiWideString with empty item');
    if Len > 0 then
      Inc(LenSum, Len + 1);
  end;
  // copy the contents
  SetLength(Result, LenSum);
  DstPtr := PWideChar(Result);
  for i := Low(List) to High(List) do begin
    ListItem := List[i];
    if ListItem <> '' then
      DstPtr := CopyMemE(DstPtr, Pointer(ListItem), (Length(ListItem) + 1) * SizeOf(WideChar));
  end;
end;

function StringListToMultiAnsiString(const List: TStrings): AnsiString;
var
  LenSum, Len, i: Integer;
  DstPtr: PAnsiChar;
  ListItem: AnsiString;
begin
  Result := '';
  if not Assigned(List) then
    Exit;
  // calculate length
  LenSum := 0;
  for i := 0 to List.Count - 1 do begin
    Len := Length(List[i]);
    Assert(Len > 0, 'StringListToMultiAnsiString with empty item');
    if Len > 0 then
      Inc(LenSum, Len + 1);
  end;
  // copy the contents
  SetLength(Result, LenSum);
  DstPtr := PAnsiChar(Result);
  for i := 0 to List.Count - 1 do begin
    ListItem := List[i];
    if ListItem <> '' then
      DstPtr := CopyMemE(DstPtr, Pointer(ListItem), Length(ListItem) + 1);
  end;
end;

function StringListToMultiWideString(const List: TStrings): WideString;
var
  LenSum, Len, i: Integer;
  DstPtr: PWideChar;
  ListItem: WideString;
begin
  Result := '';
  if not Assigned(List) then
    Exit;
  // calculate length
  LenSum := 0;
  for i := 0 to List.Count - 1 do begin
    Len := Length(List[i]);
    Assert(Len > 0, 'StringListToMultiWideString with empty item');
    if Len > 0 then
      Inc(LenSum, Len + 1);
  end;
  // copy the contents
  SetLength(Result, LenSum);
  DstPtr := PWideChar(Result);
  for i := 0 to List.Count - 1 do begin
    ListItem := List[i];
    if ListItem <> '' then
      DstPtr := CopyMemE(DstPtr, Pointer(ListItem), (Length(ListItem) + 1) * SizeOf(WideChar));
  end;
end;

// *******************************************************************************************

procedure MultiAnsiStringToStringList(const Value: AnsiString; out DestList: TDynArrayAnsiString);
var
  PtrValueEnd, PtrStart, PtrEnd: PAnsiChar;
  Count, i: Integer;
  ListItem: AnsiString;
begin
  // get count
  PtrStart := PAnsiChar(Value);
  PtrValueEnd := PtrStart + Length(Value);
  Count := 0;
  while PtrStart^ <> #0 do begin
    PtrEnd := StrEnd(PtrStart);
    Inc(Count);
    // single string or missing the double null at end
    if PtrEnd >= PtrValueEnd then
      Break;
    PtrStart := PtrEnd + 1;
  end;
  SetLength(DestList, Count);
  // get items
  PtrStart := PAnsiChar(Value);
  for i := Low(DestList) to High(DestList) do begin
    PtrEnd := StrEnd(PtrStart);
    SetString(ListItem, PtrStart, PtrEnd - PtrStart);
    DestList[i] := ListItem;
    PtrStart := PtrEnd + 1;
  end;
end;

procedure MultiWideStringToStringList(const Value: WideString; out DestList: TDynArrayWideString);
var
  PtrValueEnd, PtrStart, PtrEnd: PWideChar;
  Count, i: Integer;
  ListItem: WideString;
begin
  // get count
  PtrStart := PWideChar(Value);
  PtrValueEnd := PtrStart + Length(Value);
  Count := 0;
  while PtrStart^ <> #0 do begin
    PtrEnd := PtrStart;
    while PtrEnd^ <> #0 do
      Inc(PtrEnd);
    Inc(Count);
    // single string or missing the double null at end
    if PtrEnd >= PtrValueEnd then
      Break;
    PtrStart := PtrEnd + 1;
  end;
  SetLength(DestList, Count);
  // get items
  PtrStart := PWideChar(Value);
  for i := Low(DestList) to High(DestList) do begin
    PtrEnd := PtrStart;
    while PtrEnd^ <> #0 do
      Inc(PtrEnd);
    SetString(ListItem, PtrStart, PtrEnd - PtrStart);
    DestList[i] := ListItem;
    PtrStart := PtrEnd + 1;
  end;
end;

procedure MultiAnsiStringToStringList(const Value: AnsiString; DestList: TStrings);
var
  PtrValueEnd, PtrStart, PtrEnd: PAnsiChar;
  ListItem: AnsiString;
begin
  Assert(Assigned(DestList));
  if not Assigned(DestList) then
    Exit;    { TODO : Exception? }
  DestList.BeginUpdate;
  try
    DestList.Clear;
    PtrStart := PAnsiChar(Value);
    PtrValueEnd := PtrStart + Length(Value);
    while (PtrStart^ <> #0) do begin
      PtrEnd := StrEnd(PtrStart);
      SetString(ListItem, PtrStart, PtrEnd - PtrStart);
      DestList.Add(ListItem);
      // single string or missing the double null at end
      if PtrEnd >= PtrValueEnd then
        Break;
      PtrStart := PtrEnd + 1;
    end;
  finally
    DestList.EndUpdate;
  end;
end;

procedure MultiWideStringToStringList(const Value: WideString; DestList: TStrings);
var
  PtrValueEnd, PtrStart, PtrEnd: PWideChar;
  ListItem: WideString;
begin
  Assert(Assigned(DestList));
  if not Assigned(DestList) then
    Exit;    { TODO : Exception? }
  DestList.BeginUpdate;
  try
    DestList.Clear;
    PtrStart := PWideChar(Value);
    PtrValueEnd := PtrStart + Length(Value);
    while (PtrStart^ <> #0) do begin
      PtrEnd := PtrStart;
      while PtrEnd^ <> #0 do
        Inc(PtrEnd);
      SetString(ListItem, PtrStart, PtrEnd - PtrStart);
      DestList.Add(ListItem);  { TODO : or other convert? }
      // single string or missing the double null at end
      if PtrEnd >= PtrValueEnd then
        Break;
      PtrStart := PtrEnd + 1;
    end;
  finally
    DestList.EndUpdate;
  end;
end;

// *******************************************************************************************

//  History:
//  2005-08-27, Peter J. Haas
//   - add StringListToMultiAnsiString, MultiAnsiStringToStringList,
//         StringListToMultiWideString, MultiWideStringToStringList,
//         StringListToMultiAnsiStringPrefix
//
//  2005-08-23, Peter J. Haas
//   - add UpperFirstCharInEveryWord, UpperFirstCharInWord
//
//  2005-08-20, Peter J. Haas
//   - add RemoveInvalidCharacters, RemoveLeadingSpecificCharacter
//
//  2005-07-08, Peter J. Haas
//   - add UpperFirstChar (from other unit)
//
//  2005-02-09, Peter J. Haas
//   - YCL version

end.
