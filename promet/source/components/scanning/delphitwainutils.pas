{GENERAL METHODS USED BY TWAIN DELPHI}
{december 2001®, made by Gustavo Daud}

{This unit contains general methods used by Delphi}
{Twain component. Some of the methods bellow aren't}
{directly related to Twain, but are pieces needed}
{to implement the component.}

unit DelphiTwainUtils;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}
{$INCLUDE DELPHITWAIN.INC}

interface

{$IFDEF WINDOWS}

uses
  Twain;

type
  {Kinds of directories to be obtained with GetCustomDirectory}
  TDirectoryKind = (dkWindows, dkSystem, dkCurrent, dkApplication, dkTemp);

  {Class to store a list of pointers}
  TPointerList = class
  private
    {Stores pointer to the allocated data}
    Data: Pointer;
    {Contains number of additional items allocated every time}
    {it needs more data to store}
    fAdditionalBlock: Integer;
    {Contains the number of items in the list}
    fCount: Integer;
    {Contains number of allocated items}
    fAllocated: Integer;
    {Allocate/deallocate memory to have enough memory}
    {to hold the new number of items}
    procedure SetAllocated(const Value: Integer);
    {Sets the AdditionalBlock property}
    procedure SetAdditionalBlock(const Value: Integer);
    {Set the number of items in the list}
    procedure SetCount(const Value: Integer);
    function GetItem(Index: Integer): Pointer;
    procedure PutItem(Index: Integer; const Value: Pointer);
  public
    {Add a new item}
    procedure Add(Value: Pointer);
    {Clear all the items in the list}
    procedure Clear;
    {Object being created or destroyed}
    constructor Create;
    destructor Destroy; override;
    {Returns/sets an item value}
    property Item[Index: Integer]: Pointer read GetItem write PutItem; default;
    {Returns the number of items}
    property Count: Integer read fCount write SetCount;
    {Number of allocated items}
    property Allocated: Integer read fAllocated write SetAllocated;
    {Additional items to alloc when it needs more memory}
    property AdditionalBlock: Integer read fAdditionalBlock write
      SetAdditionalBlock;
  end;

{Returns custom Microsoft Windows® directories}
function GetCustomDirectory(const DirectoryKind: TDirectoryKind): String;
{Returns the last error string from Microsoft Windows®}
function GetLastErrorText(): String;
{Returns if the directory exists}
function DirectoryExists(const Directory: String): Boolean;
{Returns if the file exists}
function FileExists(const FilePath: String): Boolean;
{Extracts the file directory part}
function ExtractDirectory(const FilePath: String): String;
{Convert from integer to string}
{$IFDEF DONTUSEVCL}function IntToStr(Value: Integer): String;{$ENDIF}
{$IFDEF DONTUSEVCL}function StrToIntDef(Value: String;
  Default: Integer): Integer;{$ENDIF}
{$IFDEF DONTUSEVCL}function CompareMem(P1, P2: pChar;
  Size: Integer): Boolean;{$ENDIF}
{Convert from twain Fix32 to extended}
function Fix32ToFloat(Value: TW_FIX32): Extended;
{Convert from extended to Fix32}
function FloatToFix32 (floater: extended): TW_FIX32;

{$ENDIF}

implementation

{$IFDEF WINDOWS}

{Units used bellow}
uses
  Windows;

{$IFDEF DONTUSEVCL}
  function CompareMem(P1, P2: pChar; Size: Integer): Boolean;
  var
    i: Integer;
  begin
    {Default result}
    Result := TRUE;
    {Search each byte}
    FOR i := 1 TO Size DO
    begin
      {Compare booth bytes}
      if P1^ <> P2^ then
      begin
        Result := FALSE;
        Exit;
      end; {if P1^ <> P2^}
      {Move to next byte}
      Inc(P1); Inc(P2);
    end {FOR i}
  end {function};
{$ENDIF}

{$IFDEF DONTUSEVCL}
  function IntToStr(Value: Integer): String;
  begin
    Str(Value, Result);
  end;
{$ENDIF}

{$IFDEF DONTUSEVCL}
  function StrToIntDef(Value: String; Default: Integer): Integer;
  var Code: Integer;
  begin
    {Try converting from string to integer}
    Val(Value, Result, Code);
    {If any error ocurred, returns default value}
    if Code <> 0 then Result := Default;
  end;
{$ENDIF}


{Convert from extended to Fix32}
function FloatToFix32 (floater: extended): TW_FIX32;
var
  fracpart : extended;
begin
  //Obtain numerical part by truncating the float number
  Result.Whole := trunc(floater);
  //Obtain fracional part by subtracting float number by
  //numerical part. Also we make sure the number is not
  //negative by multipling by -1 if it is negative
  fracpart := floater - result.Whole;
  if fracpart < 0 then fracpart := fracpart * -1;
  //Multiply by 10 until there is no fracional part any longer
  while FracPart - trunc(FracPart) <> 0 do fracpart := fracpart * 10;
  //Return fracional part
  Result.Frac := trunc(fracpart);
end;

{Convert from twain Fix32 to extended}
function Fix32ToFloat(Value: TW_FIX32): Extended;
begin
  Result := Value.Whole + (Value.Frac / 65536.0);
end;

{Returns the last position for any of the characters in the parameter}
function LastPosition(const Text, characters: String): Integer;
var
  x, y: Integer;  {For loop variables}
begin
  Result := Length(Text);  {Initial result}

  {Search each character in the text}
  FOR x := 1 TO Length(Text) DO
  begin
    {Test for each character}
    FOR y := 1 TO Length(characters) DO
      if Text[x] = characters[y] then
        Result := x;
  end {for x}
end;

{Extracts the file directory}
function ExtractDirectory(const FilePath: String): String;
begin
  {Searches for the last \ or : characters}
  {ex: c:\windows\system32\yfile.ext or c:autoexec.bat}
  Result := Copy(FilePath, 1, LastPosition(FilePath, '\:'));
end;

{Returns if the file exists}
function FileExists(const FilePath: String): Boolean;
var
  FindData  : TWin32FindData;
  FindHandle: THandle;
begin
  {Searches for the file}
  FindHandle := FindFirstFile(PChar(FilePath), FindData);
  Result := (FindHandle <> INVALID_HANDLE_VALUE);
  {In case it found, closes the FindFirstFile handle}
  if Result then FindClose(FindHandle);
end;

{Returns if the directory exists}
function DirectoryExists(const Directory: String): Boolean;
var
  Attr: DWORD;
begin
  {Calls GetFileAttributes to verify}
  Attr := GetFileAttributes(PChar(Directory));
  Result := (Attr <> $FFFFFFFF) and (Attr and FILE_ATTRIBUTE_DIRECTORY <> 0);
end;

{Makes an language identifier using the two ids}
function MAKELANGID(p, s: WORD): DWORD;
begin
  Result := (s shl 10) or p;
end;

{Returns the last error string from Microsoft Windows®}
function GetLastErrorText(): String;
var
  Buffer: Array[Byte] of Char;
  Len   : DWORD;
begin
  {Calls format message to translate from the error code ID to}
  {a text understandable error}
  Len := Windows.FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM or
    FORMAT_MESSAGE_ARGUMENT_ARRAY, nil, GetLastError(),
    MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT), Buffer, sizeof(Buffer), nil);
  {Remove this chars from the ending of the result}
  while (Len > 0) and (Buffer[Len - 1] in [#0..#32, '.']) do Dec(Len);
  {Fills result}
  SetString(Result, Buffer, Len);
end;

{Includes a trailing backslash in the end of the directory; if necessary}
procedure IncludeTrailingBackslash(var Directory: String);
begin
  {If there isn't already a backslash, add one}
  if Directory[Length(Directory)] <> '\' then
    Directory := Directory + '\'
end;

{Returns custom Microsoft Windows® directories}
function GetCustomDirectory(const DirectoryKind: TDirectoryKind): String;
const
  {Default maximum size for directories}
  DEF_DIRLEN = MAX_PATH;

  {Calls appropriate method and returns necessary size}
  function CallDirectoryMethod(Buffer: Pointer; Size: UINT): UINT;
  begin
    {Test the directory needed by the parameter}
    case DirectoryKind of
      {Windows directory}
      dkWindows: Result := Windows.GetWindowsDirectory(Buffer, Size);
      {System directory}
      dkSystem : Result := Windows.GetSystemDirectory(Buffer, Size);
      {Current directory}
      dkCurrent: Result := Windows.GetCurrentDirectory(Size, Buffer);
      {Application directory}
      dkApplication: Result := Windows.GetModuleFileName(0, Buffer, Size);
      {Temp directory}
      dkTemp   : Result := Windows.GetTempPath(Size, Buffer);
      {Unknown directory}
      else Result := 0;
    end {case}
  end;

var
  DirectoryLen: UINT;
begin
  {Set length of the resulting buffer to MAX_PATH to try to hold}
  {windows directory}
  SetLength(Result, DEF_DIRLEN + 1);
  {Tries to obtain the windows directory and stores the size}
  DirectoryLen := CallDirectoryMethod(@Result[1], DEF_DIRLEN);

  {In case it was not enough to hold windows directory, enlarge}
  if DirectoryLen > DEF_DIRLEN then
  begin
    {Try again, now with the right size}
    SetLength(Result, DirectoryLen + 1);
    CallDirectoryMethod(@Result[1], DirectoryLen);
  end
  else {Otherwise, adjust the result to excluded unused data}
    SetLength(Result, DirectoryLen);

  {In case the user searched for the application directory}
  {extracts just the directory part}
  if DirectoryKind = dkApplication then
    Result := ExtractDirectory(Result);
  {Add a trailing backslash to end of the directory name}
  IncludeTrailingBackslash(Result);
end;

{ TPointerList object implementation }

{Add a new item}
procedure TPointerList.Add(Value: Pointer);
begin
  {Increase number of items and update new item}
  Count := Count + 1;
  Item[Count - 1] := Value;
end;

{Clear all the items in the list}
procedure TPointerList.Clear;
begin
  {Set number of items to 0 and initialize again allocated items}
  Count := 0;
  Allocated := AdditionalBlock;
end;

{TPointerList being created}
constructor TPointerList.Create;
begin
  {Let ancestor receive the call}
  inherited Create;

  {Allocate a number of items}
  fAdditionalBlock := 10;
  fAllocated := fAdditionalBlock;
  GetMem(Data, (fAllocated * sizeof(Pointer)));
end;

{TPointerList being destroyed}
destructor TPointerList.Destroy;
begin
  {Deallocate data}
  FreeMem(Data, (fAllocated * sizeof(Pointer)));

  {Let ancestor receive and finish}
  inherited Destroy;
end;

{Returns an item from the list}
function TPointerList.GetItem(Index: Integer): Pointer;
begin
  {Check item bounds and return item}
  if Index in [0..Count - 1] then
    {$IFDEF CPU32}
    Longint(Result) := pLongint(Longint(Data) + (Index * sizeof(Pointer)))^
    {$ENDIF}
    {$IFDEF CPU64}
    Int64(Result) := pLongint(Longint(Data) + (Index * sizeof(Pointer)))^
    {$ENDIF}
  else Result := nil; {Otherwise returns nil}
end;

{Sets an item from the list}
procedure TPointerList.PutItem(Index: Integer; const Value: Pointer);
begin
  {Check item bounds and sets item}
  if Index in [0..Count - 1] then
    pLongint(Longint(Data) + (Index * sizeof(Pointer)))^ := Longint(Value);
end;

{Sets the AdditionalBlock property}
procedure TPointerList.SetAdditionalBlock(const Value: Integer);
begin
  {Value must be a positive number greater than 0}
  if (Value > 0) then
    fAdditionalBlock := Value;
end;

{Allocate/deallocate memory to have enough memory to hold}
{the new number of items}
procedure TPointerList.SetAllocated(const Value: Integer);
begin
  {Must be always greater than 0 the number of allocated items}
  {And it also should not be smaller than count}
  if (Value > 0) and (Value <= Count) then
  begin
    {Just realloc memory and update property variable}
    ReallocMem(Data, (Value * sizeof(Pointer)));
    fAllocated := Value;
  end {if (Value <> 0)}
end;

{Set the number of items in the list}
procedure TPointerList.SetCount(const Value: Integer);
begin
  {Value must be 0 or greater}
  if (Value >= 0) then
  begin
    {If there is no more memory to hold data, allocate some more}
    while (Value > fAllocated) do
      Allocated := Allocated + fAdditionalBlock;
    {Update property}
    fCount := Value;
  end {if (Value >= 0)}
end;

{$ENDIF}

end.
