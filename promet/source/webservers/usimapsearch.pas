unit usimapsearch;

// --------------------------------------------------------------------------
// Design at least inspired by the Cyrus IMAP Server
// http://asg.web.cmu.edu/cyrus
// --------------------------------------------------------------------------

interface

uses Classes;

type
  TFlagMask = longint;   // Bitmask containing status flags of the message
  TUnixTime = integer;

  TIMAPSearch = class
  private
    FFlagsSet: TFlagMask;
    FFlagsUnset: TFlagMask;
    FSmaller: integer;
    FLarger: integer;
    FBefore: int64;
    FSince: int64;
    FSentBefore: int64;
    FSentSince: int64;
    FCharset: string;
    FSequence: TStringList;
    FUIDSequence: TStringList;
    FBodyStrings: TStringList;
    FTextStrings: TStringList;
    FHeaderStrings: TStringList;
    FSubList: TList;
    procedure SetFlags(Value: TFlagMask);
    procedure UnsetFlags(Value: TFlagMask);
    procedure SetSmaller(Value: integer);
    procedure SetLarger(Value: integer);
    procedure SetBefore(Value: int64);
    procedure SetSince(Value: int64);
    procedure SetSentBefore(Value: int64);
    procedure SetSentSince(Value: int64);
  public
    property FlagsSet: TFlagMask read FFlagsSet write SetFlags;
    property FlagsUnset: TFlagMask read FFlagsUnset write UnsetFlags;
    property Smaller: integer read FSmaller write SetSmaller;
    property Larger: integer read FLarger write SetLarger;
    property Before: int64 read FBefore write SetBefore;
    property Since: int64 read FSince write SetSince;
    property SentBefore: int64 read FSentBefore write SetSentBefore;
    property SentSince: int64 read FSentSince write SetSentSince;
    property Charset: string read FCharset;
    property Sequence: TStringList read FSequence;
    property UIDSequence: TStringList read FUIDSequence;
    property BodyStrings: TStringList read FBodyStrings;
    property TextStrings: TStringList read FTextStrings;
    property HeaderStrings: TStringList read FHeaderStrings;
    property Subs: TList read FSubList write FSubList;
    procedure AddSeqStr(Value: string);
    procedure AddUIDStr(Value: string);
    procedure AddBodyStr(Value: string);
    procedure AddTextStr(Value: string);
    procedure AddHeaderStr(Header, Value: string);
    constructor Create(ACharset: string);
    destructor Destroy; override;
  end;

  TSearchSub = class
  public
    Sub1: TIMAPSearch;     // If sub2 is null, then sub1 is NOT'ed.
    Sub2: TIMAPSearch;     // Otherwise sub1 and sub2 are OR'ed.
    destructor Destroy; override;
  end;

  TRfcTimezone = string;

type
  TIMAPStringtype = (IMAP_STRING_ATOM, IMAP_STRING_QUOTED, IMAP_STRING_LITERAL);
  TIMAPStringtypes = set of TIMAPStringtype;

const
  IMAP_STRING_STRING = [IMAP_STRING_QUOTED, IMAP_STRING_LITERAL];
  IMAP_STRING_ASTRING = IMAP_STRING_STRING + [IMAP_STRING_ATOM];

  FLAGNONE: TFlagMask = 0;
  FLAGSEEN: TFlagMask = 1;
  FLAGANSWERED: TFlagMask = 2;
  FLAGFLAGGED: TFlagMask = 4;
  FLAGDELETED: TFlagMask = 8;
  FLAGDRAFT: TFlagMask = 16;
  FLAGRECENT: TFlagMask = 32;

  FLAGAPPROVE: TFlagMask = 64;  //Moder
  FLAGDECLINE: TFlagMask = 128; //Moder

function GetSearchPgm(Args: string; SearchPgm: TIMAPSearch): boolean;
function ImapDateTimeToDateTime(ImapDateTime: string): TDateTime;
function ImapDateTextToDateTime(ImapDateText: string): TDateTime;
function DateTimeGMTToImapDateTime(DateTime: TDateTime;
  RfcTimezone: TRfcTimezone): string;
function DateTimeToUnixTime(DateTime: TDateTime): TUnixTime;
function UnixTimeToDateTime(UnixTime: TUnixTime): TDateTime;
function TrimWhSpace(const s: string): string;
function PosWhSpace(s: string): integer;
function QuotedStringOrToken( s: String ): String;
function TrimQuotes(Data: string): string;
procedure GetString(var InStr: string; out OutStr: string;
  const Stringtype: TIMAPStringtypes);
function TrimParentheses(Data: string): string;

// --------------------------------------------------------------------------

implementation

uses SysUtils, uBaseApplication;

{ TIMAPSearch }

procedure TIMAPSearch.SetFlags(Value: TFlagMask);
begin
  FFlagsSet := FFlagsSet or Value;
end;

procedure TIMAPSearch.UnsetFlags(Value: TFlagMask);
begin
  FFlagsUnset := FFlagsUnset or Value;
end;

procedure TIMAPSearch.SetSmaller(Value: integer);
begin
  if Value < FSmaller then
    FSmaller := Value;
end;

procedure TIMAPSearch.SetLarger(Value: integer);
begin
  if Value > FLarger then
    FLarger := Value;
end;

procedure TIMAPSearch.SetBefore(Value: int64);
begin
  if Value < FBefore then
    FBefore := Value;
end;

procedure TIMAPSearch.SetSince(Value: int64);
begin
  if Value > FSince then
    FSince := Value;
end;

procedure TIMAPSearch.SetSentBefore(Value: int64);
begin
  if Value < FSentBefore then
    FSentBefore := Value;
end;

procedure TIMAPSearch.SetSentSince(Value: int64);
begin
  if Value > FSentSince then
    FSentSince := Value;
end;

procedure TIMAPSearch.AddSeqStr(Value: string);
begin
  FSequence.Add(Value);
end;

procedure TIMAPSearch.AddUIDStr(Value: string);
begin
  FUIDSequence.Add(Value);
end;

procedure TIMAPSearch.AddBodyStr(Value: string);
begin
  FBodyStrings.Add(UpperCase(Value));
end;

procedure TIMAPSearch.AddTextStr(Value: string);
begin
  FTextStrings.Add(UpperCase(Value));
end;

procedure TIMAPSearch.AddHeaderStr(Header, Value: string);
begin
  if Header = '' then
    exit;
  if Header[Length(Header)] <> ':' then
    Header := Header + ':';
  FHeaderStrings.Add(Header + UpperCase(Value));
end;

constructor TIMAPSearch.Create(ACharset: string);
begin
  inherited Create;

  FCharset := ACharset;
  FFlagsSet := FLAGNONE;
  FFlagsUnset := FLAGNONE;
  FSmaller := High(integer);
  FLarger := 0;
  FBefore := High(int64);
  FSince := 0;
  FSentBefore := High(int64);
  FSentSince := 0;
  FSequence := TStringList.Create;
  FUIDSequence := TStringList.Create;
  FBodyStrings := TStringList.Create;
  FTextStrings := TStringList.Create;
  FHeaderStrings := TStringList.Create;
  FSubList := TList.Create;
end;

destructor TIMAPSearch.Destroy;
var
  i: integer;
begin
  FSequence.Free;
  FUIDSequence.Free;
  FBodyStrings.Free;
  FTextStrings.Free;
  FHeaderStrings.Free;
  for i := 0 to FSubList.Count - 1 do
    TSearchSub(FSubList[i]).Free;
  FSubList.Free;

  inherited Destroy;
end;

// --------------------------------------------------------------------------

{ TSearchSub }

destructor TSearchSub.Destroy;
begin
  if Assigned(Sub1) then
    Sub1.Free;
  if Assigned(Sub2) then
    Sub2.Free;

  inherited Destroy;
end;

procedure GetString(var InStr: string; out OutStr: string;
  const Stringtype: TIMAPStringtypes);
var
  i, Size: integer;
  aLen: Integer;
begin
  OutStr := '';
  if InStr = '' then
    exit;

  if (IMAP_STRING_QUOTED in Stringtype) and (InStr[1] = '"') then
  begin
    i := 2;
    while i <= Length(InStr) do
    begin
      case Instr[i] of
        '\': System.Delete(InStr, i, 1);
        '"':
        begin
          OutStr := Copy(InStr, 2, i - 2);
          System.Delete(InStr, 1, i);
          exit;
        end;
      end;
      Inc(i);
    end;
    if Assigned(BaseApplication) then with BaseApplication as IBaseApplication do
      Debug('Error parsing quoted string: No closing quotation mark');

  end
  else if (IMAP_STRING_LITERAL in Stringtype) and (InStr[1] = '{') then
    begin
      for i := 2 to Length(InStr) do
      begin
        if InStr[i] = '}' then
        begin
          if InStr[i - 1] = '+' // Literal+ //NHB
          then
            Size := StrToIntDef(Copy(InStr, 2, i - 3), -1)
          else
            Size := StrToIntDef(Copy(InStr, 2, i - 2), -1);
          System.Delete(InStr, 1, i + 2);
          if Size < 0 then
            break;
          aLen := Length(InStr);
          if aLen >= Size then
          begin
            OutStr := Copy(InStr, 1, Size);
            System.Delete(InStr, 1, Size);
          end
          else
          begin
            InStr := '';
            if Assigned(BaseApplication) then with BaseApplication as IBaseApplication do
              Debug('Error parsing literal string: literal too short');
          end;
          exit;
        end;
      end;
      if Assigned(BaseApplication) then with BaseApplication as IBaseApplication do
        Debug('Error parsing literal string: malformed literal');

    end
    else if (IMAP_STRING_ATOM in Stringtype) then
      begin
        for i := 1 to Length(InStr) do
        begin
          if InStr[i] in [' ', '(', ')', '"'] then
          begin
            OutStr := Copy(InStr, 1, i - 1);
            System.Delete(InStr, 1, i - 1);
            exit;
          end;
        end;
        OutStr := InStr;
      end;

  InStr := '';
end;

function GetAString(var Args: string; var Chr: char): string;
var
  Str: string;
begin
  Result := '';
  if Chr = #0 then
    exit;
  GetString(Args, Str, IMAP_STRING_ASTRING);
  if Length(Args) > 0 then
  begin
    Chr := Args[1];
    System.Delete(Args, 1, 1);
  end
  else
  begin
    if Length(Str) > 0 then
      Chr := Str[Length(Str)]
    else
    begin
      if Assigned(BaseApplication) then with BaseApplication as IBaseApplication do
        Warning('IMAP: Missing required argument');
      Chr := #0;
    end;
  end;
  Result := Str;
end;

function GetDate(DateString: string): int64;
begin
  Result := Trunc(ImapDateTextToDateTime(DateString));
end;

procedure ParseSearchPgm(var Args: string; SearchPgm: TIMAPSearch;
  var Chr: char); forward;

procedure ParseCriteria(var Args: string; SearchPgm: TIMAPSearch; var Chr: char);
var
  Key, S: string;
  Sub: TSearchSub;
begin
  Key := UpperCase(GetAString(Args, Chr));
  if Key = '' then
  begin
    if Chr <> '(' then
    begin
      if Assigned(BaseApplication) then with BaseApplication as IBaseApplication do
        Warning('IMAP: Missing required argument in SEARCH command');
      Chr := #0;
      exit;
    end;
    ParseSearchPgm(Args, SearchPgm, Chr);
    if Chr <> ')' then
    begin
      if Assigned(BaseApplication) then with BaseApplication as IBaseApplication do
        Warning('IMAP: Missing required closing paren in SEARCH command');
      Chr := #0;
    end
    else if Args <> '' then
      begin
        Chr := Args[1];
        System.Delete(Args, 1, 1);
      end;
    exit;
  end;
  case Key[1] of
    '0'..'9', '*': SearchPgm.AddSeqStr(Key);
    'A': if Key = 'ANSWERED' then
        SearchPgm.FlagsSet := FLAGANSWERED;
    'B':
    begin
      S := GetAString(Args, Chr);
      if Key = 'BCC' then
        SearchPgm.AddHeaderStr('bcc:', S)
      else if Key = 'BEFORE' then
          SearchPgm.Before := GetDate(S)
        else if Key = 'BODY' then
            SearchPgm.AddBodyStr(S);
    end;
    'C': if Key = 'CC' then
        SearchPgm.AddHeaderStr('cc:', GetAString(Args, Chr));
    'D': if Key = 'DELETED' then
        SearchPgm.FlagsSet := FLAGDELETED
      else if Key = 'DRAFT' then
          SearchPgm.FlagsSet := FLAGDRAFT;
    'F': if Key = 'FLAGGED' then
        SearchPgm.FlagsSet := FLAGFLAGGED
      else if Key = 'FROM' then
          SearchPgm.AddHeaderStr('from:', GetAString(Args, Chr));
    'H': if Key = 'HEADER' then
      begin
        S := GetAString(Args, Chr);
        if S <> '' then
          SearchPgm.AddHeaderStr(S, GetAString(Args, Chr));
      end;
    'K': if Key = 'KEYWORD' then
      begin
        if Assigned(BaseApplication) then with BaseApplication as IBaseApplication do
          Warning('IMAP: the KEYWORD search is not implemented.');
        GetAString(Args, Chr);
      end;
    'L': if Key = 'LARGER' then
        SearchPgm.Larger := StrToIntDef(GetAString(Args, Chr), 0);
    'N': if Key = 'NEW' then
      begin
        SearchPgm.FlagsSet := FLAGRECENT;
        SearchPgm.FlagsUnset := FLAGSEEN;
      end
      else
        if Key = 'NOT' then
        begin
          Sub := TSearchSub.Create;
          try
            Sub.Sub1 := TIMAPSearch.Create(SearchPgm.Charset);
            ParseCriteria(Args, Sub.Sub1, Chr);
            if Chr <> #0 then
              SearchPgm.Subs.Add(Sub)
            else
            begin
              if Assigned(BaseApplication) then with BaseApplication as IBaseApplication do
                Warning('IMAP: Missing argument in SEARCH NOT command');
              Sub.Free;
            end
          except
            Sub.Free
          end;
        end;
    'O': if Key = 'OLD' then
      begin
        SearchPgm.FlagsUnset := FLAGRECENT;
      end
      else
        if Key = 'ON' then
        begin
          S := GetAString(Args, Chr);
          SearchPgm.Before := GetDate(S) + 1;
          SearchPgm.Since := GetDate(S) - 1;
        end
        else
          if Key = 'OR' then
          begin
            Sub := TSearchSub.Create;
            try
              Sub.Sub1 := TIMAPSearch.Create(SearchPgm.Charset);
              Sub.Sub2 := TIMAPSearch.Create(SearchPgm.Charset);
              ParseCriteria(Args, Sub.Sub1, Chr);
              if Chr = ' ' then
                ParseCriteria(Args, Sub.Sub2, Chr);
              if Chr <> #0 then
                SearchPgm.Subs.Add(Sub)
              else
              begin
                if Assigned(BaseApplication) then with BaseApplication as IBaseApplication do
                  Warning('IMAP: Missing argument in SEARCH OR command');
                Sub.Free;
              end
            except
              Sub.Free
            end;
          end;
    'R': if Key = 'RECENT' then
        SearchPgm.Flagsset := FLAGRECENT;
    'S': if Key = 'SEEN' then
      begin
        SearchPgm.FlagsSet := FLAGSEEN;
      end
      else
      begin
        S := GetAString(Args, Chr);
        if Key = 'SENTBEFORE' then
          SearchPgm.SentBefore := GetDate(S)
        else if Key = 'SENTON' then
          begin
            SearchPgm.SentBefore := GetDate(S) + 1;
            SearchPgm.SentSince := GetDate(S) - 1;
          end
          else if Key = 'SENTSINCE' then
            SearchPgm.SentSince := GetDate(S)
          else if Key = 'SINCE' then
            SearchPgm.Since := GetDate(S)
          else if Key = 'SMALLER' then
            SearchPgm.Smaller := StrToIntDef(S, High(integer))
          else if Key = 'SUBJECT' then
            SearchPgm.AddHeaderStr('subject:', S);
      end;
    'T':
    begin
      S := GetAString(Args, Chr);
      if Key = 'TEXT' then
        SearchPgm.AddTextStr(S)
      else if Key = 'TO' then
          SearchPgm.AddHeaderStr('to:', S);
    end;
    'U':
    begin
      if Key = 'UID' then
        SearchPgm.AddUIDStr(GetAString(Args, Chr))
      else if Key = 'UNANSWERED' then
        SearchPgm.FlagsUnset := FLAGANSWERED
      else if Key = 'UNDELETED' then
        SearchPgm.FlagsUnset := FLAGDELETED
      else if Key = 'UNDRAFT' then
        SearchPgm.FlagsUnset := FLAGDRAFT
      else if Key = 'UNFLAGGED' then
        SearchPgm.FlagsUnset := FLAGFLAGGED
      else if Key = 'UNSEEN' then
        SearchPgm.FlagsUnset := FLAGSEEN
      else if Key = 'UNKEYWORD' then
        begin
          if Assigned(BaseApplication) then with BaseApplication as IBaseApplication do
            Warning('IMAP: the KEYWORD search is not implemented.');
          GetAString(Args, Chr);
        end;
    end
  end;
end;

procedure ParseSearchPgm(var Args: string; SearchPgm: TIMAPSearch; var Chr: char);
begin
  repeat
    ParseCriteria(Args, SearchPgm, Chr)
  until Chr <> ' ';
end;

function GetSearchPgm(Args: string; SearchPgm: TIMAPSearch): boolean;
var
  Chr: char;
begin
  Chr := #1; // Bugfix
  ParseSearchPgm(Args, SearchPgm, Chr);
  Result := (Chr <> #0);
end;

function PosWhSpace(s: string): integer;
var
  j: integer;
begin
  Result := Pos(' ', s);
  if Result = 0 then
  begin
    Result := Pos(#9, s);
  end
  else
  begin
    j := Pos(#9, s);
    if (j > 0) and (j < Result) then
      Result := j;
  end;
end;

function TrimWhSpace(const s: string): string;
var
  i, p, l: integer;
begin
  p := 1;
  l := Length(s);
  for i := p to l do
  begin
    if (s[i] = #9) or (s[i] = ' ') then
      Inc(p)
    else
      break;
  end;
  for i := l downto p do
  begin
    if (s[i] = #9) or (s[i] = ' ') then
      Dec(l)
    else
      break;
  end;
  if p > l then
    Result := ''
  else if (p = 1) and (l = Length(s)) then
      Result := s
    else
      Result := Copy(s, p, l - p + 1);
end;

function TrimEnclosingChars(Data, FirstChar, LastChar: string): string;
var
  i: integer;
begin
  Data := TrimWhSpace(Data);
  i := length(Data);
  if (i > 1) and (Data[1] = FirstChar) and (Data[i] = LastChar) then
    Result := copy(Data, 2, i - 2)
  else
    Result := Data;
end;

function QuotedStringOrToken( s: String ): String;
begin
   s := TrimWhSpace( s );
   GetString( s, Result, [IMAP_STRING_QUOTED, IMAP_STRING_ATOM] );
   Result := TrimWhSpace( Result )
end;

function TrimQuotes(Data: string): string;
begin
  Result := TrimEnclosingChars(Data, '"', '"');
end;

function TrimParentheses(Data: string): string;
begin
  Result := TrimEnclosingChars(Data, '(', ')');
end;

function DateTimeToUnixTime(DateTime: TDateTime): TUnixTime;
begin
  if DateTime<EncodeDate(1970, 1, 1) then
    DateTime:=EncodeDate(1970, 1, 1);
  Result := Round((DateTime - EncodeDate(1970, 1, 1)) * 86400);
end;

function UnixTimeToDateTime(UnixTime: TUnixTime): TDateTime;
var
  Days, Hours, Mins, Secs, h: integer;
begin
  Secs := UnixTime;

  if Secs >= 0 then
  begin
    Days := Secs div 86400;
    Secs := Secs mod 86400;
  end
  else
  begin
    h := Secs and $1;
    Secs := Secs shr 1;

    Days := Secs div (86400 shr 1);
    Secs := Secs mod (86400 shr 1);

    Secs := (Secs shl 1) or h;
  end;

  Hours := Secs div 3600;
  Secs := Secs mod 3600;
  Mins := Secs div 60;
  Secs := Secs mod 60;

  Result := EncodeDate(1970, 1, 1) + Days + EncodeTime(Hours, Mins, Secs, 0);
end;

function MinutesToDateTime(Minutes: integer): TDateTime;
begin
  Result := (Minutes / 60.0 / 24.0);
end;

function RfcTimezoneToBiasMinutes(RfcTimeZone: TRfcTimeZone): integer;
begin
  Result := 0;
  if RfcTimeZone = '' then
    exit;

  if RfcTimeZone[1] in ['+', '-'] then
  begin

    Result := strtointdef(copy(RfcTimeZone, 2, 2), 0) * 60 +
      strtointdef(copy(RfcTimeZone, 4, 2), 0);
    if (Result < 0) or (Result >= 24 * 60) then
      Result := 0;
    if RfcTimeZone[1] = '+' then
      Result := -Result;

  end
  else
  begin

    RfcTimeZone := UpperCase(RfcTimeZone);

    if RfcTimeZone = 'GMT' then
      Result := 0
    else if RfcTimeZone = 'UT' then
      Result := 0
    else if RfcTimeZone = 'EST' then
      Result := -5 * 60
    else if RfcTimeZone = 'EDT' then
      Result := -4 * 60
    else if RfcTimeZone = 'CST' then
      Result := -6 * 60
    else if RfcTimeZone = 'CDT' then
      Result := -5 * 60
    else if RfcTimeZone = 'MST' then
      Result := -7 * 60
    else if RfcTimeZone = 'MDT' then
      Result := -6 * 60
    else if RfcTimeZone = 'PST' then
      Result := -8 * 60
    else if RfcTimeZone = 'PDT' then
      Result := -7 * 60
    else if RfcTimeZone = 'A' then
      Result := -1 * 60
    else if RfcTimeZone = 'B' then
      Result := -2 * 60
    else if RfcTimeZone = 'C' then
      Result := -3 * 60
    else if RfcTimeZone = 'D' then
      Result := -4 * 60
    else if RfcTimeZone = 'E' then
      Result := -5 * 60
    else if RfcTimeZone = 'F' then
      Result := -6 * 60
    else if RfcTimeZone = 'G' then
      Result := -7 * 60
    else if RfcTimeZone = 'H' then
      Result := -8 * 60
    else if RfcTimeZone = 'I' then
      Result := -9 * 60
    else if RfcTimeZone = 'K' then
      Result := -10 * 60
    else if RfcTimeZone = 'L' then
      Result := -11 * 60
    else if RfcTimeZone = 'M' then
      Result := -12 * 60
    else if RfcTimeZone = 'N' then
      Result := 1 * 60
    else if RfcTimeZone = 'O' then
      Result := 2 * 60
    else if RfcTimeZone = 'P' then
      Result := 3 * 60
    else if RfcTimeZone = 'Q' then
      Result := 4 * 60
    else if RfcTimeZone = 'R' then
      Result := 5 * 60
    else if RfcTimeZone = 'S' then
      Result := 6 * 60
    else if RfcTimeZone = 'T' then
      Result := 7 * 60
    else if RfcTimeZone = 'U' then
      Result := 8 * 60
    else if RfcTimeZone = 'V' then
      Result := 9 * 60
    else if RfcTimeZone = 'W' then
      Result := 10 * 60
    else if RfcTimeZone = 'X' then
      Result := 11 * 60
    else if RfcTimeZone = 'Y' then
      Result := 12 * 60
    else if RfcTimeZone = 'Z' then
      Result := 0;
  end;
end;

const
  RFC_DAY_NAMES = 'SunMonTueWedThuFriSat';
  RFC_MONTH_NAMES = 'JanFebMarAprMayJunJulAugSepOctNovDec';

function ImapDateTimeToDateTime(ImapDateTime: string): TDateTime;
  // RFC 2060
  //   date_time ::= <"> date_day_fixed "-" date_month "-" date_year
  //                 SPACE time SPACE zone <">
  //   date_day        ::= 1*2digit
  //   date_month      ::= "Jan" / "Feb" / "Mar" / "Apr" / "May" / "Jun" /
  //                       "Jul" / "Aug" / "Sep" / "Oct" / "Nov" / "Dec"
  //   date_year       ::= 4digit
  //   time ::= 2digit ":" 2digit ":" 2digit
  //   zone ::= ("+" / "-") 4digit
var
  s, h, tz: string;
  i, yyyy, mm, dd, hh, nn, ss: integer;
begin
  Result := 29221.0;
  s := TrimWhSpace(ImapDateTime);
  if s = '' then
    exit;
  try
    i := Pos('-', s);
    dd := StrToInt(copy(s, 1, i - 1));
    System.Delete(s, 1, i);
    if s = '' then
      exit;
    i := Pos('-', s);
    h := lowercase(copy(s, 1, i - 1));
    mm := ((Pos(h, LowerCase(RFC_MONTH_NAMES)) - 1) div 3) + 1;
    System.Delete(s, 1, i);
    yyyy := StrToInt(copy(s, 1, 4));
    System.Delete(s, 1, 4);
    s := TrimWhSpace(s);
    i := Pos(' ', s);
    if i = 0 then
    begin
      h := s;
      tz := '';
    end
    else
    begin
      h := TrimWhSpace(copy(s, 1, i - 1));
      tz := UpperCase(TrimWhSpace(copy(s, i + 1, 32)));
    end;

    hh := StrToInt(copy(h, 1, 2));
    nn := StrToInt(copy(h, 4, 2));
    ss := StrToInt(copy(h, 7, 2));

    Result := EncodeDate(yyyy, mm, dd) +
      MinutesToDateTime(RfcTimezoneToBiasMinutes(tz)) // -> GMT
      + EncodeTime(hh, nn, ss, 0);
  except
  end;
end;

function ImapDateTextToDateTime(ImapDateText: string): TDateTime;
  //   date_text  ::= date_day "-" date_month "-" date_year
begin
  Result := ImapDateTimeToDateTime(ImapDateText + ' 00:00:00 +0000');
end;

function DateTimeGMTToImapDateTime(DateTime: TDateTime;
  RfcTimezone: TRfcTimezone): string;
var
  sDT: string;
  MOY: integer;
begin
  if RfcTimezone = '' then
    RfcTimezone := '+0000';

  DateTime := DateTime - MinutesToDateTime(RfcTimezoneToBiasMinutes(RfcTimezone));

  sDT := FormatDateTime('dd"."mm"."yyyy hh":"nn":"ss', DateTime);
  MOY := StrToInt(copy(sDT, 4, 2));

  // Date: 27-Mar-1998 12:12:50 +1300

  Result := copy(sDT, 1, 2) + '-' + copy(
    RFC_MONTH_NAMES, MOY * 3 - 2, 3) + '-' + copy(sDT, 7, 4) +
    ' ' + copy(sDT, 12, 8) + ' ' + RfcTimezone;
end;

end.
