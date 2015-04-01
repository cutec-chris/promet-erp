unit usimapmailbox;

interface

uses Classes, usimapsearch,mimemess,mimepart,synautil,blcksock,syncobjs,
  usbaseserver;

type
  TMessageSet = array of LongInt;
  TStoreMode = set of (smAdd, smReplace, smDelete);

  tOnNewMess = procedure of object;
  tOnExpunge = procedure(Number: integer) of object;

  pIMAPNotification = ^tIMAPNotification;

  tIMAPNotification = record
    OnNewMess: tOnNewMess;
    OnExpunge: tOnExpunge;
  end;

  { TImapMailbox }

  TImapMailbox = class
  private
    fCritSection: TRTLCriticalSection;
    fPath: string;
    FUIDNext: longint;
    FUIDvalidity: TUnixTime;
    fUsers: TList;
    fReadOnly: boolean;
    procedure AddMessage(Flags: string; TimeStamp: TUnixTime);
    function FlagMaskToString(FlagMask: TFlagMask): string;
    function GetPossFlags: string;
    //procedure WriteStatus; //Not Critical_Section-Protected!
    function Find(Search: TIMAPSearch; MsgSet: TMessageSet): TMessageSet;
    function FindMessageSets(MsgSet1, MsgSet2: TMessageSet; Exclude: boolean): TMessageSet;
  protected
    FUnseen: longint;
    FRecent: longint;
    FMessages: longint;
    function JoinMessageSets(MsgSet1, MsgSet2: TMessageSet): TMessageSet;
  public
    function StringToFlagMask(Flags: string): TFlagMask;
    function StrToMsgSet(s: string; UseUID: boolean): TMessageSet;virtual;abstract;
    function  GetUID( Index: LongInt ): LongInt;virtual;abstract;
    function  GetUIDStr( Index: LongInt ): String;virtual;abstract;
    function  GetIndex( UID: LongInt ): Integer;virtual;abstract;
    function  SetFlags( Index: LongInt; Flags: TFlagMask ): TFlagMask;virtual;abstract;
    function  GetFlags( Index: LongInt ): TFlagMask;virtual;abstract;
    function  AddFlags( Index: LongInt; Flags: TFlagMask ): TFlagMask;virtual;abstract;
    function  RemoveFlags( Index: LongInt; Flags: TFlagMask ): TFlagMask;virtual;abstract;
    function  GetTimeStamp( Index: LongInt ): TUnixTime;virtual;abstract;
    function  GetMessage( UID: LongInt ): TMimeMess;virtual;abstract;

    function CopyMessage(MsgSet: TMessageSet; Destination: TImapMailbox): boolean;virtual;
    function AppendMessage(AThread: TSTcpThread;MsgTxt: string; Flags: string;TimeStamp: TUnixTime): string;virtual;

    function FindFlags(MsgSet: TMessageSet; Flags: TFlagMask;  Exclude: boolean): TMessageSet;virtual;
    function FindSize(MsgSet: TMessageSet; Min, Max: integer): TMessageSet;virtual;abstract;
    function FindTimeStamp(MsgSet: TMessageSet; After, Before: int64): TMessageSet;virtual;
    function FindContent(MsgSet: TMessageSet; After, Before: int64; Charset: string; HeaderList, BodyStrings,  TextStrings: TStringList): TMessageSet;virtual;abstract;

    function GetEnvelope(MyMessage : TMimeMess) : string;
    function BodyStructure(Part: TMimePart; Extensible: Boolean): String;
    function GetAddresses(MyMessage : TMimeMess;  HdrNam: String ): String;
    function GetBodySection(MyMessage: TMimeMess; Section: string; Nested: Boolean; var Offset, Maximum: Integer): string;
    function GetAddressStructure( Address: String ): String;

    function GetHeaderFields(MyMessage : TMimeMess; Fields: String; Exclude: Boolean ): String;
    function GetBodyFields(Part: TMimePart): string;
    // Header
    function FullHeader(MyMessage : TMimeMess) : string;
    // Body
    //function FullText(MyMessage : TMimeMess) : string;
    function BodySection(MyMessage : TMimeMess; var Section: String ): String;
    function FullBody(MyMessage : TMimeMess) : string;
    //Header+Body
    function Text(MyMessage : TMimeMess) : string;

    property MBReadOnly: boolean read fReadOnly write fReadOnly;
    property GetUIDnext: LongInt read FUIDNext;
    property GetUIDvalidity: TUnixTime read FUIDvalidity;
    property PossFlags: string read GetPossFlags;
    procedure Lock;virtual;
    procedure Unlock;virtual;
    procedure RemoveRecentFlags;
    procedure AddUser(Notify: pIMAPNotification);
    procedure RemoveUser(Notify: pIMAPNotification; out NoUsersLeft: boolean);
    procedure Expunge(ExcludeFromResponse: pIMAPNotification);

    function Search(SearchStruct: TIMAPSearch; UseUID: boolean): string;
    function Fetch(Idx: integer; MsgDat: string; var Success: boolean): string;
    function Store(Idx: integer; Flags: string; Mode: TStoreMode): string;
    function AreValidFlags(Flags: string): boolean;

    procedure AddIncomingMessage(const Flags: string = '');
    procedure SendMailboxUpdate;

    property Messages : Integer read FMessages;
    property Recent : Integer read FRecent;
    property Unseen : Integer read FUnseen;
    property  Path           : String     read  fPath;

    constructor Create(APath: string;CS : TCriticalSection);virtual;
    destructor Destroy; override;
  end;

  function NowGMT: TDateTime;
  function  SwitchByteOrder( Input: LongInt ): Longint;

implementation

uses SysUtils,uBaseApplication;

// --------------------------------------------------------- TImapMailbox -----
function TImapMailbox.StringToFlagMask(Flags: string): TFlagMask;
begin
  Result := FLAGNONE;
  Flags := uppercase(Flags);
  if pos('\SEEN', Flags) > 0 then
    Result := Result or FLAGSEEN;
  if pos('\ANSWERED', Flags) > 0 then
    Result := Result or FLAGANSWERED;
  if pos('\FLAGGED', Flags) > 0 then
    Result := Result or FLAGFLAGGED;
  if pos('\DELETED', Flags) > 0 then
    Result := Result or FLAGDELETED;
  if pos('\DRAFT', Flags) > 0 then
    Result := Result or FLAGDRAFT;
  if pos('\RECENT', Flags) > 0 then
    Result := Result or FLAGRECENT;
  //Moder
  if pos('\APPROVE', Flags) > 0 then
    Result := Result or FLAGAPPROVE;
  if pos('\DECLINE', Flags) > 0 then
    Result := Result or FLAGDECLINE;
end;

function TImapMailbox.FlagMaskToString(FlagMask: TFlagMask): string;
begin
  Result := '';

  //Moder
  if FlagMask and FLAGAPPROVE = FLAGAPPROVE then
    Result := Result + ' \Approve';
  if FlagMask and FLAGDECLINE = FLAGDECLINE then
    Result := Result + ' \Decline';


  if FlagMask and FLAGSEEN = FLAGSEEN then
    Result := Result + ' \Seen';
  if FlagMask and FLAGANSWERED = FLAGANSWERED then
    Result := Result + ' \Answered';
  if FlagMask and FLAGFLAGGED = FLAGFLAGGED then
    Result := Result + ' \Flagged';
  if FlagMask and FLAGDELETED = FLAGDELETED then
    Result := Result + ' \Deleted';
  if FlagMask and FLAGDRAFT = FLAGDRAFT then
    Result := Result + ' \Draft';
  if FlagMask and FLAGRECENT = FLAGRECENT then
    Result := Result + ' \Recent';
  if Result <> '' then
    Delete(Result, 1, 1);
  Result := '(' + Result + ')';
end;

function TImapMailbox.AreValidFlags(Flags: string): boolean;
begin
  Result := not (pos('\RECENT', Flags) > 0);
end;

function TImapMailbox.GetPossFlags: string;
begin
  Result := '(\Answered \Flagged \Deleted \Seen \Draft)';
end;

procedure TImapMailbox.Expunge(ExcludeFromResponse: pIMAPNotification);
begin
end;

function TImapMailbox.Store(Idx: integer; Flags: string; Mode: TStoreMode): string;
var
  FlagMsk: tFlagMask;
begin
  if fReadOnly then begin
    Result := FlagMaskToString(GetFlags( Idx ));
    exit
  end;
  try
     Lock;
     FlagMsk := StringToFlagMask(Flags);
     if FlagMsk and FLAGSEEN = FLAGSEEN then begin
        if GetFlags(Idx) and FLAGSEEN = FLAGSEEN then begin
           if Mode = [smDelete] then inc( FUnseen )
        end else begin
           if Mode <= [smReplace, smAdd] then dec( FUnseen )
        end
     end else begin
        if (GetFlags(Idx) and FLAGSEEN = FLAGSEEN) then
           if Mode = [smReplace] then inc( FUnseen );
     end;

     if      Mode = [smReplace] then FlagMsk := SetFlags   ( Idx, FlagMsk )
     else if Mode = [smDelete]  then FlagMsk := RemoveFlags( Idx, FlagMsk )
     else if Mode = [smAdd]     then FlagMsk := AddFlags   ( Idx, FlagMsk )
     else                            FlagMsk := GetFlags   ( Idx );
     Result := FlagMaskToString(FlagMsk);
  finally
     Unlock;
  end;
end;

procedure TImapMailbox.RemoveRecentFlags;
var
  i: integer;
begin
  lock;
  try
    if fReadOnly then
      exit;
    //for i := 0 to fIndex.Count-1 do fIndex.RemoveFlags( i, FLAGRECENT );
    //fStatus.Recent := 0;
  finally
    unlock
  end;
end;

procedure TImapMailbox.AddMessage(Flags: string; TimeStamp: TUnixTime);
var
  FMFlags: tFlagMask;
begin
  try
    Lock;
    FMFlags := StringToFlagMask(Flags);
    //TODO:reimplement
    //fIndex.AddEntry( GetUIDnext, FMFlags, TimeStamp );
    Inc(FUIDnext);
    Inc(FMessages);
    if FMFlags and FLAGSEEN <> FLAGSEEN then
      Inc(FUnseen);
    if FMFlags and FLAGRECENT = FLAGRECENT then
      Inc(FRecent);
  finally
    Unlock;
  end;
end;

function TImapMailbox.CopyMessage(MsgSet: TMessageSet;
  Destination: TImapMailbox): boolean;
begin
  Result := False;
end;

function TImapMailbox.AppendMessage(AThread: TSTcpThread; MsgTxt: string;
  Flags: string; TimeStamp: TUnixTime): string;
var
  Bytes: integer;
  FileName: string;
begin
  Result := 'NO APPEND error: [Read-Only] ';
  if fReadOnly then
    exit;
  Result := 'NO APPEND error';
end;

function MinutesToDateTime(Minutes: integer): TDateTime;
begin
  Result := (Minutes / 60.0 / 24.0);
end;

function NowBiasMinutes: integer;
begin
end;

function NowBias: TDateTime;
begin
  Result := MinutesToDateTime(NowBiasMinutes);
end;

function NowGMT: TDateTime;
begin
  Result := Now + NowBias;
end;

function SwitchByteOrder(Input: Longint): Longint;
begin
  Result :=  Input                shl 24 +
            (Input and $0000FF00) shl  8 +
            (Input and $00FF0000) shr  8 +
             Input                shr 24
end;

procedure TImapMailbox.AddIncomingMessage(const Flags: string);
begin
  if Flags = '' then
    AddMessage(FlagMaskToString(FLAGRECENT), DateTimeToUnixTime(NowGMT))
  else
    AddMessage(Flags, DateTimeToUnixTime(NowGMT));
  SendMailboxUpdate;
end;

procedure TImapMailbox.SendMailboxUpdate;
var
  i: integer;
begin
  for i := 0 to fUsers.Count - 1 do
    try
      if Assigned(fUsers.Items[i]) then
      begin
        pIMAPNotification(fUsers.Items[i])^.OnNewMess;
{
           TSrvIMAPCli( Users.Items[i] ).SendRes( IntToStr(GetMessages) + ' EXISTS');
           if FirstUser then begin
              TSrvIMAPCli( Users.Items[i] ).SendRes( IntToStr(GetRecent) + ' RECENT');
              RemoveRecentFlags;
              FirstUser := False;
           end }
      end
    except
      Continue
    end;
  if not fReadOnly then
    RemoveRecentFlags;
end;

function ExtractParameter(var Params: string): string;
var
  i: integer;
begin
  Params := TrimWhSpace(Params);
  i := PosWhSpace(Params);
  if (i > 0) and (Pos('[', Params) < i) and (Pos(']', Params) > i) then
  begin
    i := Pos(']', Params) + 1;
    while i <= Length(Params) do
    begin
      if Params[i] in [#9, ' '] then
        break;
      Inc(i);
    end;
  end;
  if (i > 0) then
  begin
    Result := Uppercase(TrimQuotes(copy(Params, 1, i - 1)));
    Params := TrimWhSpace(copy(Params, i + 1, length(Params) - i));
  end
  else
  begin
    Result := Uppercase(TrimQuotes(Params));
    Params := '';
  end;
end;

function TImapMailbox.Fetch(Idx: integer; MsgDat: string;
  var Success: boolean): string;

  function MakeLiteral(Txt: string): string;
  begin
    Result := '{' + IntToStr(Length(Txt)) + '}' + #13#10 + Txt;
  end;

var
  Filename, Args, DataItem, Data: string;
  MyMail: TMimeMess = nil;
  sl: TStringList;

  procedure AddDataValue(NewValue: string);
  begin
    Data := Data + ' ' + DataItem + ' ' + NewValue;
  end;

begin
  Args := MsgDat;
  Data := '';
  sl := TStringList.Create;
  try
    try
    repeat
      DataItem := ExtractParameter( Args );
      if DataItem = 'FLAGS' then begin
        AddDataValue( FlagMaskToString( GetFlags(Idx) ) )
      end else if DataItem = 'INTERNALDATE' then begin
        AddDataValue( '"' + DateTimeGMTToImapDateTime( UnixTimeToDateTime(GetTimeStamp(Idx) ), '+0000' ) + '"' )
      end else if DataItem = 'UID' then begin
        AddDataValue(GetUIDStr(Idx))
      end else begin
        if Not Assigned(MyMail) then begin
          MyMail := GetMessage(GetUID(Idx));
          if not Assigned(MyMail) then break;
        end;
        if DataItem = 'ENVELOPE' then AddDataValue( GetEnvelope(MyMail) )
        else if DataItem = 'RFC822' then AddDataValue( MakeLiteral( MyMail.Lines.Text ) )  //TODO:wo Header ??
        else if DataItem = 'RFC822.HEADER' then
          begin
            MyMail.Header.EncodeHeaders(sl);
            AddDataValue( MakeLiteral( sl.Text + #13#10 ) )
          end
        else if DataItem = 'RFC822.TEXT' then AddDataValue( MakeLiteral( MyMail.Lines.Text ) )
        else if DataItem = 'RFC822.SIZE' then AddDataValue( IntToStr( Length(MyMail.Lines.Text) ) )
        else if DataItem = 'BODYSTRUCTURE' then AddDataValue( BodyStructure(MyMail.MessagePart, True ) )
        else if DataItem = 'BODY' then AddDataValue( BodyStructure(MyMail.MessagePart, False ) )
        else if Copy( DataItem, 1, 4 ) = 'BODY' then
          begin
            if Copy( DataItem, 5, 5 ) = '.PEEK' then System.Delete( DataItem, 5, 5 )
            else if GetFlags( Idx ) and FLAGSEEN <> FLAGSEEN then begin
              // The \Seen flag is implicitly set; if this causes the flags to
              // change they SHOULD be included as part of the FETCH responses.
              Store(idx, '\seen', [smAdd]);
              Args := Args + ' FLAGS';
            end;
            AddDataValue( MakeLiteral( BodySection(MyMail, DataItem ) ) )
          end
        else
          begin
            //Log( LOGID_WARN, 'IMAPMailbox.Fetch.UnsupportedPar',
            //      'Unsupported Imap FETCH parameter: "%s"', DataItem);
            Success := False
          end
       end;
    until Args = '';
    except
      begin
        Result := '';
        exit;
      end;
    end;
  finally
    FreeAndNil(MyMail);
    sl.Free;
  end;
  System.Delete( Data, 1, 1 );
  Result := IntToStr(Idx+1) + ' FETCH (' + Data + ')'
end;

procedure TImapMailbox.AddUser(Notify: pIMAPNotification);
begin
  fUsers.Add(Notify);
end;

procedure TImapMailbox.RemoveUser(Notify: pIMAPNotification;
  out NoUsersLeft: boolean);
var
  i: integer;
begin
  NoUsersLeft := False;
  i := fUsers.IndexOf(Notify);
  if i >= 0 then
    fUsers.Delete(i);
  if fUsers.Count = 0 then
    NoUsersLeft := True;
end;

procedure TImapMailbox.Lock;
begin
  EnterCriticalSection(fCritSection);
end;

procedure TImapMailbox.Unlock;
begin
  LeaveCriticalSection(fCritSection);
end;

constructor TImapMailbox.Create(APath: string; CS: TCriticalSection);
begin
  inherited Create;
  InitCriticalSection(fCritSection);
  fPath := APath;
  fUsers := TList.Create;
  fReadOnly := False;
end;

destructor TImapMailbox.Destroy;
begin
  if fUsers.Count > 0 then
    raise Exception.Create('TImapMailbox.Destroy: imap mailbox is still in use!');
  fUsers.Free; // TODO: User direkt aufräumen oder warnen?
  DoneCriticalSection(fCritSection);
  inherited;
end;

function TImapMailbox.Search(SearchStruct: TIMAPSearch; UseUID: boolean): string;
var
  FoundMessages: TMessageSet;
  i: integer;
begin
  Result := '';
  FoundMessages := Find(SearchStruct, StrToMsgSet('1:*', False));
  for i := 0 to Length(FoundMessages) - 1 do
    if UseUID then
      Result := Result + ' ' + GetUIDStr(FoundMessages[i] - 1)
    else
      Result := Result + ' ' + IntToStr(FoundMessages[i]);
end;


function TImapMailbox.Find(Search: TIMAPSearch; MsgSet: TMessageSet): TMessageSet;
var
  Found: TMessageSet;
  i: integer;
begin
  SetLength(Result, 0);
  SetLength(Found, Length(MsgSet));
  for i := 0 to High(Found) do
    Found[i] := MsgSet[i];

  // do the fast searches first
  if Search.Sequence.Count > 0 then
  begin
    for i := 0 to Search.Sequence.Count - 1 do
      Found := FindMessageSets(Found, StrToMsgSet(
        Search.Sequence[i], False), False);
    if Length(Found) = 0 then
      exit;
  end;
  if Search.UIDSequence.Count > 0 then
  begin
    for i := 0 to Search.UIDSequence.Count - 1 do
      Found := FindMessageSets(Found, StrToMsgSet(
        Search.UIDSequence[i], True), False);
    if Length(Found) = 0 then
      exit;
  end;
  if Search.FlagsSet <> FLAGNONE then
  begin
    Found := FindFlags(Found, Search.FlagsSet, False);
    if Length(Found) = 0 then
      exit;
  end;
  if Search.FlagsUnset <> FLAGNONE then
  begin
    Found := FindFlags(Found, Search.FlagsUnset, True);
    if Length(Found) = 0 then
      exit;
  end;
  if (Search.Since > 0) or (Search.Before < High(int64)) then
  begin
    Found := FindTimeStamp(Found, Search.Since, Search.Before);
    if Length(Found) = 0 then
      exit;
  end;
  if (Search.Larger > 0) or (Search.Smaller < High(integer)) then
  begin
    Found := FindSize(Found, Search.Larger, Search.Smaller);
    if Length(Found) = 0 then
      exit;
  end;

  // slow searches: each message has to be read
  if (Search.SentSince > 0) or (Search.SentBefore < High(int64)) or
    (Search.HeaderStrings.Count > 0) or (Search.BodyStrings.Count > 0) or
    (Search.TextStrings.Count > 0) then
  begin
    Found := FindContent(Found, Search.SentSince, Search.SentBefore,
      Search.Charset, Search.HeaderStrings,
      Search.BodyStrings, Search.TextStrings);
    if Length(Found) = 0 then
      exit;
  end;

  // inferior nested searches  (OR / NOT)
  if Search.Subs.Count > 0 then
    for i := 0 to Search.Subs.Count - 1 do
      with TSearchSub(Search.Subs[i]) do
        if Assigned(Sub2) then
          Found := JoinMessageSets(Find(Sub1, Found), Find(Sub2, Found))
        else
          Found := FindMessageSets(Found, Find(Sub1, Found), True);

  SetLength(Result, Length(Found));
  for i := 0 to High(Result) do
    Result[i] := Found[i];
end;

function TImapMailbox.FindMessageSets(MsgSet1, MsgSet2: TMessageSet;
  Exclude: boolean): TMessageSet;
  // Returns messages that are part of MsgSet1 AND (not) MsgSet2
var
  i, j, k: integer;
  Found: boolean;
begin
  SetLength(Result, Length(MsgSet1));
  j := 0;
  for i := 0 to High(MsgSet1) do
  begin
    Found := False;
    for k := 0 to High(MsgSet2) do
    begin
      if (MsgSet2[k] = MsgSet1[i]) then
      begin
        Found := True;
        break;
      end;
    end;
    if Found xor Exclude then
    begin
      Result[j] := MsgSet1[i];
      Inc(j);
    end;
  end;
  SetLength(Result, j);
end;

function TImapMailbox.JoinMessageSets(MsgSet1, MsgSet2: TMessageSet): TMessageSet;
  // Returns messages that are part of MsgSet1 OR MsgSet2
var
  i, j, k: integer;
  Unique: boolean;
begin
  SetLength(Result, Length(MsgSet1) + Length(MsgSet2));
  j := Length(MsgSet1);
  for i := 0 to j - 1 do
    Result[i] := MsgSet1[i];
  for i := 0 to High(MsgSet2) do
  begin
    Unique := True;
    for k := 0 to High(MsgSet1) do
      if MsgSet2[i] = MsgSet1[k] then
      begin
        Unique := False;
        break;
      end;
    if Unique then
    begin
      Result[j] := MsgSet2[i];
      Inc(j);
    end;
  end;
  SetLength(Result, j);
end;

function TImapMailbox.GetEnvelope(MyMessage: TMimeMess): string;
begin
  Result := '(' + MyMessage.Header.FindHeader( 'date:' ) +
            ' ' + MyMessage.Header.Subject +
            ' ' + GetAddresses(MyMessage, 'from:' ) +
            ' ' + GetAddresses(MyMessage, 'sender:' ) +
            ' ' + GetAddresses(MyMessage, 'reply-to:' ) +
            ' ' + GetAddresses(MyMessage, 'to:' ) +
            ' ' + GetAddresses(MyMessage, 'cc:' ) +
            ' ' + GetAddresses(MyMessage, 'bcc:' ) +
            ' ' + MyMessage.Header.ReplyTo +
            ' ' + MyMessage.Header.MessageID + ')';
end;

function NString( Data: String ): String;
Var i, j: Integer;
begin
   if Data = '' then begin
      Result := 'NIL'
   end else begin
      Result := '"';
      j := 1;
      // Escape quotes and backslashes: '"' -> '\"' and '\' -> '\\'
      for i := 1 to Length( Data ) do begin
         if Data[i] in ['"','\'] then begin
            Result := Result + Copy( Data, j, i-j ) + '\';
            j := i;
         end
      end;
      Result := Result + Copy( Data, j, Length(Data)-j+1 ) + '"'
   end
end;

function TImapMailbox.GetBodyFields(Part : TMimePart): string;
begin
  Result := '("charset" "'+Part.Charset + '") ' +
            NString(Part.ContentID) + ' ' +
            NString(Part.Description) + ' ' +
            NString(Part.Encoding) + ' ' +
            IntToStr( Length( Part.Lines.Text ) )
end;

function TImapMailbox.BodyStructure(Part: TMimePart; Extensible: Boolean
  ): String;
var  Data : String;
     i    : Integer;
begin
  if Uppercase(Part.Primary) = 'MULTIPART' then
    begin  // Multipart
      // Multiple parts are indicated by parenthesis nesting.  Instead of
      // a body type as the first element of the parenthesized list there is
      // a nested body.  The second element of the parenthesized list is the
      // multipart subtype (mixed, digest, parallel, alternative, etc.). }
      Data := '(';
      for i := 0 to Part.GetSubPartCount-1 do
          Data := Data + BodyStructure(Part.GetSubPart(i),Extensible );
      Data := Data + ' "' + Part.Secondary +'"';

      //if Extensible then
      //  Data := Data + ' ' + Part.ContentID + ' ' + Part.Disposition GetBodyDisposition + ' ' + GetBodyLanguage;
    end
  else
    begin
      Data := '("' + Part.Primary + '" "' + Part.Secondary + '" ' + GetBodyFields(Part);
      if uppercase(Part.Primary) = 'TEXT' then Data := Data + ' ' + IntToStr( Part.Lines.Count )
      else if (uppercase(Part.Primary) = 'MESSAGE') and (uppercase(Part.Secondary) = 'RFC822') then
        begin
          {
          if Part.GetSubPartCount > 0 then
            Data := Data + ' ' +  GetEnvelope + ' ' +
                      Parts[0].BodyStructure( Extensible ) + ' ' +
                      IntToStr( Lines )
           else
              LogRaw( LOGID_WARN, 'Error parsing RFC822 message: There is no message.' );
              }
        end;

        //if Extensible then Data := Data + ' ' + GetBodyMD5 + ' ' + GetBodyDisposition + ' ' + GetBodyLanguage;
     end;
     Result := Data + ')'
end;

function TImapMailbox.GetAddresses(MyMessage: TMimeMess; HdrNam: String
  ): String;
var
  i : Integer;
  s : String;
begin
  s := MyMessage.Header.FindHeader(HdrNam);
  if s = '' then
    begin
      // default Sender: and Reply-To: to From:
      if (HdrNam = 'reply-to:') or (HdrNam = 'sender:') then
        s := MyMessage.Header.From;
     end;
     if s = '' then begin
        Result := 'NIL'
     end else begin
        // TODO
        // ----
        // [RFC-822] group syntax is indicated by a special form of address
        // structure in which the host name field is NIL.  If the mailbox
        // name field is also NIL, this is an end of group marker
        // (semi-colon in RFC 822 syntax).  If the mailbox name field is
        // non-NIL, this is a start of group marker, and the mailbox name
        // field holds the group name phrase.
        i := Pos( ',', s );
        while i > 0 do begin
           Result := Result + GetAddressStructure( TrimWhSpace( Copy( s, 1, i-1 ) ) );
           System.Delete( s, 1, i );
           i := Pos( ',', s )
        end;
        Result := '(' + Result + GetAddressStructure( TrimWhSpace( s ) ) + ')';
     end
end;

procedure SplitStringIdx( Idx: Integer; var Data: String; out Rest: String );
begin
   if Idx > 0 then begin
      Rest := Copy( Data, Idx+1, Length(Data)-Idx );
      SetLength( Data, Idx-1 );
   end else Rest := '';
end;

procedure SplitStringWhSpace( var Data: String; out Rest: String );
begin
     SplitStringIdx( PosWhSpace( Data ), Data, Rest );
     TrimWhSpace( Rest );
end;

procedure SplitString( SubStr: String; var Data: String; out Rest: String );
begin
  SplitStringIdx( Pos( SubStr, Data ), Data, Rest );
end;

function TImapMailbox.GetBodySection(MyMessage: TMimeMess; Section: string;
  Nested: Boolean; var Offset, Maximum: Integer): string;
// The text of a particular body section. The section specification is a set
// of zero or more part specifiers delimited by periods. A part specifier is
// either a part number or one of the following: HEADER, HEADER.FIELDS,
// HEADER.FIELDS.NOT, MIME, and TEXT. An empty section specification refers
// to the entire message, including the header.
var
  Part       : Integer;
  SubSection : String;
  Fields     : String;
begin
  Result := '';
  SplitString( '.', Section, SubSection );
  // The TEXT part specifier refers to the text body of the message,
  // omitting the [RFC-822] header.
  if Section = 'TEXT' then begin
    Result := FullBody(MyMessage)
  end else if Section = '' then
    begin
      if Nested then Result := FullBody(MyMessage)
      else Result := Text(MyMessage)
      end else if Section = 'HEADER' then begin
        if SubSection = '' then Result := FullHeader(MyMessage) + CRLF
        else if Copy( SubSection, 1, 6 ) = 'FIELDS' then begin
           SplitStringWhSpace( Subsection, Fields );
           Result := GetHeaderFields(MyMessage, TrimParentheses(Fields),Copy(SubSection,7,4) = '.NOT' ) + CRLF
        end

     // The MIME part specifier refers to the [MIME-IMB] header for this part.
     // The MIME part specifier MUST be prefixed by one or more numeric part specifiers.
     end else if Section = 'MIME' then begin
        if Nested then Result := FullHeader(MyMessage) + CRLF
        else
          begin
            //LogRaw( LOGID_WARN, 'Invalid Client operation: Fetching body section - MIME '+
            //                  'part specifier not prefixed by numeric part specifier.' )
          end;

     end else begin
        Part := StrToInt( Section );
        if Part > 0 then begin
           if MyMessage.MessagePart.ContentID = 'MULTIPART' then begin
              if Part > MyMessage.MessagePart.GetSubPartCount then
                 //LogRaw( LOGID_WARN, 'Invalid Client operation: Fetching part '+
                 //     IntToStr(Part) + ' of ' + IntToStr(Length(Parts)) + ' part message.' )
              else Result := MyMessage.MessagePart.GetSubPart(Part-1).Lines.Text;
           end else begin
              // Non-[MIME-IMB] messages, and non-multipart [MIME-IMB] messages
              // with no encapsulated message, only have a part 1.
              if Part = 1 then Result := FullBody(MyMessage)
              else //LogRaw( LOGID_WARN, 'Invalid Client operation: Fetching part ' +
                   //     IntToStr(Part) + ' of non-multipart message.' );
           end;
        end else //LogRaw( LOGID_WARN, 'Invalid Client operation: ' +
                 //     'Fetching body section ' + Section + '.' );
     end;

     if Offset > 0 then begin
        System.Delete( Result, 1, Offset );
        Offset := 0;
     end;
     if Maximum > -1 then begin
        if Maximum < Length(Result) then SetLength( Result, Maximum );
        Maximum := -1;
     end;
end;

function TImapMailbox.BodySection(MyMessage: TMimeMess; var Section: String
  ): String;
var  Offset, Maximum, i: Integer;
     s: String;
begin
  Offset  :=  0;
  Maximum := -1;

  i := Pos('<', Section);
  if (i > 0) and (Section[Length(Section)] = '>') then begin
    s := Copy( Section, i, Length(Section)-i+1 );
    i := Pos( '.', s );
    if i <> 0 then begin
       Offset  := StrToIntDef( Copy( s, 2, i-2 ), 0 );
       Maximum := StrToIntDef( Copy( s, i+1, Length(s)-i-1 ), -1 );
       System.Delete( Section, Pos('<',Section)+i-1, Length(s)-i );
    end
  end;

  Result := GetBodySection(MyMessage, Copy( Section, 6, Pos(']',Section)-6 ),  False, Offset, Maximum );
end;

function TImapMailbox.GetAddressStructure(Address: String): String;
// An address structure is a parenthesized list that describes an electronic
// mail address.  The fields of an address structure are in the following
// order: personal name, [SMTP] at-domain-list (source route), mailbox name,
// and host name.
var  Name, Mailbox, Host, s : String;
     i : Integer;
begin
   Mailbox     := 'NIL';
   Host        := 'NIL';

   s := Address;
   i := PosWhSpace( s );
   if Pos( '@', s ) < i then begin       // gray@cac.washington.edu (Terry Gray)
      Name := TrimParentheses( Copy( s, i+1, Length(s)-i ) );
      SetLength( s, i-1 );
   end else begin                        // Terry Gray <gray@cac.washington.edu>
      for i := Length(s)-5 downto 1 do if s[i] in [#9,' '] then begin
         Name := TrimQuotes( Copy( s, 1, i-1 ) );
         System.Delete( s, 1, i );
         break
      end
   end;
   Name := NString( Name );
   s := GetEmailAddr( s );
   i := Pos( '@', s );
   if i > 0 then begin
      Mailbox := NString( Copy( s, 1, i-1 ) );
      Host    := NString( Copy( s, i+1, Length(s) ) );
   end else
      Mailbox := NString( s );

   // ("Terry Gray" NIL "gray" "cac.washington.edu")
   Result := '(' + Name + ' NIL ' + Mailbox + ' ' + Host + ')'
end;

function TImapMailbox.GetHeaderFields(MyMessage: TMimeMess; Fields: String;
  Exclude: Boolean): String;
var
  tmp: String;
  tmp1: String;
  sl: TStringList;
  i: Integer;
  aIdx: Integer;
begin
  tmp := Fields;
  sl := TStringList.Create;
  sl.NameValueSeparator:=':';
  sl.Text:=FullHeader(MyMessage);
  for i := 0 to sl.Count-1 do
    if sl.Names[i]<>lowercase(sl.Names[i]) then
      begin
        sl[i] := lowercase(copy(sl[i],0,pos(':',sl[i])-1))+':'+copy(sl[i],pos(':',sl[i])+1,length(sl[i]));
      end;
  while pos(' ',tmp)>0 do
    begin
      tmp1 := '';
      aIdx := sl.IndexOfName(lowercase(copy(tmp,0,pos(' ',tmp)-1)));
      if aIdx>-1 then
        Result := Result+CRLF+sl[aIdx];
      tmp := copy(tmp,pos(' ',tmp)+1,length(tmp));
    end;
  System.Delete(Result,1,2);
  Result := Trim( Result ) + CRLF;
  sl.Free;
end;

function TImapMailbox.FullBody(MyMessage: TMimeMess): string;
begin
  Result := MyMessage.MessagePart.Lines.Text;
end;

function TImapMailbox.FullHeader(MyMessage: TMimeMess): string;
var
  sl: TStringList;
begin
  sl := TStringList.Create;
  MyMessage.Header.EncodeHeaders(sl);
  Result := sl.Text;
end;

function TImapMailbox.Text(MyMessage: TMimeMess): string;
begin
  Result := MyMessage.Lines.Text;
end;

function TImapMailbox.FindFlags(MsgSet: TMessageSet; Flags: TFlagMask;
  Exclude: boolean): TMessageSet;
var
  i, j: integer;
begin
  SetLength(Result, Length(MsgSet));
  j := 0;
  for i := 0 to High(MsgSet) do
    if ((GetFlags(MsgSet[i] - 1) and Flags) = Flags) xor Exclude then
    begin
      Result[j] := MsgSet[i];
      Inc(j);
    end;
  SetLength(Result, j);
end;

function TImapMailbox.FindTimeStamp(MsgSet: TMessageSet;
  After, Before: int64): TMessageSet;
var
  i, j: integer;
  MyDate: int64;
begin
  SetLength(Result, Length(MsgSet));
  j := 0;
  for i := 0 to High(MsgSet) do
  begin
    MyDate := Trunc(UnixTimeToDateTime(GetTimeStamp(MsgSet[i] - 1)));
    if (MyDate > After) and (MyDate < Before) then
    begin
      Result[j] := MsgSet[i];
      Inc(j);
    end;
  end;
  SetLength(Result, j);
end;

end.
