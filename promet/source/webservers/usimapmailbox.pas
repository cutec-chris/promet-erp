unit usimapmailbox;

interface

uses Classes, usimapsearch;

type
  TMessageSet = array of integer;
  TStoreMode = set of (smAdd, smReplace, smDelete);

  tOnNewMess = procedure of object; //HSR //IDLE
  tOnExpunge = procedure(Number: integer) of object; //HSR //IDLE

  pIMAPNotification = ^tIMAPNotification;

  tIMAPNotification = record //HSR //IDLE
    OnNewMess: tOnNewMess;
    OnExpunge: tOnExpunge;
  end;

  TImapMailbox = class
  private
    fCritSection: TRTLCriticalSection;
    //fIndex       : TImapMailboxIndex;
    fPath: string;
    FUIDNext: longint;
    FUIDvalidity: TUnixTime;
    FUnseen: longint;
    FRecent: longint;
    FMessages: longint;
    //fStatus      : TMbxStatus;
    fUsers: TList;
    fReadOnly: boolean; //ClientRO
    //function  GetStatus: TMbxStatus;
    procedure AddMessage(Flags: string; TimeStamp: TUnixTime);
    function StringToFlagMask(Flags: string): TFlagMask;
    function FlagMaskToString(FlagMask: TFlagMask): string;
    function GetPossFlags: string;
    //procedure WriteStatus; //Not Critical_Section-Protected!
    {MG}{Search-new}
    function Find(Search: TIMAPSearch; MsgSet: TMessageSet): TMessageSet;
    function JoinMessageSets(MsgSet1, MsgSet2: TMessageSet): TMessageSet;
    function FindMessageSets(MsgSet1, MsgSet2: TMessageSet;
      Exclude: boolean): TMessageSet;
    function FindFlags(MsgSet: TMessageSet; Flags: TFlagMask;
      Exclude: boolean): TMessageSet;
    function FindSize(MsgSet: TMessageSet; Min, Max: integer): TMessageSet;
    function FindTimeStamp(MsgSet: TMessageSet; After, Before: int64): TMessageSet;
    function FindContent(MsgSet: TMessageSet; After, Before: int64;
      Charset: string; HeaderList, BodyStrings,
      TextStrings: TStringList): TMessageSet;
    {/Search-new}
  public
    function StrToMsgSet(s: string; UseUID: boolean): TMessageSet;

    //property  Status         : TMBxStatus read  fStatus;
    property Path: string read fPath;
    property MBReadOnly: boolean read fReadOnly write fReadOnly;
    //ClientRO //ToDo: 'write' von ReadOnly löschen oder umfunktionieren
    property GetUIDnext: longint read FUIDNext;
    property GetUIDvalidity: TUnixTime read FUIDvalidity;
    property PossFlags: string read GetPossFlags;
    procedure Lock;
    procedure Unlock;
    procedure RemoveRecentFlags;
    procedure AddUser(Notify: pIMAPNotification);
    procedure RemoveUser(Notify: pIMAPNotification; out NoUsersLeft: boolean);
    procedure Expunge(ExcludeFromResponse: pIMAPNotification);

    function Search(SearchStruct: TIMAPSearch; UseUID: boolean): string;
    {MG}{Search-new}
    function Fetch(Idx: integer; MsgDat: string; var Success: boolean): string;
    function CopyMessage(MsgSet: TMessageSet; Destination: TImapMailbox): boolean;
    function Store(Idx: integer; Flags: string; Mode: TStoreMode): string;
    function AppendMessage(MsgTxt: string; Flags: string;
      TimeStamp: TUnixTime): string;
    function AreValidFlags(Flags: string): boolean;

    procedure AddIncomingMessage(const Flags: string = '');
    procedure SendMailboxUpdate;

    property Messages : Integer read FMessages;
    property Recent : Integer read FRecent;
    property Unseen : Integer read FUnseen;

    constructor Create(APath: string);
    destructor Destroy; override;
  end;

  function NowGMT: TDateTime;

implementation

uses SysUtils, syncobjs,uBaseApplication;

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
  {
  if fReadOnly then begin
    Result := FlagMaskToString(fIndex.GetFlags( Idx ));
    exit
  end;
  try
     Lock;
     FlagMsk := StringToFlagMask(Flags);
     if FlagMsk and FLAGSEEN = FLAGSEEN then begin
        if fIndex.GetFlags(Idx) and FLAGSEEN = FLAGSEEN then begin
           if Mode = [smDelete] then inc( fStatus.Unseen )
        end else begin
           if Mode <= [smReplace, smAdd] then dec( fStatus.Unseen )
        end
     end else begin
        if (fIndex.GetFlags(Idx) and FLAGSEEN = FLAGSEEN) then
           if Mode = [smReplace] then inc( fStatus.Unseen );
     end;

     if      Mode = [smReplace] then FlagMsk := fIndex.SetFlags   ( Idx, FlagMsk )
     else if Mode = [smDelete]  then FlagMsk := fIndex.RemoveFlags( Idx, FlagMsk )
     else if Mode = [smAdd]     then FlagMsk := fIndex.AddFlags   ( Idx, FlagMsk )
     else                            FlagMsk := fIndex.GetFlags   ( Idx ); // just in case ...
     Result := FlagMaskToString(FlagMsk);
     WriteStatus
  finally
     Unlock;
  end;
  }
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

{AP2}{Destination-Lock fuer MessageCopy}
function TImapMailbox.CopyMessage(MsgSet: TMessageSet;
  Destination: TImapMailbox): boolean;
begin
end;

{/Destination-Lock fuer MessageCopy}

function TImapMailbox.AppendMessage(MsgTxt: string; Flags: string;
  TimeStamp: TUnixTime): string;
var
  Bytes: integer;
  FileName: string;
begin
  Result := 'NO APPEND error: [Read-Only] ';
  if fReadOnly then
    exit; //ClientRO

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
  //     MyMail: TImapMessage;

  procedure AddDataValue(NewValue: string);
  begin
    Data := Data + ' ' + DataItem + ' ' + NewValue;
  end;

begin
  Args := MsgDat;
  Data := '';
  //MyMail := nil;
   {
   Filename := fPath + fIndex.GetUIDStr( Idx ) + '.'+Def_Extension_Mail;
   if not FileExists2( Filename ) then exit;
   try
      repeat
         DataItem := ExtractParameter( Args );
         if DataItem = 'FLAGS' then begin
            AddDataValue( FlagMaskToString( fIndex.GetFlags(Idx) ) )
         end else
         if DataItem = 'INTERNALDATE' then begin
            AddDataValue( '"' + DateTimeGMTToImapDateTime( UnixTimeToDateTime(
                          fIndex.GetTimeStamp(Idx) ), '+0000' ) + '"' )
         end else
         if DataItem = 'UID' then begin
            AddDataValue( fIndex.GetUIDStr(Idx) )
         end else begin
            If Not Assigned(MyMail) then begin
               MyMail := TImapMessage.Create;
               MyMail.LoadFromFile( Filename )
            end;
            if DataItem = 'ENVELOPE' then AddDataValue( MyMail.Envelope )
            else if DataItem = 'RFC822' then AddDataValue( MakeLiteral( MyMail.Text ) )
            else if DataItem = 'RFC822.HEADER' then AddDataValue( MakeLiteral( MyMail.FullHeader + #13#10 ) )
            else if DataItem = 'RFC822.TEXT' then AddDataValue( MakeLiteral( MyMail.FullBody ) )
            else if DataItem = 'RFC822.SIZE' then AddDataValue( IntToStr( Length(MyMail.Text) ) )
            else if DataItem = 'BODYSTRUCTURE' then AddDataValue( MyMail.BodyStructure( True ) )
            else if DataItem = 'BODY' then AddDataValue( MyMail.BodyStructure( False ) )
            else if Copy( DataItem, 1, 4 ) = 'BODY' then begin
               if Copy( DataItem, 5, 5 ) = '.PEEK' then System.Delete( DataItem, 5, 5 )
               else if fIndex.GetFlags( Idx ) and FLAGSEEN <> FLAGSEEN then begin
                  // The \Seen flag is implicitly set; if this causes the flags to
                  // change they SHOULD be included as part of the FETCH responses.
                  Store(idx, '\seen', [smAdd]);
                  Args := Args + ' FLAGS';
               end;
               AddDataValue( MakeLiteral( MyMail.BodySection( DataItem ) ) )
            end else begin
               Log( LOGID_WARN, 'IMAPMailbox.Fetch.UnsupportedPar',
                  'Unsupported Imap FETCH parameter: "%s"', DataItem);
               Success := False
            end
         end;
      until Args = '';
   finally
      FreeAndNil(MyMail)
   end;
   System.Delete( Data, 1, 1 );
   Result := IntToStr(Idx+1) + ' FETCH (' + Data + ')'
   }
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

constructor TImapMailbox.Create(APath: string);
begin
  inherited Create;
  InitCriticalSection(fCritSection);
  fPath := APath;
  ForceDirectories(APath);
  fUsers := TList.Create;
  fReadOnly := False; //ClientRO
  //ToDo: Read-Only einzelner Mailboxen kann hier gesteuert werden.
end;

destructor TImapMailbox.Destroy;
begin
  if fUsers.Count > 0 then
    raise Exception.Create('TImapMailbox.Destroy: imap mailbox is still in use!');
  fUsers.Free; // TODO: User direkt aufräumen oder warnen?
  DoneCriticalSection(fCritSection);

  inherited;
end;

function TImapMailbox.StrToMsgSet(s: string; UseUID: boolean): TMessageSet;

  function SeqNumber(s: string): integer;
  var
    i: integer;
  begin
    {
    if UseUID then
      Result := fIndex.GetUID(Status.Messages - 1)
    else
      Result := Status.Messages;
    }
    if s = '*' then
      exit;
    if s = '4294967295' // ugly workaround - we should have used u_int32
    then
      i := 2147483647
    else
      i := StrToInt(s);
    if i > Result then
      Inc(Result)
    else
      Result := i;
  end;

  function GetSet(s: string): TMessageSet;
  var
    i, j, Start, Finish: integer;
  begin
    i := Pos(':', s);
    if i > 0 then
    begin
      Start := SeqNumber(copy(s, 1, i - 1));
      System.Delete(s, 1, i);
    end
    else
      Start := SeqNumber(s);
    Finish := SeqNumber(s);
    if Finish < Start then
    begin
      i := Finish;
      Finish := Start;
      Start := i;
    end;
    SetLength(Result, Finish - Start + 1);
    j := 0;
    for i := Start to Finish do
    begin
      {
      if UseUID then
        Result[j] := fIndex.GetIndex(i) + 1
      else
        Result[j] := i;
      if (Result[j] > 0) and (Result[j] <= Status.Messages) then
        Inc(j);
      }
    end;
    SetLength(Result, j);
  end;

var
  i: integer;
begin
  SetLength(Result, 0);
  s := TrimWhSpace(s);
  if s > '' then
  begin
    i := Pos(',', s);
    while i > 0 do
    begin
      Result := JoinMessageSets(Result, GetSet(copy(s, 1, i - 1)));
      System.Delete(s, 1, i);
      i := Pos(',', s);
    end;
    Result := JoinMessageSets(Result, GetSet(s));
  end;
end;

{MG}{Search-new}
function TImapMailbox.Search(SearchStruct: TIMAPSearch; UseUID: boolean): string;
var
  FoundMessages: TMessageSet;
  i: integer;
begin
  Result := '';
  FoundMessages := Find(SearchStruct, StrToMsgSet('1:*', False));
  {
  for i := 0 to Length(FoundMessages) - 1 do
    if UseUID then
      Result := Result + ' ' + fIndex.GetUIDStr(FoundMessages[i] - 1)
    else
      Result := Result + ' ' + IntToStr(FoundMessages[i]);
   }
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

function TImapMailbox.FindFlags(MsgSet: TMessageSet; Flags: TFlagMask;
  Exclude: boolean): TMessageSet;
var
  i, j: integer;
begin
  SetLength(Result, Length(MsgSet));
  j := 0;
  {
  for i := 0 to High(MsgSet) do
    if ((fIndex.GetFlags(MsgSet[i] - 1) and Flags) = Flags) xor Exclude then
    begin
      Result[j] := MsgSet[i];
      Inc(j);
    end;
  }
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
  {
  for i := 0 to High(MsgSet) do
  begin
    MyDate := Trunc(UnixTimeToDateTime(fIndex.GetTimeStamp(MsgSet[i] - 1)));
    if (MyDate > After) and (MyDate < Before) then
    begin
      Result[j] := MsgSet[i];
      Inc(j);
    end;
  end;
  }
  SetLength(Result, j);
end;

function TImapMailbox.FindSize(MsgSet: TMessageSet; Min, Max: integer): TMessageSet;
var
  i, j: integer;
  SR: TSearchRec;
begin
  SetLength(Result, Length(MsgSet));
  j := 0;
  if Min < Max then
  begin
    {
    for i := 0 to High(MsgSet) do
    begin
      if SysUtils.FindFirst(fPath + fIndex.GetUIDStr(MsgSet[i] - 1) +
        '.' + Def_Extension_Mail, faAnyFile, SR) = 0 then
      begin
        if (SR.Size > Min) and (SR.Size < Max) then
        begin
          Result[j] := MsgSet[i];
          Inc(j);
        end;
        SysUtils.FindClose(SR);
      end;
    end;
    }
  end;
  SetLength(Result, j);
end;

function TImapMailbox.FindContent(MsgSet: TMessageSet; After, Before: int64;
  Charset: string;
  HeaderList, BodyStrings, TextStrings: TStringList):
TMessageSet;

  function GetCharset(ContentType: string): string;
  var
    i: integer;
  begin
    Result := '';
    ContentType := UpperCase(ContentType);
    i := Pos('CHARSET=', ContentType);
    if i > 0 then
    begin
      System.Delete(ContentType, 1, i + 7);
      Result := UpperCase(QuotedStringOrToken(ContentType));
    end;
  end;

  function Has8BitChar(txt: string): boolean;
  var
    i: integer;
  begin
    Result := True;
    for i := 1 to Length(txt) do
      if Ord(txt[i]) > 127 then
        exit;
    Result := False;
  end;

  function BadCombination(MyCharset: string; SearchStr: string): boolean;
  begin
    Result := False;
    MyCharset := UpperCase(MyCharset);
    if (MyCharset <> 'UTF-8') and (MyCharset <> 'UTF-7') then
    begin
      if (MyCharset = Charset) then
        exit;
      if not Has8BitChar(SearchStr) then
        exit;
    end;
    Result := True;
    with BaseApplication as IBaseApplication do
      Warning(Format('IMAP SEARCH: Search charset (%s) and message charset (%s) differ. The current message is ignored.',[Charset, MyCharset]));
  end;

var
  i, j, k, m: integer;
  HdrValue: string;
  HdrName: string;
  //MyMail: TArticle;
  MyHeader: string;
  MyString: string;
  MyCharset: string;
  MyDate: int64;
  NotFound: boolean;
begin
  SetLength(Result, Length(MsgSet));
  j := 0;
  {
  MyMail := TArticle.Create;
  try
    for i := 0 to High(MsgSet) do
    begin
      MyMail.LoadFromFile(
        fPath + fIndex.GetUIDStr(MsgSet[i] - 1) + '.' +
        Def_Extension_Mail);
      NotFound := False;

      // Search headers
      for k := 0 to HeaderList.Count - 1 do
      begin
        m := Pos(':', HeaderList[k]);
        HdrName := Copy(HeaderList[k], 1, m);
        HdrValue := UpperCase(
          Copy(HeaderList[k], m + 1, Length(HeaderList[k]) - m));
        if HdrValue <> '' then
        begin
          MyHeader := UpperCase(DecodeHeadervalue(
            MyMail.Header[HdrName], MyCharset));
          if BadCombination(MyCharset, HdrValue) then
          begin
            NotFound := True;
            break;
          end;
          if Pos(HdrValue, MyHeader) = 0 then
          begin
            NotFound := True;
            break;
          end;
        end
        else
        begin
          if not MyMail.HeaderExists(HeaderList[k]) then
          begin
            NotFound := True;
            break;
          end;
        end;
      end;
      if NotFound then
        continue;

      // Search message date
      MyDate := Trunc(RfcDateTimeToDateTimeGMT(MyMail.Header['Date:']));
      if (MyDate <= After) or (MyDate >= Before) then
        continue;

      if (BodyStrings.Count > 0) or (TextStrings.Count > 0) then
      begin
        MyCharset := GetCharset(MyMail.Header['Content-Type:']);
        if BadCombination(MyCharset, BodyStrings.Text + TextStrings.Text)
        then
          continue;

        // Search body text
        MyString := UpperCase(MyMail.FullBody);
        for k := 0 to BodyStrings.Count - 1 do
        begin
          if Pos(BodyStrings[k], MyString) = 0 then
          begin
            NotFound := True;
            break;
          end;
        end;
        if NotFound then
          continue;

        // Search full text
        MyString := UpperCase(MyMail.FullHeader) + #13#10 + MyString;
        for k := 0 to TextStrings.Count - 1 do
        begin
          if Pos(TextStrings[k], MyString) = 0 then
          begin
            NotFound := True;
            break;
          end;
        end;
        if NotFound then
          continue;
      end;

      Result[j] := MsgSet[i];
      Inc(j);
    end
  finally
    MyMail.Free
  end;
  }
  SetLength(Result, j);
end;

{/Search-new}
end.
