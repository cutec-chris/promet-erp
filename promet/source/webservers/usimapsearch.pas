unit usimapsearch;

// --------------------------------------------------------------------------
// Design at least inspired by the Cyrus IMAP Server
// http://asg.web.cmu.edu/cyrus
// --------------------------------------------------------------------------

interface

uses Classes;

type
  TFlagMask = LongInt;   // Bitmask containing status flags of the message
  TIMAPSearch = class
     private
       FFlagsSet      : TFlagMask;
       FFlagsUnset    : TFlagMask;
       FSmaller       : Integer;
       FLarger        : Integer;
       FBefore        : Int64;
       FSince         : Int64;
       FSentBefore    : Int64;
       FSentSince     : Int64;
       FCharset       : String;
       FSequence      : TStringList;
       FUIDSequence   : TStringList;
       FBodyStrings   : TStringList;
       FTextStrings   : TStringList;
       FHeaderStrings : TStringList;
       FSubList       : TList;
       procedure SetFlags      ( Value: TFlagMask );
       procedure UnsetFlags    ( Value: TFlagMask );
       procedure SetSmaller    ( Value: Integer );
       procedure SetLarger     ( Value: Integer );
       procedure SetBefore     ( Value: Int64 );
       procedure SetSince      ( Value: Int64 );
       procedure SetSentBefore ( Value: Int64 );
       procedure SetSentSince  ( Value: Int64 );
     public
       property  FlagsSet      : TFlagMask   read FFlagsSet   write SetFlags;
       property  FlagsUnset    : TFlagMask   read FFlagsUnset write UnsetFlags;
       property  Smaller       : Integer     read FSmaller    write SetSmaller;
       property  Larger        : Integer     read FLarger     write SetLarger;
       property  Before        : Int64       read FBefore     write SetBefore;
       property  Since         : Int64       read FSince      write SetSince;
       property  SentBefore    : Int64       read FSentBefore write SetSentBefore;
       property  SentSince     : Int64       read FSentSince  write SetSentSince;
       property  Charset       : String      read FCharset;
       property  Sequence      : TStringList read FSequence;
       property  UIDSequence   : TStringList read FUIDSequence;
       property  BodyStrings   : TStringList read FBodyStrings;
       property  TextStrings   : TStringList read FTextStrings;
       property  HeaderStrings : TStringList read FHeaderStrings;
       property  Subs          : TList       read FSubList    write FSubList;
       procedure AddSeqStr     ( Value: String );
       procedure AddUIDStr     ( Value: String );
       procedure AddBodyStr    ( Value: String );
       procedure AddTextStr    ( Value: String );
       procedure AddHeaderStr  ( Header, Value: String );
       constructor Create( ACharset: String );
       destructor Destroy; override;
   end;

   TSearchSub = class
     public
       Sub1     : TIMAPSearch;     // If sub2 is null, then sub1 is NOT'ed.
       Sub2     : TIMAPSearch;     // Otherwise sub1 and sub2 are OR'ed.
       destructor Destroy; override;
   end;
const
   FLAGNONE     : TFlagMask =  0;
   FLAGSEEN     : TFlagMask =  1;
   FLAGANSWERED : TFlagMask =  2;
   FLAGFLAGGED  : TFlagMask =  4;
   FLAGDELETED  : TFlagMask =  8;
   FLAGDRAFT    : TFlagMask = 16;
   FLAGRECENT   : TFlagMask = 32;

   FLAGAPPROVE  : TFlagMask = 64;  //Moder
   FLAGDECLINE  : TFlagMask = 128; //Moder

function GetSearchPgm( Args: String; SearchPgm: TIMAPSearch ): Boolean;

// --------------------------------------------------------------------------

implementation

uses SysUtils;

{ TIMAPSearch }

procedure TIMAPSearch.SetFlags( Value: TFlagMask );
begin
     FFlagsSet := FFlagsSet or Value
end;

procedure TIMAPSearch.UnsetFlags( Value: TFlagMask );
begin
     FFlagsUnset := FFlagsUnset or Value
end;

procedure TIMAPSearch.SetSmaller( Value: Integer );
begin
     if Value < FSmaller then FSmaller := Value
end;

procedure TIMAPSearch.SetLarger( Value: Integer );
begin
     if Value > FLarger then FLarger := Value
end;

procedure TIMAPSearch.SetBefore( Value: Int64 );
begin
     if Value < FBefore then FBefore := Value
end;

procedure TIMAPSearch.SetSince( Value: Int64 );
begin
     if Value > FSince then FSince := Value
end;
procedure TIMAPSearch.SetSentBefore( Value: Int64 );
begin
     if Value < FSentBefore then FSentBefore := Value
end;

procedure TIMAPSearch.SetSentSince( Value: Int64 );
begin
     if Value > FSentSince then FSentSince := Value
end;

procedure TIMAPSearch.AddSeqStr( Value: String );
begin
     FSequence.Add( Value );
end;

procedure TIMAPSearch.AddUIDStr( Value: String );
begin
     FUIDSequence.Add( Value );
end;

procedure TIMAPSearch.AddBodyStr( Value: String );
begin
     FBodyStrings.Add( UpperCase( Value ) )
end;

procedure TIMAPSearch.AddTextStr( Value: String );
begin
     FTextStrings.Add( UpperCase( Value ) )
end;

procedure TIMAPSearch.AddHeaderStr( Header, Value: String );
begin
     if Header = '' then exit;
     if Header[Length(Header)] <> ':' then Header := Header + ':';
     FHeaderStrings.Add( Header + UpperCase( Value ) )
end;

constructor TIMAPSearch.Create( ACharset: String );
begin
     inherited Create;

     FCharset       := ACharset;
     FFlagsSet      := FLAGNONE;
     FFlagsUnset    := FLAGNONE;
     FSmaller       := High( Integer );
     FLarger        := 0;
     FBefore        := High( Int64 );
     FSince         := 0;
     FSentBefore    := High( Int64 );
     FSentSince     := 0;
     FSequence      := TStringList.Create;
     FUIDSequence   := TStringList.Create;
     FBodyStrings   := TStringList.Create;
     FTextStrings   := TStringList.Create;
     FHeaderStrings := TStringList.Create;
     FSubList       := TList.Create;
end;

destructor TIMAPSearch.Destroy;
var  i : Integer;
begin
     FSequence.Free;
     FUIDSequence.Free;
     FBodyStrings.Free;
     FTextStrings.Free;
     FHeaderStrings.Free;
     for i := 0 to FSubList.Count - 1 do TSearchSub(FSubList[i]).Free;
     FSubList.Free;

     inherited Destroy;
end;

// --------------------------------------------------------------------------

{ TSearchSub }

destructor TSearchSub.Destroy;
begin
     if Assigned( Sub1 ) then Sub1.Free;
     if Assigned( Sub2 ) then Sub2.Free;

     inherited Destroy;
end;

// --------------------------------------------------------------------------

function GetAString( var Args: String; var Chr: Char ): String;
var  Str: String;
begin
     Result := '';
     if Chr = #0 then exit;
     GetString( Args, Str, IMAP_STRING_ASTRING );
     if Length( Args ) > 0 then begin
        Chr := Args[1];
        System.Delete( Args, 1, 1 )
     end else begin
        if Length( Str ) > 0 then
           Chr := Str[Length(Str)]
        else begin
           Log( LOGID_WARN, 'IMAPSearch.missingarg', 'IMAP: Missing required argument' );
           Chr := #0
        end
     end;
     Result := Str
end;

function GetDate( DateString: String ): Int64;
begin
     Result := Trunc( ImapDateTextToDateTime( DateString) )
end;

procedure ParseSearchPgm( var Args: String; SearchPgm: TIMAPSearch; var Chr: Char ); forward;

procedure ParseCriteria( var Args: String; SearchPgm: TIMAPSearch; var Chr: Char );
var  Key, S: String;
     Sub: TSearchSub;
begin
     Key := UpperCase( GetAString( Args, Chr ) );
     if Key = '' then begin
        if Chr <> '(' then begin
           Log( LOGID_WARN, 'IMAPSearch.searchcommand.missingarg', 'IMAP: Missing required argument in SEARCH command' );
           Chr := #0;
           exit
        end;
        ParseSearchPgm( Args, SearchPgm, Chr );
        if Chr <> ')' then begin
           Log( LOGID_WARN, 'IMAPSearch.missingclosingparen', 'IMAP: Missing required closing paren in SEARCH command' );
           Chr := #0
        end else if Args <> '' then begin
           Chr := Args[1];
           System.Delete( Args, 1, 1 )
        end;
        exit
     end;
     case Key[1] of
        '0'..'9','*': SearchPgm.AddSeqStr( Key );
        'A': If Key = 'ANSWERED' then SearchPgm.FlagsSet := FLAGANSWERED;
        'B': begin
                S := GetAString( Args, Chr );
                if Key = 'BCC' then SearchPgm.AddHeaderStr( 'bcc:', S )
                else if Key = 'BEFORE' then SearchPgm.Before := GetDate( S )
                else if Key = 'BODY' then SearchPgm.AddBodyStr( S )
             end;
        'C': if Key = 'CC' then SearchPgm.AddHeaderStr( 'cc:', GetAString( Args, Chr ) );
        'D': if Key = 'DELETED' then SearchPgm.FlagsSet := FLAGDELETED
             else if Key = 'DRAFT' then SearchPgm.FlagsSet := FLAGDRAFT;
        'F': if Key = 'FLAGGED' then SearchPgm.FlagsSet := FLAGFLAGGED
             else if Key = 'FROM' then SearchPgm.AddHeaderStr( 'from:', GetAString( Args, Chr ) );
        'H': if Key = 'HEADER' then begin
                S := GetAString( Args, Chr );
                if S <> '' then SearchPgm.AddHeaderStr( S, GetAString( Args, Chr ) )
             end;
        'K': if Key = 'KEYWORD' then begin
                Log( LOGID_WARN, 'IMAPSearch.KeyWord.NotImplemented',  'IMAP: the KEYWORD search is not implemented.' );
                GetAString( Args, Chr );
             end;
        'L': if Key = 'LARGER' then SearchPgm.Larger := StrToIntDef( GetAString( Args, Chr ), 0 );
        'N': if Key = 'NEW' then begin
                SearchPgm.FlagsSet := FLAGRECENT;
                SearchPgm.FlagsUnset := FLAGSEEN;
             end  else 
             if Key = 'NOT' then begin
                Sub := TSearchSub.Create;
                try
                   Sub.Sub1 := TIMAPSearch.Create( SearchPgm.Charset );
                   ParseCriteria( Args, Sub.Sub1, Chr );
                   if Chr <> #0 then
                      SearchPgm.Subs.Add( Sub )
                   else begin
                      Log( LOGID_WARN, 'IMAPSearch.searchnotcommand.missingarg', 
                         'IMAP: Missing argument in SEARCH NOT command' );
                      Sub.Free
                   end
                except
                   Sub.Free
                end
             end;
        'O': if Key = 'OLD' then begin
                SearchPgm.FlagsUnset := FLAGRECENT
             end else 
             if Key = 'ON' then begin
                S := GetAString( Args, Chr );
                SearchPgm.Before := GetDate( S ) + 1;
                SearchPgm.Since  := GetDate( S ) - 1;
             end else 
             if Key = 'OR' then begin
                Sub := TSearchSub.Create;
                try
                   Sub.Sub1 := TIMAPSearch.Create( SearchPgm.Charset );
                   Sub.Sub2 := TIMAPSearch.Create( SearchPgm.Charset );
                   ParseCriteria( Args, Sub.Sub1, Chr );
                   if Chr = ' ' then ParseCriteria( Args, Sub.Sub2, Chr );
                   if Chr <> #0 then
                      SearchPgm.Subs.Add( Sub )
                   else begin
                      Log( LOGID_WARN, 'IMAPSearch.searchorcommand.missingarg', 
                         'IMAP: Missing argument in SEARCH OR command' );
                      Sub.Free
                   end
                except
                   Sub.Free
                end
             end;
        'R': if Key = 'RECENT' then SearchPgm.Flagsset := FLAGRECENT;
        'S': if Key = 'SEEN' then begin
                SearchPgm.FlagsSet := FLAGSEEN
             end else begin
                 S := GetAString( Args, Chr );
                 if Key = 'SENTBEFORE' then SearchPgm.SentBefore := GetDate( S )
                 else if Key = 'SENTON' then begin
                    SearchPgm.SentBefore := GetDate( S ) + 1;
                    SearchPgm.SentSince  := GetDate( S ) - 1;
                 end 
                 else if Key = 'SENTSINCE' then SearchPgm.SentSince := GetDate( S )
                 else if Key = 'SINCE' then SearchPgm.Since := GetDate( S )
                 else if Key = 'SMALLER' then SearchPgm.Smaller := StrToIntDef( S, High(Integer) )
                 else if Key = 'SUBJECT' then SearchPgm.AddHeaderStr( 'subject:', S )
             end;
        'T': begin
                S := GetAString( Args, Chr );
                if Key = 'TEXT' then SearchPgm.AddTextStr( S )
                else if Key = 'TO' then SearchPgm.AddHeaderStr( 'to:', S )
              end;
        'U': begin
                if      Key = 'UID' then SearchPgm.AddUIDStr( GetAString( Args, Chr ) )
                else if Key = 'UNANSWERED' then SearchPgm.FlagsUnset := FLAGANSWERED
                else if Key = 'UNDELETED' then SearchPgm.FlagsUnset := FLAGDELETED
                else if Key = 'UNDRAFT' then SearchPgm.FlagsUnset := FLAGDRAFT
                else if Key = 'UNFLAGGED' then SearchPgm.FlagsUnset := FLAGFLAGGED
                else if Key = 'UNSEEN' then SearchPgm.FlagsUnset := FLAGSEEN
                else if Key = 'UNKEYWORD' then begin
                   Log( LOGID_WARN, 'IMAPSearch.keywordsearch.notimplementedyet', 
                     'IMAP: the KEYWORD search is not implemented.' );
                   GetAString( Args, Chr );
                end 
             end
     end;
end;

procedure ParseSearchPgm( var Args: String; SearchPgm: TIMAPSearch; var Chr: Char );
begin
     repeat ParseCriteria( Args, SearchPgm, Chr )
     until Chr <> ' ';
end;

function GetSearchPgm( Args: String; SearchPgm: TIMAPSearch ): Boolean;
var  Chr : Char;
begin
     LogRaw( LOGID_DETAIL, 'Parsing search criteria ...' );
     Chr := #1; // Bugfix
     ParseSearchPgm( Args, SearchPgm, Chr );
     Result := (Chr <> #0)
end;

// --------------------------------------------------------------------------

end.
