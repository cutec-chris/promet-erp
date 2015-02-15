unit usimapserver;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, usbaseserver, mimemess, mimepart, blcksock,
  usimapmailbox, usimapsearch;

type

  { TSImapServer }

  TSImapServer = class(TSTcpServer)
  private
    CurrentUserName: String;
    CurrentTag: string;
    FIMAPDelay: Integer;
    FIMAPNCBrain: Boolean;
    FUseIMAPID: Boolean;
    Selected: TImapMailbox;
    CS_THR_IDLE: TRTLCriticalSection;
    SendExpunge: array of integer;
    SendNewMessages: boolean;
    IdleState: boolean;

    procedure LogRaw(AThread: TSTcpThread; Txt: string);
    procedure SendData(AThread: TSTcpThread; const AText: string);
    procedure SendResLit(AThread: TSTcpThread; Txt: string);
    procedure SendResTag(AThread: TSTcpThread; Txt: string);
    procedure SendRes(AThread: TSTcpThread; Txt: string);
    function SendRequest(AThread: TSTcpThread; const AText: string) : string;

    function  MBSelect( Mailbox: string; ReadOnly : Boolean ): boolean;
    function  MBCreate( Mailbox: string ): boolean;
    function  MBDelete( Mailbox: string ): boolean;
    function  MBExists( var Mailbox: string ): boolean;
    function  MBRename( OldName, NewName: String ): Boolean;
    function  MBLogin(  var Mailbox: TImapMailbox; Path: String; LINotify : Boolean ): Boolean; //HSR //IDLE (Chg)
    procedure MBLogout( var Mailbox: TImapMailbox; LOSel : Boolean ); //HSR //IDLE (Chg)

    procedure HandleCommand(AThread: TSTcpThread; const CmdLine: string);

    procedure Cmd_APPEND(AThread: TSTcpThread;Par: string);
    procedure Cmd_AUTHENTICATE(AThread: TSTcpThread;Par: string);
    procedure Cmd_CAPA(AThread: TSTcpThread;Par: string);
    procedure Cmd_CHECK(AThread: TSTcpThread;Par: string);
    procedure Cmd_CLOSE(AThread: TSTcpThread;Par: string);
    procedure Cmd_COPY(AThread: TSTcpThread;Par: string);
    procedure Cmd_CREATE(AThread: TSTcpThread;Par: string);
    procedure Cmd_DELETE(AThread: TSTcpThread;Par: string);
    procedure Cmd_EXAMINE(AThread: TSTcpThread;Par: string);
    procedure Cmd_EXPUNGE(AThread: TSTcpThread;Par: string);
    procedure Cmd_FETCH(AThread: TSTcpThread;Par: string);
    procedure Cmd_HELP(AThread: TSTcpThread;Par: string);
    procedure Cmd_ID(AThread: TSTcpThread;Par: string); //JW //IMAP ID
    procedure Cmd_IDLE(AThread: TSTcpThread;Par: string); //HSR //IDLE
    procedure Cmd_LIST(AThread: TSTcpThread;Par: string);
    procedure Cmd_LOGIN(AThread: TSTcpThread;Par: string);
    procedure Cmd_LOGOUT(AThread: TSTcpThread;Par: string);
    procedure Cmd_LSUB(AThread: TSTcpThread;Par: string);
    procedure Cmd_NCBrain(AThread: TSTcpThread;Par: string);
    procedure Cmd_NOOP(AThread: TSTcpThread;Par: string);
    procedure Cmd_RENAME(AThread: TSTcpThread;Par: string);
    procedure Cmd_SEARCH(AThread: TSTcpThread;Par: string);
    procedure Cmd_SELECT(AThread: TSTcpThread;Par: string);
    procedure Cmd_STARTTLS(AThread: TSTcpThread;Par: string); {MG}{SSL}
    procedure Cmd_STATUS(AThread: TSTcpThread;Par: string);
    procedure Cmd_STORE(AThread: TSTcpThread;Par: string);
    procedure Cmd_SUBSCRIBE(AThread: TSTcpThread;Par: string);
    procedure Cmd_UID(AThread: TSTcpThread;Par: string);
    procedure Cmd_UNSUBSCRIBE(AThread: TSTcpThread;Par: string);

  protected
    procedure SetActive(const AValue: boolean); override;
    procedure Execute(AThread: TSTcpThread); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property IMAPDelay : Integer read FIMAPDelay write FIMAPDelay;
    property UseIMAPID : Boolean read FUseIMAPID write FUseIMAPID;
    property IMAPNCBrain : Boolean read FIMAPNCBrain write FIMAPNCBrain;
  published
  end;

implementation

uses uBaseApplication,base64;

resourcestring
  SIMAPWelcomeMessage = 'service ready';

function RTrim(const s: string): string;
var
  i, l: integer;
begin
  l := Length(s);
  for i := l downto 1 do
  begin
    if (s[i] = #9) or (s[i] = ' ') then
      Dec(l)
    else
      break;
  end;
  if l = 0 then
    Result := ''
  else if (l = Length(s)) then
      Result := s
    else
      Result := Copy(s, 1, l);
end;

function LTrim(const s: string): string;
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
  if p > l then
    Result := ''
  else if (p = 1) then
      Result := s
    else
      Result := Copy(s, p, l - p + 1);
end;


procedure TSImapServer.HandleCommand(AThread : TSTcpThread;const CmdLine: string);
var
  LogCmdLine, Cmd, Par: string;
  j: integer;
  nCmdLine: string;
begin
  nCmdLine := CmdLine;
  try
    j := PosWhSpace(nCmdLine);
    if j = 0 then
    begin
      Cmd := UpperCase(nCmdLine);
      Par := '';
    end
    else
    begin
      Cmd := UpperCase(copy(nCmdLine, 1, j - 1));
      Par := LTrim(copy(nCmdLine, j + 1, length(nCmdLine)));
    end;

    if (Cmd = 'LOGIN') then
      LogCmdLine := 'LOGIN ' + copy(Par, 1, PosWhSpace(Par) - 1) + ' [...]'
    else if Length(nCmdLine) > 250 then
        LogCmdLine := copy(nCmdLine, 1, 250) + ' [...]'
      else
        LogCmdLine := nCmdLine;

    if CmdLine = '' then
      exit;

    // Workaround for tired OutlookXP Clients
    if IMAPDelay > 0 then
      sleep(IMAPDelay);

    // commands (no authentication required)
    if Cmd = 'HELP' then
    begin
      Cmd_HELP(AThread,Par);
      exit;
    end; //NOT IN THE STANDARD!!!
    if Cmd = 'CAPABILITY' then
    begin
      Cmd_CAPA(AThread,Par);
      exit;
    end;
    if (Cmd = 'ID') and UseIMAPID then
    begin
      Cmd_ID(AThread,Par);
      exit;
    end; //JW //IMAP ID
    if (Cmd = 'NETSCAPE') and FIMAPNCBrain then
    begin
      Cmd_NCBrain(AThread,Par);
      exit;
    end; //HSR //NCBrain

    if Cmd = 'NOOP' then
    begin
      Cmd_NOOP(AThread,Par);
      exit;
    end;
    if Cmd = 'LOGOUT' then
    begin
      Cmd_LOGOUT(AThread,Par);
      exit;
    end;
    if CurrentUserName = '' then
    begin
      if Cmd = 'STARTTLS' then
      begin
        Cmd_STARTTLS(AThread,Par);
        exit;
      end; {MG}{SSL}
      if Cmd = 'AUTHENTICATE' then
      begin
        Cmd_AUTHENTICATE(AThread,Par);
        exit;
      end;
      if Cmd = 'LOGIN' then
      begin
        Cmd_LOGIN(AThread,Par);
        exit;
      end;
    end;

    // check authentication
    if CurrentUserName = '' then
    begin
      with BaseApplication as IBaseApplication do
        Warning(Format('This command need authentication, but client is not authenticated yet: %s',[nCmdLine]));
      SendResTag(AThread,'BAD Authentication required!');
      exit;
    end;

    // commands (authentication required)
    if Cmd = 'LIST' then
    begin
      Cmd_LIST(AThread,Par);
      exit;
    end;
    if Cmd = 'SELECT' then
    begin
      Cmd_SELECT(AThread,Par);
      exit;
    end;
    if Cmd = 'EXAMINE' then
    begin
      Cmd_EXAMINE(AThread,Par);
      exit;
    end;
    if Cmd = 'SUBSCRIBE' then
    begin
      Cmd_SUBSCRIBE(AThread,Par);
      exit;
    end;
    if Cmd = 'UNSUBSCRIBE' then
    begin
      Cmd_UNSUBSCRIBE(AThread,Par);
      exit;
    end;
    if Cmd = 'LSUB' then
    begin
      Cmd_LSUB(AThread,Par);
      exit;
    end;
    if Cmd = 'STATUS' then
    begin
      Cmd_STATUS(AThread,Par);
      exit;
    end;
    if Cmd = 'DELETE' then
    begin
      Cmd_DELETE(AThread,Par);
      exit;
    end;
    if Cmd = 'CREATE' then
    begin
      Cmd_CREATE(AThread,Par);
      exit;
    end;
    if Cmd = 'RENAME' then
    begin
      Cmd_RENAME(AThread,Par);
      exit;
    end;
    if Cmd = 'APPEND' then
    begin
      Cmd_APPEND(AThread,Par);
      exit;
    end;
    if Cmd = 'IDLE' then
    begin
      Cmd_IDLE(AThread,Par);
      exit;
    end; //HSR //IDLE

    if not Assigned(Selected) then
    begin
      with BaseApplication as IBaseApplication do
        Warning(Format('This command need a mailbox be selected, but there is no mailbox selected yet: %s',[nCmdLine]));
      SendResTag(AThread,'BAD Selection of a mailbox is required!');
      exit;
    end;

    //commands for "selected"-mode
    if Cmd = 'CHECK' then
    begin
      Cmd_CHECK(AThread,Par);
      exit;
    end;
    if Cmd = 'CLOSE' then
    begin
      Cmd_CLOSE(AThread,Par);
      exit;
    end;
    if Cmd = 'EXPUNGE' then
    begin
      Cmd_EXPUNGE(AThread,Par);
      exit;
    end;
    if Cmd = 'SEARCH' then
    begin
      Cmd_SEARCH(AThread,Par);
      exit;
    end;
    if Cmd = 'FETCH' then
    begin
      Cmd_FETCH(AThread,Par);
      exit;
    end;
    if Cmd = 'STORE' then
    begin
      Cmd_STORE(AThread,Par);
      exit;
    end;
    if Cmd = 'COPY' then
    begin
      Cmd_COPY(AThread,Par);
      exit;
    end;
    if Cmd = 'UID' then
    begin
      Cmd_UID(AThread,Par);
      exit;
    end;

    // unknown (sub-) command
    SendResTag(AThread,'BAD Command not implemented.');
    with BaseApplication as IBaseApplication do
      Warning(Format('Unsupported IMAP-command: %s', [nCmdLine]));

  except
    on E: Exception do
    begin
      with BaseApplication as IBaseApplication do
        Error('HandleCommand.ErrorCommand:' + LogCmdLine);
    end;
  end;
end;


//-------------------------------------------------Kommandos--------

procedure TSImapServer.Cmd_HELP(AThread: TSTcpThread; Par: string);
var
  s: string;
begin
  if Par = '' then
  begin
    SendRes(AThread,'Implemented Commands follows');
    s :=
      'APPEND msgs fgs dt' + CRLF + 'AUTHENTICATE methode' + CRLF + //JW //IMAP-Auth
      'CAPABILITY' + CRLF + 'CHECK' + CRLF + 'CLOSE' + CRLF +
      'COPY msgs mbx' + CRLF + 'CREATE mbx' + CRLF + 'DELETE mbx' +
      CRLF + 'EXAMINE mbx' + CRLF + 'EXPUNGE' + CRLF + 'FETCH msgs fdat' +
      CRLF + 'HELP [topic]' + CRLF + //!!
      'IDLE  ..  DONE' + CRLF + //HSR //IDLE
      'LIST ref mbx' + CRLF + 'LOGIN usr pasw' + CRLF + 'LOGOUT' +
      CRLF + 'LSUB ref mbx' + CRLF + 'NOOP' + CRLF + 'RENAME mbx mbx' +
      CRLF + 'SEARCH' + CRLF + //Parameter?
      'SELECT mbx' + CRLF + 'STARTTLS' + CRLF + 'STATUS mbx' +
      CRLF + 'STORE msgs sdat val' + CRLF + '    flags[.silent]' +
      CRLF + '    +flags[.silent]' + CRLF + '    -flags[.silent]' +
      CRLF + 'SUBSCRIBE mbx' + CRLF + 'UID cmd params' + CRLF +
      '    COPY' + CRLF + '    FETCH' + CRLF + '    SEARCH' +
      CRLF + '    STORE' + CRLF + 'UNSUBSCRIBE mbx';
    if UseIMAPID then
      s := s + CRLF + 'ID params';
    SendResLit(AThread,s);

  end
  else
  begin
    par := uppercase(par);
    if par = 'APPEND' then
      SendResLit(AThread,
        'Parameter:  mailbox name' + CRLF + '            OPTIONAL flag list' +
        CRLF + '            OPTIONAL date/time string' + CRLF +
        '            message literal' + CRLF + '' + CRLF +
        'Adds a new message with flags and datetime set' + CRLF +
        'into the given mailbox. The message itself is' + CRLF + 'given literal.'
        );
    if par = 'AUTHENTICATE' then
      SendResLit(AThread,
        'SASL secur authentifications.' + CRLF + ''
        );
    if par = 'CAPABILITY' then
      SendResLit(AThread,
        'Capability gives back some Flags/Params' + CRLF +
        'to handle "new" commands.' + CRLF +
        'Try to get a list of all available extensions.'
        );
    if par = 'CHECK' then
      SendResLit(AThread,
        'CHECK returns a mailbox status update.'
        );
    if par = 'CLOSE' then
      SendResLit(AThread,
        'Deselects (closes) the current selected mailbox.' + CRLF +
        '''EXPUNGE'' is called automatically if the' + CRLF + 'mailbox is read-write'
        );
    if par = 'COPY' then
      SendResLit(AThread,
        ''
        );
    if par = 'CREATE' then
      SendResLit(AThread,
        'Parameter: mailbox' + CRLF +
        'Creates a new mailbox. Hierachy-delimiter: ''/'''
        );
    if par = 'DELETE' then
      SendResLit(AThread,''
        );
    if par = 'EXAMINE' then
      SendResLit(AThread,
        'Parameter: mailbox' + CRLF +
        'Selects another mailbox read-only.' + CRLF + '' +
        CRLF + 'If that mailbox doesn''t exists, this command only deselect' +
        CRLF + 'selected mailbox. Hierachy-delimiter: ''/'''
        );
    if par = 'EXPUNGE' then
      SendResLit(AThread,
        'Deletes messages flagged with "/delete".' + CRLF +
        'Use it for getting a ''clean'' mailbox.'
        );
    if par = 'FETCH' then
      SendResLit(AThread,''
        );
    if par = 'HELP' then
      SendResLit(AThread,
        'This command <g>. It''s NOT defined by RFC!' + CRLF +
        'If I''m ready it could be deleted. Also it could be changed' +
        CRLF + 'and so on. Use it only with(for) telnet-experiments.' +
        CRLF + '' + CRLF +
        'Parameter could be the command to which you want to get help' +
        CRLF + '' + CRLF +
        'BTW: I couldn''t use "XHELP" ''cause nobody would find it.'
        );
    {JW}{IMAP ID}
    if (par = 'ID') and UseIMAPID then
      SendResLit(AThread,
        'Parameter: Property of client or "nil" token. Ignored.' +
        CRLF + 'Gives out an ID of the server. It MUST NOT be used to ' +
        CRLF + 'change clients behavior!'
        );
    {/JW}
    if par = 'IDLE' then //HSR //IDLE
      SendResLit(AThread,'IDLE' + CRLF + 'Wait til client sends DONE<CRLF>.' +
        CRLF + 'While the server is in this state it sends' + CRLF +
        'EXISTS and RECENT without request' + CRLF +
        'Client MUST NOT send any commands in this state'
        );

    if par = 'LIST' then
      SendResLit(AThread,''
        );
    if par = 'LOGIN' then
      SendResLit(AThread,
        'Normal authentifications. Need as parameters USER *and* PASS.' +
        CRLF + '''Cause there is no preauth and AUTHENTICATE-command in this' +
        CRLF + 'implementation, you have to use this command.'
        );
    if par = 'LOGOUT' then
      SendResLit(AThread,
        'Goes out of mailboxes and terminates the connection!' +
        CRLF + 'Use it like "QUIT" in POP3/SMTP/NNTP'
        );
    if par = 'LSUB' then
      SendResLit(AThread,''
        );
    if par = 'NOOP' then
      SendResLit(AThread,
        'NOOP returns a mailbox status update.' + CRLF +
        'You can use the NOOP command to reset the ' + CRLF +
        'inactivity autologout timer on the server.'
        );
    if par = 'RENAME' then
      SendResLit(AThread,''
        );
    if par = 'SEARCH' then
      SendResLit(AThread,
        'Parameter: OPTIONAL [CHARSET] specification' + CRLF +
        '           searching criteria (one or more)' + CRLF + '' +
        CRLF + 'The SEARCH command searches the mailbox for' +
        CRLF + 'messages that match the given searching criteria.' +
        CRLF + '' + CRLF + 'The defined search keys are as follows:' +
        CRLF + '  <message set>     Message numbers' + CRLF +
        '  ALL               All messages' + CRLF +
        '  Answered          Flag "/Answered"' + CRLF +
        '  BCC <string>      BCC in envelope-structure' + CRLF +
        '  Before <date>     Older messages then <date>' + CRLF +
        '  BODY <string>     Is <string> in body?' + CRLF +
        '  CC <string>       CC in envelope-structure' + CRLF +
        '  Deleted           Flag "/Deleted"' + CRLF +
        '  Draft             Flag "/Draft"' + CRLF +
        '  Flagged           Flag "/Flagged"' + CRLF +
        '  FROM <string>     FROM in envelope-structure' + CRLF +
        '  Header <field> <string>' + CRLF +
        '                    Contains header <field> <string>?' + CRLF +
        '  Keyword <flag>    Flag <flag>' + CRLF +
        '  Larger <n>        Larger than <n> octets?' + CRLF +
        '  New               Flag "/Recent" and not flag "/Seen"' + CRLF +
        '  NOT <search-key>  Don''t match <search-key>' + CRLF +
        '  Old               Not flag "/Recent"' + CRLF +
        '  On <date>         Internal date = <date>?' + CRLF +
        '  OR <s-k1> <s-k2>  Matches <search-key1> OR <search-key2>' +
        CRLF + '  Recent            Flag "/Recent"' + CRLF +
        '  Seen              Flag "/Seen"' + CRLF +
        '  Sentbefore <date> Is header "date" <  <date>?' + CRLF +
        '  Senton <date>     Is header "date" =  <date>?' + CRLF +
        '  Sentsince <date>  Is header "date" => <date>?' + CRLF +
        '  Since <date>      Internal date => <date?' + CRLF +
        '  Smaller <n>       Smaller than <n> octets?' + CRLF +
        '  SUBJECT <string>  SUBJECT in envelope-structure' + CRLF +
        '  Text <string>     Contains <string> in header or body?' + CRLF +
        '  TO <string>       TO in envelope-structure' + CRLF +
        '  UID <message-set> Corresponding to the UID-Set?' + CRLF +
        '  Unanswered        Not flag "/Answered"' + CRLF +
        '  Undeleted         Not flag "/Deleted"' + CRLF +
        '  Undraft           Not flag "/Draft"' + CRLF +
        '  Unflagged         Not flag "/Flagged"' + CRLF +
        '  Unkeyword <flag>  Not flag <flag>' + CRLF +
        '  Unseen            Not flag "/Seen"'
        );
    if par = 'SELECT' then
      SendResLit(AThread,
        'Parameter: mailbox' + CRLF + 'Select another mailbox read-write.' +
        CRLF + 'If that mailbox doesn''t exists, this command only deselect' +
        CRLF + 'selected mailbox. Hierachy-delimiter: ''/'''
        );
    if par = 'STARTTLS' then
      SendResLit(AThread,''
        );
    if par = 'STATUS' then
      SendResLit(AThread,
        'Parameter:  mailbox name' + CRLF +
        '            status data item names' + CRLF + '' +
        CRLF + 'Gets the status you want of the specified mailbox' +
        CRLF + '  MESSAGES       Number of all messages' + CRLF +
        '  RECENT         Number of messages with "\Recent" flag' +
        CRLF + '  UIDNEXT        The UID that will be assigned to a new message' +
        CRLF + '  UIDVALIDITY    The unique identifier validity value' +
        CRLF + '  UNSEEN         Number of messages without "\Seen" flag'
        );
    if par = 'STORE' then
      SendResLit(AThread,
        'Arguments:  message set' + CRLF +
        '            message data item name' + CRLF +
        '            value for message data item' + CRLF +
        '' + CRLF + 'The STORE command alters data associated with a message '
        + CRLF + 'in the mailbox.' + CRLF + '' +
        CRLF + 'valid message data item names:' + CRLF +
        '     (FLAGS|+FLAGS|-FLAGS)[.SILENT]' + CRLF +
        'valid values for message data item names:' + CRLF +
        '     (\Deleted \Flagged \Seen \Answered \Marked)' + CRLF
        );
    if par = 'SUBSCRIBE' then
      SendResLit(AThread,''
        );
    if par = 'UID' then
      SendResLit(AThread,'');
    if par = 'UNSUBSCRIBE' then
      SendResLit(AThread,'');
    s := '|APPEND|AUTHENTICATE|CAPABILITY|CHECK|' +
      'CLOSE|COPY|CREATE|DELETE|EXAMINE|EXPUNGE|' +
      'FETCH|HELP|IDLE|LIST|LOGIN|LOGOUT|LSUB|NOOP|' +
      'RENAME|SEARCH|SELECT|STARTTLS|STATUS|' + 'STORE|SUBSCRIBE|UID|UNSUBSCRIBE|';
    if UseIMAPID then //JW //IMAP ID
      s := s + 'ID|';
    if not (pos(('|' + par + '|'), s) > 0) then
      SendResTag(AThread,'NO I couldn''t find this command in my help-database!')
    else
      SendResTag(AThread,'OK You''ve the help you want, haven''t you?');
  end;
end;

function CutFirstParam( var Parameters: String ): String;
var  Str: String;
begin
     GetString( Parameters, Str, IMAP_STRING_ASTRING );
     //Parameters := TrimWhSpace( Parameters );
     //Literals können sonst kaputt gehen!
     Parameters := LTrim( Parameters );
     Result := TrimWhSpace( Str );
end;

procedure TSImapServer.Cmd_APPEND(AThread: TSTcpThread; Par: string); {MG}{Literal}
var
  Mailbox, TimeStr, MessageText: string;
  i: integer;
  Flags: string;
  Time: TUnixTime;
  DestMailbox: TImapMailbox;
begin
  Mailbox := CutFirstParam(Par);
  if (Mailbox = '') or (Par = '') then
  begin
    with BaseApplication as IBaseApplication do
      Warning('IMAP-server - Missing arguments for APPEND.');
    SendResTag(AThread,'BAD arguments missing for APPEND!');
    exit;
  end;
  if not MBExists(Mailbox) then
  begin
    with BaseApplication as IBaseApplication do
      Warning('IMAP-server: Unknown mailbox for APPEND');
    SendResTag(AThread,'NO [TRYCREATE] APPEND error: mailbox not known');
    exit;
  end;

  i := Pos(')', Par);
  if (i > 0) and (i < Pos('{', Par)) then
  begin
    Flags := trim(Copy(Par, Pos('(', Par) + 1, i - Pos('(', Par) - 1));
    Par := LTrim(copy(Par, i + 1, length(Par) - i)); // NHB
  end
  else
  begin
    Flags := '';
  end;

  if Copy(Par, 1, 1) = '"' then
    TimeStr := CutFirstParam(Par)
  else
    TimeStr := '';
  if TimeStr <> '' then
    Time := DateTimeToUnixTime(ImapDateTimeToDateTime(TimeStr))
  else
    Time := DateTimeToUnixTime(nowGMT);

  if Par = '' then
  begin
    with BaseApplication as IBaseApplication do
      Warning('IMAP-server - Message missing for APPEND.');
    SendResTag(AThread,'NO APPEND without message literal!');
    exit;
  end;
  MessageText := CutFirstParam(Par);
  if MessageText = '' then
  begin
    with BaseApplication as IBaseApplication do
      Warning('IMAP-server - Message missing for APPEND.');
    SendResTag(AThread,'NO APPEND without message literal!');
    exit;
  end;

  if Assigned(Selected) and (Mailbox = Selected.Path) then
  begin
    SendResTag(AThread, Selected.AppendMessage(MessageText, Flags, Time));
  end
  else if not MBLogin(DestMailbox, Mailbox, False) then
    begin
      SendResTag(AThread, 'NO APPEND error: can''t open destination mailbox');
    end
    else
    begin
      try
        SendResTag(AThread, DestMailbox.AppendMessage(MessageText, Flags, Time));
      finally
        MBLogout(DestMailbox, False)
      end;
    end;
end;

procedure TSImapServer.Cmd_AUTHENTICATE(AThread: TSTcpThread; Par: string); //JW //IMAP-Auth
var
  realm, nonce, cnonce, qop, username, nc, realm2, digesturi, response,
  a1, a2, rspauth: string;
  s, TimeStamp, Hash, pass: string;
begin
  CurrentUserName := '';
  try
    par := uppercase(par);
    if (par = 'LOGIN') then
    begin
      s := 'Username:';
      s := '+ ' + EncodeStringBase64(s);
      s := SendRequest(AThread,s);
      if s = '' then
      begin
        //LogRaw(LOGID_DETAIL, 'Auth LOGIN protocol error');
        SendResTag(AThread,'NO Authentification failed!');
        Exit;
      end;
      if s = '*' then
      begin
        //LogRaw(LOGID_DETAIL, 'Auth LOGIN cancelled by client');
        SendResTag(AThread,'BAD Authentification failed!');
        Exit;
      end;
      s := DecodeStringBase64(s);
      //LogRaw(LOGID_INFO, '> ' + s);
      CurrentUserName := TrimWhSpace(s);
      s := 'Password:';
      s := '+ ' + EncodeStringBase64(s);
      s := SendRequest(AThread,s);
      if s = '' then
      begin
        //LogRaw(LOGID_DETAIL, 'Auth LOGIN protocol error');
        SendResTag(AThread,'NO Authentification failed!');
        Exit;
      end;
      if s = '*' then
      begin
        //LogRaw(LOGID_DETAIL, 'Auth LOGIN cancelled by client');
        SendResTag(AThread,'BAD Authentification failed!');
        Exit;
      end;
      s := DecodeStringBase64(s);
      CurrentUserName := '';
      //SendResTag(AThread,LoginUser(s, 'LOGIN'));
    end
    else
      if (par = 'PLAIN') {and Assigned(SSL)} then
      begin
        TimeStamp := MidGenerator(Def_FQDNforMIDs);
        s := '+ ' + EncodeB64(TimeStamp[1], length(TimeStamp));
        s := SendRequest(s);
        if s = '' then
        begin
          //LogRaw(LOGID_DETAIL, 'Auth LOGIN protocoll error');
          SendResTag('NO Authentification failed!');
          Exit;
        end;
        if s = '*' then
        begin
          //LogRaw(LOGID_DETAIL, 'Auth LOGIN cancel by client');
          SendResTag('BAD Authentification failed!');
          Exit;
        end;
        s := DecodeStringBase64(s);
        CurrentUserName := TrimWhSpace(copy(s, pos(#0, s) + 1, 500));
        s := TrimWhSpace(copy(CurrentUserName,
          pos(#0, CurrentUserName) + 1, 500));
        CurrentUserName := TrimWhSpace(
          copy(CurrentUserName, 1, pos(#0, CurrentUserName) - 1));
        CurrentUserID := ACTID_INVALID;
        SendResTag(LoginUser(s, 'PLAIN'));
      end
      else

        if Par = 'DIGEST-MD5' then
        begin
          // build challenge
          if Def_FQDN <> '' then
            realm := Def_FQDN
          else
            realm := 'localhost';
          s := 'realm="' + realm + '"';
          nonce := HMAC_SHA1(nonce, IntToHex(PRNG(MaxInt), 8) +
            IntToHex(PRNG(MaxInt), 8) + IntToHex(PRNG(MaxInt), 8) +
            IntToHex(PRNG(MaxInt), 8));
          nonce := EncodeB64(nonce[1], length(nonce));
          s := s + ',' + 'nonce="' + nonce + '"';
          qop := 'auth';
          s := s + ',' + 'qop="' + qop + '"';
          s := s + ',' + 'algorithm=' + 'md5-sess';
          LogRaw(LOGID_DEBUG, 'DIGEST-MD5 challenge: ' + s);
          s := EncodeB64(s[1], length(s));
          // send challenge, get response
          s := SendRequest('+ ' + s);
          if s = '' then
          begin
            Log(LOGID_Error, 'Server.EmptyResponeReceived', 'empty response received');
            SendResTag('BAD Authentification failed!');
            exit;
          end;
          // check response, extract values
          s := DecodeB64(s[1], length(s));
          LogRaw(LOGID_DEBUG, 'DIGEST-MD5 response: ' + s);
          // check username
          username := ExtractQuotedParameter(s, 'username');
          if username = '' then
          begin
            Log(LOGID_Error, 'Server.MissingUsernameInAnswer',
              'missing username in answer');
            SendResTag('BAD Authentification failed!');
            exit;
          end;
          CurrentUserName := trim(username);
          CurrentUserID := ACTID_INVALID;
          nonce := ExtractQuotedParameter(s, 'nonce');
          if nonce = '' then
          begin
            Log(LOGID_Error, 'Server.MissingNonceInAnswer', 'missing nonce in answer');
            SendResTag('BAD Authentification failed!');
            exit;
          end;
          cnonce := ExtractQuotedParameter(s, 'cnonce');
          if cnonce = '' then
          begin
            Log(LOGID_Error, 'Server.MissingCnonceInAnswer',
              'missing cnonce in answer');
            SendResTag('BAD Authentification failed!');
            exit;
          end;
          nc := ExtractQuotedParameter(s, 'nc');
          if nc <> '00000001' then
          begin
            Log(LOGID_Error, 'Server.WrongNCValue', 'wrong nc value');
            SendResTag('BAD Authentification failed!');
            exit;
          end;
          qop := ExtractQuotedParameter(s, 'qop');
          if qop = '' then
            qop := 'auth'
          else
            if (lowercase(qop) <> 'auth') then
            begin
              Log(LOGID_Error, 'Server.UnsupportedHashQualityProtection',
                'unsupported hash quality protection ´%s', qop);
              SendResTag('BAD Authentification failed!');
              exit;
            end;
          if pos('realm=', s) = 0 then
          begin
            Log(LOGID_Error, 'Server.MissingRealmInAnswer', 'missing realm in answer');
            SendResTag('BAD Authentification failed!');
            exit;
          end;
          realm2 := ExtractQuotedParameter(s, 'realm');
          if realm2 = '' then
          begin
            Log(LOGID_Error, 'Server.MissingRealmInAnswer', 'missing realm in answer');
            SendResTag('BAD Authentification failed!');
            exit;
          end;
          if realm2 <> realm then
          begin
            Log(LOGID_Error, 'Server.WrongRealmInAnswer', 'wrong realm in answer');
            SendResTag('BAD Authentification failed!');
            exit;
          end;
          digesturi := ExtractQuotedParameter(s, 'digest-uri');
          response := ExtractQuotedParameter(s, 'response');
          if length(response) <> 32 then
          begin
            Log(LOGID_Error, 'Server.WrongResponseLenInAnswer',
              'wrong response length in answer');
            SendResTag('BAD Authentification failed!');
            exit;
          end;
          // build expected response and compare with received one
          try
            CurrentUserID := CfgAccounts.Users.IDOf(CurrentUserName);
            if CurrentUserID = ACTID_INVALID then
            begin
              Log(LOGID_Error, 'Server.Rejected.UnknownUser',
                'login rejected, unknown user');
              CurrentUserName := '';
              SendResTag('BAD Authentification failed!');
              exit;
            end;
            pass := CfgAccounts.Users.Find(CurrentUserID).Password;
            a1 := MD5OfStr(username + ':' + realm + ':' + pass) +
              ':' + nonce + ':' + cnonce;
            A2 := 'AUTHENTICATE:' + digesturi;
            s := MD5toHex(MD5OfStr(MD5toHex(MD5OfStr(A1)) +
              ':' + nonce + ':' + nc + ':' + cnonce + ':' + qop +
              ':' + MD5toHex(MD5OfStr(A2))));
            if s <> response then
            begin
              Log(LOGID_Error, 'Server.Rejected.WrongResponseValue',
                'login rejected, wrong response value');
              SendResTag('BAD Authentification failed!');
              CurrentUserName := '';
              CurrentUserID := ACTID_INVALID;
              exit;
            end;
          except
            Log(LOGID_Error, 'Server.UnknownError', 'unknown error');
            SendResTag('BAD Authentification failed!');
            CurrentUserName := '';
            CurrentUserID := ACTID_INVALID;
            exit;
          end;
          // build rspauth and send it
          a2 := ':' + digesturi;
          rspauth := MD5toHex(MD5OfStr(MD5toHex(MD5OfStr(A1)) +
            ':' + nonce + ':' + nc + ':' + cnonce + ':' + qop + ':' +
            MD5toHex(MD5OfStr(A2))));
          s := 'rspauth=' + rspauth;
          LogRaw(LOGID_DEBUG, 'DIGEST-MD5 rspauth: ' + s);
          s := EncodeB64(s[1], length(s));
          s := SendRequest('+ ' + s);
          SendResTag(LoginUser(pass, 'DIGEST-MD5'));
        end
        else

          if par = 'CRAM-MD5' then
          begin
            TimeStamp := MidGenerator(Def_FQDNforMIDs);
            s := '+ ' + EncodeB64(TimeStamp[1], length(TimeStamp));
            s := SendRequest(s);
            if s = '' then
              exit;
            s := DecodeB64(s[1], length(s));
            if s = '' then
            begin
              LogRaw(LOGID_DETAIL, 'Auth LOGIN protocoll error');
              SendResTag('NO Authentification failed!');
              Exit;
            end;
            if s = '*' then
            begin
              LogRaw(LOGID_DETAIL, 'Auth LOGIN cancel by client');
              SendResTag('BAD Authentification failed!');
              Exit;
            end;
            Hash := TrimWhSpace(copy(s, PosWhSpace(s) + 1, 32));
            CurrentUserName := TrimWhSpace(copy(s, 1, PosWhSpace(s) - 1));
            CurrentUserID := CfgAccounts.Users.IDOf(CurrentUserName);
            if CurrentUserID = ACTID_INVALID then
            begin
              CurrentUserName := '';
            end
            else
            begin
              pass := CfgAccounts.Users.Find(CurrentUserID).Password;
              s := MD5HMAC(pass, TimeStamp);
              s := MD5toHex(s);
              if s = Hash then
                SendResTag(LoginUser(pass, 'CRAM-MD5'))
              else
              begin
                SendResTag('NO Authentification rejected!');
                CurrentUserID := ACTID_INVALID;
                CurrentUserName := '';
              end;
            end;
          end
          else
            if par = 'CRAM-SHA1' then
            begin
              TimeStamp := MidGenerator(Def_FQDNforMIDs);
              s := '+ ' + EncodeB64(TimeStamp[1], length(TimeStamp));
              s := SendRequest(s);
              if s = '' then
                exit;
              s := DecodeB64(s[1], length(s));
              if s = '' then
              begin
                LogRaw(LOGID_DETAIL, 'Auth LOGIN protocoll error');
                SendResTag('NO Authentification failed!');
                Exit;
              end;
              if s = '*' then
              begin
                LogRaw(LOGID_DETAIL, 'Auth LOGIN cancel by client');
                SendResTag('BAD Authentification failed!');
                Exit;
              end;
              Hash := TrimWhSpace(copy(s, PosWhSpace(s) + 1, 40)); // Fix Arne Schloh
              CurrentUserName := TrimWhSpace(copy(s, 1, PosWhSpace(s) - 1));
              CurrentUserID := CfgAccounts.Users.IDOf(CurrentUserName);
              if CurrentUserID = ACTID_INVALID then
              begin
                CurrentUserName := '';
              end
              else
              begin
                pass := CfgAccounts.Users.Find(CurrentUserID).Password;
                s := HMAC_SHA1(pass, TimeStamp);
                s := SHA1toHex(s);
                if s = Hash then
                  SendResTag(LoginUser(pass, 'CRAM-SHA1'))
                else
                begin
                  SendResTag('NO Authentification rejected!');
                  CurrentUserID := ACTID_INVALID;
                  CurrentUserName := '';
                end;
              end;
            end
            else
            begin
              CurrentUserName := '';
              CurrentUserID := ACTID_INVALID;
              SendResTag('NO Unknown AUTH mechanism ' + par);
              Log(LOGID_WARN, 'Server.unknownAUTHmechanism',
                'Unknown AUTH mechanism %s', par);
            end;
  except
    CurrentUserID := ACTID_INVALID;
    CurrentUserName := '';
  end;
  if CurrentUserID = ACTID_INVALID then
    Log(LOGID_WARN, 'IMAPServer.authenticate.failed', 'IMAP: AUTHENTICATE: Failed')
  else
    Log(LOGID_INFO, 'IMAPServer.authenticate.ok',
      'IMAP: AUTHENTICATE: User %s logged in', CurrentUserName);
end;

procedure TSImapServer.Cmd_CAPA(AThread: TSTcpThread; Par: string);
var
  capabilities: string;
begin
  if par <> '' then
  begin
    Log(LOGID_WARN, 'IMAPServer.capa.tooManyArgs', 'IMAP: CAPA: Too many arguments!');
    SendResTag('BAD I don''t know parameters for CAPABILITY!');
    exit;
  end;
  //---Standard-CAPAs--------------------------------
  capabilities := 'IMAP4rev1 ' + 'AUTH=CRAM-SHA1 ' +
    'AUTH=CRAM-MD5 '   //JW //IMAP-Auth
    + 'AUTH=DIGEST-MD5 ' //JW //SASL-DIGEST
    + 'IDLE '            //HSR //IDLE
    + 'LITERAL+ ';        //HSR //Literal+


  //---Bedingte CAPAs--------------------------------
  if Def_IMAPNCBrain then
    capabilities := capabilities + 'X-NETSCAPE ';     //HSR //NCBrain

  if not Def_IMAP_DisableSASLLogin then
    capabilities := capabilities + 'AUTH=LOGIN '; //JW //IMAP-Auth

  {MG}{SSL}
  if not Assigned(SSLConnection) then
  begin
    if Assigned(SSLContext) then
      capabilities := capabilities + 'STARTTLS ';
    if (Def_LocalImapTlsMode = 2) and not Def_IMAP_DisableLogin then
      //HSR //LOGINDISABLED
      capabilities := capabilities + 'LOGINDISABLED ';
  end
  else
    capabilities := capabilities + 'AUTH=PLAIN ';
  {/SSL}

  if Def_IMAP_DisableLogin then //HSR //LOGINDISABLED
    capabilities := capabilities + 'LOGINDISABLED ';

  if Def_IMAP_ID then //JW //IMAP ID
    capabilities := capabilities + 'ID ';

  //---Sending---------------------------------------
  SendRes('CAPABILITY ' + trim(capabilities));

  LogRaw(LOGID_DETAIL, 'IMAP: Sent capabilities: ' + capabilities);
  SendResTag('OK I''m ready sending capabilities!');
end;

procedure TSImapServer.Cmd_CHECK(AThread: TSTcpThread; Par: string);
begin
  if Par <> '' then
  begin
    Log(LOGID_WARN, 'IMAPServer.check.tooManyArgs',
      'IMAP: CHECK: Too many arguments!');
    SendResTag('BAD I don''t know parameters for CHECK!');
  end
  else
  begin
    if Assigned(Selected) then
    begin
      try
        Selected.Lock;
        Selected.SendMailboxUpdate;
      finally
        Selected.Unlock;
      end;
    end;
    LogRaw(LOGID_DETAIL, 'IMAP: CHECK');
    SendResTag('OK CHECK completed.');
  end;
end;

procedure TSImapServer.Cmd_CLOSE(AThread: TSTcpThread; Par: string);
begin
  if Par <> '' then
  begin
    Log(LOGID_WARN, 'IMAPServer.close.tooManyArgs',
      'IMAP: CLOSE: Too many arguments!');
    SendResTag('BAD I don''t know parameters for CLOSE!');
  end
  else
  begin
    try
      if not Selected.MBReadOnly then
      begin //ClientRO
        Log(LOGID_INFO, 'IMAPServer.capa.info',
          'IMAP: Expunge (Close) of %s', Selected.Path);
        Selected.Expunge(SelNotify);
      end
    finally
      MBLogout(Selected, True);
      LogRaw(LOGID_DETAIL, 'IMAP: CLOSE!');
      SendResTag('OK Mailbox closed.');
    end;
  end;
end;

procedure TSImapServer.Cmd_COPY(AThread: TSTcpThread; Par: string);
var
  MsgSetStr, Destination: string;
begin
  MsgSetStr := CutFirstParam(Par);
  Destination := CutFirstParam(Par);
  if (MsgSetStr = '') or (Destination = '') then
  begin
    SendResTag('BAD COPY without message set / mailbox!');
    Log(LOGID_WARN, 'IMAPServer.copy.missingArgs',
      'IMAP: COPY: Missing arguments!');
  end
  else
  begin
    Log(LOGID_INFO, 'IMAPServer.copy.info', 'IMAP: COPY %s to %s',
      [MsgSetStr, Destination]);
    DoCopy(Selected.StrToMsgSet(MsgSetStr, False), 'COPY', Destination);
  end;
end;

procedure TSImapServer.Cmd_CREATE(AThread: TSTcpThread; Par: string);
var
  Mailbox: string;
begin
  Mailbox := CutFirstParam(Par);
  if Mailbox = '' then
  begin
    SendResTag('BAD CREATE without mailbox!');
    Log(LOGID_WARN, 'IMAPServer.create.nomailbox',
      'IMAP: CREATE: No mailbox given');
  end
  else if MBCreate(Mailbox) then
    begin
      Log(LOGID_INFO, 'IMAPServer.create.info', 'IMAP: CREATE: Mailbox "%s" created',
        Mailbox);
      SendResTag('OK Mailbox created!');
    end
    else
    begin
      Log(LOGID_WARN, 'IMAPServer.create.failed',
        'IMAP: CREATE: Mailbox "%s" not created', Mailbox);
      SendResTag('NO Mailbox not created!');
    end;
end;

procedure TSImapServer.Cmd_DELETE(AThread: TSTcpThread; Par: string);
var
  Mailbox: string;
begin
  Mailbox := CutFirstParam(Par);
  if Mailbox = '' then
  begin
    Log(LOGID_WARN, 'IMAPServer.delete.missingmailbox',
      'IMAP: DELETE: Missing mailbox');
    SendResTag('BAD DELETE without mailbox!');
  end
  else if MBDelete(Mailbox) then
    begin
      Log(LOGID_INFO, 'IMAPServer.delete.info', 'IMAP: DELETE: Mailbox "%s" deleted',
        Mailbox);
      SendResTag('OK Mailbox deleted!');
    end
    else
    begin
      Log(LOGID_WARN, 'IMAPServer.delete.failed',
        'IMAP: DELETE: Mailbox "%s" not deleted', Mailbox);
      SendResTag('NO Mailbox not deleted!');
    end;
end;

procedure TSImapServer.Cmd_EXAMINE(AThread: TSTcpThread; Par: string);
var
  Mailbox: string;
begin
  Mailbox := CutFirstParam(Par);
  if Mailbox = '' then
  begin
    Log(LOGID_WARN, 'IMAPServer.examine.missingmailbox',
      'IMAP: EXAMINE: Missing mailbox');
    SendResTag('BAD EXAMINE without mailbox!');
  end
  else
  begin
    if MBSelect(Mailbox, True) then
    begin
      SendRes('OK [PERMANENTFLAGS ()] No permanent flags permitted');
      SendResTag('OK [READ-ONLY] Mailbox opened');
      Log(LOGID_INFO, 'IMAPServer.examine.info',
        'IMAP: EXAMINE: Mailbox "%s" opened Read-Only', Mailbox);
    end
    else
    begin
      MBLogout(Selected, True);
      SendResTag('NO EXAMINE failed!');
      Log(LOGID_WARN, 'IMAPServer.examine.failed',
        'IMAP: EXAMINE: Failed to open mailbox "%s" Read-Only', Mailbox);
    end;
  end;
end;

procedure TSImapServer.Cmd_EXPUNGE(AThread: TSTcpThread; Par: string);
begin
  if Par <> '' then
  begin
    Log(LOGID_WARN, 'IMAPServer.expunge.toomanyargs',
      'IMAP: EXPUNGE: Too many arguments');
    SendResTag('BAD I don''t know parameters for EXPUNGE!');
  end
  else if Selected.MBReadOnly then
    begin
      Log(LOGID_WARN, 'IMAPServer.expunge.mailboxreadonly',
        'IMAP: EXPUNGE: Mailbox Read-Only');
      SendResTag('NO I can''t EXPUNGE (mailbox is read-only).');
    end
    else
    begin
      Selected.Expunge(nil);
      Log(LOGID_INFO, 'IMAPServer.expunge.info',
        'IMAP: EXPUNGE: All marked messages are deleted');
      SendResTag('OK All deleted messages are removed.');
    end;
end;

procedure TSImapServer.Cmd_FETCH(AThread: TSTcpThread; Par: string);
var
  MsgSetStr: string;
begin
  MsgSetStr := CutFirstParam(Par);
  if (MsgSetStr = '') or (Par = '') then
  begin
    Log(LOGID_WARN, 'IMAPServer.fetch.missingargs',
      'IMAP: FETCH: Missing arguments');
    SendResTag('BAD FETCH without message set / data!');
  end
  else
  begin
    LogRaw(LOGID_DETAIL, 'IMAP: FETCH');
    DoFetch(Selected.StrToMsgSet(MsgSetStr, False), 'FETCH', Par);
  end;
end;

{JW}{IMAP ID}
procedure TSImapServer.Cmd_ID(AThread: TSTcpThread; Par: string);
var
  id: string;
begin
  if par = '' then
  begin
    LogRaw(LOGID_Detail, 'IMAP Client ID: missing argument!');
    SendResTag('BAD I''m missing parameters for ID!');
    exit;
  end;
  ID := '("name" "Hamster" ' + '"version" "' + GetMyBuildInfo + '" ' +
    '"os" "windows" ' + '"os-version" "' + GetWinVerInfo + '" ' +
    '"support-url" "news:hamster.de.misc")';

  SendRes('ID ' + ID);
  LogRaw(LOGID_DETAIL, 'IMAP: Sent ID: ' + ID);
  SendResTag('OK ID completed!');
end;

{/JW}

procedure TSImapServer.Cmd_IDLE(AThread: TSTcpThread; Par: string); //HSR //IDLE
begin
  if Par <> '' then
  begin
    Log(LOGID_WARN, 'IMAPServer.idle.toomanyargs', 'IMAP: IDLE: Too many arguments!');
    SendResTag('BAD I don''t know parameters for IDLE!');
  end
  else
  begin
    if CurrentUserID <> ACTID_INVALID then
    begin
      LogRaw(LOGID_DETAIL, 'IMAP: IDLE begin');
      SendData('+ You are IDLE now, changes will be sent immidiatelly' + CRLF);
      IdleState := True;
    end
    else
    begin
      LogRaw(LOGID_DETAIL, 'IMAP: IDLE didn''t begin');
      SendResTag('NO Not authenticated.');
    end;
  end;
end;


{MG}{IMAP-List}
procedure TSImapServer.Cmd_LIST(AThread: TSTcpThread; Par: string);
begin
  LogRaw(LOGID_DETAIL, 'IMAP: LIST');
  DoList(Par, False);
end;

{/IMAP-List}

procedure TSImapServer.Cmd_LOGIN(AThread: TSTcpThread; Par: string);
var
  Pass: string;
begin
  CurrentUserID := ACTID_INVALID;
  CurrentUsername := '';
  {MG}{SSL}
  // disable the LOGIN command which uses clear-text passwords
  // unless encryption is active for security reasons
  if (Def_LocalImapTlsMode = 2) and not Assigned(SSLConnection) then
  begin
    SendResTag('BAD TLS connection required for LOGIN - try STARTTLS');
    Log(LOGID_WARN, 'IMAPServer.login.tlsneeded',
      'IMAP: LOGIN: TLS needed for this command by config');
    exit;
  end;
  {/SSL}

  if (Def_IMAP_DisableLogin) then
  begin
    SendResTag('BAD LOGIN is switched off by server-admin. Please use AUTHENTICATE.');
    Log(LOGID_WARN, 'IMAPServer.login.disabled',
      'IMAP: LOGIN: Disabled by config');
    exit;
  end;

  CurrentUsername := CutFirstParam(Par);
  Pass := CutFirstParam(Par);
  if (CurrentUserName = '') or (Pass = '') then
  begin
    SendResTag('BAD LOGIN without User / Pass!');
    Log(LOGID_WARN, 'IMAPServer.login.missinguserpass',
      'IMAP: LOGIN: Missing User/Pass');
  end
  else
  begin
    SendResTag(LoginUser(Pass, ''));
    Log(LOGID_INFO, 'IMAPServer.login.info', 'IMAP: LOGIN: User %s logged in.',
      CurrentUserName);
  end;
end;

procedure TSImapServer.Cmd_LOGOUT(AThread: TSTcpThread; Par: string);
begin
  try
    LogRaw(LOGID_INFO, 'IMAP: LOGOUT');
    CurrentUserID := ACTID_INVALID;
    try
      if Assigned(Selected) then
        MBLogout(Selected, True);
    except
    end;

    if CurrentUserID >= 0 then
      CurrentUserID := ACTID_INVALID;

    LogRaw(LOGID_DETAIL, 'IMAP: Let the client disconnect-->disconnect');
    if ClientSocket.Connected then
      SendRes('BYE IMAP4rev1 closing connection - goodbye!');
    if ClientSocket.Connected then
      SendResTag('OK Closing.');

    Sleep(Def_LocalTimeoutQuitDelay);
    try
      if ClientSocket.Connected then
        ClientSocket.Close;
    except
      on E: Exception do
        LogRaw(LOGID_DEBUG, 'Exception on Socket.Close: ' + E.Message);
    end;
  finally
    Terminate
  end;
end;

{MG}{IMAP-List}
procedure TSImapServer.Cmd_LSUB(AThread: TSTcpThread; Par: string);
begin
  LogRaw(LOGID_DETAIL, 'IMAP: LSUB');
  DoList(Par, True);
end;

{/IMAP-List}

{HSR}{NCBRain}
procedure TSImapServer.Cmd_NCBrain(AThread: TSTcpThread; Par: string);
const
  NCBURL = 'http://www.rimarts.co.jp';
begin //Got out of Cyrus-Source
  SendRes('OK [NETSCAPE]');
  SendRes('* VERSION 1.0 UNIX');
  SendRes('* ACCOUNT-URL "' + NCBURL + '"');
  SendResTag('OK Your brain is done now...');
end;

{/HSR}

procedure TSImapServer.Cmd_NOOP(AThread: TSTcpThread; Par: string);
begin
  if par <> '' then
  begin
    Log(LOGID_WARN, 'IMAPServer.noop.toomanyargs', 'IMAP: NOOP: Too many arguments');
    SendResTag('BAD I don''t know parameters for NOOP!');
  end
  else
  begin
    if Assigned(Selected) then
    begin
      try
        Selected.Lock;
        Selected.SendMailboxUpdate;
      finally
        Selected.Unlock;
      end;
    end;
    LogRaw(LOGID_DETAIL, 'IMAP: NOOP');
    SendResTag('OK Noop isn''t slow, is it? ;-)');
  end;
end;

procedure TSImapServer.Cmd_RENAME(AThread: TSTcpThread; Par: string);
var
  OldName, NewName: string;
begin
  OldName := CutFirstParam(Par);
  NewName := CutFirstParam(Par);

  if (OldName = '') or (NewName = '') then
  begin
    SendResTag('BAD RENAME without existing / new name!');
    Log(LOGID_WARN, 'IMAPServer.rename.missingargs',
      'IMAP: RENAME: Missing arguments');
  end
  else if MBRename(OldName, NewName) then
    begin
      Log(LOGID_INFO, 'IMAPServer.rename.info', 'IMAP: RENAME: %s to %s',
        [OldName, NewName]);
      SendResTag('OK Mailbox renamed.');
    end
    else
    begin
      Log(LOGID_WARN, 'IMAPServer.rename.failed', 'IMAP: RENAME failed');
      SendResTag('NO Mailbox not renamed!');
    end;
end;

procedure TSImapServer.Cmd_SEARCH(AThread: TSTcpThread; Par: string);
begin
  if par = '' then
  begin
    Log(LOGID_WARN, 'IMAPServer.search.missingargs',
      'IMAP: SEARCH: Missing arguments');
    SendResTag('BAD SEARCH without arguments!');
  end
  else
  begin
    LogRaw(LOGID_DETAIL, 'IMAP: SEARCH');
    DoSearch(False, Par);
  end;
end;

procedure TSImapServer.Cmd_SELECT(AThread: TSTcpThread; Par: string);
var
  Mailbox: string;
begin
  if Assigned(Selected) then
    MBLogout(Selected, True); //RFC!

  Mailbox := CutFirstParam(Par);
  if Mailbox = '' then
  begin
    Log(LOGID_WARN, 'IMAPServer.select.missingargs',
      'IMAP: SELECT: Missing arguments');
    SendResTag('BAD SELECT without mailbox!');
  end
  else
  begin
    if MBSelect(Mailbox, fReadOnly) then
    begin
      if Selected.MBReadOnly then
      begin //ClientRO
        SendRes('OK [PERMANENTFLAGS ()] Flags you can change permanently: NONE');
        SendResTag('OK [READ-ONLY] Mailbox opened');
        Log(LOGID_INFO, 'IMAPServer.select.openedreadonly',
          'IMAP: SELECT: %s opened (Read-Only)', Mailbox);
      end
      else
      begin
        SendRes('OK [PERMANENTFLAGS ' + Selected.PossFlags +
          '] Flags you can change permanently');
        SendResTag('OK [READ-WRITE] Mailbox opened');
        Log(LOGID_INFO, 'IMAPServer.select.opened',
          'IMAP: SELECT: %s opened (Read-Write)', Mailbox);
      end;
    end
    else
    begin
      MBLogout(Selected, True);
      SendResTag('NO SELECT failed!');
      Log(LOGID_WARN, 'IMAPServer.select.failed',
        'IMAP: SELECT failed at %s', mailbox);
    end;
  end;
end;

procedure TSImapServer.Cmd_STARTTLS(AThread: TSTcpThread; Par: string); {MG}{SSL}
begin
  if not SSLReady then
  begin
    SendResTag('BAD Command not implemented.');
    Log(LOGID_WARN, 'IMAPServer.starttls.disabled',
      'IMAP: STARTTLS not available by config');
  end
  else
    if Par <> '' then
    begin
      SendResTag('BAD I don''t know parameters for STARTTLS!');
      Log(LOGID_WARN, 'IMAPServer.starttls.toomanyargs',
        'IMAP: STARTTLS: Too many arguments');
    end
    else
      if SSLContext = nil then
      begin
        SendResTag('BAD TLS not available due to temporary error');
        Log(LOGID_ERROR, 'IMAPServer.starttls.wrongsslcontext',
          'IMAP: STARTTLS: SSLContext is wrong');
      end
      else
        if SSLConnection = nil then
        begin
          Log(LOGID_INFO, 'IMAPServer.starttls.beginning',
            'IMAP: STARTTLS: Beginning transaction with SSL');
          SendResTag('OK Begin TLS negotiation');
          if StartSSL then
          begin
            if CurrentUserID <> ACTID_INVALID then
              CurrentUserID := ACTID_INVALID;
            CurrentUserName := '';
          end;
        end
        else
        begin
          Log(LOGID_WARN, 'IMAPServer.starttls.wasstartedbefore',
            'IMAP: STARTTLS: TLS was already started');
          SendResTag('BAD Command not permitted when TLS active');
        end;
end;

procedure TSImapServer.Cmd_STATUS(AThread: TSTcpThread; Par: string);
var
  Mailbox, MbName, Status, StatusU, Erg: string;
  MbxStatus: TMbxStatus;
  i: integer;
begin
  MailBox := CutFirstParam(Par);
  if (Mailbox = '') or (Par = '') then
  begin
    Log(LOGID_WARN, 'IMAPServer.status.missingargs',
      'IMAP: STATUS: Missing arguments');
    SendResTag('BAD STATUS without mailbox / status-data!');
    exit;
  end;
  MbName := Mailbox;

  if not SafeString(Mailbox) then
  begin
    Log(LOGID_WARN, 'IMAPServer.status.forbiddenchars',
      'IMAP: STATUS: Forbidden characters');
    SendResTag('BAD STATUS Mailbox parameter contains forbidden characters!');
    exit;
  end;
  if not MBExists(Mailbox) then
  begin
    Log(LOGID_WARN, 'IMAPServer.status.invalidmailbox',
      'IMAP: STATUS: Invalid Mailbox "%s"', Mailbox);
    SendResTag('NO STATUS error: mailbox does not exist!');
    exit;
  end;

  FillChar(MbxStatus, SizeOf(MbxStatus), 0);
  if CfgAccounts.IMAPMailboxLock(Mailbox, True) then
  begin
    with TImapMailbox.Create(Mailbox) do
      try
        Lock;
        MbxStatus := Status
      finally
        Unlock;
        Free
      end;
  end
  else
  begin
    with TImapMailbox(CfgAccounts.GetIMAPMailbox(Mailbox)) do
      try
        MbxStatus := Status
      except
      end;
  end;

  Status := TrimParentheses(TrimQuotes(Par)) + ' ';
  Erg := '';
  i := PosWhSpace(Status);
  repeat
    StatusU := uppercase(copy(Status, 1, i - 1));
    Status := copy(Status, i + 1, length(Status));

    if StatusU = 'MESSAGES' then
      Erg := Erg + ' MESSAGES ' + IntToStr(MbxStatus.Messages);

    if StatusU = 'RECENT' then
      Erg := Erg + ' RECENT ' + IntToStr(MbxStatus.Recent);

    if StatusU = 'UIDNEXT' then
      Erg := Erg + ' UIDNEXT ' + IntToStr(MbxStatus.UIDNext);

    if StatusU = 'UIDVALIDITY' then
      Erg := Erg + ' UIDVALIDITY ' + IntToStr(MbxStatus.UIDValidity);

    if StatusU = 'UNSEEN' then
      Erg := Erg + ' UNSEEN ' + IntToStr(MbxStatus.Unseen);

    i := PosWhSpace(Status);
  until i = 0;

  SendRes('STATUS "' + MbName + '" (' + Trim(Erg) + ')');
  SendResTag('OK You now have the status!');
  LogRaw(LOGID_DETAIL, 'IMAP: STATUS');
end;

procedure TSImapServer.Cmd_STORE(AThread: TSTcpThread; Par: string);
var
  MsgSetStr: string;
begin
  MsgSetStr := CutFirstParam(Par);
  if (MsgSetStr = '') or (Par = '') then
  begin
    Log(LOGID_WARN, 'IMAPServer.store.missingargs',
      'IMAP: STORE: Missing arguments');
    SendResTag('BAD STORE arguments missing!');
  end
  else
  begin
    LogRaw(LOGID_DETAIL, 'IMAP: STORE parameters');
    DoStore(Selected.StrToMsgSet(MsgSetStr, False), 'STORE', Par);
  end;
end;

{IMAP-List}
procedure TSImapServer.Cmd_SUBSCRIBE(AThread: TSTcpThread; Par: string);
var
  Mailbox: string;
  hdlFile: integer;
begin
  Mailbox := uppercase(CutFirstParam(Par));
  if (Mailbox = '') or (not SafeString(Mailbox)) then
  begin
    Log(LOGID_WARN, 'IMAPServer.subscribe.missingorbadargs',
      'IMAP: SUBSCRIBE: Missing or bad arguments');
    SendResTag('BAD SUBSCRIBE without valid mailbox!');
  end
  else
  begin
    if uppercase(Mailbox) = 'INBOX' then
      Mailbox := MailboxPath
    else
      Mailbox := MailboxPath + ReplacePathDelimiters(Mailbox);

    if not DirectoryExists(Mailbox) then
    begin
      Log(LOGID_WARN, 'IMAPServer.subscribe.invalidmailbox',
        'IMAP: SUBSCRIBE: Mailbox "%s" does not exist', Mailbox);
      SendResTag('NO SUBSCRIBE mailbox not available!');
    end
    else
    begin
      hdlFile := FileCreate(Mailbox + IMAPSUBSCR_FILENAME);
      if hdlFile = -1 then
      begin
        Log(LOGID_WARN, 'IMAPServer.subscribe.failed',
          'IMAP: SUBSCRIBE: Mailbox "%s" can''t be subscribed', Mailbox);
        SendResTag('NO SUBSCRIBE mailbox can''t be subscribed!');
      end
      else
      begin
        LogRaw(LOGID_DETAIL, 'IMAP: SUBSCRIBE: ' + MailBox);
        SendResTag('OK Mailbox subscribed');
        FileClose(hdlFile);
      end;
    end;
  end;
end;

{/IMAP-List}

procedure TSImapServer.Cmd_UNSUBSCRIBE(AThread: TSTcpThread; Par: string);
var
  MailBox: string;
begin
  Mailbox := uppercase(CutFirstParam(Par));
  if (Mailbox = '') or (not SafeString(Mailbox)) then
  begin
    Log(LOGID_WARN, 'IMAPServer.unsubscribe.missingorbadargs',
      'IMAP: UNSUBSCRIBE: Missing or bad arguments');
    SendResTag('BAD UNSUBSCRIBE without valid mailbox!');
  end
  else
  begin
    if uppercase(Mailbox) = 'INBOX' then
      Mailbox := MailboxPath
    else
      Mailbox := MailboxPath + ReplacePathDelimiters(Mailbox);
    if not DirectoryExists(Mailbox) then
    begin
      Log(LOGID_INFO, 'IMAPServer.unsubscribe.invalidmailbox',
        'IMAP: UNSUBSCRIBE: %s - Mailbox don''t exists', Mailbox);
      SendResTag('OK Non-Existing mailbox is unsubscribed');
    end
    else if not FileExists2(Mailbox + IMAPSUBSCR_FILENAME) then
      begin
        Log(LOGID_INFO, 'IMAPServer.subscribe.isntsubscribed',
          'IMAP: UNSUBSCRIBE: %s - Not subscribed yet', Mailbox);
        SendResTag('OK unsubscribed mailbox is unsubscribed (again)');
      end
      else if SysUtils.DeleteFile(Mailbox + IMAPSUBSCR_FILENAME) then
        begin
          SendResTag('OK Mailbox unsubscribed');
          LogRaw(LOGID_DETAIL, 'IMAP: UNSUBSCRIBE: ' + MailBox);
        end
        else
        begin
          SendResTag('NO Mailbox can''t be unsubscribed');
          LogRaw(LOGID_DETAIL, 'IMAP: UNSUBSCRIBE ' + Mailbox +
            ' failed: Can''t delete file');
        end;
  end;
end;

procedure TSImapServer.Cmd_UID(AThread: TSTcpThread; Par: string);
var
  i: integer;
  Command, CmdParams: string;
  MsgSet: TMessageSet;
begin
  SetLength(MsgSet, 0);

  i := PosWhSpace(Par);
  if (par = '') or (i = 0) then
  begin
    Log(LOGID_WARN, 'IMAPServer.uid.missingargs', 'IMAP: UID: Missing arguments');
    SendResTag('BAD UID without Command/Cmd-Params!');
    exit;
  end;
  Command := Uppercase(TrimQuotes(copy(Par, 1, i - 1)));
  CmdParams := TrimQuotes(copy(Par, i + 1, length(Par)));

  LogRaw(LOGID_DETAIL, 'IMAP: UID-' + Command);

  if Command = 'SEARCH' then
  begin
    DoSearch(True, CmdParams);
  end
  else
  begin
    i := PosWhSpace(CmdParams);
    if i = 0 then
    begin
      Log(LOGID_WARN, 'IMAPServer.uid-command.missingargs',
        'IMAP: UID-%s: Missing arguments', Command);
      SendResTag('BAD UID ' + Command + ': not enough arguments!');
      exit;
    end;

    MsgSet := Selected.StrToMsgSet(copy(CmdParams, 1, i - 1), True);
    CmdParams := TrimQuotes(TrimWhSpace(
      copy(CmdParams, i + 1, length(CmdParams))));

    if Command = 'COPY' then
      DoCopy(MsgSet, 'UID COPY', CmdParams)
    else if Command = 'STORE' then
        DoStore(MsgSet, 'UID STORE', CmdParams)
      else if Command = 'FETCH' then
          DoFetch(MsgSet, 'UID FETCH', CmdParams)
        else
        begin
          Log(LOGID_WARN, 'IMAPServer.uid-command.unknown',
            'IMAP: UID-%s unknown', Command);
          SendResTag('BAD I don''t know this UID-command!');
        end;
  end;
end;

procedure TSImapServer.LogRaw(AThread: TSTcpThread; Txt: string);
begin
  if Assigned(OnLog) then
    OnLog(AThread, False, Txt);
end;

procedure TSImapServer.SendData(AThread: TSTcpThread; const AText: string);
begin
  AThread.WriteLn(AText);
  if Assigned(OnLog) then
    OnLog(AThread, False, AText);
end;

procedure TSImapServer.SendResLit(AThread: TSTcpThread; Txt: string);
begin
  if Length(Txt) > 250 then
    LogRaw(AThread, '< * ' + Copy(Txt, 1, 250) + ' [...]')
  else
    LogRaw(AThread, '< * ' + Txt);
  SendData(AThread, CurrentTag + ' ' + '{' + IntToStr(length(Txt + CRLF)) + '}');
  SendData(AThread, Txt);
end;

procedure TSImapServer.SendResTag(AThread: TSTcpThread; Txt: string);
begin
  if Length(Txt) > 250 then
    LogRaw(AThread, '< * ' + Copy(Txt, 1, 250) + ' [...]')
  else
    LogRaw(AThread, '< * ' + Txt);
  SendData(AThread, '* ' + Txt);
end;

procedure TSImapServer.SendRes(AThread: TSTcpThread; Txt: string);
var
  i: integer;
begin
  if Assigned(Selected) then
    try
      Selected.Lock;
      try
        EnterCriticalSection(CS_THR_IDLE);
        if (length(SendExpunge) > 0) then
        begin
          for i := 0 to length(SendExpunge) - 1 do
            SendRes(AThread, IntToStr(SendExpunge[i]) + ' EXPUNGE');
          if not SendNewMessages then //Don't send it double
            SendRes(AThread, IntToStr(Selected.Messages) + ' EXISTS');
          SetLength(SendExpunge, 0);
        end;
        if SendNewMessages then
        begin
          SendRes(AThread, IntToStr(Selected.Messages) + ' EXISTS');
          SendRes(AThread, IntToStr(Selected.Recent) + ' RECENT');
          SendNewMessages := False;
        end;
      finally
        LeaveCriticalSection(CS_THR_IDLE)
      end;
    finally
      Selected.Unlock
    end;
  SendData(AThread, CurrentTag + ' ' + Txt + CRLF);
end;

function TSImapServer.SendRequest(AThread: TSTcpThread; const AText: string
  ): string;
begin
  AThread.WriteLn(AText);
  if Assigned(OnLog) then
    OnLog(AThread, False, AText);
  Result := AThread.ReadLn(Timeout);
end;

function TSImapServer.MBSelect(Mailbox: string; ReadOnly: Boolean): boolean;
begin

end;

function TSImapServer.MBCreate(Mailbox: string): boolean;
begin

end;

function TSImapServer.MBDelete(Mailbox: string): boolean;
begin

end;

function TSImapServer.MBExists(var Mailbox: string): boolean;
begin

end;

function TSImapServer.MBRename(OldName, NewName: String): Boolean;
begin

end;

function TSImapServer.MBLogin(var Mailbox: TImapMailbox; Path: String;
  LINotify: Boolean): Boolean;
begin

end;

procedure TSImapServer.MBLogout(var Mailbox: TImapMailbox; LOSel: Boolean);
begin

end;

procedure TSImapServer.SetActive(const AValue: boolean);
begin
  inherited SetActive(AValue);
end;

procedure TSImapServer.Execute(AThread: TSTcpThread);
var
  LRow, LCmd, LTag: string;
begin
  try
    SendData(AThread, '* OK IMAP4rev1 ' + SIMAPWelcomeMessage);
    while (not AThread.Terminated) do
    begin
      LRow := AThread.ReadLn(Timeout);
      if (not AThread.Connected) then
        Break
      else if LRow <> '' then
        begin
          if Assigned(OnLog) then
            OnLog(AThread, True, LRow);

        end;
    end;
  finally
  end;
end;

constructor TSImapServer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FIMAPDelay:=0;
  FUseIMAPID:=True;
  FIMAPNCBrain :=False;
  CurrentUserName:='';
end;

destructor TSImapServer.Destroy;
begin
  inherited Destroy;
end;

end.
