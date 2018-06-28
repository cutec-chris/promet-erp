unit usimapserver;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, usbaseserver, mimemess, mimepart, blcksock,
  usimapmailbox, usimapsearch,synsock;

type
  TSImapThread = class(TSTcpThread)
    procedure SImapThreadException(Sender: TObject);
  private
    FServer: TSTcpServer;
  public
    CurrentUserName: String;
    CurrentTag: string;
    TmpData : AnsiString;
    LiteralLength : LongInt;
    SendExpunge: array of integer;
    SendNewMessages: boolean;
    Selected: TImapMailbox;
    constructor Create(AServer : TSTcpServer; ASocket: TSocket);
    destructor Destroy; override;
  end;

  { TSImapServer }

  TSImapServer = class(TSTcpServer)
  private

    FIMAPDelay: Integer;
    FIMAPNCBrain: Boolean;
    FLocalTimeoutQuitDelay: Integer;
    fReadOnly: Boolean;
    FUseIMAPID: Boolean;
    CS_THR_IDLE: TRTLCriticalSection;
    IdleState: boolean;
    SelNotify : pIMAPNotification;
    FDisableLog : Integer;
    bThread : TSTcpThread;
    bCommand : string;
    procedure DoHandleCommand;

    function LoginUser(AThread: TSTcpThread;  Password: String; AuthMechanism : String ): String;

    function  HandleData(AThread : TSTcpThread;BufInStrm : string): String;
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
    procedure Cmd_ID(AThread: TSTcpThread;Par: string);
    procedure Cmd_IDLE(AThread: TSTcpThread;Par: string);
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
    HierarchyDelimiter : Char;

    procedure LogRaw(AThread: TSTcpThread; Txt: string);
    procedure SendData(AThread: TSTcpThread; const AText: string);
    procedure SendResult(AThread: TSTcpThread; const ATxt: string);
    procedure SendResLit(AThread: TSTcpThread; Txt: string);
    procedure SendResTag(AThread: TSTcpThread; Txt: string);
    procedure SendRes(AThread: TSTcpThread; Txt: string);
    function SendRequest(AThread: TSTcpThread; const AText: string) : string;
    function SafeString( Path: String ): Boolean;
    procedure SocketRelease(ASocket : TSTcpThread);virtual;

    procedure SetActive(const AValue: boolean); override;
    procedure Execute(AThread: TSTcpThread); override;
  public
    function  MBSelect(AThread: TSTcpThread; Mailbox: string; ReadOnly : Boolean ): boolean; virtual;
    function  MBGet(AThread: TSTcpThread; Mailbox: string): TImapMailbox; virtual;
    function  MBCreate(AThread: TSTcpThread; Mailbox: string ): boolean; virtual;
    function  MBDelete(AThread: TSTcpThread; Mailbox: string ): boolean; virtual;
    function  MBExists(AThread: TSTcpThread; var Mailbox: string ): boolean; virtual;
    function  MBRename(AThread: TSTcpThread; OldName, NewName: String ): Boolean; virtual;
    function  MBLogin(AThread: TSTcpThread;  var Mailbox: TImapMailbox; Path: String; LINotify : Boolean ): Boolean; virtual;
    procedure MBLogout(AThread: TSTcpThread; var Mailbox: TImapMailbox; LOSel : Boolean ); virtual;
    procedure DoSearch(AThread: TSTcpThread; UseUID: Boolean; Par: String ); virtual;abstract;
    procedure DoCopy(AThread: TSTcpThread; MsgSet: TMessageSet; Command, Destination: String );virtual;abstract;
    procedure DoStore(AThread: TSTcpThread; MsgSet: TMessageSet; Command, Par: String );virtual;abstract;
    procedure DoFetch(AThread: TSTcpThread; MsgSet: TMessageSet; Command, Par: String );virtual;abstract;
    procedure DoList(AThread: TSTcpThread; Par: String; LSub: Boolean ); virtual;abstract;
    procedure DoSubscribe(AThread: TSTcpThread; Par: String);virtual;
    procedure DoUnSubscribe(AThread: TSTcpThread; Par: String);virtual;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DisableLog;
    procedure EnableLog;
    property IMAPDelay : Integer read FIMAPDelay write FIMAPDelay;
    property UseIMAPID : Boolean read FUseIMAPID write FUseIMAPID;
    property IMAPNCBrain : Boolean read FIMAPNCBrain write FIMAPNCBrain;
    property LocalTimeoutQuitDelay : Integer read FLocalTimeoutQuitDelay write FLocalTimeoutQuitDelay;
    property ReadOnly : Boolean read fReadOnly write fReadOnly;
  published
  end;

  function TrimParentheses( Data: String ): String;
  function CutFirstParam( var Parameters: String ): String;

implementation

uses uBaseApplication,base64,Utils;

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

procedure TSImapThread.SImapThreadException(Sender: TObject);
begin
  raise Exception.Create('Exception in Thread');
  Destroy;
end;

constructor TSImapThread.Create(AServer: TSTcpServer; ASocket: TSocket);
begin
  inherited Create(ASocket);
  FServer := AServer;
  LiteralLength:=0;
  CurrentUserName:='';
  SendNewMessages := false;
  SetLength(SendExpunge, 0);
  OnException:=@SImapThreadException;
end;

destructor TSImapThread.Destroy;
begin
  TSImapServer(FServer).SocketRelease(Self);
  inherited Destroy;
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
    end;
    if (Cmd = 'NETSCAPE') and FIMAPNCBrain then
    begin
      Cmd_NCBrain(AThread,Par);
      exit;
    end;

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
    if TSImapThread(AThread).CurrentUserName = '' then
    begin
      if Cmd = 'STARTTLS' then
      begin
        Cmd_STARTTLS(AThread,Par);
        exit;
      end; {MG}{SSL}
    end;
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

    // check authentication
    if TSImapThread(AThread).CurrentUserName = '' then
    begin
      if Assigned(BaseApplication) then with BaseApplication as IBaseApplication do
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

    if not Assigned(TSImapThread(AThread).Selected) then
    begin
      if Assigned(BaseApplication) then with BaseApplication as IBaseApplication do
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
    if Assigned(BaseApplication) then with BaseApplication as IBaseApplication do
      Warning(Format('Unsupported IMAP-command: %s', [nCmdLine]));

  except
    on E: Exception do
    begin
      if Assigned(BaseApplication) then with BaseApplication as IBaseApplication do
        Error('HandleCommand.ErrorCommand:' + LogCmdLine);
    end;
  end;
end;
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
    if Assigned(BaseApplication) then with BaseApplication as IBaseApplication do
      Warning('IMAP-server - Missing arguments for APPEND.');
    SendResTag(AThread,'BAD arguments missing for APPEND!');
    exit;
  end;
  if not MBExists(AThread,Mailbox) then
  begin
    if Assigned(BaseApplication) then with BaseApplication as IBaseApplication do
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
    if Assigned(BaseApplication) then with BaseApplication as IBaseApplication do
      Warning('IMAP-server - Message missing for APPEND.');
    SendResTag(AThread,'NO APPEND without message literal!');
    exit;
  end;
  MessageText := CutFirstParam(Par);
  if MessageText = '' then
  begin
    if Assigned(BaseApplication) then with BaseApplication as IBaseApplication do
      Warning('IMAP-server - Message missing for APPEND.');
    SendResTag(AThread,'NO APPEND without message text!');
    exit;
  end;
  if Assigned(TSImapThread(AThread).Selected) and (Mailbox = TSImapThread(AThread).Selected.Path) then
  begin
    SendResTag(AThread, TSImapThread(AThread).Selected.AppendMessage(AThread,MessageText, Flags, Time));
  end
  else if not MBLogin(AThread,DestMailbox, Mailbox, False) then
    begin
      SendResTag(AThread, 'NO APPEND error: can''t open destination mailbox');
    end
    else
    begin
      try
        SendResTag(AThread, DestMailbox.AppendMessage(AThread,MessageText, Flags, Time));
      finally
        MBLogout(AThread,DestMailbox, False)
      end;
    end;
end;
function ExtractQuotedParameter(container,parameter:string):string;
begin
  result:='';
  // parameter in container not present
  if pos(parameter,container)=0 then exit;
  parameter:=trim(parameter);
  container:=trim(container);
  repeat
    // check the first parameter in container
    if copy(container+'=',1,length(parameter)+1)=parameter+'=' then begin
      result:=copy(container,length(parameter)+2,length(container));
      if result[1]='"' then begin   // check if dequoting required
        result:=copy(result,2,length(result)-1) ;
        result:=copy(result,1,pos('"',result)-1);
      end else
        // if not end of container cut the parameter
        if pos(',',result)<>0 then
          result:=copy(result,1,pos(',',result)-1);
      exit;
    end else
    // check if exist a next parameter
    if (pos('"',container)>0) and
        (pos(',',container)>pos('"',container)) then begin
      // remove current qoutet parameter
      container:=copy(container,pos('"',container)+1,length(container));
      container:=copy(container,pos('"',container)+1,length(container));
      if pos(',',container)>0 then // remove seprtor ","
        container:=copy(container,pos(',',container)+1,length(container));
    end else
    // remove currend unquotet parameter
    if pos(',',container)=0 then
      container:=''
    else
      container:=copy(container,pos(',',container)+1,length(container));
  until  container=''
end;
procedure TSImapServer.Cmd_AUTHENTICATE(AThread: TSTcpThread; Par: string); //JW //IMAP-Auth
var
  realm, nonce, cnonce, qop, username, nc, realm2, digesturi, response,
  a1, a2, rspauth: string;
  s, TimeStamp, Hash, pass: string;
begin
  TSImapThread(AThread).CurrentUserName := '';
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
      TSImapThread(AThread).CurrentUserName := TrimWhSpace(s);
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
      TSImapThread(AThread).CurrentUserName := '';
      SendResTag(AThread,LoginUser(AThread,s, 'LOGIN'));
    end
    else
      if (par = 'PLAIN') {and Assigned(SSL)} then
      begin
        TimeStamp := TimeToStr(Now());
        s := '+ ' + EncodeStringBase64(TimeStamp);
        s := SendRequest(AThread,s);
        if s = '' then
        begin
          //LogRaw(LOGID_DETAIL, 'Auth LOGIN protocoll error');
          SendResTag(AThread,'NO Authentification failed!');
          Exit;
        end;
        if s = '*' then
        begin
          //LogRaw(LOGID_DETAIL, 'Auth LOGIN cancel by client');
          SendResTag(AThread,'BAD Authentification failed!');
          Exit;
        end;
        s := DecodeStringBase64(s);
        TSImapThread(AThread).CurrentUserName := TrimWhSpace(copy(s, pos(#0, s) + 1, 500));
        s := TrimWhSpace(copy(TSImapThread(AThread).CurrentUserName,
          pos(#0, TSImapThread(AThread).CurrentUserName) + 1, 500));
        TSImapThread(AThread).CurrentUserName := TrimWhSpace(
          copy(TSImapThread(AThread).CurrentUserName, 1, pos(#0, TSImapThread(AThread).CurrentUserName) - 1));
        SendResTag(AThread,LoginUser(AThread,s, 'PLAIN'));
      end
      else
      begin
        TSImapThread(AThread).CurrentUserName := '';
        SendResTag(AThread,'NO Unknown AUTH mechanism ' + par);
      end;
  except
    TSImapThread(AThread).CurrentUserName := '';
  end;
  {
  if CurrentUserName = '' then
    Log(LOGID_WARN, 'IMAPServer.authenticate.failed', 'IMAP: AUTHENTICATE: Failed')
  else
    Log(LOGID_INFO, 'IMAPServer.authenticate.ok',
      'IMAP: AUTHENTICATE: User %s logged in', CurrentUserName);
  }
end;
procedure TSImapServer.Cmd_CAPA(AThread: TSTcpThread; Par: string);
var
  capabilities: string;
begin
  if par <> '' then
  begin
    SendResTag(AThread,'BAD I don''t know parameters for CAPABILITY!');
    exit;
  end;
  //---Standard-CAPAs--------------------------------
  capabilities := 'IMAP4rev1 '
  //  + 'IDLE '
  //  + 'LITERAL+ '
  ;


  //---Bedingte CAPAs--------------------------------
  //if  Def_IMAPNCBrain then
  //  capabilities := capabilities + 'X-NETSCAPE ';     //HSR //NCBrain

  capabilities := capabilities + 'AUTH=LOGIN '; //JW //IMAP-Auth

//  if not Assigned(SSLConnection) then
//    begin
//      if Assigned(SSLContext) then
//        capabilities := capabilities + 'STARTTLS ';
//      if (Def_LocalImapTlsMode = 2) and not Def_IMAP_DisableLogin then
//        capabilities := capabilities + 'LOGINDISABLED ';
//    end
//  else
    capabilities := capabilities + 'AUTH=PLAIN ';

  //if Def_IMAP_DisableLogin then //HSR //LOGINDISABLED
  //  capabilities := capabilities + 'LOGINDISABLED ';
  if  UseIMAPID then
    capabilities := capabilities + 'ID ';

  //---Sending---------------------------------------
  SendRes(AThread,'CAPABILITY ' + trim(capabilities));

  SendResTag(AThread,'OK I''m ready sending capabilities!');
end;
procedure TSImapServer.Cmd_CHECK(AThread: TSTcpThread; Par: string);
begin
  if Par <> '' then
  begin
    SendResTag(AThread,'BAD I don''t know parameters for CHECK!');
  end
  else
  begin
    if Assigned(TSImapThread(AThread).Selected) then
    begin
      try
        TSImapThread(AThread).Selected.Lock;
        TSImapThread(AThread).Selected.SendMailboxUpdate;
      finally
        TSImapThread(AThread).Selected.Unlock;
      end;
    end;
    SendResTag(Athread,'OK CHECK completed.');
  end;
end;
procedure TSImapServer.Cmd_CLOSE(AThread: TSTcpThread; Par: string);
begin
  if Par <> '' then
  begin
    SendResTag(AThread,'BAD I don''t know parameters for CLOSE!');
  end
  else
  begin
    try
      if not TSImapThread(AThread).Selected.MBReadOnly then
      begin //ClientRO
        TSImapThread(AThread).Selected.Expunge(SelNotify);
      end
    finally
      MBLogout(AThread,TSImapThread(AThread).Selected, True);
      SendResTag(AThread,'OK Mailbox closed.');
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
    SendResTag(AThread,'BAD COPY without message set / mailbox!');
  end
  else
  begin
    DoCopy(AThread,TSImapThread(AThread).Selected.StrToMsgSet(MsgSetStr, False), 'COPY', Destination);
  end;
end;
procedure TSImapServer.Cmd_CREATE(AThread: TSTcpThread; Par: string);
var
  Mailbox: string;
begin
  Mailbox := CutFirstParam(Par);
  if Mailbox = '' then
  begin
    SendResTag(AThread,'BAD CREATE without mailbox!');
  end
  else if MBCreate(AThread,Mailbox) then
    begin
      SendResTag(AThread,'OK Mailbox created!');
    end
    else
    begin
      SendResTag(AThread,'NO Mailbox not created!');
    end;
end;
procedure TSImapServer.Cmd_DELETE(AThread: TSTcpThread; Par: string);
var
  Mailbox: string;
begin
  Mailbox := CutFirstParam(Par);
  if Mailbox = '' then
  begin
    SendResTag(AThread,'BAD DELETE without mailbox!');
  end
  else if MBDelete(AThread,Mailbox) then
    begin
      SendResTag(AThread,'OK Mailbox deleted!');
    end
    else
    begin
      SendResTag(AThread,'NO Mailbox not deleted!');
    end;
end;
procedure TSImapServer.Cmd_EXAMINE(AThread: TSTcpThread; Par: string);
var
  Mailbox: string;
begin
  Mailbox := CutFirstParam(Par);
  if Mailbox = '' then
  begin
    SendResTag(AThread,'BAD EXAMINE without mailbox!');
  end
  else
  begin
    if MBSelect(AThread,Mailbox, True) then
    begin
      SendRes(AThread,'OK [PERMANENTFLAGS ()] No permanent flags permitted');
      SendResTag(AThread,'OK [READ-ONLY] Mailbox opened');
    end
    else
    begin
      MBLogout(AThread,TSImapThread(AThread).Selected, True);
      SendResTag(AThread,'NO EXAMINE failed!');
    end;
  end;
end;
procedure TSImapServer.Cmd_EXPUNGE(AThread: TSTcpThread; Par: string);
begin
  if Par <> '' then
  begin
    SendResTag(AThread,'BAD I don''t know parameters for EXPUNGE!');
  end
  else if TSImapThread(AThread).Selected.MBReadOnly then
    begin
      SendResTag(AThread,'NO I can''t EXPUNGE (mailbox is read-only).');
    end
    else
    begin
      TSImapThread(AThread).Selected.Expunge(nil);
      SendResTag(AThread,'OK All deleted messages are removed.');
    end;
end;
procedure TSImapServer.Cmd_FETCH(AThread: TSTcpThread; Par: string);
var
  MsgSetStr: string;
begin
  MsgSetStr := CutFirstParam(Par);
  if (MsgSetStr = '') or (Par = '') then
  begin
    SendResTag(AThread,'BAD FETCH without message set / data!');
  end
  else
  begin
    DoFetch(AThread,TSImapThread(AThread).Selected.StrToMsgSet(MsgSetStr, False), 'FETCH', Par);
  end;
end;
procedure TSImapServer.Cmd_ID(AThread: TSTcpThread; Par: string);
var
  id: string;
begin
  if par = '' then
  begin
    SendResTag(AThread,'BAD I''m missing parameters for ID!');
    exit;
  end;
  ID := '("name" "'+ApplicationName+'" ' + '"version" "' + '0.0.0' + '" ' +
    '"os" "hidden" ' + '"os-version" "none" ' +
    '"support-url" "none")';

  SendRes(AThread,'ID ' + ID);
  SendResTag(AThread,'OK ID completed!');
end;
procedure TSImapServer.Cmd_IDLE(AThread: TSTcpThread; Par: string); //HSR //IDLE
begin
  if Par <> '' then
  begin
    SendResTag(AThread,'BAD I don''t know parameters for IDLE!');
  end
  else
  begin
    if TSImapThread(AThread).CurrentUserName <> '' then
    begin
      SendData(AThread,'+ You are IDLE now, changes will be sent immidiatelly' + CRLF);
      IdleState := True;
    end
    else
    begin
      SendResTag(AThread,'NO Not authenticated.');
    end;
  end;
end;
procedure TSImapServer.Cmd_LIST(AThread: TSTcpThread; Par: string);
begin
  DoList(AThread,Par, False);
end;
procedure TSImapServer.Cmd_LOGIN(AThread: TSTcpThread; Par: string);
var
  Pass: string;
begin
  TSImapThread(AThread).CurrentUsername := '';
  TSImapThread(AThread).CurrentUsername := CutFirstParam(Par);
  Pass := CutFirstParam(Par);
  if (TSImapThread(AThread).CurrentUserName = '') or (Pass = '') then
    SendResTag(AThread,'BAD LOGIN without User / Pass!')
  else
  begin
    SendResTag(AThread,LoginUser(AThread,Pass, ''));
  end;
end;
procedure TSImapServer.Cmd_LOGOUT(AThread: TSTcpThread; Par: string);
begin
  try
    TSImapThread(AThread).CurrentUserName := '';
    try
      if Assigned(TSImapThread(AThread).Selected) then
        MBLogout(AThread,TSImapThread(AThread).Selected, True);
    except
    end;

    if  AThread.Connected then
      SendRes(AThread,'BYE IMAP4rev1 closing connection - goodbye!');
    if AThread.Connected then
      SendResTag(AThread,'OK Closing.');

    Sleep(LocalTimeoutQuitDelay);
    try
      if AThread.Connected then
        AThread.Disconnect;
    except
    end;
  finally
  end;
end;
procedure TSImapServer.Cmd_LSUB(AThread: TSTcpThread; Par: string);
begin
  DoList(AThread,Par, True);
end;
procedure TSImapServer.Cmd_NCBrain(AThread: TSTcpThread; Par: string);
const
  NCBURL = 'http://www.rimarts.co.jp';
begin //Got out of Cyrus-Source
  SendRes(AThread,'OK [NETSCAPE]');
  SendRes(AThread,'* VERSION 1.0 UNIX');
  SendRes(AThread,'* ACCOUNT-URL "' + NCBURL + '"');
  SendResTag(AThread,'OK Your brain is done now...');
end;
procedure TSImapServer.Cmd_NOOP(AThread: TSTcpThread; Par: string);
begin
  if par <> '' then
  begin
    SendResTag(AThread,'BAD I dont know parameters for NOOP!');
  end
  else
  begin
    if Assigned(TSImapThread(AThread).Selected) then
    begin
      try
        TSImapThread(AThread).Selected.Lock;
        TSImapThread(AThread).Selected.SendMailboxUpdate;
      finally
        TSImapThread(AThread).Selected.Unlock;
      end;
    end;
    SendResTag(AThread,'OK Noop');
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
    SendResTag(AThread,'BAD RENAME without existing / new name!');
  end
  else if MBRename(AThread,OldName, NewName) then
    begin
      SendResTag(AThread,'OK Mailbox renamed.');
    end
    else
    begin
      SendResTag(AThread,'NO Mailbox not renamed!');
    end;
end;
procedure TSImapServer.Cmd_SEARCH(AThread: TSTcpThread; Par: string);
begin
  if par = '' then
  begin
    SendResTag(AThread,'BAD SEARCH without arguments!');
  end
  else
  begin
    DoSearch(AThread,False, Par);
  end;
end;
procedure TSImapServer.Cmd_SELECT(AThread: TSTcpThread; Par: string);
var
  Mailbox: string;
begin
  if Assigned(TSImapThread(AThread).Selected) then
    MBLogout(AThread,TSImapThread(AThread).Selected, True); //RFC!

  Mailbox := CutFirstParam(Par);
  if Mailbox = '' then
  begin
    SendResTag(AThread,'BAD SELECT without mailbox!');
  end
  else
  begin
    if MBSelect(AThread,Mailbox, fReadOnly) then
    begin
      if TSImapThread(AThread).Selected.MBReadOnly then
      begin //ClientRO
        SendRes(AThread,'OK [PERMANENTFLAGS ()] Flags you can change permanently: NONE');
        SendResTag(AThread,'OK [READ-ONLY] Mailbox opened');
      end
      else
      begin
        SendRes(AThread,'OK [PERMANENTFLAGS ' + TSImapThread(AThread).Selected.PossFlags + '] Flags you can change permanently');
        SendResTag(AThread,'OK [READ-WRITE] Mailbox opened');
      end;
    end
    else
    begin
      MBLogout(AThread,TSImapThread(AThread).Selected, True);
      SendResTag(AThread,'NO SELECT failed!');
    end;
  end;
end;
function TrimEnclosingChars( Data, FirstChar, LastChar: String ): String;
var  i: Integer;
begin
  Data := TrimWhSpace( Data );
  i := length( Data );
  if (i>1) and (Data[1] = FirstChar) and (Data[i] = LastChar) then
    Result := copy( Data, 2, i-2 )
  else
    Result := Data
end;
function TrimParentheses( Data: String ): String;
begin
  Result := TrimEnclosingChars( Data, '(', ')' )
end;
procedure TSImapServer.Cmd_STARTTLS(AThread: TSTcpThread; Par: string); {MG}{SSL}
begin
  {
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
 }
end;
procedure TSImapServer.Cmd_STATUS(AThread: TSTcpThread; Par: string);
var
  Mailbox, MbName, Status, StatusU, Erg: string;
  i: integer;
begin
  MailBox := CutFirstParam(Par);
  if (Mailbox = '') or (Par = '') then
  begin
    SendResTag(AThread,'BAD STATUS without mailbox / status-data!');
    exit;
  end;
  MbName := Mailbox;

  if not SafeString(Mailbox) then
  begin
    SendResTag(AThread,'BAD STATUS Mailbox parameter contains forbidden characters!');
    exit;
  end;
  if not MBExists(AThread,Mailbox) then
  begin
    SendResTag(AThread,'NO STATUS error: mailbox does not exist!');
    exit;
  end;

  with MBGet(AThread,Mailbox) do
  try
    Lock;
    Status := TrimParentheses(TrimQuotes(Par)) + ' ';
    Erg := '';
    i := PosWhSpace(Status);
    repeat
      StatusU := uppercase(copy(Status, 1, i - 1));
      Status := copy(Status, i + 1, length(Status));

      if StatusU = 'MESSAGES' then
        Erg := Erg + ' MESSAGES ' + IntToStr(Messages);

      if StatusU = 'RECENT' then
        Erg := Erg + ' RECENT ' + IntToStr(Recent);

      if StatusU = 'UIDNEXT' then
        Erg := Erg + ' UIDNEXT ' + IntToStr(GetUIDnext);

      if StatusU = 'UIDVALIDITY' then
        Erg := Erg + ' UIDVALIDITY ' + IntToStr(GetUIDvalidity);

      if StatusU = 'UNSEEN' then
        Erg := Erg + ' UNSEEN ' + IntToStr(Unseen);

      i := PosWhSpace(Status);
    until i = 0;
  finally
    Unlock;
    Free
  end;

  SendRes(AThread,'STATUS "' + MbName + '" (' + Trim(Erg) + ')');
  SendResTag(AThread,'OK You now have the status!');
end;
procedure TSImapServer.Cmd_STORE(AThread: TSTcpThread; Par: string);
var
  MsgSetStr: string;
begin
  MsgSetStr := CutFirstParam(Par);
  if (MsgSetStr = '') or (Par = '') then
  begin
    SendResTag(AThread,'BAD STORE arguments missing!');
  end
  else
  begin
    DoStore(AThread,TSImapThread(AThread).Selected.StrToMsgSet(MsgSetStr, False), 'STORE', Par);
  end;
end;
procedure TSImapServer.Cmd_SUBSCRIBE(AThread: TSTcpThread; Par: string);
var
  Mailbox: string;
  hdlFile: integer;
begin
  Mailbox := uppercase(CutFirstParam(Par));
  if (Mailbox = '') or (not SafeString(Mailbox)) then
  begin
    SendResTag(AThread,'BAD SUBSCRIBE without valid mailbox!');
  end
  else
  begin
    DoSubscribe(AThread,MailBox);
  end;
end;
procedure TSImapServer.Cmd_UNSUBSCRIBE(AThread: TSTcpThread; Par: string);
var
  MailBox: string;
begin
  Mailbox := uppercase(CutFirstParam(Par));
  if (Mailbox = '') or (not SafeString(Mailbox)) then
  begin
    SendResTag(AThread,'BAD UNSUBSCRIBE without valid mailbox!');
  end
  else
  begin
    DoUnSubscribe(AThread,MailBox);
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
    SendResTag(AThread,'BAD UID without Command/Cmd-Params!');
    exit;
  end;
  Command := Uppercase(TrimQuotes(copy(Par, 1, i - 1)));
  CmdParams := TrimQuotes(copy(Par, i + 1, length(Par)));

  if Command = 'SEARCH' then
  begin
    DoSearch(AThread,True, CmdParams);
  end
  else
  begin
    i := PosWhSpace(CmdParams);
    if i = 0 then
    begin
      SendResTag(AThread,'BAD UID ' + Command + ': not enough arguments!');
      exit;
    end;

    if Uppercase(copy(CmdParams, 1, i - 1))='1:*' then
      begin
        SetLength(MsgSet,1);
        MsgSet[0] := -1;
      end
    else
      MsgSet := TSImapThread(AThread).Selected.StrToMsgSet(copy(CmdParams, 1, i - 1), True);
    CmdParams := TrimQuotes(TrimWhSpace(
      copy(CmdParams, i + 1, length(CmdParams))));

    if Command = 'COPY' then
      DoCopy(AThread,MsgSet, 'UID COPY', CmdParams)
    else if Command = 'STORE' then
      DoStore(AThread,MsgSet, 'UID STORE', CmdParams)
    else if Command = 'FETCH' then
      DoFetch(AThread,MsgSet, 'UID FETCH', CmdParams)
    else
      begin
        SendResTag(AThread,'BAD I don''t know this UID-command!');
      end;
  end;
end;
procedure TSImapServer.LogRaw(AThread: TSTcpThread; Txt: string);
begin
  if FDisableLog > 0 then exit;
  if pos('LOGIN ',uppercase(Txt))>0 then
    Txt := copy(Txt,0,pos('LOGIN ',uppercase(Txt))-1);
  if Assigned(OnLog) then
    OnLog(AThread, False, Txt);
end;
procedure TSImapServer.SendData(AThread: TSTcpThread; const AText: string);
begin
  if AThread.Connected then
    AThread.Write(AText);
end;
procedure TSImapServer.SendResult(AThread: TSTcpThread; const ATxt: string);
begin
  if Length( ATxt ) > 250
     then LogRaw( AThread, Copy( ATxt, 1, 250 ) + ' [...]' )
     else LogRaw( AThread, ATxt );
  SendData(AThread, ATxt + CRLF );
end;
procedure TSImapServer.SendResLit(AThread: TSTcpThread; Txt: string);
begin
  if Length( Txt ) > 250
     then LogRaw( AThread, '< * ' + Copy( Txt, 1, 250 ) + ' [...]' )
     else LogRaw( AThread, '< * ' + Txt );
  SendData(AThread, TSImapThread(AThread).CurrentTag + ' ' + '{' + IntToStr(length(Txt + CRLF)) + '}' + CRLF );
  SendData(AThread, Txt + CRLF );
end;
procedure TSImapServer.SendResTag(AThread: TSTcpThread; Txt: string);
var
  i: Integer;
begin
  if Assigned(TSImapThread(AThread).Selected) then try
    TSImapThread(AThread).Selected.Lock;
    try
     // EnterCriticalSection( CS_THR_IDLE );
      if (length(TSImapThread(AThread).SendExpunge)>0) then begin
        for i := 0 to length(TSImapThread(AThread).SendExpunge)-1 do
          SendRes(AThread, IntToStr(TSImapThread(AThread).SendExpunge[i]) + ' EXPUNGE');
        if not TSImapThread(AThread).SendNewMessages then //Don't send it double
          SendRes(AThread, IntToStr(TSImapThread(AThread).Selected.Messages) + ' EXISTS');
        SetLength(TSImapThread(AThread).SendExpunge, 0)
      end;
      if TSImapThread(AThread).SendNewMessages then begin
        SendRes(AThread, IntToStr(TSImapThread(AThread).Selected.Messages) + ' EXISTS');
        SendRes(AThread, IntToStr(TSImapThread(AThread).Selected.Recent)   + ' RECENT');
        TSImapThread(AThread).SendNewMessages := false
      end;
    finally
      //LeaveCriticalSection( CS_THR_IDLE )
    end;
  finally
    TSImapThread(AThread).Selected.Unlock
  end;
  SendResult(AThread, TSImapThread(AThread).CurrentTag + ' ' + Txt )
end;
procedure TSImapServer.SendRes(AThread: TSTcpThread; Txt: string);
begin
  if Length( Txt ) > 250
     then LogRaw( AThread, '* ' + Copy( Txt, 1, 250 ) + ' [...]' )
     else LogRaw( AThread, '* ' + Txt );
  SendData(AThread, '* ' + Txt + CRLF );
end;
function TSImapServer.SendRequest(AThread: TSTcpThread; const AText: string
  ): string;
begin
  AThread.WriteLn(AText);
  if Assigned(OnLog) then
    OnLog(AThread, False, AText);
  Result := AThread.ReadLn(Timeout+100);
end;
function TSImapServer.SafeString(Path: String): Boolean;
var  i : Integer;
     SafeChars: Set of Char;
begin
   Result := False;
   SafeChars := [' ', '!', '#'..'.', '0'..'9', ';', '=',
                 '@'..'[', ']'..'{', '}', '~', HierarchyDelimiter];
   for i := 1 to Length(Path) do
      if not (Path[i] in SafeChars) then exit;
   Result := True;
end;

procedure TSImapServer.SocketRelease(ASocket: TSTcpThread);
begin
end;

procedure TSImapServer.DoHandleCommand;
begin
  HandleCommand(bThread, bCommand);
end;

function TSImapServer.LoginUser(AThread: TSTcpThread; Password: String;
  AuthMechanism: String): String;
var
  ares: Boolean = False;
begin

  Result := TSImapThread(AThread).CurrentTag + 'BAD System-error, check logfile. [0]';
  if Assigned(OnLogin) then
    begin
      aRes := OnLogin(AThread, TSImapThread(AThread).CurrentUserName,Password);
      if not ares then Result := 'NO Authentication rejected'
      else
        begin
          if AuthMechanism='' then
            Result := 'OK LOGIN completed.'
          else
            Result := 'OK ' + AuthMechanism + ' completed.';
        end;
    end;
end;
function TSImapServer.MBSelect(AThread: TSTcpThread; Mailbox: string;
  ReadOnly: Boolean): boolean;
begin
  Result := False;
end;
function TSImapServer.MBGet(AThread: TSTcpThread; Mailbox: string
  ): TImapMailbox;
begin
  Result := nil;
end;
function TSImapServer.MBCreate(AThread: TSTcpThread; Mailbox: string): boolean;
begin
  Result := False;
end;
function TSImapServer.MBDelete(AThread: TSTcpThread; Mailbox: string): boolean;
begin
  Result := False;
end;
function TSImapServer.MBExists(AThread: TSTcpThread; var Mailbox: string
  ): boolean;
begin
  Result := False;
end;
function TSImapServer.MBRename(AThread: TSTcpThread; OldName, NewName: String
  ): Boolean;
begin
  Result := False;
end;
function TSImapServer.MBLogin(AThread: TSTcpThread; var Mailbox: TImapMailbox;
  Path: String; LINotify: Boolean): Boolean;
begin
  Result := False;
end;
procedure TSImapServer.MBLogout(AThread: TSTcpThread;
  var Mailbox: TImapMailbox; LOSel: Boolean);
begin
end;
procedure TSImapServer.DoSubscribe(AThread: TSTcpThread; Par: String);
begin

end;
procedure TSImapServer.DoUnSubscribe(AThread: TSTcpThread; Par: String);
begin

end;
function TSImapServer.HandleData(AThread: TSTcpThread; BufInStrm: string
  ): String;
var  i: Integer;
  tmp: String;
begin
   Result := 'BAD Command failed (unknown reason, see logfile)';
   i := Pos( ' ', BufInStrm );

   if (copy(BufInStrm,length(BufInStrm)-2,1)='}') then
     begin
       if (copy(BufInStrm,length(BufInStrm)-2,1)='}') then
         begin
           tmp := copy(BufInStrm,rpos('{',BufInStrm)+1,length(BufInStrm)-2);
           tmp := copy(tmp,0,length(tmp)-3);
           if TryStrToInt(tmp,TSImapThread(AThread).LiteralLength) then
             begin
               TSImapThread(AThread).CurrentTag := Copy( BufInStrm, 1, i-1 );
               System.Delete( BufInStrm, 1, i );
               TSImapThread(AThread).TmpData:=BufInStrm;
               SendData(AThread, '+ Ready to receive '+IntToStr(TSImapThread(AThread).LiteralLength)+' bytes' + CRLF );
             end;
         end;
     end
   else
     begin
       TSImapThread(AThread).CurrentTag := Copy( BufInStrm, 1, i-1 );
       System.Delete( BufInStrm, 1, i );

       if trim(TSImapThread(AThread).CurrentTag)='' then begin
         TSImapThread(AThread).CurrentTag := BufInStrm;
         System.Delete(BufInStrm, 1, length(TSImapThread(AThread).CurrentTag) );
         Result := 'BAD Command failed (missing TAG)'
       end else begin
         TSImapThread(AThread).TmpData := '';
         bThread := AThread;
         bCommand:=copy(BufInStrm,0,length(BufInStrm)-2);
         AThread.Synchronize(AThread,@DoHandleCommand);
         Result := ''
       end;
     end;
   SetLength( BufInStrm, 0 );
end;
procedure TSImapServer.SetActive(const AValue: boolean);
begin
  inherited SetActive(AValue);
end;
procedure TSImapServer.Execute(AThread: TSTcpThread);
var
  LRow, LCmd, LTag: string;
  aRes: String;
  tmp: String;
  aRow: String;
begin
  try
    TSImapThread(AThread).LiteralLength:=0;
    LRow:='';
    SendData(AThread, '* OK IMAP4rev1 ' + SIMAPWelcomeMessage+ CRLF);
    while (not AThread.Terminated) do
      begin
        if TSImapThread(AThread).LiteralLength>0 then
          begin
            LRow := AThread.ReadPacket(Timeout);
            if length(LRow)>TSImapThread(AThread).LiteralLength then
              begin
                TSImapThread(AThread).TmpData:=TSImapThread(AThread).TmpData+copy(LRow,0,TSImapThread(AThread).LiteralLength);
                LRow := copy(LRow,TSImapThread(AThread).LiteralLength,length(LRow));
                TSImapThread(AThread).LiteralLength:=0;
              end
            else
              begin
                TSImapThread(AThread).LiteralLength:=TSImapThread(AThread).LiteralLength-length(LRow);
                TSImapThread(AThread).TmpData:=TSImapThread(AThread).TmpData+LRow;
                LRow:='';
              end;
            if TSImapThread(AThread).LiteralLength<=0 then
              begin
                HandleCommand(AThread,TSImapThread(AThread).TmpData);
              end;
          end
        else
          LRow := LRow+AThread.ReadPacket(Timeout);
        while (not AThread.Terminated) and (pos(CRLF,LRow)>0) do
          begin
            aRow := copy(LRow,0,pos(CRLF,LRow)+length(CRLF)-1);
            LRow:=copy(LRow,pos(CRLF,LRow)+length(CRLF),Length(LRow));
            if (not AThread.Connected) then
              Break
            else if aRow <> CRLF then
              begin
                if Assigned(OnLog) then
                  begin
                    tmp := aRow;
                    if pos('login ',LowerCase(aRow))>0 then
                      tmp := copy(aRow,0,pos('login ',LowerCase(aRow))+5);
                    OnLog(AThread, True, tmp);
                  end;
                aRes := HandleData(AThread,aRow);
                if aRes <> '' then
                  AThread.WriteLn(ares);
              end;
          end;
        sleep(10);
      end;
  except
    AThread.Terminate;
  end;
end;
constructor TSImapServer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FIMAPDelay:=0;
  FDisableLog:=0;
  FUseIMAPID:=True;
  FIMAPNCBrain :=False;
  FLocalTimeoutQuitDelay := 100;
  //Timeout:=10000;
  HierarchyDelimiter := '/';
  InitCriticalSection( CS_THR_IDLE );
  IdleState       := false;
  ClassType:=TSImapThread;
end;
destructor TSImapServer.Destroy;
begin
  Active:=False;
  DoneCriticalsection( CS_THR_IDLE );
  inherited Destroy;
end;
procedure TSImapServer.DisableLog;
begin
  inc(FDisableLog);
end;
procedure TSImapServer.EnableLog;
begin
  dec(fDisableLog);
end;
end.
