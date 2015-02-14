unit ussmtpserver;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, usbaseserver, mimemess, mimepart;

type
  TSSMTPMailEvent = procedure(aSocket: TSTcpThread; aMail: TStrings;
    aFrom: string; aTo: TStrings) of object;
  TSSMTPAcceptEvent = function(aSocket: TSTcpThread; aFrom: string;
    aTo: TStrings): boolean of object;

  TSmtpSession = class
  private
    FSubject, FFrom: string;
    FMessage: TMimeMess;
    FToo: TStringList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
  published
    property From: string read FFrom write FFrom;
    property Too: TStringList read FToo write FToo;
    property Subject: string read FSubject write FSubject;
    property MimeMsg: TMimeMess read FMessage write FMessage;
  end;

type

  { TSSmtpServer }

  TSSmtpServer = class(TSTcpServer)
  private
    FMailroot, FPostmaster, FPostmasterEMail: string;
    FOnMAccept: TSSMTPAcceptEvent;
    FOnMail: TSSMTPMailEvent;
    FPermissions, FLocalDomains, FTrustedDomains, FBannedAttachments: TStringList;
    FMaxMsgSize: integer;
    function FindFileType(const AExtension: string; const AMimePart: TMimePart): boolean;
    procedure SendToClient(AThread: TSTcpThread; const AText: string);
    procedure DoCommandHelo(AThread: TSTcpThread; ASession: TSmtpSession;
      const AParam: string); virtual;
    procedure DoCommandQuit(AThread: TSTcpThread; ASession: TSmtpSession;
      const AParam: string); virtual;
    procedure DoCommandData(AThread: TSTcpThread; ASession: TSmtpSession;
      const AParam: string); virtual;
    procedure DoCommandMail(AThread: TSTcpThread; ASession: TSmtpSession;
      const AParam: string); virtual;
    procedure DoCommandRcpt(AThread: TSTcpThread; ASession: TSmtpSession;
      const AParam: string); virtual;
    procedure DoCommandRSet(AThread: TSTcpThread; ASession: TSmtpSession;
      const AParam: string); virtual;
    procedure DoCommandError(AThread: TSTcpThread; ASession: TSmtpSession;
      const AParam: string); virtual;
  protected
    procedure SetActive(const AValue: boolean); override;
    procedure Execute(AThread: TSTcpThread); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property LocalDomains: TStringList read FLocalDomains write FLocalDomains;
    property Permissions: TStringList read FPermissions write FPermissions;
    property Postmaster: string read FPostmaster write FPostmaster;
    property TrustedDomains: TStringList read FTrustedDomains write FTrustedDomains;
    property MaxMsgSize: integer read FMaxMsgSize write FMaxMsgSize default 1; // 1MB
    property BannedAttachments: TStringList read FBannedAttachments
      write FBannedAttachments;
    property OnMailreceived: TSSMTPMailEvent read FOnMail write FOnMail;
    property OnAcceptMail: TSSMTPAcceptEvent read FOnMAccept write FOnMAccept;
  end;

implementation

uses
  mailchck, synautil;

resourcestring
  SSMTPNoRecipients = 'No recipients: need RCPT';
  SSMTPSendMessage = 'Send MimeMsg. End with CRLF.CRLF';
  SSMTPMaxMessageSizeExceeded = 'Maximum message size exceeded';
  SSMTPNotAlloweAttachmentFound =
    'A not allowed attachment found, mail rejected for security reasons';
  SSMTPCommandUnknown = 'error - SMTP command unknown or not implemented yet!';
  SSMTPHelo = 'OK Helo';
  SSMTPReadyFor = 'OK smtp ready for';
  SSMTPNoSpamPlease = 'No spam please';
  SSMTPClosingConnection = 'closing connection';
  SSMTPNeedMailFrom = 'No originator: need MAIL';
  SSMTPWeDoNotRelayFor = 'Sorry, we do not relay for';
  SSMTPResetOK = 'OK - Reset';
  SSMTPWelcomeMessage = 'service ready';
  SSMTPCommandIsNil = 'SMTP command is nil';

constructor TSmtpSession.Create;
begin
  FMessage := TMimeMess.Create;
  FToo := TStringList.Create;
  // to avoid duplicates
  FToo.Duplicates := dupIgnore;
  FToo.Sorted := True;
  Clear;
end;

destructor TSmtpSession.Destroy;
begin
  FreeAndNil(FMessage);
  FreeAndNil(FToo);
  inherited;
end;

procedure TSmtpSession.Clear;
begin
  FMessage.Clear;
  FToo.Clear;
end;

constructor TSSmtpServer.Create(AOwner: TComponent);
begin
  inherited;
  FPermissions := TStringList.Create;
  FLocalDomains := TStringList.Create;
  FTrustedDomains := TStringList.Create;
  FBannedAttachments := TStringList.Create;
  FMaxMsgSize := 1;
  ListenPort := 25;
end;

destructor TSSmtpServer.Destroy;
begin
  if (GetActive) then
    SetActive(False);
  FreeAndNil(FBannedAttachments);
  FreeAndNil(FTrustedDomains);
  FreeAndNil(FPermissions);
  FreeAndNil(FLocalDomains);
  inherited;
end;

procedure TSSmtpServer.DoCommandData(AThread: TSTcpThread; ASession: TSmtpSession;
  const AParam: string);
var
  LFolder, LRcvdRow: string;
  LSize, i: integer;
  LBanned: boolean;
begin
  if (ASession.Too.Count = 0) then
    SendToClient(AThread, '513 ' + SSMTPNoRecipients)
  else
  begin
    SendToClient(AThread, '354 ' + SSMTPSendMessage);
    repeat
      if (not Active) then
        Break
      else
      begin
        LRcvdRow := AThread.ReadLn(Timeout);
        if (not AThread.Connected) then
          Break
        else if (AThread.ReadTimedOut) then
            Break
          else
          begin
            if (AnsiSameText('.', LRcvdRow)) then
            begin
              { todo: max mime file size checking }
              LSize := Length(ASession.MimeMsg.Lines.Text) div (1024 * 1024);
              if (LSize > FMaxMsgSize) then
                SendToClient(AThread, '503 ' + SSMTPMaxMessageSizeExceeded)
              else
              begin
                { todo: attachments filter }
                LBanned := False;
                ASession.MimeMsg.DecodeMessage;
                for i := 0 to FBannedAttachments.Count - 1 do
                begin
                  if (FindFileType(FBannedAttachments[i],
                    ASession.MimeMsg.MessagePart)) then
                  begin
                    LBanned := True;
                    Break;
                  end;
                end;
                if (LBanned) then
                begin
                  SendToClient(AThread, '504 ' + SSMTPNotAlloweAttachmentFound);
                end
                else
                begin
                  if Assigned(OnMailreceived) then
                    OnMailreceived(AThread, ASession.MimeMsg.Lines,
                      ASession.MimeMsg.Header.From, ASession.MimeMsg.Header.ToList);
                  SendToClient(AThread, '250 OK');
                end;
              end;
              ASession.Clear;
              Break;
            end
            else
              ASession.MimeMsg.Lines.Add(LRcvdRow);
          end;
      end;
    until (False);
  end;
end;

// -----------------------------------------------------------------------------
procedure TSSmtpServer.DoCommandError(AThread: TSTcpThread;
  ASession: TSmtpSession; const AParam: string);
begin
  SendToClient(AThread, '520 ' + SSMTPCommandUnknown + ' (' + AParam + ')');
end;

procedure TSSmtpServer.DoCommandHelo(AThread: TSTcpThread; ASession: TSmtpSession;
  const AParam: string);
begin
  SendToClient(AThread, '250 ' + SSMTPHelo + ' ' + AThread.Connection.PeerName);
end;

// -----------------------------------------------------------------------------
procedure TSSmtpServer.DoCommandMail(AThread: TSTcpThread; ASession: TSmtpSession;
  const AParam: string);
var
  LMailFromOK: boolean;
  LDomain, LEmail: string;
begin
  { todo: anti spam filter (list based or/and user's mail lookup (verify) }
  LMailFromOK := False;
  LEmail := GetEmailAddr(AParam);
  LDomain := SeparateRight(LEmail, '@');
  if (LMailFromOK) then
  begin
    ASession.From := LEmail;
    SendToClient(AThread, '250 ' + SSMTPReadyFor + ' ' + ASession.From);
  end
  else
  begin
    SendToClient(AThread, '502 ' + SSMTPNoSpamPlease);
  end;
end;

// -----------------------------------------------------------------------------
procedure TSSmtpServer.DoCommandQuit(AThread: TSTcpThread; ASession: TSmtpSession;
  const AParam: string);
begin
  SendToClient(AThread, '221 ' + SSMTPClosingConnection);
  AThread.Disconnect;
end;

// -----------------------------------------------------------------------------
procedure TSSmtpServer.DoCommandRcpt(AThread: TSTcpThread; ASession: TSmtpSession;
  const AParam: string);
var
  LRcptOK: boolean;
  i: integer;
  LDomain, LEmail, LUserPart: string;
begin
  if (AnsiSameText(EmptyStr, ASession.From)) then
    SendToClient(AThread, '512 ' + SSMTPNeedMailFrom)
  else
  begin
    { todo: relay permission }
    LRcptOK := False;
    LEmail := GetEmailAddr(AParam);
    LDomain := SeparateRight(LEmail, '@');
    LUserPart := SeparateLeft(LEmail, '@');

    if (Assigned(OnAcceptMail) and OnAcceptMail(AThread, LEmail, ASession.FToo)) then
      LRcptOK := True
    else if (FLocalDomains.IndexOf(LDomain) >= 0) then
      begin // if the user does not exists, but it is a local domain, deliver to postmaster
        LRcptOK := True;
        LEmail := FPostmasterEMail;
      end;
    if (LRcptOK) then
    begin
      ASession.Too.Add(LEmail);
      SendToClient(AThread, '250 ' + SSMTPReadyFor + AParam);
    end
    else
    begin
      SendToClient(AThread, '501 ' + SSMTPWeDoNotRelayFor + ' ' + AParam);
    end;
  end;
end;

// -----------------------------------------------------------------------------
procedure TSSmtpServer.DoCommandRSet(AThread: TSTcpThread; ASession: TSmtpSession;
  const AParam: string);
begin
  ASession.Clear;
  SendToClient(AThread, '250 ' + SSMTPResetOK);
end;

// -----------------------------------------------------------------------------
procedure TSSmtpServer.SetActive(const AValue: boolean);
var
  i: integer;
begin
  if (AValue) then
  begin
  end;
  inherited;
end;

// -----------------------------------------------------------------------------
procedure TSSmtpServer.SendToClient(AThread: TSTcpThread; const AText: string);
begin
  AThread.WriteLn(AText);
  if Assigned(OnLog) then
    OnLog(AThread, False, AText);
end;

// -----------------------------------------------------------------------------
function TSSmtpServer.FindFileType(const AExtension: string;
  const AMimePart: TMimePart): boolean;
var
  i: integer;
begin
  Result := False;
  for i := 0 to AMimePart.GetSubPartCount - 1 do
  begin
    if (AMimePart.GetSubPart(i).GetSubPartCount > 0) then
      FindFileType(AExtension, AMimePart.GetSubPart(i))
    else
    begin
      if (AnsiSameText(AExtension, ExtractFileExt(AMimePart.GetSubPart(i).FileName)))
      then
      begin
        Result := True;
        Break;
      end;
    end;
  end;
end;

// -----------------------------------------------------------------------------
procedure TSSmtpServer.Execute(AThread: TSTcpThread);
var
  LRow, LCmd, LTag: string;
  LSession: TSmtpSession;
begin
  LSession := TSmtpSession.Create;
  try
    SendToClient(AThread, '220 ' + SSMTPWelcomeMessage);
    while (not AThread.Terminated) do
    begin
      LRow := AThread.ReadLn(Timeout);

      if (AThread.ReadTimedOut) then
        Break
      else if (not AThread.Connected) then
          Break
        else
        begin
          if Assigned(OnLog) then
            OnLog(AThread, True, LRow);

          if (AnsiPos(':', LRow) > 0) then
            LCmd := Fetch(LRow, ':')
          else
            LCmd := Fetch(LRow, #32);
          LTag := Fetch(LRow, #32);

          if (AnsiCompareText(EmptyStr, LCmd) = 0) then
            SendToClient(AThread, '500 ' + SSMTPCommandIsNil)
          else
          begin
            if (AnsiPos(' ', LCmd) > 0) then
              Delete(LCmd, AnsiPos(' ', LCmd), Length(LCmd));
            if (AnsiCompareText('HELO', LCmd) = 0) then
              DoCommandHelo(AThread, LSession, LTag)
            else if (AnsiCompareText('EHLO', LCmd) = 0) then
              DoCommandError(AThread, LSession, LTag)
            else if (AnsiCompareText('MAIL', LCmd) = 0) then
              DoCommandMail(AThread, LSession, LTag)
            else if (AnsiCompareText('RCPT', LCmd) = 0) then
              DoCommandRcpt(AThread, LSession, LTag)
            else if (AnsiCompareText('DATA', LCmd) = 0) then
              DoCommandData(AThread, LSession, LTag)
            else if (AnsiCompareText('SEND', LCmd) = 0) then
              DoCommandError(AThread, LSession, LTag)
            else if (AnsiCompareText('SOML', LCmd) = 0) then
              DoCommandError(AThread, LSession, LTag)
            else if (AnsiCompareText('SAML', LCmd) = 0) then
              DoCommandError(AThread, LSession, LTag)
            else if (AnsiCompareText('RSET', LCmd) = 0) then
              DoCommandRSet(AThread, LSession, LTag)
            else if (AnsiCompareText('EXPN', LCmd) = 0) then
              DoCommandError(AThread, LSession, LTag)
            else if (AnsiCompareText('HELP', LCmd) = 0) then
              DoCommandError(AThread, LSession, LTag)
            else if (AnsiCompareText('VRFY', LCmd) = 0) then
              DoCommandError(AThread, LSession, LTag)
            else if (AnsiCompareText('NOOP', LCmd) = 0) then
              DoCommandError(AThread, LSession, LTag)
            else if (AnsiCompareText('QUIT', LCmd) = 0) then
              DoCommandQuit(AThread, LSession, LTag)
            else if (AnsiCompareText('TURN', LCmd) = 0) then
              DoCommandError(AThread, LSession, LTag)
            else if (AnsiCompareText('ETRN', LCmd) = 0) then
              DoCommandError(AThread, LSession, LTag)
            else
              SendToClient(AThread, '500 ' + SSMTPCommandUnknown);
          end;
        end;
    end;
  finally
    FreeAndNil(LSession);
  end;
end;

end.
