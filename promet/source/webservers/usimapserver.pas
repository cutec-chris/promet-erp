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
    CurrentTag: string;

    procedure LogRaw(AThread: TSTcpThread; Txt: string);
    procedure SendData(AThread: TSTcpThread; const AText: string);
    procedure SendResLit(AThread: TSTcpThread; Txt: string);
    procedure SendResTag(AThread: TSTcpThread; Txt: string);
    procedure SendRes(AThread: TSTcpThread; Txt: string);
  protected
    procedure SetActive(const AValue: boolean); override;
    procedure Execute(AThread: TSTcpThread); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
  end;

implementation

{ TSImapServer }

procedure TSImapServer.LogRaw(AThread: TSTcpThread; Txt: string);
begin
  if Assigned(OnLog) then
    OnLog(AThread, False, Txt);
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
            SendRes(IntToStr(SendExpunge[i]) + ' EXPUNGE');
          if not SendNewMessages then //Don't send it double
            SendRes(IntToStr(Selected.Status.Messages) + ' EXISTS');
          SetLength(SendExpunge, 0);
        end;
        if SendNewMessages then
        begin
          SendRes(IntToStr(Selected.Status.Messages) + ' EXISTS');
          SendRes(IntToStr(Selected.Status.Recent) + ' RECENT');
          SendNewMessages := False;
        end;
      finally
        LeaveCriticalSection(CS_THR_IDLE)
      end;
    finally
      Selected.Unlock
    end;
  SendResult(CurrentTag + ' ' + Txt);
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

constructor TSImapServer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TSImapServer.Destroy;
begin
  inherited Destroy;
end;

end.
