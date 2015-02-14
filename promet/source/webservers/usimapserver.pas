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
    Selected       : TImapMailbox;
    CS_THR_IDLE     : TRTLCriticalSection;
    SendExpunge     : array of Integer;
    SendNewMessages : Boolean;
    IdleState       : Boolean;

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

resourcestring
  SIMAPWelcomeMessage = 'service ready';

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
            SendRes(AThread, IntToStr(SendExpunge[i]) + ' EXPUNGE');
          if not SendNewMessages then //Don't send it double
            SendRes(AThread,IntToStr(Selected.Messages) + ' EXISTS');
          SetLength(SendExpunge, 0);
        end;
        if SendNewMessages then
        begin
          SendRes(AThread,IntToStr(Selected.Messages) + ' EXISTS');
          SendRes(AThread,IntToStr(Selected.Recent) + ' RECENT');
          SendNewMessages := False;
        end;
      finally
        LeaveCriticalSection(CS_THR_IDLE)
      end;
    finally
      Selected.Unlock
    end;
  SendData(AThread,CurrentTag + ' ' + Txt + CRLF);
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
    SendData(AThread, '* OK IMAP4rev1 ' + SIMAPWelcomeMessage + CRLF);
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
