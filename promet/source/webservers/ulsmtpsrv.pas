{*******************************************************************************
  Copyright (C) Christian Ulrich info@cu-tec.de

  This source is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 2 of the License, or commercial alternative
  contact us for more information

  This code is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
  to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
  MA 02111-1307, USA.
Created 01.06.2006
*******************************************************************************}
unit ulsmtpsrv;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, lNet, lEvents;

type
  TLSMTPSocket = class(TLSocket)
    procedure LSMTPSocketError(aHandle: TLHandle; const msg: string);
  private
    FBuffer: string;
    FId: Integer;
    FShouldSend: Boolean;
    FTo: TStringList;
    FUser: string;
//    FGroup : TNNTPGroup;
//    FMessage : TMimeMess;
    FmessageIdx : Integer;
    FPostMode : Boolean;
    FPostMessage : TStringList;
    FSendBuffer : TStrings;
    FError : Boolean;
    FTerminated : Boolean;
  protected
    procedure DoSendBuffer;
  public
    DontLog : Boolean;
    property SendBuffer : TStrings read FSendBuffer;
    function Send(const aData; const aSize: Integer): Integer; override;
    property Buffer : string read FBuffer write FBuffer;
    procedure LineReceived(aLine : string);
    property User : string read FUser write FUser;
    property SendTo : TStringList read FTo;
    property Id : Integer read FId write FId;
    property ShouldSend : Boolean read FShouldSend write FShouldSend;
    constructor Create;override;
    destructor Destroy;override;
  end;
  TLLoginEvent = function(aSocket : TLSocket;aUser,aPasswort : string) : Boolean of object;
  TLLogEvent = procedure(aSocket : TLSocket;DirectionIn : Boolean;aMessage : string) of object;
  TLSMTPMailEvent = procedure(aSocket : TLSMTPSocket;aMail : TStrings;aFrom : string;aTo : TStrings) of object;
  TLSMTPAcceptEvent = function(aSocket : TLSMTPSocket;aFrom : string;aTo : TStrings) : Boolean of object;

  { TLSMTPServer }

  TLSMTPServer = class(TLTcp)
  private
    FListenInterface: string;
    FListenPort: Integer;
    FLog: TLLogEvent;
    FLogin: TLLoginEvent;
    FOnMAccept: TLSMTPAcceptEvent;
    FOnMail: TLSMTPMailEvent;
    FSocketCounter : Integer;
  protected
    procedure AcceptEvent(aSocket: TLHandle); override;
   procedure ReceiveEvent(aSocket: TLHandle); override;
    procedure CanSendEvent(aSocket: TLHandle); override;
  public
   constructor Create(aOwner: TComponent); override;
   procedure Start;
   property OnLogin : TLLoginEvent read FLogin write FLogin;
   property OnLog : TLLogEvent read FLog write FLog;
   property OnMailreceived : TLSMTPMailEvent read FOnMail write FOnMail;
   property OnAcceptMail : TLSMTPAcceptEvent read FOnMAccept write FOnMAccept;
   property ListenInterface : string read FListenInterface write FListenInterface;
   property ListenPort : Integer read FListenPort write FListenPort;
   procedure CallAction; override;
  end;

implementation
const
  CRLF=#13#10;
procedure TLSMTPSocket.LSMTPSocketError(aHandle: TLHandle; const msg: string);
begin

end;

procedure TLSMTPSocket.DoSendBuffer;
begin

end;

function TLSMTPSocket.Send(const aData; const aSize: Integer): Integer;
var
  aMessage : string;
begin
  if (not DontLog) and Assigned(TLSMTPServer(Creator).OnLog) then
    begin
      Setlength(aMessage,aSize);
      Move(aData,aMessage[1],aSize);
      Setlength(aMessage,aSize-2);
      TLSMTPServer(Creator).OnLog(Self,True,aMessage);
    end;
  Result:=inherited Send(aData, aSize);
end;

procedure TLSMTPSocket.LineReceived(aLine: string);
var
  Answered : Boolean = False;
  aCommand: String;
  aParams: String;
  Result: Boolean;

  procedure Answer(aMsg : string);
  begin
    if SendMessage(aMsg+CRLF) = 0 then
      FSendBuffer.Add(aMsg);
    Answered := True;
  end;
begin
  if Assigned(TLSMTPServer(Self.Creator).OnLog) and (not FPostMode) then
    begin
      if not (pos('PASS',Uppercase(aLine)) > 0) then
        TLSMTPServer(Self.Creator).OnLog(Self,False,aLine)
      else
        TLSMTPServer(Self.Creator).OnLog(Self,False,copy(aLine,0,pos('PASS',Uppercase(aLine))+4));
    end;
  if FPostMode then
    begin
      if aLine = '.' then
        begin
          DontLog := false;
          FPostMode := False;
          if Assigned(TLSMTPServer(Self.Creator).OnMailreceived) then
            TLSMTPServer(Self.Creator).OnMailreceived(Self,FPostMessage,FUser,FTo);
          Answer('250 Ok');
        end
      else FPostMessage.Add(aLine);
      exit;
    end;
  if pos(':',aLine) > 0 then
    begin
      aCommand := Uppercase(copy(aLine,0,pos(':',aLine)-1));
      aParams := trim(copy(aLine,pos(':',aLine)+1,length(aLine)));
    end
  else if pos(' ',aLine) > 0 then
    begin
      aCommand := Uppercase(copy(aLine,0,pos(' ',aLine)-1));
      aParams := trim(copy(aLine,pos(' ',aLine)+1,length(aLine)));
    end
  else
    aCommand := Uppercase(aLine);
  if aCommand = 'HELO' then
    begin
      Answer('250 Hello '+aParams);
    end
  else if aCommand = 'EHLO' then
    begin
      Answer('250 Hello '+aParams);
      Answer('250 AUTH CRAM-MD5 TLS LOGIN PLAIN');
    end
  else if aCommand = 'MAIL FROM' then
    begin
      FTo.Clear;
      FUser:=aParams;
      Answer('250 OK');
    end
  else if aCommand = 'RCPT TO' then
    begin
      FTo.Add(aParams);
      Result := True;
      if Assigned(TLSMTPServer(Self.Creator).OnAcceptMail) then
        Result := TLSMTPServer(Self.Creator).OnAcceptMail(Self,FUser,FTo);
      if not Result then
        Answer('550 no such user')
      else
        Answer('250 OK');
    end
  else if aCommand = 'DATA' then
    begin
      Answer('354 End data with <CR><LF>.<CR><LF>');
      DontLog := True;
      FPostMessage.Clear;
      FPostMode:=True;
    end
  else if aCommand = 'QUIT' then
    begin
      Answer('221 closing channel');
    end
  else if aCommand = 'RSET' then
    begin
      FUser:='';
      FTo.Clear;
      Answer('250 OK');
    end
  else
    Answer('502 Command not implemented');
end;

constructor TLSMTPSocket.Create;
begin
  inherited Create;
  FTo := TStringList.Create;
  FPostMessage := TStringList.Create;
end;

destructor TLSMTPSocket.Destroy;
begin
  FPostMessage.Free;
  FTo.Destroy;
  inherited Destroy;
end;

procedure TLSMTPServer.AcceptEvent(aSocket: TLHandle);
begin
  inherited AcceptEvent(aSocket);
  TLSocket(aSocket).SendMessage('220 service ready'+CRLF);
  with TLSMTPSocket(aSocket) do
    if Id = 0 then
      begin
        Id := FSocketCounter+1;
        inc(FSocketCounter);
      end;
end;

procedure TLSMTPServer.ReceiveEvent(aSocket: TLHandle);
var
  aMessage : string;
begin
  inherited ReceiveEvent(aSocket);
  if TLSocket(aSocket).getMessage(aMessage) > 0 then
    begin
      TLSMTPSocket(aSocket).Buffer := TLSMTPSocket(aSocket).Buffer+aMessage;
      while pos(CRLF,TLSMTPSocket(aSocket).Buffer) > 0 do
        begin
          TLSMTPSocket(aSocket).LineReceived(copy(TLSMTPSocket(aSocket).Buffer,0,pos(CRLF,TLSMTPSocket(aSocket).Buffer)-1));
          TLSMTPSocket(aSocket).Buffer := copy(TLSMTPSocket(aSocket).Buffer,pos(CRLF,TLSMTPSocket(aSocket).Buffer)+2,length(TLSMTPSocket(aSocket).Buffer));
        end;
    end;
end;

procedure TLSMTPServer.CanSendEvent(aSocket: TLHandle);
begin
  inherited CanSendEvent(aSocket);
  TLSMTPSocket(aSocket).ShouldSend := True;
end;

constructor TLSMTPServer.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FSocketCounter := 0;
  SocketClass := TLSMTPSocket;
  FListenInterface:=LADDR_ANY;
  FListenPort := 25;
end;

procedure TLSMTPServer.Start;
begin
  if not Listen(ListenPort,ListenInterface) then raise Exception.Create('Listen Failed')
end;

procedure TLSMTPServer.CallAction;
begin
  inherited CallAction;
  while IterNext do
    if Iterator is TLSMTPSocket then
      TLSMTPSocket(Iterator).DoSendBuffer;
end;

end.
