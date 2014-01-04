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
unit uLNNTP;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, lNet, lEvents, mimemess, db, dateutils;
type
  TNNTPGroup = class(TComponent)
  private
    FMessageIdx: LargeInt;
  protected
    FFirstID: LargeInt;
    FName: string;
    function GetCount: Integer;virtual;
    function GetLastID: LargeInt;virtual;
    function GetFirstID: LargeInt;virtual;
    function GetMessage(Idx : Integer): TMimeMess;virtual;
    function GetMessageByID(Idx : string): TMimeMess;virtual;
    function SelectMessages(aFilter : string) : Boolean;virtual;
    function GetCreatedAt: TDateTime;virtual;
    function XOverAnswer : string;virtual;abstract;
    function PostArticle(aArticle : TStrings) : Boolean;virtual;abstract;
  public
    constructor Create(aName : string;FirstID : LargeInt);virtual;
    destructor Destroy;override;
    property Name : string read FName;
    property Count : Integer read GetCount;
    property FirstID : LargeInt read GetFirstID;
    property LastID : LargeInt read GetLastID;
    property Message[Idx : Integer] : TMimeMess read GetMessage;
    property MessageByID[Idx : string] : TMimeMess read GetMessageByID;
    property MessageIdx : LargeInt read FMessageIdx write FmessageIdx;
    property CreatedAt : TDateTime read GetCreatedAt;
  end;
  TNNTPGroupClass = class of TNNTPGroup;
  TNNTPGroups = class(TList)
  private
    function Get(Idx : Integer): TNNTPGroup;
  public
    property Group[Idx : Integer] : TNNTPGroup read Get;
  end;
  TLNNTPSocket = class(TLSocket)
    procedure LNNTPSocketError(aHandle: TLHandle; const msg: string);
  private
    FBuffer: string;
    FId: Integer;
    FShouldSend: Boolean;
    FUser: string;
    FGroup : TNNTPGroup;
    FMessage : TMimeMess;
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
    property Id : Integer read FId write FId;
    property ShouldSend : Boolean read FShouldSend write FShouldSend;
    constructor Create;override;
    destructor Destroy;override;
  end;
  TLNNTPLoginEvent = function(aSocket : TLNNTPSocket;aUser,aPasswort : string) : Boolean of object;
  TLNNTPLogEvent = procedure(aSocket : TLNNTPSocket;DirectionIn : Boolean;aMessage : string) of object;
  TLNNTPServer = class(TLTcp)
  private
    FGroups: TNNTPGroups;
    FLog: TLNNTPLogEvent;
    FLogin: TLNNTPLoginEvent;
    FSocketCounter : Integer;
  protected
    procedure AcceptEvent(aSocket: TLHandle); override;
   procedure ReceiveEvent(aSocket: TLHandle); override;
    procedure CanSendEvent(aSocket: TLHandle); override;
  public
   constructor Create(aOwner: TComponent); override;
   destructor Destroy; override;
   property OnLogin : TLNNTPLoginEvent read FLogin write FLogin;
   property OnLog : TLNNTPLogEvent read FLog write FLog;
   property Groups : TNNTPGroups read FGroups;
   procedure CallAction; override;
  end;
implementation
uses lHTTPUtil;
const
  CRLF=#13#10;
function TNNTPGroups.Get(Idx : Integer): TNNTPGroup;
begin
  if Idx < Count then
    Result := TNNTPGroup(Items[Idx]);
end;
function TNNTPGroup.GetMessageByID(Idx : string): TMimeMess;
begin
  Result := nil;
end;
function TNNTPGroup.SelectMessages(aFilter: string): Boolean;
begin
  Result := False;
end;
function TNNTPGroup.GetCreatedAt: TDateTime;
begin
  Result := Now();
end;
function TNNTPGroup.GetFirstID: LargeInt;
begin
  Result := FFirstID;
end;
function TNNTPGroup.GetCount: Integer;
begin
  Result := 0;
end;
function TNNTPGroup.GetLastID: LargeInt;
begin
  Result := 0;
end;
function TNNTPGroup.GetMessage(Idx : Integer): TMimeMess;
begin
  Result := nil;
end;
constructor TNNTPGroup.Create(aName: string;FirstID : LargeInt);
begin
  FName := aName;
  FFirstID := FirstID;
end;
destructor TNNTPGroup.Destroy;
begin
  inherited Destroy;
end;
procedure TLNNTPSocket.LNNTPSocketError(aHandle: TLHandle; const msg: string);
begin
  TLNNTPSocket(aHandle).FError:=True;
end;
procedure TLNNTPSocket.DoSendBuffer;
var
  tmp: String;
begin
  try
    if (SendBuffer.Count = 0) or (not FShouldSend) then exit;
    while (not Fterminated) and (SendBuffer.Count > 0) do
      begin
        tmp := SendBuffer[0];
        DontLog := True;
        if SendMessage(tmp+CRLF) > 0 then
          begin
            DontLog := False;
            if FTerminated then exit;
            SendBuffer.Delete(0);
            try
              if Assigned(Creator) then
                Creator.CallAction;
            except
            end;
          end
        else
          begin
            DontLog := False;
            break;
          end;
      end;
    if (SendBuffer.Count = 0) then
      FShouldSend := False;
  except //Client disconnects ??
  end;
end;
function TLNNTPSocket.Send(const aData; const aSize: Integer): Integer;
var
  aMessage : string;
begin
  if (not DontLog) and Assigned(TLNNTPServer(Creator).OnLog) then
    begin
      Setlength(aMessage,aSize);
      Move(aData,aMessage[1],aSize);
      Setlength(aMessage,aSize-2);
      TLNNTPServer(Creator).OnLog(Self,True,aMessage);
    end;
  Result:=inherited Send(aData, aSize);
end;
procedure TLNNTPSocket.LineReceived(aLine: string);
var
  aCommand : string;
  aParams : string = '';
  Answered : Boolean = False;
  tmp : string;
  aMessage : TMimeMess;
  procedure Answer(aMsg : string);
  begin
    if SendMessage(aMsg+CRLF) = 0 then
      FSendBuffer.Add(aMsg);
    Answered := True;
  end;
  function IsNumeric(s:String):Boolean;
  var i:Integer;
  begin
    Result:=False;
    for i := 1 to Length(s) do
      if (s[i] > '9') or (s[i] < '0') then exit;
    Result:=True;
  end;
  function SelectMessage : Boolean;
  begin
    Result := False;
    if not Assigned(FGroup) then exit;
    if IsNumeric(aParams) then
      begin
        if FMessageIdx = StrToInt(aParams) then
          begin
            aMessage := FMessage;
            Result := True;
          end
        else
          begin
            aMessage := FGroup.Message[StrToInt(aParams)];
            FMessageIdx := FGroup.MessageIdx;
            FMessage := aMessage;
            if Assigned(aMessage) then
              begin
                result := True;
              end;
          end;
      end
    else
      begin
        if Assigned(FMessage) and (FMessage.Header.MessageID = aParams) then
          begin
            aMessage := Fmessage;
            Result := True;
          end
        else
          begin
            aMessage := FGroup.MessageByID[aParams];
            FMessageIdx := FGroup.MessageIdx;
            Fmessage := aMessage;
            if Assigned(aMessage) then
              begin
                aMessage.Encodemessage;
                Result := True;
              end;
          end;
      end;
  end;
var
  i : Integer;
  aGroup : TNNTPGroup;
  Found : Boolean;
  sl : TStringList;
  agroupClass : TNNTPGroupClass;
  aDate: TDateTime;
begin
  if Assigned(TLNNTPServer(Self.Creator).OnLog) and (not FPostMode) then
    begin
      if not (pos('PASS',Uppercase(aLine)) > 0) then
        TLNNTPServer(Self.Creator).OnLog(Self,False,aLine)
      else
        TLNNTPServer(Self.Creator).OnLog(Self,False,copy(aLine,0,pos('PASS',Uppercase(aLine))+4));
    end;
  if FPostMode then
    begin
      if aLine = '.' then
        begin
          DontLog := false;
          FPostMode := False;
          if Assigned(FGroup) then
            begin
              if FGroup.PostArticle(FPostMessage) then
                Answer('240 Article received OK')
              else
                Answer('441 Posting failed');
            end
          else
            begin
              aGroup := TLNNTPServer(Creator).Groups.Group[0];
              if Assigned(FGroup) then FGroup.Destroy;
              aGroupClass := TNNTPGroupClass(aGroup.ClassType);
              FGroup := aGroupClass.Create(aGroup.Name,aGroup.FirstID);
              if FGroup.PostArticle(FPostMessage) then
                Answer('240 Article received OK')
              else
                Answer('441 Posting failed');
              FreeAndNil(FGroup);
            end;
        end
      else FPostMessage.Add(aLine);
      exit;
    end;
  if pos(' ',aLine) > 0 then
    begin
      aCommand := Uppercase(copy(aLine,0,pos(' ',aLine)-1));
      aParams := trim(copy(aLine,pos(' ',aLine)+1,length(aLine)));
    end
  else
    aCommand := Uppercase(aLine);
  if aCommand = 'GROUP' then
    begin
      if (FUser = '') then
        begin
          Answer('480 Authentication required');
          exit;
        end;
      Found := False;
      for i := 0 to TLNNTPServer(Creator).Groups.Count-1 do
        begin
          if TLNNTPServer(Creator).Groups.Group[i].Name = aParams then
            begin
              aGroup := TLNNTPServer(Creator).Groups.Group[i];
              if Assigned(FGroup) then FGroup.Destroy;
              aGroupClass := TNNTPGroupClass(aGroup.ClassType);
              FGroup := aGroupClass.Create(aGroup.Name,aGroup.FirstID);
              Answer(Format('211 %d %d %d %s',[aGroup.Count,aGroup.FirstID,aGroup.LastID,aGroup.Name]));
              Found := True;
              break;
            end;
        end;
      if not Found then Answer('411 Group not Found.');
    end
  else if aCommand = 'HEAD' then
    begin
      if Assigned(FGroup) then
        begin
          if SelectMessage and Assigned(aMessage) then
            begin
              sl := TStringList.Create;
              aMessage.Header.EncodeHeaders(sl);
              Answer('221 '+InTToStr(FMessageIdx)+' <'+aMessage.Header.MessageID+'>');
              for i := 0 to sl.Count-1 do
                Answer(sl[i]);
              Answer(CRLF+'.');
              sl.Free;
            end
          else Answer('423 No article');
        end
      else Answer('412 No newsgroup selected');
    end
  else if aCommand = 'ARTICLE' then
    begin
      if Assigned(FGroup) then
        begin
          if SelectMessage and Assigned(aMessage) then
            begin
              Answer('220 '+InTToStr(FMessageIdx)+' <'+aMessage.Header.MessageID+'>');
              DontLog := True;
              for i := 0 to aMessage.Lines.Count-1 do
                Answer(aMessage.Lines[i]);
              DontLog := False;
              Answer('.');
            end
          else Answer('423 No article');
        end
      else Answer('412 No newsgroup selected');
    end
  else if aCommand = 'STAT' then
    begin
      if Assigned(FGroup) then
        begin
          if SelectMessage and Assigned(aMessage) then
            begin
              aMessage.Encodemessage;
              Answer('223 '+InTToStr(FMessageIdx)+' <'+aMessage.Header.MessageID+'>');
            end
          else Answer('423 No article');
        end
      else Answer('412 No newsgroup selected');
    end
  else if aCommand = 'MODE' then
    begin
//      if FUser = '' then
      Answer('201 ignored.');
    end
  else if aCommand = 'POST' then
    begin
      if (FUser = '') {and TLNNTPServer(Creator).ListRequiresAuth} then
        begin
          Answer('480 Authentication required');
          exit;
        end
      else
        begin
          Answer('340 Input article; end with <CR-LF>.<CR-LF>');
          FPostMode := True;
          FPostMessage.Clear;
          DontLog := True;
        end;
    end
  else if aCommand = 'XOVER' then
    begin
      if Assigned(FGroup) then
        begin
          if FGroup.SelectMessages(aParams) then
            begin
              Answer('224 Overview information follows');
              aParams := FGroup.XOverAnswer;
              DontLog := True;
              while aParams <> '' do
                begin
                  Answer(aParams);
                  aParams := FGroup.XOverAnswer;
                end;
              DontLog := False;
              Answer('.')
            end
          else Answer('412 wrong Parameters');
        end
      else Answer('412 not in a newsgroup');
    end
  else if aCommand = 'LIST' then
    begin
      if (FUser = '') {and TLNNTPServer(Creator).ListRequiresAuth} then
        begin
          Answer('480 Authentication required');
          exit;
        end
      else if (aParams = '') or (Uppercase(aParams) = 'ACTIVE') then
        begin
          Answer('215 list of newsgroups follows');
          for i := 0 to TLNNTPServer(Creator).Groups.Count-1 do
            begin
              aGroup := TLNNTPServer(Creator).Groups.Group[i];
              Answer(Format('%s %d %d %s',[aGroup.Name,aGroup.FirstID,aGroup.LastID,'y']));
            end;
          Answer('.');
        end
      else if (Uppercase(aParams) = 'OVERVIEW.FMT') then
        begin
          Answer('215 Order of fields in overview database.');
          Answer('Subject:');
          Answer('From:');
          Answer('Date:');
          Answer('Message-ID:');
          Answer('References:');
          Answer('Bytes:');
          Answer('Lines:');
          Answer('.');
        end
      else Answer('500 Command not implemented.');
    end
  else if aCommand = 'NEWGROUPS' then
    begin
      tmp := copy(aParams,0,pos(' ',aParams)-1);
      if length(tmp) = 6 then
        begin
          Insert(DateSeparator,tmp,3);
          Insert(DateSeparator,tmp,6);
        end
      else if length(tmp) = 8 then
        begin
          Insert(DateSeparator,tmp,5);
          Insert(DateSeparator,tmp,8);
        end;
      if (FUser = '') {and TLNNTPServer(Creator).ListRequiresAuth} then
        begin
          Answer('480 Authentication required');
          exit;
        end
      else if TryStrToDate(tmp,aDate,'yymmdd')
           or TryStrToDate(tmp,aDate,'yyyymmdd') then
        begin
          aParams := copy(aParams,pos(' ',aParams)+1,length(aParams));
          aParams := copy(aParams,pos(' ',aParams)+1,length(aParams));
          if aParams = 'GMT' then
            aDate := GMTToLocalTime(aDate);
          Answer('215 list of newsgroups follows');
          for i := 0 to TLNNTPServer(Creator).Groups.Count-1 do
            begin
              aGroup := TLNNTPServer(Creator).Groups.Group[i];
              if aGroup.CreatedAt > aDate then
                Answer(Format('%s %d %d %s',[aGroup.Name,aGroup.FirstID,aGroup.LastID,'y']));
            end;
          Answer('.');
        end
      else Answer('500 Command not implemented.');
    end
  else if aCommand = 'CAPABILITIES' then
    begin
      Answer('101 Capability list follows');
      Answer('VERSION 2');
      Answer('LIST ACTIVE NEWSGROUPS OVERVIEW.FMT');
      Answer('XOVER');
      Answer('.');
    end
  else if aCommand = 'AUTHINFO' then
    begin
      tmp := Uppercase(copy(aParams,0,pos(' ',aParams)-1));
      aParams := copy(aParams,pos(' ',aParams)+1,length(aParams));
      if tmp = 'USER' then
        begin
          FUser := aParams;
          Answer('381 More authentication information required.');
        end
      else if tmp = 'PASS' then
        begin
          if Assigned(TLNNTPServer(Creator).OnLogin) then
            if TLNNTPServer(Creator).OnLogin(Self,FUser,aParams) then
              begin
                Answer('281 Authentication accepted');
                exit;
              end;
          FUser := '';
          Answer('482 Authentication rejected')
        end
      else Answer('500 Command not implemented.');
    end
  else if aCommand = 'QUIT' then
    begin
      Answer('205 Closing connection.');
      Disconnect;
    end
  else if aCommand = '' then
  else Answer('500 Command not implemented.');
end;
constructor TLNNTPSocket.Create;
begin
  inherited Create;
  Fid := 0;
  FGroup := nil;
  FUser := '';
  FPostMode := False;
  FPostMessage := TStringList.Create;
  DontLog := False;
  FSendBuffer := TStringList.Create;
  FError := False;
  FTerminated := False;
  Self.OnError:=@LNNTPSocketError;
end;
destructor TLNNTPSocket.Destroy;
begin
  FTerminated := True;
  if Assigned(FGroup) then fGroup.Destroy;
  FSendBuffer.Free;
  FPostMessage.Destroy;
  inherited;
end;
procedure TLNNTPServer.AcceptEvent(aSocket: TLHandle);
begin
  inherited AcceptEvent(aSocket);
  TLSocket(aSocket).SendMessage('200 Greets from NNTP Server'+CRLF);
  with TLNNTPSocket(aSocket) do
    if Id = 0 then
      begin
        Id := FSocketCounter+1;
        inc(FSocketCounter);
      end;
end;
procedure TLNNTPServer.ReceiveEvent(aSocket: TLHandle);
var
  aMessage : string;
begin
  inherited ReceiveEvent(aSocket);
  if TLSocket(aSocket).getMessage(aMessage) > 0 then
    begin
      TLNNTPSocket(aSocket).Buffer := TLNNTPSocket(aSocket).Buffer+aMessage;
      while pos(CRLF,TLNNTPSocket(aSocket).Buffer) > 0 do
        begin
          TLNNTPSocket(aSocket).LineReceived(copy(TLNNTPSocket(aSocket).Buffer,0,pos(CRLF,TLNNTPSocket(aSocket).Buffer)-1));
          TLNNTPSocket(aSocket).Buffer := copy(TLNNTPSocket(aSocket).Buffer,pos(CRLF,TLNNTPSocket(aSocket).Buffer)+2,length(TLNNTPSocket(aSocket).Buffer));
        end;
    end;
end;
procedure TLNNTPServer.CanSendEvent(aSocket: TLHandle);
var
  tmp: String;
  maxlen: Integer = 0;
  aSock: TLSocket;
begin
  inherited CanSendEvent(aSocket);
  TLNNTPSocket(aSocket).ShouldSend := True;
end;
constructor TLNNTPServer.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FSocketCounter := 0;
  FGroups := TNNTPGroups.Create;
  SocketClass := TLNNTPSocket;
  Listen(119);
end;
destructor TLNNTPServer.Destroy;
begin
  FGroups.Destroy;
  inherited Destroy;
end;
procedure TLNNTPServer.CallAction;
begin
  inherited CallAction;
  while IterNext do
    if Iterator is TLNNTPSocket then
      TLNNTPSocket(Iterator).DoSendBuffer;
end;

end.
