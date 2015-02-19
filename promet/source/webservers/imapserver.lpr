{*******************************************************************************
Copyright (C) Christian Ulrich info@cu-tec.de

This source is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2 of the License, or commercial alternative
contact us for more information

This code is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
details.

A copy of the GNU General Public License is available on the World Wide Web
at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
MA 02111-1307, USA.
Created 01.06.2006
*******************************************************************************}
program imapserver;
{$mode objfpc}{$H+}
uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils, types, pcmdprometapp, CustApp, uBaseCustomApplication,
  laz_synapse, uBaseDBInterface, uData, uBaseApplication, uBaseDbClasses,
  synautil, ureceivemessage, uMimeMessages, ussmtpserver, usimapserver,
  usimapsearch, mimemess, usbaseserver, uSha1, usimapmailbox,RegExpr, db,
  Utils,uMessages;
type
  TPIMAPServer = class(TBaseCustomApplication)
    function ServerAcceptMail(aSocket: TSTcpThread; aFrom: string;
      aTo: TStrings): Boolean;
    procedure ServerLog(aSocket: TSTcpThread; DirectionIn: Boolean;
      aMessage: string);
    function ServerLogin(aSocket: TSTcpThread; aUser, aPasswort: string
      ): Boolean;
    procedure SMTPServerMailreceived(aSocket: TSTcpThread; aMail: TStrings;
      aFrom: string; aTo: TStrings);
  private
    IMAPServer: TSIMAPServer;
    SMTPServer: TSSMTPServer;
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

  { TPrometMailBox }

  TPrometMailBox = class(TImapMailbox)
  private
    Folder : TMessageList;
    FHighestUID : LongInt;
    function  GetUID( Index: Integer ): LongInt;
    function  GetUIDStr( Index: Integer ): String;
    function  GetIndex( UID: LongInt ): Integer;
  public
    function StrToMsgSet(s: string; UseUID: boolean): TMessageSet;override;
    constructor Create(APath: string);override;
    destructor Destroy; override;
  end;

  TPrometImapServer = class(TSImapServer)
  private
    MailBoxes : TTree;
  public
    function  MBSelect(AThread: TSTcpThread; Mailbox: string; aReadOnly : Boolean ): boolean; override;
    function  MBCreate(AThread: TSTcpThread; Mailbox: string ): boolean; override;
    function  MBDelete(AThread: TSTcpThread; Mailbox: string ): boolean; override;
    function  MBExists(AThread: TSTcpThread; var Mailbox: string ): boolean; override;
    function  MBRename(AThread: TSTcpThread; OldName, NewName: String ): Boolean; override;
    function  MBLogin(AThread: TSTcpThread; var Mailbox: TImapMailbox; Path: String; LINotify : Boolean ): Boolean; override;
    procedure MBLogout(AThread: TSTcpThread; var Mailbox: TImapMailbox; LOSel : Boolean ); override;
    procedure DoSearch(AThread: TSTcpThread; UseUID: Boolean; Par: String ); override;
    procedure DoCopy(AThread: TSTcpThread; MsgSet: TMessageSet; Command, Destination: String );override;
    procedure DoStore(AThread: TSTcpThread; MsgSet: TMessageSet; Command, Par: String );override;
    procedure DoFetch(AThread: TSTcpThread; MsgSet: TMessageSet; Command, Par: String );override;
    procedure DoList(AThread: TSTcpThread; Par: String; LSub: Boolean ); override;
    procedure DoSubscribe(AThread: TSTcpThread; Par: String);override;
    procedure DoUnSubscribe(AThread: TSTcpThread; Par: String);override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

{ TPrometMailBox }

function TPrometMailBox.GetUID(Index: Integer): LongInt;
begin
  Folder.DataSet.First;
  Folder.DataSet.MoveBy(Index);
  Result := SwitchByteOrder(Folder.FieldByName('GRP_ID').AsLongint);
end;

function TPrometMailBox.GetUIDStr(Index: Integer): String;
begin
  Result := IntToStr( GetUID( Index ) );
end;

function TPrometMailBox.GetIndex(UID: LongInt): Integer;
begin
  if Folder.DataSet.Locate('GRP_ID',SwitchByteOrder(UID),[]) then
    Result := Folder.DataSet.RecNo;
end;

function TPrometMailBox.StrToMsgSet(s: string; UseUID: boolean): TMessageSet;
  function SeqNumber(s: string): integer;
  var
    i: LongInt;
  begin
    if UseUID then
      Result := GetUID(Messages - 1)
    else
      Result := Messages;
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
    i, j, Start, Finish: LongInt;
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
      if UseUID then
        Result[j] := GetIndex(i) + 1
      else
        Result[j] := i;
      if (Result[j] > 0) and (Result[j] <= Messages) then
        Inc(j);
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

constructor TPrometMailBox.Create(APath: string);
var
  Tree: TTree;
  aCnt: TDataSet;
begin
  inherited Create(APath);
  Folder := TMessageList.Create(nil);
  Tree := TTree.Create(nil);
  Tree.Filter(Data.QuoteField('NAME')+'='+Data.QuoteValue(APath));
  Folder.SelectByDir(Tree.Id.AsVariant);
  Folder.Open;
  aCnt := Data.GetNewDataSet('select count('+Data.QuoteField('READ')+') as "READ",count(*) as "MESSAGES",max("GRP_ID") as "HUID" from '+Data.QuoteField(Folder.TableName)+' where '+Data.QuoteField('TREEENTRY')+'='+Data.QuoteValue(Tree.Id.AsString));
  aCnt.Open;
  FMessages:=aCnt.FieldByName('MESSAGES').AsInteger;
  FUnseen:=FMessages-aCnt.FieldByName('READ').AsInteger;
  FHighestUID:=aCnt.FieldByName('HUID').AsLongint;
  aCnt.Free;
  Folder.First;
  while not Folder.EOF do
    begin
      if Folder.FieldByName('GRP_ID').IsNull then
        begin
          Folder.Edit;
          Folder.FieldByName('GRP_ID').AsLongint:=FHighestUID+1;
          inc(FHighestUID);
          Folder.Post;
        end;
      Folder.Next;
    end;
end;

destructor TPrometMailBox.Destroy;
begin
  Folder.Free;
  inherited Destroy;
end;

function TPrometImapServer.MBSelect(AThread: TSTcpThread; Mailbox: string;
  aReadOnly: Boolean): boolean;
var
  i: Integer;
begin
  Result:=False;
  MailBoxes.DataSet.Filtered:=False;
  if MailBoxes.Locate('NAME',AnsiToUtf8(Mailbox),[]) then
    begin
      Selected := TPrometMailbox.Create(AnsiToUtf8(Mailbox));
      result := True;
    end
  else if Mailbox='INBOX' then
    begin
      if MailBoxes.Locate('SQL_ID',TREE_ID_MESSAGES,[]) then
        begin
          Selected := TPrometMailbox.Create(MailBoxes.FieldByName('NAME').AsString);
          result := True;
        end;
    end
  else if lowercase(Mailbox)='trash' then
    begin
      if MailBoxes.Locate('SQL_ID',TREE_ID_DELETED_MESSAGES,[]) then
        begin
          Selected := TPrometMailbox.Create(MailBoxes.FieldByName('NAME').AsString);
          result := True;
        end;
    end;
  if Result then
    begin
      SendRes(AThread, IntToStr( Selected.Messages ) + ' EXISTS');
      SendRes(AThread, IntToStr( Selected.Recent ) + ' RECENT');
      i := Selected.Messages - Selected.Unseen + 1;
      if Selected.Unseen > 0 then SendRes(AThread, 'OK [UNSEEN ' + IntToStr(i) + '] First message-number unseen.');
      SendRes(AThread, 'OK [UIDVALIDITY ' + IntToStr( Selected.GetUIDvalidity ) + ']');
      SendRes(AThread, 'FLAGS '+Selected.PossFlags);
      Selected.MBReadOnly := Selected.MBReadOnly OR ReadOnly; //ClientRO //Soll die MB ReadOnly geöffnet werden?
      if not Selected.MBReadOnly then Selected.RemoveRecentFlags;
    end;
end;

function TPrometImapServer.MBCreate(AThread: TSTcpThread; Mailbox: string
  ): boolean;
begin
  Result:=False;
end;

function TPrometImapServer.MBDelete(AThread: TSTcpThread; Mailbox: string
  ): boolean;
begin
  Result:=False;
end;

function TPrometImapServer.MBExists(AThread: TSTcpThread; var Mailbox: string
  ): boolean;
begin
  Result:=False;
end;

function TPrometImapServer.MBRename(AThread: TSTcpThread; OldName,
  NewName: String): Boolean;
begin
  Result:=False;
end;

function TPrometImapServer.MBLogin(AThread: TSTcpThread;
  var Mailbox: TImapMailbox; Path: String; LINotify: Boolean): Boolean;
begin
  Result:=False;
end;

procedure TPrometImapServer.MBLogout(AThread: TSTcpThread;
  var Mailbox: TImapMailbox; LOSel: Boolean);
begin
end;

procedure TPrometImapServer.DoSearch(AThread: TSTcpThread; UseUID: Boolean;
  Par: String);
begin
end;

procedure TPrometImapServer.DoCopy(AThread: TSTcpThread; MsgSet: TMessageSet;
  Command, Destination: String);
begin
end;

procedure TPrometImapServer.DoStore(AThread: TSTcpThread; MsgSet: TMessageSet;
  Command, Par: String);
begin
end;

procedure TPrometImapServer.DoFetch(AThread: TSTcpThread; MsgSet: TMessageSet;
  Command, Par: String);
var
  Success : Boolean;
  SendS, MsgDat  : String;
  i       : Integer;
begin
  Success := True;
  MsgDat := TrimParentheses( Uppercase( Par ) );
  // macros
  StringReplace( MsgDat, 'FAST', 'FLAGS INTERNALDATE RFC822.SIZE', [] );
  StringReplace( MsgDat, 'ALL',  'FLAGS INTERNALDATE RFC822.SIZE ENVELOPE', [] );
  StringReplace( MsgDat, 'FULL', 'FLAGS INTERNALDATE RFC822.SIZE ENVELOPE BODY', [] );
  // Server implementations MUST implicitly include the UID message data item
  // as part of any FETCH response caused by a UID command, regardless of
  // whether a UID was specified as a message data item to the FETCH.
  if ( Command = 'UID FETCH' ) and ( Pos( 'UID', MsgDat ) = 0 ) then
    MsgDat := MsgDat + ' UID';

  for i := 0 to High(MsgSet) do
    begin
      SendS := Selected.Fetch( MsgSet[i]-1, MsgDat, Success );
      if (trim(SendS) <> '') AND Success then SendRes (AThread, SendS )
    end;
  if Success then
    SendResTag(AThread, 'OK ' + Command + ' is now completed' )
  else
    SendResTag(AThread, 'NO ' + Command + ' error' );
end;

procedure TPrometImapServer.DoList(AThread: TSTcpThread; Par: String;
  LSub: Boolean);
  procedure SendList( Txt: String );
  var
    s: String;
  begin
    if LSub then s := 'LSUB (' else s := 'LIST (';
    //TODO:if FileExists2( MailBoxPath + ReplacePathDelimiters( Txt ) + IMAPNOSELECT_FILENAME ) then  s := s + '\'+IMAPNOSELECT_FILENAME;
    //Fuer LSUB nur Subscribed-Folders anwenden
    if (NOT LSub) OR (True{Todo:Subscrbed}) then
      SendRes(AThread, s + ') "' + HierarchyDelimiter + '" "' + Txt + '"' );
  end;

  procedure ScanFolders( RegEx : string; aParent : Int64; Base: String );
  var  SR : TSearchRec;
       Found: String;
  begin
    MailBoxes.DataSet.Filter:='"PARENT"='+IntToStr(aParent);
    MailBoxes.DataSet.Filtered := True;
    MailBoxes.First;
    while not MailBoxes.EOF do
      begin
        Found := Base + Utf8ToAnsi(MailBoxes.FieldByName('NAME').AsString);
        if (uppercase(Found) <> 'INBOX') then
          begin
            if ExecRegExpr(RegEx, Found ) then
              SendList( Found );
            ScanFolders( RegEx, MailBoxes.FieldByName('SQL_ID').AsVariant, Found + HierarchyDelimiter );
          end
        else
          begin
            ScanFolders( RegEx, MailBoxes.FieldByName('SQL_ID').AsVariant, 'INBOX' + HierarchyDelimiter)
          end
       end;
  end;

var
  i : Integer;
  Reference, Mailbox, Pattern, RegEx : String;
begin
  i := PosWhSpace( Par );
  if (par='') or (i=0) then
    begin
      SendResTag(AThread,'BAD missing reference/mailbox parameter!');
      exit;
    end;
  Reference := TrimQuotes( copy( Par, 1, i-1 ) );
  Mailbox   := TrimQuotes( copy( Par, i+1, length(Par) ) );

  if not SafeString( Reference ) then
    begin
      SendResTag(AThread, 'BAD reference parameter contains forbidden characters!' );
      exit;
    end;
  if (Mailbox = '') then
    begin
      if not LSub then
        begin
          SendRes(AThread, 'LIST (\NOSELECT) "' + HierarchyDelimiter + '" "' + Copy( Reference, 1, Pos(HierarchyDelimiter,Reference)- 1 ) + '"' )
        end
      else
        begin
          SendRes(AThread, 'LSUB (\NOSELECT) "' + HierarchyDelimiter + '" "' + Copy( Reference, 1, Pos(HierarchyDelimiter,Reference)- 1 ) + '"' )
        end
    end
  else
    begin
      if not SafeString( Mailbox ) then
        begin
          SendResTag(AThread,'BAD mailbox parameter contains forbidden characters!' );
          exit;
        end;
      Pattern := Reference + Mailbox;
      RegEx := '^';
      for i := 1 to Length( Pattern ) do
        begin
          case Pattern[i] of
            '*' : RegEx := RegEx + '.*';
            '%' : RegEx := RegEx + '[^' + HierarchyDelimiter + ']*';
            '+', '-', '.', '$', '(', ')': RegEx := RegEx+'\'+Pattern[i];
            else  RegEx := RegEx + Pattern[i];
          end
        end;
      RegEx := RegEx + '$';
      if ExecRegExpr(RegEx, 'INBOX' ) then SendList( 'INBOX' );
      ScanFolders( RegEx, 0, '' );
    end;
  SendResTag(AThread, 'OK You have now the List!' )
end;

procedure TPrometImapServer.DoSubscribe(AThread: TSTcpThread; Par: String);
begin
end;

procedure TPrometImapServer.DoUnSubscribe(AThread: TSTcpThread; Par: String);
begin
end;

constructor TPrometImapServer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  MailBoxes := TTree.Create(nil);
  Data.RefreshUsersFilter;
  Data.SetFilter(MailBoxes,Data.QuoteField('TYPE')+'='+Data.QuoteValue('N')+' OR '+Data.QuoteField('TYPE')+'='+Data.QuoteValue('B'),0,'','ASC',False,True,True);
end;

destructor TPrometImapServer.Destroy;
begin
  MailBoxes.Free;
  inherited Destroy;
end;

function TPIMAPServer.ServerAcceptMail(aSocket: TSTcpThread; aFrom: string;
  aTo: TStrings): Boolean;
var
  aRes: Boolean = false;
  aUser: TUser;
  i: Integer;
begin
  aUser := TUser.Create(nil);
  for i := 0 to aTo.Count-1 do
    begin
      if Data.IsSQLDb then
        Data.SetFilter(aUser,'UPPER("EMAIL")=UPPER('''+GetmailAddr(aTo[i])+''')')
      else
        Data.SetFilter(aUser,'"EMAIL"='''+GetEmailAddr(aTo[i])+'''');
      aRes := aRes or (aUser.Count>0);
    end;
  if not aRes then
    begin
      if Data.IsSQLDb then
        Data.SetFilter(aUser,'UPPER("EMAIL")=UPPER('''+GetmailAddr(aFrom)+''')')
      else
        Data.SetFilter(aUser,'"EMAIL"='''+GetmailAddr(afrom)+'''');
      aRes := aRes or (aUser.Count>0);
    end;
  aUser.Free;
  Result := aRes;
end;

procedure TPIMAPServer.ServerLog(aSocket: TSTcpThread; DirectionIn: Boolean;
  aMessage: string);
var
  aID: Integer;
begin
  with Self as IBaseApplication do
    begin
      if aSocket is TSTcpThread then
        aID := TSTcpThread(aSocket).Id;
      if aSocket is TSTcpThread then
        aID := TSTcpThread(aSocket).Id;
      if DirectionIn then
        begin
          Info(IntToStr(aId)+':>'+aMessage);
        end
      else
        begin
          Info(IntToStr(aId)+':<'+aMessage);
        end;
    end;
end;

function TPIMAPServer.ServerLogin(aSocket: TSTcpThread; aUser,
  aPasswort: string): Boolean;
begin
  Result := False;
  Data.Users.DataSet.Refresh;
  with Self as IBaseDBInterface do
    begin
      if Data.Users.DataSet.Locate('LOGINNAME',aUser,[]) or Data.Users.DataSet.Locate('NAME',aUser,[]) then
        begin
          if (Data.Users.CheckPasswort(aPasswort)) then
            Result := True;
        end;
    end;
  with Self as IBaseApplication do
    begin
      if Result then
        Log(IntToStr(TSTcpThread(aSocket).Id)+':Login:'+aUser)
      else
        Error('Login failed:'+aUser);
    end;
end;

procedure TPIMAPServer.SMTPServerMailreceived(aSocket: TSTcpThread;
  aMail: TStrings; aFrom: string; aTo: TStrings);
var
  aUser: TUser;
  i: Integer;
  Found: Boolean = False;
  msg: TMimeMess;
  aUID: String;
  aMessage: TMimeMessage;
begin
  aUser := TUser.Create(nil);
  //mail to an User
  for i := 0 to aTo.Count-1 do
    begin
      if Data.IsSQLDb then
        Data.SetFilter(aUser,'UPPER("EMAIL")=UPPER('''+GetmailAddr(lowercase(aTo[i]))+''')')
      else
        Data.SetFilter(aUser,'"EMAIL"='''+GetmailAddr(lowercase(aTo[i]))+'''');
      if aUser.Count>0 then
        begin
          Data.Users.GotoBookmark(aUser.GetBookmark);
          aMessage := TMimeMessage.Create(nil);
          msg := TMimeMess.Create;
          msg.Lines.Text := aMail.Text;
          msg.DecodeMessage;
          aUID := msg.Header.MessageID;
          ureceivemessage.Init;
          if ureceivemessage.CheckHeader(aUID,msg,aUser.Accountno.AsString) then
            begin
              ureceivemessage.aTreeEntry:=TREE_ID_MESSAGES;
              ureceivemessage.ReceiveMessage(aUID,aMail,aMessage);
            end;
          aMessage.Free;
          msg.Free;
          Found := True;
        end;
    end;
  //mail from an User
  if not Found then
    begin
      if Data.IsSQLDb then
        Data.SetFilter(aUser,'UPPER("EMAIL")=UPPER('''+GetmailAddr(lowercase(aFrom))+''')')
      else
        Data.SetFilter(aUser,'"EMAIL"='''+GetmailAddr(lowercase(afrom))+'''');
      if aUser.Count>0 then
        begin
          aMessage := TMimeMessage.Create(nil);
          msg := TMimeMess.Create;
          msg.Lines.Text := aMail.Text;
          msg.DecodeMessage;
          aUID := msg.Header.MessageID;
          ureceivemessage.Init;
          ureceivemessage.aTreeEntry:=TREE_ID_SEND_MESSAGES;
          ureceivemessage.ReceiveMessage(aUID,aMail,aMessage);
          aMessage.Free;
          msg.Free;
          Found := True;
        end;
    end;
  aUser.Free;
end;

procedure TPIMAPServer.DoRun;
var
  y,m,d,h,mm,s,ss: word;
  aTime: TDateTime;
begin
  with Self as IBaseDBInterface do
    begin
      DBLogout;
      if not Login then
        begin
          writeln('Login failed');
          exit;
        end;
    end;
  IMAPServer := TPrometImapServer.Create(Self);
  IMAPServer.ListenPort := 143;
  if HasOption('port') then
    begin
      IMAPServer.ListenPort := StrToInt(GetOptionValue('port'));
      Info('using port for imap:'+GetOptionValue('port'));
    end;
  if HasOption('imapport') then
    IMAPServer.ListenPort := StrToInt(GetOptionValue('imapport'));
  SMTPServer := TSSMTPServer.Create(Self);
  if GetOptionValue('i','interface')<>'' then
    begin
      IMAPServer.ListenInterface := GetOptionValue('i','interface');
      SMTPServer.ListenInterface := GetOptionValue('i','interface');
      Info('using interface:'+GetOptionValue('i','interface'));
    end;
  SMTPServer.ListenPort := StrToIntDef(GetOptionValue('smtpport'),587);
  if GetOptionValue('smtpport')<>'' then
    begin
      Info('using port for smtp:'+GetOptionValue('smtpport'));
    end;
  SMTPServer.OnLogin :=@ServerLogin;
  if HasOption('server-log') then
    SMTPServer.OnLog:=@ServerLog;
  SMTPServer.OnMailreceived:=@SMTPServerMailreceived;
  SMTPServer.OnAcceptMail:=@ServerAcceptMail;
  if HasOption('server-log') then
    SMTPServer.OnLog:=@ServerLog;
  try
    IMAPServer.Start;
  except
    Error('failed to open IMAP Port '+IntToStr(IMAPServer.ListenPort));
    raise;
  end;
  try
    SMTPServer.Start;
  except
    Error('failed to open SMTP Port '+IntToStr(SMTPServer.ListenPort));
    raise;
  end;
  IMAPServer.OnLogin :=@ServerLogin;
  IMAPServer.OnLog:=@ServerLog;
//  if HasOption('server-log') then
//    IMAPServer.OnDebug:=@ServerLog;
  //IMAPServer.SocketClass:=TPIMAPSocket;
  aTime := Now();
  while not Terminated do
    begin
      sleep(100);
      if (Now()-aTime) > (1/HoursPerDay) then break;
    end;
  // stop program loop
  Terminate;
end;

constructor TPIMAPServer.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=False;
end;

destructor TPIMAPServer.Destroy;
begin
  IMAPServer.Free;
  SMTPServer.Free;
  inherited Destroy;
end;

var
  Application: TPIMAPServer;

{$R *.res}

begin
  Application:=TPIMAPServer.Create(nil);
  Application.Run;
  Application.Free;
end.
