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
  usimapsearch, mimemess, usbaseserver, usimapmailbox,RegExpr, db,
  Utils,uMessages,syncobjs,uPerson,uIntfStrConsts;
type
  { TPrometMailBox }

  TPrometMailBox = class(TImapMailbox)
  private
    FParent : Variant;
    Folder : TMessageList;
    FHighestUID : LongInt;
    FLowestUID : LongInt;
    DBCS : TCriticalSection;
    function GotoIndex(Index : LongInt) : Boolean;
  public
    function  GetUID( Index: LongInt ): LongInt;override;
    function  GetUIDStr( Index: LongInt ): String;override;
    function GetIndex(UID: LongInt): LongInt; override;
    function  SetFlags( Index: LongInt; Flags: TFlagMask ): TFlagMask;override;
    function  GetFlags( Index: LongInt ): TFlagMask;override;
    function  AddFlags( Index: LongInt; Flags: TFlagMask ): TFlagMask;override;
    function  RemoveFlags( Index: LongInt; Flags: TFlagMask ): TFlagMask;override;
    function StrToMsgSet(s: string; UseUID: boolean): TMessageSet;override;
    function  GetTimeStamp( Index: LongInt ): TUnixTime;override;
    function  GetMessage( UID: LongInt ): TMimeMess;override;
    function CopyMessage(MsgSet: TMessageSet; Destination: TImapMailbox): boolean;override;
    function AppendMessage(AThread: TSTcpThread;MsgTxt: string; Flags: string; TimeStamp: TUnixTime): string; override;
    constructor Create(APath: String; CS: TCriticalSection); override;
    destructor Destroy; override;
  end;

  { TPrometImapServer }

  TPrometImapServer = class(TSImapServer)
  private
    MailBoxes : TTree;
    DbCS : TCriticalSection;
    function GotoMailBox(MailBox : string) : Boolean;
  protected
    procedure DoClientCreate(AThread: TSTcpThread); override;
    procedure DoClientDestroy(ASender: TObject); override;
  public
    function  MBSelect(AThread: TSTcpThread; Mailbox: string; aReadOnly : Boolean ): boolean; override;
    function MBGet(AThread: TSTcpThread; Mailbox: string): TImapMailbox; override;
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
    property CS : TCriticalSection read DbCS;
  end;

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
    IMAPServer: TPrometImapServer;
    SMTPServer: TSSMTPServer;
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

{ TPrometMailBox }

function TPrometMailBox.GotoIndex(Index: LongInt): Boolean;
begin
  if Index <> Folder.DataSet.RecNo then
    begin
      Folder.DataSet.First;
      Folder.DataSet.MoveBy(Index-1);
    end;
  Result := True;
end;

function TPrometMailBox.GetUID(Index: LongInt): LongInt;
begin
  GotoIndex(Index);
  Result := Folder.FieldByName('GRP_ID').AsLongint;
end;

function TPrometMailBox.GetUIDStr(Index: LongInt): String;
begin
  Result := IntToStr( GetUID( Index ) );
end;

function TPrometMailBox.GetIndex(UID: LongInt): LongInt;
begin
  Result := -1;
  if Folder.DataSet.Locate('GRP_ID',UID,[]) then
    Result := Folder.DataSet.RecNo;
end;

function TPrometMailBox.SetFlags(Index: LongInt; Flags: TFlagMask): TFlagMask;
begin
  Result := 0;
  if GotoIndex(Index) then
    begin
      Folder.Edit;
      Folder.FieldByName('GRP_FLAGS').Clear;
      if Flags and FLAGSEEN = FLAGSEEN then
        Folder.FieldByName('READ').AsString:='Y'
      else Folder.FieldByName('READ').AsString:='N';

      if (Flags and FLAGANSWERED = FLAGANSWERED) and (Folder.FieldByName('ANSWERED').IsNull) then
        Folder.FieldByName('ANSWERED').AsDateTime:=Now()
      else Folder.FieldByName('ANSWERED').Clear;

      if Flags and FLAGFLAGGED = FLAGFLAGGED then
        Folder.FieldByName('FLAGGED').AsString:='Y'
      else Folder.FieldByName('FLAGGED').AsString:='N';

      if Flags and FLAGDRAFT = FLAGDRAFT then
        Folder.FieldByName('DRAFT').AsString:='Y'
      else Folder.FieldByName('DRAFT').AsString:='N';
      Result := GetFlags(Index);
    end;
end;

function TPrometMailBox.GetFlags(Index: LongInt): TFlagMask;
var
  MR: TFlagMask;
begin
  GotoIndex(Index);
  if Folder.FieldByName('GRP_FLAGS').IsNull then
    begin
      FillChar( MR, sizeof(MR), 0 );
      if Folder.FieldByName('READ').AsString='Y' then
        MR := MR or FLAGSEEN;
      if not Folder.FieldByName('ANSWERED').IsNull then
        MR := MR or FLAGANSWERED;
      if Folder.FieldByName('FLAGGED').AsString='Y' then
        MR := MR or FLAGFLAGGED;
      if Folder.FieldByName('DRAFT').AsString='Y' then
        MR := MR or FLAGDRAFT;
      Folder.Edit;
      Folder.FieldByName('GRP_FLAGS').AsInteger := MR;
      Folder.Post;
    end
  else
    MR := Folder.FieldByName('GRP_FLAGS').AsInteger;
  Result := MR;
end;

function TPrometMailBox.AddFlags(Index: LongInt; Flags: TFlagMask): TFlagMask;
begin
  Result := SetFlags( Index, GetFlags(Index) or Flags )
end;

function TPrometMailBox.RemoveFlags(Index: LongInt; Flags: TFlagMask
  ): TFlagMask;
begin
  Result := SetFlags( Index, GetFlags(Index) and not Flags )
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
    if UseUID and (Result < FLowestUID) then
      Result := FLowestUID;
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
    SetLength(Result,100);
    j := 0;
    for i := Start to Finish do
    begin
      if length(Result)<j+1 then
        Setlength(Result,j+100);
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

function TPrometMailBox.GetTimeStamp(Index: LongInt): TUnixTime;
begin
  if Index <> Folder.DataSet.RecNo then
    begin
      Folder.DataSet.First;
      Folder.DataSet.MoveBy(Index);
    end;
  Result := DateTimeToUnixTime(Folder.FieldByName('SENDDATE').AsDateTime);
end;

function TPrometMailBox.GetMessage(UID: LongInt): TMimeMess;
var
  aMessage: TMimeMessage;
begin
  DBCS.Enter;
  aMessage := TMimeMessage.Create(nil);
  aMessage.SelectByGrpID(UID,FParent);
  aMessage.Open;
  Result := aMessage.EncodeMessage;
  Result.EncodeMessage;
  aMessage.Free;
  DBCS.Leave;
end;

function TPrometMailBox.CopyMessage(MsgSet: TMessageSet;
  Destination: TImapMailbox): boolean;
var  i: Integer;
     FileNameS,FileNameD: String;
     listFileNameD: TStringList;
     nUIDnext: LongInt;
     aMessage: TMimeMessage;
begin
  if Destination.MBReadOnly then begin
    Result := false;
    exit
  end;
  Result := True;
  Lock;
  try
    for i := 0 to High(MsgSet) do
      begin
        if not GotoIndex(MsgSet[i]) then
          begin
            Result := False;
            exit;
          end;
        //TODO:copy the Msg real at ime wo only change the TREEENTRY couse mostly the messages will not be copied but moved
        aMessage := TMimeMessage.Create(nil);
        aMessage.SelectByGrpID(GetUID(MsgSet[i]),FParent);
        aMessage.Open;
        aMessage.Edit;
        aMessage.FieldByName('TREEENTRY').AsVariant:=FParent;
        aMessage.Post;
        aMessage.Free;
      end;
  finally
    Unlock;
  end;
  Destination.SendMailboxUpdate;
end;

function TPrometMailBox.AppendMessage(AThread: TSTcpThread; MsgTxt: string;
  Flags: string; TimeStamp: TUnixTime): string;
var
  aMsg: TMimeMess;
  aArticle: TStringList;
  aID: String;
  i: Integer;
  aChr: Char;
  aMessage: TMimeMessage;
  atmp: String;
  CustomerCont: TPersonContactData;
  Customers: TPerson;
begin
  DBCS.Enter;
  Result:=inherited;
  try
    aMsg := TMimeMess.Create;
    aArticle := TStringList.Create;
    aArticle.Text:=MsgTxt;
    aMsg.Lines.Assign(aArticle);
    aArticle.Free;
    aMsg.DecodeMessage;
    if pos('@email.android.com',aMsg.Header.MessageID)>0 then
      begin
        //Android internal client fails on updating internaldate, dont let them store anything
        aMsg.Free;
        exit;
      end;
    if aMsg.Header.MessageID='' then
      begin
        randomize;
        aID := '';
        for i := 0 to 45 do
          begin
            aChr := chr(ord('0')+random(74));
            if aChr in ['a'..'z','0'..'9','A'..'Z'] then
              aID := aID+aChr;
          end;
        aMsg.Header.MessageID := aID;
      end;
    aMessage := TMimeMessage.Create(nil);
    amessage.Filter(Data.QuoteField('ID')+'='+Data.QuoteValue(aMsg.Header.MessageID)+' and '+Data.QuoteField('TREEENTRY')+'='+Data.QuoteValue(FParent));
    if aMessage.Count=0 then
      begin
        aMessage.Insert;
        aMessage.FieldByName('ID').Clear;
        aMessage.Dataset.FieldByName('USER').AsString := AThread.User;
        aMessage.Dataset.FieldByName('TYPE').AsString := 'EMAIL';
        aMessage.Dataset.FieldByName('READ').AsString := 'N';
        aMessage.DecodeMessage(aMsg);
        aMessage.FieldbyName('TREEENTRY').AsVariant := FParent;
        aSubject := SysToUni(amsg.Header.Subject);
        atmp:=SysToUni(getemailaddr(aMsg.Header.From));
        CustomerCont := TPersonContactData.CreateEx(nil);
        if Data.IsSQLDb then
          Data.SetFilter(CustomerCont,'UPPER("DATA")=UPPER('''+atmp+''')')
        else
          Data.SetFilter(CustomerCont,'"DATA"='''+atmp+'''');
        if CustomerCont.Count=0 then
          begin
            atmp := copy(aMsg.Header.From,0,pos('<',aMsg.Header.From)-1);
            if Data.IsSQLDb then
              Data.SetFilter(CustomerCont,'UPPER("DATA")=UPPER('''+atmp+''')')
            else
              Data.SetFilter(CustomerCont,'"DATA"='''+atmp+'''');
            if (CustomerCont.Count=0) then
              begin
                if copy(atmp,0,1)='+' then
                  atmp := '0'+copy(atmp,3,length(atmp));
                if Data.IsSQLDb then
                  Data.SetFilter(CustomerCont,'UPPER("DATA")=UPPER('''+atmp+''')')
                else
                  Data.SetFilter(CustomerCont,'"DATA"='''+atmp+'''');
              end;
          end;
        Customers := TPerson.Create(nil);
        Data.SetFilter(Customers,'"ACCOUNTNO"='+Data.QuoteValue(CustomerCont.DataSet.FieldByName('ACCOUNTNO').AsString));
        CustomerCont.Free;
        if Customers.Count > 0 then
          begin
            Customers.History.Open;
            Customers.History.AddItem(Customers.DataSet,Format(strActionMessageReceived,[aSubject]),
                                      'MESSAGEIDX@'+aMessage.FieldByName('ID').AsString+'{'+aSubject+'}',
                                      '',
                                      nil,
                                      ACICON_MAILNEW);
            Customers.History.Edit;
            with Customers.History.DataSet as IBaseManageDB do
              UpdateStdFields := False;
            with Customers.History.DataSet as IBaseManageDB do
              UpdateStdFields := True;
            Customers.History.Post;
          end;
        aMessage.DataSet.Post;
        Result := 'OK APPEND completed';
      end
    else
      begin
        aMessage.Edit;
        aMessage.FieldByName('TIMESTAMPD').AsDateTime:=Now();
        aMessage.Post;
        Result := 'OK APPEND completed';
      end;
    aMsg.Free;
    aMessage.Destroy;
  except
    Result := 'NO APPEND error';
  end;
  DBCS.Leave;
end;

constructor TPrometMailBox.Create(APath: String; CS: TCriticalSection);
var
  Tree: TTree;
  aCnt: TDataSet;
  aFilter: String;
begin
  inherited Create(APath,CS);
  DBCS := CS;
  Folder := TMessageList.Create(nil);
  DBCS.Enter;
  Tree := TTree.Create(nil);
  Tree.Select(APath);
  Tree.Open;
  FParent := APath;
  aFilter := Data.QuoteField('TREEENTRY')+'='+Data.QuoteValue(Tree.Id.AsString)+' and '+Data.QuoteField('USER')+'='+Data.QuoteValue(Data.Users.Accountno.AsString);
  Folder.SortFields:='GRP_ID,MSG_ID';
  Folder.SortDirection:=sdAscending;
  Folder.Filter(aFilter);
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
  aCnt := Data.GetNewDataSet('select count('+Data.QuoteField('READ')+') as "READ",count(*) as "MESSAGES",max("GRP_ID") as "HUID", min("GRP_ID") as "MUID" from '+Data.QuoteField(Folder.TableName)+' where '+aFilter);
  aCnt.Open;
  DBCS.Leave;
  FMessages:=aCnt.FieldByName('MESSAGES').AsInteger;
  FUnseen:=FMessages-aCnt.FieldByName('READ').AsInteger;
  FHighestUID:=aCnt.FieldByName('HUID').AsLongint;
  FLowestUID:=aCnt.FieldByName('MUID').AsLongint;
  aCnt.Free;
end;

destructor TPrometMailBox.Destroy;
begin
  Folder.Free;
  inherited Destroy;
end;

function TPrometImapServer.GotoMailBox(MailBox: string): Boolean;
begin
  Result := False;
  MailBox:=copy(MailBox,RPos('/',MailBox)+1,length(MailBox));
  if MailBoxes.Locate('NAME',AnsiToUtf8(Mailbox),[]) then
    begin
      result := True;
    end
  else if lowercase(Mailbox)='inbox' then
    begin
      if MailBoxes.Locate('SQL_ID',TREE_ID_MESSAGES,[]) then
        begin
          result := True;
        end;
    end
  else if lowercase(Mailbox)='sent' then
    begin
      if MailBoxes.Locate('SQL_ID',TREE_ID_SEND_MESSAGES,[]) then
        begin
          result := True;
        end;
    end
  else if lowercase(Mailbox)='trash' then
    begin
      if MailBoxes.Locate('SQL_ID',TREE_ID_DELETED_MESSAGES,[]) then
        begin
          result := True;
        end;
    end;
end;

procedure TPrometImapServer.DoClientCreate(AThread: TSTcpThread);
begin
  inherited DoClientCreate(AThread);
  LogRaw(AThread,'Client created '+IntToStr(AThread.Id));
end;

procedure TPrometImapServer.DoClientDestroy(ASender: TObject);
begin
  LogRaw(TSTcpThread(ASender),'Client destroyed '+IntToStr(TSTcpThread(ASender).Id));
  inherited DoClientDestroy(ASender);
end;

function TPrometImapServer.MBSelect(AThread: TSTcpThread; Mailbox: string;
  aReadOnly: Boolean): boolean;
var
  i: Integer;
begin
  Result:=False;
  if GotoMailBox(Mailbox) then
    begin
      TSImapThread(AThread).Selected := TPrometMailbox.Create(MailBoxes.Id.AsVariant,DBCS);
      Result := True;
    end;
  if Result then
    begin
      SendRes(AThread, IntToStr( TSImapThread(AThread).Selected.Messages ) + ' EXISTS');
      SendRes(AThread, IntToStr( TSImapThread(AThread).Selected.Recent ) + ' RECENT');
      i := TSImapThread(AThread).Selected.Messages - TSImapThread(AThread).Selected.Unseen + 1;
      if TSImapThread(AThread).Selected.Unseen > 0 then SendRes(AThread, 'OK [UNSEEN ' + IntToStr(i) + '] First message-number unseen.');
      SendRes(AThread, 'OK [UIDVALIDITY ' + IntToStr( TSImapThread(AThread).Selected.GetUIDvalidity ) + ']');
      SendRes(AThread, 'FLAGS '+TSImapThread(AThread).Selected.PossFlags);
      TSImapThread(AThread).Selected.MBReadOnly := TSImapThread(AThread).Selected.MBReadOnly OR ReadOnly; //ClientRO //Soll die MB ReadOnly geöffnet werden?
      if not TSImapThread(AThread).Selected.MBReadOnly then TSImapThread(AThread).Selected.RemoveRecentFlags;
    end;
end;

function TPrometImapServer.MBGet(AThread: TSTcpThread; Mailbox: string
  ): TImapMailbox;
begin
  Result := nil;
  if GotoMailBox(Mailbox) then
    begin
      Result := TPrometMailbox.Create(MailBoxes.Id.AsVariant,DBCS);
    end;
end;

function TPrometImapServer.MBCreate(AThread: TSTcpThread; Mailbox: string
  ): boolean;
begin
  DbCS.Enter;
  try
    MailBoxes.Append;
    MailBoxes.Text.AsString:=Mailbox;
    MailBoxes.FieldByName('TYPE').AsString:='N';
    MailBoxes.Post;
    Result:=True;
  except
    Result:=False;
  end;
  DbCS.Leave;
end;

function TPrometImapServer.MBDelete(AThread: TSTcpThread; Mailbox: string
  ): boolean;
begin
  Result:=False;
end;

function TPrometImapServer.MBExists(AThread: TSTcpThread; var Mailbox: string
  ): boolean;
begin
  Result := GotoMailBox(Mailbox);
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
  if GotoMailBox(Path) then
    begin
      Mailbox := TPrometMailbox.Create(MailBoxes.Id.AsVariant,DBCS);
      Result := True;
    end;
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
var
  DestMailbox : TImapMailbox;
begin
  if not Assigned(TSImapThread(AThread).Selected) then
    begin
      SendResTag(AThread, 'NO no Mailbox selected.');
      exit;
    end;
  if TSImapThread(AThread).Selected.MBReadOnly then begin
     SendResTag(AThread,'NO selected mailbox is read-only.');
     exit;
  end;
  DestMailBox := NIL;
  if not MBExists(AThread, Destination ) then begin
    SendResTag(AThread, 'NO [TRYCREATE] ' + Command + ' error: destination mailbox not known' );
    exit
  end;

  if not MBLogin(AThread, DestMailbox, Destination, false ) then begin
    SendResTag(AThread, 'NO ' + Command + ' error: can''t open destination mailbox' );
    exit
  end;

  try
    if TSImapThread(AThread).Selected.CopyMessage( MsgSet, DestMailbox ) then begin
       SendResTag(AThread, 'OK ' + Command + ' completed' );
    end else
       SendResTag(AThread, 'NO ' + Command + ' error: can''t copy messages' );
    MBLogout(AThread, DestMailbox, false );
  except
    on E:Exception do
      with BaseApplication as IBaseApplication do
        Error(Format('IMAP-server - Error on DoCopy: %s', [E.Message]));
  end
end;

procedure TPrometImapServer.DoStore(AThread: TSTcpThread; MsgSet: TMessageSet;
  Command, Par: String);
var  i: integer;
     MsgDat: string;
     Flags, NewFlags: String;
     Silent : Boolean;
     Mode : TStoreMode;
begin
  if not Assigned(TSImapThread(AThread).Selected) then
    begin
      SendResTag(AThread,'NO no Mailbox selected.');
      exit;
    end;
  if TSImapThread(AThread).Selected.MBReadOnly then begin
     SendResTag(AThread,'NO selected mailbox is read-only.');
     exit;
  end;

  i := PosWhSpace( Par );
  MsgDat := Uppercase( TrimQuotes( copy( Par, 1, i ) ) );
  Flags  := Uppercase( TrimParentheses( copy( Par, i+1, length(Par)-i ) ) ); {MG}{Imap-Store}

  if not TSImapThread(AThread).Selected.AreValidFlags(Flags) then begin
     SendResTag(AThread,'NO The \Recent flag may not used as an argument in STORE!');
     exit;
  end;

  i := pos( '.', Msgdat);
  if (i>0) and (copy( MsgDat, i, 7 )='.SILENT') then begin
     Silent := True;
     MsgDat := copy( MsgDat, 1, i-1 );
  end else
     Silent := False;

  if      MsgDat = 'FLAGS'  then Mode := [smReplace]
  else if MsgDat = '+FLAGS' then Mode := [smAdd]
  else if MsgDat = '-FLAGS' then Mode := [smDelete]
  else begin
     SendResTag(AThread,'NO ' + Command + ' with unknown message-data!');
     exit;
  end;

  for i := 0 to High(MsgSet) do begin
     NewFlags := TSImapThread(AThread).Selected.Store( MsgSet[i]-1, Flags, Mode );
     if not Silent then SendRes(AThread, IntToStr(MsgSet[i]) + ' FETCH (FLAGS ' +
                                 NewFlags + ')' );
  end;
  SendResTag(AThread,'OK you''ve stored your flags now!')
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
  DisableLog;
  for i := 0 to High(MsgSet) do
    begin
      SendS := TSImapThread(AThread).Selected.Fetch( MsgSet[i]-1, MsgDat, Success );
      if (trim(SendS) <> '') AND Success then SendRes (AThread, SendS )
    end;
  EnableLog;
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
       aMailBoxes : TTree;
  begin
    aMailBoxes := TTree.Create(nil);
    DbCS.Enter;
    Data.SetFilter(aMailBoxes,'('+Data.QuoteField('TYPE')+'='+Data.QuoteValue('N')+' OR '+Data.QuoteField('TYPE')+'='+Data.QuoteValue('B')+') AND '+Data.QuoteField('PARENT')+'='+Data.QuoteValue(IntToStr(aParent)),0,'','ASC',False,True,True);
    DbCS.Leave;
    aMailBoxes.First;
    while not aMailBoxes.EOF do
      begin
        if aMailBoxes.Id.AsVariant = TREE_ID_MESSAGES then
          Found := Base + 'INBOX'
        else if aMailBoxes.Id.AsVariant = TREE_ID_DELETED_MESSAGES then
          Found := Base + 'Trash'
        else if aMailBoxes.Id.AsVariant = TREE_ID_SEND_MESSAGES then
          Found := Base + 'Sent'
        else
          Found := Base + Utf8ToAnsi(aMailBoxes.FieldByName('NAME').AsString);
        if ExecRegExpr(RegEx, Found ) then
          SendList( Found );
        ScanFolders( RegEx, aMailBoxes.FieldByName('SQL_ID').AsVariant, Found + HierarchyDelimiter );
        aMailBoxes.Next;
      end;
    aMailBoxes.Free;
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
      ScanFolders( RegEx, 0, '' );
    end;
  if not LSub then
    SendResTag(AThread, 'OK LIST completed.' )
  else
    SendResTag(AThread, 'OK Lsub completed.' );
end;

procedure TPrometImapServer.DoSubscribe(AThread: TSTcpThread; Par: String);
var
  Mailbox: String;
begin
  Mailbox := uppercase( CutFirstParam(Par) );
  if (Mailbox = '') or (not SafeString(Mailbox)) then
    SendResTag(AThread, 'BAD SUBSCRIBE without valid mailbox!' )
  else
    begin
      if not GotoMailBox(Mailbox) then
        begin
          SendResTag(AThread, 'BAD Mailbox not valid!' );
          exit;
        end;

    end;
end;

procedure TPrometImapServer.DoUnSubscribe(AThread: TSTcpThread; Par: String);
var
  Mailbox: String;
begin
  Mailbox := uppercase( CutFirstParam(Par) );
  if (Mailbox = '') or (not SafeString(Mailbox)) then
    SendResTag(AThread, 'BAD SUBSCRIBE without valid mailbox!' )
  else
    begin
      if not GotoMailBox(Mailbox) then
        begin
          SendResTag(AThread, 'BAD Mailbox not valid!' );
          exit;
        end;
      SendResTag(AThread, 'BAD not implemented!' )
    end;
end;

constructor TPrometImapServer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  DBCs := TCriticalSection.Create;
  MailBoxes := TTree.Create(nil);
  Data.SetFilter(MailBoxes,Data.QuoteField('TYPE')+'='+Data.QuoteValue('N')+' OR '+Data.QuoteField('TYPE')+'='+Data.QuoteValue('B'),0,'','ASC',False,True,False);
end;

destructor TPrometImapServer.Destroy;
begin
  DBCS.Free;
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
  IMAPServer.CS.Enter;
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
  if Result then
    begin
      Data.RefreshUsersFilter;
    end;
  if Result then
    aSocket.User:=Data.Users.Accountno.AsString;
  IMAPServer.CS.Leave;
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
  if HasOption('server-log') then
    IMAPServer.OnLog:=@ServerLog;
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
  with BaseApplication as IBaseApplication do
    begin
      AppVersion:={$I ../base/version.inc};
      AppRevision:={$I ../base/revision.inc};
    end;
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
