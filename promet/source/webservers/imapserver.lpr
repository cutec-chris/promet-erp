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
  Utils,uMessages,syncobjs,uPerson,uIntfStrConsts,uBaseDatasetInterfaces,
  variants,synachar;
type
  { TPrometMailBox }

  TPrometMailBox = class(TImapMailbox)
  private
    FParent : Variant;
    Folder : TMessageList;
    Connection : TComponent;
    FHighestUID : Int64;
    FLowestUID : Int64;
    FLockedFrom : string;
    _DBCS : TCriticalSection;
    function GotoIndex(Index : LongInt) : Boolean;
  public
    function  GetUID( Index: LongInt ): LongInt;override;
    function  GetUIDStr( Index: LongInt ): String;override;
    function GetIndex(UID: LongInt): LongInt; override;
    procedure InternalSetFlags(aFolder: TMessageList; Flags: TFlagMask);
    function  SetFlags( Index: LongInt; Flags: TFlagMask ): TFlagMask;override;
    function  GetFlags( Index: LongInt ): TFlagMask;override;
    function  AddFlags( Index: LongInt; Flags: TFlagMask ): TFlagMask;override;
    function  RemoveFlags( Index: LongInt; Flags: TFlagMask ): TFlagMask;override;
    function StrToMsgSet(s: string; UseUID: boolean): TMessageSet;override;
    function  GetTimeStamp( Index: LongInt ): TUnixTime;override;
    function GetUIDnext: LongInt; override;
    function  GetMessage( UID: LongInt;HeaderOnly : Boolean ): TMimeMess;override;
    function CopyMessage(MsgSet: TMessageSet; Destination: TImapMailbox): boolean;override;
    function AppendMessage(AThread: TSTcpThread;MsgTxt: string; Flags: string; TimeStamp: TUnixTime): string; override;
    function FindContent(MsgSet: TMessageSet; After, Before: int64; Charset: string; HeaderList, BodyStrings,  TextStrings: TStringList): TMessageSet;override;
    procedure InternalLock(WhoAmI : string);
    procedure InternalUnlock(WhoAmI : string);
    procedure RefreshFolder(aThread : TSTcpThread);
    constructor Create(aThread : TSTcpThread;APath: String; CS: TCriticalSection); override;
    destructor Destroy; override;
  end;

  { TPrometImapServer }

  TPrometImapServer = class(TSImapServer)
  private
    MailBoxes : TTree;
    _DbCS : TCriticalSection;
    _LockFrom : TSTcpThread;
    _LockName : string;
    function GotoMailBox(MailBox: string; Socket: TSTcpThread): Boolean;
  protected
    procedure DoClientCreate(AThread: TSTcpThread); override;
    procedure DoClientDestroy(ASender: TObject); override;
    procedure SocketRelease(ASocket: TSTcpThread); override;
  public
    function MBSelect(AThread: TSTcpThread; Mailbox: string; aReadOnly: Boolean): boolean; override;
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
    procedure InternalLock(WhoAmI : string;Socket : TSTcpThread);
    procedure InternalUnlock(WhoAmI : string;Socket : TSTcpThread);
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
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

procedure TPrometMailBox.InternalLock(WhoAmI: string);
var
  i: Integer;
begin
  with BaseApplication as IBaseApplication do
    Debug('  Locking entered from '+WhoAmI);
  exit;
  if not _DBCS.TryEnter then
    begin
      with BaseApplication as IBaseApplication do
        Debug('already Locked from '+FLockedFrom+' !');
      for i := 0 to 5000 do
        begin
          sleep(1);
          if _DBCS.TryEnter then
            exit;
        end;
      if not _DBCS.TryEnter then
        raise Exception.Create('Lock after 5secs not released !!');
    end
  else FLockedFrom:=WhoAmI;
end;

procedure TPrometMailBox.InternalUnlock(WhoAmI: string);
begin
  with BaseApplication as IBaseApplication do
    Debug('UnLocking entered from '+WhoAmI);
  exit;
  _DBCS.Leave;
end;

function TPrometMailBox.GetUID(Index: LongInt): LongInt;
begin
  if GotoIndex(Index) then
    Result := Folder.FieldByName('GRP_ID').AsLongint
  else Result:=-1;
end;

function TPrometMailBox.GetUIDStr(Index: LongInt): String;
begin
  Result := IntToStr( GetUID( Index ) );
  if Result='-1' then Result := '';
end;

function TPrometMailBox.GetIndex(UID: LongInt): LongInt;
begin
  Result := -1;
  if not Assigned(Folder) then exit;
  InternalLock('GetIndex');
  if Folder.DataSet.Locate('GRP_ID',UID,[]) then
    Result := Folder.DataSet.RecNo;
  with BaseApplication as IBaseApplication do
    Debug('GetIndex('+IntToStr(UID)+')='+IntToStr(result));
  InternalUnlock('GetIndex');
end;

procedure TPrometMailBox.InternalSetFlags(aFolder: TMessageList;
  Flags: TFlagMask);
begin
  aFolder.Edit;
  aFolder.FieldByName('GRP_FLAGS').Clear;
  if Flags and FLAGSEEN = FLAGSEEN then
    aFolder.FieldByName('READ').AsString:='Y'
  else aFolder.FieldByName('READ').AsString:='N';

  if (Flags and FLAGANSWERED = FLAGANSWERED) and (Folder.FieldByName('ANSWERED').IsNull) then
    aFolder.FieldByName('ANSWERED').AsDateTime:=Now()
  else aFolder.FieldByName('ANSWERED').Clear;

  if Flags and FLAGFLAGGED = FLAGFLAGGED then
    aFolder.FieldByName('FLAGGED').AsString:='Y'
  else aFolder.FieldByName('FLAGGED').AsString:='N';

  if Flags and FLAGDRAFT = FLAGDRAFT then
    aFolder.FieldByName('DRAFT').AsString:='Y'
  else aFolder.FieldByName('DRAFT').AsString:='N';
  if Flags and FLAGDELETED = FLAGDELETED then
    aFolder.FieldByName('TREEENTRY').AsVariant:=TREE_ID_DELETED_MESSAGES;
end;

function TPrometMailBox.SetFlags(Index: LongInt; Flags: TFlagMask): TFlagMask;
begin
  Result := 0;
  if GotoIndex(Index) then
    begin
      InternalLock('SetFlags');
      InternalSetFlags(Folder,Flags);
      InternalUnlock('SetFlags');
      Result := GetFlags(Index);
    end;
end;

function TPrometMailBox.GetFlags(Index: LongInt): TFlagMask;
var
  MR: TFlagMask;
begin
  if GotoIndex(Index) then
    begin
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
          if Folder.FieldByName('TREEENTRY').AsVariant=TREE_ID_DELETED_MESSAGES then MR := MR or FLAGDELETED;
          InternalLock('GetFlags');
          Folder.Edit;
          Folder.FieldByName('GRP_FLAGS').AsInteger := MR;
          Folder.Post;
          InternalUnlock('GetFlags');
        end
      else
        MR := Folder.FieldByName('GRP_FLAGS').AsInteger;
      Result := MR;
    end
  else Result := 0;
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
      Result := GetUID(Messages)
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
    SetLength(Result,100);
    j := 0;
    if UseUID then
      begin
        if Start < FLowestUID then
          Start := FLowestUID;
        if Finish>FHighestUID then
          Finish:=FHighestUID;
      end;
    for i := Start to Finish do
    begin
      if length(Result)<j+1 then
        Setlength(Result,j+1000);
      if UseUID then
        Result[j] := GetIndex(i) + 1
      else
        Result[j] := i;
      if (Result[j] > 0) and (Result[j] <= Messages+1) then
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

function TPrometMailBox.GetUIDnext: LongInt;
begin
  Result := FHighestUID+1;
end;

function TPrometMailBox.GetMessage(UID: LongInt; HeaderOnly: Boolean
  ): TMimeMess;
var
  aMessage: TMimeMessage;
begin
  InternalLock('GetMessage '+IntToStr(UID));
  try
    aMessage := TMimeMessage.Create(nil);
    aMessage.SelectByGrpID(UID,FParent);
    aMessage.Open;
    if aMessage.Count>0 then
      Result := aMessage.EncodeMessage(HeaderOnly)
    else Result := nil;
    aMessage.Free;
  finally
    InternalUnlock('GetMessage '+IntToStr(UID));
  end;
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
  InternalLock('CopyMessage');
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
        if aMessage.Count>0 then
          begin
            aMessage.Edit;
            aMessage.FieldByName('TREEENTRY').AsVariant:=FParent;
            aMessage.Post;
          end;
        FreeAndNil(aMessage);
      end;
  finally
    Unlock;
    InternalUnlock('CopyMessage');
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
  InternalLock('AppendMessage');
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
    Data.StartTransaction(aMessage.Connection);
    amessage.Filter(Data.QuoteField('ID')+'='+Data.QuoteValue(aMsg.Header.MessageID)+' and '+Data.QuoteField('TREEENTRY')+'='+Data.QuoteValue(FParent));
    if aMessage.Count=0 then
      begin
        aMessage.Insert;
        aMessage.FieldByName('ID').Clear;
        aMessage.Dataset.FieldByName('USER').AsString := AThread.User;
        aMessage.Dataset.FieldByName('TYPE').AsString := 'EMAIL';
        aMessage.Dataset.FieldByName('READ').AsString := 'N';
        aMessage.FieldByName('SENDDATE').AsDateTime:=UnixTimeToDateTime(TimeStamp);
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
            Customers.History.AddItem(Customers.DataSet,Format(strActionMessageReceived,[copy(aSubject,0,100)]),
                                      'MESSAGEIDX@'+aMessage.FieldByName('ID').AsString+'{'+copy(aSubject,0,100)+'}',
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
        Result := 'OK APPEND completed.';
      end
    else
      begin
        aMessage.Edit;
        aMessage.FieldByName('TIMESTAMPD').AsDateTime:=Now();
        aMessage.Post;
        Result := 'OK APPEND completed.';
      end;
    aMsg.Free;
    InternalSetFlags(aMessage,StringToFlagMask(Flags));
    aMessage.Post;
    Data.CommitTransaction(aMessage.Connection);
    FreeAndNil(aMessage);
  except
    Result := 'NO APPEND error';
    if Assigned(aMessage) then
      begin
        aMessage.CascadicCancel;
        FreeandNil(aMessage);
      end;
    Data.RollbackTransaction(aMessage.Connection);
  end;
  InternalUnlock('AppendMessage');
  //RefreshFolder(AThread);
end;

function TPrometMailBox.FindContent(MsgSet: TMessageSet; After, Before: int64;
  Charset: string; HeaderList, BodyStrings, TextStrings: TStringList
  ): TMessageSet;
function GetCharset( ContentType: String ): String;
var  i : Integer;
begin
     Result := '';
     ContentType := UpperCase( ContentType );
     i := Pos( 'CHARSET=', ContentType );
     if i > 0 then begin
        System.Delete( ContentType, 1, i+7 );
        Result := UpperCase( QuotedStringOrToken( ContentType ) )
     end
end;
function Has8BitChar( txt: String ): Boolean;
var  i: Integer;
begin
     Result := True;
     for i := 1 to Length( txt ) do if Ord( txt[i] ) > 127 then exit;
     Result := False
end;
function RfcTimezoneToBiasMinutes( RfcTimeZone: TRfcTimeZone ): Integer;
begin
   Result := 0;
   if RfcTimeZone='' then exit;

   if RfcTimeZone[1] in [ '+', '-' ] then begin

      Result := strtointdef( copy(RfcTimeZone,2,2), 0 ) * 60
              + strtointdef( copy(RfcTimeZone,4,2), 0 );
      if (Result<0) or (Result>=24*60) then Result:=0;
      if RfcTimeZone[1]='+' then Result:=-Result;

   end else begin

      RfcTimeZone := UpperCase( RfcTimeZone );

      if      RfcTimeZone='GMT' then Result:=  0
      else if RfcTimeZone='UT'  then Result:=  0

      else if RfcTimeZone='EST' then Result:= -5*60
      else if RfcTimeZone='EDT' then Result:= -4*60
      else if RfcTimeZone='CST' then Result:= -6*60
      else if RfcTimeZone='CDT' then Result:= -5*60
      else if RfcTimeZone='MST' then Result:= -7*60
      else if RfcTimeZone='MDT' then Result:= -6*60
      else if RfcTimeZone='PST' then Result:= -8*60
      else if RfcTimeZone='PDT' then Result:= -7*60

      else if RfcTimeZone='A'   then Result:= -1*60
      else if RfcTimeZone='B'   then Result:= -2*60
      else if RfcTimeZone='C'   then Result:= -3*60
      else if RfcTimeZone='D'   then Result:= -4*60
      else if RfcTimeZone='E'   then Result:= -5*60
      else if RfcTimeZone='F'   then Result:= -6*60
      else if RfcTimeZone='G'   then Result:= -7*60
      else if RfcTimeZone='H'   then Result:= -8*60
      else if RfcTimeZone='I'   then Result:= -9*60
      else if RfcTimeZone='K'   then Result:=-10*60
      else if RfcTimeZone='L'   then Result:=-11*60
      else if RfcTimeZone='M'   then Result:=-12*60
      else if RfcTimeZone='N'   then Result:=  1*60
      else if RfcTimeZone='O'   then Result:=  2*60
      else if RfcTimeZone='P'   then Result:=  3*60
      else if RfcTimeZone='Q'   then Result:=  4*60
      else if RfcTimeZone='R'   then Result:=  5*60
      else if RfcTimeZone='S'   then Result:=  6*60
      else if RfcTimeZone='T'   then Result:=  7*60
      else if RfcTimeZone='U'   then Result:=  8*60
      else if RfcTimeZone='V'   then Result:=  9*60
      else if RfcTimeZone='W'   then Result:= 10*60
      else if RfcTimeZone='X'   then Result:= 11*60
      else if RfcTimeZone='Y'   then Result:= 12*60
      else if RfcTimeZone='Z'   then Result:=  0;

   end;
end;
Const
   RFC_DAY_NAMES   = 'SunMonTueWedThuFriSat';
   RFC_MONTH_NAMES = 'JanFebMarAprMayJunJulAugSepOctNovDec';

function MinutesToDateTime( Minutes: Integer ): TDateTime;
begin
   Result := ( Minutes / 60.0 / 24.0 );
end;
function RfcDateTimeToDateTimeGMT( RfcDateTime: string; ErrorDefault: TDateTime ) : TDateTime;
var  s, h, tz : String;
     i, yyyy, mm, dd, hh, nn, ss : Integer;
begin
   s := TrimWhSpace( RfcDateTime );
   if s='' then begin Result:=ErrorDefault; exit; end;

   try
      // Date: Fri, 27 Mar 1998 12:12:50 +1300

      i := Pos( ',', s );
      if (i>0) and (i<10) then begin
         System.Delete( s, 1, i ); // "Tue,", "Tuesday,"
         s := TrimWhSpace(s);
      end;

      i := Pos(' ',s);
      dd := strtoint( copy(s,1,i-1) );
      System.Delete( s, 1, i );
      s := TrimWhSpace(s);

      i := Pos(' ',s);
      h := lowercase( copy(s,1,i-1) );
      mm := ( ( Pos(h,LowerCase(RFC_MONTH_NAMES)) - 1 ) div 3 ) + 1;
      System.Delete( s, 1, i );
      s := TrimWhSpace(s);

      i := Pos(' ',s);
      yyyy := strtoint( copy(s,1,i-1) );
      if yyyy<100 then begin
         if yyyy>=50 then yyyy:=yyyy+1900 else yyyy:=yyyy+2000;
      end;
      System.Delete( s, 1, i );
      s := TrimWhSpace(s);

      i := Pos(' ',s);
      if i=0 then begin
         h := s;
         tz := '';
      end else begin
         h := TrimWhSpace( copy(s,1,i-1) );
         tz := UpperCase( TrimWhSpace( copy(s,i+1,32) ) );
      end;

      i:=Pos(':',h); if i=2 then h:='0'+h;
      hh := strtoint( copy(h,1,2) );
      nn := strtoint( copy(h,4,2) );
      ss := strtoint( copy(h,7,2) );

      Result := EncodeDate( yyyy, mm, dd )
              + MinutesToDateTime( RfcTimezoneToBiasMinutes( tz ) ) // -> GMT
              + EncodeTime( hh, nn, ss, 0 );
   except
      Result := ErrorDefault
   end;
end;
function RfcDateTimeToDateTimeGMT( RfcDateTime: string ) : TDateTime;
const OldDefault = 29221.0; // =EncodeDate(1980,1,1)
begin
   Result := RfcDateTimeToDateTimeGMT( RfcDateTime, OldDefault );
end;
function BadCombination( MyCharset: String; SearchStr: String ): Boolean;
begin
     Result := False;
     MyCharset := UpperCase( MyCharset );
     if (MyCharset <> 'UTF-8') and (MyCharset <> 'UTF-7') then begin
        if (MyCharset = Charset) then exit;
        if not Has8BitChar( SearchStr ) then exit;
     end;
     Result := True;
     with BaseApplication as IBaseApplication do
       Warning(Format('IMAP SEARCH: Search charset (%s) and message charset (%s) differ. The current message is ignored.',[Charset,MyCharset]));
end;
var  i, j, k, m : Integer;
  HdrValue   : String;
  HdrName    : String;
  MyMail     : TMimeMess = nil;
  MyHeader   : String;
  MyString   : String;
  MyCharset  : String;
  MyDate     : Int64;
  NotFound   : Boolean;
  aHeaders: TStrings;
  aFilter : string = '';
  aMsgs: TMessageList;
begin
  //Headers
  for k := 0 to HeaderList.Count - 1 do begin
     m := Pos( ':', HeaderList[k] );
     HdrName  := Copy( HeaderList[k], 1, m );
     HdrValue := UpperCase( Copy( HeaderList[k], m+1, Length(HeaderList[k])-m ) );
     if HdrValue <> '' then
       begin
         aFilter := aFilter+' OR '+Data.ProcessTerm('UPPER('+Data.QuoteField('ID')+')=UPPER('+Data.QuoteValue(GetEmailAddr(HdrValue))+')');
       end
  end;
  aFilter := '('+copy(aFilter,5,length(aFilter))+')';
  //Date
  aFilter := aFilter+' AND ('+Data.QuoteField('SENDDATE')+'>'+Data.DateTimeToFilter(After)+' AND '+Data.QuoteField('SENDDATE')+'<'+Data.DateTimeToFilter(Before)+')';
  InternalLock('FindContent');
  aMsgs := TMessageList.Create(nil);
  aMsgs.Filter(aFilter,0);
  InternalUnLock('FindContent');
  i := 0;
  SetLength( Result, aMsgs.Count );
  while not aMsgs.EOF do
    begin
      Result[i] := GetIndex(aMsgs.FieldByName('GRP_ID').AsLargeInt);
      inc(i);
      aMsgs.Next;
    end;
  aMsgs.Free;
  {
  SetLength( Result, Length(MsgSet) );
  j := 0;
  try
    for i := 0 to High( MsgSet ) do begin
      MyMail := GetMessage(GetUID(MsgSet[i]-1));
      NotFound := False;

      // Search headers
      aHeaders := TStringList.Create;
      MyMail.Header.EncodeHeaders(aHeaders);
      for k := 0 to HeaderList.Count - 1 do begin
         m := Pos( ':', HeaderList[k] );
         HdrName  := Copy( HeaderList[k], 1, m );
         HdrValue := UpperCase( Copy( HeaderList[k], m+1, Length(HeaderList[k])-m ) );
         if HdrValue <> '' then begin
            MyHeader := UpperCase(aHeaders.Text);
            if BadCombination( MyCharset, HdrValue ) then begin
               NotFound := True;
               break
            end;
            if Pos( HdrValue, MyHeader ) = 0 then begin
               NotFound := True;
               break
            end
         end else begin
            if not (MyMail.Header.FindHeader( HeaderList[k] )='') then begin
               NotFound := True;
               break
            end
         end
      end;
      aHeaders.Free;
      if NotFound then continue;

      // Search message date
      MyDate := Trunc( MyMail.Header.Date );
      if (MyDate <= After) or (MyDate >= Before) then continue;

      if (BodyStrings.Count > 0) or (TextStrings.Count > 0) then begin
         //MyCharset := GetCharset( MyMail.Header.CharsetCode ['Content-Type:'] );
         //if BadCombination( MyCharset, BodyStrings.Text + TextStrings.Text )
         //   then continue;

         // Search body text
         MyString := UpperCase( MyMail.Lines.Text );
         for k := 0 to BodyStrings.Count - 1 do begin
            if Pos( BodyStrings[k], MyString ) = 0 then begin
               NotFound := True;
               break
            end
         end;
         if NotFound then continue;

         // Search full text
         MyString := UpperCase( aHeaders.Text ) + #13#10 + MyString;
         for k := 0 to TextStrings.Count - 1 do begin
            if Pos( TextStrings[k], MyString ) = 0 then begin
               NotFound := True;
               break
            end
         end;
         if NotFound then continue;
      end;
      Result[j] := MsgSet[i];
      inc( j );
   end
  finally
  end;
  SetLength( Result, j )
  }
end;

procedure TPrometMailBox.RefreshFolder(aThread: TSTcpThread);
var
  Tree: TTree;
  aCnt: TDataSet;
  aFilter: String;
  ActId: LongInt;
  aChanged: Integer = 0;
begin
  InternalLock('RefreshFolder');
  try
  Tree := TTree.Create(nil);
  Tree.Select(Path);
  Tree.Open;
  FParent := Path;
  aFilter := Data.QuoteField('TREEENTRY')+'='+Data.QuoteValue(Tree.Id.AsString);
  aFilter := aFilter+' AND '+Data.QuoteField('USER')+'='+Data.QuoteValue(aThread.User);
  with BaseApplication as IBaseApplication do
    Debug('Refreshing Folder:'+Path+' Filter:'+aFilter);
  aCnt := Data.GetNewDataSet('select count('+Data.QuoteField('READ')+') as "READ",count(*) as "MESSAGES",max("GRP_ID") as "HUID", min("GRP_ID") as "MUID" from '+Data.QuoteField(Folder.TableName)+' where '+aFilter);
  aCnt.Open;
  FHighestUID:=StrToIntDef(aCnt.FieldByName('HUID').AsString,0);
  FLowestUID:=StrToIntDef(aCnt.FieldByName('MUID').AsString,0);
  Folder.SortFields:='GRP_ID,MSG_ID';
  Folder.SortDirection:=sdAscending;
  Folder.Filter(aFilter+' AND '+Data.QuoteField('GRP_ID')+' is NULL');
  with BaseApplication as IBaseApplication do
    Debug('Items without GRP_ID:'+IntToStr(Folder.Count));
  while Folder.Locate('GRP_ID',Null,[]) do
    begin
      if (Folder.FieldByName('GRP_ID').IsNull) or (Folder.FieldByName('GRP_ID').AsInteger=ActId) then
        begin
          Folder.Edit;
          Folder.FieldByName('GRP_ID').AsLongint:=FHighestUID+1;
          inc(FHighestUID);
          Folder.Post;
          inc(aChanged);
        end;
      ActId := Folder.FieldByName('GRP_ID').AsInteger;
    end;
  Folder.Filter(aFilter);
  Folder.First;
  ActId := -1;
  if aChanged>0 then
    begin
      Folder.Filter(aFilter);
      Folder.First;
    end;
  with BaseApplication as IBaseApplication do
    Debug('Folder Count:'+IntToStr(Folder.Count));
  aCnt.Free;
  aCnt := Data.GetNewDataSet('select count('+Data.QuoteField('READ')+') as "READ",count(*) as "MESSAGES",max("GRP_ID") as "HUID", min("GRP_ID") as "MUID" from '+Data.QuoteField(Folder.TableName)+' where '+aFilter);
  aCnt.Open;
  FMessages:=StrToIntDef(aCnt.FieldByName('MESSAGES').AsString,0);
  FUnseen:=FMessages-StrToIntDef(aCnt.FieldByName('READ').AsString,0);
  inc(FUnseen,aChanged);
  aCnt.Free;
  finally
    InternalUnlock('RefreshFolder');
  end;
end;

constructor TPrometMailBox.Create(aThread: TSTcpThread; APath: String;
  CS: TCriticalSection);
begin
  inherited Create(aThread,APath,CS);
  _DBCS := CS;
  InternalLock('Create');
  Connection := Data.GetNewConnection;
  Folder := TMessageList.CreateEx(nil,Data,Connection);
  InternalUnlock('Create');
  RefreshFolder(aThread);
  GetUIDvalidity:=DateTimeToUnixTime(Folder.TimeStamp.AsDateTime) and $FFFF;
end;

destructor TPrometMailBox.Destroy;
begin
  Folder.Free;
  Connection.Free;
  inherited Destroy;
end;

function TPrometImapServer.GotoMailBox(MailBox: string; Socket: TSTcpThread
  ): Boolean;
var
  Parent: String;
  ParentId : Variant;
begin
  Result := False;
  MailBox:=CharsetConversion(MailBox,UTF_7mod,UTF_8);
  if pos('/',MailBox)>0 then
    Parent := copy(MailBox,0,RPos('/',MailBox)-1);
  MailBox:=copy(MailBox,RPos('/',MailBox)+1,length(MailBox));
  InternalLock('GotoMailbox',Socket);
  try
  if MailBoxes.Locate('NAME',AnsiToUtf8(Mailbox),[]) then
    begin
      result := True;
      if Parent <> '' then
        if MailBoxes.FieldByName('PARENT').AsLargeInt=0 then
          begin
            Result := Result and MailBoxes.Locate('NAME',copy(Parent,RPos('/',Parent)+1,length(Parent)),[]);
            ParentId := MailBoxes.Id.AsVariant;
            Result := Result and MailBoxes.Locate('NAME;PARENT',VarArrayOf([MailBox,ParentId]),[]);
          end;
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
  finally
    InternalUnlock('GotoMailBox',Socket);
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

procedure TPrometImapServer.SocketRelease(ASocket: TSTcpThread);
begin
  InternalUnlock('Socket release',ASocket);
  inherited SocketRelease(ASocket);
end;

function TPrometImapServer.MBSelect(AThread: TSTcpThread; Mailbox: string;
  aReadOnly: Boolean): boolean;
var
  i: Integer;
begin
  Result:=False;
  if GotoMailBox(Mailbox,AThread) then
    begin
      TSImapThread(AThread).Selected := TPrometMailbox.Create(AThread,MailBoxes.Id.AsVariant,_DBCS);
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
  if GotoMailBox(Mailbox,AThread) then
    begin
      Result := TPrometMailbox.Create(AThread,MailBoxes.Id.AsVariant,_DBCS);
    end;
end;

function TPrometImapServer.MBCreate(AThread: TSTcpThread; Mailbox: string
  ): boolean;
begin
  InternalLock('MBCreate',AThread);
  try
  try
    MailBoxes.Append;
    MailBoxes.Text.AsString:=Mailbox;
    MailBoxes.FieldByName('TYPE').AsString:='N';
    MailBoxes.Post;
    Result:=True;
  except
    Result:=False;
  end;
  finally
    InternalUnlock('MBCreate',AThread);
  end;
end;

function TPrometImapServer.MBDelete(AThread: TSTcpThread; Mailbox: string
  ): boolean;
begin
  Result:=False;
end;

function TPrometImapServer.MBExists(AThread: TSTcpThread; var Mailbox: string
  ): boolean;
begin
  Result := GotoMailBox(Mailbox,AThread);
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
  if GotoMailBox(Path,AThread) then
    begin
      Mailbox := TPrometMailbox.Create(aThread,MailBoxes.Id.AsVariant,_DBCS);
      Result := True;
    end;
end;

procedure TPrometImapServer.MBLogout(AThread: TSTcpThread;
  var Mailbox: TImapMailbox; LOSel: Boolean);
begin
end;

procedure TPrometImapServer.DoSearch(AThread: TSTcpThread; UseUID: Boolean;
  Par: String);
var
  Command: String;
  Charset: String;
  i: Integer;
  SearchPgm: TIMAPSearch;
begin
  if UseUID then Command := 'UID SEARCH' else Command := 'SEARCH';
  Charset  := '';
  if uppercase( copy( Par, 1, 7 ) ) = 'CHARSET' then begin
     Delete( Par, 1, 8 );
     Par := TrimWhSpace( Par );
     i := PosWhSpace( Par );
     if (Par='') or (i=0) then begin
        SendResTag(AThread, 'BAD ' + Command + ' charset argument missing' );
        exit
     end;
     Charset := Uppercase( TrimQuotes( copy( Par, 1, i-1 ) ) );
     Par := TrimWhSpace( copy( Par, i+1, length(Par) ) );
     if (Charset = 'UTF-8') or (Charset = 'UTF-7') then begin
        SendResTag(AThread, 'NO [BADCHARSET] specified charset not supported' );
        exit
     end
  end;
  SearchPgm := TIMAPSearch.Create( Charset );
  try
    try
       if GetSearchPgm( Par, SearchPgm ) then begin
          SendRes(AThread, 'SEARCH' + TSImapThread(AThread).Selected.Search( SearchPgm, UseUID ) );
          SendResTag(AThread, 'OK ' + Command + ' completed.' )
       end else
          SendResTag(AThread, 'BAD ' + Command + ' invalid syntax (see server log for details)' );
     except
       on e : Exception do
         SendResTag(AThread, 'ERROR ' + Command + ' '+e.Message );
     end;
  finally
     SearchPgm.Free
  end
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
       SendResTag(AThread, 'OK ' + Command + ' completed.' );
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
  aRec: TBookmark;
begin
  try
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
    with BaseApplication  do
      if not HasOption('debug') then
        DisableLog;
    if (Length(MsgSet)=1) and (MsgSet[0]=-1) then //Fetchall
      begin
        with TPrometMailBox(TSImapThread(AThread).Selected).Folder.DataSet as IBaseDbFilter do
          with BaseApplication as IBaseApplication do
            Debug('Fetchall Filter: '+Filter+' Count:'+IntToStr(TPrometMailBox(TSImapThread(AThread).Selected).Folder.Count));
        TPrometMailBox(TSImapThread(AThread).Selected).Folder.First;
        while not TPrometMailBox(TSImapThread(AThread).Selected).Folder.EOF do
          begin
            aRec := TPrometMailBox(TSImapThread(AThread).Selected).Folder.DataSet.GetBookmark;
            SendS := TSImapThread(AThread).Selected.Fetch(TPrometMailBox(TSImapThread(AThread).Selected).Folder.DataSet.RecNo , MsgDat, Success );
            if (trim(SendS) <> '') AND Success then SendRes (AThread, SendS );
            TPrometMailBox(TSImapThread(AThread).Selected).Folder.DataSet.GotoBookmark(aRec);
            TPrometMailBox(TSImapThread(AThread).Selected).Folder.Next;
            if TSImapThread(AThread).Terminated then break;
          end;
      end
    else
      begin
        for i := Low(MsgSet) to High(MsgSet) do
          begin
            SendS := TSImapThread(AThread).Selected.Fetch( MsgSet[i]-1, MsgDat, Success );
            if TSImapThread(AThread).Terminated then break;
            if (trim(SendS) <> '') AND Success then SendRes (AThread, SendS );
          end;
      end;
    with BaseApplication  do
      if not HasOption('debug') then
        EnableLog;
  except
    Success := False;
  end;
  if Success then
    SendResTag(AThread, 'OK ' + Command + ' is now completed.' )
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
    _DbCS.Enter;
    aMailBoxes := TTree.Create(nil);
    Data.SetFilter(aMailBoxes,'('+Data.QuoteField('TYPE')+'='+Data.QuoteValue('N')+' OR '+Data.QuoteField('TYPE')+'='+Data.QuoteValue('B')+') AND '+Data.QuoteField('PARENT')+'='+Data.QuoteValue(IntToStr(aParent)),0,'','ASC',False,True,True);
    _DbCS.Leave;
    aMailBoxes.First;
    while not aMailBoxes.EOF do
      begin
        if aMailBoxes.Id.AsVariant = TREE_ID_MESSAGES then
          begin
            if not LSub then
              Found := Base + 'INBOX'
          end
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
      if not GotoMailBox(Mailbox,AThread) then
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
      if not GotoMailBox(Mailbox,AThread) then
        begin
          SendResTag(AThread, 'BAD Mailbox not valid!' );
          exit;
        end;
      SendResTag(AThread, 'BAD not implemented!' )
    end;
end;

procedure TPrometImapServer.InternalLock(WhoAmI: string; Socket: TSTcpThread);
var
  i: Integer;
begin
  with BaseApplication as IBaseApplication do
    Debug('  Locking entered from '+WhoAmI);
  if not _DBCS.TryEnter then
    begin
      with BaseApplication as IBaseApplication do
        Debug('already Locked from '+_LockName+' !');
      for i := 0 to 5000 do
        begin
          sleep(1);
          if _DBCS.TryEnter then
            begin
              with BaseApplication as IBaseApplication do
                Debug('  Locking finished '+WhoAmI);
              _LockFrom := Socket;
              _LockName:=WhoAmI;
              exit;
            end;
        end;
      if not _DBCS.TryEnter then
        raise Exception.Create('Lock after 5secs not released !!');
    end
  else
    begin
      _LockFrom := Socket;
      _LockName:=WhoAmI;
      with BaseApplication as IBaseApplication do
        Debug('  Locking finished '+WhoAmI);
    end;
end;

procedure TPrometImapServer.InternalUnlock(WhoAmI: string; Socket: TSTcpThread);
begin
  if Socket=_LockFrom then
    begin
      with BaseApplication as IBaseApplication do
        Debug('UnLocking entered from '+WhoAmI);
      _DbCS.Leave;
    end;
end;

constructor TPrometImapServer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  _DBCs := TCriticalSection.Create;
  MailBoxes := TTree.Create(nil);
  _DbCS.Enter;
  Data.SetFilter(MailBoxes,Data.QuoteField('TYPE')+'='+Data.QuoteValue('N')+' OR '+Data.QuoteField('TYPE')+'='+Data.QuoteValue('B'),0,'','ASC',False,True,False);
  _DbCS.Leave;
end;

destructor TPrometImapServer.Destroy;
begin
  _DBCS.Free;
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
  IMAPServer.InternalLock('ServerAcceptMail',aSocket);
  try
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
  finally
    IMAPServer.InternalUnlock('ServerAcceptMail',aSocket);
  end;
  Result := aRes;
end;

procedure TPIMAPServer.ServerLog(aSocket: TSTcpThread; DirectionIn: Boolean;
  aMessage: string);
var
  aID: Integer;
  msg: String;
begin
  with Self as IBaseApplication do
    begin
      if aSocket is TSTcpThread then
        aID := TSTcpThread(aSocket).Id;
      if aSocket is TSTcpThread then
        aID := TSTcpThread(aSocket).Id;
      if DirectionIn then
        msg := IntToStr(aId)+':>'+aMessage
      else
        msg := IntToStr(aId)+':<'+aMessage;
      Info(msg);
    end;
end;

function TPIMAPServer.ServerLogin(aSocket: TSTcpThread; aUser,
  aPasswort: string): Boolean;
var
  aUsers: TUser;
begin
  Result := False;
  IMAPServer.InternalLock('Login',aSocket);
  try
    try
      Result := Data.Authenticate(aUser,aPasswort);
      with Self as IBaseApplication do
        begin
          if Result then
            Log(IntToStr(TSTcpThread(aSocket).Id)+':Login:'+aUser)
          else
            Error('Login failed:'+aUser);
        end;
      if Result then
        aSocket.User:=Data.Users.Accountno.AsString;
    except
      Result := False;
    end;
  finally
    IMAPServer.InternalUnlock('Login',aSocket);
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
  IMAPServer.Internallock('ServerMailReceived',aSocket);
  try
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
  finally
    IMAPServer.InternalUnlock('ServerMailReceived',aSocket);
  end;
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
          Error('Login failed');
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
      CheckSynchronize(100);
      if (Now()-aTime) > ((1/HoursPerDay)) then
        begin
          break;
        end;
    end;
  IMAPServer.Active:=False;
  SMTPServer.Active:=False;
  // stop program loop
  Terminate;
  exit;
end;

constructor TPIMAPServer.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
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
  with Self as IBaseDBInterface do
    DBLogout;
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
