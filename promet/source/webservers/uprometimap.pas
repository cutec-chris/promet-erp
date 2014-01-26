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
unit uprometimap;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, uLIMAP, uMessages, MimeMess, uMimeMessages, db,cwstring,
  LCLProc;

{.$DEFINE DEBUG}
type

  { TPIMAPSocket }

  TPIMAPSocket = class(TLIMAPSocket)
  public
    procedure RefreshFolders; override;
    function SelectUser: Boolean; override;
  end;

  { TPIMAPFolder }

  TPIMAPFolder = class(TIMAPFolder)
  private
    FMessages : TMessageList;
    FTreeEntry : string;
    FGroupName : string;
    FCount : Integer;
    FUnreadCount : Integer;
    FLastID : LargeInt;
    FetchSequence : Integer;
    FSelector: String;
    FUseUID : Boolean;
    FSequenceNumbers : TStringList;
    function SelectNext : Boolean;
    function GenerateMessage : TMimeMess;
    procedure RefreshFirstID;
    procedure RefreshCount;
  protected
    function GetLastID: LargeInt;override;
    function GetFirstID: LargeInt;override;
    function GetCount: Integer;override;
    function GetUnreadCount: Integer;override;
    function GetNextID: LargeInt; override;
    function GetMessage(Idx : Integer): TMimeMess;override;
    function GetMessageByID(Idx : string): TMimeMess;override;
    function GetCreatedAt: TDateTime; override;
    function PostArticle(aArticle : TStrings;aUser : string;FPostFlags : string;
    FPostDateTime : string) : Boolean;override;
    function SelectMessages(aFilter : string;aUseUID : Boolean) : Boolean;override;
    function FetchOneEntry(aFetch: string): TStrings; override;
    function StoreOneEntry(aFetch: string): TStrings; override;
    function CopyOneEntry(aParams: string): TStrings; override;
    function Search(aParams: string): string; override;
  public
    constructor Create(aParent : TIMAPFolders;aName : string;aUID : string);override;
    destructor Destroy;override;
    property TreeEntry : string read FTreeEntry;
  end;

implementation
uses uData,Variants,SynaUtil,uSessionDBClasses,uBaseDBInterface,Utils,
  uPerson,LConvEncoding,uIntfStrConsts;

{ TPIMAPSocket }

procedure TPIMAPSocket.RefreshFolders;
var
  aGroup: TPIMAPFolder;
begin
  Folders.Clear;
  SelectUser;
  Data.SetFilter(Data.Tree,Data.QuoteField('PARENT')+'=0 and '+Data.QuoteField('TYPE')+'='+Data.QuoteValue('N')+' OR '+Data.QuoteField('TYPE')+'='+Data.QuoteValue('B'),0,'','ASC',False,True,True);
  with Data.Tree.DataSet do
    begin
      First;
      while not EOF do
        begin
          if Data.Tree.Id.AsVariant = TREE_ID_MESSAGES then
            aGroup := TPIMAPFolder.Create(Folders,'INBOX',Data.Tree.Id.AsString)
          else if Data.Tree.Id.AsVariant = TREE_ID_DELETED_MESSAGES then
            aGroup := TPIMAPFolder.Create(Folders,'Trash',Data.Tree.Id.AsString)
          else if Data.Tree.Id.AsVariant = TREE_ID_SEND_MESSAGES then
            aGroup := TPIMAPFolder.Create(Folders,'Sent',Data.Tree.Id.AsString)
          else
            aGroup := TPIMAPFolder.Create(Folders,FieldByName('NAME').AsString,Data.Tree.Id.AsString);
          Folders.Add(aGroup);
          next;
        end;
    end;
end;

function TPIMAPSocket.SelectUser : Boolean;
begin
  Result := Data.Users.DataSet.Locate('LOGINNAME',User,[]) or Data.Users.DataSet.Locate('NAME',User,[]);
  Data.Users.Rights.ResetCache;
  Data.RefreshUsersFilter;
end;

function TPIMAPFolder.SelectNext: Boolean;
var
  aFilter: String;
  Arg1: String;
  Arg2: String;
  Max: Integer;
begin
  Max := 1;
  Result := True;
  if pos(',',FSelector)>0 then
    begin
      aFilter := copy(FSelector,0,pos(',',FSelector)-1);
      FSelector := copy(FSelector,pos(',',FSelector)+1,length(FSelector));
    end
  else
    begin
      aFilter := FSelector;
      FSelector:='';
    end;
  {$IFDEF DEBUG}
  debugln('SelectNext:'+aFilter);
  {$ENDIF}
  if pos(':',aFilter) = 0 then
    begin
      Arg1 := aFilter;
      Max := 1;
      Arg2 := '';
    end
  else
    begin
      Arg1 := copy(aFilter,0,pos(':',aFilter)-1);
      Arg2 := copy(aFilter,pos(':',aFilter)+1,length(aFilter));
    end;
  if aFilter='' then
    begin
      Max := 0;
      Result := False;
      exit;
    end;
  FMessages.DataSet.Last;
  if not FUseUID then
    begin
      if FSequenceNumbers.Count<=StrToInt(Arg1) then
        FMessages.GotoBookmark(FSequenceNumbers[StrToInt(Arg1)-1])
      else
        FMessages.DataSet.MoveBy(-(StrToInt(Arg1)-1))
    end
  else
    begin
      Result := FMessages.GotoBookmark(StrToIntDef(Arg1,0));
      if (not Result) and (StrToIntDef(Arg2,9999)<9999) then
        begin
          FMessages.Last;
          FMessages.DataSet.MoveBy(StrToIntDef(Arg2,0));
          if StrToIntDef(Arg2,9999)<9999 then
            Max := StrToIntDef(Arg2,0)-StrToIntDef(Arg1,0);
          Result := FMessages.Count>0;
        end;
    end;
  if (trim(Arg2) = '') and FUseUID then
    Max := 1
  else if (trim(Arg2) = '*') then
    begin
      Max := FMessages.Count;
      result := True;
    end
  else
    begin
      try
        Max := (StrToInt(Arg2)-StrToInt(Arg1))+1;
      except
        Result := False;
      end;
    end;
  FSelectCount:=Max;
end;

function TPIMAPFolder.GenerateMessage: TMimeMess;
var
aMessage: TMimeMessage;
begin
  aMessage := TMimeMessage.Create(Self,Data);
  aMessage.Select(FMessages.Id.AsVariant);
  aMessage.Open;
  Result := aMessage.EncodeMessage;
  Result.EncodeMessage;
  aMessage.Free;
end;
procedure TPIMAPFolder.RefreshFirstID;
begin
  FMessages.Filter(Data.QuoteField('TREEENTRY')+'='+Data.QuoteValue(FTreeEntry)+' AND '+Data.QuoteField('USER')+'='+Data.QuoteValue(Data.Users.FieldByName('ACCOUNTNO').AsString),300,'SENDDATE','DESC');
  FMessages.Last;
  if FMessages.Count > 0 then
    FFirstID := FMessages.Id.AsVariant;
  {$IFDEF DEBUG}
  debugln('RefreshFirstID:'+FMessages.Id.AsString);
  {$ENDIF}
end;
procedure TPIMAPFolder.RefreshCount;
begin
  FFIrstID := 0;
  FlastID := 0;
  Data.SetFilter(FMessages,Data.QuoteField('TREEENTRY')+'='+Data.QuoteValue(FTreeEntry)+' and '+Data.QuoteField('READ')+'='+Data.QuoteValue('N')+' AND '+Data.QuoteField('USER')+'='+Data.QuoteValue(Data.Users.FieldByName('ACCOUNTNO').AsString),300,'SENDDATE','DESC');
  FUnreadCount := FMessages.Count;
  Data.SetFilter(FMessages,Data.QuoteField('TREEENTRY')+'='+Data.QuoteValue(FTreeEntry)+' AND '+Data.QuoteField('USER')+'='+Data.QuoteValue(Data.Users.FieldByName('ACCOUNTNO').AsString),300,'SENDDATE','DESC');
  if FMessages.Count > 0 then
    begin
      FMessages.Last;
      FFirstID:=FMessages.Id.AsVariant;
      Fmessages.DataSet.First;
      FLastID:=FMessages.Id.AsVariant;
    end;
  FCount := FMessages.Count;
end;
function TPIMAPFolder.GetLastID: LargeInt;
begin
  Result := FLastID;
  if Result <= FirstID then Result := FirstID+1;
end;
function TPIMAPFolder.GetFirstID: LargeInt;
begin
  Result := FFirstID;
end;
function TPIMAPFolder.GetCount: Integer;
begin
  RefreshCount;
  Result := FCount;
end;

function TPIMAPFolder.GetUnreadCount: Integer;
begin
  RefreshCount;
  Result := FUnreadCount;
end;

function TPIMAPFolder.GetNextID: LargeInt;
begin
  Result := Data.GetUniID(nil,'GEN_SQL_ID',False);
end;

function TPIMAPFolder.GetMessage(Idx: Integer): TMimeMess;
begin
  Result := nil;
  Data.SetFilter(FMessages,Data.QuoteField('TREEENTRY')+'='+Data.QuoteValue(FTreeEntry)+' AND '+Data.QuoteField('SQL_ID')+'='+Data.QuoteValue(IntToStr(Idx)),1);
  MessageIdx := -1;
  if FMessages.Count > 0 then
    begin
      MessageIdx := Fmessages.Id.AsVariant;
      Result := GenerateMessage;
    end;
end;
function TPIMAPFolder.GetMessageByID(Idx: string): TMimeMess;
begin
  Result:=nil;
end;
function TPIMAPFolder.GetCreatedAt: TDateTime;
begin
  Result:=inherited GetCreatedAt;
  if Data.Tree.DataSet.Locate('ID',FTreeEntry,[]) then
    Result := Data.Tree.TimeStamp.AsDateTime;
end;
function TPIMAPFolder.PostArticle(aArticle: TStrings;aUser : string;FPostFlags : string;
    FPostDateTime : string): Boolean;
var
  aMsg: TMimeMess;
  aMessage: TMimeMessage;
  aID: String;
  i: Integer;
  aChr: Char;
  CustomerCont: TPersonContactData;
  Customers: TPerson;
  atmp: String;
  aSubject: String;
begin
  Result := False;
  aMsg := TMimeMess.Create;
  aMsg.Lines.Assign(aArticle);
  aMsg.DecodeMessage;
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
  aMessage := TMimeMessage.Create(Self,Data);
  amessage.Filter(Data.QuoteField('ID')+'='+Data.QuoteValue(aMsg.Header.MessageID){+' and '+Data.QuoteField('TREEENTRY')+'='+Data.QuoteValue(FTreeEntry)});
  if aMessage.Count=0 then
    begin
      aMessage.Insert;
      aMessage.FieldByName('ID').Clear;
      if Data.Users.DataSet.Locate('NAME',aUser,[loCaseInsensitive]) then
        aMessage.Dataset.FieldByName('USER').AsString := Data.Users.DataSet.FieldByName('ACCOUNTNO').AsString;
      aMessage.Dataset.FieldByName('TYPE').AsString := 'EMAIL';
      aMessage.Dataset.FieldByName('READ').AsString := 'N';
      aMessage.DecodeMessage(aMsg);
      aMessage.FieldbyName('TREEENTRY').AsString := FTreeEntry;
      if FPostDateTime<>'' then
        aMessage.FieldByName('SENDDATE').AsDateTime:=DecodeRfcDateTime(FPostDateTime);
      aSubject := ConvertEncoding(amsg.Header.Subject,GuessEncoding(amsg.Header.Subject),EncodingUTF8);
      atmp:=ConvertEncoding(getemailaddr(aMsg.Header.From),GuessEncoding(getemailaddr(aMsg.Header.From)),EncodingUTF8);
      CustomerCont := TPersonContactData.Create(Self,Data);
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
      Customers := TPerson.Create(Self,Data);
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
          if FPostDateTime<>'' then
            Customers.History.TimeStamp.AsDateTime:=aMessage.FieldByName('SENDDATE').AsDateTime;
          with Customers.History.DataSet as IBaseManageDB do
            UpdateStdFields := True;
          Customers.History.Post;
          {
          if Data.Users.DataSet.Locate('NAME',aUser,[loCaseInsensitive]) then
          Data.Users.History.AddItemWithoutUser(Customers.DataSet,Format(strActionMessageReceived,[aSubject]),
                                        'MESSAGEIDX@'+aMessage.FieldByName('ID').AsString+'{'+aSubject+'}',
                                        '',
                                        nil,
                                        ACICON_MAILNEW);
          Data.Users.History.Edit;
          with Data.Users.History.DataSet as IBaseManageDB do
            UpdateStdFields := False;
          if FPostDateTime<>'' then
            Data.Users.History.TimeStamp.AsDateTime:=aMessage.FieldByName('SENDDATE').AsDateTime;
          Data.Users.History.Post;
          with Data.Users.History.DataSet as IBaseManageDB do
            UpdateStdFields := True;
          }
        end;
      aMessage.DataSet.Post;
      Result := True;
    end;
  aMsg.Free;
  aMessage.Destroy;
end;

function TPIMAPFolder.SelectMessages(aFilter: string;aUseUID : Boolean): Boolean;
var
  Arg1: String;
  Arg2: String;
  Max: Integer;
begin
  Result:=False;
  Max := 0;
  FSelector := aFilter;
  FUseUID := aUseUID;
  FMessages.Last;
  Result := SelectNext;
end;

function TPIMAPFolder.FetchOneEntry(aFetch: string): TStrings;
var
  i: Integer;
  tmp: String = '';
  aMessage: TMimeMessage = nil;
  aSL: TStringList;
  aLen: Integer;
  a: Integer;
  aMime: TMimeMess = nil;
  aFields: String;
  bsl: TStringList;
  Found: Boolean;
  bFetch: String;
  aSize: String;
  tmpRecNo: String;
  FAdded: Boolean;
begin
  if FSelectCount=0 then
    if not SelectNext then
      begin
        Result := nil;
        exit;
      end;
  Result := TStringList.Create;
  aFetch := aFetch+' ';
  if not FMessages.DataSet.BOF then
    begin
      if FSequenceNumbers.IndexOf(FMessages.Id.AsString)=-1 then
        begin
          tmpRecNo := IntToStr(FSequenceNumbers.Add(FMessages.Id.AsString)+1);
        end
      else tmpRecNo:=IntToStr(FSequenceNumbers.IndexOf(FMessages.Id.AsString)+1);
      tmp := '* '+tmpRecNo+' FETCH (';
      while pos(' ',aFetch)>0 do
        begin
          bFetch := copy(afetch,0,pos(' ',afetch)-1);
          if pos('<',bFetch)>0 then
            bfetch := copy(bFetch,0,pos('<',bFetch)-1);
          afetch := copy(afetch,pos(' ',afetch)+1,length(afetch));
          case Uppercase(bFetch) of
          'UID':
            begin
              tmp := tmp+'UID '+FMessages.FieldByName('SQL_ID').AsString+' ';
            end;
          'FLAGS','(FLAGS)':
            begin
              FAdded := False;
              tmp := tmp+'FLAGS (';
              if FMessages.FieldByName('READ').AsString='Y' then
                begin
                  tmp+='\Seen ';
                  FAdded:=True;
                end;
              if FMessages.FieldByName('FLAGGED').AsString='Y' then
                begin
                  tmp+='\Flagged ';
                  FAdded:=True;
                end;
              if FMessages.FieldByName('DRAFT').AsString='Y' then
                begin
                  tmp+='\Draft ';
                  FAdded:=True;
                end;
              if FMessages.FieldByName('ANSWERED').AsString='Y' then
                begin
                  tmp+='\Answered ';
                  FAdded:=True;
                end;
              if FMessages.FieldByName('TREEENTRY').AsVariant=TREE_ID_DELETED_MESSAGES then
                begin
                  tmp+='\Deleted ';
                  FAdded:=True;
                end;
              if FAdded then
                tmp := copy(tmp,0,length(tmp)-1);
              tmp+=') ';
            end;
          'INTERNALDATE':
            begin
              tmp := tmp+'INTERNALDATE "'+Rfc822DateTime(FMessages.FieldByName('SENDDATE').AsDateTime)+'" ';
            end;
          'RFC822.SIZE':
            begin
              if not Assigned(aMessage) then
                begin
                  aMessage := TMimeMessage.Create(Self,Data);
                  aMessage.Select(FMessages.Id.AsVariant);
                  aMessage.Open;
                end;
              if not Assigned(aMime) then
                aMime := aMessage.EncodeMessage;
              aMime.Lines.TextLineBreakStyle:=tlbsCRLF;
              aSize := IntToStr(length(aMime.Lines.Text)+2);
              //aSize := FMessages.FieldByName('SIZE').AsString;
              if aSize = '' then aSize := '0';
              tmp := tmp+'RFC822.SIZE '+aSize+' ';
            end;
          'RFC822.HEADER','BODY[HEADER]':
            begin
              if not Assigned(aMessage) then
                begin
                  aMessage := TMimeMessage.Create(Self,Data);
                  aMessage.Select(FMessages.Id.AsVariant);
                  aMessage.Open;
                end;
              if aMessage.Count>0 then
                aMessage.Content.Open;
              aSL := TStringList.Create;
              aSL.text := aMessage.Content.FieldByName('HEADER').AsString;
              aSL.TextLineBreakStyle:=tlbsCRLF;
              aLen := length(aSL.Text);
              tmp := tmp+bFetch+' {'+IntToStr(aLen+2)+'}'+#13#10+aSL.Text;
              aSL.Free;
            end;
          'RFC822','BODY[]','BODY.PEEK[]':
            begin
              if not Assigned(aMessage) then
                begin
                  aMessage := TMimeMessage.Create(Self,Data);
                  aMessage.Select(FMessages.Id.AsVariant);
                  aMessage.Open;
                end;
              if not Assigned(aMime) then
                aMime := aMessage.EncodeMessage;
              aMime.Lines.TextLineBreakStyle:=tlbsCRLF;
              aSL := TStringList.Create;
              aSL.text := aMime.Lines.Text;
              aLen :=  length(aSL.Text);
              if (pos('RFC822',bFetch)>0) then
                tmp := tmp+'RFC822 {'+IntToStr(aLen+2)+'}'+#13#10+aSL.Text+#13#10+' ';
              if (pos('BODY[]',bFetch)>0)
              or (pos('BODY.PEEK[]',bFetch)>0)
              then
                tmp := tmp+'BODY[] {'+IntToStr(aLen+2)+'}'+#13#10+aSL.Text+#13#10+' ';
              aSL.Free;
            end;
          'BODY.PEEK[HEADER.FIELDS':
            begin
              if not Assigned(aMessage) then
                begin
                  aMessage := TMimeMessage.Create(Self,Data);
                  aMessage.Select(FMessages.Id.AsVariant);
                  aMessage.Open;
                end;
              if not Assigned(aMime) then
                aMime := aMessage.EncodeMessage;
              aMime.Lines.TextLineBreakStyle:=tlbsCRLF;
              //BODY.PEEK[HEADER.FIELDS (From To Cc Bcc Subject Date Message-ID Priority X-Priority References Newsgroups In-Reply-To Content-Type)]
              aFields := copy(aFetch,pos('(',aFetch)+1,length(aFetch));
              aFields := copy(aFields,0,pos(')',aFields)-1);
              aSL := TStringList.Create;
              aSL.TextLineBreakStyle:=tlbsCRLF;
              bsl := TStringlist.Create;
              aMime.Header.EncodeHeaders(bsl);
              while pos(' ',aFields)>0 do
                begin
                  Found := False;
                  for a := 0 to bsl.Count-1 do
                    if Uppercase(copy(bsl[a],0,pos(':',bsl[a])-1)) = Uppercase(copy(aFields,0,pos(' ',aFields)-1)) then
                      begin
                        aSl.Add(bsl[a]);
                        Found := True;
                        break;
                      end;
                  //if not Found then
                  //  aSL.Add(copy(aFields,0,pos(' ',aFields)-1)+':');
                  aFields := copy(aFields,pos(' ',aFields)+1,length(aFields));
                end;
              if aFields<>'' then
                begin
                  Found := False;
                  for a := 0 to bsl.Count-1 do
                    if Uppercase(copy(bsl[a],0,pos(':',bsl[a])-1)) = Uppercase(aFields) then
                      begin
                        aSl.Add(bsl[a]);
                        Found := True;
                        break;
                      end;
                  //if not Found then
                  //  aSL.Add(aFields+':');
                  aFields := copy(aFields,pos(' ',aFields)+1,length(aFields));
                end;
              aLen := 0;
              aSL.Add('');
              aLen :=  length(aSL.Text);
              aFields := bFetch+' '+aFetch;
              aFields := copy(aFields,0,pos(']',aFields));
              aFields := StringReplace(aFields,'BODY.PEEK[','BODY[',[]);
              tmp := tmp+aFields+' {'+IntToStr(aLen)+'}'+#13#10+aSL.Text+' ';
              aSL.Free;
              bSL.Free;
              aFetch:=copy(aFetch,pos(']',afetch)+1,length(aFetch));
            end
{          else if trim(bFetch) <> '' then
            begin
              Result.Clear;
              exit;
            end;}
          end;
        end;
      Result.Add(copy(tmp,0,length(tmp)-1)+')');
      {$IFDEF DEBUG}
      debugln('FetchOneEntry:'+FMessages.Id.AsString+' '+FMessages.Subject.AsString+' '+tmpRecNo);
      {$ENDIF}
      FetchSequence:=FetchSequence+1;
      FreeAndNil(aMessage);
      FreeAndNil(aMime);
      Fmessages.DataSet.Prior;
      Dec(FSelectCount,1);
    end
  else
    FreeAndNil(Result);
end;

function TPIMAPFolder.StoreOneEntry(aFetch: string): TStrings;
var
  tmpRecNo: String;
  bFetch: String;
  tmp: String;
  bOperator: String;
  aParams: String;
begin
  if FSelectCount=0 then
    if not SelectNext then
      begin
        Result := nil;
        exit;
      end;
  Result := TStringList.Create;
  aFetch := aFetch+' ';
  if not FMessages.DataSet.BOF then
    begin
      if FSequenceNumbers.IndexOf(FMessages.Id.AsString)=-1 then
        begin
          tmpRecNo := IntToStr(FSequenceNumbers.Add(FMessages.Id.AsString)+1);
        end
      else tmpRecNo:=IntToStr(FSequenceNumbers.IndexOf(FMessages.Id.AsString)+1);
      tmp := '* '+tmpRecNo+' FETCH (';
      while pos(' ',aFetch)>0 do
        begin
          bFetch := copy(afetch,0,pos(' ',afetch)-1);
          bOperator := copy(bFetch,0,1);
          bFetch := copy(bFetch,2,length(bFetch));
          if pos('<',bFetch)>0 then
            bfetch := copy(bFetch,0,pos('<',bFetch)-1);
          afetch := copy(afetch,pos(' ',afetch)+1,length(afetch));
          case UpperCase(bFetch) of
          'FLAGS','(FLAGS)','FLAGS.SILENT':
            begin
              aParams := Uppercase(copy(trim(aFetch),2,pos(')',trim(aFetch))-1));
              FMessages.Edit;
              if bOperator='+' then
                bOperator:='Y'
              else bOperator := 'N';
              if pos('\SEEN',aParams)>0 then
                FMessages.FieldByName('READ').AsString:=bOperator;
              if (pos('\ANSWERED',aParams)>0) and (bOperator='Y') then
                FMessages.FieldByName('ANSWERED').AsDateTime:=Now();
              if (pos('\DELETED',aParams)>0) and (bOperator='Y') then
                FMessages.FieldByName('TREEENTRY').AsVariant:=TREE_ID_DELETED_MESSAGES;
              if (pos('\FLAGGED',aParams)>0) then
                FMessages.FieldByName('FLAGGED').AsString:=bOperator;
              if (pos('\DRAFT',aParams)>0) then
                FMessages.FieldByName('DRAFT').AsString:=bOperator;
              FMessages.Post;
              if bFetch <> 'FLAGS.SILENT' then
                begin
                  tmp := tmp+'FLAGS (';
                  if FMessages.FieldByName('READ').AsString='Y' then
                    tmp+='\Seen ';
                  if FMessages.FieldByName('FLAGGED').AsString='Y' then
                    tmp+='\Flagged ';
                  if FMessages.FieldByName('DRAFT').AsString='Y' then
                    tmp+='\Draft ';
                  if FMessages.FieldByName('ANSWERED').AsString='Y' then
                    tmp+='\Answered ';
                  if FMessages.FieldByName('TREEENTRY').AsVariant=TREE_ID_DELETED_MESSAGES then
                    tmp+='\Deleted '
                  else tmp +=' ';
                  tmp := copy(tmp,0,length(tmp)-1);
                  tmp+=') ';
                  {$IFDEF DEBUG}
                  debugln(tmp);
                  {$ENDIF}
                end
              else
                tmp:='';
            end;
{          else if trim(bFetch) <> '' then
            begin
              Result.Clear;
              exit;
            end;}
          end;
        end;
      {$IFDEF DEBUG}
      debugln('StoreOneEntry:'+FMessages.Id.AsString+' '+FMessages.Subject.AsString);
      {$ENDIF}
      if tmp <> '' then
        Result.Add(copy(tmp,0,length(tmp)-1)+')');
      FetchSequence:=FetchSequence+1;
      Fmessages.DataSet.Prior;
      Dec(FSelectCount,1);
    end
  else
    FreeAndNil(Result);
end;

function TPIMAPFolder.CopyOneEntry(aParams: string): TStrings;
var
  aNewMessage: TMessageList;
  tmpRecNo: String;
  i: Integer;
  tmp: String;
  aFolder: TPIMAPFolder;
begin
  //we implement copy as move since that makes in most cases more sense
  if FSelectCount=0 then
    if not SelectNext then
      begin
        Result := nil;
        exit;
      end;
  if copy(aParams,0,1)='"' then
    begin
      tmp := copy(aParams,2,length(aParams));
      tmp := copy(tmp,0,pos('"',tmp)-1);
    end
  else
    tmp := copy(aParams,0,pos(' ',aParams)-1);
  for i := 0 to Parent.Count-1 do
    begin
      if Parent.Folder[i].Name = tmp then
        begin
          aFolder := TPIMAPFolder(Parent.Folder[i]);
        end;
    end;
  Result := TStringList.Create;
  //aNewMessage := TMessageList.Create(nil,Data);
  if not FMessages.DataSet.BOF then
    begin
      if FSequenceNumbers.IndexOf(FMessages.Id.AsString)=-1 then
        begin
          tmpRecNo := IntToStr(FSequenceNumbers.Add(FMessages.Id.AsString)+1);
        end
      else tmpRecNo:=IntToStr(FSequenceNumbers.IndexOf(FMessages.Id.AsString)+1);
      FMessages.Edit;
      Fmessages.FieldByName('TREEENTRY').AsVariant:=aFolder.TreeEntry;
      FMessages.Post;
      {aNewMessage.Insert;
      for i := 1 to FMessages.DataSet.Fields.Count-1 do
        aNewMessage.DataSet.Fields[i].AsVariant:=FMessages.DataSet.Fields[i].AsVariant;
      aNewMessage.FieldByName('TREEENTRY').AsVariant:=aFolder.FTreeEntry;
      aNewMessage.Post;}
      Fmessages.DataSet.Prior;
      Dec(FSelectCount,1);
    end;
  //aNewMessage.Free;
end;

function TPIMAPFolder.Search(aParams: string): string;
var
  tmpRecNo: String;
  Found: Boolean = False;
  aSQL: String;
  aSet: String;

  function NextParam(Command : Boolean = True) : string;
  begin
    if pos(' ',aParams)>0 then
      begin
        Result := copy(aParams,0,pos(' ',aParams)-1);
        aParams := copy(aParams,pos(' ',aParams)+1,length(aParams));
      end
    else
      begin
        Result := aParams;
        aParams := '';
      end;
  end;
  procedure ProcessSetEntry(aEntry : string);
  begin
    aEntry:=trim(aEntry);
    if aEntry='' then exit;
    if pos(':',aEntry) >0 then
      aSQL := aSQL+'('+Data.QuoteField('SQL_ID')+'>'+Data.QuoteValue(copy(aEntry,0,pos(':',aEntry)-1)+' and '+Data.QuoteField('SQL_ID')+'>'+Data.QuoteValue(copy(aEntry,pos(':',aEntry)+1,length(aEntry))))+') or '
    else
      aSQL := aSQL+Data.QuoteField('SQL_ID')+'='+Data.QuoteValue(aEntry)+' or ';
  end;
begin
  //TODO:more Selections possible
  Result:='* SEARCH';
  if aParams <> '' then
    begin
      //convert to filter
      aSQL := Data.QuoteField('TREEENTRY')+'='+Data.QuoteValue(FTreeEntry)+' AND '+Data.QuoteField('USER')+'='+Data.QuoteValue(Data.Users.FieldByName('ACCOUNTNO').AsString)+' and ';
      //unsupported
      //BODY <string>  Messages that contain the specified string in the body of the message.
      //CC <string>    Messages that contain the specified string in the envelope structure's CC field.
      //KEYWORD <flag> Messages with the specified keyword set.
      //LARGER <n>     Messages with an [RFC-822] size larger than the specified number of octets.
      //NEW            Messages that have the \Recent flag set but not the \Seen flag.  This is functionally equivalent to "(RECENT UNSEEN)".
      //OLD            Messages that do not have the \Recent flag set. This is functionally equivalent to "NOT RECENT" (as opposed to "NOT NEW").
      //OR <search-key1> <search-key2> Messages that match either search key.
      //RECENT         Messages that have the \Recent flag set.
      //SMALLER <n>    Messages with an [RFC-822] size smaller than the specified number of octets.
      //TEXT <string>  Messages that contain the specified string in the header or body of the message.
      //TO <string>    Messages that contain the specified string in the envelope structure's TO field.
      //UNANSWERED     Messages that do not have the \Answered flag set.
      //UNDELETED      Messages that do not have the \Deleted flag set.
      //UNDRAFT        Messages that do not have the \Draft flag set.
      //UNKEYWORD <flag> Messages that do not have the specified keyword set.
      //UNSEEN         Messages that do not have the \Seen flag set.
      while length(aParams)>0 do
        begin
          case NextParam of
          'BEFORE'://BEFORE <date>  Messages whose internal date is earlier than the specified date.
            aSQL := aSQL+Data.QuoteField('TIMESTAMPD')+'<'+Data.DateTimeToFilter(SynaUtil.DecodeRfcDateTime(NextParam(False)))+' and ';
          'ON'://ON <date>      Messages whose internal date is within the specified date.
            aSQL := aSQL+Data.QuoteField('TIMESTAMPD')+'='+Data.DateTimeToFilter(SynaUtil.DecodeRfcDateTime(NextParam(False)))+' and ';
          'SINCE'://SINCE <date>   Messages whose internal date is within or later than the specified date.
            aSQL := aSQL+Data.QuoteField('TIMESTAMPD')+'>'+Data.DateTimeToFilter(SynaUtil.DecodeRfcDateTime(NextParam(False)))+' and ';
          'SENTBEFORE'://SENTBEFORE <date> Messages whose [RFC-822] Date: header is earlier than the specified date.
            aSQL := aSQL+Data.QuoteField('SENDDATE')+'<'+Data.DateTimeToFilter(SynaUtil.DecodeRfcDateTime(NextParam(False)))+' and ';
          'SENTON'://SENTON <date>  Messages whose [RFC-822] Date: header is within the specified date.
            aSQL := aSQL+Data.QuoteField('SENDDATE')+'='+Data.DateTimeToFilter(SynaUtil.DecodeRfcDateTime(NextParam(False)))+' and ';
          'SENTSINCE'://SENTSINCE <date> Messages whose [RFC-822] Date: header is within or later than the specified date.
            aSQL := aSQL+Data.QuoteField('SENDDATE')+'>'+Data.DateTimeToFilter(SynaUtil.DecodeRfcDateTime(NextParam(False)))+' and ';
          'DELETED'://DELETED        Messages with the \Deleted flag set.
            aSQL := aSQL+Data.QuoteField('TREEENTRY')+'='+Data.QuoteValue(IntToStr(TREE_ID_DELETED_MESSAGES))+' and ';
          'UNDELETED'://DELETED        Messages with the \Deleted flag set.
            aSQL := aSQL+Data.QuoteField('TREEENTRY')+'<>'+Data.QuoteValue(IntToStr(TREE_ID_DELETED_MESSAGES))+' and ';
          'DRAFT'://DRAFT          Messages with the \Draft flag set.
            aSQL := aSQL+Data.QuoteField('DRAFT')+'='+Data.QuoteValue('Y')+' and ';
          'FLAGGED'://FLAGGED        Messages with the \Flagged flag set.
            aSQL := aSQL+Data.QuoteField('DRAFT')+'='+Data.QuoteValue('Y')+' and ';
          'UNFLAGGED'://UNFLAGGED      Messages that do not have the \Flagged flag set.
            aSQL := aSQL+Data.QuoteField('DRAFT')+'<>'+Data.QuoteValue('Y')+' and ';
          'SEEN'://SEEN           Messages that have the \Seen flag set.
            aSQL := aSQL+Data.QuoteField('READ')+'='+Data.QuoteValue('Y')+' and ';
          'FROM'://FROM <string>  Messages that contain the specified string in the envelope structure's FROM field.
            aSQL := aSQL+Data.ProcessTerm(Data.QuoteField('SENDER')+'='+Data.QuoteValue('*'+NextParam(False)+'*'))+' and ';
          'SUBJECT'://SUBJECT <string> Messages that contain the specified string in the envelope structure's SUBJECT field.
            aSQL := aSQL+Data.ProcessTerm(Data.QuoteField('SUBJECT')+'='+Data.QuoteValue('*'+NextParam(False)+'*'))+' and ';
          'HEADER'://HEADER <field-name> <string> Messages that have a header with the specified field-name (as defined in [RFC-822]) and that contains the specified string in the [RFC-822] field-body.
             begin
               case NextParam of
               'MESSAGE-ID':
                 aSQL := aSQL+Data.QuoteField('ID')+'='+Data.QuoteValue(GetmailAddr(NextParam(False)))+' and ';
               else
                 begin
                   raise Exception.Create('criteria not allowed');
                   exit;
                 end;
               end;
             end;
          'UID'://UID <message set> Messages with unique identifiers corresponding to the specified unique identifier set.
             begin
               aSQL := aSQL+'(';
               aSet := NextParam(False);
               while pos(',',aSet)>0 do
                 begin
                   ProcessSetEntry(copy(aSet,0,pos(',',aSet)-1));
                   aSet := copy(aSet,pos(',',aSet)+1,length(aSet));
                 end;
               ProcessSetEntry(aSet);
               aSQL := aSQL+')';
             end;
          'NOT'://NOT <search-key> Messages that do not match the specified search key.
            aSQL := aSQL+' not ';
          else
            begin
              raise Exception.Create('criteria not allowed');
              exit;
            end;
          end;
        end;
      FMessages.Filter(copy(aSQL,0,length(aSQL)-5));
      FMessages.DataSet.Last;
    end;
  while (FSelectCount>0) and (not FMessages.DataSet.BOF) do
    begin
      if FSequenceNumbers.IndexOf(FMessages.Id.AsString)=-1 then
        tmpRecNo := IntToStr(FSequenceNumbers.Add(FMessages.Id.AsString)+1)
      else tmpRecNo:=IntToStr(FSequenceNumbers.IndexOf(FMessages.Id.AsString)+1);
      Found := True;
      if FUseUID then
        Result := Result+' '+FMessages.Id.AsString
      else
        Result := Result+' '+tmpRecNo;
      dec(FSelectCount);
      FMessages.Prior;
    end;
end;

constructor TPIMAPFolder.Create(aParent: TIMAPFolders; aName: string;
  aUID: string);
begin
  FSequenceNumbers := TStringList.Create;
  FetchSequence := 1;
  FMessages := TMessageList.Create(Self,Data);
  if Data.Tree.DataSet.Locate('SQL_ID',aUID,[loCaseInsensitive]) then
    FTreeEntry := Data.Tree.Id.AsString
  else if Data.Tree.DataSet.Locate('NAME',aName,[loCaseInsensitive]) then
    FTreeEntry := Data.Tree.Id.AsString;
  FGroupName := aName;
  RefreshFirstID;
  inherited Create(aParent,FGroupName,FTreeEntry);
end;
destructor TPIMAPFolder.Destroy;
begin
  FSequenceNumbers.Free;
  if Assigned(FMessages) then
    FMessages.Free;
  inherited Destroy;
end;
end.

