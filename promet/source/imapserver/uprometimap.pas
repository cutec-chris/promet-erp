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
  Classes, SysUtils, uLIMAP, uMessages, MimeMess, uMimeMessages, db,cwstring;
type

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
    FSelectCount : Integer;
    FUseUID : Boolean;
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
  public
    constructor Create(aName : string;aUID : string);override;
    destructor Destroy;override;
  end;

implementation
uses uData,Variants,SynaUtil,uSessionDBClasses,uBaseDBInterface,Utils,
  uPerson,LConvEncoding,uIntfStrConsts;

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
  FMessages.DataSet.First;
  if not FUseUID then
    FMessages.DataSet.MoveBy(StrToInt(Arg1)-1)
  else
    begin
      Result := FMessages.GotoBookmark(StrToInt(Arg1));
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
  FMessages.Filter(Data.QuoteField('TREEENTRY')+'='+Data.QuoteValue(FTreeEntry)+' AND '+Data.QuoteField('USER')+'='+Data.QuoteValue(Data.Users.FieldByName('ACCOUNTNO').AsString),100,'SENDDATE');
  FMessages.First;
  if FMessages.Count > 0 then
    FFirstID := FMessages.Id.AsVariant;
end;
procedure TPIMAPFolder.RefreshCount;
begin
  FFIrstID := 0;
  FlastID := 0;
  Data.SetFilter(FMessages,Data.QuoteField('TREEENTRY')+'='+Data.QuoteValue(FTreeEntry)+' and '+Data.QuoteField('READ')+'='+Data.QuoteValue('N')+' AND '+Data.QuoteField('USER')+'='+Data.QuoteValue(Data.Users.FieldByName('ACCOUNTNO').AsString),100,'SENDDATE');
  FUnreadCount := FMessages.Count;
  Data.SetFilter(FMessages,Data.QuoteField('TREEENTRY')+'='+Data.QuoteValue(FTreeEntry)+' AND '+Data.QuoteField('USER')+'='+Data.QuoteValue(Data.Users.FieldByName('ACCOUNTNO').AsString),100,'SENDDATE');
  if FMessages.Count > 0 then
    begin
      FFirstID:=FMessages.Id.AsVariant;
      Fmessages.DataSet.Last;
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
  randomize;
  aID := '';
  for i := 0 to 45 do
    begin
      aChr := chr(ord('0')+random(74));
      if aChr in ['a'..'z','0'..'9','A'..'Z'] then
        aID := aID+aChr;
    end;
  aMsg.Header.MessageID := aID;
  aMessage := TMimeMessage.Create(Self,Data);
  aMessage.Select(0);
  aMessage.Open;
  aMessage.DataSet.Insert;
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
      if Data.Users.DataSet.Locate('NAME',aUser,[loCaseInsensitive]) then
      Data.Users.History.AddItemWithoutUser(Customers.DataSet,Format(strActionMessageReceived,[aSubject]),
                                    'MESSAGEIDX@'+aMessage.FieldByName('ID').AsString+'{'+aSubject+'}',
                                    '',
                                    nil,
                                    ACICON_MAILNEW);
    end;
  aMsg.Free;
  aMessage.DataSet.Post;
  Result := True;
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
  Result := SelectNext;
end;

function TPIMAPFolder.FetchOneEntry(aFetch: string): TStrings;
var
  i: Integer;
  tmp: String;
  aMessage: TMimeMessage = nil;
  aSL: TStringList;
  aLen: Integer;
  a: Integer;
  aMime: TMimeMess;
  aFields: String;
  bsl: TStringList;
  Found: Boolean;
  bFetch: String;
  aSize: String;
begin
  if FSelectCount=0 then
    if not SelectNext then
      begin
        Result := nil;
        exit;
      end;
  Result := TStringList.Create;
  aFetch := aFetch+' ';
  if not FMessages.DataSet.EOF then
    begin
      tmp := '* '+IntToStr(FMessages.DataSet.RecNo)+' FETCH (';
      while pos(' ',aFetch)>0 do
        begin
          bFetch := copy(afetch,0,pos(' ',afetch)-1);
          if pos('<',bFetch)>0 then
            bfetch := copy(bFetch,0,pos('<',bFetch)-1);
          afetch := copy(afetch,pos(' ',afetch)+1,length(afetch));
          case bFetch of
          'UID':
            begin
              tmp := tmp+'UID '+FMessages.FieldByName('SQL_ID').AsString+' ';
            end;
          'FLAGS','(FLAGS)':
            begin
              tmp := tmp+'FLAGS (';
              if FMessages.FieldByName('READ').AsString='Y' then
                tmp+='\Seen ';
              if FMessages.FieldByName('ANSWERED').AsString='Y' then
                tmp+='\Answered ';
              if FMessages.FieldByName('TREEENTRY').AsVariant=TREE_ID_DELETED_MESSAGES then
                tmp+='\Deleted '
              else tmp +=' ';
              tmp := copy(tmp,0,length(tmp)-1);
              tmp+=') ';
            end;
          'INTERNALDATE':
            begin
              tmp := tmp+'INTERNALDATE "'+Rfc822DateTime(FMessages.FieldByName('SENDDATE').AsDateTime)+'" ';
            end;
          'RFC822.SIZE':
            begin
              aSize := FMessages.FieldByName('SIZE').AsString;
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
              aLen := 0;
              aLen :=  length(aSL.Text);
              if (pos('RFC822.HEADER ',aFetch)>0) then
                tmp := tmp+'RFC822.HEADER {'+IntToStr(aLen+2)+'}'+#13#10+aSL.Text;
              if (pos('BODY[HEADER] ',aFetch)>0) then
                tmp := tmp+'BODY[HEADER] {'+IntToStr(aLen+2)+'}'+#13#10+aSL.Text;
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
              aMime := aMessage.EncodeMessage;
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
              aMime.Free;
            end;
          'BODY.PEEK[HEADER.FIELDS':
            begin
              if not Assigned(aMessage) then
                begin
                  aMessage := TMimeMessage.Create(Self,Data);
                  aMessage.Select(FMessages.Id.AsVariant);
                  aMessage.Open;
                end;
              aMime := aMessage.EncodeMessage;
              //BODY.PEEK[HEADER.FIELDS (From To Cc Bcc Subject Date Message-ID Priority X-Priority References Newsgroups In-Reply-To Content-Type)]
              aFields := copy(aFetch,pos('(',aFetch)+1,length(aFetch));
              aFields := copy(aFields,0,pos(')',aFields)-1);
              aSL := TStringList.Create;
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
                  if not Found then
                    aSL.Add(copy(aFields,0,pos(' ',aFields)-1)+':');
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
                  if not Found then
                    aSL.Add(aFields+':');
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
              aMime.Free;
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
      FetchSequence:=FetchSequence+1;
      FreeAndNil(aMessage);
      Fmessages.DataSet.Next;
      Dec(FSelectCount,1);
    end
  else
    FreeAndNil(Result);
end;

constructor TPIMAPFolder.Create(aName: string;aUID : string);
begin
  FetchSequence := 1;
  FMessages := TMessageList.Create(Self,Data);
  if Data.Tree.DataSet.Locate('SQL_ID',aUID,[loCaseInsensitive]) then
    FTreeEntry := Data.Tree.Id.AsString
  else if Data.Tree.DataSet.Locate('NAME',aName,[loCaseInsensitive]) then
    FTreeEntry := Data.Tree.Id.AsString;
  FGroupName := aName;
  RefreshFirstID;
  inherited Create(FGroupName,FTreeEntry)
end;
destructor TPIMAPFolder.Destroy;
begin
  if Assigned(FMessages) then
    FMessages.Free;
  inherited Destroy;
end;
end.

