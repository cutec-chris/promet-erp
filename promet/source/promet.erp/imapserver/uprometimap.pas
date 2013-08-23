unit uprometimap;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, uLIMAP, uMessages, MimeMess, uMimeMessages, db,cwstring;
type
  TPIMAPFolder = class(TIMAPFolder)
  private
    FMessages : TMessageList;
    FTreeEntry : string;
    FGroupName : string;
    FCount : Integer;
    FUnreadCount : Integer;
    FLastID : LargeInt;
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
  Data.SetFilter(FMessages,Data.QuoteField('TREEENTRY')+'='+Data.QuoteValue(FTreeEntry),1,'ASC');
  if FMessages.Count > 0 then
    FFirstID := FMessages.Id.AsVariant;
end;
procedure TPIMAPFolder.RefreshCount;
begin
  FFIrstID := 0;
  FlastID := 0;
  Data.SetFilter(FMessages,Data.QuoteField('TREEENTRY')+'='+Data.QuoteValue(FTreeEntry)+' and '+Data.QuoteField('READ')+'='+Data.QuoteValue('N'),0,'ASC');
  FUnreadCount := FMessages.Count;
  Data.SetFilter(FMessages,Data.QuoteField('TREEENTRY')+'='+Data.QuoteValue(FTreeEntry),0,'ASC');
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
  Data.SetFilter(FMessages,Data.QuoteField('TREEENTRY')+'='+Data.QuoteValue(FTreeEntry)+' AND '+Data.QuoteField('SQL_ID')+'='+Data.QuoteValue(IntToStr(Idx)),1,'ASC');
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
  if not aUseUID then
    begin
      aFilter := Data.QuoteField('TREEENTRY')+'='+Data.QuoteValue(FTreeEntry);
      Max := StrToInt(Arg1);
      Data.SetFilter(FMessages,aFilter,Max,'ASC');
      FMessages.DataSet.last;
      Arg1 := FMessages.Id.AsString;
    end;
  if (trim(Arg2) = '') or (trim(Arg2) = '*') then
    aFilter := Data.QuoteField('TREEENTRY')+'='+Data.QuoteValue(FTreeEntry)+' AND '+Data.QuoteField('SQL_ID')+' >= '+Data.QuoteValue(Arg1)
  else
    begin
      aFilter := Data.QuoteField('TREEENTRY')+'='+Data.QuoteValue(FTreeEntry)+' AND '+Data.QuoteField('SQL_ID')+' >= '+Data.QuoteValue(Arg1)+' AND '+Data.QuoteField('SQL_ID')+' <= '+Data.QuoteValue(Arg2);
      try
        Max := (StrToInt(Arg2)-StrToInt(Arg1))+1;
        aFilter := Data.QuoteField('TREEENTRY')+'='+Data.QuoteValue(FTreeEntry)+' AND '+Data.QuoteField('SQL_ID')+' >= '+Data.QuoteValue(Arg1);
      except
        aFilter := Data.QuoteField('TREEENTRY')+'='+Data.QuoteValue(FTreeEntry)+' AND '+Data.QuoteField('SQL_ID')+' >= '+Data.QuoteValue(Arg1)+' AND '+Data.QuoteField('SQL_ID')+' <= '+Data.QuoteValue(Arg2);
        Max := 0;
      end;
    end;
  Data.SetFilter(FMessages,aFilter,Max,'ASC');
  FMessages.DataSet.First;
  Result := True;
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
begin
  Result := TStringList.Create;
  i := 1;
  aFetch := aFetch+' ';
  if not FMessages.DataSet.EOF then
    begin
      tmp := '* '+IntToStr(i)+' FETCH (';
      if pos('UID',aFetch)>0 then
        begin
          tmp := tmp+'UID '+FMessages.FieldByName('SQL_ID').AsString+' ';
        end;
      if pos('FLAGS',aFetch)>0 then
        begin
          tmp := tmp+'FLAGS (';
          if FMessages.FieldByName('READ').AsString='Y' then
            tmp+='\Seen ';
          if FMessages.FieldByName('READ').AsString='Y' then
            tmp+='\Answered ';
          if FMessages.FieldByName('TREEENTRY').AsVariant=TREE_ID_DELETED_MESSAGES then
            tmp+='\Deleted ';
          tmp := copy(tmp,0,length(tmp)-1);
          tmp+=') ';
        end;
      if pos('INTERNALDATE',aFetch)>0 then
        begin
          tmp := tmp+'INTERNALDATE "'+Rfc822DateTime(FMessages.FieldByName('SENDDATE').AsDateTime)+'" ';
        end;
      if pos('RFC822.SIZE',aFetch)>0 then
        begin
          tmp := tmp+'RFC822.SIZE '+FMessages.FieldByName('SIZE').AsString+' ';
        end;
      if (pos('RFC822.HEADER ',aFetch)>0)
      or (pos('BODY[HEADER] ',aFetch)>0)
      then
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
      if (pos('RFC822 ',aFetch)>0)
      or (pos('BODY[] ',aFetch)>0)
      then
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
          aLen := 0;
          aLen :=  length(aSL.Text);
          if (pos('RFC822 ',aFetch)>0) then
            tmp := tmp+'RFC822 {'+IntToStr(aLen+2)+'}'+#13#10+aSL.Text;
          if (pos('BODY[] ',aFetch)>0) then
            tmp := tmp+'BODY[] {'+IntToStr(aLen+2)+'}'+#13#10+aSL.Text;
          aSL.Free;
        end;
      if pos('BODY.PEEK[HEADER.FIELDS (',aFetch)>0 then
        begin
          if not Assigned(aMessage) then
            begin
              aMessage := TMimeMessage.Create(Self,Data);
              aMessage.Select(FMessages.Id.AsVariant);
              aMessage.Open;
            end;
          aMime := aMessage.EncodeMessage;
          //BODY.PEEK[HEADER.FIELDS (From To Cc Bcc Subject Date Message-ID Priority X-Priority References Newsgroups In-Reply-To Content-Type)]
          aFields := copy(aFetch,pos('BODY.PEEK[HEADER.FIELDS (',aFetch)+25,length(aFetch));
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
//              if not Found then
//                aSL.Add(copy(aFields,0,pos(' ',aFields)-1)+':');
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
//              if not Found then
//                aSL.Add(aFields+':');
              aFields := copy(aFields,pos(' ',aFields)+1,length(aFields));
            end;
          aLen := 0;
          aLen :=  length(aSL.Text);
          aSl.Add('');
          aSl.Add('');
          aFields := copy(aFetch,pos('BODY.PEEK[HEADER.FIELDS (',aFetch),length(aFetch));
          aFields := copy(aFields,0,pos(']',aFields));
          aFields := StringReplace(aFields,'BODY.PEEK[','BODY[',[]);
          tmp := tmp+aFields+' {'+IntToStr(aLen)+'}'+#13#10+aSL.Text;
          aSL.Free;
          bSL.Free;
          aMime.Free;
        end;
      Result.Text := Result.Text+copy(tmp,0,length(tmp)-1)+')';
      i:=i+1;
      FreeAndNil(aMessage);
      Fmessages.DataSet.Next;
    end
  else
    FreeAndNil(Result);
end;

constructor TPIMAPFolder.Create(aName: string;aUID : string);
begin
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

