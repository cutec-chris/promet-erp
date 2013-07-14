program mta;
{$mode objfpc}{$H+}
uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, pcmdprometapp, CustApp, uBaseCustomApplication, lnetbase,
  lNet, uBaseDBInterface, md5,uData,eventlog,
  pmimemessages, fileutil,lconvencoding,uBaseApplication, ulsmtpsrv,
  dnssend,smtpsend,synamisc,uBaseDbClasses,uMimeMessages,mimemess,laz_synapse,
  synautil,uPerson,db,Utils,variants,uMessages,LCLProc,LCLIntf;
resourcestring
  strActionMessageReceived                   = '%s';
type
  TMessageSubscribings = class(TBaseDBDataset)
  public
    procedure DefineFields(aDataSet: TDataSet); override;
    procedure SelectByRef(aID: Variant);
  end;

  { TPMTAServer }

  TPMTAServer = class(TBaseCustomApplication)
    function ServerAcceptMail(aSocket: TLSMTPSocket; aFrom: string;
      aTo: TStrings): Boolean;
    procedure ServerLog(aSocket: TLSMTPSocket; DirectionIn: Boolean;
      aMessage: string);
    function ServerLogin(aSocket: TLSMTPSocket; aUser, aPasswort: string
      ): Boolean;
    procedure ServerMailreceived(aSocket: TLSMTPSocket; aMail: TStrings;aFrom : string;aTo : TStrings);
  private
    Server: TLSMTPServer;
    Subscribers: TMessageSubscribings;
    NextCollectTime : TDateTime;
    NextSendTime : TDateTime;
    Defaultdomain : string;
    procedure DoCollectGroupMessages;
    procedure DoSendMails;
    procedure DoSendMail(aTo,aFrom,aSubject,aText : string);
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

procedure TMessageSubscribings.DefineFields(aDataSet: TDataSet);
begin
  with aDataSet as IBaseManageDB do
    begin
      TableName := 'SUBSCRIBERS';
      if Assigned(ManagedFieldDefs) then
        with ManagedFieldDefs do
          begin
            Add('REF_ID',ftLargeInt,0,True);
            Add('NAME',ftString,200,False);
            Add('EMAIL',ftString,200,True);
            Add('LASTMESSAGE',ftDateTime,0,True);
          end;
    end;
end;

procedure TMessageSubscribings.SelectByRef(aID: Variant);
var
  aField: String = '';
begin
  with BaseApplication as IBaseDBInterface do
    with DataSet as IBaseDBFilter do
      begin
        aField := 'REF_ID';
        if (VarIsNumeric(aID) and (aID = 0))
        or (VarIsStr(aID) and (aID = ''))
        or (aID = Null)  then
          begin
            with DataSet as IBaseManageDb do
              Filter := Data.QuoteField(TableName)+'.'+Data.QuoteField(aField)+'='+Data.QuoteValue('0');
          end
        else
          begin
            with DataSet as IBaseManageDb do
              Filter := Data.QuoteField(TableName)+'.'+Data.QuoteField(aField)+'='+Data.QuoteValue(Format('%d',[Int64(aID)]));
          end;
      end;
end;

procedure TPMTAServer.ServerLog(aSocket: TLSMTPSocket; DirectionIn: Boolean;
  aMessage: string);
begin
  with Self as IBaseApplication do
    begin
      if DirectionIn then
        begin
          Info(IntToStr(aSocket.Id)+':>'+aMessage);
        end
      else
        begin
          Info(IntToStr(aSocket.Id)+':<'+aMessage);
        end;
    end;
end;

function TPMTAServer.ServerLogin(aSocket: TLSMTPSocket; aUser,
  aPasswort: string): Boolean;
begin
  Result := False;
  with Self as IBaseDBInterface do
    begin
      if Data.Users.DataSet.Locate('LOGINNAME',aUser,[]) then
        begin
          if (Data.Users.CheckPasswort(aPasswort)) then
            Result := True;
        end;
    end;
  with Self as IBaseApplication do
    begin
      if Result then
        Log('Login:'+aUser)
      else
        Error('Login failed:'+aUser);
    end;
end;

function TPMTAServer.ServerAcceptMail(aSocket: TLSMTPSocket; aFrom: string;
  aTo: TStrings): Boolean;
var
  aUser: TUser;
  aRes: Boolean;
  i: Integer;
  tmp: String;
begin
  aRes := aTo.Count>0;
  aUser := TUser.Create(nil,Data);
  for i := 0 to aTo.Count-1 do
    begin
      if Data.IsSQLDb then
        Data.SetFilter(aUser,'UPPER("EMAIL")=UPPER('''+GetEmailAddr(aTo[i])+''')')
      else
        Data.SetFilter(aUser,'"EMAIL"='''+GetEmailAddr(aTo[i])+'''');
      aRes := aRes and (aUser.Count>0);
    end;
  aUser.Free;
  if (not aRes) and (aTo.Count>0) then
    begin
      for i := 0 to aTo.Count-1 do
        begin
          tmp := GetEmailAddr(aTo[i]);
          tmp := copy(tmp,0,pos('@',tmp)-1);
          ares := Data.Tree.DataSet.Locate('NAME',tmp,[loCaseInsensitive]);
          if ares then break;
        end;
    end;
  Result := aRes;
end;

procedure TPMTAServer.ServerMailreceived(aSocket: TLSMTPSocket;
  aMail: TStrings; aFrom: string; aTo: TStrings);
var
  y,m,d,h,mm,s,ss: word;
  aUser: TUser;
  aRes: Boolean;
  aMessage: TMimeMessage;
  msg: TMimeMess;
  i: Integer;
  atmp: String;
  CustomerCont: TPersonContactData;
  Customers: TPerson;
  aSubject: String;
  aTreeEntry: Integer;
  tmp: String;
begin
  //Try to find User
  aRes := aTo.Count>0;
  for i := 0 to aTo.Count-1 do
    begin
      aUser := TUser.Create(nil,Data);
      if Data.IsSQLDb then
        Data.SetFilter(aUser,'UPPER("EMAIL")=UPPER('''+GetEmailAddr(aTo[i])+''')')
      else
        Data.SetFilter(aUser,'"EMAIL"='''+GetEmailAddr(aTo[i])+'''');
      if aUser.Count>0 then
        begin
          aMessage := TMimeMessage.Create(nil,Data);
          aMessage.Insert;
          aMessage.FieldByName('USER').AsString := aUser.DataSet.FieldByName('ACCOUNTNO').AsString;
          aMessage.FieldByName('TYPE').AsString := 'EMAIL';
          aMessage.FieldByName('READ').AsString := 'N';
          msg := TMimeMess.Create;
          msg.Lines.Text:=aMail.Text;
          msg.DecodeMessage;
          aMessage.DecodeMessage(msg);
          atmp:=ConvertEncoding(getemailaddr(msg.Header.From),GuessEncoding(getemailaddr(msg.Header.From)),EncodingUTF8);
          aSubject :=ConvertEncoding(getemailaddr(msg.Header.Subject),GuessEncoding(getemailaddr(msg.Header.Subject)),EncodingUTF8);
          CustomerCont := TPersonContactData.Create(Self,Data);
          if Data.IsSQLDb then
            Data.SetFilter(CustomerCont,'UPPER("DATA")=UPPER('''+atmp+''')')
          else
            Data.SetFilter(CustomerCont,'"DATA"='''+atmp+'''');
          Customers := TPerson.Create(Self,Data);
          Customers.SelectByAccountNo(CustomerCont.DataSet.FieldByName('ACCOUNTNO').AsString);
          Customers.Open;
          CustomerCont.Free;
          if Customers.Count > 0 then
            begin
              Customers.History.Open;
              Customers.History.AddItem(Customers.DataSet,Format(strActionMessageReceived,[aSubject]),
                                        Data.BuildLink(aMessage.DataSet),
                                        '',
                                        nil,
                                        ACICON_MAILNEW);
              aTreeEntry := TREE_ID_MESSAGES;
              aUser.History.AddItemWithoutUser(Customers.DataSet,Format(strActionMessageReceived,[aSubject]),
                                            Data.BuildLink(aMessage.DataSet),
                                            '',
                                            nil,
                                            ACICON_MAILNEW);
            end
          else
            begin
              aTreeEntry := TREE_ID_UNKNOWN_MESSAGES;
            end;
          if not aMessage.CanEdit then aMessage.DataSet.Edit;
          aMessage.FieldByName('TREEENTRY').AsInteger:=aTreeEntry;
          aMessage.Post;
          msg.Free;
          aMessage.Free;
        end
      else aRes := False;
      aUser.Free;
    end;
  if (not aRes) and (aTo.Count>0) then //try to find group
    begin
      for i := 0 to aTo.Count-1 do
        begin
          tmp := GetEmailAddr(aTo[i]);
          tmp := copy(tmp,0,pos('@',tmp)-1);
          ares := Data.Tree.DataSet.Locate('NAME',tmp,[loCaseInsensitive]);
          if ares then
            begin
              msg := TMimeMess.Create;
              msg.Lines.Text:=aMail.Text;
              msg.DecodeMessage;
              if lowercase(copy(msg.Header.Subject,0,9))='subscribe' then
                begin
                  Info('subscribing '+msg.Header.From);
                  if Data.IsSQLDb then
                    Data.SetFilter(Subscribers,'UPPER("EMAIL")=UPPER('''+GetEmailAddr(msg.Header.From)+''')')
                  else
                    Data.SetFilter(Subscribers,'"EMAIL"='''+GetEmailAddr(msg.Header.From)+'''');
                  if Subscribers.Count=0 then
                    begin
                      Subscribers.Append;
                      Subscribers.FieldByName('REF_ID').AsVariant:=Data.Tree.Id.AsVariant;
                      Subscribers.FieldByName('EMAIL').AsString := GetEmailAddr(msg.Header.From);
                      Subscribers.FieldByName('NAME').AsString := GetEmailDesc(msg.Header.From);
                      Subscribers.FieldByName('LASTMESSAGE').AsDateTime:=Now();
                      tmp := lowercase(trim(copy(msg.Header.Subject,11,length(msg.Header.Subject))));
                      if copy(tmp,length(tmp),1)='w' then
                        begin
                          tmp := copy(tmp,0,length(tmp)-1);
                          if Isnumeric(tmp) then
                            Subscribers.FieldByName('LASTMESSAGE').AsDateTime:=Now()-(StrToInt(tmp)*7);
                        end
                      else if IsNumeric(tmp) then
                        Subscribers.FieldByName('LASTMESSAGE').AsDateTime:=Now()-StrToInt(tmp);
                      Subscribers.Post;
                      tmp := GetEmailAddr(aTo[i]);
                      tmp := copy(tmp,0,pos('@',tmp)-1);
                      DoSendMail(msg.Header.From,aTo[i],'['+Data.Tree.FieldByName('NAME').AsString+'] '+'Subscribing succesful',Format('You are successful subscribed on "%s" and you will get older Messages since %s in the next minutes.'+LineEnding+'You can now send Messages to '+aTo[i]+' to post a Message (and send it to the Group). Also you can answer to messages from the Group directly.',[tmp,DateTimeToStr( Subscribers.FieldByName('LASTMESSAGE').AsDateTime)]));
                    end;
                end
              else if lowercase(copy(msg.Header.Subject,0,11))='unsubscribe' then
                begin
                  Info('unsubscribing '+msg.Header.From);
                  if Data.IsSQLDb then
                    Data.SetFilter(Subscribers,'UPPER("EMAIL")=UPPER('''+GetEmailAddr(msg.Header.From)+''')')
                  else
                    Data.SetFilter(Subscribers,'"EMAIL"='''+GetEmailAddr(msg.Header.From)+'''');
                  while Subscribers.Count>0 do
                    Subscribers.Delete;
                  tmp := GetEmailAddr(aTo[i]);
                  tmp := copy(tmp,0,pos('@',tmp)-1);
                  DoSendMail(msg.Header.From,aTo[i],'['+Data.Tree.FieldByName('NAME').AsString+'] '+'Unsubscribing succesful',Format('You will not longer get Messages from the group "%s".',[tmp]));
                end
              else
                begin
                  aMessage := TMimeMessage.Create(nil,Data);
                  aMessage.Insert;
                  aMessage.FieldByName('TYPE').AsString := 'EMAIL';
                  aMessage.FieldByName('READ').AsString := 'N';
                  aMessage.FieldByName('USER').AsString := '*';
                  aMessage.FieldByName('TREEENTRY').AsVariant:=Data.Tree.Id.AsVariant;
                  aMessage.DecodeMessage(msg);
                  if not Data.Numbers.HasNumberSet('NG.'+aMessage.FieldByName('TREEENTRY').AsString) then
                    begin
                      Data.Numbers.Insert;
                      Data.Numbers.FieldByName('TABLENAME').AsString:='NG.'+aMessage.FieldByName('TREEENTRY').AsString;
                      Data.Numbers.FieldByName('TYPE').AsString:='N';
                      Data.Numbers.FieldByName('INCR').AsInteger:=1;
                      Data.Numbers.FieldByName('ACTUAL').AsVariant:=1;
                      Data.Numbers.FieldByName('STOP').AsVariant:=9999999999;
                      Data.Numbers.DataSet.Post;
                    end;
                  if not aMessage.CanEdit then aMessage.DataSet.Edit;
                  aMessage.FieldByName('GRP_ID').AsString:=Data.Numbers.GetNewNumber('NG.'+aMessage.FieldByName('TREEENTRY').AsString);
                  aMessage.Post;
                  aMessage.Free;
                end;
              msg.Free;
              break;
            end;
        end;
    end;
  if not aRes then
    begin
      DecodeDate(Now(),y,m,d);
      DecodeTime(Now(),h,mm,s,ss);
      aMail.SaveToFile(Format('mail_%.4d-%.2d-%.2d %.2d_%.2d_%.2d_%.4d.msg',[y,m,d,h,mm,s,ss]));
    end;
end;

procedure TPMTAServer.DoCollectGroupMessages;
var
  aMessage: TMessage;
  aFilter: String;
  bMessages: TMessageList;
begin
  if NextCollectTime>Now() then exit;
  NextCollectTime:=Now+((1/MinsPerDay)*0.5);
  aMessage := TMessage.Create(nil,Data);
  bMessages := TMessageList.Create(nil,Data);
  with Data.Tree.DataSet do
    begin
      First;
      while not EOF do
        begin
          Subscribers.SelectByRef(Data.Tree.id.AsVariant);
          Subscribers.Open;
          Subscribers.First;
          while not Subscribers.EOF do
            begin
              if Data.Tree.DataSet.FieldDefs.IndexOf('ID')>-1 then
                aFilter := Data.QuoteField('TREEENTRY')+'='+Data.QuoteValue(Data.Tree.FieldByName('ID').AsString)+' AND '+Data.QuoteField('SENDDATE')+' >= '+Data.DateTimeToFilter(Subscribers.FieldByName('LASTMESSAGE').AsDateTime)
              else
                aFilter := Data.QuoteField('TREEENTRY')+'='+Data.QuoteValue(Data.Tree.Id.AsString)+' AND '+Data.QuoteField('SENDDATE')+' >= '+Data.DateTimeToFilter(Subscribers.FieldByName('LASTMESSAGE').AsDateTime);
              Data.SetFilter(aMessage,aFilter,100,'GRP_ID','ASC');
              if aMessage.Count>0 then
                begin
                  if not Subscribers.CanEdit then Subscribers.DataSet.Edit;
                  Subscribers.FieldByName('LASTMESSAGE').AsDateTime:=Now();
                  Subscribers.Post;
                  aMessage.First;
                  while not aMessage.EOF do
                    begin
                      bMessages.Append;
                      bMessages.DataSet.FieldByName('USER').AsString:='*';
                      bMessages.DataSet.FieldByName('TREEENTRY').AsInteger:=TREE_ID_SEND_MESSAGES;
                      bMessages.DataSet.FieldByName('ID').AsVariant:=aMessage.DataSet.FieldByName('ID').AsVariant;
                      bMessages.DataSet.FieldByName('MSG_ID').AsVariant:=aMessage.DataSet.FieldByName('MSG_ID').AsVariant;
                      bMessages.DataSet.FieldByName('GRP_ID').AsVariant:=aMessage.DataSet.FieldByName('GRP_ID').AsVariant;
                      bMessages.DataSet.FieldByName('TYPE').AsVariant:=aMessage.DataSet.FieldByName('TYPE').AsVariant;
                      bMessages.DataSet.FieldByName('READ').AsString:='N';
                      bMessages.DataSet.FieldByName('SENDER').AsVariant:=aMessage.DataSet.FieldByName('REPLYTO').AsVariant;
                      if bMessages.DataSet.FieldByName('SENDER').AsString='' then
                        bMessages.DataSet.FieldByName('SENDER').AsVariant := aMessage.DataSet.FieldByName('SENDER').AsVariant;
                      bMessages.DataSet.FieldByName('SENDDATE').AsDateTime:=Now();
                      bMessages.DataSet.FieldByName('RECEIVERS').AsString:=Subscribers.FieldByName('EMAIL').AsString;
                      bMessages.DataSet.FieldByName('REPLYTO').AsVariant:=Data.Tree.FieldByName('NAME').AsString+'@'+defaultdomain;
                      bMessages.DataSet.FieldByName('SUBJECT').AsVariant:='['+Data.Tree.FieldByName('NAME').AsString+'] '+aMessage.DataSet.FieldByName('SUBJECT').AsVariant;
                      bMessages.DataSet.FieldByName('PARENT').AsVariant:=aMessage.DataSet.FieldByName('PARENT').AsVariant;
                      bMessages.DataSet.Post;
                      aMessage.Next;
                    end;
                end;
              Subscribers.Next;
            end;
          Next;
        end;
    end;
  aMessage.Free;
  bMessages.Free;
end;

procedure TPMTAServer.DoSendMails;
var
  aServers: TStringList;
  DNSServers: TStringList;
  MessageIndex: TMessageList;
  i: Integer;
  smtp: TSMTPSend;
  aMessage: TMimeMessage;
  msg: TMimeMess;
  aTo: String;
begin
  if NextSendTime>Now() then exit;
  NextSendTime:=Now+((1/MinsPerDay)*1);
  aServers := tStringList.Create;
  DNSServers := TStringList.Create;
  MessageIndex := TmessageList.Create(nil,data);
  Data.SetFilter(MessageIndex,Data.QuoteField('TREEENTRY')+'='+Data.QuoteValue(IntToStr(TREE_ID_SEND_MESSAGES))+' AND '+Data.QuoteField('READ')+'='+Data.QuoteValue('N'));
  if MessageIndex.Count>0 then
    begin
      DNSServers.CommaText:=GetDNS;
      while not MessageIndex.DataSet.EOF do
        begin
          aMessage := TMimeMessage.Create(nil,Data);
          aMessage.Select(MessageIndex.ID.AsVariant);
          aMessage.Open;
          if aMessage.Count>0 then
            begin
              aTo := aMessage.FieldByName('RECEIVERS').AsString;
              for i := 0 to DNSServers.Count-1 do
                begin
                  GetMailServers(DNSServers[i],copy(aTo,pos('@',aTo)+1,length(aTo)),aServers);
                  if aServers.Count>0 then break;
                end;
              if aServers.Count>0 then
                begin
                  smtp := TSMTPSend.Create;
                  smtp.TargetHost := aServers[0];
                  if SMTP.Login then
                    begin
                      with aMessage.DataSet do
                        begin
                          msg := aMessage.EncodeMessage;
                          msg.Header.ToList.Clear;
                          msg.Header.ToList.Add(aTo);
                          msg.EncodeMessage;
                          Info('sending Message "'+msg.Header.Subject+'" to '+aTo);
                          if smtp.MailFrom(msg.Header.ReplyTo,length(msg.ToString)) then
                            if smtp.MailTo(msg.Header.ToList[0]) then
                              begin
                                if smtp.MailData(msg.Lines) then
                                  begin
                                    if not aMessage.CanEdit then amessage.DataSet.Edit;
                                    aMessage.FieldByName('READ').AsString:='Y';
                                    aMessage.Post;
                                    Info('->ok');
                                  end;
                              end;
                         if aMessage.FieldByName('READ').AsString <> 'Y' then
                           Info('->failed '+smtp.FullResult.Text);
                        end;
                      smtp.Logout;
                    end;
                  smtp.Free;
                end;
            end;
          aMessage.Free;
          MessageIndex.Next;
        end;
    end;
  DNSServers.Free;
  aServers.Free;
  MessageIndex.Free;
end;

procedure TPMTAServer.DoSendMail(aTo, aFrom, aSubject, aText: string);
var
  aMessage: TMessage;
  tmpID: String;
  aGUID: TGUID;
  atxt: TStringList;
  aStrm: TStringStream;
begin
  aMessage := TMessage.Create(nil,Data);
  aMessage.Append;
  Randomize;
  tmpID := '';
  CreateGUID(aGUID);
  tmpID := StringReplace(StringReplace(StringReplace(GUIDToString(aGUID),'-','',[rfReplaceAll]),'{','',[rfReplaceAll]),'{','',[rfReplaceAll]);
  aMessage.DataSet.FieldByName('USER').AsString:='*';
  aMessage.DataSet.FieldByName('TREEENTRY').AsInteger:=TREE_ID_SEND_MESSAGES;
  aMessage.DataSet.FieldByName('ID').AsString:=tmpID;
  aMessage.DataSet.FieldByName('SQL_ID').AsVariant:=Data.GetUniID;
  aMessage.DataSet.FieldByName('MSG_ID').AsVariant:=aMessage.DataSet.FieldByName('SQL_ID').AsVariant;
  aMessage.DataSet.FieldByName('TYPE').AsVariant:='EMAIL';
  aMessage.DataSet.FieldByName('READ').AsString:='N';
  aMessage.DataSet.FieldByName('SENDER').AsString:=aFrom;
  aMessage.DataSet.FieldByName('SENDDATE').AsDateTime:=Now();
  aMessage.DataSet.FieldByName('RECEIVERS').AsString:=aTo;
  aMessage.DataSet.FieldByName('SUBJECT').AsVariant:=aSubject;
  aMessage.DataSet.Post;
  aMessage.Content.Select(tmpID);
  aMessage.Content.Append;
  aMessage.Content.FieldByName('ID').AsString:=tmpID;
  aMessage.Content.FieldByName('DATATYP').AsString:='PLAIN';
  aMessage.Content.DataSet.Post;
  aStrm := TStringStream.Create(aText);
  Data.StreamToBlobField(aStrm,aMessage.Content.DataSet,'DATA');
  aStrm.Free;
  aMessage.Free;
  NextSendTime:=Now();
  DoSendMails;
end;

procedure TPMTAServer.DoRun;
var
  y,m,d,h,mm,s,ss: word;
  aServers: TStrings;
  smtp: TSMTPSend;
  DNSServers: TStringList;
  i: Integer;
begin
  debugln('starting...');
  with Self as IBaseDBInterface do
    begin
      DBLogout;
      if not Login then
        begin
          debugln('login failed');
          exit;
        end;
    end;
  debugln('login ok');
  with Self as IBaseApplication do
    begin
      DecodeDate(Now(),y,m,d);
      DecodeTime(Now(),h,mm,s,ss);
      GetLog.Active := False;
      GetLog.FileName := Format('mta_server_log_%.4d-%.2d-%.2d %.2d_%.2d_%.2d_%.4d.log',[y,m,d,h,mm,s,ss]);
      getLog.LogType := ltFile;
      GetLog.Active:=True;
    end;
  DefaultDomain := GetOptionValue('d','domain');
  Subscribers :=  TMessageSubscribings.Create(nil,Data);
  Subscribers.CreateTable;
  Data.SetFilter(Data.Tree,Data.QuoteField('TYPE')+'='+Data.QuoteValue('B'));
  Server.OnLogin :=@ServerLogin;
  Server.OnLog:=@ServerLog;
  Server.OnMailreceived:=@ServerMailreceived;
  Server.OnAcceptMail:=@ServerAcceptMail;
  debugln('server running...');
  NextCollectTime := Now();
  NextSendTime := Now();
  Server.Start;
  i := 0;
  while not Terminated do
    begin
      inc(i);
      Server.CallAction;
      sleep(10);
      if i >100 then
        begin
          DoCollectGroupMessages;
          DoSendMails;
          sleep(100);
          i := 0;
        end;
    end;
  // stop program loop
  Subscribers.Free;
  Terminate;
end;

constructor TPMTAServer.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
  Server := TLSMTPServer.Create(Self);
  if GetOptionValue('i','interface')<>'' then
    begin
      Server.ListenInterface := GetOptionValue('i','interface');
      debugln('using interface:'+GetOptionValue('i','interface'));
    end;
  if GetOptionValue('p','port')<>'' then
    begin
      Server.ListenPort := StrToIntDef(GetOptionValue('p','port'),25);
      debugln('using port:'+GetOptionValue('p','port'));
    end;
end;

destructor TPMTAServer.Destroy;
begin
  Server.Free;
  inherited Destroy;
end;

var
  Application: TPMTAServer;

{$R *.res}

begin
  Application:=TPMTAServer.Create(nil);
  Application.Run;
  Application.Free;
end.
