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
*******************************************************************************}
program feedreceiver;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils, CustApp
  { you can add units after this },db,Utils,
  uData, uBaseCustomApplication,uBaseDbClasses,
  synautil,httpsend, laz_synapse,uMessages,uDocuments,uBaseDbInterface,
  dom,xmlread,md5, uIntfStrConsts, pcmdprometapp,Variants,
  uBaseApplication,uminiconvencoding,ssl_openssl;

type

  { TRSSReceiver }

  TRSSReceiver = class(TBaseCustomApplication)
    procedure httpHeadersChange(Sender: TObject);
  private
    mailaccounts : string;
    aLastMessage: TDateTime;
    http: THTTPSend;
  protected
    MessageIndex : TMessageList;
    Message : TMessage;
    aConnection: TComponent;
    procedure DoRun; override;
    function CommandReceived(Sender: TObject; aCommand: string): Boolean;
    procedure ReceiveMails(aUser : string);
    function GetSingleInstance : Boolean; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

procedure TRSSReceiver.httpHeadersChange(Sender: TObject);
var
  i: Integer;
  s: String;
  aDate: TDateTime;
begin
  for i := 0 to TStringList(Sender).Count-1 do
    begin
      s := TStringList(Sender)[i];
      Debug(s);
      if copy(LowerCase(s),0,5)='date:' then
        begin
          aDate := DecodeRfcDateTime(copy(s,6,length(s)));
          if aLastMessage>0 then
            begin
              Debug('Last Message:'+DateTimeToStr(aLastMessage));
              Debug('Last changed:'+DateTimeToStr(aDate));
            end
          else
            begin
              Debug('No last message, full Download');
            end;
          if aDate<aLastMessage then
            begin
              http.Abort;
              Info('Abort Download, no change');
            end;
        end;
    end;
end;

procedure TRSSReceiver.DoRun;
var
  i: Integer;
  StartTime: TDateTime;
  aTime: Extended;
begin
  with BaseApplication as IBaseApplication do
    begin
      AppVersion:={$I ../base/version.inc};
      AppRevision:={$I ../base/revision.inc};
    end;
  Info('feedreceiver started...');
  if not Login then Terminate;
  Info('login ok');
  RegisterMessageHandler;
  MessageHandler.RegisterCommandHandler(@Commandreceived);
  StartTime := Now();
  with Data.Users.DataSet do
    begin
      First;
      while not EOF do
        begin
          try
            ReceiveMails(FieldByName('NAME').AsString);
          except
          end;
          Next;
        end;
    end;
  Terminate;
end;

function TRSSReceiver.CommandReceived(Sender: TObject; aCommand: string
  ): Boolean;
var
  aUser: String;
begin
  Result := False;
  if copy(aCommand,0,8) = 'Receive(' then
    begin
      aUser := copy(aCommand,9,length(aCommand));
      aUser := copy(aUser,0,length(aUser)-1);
      ReceiveMails(aUser);
      Result := True;
    end;
end;
procedure TRSSReceiver.ReceiveMails(aUser: string);
var
  aNode : TDOMNode;
  Doc : TXMLDocument;
  tmp: String;
  MID: String;
  BMID : Int64;
  aSendDate: Double;
  aFeedName: String;
  aTreeEntry: Integer;
  ss: TStringStream;
  MyFormatSettings : TFormatSettings;
  OldformatSettings: TFormatSettings;
  aDateNode: TDOMNode;
  aIDNode: TDOMNode;
  aTitleNode: TDOMNode;
  aMessageNode: TDOMNode;
  FeedType: String;
  aLinkValue: String;
  aLoc: String;
  aDir: String;
  aSource: String;
  function DoDecode(aIn : string) : string;
  begin
    Result := uminiconvencoding.ConvertEncoding(aIn,GuessEncoding(aIn),EncodingUTF8);
  end;

begin
  MessageIndex := TMessageList.CreateEx(Self,Data,aConnection);
  Message := TMessage.CreateEx(Self,Data,aConnection);
  Data.DeletedItems.DataSet.Open;
  Info('Importing for User '+Data.Users.DataSet.FieldByName('NAME').AsString);
  with Self as IBaseDbInterface do
    mailaccounts := DBConfig.ReadString('MAILACCOUNTS','');
  http := THTTPSend.Create;
  while pos('|',mailaccounts) > 0 do
    begin  //Servertype;RSS Feed
      if copy(mailaccounts,0,pos(';',mailaccounts)-1) = 'FEED' then
        begin
          http.Clear;
          mailaccounts := copy(mailaccounts,pos(';',mailaccounts)+1,length(mailaccounts));
          http.Headers.OnChange:=@httpHeadersChange;
          http.UserAgent:='Mozilla/5.0 (Windows NT 5.1; rv:6.0.2)';
          Info('Importing Feed '+copy(mailaccounts,0,pos(';',mailaccounts)-1));
          aSource := copy(mailaccounts,0,pos(';',mailaccounts)-1);
          Data.SetFilter(MessageIndex,Data.QuoteField('CC')+'='+Data.QuoteValue(aSource),1,'SENDDATE','DESC');
          aLastMessage := MessageIndex.FieldByName('SENDDATE').AsDateTime;
          http.Headers.Clear;
          if aLastMessage>0 then
            http.Headers.Add('If-Modified-Since: '+synautil.Rfc822DateTime(aLastMessage));
          http.HTTPMethod('GET',aSource);
          if HasOption('debug') then
            http.Document.SaveToFile('/tmp/rss.xml');
          if (http.ResultCode = 302)
          or (http.ResultCode = 301)
          then
            begin
              aLoc := Copy(http.Headers.Text, Pos('location:', LowerCase(http.Headers.Text)) + 9, MaxInt);
              aLoc := trim(LowerCase(Copy(aLoc, 1, Pos(#10, aLoc) - 1)));
              Info('>>redirecting Feed '+aLoc);
              http.Clear;
              if aLastMessage>0 then
                http.Headers.Add('If-Modified-Since: '+synautil.Rfc822DateTime(aLastMessage));
              http.HTTPMethod('GET',aLoc);
              if HasOption('debug') then
                http.Document.SaveToFile('/tmp/rss.xml');
            end;
          mailaccounts := copy(mailaccounts,pos(';',mailaccounts)+1,length(mailaccounts));
          mailaccounts := copy(mailaccounts,pos(';',mailaccounts)+1,length(mailaccounts));
          mailaccounts := copy(mailaccounts,pos(';',mailaccounts)+1,length(mailaccounts));
          mailaccounts := copy(mailaccounts,pos(';',mailaccounts)+1,length(mailaccounts));
          if http.ResultCode = 200 then
            begin
              try
                ReadXMLFile(Doc,http.Document);
                aNode := Doc.DocumentElement;
                if  Assigned(aNode.Attributes.GetNamedItem('xmlns'))
                and (pos('/ATOM',UpperCase(aNode.Attributes.GetNamedItem('xmlns').NodeValue)) > 0) then
                  FeedType := 'ATOM'
                else
                  FeedType := 'RSS';
                if FeedType = 'ATOM' then
                  aFeedName := copy(DoDecode(aNode.FindNode('title').FirstChild.NodeValue),0,99);
                if Assigned(aNode) then
                  aNode := aNode.FirstChild;
                while Assigned(aNode) do
                  begin
                    if aNode.NodeName = 'channel' then
                      begin
                        aFeedName := DoDecode(aNode.FindNode('title').FirstChild.NodeValue);
                        if aNode.NextSibling = nil then
                          aNode := aNode.FirstChild;
                      end;
                    if (aNode.NodeName = 'item')
                    or (aNode.NodeName = 'entry') then
                      begin
                        aIDNode := aNode.FindNode('id');
                        if not Assigned(aIDNode) then
                          aIDNode := aNode.FindNode('guid');
                        if FeedType = 'RSS' then
                          aLinkValue := aNode.FindNode('link').FirstChild.NodeValue
                        else
                          aLinkValue := aNode.FindNode('link').Attributes.GetNamedItem('href').NodeValue;
                        aTitleNode := aNode.FindNode('title');
                        if not Assigned(aTitleNode) then
                          aTitleNode := aNode.FindNode('summary');
                        if Assigned(aIDNode) then
                          tmp := StringReplace(trim(aIDNode.FirstChild.NodeValue),lineending,'',[rfReplaceAll])
                        else if Assigned(aTitleNode) then
                          tmp := StringReplace(trim(aTitleNode.FirstChild.NodeValue),lineending,'',[rfReplaceAll]);
                        MID := MD5Print(MD5String(tmp));
                        Data.SetFilter(MessageIndex,Data.QuoteField('ID')+'='+Data.QuoteValue(MID));
                        Data.SetFilter(Data.DeletedItems,Data.Processterm(Data.QuoteField('LINK')+'='+Data.QuoteValue('MESSAGEIDX@'+MID+'*')));
                        aMessageNode := aNode.FindNode('description');
                        if not Assigned(aMessageNode) then
                          aMessageNode := aNode.FindNode('content');
                        if not Assigned(aMessageNode) then
                          aMessageNode := aNode.FindNode('content:encoded');

                        //Date
                        MyFormatSettings.DateSeparator := '-';
                        MyFormatSettings.TimeSeparator := ':';
                        MyFormatSettings.ShortDateFormat := 'yyyy-mm-dd';
                        MyFormatSettings.LongDateFormat := 'yyyy-mm-dd';
                        MyFormatSettings.ShortTimeFormat := 'hh:nn:ss';
                        MyFormatSettings.LongTimeFormat := 'hh:nn:ss';
                        OldformatSettings := DefaultFormatSettings;
                        DefaultFormatSettings := MyformatSettings;
                        aSendDate := 0;
                        if FeedType = 'RSS' then
                          begin
                            aDateNode := aNode.FindNode('dc:date');
                            if not Assigned(aDateNode) then
                              aDateNode := aNode.FindNode('PubDate');
                            if not Assigned(aDateNode) then
                              aDateNode := aNode.FindNode('pubDate');
                            if not Assigned(aDateNode) then
                              aDateNode := aNode.FindNode('pubdate');
                          end
                        else
                          aDateNode := aNode.FindNode('updated');
                        if Assigned(aDateNode) then
                          begin
                            tmp := aDateNode.FirstChild.NodeValue;
                            tmp := StringReplace(tmp,'T',' ',[]);
                            tmp := StringReplace(tmp,'Z','',[]);
                            tmp := copy(tmp,0,19);
                            if (not TryStrToDateTime(tmp,aSendDate)) then
                              aSendDate := 0;
                          end;
                        DefaultFormatSettings := OldformatSettings;
                        if (aSendDate = 0) and Assigned(aDateNode) then
                          aSendDate := DecodeRfcDateTime(aDateNode.FirstChild.NodeValue);
                        if (not MessageIndex.DataSet.Locate('ID',MID,[]))
                        and (not Data.DeletedItems.DataSet.Locate('LINK','MESSAGEIDX@'+MID,[loPartialKey]))
                        then
                          begin
                            Info('New Entry '+MID);
                            aTreeEntry := TREE_ID_UNKNOWN_MESSAGES;
                            Data.SetFilter(Data.Tree,Data.QuoteField('TYPE')+'='+Data.QuoteValue('N'));
                            aDir := trim(copy(mailaccounts,0,pos(';',mailaccounts)-1));
                            if Data.Tree.DataSet.Locate('NAME',aDir,[loCaseInsensitive]) then
                              aTreeEntry := Data.Tree.FieldByName('SQL_ID').AsVariant
                            else
                              begin
                                Data.SetFilter(Data.Tree,Data.QuoteField('TYPE')+'='+Data.QuoteValue('B'));
                                if Data.Tree.DataSet.Locate('NAME',aDir,[loCaseInsensitive]) then
                                  aTreeEntry := Data.Tree.FieldByName('SQL_ID').AsVariant
                              end;
                            with MessageIndex.DataSet do
                              begin //Message not there
                                Insert;
                                BMID := Data.GetUniID;
                                FieldByName('USER').AsString := Data.Users.DataSet.FieldByName('ACCOUNTNO').AsString;
                                FieldByName('ID').AsString := MID;
                                FieldByName('MSG_ID').AsInteger := BMID;
                                FieldByName('TYPE').AsString := 'FEED';
                                FieldByName('READ').AsString := 'N';
                                FieldByName('SENDER').AsString := aFeedName;
                                FieldByName('CC').AsString := aSource;
                                if aSendDate <> 0 then
                                  begin
                                    FieldByName('SENDDATE').AsDateTime := aSendDate;
                                    if FieldDefs.IndexOf('SENDTIME') <> -1 then
                                      FieldByName('SENDTIME').AsFloat := Frac(aSendDate);
                                  end
                                else
                                  begin
                                    FieldByName('SENDDATE').AsDateTime := Now();
                                    if FieldDefs.IndexOf('SENDTIME') <> -1 then
                                      FieldByName('SENDTIME').AsFloat := Frac(Now());
                                  end;
                                FieldbyName('SUBJECT').AsString := DoDecode(aTitleNode.FirstChild.NodeValue);
                                FieldByName('TREEENTRY').AsInteger := aTreeEntry;
                                FieldByName('TIMESTAMPD').AsDateTime := Trunc(Now());
                                if FieldDefs.IndexOf('TIMESTAMPT') <> -1 then
                                  FieldByName('TIMESTAMPT').AsFloat := Frac(Now());
                                Post;
                              end;
                            Data.SetFilter(Message,Data.QuoteField('ID')+'='+Data.QuoteValue(MID));
                            with Message.DataSet do
                              begin
                                Edit;
                                FieldByName('ID').AsString := MID;
                                FieldByName('SQL_ID').AsInteger := BMID;
                                FieldByName('TIMESTAMPD').AsDateTime := Now();
                                FieldByName('DATATYP').AsString := 'HTML';
                                Post;
                                tmp := '';
                                if Assigned(aMessageNode) and Assigned(aMessageNode.FirstChild) then
                                  begin
                                    tmp := aMessageNode.FirstChild.NodeValue;
                                    if tmp = '' then
                                      tmp := DoDecode(aMessageNode.TextContent);
                                  end;
                                if pos(DoDecode(aTitleNode.FirstChild.NodeValue),tmp)=0 then
                                  tmp := '<b>'+DoDecode(aTitleNode.FirstChild.NodeValue)+'</b><br>'+tmp;
                                tmp := tmp+'<br><a href='''+aLinkValue+'''>'+strGotoFeed+'</a>';
                                tmp := '<html><body>'+tmp+'</body></html>';
                                ss := TStringStream.Create(tmp);
                                Data.StreamToBlobField(ss,Message.DataSet,'DATA');
                                ss.Free;
                                MessageHandler.SendCommand('prometerp','Message.refresh');
                                Data.Users.History.AddMessageItem(Message.DataSet,Message.ToString,MessageIndex.FieldbyName('SUBJECT').AsString,'Feed','MESSAGEIDX@'+MID+'{'+MessageIndex.FieldbyName('SUBJECT').AsString+'}');
                              end;
                          end
                        else if (aSendDate-0.001 > MessageIndex.DataSet.FieldByName('SENDDATE').AsDateTime)
                             and (not Data.DeletedItems.DataSet.Locate('LINK','MESSAGEIDX@'+MID,[loPartialKey])) then
                          begin
                            Info(DateTimeToStr(aSendDate)+' > '+DateTimeToStr(MessageIndex.DataSet.FieldByName('SENDDATE').AsDateTime)) ;
                            aTreeEntry := TREE_ID_MESSAGES;
                            with MessageIndex.DataSet do
                              begin //Message not there
                                Edit;
                                FieldByName('READ').AsString := 'N';
                                FieldByName('SENDDATE').AsDateTime := aSendDate;
                                if FieldByName('TREEENTRY').AsInteger <> TREE_ID_DELETED_MESSAGES then
                                  if FieldByName('TREEENTRY').AsInteger <> TREE_ID_UNKNOWN_MESSAGES then
                                    FieldByName('TREEENTRY').AsInteger := aTreeEntry;
                                Post;
                              end;
                          end;
                      end;
                    aNode := aNode.NextSibling;
                  end;
              except
                on e : Exception do
                  begin
                    Warning(e.Message);
                    aNode := aNode.NextSibling;
                  end;
              end;
              Doc.Free;
            end
          else
            Warning('failed to import Feed:'+http.ResultString);
        end;
      mailaccounts := copy(mailaccounts,pos('|',mailaccounts)+1,length(mailaccounts));
    end;
  http.Free;
  Message.Destroy;
  MessageIndex.Destroy;
end;

function TRSSReceiver.GetSingleInstance: Boolean;
begin
  Result := False;
end;

constructor TRSSReceiver.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
end;

destructor TRSSReceiver.Destroy;
begin
  inherited Destroy;
end;

var
  Application: TRSSReceiver;

begin
  Application:=TRSSReceiver.Create(nil);
  Application.Run;
  Application.Free;
end.

