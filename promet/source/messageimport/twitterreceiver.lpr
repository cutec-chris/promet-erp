 program twitterreceiver;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp,
  Interfaces
  { you can add units after this },db,Utils,
  FileUtil,Forms,uData, uIntfStrConsts, pcmdprometapp,uBaseCustomApplication,
  uBaseApplication,httpsend,jsonparser,jsonscanner, fpjson,uBaseDBInterface,
  ssl_openssl,synautil,uBaseDbClasses,LConvEncoding;

type

  { PrometCmdApp }

  PrometCmdApp = class(TBaseCustomApplication)
  private
    mailaccounts : string;
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure ReceiveMails(aUser : string);
  end;

{ PrometCmdApp }

procedure PrometCmdApp.DoRun;
begin
  with BaseApplication as IBaseApplication do
    begin
      AppVersion:={$I ../base/version.inc};
      AppRevision:={$I ../base/revision.inc};
    end;
  if not Login then Terminate;
  //Your logged in here on promet DB
  with Data.Users.DataSet do
    begin
      First;
      while not EOF do
        begin
          ReceiveMails(FieldByName('NAME').AsString);
          Next;
        end;
    end;
  // stop program loop
  Terminate;
end;

constructor PrometCmdApp.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor PrometCmdApp.Destroy;
begin
  inherited Destroy;
end;

procedure PrometCmdApp.ReceiveMails(aUser: string);
var
  omailaccounts: String;
  http: THTTPSend;
  url: String;
  Parser: TJSONParser;
  jData: TJSONData;
  ReplaceOmailaccounts: Boolean;
  aData: TJSONData;
  i: Integer;
  text: String;
  adate: TDateTime;
  buser: String;
  aHist: TBaseHistory;
  aTime: TDateTime;
  aCat: String;
  aref: String;
  author: TJSONStringType;
  Retry: Integer = 4;
  purl: String;
  tmp: String;
  aId: TJSONStringType;
begin
  omailaccounts := '';
  mailaccounts := '';
  with Self as IBaseDbInterface do
    mailaccounts := DBConfig.ReadString('MAILACCOUNTS','');
  ReplaceOmailaccounts := false;
  aHist := TBaseHistory.Create(nil,Data);
  with aHist.DataSet as IBaseManageDB do
    UpdateStdFields := False;
  while pos('|',mailaccounts) > 0 do
    begin  //Servertype;Server;Username;Password;Active
      if copy(mailaccounts,0,pos(';',mailaccounts)-1) = 'Twitter' then
        begin
          mailaccounts := copy(mailaccounts,pos(';',mailaccounts)+1,length(mailaccounts));
          http := THTTPSend.Create;
          //https://friends.ullihome.de/api/statuses/home_timeline.xml?count=5
          //https://friends.ullihome.de/api/statuses/home_timeline.xml?since_id=123455
          http.UserAgent:='Mozilla/5.0 (Windows NT 5.1; rv:6.0.2)';
          writeln('Importing Twitter Feed '+copy(mailaccounts,0,pos(';',mailaccounts)-1));
          purl := copy(mailaccounts,0,pos(';',mailaccounts)-1);
          url := purl+'statuses/home_timeline.json';
          mailaccounts := copy(mailaccounts,pos(';',mailaccounts)+1,length(mailaccounts));
          http.UserName := copy(mailaccounts,0,pos(';',mailaccounts)-1);
          mailaccounts := copy(mailaccounts,pos(';',mailaccounts)+1,length(mailaccounts));
          http.Password := copy(mailaccounts,0,pos(';',mailaccounts)-1);
          mailaccounts := copy(mailaccounts,pos(';',mailaccounts)+1,length(mailaccounts));
          omailaccounts := omailaccounts+'Twitter;'+purl+';'+http.UserName+';'+http.Password+';'+copy(mailaccounts,0,pos(';',mailaccounts)-1)+';';
          omailaccounts := omailaccounts+'L:;';
          if (copy(mailaccounts,0,pos(';',mailaccounts)-1)= 'YES') then
            begin
              mailaccounts := copy(mailaccounts,pos(';',mailaccounts)+1,length(mailaccounts));
              tmp := copy(mailaccounts,0,pos(';',mailaccounts)-1);
              if copy(tmp,0,2)='L:' then
                tmp := copy(tmp,3,length(tmp));
              //tmp := '';
              if tmp = '' then url := url+'?count=2200'
              else url := url+'?since_id='+tmp;
              http.HTTPMethod('GET',url);
              Parser := TJSONParser.Create(http.Document);
              jData := Parser.Parse;
              Retry := 4;
              if jData.Count>0 then
                aId := TJSONObject(jData.Items[0]).Elements['id'].AsString;
              while Retry>0 do
                begin
                  i := 0;
                  while i < jData.Count do
                    begin
                      aData := jData.Items[i];
                      if Assigned(aData) and Assigned(TJSONObject(aData).Elements['text']) then
                        begin
                          text := ConvertEncoding(TJSONObject(aData).Elements['text'].AsString,GuessEncoding(TJSONObject(aData).Elements['text'].AsString),encodingUTF8);
                          aCat := TJSONObject(aData).Elements['id'].AsString;
                          aref := TJSONObject(aData).Elements['in_reply_to_status_id'].AsString;
                          Data.SetFilter(aHist,Data.QuoteField('REFOBJECT')+'='+Data.QuoteValue(aCat));
                          if aHist.Count=0 then
                            begin
                              if aRef = '0' then aRef := '';
                              aTime := DecodeRfcDateTime(TJSONObject(aData).Elements['created_at'].AsString);
                              author := TJSONObject(TJSONObject(aData).Elements['user']).Elements['name'].AsString;
                              author := ConvertEncoding(author,GuessEncoding(author),encodingUTF8);
                              if aRef='' then
                                begin
                                  inc(Retry,2);
                                  aHist.AddItem(Data.Users.DataSet,text,'',author,nil,ACICON_EXTERNALCHANGED,'',False,False);
                                  aHist.TimeStamp.AsDateTime:=aTime;
                                  aHist.FieldByName('REF_ID').AsVariant:=Data.Users.Id.AsVariant;
                                  aHist.FieldByName('REFOBJECT').AsString:=aCat;
                                  aHist.FieldByName('CHANGEDBY').Clear;
                                  try
                                    aHist.Post;
                                  except
                                    on e : Exception do
                                      begin
                                        //ReplaceOmailaccounts:=False;
                                        aId := TJSONObject(jData.Items[i]).Elements['id'].AsString;
                                        jData.Items[i] := nil;
                                        WriteLn(e.Message);
                                      end;
                                  end;
                                end
                              else
                                begin
                                  Data.SetFilter(aHist,Data.QuoteField('REFOBJECT')+'='+Data.QuoteValue(aRef));
                                  if aHist.Count>0 then
                                    begin
                                      inc(Retry,2);
                                      aHist.AddParentedItem(Data.Users.DataSet,text,aHist.Id.AsVariant,'',author,nil,ACICON_EXTERNALCHANGED,'',False,False);
                                      aHist.TimeStamp.AsDateTime:=aTime;
                                      aHist.FieldByName('REF_ID').AsVariant:=Data.Users.Id.AsVariant;
                                      aHist.FieldByName('REFOBJECT').AsString:=aCat;
                                      aHist.FieldByName('CHANGEDBY').Clear;
                                      try
                                        aHist.Post;
                                      except
                                        on e : Exception do
                                          begin
                                            //ReplaceOmailaccounts:=False;
                                            aId := TJSONObject(jData.Items[i]).Elements['id'].AsString;
                                            jData.Items[i] := nil;
                                            WriteLn(e.Message);
                                          end;
                                      end;
                                    end
                                end;
                              inc(i);
                            end
                          else jData.Items[i] := nil;
                       end
                     else inc(i);
                    end;
                  dec(Retry);
                  for i := 0 to jData.Count-1 do
                    if Assigned(jData.Items[i]) then
                      inc(Retry);
                  writeln(Retry);
                end;
              if aId <> '' then
                begin
                  ReplaceOmailaccounts:=True;
                  omailaccounts := copy(omailaccounts,0,length(omailaccounts)-1)+aId+';';
                end;
              Parser.Free;
              http.Free;
            end;
          omailaccounts := omailaccounts+'|';
        end
      else
        omailaccounts := omailaccounts+copy(mailaccounts,0,pos('|',mailaccounts));
      mailaccounts := copy(mailaccounts,pos('|',mailaccounts)+1,length(mailaccounts));
    end;
  if ReplaceOmailaccounts then
    begin
      with Self as IBaseDbInterface do
        DBConfig.WriteString('MAILACCOUNTS',omailaccounts);
    end;
  aHist.Free;
end;

var
  Application: PrometCmdApp;

begin
  Application:=PrometCmdApp.Create(nil);
  Application.Run;
  Application.Free;
end.

