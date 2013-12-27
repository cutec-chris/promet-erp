 program twitterreceiver;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp,
  Interfaces
  { you can add units after this },db, Utils, FileUtil, Forms, uData,
  uIntfStrConsts, pcmdprometapp, uBaseCustomApplication, wiki2html_pkg,
  uBaseApplication, httpsend, jsonparser, jsonscanner, fpjson, uBaseDBInterface,
  ssl_openssl, synautil, uBaseDbClasses, LConvEncoding,htmltowiki,uDocuments;

type

  { PrometCmdApp }

  PrometCmdApp = class(TBaseCustomApplication)
    procedure PrometCmdAppConvertImage(Image: string; var OutFile: string);
  private
    mailaccounts : string;
    aHist: TBaseHistory;
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
procedure PrometCmdApp.PrometCmdAppConvertImage(Image: string;
  var OutFile: string);
var
  http: THTTPSend;
  Document: TDocument;
  aFilename: String;
begin
  http := THTTPSend.Create;
  try
  http.UserAgent:='Mozilla/5.0 (Windows NT 5.1; rv:6.0.2)';
  Write('getting '+OutFile+' ...');
  http.Timeout:=1000;
  http.HTTPMethod('GET',OutFile);
  WriteLn('->OK');
  if http.ResultCode=200 then
    begin
      Document := TDocument.Create(Self,Data);
      Document.Select(0);
      Document.Open;
      Document.Ref_ID := aHist.Id.AsVariant;
      Document.BaseTyp := 'H';
      Document.BaseID := aHist.Id.AsString;
      Document.BaseVersion := Null;
      Document.BaseLanguage := Null;
      aFilename := ExtractFileName(Image);
      Document.AddFromStream(copy(ExtractFileName(aFilename),0,rpos('.',ExtractFileName(aFileName))-1),
                                                 copy(ExtractFileExt(aFileName),2,length(ExtractFileExt(aFileName))),
                                                 http.Document,
                                                 '',
                                                 Now());
      OutFile := aFilename;
      Document.Free;
    end;
  finally
    http.Free;
  end;
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
  aTime: TDateTime;
  aCat: String;
  aref: String;
  author: TJSONStringType;
  Retry: Integer = 4;
  nothingimported : Integer = 0;
  purl: String;
  tmp: String;
  aId: TJSONStringType;
  Somethingimported: Boolean;
  html: TJSONData;
  atext: TJSONData;
  asource: string;
begin
  omailaccounts := '';
  mailaccounts := '';
  with Self as IBaseDbInterface do
    mailaccounts := DBConfig.ReadString('MAILACCOUNTS','');
  ReplaceOmailaccounts := false;
  aHist := TBaseHistory.Create(nil,Data);
  htmltowiki.OnConvertImage:=@PrometCmdAppConvertImage;
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
              url := purl+'statuses/home_timeline.json?count=2500';
              if tmp <> '' then url := url+'&since_id='+tmp;
              //url := purl+'statuses/home_timeline.json';
              http.HTTPMethod('GET',url);
              Parser := TJSONParser.Create(http.Document);
              jData := Parser.Parse;
              Retry := 4;
              if jData.Count>0 then
                aId := TJSONObject(jData.Items[0]).Elements['id'].AsString;
              while Retry>0 do
                begin
                  i := 0;
                  Somethingimported := false;
                  while i < jData.Count do
                    begin
                      aData := jData.Items[i];
                      if Assigned(aData) and Assigned(TJSONObject(aData).Elements['text']) then
                        begin
                          atext := TJSONObject(aData).Elements['text'];
                          if Assigned(TJSONObject(aData).Elements['source']) then
                            asource := TJSONObject(aData).Elements['source'].AsString
                          else asource := '';
                          if Assigned(aText) and (not aText.IsNull) then
                            text := atext.AsString
                          else text := '';
                          text := ConvertEncoding(text,GuessEncoding(text),encodingUTF8);
                          html := TJSONObject(aData).Find('statusnet_html');
                          if trim(text) <> '' then
                            begin
                              if Assigned(TJSONObject(aData).Elements['id']) then
                                aCat := TJSONObject(aData).Elements['id'].AsString
                              else aCat := '';
                              if Assigned(TJSONObject(aData).Elements['in_reply_to_status_id']) and (not TJSONObject(aData).Elements['in_reply_to_status_id'].IsNull) then
                                aref := TJSONObject(aData).Elements['in_reply_to_status_id'].AsString
                              else aref := '';
                              Data.SetFilter(aHist,Data.QuoteField('REFOBJECT')+'='+Data.QuoteValue(aCat));
                              if aHist.Count=0 then
                                begin
                                  if aRef = '0' then aRef := '';
                                  if Assigned(TJSONObject(aData).Elements['created_at']) then
                                    aTime := DecodeRfcDateTime(TJSONObject(aData).Elements['created_at'].AsString)
                                  else aTime := now();
                                  if Assigned(TJSONObject(TJSONObject(aData).Elements['user'])) and Assigned(TJSONObject(TJSONObject(aData).Elements['user']).Elements['name']) then
                                    author := TJSONObject(TJSONObject(aData).Elements['user']).Elements['name'].AsString
                                  else author := '';
                                  author := ConvertEncoding(author,GuessEncoding(author),encodingUTF8);
                                  if aRef='' then
                                    begin
                                      inc(Retry,2);
                                      Somethingimported:=True;
                                      Write('new Entry from '+author);
                                      aHist.AddItem(Data.Users.DataSet,text,'',author,nil,ACICON_EXTERNALCHANGED,'',False,False);
                                      aHist.TimeStamp.AsDateTime:=aTime;
                                      aHist.FieldByName('REF_ID').AsVariant:=Data.Users.Id.AsVariant;
                                      aHist.FieldByName('REFOBJECT').AsString:=aCat;
                                      aHist.FieldByName('SOURCE').AsString:=asource;
                                      aHist.FieldByName('CHANGEDBY').Clear;
                                      if Assigned(html) and (not html.IsNull) then
                                        begin
                                          aHist.Post;
                                          aHist.DataSet.Edit;
                                          text := TJSONObject(aData).Elements['statusnet_html'].AsString;
                                          text := HTML2WikiText(text);
                                        end;
                                      aHist.FieldByName('ACTION').AsString:=text;
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
                                          Somethingimported:=True;
                                          WriteLn('new Subentry from '+author);
                                          aHist.AddParentedItem(Data.Users.DataSet,text,aHist.Id.AsVariant,'',author,nil,ACICON_EXTERNALCHANGED,'',False,False);
                                          aHist.TimeStamp.AsDateTime:=aTime;
                                          aHist.FieldByName('REF_ID').AsVariant:=Data.Users.Id.AsVariant;
                                          aHist.FieldByName('REFOBJECT').AsString:=aCat;
                                          aHist.FieldByName('SOURCE').AsString:=asource;
                                          aHist.FieldByName('CHANGEDBY').Clear;
                                          if Assigned(html) and (not html.IsNull) then
                                            begin
                                              aHist.Post;
                                              aHist.DataSet.Edit;
                                              text := TJSONObject(aData).Elements['statusnet_html'].AsString;
                                              text := HTML2WikiText(text);
                                            end;
                                          aHist.FieldByName('ACTION').AsString:=text;
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
                          else jData.Items[i] := nil;
                       end
                     else inc(i);
                    end;
                  dec(Retry);
                  if not Somethingimported then
                    inc(nothingimported);
                  if nothingimported>10 then break;
                  for i := 0 to jData.Count-1 do
                    if Assigned(jData.Items[i]) then
                      inc(Retry);
                  writeln('retrying '+IntToStr(Retry));
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

