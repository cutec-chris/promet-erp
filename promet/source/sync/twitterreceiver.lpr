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
Created 13.06.2014
*******************************************************************************}
 program twitterreceiver;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp
  { you can add units after this },db, Utils, FileUtil, uData,
  uIntfStrConsts, pcmdprometapp, uBaseCustomApplication, wiki2html_pkg,
  uBaseApplication, httpsend, jsonparser, jsonscanner, fpjson, uBaseDBInterface,
  ssl_openssl, synautil, uBaseDbClasses, LConvEncoding,htmltowiki,uDocuments,
  uPerson;

type

  { PrometCmdApp }

  PrometCmdApp = class(TBaseCustomApplication)
    procedure PrometCmdAppConvertImage(Image: string; var OutFile: string);
  private
    mailaccounts : string;
    aHist: TBaseHistory;
    Customers: TPerson;
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
  Info('twitterreceiver started...');
  if not Login then Terminate;
  Info('login ok...');
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
  Info('getting '+OutFile+' ...');
  http.Timeout:=1000;
  http.HTTPMethod('GET',OutFile);
  if http.ResultCode=200 then
    begin
      Document := TDocument.Create(Self);
      Document.Select(0);
      Document.Open;
      Document.Ref_ID := Customers.History.Id.AsVariant;
      Document.BaseTyp := 'H';
      Document.BaseID := Customers.History.Id.AsString;
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
  CustomerCont: TPersonContactData;
  uid: string;
  IsSubItem: Boolean;
begin
  omailaccounts := '';
  mailaccounts := '';
  with Self as IBaseDbInterface do
    mailaccounts := DBConfig.ReadString('MAILACCOUNTS','');
  ReplaceOmailaccounts := false;
  aHist := TBaseHistory.Create(nil);
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
          Info('Importing Twitter Feed '+copy(mailaccounts,0,pos(';',mailaccounts)-1));
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
              url := purl+'statuses/home_timeline.json?count=500';
              if tmp <> '' then url := purl+'statuses/home_timeline.json?since_id='+tmp;
              //url := purl+'statuses/home_timeline.json';
              http.HTTPMethod('GET',url);
              if http.ResultCode=200 then
                begin
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
                          if Assigned(aData) then
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
                              if Assigned(TJSONObject(TJSONObject(aData).Elements['user'])) and Assigned(TJSONObject(TJSONObject(aData).Elements['user']).Elements['name']) then
                                author := TJSONObject(TJSONObject(aData).Elements['user']).Elements['name'].AsString
                              else author := '';
                              author := ConvertEncoding(author,GuessEncoding(author),encodingUTF8);
                              if trim(text) <> '' then
                                begin
                                  if Assigned(TJSONObject(aData).Elements['id']) then
                                    aCat := TJSONObject(aData).Elements['id'].AsString
                                  else aCat := '';
                                  if Assigned(TJSONObject(aData).Elements['in_reply_to_status_id']) and (not TJSONObject(aData).Elements['in_reply_to_status_id'].IsNull) then
                                    aref := TJSONObject(aData).Elements['in_reply_to_status_id'].AsString
                                  else aref := '';
                                  if aRef = '0' then aRef := '';
                                  if Assigned(TJSONObject(aData).Elements['created_at']) then
                                    aTime := DecodeRfcDateTime(TJSONObject(aData).Elements['created_at'].AsString)
                                  else aTime := now();
                                  if Assigned(TJSONObject(TJSONObject(aData).Elements['user'])) and Assigned(TJSONObject(TJSONObject(aData).Elements['user']).Elements['screen_name']) then
                                    uid := TJSONObject(TJSONObject(aData).Elements['user']).Elements['screen_name'].AsString
                                  else uid := '';
                                  uid := StringReplace(uid,'''','',[rfReplaceAll]);

                                  Data.SetFilter(aHist,Data.QuoteField('REFOBJECT')+'='+Data.QuoteValue(aCat));
                                  if aHist.Count=0 then
                                    begin
                                      try
                                        CustomerCont := TPersonContactData.Create(Self);
                                        if Data.IsSQLDb then
                                          Data.SetFilter(CustomerCont,'"TYPE"=''SON'' AND UPPER("DATA")=UPPER('''+uid+''')')
                                        else
                                          Data.SetFilter(CustomerCont,'"TYPE"=''SON'' AND "DATA"='''+uid+'''');
                                      except
                                      end;
                                      Customers := TPerson.Create(Self);
                                      Data.SetFilter(Customers,'"ACCOUNTNO"='+Data.QuoteValue(CustomerCont.DataSet.FieldByName('ACCOUNTNO').AsString));
                                      CustomerCont.Free;
                                      if aRef<>'' then
                                        Data.SetFilter(aHist,Data.QuoteField('REFOBJECT')+'='+Data.QuoteValue(aRef));
                                      IsSubItem := (aRef='') or ((aHist.Count=0) and (nothingimported>5));
                                      if (Customers.Count = 0) and (not IsSubItem) and (trim(author)<>'') then
                                        begin
                                          //Add Customer
                                          Customers.Insert;
                                          Customers.Text.AsString:=author;
                                          Customers.DataSet.Post;
                                          Customers.ContactData.Insert;
                                          Customers.ContactData.Typ.AsString:='SON';
                                          Customers.ContactData.Data.AsString:=uid;
                                          Customers.ContactData.Description.AsString:=asource;
                                          Customers.ContactData.Post;
                                          //Follow Customer
                                          if aRef = '' then
                                            begin
                                              Data.Users.Follows.Insert;
                                              Data.Users.Follows.Link.AsString:=Data.BuildLink(Customers.DataSet);
                                              Data.Users.Follows.Post;
                                            end;
                                        end;
                                      if Customers.Count>0 then
                                        begin
                                          Customers.History.Open;
                                          with Customers.History.DataSet as IBaseManageDB do
                                            UpdateStdFields := False;
                                        end;
                                      if IsSubItem then
                                        begin
                                          inc(Retry,6);
                                          Somethingimported:=True;
                                          Info('new Entry from '+author);
                                          Customers.History.AddItem(Customers.DataSet,text,'',author,nil,ACICON_EXTERNALCHANGED,'',False,False);
                                          Customers.History.TimeStamp.AsDateTime:=aTime;
                                          Customers.History.FieldByName('REFOBJECT').AsString:=aCat;
                                          Customers.History.FieldByName('SOURCE').AsString:=asource;
                                          Customers.History.FieldByName('CHANGEDBY').Clear;
                                          if Assigned(html) and (not html.IsNull) then
                                            begin
                                              Customers.History.Post;
                                              Customers.History.DataSet.Edit;
                                              text := TJSONObject(aData).Elements['statusnet_html'].AsString;
                                              text := HTML2WikiText(text);
                                            end;
                                          Customers.History.FieldByName('ACTION').AsString:=text;
                                          try
                                            Customers.History.Post;
                                          except
                                            on e : Exception do
                                              begin
                                                //ReplaceOmailaccounts:=False;
                                                aId := TJSONObject(jData.Items[i]).Elements['id'].AsString;
                                                jData.Items[i] := nil;
                                                Error(e.Message);
                                              end;
                                          end;
                                        end
                                      else
                                        begin
                                          if aHist.Count>0 then
                                            begin
                                              inc(Retry,6);
                                              Somethingimported:=True;
                                              with aHist.DataSet as IBaseManageDB do
                                                UpdateStdFields := False;
                                              aHist.Edit;
                                              aHist.FieldByName('READ').AsString:='N';
                                              aHist.Post;
                                              Info('new Subentry from '+author);
                                              Customers.History.AddParentedItem(Customers.DataSet,text,aHist.Id.AsVariant,'',author,nil,ACICON_EXTERNALCHANGED,'',False,False);
                                              Customers.History.TimeStamp.AsDateTime:=aTime;
                                              Customers.History.FieldByName('REFOBJECT').AsString:=aCat;
                                              Customers.History.FieldByName('SOURCE').AsString:=asource;
                                              Customers.History.FieldByName('CHANGEDBY').Clear;
                                              if Assigned(html) and (not html.IsNull) then
                                                begin
                                                  Customers.History.Post;
                                                  Customers.History.DataSet.Edit;
                                                  text := TJSONObject(aData).Elements['statusnet_html'].AsString;
                                                  text := HTML2WikiText(text);
                                                end;
                                              Customers.History.FieldByName('ACTION').AsString:=text;
                                              try
                                                Customers.History.Post;
                                              except
                                                on e : Exception do
                                                  begin
                                                    //ReplaceOmailaccounts:=False;
                                                    aId := TJSONObject(jData.Items[i]).Elements['id'].AsString;
                                                    jData.Items[i] := nil;
                                                    Error(e.Message);
                                                  end;
                                              end;
                                            end
                                        end;
                                      Customers.Free;
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
                    end;
                  if aId <> '' then
                    begin
                      ReplaceOmailaccounts:=True;
                      omailaccounts := copy(omailaccounts,0,length(omailaccounts)-1)+aId+';';
                    end;
                  Parser.Free;
                end
              else
                Error('failed to get Data '+http.ResultString+' Code:'+IntToStr(http.ResultCode));
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

