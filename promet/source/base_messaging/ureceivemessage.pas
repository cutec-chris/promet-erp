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
  Created 25.01.2014
*******************************************************************************}
unit ureceivemessage;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,mimemess,uPerson,synautil,uData,uIntfStrConsts,
  uBaseDBInterface,mailchck,uMessages,uMimeMessages,db,uBaseDbClasses,
  Utils;

var
  Spampoints : real;
  aSender : string;
  aSubject : string;
  aTreeEntry: Integer;
  aSendDate: TDateTime;
  aReceivedDate: TDateTime;

procedure Init;
function CheckHeader(mID : string;msg : TMimeMess;aUser : string) : Boolean;
procedure ArchiveMessage(mID: string; aMsg: TStrings);
procedure ReceiveMessage(aMID : string; aMSG : TStrings;aMessage : TMimeMessage);

implementation

procedure Init;
begin
  Spampoints:=0;
  aSender:='';
  aSubject:='';
  aTreeEntry:=TREE_ID_MESSAGES;
end;

function CheckHeader(mID: string; msg: TMimeMess; aUser: string): Boolean;
var
  atmp: String;
  CustomerCont: TPersonContactData;
  Customers: TPerson;
  b: Integer;
  a: Integer;
  lSP: Integer;
  aDate: TDateTime;
  aTransmitTime: Int64;
  aChk: Integer;
  aUserThere: Boolean;
  i: Integer;
  btmp: String;
begin
  Result := True;
  aSender :=  SysToUni(msg.Header.From);
  aSubject := SysToUni(msg.Header.Subject);
  atmp:=SysToUni(getemailaddr(msg.Header.From));
  CustomerCont := TPersonContactData.Create(nil);
  atmp := StringReplace(atmp,'''','',[rfReplaceAll]);
  if Data.IsSQLDb then
    Data.SetFilter(CustomerCont,Data.ProcessTerm('UPPER("DATA")=UPPER('''+atmp+''')'))
  else
    Data.SetFilter(CustomerCont,Data.ProcessTerm('"DATA"='''+atmp+''''));
  Customers := TPerson.Create(nil);
  Data.SetFilter(Customers,'"ACCOUNTNO"='+Data.QuoteValue(CustomerCont.DataSet.FieldByName('ACCOUNTNO').AsString));
  CustomerCont.Free;
  if Customers.Count > 0 then
    begin
      Customers.History.Open;
      try
      Customers.History.AddItem(Customers.DataSet,Format(strActionMessageReceived,[aSubject]),
                                'MESSAGEIDX@'+mID+'{'+aSubject+'}',
                                '',
                                nil,
                                ACICON_MAILNEW);
      except
      end;
      aTreeEntry := TREE_ID_MESSAGES;
    end
  else
    begin
      aTreeEntry := TREE_ID_UNKNOWN_MESSAGES;
    end;
  Customers.Free;
  SpamPoints := 0;
  if msg.Header.ToList.Count > 0 then
    begin
      if getemailaddr(trim(msg.Header.ToList[0])) = getemailaddr(trim(msg.Header.From)) then
        aTreeEntry := TREE_ID_SPAM_MESSAGES;
      for i := 0 to msg.Header.ToList.Count-1 do
        if getemailaddr(trim(msg.Header.ToList[i])) = getemailaddr(trim(aUser)) then
          aUserThere:=True;
    end;
  if not aUserThere then
    Spampoints:=Spampoints+5;
  if Spampoints>0 then
    aTreeEntry:=TREE_ID_UNKNOWN_MESSAGES;
  if aTreeEntry <> TREE_ID_MESSAGES then
    begin
      aUserThere := True;
      if pos('@',aUser)>0 then
        aUserThere := False;
      aSendDate := Now();
      aReceivedDate := 0;
      if aTreeEntry = TREE_ID_UNKNOWN_MESSAGES then
        begin //Filter Spam
          b := 0;
          for a := 0 to msg.Header.CustomHeaders.Count-1 do
            begin
              lSP := 0;
              atmp := msg.Header.CustomHeaders[a];
              if copy(atmp,0,9) = 'Received:' then
                begin
                  inc(b);
                  lSP := 1;
                  atmp := copy(atmp,16,length(atmp));
                  btmp := copy(atmp,rpos(';',atmp)+1,length(atmp));
                  aDate := DecodeRfcDateTime(trim(btmp));
                  if aDate > aReceivedDate then aReceivedDate:=aDate;
                  if aDate < aSendDate then aSendDate:=aDate;
                  atmp := trim(copy(atmp,0,pos('by',lowercase(atmp))-1));
                  if copy(atmp,0,1) = '[' then
                    lSP := lSP+2;//kein DNS
                  if (pos('with esmtps',lowercase(msg.Header.CustomHeaders[a]))>0)//verschlüsselte Verbindung
                  or (pos('with local',lowercase(msg.Header.CustomHeaders[a]))>0)//lokal
                  then
                    lSP := 0;
                end
              else if copy(atmp,0,17)='List-Unsubscribe:' then
                lSP := lSP+5;//Alle Spammer versuchen sich als Mailingliste auszugeben
                             //und pber den List-Unsubscribe nen Button einzublenden "zum abbestellen"
              SpamPoints+=lSP;
            end;
          aTransmitTime := trunc((aReceivedDate-aSendDate)*MinsPerDay);
          if aTransmitTime < 3 then aTransmitTime:=0;
          SpamPoints+=aTransmitTime;
          a := 0;
          if msg.Header.FindHeader('X-Spam-Flag') = 'YES' then
            SpamPoints := SpamPoints+4;
          if  (msg.Header.FindHeader('X-GMX-Antispam') <> '') then
            begin
              if TryStrToInt(trim(copy(trim(msg.Header.FindHeader('X-GMX-Antispam')),0,pos(' ',trim(msg.Header.FindHeader('X-GMX-Antispam')))-1)),a) then
                if a > 0 then
                  SpamPoints := SpamPoints+4;
            end;
          atmp := trim(msg.Header.From);
          if (pos('>',atmp) > 0) and (pos('<',atmp) > 0) then
            atmp := getemailaddr(atmp)
          else if atmp = '' then
            SpamPoints := SpamPoints+4   //Kein Absender
          else
            SpamPoints := SpamPoints+1; //Kein Realname
          //Mails mit großer Empfängeranzahl
          SpamPoints := SpamPoints+((msg.Header.ToList.Count*0.5)-0.5);
        end;
      atmp := trim(msg.Header.From);
      if (SpamPoints>0) and (Spampoints<=5) then
        begin
          if (pos('>',atmp) > 0) and (pos('<',atmp) > 0) then
            atmp := getemailaddr(atmp);
          {aChk := mailcheck(atmp);
          case aChk of
          1,2,3: aChk := 0;
          end;
          SpamPoints+=aChk;}
        end;
      if SpamPoints > 5 then
        begin
          aTreeEntry := TREE_ID_SPAM_MESSAGES;
        end;
    end;
end;

procedure ArchiveMessage(mID: string; aMsg: TStrings);
var
  ArchiveMsg: TArchivedMessage;
  ss: TStringStream;
begin
  ArchiveMsg := TArchivedMessage.Create(nil);
  ArchiveMsg.CreateTable;
  ArchiveMsg.Insert;
  ArchiveMsg.DataSet.FieldByName('ID').AsString:=mID;
  ss := TStringStream.Create(aMsg.Text);
  Data.StreamToBlobField(ss,ArchiveMsg.DataSet,'DATA');
  ss.Free;
  ArchiveMsg.DataSet.Post;
end;

procedure ReceiveMessage(aMID: string; aMSG: TStrings; aMessage: TMimeMessage);
var
  BMID: LargeInt;
  fullmsg: TMimeMess;
  Customers: TPerson;
  CustomerCont: TPersonContactData;
  atmp: String;
  aMessageL: TMessageList;
  aHist: TBaseHistory;
begin
  aMessage.Select(0);
  aMessage.Open;
  with aMessage.DataSet do
    begin //Messagenot there
      BMID := Data.GetUniID(nil);
      Insert;
      FieldByName('USER').AsString := Data.Users.DataSet.FieldByName('ACCOUNTNO').AsString;
      FieldByName('ID').AsString := aMID;
      FieldByName('MSG_ID').AsInteger:=BMID;
      FieldByName('TYPE').AsString := 'EMAIL';
      FieldByName('READ').AsString := 'N';
      fullmsg := TMimeMess.Create;
      fullmsg.Lines.Text:=aMSG.Text;
      fullmsg.DecodeMessage;
      aMessage.DecodeMessage(fullmsg);
      atmp:=SysToUni(getemailaddr(fullmsg.Header.From));
      try
        atmp := StringReplace(atmp,'''','',[rfReplaceAll]);
        CustomerCont := TPersonContactData.Create(nil);
        if Data.IsSQLDb then
          Data.SetFilter(CustomerCont,Data.ProcessTerm('UPPER("DATA")=UPPER('''+atmp+''')'))
        else
          Data.SetFilter(CustomerCont,Data.ProcessTerm('"DATA"='''+atmp+''''));
      except
      end;
      Customers := TPerson.Create(nil);
      Data.SetFilter(Customers,'"ACCOUNTNO"='+Data.QuoteValue(CustomerCont.DataSet.FieldByName('ACCOUNTNO').AsString));
      CustomerCont.Free;
      fullmsg.Free;
      FieldByName('TREEENTRY').AsInteger := aTreeEntry;
      if (aTreeEntry = TREE_ID_MESSAGES)
      then
        begin
          if FieldByName('PARENT').AsString<>'' then
            begin
              aMessageL := TMessageList.CreateEx(nil,aMessage.DataModule);
              aMessageL.SelectByMsgID(FieldByName('PARENT').AsVariant);
              aMessageL.Open;
              if aMessageL.Count>0 then
                begin
                  aHist := TBaseHistory.CreateEx(nil,aMessageL.DataModule);
                  aHist.Filter(Data.ProcessTerm(Data.QuoteField('LINK')+'='+Data.QuoteValue('MESSAGEIDX@'+aMessageL.FieldByName('ID').AsString+'*')));
                  if aHist.Count>0 then
                    Data.Users.History.AddMessageItem(Customers.DataSet,aMessage.ToString,aSubject,'e-Mail','MESSAGEIDX@'+aMID+'{'+aSubject+'}',aHist.Id.AsVariant)
                  else
                    Data.Users.History.AddMessageItem(Customers.DataSet,aMessage.ToString,aSubject,'e-Mail','MESSAGEIDX@'+aMID+'{'+aSubject+'}');
                end;
            end
          else
            Data.Users.History.AddMessageItem(Customers.DataSet,aMessage.ToString,aSubject,'e-Mail','MESSAGEIDX@'+aMID+'{'+aSubject+'}');
        end;
      if aTreeEntry = TREE_ID_SPAM_MESSAGES then
        FieldByName('READ').AsString := 'Y';
      Post;
      aMessage.History.Open;
      aMessage.History.AddItem(aMessage.DataSet,Format(strActionMessageReceived,[DateTimeToStr(Now())]),
                                '',
                                '',
                                nil,
                                ACICON_MAILNEW);
      if SpamPoints>0 then
        aMessage.History.AddItem(aMessage.DataSet,strMessageSpamPoints+' '+FloatToStr(SpamPoints),
                                  '',
                                  '',
                                  nil,
                                  ACICON_MAILNEW);
      Customers.Free;
    end;
end;

end.

