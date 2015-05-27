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
unit uMimeMessages;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, MimeMess, mimepart, uMessages,
  uDocuments, uBaseDbClasses, Variants, db, synacode, synachar,
  zipper,uminiconvencoding;
type
  TMimeMessage = class(TMessage)
    procedure DataSetContentDataSetDataSetmsgMessagePartWalkPart(
      const Sender: TMimePart);
  private
    TextThere : Boolean;
    HtmlThere : Boolean;
    procedure MailAddressesFromString(aSource: string; aTarget: TStrings);
  public
    function EncodeMessage : TMimeMess;
    procedure DecodeMessage(msg : TMimeMess);
  end;
  function GetmailAddr(aIn : string) : string;
implementation
uses uData, Utils, synautil,uBaseDbInterface,uBaseApplication;
function GetmailAddr(aIn : string) : string;
begin
  Result := GetEmailAddr(aIn);
  if copy(result,0,1)='<' then
    result := copy(Result,2,length(Result)-2);
end;
procedure TMimeMessage.DataSetContentDataSetDataSetmsgMessagePartWalkPart(
  const Sender: TMimePart);
var
  s: String;
  ss : TStringStream;
  Document: TDocument;
  sl: TStringList;
  aFilename : string;
  i: Integer;
  atmp: String;
begin
  Sender.TargetCharset := UTF_8;
  Sender.ConvertCharset := True;
  Sender.DecodePart;
  if (Sender.PrimaryCode = MP_TEXT) and ((not TextThere) or (not HtmlThere)) then
    begin
      if ((UpperCase(Sender.Secondary) = 'PLAIN') or (UpperCase(Sender.Secondary) = 'HTML'))  then
        begin
          if Sender.AttachInside then
            begin
              sl := TStringList.Create;
              sl.LoadFromStream(Sender.DecodedLines);
              i := 0;
              Document := nil;
              while i < sl.Count do
                begin
                  if pos('BEGIN',Uppercase(sl[i])) = 1 then
                    begin
                      if Content.State = dsInsert then
                        begin
                          Content.DataSet.Post;
                          Content.DataSet.Edit;
                        end;
                      Document := TDocument.CreateEx(Self,Data);
                      Document.Select(0);
                      Document.Open;
                      Document.Ref_ID := Content.Id.AsVariant;
                      Document.BaseTyp := 'N';
                      Document.BaseID := DataSet.FieldByName('ID').AsString;
                      Document.BaseVersion := Null;
                      Document.BaseLanguage := Null;
                      aFilename := copy(sl[i],pos(' ',sl[i])+1,length(sl[i]));
                      aFilename := copy(aFileName,pos(' ',aFileName)+1,length(aFileName));
                      sl.Delete(i);
                      s := '';
                    end
                  else if (pos('END',Uppercase(sl[i])) = 1) and Assigned(Document) then
                    begin
                      ss := TStringStream.Create(s);
                      Document.AddFromStream(copy(ExtractFileName(aFilename),0,rpos('.',ExtractFileName(aFileName))-1),
                                             copy(ExtractFileExt(aFileName),2,length(ExtractFileExt(aFileName))),
                                             ss,
                                             '',
                                             Now());
                      FreeAndNil(Document);
                      sl.Delete(i);
                    end
                  else if Assigned(Document) then
                    begin
                      s := s + DecodeUU(sl[i]);
                      sl.Delete(i);
                    end
                  else inc(i);
                end;
              s := sl.Text;
              sl.Destroy;
            end
          else
            s := ReadStrFromStream(Sender.DecodedLines,Sender.DecodedLines.Size);
          ss := TStringStream.Create('');
          Data.BlobFieldToStream(Content.DataSet,'DATA',ss);
          if (UpperCase(Sender.Secondary) = 'PLAIN') then
            s := ss.DataString+lineending+s;
          ss.Free;
          ss := TStringStream.Create(s);
          if Content.DataSet.FieldByName('DATATYP').AsString = '' then
            Content.DataSet.FieldByName('DATATYP').AsString := Uppercase(Sender.Secondary);
          if Content.DataSet.FieldByName('DATATYP').AsString = Uppercase(Sender.Secondary) then
            begin
              Data.StreamToBlobField(ss,Content.DataSet,'DATA');
              TextThere := True;
            end
          else if Sender.Secondary = 'HTML' then
            begin //Use HTML if PLAIN+HTML are avalible
              Content.DataSet.FieldByName('DATATYP').AsString := Uppercase(Sender.Secondary);
              Data.StreamToBlobField(ss,Content.DataSet,'DATA');
              ss.Free;
              ss := TStringStream.Create('');
              Data.BlobFieldToStream(Content.DataSet,'DATA',ss);
              s := ss.DataString;
              HtmlThere := True;
            end;
          ss.Free;
       end;
    end
  else if (Sender.PrimaryCode = MP_BINARY) or (TextThere or HtmlThere) then
    begin
      if copy(ExtractFileName(Sender.Filename),0,rpos('.',ExtractFileName(Sender.FileName))-1) = '' then exit;
      if not Content.DataSet.Active then Content.Open;
      if Content.State = dsInsert then
        begin
          Content.DataSet.Post;
          Content.DataSet.Edit;
        end;
      Document := TDocument.CreateEx(Self,Data);
      Document.Select(0);
      Document.Open;
      Document.Ref_ID := Content.Id.AsVariant;
      Document.BaseTyp := 'N';
      Document.BaseID := DataSet.FieldByName('ID').AsString;
      Document.BaseVersion := Null;
      Document.BaseLanguage := Null;
      atmp := Sender.Filename;
      atmp := SysToUni(atmp);
      Document.AddFromStream(copy(ExtractFileName(atmp),0,rpos('.',ExtractFileName(atmp))-1),
                             copy(ExtractFileExt(atmp),2,length(ExtractFileExt(atmp))),
                             Sender.DecodedLines,
                             Sender.ContentID,
                             Now(),False);
      Document.Free;
    end;
end;

procedure TMimeMessage.MailAddressesFromString(aSource: string; aTarget: TStrings
  );
var
  tmp: String;
begin
  tmp := aSource;
  tmp := StringReplace(tmp,';',',',[rfReplaceAll]);
  while (pos(',',tmp) > 0) do
    begin
      aTarget.Add(copy(tmp,0,pos(',',tmp)-1));
      tmp := copy(tmp,pos(',',tmp)+1,length(tmp));
    end;
  if pos('@',tmp) > 0 then
    aTarget.Add(tmp);
end;
function TMimeMessage.EncodeMessage: TMimeMess;
var
  aMessage: TMimeMess;
  sl: TStringList;
  ss: TStringStream;
  MP: TMimePart;
  aDocument: TDocument;
  aMimePart: TMimePart;
  aParent: TMessageList;
  aPart: TMimePart;
  tmp: String;
  aEncoding: String;
begin
  Result := nil;
  aMessage := TMimeMess.Create;
  sl := TStringList.Create;
  Content.Open;
  if Content.Count=0 then exit;
  sl.Text := Content.DataSet.FieldByName('HEADER').AsString;
  aMessage.Header.DecodeHeaders(sl);
  aMessage.Header.CharsetCode:=UTF_8;
  with BaseApplication as IBaseApplication do
    aMessage.Header.XMailer := Appname+' '+StringReplace(Format('Version %f Build %d',[AppVersion,AppRevision]),',','.',[rfReplaceAll]);
  aMessage.Header.From := DataSet.FieldByName('SENDER').AsString;
  aMessage.Header.Date := DataSet.FieldByName('SENDDATE').AsDateTime;
  if not DataSet.FieldByName('REPLYTO').IsNull then
    aMessage.Header.ReplyTo:=DataSet.FieldByName('REPLYTO').AsString;
  if not DataSet.FieldByName('PARENT').IsNull then
    begin
      aParent := TMessageList.CreateEx(nil,DataModule);
      aParent.SelectByMsgID(DataSet.FieldByName('PARENT').AsVariant);
      aParent.Open;
      if aParent.Count>0 then
        begin
          aMessage.Header.CustomHeaders.Add('References: <'+aParent.FieldByName('ID').AsString+'>');
        end;
      aParent.Free;
    end;
  aMessage.Header.Subject := CharsetConversion(DataSet.FieldByName('SUBJECT').AsString,UTF_8,amessage.Header.CharsetCode);
  aMessage.Header.MessageID := DataSet.FieldByName('ID').AsString;
  MailAddressesFromString(Content.DataSet.FieldByName('RECEIVERS').AsString,aMessage.Header.ToList);
  if Content.Count > 0 then
    begin
      if not Documents.DataSet.Active then
        Documents.CreateTable;
      if not Content.Id.IsNull then
        Documents.Select(Content.Id.AsVariant,'N',DataSet.FieldByName('ID').AsString,Null,Null)
      else
        Documents.Select(0);
      Documents.Open;
      if (Content.DataSet.FieldByName('DATATYP').AsString = 'PLAIN') and (Documents.Count = 0) then
        begin
          ss := TStringStream.Create('');
          Data.BlobFieldToStream(Content.DataSet,'DATA',ss);
          ss.Position := 0;
          tmp := ss.DataString;
          sl.Text:=tmp;
          aMessage.AddPartTextEx(sl,nil,UTF_8,True,ME_QUOTED_PRINTABLE);
//          aMessage.AddPartText(sl,nil);
          ss.Free;
        end
      else
        begin
          ss := TStringStream.Create('');
          Data.BlobFieldToStream(Content.DataSet,'DATA',ss);
          ss.Position := 0;
          tmp := ss.DataString;
          sl.Text:=tmp;
          MP := aMessage.AddPartMultipart('mixed', nil);
          if Content.DataSet.FieldByName('DATATYP').AsString = 'PLAIN' then
            aMessage.AddPartTextEx(sl,MP,UTF_8,True,ME_QUOTED_PRINTABLE)
          else if Content.DataSet.FieldByName('DATATYP').AsString = 'HTML' then
            begin
              tmp := ss.DataString;
              aEncoding := GuessEncoding(tmp);
              if pos('ENCODING',Uppercase(tmp)) = 0 then
                tmp := ConvertEncoding(tmp,aEncoding,EncodingAnsi);
              sl.Text:= tmp;
              aPart := aMessage.AddPartHTML(sl,MP);
            end;
          while not Documents.DataSet.EOF do
            begin
              if Documents.DataSet.FieldByName('ISDIR').AsString <> 'Y' then
                begin
                  aDocument := TDocument.CreateEx(Self,Data);
                  aDocument.SelectByNumber(Documents.DataSet.FieldByName('NUMBER').AsInteger);
                  aDocument.Open;
                  if aDocument.Count>0 then
                    begin
                      aDocument.DataSet.Last;
                      aMimePart := aMessage.AddPart(MP);
                      with aMimePart do
                        begin
                          //-TODO: goto last revision
                          aDocument.CheckoutToStream(DecodedLines);
                          DecodedLines.Position:=0;
                          FileName := aDocument.DataSet.FieldByName('NAME').AsString+'.'+aDocument.DataSet.FieldByName('EXTENSION').AsString;
                          MimeTypeFromExt(FileName);
                          Description := 'Attached file: ' + FileName;
                          ss := TStringStream.Create('');
                          Data.BlobFieldToStream(aDocument.DataSet,'FULLTEXT',ss);
                          if length(ss.DataString)<20 then
                            ContentID := ss.DataString;
                          ss.Destroy;
                          if ContentId='' then
                            Disposition := 'attachment'
                          else
                            Disposition := 'inline';
                          FileName := FileName;
                          EncodingCode := ME_BASE64;
                          EncodePart;
                          EncodePartHeader;
                        end;
                    end;
                  aDocument.Destroy;
                  Documents.DataSet.Next;
                end;
           end;
        end;
    end;
  sl.Free;
  Result := aMessage;
  if Assigned(result) then
    begin
      aMessage.Encodemessage;
      try
        if (Self.DataSet.FieldByName('LINES').IsNull) and (not Self.DataSet.FieldByName('USER').IsNull) then
          begin
            if not Self.CanEdit then
              Self.DataSet.Edit;
            Self.DataSet.FieldbyName('LINES').AsInteger := aMessage.Lines.Count;
            Self.DataSet.FieldbyName('SIZE').AsInteger := length(aMessage.Lines.text);
            Self.DataSet.Post;
          end;
      except
      end;
    end;
end;
procedure TMimeMessage.DecodeMessage(msg: TMimeMess);
var
  atmp: String;
  sl: TStringList;
  aMsgList: TMessageList;
  aTree: TTree;
  aMessages: TMessageList;
begin
  if not CanEdit then
    DataSet.Edit;
  with DataSet do
    begin
      if FieldbyName('MSG_ID').IsNull then
        begin
          with BaseApplication as IBaseDbInterface do
            FieldByName('MSG_ID').AsInteger:=Data.GetUniID(Connection);
        end;
      if FieldByName('ID').IsNull then
        FieldByName('ID').AsString := msg.Header.MessageID;
      atmp := SysToUni(msg.Header.From);
      FieldByName('SENDER').AsString := atmp;
      FieldByName('REPLYTO').AsString := SysToUni(msg.Header.ReplyTo);
      FieldByName('SENDDATE').AsDateTime := msg.Header.Date;
      if FieldDefs.IndexOf('SENDTIME') <> -1 then
        FieldByName('SENDTIME').AsFloat := Frac(msg.Header.Date);
      atmp := SysToUni(msg.Header.Subject);
      FieldbyName('SUBJECT').AsString := atmp;
      FieldbyName('LINES').AsInteger := msg.Lines.Count;
      FieldbyName('SIZE').AsInteger := length(msg.Lines.text);
      if msg.Header.FindHeader('Newsgroups') <> '' then
        begin
          aTree := TTree.CreateEx(Self,Data,Connection);
          aTree.Open;
          atmp := trim(msg.Header.FindHeader('Newsgroups'));
          if aTree.DataSet.Locate('TYPE;NAME',VarArrayOf(['B',atmp]),[loCaseInsensitive]) then
            FieldByName('TREEENTRY').AsInteger := aTree.DataSet.FieldByName('ID').AsInteger
          else
            begin
              atmp := StringReplace(trim(msg.Header.FindHeader('Newsgroups')),'_',' ',[rfReplaceAll]);
              if aTree.DataSet.Locate('TYPE;NAME',VarArrayOf(['B',atmp]),[loCaseInsensitive]) then
                FieldByName('TREEENTRY').AsInteger := aTree.DataSet.FieldByName('ID').AsInteger;
            end;
          aTree.Free;
        end;
      if msg.Header.FindHeader('References') <> '' then
        begin
          aMsgList := TMessageList.CreateEx(Self,Data,Connection);
          atmp := msg.Header.FindHeader('References');
          while pos('<',atmp) > 0 do
            begin
              atmp := copy(atmp,pos('<',atmp)+1,length(atmp));
              aMsgList.SelectByID(copy(atmp,0,pos('>',atmp)-1));
              aMsgList.Open;
              if aMsgList.Count > 0 then
                FieldbyName('PARENT').AsInteger := aMsgList.Number.AsInteger;
            end;
          aMsgList.Destroy;
        end;
      Post;
      Edit;
      Content.Open;
      with Content.DataSet do
        begin
          Insert;
          Content.DataSet.FieldByName('ID').AsString := Self.DataSet.FieldByName('ID').AsString;
          if Content.DataSet.FieldByName('ID').AsString <> Self.DataSet.FieldByName('ID').AsString then
            Self.DataSet.FieldByName('ID').AsString := Content.DataSet.FieldByName('ID').AsString;
          FieldByName('SQL_ID').AsVariant:=Self.DataSet.FieldByName('MSG_ID').AsVariant;
          FieldbyName('REPLYTO').AsString := msg.Header.ReplyTo;
          FieldbyName('RECEIVERS').AsString := msg.Header.ToList.text;
          FieldbyName('CC').AsString := msg.Header.CcList.text;
          FieldByName('TIMESTAMPD').AsDateTime := Now();
          if FieldDefs.IndexOf('TIMESTAMPT') <> -1 then
            FieldByName('TIMESTAMPT').AsFloat := Frac(Now());
          Content.DataSet.Post;
          Content.DataSet.Edit;
          TextThere := False;
          HtmlThere := False;
          sl := TStringList.Create;
          msg.Header.EncodeHeaders(sl);
          FieldbyName('HEADER').AsString := sl.Text;
          sl.Free;
          msg.MessagePart.OnWalkPart:=@DataSetContentDataSetDataSetmsgMessagePartWalkPart;
          msg.MessagePart.WalkPart;
          if Content.CanEdit then
            Content.DataSet.Post;
        end;
    end;
end;

end.

