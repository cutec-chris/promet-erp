unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  dnssend,smtpsend,synamisc,mimemess, mimepart,synachar,fpolestorage;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Edit1: TEdit;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

procedure TForm1.Button1Click(Sender: TObject);
var
  DNSServers : TStringList;
  msg: TMimeMess;
  sl: TStringList;
  smtp: TSMTPSend;
  i: Integer;
  aTo: TCaption;
  aServers: TStringList;
  mimePart: TMimePart;
  t: TStringList;
begin
  aServers := TStringList.Create;
  DNSServers := TStringList.Create;
  DNSServers.CommaText:=GetDNS;
  aTo := Edit1.Text;
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
          msg := TMimeMess.Create;
          msg.Header.Subject:='Aufgabenblah';
          msg.Header.ToList.Text:=Edit1.Text;
          {
          try
            t := TStringList.Create;
            t.Add('BEGIN:VCALENDAR');
            t.Add('VERSION:2.0');
            t.Add('PRODID:-//ABC Corporation//NONSGML My Product//EN');
            t.Add('BEGIN:VTODO');
            t.Add('DTSTAMP:19980130T134500Z');
            t.Add('SEQUENCE:2');
            t.Add('UID:uid4@host1.com');
//            t.Add('ACTION:AUDIO');
//            t.Add('TRIGGER:19980403T120000');
//            t.Add('REPEAT:4');
            t.Add('DURATION:PT1H');
            t.Add('END:VTODO');
            t.Add('END:VCALENDAR');
            t.LoadFromFile('c:\3.ics');
            mimePart := msg.AddPart(nil);
            with mimePart do
              begin
                t.SaveToStream(DecodedLines);
                Primary := 'text';
                Secondary := 'calendar; method=REQUEST';
                CharsetCode := GetCurCP;
                EncodingCode := ME_8BIT;
                EncodePart;
                CharsetCode := UTF_8;
                EncodePartHeader;
              end;
           finally
             t.Free;
           end;
         }
         try
           t := TStringList.Create;
           t.Add('From: "Rechlin, Andreas" <rechlin@tcs-germany.de>');
           t.Add('Subject: =?iso-8859-1?Q?1_W=F6chiges_Praktikum_in_Montage?=');
           t.Add('Thread-Index: Ac7p5hkH0iwuVC3jRWmGsa5Rq6F19g==');
           t.Add('Date: Mon, 25 Nov 2013 13:56:11 +0000');
           t.Add('X-MS-Has-Attach:');
           t.Add('X-MS-TNEF-Correlator:');
           t.Add('Content-Type: text/plain; charset="iso-8859-1"');
           t.Add('Content-Transfer-Encoding: quoted-printable');
           t.Add('MIME-Version: 1.0');
           t.Add('');

           mimePart := msg.AddPart(nil);
           with mimePart do
             begin
               t.SaveToStream(DecodedLines);
               Primary := 'message';
               Secondary := 'rfc822';
               CharsetCode := GetCurCP;
               EncodingCode := ME_8BIT;
               EncodePart;
               CharsetCode := UTF_8;
               EncodePartHeader;
             end;
          finally
            t.Free;
          end;
          smtp.MailFrom(aTo,length(aTo));
          smtp.MailTo(Edit1.Text);
          msg.EncodeMessage;
          //t := TStringList.Create;
          //t.LoadFromFile('c:\1.rtf');
          smtp.MailData(msg.Lines);
          //t.Free;
          smtp.Free;
        end;
    end;
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  aStor: TOLEStorage;
  aDoc : TOLEDocument;
begin
  aStor := TOLEStorage.Create;
  aStor.ReadOLEFile('c:\testaufgabe.msg',aDoc,'!');
end;

end.

