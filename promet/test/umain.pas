unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    eServer: TEdit;
    Memo1: TMemo;
    Servers: TMemo;
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
uses ldapsend,dnssend;
{$R *.lfm}

procedure TForm1.Button1Click(Sender: TObject);
var
  ldap: TLDAPsend;
  l: TStringList;
begin
  ldap:= TLDAPsend.Create;
  l := TStringList.Create;
  try
    ldap.TargetHost := eServer.Text;
    ldap.Login;
    ldap.Bind;
    l.Add('displayname');
    l.Add('description');
    l.Add('givenName');
    l.Add('*');
    ldap.Search('dc=tcs,dc=biz', False, '(objectclass=*)', l);
    memo1.Lines.Add(LDAPResultdump(ldap.SearchResult));
    ldap.Logout;
  finally
    ldap.Free;
    l.Free;
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  DNS: TDNSSend;
  t: TStringList;
  n, m, x: Integer;
begin
  Servers.Lines.Clear;
  t := TStringList.Create;
  DNS := TDNSSend.Create;
  try
    DNS.TargetHost := '10.0.0.5';
    if DNS.DNSQuery('_ldap._tcp.'+'tcs.biz', QType_SRV, t) then
    begin
      { normalize preference number to 5 digits }
      for n := 0 to t.Count - 1 do
      begin
        x := Pos(',', t[n]);
        if x > 0 then
          for m := 1 to 6 - x do
            t[n] := '0' + t[n];
      end;
      { sort server list }
      t.Sorted := True;
      { result is sorted list without preference numbers }
      for n := 0 to t.Count - 1 do
      begin
        x := Pos(',', t[n]);
        Servers.Lines.Add(Copy(t[n], x + 1, Length(t[n]) - x));
      end;
    end;
  finally
    DNS.Free;
    t.Free;
  end;
end;

end.

