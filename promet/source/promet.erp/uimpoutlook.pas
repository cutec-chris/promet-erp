unit uimpoutlook;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil,  Forms, Controls, Graphics, Dialogs,
  StdCtrls, Buttons, ButtonPanel, uIntfStrConsts, db
  {$IFDEF WINDOWS}
  ,ComObj
  {$ENDIF}
  ;

type

  { TfOutlookImport }

  TfOutlookImport = class(TForm)
    ButtonPanel1: TButtonPanel;
    lbContacts: TListBox;
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  fOutlookImport: TfOutlookImport;

function ContOutlookImport : Boolean;
function ContOutlookExport : Boolean;

implementation
{$R *.lfm}

uses uData;

resourcestring
  strInsertNewContact           = 'neuen Kontakt einfügen ?';


function ContoutlookImport : Boolean;
{$ifdef WINDOWS}
const
  olFolderContacts = $0000000A;
var
  outlook, NameSpace, Contacts, Contact: OleVariant;
  i: Integer;
{$endif}
begin
{$ifdef WINDOWS}
  Outlook := CreateOleObject('Outlook.Application');
  NameSpace := outlook.GetNameSpace('MAPI');
  fOutlookImport.lbContacts.Clear;
  Contacts := NameSpace.GetDefaultFolder(olFolderContacts);
  for i := 1 to Contacts.Items.Count do
  begin
    Contact := Contacts.Items.Item(i);
    fOutlookImport.lbContacts.Items.Add(SysToUTF8(Contact.FullName));
  end;
  Result := fOutlookImport.Showmodal = mrOK;
  if Result then
    begin
      if fOutlookImport.lbContacts.SelCount = 1 then
        begin
          if MessageDlg(strInsertNewContact,mtInformation,[mbYes,mbNo],0) = mrYes then
            Data.Customers.DataSet.Append;
        end
      else
        Data.Customers.DataSet.Append;
      if not (Data.Customers.DataSet.State = dsInsert) then
        Data.Customers.DataSet.Edit;
      for i := 1 to Contacts.Items.Count do
        begin
          Contact := Contacts.Items.Item(i);
          if fOutlookImport.lbContacts.Selected[i-1] then
            begin
              if (not ((Data.Customers.DataSet.State = dsInsert)
                    or (Data.Customers.DataSet.State = dsEdit))) then
                Data.Customers.DataSet.Append;
              Data.Customers.FieldByName('NAME').AsString:=SysToUTF8(Contact.LastNameAndFirstName);
              Data.Customers.FieldByName('INFO').AsString:=SysToUTF8(Contact.Body);
              //Addresses
              if SysToUTF8(Contact.BusinessAddress) <> '' then
                begin
                  Data.Addresses.DataSet.Append;
                  with Data.Addresses.DataSet do
                    begin
                      FieldByName('ZIP').AsString := SysToUTF8(Contact.BusinessAddressPostalCode);
                      FieldbyName('CITY').AsString := SysToUTF8(Contact.BusinessAddressCity);
                      FieldbyName('ADDRESS').AsString := SysToUTF8(Contact.BusinessAddressStreet);
                      FieldByName('NAME').AsString:=SysToUTF8(Contact.LastNameAndFirstName);
                      Post;
                    end;
                end;
              if SysToUTF8(Contact.HomeAddress) <> '' then
                begin
                  Data.Addresses.DataSet.Append;
                  with Data.Addresses.DataSet do
                    begin
                      FieldByName('ZIP').AsString := SysToUTF8(Contact.HomeAddressPostalCode);
                      FieldbyName('CITY').AsString := SysToUTF8(Contact.HomeAddressCity);
                      FieldbyName('ADDRESS').AsString := SysToUTF8(Contact.HomeAddressStreet);
                      FieldByName('NAME').AsString:=SysToUTF8(Contact.LastNameAndFirstName);
                      Post;
                    end;
                end;
              if SysToUTF8(Contact.MailingAddress) <> '' then
                begin
                  Data.Addresses.DataSet.Append;
                  with Data.Addresses.DataSet do
                    begin
                      FieldByName('ZIP').AsString := SysToUTF8(Contact.MailingAddressPostalCode);
                      FieldbyName('CITY').AsString := SysToUTF8(Contact.MailingAddressCity);
                      FieldbyName('ADDRESS').AsString := SysToUTF8(Contact.MailingAddressStreet);
                      FieldByName('NAME').AsString:=SysToUTF8(Contact.LastNameAndFirstName);
                      Post;
                    end;
                end;
              //Contact Elements
              if SysToUTF8(Contact.Birthday) <> '01.01.4501' then
                with Data.CustomerCont.DataSet do
                  begin
                    Append;
                    FieldByName('TYPE').AsString:='BIR';
                    FieldbyName('DATA').AsString:=SysToUTF8(Contact.Birthday);
                    Post;
                  end;
              if SysToUTF8(Contact.BusinessTelephoneNumber) <> '' then
                with Data.CustomerCont.DataSet do
                  begin
                    Append;
                    FieldByName('TYPE').AsString:='TELB';
                    FieldbyName('DATA').AsString:=SysToUTF8(Contact.BusinessTelephoneNumber);
                    Post;
                  end;
              if SysToUTF8(Contact.Business2TelephoneNumber) <> '' then
                with Data.CustomerCont.DataSet do
                  begin
                    Append;
                    FieldByName('TYPE').AsString:='TELB';
                    FieldbyName('DATA').AsString:=SysToUTF8(Contact.Business2TelephoneNumber);
                    Post;
                  end;
              if SysToUTF8(Contact.HomeTelephoneNumber) <> '' then
                with Data.CustomerCont.DataSet do
                  begin
                    Append;
                    FieldByName('TYPE').AsString:='TELB';
                    FieldbyName('DATA').AsString:=SysToUTF8(Contact.HomeTelephoneNumber);
                    Post;
                  end;
              if SysToUTF8(Contact.Home2TelephoneNumber) <> '' then
                with Data.CustomerCont.DataSet do
                  begin
                    Append;
                    FieldByName('TYPE').AsString:='TELB';
                    FieldbyName('DATA').AsString:=SysToUTF8(Contact.Home2TelephoneNumber);
                    Post;
                  end;
              if SysToUTF8(Contact.CompanyMainTelephoneNumber) <> '' then
                with Data.CustomerCont.DataSet do
                  begin
                    Append;
                    FieldByName('TYPE').AsString:='TELB';
                    FieldbyName('DATA').AsString:=SysToUTF8(Contact.CompanyMainTelephoneNumber);
                    Post;
                  end;
              if SysToUTF8(Contact.PrimaryTelephoneNumber) <> '' then
                with Data.CustomerCont.DataSet do
                  begin
                    Append;
                    FieldByName('TYPE').AsString:='TEL';
                    FieldbyName('DATA').AsString:=SysToUTF8(Contact.PrimaryTelephoneNumber);
                    Post;
                  end;
              if SysToUTF8(Contact.ISDNNumber) <> '' then
                with Data.CustomerCont.DataSet do
                  begin
                    Append;
                    FieldByName('TYPE').AsString:='TEL';
                    FieldbyName('DATA').AsString:=SysToUTF8(Contact.ISDNNumber);
                    Post;
                  end;
              if SysToUTF8(Contact.Email1Address) <> '' then
                with Data.CustomerCont.DataSet do
                  begin
                    Append;
                    FieldByName('TYPE').AsString:='MAIL';
                    FieldbyName('DATA').AsString:=SysToUTF8(Contact.Email1Address);
                    Post;
                  end;
              if SysToUTF8(Contact.Email2Address) <> '' then
                with Data.CustomerCont.DataSet do
                  begin
                    Append;
                    FieldByName('TYPE').AsString:='MAIL';
                    FieldbyName('DATA').AsString:=SysToUTF8(Contact.Email2Address);
                    Post;
                  end;
              if SysToUTF8(Contact.Email3Address) <> '' then
                with Data.CustomerCont.DataSet do
                  begin
                    Append;
                    FieldByName('TYPE').AsString:='MAIL';
                    FieldbyName('DATA').AsString:=SysToUTF8(Contact.Email3Address);
                    Post;
                  end;
              if SysToUTF8(Contact.NickName) <> '' then
                with Data.CustomerCont.DataSet do
                  begin
                    Append;
                    FieldByName('TYPE').AsString:='NICK';
                    FieldbyName('DATA').AsString:=SysToUTF8(Contact.NickName);
                    Post;
                  end;
              if SysToUTF8(Contact.PersonalHomePage) <> '' then
                with Data.CustomerCont.DataSet do
                  begin
                    Append;
                    FieldByName('TYPE').AsString:='INT';
                    FieldbyName('DATA').AsString:=SysToUTF8(Contact.PersonalHomePage);
                    Post;
                  end;
              if SysToUTF8(Contact.BusinessHomePage) <> '' then
                with Data.CustomerCont.DataSet do
                  begin
                    Append;
                    FieldByName('TYPE').AsString:='INT';
                    FieldbyName('DATA').AsString:=SysToUTF8(Contact.BusinessHomePage);
                    Post;
                  end;
              if SysToUTF8(Contact.MobileTelephoneNumber) <> '' then
                with Data.CustomerCont.DataSet do
                  begin
                    Append;
                    FieldByName('TYPE').AsString:='CEL';
                    FieldbyName('DATA').AsString:=SysToUTF8(Contact.MobileTelephoneNumber);
                    Post;
                  end;
              Data.Customers.DataSet.Post;
            end;
        end;
    end;
  Outlook := UnAssigned;
{$endif}
end;

function ContOutlookExport : Boolean;
begin

end;

initialization

end.
