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
    fOutlookImport.lbContacts.Items.Add(SysToUni(Contact.FullName));
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
              Data.Customers.FieldByName('NAME').AsString:=SysToUni(Contact.LastNameAndFirstName);
              Data.Customers.FieldByName('INFO').AsString:=SysToUni(Contact.Body);
              //Addresses
              if SysToUni(Contact.BusinessAddress) <> '' then
                begin
                  Data.Addresses.DataSet.Append;
                  with Data.Addresses.DataSet do
                    begin
                      FieldByName('ZIP').AsString := SysToUni(Contact.BusinessAddressPostalCode);
                      FieldbyName('CITY').AsString := SysToUni(Contact.BusinessAddressCity);
                      FieldbyName('ADDRESS').AsString := SysToUni(Contact.BusinessAddressStreet);
                      FieldByName('NAME').AsString:=SysToUni(Contact.LastNameAndFirstName);
                      Post;
                    end;
                end;
              if SysToUni(Contact.HomeAddress) <> '' then
                begin
                  Data.Addresses.DataSet.Append;
                  with Data.Addresses.DataSet do
                    begin
                      FieldByName('ZIP').AsString := SysToUni(Contact.HomeAddressPostalCode);
                      FieldbyName('CITY').AsString := SysToUni(Contact.HomeAddressCity);
                      FieldbyName('ADDRESS').AsString := SysToUni(Contact.HomeAddressStreet);
                      FieldByName('NAME').AsString:=SysToUni(Contact.LastNameAndFirstName);
                      Post;
                    end;
                end;
              if SysToUni(Contact.MailingAddress) <> '' then
                begin
                  Data.Addresses.DataSet.Append;
                  with Data.Addresses.DataSet do
                    begin
                      FieldByName('ZIP').AsString := SysToUni(Contact.MailingAddressPostalCode);
                      FieldbyName('CITY').AsString := SysToUni(Contact.MailingAddressCity);
                      FieldbyName('ADDRESS').AsString := SysToUni(Contact.MailingAddressStreet);
                      FieldByName('NAME').AsString:=SysToUni(Contact.LastNameAndFirstName);
                      Post;
                    end;
                end;
              //Contact Elements
              if SysToUni(Contact.Birthday) <> '01.01.4501' then
                with Data.CustomerCont.DataSet do
                  begin
                    Append;
                    FieldByName('TYPE').AsString:='BIR';
                    FieldbyName('DATA').AsString:=SysToUni(Contact.Birthday);
                    Post;
                  end;
              if SysToUni(Contact.BusinessTelephoneNumber) <> '' then
                with Data.CustomerCont.DataSet do
                  begin
                    Append;
                    FieldByName('TYPE').AsString:='TELB';
                    FieldbyName('DATA').AsString:=SysToUni(Contact.BusinessTelephoneNumber);
                    Post;
                  end;
              if SysToUni(Contact.Business2TelephoneNumber) <> '' then
                with Data.CustomerCont.DataSet do
                  begin
                    Append;
                    FieldByName('TYPE').AsString:='TELB';
                    FieldbyName('DATA').AsString:=SysToUni(Contact.Business2TelephoneNumber);
                    Post;
                  end;
              if SysToUni(Contact.HomeTelephoneNumber) <> '' then
                with Data.CustomerCont.DataSet do
                  begin
                    Append;
                    FieldByName('TYPE').AsString:='TELB';
                    FieldbyName('DATA').AsString:=SysToUni(Contact.HomeTelephoneNumber);
                    Post;
                  end;
              if SysToUni(Contact.Home2TelephoneNumber) <> '' then
                with Data.CustomerCont.DataSet do
                  begin
                    Append;
                    FieldByName('TYPE').AsString:='TELB';
                    FieldbyName('DATA').AsString:=SysToUni(Contact.Home2TelephoneNumber);
                    Post;
                  end;
              if SysToUni(Contact.CompanyMainTelephoneNumber) <> '' then
                with Data.CustomerCont.DataSet do
                  begin
                    Append;
                    FieldByName('TYPE').AsString:='TELB';
                    FieldbyName('DATA').AsString:=SysToUni(Contact.CompanyMainTelephoneNumber);
                    Post;
                  end;
              if SysToUni(Contact.PrimaryTelephoneNumber) <> '' then
                with Data.CustomerCont.DataSet do
                  begin
                    Append;
                    FieldByName('TYPE').AsString:='TEL';
                    FieldbyName('DATA').AsString:=SysToUni(Contact.PrimaryTelephoneNumber);
                    Post;
                  end;
              if SysToUni(Contact.ISDNNumber) <> '' then
                with Data.CustomerCont.DataSet do
                  begin
                    Append;
                    FieldByName('TYPE').AsString:='TEL';
                    FieldbyName('DATA').AsString:=SysToUni(Contact.ISDNNumber);
                    Post;
                  end;
              if SysToUni(Contact.Email1Address) <> '' then
                with Data.CustomerCont.DataSet do
                  begin
                    Append;
                    FieldByName('TYPE').AsString:='MAIL';
                    FieldbyName('DATA').AsString:=SysToUni(Contact.Email1Address);
                    Post;
                  end;
              if SysToUni(Contact.Email2Address) <> '' then
                with Data.CustomerCont.DataSet do
                  begin
                    Append;
                    FieldByName('TYPE').AsString:='MAIL';
                    FieldbyName('DATA').AsString:=SysToUni(Contact.Email2Address);
                    Post;
                  end;
              if SysToUni(Contact.Email3Address) <> '' then
                with Data.CustomerCont.DataSet do
                  begin
                    Append;
                    FieldByName('TYPE').AsString:='MAIL';
                    FieldbyName('DATA').AsString:=SysToUni(Contact.Email3Address);
                    Post;
                  end;
              if SysToUni(Contact.NickName) <> '' then
                with Data.CustomerCont.DataSet do
                  begin
                    Append;
                    FieldByName('TYPE').AsString:='NICK';
                    FieldbyName('DATA').AsString:=SysToUni(Contact.NickName);
                    Post;
                  end;
              if SysToUni(Contact.PersonalHomePage) <> '' then
                with Data.CustomerCont.DataSet do
                  begin
                    Append;
                    FieldByName('TYPE').AsString:='INT';
                    FieldbyName('DATA').AsString:=SysToUni(Contact.PersonalHomePage);
                    Post;
                  end;
              if SysToUni(Contact.BusinessHomePage) <> '' then
                with Data.CustomerCont.DataSet do
                  begin
                    Append;
                    FieldByName('TYPE').AsString:='INT';
                    FieldbyName('DATA').AsString:=SysToUni(Contact.BusinessHomePage);
                    Post;
                  end;
              if SysToUni(Contact.MobileTelephoneNumber) <> '' then
                with Data.CustomerCont.DataSet do
                  begin
                    Append;
                    FieldByName('TYPE').AsString:='CEL';
                    FieldbyName('DATA').AsString:=SysToUni(Contact.MobileTelephoneNumber);
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
