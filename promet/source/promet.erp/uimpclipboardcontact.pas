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
Created 01.06.2006
*******************************************************************************}

unit uImpClipboardContact;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,Clipbrd,Utils, db, Dialogs, uIntfStrConsts, Controls,
  uPerson;

function ContClipBoardImport(Customers : TPerson;AskForNewContact : Boolean = True) : Boolean;

implementation

uses uData;

resourcestring
  strInsertNewContact           = 'neuen Kontakt einfügen ?';


function ContClipBoardImport(Customers : TPerson;AskForNewContact : Boolean = True) : Boolean;
var
  Addr : TStringList;
  i: Integer;
  tmp: string;
begin
  Addr := TStringList.Create;
  tmp := Clipboard.AsText;
  tmp := StringReplace(tmp,',',lineending,[rfReplaceAll]);
  Addr.Text := tmp;

  if (Addr.Count = 0) then exit;
  //Delete clear lines
  i := 0;
  while i < Addr.Count do
    if Addr[i] = '' then
      Addr.Delete(i)
    else
      inc(i);
  if (Addr.Count = 0) then exit;
  //Insert Customer
  if AskForNewContact then
    if MessageDlg(strInsertNewContact,mtInformation,[mbYes,mbNo],0) = mrYes then
      Customers.DataSet.Append;
  //Check and Remove for Contact propertys
  i := 0;
  while i < Addr.Count do
    begin
      if (pos('tel',lowercase(Addr[i])) > 0) or (pos('phone',lowercase(Addr[i])) > 0) or (pos('mobile',lowercase(Addr[i])) > 0) then
        begin
          Customers.CustomerCont.DataSet.Append;
          Customers.CustomerCont.FieldByName('TYPE').AsString := 'TEL';
          if pos(':',Addr[i]) > 0 then
            Customers.CustomerCont.FieldByName('DATA').AsString := copy(Addr[i],pos(':',Addr[i])+1,length(Addr[i]))
          else
            Customers.CustomerCont.FieldByName('DATA').AsString := Addr[i];
          Customers.CustomerCont.DataSet.Post;
          Addr.Delete(i);
        end
      else if pos('fax',lowercase(Addr[i])) > 0 then
        begin
          Customers.CustomerCont.DataSet.Append;
          Customers.CustomerCont.FieldByName('TYPE').AsString := 'FAX';
          if pos(':',Addr[i]) > 0 then
            Customers.CustomerCont.FieldByName('DATA').AsString := copy(Addr[i],pos(':',Addr[i])+1,length(Addr[i]))
          else
            Customers.CustomerCont.FieldByName('DATA').AsString := Addr[i];
          Customers.CustomerCont.DataSet.Post;
          Addr.Delete(i);
        end
      else if pos('mail',lowercase(Addr[i])) > 0 then
        begin
          Customers.CustomerCont.DataSet.Append;
          Customers.CustomerCont.FieldByName('TYPE').AsString := 'MAIL';
          if pos(':',Addr[i]) > 0 then
            Customers.CustomerCont.FieldByName('DATA').AsString := copy(Addr[i],pos(':',Addr[i])+1,length(Addr[i]))
          else
            Customers.CustomerCont.FieldByName('DATA').AsString := Addr[i];
          Customers.CustomerCont.DataSet.Post;
          Addr.Delete(i);
        end
      else
        inc(i);
    end;
  //The rest should be the adress
  if Customers.Address.DataSet.State <> dsInsert then
    Customers.Address.DataSet.Append;
  Customers.Address.FromString(Addr.Text);
  Addr.Free;
  Customers.Edit;
  Customers.FieldByName('NAME').AsString:=Customers.Address.DataSet.FieldByName('NAME').AsString;
end;

end.
