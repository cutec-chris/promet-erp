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
  if Customers.DataSet.State=dsInsert then
    begin
      Customers.FieldByName('NAME').AsString := trim(Addr[0]);
      tmp := StringReplace(UpperCase(StringReplace(ValidateFileName(Customers.FieldByName('NAME').AsString),'_','',[rfReplaceAll])),' ','',[rfReplaceAll]);
      tmp := StringReplace(tmp,'-','',[rfReplaceAll]);
      Customers.FieldByName('MATCHCODE').AsString := tmp;
    end;
  if Customers.Address.DataSet.State <> dsInsert then
    Customers.Address.DataSet.Append;
  Customers.Address.FieldByName('NAME').AsString := trim(Addr[0]);
  Addr.Delete(0);
  i := Addr.Count-1;
  if Addr.Count = 0 then exit;
  while i > 0 do
    begin
      tmp := Addr[i];
      if IsNumeric(copy(trim(tmp),0,pos(' ',trim(tmp))-1)) then
        begin
          Customers.Address.FieldByName('ZIP').AsString := copy(trim(tmp),0,pos(' ',trim(tmp))-1);
          Customers.Address.FieldByName('CITY').AsString := copy(trim(tmp),pos(' ',trim(tmp))+1,length(trim(tmp)));
          Addr.Delete(i);
          break;
        end
      else if IsNumeric(copy(copy(trim(tmp),pos(' ',trim(tmp))+1,length(trim(tmp))),pos(' ',copy(trim(tmp),pos(' ',trim(tmp))+1,length(trim(tmp))))+1,length(copy(trim(tmp),pos(' ',trim(tmp))+1,length(trim(tmp))))))
           or IsNumeric(copy(copy(trim(tmp),pos('-',trim(tmp))+1,length(trim(tmp))),pos('-',copy(trim(tmp),pos('-',trim(tmp))+1,length(trim(tmp))))+1,length(copy(trim(tmp),pos('-',trim(tmp))+1,length(trim(tmp)))))) then
        begin
          Customers.Address.FieldByName('ZIP').AsString := copy(copy(trim(tmp),pos(' ',trim(tmp))+1,length(trim(tmp))),pos(' ',copy(trim(tmp),pos(' ',trim(tmp))+1,length(trim(tmp))))+1,length(copy(trim(tmp),pos(' ',trim(tmp))+1,length(trim(tmp)))));
          tmp := copy(tmp,pos(Customers.Address.FieldByName('ZIP').AsString,tmp)+length(Customers.Address.FieldByName('ZIP').AsString),length(tmp));
          Customers.Address.FieldByName('CITY').AsString := tmp;
          Addr.Delete(i);
          break;
        end
      else if IsNumeric(copy(copy(trim(tmp),pos('-',trim(tmp))+1,length(trim(tmp))),pos('-',copy(trim(tmp),pos('-',trim(tmp))+1,length(trim(tmp))))+1,length(copy(trim(tmp),pos('-',trim(tmp))+1,length(trim(tmp)))))) then
        begin
          Customers.Address.FieldByName('ZIP').AsString := copy(copy(trim(tmp),pos('-',trim(tmp))+1,length(trim(tmp))),pos('-',copy(trim(tmp),pos('-',trim(tmp))+1,length(trim(tmp))))+1,length(copy(trim(tmp),pos('-',trim(tmp))+1,length(trim(tmp)))));
          tmp := copy(tmp,pos(Customers.Address.FieldByName('ZIP').AsString,tmp)+length(Customers.Address.FieldByName('ZIP').AsString),length(tmp));
          Customers.Address.FieldByName('CITY').AsString := tmp;
          Addr.Delete(i);
          break;
        end;
      dec(i);
    end;
  i := Addr.Count-1;
  if i > -1 then
    begin
      Customers.Address.FieldByName('ADDRESS').AsString := Addr[i];
      Addr.Delete(i);
    end;
  if Addr.Count > 0 then
    Customers.Address.FieldByName('ADDITIONAL').AsString := Addr[0];
  Addr.Free;
end;

end.
