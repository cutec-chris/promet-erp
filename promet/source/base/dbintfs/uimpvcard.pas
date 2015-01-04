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
unit uImpVCard;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, Utils, uPerson, synacode,db,
  uVTools
  {$IFDEF LCL}
  , Graphics
  {$ENDIF}
  ;
function VCardImport(Customers : TPerson;vIn : TStrings;DoChange : Boolean = False) : Boolean;
function VCardExport(Customers : TPerson;vOut : TStrings) : Boolean;
implementation
function VCardImport(Customers : TPerson;vIn : TStrings;DoChange : Boolean = False): Boolean;
var
  InBody : Boolean;
  NField : string;
  tmp : string;
  i: Integer;
begin
  Result := False;
  i := 0;
  while i < vIn.Count do
    begin
      with Customers do
        begin
          tmp := vIn[i];
          inc(i);
          if IsField('BEGIN',tmp) then
            begin
              if not (DoChange) then
                begin
                  Append;
                end
              else Customers.Edit;
              InBody := True
            end
          else if IsField('END',tmp) then
            begin
              Post;
              InBody := False
            end
          else if InBody then
            begin
              Customers.ContactData.Active := True;
              Customers.Address.Active := True;
              if IsField('UID',tmp) then
                FieldByName('ORIGID').AsString := GetValue(tmp)
              else if IsField('N',tmp) then
                NField := GetValue(tmp)
              else if IsField('FN',tmp) then
                FieldByName('NAME').AsString := GetValue(tmp)
              else if IsField('TEL',tmp) then
                begin
                  if HasAttrib('WORK',tmp) and HasAttrib('CELL',tmp) then
                    begin
                      if not Customers.ContactData.Locate('DATA',GetValue(tmp),[]) then
                        Customers.ContactData.DataSet.Append
                      else
                        Customers.ContactData.Edit;
                      Customers.ContactData.FieldByName('TYPE').AsString := 'CELB';
                      Customers.ContactData.FieldByName('DATA').AsString := GetValue(tmp);
                      Customers.ContactData.Post;
                    end
                  else if HasAttrib('HOME',tmp) and HasAttrib('CELL',tmp) then
                    begin
                      if not Customers.ContactData.Locate('DATA',GetValue(tmp),[]) then
                        Customers.ContactData.DataSet.Append
                      else
                        Customers.ContactData.Edit;
                      Customers.ContactData.FieldByName('TYPE').AsString := 'CELP';
                      Customers.ContactData.FieldByName('DATA').AsString := GetValue(tmp);
                      Customers.ContactData.Post;
                    end
                  else if HasAttrib('HOME',tmp) and HasAttrib('VOICE',tmp) then
                    begin
                      if not Customers.ContactData.Locate('DATA',GetValue(tmp),[]) then
                        Customers.ContactData.DataSet.Append
                      else
                        Customers.ContactData.Edit;
                      Customers.ContactData.FieldByName('TYPE').AsString := 'TELP';
                      Customers.ContactData.FieldByName('DATA').AsString := GetValue(tmp);
                      Customers.ContactData.Post;
                    end
                  else if HasAttrib('WORK',tmp) and HasAttrib('VOICE',tmp) then
                    begin
                      if not Customers.ContactData.Locate('DATA',GetValue(tmp),[]) then
                        Customers.ContactData.DataSet.Append
                      else
                        Customers.ContactData.Edit;
                      Customers.ContactData.FieldByName('TYPE').AsString := 'TELB';
                      Customers.ContactData.FieldByName('DATA').AsString := GetValue(tmp);
                      Customers.ContactData.Post;
                    end
                  else if HasAttrib('CELL',tmp) then
                    begin
                      if not Customers.ContactData.Locate('DATA',GetValue(tmp),[]) then
                        Customers.ContactData.DataSet.Append
                      else
                        Customers.ContactData.Edit;
                      Customers.ContactData.FieldByName('TYPE').AsString := 'CEL';
                      Customers.ContactData.FieldByName('DATA').AsString := GetValue(tmp);
                      Customers.ContactData.Post;
                    end
                  else if HasAttrib('FAX',tmp) then
                    begin
                      if not Customers.ContactData.Locate('DATA',GetValue(tmp),[]) then
                        Customers.ContactData.DataSet.Append
                      else
                        Customers.ContactData.Edit;
                      Customers.ContactData.FieldByName('TYPE').AsString := 'FAX';
                      Customers.ContactData.FieldByName('DATA').AsString := GetValue(tmp);
                      Customers.ContactData.Post;
                    end
                  else
                    begin
                      if not Customers.ContactData.Locate('DATA',GetValue(tmp),[]) then
                        Customers.ContactData.DataSet.Append
                      else
                        Customers.ContactData.Edit;
                      Customers.ContactData.FieldByName('TYPE').AsString := 'TEL';
                      Customers.ContactData.FieldByName('DATA').AsString := GetValue(tmp);
                      Customers.ContactData.Post;
                    end;
                end
              else if IsField('ADR',tmp) then
                begin
                  tmp := GetValue(tmp);
                  tmp := copy(tmp,pos(';',tmp)+1,length(tmp));
                  tmp := copy(tmp,pos(';',tmp)+1,length(tmp));
                  if not Customers.Address.Locate('ADDRESS',VDecodeString(copy(tmp,0,pos(';',tmp)-1)),[]) then
                    Customers.Address.Append
                  else
                    Customers.Address.Edit;
                  Customers.Address.FieldByName('ADDRESS').AsString := VDecodeString(copy(tmp,0,pos(';',tmp)-1));
                  tmp := copy(tmp,pos(';',tmp)+1,length(tmp));
                  Customers.Address.FieldByName('CITY').AsString := VDecodeString(copy(tmp,0,pos(';',tmp)-1));
                  tmp := copy(tmp,pos(';',tmp)+1,length(tmp));
                  Customers.Address.FieldByName('STATE').AsString := VDecodeString(copy(tmp,0,pos(';',tmp)-1));
                  tmp := copy(tmp,pos(';',tmp)+1,length(tmp));
                  Customers.Address.FieldByName('ZIP').AsString := VDecodeString(copy(tmp,0,pos(';',tmp)-1));
                  tmp := copy(tmp,pos(';',tmp)+1,length(tmp));
                  Customers.Address.FieldByName('COUNTRY').AsString := VDecodeString(tmp);
                  tmp := NField;
                  Customers.Address.FieldByName('NAME').AsString := VDecodeString(copy(tmp,0,pos(';',tmp)-1));
                  tmp := copy(tmp,pos(';',tmp)+1,length(tmp));
                  Customers.Address.FieldByName('CNAME').AsString := VDecodeString(copy(tmp,0,pos(';',tmp)-1));
                  tmp := copy(tmp,pos(';',tmp)+1,length(tmp));
                  tmp := copy(tmp,pos(';',tmp)+1,length(tmp));
                  Customers.Address.FieldByName('TITLE').AsString := VDecodeString(tmp);
                  Customers.Address.Post;
                end
              else if IsField('EMAIL',tmp) then
                begin
                  if HasAttrib('WORK',tmp) then
                    begin
                      if not Customers.ContactData.Locate('DATA',GetValue(tmp),[]) then
                        Customers.ContactData.DataSet.Append
                      else
                        Customers.ContactData.Edit;
                      Customers.ContactData.FieldByName('TYPE').AsString := 'MLB';
                      Customers.ContactData.FieldByName('DATA').AsString := GetValue(tmp);
                      Customers.ContactData.Post;
                    end
                  else if HasAttrib('HOME',tmp) then
                    begin
                      if not Customers.ContactData.Locate('DATA',GetValue(tmp),[]) then
                        Customers.ContactData.DataSet.Append
                      else
                        Customers.ContactData.Edit;
                      Customers.ContactData.FieldByName('TYPE').AsString := 'MLP';
                      Customers.ContactData.FieldByName('DATA').AsString := GetValue(tmp);
                      Customers.ContactData.Post;
                    end
                  else
                    begin
                      if not Customers.ContactData.Locate('DATA',GetValue(tmp),[]) then
                        Customers.ContactData.DataSet.Append
                      else
                        Customers.ContactData.Edit;
                      Customers.ContactData.FieldByName('TYPE').AsString := 'MAIL';
                      Customers.ContactData.FieldByName('DATA').AsString := GetValue(tmp);
                      Customers.ContactData.Post;
                    end;
                end
              else if IsField('INTERNET',tmp) then
                begin
                  if not Customers.ContactData.Locate('DATA',GetValue(tmp),[]) then
                    Customers.ContactData.DataSet.Append
                  else
                    Customers.ContactData.Edit;
                  Customers.ContactData.FieldByName('TYPE').AsString := 'INT';
                  Customers.ContactData.FieldByName('DATA').AsString := GetValue(tmp);
                  Customers.ContactData.Post;
                end
              else //debugln('Field not found:'+tmp)
              ;
            end;
        end;
    end;
  Result := True;
end;

function VCardExport(Customers : TPerson;vOut : TStrings): Boolean;
var
  s: TStream;
  {$IFDEF LCL}
  iPerson: TPicture;
  aBitmap: TBitmap;
  {$ENDIF}
  ss: TStringStream;
  GraphExt: String;
  Aspect: Double;

  procedure WriteField(Name : string;Value : string;Encoding : string = '8bit';Charset : string = 'utf8');
  begin
    if (Encoding = '') and (Charset = '') then
      vOut.Add(Name+':'+Value)
    else if (Encoding <> '') and (Charset = '') then
      vOut.Add(Name+';ENCODING='+Encoding+':'+Value)
    else
      vOut.Add(Name+';ENCODING='+Encoding+';CHARSET='+Charset+':'+Value);
  end;

begin
  with Customers do
    begin
      vOut.Add('BEGIN:VCARD');
      WriteField('VERSION','2.1','','');
      if FieldByName('ORIGID').IsNull then
        WriteField('UID',FieldByName('SQL_ID').AsString,'','')
      else
        WriteField('UID',FieldByName('ORIGID').AsString,'','');
      WriteField('FN',FieldByName('NAME').AsString);
      if not FieldByName('INFO').IsNull then
        WriteField('NOTE',VEncodeString(FieldByName('INFO').AsString),'QUOTED-PRINTABLE');
      Customers.ContactData.Open;
      Customers.ContactData.First;
      while not Customers.ContactData.EOF do
        begin
          if trim(Customers.ContactData.FieldByName('TYPE').AsString) = 'TEL' then
            WriteField('TEL;VOICE',Customers.ContactData.FieldByName('DATA').AsString)
          else if trim(Customers.ContactData.FieldByName('TYPE').AsString) = 'TELB' then
            WriteField('TEL;WORK;VOICE',Customers.ContactData.FieldByName('DATA').AsString)
          else if trim(Customers.ContactData.FieldByName('TYPE').AsString) = 'TELP' then
            WriteField('TEL;HOME;VOICE',Customers.ContactData.FieldByName('DATA').AsString)
          else if trim(Customers.ContactData.FieldByName('TYPE').AsString) = 'CEL' then
            WriteField('TEL;CELL;VOICE',Customers.ContactData.FieldByName('DATA').AsString)
          else if trim(Customers.ContactData.FieldByName('TYPE').AsString) = 'CELB' then
            WriteField('TEL;CELL;WORK;VOICE',Customers.ContactData.FieldByName('DATA').AsString)
          else if trim(Customers.ContactData.FieldByName('TYPE').AsString) = 'CELP' then
            WriteField('TEL;CELL;HOME;VOICE',Customers.ContactData.FieldByName('DATA').AsString)
          else if trim(Customers.ContactData.FieldByName('TYPE').AsString) = 'FAX' then
            WriteField('TEL;FAX',Customers.ContactData.FieldByName('DATA').AsString)
          else if trim(Customers.ContactData.FieldByName('TYPE').AsString) = 'MAIL' then
            WriteField('EMAIL;INTERNET;PREF',Customers.ContactData.FieldByName('DATA').AsString)
          else if trim(Customers.ContactData.FieldByName('TYPE').AsString) = 'MLB' then
            WriteField('EMAIL;INTERNET;WORK;PREF',Customers.ContactData.FieldByName('DATA').AsString)
          else if trim(Customers.ContactData.FieldByName('TYPE').AsString) = 'MLP' then
            WriteField('EMAIL;INTERNET;HOME;PREF',Customers.ContactData.FieldByName('DATA').AsString)
          else if trim(Customers.ContactData.FieldByName('TYPE').AsString) = 'INT' then
            WriteField('INTERNET',Customers.ContactData.FieldByName('DATA').AsString)
          ;
          Customers.ContactData.Next;
        end;
      Customers.Address.Open;
      Customers.Address.First;
      WriteField('N',Customers.Address.FieldByName('NAME').AsString+';'+Customers.Address.FieldByName('CNAME').AsString+';;'+Customers.Address.FieldByName('TITLE').AsString);
      while not Customers.Address.EOF do
        begin
          if Customers.Address.FieldByName('ACTIVE').AsString <> 'N' then
            WriteField('ADR;TYPE=HOME',';;'+VEncodeString(Customers.Address.FieldByName('ADDRESS').AsString)+';'
                    +VEncodeString(Customers.Address.FieldByName('CITY').AsString)+';'
                    +VEncodeString(Customers.Address.FieldByName('STATE').AsString)+';'
                    +VEncodeString(Customers.Address.FieldByName('ZIP').AsString),'QUOTED-PRINTABLE','');
          Customers.Address.Next;
        end;
      with Customers.Images do
        begin
          Open;
          if Count > 0 then
            begin
              {$IFDEF LCL}
              iPerson := TPicture.Create;
              s := Customers.Images.DataSet.CreateBlobStream(Customers.Images.FieldByName('IMAGE'),bmRead);
              if (S=Nil) or (s.Size = 0) then
                begin
                  iPerson.Clear;
                end
              else
                begin
                  GraphExt :=  s.ReadAnsiString;
                  iPerson.LoadFromStreamWithFileExt(s,GraphExt);
                end;
              s.Free;
              aBitmap := TBitmap.Create;
              if iPerson.Width > 200 then
                begin
                  Aspect := iPerson.Width/iPerson.Height;
                  aBitmap.Width:=100;
                  aBitmap.Height:=round(100/Aspect);
                  aBitmap.Canvas.StretchDraw(Rect(0,0,100,round(100/Aspect)),iPerson.Graphic);
                  iPerson.Assign(aBitmap);
                end;
              ss := TStringStream.Create('');
              iPerson.SaveToStreamWithFileExt(ss,'jpg');
              aBitmap.Free;
              ss.Position:=0;
              WriteField('PHOTO;ENCODING=b;TYPE=image/jpeg',EncodeBase64(ss.DataString),'','');
              {$ENDIF}
              ss.Free;
            end;
        end;
      vOut.Add('END:VCARD');
    end;
  Result := True;
end;

end.
