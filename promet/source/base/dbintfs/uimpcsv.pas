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
unit uImpCSV;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, db, Utils, uBaseDbInterface, uBaseApplication;
procedure CSVExport(Filename : string;Delemiter : char;DataSet : TDataSet);
procedure CSVImport(Filename : string;Delemiter : char;DataSet : TDataSet);
implementation
uses uData;
procedure CSVExport(Filename: string; Delemiter: char; DataSet: TDataSet);
var
  f : TextFile;
  i: Integer;
  a: Integer;
begin
  AssignFile(f,Filename);
  rewrite(f);
  with DataSet as IBaseDbFilter do
    begin
      Filter := '';
      Limit := 0;
    end;
  DataSet.Open;
  for i := 0 to DataSet.FieldCount-1 do
    begin
      if (DataSet.FieldDefs[i].Name = 'SQLITE_ID')
      or (DataSet.FieldDefs[i].Name = 'SQL_ID')
      or (DataSet.FieldDefs[i].Name = 'TIMESTAMPD')
      then continue;
      write(f,DataSet.FieldDefs[i].Name);
      write(f,Delemiter);
    end;
  writeln(f,'');
  a := 0;
  DataSet.First;
  while not DataSet.EOF do
    begin
      for i := 0 to DataSet.FieldCount-1 do
        begin
          if (DataSet.FieldDefs[i].Name = 'SQLITE_ID')
          or (DataSet.FieldDefs[i].Name = 'SQL_ID')
          or (DataSet.FieldDefs[i].Name = 'TIMESTAMPD')
          then continue;
          if not ((DataSet.FieldDefs[i].DataType = ftMemo) or (DataSet.FieldDefs[i].DataType = ftBlob)) then
            write(f,'"'+StringReplace(DataSet.Fields[i].AsString,#13,'',[rfReplaceAll])+'"')
          else
            begin
              Data.BlobFieldToFile(DataSet,DataSet.FieldDefs[i].Name,ExtractFileDir(Filename)+DataSet.Name+'_'+DataSet.FieldDefs[i].name+IntToStr(a)+'.dat');
              write(f,'"'+DataSet.Name+'_'+DataSet.FieldDefs[i].name+IntToStr(a)+'.dat'+'"')
            end;
          write(f,Delemiter);
        end;
      write(f,#13);
      DataSet.Next;
      inc(a);
    end;
  closefile(f);
end;
procedure CSVImport(Filename: string; Delemiter: char; DataSet: TDataSet);
var
  f : TextFile;
  header : tstringlist;
  tmp: string;
  c : char;
  idx : Integer;
  intextseparator : Boolean;
  lc: Char;
begin
  DataSet.DisableControls;
  header := TStringList.Create;
  AssignFile(f,Filename);
  Reset(f);
  try
  readln(f,tmp);
  header.Delimiter:=Delemiter;
  header.DelimitedText := StringReplace(tmp,'"','',[rfReplaceAll]);
  intextseparator := false;
  idx := 0;
  tmp := '';
  DataSet.Append;
  while not eof(f) do
    begin
      lc := c;
      read(f,c);
      if (c=#13) or ((lc<>#13) and (c=#10)) then
        begin
          if (idx < header.Count) and (DataSet.FieldDefs.IndexOf(header[idx]) <> -1) then
            DataSet.FieldByName(header[idx]).AsString := SysToUni(tmp);
          tmp := '';
          idx := 0;
          intextseparator := false;
          try
            DataSet.Post;
          except
            on e : Exception do
              begin
                //debugln(e.Message);
                DataSet.Cancel;
              end;
          end;
          DataSet.Append;
        end
      else if (c=#10) and (tmp = '') then
      else if c = Delemiter then
        begin
          if not intextseparator then
            begin
              if tmp <> '' then
                begin
                  try
                    if (FileExists(UniToSys(ExtractFileDir(Filename)+tmp))) then
                      begin
                        Data.FileToBlobField(ExtractFileDir(Filename)+tmp,DataSet,header[idx]);
                      end
                    else
                      DataSet.FieldByName(header[idx]).AsString := tmp;//SysToUni(ConvertEncoding(tmp,GuessEncoding(tmp),EncodingUTF8));
                  except
                  end;
                end;
              tmp := '';
              inc(idx);
            end
          else
            tmp := tmp+c;
        end
      else if c = '"' then
        begin
          intextseparator := not intextseparator;
        end
      else
        begin
          tmp := tmp+c;
        end;
    end;
  except
    on e : Exception do
      begin
        with BaseApplication as IBaseApplication do
          begin
            debug('Zeile:'+tmp+' Fehler:'+e.Message);
          end;
      end;
  end;
  DataSet.EnableControls;
  CloseFile(f);
  if DataSet.State = dsInsert then
    DataSet.Cancel;
  header.Destroy;
end;
end.
