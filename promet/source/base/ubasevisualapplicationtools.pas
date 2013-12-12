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
unit ubasevisualapplicationtools;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, DBGrids, Grids, DB, uBaseApplication, uBaseDbInterface, Utils;
type
  TFilterCellTextEvent = procedure(Sender : TObject;aCol : TColumn;aText : string;var NewText : string) of object;
function BuildAutofilter(List : TDBGrid;Header : TStringGrid;aEvent : TFilterCellTextEvent = nil) : string;
implementation
uses uData;
function BuildAutofilter(List : TDBGrid;Header : TStringGrid;aEvent : TFilterCellTextEvent) : string;
  function BuildColumnFilter(Field : TField;Filter : string) : string;
  var
    aValue: String;
    aDate: TDateTime;
    aFloat: Extended;
  begin
    Result := '';
    if not Assigned(Field) then exit;
    if (copy(trim(Filter),0,1) = '>')
    or (copy(trim(Filter),0,1) = '<')
    then
      begin
        with BaseApplication as IBaseDbInterface do
          begin
            aValue := trim(copy(Filter,2,length(Filter)));
            if aValue = '' then exit;
            if TryStrToDate(aValue,aDate) then
              Result := Data.QuoteField(Field.FieldName)+copy(trim(Filter),0,1)+Data.DateToFilter(aDate)
            else
              Result := Data.QuoteField(Field.FieldName)+copy(trim(Filter),0,1)+Data.QuoteValue(aValue);
          end;
      end
    else if copy(trim(Filter),0,1) = '!' then
      begin
        aValue := trim(copy(Filter,2,length(Filter)));
        if TryStrToDate(aValue,aDate) then
          Result := Data.QuoteField(Field.FieldName)+'<>'+Data.DateToFilter(trunc(aDate))
        else if TryStrToFloat(aValue,aFloat) then
          Result := Data.QuoteField(Field.FieldName)+'<>'+Data.QuoteValue(StringReplace(aValue,',','.',[]))
        else
          begin
            with Field.DataSet as IBaseDbFilter do
              begin
                if (Field.DataType = ftMemo) or (Field.DataType = ftWideMemo) then
                  Result := Data.ProcessTerm('UPPER(cast('+Data.QuoteField(Field.FieldName)+' as varchar(100)))<>UPPER('+Data.QuoteValue(aValue)+')')
                else
                  Result := Data.ProcessTerm('UPPER('+Data.QuoteField(Field.FieldName)+')<>UPPER('+Data.QuoteValue(aValue)+')');
              end;
          end;
      end
    else if trim(Filter) = 'NULL' then
      begin
        Result := Data.QuoteField(Field.FieldName)+' is NULL';
      end
    else
      begin
        with BaseApplication as IBaseDbInterface do
          begin
            aValue := trim(Filter);
            if copy(aValue,0,1) = '=' then aValue := copy(aValue,2,length(aValue));
            if aValue = '' then exit;
            if TryStrToDate(aValue,aDate) and ((Field.DataType=ftDateTime) or (Field.DataType=ftDate)) then
              Result := Data.QuoteField(Field.FieldName)+'>='+Data.DateToFilter(trunc(aDate))+' AND '+Data.QuoteField(Field.FieldName)+'<'+Data.DateToFilter(trunc(aDate)+1)
            else if TryStrToFloat(aValue,aFloat) then
              Result := Data.QuoteField(Field.FieldName)+'='+Data.QuoteValue(StringReplace(aValue,',','.',[]))
            else
              begin
                if copy(aValue,0,1)='"' then
                  aValue := copy(aValue,2,length(aValue))
                else if (pos('*',aValue)=0)
                and (pos('?',aValue)=0) then
                  aValue := '*'+aValue;
                if copy(aValue,length(aValue),1)='"' then
                  aValue := copy(aValue,0,length(aValue)-1)
                else if (rpos('*',aValue)=1)
                and (pos('?',aValue)=0) then
                  aValue := aValue+'*';
                with Field.DataSet as IBaseDbFilter do
                  begin
                    if (Field.DataType = ftMemo) or (Field.DataType = ftWideMemo) then
                      Result := Data.ProcessTerm('UPPER(cast('+Data.QuoteField(Field.FieldName)+' as varchar(100)))=UPPER('+Data.QuoteValue(aValue)+')')
                    else
                      Result := Data.ProcessTerm('UPPER('+Data.QuoteField(Field.FieldName)+')=UPPER('+Data.QuoteValue(aValue)+')');
                  end;
              end;
          end;
      end;
  end;
var
  i: Integer;
begin
  with BaseApplication as IBaseDbInterface do
    if not Assigned(Data) then exit;
  if Header.RowCount < 2 then exit;
  if Header.ColCount < List.Columns.Count+1 then exit;
  for i := 1 to List.Columns.Count do
    if (Header.Cells[i,1] <> '') and (BuildColumnFilter(List.Columns[i-1].Field,Header.Cells[i,1]) <> '') then
      begin
        Result := Result+' AND ('+BuildColumnFilter(List.Columns[i-1].Field,Header.Cells[i,1])+')';
      end;
  if copy(Result,0,5) = ' AND ' then Result := copy(Result,6,length(Result));
end;
end.

