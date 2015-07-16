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
Created 16.07.2015
*******************************************************************************}
program pextracttext;

uses laz_fpspreadsheet, Classes, fpolestorage, poffice, general_nogui,
  uminiconvencoding, sysutils, Utils,uOODocument;

function StripUnwantedChar(Text: string):string;
var
  Allowed: Set of Char;
  i, LeftOvers: Integer;
begin
  Allowed := [' ', '0'..'9', 'a'..'z', 'A'..'Z', '~'..')', '-', '.', '\', ':', '`', '/', '<', ',', '>', ';', '{', '}',#13,#9];

  SetLength(Result, Length(Text));
  LeftOvers := 1;
  for i := 1 to Length(Text) do begin
    if Text[i] in Allowed then begin
      Result[LeftOvers]:= Text[i];
      Inc(LeftOvers);
    end
  end;
  SetLength(Result, LeftOvers-1);
end;
function GetWordText(aFileName: string; var aText: string
  ): Boolean;
var
  MemStream: TMemoryStream;
  OLEStorage: TOLEStorage;
  OLEDocument : TOLEDocument;
  aStringStream: TStringStream;
  aContent : string;
  aContent2: String;
begin
  MemStream := TMemoryStream.Create;
  OLEStorage := TOLEStorage.Create;
  try
    // Only one stream is necessary for any number of worksheets
    OLEDocument.Stream := MemStream;
    OLEStorage.ReadOLEFile(aFileName, OLEDocument,'WordDocument');
    if MemStream.Seek($800,soFromBeginning) = $800 then
      begin
        Setlength(aContent,MemStream.Size-$800);
        MemStream.Read(aContent[1],MemStream.Size-$800);
        aContent2 := ConvertEncoding(aContent,EncodingUCS2LE,EncodingUTF8);
        aText:=StripUnwantedChar(aContent2);
      end;
    DeleteFile(UniToSys(aFileName));
  finally
    OLEStorage.Free;
  end;
end;

var
  aFile: String;
  aText: TStringList;
  bText : string;
  i: Integer;
  aDoc: TODFDocument;
begin
  aFile := UniToSys(ParamStr(Paramcount));
  if FileExists(aFile) then
    begin
      aText := TStringList.Create;
      case lowercase(ExtractFileExt(aFile)) of
      '.doc':
          begin
            GetWordText(aFile,bText);
            aText.Text:=bText;
          end;
      '.odt':
          begin
            aDoc := TODFDocument.Create(aFile);
            bText := aDoc.AsString;
            aDoc.Free;
            aText.Text:=bText;
          end;
      else
        begin
          //aText.LoadFromFile(aFile);
          //aText.Text:=StripUnwantedChar(aText.Text);
        end;
      end;
      if aText.Count>0 then
        for i := 0 to aText.Count-1 do
          writeln(aText[i]);
    end
  else writeln('no File "'+aFile+'" found');
end.

