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
Created 14.12.2013
*******************************************************************************}
unit htmltowiki;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,Utils;

function HTML2WikiText(input: string;RemoveLinkOffset : string = ''): string;
function StripHTML(input : string) : string;

type
  TIImageConvertFunc = procedure(Image : string;var OutFile : string) of Object;
var
  OnConvertImage : TIImageConvertFunc;

implementation

procedure RemoveTag(var aOut,bOut : string;aTag : string;AllowShortenClose : Boolean = False;IgnoreWhen : string = '');
var
  ShortCloser: Boolean;
  aTagOpen: Integer;
  atmp : string = '';
begin
  while pos('<'+aTag,lowercase(aout))>0 do
    begin
      bOut := bOut+copy(aout,0,pos('<'+aTag,lowercase(aout))-1);
      aOut := copy(aOut,pos('<'+aTag,lowercase(aout))+1+length(aTag),length(aOut));
      aTagOpen := 1;
      ShortCloser:=False;
      while (aTagOpen>0) and (length(aOut)>0) do
        begin
          if copy(aOut,0,1)='<' then inc(aTagOpen);
          if copy(aOut,0,2)='/>' then
            ShortCloser := True;
          if copy(aOut,0,1)='>' then dec(aTagOpen);
          atmp := atmp+copy(aOut,0,1);
          aOut := copy(aOut,2,length(aOut));
        end;
      if (IgnoreWhen<>'') and (pos(IgnoreWhen,atmp)>0) then
        bout := bout+atmp;
      if not ShortCloser then
        begin
          if (pos('</'+aTag+'>',lowercase(aout)) >= pos('<',aout)) or (not AllowShortenClose) then
            begin
              atmp := copy(aOut,0,pos('</'+aTag+'>',lowercase(aout))+3+length(aTag));
              if (IgnoreWhen<>'') and (pos(IgnoreWhen,atmp)>0) then
                bout := bout+atmp;
              aOut := copy(aOut,pos('</'+aTag+'>',lowercase(aout))+3+length(aTag),length(aOut))
            end
          else
            aOut := copy(aOut,pos('<',aout),length(aOut));
        end;
    end;
  aOut := bOut+aOut;
  bOut := '';
end;

function HTML2WikiText(input: string; RemoveLinkOffset: string): string;
var
  aOut: String;
  bOut: String;
  procedure RemoveComments;
  var
    ShortCloser: Boolean;
    aTagOpen: Integer;
  begin
    while pos('<!--',aout)>0 do
      begin
        bOut := bOut+copy(aout,0,pos('<!--',aout)-1);
        aOut := copy(aOut,pos('<!--',aout)+4,length(aOut));
        aOut := copy(aOut,pos('-->',aout)+3,length(aOut));
      end;
    aOut := bOut+aOut;
    bOut := '';
  end;
  procedure RemoveProperty(aProp : string);
  var
    ShortCloser: Boolean;
    aTagOpen: Integer;
  begin
    while pos('<',aout)>0 do
      begin
        bOut := bOut+copy(aout,0,pos('<',aout));
        aOut := copy(aOut,pos('<',aout)+1,length(aOut));
        aTagOpen := 1;
        while (aTagOpen>0) and (length(aOut)>0) do
          begin
            if copy(aOut,0,1)='<' then inc(aTagOpen);
            if copy(aOut,0,1)='>' then dec(aTagOpen);
            if lowercase(copy(aOut,0,length(aProp))) = lowercase(aProp) then
              begin
                aOut := copy(aOut,length(aProp)+1,length(aOut));
                while copy(aOut,0,1)=' ' do aOut := copy(aOut,2,length(aOut));
                if copy(aOut,0,1)='=' then
                  aOut := copy(aOut,2,length(aOut));
                if copy(aOut,0,1)='"' then
                  aOut := copy(aOut,2,length(aOut));
                while (copy(aOut,0,1)<>'"') and (copy(aOut,0,1)<>'>') do
                  aOut := copy(aOut,2,length(aOut));
                if copy(aOut,0,1)='"' then
                  aOut := copy(aOut,2,length(aOut));
              end
            else
              begin
                bOut := bOut+copy(aOut,0,1);
                aOut := copy(aOut,2,length(aOut));
              end;
          end;
      end;
    aOut := bOut+aOut;
    bOut := '';
  end;
  procedure ConvertLinks;
  var
    tmp: String;
  begin
    while pos('<a',lowercase(aout))>0 do
      begin
        bOut := bOut+copy(aout,0,pos('<a',lowercase(aout))-1);
        aOut := copy(aOut,pos('<a',lowercase(aout))+2,length(aOut));
        tmp := copy(aOut,0,pos('/a>',lowercase(aOut)));
        aOut := copy(aOut,pos('/a>',lowercase(aout))+3,length(aOut));
        tmp := copy(tmp,pos('href',lowercase(tmp))+4,length(tmp));
        tmp := copy(tmp,pos('=',lowercase(tmp))+1,length(tmp));
        if pos('"',tmp)=1 then
          begin
            tmp := copy(tmp,2,length(tmp));
            bOut := bOut+'['+copy(tmp,0,pos('"',tmp)-1)
          end
        else if pos('''',tmp)=1 then
          begin
            tmp := copy(tmp,2,length(tmp));
            bOut := bOut+'['+copy(tmp,0,pos('''',tmp)-1)
          end
        else
          bOut := bOut+'['+copy(tmp,0,pos('>',tmp)-1);
        tmp := copy(tmp,pos('>',lowercase(tmp))+1,length(tmp));
        tmp := copy(tmp,0,pos('<',lowercase(tmp))-1);
        if trim(tmp) <> '' then
          bOut := bOut+' '+trim(tmp);
        bOut := bOut+']';
      end;
    aOut := bOut+aOut;
    bOut := '';
  end;
  procedure ConvertImages;
  var
    tmp: String;
    aImg: String;
  begin
    while pos('<img',lowercase(aout))>0 do
      begin
        bOut := bOut+copy(aout,0,pos('<img',lowercase(aout))-1);
        aOut := copy(aOut,pos('<img',lowercase(aout))+4,length(aOut));
        tmp := copy(aOut,0,pos('>',lowercase(aOut)));
        aOut := copy(aOut,pos('>',lowercase(aout))+1,length(aOut));
        tmp := copy(tmp,pos('src',lowercase(tmp))+3,length(tmp));
        tmp := copy(tmp,pos('=',lowercase(tmp))+1,length(tmp));
        if pos('"',tmp)=1 then
          begin
            tmp := copy(tmp,2,length(tmp));
            aImg := copy(tmp,0,pos('"',tmp)-1);
            if Assigned(OnConvertImage) then
              OnConvertImage(aImg,aImg);
          end
        else if pos('''',tmp)=1 then
          begin
            tmp := copy(tmp,2,length(tmp));
            aImg := copy(tmp,0,pos('''',tmp)-1);
            if Assigned(OnConvertImage) then
              OnConvertImage(aImg,aImg);
          end
        else
          begin
            aImg := copy(tmp,0,pos('>',tmp)-1);
            if Assigned(OnConvertImage) then
              OnConvertImage(aImg,aImg);
          end;
        if trim(aImg)<>'' then
          begin
            bOut := bOut+'[[Image:'+aImg;
            bOut := bOut+']]';
          end;
      end;
    aOut := bOut+aOut;
    bOut := '';
  end;
  procedure ConvertBr;
  var
    tmp: String;
  begin
    while pos('<br',lowercase(aout))>0 do
      begin
        bOut := bOut+copy(aout,0,pos('<br',lowercase(aout))-1);
        aOut := copy(aOut,pos('<br',lowercase(aout))+3,length(aOut));
        aOut := copy(aOut,pos('>',lowercase(aout))+1,length(aOut));
        bOut := bOut+#10;
      end;
    aOut := bOut+aOut;
    bOut := '';
  end;
  procedure ConvertTag(aTag : string;aRepl : string);
  var
    tmp: String;
  begin
    while pos('<'+aTag,lowercase(aout))>0 do
      begin
        bOut := bOut+copy(aout,0,pos('<'+aTag,lowercase(aout))-1);
        aOut := copy(aOut,pos('<'+aTag,lowercase(aout))+1+length(aTag),length(aOut));
        if  (copy(aOut,pos('<'+aTag,lowercase(aout))+1,1)<>' ')
        and (copy(aOut,pos('<'+aTag,lowercase(aout))+1,1)<>'>') then
          begin
            bOut := bOut+'<'+aTag;
            Continue;
          end;
        tmp := copy(aOut,0,pos('/'+aTag+'>',lowercase(aOut))-2);
        if copy(tmp,0,1)='>' then tmp := copy(tmp,2,length(tmp));
        aOut := copy(aOut,pos('/'+aTag+'>',lowercase(aout))+2+length(aTag),length(aOut));
        bOut := bOut+aRepl+tmp+aRepl;
      end;
    aOut := bOut+aOut;
    bOut := '';
  end;
begin
  aOut := copy(input,pos('<body>',lowercase(input)),length(input));
  aOut := StringReplace(aOut,'</body>','',[rfReplaceAll]);
  aOut := StringReplace(aOut,'<body>','',[rfReplaceAll]);
  aOut := StringReplace(aOut,'</html>','',[rfReplaceAll]);
  bOut := '';
  RemoveComments;
  RemoveTag(aOut,bOut,'script');
  RemoveTag(aOut,bOut,'style');
  ConvertImages;
  ConvertLinks;
  ConvertBr;
  ConvertTag('b','''''''');
  ConvertTag('strong','''''''');
  ConvertTag('i','''''');
  ConvertTag('em','''''');
  Result := StripHTML(aOut);
  Result := Utils.HTMLDecode(Result);
end;

function StripHTML(input: string): string;
var
  aOut: String;
  bOut: String;
  TagOpen: Integer;
begin
  aOut := StringReplace(input,'<<','<',[rfReplaceAll]);
  aOut := StringReplace(aOut,'</div>','</div>'+#10,[rfReplaceAll]);
  aOut := StringReplace(aOut,'<br>',#10,[rfReplaceAll]);
  bOut := '';
  RemoveTag(aOut,bOut,'script');
  RemoveTag(aOut,bOut,'style');
  TagOpen := 0;
  while length(aOut)>0 do
    begin
      if copy(aOut,0,1)='<' then
        begin
          aOut := copy(aOut,2,length(aOut));
          TagOpen:=1;
          while (TagOpen>0) and (length(aOut)>0) do
            begin
              if copy(aOut,0,1)='<' then inc(TagOpen);
              if copy(aOut,0,1)='>' then dec(TagOpen);
              aOut := copy(aOut,2,length(aOut));
            end;
        end
      else
        begin
          bOut := bOut+copy(aOut,0,1);
          aOut := copy(aOut,2,length(aOut));
        end;
    end;
  Result := HTMLDecode(bOut);
  //Result:=ConvertEncoding(Result,GuessEncoding(Result),EncodingUTF8);
end;

end.

