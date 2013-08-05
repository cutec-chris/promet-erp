unit wikitohtml;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Utils, FileUtil,RegExpr;

function WikiText2HTML(input: string;LinkOffset : string = '';RemoveLinkOffset : string = '';IproChanges : Boolean = False): string;

type
  TImageConvertFunc = procedure(Image : string;var OutFile : string; var aLinkTags,aHref,aTags,aWidth : string) of Object;
  TWikiIncludeFunc = procedure(Inp : string;var Outp : string) of Object;
var
  OnConvertImage : TImageConvertFunc;
  OnWikiInclude : TWikiIncludeFunc;

implementation

function WikiText2HTML(input: string;LinkOffset : string;RemoveLinkOffset : string = '';IproChanges : Boolean = False): string;
var
  output : string;
  istr: String;
  ostr: String;
  open_uls: Integer;
  act_uls: Integer;
  i: LongInt;
  tstr: String;
  intd: Boolean;
  linkcontent: String;
  aLink: String;
  procedure DoReplace(var InStr,OutStr : string;ReplaceTag,NewTag : string;MustbeInOneLine : Boolean = False);
  var
    NewLine: String;
  begin
    while pos(ReplaceTag,instr) > 0 do
      begin
        NewLine := copy(instr,pos(ReplaceTag,instr)+length(ReplaceTag),length(instr));
        if MustBeInOneLine
        and ((pos(#10,NewLine) < pos(ReplaceTag,NewLine))) and (not (length(NewLine) = pos(ReplaceTag,NewLine)+length(ReplaceTag)-1)) then
          break;
        outstr := outstr+copy(instr,0,pos(ReplaceTag,instr)-1);
        instr := copy(instr,pos(replaceTag,instr)+length(ReplaceTag),length(instr));
        outstr := outstr+'<'+NewTag+'>'+copy(instr,0,pos(ReplaceTag,instr)-1)+'</'+NewTag+'>';
        instr := copy(instr,pos(ReplaceTag,instr)+length(ReplaceTag),length(instr));
      end;
    outstr := outstr+instr;
    instr := outstr;
    outstr := '';
  end;
  procedure ReplaceImages(ImageTagName : string);
  var
    ImageFile: String;
    tmp : string;
    aLinkTags : string = '';
    aHref : string = '';
    aTags : string = '';
    aWidth: String;
    NewAlt: String;
  begin
      while pos('[['+ImageTagName+':',istr) > 0 do
        begin
          aWidth := '';
          ostr := ostr+copy(istr,0,pos('[['+ImageTagName+':',istr)-1);
          istr := copy(istr,pos('[['+ImageTagName+':',istr)+length(ImageTagname)+3,length(istr));
          if (pos('|',istr) > 0) and (pos('|',istr) < pos(']]',istr)) then
            begin
              ImageFile := UTF8ToSys(HTMLDecode(copy(istr,0,pos('|',istr)-1)));
              ostr := ostr+'<a~LINKTAGS href="~HREF"><img~ATAGS src="~IMAGEFILE"';
              while (pos('|',istr) > 0) and (pos('|',istr) < pos(']]',istr)) do
                begin
                  tmp := copy(istr,0,pos('|',istr)-1);
                  istr := copy(istr,pos('|',istr)+1,length(istr));
                  if (tmp = 'right')
                  or (tmp = 'left')
                  then
                    begin
                      if IproChanges then
                        ostr := ostr+ ' align="'+tmp+'"'
                      else
                        ostr := ostr+ ' style="float:'+tmp+';"';
                    end
                  else if pos('px',tmp) > 0 then
                    begin
                      ostr := ostr+' width="'+StringReplace(tmp,'px','',[rfIgnoreCase])+'"';
                      aWidth := StringReplace(tmp,'px','',[rfIgnoreCase]);
                    end
                  else ostr := ostr+' alt="'+tmp+'"';
                end;
              tmp := copy(istr,0,pos(']]',istr)-1);
              if (tmp = 'right')
              or (tmp = 'left')
              then
                begin
                  if IproChanges then
                    ostr := ostr+ ' align="'+tmp+'"'
                  else
                    ostr := ostr+ ' style="float:'+tmp+';"';
                end
              else if pos('px',tmp) > 0 then
                begin
                  ostr := ostr+' width="'+StringReplace(tmp,'px','',[rfIgnoreCase])+'"';
                  aWidth := StringReplace(tmp,'px','',[rfIgnoreCase]);
                end
              else ostr := ostr+' alt="'+tmp+'"';
              istr := copy(istr,pos(']]',istr)+2,length(istr));
              if Assigned(OnConvertImage) then
                OnConvertImage(ImageFile,ImageFile,aLinkTags,aHref,aTags,aWidth);
              if aHref = '' then
                begin
                  ostr := StringReplace(ostr,'<a~LINKTAGS href="~HREF">','',[]);
                  ostr := ostr+'>';
                end
              else
                begin
                  ostr := StringReplace(ostr,'~LINKTAGS',aLinkTags,[]);
                  ostr := StringReplace(ostr,'~HREF',aHref,[]);
                  ostr := ostr+'></a>';
                end;
              ostr := StringReplace(ostr,'~IMAGEFILE',ImageFile,[]);
              ostr := StringReplace(ostr,'~ATAGS',aTags,[]);
            end
          else
            begin
              if Assigned(OnConvertImage) then
                OnConvertImage(copy(istr,0,pos(']]',istr)-1),ImageFile,aLinkTags,aHref,aTags,aWidth)
              else ImageFile := UTF8ToSys(HTMLDecode(copy(istr,0,pos(']]',istr)-1)));
              NewAlt := copy(istr,0,pos(']]',istr)-1);
              NewAlt := copy(NewAlt,0,rpos('.',NewAlt)-1);
              if aHRef = '' then
                ostr := ostr+'<img'+aTags+' src="'+ImageFile+'" alt="'+NewAlt+'">'
              else
                ostr := ostr+'<a'+aLinkTags+' href="'+aHref+'"><img'+aTags+' src="'+ImageFile+'" alt="'+NewAlt+'"></a>';
              istr := copy(istr,pos(']]',istr)+2,length(istr));
            end;
        end;
  end;
  procedure ReplaceDownload(ImageTagName : string);
  var
    ImageFile: String;
    tmp : string;
    aLinkTags : string = '';
    aHref : string = '';
    aTags : string = '';
  begin
      while pos('[['+ImageTagName+':',istr) > 0 do
        begin
          ostr := ostr+copy(istr,0,pos('[['+ImageTagName+':',istr)-1);
          istr := copy(istr,pos('[['+ImageTagName+':',istr)+length(ImageTagname)+3,length(istr));
          if (pos('|',istr) > 0) and (pos('|',istr) < pos(']]',istr)) then
            begin
              ImageFile := UTF8ToSys(HTMLDecode(copy(istr,0,pos('|',istr)-1)));
              ostr := ostr+'<a href="'+'/downloads/'+ImageFile+'"';
              while (pos('|',istr) > 0) and (pos('|',istr) < pos(']]',istr)) do
                begin
                  tmp := copy(istr,0,pos('|',istr)-1);
                  istr := copy(istr,pos('|',istr)+1,length(istr));
                end;
              tmp := copy(istr,0,pos(']]',istr)-1);
              istr := copy(istr,pos(']]',istr)+2,length(istr));
              ostr := ostr+'>'+tmp+'</a>';
            end
          else
            begin
              ImageFile := UTF8ToSys(HTMLDecode(copy(istr,0,pos(']]',istr)-1)));
              ostr := ostr+'<a'+aLinkTags+' href="'+'/downloads/'+ImageFile+'">'+ImageFile+'</a>';
              istr := copy(istr,pos(']]',istr)+2,length(istr));
            end;
        end;
  end;
  procedure ReplaceIncludes(ImageTagName : string);
  var
    ImageFile: String;
    tmp : string = '';
    aLinkTags : string = '';
    aHref : string = '';
    aTags : string = '';
  begin
      while pos('[['+ImageTagName+':',istr) > 0 do
        begin
          ostr := ostr+copy(istr,0,pos('[['+ImageTagName+':',istr)-1);
          istr := copy(istr,pos('[['+ImageTagName+':',istr)+length(ImageTagname)+3,length(istr));
          ImageFile := UTF8ToSys(HTMLDecode(copy(istr,0,pos(']]',istr)-1)));
          istr := copy(istr,pos(']]',istr)+2,length(istr));
          if Assigned(OnWikiInclude) then
            begin
              tmp := '';
              OnWikiInclude(ImageFile,tmp);
              ostr+=tmp;
            end;
        end;
  end;
  procedure ReplaceLinks(LinkTagName : string);
  begin
    while pos('['+LinkTagName,lowercase(istr)) > 0 do
      begin
        ostr := ostr+copy(istr,0,pos('['+LinkTagName,lowercase(istr))-1);
        istr := copy(istr,pos('['+LinkTagName,lowercase(istr)),length(istr));
        if (pos(' ',istr) > 0) and (pos(' ',istr) < pos(']',lowercase(istr))) then
          begin
            ostr := ostr+'<a href="'+StringReplace(copy(istr,2,pos(' ',istr)-2),LinkTagName+'./','',[rfReplaceAll])+'" target="_BLANK">';
            istr := copy(istr,pos(' ',istr)+1,length(istr));
            ostr := ostr+copy(istr,0,pos(']',lowercase(istr))-1)+'</a>';
            istr := copy(istr,pos(']',lowercase(istr))+1,length(istr));
          end
        else
          begin
            ostr := ostr+'<a href="'+StringReplace(copy(istr,2,pos(']',lowercase(istr))-2),LinkTagName+'./','',[rfReplaceAll])+'" target="_BLANK">'+StringReplace(copy(istr,2,pos(']',lowercase(istr))-2),LinkTagName+'./','',[rfReplaceAll])+'</a>';
            istr := copy(istr,pos(']',lowercase(istr))+1,length(istr));
          end;
      end;
  end;
begin
  istr := SysToUTF8(input);
  ostr := '';
  open_uls := 0;
  act_uls := 0;
  //all newlines to \n
  ostr := StringReplace(ostr,#13#10,#10,[rfReplaceAll]);
  ostr := StringReplace(ostr,#10#13,#10,[rfReplaceAll]);
  //Remove NOTOC
  istr := StringReplace(istr,'__NOTOC__','',[rfReplaceAll]);
  //Remove TOC
  istr := StringReplace(istr,'__TOC__','',[rfReplaceAll]);
  //Remove Templates
  while pos('{{',istr) > 0 do
    begin
      ostr := ostr+copy(istr,0,pos('{{',istr)-1);
      istr := copy(istr,pos('{{',istr)+2,length(istr));

      istr := copy(istr,pos('}}',istr)+2,length(istr));
    end;
  ostr := ostr+istr;
  istr := ostr;
  ostr := '';
  //Replace Umlauts
  istr := StringReplace(istr, 'ä', '&auml;', [rfreplaceall]);
  istr := StringReplace(istr, 'ö', '&ouml;', [rfreplaceall]);
  istr := StringReplace(istr, 'ü', '&uuml;', [rfreplaceall]);
  istr := StringReplace(istr, 'Ä', '&Auml;', [rfreplaceall]);
  istr := StringReplace(istr, 'Ö', '&Ouml;', [rfreplaceall]);
  istr := StringReplace(istr, 'Ü', '&Uuml;', [rfreplaceall]);
  istr := StringReplace(istr, 'ß', '&szlig;', [rfreplaceall]);
  //Replace Lists
  while pos('*',istr) > 0 do
    begin
      ostr := ostr+copy(istr,0,pos('*',istr)-1);
      istr := copy(istr,pos('*',istr)+1,length(istr));
      inc(act_uls);
      while (length(istr) > 0) and (istr[1] = '*') do
        begin
          inc(act_uls);
          istr := copy(istr,2,length(istr));
        end;
      if open_uls < act_uls then
        begin
          for i := open_uls to act_uls-1 do
            ostr := ostr+'<ul>';
        end
      else
        begin
          for i := act_uls to open_uls-1 do
            ostr := ostr+'</ul>';
        end;
      open_uls := act_uls;
      act_uls := 0;
      ostr := ostr+'<li>';
      if pos(#10,istr) > 0 then
        begin
          ostr := ostr+copy(istr,0,pos(#10,istr)-1);
          istr := copy(istr,pos(#10,istr)+1,length(istr));
        end
      else
        begin
          ostr := ostr+istr;
          istr := '';
        end;
      ostr := ostr+'</li>';
      if (length(istr) > 0) and (istr[1] <> '*') then
        begin
          for i := 0 to open_uls-1 do
            ostr := ostr+'</ul>';
          open_uls := 0;
        end;
    end;
  ostr := ostr+istr;
  istr := ostr;
  ostr := '';
  open_uls := 0;
  act_uls := 0;
  //Replace Numerated Lists
  while pos('#',istr) > 0 do
    begin
      ostr := ostr+copy(istr,0,pos('#',istr)-1);
      istr := copy(istr,pos('#',istr)+1,length(istr));
      inc(act_uls);
      while istr[1] = '#' do
        begin
          inc(act_uls);
          istr := copy(istr,2,length(istr));
        end;
      if open_uls < act_uls then
        begin
          for i := open_uls to act_uls-1 do
            ostr := ostr+'<ol>';
        end
      else
        begin
          for i := act_uls to open_uls-1 do
            ostr := ostr+'</ol>';
        end;
      open_uls := act_uls;
      act_uls := 0;
      ostr := ostr+'<li>';
      if pos(#10,istr) > 0 then
        begin
          ostr := ostr+copy(istr,0,pos(#10,istr)-1);
          istr := copy(istr,pos(#10,istr)+1,length(istr));
        end
      else
        begin
          ostr := ostr+istr;
          istr := '';
        end;
      ostr := ostr+'</li>';
      if (length(istr) > 0) and (istr[1] <> '#') then
        begin
          for i := 0 to open_uls-1 do
            ostr := ostr+'</ol>';
          open_uls := 0;
        end;
    end;
  ostr := ostr+istr;
  istr := ostr;
  ostr := '';
  //Replace Tables
  while pos('{|',istr) > 0 do
    begin
      ostr := ostr+copy(istr,0,pos('{|',istr)-1);
      istr := copy(istr,pos('{|',istr)+2,length(istr));
      //remove also content behind {|
      istr := copy(istr,pos(#10,istr)-1,length(istr));
      tstr := copy(istr,0,pos(#10+'|}',istr)-1);
      istr := copy(istr,pos(#10+'|}',istr)+3,length(istr));
      ostr := ostr+'<table><tr>';
      tstr := StringReplace(tstr,'|-','</tr><tr>',[rfReplaceAll]);
      intd := False;
      while length(tstr) > 2 do
        begin
          if ((tstr[1] = #10) and (tstr[2] = '|'))
          or ((tstr[1] = #10) and (tstr[2] = '!')) then
            begin
              if inTD then
                ostr := ostr+'</td>'
              else
                ostr := ostr+'<td>';
              inTD := not inTD;
              tstr := copy(tstr,3,length(tstr));
            end
          else if ((tstr[1] = '!') and (tstr[2] = '!'))
               or ((tstr[1] = '|') and (tstr[2] = '|')) then
            begin
              if inTD then
                begin
                  ostr := ostr+'</td><td>'
                end
              else //Schould never happen
                begin
                  ostr := ostr+'<td>';
                  inTD := True;
                end;
              tstr := copy(tstr,3,length(tstr));
            end
          else
            begin
              if (tstr[1] = #10) and InTD then
                begin
                  ostr := ostr+'</td>';
                  InTD := False;
                end
              else
                ostr := ostr+tstr[1];
              tstr := copy(tstr,2,length(tstr));
            end;
        end;
      ostr := ostr+tstr+'</tr></table>';
    end;
  ostr := ostr+istr;
  istr := ostr;
  ostr := '';
  //Replace Images
  ReplaceImages('Bild');
  ostr := ostr+istr;
  istr := ostr;
  ostr := '';
  ReplaceImages('Image');
  ostr := ostr+istr;
  istr := ostr;
  ostr := '';
  ReplaceDownload('Download');
  ostr := ostr+istr;
  istr := ostr;
  ostr := '';
  ReplaceIncludes('Include');
  ostr := ostr+istr;
  istr := ostr;
  ostr := '';
  //Replace Links
  while pos('[[',istr) > 0 do
    begin
      ostr := ostr+copy(istr,0,pos('[[',istr)-1);
      istr := copy(istr,pos('[[',istr)+2,length(istr));
      if (pos('|',istr) > 0) and (pos('|',istr) < pos(']]',istr)) then
        begin
          linkcontent := copy(istr,0,pos('|',istr)-1);
              aLink := linkoffset+UTF8ToSys(HTMLDecode(linkcontent));
              if copy(aLink,0,length(RemoveLinkOffset)) = RemoveLinkOffset then
                aLink := copy(aLink,length(RemoveLinkOffset),length(aLink));
              if pos('::',linkcontent) > 0 then
                ostr := ostr+'<a href="'+aLink+'">'
              else
                ostr := ostr+'<a href="'+aLink+'">';
              istr := copy(istr,pos('|',istr)+1,length(istr));
              ostr := ostr+copy(istr,0,pos(']]',istr)-1)+'</a>';
              istr := copy(istr,pos(']]',istr)+2,length(istr));
        end
      else
        begin
          linkcontent := copy(istr,0,pos(']]',istr)-1);
              aLink := linkoffset+lowercase(linkcontent);
              if copy(aLink,0,length(RemoveLinkOffset)) = RemoveLinkOffset then
                aLink := copy(aLink,length(RemoveLinkOffset),length(aLink));
              if pos('::',linkcontent) > 0 then
                ostr := ostr+'<a href="'+aLink+'">'
              else
                ostr := ostr+'<a href="'+aLink+'">'+copy(istr,0,pos(']]',istr)-1)+'</a>';
              istr := copy(istr,pos(']]',istr)+2,length(istr));
        end;
    end;
  ostr := ostr+istr;
  istr := ostr;
  ostr := '';
  //Replace extern Links
  ReplaceLinks('http://');
  ostr := ostr+istr;
  istr := ostr;
  ostr := '';
  ReplaceLinks('https://');
  ostr := ostr+istr;
  istr := ostr;
  ostr := '';
  ReplaceLinks('mailto://');
  ostr := ostr+istr;
  istr := ostr;
  ostr := '';
  //Replace Bold Text
  istr := ReplaceRegExpr('''''''(.*?)''''''',istr,'<b>$1</b>',True);
  //Replace Italic Text
  istr := ReplaceRegExpr('''''(.*?)''''',istr,'<i>$1</i>',True);
  if IproChanges then
    begin
      istr := ReplaceRegExpr('======(.*?)======',istr,'<h5>$1</h5>',True);
      istr := ReplaceRegExpr('=====(.*?)=====',istr,'<h5>$1</h5>',True);
      istr := ReplaceRegExpr('====(.*?)====',istr,'<h4>$1</h4>',True);
      istr := ReplaceRegExpr('===(.*?)===',istr,'<h3>$1</h3>',True);
      istr := ReplaceRegExpr('==(.*?)==',istr,'<h2>$1</h2>',True);
      //istr := ReplaceRegExpr('\n=(.*?)=',istr,'<h1>$1</h1>',True);
    end
  else
    begin
      istr := ReplaceRegExpr('======(.*?)======',istr,'<h5>$1</h5>',True);
      istr := ReplaceRegExpr('=====(.*?)=====',istr,'<h4>$1</h4>',True);
      istr := ReplaceRegExpr('====(.*?)====',istr,'<h3>$1</h3>',True);
      istr := ReplaceRegExpr('===(.*?)===',istr,'<h2>$1</h2>',True);
      istr := ReplaceRegExpr('==(.*?)==',istr,'<h1>$1</h1>',True);
      //istr := ReplaceRegExpr('\n=(.*?)=',istr,'<h1>$1</h1>',True);
    end;
  //Process unformated stuff
  while pos(#10+' ',istr) > 0 do
    begin
      //Replace Line breaks in text bevore pre
      ostr := ostr+StringReplace(StringReplace(copy(istr,0,pos(#10+' ',istr)-1),#10#10,'<br><br>',[rfReplaceAll]),#10,'',[rfReplaceAll]);
      istr := copy(istr,pos(#10+' ',istr)+2,length(istr));
      ostr := ostr+'<pre>';
      while (pos(#10+' ',istr) > 0) do
        begin
          ostr := ostr+copy(istr,0,pos(#10,istr));
          istr := copy(istr,pos(#10,istr)+1,length(istr));
          if (length(istr) > 0) and (istr[1] <> ' ') then
            break
          else
            istr := copy(istr,2,length(istr));
        end;
      ostr := ostr+'</pre>';
    end;
  ostr := ostr+StringReplace(istr,#10#10,'<br><br>',[rfReplaceAll]);
  ostr := StringReplace(ostr,#13#10,'<br>',[rfReplaceAll]);
  ostr := StringReplace(ostr,#10#13,'<br>',[rfReplaceAll]);
  //Remove <br> after <h*>
  if IproChanges then
    begin
      ostr := StringReplace(ostr,'</h1><br>','</h1>',[rfReplaceAll]);
      ostr := StringReplace(ostr,'</h2><br>','</h2>',[rfReplaceAll]);
      ostr := StringReplace(ostr,'</h3><br>','</h3>',[rfReplaceAll]);
      ostr := StringReplace(ostr,'</h4><br>','</h4>',[rfReplaceAll]);
      ostr := StringReplace(ostr,'</h5><br>','</h5>',[rfReplaceAll]);
      ostr := StringReplace(ostr,'</h6><br>','</h6>',[rfReplaceAll]);
    end;

  Result := ostr;
end;


end.
