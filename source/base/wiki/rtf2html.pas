unit rtf2html;

(*
--------------------------------------------------------------------------------

Revision history:
 Nb.   Date:         Author   What was done.
 003   ?             ?        ?
 002   21-aug-1997   TSE      Minor (very minor) cleanup before release
 001   20-aug-1997   TSE      Unit created - RtfToHTML function
                              designed and written.

Author list:
 TSE	Troels Skovmand Eriksen		TSEriksen@cyberdude.com
                                        TSErikse@post8.tele.dk
 ?      ?                               ?

Supported features:
 rev. 001	Indents, Bullets, Left-, Centered & Rightjustified text,
                Text styles (bold, italics and underline),
                Fonts (face, size, color).
 rev. 002	- do -
 rev.   3	?

--------------------------------------------------------------------------------

 This unit and all procedures and functions herein is released as
freeware. Any components or units created using this unit or
portions hereoff must be released as freeware (This does not
cover applications - they may be shareware/commercial as needed).

 Part of the function RtfToHTML may be covered by some obscure
Microsoft copyright since it reads the RTF format - check it out
yourself, if you do something worthwhile.

 Please let this preface stay if you publish a changed/updated 
version of this unit and write all changes the "Revision history" 
section above. Who-Dun-it information should be inserted in the 
"Author list" and the "Supported features" section should be updated. 
This makes it easier to pass the blame :-) 

 I'm finished with this unit for now - but please email a copy of
any changes you make to me - Troels S Eriksen.

--------------------------------------------------------------------------------

 The following should be fixed - if anybody want to do it?

   * Should be rewritten into a conversion class - could be tricky, since
     it seems like a stream only contains 4 Kb at a time ...
   * Code should be cleaned up - this below is not that fast ...
   * The indents (\li###) should be translated to <BLOCKQUOTE> or
     just a <UL> with no <LI> elements.
   * The hanging paragraphs should be translated to definitionlists ( the
     <DL COMPACT> <DT> term <DD> definition </DL> structure )
   * Tabs (\tab) should be fixed somehow ( heck, I DO want
     a <TAB> code ! )
   * Embedded objects / pictures should be converted to .gif's
     - I know it's possible
   * Some nice way to handle links ( the way .rtf-sources for
     helpfiles do ? )
   * A even more nice way of handling tables - could fix the
     indent / tab-problems as well

--------------------------------------------------------------------------------

 The idea and logic behind this weirdo function :

 Well, the idea was to write a pascal RTF-2-HTML converter which
doesn't just do some "search and replace" - but actually converts
the dammed stuff.

 Since there's a difference between HTML and RTF in the
code-sequencing, I decided to try storing all paragraph and
textformatting information in two records (PARFMT and
TXTFMT) and only write the contents of these to the output
"stream" when needed.

 This first attempt is successfull - not highly, but it'll convert
the contents of a TRichEdit control and most other .rtf documents
to HTML and keep the general layout.

Enjoy it
Troels S Eriksen, Denmark

--------------------------------------------------------------------------------

*)
interface
 {$mode objfpc}{$H+}

 function RtfToHtml(const rtf:string):string;

implementation
uses
  Classes, SysUtils, Utils;

function RtfToHtml(const rtf:string):string;

type
  TState = record
    FntTbl : boolean;
    ColTbl : boolean;
    FntLst,
    ColLst : TStringList;
  end;

  TPARFMT = record
    Alignment : TAlignment;  { højre, venstre, centreret tekst }
    Bullets   : integer;	   { Skriv bulletliste   <UL>  = 1
                                     Skriv element       <LI>  = 2
                                     Skriv element slut  </LI> = 3
                                     Skriv liste slut    </UL> = 4 }
    Written   : boolean;	   { true hvis skrevet til streng }
  end;

  TTXTFMT = record
    ChangeF   : boolean;
    DefFont   : integer;
    Font      : integer;
    Fontsize  : integer;
    Color     : integer;
    Bold      : integer;
    Italics   : integer;
    Underline : integer;
    Written   : boolean;
  end;

var
  indx : integer;  // index i rtf-streng
  ParFmt : TParFmt;
  TxtFmt : TTxtFmt;
  State  : TState;

  Group  : integer;
  Col    : string[10];
  Fnt    : string[63];

  procedure WritePlain(c:string);
  begin
    Result := Result+c;
  end;

  procedure WriteChar(c:Char);
    var
      S : string;
    begin
      s:='';
      // First - get ready to write paragraph formatting
      With PARFMT do if not Written then begin
        // TextAttr's must be off before starting a new paragraph
{
       add "uses forms" to the implementation or interface statement,
       then call application.processmessages here - this would allow
       you to work the application interface will saving a large file.
}
        With TXTFMT do begin
          if bold>1 then begin
            s:=s+'</B>';
            if bold=3 then bold:=0;
          end;
          if italics>1 then begin
            s:=s+'</I>';
            if italics=3 then Italics:=0;
          end;
          if underline>1 then begin
            s:=s+'</U>';
            if underline=3 then Underline:=0;
          end;
        end;
        { Write either bulletlist or left-, center, rightjustified paragraph
          (doing it this way makes bulletlists leftjustified no matter what) }
        case Bullets of
          0 : case Alignment of
            taLeftJustify : s:=s+#13#10'<P>';
            taRightJustify: s:=s+#13#10'<P ALIGN=RIGHT>';
            taCenter      : s:=s+#13#10'<P ALIGN=CENTER>';
          end;
          1 : s:=s+#13#10'<UL>';
          2 : s:=s+#13#10'<LI>';
          3 : s:=s+'</LI>';
          4 : begin
            s:=s+#13#10'</UL>';
            Bullets:=0;
          end;
          5 : begin
            s:=s+'<BR>'#13#10#160#32#160#32#160;
            Bullets:=0;
          end;
        end;
        // If any textattr's was on before - they are re-enabled
        With TXTFMT do begin
          If Bold=2 then s:=s+'<B>';
          If Italics=2 then s:=s+'<I>';
          If Underline=2 then s:=s+'<U>';
        end;
        Written:=TRUE;
      end; { PARFMT }
      // Second - Write any textattr's
      With TXTFMT do if not written then begin
        // If font has changed - write it
        If changeF then begin
          {
          s:=s+'<FONT FACE="'+state.fntlst.strings[Font]+
               '" COLOR="'+state.collst.strings[Color]+
               '" SIZE="'+IntToStr(FontSize)+'">';
          }
          if state.collst.Count>Color then
            begin
              s:=s+'<FONT COLOR="'+state.collst.strings[Color]+
                   '" SIZE="'+IntToStr(FontSize)+'">';
            end
          else
            begin
             s:=s+'<FONT SIZE="'+IntToStr(FontSize)+'">';
            end;
          ChangeF:=FALSE;
        end;
        // If any textattr's should be written - do it
        case Bold of
          1 : begin
            s:=s+'<B>';
            bold:=2;
          end;
          3 : begin
            s:=s+'</B>';
            Bold:=0;
          end;
        end;
        case Italics of
          1 : begin
            s:=s+'<I>';
            Italics:=2;
          end;
          3 : begin
            s:=s+'</I>';
            Italics:=0;
          end;
        end;
        case Underline of
          1 : begin
            s:=s+'<U>';
            Underline:=2;
          end;
          3 : begin
            s:=s+'</U>';
            Underline:=0;
          end;
        end;
        Written:=TRUE;
      end;
      // At last - write the character it self
      case c of
        #0  : result:=result+s;          // Writes pending codes only
        #9  : result:=result+s+#9;       // Writes tab char
        '>' : result:=result+s+'&gt';    // Writes "greater than"
        '<' : result:=result+s+'&lt';    // Writes "less than"
        else  result:=result+s+c;        // Writes a character
      end;
    end; { WriteChar }
  function Resolve(c:char):integer;
  { Convert char to integer value - used to decode \'## to an ansi-value }
  begin
    case byte(c) of
      48..57 : Result:=byte(c)-48;
      65..70 : Result:=byte(c)-55;
      else     Result:=0;
    end;
  end; { resolve }
  function CollectCode(i:integer):integer;
  var
    Value,
    Keyword : string;
    a       : integer;
  begin
    KeyWord:='';
    // First - check if keyword is any "special" keyword or is a normal one ...
    case rtf[i+1] of
      '*' : begin    // Ignorre to end of group
        a:=group;
        repeat
          case rtf[i] of
            '{' : inc(group);
            '}' : dec(group);
          end;
          inc(i);
        until ((group+1)=a) or (i>length(rtf));
        result:=i-1;
      end;
      #39 : begin  // Decode hex value 
        WriteChar(char(resolve(upcase(rtf[i+2]))*16+resolve(upcase(rtf[i+3]))));
        Inc(i,3);
        result:=i;
      end;
      '\','{','}' : begin  // Return special character
        WriteChar(rtf[i+1]);
        inc(i);
        result:=i;
      end;
      else begin
        // First - get keyword ...
        repeat
          keyword:=keyword+rtf[i];
          inc(i);
        until (rtf[i] in ['&','{','\','}',' ',';','-','0'..'9',#13,#10]);
        // Second - get any value following ...
        Value  :='';
        if (keyword = '\') and (rtf[i]='&') then
          begin
            if (not TxtFmt.Written) or (not ParFmt.Written) then
              WriteChar(#0);
            inc(i,5);
            value := '$'+rtf[i]+rtf[i+1];
            WritePlain(AnsiToUtf8(chr(StrToInt(value))));
            inc(i,2);
          end
        else
          begin
            While (rtf[i] in ['a'..'z','-','0'..'9',#13,#10]) do begin
              value:=value+rtf[i];
              inc(i);
            end;
          end;
        if rtf[i]=' ' then inc(i);
        while (rtf[i] in ['{','}',';']) do inc(i);
        result:=i-1;
        { Check which keyword and what to do - NB: Test shows that using
          IF THEN ELSE .. is approx. 10% more efficient than calling EXIT }
        if keyword='\par' then with PARFMT do begin
          // New paragraph or bullet item
          if Bullets=2 then Bullets:=3;
          Written:=FALSE;
        end else if keyword='\f' then case state.fnttbl of
          true : begin                        // Make fontlist
            fnt:='';
            While (rtf[i]<>' ') do inc(i);      // Ignore fontfamily info etc
            inc(i);
            While (rtf[i]<>';') do begin        // Read font name
              Fnt:=Fnt+rtf[i];
              inc(i);
            end;
            dec(group);                       // Stop group
            result:=i+1;                      // Move one beyond group end
            State.FntLst.Add(Fnt);	      // Add fontname to fontlist
          end; { true }
          false: With TXTFMT do begin         // Use fontlist
            a:=StrToIntDef(value,0);
            if font<>a then begin	      // Change Textattr's to new font
              ChangeF:=TRUE;
              Written:=FALSE;
              FONT   :=a;
            end;
            with TXTFMT do begin                 // Zero textattr's
             If bold=2 then Bold:=3;
             If Italics=2 then Italics:=3;
             If Underline=2 then Underline:=3;
             if (bold=3) or (italics=3) or (underline=3) or (Color<>0) then begin
               color:=0;
               Written:=FALSE;
               WriteChar(#0);
             end;
            end;
          end; { false }
        end else if keyword='\plain' then
         with TXTFMT do begin                 // Zero textattr's
          If bold=2 then Bold:=3;
          If Italics=2 then Italics:=3;
          If Underline=2 then Underline:=3;
          if (bold=3) or (italics=3) or (underline=3) or (Color<>0) then begin
            color:=0;
            Written:=FALSE;
            WriteChar(#0);
          end;
        end else if keyword='\fs' then with TXTFMT do begin  // Change fontsize
          case StrToIntDef(value,11) div 2 of
             1.. 5 : a:=1;
             6.. 9 : a:=2;
            10..11 : a:=3;
            12..13 : a:=4;
            14..15 : a:=5;
            else     a:=6;
          end;
          if a<>Fontsize then begin
            Written:=False;
            Fontsize:=a;
            ChangeF:=TRUE;
          end;
        end else if keyword='\tab' then begin
          WriteChar(#9);
        end else if keyword='\ul' then with TXTFMT do begin  // Set underline
          Written:=FALSE;
          if underline=0 then Underline:=1
          else if Value='0' then
            underline := 3;
        end else if keyword='\b' then with TXTFMT do begin   // Set bold
          Written:=FALSE;
          if bold=0 then
            Bold:=1
          else if Value='0' then
            Bold := 3;
        end else if keyword='\i' then with TXTFMT do begin   // Set italics
          Written:=FALSE;
          if italics=0 then Italics:=1
          else if Value='0' then
            Italics := 3;
        end else if keyword='\cf' then with TXTFMT do begin  // Change fontcolor
          a:=StrToIntDef(value,0);
          If Color<>a then begin
            Written:=FALSE;
            ChangeF:=TRUE;
            Color:=a;
          end;
        end else if keyword='\qc' then begin     // Set paragraphformat (center)
          PARFMT.Alignment:=taCenter;
          PARFMT.Written:=FALSE;
        end else if keyword='\qr' then begin     // Set paragraphformat (right)
          PARFMT.Alignment:=taRightJustify;
          PARFMT.Written:=FALSE;
        end else if keyword='\pntext' then
         with PARFMT do begin                    // Start bullet list item
          Written   :=FALSE;
          Bullets   :=2;
          a:=group;
          repeat
            case rtf[i] of
              '{' : inc(group);
              '}' : dec(group);
            end;
            inc(i);
          until (group+1)=a;
          result:=i-1;
        end else if keyword='\fi' then with PARFMT do begin // Start bullet list
          Written   :=FALSE;
          Bullets   :=1;
          WriteChar(#0);
        end else if keyword='\pard' then
         with PARFMT do begin                // Stop paragraph / Bulletlist
          Alignment:=taLeftJustify;
          If Bullets>0 then
            Bullets:=4;
          Written:=FALSE;
        end else if keyword='\red' then begin
          col:='#'+IntToHex(StrToIntDef(value,255),2);   // Get Red color
        end else if keyword='\green' then begin
          col:=col+IntToHex(StrToIntDef(value,255),2);   // Get Green color
        end else if keyword='\blue' then begin
          col:=col+IntToHex(StrToIntDef(value,255),2);  // Get blue color
          State.ColLst.Add(col);                        // Add RGB in colorlist
        end else if keyword='\deff' then with TXTFMT do begin
          DefFont:=StrToIntDef(value,0);              // Default font
        end else if keyword='\fonttbl' then begin
          state.fnttbl:=true;                        // Create font-list
        end else if keyword='\colortbl' then begin
          state.fnttbl:=false;
          state.coltbl:=true;                        // Create color-list
        end else if keyword='\deflang' then begin
          state.fnttbl:=False;                       // Update is finished
          With PARFMT do begin                       // Setup paragraphformat
            Alignment:=taLeftJustify;
            Written:=false;
            Bullets:=0;
          end;
          With TXTFMT do begin                       // Setup font-format
            Font      :=DefFont;
            Fontsize  :=3;
            Color     :=0;
            Bold      :=0;
            Italics   :=0;
            Underline :=0;
            Written   :=false;
          end;
          state.coltbl:=True;                        // Update is finished
        end { last if then   }
      else begin
        state.fnttbl:=false;
      end;
      end;  { case else }
    end;
  end;  { collectcode }
  function CleanUp(s:string):string;
  // This could be done without, but - hey - it's nice
  var
    a : integer;
  begin
    with TXTFMT do begin                 // Zero textattr's
     If bold=2 then Bold:=3;
     If Italics=2 then Italics:=3;
     If Underline=2 then Underline:=3;
     if (bold=3) or (italics=3) or (underline=3) or (Color<>0) then begin
       color:=0;
       Written:=FALSE;
       WriteChar(#0);
     end;
    end;

    // Nice up any empty <P>aragraph statements
    While pos(#13#10'<P>'#13#10'<P',s)>0 do begin
      a:=pos(#13#10'<P>'#13#10'<P',s);
      system.delete(s,a,6);
      system.insert('</P>',s,a);
    end;
    Result := StringReplace(result,'#13#10</P>','</P>',[rfReplaceAll]);
    result:=s;
  end; { cleanup }
var
  crsr : integer;

begin
  FillChar(TxtFmt,sizeof(TxtFmt),0);
  FillChar(ParFmt,sizeof(ParFmt),0);
  Group:=0;
  try
    try
    State.FntLst:=TstringList.Create;    // Create fontlist
    State.ColLst:=TstringList.Create;    // Create colorlist
    indx:=0;
    result:='';
    if length(rtf)<1 then exit;
    repeat
      inc(indx);
      case rtf[indx] of
        #0..#31 : ;                      // Ascii ctrl-char - ignorre
        '{' : Inc(group);
        '}' :
          begin
            Dec(group);
            if state.fnttbl then state.fnttbl := False;
          end;
        '\' : indx:=collectcode(indx);   // Code found - the fun starts ...
        else begin
          WriteChar(rtf[indx]);         // Write char and any pending html-codes ...
          Inc(indx);                    // Speedwrite normal chars till next special one
          while (indx<length(rtf)) and
                not (rtf[indx] in ['{','}','\','<','>',#0..#31]) do begin
            result:=result+rtf[indx];
            inc(indx);
          end;
          dec(indx);
        end;

      end;
    until indx>=length(rtf);
    except
    end;
  finally
    result:=cleanup(result);		  // Return the HTML document
    State.FntLst.free;
    State.ColLst.free;
  end;
end;

end.
