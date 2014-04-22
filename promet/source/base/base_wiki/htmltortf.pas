unit htmltortf;

{$mode objfpc}

interface

uses
  Classes, SysUtils, RichMemo, Graphics, Utils, LClIntf;

procedure HTMLtoRTF(html: string; var rtf: TRichMemo);
function RTFtoHTML(rtf: TRichMemo;Title : string) : string;

implementation

procedure HTMLtoRTF(html: string; var rtf: TRichMemo);
var
 i, dummy, row: Integer;
 cfont: TFont; { Standard sschrift }
 Tag, tagparams: string;
 params: TStringList;
 FontParams : TFontParams;

 function GetTag(s: string; var i: Integer; var Tag, tagparams: string): Boolean;
 var
   a_tag: Boolean;
 begin
   GetTag  := False;
   Tag  := '';
   tagparams := '';
   a_tag  := False;

   while i <= Length(s) do
   begin
     Inc(i);
     // es wird nochein tag geöffnet --> das erste war kein tag;
     if s[i] = '<' then
     begin
       GetTag := False;
       Exit;
     end;

     if s[i] = '>' then
     begin
       GetTag := True;
       Exit;
     end;

     if not a_tag then
     begin
       if s[i] = ' ' then
       begin
         if Tag <> '' then a_tag := True;
       end
       else
         Tag := Tag + s[i];
     end
     else
       tagparams := tagparams + s[i];
   end;
 end;

 procedure GetTagParams(tagparams: string; var params: TStringList);
 var
   i: Integer;
   s: string;
   gleich: Boolean;

   // kontrolliert ob nach dem zeichen bis zum nächsten zeichen ausser
   // leerzeichen ein Ist-Gleich-Zeichen kommt
   function notGleich(s: string; i: Integer): Boolean;
   begin
     notGleich := True;
     while i <= Length(s) do
     begin
       Inc(i);
       if s[i] = '=' then
       begin
         notGleich := False;
         Exit;
       end
       else if s[i] <> ' ' then Exit;
     end;
   end;
 begin
   Params.Clear;
   s := '';
   for i := 1 to Length(tagparams) do
   begin
     if (tagparams[i] <> ' ') then
     begin
       if tagparams[i] <> '=' then gleich := False;
       if (tagparams[i] <> '''') and (tagparams[i] <> '"') then s := s + tagparams[i]
     end
     else
     begin
       if (notGleich(tagparams, i)) and (not Gleich) then
       begin
         params.Add(s);
         s := '';
       end
       else
         Gleich := True;
     end;
   end;
   params.Add(s);
 end;

 function HtmlToColor(Color: string): TColor;
 begin
   Result := StringToColor('$' + Copy(Color, 6, 2) + Copy(Color, 4,
     2) + Copy(Color, 2, 2));
 end;

 procedure TransformSpecialChars(var s: string; i: Integer);
 var
   c: string;
   z, z2: Byte;
   i2: Integer;
 const
   nchars = 9;
   chars: array[1..nchars, 1..2] of string =
     (('Ö', 'Ö'), ('ö', 'ö'), ('Ä', 'Ä'), ('ä', 'ä'),
     ('Ü', 'Ü'), ('ü', 'ü'), ('ß', 'ß'), ('<', '<'),
     ('>', '>'));
 begin
   // Maximal die nächsten 7 zeichen auf sonderzeichen überprüfen
   c  := '';
   i2 := i;
   for z := 1 to 7 do
   begin
     c := c + s[i2];
     for z2 := 1 to nchars do
     begin
       if chars[z2, 1] = c then
       begin
         Delete(s, i, Length(c));
         Insert(chars[z2, 2], s, i);
         Exit;
       end;
     end;
     Inc(i2);
   end;
 end;

 // HtmlTag Schriftgröße in pdf größe umwandeln
 function CalculateRTFSize(pt: Integer): Integer;
 begin
   case pt of
     1: Result := 6;
     2: Result := 9;
     3: Result := 12;
     4: Result := 15;
     5: Result := 18;
     6: Result := 22;
     else
       Result := 30;
   end;
 end;


 // Die Font-Stack Funktionen
type
 fontstack = record
   Font: array[1..100] of tfont;
   Pos: Byte;
 end;

 procedure CreateFontStack(var s: fontstack);
 begin
   s.Pos := 0;
 end;

 procedure PushFontStack(var s: Fontstack; fnt: TFont);
 begin
   Inc(s.Pos);
   s.Font[s.Pos] := TFont.Create;
   s.Font[s.Pos].Assign(fnt);
 end;

 procedure PopFontStack(var s: Fontstack; var fnt: TFont);
 begin
   if (s.Font[s.Pos] <> nil) and (s.Pos > 0) then
   begin
     fnt.Assign(s.Font[s.Pos]);
     // vom stack nehmen
     s.Font[s.Pos].Free;
     Dec(s.Pos);
   end;
 end;

 procedure FreeFontStack(var s: Fontstack);
 begin
   while s.Pos > 0 do
   begin
     s.Font[s.Pos].Free;
     Dec(s.Pos);
   end;
 end;
var
 fo_cnt: array[1..1000] of tfont;
 fo_liste: array[1..1000] of Boolean;
 fo_pos: TStringList;
 fo_stk: FontStack;
 wordwrap, liste: Boolean;
begin
 CreateFontStack(fo_Stk);

 fo_Pos := TStringList.Create;

 rtf.Lines.BeginUpdate;
 rtf.Lines.Clear;
 // Das wordwrap vom richedit merken
 wordwrap  := rtf.wordwrap;
 rtf.WordWrap := False;


 // erste Zeile hinzufügen
 rtf.Lines.Add('');
 Params := TStringList.Create;



 cfont := TFont.Create;
 cfont.Assign(rtf.Font);


 i := 1;
 row := 0;
 Liste := False;
 // Den eigentlichen Text holen und die Formatiorung merken
 rtf.selstart := 0;
 if Length(html) = 0 then Exit;
 repeat;


   if html[i] = '<' then
   begin
     dummy := i;
     GetTag(html, i, Tag, tagparams);
     GetTagParams(tagparams, params);

     // Das Font-Tag
     if Uppercase(Tag) = 'FONT' then
     begin
       // Schrift auf fontstack sichern
       pushFontstack(fo_stk, cfont);
       if params.Values['size'] <> '' then
         cfont.Size := CalculateRTFSize(StrToInt(params.Values['size']));

       if params.Values['color'] <> '' then cfont.Color :=
           htmltocolor(params.Values['color']);
     end
     else if Uppercase(Tag) = '/FONT' then  popFontstack(fo_stk, cfont)
     else // Die H-Tags-Überschriften
     if Uppercase(Tag) = 'H1' then
     begin
       // Schrift auf fontstack sichern
       pushFontstack(fo_stk, cfont);
       cfont.Size := 6;
     end
     else if Uppercase(Tag) = '/H1' then  popFontstack(fo_stk, cfont)
     else // Die H-Tags-Überschriften
     if Uppercase(Tag) = 'H2' then
     begin
       // Schrift auf fontstack sichern
       pushFontstack(fo_stk, cfont);
       cfont.Size := 9;
     end
     else if Uppercase(Tag) = '/H2' then  popFontstack(fo_stk, cfont)
     else // Die H-Tags-Überschriften
     if Uppercase(Tag) = 'H3' then
     begin
       // Schrift auf fontstack sichern
       pushFontstack(fo_stk, cfont);
       cfont.Size := 12;
     end
     else if Uppercase(Tag) = '/H3' then  popFontstack(fo_stk, cfont)
     else // Die H-Tags-Überschriften
     if Uppercase(Tag) = 'H4' then
     begin
       // Schrift auf fontstack sichern
       pushFontstack(fo_stk, cfont);
       cfont.Size := 15;
     end
     else if Uppercase(Tag) = '/H4' then  popFontstack(fo_stk, cfont)
     else // Die H-Tags-Überschriften
     if Uppercase(Tag) = 'H5' then
     begin
       // Schrift auf fontstack sichern
       pushFontstack(fo_stk, cfont);
       cfont.Size := 18;
     end
     else if Uppercase(Tag) = '/H5' then  popFontstack(fo_stk, cfont)
     else // Die H-Tags-Überschriften
     if Uppercase(Tag) = 'H6' then
     begin
       // Schrift auf fontstack sichern
       pushFontstack(fo_stk, cfont);
       cfont.Size := 22;
     end
     else if Uppercase(Tag) = '/H6' then  popFontstack(fo_stk, cfont)
     else // Die H-Tags-Überschriften
     if Uppercase(Tag) = 'H7' then
     begin
       // Schrift auf fontstack sichern
       pushFontstack(fo_stk, cfont);
       cfont.Size := 27;
     end
     else if Uppercase(Tag) = '/H7' then  popFontstack(fo_stk, cfont)
     else // Bold-Tag

     if Uppercase(Tag) = 'B' then cfont.Style := cfont.Style + [fsbold]
     else if Uppercase(Tag) = '/B' then cfont.Style := cfont.Style - [fsbold]
     else // Italic-Tag

     if Uppercase(Tag) = 'I' then cfont.Style := cfont.Style + [fsitalic]
     else if Uppercase(Tag) = '/I' then cfont.Style := cfont.Style - [fsitalic]
     else // underline-Tag

     if Uppercase(Tag) = 'U' then cfont.Style := cfont.Style + [fsunderline]
     else if Uppercase(Tag) = '/U' then cfont.Style := cfont.Style - [fsunderline]
     else // underline-Tag

     if Uppercase(Tag) = 'UL' then liste := True
     else if Uppercase(Tag) = '/UL' then
     begin
       liste := False;
       rtf.Lines.Add('');
       Inc(row);
       rtf.Lines.Add('');
       Inc(row);
     end
     else // BR - Breakrow tag

     if (Uppercase(Tag) = 'BR') or (Uppercase(Tag) = 'LI') then
     begin
       rtf.Lines.Add('');
       Inc(row);
     end;

     // unbekanntes tag als text ausgeben
     // else rtf.Lines[row]:=RTF.lines[row]+'<'+tag+' '+tagparams+'>';

     fo_pos.Add(IntToStr(rtf.selstart));
     fo_cnt[fo_pos.Count] := TFont.Create;
     fo_cnt[fo_pos.Count].Assign(cfont);
     fo_liste[fo_pos.Count] := liste;
   end
   else
   begin
     // Spezialzeichen übersetzen
     if html[i] = '&' then Transformspecialchars(html, i);

     if (Ord(html[i]) <> 13) and (Ord(html[i]) <> 10) then
       rtf.Lines[row] := RTF.Lines[row] + html[i];
   end;

   Inc(i);

 until i >= Length(html);
 // dummy eintragen
 fo_pos.Add('999999');

 // Den fertigen Text formatieren
 for i := 0 to fo_pos.Count - 2 do
 begin
   FontParams.Style:=fo_cnt[i + 1].Style;
   FontParams.Size:=fo_cnt[i + 1].Size;
   FontParams.Color:=fo_cnt[i + 1].Color;
   rtf.SetTextAttributes(StrToInt(fo_pos[i]),StrToInt(fo_pos[i + 1]) - StrToInt(fo_pos[i]),FontParams);

   // die font wieder freigeben;
   fo_cnt[i + 1].Free;
 end;

 // die Paragraphen also Listen setzen
 {
 i := 0;
 while i <= fo_pos.Count - 2 do
 begin
   if fo_liste[i + 1] then
   begin
     rtf.SelStart := StrToInt(fo_pos[i + 1]);
     while fo_liste[i + 1] do Inc(i);
     rtf.SelLength := StrToInt(fo_pos[i - 1]) - rtf.SelStart;
     rtf.Paragraph.Numbering := nsBullet;
   end;
   Inc(i);
 end;
 }
 rtf.Lines.EndUpdate;
 Params.Free;
 cfont.Free;
 rtf.WordWrap := wordwrap;
 FreeFontStack(fo_stk);
end;

function RTFtoHTML(rtf: TRichMemo;Title : string): string;

  function CalculateSize(pt: integer): integer;
  begin
    case pt of
      0..7: result := 1;
      8..10: result := 2;
      11..13: result := 3;
      14..16: result := 4;
      17..20: result := 5;
      21..24: result := 6;
      else result := 7;
    end;
  end;
var
  aRangeStart: Integer;
  aOldRangeStart: LongInt = -1;
  aText: String;
  aTextparams: TFontParams;
  aRangeEnd: Integer;
begin
  Result := '<html><head><title>'+Title+'</title><meta name="generator" content="Promet-ERP"></head>'+
            '<body text="#000000" bgcolor="#FFFFFF" link="#FF0000" alink="#FF0000" vlink="#FF0000">';
  rtf.GetStyleRange(0,aRangeStart,aRangeEnd);
  while aOldRangeStart < aRangeStart do
    begin
      rtf.SelStart:=aRangeStart;
      rtf.SelLength:=aRangeEnd;
      aText := rtf.SelText;
      aText := HTMLEncode(aText);
      aText := StringReplace(aText,lineending,'<br>',[rfReplaceAll]);
      rtf.GetTextAttributes(aRangeStart,aTextparams);
      Result := Result+'<font name="'+aTextParams.Name+'" size="'+IntToStr(CalculateSize(aTextparams.Size))+'" color="#'+
                IntToHex(GetRValue(aTextParams.Color),2)+
                IntToHex(GetGValue(aTextParams.Color),2)+
                IntToHex(GetBValue(aTextParams.Color),2)+'"><p>';
      if fsBold in aTextParams.Style then
        Result := Result+'<b>';
      if fsItalic in aTextParams.Style then
        Result := Result+'<i>';
      if fsUnderline in aTextParams.Style then
        Result := Result+'<u>';
      Result := Result+aText;
      if fsBold in aTextParams.Style then
        Result := Result+'</b>';
      if fsItalic in aTextParams.Style then
        Result := Result+'</i>';
      if fsUnderline in aTextParams.Style then
        Result := Result+'</u>';
      Result := Result+'</p></font>';
      aOldRangeStart := aRangeStart;
      rtf.GetStyleRange(aOldRangeStart+aRangeEnd,aRangeStart,aRangeEnd);
    end;
  if aOldRangeStart = -1 then
    Result := Result+rtf.Text;
  Result := Result+'</body></html>';
end;

end.
