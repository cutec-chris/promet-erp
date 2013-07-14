UNIT fuzzysearch;

{$mode objfpc}{$H+}
interface

uses Math,SysUtils;

FUNCTION PrepareTheString ( OriginStr: String; VAR ConvStr:String): Integer;
FUNCTION NGramMatch ( TextPara, SearchStr    : String;
                      SearchStrLen, NGramLen : Integer;
                      VAR MaxMatch           : Integer ) : Integer;
function StrSimilar (s1, s2: string): Integer;
function SearchInStr(Source,Search : string;var Around : string;MaxAround : Integer) : Integer;

implementation

//CONST MaxParLen = 255;

FUNCTION PrepareTheString ( OriginStr: String; VAR ConvStr:String): Integer;
VAR i:Integer;
BEGIN
ConvStr:=OriginStr;
FOR i:=1 TO Length(OriginStr) DO
   BEGIN
   ConvStr[i]:=UpCase(ConvStr[i]);
   IF ConvStr[i]<'0' THEN
      ConvStr[i]:=' '
   ELSE
      CASE ConvStr[i] OF
         Chr(196) : ConvStr[i]:=Chr(228);
         Chr(214) : ConvStr[i]:=Chr(246);
         Chr(220) : ConvStr[i]:=Chr(252);
         Chr(142) : ConvStr[i]:=Chr(132);
         Chr(153) : ConvStr[i]:=Chr(148);
         Chr(154) : ConvStr[i]:=Chr(129);
         ':'      : ConvStr[i]:=' ';
         ';'      : ConvStr[i]:=' ';
         '<'      : ConvStr[i]:=' ';
         '>'      : ConvStr[i]:=' ';
         '='      : ConvStr[i]:=' ';
         '?'      : ConvStr[i]:=' ';
         '['      : ConvStr[i]:=' ';
         ']'      : ConvStr[i]:=' ';
         '/'      : ConvStr[i]:=' ';
         '\'      : ConvStr[i]:=' ';
         END;
   END;
PrepareTheString:=i;
END;

FUNCTION NGramMatch ( TextPara, SearchStr    : String;
                      SearchStrLen, NGramLen : Integer;
                      VAR MaxMatch           : Integer ) : Integer;

VAR NGram      : String[8];
    NGramCount : Integer;
    i, Count   : Integer;
BEGIN
  NGramCount:=SearchStrLen-NGramLen+1;
  Count:=0;
  MaxMatch:=0;
  i := 1;
  while i <= NGramCount do
    begin
      NGram:=Copy(SearchStr,i,NGramLen);
      if (NGram[NGramLen-1]=' ') AND (NGram[1]<>' ') THEN
        Inc(i,NGramLen-3)    (* Wird in der Schleife noch erhoeht! *)
      else
        begin
          Inc(MaxMatch,NGramLen);
          if Pos(NGram,TextPara)>0 then
            Inc(Count);
        end;
      inc(i);
    end;
  NGramMatch:=Count*NGramLen;
END;

function StrSimilar (s1, s2: string): Integer;
var hit: Integer; // Number of identical chars
    p1, p2: Integer; // Position count
    l1, l2: Integer; // Length of strings
    pt: Integer; // for counter
    diff: Integer; // unsharp factor
    hstr: string; // help var for swapping strings
    // Array shows if position is already tested
    test: array [1..255] of Boolean;
begin
 // Test Length and swap, if s1 is smaller
 // we alway search along the longer string
 if Length(s1) < Length(s2) then begin
  hstr:= s2; s2:= s1; s1:= hstr;
 end;
 // store length of strings to speed up the function
 l1:= Length (s1);
 l2:= Length (s2);
 p1:= 1; p2:= 1; hit:= 0;
 // calc the unsharp factor depending on the length
 // of the strings. Its about a third of the length
 diff:= Max (l1, l2) div 3 + ABS (l1 - l2);
 // init the test array
 for pt:= 1 to l1 do test[pt]:= False;
 // loop through the string
 repeat
  // position tested?
  if not test[p1] then begin
   // found a matching character?
   if (s1[p1] = s2[p2]) and (ABS(p1-p2) <= diff) then begin
    test[p1]:= True;
    Inc (hit); // increment the hit count
    // next positions
    Inc (p1); Inc (p2);
    if p1 > l1 then p1:= 1;
   end else begin
    // Set test array
    test[p1]:= False;
    Inc (p1);
    // Loop back to next test position if end of the string
    if p1 > l1 then begin
     while (p1 > 1) and not (test[p1]) do Dec (p1);
     Inc (p2)
    end;
   end;
  end else begin
   Inc (p1);
   // Loop back to next test position if end of string
   if p1 > l1 then begin
    repeat Dec (p1); until (p1 = 1) or test[p1];
    Inc (p2);
   end;
  end;
 until p2 > Length(s2);
 // calc procentual value
 Result:= 100 * hit DIV l1;
end;

function SearchInStr(Source, Search: string; var Around: string;
  MaxAround: Integer): Integer;
var
  i: Integer;
  Max: LongInt;
  idx: LongInt;
  a: LongInt;
  aSearch : string;
begin
  Result := 0;
  Max := 0;
  if length(Search) > length(Source) then
    aSearch := copy(Search,0,length(Source))
  else
    aSearch := Search;
  if Pos(UpperCase(aSearch),UpperCase(Source)) > 0 then
    begin
      if Pos(UpperCase(aSearch),UpperCase(Source)) = 1 then
        Result := 100
      else
        Result := 90;
      if length(Source) < MaxAround then
        Around := Source
      else
        Around := '...'+copy(Source,Pos(UpperCase(aSearch),UpperCase(Source))-(((MaxAround-6) div 2)+(length(aSearch) div 2)),MaxAround-6)+'...';
    end
  else
    begin
      for i := 0 to length(Source)-length(aSearch)-1 do
        begin
          a := StrSimilar(UpperCase(copy(Source,i,length(aSearch))),UpperCase(aSearch));
          if a > Max then
            begin
              Max := a;
              idx := i;
            end;
        end;
      if Max > 0 then
        begin
          if i = 0 then
            Result := Max
          else
            Result := round(Max*0.7);
          if length(Source) < MaxAround then
            Around := Source
          else
            Around := '...'+copy(Source,idx-(((MaxAround-6) div 2)+(length(aSearch) div 2)),MaxAround-6)+'...';
        end;
      Result := round(Result*0.8);
    end;
end;


END.
