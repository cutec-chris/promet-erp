unit dIPTC;

{----------------------------------------
Revision: 1.6 check in time: 2009-02-05 23:01:40 by Stefan
Workfile edit time: 2009-02-05 22:50:46
Labeled by Stefan as 'Stefan s Komponenten 2009-03-23'
Vorbereitung zur Migration nach Delphi 2009 
- Explizite Typecast auf AnsiString für Funktionsaufrufe von 
  - trim 
  - IntToStr, IntToHex 
  - AnsiUpperCase, AnsiLowerCase 
  - ExtractFileName/Ext 
- Explizite Typecast auf String für Parameter bei Funktionsaufrufen von 
  - trim 
  - StrToInt 
- ErrStr wieder von String auf AnsiString
----------------------------------------
Revision: 1.5 check in time: 2009-02-05 22:20:31 by Stefan
Workfile edit time: 2009-02-05 15:36:20
Vorbereitung zur Migration nach Delphi 2009 
- Explizite Typecast auf AnsiString für Funktionsaufrufe von 
  - trim 
  - IntToStr, IntToHex 
- Explizite Typecast auf String für Parameter bei Funktionsaufrufen von 
  - trim 
  - StrToInt
----------------------------------------
Revision: 1.4 check in time: 2009-02-05 20:26:54 by Stefan
Workfile edit time: 2009-02-04 17:14:18
Vorbereitung zur Migration nach Delphi 2009 
- String -> AnsiString 
- Char -> AnsiChar 
- Char() -> AnsiChar() 
- Uppercase -> AnsiUppercase 
- Lowercase -> AnsiLowercase
----------------------------------------
Revision: 1.3 check in time: 2008-01-02 18:23:38 by Stefan
Workfile edit time: 2008-01-02 18:22:51
+ QVCS-Header hinzugefügt 
- Unbenutzte Variablendeklarationen entfernt
----------------------------------------
Revision: 1.2 check in time: 2006-05-26 19:51:47 by Stefan
Workfile edit time: 2006-04-20 08:24:48
Labeled by Stefan as 'dEXIF 1.03d 2006-04-20'
dEXIF 1.03d 2006-04-20 }

// unit IPTC - Copyright 2001-2006, Gerry McGuire
//--------------------------------------------------------------------------
// Program to pull the IPTC (Photoshop) information out of various
// types of digital camera files.  This information can coexist in
// files containing EXIF data.  See README.TXT and LICENSE.TXT for
// information regarding the lawful use of this code.
//
// Initial Delphi unit - Gerry McGuire September 2001 - V 0.9 beta
//--------------------------------------------------------------------------
// This is based on an example provided by Earl F. Glynn.
// His web pages on graphics and Delphi programming at http://www.efg2.com
// have no equal!
//--------------------------------------------------------------------------
// I have found several often conflicting IPTC definitions in use.
// This code is designed to be easily extended.  For each new field
// enter one line in the IPTCTable and increment the TagCnt constant.
//--------------------------------------------------------------------------

interface
  uses classes, sysutils
  {$IFNDEF DELPHI}
  {$DEFINE dExifNoJpeg}
  {$ENDIF}
  {$IFNDEF dExifNoJpeg}
  ,jpeg
  {$ENDIF}
  {$IFDEF WINDOWS}
  ,windows
  {$ENDIF}
  ;

const dIPTCVersion: ansistring = '1.03d';
      TagArrayGrowth = 25;
type

  StrFunct = function (instr:ansistring):ansistring;

  TTagEntry = record
    TID: integer;        // TagTableID - EXIF use
    TType: word;         // tag type
    ICode: Word;         // iptc code
    Tag: word;           // primary key
    Name:ansistring;        // searchable
    Desc:ansistring;        // translatable
    Code:ansistring;        // decode capability
    Data:ansistring;        // display value
    Raw:ansistring;         // unprocessed value
    PRaw: integer;       // pointer to unprocessed
    FormatS:ansistring;      // Format string
    Size: integer;       // used by ITPC module
    CallBack: StrFunct;  // formatting string
  end;

  TTagDefArray = array of TTagEntry;

  {
  ITag = record
    ICode: word;
    Tag:  word;
    Name:ansistring;
    Desc:ansistring;
    Size: word;
    Data:ansistring;
  end;
    }
  ITag = TTagEntry;

  TIPTCdata = class
  private
    function getTimeZoneStr:ansistring;
  protected
    MaxTag: integer;
    parent: tobject;
    fITagCount : integer;
    fITagArray: array of iTag;
    function GetTagElement(TagID: integer): ITag;
    procedure SetTagElement(TagID: integer; const Value: ITag);
    function GetCount: integer;
    procedure SetCount(const Value: integer);
    procedure SetDateTimePrim(TimeIn: TDateTime; prefix:ansistring);
  public
//    Filename :ansistring;
    constructor Create(p:tobject);
    procedure Reset;
    property ITagArray[TagID:integer]: ITag
        read GetTagElement write SetTagElement; default;
    property Count : integer read GetCount write SetCount;
    function HasData: boolean;
    Function Clone(source:TIPTCdata):TIPTCdata;
    Function ParseIPTCStrings(buff:ansistring):tstringlist;
    Procedure ParseIPTCArray; overload;
    Procedure ParseIPTCArray(buff:ansistring);  overload;
    function IPTCArrayToBuffer:ansistring;
    function IPTCArrayToXML:tstringlist;

    function LookupTag(SearchStr:ansistring):integer; virtual;
    Function LookupTagDefn(item:ansistring): integer;
    function LookupTagByDesc(SearchStr:ansistring): integer;

    procedure RemoveTag( tagstr:ansistring ); virtual;
    function AddTag(tagstr:ansistring; dataval:ansistring = ''):integer; virtual;
    function AppendToTag(tagstr:ansistring; dataval:ansistring):integer; virtual;
    function AddOrAppend(tagstr:ansistring; dataval:ansistring):integer; virtual;
    function UpdateTag(tagstr, dataval:ansistring): integer;
    procedure SetTagByIdx(idx:integer; val:ansistring);
    function GetTag(tagstr:ansistring; defval:ansistring=''):ansistring; virtual;
    function ReadFile(fname:ansistring):boolean; virtual;
    function ReadFileStrings(fname:ansistring):tstringlist;
    function AddTagToArray(nextTag: iTag): integer;
    function GetDateTime: TDateTime;
    procedure SetDateTime(TimeIn: TDateTime);
    procedure SetDateTimeExt(TimeIn: TDateTime; prefix:ansistring);
    function GetMultiPartTag(tagName:ansistring):tstringlist;
    procedure WriteFile(fname:ansistring;origname:ansistring = ''); overload;
{$IFNDEF dExifNoJpeg}
    procedure WriteFile(fname:ansistring;memImage:tjpegimage); overload;
{$ENDIF}
  end;

const IPTCTAGCNT = 49;
      MultiTagSep: ansistring = ',';

var
  rawDefered : boolean = false;
  defaultTimeZone:ansistring = '_0000';
  IPTCMultiTags: set of byte = [20,25];
  IPTCTable : array [0..IPTCTAGCNT-1] of ITag =
    (( TID:0;TType:0;ICode: 2; Tag:  0; Name:'SKIP';         Desc:'Record Version';Code:'';Data:'';Raw:'';PRaw:0;FormatS:'';Size:64),
     ( TID:0;TType:0;ICode: 2; Tag:  3; Name:'ObjectType';   Desc:'Object Type Ref';Code:'';Data:'';Raw:'';PRaw:0;FormatS:'';Size:67),
     ( TID:0;TType:0;ICode: 2; Tag:  4; Name:'ObjectAttr';   Desc:'Object Attribute Ref';  Code:'';Data:'';Raw:'';PRaw:0;FormatS:'';Size:67),
     ( TID:0;TType:0;ICode: 2; Tag:  5; Name:'ObjectName';   Desc:'Object name';  Code:'';Data:'';Raw:'';PRaw:0;FormatS:'';Size:64),
     ( TID:0;TType:0;ICode: 2; Tag:  7; Name:'EditStatus';   Desc:'Edit Status';  Code:'';Data:'';Raw:'';PRaw:0;FormatS:'';Size:64),
     ( TID:0;TType:0;ICode: 2; Tag:  8; Name:'EditorialUpdate';  Desc:'Editorial Update';  Code:'';Data:'';Raw:'';PRaw:0;FormatS:'';Size:2),
     ( TID:0;TType:0;ICode: 2; Tag: 10; Name:'Urgency';      Desc:'Urgency';      Code:'';Data:'';Raw:'';PRaw:0;FormatS:'';Size:1),
     ( TID:0;TType:0;ICode: 2; Tag: 12; Name:'SubRef';       Desc:'Subject Reference';     Code:'';Data:'';Raw:'';PRaw:0;FormatS:'';Size:236),
     ( TID:0;TType:0;ICode: 2; Tag: 15; Name:'Category';     Desc:'Category';     Code:'';Data:'';Raw:'';PRaw:0;FormatS:'';Size:3),
     ( TID:0;TType:0;ICode: 2; Tag: 20; Name:'SuppCategory'; Desc:'Supplemental category'; Code:'';Data:'';Raw:'';PRaw:0;FormatS:'';Size:32),
     ( TID:0;TType:0;ICode: 2; Tag: 22; Name:'FixtureID';    Desc:'Fixture ID';   Code:'';Data:'';Raw:'';PRaw:0;FormatS:'';Size:32),
     ( TID:0;TType:0;ICode: 2; Tag: 25; Name:'Keywords';     Desc:'Keywords';     Code:'';Data:'';Raw:'';PRaw:0;FormatS:'';Size:64),
     ( TID:0;TType:0;ICode: 2; Tag: 26; Name:'ContentLocCode'; Desc:'Content Location Code'; Code:'';Data:'';Raw:'';PRaw:0;FormatS:'';Size: 3),
     ( TID:0;TType:0;ICode: 2; Tag: 27; Name:'ContentLocName'; Desc:'Content Location Name'; Code:'';Data:'';Raw:'';PRaw:0;FormatS:'';Size: 64),
     ( TID:0;TType:0;ICode: 2; Tag: 30; Name:'ReleaseDate';  Desc:'Release Date'; Code:'';Data:'';Raw:'';PRaw:0;FormatS:'';Size:8),
     ( TID:0;TType:0;ICode: 2; Tag: 35; Name:'ReleaseTime';  Desc:'Release Time'; Code:'';Data:'';Raw:'';PRaw:0;FormatS:'';Size:11),
     ( TID:0;TType:0;ICode: 2; Tag: 37; Name:'ExpireDate';  Desc:'Expiration Date'; Code:'';Data:'';Raw:'';PRaw:0;FormatS:'';Size:8),
     ( TID:0;TType:0;ICode: 2; Tag: 38; Name:'ExpireTime';  Desc:'Expiration Time'; Code:'';Data:'';Raw:'';PRaw:0;FormatS:'';Size:11),
     ( TID:0;TType:0;ICode: 2; Tag: 40; Name:'SpecialInstru'; Desc:'Special Instructions'; Code:'';Data:'';Raw:'';PRaw:0;FormatS:'';Size:256),
     ( TID:0;TType:0;ICode: 2; Tag: 42; Name:'ActionAdvised'; Desc:'Action Advised'; Code:'';Data:'';Raw:'';PRaw:0;FormatS:'';Size:2),
     ( TID:0;TType:0;ICode: 2; Tag: 45; Name:'RefService';    Desc:'Reference Service'; Code:'';Data:'';Raw:'';PRaw:0;FormatS:'';Size:10),
     ( TID:0;TType:0;ICode: 2; Tag: 47; Name:'RefDate';  Desc:'Reference Date'; Code:'';Data:'';Raw:'';PRaw:0;FormatS:'';Size:8),
     ( TID:0;TType:0;ICode: 2; Tag: 50; Name:'RefNumber';    Desc:'Reference Number'; Code:'';Data:'';Raw:'';PRaw:0;FormatS:'';Size:8),
     ( TID:0;TType:0;ICode: 2; Tag: 55; Name:'DateCreated';  Desc:'Date created'; Code:'';Data:'';Raw:'';PRaw:0;FormatS:'';Size:8),
     ( TID:0;TType:0;ICode: 2; Tag: 60; Name:'TimeCreated';  Desc:'Time created'; Code:'';Data:'';Raw:'';PRaw:0;FormatS:'';Size:11),
     ( TID:0;TType:0;ICode: 2; Tag: 62; Name:'DigitizeDate';  Desc:'Digital Creation Date'; Code:'';Data:'';Raw:'';PRaw:0;FormatS:'';Size:8),
     ( TID:0;TType:0;ICode: 2; Tag: 63; Name:'DigitizeTime';  Desc:'Digital Creation Time'; Code:'';Data:'';Raw:'';PRaw:0;FormatS:'';Size:11),
     ( TID:0;TType:0;ICode: 2; Tag: 65; Name:'OriginatingProgram'; Desc:'Originating Program'; Code:'';Data:'';Raw:'';PRaw:0;FormatS:'';Size: 32),
     ( TID:0;TType:0;ICode: 2; Tag: 70; Name:'ProgramVersion'; Desc:'Program version'; Code:'';Data:'';Raw:'';PRaw:0;FormatS:'';Size: 10),
     ( TID:0;TType:0;ICode: 2; Tag: 75; Name:'ObjectCycle';  Desc:'Object Cycle'; Code:'';Data:'';Raw:'';PRaw:0;FormatS:'';Size:1),
     ( TID:0;TType:0;ICode: 2; Tag: 80; Name:'ByLine';       Desc:'ByLine';       Code:'';Data:'';Raw:'';PRaw:0;FormatS:'';Size:32),
     ( TID:0;TType:0;ICode: 2; Tag: 85; Name:'ByLineTitle';  Desc:'ByLine Title'; Code:'';Data:'';Raw:'';PRaw:0;FormatS:'';Size:32),
     ( TID:0;TType:0;ICode: 2; Tag: 90; Name:'City';         Desc:'City';         Code:'';Data:'';Raw:'';PRaw:0;FormatS:'';Size:32),
     ( TID:0;TType:0;ICode: 2; Tag: 92; Name:'SubLocation';  Desc:'Sublocation';  Code:'';Data:'';Raw:'';PRaw:0;FormatS:'';Size:32),
     ( TID:0;TType:0;ICode: 2; Tag: 95; Name:'State';        Desc:'Province/State';  Code:'';Data:'';Raw:'';PRaw:0;FormatS:'';Size:32),
     ( TID:0;TType:0;ICode: 2; Tag:100; Name:'LocationCode'; Desc:'Country/Primary Location Code'; Code:'';Data:'';Raw:'';PRaw:0;FormatS:'';Size: 3),
     ( TID:0;TType:0;ICode: 2; Tag:101; Name:'LocationName'; Desc:'Country/Primary Location Name'; Code:'';Data:'';Raw:'';PRaw:0;FormatS:'';Size:   64),
     ( TID:0;TType:0;ICode: 2; Tag:103; Name:'TransmissionRef'; Desc:'Original Transmission Reference';     Code:'';Data:'';Raw:'';PRaw:0;FormatS:'';Size:   32),
     ( TID:0;TType:0;ICode: 2; Tag:105; Name:'ImageHeadline'; Desc:'Image headline'; Code:'';Data:'';Raw:'';PRaw:0;FormatS:'';Size:256),
     ( TID:0;TType:0;ICode: 2; Tag:110; Name:'ImageCredit';  Desc:'Image credit';  Code:'';Data:'';Raw:'';PRaw:0;FormatS:'';Size:32),
     ( TID:0;TType:0;ICode: 2; Tag:115; Name:'Source';       Desc:'Source';        Code:'';Data:'';Raw:'';PRaw:0;FormatS:'';Size:32),
     ( TID:0;TType:0;ICode: 2; Tag:116; Name:'Copyright';    Desc:'Copyright Notice';  Code:'';Data:'';Raw:'';PRaw:0;FormatS:'';Size:128),
     ( TID:0;TType:0;ICode: 2; Tag:118; Name:'Contact';      Desc:'Contact';       Code:'';Data:'';Raw:'';PRaw:0;FormatS:'';Size:128),
     ( TID:0;TType:0;ICode: 2; Tag:120; Name:'ImageCaption'; Desc:'Image caption'; Code:'';Data:'';Raw:'';PRaw:0;FormatS:'';Size:2000),
     ( TID:0;TType:0;ICode: 2; Tag:122; Name:'ImageCaptionWriter'; Desc:'Image caption writer'; Code:'';Data:'';Raw:'';PRaw:0;FormatS:'';Size:32),
     ( TID:0;TType:0;ICode: 2; Tag:130; Name:'ImageType';    Desc:'Image type';    Code:'';Data:'';Raw:'';PRaw:0;FormatS:'';Size:2 ),
     ( TID:0;TType:0;ICode: 2; Tag:131; Name:'Orientation';  Desc:'Image Orientation'; Code:'';Data:'';Raw:'';PRaw:0;FormatS:'';Size:1 ),
     ( TID:0;TType:0;ICode: 2; Tag:135; Name:'LangID';       Desc:'Language ID';   Code:'';Data:'';Raw:'';PRaw:0;FormatS:'';Size:3 ),
     ( TID:0;TType:0;ICode: 8; Tag:10;  Name:'Subfile';      Desc:'Subfile';       Code:'';Data:'';Raw:'';PRaw:0;FormatS:'';Size:2 )
    );

procedure IPTCWriteTransFile(fname:ansistring);
function IPTCReadTransFile(fname:ansistring):boolean;

implementation

uses dEXIF;

var
  buffer:ansistring;

constructor TIPTCdata.Create(p:tobject);
begin
  inherited create;
  fITagCount := 0;
  parent := p;
end;

function TIPTCdata.GetCount: integer;
begin
  result := fITagCount;
end;

procedure TIPTCdata.SetCount(const Value: integer);
begin
  fITagCount := value;
end;

function TIPTCdata.GetTagElement(TagID: integer): ITag;
begin
  result := fITagArray[TagID]
end;

procedure TIPTCdata.SetTagElement(TagID: integer; const Value: ITag);
begin
  fITagArray[TagID] := Value;
end;

Function ExtractTag(var start:integer):iTag;
var blen,x,tagId,code,i:integer;
    tmp:iTag;
begin
  FillChar(tmp,sizeof(iTag),0);
  code := byte(buffer[start]);
  tagId := byte(buffer[start+1]);     // should be #$1C
  blen := (byte(buffer[start+2]) shl 8 ) or byte(buffer[start+3]);
  x := blen;
  inc(start,4);                      // skip length bytes
  if code in [2,8] then
  begin
    tmp.Tag := 65534;
    for i := 0 to IPTCTAGCNT-1 do
      if (IPTCTable[i].Tag = tagid) and
         (IPTCTable[i].ICode = code) then
      begin
        if IPTCTable[i].name <> 'SKIP' then
        begin
          tmp := IPTCTable[i];
          tmp.Data := copy(buffer,start,x);
        end;
        break;
      end;
    if tmp.Tag = 65534 then
    begin
      tmp.name := 'Custom_'+AnsiString(inttostr(tagid));
      tmp.Desc := 'Custom_'+AnsiString(inttostr(tagid));
      tmp.Tag := tagid;
      tmp.ICode := code;
      tmp.Data := copy(buffer,start,x);
      tmp.Size := 64; // length for unknown fields ?
    end;
  end;
  start := start+x+1;
  result := tmp;
end;

//  This function returns the index of a tag name
//  in the tag buffer.
Function TIPTCdata.LookupTag(SearchStr:ansistring):integer;
var i: integer;
begin
 SearchStr := AnsiString(AnsiUpperCase(SearchStr));
 result := -1;
 for i := 0 to Count-1 do
   if AnsiString(AnsiUpperCase(iTagArray[i].Name)) = SearchStr then
   begin
     result := i;
     break;
   end;
end;

//  This function returns the index of a tag name
//  in the tag buffer. It searches by the description
//  which is most likely to be used as a label
Function TIPTCdata.LookupTagByDesc(SearchStr:ansistring):integer;
var i: integer;
begin
 SearchStr := AnsiString(AnsiUpperCase(SearchStr));
 result := -1;
 for i := 0 to Count-1 do
   if AnsiString(AnsiUpperCase(iTagArray[i].Desc)) = SearchStr then
   begin
     result := i;
     break;
   end;
end;

//  This function returns the index of a tag definition
//  for a given tag name.
function TIPTCdata.LookupTagDefn(item:ansistring): integer;
var i:integer;
begin
  result := -1;
  for i := 0 to IPTCTAGCNT-1 do
  begin
    if AnsiString(AnsiLowerCase(item)) = AnsiString(AnsiLowerCase(IPTCtable[i].Name)) then
    begin
      result := i;
      break;
    end;
  end;
end;

Function TIPTCdata.ParseIPTCStrings(buff:ansistring):tstringlist;
var ts:tstringlist;
    tmpItem:itag;
    start,i,j:Integer;
begin
  ts := tstringlist.Create;
  buffer := buff;
  i := Pos('Photoshop 3.0',buff)+13;
  for j := i to length(buffer) do       // Look for first field marker
    if ( byte(buffer[j]) = $1C) and
       ( byte(buffer[j+1]) in [2,8]) then
      break;
  start := j+1;
  while (start < length(buffer)-2) do   // Work through buffer
  begin
    tmpItem := ExtractTag(start);
    if tmpItem.Name <> '' then         // Empty fields are masked out
      ts.Add(tmpItem.Desc+DexifDelim+tmpItem.Data);
  end;
  result := ts;
end;
 
function TIPTCdata.AddTagToArray(nextTag:iTag):integer;
begin
  if nextTag.tag <> 0 then     // Empty fields are masked out
  begin
    if fITagCount >= MaxTag-1 then
    begin
      inc(MaxTag,TagArrayGrowth);
      SetLength(fITagArray,MaxTag);
    end;
    fITagArray[fITagCount] := nextTag;
    inc(fITagCount);
  end;
  result := fITagCount-1;
end;
 
Procedure TIPTCdata.ParseIPTCArray;
begin
  ParseIPTCArray(timgdata(parent).IPTCsegment^.data);
end;

Procedure TIPTCdata.ParseIPTCArray(buff:ansistring);
var nextTag:itag;
    start,i,j:Integer;
begin
  reset;
  buffer := buff;
  i := Pos('Photoshop 3.0',buff)+13;
  for j := i to length(buffer) do       // Look for first field marker
    if ( byte(buffer[j]) = $1C) and
       ( byte(buffer[j+1]) in [2,8]) then
      break;
  start := j+1;
  while (start < length(buffer)-2) do   // Work through buffer
  begin
    nextTag := ExtractTag(start);       // Start is incremented by function
    if nextTag.Tag in IPTCMultiTags then
    begin
      AppendToTag(nextTag.Name,nextTag.Data)
    end
    else
      AddTagToArray(nextTag);
  end;
end;
 
function MakeEntry(code,tag:integer;data:ansistring):ansistring;
var buff,sLen:ansistring;
  bLen:integer;
begin
  bLen := length(Data);
  sLen := ansichar(blen div 256)+ansichar(blen mod 256);
  result := buff+ansichar($1C)+ansichar(code)+ansichar(tag)+sLen+Data;
end;

function TIPTCdata.IPTCArrayToXML: tstringlist;
var
  buff:tstringlist;
  i:integer;
begin
  buff := TStringList.Create;
  buff.add('   <ITPCdata>');
  for i := 0 to Count-1 do
    with ITagArray[i] do
    begin
      buff.add('   <'+name+'>');
      if tag in [105,120] // headline and image caption
        then buff.add('      <![CDATA['+data+']]>')
        else buff.add('      '+data);
      buff.add('   </'+name+'>');
    end;
  buff.add('   </ITPCdata>');
  result := buff;
end;
 
function SplitMultiTag(code, tag:integer; buff:ansistring):ansistring;
var
  tmps:ansistring;
  j:integer;
begin
  result := '';
  while trim(string(buff)) <> '' do
  begin
    j := Pos(MultiTagSep,buff);
    if j > 0 then
    begin
      tmps := AnsiString(trim(string(copy(buff,1,j-1))));
      buff := AnsiString(trim(string(copy(buff,j+1,maxint))));
    end
    else
    begin
      tmps := buff;
      buff := '';
    end;
    result := result+MakeEntry(code,tag,tmps);
  end;
end;
 
function TIPTCdata.IPTCArrayToBuffer:ansistring;
var
  buff,slen,h2:ansistring;
  blen,i:integer;
begin
  buff := '';
  // load up the particular data
  for i := 0 to Count-1 do
    with ITagArray[i] do
    if (icode=2) and (tag in IPTCMultiTags) then
      buff := buff+SplitMultiTag(icode,tag,data)
    else
      buff := buff+MakeEntry(icode,tag,data);
 
// Photoshop requires the following headers:
  if not odd(length(buff)) then
    buff := buff+#0;
  h2 := MakeEntry(2,0,#0#2);
  bLen := length(buff)+length(h2);
  sLen := ansichar(blen div 256)+ansichar(blen mod 256);
  buff := 'Photoshop 3.0'#0'8BIM'#4#4#0#0#0#0+slen+h2+buff;
 
// Photoshop requires the following End-of-data marker:
  result := buff+'8BIM'#$04#$0B#0#0#0#0#0#0;
end;

function TIPTCdata.Clone(source: TIPTCdata): TIPTCdata;
var
  newie:TIPTCdata;
begin
  newie := TIPTCdata.Create(parent);
  newie.fITagArray := copy(source.fITagArray,0,MaxTag);
  newie.fITagCount := source.fITagCount;
  result := newie;
end;

function TIPTCdata.AddOrAppend(tagstr, dataval:ansistring): integer;
var
  i:integer;
begin
  result := -1;
  i := LookupTagDefn(tagStr);  // see if keyword is valid
  if i >= 0 then
  begin
    if (IPTCTable[i].Tag in IPTCMultiTags) then
      result := AppendToTag(tagstr,dataVal)
    else
      result := AddTag(tagstr,dataval);
  end;
end;

function noDups(exst,newstr:ansistring):ansistring;
var
  lst,nlst: tstringList;
  s:ansistring;
  i:integer;
begin
  lst := tstringlist.Create;
  nlst := tstringlist.Create;
  lst.CommaText := exst;
  lst.CaseSensitive := false;
  nlst.CommaText := newstr;
  for i := 0 to nlst.Count-1 do
  begin
    s := AnsiString(trim(string(nlst[i])));
    if (lst.IndexOf(s) < 0) then
    begin
      lst.Add(s);
    end;
  end;
  result := AnsiString(lst.CommaText);
end;

function TIPTCdata.AppendToTag(tagstr, dataval:ansistring): integer;
var
  inspt:integer;   // INSertion PoinT
begin
  inspt := LookupTag(tagstr);
  if (inspt >= 0) then
  begin
    if dataval <> '' then
      fITagArray[inspt].Data :=
          noDups(fITagArray[inspt].Data,dataval)
  end
  else
    inspt := AddTag(tagstr,noDups('',dataval));
  result := inspt;
end;

function TIPTCdata.UpdateTag(tagstr, dataval:ansistring): integer;
var inspt:integer;   // INSertion PoinT
begin
  inspt := LookupTag(tagstr);
  if (inspt >= 0) then
  begin
    if dataval <> '' then
      fITagArray[inspt].Desc := dataval
  end;
  result := inspt;
end;

function TIptcData.GetMultiPartTag(tagName:ansistring):tstringlist;
var tmp:tstringlist;
begin
  tmp := tstringlist.create;
  tmp.CommaText := StringReplace(
    GetTag(tagname),MultiTagSep,',',[rfReplaceAll]);
  result := tmp;
end;
 
function TIPTCdata.AddTag(tagstr, dataval:ansistring): integer;
var inspt,defidx:integer;
  newTag:itag;
begin
  inspt := LookupTag(tagstr);
  if (inspt >= 0) then
  begin
    if dataval <> '' then
      fITagArray[inspt].Data := dataval
 end
  else
  begin
    defidx := LookupTagDefn(tagstr);
    if defidx < 0 then
    begin
      result := -1;
      exit;  // not a defined node, do not insert
    end;
    newTag := IPTCTable[defidx];
    newTag.Data := dataVal;
    inspt := AddTagToArray(newTag);
  end;
  result := inspt;
end;

procedure TIPTCdata.RemoveTag(tagstr:ansistring);
var rempt,i:integer;
begin
 rempt := LookupTag(tagstr);
 if (rempt >= 0) then
 begin
   for i := rempt to fITagCount-2 do
     fITagArray[i] := fITagArray[i+1];
   dec(fITagCount);
 end;
end;
 
procedure TIPTCdata.Reset;
begin
 Count := 0 ;
 FillChar(fITagArray[0],sizeof(iTag)*MaxTag,0);  // clear out old data
end;

function TIPTCdata.GetTag(tagstr:ansistring; defval:ansistring=''):ansistring;
var i:integer;
begin
  result := defval;
  i := LookupTag(tagstr);
  if i >=0 then
    result := ITagArray[i].Data;
end;

Function TIPTCdata.HasData:boolean;
begin
  result := Count > 0;
end;
 
function TIPTCdata.ReadFile(fname:ansistring):boolean;
var p:tImgData;
begin
  p := tImgData(parent);
  Reset;
  p.ProcessFile(FName);                      // Get data from file.
  if p.IPTCSegment <> nil then               // If IPTC segment detected
  begin
    ParseIPTCArray(p.IPTCSegment^.Data);
//    filename := FName;
  end;
  result := HasData();
end;
 
function TIPTCdata.ReadFileStrings(fname:ansistring):tstringlist;
begin
  result := ParseIPTCStrings(timgdata(parent).IPTCSegment^.Data);
end;
 
{$IFNDEF dExifNoJpeg}
 
procedure TIPTCdata.WriteFile(fname:ansistring;memImage:tjpegimage);
var tmp:ansistring;
begin
  tmp := IPTCArrayToBuffer;                       // Create temp buffer
  timgdata(parent).MakeIPTCSegment(tmp);          // Create IPTC segment
  timgdata(parent).WriteEXIFjpeg(memImage,FName); // Write to disk
end;
 
procedure TIPTCdata.WriteFile(FName:ansistring; OrigName :ansistring = '');
var tmp:ansistring;
    Orig:tjpegimage;
begin
  Orig := TJPEGImage.Create;
  if OrigName = '' then
    OrigName := FName;
  Orig.LoadFromFile(OrigName);                // Get the image
  tmp := IPTCArrayToBuffer;                   // Create temp buffer
  timgdata(parent).MakeIPTCSegment(tmp);      // Create IPTC segment
  timgdata(parent).WriteEXIFjpeg(Orig,FName); // Write to disk
  Orig.free;
end;

{$ELSE}

procedure TIPTCdata.WriteFile(fname:ansistring; origname :ansistring = '');
begin
  // if you're not using Borland's jpeg unit
  // then you should override/avoid this method
  raise exception.create('WriteIPTCfile does nothing!');
  // I suppose I should make this method abstract...
end;
 
{$ENDIF}
procedure TIPTCdata.SetTagByIdx(idx: integer; val:ansistring);
begin
  fITagArray[idx].Data := val;
end;

function GetTimeZoneBias:longint;
{$IFDEF WINDOWS}
var
  TZoneInfo: TTimeZoneInformation;
{$ENDIF}
begin
  {$IFDEF WINDOWS}
  GetTimeZoneInformation(TZoneInfo);
  result := TZoneInfo.Bias;
  {$ENDIF}
end;

function TIPTCdata.getTimeZoneStr:ansistring;
var tmp,h,m:integer;
    sign:ansistring;
begin
  result := defaultTimeZone;
  if defaultTimeZone <> '_0000' then
    exit;
  tmp := GetTimeZoneBias();
  h := abs(tmp) div 60; // hours
  m := abs(tmp) mod 60; // minutes
  if tmp < 0         // local time correction: invertsign
    then sign := '+'
    else sign := '-';
  result := AnsiString(Format('%s%.2d%.2d',[sign,h,m]));
end;

procedure TIPTCdata.SetDateTimePrim(TimeIn:TDateTime; prefix:ansistring);
var dateStr, timeStr, timeZone:ansistring;
begin
  if AnsiString(AnsiLowerCase(prefix)) = 'default' then
  begin
    datestr := 'DateCreated';
    timestr := 'TimeCreated';
  end
  else
  begin
    datestr := prefix+'Date';
    timestr := prefix+'Time';
  end;
  timeZone := getTimeZoneStr();  // use local time zone
  AddTag(datestr, AnsiString(FormatDateTime('yyyymmdd',TimeIn)));
  AddTag(timestr, AnsiString(FormatDateTime('hhnnss',TimeIn))+timeZone);
end;

procedure TIPTCdata.SetDateTime(TimeIn:TDateTime);
begin
  SetDateTimePrim(TimeIn,'Default');
end;

procedure TIPTCdata.SetDateTimeExt(TimeIn:TDateTime; prefix:ansistring);
begin
  SetDateTimePrim(TimeIn,prefix);
end;

function TIPTCdata.GetDateTime:TDateTime;
type
  TConvert= packed record
     year: Array [1..4] of ansichar;
     mon, day, hr, min, sec: Array [1..2] of ansichar;
  end;
  PConvert= ^TConvert;
var
  tsd,tst:ansistring;
begin
   try
     tsd := GetTag('DateCreated','00000000');
     tst := tsd+GetTag('TimeCreated','000000');
     with PConvert( @tst[1] )^ do
       Result := EncodeDate( StrToInt( year),
                             StrToInt( mon ),
                             StrToInt( day ))
              +  EncodeTime( StrToInt( hr  ),
                             StrToInt( min ),
                             StrToInt( sec ), 0);
   except
     result := 0;
   end;
end;

procedure IPTCWriteTransFile(fname:ansistring);
var tmp:tstringlist;
    i: integer;
begin
  tmp := tstringlist.Create;
  for i := 0 to IPTCTAGCNT-1 do
    tmp.Add( IPTCTable[i].Name+'='+ IPTCTable[i].Desc);
  tmp.SaveToFile(fname);
  tmp.Free;
end;

function IPTCReadTransFile(fname:ansistring):boolean;
var tmp:tstringlist;
    i: integer;
    ts:ansistring;
begin
  result := false;
  if not fileexists(fname) then
    exit;
  tmp := tstringlist.Create;
  tmp.LoadFromFile(fname);
  for i := 0 to IPTCTAGCNT-1 do
  begin
    ts := AnsiString(tmp.Values[IPTCTable[i].Name]);
    if ts > '' then
      IPTCTable[i].Desc := ts;
  end;
  tmp.Free;
end;

end.