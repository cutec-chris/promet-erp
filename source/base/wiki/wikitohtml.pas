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
unit wikitohtml;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Utils,RegExpr,htmltowiki;

function WikiText2HTML(input: string;LinkOffset : string = '';RemoveLinkOffset : string = '';IproChanges : Boolean = False;aLevel : Integer = 0): string;
function StripWikiText(input : string) : string;

type
  TImageConvertFunc = procedure(Image : string;var OutFile : string; var aLinkTags,aHref,aTags,aWidth : string) of Object;
  TWikiIncludeFunc = procedure(Inp : string;var Outp : string;aLevel : Integer = 0) of Object;
var
  OnConvertImage : TImageConvertFunc;
  OnWikiInclude : TWikiIncludeFunc;
  OnWikiLink : TWikiIncludeFunc;
  OnWikiRefresh : TWikiIncludeFunc;

implementation

uses uminiconvencoding;

function IsUNCPath(const Path: String): Boolean;
begin
  Result := false;
end;

function ExtractUNCVolume(const Path: String): String;
begin
  Result := '';
end;

{
  Returns
  - DriveLetter + : + PathDelim on Windows (if present) or
  - UNC Share on Windows if present or
  - PathDelim if FileName starts with PathDelim on Unix or Wince or
  - Empty string of non eof the above applies
}
function ExtractFileRoot(FileName: String): String;
var
  Len: Integer;
begin
  Result := '';
  Len := Length(FileName);
  if (Len > 0) then
  begin
    if IsUncPath(FileName) then
    begin
      Result := ExtractUNCVolume(FileName);
      // is it like \\?\C:\Directory?  then also include the "C:\" part
      if (Result = '\\?\') and (Length(FileName) > 6) and
         (FileName[5] in ['a'..'z','A'..'Z']) and (FileName[6] = ':') and (FileName[7] in AllowDirectorySeparators)
      then
        Result := Copy(FileName, 1, 7);
    end
    else
    begin
      {$if defined(unix) or defined(wince)}
      if (FileName[1] = PathDelim) then Result := PathDelim;
      {$else}
      if (Len > 2) and (FileName[1] in ['a'..'z','A'..'Z']) and (FileName[2] = ':') and (FileName[3] in AllowDirectorySeparators) then
        Result := UpperCase(Copy(FileName,1,3));
      {$endif}
    end;
  end;
end;

function CompareFilenames(const Filename1, Filename2: string): integer;
{$IFDEF darwin}
var
  F1: CFStringRef;
  F2: CFStringRef;
{$ENDIF}
begin
  {$IFDEF darwin}
  if Filename1=Filename2 then exit(0);
  if (Filename1='') or (Filename2='') then
    exit(length(Filename2)-length(Filename1));
  F1:=CFStringCreateWithCString(nil,Pointer(Filename1),kCFStringEncodingUTF8);
  F2:=CFStringCreateWithCString(nil,Pointer(Filename2),kCFStringEncodingUTF8);
  Result:=CFStringCompare(F1,F2,kCFCompareNonliteral
          {$IFDEF CaseInsensitiveFilenames}+kCFCompareCaseInsensitive{$ENDIF});
  CFRelease(F1);
  CFRelease(F2);
  {$ELSE}
    {$IFDEF CaseInsensitiveFilenames}
    Result:=UTF8CompareText(Filename1, Filename2);
    {$ELSE}
    Result:=CompareStr(Filename1, Filename2);
    {$ENDIF}
  {$ENDIF}
end;

function ChompPathDelim(const Path: string): string;
var
  Len, MinLen: Integer;
begin
  Result:=Path;
  if Path = '' then
    exit;
  Len:=length(Result);
  if (Result[1] in AllowDirectorySeparators) then begin
    MinLen := 1;
    {$IFDEF HasUNCPaths}
    if (Len >= 2) and (Result[2] in AllowDirectorySeparators) then
      MinLen := 2; // keep UNC '\\', chomp 'a\' to 'a'
    {$ENDIF}
  end
  else begin
    MinLen := 0;
    {$IFdef MSWindows}
    if (Len >= 3) and (Result[1] in ['a'..'z', 'A'..'Z'])  and
       (Result[2] = ':') and (Result[3] in AllowDirectorySeparators)
    then
      MinLen := 3;
    {$ENDIF}
  end;

  while (Len > MinLen) and (Result[Len] in AllowDirectorySeparators) do dec(Len);
  if Len<length(Result) then
    SetLength(Result,Len);
end;

{
  Returns True if it is possible to create a relative path from Source to Dest
  Function must be thread safe, so no expanding of filenames is done, since this
  is not threadsafe (at least on Windows platform)

  - Dest and Source must either be both absolute filenames, or relative
  - Dest and Source cannot contain '..' since no expanding is done by design
  - Dest and Source must be on same drive or UNC path (Windows)
  - if both Dest and Source are relative they must at least share their base directory
  - Double PathDelims are ignored (unless they are part of the UNC convention)

  - if UsePointDirectory is True and Result is True then if RelPath is Empty string, RelPath becomes '.'
  - if AlwaysRequireSharedBaseFolder is False then Absolute filenames need not share a basefolder

  - if the function succeeds RelPath contains the relative path from Source to Dest,
    no PathDelimiter is appended to the end of RelPath

  Examples:
  - Dest = /foo/bar Source = /foo Result = True RelPath = bar
  - Dest = /foo///bar Source = /foo// Result = True RelPath = bar
  - Dest = /foo Source = /foo/bar Result = True RelPath = ../
  - Dest = /foo/bar Source = /bar Result = True RelPath = ../foo/bar
  - Dest = foo/bar Source = foo/foo Result = True RelPath = ../bar
  - Dest = foo/bar Source = bar/foo Result = False (no shared base directory)
  - Dest = /foo Source = bar Result = False (mixed absolute and relative)
  - Dest = c:foo Source = c:bar Result = False (no expanding)
  - Dest = c:\foo Source = d:\bar Result is False (different drives)
  - Dest = \foo Source = foo (Windows) Result is False (too ambiguous to guess what this should mean)
  - Dest = /foo Source = /bar AlwaysRequireSharedBaseFolder = True Result = False
  - Dest = /foo Source = /bar AlwaysRequireSharedBaseFolder = False Result = True RelPath = ../foo
}

function TryCreateRelativePath(const Dest, Source: String; UsePointDirectory: boolean;
                               AlwaysRequireSharedBaseFolder: Boolean; out RelPath: String): Boolean;
Const
  MaxDirs = 129;
Type
  TDirArr =  Array[1..MaxDirs] of String;

  function SplitDirs(Dir: String; out Dirs: TDirArr): Integer;
  var
    Start, Stop, Len: Integer;
    S: String;
  begin
    Result := 0;
    Len := Length(Dir);
    if (Len = 0) then Exit;
    Start := 1;
    Stop := 1;

    While Start <= Len do
    begin
      if (Dir[Start] in AllowDirectorySeparators) then
      begin
        S := Copy(Dir,Stop,Start-Stop);
        //ignore empty strings, they are caused by double PathDelims, which we just ignore
        if (S <> '') then
        begin
          Inc(Result);
          if Result>High(Dirs) then
            raise Exception.Create('too many sub directories');
          Dirs[Result] := S;
        end;
        Stop := Start + 1;
      end;
      Inc(Start);
    end;
    //If (Len > 0) then

    S := Copy(Dir,Stop,Start-Stop);
    if (S <> '') then
    begin
      Inc(Result);
      Dirs[Result] := S;
    end;
  end;


var
  {$if ((FPC_VERSION>=3) or ((FPC_VERSION>=2) and ((FPC_RELEASE>=5) or ((FPC_RELEASE>=4) and (FPC_PATCH>=1)))))}
  CompareFunc: function(const Item1, Item2: String): LongInt;
  {$else}
  CompareFunc: function(const Item1, Item2: String): PtrInt;
  {$endif}
  SourceRoot, DestRoot, CmpDest, CmpSource: String;
  CmpDestLen, CmpSourceLen, DestCount, SourceCount, i,
  SharedFolders, LevelsBack, LevelsUp: Integer;
  SourceDirs, DestDirs: Array[1..MaxDirs] of String;
  IsAbs: Boolean;
begin
  Result := False;
  if (Dest = '') or (Source = '') then Exit;
  if (Pos('..',Dest) > 0) or (Pos('..',Source) > 0) then Exit;
  SourceRoot := ExtractFileRoot(Source);
  DestRoot := ExtractFileRoot(Dest);
  //debugln('TryCreaterelativePath: DestRoot = "',DestRoot,'"');
  //debugln('TryCreaterelativePath: SourceRoot = "',SourceRoot,'"');
  //Root must be same: either both absolute filenames or both relative (and on same drive in Windows)
  if (CompareFileNames(SourceRoot, DestRoot) <> 0) then Exit;
  IsAbs := (DestRoot <> '');
  {$if defined(windows) and not defined(wince)}
  if not IsAbs then  // relative paths
  begin
    //we cannot handle files like c:foo
    if ((Length(Dest) > 1) and (UpCase(Dest[1]) in ['A'..'Z']) and (Dest[2] = ':')) or
       ((Length(Source) > 1) and (UpCase(Source[1]) in ['A'..'Z']) and (Source[2] = ':')) then Exit;
    //we cannot handle combinations like dest=foo source=\bar or the other way around
    if ((Dest[1] in AllowDirectorySeparators) and not (Source[1] in AllowDirectorySeparators)) or
       (not (Dest[1] in AllowDirectorySeparators) and (Source[1] in AllowDirectorySeparators)) then Exit;
  end;
  {$endif}

  {$IFDEF CaseInsensitiveFilenames}
  CompareFunc := @UTF8CompareText;
  {$else CaseInsensitiveFilenames}
  CompareFunc := @CompareStr;
  {$endif CaseInsensitiveFilenames}

  CmpSource := Source;
  CmpDest := Dest;
  {$IFDEF darwin}
  CmpSource := GetDarwinSystemFilename(CmpSource);
  CmpDest := GetDarwinSystemFilename(CmpDest);
  {$ENDIF}


  CmpDest := ChompPathDelim(Dest);
  CmpSource := ChompPathDelim(Source);
  if IsAbs then
  begin
    System.Delete(CmpSource,1,Length(SourceRoot));
    System.Delete(CmpDest,1,Length(DestRoot));
  end;

  //Get rid of excessive trailing PathDelims now after (!) we stripped Root
  while (Length(CmpDest) > 0) and (CmpDest[Length(CmpDest)] in AllowDirectorySeparators) do System.Delete(CmpDest,Length(CmpDest),1);
  while (Length(CmpSource) > 0) and (CmpSource[Length(CmpSource)] in AllowDirectorySeparators) do System.Delete(CmpSource,Length(CmpSource),1);

  //debugln('TryCreaterelativePath: CmpDest   = "',cmpdest,'"');
  //debugln('TryCreaterelativePath: CmpSource = "',cmpsource,'"');
  CmpDestLen := Length(CmpDest);
  CmpSourceLen := Length(CmpSource);

  DestCount := SplitDirs(CmpDest, DestDirs);
  SourceCount :=  SplitDirs(CmpSource, SourceDirs);

  //debugln('TryCreaterelativePath: DestDirs:');
  //for i := 1 to DestCount do debugln(DbgS(i),' "',DestDirs[i],'"'); debugln;
  //debugln('TryCreaterelativePath:');
  //for i := 1 to SourceCount do debugln(DbgS(i),' "',SourceDirs[i],'"'); debugln;


  i := 1;
  SharedFolders := 0;
  while (i <= DestCount) and (i <= SourceCount) do
  begin
    if (CompareFunc(DestDirs[i], SourceDirs[i]) = 0) then
    begin
      Inc(SharedFolders);
      Inc(i);
    end
    else
    begin
      Break;
    end;
  end;

  //debugln('TryCreaterelativePath: SharedFolders = ',DbgS(SharedFolders));
  if (SharedFolders = 0) and ((not IsAbs) or AlwaysRequireSharedBaseFolder) and not ((CmpDestLen = 0) or (CmpSourceLen = 0)) then
  begin
    //debguln('TryCreaterelativePath: FAIL: IsAbs = ',DbgS(IsAs),' AlwaysRequireSharedBaseFolder = ',DbgS(AlwaysRequireSharedBaseFolder),
    //' SharedFolders = 0, CmpDestLen = ',DbgS(cmpdestlen),' CmpSourceLen = ',DbgS(CmpSourceLen));
    Exit;
  end;
  LevelsBack := SourceCount - SharedFolders;
  LevelsUp := DestCount - SharedFolders;
  //debugln('TryCreaterelativePath: LevelsBack = ',DbgS(Levelsback));
  //debugln('TryCreaterelativePath: LevelsUp   = ',DbgS(LevelsUp));
  if (LevelsBack > 0) then
  begin
    RelPath := '';
    for i := 1 to LevelsBack do RelPath := '..' + PathDelim + Relpath;

    for i := LevelsUp downto 1 do
    begin
      if (RelPath <> '') and not (RelPath[Length(RelPath)] in AllowDirectorySeparators) then RelPath := RelPath + PathDelim;
      RelPath := RelPath + DestDirs[DestCount + 1 - i];
    end;
    RelPath := ChompPathDelim(RelPath);
  end
  else
  begin
    RelPath := '';
    for i := LevelsUp downto 1 do
    begin
      if (RelPath <> '') then RelPath := RelPath + PathDelim;
      RelPath := RelPath + DestDirs[DestCount + 1 - i];
    end;
  end;
  if UsePointDirectory and (RelPath = '') then
    RelPath := '.'; // Dest = Source

  Result := True;
end;


function WikiText2HTML(input: string; LinkOffset: string;
  RemoveLinkOffset: string; IproChanges: Boolean; aLevel: Integer): string;
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
  otstr: String;
  tmp: String;
  bLink: String;
  procedure DoReplace(var InStr,OutStr : string;ReplaceTag,NewTag : string;MustbeInOneLine : Boolean = False);
  var
    NewLine: String;
  begin
    while pos(ReplaceTag,instr) > 0 do
      begin
        NewLine := copy(instr,pos(ReplaceTag,instr)+length(ReplaceTag),length(instr));
        if MustBeInOneLine
        and ((pos(#13,NewLine) < pos(ReplaceTag,NewLine))) and (not (length(NewLine) = pos(ReplaceTag,NewLine)+length(ReplaceTag)-1)) then
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
              ImageFile := UniToSys(HTMLDecode(copy(istr,0,pos('|',istr)-1)));
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
              else ImageFile := UniToSys(HTMLDecode(copy(istr,0,pos(']]',istr)-1)));
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
              ImageFile := UniToSys(HTMLDecode(copy(istr,0,pos('|',istr)-1)));
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
              ImageFile := UniToSys(HTMLDecode(copy(istr,0,pos(']]',istr)-1)));
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
          ImageFile := UniToSys(HTMLDecode(copy(istr,0,pos(']]',istr)-1)));
          istr := copy(istr,pos(']]',istr)+2,length(istr));
          if Assigned(OnWikiInclude) then
            begin
              tmp := '';
              OnWikiInclude(ImageFile,tmp,aLevel+1);
              ostr+=tmp;
              if (tmp='') and (copy(istr,0,1)=#13) then
                istr := copy(istr,2,length(istr));
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
            if pos(']',lowercase(istr))>0 then
              istr := copy(istr,pos(']',lowercase(istr))+1,length(istr))
            else istr := '';
          end;
      end;
  end;
begin
  istr := ConvertEncoding(input,GuessEncoding(Input),EncodingUTF8);
  ostr := '';
  open_uls := 0;
  act_uls := 0;
  //all newlines to \n
  istr := StringReplace(istr,#13#10,#13,[rfReplaceAll]);
  istr := StringReplace(istr,#10#13,#13,[rfReplaceAll]);
  if copy(trim(istr),0,1)='=' then
    istr := #13+istr;
  if copy(trim(istr),0,1)='*' then
    istr := #13+istr;
  istr := StringReplace(istr,#10,#13,[rfReplaceAll]);
  //Remove NOTOC                           if Assigned(OnWikiInclude) then
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
  while pos(#13+'*',istr) > 0 do
    begin
      ostr := ostr+copy(istr,0,pos(#13+'*',istr)-1);
      istr := copy(istr,pos(#13+'*',istr)+2,length(istr));
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
      if pos(#13,istr) > 0 then
        begin
          ostr := ostr+copy(istr,0,pos(#13,istr)-1);
          istr := copy(istr,pos(#13,istr),length(istr));
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
  while pos(#13+'#',istr) > 0 do
    begin
      ostr := ostr+copy(istr,0,pos(#13+'#',istr)-1);
      istr := copy(istr,pos(#13+'#',istr)+2,length(istr));
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
      if pos(#13,istr) > 0 then
        begin
          ostr := ostr+copy(istr,0,pos(#13,istr)-1);
          istr := copy(istr,pos(#13,istr),length(istr));
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
      istr := copy(istr,pos(#13,istr)-1,length(istr));
      tstr := copy(istr,0,pos(#13+'|}',istr)-1);
      istr := copy(istr,pos(#13+'|}',istr)+3,length(istr));
      otstr := '<table><tr valign="top">';
      //tstr := StringReplace(tstr,'|-','</tr><tr valign="top" align="left">',[rfReplaceAll]);
      intd := False;
      while length(tstr) > 2 do
        begin
          if ((tstr[1] = #13) and (tstr[2] = '|') and ((length(tstr)=2) or (tstr[3] = '-')))
          or ((tstr[1] = #13) and (tstr[2] = '!') and ((length(tstr)=2) or (tstr[3] = '-'))) then
            begin
              if inTD then
                otstr := otstr+'</td>';
              inTD := False;
              otstr := otstr+'</tr><tr valign="top" align="left">';
              tstr := copy(tstr,4,length(tstr));
            end
          else if ((tstr[1] = #13) and (tstr[2] = '|') and ((length(tstr)=2) or (tstr[3] <> '|')))
          or ((tstr[1] = #13) and (tstr[2] = '!') and ((length(tstr)=2) or (tstr[3] <> '!'))) then
            begin
              if inTD then
                otstr := otstr+'</td>'
              else
                otstr := otstr+'<td align="left">';
              inTD := not inTD;
              tstr := copy(tstr,3,length(tstr));
            end
          else if ((tstr[1] = '!') and (tstr[2] = '!'))
               or ((tstr[1] = '|') and (tstr[2] = '|')) then
            begin
              if inTD then
                begin
                  otstr := otstr+'</td><td align="left">'
                end
              else //Schould never happen
                begin
                  otstr := otstr+'<td>';
                  inTD := True;
                end;
              tstr := copy(tstr,3,length(tstr));
            end
          else
            begin
              otstr := otstr+tstr[1];
              tstr := copy(tstr,2,length(tstr));
            end;
        end;
      otstr := otstr+tstr+'</tr></table>';
      ostr := ostr+otstr+istr;
      istr := ostr;
      ostr := '';
    end;
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
              aLink := linkoffset+UniToSys(HTMLDecode(linkcontent));
              if TryCreateRelativePath('./'+aLink,'./'+RemoveLinkOffset,False,False,tmp) then
                aLink := tmp;
              //if copy(aLink,0,length(RemoveLinkOffset)) = RemoveLinkOffset then
              //  aLink := copy(aLink,length(RemoveLinkOffset)+1,length(aLink));
              if Assigned(OnWikiLink) then
                begin
                  tmp := '';
                  OnWikiLink(aLink,tmp,aLevel+1);
                  aLink := tmp;
                end;
              ostr := ostr+'<a href="'+aLink+'"';
              istr := copy(istr,pos('|',istr)+1,length(istr));
              ostr := ostr+' title="'+copy(istr,0,pos(']]',istr)-1)+'" alt="'+copy(istr,0,pos(']]',istr)-1)+'">'+copy(istr,0,pos(']]',istr)-1)+'</a>';
              istr := copy(istr,pos(']]',istr)+2,length(istr));
        end
      else
        begin
          linkcontent := copy(istr,0,pos(']]',istr)-1);
          aLink := linkoffset+linkcontent;
          if copy(aLink,0,length(RemoveLinkOffset)) = RemoveLinkOffset then
            aLink := copy(aLink,length(RemoveLinkOffset),length(aLink));
          if Assigned(OnWikiLink) then
            begin
              tmp := '';
              OnWikiLink(aLink,tmp,aLevel+1);
              aLink := tmp;
            end;
          if pos('::',linkcontent) > 0 then
            ostr:=ostr+'<a href="'+aLink+'">'
          else
            ostr := ostr+'<a href="'+aLink+'" title="'+copy(istr,0,pos(']]',istr)-1)+'">'+copy(istr,0,pos(']]',istr)-1)+'</a>';
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
      istr := ReplaceRegExpr('\r======(.*?)======',istr,#13'<h5>$1</h5>',True);
      istr := ReplaceRegExpr('\r=====(.*?)=====',istr,#13'<h5>$1</h5>',True);
      istr := ReplaceRegExpr('\r====(.*?)====',istr,#13'<h4>$1</h4>',True);
      istr := ReplaceRegExpr('\r===(.*?)===',istr,#13'<h3>$1</h3>',True);
      istr := ReplaceRegExpr('\r==(.*?)==',istr,#13'<h2>$1</h2>',True);
      istr := ReplaceRegExpr('\r=(.*?)=\r',istr,#13'<h2>$1</h2>',True);
    end
  else
    begin
      istr := ReplaceRegExpr('\r======(.*?)======',istr,#13'<h5>$1</h5>',True);
      istr := ReplaceRegExpr('\r=====(.*?)=====',istr,#13'<h4>$1</h4>',True);
      istr := ReplaceRegExpr('\r====(.*?)====',istr,#13'<h3>$1</h3>',True);
      istr := ReplaceRegExpr('\r===(.*?)===',istr,#13'<h2>$1</h2>',True);
      istr := ReplaceRegExpr('\r==(.*?)==',istr,#13'<h1>$1</h1>',True);
      istr := ReplaceRegExpr('\r=(.*?)=\r',istr,#13'<h1>$1</h1>'#13,True);
    end;
  //Process unformated stuff
  while pos(#13+' ',istr) > 0 do
    begin
      //Replace Line breaks in text bevore pre
      ostr := ostr+StringReplace(StringReplace(copy(istr,0,pos(#13+' ',istr)-1),#13#13,'<br><br>',[rfReplaceAll]),#13,'',[rfReplaceAll]);
      istr := copy(istr,pos(#13+' ',istr)+2,length(istr));
      ostr := ostr+'<pre>';
      while (pos(#13+' ',istr) > 0) do
        begin
          ostr := ostr+copy(istr,0,pos(#13,istr));
          istr := copy(istr,pos(#13,istr)+1,length(istr));
          if (length(istr) > 0) and (istr[1] <> ' ') then
            break
          else
            istr := copy(istr,2,length(istr));
        end;
      ostr := ostr+'</pre>';
    end;
  ostr := ostr+StringReplace(istr,#13#13,'<br><br>',[rfReplaceAll]);
  if copy(ostr,0,1)=#13 then ostr := copy(ostr,2,length(ostr));
  ostr := StringReplace(ostr,#13,'<br>',[rfReplaceAll]);
  //Remove <br> after <h*>
  if IproChanges then
    begin
      ostr := StringReplace(ostr,'</h1><br>','</h1>',[rfReplaceAll]);
      ostr := StringReplace(ostr,'</h2><br>','</h2>',[rfReplaceAll]);
      ostr := StringReplace(ostr,'</h3><br>','</h3>',[rfReplaceAll]);
      ostr := StringReplace(ostr,'</h4><br>','</h4>',[rfReplaceAll]);
      ostr := StringReplace(ostr,'</h5><br>','</h5>',[rfReplaceAll]);
      ostr := StringReplace(ostr,'</h6><br>','</h6>',[rfReplaceAll]);
      Result := ostr;
    end
  else
    Result := ostr;
end;

function StripWikiText(input: string): string;
begin
  Result := StripHTML(WikiText2HTML(input));
end;


end.

