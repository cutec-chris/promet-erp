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
unit uOCR;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, ProcessUtils, FileUtil, Graphics,
  FPImage, FPWritePNM, IntfGraphics, Utils, SynaUtil,
  uminiconvencoding,uDocuments,uImaging,FPReadJPEG,FPReadPNG,
  dateutils,uBaseDbClasses;
type
  TOCRPages = TList;
  TGOCRProcess = class(TExtendedProcess)
  private
    FPages: TOCRPages;
    FNumber : Integer;
    OldDone : TNotifyEvent;
    procedure GOCRProcessDone(Sender: TObject);
  public
    procedure Execute; override;
    constructor Create(Pages : TOCRPages;Image : TPicture);
  end;
  TCuneIFormProcess = class(TExtendedProcess)
  private
    FPages: TOCRPages;
    FNumber : Integer;
    OldDone : TNotifyEvent;
    procedure GOCRProcessDone(Sender: TObject);
  public
    procedure Execute; override;
    constructor Create(Pages : TOCRPages;Image : TPicture);
  end;
  TTesseractProcess = class(TExtendedProcess)
  private
    FPages: TOCRPages;
    FNumber : Integer;
    OldDone : TNotifyEvent;
    procedure ProcessDone(Sender: TObject);
  public
    procedure Execute; override;
    constructor Create(Pages : TOCRPages;Image : TPicture);
  end;
  TUnPaperProcess = class(TExtendedProcess)
  private
    FImage: TPicture;
    OldDone : TNotifyEvent;
    procedure UnpaperProcessDone(Sender: TObject);
  public
    procedure Execute; override;
    constructor Create(Image : TPicture);
  end;
function DoOCR(aDoc : TDocument;reworkImage : Boolean = True) : TOCRPages;
procedure StartOCR(Pages : TOCRPages;Image : TPicture;reworkImage : Boolean = True);
function FixText(aText : TStrings) : Integer;
function GetTitle(aText : TStrings;aBase : Int64 = 0) : string;
function GetTitleEx(aText : TStrings;aBase : Int64;var aStart,aLen : Integer) : string;
function GetDate(aText: TStrings): TDateTime;
function GetDateEx(aText: TStrings;var aStart,aLen : Integer): TDateTime;
var
  OnallprocessDone : TNotifyEvent;
implementation
uses uBaseApplication;
var
  Processes : TList;

function DoOCR(aDoc: TDocument;reworkImage : Boolean = True): TOCRPages;
var
  aFullstream: TMemoryStream;
  aPic: TPicture;
  Fail: Boolean;
  aText : string = '';
begin
  aFullstream := TMemoryStream.Create;
  aDoc.CheckoutToStream(aFullStream);
  aFullStream.Position:=0;
  aDoc.GetText(aFullstream,ExtractFileExt(aDoc.FileName),aText);
  aFullStream.Position:=0;
  Result := TOCRPages.Create;
  if aText<>'' then  //Trial to get Text from other Files than Images
    begin
      Fail:=False;
      Result.Add(tStringList.Create);
      TStringList(Result[result.Count-1]).Text:=atext;
    end
  else
    begin
      aPic := TPicture.Create;
      Fail := False;
      try
        aPic.LoadFromStreamWithFileExt(aFullStream,ExtractFileExt(aDoc.FileName));
      except
        Fail := True;
      end;
      aFullStream.Free;
      if not Fail then
        StartOCR(Result,aPic,reworkImage);
      aPic.Free;
    end;
end;

procedure StartOCR(Pages: TOCRPages;Image : TPicture;reworkImage : Boolean = True);
var
  aImage: TFPMemoryImage;
  r: TFPReaderJPEG;
  aP: TExtendedProcess;
begin
  try
    if reworkImage then
      begin
        aImage := TFPMemoryImage.Create(1,1);
        with BaseApplication as IBaseApplication do
          begin
            DeleteFileUTF8(GetInternalTempDir+'rpv.jpg');
            Image.SaveToFile(GetInternalTempDir+'rpv.jpg');
            r := TFPReaderJPEG.Create;
            aImage.LoadFromFile(GetInternalTempDir+'rpv.jpg',r);
            r.Free;
            uImaging.Delight(aImage);
            aImage.SaveToFile(GetInternalTempDir+'rpv.jpg');
            aImage.Free;
            Image.LoadFromFile(GetInternalTempDir+'rpv.jpg');
            DeleteFileUTF8(GetInternalTempDir+'rpv.jpg');
          end;
      end;
  except
    on e : Exception do
      begin
        with BaseApplication as IBaseApplication do
          Error(e.Message);
      end;
  end;
  try
    aP := TTesseractProcess.Create(Pages,Image);
    FreeAndNil(aP);
  except
  end;
  if Pages.Count=0 then
    begin
      try
        aP := TCuneIFormProcess.Create(Pages,Image);
        FreeAndNil(aP);
      except
      end;
    end;
  if Pages.Count=0 then
    begin
      try
        aP := TGOCRProcess.Create(Pages,Image);
        FreeAndNil(aP);
      except
      end;
    end;
end;
function FixText(aText: TStrings): Integer;
var
  BadChars : Integer = 0;
  i: Integer;
  function RemoveBadChars(Line : string) : string;
  var
    i: Integer;
  begin
    for i := 1 to length(Line) do
      if IsNumeric(Line[i])
      or ((ord(Line[i])>=34)
      and (ord(Line[i])<=254))
      then
        Result := Result+Line[i]
      else
        begin
          Result := Result+' ';
          inc(BadChars);
        end;
  end;
begin
  for i := 0 to aText.Count-1 do
    begin
      aText[i] := AnsiToUtf8(RemoveBadChars(Utf8ToAnsi(aText[i])));
    end;
  Result := BadChars;
end;
function GetTitle(aText: TStrings;aBase : Int64 = 0): string;
var
  aStart: Integer;
  aLen: Integer;
begin
  Result := GetTitleEx(aText,aBase,aStart,aLen);
end;
function StrToSysDate(s : string) : TDateTime;
var
   df:string;
   d,m,y,ly:word;
   n,i:longint;
   c:word;
   dp,mp,yp,which : Byte;
   s1:string[4];
   values:array[1..3] of longint;
   LocalTime:tsystemtime;
   YearMoreThenTwoDigits : boolean;
begin
  Result := 0;
  YearMoreThenTwoDigits := False;
  df := UpperCase(ShortDateFormat);
  { Determine order of D,M,Y }
  yp:=0;
  mp:=0;
  dp:=0;
  Which:=0;
  i:=0;
  while (i<Length(df)) and (Which<3) do
   begin
     inc(i);
     Case df[i] of
       'Y' :
         if yp=0 then
          begin
            Inc(Which);
            yp:=which;
          end;
       'M' :
         if mp=0 then
          begin
            Inc(Which);
            mp:=which;
          end;
       'D' :
         if dp=0 then
          begin
            Inc(Which);
            dp:=which;
          end;
     end;
   end;
  if Which<>3 then
   exit;
{ Get actual values }
  for i := 1 to 3 do
    values[i] := 0;
  s1 := '';
  n := 0;
  for i := 1 to length(s) do
   begin
     if s[i] in ['0'..'9'] then
      s1 := s1 + s[i];

     { space can be part of the shortdateformat, and is defaultly in slovak
       windows, therefor it shouldn't be taken as separator (unless so specified)
       and ignored }
     if (DateSeparator <> ' ') and (s[i] = ' ') then
       Continue;

     if (s[i] = dateseparator) or ((i = length(s)) and (s[i] in ['0'..'9'])) then
      begin
        inc(n);
        if n>3 then
         exit;
         // Check if the year has more then two digits (if n=yp, then we are evaluating the year.)
        if (n=yp) and (length(s1)>2) then YearMoreThenTwoDigits := True;
        val(s1, values[n], c);
        if c<>0 then
         exit;
        s1 := '';
      end
     else if not (s[i] in ['0'..'9']) then
       exit;
   end ;
  // Fill in values.
  getLocalTime(LocalTime);
  ly := LocalTime.Year;
  If N=3 then
   begin
     y:=values[yp];
     m:=values[mp];
     d:=values[dp];
   end
  Else
  begin
    Y:=ly;
    If n<2 then
     begin
       d:=values[1];
       m := LocalTime.Month;
     end
    else
     If dp<mp then
      begin
        d:=values[1];
        m:=values[2];
      end
    else
      begin
        d:=values[2];
        m:=values[1];
      end;
  end;
  if (y >= 0) and (y < 100) and not YearMoreThenTwoDigits then
    begin
    ly := ly - TwoDigitYearCenturyWindow;
    Inc(Y, ly div 100 * 100);
    if (TwoDigitYearCenturyWindow > 0) and (Y < ly) then
      Inc(Y, 100);
    end;
  try
    Result := EncodeDate(y, m, d);
  except
    Result := 0;
  end;
end;

function WordsCount(s: string ): integer;
var
  ps: PChar;
  nSpaces, n: integer;
begin
  n := 0;
  // make it a string that ends with value zero
  s := s + #0;
  // point to start of string s
  ps := @s[ 1 ];
  // keep loop going up to zero ending
  while( #0 <> ps^ ) do
  begin
    // checks for space = ' '
    while((' ' = ps^) and (#0 <> ps^)) do
    begin
      inc( ps );
    end;
    nSpaces := 0;
    while((' ' <> ps^) and (#0 <> ps^)) do
    begin
      inc(nSpaces);
      inc(ps);
    end;
    if (nSpaces > 0) then
    begin
      inc( n );
    end;
  end;
  Result := n;
end;

function GetTitleEx(aText: TStrings; aBase: Int64; var aStart, aLen: Integer
  ): string;
var
  i: Integer;
  Res: Boolean = False;
  aBase2: Integer;
  aLine: String;

  function ExtractSpecial(line,ident : string) : string;
  begin
    Result := '';
    if pos(Uppercase(ident),Uppercase(line)) = 1 then
      Result := copy(line,pos(Uppercase(ident),Uppercase(line)),length(line))
    else if pos(Uppercase(' '+ident),Uppercase(line)) > 0 then
      Result := copy(line,pos(Uppercase(ident),Uppercase(line)),length(line));
    if pos('  ',Result) > 0 then
      Result := copy(Result,0,pos('   ',Result)-1);
    Result := trim(Result);
  end;
begin
  result := '';
  for i := 0 to aText.Count-1 do
    begin
      aLine := aText[i];
      Result := ExtractSpecial(aLine,'Rechnung');
      if Result='' then
        Result := ExtractSpecial(aLine,'Lieferschein');
      if Result='' then
        Result := ExtractSpecial(aLine,'Auftrag');
      if Result='' then
        Result := ExtractSpecial(aLine,'Auftragsbestätigung');
      if Result='' then
        Result := ExtractSpecial(aLine,'Invoice');
      if Result='' then
      Result := ExtractSpecial(aLine,'Bill');
      if Result <> '' then
        begin
          aStart:=Pos(Result,atext.Text)-1;
          aLen:=length(Result);
          exit;
        end;
    end;
  GetDateEx(aText,aStart,aLen);
  i := 0;
  while aStart>Length(aText[i]) do
    begin
      aStart := aStart-Length(aText[i]);
      inc(i);
    end;
  aBase := i;
  aBase2 := 0;
  for i := aBase to aText.Count-1 do
    begin
      if (pos('SEHR',Uppercase(aText[i])) > 0)
      or (pos('GEEHRTE',Uppercase(aText[i])) > 0)
      or (pos('DEAR',Uppercase(aText[i])) > 0)
      then
        aBase2 := i;
    end;
  for i := aBase2-1 downto aBase do
    begin
      if (trim(aText[i])='') and (Result <>'') then break;
      if (WordsCount(aText[i])>0) and (WordsCount(aText[i])<5) then
        begin
          Result := aText[i];
          aStart:=Pos(Result,atext.Text)-1;
          aLen:=length(Result);
        end;
    end;
  if Result = '' then
    begin
      i := 0;
      while trim(aText[i])='' do
        inc(i);
      if (WordsCount(aText[i])>0) and (WordsCount(aText[i])<5) then
        begin
          Result := aText[i];
          aStart:=Pos(Result,atext.Text)-1;
          aLen:=length(Result);
        end;
    end;
end;

function GetDate(aText: TStrings): TDateTime;
var
  aPos: Integer;
  aStart: Integer;
begin
  Result := GetDateEx(aText,aStart,aPos);
end;

{ Returns a count of the number of occurences of SubText in Text }
function CountOccurences( const SubText: string;
                          const Text: string): Integer;
begin
  if (SubText = '') OR (Text = '') OR (Pos(SubText, Text) = 0) then
    Result := 0
  else
    Result := (Length(Text) - Length(StringReplace(Text, SubText, '', [rfReplaceAll]))) div  Length(subtext);
end;  { CountOccurences }

function GetDateEx(aText: TStrings; var aStart, aLen: Integer): TDateTime;
var
  tmp: string;
  i: Integer;
  mon: Integer;
  a: Integer;
  aDate: String;
  b: Integer;
  function IsDate(Str : string) : TDateTime;
  var
    OD : TDateTime;
  begin
    Result := 0;
    if (Result=0) and (StrToSysDate(Str) > 0) and (CountOccurences(DateSeparator,Str)=2) then Result := StrToSysDate(Str);
    if (Result=0) and (GetDateMDYFromStr(Str) > 0) and (CountOccurences(DateSeparator,Str)=2) then Result := GetDateMDYFromStr(Str);
  end;

begin
  for i := 0 to aText.Count-1 do
    begin
      tmp := aText[i];
      tmp := Stringreplace(tmp,' ','',[rfReplaceAll]);
      for mon := 1 to 12 do
        tmp := Stringreplace(tmp,Shortmonthnames[mon],IntToStr(mon),[]);
      for a := 1 to length(tmp)-4 do
        begin
          aDate := copy(tmp,a,length(tmp));
          Result := IsDate(aDate);
          if (Result<IncYear(Now,15)) and (Result>IncYear(Now,-15)) then
            begin
              aStart := Pos(aDate,aText.Text)-1;
              aLen:=length(aDate);
              exit;
            end;
        end;
    end;
  for i := 0 to aText.Count-1 do
    begin
      tmp := aText[i];
      tmp := Stringreplace(tmp,' ','',[rfReplaceAll]);
      tmp := Stringreplace(tmp,'O','0',[rfReplaceAll]);
      tmp := Stringreplace(tmp,'o','0',[rfReplaceAll]);
      for mon := 1 to 12 do
        tmp := Stringreplace(tmp,Shortmonthnames[mon],IntToStr(mon),[]);
      for a := 1 to length(tmp)-4 do
        begin
          aDate := copy(tmp,a,length(tmp));
          Result := IsDate(aDate);
          if (Result<IncYear(Now,15)) and (Result>IncYear(Now,-15)) then
            begin
              aStart := Pos(aDate,aText.Text)-1;
              aLen:=length(aDate);
              exit;
            end;
        end;
    end;
end;

{ TTesseractProcess }

procedure TTesseractProcess.ProcessDone(Sender: TObject);
var
  aSList: TStringList;
begin
  with BaseApplication as IBaseApplication do
    begin
      SysUtils.DeleteFile(GetInternalTempDir+IntToStr(FNumber)+'export.jpg');
      aSList := TStringList.Create;
      if FileExists(GetInternalTempDir+IntToStr(FNumber)+'export.txt') then
        begin
          aSList.LoadFromFile(GetInternalTempDir+IntToStr(FNumber)+'export.txt');
          aSList.Text := ConvertEncoding(aSList.Text,GuessEncoding(aSList.Text),EncodingUTF8);
          //SysUtils.DeleteFile(GetInternalTempDir+IntToStr(FNumber)+'export.txt');
          FPages.Add(aSList);
        end;
      if Processes.IndexOf(Self) > -1 then
        Processes.Remove(Self);
      if Assigned(OnallprocessDone) then
        begin
          if Processes.Count = 0 then
            OnallprocessDone(nil);
        end;
      if Assigned(OldDone) then
        OldDone(Self);
    end;
end;

procedure TTesseractProcess.Execute;
begin
  inherited Execute;
  while Running do
    begin
      sleep(100);
    end;
  ProcessDone(nil);
end;

constructor TTesseractProcess.Create(Pages: TOCRPages; Image: TPicture);
var
  aPath: String;
begin
  aPath := 'tesseract'+ExtractFileExt(BaseApplication.ExeName);
  {$IFDEF WINDOWS}
  aPath := AppendPathDelim(AppendPathDelim(BaseApplication.Location)+'tools'+DirectorySeparator+'tesseract')+aPath;
  {$ENDIF}
  FNumber := Processes.Add(Self);
  with BaseApplication as IBaseApplication do
    begin
      Image.SaveToFile(GetInternalTempDir+IntToStr(FNumber)+'export.jpg');
      aPath := aPath+' '+GetInternalTempDir+IntToStr(FNumber)+'export.jpg '+GetInternalTempDir+IntToStr(FNumber)+'export -l deu';
    end;
  OldDone := Self.OnDone;
  Self.OnDone:=@ProcessDone;
  FPages := Pages;
  inherited Create(aPath);
end;

procedure TUnPaperProcess.UnpaperProcessDone(Sender: TObject);
begin
  with BaseApplication as IBaseApplication do
    begin
      FImage.LoadFromFile(GetInternalTempDir+'unpaperexport.pnm');
      SysUtils.DeleteFile(GetInternalTempDir+'unpaperexport.pnm');
    end;
  if Assigned(OldDone) then
    OldDone(Self);
end;
procedure TUnPaperProcess.Execute;
begin
  inherited Execute;
  while Running do
    begin
      sleep(100);
    end;
  UnpaperProcessDone(nil);
end;
constructor TUnPaperProcess.Create(Image: TPicture);
var
  aPath: String;
  aImage: TLazIntfImage;
  Writer: TFPCustomImageWriter;
begin
  aPath := 'unpaper'+ExtractFileExt(BaseApplication.ExeName);
  {$IFDEF WINDOWS}
  aPath := AppendPathDelim(AppendPathDelim(BaseApplication.Location)+'tools')+aPath;
  {$ENDIF}
  aImage := TLazIntfImage.Create(1,1);
  aImage.LoadFromBitmap(Image.Bitmap.Handle,Image.Bitmap.MaskHandle);
  {$IFDEF VER2_5_1}
  Writer := TFPWriterPNM.Create;
  {$ENDIF}
  {$IFDEF VER2_6}
  Writer := TFPWriterPNM.Create;
  {$ENDIF}
  {$IFDEF VER2_4}
  Writer := TFPWriterPNM.Create(3);
  {$ENDIF}
  with BaseApplication as IBaseApplication do
    aImage.SaveToFile(GetInternalTempDir+'unpaperexport.pnm',Writer);
  Writer.Free;
  aImage.Free;
  with BaseApplication as IBaseApplication do
    aPath := aPath+' --layout single '+GetInternalTempDir+'unpaperexport.pnm';
  OldDone := Self.OnDone;
  Self.OnDone:=@UnpaperProcessDone;
  FImage := Image;
  inherited Create(aPath);
end;
procedure TCuneIFormProcess.GOCRProcessDone(Sender: TObject);
var
  aSList: TStringList;
begin
  with BaseApplication as IBaseApplication do
    begin
      SysUtils.DeleteFile(GetInternalTempDir+IntToStr(FNumber)+'export.jpg');
      aSList := TStringList.Create;
      if FileExists(GetInternalTempDir+IntToStr(FNumber)+'export.txt') then
        begin
          aSList.LoadFromFile(GetInternalTempDir+IntToStr(FNumber)+'export.txt');
          aSList.Text := ConvertEncoding(aSList.Text,GuessEncoding(aSList.Text),EncodingUTF8);
          SysUtils.DeleteFile(GetInternalTempDir+IntToStr(FNumber)+'export.txt');
          FPages.Add(aSList);
        end;
      if Processes.IndexOf(Self) > -1 then
        Processes.Remove(Self);
      if Assigned(OnallprocessDone) then
        begin
          if Processes.Count = 0 then
            OnallprocessDone(nil);
        end;
      if Assigned(OldDone) then
        OldDone(Self);
    end;
end;
procedure TCuneIFormProcess.Execute;
begin
  inherited Execute;
  while Running do
    begin
      sleep(100);
    end;
  GOCRProcessDone(nil);
end;
constructor TCuneIFormProcess.Create(Pages: TOCRPages; Image: TPicture);
var
  aPath: String;
begin
  aPath := 'cuneiform'+ExtractFileExt(BaseApplication.ExeName);
  {$IFDEF WINDOWS}
  aPath := AppendPathDelim(AppendPathDelim(BaseApplication.Location)+'tools'+DirectorySeparator+'cuneiform')+aPath;
  {$ENDIF}
  FNumber := Processes.Add(Self);
  with BaseApplication as IBaseApplication do
    begin
      Image.SaveToFile(GetInternalTempDir+IntToStr(FNumber)+'export.jpg');
      aPath := aPath+' -l ger '+GetInternalTempDir+IntToStr(FNumber)+'export.jpg -o '+GetInternalTempDir+IntToStr(FNumber)+'export.txt';
    end;
  OldDone := Self.OnDone;
  Self.OnDone:=@GOCRProcessDone;
  FPages := Pages;
  inherited Create(aPath);
end;
procedure TGOCRProcess.GOCRProcessDone(Sender: TObject);
var
  aSList: TStringList;
begin
  with BaseApplication as IBaseApplication do
    begin
      SysUtils.DeleteFile(GetInternalTempDir+IntToStr(FNumber)+'export.pnm');
      aSList := TStringList.Create;
      if FileExists(GetInternalTempDir+IntToStr(FNumber)+'export.txt') then
        begin
          aSList.LoadFromFile(GetInternalTempDir+IntToStr(FNumber)+'export.txt');
          aSList.Text := ConvertEncoding(aSList.Text,GuessEncoding(aSList.Text),EncodingUTF8);
          SysUtils.DeleteFile(GetInternalTempDir+IntToStr(FNumber)+'export.txt');
          FPages.Add(aSList);
        end;
      if Processes.IndexOf(Self) > -1 then
        Processes.Remove(Self);
      if Assigned(OnallprocessDone) then
        begin
          if Processes.Count = 0 then
            OnallprocessDone(nil);
        end;
      if Assigned(OldDone) then
        OldDone(Self);
    end;
end;
procedure TGOCRProcess.Execute;
begin
  inherited Execute;
  while Running do
    begin
      sleep(100);
    end;
  GOCRProcessDone(nil);
end;
constructor TGOCRProcess.Create(Pages: TOCRPages; Image: TPicture);
var
  aPath: String;
  aImage: TLazIntfImage;
  Writer: TFPCustomImageWriter;
begin
  aPath := 'gocr'+ExtractFileExt(BaseApplication.ExeName);
  {$IFDEF WINDOWS}
  aPath := AppendPathDelim(AppendPathDelim(BaseApplication.Location)+'tools')+aPath;
  {$ENDIF}
  aImage := TLazIntfImage.Create(1,1);
  aImage.LoadFromBitmap(Image.Bitmap.Handle,Image.Bitmap.MaskHandle);
  {$IFDEF VER2_5_1}
  Writer := TFPWriterPNM.Create;
  {$ENDIF}
  {$IFDEF VER2_6}
  Writer := TFPWriterPNM.Create;
  {$ENDIF}
  {$IFDEF VER2_4}
  Writer := TFPWriterPNM.Create(3);
  {$ENDIF}
  FNumber := Processes.Add(Self);
  with BaseApplication as IBaseApplication do
    begin
      aImage.SaveToFile(GetInternalTempDir+IntToStr(FNumber)+'export.pnm',Writer);
      Writer.Free;
      aImage.Free;
      aPath := aPath+' '+GetInternalTempDir+IntToStr(FNumber)+'export.pnm -o '+GetInternalTempDir+IntToStr(FNumber)+'export.txt';
    end;
  OldDone := Self.OnDone;
  Self.OnDone:=@GOCRProcessDone;
  FPages := Pages;
  inherited Create(aPath);
end;
initialization
  Processes := TList.Create;
finalization
  Processes.Free;
end.


