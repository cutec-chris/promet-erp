unit uHelp;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  IpHtml,IpMsg, StdCtrls,uGeneralStrConsts, Buttons,fuzzysearch,
  FastHTMLParser,Utils,FileUtil,LCLIntf;

type

  { TSimpleIpHtml }

  TSimpleIpHtml = class(TIpHtml)
  public
    property OnGetImageX;
    constructor Create;
  end;
  
  TFoundPlace = record
    text : string;
    Similarity : real;
  end;
  
  TSearchResult = record
    Title : string;
    Adress : string;
    Count : Integer;
    Similarity : real;
    FoundPlaces : array of TFoundPlace;
  end;

  TOpenPageFailedEvent = procedure(Sender : TObject;Page : string) of Object;

  { TfHelpBrowser }

  TfHelpBrowser = class(TForm)
    bBack: TBitBtn;
    bNext: TBitBtn;
    bHome: TBitBtn;
    bSearch: TBitBtn;
    eSearch: TEdit;
    HelpViewer: TIpHtmlPanel;
    Image2: TImage;
    lNoHelp: TLabel;
    Panel1: TPanel;
    pToolBar: TPanel;
    procedure bSearchClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure HtmlViewerHotClick(Sender: TObject);
    procedure bBackClick(Sender: TObject);
    procedure bHomeClick(Sender: TObject);
    procedure bNextClick(Sender: TObject);
    procedure pToolBarClick(Sender: TObject);
  private
    FHelpOffSet: string;
    FOnOpenPageFailed: TOpenPageFailedEvent;
    { private declarations }
    OldPath : string;
    History : TStringList;
    Historyindex : Integer;
    HomeURL : string;
    SearchResultFile : TStringStream;
    SBold : Boolean;
    STitle : Boolean;
    SSearchStr : string;
    SFileName : string;
    SNewFound : Boolean;
    TxtTitle : string;
    SearchResults : array of TSearchResult;
    Language : string;
    procedure HTMLGetImageX(Sender: TIpHtmlNode; const URL: string;var Picture: TPicture);
    procedure CheckButtons;
    procedure SethelpOffset(const AValue: string);
    procedure SetOnOpenPageFailed(const AValue: TOpenPageFailedEvent);
    procedure TagFound(aNocase : string;xTag: string);
    procedure TextFound(xText: string);
  public
    { public declarations }
    property HelpOffset : string read FHelpOffSet write SethelpOffset;
    procedure OpenHelp(const Filename: string);
    procedure GotoHelp(Keyword : string);
    procedure SetLanguage(Lang : string);
    procedure FuzzySearch(Dir : string;Search : string);
    procedure FuzzySearchFile(FName : string;Search : string);
    property  OnOpenPageFailed : TOpenPageFailedEvent read FOnOpenPageFailed write SetOnOpenPageFailed;
  end;

var
  fHelpBrowser: TfHelpBrowser;

implementation

{ TfHelpBrowser }

procedure TfHelpBrowser.FuzzySearch(Dir: string; Search: string);
var
  FindRec : TSearchRec;
begin
  IF FindFirst(Dir + '*.*', faAnyFile, FindRec) = 0 THEN
    REPEAT
      IF (FindRec.Name <> '.') AND (FindRec.Name <> '..') THEN
        begin
          if FindRec.Attr and faDirectory = faDirectory then
            FuzzySearch(Dir+FindRec.Name+DirectorySeparator,Search)
          else if (UpperCase(ExtractFileExt(FindRec.Name)) = '.HTML') and (copy(UpperCase(FindRec.Name),length(FindRec.Name)-6,length(FindRec.Name)) = UpperCase(copy(Language,0,2))+'.HTML') then
            FuzzySearchFile(Dir+FindRec.Name,Search);
        end;
    UNTIL FindNext(FindRec) <> 0;
  FindClose(FindRec);
end;

procedure TfHelpBrowser.FuzzySearchFile(FName: string; Search: string);
var
  Parser : THtmlParser;
  SF: TStringList;
  aSimilarity : real;
  i: Integer;
begin
  TxtTitle := '';
  STitle := False;
  SBold := False;
  SNewFound := True;
  SSearchStr := Search;
  SF := TStringList.Create;
  SF.LoadFromFile(FName);
  SFileName := FName;
  Parser := THtmlParser.Create(SF.Text);
  SF.Free;
  Parser.OnFoundTag := @TagFound;
  Parser.OnFoundText := @TextFound;
  Parser.Exec;
  Parser.Free;
end;

procedure TfHelpBrowser.TagFound(aNocase : string;xTag: string);
begin
  if xTag = '<B>' then
    SBold := True
  else if xTag = '</B>' then
    SBold := False
  else if xTag = '<TITLE>' then
    STitle := True
  else if xTag = '</TITLE>' then
    STitle := False;
end;

procedure TfHelpBrowser.TextFound(xText: string);
var
  TextLen: Integer;
  TextPara : string;
  TextBuffer : String;
  NGram1Len    : Integer;
  NGram2Len    : Integer;
  MatchCount1  : Integer;
  MatchCount2  : Integer;
  MaxMatch1    : Integer;
  MaxMatch2    : Integer;
  Similarity   : Real;
  SearchStr : string;
  aSearchStr : string;
  SearchStrLen: Integer;
  Around : string;
begin
  if STitle then
    TxtTitle := xText;
  SearchStr := SSearchStr+' ';
  while pos(' ',SearchStr) > 0 do
    begin
      aSearchStr := copy(SearchStr,0,pos(' ',SearchStr)-1);
      SearchStr := copy(SearchStr,pos(' ',SearchStr)+1,length(SearchStr));
      Around := '';

      Similarity := SearchInStr(xText,aSearchStr,Around,40);

      if STitle then
      else if SBold then
        Similarity:=0.85 * Similarity
      else
        Similarity:=0.70 * Similarity;
      if Similarity < 50 then
        exit;
      //We have a valid result
      if SNewFound then
        begin
          Setlength(SearchResults,length(SearchResults)+1);
          SearchResults[length(SearchResults)-1].Adress := sFileName;
          SearchResults[length(SearchResults)-1].Title := TxtTitle;
          Setlength(SearchResults[length(SearchResults)-1].FoundPlaces,0);
        end;
      Setlength(SearchResults[length(SearchResults)-1].FoundPlaces,length(SearchResults[length(SearchResults)-1].FoundPlaces)+1);
      if Around = '' then
        SearchResults[length(SearchResults)-1].FoundPlaces[length(SearchResults[length(SearchResults)-1].FoundPlaces)-1].Text := xText
      else
        SearchResults[length(SearchResults)-1].FoundPlaces[length(SearchResults[length(SearchResults)-1].FoundPlaces)-1].Text := Around;
      SearchResults[length(SearchResults)-1].FoundPlaces[length(SearchResults[length(SearchResults)-1].FoundPlaces)-1].Similarity := Similarity;
      SNewFound := False;
    end;
end;

procedure TfHelpBrowser.bSearchClick(Sender: TObject);
var
  NewHTML: TSimpleIpHtml;
  i: Integer;
  a: Integer;
  Idx : Integer;
  Max : real;
  b: Integer;
  aSimilarity: real;
begin
  if eSearch.Text = '' then
    exit;
  SearchResultFile := TStringStream.Create('');
  SearchResultFile.WriteString('<html>'+#10#13+'<head>'+#10#13+'<title>'+strSearchresults+'</title>'+#10#13+'</head>'+#10#13+'<body link="#0000ff" vlink="#800080">'+#10#13+'<big><b>'+strSearchResults+'</b></big>'+#10#13+'<br><br><hr><br>');
  FuzzySearch(ExtractFilePath(Application.Exename)+Directoryseparator+'help'+DirectorySeparator,eSearch.Text);
  Max := 0;
  for i := 0 to length(SearchResults)-1 do
    if length(SearchResults[i].FoundPlaces) > Max then
      Max := length(SearchResults[i].FoundPlaces);
  for i := 0 to length(SearchResults)-1 do
    begin
      //calculate Similarity
      aSimilarity := 0;
      for a := 0 to length(SearchResults[i].FoundPlaces)-1 do
        aSimilarity := aSimilarity+SearchResults[i].FoundPlaces[a].Similarity;
      aSimilarity := aSimilarity / length(SearchResults[i].FoundPlaces);
      //60% Ergebnisgüte
      SearchResults[i].Similarity := (aSimilarity*60)/100;
      SearchResults[i].Similarity := SearchResults[i].Similarity+((40*length(SearchResults[i].FoundPlaces))/round(Max));
    end;
  for i := 0 to length(SearchResults)-1 do
    begin
      Max := 0;
      for a := 0 to length(SearchResults)-1 do
        if SearchResults[a].Similarity > Max then
          begin
            Max := SearchResults[a].Similarity;
            Idx := a;
            SearchResults[a].Similarity := 0;
          end;
      if Max > 0 then
        begin
          SearchResultFile.WriteString('<table border="0" width="100%"><colgroup><col width="6*"><col width="1*"></colgroup>');
          SearchResultFile.WriteString('<tr><td><big><p><a href="'+SearchResults[idx].Adress+'">'+SearchResults[idx].Title+'</a></big></td><td align=right>'+strMatch+' '+FormatFloat('0.0',Max)+' %</p></td>');
          for b := 0 to length(SearchResults[idx].FoundPlaces)-1 do
            SearchResultFile.WriteString('<tr><td><small>'+StringReplace(SearchResults[idx].FoundPlaces[b].text,eSearch.Text,'<b>'+eSearch.Text+'</b>',[rfReplaceAll])+'</small></td><td align=right><small>'+FormatFloat('0.0',SearchResults[idx].FoundPlaces[b].Similarity)+' %</small></td></tr>');
          SearchResultFile.WriteString('</table>');
        end;
    end;
  if length(SearchResults) = 0 then
    SearchResultFile.WriteString('<p>'+strNothingFound+'</p><br>');
  for i := 0 to length(SearchResults)-1 do
    Setlength(SearchResults[i].FoundPlaces,0);
  Setlength(SearchResults,0);
  SearchResultFile.WriteString(#10#13+'</body>'+#10#13+'</html>');
  SearchResultFile.Position := 0;
  NewHTML:=TSimpleIpHtml.Create; // Beware: Will be freed automatically by IpHtmlPanel1
  NewHTML.OnGetImageX:=@HTMLGetImageX;
  NewHTML.LoadFromStream(SearchResultFile);
  HelpViewer.SetHtml(NewHTML);
  SearchResultFile.Free;
end;

procedure TfHelpBrowser.FormCreate(Sender: TObject);
begin
  FhelpOffset := ChangeFileExt(ExtractFileName(Application.Exename),'');
  History := TStringList.Create;
  HistoryIndex := -1;
end;

procedure TfHelpBrowser.FormDestroy(Sender: TObject);
begin
  History.Free;
end;

procedure TfHelpBrowser.HtmlViewerHotClick(Sender: TObject);
var
  NodeA: TIpHtmlNodeA;
  NewFilename: String;
  realFn: String;
begin
  if HelpViewer.HotNode is TIpHtmlNodeA then
    begin
      NodeA:=TIpHtmlNodeA(HelpViewer.HotNode);
      NewFilename:=NodeA.HRef;
      if Uppercase(NodeA.Target) = '_BLANK' then
        begin
          if not FileExists(NewFilename) then
            realFn := OldPath+NewFileName
          else
            realFn := newFileName;
          realFN := StringReplace(realFN,'\',DirectorySeparator,[rfReplaceAll]);
          realFN := StringReplace(realFN,'/',DirectorySeparator,[rfReplaceAll]);
          realFN := Stringreplace(realFn,DirectorySeparator+DirectorySeparator,DirectorySeparator,[rfReplaceAll]);
          OpenURL(realFN);
        end
      else
        begin
          if HistoryIndex < History.Count-1 then
            while History.Count > HistoryIndex+1 do
              History.Delete(History.Count-1);
          OpenHelp(NewFilename);
          History.Add(NewFileName);
          HistoryIndex := History.Count-1;
        end;
      CheckButtons;
    end;
end;

procedure TfHelpBrowser.bBackClick(Sender: TObject);
begin
  dec(HistoryIndex);
  OpenHelp(History[HistoryIndex]);
  CheckButtons;
end;

procedure TfHelpBrowser.bHomeClick(Sender: TObject);
begin
  if HistoryIndex < History.Count-1 then
    while History.Count > HistoryIndex+1 do
      History.Delete(History.Count-1);
  OpenHelp(HomeURL);
  History.Add(HomeURL);
  HistoryIndex := History.Count-1;
  CheckButtons;
end;

procedure TfHelpBrowser.bNextClick(Sender: TObject);
begin
  inc(HistoryIndex);
  OpenHelp(History[HistoryIndex]);
  CheckButtons;
end;

procedure TfHelpBrowser.pToolBarClick(Sender: TObject);
begin

end;

procedure TfHelpBrowser.OpenHelp(const Filename: string);
var
  fs: TFileStream;
  NewHTML: TSimpleIpHtml;
  realFN : string;
begin
  if not FileExists(Filename) then
    realFn := OldPath+FileName
  else
    realFn := FileName;
  realFN := StringReplace(realFN,'\',DirectorySeparator,[rfReplaceAll]);
  realFN := StringReplace(realFN,'/',DirectorySeparator,[rfReplaceAll]);
  realFN := Stringreplace(realFn,DirectorySeparator+DirectorySeparator,DirectorySeparator,[rfReplaceAll]);
  OldPath := ExtractFilePath(realFn);
  if FileExistsUTF8(realFN) then
    begin
      fs:=TFileStream.Create(UTF8ToSys(realFn),fmOpenRead);
      try
        NewHTML:=TSimpleIpHtml.Create; // Beware: Will be freed automatically by IpHtmlPanel1
        NewHTML.OnGetImageX:=@HTMLGetImageX;
        NewHTML.LoadFromStream(fs);
      finally
        fs.Free;
      end;
      HelpViewer.SetHtml(NewHTML);
    end
  else
    begin
      if Assigned(FOnOpenPageFailed) then
        FOnOpenPageFailed(Self,Filename)
      else
        MessageDlg('Unable to open HTML file', 'HTML File: '+realFn,mtError,[mbCancel],0);
    end;
end;

procedure TfHelpBrowser.GotoHelp(Keyword: string);
begin
  if (not FileExists(ValidateFileName(KeyWord)+'.html'))
  and (not FileExists(OldPath+ValidateFileName(KeyWord)+'.html')) then
    exit;
  OpenHelp(ValidateFilename(Keyword)+'.html');
end;

procedure TfHelpBrowser.HTMLGetImageX(Sender: TIpHtmlNode; const URL: string;
  var Picture: TPicture);
var
  realFN : string;
begin
  try
    if Picture=nil then
      Picture:=TPicture.Create;
    realFN := SysToUTF8(URL);
    if not FileExists(realFN) then
      realFn := OldPath+URL
    else
      realFn := URL;
    Picture.LoadFromFile(realFN);
  except
  end;
end;

procedure TfHelpBrowser.CheckButtons;
begin
  bBack.Enabled := HistoryIndex > 0;
  bNext.Enabled := (History.Count-1) > HistoryIndex;
  bHome.Enabled := HomeUrl <> '';
end;

procedure TfHelpBrowser.SethelpOffset(const AValue: string);
begin
  if FHelpOffSet=AValue then exit;
  FHelpOffSet:=AValue;
end;

procedure TfHelpBrowser.SetOnOpenPageFailed(const AValue: TOpenPageFailedEvent
  );
begin
  if FOnOpenPageFailed=AValue then exit;
  FOnOpenPageFailed:=AValue;
end;

procedure TfHelpBrowser.SetLanguage(Lang: string);
begin
  Self.Caption := strHelp;
  lNoHelp.Caption := strNohelpavalible;
  Language := Lang;
  if Language = '' then
    Language := 'english';
  HelpViewer.Visible := True;
  History.Clear;
  if FileExists(AppendPathDelim(AppendPathDelim(ExtractFilePath(Application.Exename))+'help')+lowercase(FHelpOffset+'-help'+copy(Language,0,2))+'.html') then
    begin
      HomeURL :=AppendPathDelim(AppendPathDelim(ExtractFilePath(Application.Exename))+'help')+lowercase(FHelpOffset+'-help'+copy(Language,0,2))+'.html';
      OpenHelp(HomeURL);
      History.Add(HomeURL);
      if FileExists(AppendPathDelim(AppendPathDelim(ExtractFilePath(Application.Exename))+'help')+lowercase(FHelpOffset+'-helpreadme'+copy(Language,0,2))+'.html') then
        begin
          OpenHelp(AppendPathDelim(AppendPathDelim(ExtractFilePath(Application.Exename))+'help')+lowercase(FHelpOffset+'-helpreadme'+copy(Language,0,2))+'.html');
          History.Add(AppendPathDelim(AppendPathDelim(ExtractFilePath(Application.Exename))+'help')+lowercase(FHelpOffset+'-helpreadme'+copy(Language,0,2))+'.html');
        end;
    end
  else if FileExists(AppendPathDelim(AppendPathDelim(ExtractFilePath(Application.Exename))+'help')+lowercase(FHelpOffset+'-help')+'.html') then
    begin
      HomeURL :=     AppendPathDelim(AppendPathDelim(ExtractFilePath(Application.Exename))+'help')+lowercase(FHelpOffset+'-help')+'.html';
      OpenHelp(HomeURL);
      History.Add(HomeURL);
    end
  else
    HelpViewer.Visible := False;
  HistoryIndex := History.Count-1;
  CheckButtons;
end;

{ TSimpleIpHtml }

constructor TSimpleIpHtml.Create;
begin
  inherited Create;
end;

initialization
  {$I uhelp.lrs}

end.

