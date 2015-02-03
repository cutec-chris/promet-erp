unit uQuickHelpFrame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, IpHtml, Forms, Controls, StdCtrls,uWiki,
  Graphics,Dialogs, ExtCtrls,uImageCache,LCLIntf;

type

  { TfQuickHelpFrame }

  TfQuickHelpFrame = class(TFrame)
    Button1: TButton;
    Button2: TButton;
    ipHTML: TIpHtmlPanel;
    Timer1: TTimer;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    function FCacheGetFile(Path: string; var NewPath: string): TStream;
    procedure fQuickHelpFrameWikiInclude(Inp: string; var Outp: string;aLevel : Integer);
    procedure ipHTMLHotClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure TSimpleIpHtmlGetImageX(Sender: TIpHtmlNode; const URL: string;
      var Picture: TPicture);
  private
    { private declarations }
    UseID: String;
    nHeight: Integer;
    FRootPage : Variant;
    aLines : Integer;
    FCache: TFileCache;
    FActNode: TIpHtmlNode;
    function Wiki2HTML(input: string): TIPHtml;
    function FindFirstSection(aName : string;aFullText : TStrings) : String;
  protected
    procedure UpdateShowing; override;
    procedure ShowControl(AControl: TControl); override;
  public
    { public declarations }
    function OpenWikiPage(aWiki : TWikiList) : Boolean;
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

  { TSimpleIpHtml }

  TSimpleIpHtml = class(TIpHtml)
  public
    property OnGetImageX;
    constructor Create;
  end;
implementation
uses uData,WikiToHTML,Math,md5,uBaseApplication,uBaseDBInterface,uDocuments,
  Utils,uPrometFrames;
{$R *.lfm}
resourcestring
  strNeverShowAgain                             = 'Soll die Schnellhilfe nie wieder angezeigt werden ?'+lineending+'Wenn Sie nein Klicken werden bei der nächsten Sitzung wieder Schnellhilfen angezeigt.';
{ TSimpleIpHtml }

constructor TSimpleIpHtml.Create;
begin
  inherited Create;
end;

{ TfQuickHelpFrame }

procedure TfQuickHelpFrame.fQuickHelpFrameWikiInclude(Inp: string;
  var Outp: string; aLevel: Integer);
var
  aNewList: TWikiList;
begin
  aNewList := TWikiList.CreateEx(Self,Data);
  if pos('|',Inp) > 0 then Inp := copy(Inp,0,pos('|',Inp)-1);
  if aNewList.FindWikiPage(Inp) then
    begin
      Outp := Outp+WikiText2HTML(aNewList.FieldByName('DATA').AsString,'','',True,aLevel+1);
    end;
  aNewList.Free;
end;

procedure TfQuickHelpFrame.ipHTMLHotClick(Sender: TObject);
var
  PageName: String;
  aParent : Integer = TREE_ID_WIKI_UNSORTED;
  ID: Integer;
begin
  if ipHTML.HotNode is TIpHtmlNodeA then
    begin
      PageName := StringReplace(TIpHtmlNodeA(IpHtml.HotNode).HRef,' ','_',[rfReplaceAll]);
      if Data.GotoLink('WIKI@'+PageName) then
      else if (pos('@',PageName)>0) and Data.GotoLink(PageName) then
      else if ((Pos('://', TIpHtmlNodeA(IpHtml.HotNode).HRef) > 0) or (pos('www',lowercase(TIpHtmlNodeA(IpHtml.HotNode).HRef)) > 0)) then
        OpenURL(TIpHtmlNodeA(IpHtml.HotNode).HRef);
    end;
end;

procedure TfQuickHelpFrame.Timer1Timer(Sender: TObject);
begin
  if ipHTML.GetContentSize.cy > 0 then
    begin
      Timer1.Enabled:=False;
      nHeight := ipHTML.GetContentSize.cy;
      if nHeight < 75 then nHeight := 75;
      height := nHeight;
    end;
end;

procedure TfQuickHelpFrame.Button1Click(Sender: TObject);
var
  aSec: String;
  result: Boolean;
  aParent: Controls.TWinControl;
  aWiki: TWikiList;
begin
  with Application as IBaseDbInterface do
    DBConfig.WriteString(UseID,'NO');
  aWiki := TWikiList.Create(nil);
  aWiki.Select(FRootPage);
  aWiki.Open;
  if OpenWikiPage(aWiki) then
    UpdateShowing
  else
    begin
      Height := 1;
    end;
  aWiki.Free;
end;

procedure TfQuickHelpFrame.Button2Click(Sender: TObject);
begin
  if MessageDlg(strNeverShowAgain,mtInformation,[mbYes,mbNo],0) = mrYes then
    begin
      with Application as IBaseDbInterface do
        DBConfig.WriteString('QUICKHELP','NO');
    end
  else
    with Application as IBaseApplication do
      QuickHelp := False;
  Timer1.Enabled:=False;
  Height := 1;
end;

function TfQuickHelpFrame.FCacheGetFile(Path: string; var NewPath: string
  ): TStream;
var
  aPicture: TPicture;
  ms: TMemoryStream;
  Picture: TPicture;
  aDocument: TDocument;
  Aspect: real;
begin
  Result := nil;
  NewPath := Path;
  aDocument := TDocument.CreateEx(Self,Data);
  Data.SetFilter(aDocument,Data.QuoteField('TYPE')+'=''W'' and '+Data.QuoteField('NAME')+'='+Data.QuoteValue(copy(ExtractFileName(Path),0,rpos('.',ExtractFileName(Path))-1)),1);
  if aDocument.DataSet.RecordCount > 0 then
    begin
      ms := TMemoryStream.Create;
      Data.BlobFieldToStream(aDocument.DataSet,'DOCUMENT',ms);
      ms.Position:=0;
      if TIpHtmlNodeIMG(FActNode).Width.LengthType = hlAbsolute then
        begin
          try
            aPicture := TPicture.Create;
            aPicture.LoadFromStreamWithFileExt(ms,aDocument.FieldByName('EXTENSION').AsString);
            Picture := TPicture.Create;
            Picture.Bitmap.Width := TIpHtmlNodeIMG(FActNode).Width.LengthValue;
            Aspect := aPicture.Height/aPicture.Width;
            Picture.Bitmap.Height := round(TIpHtmlNodeIMG(FActNode).Width.LengthValue*Aspect);
            Picture.Bitmap.Canvas.AntialiasingMode:= amOn;
            Picture.Bitmap.Canvas.StretchDraw(Rect(0,0,Picture.Width,Picture.Height),aPicture.Graphic);
            aPicture.Free;
            ms.Free;
            ms := TMemoryStream.Create;
            Picture.SaveToStreamWithFileExt(ms,'png');
            NewPath := Copy(Path,0,length(path)-length(ExtractFileExt(Path)))+'.png';
            ms.Position:=0;
            Picture.Free;
          except
            on e : exception do
              begin
                ms.Free;
                ms := TMemoryStream.Create;
                Data.BlobFieldToStream(aDocument.DataSet,'DOCUMENT',ms);
                ms.Position:=0;
              end;
          end;
        end;
      Result := ms;
    end;
  aDocument.Free;
end;

procedure TfQuickHelpFrame.TSimpleIpHtmlGetImageX(Sender: TIpHtmlNode;
  const URL: string; var Picture: TPicture);
var
  aPicture: TPicture;
  NewURL : string;
  aFile: Classes.TMemoryStream;
begin
  FActNode := Sender;
  aFile := FCache.GetFile(URL,NewURL);
  if Assigned(aFile) then
    begin
      Picture := TPicture.Create;
      aFile.Position := 0;
      try
        Picture.LoadFromStreamWithFileExt(aFile,ExtractFileExt(NewURL));
      except
      end;
    end;
end;

function TfQuickHelpFrame.Wiki2HTML(input: string): TIPHtml;
var
  ss: TStringStream;
begin
  WikiToHTML.OnWikiInclude:=@fQuickHelpFrameWikiInclude;
  ss:=TStringStream.Create(UniToSys('<html><head></head><body bgcolor=#ffffcc>'+WikiText2HTML(input,'','',True)+'<br><br><br></body></html>'));
  ss.Position := 0;
  try
    Result:=TSimpleIPHtml.Create;
    TSimpleIPHtml(Result).OnGetImageX:=@TSimpleIpHtmlGetImageX;
    Result.LoadFromStream(ss);
  finally
    ss.Free;
  end;
end;

function TfQuickHelpFrame.FindFirstSection(aName: string; aFullText: TStrings
  ): String;
var
  UseThis: Boolean = False;
  i: Integer;
  aWiki: TWikiList;
  PageName: String;
begin
  aLines := 0;
  Result := '';
  for i := 0 to aFullText.Count-1 do
    begin
      if copy(trim(aFullText[i]),0,5)='===[[' then
        begin
          PageName := copy(trim(aFullText[i]),6,length(trim(aFullText[i])));
          PageName:=copy(PageName,0,pos(']]',PageName)-1);
          if pos('|',PageName)>0 then
            PageName:=copy(PageName,0,pos('|',PageName)-1);
          aWiki := TWikiList.Create(nil);
          if aWiki.FindWikiPage(PageName) then
            if OpenWikiPage(aWiki) then
              begin
                aWiki.Free;
                result := 'OK';
                exit;
              end;
          aWiki.Free;
        end
      else if copy(trim(aFullText[i]),0,3)='===' then
        begin
          if UseThis then
            begin
              break;
            end;
          Result := '';
          with Application as IBaseDbInterface do
            if DBConfig.ReadString('QHI:'+MD5Print(MD5String(aFullText[i]+aName)),'YES') <> 'NO'  then
              begin
                UseThis := True;
                UseID := 'QHI:'+MD5Print(MD5String(aFullText[i]+aName));
              end;
          continue;
        end;
      if UseThis then
        begin
          Result := Result+aFullText[i]+lineending;
          inc(aLines);
        end;
    end;
end;

procedure TfQuickHelpFrame.UpdateShowing;
begin
  inherited UpdateShowing;
  if Visible and (Height>1) then
    begin
      nHeight := (ipHTML.Canvas.TextHeight('A')+6)*aLines;
      if nHeight < 75 then nHeight := 75;
      Height := nHeight;
      Timer1.Enabled:=True;
    end;
end;

procedure TfQuickHelpFrame.ShowControl(AControl: TControl);
begin
  inherited ShowControl(AControl);
end;

function TfQuickHelpFrame.OpenWikiPage(aWiki : TWikiList): Boolean;
var
  aSec: String;
  aName: String;
  aFullText: TStringList;
begin
  with Application as IBaseDbInterface do
    if DBConfig.ReadString('QUICKHELP','YES') = 'NO'  then
      begin
        Result := False;
        exit;
      end;
  with Application as IBaseApplication do
    if not QuickHelp then
      begin
        Result := False;
        exit;
      end;
  aFullText := TStringList.Create;
  aName := aWiki.FieldByName('NAME').AsString;
  if FRootPage=0 then
    FRootPage:=aWiki.Id.AsVariant;
  aFullText.Text := aWiki.FieldByName('DATA').AsString;
  aSec := FindFirstSection(aName,aFullText);
  result := aSec<>'';
  if Result and (aSec<>'OK') then
    begin
      ipHTML.SetHtml(Wiki2HTML(aSec));
    end;
  FreeAndNil(aFullText);
end;

constructor TfQuickHelpFrame.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FCache := TFileCache.Create(30);
  FCache.OnGetFile:=@FCacheGetFile;
  Name := '';
  FRootPage:=0;
end;

destructor TfQuickHelpFrame.Destroy;
begin
  FCache.Destroy;
  inherited Destroy;
end;

end.

