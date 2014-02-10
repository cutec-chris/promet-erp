{ Copyright (C) Christian Ulrich info@cu-tec.de

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
Created 01.07.2006
*******************************************************************************}
unit uWikiFrame;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, FileUtil, Forms, Controls, ComCtrls, DbCtrls, Buttons,
  StdCtrls, ExtCtrls, IpHtml, db, uPrometFrames, uExtControls, Graphics,
  DBGrids, ActnList, Dialogs, Menus, uImageCache;
type
  THistory = class(TStringList)
  private
    FFFWdAction: TAction;
    FHIndex : Integer;
    FRewAction: TAction;
    DontAdd : Boolean;
    procedure SetIndex(const AValue: Integer);
  public
    constructor Create;
    function Add(const S: string): Integer; override;
    procedure Clear; override;
    procedure GoBack;
    procedure GoFwd;
    property HistoryIndex : Integer read FHIndex write SetIndex;
    property FwdAction : TAction read FFFWdAction write FFFWdAction;
    property RewAction : TAction read FRewAction write FRewAction;
  end;
  TfWikiFrame = class(TPrometMainFrame)
    acBack: TAction;
    acForward: TAction;
    acIndex: TAction;
    ActionList: TActionList;
    bItalic: TSpeedButton;
    DBText1: TDBText;
    dnEdit: TDBNavigator;
    eTitle: TDBEdit;
    eWikiPage: TDBMemo;
    Label3: TLabel;
    lTitle: TLabel;
    MenuItem1: TMenuItem;
    Panel1: TPanel;
    pmHistory: TPopupMenu;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    SpeedButton4: TSpeedButton;
    SpeedButton6: TSpeedButton;
    SpeedButton7: TSpeedButton;
    Wiki: TDatasource;
    ipHTML: TIpHtmlPanel;
    Panel4: TPanel;
    pcPages: TExtMenuPageControl;
    pToolbar: TPanel;
    tbMenue: TToolButton;
    tbToolBar: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    tsEdit: TTabSheet;
    tsView: TTabSheet;
    procedure acBackExecute(Sender: TObject);
    procedure acForwardExecute(Sender: TObject);
    procedure acIndexExecute(Sender: TObject);
    procedure bItalicClick(Sender: TObject);
    function FCacheGetFile(Path: string): TStream;
    procedure ipHTMLHotClick(Sender: TObject);
    procedure OpenHistoryItemClick(Sender: TObject);
    procedure pmHistoryPopup(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure SpeedButton3Click(Sender: TObject);
    procedure SpeedButton4Click(Sender: TObject);
    procedure TSimpleIpHtmlGetImageX(Sender: TIpHtmlNode; const URL: string;
      var Picture: TPicture);
    procedure tsViewShow(Sender: TObject);
    procedure WikiStateChange(Sender: TObject);
  private
    { private declarations }
    FHistory : THistory;
    FCache: TFileCache;
    FActNode: TIpHtmlNode;
    function Wiki2HTML(input: string): TIPHtml;
    procedure AddDocuments(Sender: TObject);
  public
    { public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy;override;
    function OpenFromLink(aLink : string) : Boolean;override;
    procedure New;override;
    procedure SetLanguage;override;
    function OpenWikiPage(PageName : string;CreateIfNotExists : Boolean = False) : Boolean;
  end;
  TSimpleIpHtml = class(TIpHtml)
  public
    property OnGetImageX;
    constructor Create;
  end;
implementation
uses uWiki,uData,WikiToHTML,uDocuments,Utils,LCLIntf,Variants,uDocumentFrame,
  uBaseDbInterface;
procedure THistory.SetIndex(const AValue: Integer);
begin
  FHIndex := AValue;
  FRewAction.Enabled := FHIndex > 0;
  FFFWdAction.Enabled := FHIndex < Count-1;
  FRewAction.Enabled := FHIndex > 0;
  FFFWdAction.Enabled := True;
  DontAdd := True;
  Data.GotoLink(Strings[FHIndex]);
  DontAdd := False;
end;
constructor THistory.Create;
begin
  DontAdd := False;
end;
function THistory.Add(const S: string): Integer;
begin
  if S = '' then exit;
  if DontAdd then exit;
  if Count > 0 then
    if s = Strings[Count-1] then exit;
  Result:=inherited Add(S);
  FRewAction.Enabled := True;
  FFFWdAction.Enabled := False;
  FHIndex := Result;
end;
procedure THistory.Clear;
begin
  inherited Clear;
  FRewAction.Enabled := false;
  FFFWdAction.Enabled := false;
  FHIndex := -1;
end;
procedure THistory.GoBack;
begin
  if FHIndex = 0 then exit;
  dec(FHIndex);
  FRewAction.Enabled := FHIndex > 0;
  FFFWdAction.Enabled := FHIndex < Count-1;
  FRewAction.Enabled := FHIndex > 0;
  FFFWdAction.Enabled := True;
  DontAdd := True;
  Data.GotoLink(Strings[FHIndex]);
  DontAdd := False;
end;
procedure THistory.GoFwd;
begin
  if FHIndex >= Count-1 then exit;
  inc(FHIndex);
  FRewAction.Enabled := FHIndex > 0;
  FFFWdAction.Enabled := FHIndex < Count-1;
  FRewAction.Enabled := True;
  FFFWdAction.Enabled := FHIndex < Count-1;
  DontAdd := True;
  Data.GotoLink(Strings[FHIndex]);
  DontAdd := False;
end;
constructor TSimpleIpHtml.Create;
begin
  inherited Create;
end;
constructor TfWikiFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCache := TFileCache.Create(30);
  FCache.OnGetFile:=@FCacheGetFile;
  DataSet := TWikiList.Create(Self,Data);
  FHistory := THistory.Create;
  FHistory.FFFWdAction := acForward;
  FHistory.RewAction := acBack;
  Wiki.DataSet := DataSet.DataSet;
end;
destructor TfWikiFrame.Destroy;
begin
  FCache.Destroy;
  FHistory.Destroy;
  DataSet.Destroy;
  DataSet := nil;
  inherited Destroy;
end;
function TfWikiFrame.OpenFromLink(aLink: string) : Boolean;
begin
  if not (copy(aLink,0,pos('@',aLink)-1) = 'WIKI') then exit;
  if rpos('{',aLink) > 0 then
    aLink := copy(aLink,0,rpos('{',aLink)-1)
  else if rpos('(',aLink) > 0 then
    aLink := copy(aLink,0,rpos('(',aLink)-1);
  Result := OpenWikiPage(copy(aLink, pos('@', aLink) + 1, length(aLink)));
end;
procedure TfWikiFrame.TSimpleIpHtmlGetImageX(Sender: TIpHtmlNode;
  const URL: string; var Picture: TPicture);
var
  aDocument: TDocument;
  ms: TMemoryStream;
  aPicture: TPicture;
  Aspect: real;
  aFile: TMemoryStream;
begin
  FActNode := Sender;
  aFile := FCache.GetFile(URL);
  if Assigned(aFile) then
    begin
      Picture := TPicture.Create;
      aFile.Position := 0;
      Picture.LoadFromStreamWithFileExt(aFile,ExtractFileExt(URL));
    end;
end;
procedure TfWikiFrame.tsViewShow(Sender: TObject);
begin
  if (not Assigned(DataSet)) or (not DataSet.DataSet.Active) then exit;
  ipHTML.SetHtml(Wiki2HTML(DataSet.FieldByName('DATA').AsString));
end;
procedure TfWikiFrame.WikiStateChange(Sender: TObject);
begin
  if DataSet.DataSet.State = dsEdit then
    tsEdit.Show;
end;
procedure TfWikiFrame.ipHTMLHotClick(Sender: TObject);
var
  PageName: String;
  aParent : Integer = TREE_ID_WIKI_UNSORTED;
  ID: Integer;
begin
  if ipHTML.HotNode is TIpHtmlNodeA then
    begin
      PageName := StringReplace(TIpHtmlNodeA(IpHtml.HotNode).HRef,' ','_',[rfReplaceAll]);
      if OpenWikiPage(PageName) then
      else if ((Pos('://', TIpHtmlNodeA(IpHtml.HotNode).HRef) > 0) or (pos('www',lowercase(TIpHtmlNodeA(IpHtml.HotNode).HRef)) > 0)) then
        OpenURL(TIpHtmlNodeA(IpHtml.HotNode).HRef)
      else
        begin
          OpenWikiPage(PageName,True);
        end;
    end;
end;
procedure TfWikiFrame.OpenHistoryItemClick(Sender: TObject);
begin
  FHistory.HistoryIndex:=TMenuItem(Sender).Tag;
end;
procedure TfWikiFrame.pmHistoryPopup(Sender: TObject);
var
  aItem: TMenuItem;
  i: Integer;
begin
  pmHistory.Items.Clear;
  for i := FHistory.Count-1 downto 0 do
    begin
      aItem := TMenuItem.Create(nil);
      aItem.Caption:=Data.GetLinkDesc(FHistory[i]);
      aItem.OnClick:=@OpenHistoryItemClick;
      if i = FHistory.HistoryIndex then
        aItem.Default := True;
      aItem.Tag := i;
      pmHistory.Items.Add(aItem);
    end;
end;
procedure TfWikiFrame.SpeedButton2Click(Sender: TObject);
begin
  if (DataSet.DataSet.State <> dsEdit)
  and (DataSet.DataSet.State <> dsInsert) then
    DataSet.DataSet.Edit;
  eWikiPage.SelText:=''''''''+eWikiPage.SelText+'''''''';
  eWikiPage.SelStart:=eWikiPage.SelStart+eWikiPage.SelLength;
end;
procedure TfWikiFrame.SpeedButton3Click(Sender: TObject);
begin
  if (DataSet.DataSet.State <> dsEdit)
  and (DataSet.DataSet.State <> dsInsert) then
    DataSet.DataSet.Edit;
  eWikiPage.SelText := '[http://]';
  eWikiPage.SelStart:=eWikiPage.SelStart+8;
end;
procedure TfWikiFrame.SpeedButton4Click(Sender: TObject);
begin
  if (DataSet.DataSet.State <> dsEdit)
  and (DataSet.DataSet.State <> dsInsert) then
    DataSet.DataSet.Edit;
  eWikiPage.SelText := '[[]]';
  eWikiPage.SelStart:=eWikiPage.SelStart+2;
end;
procedure TfWikiFrame.acBackExecute(Sender: TObject);
begin
  FHistory.GoBack;
end;
procedure TfWikiFrame.acForwardExecute(Sender: TObject);
begin
  FHistory.GoFwd;
end;
procedure TfWikiFrame.acIndexExecute(Sender: TObject);
begin
  OpenWikiPage('INDEX');
end;
procedure TfWikiFrame.bItalicClick(Sender: TObject);
begin
  if (DataSet.DataSet.State <> dsEdit)
  and (DataSet.DataSet.State <> dsInsert) then
    DataSet.DataSet.Edit;
  eWikiPage.SelText:=''''''+eWikiPage.SelText+'''''';
  eWikiPage.SelStart:=eWikiPage.SelStart+eWikiPage.SelLength;
end;
function TfWikiFrame.FCacheGetFile(Path: string): TStream;
var
  aPicture: TPicture;
  ms: TMemoryStream;
  Picture: TPicture;
  aDocument: TDocument;
  Aspect: real;
begin
  Result := nil;
  aDocument := TDocument.Create(Self,Data);
  Data.SetFilter(aDocument,Data.QuoteField('TYPE')+'=''W'' and '+Data.QuoteField('NAME')+'='+Data.QuoteValue(copy(ExtractFileName(Path),0,rpos('.',ExtractFileName(Path))-1)),1);
  if aDocument.DataSet.RecordCount > 0 then
    begin
      ms := TMemoryStream.Create;
      Data.BlobFieldToStream(aDocument.DataSet,'DOCUMENT',ms);
      ms.Position:=0;
      if TIpHtmlNodeIMG(FActNode).Width.LengthType = hlAbsolute then
        begin
          aPicture := TPicture.Create;
          aPicture.LoadFromStreamWithFileExt(ms,aDocument.FieldByName('EXTENSION').AsString);
          Picture := TPicture.Create;
          Picture.Bitmap.Width := TIpHtmlNodeIMG(FActNode).Width.LengthValue;
          Aspect := aPicture.Height/aPicture.Width;
          Picture.Bitmap.Height := round(TIpHtmlNodeIMG(FActNode).Width.LengthValue*Aspect);
          Picture.Bitmap.Canvas.StretchDraw(Rect(0,0,Picture.Width,Picture.Height),aPicture.Graphic);
          aPicture.Free;
          ms.Free;
          ms := TMemoryStream.Create;
          Picture.SaveToStreamWithFileExt(ms,aDocument.FieldByName('EXTENSION').AsString);
          ms.Position:=0;
        end;
      Result := ms;
    end;
  aDocument.Free;
end;
function TfWikiFrame.Wiki2HTML(input: string): TIPHtml;
var
  ss: TStringStream;
begin
  ss:=TStringStream.Create(UTF8ToSys('<html><head><title>'+DataSet.FieldByName('CAPTION').AsString+'</title></head><body>'+WikiText2HTML(input,'','',True)+'<br><br><br></body></html>'));
  ss.Position := 0;
  try
    Result:=TSimpleIPHtml.Create;
    TSimpleIPHtml(Result).OnGetImageX:=@TSimpleIpHtmlGetImageX;
    Result.LoadFromStream(ss);
  finally
    ss.Free;
  end;
end;
procedure TfWikiFrame.AddDocuments(Sender: TObject);
var
  aDocuments: TDocuments;
begin
  if not Assigned(TfDocumentFrame(Sender).DataSet) then
    begin
      aDocuments := TDocuments.Create(Self,Data);
      TfDocumentFrame(Sender).DataSet := aDocuments;
      TfDocumentFrame(Sender).Refresh(DataSet.Id.AsVariant,'W',DataSet.FieldByName('NAME').AsString,Null,Null);
    end;
end;
procedure TfWikiFrame.New;
begin
end;
procedure TfWikiFrame.SetLanguage;
begin
end;
function TfWikiFrame.OpenWikiPage(PageName: string;CreateIfNotExists : Boolean = False) : Boolean;
var
  aParent : Integer = 0;
  aDocuments: TDocuments;
  aDocFrame: TfDocumentFrame;
  aPageIndex: Integer;
  aDocPage: TTabSheet;
begin
  Result := TWikiList(DataSet).FindWikiPage(pageName);
  aParent := TWikiList(DataSet).ActiveTreeID;
  pcPages.ClearTabClasses;
  pcPages.AddTabClass(TfDocumentFrame,strFiles,@AddDocuments);
  if Result then
    begin
      tsView.Show;
      Screen.Cursor := crHourglass;
      ipHTML.SetHtml(Wiki2HTML(DataSet.FieldByName('DATA').AsString));
      while pcPages.PageCount > 3 do
        pcPages.Pages[2].Destroy;
      if (FDataSet.State <> dsInsert) and (fDataSet.Count > 0) then
        begin
          aDocuments := TDocuments.Create(Self,Data);
          aDocuments.CreateTable;
          aDocuments.Select(DataSet.Id.AsVariant ,'W',DataSet.FieldByName('NAME').AsString,Null,Null);
          aDocuments.Open;
          if aDocuments.Count = 0 then
            aDocuments.Free
          else
            begin
              aDocPage := pcPages.GetTab(TfDocumentFrame);
              if Assigned(aDocPage) then
                begin
                  aDocFrame := TfDocumentFrame(aDocPage.Controls[0]);
                  aDocFrame.DataSet := aDocuments;
                end
              else
                begin
                  aDocFrame := TfDocumentFrame.Create(Self);
                  aDocFrame.DataSet := aDocuments;
                  aPageIndex := pcPages.AddTab(aDocFrame,False);
                  pcPages.Visible:=False;
                  tsEdit.PageIndex:=1;
                  pcPages.ActivePage := tsView;
                  pcPages.Visible:=True;
                end;
            end;
        end;
      Screen.Cursor := crDefault;
      FHistory.Add(Data.BuildLink(DataSet.DataSet));
    end
  else if CreateIfNotExists then
    begin
      DataSet.DataSet.Insert;
      DataSet.FieldByName('NAME').AsString := copy(PageName,rpos('/',PageName)+1,length(PageName));
      if aParent = 0 then
        aParent := TREE_ID_WIKI_UNSORTED;
      DataSet.FieldByName('TREEENTRY').AsInteger := aParent;
      try
        tsEdit.Show;
      except
      end;
   end;
end;
{$R *.lfm}
end.

