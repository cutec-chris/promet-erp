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
Created 01.07.2006
*******************************************************************************}
unit uWikiFrame;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, FileUtil, Forms, Controls, ComCtrls, DbCtrls, Buttons,
  StdCtrls, ExtCtrls, IpHtml, db, uPrometFrames, uExtControls, Graphics,
  DBGrids, ActnList, Dialogs, Menus, uImageCache, uBaseDbClasses, LCLProc,
  Clipbrd, contnrs,Aspell;
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

  { TfWikiFrame }

  TfWikiFrame = class(TPrometMainFrame)
    acBack: TAction;
    acForward: TAction;
    acIndex: TAction;
    acScreenshot: TAction;
    acImage: TAction;
    acSpellCheck: TAction;
    acExport: TAction;
    acEdit: TAction;
    acRefresh: TAction;
    ActionList: TActionList;
    Bevel1: TBevel;
    Bevel2: TBevel;
    bItalic: TSpeedButton;
    bTransfer: TSpeedButton;
    ExtRotatedLabel1: TExtRotatedLabel;
    ExtRotatedLabel2: TExtRotatedLabel;
    Keywords: TDatasource;
    DBGrid1: TDBGrid;
    DBText1: TDBText;
    dnEdit: TDBNavigator;
    eTitle: TDBEdit;
    eWikiPage: TDBMemo;
    Label1: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    lTitle: TLabel;
    MenuItem1: TMenuItem;
    Panel1: TPanel;
    pEdit2: TPanel;
    pMiddle: TPanel;
    pLeft: TPanel;
    Panel3: TPanel;
    pmHistory: TPopupMenu;
    pToolbar1: TPanel;
    pEdit1: TPanel;
    SaveDialog1: TSaveDialog;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    SpeedButton4: TSpeedButton;
    SpeedButton6: TSpeedButton;
    sbSpellcheck: TSpeedButton;
    SpeedButton8: TSpeedButton;
    SpeedButton9: TSpeedButton;
    RefreshTimer: TTimer;
    tbMenue1: TToolButton;
    tbToolBar1: TToolBar;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TSpeedButton;
    ToolButton6: TSpeedButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    Wiki: TDatasource;
    ipHTML: TIpHtmlPanel;
    pTop: TPanel;
    pcPages: TExtMenuPageControl;
    pToolbar: TPanel;
    tbMenue: TToolButton;
    tbToolBar: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    tsEdit: TTabSheet;
    procedure acBackExecute(Sender: TObject);
    procedure acEditExecute(Sender: TObject);
    procedure acExportExecute(Sender: TObject);
    procedure acForwardExecute(Sender: TObject);
    procedure acImageExecute(Sender: TObject);
    procedure acIndexExecute(Sender: TObject);
    procedure acRefreshExecute(Sender: TObject);
    procedure acScreenshotExecute(Sender: TObject);
    procedure acSpellCheckExecute(Sender: TObject);
    procedure bItalicClick(Sender: TObject);
    function FCacheGetFile(Path: string;var NewPath : string): TStream;
    procedure fWikiFrameWikiInclude(Inp: string; var Outp: string; aLevel: Integer=0
      );
    procedure ipHTMLHotClick(Sender: TObject);
    procedure OpenHistoryItemClick(Sender: TObject);
    procedure pmHistoryPopup(Sender: TObject);
    procedure RefreshTimerTimer(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure SpeedButton3Click(Sender: TObject);
    procedure SpeedButton4Click(Sender: TObject);
    procedure SpeedButton9Click(Sender: TObject);
    procedure TSimpleIpHtmlGetImageX(Sender: TIpHtmlNode; const URL: string;
      var Picture: TPicture);
    procedure tsViewShow(Sender: TObject);
    procedure WikiDataChange(Sender: TObject; Field: TField);
    procedure WikiStateChange(Sender: TObject);
  private
    { private declarations }
    FHistory : THistory;
    FCache: TFileCache;
    FActNode: TIpHtmlNode;
    FEditable: Boolean;
    FVariables: TStrings;
    aDataThere : Boolean;
    function GetLeftBar: Boolean;
    procedure SetLeftBar(AValue: Boolean);
    function Wiki2HTML(input: string): TIPHtml;
    procedure AddDocuments(Sender: TObject);
    procedure DoView;
    procedure DoEdit;
  public
    { public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy;override;

    function CanHandleLink(aLink : string): Boolean; override;
    function OpenFromLink(aLink : string) : Boolean;override;
    procedure New;override;

    procedure SetLanguage;override;
    procedure SetRights(Editable : Boolean);
    function OpenWikiPage(PageName : string;CreateIfNotExists : Boolean = False) : Boolean;
    procedure Refresh;
    procedure ShowFrame;override;
    procedure DoRefresh; override;
    property Variables : TStrings read FVariables;
    property LeftBar : Boolean read GetLeftBar write SetLeftBar;
  end;
  TSimpleIpHtml = class(TIpHtml)
  public
    property OnGetImageX;
    constructor Create;
  end;
implementation
uses uWiki,uData,WikiToHTML,uDocuments,Utils,LCLIntf,Variants,
  uBaseDbInterface,uscreenshotmain,uMessages,uDocumentFrame,sqlparser,
  sqlscanner, sqltree,uBaseVisualApplication,uStatistic,uspelling,uBaseApplication,
  uBaseVisualControls,uRTFtoTXT,uIntfStrConsts;
procedure THistory.SetIndex(const AValue: Integer);
begin
  Move(AValue,Count-1);
  FHIndex := Count-1;
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
  while FHIndex<Count-1 do
    Delete(Count-1);
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
  FVariables := TStringList.Create;
  FVariables.Values['USER'] := Data.Users.Id.AsString;
  FVariables.Values['ACCOUNTNO'] := Data.Users.FieldByName('ACCOUNTNO').AsString;
  FVariables.Values['IDCODE'] := Data.Users.FieldByName('IDCODE').AsString;
  FVariables.Values['GROUPID'] := Data.Users.FieldByName('PARENT').AsString;
  FEditable:=False;
  DataSet := TWikiList.CreateEx(Self,Data);
  FHistory := THistory.Create;
  FHistory.FFFWdAction := acForward;
  FHistory.RewAction := acBack;
  Wiki.DataSet := DataSet.DataSet;
  Keywords.DataSet := TWikiList(DataSet).Keywords.DataSet;
  {$ifdef DARWIN}
  ipHTML.DefaultFontSize:=14;
  {$endif}
end;
destructor TfWikiFrame.Destroy;
begin
  FCache.Destroy;
  FHistory.Destroy;
  FVariables.Free;
  try
    DataSet.Destroy;
    DataSet := nil;
  except
  end;
  try
  inherited Destroy;
  except
  end;
end;

function TfWikiFrame.CanHandleLink(aLink: string): Boolean;
begin
  Result := (copy(aLink,0,pos('@',aLink)-1) = 'WIKI');
end;

function TfWikiFrame.OpenFromLink(aLink: string) : Boolean;
begin
  if not CanHandleLink(aLink) then exit;
  if rpos('{',aLink) > 0 then
    aLink := copy(aLink,0,rpos('{',aLink)-1)
  else if rpos('(',aLink) > 0 then
    aLink := copy(aLink,0,rpos('(',aLink)-1);
  Result := OpenWikiPage(copy(aLink, pos('@', aLink) + 1, length(aLink)),Data.Users.Rights.Right('WIKI')>RIGHT_READ);
end;
procedure TfWikiFrame.TSimpleIpHtmlGetImageX(Sender: TIpHtmlNode;
  const URL: string; var Picture: TPicture);
var
  aPicture: TPicture = nil;
  aFile: TMemoryStream = nil;
  NewURL : string = '';
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
        FreeAndNil(Picture);
      end;
    end;
end;
procedure TfWikiFrame.tsViewShow(Sender: TObject);
begin
  Refresh;
end;

procedure TfWikiFrame.WikiDataChange(Sender: TObject; Field: TField);
begin
  DataSet.Change;
end;

procedure TfWikiFrame.WikiStateChange(Sender: TObject);
begin
  if DataSet.DataSet.State = dsEdit then
    DoEdit;
end;
procedure TfWikiFrame.ipHTMLHotClick(Sender: TObject);
var
  PageName: String;
  aParent : Integer = TREE_ID_WIKI_UNSORTED;
  ID: Integer;
  i: Integer;
  aLink: String;
begin
  if Assigned(IpHtml.HotNode) and (ipHTML.HotNode is TIpHtmlNodeA) then
    begin
      aLink := TIpHtmlNodeA(IpHtml.HotNode).HRef;
      PageName := StringReplace(aLink,' ','_',[rfReplaceAll]);
      for i := 0 to FVariables.Count-1 do
        pageName := StringReplace(PageName,'@VARIABLES.'+FVariables.Names[i]+'@',FVariables.ValueFromIndex[i],[rfReplaceAll,rfIgnoreCase]);
      if OpenWikiPage(PageName) or OpenWikiPage(lowercase(PageName)) then
      else if (pos('@',PageName)>0) and Data.GotoLink(PageName) then
        begin
        end
      else if ((Pos('://', aLink) > 0) or (pos('www',lowercase(aLink)) > 0)) then
        OpenURL(aLink)
      else if (pos('@',PageName) = 0) and FEditable then
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
      aItem := TMenuItem.Create(pmHistory);
      aItem.Caption:=Data.GetLinkDesc(FHistory[i]);
      aItem.OnClick:=@OpenHistoryItemClick;
      if i = FHistory.HistoryIndex then
        aItem.Default := True;
      aItem.Tag := i;
      pmHistory.Items.Add(aItem);
    end;
end;

procedure TfWikiFrame.RefreshTimerTimer(Sender: TObject);
begin
  RefreshTimer.Enabled:=False;
  Refresh;
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
  if Data.Users.Rights.Right('WIKI')<=RIGHT_READ then exit;
  if (DataSet.DataSet.State <> dsEdit)
  and (DataSet.DataSet.State <> dsInsert) then
    DataSet.DataSet.Edit;
  eWikiPage.SelText := '[http://]';
  eWikiPage.SelStart:=eWikiPage.SelStart+length(eWikiPage.SelText)-1;
end;
procedure TfWikiFrame.SpeedButton4Click(Sender: TObject);
begin
  if Data.Users.Rights.Right('WIKI')<=RIGHT_READ then exit;
  if (DataSet.DataSet.State <> dsEdit)
  and (DataSet.DataSet.State <> dsInsert) then
    DataSet.DataSet.Edit;
  eWikiPage.SelText := '[[]]';
  eWikiPage.SelStart:=eWikiPage.SelStart+2;
end;

procedure TfWikiFrame.SpeedButton9Click(Sender: TObject);
var
  Stream: TStringStream;
begin
  if Data.Users.Rights.Right('WIKI')<=RIGHT_READ then exit;
  if Clipboard.HasFormat(LinkClipboardFormat) then
    begin
      Stream := TStringstream.Create('');
      if Clipboard.GetFormat(LinkClipboardFormat,Stream) then
        begin
          if (DataSet.DataSet.State <> dsEdit)
          and (DataSet.DataSet.State <> dsInsert) then
            DataSet.DataSet.Edit;
          if pos('{',Stream.DataString)>0 then
            eWikiPage.SelText := '[['+copy(Stream.DataString,0,pos('{',Stream.DataString)-1)+'|'+Data.GetLinkDesc(Stream.DataString)+']]'
          else
            eWikiPage.SelText := '[['+Stream.DataString+']]';
          eWikiPage.SelStart:=eWikiPage.SelStart+2;
        end;
      Stream.Free;
    end;
end;

procedure TfWikiFrame.acBackExecute(Sender: TObject);
begin
  FHistory.GoBack;
end;

procedure TfWikiFrame.acEditExecute(Sender: TObject);
begin
  if acEdit.Checked then
    DoEdit
  else DoView;
end;

procedure TfWikiFrame.acExportExecute(Sender: TObject);
var
  aFN: String;
begin
  if SaveDialog1.Execute then
    begin
      aFN := SaveDialog1.FileName;
      if lowercase(ExtractFileExt(aFN))<>'.html' then
        aFN := aFN+'.html';
      TWikiList(DataSet).ExportToHTML(aFN,OnWikiInclude);
    end;
end;

procedure TfWikiFrame.acForwardExecute(Sender: TObject);
begin
  FHistory.GoFwd;
end;
procedure TfWikiFrame.acImageExecute(Sender: TObject);
begin
  if Data.Users.Rights.Right('WIKI')<=RIGHT_READ then exit;
  if (DataSet.DataSet.State <> dsEdit)
  and (DataSet.DataSet.State <> dsInsert) then
    DataSet.DataSet.Edit;
  eWikiPage.SelText := '[[Bild:]]';
  eWikiPage.SelStart:=eWikiPage.SelStart+length(eWikiPage.SelText)-2;
end;
procedure TfWikiFrame.acIndexExecute(Sender: TObject);
begin
  OpenWikiPage('INDEX');
end;

procedure TfWikiFrame.acRefreshExecute(Sender: TObject);
begin
  Refresh;
end;

procedure TfWikiFrame.acScreenshotExecute(Sender: TObject);
var
  aDocuments: TDocuments;
  aDocument: TDocument;
  aDocPage: TTabSheet;
  aName : string = 'screenshot.jpg';
  aDocFrame: TfDocumentFrame;
  aPageIndex: Integer;
begin
  if Data.Users.Rights.Right('WIKI')<=RIGHT_READ then exit;
  Application.ProcessMessages;
  Application.MainForm.Hide;
  Application.ProcessMessages;
  aName := InputBox(strScreenshotName, strEnterAnName, aName);
  Application.ProcessMessages;
  Application.CreateForm(TfScreenshot,fScreenshot);
  with BaseApplication as IBaseApplication do
    fScreenshot.SaveTo:=AppendPathDelim(GetInternalTempDir)+aName;
  fScreenshot.Show;
  while fScreenshot.Visible do Application.ProcessMessages;
  fScreenshot.Destroy;
  fScreenshot := nil;
  if DataSet.State=dsInsert then
    begin
      DataSet.Post;
      DataSet.Edit;
    end;
  aDocument := TDocument.CreateEx(Self,Data);
  aDocument.Select(DataSet.Id.AsVariant ,'W',DataSet.FieldByName('NAME').AsString,Null,Null);
  with BaseApplication as IBaseApplication do
    aDocument.AddFromFile(AppendPathDelim(GetInternalTempDir)+aName);
  aDocument.Free;
  aDocuments := TDocuments.CreateEx(Self,Data);
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
          DoView;
        end;
    end;
  if (DataSet.DataSet.State <> dsEdit)
  and (DataSet.DataSet.State <> dsInsert) then
    DataSet.DataSet.Edit;
  eWikiPage.SelText := '[[Bild:'+aName+']]';
  eWikiPage.SelStart:=eWikiPage.SelStart+length(eWikiPage.SelText);

  Application.MainForm.Show;
end;

procedure TfWikiFrame.acSpellCheckExecute(Sender: TObject);
begin
  SetFocus;
  eWikiPage.SetFocus;
  fSpellCheck.Execute(eWikiPage,eWikiPage.SelStart);
end;

procedure TfWikiFrame.bItalicClick(Sender: TObject);
begin
  if (DataSet.DataSet.State <> dsEdit)
  and (DataSet.DataSet.State <> dsInsert) then
    DataSet.DataSet.Edit;
  eWikiPage.SelText:=''''''+eWikiPage.SelText+'''''';
  eWikiPage.SelStart:=eWikiPage.SelStart+eWikiPage.SelLength;
end;
function TfWikiFrame.FCacheGetFile(Path: string;var NewPath : string): TStream;
var
  aPicture: TPicture;
  ms: TMemoryStream;
  Picture: TPicture;
  aDocument: TDocument;
  Aspect: real;
  aNumber: integer;
  tmp: String;
begin
  Result := nil;
  NewPath := Path;
  if copy(uppercase(Path),0,5)='ICON(' then
    begin
      if TryStrToInt(copy(Path,6,length(Path)-6),aNumber) then
        begin
          ms := TMemoryStream.Create;
          Picture := TPicture.Create;
          fVisualControls.Images.GetBitmap(aNumber,Picture.Bitmap);
          Picture.SaveToStreamWithFileExt(ms,'png');
          NewPath := Copy(Path,0,length(path)-length(ExtractFileExt(Path)))+'.png';
          ms.Position:=0;
          Result := ms;
          Picture.Free;
        end;
    end
  else if copy(uppercase(Path),0,12)='HISTORYICON(' then
    begin
      tmp := copy(Path,13,length(Path)-13);
      if TryStrToInt(tmp,aNumber) then
        begin
          ms := TMemoryStream.Create;
          Picture := TPicture.Create;
          fVisualControls.HistoryImages.GetBitmap(aNumber,Picture.Bitmap);
          Picture.SaveToStreamWithFileExt(ms,'png');
          NewPath := Copy(Path,0,length(path)-length(ExtractFileExt(Path)))+'.png';
          ms.Position:=0;
          Result := ms;
          Picture.Free;
        end;
    end
  else
    begin
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
end;
type
  TPlainProcedure = procedure;
procedure TfWikiFrame.fWikiFrameWikiInclude(Inp: string; var Outp: string;aLevel : Integer = 0);
var
  aList: TMessageList;
  aMessage: TMessage;
  aCount : Integer;
  ss: TStringStream;
  aNewList: TWikiList;
  FSQLStream: TStringStream;
  FSQLScanner: TSQLScanner;
  FSQLParser: TSQLParser;
  bStmt: TSQLElement;
  aTableName: TSQLStringType;
  aClass: TBaseDBDatasetClass;
  aDs: TBaseDbDataSet;
  aFilter: TSQLStringType;
  aRight: String;
  aLimit: Integer = 10;
  i: Integer;
  aLimitS: String;
  aElem: TSQLElement;
  aName: TSQLStringType;
  aStatistic: TStatistic;
  aSQL: String;
  aRDs: TDataSet = nil;
  tmp: String;
  IncHeader: Boolean;
  aConn: TComponent = nil;
  aConditionOK : Boolean = True;
  aCondition: String = '';
  aTmpFloat: Extended;
  IsForm: Boolean;
  aInclude: String;
  nInp: String;
  ConvertRTF: Boolean = False;
  procedure BuildLinkRow(aBDS : TDataSet);
  var
    aLink: String;
  begin
    aLink := Data.BuildLink(aBDS);
    Outp+='<li><a href="'+aLink+'" title="'+Data.GetLinkDesc(aLink)+#10+Data.GetLinkLongDesc(aLink)+'">'+HTMLEncode(Data.GetLinkDesc(aLink))+'</a></li>';
  end;
  function BuildTableRow(aBDS : TDataSet;aStmt : TSQLElement) : string;
  var
    aLink: String;
    i: Integer;
    a: Integer;
    aName: TSQLStringType;
    aElem: TSQLElement;
    aLinkBase: String;
  begin
    result := '';
    aLink := Data.BuildLink(aBDS);
    Result+='<tr valign="top" align="left">';
    if TSQLSelectStatement(aStmt).All then
      begin
        for a := 0 to aDS.DataSet.FieldCount-1 do
          Result+='<td>'+aBDS.Fields[a].AsString+'</td>'
      end
    else
      begin
        for i := 0 to TSQLSelectStatement(aStmt).Fields.Count-1 do
          begin
            aElem := TSQLSelectStatement(aStmt).Fields[i];
            if aElem is TSQLSelectField then
              begin
                aName := TSQLSelectField(aElem).Expression.GetAsSQL([]);
                if copy(uppercase(aName),0,5)='LINK(' then
                  begin
                    aName := copy(aName,6,length(aName)-6);
                    if pos('(',aName)>0 then
                      begin
                        aLinkBase := copy(aName,0,pos('(',aName)-1);
                        aName := copy(aName,pos('(',aName)+1,length(aname));
                        aName := copy(aName,0,length(aName)-1);
                        aLink := aLinkBase+copy(aLink,pos('@',aLink),length(aLink));
                      end;
                    if pos('.',aName)>0 then
                      aName := copy(aName,rpos('.',aName)+1,length(aName));
                    if (aBDS.FieldDefs.IndexOf(aName)>-1) then
                      Result+='<td><a href="'+aLink+'" title="'+Data.GetLinkDesc(aLink)+#10+Data.GetLinkLongDesc(aLink)+'">'+HTMLEncode(aBDS.Fields[aBDS.FieldDefs.IndexOf(aName)].AsString)+'</a></td>'
                  end
                else if copy(uppercase(aName),0,4)='RTF(' then
                  begin
                    aName := copy(aName,5,length(aName)-5);
                    if pos('(',aName)>0 then
                      begin
                        aName := copy(aName,pos('(',aName)+1,length(aname));
                        aName := copy(aName,0,length(aName)-1);
                      end;
                    Result+='<td>'+HTMLEncode(RTF2Plain(aBDS.Fields[aBDS.FieldDefs.IndexOf(aName)].AsString))+'</td>';
                  end
                else if copy(uppercase(aName),0,5)='ICON(' then
                  begin
                    aName := copy(aName,6,length(aName)-6);
                    if pos('(',aName)>0 then
                      begin
                        aName := copy(aName,pos('(',aName)+1,length(aname));
                        aName := copy(aName,0,length(aName)-1);
                      end;
                    Result+='<td><img src="ICON('+aBDS.Fields[aBDS.FieldDefs.IndexOf(aName)].AsString+')"></img></td>';
                  end
                else if copy(uppercase(aName),0,12)='HISTORYICON(' then
                  begin
                    aName := copy(aName,12,length(aName)-11);
                    if pos('(',aName)>0 then
                      begin
                        aName := copy(aName,pos('(',aName)+1,length(aname));
                        aName := copy(aName,0,length(aName)-1);
                      end;
                    Result+='<td><img src="HISTORYICON('+aBDS.Fields[aBDS.FieldDefs.IndexOf(aName)].AsString+')"></img></td>';
                  end
                else if (aBDS.FieldDefs.IndexOf(copy(aName,rpos('.',aName)+1,length(aName)))>-1) then
                  begin
                    if pos('.',aName)>0 then
                      aName := copy(aName,rpos('.',aName)+1,length(aName));
                    Result+='<td>'+HTMLEncode(aBDS.Fields[aBDS.FieldDefs.IndexOf(aName)].AsString)+'</td>';
                  end
                else if Assigned(TSQLSelectField(aElem).AliasName) then
                  begin
                    aName := TSQLSelectField(aElem).AliasName.GetAsSQL([]);
                    if (aBDS.FieldDefs.IndexOf(aName)>-1) then
                     Result+='<td>'+HTMLEncode(aBDS.Fields[aBDS.FieldDefs.IndexOf(aName)].AsString)+'</td>'
                  end;
              end;
          end;
      end;
    Result+='</tr>';
  end;
  procedure AddHeader(aStmt : TSQLElement);
  var
    b: Integer;
  begin
    Outp+='<thead><tr>';
    for b := 0 to TSQLSelectStatement(aStmt).Fields.Count-1 do
      begin
        aElem := TSQLSelectStatement(aStmt).Fields[b];
        if aElem is TSQLSelectField then
          begin
            if Assigned(TSQLSelectField(aElem).AliasName) then
              Outp+='<th><b>'+HTMLEncode(StringReplace(TSQLSelectField(aElem).AliasName.GetAsSQL([sfoSingleQuoteIdentifier]),'''','',[rfReplaceAll]))+'</b></th>'
            else
              begin
                aName := TSQLSelectField(aElem).Expression.GetAsSQL([sfoSingleQuoteIdentifier]);
                if copy(uppercase(aName),0,5)='LINK(' then
                  begin
                    aName := copy(aName,6,length(aName)-6);
                    if pos('(',aName)>0 then
                      begin
                        aName := copy(aName,pos('(',aName)+1,length(aname));
                        aName := copy(aName,0,length(aName)-1);
                      end;
                  end;
                if pos('.',aName)>0 then
                  aName := copy(aName,pos('.',aName)+1,length(aName));
                Outp+='<th><b>'+HTMLEncode(StringReplace(aName,'''','',[rfReplaceAll]))+'</b></th>';
              end;
          end;
      end;
    Outp+='</tr></thead>';
  end;
  procedure FilterSQL(aType : Integer;IncHeader : Boolean = False);
  var
    a: Integer;
    aOrderDir: TSQLOrderDirection;
    aOrder: string = '';
    aOrderDirStr: String = 'ASC';
    aRDS: TDataSet;
    i: Integer;
    aTable: TSQLElement;
    NoRights: Boolean;
    aStmt: TSQLElement;
    aRightInt: Integer;
  begin
    FSQLStream := TStringStream.Create(Inp+';');
    FSQLScanner := TSQLScanner.Create(FSQLStream);
    FSQLParser := TSQLParser.Create(FSQLScanner);
    try
      aFilter:='';
      aStmt := FSQLParser.Parse;
      a := 0;
      aTable := TSQLSelectStatement(aStmt).Tables[a];
      if aTable is TSQLSimpleTableReference then
       begin
         aTableName := TSQLSimpleTableReference(aTable).ObjectName.Name;
         aRight := UpperCase(aTableName);
       end;
      if  (aType <> 3)
      and (aType <> 4)
      then
        begin
          if Data.ListDataSetFromLink(aTableName+'@',aClass) then
            begin
              if IncHeader then
                AddHeader(aStmt);
              if aType=1 then Outp+='<tbody align="left" valign="top">';
              aDs := TBaseDBDataset(aClass.Create(nil));
              if Assigned(TSQLSelectStatement(aStmt).Where) then
                aFilter:=TSQLSelectStatement(aStmt).Where.GetAsSQL([sfoDoubleQuoteIdentifier]);
              if Assigned(TSQLSelectStatement(aStmt).Orderby) and (TSQLSelectStatement(aStmt).Orderby.Count>0) then
                begin
                  aOrder:=TSQLIdentifierName(TSQLOrderByElement(TSQLSelectStatement(aStmt).Orderby.Elements[0]).Field).Name;
                  aOrderDir := TSQLOrderByElement(TSQLSelectStatement(aStmt).Orderby.Elements[0]).OrderBy;
                end;
              if ((data.Users.Rights.Right(aRight)>RIGHT_READ) or (data.Users.Rights.Right(aRight)=-1)) and (Assigned(aDS)) then
                begin
                  if aOrder<>'' then
                    begin
                      if aOrderDir=obAscending then
                        aOrderDirStr := 'ASC'
                      else aOrderDirStr := 'DESC';
                    end;
                  if (aDs.ActualFilter<>'') and (aFilter<>'') then
                    aDs.Filter('('+aDs.ActualFilter+') AND ('+aFilter+')',aLimit)
                  else if (aFilter = '') and (aDs.ActualFilter<>'') then
                    aDs.FilterEx('('+aDs.ActualFilter+')',aLimit,aOrder,aOrderDirStr)
                  else
                    aDs.FilterEx(aFilter,aLimit,aOrder,aOrderDirStr);
                  while not aDS.EOF do
                    begin
                      case aType of
                      0:BuildLinkRow(aDs.DataSet);
                      1:Outp+=BuildTableRow(aDs.DataSet,aStmt);
                      end;
                      aDataThere:=True;
                      aDs.Next;
                    end;
                end;
              aDS.Free;
              if aType=1 then Outp+='</tbody>';
            end
          else //pure SQL
            begin //TODO:better rights check ??
              NoRights := False;
              for a := 0 to TSQLSelectStatement(aStmt).Tables.Count-1 do
                begin
                  aTable := TSQLSelectStatement(aStmt).Tables[a];
                  if aTable is TSQLSimpleTableReference then
                    begin
                      aTableName := TSQLSimpleTableReference(aTable).ObjectName.Name;
                      aRight := UpperCase(aTableName);
                      aRightInt := Data.Users.Rights.Right(aRight);
                      if (aRightInt>-1) and (aRightInt<RIGHT_READ) then
                        NoRights := True;
                    end;
                end;
              if not NoRights then
                begin
                  aSQL := TSQLSelectStatement(aStmt).GetAsSQL([sfoDoubleQuoteIdentifier]);
                  aSQL := ReplaceSQLFunctions(aSQL);
                  if aLimit>0 then
                    aSQL := AddSQLLimit(aSQL,aLimit);
                  aRDS := Data.GetNewDataSet(aSQL);
                  try
                    aRDS.Open;
                    if IncHeader then
                      AddHeader(aStmt);
                    if aType=1 then Outp+='<tbody align="left" valign="top">';
                    while not aRDS.EOF do
                      begin
                        case aType of
                        0:BuildLinkRow(aRDS);
                        1:Outp+=BuildTableRow(aRDs,aStmt);
                        end;
                        aDataThere:=True;
                        aRDs.Next;
                      end;
                  except
                    on e : Exception do
                      begin
                        Outp+='error:'+e.Message+'<br>';
                        aDataThere:=True;
                      end;
                  end;
                end;
            end;
        end
      else
        begin
          aSQL := TSQLSelectStatement(aStmt).GetAsSQL([sfoDoubleQuoteIdentifier]);
          aSQL := ReplaceSQLFunctions(aSQL);
          if aLimit>0 then
            aSQL := AddSQLLimit(aSQL,aLimit);
          aRDS := Data.GetNewDataSet(aSQL);
          aRDS.Open;
          while not aRDS.EOF do
            begin
              if ((data.Users.Rights.Right(aRight)>RIGHT_READ) or (data.Users.Rights.Right(aRight)=-1)) and (Assigned(aRDS)) then
                begin
                  for i := 0 to aRDS.FieldCount-1 do
                    begin
                      if aType=3 then
                        begin
                          if i>0 then Outp+=',';
                          tmp := HTMLEncode(aRDS.Fields[i].AsString);
                          Outp += tmp;
                          if TryStrToFloat(tmp,aTmpFloat) then
                            begin
                              if aTmpFloat<>0 then
                                aDataThere:=True;
                            end
                          else if tmp<>'' then aDataThere:=True;
                        end
                      else if (aType=4) then
                        begin
                          tmp := aRDS.Fields[i].FieldName;
                          Variables.Values[tmp]:=aRDS.Fields[i].AsString;
                          aDataThere:=True;
                        end;
                    end;
                end;
              if aType = 4 then
                begin
                  aNewList := TWikiList.CreateEx(Self,Data);
                  if aNewList.FindWikiPage(aInclude) then
                    begin
                      Inp := aNewList.FieldByName('DATA').AsString;
                      for i := 0 to FVariables.Count-1 do
                        begin
                          Inp := StringReplace(Inp,'@VARIABLES.'+FVariables.Names[i]+'@',FVariables.ValueFromIndex[i],[rfReplaceAll,rfIgnoreCase]);
                          Inp := StringReplace(Inp,'@VARIABLES.'+FVariables.Names[i]+':HTTP@',HTTPEncode(FVariables.ValueFromIndex[i]),[rfReplaceAll,rfIgnoreCase]);
                        end;
                      Outp:=Outp+WikiText2HTML(Inp,'','',True);
                    end;
                  aNewList.Free;
                end;
              aRDS.Next;
            end;
          aRDS.Free;
        end;
      FreeAndNil(aStmt);
    except
      on e : Exception do
        begin
          Outp+='error:'+e.Message+'<br>';
          aDataThere:=True;
        end;
    end;
    FSQLScanner.Free;
    FSQLParser.Free;
    FSQLStream.Free;
  end;
begin
  with BaseApplication as IBaseApplication do
    Debug('WikiInclude:'+Inp);
  if pos('datathere(',lowercase(Inp))>0 then
    aDataThere:=False;
  if copy(lowercase(Inp),0,3)='if(' then
    begin
      aCondition := copy(Inp,4,pos(';',Inp)-4);
      Inp := copy(Inp,pos(';',Inp)+1,length(Inp));
      Inp := copy(Inp,0,length(Inp)-1);
      if copy(lowercase(aCondition),0,6)='right(' then
        begin
          aConditionOK:=Data.Users.Rights.Right(copy(aCondition,7,length(aCondition)-7))>=RIGHT_READ;
        end;
    end;
  if copy(lowercase(Inp),0,4)='rtf(' then
    begin
      Inp := copy(Inp,5,length(Inp)-5);
      ConvertRTF := True;
    end;
  if not aConditionOK then exit;
  for i := 0 to FVariables.Count-1 do
    begin
      Inp := StringReplace(Inp,'@VARIABLES.'+FVariables.Names[i]+'@',FVariables.ValueFromIndex[i],[rfReplaceAll,rfIgnoreCase]);
      Inp := StringReplace(Inp,'@VARIABLES.'+FVariables.Names[i]+':HTTP@',HTTPEncode(FVariables.ValueFromIndex[i]),[rfReplaceAll,rfIgnoreCase]);
    end;
  if Uppercase(copy(Inp,0,6)) = 'BOARD(' then
    begin
      Inp := copy(Inp,7,length(Inp));
      Data.SetFilter(Data.Tree,'',0,'','ASC',False,True,True);
      if Data.Tree.DataSet.Locate('NAME',copy(Inp,0,pos(',',Inp)-1),[loCaseInsensitive]) then
        begin
          Inp := copy(Inp,pos(',',Inp)+1,length(Inp));
          Inp := copy(Inp,0,pos(')',Inp)-1);
          if not  TryStrToInt(Inp,aCount) then aCount := 30;
          aList := TMessageList.Create(nil);
          Data.SetFilter(aList,Data.QuoteField('TREEENTRY')+'='+Data.QuoteValue(VarToStr(Data.Tree.Id.AsVariant)));
          while not aList.DataSet.EOF do
            begin
              if aCount <= 0 then break;
              aMessage := TMessage.CreateEx(Self,Data);
              aMessage.Select(aList.Id.AsVariant);
              aMessage.Open;
              if aMessage.Count > 0 then
                begin
                  Outp := Outp+'<b>'+aMessage.FieldByName('SUBJECT').AsString+'</b>';
                  aMessage.Content.Open;
                  if aMessage.Content.Count > 0 then
                    begin
                      ss := TStringStream.Create('');
                      Data.BlobFieldToStream(aMessage.Content.DataSet,'DATA',ss);
                      Outp := Outp+'<br>'+WikiText2HTML(ss.DataString,'','',True)+'<br>'+DateTimeToStr(aMessage.FieldByName('SENDDATE').AsDateTime)+'<br>';
                      ss.Free;
                    end;
                end;
              aMessage.Free;
              aList.DataSet.Next;
            end;
          aList.Free;
        end;
    end
  else if Uppercase(copy(Inp,0,9)) = 'SQLLINKS(' then
    begin
      Inp := copy(Inp,10,length(Inp)-10);
      if pos(';',Inp)>0 then
        begin
          aLimitS := copy(Inp,rpos(';',Inp)+1,length(Inp));
          if IsNumeric(aLimitS) then
            begin
              Inp := copy(Inp,0,rpos(';',Inp)-1);
              aLimit := StrToIntDef(aLimitS,10);
            end;
        end;
      Outp+='<ol>';
      FilterSQL(0);
      Outp+='</ol>';
    end
  else if Uppercase(copy(Inp,0,9)) = 'SQLTABLE(' then
    begin
      Inp := copy(Inp,10,length(Inp)-10);
      if pos(';',Inp)>0 then
        begin
          aLimitS := copy(Inp,rpos(';',Inp)+1,length(Inp));
          if IsNumeric(aLimitS) then
            begin
              Inp := copy(Inp,0,rpos(';',Inp)-1);
              aLimit := StrToIntDef(aLimitS,10);
            end;
        end;
      Outp+='<table>';
      FilterSQL(1);
      Outp+='</table>';
      if pos('error:',Outp)>0 then
        Outp := StringReplace(Outp,'table>','p>',[rfReplaceAll]);
    end
  else if Uppercase(copy(Inp,0,10)) = 'SQLTABLEH(' then
    begin
      Inp := copy(Inp,11,length(Inp)-11);
      if pos(';',Inp)>0 then
        begin
          aLimitS := copy(Inp,rpos(';',Inp)+1,length(Inp));
          if IsNumeric(aLimitS) then
            begin
              Inp := copy(Inp,0,rpos(';',Inp)-1);
              aLimit := StrToIntDef(aLimitS,10);
            end;
        end;
      Outp+='<table>';
      FilterSQL(1,True);
      Outp+='</table>';
      if pos('error:',Outp)>0 then
        Outp := StringReplace(Outp,'table>','p>',[rfReplaceAll]);
    end
  else if (Uppercase(copy(Inp,0,10)) = 'STATISTIC(')
       or (Uppercase(copy(Inp,0,11)) = 'STATISTICH(')
  then
    begin
      Inp := copy(Inp,11,length(Inp)-11);
      IncHeader := False;
      if copy(Inp,0,1)='(' then
        begin
          Inp := copy(Inp,2,length(Inp));
          IncHeader := True;
        end;
      if pos(';',Inp)>0 then
        begin
          aLimitS := copy(Inp,rpos(';',Inp)+1,length(Inp));
          if IsNumeric(aLimitS) then
            begin
              Inp := copy(Inp,0,rpos(';',Inp)-1);
              aLimit := StrToIntDef(aLimitS,10);
            end;
        end;
      aSQL := copy(Inp,0,rpos(' ',Inp)-1);
      if aSQL <> '' then
        aSQL := aSQL+' STATISTICS';
      FSQLStream := TStringStream.Create(aSQL);
      FSQLScanner := TSQLScanner.Create(FSQLStream);
      FSQLParser := TSQLParser.Create(FSQLScanner);
      Inp := copy(Inp,rpos(' ',Inp)+1,length(Inp));
      aStatistic := nil;
      try
        aFilter:='';
        bStmt := FSQLParser.Parse;
        aStatistic := TStatistic.Create(nil);
        if pos('(',Inp)>0 then
          begin
            tmp := copy(Inp,pos('(',Inp)+1,length(Inp)-1);
            tmp := copy(tmp,0,length(tmp)-1);
            Variables.Values[copy(tmp,0,pos('=',tmp)-1)] := copy(tmp,pos('=',tmp)+1,length(tmp));
            Inp := copy(Inp,0,pos('(',Inp)-1);
          end;
        aStatistic.SelectFromLink(Inp);
        aStatistic.Open;
        if aStatistic.Count>0 then
          begin
            aConn := Data.GetNewConnection;
            aSQL := aStatistic.BuildQuerry(Variables);
            aSQL := ReplaceSQLFunctions(aSQL);
            aRDs := Data.GetNewDataSet(aSQL,aConn);
            try
              aRDS.Open;
            except
              on e : Exception do
                begin
                  Outp+='error:'+e.Message+'<br>';
                  aDataThere:=True;
                end;
            end;
            Outp+='<table>';
            if IncHeader then
              AddHeader(bStmt);
            Outp+='<tbody align="left" valign="top">';
            while (not aRDS.EOF) and (aLimit>0) do
              begin
                aDataThere:=True;
                Outp+=BuildTableRow(aRDs,bStmt);
                dec(aLimit,1);
                aRDS.Next;
              end;
            Outp+='</tbody>';
            Outp+='</table>';
          end;
      finally
        FreeAndNil(aRds);
        FreeAndNil(aConn);
        FreeAndNil(aStatistic);
      end;
      FreeAndNil(bStmt);
      FSQLScanner.Free;
      FSQLParser.Free;
      FSQLStream.Free;
    end
  else if Uppercase(copy(Inp,0,15)) = 'STATISTICVALUE(' then
    begin
      Inp := copy(Inp,16,length(Inp)-16);
      if pos(';',Inp)>0 then
        begin
          aLimitS := copy(Inp,rpos(';',Inp)+1,length(Inp));
          if IsNumeric(aLimitS) then
            begin
              Inp := copy(Inp,0,rpos(';',Inp)-1);
              aLimit := StrToIntDef(aLimitS,10);
            end;
        end;
      aSQL := copy(Inp,0,rpos(' ',Inp)-1);
      if aSQL <> '' then
        aSQL := aSQL+' STATISTICS';
      FSQLStream := TStringStream.Create(aSQL);
      FSQLScanner := TSQLScanner.Create(FSQLStream);
      FSQLParser := TSQLParser.Create(FSQLScanner);
      Inp := copy(Inp,rpos(' ',Inp)+1,length(Inp));
      try
        aFilter:='';
        bStmt := FSQLParser.Parse;
        aStatistic := TStatistic.Create(nil);
        aStatistic.SelectFromLink(Inp);
        aStatistic.Open;
        if aStatistic.Count>0 then
          begin
            aConn := Data.GetNewConnection;
            aRDs := Data.GetNewDataSet(aStatistic.BuildQuerry(Variables),aConn);
            try
              aRDS.Open;
            except
              on e : Exception do
                begin
                  Outp+='error:'+e.Message+'<br>';
                  aDataThere:=True;
                end;
            end;
            tmp := BuildTableRow(aRDs,bStmt);
            tmp := StringReplace(tmp,'<tr>','',[rfReplaceall]);
            tmp := StringReplace(tmp,'</tr>','',[rfReplaceall]);
            tmp := StringReplace(tmp,'<td>','',[rfReplaceall]);
            tmp := StringReplace(tmp,'</td>','',[rfReplaceall]);
            Outp+=tmp;
            if TryStrToFloat(tmp,aTmpFloat) then
              if aTmpFloat<>0 then
                aDataThere:=True;
          end;
      finally
        FreeAndNil(aRds);
        FreeAndNil(aConn);
        FreeAndNil(aStatistic);
      end;
      FreeAndNil(bStmt);
      FSQLScanner.Free;
      FSQLParser.Free;
      FSQLStream.Free;
    end
  else if (Uppercase(copy(Inp,0,4)) = 'SQL(')
       or (Uppercase(copy(Inp,0,5)) = 'FORM(') then
    begin
      IsForm := (Uppercase(copy(Inp,0,5)) = 'FORM(');
      Inp := copy(Inp,pos('(',Inp)+1,length(Inp)-(pos('(',Inp)+1));
      if IsForm then
        begin
          aInclude := copy(Inp,0,pos(';',Inp)-1);
          Inp := copy(Inp,pos(';',Inp)+1,length(Inp));
        end;
      if pos(';',Inp)>0 then
        begin
          aLimitS := copy(Inp,rpos(';',Inp)+1,length(Inp));
          if IsNumeric(aLimitS) then
            begin
              Inp := copy(Inp,0,rpos(';',Inp)-1);
              aLimit := StrToIntDef(aLimitS,10);
            end;
        end;
      if not IsForm then
        FilterSQL(3)
      else
        begin
          FilterSQL(4);
        end;
    end
  else
    begin
      aNewList := TWikiList.CreateEx(Self,Data);
      nInp := Inp;
      if pos('|',nInp) > 0 then nInp := copy(nInp,0,pos('|',nInp)-1);
      nInp := StringReplace(nInp,'%username%',Data.Users.Text.AsString,[]);
      if aNewList.FindWikiPage(nInp) and (aLevel < 150) then
        begin
          Outp := Outp+WikiText2HTML(aNewList.FieldByName('DATA').AsString,'','',True,aLevel+1);
        end;
      aNewList.Free;
    end;
  if copy(lowercase(aCondition),0,10)='datathere(' then
    begin
      if not aDataThere then Outp := '';
    end;
  if ConvertRTF then
    begin
      Outp:=RTF2Plain(OutP);
    end;
end;

function TfWikiFrame.Wiki2HTML(input: string): TIPHtml;
var
  ss: TStringStream;
begin
  aDataThere:=False;
  WikiToHTML.OnWikiInclude:=@fWikiFrameWikiInclude;
  ss:=TStringStream.Create(UniToSys('<html><head><title>'+DataSet.FieldByName('CAPTION').AsString+'</title></head><body>'+WikiText2HTML(input,'','',True)+'<br><br><br></body></html>'));
  ss.Position := 0;
  try
    Result:=TSimpleIPHtml.Create;
    TSimpleIPHtml(Result).OnGetImageX:=@TSimpleIpHtmlGetImageX;
    Result.LoadFromStream(ss);
  finally
    ss.Free;
  end;
end;

procedure TfWikiFrame.SetLeftBar(AValue: Boolean);
begin
  pLeft.Visible:=aValue;
  pTop.Visible:=not AValue;
end;

function TfWikiFrame.GetLeftBar: Boolean;
begin
  Result := pLeft.Visible;
end;

procedure TfWikiFrame.AddDocuments(Sender: TObject);
var
  aDocuments: TDocuments;
begin
  if not Assigned(TfDocumentFrame(Sender).DataSet) then
    begin
      aDocuments := TDocuments.CreateEx(Self,Data);
      TfDocumentFrame(Sender).DataSet := aDocuments;
      TfDocumentFrame(Sender).Refresh(DataSet.Id.AsVariant,'W',DataSet.FieldByName('NAME').AsString,Null,Null);
    end;
end;

procedure TfWikiFrame.DoView;
begin
  ipHTML.Visible:= True;
  pcPages.Visible := False;
  acEdit.Checked:=False;
  Refresh;
end;

procedure TfWikiFrame.DoEdit;
begin
  ipHTML.Visible:= False;
  pcPages.Visible := True;
  acEdit.Checked:=True;
end;

procedure TfWikiFrame.New;
begin
end;
procedure TfWikiFrame.SetLanguage;
begin
end;
procedure TfWikiFrame.SetRights(Editable : Boolean);
begin
  FEditable := Editable;
  pEdit1.Visible:=Editable;
  pEdit2.Visible:=Editable;
end;
function TfWikiFrame.OpenWikiPage(PageName: string;CreateIfNotExists : Boolean = False) : Boolean;
var
  aParent : Variant;
  aDocuments: TDocuments;
  aDocFrame: TfDocumentFrame;
  aPageIndex: Integer;
  aDocPage: TTabSheet;
  aWiki: TWikiList;
begin
  with BaseApplication as IBaseApplication do
    Debug('OpenWikiPage:'+PageName);
  aWiki := TWikiList.Create(nil);
  Result := aWiki.FindWikiPage(pageName);
  aWiki.Free;
  DoView;
  if Result then
    begin
      TWikiList(DataSet).FindWikiPage(PageName);
      aParent := TWikiList(DataSet).ActiveTreeID;
      pcPages.ClearTabClasses;
      if FEditable then
        pcPages.AddTabClass(TfDocumentFrame,strFiles,@AddDocuments);
      //tsView.Show;
      Screen.Cursor := crHourglass;
      ipHTML.SetHtml(Wiki2HTML(DataSet.FieldByName('DATA').AsString));
      while pcPages.PageCount > 3 do
        pcPages.Pages[2].Destroy;
      if (FDataSet.State <> dsInsert) and (fDataSet.Count > 0) then
        begin
          aDocuments := TDocuments.CreateEx(Self,Data);
//          aDocuments.CreateTable;
          aDocuments.Select(DataSet.Id.AsVariant,'W',DataSet.FieldByName('NAME').AsString,Null,Null);
          aDocuments.Open;
          if aDocuments.Count = 0 then
            aDocuments.Free
          else
            begin
              aDocPage := pcPages.GetTab(TfDocumentFrame);
              if FEditAble then
                begin
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
                      DoView
                    end;
                end;
            end;
        end;
      Screen.Cursor := crDefault;
      FHistory.Add(Data.BuildLink(DataSet.DataSet));
    end
  else if CreateIfNotExists then
    begin
      pcPages.ClearTabClasses;
      TWikiList(DataSet).FindWikiPage(PageName);
      aParent := TWikiList(DataSet).ActiveTreeID;
      DataSet.Insert;
      DataSet.FieldByName('NAME').AsString := copy(PageName,rpos('/',PageName)+1,length(PageName));
      if aParent = 0 then
        aParent := TREE_ID_WIKI_UNSORTED;
      DataSet.FieldByName('TREEENTRY').AsVariant:= aParent;
      try
        DoEdit;
      except
      end;
   end;
  with BaseApplication as IBaseApplication do
    Debug('OpenWikiPage:'+PageName+' -> done');
end;

procedure TfWikiFrame.Refresh;
begin
  if (not Assigned(DataSet)) or (not DataSet.DataSet.Active) then exit;
  ipHTML.SetHtml(Wiki2HTML(DataSet.FieldByName('DATA').AsString));
end;

procedure TfWikiFrame.ShowFrame;
begin
  inherited ShowFrame;
  DoRefresh;
end;

procedure TfWikiFrame.DoRefresh;
begin
  inherited DoRefresh;
  if TWikiList(DataSet).isDynamic then
    RefreshTimer.Enabled:=True;
end;

var
  aEntry : TPrometMenuEntry;
initialization
  TBaseVisualApplication(Application).RegisterForm(TfWikiFrame);
  aEntry := TPrometMenuEntry.Create(strWiki,strWiki,'FOLDER@WIKI',IMAGE_WIKI,'W');
  AddMenuEntry(aEntry);
{$R *.lfm}
end.

