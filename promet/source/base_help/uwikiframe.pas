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
  DBGrids, ActnList, Dialogs, Menus, uImageCache, uBaseDbClasses,LCLProc;
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
    ActionList: TActionList;
    bItalic: TSpeedButton;
    Keywords: TDatasource;
    DBGrid1: TDBGrid;
    DBText1: TDBText;
    dnEdit: TDBNavigator;
    eTitle: TDBEdit;
    eWikiPage: TDBMemo;
    Label1: TLabel;
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
    SpeedButton8: TSpeedButton;
    SpeedButton9: TSpeedButton;
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
    procedure acImageExecute(Sender: TObject);
    procedure acIndexExecute(Sender: TObject);
    procedure acScreenshotExecute(Sender: TObject);
    procedure bItalicClick(Sender: TObject);
    function FCacheGetFile(Path: string;var NewPath : string): TStream;
    procedure fWikiFrameWikiInclude(Inp: string; var Outp: string);
    procedure ipHTMLHotClick(Sender: TObject);
    procedure OpenHistoryItemClick(Sender: TObject);
    procedure pmHistoryPopup(Sender: TObject);
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
    function Wiki2HTML(input: string): TIPHtml;
    procedure AddDocuments(Sender: TObject);
  public
    { public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy;override;
    function OpenFromLink(aLink : string) : Boolean;override;
    procedure New;override;
    procedure SetLanguage;override;
    procedure SetRights(Editable : Boolean);
    function OpenWikiPage(PageName : string;CreateIfNotExists : Boolean = False) : Boolean;
    procedure Refresh;
    procedure DoRefresh; override;
    property Variables : TStrings read FVariables;
  end;
  TSimpleIpHtml = class(TIpHtml)
  public
    property OnGetImageX;
    constructor Create;
  end;
implementation
uses uWiki,uData,WikiToHTML,uDocuments,Utils,LCLIntf,Variants,
  uBaseDbInterface,uscreenshotmain,uMessages,uDocumentFrame,fpsqlparser,
  fpsqlscanner, fpsqltree;
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
  DataSet := TWikiList.Create(Self,Data);
  FHistory := THistory.Create;
  FHistory.FFFWdAction := acForward;
  FHistory.RewAction := acBack;
  Wiki.DataSet := DataSet.DataSet;
  Keywords.DataSet := TWikiList(DataSet).Keywords.DataSet;
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
      else if (pos('@',PageName)>0) and Data.GotoLink(PageName) then
        begin
        end
      else if ((Pos('://', TIpHtmlNodeA(IpHtml.HotNode).HRef) > 0) or (pos('www',lowercase(TIpHtmlNodeA(IpHtml.HotNode).HRef)) > 0)) then
        OpenURL(TIpHtmlNodeA(IpHtml.HotNode).HRef)
      else if pos('@',PageName) = 0 then
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
  eWikiPage.SelStart:=eWikiPage.SelStart+length(eWikiPage.SelText)-1;
end;
procedure TfWikiFrame.SpeedButton4Click(Sender: TObject);
begin
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
  if Clipboard.HasFormat(LinkClipboardFormat) then
    begin
      Stream := TStringstream.Create('');
      if Clipboard.GetFormat(LinkClipboardFormat,Stream) then
        begin
          if (DataSet.DataSet.State <> dsEdit)
          and (DataSet.DataSet.State <> dsInsert) then
            DataSet.DataSet.Edit;
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
procedure TfWikiFrame.acForwardExecute(Sender: TObject);
begin
  FHistory.GoFwd;
end;
procedure TfWikiFrame.acImageExecute(Sender: TObject);
begin
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
procedure TfWikiFrame.acScreenshotExecute(Sender: TObject);
var
  aDocuments: TDocuments;
  aDocument: TDocument;
  aDocPage: TTabSheet;
  aName : string = 'screenshot.jpg';
  aDocFrame: TfDocumentFrame;
  aPageIndex: Integer;
begin
  Application.ProcessMessages;
  Application.MainForm.Hide;
  Application.ProcessMessages;
  aName := InputBox(strScreenshotName, strEnterAnName, aName);
  Application.ProcessMessages;
  Application.CreateForm(TfScreenshot,fScreenshot);
  fScreenshot.SaveTo:=AppendPathDelim(GetTempDir)+aName;
  fScreenshot.Show;
  while fScreenshot.Visible do Application.ProcessMessages;
  fScreenshot.Destroy;
  fScreenshot := nil;
  if DataSet.State=dsInsert then
    begin
      DataSet.Post;
      DataSet.Edit;
    end;
  aDocument := TDocument.Create(Self,Data);
  aDocument.Select(DataSet.Id.AsVariant ,'W',DataSet.FieldByName('NAME').AsString,Null,Null);
  aDocument.AddFromFile(AppendPathDelim(GetTempDir)+aName);
  aDocument.Free;
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
  if (DataSet.DataSet.State <> dsEdit)
  and (DataSet.DataSet.State <> dsInsert) then
    DataSet.DataSet.Edit;
  eWikiPage.SelText := '[[Bild:'+aName+']]';
  eWikiPage.SelStart:=eWikiPage.SelStart+length(eWikiPage.SelText);

  Application.MainForm.Show;
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
begin
  Result := nil;
  NewPath := Path;
  aDocument := TDocument.Create(Self,Data);
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
type
  TPlainProcedure = procedure;
procedure TfWikiFrame.fWikiFrameWikiInclude(Inp: string; var Outp: string);
var
  aList: TMessageList;
  aMessage: TMessage;
  aCount : Integer;
  ss: TStringStream;
  aNewList: TWikiList;
  FSQLStream: TStringStream;
  FSQLScanner: TSQLScanner;
  FSQLParser: TSQLParser;
  aStmt: TSQLElement;
  aTableName: TSQLStringType;
  aClass: TBaseDBDatasetClass;
  aDs: TBaseDbDataSet;
  aFilter: TSQLStringType;
  aRight: String;
  aLimit: Integer = 10;
  i: Integer;

  procedure BuildLinkRow;
  var
    aLink: String;
  begin
    aLink := Data.BuildLink(aDs.DataSet);
    Outp+='<li><a href="'+aLink+'" title="'+Data.GetLinkDesc(aLink)+#10+Data.GetLinkLongDesc(aLink)+'">'+HTMLEncode(Data.GetLinkDesc(aLink))+'</a></li>';
  end;
  procedure BuildTableRow;
  var
    aLink: String;
    i: Integer;
    a: Integer;
    aName: TSQLStringType;
    aElem: TSQLElement;
  begin
    aLink := Data.BuildLink(aDs.DataSet);
    Outp+='<tr>';
    if TSQLSelectStatement(aStmt).All then
      begin
        for a := 0 to aDS.DataSet.FieldCount-1 do
          Outp := Outp+'<td>'+aDS.DataSet.Fields[a].AsString+'</td>'
      end
    else
      begin
        for i := 0 to TSQLSelectStatement(aStmt).Fields.Count-1 do
          begin
            aElem := TSQLSelectStatement(aStmt).Fields[i];
            if aElem is TSQLSelectField then
              begin
                aName := TSQLSelectField(aElem).Expression.GetAsSQL([]);
                if (aDS.DataSet.FieldDefs.IndexOf(aName)>0) then
                  Outp := Outp+'<td>'+aDS.DataSet.Fields[i].AsString+'</td>'
              end;
          end;
      end;
    Outp+='</tr>';
  end;

  procedure FilterSQL(aType : Integer);
  var
    a: Integer;
    aOrderDir: TSQLOrderDirection;
    aOrder: string = '';
    aOrderDirStr: String = 'ASC';
    aRDS: TDataSet;
    i: Integer;
  begin
    FSQLStream := TStringStream.Create(Inp);
    FSQLScanner := TSQLScanner.Create(FSQLStream);
    FSQLParser := TSQLParser.Create(FSQLScanner);
    try
      aFilter:='';
      aStmt := FSQLParser.Parse;
      for a := 0 to TSQLSelectStatement(aStmt).Tables.Count-1 do
        begin
          aTableName := TSQLSimpleTableReference(TSQLSelectStatement(aStmt).Tables[a]).ObjectName.Name;
          if Data.ListDataSetFromLink(aTableName+'@',aClass) then
            begin
              aRight := UpperCase(aTableName);
              if aType <> 3 then
                begin
                  aDs := TBaseDBDataset(aClass.Create(nil,Data));
                  if Assigned(TSQLSelectStatement(aStmt).Where) then
                    aFilter:=TSQLSelectStatement(aStmt).Where.GetAsSQL([sfoDoubleQuoteIdentifier]);
                  if Assigned(TSQLSelectStatement(aStmt).Orderby) and (TSQLSelectStatement(aStmt).Orderby.Count>0) then
                    begin
                      aOrder:=TSQLIdentifierName(TSQLOrderByElement(TSQLSelectStatement(aStmt).Orderby.Elements[0]).Field).Name;
                      aOrderDir := TSQLOrderByElement(TSQLSelectStatement(aStmt).Orderby.Elements[0]).OrderBy;
                    end;
                  if (data.Users.Rights.Right(aRight)>RIGHT_READ) and (Assigned(aDS)) then
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
                        aDs.Filter('('+aDs.ActualFilter+')',aLimit,aOrder,aOrderDirStr)
                      else
                        aDs.Filter(aFilter,aLimit,aOrder,aOrderDirStr);
                      while not aDS.EOF do
                        begin
                          case aType of
                          0:BuildLinkRow;
                          1:BuildTableRow;
                          end;
                          aDs.Next;
                        end;
                    end;
                  aDS.Free;
                end
              else
                begin
                  aRDS := Data.GetNewDataSet(TSQLSelectStatement(aStmt).GetAsSQL([sfoDoubleQuoteIdentifier]));
                  if (data.Users.Rights.Right(aRight)>RIGHT_READ) and (Assigned(aRDS)) then
                    begin
                      for i := 0 to aRDS.FieldCount-1 do
                        begin
                          if i>0 then Outp+=',';
                          Outp += aRDS.Fields[i].AsString;
                        end;
                      aRDs.Next;
                    end;
                  aRDS.Free;
                end;
            end;
        end;
    except
    end;
    FSQLScanner.Free;
    FSQLParser.Free;
    FSQLStream.Free;
  end;

begin
  for i := 0 to FVariables.Count-1 do
    StringReplace(Inp,'VARIABLES.'+FVariables.Names[i],FVariables.ValueFromIndex[i],[rfReplaceAll]);
  if Uppercase(copy(Inp,0,6)) = 'BOARD(' then
    begin
      Inp := copy(Inp,7,length(Inp));
      Data.SetFilter(Data.Tree,'',0,'','ASC',False,True,True);
      if Data.Tree.DataSet.Locate('NAME',copy(Inp,0,pos(',',Inp)-1),[loCaseInsensitive]) then
        begin
          Inp := copy(Inp,pos(',',Inp)+1,length(Inp));
          Inp := copy(Inp,0,pos(')',Inp)-1);
          if not  TryStrToInt(Inp,aCount) then aCount := 30;
          aList := TMessageList.Create(nil,Data);
          Data.SetFilter(aList,Data.QuoteField('TREEENTRY')+'='+Data.QuoteValue(VarToStr(Data.Tree.Id.AsVariant)));
          while not aList.DataSet.EOF do
            begin
              if aCount <= 0 then break;
              aMessage := TMessage.Create(Self,Data);
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
          aLimit := StrToIntDef(copy(Inp,pos(';',Inp)+1,length(Inp)),10);
          Inp := copy(Inp,0,pos(';',Inp)-1);
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
          aLimit := StrToIntDef(copy(Inp,pos(';',Inp)+1,length(Inp)),10);
          Inp := copy(Inp,0,pos(';',Inp)-1);
        end;
      Outp+='<table>';
      FilterSQL(1);
      Outp+='</table>';
    end
  else if Uppercase(copy(Inp,0,4)) = 'SQL(' then
    begin
      Inp := copy(Inp,5,length(Inp)-5);
      if pos(';',Inp)>0 then
        begin
          aLimit := StrToIntDef(copy(Inp,pos(';',Inp)+1,length(Inp)),10);
          Inp := copy(Inp,0,pos(';',Inp)-1);
        end;
      FilterSQL(3);
    end
  else
    begin
      aNewList := TWikiList.Create(Self,Data);
      if pos('|',Inp) > 0 then Inp := copy(Inp,0,pos('|',Inp)-1);
      Inp := StringReplace(Inp,'%username%',Data.Users.Text.AsString,[]);
      if aNewList.FindWikiPage(Inp) then
        begin
          Outp := Outp+WikiText2HTML(aNewList.FieldByName('DATA').AsString,'','',True);
        end;
      aNewList.Free;
    end;
end;
function TfWikiFrame.Wiki2HTML(input: string): TIPHtml;
var
  ss: TStringStream;
begin
  WikiToHTML.OnWikiInclude:=@fWikiFrameWikiInclude;
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
procedure TfWikiFrame.SetRights(Editable : Boolean);
begin
  FEditable := Editable;
  tsEdit.TabVisible := Editable;
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
  aWiki := TWikiList.Create(nil,Data);
  Result := aWiki.FindWikiPage(pageName);
  aWiki.Free;
  if Result then
    begin
      TWikiList(DataSet).FindWikiPage(PageName);
      aParent := TWikiList(DataSet).ActiveTreeID;
      pcPages.ClearTabClasses;
      if FEditable then
        pcPages.AddTabClass(TfDocumentFrame,strFiles,@AddDocuments);
      tsView.Show;
      Screen.Cursor := crHourglass;
      ipHTML.SetHtml(Wiki2HTML(DataSet.FieldByName('DATA').AsString));
      while pcPages.PageCount > 3 do
        pcPages.Pages[2].Destroy;
      if (FDataSet.State <> dsInsert) and (fDataSet.Count > 0) then
        begin
          aDocuments := TDocuments.Create(Self,Data);
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
                      pcPages.Visible:=False;
                      tsEdit.PageIndex:=1;
                      pcPages.ActivePage := tsView;
                      pcPages.Visible:=True;
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
        tsEdit.Show;
      except
      end;
   end;
end;

procedure TfWikiFrame.Refresh;
begin
  if (not Assigned(DataSet)) or (not DataSet.DataSet.Active) then exit;
  ipHTML.SetHtml(Wiki2HTML(DataSet.FieldByName('DATA').AsString));
end;

procedure TfWikiFrame.DoRefresh;
begin
  inherited DoRefresh;
  if TWikiList(DataSet).isDynamic then
    Refresh;
end;

{$R *.lfm}
end.

