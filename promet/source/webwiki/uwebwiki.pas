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
unit uWebWiki;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, FileUtil, XMLPropStorage, HTTPDefs, fpHTTP,
  fpWeb, fphtml, uData, db, fpTemplate, Utils, uWiki, uDocuments,uBaseSearch,
  uBaseDBClasses,uIntfStrConsts,ubaseconfig;
type

  { TfmWikiPage }

  TfmWikiPage = class(TFPWebModule)
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
    procedure DataModuleGetAction(Sender: TObject; ARequest: TRequest;
      var ActionName: String);
    procedure exitRequest(Sender: TObject; ARequest: TRequest;
      AResponse: TResponse; var Handled: Boolean);
    procedure fmWikiPageWikiInclude(Inp: string; var Outp: string;aLevel : Integer = 0);
    procedure FSearchItemFound(aIdent: string; aName: string; aStatus: string;aActive : Boolean;
      aLink: string; aItem: TBaseDBList=nil);
    procedure lastchangesrssRequest(Sender: TObject; ARequest: TRequest;
      AResponse: TResponse; var Handled: Boolean);
    procedure ReplaceMainTags(Sender: TObject; const TagString: String;
      TagParams: TStringList; out ReplaceText: String);
    procedure SearchTagreplace(Sender: TObject; const TagString: String;
      TagParams: TStringList; out ReplaceText: String);
    procedure showlastchangesRequest(Sender: TObject; ARequest: TRequest;
      AResponse: TResponse; var Handled: Boolean);
    procedure Showrequest(Sender: TObject; ARequest: TRequest;
      AResponse: TResponse; var Handled: Boolean);
    procedure SearchRequest(Sender: TObject; ARequest: TRequest;
      AResponse: TResponse; var Handled: Boolean);
    procedure Createrequest(Sender: TObject; ARequest: TRequest;
      AResponse: TResponse; var Handled: Boolean);
    procedure TFPWebActionTemplateReplaceTag(Sender: TObject;
      const TagString: String; TagParams: TStringList; out ReplaceText: String);
  private
    { private declarations }
    Path: String;
    actTagParams: TStringList;
    FSearch: TSearch;
    FSearchResult : TStringList;
    FTemplate : TStringList;
    procedure ConvertImage(Image: string; var OutFile: string; var aLinkTags,aHref,aTags,aWidth : string);
    function CreateAllowed : Boolean;
  public
    { public declarations }
    Title : string;
    IsMainPage : Boolean;
    MainMenue : TStringList;
    MetaData : TStringList;
    SearchItems : TStringList;
    IgnoreLinks : TStringList;
    Wiki : TWikiList;
    Documents : TDocument;
    function IgnorePage(LinkValue : string) : Boolean;
    procedure SettemplateParams(aTemplate : TFPTemplate);
    procedure ReplaceStdTags(Sender: TObject; const TagString: String;TagParams: TStringList; out ReplaceText: String);
    constructor Create(AOwner: TComponent); override;
  end;
var
  fmWikiPage: TfmWikiPage;
const
  LAST_CHANGES_COUNT = 20;
implementation
uses WikitoHTML, uBaseApplication,uBaseDbInterface,Variants,
  uBaseFCGIApplication, htmltowiki, uError, fpImage, FPReadJPEGintfd,
  fpCanvas,fpImgCanv,ushop,uMessages,DOM,XMLWrite,synautil,uBaseWebSession;
resourcestring
  strLastChanges                     = 'Letzte Änderungen';
  strLastChangesDesc                 = 'Letzte Änderungen der Website';
{$R *.lfm}
procedure TfmWikiPage.DataModuleCreate(Sender: TObject);
begin
  Wiki := TWikiList.Create(nil,Data);
  Documents := TDocument.Create(nil,Data);
  MainMenue := TStringList.Create;
  SearchItems := TStringList.Create;
  IgnoreLinks := TStringList.Create;
  MetaData := TStringList.Create;
  if FileExists('metadata.inc') then
    MetaData.LoadFromFile('metadata.inc');
  with BaseApplication as IBaseConfig do
    begin
      Config.ReadStrings('MainMenue',MainMenue);
      Config.ReadStrings('SearchItems',SearchItems);
      Config.ReadStrings('IgnoreLinks',IgnoreLinks);
    end;
  FTemplate := TStringList.Create;
  uBaseSearch.AddSearchAbleDataSet(TWikiList);
end;
procedure TfmWikiPage.DataModuleDestroy(Sender: TObject);
begin
  MetaData.Free;
  IgnoreLinks.Free;
  SearchItems.Free;
  MainMenue.Free;
  FTemplate.Free;
end;
procedure TfmWikiPage.DataModuleGetAction(Sender: TObject; ARequest: TRequest;
  var ActionName: String);
var
  Result: string;
  aParent : Variant;
  Found: Boolean;
  PageName: String;
begin
  aParent := 0;
  Path := '';
  If (ActionVar<>'') then
    Result:=ARequest.QueryFields.Values[ActionVar];
  If (Result='') then
    begin
      Result := copy(ARequest.PathInfo,2,length(ARequest.PathInfo));
      Result := copy(Result,0,pos('/',Result)-1);
    end;
  if Result = '' then
    Result:=ARequest.GetNextPathInfo;
  IsMainPage:=false;
  if (ARequest.PathInfo = '') or (ARequest.PathInfo = '/') or (Result = 'wiki') or (Result = 'show') then
    begin
      Result := 'show';
      Path := copy(ARequest.PathInfo,2,length(ARequest.PathInfo));
      Path := copy(Path,pos('/',Path)+1,length(Path));
      if Path = '' then
        begin
          with BaseApplication as IBaseConfig do
            Path := Config.ReadString('INDEX','INDEX');
          IsMainPage:=True;
        end;
      Path := trim(Path);
    end;
  Found := False;
  PageName := Path;

  while pos('/',PageName) > 0 do
    begin
      Data.Tree.Open;
      if Data.Tree.DataSet.Locate('NAME;PARENT',VarArrayOf([copy(PageName,0,pos('/',PageName)-1),aParent]),[])
      or Data.Tree.DataSet.Locate('NAME;PARENT',VarArrayOf([copy(PageName,0,pos('/',PageName)-1),aParent]),[loCaseInSensitive]) then
        begin
          PageName := copy(PageName,pos('/',PageName)+1,length(PageName));
          aParent := Data.Tree.Id.AsVariant;
        end
      else
        begin
          Data.SetFilter(Data.Tree,'',0,'','ASC',False,True,True);
          if Data.Tree.DataSet.Locate('NAME;PARENT',VarArrayOf([copy(PageName,0,pos('/',PageName)-1),aParent]),[])
          or Data.Tree.DataSet.Locate('NAME;PARENT',VarArrayOf([copy(PageName,0,pos('/',PageName)-1),aParent]),[loCaseInSensitive]) then
            begin
              PageName := copy(PageName,pos('/',PageName)+1,length(PageName));
              aParent := Data.Tree.Id.AsVariant;
            end
          else break;
        end;
    end;
  PageName := StringReplace(Pagename,' ','_',[rfReplaceAll]);
  Found := Wiki.DataSet.Active and Wiki.DataSet.Locate('TREEENTRY;NAME',VarArrayOf([aParent,PageName]),[]);
  if not Found then
    begin
      Data.SetFilter(Wiki,Data.QuoteField('TREEENTRY')+'='+Data.QuoteValue(VarToStr(aParent)));
      Found := Wiki.DataSet.Locate('TREEENTRY;NAME',VarArrayOf([aParent,PageName]),[loCaseInsensitive]);
      if not Found then
        begin
          Data.SetFilter(Wiki,Data.QuoteField('NAME')+'='+Data.QuoteValue(PageName));
          Found := Wiki.DataSet.Locate('TREEENTRY;NAME',VarArrayOf([Null,PageName]),[loCaseInsensitive]);
        end;
    end;
  if Actions.FindAction(path) <> nil then
    Result := path;
  if Result = 'show' then
    begin
      if not Found then
        begin
          Result := 'create';
        end;
    end;
  TBaseWebSession(Session).AddHistoryUrl(ARequest.PathInfo);
  ActionName := Result;
end;
procedure TfmWikiPage.exitRequest(Sender: TObject; ARequest: TRequest;
  AResponse: TResponse; var Handled: Boolean);
begin
  Wiki.Destroy;
  Documents.Destroy;
  Application.Port:=0;
  Application.Terminate;
  Handled := True;
end;
procedure TfmWikiPage.fmWikiPageWikiInclude(Inp: string; var Outp: string;
  aLevel: Integer);
var
  aCount : Integer;
  aList: TMessageList;
  aMessage: TMessage;
  ss: TStringStream;
  FDataSet: TWikiList;
  aRow: String;
  aContent: String;
begin
  if not Assigned(actTagParams) then exit;
  if Uppercase(copy(Inp,0,6)) = 'BOARD(' then
    begin
      Inp := copy(Inp,7,length(Inp));
      try
      Data.SetFilter(Data.Tree,'',0,'','ASC',False,True);
      if Data.Tree.DataSet.Locate('NAME',copy(Inp,0,pos(',',Inp)-1),[loCaseInsensitive]) then
        begin
          Outp := actTagParams.Values['BOARDHEADER'];
          aRow := actTagParams.Values['BOARDONEROW'];
          Inp := copy(Inp,pos(',',Inp)+1,length(Inp));
          Inp := copy(Inp,0,pos(')',Inp)-1);
          if not  TryStrToInt(Inp,aCount) then aCount := 30;
          try
            aList := TMessageList.Create(nil,Data);
            Data.SetFilter(aList,Data.QuoteField('TREEENTRY')+'='+Data.QuoteValue(VarToStr(Data.Tree.Id.AsVariant)),aCount,'SENDDATE','DESC');
            while not aList.DataSet.EOF do
              begin
                if aCount <= 0 then break;
                try
                  aMessage := TMessage.Create(Self,Data);
                  aMessage.Select(aList.Id.AsVariant);
                  aMessage.Open;
                  if aMessage.Count > 0 then
                    begin
                      aMessage.Content.Open;
                      if aMessage.Content.Count > 0 then
                        begin
                          try
                            ss := TStringStream.Create('');
                            Data.BlobFieldToStream(aMessage.Content.DataSet,'DATA',ss);
                            if aMessage.Content.FieldByName('DATATYP').AsString='WIKI' then
                              aContent := WikiText2HTML(ss.DataString,'','')
                            else
                              aContent := ss.DataString;
                            Outp := Outp+StringReplace(
                                         StringReplace(
                                         StringReplace(aRow,'~Subject',aMessage.FieldByName('SUBJECT').AsString,[rfReplaceAll])
                                                           ,'~Date',DateTimeToStr(aMessage.FieldByName('SENDDATE').AsDateTime),[rfReplaceAll])
                                                           ,'~Content',aContent,[rfReplaceAll]);
                          finally
                            ss.Free;
                          end;
                        end;
                    end;
                finally
                  aMessage.Free;
                end;
                aList.DataSet.Next;
              end;
          finally
            aList.Free;
          end;
          Outp := Outp+actTagParams.Values['BOARDFOOTER'];
        end;
      except
      end;
    end
  else
    begin
      try
        FDataSet := TWikiList.Create(Self,Data);
        if pos('|',Inp) > 0 then
          Inp := copy(Inp,0,pos('|',Inp)-1);
        if TWikiList(FDataSet).FindWikiPage(Inp) then
          begin
            Outp := Outp+WikiText2HTML(FDataSet.FieldByName('DATA').AsString,'','');
          end;
      finally
        FDataSet.Free;
      end;
    end;
end;
procedure TfmWikiPage.FSearchItemFound(aIdent: string; aName: string;
  aStatus: string;aActive : Boolean; aLink: string; aItem: TBaseDBList=nil);
var
  LinkValue: String;
  aOffset: String;
  i: Integer;
begin
  LinkValue := copy(aLink,pos('@',aLink)+1,length(aLink));
  if Pos('{',LinkValue) > 0 then
    LinkValue := copy(LinkValue,0,pos('{',LinkValue)-1);
  if not IgnorePage(LinkValue) then
    FSearchResult.Add(aLink);
end;
procedure TfmWikiPage.lastchangesrssRequest(Sender: TObject;
  ARequest: TRequest; AResponse: TResponse; var Handled: Boolean);
var
  Doc: TXMLDocument;                                  // variable to document
  RootNode, parentNode, nofilho: TDOMNode;                    // variable to nodes
  ss: TStringStream;
  LinkValue: String;
  LinkRValue: String;
  LinkDesc: String;
  aWiki: TWikiList;
  LinkLocation: String;
  LinkBase: String;
  channelNode: TDOMNode;
  itemNode: TDOMElement;
begin
  try
    try
      ss := TStringStream.Create('');
      Doc := TXMLDocument.Create;
      try
        RootNode := Doc.CreateElement('rss');
        TDOMElement(RootNode).SetAttribute('version', '2.0');
        Doc.Appendchild(RootNode);
        RootNode:= Doc.DocumentElement;
        parentNode := Doc.CreateElement('channel');
        channelNode := parentNode;
        RootNode.Appendchild(parentNode);
        parentNode := Doc.CreateElement('title');
        nofilho := Doc.CreateTextNode(HTMLEncode(strLastChanges));
        parentNode.Appendchild(nofilho);
        RootNode.ChildNodes.Item[0].AppendChild(parentNode);
        parentNode := Doc.CreateElement('pubDate');
        nofilho := Doc.CreateTextNode(Rfc822DateTime((Now())));
        parentNode.Appendchild(nofilho);
        with BaseApplication as IBaseConfig do
          begin
            LinkBase := Config.ReadString('WebsiteCompleteURL','');
          end;
        LinkBase := LinkBase+'/wiki/';
        RootNode.ChildNodes.Item[0].AppendChild(parentNode);
        parentNode := Doc.CreateElement('link');
        nofilho := Doc.CreateTextNode(LinkBase);
        parentNode.Appendchild(nofilho);
        RootNode.ChildNodes.Item[0].AppendChild(parentNode);
        parentNode := Doc.CreateElement('description');
        nofilho := Doc.CreateTextNode(HTMLEncode(strLastChangesDesc));
        parentNode.Appendchild(nofilho);
        RootNode.ChildNodes.Item[0].AppendChild(parentNode);

        aWiki := TWikiList.Create(Self,Data);
        try
          Data.SetFilter(aWiki,'',LAST_CHANGES_COUNT,'TIMESTAMPD','DESC');
          while not aWiki.DataSet.EOF do
            begin
              LinkValue := Data.BuildLink(aWiki.DataSet);
              LinkRValue := LinkValue;
              LinkDesc := HTMLEncode(Data.GetLinkDesc(LinkValue));
              LinkValue := copy(LinkValue,pos('@',LinkValue)+1,length(LinkValue));
              if rpos('{',LinkValue) > 0 then
                LinkValue := copy(LinkValue,0,rpos('{',LinkValue)-1)
              else if rpos('(',LinkValue) > 0 then
                LinkValue := copy(LinkValue,0,rpos('(',LinkValue)-1);
              if not IgnorePage(LinkValue) then
                begin
                  try
                    if rpos('(',Linkdesc) > 0 then
                      begin
                        LinkLocation := copy(LinkDesc,rpos('(',LinkDesc)+1,length(LinkDesc));
                        LinkLocation := copy(LinkLocation,0,length(LinkLocation)-1);
                        LinkDesc := copy(LinkDesc,0,rpos('(',LinkDesc)-1);
                      end
                    else if rpos('{',Linkdesc) > 0 then
                      begin
                        LinkLocation := copy(LinkDesc,rpos('{',LinkDesc)+1,length(LinkDesc));
                        LinkLocation := copy(LinkLocation,0,length(LinkLocation)-1);
                        LinkDesc := copy(LinkDesc,0,rpos('{',LinkDesc)-1);
                      end;
                    itemNode := Doc.CreateElement('item');
                    ChannelNode.Appendchild(itemNode);
                    parentNode := Doc.CreateElement('title');
                    nofilho := Doc.CreateTextNode(LinkDesc);
                    parentNode.Appendchild(nofilho);
                    itemNode.AppendChild(parentNode);
                    parentNode := Doc.CreateElement('description');
                    nofilho := Doc.CreateTextNode(HTMLEncode(Data.GetLinkLongDesc(LinkRValue)));
                    parentNode.Appendchild(nofilho);
                    itemNode.AppendChild(parentNode);
                    parentNode := Doc.CreateElement('link');
                    nofilho := Doc.CreateTextNode(LinkBase+LinkValue);
                    parentNode.Appendchild(nofilho);
                    itemNode.AppendChild(parentNode);
                    parentNode := Doc.CreateElement('guid');
                    nofilho := Doc.CreateTextNode(LinkValue);
                    parentNode.Appendchild(nofilho);
                    itemNode.AppendChild(parentNode);
                    parentNode := Doc.CreateElement('pubDate');
                    nofilho := Doc.CreateTextNode(Rfc822DateTime(aWiki.TimeStamp.AsDateTime));
                    parentNode.Appendchild(nofilho);
                  except
                    parentNode := nil;
                  end;
                  if Assigned(parentNode) then
                    itemNode.AppendChild(parentNode);
                end;
              aWiki.DataSet.Next;
            end;
          WriteXMLFile(Doc, ss);                     // write to XML
          AResponse.Content := ss.DataString;
          AResponse.ContentType := 'text/xml';
          AResponse.SendContent;
          Handled := True;
        finally
          aWiki.Free;
        end;
      finally
        Doc.Free;
      end;
    finally
      ss.Free;
    end;
  except
  end;
end;
procedure TfmWikiPage.ReplaceStdTags(Sender: TObject; const TagString: String;
  TagParams: TStringList; out ReplaceText: String);
var
  aRow: string;
  i: Integer;
begin
  if AnsiCompareText(TagString, 'Title') = 0 then
    begin
      ReplaceText := HTMLEncode(Title);
    end
  else if AnsiCompareText(TagString, 'DateTime') = 0 then
    ReplaceText := FormatDateTime(TagParams.Values['FORMAT'], Now)
  else if AnsiCompareText(TagString, 'TOPNAVIGATION') = 0 then
    begin
      ReplaceText := Tagparams.Values['HEADER'];
      aRow := TagParams.Values['ONEROW'];
      for i := 0 to Mainmenue.Count-1 do
        ReplaceText := ReplaceText+StringReplace(
                                   StringReplace(aRow,'~LinkValue',Mainmenue.ValueFromIndex[i],[rfReplaceAll])
                                                     ,'~NameValue',Mainmenue.Names[i],[rfReplaceAll]);
      if  Tagparams.Values['BASELOCATION'] <> '' then
        ReplaceText := ReplaceText+StringReplace(Tagparams.Values['FOOTER'],'~BaseLocation',Tagparams.Values['BASELOCATION'],[rfReplaceAll]);
    end
  else if AnsiCompareText(TagString, 'MetaData') = 0 then
    begin
      ReplaceText := MetaData.Text;
    end
  else if AnsiCompareText(TagString, 'SEARCHBAR') = 0 then
    begin
      ReplaceText := TagParams.Values['SEARCHFORM'];
    end;
end;
constructor TfmWikiPage.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  WikitoHTML.OnWikiInclude:=@fmWikiPageWikiInclude;
end;
function TfmWikiPage.CreateAllowed: Boolean;
begin
  Result := False;
end;
function TfmWikiPage.IgnorePage(LinkValue: string): Boolean;
var
  i: Integer;
  aOffset: String;
begin
  Result := True;
  if IgnoreLinks.IndexOf(LinkValue) > -1 then exit;
  if SearchItems.Count > 0 then
    begin
      for i := 0 to SearchItems.Count-1 do
        begin
          aOffset := SearchItems.ValueFromIndex[i];
          if copy(LinkValue,0,length(aOffset)) = aOffset then
            begin
              Result := False;
              break;
            end;
        end;
    end
  else
    begin
      for i := 0 to MainMenue.Count-1 do
        begin
          aOffset := copy(MainMenue.ValueFromIndex[i],pos('wiki/',MainMenue.ValueFromIndex[i])+5,length(MainMenue.ValueFromIndex[i]));
          if rpos('/',aOffset) > 0 then
            aOffset := copy(aOffset,0,rpos('/',aOffset));
          if copy(LinkValue,0,length(aOffset)) = aOffset then
            begin
              if IgnoreLinks.IndexOf(LinkValue) = -1 then
                Result := False;
              break;
            end;
        end;
    end;
end;
procedure TfmWikiPage.ConvertImage(Image : string;var OutFile : string; var aLinkTags,aHref,aTags,aWidth : string);
var
  ImageFile: String;
  fs: TFileStream;
  SmallFile: String;
  aPicture: TFPMemoryImage;
  Aspect: real;
  aPicture2: TFPMemoryImage;
  aCanvas: TFPImageCanvas;
begin
  try
    with BaseApplication as IBaseConfig do
      begin
        ImageFile := AppendPathDelim(AppendPathDelim(Config.ReadString('DOCROOTPATH',''))+'images')+ValidateFileName(Image);
        ForceDirectoriesUTF8(AppendPathDelim(Config.ReadString('DOCROOTPATH',''))+'images');
      end;
    OutFile := '/images/'+ValidateFileName(Image);
    if not FileExists(ImageFile) or ((aWidth <> '') and (not Fileexists(copy(ImageFile,0,rpos('.',ImageFile)-1)+'_'+aWidth+'px.jpg'))) then
      begin
        Data.SetFilter(Documents,'"TYPE"=''W'' and "NAME"='+Data.QuoteValue(copy(ExtractFileName(Image),0,rpos('.',ExtractFileName(Image))-1)),1);
        if Documents.DataSet.RecordCount > 0 then
          begin
            try
              fs := TFileStream.Create(ImageFile,fmCreate);
              Data.BlobFieldToStream(Documents.DataSet,'DOCUMENT',fs);
            finally
              fs.Free;
            end;
            if aWidth <> '' then
              begin
                SmallFile := copy(ImageFile,0,rpos('.',ImageFile)-1)+'_'+aWidth+'px.jpg';
                try
                  aPicture := TFPMemoryImage.Create(0,0);
                  aPicture.LoadFromFile(ImageFile);
                  Aspect := aPicture.Width/aPicture.Height;
                  try
                    aPicture2 := TFPMemoryImage.Create(StrToInt(aWidth),round(StrToInt(aWidth)/Aspect));
                    aCanvas := TFPImageCanvas.create(aPicture2);
                    try
                      aCanvas.Height := aPicture2.Height;
                      aCanvas.Width := aPicture2.Width;
                      aCanvas.StretchDraw(0,0,StrToInt(aWidth),round(StrToInt(aWidth)/Aspect),aPicture);
                    finally
                      aCanvas.Free;
                    end;
                    aPicture2.SaveToFile(SmallFile);
                  finally
                    aPicture2.Free;
                  end;
                finally
                  aPicture.Free;
                end;
              end;
          end;
      end;
    if aWidth <> '' then
      begin
        aHref := OutFile;
        OutFile := copy(aHref,0,rpos('.',aHref)-1)+'_'+aWidth+'px.jpg';
        aLinkTags := ' class="canhighlight"';
      end
    else
      begin
        aHref := '';
        aLinkTags := '';
      end;
  except
  end;
end;
procedure TfmWikiPage.ReplaceMainTags(Sender: TObject; const TagString: String;
  TagParams: TStringList; out ReplaceText: String);
var
  aKeyWords: String;
begin
  actTagParams := TagParams;
  if AnsiCompareText(TagString, 'CONTENT') = 0 then
    begin
      ReplaceText := TagParams.Values['CHEADER'];
      WikiToHTML.OnConvertImage:=@ConvertImage;
      with BaseApplication as IBaseConfig do
        ReplaceText := Replacetext+Stringreplace(Tagparams.Values['CCONTENT'],'~Content',WikiText2HTML(Wiki.FieldByName('DATA').AsString,'/cgi-bin/wiki.cgi/wiki/',Config.ReadString('REMOVELINKOFFSET','')),[rfReplaceAll]);
      ReplaceText := ReplaceText+TagParams.Values['CFOOTER'];
    end
  else if AnsiCompareText(TagString, 'DESCRIPTION') = 0 then
    begin
      ReplaceText := HTMLEncode(copy(StripWikiText(Wiki.FieldByName('DATA').AsString),0,200));
      if rpos('.',ReplaceText) > 100 then
        ReplaceText := copy(ReplaceText,0,rpos('.',ReplaceText))
      else if rpos(' ',ReplaceText) > 100 then
        ReplaceText := copy(ReplaceText,0,rpos(' ',ReplaceText))
      else
        ReplaceText := copy(ReplaceText,0,120);
    end
  else if AnsiCompareText(TagString, 'KEYWORDS') = 0 then
    begin
      with Wiki.Keywords.DataSet as IBaseDbFilter do
        Filter := '';
      Wiki.Keywords.Open;
      if Wiki.Keywords.Count>0 then
        begin
          aKeyWords := '';
          while not Wiki.Keywords.DataSet.EOF do
            begin
              aKeyWords := aKeyWords+','+Wiki.Keywords.FieldByName('KEYWORD').AsString;
              Wiki.Keywords.DataSet.Next;
            end;
          aKeyWords := copy(aKeyWords,2,length(aKeyWords));
          ReplaceText := StringReplace(TagParams.Values['HEADER'],'~Keywords',aKeyWords,[rfReplaceAll]);
        end;
    end
  else if IsMainPage and (AnsiCompareText(TagString, 'MAINPAGEADDON') = 0) then
    begin
      ReplaceText := TagParams.Values['HEADER'];
    end
  else
    begin
      TagParams.Values['BASELOCATION'] := 'wiki';
      ReplaceStdTags(Sender,TagString,TagParams,ReplaceText);
    end;
  ReplaceText := UTF8ToSys(ReplaceText);
end;
procedure TfmWikiPage.SearchTagreplace(Sender: TObject;
  const TagString: String; TagParams: TStringList; out ReplaceText: String);
var
  aRow: string;
  i: Integer;
  LinkValue: String;
  LinkDesc: String;
  LinkLocation: String;
begin
  if AnsiCompareText(TagString, 'CONTENT') = 0 then
    begin
      ReplaceText := TagParams.Values['CHEADER'];
      aRow := TagParams.Values['CONEROW'];
      for i := 0 to FSearchResult.Count-1 do
        begin
          LinkValue := copy(FSearchResult[i],pos('@',FSearchResult[i])+1,length(FSearchResult[i]));
          if rpos('{',LinkValue) > 0 then
            LinkValue := copy(LinkValue,0,rpos('{',LinkValue)-1)
          else if rpos('(',LinkValue) > 0 then
            LinkValue := copy(LinkValue,0,rpos('(',LinkValue)-1);
          LinkDesc := HTMLEncode(Data.GetLinkDesc(FSearchResult[i]));
          if rpos('(',Linkdesc) > 0 then
            begin
              LinkLocation := copy(LinkDesc,rpos('(',LinkDesc)+1,length(LinkDesc));
              LinkLocation := copy(LinkLocation,0,length(LinkLocation)-1);
              LinkDesc := copy(LinkDesc,0,rpos('(',LinkDesc)-1);
            end
          else if rpos('{',Linkdesc) > 0 then
            begin
              LinkLocation := copy(LinkDesc,rpos('{',LinkDesc)+1,length(LinkDesc));
              LinkLocation := copy(LinkLocation,0,length(LinkLocation)-1);
              LinkDesc := copy(LinkDesc,0,rpos('{',LinkDesc)-1);
            end;
          ReplaceText := ReplaceText+StringReplace(
                                     StringReplace(
                                     StringReplace(
                                     StringReplace(
                                     StringReplace(aRow,'~LinkValue',LinkValue,[rfReplaceAll])
                                                       ,'~LinkName',LinkDesc,[rfReplaceAll])
                                                       ,'~LinkLocation',LinkLocation,[rfReplaceAll])
                                                       ,'~ProductImage','',[rfReplaceAll])
                                                       ,'~LinkDesc',Data.GetLinkLongDesc(FSearchResult[i]),[rfReplaceAll]);
        end;
      if FSearchResult.Count = 0 then
        ReplaceText := ReplaceText+TagParams.Values['CNONEFOUND'];
      ReplaceText := ReplaceText+TagParams.Values['CFOOTER'];
    end
  else
    ReplaceStdTags(Sender,TagString,TagParams,ReplaceText);
  ReplaceText := UTF8ToSys(ReplaceText);
end;
procedure TfmWikiPage.showlastchangesRequest(Sender: TObject;
  ARequest: TRequest; AResponse: TResponse; var Handled: Boolean);
begin
  SettemplateParams(TFPWebAction(Sender).Template);
  Title := strLastChanges;
  TFPWebAction(Sender).Template.OnReplaceTag:=@TFPWebActionTemplateReplaceTag;
  AResponse.Content := TFPWebAction(Sender).Template.GetContent;
  AResponse.SendContent;
  Handled := true;
end;
procedure TfmWikiPage.Showrequest(Sender: TObject; ARequest: TRequest;
  AResponse: TResponse; var Handled: Boolean);
begin
  SettemplateParams(TFPWebAction(Sender).Template);
  Title := Wiki.FieldByName('CAPTION').Text;
  AResponse.Content := TFPWebAction(Sender).Template.GetContent;
  AResponse.SendContent;
  Handled := true;
end;
procedure TfmWikiPage.SearchRequest(Sender: TObject; ARequest: TRequest;
  AResponse: TResponse; var Handled: Boolean);
var
  Locations : TSearchLocations;
begin
  SettemplateParams(TFPWebAction(Sender).Template);
  Title := 'Suche';
  TFPWebAction(Sender).Template.OnReplaceTag :=@SearchTagreplace;
  Setlength(Locations,1);
  Locations[0] := strWiki;
  FSearch := TSearch.Create([fsIdents,fsShortNames,fsDescription],Locations,True,30);
  FSearch.OnItemFound:=@FSearchItemFound;
  FSearchResult := TStringList.Create;
  FSearch.Start(ARequest.QueryFields.Values['search']);
  FreeAndNil(FSearch);
  AResponse.Content := TFPWebAction(Sender).Template.GetContent;
  FreeAndNil(FSearchResult);
  AResponse.SendContent;
  Handled := true;
end;
procedure TfmWikiPage.Createrequest(Sender: TObject; ARequest: TRequest;
  AResponse: TResponse; var Handled: Boolean);
begin
  {
  SettemplateParams(TFPWebAction(Sender).Template);
  with Wiki.DataSet do
    begin
      Insert;
      FieldByName('NAME').AsString:=Path;
      Title := FieldByName('NAME').AsString+'*';
    end;
  AResponse.Content := TFPWebAction(Sender).Template.GetContent;
  }
  if CreateAllowed then
    begin
      AResponse.Code := 404;
      AResponse.SendContent;
      Handled :=True;
    end
  else
    begin
      if not Assigned(fmError) then
        Application.CreateForm(TfmError,fmError);
      fmError.HandleRequest(ARequest,AResponse);
    end;
end;
procedure TfmWikiPage.TFPWebActionTemplateReplaceTag(Sender: TObject;
  const TagString: String; TagParams: TStringList; out ReplaceText: String);
var
  aRow: string;
  i: Integer;
  LinkValue: String;
  LinkDesc: String;
  LinkLocation: String;
  aWiki: TWikiList;
  LinkRValue: String;
begin
  if (AnsiCompareText(TagString, 'RSSFEED') = 0)
  or (AnsiCompareText(TagString, 'RSSFEED2') = 0)
  then
    begin
      ReplaceText := StringReplace(TagParams.Values['HEADER'],'~RSSFeed','/wiki/lastchanges.xml',[rfReplaceAll]);
    end
  else if AnsiCompareText(TagString, 'CONTENT') = 0 then
    begin
      ReplaceText := TagParams.Values['CHANGHEADER'];
      aRow := TagParams.Values['CHANGONEROW'];
      aWiki := TWikiList.Create(Self,Data);
      Data.SetFilter(aWiki,'',LAST_CHANGES_COUNT,'TIMESTAMPD','DESC');
      while not aWiki.DataSet.EOF do
        begin
          LinkValue := Data.BuildLink(aWiki.DataSet);
          LinkRValue := LinkValue;
          LinkDesc := HTMLEncode(Data.GetLinkDesc(LinkValue));
          LinkValue := copy(LinkValue,pos('@',LinkValue)+1,length(LinkValue));
          if rpos('{',LinkValue) > 0 then
            LinkValue := copy(LinkValue,0,rpos('{',LinkValue)-1)
          else if rpos('(',LinkValue) > 0 then
            LinkValue := copy(LinkValue,0,rpos('(',LinkValue)-1);
          if not IgnorePage(LinkValue) then
            begin
              if rpos('(',Linkdesc) > 0 then
                begin
                  LinkLocation := copy(LinkDesc,rpos('(',LinkDesc)+1,length(LinkDesc));
                  LinkLocation := copy(LinkLocation,0,length(LinkLocation)-1);
                  LinkDesc := copy(LinkDesc,0,rpos('(',LinkDesc)-1);
                end
              else if rpos('{',Linkdesc) > 0 then
                begin
                  LinkLocation := copy(LinkDesc,rpos('{',LinkDesc)+1,length(LinkDesc));
                  LinkLocation := copy(LinkLocation,0,length(LinkLocation)-1);
                  LinkDesc := copy(LinkDesc,0,rpos('{',LinkDesc)-1);
                end;
              ReplaceText := ReplaceText+StringReplace(
                                         StringReplace(
                                         StringReplace(
                                         StringReplace(
                                         StringReplace(
                                         StringReplace(aRow,'~LinkValue',LinkValue,[rfReplaceAll])
                                                           ,'~LinkName',LinkDesc,[rfReplaceAll])
                                                           ,'~LinkLocation',LinkLocation,[rfReplaceAll])
                                                           ,'~ProductImage','',[rfReplaceAll])
                                                           ,'~LastChanged',aWiki.TimeStamp.AsString,[rfReplaceAll])
                                                           ,'~LinkDesc',Data.GetLinkLongDesc(LinkRValue),[rfReplaceAll]);
            end;
          aWiki.DataSet.Next;
        end;
      ReplaceText := ReplaceText+TagParams.Values['CHANGFOOTER'];
    end
  else
    ReplaceStdTags(Sender,TagString,TagParams,ReplaceText);
  ReplaceText := UTF8ToSys(ReplaceText);
end;
procedure TfmWikiPage.SettemplateParams(aTemplate : TFPTemplate);
var
  sl: TStringList;
begin
  if not Assigned(Self) then
    begin
      Application.CreateForm(TfmWikiPage,fmWikiPage);
      Self := fmWikiPage;
    end;
  aTemplate.AllowTagParams := True;
  aTemplate.StartDelimiter := '{+';
  aTemplate.EndDelimiter := '+}';
//  if FTemplate.Count = 0 then
    begin
      with BaseApplication as IBaseConfig do
        FTemplate.LoadFromFile(AppendPathDelim(AppendPathDelim(Config.ReadString('DOCROOTPATH',''))+'templates')+'maintemplate.html');
      sl := TStringList.Create;
      try
        with BaseApplication as IBaseConfig do
          sl.LoadFromFile(AppendPathDelim(AppendPathDelim(Config.ReadString('DOCROOTPATH',''))+'templates')+'ajaxtemplate.html');
        FTemplate.Text:=StringReplace(FTemplate.text,'~MainTemplateContent',sl.Text,[]);
        with BaseApplication as IBaseConfig do
          begin
            FTemplate.Text:=StringReplace(FTemplate.text,'~WebsiteTitle',Config.ReadString('WebsiteTitle',''),[rfReplaceAll]);
            FTemplate.Text:=StringReplace(FTemplate.text,'~WebsiteURL',Config.ReadString('WebsiteURL',''),[rfReplaceAll]);
          end;
      finally
        sl.Free;
      end;
    end;
  aTemplate.Template := FTemplate.Text;
  aTemplate.OnReplaceTag :=@ReplaceMainTags;
end;
initialization
  RegisterHTTPModule('Wiki', TfmWikiPage);
end.

