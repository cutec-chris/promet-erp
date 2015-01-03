unit uMain; 
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, FileUtil, XMLPropStorage, HTTPDefs, websession, fpHTTP,
  fpWeb, fphtml, uData, db, fpTemplate, Utils, uWiki, uDocuments,uBaseSearch,
  uBaseDBClasses,uIntfStrConsts;
type
  TWikiPage = class(TFPWebModule)
    Mainmenue: TXMLPropStorage;
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
    procedure DataModuleGetAction(Sender: TObject; ARequest: TRequest;
      var ActionName: String);
    procedure FSearchItemFound(aIdent: string; aName: string; aStatus: string;
      aLink: string; aItem: TBaseDBList=nil);
    procedure ReplaceMainTags(Sender: TObject; const TagString: String;
      TagParams: TStringList; out ReplaceText: String);
    procedure SearchTagreplace(Sender: TObject; const TagString: String;
      TagParams: TStringList; out ReplaceText: String);
    procedure Showrequest(Sender: TObject; ARequest: TRequest;
      AResponse: TResponse; var Handled: Boolean);
    procedure SearchRequest(Sender: TObject; ARequest: TRequest;
      AResponse: TResponse; var Handled: Boolean);
    procedure Createrequest(Sender: TObject; ARequest: TRequest;
      AResponse: TResponse; var Handled: Boolean);
  private
    { private declarations }
    Title : string;
    Path: String;
    FSearch: TSearch;
    FSearchResult : TStringList;
    procedure ConvertImage(Image: string; var OutFile: string);
    procedure SettemplateParams(aTemplate : TFPTemplate);
    procedure ReplaceStdTags(Sender: TObject; const TagString: String;TagParams: TStringList; out ReplaceText: String);
  public
    { public declarations }
    Wiki : TWikiList;
    Documents : TDocument;
  end;
var
  WikiPage: TWikiPage;

implementation
uses WikitoHTML, uAppconsts,uBaseApplication,uBaseDbInterface,Variants;
{$R *.lfm}
procedure TWikiPage.DataModuleCreate(Sender: TObject);
begin
  with BaseApplication as IBaseApplication do
    begin
      SetConfigName('webconfig');
      RestoreConfig;
      Login;
    end;
  Wiki := TWikiList.Create(nil);
  Documents := TDocument.Create(nil);
  Data.Tree.Open;
end;
procedure TWikiPage.DataModuleDestroy(Sender: TObject);
begin
// Wiki.Destroy;
// Documents.Destroy;
end;
procedure TWikiPage.DataModuleGetAction(Sender: TObject; ARequest: TRequest;
  var ActionName: String);
var
  Result: string;
  aParent : Variant = TREE_ID_WIKI_UNSORTED;
  Found: Boolean;
  PageName: String;
begin
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
  if (ARequest.PathInfo = '') or (Result = 'wiki') or (Result = 'show') then
    begin
      Result := 'show';
      Path := copy(ARequest.PathInfo,2,length(ARequest.PathInfo));
      Path := copy(Path,pos('/',Path)+1,length(Path));
      if Path = '' then
        with BaseApplication as IBaseApplication do
          Path := Config.ReadString('INDEX','INDEX');
      Path := trim(Path);
    end;
  Found := False;
  PageName := Path;
  while pos('/',PageName) > 0 do
    begin
      if Data.Tree.DataSet.Locate('NAME',copy(PageName,0,pos('/',PageName)-1),[loCaseInsensitive]) then
        begin
          PageName := copy(PageName,pos('/',PageName)+1,length(PageName));
          aParent := Data.Tree.Id.AsVariant;
        end
      else
        begin
          Data.SetFilter(Data.Tree,'',0,'','ASC',False,True,True);
          if Data.Tree.DataSet.Locate('NAME',copy(PageName,0,pos('/',PageName)-1),[loCaseInsensitive]) then
            begin
              PageName := copy(PageName,pos('/',PageName)+1,length(PageName));
              aParent := Data.Tree.Id.AsVariant;
            end
          else break;
        end;
    end;
  PageName := Stringreplace(PageName,' ','_',[rfReplaceAll]);
  Found := Wiki.DataSet.Active and Wiki.DataSet.Locate('TREEENTRY;NAME',VarArrayOf([aParent,PageName]),[]);
  if not Found then
    begin
      Data.SetFilter(Wiki,Data.QuoteField('TREEENTRY')+'='+Data.QuoteValue(VarToStr(aParent)));
      Found := Wiki.DataSet.Locate('TREEENTRY;NAME',VarArrayOf([aParent,PageName]),[loCaseInsensitive]);
      if not Found then
        begin
          Data.SetFilter(Wiki,Data.QuoteField('NAME')+'='+Data.QuoteValue(PageName));
          Found := Wiki.DataSet.Locate('TREEENTRY;NAME',varArrayOf([Null,PageName]),[loCaseInsensitive]);
        end;
    end;
  if Result = 'show' then
    begin
      if not Found then
        begin
          Result := 'create';
        end;
    end;
  ActionName := Result;
{
  //Find Activeuser
  Data.SetFilter(Data.ActiveUsers,'HOST='+Data.QuoteValue(ARequest.RemoteAddr)+' and CLIENT='+Data.QuoteValue(ExtractFileName(Paramstr(0))));
  if Data.ActiveUsers.DataSet.RecordCount > 0 then
    begin //Users with same IP
      while Data.ActiveUsers.DataSet.RecordCount > 1 do
        Data.ActiveUsers.DataSet.Delete;
      with Data.ActiveUsers.DataSet do
        begin
          Edit;
          FieldByName('TIMESTAMPD').AsDateTime := Now();
          if FieldDefs.IndexOf('TIMESTAMPT') <> -1 then
            FieldByName('TIMESTAMPT').AsFloat    := Frac(Now());
          FieldByName('EXPIRES').AsDateTime := Now()+0.1;
          Post;
        end;
    end
  else //New Session
    begin
      //Create new Entry
      with Data.ActiveUsers.DataSet do
        begin
          Insert;
          FieldByName('NAME').AsString:=ARequest.RemoteAddr;
          FieldByName('ClIENT').AsString:=ExtractFileName(Paramstr(0));
          FieldByName('HOST').AsString:=ARequest.RemoteAddr;
          FieldByName('VERSION').AsString:=StringReplace(Format('Version %f Build %d',[vVersion,vRevision]),',','.',[rfReplaceAll]);
          FieldByName('TIMESTAMPD').AsDateTime := Now();
          FieldByName('EXPIRES').AsDateTime := Now()+0.1;
          if FieldDefs.IndexOf('TIMESTAMPT') <> -1 then
            FieldByName('TIMESTAMPT').AsFloat    := Frac(Now());
          Post;
        end;
    end;
}
end;
procedure TWikiPage.FSearchItemFound(aIdent: string; aName: string;
  aStatus: string; aLink: string; aItem: TBaseDBList=nil);
begin
  FSearchResult.Add(aLink);
end;
procedure TWikiPage.ReplaceStdTags(Sender: TObject; const TagString: String;
  TagParams: TStringList; out ReplaceText: String);
var
  aRow: string;
  i: Integer;
begin
  if AnsiCompareText(TagString, 'Title') = 0 then
    begin
      ReplaceText := Title;
    end
  else if AnsiCompareText(TagString, 'DateTime') = 0 then
    ReplaceText := FormatDateTime(TagParams.Values['FORMAT'], Now)
  else if AnsiCompareText(TagString, 'TOPNAVIGATION') = 0 then
    begin
      ReplaceText := Tagparams.Values['HEADER'];
      aRow := TagParams.Values['ONEROW'];
      for i := 0 to Mainmenue.StoredValues.Count-1 do
        ReplaceText := ReplaceText+StringReplace(
                                   StringReplace(aRow,'~LinkValue',Mainmenue.StoredValues[i].Value,[rfReplaceAll])
                                                     ,'~NameValue',Mainmenue.StoredValues[i].Name,[rfReplaceAll]);
      ReplaceText := ReplaceText+Tagparams.Values['FOOTER'];
    end
  else if AnsiCompareText(TagString, 'SEARCHBAR') = 0 then
    begin
      ReplaceText := TagParams.Values['SEARCHFORM'];
    end;
end;
procedure TWikiPage.ConvertImage(Image : string;var OutFile : string);
var
  ImageFile: String;
  fs: TFileStream;
begin
  with BaseApplication as IBaseApplication do
    ImageFile := AppendPathDelim(AppendPathDelim(Config.ReadString('DOCROOTPATH',''))+'images')+ValidateFileName(Image);
  if not FileExists(ImageFile) then
    begin
      Data.SetFilter(Documents,'"TYPE"=''W'' and "NAME"='+Data.QuoteValue(copy(ExtractFileName(Image),0,rpos('.',ExtractFileName(Image))-1)),1);
      if Documents.DataSet.RecordCount > 0 then
        begin
          fs := TFileStream.Create(ImageFile,fmCreate);
          Data.BlobFieldToStream(Documents.DataSet,'DOCUMENT',fs);
          fs.Free;
        end;
    end;
  OutFile := '/images/'+ValidateFileName(Image);
end;
procedure TWikiPage.ReplaceMainTags(Sender: TObject; const TagString: String;
  TagParams: TStringList; out ReplaceText: String);
begin
  if AnsiCompareText(TagString, 'CONTENT') = 0 then
    begin
      ReplaceText := TagParams.Values['CHEADER'];
      WikiToHTML.OnConvertImage:=@ConvertImage;
      with BaseApplication as IBaseApplication do
        ReplaceText := Replacetext+UniToSys(Stringreplace(Tagparams.Values['CCONTENT'],'~Content',WikiText2HTML(Wiki.FieldByName('DATA').AsString,'/cgi-bin/wiki.cgi/wiki/',Config.ReadString('REMOVELINKOFFSET',''),True),[rfReplaceAll]));
      ReplaceText := ReplaceText+TagParams.Values['CFOOTER'];
    end
  else
    ReplaceStdTags(Sender,TagString,TagParams,ReplaceText);
  ReplaceText := UniToSys(ReplaceText);
end;
procedure TWikiPage.SearchTagReplace(Sender: TObject; const TagString: String;
  TagParams: TStringList; out ReplaceText: String);
var
  aRow: string;
  i: Integer;
  LinkValue: String;
begin
  if AnsiCompareText(TagString, 'CONTENT') = 0 then
    begin
      ReplaceText := TagParams.Values['CHEADER'];
      aRow := TagParams.Values['CONEROW'];
      for i := 0 to FSearchResult.Count-1 do
        begin
          LinkValue := 'wiki/'+copy(FSearchResult[i],pos('@',FSearchResult[i])+1,length(FSearchResult[i]));
          if rpos('{',LinkValue) > 0 then
            LinkValue := copy(LinkValue,0,rpos('{',LinkValue)-1)
          else if rpos('(',LinkValue) > 0 then
            LinkValue := copy(LinkValue,0,rpos('(',LinkValue)-1);
          ReplaceText := ReplaceText+StringReplace(
                                     StringReplace(aRow,'~LinkValue',LinkValue,[rfReplaceAll])
                                                       ,'~LinkName',Data.GetLinkDesc(FSearchResult[i]),[rfReplaceAll]);
        end;
      ReplaceText := ReplaceText+TagParams.Values['CFOOTER'];
    end
  else
    ReplaceStdTags(Sender,TagString,TagParams,ReplaceText);
  ReplaceText := UniToSys(ReplaceText);
end;
procedure TWikiPage.Showrequest(Sender: TObject; ARequest: TRequest;
  AResponse: TResponse; var Handled: Boolean);
var
  C: TCookie;
begin
  {
  If Session is TFPWebSession then
    begin
      C:=AResponse.Cookies.FindCookie((Session as TFPWebSession).SessionCookie);
    end;
  }
  SettemplateParams(TFPWebAction(Sender).Template);
  Title := Wiki.FieldByName('CAPTION').AsString;
  if trim(Title) = '' then
    Title := Wiki.FieldByName('NAME').AsString;
  AResponse.Content := TFPWebAction(Sender).Template.GetContent;
  Handled := true;
end;
procedure TWikiPage.SearchRequest(Sender: TObject; ARequest: TRequest;
  AResponse: TResponse; var Handled: Boolean);
var
  Locations : TSearchLocations;
begin
  SettemplateParams(TFPWebAction(Sender).Template);
  Title := 'Suche';
  TFPWebAction(Sender).Template.OnReplaceTag :=@SearchTagreplace;
  Setlength(Locations,1);
  Locations[0] := strWiki;
  FSearch := TSearch.Create([fsFulltext],Locations,False,10);
  FSearch.OnItemFound:=@FSearchItemFound;
  FSearchResult := TStringList.Create;
  FSearch.Start(ARequest.QueryString);
  FreeAndNil(FSearch);
  AResponse.Content := TFPWebAction(Sender).Template.GetContent;
  FreeAndNil(FSearchResult);
  Handled := true;
end;
procedure TWikiPage.Createrequest(Sender: TObject; ARequest: TRequest;
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
  AResponse.CodeText:='Not Found';
  Aresponse.Code:=404;
  Handled :=True;
end;
procedure TWikiPage.SettemplateParams(aTemplate : TFPTemplate);
begin
  aTemplate.AllowTagParams := True;
  aTemplate.StartDelimiter := '{+';
  aTemplate.EndDelimiter := '+}';
  with BaseApplication as IBaseApplication do
    aTemplate.FileName := AppendPathDelim(AppendPathDelim(Config.ReadString('DOCROOTPATH',''))+'templates')+'maintemplate.html';
  aTemplate.OnReplaceTag :=@ReplaceMainTags;
end;
initialization
  RegisterHTTPModule('Wiki', TWikiPage);
end.
