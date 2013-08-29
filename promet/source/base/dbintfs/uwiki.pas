{*******************************************************************************
Dieser Sourcecode darf nicht ohne gültige Geheimhaltungsvereinbarung benutzt werden
und ohne gültigen Vertriebspartnervertrag weitergegeben oder kommerziell verwertet werden.
You have no permission to use this Source without valid NDA
and copy it without valid distribution partner agreement
Christian Ulrich
info@cu-tec.de
Created 01.07.2006
*******************************************************************************}
unit uWiki;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, uBaseDbClasses, db, uBaseDBInterface, uDocuments,
  uBaseApplication, uBaseSearch, uIntfStrConsts;
type
  TKeywords = class(TBaseDbDataSet)
  public
    procedure DefineFields(aDataSet : TDataSet);override;
    procedure Open; override;
  end;
  TWikiList = class(TBaseDBList)
    procedure FKeywordsDataSetAfterInsert(aDataSet: TDataSet);
  private
    FActiveTreeID: Variant;
    FKeywords: TKeywords;
    FKeywordDS : TDataSource;
  public
    function GetTyp: string;override;
    procedure DefineFields(aDataSet : TDataSet);override;
    function GetTextFieldName: string;override;
    function GetNumberFieldName : string;override;
    function GetDescriptionFieldName: string;override;
    function FindWikiPage(PageName : string;Docreate : Boolean = False) : Boolean;
    function PageAsText : string;
    property ActiveTreeID : Variant read FActiveTreeID;
    constructor Create(aOwner: TComponent; DM: TComponent;
       aConnection: TComponent=nil; aMasterdata: TDataSet=nil); override;
    destructor Destroy; override;
    function CreateTable: Boolean; override;
    property Keywords : TKeywords read FKeywords;
  end;
implementation
uses uData,Variants,HTMLConvert,WikiToHtml;

procedure TKeywords.DefineFields(aDataSet: TDataSet);
begin
  with aDataSet as IBaseManageDB do
    begin
      TableName := 'KEYWORDS';
      TableCaption:=strKeywords;
      if Assigned(ManagedFieldDefs) then
        with ManagedFieldDefs do
          begin
            Add('REF_ID_ID',ftLargeint,0,False);
            Add('KEYWORD',ftString,60,True);
          end;
    end;
end;

procedure TKeywords.Open;
begin
  with BaseApplication as IBaseDbInterface do
    begin
      with DataSet as IBaseDBFilter do
        begin
        if Assigned(Parent) then
          begin
            if Filter <> '' then
              begin
                if not Parent.Id.IsNull then
                  Filter := Filter+' AND '+Data.QuoteField('REF_ID_ID')+'='+Data.QuoteValue(Parent.Id.AsString)
                else
                  Filter := Filter+' AND '+Data.QuoteField('REF_ID_ID')+'= 0';
              end
            else
              begin
              if not Parent.Id.IsNull then
                Filter := Data.QuoteField('REF_ID_ID')+'='+Data.QuoteValue(Parent.Id.AsString)
              else
                Filter := Data.QuoteField('REF_ID_ID')+'= 0';
              end;
          end;
        end;
    end;
  inherited Open;
end;

procedure TWikiList.FKeywordsDataSetAfterInsert(aDataSet: TDataSet);
begin
  aDataSet.FieldByName('REF_ID_ID').AsVariant:=Self.Id.AsVariant;
end;
function TWikiList.GetTyp: string;
begin
  Result := 'W';
end;

procedure TWikiList.DefineFields(aDataSet: TDataSet);
begin
  with aDataSet as IBaseManageDB do
    begin
      TableName := 'WIKI';
      TableCaption := strWiki;
      if Assigned(ManagedFieldDefs) then
        with ManagedFieldDefs do
          begin
            Add('TREEENTRY',ftLargeint,0,false);
            Add('NAME',ftString,40,True);
            Add('CAPTION',ftString,120,False);
            Add('LANGUAGE',ftString,3,False);
            Add('CREATEDBY',ftString,4,False);
            Add('CHANGEDBY',ftString,4,False);
            Add('DATA',ftMemo,0,False);
          end;
    end;
end;
function TWikiList.GetTextFieldName: string;
begin
  Result:='NAME';
end;
function TWikiList.GetNumberFieldName: string;
begin
  Result:='SQL_ID';
end;

function TWikiList.GetDescriptionFieldName: string;
begin
  Result:='DATA';
end;

function TWikiList.FindWikiPage(PageName: string;Docreate : Boolean = False): Boolean;
var
  aTree: TTree;
  aParent : Variant;
  aID: Variant;
begin
  with Keywords.DataSet as IBaseDbFilter do
    Filter := '';
  aParent := 0;
  FActiveTreeID := aParent;
  aTree := TTree.Create(Self,Data);
  if copy(PageName,0,7) = 'http://' then exit;
  while pos('/',PageName) > 0 do
    begin
      aTree.Open;
      if aTree.DataSet.Locate('NAME;PARENT',VarArrayOf([copy(PageName,0,pos('/',PageName)-1),aParent]),[])
      or aTree.DataSet.Locate('NAME;PARENT',VarArrayOf([copy(PageName,0,pos('/',PageName)-1),aParent]),[loCaseInSensitive]) then
        begin
          PageName := copy(PageName,pos('/',PageName)+1,length(PageName));
          aParent := aTree.Id.AsVariant;
        end
      else
        begin
          Data.SetFilter(aTree,'',0);
          if aTree.DataSet.Locate('NAME;PARENT',VarArrayOf([copy(PageName,0,pos('/',PageName)-1),aParent]),[])
          or aTree.DataSet.Locate('NAME;PARENT',VarArrayOf([copy(PageName,0,pos('/',PageName)-1),aParent]),[loCaseInSensitive]) then
            begin
              PageName := copy(PageName,pos('/',PageName)+1,length(PageName));
              aParent := aTree.Id.AsVariant;
            end
          else
            begin
              with aTree.DataSet do
                begin
                  Append;
                  FieldByName('TYPE').AsString := 'W';
                  FieldByName('NAME').AsString := copy(PageName,0,pos('/',PageName)-1);
                  if aParent <> TREE_ID_WIKI_UNSORTED then
                    FieldByName('PARENT').AsVariant := aParent
                  else
                    FieldByName('PARENT').AsInteger := 0;
                  Post;
                  PageName := copy(PageName,pos('/',PageName)+1,length(PageName));
                  aParent := aTree.Id.AsVariant;
                end;
            end;
        end;
    end;
  Result := DataSet.Active and DataSet.Locate('TREEENTRY;NAME',VarArrayOf([aParent,PageName]),[]);
  if not Result then
    begin
      Data.SetFilter(Self,Data.QuoteField('TREEENTRY')+'='+Data.QuoteValue(VarToStr(aParent)));
      Result := DataSet.Locate('TREEENTRY;NAME',VarArrayOf([aParent,PageName]),[loCaseInsensitive]);
      if not Result then
        begin
          Data.SetFilter(Self,Data.QuoteField('NAME')+'='+Data.QuoteValue(PageName));
          Result := DataSet.Locate('TREEENTRY;NAME',VarArrayOf([Null,PageName]),[loCaseInsensitive]);
        end;
    end;
  if Result then Keywords.Open;
  FActiveTreeID := aParent;
  aTree.Free;
end;
function TWikiList.PageAsText: string;
begin
  Result := HTMLToTxt(WikiText2HTML(DataSet.FieldByName('DATA').AsString,'',''));
end;

constructor TWikiList.Create(aOwner: TComponent; DM: TComponent;
  aConnection: TComponent; aMasterdata: TDataSet);
begin
  inherited Create(aOwner, DM, aConnection, aMasterdata);
  FKeywords := TKeywords.Create(Self,DM,aConnection);
  FKeywords.DataSet.AfterInsert:=@FKeywordsDataSetAfterInsert;
end;

destructor TWikiList.Destroy;
begin
  if Assigned(FKeywords) then
    FreeAndNil(FKeywords);
  inherited Destroy;
end;

function TWikiList.CreateTable: Boolean;
begin
  Result:=inherited CreateTable;
  FKeywords.CreateTable;
end;

initialization
end.

