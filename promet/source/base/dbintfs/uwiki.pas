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
*******************************************************************************}
unit uWiki;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, uBaseDbClasses, db, uBaseDBInterface, uDocuments,
  uBaseApplication, uBaseSearch, uIntfStrConsts,WikiToHtml;
type
  TKeywords = class(TBaseDbDataSet)
  public
    procedure DefineFields(aDataSet : TDataSet);override;
    procedure Open; override;
  end;
  TWikiList = class(TBaseDBList)
    procedure FKeywordsDataSetAfterInsert(aDataSet: TDataSet);
    procedure WikiListWikiLink(Inp: string; var Outp: string; aLevel: Integer=0
      );
  private
    FActiveTreeID: Variant;
    FKeywords: TKeywords;
    FKeywordDS : TDataSource;
    FOutDir,FOutSub,FOutExt : string;
    FOutTodo: TStringList;
  protected
  public
    function GetTyp: string;override;
    procedure DefineFields(aDataSet : TDataSet);override;
    function GetTextFieldName: string;override;
    function GetNumberFieldName : string;override;
    function GetDescriptionFieldName: string;override;
    function FindWikiPage(PageName : string;aDocreate : Boolean = False) : Boolean;
    function FindWikiFolder(PageName : string) : Boolean;
    function GetFullPath : string;
    function isDynamic : Boolean;
    function PageAsText : string;
    property ActiveTreeID : Variant read FActiveTreeID;
    constructor CreateEx(aOwner: TComponent; DM: TComponent;
       aConnection: TComponent=nil; aMasterdata: TDataSet=nil); override;
    destructor Destroy; override;
    function CreateTable: Boolean; override;
    property Keywords : TKeywords read FKeywords;
    function ExportToHTML(aFile: string; aInclude: TWikiIncludeFunc): Boolean;
  end;
implementation
uses Variants,htmltowiki,Utils;

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
            Add('TREEENTRY',ftLargeInt,0,false);
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

function TWikiList.FindWikiPage(PageName: string;aDocreate : Boolean = False): Boolean;
var
  aTree: TTree;
  aParent : LargeInt;
  aID: Variant;
  tmp: String;
begin
  Result := False;
  with Keywords.DataSet as IBaseDbFilter do
    Filter := '';
  aParent := 0;
  FActiveTreeID := aParent;
  aTree := TTree.CreateEx(Self,DataModule);
  aTree.Filter(TBaseDBModule(DataModule).QuoteField('TYPE')+'='+TBaseDBModule(DataModule).QuoteValue('W'));
  if pos('://',PageName) > 0 then exit;
  while pos('/',PageName) > 0 do
    begin
      aTree.Open;
      if aTree.DataSet.Locate('NAME;PARENT;TYPE',VarArrayOf([copy(PageName,0,pos('/',PageName)-1),aParent,'W']),[])
      or aTree.DataSet.Locate('NAME;PARENT;TYPE',VarArrayOf([copy(PageName,0,pos('/',PageName)-1),aParent,'W']),[loCaseInSensitive]) then
        begin
          tmp := aTree.FieldByName('NAME').AsString;
          PageName := copy(PageName,pos('/',PageName)+1,length(PageName));
          aParent := aTree.Id.AsVariant;
        end
      else
        begin
          TBaseDBModule(DataModule).SetFilter(aTree,'',0);
          if aTree.DataSet.Locate('NAME;PARENT;TYPE',VarArrayOf([copy(PageName,0,pos('/',PageName)-1),aParent,'W']),[])
          or aTree.DataSet.Locate('NAME;PARENT;TYPE',VarArrayOf([copy(PageName,0,pos('/',PageName)-1),aParent,'W']),[loCaseInSensitive]) then
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
      TBaseDBModule(DataModule).SetFilter(Self,TBaseDBModule(DataModule).QuoteField('TREEENTRY')+'='+TBaseDBModule(DataModule).QuoteValue(IntToStr(aParent)));
      Result := DataSet.Locate('TREEENTRY;NAME',VarArrayOf([aParent,PageName]),[loCaseInsensitive]);
      if not Result then
        begin
          TBaseDBModule(DataModule).SetFilter(Self,TBaseDBModule(DataModule).QuoteField('NAME')+'='+TBaseDBModule(DataModule).QuoteValue(PageName));
          Result := DataSet.Locate('TREEENTRY;NAME',VarArrayOf([Null,PageName]),[loCaseInsensitive]);
        end;
    end;
  if Result then Keywords.Open;
  FActiveTreeID := aParent;
  aTree.Free;
end;

function TWikiList.FindWikiFolder(PageName: string): Boolean;
var
  aParent: Variant;
  aTree: TTree;
begin
  Result := False;
  aParent := 0;
  aTree := TTree.CreateEx(Self,TBaseDBModule(DataModule));
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
          TBaseDBModule(DataModule).SetFilter(aTree,'',0);
          if aTree.DataSet.Locate('NAME;PARENT',VarArrayOf([copy(PageName,0,pos('/',PageName)-1),aParent]),[])
          or aTree.DataSet.Locate('NAME;PARENT',VarArrayOf([copy(PageName,0,pos('/',PageName)-1),aParent]),[loCaseInSensitive]) then
            begin
              PageName := copy(PageName,pos('/',PageName)+1,length(PageName));
              aParent := aTree.Id.AsVariant;
            end
          else
            begin
              result := False;
              aTree.Free;
              exit;
            end;
        end;
    end;
  TBaseDBModule(DataModule).SetFilter(Self,TBaseDBModule(DataModule).QuoteField('TREEENTRY')+'='+TBaseDBModule(DataModule).QuoteValue(VarToStr(aParent)));
  Result := Count>0;
  aTree.Free;
end;

function TWikiList.GetFullPath: string;
var
  aTree: TTree;
begin
  aTree := TTree.CreateEx(Self,DataModule);
  Result := FieldByName('NAME').AsString;
  aTree.Filter(TBaseDBModule(DataModule).QuoteField('SQL_ID')+'='+TBaseDBModule(DataModule).QuoteValue(DataSet.FieldByName('TREEENTRY').AsString));
  while aTree.Count>0 do
    begin
      Result := aTree.FieldByName('NAME').AsString+'/'+Result;
      aTree.Filter(TBaseDBModule(DataModule).QuoteField('SQL_ID')+'='+TBaseDBModule(DataModule).QuoteValue(aTree.FieldByName('PARENT').AsString));
    end;
  aTree.Free;
end;

function TWikiList.isDynamic: Boolean;
begin
  Result := False;
  if not Active then exit;
  Result := pos('[[INCLUDE:',Uppercase(FieldByName('DATA').AsString))>0;
end;

function TWikiList.PageAsText: string;
begin
  Result := StripHTML(WikiText2HTML(DataSet.FieldByName('DATA').AsString,'',''));
end;

constructor TWikiList.CreateEx(aOwner: TComponent; DM: TComponent;
  aConnection: TComponent; aMasterdata: TDataSet);
begin
  inherited CreateEx(aOwner, DM, aConnection, aMasterdata);
  FKeywords := TKeywords.CreateEx(Self,DM,aConnection);
  FKeywords.DataSet.AfterInsert:=@FKeywordsDataSetAfterInsert;
end;

destructor TWikiList.Destroy;
begin
  FreeAndNil(FKeywords);
  inherited Destroy;
end;

function TWikiList.CreateTable: Boolean;
begin
  Result:=inherited CreateTable;
  FKeywords.CreateTable;
end;

procedure TWikiList.WikiListWikiLink(Inp: string; var Outp: string;
  aLevel: Integer=0);
var
  tmp: String;
  sl: TStringList;
  aFN: String;
begin
  Outp := FOutSub+'/'+Inp+FOutExt;
  tmp := FOutDir+'/'+FOutSub+'/'+Inp;
  tmp := copy(tmp,0,rpos('/',tmp)-1);
  tmp := StringReplace(tmp,'/',DirectorySeparator,[rfReplaceAll]);
  tmp := StringReplace(tmp,DirectorySeparator+DirectorySeparator,DirectorySeparator,[rfReplaceAll]);
  ForceDirectories(tmp);
  sl := TStringList.Create;
  aFN := StringReplace(StringReplace(FOutDir+'/'+Outp,'/',DirectorySeparator,[rfReplaceAll]),'//','/',[rfReplaceAll]);
  if FOutTodo.IndexOf(Inp)=-1 then
    FOutTodo.Values[Inp] := GetFullPath;
end;

function TWikiList.ExportToHTML(aFile: string; aInclude: TWikiIncludeFunc
  ): Boolean;
var
  sl: TStringList;
  aPage: TWikiList;
  Outp: String;
  aFN: String;
  aRelPath: String;
  aLinkOffs: String;
  aRemPath: String;
  tmp: String;
begin
  WikiToHtml.OnWikiLink:=@WikiListWikiLink;
  WikiToHtml.OnWikiInclude:=aInclude;
  FOutDir := ExtractFileDir(aFile);
  FOutSub := copy(ExtractFileName(aFile),0,rpos('.',ExtractFileName(aFile))-1);
  FOutExt := ExtractFileExt(aFile);
  FOutTodo := TStringList.Create;
  sl := TStringList.Create;
  sl.Text := WikiText2HTML(DataSet.FieldByName('DATA').AsString,'','');
  sl.SaveToFile(aFile);
  sl.Free;
  while FOutTodo.Count>0 do
    begin
      aPage := TWikiList.CreateEx(nil,DataModule,Connection);
      Outp := FOutSub+'/'+FOutTodo.Names[0]+FOutExt;
      aFN := StringReplace(StringReplace(FOutDir+'/'+Outp,'/',DirectorySeparator,[rfReplaceAll]),'//','/',[rfReplaceAll]);
      if (not FileExists(aFN)) and aPage.FindWikiPage(FOutTodo.Names[0]) then
        begin
          //aRelPath := StringReplace(FileUtil.CreateRelativePath(FOutTodo.Names[0],FOutTodo.ValueFromIndex[0]),DirectorySeparator,'/',[rfReplaceAll]);
          tmp := Outp;
          tmp := copy(tmp,pos('/',tmp)+1,length(tmp));
          aLinkOffs := '';
          aRemPath := '';
          while copy(aRelPath,0,3)='../' do
            begin
              aRemPath := aRemPath+copy(tmp,0,pos('/',tmp)-1);
              tmp := copy(tmp,pos('/',tmp)+1,length(tmp));
              aLinkOffs := aLinkOffs+'../';
              aRelPath:=copy(aRelPath,4,length(aRelPath));
            end;
          sl.Text := WikiText2HTML(aPage.DataSet.FieldByName('DATA').AsString,aLinkOffs,aRemPath);
          sl.SaveToFile(aFN);
          sl.Free;
        end;
      FOutTodo.Delete(0);
      aPage.Free;
    end;
  FOutTodo.Free;
end;

initialization
end.

