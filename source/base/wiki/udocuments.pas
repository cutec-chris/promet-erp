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
unit uDocuments;
{$mode objfpc}
{$H+}
interface
uses
  Classes, SysUtils, db, uBaseDBClasses, Utils,
  usimpleprocess,ubasedatasetinterfaces2;
type
  { TDocuments }

  TDocuments = class(TBaseDBList)
  private
    FBaseID: string;
    FBaseLanguage: Variant;
    FBaseTyp: Variant;
    FBaseVersion: Variant;
    FParentID: Variant;
    FRefID: LargeInt;
    function GetCreationDate: TDateTime;virtual;
    function GetFileName: string;
    function GetFileSize: Int64;virtual;
    function GetIsDir: Boolean;
    function GetIsLink: Boolean;
    function GetLastModified: TDateTime;virtual;
    procedure SetParentID(AValue: Variant);
  public
    procedure Open;override;
    function GetTyp: string;override;
    property Ref_ID : LargeInt read FRefID write FRefID;
    property BaseTyp : Variant read FBaseTyp write FBaseTyp;
    property BaseID : string read FBaseID write FBaseID;
    property BaseVersion : Variant read FBaseVersion write FBaseVersion;
    property BaseLanguage : Variant read FBaseLanguage write FBaseLanguage;
    property ParentID : Variant read FParentID write SetParentID;
    function GetNumberFieldName : string;override;
    property FileName : string read GetFileName;
    procedure SelectByNumber(aNumber : Variant);virtual;
    procedure SelectByID(aID : LargeInt);
    procedure SelectByLink(aLink : string);
    procedure DefineFields(aDataSet : TDataSet);override;
    procedure Select(aID : LargeInt;aType : string;aParent : LargeInt = 0);overload;virtual;
    procedure Select(aID : largeInt;aType : string;aTID : string;aVersion : Variant;aLanguage : Variant;aParent : LargeInt = 0);overload;virtual;
    procedure SelectByReference(aID : Variant);
    function OpenPath(aPath : string;aPathDelim : string = PathDelim) : Boolean;
    property IsDir : Boolean read GetIsDir;
    property IsLink : Boolean read GetIsLink;
    function GotoLink : Boolean;
    function SelectFile(aFilename : string) : Boolean;
    function Delete : Boolean;override;
  published
    property Ref_ID_ID : Int64 read FRef_ID_ID write FRef_ID_ID;
    property Typ: Boolean read FType write FType;
    property IsDir: Boolean read FIsDir write FIsDir;
    property IsLink: Boolean read  write False);
    property Parent: Integer read  write ,false);
    property Number : Int64 read  write ;
    property Name: string index 240,True);
    property Extension: string index 15,false);
    property Revision: Integer read  write ,True);
    property Status: string index 4,false);
    property Size : Int64 read  write false);
    property Checksum: string index 32,False);
    property Full: Boolean read  write False);
    property Date: TDateTime read  write ;;
    property CreatedBy: string index 4,False);
    property ChangedBy: string index 4,False);
    property Message: string index 100,False);
    property Fulltext: string read write ;
    //property DOCUMENT',ftBlob,0,False);
  end;
  TDocumentTemplates = class(TBaseDBList)
  public
    procedure DefineFields(aDataSet : TDataSet);override;
  end;
  TCheckCheckinFilesEvent = function(aFiles : TStrings;Directory: string;var Desc : string) : Boolean of object;
  TCheckCheckOutFileEvent = procedure(aFile : string) of object;
  TMimeTypes = class(TBaseDBDataset)
  public
    procedure DefineFields(aDataSet: TDataSet); override;
    procedure Open; override;
    procedure FillDefaults(aDataSet: TDataSet); override;
  end;
  TDocumentActions = class(TBaseDBDataset)
  public
    procedure DefineFields(aDataSet: TDataSet); override;
    procedure Open; override;
    procedure FillDefaults(aDataSet: TDataSet); override;
  end;

  { TDocument }

  TDocument = class(TDocuments)
  private
    FAfterCheckinFiles: TNotifyEvent;
    FBaseParent: TDocuments;
    FDocumentActions: TDocumentActions;
    FMimeTypes: TMimeTypes;
    FOnCheckCheckinFiles: TCheckCheckinFilesEvent;
    FExtDesc : string;
    FOnCheckCheckOutFile: TCheckCheckOutFileEvent;
    function GetCreationDate: TDateTime;override;
    function GetFileSize: Int64;override;
    function GetLastModified: TDateTime;override;
    procedure SetbaseParent(AValue: TDocuments);
  protected
  public
    constructor CreateEx(aOwner: TComponent; DM: TComponent;
       aConnection: TComponent=nil; aMasterdata: TDataSet=nil); override;
    destructor Destroy; override;
    function CreateTable : Boolean;override;
    procedure Select(aID : LargeInt;aType : string;aParent : LargeInt);overload;override;
    procedure Select(aID : LargeInt;aType : string;aTID : string;aVersion : Variant;aLanguage : Variant;aParent : LargeInt = 0);overload;override;
    procedure SelectByNumber(aNumber : Variant);override;
    procedure FillDefaults(aDataSet : TDataSet);override;
    procedure AddFromStream(eName,Extension: string; Stream: TStream;aText : string = ''; AddDate: TDateTime = 0;SetText : Boolean = True);
    procedure AddFromFile(aFilename : string;aText : string = '';AddDate : TDateTime = 0);
    procedure AddFromDir(aFilename : string;aText : string = '';DoDelete : Boolean = False;AddDate : TDateTime = 0);
    procedure AddFromLink(aLink : string);
    procedure MoveTo(aID : largeInt;aType : string;aTID : string;aVersion : Variant;aLanguage : Variant;aParent : LargeInt);
    procedure CreateDirectory(aName : string);
    function GetCheckoutPath(Directory: string; TempID: string): string;
    function GetIDCheckoutPath(Directory: string; TempID: string): string;
    function DoCheckout(Directory : string;aRevision : Integer = -1;aNewFileName : string = '') : Boolean;
    function CheckoutToStream(aStream : TStream;aRevision : Integer = -1;aSize : Integer = -1) : Boolean;
    procedure CheckInFromStream(aStream: TStream; Desc: string='');
    function CollectCheckInFiles(Directory : string) : TStrings;
    function CheckCheckInFiles(aFiles : TStrings;Directory: string) : Boolean;
    function CheckinFiles(aFiles : TStrings;Directory: string;Desc : string = '') : Boolean;
    property BaseParent : TDocuments read FBaseParent write SetbaseParent;
    property OnCheckCheckinFiles : TCheckCheckinFilesEvent read FOnCheckCheckinFiles write FOnCheckCheckinFiles;
    property AftercheckInFiles : TNotifyEvent read FAfterCheckinFiles write FAfterCheckInFiles;
    property OnCheckCheckOutFile : TCheckCheckOutFileEvent read FOnCheckCheckOutFile write FOnCheckCheckOutFile;
    function Delete : Boolean;override;
    property MimeTypes : TMimeTypes read FMimeTypes;
    property DocumentActions : TDocumentActions read FDocumentActions;
  end;
implementation
uses uBaseDBInterface,uBaseApplication, uBaseApplicationTools,md5,
  Variants,Process,uthumbnails;
resourcestring
  strFailedCreatingDiff         = 'konnte Differenzdatei von Datei %s nicht erstellen';
  strInvalidLink                = 'Dieser Link ist auf dieser Datenbank ungültig !';
  strLinkNotFound               = 'Verweis konnte nicht gefunden werden !';
var FUsedFields : string;
procedure TDocumentActions.DefineFields(aDataSet: TDataSet);
begin
  with aDataSet as IBaseManageDB do
    begin
      TableName := 'DOCUMENTACTIONS';
      if Assigned(ManagedFieldDefs) then
        with ManagedFieldDefs do
          begin
            property HOST: string index 100,False);
            property NUMBER: Integer read  write ,False);
            property CODIR: Boolean read  write False);
            property ADDFILES: Boolean read  write False);
            property ACTION: string index 1,False);
            property ACTIONCMD',ftMemo,0,False);
            property USESTARTER: Boolean read  write False);
          end;
      if Assigned(ManagedIndexdefs) then
        with ManagedIndexDefs do
          begin
            property HOST','HOST',[]);
            property NUMBER','NUMBER',[]);
          end;
    end;
end;

procedure TDocumentActions.Open;
begin
  with BaseApplication as IBaseDbInterface do
    begin
      with DataSet as IBaseDBFilter do
        begin
          Limit := 0;
          BaseFilter := Data.QuoteField('HOST')+'='+Data.QuoteValue(GetSystemName);
        end;
    end;
  inherited Open;
end;

procedure TDocumentActions.FillDefaults(aDataSet: TDataSet);
begin
  inherited FillDefaults(aDataSet);
  aDataSet.FieldByName('HOST').AsString:=GetSystemName;
end;

procedure TMimeTypes.DefineFields(aDataSet: TDataSet);
begin
  with aDataSet as IBaseManageDB do
    begin
      TableName := 'MIMETYPES';
      if Assigned(ManagedFieldDefs) then
        with ManagedFieldDefs do
          begin
            property HOST: string index 100,False);
            property EXTENSIONS: string index 150,False);
            property MIME: string index 80,True);
            property ADDFILES: Boolean read  write False);
            property DESC: string index 60,False);
            property VIEW: string index 255,False);
            property EDIT: string index 255,False);
            property PRINT: string index 255,False);
            property USESTARTER: Boolean read  write False);
            property TIMESTAMP',ftDate,0,False);
          end;
    end;
end;

procedure TMimeTypes.Open;
begin
  with BaseApplication as IBaseDbInterface do
    begin
      with DataSet as IBaseDBFilter do
        begin
          Limit := 0;
          BaseFilter := Data.QuoteField('HOST')+'='+Data.QuoteValue(GetSystemName);
        end;
    end;
  inherited Open;
end;

procedure TMimeTypes.FillDefaults(aDataSet: TDataSet);
begin
  inherited FillDefaults(aDataSet);
  aDataSet.FieldByName('HOST').AsString:=GetSystemName;
end;

procedure TDocument.SetbaseParent(AValue: TDocuments);
begin
  if FBaseParent=AValue then Exit;
  FBaseParent:=AValue;
  Ref_ID := FBaseParent.Ref_ID;
  FBaseTyp:=FBaseParent.BaseTyp;
  FBaseID:=FBaseParent.BaseID;
  FBaseVersion:=FBaseParent.BaseVersion;
  FBaseLanguage:=FBaseParent.BaseLanguage;
  if FBaseParent.Count > 0 then
    ParentID:=FBaseParent.FieldByName('NUMBER').AsVariant;
end;
constructor TDocument.CreateEx(aOwner: TComponent; DM: TComponent;
  aConnection: TComponent; aMasterdata: TDataSet);
begin
  inherited CreateEx(aOwner, DM, aConnection, aMasterdata);
  with BaseApplication as IBaseDbInterface do
    begin
      with DataSet as IBaseDBFilter do
        begin
          Limit := 0;
          UsePermissions:=False;
        end;
    end;
  FMimeTypes := TMimeTypes.CreateEx(Self,DataModule,aConnection,nil);
  FDocumentActions := TDocumentActions.CreateEx(Self,DataModule,aConnection,nil);
end;

destructor TDocument.Destroy;
begin
  FMimeTypes.Free;
  FDocumentActions.Free;
  inherited Destroy;
end;
function TDocument.CreateTable : Boolean;
begin
  Result := inherited CreateTable;
  FDocumentActions.CreateTable;
  FMimeTypes.CreateTable;
end;
function TDocument.GetCreationDate: TDateTime;
var
  aRec: LargeInt;
begin
  if Count = 0 then exit;
  aRec := GetBookmark;
  DataSet.First;
  Result := DataSet.FieldByName('DATE').AsDateTime;
  GotoBookmark(aRec);
end;
function TDocument.GetFileSize: Int64;
var
  aRec: LargeInt;
begin
  Result := 0;
  if Count = 0 then exit;
  aRec := GetBookmark;
  DataSet.First;
  Result := DataSet.FieldByName('SIZE').AsInteger;
  GotoBookmark(aRec);
end;
function TDocument.GetLastModified: TDateTime;
var
  aRec: LargeInt;
begin
  if Count = 0 then exit;
  aRec := GetBookmark;
  DataSet.First;
  Result := DataSet.FieldByName('TIMESTAMPD').AsDateTime;
  GotoBookmark(aRec);
end;
procedure TDocument.Select(aID: LargeInt;aType : string;aParent: LargeInt);
var
  tmpfields: String;
begin
  with BaseApplication as IBaseDbInterface do
    begin
      with Self.DataSet as IBaseDBFilter,Self.DataSet as IBaseManageDB do
        begin
          tmpfields := GetUsedFields;
          Filter := Data.QuoteField(TableName)+'.'+Data.QuoteField('REF_ID_ID')+'='+Data.QuoteValue(IntToStr(aID))+' and '+Data.QuoteField('PARENT')+'='+Data.QuoteValue(IntToStr(aParent));
          SortDirection := sdAscending;
          SortFields := 'REVISION,FULL';
          Fields := tmpFields;
          Limit := 0;
        end;
    end;
  FRefID := aID;
  FBaseID := '';
  FBaseTyp:= aType;
  FBaseVersion := Null;
  FBaseLanguage := Null;
  ParentID := aParent;
end;

procedure TDocument.Select(aID: LargeInt; aType: string; aTID: string;
  aVersion: Variant; aLanguage: Variant; aParent: LargeInt);
var
  tmpfields: String;
begin
  with BaseApplication as IBaseDbInterface do
    begin
      with DataSet as IBaseDBFilter,DataSet as IBaseManageDB do
        begin
          tmpfields := GetUsedFields;
          Fields := tmpfields;
          if pos(Data.QuoteField('ID'),tmpfields) > 0 then
            begin
              Filter := '((('+Data.QuoteField(TableName)+'.'+Data.QuoteField('REF_ID_ID')+'='+Data.QuoteValue(IntToStr(aID))+') or ('
                       +Data.ProcessTerm(Data.QuoteField('TYPE')+'='+Data.QuoteValue(aType))+' and '
                       +Data.ProcessTerm(Data.QuoteField('ID')+'='+Data.QuoteValue(aTID))+' and '
                       +{ProcessTerm(}Data.QuoteField('VERSION')+'='+VarToStr(aVersion){)}+' and '
                       +Data.ProcessTerm(Data.QuoteField('LANGUAGE')+'='+VarToStr(aLanguage))+')) and '
                       +Data.QuoteField('PARENT')+'='+Data.QuoteValue(IntToStr(aParent))+')';
            end
          else
            begin
              Select(aID,aType,aParent);
              exit;
            end;
          SortDirection := sdAscending;
          SortFields := 'REVISION,FULL';
          Limit := 0;
        end;
    end;
  FRefID := aID;
  FBaseTyp := aType;
  FBaseID := aTID;
  FBaseVersion := aVersion;
  FBaseLanguage := aLanguage;
  ParentID := aParent;
end;
procedure TDocument.SelectByNumber(aNumber: Variant);
begin
  inherited SelectByNumber(aNumber);
  with BaseApplication as IBaseDbInterface do
    begin
      with Self.DataSet as IBaseDBFilter do
        begin
          SortFields := 'REVISION,FULL';
          SortDirection := sdAscending;
          Limit := 0;
        end;
    end;
end;
procedure TDocument.AddFromStream(eName, Extension: string; Stream: TStream;
  aText: string; AddDate: TDateTime; SetText: Boolean);
var
  DocID: LargeInt;
  ss: TStringStream;
  OldPos: Int64;
begin
  with BaseApplication as IBaseDbInterface do
    begin
      Append;
      with DataSet do
        begin
          //TODO:Check if this document already is in the list
          DocID := Data.GetUniID(Connection);
          FieldByName('ISDIR').AsString := 'N';
          FieldByName('REVISION').AsString := '0';
          //TODO: Largeint ????
          FieldByName('NUMBER').AsVariant := DocID;
          FieldByName('NAME').AsString := copy(eName,0,FieldByName('NAME').Size);
          FieldByName('EXTENSION').AsString := Extension;
          FieldByName('SIZE').AsInteger := Stream.Size;
          FieldByName('FULL').AsString:='Y';
          OldPos := Stream.Position;
          Data.StreamToBlobField(Stream,DataSet,'DOCUMENT');
          if (aText = '') then
            begin
              Stream.Position:=OldPos;
              if SetText then
                GetContentText(Stream,'.'+Extension,aText);
            end;
          if aText <> '' then
            begin
              ss := TStringStream.Create(aText);
              Data.StreamToBlobField(ss,DataSet,'FULLTEXT');
              ss.Free;
            end;
          if AddDate = 0 then
            FieldByName('DATE').AsFloat := Now()
          else
            FieldByName('DATE').AsFloat := AddDate;
          Post;
          Change;
        end;
    end;
end;
procedure TDocument.AddFromFile(aFilename: string; aText: string;
  AddDate: TDateTime);
var
  aStream: TFileStream;
begin
  aStream := TFileStream.Create(UniToSys(aFilename),fmOpenRead);
  try
    if rpos('.',ExtractFileName(aFileName)) > 0 then
      AddFromStream(copy(ExtractFileName(aFileName),0,rpos('.',ExtractFileName(aFileName))-1),copy(ExtractFileExt(aFileName),2,length(aFileName)),aStream,aText,AddDate)
    else
      AddFromStream(ExtractFileName(aFileName),'',aStream,aText,AddDate);
    Change;
  finally
    aStream.Free;
  end;
end;
procedure TDocument.AddFromDir(aFilename: string; aText: string;
  DoDelete: Boolean; AddDate: TDateTime);
var
  DocID: LargeInt;
  FindRec: TSearchRec;
  aDocument: TDocument;
begin
  with BaseApplication as IBaseDbInterface do
    begin
      Append;
      with DataSet do
        begin
          //TODO:Check if this document already is in the list
          DocID := Data.GetUniID(Connection);
          FieldByName('ISDIR').AsString := 'Y';
          FieldByName('REVISION').AsString := '0';
          FieldByName('NUMBER').AsVariant := DocID;
          FieldByName('NAME').AsString := ExtractFileName(aFilename);
          if AddDate = 0 then
            FieldByName('DATE').AsFloat := Now()
          else
            FieldByName('DATE').AsFloat := AddDate;
          Post;
          if SysUtils.FindFirst(UniToSys(AppendPathDelim(aFileName)+'*'), faAnyFile, FindRec) = 0 THEN
            repeat
              if (FindRec.Name <> '.') AND (FindRec.Name <> '..') THEN
                begin
                  aDocument := TDocument.CreateEx(Self,Data,Connection);
                  aDocument.Select(0);
                  aDocument.Open;
                  aDocument.Ref_ID:=FRefID;
                  aDocument.BaseID:=FBaseID;
                  aDocument.BaseTyp:=FBaseTyp;
                  aDocument.BaseLanguage:=FBaseLanguage;
                  aDocument.BaseLanguage:=FBaseVersion;
                  aDocument.ParentID:=DocID;
                  if FindRec.Attr and faDirectory = faDirectory then
                    aDocument.AddFromDir(AppendPathDelim(aFilename)+FindRec.Name,aText,DoDelete,AddDate)
                  else
                    aDocument.AddFromFile(AppendPathDelim(aFilename)+FindRec.Name,aText,AddDate);
                  Change;
                  aDocument.Free;
                end
            until SysUtils.FindNext(FindRec) <> 0;
        end;
    end;
end;
procedure TDocument.AddFromLink(aLink: string);
var
  aDocument: TDocument;
  DocID: Int64;
  Stream: TStringStream;
begin
  if copy(aLink,0,9) <> 'DOCUMENTS' then exit;
  with BaseApplication as IBaseDbInterface do
    begin
      aDocument := TDocument.CreateEx(Self,Data,Connection);
      aDocument.SelectByLink(aLink);
      aDocument.Open;
      if aDocument.Count > 0 then
        begin
          //TODO:Check if this document already is in the list
          DocID := Data.GetUniID(Connection);
          Self.Append;
          Self.FieldByName('ISLINK').AsString := 'Y';
          Self.FieldByName('ISDIR').AsString := aDocument.FieldByName('ISDIR').AsString;
          Self.FieldByName('REVISION').AsString := '0';
          Self.FieldByName('NUMBER').AsVariant := DocID;
          Self.FieldByName('NAME').AsString := aDocument.FieldByName('NAME').AsString;
          Self.FieldByName('EXTENSION').AsString := aDocument.FieldByName('EXTENSION').AsString;
          Stream := TStringStream.Create(aLink);
          Data.StreamToBlobField(Stream,DataSet,'DOCUMENT');
          Stream.Free;
          Self.FieldByName('DATE').AsFloat := aDocument.FieldByName('DATE').AsFloat;
          Self.DataSet.Post;
        end;
      aDocument.Free;
    end;
end;
procedure TDocument.MoveTo(aID: largeInt; aType: string; aTID: string;
  aVersion: Variant; aLanguage: Variant; aParent: LargeInt);
begin
  with DataSet do
    begin
      First;
      while not EOF do
        begin
          DataSet.Edit;
          if DataSet.FieldDefs.IndexOf('REF_ID_ID') > -1 then
            FieldByName('REF_ID_ID').AsVariant  := aID;
          FieldByName('PARENT').AsVariant  := aParent;
          with BaseApplication as IBaseDbInterface do
            FieldByName('CHANGEDBY').AsString := Data.Users.IDCode.AsString;
          FieldByName('TYPE').AsVariant := aType;
          if DataSet.FieldDefs.IndexOf('ID') > -1 then
            begin
              FieldByName('ID').AsString  := aTID;
              FieldByName('VERSION').AsVariant  := aVersion;
              FieldByName('LANGUAGE').AsVariant  := aLanguage;
            end;
          DataSet.Post;
          Next;
        end;
    end;
end;
procedure TDocument.CreateDirectory(aName: string);
var
  DocID: Int64;
begin
  with BaseApplication as IBaseDbInterface do
    begin
      with DataSet do
        begin
          //TODO:Check if this dir already is in the list
          DocID := Data.GetUniID(Connection);
          if not CanEdit then
            Append;
          FieldByName('ISDIR').AsString := 'Y';
          FieldByName('NUMBER').AsVariant := DocID;
          FieldByName('REVISION').AsString := '0';
          FieldByName('NAME').AsString := aName;
          FieldByName('DATE').AsDateTime := Now();
          Post;
        end;
    end;
end;
procedure TDocument.FillDefaults(aDataSet: TDataSet);
begin
  with aDataSet,BaseApplication as IBaseDbInterface do
    begin
      if DataSet.FieldDefs.IndexOf('REF_ID_ID') > -1 then
        FieldByName('REF_ID_ID').AsVariant  := FRefID;
      FieldByName('PARENT').AsVariant  := FParentID;
      if Data.Users.DataSet.Active then
      FieldByName('CHANGEDBY').AsString := Data.Users.IDCode.AsString;
      FieldByName('TYPE').AsVariant := BaseTyp;
      if DataSet.FieldDefs.IndexOf('ID') > -1 then
        begin
          FieldByName('ID').AsString  := FBaseID;
          FieldByName('VERSION').AsVariant  := FBaseVersion;
          FieldByName('LANGUAGE').AsVariant  := FBaseLanguage;
        end;
    end;
end;
procedure TDocumentTemplates.DefineFields(aDataSet: TDataSet);
begin
  with aDataSet as IBaseManageDB do
    begin
      TableName := 'TEMPLATES';
      if Assigned(ManagedFieldDefs) then
        with ManagedFieldDefs do
          begin
            property TYPE: string index 1,True);
            property NAME: string index 160,True);
            property EXTENSION: string index 15,false);
            property DOCUMENT',ftBlob,0,False);
          end;
    end;
end;

function TDocuments.GetFileName: string;
begin
  result := '';
  if Count > 0 then
    begin
      if DataSet.FieldByName('EXTENSION').AsString <> '' then
        Result := DataSet.FieldByName('NAME').AsString+'.'+DataSet.FieldByName('EXTENSION').AsString
      else
        Result := DataSet.FieldByName('NAME').AsString;
    end;
end;
function TDocuments.GetCreationDate: TDateTime;
begin
  if Count = 0 then exit;
  Result := DataSet.FieldByName('DATE').AsDateTime;
end;
function TDocuments.GetFileSize: Int64;
var
  aDocument: TDocument;
  ss: TStringStream;
begin
  if Count = 0 then exit;
  if DataSet.FieldByName('ISLINK').AsString = 'Y' then
    Result := -1
  else
    Result := DataSet.FieldByName('SIZE').AsInteger;
end;
function TDocuments.GetIsDir: Boolean;
begin
  if not DataSet.Active then exit;
  Result := DataSet.FieldByName('ISDIR').AsString = 'Y';
end;
function TDocuments.GetIsLink: Boolean;
begin
  if not DataSet.Active then exit;
  Result := DataSet.FieldByName('ISLINK').AsString = 'Y';
end;
function TDocuments.GetLastModified: TDateTime;
begin
  if Count = 0 then exit;
  Result := DataSet.FieldByName('TIMESTAMPD').AsDateTime;
end;
procedure TDocuments.SetParentID(AValue: Variant);
begin
  if FParentID=AValue then Exit;
  FParentID:=AValue;
end;

function TDocuments.GetTyp: string;
begin
  Result := 'D';
end;

procedure TDocuments.Open;
begin
  inherited Open;
  if Count > 0 then
    begin
      if DataSet.FieldDefs.IndexOf('REF_ID_ID') <> -1 then
        begin
          if DataSet.FieldByName('REF_ID_ID').IsNull then
            FrefID := 0
          else
            FRefID := DataSet.FieldByName('REF_ID_ID').AsVariant;
        end;
      FBaseTyp := DataSet.FieldByName('TYPE').AsVariant;
      if (DataSet.FieldDefs.IndexOf('ID') <> -1) then
        begin
          FBaseID := DataSet.FieldByName('ID').AsString;
          FBaseVersion := DataSet.FieldByName('VERSION').AsVariant;
          FBaseLanguage := DataSet.FieldByName('LANGUAGE').AsVariant;
        end;
    end;
  if ParentID = Null then
    ParentID := 0;
end;

function TDocuments.GetNumberFieldName: string;
begin
  Result := 'NUMBER';
end;
procedure TDocuments.SelectByNumber(aNumber: Variant);
var
  tmpfields: string = '';
begin
  with BaseApplication as IBaseDbInterface do
    begin
      with Self.DataSet as IBaseDBFilter do
        begin
          tmpfields := GetUsedFields;
          SortDirection := sdAscending;
          SortFields := 'TIMESTAMPD,SQL_ID';
          Fields := tmpFields;
          if aNumber <> Null then
            Filter := Data.QuoteField('NUMBER')+'='+TBaseDBModule(DataModule).QuoteValue(Format('%d',[Int64(aNumber)]))
          else
            Filter := Data.QuoteField('NUMBER')+'='+Data.QuoteValue('0');
          Limit := 0;
        end;
    end;
end;
procedure TDocuments.SelectByID(aID: LargeInt);
var
  tmpfields: String;
begin
  with BaseApplication as IBaseDbInterface do
    begin
      with Self.DataSet as IBaseDBFilter do
        begin
          tmpfields := GetUsedFields;
          SortDirection := sdAscending;
          SortFields := 'TIMESTAMPD,SQL_ID';
          Fields := tmpFields;
          Limit := 0;
        end;
    end;
  inherited Select(aID);
end;

procedure TDocuments.SelectByLink(aLink: string);
var
  aTmp: String;
  aType: String;
  aVersion: variant;
  aNumber: String;
  aParent : Int64 = 0;
  tmpfields: String;
  aTID: String;
begin
  Self.Select(0);
  if copy(aLink,0,pos('@',aLink)) = 'DOCUMENTS.ID@' then
    begin
      aNumber := copy(aLink,pos('@',aLink)+1,length(aLink));
      if rpos('{',aNumber) > 0 then
        aNumber := copy(aNumber,0,rpos('{',aNumber)-1)
      else if rpos('(',aNumber) > 0 then aNumber := copy(aNumber,0,rpos('(',aNumber)-1);
      Self.SelectByNumber(StrToInt64(aNumber))
    end
  else if copy(aLink,0,pos('@',aLink)) = 'DOCUMENTS@' then
    begin
      aTmp := copy(aLink,pos('@',aLink)+1,length(aLink));
      aType := copy(aTmp,0,pos('&&',aTmp)-1);
      aTmp := copy(aTmp,pos('&&',aTmp)+2,length(aTmp));
      aTID := copy(aTmp,0,pos('&&',aTmp)-1);
      aTmp := copy(aTmp,pos('&&',aTmp)+2,length(aTmp));
      aVersion := copy(aTmp,0,pos('&&',aTmp)-1);
      if aVersion = '' then aversion := Null;
      aTmp := copy(aTmp,pos('&&',aTmp)+2,length(aTmp));
      aNumber := aTmp;
      if rpos('{',aNumber) > 0 then
        aNumber := copy(aNumber,0,rpos('{',aNumber)-1)
      else if rpos('(',aNumber) > 0 then aNumber := copy(aNumber,0,rpos('(',aNumber)-1);
      with BaseApplication as IBaseDbInterface do
        begin
          with DataSet as IBaseDBFilter do
            begin
              tmpfields := GetUsedFields;
              Fields := tmpfields;
              if pos(Data.QuoteField('ID'),tmpfields) > 0 then
                begin
                  Filter := '('
                           +Data.ProcessTerm(Data.QuoteField('TYPE')+'='+Data.QuoteValue(aType))+' and '
                           +Data.ProcessTerm(Data.QuoteField('ID')+'='+Data.QuoteValue(aTID))+' and '
//                           +{ProcessTerm(}Data.QuoteField('VERSION')+'='+VarToStr(aVersion){)}+' and '
                           +Data.ProcessTerm(Data.QuoteField('NUMBER')+'='+Data.QuoteValue(aNumber))+')';
                end
              else
                begin
                  Self.SelectByNumber(StrToInt(aNumber))
//                  raise Exception.Create(strInvalidLink);
                end;
              SortDirection := sdAscending;
              SortFields := 'TIMESTAMPD';
              Limit := 0;
            end;
        end;
{
      FRefID := 0;
      FBaseTyp := aType;
      FBaseID := aTID;
      FBaseVersion := aVersion;
      FBaseLanguage := Null;
}
    end;
end;
procedure TDocuments.Select(aID: LargeInt;aType : string;aParent : LargeInt);
begin
  with BaseApplication as IBaseDbInterface do
    begin
      with DataSet as IBaseDBFilter,DataSet as IBaseManageDB do
        begin
          Fields := FUsedFields;
          Filter := Data.QuoteField(TableName)+'.'+Data.QuoteField('REF_ID_ID')+'='+Data.QuoteValue(IntToStr(aID))+' and '+Data.QuoteField('PARENT')+'='+Data.QuoteValue(IntToStr(aParent))+' and '+Data.QuoteField('TYPE')+'='+Data.QuoteValue(aType)+' and '+Data.QuoteField('REVISION')+'='+Data.QuoteValue('0');
          Limit := 0;
        end;
    end;
  FRefID := aID;
  FBaseID := '';
  FBaseTyp := aType;
  FBaseVersion := Null;
  FBaseLanguage := Null;
  ParentID := aParent;
end;
function TDocuments.Delete: Boolean;
var
  aDocument: TDocument;
  {%H-}tmp: String;
begin
  Result := False;
  aDocument := TDocument.CreateEx(Self,DataModule,Connection);
  aDocument.SelectByNumber(DataSet.FieldByName('NUMBER').AsVariant);
  aDocument.Open;
  if aDocument.Count>0 then
    begin
      aDocument.Delete;
      Result := True;
    end;
  aDocument.Free;
  DataSet.Refresh;
end;

procedure TDocuments.Select(aID: largeInt; aType: string; aTID: string;
  aVersion: Variant; aLanguage: Variant; aParent: LargeInt);
var
  tmp: String;
begin
  with BaseApplication as IBaseDbInterface do
    begin
      with DataSet as IBaseDBFilter do
        begin
          tmp := GetUsedFields;
          Fields := tmp;
          Limit := 0;
          if pos(Data.QuoteField('ID'),tmp) > 0 then
            begin
              Filter := '((('+Data.QuoteField('REF_ID_ID')+'='+Data.QuoteValue(IntToStr(aID))+') or ('
                       +Data.ProcessTerm(Data.QuoteField('TYPE')+'='+Data.QuoteValue(aType))+' and '
                       +Data.ProcessTerm(Data.QuoteField('ID')+'='+Data.QuoteValue(aTID))+' and '
                       +Data.ProcessTerm(Data.QuoteField('VERSION')+'='+Data.QuoteValue(VarToStr(aVersion)))+' and '
                       +Data.ProcessTerm(Data.QuoteField('LANGUAGE')+'='+Data.QuoteValue(VarToStr(aLanguage)))+')) and '
                       +Data.QuoteField('PARENT')+'='+Data.QuoteValue(IntToStr(aParent))+') and '
                       +Data.QuoteField('REVISION')+'='+Data.QuoteValue('0');
            end
          else
            begin
              Select(aID,aType,aParent);
              exit;
            end;
        end;
    end;
  FRefID := aID;
  FBaseTyp := aType;
  FBaseID := aTID;
  FBaseVersion := aVersion;
  FBaseLanguage := aLanguage;
  ParentID := aParent;
end;

procedure TDocuments.SelectByReference(aID: Variant);
var
  aField: String = '';
begin
  with BaseApplication as IBaseDBInterface do
    with DataSet as IBaseDBFilter do
      begin
        aField := 'REF_ID_ID';
        if (VarIsNumeric(aID) and (aID = 0))
        or (VarIsStr(aID) and (aID = ''))
        or (aID = Null)  then
          begin
            with DataSet as IBaseManageDb do
              Filter := Data.QuoteField(TableName)+'.'+Data.QuoteField(aField)+'='+Data.QuoteValue('0');
          end
        else
          begin
            with DataSet as IBaseManageDb do
              Filter := Data.QuoteField(TableName)+'.'+Data.QuoteField(aField)+'='+Data.QuoteValue(Format('%d',[Int64(aID)]));
          end;
      end;
end;

function TDocuments.OpenPath(aPath: string; aPathDelim: string): Boolean;
  function RecourseDirs(tmpDocs : TDocuments;nPath : string) : Boolean;
  var
    atmp: String;
  begin
    Result := False;
    tmpDocs.DataSet.First;
    while not tmpDocs.DataSet.EOF do
      begin
        if tmpDocs.IsDir and (tmpDocs.FileName = copy(nPath,0,pos(aPathDelim,nPath)-1)) then
          begin
            atmp := copy(nPath,pos(aPathDelim,nPath)+1,length(nPath));
            if pos(aPathDelim,atmp) > 0 then
              begin
                tmpDocs.Select(Self.Ref_ID,BaseTyp,BaseID,BaseVersion,BaseLanguage,tmpDocs.FieldByName('NUMBER').AsVariant);
                tmpDocs.Open;
                Result := RecourseDirs(tmpDocs,atmp);
              end
            else
              begin
                Result := True;
                exit;
              end;
          end;
        if Result then exit;
        tmpDocs.DataSet.Next;
      end;
  end;
var
  tmpDocs: TDocuments;
begin
  tmpDocs := TDocuments.CreateEx(nil,DataModule,Connection);
  tmpDocs.Select(Self.Ref_ID,BaseTyp,BaseID,BaseVersion,BaseLanguage,0);
  tmpDocs.Open;
  while copy(aPath,0,1) = aPathDelim do
    aPath := copy(aPath,2,length(aPath));
  Result := False;
  if (pos(aPathDelim,aPath) > 0) then
    Result := RecourseDirs(tmpDocs,aPath);
  aPath := copy(aPath,rpos(aPathDelim,aPath)+1,length(aPath));
  if Result then
    begin
      Select(tmpDocs.Ref_ID,tmpDocs.BaseTyp,tmpDocs.BaseID,tmpDocs.BaseVersion,tmpDocs.BaseLanguage,tmpDocs.FieldByName('NUMBER').AsVariant);
      Open;
      Result := True;
    end
  else
    begin
      Result := DataSet.Active and (DataSet.Locate('NAME',aPath,[]) or (aPath=''));
    end;
  tmpDocs.Free;
end;

function TDocuments.GotoLink: Boolean;
var
  aRes: Boolean;
  ss: TStringStream;
begin
  aRes := IsLink;
  Result := aRes;
  while aRes do
    begin
      ss := TStringStream.Create('');
      with BaseApplication as IBaseDbInterface do
        Data.BlobFieldToStream(DataSet,'DOCUMENT',ss);
      SelectByLink(ss.DataString);
      Open;
      ss.Free;
      aRes := IsLink;
    end;
end;

function TDocuments.SelectFile(aFilename: string): Boolean;
var
  aNamePart: String = '';
  aExt: String = '';
begin
  if pos('.',aFilename) > 0 then
    begin
      aNamePart := copy(aFileName,0,rpos('.',aFilename)-1);
      aExt := copy(aFileName,rpos('.',aFilename)+1,length(aFilename));
      Result := DataSet.Locate('NAME;EXTENSION',VarArrayOf([aNamePart,aExt]),[]);
    end
  else
    begin
      aNamePart := aFileName;
      Result := DataSet.Locate('NAME',VarArrayOf([aNamePart]),[]);
    end;
end;
function TDocument.GetCheckoutPath(Directory: string; TempID: string): string;
var
  TempPath: String = '';
begin
  TempPath := GetIDCheckoutPath(Directory,TempID);
  if (DataSet.FieldByName('ISDIR').AsString <> 'Y') and (DataSet.FieldByName('EXTENSION').AsString <> '') then
    Result := AppendPathDelim(TempPath)+DataSet.FieldByName('NAME').AsString+'.'+DataSet.FieldByName('EXTENSION').AsString
  else
    Result := AppendPathDelim(TempPath)+DataSet.FieldByName('NAME').AsString;
end;
function TDocument.GetIDCheckoutPath(Directory: string; TempID: string
  ): string;
var
  TempPath: String;
begin
  with BaseApplication as IBaseApplication do
    TempPath:=GetInternalTempDir;
  TempPath := AppendPathDelim(TempPath)+TempID+DirectorySeparator;
  if Directory <> '' then
    TempPath := TempPath+Directory+DirectorySeparator;
  Result := TempPath;
end;
function TDocument.DoCheckout(Directory: string; aRevision: Integer;
  aNewFileName: string): Boolean;
var
  aDocuments: TDocuments;
  aDocument: TDocument;
  aName: String;
  ss: TStringStream;
  aFile: TFileStream;
begin
  Result := False;
  if IsDir then
    begin
      aDocuments := TDocuments.CreateEx(Self,DataModule,Connection);
      if  (DataSet.FieldByName('ISLINK').AsString <> 'Y') then
        begin
          aDocuments.Select(FRefID,FBaseTyp,FBaseID,FBaseVersion,FBaseLanguage,DataSet.FieldByName('NUMBER').AsVariant);
          aDocuments.Open;
          with aDocuments.DataSet do
            begin
              Result := True;
              while not EOF do
                begin
                  aDocument := TDocument.CreateEx(Self,DataModule,Connection);
                  aDocument.OnCheckCheckOutFile:=Self.OnCheckCheckOutFile;
                  aDocument.SelectByNumber(FieldByName('NUMBER').AsVariant);
                  aDocument.Open;
                  Result := Result and aDocument.DoCheckOut(AppendPathDelim(Directory)+DataSet.FieldByName('NAME').AsString,aRevision,aNewFileName);
                  aDocument.Free;
                  Next;
                end;
            end;
        end
      else
        begin
          ss := TStringStream.Create('');
          with BaseApplication as IBaseDbInterface do
            Data.BlobFieldToStream(DataSet,'DOCUMENT',ss);
          aDocuments.SelectByLink(ss.DataString);
          ss.Free;
          aDocuments.Open;
          with aDocuments.DataSet do
            begin
              Result := True;
              while not EOF do
                begin
                  aDocument := TDocument.CreateEx(Self,DataModule,Connection);
                  aDocument.OnCheckCheckOutFile:=Self.OnCheckCheckOutFile;
                  aDocument.SelectByNumber(FieldByName('NUMBER').AsVariant);
                  aDocument.Open;
                  Result := Result and aDocument.DoCheckOut(Directory,aRevision,aNewFileName);
                  aDocument.Free;
                  Next;
                end;
            end;
        end;
      aDocuments.Free;
    end
  else
    begin
      DataSet.First;
      if aNewFileName<>'' then
        aName := AppendPathDelim(Directory)+aNewFileName
      else if DataSet.FieldByName('EXTENSION').AsString <> '' then
        aName := AppendPathDelim(Directory)+DataSet.FieldByName('NAME').AsString+'.'+DataSet.FieldByName('EXTENSION').AsString
      else if DataSet.FieldByName('NAME').AsString <> '' then
        aName := AppendPathDelim(Directory)+DataSet.FieldByName('NAME').AsString;
      if DataSet.FieldByName('ISLINK').AsString = 'Y' then
        begin
          DataSet.Last;
          ss := TStringStream.Create('');
          with BaseApplication as IBaseDbInterface do
            Data.BlobFieldToStream(DataSet,'DOCUMENT',ss);
          aDocument := TDocument.CreateEx(Self,DataModule,Connection);
          aDocument.SelectByLink(ss.DataString);
          ss.Free;
          aDocument.Open;
          try
            if aDocument.Count > 0 then
              Result := aDocument.DoCheckOut(AppendPathDelim(Directory),aRevision)
            else raise Exception.Create(strLinkNotFound);
          finally
            aDocument.Free;
          end;
        end
      else
        begin
          DataSet.Last;
          if not FileExists(UniToSys(aName)) or (DataSet.FieldByName('CHECKSUM').AsString <> MD5Print(MD5File(aName))) then
            begin
              if Assigned(FOnCheckCheckOutFile) then
                FOnCheckCheckOutFile(aName);
              if (Directory <> '') then
                ForceDirectories(UniToSys(AppendPathDelim(Directory)));
              aFile := TFileStream.Create(UniToSys(aName),fmCreate);
              Result := CheckoutToStream(aFile,aRevision);
              aFile.Free;
              //generate original Checksum if not there
              if DataSet.FieldByName('CHECKSUM').AsString = '' then
                begin
                  with BaseApplication as IBaseDbInterface,BaseApplication as IBaseApplication do
                    begin
                      Edit;
                      FieldByName('CHECKSUM').AsString := MD5Print(MD5File(aName));
                      Post;
                    end;
                end;
            end;
        end;
    end;
end;
function TDocument.CheckoutToStream(aStream: TStream; aRevision: Integer;
  aSize: Integer): Boolean;
var
  aDocument: TDocument;
  ss: TStringStream;
  aFS: TFileStream;
  aRev: Integer;
begin
  Result := False;
  if not IsDir then
    begin
      if DataSet.FieldByName('ISLINK').AsString = 'Y' then
        begin
          DataSet.Last;
          ss := TStringStream.Create('');
          with BaseApplication as IBaseDbInterface do
            Data.BlobFieldToStream(DataSet,'DOCUMENT',ss);
          aDocument := TDocument.CreateEx(Self,DataModule,Connection);
          aDocument.SelectByLink(ss.DataString);
          ss.Free;
          aDocument.Open;
          try
            if aDocument.Count > 0 then
              Result := aDocument.CheckoutToStream(aStream,aRevision,aSize)
          finally
            aDocument.Free;
          end;
        end
      else
        begin
          DataSet.Last;
          if aRevision > -1 then
            begin
              Result := True;
              //go to the revision
              while (DataSet.FieldByName('REVISION').AsInteger>aRevision) and (not DataSet.BOF) do
                DataSet.Prior;
              //go back to prior full save
              while (DataSet.FieldByName('FULL').AsString <> 'Y') and (not DataSet.BOF) do
                DataSet.Prior;
              //patch revision by revision till the target rev
              with BaseApplication as IBaseDbInterface,BaseApplication as IBaseApplication do
                Result := Result and Data.BlobFieldToFile(DataSet,'DOCUMENT',GetInternalTempDir+'prometheusfile.tmp',aSize);//full file
              aRev := DataSet.FieldByName('REVISION').AsInteger;
              while (aRev<aRevision) and (not DataSet.EOF)  do
                begin
                  DataSet.Next;
                  aRev := DataSet.FieldByName('REVISION').AsInteger;
                  //diff it
                  with BaseApplication as IBaseDbInterface,BaseApplication as IBaseApplication do
                    begin
                      Data.BlobFieldToFile(DataSet,'DOCUMENT',GetInternalTempDir+'prometheusfile1.tmp');//full file
                      {$IFDEF WINDOWS}
                      ExecProcessEx('"'+AppendPathDelim(AppendPathDelim(ExtractFilePath(Paramstr(0)))+'tools')+'bspatch'+ExtractFileExt(ParamStr(0))+'" "'+GetInternalTempDir+'prometheusfile.tmp" "prometheusfile.tmp" "'+GetInternalTempDir+'prometheusfile1.tmp"');
                      {$ELSE}
                      ExecProcess('"'+'bspatch'+ExtractFileExt(ParamStr(0))+'" "'+GetInternalTempDir+'prometheusfile.tmp" "'+GetInternalTempDir+'prometheusfile.tmp" "'+GetInternalTempDir+'prometheusfile1.tmp"','',True);
                      {$ENDIF}
                    end;
                end;
              with BaseApplication as IBaseDbInterface,BaseApplication as IBaseApplication do
                aFS := TFileStream.Create(GetInternalTempDir+'prometheusfile.tmp',fmOpenRead);
              aStream.CopyFrom(aFS,0);
              aFS.Free;
              with BaseApplication as IBaseDbInterface,BaseApplication as IBaseApplication do
                begin
                  DeleteFile(GetInternalTempDir+'prometheusfile.tmp');
                  DeleteFile(GetInternalTempDir+'prometheusfile1.tmp');
                end;
            end
          else
            with BaseApplication as IBaseDbInterface do
              Result := Data.BlobFieldToStream(DataSet,'DOCUMENT',aStream,aSize);
        end;
    end;
end;

procedure TDocument.CheckInFromStream(aStream: TStream;Desc : string = '');
//This routine cant do diffs
var
  OldRec: LargeInt;
  aNumber,aVersion,aLanguage,aRev : Variant;
  aTyp: String;
  aID: String;
  aParent: Integer;
  aEName: String;
  aExtension: String;
  UseFullFile: Boolean;
  aRevision: Int64;
begin
  try
    aRevision := StrToInt64(TBaseDBModule(Self.DataModule).Numbers.GetNewNumber('DOCUMENTS'));
  except
    raise;
    exit;
  end;
  //Store all needed values
  with DataSet do
    begin
      aNumber := FieldByName('NUMBER').AsVariant;
      aTyp := FieldByName('TYPE').AsString;
      if FieldDefs.IndexOf('ID') > -1 then
        begin
          aID := FieldByName('ID').AsString;
          aLanguage := FieldByName('LANGUAGE').AsVariant;
          aVersion := FieldByName('VERSION').Asvariant;
        end;
      aParent := FieldByName('PARENT').AsInteger;
      aEName := FieldByName('NAME').AsString;
      aExtension := FieldByName('EXTENSION').AsString;
      UseFullFile := True;
      //delete this revision when its not revision 0
      Last;
      if UseFullFile and (RecordCount > 1) then
        begin
          aRev := FieldByName('REVISION').AsVariant;
          Prior;
          if aRev = FieldByName('REVISION').AsVariant then
            begin
              Delete;
              Append;
            end
          else
            Last;
        end;
      if State = dsBrowse then
        begin
          if RecordCount <= 1 then
            Append
          else Edit;
        end;
      FieldByName('TYPE').AsString := aTyp;
      if FieldDefs.IndexOf('ID') > -1 then
        begin
          FieldByName('ID').AsString := aID;
          FieldByName('LANGUAGE').AsVariant := aLanguage;
          FieldByName('VERSION').AsVariant := aVersion;
        end;
      FieldByName('ISDIR').AsString := 'N';
      FieldByName('PARENT').AsInteger := aParent;
      FieldByName('REVISION').AsVariant := aRevision;
      FieldByName('MESSAGE').AsString := Desc;
      FieldByName('NUMBER').AsVariant := aNumber;
      FieldByName('CHECKSUM').Clear;
      if not UseFullFile then
        FieldByName('FULL').AsString := 'N'
      else
        FieldByName('FULL').AsString := 'Y';
      FieldByName('NAME').AsString := aEName;
      FieldByName('EXTENSION').AsString := aExtension;
      FieldByName('DATE').AsFloat := Now();
      FieldByName('TIMESTAMPD').AsdateTime := Now();
      if FieldDefs.IndexOf('TIMESTAMPT') <> -1 then
        FieldByName('TIMESTAMPT').AsFloat := Frac(Now());
      //add the complete file
      if State <> dsInsert then
        begin
          Post;
          Append;
        end;
      FieldByName('TYPE').AsString := aTyp;
      if FieldDefs.IndexOf('ID') > -1 then
        begin
          FieldByName('ID').AsString := aID;
          FieldByName('LANGUAGE').AsVariant := aLanguage;
          FieldByName('VERSION').AsVariant := aVersion;
        end;
      FieldByName('ISDIR').AsString := 'N';
      FieldByName('PARENT').AsInteger := aParent;
      FieldByName('REVISION').AsVariant := aRevision;
      FieldByName('NUMBER').AsVariant := aNumber;
      FieldByName('NAME').AsString := aEName;
      FieldByName('FULL').AsString:='Y';
      FieldByName('EXTENSION').AsString := aExtension;
      with BaseApplication as IBaseDbInterface do
        Data.StreamToBlobField(aStream,DataSet,'DOCUMENT');
      FieldByName('SIZE').AsInteger:=aStream.Size;
      FieldByName('DATE').AsFloat := Now();
      with BaseApplication as IBaseDbInterface do
        FieldByName('CHANGEDBY').AsString := Data.Users.IDCode.AsString;
      FieldByName('TIMESTAMPD').AsDateTime := Now();
      if FieldDefs.IndexOf('TIMESTAMPT') <> -1 then
        FieldByName('TIMESTAMPT').AsFloat := Frac(Now());
      Post;
    end;
end;

function TDocument.CollectCheckInFiles(Directory: string): TStrings;
  procedure FindFiles(aDir : string);
  var
    Info: TSearchRec;
  begin
    If FindFirst (AppendPathDelim(aDir)+'*',faAnyFile and faDirectory,Info)=0 then
      Repeat
        if copy(Info.Name,0,1) <> '.' then
          With Info do
            begin
              If (Attr and faDirectory) = faDirectory then
                FindFiles(AppendPathDelim(aDir)+Name)
              else
                Result.Values[AppendPathDelim(aDir)+Name] := 'N';
            end;
      Until FindNext(info)<>0;
    FindClose(Info);
  end;
  procedure CheckFiles(aDoc : TDocument;aDir : string);
  var
    aDocuments: TDocuments;
    aDocument: TDocument;
    ss: TStringStream;
  begin
    if aDoc.IsDir and (not aDoc.IsLink) then
      begin
        aDocuments := TDocuments.CreateEx(Self,DataModule,Connection);
        aDocuments.Select(aDoc.Ref_ID,aDoc.BaseTyp,aDoc.BaseID,aDoc.BaseVersion,aDoc.BaseLanguage,aDoc.FieldByName('NUMBER').AsVariant);
        aDocuments.Open;
        with aDocuments.DataSet do
          begin
            while not EOF do
              begin
                aDocument := TDocument.CreateEx(Self,DataModule,Connection);
                aDocument.SelectByNumber(FieldByName('NUMBER').AsVariant);
                aDocument.Open;
                if aDocument.FieldByName('EXTENSION').AsString <> '' then
                  CheckFiles(aDocument,AppendPathDelim(aDir)+aDocument.FieldByName('NAME').AsString+'.'+aDocument.FieldByName('EXTENSION').AsString)
                else
                  CheckFiles(aDocument,AppendPathDelim(aDir)+aDocument.FieldByName('NAME').AsString);
                aDocument.Free;
                Next;
              end;
          end;
        aDocuments.Free;
      end
    else
      begin
        if aDoc.IsLink then
          begin
            aDoc.DataSet.Last;
            ss := TStringStream.Create('');
            with BaseApplication as IBaseDbInterface do
              Data.BlobFieldToStream(aDoc.DataSet,'DOCUMENT',ss);
            aDocument := TDocument.CreateEx(Self,DataModule,Connection);
            aDocument.SelectByLink(ss.DataString);
            ss.Free;
            aDocument.Open;
            try
              if aDocument.Count > 0 then
                CheckFiles(aDocument,aDir);
            finally
              aDocument.Free;
            end;
          end
        else
          begin
            aDoc.DataSet.Last;
            //generate original Checksum if not there
            if aDoc.FieldByName('CHECKSUM').AsString = '' then
              begin
                with BaseApplication as IBaseDbInterface,BaseApplication as IBaseApplication do
                  begin
                    Data.BlobFieldToFile(aDoc.DataSet,'DOCUMENT',GetInternalTempDir+'prometheusfile.tmp');
                    aDoc.DataSet.Edit;
                    aDoc.FieldByName('CHECKSUM').AsString := MD5Print(MD5File(GetInternalTempDir+'prometheusfile.tmp'));
                    aDoc.DataSet.Post;
                    DeleteFile(UniToSys(GetInternalTempDir+'prometheusfile.tmp'));
                  end;
              end;
            //if File isnt there then we have to do nothing
            if FileExists(UniToSys(aDir)) then
              begin
                try
                  if MD5Print(MD5File(UniToSys(aDir))) <> aDoc.FieldByName('CHECKSUM').AsString then
                    Result.Values[aDir] := 'C'
                  else if Result.IndexOfName(aDir) > -1 then
                    Result.Delete(Result.IndexOfName(aDir));
                except
                  if Result.IndexOfName(aDir) > -1 then
                    Result.Delete(Result.IndexOfName(aDir));
                end;
              end;
          end;
      end;
  end;
begin
  Result := TStringList.Create;
  DataSet.First;
  if DataSet.FieldByName('EXTENSION').AsString <> '' then
    CheckFiles(Self,AppendPathDelim(Directory)+DataSet.FieldByName('NAME').AsString+'.'+DataSet.FieldByName('EXTENSION').AsString)
  else
    begin
      FindFiles(AppendPathDelim(Directory)+DataSet.FieldByName('NAME').AsString);
      CheckFiles(Self,AppendPathDelim(Directory)+DataSet.FieldByName('NAME').AsString);
    end;
end;
function TDocument.CheckCheckInFiles(aFiles: TStrings;Directory: string): Boolean;
begin
  Result := false;
  FExtDesc := '';
  if Assigned(OnCheckCheckinFiles) then
    Result := OnCheckCheckinFiles(aFiles,Directory,FExtDesc);
end;
function TDocument.CheckinFiles(aFiles: TStrings;Directory: string;Desc : string = ''): Boolean;
var
  aChanged: Boolean = False;
  function CheckFiles(aDoc : TDocument;aDir : string;aRevision : LargeInt) : Boolean;
  var
    aDocuments: TDocuments;
    aDocument: TDocument;
    OldRec: LongInt;
    aNumber: LargeInt;
    aTyp: String;
    aID: String;
    aLanguage,aVersion : variant;
    aParent: Integer;
    aEName: String;
    aExtension: String;
    ss: TStringStream;
    UseFullFile: Boolean = False;
    aRev : Variant;
  begin
    Result := True;
    if aDoc.IsLink then
      begin
        aDoc.DataSet.Last;
        ss := TStringStream.Create('');
        with BaseApplication as IBaseDbInterface do
          Data.BlobFieldToStream(aDoc.DataSet,'DOCUMENT',ss);
        aDocument := TDocument.CreateEx(Self,DataModule,Connection);
        aDocument.SelectByLink(ss.DataString);
        ss.Free;
        aDocument.Open;
        try
          if aDocument.Count > 0 then
            begin
              Result := Result and CheckFiles(aDocument,aDir,aRevision)
            end
          else
            begin
              Result := False;
              raise Exception.Create(strLinkNotFound);
            end;
        finally
          aDocument.Free;
        end;
      end
    else if aDoc.IsDir then
      begin
        aDocuments := TDocuments.CreateEx(Self,DataModule,Connection);
        aDocuments.Select(aDoc.Ref_ID,aDoc.BaseTyp,aDoc.BaseID,aDoc.BaseVersion,aDoc.BaseLanguage,aDoc.FieldByName('NUMBER').AsVariant);
        aDocuments.Open;
        with aDocuments.DataSet do
          begin
            while not EOF do
              begin
                aDocument := TDocument.CreateEx(Self,DataModule,Connection);
                aDocument.SelectByNumber(FieldByName('NUMBER').AsVariant);
                aDocument.Open;
                if aDocument.FieldByName('EXTENSION').AsString <> '' then
                  Result := Result and CheckFiles(aDocument,AppendPathDelim(aDir)+aDocument.FieldByName('NAME').AsString+'.'+aDocument.FieldByName('EXTENSION').AsString,aRevision)
                else
                  Result := Result and CheckFiles(aDocument,AppendPathDelim(aDir)+aDocument.FieldByName('NAME').AsString,arevision);
                aDocument.Free;
                Next;
              end;
          end;
        aDocuments.Free;
      end
    else
      begin
        //if File isnt there then we have to do nothing
        if FileExists(UniToSys(aDir)) then
          begin
            if aFiles.IndexOfName(aDir) <> -1 then
              begin
                //Checkin File
                OldRec := aDoc.GetBookmark;
                aDoc.DataSet.Last;
                //Save old File
                with BaseApplication as IBaseDbInterface,BaseApplication as IBaseApplication do
                  Data.BlobFieldToFile(aDoc.DataSet,'DOCUMENT',GetInternalTempDir+'prometheusfile.tmp');
                aDoc.GotoBookmark(OldRec);
                //Store all needed values
                with aDoc.DataSet do
                  begin
                    aNumber := FieldByName('NUMBER').AsVariant;
                    aTyp := FieldByName('TYPE').AsString;
                    if FieldDefs.IndexOf('ID') > -1 then
                      begin
                        aID := FieldByName('ID').AsString;
                        aLanguage := FieldByName('LANGUAGE').AsVariant;
                        aVersion := FieldByName('VERSION').Asvariant;
                      end;
                    aParent := FieldByName('PARENT').AsInteger;
                    aEName := FieldByName('NAME').AsString;
                    aExtension := FieldByName('EXTENSION').AsString;
                    //make an diff and add it
                    First;
                    if not CanEdit then
                      Edit;
                    FieldByName('SIZE').AsInteger:=FileSize(aDir);
                    Post;
                    //TODO: use a better logic to not always use full files but for now its the save way
                    UseFullFile := True;
                    if not UseFullFile then
                      begin
                        with BaseApplication as IBaseDbInterface,BaseApplication as IBaseApplication do
                          begin
                            //diff it
                            {$IFDEF WINDOWS}
                            ExecProcessEx('"'+AppendPathDelim(AppendPathDelim(ExtractFilePath(Paramstr(0)))+'tools')+'bsdiff'+ExtractFileExt(ParamStr(0))+'" "'+GetInternalTempDir+'prometheusfile.tmp" "'+aDir+'" "'+GetInternalTempDir+'prometheusfile1.tmp"');
                            {$ELSE}
                            ExecProcess('"'+'bsdiff'+ExtractFileExt(ParamStr(0))+'" "'+GetInternalTempDir+'prometheusfile.tmp" "'+aDir+'" "'+GetInternalTempDir+'prometheusfile1.tmp"','',True);
                            {$ENDIF}
                          end;
                      end;
                    with BaseApplication as IBaseDbInterface,BaseApplication as IBaseApplication do
                      begin
                        if (not FileExists(UniToSys(GetInternalTempDir+'prometheusfile1.tmp'))) or UseFullfile then
                          begin
                            UseFullfile := True;
                            DeleteFile(UniToSys(GetInternalTempDir+'prometheusfile1.tmp'));
                            //Use Full File if no diff is possible
                            if not CopyFile(aDir,UniToSys(GetInternalTempDir+'prometheusfile1.tmp')) then
                              begin
                                Result := False;
                                raise Exception.Create(Format(strFailedCreatingDiff,[aDir]));
                                exit;
                              end;
                          end;
                      end;
                    //delete this revision when its not revision 0
                    Last;
                    if UseFullFile and (RecordCount > 1) then
                      begin
                        aRev := FieldByName('REVISION').AsVariant;
                        Prior;
                        if aRev = FieldByName('REVISION').AsVariant then
                          begin
                            Delete;
                            Append;
                          end
                        else
                          Last;
                      end;
                    if State = dsBrowse then
                      begin
                        if RecordCount <= 1 then
                          Append
                        else Edit;
                      end;
                    FieldByName('TYPE').AsString := aTyp;
                    if FieldDefs.IndexOf('ID') > -1 then
                      begin
                        FieldByName('ID').AsString := aID;
                        FieldByName('LANGUAGE').AsVariant := aLanguage;
                        FieldByName('VERSION').AsVariant := aVersion;
                      end;
                    FieldByName('ISDIR').AsString := 'N';
                    FieldByName('PARENT').AsInteger := aParent;
                    FieldByName('REVISION').AsVariant := aRevision;
                    FieldByName('MESSAGE').AsString := Desc;
                    FieldByName('NUMBER').AsVariant := aNumber;
                    FieldByName('CHECKSUM').Clear;
                    if not UseFullFile then
                      FieldByName('FULL').AsString := 'N'
                    else
                      FieldByName('FULL').AsString := 'Y';
                    FieldByName('NAME').AsString := aEName;
                    FieldByName('EXTENSION').AsString := aExtension;
                    with BaseApplication as IBaseDbInterface,BaseApplication as IBaseApplication do
                      Data.FileToBlobField(GetInternalTempDir+'prometheusfile1.tmp',aDoc.DataSet,'DOCUMENT');
                    FieldByName('DATE').AsFloat := Now();
                    FieldByName('TIMESTAMPD').AsdateTime := Now();
                    if FieldDefs.IndexOf('TIMESTAMPT') <> -1 then
                      FieldByName('TIMESTAMPT').AsFloat := Frac(Now());
                    Post;
                    //delete diff
                    with BaseApplication as IBaseDbInterface,BaseApplication as IBaseApplication do
                      begin
                        DeleteFile(UniToSys(GetInternalTempDir+'prometheusfile.tmp'));
                        DeleteFile(UniToSys(GetInternalTempDir+'prometheusfile1.tmp'));
                      end;
                    //add the complete file
                    Append;
                    FieldByName('TYPE').AsString := aTyp;
                    if FieldDefs.IndexOf('ID') > -1 then
                      begin
                        FieldByName('ID').AsString := aID;
                        FieldByName('LANGUAGE').AsVariant := aLanguage;
                        FieldByName('VERSION').AsVariant := aVersion;
                      end;
                    FieldByName('ISDIR').AsString := 'N';
                    FieldByName('PARENT').AsInteger := aParent;
                    FieldByName('REVISION').AsVariant := aRevision;
                    FieldByName('NUMBER').AsVariant := aNumber;
                    FieldByName('NAME').AsString := aEName;
                    FieldByName('FULL').AsString:='Y';
                    FieldByName('EXTENSION').AsString := aExtension;
                    with BaseApplication as IBaseDbInterface do
                      Data.FileToBlobField(aDir,aDoc.DataSet,'DOCUMENT');
                    FieldByName('CHECKSUM').AsString := MD5Print(MD5File(aDir));
                    FieldByName('SIZE').AsInteger:=FileSize(aDir);
                    FieldByName('DATE').AsFloat := Now();
                    with BaseApplication as IBaseDbInterface do
                      FieldByName('CHANGEDBY').AsString := Data.Users.IDCode.AsString;
                    FieldByName('TIMESTAMPD').AsDateTime := Now();
                    if FieldDefs.IndexOf('TIMESTAMPT') <> -1 then
                      FieldByName('TIMESTAMPT').AsFloat := Frac(Now());
                    Post;
                    aChanged := True;
                  end;
                //Delete from List
                aFiles.Delete(aFiles.IndexOfName(aDir));
              end;
          end;
      end;
  end;
var
  aRevision: LargeInt;
  aDocument: TDocument;
  aDataSet: TDataSet;
  aBaseDir: TDocuments;
  i: Integer;
  tmp: String;
  aDocuments: TDocuments;
begin
  if (Desc = '') and (FExtDesc <> '') then Desc := FExtDesc;
  //Get last revision
  Result := False;
  try
    aRevision := StrToInt64(TBaseDBModule(Self.DataModule).Numbers.GetNewNumber('DOCUMENTS'));
  except
    raise;
    exit;
  end;
  DataSet.First;
  if DataSet.FieldByName('EXTENSION').AsString <> '' then
    Result := CheckFiles(Self,AppendPathDelim(Directory)+DataSet.FieldByName('NAME').AsString+'.'+DataSet.FieldByName('EXTENSION').AsString,aRevision+1)
  else
    Result := CheckFiles(Self,AppendPathDelim(Directory)+DataSet.FieldByName('NAME').AsString,aRevision+1);
  //New Files
  i := 0;
  while i < aFiles.Count do
    begin
      if aFiles.ValueFromIndex[i] = 'N' then
        begin
          tmp := aFiles.Names[i];
          if copy(tmp,0,length(Directory)) = Directory then
            tmp := copy(tmp,length(Directory)+1,length(tmp));
          if copy(tmp,0,1) = DirectorySeparator then tmp := copy(tmp,length(DirectorySeparator)+1,length(tmp));
          if copy(tmp,0,pos(DirectorySeparator,tmp)-1) = Self.FieldByName('NAME').AsString then
            tmp := copy(tmp,pos(DirectorySeparator,tmp)+length(DirectorySeparator),length(tmp));
          if copy(tmp,0,1) = DirectorySeparator then tmp := copy(tmp,length(DirectorySeparator)+1,length(tmp));
          aBaseDir := TDocuments.CreateEx(Self,DataModule,Connection);
          aBaseDir.Select(Id.AsVariant);
          aBaseDir.Open;
          while pos(DirectorySeparator,tmp) > 0 do
            begin
              aDocuments := TDocuments.CreateEx(Self,DataModule,Connection);
              aDocuments.Select(Ref_ID,BaseTyp,BaseID,BaseVersion,BaseLanguage,aBaseDir.FieldByName('NUMBER').AsVariant);
              aDocuments.Open;
              if aDocuments.DataSet.Locate('NAME;ISDIR',VarArrayOf([copy(tmp,0,pos(DirectorySeparator,tmp)-1),'Y']),[]) then
                begin //Found Dir
                  aBaseDir.Select(aDocuments.Id.AsVariant);
                  aBaseDir.Open;
                end
              else //New Dir
                begin
                  aDocument := TDocument.CreateEx(Self,DataModule,Connection);
                  aDocument.Select(0);
                  aDocument.Open;
                  aDocument.Ref_ID:=Ref_ID;
                  aDocument.BaseID:=BaseID;
                  aDocument.BaseTyp:=BaseTyp;
                  aDocument.BaseLanguage:=BaseLanguage;
                  aDocument.BaseVersion:=BaseVersion;
                  aDocument.ParentID:=aBaseDir.FieldByName('NUMBER').AsVariant;
                  with aDocument.DataSet do
                    begin
                      Append;
                      FieldByName('ISDIR').AsString := 'Y';
                      FieldByName('NUMBER').AsInteger := TBaseDbModule(DataModule).GetUniID(Connection);
                      FieldByName('REVISION').AsString := '0';
                      FieldByName('NAME').AsString := copy(tmp,0,pos(DirectorySeparator,tmp)-1);
                      FieldByName('DATE').AsDateTime := Now();
                      Post;
                      Change;
                    end;
                  aBaseDir.Select(aDocument.Id.AsVariant);
                  aBaseDir.Open;
                  aDocument.Free;
                end;
              aDocuments.Free;
              tmp := copy(tmp,pos(DirectorySeparator,tmp)+length(DirectorySeparator),length(tmp));
            end;
          //Create new File
          if aBaseDir.Count > 0 then
            begin
              aDocument := TDocument.CreateEx(Self,DataModule,Connection);
              aDocument.Select(0);
              aDocument.Open;
              aDocument.Ref_ID:=Ref_ID;
              aDocument.BaseID:=BaseID;
              aDocument.BaseTyp:=BaseTyp;
              aDocument.BaseLanguage:=BaseLanguage;
              aDocument.BaseVersion:=BaseVersion;
              aDocument.ParentID:=aBaseDir.FieldByName('NUMBER').AsVariant;
              aDocument.AddFromFile(aFiles.Names[i],Desc);
              aDocument.free;
              aBaseDir.Free;
              aFiles.Delete(i);
            end
          else inc(i);
        end
      else inc(i);
    end;
  if aChanged then
    if Assigned(FAfterCheckinFiles) then
      FAfterCheckinFiles(Self);
end;
function TDocument.Delete: Boolean;
var
  aDocuments: TDocuments;
begin
  Result := False;
  if not DataSet.Active then exit;
  //DataSet.Refresh;
  if IsDir then
    begin
      aDocuments := TDocuments.CreateEx(Self,DataModule,Connection);
      aDocuments.Select(Ref_ID,BaseTyp,BaseID,BaseVersion,BaseLanguage,DataSet.FieldByName('NUMBER').AsVariant);
      aDocuments.Open;
      while aDocuments.Count > 0 do
        begin
          aDocuments.Delete;
          Result := True;
        end;
      aDocuments.Free;
    end;
  while (Count > 0) do
    begin
      DataSet.Delete;
      Result := True;
    end;
end;

initialization
  FUsedFields := '';
  RegisterdataSetClass('DOCUMENT',TDocument);
end.

