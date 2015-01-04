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
unit uBaseDocPages;

{$mode objfpc}{$H+}

interface

uses
  Classes,SysUtils,uDocuments,uBaseDbClasses,uBaseDBInterface,db,uIntfStrConsts,
  Utils,usimpleprocess;
type

  { TDocPages }

  TDocPages = class(TBaseDBDataset)
  private
    FTyp: string;
    FUsedFields : string;
    procedure SetParamsFromExif(extn : string;aFullStream : TStream);
    procedure SetType(AValue: string);
  public
    function GetUsedFields : string;
    procedure PrepareDataSet;
    procedure Open; override;
    procedure Select(aID: Variant); override;
    procedure DefineFields(aDataSet: TDataSet); override;
    procedure Add(aDocuments: TDocuments);
    procedure AddFromFile(aFile : UTF8String);
    property Typ : string read FTyp write SetType;
  end;

implementation
uses uData,uBaseApplication,dEXIF,Process,
  uthumbnails;

procedure TDocPages.SetParamsFromExif(extn: string; aFullStream: TStream);
var
  exif: TImgData;
  aTime: TDateTime;
begin
  exif := TImgData.Create;
  aFullStream.Position:=0;
  if (extn = '.jpg') or (extn = '.jpeg') or (extn = '.jpe') then
    begin
      exif.ReadJpegSections(tStream(aFullStream));
    end;
  if (extn = '.tif') or (extn = '.tiff') or (extn = '.nef') then
    begin
      exif.ReadTiffSections(tStream(aFullStream));
    end;
  if Assigned(exif.ExifObj) then
    begin
      if exif.ExifObj.dt_orig_oset > 0 then
        aTime := exif.ExifObj.ExtrDateTime(exif.ExifObj.dt_orig_oset)
      else
        aTime := exif.ExifObj.GetImgDateTime;
      if aTime > 0 then
        FieldByName('ORIGDATE').AsDateTime:=aTime;
    end;
  if Assigned(exif.IptcObj) then
    begin
      aTime := exif.IptcObj.GetDateTime;
      if aTime > 0 then
        FieldByName('ORIGDATE').AsDateTime:=aTime;
    end;
  exif.Free;
end;

procedure TDocPages.SetType(AValue: string);
begin
  if FTyp=AValue then Exit;
  FTyp:=AValue;
  ActualFilter := TBaseDBModule(DataModule).QuoteField('TYPE')+'='+TBaseDBModule(DataModule).QuoteValue(FTyp);
end;

function TDocPages.GetUsedFields: string;
var
  tmpFields : string = '';
  i: Integer;
  aOldLimit: Integer;
  OldUseP: Boolean;
begin
  if FUsedFields = '' then
    begin
      with BaseApplication as IBaseDbInterface do
        begin
          with Self.DataSet as IBaseDBFilter,Self.DataSet as IBaseManageDB do
            begin
              Filter := Data.ProcessTerm(Data.QuoteField(TableName)+'.'+Data.QuoteField('SQL_ID')+'='+Data.QuoteValue(''));
              Fields := '';
              aOldLimit := Limit;
              Limit := 1;
              OldUseP := UsePermissions;
              UsePermissions:=False;
              Open;
              for i := 0 to DataSet.FieldDefs.Count-1 do
                if  (DataSet.FieldDefs[i].Name <> 'THUMBNAIL')
                then
                  tmpfields := tmpfields+','+Data.QuoteField(TableName)+'.'+Data.QuoteField(DataSet.FieldDefs[i].Name);
              tmpFields := copy(tmpFields,2,length(tmpFields));
              FUsedFields := tmpFields;
              Limit := aOldLimit;
              UsePermissions:=OldUseP;
              Filter := '';
            end;
        end;
    end;
  Result := FUsedFields;
end;

procedure TDocPages.PrepareDataSet;
begin
  if FUsedFields = '' then
    GetUsedFields;
  with DataSet as IBaseDBFilter do
    begin
      Fields:=FUsedFields;
    end;
end;

procedure TDocPages.Open;
begin
  inherited Open;
end;

procedure TDocPages.Select(aID: Variant);
var
  tmp: String;
begin
  with DataSet as IBaseDBFilter do
    begin
      tmp := GetUsedFields;
      Fields := tmp;
    end;
  inherited Select(aID);
end;

procedure TDocPages.DefineFields(aDataSet: TDataSet);
begin
  with aDataSet as IBaseManageDB do
    begin
      TableName := 'DOCPAGES';
      TableCaption:=strDocuments;
      if Assigned(ManagedFieldDefs) then
        with ManagedFieldDefs do
          begin
            Add('PAGE',ftInteger,0,False);
            Add('TYPE',ftString,1,False);
            Add('TAGS',ftString,500,False);
            Add('NAME',ftString,100,False);
            Add('ORIGDATE',ftDateTime,0,False);
            Add('DONE',ftString,1,False);
            Add('CHANGEDBY',ftString,4,False);
            Add('LINK',ftString,400,False);
            Add('TREEENTRY',ftLargeint,0,False);
            Add('FULLTEXT',ftMemo,0,False);
            Add('THUMBNAIL',ftBlob,0,False);
          end;
    end;
end;

procedure TDocPages.Add(aDocuments: TDocuments);
var
  aDocument: TDocument;
  aStream: TMemoryStream;
  aFullStream: TMemoryStream;
  extn: String;
  aTime: TDateTime;
  bDocument: TDocument;
  aSStream: TStringStream;
begin
  aDocument := TDocument.Create(nil);
  aDocument.SelectByID(aDocuments.Id.AsVariant);
  aDocument.Open;
  if aDocument.Count>0 then
    begin
      Insert;
      FieldByName('NAME').AsString:=aDocuments.FileName;
      if FTyp <> '' then
        FieldByName('TYPE').AsString:=FTyp;
      aStream := TMemoryStream.Create;
      aFullStream := TMemoryStream.Create;
      aDocument.CheckoutToStream(aFullStream);
      extn :=  AnsiString(AnsiLowerCase(ExtractFileExt(aDocuments.filename)));
      aFullStream.Position:=0;
      SetParamsFromExif(extn,aFullStream);
      aSStream := TStringStream.Create('');
      Data.BlobFieldToStream(aDocument.DataSet,'FULLTEXT',aSStream);
      GenerateThumbNail(ExtractFileExt(aDocument.FileName),aFullStream,aStream,aSStream.DataString);
      aSStream.Free;
      if FieldByName('ORIGDATE').IsNull then
        FieldByName('ORIGDATE').AsDateTime:=aDocument.FieldByName('DATE').AsDateTime;
      if FieldByName('ORIGDATE').IsNull then
        FieldByName('ORIGDATE').AsDateTime:=Now();
      Post;
      if aStream.Size>0 then
        Data.StreamToBlobField(aStream,Self.DataSet,'THUMBNAIL');
      bDocument := TDocument.Create(nil);
      bdocument.Ref_ID:=Id.AsVariant;
      bDocument.BaseTyp:='S';
      bDocument.AddFromLink(Data.BuildLink(aDocument.DataSet));
      bDocument.Free;
      aStream.Free;
      aFullStream.Free;
    end;
  aDocument.Free;
end;
procedure TDocPages.AddFromFile(aFile: UTF8String);
var
  aDocument: TDocument;
  aStream: TMemoryStream;
  aFullStream: TMemoryStream;
  extn: String;
  aTime: TDateTime;
  aSecFile: String = '';
  aProc: TProcess;
  aSL: TStringList;
  aText: string;
  ss: TStringStream;
begin
  if FileExists(aFile) then
    begin
      Insert;
      FieldByName('NAME').AsString:=ExtractFileName(aFile);
      if FTyp <> '' then
        FieldByName('TYPE').AsString:=FTyp;
      Post;
      DataSet.Edit;
      aDocument := TDocument.Create(nil);
      adocument.Ref_ID:=Id.AsVariant;
      aDocument.BaseTyp:='S';
      aDocument.AddFromFile(aFile);
      aStream := TMemoryStream.Create;
      aFullStream := TMemoryStream.Create;
      extn :=  AnsiString(AnsiLowerCase(ExtractFileExt(aDocument.filename)));
      if (extn = '.cr2')
      or (extn = '.crw')
      or (extn = '.dng')
      or (extn = '.raw')
      or (extn = '.erf')
      or (extn = '.raf')
      or (extn = '.3fr')
      or (extn = '.fff')
      or (extn = '.dcr')
      or (extn = '.dcs')
      or (extn = '.kdc')
      or (extn = '.rwl')
      or (extn = '.mef')
      or (extn = '.mfw')
      or (extn = '.iiq')
      or (extn = '.mrw')
      or (extn = '.mdc')
      or (extn = '.nef')
      or (extn = '.nrw')
      or (extn = '.orf')
      or (extn = '.rw2')
      or (extn = '.pef')
      or (extn = '.srw')
      or (extn = '.x3f')
      or (extn = '.cs1')
      or (extn = '.cs4')
      or (extn = '.cs16')
      or (extn = '.srf')
      or (extn = '.sr2')
      or (extn = '.arw')
      then
        begin
          if FileExists(copy(aFile,0,rpos('.',aFile)-1)+'.jpg') then
            aSecFile := copy(aFile,0,rpos('.',aFile)-1)+'.jpg'
          else if FileExists(copy(aFile,0,rpos('.',aFile)-1)+'.JPG') then
            aSecFile := copy(aFile,0,rpos('.',aFile)-1)+'.JPG'
          else if FileExists(copy(aFile,0,rpos('.',aFile)-1)+'.Jpg') then
            aSecFile := copy(aFile,0,rpos('.',aFile)-1)+'.Jpg';
          if aSecFile = '' then
            begin
              ExecProcessEx('ufraw-batch --silent --create-id=also --out-type=jpg --exif "--output='+copy(aFile,0,rpos('.',aFile)-1)+'.jpg"'+' "'+aFile+'"');
              if FileExists(copy(aFile,0,rpos('.',aFile)-1)+'.jpg') then
                aSecFile := copy(aFile,0,rpos('.',aFile)-1)+'.jpg'
            end;
          if aSecFile <> '' then
            begin
              aDocument.Free;
              aDocument := TDocument.Create(nil);
              adocument.Ref_ID:=Id.AsVariant;
              aDocument.BaseTyp:='S';
              aDocument.AddFromFile(aSecFile);
              aDocument.CheckoutToStream(aFullStream);
              extn :=  AnsiString(AnsiLowerCase(ExtractFileExt(aDocument.filename)));
            end;
        end;
      if aFullStream.Size=0 then
        aDocument.CheckoutToStream(aFullStream);
      aFullStream.Position:=0;
      SetParamsFromExif(extn,aFullStream);
      if FieldByName('ORIGDATE').IsNull then
        FieldByName('ORIGDATE').AsDateTime:=aDocument.FieldByName('DATE').AsDateTime;
      if FieldByName('ORIGDATE').IsNull then
        FieldByName('ORIGDATE').AsDateTime:=Now();
      aDocument.GetText(aFullStream,extn,aText);
      GenerateThumbNail(ExtractFileExt(aDocument.FileName),aFullStream,aStream,aText);
      Self.Post;
      if aText<>'' then
        begin
          ss := TStringStream.Create(aText);
          Data.StreamToBlobField(ss,Self.DataSet,'FULLTEXT');
          ss.Free;
        end;
      if aStream.Size>0 then
        Data.StreamToBlobField(aStream,Self.DataSet,'THUMBNAIL');
      aStream.Free;
      aFullStream.Free;
      aDocument.Free;
    end;
end;

end.

