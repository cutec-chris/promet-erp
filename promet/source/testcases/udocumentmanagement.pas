unit udocumentmanagement;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, udocuments, utils,
  uBaseApplication;

type

  DocumentManagmement= class(TTestCase)
  published
    procedure CreateFile;
    procedure CreateDocuments;
    procedure CheckIn;
    procedure CollectFilesUnchanged;
    procedure CollectFilesChanged;
    procedure Checkin2;
    procedure Delete;
    //Check checkin into Link Structure
    procedure CreateDir;
    procedure CheckinToDir;
    procedure CreateLink;
    procedure CheckOut;
    procedure CollectFilesUnchanged2;
    procedure CollectFilesChanged2;
    procedure CheckIn3;
    procedure Delete2;
    procedure Free;
  end;

implementation
uses uData;
var
  Temppath : string;
  aDoc,aDoc2 : TDocument;
  aDir : TDocuments;
  aCheckinFiles: TStrings;
procedure DocumentManagmement.CreateFile;
var
  sl: TStringList;
begin
  if Assigned(BaseApplication) then with BaseApplication as IBaseApplication do
    TempPath := AppendPathDelim(GetInternalTempDir)+'ptc';
  ForceDirectories(TempPath);
  sl := TStringList.Create;
  sl.Text:='1';
  sl.SaveToFile(AppendPathDelim(TempPath)+'testfile.txt');
  sl.Free;
end;
procedure DocumentManagmement.CreateDocuments;
begin
  aDoc := TDocument.Create(nil);
  aDoc.CreateTable;
end;
procedure DocumentManagmement.Checkin;
begin
  aDoc.SelectByID(0);
  aDoc.Open;
  aDoc.Ref_ID:=123456789;
  aDoc.BaseTyp:='T';
  aDoc.BaseID:='123456789';
  aDoc.AddFromFile(AppendPathDelim(TempPath)+'testfile.txt');
  Check(aDoc.Count = 1,'Count <> 1');
end;

procedure DocumentManagmement.CollectFilesUnchanged;
begin
  aCheckInFiles := aDoc.CollectCheckInFiles(AppendPathDelim(TempPath));
  Check(aCheckinFiles.Count = 0,'Count <> 0');
  aCheckInFiles.Free;
end;

procedure DocumentManagmement.CollectFilesChanged;
var
  sl: TStringList;
begin
  sl := TStringList.Create;
  sl.Text:='1'+lineending+'2';
  sl.SaveToFile(AppendPathDelim(TempPath)+'testfile.txt');
  sl.Free;
  aCheckInFiles := aDoc.CollectCheckInFiles(AppendPathDelim(TempPath));
  Check(aCheckinFiles.Count = 1,'Count <> 1');
end;
procedure DocumentManagmement.Checkin2;
begin
  aDoc.CheckinFiles(aCheckinFiles,AppendPathDelim(TempPath));
  Check(aDoc.Count = 3,'Count <> 3 '+IntToStr(aDoc.Count));
  aCheckinFiles.Free;
end;

procedure DocumentManagmement.Delete;
begin
  aDoc.Delete;
  Check(aDoc.Count = 0,'Count <> 0');
end;

procedure DocumentManagmement.CreateDir;
begin
  aDoc.CreateDirectory('adir');
  aDir := TDocuments.Create(nil);
  aDir.Select(aDoc.Ref_ID,aDoc.BaseTyp,aDoc.BaseID,aDoc.BaseVersion,aDoc.BaseLanguage,aDoc.FieldByName('NUMBER').AsVariant);
  aDir.Open;
end;

procedure DocumentManagmement.CheckinToDir;
begin
  aDoc2 := TDocument.Create(nil);
  aDoc2.SelectByID(0);
  aDoc2.Open;
  aDoc2.Ref_ID:=123456789;
  aDoc2.BaseTyp:='T';
  aDoc2.BaseID:='123456789';
  aDoc2.ParentID:=aDoc.Number.AsVariant;
  aDoc2.AddFromFile(AppendPathDelim(TempPath)+'testfile.txt');
end;

procedure DocumentManagmement.CreateLink;
var
  aLink: String;
begin
  aLink := Data.BuildLink(aDoc.DataSet);
  aDoc.AddFromLink(aLink);
end;

procedure DocumentManagmement.CheckOut;
begin
  ForceDirectories(AppendPathDelim(TempPath)+'1');
  aDoc.DoCheckout(AppendPathDelim(TempPath)+'1');
end;

procedure DocumentManagmement.CollectFilesUnchanged2;
var
  aCount: Integer;
begin
  aCheckInFiles := aDoc.CollectCheckInFiles(AppendPathDelim(AppendPathDelim(TempPath)+'1'));
  aCount := aCheckinFiles.Count;
  aCheckInFiles.Free;
  Check(aCount = 0,'Count <> 0');
end;

procedure DocumentManagmement.CollectFilesChanged2;
var
  sl: TStringList;
begin
  sl := TStringList.Create;
  sl.Text:='1'+lineending+'2'+lineending+'3';
  ForceDirectories(AppendPathDelim(AppendPathDelim(AppendPathDelim(TempPath)+'1')+'adir'));
  sl.SaveToFile(AppendPathDelim(AppendPathDelim(AppendPathDelim(TempPath)+'1')+'adir')+'testfile.txt');
  sl.Free;
  aCheckInFiles := aDoc.CollectCheckInFiles(AppendPathDelim(AppendPathDelim(TempPath)+'1'));
  Check(aCheckinFiles.Count = 1,'Count <> 1');
end;

procedure DocumentManagmement.CheckIn3;
begin

  aCheckInFiles.Free;
end;

procedure DocumentManagmement.Delete2;
begin
  aDoc.Delete;
  aDoc2.Delete;
  Check(aDoc.Count = 0,'Count <> 0');
  Check(aDoc2.Count = 0,'Count <> 0');
end;

procedure DocumentManagmement.Free;
begin
  aDoc.Free;
  aDoc2.Free;
  aDir.Free;
//  DeleteDirectory(TempPath,False);
end;

initialization

  RegisterTest(DocumentManagmement); 
end.

