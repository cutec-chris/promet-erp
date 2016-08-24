unit timportexport;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, uMasterdata;

type

  { TImportExportTest }

  TImportExportTest= class(TTestCase)
  published
    procedure CreateArticle;
    procedure ExportToJSon;
  end;

implementation

var
  aMD : TMasterdata;

procedure TImportExportTest.CreateArticle;
begin
  aMD := TMasterdata.Create(nil);
  aMd.ActualLimit:=1;
  aMD.Open;
end;

procedure TImportExportTest.ExportToJSon;
var
  aJson: String;
  ss: TStringStream;
  fs: TFileStream;
begin
  aJson := aMd.ExportToJSON;
  ss := TStringStream.Create(aJson);
  fs := TFileStream.Create(GetTempDir+'test.json',fmCreate);
  fs.CopyFrom(ss,0);
  fs.Free;
  ss.Free;
end;



initialization

  RegisterTest(TImportExportTest);
end.

