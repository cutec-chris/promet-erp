unit twikitest;

{$mode objfpc}{$H+}

interface

uses
Classes, SysUtils, fpcunit, testutils, testregistry, uWiki;

type

  { TWikiTestC }

  TWikiTestC= class(TTestCase)
  published
    procedure CreateWikiList;
    procedure FindAdminStartPage;
    procedure ConvertToHtml;
    procedure Free;
  end;

implementation

var
  aWikiList: TWikiList;

{ TWikiTestC }

procedure TWikiTestC.CreateWikiList;
begin
  aWikiList := TWikiList.Create(nil);
end;

procedure TWikiTestC.FindAdminStartPage;
begin
  AssertTrue(aWikiList.FindWikiPage('Promet-ERP-Help/users/Administrator'));
end;

procedure TWikiTestC.ConvertToHtml;
begin
  AssertTrue(aWikiList.ExportToHTML(GetTempDir+'test.html',@aWikiList.BasicWikiInclude));
  //AssertTrue(FileExists(GetTempDir+'test.html'));
  //AssertTrue(DeleteFile(GetTempDir+'test.html'))
end;

procedure TWikiTestC.Free;
begin
  aWikiList.Free;
end;

initialization

  RegisterTest(TWikiTestC);
end.

