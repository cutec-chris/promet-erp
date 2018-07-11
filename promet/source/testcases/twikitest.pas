unit twikitest;

{$mode objfpc}{$H+}

interface

uses
Classes, SysUtils, fpcunit, testutils, testregistry, uWiki, uBaseDbClasses;

type

  { TWikiTestC }

  TWikiTestC= class(TTestCase)
  published
    procedure CreateUser;
    procedure UserRights;
    procedure CreateWikiList;
    procedure FindAdminStartPage;
    procedure ConvertToHtml;
    procedure Free;
  end;

implementation

var
  aWikiList: TWikiList;
  aUser: TUser;

{ TWikiTestC }

procedure TWikiTestC.CreateUser;
begin
  aUser := TUser.Create(nil);
  aUser.Open;
end;

procedure TWikiTestC.UserRights;
var
  tmp: Integer;
begin
  tmp := aUser.Rights.Right('DOCUMENTS');
  tmp := aUser.Rights.Right('HISTORY');
  tmp := aUser.Rights.Right('LISTS');
end;

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
  aUser.Free;
end;

initialization

  RegisterTest(TWikiTestC);
end.

