unit twikitest;

{$mode objfpc}{$H+}

interface

uses
Classes, SysUtils, fpcunit, testutils, testregistry, uWiki, uBaseDbClasses,Utils;

type

  { TWikiTestC }

  TWikiTestC= class(TTestCase)
  published
    procedure CreateUser;
    procedure UserRights;
    procedure CreateWikiList;
    procedure FindAdminStartPage;
    procedure ConvertToHtml;
    procedure CheckSQLErrors;
    procedure Free;
  end;

implementation

var
  aWikiList: TWikiList;
  aUser: TUser;
  aHtml: String;

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
var
  aTime: Int64;
begin
  aTime := GetTicks;
  aHtml := aWikiList.PageAsHtml(True,True);
  aTime := GetTicks-aTime;
  writeln('Time: ',aTime);
end;

procedure TWikiTestC.CheckSQLErrors;
var
  aTxt: String;
begin
  aTxt := copy(aHtml,pos('error:',aHtml),length(aHtml));
  aTxt := copy(aTxt,0,pos('<',atxt)-1);
  AssertTrue('Error in SQL:'+aTxt,pos('error:',aHtml)=0);
end;

procedure TWikiTestC.Free;
begin
  aWikiList.Free;
  aUser.Free;
end;

initialization

  RegisterTest(TWikiTestC);
end.

