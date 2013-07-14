unit ulinkcheck;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, uBaseDBInterface,
  uData;

type

  LickCheck= class(TTestCase)
  published
    procedure BuildLink;
    procedure GetLinkDescOld;
    procedure GetLinkDescWebsite;
    procedure GetLinkIcon;
  end;

implementation

procedure LickCheck.BuildLink;
var
  aLink: String;
begin
  aLink := Data.BuildLink(Data.Users.DataSet);
  Check(copy(aLink,0,6) = 'USERS@');
end;

procedure LickCheck.GetLinkDescOld;
begin
  Check(Data.GetLinkDesc('TEST@TEST(Testartikel)(langtext)') = 'langtext');
end;

procedure LickCheck.GetLinkDescWebsite;
begin
  Check(Data.GetLinkDesc('http://test.de') = strWebsite);
end;

procedure LickCheck.GetLinkIcon;
begin
  Check(Data.GetLinkIcon('http://test.de') = IMAGE_WEBSITE);
end;



initialization

  RegisterTest(LickCheck); 
end.
