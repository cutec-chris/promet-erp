unit uprojecttest;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, uProjects;

type

  ProjectTest= class(TTestCase)
  published
    procedure Create;
    procedure CheckHistory;
//    procedure SelectfromLink;
    procedure Delete;
    procedure Free;
  end;

implementation
uses uData;
var
  aPrj : TProject;
procedure ProjectTest.Create;
begin
  aPrj := Tproject.Create(nil);
  aPrj.CreateTable;//get sure that the table is there
  aPrj.Insert;
  aPrj.Text.AsString := 'Testarticle';
  aPrj.CascadicPost;
end;

procedure ProjectTest.CheckHistory;
begin
  //Check(aPrj.History.Count = 1,'History <> 1 Entrys');
end;
{
procedure ProjectTest.SelectfromLink;
begin
  aPrj.SelectFromLink(Data.BuildLink(aPrj.DataSet));
  aPrj.Open;
  Check(aPrj.Count = 1,'Selected Count = '+IntToStr(aPrj.Count))
end;
}
procedure ProjectTest.Delete;
begin
  aPrj.Delete;
end;

procedure ProjectTest.Free;
begin
  aPrj.Free;
end;



initialization

  RegisterTest(ProjectTest); 
end.
