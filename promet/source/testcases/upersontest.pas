unit upersontest;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry,
  uPerson;

type

  PersonTest= class(TTestCase)
  published
    procedure Create;
    procedure CheckHistory;
    procedure SelectfromLink;
    procedure Delete;
    procedure Free;
  end;

implementation
uses uData;
var
  aPerson : TPerson;
procedure PersonTest.Create;
begin
  aPerson := TPerson.Create(nil,Data);
  aPerson.CreateTable;//get sure that the table is there
  aPerson.Insert;
  aPerson.Text.AsString := 'Testperson';
  aPerson.CascadicPost;
end;

procedure PersonTest.CheckHistory;
begin
  Check(aPerson.History.Count = 1,'History <> 1 Entrys');
end;

procedure PersonTest.SelectfromLink;
begin
  aPerson.SelectFromLink(Data.BuildLink(aPerson.DataSet));
  aPerson.Open;
  Check(aPerson.Count = 1,'Selected Count = '+IntToStr(aPerson.Count))
end;

procedure PersonTest.Delete;
begin
  aPerson.Delete;
end;

procedure PersonTest.Free;
begin
  aPerson.Free;
end;



initialization

  RegisterTest(PersonTest); 
end.

