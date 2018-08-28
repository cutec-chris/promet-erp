unit upersontest;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry,
  uPerson,fpjson,jsonparser,uBaseDbClasses;

type

  { PersonTest }

  PersonTest= class(TTestCase)
  published
    procedure Create;
    procedure CreateAddress;
    procedure CheckHistory;
    procedure SelectfromLink;
    procedure ExportToJSON;
    procedure ExportToJSONExtMode;
    procedure Free;
  end;

implementation
uses uData;
var
  aPerson : TPerson;
const
  CName: array [1 .. 11] of String = ('Christian', 'Gabi', 'Gisela','Andreas','Heinz','Mirko','Andrea','Hans','Hartmuth','Carsten','Abisolier');
  Name: array [1 .. 6] of String = (' Hauser', ' Kunz', ' Kreuz',' Stahl',' Wendt',' Voigt');

procedure PersonTest.Create;
begin
  aPerson := TPerson.Create(nil);
  aPerson.Open;
  aPerson.Insert;
  Randomize;
  aPerson.Text.AsString := CName[Random(High(CName))]+Name[Random(High(Name))];
  aPerson.CascadicPost;
end;

const
  Street1: array [1 .. 13] of String = ('Rotdorn', 'Haag', 'Haberkorn','Haller','Hämmerling','Kreuz','Bunte','Nickel','Nördlinger','Nord','Süd','West','Ost');
  Street2: array [1 .. 9] of String = (' straße', 'gasse', 'weg',' Straße','-Weg','-Straße','rain',' Weg','weiler');

const
  Place1: array [1 .. 12] of String = ('Muster', 'Haag', 'Magde','Wart','Andreas','Wummel','Nachter','Nickel','Nord','Süd','West','Ost');
  Place2: array [1 .. 6] of String = ('burg', 'hausen', 'walde','stede','stadt','stedt');

procedure PersonTest.CreateAddress;
begin
  aPerson.Address.Insert;
  Randomize;
  aPerson.Address.AdressName.AsString:=aPerson.Text.AsString;
  aPerson.Address.Address.AsString:=Street1[Random(High(Street1))]+Street2[Random(High(Street2))]+' '+IntToStr(random(99));
  aPerson.Address.Zip.AsString:=Format('%5d',[Random(78900)]);
  aPerson.Address.City.AsString:=Place1[Random(High(Place1))]+Place2[Random(High(Place2))];
  aPerson.Address.CascadicPost;
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

procedure PersonTest.ExportToJSON;
var
  Parser: TJSONParser;
  aData: TJSONData;
begin
  Parser := TJSONParser.Create;
  Parser.Create(aPerson.ExportToJSON);
  aData := Parser.Parse;
  Parser.Free;
  aData.Free;
end;

procedure PersonTest.ExportToJSONExtMode;
var
  Parser: TJSONParser;
  aData: TJSONData;
begin
  Parser := TJSONParser.Create;
  Parser.Create(aPerson.ExportToJSON(emExtJS));
  aData := Parser.Parse;
  Parser.Free;
  aData.Free;
end;

procedure PersonTest.Free;
begin
  aPerson.Free;
end;



initialization

  RegisterTest(PersonTest); 
end.

