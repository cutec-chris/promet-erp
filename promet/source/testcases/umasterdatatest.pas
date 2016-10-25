unit umasterdatatest;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, uMasterdata;

type

  { MasterdataTest }

  MasterdataTest= class(TTestCase)
  published
    procedure Create;
    procedure CheckHistory;
    procedure SelectfromLink;
    procedure Delete;
    procedure Free;
  end;

implementation
uses uData;
const
  Article1: array [1 .. 11] of String = ('Kombi', 'Kneif', 'Spitz','Rohr','Wassperpumpen','Montier','Sicherungsring','Flach','Loch','Rund','Abisolier');
  Article2: array [1 .. 6] of String = (' rot', ' gelb', ' grün',' blau',' schwarz',' orange');
var
  aMD : TMasterdata;

procedure MasterdataTest.Create;
begin
  aMD := TMasterdata.Create(nil);
  aMD.Open;
  aMD.Insert;
  Randomize;
  aMD.Text.AsString := Article1[Random(High(Article1))]+'zange '+Article2[Random(High(Article2))];
  aMD.CascadicPost;
end;

procedure MasterdataTest.CheckHistory;
begin
  Check(aMD.History.Count = 1,'History <> 1 Entrys');
end;

procedure MasterdataTest.SelectfromLink;
begin
  aMD.SelectFromLink(Data.BuildLink(aMD.DataSet));
  aMD.Open;
  Check(aMD.Count = 1,'Selected Count = '+IntToStr(aMD.Count))
end;

procedure MasterdataTest.Delete;
begin
  aMd.Delete;
  Check(aMD.Count = 0,'Selected Count = '+IntToStr(aMD.Count))
end;

procedure MasterdataTest.Free;
begin
  aMD.Free;
end;



initialization

  RegisterTest(MasterdataTest); 
end.

