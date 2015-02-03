unit umasterdatatest;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, uMasterdata;

type

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
var
  aMD : TMasterdata;
procedure MasterdataTest.Create;
begin
  aMD := TMasterdata.Create(nil);
  aMD.CreateTable;//get sure that the table is there
  aMD.Insert;
  aMD.Text.AsString := 'Testarticle';
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
  aMD.Delete;
end;

procedure MasterdataTest.Free;
begin
  aMD.Free;
end;



initialization

  RegisterTest(MasterdataTest); 
end.

