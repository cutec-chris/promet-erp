unit tmultidbtest;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, uBaseDbClasses,Utils, db,
  uData,uBaseDBInterface,uMasterdata;

type
  { TMultiDBTestC }

  TMultiDBTestC= class(TTestCase)
  published
    procedure UpgradeDatabase;
    procedure CreateThreads;
    procedure WaitForThreads;
  end;

  { TMultiDataThread }

  TMultiDataThread = class(TThread)
  private
    FData : TBaseDBModule;
    FTest : TMultiDBTestC;
  public
    Done : Boolean;
    constructor Create(Data : TBaseDBModule;Test : TMultiDBTestC);
    procedure Execute; override;
  end;

implementation

var
  aThreads : array of TMultiDataThread;

{ TMultiDataThread }

constructor TMultiDataThread.Create(Data: TBaseDBModule;Test : TMultiDBTestC);
begin
  FData := Data;
  Done := False;
  FTest := Test;
  FTest.AssertTrue(FData.Authenticate('Administrator',''));
  inherited Create(False);
end;

procedure TMultiDataThread.Execute;
var
  aData: TDataSet;
  aMs: TMasterdata;
begin

  {
  aData := FData.GetNewDataSet('select * from "MASTERDATA"');
  aData.Open;
  aData.Locate('ID','nonextistingident',[]); //fetch all Data and iterate over it
  aData.Free;
  }


  aMs := TMasterdata.CreateEx(nil,FData);
  aMs.Open;  //fetch first records
  //aMs.Locate('ID','nonextistingident',[]); //fetch all Data and iterate over it
  aMs.Free;

  FData.CriticalSection.Leave;
  Done := True;
end;

{ TMultiDBTestC }

procedure TMultiDBTestC.UpgradeDatabase;
begin
  InitMultiData(4);
end;

procedure TMultiDBTestC.CreateThreads;
var
  i: Integer;
begin
  Setlength(aThreads,4);
  for i := 0 to length(aThreads)-1 do
    aThreads[i] := TMultiDataThread.Create(uData.GetData,Self);
end;

procedure TMultiDBTestC.WaitForThreads;
var
  i: Integer;
begin
  for i := 0 to length(aThreads)-1 do
    begin
      while not aThreads[i].Done do sleep(1);
      aThreads[i].WaitFor;
      aThreads[i].Free;
      aThreads[i] := nil;
    end;
end;

initialization

  RegisterTest(TMultiDBTestC);
end.

