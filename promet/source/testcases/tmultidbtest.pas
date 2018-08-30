unit tmultidbtest;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, uBaseDbClasses,Utils, db,
  uData,uBaseDBInterface,uMasterdata,uBaseDatasetInterfaces;

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
    constructor Create(Test : TMultiDBTestC);
    procedure Execute; override;
  end;

implementation

var
  aThreads : array of TMultiDataThread;

{ TMultiDataThread }

constructor TMultiDataThread.Create(Test : TMultiDBTestC);
begin
  Done := False;
  FTest := Test;
  inherited Create(False);
end;

procedure TMultiDataThread.Execute;
var
  aData: TDataSet;
  aMs: TMasterdata;
begin
  try
    FData := TBaseDBModule.Create(nil);
    FData.SetProperties(Data.Properties);
    FTest.AssertTrue(FData.Authenticate('Administrator',''));
    {
    aData := FData.GetNewDataSet('select * from "MASTERDATA"');
    aData.Open;
    aData.Locate('ID','nonextistingident',[]); //fetch all Data and iterate over it
    aData.Free;
    }

    aMs := TMasterdata.CreateEx(nil,FData);
    writeln(Integer(Self),' opening...');
    aMs.Open;  //fetch first records
    writeln(Integer(Self),' open');
    aMs.Locate('ID','nonextistingident',[]); //fetch all Data and iterate over it
    aMs.Free;

    FData.Free;

  except
    on e : Exception do
      FTest.AssertFalse(e.Message,True);
  end;
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
  Setlength(aThreads,1);
  for i := 0 to length(aThreads)-1 do
    aThreads[i] := TMultiDataThread.Create(Self);
end;

procedure TMultiDBTestC.WaitForThreads;
var
  i: Integer;
begin
  writeln('');
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

