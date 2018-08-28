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
  nData: TBaseDBModule;
begin
  try
    nData := TBaseDBModule.Create(nil);
    writeln(FData.Properties);
    nData.SetProperties(FData.Properties);
    {
    aData := FData.GetNewDataSet('select * from "MASTERDATA"');
    aData.Open;
    aData.Locate('ID','nonextistingident',[]); //fetch all Data and iterate over it
    aData.Free;
    }


    aMs := TMasterdata.CreateEx(nil,nData);
    writeln(Integer(Self),' opening...');
    writeln(Integer(Self),' SQL:',(aMs as IBaseDBFilter).GetSQL);
    aMs.Open;  //fetch first records
    writeln(Integer(Self),' open');
    //aMs.Locate('ID','nonextistingident',[]); //fetch all Data and iterate over it
    aMs.Free;

    nData.Free;

  except
    on e : Exception do
      writeln(Integer(Self),' Exception '+e.Message);
  end;

  //FData.CriticalSection.Leave;
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

