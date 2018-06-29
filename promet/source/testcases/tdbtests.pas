unit tdbtests;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry,
  uData,uBaseDbClasses,uBaseDatasetInterfaces;

type

  { TDBTests }

  TDBTest= class(TTestCase)
  published
    procedure Escaping;
    procedure FetchAll;
  end;

implementation

procedure TDBTest.Escaping;
var
  tmp: String;
begin
  tmp := '''';
  Check(Data.EscapeString('''') <> tmp,'Escaping failed');
end;

procedure TDBTest.FetchAll;
var
  aHist: TBaseHistory;
begin
  aHist := TBaseHistory.Create(nil);
  if Data.GetDBType<>'sqlite' then
    begin
      with aHist.DataSet as IBaseDbFilter do
        Check(FetchRows=20,'FetchRows:'+IntToStr(FetchRows));
      aHist.Open;
      Check(aHist.DataSet.RecordCount<=20,'Fetched Rows:'+IntToStr(aHist.DataSet.RecordCount));
    end;
  aHist.Free;
end;



initialization

  RegisterTest(TDBTest);
end.

