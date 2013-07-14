unit tdbtests;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry,
  uData;

type

  { TDBTests }

  TDBTest= class(TTestCase)
  published
    procedure Escaping;
  end;

implementation

procedure TDBTest.Escaping;
var
  tmp: String;
begin
  tmp := '''';
  Check(Data.EscapeString('''') <> tmp,'Escaping failed');
end;



initialization

  RegisterTest(TDBTest);
end.

