unit uVisualLogin;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, uVisualTests;

type

  VisualLogin= class(TTestCase)
  published
    procedure TestHookUp; 
  end; 

implementation

procedure VisualLogin.TestHookUp; 
begin
end;



initialization

  RegisterTest(VisualLogin); 
end.
