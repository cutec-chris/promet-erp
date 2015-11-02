unit tscripttest;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, uprometscripts,
  uprometpascalscript,uprometpythonscript,genpascalscript,genpythonscript,
  genscript,uprometcscript;

type

  TPScriptTest= class(TTestCase)
  published
    procedure PascalScriptCreate;
    procedure PascalBasicTest;
    procedure PascalScriptFree;
  end;

var
  aScript : TScript;

implementation

procedure TPScriptTest.PascalScriptCreate;
begin
  aScript := TPrometPascalScript.Create;
  aScript.Init;
end;

procedure TPScriptTest.PascalBasicTest;
begin
  aScript.Source:='program Test; begin writeln(''Hello World''); end.';
  with aScript as TPascalScript do
    begin
      Check(Compile,'Compiling failed '+Output);
      Check(Execute(Null),'Execution failed '+Output);
    end;
end;

procedure TPScriptTest.PascalScriptFree;
begin
  aScript.Free;
end;

initialization

  RegisterTest(TPScriptTest);
end.

