program prometerp_cmd_test;

{$mode objfpc}{$H+}

uses
  Classes, consoletestrunner, lazmouseandkeyinput,Interfaces
  ,visualnewmandant
  ;

type

  { TLazTestRunner }

  TMyTestRunner = class(TTestRunner)
  protected
  // override the protected methods of TTestRunner to customize its behavior
  end;

var
  Application: TMyTestRunner;

begin
  Application := TMyTestRunner.Create(nil);
  Application.Initialize;
  Application.Run;
  Application.Free;
end.
