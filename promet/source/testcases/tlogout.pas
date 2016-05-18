unit tlogout;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, uData,
  Dialogs, uBaseApplication;

type

  { LogoutTest }

  LogoutTest= class(TTestCase)
  published
    procedure Logout;
  end;

implementation

procedure LogoutTest.Logout;
begin
//  with BaseApplication as IBaseApplication do
//    Logout;
  with BaseApplication as IBaseApplication do
    DoExit;
end;

initialization

  RegisterTest(LogoutTest); 
end.

