unit tLogin;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, uData,
  Dialogs, uBaseApplication, uBaseDbInterface;

type

  { LoginTest }

  LoginTest= class(TTestCase)
  protected
  published
    procedure LoadMandants;
    procedure Login;
  end;

implementation

procedure LoginTest.LoadMandants;
begin
  with BaseApplication as IBaseDbInterface do
    begin
      if not LoadMandants then
        fail('LoadMandants failed');
    end;
end;

procedure LoginTest.Login;
begin
  if Assigned(BaseApplication) then with BaseApplication as IBaseApplication do
    Login;
  with BaseApplication as IBaseDbInterface do
    uData.DataM := getDB;
  AssertNotNull('Data not Assigned',Data);
end;

initialization
  RegisterTest(LoginTest);
finalization
  FreeANdNil(uData.DataM);
end.

