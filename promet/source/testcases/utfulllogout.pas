unit utfulllogout;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, uBaseApplication,
  Forms;

type

  TFullLogout= class(TTestCase)
  published
    procedure Logout;
  end; 

implementation

procedure TFullLogout.Logout;
begin
  if Assigned(BaseApplication) then with BaseApplication as IBaseApplication do
    Logout;
end;



initialization

//  RegisterTest(TFullLogout);
end.

