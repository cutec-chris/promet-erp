{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit pcmdprometapp;

interface

uses
  uBaseCustomApplication, uPowerState, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('pcmdprometapp', @Register);
end.
