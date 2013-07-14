{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit RarBar_package;

interface

uses
  RarBar, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('RarBar', @RarBar.Register);
end;

initialization
  RegisterPackage('RarBar_package', @Register);
end.
