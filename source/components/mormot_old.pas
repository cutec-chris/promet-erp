{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit mormot_old;

{$warn 5023 off : no warning about unused units}
interface

uses
  SynCommons, mORMot, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('mormot_old', @Register);
end.
