{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit mormot_zeos;

{$warn 5023 off : no warning about unused units}
interface

uses
  SynDBZeos, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('mormot_zeos', @Register);
end.
