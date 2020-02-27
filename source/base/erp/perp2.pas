{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit perp2;

{$warn 5023 off : no warning about unused units}
interface

uses
  uMasterdata, uOrder, uPerson, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('perp2', @Register);
end.
