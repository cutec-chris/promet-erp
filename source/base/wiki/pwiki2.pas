{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit pwiki2;

{$warn 5023 off : no warning about unused units}
interface

uses
  uWiki, uDocuments, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('pwiki2', @Register);
end.
