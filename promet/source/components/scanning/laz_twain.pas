{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit laz_twain;

{$warn 5023 off : no warning about unused units}
interface

uses
  DelphiTwain, DelphiTwainUtils, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('laz_twain', @Register);
end.
