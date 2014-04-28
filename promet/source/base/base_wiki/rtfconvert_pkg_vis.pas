{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit rtfconvert_pkg_vis;

interface

uses
  htmltortf, FontComboBox, urichframe, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('FontComboBox', @FontComboBox.Register);
end;

initialization
  RegisterPackage('rtfconvert_pkg_vis', @Register);
end.
