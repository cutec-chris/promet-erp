{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit pproject2;

{$warn 5023 off : no warning about unused units}
interface

uses
  uProjects, umeeting, utask, uTimes, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('pproject2', @Register);
end.
