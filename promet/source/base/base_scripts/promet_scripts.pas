{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit promet_scripts;

interface

uses
  uprometpascalscript, uspeakinginterface, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('promet_scripts', @Register);
end.
