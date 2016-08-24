{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit ptimes;

interface

uses
  uEnterTime, uTimeOptions, utimeregistration, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('ptimes', @Register);
end.
