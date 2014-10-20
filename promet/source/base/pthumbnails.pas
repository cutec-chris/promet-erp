{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit pthumbnails;

interface

uses
  fpthumbresize, FPReadJPEGintfd, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('pthumbnails', @Register);
end.
