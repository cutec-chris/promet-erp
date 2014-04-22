{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit base_inet;

interface

uses
  uFavoritesIE, ufavoriteschrome, ufavorites, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('base_inet', @Register);
end.
