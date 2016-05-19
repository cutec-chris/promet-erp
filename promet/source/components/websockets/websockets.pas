{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit websockets;

{$warn 5023 off : no warning about unused units}
interface

uses
  BSysUtils, BClasses, CustomServer2, WebSocket2, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('websockets', @Register);
end.
