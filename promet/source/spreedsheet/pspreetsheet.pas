{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit pspreetsheet;

interface

uses
  uspreetsheet, usortparamsform, uformatsettingsform, sCtrls, ucurrencyform, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('pspreetsheet', @Register);
end.
