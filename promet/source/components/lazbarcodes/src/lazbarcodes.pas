{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit lazbarcodes; 

interface

uses
  zint, uaztec, ubasic, udatamatrix, uhelper, uqr, ureedsolomon, urender, 
  usjis, ubarcodes, LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('ubarcodes', @ubarcodes.Register); 
end; 

initialization
  RegisterPackage('lazbarcodes', @Register); 
end.
