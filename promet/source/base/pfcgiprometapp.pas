{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit pfcgiprometapp; 

interface

uses
  ubasefcgiapplication, uUserAgents, LazarusPackageIntf;

implementation

procedure Register; 
begin
end; 

initialization
  RegisterPackage('pfcgiprometapp', @Register); 
end.
