{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit sql_parser;

interface

uses
  sqlparser, sqlscanner, sqltree, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('sql_parser', @Register);
end.
