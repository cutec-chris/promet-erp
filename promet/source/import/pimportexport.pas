{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit pImportExport;

interface

uses
  uDataImportConfig, uDataimportCSV, uDataImportDBF, uDataImportFixedLength, 
  uDataImportOffice, uDataImport, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('pImportExport', @Register);
end.
