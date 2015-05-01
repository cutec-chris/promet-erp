{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit zmsql;

interface

uses
  janSQL, janSQLExpression2, janSQLStrings, janSQLTokenizer, mwStringHashList, 
  ZMBufDataset, ZMConnection, ZMQueryDataSet, ZMReferentialKey, 
  ZMBufDataset_parser, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('zmsql', @Register);
end.
