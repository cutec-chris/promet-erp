{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit zcomponent_nogui;

interface

uses
  ZConnection, ZDatasetUtils, ZSqlMetadata, ZDataset, ZSqlMonitor, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('zcomponent_nogui', @Register);
end.
