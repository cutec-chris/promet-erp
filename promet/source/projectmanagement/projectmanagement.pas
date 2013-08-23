{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit projectmanagement;

interface

uses
  uProjectFrame, uprojectoverview, uminuteframe, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('projectmanagement', @Register);
end.
