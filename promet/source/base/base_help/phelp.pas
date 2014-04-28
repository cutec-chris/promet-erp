{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit phelp;

interface

uses
  uWikiFrame, uHelpContainer, uWikiMessage, uspelling, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('phelp', @Register);
end.
