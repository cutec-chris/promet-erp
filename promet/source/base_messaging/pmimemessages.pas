{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit pmimemessages;

interface

uses
  uMimeMessages, mailchck, ureceivemessage, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('pmimemessages', @Register);
end.
