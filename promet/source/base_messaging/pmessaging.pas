{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit pmessaging;

interface

uses
  mailchck, pmimemessages, uMessageEdit, uMessageFrame, uMimeMessages, 
  uViewMessage, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('pmessaging', @Register);
end.
