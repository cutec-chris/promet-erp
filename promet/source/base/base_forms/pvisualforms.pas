{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit pvisualforms;

interface

uses
  uSelectReport, uLogWait, uSendMail, uSearch, uEditText, uwait, uNRights, 
  uTimeLine, uRowEditor, ugridview, uscreenshotmain, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('pvisualforms', @Register);
end.
