{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit pvisualframes;

interface

uses
  uHistoryFrame, uFilterFrame, uhistoryadditem, uListFrame, uclipp, 
  uLinkFrame, uImageFrame, uMainTreeFrame, uPrometFrames, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('pvisualframes', @Register);
end.
