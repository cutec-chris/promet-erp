{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit pvisualbaseframes;

interface

uses
  uPrometFrames, uprometframesinplace, uPrometFramesInplaceDB, 
  uQuickHelpFrame, uImageCache, uFilterFrame, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('pvisualbaseframes', @Register);
end.
