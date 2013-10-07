{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit lazreport_addons;

interface

uses
  lr_2dbarcodes, lr_tachart, lr_richview, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('lr_2dbarcodes', @lr_2dbarcodes.Register);
  RegisterUnit('lr_tachart', @lr_tachart.Register);
  RegisterUnit('lr_richview', @lr_richview.Register);
end;

initialization
  RegisterPackage('lazreport_addons', @Register);
end.
