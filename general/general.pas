{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit general;

interface

uses
  ubenchmark, ucomport, uError, uExtControls, uInfo, ProcessUtils, 
  uLanguageUtils, PoTranslator, UtilsVis, uColors, uImaging, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('uExtControls', @uExtControls.Register);
end;

initialization
  RegisterPackage('general', @Register);
end.
