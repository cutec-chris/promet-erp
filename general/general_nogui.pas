{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit general_nogui;

interface

uses
  Utils, uGeneralStrConsts, SecureUtils, umashineid, uModifiedDS, uRTFtoTXT, 
  MathParser, Aspell, usimpleprocess, uSunRise, UtilsDate, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('general_nogui', @Register);
end.
