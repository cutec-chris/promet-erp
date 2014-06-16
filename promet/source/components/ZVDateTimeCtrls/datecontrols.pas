{ This file was automatically created by Lazarus. do not edit ! 
  This source is only used to compile and install the package.
 }

unit datecontrols; 

interface

uses
  DateControlsReg, DatePicker, DBDatePicker, LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('DateControlsReg', @DateControlsReg.Register); 
end; 

initialization
  RegisterPackage('DateControls', @Register); 
end.
