{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit laz_fpspreadsheet_visual; 

interface

uses
  fpspreadsheetgrid, fpspreadsheetchart, LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('fpspreadsheetgrid', @fpspreadsheetgrid.Register); 
  RegisterUnit('fpspreadsheetchart', @fpspreadsheetchart.Register); 
end; 

initialization
  RegisterPackage('laz_fpspreadsheet_visual', @Register); 
end.
