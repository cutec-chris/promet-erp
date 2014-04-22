{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit wiki2html_pkg;

interface

uses
  wikitohtml, htmltowiki, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('wiki2html_pkg', @Register);
end.
