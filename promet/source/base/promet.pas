{ Diese Datei wurde automatisch von Lazarus erzeugt. Sie darf nicht bearbeitet werden!
  Dieser Quelltext dient nur dem Übersetzen und Installieren des Packages.
 }

unit promet; 

interface

uses
  upositionframe, LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('upositionframe', @upositionframe.Register); 
end; 

initialization
  RegisterPackage('promet', @Register); 
end.
