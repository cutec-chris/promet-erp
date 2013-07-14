unit DateControlsReg; 

{$mode objfpc}{$H+}

interface

procedure Register;

implementation

uses
  Classes, DatePicker, DBDatePicker, LResources;

procedure Register;
begin
  RegisterComponents('DateControls', [
                          TDatePicker,
                          TDBDatePicker
                     ]);
end;

initialization
{$i datecontrols.lrs}

end.

