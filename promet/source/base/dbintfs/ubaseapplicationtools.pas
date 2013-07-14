unit ubaseapplicationtools;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uBaseApplication, uBaseDbInterface, Utils;

function VarToStr(aVar : Variant) : string;

implementation
function VarToStr(aVar : Variant) : string;
begin
  with BaseApplication as IBaseDbInterface do
    begin
      if aVar = Null then
        Result := Data.QuoteValue('')
      else Result := Data.QuoteValue(aVar);
    end;
end;

end.
