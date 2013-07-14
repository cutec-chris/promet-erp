unit ufavorites;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  ufavoriteschrome,
  uFavoritesIE;

function GetFavorites : TStrings;

implementation

function GetFavorites: TStrings;
var
  aLst : TStrings;
begin
  Result := TStringList.Create;
  aLst := GetIEFavourites;
  if Assigned(aLst) then
    begin
      Result.AddStrings(aLst);
      aLst.Free;
    end;
  aLst := GetChromeFavourites;
  if Assigned(aLst) then
    begin
      Result.AddStrings(aLst);
      aLst.Free;
    end;
end;

end.

