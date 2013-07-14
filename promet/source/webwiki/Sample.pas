unit Sample;

interface

function LookupCountry: string;
function LookupCity: string;
function LookupOrg: string;

implementation

uses GeoIP;

function LookupCountry: string;
var
   GeoIP: TGeoIP;
   GeoIPCountry: TGeoIPCountry;
begin
  GeoIP := TGeoIP.Create('GeoIP.dat');
  try
    if GeoIP.GetCountry('207.46.244.188', GeoIPCountry) = GEOIP_SUCCESS then
    begin
      Result := GeoIPCountry.CountryName;
    end
    else
    begin
      Result := 'ERROR';
    end;
  finally
    GeoIP.Free;
  end;
end;

function LookupCity: string;
var
   GeoIP: TGeoIP;
   GeoIPCity: TGeoIPCity;
begin
  GeoIP := TGeoIP.Create('GeoIPCity.dat');
  try
    if GeoIP.GetCity('207.46.244.188', GeoIPCity) = GEOIP_SUCCESS then
    begin
      Result := GeoIPCity.City + ', ' + GeoIPCity.Region + ', ' + GeoIPCity.CountryName;
    end
    else
    begin
      Result := 'ERROR';
    end;
  finally
    GeoIP.Free;
  end;
end;

function LookupOrg: string;
var
   GeoIP: TGeoIP;
   GeoIPOrg: TGeoIPOrg;
begin
  GeoIP := TGeoIP.Create('GeoIPOrg.dat');
  try
    if GeoIP.GetOrg('207.46.244.188', GeoIPOrg) = GEOIP_SUCCESS then
    begin
      Result := GeoIPOrg.Name;
    end
    else
    begin
      Result := 'ERROR';
    end;
  finally
    GeoIP.Free;
  end;
end;

end.
