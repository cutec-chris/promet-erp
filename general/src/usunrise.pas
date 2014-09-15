unit uSunRise;
{ Funktion zur Berechnung von Sonnenaufgang und Sonenuntergang }
{ Parameter                                                    }
{  Input: Lat     = Latitude = Länge in Grad                   }
{         Lon     = Longitude = Breite in Grad                 }
{         Date    = Datum                                      }
{ Output: SunRise = Sonnenaufgangszeit                         }
{         SunSet  = Sonnenuntergangszeit                       }
{         dt      = Maximal mögliche Sonnenscheinzeit          }

interface

  uses SysUtils, Math;
  procedure SunShine(Lat,Lon:Double;Date:TDateTime;var SunRise,SunSet,dt:TDateTime);

implementation

  { ArcTan von X/Y in rad von 0 bis 2*pi }
  Function ATan2(Y,X:Double):Double;
  Var at:Double;
  Begin
    If X <> 0.0 Then Begin
      at := ArcTan(Y/X);
    End Else Begin
      If Y > 0.0  Then Begin
        at := Pi / 2;
      End Else Begin
        at := Pi * 3 / 2;
      End;
    End;
    If X < 0.0  Then at := at + Pi;
    If at < 0.0 Then at := at + pi * 2;
    Result := at;
  End;

  { Float Modulus }
  Function RMod(X,Y:Double):Double;
  Begin
    Result := X - Int(X/Y) * Y;
  End;

  Procedure SunShine(Lat,Lon:Double;Date:TDateTime;var SunRise,SunSet,dt:TDateTime);
  var h,l,m,ra,tr,ts,cd,cl,cLat,sd,sl,sLAt:Double;
      N:Integer; yy,mm,dd:Word;
  Begin
    { Tag des Jahres }
    DecodeDate(Date,yy,mm,dd);
    N := Trunc(EncodeDate(yy,mm,dd)) - Trunc(EncodeDate(yy,1,1)) + 1;

    { Sonnenaufgang }
    sLat := Sin(DegToRad(LAT));
    cLat := Cos(DegToRad(LAT));
    tr := N + (6.0 - Lon / 15.0) / 24.0;
    m := 0.9856 * tr - 3.289;
    l := RMod(m + 1.916 * Sin(DegToRad(m)) + 0.020 * Sin(DegToRad(2.0 * m)) + 282.634,360.0);
    sl := Sin(DegToRad(l));
    cl := Cos(DegToRad(l));
    ra := 180 / pi * ATan2(0.91746 * sl,cl) / 15.0;
    sd := 0.39782 * sl;
    cd := SQRT(1.0 - SQR(sd));
    h := (360.0 - 180 / pi * ArcCos((-0.01454 - sd * sLat) / (cd * cLat))) / 15.0;
    tr := RMod(H + ra - 0.065710 * tr - 6.622 - Lon / 15.0,24.0);
    SunRise := Trunc(Date) + tr / 24;

    { Sonnenuntergang }
    ts := N + (18.0 - Lon / 15.0) / 24.0;
    m := 0.9856 * ts - 3.289;
    l := RMod(m + 1.916 * Sin(DegToRad(m)) + 0.020 * Sin(DegToRad(2.0 * m)) + 282.634,360.0);
    sl := Sin(DegToRad(l));
    cl := Cos(DegToRad(l));
    ra := 180 / pi * ATan2(0.91746 * sl,cl) / 15.0;
    sd := 0.39782 * sl;
    cd := Sqrt(1.0 - Sqr(sd));
    h := (180 / pi * ArcCos((-0.01454 - sd * sLat) / (cd * cLat))) / 15.0;
    ts := RMod(h + ra - 0.065710 * ts - 6.622 - Lon / 15.0 + 24.0,24.0);
    If ts < 0.0 Then ts := ts + 24.0;
    SunSet := Trunc(Date) + ts / 24;

    { Mögliche Sonnenstunden }
    dt := (SunSet - SunRise) * 24;
  End;

end.
