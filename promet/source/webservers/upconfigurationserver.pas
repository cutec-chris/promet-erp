{*******************************************************************************
  Copyright (C) Christian Ulrich info@cu-tec.de

  This source is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 2 of the License, or commercial alternative
  contact us for more information

  This code is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
  to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
  MA 02111-1307, USA.
Created 28.03.2017
*******************************************************************************}
unit upconfigurationserver;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, uappserverhttp,uAppServer,uData;

implementation

function HandleConfigRequest(Sender : TAppNetworkThrd;Method, URL: string;Headers : TStringList;Input,Output : TMemoryStream): Integer;
var
  i: Integer;
  aParameters: TStringList;
  s: String;
  tmp: String;
  aResult: TStringList;
begin
  Result := 500;
  aParameters := TStringList.Create;
  aResult := TStringList.Create;
  try
    aParameters.Clear;
    for i := 0 to Headers.Count-1 do
      begin
        s := Headers[i];
        tmp := copy(s,0,pos(':',s)-1);
        aParameters.Add(lowercase(tmp)+':'+trim(copy(s,pos(':',s)+1,length(s))));
      end;
    if copy(lowercase(url),0,15)='/configuration/' then
      begin
        Url := copy(url,16,length(url));
        if lowercase(url) = 'add' then
          begin
            Result := 200;

          end
        else if lowercase(url) = 'status' then
          begin
            if Assigned(uData.Data) then
              Result := 403
            else
              Result := 200;
          end;
      end;
  except
    Result:=500;
  end;
  aResult.Free;
  aParameters.Free;
end;

{ TPrometWebDAVMaster }

initialization
  uappserverhttp.RegisterHTTPHandler(@HandleConfigRequest);

end.

