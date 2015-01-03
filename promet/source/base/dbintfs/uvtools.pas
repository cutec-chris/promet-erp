unit uvtools;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils,Utils;
function IsField(FName,Value: string): boolean;
function GetValue(Value : string;IsUTF8 : Boolean = False) : string;
function SetValue(Value : string) : string;
function HasAttrib(Attrib,Value : string) : Boolean;
function VEncodeString(s : string) : string;
function VDecodeString(s : string) : string;
implementation
function IsField(FName,Value: string): boolean;
var
  i,j: integer;
begin
  i := Length(FName);
  j := Length(Value);
  Result := (Pos(FName,Value) = 1) and ((i = j) or
    (Value[i+1] in [';',':']) or (FName[i] in [';',':']));
end;
function GetValue(Value : string;IsUTF8 : Boolean = False) : string;
begin
  if IsUTF8 then
    result := VDecodeString(copy(Value,pos(':',Value)+1,length(Value)))
  else
    Result := SysToUni(VDecodeString(copy(Value,pos(':',Value)+1,length(Value))));
end;

function SetValue(Value: string): string;
begin
  Result := UniToSys(Value);
end;

function HasAttrib(Attrib,Value : string) : Boolean;
begin
  Result := pos(Attrib,Value) > 0;
end;
function VEncodeString(s : string) : string;
var
  i,j,k,m,n: Integer;
begin
  Result := '';
  j := 0; k := Length(s);
  for i := 1 to k do begin
    if s[i] = '=' then begin
      Result := Result + '=' + IntToHex(Ord(s[i]),2);
      inc(j,2);
    end
    else
      if ((s[i] >= #32) and (s[i] <= #126)) then
        Result := Result + s[i]
      else begin
        Result := Result + '=' + IntToHex(Ord(s[i]),2);
        inc(j,2);
      end;
    inc(j);
    if (j > 73) and (i < k) then begin
      n := Length(Result);
      m := n;
      while (m <> 0) and (Result[m] <> ' ') do dec(m);
      if m <> 0 then begin
        Insert('=' + sLinebreak,Result,m);
        j := n - m + 1;
      end;
    end;
  end;
end;
function VDecodeString(s : string) : string;
begin
  s := trim(s);
  Result := '';

  while length(s) > 0 do begin
    // Check for 'soft' line break
    if (s[1] = '=') and (Length(s) > 1) then begin
      Result := Result + chr(StrToInt('$' + s[2] + s[3]));
      Delete(s, 1, 3);
    end
    else begin
      // If 'soft' line break, just delete it
      if s[1] <> '=' then Result := Result + s[1];
      Delete(s, 1, 1);
    end;
  end;
end;
end.

