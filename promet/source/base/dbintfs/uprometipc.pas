unit uprometipc;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,Utils,uBaseDbClasses;
type
  TMessageFunction = function(aMessage : string) : Boolean;

function SendIPCMessage(aMessage : string) : Boolean;
function PeekIPCMessages : Boolean;

var
  OnMessageReceived : TMessageFunction = nil;

implementation

function SendIPCMessage(aMessage: string): Boolean;
var
  sl: TStringList;
begin
  Result := False;
  sl := TStringList.Create;
  try
    try
      if FileExists(GetInternalTempDir+'PMSMessagemenager') then
        sl.LoadFromFile(GetInternalTempDir+'PMSMessagemenager');
      sl.Add(aMessage);
      sl.SaveToFile(GetInternalTempDir+'PMSMessagemenager');
      Result := True;
    except
    end;
  finally
    sl.Free;
  end;
end;

function PeekIPCMessages: Boolean;
var
  sl: TStringList;
  achanged: Boolean;
  i: Integer;
  fs: TFileStream;
begin
  Result := False;
  sl := TStringList.Create;
  try
    try
      if FileExists(GetInternalTempDir+'PMSMessagemenager') then
        begin
          fs := TFileStream.Create(GetInternalTempDir+'PMSMessagemenager',fmShareCompat);
          sl.LoadFromStream(fs);
          fs.Free;
        end;
      i := 0;
      achanged := False;
      while i < sl.Count do
        begin
          if Assigned(OnMessageReceived) and OnMessageReceived(sl[i]) then
            begin
              sl.Delete(i);
              achanged := True;
            end
          else inc(i);
        end;
      if aChanged then
        sl.SaveToFile(GetInternalTempDir+'PMSMessagemenager');
    except
    end;
  finally
    Result := aChanged;
    sl.Free;
  end;
end;

end.

