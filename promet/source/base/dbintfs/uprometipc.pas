unit uprometipc;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,Utils,uBaseDbClasses;
type
  TMessageFunction = function(aMessage : string) : Boolean;

function SendIPCMessage(aMessage : string;aIPCFile : string = '') : Boolean;
function PeekIPCMessages(aIPCFile : string = '') : Boolean;

var
  OnMessageReceived : TMessageFunction = nil;
  IPCFile : string;

implementation
uses uBaseApplication;
function SendIPCMessage(aMessage: string; aIPCFile: string): Boolean;
var
  sl: TStringList;
begin
  if aIPCFile='' then
    aIPCFile:=IPCFile;
  Result := False;
  sl := TStringList.Create;
  try
    try
      with BaseApplication as IBaseApplication do
        begin
          if FileExists(aIPCFile) then
            sl.LoadFromFile(aIPCFile);
          sl.Add(aMessage);
          sl.SaveToFile(aIPCFile);
          Result := True;
        end;
    except
    end;
  finally
    sl.Free;
  end;
end;

function PeekIPCMessages(aIPCFile : string = ''): Boolean;
var
  sl: TStringList;
  achanged: Boolean;
  i: Integer;
  fs: TFileStream;
begin
  if aIPCFile='' then
    aIPCFile:=IPCFile;
  Result := False;
  sl := TStringList.Create;
  try
    try
      with BaseApplication as IBaseApplication do
        begin
          if FileExists(UniToSys(aIPCFile)) then
            begin
              fs := TFileStream.Create(aIPCFile,fmShareCompat);
              sl.LoadFromStream(fs);
              fs.Free;
            end;
          i := 0;
          sl.SaveToFile(aIPCFile); //when not saveable, raise exception and exit
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
            sl.SaveToFile(aIPCFile);
        end;
    except
    end;
  finally
    Result := aChanged;
    sl.Free;
  end;
end;

initialization
  IPCFile := GetTempDir+'PMSMessagemenager';

finalization
  DeleteFile(IPCFile);
end.

