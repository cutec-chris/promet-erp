library tinkerforge;

{$mode objfpc}{$H+}

uses
  Classes,sysutils;

function TinkerforgeEnumerate(AbortAfterFirst : Boolean) : Integer;stdcall;
begin
  Result :=0;
end;

function ScriptDefinition : PChar;stdcall;
begin
  Result := 'function TinkerforgeEnumerate(AbortAfterFirst : Boolean) : Integer;'
            ;
end;

exports
  TinkerforgeEnumerate,
  ScriptDefinition;

end.
