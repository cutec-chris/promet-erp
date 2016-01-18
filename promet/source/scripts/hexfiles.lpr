library hexfiles;

{$mode objfpc}{$H+}
{$DEFINE USE_BIN_STR}

uses
  Classes, sysutils,uHexBuffer, general_nogui;

var
  aHexBuffer : THexBuffer;

function ReadHexFile(Filename : PChar) : Boolean;stdcall;
begin
  if not Assigned(aHexBuffer) then aHexBuffer := THexBuffer.Create(nil);
  try
    aHexBuffer.Filename := Filename;
    Result := aHexBuffer.DataSize>0;
  except
    result := False;
  end;
end;

function SaveHexFile(Filename : PChar) : Boolean;stdcall;
begin
  if not Assigned(aHexBuffer) then aHexBuffer := THexBuffer.Create(nil);
  try
    Result := aHexBuffer.SaveToFile(Filename);
  except
    result := false;
  end;
end;
function GetHexByte(aIndex : Integer) : Integer;stdcall;
begin
  if not Assigned(aHexBuffer) then aHexBuffer := THexBuffer.Create(nil);
  Result := -1;
  try
    aHexBuffer.Update;
    if aHexBuffer.DataUsed[aIndex] then
      Result := aHexBuffer.DataBin[aIndex];
  except
    result := -1;
  end;
end;
function SetHexByte(aIndex : Integer;avalue : byte) : Boolean;stdcall;
begin
  if not Assigned(aHexBuffer) then aHexBuffer := THexBuffer.Create(nil);
  aHexBuffer.Update;
  Result := True;
  try
    aHexBuffer.DataBin[aIndex] := avalue;
    aHexBuffer.Update;
  except
    result := False;
  end;
end;

function ScriptDefinition : PChar;stdcall;
begin
  Result := 'function ReadHexFile(Filename : PChar) : Boolean;stdcall;'
       +#10+'function SaveHexFile(Filename : PChar) : Boolean;stdcall;'
       +#10+'function GetHexByte(aIndex : Integer) : Integer;stdcall;'
       +#10+'function SetHexByte(aIndex : Integer;avalue : byte) : Boolean;stdcall;'
       ;
end;

exports
  ReadHexFile,
  SaveHexFile,
  GetHexByte,
  SetHexByte,
  ScriptDefinition;

initialization
  aHexBuffer := nil;
finalization
  FreeAndNil(aHexBuffer);
end.
