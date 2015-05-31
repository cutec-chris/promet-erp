library dbusapi;

{$mode objfpc}{$H+}
{$interfaces CORBA}

uses
  Classes,sysutils,dbus;

procedure ScriptCleanup;
begin
end;
procedure ScriptTool;
begin
end;
function ScriptDefinition : PChar;stdcall;
begin
  Result := 'function TfEnumerate : Integer;stdcall;'
       +#10+'function TfConnect(Host : PChar;Port : Integer) : Boolean;stdcall;'
       +#10+'function TfDisconnect : Boolean;stdcall;'

       +#10+'procedure TfLCDBackLightOn;stdcall;'
       +#10+'procedure TfLCDBackLightOff;stdcall;'
       +#10+'procedure TfLCDWrite(x,y : Integer;text : string);stdcall;'
       +#10+'procedure TfLCDClear;stdcall;'
       +#10+'function TfLCDButtonPressed(Button : byte) : Boolean;'

       +#10+'function TfGetVoltageById(id : Integer) : LongInt;stdcall;'
       +#10+'function TfGetVoltage(Position : char) : LongInt;stdcall;'
       +#10+'function TfGetCurrentById(id : Integer) : LongInt;stdcall;'
       +#10+'function TfGetCurrent(Position : char) : LongInt;stdcall;'
       +#10+'function TfGetPowerById(id : Integer) : LongInt;stdcall;'
       +#10+'function TfGetPower(Position : char) : LongInt;stdcall;'

       +#10+'function TfSetRelais(Position : char;Relais : Integer;SwitchOn : Boolean) : Boolean;stdcall;'
            ;
end;

exports
  dbus_error_init,
  dbus_bus_get,
  dbus_error_is_set,

  ScriptCleanup,
  ScriptTool,
  ScriptDefinition;

initialization
end.
