library tinkerforge;

{$mode objfpc}{$H+}

uses
  Classes,sysutils, IPConnection, Device, BrickletLCD20x4, BrickletLCD16x2,
  BrickletVoltageCurrent;
type
  TStation = class
    procedure ipconConnected(sender: TIPConnection; const connectReason: byte);
    procedure ipconEnumerate(sender: TIPConnection; const uid: string;
      const connectedUid: string; const position: char;
      const hardwareVersion: TVersionNumber;
      const firmwareVersion: TVersionNumber; const deviceIdentifier: word;
      const enumerationType: byte);
  private
    ipcon: TIPConnection;
    Devices : TList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure ConnectedCB(sender: TIPConnection; const connectedReason: byte);
    property Conn : TIPConnection read ipcon;
  end;

var
  Station : TStation;

procedure TStation.ipconConnected(sender: TIPConnection;
  const connectReason: byte);
begin
  if (connectReason = IPCON_CONNECT_REASON_AUTO_RECONNECT) then begin
    ipcon.Enumerate;
  end;
end;
procedure TStation.ipconEnumerate(sender: TIPConnection; const uid: string;
  const connectedUid: string; const position: char;
  const hardwareVersion: TVersionNumber; const firmwareVersion: TVersionNumber;
  const deviceIdentifier: word; const enumerationType: byte);
var
  Dev : TDevice;
begin
  if ((enumerationType = IPCON_ENUMERATION_TYPE_CONNECTED) or
      (enumerationType = IPCON_ENUMERATION_TYPE_AVAILABLE)) then
    begin
      if (deviceIdentifier = BRICKLET_LCD_20X4_DEVICE_IDENTIFIER) then begin
        Dev := TBrickletLCD20x4.Create(UID, ipcon);
        Devices.Add(Dev);
        TBrickletLCD20x4(Dev).ClearDisplay();
      end;
      if (deviceIdentifier = BRICKLET_LCD_16X2_DEVICE_IDENTIFIER) then begin
        Dev := TBrickletLCD16x2.Create(UID, ipcon);
        Devices.Add(Dev);
        TBrickletLCD16x2(Dev).ClearDisplay();
      end;
      if (deviceIdentifier = BRICKLET_VOLTAGE_CURRENT_DEVICE_IDENTIFIER) then begin
        Dev := TBrickletVoltageCurrent.Create(UID, ipcon);
        Devices.Add(Dev);
      end;
    end;
end;
constructor TStation.Create;
begin
  ipcon := TIPConnection.Create;
  Devices := TList.Create;
  ipcon.OnEnumerate:=@ipconEnumerate;
  ipcon.OnConnected:=@ipconConnected;
end;
destructor TStation.Destroy;
begin
  Devices.Free;
  ipcon.Free;
  inherited Destroy;
end;
procedure TStation.ConnectedCB(sender: TIPConnection;
  const connectedReason: byte);
begin
end;
function TfConnect(Host : PChar;Port : Integer) : Boolean;stdcall;
begin
  if not Assigned(Station) then
    Station := TStation.Create;
  Station.Conn.Connect(Host,Port);
  Station.Conn.Enumerate;
  result := Station.Conn.IsConnected;
end;
function TfDisconnect : Boolean;stdcall;
begin
  Result := True;
  FreeAndNil(Station);
end;
function TfEnumerate : Integer;stdcall;
begin
  sleep(10);
  Result :=Station.Devices.Count;
end;
procedure TfLCDBackLightOn;stdcall;
var
  i: Integer;
begin
  for i := 0 to Station.Devices.Count-1 do
    begin
      if TDevice(Station.Devices[i]) is TBrickletLCD16x2 then
        with TBrickletLCD16x2(Station.Devices[i]) do
          begin
            BacklightOn;
          end;
      if TDevice(Station.Devices[i]) is TBrickletLCD20x4 then
        with TBrickletLCD20x4(Station.Devices[i]) do
          begin
            BacklightOn;
          end;
    end;
end;
procedure TfLCDBackLightOff;stdcall;
var
  i: Integer;
begin
  for i := 0 to Station.Devices.Count-1 do
    begin
      if TDevice(Station.Devices[i]) is TBrickletLCD16x2 then
        with TBrickletLCD16x2(Station.Devices[i]) do
          begin
            BacklightOff;
          end;
      if TDevice(Station.Devices[i]) is TBrickletLCD20x4 then
        with TBrickletLCD20x4(Station.Devices[i]) do
          begin
            BacklightOff;
          end;
    end;
end;
procedure TfLCDWrite(x,y : Integer;text : string);stdcall;
var
  i: Integer;
begin
  for i := 0 to Station.Devices.Count-1 do
    begin
      if TDevice(Station.Devices[i]) is TBrickletLCD16x2 then
        with TBrickletLCD16x2(Station.Devices[i]) do
          begin
            WriteLine(y,x,text);
          end;
      if TDevice(Station.Devices[i]) is TBrickletLCD20x4 then
        with TBrickletLCD20x4(Station.Devices[i]) do
          begin
            WriteLine(y,x,text);
          end;
    end;
end;
procedure TfLCDClear;stdcall;
var
  i: Integer;
begin
  for i := 0 to Station.Devices.Count-1 do
    begin
      if TDevice(Station.Devices[i]) is TBrickletLCD16x2 then
        with TBrickletLCD16x2(Station.Devices[i]) do
          begin
            ClearDisplay;
          end;
      if TDevice(Station.Devices[i]) is TBrickletLCD20x4 then
        with TBrickletLCD20x4(Station.Devices[i]) do
          begin
            ClearDisplay;
          end;
    end;
end;
function TfLCDButtonPressed(Button : byte) : Boolean;
var
  i: Integer;
begin
  for i := 0 to Station.Devices.Count-1 do
    begin
      if TDevice(Station.Devices[i]) is TBrickletLCD16x2 then
        with TBrickletLCD16x2(Station.Devices[i]) do
          begin
            Result := IsButtonPressed(Button);
          end;
      if TDevice(Station.Devices[i]) is TBrickletLCD20x4 then
        with TBrickletLCD20x4(Station.Devices[i]) do
          begin
            Result := IsButtonPressed(Button);
          end;
    end;
end;
function TfGetVoltageById(id : Integer) : LongInt;stdcall;
var
  a: Integer;
  i: Integer;
begin
  Result := -1;
  a := 0;
  for i := 0 to Station.Devices.Count-1 do
    begin
      if TDevice(Station.Devices[i]) is TBrickletVoltageCurrent then
        begin
          if a=id then
            begin
              Result := TBrickletVoltageCurrent(Station.Devices[i]).GetVoltage;
              exit;
            end;
          inc(a);
        end;
    end;
end;
function TfGetVoltage(position : char) : LongInt;stdcall;
var
  a: Integer;
  i: Integer;
  aDID: word;
  aFWV: TVersionNumber;
  aHWV: TVersionNumber;
  aPosition: char;
  aConUID: string;
  aUid: string;
begin
  Result := -1;
  for i := 0 to Station.Devices.Count-1 do
    begin
      if TDevice(Station.Devices[i]) is TBrickletVoltageCurrent then
        begin
          TDevice(Station.Devices[i]).GetIdentity(aUid,aConUID,aPosition,aHWV,aFWV,aDID);
          if lowercase(position)=lowercase(aPosition) then
            begin
              Result := TBrickletVoltageCurrent(Station.Devices[i]).GetVoltage;
              exit;
            end;
          inc(a);
        end;
    end;
end;
function TfGetCurrentById(id : Integer) : LongInt;stdcall;
var
  a: Integer;
  i: Integer;
begin
  Result := -1;
  a := 0;
  for i := 0 to Station.Devices.Count-1 do
    begin
      if TDevice(Station.Devices[i]) is TBrickletVoltageCurrent then
        begin
          if a=id then
            begin
              Result := TBrickletVoltageCurrent(Station.Devices[i]).GetCurrent;
              exit;
            end;
          inc(a);
        end;
    end;
end;
function TfGetCurrent(position : char) : LongInt;stdcall;
var
  a: Integer;
  i: Integer;
  aDID: word;
  aFWV: TVersionNumber;
  aHWV: TVersionNumber;
  aPosition: char;
  aConUID: string;
  aUid: string;
begin
  Result := -1;
  for i := 0 to Station.Devices.Count-1 do
    begin
      if TDevice(Station.Devices[i]) is TBrickletVoltageCurrent then
        begin
          TDevice(Station.Devices[i]).GetIdentity(aUid,aConUID,aPosition,aHWV,aFWV,aDID);
          if lowercase(position)=lowercase(aPosition) then
            begin
              Result := TBrickletVoltageCurrent(Station.Devices[i]).GetCurrent;
              exit;
            end;
          inc(a);
        end;
    end;
end;
function TfGetPowerById(id : Integer) : LongInt;stdcall;
var
  a: Integer;
  i: Integer;
begin
  Result := -1;
  a := 0;
  for i := 0 to Station.Devices.Count-1 do
    begin
      if TDevice(Station.Devices[i]) is TBrickletVoltageCurrent then
        begin
          if a=id then
            begin
              Result := TBrickletVoltageCurrent(Station.Devices[i]).GetPower;
              exit;
            end;
          inc(a);
        end;
    end;
end;
function TfGetPower(position : char) : LongInt;stdcall;
var
  a: Integer;
  i: Integer;
  aDID: word;
  aFWV: TVersionNumber;
  aHWV: TVersionNumber;
  aPosition: char;
  aConUID: string;
  aUid: string;
begin
  Result := -1;
  for i := 0 to Station.Devices.Count-1 do
    begin
      if TDevice(Station.Devices[i]) is TBrickletVoltageCurrent then
        begin
          TDevice(Station.Devices[i]).GetIdentity(aUid,aConUID,aPosition,aHWV,aFWV,aDID);
          if lowercase(position)=lowercase(aPosition) then
            begin
              Result := TBrickletVoltageCurrent(Station.Devices[i]).GetPower;
              exit;
            end;
          inc(a);
        end;
    end;
end;

procedure ScriptCleanup;
begin
  TfDisconnect;
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

            ;
end;

exports
  TfConnect,
  TfEnumerate,
  TfDisconnect,

  TfLCDBackLightOn,
  TfLCDBackLightOff,
  TfLCDWrite,
  TfLCDClear,
  TfLCDButtonPressed,

  TfGetVoltage,
  TfGetVoltageById,
  TfGetCurrent,
  TfGetCurrentById,
  TfGetPower,
  TfGetPowerById,

  ScriptCleanup,
  ScriptDefinition;

end.
