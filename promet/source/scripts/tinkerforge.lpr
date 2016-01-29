library tinkerforge;

{$mode objfpc}{$H+}
{$interfaces CORBA}

uses
  Classes,sysutils, IPConnection, Device, BrickletLCD20x4, BrickletLCD16x2,
  BrickletVoltageCurrent,BrickletIndustrialQuadRelay,BrickletDualRelay,process,
  Utils,BrickletColor;
type
  TStation = class
    procedure ipconConnected(sender: TIPConnection; const connectReason: byte);
    procedure ipconDisconnected(sender: TIPConnection;
      const disconnectReason: byte);
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
  BrickV: TProcess;
  DeviceList : string;

procedure TStation.ipconConnected(sender: TIPConnection;
  const connectReason: byte);
begin
  if (connectReason = IPCON_CONNECT_REASON_AUTO_RECONNECT) then begin
    ipcon.Enumerate;
  end;
end;

procedure TStation.ipconDisconnected(sender: TIPConnection;
  const disconnectReason: byte);
begin
  writeln('puff');
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
      DeviceList := DeviceList+connectedUid+','+IntToStr(deviceIdentifier)+','+position+','+uid+#10;
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
      if (deviceIdentifier = BRICKLET_INDUSTRIAL_QUAD_RELAY_DEVICE_IDENTIFIER) then begin
        Dev := TBrickletIndustrialQuadRelay.Create(UID, ipcon);
        Devices.Add(Dev);
      end;
      if (deviceIdentifier = BRICKLET_DUAL_RELAY_DEVICE_IDENTIFIER) then begin
        Dev := TBrickletDualRelay.Create(UID, ipcon);
        Devices.Add(Dev);
      end;
      if (deviceIdentifier = BRICKLET_COLOR_DEVICE_IDENTIFIER) then begin
        Dev := TBrickletColor.Create(UID, ipcon);
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
  ipcon.OnDisconnected:=@ipconDisconnected;
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
  DeviceList := '';
  if not Assigned(Station) then
    Station := TStation.Create;
  try
    Station.Conn.Connect(Host,Port);
    Station.Conn.Enumerate;
    result := Station.Conn.IsConnected;
    sleep(20);
  except
    Result := False;
  end;
end;
function TfDisconnect : Boolean;stdcall;
begin
  Result := True;
  FreeAndNil(Station);
end;
function TfEnumerate : Integer;stdcall;
begin
  if Station=nil then exit;
  sleep(10);
  Result :=Station.Devices.Count;
end;
procedure TfLCDBackLightOn;stdcall;
var
  i: Integer;
begin
  if Station=nil then exit;
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
  if Station=nil then exit;
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
  if Station=nil then exit;
  if not Station.ipcon.IsConnected then exit;
  try
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
  except
  end;
end;
procedure TfLCDClear;stdcall;
var
  i: Integer;
begin
  if Station=nil then exit;
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
function TfLCDButtonPressed(Button : byte) : Boolean;stdcall;
var
  i: Integer;
begin
  Result := False;
  if Station=nil then exit;
  try
    if not Station.ipcon.IsConnected then exit;
    if Button>3 then exit;
    for i := 0 to Station.Devices.Count-1 do
      begin
        if TDevice(Station.Devices[i]) is TBrickletLCD16x2 then
          with TDevice(Station.Devices[i]) as TBrickletLCD16x2 do
            Result := IsButtonPressed(Button);
        if TDevice(Station.Devices[i]) is TBrickletLCD20x4 then
          with TDevice(Station.Devices[i]) as TBrickletLCD20x4 do
            Result := IsButtonPressed(Button);
      end;
  except
  end;
end;
function TfGetVoltageById(id : Integer) : LongInt;stdcall;
var
  a: Integer;
  i: Integer;
begin
  Result := -1;
  a := 0;
  if Station=nil then exit;
  if not Station.ipcon.IsConnected then exit;
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
function TfGetVoltage(position : pchar) : LongInt;stdcall;
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
  if Station=nil then exit;
  if not Station.ipcon.IsConnected then exit;
  for i := 0 to Station.Devices.Count-1 do
    begin
      if TDevice(Station.Devices[i]) is TBrickletVoltageCurrent then
        begin
          TDevice(Station.Devices[i]).GetIdentity(aUid,aConUID,aPosition,aHWV,aFWV,aDID);
          if (lowercase(position)=lowercase(aConUID)+'.'+lowercase(aPosition)) or (lowercase(position)=lowercase(aPosition)) then
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
  if Station=nil then exit;
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
function TfGetCurrent(position : pchar) : LongInt;stdcall;
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
  if Station=nil then exit;
  for i := 0 to Station.Devices.Count-1 do
    begin
      if TDevice(Station.Devices[i]) is TBrickletVoltageCurrent then
        begin
          TDevice(Station.Devices[i]).GetIdentity(aUid,aConUID,aPosition,aHWV,aFWV,aDID);
          if (lowercase(position)=lowercase(aConUID)+'.'+lowercase(aPosition)) or (lowercase(position)=lowercase(aPosition)) then
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
  if Station=nil then exit;
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
function TfGetPower(position : pchar) : LongInt;stdcall;
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
  if Station=nil then exit;
  for i := 0 to Station.Devices.Count-1 do
    begin
      if TDevice(Station.Devices[i]) is TBrickletVoltageCurrent then
        begin
          TDevice(Station.Devices[i]).GetIdentity(aUid,aConUID,aPosition,aHWV,aFWV,aDID);
          if (lowercase(position)=lowercase(aConUID)+'.'+lowercase(aPosition)) or (lowercase(position)=lowercase(aPosition)) then
            begin
              Result := TBrickletVoltageCurrent(Station.Devices[i]).GetPower;
              exit;
            end;
          inc(a);
        end;
    end;
end;
function TfSetRelais(Position : pchar;Relais : Integer;SwitchOn : Boolean) : Boolean;stdcall;
var
  aUid: string;
  aConUID: string;
  aPosition: char;
  aHWV: TVersionNumber;
  aFWV: TVersionNumber;
  aDID: word;
  aRelais: boolean;
  bRelais: boolean;
  sRelais: Word;
  i: Integer;
begin
  Result := False;
  if Station=nil then exit;
  for i := 0 to Station.Devices.Count-1 do
    //begin
      //if TDevice(Station.Devices[i]) is TBrickletVoltageCurrent then
        begin
          TDevice(Station.Devices[i]).GetIdentity(aUid,aConUID,aPosition,aHWV,aFWV,aDID);
          if (lowercase(position)=lowercase(aConUID)+'.'+lowercase(aPosition)) or (lowercase(position)=lowercase(aPosition)) then
            begin
              if TObject(Station.Devices[i]) is TBrickletDualRelay then
                begin
                  Result := True;
                  TBrickletDualRelay(Station.Devices[i]).GetState(aRelais,bRelais);
                  case Relais of
                  0:TBrickletDualRelay(Station.Devices[i]).SetState(SwitchOn,bRelais);
                  1:TBrickletDualRelay(Station.Devices[i]).SetState(aRelais,SwitchOn);
                  else Result := False;
                  end;
                end
              else if TObject(Station.Devices[i]) is TBrickletIndustrialQuadRelay then
                begin
                  Result := True;
                  sRelais := TBrickletIndustrialQuadRelay(Station.Devices[i]).GetValue;
                  if SwitchOn then
                    begin
                      if sRelais and (1 shl Relais) <> (1 shl Relais) then
                        sRelais:=sRelais or (1 shl Relais)
                    end
                  else
                    begin
                      if sRelais and (1 shl Relais) = (1 shl Relais) then
                        sRelais:=sRelais xor (1 shl Relais);
                    end;
                  Result := sRelais<=$F;
                  if Result then
                    TBrickletIndustrialQuadRelay(Station.Devices[i]).SetValue(sRelais);
                end;
              exit;
            end;
        end;
    //end;
end;
function TfGetColorById(id : Integer) : Cardinal;stdcall;
var
  a: Integer;
  i: Integer;
  r: word;
  g: word;
  b: word;
  c: word;
  rgbc: array[0..3] of Byte;
begin
  Result := -1;
  a := 0;
  if Station=nil then exit;
  for i := 0 to Station.Devices.Count-1 do
    begin
      if TDevice(Station.Devices[i]) is TBrickletColor then
        begin
          if a=id then
            begin
              TBrickletColor(Station.Devices[i]).GetColor(r,g,b,c);
              rgbc[0] := r div 256;
              rgbc[1] := g div 256;
              rgbc[2] := b div 256;
              rgbc[3] := c div 256;
              Result := dword(rgbc);
              exit;
            end;
          inc(a);
        end;
    end;
end;
function TfGetColor(position : pchar) : Cardinal;stdcall;
var
  a: Integer;
  i: Integer;
  aDID: word;
  aFWV: TVersionNumber;
  aHWV: TVersionNumber;
  aPosition: char;
  aConUID: string;
  aUid: string;
  r: word;
  g: word;
  b: word;
  c: word;
  rgbc: array[0..3] of Byte;
begin
  Result := -1;
  if Station=nil then exit;
  for i := 0 to Station.Devices.Count-1 do
    begin
      if TDevice(Station.Devices[i]) is TBrickletColor then
        begin
          TDevice(Station.Devices[i]).GetIdentity(aUid,aConUID,aPosition,aHWV,aFWV,aDID);
          if (lowercase(position)=lowercase(aConUID)+'.'+lowercase(aPosition)) or (lowercase(position)=lowercase(aPosition)) then
            begin
              TBrickletColor(Station.Devices[i]).GetColor(r,g,b,c);
              rgbc[0] := r div 256;
              rgbc[1] := g div 256;
              rgbc[2] := b div 256;
              rgbc[3] := c div 256;
              Result := dword(rgbc);
              exit;
            end;
          inc(a);
        end;
    end;
end;
function TfGetDeviceList : pchar;stdcall;
begin
  Result := pchar(DeviceList);
end;
procedure ScriptCleanup;stdcall;
begin
  TfDisconnect;
end;
procedure ScriptTool;
begin
  if not Assigned(BrickV) then
    begin
      BrickV := TProcess.Create(nil);
      BrickV.Executable:='brickv';
      BrickV.CurrentDirectory:=GetProgramDir+'Tinkerforge'+DirectorySeparator+'Brickv';
      BrickV.Options:=[poNoConsole];
      try
        BrickV.Execute;
      except
        FreeAndNil(BrickV);
      end;
    end;
end;
function ScriptDefinition : PChar;stdcall;
begin
  Result := 'function TfEnumerate : Integer;stdcall;'
       +#10+'function TfConnect(Host : PChar;Port : Integer) : Boolean;stdcall;'
       +#10+'function TfDisconnect : Boolean;stdcall;'
       +#10+'function TfGetDeviceList : pchar;stdcall;'

       +#10+'procedure TfLCDBackLightOn;stdcall;'
       +#10+'procedure TfLCDBackLightOff;stdcall;'
       +#10+'procedure TfLCDWrite(x,y : Integer;text : string);stdcall;'
       +#10+'function TfLCDButtonPressed(Button : byte) : Boolean;stdcall;'
       +#10+'procedure TfLCDClear;stdcall;'

       +#10+'function TfGetVoltageById(id : Integer) : LongInt;stdcall;'
       +#10+'function TfGetVoltage(Position : pchar) : LongInt;stdcall;'
       +#10+'function TfGetCurrentById(id : Integer) : LongInt;stdcall;'
       +#10+'function TfGetCurrent(Position : pchar) : LongInt;stdcall;'
       +#10+'function TfGetPowerById(id : Integer) : LongInt;stdcall;'
       +#10+'function TfGetPower(Position : pchar) : LongInt;stdcall;'

       +#10+'function TfGetColor(Position : pchar) : Cardinal;stdcall;'
       +#10+'function TfGetColorById(id : Integer) : Cardinal;stdcall;'

       +#10+'function TfSetRelais(Position : pchar;Relais : Integer;SwitchOn : Boolean) : Boolean;stdcall;'

            ;
end;

exports
  TfConnect,
  TfEnumerate,
  TfDisconnect,
  TfGetDeviceList,

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

  TfGetColor,
  TfGetColorById,

  TfSetRelais,

  ScriptCleanup,
  ScriptTool,
  ScriptDefinition;

initialization
  Station:=nil;
end.
