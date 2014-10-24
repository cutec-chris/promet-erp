library tinkerforge;

{$mode objfpc}{$H+}

uses
  Classes,sysutils, IPConnection, Device, BrickletLCD20x4, BrickletLCD16x2;
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
    procedure Execute;
    property Conn : TIPConnection read ipcon;
  end;

var
  Station : TStation;

function TinkerforgeConnect(Host : PChar;Port : Integer) : Boolean;stdcall;
begin
  if not Assigned(Station) then
    Station := TStation.Create;
  Station.Conn.Connect(Host,Port);
  result := Station.Conn.IsConnected;
end;

function TinkerforgeEnumerate : Integer;stdcall;
begin
  Result :=0;
  Station.Conn.Enumerate;
end;

function ScriptDefinition : PChar;stdcall;
begin
  Result := 'function TinkerforgeEnumerate : Integer;stdcall;'
       +#10+'function TinkerforgeConnect(Host : PChar;Port : Integer) : Boolean;stdcall;'
            ;
end;

exports
  TinkerforgeConnect,
  TinkerforgeEnumerate,
  ScriptDefinition;

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
        TBrickletLCD20x4(Dev).ClearDisplay();
        TBrickletLCD20x4(Dev).BacklightOn();
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

procedure TStation.Execute;
begin

end;

end.
