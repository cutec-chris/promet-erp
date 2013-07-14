unit umodem;

{$mode objfpc}{$H+}

interface

uses
  Forms, Classes, SysUtils, uComPort, LCLIntf,Dialogs;
  
type
  TSpeakerOnType = (soNever,soTillConnection,soEver);

  { TModem }

  TModem = class
  private
    FATTermination: char;
    FActive: Boolean;
    FATTimeout: Integer;
    FCallParameters: string;
    FCloseUpString: string;
    FInitString: string;
    FPort: TComPort;
    FResetString: string;
    FBuffer : string;
    FSpeakerOffSeting: string;
    FSpeakerOffString: string;
    FSpeakerOnString: string;
    FSpeakerOnType: TSpeakerOnType;
    FSpeakerTillConnectionString: string;
    FTermination: char;
    procedure SetActive(const AValue: Boolean);
    procedure FPortCharRecived(Port: TComport; c: char);
    procedure SetSpeakerOn(const AValue: TSpeakerOnType);
    procedure SetTermination(const AValue: char);
  public
    constructor Create(PortName : string);
    destructor Destroy;override;
    property Port : TComPort read FPort write FPort;
    property Active : Boolean read FActive write SetActive;
    procedure Open;
    procedure Close;
    function Reset : Boolean;
    function ShortATCommand(Cmd : string) : Boolean;
    function ATCommand(Cmd : string) : string;
    property SpeakerOn : TSpeakerOnType read FSpeakerOnType write SetSpeakerOn;
    function Connect(Number : string) : Boolean;
    property CallParameters : string read FCallParameters write fCallParameters;
    function SendString(data : string) : Boolean;
    function CheckLineRecived(var Line : string) : Boolean;
    function CheckCharRecived(var c : char) : Boolean;
    property LineTerminator : char read FTermination write SetTermination;
    function ReciveLine(Timeout : Integer) : string;
    procedure Disconnect;
    property ATTimeout : Integer read FATTimeout write FATTimeout;
    property ATInitString : string read FInitString write FInitString;
    property ATResetString : string read FResetString write FResetString;
    property ATTerminationChar : char read FATTermination write FATTermination;
    property ATCloseUpString : string read FCloseUpString write fCloseUpString;
    property ATSpeakerOnString : string read FSpeakerOnString write FSpeakerOnString;
    property ATSpeakerOffString : string read FSpeakerOffString write FSpeakerOffSeting;
    property ATSpeakerTillConnectionString : string read FSpeakerTillConnectionString write FSpeakerTillConnectionString;
  end;

implementation

{ TModem }

procedure TModem.FPortCharRecived(Port: TComport; c: char);
begin
  FBuffer := FBuffer+c;
end;

procedure TModem.SetSpeakerOn(const AValue: TSpeakerOnType);
begin
  if FSpeakerOnType=AValue then exit;
  FSpeakerOnType:=AValue;
  case AValue of
  soNever:ShortATCommand(ATSpeakerOffString);
  soTillConnection:ShortATCommand(ATSpeakerTillConnectionString);
  soEver:ShortATCommand(ATSpeakerOnString);
  end;
end;

procedure TModem.SetTermination(const AValue: char);
begin
  if FTermination=AValue then exit;
  FTermination:=AValue;
  FPort.LineTerminator := AValue;
end;

procedure TModem.SetActive(const AValue: Boolean);
begin
  if FActive=AValue then exit;
  FActive:=AValue;
  if AValue then
    Open
  else
    Close;
end;

constructor TModem.Create(PortName: string);
begin
  FPort := TComPort.Create(PortName);
  FPort.OnCharRecived :=@FPortCharRecived;
  FATTimeout := 1500;
  
  FInitString := 'AT';
  FResetString := 'AT&F';
  FATTermination := #13;
  FCloseUpString := '~~~+++~~~ATH0';
  FSpeakerOnString := 'ATM2';
  FSpeakerOffString := 'ATM0';
  FSpeakerTillConnectionString := 'ATM1';
end;

destructor TModem.Destroy;
begin
  if Assigned(FPort) then
    FPort.Free;
end;

procedure TModem.Open;
begin
  FActive := False;
  if not FPort.Active then
    begin
      FPort.Open;
      if not FPort.Active then exit;
    end;
  FActive := True;
{  if ShortATCommand(FInitString) then
    FActive := True
  else
    FPort.Close;}
end;

procedure TModem.Close;
begin
  FPort.Close;
end;

function TModem.Reset: Boolean;
begin
  Result := ShortATCommand(ATResetString);
end;

function TModem.ShortATCommand(Cmd: string): Boolean;
var
  atm: Int64;
  Recived : string;
begin
  Fbuffer := '';
  Result := false;
  if not FPort.SendString(cmd+#13) then
    begin
      Result := False;
      exit;
    end;
  atm := GetTickCount;
  while (FATTimeOut) > (GetTickCount - atm) do
    begin
      while pos(FATTermination,FBuffer) > 0 do
        begin
          Recived := StringReplace(copy(FBuffer,0,pos(FATTermination,FBuffer)-1),#10,'',[rfReplaceAll]);
          FBuffer := copy(FBuffer,pos(FATTermination,FBuffer)+1,length(Fbuffer));
          if Recived = 'OK' then
            begin
              Result := true;
              exit;
            end
          else if Recived = 'ERROR' then
            exit;
        end;
      Application.Processmessages;
    end;
  while pos(FATTermination,FBuffer) > 0 do
    begin
      Recived := StringReplace(copy(FBuffer,0,pos(FATTermination,FBuffer)-1),#10,'',[rfReplaceAll]);
      FBuffer := copy(FBuffer,pos(FATTermination,FBuffer)+1,length(Fbuffer));
      if Recived = 'OK' then
        begin
          Result := true;
          exit;
        end
      else if Recived = 'ERROR' then
        exit;
    end;
end;

function TModem.ATCommand(Cmd: string): string;
var
  atm: Int64;
  Recived : string;
begin
  FBuffer := '';
  Result := '';
  FPort.ClearBuffer;
  FPort.SendString(cmd+#13);
  atm := GetTickCount;
  while (FATTimeOut) > (GetTickCount - atm) do
    begin
      while pos(FATTermination,FBuffer) > 0 do
        begin
          Recived := StringReplace(StringReplace(copy(FBuffer,0,pos(FATTermination,FBuffer)-1),#10,'',[rfReplaceAll]),#0,'',[rfReplaceAll]);
          FBuffer := copy(FBuffer,pos(FATTermination,FBuffer)+1,length(Fbuffer));
          if (Recived <> Cmd) and (Recived <> '') then
            begin
              Result := Recived;
              exit;
            end
        end;
      Application.Processmessages;
      sleep(100);
    end;
  if Result = '' then
    if pos(FATTermination,FBuffer) > 0 then
      begin
        Recived := StringReplace(copy(FBuffer,0,pos(FATTermination,FBuffer)-1),#10,'',[rfReplaceAll]);
        FBuffer := copy(FBuffer,pos(FATTermination,FBuffer)+1,length(Fbuffer));
        if (Recived <> Cmd) and (Recived <> '') then
          begin
            Result := Recived;
            exit;
          end
      end;
end;

function TModem.Connect(Number: string): Boolean;
var
  atm: Int64;
  Recived : string;
begin
  Result := false;
  FPort.SendString('ATDT'+FCallParameters+Number+#13);
  atm := GetTickCount;
  while (FATTimeOut*90) > (GetTickCount - atm) do
    begin
      while pos(FATTermination,FBuffer) > 0 do
        begin
          Recived := StringReplace(copy(FBuffer,0,pos(FATTermination,FBuffer)-1),#10,'',[rfReplaceAll]);
          FBuffer := copy(FBuffer,pos(FATTermination,FBuffer)+1,length(Fbuffer));
          if pos('CONNECT',Recived) > 0 then
            begin
              Result := true;
              exit;
            end
          else if Recived = 'ERROR' then
            exit
          else if Recived = 'NO CARRIER' then
            exit
          else if Recived = 'BUISY' then
            exit
          else if Recived = 'NO DIALTONE' then
            exit;
        end;
      Application.Processmessages;
    end;
end;

function TModem.SendString(data: string) : Boolean;
begin
  Result := FPort.SendString(data);
end;

function TModem.CheckLineRecived(var Line: string): Boolean;
begin
  Result := FPort.CheckLineRecived(Line);
end;

function TModem.CheckCharRecived(var c: char): Boolean;
begin
  Result := FPort.CheckCharRecived(c);
end;

function TModem.ReciveLine(Timeout: Integer): string;
begin
  Result := '';
  while (Result = '') and (length(FBuffer) > 0) do
    begin
      if pos(#10,FBuffer) > 0 then
        begin
          Result := copy(FBuffer,0,pos(#10,FBuffer)-1);
          FBuffer := copy(FBuffer,pos(#10,FBuffer)+1,length(FBuffer));
        end
       else
         begin
           Result := FBuffer;
           FBuffer := '';
         end;
    end;
  if Result = '' then
    Result := FPort.RecivLine(Timeout);
end;

procedure TModem.Disconnect;
begin
  ShortATCommand(FCloseUpString);
end;

end.

