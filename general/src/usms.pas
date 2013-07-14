unit usms;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,Utils,uModem,uGeneralStrConsts;
  
type

  { TPhoneNumber }

  TPhoneNumber = class(TComponent)
  private
    FNID: Integer;
    FNumber: string;
    Fpreselection: string;
  public
    constructor Create(Number : string);
    property Preselection : string read Fpreselection;
    property Number : string read FNumber;
    property NID : Integer read FNID;
    function BuildNumber : string;
    function BuildModemCompatibleNumber : string;
  end;
  
  TSMSGateway = class;
  
  TSmsMTI = (mtiDeliver, mtiDeliverRep, mtiStatusRep, mtiCommand,mtiSubmit, mtiSubmitRep);

  { TSMS }

  TSMS = class
  private
    FDCS: byte;
    FMTI: TSmsMTI;
    FNotification: TPhoneNumber;
    FPID: byte;
    FReciver: TPhoneNumber;
    FReference: byte;
    FRejDuplicates: Boolean;
    FSender: TPhoneNumber;
    FText: string;
    FTimeStamp: TDateTime;
  public
    constructor Create(ATarget : TPhoneNumber;ASender : TPhoneNumber;AText : string);
    property Text : string read FText write Ftext;
    property Sender : TPhoneNumber read FSender;
    property Reciver : TPhoneNumber read FReciver write FReciver;
    property Notification : TPhoneNumber read FNotification write FNotification;
    property MessageTyp : TSmsMTI read FMTI write FMTI;
    property RejectDuplicates : Boolean read FRejDuplicates write FRejDuplicates;
    property Reference : byte read FReference write FReference;
    property ProtocolID : byte read FPID write FPID;
    property DataCodingSheme : byte read FDCS write FDCS;
    property TimeStamp : TDateTime read FTimeStamp write FTimeStamp;
    function Send(Gateway : TSMSGateway) : Boolean;
  end;
  
  TStatusEvent = procedure(Status : string) of Object;
  
  { TSMSGateway }

  TSMSGateway = class
  private
    FOnLog: TStatusEvent;
    FOnStatus: TStatusEvent;
  public
    function Connect : Boolean;virtual;abstract;
    procedure Disconnect;virtual;abstract;
    function SendSMS(SMS : TSMS) : Boolean;virtual;abstract;
    property OnStatus : TStatusEvent read FOnStatus write FonStatus;
    property OnLog : TStatusEvent read FOnLog write FonLog;
  end;
  
  { TUCPGateway }

  TUCPGateway = class(TSMSGateway)
  private
    FSMSC: TPhoneNumber;
    FModem:TModem;
    FWParam: string;
    function BuildSMS(SMS : TSMS) : string;
  public
    property Modem : TModem read FModem write FModem;
    property SMSC : TPhoneNumber read FSMSC write FSMSC;
    function Connect : Boolean;override;
    procedure Disconnect;override;
    function SendSMS(SMS : TSMS) : Boolean;override;
  end;

  { TTAPGateway }

  TTAPGateway = class(TSMSGateway)
  private
    FFormat: string;
    FPassword: string;
    FSMSC: TPhoneNumber;
    Fmodem : TModem;
    function BuildSMS(SMS : TSMS) : string;
  public
    constructor Create;
    property ParameterFormat : string read FFormat write FFormat;
    property Modem : TModem read FModem write FModem;
    property Password : string read FPassword write FPassword;
    property SMSC : TPhoneNumber read FSMSC write FSMSC;
    function Connect : Boolean;override;
    procedure Disconnect;override;
    function SendSMS(SMS : TSMS) : Boolean;override;
  end;

  { TPDUGateway }

  TPDUGateway = class(TSMSGateway)
  private
    FModem : TModem;
    function BuildSMS(SMS : TSMS) : string;
  public
    property Modem : TModem read FModem write FModem;
    function Connect : Boolean;override;
    procedure Disconnect;override;
    function SendSMS(SMS : TSMS) : Boolean;override;
  end;

implementation

uses PduConv;

{ TPhoneNumber }

constructor TPhoneNumber.Create(Number: string);
var
  TmpNumber : string;
begin
  if copy(trim(Number),0,1) = '+' then
    begin
      FNID := StrToInt(copy(trim(Number),2,2));
      TmpNumber := trim(copy(trim(Number),4,length(Number)));
    end
  else
    begin
      FNID := 49;
      if copy(Number,0,1) = '0' then
        TmpNumber := copy(trim(Number),2,length(Number))
      else
        TmpNumber := trim(Number);
    end;
  if pos(' ',TmpNumber) > 0 then
    begin
      FPreselection := copy(TmpNumber,0,pos(' ',TmpNumber)-1);
      FNumber := copy(TmpNumber,pos(' ',TmpNumber)+1,length(TmpNumber));
    end
  else
    begin
      FNumber := TmpNumber;
    end;
end;

function TPhoneNumber.BuildNumber: string;
begin
  Result := '+'+IntToStr(FNID)+FPreselection+FNumber;
end;

function TPhoneNumber.BuildModemCompatibleNumber: string;
begin
  if FNID = 49 then
    Result := '0'+FPreselection+FNumber
  else
    Result := '00'+IntToStr(FNID)+FPreselection+FNumber;
end;

{ TSMS }

constructor TSMS.Create(ATarget: TPhoneNumber;ASender : TPhoneNumber;AText: string);
begin
  FReciver := ATarget;
  FSender := ASender;
  FText := AText;
end;

function TSMS.Send(Gateway: TSMSGateway): Boolean;
begin
  Result := false;
  if Assigned(Gateway) then
    Result := Gateway.SendSMS(Self);
end;

{ TUCPGateway }

function TUCPGateway.BuildSMS(SMS: TSMS): string;
var
  chk : Integer;
  i: Integer;
begin
  Result := 'O/';                                //O=A R=Q
  Result := Result+'51/';                        //Formattyp 51
  Result := Result+SMS.Reciver.BuildModemCompatibleNumber+'/';  //Zielrufnummer
  Result := Result+SMS.Sender.BuildModemCompatibleNumber+'/';   //Eigene Rufnummer
  Result := Result+'/';
  if Assigned(SMS.Notification) then
    begin
      Result := Result+'1/';                     //Bestätigung on
      Result := Result+SMS.Notification.BuildModemCompatibleNumber+'/'; //Bestätigung senden an
      Result := result+'/0100////////////';      //Bestätigung an Mobiltelefon
    end
  else
    Result := Result+'///////////////';
  Result := Result+'3//';                        //Format 3
  for i := 1 to length(SMS.Text) do
    Result := Result+IntToHex(byte(SMS.Text[i]),2);
  Result := Result+'/////////////';
  Result := Format('%.2d/%.5d/',[SMS.Reference,length(Result)+11])+Result; //Message ID und Länge
  chk := 0;
  for i := 1 to length(Result) do
    chk := chk+byte(result[i]);
  Result := Result+IntToHex(byte(chk),2);
  Result := Char(2)+Result+Char(3);
end;

function TUCPGateway.Connect: Boolean;
var
  Res: String;
begin
  Result := False;
  if Modem.Active then exit;
  Modem.AtTimeout := 600;
  if Assigned(FOnStatus) then
    FOnStatus(strInitingModem);
  Modem.Active := True;
  if not Modem.Active then
    begin
      if Assigned(FOnStatus) then
        FOnStatus(strNoModem);
      exit;
    end;
  if Assigned(FOnStatus) then
    FOnStatus(strResetingModem);
  if not FModem.Reset then
    begin
      if Assigned(FOnStatus) then
        FOnStatus(strResetModemFail);
      FModem.Active := False;
      exit;
    end;
  FModem.SpeakerOn := soNever;
  if Assigned(FOnStatus) then
    FOnStatus(strConnectingToSMSC+' '+FWParam+FSMSC.BuildModemCompatibleNumber);
  if Assigned(FOnLog) then
    FOnLog(strConnectingToSMSC+' '+FWParam+FSMSC.BuildModemCompatibleNumber);
  if FModem.Connect(FSMSC.BuildModemCompatibleNumber) then
    begin
      Result := True;
      if Assigned(FOnStatus) then
        FOnStatus(strConnectedToSMSC);
    end
  else
    begin
      result := false;
      if Assigned(FOnStatus) then
        FOnStatus(strFailedToConnectToSMSC+' ('+trim(Res)+')');
      if Assigned(FOnLog) then
        FOnLog(strFailedToConnectToSMSC+' ('+trim(Res)+')');
    end;
  if Result = False then
    begin
      FModem.Active := False;
    end;
end;

procedure TUCPGateway.Disconnect;
begin
  if Assigned(FOnStatus) then
    FOnStatus(strDisconnectingFromSMSC);
  if Assigned(FOnLog) then
    FOnLog(strDisconnectingFromSMSC);
  FModem.Disconnect;
  FModem.Close;
end;

function TUCPGateway.SendSMS(SMS: TSMS): Boolean;
var
  Res : string;
begin
  Result := False;
  if not FModem.Active then
    exit;
  FModem.LineTerminator := #3;
  if Assigned(FOnStatus) then
    FOnStatus(strSendingMessage);
  FModem.SendString(BuildSMS(SMS));
  if Assigned(FOnLog) then
    FOnLog(BuildSMS(SMS));
  if Assigned(FOnStatus) then
    FOnStatus(strWaitingForResponse);
  Res := FModem.ReciveLine(60000);
  if (Res <> '') and Assigned(FOnLog) then
    FOnLog(res);
  Result := True;
end;

const
   CR = $D;
   LF = $A;
   ESC= $1B;
   STX= $2;
   ETX= $3;
   US = $1F;
   ETB= $17;
   EOT= $4;
   SUB= $1A;
   ACK= $6;
   NAK= $15;

{ TTAPGateway }

function TTAPGateway.BuildSMS(SMS: TSMS): string;
var
 chk : word;
 i: Integer;
begin
  Result := char(STX)
            +SMS.Reciver.BuildModemCompatibleNumber+char(CR)
            +SMS.Text+char(CR)
            +char(ETX);
  chk := 0;
  for i := 1 to length(Result) do
    chk := chk+(byte(Result[i]) and $7F);
  chk := chk and $FFF;
  Result := Result+IntToStr(chk)+
            char(CR);
end;

constructor TTAPGateway.Create;
begin
  FPassword := 'PG1';
end;

function TTAPGateway.Connect: Boolean;
var
  i: Integer;
  res: String;
begin
  Result := False;
  if Modem.Active then exit;
  Modem.AtTimeout := 600;
  if Assigned(FOnStatus) then
    FOnStatus(strInitingModem);
  Modem.Active := True;
  if not Modem.Active then
    begin
      if Assigned(FOnStatus) then
        FOnStatus(strNoModem);
      exit;
    end;
  if Assigned(FOnStatus) then
    FOnStatus(strResetingModem);
  if not FModem.Reset then
    begin
      if Assigned(FOnStatus) then
        FOnStatus(strResetModemFail);
      FModem.Active := False;
      exit;
    end;
  FModem.SpeakerOn := soNever;
  if Assigned(FOnStatus) then
    FOnStatus(strConnectingToSMSC+' '+FSMSC.BuildModemCompatibleNumber);
  if Assigned(FOnLog) then
    FOnLog(strConnectingToSMSC+' '+FSMSC.BuildModemCompatibleNumber);
  if FModem.Connect(FSMSC.BuildModemCompatibleNumber) then
    begin
      if Assigned(FOnStatus) then
        FOnStatus(strConnectedToSMSC);
      for i := 0 to 2 do
        begin
          FModem.LineTerminator := #13;
          FModem.SendString(#13);
          if Assigned(FOnLog) then
            FOnLog(#13);
          res := FModem.ReciveLine(1000);
          if Assigned(FOnLog) then
            FOnLog(Res);
          if pos('ID=',res) > 0 then
            begin
              Result := True;
              break;
            end;
        end;
    end
  else
    begin
      result := false;
      if Assigned(FOnStatus) then
        FOnStatus(strFailedToConnectToSMSC+' ('+trim(Res)+')');
      if Assigned(FOnLog) then
        FOnLog(strFailedToConnectToSMSC+' ('+trim(Res)+')');
    end;
  if Result then
    begin
      FModem.SendString(char(ESC)+FPassword+char(CR));
      if Assigned(FOnLog) then
        FOnLog(char(ESC)+FPassword+char(CR));
      res := FModem.ReciveLine(1000);
      while res <> '' do
        begin
          if Assigned(FOnLog) then
            FOnLog(res);
          res := FModem.ReciveLine(1000);
        end;
    end;
  if Result = False then
    begin
      FModem.Active := False;
    end;
end;

procedure TTAPGateway.Disconnect;
begin
  if Assigned(FOnStatus) then
    FOnStatus(strDisconnectingFromSMSC);
  if Assigned(FOnLog) then
    FOnLog(strDisconnectingFromSMSC);
  FModem.Disconnect;
  FModem.Close;
end;

function TTAPGateway.SendSMS(SMS: TSMS): Boolean;
var
  Res : string;
begin
  Result := False;
  if not FModem.Active then
    exit;
  FModem.LineTerminator := #3;
  if Assigned(FOnStatus) then
    FOnStatus(strSendingMessage);
  FModem.SendString(BuildSMS(SMS));
  if Assigned(FOnLog) then
    FOnLog(BuildSMS(SMS));
  if Assigned(FOnStatus) then
    FOnStatus(strWaitingForResponse);
  Res := FModem.ReciveLine(60000);
  if (Res <> '') and Assigned(FOnLog) then
    FOnLog(res);
  Result := True;
end;

{ TPDUGateway }

function TPDUGateway.BuildSMS(SMS: TSMS): string;
begin
  Result := EncodePDU(SMS);
end;

function TPDUGateway.Connect: Boolean;
begin
  Result := False;
  if Modem.Active then exit;
  Modem.AtTimeout := 600;
  if Assigned(FOnStatus) then
    FOnStatus(strInitingModem);
  Modem.Active := True;
  if not Modem.Active then
    begin
      if Assigned(FOnStatus) then
        FOnStatus(strNoModem);
      exit;
    end;
  if Assigned(FOnStatus) then
    FOnStatus(strResetingModem);
  if not FModem.Reset then
    begin
      if Assigned(FOnStatus) then
        FOnStatus(strResetModemFail);
      FModem.Active := False;
      exit;
    end;
  Result := True;
end;

procedure TPDUGateway.Disconnect;
begin
  FModem.Active := False;
end;

function TPDUGateway.SendSMS(SMS: TSMS): Boolean;
var
  Res: String;
begin
  FModem.LineTerminator := #13;
  if Assigned(FOnStatus) then
    FOnStatus(strSendingMessage);
  FModem.SendString(BuildSMS(SMS));
  if Assigned(FOnLog) then
    FOnLog(BuildSMS(SMS));
  if FModem.ShortATCommand('AT+CMGF=0') then
    begin
      FModem.SendString('AT+GMGS='+IntToStr(length(BuildSMS(SMS)))+#13);
      Res := FModem.ReciveLine(500);
      if Res = '>' then
        begin
          FModem.SendString(BuildSMS(SMS)+char($1A));
        end
      else
        if Assigned(FOnStatus) then
          FOnStatus(strErrorSendingMessage);
    end
  else
    if Assigned(FOnStatus) then
      FOnStatus(strErrorSendingMessage);
end;

end.

