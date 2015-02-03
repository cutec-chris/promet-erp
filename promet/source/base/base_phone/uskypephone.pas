unit uSkypePhone;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uPhones, uSkype, Forms, Dialogs;
  
type

  { TSkypePhoneLine }

  TSkypePhoneLine = class(TPhoneLine)
    procedure SkypeMessageHandlerAnswer(Sender: TObject; Answer: String);
  private
    SkypeMessageHandler: TSkypeMessageHandler;
  protected
    function GetName: string;override;
    procedure DoAnswer(cID : string);override;
    procedure DoHangup(cID : string);override;
    procedure DoHold(cID : string);override;
    procedure DoContinue(cID : string);override;
  public
    function Connect : Boolean;override;
    constructor Create;
    destructor Destroy;override;
  end;

procedure RegisterPhoneLines;

implementation

uses uBaseApplication,ubaseconfig;

procedure RegisterPhoneLines;
var
  aLine: TSkypePhoneLine;
  aLines: String;
begin
  aLine := TSkypePhoneLine.Create;
  Phones.Add(aLine);
  try
  with Application as IBaseConfig do
    aLines := Config.ReadString('PHONELINES','');
  except
  end;
  if (pos(aLine.Name,aLines)>0) or (not aLine.Connect) then
    begin
      exit;
    end;
end;

{ TSkypePhoneLine }

procedure TSkypePhoneLine.SkypeMessageHandlerAnswer(Sender: TObject;
  Answer: String);
var
  tmp: String;
  cID : string;
  aCall: TCall;
begin
//  fMain.ListBox1.Items.Add(Answer);
  if copy(Answer,0,5) = 'CALL ' then
    begin
      tmp := copy(Answer,6,length(Answer));
      cID := copy(tmp,0,pos(' ',tmp));
      aCall := Calls[cID];
      if not Assigned(aCall) then
        begin
          aCall := TCall.Create(cID);
          aCall.Parent := Self;
          Add(aCall);
          SkypeMessageHandler.Command('GET CALL '+cID+' PARTNER_HANDLE');
          if Assigned(OnCall) then
            OnCall(Self,aCall);
        end;
      tmp := copy(tmp,pos(' ',tmp)+1,length(tmp));
      if copy(tmp,0,7) = 'STATUS ' then
        begin
          tmp := copy(tmp,8,length(tmp));
          if tmp = 'RINGING' then
            aCall.Status := csRinging
          else if tmp = 'INPROGRESS' then
            aCall.Status := csInprogress
          else if tmp = 'FINISHED' then
            aCall.Status := csFinished
          else if tmp = 'ROUTING' then
            aCall.Status := csRouting
          else if tmp = 'FAILED' then
            aCall.Status := csFailed
          else if tmp = 'HOLD' then
            aCall.Status := csHold
          else if tmp = 'REFUSED' then
            aCall.Status := csRefused
          else if tmp = 'BUSY' then
            aCall.Status := csBusy
          else if tmp = 'MISSED' then
            aCall.Status := csFinished;
        end
      else if copy(tmp,0,15) = 'PARTNER_HANDLE ' then
        begin
          tmp := copy(tmp,16,length(tmp));
          aCall.From:=tmp;
          aCall.Status := aCall.Status;
        end;
    end;
end;

function TSkypePhoneLine.GetName: string;
begin
  Result:='Skype';
end;

procedure TSkypePhoneLine.DoAnswer(cID: string);
begin
  SkypeMessageHandler.Command('SET CALL '+cID+' STATUS INPROGRESS');
end;

procedure TSkypePhoneLine.DoHangup(cID: string);
begin
  SkypeMessageHandler.Command('SET CALL '+cID+' STATUS FINISHED');
end;

procedure TSkypePhoneLine.DoHold(cID: string);
begin
  SkypeMessageHandler.Command('SET CALL '+cID+' STATUS ONHOLD');
end;

procedure TSkypePhoneLine.DoContinue(cID: string);
begin
  SkypeMessageHandler.Command('SET CALL '+cID+' STATUS INPROGRESS');
end;

function TSkypePhoneLine.Connect: Boolean;
begin
  Result := SkypeMessageHandler.Initiate;
end;

constructor TSkypePhoneLine.Create;
begin
  inherited Create;
  SkypeMessageHandler := TSkypeMessageHandler.Create(Application.MainForm);
  SkypeMessageHandler.OnAnswer:=@SkypeMessageHandlerAnswer;
end;

destructor TSkypePhoneLine.Destroy;
begin
  SkypeMessageHandler.Free;
  inherited Destroy;
end;

end.

