//CALL 232 STATUS RINGING
//CALL 232 STATUS INPROGRESS
//CALL 232 STATUS FINISHED


unit uSkype;

{$mode objfpc}{$H+}

interface

uses
 {$IFDEF WINDOWS}
 Windows,
 {$ELSE}
// dbus,
 {$ENDIF}
 SysUtils, Classes, Graphics, Controls, Forms, Dialogs;

type
 TAttachEvent = procedure (Sender: TObject; APIAttached : Integer)of object;
 TAnswerEvent = procedure (Sender: TObject; Answer : String) of object;

 { TSkypeMessageHandler }

 TSkypeMessageHandler = class(TWinControl)
 private
   fOnAPIAttach : TAttachEvent;
   fOnAnswer : TAnswerEvent;
   {$IFDEF WINDOWS}
   OldProc : Windows.WNDPROC;
   {$ENDIF}
 protected
    WM_SkypeControlAPIDiscover: LongWord;
    WM_SkypeControlAPIAttach: LongWord;
    HWND_SkypeAPIWindowHandle: LongInt;
    FParent : TWinControl;
 public
   destructor Destroy; override;
   function Initiate : Boolean;
   procedure Command(cmd : string);
 published
   constructor Create (AOwner : TWinControl);
   property OnAPIAttach : TAttachEvent read fOnAPIAttach write fOnAPIAttach;
   property OnAnswer : TAnswerEvent read fOnAnswer write fOnAnswer;
 end;

var
  aSkype : TSkypeMessagehandler;

implementation

{$IFDEF WINDOWS}
function HeaderProc(wnd: HWND; Msg: Cardinal; wParam: wParam; lParam: lParam): Longint;stdcall;
var
  hti: THDHitTestInfo;
  data : PCopyDataStruct;
begin
  if Msg = aSkype.WM_SkypeControlAPIAttach then
    begin
      if LParam = 0 then
        aSkype.HWND_SkypeAPIWindowHandle := WParam;
      if Assigned(aSkype.fOnAPIAttach) then
        aSkype.fOnAPIAttach(aSkype,wParam);
      Result := 1;
    end
  else if (aSkype.HWND_SkypeAPIWindowHandle > 0) and (Msg = WM_COPYDATA) and (wParam = aSkype.HWND_SkypeAPIWindowHandle) then
    begin
      Data := PCopyDataStruct(lParam);
      if Assigned(aSkype.fOnAnswer) then
        aSkype.fOnAnswer(aSkype,PChar(Data^.lpData));
      Result := 1
    end
  else
    Result := CallWindowProc(aSkype.OldProc, wnd, Msg, wParam, lParam);
end;
{$ENDIF}

constructor TSkypeMessageHandler.Create (AOwner : TWinControl);
begin
  inherited Create (AOwner);
  FParent := AOwner;
{$IFDEF WINDOWS}
  Try
    WM_SkypeControlAPIDiscover := RegisterWindowMessage('SkypeControlAPIDiscover');
    WM_SkypeControlAPIAttach := RegisterWindowMessage('SkypeControlAPIAttach');
  Except
    WM_SkypeControlAPIDiscover := 0;
    WM_SkypeControlAPIAttach:= 0;
  End;
{$ENDIF}
  aSkype := Self;
end;

destructor TSkypeMessageHandler.Destroy;
begin
{$IFDEF WINDOWS}
 WM_SkypeControlAPIDiscover := 0;
 if LONG(OldProc) <> 0 then
   SetWindowLong(FParent.Handle, GWL_WNDPROC, LONG(OldProc));
{$ENDIF}
 inherited
end;

function TSkypeMessageHandler.Initiate: Boolean;
var
  dwBSMRecipients: DWORD;
begin
{$IFDEF WINDOWS}
 dwBSMRecipients := BSM_APPLICATIONS;
 Try
   If WM_SkypeControlAPIDiscover <> 0 then
   begin
    {$IFDEF CPUI386}
    LONG(OldProc) := SetWindowLong(FParent.Handle, GWL_WNDPROC, Integer(@HeaderProc));
    {$ELSE}
    Int64(OldProc) := SetWindowLong(FParent.Handle, GWL_WNDPROC, Integer(@HeaderProc));
    {$ENDIF}
    dwBSMRecipients := BSM_APPLICATIONS;
    BroadcastSystemMessage((BSF_FORCEIFHUNG Or BSF_IGNORECURRENTTASK Or BSF_POSTMESSAGE),@dwBSMRecipients, WM_SkypeControlAPIDiscover,FParent.Handle, 0);
   end;
 except
   Result := False;
 end;
{$ENDIF}
end;

procedure TSkypeMessageHandler.Command(cmd: string);
{$IFDEF WINDOWS}
var
  CopyData: CopyDataStruct;
{$ENDIF}
begin
{$IFDEF WINDOWS}
  if CMD <> '' then
  begin
    CopyData.dwData := 0;
    CopyData.lpData := PChar(CMD);
    CopyData.cbData := Length(CMD)+1;
    Windows.SendMessage(HWND_SkypeAPIWindowHandle, WM_COPYDATA, FParent.Handle, LPARAM(@CopyData))
  end
{$ENDIF}
end;


end.
