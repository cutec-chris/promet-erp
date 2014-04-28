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
   {$IFDEF LINUX}
   {$DEFINE DBUS}
   dbus, ctypes,
   {$ENDIF}
 {$ENDIF}
 SysUtils, Classes, Graphics, Controls, Dialogs;

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
   {$IFDEF WINDOWS}
   WM_SkypeControlAPIDiscover: LongWord;
   WM_SkypeControlAPIAttach: LongWord;
   HWND_SkypeAPIWindowHandle: LongInt;
   {$ENDIF}
   {$IFDEF DBUS}
   err: DBusError;
   conn: PDBusConnection;
   vTable: DBusObjectPathVTable;
   {$ENDIF}
   FParent : TWinControl;
   FConnected: Boolean;
 public
   destructor Destroy; override;
   function Initiate : Boolean;
   procedure Command(cmd : string);
 published
   constructor Create (AOwner : TWinControl);
   procedure Connect;
   property OnAPIAttach : TAttachEvent read fOnAPIAttach write fOnAPIAttach;
   property OnAnswer : TAnswerEvent read fOnAnswer write fOnAnswer;
 end;

var
  aSkype : TSkypeMessagehandler;

implementation

{$IFDEF DBUS}
function VTableFunc(connection: PDBusConnection; message_: PDBusMessage;
  user_data: Pointer): DBusHandlerResult; cdecl;
var
  rply: PChar='';
  args: DBusMessageIter;
begin
  // read the parameters
  if (dbus_message_iter_init(message_, @args) = 0) then
//     Writeln('Get:Message has no arguments!')
  else if (DBUS_TYPE_STRING <> dbus_message_iter_get_arg_type(@args)) then
//     Writeln('Get:Argument is not string!')
  else
     begin
       dbus_message_iter_get_basic(@args, @rply);
       aSkype.fOnAnswer(aSkype,rply);
     end;
  Result := DBUS_HANDLER_RESULT_HANDLED;
end;
procedure VTableUnregister(connection: PDBusConnection; user_data: Pointer); cdecl;
begin
end;
{$ENDIF}
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
  aSkype := Self;
  FConnected := False;
end;

procedure TSkypeMessageHandler.Connect;
begin
{$IFDEF WINDOWS}
  Try
    WM_SkypeControlAPIDiscover := RegisterWindowMessage('SkypeControlAPIDiscover');
    WM_SkypeControlAPIAttach := RegisterWindowMessage('SkypeControlAPIAttach');
  Except
    WM_SkypeControlAPIDiscover := 0;
    WM_SkypeControlAPIAttach:= 0;
  End;
  FConnected := True;
{$ENDIF}
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
 aSkype := Self;
 if not FConnected then Connect;
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
 {$IFDEF DBUS}
  { Initializes the errors }
  dbus_error_init(@err);
  { Connection }
  conn := dbus_bus_get(DBUS_BUS_SESSION, @err);
  if dbus_error_is_set(@err) <> 0 then
  begin
    //error
    dbus_error_free(@err);
  end;
  if conn = nil then Exit;
  { Registering callback funtion for skype signals (but they are methods really) }
  vTable.message_function := @VTableFunc;
  vTable.unregister_function := @VTableUnregister;
  if dbus_connection_register_object_path(conn,'/com/Skype/Client',@vTable,nil)=0 then
    ;//Register Error
  if Assigned(fOnAPIAttach) then
    fOnAPIAttach(aSkype,0);
  Command('NAME '+'Promet-ERP')
{$ENDIF}
end;

procedure TSkypeMessageHandler.Command(cmd: string);
{$IFDEF WINDOWS}
var
  CopyData: CopyDataStruct;
{$ENDIF}
{$IFDEF DBUS}
var
  msg: PDBusMessage;
  args: DBusMessageIter;
  pending: PDBusPendingCall;
  stat: Boolean;
  level: dbus_uint32_t;
  rply: PChar='';
  i: Integer;
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
{$IFDEF DBUS}
  // create a new method call and check for errors
  msg := dbus_message_new_method_call('com.Skype.API', // target for the method call
                                      '/com/Skype', // object to call on
                                      'com.Skype.API', // interface to call on
                                      'Invoke'); // method name
  if (msg = nil) then
  begin
//    Writeln('Message Null');
    Exit;
  end;

  // append arguments
  dbus_message_iter_init_append(msg, @args);
  if (dbus_message_iter_append_basic(@args, DBUS_TYPE_STRING, @Cmd) = 0) then
  begin
//    Writeln('Out Of Memory!');
    Exit;
  end;

  // send message and get a handle for a reply
  if (dbus_connection_send_with_reply(conn, msg, @pending, -1) = 0) then // -1 is default timeout
  begin
//    Writeln('Out Of Memory!');
    Exit;
  end;
  if (pending = nil) then
  begin
//    Writeln('Pending Call Null');
    Exit;
  end;
  dbus_connection_flush(conn);

//  Writeln('Request Sent');

  // free message
  dbus_message_unref(msg);

  // block until we recieve a reply
  dbus_pending_call_block(pending);

  // get the reply message
  msg := dbus_pending_call_steal_reply(pending);
  if (msg = nil) then
  begin
//    Writeln('Reply Null');
    Exit;
  end;
  // free the pending message handle
  dbus_pending_call_unref(pending);

  // read the parameters
  if (dbus_message_iter_init(msg, @args) = 0) then
//     Writeln('Message has no arguments!')
  else if (DBUS_TYPE_STRING <> dbus_message_iter_get_arg_type(@args)) then
//     Writeln('Argument is not string!')
  else
     begin
       dbus_message_iter_get_basic(@args, @rply);
       aSkype.fOnAnswer(aSkype,rply);
     end;
  dbus_message_unref(msg);
{$ENDIF}
end;


end.
