unit uPowerState;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils
  {$IFDEF WINDOWS}
  ,windows
  {$ENDIF}
  {$IFDEF UNIX}
  {$ifndef DARWIN}
  ,dbus
  {$endif}
  {$ENDIF}
  ;
{$IFDEF UNIX}
{$ifndef DARWIN}
  {$DEFINE DBUS}
{$endif}
{$ENDIF}

type
  TPowerStateMonitor = class(TObject)
  private
    FOnResume: TNotifyEvent;
    FOnStandby: TNotifyEvent;
    {$IFDEF DBUS}
    err: DBusError;
    conn: PDBusConnection;
    vTable: DBusObjectPathVTable;
    {$ENDIF}
    procedure Messagereceived(aMessage : string);
  public
    constructor Create;
    destructor Destroy;override;
  published
    property OnStandby : TNotifyEvent read FOnStandby write FOnStandby;
    property OnResume : TNotifyEvent read FOnResume write FOnResume;
  end;

implementation
uses uBaseApplication;
var
  aPowerStateMonitor : TPowerStateMonitor;
{$IFDEF DBUS}
function VTableFunc(connection: PDBusConnection; message_: PDBusMessage;
  user_data: Pointer): DBusHandlerResult; cdecl;
var
  rply: PChar='';
  args: DBusMessageIter;
begin
  with BaseApplication as IBaseApplication do
    begin
      Debug('VTableFunc!');
      // read the parameters
      if (dbus_message_iter_init(message_, @args) = 0) then
         Debug('Get:Message has no arguments!')
      else if (DBUS_TYPE_STRING <> dbus_message_iter_get_arg_type(@args)) then
         Debug('Get:Argument is not string!')
      else
         begin
           dbus_message_iter_get_basic(@args, @rply);
           aPowerStateMonitor.MessageReceived(rply);
         end;
      Result := DBUS_HANDLER_RESULT_HANDLED;
    end;
end;
procedure VTableUnregister(connection: PDBusConnection; user_data: Pointer); cdecl;
begin
 with BaseApplication as IBaseApplication do
   debug('VTableUnregister!')
end;
{$ENDIF}
procedure TPowerStateMonitor.Messagereceived(aMessage: string);
begin
  //debugln(aMessage);
end;
constructor TPowerStateMonitor.Create;
begin
 with BaseApplication as IBaseApplication do
   begin
    aPowerStateMonitor := Self;
    {$IFDEF DBUS}
     { Initializes the errors }
     dbus_error_init(@err);
     { Connection }
     conn := dbus_bus_get(DBUS_BUS_SYSTEM, @err);
     if dbus_error_is_set(@err) <> 0 then
     begin
       //error
       dbus_error_free(@err);
     end;
     if conn = nil then Exit;
     dbus_bus_add_match(conn,PChar('type=''signal'',interface=''org.freedesktop.UPower'''), @err);
     if dbus_error_is_set(@err) <> 0 then
     begin
       //error
       dbus_error_free(@err);
     end;
     if dbus_connection_add_filter (conn, @VTableFunc, nil, nil) = 0 then
       debug('register upower conn failed');

  //   vTable.message_function := @VTableFunc;
  //   vTable.unregister_function := @VTableUnregister;
  //   if dbus_connection_register_object_path(conn,'/org/freedesktop/UPower',@vTable,nil)=0 then
  //     debugln('register upower conn failed');
   {$ENDIF}
   end;
end;

destructor TPowerStateMonitor.Destroy;
begin
  {$IFDEF DBUS}
   dbus.dbus_connection_close(conn);
 {$ENDIF}
  inherited Destroy;
end;

end.

