unit gtkdragdrop;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,LMessages,gtk,gdk,glib,Controls,Dialogs;
  
  procedure DragAcceptFiles(handle :THandle;Enabled : Boolean);
  function  DragQueryFile(WParam : LongInt;i : Longint;acFileName : PChar;cnMaxFileNameLen : Integer) : Integer;
  procedure DragFinish(WParam : Longint);

implementation

var
  Targets: array[0..2] of TGtktargetEntry = ((target:'text/plain';flags:0;info:0),
                                             (target:'text/uri-list';flags:0;info:1),
                                             (target:'STRING';flags:0;info:1));

function DropCallback(widget : PGtkWidget; dc : PGdkDragContext;x,y : gint;selection:PGtkSelectionData;info,t:guint;data : gpointer) : GBoolean;cdecl;
var
  list : PGList;
  Message : TLMessage;
begin
  Showmessage('DropCallback');
  if (Data <> nil) and (selection <> nil) and (selection^.length >= 0) then
    begin
      list := dc^.targets;
      while list <> nil do
        begin
          if (selection^.data  <> nil) and (StrPos(PChar(selection^.data),'file://') = PChar(selection^.data)) then
            begin
              Message.Msg := LM_DROPFILES;
              Message.wParam := LongInt(selection^.data);
              TWinControl(data).Dispatch(Message);
              exit;
            end;
          list := list^.next;
        end;
    end;
end;

procedure DragAcceptFiles(handle :THandle;Enabled : Boolean);
begin
  if Enabled then
    begin
      gtk_drag_dest_set(PGtkWidget(Handle),GTK_DEST_DEFAULT_ALL,@Targets[0],length(Targets),GDK_ACTION_COPY);
      gtk_signal_connect(PGtkObject(Handle),'drag_data_recived',TGTKSignalFunc(@DropCallback),gpointer(FindOwnerControl(Handle)));
    end
  else
    gtk_drag_dest_unset(PGtkWidget(Handle));
end;

function  DragQueryFile(WParam : LongInt;i : Longint;acFileName : PChar;cnMaxFileNameLen : Integer) : Integer;
var
  SL : TStringList;
begin
  SL := TStringList.Create;
  try
    SL.Text := PChar(wParam);
  except
  end;
  if i >= Sl.Count then
    Result := SL.Count
  else
    begin
{      if cnMaxFileNameLen > length(SL[i]) then
        Move(SL[i],acFileName,length(SL[i]))
      else
        Move(SL[i],acFileName,cnMaxFileNameLen);}
    end;
end;

procedure DragFinish(WParam : Longint);
begin
end;


end.

