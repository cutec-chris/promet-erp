{*******************************************************************************
  Copyright (C) Christian Ulrich info@cu-tec.de

  This source is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 2 of the License, or commercial alternative
  contact us for more information

  This code is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
  to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
  MA 02111-1307, USA.
*******************************************************************************}
unit uSystemMessage;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, uBaseDbInterface, uBaseDbClasses, db, uBaseApplication,
  Variants;
type
  TSystemCommands = class(TBaseDBDataSet)
  public
    procedure DefineFields(aDataSet : TDataSet);override;
  end;
  TSystemMessages = class(TBaseDBDataSet)
  public
    procedure DefineFields(aDataSet : TDataSet);override;
  end;
  TSystemMessageEvent = function(Sender : TObject;aMessage : string) : Boolean of Object;
  TSystemCommandEvent = function(Sender : TObject;aCommand : string) : Boolean of Object;
  TMessageHandler = class(TThread)
  private
    Data : TBaseDBModule;
    Connection : TComponent;
    aSleepTime : Integer;
    ExceptMessage : string;
    CommandHandlers : array of TSystemCommandEvent;
    FExit: TNotifyEvent;
    MessageHandlers : array of TSystemMessageEvent;
    SysCommands: TSystemCommands;
    SysMessages: TSystemMessages;
    procedure CommandThere;
    procedure MessageThere;
    procedure ShowException;
    procedure DoTerminate;
  public
    constructor Create(aData : TBaseDBModule);
    procedure Execute; override;
    property OnExit : TNotifyEvent read FExit write FExit;
    procedure RegisterCommandHandler(CommandHandler : TSystemCommandEvent);
    function SendCommand(Target,Command : string) : Boolean;
  end;
implementation
procedure TSystemMessages.DefineFields(aDataSet: TDataSet);
begin
  with aDataSet as IBaseManageDB do
    begin
      TableName := 'SYSTEMMESSAGES';
      if Assigned(ManagedFieldDefs) then
        with ManagedFieldDefs do
          begin
            Add('AUTO_ID',ftLargeint,0,True);
            Add('PROCESS_ID',ftLargeInt,0,True);
            Add('COMMAND_ID',ftLargeInt,0,True);
            Add('MESSAGE',ftMemo,0,False);
          end;
    end;
end;
procedure TSystemCommands.DefineFields(aDataSet: TDataSet);
begin
  with aDataSet as IBaseManageDB do
    begin
      TableName := 'SYSTEMCOMMANDS';
      if Assigned(ManagedFieldDefs) then
        with ManagedFieldDefs do
          begin
            Add('AUTO_ID',ftLargeint,0,True);
            Add('PROCESS_ID',ftLargeInt,0,True);
            Add('COMMAND',ftMemo,0,False);
          end;
    end;
end;
procedure TMessageHandler.CommandThere;
var
  Found: Boolean;
  i: Integer;
begin
  Found := False;
  for i := 0 to length(CommandHandlers)-1 do
    if CommandHandlers[i](Self,SysCommands.FieldByName('COMMAND').AsString) then
      begin
        Found := True;
        SysCommands.DataSet.Delete;
        break;
      end;
  if not Found then
    SysCommands.DataSet.Next;
end;
procedure TMessageHandler.MessageThere;
begin

end;
procedure TMessageHandler.ShowException;
begin
end;
procedure TMessageHandler.DoTerminate;
var
  i: Integer;
begin
  Data.IgnoreOpenRequests := True;
  for i := 0 to length(CommandHandlers)-1 do
    if CommandHandlers[i](Self,'ForcedShutdown') then
      break;
end;
constructor TMessageHandler.Create(aData : TBaseDBModule);
begin
  Data := aData;
  Connection := Data.GetNewConnection;
  aSleepTime := 12000;
  SysCommands := TSystemCommands.CreateEx(nil,Data,Connection);
  SysCommands.CreateTable;
  Data.SetFilter(SysCommands,Data.QuoteField('PROCESS_ID')+'='+Data.QuoteValue(IntToStr(Data.SessionID)),5);
  SysMessages := TSystemMessages.CreateEx(nil,Data,Connection);
  SysMessages.CreateTable;
  Data.SetFilter(SysMessages,Data.QuoteField('PROCESS_ID')+'='+Data.QuoteValue(IntToStr(Data.SessionID)),5);
  if not BaseApplication.HasOption('disablethreads') then
    inherited Create(False)
  else
    Execute;
end;

procedure TMessageHandler.Execute;
var
  ResSleepTime: LongInt;
const
  MinsleepTime = 700;
  MaxSleepTime = 30000;
begin
  while not Terminated do
    begin
      if (not Assigned(SysCommands)) or (not Assigned(SysCommands.DataSet)) then break;
      try
        SysCommands.DataSet.Refresh;
      except
        on e : Exception do
          begin
            ExceptMessage := e.Message;
            Synchronize(@ShowException);
            if Assigned(FExit) then
              FExit(Self);
            Synchronize(@DoTerminate);
            break;
          end;
      end;
      if SysCommands.Count > 0 then
        begin
          with SysCommands.DataSet do
            begin
              First;
              while not EOF do
                begin
                  {$IFNDEF LCLnogui}
                  Synchronize(@CommandThere);
                  {$ELSE}
                  CommandThere;
                  {$ENDIF}
                end;
            end;
          aSleepTime := MinSleepTime;
        end
      else if aSleepTime < MaxSleepTime then
        inc(aSleepTime,300);
      ResSleepTime := aSleepTime;
      while ResSleepTime > 0 do
        begin
          if Terminated then break;
          sleep(100);
          dec(ResSleepTime,100);
        end;
    end;
  if Assigned(FExit) then
    FExit(Self);
  FreeAndNil(SysCommands);
  FreeAndNil(SysMessages);
  FreeAndNil(Connection);
end;

procedure TMessageHandler.RegisterCommandHandler(
  CommandHandler: TSystemCommandEvent);
begin
  Setlength(CommandHandlers,length(CommandHandlers)+1);
  Commandhandlers[length(CommandHandlers)-1] := CommandHandler;
end;
function TMessageHandler.SendCommand(Target, Command: string) : Boolean;
var
  SysCmd: TSystemCommands;
  Procs: TActiveUsers;
begin
  if not Assigned(Self) then exit;
  Procs := TActiveUsers.Create(nil);
  SysCmd := TSystemCommands.Create(nil);
  try
    with Procs.DataSet as IBaseDbFilter do
      Data.SetFilter(Procs,Data.ProcessTerm(Data.QuoteField('CLIENT')+'='+Data.QuoteValue(Target)));
    Data.SetFilter(SysCmd,'');
    Procs.DataSet.First;
    while not Procs.DataSet.EOF do
      begin
        if not SysCmd.DataSet.Locate('PROCESS_ID,COMMAND',VarArrayOf([Procs.Id.AsString,Command]),[]) then
          begin
            SysCmd.DataSet.Append;
            SysCmd.FieldByName('PROCESS_ID').AsString:=Procs.FieldByName('SQL_ID').AsString;
            SysCmd.FieldByName('COMMAND').AsString:=Command;
            SysCmd.DataSet.Post;
          end;
        Procs.DataSet.Next;
      end;
  finally
    SysCmd.Free;
    Procs.Free;
  end;
end;
end.

