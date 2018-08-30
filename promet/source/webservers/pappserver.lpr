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
Created 01.06.2010
*******************************************************************************}
program pappserver;
{$mode objfpc}{$H+}
uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils, CustApp, uBaseDBInterface, uData, Process, db,
  uSystemMessage, uPowerState, pcmdprometapp, math, uBaseCustomApplication,
  uprometwikiserver, upwebdavserver, uBaseApplication, Utils,
  uProcessManagement, eventlog,
  uIntfStrConsts, uprometmsgnetwork, uappserverhttp, uappserverpubsub,
  upconfigurationserver, uwebreports,uAppServer;
type
  { TProcessManager }

  TProcessManager = class(TBaseCustomApplication)
    procedure ProcessManagerException(Sender: TObject; E: Exception);
  private
    PowerStateMonitor: TPowerStateMonitor;
  protected
    //Processes : array of TProcProcess;
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;
const
  RefreshAll = 15;//5 mins refresh

procedure OnShowException(Msg: ShortString);
begin
  writeln('Exception:'+Msg);
end;

{ TProcProcess }
procedure TProcessManager.ProcessManagerException(Sender: TObject; E: Exception
  );
begin
  Error(e.Message);
end;
procedure TProcessManager.DoRun;
var
  aTime: TDateTime;
  aSystem: String;
  i, b: Integer;
  Data2: TBaseDBModule;
begin
  {$IFDEF DEBUG}
  DeleteFile('heap.trc');
  SetHeapTraceOutput('heap.trc');
  {$ENDIF}
  GetLog.LogType:=ltSystem;
  GetLog.Active:=False;
  if CanWriteToProgramDir and HasOption('debug') then
    begin
      GetLog.FileName:='processmanager.log';
      GetLog.LogType:=ltFile;
      GetLog.AppendContent:=False;
    end;
  Info('processmanager starting...');
  with BaseApplication as IBaseDbInterface do
    begin
      Info('loading mandants...');
      if not LoadMandants then
        begin
          Error(strFailedtoLoadMandants);
        end;
      if not HasOption('m','mandant') then
        begin
          Error(strMandantnotSelected);
        end;
      Info('login...');
      if (not DBLogin(GetOptionValue('m','mandant'),GetOptionValue('u','user'),False,False)) and (not DBLogin('standard','',False,False)) then
        begin
          Error(strLoginFailed+' '+LastError);
          InitMsgNetwork;
        end
      else
        begin
          Info('processmanager login successful');
          uData.DataM := Data;
          InitMsgNetwork;
          Data2 := TBaseDBModule.Create(nil);
          Data2.SetProperties(Data.Properties);
        end;
    end;
  aSystem := GetSystemName;
  if HasOption('systemname') then
    aSystem:=GetOptionValue('systemname');
  Info(aSystem+' running');
  if not Assigned(uData.Data) then
    Info('Waiting for Mandant Configuration');
  while not Assigned(uData.Data) do
    begin
      //CheckSynchronize(50);
      sleep(50);
    end;
  if Assigned(uData.Data) then
    begin
      Data2.ProcessClient.Startup;
      Data2.ProcessClient.Processes.Open;
      Data2.ProcessClient.Processes.Parameters.Open;
      if Data2.ProcessClient.Locate('NAME',aSystem,[]) then
        if Data2.ProcessClient.FieldByName('STATUS').AsString='I' then
          begin
            Terminate;
            exit;
          end;
      aTime := Now();
      i := 0;
      try
      while (not Terminated) and ((Now()-aTime) < ((1/MinsPerDay)*StrToIntDef(GetOptionValue('restarttime'),1200))) do
        begin
          CheckSynchronize(5);
          //sleep(10);
          if i > 60000 then
            begin
              i := 0;
              //EnterCriticalsection(GlobalLock);
              if not Data2.ProcessClient.ProcessAll(aSystem) then
                begin
                  Terminate;
                  exit;
                end;
              //LeaveCriticalsection(GlobalLock);
            end;
          CheckSynchronize(5);
          inc(i);
        end;
      except
        on e : Exception do
          Error('Exception:'+e.Message);
      end;
      Data2.Free;
    end;
  // stop program loop
  Terminate;
end;
constructor TProcessManager.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FreeAndNil(FMsgClient);
  StopOnException:=True;
  OnException:=@ProcessManagerException;
  //PowerStateMonitor := TPowerStateMonitor.Create;
end;
destructor TProcessManager.Destroy;
begin
  Info('Shutting down ...');
  if Assigned(Data) then
    Data.ProcessClient.ShutDown;
  inherited Destroy;
end;
var
  Application: TProcessManager;
{$R *.res}
begin
  Application:=TProcessManager.Create(nil);
  Application.Run;
  Application.Free;
end.
