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
program processmanager;
{$mode objfpc}{$H+}
uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp, uBaseDBInterface,
  uData,Process, db, uSystemMessage,
  uPowerState, pcmdprometapp,math,uBaseCustomApplication,
  uBaseApplication,Utils,uProcessManagement,eventlog,uIntfStrConsts;
type
  { TProcessManager }

  TProcessManager = class(TBaseCustomApplication)
    procedure ProcessManagerException(Sender: TObject; E: Exception);
  private
    PowerStateMonitor: TPowerStateMonitor;
  protected
    Processes : array of TProcProcess;
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;
const
  RefreshAll = 15;//5 mins refresh

{ TProcProcess }
procedure TProcessManager.ProcessManagerException(Sender: TObject; E: Exception
  );
begin
  Error(e.Message);
end;
procedure TProcessManager.DoRun;
begin
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
          raise Exception.Create(strFailedtoLoadMandants);
          Terminate;
        end;
      if not HasOption('m','mandant') then
        begin
          Error(strMandantnotSelected);
          raise Exception.Create(strMandantnotSelected);
          Terminate;
        end;
      Info('login...');
      if not DBLogin(GetOptionValue('m','mandant'),'',False,False) then
        begin
          Error(strLoginFailed+' '+LastError);
          raise Exception.Create(strLoginFailed+' '+LastError);
          Terminate;
        end;
      uData.Data := Data;
    end;
  Info('processmanager login successful');
  Data.ProcessClient.CreateTable;
  Data.ProcessClient.Open;
  if not Data.ProcessClient.DataSet.Locate('NAME',GetSystemName,[]) then
    begin
      Data.ProcessClient.Insert;
      Data.ProcessClient.DataSet.FieldByName('NAME').AsString:=GetSystemName;
      Data.ProcessClient.DataSet.FieldByName('STATUS').AsString:='R';
      Data.ProcessClient.DataSet.Post;
      Info(getSystemName+' added and running');
    end
  else
    begin
      Data.ProcessClient.DataSet.Edit;
      Data.ProcessClient.DataSet.FieldByName('STATUS').AsString:='R';
      Data.ProcessClient.DataSet.Post;
      Info(getSystemName+' running');
    end;
  Data.ProcessClient.Processes.Open;
  Data.ProcessClient.Processes.Parameters.Open;
  while not Terminated do
    begin
      Data.ProcessClient.RefreshList;
      if Data.ProcessClient.DataSet.Locate('NAME','*',[]) then
        Data.ProcessClient.Process
      else
        begin
          Data.ProcessClient.Insert;
          Data.ProcessClient.FieldByName('NAME').AsString:='*';
          Data.ProcessClient.FieldByName('STATUS').AsString:='N';
          Data.ProcessClient.FieldByName('NOTES').AsString:=strRunsOnEveryMashine;
          Data.ProcessClient.Post;
        end;
      if Data.ProcessClient.DataSet.Locate('NAME',GetSystemName,[]) then
        begin
          if Data.ProcessClient.FieldByName('STATUS').AsString <> 'R' then
            begin
              Terminate;
              exit;
            end;
          Data.ProcessClient.Process;
        end;
      sleep(3000);
    end;
  try
    Data.ProcessClient.DataSet.Edit;
    Data.ProcessClient.DataSet.FieldByName('STATUS').AsString:='N';
    Data.ProcessClient.DataSet.Post;
  except
  end;
  // stop program loop
  Terminate;
end;
constructor TProcessManager.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=False;
  PowerStateMonitor := TPowerStateMonitor.Create;
end;
destructor TProcessManager.Destroy;
var
  i: Integer;
begin
  for i := 0 to length(Processes)-1 do
    Processes[i].Free;
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
