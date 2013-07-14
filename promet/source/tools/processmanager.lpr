program processmanager;
{$mode objfpc}{$H+}
uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp, uBaseDBInterface,
  UTF8Process,uData,FileUtil,Process, db, uSystemMessage,
  uPowerState, pcmdprometapp,LCLProc,math,uBaseCustomApplication,
  uBaseApplication,Utils,uProcessManagement,eventlog,uIntfStrConsts;
type
  { TProcessManager }

  TProcessManager = class(TBaseCustomApplication)
    procedure ProcessManagerException(Sender: TObject; E: Exception);
  private
    PowerStateMonitor: TPowerStateMonitor;
  protected
    SysCommands: TSystemCommands;
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
var
  ErrorMsg: String;
  cmd: String;
  Process: TProcProcess;
  aProcess: String;
  Found: Boolean;
  i: Integer;
  sl: TStringList;
  a: Integer;
  tmp: String;
  aRefresh : Integer = 0;
  aInt: Integer;
  aNow: TDateTime;
  bProcess: TProcProcess;
  function BuildCmdLine : string;
  begin
    with Data.ProcessClient.Processes.Parameters.DataSet do
      begin
        First;
        while not EOF do
          begin
            cmd := cmd+' --'+FieldByName('PARAMETER').AsString+'='+{$IFDEF WINDOWS}'"'+{$ENDIF}FieldByName('VALUE').AsString{$IFDEF WINDOWS}+'"'{$ENDIF};
            Next;
          end;
      end;
    if pos('--mandant',lowercase(cmd)) = 0 then
      begin
        {$IFDEF WINDOWS}
        cmd := cmd+' --mandant="'+GetOptionValue('m','mandant')+'"';
        {$ELSE}
        cmd := cmd+' --mandant='+GetOptionValue('m','mandant')+'';
        {$ENDIF}
      end;
//              if Data.Users.DataSet.Active then
//                cmd := cmd+' --user="'+Data.Users.DataSet.FieldByName('NAME').AsString+'"';
  end;

begin
  SysCommands := nil;
  if CanWriteToProgramDir then
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
      aNow := Now();
      if aNow > 0 then
        begin
          if aRefresh = 0 then
            begin
              Data.ProcessClient.DataSet.Refresh;
              Data.ProcessClient.Processes.DataSet.Refresh;
              aRefresh:=RefreshAll;
            end;
          if Data.ProcessClient.DataSet.Locate('NAME',GetSystemName,[]) then
            if Data.ProcessClient.DataSet.FieldByName('STATUS').AsString <> 'R' then
              begin
                Terminate;
                break;
              end;
          Data.ProcessClient.Processes.DataSet.First;
          while not Data.ProcessClient.Processes.DataSet.EOF do
            begin
              aProcess := Data.ProcessClient.Processes.DataSet.FieldByName('NAME').AsString;
              if FileExists(aProcess+ExtractFileExt(ExeName)) then
                begin
                  Found := False;
                  tmp := aProcess;
                  for i := 0 to length(Processes)-1 do
                    if copy(Processes[i].CommandLine,0,length(tmp)) = tmp then
                      begin
                        bProcess := Processes[i];
                        if bProcess.Active then
                          Found := True
                        else
                          begin
                            tmp := aProcess+BuildCmdLine;
                            sl := TStringList.Create;
                            sl.LoadFromStream(bProcess.Output);
                            for a := 0 to sl.Count-1 do
                              Log(aprocess+':'+sl[a]);
                            sl.Free;
                            if not bProcess.Informed then
                              begin
                                Log(aprocess+':'+strExitted);
                                bProcess.DoExit;
                                bProcess.Informed := True;
                              end;
                            if (aNow > bProcess.Timeout) {and (bProcess.Timeout > 0)} then
                              begin
                                Log(aprocess+':'+strStartingProcessTimeout+' '+DateTimeToStr(bProcess.Timeout)+'>'+DateTimeToStr(aNow));
                                bProcess.Timeout := aNow+(max(Data.ProcessClient.Processes.DataSet.FieldByName('INTERVAL').AsInteger,2)/MinsPerDay);
                                Log(aProcess+':'+strStartingProcess+' ('+bProcess.CommandLine+')');
                                bProcess.Execute;
                                bProcess.Informed := False;
                                Log(aprocess+':'+strStartingNextTimeout+' '+DateTimeToStr(bProcess.Timeout));
                              end;
                            Found := True;
                          end;
                      end;
                  if not Found then
                    begin
                      cmd := aProcess+ExtractFileExt(ExeName);
                      cmd := cmd+BuildCmdLine;
                      Log(aProcess+':'+strStartingProcess+' ('+cmd+')');
                      Process := TProcProcess.Create(Self);
                      Process.Id := Data.ProcessClient.Processes.Id.AsVariant;
                      Process.Informed:=False;
                      Setlength(Processes,length(Processes)+1);
                      Processes[length(Processes)-1] := Process;
                      Process.CommandLine:=cmd;
                      Process.CurrentDirectory:=Location;
                      Process.Options := [poNoConsole,poUsePipes];
                      Process.Execute;
                      Process.Timeout := aNow+(max(Data.ProcessClient.Processes.DataSet.FieldByName('INTERVAL').AsInteger,2)/MinsPerDay);
                      Log(aprocess+':'+strStartingNextTimeout+' '+DateTimeToStr(Processes[i].Timeout));
                    end;
                end
              else Log(aProcess+ExtractFileExt(ExeName)+':'+'File dosend exists');
              Data.ProcessClient.Processes.DataSet.Next;
            end;
        end;
      sleep(20*1000);
    end;
  try
    Data.ProcessClient.DataSet.Edit;
    Data.ProcessClient.DataSet.FieldByName('STATUS').AsString:='N';
    Data.ProcessClient.DataSet.Post;
  except
  end;
  // stop program loop
  for i := 0 to length(Processes)-1 do
    Processes[i].Free;
  Terminate;
end;
constructor TProcessManager.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
  OnException:=@ProcessManagerException;
  PowerStateMonitor := TPowerStateMonitor.Create;
end;
destructor TProcessManager.Destroy;
begin
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
