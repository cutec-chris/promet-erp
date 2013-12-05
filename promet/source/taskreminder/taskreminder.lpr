 program taskreminder;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp,
  Interfaces
  { you can add units after this },db,Utils,
  FileUtil,Forms,uData, uIntfStrConsts, pcmdprometapp,uBaseCustomApplication,
  uBaseApplication,uBaseDbClasses,utask;

type

  { PrometCmdApp }

  PrometCmdApp = class(TBaseCustomApplication)
  private
    mailaccounts : string;
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

{ PrometCmdApp }

procedure PrometCmdApp.DoRun;
var
  aUsers: TUser;
  aTasks: TTaskList;
begin
  with BaseApplication as IBaseApplication do
    begin
      AppVersion:={$I ../base/version.inc};
      AppRevision:={$I ../base/revision.inc};
    end;
  if not Login then Terminate;
  //Your logged in here on promet DB

  aUsers := TUser.Create(nil,Data);
  aUsers.Filter(Data.QuoteField('LOGINACTIVE')+'='+Data.QuoteValue('N'));
  aTasks := TTaskList.Create(nil,Data);
  aUsers.First;
  while not aUsers.EOF do
    begin
      aTasks.Filter(Data.QuoteField('USER')+'='+Data.QuoteValue(aUsers.FieldByName('ACCOUNTNO').AsString)+' AND '+Data.QuoteField('COMPLETED')+'<>'+Data.QuoteValue('Y')+' AND '+Data.QuoteField('SEEN')+'<>'+Data.QuoteValue('Y'));
      if not aTasks.EOF then
        writeln(aUsers.Text.AsString);
      while not aTasks.EOF do
        begin
          writeln('  '+aTasks.FieldByName('SUMMARY').AsString);
          atasks.Next;
        end;
      aUsers.Next;
    end;
  aUsers.Free;
  aTasks.Free;
  // stop program loop
  Terminate;
end;

constructor PrometCmdApp.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor PrometCmdApp.Destroy;
begin
  inherited Destroy;
end;

var
  Application: PrometCmdApp;

begin
  Application:=PrometCmdApp.Create(nil);
  Application.Run;
  Application.Free;
end.

