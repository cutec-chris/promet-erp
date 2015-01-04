 program pscript;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp
  { you can add units after this },db,Utils,
  FileUtil,uData, uIntfStrConsts, pcmdprometapp,uBaseCustomApplication,
  uBaseApplication,uprometscripts, uImaging;

type

  { PrometCmdApp }

  PrometCmdApp = class(TBaseCustomApplication)
    procedure aScriptReadln(var s: string);
    procedure aScriptWrite(const s: string);
    procedure aScriptWriteln(const s: string);
  private
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

{ PrometCmdApp }

procedure PrometCmdApp.aScriptReadln(var s: string);
begin
  readln(s);
end;

procedure PrometCmdApp.aScriptWrite(const s: string);
begin
  write(s);
end;

procedure PrometCmdApp.aScriptWriteln(const s: string);
begin
  writeln(s);
end;

procedure PrometCmdApp.DoRun;
var
  aScript: TBaseScript;
begin
  with BaseApplication as IBaseApplication do
    begin
      AppVersion:={$I ../base/version.inc};
      AppRevision:={$I ../base/revision.inc};
    end;
  if not Login then Terminate;
  //Your logged in here on promet DB
  aScript := TBaseScript.Create(nil);
  aScript.Readln:=@aScriptReadln;
  aScript.Write:=@aScriptWrite;
  aScript.Writeln:=@aScriptWriteln;
  aScript.Open;
  if not aScript.Locate('NAME',ParamStr(ParamCount),[loCaseInsensitive]) then
    begin
      writeln('Script "'+ParamStr(ParamCount)+'" not found !');
      aScript.Free;
      Terminate;
      exit;
    end;
  aScript.Execute(Null);
  aScript.Free;
  readln;
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

