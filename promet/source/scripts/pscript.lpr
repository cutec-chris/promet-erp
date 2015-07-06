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
Created 08.08.2014
*******************************************************************************}
 program pscript;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp
  { you can add units after this },db,Utils,
  uData, uIntfStrConsts, pcmdprometapp,uBaseCustomApplication,
  uBaseApplication,uprometscripts,genpascalscript,uprometpascalscript;

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

procedure aSleep(MiliSecValue: cardinal);
begin
  sleep(MiliSecValue);
end;

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
  bScript: TScript;
  sl: TStringList;
begin
  with BaseApplication as IBaseApplication do
    begin
      AppVersion:={$I ../base/version.inc};
      AppRevision:={$I ../base/revision.inc};
    end;
  if HasOption('m','mandant') then
    begin
      if not Login then Terminate;
      //Your logged in here on promet DB
      aScript := TBaseScript.Create(nil);
      //aScript.Sleep:=@aSleep;
      aScript.SelectByName(ParamStr(ParamCount));
      aScript.Open;
      if not aScript.Locate('NAME',ParamStr(ParamCount),[loCaseInsensitive]) then
        begin
          if not FileExists(ParamStr(ParamCount)) then
            begin
              writeln('Script "'+ParamStr(ParamCount)+'" not found !');
              aScript.Free;
              Terminate;
              exit;
            end
          else //Load file from Directory
            begin
              writeln('not implemented !!');
            end;
        end;
      if lowercase(aScript.FieldByName('SYNTAX').AsString)='pascal' then
        begin
          aScript.Free;
          aScript := TPrometPascalScript.Create(nil);
          aScript.SelectByName(ParamStr(ParamCount));
          aScript.Open;
          if not aScript.Locate('NAME',ParamStr(ParamCount),[loCaseInsensitive]) then
            begin
              writeln('Script "'+ParamStr(ParamCount)+'" not found !');
            end;
        end;
      aScript.Readln:=@aScriptReadln;
      aScript.Write:=@aScriptWrite;
      aScript.Writeln:=@aScriptWriteln;
      aScript.Execute(Null);
      aScript.Free;
    end
  else if FileExists(ParamStr(ParamCount)) then//no database access
    begin
      bScript := TPascalScript.Create;
      sl := TStringList.Create;
      sl.LoadFromFile(ParamStr(ParamCount));
      sl.Free;
      bScript.Source:=sl.Text;
      if bScript is TByteCodeScript then
        if not (bScript as TByteCodeScript).Compile then
          begin
            writeln('Compilation failed:'+bScript.Results);
            Terminate;
            exit;
          end;
      if not bScript.Execute(Null) then
        begin
          writeln('Execute failed:'+bScript.Results);
          Terminate;
          exit;
        end
      else
        begin
          writeln('Execute successful:'+bScript.Results);
        end;
      bScript.Free;
    end
  else
    writeln('Sytanx: [Params] Scriptname');
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

