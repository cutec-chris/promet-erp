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
 program import_fhem;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils, CustApp
  { you can add units after this },db,Utils,
  FileUtil,uData, uIntfStrConsts, pcmdprometapp,uBaseCustomApplication, ufhem,
  uBaseDbClasses,uMeasurement,
  uBaseApplication;

type

  { PrometCmdApp }

  PrometCmdApp = class(TBaseCustomApplication)
    procedure FhemInfo(aInfo: string);
    procedure FhemTerminate(Sender: TObject);
  private
    anObject: TObjects;
    aMeasurement: TMeasurement;
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

{ PrometCmdApp }

procedure PrometCmdApp.FhemInfo(aInfo: string);
var
  Typ: String;
  Dev: String;
  reading: String;
  aObject: String;
  PayloadFloat: Extended;
  payload: String;
  aProp : string;
begin
  Debug(StringReplace(aInfo,'<br>','',[rfReplaceAll]));
  aInfo := copy(aInfo,pos(' ',aInfo)+1,length(aInfo));//Date
  aInfo := copy(aInfo,pos(' ',aInfo)+1,length(aInfo));//Time
  Typ := copy(aInfo,0,pos(' ',aInfo)-1);
  aInfo := copy(aInfo,pos(' ',aInfo)+1,length(aInfo));
  Dev := copy(aInfo,0,pos(' ',aInfo)-1);
  aInfo := copy(aInfo,pos(' ',aInfo)+1,length(aInfo));
  reading := copy(aInfo,0,pos(' ',aInfo)-1);
  if pos(':',reading)=0 then exit;
  reading := copy(reading,0,pos(':',aInfo)-1);
  aInfo := copy(aInfo,pos(' ',aInfo)+1,length(aInfo));
  payload := copy(aInfo,0,pos('<',aInfo)-1);
  aObject := Dev;
  aprop := reading;
  try
    if not TryStrToFloat(payload,PayloadFloat) then
      begin
        if (lowercase(payload)='ok')
        or (lowercase(payload)='true')
        or (lowercase(payload)='y')
        or (lowercase(payload)='on')
        then PayloadFloat:=1
        else
        if (lowercase(payload)='false')
        or (lowercase(payload)='n')
        or (lowercase(payload)='off')
        then PayloadFloat:=0
        else exit;
      end;
    if anObject.Active and (anObject.Number.AsString<> aObject) then
      begin
        anObject.SelectFromNumber(aObject);
        anObject.Open;
      end;
    if anObject.Count>0 then
      begin
        aMeasurement.Open;
        if aMeasurement.Locate('ID',aProp,[loCaseInsensitive]) then
          begin
            if (aMeasurement.Current.AsFloat<>PayloadFloat) then
              begin
                aMeasurement.Edit;
                aMeasurement.Current.AsFloat:=PayloadFloat;
                aMeasurement.Post;
              end;
          end;
      end;
  except
    Terminate;
  end;

end;

procedure PrometCmdApp.FhemTerminate(Sender: TObject);
begin
  Terminate;
end;

procedure PrometCmdApp.DoRun;
var
  Port: String;
  Fhem: TFHEMLogThread;
  aServer: String;
begin
  with BaseApplication as IBaseApplication do
    begin
      AppVersion:={$I ../base/version.inc};
      AppRevision:={$I ../base/revision.inc};
    end;
  if not Login then Terminate;
  //Your logged in here on promet DB
  aServer := GetOptionValue('f','fhem');
  if aServer = '' then
    begin
      writeln('You must specify an Fehm Server (--fhem=IP)');
      Terminate;
      exit;
    end;
  Fhem := TFHEMLogThread.Create(aServer);
  Fhem.OnTerminate:=@FhemTerminate;
  Fhem.OnInfo:=@FhemInfo;
  anObject:=TObjects.Create(nil);
  aMeasurement := TMeasurement.CreateEx(nil,Data,anObject.Connection,anObject.DataSet);
  try
    while not Terminated do
      begin
        sleep(100);
        CheckSynchronize(100);
      end;
  finally
    FreeAndNil(aMeasurement);
    FreeAndNil(anObject);
  end;
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

