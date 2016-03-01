library audio;

{$mode objfpc}{$H+}

uses
  Classes, laz_acs, acs_stdaudio, acs_wave,acs_converters ,Sysutils,acs_allformats,acs_file,
  acs_misc,acs_indicator,acs_audio,acs_volumequery;


var
  Output : TAcsAudioOut;
  FileInput : TAcsFileIn;
  FileLoaded : Boolean;
  Input : TAcsAudioIn;
  Indicator : TAcsVolumeQuery;
  NullOutput : TAcsNULLOut;

procedure CreateClasses;
begin
  if not Assigned(Output) then
    begin
      Output := TAcsAudioOut.Create(nil);
      Output.Driver:='DirectSound';
    end;
  if not Assigned(FileInput) then
    begin
      FileInput := TAcsFileIn.Create(nil);
      FileInput.Loop:=False;
      Output.Input:=FileInput;
    end;
  if not Assigned(Input) then
    begin
      Input := TAcsAudioIn.Create(nil);
      Input.Driver:='Wavemapper';
    end;
  if not Assigned(Indicator) then
    begin
      Indicator := TAcsVolumeQuery.Create(nil);
      Indicator.Input := Input;
    end;
  if not Assigned(NullOutput) then
    begin
      NullOutput := TAcsNULLOut.Create(nil);
      NullOutput.Input := Indicator;
      //NullOutput.Input := Input;
      NullOutput.BufferSize:=$10;
      //NullOutput.FileName:='c:\tmp.wav';
    end;
end;

function ListInputs : PChar;stdcall;
var
  i: Integer;
  Res: String;
begin
  Res := '';
  CreateClasses;
  for i := 0 to Input.DeviceCount-1 do
    Res := Res+Input.DeviceInfo[i].DeviceName+LineEnding;
  Result := PChar(Res);
end;

function SetInput(aName : PChar) : Boolean;stdcall;
var
  i: Integer;
begin
  Result := False;
  CreateClasses;
  for i := 0 to Input.DeviceCount-1 do
    if Input.DeviceInfo[i].DeviceName = aName then
      begin
        Input.Device:=i;
        Result := True;
      end;
end;

function ListOutputs : PChar;stdcall;
var
  i: Integer;
  Res: String;
begin
  Res := '';
  CreateClasses;
  for i := 0 to Output.DeviceCount-1 do
    Res := Res+Output.DeviceInfo[i].DeviceName+LineEnding;
  Result := PChar(Res);
end;

function SetOutput(aName : PChar) : Boolean;stdcall;
var
  i: Integer;
begin
  Result := False;
  CreateClasses;
  for i := 0 to Output.DeviceCount-1 do
    if Output.DeviceInfo[i].DeviceName = aName then
      begin
        Output.Device:=i;
        Result := True;
      end;
end;

function Start : Boolean;stdcall;
begin
  Result := True;
  CreateClasses;
  Result := FileLoaded;
  if not Result then exit;
  try
    Output.Run;
    Result := Output.Active;
  except
    Result := False;
  end;
end;

function IsPlaying : Boolean;stdcall;
begin
  Result := Output.Active;
end;

function StartRecording : Boolean;stdcall;
begin
  Result := True;
  CreateClasses;
  if not Result then exit;
  try
    NullOutput.Run;
    Result := NullOutput.Active;
  except
    Result := False;
  end;
end;

function IsRecording : Boolean;stdcall;
begin
  Result := NullOutput.Active;
end;

function GetRecordVolumeDB : Double;stdcall;
begin
  Result := (Indicator.dbLeft+Indicator.dbRight)/2;
end;

function GetRecordVolume : Word;stdcall;
begin
  Result := round((Indicator.volLeft+Indicator.volRight)/2);
end;

function StopRecording : Boolean;stdcall;
begin
  Result := True;
  CreateClasses;
  try
    NullOutput.Stop;
  except
    Result := False;
  end;
end;

function Stop : Boolean;stdcall;
begin
  Result := True;
  CreateClasses;
  try
    Output.Stop;
  except
    Result := False;
  end;
end;

function LoadFile(aFilename : PChar) : Boolean;stdcall;
begin
  Result := True;
  FileLoaded:=False;
  CreateClasses;
  try
    Output.Stop;
    FileInput.FileName:=aFilename;
    FileInput.Init;
    Result := FileInput.Size>0;
    FileInput.FileName:=aFilename;
    FileLoaded:=Result;
  except
    Result := False;
  end;
end;

procedure ScriptCleanup;
begin
  if Assigned(Output) then
    Output.Stop;
  if Assigned(NullOutput) then
    NullOutput.Stop;
  FreeAndNil(Output);
  FreeAndNil(Input);
  FreeAndNil(FileInput);
  FreeAndNil(Indicator);
  FreeAndNil(NullOutput);
end;
function ScriptDefinition : PChar;stdcall;
begin
  Result := 'function ListInputs : PChar;stdcall;'
       +#10+'function SetInput(aName : PChar) : Boolean;stdcall;'
       +#10+'function ListOutputs : PChar;stdcall;'
       +#10+'function SetOutput(aName : PChar) : Boolean;stdcall;'
       +#10+'function LoadFile(aFilename : PChar) : Boolean;stdcall;'
       +#10+'function Start : Boolean;stdcall;'
       +#10+'function IsPlaying : Boolean;stdcall;'
       +#10+'function Stop : Boolean;stdcall;'
       +#10+'function StartRecording : Boolean;stdcall;'
       +#10+'function IsRecording : Boolean;stdcall;'
       +#10+'function GetRecordVolume : Word;stdcall;'
       +#10+'function GetRecordVolumeDB : Double;stdcall;'
       +#10+'function StopRecording : Boolean;stdcall;'
            ;
end;

exports
  ListInputs,
  ListOutputs,
  Start,
  IsPlaying,
  Stop,
  LoadFile,
  SetOutput,
  SetInput,
  StartRecording,
  IsRecording,
  GetRecordVolume,
  GetRecordVolumeDB,
  StopRecording,

  ScriptCleanup,
  ScriptDefinition;

initialization
  Output := nil;
  Input := nil;
  FileLoaded := False;
end.

