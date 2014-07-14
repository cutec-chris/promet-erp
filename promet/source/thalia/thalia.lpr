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
 program thalia;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp
  { you can add units after this },db,Utils, general_nogui,
  FileUtil,uData, uIntfStrConsts, pcmdprometapp,uBaseCustomApplication,
  uBaseApplication,uBaseDbClasses,uBaseDBInterface,uSpeaker,
  umath,uwikipedia, uinformation;
type
  PrometCmdApp = class(TBaseCustomApplication)
    procedure FSpeakerDebugMessage(sentence: string);
    procedure FSpeakerSystemMessage(sentence: string);
  private
    FSpeaker: TSpeaker;
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;
  TThaliaAnswers = class(TBaseDBDataset)
  public
    procedure DefineFields(aDataSet: TDataSet); override;
  end;
  TThaliaScentences = class(TBaseDBDataset)
  private
    FAnswers: TThaliaAnswers;
  public
    procedure DefineFields(aDataSet: TDataSet); override;
    constructor Create(aOwner: TComponent; DM: TComponent;
      aConnection: TComponent=nil; aMasterdata: TDataSet=nil); override;
    function CreateTable: Boolean; override;
    destructor Destroy; override;
    property Answers : TThaliaAnswers read FAnswers;
  end;
  TThaliaDict = class(TBaseDBDataset)
  public
    procedure DefineFields(aDataSet: TDataSet); override;
  end;
  TThaliaVariables = class(TBaseDBDataset)
  public
    procedure DefineFields(aDataSet: TDataSet); override;
  end;
  TThaliaInterlocutors = class(TBaseDBDataset)
  private
    FVariables: TThaliaVariables;
  public
    constructor Create(aOwner: TComponent; DM: TComponent;
      aConnection: TComponent=nil; aMasterdata: TDataSet=nil); override;
    destructor Destroy; override;
    function CreateTable: Boolean; override;
    procedure DefineFields(aDataSet: TDataSet); override;
    property Variables : TThaliaVariables read FVariables;
  end;
  TPrometSpeakerData = class(TSpeakerData)
  private
    FSentences: TThaliaScentences;
    FWords: TThaliaDict;
    FInterlocutors : TThaliaInterlocutors;
  public
    constructor Create;
    destructor Destroy; override;
    function GetAnswers(aFilter: string): TDataSet; override;
    function GetScentences(aFilter: string): TDataSet; override;
    function GetWords(aFilter: string): TDataSet; override;
    function SetVariable(aInterlocutor : string;aVarname : string;aValue : string) : Boolean;override;
    function GetVariable(aInterlocutor : string;aVarname : string) : string;override;
  end;
  TPrometSpeakerInterface=class(TSpeakerInterface)
  public
    procedure Connect; override;
    procedure Disconnect; override;
    procedure Talk(user, sentence: string); override;
    function Process(NeedNewMessage: Boolean=False): boolean; override;
    function GetID: string; override;
    function IsUser(user: string): Boolean; override;
  end;

{ TThaliaVariables }

procedure TThaliaVariables.DefineFields(aDataSet: TDataSet);
begin
  with aDataSet as IBaseManageDB do
    begin
      TableName := 'THALIA_VARIABLES';
      if Assigned(ManagedFieldDefs) then
        with ManagedFieldDefs do
          begin
            Add('NAME',ftString,100,true);
            Add('VALUE',ftMemo,0,false);
          end;
    end;
end;

{ TThaliaInterlocutors }

constructor TThaliaInterlocutors.Create(aOwner: TComponent; DM: TComponent;
  aConnection: TComponent; aMasterdata: TDataSet);
begin
  inherited Create(aOwner, DM, aConnection, aMasterdata);
  FVariables := TThaliaVariables.Create(aOwner,DM,aConnection,DataSet);
end;

destructor TThaliaInterlocutors.Destroy;
begin
  FVariables.Free;
  inherited Destroy;
end;

function TThaliaInterlocutors.CreateTable: Boolean;
begin
  Result:=inherited CreateTable;
  FVariables.CreateTable;
end;

procedure TThaliaInterlocutors.DefineFields(aDataSet: TDataSet);
begin
  with aDataSet as IBaseManageDB do
    begin
      TableName := 'THALIA_INTERLOC';
      if Assigned(ManagedFieldDefs) then
        with ManagedFieldDefs do
          begin
            Add('NAME',ftString,100,true);
          end;
    end;
end;

procedure TPrometSpeakerInterface.Connect;
begin

end;

procedure TPrometSpeakerInterface.Disconnect;
begin

end;

procedure TPrometSpeakerInterface.Talk(user, sentence: string);
begin

end;

function TPrometSpeakerInterface.Process(NeedNewMessage: Boolean): boolean;
begin

end;

function TPrometSpeakerInterface.GetID: string;
begin

end;

function TPrometSpeakerInterface.IsUser(user: string): Boolean;
begin

end;

constructor TPrometSpeakerData.Create;
begin
  FSentences := TThaliaScentences.Create(nil,Data);
  FSentences.CreateTable;
  FWords := TThaliaDict.Create(nil,Data);
  FWords.CreateTable;
  FInterlocutors := TThaliaInterlocutors.Create(nil,Data);
  FInterlocutors.CreateTable;
end;
destructor TPrometSpeakerData.Destroy;
begin
  FInterlocutors.Free;
  FSentences.Free;
  FWords.Free;
  inherited Destroy;
end;
function TPrometSpeakerData.GetAnswers(aFilter: string): TDataSet;
begin
  FSentences.Answers.Open;
  Result := FSentences.Answers.DataSet;
end;
function TPrometSpeakerData.GetScentences(aFilter: string): TDataSet;
begin
  FSentences.Filter(aFilter);
  Result := FSentences.DataSet;
end;
function TPrometSpeakerData.GetWords(aFilter: string): TDataSet;
begin
  FWords.Filter(aFilter);
  Result := FSentences.DataSet;
end;

function TPrometSpeakerData.SetVariable(aInterlocutor: string;
  aVarname: string; aValue: string): Boolean;
begin
  FInterlocutors.Open;
  if not FInterlocutors.Locate('NAME',aInterlocutor,[]) then
    begin
      FInterlocutors.Insert;
      FInterlocutors.FieldByName('NAME').AsString:=aInterlocutor;
      FInterlocutors.Post;
    end;
  FInterlocutors.Variables.Filter(Data.QuoteField('NAME')+'='+Data.QuoteValue(aVarname));
  if FInterlocutors.Variables.Count=0 then
    begin
      FInterlocutors.Variables.Insert;
      FInterlocutors.Variables.FieldByName('NAME').AsString:=aVarname;
    end
  else
    FInterlocutors.Variables.Edit;
  FInterlocutors.Variables.FieldByName('VALUE').AsString:=aValue;
  FInterlocutors.Variables.Post;
end;

function TPrometSpeakerData.GetVariable(aInterlocutor: string; aVarname: string
  ): string;
begin
  Result := '';
  FInterlocutors.Open;
  if FInterlocutors.Locate('NAME',aInterlocutor,[]) then
    begin
      FInterlocutors.Variables.Filter(Data.QuoteField('NAME')+'='+Data.QuoteValue(aVarname));
      if FInterlocutors.Variables.Count>0 then
        Result := FInterlocutors.Variables.FieldByName('VALUE').AsString;
    end;
end;

procedure TThaliaAnswers.DefineFields(aDataSet: TDataSet);
begin
  with aDataSet as IBaseManageDB do
    begin
      TableName := 'THALIA_ANSWERS';
      if Assigned(ManagedFieldDefs) then
        with ManagedFieldDefs do
          begin
            Add('ANSWER',ftString,1000,true);
          end;
    end;
end;
procedure TThaliaScentences.DefineFields(aDataSet: TDataSet);
begin
  with aDataSet as IBaseManageDB do
    begin
      TableName := 'THALIA_SENTENCES';
      if Assigned(ManagedFieldDefs) then
        with ManagedFieldDefs do
          begin
            Add('ID',ftLargeint,0,true);
            Add('WORDS',ftString,500,true);
            Add('CATEGORY',ftString,60,false);
            Add('TYPE',ftSmallInt,0,true);
            Add('PRIORITY',ftSmallInt,0,true);
          end;
    end;
end;
procedure TThaliaDict.DefineFields(aDataSet: TDataSet);
begin
  with aDataSet as IBaseManageDB do
    begin
      TableName := 'THALIA_DICT';
      if Assigned(ManagedFieldDefs) then
        with ManagedFieldDefs do
          begin
            Add('WORD',ftString,100,true);
            Add('TYPE',ftSmallInt,0,true);
          end;
    end;
end;
constructor TThaliaScentences.Create(aOwner: TComponent; DM: TComponent;
  aConnection: TComponent; aMasterdata: TDataSet);
begin
  inherited Create(aOwner, DM, aConnection, aMasterdata);
  FAnswers := TThaliaAnswers.Create(aOwner,DM,aConnection,DataSet);
end;
function TThaliaScentences.CreateTable: Boolean;
begin
  Result:=inherited CreateTable;
  FAnswers.CreateTable;
end;
destructor TThaliaScentences.Destroy;
begin
  FAnswers.Free;
  inherited Destroy;
end;

procedure PrometCmdApp.FSpeakerDebugMessage(sentence: string);
begin
  if not HasOption('q','quiet') then
    if HasOption('d','debug') then
      write(sentence);
end;

procedure PrometCmdApp.FSpeakerSystemMessage(sentence: string);
begin
  if not HasOption('q','quiet') then
    write(sentence);
end;

procedure PrometCmdApp.DoRun;
begin
  with BaseApplication as IBaseApplication do
    begin
      AppVersion:={$I ../base/version.inc};
      AppRevision:={$I ../base/revision.inc};
    end;
  if not Login then Terminate;
  //Your logged in here on promet DB
  FSpeaker := TSpeaker.Create('thalia','deutsch',TPrometSpeakerData.Create);
  FSpeaker.FastAnswer := True;
  if HasOption('c','cmdln') then
    FSpeaker.Intf := TCmdLnInterface.Create
  else
    FSpeaker.Intf := TPrometSpeakerInterface.Create;
  FSpeaker.OnSystemMessage:=@FSpeakerSystemMessage;
  FSpeaker.OnDebugMessage:=@FSpeakerDebugMessage;
  if not HasOption('q','quiet') then
    writeln('Connecting...');
  FSpeaker.Intf.Connect;
  if not HasOption('q','quiet') then
    writeln('Started OK...');
  while FSpeaker.Process(True) do sleep(100);
  if not HasOption('q','quiet') then
    writeln('Disconnecting...');
  FSpeaker.Intf.Disconnect;
  // stop program loop
  readln;
  Terminate;
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
  FSpeaker.Free;
  inherited Destroy;
end;
var
  Application: PrometCmdApp;
begin
  Application:=PrometCmdApp.Create(nil);
  Application.Run;
  Application.Free;
end.

