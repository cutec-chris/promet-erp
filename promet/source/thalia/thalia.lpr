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
  uBaseApplication,uBaseDbClasses,uBaseDBInterface,uSpeaker;

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

  { TThaliaAnswers }

  TThaliaAnswers = class(TBaseDBDataset)
  public
    procedure DefineFields(aDataSet: TDataSet); override;
  end;

  { TThaliaScentences }

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

  { TThaliaDict }

  TThaliaDict = class(TBaseDBDataset)
  public
    procedure DefineFields(aDataSet: TDataSet); override;
  end;

  { TPrometSpeakerData }

  TPrometSpeakerData = class(TSpeakerData)
  private
    FSentences: TThaliaScentences;
    FWords: TThaliaDict;
  public
    constructor Create;
    destructor Destroy; override;
    function GetAnswers(aFilter: string): TDataSet; override;
    function GetScentences(aFilter: string): TDataSet; override;
    function GetWords(aFilter: string): TDataSet; override;
  end;

{ TPrometSpeakerData }

constructor TPrometSpeakerData.Create;
begin
  FSentences := TThaliaScentences.Create(nil,Data);
  FWords := TThaliaDict.Create(nil,Data);
end;

destructor TPrometSpeakerData.Destroy;
begin
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

{ TThaliaAnswers }

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

{ TThaliaScentences }

procedure TThaliaScentences.DefineFields(aDataSet: TDataSet);
begin
  with aDataSet as IBaseManageDB do
    begin
      TableName := 'THALIA_SENTENCES';
      if Assigned(ManagedFieldDefs) then
        with ManagedFieldDefs do
          begin
            Add('WORDS',ftString,500,true);
            Add('CATEGORY',ftString,60,false);
            Add('TYPE',ftSmallInt,0,true);
          end;
    end;
end;

{ TThaliaDict }

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

{ PrometCmdApp }

procedure PrometCmdApp.DoRun;
begin
  with BaseApplication as IBaseApplication do
    begin
      AppVersion:={$I ../base/version.inc};
      AppRevision:={$I ../base/revision.inc};
    end;
  if not Login then Terminate;
  //Your logged in here on promet DB


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

