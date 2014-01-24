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
unit uProcessManagement;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils,uBaseDbClasses,db,UTF8Process;
type
  TProcProcess = class(TProcessUTF8)
  private
    FId: Variant;
    FInformed: Boolean;
    FName: string;
    FTimeout: TDateTime;
    procedure SetTimeout(AValue: TDateTime);
  public
    aOutput,aBuffer,aLogOutput : string;
    property Timeout : TDateTime read FTimeout write SetTimeout;
    property Informed : Boolean read FInformed write FInformed;
    property Name : string read FName write FName;
    property Id : Variant read FId write FId;
    procedure Execute; override;
    procedure DoExit;
  end;

  TProcessParameters = class(TBaseDBDataset)
  public
    procedure DefineFields(aDataSet: TDataSet); override;
  end;
  TProcesses = class(TBaseDBDataset)
  private
    FProcessParameters: TProcessParameters;
  public
    constructor Create(aOwner: TComponent; DM: TComponent;
      aConnection: TComponent=nil; aMasterdata: TDataSet=nil); override;
    destructor Destroy; override;
    function CreateTable : Boolean;override;
    procedure DefineFields(aDataSet: TDataSet); override;
    property Parameters : TProcessParameters read FProcessParameters;
  end;
  TProcessClient = class(TBaseDBDataset)
  private
    FProcesses: TProcesses;
  public
    constructor Create(aOwner: TComponent; DM: TComponent;
       aConnection: TComponent=nil; aMasterdata: TDataSet=nil); override;
    destructor Destroy; override;
    function CreateTable : Boolean;override;
    procedure DefineFields(aDataSet: TDataSet); override;
    property Processes : TProcesses read FProcesses;
  end;

implementation
uses uBaseDBInterface,uData;
procedure TProcProcess.SetTimeout(AValue: TDateTime);
begin
  if FTimeout=AValue then Exit;
  FTimeout:=AValue;
end;
procedure TProcProcess.Execute;
var
  aProc: TProcesses;
begin
  aProc := uProcessManagement.TProcesses.Create(nil,Data);
  aProc.Select(Id);
  aProc.Open;
  if aProc.Count > 0 then
    begin
      aProc.DataSet.Edit;
      aProc.FieldByName('STATUS').AsString:='R';
      aProc.DataSet.Post;
    end;
  inherited Execute;
  aProc.Free;
end;

procedure TProcProcess.DoExit;
var
  aProc: TProcesses;
begin
  aProc := uProcessManagement.TProcesses.Create(nil,Data);
  aProc.Select(Id);
  aProc.Open;
  if aProc.Count > 0 then
    begin
      aProc.DataSet.Edit;
      aProc.FieldByName('STATUS').AsString:='N';
      aProc.DataSet.Post;
    end;
  aProc.Free;
end;
procedure TProcessParameters.DefineFields(aDataSet: TDataSet);
begin
  with aDataSet as IBaseManageDB do
    begin
      TableName := 'PROCESSPARAMETERS';
      if Assigned(ManagedFieldDefs) then
        with ManagedFieldDefs do
          begin
            Add('NAME',ftString,60,True);
            Add('VALUE',ftString,60,False);
          end;
    end;
end;

constructor TProcesses.Create(aOwner: TComponent; DM: TComponent;
      aConnection: TComponent=nil; aMasterdata: TDataSet=nil);
begin
  inherited;
  FProcessParameters := TProcessParameters.Create(Self, DM,aConnection,DataSet);
end;

destructor TProcesses.Destroy;
begin
  FProcessParameters.Destroy;
  inherited Destroy;
end;

function TProcesses.CreateTable : Boolean;
begin
  Result := inherited CreateTable;
  FProcessParameters.CreateTable;
end;

procedure TProcesses.DefineFields(aDataSet: TDataSet);
begin
  with aDataSet as IBaseManageDB do
    begin
      TableName := 'PROCESS';
      if Assigned(ManagedFieldDefs) then
        with ManagedFieldDefs do
          begin
            Add('NAME',ftString,250,True);
            Add('INTERVAL',ftInteger,0,False);
            Add('STATUS',ftString,4,False);
            Add('LOG',ftMemo,0,False);
          end;
    end;
end;

constructor TProcessClient.Create(aOwner: TComponent; DM: TComponent;
  aConnection: TComponent; aMasterdata: TDataSet);
begin
  inherited;
  FProcesses := TProcesses.Create(Self, DM,aConnection,DataSet);
end;

destructor TProcessClient.Destroy;
begin
  FProcesses.Destroy;
  inherited Destroy;
end;

function TProcessClient.CreateTable : Boolean;
begin
  Result := inherited CreateTable;
  FProcesses.CreateTable;
end;

procedure TProcessClient.DefineFields(aDataSet: TDataSet);
begin
  with aDataSet as IBaseManageDB do
    begin
      TableName := 'PROCESSCLIENTS';
      if Assigned(ManagedFieldDefs) then
        with ManagedFieldDefs do
          begin
            Add('NAME',ftString,60,True);
            Add('STATUS',ftString,4,True);
            Add('NOTES',ftString,200,False);
          end;
    end;
end;

end.

