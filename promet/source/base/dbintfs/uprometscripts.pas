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
unit uprometscripts;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uBaseDbClasses, uBaseDBInterface, db, Utils
  ,uBaseDatasetInterfaces,uBaseERPDBClasses;

type
  { TBaseScript }

  TBaseScript = class(TBaseERPList,IBaseHistory)
  private
    aDS: TDataSet;
    FHistory: TBaseHistory;
    FLinks: TLinks;
    FDataSource: TDataSource;
    FRlFunc: TStrInFunc;
    FWrFunc: TStrOutFunc;
    FWriFunc: TStrOutFunc;
    procedure SQLConn;
  protected
    procedure DoSetResults(aRes : string);
    procedure DoSetStatus(s : string);
    function GetTextFieldName: string;override;
    function GetNumberFieldName : string;override;
    procedure InternalWrite(const s: string);
    procedure InternalWriteln(const s: string);
    procedure InternalReadln(var s: string);
    function GetHistory: TBaseHistory;
  public
    constructor CreateEx(aOwner: TComponent; DM: TComponent;
      aConnection: TComponent=nil; aMasterdata: TDataSet=nil); override;
    procedure DefineFields(aDataSet: TDataSet); override;
    procedure FillDefaults(aDataSet: TDataSet); override;
    function SelectByName(aName: string): Boolean;
    function Execute(Parameters : Variant) : Boolean;virtual;
    property Write : TStrOutFunc read FWriFunc write FWriFunc;
    property Writeln : TStrOutFunc read FWrFunc write FWRFunc;
    property Readln : TStrInFunc read FRlFunc write FRlFunc;
    property History : TBaseHistory read FHistory;
    property Links : TLinks read FLinks;
    destructor Destroy;override;
  end;

  function ProcessScripts : Boolean;//process Scripts that must be runned cyclic

var
  Historyrun : Boolean;

implementation
uses uStatistic,uData,httpsend,variants,uPerson,uMasterdata,uProjects,uOrder,
  uBaseApplication,uSystemMessage,utask,uMessages,uDocuments;
function ProcessScripts : Boolean;//process Scripts that must be runned cyclic Result shows that it should be runned faster (debug)
var
  aScript: TBaseScript;
  aHistory: TBaseHistory;
  bScript: TBaseScript;
begin
  Result:=false;
  aScript := TBaseScript.Create(nil);
  aScript.Filter(Data.QuoteField('RUNEVERY')+'>'+Data.QuoteValue('0')+' OR '+Data.QuoteField('STATUS')+'='+Data.QuoteValue('d'));
  while not aScript.EOF do
    begin
      if (aScript.FieldByName('STATUS').AsString<>'S') and ((aScript.FieldByName('RUNMASHINE').AsString='') or (pos(GetSystemName,aScript.FieldByName('RUNMASHINE').AsString)>0)) then
        if (aScript.FieldByName('LASTRUN').AsDateTime+(aScript.FieldByName('RUNEVERY').AsInteger/MinsPerDay)<Now()) or (aScript.FieldByName('STATUS').AsString='d') or (aScript.FieldByName('STATUS').AsString='r') then
          begin
            bScript := TBaseScript.CreateEx(nil,aScript.DataModule,aScript.Connection);
            bScript.Select(aScript.Id.AsVariant);
            bScript.Open;
            Result := (aScript.FieldByName('STATUS').AsString='d');
            if bScript.Count=1 then
              bScript.Execute(Null);
            bScript.Free;
          end;
      aScript.Next;
    end;
  if Historyrun then
    begin
      aScript.Filter(Data.QuoteField('RUNONHISTORY')+'='+Data.QuoteValue('Y'));
      if (not aScript.EOF) then
        begin
          if aHistory.Count>0 then
            begin
              aHistory := TBaseHistory.Create(nil);
              while not aScript.EOF do
                begin
                  if aScript.FieldByName('STATUS').AsString<>'E' then
                    if aScript.FieldByName('LASTRUN').AsDateTime+(aScript.FieldByName('RUNEVERY').AsInteger/MinsPerDay)<Now() then
                      begin
                        aHistory.Filter(Data.QuoteField('DATE')+'>'+Data.DateTimeToFilter(aScript.FieldByName('LASTRUN').AsDateTime));
                        aHistory.Last;
                        while not aHistory.DataSet.BOF do
                          begin
                            aHistory.Prior;
                            bScript := TBaseScript.CreateEx(nil,aScript.DataModule,aScript.Connection);
                            bScript.Select(aScript.Id.AsVariant);
                            bScript.Open;
                            if bScript.Count=1 then
                              bScript.Execute(VarArrayOf([aHistory.FieldByName('ACTION').AsString,aHistory.FieldByName('DATE').AsDateTime]));
                            bScript.Free;
                          end;
                      end;
                  aScript.Next;
                end;
              aHistory.Free;
            end;
        end
      else Historyrun:=False;
    end;
  aScript.Free;
end;

procedure TBaseScript.SQLConn;
var
  aSQL: String;
begin
  aSQL := ReplaceSQLFunctions(FieldByName('SCRIPT').AsString);
  aDS := TBaseDBModule(DataModule).GetNewDataSet(aSQL,Connection);
end;

destructor TBaseScript.Destroy;
begin
  FLinks.Free;
  FHistory.Free;
  FDataSource.Destroy;
  inherited Destroy;
end;

procedure TBaseScript.DoSetResults(aRes: string);
begin
  Edit;
  FieldByName('LASTRESULT').AsString:=aRes;
  Post;
end;

procedure TBaseScript.DoSetStatus(s: string);
begin
  Edit;
  FieldByName('STATUS').AsString:=s;
  Post;
end;

function TBaseScript.GetTextFieldName: string;
begin
  Result := 'NAME';
end;

function TBaseScript.GetNumberFieldName: string;
begin
  Result := 'SQL_ID';
end;

procedure TBaseScript.InternalWrite(const s: string);
begin
  if Assigned(FWriFunc) then FWriFunc(s);
end;

procedure TBaseScript.InternalWriteln(const s: string);
begin
  if Assigned(FWrFunc) then FWrFunc(s);
end;

procedure TBaseScript.InternalReadln(var s: string);
begin
  if Assigned(FRlFunc) then FRlFunc(s);
end;

function TBaseScript.GetHistory: TBaseHistory;
begin
  Result := FHistory;
end;

constructor TBaseScript.CreateEx(aOwner: TComponent; DM: TComponent;
  aConnection: TComponent; aMasterdata: TDataSet);
begin
  inherited CreateEx(aOwner, DM, aConnection, aMasterdata);
  FDataSource := TDataSource.Create(Self);
  FDataSource.DataSet := DataSet;
  FHistory := TBaseHistory.CreateEx(Self,DM,aConnection,DataSet);
  FLinks := TLinks.CreateEx(Self,DM,aConnection);
end;

procedure TBaseScript.DefineFields(aDataSet: TDataSet);
begin
  with aDataSet as IBaseManageDB do
    begin
      TableName := 'SCRIPTS';
      if Assigned(ManagedFieldDefs) then
        with ManagedFieldDefs do
          begin
            Add('TYPE',ftString,1,False);
            Add('PARENT',ftLargeint,0,False);
            Add('NAME',ftString,60,True);
            Add('STATUS',ftString,3,false);
            Add('SYNTAX',ftString,15,True);
            Add('RUNEVERY',ftInteger,0,False);
            Add('RUNMASHINE',ftString,150,False);
            Add('RUNONHISTORY',ftString,1,False);
            Add('LASTRUN',ftDateTime,0,False);
            Add('SCRIPT',ftMemo,0,false);
            Add('NOTE',ftMemo,0,false);
            Add('FOLDSTATE',ftString,200,false);
            Add('LASTRESULT',ftMemo,0,false);
          end;
      if Assigned(ManagedIndexdefs) then
        with ManagedIndexDefs do
          Add('NAME','NAME',[ixUnique]);
    end;
end;
procedure TBaseScript.FillDefaults(aDataSet: TDataSet);
begin
  FieldByName('SYNTAX').AsString:='Pascal';
  FieldByName('SCRIPT').AsString:='begin'+LineEnding+'  '+LineEnding+'end.';
  inherited FillDefaults(aDataSet);
end;

function TBaseScript.SelectByName(aName: string): Boolean;
begin
  with BaseApplication as IBaseDBInterface do
    with DataSet as IBaseDBFilter do
      begin
        Filter := Data.QuoteField('NAME')+'='+Data.QuoteValue(aName);
      end;
end;

function TBaseScript.Execute(Parameters: Variant): Boolean;
var
  aStartTime: TDateTime;
begin
  Result := False;
  if Count=0 then exit;
  OpenItem(False);
  if (pos(GetSystemName,FieldByName('RUNMASHINE').AsString)>0) or (trim(FieldByName('RUNMASHINE').AsString)='') then
    begin
      DoSetStatus('R');
      Edit;
      FieldByName('LASTRESULT').Clear;
      aStartTime:=Now();
      Post;
      Result := False;
      try
        if lowercase(FieldByName('SYNTAX').AsString) = 'sql' then
          begin
            try
              SQLConn;
              with aDS as IBaseDbFilter do
                DoExecSQL;
              with aDS as IBaseDbFilter do
                DoSetResults('Num Rows Affected: '+IntToStr(NumRowsAffected));
              Result := True;
            except
              on e : Exception do
                begin
                  InternalWriteln('Error:'+e.Message);
                  DoSetResults(e.Message);
                  Result := False;
                end;
            end;
          end;
        if Result then
          begin
            DoSetStatus('N');
            Edit;
            FieldByName('LASTRUN').AsDateTime:=aStartTime;
            Post;
          end
        else
          begin
            DoSetStatus('E');
          end;
      except
      end;
    end
  else
    begin
      DoSetStatus('r');
      while FieldByName('STATUS').AsString='r' do
        begin
          sleep(500);
          DataSet.Refresh;
        end;
    end;
end;

initialization
  Historyrun:=True;
end.

