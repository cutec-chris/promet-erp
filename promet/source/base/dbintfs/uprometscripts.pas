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
  Classes, SysUtils, uBaseDbClasses, uBaseDBInterface, db, genpascalscript
  ,uPSRuntime,uPSCompiler,uPSUtils;

type
  { TBaseScript }

  TBaseScript = class(TBaseDBDataset)
    procedure DataSetAfterScroll(ADataSet: TDataSet);
    function TPascalScriptUses(Sender: TPascalScript; const aName: tbtString
      ): Boolean;
  private
    aDS: TDataSet;
    FRlFunc: TReadlnFunc;
    FScript: TScript;
    FSlFunc: TSleepFunc;
    FWrFunc: TWritelnFunc;
    FWriFunc: TWriteFunc;
    FDataSource: TDataSource;
    procedure SQLConn;
    procedure DoSetResults;
    procedure DoSetStatus(s : string);
  protected
    procedure InternalWrite(const s: string);
    procedure InternalWriteln(const s: string);
    procedure InternalReadln(var s: string);

    function InternalDataSet(SQL : string) : TDataSet;
    function InternalHistory(Action: string; ParentLink: string; Icon: Integer=0;
      ObjectLink: string=''; Reference: string='';Commission: string='';Date:TDateTime = 0) : Boolean;
    function InternalUserHistory(Action: string;UserName: string; Icon: Integer; ObjectLink: string;
      Reference: string; Commission: string; Date: TDateTime): Boolean;
  public
    constructor Create(aOwner: TComponent; DM: TComponent;
      aConnection: TComponent=nil; aMasterdata: TDataSet=nil); override;
    procedure DefineFields(aDataSet: TDataSet); override;
    procedure FillDefaults(aDataSet: TDataSet); override;
    property Write : TWriteFunc read FWriFunc write FWriFunc;
    property Writeln : TWritelnFunc read FWrFunc write FWRFunc;
    property Readln : TReadlnFunc read FRlFunc write FRlFunc;
    property Sleep : TSleepFunc read FSlFunc write FSlFunc;
    property Script : TScript read FScript;
    function Execute(Parameters : Variant) : Boolean;
    destructor Destroy;override;
  end;

  function ProcessScripts : Boolean;//process Scripts that must be runned cyclic

implementation
uses uStatistic,uData,httpsend,Utils,variants;
function ProcessScripts : Boolean;//process Scripts that must be runned cyclic Result shows that it should be runned faster (debug)
var
  aScript: TBaseScript;
  aHistory: TBaseHistory;
  bScript: TBaseScript;
begin
  Result:=false;
  aScript := TBaseScript.Create(nil,Data);
  aScript.Filter(Data.QuoteField('RUNEVERY')+'>'+Data.QuoteValue('0')+' OR '+Data.QuoteField('STATUS')+'='+Data.QuoteValue('d'));
  while not aScript.EOF do
    begin
      if (aScript.FieldByName('STATUS').AsString<>'S') and ((aScript.FieldByName('RUNMASHINE').AsString='') or (pos(GetSystemName,aScript.FieldByName('RUNMASHINE').AsString)>0)) then
        if (aScript.FieldByName('LASTRUN').AsDateTime+(aScript.FieldByName('RUNEVERY').AsInteger/MinsPerDay)<Now()) or (aScript.FieldByName('STATUS').AsString='d') or (aScript.FieldByName('STATUS').AsString='r') then
          begin
            bScript := TBaseScript.Create(nil,aScript.DataModule,aScript.Connection);
            bScript.Select(aScript.Id.AsVariant);
            bScript.Open;
            Result := (aScript.FieldByName('STATUS').AsString='d');
            if bScript.Count=1 then
              bScript.Execute(Null);
            bScript.Free;
          end;
      aScript.Next;
    end;
  aScript.Filter(Data.QuoteField('RUNONHISTORY')+'='+Data.QuoteValue('Y'));
  if (not aScript.EOF) then
    begin
      aHistory := TBaseHistory.Create(nil,Data);
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
                    bScript := TBaseScript.Create(nil,aScript.DataModule,aScript.Connection);
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
  aScript.Free;
end;
function TBaseScript.InternalDataSet(SQL: string): TDataSet;
begin
  Result := TBaseDBModule(DataModule).GetNewDataSet(SQL,Connection);
end;
function TBaseScript.InternalHistory(Action: string; ParentLink: string;
  Icon: Integer; ObjectLink: string; Reference: string; Commission: string;
  Date: TDateTime): Boolean;
var
  aHistory: TBaseHistory;
  aDataSetClass: TBaseDBDatasetClass;
  aDataSet: TBaseDBDataset;
begin
  Result := False;
  if TBaseDBModule(DataModule).DataSetFromLink(ParentLink,aDataSetClass) then
    begin
      aDataSet := aDataSetClass.Create(nil,DataModule,Connection);
      TBaseDbList(aDataSet).SelectFromLink(ParentLink);
      aDataSet.Open;
      if aDataSet.Count>0 then
        begin
          aHistory := TBaseHistory.Create(nil,DataModule,Connection);
          aHistory.AddItem(aDataSet.DataSet,Action,ObjectLink,Reference,nil,Icon,Commission);
          aHistory.Free;
          result := True;
        end;
      aDataSet.Free;
    end;
end;
function TBaseScript.InternalUserHistory(Action: string; UserName: string;
  Icon: Integer; ObjectLink: string; Reference: string; Commission: string;
  Date: TDateTime): Boolean;
begin
  Result := False;
  if Data.Users.Locate('NAME',UserName,[loCaseInsensitive]) then
    begin
      Data.Users.History.AddItem(Data.Users.DataSet,Action,ObjectLink,Reference,nil,Icon,Commission);
      Result := True;
    end;
end;

procedure TBaseScript.DataSetAfterScroll(ADataSet: TDataSet);
begin
  TPascalScript(FScript).OnUses:=@TPascalScriptUses;
  FScript.Source:=FieldByName('SCRIPT').AsString;
end;

function TBaseScript.TPascalScriptUses(Sender: TPascalScript;
  const aName: tbtString): Boolean;
begin
  if aName = 'SYSTEM' then
    begin
      Result := True;
      try
        Sender.AddMethod(Self,@TBaseScript.InternalWriteln,'procedure Writeln(P1: string);');
        Sender.AddMethod(Self,@TBaseScript.InternalWrite,'procedure Write(P1: string);');
      except
        Result := False; // will halt compilation
      end;
    end
  else if aName = 'PROMET' then
    begin
      Result := True;
      try
        Sender.InternalUses(Sender.Compiler,'db');
        Sender.InternalUses(Sender.Compiler,'dateutils');
        Sender.AddMethod(Self,@TBaseScript.InternalDataSet,'function DataSet(SQL : string) : TDataSet;');
        Sender.AddMethod(Self,@TBaseScript.InternalHistory,'function History(Action : string;ParentLink : string;Icon : Integer;ObjectLink : string;Reference : string;Commission: string;Date:TDateTime) : Boolean;');
        Sender.AddMethod(Self,@TBaseScript.InternalUserHistory,'function UserHistory(Action : string;User   : string;Icon : Integer;ObjectLink : string;Reference : string;Commission: string;Date:TDateTime) : Boolean;');
      except
        Result := False; // will halt compilation
      end;
    end
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
  fScript.Destroy;
  FDataSource.Destroy;
  inherited Destroy;
end;

procedure TBaseScript.DoSetResults;
begin
  Edit;
  FieldByName('LASTRESULT').AsString:=Script.Results;
  Post;
end;

procedure TBaseScript.DoSetStatus(s: string);
begin
  Edit;
  FieldByName('STATUS').AsString:=s;
  Post;
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

constructor TBaseScript.Create(aOwner: TComponent; DM: TComponent;
  aConnection: TComponent; aMasterdata: TDataSet);
begin
  inherited Create(aOwner, DM, aConnection, aMasterdata);
  FScript := TPascalScript.Create;
  FDataSource := TDataSource.Create(Self);
  FDataSource.DataSet := DataSet;
  DataSet.AfterScroll:=@DataSetAfterScroll;
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
function TBaseScript.Execute(Parameters: Variant): Boolean;
begin
  if (pos(GetSystemName,FieldByName('RUNMASHINE').AsString)>0) or (trim(FieldByName('RUNMASHINE').AsString)='') then
    begin
      DoSetStatus('R');
      Edit;
      FieldByName('LASTRESULT').Clear;
      FieldByName('LASTRUN').AsDateTime:=Now();
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
                Script.Results:='Num Rows Affected: '+IntToStr(NumRowsAffected);
              DoSetResults;
              Result := True;
            except
              on e : Exception do
                begin
                  Script.Results := e.Message;
                  DoSetResults;
                  Result := False;
                end;
            end;
          end
        else if lowercase(FieldByName('SYNTAX').AsString) = 'pascal' then
          begin
            Result := FScript.Execute(Parameters);
            if not Result then
              DoSetResults;
          end;
        if Result then
          begin
            DoSetStatus('N');
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
  LoadedLibs := TList.Create;
finalization
  LoadedLibs.Clear;
  LoadedLibs.Free;
end.

