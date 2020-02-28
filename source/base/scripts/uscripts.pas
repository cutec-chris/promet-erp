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
unit uscripts;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uBaseDbClasses, db, Utils
  ,uBaseDatasetInterfaces2,uBaseERPDBClasses,contnrs,genscript;

type

  { TScriptLinks }

  TScriptLinks = class(TLinks)
  public
    procedure FillDefaults; override;
  end;

  { TBaseScript }

  TBaseScript = class(TBaseERPList,IBaseHistory)
    procedure aScriptCheckModule(Sender: TObject);
  private
    FNote: string;
    FType,FName,FVersion,FStatus,FSyntax,FRunMashine,FSource,FFoldState : string;
    FPriority : Integer;
    FParent : Int64;
    FActive : Boolean;
    FActObject: TBaseDBDataset;
    FDWrFunc: TStrOutFunc;
    FHistory: TBaseHistory;
    FLinks: TScriptLinks;
    FDataSource: TDataSource;
    FRlFunc: TStrInFunc;
    FWrFunc: TStrOutFunc;
    FWriFunc: TStrOutFunc;
    FScript : TScript;
    FSelectedName : variant;
    FStateChange: TNotifyEvent;
    FStatusProblems: TStringList;
    function GetScript: TScript;
    function GetVersion: TField;
    procedure SetDWRFunc(AValue: TStrOutFunc);
    procedure SetRlFunc(AValue: TStrInFunc);
    procedure SetWRFunc(AValue: TStrOutFunc);
    procedure SetWriFunc(AValue: TStrOutFunc);
    procedure ConnectEvents;
    procedure DoSetStatus(s : string);
  protected
    function GetTextFieldName: string;override;
    function GetDescriptionFieldName: string;override;
    function GetNumberFieldName : string;override;
    function GetHistory: TBaseHistory;
    function GetStatusFieldName: string;override;
  public
    constructor Create(aOwner: TPersistent); override;
    class function MapField(aField: string): string; override;
    class function GetRealTableName: string; override;
    procedure FillDefaults; override;
    property Script : TScript read GetScript;
    procedure ResetScript;
    procedure CheckStatus(Output: TStringList; Module, aStatus: string);
    function CheckScript : string;
    function Execute(Parameters : Variant;Debug : Boolean = False) : Boolean;virtual;
    property Write : TStrOutFunc read FWriFunc write SetWriFunc;
    property Writeln : TStrOutFunc read FWrFunc write SetWRFunc;
    property Debugln : TStrOutFunc read FDWrFunc write SetDWRFunc;
    property Readln : TStrInFunc read FRlFunc write SetRlFunc;
    property History : TBaseHistory read FHistory;
    property Links : TScriptLinks read FLinks;
    function Copy(aNewVersion : Variant) : Boolean;
    procedure OpenItem(AccHistory: Boolean=True); override;
    property ActualObject : TBaseDBDataset read FActObject write FActObject;
    function Versionate(aNewversion : Variant;aMakeActive : Boolean = True) : Boolean;
    function Compile : Boolean;
    destructor Destroy;override;
    property OnStateChange : TNotifyEvent read FStateChange write FStateChange;
    property StatusProblems : TStringList read FStatusProblems;
  published
    property Typ: string index 1 read FType write FType;
    property Parent: Int64 read FParent write FParent;
    property Name: string index 60 read FName write FName;
    property Status: string index 4 read FStatus write FStatus;
    property Version: string index 25 read FVersion write FVersion;
    property Active: Boolean read FActive write FActive;
    property Syntax: string index 15 read FSyntax write FSyntax;
    property Priority: Integer read FPriority write FPriority;
    property RunMashine: string index 150 read FRunMashine write FRunMashine;
    //property RunOnHistory: Boolean read FRunOnHistory write FRunOnHistory;
    property Source: string read FSource write FSource;
    property Note: string read FNote write FNote;
    property FoldState: string index 250 read FFoldState write FFoldState;
  end;

  TSQLScript = class(TScript)
  private
    aDS: TDataSet;
    procedure SQLConn;
    procedure DoSetResults(aRes : string);
  protected
    function GetTyp: string; override;
    function GetStatus: TScriptStatus; override;
  public
    function Execute(aParameters: Variant;Debug : Boolean = false): Boolean; override;
  end;

  function ProcessScripts : Boolean;//process Scripts that must be runned cyclic
  procedure RegisterScriptType(aType : TScriptClass);

var
  Historyrun : Boolean;
  ScriptTypes : TClassList;
  FStatusCache : TStringList;

implementation
uses uData,httpsend,variants,uIntfStrConsts;
resourcestring
  strScripts                 = 'Scripte';

procedure RegisterScriptType(aType : TScriptClass);
begin
  ScriptTypes.Add(aType);
end;

function ProcessScripts : Boolean;//process Scripts that must be runned cyclic Result shows that it should be runned faster (debug)
var
  aScript: TBaseScript;
  aHistory: TBaseHistory;
  bScript: TBaseScript;
begin
  {
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
  }
end;

{ TScriptLinks }

procedure TScriptLinks.FillDefaults;
begin
  inherited FillDefaults;
  RRef_ID:=(Parent as TBaseScript).SQL_ID;
end;

procedure TSQLScript.SQLConn;
var
  aSQL: String;
begin
  aSQL := Source;//ReplaceSQLFunctions(Source);
  //aDS := TBaseDBModule(Data).GetNewDataSet(aSQL);
end;

procedure TSQLScript.DoSetResults(aRes: string);
begin
  Results:=aRes;
end;

function TSQLScript.GetTyp: string;
begin
  Result := 'SQL';
end;

function TSQLScript.GetStatus: TScriptStatus;
begin
  Result:=inherited GetStatus;
end;

function TSQLScript.Execute(aParameters: Variant; Debug: Boolean): Boolean;
begin
  {
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
        if Assigned(Write) then
          Write('Error:'+e.Message);
        DoSetResults(e.Message);
        Result := False;
      end;
  end;
  }
end;

procedure TBaseScript.ConnectEvents;
begin
  if Assigned(GetScript) then
    begin
      GetScript.Write:=Write;
      GetScript.Readln:=Readln;
      GetScript.Writeln:=Writeln;
      GetScript.Debugln:=Debugln;
    end;
end;

procedure TBaseScript.DoSetStatus(s: string);
begin
  Edit;
  FieldByName('STATUS').AsString:=s;
  Post;
end;

function TBaseScript.GetVersion: TField;
begin
  Result := FieldByName('VERSION');
end;

procedure TBaseScript.SetDWRFunc(AValue: TStrOutFunc);
begin
  if FDWrFunc=AValue then Exit;
  FDWrFunc:=AValue;
  ConnectEvents;
end;

procedure TBaseScript.SetRlFunc(AValue: TStrInFunc);
begin
  if FRlFunc=AValue then Exit;
  FRlFunc:=AValue;
  ConnectEvents;
end;

procedure TBaseScript.SetWRFunc(AValue: TStrOutFunc);
begin
  if FWrFunc=AValue then Exit;
  FWrFunc:=AValue;
  ConnectEvents;
end;

procedure TBaseScript.SetWriFunc(AValue: TStrOutFunc);
begin
  if FWriFunc=AValue then Exit;
  FWriFunc:=AValue;
  ConnectEvents;
end;

procedure TBaseScript.aScriptCheckModule(Sender: TObject);
begin
  if Assigned(FStatusProblems) then
    CheckStatus(FStatusProblems,(Sender as TBaseScript).Script.Name,(Sender as TBaseScript).Status);
end;

function TBaseScript.GetScript: TScript;
var
  aScript: TScript = nil;
  i: Integer;
  aType: String;
begin
  aScript:=nil;
  if (not Assigned(FScript)) and (Assigned(FieldByName('SYNTAX'))) then
    begin
      for i := 0 to ScriptTypes.Count-1 do
        begin
          aScript := TScript(ScriptTypes[i].Create);
          if Uppercase(aScript.Typ)<>Uppercase(FieldByName('SYNTAX').AsString) then
            FreeAndNil(aScript);
          if Assigned(aScript) then break;
        end;
      if Assigned(aScript) then
        begin
          aScript.Init;
          aScript.Source:=FieldByName('SCRIPT').AsString;
          aScript.Name:=FieldByName('NAME').AsString;
          aScript.OnCheckModule:=@aScriptCheckModule;
          FScript:=aScript;
          FScript.Parent:=Self;
          FScript.Id:=SQL_ID;
          FScript.Version:=Version;
          ConnectEvents;
        end;
    end;
  Result := FScript;
end;

destructor TBaseScript.Destroy;
begin
  FStatusProblems.Free;
  ResetScript;
  //FLinks.Free;
  //FHistory.Free;
  //FDataSource.Destroy;
  inherited Destroy;
end;

function TBaseScript.GetTextFieldName: string;
begin
  Result := 'NAME';
end;

function TBaseScript.GetDescriptionFieldName: string;
begin
  Result:='SCRIPT';
end;

function TBaseScript.GetNumberFieldName: string;
begin
  Result := 'NAME';
end;

function TBaseScript.GetHistory: TBaseHistory;
begin
  Result := FHistory;
end;

function TBaseScript.GetStatusFieldName: string;
begin
  Result:='STATUS';
end;

constructor TBaseScript.Create(aOwner: TPersistent);
begin
  inherited Create(aOwner);
  FStatusProblems := TStringList.Create;
  FSelectedName := Null;
  FHistory := TBaseHistory.Create(Self);
  FLinks := TScriptLinks.Create(Self);
end;

class function TBaseScript.MapField(aField: string): string;
begin
  Result:=inherited MapField(aField);
  if Result = 'Source' then
    Result := 'SCRIPT'
  else if Result = 'Typ' then
    Result := 'TYPE'
  ;
end;

class function TBaseScript.GetRealTableName: string;
begin
  Result:='SCRIPTS';
end;

procedure TBaseScript.FillDefaults;
begin
  inherited FillDefaults;
  Syntax :='Pascal';
  Source:='begin'+LineEnding+'  '+LineEnding+'end.';
  Active := True;
  if FSelectedName<>Null then
    FieldByName('NAME').AsVariant:=FSelectedName;
end;

procedure TBaseScript.ResetScript;
begin
  try
    if Assigned(FScript) then
      begin
        FScript.OnRunLine:=nil;
        FreeAndNil(FScript);
      end;
  except
    {
    on e : Exception do
      if Assigned(BaseApplication) then with BaseApplication as IBaseApplication do
        Warning('Reset Script Internal Error '+FieldByName('NAME').AsString+':'+e.Message);
    }
  end;
end;

procedure TBaseScript.CheckStatus(Output: TStringList; Module,aStatus: string);
begin
  {
  if FStatusCache.Count=0 then
    begin
      Data.States.Filter(Data.QuoteField('TYPE')+'='+Data.QuoteValue('S'));
      Data.States.First;
      while not Data.States.EOF do
        begin
          if Data.States.FieldByName('ACTIVE').AsString='N' then
            FStatusCache.Values[Data.States.FieldByName('STATUS').AsString]:=Data.States.FieldByName('STATUSNAME').AsString;
          Data.States.Next;
        end;
    end;
  if FStatusCache.Values[aStatus]<>'' then
    Output.Add(Module+'='+FStatusCache.Values[aStatus]);
  }
end;

function TBaseScript.CheckScript: string;
begin
  Result := '';
  if Assigned(GetScript) then
    if FScript is TByteCodeScript then
      with FScript as TByteCodeScript do
        begin
          Compile;
        end;
end;

function TBaseScript.Execute(Parameters: Variant; Debug: Boolean): Boolean;
var
  aStartTime: TDateTime;
begin
  Result := False;
  try
    if Assigned(GetScript) then
      Result := FScript.Execute(Parameters,Debug);
  except
    Result := False;
  end;
end;

function TBaseScript.Copy(aNewVersion: Variant): Boolean;
var
  bScript: TBaseScript;
begin
  {
  Result := True;
  bScript := TBaseScript.CreateEx(Self,DataModule,Self.Connection);
  try
    try
      bScript.Select(Id.AsVariant);
      bScript.Append;
      bScript.DirectAssign(Self);
      if aNewVersion <> bScript.Version.AsVariant then
        bScript.Version.AsVariant:=aNewVersion;
      bScript.CascadicPost;
      Self.Select(bScript.Id.AsVariant);
      Self.Open;
    except
      Result := False;
    end;
  finally
    bScript.Free;
  end;
  DataSet.Edit;
  Change;
  }
end;

procedure TBaseScript.OpenItem(AccHistory: Boolean);
var
  //aObj: TObjects;
  aID: String;
  aFilter: String;
begin
  {
  if Self.Count=0 then exit;
  try
    try
      aObj := TObjects.Create(nil);
      if (DataSet.State<>dsInsert) then
        begin
          if not Data.TableExists(aObj.TableName) then
            begin
              aObj.CreateTable;
              aObj.Free;
              aObj := TObjects.CreateEx(nil,Data,nil,DataSet);
            end;
          with aObj.DataSet as IBaseDBFilter do
            begin
              aFilter :=  Data.QuoteField(aObj.TableName)+'.'+Data.QuoteField('SQL_ID')+'='+Data.QuoteValue(Self.Id.AsVariant);
              Filter := aFilter;
              Limit := 0;
            end;
          aObj.Open;
          if aObj.Count=0 then
            begin
              aObj.Insert;
              aObj.Text.AsString := Self.Text.AsString;
              aObj.FieldByName('SQL_ID').AsVariant:=Self.Id.AsVariant;
              if aObj.Number.AsString<>Self.Number.AsString then
                begin
                  aObj.Edit;
                  aObj.Number.AsString := Self.FieldByName('NAME').AsString;
                end;
              if Assigned(Self.Matchcode) then
                aObj.Matchcode.AsString := Self.Matchcode.AsString;
              if Assigned(Self.Status) then
                aObj.Status.AsString := Self.Status.AsString;
              aObj.Number.AsVariant:=Self.Number.AsVariant;
              aObj.FieldByName('LINK').AsString:=Data.BuildLink(Self.DataSet);
              aObj.FieldByName('ICON').AsInteger:=Data.GetLinkIcon(Data.BuildLink(Self.DataSet),True);
              aObj.FieldByName('VERSION').AsString:=Self.FieldByName('VERSION').AsString;
              aObj.Post;
              Self.GenerateThumbnail;
            end
          else //Modify existing
            begin
              if CanEdit then
                aObj.Edit;
              while aObj.Count>1 do
                aObj.Delete;
              if aObj.Text.AsString<>Self.Text.AsString then
                begin
                  aObj.Edit;
                  aObj.Text.AsString := Self.Text.AsString;
                end;
              if aObj.Number.AsString<>Self.Number.AsString then
                begin
                  aObj.Edit;
                  aObj.Number.AsString := Self.FieldByName('NAME').AsString;
                end;
              if Assigned(Self.Status) and (aObj.Status.AsString<>Self.Status.AsString) then
                begin
                  aObj.Edit;
                  aObj.Status.AsString := Self.Status.AsString;
                end;
              if Assigned(Self.Matchcode) and (aObj.Matchcode.AsString<>Self.Matchcode.AsString) then
                begin
                  aObj.Edit;
                  aObj.Matchcode.AsString := Self.Matchcode.AsString;
                end;
              if aObj.FieldByName('LINK').AsString<>Data.BuildLink(Self.DataSet) then
                begin
                  aObj.Edit;
                  aObj.FieldByName('LINK').AsString:=Data.BuildLink(Self.DataSet);
                end;
              if aObj.FieldByName('VERSION').AsString<>Self.FieldByName('VERSION').AsString then
                begin
                  aObj.Edit;
                  aObj.FieldByName('VERSION').AsString:=Self.FieldByName('VERSION').AsString;
                end;
              if aObj.CanEdit then
                aObj.Post;
            end;
        end;
    finally
      aObj.Free;
    end;
  except
  end;
  }
end;

function TBaseScript.Versionate(aNewversion: Variant; aMakeActive: Boolean
  ): Boolean;
var
  bScript: TBaseScript;
begin
  {
  Result := Copy(aNewversion);
  if aMakeActive then
    begin
      bScript := TBaseScript.CreateEx(Self,DataModule,Self.Connection);
      try
        try
          bScript.SelectByName(Number.AsString);
          bScript.Open;
          while not bScript.EOF do
            begin
              bScript.Edit;
              if bScript.Id.AsVariant<>Self.Id.AsVariant then
                bScript.FieldByName('ACTIVE').AsString:='N'
              else
                bScript.FieldByName('ACTIVE').AsString:='Y';
              bScript.Post;
              bScript.Next;
            end;
        except
          Result := False;
        end;
      finally
        bScript.Free;
      end;
    end
  else
    begin
      Edit;
      FieldByName('ACTIVE').AsString:='N';
      Post;
    end;
  }
end;

function TBaseScript.Compile: Boolean;
begin
  FStatusProblems.Clear;
  Result := True;
  if Assigned(GetScript) and (FScript is TByteCodeScript) then
    begin
      if Assigned(Script.OnCheckModule) then
        Script.OnCheckModule(Self);
      Result := TByteCodeScript(FScript).Compile;
    end;
end;

initialization
  Historyrun:=True;
  ScriptTypes:=TClassList.Create;
  RegisterScriptType(TSQLScript);
  RegisterdataSetClass('SCRIPTS',TBaseScript);
  FStatusCache := TStringList.Create;
finalization
  FStatusCache.Free;
  ScriptTypes.Free;
end.

