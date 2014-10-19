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
  Classes, SysUtils, uBaseDbClasses, uBaseDBInterface, db, uPSCompiler,
  uPSC_classes, uPSC_DB, uPSC_dateutils, uPSC_dll, uPSRuntime,
  uPSR_classes, uPSR_DB, uPSR_dateutils, uPSR_dll, uPSUtils,Process,usimpleprocess,
  Utils,variants,UTF8Process,dynlibs;

type
  TWritelnFunc = procedure(const s: string) of object;
  TWriteFunc = procedure(const s: string) of object;
  TReadlnFunc = procedure(var s: string) of object;
  TBaseScript = class;

  { TBaseScript }

  TBaseScript = class(TBaseDBDataset)
    procedure FThreadTerminate(Sender: TObject);
  private
    CompleteOutput : string;
    FRlFunc: TReadlnFunc;
    FWrFunc: TWritelnFunc;
    FWriFunc: TWriteFunc;
    FProcess: TProcessUTF8;
    FRuntime : TPSExec;
    FResults : string;
    aDS: TDataSet;
    FParameters: Variant;
    procedure SQLConn;
    procedure DoSetResults;
    procedure DoSetStatus(s : string);
    procedure DoWriteln;
  protected
    procedure InternalWrite(const s: string);
    procedure InternalWriteln(const s: string);
    procedure InternalReadln(var s: string);
    function InternalParamStr(Param : Integer) : String;
    function InternalParamCount : Integer;

    procedure InternalExec(cmd : string;ShowConsole : Boolean = False);
    function InternalExecActive: Boolean;
    function InternalKill: Boolean;

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
    property Runtime : TPSExec read FRuntime write FRuntime;
    function Execute(Parameters : Variant) : Boolean;
    destructor Destroy;override;
  end;

  function ProcessScripts : Boolean;//process Scripts that must be runned cyclic
  procedure ExtendRuntime(Runtime: TPSExec; ClassImporter: TPSRuntimeClassImporter;Script : TBaseScript);
  function ExtendCompiler(Sender: TPSPascalCompiler; const Name: tbtString): Boolean;

var
  UsesFunctions : string;

implementation
uses uStatistic,uData;
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
        if (aScript.FieldByName('LASTRUN').AsDateTime+(aScript.FieldByName('RUNEVERY').AsInteger/MinsPerDay)<Now()) or (aScript.FieldByName('STATUS').AsString='d') then
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
procedure TBaseScript.InternalExec(cmd: string; ShowConsole: Boolean);
var
  aLine: String;
begin
  FProcess.CommandLine:=cmd;
  FProcess.Options:=[poUsePipes,poNoConsole];
  if ShowConsole then
    FProcess.Options:=[poUsePipes];
  CompleteOutput:='';
  FProcess.ShowWindow:=swoNone;
  try
    FProcess.Execute;
  except
    on e : exception do
      begin
        aLine := 'Error:'+e.Message;
        if Assigned(FRuntime) then
          FRuntime.RunProcPN([aLine],'EXECLINERECEIVED');
      end;
  end;
end;
function TBaseScript.InternalExecActive : Boolean;
var
  ReadSize: LongInt;
  Buffer : string;
  ReadCount: LongInt;
  aLine: String;
begin
  Result := Assigned(FProcess) and FProcess.Active;
  ReadSize := FProcess.Output.NumBytesAvailable;
  while ReadSize>0 do
    begin
      Setlength(Buffer,ReadSize);
      ReadCount := FProcess.Output.Read(Buffer[1], ReadSize);
      CompleteOutput:=CompleteOutput+copy(Buffer,0,ReadCount);
      ReadSize := FProcess.Output.NumBytesAvailable;
    end;
  while pos(#10,CompleteOutput)>0 do
    begin
      aLine := copy(CompleteOutput,0,pos(#10,CompleteOutput)-1);
      if Assigned(FRuntime) then
        FRuntime.RunProcPN([aLine],'EXECLINERECEIVED');
      CompleteOutput:=copy(CompleteOutput,pos(#10,CompleteOutput)+1,length(CompleteOutput));
    end;
end;
function TBaseScript.InternalKill : Boolean;
begin
  Result := Assigned(FProcess);
  if Result then
    begin
      FProcess.Terminate(0);
      while FProcess.Running do InternalExecActive;
      InternalExecActive;
    end;
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

function FixScript(aScript: string): string;
begin
  if pos('uses',lowercase(aScript))>0 then
    begin
      Result := copy(aScript,0,pos('uses',lowercase(aScript))+4);
      aScript := copy(aScript,pos('uses',lowercase(aScript))+4,length(aScript));
      Result := Result+copy(aScript,0,pos(';',aScript)+1);
      aScript := copy(aScript,pos(';',aScript)+1,length(aScript));
      Result := Result+'{$I uses.inc}'+aScript;
    end;
  UsesFunctions:='';
end;

procedure ExtendRuntime(Runtime: TPSExec;
  ClassImporter: TPSRuntimeClassImporter; Script: TBaseScript);
begin
  Runtime.RegisterDelphiMethod(Script,@TBaseScript.InternalExec, 'EXEC', cdRegister);
  Runtime.RegisterDelphiMethod(Script,@TBaseScript.InternalExecActive, 'EXECACTIVE', cdRegister);
  Runtime.RegisterDelphiMethod(Script,@TBaseScript.InternalKill, 'KILL', cdRegister);

  Runtime.RegisterDelphiMethod(Script,@TBaseScript.InternalDataSet, 'DATASET', cdRegister);
  Runtime.RegisterDelphiMethod(Script,@TBaseScript.InternalHistory, 'HISTORY', cdRegister);
  Runtime.RegisterDelphiMethod(Script,@TBaseScript.InternalUserHistory, 'USERHISTORY', cdRegister);

  uPSR_DB.RIRegister_DB(ClassImporter);
  uPSR_dateutils.RegisterDateTimeLibrary_R(Runtime);
  uPSR_dll.RegisterDLLRuntime(Runtime);
end;
type
  aProcT = function : pchar;stdcall;
function ExtendCompiler(Sender: TPSPascalCompiler; const Name: tbtString): Boolean;
var
  aLib: TLibHandle;
  aProc: aProcT;
  Procs : TStringList;
  sProc: String;
  i: Integer;
  aLibName: TbtString;
  tmp: String;
  newUnit: String;
begin
  Result := True;
  try
    if lowercase(Name)='exec' then
      begin
        Sender.AddDelphiFunction('procedure Exec(cmd : string;ShowConsole : Boolean);');
        Sender.AddDelphiFunction('function ExecActive : Boolean;');
        Sender.AddDelphiFunction('function Kill : Boolean;');
      end
    else if lowercase(Name)='promet' then
      begin
        Sender.AddDelphiFunction('function DataSet(SQL : string) : TDataSet;');
        Sender.AddDelphiFunction('function History(Action : string;ParentLink : string;Icon : Integer;ObjectLink : string;Reference : string;Commission: string;Date:TDateTime) : Boolean;');
        Sender.AddDelphiFunction('function UserHistory(Action : string;User   : string;Icon : Integer;ObjectLink : string;Reference : string;Commission: string;Date:TDateTime) : Boolean;');
      end
    else if lowercase(Name)='net' then
      begin
        Sender.AddDelphiFunction('function Get(URL : string) : string;');
        Sender.AddDelphiFunction('function Post(URL,Content : string) : string;');
      end
    else if lowercase(Name)='db' then
      begin
        uPSC_DB.SIRegister_DB(Sender);
      end
    else if lowercase(Name)='dateutils' then
      begin
        uPSC_dateutils.RegisterDateTimeLibrary_C(Sender);
      end
    else
      begin
        Result := False;
        if FileExists(ExtractFilePath(ParamStr(0))+Name+'.dll') then
          aLibName := ExtractFilePath(ParamStr(0))+Name+'.dll';
        if FileExists(ExtractFilePath(ParamStr(0))+Name+'.so') then
          aLibName := ExtractFilePath(ParamStr(0))+Name+'.so';
        if FileExists(ExtractFilePath(ParamStr(0))+Name+'.dylib') then
          aLibName := ExtractFilePath(ParamStr(0))+Name+'.dylib';
        if FileExists(aLibname) then
          begin
            uPSC_dll.RegisterDll_Compiletime(Sender);
            aLib := LoadLibrary(ExtractFilePath(ParamStr(0))+DirectorySeparator+Name+'.dll');
            if aLib <> dynlibs.NilHandle  then
              begin
                aProc := aprocT(dynlibs.GetProcAddress(aLib,'ScriptDefinition'));
                if Assigned(aProc) then
                  begin
                    newUnit := '';//'unit tcsbus;'+LineEnding+'interface';
                    Procs := TStringList.Create;
                    sProc := aProc();
                    Procs.text := sProc;
                    for i := 0 to procs.Count-1 do
                      begin
                        sProc := trim(procs[i]);
                        tmp := copy(sProc,pos(' ',sProc)+1,length(sProc));
                        if pos('(',tmp)>0 then
                          tmp := copy(tmp,0,pos('(',tmp)-1);
                        if pos(':',tmp)>0 then
                          tmp := trim(copy(tmp,0,pos(':',tmp)-1));
                        tmp := StringReplace(sProc,'external','external '''''+tmp+'@'+ExtractFileName(aLibname)+'''''',[]);
                        newUnit := newUnit+LineEnding+tmp;
                      end;
                    //newUnit := newUnit+LineEnding+'end.';

                    Sender.Compile(newUnit);
                    Procs.Free;
                    Result := True;
                  end;
                FreeLibrary(aLib);
              end;
          end;
      end;
  except
    begin
      raise;
      Result := False; // will halt compilation
    end;
  end;
end;

function ExtendICompiler(Sender: TPSPascalCompiler; const Name: tbtString): Boolean;
var
  aRec: TPSType;
begin
  result := False;
  if Name = 'SYSTEM' then
    begin
      Result := True;
      try
        Sender.AddDelphiFunction('procedure Writeln(P1: string);');
        Sender.AddDelphiFunction('procedure Write(P1: string);');
        Sender.AddDelphiFunction('function ParamStr(Param : Integer) : String;');
        Sender.AddDelphiFunction('function ParamCount : Integer;');
      except
        Result := False; // will halt compilation
      end;
      RegisterDll_Compiletime(Sender);
    end
  else
    result := ExtendCompiler(Sender,Name);
end;
procedure ExtendIRuntime(Runtime: TPSExec; ClassImporter: TPSRuntimeClassImporter;Script : TBaseScript);
begin
  Runtime.RegisterDelphiMethod(Script, @TBaseScript.InternalWriteln, 'WRITELN', cdRegister);
  Runtime.RegisterDelphiMethod(Script, @TBaseScript.InternalWrite, 'WRITE', cdRegister);
  Runtime.RegisterDelphiMethod(Script, @TBaseScript.Internalreadln, 'READLN', cdRegister);
  Runtime.RegisterDelphiMethod(Script, @TBaseScript.InternalParamStr, 'PARAMSTR', cdRegister);
  Runtime.RegisterDelphiMethod(Script, @TBaseScript.InternalParamCount, 'PARAMCOUNT', cdRegister);
  RegisterDLLRuntime(Runtime);
  ExtendRuntime(Runtime,ClassImporter,Script);
end;

procedure TBaseScript.SQLConn;
var
  aSQL: String;
begin
  aSQL := ReplaceSQLFunctions(FieldByName('SCRIPT').AsString);
  aDS := TBaseDBModule(DataModule).GetNewDataSet(aSQL,Connection);
end;

procedure TBaseScript.FThreadTerminate(Sender: TObject);
begin
  InternalWriteln(FResults);
end;

destructor TBaseScript.Destroy;
begin
  if Assigned(FProcess) then
    begin
      if Assigned(FProcess) then
        FreeAndNil(FProcess);
      if Assigned(FRuntime) then
        FRuntime.Stop;
    end;
  inherited Destroy;
end;

procedure TBaseScript.DoSetResults;
begin
  Edit;
  FieldByName('LASTRESULT').AsString:=FieldByName('LASTRESULT').AsString+lineending+FResults;
  Post;
end;

procedure TBaseScript.DoSetStatus(s: string);
begin
  Edit;
  FieldByName('STATUS').AsString:=s;
  Post;
end;

procedure TBaseScript.DoWriteln;
begin

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

function TBaseScript.InternalParamStr(Param: Integer): String;
begin
  Result:='';
  if Param<VarArrayHighBound(FParameters,1) then
    Result:=FParameters[Param];
end;

function TBaseScript.InternalParamCount: Integer;
begin
  Result := VarArrayHighBound(FParameters,1);
end;

constructor TBaseScript.Create(aOwner: TComponent; DM: TComponent;
  aConnection: TComponent; aMasterdata: TDataSet);
begin
  inherited Create(aOwner, DM, aConnection, aMasterdata);
  FProcess := TProcessUTF8.Create(nil);
  FProcess.ShowWindow:=swoNone;
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
var
  Compiler: TPSPascalCompiler;
  Bytecode: tbtString;
  i: Integer;
  ClassImporter: TPSRuntimeClassImporter;
begin
  FParameters := Parameters;
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
            FResults:='Num Rows Affected: '+IntToStr(NumRowsAffected);
          DoWriteln;
          DoSetResults;
          Result := True;
        except
          on e : Exception do
            begin
              FResults := e.Message;
              DoWriteln;
              DoSetResults;
              Result := False;
            end;
        end;
      end
    else if lowercase(FieldByName('SYNTAX').AsString) = 'pascal' then
      begin
        Compiler:= TPSPascalCompiler.Create;
        Compiler.OnUses:= @ExtendICompiler;
        try
          Result:= Compiler.Compile(FieldByName('SCRIPT').AsString) and Compiler.GetOutput(Bytecode);
          FResults:='';
          for i:= 0 to Compiler.MsgCount - 1 do
            if Length(FResults) = 0 then
              FResults:= Compiler.Msg[i].MessageToString
            else
              FResults:= FResults + #13#10 + Compiler.Msg[i].MessageToString;
          if FResults<>'' then
            DoSetResults;
        finally
          Compiler.Free;
        end;
        if Result then
          begin
            FRuntime:= TPSExec.Create;
            ClassImporter:= TPSRuntimeClassImporter.CreateAndRegister(FRuntime, false);
            try
              ExtendIRuntime(FRuntime, ClassImporter, Self);
              Result:= FRuntime.LoadData(Bytecode)
                    and FRuntime.RunScript
                    and (FRuntime.ExceptionCode = erNoError);
              if not Result then
                FResults:= PSErrorToString(FRuntime.LastEx, '');
            finally
              ClassImporter.Free;
              FreeAndNil(FRuntime);
            end;
            if FResults<>'' then
              DoSetResults;
            if Assigned(FProcess) then InternalKill;
            Result := True;
          end;
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
end;

end.

