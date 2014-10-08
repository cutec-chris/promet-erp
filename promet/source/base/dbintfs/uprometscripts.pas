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
  uPSC_classes, uPSC_dateutils, uPSC_dll, uPSRuntime,
  uPSR_classes, uPSR_DB, uPSR_dll, uPSUtils,Process,usimpleprocess,
  Utils,variants;

type
  TWritelnFunc = procedure(const s: string) of object;
  TWriteFunc = procedure(const s: string) of object;
  TReadlnFunc = procedure(var s: string) of object;
  TBaseScript = class;

  { TScriptThread }

  TScriptThread = class(TThread)
  private
    Params : Variant;
    FParentDS : TBaseScript;
    FStatus : string;
    FResults : string;
    FScript : string;
    FSyntax: String;
    aConnection: TComponent;
    aDS: TDataSet;
    CompleteOutput : string;
    procedure SetStatus;
    procedure SetResults;
    procedure SQLConn;
    procedure SQLConnF;
    procedure GetScript;
    procedure DoWriteln;
  public
    FProcess : TProcess;
    FRuntime : TPSExec;
    procedure InternalExec(cmd : string);
    function InternalExecActive: Boolean;
    function InternalKill: Boolean;
    constructor Create(Parameters : Variant;aParent : TBaseScript);
    procedure Execute; override;
    destructor Destroy; override;
  end;

  { TBaseScript }

  TBaseScript = class(TBaseDBDataset)
    procedure FThreadTerminate(Sender: TObject);
  private
    CompleteOutput : string;
    FRlFunc: TReadlnFunc;
    FWrFunc: TWritelnFunc;
    FWriFunc: TWriteFunc;
    FThread: TScriptThread;
  protected
    procedure InternalWrite(const s: string);
    procedure InternalWriteln(const s: string);
    procedure InternalReadln(var s: string);
  public
    constructor Create(aOwner: TComponent; DM: TComponent;
      aConnection: TComponent=nil; aMasterdata: TDataSet=nil); override;
    procedure DefineFields(aDataSet: TDataSet); override;
    procedure FillDefaults(aDataSet: TDataSet); override;
    property Write : TWriteFunc read FWriFunc write FWriFunc;
    property Writeln : TWritelnFunc read FWrFunc write FWRFunc;
    property Readln : TReadlnFunc read FRlFunc write FRlFunc;
    property Thread : TScriptThread read FThread;
    function Execute(Parameters : Variant) : Boolean;
  end;

  function ProcessScripts : Boolean;//process Scripts that must be runned cyclic
  procedure ExtendRuntime(Runtime: TPSExec; ClassImporter: TPSRuntimeClassImporter;ScriptThread : TScriptThread);
  function ExtendCompiler(Sender: TPSPascalCompiler; const Name: tbtString): Boolean;

implementation
uses uStatistic,uData;
function ProcessScripts : Boolean;//process Scripts that must be runned cyclic
var
  aScript: TBaseScript;
  aHistory: TBaseHistory;
  bScript: TBaseScript;
begin
  aScript := TBaseScript.Create(nil,Data);
  aScript.Filter(Data.QuoteField('RUNEVERY')+'>'+Data.QuoteValue('0'));
  while not aScript.EOF do
    begin
      if (aScript.FieldByName('STATUS').AsString<>'E') and ((aScript.FieldByName('RUNMASHINE').AsString='') or (pos(GetSystemName,aScript.FieldByName('RUNMASHINE').AsString)>0)) then
        if aScript.FieldByName('LASTRUN').AsDateTime+(aScript.FieldByName('RUNEVERY').AsInteger/MinsPerDay)<Now() then
          begin
            bScript := TBaseScript.Create(nil,aScript.DataModule,aScript.Connection);
            bScript.Select(aScript.Id.AsVariant);
            bScript.Open;
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
procedure TScriptThread.InternalExec(cmd : string);
begin
  FProcess := TProcess.Create(nil);
  FProcess.CommandLine:=cmd;
  CompleteOutput:='';
  FProcess.Options:=FProcess.Options+[poUsePipes,poNoConsole];
  FProcess.Execute;
end;
function TScriptThread.InternalExecActive : Boolean;
var
  ReadSize: DWord;
  Buffer : string[127];
  ReadCount: LongInt;
  aLine: String;
begin
  Result := Assigned(FProcess) and FProcess.Active;
  if Assigned(FProcess) then
    begin
      ReadSize := FProcess.Output.NumBytesAvailable;
      while ReadSize>0 do
        begin
          if ReadSize > SizeOf(Buffer) then
            ReadSize := SizeOf(Buffer);
          ReadCount := FProcess.Output.Read(Buffer[0], ReadSize);
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
      if not FProcess.Active then
        FreeAndNil(FProcess);
    end;
end;
function TScriptThread.InternalKill : Boolean;
begin
  Result := Assigned(FProcess);
  if Result then
    begin
      FProcess.Terminate(0);
      while Assigned(FProcess) do InternalExecActive;
    end;
end;
procedure ExtendRuntime(Runtime: TPSExec; ClassImporter: TPSRuntimeClassImporter;ScriptThread : TScriptThread);
begin
  Runtime.RegisterDelphiMethod(ScriptThread,@TScriptThread.InternalExec, 'EXEC', cdRegister);
  Runtime.RegisterDelphiMethod(ScriptThread,@TScriptThread.InternalExecActive, 'EXECACTIVE', cdRegister);
  Runtime.RegisterDelphiMethod(ScriptThread,@TScriptThread.InternalKill, 'KILL', cdRegister);
end;
function ExtendCompiler(Sender: TPSPascalCompiler; const Name: tbtString): Boolean;
begin
  Result := True;
  try
    Sender.AddDelphiFunction('procedure Exec(cmd : string);');
    Sender.AddDelphiFunction('function ExecActive : Boolean;');
    Sender.AddDelphiFunction('function Kill : Boolean;');
  except
    Result := False; // will halt compilation
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
  RegisterDLLRuntime(Runtime);
  ExtendRuntime(Runtime,ClassImporter,Script.Thread);
end;

{ TScriptThread }

procedure TScriptThread.SetStatus;
begin
  if not Assigned(FParentDS) then exit;
  FParentDS.Edit;
  FParentDS.FieldByName('STATUS').AsString:=FStatus;
  FParentDS.Post;
end;

procedure TScriptThread.SetResults;
begin
  FParentDS.Edit;
  FParentDS.FieldByName('LASTRESULT').AsString:=FResults;
  FParentDS.Post;
end;

procedure TScriptThread.SQLConn;
var
  aSQL: String;
begin
  aConnection := TBaseDBModule(FParentDS.DataModule).GetNewConnection;
  aSQL := ReplaceSQLFunctions(FParentDS.FieldByName('SCRIPT').AsString);
  aDS := TBaseDBModule(FParentDS.DataModule).GetNewDataSet(aSQL);
end;

procedure TScriptThread.SQLConnF;
begin
  aConnection.Free;
end;

procedure TScriptThread.GetScript;
begin
  FScript:=FParentDS.FieldByName('SCRIPT').AsString;
end;

procedure TScriptThread.DoWriteln;
begin
  if Assigned(FParentDS.FWrFunc) then FParentDS.FWrFunc(FResults);
end;

constructor TScriptThread.Create(Parameters: Variant; aParent: TBaseScript);
begin
  Params := Parameters;
  FParentDS := aParent;
  if Assigned(FParentDS) and (FParentDS.Active) then
    FSyntax := FParentDS.FieldByName('SYNTAX').AsString;
  inherited Create(True)
end;

procedure TScriptThread.Execute;
var
  Compiler: TPSPascalCompiler;
  Result: Boolean;
  Bytecode: tbtString;
  i: Integer;
  ClassImporter: TPSRuntimeClassImporter;
begin
  FStatus := 'R';
  Synchronize(@SetStatus);
  FResults := '';
  Synchronize(@SetResults);
  try
    if lowercase(Fsyntax) = 'sql' then
      begin
        try
          Synchronize(@SQLConn);
          with aDS as IBaseDbFilter do
            DoExecSQL;
          with aDS as IBaseDbFilter do
            FResults:='Num Rows Affected: '+IntToStr(NumRowsAffected);
          Synchronize(@DoWriteln);
          Synchronize(@SetResults);
        except
          on e : Exception do
            begin
              FResults := e.Message;
              Synchronize(@DoWriteln);
              Synchronize(@SetResults);
            end;
        end;
        Synchronize(@SQLConnF);
      end
    else if lowercase(FSyntax) = 'pascal' then
      begin
        Compiler:= TPSPascalCompiler.Create;
        Compiler.OnUses:= @ExtendICompiler;
        try
          Synchronize(@GetScript);
          Result:= Compiler.Compile(FScript) and Compiler.GetOutput(Bytecode);
          FResults:='';
          for i:= 0 to Compiler.MsgCount - 1 do
            if Length(FResults) = 0 then
              FResults:= Compiler.Msg[i].MessageToString
            else
              FResults:= FResults + #13#10 + Compiler.Msg[i].MessageToString;
          if FResults<>'' then
            Synchronize(@SetResults);
        finally
          Compiler.Free;
        end;
        if Result then
          begin
            FRuntime:= TPSExec.Create;
            ClassImporter:= TPSRuntimeClassImporter.CreateAndRegister(FRuntime, false);
            try
              ExtendIRuntime(FRuntime, ClassImporter, FParentDS);
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
              Synchronize(@SetResults);
            if Assigned(FProcess) then InternalKill;
          end;
      end;
    if Result then
      begin
        FStatus:='N';
        Synchronize(@SetStatus);
      end
    else
      begin
        FStatus:='E';
        Synchronize(@SetStatus);
      end;
  except
  end;
end;

destructor TScriptThread.Destroy;
begin
  if Assigned(FProcess) then
    begin
      if Assigned(FProcess) then
        FProcess.Terminate(0);
      if Assigned(FRuntime) then
        FRuntime.Stop;
      Self.Terminate;
      Self.WaitFor;
    end;
  inherited Destroy;
end;

procedure TBaseScript.FThreadTerminate(Sender: TObject);
begin
  TScriptThread(Sender).Free;
end;

procedure TBaseScript.InternalWrite(const s: string);
begin
  if Assigned(Write) then Write(s);
end;

procedure TBaseScript.InternalWriteln(const s: string);
begin
  if Assigned(Writeln) then Writeln(s);
end;

procedure TBaseScript.InternalReadln(var s: string);
begin
  if Assigned(Readln) then Readln(s);
end;

constructor TBaseScript.Create(aOwner: TComponent; DM: TComponent;
  aConnection: TComponent; aMasterdata: TDataSet);
begin
  inherited Create(aOwner, DM, aConnection, aMasterdata);
  FThread := TScriptThread.Create(Null,self);
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
  Result := True;
  FThread := TScriptThread.Create(Parameters,Self);
  FThread.OnTerminate:=@FThreadTerminate;
  FThread.Resume;
end;

end.

