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
  uPSR_classes, uPSR_DB, uPSR_dll, uPSUtils,Process,usimpleprocess;

type
  TWritelnFunc = procedure(const s: string) of object;
  TWriteFunc = procedure(const s: string) of object;
  TReadlnFunc = procedure(var s: string) of object;
  TBaseScript = class(TBaseDBDataset)
  private
    FRlFunc: TReadlnFunc;
    FWrFunc: TWritelnFunc;
    FWriFunc: TWriteFunc;
  protected
    procedure InternalWrite(const s: string);
    procedure InternalWriteln(const s: string);
    procedure Internalreadln(var s: string);
  public
    procedure DefineFields(aDataSet: TDataSet); override;
    procedure FillDefaults(aDataSet: TDataSet); override;
    property Write : TWriteFunc read FWriFunc write FWriFunc;
    property Writeln : TWritelnFunc read FWrFunc write FWRFunc;
    property Readln : TReadlnFunc read FRlFunc write FRlFunc;
    function Execute : Boolean;
  end;

  { TProcessThread }

  TProcessThread = class(TThread)
  private
    aLine : string;
    procedure OutputLine;
  public
    constructor Create(cmd : string);
    procedure Execute; override;
  end;

  function ProcessScripts : Boolean;//process Scripts that must be runned cyclic
  procedure ExtendRuntime(Runtime: TPSExec; ClassImporter: TPSRuntimeClassImporter;Script : TBaseScript);
  function ExtendCompiler(Sender: TPSPascalCompiler; const Name: tbtString): Boolean;
  procedure ResetProcess;

implementation
uses uStatistic,uData;
var
  FProcess : TProcess;
  FRuntime : TPSExec;
  FThread: TProcessThread;
function ProcessScripts : Boolean;//process Scripts that must be runned cyclic
var
  aScript: TBaseScript;
begin
  aScript := TBaseScript.Create(nil,Data);
  aScript.Filter(Data.QuoteField('RUNEVERY')+'>'+Data.QuoteValue('0'));
  while not aScript.EOF do
    begin
      if aScript.FieldByName('STATUS').AsString<>'E' then
        if aScript.FieldByName('LASTRUN').AsDateTime+(aScript.FieldByName('RUNEVERY').AsInteger/MinsPerDay)<Now() then
          aScript.Execute;
      aScript.Next;
    end;
  aScript.Free;
end;
procedure InternalExec(cmd : string);
begin
  FThread := TProcessThread.Create(cmd);
end;
function InternalExecActive : Boolean;
begin
  Result := Assigned(FProcess) and FProcess.Active;
end;
function InternalKill : Boolean;
begin
  Result := Assigned(FProcess);
  if Result then
    begin
      FProcess.Terminate(0);
      while Assigned(FProcess) do sleep(10);
    end;
end;
procedure ExtendRuntime(Runtime: TPSExec; ClassImporter: TPSRuntimeClassImporter;Script : TBaseScript);
begin
  Runtime.RegisterDelphiFunction(@InternalExec, 'EXEC', cdRegister);
  Runtime.RegisterDelphiFunction(@InternalExecActive, 'EXECACTIVE', cdRegister);
  Runtime.RegisterDelphiFunction(@InternalKill, 'KILL', cdRegister);
  FRuntime := Runtime;
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

procedure ResetProcess;
begin
  if Assigned(FProcess) then InternalKill;
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
  ExtendRuntime(Runtime,ClassImporter,Script);
end;

procedure TProcessThread.OutputLine;
begin
  FRuntime.RunProcPN([aLine],'EXECLINERECEIVED');
end;

constructor TProcessThread.Create(cmd: string);
begin
  FProcess := TProcess.Create(nil);
  FProcess.CommandLine:=cmd;
  FProcess.Options:=FProcess.Options+[poUsePipes,poNoConsole];
  inherited Create(false);
end;

procedure TProcessThread.Execute;
var
  aVars: TPSList;
  procedure ProcessOutput;
  var
    ReadSize: DWord;
    Buffer: array[0..127] of char;
    ReadCount: LongInt;
    CompleteOutput : string = '';
    aProc: Cardinal;
    aVar: TPSVariant;
    aFunc: PIFVariant;
    aSproc: TbtString;
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
        Self.Synchronize(@OutputLine);
        CompleteOutput:=copy(CompleteOutput,pos(#10,CompleteOutput)+1,length(CompleteOutput));
      end;
  end;

begin
  FProcess.Execute;
  aVars := TPSList.Create;
  sleep(100);
  while FProcess.Active and (not Terminated) do
    begin
      ProcessOutput;
      sleep(100);
    end;
  ProcessOutput;
  aVars.Free;
  if FProcess.Active then
    FProcess.Terminate(0);
  FreeAndNil(FProcess);
end;

procedure TBaseScript.InternalWrite(const s: string);
begin
  if Assigned(Write) then Write(s);
end;

procedure TBaseScript.InternalWriteln(const s: string);
begin
  if Assigned(Writeln) then Writeln(s);
end;

procedure TBaseScript.Internalreadln(var s: string);
begin
  if Assigned(Readln) then Readln(s);
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
function TBaseScript.Execute: Boolean;
var
  aDS: TDataSet;
  Compiler: TPSPascalCompiler;
  Messages: TbtString='';
  ClassImporter: TPSRuntimeClassImporter;
  Bytecode: tbtString;
  i: Integer;
  RuntimeErrors: TbtString='';
  aSQL: String;
begin
  try
    result := False;
    Edit;
    FieldByName('STATUS').AsString:='R';
    FieldByName('LASTRESULT').Clear;
    Post;
    if lowercase(FieldByName('SYNTAX').AsString) = 'sql' then
      begin
        aSQL := ReplaceSQLFunctions(FieldByName('SCRIPT').AsString);
        aDS := TBaseDBModule(DataModule).GetNewDataSet(aSQL);
        try
          with aDS as IBaseDbFilter do
            DoExecSQL;
          Result := True;
          with aDS as IBaseDbFilter do
            if Assigned(Writeln) then Writeln('Num Rows Affected: '+IntToStr(NumRowsAffected));
          Edit;
          with aDS as IBaseDbFilter do
            FieldByName('LASTRESULT').AsString:='Num Rows Affected: '+IntToStr(NumRowsAffected);
        except
          on e : Exception do
            begin
              if Assigned(Writeln) then Writeln(e.Message);
              Result := False;
              Edit;
              FieldByName('LASTRESULT').AsString:=e.Message+LineEnding+'SQL:'+LineEnding+aSQL;
            end;
        end;
        //aDS.Free;
      end
    else if lowercase(FieldByName('SYNTAX').AsString) = 'pascal' then
      begin
        Compiler:= TPSPascalCompiler.Create;
        Compiler.OnUses:= @ExtendICompiler;
        try
          Result:= Compiler.Compile(FieldByName('SCRIPT').AsString) and Compiler.GetOutput(Bytecode);
          for i:= 0 to Compiler.MsgCount - 1 do
            if Length(Messages) = 0 then
              Messages:= Compiler.Msg[i].MessageToString
            else
              Messages:= Messages + #13#10 + Compiler.Msg[i].MessageToString;
          if Messages<>'' then
            begin
              Edit;
              FieldByName('LASTRESULT').AsString:='Compile Messages:'+Messages;
            end;
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
                RuntimeErrors:= PSErrorToString(FRuntime.LastEx, '');
            finally
              ClassImporter.Free;
              FreeAndNil(FRuntime);
            end;
            if RuntimeErrors<>'' then
              begin
                Edit;
                FieldByName('LASTRESULT').AsString:='Runtime Errors:'+RuntimeErrors;
              end;
            ResetProcess;
          end;
      end;
    Edit;
    if Result then
      begin
        FieldByName('STATUS').AsString:='N';
        FieldByName('LASTRUN').AsDateTime:=Now();
      end
    else
      FieldByName('STATUS').AsString:='E';
    Post;
  except
    result := False;
  end;
end;

end.

