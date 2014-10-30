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
unit genpascalscript;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uPSCompiler,db,
  uPSC_classes, uPSC_DB, uPSC_dateutils, uPSC_dll, uPSRuntime,
  uPSR_classes, uPSR_DB, uPSR_dateutils, uPSR_dll, uPSUtils,
  Process,usimpleprocess,Utils,variants,UTF8Process,dynlibs;

type
  TWritelnFunc = procedure(const s: string) of object;
  TWriteFunc = procedure(const s: string) of object;
  TReadlnFunc = procedure(var s: string) of object;
  TSleepFunc = procedure(MiliSecValue : cardinal);

  TLoadedLib = class
  public
    Name : string;
    Code : string;
    Handle : THandle;
    constructor Create;
  end;

  TScript = class
  private
    FParameters: Variant;
    FResults: string;
    FSource: string;
    FStatus: char;
    FStatusChanged: TNotifyEvent;
    procedure SetStatus(AValue: char);
  public
    function Execute(aParameters : Variant) : Boolean;virtual;
    property Source : string read FSource write FSource;
    property Parameters : Variant read FParameters write FParameters;
    property Status : char read FStatus write SetStatus;
    property Results : string read FResults write FResults;
    property OnStatusChanged : TNotifyEvent read FStatusChanged write FStatusChanged;
  end;

  TPascalScript = class;

  TPascalOnUses = function(Sender: TPascalScript; const Name: tbtString): Boolean of object;

  TPascalScript = class(TScript)
  private
    CompleteOutput : string;
    FOnUses: TPascalOnUses;
    FProcess: TProcessUTF8;
    FRuntime : TPSExec;
    FRuntimeFree: Boolean;
    FCompiler: TPSPascalCompiler;
    FCompilerFree: Boolean;
    FClassImporter: TPSRuntimeClassImporter;
    procedure SetCompiler(AValue: TPSPascalCompiler);
    procedure SetRuntime(AValue: TPSExec);
  protected
    function InternalParamStr(Param : Integer) : String;
    function InternalParamCount : Integer;
    procedure InternalChDir(Directory : string);
    procedure InternalMkDir(Directory : string);

    procedure InternalExec(cmd : string;ShowConsole : Boolean = False);
    function InternalExecActive: Boolean;
    function InternalExecResult: Integer;
    function InternalKill: Boolean;
    procedure InternalBeep;
    procedure InternalSleep(MiliSecValue: LongInt);

    function InternalGet(URL : string) : string;
    function InternalPost(URL,Content : string) : string;
  public
    function InternalUses(Comp : TPSPascalCompiler;Name : string) : Boolean;
    function Execute(aParameters: Variant): Boolean; override;
    property Runtime : TPSExec read FRuntime write SetRuntime;
    property Compiler : TPSPascalCompiler read FCompiler write SetCompiler;
    function AddMethodEx(Slf, Ptr: Pointer; const Decl: tbtstring; CallingConv: uPSRuntime.TPSCallingConvention): Boolean;
    function AddMethod(Slf, Ptr: Pointer; const Decl: tbtstring): Boolean;
    property OnUses : TPascalOnUses read FOnUses write FOnUses;
    constructor Create;
    destructor Destroy; override;
  end;

var
  LoadedLibs : TList;

implementation

uses httpsend;

function ExtendICompiler(Sender: TPSPascalCompiler; const Name: tbtString
  ): Boolean;
begin
  TPascalScript(Sender.Obj).InternalUses(Sender,Name);
end;

constructor TLoadedLib.Create;
begin
  Handle:=0;
end;

procedure TScript.SetStatus(AValue: char);
begin
  if FStatus=AValue then Exit;
  FStatus:=AValue;
  if Assigned(FStatusChanged) then
    FStatusChanged(Self);
end;

function TScript.Execute(aParameters: Variant): Boolean;
begin
  FParameters:=aParameters;
end;

procedure TPascalScript.InternalExec(cmd: string; ShowConsole: Boolean);
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
function TPascalScript.InternalExecActive : Boolean;
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

function TPascalScript.InternalExecResult: Integer;
begin
  Result := FProcess.ExitStatus;
end;

function TPascalScript.InternalKill : Boolean;
begin
  Result := Assigned(FProcess);
  if Result then
    begin
      FProcess.Terminate(0);
      while FProcess.Running do InternalExecActive;
      InternalExecActive;
    end;
end;

procedure TPascalScript.InternalBeep;
begin
  Beep;
end;

procedure TPascalScript.InternalSleep(MiliSecValue: LongInt);
begin
  sleep(MiliSecValue);
end;

function TPascalScript.InternalGet(URL: string): string;
var
  ahttp: THTTPSend;
begin
  ahttp := THTTPSend.Create;
  ahttp.HTTPMethod('GET',URL);
  if ahttp.ResultCode=200 then
    begin
      setlength(Result,ahttp.Document.Size);
      ahttp.Document.Read(Result[1],ahttp.Document.Size);
    end
  else Result:='';
  ahttp.Free;
end;

function TPascalScript.InternalPost(URL, Content: string): string;
var
  ahttp: THTTPSend;
begin
  ahttp := THTTPSend.Create;
  ahttp.Document.Write(Content[1],length(Content));
  ahttp.HTTPMethod('POST',URL);
  if ahttp.ResultCode=200 then
    begin
      setlength(Result,ahttp.Document.Size);
      ahttp.Document.Read(Result[1],ahttp.Document.Size);
    end
  else Result:='';
  ahttp.Free;
end;
function IProcessDllImport(Sender: TPSExec; p: TPSExternalProcRec; Tag: Pointer
  ): Boolean;
var
  i: LongInt;
  pv: PIFProcRec;
  h: LongInt;
  aLib: String;
  Caller: TPSExec;
  a: Integer;
  s: String;
  ph: PLoadedDll;
  aLibName: String;
  actLib: TbtString;
begin
  Result := ProcessDllImport(Sender,p);

  aLib := lowercase(copy(p.Decl,5,length(p.Decl)));
  aLibName := lowercase(copy(aLib,0,rpos('.',aLib)-1));
  for a := 0 to LoadedLibs.Count-1 do
    if (aLibName = lowercase(TLoadedLib(LoadedLibs[a]).Name)) and (TLoadedLib(LoadedLibs[a]).Handle=0) then
      begin
        Caller := Sender;
        i := 2147483647; // maxint
        repeat
          ph := Caller.FindProcResource2(@dllFree, i);
          if (ph = nil) then break;
          actLib := lowercase(ph^.dllname);
          if (actLib = aLib) then
            TLoadedLib(LoadedLibs[a]).Handle := ph^.dllhandle;
        until false;
      end;
end;
type
  aProcT = function : pchar;stdcall;
function TPascalScript.InternalUses(Comp: TPSPascalCompiler; Name: string
  ): Boolean;
var
  aLib: TLibHandle;
  aProc: aProcT;
  Procs : TStringList;
  sProc: String;
  i: Integer;
  aLibName: TbtString;
  tmp: String;
  newUnit: String;
  tmp1,tmp2: String;
  NewLib: TLoadedLib;
  tmp3: String;
begin
  Result := True;
  try
    if lowercase(Name)='system' then
      begin
        AddMethod(Self,@TPascalScript.InternalChDir,'procedure ChDir(Dir : string);');
        AddMethod(Self,@TPascalScript.InternalMkDir,'procedure MkDir(Dir : string);');
      end
    else if lowercase(Name)='sysutils' then
      begin
        AddMethod(Self,@TPascalScript.InternalBeep,'procedure Beep;');
        AddMethod(Self,@TPascalScript.InternalSleep,'procedure Sleep(MiliSecValue : LongInt);');
      end
    else if lowercase(Name)='exec' then
      begin
        AddMethod(Self,@TPascalScript.InternalExec,'procedure Exec(cmd : string;ShowConsole : Boolean);');
        AddMethod(Self,@TPascalScript.InternalExecActive,'function ExecActive : Boolean;');
        AddMethod(Self,@TPascalScript.InternalExecResult,'function ExecResult : Integer;');
        AddMethod(Self,@TPascalScript.InternalKill,'function Kill : Boolean;');
      end
    else if lowercase(Name)='net' then
      begin
        AddMethod(Self,@TPascalScript.InternalGet,'function Get(URL : string) : string;');
        AddMethod(Self,@TPascalScript.InternalPost,'function Post(URL,Content : string) : string;');
      end
    else if lowercase(Name)='db' then
      begin
        uPSC_DB.SIRegister_DB(Comp);
        uPSR_DB.RIRegister_DB(FClassImporter);
      end
    else if lowercase(Name)='dateutils' then
      begin
        uPSC_dateutils.RegisterDateTimeLibrary_C(Comp);
        uPSR_dateutils.RegisterDateTimeLibrary_R(Runtime);
      end
    else if lowercase(Name)='classes' then
      begin
        uPSC_classes.SIRegister_Classes(Comp,False);
        uPSR_classes.RIRegister_Classes(FClassImporter,false);
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
            if not Assigned(Comp.OnExternalProc) then
              uPSC_dll.RegisterDll_Compiletime(Comp);
            Runtime.AddSpecialProcImport('dll', @IProcessDllImport, nil);
            Runtime.RegisterFunctionName('UNLOADDLL', @UnloadProc, nil, nil);
            Runtime.RegisterFunctionName('DLLGETLASTERROR', @GetLastErrorProc, nil, nil);
            for i := 0 to LoadedLibs.Count-1 do
              if TLoadedLib(LoadedLibs[i]).Name=Name then
                begin
                  Comp.Compile(TLoadedLib(LoadedLibs[i]).Code);
                  Result := True;
                  exit;
                end;
            aLib := LoadLibrary(ExtractFilePath(ParamStr(0))+DirectorySeparator+Name+'.dll');
            if aLib <> dynlibs.NilHandle  then
              begin
                aProc := aprocT(dynlibs.GetProcAddress(aLib,'ScriptDefinition'));
                if Assigned(aProc) then
                  begin
                    newUnit := 'unit '+name+';'+LineEnding+'interface'+LineEnding+'type';
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
                          tmp := trim(copy(tmp,0,pos(':',tmp)-1))
                        else if pos(';',tmp)>0 then
                          tmp := trim(copy(tmp,0,pos(';',tmp)-1));
                        if pos(')',sProc)>0 then
                          tmp1 := copy(sProc,0,pos(')',sProc))
                        else tmp1 := '';
                        tmp3 := copy(sProc,length(tmp1)+1,length(sProc));
                        tmp1 := tmp1+copy(tmp3,0,pos(';',tmp3));
                        tmp2 := copy(sProc,pos(')',sProc)+1,length(sProc));
                        tmp2 := copy(tmp2,pos(';',tmp2)+1,Length(sProc));
                        tmp2 := copy(tmp2,0,pos(';',tmp2)-1);
                        if tmp2<>'' then
                          tmp2 := ' '+tmp2;
                        tmp := '  '+tmp1+'external '''+tmp+'@'+ExtractFileName(aLibname)+tmp2+''';';
                        newUnit := newUnit+LineEnding+tmp;
                      end;
                    newUnit := newUnit+LineEnding+'implementation'+lineending+'end.';
                    NewLib := TLoadedLib.Create;
                    NewLib.Name:=Name;
                    NewLib.Code:=newUnit;
                    LoadedLibs.Add(NewLib);
                    Comp.Compile(newUnit);
                    Procs.Free;
                    Result := True;
                  end;
                FreeLibrary(aLib);
              end;
          end
        else //unit uses
          begin
            Result := False;
          end;
      end;
  except
    begin
      raise;
      Result := False; // will halt compilation
    end;
  end;
  if Assigned(FOnUses) then
    Result := FOnUses(Self,Name) or Result;
end;

procedure TPascalScript.SetCompiler(AValue: TPSPascalCompiler);
begin
  if FCompiler=AValue then Exit;
  if FCompilerFree then
    FCompiler.Free;
  FCompiler:=AValue;
  FCompilerFree := False;
end;

procedure TPascalScript.SetRuntime(AValue: TPSExec);
begin
  if FRuntime=AValue then Exit;
  if FRuntimeFree then
    FRuntime.Free;
  FRuntime:=AValue;
  FRuntimeFree:=False;
end;

function TPascalScript.InternalParamStr(Param: Integer): String;
begin
  Result:='';
  if Param<VarArrayHighBound(FParameters,1) then
    Result:=FParameters[Param];
end;

function TPascalScript.InternalParamCount: Integer;
begin
  Result := VarArrayHighBound(FParameters,1);
end;

procedure TPascalScript.InternalChDir(Directory: string);
begin
  chdir(Directory);
end;

procedure TPascalScript.InternalMkDir(Directory: string);
begin
  mkdir(Directory);
end;
function TPascalScript.Execute(aParameters: Variant): Boolean;
var
  Bytecode: tbtString;
  i: Integer;
begin
  Compiler.Obj := Self;
  Compiler.OnUses:= @ExtendICompiler;
  Result:= Compiler.Compile(Source) and Compiler.GetOutput(Bytecode);
  FResults:='';
  for i:= 0 to Compiler.MsgCount - 1 do
    if Length(FResults) = 0 then
      FResults:= Compiler.Msg[i].MessageToString
    else
      FResults:= FResults + #13#10 + Compiler.Msg[i].MessageToString;
  if Result then
    begin
      //ExtendIRuntime(FRuntime, ClassImporter, Self);
      Result:= FRuntime.LoadData(Bytecode)
            and FRuntime.RunScript
            and (FRuntime.ExceptionCode = erNoError);
      if not Result then
        FResults:= PSErrorToString(FRuntime.LastEx, '');
      if FProcess.Running then InternalKill;
      Result := True;
    end;
end;

function TPascalScript.AddMethodEx(Slf, Ptr: Pointer; const Decl: tbtstring;
  CallingConv: uPSRuntime.TPSCallingConvention): Boolean;
var
  P: TPSRegProc;
begin
  p := FCompiler.AddDelphiFunction(Decl);
  if p <> nil then
  begin
    FRuntime.RegisterDelphiMethod(Slf, Ptr, p.Name, CallingConv);
    Result := True;
  end else Result := False;
end;

function TPascalScript.AddMethod(Slf, Ptr: Pointer; const Decl: tbtstring
  ): Boolean;
begin
  Result := AddMethodEx(Slf, Ptr, Decl, cdRegister);
end;

constructor TPascalScript.Create;
begin
  FProcess := TProcessUTF8.Create(nil);
  FProcess.ShowWindow:=swoNone;
  FCompiler:= TPSPascalCompiler.Create;
  FCompilerFree:=True;
  FRuntime:= TPSExec.Create;
  FRuntimeFree := True;
  FClassImporter:= TPSRuntimeClassImporter.CreateAndRegister(FRuntime, false);
end;

destructor TPascalScript.Destroy;
begin
  if Assigned(FProcess) then
    begin
      if Assigned(FProcess) then
        FreeAndNil(FProcess);
      if Assigned(FRuntime) then
        FRuntime.Stop;
    end;
  if FCompilerFree then
    FCompiler.Free;
  if FRuntimeFree then
    FRuntime.Free;
  inherited Destroy;
end;

end.

