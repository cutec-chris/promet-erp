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
Created 07.04.2015
*******************************************************************************}
unit uprometpascalscript;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, genpascalscript, uPSRuntime,uPSCompiler,uPSUtils,
  Utils,db,genscript,uBaseDbClasses,sha1,base64,md5,uData;

type
  TScriptInternalPrint = function (aType,Reportname,Printer : string;Copies : Integer) : Boolean;
  TScriptInternalPrinterAvalible = function(Printer : string) : Boolean;
  TScriptNumbersetEmpty = function(aNumberset : string) : Boolean;
  { TPrometPascalScript }

  TPrometPascalScript = class(TPascalScript)
    function TPascalScriptUses(Sender: TPascalScript; const aName: tbtString;
      OnlyAdditional : Boolean): Boolean;
  private
    FRlFunc: TStrInFunc;
    FScript: TScript;
    FSlFunc: TSleepFunc;
    FWrFunc: TStrOutFunc;
    FWriFunc: TStrOutFunc;

    function InternalParamStr(Param : Integer) : String;
    function InternalParamCount : Integer;

    function InternalDataSet(SQL : string) : TDataSet;
    function InternalData : TBaseDBModule;
    function ActualObject : TBaseDBDataset;
    procedure InternalStorValue(aName, Value: string);
    function InternalGetValue(aName : string) : string;
    procedure InternalMemoryStorValue(aName, Value: string);
    function InternalMemoryGetValue(aName : string) : string;
    procedure InternalExecuteScript(aCommand, aClient: string);

    procedure InternalExecuteScriptFuncionPS(aScript, aFunc, aParam: string);
    function InternalExecuteScriptFuncionPSRS(aScript, aFunc, aParam: string) : string;
    function InternalExecuteScriptFuncionRS(aScript, aFunc : string) : string;

    function InternalSHA1(aInput : string) : string;
    function InternalMD5(aInput : string) : string;
    function InternalMD5File(aInputFile : string) : string;
    function InternalBase64Encode(aInput : string) : string;
    function InternalBase64FileEncode(aInputFile : string) : string;
    function InternalBase64FileDecode(aInput,aOutputFile : string) : Boolean;
    function InternalBase64Decode(aInput : string) : string;

    function InternalPrint(aType,Reportname,Printer : string;Copies : Integer) : Boolean;
    function InternalPrinterAvalible(Printer : string) : Boolean;
    procedure InternalSetReportVariable(aName,Value : string);
    function InternalSetReportImage(aName,aImage : string) : Boolean;

    function InternalGetNumberFromNumberset(Numberset : string) : string;

    function InternalSaveFilefromDocuments(Filename,OutPath : string) : Boolean;
  public
    function InternalUses(Comp: TPSPascalCompiler; aName: string): Boolean; override;
    property Sleep : TSleepFunc read FSlFunc write FSlFunc;
    constructor Create; override;
    destructor Destroy;override;
  end;

var
  FContextDataSet : TDataSet;
  FVariables : TStringList;
  FReportImages : TStringList;
  OnInternalPrint : TScriptInternalPrint;
  OnInternalPrinterAvalible : TScriptInternalPrinterAvalible;
  FReportVariables : TStringList;
  OnNumbersetEmpty : TScriptNumbersetEmpty;

implementation

uses variants, ubasedatasetinterfaces2,memds,uscripts
  {$IFDEF WINDOWS}
  ,uPSC_comobj
  ,uPSR_comobj
  {$endif}
  ;

function TPrometPascalScript.TPascalScriptUses(Sender: TPascalScript;
  const aName: tbtString; OnlyAdditional: Boolean): Boolean;
var
  aVersion,aIName: String;
begin
  Result:=False;
  if aName = 'SYSTEM' then
    begin
      Result := True;
      try
        Sender.AddMethod(Self,@TPrometPascalScript.InternalWriteln,'procedure Writeln(P1: string);');
        Sender.AddMethod(Self,@TPrometPascalScript.InternalDebugln,'procedure Debugln(P1: string);');
        Sender.AddMethod(Self,@TPrometPascalScript.InternalWrite,'procedure Write(P1: string);');
        Sender.AddMethod(Self, @TPrometPascalScript.InternalParamStr,'function ParamStr(Param : Integer) : String;');
        Sender.AddMethod(Self, @TPrometPascalScript.InternalParamCount,'function ParamCount : Integer;');
        Sender.AddFunction(@UniToSys,'function UniToSys(const s: string): string;');
        Sender.AddFunction(@SysToUni,'function SysToUni(const s: string): string;');
      except
        Result := False; // will halt compilation
      end;
    end
  else if aName = 'SCRIPT' then
    begin
      Result := True;
      Sender.AddMethod(Self,@TPrometPascalScript.InternalExecuteScriptFuncionPS,'procedure ExecuteScriptFunctionPS(aScript, aFunc, aParam: string);');
      Sender.AddMethod(Self,@TPrometPascalScript.InternalExecuteScriptFuncionPSRS,'function ExecuteScriptFunctionPSRS(aScript, aFunc, aParam: string) : string;');
      Sender.AddMethod(Self,@TPrometPascalScript.InternalExecuteScriptFuncionRS,'function ExecuteScriptFunctionRS(aScript, aFunc : string) : string;');
    end
  else if aName = 'MD5' then
    begin
      Result := True;
      Sender.AddMethod(Self,@TPrometPascalScript.InternalMD5,'function MD5(aInput : string) : string;');
      Sender.AddMethod(Self,@TPrometPascalScript.InternalMD5File,'function MD5File(aInputFile : string) : string;');
    end
  else if aName = 'SHA1' then
    begin
      Result := True;
      Sender.AddMethod(Self,@TPrometPascalScript.InternalSHA1,'function SHA1(aInput : string) : string;');
    end
  else if aName = 'BASE64' then
    begin
      Result := True;
      Sender.AddMethod(Self,@TPrometPascalScript.InternalBase64Encode,'function Base64Encode(aInput : string) : string;');
      Sender.AddMethod(Self,@TPrometPascalScript.InternalBase64Decode,'function Base64Decode(aInput : string) : string;');
      Sender.AddMethod(Self,@TPrometPascalScript.InternalBase64FileEncode,'function Base64FileEncode(aInputFile : string) : string;');
      Sender.AddMethod(Self,@TPrometPascalScript.InternalBase64FileDecode,'function Base64FileDecode(aInput,aOutputFile : string) : Boolean;');

    end
  else if aName = 'PROMET' then
    begin
      Result := True;
      try
        Sender.AddFunction(@Utils.SystemUserName,'function SystemUserName : string;');
        Sender.AddFunction(@Utils.GetSystemName,'function GetSystemName : string;');
        Sender.InternalUses(Sender.Compiler,'DB');
        Sender.InternalUses(Sender.Compiler,'DATEUTILS');
        Sender.AddMethod(Self,@TPrometPascalScript.InternalDataSet,'function DataSet(SQL : string) : TDataSet;');
        Sender.AddMethod(Self,@TPrometPascalScript.InternalData,'function Data : TBaseDBModule');
        //Sender.ClassImporter.Add();
      except
        Result := False; // will halt compilation
      end;
    end
  {$ifdef WINDOWS}
  else if aName = 'COMOBJ' then
    begin
      Result := True;
      SIRegister_ComObj(Sender.Compiler);
      RIRegister_ComObj(Sender.Runtime);
    end
  {$endif}
  else if not OnlyAdditional then
    begin
      try
        {
        aScript := TBaseScript.CreateEx(nil,Data);
        try
          Result:=False;
          aVersion:='';
          aIName := aName;
          if pos('_',aIName)>0 then
            begin
              aVersion := copy(aIName,pos('_',aIName)+1,length(aIName));
              aIName := copy(aIName,0,pos('_',aIName)-1);
            end;
          if Assigned(BaseApplication) then with BaseApplication as IBaseApplication do
            Debug('Uses get Unit from Database:'+aName);
          if aVersion = '' then
            aScript.Filter(Data.ProcessTerm('UPPER('+Data.QuoteField('NAME')+')=UPPER('+Data.QuoteValue(aIName)+') AND '+Data.QuoteField('ACTIVE')+'='+Data.QuoteValue('Y')))
          else
            aScript.Filter(Data.ProcessTerm('UPPER('+Data.QuoteField('NAME')+')=UPPER('+Data.QuoteValue(aIName)+') AND UPPER('+Data.QuoteField('VERSION')+')=UPPER('+Data.QuoteValue(aVersion))+')');
          if aScript.Count>0 then
            if aScript.Locate('NAME',aIName,[loCaseInsensitive]) then
              begin
                if Assigned(OnCheckModule) then
                  OnCheckModule(aScript);
                Result := Sender.Compiler.Compile(aScript.FieldByName('SCRIPT').AsString);
              end;
        except
          Result := False;
        end;}
      finally
        //FreeAndNil(aScript);
      end;
    end;
  if not OnlyAdditional then
    begin
    end
  else Result:=True;
end;

function TPrometPascalScript.InternalParamStr(Param: Integer): String;
begin
  Result:='';
  if not VarIsArray(FScript.Parameters) then exit;
  if Param<=VarArrayHighBound(FScript.Parameters,1) then
    Result:=FScript.Parameters[Param];
end;

function TPrometPascalScript.InternalParamCount: Integer;
begin
  if not VarIsArray(FScript.Parameters) then exit;
  Result := VarArrayHighBound(FScript.Parameters,1)+1;
end;

function TPrometPascalScript.InternalDataSet(SQL: string): TDataSet;
var
  aDS: TMemDataset;
begin
  aDS := TMemDataset.Create(nil);
  if TBaseDBModule(Data).ExecuteDirect(SQL,aDS) then
    Result := aDS
  else aDS.Free;
end;

function TPrometPascalScript.InternalData: TBaseDBModule;
begin
  Result := uData.Data;
end;

function TPrometPascalScript.ActualObject: TBaseDBDataset;
begin
  Result := nil;
  //if Assigned(Parent) and (Parent is TBaseScript) then
  //  Result := TBaseScript(Parent).ActualObject;
end;

procedure TPrometPascalScript.InternalStorValue(aName, Value : string);
begin
end;

function TPrometPascalScript.InternalGetValue(aName: string): string;
begin
end;

procedure TPrometPascalScript.InternalMemoryStorValue(aName, Value: string);
begin
  fVariables.Values[aName] := Value;
end;

function TPrometPascalScript.InternalMemoryGetValue(aName: string): string;
begin
  result := fVariables.Values[aName];
end;

procedure TPrometPascalScript.InternalExecuteScript(aCommand, aClient: string);
begin
  {
  if Assigned(BaseApplication) then
    with BaseApplication as IBaseApplication do
      TMessageHandler(GetMessageManager).SendCommand(aClient,aCommand);
  }
end;

procedure TPrometPascalScript.InternalExecuteScriptFuncionPS(aScript,aFunc,
  aParam: string);
//var
//  bScript: TBaseScript;
begin
  {
  bScript := TBaseScript.Create(nil);
  bScript.Filter(Data.QuoteField('NAME')+'='+Data.QuoteValue(aScript));
  if bScript.Count>0 then
    begin
      try
        if TPrometPascalScript(bScript.Script).Compile then
          TPrometPascalScript(bScript.Script).Runtime.RunProcPN([aParam],aFunc);
      except
      end;
    end;
  bScript.Free;
  }
end;

function TPrometPascalScript.InternalExecuteScriptFuncionPSRS(aScript, aFunc,
  aParam: string): string;
{
var
  bScript: TBaseScript;
}
begin
  {
  bScript := TBaseScript.Create(nil);
  bScript.Filter(Data.QuoteField('NAME')+'='+Data.QuoteValue(aScript));
  if bScript.Count>0 then
    begin
      try
        if TPascalScript(bScript.Script).Compile then
          Result := TPascalScript(bScript.Script).Runtime.RunProcPN([aParam],aFunc);
      except
        Result := '';
      end;
    end;
  bScript.Free;
  }
end;

function TPrometPascalScript.InternalExecuteScriptFuncionRS(aScript,
  aFunc: string): string;
  {
var
  bScript: TBaseScript;
  }
begin
  {
  bScript := TBaseScript.Create(nil);
  bScript.Filter(Data.QuoteField('NAME')+'='+Data.QuoteValue(aScript));
  if bScript.Count>0 then
    begin
      try
        if TPascalScript(bScript.Script).Compile then
          Result := TPascalScript(bScript.Script).Runtime.RunProcPN([],aFunc);
      except
        Result := '';
      end;
    end;
  bScript.Free;
  }
end;

function TPrometPascalScript.InternalSHA1(aInput: string): string;
begin
  Result := SHA1Print(SHA1String(aInput));
end;

function TPrometPascalScript.InternalMD5(aInput: string): string;
begin
  Result := md5.MD5Print(MD5String(aInput));
end;

function TPrometPascalScript.InternalMD5File(aInputFile: string): string;
begin
  Result := md5.MD5Print(md5.MD5File(aInputFile));
end;

function TPrometPascalScript.InternalBase64Encode(aInput: string): string;
begin
  Result := base64.EncodeStringBase64(aInput);
end;

function TPrometPascalScript.InternalBase64FileEncode(aInputFile: string
  ): string;
var
  Outstream: TStringStream;
  Encoder: TBase64EncodingStream;
  InStream: TFileStream;
begin
  Outstream:=TStringStream.Create('');
  InStream := TFileStream.Create(aInputFile,fmOpenRead);
  try
    Encoder:=TBase64EncodingStream.create(outstream);
    try
      Encoder.CopyFrom(InStream,0);
    finally
      Encoder.Free;
      end;
    Result:=Outstream.DataString;
  finally
    Outstream.free;
    InStream.Free;
  end;
end;

function TPrometPascalScript.InternalBase64FileDecode(aInput,
  aOutputFile: string): Boolean;
var
  Outstream: TStringStream;
  FileStream: TFileStream;
begin
  Outstream:=TStringStream.Create(base64.DecodeStringBase64(aInput));
  FileStream := TFileStream.Create(aOutputFile,fmCreate);
  FileStream.CopyFrom(Outstream,0);
  FileStream.Free;
  Outstream.Free;
  Result := True;
end;

function TPrometPascalScript.InternalBase64Decode(aInput: string): string;
begin
  Result := base64.DecodeStringBase64(aInput);
end;

function TPrometPascalScript.InternalPrint(aType, Reportname, Printer: string;
  Copies: Integer): Boolean;
begin
  Result := False;
  if Assigned(OnInternalPrint) then
    Result := OnInternalPrint(aType,ReportName,Printer,Copies);
end;

function TPrometPascalScript.InternalPrinterAvalible(Printer: string): Boolean;
begin
  Result := False;
  if Assigned(OnInternalPrinterAvalible) then
    Result := OnInternalPrinterAvalible(Printer);
end;

procedure TPrometPascalScript.InternalSetReportVariable(aName, Value: string);
begin
  if not Assigned(FReportVariables) then
    FReportVariables := TStringList.Create;
  FReportVariables.Values[aName] := Value;
end;

function TPrometPascalScript.InternalSetReportImage(aName, aImage: string
  ): Boolean;
begin
  FReportImages.Values[aName] := aImage;
  Result := True;
end;

function TPrometPascalScript.InternalGetNumberFromNumberset(Numberset: string
  ): string;
begin
  try
    //Result := Data.Numbers.GetNewNumber(Numberset);
  except
    Result := '';
  end;
  if Result='' then
    if Assigned(OnNumbersetEmpty) then
      if OnNumbersetEmpty(Numberset) then
        //Result := Data.Numbers.GetNewNumber(Numberset);
end;

function TPrometPascalScript.InternalSaveFilefromDocuments(Filename,
  OutPath: string): Boolean;
{
var
  aDocuments: TDocument;
  aStream: TFileStream;
  aName: String;
  aExt: String;
  aDocument: TDocument;
  aScript: TBaseScript;
}
begin
  {
  Result := False;
  aName := ExtractFileName(Filename);
  if rpos('.',aName)>0 then
    aName := copy(aName,0,rpos('.',aName)-1);
  aExt := ExtractFileExt(Filename);
  if pos('.',aExt)>0 then
    aExt := copy(aExt,pos('.',aExt)+1,length(aExt));
  aDocuments := TDocument.Create(nil);
  aDocuments.Select(Id,'S',Id,Version,Null);
  aDocuments.Open;
  if aDocuments.Locate('NAME;EXTENSION',VarArrayOf([aName,aExt]),[loCaseInsensitive]) then
    begin
      aDocument := TDocument.Create(nil);
      aDocument.SelectByNumber(aDocuments.FieldByName('NUMBER').AsVariant);
      aDocument.Open;
      aStream := TFileStream.Create(OutPath,fmCreate);
      Result := aDocument.CheckoutToStream(aStream);
      aStream.Free;
      aDocument.Free;
    end
  else if Assigned(ActualObject) then
    begin
      if  (ActualObject is TBaseDbList)
      and (ActualObject.FieldByName('VERSION')<>nil)
      then
        begin
          aDocuments.Select(ActualObject.Id.AsLargeInt,TBaseDbList(ActualObject).GetTyp,ActualObject.Id.AsString,TBaseDbList(ActualObject).FieldByName('VERSION').AsVariant,Null);
          aDocuments.Open;
          if aDocuments.Locate('NAME;EXTENSION',VarArrayOf([aName,aExt]),[loCaseInsensitive]) then
            begin
              aDocument := TDocument.Create(nil);
              aDocument.SelectByNumber(aDocuments.FieldByName('NUMBER').AsVariant);
              aDocument.Open;
              aStream := TFileStream.Create(OutPath,fmCreate);
              Result := aDocument.CheckoutToStream(aStream);
              aStream.Free;
              aDocument.Free;
            end
        end;
    end;
  aDocuments.Free;
  }
end;

function TPrometPascalScript.InternalUses(Comp: TPSPascalCompiler; aName: string
  ): Boolean;
begin
  Result:=inherited InternalUses(Comp, aName);
  Result := TPascalScriptUses(Self,aName,Result);
end;

constructor TPrometPascalScript.Create;
begin
  inherited Create;
  FReportVariables := nil
end;


destructor TPrometPascalScript.Destroy;
begin
  try
    if Assigned(FReportVariables) then
      FreeAndNil(FReportVariables);
  except
  end;
  inherited Destroy;
end;

initialization
  RegisterScriptType(TPrometPascalScript);
  FVariables := TStringList.Create;
  FReportImages := TStringList.Create;
  OnInternalPrint := nil;
finalization
  FVariables.Free;
  FReportImages.Free;
end.

