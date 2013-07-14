{******************************************************************************}
{*        Copyright 1999-2001 by J.Friebel all rights reserved.               *}
{*        Autor           :  Jörg Friebel                                     *}
{*        Compiler        :  Delphi 4 / 5                                     *}
{*        System          :  Windows NT / 2000                                *}
{*        Projekt         :  TAPI Komponenten (TAPI Version 1.4 bis 3.0)      *}
{*        Last Update     :  17.02.2002                                       *}
{*        Version         :  1.4                                              *}
{*        EMail           :  tapi@delphiclub.de                               *}
{******************************************************************************}
{*                                                                            *}
{*    This File is free software; You can redistribute it and/or modify it    *}
{*    under the term of GNU Library General Public License as published by    *}
{*    the Free Software Foundation. This File is distribute in the hope       *}
{*    it will be useful "as is", but WITHOUT ANY WARRANTY OF ANY KIND;        *}
{*    See the GNU Library Public Licence for more details.                    *}
{*                                                                            *}
{******************************************************************************}
{*                                                                            *}
{*    Diese Datei ist Freie-Software. Sie können sie weitervertreiben         *}
{*    und/oder verändern im Sinne der Bestimmungen der "GNU Library GPL"      *}
{*    der Free Software Foundation. Diese Datei wird,"wie sie ist",           *}
{*    zur Verfügung gestellt, ohne irgendeine GEWÄHRLEISTUNG                  *}
{*                                                                            *}
{******************************************************************************}
{*                          www.delphiclub.de                                 *}
{******************************************************************************}
unit TAPIHelpFunc;
{$IFDEF FPC}
{$MODE DELPHI}
{$ENDIF}

interface
{$IFDEF WINDOWS}
{$IFDEF CPU32}

uses Windows,Classes,Forms;

{$INCLUDE TAPI.INC}


procedure WaitForAsyncResult(ResultID:Longint);

function GetDataFromTAPIStruct(var Struct,OffsetStruct;Size:DWord):String;overload;
function GetDataFromTAPIStructP(var Struct,OffsetStruct;Size:DWord):Pointer;

{$ENDIF}
{$ENDIF}

implementation

{$IFDEF WINDOWS}
{$IFDEF CPU32}

uses SysUtils,TAPISystem;

procedure WaitForAsyncResult(ResultID:LongInt);
var
  M:TMsg;
  obj:TObject;
begin
  if LongInt(ResultID) >0 then
  begin
    {$IFDEF DEBUG}
    OutputDebugString('+++  Warte auf Reply  +++');
    {$ENDIF}
    while True do
    begin
      GetMessage(M,0,0,0) ;
      TranslateMessage(M);
      DispatchMessage(M);

      obj:=AppTAPIMgr.AsyncList.GetObject(ResultID);
      if Assigned(obj)=False then
      begin
        {$IFDEF DEBUG}
        OutputDebugString('+++  Reply bearbeitet +++');
        {$ENDIF}
        Break;
      end;
    end;
  end;
end;

function GetDataFromTAPIStruct(var Struct,OffsetStruct;Size:DWord):String;

begin
  Result:='';
  Result:=PChar(GetDataFromTAPIStructP(Struct,OffsetStruct,Size));
end;

function GetDataFromTAPIStructP(var Struct,OffsetStruct;Size:DWord):Pointer;
var AStr:Pointer;
begin
  {$IFDEF CPU32}
  Result:=nil;
  if Size > 0 then
  begin
    asm
      push eax
      push ebx
      push esi
      mov esi,[OffsetStruct]
      mov ebx,[esi]
      mov esi,[Struct]
      mov eax,[esi]
      add eax,ebx;
      mov [AStr],eax
      pop esi
      pop ebx
      pop eax
    end;
    Result:=AStr;
  end;
  {$ENDIF}
end;


{$ENDIF}
{$ENDIF}
end.