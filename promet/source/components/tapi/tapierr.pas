{******************************************************************************}
{*        Copyright 1999-2001 by J.Friebel all rights reserved.               *}
{*        Autor           :  Jörg Friebel                                     *}
{*        Compiler        :  Delphi 4 / 5                                     *}
{*        System          :  Windows NT / 2000                                *}
{*        Projekt         :  TAPI Komponenten (TAPI Version 1.4 bis 3.0)      *}
{*        Last Update     :  17.02.2001                                       *}
{*        Version         :  1.0                                              *}
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
unit TAPIErr;

interface
{$IFDEF WINDOWS}
{$IFDEF CPU32}
uses sysutils,windows;

{.$R lineerror.rc}
{$INCLUDE TAPI.INC}
{$INCLUDE VERS.INC}

const TAPIERR_BADDEVICEID = DWORD($0000001);
      TAPIERR_NOTACTIVE   = DWORD($0000002);  

type ETAPIError = class(Exception)
public
  ErrorCode:LongInt;
end;

type ELineError =class(ETAPIError);


type EPhoneError =class(ETAPIError);

function GetTAPIErrorResID(var ErrorCode:Longint):Integer;
procedure RaiseTAPIError(ErrCode:LongInt);

function GetLineErrorResID(var ErrorCode:Longint):Integer;
procedure RaiseTAPILineError(ErrCode:LongInt);


function GetPhoneErrorResID(var ErrorCode:Longint):Integer;
procedure RaiseTAPIPhoneError(ErrCode:LongInt);


{$ENDIF}
{$ENDIF}

implementation

{$IFDEF WINDOWS}
{$IFDEF CPU32}

{  Allgemeines}

function GetLineErrorResID(var ErrorCode:Longint):Integer;
begin
  Result:=(ErrorCode-Longint($80000000))+100;
end;

function GetPhoneErrorResID(var ErrorCode:Longint):Integer;
begin
  Result:=(ErrorCode-Longint($90000000))+1000;
end;

function GetTAPIErrorResID(var ErrorCode:Longint):Integer;
begin
  Result:=(ErrorCode)+2000;
end;

procedure RaiseTAPIError(ErrCode:LongInt);
{var
  Error:ETAPIError;}
begin
{  {$IFDEF D5_OR_GR}
  Error := ETAPIError.CreateRes(GetTAPIErrorResID(ErrCode));
  {$ELSE}
  Error := ETAPIError.CreateRes(GetTAPIErrorResID(ErrCode),0);
  {$ENDIF}
  Error.ErrorCode := ErrCode;
  raise Error;}
end;

procedure RaiseTAPILineError(ErrCode:LongInt);
{var
  Error: ELineError;}
begin
{  {$IFDEF D5_OR_GR}
  Error := ELineError.CreateRes(GetLineErrorResID(ErrCode));
  {$ELSE}
  Error := ELineError.CreateRes(GetLineErrorResID(ErrCode),0);
  {$ENDIF}
  Error.ErrorCode := ErrCode;

   raise Error;}
end;

procedure RaiseTAPIPhoneError(ErrCode:LongInt);
{var
  Error: EPhoneError;}
begin
{  {$IFDEF D5_OR_GR}
  Error := EPhoneError.CreateRes(GetPhoneErrorResID(ErrCode));
  {$ELSE}
  Error := EPhoneError.CreateRes(GetPhoneErrorResID(ErrCode),0);
  {$ENDIF}
  Error.ErrorCode := ErrCode;
  raise Error;}
end;


{$ENDIF}
{$ENDIF}
end.