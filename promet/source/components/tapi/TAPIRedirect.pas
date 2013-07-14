{******************************************************************************}
{*        Copyright 1999-2001 by J.Friebel all rights reserved.               *}
{*        Autor           :  Jörg Friebel                                     *}
{*        Compiler        :  Delphi 4 / 5                                     *}
{*        System          :  Windows NT / 2000                                *}
{*        Projekt         :  TAPI Komponenten (TAPI Version 1.4 bis 3.0)      *}
{*        Last Update     :  18.12.2001                                       *}
{*        Version         :  0.1                                              *}
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

unit TAPIRedirect;

interface
{$IFDEF WINDOWS}
{$IFDEF CPU32}
{$INCLUDE TAPI.INC}

uses Windows, Classes, TAPI ,TAPIAddress;

type
  TTAPIRedirectAddress = class(TTAPIAddress)
  private
  protected
    
  public
    function Redirect:LongWord;
  end;

procedure Register;

{$ENDIF}
{$ENDIF}

implementation

{$IFDEF WINDOWS}
{$IFDEF CPU32}

uses TAPISystem,TAPIErr, SysUtils,TAPIServices;

procedure Register;
begin
{$IFDEF TAPI30}
  RegisterComponents('TAPI30', [TTAPIRedirectAddress]);
{$ELSE}
{$IFDEF TAPI22}
  RegisterComponents('TAPI22', [TTAPIRedirectAddress]);
{$ELSE}
{$IFDEF TAPI21}
  RegisterComponents('TAPI21', [TTAPIRedirectAddress]);
{$ELSE}
{$IFDEF TAPI20}
  RegisterComponents('TAPI20', [TTAPIRedirectAddress]);
{$ELSE}
  RegisterComponents('TAPI', [TTAPIRedirectAddress]);
{$ENDIF}
{$ENDIF}
{$ENDIF}
{$ENDIF}
end;

{ TTAPIRedirectAddress }

function TTAPIRedirectAddress.Redirect:LongWord;
var R:Longint;
begin
  Result:=0;
  if Assigned(InBoundCall) then
  begin
    if InBoundCall.Handle > DWORD(0)then
    begin
      {$IFDEF DEBUG}
      OutputDebugString(PChar('Redirect Address='+DialableAddress));
      {$ENDIF}
      R:=LineRedirect(InBoundCall.Handle,PChar(DialableAddress),CountryCode);
      {$IFDEF DEBUG}
      OutputDebugString(PChar('Redirect AsynID='+IntToStr(R)));
      {$ENDIF}
      if DWord(R)>DWord($80000000) then
      begin
        RaiseTAPILineError(R);
      end
      else
      begin
        AppTAPIMgr.AsyncList.Add(afRedirect,R,InBoundCall);
        Result:=R;
      end;
    end;
  end;
end;

{$ENDIF}
{$ENDIF}
end.