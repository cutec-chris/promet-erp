{******************************************************************************}
{*        Copyright 1999-2001 by J.Friebel all rights reserved.               *}
{*        Autor           :  Jörg Friebel                                     *}
{*        Compiler        :  Delphi 4 / 5                                     *}
{*        System          :  Windows NT / 2000                                *}
{*        Projekt         :  TAPI Komponenten (TAPI Version 1.4 bis 3.0)      *}
{*        Last Update     :  02.06.2000                                       *}
{*        Version         :  0.5                                              *}
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
unit TAPICurVer;

interface
{$IFDEF WINDOWS}
{$IFDEF CPU32}
{$INCLUDE TAPI.INC}


const
{$IFDEF TAPI10}
    TAPI_CURRENT_VERSION=$00010000;
{$ENDIF}



{$IFDEF TAPI30}
  TAPI_CURRENT_VERSION=$00030000;
{$ELSE}
  {$IFDEF TAPI22}
    TAPI_CURRENT_VERSION=$00020002;
  {$ELSE}
    {$IFDEF TAPI21}
      TAPI_CURRENT_VERSION=$00020001;
    {$ELSE}
      {$IFDEF TAPI20}
        TAPI_CURRENT_VERSION=$00020000;
      {$ELSE}
        TAPI_CURRENT_VERSION=$00010004;
      {$ENDIF}
    {$ENDIF}
  {$ENDIF}
{$ENDIF}


{$ENDIF}
{$ENDIF}

implementation

end.
 