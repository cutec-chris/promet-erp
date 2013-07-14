{******************************************************************************}
{*        Copyright 1999-2001 by J.Friebel all rights reserved.               *}
{*        Autor           :  Jörg Friebel                                     *}
{*        Compiler        :  Delphi 4 / 5                                     *}
{*        System          :  Windows NT / 2000                                *}
{*        Projekt         :  TAPI Komponenten (TAPI Version 1.4 bis 3.0)      *}
{*        Last Update     :  10.05.2002                                       *}
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
unit DevConfProp;

interface

{$INCLUDE VERS.INC}

{$IFDEF D6_OR_GR}
uses DesignIntf,classes,DevConf,DesignEditors;
{$ELSE}
uses dsgnintf,classes,DevConf,typinfo;
{$ENDIF}

{$INCLUDE TAPI.INC}

type
  TDeviceConfigPropE =class(TClassProperty)
  private
    FDevConfig:TDevConfig;
  public
    procedure Edit;override;
    function GetAttributes:TPropertyAttributes; override;
  end;

procedure Register;

implementation

uses sysutils,TAPI,TAPICall,Forms,TAPIErr;

procedure Register;
begin
  RegisterPropertyEditor(TypeInfo(TDevConfig),nil,'',TDeviceConfigPropE);
end;

{ TDeviceConfigPropE }

procedure TDeviceConfigPropE.Edit;
var R:LongInt;
begin
  FDevConfig:=TDevConfig(GetOrdValue);
  R:=LineConfigDialog(FDevConfig.DeviceID,Application.Handle,PChar(FDevConfig.DeviceClass));
  if R<> 0 then RaiseTAPILineError(R);
end;

function TDeviceConfigPropE.GetAttributes: TPropertyAttributes;
begin
  Result:=[paDialog];
end;



end.
