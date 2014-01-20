{******************************************************************************}
{*        Copyright 1999-2001 by J.Friebel all rights reserved.               *}
{*        Autor           :  Jörg Friebel                                     *}
{*        Compiler        :  Delphi 4 / 5                                     *}
{*        System          :  Windows NT / 2000                                *}
{*        Projekt         :  TAPI Komponenten (TAPI Version 1.4 bis 3.0)      *}
{*        Last Update     :  21.03.2002                                       *}
{*        Version         :  1.1                                              *}
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
unit TAPIProvProp;
{$IFDEF FPC}
{$MODE DELPHI}{$H+}
{$ENDIF}

interface

{$INCLUDE VERS.INC}

{$IFDEF D6_OR_GR}
uses DesignIntf,classes,TAPIProv,DesignEditors;
{$ELSE}
uses dsgnintf,classes,TAPIProv,typinfo;
{$ENDIF}

{$INCLUDE TAPI.INC}

type
TFileNamePropE =class(TEnumProperty)
private
  FProvList:TTAPIProviderList;
public
  procedure Edit;override;
  function GetAttributes:TPropertyAttributes; override;
  function GetValue: string; override;
  procedure SetValue(const Value: string); override;
  procedure GetValues(Proc: TGetStrProc); override;
protected
end;

procedure Register;

implementation

uses sysutils;

procedure Register;
begin
  RegisterPropertyEditor(TypeInfo(TTAPIProviderList),TTAPIProvider,'',TFileNamePropE);
end;


{ TFileNamePropE }

procedure TFileNamePropE.Edit;
begin
  inherited;
end;

function TFileNamePropE.GetAttributes: TPropertyAttributes;
begin
  Result:=[paValueList];
end;

function TFileNamePropE.GetValue: string;
var I:Integer;
begin
   FProvList:=TTAPIProviderList(GetOrdValue);
   I:=FProvList.ProviderIndex;
   result:=FProvList[i];
end;

procedure TFileNamePropE.GetValues(Proc:TGetStrProc);
var i:Integer;
    NP:Integer;
    S:String;
begin
  FProvList:=TTAPIProviderList(GetOrdvalue);
  NP:=FProvList.NumProviders;
  for i:=1 to NP do
  begin
    S:= FProvList.GetProviderFileName(i);
    Proc(s);
  end;
end;

procedure TFileNamePropE.SetValue(const Value: string);
var I,R,NP:Integer;
begin
  R:=1;
  FProvList:=TTAPIProviderList(GetOrdvalue);
  NP:=FProvList.NumProviders;
  for i:=1 to NP do
  begin
    if CompareText(Value, FProvList.GetProviderFileName(i)) = 0 then R:=i;
  end;
  FProvList.ProviderIndex:=R;
  Modified;
end;



end.
