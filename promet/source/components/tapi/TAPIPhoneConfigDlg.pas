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

unit TAPIPhoneConfigDlg;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  TAPI,TAPIDevices;
{$INCLUDE TAPI.INC}
type
  TTAPIPhoneConfigDlg = class(TComponent)
  private
    // Externe Zuweisung
    FDevice:TTAPIPhoneDevice;
    // API Var
    FDeviceClass:String;//TDeviceClassValue;
    // API Func
    procedure ConfigDialog;
    function GetDeviceClass: String;
    procedure SetDeviceClass(const Value: String);
  protected
    { Protected-Deklarationen }
  public
    { Public-Deklarationen }
    procedure Execute;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    { Published-Deklarationen }
    property Device:TTAPIPhoneDevice read FDevice write FDevice;
    property DeviceClass:String read GetDeviceClass write SetDeviceClass ;
  end;

procedure Register;

implementation

uses TAPIErr;

procedure Register;
begin
{$IFDEF TAPI30}
  RegisterComponents('TAPI30', [TTAPIPhoneConfigDlg]);
{$ELSE}
{$IFDEF TAPI22}
  RegisterComponents('TAPI22', [TTAPIPhoneConfigDlg]);
{$ELSE}
{$IFDEF TAPI21}
  RegisterComponents('TAPI21', [TTAPIPhoneConfigDlg]);
{$ELSE}
{$IFDEF TAPI20}
  RegisterComponents('TAPI20', [TTAPIPhoneConfigDlg]);
{$ELSE}
  RegisterComponents('TAPI', [TTAPIPhoneConfigDlg]);
{$ENDIF}
{$ENDIF}
{$ENDIF}
{$ENDIF}
end;

{ TTAPIPhonConfigDlg }

procedure TTAPIPhoneConfigDlg.ConfigDialog;
var R:Longint;
begin
  R:=PhoneConfigDialog(FDevice.ID,TForm(Owner).Handle,PChar(FDeviceClass));
  if DWord(R)>DWord($80000000) then
  begin
    RaiseTAPIPhoneError(R);
  end;
end;

constructor TTAPIPhoneConfigDlg.Create(AOwner: TComponent);
begin
  inherited;
  DeviceClass:='tapi/phone';
end;

destructor TTAPIPhoneConfigDlg.Destroy;
begin
  inherited;

end;

procedure TTAPIPhoneConfigDlg.Execute;
begin
  ConfigDialog;
end;

function TTAPIPhoneConfigDlg.GetDeviceClass: String;
begin
  Result:=FDeviceClass;
end;

procedure TTAPIPhoneConfigDlg.SetDeviceClass(const Value: String);
begin
  FDeviceClass:=Value;
end;

end.

