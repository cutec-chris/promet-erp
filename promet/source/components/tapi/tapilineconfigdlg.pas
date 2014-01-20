{******************************************************************************}
{*        Copyright 1999-2001 by J.Friebel all rights reserved.               *}
{*        Autor           :  Jörg Friebel                                     *}
{*        Compiler        :  Delphi 4 / 5 / 6                                 *}
{*        System          :  Windows NT / 2000                                *}
{*        Projekt         :  TAPI Komponenten (TAPI Version 1.4 bis 3.0)      *}
{*        Last Update     :  11.05.2002                                       *}
{*        Version         :  2.0                                              *}
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

unit TAPILineConfigDlg;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  TAPI,TAPIDevices;
{$INCLUDE TAPI.INC}
type
  TTAPILineConfigDlg = class(TComponent)
  private
    // Externe Zuweisung
    FDevice:TTAPILineDevice;
    // API Var
    FDeviceClass:String;
    // API Func
    procedure ConfigDialog;virtual;
    function GetDeviceClass: String;
    procedure SetDeviceClass(const Value: String);
    procedure SetDevice(const Value: TTAPILineDevice);virtual;
  protected
    { Protected-Deklarationen }
  public
    { Public-Deklarationen }
    procedure Execute;
    constructor Create(AOwner: TComponent);override ;
    destructor Destroy; override;
  published
    { Published-Deklarationen }
    property Device:TTAPILineDevice read FDevice write SetDevice;
    property DeviceClass:String read GetDeviceClass write SetDeviceClass ;
  end;

  TTAPILineConfigDlgEdit = class(TTAPILineConfigDlg)
  private
    FFileName:String;
    FDeviceConfigIn:PVarString;
    FDeviceConfigOut:PVarString;
    procedure ConfigDialog;override;
    procedure SetDevice(const Value: TTAPILineDevice);override;
  protected
  public
    constructor Create(AOwner: TComponent);override ;
    destructor Destroy; override;
    property DeviceConfigIn:PVarString read FDeviceConfigIn write FDeviceConfigIn;
    property DeviceConfigOut:PVarString read FDeviceConfigOut write FDeviceConfigOut;
  published
    property FileName:String read FFileName write FFileName;
  end;

procedure Register;

implementation

uses TAPIErr;

procedure Register;
begin
{$IFDEF TAPI30}
  RegisterComponents('TAPI30', [TTAPILineConfigDlg]);
  RegisterComponents('TAPI30', [TTAPILineConfigDlgEdit]);
{$ELSE}
{$IFDEF TAPI22}
  RegisterComponents('TAPI22', [TTAPILineConfigDlg]);
  RegisterComponents('TAPI22', [TTAPILineConfigDlgEdit]);
{$ELSE}
{$IFDEF TAPI21}
  RegisterComponents('TAPI21', [TTAPILineConfigDlg]);
  RegisterComponents('TAPI21', [TTAPILineConfigDlgEdit]);
{$ELSE}
{$IFDEF TAPI20}
  RegisterComponents('TAPI20', [TTAPILineConfigDlg]);
  RegisterComponents('TAPI20', [TTAPILineConfigDlgEdit]);
{$ELSE}
  RegisterComponents('TAPI', [TTAPILineConfigDlg]);
  RegisterComponents('TAPI', [TTAPILineConfigDlgEdit]);
{$ENDIF}
{$ENDIF}
{$ENDIF}
{$ENDIF}
end;


{ TTAPILineConfigDlg }

procedure TTAPILineConfigDlg.ConfigDialog;
var R:Longint;
begin
  R:=LineConfigDialog(FDevice.ID,TForm(Owner).Handle,PChar(FDeviceClass));
  if DWord(R)>DWord($80000000) then
  begin
    RaiseTAPILineError(R);
  end;
end;

constructor TTAPILineConfigDlg.Create(AOwner: TComponent);
begin
  inherited;
  FDeviceClass:='tapi/line';
end;

destructor TTAPILineConfigDlg.Destroy;
begin
  inherited;

end;

procedure TTAPILineConfigDlg.Execute;
begin
  ConfigDialog;
end;

function TTAPILineConfigDlg.GetDeviceClass: String;
begin
  Result:=FDeviceClass;
end;

procedure TTAPILineConfigDlg.SetDevice(const Value: TTAPILineDevice);
begin
  FDevice := Value;
end;

procedure TTAPILineConfigDlg.SetDeviceClass(const Value: String);
begin
  FDeviceClass:=Value;
end;

{ TTAPILineConfigDlgEdit }

procedure TTAPILineConfigDlgEdit.ConfigDialog;
var R:LongInt;
begin
  IF Assigned(FDeviceConfigIn)=False then FDeviceConfigIn:=Device.DevConfig.Config.GetStruct;
  R:=LineConfigDialogEdit(FDevice.ID,TForm(Owner).Handle,PChar(FDeviceClass),FDeviceConfigIn,SizeOf(FDeviceConfigIn),FDeviceConfigOut);
  if DWord(R)>DWord($80000000) then
  begin
    RaiseTAPILineError(R);
  end;
end;

constructor TTAPILineConfigDlgEdit.Create(AOwner: TComponent);
begin
  inherited;
  GetMem(FDeviceConfigOut,SizeOf(TVarString)+1000);
end;

destructor TTAPILineConfigDlgEdit.Destroy;
begin
  FreeMem(FDeviceConfigOut);
  inherited;

end;

procedure TTAPILineConfigDlgEdit.SetDevice(const Value: TTAPILineDevice);
begin
  inherited;
  FDeviceConfigIn:=FDevice.DevConfig.Config.GetStruct;
end;

end.
