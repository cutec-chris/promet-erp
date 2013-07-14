{******************************************************************************}
{*        Copyright 1999-2001 by J.Friebel all rights reserved.               *}
{*        Autor           :  Jörg Friebel                                     *}
{*        Compiler        :  Delphi 4 / 5                                     *}
{*        System          :  Windows NT / 2000                                *}
{*        Projekt         :  TAPI Komponenten (TAPI Version 1.4 bis 3.0)      *}
{*        Last Update     :  23.11.2002                                       *}
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
unit DevConf;

interface

{$IFDEF WINDOWS}
{$IFDEF CPU32}

{$IFDEF FPC}
{$MODE DELPHI}
{$ENDIF}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  TAPI;

{$INCLUDE TAPI.INC}
{$INCLUDE VERS.INC}

type
  TDevConfig = class(TPersistent)
  private
    FConf:PVarString;
    FID: DWord;
    FDeviceClass: String;
    FLoaded:Boolean;
  protected

  public
    constructor Create(AID: DWord;ADeviceClass:String);
    destructor Destroy;override;
    function GetSize:Integer;
    function GetStruct:PVarString;
    procedure LoadFromDevice;
    procedure SendToDevice;
    procedure LoadFromFile(FileName:String);
    procedure LoadFromStream(Stream: TStream);
    procedure SaveToFile(FileName:String);
    procedure SaveToStream(Stream: TStream);
    property DeviceID: DWord read FID ;
    property DeviceClass:String read FDeviceClass ;
  end;

  TTAPILineDeviceConfig = Class(TComponent)
  private
    FConfig:TDevConfig;
    FID: DWord;
    FDeviceClass:String;
    procedure SetDeviceClass(const Value: String);
    procedure SetDeviceID(const Value: DWord);
  protected
    procedure UpdateConf;
  public
    constructor Create(Owner:TComponent); override;
    destructor Destroy;override;
  published
    property Config:TDevConfig read FConfig write FConfig;
    property DeviceID: DWord read FID write SetDeviceID default 0;
    property DeviceClass:String read FDeviceClass write SetDeviceClass ;
  end;


procedure Register;

{$ENDIF}
{$ENDIF}

implementation

{$IFDEF WINDOWS}
{$IFDEF CPU32}

uses TAPIErr;


procedure Register;
begin
{$IFDEF TAPI30}
  RegisterComponents('TAPI30', [TTAPILineDeviceConfig]);
{$ELSE}
{$IFDEF TAPI22}
  RegisterComponents('TAPI22', [TTAPILineDeviceConfig]);
{$ELSE}
{$IFDEF TAPI21}
  RegisterComponents('TAPI21', [TTAPILineDeviceConfig]);
{$ELSE}
{$IFDEF TAPI20}
  RegisterComponents('TAPI20', [TTAPILineDeviceConfig]);
{$ELSE}
  RegisterComponents('TAPI', [TTAPILineDeviceConfig]);
{$ENDIF}
{$ENDIF}
{$ENDIF}
{$ENDIF}
end;

{ TDevConfig }

constructor TDevConfig.Create(AID: DWord;ADeviceClass:String);
var Size:Integer;
begin
  inherited Create;
  FID:=AID;
  FDeviceClass:=ADeviceClass;
  Size:=SizeOf(TVarString)+1000;
  GetMem(FConf,Size);
  FConf^.dwTotalSize:=Size;
  FLoaded:=False;
end;

destructor TDevConfig.Destroy;
begin
  FreeMem(FConf);
  inherited;
end;

procedure TDevConfig.SendToDevice;
var R:LongInt;
begin
  R:=LineSetDevConfig(FID,FConf,SizeOf(FConf),PChar(FDeviceClass));
  if DWord(R)>DWord($80000000) then
  begin
    RaiseTAPILineError(R);
  end;
end;

procedure TDevConfig.LoadFromDevice;
var R:LongInt;
begin
  R:=LineGetDevConfig(FID,FConf,PChar(FDeviceClass));
  if DWord(R)>DWord($80000000) then
  begin
    RaiseTAPILineError(R);
  end;
  FLoaded:=True;
end;

procedure TDevConfig.LoadFromFile(FileName: String);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TDevConfig.LoadFromStream(Stream: TStream);
begin
  Stream.Position := 0;
  Stream.ReadBuffer(FConf^, SizeOf(FConf));
  FLoaded:=True;
end;

procedure TDevConfig.SaveToFile(FileName: String);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmCreate);
  try
    SaveToStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TDevConfig.SaveToStream(Stream: TStream);
begin
  Stream.WriteBuffer(FConf^, SizeOf(FConf));
end;

function TDevConfig.GetSize: Integer;
begin
  if FLoaded=False then LoadFromDevice;
  Result:=FConf.dwNeededSize;
end;

function TDevConfig.GetStruct: PVarString;
begin
  if FLoaded=False then LoadFromDevice;
  Result:=FConf;
end;

{ TTAPILineDeviceConfig }

constructor TTAPILineDeviceConfig.Create(Owner: TComponent);
begin
  inherited;
  FID:=0;
  FDeviceClass:='tapi/line';
  UpdateConf;
end;

destructor TTAPILineDeviceConfig.Destroy;
begin
  FConfig.Free;
  inherited;
end;

procedure TTAPILineDeviceConfig.SetDeviceClass(const Value: String);
begin
  if FDeviceClass <> Value then
  begin
    FDeviceClass:=Value;
    UpdateConf;
  end;
end;

procedure TTAPILineDeviceConfig.SetDeviceID(const Value: DWord);
begin
  if FID <> Value then
  begin
    FID:=Value;
    UpdateConf;
  end;
end;

procedure TTAPILineDeviceConfig.UpdateConf;
begin
  if Assigned(FConfig) then FConfig.Free;
  FConfig:=TDevConfig.Create(FID,FDeviceClass);
end;
{$ENDIF}
{$ENDIF}

end.
 