{*******************************************************************************
Dieser Sourcecode darf nicht ohne gültige Geheimhaltungsvereinbarung benutzt werden
und ohne gültigen Vertriebspartnervertrag weitergegeben werden.
You have no permission to use this Source without valid NDA
and copy it without valid distribution partner agreement
CU-TEC Christian Ulrich
info@cu-tec.de
*******************************************************************************}

unit uColorFrame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, StdCtrls, ColorBox, Dialogs,
  Graphics, uIntfStrConsts, uBaseDbInterface;

type

  { TfColorFrame }

  TfColorFrame = class(TFrame)
    bInfoColor: TColorButton;
    cbInfoColor: TColorBox;
    lActiveFieldColor: TLabel;
    lSelectColor: TLabel;
    lSystemColors: TLabel;
    procedure bInfoColorColorChanged(Sender: TObject);
    procedure cbInfoColorChange(Sender: TObject);
  private
    fCaption: string;
    FColorName: string;
    FDefaultColor: TColor;
    fInfoCaption: string;
    FName: string;
    DontUpdate : Boolean;
    FOnChangeColor: TNotifyEvent;
    function GetColor: TColor;
    procedure SetInfoCaption(const AValue: string);
    procedure SetColor(const AValue: TColor);
    procedure SetColorName(const AValue: string);
    { private declarations }
  public
    { public declarations }
  published
    property InfoCaption : string read fInfoCaption write SetInfoCaption;
    property ColorName : string read FColorName write SetColorName;
    property DefaultColor : TColor read FDefaultColor write FDefaultColor;
    property Color : TColor read GetColor write SetColor;
    property OnChangeColor : TNotifyEvent read FOnChangeColor write FOnChangeColor;
  end; 

implementation

resourcestring
  strSelectColor                = 'Farbauswahl';
  strSystemColors               = 'Systemfarben';


{ TfColorFrame }

procedure TfColorFrame.cbInfoColorChange(Sender: TObject);
begin
  bInfoColor.ButtonColor:=cbInfoColor.Selected;
  if (FColorName <> '') and (not DontUpdate) then
    with Application as IBaseDbInterface do
      DBConfig.WriteString('COLOR:'+FColorName,ColorToString(cbInfoColor.Selected));
  if Assigned(FOnChangeColor) then
    FOnChangeColor(Self);
end;

procedure TfColorFrame.bInfoColorColorChanged(Sender: TObject);
begin
  cbInfoColor.Selected:=bInfoColor.ButtonColor;
  if (FColorName <> '') and (not DontUpdate) then
    with Application as IBaseDbInterface do
      DBConfig.WriteString('COLOR:'+FColorName,ColorToString(cbInfoColor.Selected));
  if Assigned(FOnChangeColor) then
    FOnChangeColor(Self);
end;

procedure TfColorFrame.SetColor(const AValue: TColor);
begin
  cbInfoColor.Selected:=AValue;
  bInfoColor.ButtonColor:=AValue;
  if (FColorName <> '') and (not DontUpdate) then
    with Application as IBaseDbInterface do
      DBConfig.WriteString('COLOR:'+FColorName,ColorToString(AValue));
  if Assigned(FOnChangeColor) then
    FOnChangeColor(Self);
end;

function TfColorFrame.GetColor: TColor;
begin
  Result := cbInfoColor.Selected;
end;

procedure TfColorFrame.SetColorName(const AValue: string);
var
  Col: TColor;
begin
  FColorName:=AValue;
  try
    with Application as IBaseDbInterface do
      Col := StringToColor(DBConfig.ReadString('COLOR:'+FColorName,ColorToString(DefaultColor)));
  except
    Col := DefaultColor;
  end;
  DontUpdate := True;
  cbInfoColor.Selected:=Col;
  bInfoColor.ButtonColor:=Col;
  if Assigned(FOnChangeColor) then
    FOnChangeColor(Self);
  DontUpdate := False;
end;

procedure TfColorFrame.SetInfoCaption(const AValue: string);
begin
  if fInfoCaption=AValue then exit;
  fInfoCaption:=AValue;
  lActiveFieldColor.Caption:=AValue;
  lSelectColor.Caption:=strSelectColor;
  lSystemColors.Caption:=strSystemColors;
end;

initialization
  {$I ucolorframe.lrs}

end.
