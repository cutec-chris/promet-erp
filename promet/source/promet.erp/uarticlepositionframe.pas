{*******************************************************************************
Dieser Sourcecode darf nicht ohne gültige Geheimhaltungsvereinbarung benutzt werden
und ohne gültigen Vertriebspartnervertrag weitergegeben werden.
You have no permission to use this Source without valid NDA
and copy it without valid distribution partner agreement
Christian Ulrich
info@cu-tec.de
Created 01.06.2006
*******************************************************************************}
unit uArticlePositionFrame;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, ExtCtrls,
  uExtControls,uPositionFrame,uBaseDBClasses,uPrometFramesInplace;
type

  { TfArticlePositionFrame }

  TfArticlePositionFrame = class(TPrometInplaceFrame)
    cbPLType: TExtDBCombobox;
    lType: TLabel;
    Panel1: TPanel;
  private
    { private declarations }
    FPosFrame: TfPosition;
  public
    { public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy;override;
    procedure SetDataSet(const AValue: TBaseDBDataSet);
    procedure SetRights(Editable : Boolean);override;
    procedure SetFocus; override;
    procedure ShowFrame; override;
  end;
implementation
{$R *.lfm}
resourcestring
  strPieceListTypeList          = 'L Im lager direkt buchen';
  strPieceListTypeOrder         = 'O In Auftrag auflösen';
  strPieceListTypeProduction    = 'P In produktion auflösen';
constructor TfArticlePositionFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPosFrame := TfPosition.Create(Self);
  FPosFrame.Parent := Self;
  FPosFrame.Align:=alClient;
  FPosFrame.Show;
  FPosFrame.BaseName:='MASTERDATA';
  cbPLType.Items.Add(strPieceListTypeList);
  cbPLType.Items.Add(strPieceListTypeOrder);
  cbPLType.Items.Add(strPieceListTypeProduction);
end;
destructor TfArticlePositionFrame.Destroy;
begin
  FPosFrame.Free;
  inherited Destroy;
end;
procedure TfArticlePositionFrame.SetDataSet(const AValue: TBaseDBDataSet);
begin
  FPosFrame.Dataset := AValue;
end;
procedure TfArticlePositionFrame.SetRights(Editable: Boolean);
begin
  FPosFrame.SetRights(Editable);
end;

procedure TfArticlePositionFrame.SetFocus;
begin
  if CanFocus and Visible then
    inherited;
  FPosFrame.SetFocus;
end;

procedure TfArticlePositionFrame.ShowFrame;
begin
  inherited ShowFrame;
  SetFocus;
end;

end.

