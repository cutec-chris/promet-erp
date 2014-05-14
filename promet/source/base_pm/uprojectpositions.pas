unit uprojectpositions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, db, FileUtil, Forms, Controls, ExtCtrls, StdCtrls, DbCtrls,
  uPositionFrame,uBaseDBClasses,uPrometFramesInplace,uProjects;

type

  { TfProjectPositions }

  TfProjectPositions = class(TPrometInplaceFrame)
    DBEdit2: TDBEdit;
    lTargetCosts1: TLabel;
    Project: TDatasource;
    DBEdit1: TDBEdit;
    lTargetCosts: TLabel;
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
    procedure SetLanguage;
    procedure SetFocus; override;
    procedure ShowFrame; override;
  end;

implementation
{$R *.lfm}
constructor TfProjectPositions.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPosFrame := TfPosition.Create(Self);
  FPosFrame.Parent := Self;
  FPosFrame.Align:=alClient;
  FPosFrame.Show;
  FPosFrame.GridView.DefaultRows:='GLOBALWIDTH:%;POSNO:32;TEXT:301;SELLPRICE:68;REALPRICE:68;QUANTITY:40;QUANTITYU:62;POSPRICE:68;REALPOSPRICE:68;';
  FPosFrame.BaseName:='PROJECTS';
end;
destructor TfProjectPositions.Destroy;
begin
  FPosFrame.Free;
  inherited Destroy;
end;
procedure TfProjectPositions.SetDataSet(const AValue: TBaseDBDataSet);
begin
  FPosFrame.Dataset := AValue;
  Project.DataSet := nil;
  if Assigned(AValue) then
    begin
      FPosFrame.SetLanguage;
      Project.DataSet := TprojectPositions(AValue).Project.DataSet;
    end;
end;
procedure TfProjectPositions.SetRights(Editable: Boolean);
begin
  FPosFrame.SetRights(Editable);
end;
procedure TfProjectPositions.SetLanguage;
begin
  FPosFrame.SetLanguage;
end;
procedure TfProjectPositions.SetFocus;
begin
  if CanFocus and Visible then
    inherited;
  FPosFrame.SetFocus;
end;

procedure TfProjectPositions.ShowFrame;
begin
  inherited ShowFrame;
  SetFocus;
end;

end.

