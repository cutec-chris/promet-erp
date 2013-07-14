{*******************************************************************************
Dieser Sourcecode darf nicht ohne gültige Geheimhaltungsvereinbarung benutzt werden
und ohne gültigen Vertriebspartnervertrag weitergegeben werden.
You have no permission to use this Source without valid NDA
and copy it without valid distribution partner agreement
Christian Ulrich
info@cu-tec.de
Created 01.06.2006
*******************************************************************************}
unit uListFrame;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, FileUtil, Forms, Controls, ExtCtrls, DbCtrls, Buttons, db,
  uFilterFrame,uPrometFramesInplace, uExtControls;
type

  { TfListFrame }

  TfListFrame = class(TPrometInplaceFrame)
    Bevel1: TBevel;
    Datasource: TDatasource;
    dnNavigator: TDBNavigator;
    ExtRotatedLabel1: TExtRotatedLabel;
    Panel1: TPanel;
    Panel2: TPanel;
  private
    { private declarations }
  public
    { public declarations }
    FList: TfFilter;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy;override;
    procedure SetRights(Editable : Boolean);override;
    procedure ShowFrame; override;
  end;

implementation
{$R *.lfm}
constructor TfListFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FList := TfFilter.Create(Self);
  with FList do
    begin
      Editable:=True;
      Parent := Self;
      Align := alClient;
      Show;
    end;
end;

destructor TfListFrame.Destroy;
begin
  FList.Free;
  inherited Destroy;
end;

procedure TfListFrame.SetRights(Editable: Boolean);
begin
  FList.Editable:=Editable;
  dnNavigator.Enabled:=Editable;
end;
procedure TfListFrame.ShowFrame;
begin
  inherited ShowFrame;
  FList.SetActive;
end;

end.

