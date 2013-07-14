{*******************************************************************************
Dieser Sourcecode darf nicht ohne gültige Geheimhaltungsvereinbarung benutzt werden
und ohne gültigen Vertriebspartnervertrag weitergegeben werden.
You have no permission to use this Source without valid NDA
and copy it without valid distribution partner agreement
Christian Ulrich
info@cu-tec.de
Created 01.06.2006
*******************************************************************************}
unit uArticleStorageFrame;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, FileUtil, Forms, Controls, ExtCtrls, DbCtrls, db,
  uFilterFrame,uPrometFramesInplace, uExtControls;
type
  TfArticleStorageFrame = class(TPrometInplaceFrame)
    Bevel1: TBevel;
    Datasource: TDatasource;
    dnNavigator: TDBNavigator;
    ExtRotatedLabel1: TExtRotatedLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    procedure FListFListFilterChanged(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    FList: TfFilter;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy;override;
    procedure SetRights(Editable : Boolean);override;
  end;

implementation
{$R *.lfm}
procedure TfArticleStorageFrame.FListFListFilterChanged(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to FList.gList.Columns.Count-1 do
    if FList.gList.Columns[i].FieldName = 'PLACE' then
      FList.gList.Columns[i].ReadOnly:=False;
end;
constructor TfArticleStorageFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FList := TfFilter.Create(Self);
  with FList do
    begin
      Editable:=false;
      Parent := Self;
      Align := alClient;
      FList.OnFilterChanged:=@FListFListFilterChanged;
      Show;
    end;
end;
destructor TfArticleStorageFrame.Destroy;
begin
  FList.DataSet := nil;
  FList.Free;
  inherited Destroy;
end;

procedure TfArticleStorageFrame.SetRights(Editable: Boolean);
begin
  dnNavigator.Enabled := Editable
end;

end.

