{*******************************************************************************
Dieser Sourcecode darf nicht ohne gültige Geheimhaltungsvereinbarung benutzt werden
und ohne gültigen Vertriebspartnervertrag weitergegeben werden.
You have no permission to use this Source without valid NDA
and copy it without valid distribution partner agreement
Christian Ulrich
info@cu-tec.de
Created 01.06.2006
*******************************************************************************}
unit uarticlerepairframe;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, FileUtil, Forms, Controls, DbCtrls, uExtControls, db,
  uMasterdata,uPrometFramesInplace;
type
  TFArticlerepairFrame = class(TPrometInplaceFrame)
    Assembly: TDatasource;
    Part: TDatasource;
    dnAssembly: TDBNavigator;
    dnPart: TDBNavigator;
    gAssemblys: TExtDBGrid;
    gParts: TExtDBGrid;
  private
    FMasterdata: TMasterdata;
    procedure SetMasterdata(const AValue: TMasterdata);
    { private declarations }
  public
    { public declarations }
    property Masterdata : TMasterdata read FMasterdata write SetMasterdata;
    procedure SetRights(Editable : Boolean);override;
  end; 
implementation
{$R *.lfm}
procedure TFArticlerepairFrame.SetMasterdata(const AValue: TMasterdata);
begin
  if FMasterdata=AValue then exit;
  FMasterdata:=AValue;
  Assembly.DataSet := AValue.Assembly.DataSet;
  AValue.Assembly.Parts.Open;
  Part.DataSet := AValue.Assembly.Parts.DataSet;
end;

procedure TFArticlerepairFrame.SetRights(Editable: Boolean);
begin
  Enabled := Editable;
end;

end.

