unit uDataSet;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil,  Forms, Controls, Graphics, Dialogs,
  DBGrids, DbCtrls, db;

type

  { TfDataSet }

  TfDataSet = class(TForm)
    Datasource: TDatasource;
    DBGrid1: TDBGrid;
    DBNavigator1: TDBNavigator;
    procedure FormDestroy(Sender: TObject);
  private
    FDataSet: TDataSet;
    procedure SetDataSet(AValue: TDataSet);
    { private declarations }
  public
    { public declarations }
    property DataSet : TDataSet read FDataSet write SetDataSet;
  end;

var
  fDataSet: TfDataSet;

implementation
{$R *.lfm}
procedure TfDataSet.FormDestroy(Sender: TObject);
begin
  FDataSet.Free;
end;

procedure TfDataSet.SetDataSet(AValue: TDataSet);
begin
  if FDataSet=AValue then Exit;
  FDataSet:=AValue;
  Datasource.DataSet:=AValue;
end;

initialization

end.

