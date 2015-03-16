unit uMeasurementOptions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, db, FileUtil, Forms, Controls, Graphics, Dialogs, DbCtrls,
  ColorBox, StdCtrls;

type

  { TfMeasurementOptions }

  TfMeasurementOptions = class(TForm)
    ColorButton1: TColorButton;
    Measurement: TDatasource;
    DBCheckBox1: TDBCheckBox;
    DBCheckBox2: TDBCheckBox;
    DBComboBox1: TDBComboBox;
    DBEdit1: TDBEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    procedure ColorButton1ColorChanged(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    function Execute: Boolean;
  end;

var
  fMeasurementOptions: TfMeasurementOptions;

implementation

{$R *.lfm}

{ TfMeasurementOptions }

procedure TfMeasurementOptions.ColorButton1ColorChanged(Sender: TObject);
begin
  if not (Measurement.DataSet.State=dsEdit) then
    Measurement.DataSet.Edit;
  Measurement.DataSet.FieldByName('COLOR').AsString:=ColorToString(ColorButton1.Color);
end;

function TfMeasurementOptions.Execute: Boolean;
begin
  Result := Showmodal=mrOK;
end;

end.

