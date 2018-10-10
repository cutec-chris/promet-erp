unit uOrderAdressDetail;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, db, FileUtil, Forms, Controls, Graphics, Dialogs, DbCtrls,
  StdCtrls, ButtonPanel, DBGrids, uExtControls, typinfo;

type

  { TfEditAdress }

  TfEditAdress = class(TForm)
    ButtonPanel1: TButtonPanel;
    cbLand: TExtDBCombobox;
    cbTitle: TDBComboBox;
    cDatasource: TDataSource;
    eAdditional: TDBEdit;
    eCallingName: TDBEdit;
    eCity: TDBEdit;
    eName: TDBEdit;
    eZip: TDBEdit;
    lAdditional: TLabel;
    lCallingName: TLabel;
    lCity: TLabel;
    lLand: TLabel;
    lName: TLabel;
    lPostalCode: TLabel;
    lStreet: TLabel;
    lTitle: TLabel;
    mAddress: TDBMemo;
  private
    { private declarations }
  public
    { public declarations }
    function Execute : Boolean;
    procedure SetLanguage;
  end;

var
  fEditAdress: TfEditAdress;

implementation

{$R *.lfm}

{ TfEditAdress }

function TfEditAdress.Execute: Boolean;
begin
  if not Assigned(Self) then
    begin
      fEditAdress := TfEditAdress.Create(Application);
      Self := fEditAdress;
    end;
  Result := fEditAdress.ShowModal = mrOK;
end;

procedure TfEditAdress.SetLanguage;
begin
  if not Assigned(Self) then
    begin
      fEditAdress := TfEditAdress.Create(Application);
      Self := fEditAdress;
    end;
end;

end.

