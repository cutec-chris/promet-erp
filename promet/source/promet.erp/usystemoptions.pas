unit uSystemOptions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, DBGrids, db,
  uOptionsFrame, uBaseDbClasses, uBaseERPDbClasses;
type
  TfSystemOptions = class(TOptionsFrame)
    gNumbers: TDBGrid;
    gPaymentTargets: TDBGrid;
    gUnits: TDBGrid;
    gVat: TDBGrid;
    lNumbers: TLabel;
    lPaymentTargets: TLabel;
    lUnits: TLabel;
    lVAT: TLabel;
    NumbersDS: TDatasource;
    PaymentTargetsDS: TDatasource;
    UnitsDS: TDatasource;
    VatDS: TDatasource;
  private
    { private declarations }
    aConnection: TComponent;
    aNumbers: TNumbersets;
    aVat: TVat;
    aUnits: TUnits;
    aPaymentTargets: TPaymentTargets;
  public
    { public declarations }
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy;override;
    procedure StartTransaction;override;
    procedure CommitTransaction;override;
    procedure RollbackTransaction;override;
  end;
implementation
{$R *.lfm}
uses uData;
constructor TfSystemOptions.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  aConnection := Data.GetNewConnection;
  aVat := TVat.Create(Self,Data,aConnection);
  VatDS.DataSet := aVat.DataSet;
  aNumbers := TNumbersets.Create(Self,Data,aConnection);
  NumbersDS.DataSet := aNumbers.DataSet;
  aUnits := TUnits.Create(Self,Data,aConnection);
  UnitsDS.DataSet := aUnits.DataSet;
  aPaymentTargets := TPaymentTargets.Create(Self,Data,aConnection);
  PaymentTargetsDS.DataSet := aPaymentTargets.DataSet;
end;
destructor TfSystemOptions.Destroy;
begin
  aNumbers.Destroy;
  aVat.Destroy;
  aUnits.Destroy;
  aPaymentTargets.Destroy;
  aConnection.Destroy;
  inherited Destroy;
end;
procedure TfSystemOptions.StartTransaction;
begin
  inherited StartTransaction;
  Data.StartTransaction(aConnection);
  aVat.Open;
  aUnits.Open;
  aPaymentTargets.Open;
  aNumbers.Open;
end;
procedure TfSystemOptions.CommitTransaction;
begin
  inherited CommitTransaction;
  Data.Commit(aConnection);
end;
procedure TfSystemOptions.RollbackTransaction;
begin
  Data.Rollback(aConnection);
  inherited RollbackTransaction;
end;
end.

