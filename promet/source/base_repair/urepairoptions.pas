unit uRepairOptions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, ExtCtrls, StdCtrls, DbCtrls,
  DBGrids, db, uOptionsFrame, uBaseDBClasses, uBaseERPDBClasses;

type
  TfRepairOptions = class(TOptionsFrame)
    gRepairProblem: TDBGrid;
    RepairDS: TDatasource;
    pMandantDetails: TPanel;
  private
    { private declarations }
    aConnection: TComponent;
    aRepair : TRepairProblems;
    aMandant: TMandantDetails;
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
constructor TfRepairOptions.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  aConnection := Data.GetNewConnection;
  aRepair := TRepairProblems.CreateEx(Self,Data,aConnection);
  RepairDS.DataSet := aRepair.DataSet;
end;

destructor TfRepairOptions.Destroy;
begin
  aRepair.Destroy;
  aConnection.Destroy;
  inherited Destroy;
end;

procedure TfRepairOptions.StartTransaction;
begin
  inherited StartTransaction;
  Data.StartTransaction(aConnection);
  aMandant.Open;
end;

procedure TfRepairOptions.CommitTransaction;
begin
  Data.CommitTransaction(aConnection);
  inherited CommitTransaction;
end;

procedure TfRepairOptions.RollbackTransaction;
begin
  Data.RollbackTransaction(aConnection);
  inherited RollbackTransaction;
end;

end.

