unit uStorageTypeOptions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, DBGrids, StdCtrls, db,
  uOptionsFrame, uBaseDBClasses, uBaseERPDBClasses, uMasterdata;

type
  TfStorageTypeOptions = class(TOptionsFrame)
    gStorage: TDBGrid;
    lStorage: TLabel;
    StorageTypeDS: TDatasource;
  private
    { private declarations }
    aConnection: TComponent;
    aStorageType: TStorageTypes;
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
constructor TfStorageTypeOptions.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  aConnection := Data.GetNewConnection;
  aStorageType := TStorageTypes.Create(Self,Data,aConnection);
  StorageTypeDS.DataSet := aStorageType.DataSet;
end;

destructor TfStorageTypeOptions.Destroy;
begin
  aStorageType.Destroy;
  aConnection.Destroy;
  inherited Destroy;
end;

procedure TfStorageTypeOptions.StartTransaction;
begin
  inherited StartTransaction;
  Data.StartTransaction(aConnection);
  aStorageType.Open;
end;

procedure TfStorageTypeOptions.CommitTransaction;
begin
  Data.Commit(aConnection);
  inherited CommitTransaction;
end;

procedure TfStorageTypeOptions.RollbackTransaction;
begin
  Data.Rollback(aConnection);
  inherited RollbackTransaction;
end;

end.

