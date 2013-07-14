unit ucategoryoptions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, ComCtrls, StdCtrls, ExtCtrls,
  DbCtrls, DBGrids, Buttons, db, uOptionsFrame, uBaseDBClasses, uBaseERPDBClasses;

type
  TfCategoryOptions = class(TOptionsFrame)
    gCategory: TDBGrid;
    CategoryDS: TDatasource;
  private
    { private declarations }
    aConnection: TComponent;
    aCategory: TCategory;
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
constructor TfCategoryOptions.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  aConnection := Data.GetNewConnection;
  aCategory := TCategory.Create(Self,Data,aConnection);
  CategoryDS.DataSet := aCategory.DataSet;
end;

destructor TfCategoryOptions.Destroy;
begin
  aCategory.Destroy;
  aConnection.Destroy;
  inherited Destroy;
end;

procedure TfCategoryOptions.StartTransaction;
begin
  inherited StartTransaction;
  Data.StartTransaction(aConnection);
  aCategory.Open;
end;

procedure TfCategoryOptions.CommitTransaction;
begin
  inherited CommitTransaction;
  Data.Commit(aConnection);
end;

procedure TfCategoryOptions.RollbackTransaction;
begin
  Data.Rollback(aConnection);
  inherited RollbackTransaction;
end;

end.

