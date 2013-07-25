unit uCurrencyOptions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, DBGrids, db,
  uOptionsFrame, uBaseDBClasses, uBaseERPDBClasses;

type
  TfCurrencyOptions = class(TOptionsFrame)
    CurrencyDS: TDatasource;
    dgCurrency: TDBGrid;
  private
    { private declarations }
    aConnection: TComponent;
    aCurrency: TCurrency;
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
constructor TfCurrencyOptions.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  aConnection := Data.GetNewConnection;
  aCurrency := TCurrency.Create(Self,Data,aConnection);
  CurrencyDS.DataSet := aCurrency.DataSet;
end;

destructor TfCurrencyOptions.Destroy;
begin
  aCurrency.Destroy;
  aConnection.Destroy;
  inherited Destroy;
end;

procedure TfCurrencyOptions.StartTransaction;
begin
  inherited StartTransaction;
  Data.StartTransaction(aConnection);
  aCurrency.Open;
end;

procedure TfCurrencyOptions.CommitTransaction;
begin
  Data.CommitTransaction(aConnection);
  inherited CommitTransaction;
end;

procedure TfCurrencyOptions.RollbackTransaction;
begin
  Data.RollbackTransaction(aConnection);
  inherited RollbackTransaction;
end;

end.

