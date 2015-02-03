unit uStateOptions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, ComCtrls, StdCtrls, ExtCtrls,
  DbCtrls, DBGrids, Buttons, db, uOptionsFrame, uBaseDBClasses, uBaseERPDBClasses;

type
  TfStateOptions = class(TOptionsFrame)
    gStates: TDBGrid;
    StatesDS: TDatasource;
  private
    { private declarations }
    aConnection: TComponent;
    aStates: TStates;
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
constructor TfStateOptions.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  aConnection := Data.GetNewConnection;
  aStates := TStates.CreateEx(Self,Data,aConnection);
  StatesDS.DataSet := aStates.DataSet;
end;

destructor TfStateOptions.Destroy;
begin
  aStates.Destroy;
  aConnection.Destroy;
  inherited Destroy;
end;

procedure TfStateOptions.StartTransaction;
begin
  inherited StartTransaction;
  Data.StartTransaction(aConnection);
  aStates.Open;
end;

procedure TfStateOptions.CommitTransaction;
begin
  inherited CommitTransaction;
  Data.CommitTransaction(aConnection);
end;

procedure TfStateOptions.RollbackTransaction;
begin
  Data.RollbackTransaction(aConnection);
  inherited RollbackTransaction;
end;

end.

