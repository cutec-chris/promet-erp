unit uUserfieldDefOptions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, DBGrids, StdCtrls, db,
  uOptionsFrame, uBaseDBClasses;

type
  TfUserFieldOptions = class(TOptionsFrame)
    gUserfields: TDBGrid;
    lOnlyOnNextStart: TLabel;
    UserfielddefsDS: TDatasource;
  private
    { private declarations }
    aConnection: TComponent;
    aUserFieldDefs: TUserfielddefs;
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
constructor TfUserFieldOptions.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  aConnection := Data.GetNewConnection;
  aUserFieldDefs := TUserFieldDefs.Create(Self,Data,aConnection);
  UserFieldDefsDS.DataSet := aUserFieldDefs.DataSet;
end;

destructor TfUserFieldOptions.Destroy;
begin
  aUserFieldDefs.Destroy;
  try
    aConnection.Destroy;
  except
  end;
  inherited Destroy;
end;

procedure TfUserFieldOptions.StartTransaction;
begin
  inherited StartTransaction;
  Data.StartTransaction(aConnection);
  aUserFieldDefs.Open;
end;

procedure TfUserFieldOptions.CommitTransaction;
begin
  Data.CommitTransaction(aConnection);
  inherited CommitTransaction;
end;

procedure TfUserFieldOptions.RollbackTransaction;
begin
  Data.RollbackTransaction(aConnection);
  inherited RollbackTransaction;
end;

end.

