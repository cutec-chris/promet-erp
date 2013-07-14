unit uLanguageOptions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, ExtCtrls, StdCtrls, DBGrids,
  DbCtrls, db, uOptionsFrame, uBaseDBClasses, uBaseERPDbClasses;

type
  TfLanguageOptions = class(TOptionsFrame)
    CountriesDS: TDatasource;
    dcCurrency: TDBComboBox;
    dgCountrys: TDBGrid;
    dgDispatchTypes: TDBGrid;
    DispatchTypesDS: TDatasource;
    eDateformat: TDBEdit;
    eTimeformat: TDBEdit;
    gLanguages: TDBGrid;
    LanguagesDS: TDatasource;
    lCompClose: TLabel;
    lCountries: TLabel;
    lCurrency: TLabel;
    lDateformat: TLabel;
    lLanguages: TLabel;
    lSatutation: TLabel;
    lShipping: TLabel;
    lTimeformat: TLabel;
    lTitles: TLabel;
    mCompClose: TDBMemo;
    mSatutation: TDBMemo;
    mTitles: TDBMemo;
    Panel3: TPanel;
  private
    { private declarations }
    aConnection: TComponent;
    aLanguages: TLanguages;
    aCountries: TCountries;
    aDispatchTypes: TDispatchTypes;
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
constructor TfLanguageOptions.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  aConnection := Data.GetNewConnection;
  aLanguages := TLanguages.Create(Self,Data,aConnection);
  LanguagesDS.DataSet := aLanguages.DataSet;
  aCountries := TCountries.Create(Self,Data,aConnection);
  CountriesDS.DataSet := aCountries.DataSet;
  aDispatchTypes := TDispatchTypes.Create(Self,Data,aConnection);
  DispatchTypesDS.DataSet := aDispatchTypes.DataSet;
end;

destructor TfLanguageOptions.Destroy;
begin
  aLanguages.Destroy;
  aCountries.Destroy;
  aDispatchTypes.Destroy;
  aConnection.Destroy;
  inherited Destroy;
end;

procedure TfLanguageOptions.StartTransaction;
begin
  inherited StartTransaction;
  Data.StartTransaction(aConnection);
  aLanguages.Open;
  aCountries.Open;
  aDispatchTypes.Open;
end;

procedure TfLanguageOptions.CommitTransaction;
begin
  Data.Commit(aConnection);
  inherited CommitTransaction;
end;

procedure TfLanguageOptions.RollbackTransaction;
begin
  Data.Rollback(aConnection);
  inherited RollbackTransaction;
end;

end.

