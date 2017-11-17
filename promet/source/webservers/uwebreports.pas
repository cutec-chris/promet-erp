unit uwebreports;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LR_Class, LR_DBSet, LR_BarC, LR_RRect, LR_Shape,
  LR_E_TXT, LR_E_HTM, lr_e_pdf,uBaseDbClasses,uBaseDBInterface,uBaseDatasetInterfaces,db;

type

  { TfWebReports }

  TfWebReports = class(TDataModule)
    frBarCodeObject1: TfrBarCodeObject;
    frDBDataSet1: TfrDBDataSet;
    frHTMExport1: TfrHTMExport;
    Report: TfrReport;
    frRoundRectObject1: TfrRoundRectObject;
    frShapeObject1: TfrShapeObject;
    frTextExport1: TfrTextExport;
    frTNPDFExport1: TfrTNPDFExport;
  private
    { private declarations }
  public
    { public declarations }
    LastError : string;
    function ExportToPDF(aFile: string): Boolean;
    procedure RegisterDataSet(aDataSet : TBaseDBDataset);
  end;

var
  fWebReports: TfWebReports;

implementation

uses uBaseApplication,Utils,LCLVersion;

{$R *.lfm}

{ TfWebReports }

function TfWebReports.ExportToPDF(aFile : string): Boolean;
var
  i: Integer;
begin
  Result := False;
  LastError:='Unknown Error';
  try
  FOR i := 0 TO frFiltersCount - 1 DO
     if pos('PDF',Uppercase(frFilters[i].FilterDesc)) > 0 then
      if Report.PrepareReport then
        begin
          {$IF ((LCL_MAJOR >= 1) and (LCL_MINOR > 5))}
          Report.ExportTo(ExportFilters[i].ClassRef,aFile);
          {$ELSE}
          Report.ExportTo(frFilters[i].ClassRef,aFile);
          {$ENDIF}
          Result := True;
        end;
    if not Result then
      LastError:='Report not found !';
  except
    on e : Exception do
      LastError:=e.Message;
  end;
end;

procedure TfWebReports.RegisterDataSet(aDataSet: TBaseDBDataset);
var
  i: Integer;
  aDS: TfrDBDataSet;
  aDSo: TDataSource;
begin
  i := 0;
  while i < ComponentCount do
    if Components[i] is TfrDBDataSet then
      Components[i].Free
    else inc(i);
  while i < ComponentCount do
    if Components[i] is TDatasource then
      Components[i].Free
    else inc(i);
  aDS := TfrDBDataSet.Create(Self);
  Self.InsertComponent(aDS);
  aDSo := TDataSource.Create(Self);
  Self.InsertComponent(aDSo);
  with aDataSet.DataSet as IBaseManageDB do
    begin
      aDS.Name:='P'+Tablename;
      aDSo.Name:=Tablename;
      aDS.DataSource := aDSo;
      aDSo.DataSet := aDataSet.DataSet;
      aDataSet.Open;
      writeln(aDataSet.DataSet.RecordCount);
    end;
end;

end.

