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
    procedure ReportGetValue(const ParName: String; var ParValue: Variant);
  private
    { private declarations }
  public
    { public declarations }
    LastError : string;
    function ExportToPDF(aFile: string): Boolean;
    procedure RegisterDataSet(aDataSet: TDataset; DeleteComponents: Boolean=True;
      aIdent: Integer=0);
  end;

var
  fWebReports: TfWebReports;

implementation

uses uBaseApplication,Utils,LCLVersion,dateutils;

{$R *.lfm}

{ TfWebReports }

procedure TfWebReports.ReportGetValue(const ParName: String;
  var ParValue: Variant);
begin
  if uppercase(ParName) = 'WEEKNO' then
    ParValue := IntToStr(WeekOfTheYear(Now()))
  else if uppercase(ParName) = 'WEEKDAY' then
    ParValue := IntToStr(DayOfTheWeek(Now()))
  ;
end;

function TfWebReports.ExportToPDF(aFile : string): Boolean;
var
  i: Integer;
begin
  Result := False;
  LastError:='Unknown Error';
  try
  {$IF ((LCL_MAJOR >= 1) and (LCL_MINOR > 5))}
  FOR i := 0 TO ExportFilters.Count - 1 DO
     if pos('PDF',Uppercase(ExportFilters[i].FilterDesc)) > 0 then
  {$ELSE}
  FOR i := 0 TO frFiltersCount - 1 DO
     if pos('PDF',Uppercase(frFilters[i].FilterDesc)) > 0 then
  {$ENDIF}
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

procedure TfWebReports.RegisterDataSet(aDataSet: TDataset;DeleteComponents : Boolean = True;aIdent : Integer = 0);
var
  i: Integer;
  aDS: TfrDBDataSet;
  aDSo: TDataSource;
  NewTableName: String;
begin
  i := 0;
  if DeleteComponents then
    begin
      while i < ComponentCount do
        if Components[i] is TfrDBDataSet then
          Components[i].Free
        else inc(i);
      while i < ComponentCount do
        if Components[i] is TDatasource then
          Components[i].Free
        else inc(i);
    end;
  try
    with aDataSet as IBaseManageDB do
      begin
        case lowercase(TableName) of
        'orderaddr':NewTableName := 'OrderAddress';
        else
          NewTableName := TableName;
        end;
        if (FindComponent('P'+NewTableName)=nil) and (FindComponent(NewTableName)=nil) then
          begin
            aDS := TfrDBDataSet.Create(nil);
            aDSo := TDataSource.Create(nil);
            aDS.Name:='P'+NewTableName;
            aDS.OpenDataSource:=True;
            aDSo.Name:=NewTableName;
            aDS.DataSource := aDSo;
            aDSo.DataSet := aDataSet;
            aDataSet.Open;
            Self.InsertComponent(aDS);
            Self.InsertComponent(aDSo);
            writeln(Format('%'+IntToStr(aIdent)+'s',[''])+'DataSet registered:'+NewTableName+'=',aDataSet.RecordCount);
            with aDataSet as IBaseSubDataSets do
              begin
                for i := 0 to GetCount-1 do
                   begin
                     RegisterDataSet(TBaseDBDataset(SubDataSet[i]).DataSet,False,aIdent+2);
                   end;
              end;
          end;
      end;
  except
  end;
end;

end.

