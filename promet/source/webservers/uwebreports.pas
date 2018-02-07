unit uwebreports;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, uBaseDbClasses,
  uBaseDBInterface, uBaseDatasetInterfaces, db,
  fpreport, fpreportfpimageexport, fpreporthtmlexport, fpreportpdfexport;

type

  { TfWebReports }

  TfWebReports = class(TComponent)
  private
    { private declarations }
  public
    { public declarations }
    LastError : string;
    function ExportToPDF(aFile: string): Boolean;
    function ExportToHTML : string;
    function ExportToText : string;
    function ExportToImage(aFile: string): Boolean;
    procedure RegisterDataSet(aDataSet: TDataset; DeleteComponents: Boolean=True;
      aIdent: Integer=0);
    procedure ManualRegisterDataSet(aDataSet: TDataset; aName: string;
      DeleteComponents: Boolean);
    procedure LoadFromFile(aFile : string);
    destructor Destroy; override;
  end;

var
  fWebReports: TfWebReports;

implementation

uses uBaseApplication,Utils,dateutils;

{ TfWebReports }
{
procedure TfWebReports.ReportGetValue(const ParName: String;
  var ParValue: Variant);
begin
  if uppercase(ParName) = 'WEEKNO' then
    ParValue := IntToStr(WeekOfTheYear(Now()))
  else if uppercase(ParName) = 'WEEKDAY' then
    ParValue := IntToStr(DayOfTheWeek(Now()))
  ;
end;
}
destructor TfWebReports.Destroy;
var
  i : Integer;
begin
  while i < ComponentCount do
    if Components[i] is TComponent then
      Components[i].Free
    else inc(i);
  while i < ComponentCount do
    if Components[i] is TDatasource then
      Components[i].Free
    else inc(i);
end;

function TfWebReports.ExportToPDF(aFile : string): Boolean;
var
  i: Integer;
begin
  Result := False;
  LastError:='Unknown Error';
end;

function TfWebReports.ExportToHTML: string;
var
  i: Integer;
  sl: TStringList;
  aFile: String;
begin
  Result := '';
  LastError:='Unknown Error';
  aFile := GetTempDir+'preport.html';
end;

function TfWebReports.ExportToText: string;
var
  i: Integer;
  sl: TStringList;
  aFile: String;
begin
  Result := '';
  LastError:='Unknown Error';
  aFile := GetTempDir+'preport.html';
end;

function TfWebReports.ExportToImage(aFile: string): Boolean;
begin
  Result := False;
  LastError:='Unknown Error';
end;

procedure TfWebReports.RegisterDataSet(aDataSet: TDataset;DeleteComponents : Boolean = True;aIdent : Integer = 0);
var
  i: Integer;
//  aDS: TfrDBDataSet;
  aDSo: TDataSource;
  NewTableName: String;
begin
  i := 0;
  if DeleteComponents then
    begin
      while i < ComponentCount do
        if Components[i] is TComponent then
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
//            aDS := TfrDBDataSet.Create(nil);
            aDSo := TDataSource.Create(nil);
//            aDS.Name:='P'+NewTableName;
//            aDS.OpenDataSource:=True;
            aDSo.Name:=NewTableName;
//            aDS.DataSource := aDSo;
            aDSo.DataSet := aDataSet;
            aDataSet.Open;
//            Self.InsertComponent(aDS);
            Self.InsertComponent(aDSo);
            //debugln(Format('%'+IntToStr(aIdent)+'s',[''])+'DataSet registered:'+NewTableName+'=',aDataSet.RecordCount);
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

procedure TfWebReports.ManualRegisterDataSet(aDataSet: TDataset; aName: string;
  DeleteComponents: Boolean);
var
  i: Integer;
//  aDS: TfrDBDataSet;
  aDSo: TDataSource;
begin
  i := 0;
  if DeleteComponents then
    begin
      while i < ComponentCount do
        if Components[i] is TComponent then
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
        if (FindComponent('P'+aName)=nil) and (FindComponent(aName)=nil) then
          begin
//            aDS := TfrDBDataSet.Create(nil);
            aDSo := TDataSource.Create(nil);
//            aDS.Name:='P'+aName;
//            aDS.OpenDataSource:=True;
            aDSo.Name:=aName;
//            aDS.DataSource := aDSo;
            aDSo.DataSet := aDataSet;
            aDataSet.Open;
//            Self.InsertComponent(aDS);
            Self.InsertComponent(aDSo);
          end;
      end;
  except
  end;
end;

procedure TfWebReports.LoadFromFile(aFile: string);
begin

end;

end.

