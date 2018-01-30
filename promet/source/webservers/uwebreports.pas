unit uwebreports;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LR_e_htmldiv, LR_e_img, LR_Class, LR_DBSet,
  LR_BarC, LR_RRect, LR_Shape, LR_E_TXT, LR_E_HTM, lr_e_pdf, uBaseDbClasses,
  uBaseDBInterface, uBaseDatasetInterfaces, db;

type

  { TfWebReports }

  TfWebReports = class(TDataModule)
    frBarCodeObject1: TfrBarCodeObject;
    frDBDataSet1: TfrDBDataSet;
    frHTMExport1: TfrHTMExport;
    frImageExport1: TfrImageExport;
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
    function ExportToHTML : string;
    function ExportToText : string;
    function ExportToPNG(aFile: string): Boolean;
    procedure RegisterDataSet(aDataSet: TDataset; DeleteComponents: Boolean=True;
      aIdent: Integer=0);
    procedure ManualRegisterDataSet(aDataSet: TDataset; aName: string;
      DeleteComponents: Boolean);
  end;

var
  fWebReports: TfWebReports;

implementation

uses uBaseApplication,Utils,LCLVersion,dateutils,Graphics;

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

function TfWebReports.ExportToHTML: string;
var
  i: Integer;
  sl: TStringList;
  aFile: String;
begin
  Result := '';
  LastError:='Unknown Error';
  aFile := GetTempDir+'preport.html';
  try
  {$IF ((LCL_MAJOR >= 1) and (LCL_MINOR > 5))}
  FOR i := 0 TO ExportFilters.Count - 1 DO
     if pos('PDF',Uppercase(ExportFilters[i].FilterDesc)) > 0 then
  {$ELSE}
  FOR i := 0 TO frFiltersCount - 1 DO
     if pos('HTML',Uppercase(frFilters[i].FilterDesc)) > 0 then
  {$ENDIF}
      if Report.PrepareReport then
        begin
          {$IF ((LCL_MAJOR >= 1) and (LCL_MINOR > 5))}
          Report.ExportTo(ExportFilters[i].ClassRef,aFile);
          {$ELSE}
          Report.ExportTo(frFilters[i].ClassRef,aFile);
          {$ENDIF}
          sl := TStringList.Create;
          sl.LoadFromFile(aFile);
          result := sl.Text;
          sl.Free;
        end;
    if Result='' then
      LastError:='Report not found !';
  except
    on e : Exception do
      LastError:=e.Message;
  end;
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
  try
  {$IF ((LCL_MAJOR >= 1) and (LCL_MINOR > 5))}
  FOR i := 0 TO ExportFilters.Count - 1 DO
     if pos('PDF',Uppercase(ExportFilters[i].FilterDesc)) > 0 then
  {$ELSE}
  FOR i := 0 TO frFiltersCount - 1 DO
     if pos('TXT',Uppercase(frFilters[i].FilterDesc)) > 0 then
  {$ENDIF}
      if Report.PrepareReport then
        begin
          {$IF ((LCL_MAJOR >= 1) and (LCL_MINOR > 5))}
          Report.ExportTo(ExportFilters[i].ClassRef,aFile);
          {$ELSE}
          Report.ExportTo(frFilters[i].ClassRef,aFile);
          {$ENDIF}
          sl := TStringList.Create;
          sl.LoadFromFile(aFile);
          result := sl.Text;
          sl.Free;
        end;
    if Result='' then
      LastError:='Report not found !';
  except
    on e : Exception do
      LastError:=e.Message;
  end;
end;

function TfWebReports.ExportToPNG(aFile: string): Boolean;
var
  i: Integer;
  aImg: TPortableNetworkGraphic;
  tmp: String;
  aNImg: TPortableNetworkGraphic;
  aTop: Integer;
  Changed: Boolean;
  a: Integer;
  FZoom: Integer;
  FBmp: TPortableNetworkGraphic;
  MaxX: Integer;
  MaxY: Integer;
  TmpVisible: Boolean;
begin
  Result := False;
  LastError:='Unknown Error';
  try
    if Report.PrepareReport then
      begin
        FBmp := TPortableNetworkGraphic.Create;
        FZoom := 1;
        MaxX := 0;
        MaxY := 0;
        for i := 0 to Report.EMFPages.Count-1 do
           begin
             if Round(Report.EMFPages[i]^.PrnInfo.Pgw * FZoom)>MaxX then
               MaxX := Round(Report.EMFPages[i]^.PrnInfo.Pgw * FZoom);
             MaxY := MaxY+Round(Report.EMFPages[i]^.PrnInfo.Pgh * FZoom);
           end;
        FBmp.SetSize(MaxX,MaxY);
        FBmp.Canvas.Brush.Color := clWhite;
        FBmp.Canvas.Brush.Style := bsSolid;
        FBmp.Canvas.FillRect(0, 0, FBmp.Width, FBmp.Height);
        MaxY := 0;
        for i := 0 to Report.EMFPages.Count-1 do
           begin
             TmpVisible := CurReport.EMFPages[i]^.Visible;
             CurReport.EMFPages[i]^.Visible := True;
             CurReport.EMFPages.Draw(i, FBmp.Canvas, Rect(0, MaxY, Round(Report.EMFPages[i]^.PrnInfo.Pgw * FZoom), Round(Report.EMFPages[i]^.PrnInfo.Pgh * FZoom)));
             CurReport.EMFPages[i]^.Visible := TmpVisible;
             MaxY := MaxY+Round(Report.EMFPages[i]^.PrnInfo.Pgh * FZoom);
           end;
        FBmp.SaveToFile(aFile);
        FBmp.Free;
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
  aDS: TfrDBDataSet;
  aDSo: TDataSource;
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
        if (FindComponent('P'+aName)=nil) and (FindComponent(aName)=nil) then
          begin
            aDS := TfrDBDataSet.Create(nil);
            aDSo := TDataSource.Create(nil);
            aDS.Name:='P'+aName;
            aDS.OpenDataSource:=True;
            aDSo.Name:=aName;
            aDS.DataSource := aDSo;
            aDSo.DataSet := aDataSet;
            aDataSet.Open;
            Self.InsertComponent(aDS);
            Self.InsertComponent(aDSo);
          end;
      end;
  except
  end;
end;

end.

