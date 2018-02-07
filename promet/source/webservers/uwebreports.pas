unit uwebreports;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, uBaseDbClasses,
  uBaseDBInterface, uBaseDatasetInterfaces, db,
  fpreport, fpreportfpimageexport, fpreporthtmlexport, fpreportpdfexport,fpreportdb,
  DOM,XMLRead;

type

  { TfWebReports }

  TfWebReports = class(TComponent)
  private
    { private declarations }
  public
    { public declarations }
    Report: TFPReport;
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
    constructor Create(AOwner: TComponent); override;
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
  Report.Free;
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
var
  aExp: TFPReportExportfpImage;
begin
  Result := False;
  LastError:='Unknown Error';
  try
    aExp:=TFPReportExportfpImage.Create(Self);
    aExp.BaseFileName:=aFile;
    Report.RunReport;
    Report.RenderReport(aExp);
    aExp.Free;
    Result := True;
  except
  end;
end;

procedure TfWebReports.RegisterDataSet(aDataSet: TDataset;DeleteComponents : Boolean = True;aIdent : Integer = 0);
var
  i: Integer;
  aDS: TFPReportDatasetData;
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
            aDS := TFPReportDatasetData.Create(nil);
            aDS.Name:='P'+NewTableName;
            //aDS.OpenDataSource:=True;
            aDS.DataSet := aDataSet;
            aDataSet.Open;
            Self.InsertComponent(aDS);
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
  aDS: TFPReportDatasetData;
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
            aDS := TFPReportDatasetData.Create(nil);
            aDS.Name:='P'+aName;
//            aDS.OpenDataSource:=True;
            aDS.DataSet := aDataSet;
            aDataSet.Open;
            Self.InsertComponent(aDS);
          end;
      end;
  except
  end;
end;

procedure TfWebReports.LoadFromFile(aFile: string);
var
  LazReport: TXMLDocument;
  i: Integer;
  j: Integer;
  Pages: TDOMNode;
  aPage: TFPReportPage;
  Config: TDOMNode;
  BaseNode: TDOMNode;
  nPage: TDOMNode;
  aBand: TFPReportCustomBand;
  aObj: TFPReportElement;
  aDataNode: TDOMNode;

  function GetProperty(aNode : TDOMNode;aName : string) : string;
  var
    bNode: TDOMNode;
  begin
    Result := '';
    bNode := aNode.FindNode(aName);
    if Assigned(bNode) then
      if Assigned(bNode.Attributes.GetNamedItem('Value')) then
        Result := bNode.Attributes.GetNamedItem('Value').NodeValue;
  end;

begin
  ReadXMLFile(LazReport, aFile);
  BaseNode := LazReport.DocumentElement.FindNode('LazReport');
  if not Assigned(BaseNode) then exit;
  Pages := BaseNode.FindNode('Pages');
  if Assigned(Pages) then
    begin
      Report.TwoPass:= GetProperty(Pages,'DoublePass') = 'True';
      with Pages.ChildNodes do
        begin
          for i := 0 to (Count - 1) do
            if copy(Item[i].NodeName,0,4)='Page' then
              begin
                aPage := TFPReportPage.Create(Report);
                aPage.PageSize.PaperName:='A4';
                nPage := Item[i];
                for j := 0 to nPage.ChildNodes.Count-1 do
                  if copy(nPage.ChildNodes.Item[j].NodeName,0,6)='Object' then
                    begin
                      aObj := nil;
                      case GetProperty(nPage.ChildNodes.Item[j],'ClassName') of
                      'TfrBandView':
                        begin
                          case GetProperty(nPage.ChildNodes.Item[j],'BandType') of
                          'btReportTitle':aBand := TFPReportTitleBand.Create(aPage);
                          'btMasterData':aBand := TFPReportDataBand.Create(aPage);
                          'btMasterHeader':aBand := TFPReportDataHeaderBand.Create(aPage);
                          'btMasterFooter':aBand := TFPReportDataFooterBand.Create(aPage);
                          'btPageHeader':aBand := TFPReportPageHeaderBand.Create(aPage);
                          'btPageFooter':aBand := TFPReportPageFooterBand.Create(aPage);
                          else aBand := TFPReportCustomBand.Create(aPage);
                          aObj := aBand;
                          end;
                        end;
                      'TfrMemoView':
                        begin
                          aObj := TFPReportMemo.Create(aBand);
                          aDataNode := nPage.ChildNodes.Item[j].FindNode('Data');
                          TFPReportMemo(aObj).Text:=GetProperty(aDataNode,'Memo');
                        end;
                      end;
                      if Assigned(aObj) and (aObj is TFPReportElement) then
                        begin
                          aDataNode := nPage.ChildNodes.Item[j].FindNode('Size');
                          if Assigned(aDataNode) then
                            begin
                              TFPReportElement(aObj).Layout.Top:=StrToFloatDef(GetProperty(aDataNode,'Top'),TFPReportElement(aObj).Layout.Top);
                              TFPReportElement(aObj).Layout.Left:=StrToFloatDef(GetProperty(aDataNode,'Left'),TFPReportElement(aObj).Layout.Left);
                              TFPReportElement(aObj).Layout.Width:=StrToFloatDef(GetProperty(aDataNode,'Width'),TFPReportElement(aObj).Layout.Width);
                              TFPReportElement(aObj).Layout.Height:=StrToFloatDef(GetProperty(aDataNode,'Height'),TFPReportElement(aObj).Layout.Height);
                            end;
                        end;
                    end
                  else writeln(nPage.ChildNodes.Item[j].NodeName);
                Report.AddPage(aPage);
              end;
        end;
    end;
  LazReport.Free;
end;

constructor TfWebReports.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Report := TFPReport.Create(Self);
end;

end.

