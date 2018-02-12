unit uwebreports;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, uBaseDbClasses,
  uBaseDBInterface, uBaseDatasetInterfaces, db,
  fpreport, fpreportfpimageexport, fpreporthtmlexport, fpreportpdfexport,fpreportdb,
  DOM,XMLRead,fpjsonreport,FPReadPNG,FPimage,FPCanvas,FPImgCanv,fpTTF,fpReportHTMLParser,
  fprepexprpars;

type

  { TfWebReports }

  TfWebReports = class(TComponent)
    procedure aParserFoundText(Text: string);
  private
    { private declarations }
    FTxt : string;
  public
    { public declarations }
    Report: TFPJSONReport;
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

uses uBaseApplication,Utils,dateutils,Graphics;

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
  i : Integer = 0;
begin
  while i < ComponentCount do
    if Components[i] is TFPReportDatasetData then
      Components[i].Free
    else inc(i);
  Report.Free;
end;

procedure TfWebReports.aParserFoundText(Text: string);
begin
  FTxt:=FTxt+Text;
end;

function TfWebReports.ExportToPDF(aFile : string): Boolean;
var
  i: Integer;
  aExp: TFPReportExportPDF;
begin
  Result := False;
  LastError:='Unknown Error';
  try
    aExp:=TFPReportExportPDF.Create(Self);
    aExp.FileName:=aFile;
    Report.RunReport;
    Report.RenderReport(aExp);
    aExp.Free;
    Result := True;
  except
    on e : Exception do
      begin
        LastError:=e.Message;
        Result := False;
      end;
  end;
end;

function TfWebReports.ExportToHTML: string;
var
  i: Integer;
  sl,sla: TStringList;
  aFile: String;
  aExp: TFPReportExportHTML;
  a: Integer;
  nFile: String;
  tmp: String;
begin
  Result := '';
  sl := TStringList.Create;
  sla := TStringList.Create;
  LastError:='Unknown Error';
  try
    aExp:=TFPReportExportHTML.Create(Self);
    aExp.BaseFileName:=ChangeFileExt(aFile,'')+'.html';
    Report.RunReport;
    Report.RenderReport(aExp);
    a := 1;
    nFile := ChangeFileExt(aFile,'')+Format(aExp.SequenceFormat,[a])+'.html';
    while FileExists(nFile) do
      begin
        sl.LoadFromFile(nFile);
        tmp := sl.Text;
        tmp := copy(tmp,pos('<body>',tmp)+6,length(tmp));
        tmp := copy(tmp,0,pos('</body>',tmp)-1);
        sla.Text := sla.text+'<div>'+tmp+'</div>';
        inc(a);
        nFile := ChangeFileExt(aFile,'')+Format(aExp.SequenceFormat,[a])+'.html';
      end;
    aExp.Free;
    Result := '<body>'+sla.Text+'</body>';
  except
    on e : Exception do
      begin
        LastError:=e.Message;
        Result := '';
      end;
  end;
  sl.Free;
  sla.Free;
end;

function TfWebReports.ExportToText: string;
var
  aParser: THTMLParser;
begin
  Result := '';
  LastError:='Unknown Error';
  aParser := THTMLParser.Create(ExportToHTML);
  aParser.OnFoundText:=@aParserFoundText;
  FTxt:='';
  aParser.Exec;
  aParser.Free;
  result := FTxt;
end;

function TfWebReports.ExportToImage(aFile: string): Boolean;
var
  aExp: TFPReportExportfpImage;
  a: Integer;
  aImg,aNImg : TFPMemoryImage;
  Changed: Boolean;
  aTop: Integer;
  nFile: String;
  Canvas: TFPImageCanvas;
  aWidth: Integer;
begin
  Result := False;
  LastError:='Unknown Error';
  try
    aExp:=TFPReportExportfpImage.Create(Self);
    aExp.BaseFileName:=ChangeFileExt(aFile,'')+'.png';
    Report.RunReport;
    Report.RenderReport(aExp);
    a := 1;
    aTop := 0;
    aWidth := 0;
    Changed := False;
    aNImg := TFPMemoryImage.Create(0,0);
    aImg := TFPMemoryImage.Create(0,0);
    Canvas := TFPImageCanvas.Create (aImg);
    nFile := ChangeFileExt(aFile,'')+Format(aExp.SequenceFormat,[a])+'.png';
    while FileExists(nFile) do
      begin
        aNImg.LoadFromFile(nFile);
        aTop := aTop+aNImg.Height+1;
        if aNImg.Width>aWidth then
          aWidth:=aNImg.Width;
        inc(a);
        nFile := ChangeFileExt(aFile,'')+Format(aExp.SequenceFormat,[a])+'.png';
      end;
    aImg.SetSize(aWidth+4,aTop+4);
    Canvas.Brush.FPColor := TColorToFPColor(clWhite);
    Canvas.Brush.Style:=bsSolid;
    Canvas.Pen.FPColor := TColorToFPColor(clWhite);
    Canvas.Pen.Style:=psSolid;
    aTop := 0;
    a := 1;
    nFile := ChangeFileExt(aFile,'')+Format(aExp.SequenceFormat,[a])+'.png';
    while FileExists(nFile) do
      begin
        aNImg.LoadFromFile(nFile);
        Canvas.Rectangle(0,aTop,aWidth+4,aTop+aNImg.Height+4);
        Canvas.Pen.FPColor := TColorToFPColor(clBlack);
        Canvas.Rectangle(1,aTop+1,aWidth+2,aTop+aNImg.Height+2);
        Canvas.Draw(2,aTop+2,aNImg);
        DeleteFile(nFile);
        aTop := aTop+aNImg.Height;
        Changed := True;
        inc(a);
        nFile := ChangeFileExt(aFile,'')+Format(aExp.SequenceFormat,[a])+'.png';
      end;
    aExp.Free;
    if Changed then
      aImg.SaveToFile(aFile);
    aImg.Free;
    Result := Changed;
  except
    on e : Exception do
      begin
        LastError:=e.Message;
        Result := False;
      end;
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
        if Components[i] is TFPReportDatasetData then
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
        if (FindComponent(NewTableName)=nil) and (FindComponent(NewTableName)=nil) then
          begin
            aDS := TFPReportDatasetData.Create(nil);
            aDS.Name:=NewTableName;
            //aDS.OpenDataSource:=True;
            aDS.DataSet := aDataSet;
            Self.InsertComponent(aDS);
            aDS.Open;
            Report.ReportData.AddReportData(aDS);
            //writeln(Format('%'+IntToStr(aIdent)+'s',[''])+'DataSet registered:'+NewTableName+'=',aDataSet.RecordCount);
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
        if Components[i] is TFPReportDatasetData then
          Components[i].Free
        else inc(i);
    end;
  try
    with aDataSet as IBaseManageDB do
      begin
        if (FindComponent(aName)=nil) and (FindComponent(aName)=nil) then
          begin
            aDS := TFPReportDatasetData.Create(nil);
            aDS.Name:=aName;
            aDS.DataSet := aDataSet;
            aDS.Open;
            Report.ReportData.AddReportData(aDS);
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
  tmp: String;
  ourBand: TFPReportCustomBand;
  OffsetTop: TFPReportUnits;
  OffsetLeft: TFPReportUnits;
  aData: TFPReportData;
  aFont: TFPFontCacheItem;
  aColor: TColor;
  aMasterData: TFPReportDataBand;
  aDetailBand: TFPReportDataBand;
  HasFrame: Boolean;

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

  function PixelsToMM(Const Dist: double) : TFPReportUnits;
  begin
    Result:=Dist*(1/3.5);
  end;
  function PageToMM(Const Dist: double) : TFPReportUnits;
  begin
    Result:=Dist*(1/2.83);
  end;

  function FindBand(aPage : TFPReportPage;aTop : double) : TFPReportCustomBand;
  var
    b : Integer;
  begin
    Result := nil;
    for b := 0 to aPage.BandCount-1 do
      begin
        if (aTop>=aPage.Bands[b].Layout.Top)
        and (aTop<=aPage.Bands[b].Layout.Top+aPage.Bands[b].Layout.Height) then
          begin
            Result := aPage.Bands[b];
            break;
          end;
      end;
  end;

  function FixDataFields(aFieldName : string) : string;
  var
    k : Integer = 0;
    atmp : string;
  begin
    Result := aFieldName;
    while k < ComponentCount do
      begin
        if Components[k] is TFPReportDatasetData then
          Result := StringReplace(Result,TFPReportDatasetData(Components[k]).Name+'.',TFPReportDatasetData(Components[k]).Name+'.',[rfReplaceAll,rfIgnoreCase]);
        inc(k);
      end;
    Result := StringReplace(Result,'PAGE#','PageNo',[rfReplaceAll,rfIgnoreCase]);
  end;

begin
  if PaperManager.PaperCount=0 then
    PaperManager.RegisterStandardSizes;
  gTTFontCache.ReadStandardFonts;
  gTTFontCache.BuildFontCache;
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
            if (copy(Item[i].NodeName,0,4)='Page') and (Item[i].NodeName<>'PageCount') then
              begin
                aMasterData := nil;
                aDetailBand := nil;
                aData := nil;
                aPage := TFPReportPage.Create(Report);
                aPage.PageSize.PaperName:='A4';
                aPage.Font.Name:='ArialMT';
                if GetProperty(Item[i],'Width')<>'' then
                  aPage.PageSize.Width := round(PageToMM(StrToFloatDef(GetProperty(Item[i],'Width'),aPage.PageSize.Width)));
                if GetProperty(Item[i],'Height')<>'' then
                  aPage.PageSize.Height := round(PageToMM(StrToFloatDef(GetProperty(Item[i],'Height'),aPage.PageSize.Width)));
                aDataNode := Item[i].FindNode('Margins');
                if Assigned(aDataNode) then
                  begin
                    aPage.Margins.Top:=PixelsToMM(StrToFloatDef(GetProperty(aDataNode,'Top'),aPage.Margins.Top));
                    aPage.Margins.Left:=PixelsToMM(StrToFloatDef(GetProperty(aDataNode,'left'),aPage.Margins.Left));
                    aPage.Margins.Right:=PixelsToMM(StrToFloatDef(GetProperty(aDataNode,'Right'),aPage.Margins.Right));
                    aPage.Margins.Bottom:=PixelsToMM(StrToFloatDef(GetProperty(aDataNode,'Bottom'),aPage.Margins.Bottom));
                  end;
                nPage := Item[i];
                for j := 0 to nPage.ChildNodes.Count-1 do
                  if copy(nPage.ChildNodes.Item[j].NodeName,0,6)='Object' then
                    begin
                      aObj := nil;
                      ourBand := nil;
                      case GetProperty(nPage.ChildNodes.Item[j],'ClassName') of
                      'TfrBandView':
                        begin
                          tmp := GetProperty(nPage.ChildNodes.Item[j],'BandType');
                          case tmp of
                          'btReportTitle':aBand := TFPReportTitleBand.Create(aPage);
                          'btMasterData':
                            begin
                              aBand := TFPReportDataBand.Create(aPage);
                              tmp := GetProperty(nPage.ChildNodes.Item[j],'DatasetStr');
                              if copy(tmp,1,1)='P' then
                                tmp := copy(tmp,2,system.length(tmp));
                              aData := TFPreportData(Self.FindComponent(tmp));
                              if Assigned(aData) then
                                begin
                                  aPage.Data := aData;
                                  TFPReportDataBand(aBand).Data := aData;
                                end;
                              aMasterData := TFPReportDataBand(aBand);
                              aMasterData.StretchMode:=smActualHeight;
                            end;
                          'btMasterHeader':
                            begin
                              aBand := TFPReportDataHeaderBand.Create(aPage);
                              if Assigned(aMasterData) then
                                aMasterData.HeaderBand := TFPReportDataHeaderBand(aBand);
                            end;
                          'btMasterFooter':
                            begin
                              aBand := TFPReportDataFooterBand.Create(aPage);
                              if Assigned(aMasterData) then
                                aMasterData.FooterBand := TFPReportDataFooterBand(aBand);
                            end;
                          'btDetailData':
                            begin
                              aBand := TFPReportDataBand.Create(aPage);
                              tmp := GetProperty(nPage.ChildNodes.Item[j],'DatasetStr');
                              if copy(tmp,1,1)='P' then
                                tmp := copy(tmp,2,system.length(tmp));
                              if Self.FindComponent(tmp) <> nil then
                                TFPReportDataBand(aBand).Data := TFPreportData(Self.FindComponent(tmp));
                              TFPReportDataBand(aBand).MasterBand := aMasterData;
                              TFPReportDataBand(aBand).StretchMode:=smActualHeight;
                              aDetailBand := TFPReportDataBand(aBand);
                            end;
                          'btDetailHeader':
                            begin
                              aBand := TFPReportDataHeaderBand.Create(aPage);
                              if Assigned(aDetailBand) then
                                aDetailBand.HeaderBand := TFPReportDataHeaderBand(aBand);
                            end;
                          'btDetailFooter':
                            begin
                              aBand := TFPReportDataFooterBand.Create(aPage);
                              if Assigned(aDetailBand) then
                                aDetailBand.FooterBand := TFPReportDataFooterBand(aBand);
                            end;
                          'btPageHeader':aBand := TFPReportPageHeaderBand.Create(aPage);
                          'btPageFooter':aBand := TFPReportPageFooterBand.Create(aPage);
                          'btGroupHeader':
                            begin
                              aBand := TFPReportGroupHeaderBand.Create(aPage);
                              tmp := GetProperty(nPage.ChildNodes.Item[j],'Condition');
                              if copy(tmp,0,1)='[' then
                                tmp := copy(tmp,2,system.length(tmp)-2);//remove []
                              tmp := FixDataFields(tmp);
                              TFPReportGroupHeaderBand(aBand).GroupCondition:=tmp;
                            end;
                          'btGroupFooter':aBand := TFPReportGroupFooterBand.Create(aPage);
                          else
                            aBand := TFPReportCustomBand.Create(aPage);
                          end;
                          aObj := aBand;
                        end;
                      'TfrMemoView':
                        begin
                          aDataNode := nPage.ChildNodes.Item[j].FindNode('Size');
                          ourBand := FindBand(aPage,PixelsToMM(StrToFloatDef(GetProperty(aDataNode,'Top'),0)));
                          aObj := TFPReportMemo.Create(ourBand);
                          //TFPReportMemo(aObj).TextAlignment.Vertical:=TFPReportVertTextAlignment.tlCenter;
                          TFPReportMemo(aObj).TextAlignment.TopMargin:=1;
                          TFPReportMemo(aObj).TextAlignment.BottomMargin:=1;
                          TFPReportMemo(aObj).StretchMode:=smActualHeight;
                          case GetProperty(nPage.ChildNodes.Item[j],'Alignment') of
                          'taRightJustify':TFPReportMemo(aObj).TextAlignment.Horizontal:=taRightJustified;
                          'taCenter':TFPReportMemo(aObj).TextAlignment.Horizontal:=taCentered;
                          end;
                          aDataNode := nPage.ChildNodes.Item[j].FindNode('Data');
                          TFPReportMemo(aObj).Text:=FixDataFields(SysToUTF8(GetProperty(aDataNode,'Memo')));
                          TFPReportMemo(aObj).UseParentFont := False;
                          TFPReportMemo(aObj).Options:=[moAllowHTML];
                          aDataNode := nPage.ChildNodes.Item[j].FindNode('Font');
                          aFont := gTTFontCache.Find(GetProperty(aDataNode,'Name'),false,false);
                          if not Assigned(aFont) then
                            aFont := gTTFontCache.Find('Arial',false,false);
                          if Assigned(aFont) then
                            TFPReportMemo(aObj).Font.Name:=aFont.PostScriptName
                          else TFPReportMemo(aObj).UseParentFont := true;
                          TFPReportMemo(aObj).Font.Size:=StrToIntDef(GetProperty(aDataNode,'Size'),TFPReportMemo(aObj).Font.Size);
                          aColor := StringToColor(GetProperty(aDataNode,'Color'));
                          TFPReportMemo(aObj).Font.Color:= RGBToReportColor(Red(aColor),Green(aColor),Blue(aColor));
                        end;
                      'TfrLineView':
                        begin
                          aDataNode := nPage.ChildNodes.Item[j].FindNode('Size');
                          ourBand := FindBand(aPage,PixelsToMM(StrToFloatDef(GetProperty(aDataNode,'Top'),0)));
                          aObj := TFPReportMemo.Create(ourBand);
                          TFPReportElement(aObj).Frame.Lines := TFPReportElement(aObj).Frame.Lines+[flTop];
                        end;
                      end;
                      if Assigned(aObj) and (aObj is TFPReportElement) then
                        begin
                          TFPReportElement(aObj).Name:=GetProperty(nPage.ChildNodes.Item[j],'Name');
                          aDataNode := nPage.ChildNodes.Item[j].FindNode('Size');
                          if Assigned(aDataNode) then
                            begin
                              if Assigned(ourBand) then
                                OffsetTop := ourBand.Layout.Top
                              else OffsetTop := 0;
                              OffsetLeft :=0;
                              if not (aObj is TFPReportCustomBand) then
                                OffsetLeft := aPage.Margins.Left;
                              TFPReportElement(aObj).Layout.Top:=PixelsToMM(StrToFloatDef(GetProperty(aDataNode,'Top'),TFPReportElement(aObj).Layout.Top))-OffsetTop;
                              TFPReportElement(aObj).Layout.Left:=PixelsToMM(StrToFloatDef(GetProperty(aDataNode,'Left'),TFPReportElement(aObj).Layout.Left))-OffsetLeft;
                              TFPReportElement(aObj).Layout.Width:=PixelsToMM(StrToFloatDef(GetProperty(aDataNode,'Width'),TFPReportElement(aObj).Layout.Width));
                              TFPReportElement(aObj).Layout.Height:=PixelsToMM(StrToFloatDef(GetProperty(aDataNode,'Height'),TFPReportElement(aObj).Layout.Height));
                            end;
                          HasFrame:=False;
                          aDataNode := nPage.ChildNodes.Item[j].FindNode('Frames');
                          if Assigned(aDataNode) then
                            begin
                              TFPReportElement(aObj).Frame.Shape:=fsNone;
                              if GetProperty(aDataNode,'FrameColor')<>'' then
                                begin
                                  aColor := StringToColor(GetProperty(aDataNode,'FrameColor'));
                                  TFPReportElement(aObj).Frame.Color:= RGBToReportColor(Red(aColor),Green(aColor),Blue(aColor));
                                end;
                              TFPReportElement(aObj).Frame.Width := StrToIntDef(GetProperty(aDataNode,'FrameWidth'),0);
                              tmp := GetProperty(aDataNode,'FrameBorders');
                              if tmp <> '' then
                                begin
                                  TFPReportElement(aObj).Frame.Lines:=[];
                                  if pos('frbBottom',tmp)>0 then
                                    TFPReportElement(aObj).Frame.Lines := TFPReportElement(aObj).Frame.Lines+[flBottom];
                                  if pos('frbTop',tmp)>0 then
                                    TFPReportElement(aObj).Frame.Lines := TFPReportElement(aObj).Frame.Lines+[flTop];
                                  if pos('frbLeft',tmp)>0 then
                                    TFPReportElement(aObj).Frame.Lines := TFPReportElement(aObj).Frame.Lines+[flLeft];
                                  if pos('frbRight',tmp)>0 then
                                    TFPReportElement(aObj).Frame.Lines := TFPReportElement(aObj).Frame.Lines+[flRight];
                                  HasFrame := True;
                                end
                              else TFPReportElement(aObj).Frame.Lines:=[];
                            end;
                          if (aObj is TFPReportMemo)
                          and (GetProperty(nPage.ChildNodes.Item[j],'FillColor')<>'clNone')
                          and (GetProperty(nPage.ChildNodes.Item[j],'FillColor')<>'') then
                            begin
                              aColor := StringToColor(GetProperty(nPage.ChildNodes.Item[j],'FillColor'));
                              TFPReportMemo(aObj).Frame.BackgroundColor:= RGBToReportColor(Red(aColor),Green(aColor),Blue(aColor));
                              TFPReportMemo(aObj).Frame.Shape:=fsRectangle;
                              if not HasFrame then
                                begin
                                  TFPReportMemo(aObj).Frame.Color:=RGBToReportColor(Red(aColor),Green(aColor),Blue(aColor));
                                  TFPReportMemo(aObj).Frame.Pen:=psClear;
                                end;
                            end;
                        end;
                    end;
                Report.AddPage(aPage);
              end;
        end;
    end;
  Report.SaveToFile(GetTempDir+'areport.json');
  LazReport.Free;
end;

Procedure BuiltinIFS(Var Result : TFPExpressionResult; Const Args : TExprParameterArray);

begin
  If Args[0].resBoolean then
    Result.resString:=Args[1].resString
  else
    Result.resString:=Args[2].resString
end;

constructor TfWebReports.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Report := TFPJSONReport.Create(Self);
end;

initialization
  BuiltinIdentifiers.AddFunction(bcBoolean,'IF','S','BSS',@BuiltinIFS);
end.

