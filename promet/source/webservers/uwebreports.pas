unit uwebreports;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, uBaseDbClasses,
  uBaseDBInterface, uBaseDatasetInterfaces, db,
  fpreport, fpreportfpimageexport, fpreporthtmlexport, fpreportpdfexport,fpreportdb,
  DOM,XMLRead,fpjsonreport,FPReadPNG,FPimage,FPCanvas,FPImgCanv,fpTTF,fpReportHTMLParser,
  fprepexprpars,nr_intrp;

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
    function ExportToImage(aFile: string;DrawFrame : Boolean = True): Boolean;
    procedure RegisterDataSet(aDataSet: TDataset; DeleteComponents: Boolean=True;
      aIdent: Integer=0);
    procedure ManualRegisterDataSet(aDataSet: TDataset; aName: string;
      DeleteComponents: Boolean);
    procedure LoadFromFile(aFile : string);
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  { RFPInterpreter }

  RFPInterpreter = class(TfrInterpretator)
  public
    procedure GetValue(const Name: String; var Value: Variant); override;
    procedure SetValue(const Name: String; Value: Variant); override;
    procedure DoFunction(const name: String; p1, p2, p3: Variant;
      var val: Variant); override;
  end;

  { TFPReportPrometMemo }

  TFPReportPrometMemo = class(TFPReportMemo)
  end;

  { TFPReportScriptMemo }

  TFPReportScriptMemo = class(TFPReportPrometMemo)
  private
    FScript: TfrInterpretator;
    FScriptSource: string;
    FPrepared : TStringList;
    procedure SetScript(AValue: string);
  protected
    function PrepareObject(aRTParent: TFPReportElement): TFPReportElement; override;
  public
    function ExpandMacro(const s: String; const AIsExpr: boolean
      ): TFPReportString; override;
    property Script : string read FScriptSource write SetScript;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  fWebReports: TfWebReports;
  ActualElement : TFPReportElement;

implementation

uses uBaseApplication,Utils,dateutils,base64,FPReadGif,FPReadJPEG,variants;

function PixelsToMM(Const Dist: double) : TFPReportUnits;
begin
  Result:=Dist*(1/3.76);
end;
function MMToPixels(Const Dist: double) : Integer;
begin
  Result:=round(Dist*(3.76));
end;

function PageToMM(Const Dist: double) : TFPReportUnits;
begin
  Result:=Dist*(1/2.83);
end;

{ RFPInterpreter }

procedure RFPInterpreter.GetValue(const Name: String; var Value: Variant);
var
  tmp: String;
  aObj: TFPReportElement;
  aVar: TFPReportVariable;
  aData: TComponent;
  aCol: TFPColor;
  function StringToColorDef(aColor : string;aDef : TFPColor) : TFPColor;
  begin
    Result := aDef;
    case copy(aColor,3,length(aColor)) of
    'White':Result := colWhite;
    'Black':Result := colBlack;
    'Green':Result := colGreen;
    'Blue':Result := colBlue;
    'Yellow':Result := colYellow;
    'Gray':Result := colGray;
    'Red':Result := colRed;
    'Magenta':Result := colMagenta;
    end;
  end;
begin
  tmp := Name;
  aObj := ActualElement;
  if not Assigned(aObj) then exit;
  if Name = 'TODAY' then
    begin
      Value := Now();
      exit;
    end;
  if pos('.',tmp)>0 then
    begin
      aData := fWebReports.FindComponent(copy(tmp,0,pos('.',tmp)-1));
      if Assigned(aData) then
        begin
          tmp := StringReplace(copy(tmp,pos('.',tmp)+1,length(tmp)),'"','',[rfReplaceAll]);
          Value := TFPReportDatasetData(aData).DataSet.FieldByName(tmp).AsVariant;
          exit;
        end;
    end;
  if pos('.',tmp)>0 then
    begin
      aObj := TFPReportElement(fWebReports.Report.FindRecursive(copy(tmp,0,pos('.',tmp)-1)));
      tmp := copy(tmp,pos('.',tmp)+1,length(tmp));
    end
  else
    aObj := ActualElement;
  if Assigned(aObj) then
    begin
      case lowercase(tmp) of
      'left':Value := MMToPixels(aObj.Parent.Layout.Left+aObj.Layout.Left);
      'top':Value := MMToPixels(aObj.Parent.Layout.Left+aObj.Layout.Top);
      'height':Value := MMToPixels(aObj.Layout.Height);
      'width':Value := MMToPixels(aObj.Layout.Width);
      'fillcolor':
        begin
          Value := TFPReportMemo(aObj).Frame.BackgroundColor;
        end;
      'fontcolor':
        begin
          Value := TFPReportMemo(aObj).Font.Color;
        end;
      end;
      if Value <> Null then exit;
    end;
  if StringToColorDef(Name,colTransparent) <> colTransparent then
    begin
      aCol := StringToColorDef(Name,colBlack);
      Value:=RGBToReportColor(aCol.red div 256,aCol.green div 256,aCol.blue div 256);
      exit;
    end;
  tmp := TFPReportScriptMemo(aObj).ExpandMacro(Name,True);
  if tmp <> '' then
    begin
      Value := tmp;
      exit;
    end;
end;

procedure RFPInterpreter.SetValue(const Name: String; Value: Variant);
var
  tmp: String;
  aObj: TFPReportElement;
  aBold: Boolean = false;
  aItalic: Boolean = false;
  aFont: TFPFontCacheItem;
begin
  tmp := Name;
  if pos('.',tmp)>0 then
    begin
      aObj := TFPReportElement(fWebReports.Report.FindRecursive(copy(tmp,0,pos('.',tmp)-1)));
      tmp := copy(tmp,pos('.',tmp)+1,length(tmp));
    end
  else
    aObj := ActualElement;
  if not Assigned(aObj) then exit;
  if Value=null then exit;
  case lowercase(tmp) of
  'left':aObj.Layout.Left:=PixelsToMM(Value);//-aObj.Parent.Layout.Left;
  'top':aObj.Layout.Top:=PixelsToMM(Value);//-aObj.Parent.Layout.Top;
  'height':aObj.Layout.Height:=PixelsToMM(Value);
  'width':aObj.Layout.Width:=PixelsToMM(Value);
  'fillcolor':
    begin
      TFPReportMemo(aObj).Frame.BackgroundColor:= uint32(Value);
      TFPReportMemo(aObj).Frame.Shape:=fsRectangle;
    end;
  'fontcolor':
    begin
      TFPReportMemo(aObj).Font.Color:= uint32(Value);
    end;
  'fontstyle':
    begin
      case Value of
      0: //none
        begin
        end;
      2:
        begin
          aBold:=True;
        end;
      end;
      aFont := gTTFontCache.Find(TFPReportMemo(aObj).Font.Name);
      if Assigned(aFont) then
        aFont := gTTFontCache.Find(aFont.FamilyName,aBold,aItalic);
      if Assigned(aFont) then
        TFPReportMemo(aObj).Font.Name:=aFont.PostScriptName
    end;
  end;
end;

procedure RFPInterpreter.DoFunction(const name: String; p1, p2, p3: Variant;
  var val: Variant);
var
  tmp: String;
begin
  tmp := Name;
end;

{ TFPReportScriptMemo }

procedure TFPReportScriptMemo.SetScript(AValue: string);
var
  mFr: TStringList;
  mErr: TStringList;
  tmp: String;
begin
  if FScriptSource=AValue then Exit;
  FScriptSource:=AValue;
  mFr := TStringList.Create;
  mFr.Text:=FScriptSource;
  mErr:= TStringList.Create;
  FScript.PrepareScript(mFr,FPrepared,mErr);
  tmp := mErr.Text;
  mFr.Free;
  mErr.Free;
end;

function TFPReportScriptMemo.PrepareObject(aRTParent: TFPReportElement
  ): TFPReportElement;
begin
  ActualElement := Self;
  try
    FScript.DoScript(FPrepared);
  except
    on e : Exception do
      Self.Text:=e.Message;
  end;
  Result:=inherited PrepareObject(aRTParent);
end;

function TFPReportScriptMemo.ExpandMacro(const s: String; const AIsExpr: boolean
  ): TFPReportString;
begin
  Result:=inherited ExpandMacro(s, AIsExpr);
end;

constructor TFPReportScriptMemo.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FScript := RFPInterpreter.Create;
  FPrepared := TStringList.Create;
end;

destructor TFPReportScriptMemo.Destroy;
begin
  FPrepared.Free;
  FScript.Free;
  inherited Destroy;
end;

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
      begin
        TFPReportDatasetData(Components[i]).DataSet.Open;
        TFPReportDatasetData(Components[i]).DataSet := nil;
        Components[i].Free
      end
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

function TfWebReports.ExportToImage(aFile: string; DrawFrame: Boolean): Boolean;
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
    Canvas.Brush.FPColor := colWhite;
    Canvas.Brush.Style:=bsSolid;
    Canvas.Pen.FPColor := colWhite;
    Canvas.Pen.Style:=psSolid;
    aTop := 0;
    a := 1;
    nFile := ChangeFileExt(aFile,'')+Format(aExp.SequenceFormat,[a])+'.png';
    while FileExists(nFile) do
      begin
        aNImg.LoadFromFile(nFile);
        Canvas.Rectangle(0,aTop,aWidth+4,aTop+aNImg.Height+4);
        Canvas.Pen.FPColor := colBlack;
        if DrawFrame then
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
    aNImg.Free;
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
  aDetailHeader : TFPReportDataHeaderBand;
  aDetailFooter : TFPReportDataFooterBand;
  aMasterData: TFPReportDataBand;
  aDetailBand: TFPReportDataBand;
  HasFrame: Boolean;
  aBold: Boolean;
  aItalic: Boolean;
  aSize: Integer;
  ss: TStringStream;
  aReader: TFPCustomImageReader;
  fs: TFileStream;
  cd: integer;
  B: Byte;
  k: Integer;
  aColor: Integer;
  function Blue(rgb: Integer): BYTE;
  begin
    Result := (rgb shr 16) and $000000ff;
  end;

  function Green(rgb: Integer): BYTE;
  begin
    Result := (rgb shr 8) and $000000ff;
  end;

  function Red(rgb: Integer): BYTE;
  begin
    Result := rgb and $000000ff;
  end;

  function GetProperty(aNode : TDOMNode;aName : string;aValue : string = 'Value') : string;
  var
    bNode: TDOMNode;
  begin
    Result := '';
    bNode := aNode.FindNode(aName);
    if Assigned(bNode) then
      if Assigned(bNode.Attributes.GetNamedItem(aValue)) then
        Result := bNode.Attributes.GetNamedItem(aValue).NodeValue;
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
    Result := StringReplace(Result,'[DATE]','[TODAY]',[rfReplaceAll,rfIgnoreCase]);
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
                aDetailHeader := nil;
                aDetailFooter := nil;
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
                              aDetailBand := TFPReportDataBand(aBand);
                              if Assigned(aDetailHeader) then
                                begin
                                  aDetailHeader.Data := TFPReportDataBand(aBand).Data;
                                  TFPReportDataBand(aBand).HeaderBand := aDetailHeader;
                                  aDetailHeader := nil;
                                end;
                              if Assigned(aDetailFooter) then
                                begin
                                  aDetailFooter.Data := TFPReportDataBand(aBand).Data;
                                  TFPReportDataBand(aBand).FooterBand := aDetailFooter;
                                  aDetailFooter := nil;
                                end;
                            end;
                          'btDetailHeader':
                            begin
                              aBand := TFPReportDataHeaderBand.Create(aPage);
                              if Assigned(aDetailBand) then
                                begin
                                  aDetailBand.HeaderBand := TFPReportDataHeaderBand(aBand);
                                  TFPReportDataHeaderBand(aBand).Data := aDetailBand.Data;
                                end
                              else aDetailHeader := TFPReportDataHeaderBand(aBand);
                            end;
                          'btDetailFooter':
                            begin
                              aBand := TFPReportDataFooterBand.Create(aPage);
                              if Assigned(aDetailBand) then
                                begin
                                  aDetailBand.FooterBand := TFPReportDataFooterBand(aBand);
                                  TFPReportDataFooterBand(aBand).Data := aDetailBand.Data;
                                end
                              else aDetailFooter := TFPReportDataFooterBand(aBand);
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
                          if Assigned(aBand) then
                            TFPReportDataBand(aBand).StretchMode:=smActualHeight;
                          aObj := aBand;
                        end;
                      'TfrMemoView':
                        begin
                          aDataNode := nPage.ChildNodes.Item[j].FindNode('Size');
                          ourBand := FindBand(aPage,PixelsToMM(StrToFloatDef(GetProperty(aDataNode,'Top'),0)));
                          aDataNode := nPage.ChildNodes.Item[j].FindNode('Data');
                          if GetProperty(aDataNode,'Script') <> '' then
                            begin
                              aObj := TFPReportScriptMemo.Create(ourBand);
                              TFPReportScriptMemo(aObj).Script := FixDataFields(GetProperty(aDataNode,'Script'));
                            end
                          else
                            aObj := TFPReportPrometMemo.Create(ourBand);
                          aDataNode := nPage.ChildNodes.Item[j].FindNode('Size');
                          case GetProperty(nPage.ChildNodes.Item[j],'Alignment') of
                          'taRightJustify':TFPReportMemo(aObj).TextAlignment.Horizontal:=taRightJustified;
                          'taCenter':TFPReportMemo(aObj).TextAlignment.Horizontal:=taCentered;
                          end;
                          case GetProperty(nPage.ChildNodes.Item[j],'Layout') of
                          'tlCenter':TFPReportMemo(aObj).TextAlignment.Vertical:=TFPReportVertTextAlignment.tlCenter;
                          'tlTop':TFPReportMemo(aObj).TextAlignment.Vertical:=TFPReportVertTextAlignment.tlTop;
                          'tlBottom':TFPReportMemo(aObj).TextAlignment.Vertical:=TFPReportVertTextAlignment.tlBottom;
                          end;
                          TFPReportMemo(aObj).StretchMode:=smActualHeight;
                          if GetProperty(nPage.ChildNodes.Item[j],'Flags') = '3' then
                            TFPReportMemo(aObj).StretchMode:=smMaxHeight;
                          TFPReportMemo(aObj).TextAlignment.TopMargin:=1;
                          aDataNode := nPage.ChildNodes.Item[j].FindNode('Data');
                          TFPReportMemo(aObj).Text:=FixDataFields(SysToUni(GetProperty(aDataNode,'Memo')));
                          TFPReportMemo(aObj).UseParentFont := False;
                          aDataNode := nPage.ChildNodes.Item[j].FindNode('Font');
                          aBold := pos('fsBold',GetProperty(aDataNode,'Style'))>0;
                          aItalic := pos('fsItalic',GetProperty(aDataNode,'Style'))>0;
                          aFont := gTTFontCache.Find(GetProperty(aDataNode,'Name'),aBold,aItalic);
                          if not Assigned(aFont) then
                            aFont := gTTFontCache.Find('Arial',aBold,aItalic);
                          if not Assigned(aFont) then
                            aFont := gTTFontCache.Find('FreeSans',aBold,aItalic);
                          if not Assigned(aFont) then
                            aFont := gTTFontCache.Find('DejaVu',aBold,aItalic);
                          if not Assigned(aFont) then
                            begin
                              with gTTFontCache do
                                for b := 0 to Count-1 do
                                  begin
                                    if (pos('sans',lowercase(Items[b].FamilyName)) > 0) and (Items[b].IsItalic = AItalic)
                                        and (Items[b].IsBold = ABold)
                                    then
                                      begin
                                        aFont := Items[b];
                                        break;
                                      end;
                                  end;
                            end;
                          if Assigned(aFont) then
                            TFPReportMemo(aObj).Font.Name:=aFont.PostScriptName
                          else TFPReportMemo(aObj).UseParentFont := true;
                          aSize := StrToIntDef(GetProperty(aDataNode,'Size'),TFPReportMemo(aObj).Font.Size);
                          if aSize>5 then dec(aSize);
                          TFPReportMemo(aObj).Font.Size:=aSize;
                          aColor := StrToIntDef(GetProperty(aDataNode,'Color'),0);
                          TFPReportMemo(aObj).Font.Color:= RGBToReportColor(Red(aColor),Green(aColor),Blue(aColor));
                        end;
                      'TfrLineView':
                        begin
                          aDataNode := nPage.ChildNodes.Item[j].FindNode('Size');
                          ourBand := FindBand(aPage,PixelsToMM(StrToFloatDef(GetProperty(aDataNode,'Top'),0)));
                          aObj := TFPReportShape.Create(ourBand);
                          TFPReportShape(aObj).ShapeType:=stLine;
                          TFPReportShape(aObj).Orientation:=orEast;
                        end;
                      'TfrPictureView':
                        begin
                          aDataNode := nPage.ChildNodes.Item[j].FindNode('Size');
                          ourBand := FindBand(aPage,PixelsToMM(StrToFloatDef(GetProperty(aDataNode,'Top'),0)));
                          aObj := TFPReportImage.Create(ourBand);
                          aDataNode := nPage.ChildNodes.Item[j].FindNode('Picture');
                          aReader:=nil;
                          case lowercase(GetProperty(aDataNode,'Type','Ext')) of
                          'jpeg','jpg':aReader := TFPReaderJPEG.Create;
                          'png':aReader := TFPReaderPNG.create;
                          'gif':aReader := TFPReaderGif.Create;
                          end;
                          if Assigned(aReader) then
                            begin
                              tmp := GetProperty(aDataNode,'Data');
                              ss := TStringStream.Create('');
                              if tmp<>'' then
                                for k:=1 to (system.length(tmp) div 2) do begin
                                  Val('$'+tmp[k*2-1]+tmp[k*2], B, cd);
                                  ss.Write(B, 1);
                                end;
                              ss.Position:=0;
                              fs := TFileStream.Create(GetTempDir+'repimage.'+GetProperty(aDataNode,'Type','Ext'),fmCreate);
                              fs.CopyFrom(ss,0);
                              fs.Free;
                              TFPReportImage(aObj).LoadFromFile(GetTempDir+'repimage.'+GetProperty(aDataNode,'Type','Ext'));
                              TFPReportImage(aObj).Stretched:=True;
                              DeleteFile(GetTempDir+'repimage.'+GetProperty(aDataNode,'Type','Ext'));
                              ss.Free;
                            end;
                          aReader.Free;
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
                                  aColor := StrToIntDef(GetProperty(aDataNode,'FrameColor'),0);
                                  TFPReportElement(aObj).Frame.Color:= RGBToReportColor(Red(aColor),Green(aColor),Blue(aColor));
                                end;
                              TFPReportElement(aObj).Frame.Width := StrToIntDef(GetProperty(aDataNode,'FrameWidth'),0);
                              TFPReportElement(aObj).Frame.Lines:=[];
                              tmp := GetProperty(aDataNode,'FrameBorders');
                              if tmp <> '' then
                                begin
                                  if pos('frbBottom',tmp)>0 then
                                    TFPReportElement(aObj).Frame.Lines := TFPReportElement(aObj).Frame.Lines+[flBottom];
                                  if pos('frbTop',tmp)>0 then
                                    TFPReportElement(aObj).Frame.Lines := TFPReportElement(aObj).Frame.Lines+[flTop];
                                  if pos('frbLeft',tmp)>0 then
                                    TFPReportElement(aObj).Frame.Lines := TFPReportElement(aObj).Frame.Lines+[flLeft];
                                  if pos('frbRight',tmp)>0 then
                                    TFPReportElement(aObj).Frame.Lines := TFPReportElement(aObj).Frame.Lines+[flRight];
                                  HasFrame := True;
                                end;
                            end;
                          if (aObj is TFPReportMemo)
                          and (GetProperty(nPage.ChildNodes.Item[j],'FillColor')<>'clNone')
                          and (GetProperty(nPage.ChildNodes.Item[j],'FillColor')<>'') then
                            begin
                              aColor := StrToIntDef(GetProperty(nPage.ChildNodes.Item[j],'FillColor'),0);
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

