unit uwebreports;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, uBaseDbClasses,
  uBaseDBInterface, uBaseDatasetInterfaces, db,
  fpreport, fpreportfpimageexport, fpreporthtmlexport, fpreportpdfexport,
  fplazreport,fpReportHTMLParser,fpreportdb, DOM,
  fprepexprpars,nr_intrp;

type

  { TInternalFPLazReport }

  TInternalFPLazReport = class(TFPLazReport)
  public
    function FixDataFields(aFieldName: string): string; override;
  end;

  { TfWebReports }

  TfWebReports = class(TComponent)
    procedure aParserFoundText(Text: string);
    procedure ReportSetCustomproperties(Sender: TObject; Data: TDOMNode);
  private
    { private declarations }
    FTxt : string;
  public
    { public declarations }
    Report: TInternalFPLazReport;
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

  { TFPReportScriptMemo }

  TFPReportScriptMemo = class(TFPReportMemo)
  private
    FScript: TfrInterpretator;
    FScriptSource: string;
    FPrepared : TStringList;
    FText: string;
    procedure SetScript(AValue: string);
  protected
    function PrepareObject(aRTParent: TFPReportElement): TFPReportElement; override;
    procedure ParseText; override;
  public
    function ExpandMacro(const s: String; const AIsExpr: boolean
      ): TFPReportString; override;
    procedure RecalcLayout; override;
    property Script : string read FScriptSource write SetScript;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  { TFPReportPrometMemo }

  TFPReportPrometMemo = class(TFPReportScriptMemo)
  end;

var
  fWebReports: TfWebReports;
  ActualElement : TFPReportElement;

implementation

uses uBaseApplication,Utils,variants,FPimage,fpTTF,FPCanvas,FPImgCanv,synautil;

{ TInternalFPLazReport }

function TInternalFPLazReport.FixDataFields(aFieldName: string): string;
begin
  Result:=inherited FixDataFields(aFieldName);
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

procedure TFPReportScriptMemo.ParseText;
var
  tmp: String;
begin
  try
    inherited ParseText;
  except
    begin
      tmp := synautil.GetBetween('[',']',Text);
      tmp := copy(Text,0,pos(tmp,Text)-1)+StringReplace(StringReplace(tmp,'[','',[rfReplaceAll]),']','',[rfReplaceAll])+']'+copy(Text,rpos(']',Text)+1,length(Text));
      Text := tmp;
      try
        inherited ParseText;
      except
        on e : Exception do
          begin
            //TODO:fWebReports.ReportLog(nil,e.Message);
            Text := '';
          end;
      end;
    end;
  end;
end;

function TFPReportScriptMemo.ExpandMacro(const s: String; const AIsExpr: boolean
  ): TFPReportString;
begin
  Result:=inherited ExpandMacro(s, AIsExpr);
end;

procedure TFPReportScriptMemo.RecalcLayout;
begin
  inherited RecalcLayout;
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
  gTTFontCache.Clear;
  while i < ComponentCount do
    if Components[i] is TFPReportDatasetData then
      begin
        TFPReportDatasetData(Components[i]).DataSet := nil;
        Components[i].Free
      end
    else inc(i);
  Report.Destroy;
  inherited;
end;

procedure TfWebReports.aParserFoundText(Text: string);
begin
  FTxt:=FTxt+Text;
end;

procedure TfWebReports.ReportSetCustomproperties(Sender: TObject; Data: TDOMNode
  );
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
begin
  if GetProperty(Data,'Script') <> '' then
    TFPReportScriptMemo(Sender).Script := Report.FixDataFields(GetProperty(Data,'Script'));
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
          begin
            TFPReportDatasetData(Components[i]).DataSet := nil;
            Components[i].Free
          end
        else inc(i);
    end;
  try
    with aDataSet as IBaseManageDB do
      begin
        if (FindComponent(aName)=nil) and (FindComponent(aName)=nil) then
          begin
            aDS := TFPReportDatasetData.Create(Self);
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
  i: Integer;
  j: Integer;
begin
  if PaperManager.PaperCount=0 then
    PaperManager.RegisterStandardSizes;
  gTTFontCache.ReadStandardFonts;
  gTTFontCache.BuildFontCache;
  Report.LoadFromFile(aFile);
end;

constructor TfWebReports.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Report := TInternalFPLazReport.Create(Self);
  Report.MemoClass:=TFPReportPrometMemo;
  Report.OnSetCustomproperties:=@ReportSetCustomproperties;
end;

end.

