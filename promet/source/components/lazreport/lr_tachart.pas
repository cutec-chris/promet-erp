unit lr_tachart;

interface

uses
  Classes, SysUtils, LResources,
  Graphics,GraphType, Controls, Forms, Dialogs,Buttons,
  StdCtrls,LCLPRoc,

  TAGraph, TASeries,LR_DSet,TADbSource,TACustomSeries,TATools,TACustomSource,
  TAChartUtils,

  LCLType,LR_Class, ExtCtrls, ButtonPanel, ColorBox,db,LR_DBSet;


type

  { TfrChartObject }

  TfrChartObject = class(TComponent)  // fake component
  public
    Constructor Create(aOwner : TComponent); override;
  end;

  TfrChartType=(frstRectangle,frstRoundRect,frstEllipse,frstTriangle,
                frstDiagonal1, frstDiagonal2);
  
  { TfrChartView }

  TfrChartView = class(TfrView)
    procedure FChartSourceGetItem(ASender: TDbChartSource;
      var AItem: TChartDataItem);
  private
    FChart : TChart;
    fDataSetStr: String;
    FFieldColor: String;
    FFieldText: String;
    FFieldX: String;
    FFieldY: String;
    FFixedColor: TColor;
    FBitmap : TBitmap;
    FOptions: TDbChartSourceOptions;
    fType: String;
    FDS: TfrDataSet;
    FDataSource : TDataSource;
    FChartSource: TDbChartSource;
    IsVirtualDS : Boolean;
    procedure DrawChart(aCanvas : TCanvas);
    procedure SetDataSet(AValue: String);
    procedure SetFieldColor(AValue: String);
    procedure SetFieldText(AValue: String);
    procedure SetFieldX(AValue: String);
    procedure SetFieldY(AValue: String);
    procedure SetFixedColor(AValue: TColor);
    procedure SetOptions(AValue: TDbChartSourceOptions);
    procedure SetType(AValue: String);
    procedure InitDataSet(const Desc: String);
  public
    constructor Create(AOwnerPage:TfrPage); override;
    destructor Destroy; override;
    procedure Assign(From: TfrView); override;
    procedure Draw(aCanvas: TCanvas); override;
    procedure Print(Stream: TStream); override;
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;
    procedure LoadFromXML(XML: TLrXMLConfig; const Path: String); override;
    procedure SaveToXML(XML: TLrXMLConfig; const Path: String); override;
  published
    property DataSet: String read fDataSetStr write SetDataSet;
    property ChartType: String read fType write SetType;
    property FieldColor: String read FFieldColor write SetFieldColor;
    property FieldText: String read FFieldText write SetFieldText;
    property FieldX: String read FFieldX write SetFieldX;
    property FieldY: String read FFieldY write SetFieldY;
    property FixedColor: TColor read FFixedColor write SetFixedColor;
    property Options: TDbChartSourceOptions read FOptions write SetOptions default [];
  end;

  { TfrChartForm }

  TfrChartForm = class(TfrObjEditorForm)
    ButtonPanel1: TButtonPanel;
    cbText: TComboBox;
    cbFieldY: TComboBox;
    cbColor: TComboBox;
    cbType: TComboBox;
    cbDataSource: TComboBox;
    cbFieldX: TComboBox;
    cbFixColor: TColorBox;
    Image1: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    procedure cbDataSourceSelect(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure ShowEditor(t: TfrView); override;
  end;

var
  frChartForm: TfrChartForm;
  aView : TfrChartView;
procedure register;

implementation

{$R *.lfm}

uses LR_Const,LR_Utils;
procedure register;
begin
  RegisterComponents('LazReport', [TfrChartObject]);
end;
constructor TfrChartView.Create(AOwnerPage: TfrPage);
begin
  inherited Create(AOwnerPage);
  FDS := nil;
  FBitmap := nil;
  FDataSource := TDataSource.Create(nil);
  fChart := TChart.Create(nil);
  Typ := gtAddIn;
  BaseName := 'Chart';
  FChart.BackColor:=clNone;
  FChart.Color:=clNone;
  FChartSource := TDbChartSource.Create(nil);
  FChartSource.DataSource := FDataSource;
  FChart.BottomAxis.Grid.Color:=clSilver;
  FChart.LeftAxis.Grid.Color:=clSilver;
  debugln('TaChart:'+Self.Name+'('+IntToHex(Integer(Pointer(Self)),8)+')'+','+Self.fDataSetStr+':Create');
end;

destructor TfrChartView.Destroy;
begin
  {
  if FDS <> nil then
    FDS.Exit;
  if IsVirtualDS then
    FDS.Free;
  }
  FreeAndNil(FBitmap);
  FDataSource.Free;
  FChart.Free;
  debugln('TaChart:'+Self.Name+'('+IntToHex(Integer(Pointer(Self)),8)+')'+','+Self.fDataSetStr+':Destroy');
  inherited Destroy;
end;

procedure TfrChartView.FChartSourceGetItem(ASender: TDbChartSource;
  var AItem: TChartDataItem);
begin
  if FFieldColor='' then
    AItem.Color:=FFixedColor;
end;

procedure TfrChartView.DrawChart(aCanvas : TCanvas);
var
  Pts         : Array[0..3] of TPoint;
begin
  with aCanvas do
  begin
    Pen.Width := Round(FrameWidth);
    Pen.Color := FrameColor;
    Pen.Style := psSolid;
    Brush.Style := bsSolid;
    Brush.Color := FillColor;
    FChart.PaintOnCanvas(aCanvas ,Rect(0, 0, FBitmap.Width, FBitmap.Height));
  end;
end;

procedure TfrChartView.SetDataSet(AValue: String);
begin
  if (fDataSetStr=AValue) and Assigned(FDS) then Exit;
  if AValue = '' then exit;
  fDataSetStr:=AValue;
  InitDataSet(DataSet);
  if Assigned(FDS) and (FDS is TfrDBDataSet) then
    FDataSource.DataSet := TfrDBDataSet(FDS).DataSet
  else FDataSource.DataSet := nil;
  if Assigned(FDataSource.DataSet) and (FDataSource.DataSet.FieldDefs.IndexOf(FieldX) > 0) then
    begin
      FChartSource.Options:=[];
      if FDataSource.DataSet.FieldDefs.Find(FieldX).DataType = ftDateTime then
        begin
          FChartSource.Options:=[dcsoDateTimeX];
          FChart.BottomAxis.Marks.Style:=smsLabel;
        end;
    end;
  debugln('TaChart:'+Self.Name+'('+IntToHex(Integer(Pointer(Self)),8)+')'+','+Self.fDataSetStr+':SetDataSet:'+AValue);
end;

procedure TfrChartView.SetFieldColor(AValue: String);
begin
  if FFieldColor=AValue then Exit;
  FFieldColor:=AValue;
  FChartSource.FieldColor:=AValue;
end;

procedure TfrChartView.SetFieldText(AValue: String);
begin
  if FFieldText=AValue then Exit;
  FFieldText:=AValue;
  FChartSource.FieldText:=AValue;
end;

procedure TfrChartView.SetFieldX(AValue: String);
begin
  if FFieldX=AValue then Exit;
  FFieldX:=AValue;
  FChartSource.FieldX:=AValue;
end;

procedure TfrChartView.SetFieldY(AValue: String);
begin
  if FFieldY=AValue then Exit;
  FFieldY:=AValue;
  FChartSource.FieldY:=AValue;
end;

procedure TfrChartView.SetFixedColor(AValue: TColor);
var
  aClass: TBasicChartSeries;
begin
  if FFixedColor=AValue then Exit;
  FFixedColor:=AValue;
  aClass := FChart.Series[0];
  if not Assigned(aClass) then exit;
  if aClass is TBarSeries then
    begin
      if (FFieldColor='') then
        TBarSeries(aClass).SeriesColor:=FFixedColor;
    end;
  if aClass is TAreaSeries then
    begin
      if (FFieldColor='') then
        TAreaSeries(aClass).AreaBrush.Color:=FFixedColor;
    end;
  if aClass is TLineSeries then
    begin
      if (FFieldColor='') then
        TLineSeries(aClass).SeriesColor:=FFixedColor;
    end;
end;

procedure TfrChartView.SetOptions(AValue: TDbChartSourceOptions);
begin
  if FOptions=AValue then Exit;
  FOptions:=AValue;
  FChartSource.Options:=AValue;
end;

procedure TfrChartView.SetType(AValue: String);
var
  aClass: TChartSeries;
begin
  debugln('TaChart:'+Self.Name+'('+IntToHex(Integer(Pointer(Self)),8)+')'+','+Self.fDataSetStr+':SetType:'+AValue);
  if fType=AValue then Exit;
  fType:=AValue;
  FChart.Series.Clear;
  if SeriesClassRegistry.IndexOf(AValue) > -1 then
    begin
      aClass := TChartSeries(TSeriesClass(SeriesClassRegistry.Objects[SeriesClassRegistry.IndexOf(AValue)]).Create(nil));
      fChart.AddSeries(aClass);
      aClass.Source := FChartSource;
    end;
end;

procedure CreateDS(const Desc: String; var DataSet: TfrDataSet; var IsVirtualDS: Boolean);
begin
  if (Desc <> '') and (Desc[1] in ['1'..'9']) then
  begin
    DataSet := TfrUserDataSet.Create(nil);
    DataSet.RangeEnd := reCount;
    DataSet.RangeEndCount := StrToInt(Desc);
    IsVirtualDS := True;
  end
  else
    DataSet := frFindComponent(CurReport.Owner, Desc) as TfrDataSet;
  if DataSet <> nil then
    DataSet.Init;
end;

procedure TfrChartView.InitDataSet(const Desc: String);
begin
  CreateDS(Desc, FDS, IsVirtualDS);

end;

procedure TfrChartView.Draw(aCanvas: TCanvas);
var
  FillC: Integer;
  x1: Int64;
  y1: Int64;
begin
  IF DataSet <> '' THEN
    InitDataSet(DataSet);
  Memo1.Assign(Memo);
  BeginUpdate;
  try
    CalcGaps;
    x1 := Round((SaveDX) * ScaleX);
    y1 := Round((SaveDY) * ScaleY);
    if not Assigned(FBitmap) or (FBitmap.Width <> x1) then
      begin
        FreeAndNil(FBitmap);
        FBitmap := TBitmap.Create;
          FillC := FillColor;
          FillColor := clNone;
          Frames :=[];
          FillColor := FillC;
          FBitmap.Width:=x1;
          FBitmap.Height:=y1;
          DrawChart(FBitmap.Canvas);
      end;
    if Assigned(FBitmap) then
      begin
        debugln('TaChart:'+Self.Name+'('+IntToHex(Integer(Pointer(Self)),8)+')'+','+Self.fDataSetStr+':Draw');
        BeginDraw(aCanvas);
        BeginUpdate;
        try
          aCanvas.Draw(x,y,FBitmap);
        finally
          EndUpdate;
        end;
      end;
    RestoreCoord;
  finally
    EndUpdate;
  end;
end;

procedure TfrChartView.Print(Stream: TStream);
begin
  inherited Print(Stream);
end;

{------------------------------------------------------------------------}
procedure TfrChartView.LoadFromStream(Stream: TStream);
var
  tmp: String;
begin
  debugln('TaChart:'+Self.Name+'('+IntToHex(Integer(Pointer(Self)),8)+')'+','+Self.fDataSetStr+':LoadFromStream');
  inherited LoadFromStream(Stream);
//  if StreamMode = smDesigning then
    begin
      RestoreProperty('ChartType',frReadString(Stream));
      RestoreProperty('FieldX',frReadString(Stream));
      RestoreProperty('FieldY',frReadString(Stream));
      RestoreProperty('FieldColor',frReadString(Stream));
      RestoreProperty('FieldText',frReadString(Stream));
      RestoreProperty('FixedColor',frReadString(Stream));

      RestoreProperty('DataSet',frReadString(Stream));
    end;
end;
procedure TfrChartView.LoadFromXML(XML: TLrXMLConfig; const Path: String);
begin
  inherited LoadFromXML(XML, Path);

  RestoreProperty('ChartType',XML.GetValue(Path+'ChartType/Value',''));
  RestoreProperty('FieldX',XML.GetValue(Path+'FieldX/Value',''));
  RestoreProperty('FieldY',XML.GetValue(Path+'FieldY/Value',''));
  RestoreProperty('FieldColor',XML.GetValue(Path+'FieldColor/Value',''));
  RestoreProperty('FieldText',XML.GetValue(Path+'FieldText/Value',''));
  RestoreProperty('FixedColor',XML.GetValue(Path+'FixedColor/Value',''));

  RestoreProperty('DataSet',XML.GetValue(Path+'DataSet/Value',''));
end;
procedure TfrChartView.SaveToStream(Stream: TStream);
begin
  debugln('TaChart:'+Self.Name+'('+IntToHex(Integer(Pointer(Self)),8)+')'+','+Self.fDataSetStr+':SaveToStream');
  inherited SaveToStream(Stream);
//  if StreamMode = smDesigning then
    begin
      frWriteString(Stream,GetSaveProperty('ChartType'));
      frWriteString(Stream,GetSaveProperty('FieldX'));
      frWriteString(Stream,GetSaveProperty('FieldY'));
      frWriteString(Stream,GetSaveProperty('FieldColor'));
      frWriteString(Stream,GetSaveProperty('FieldText'));
      frWriteString(Stream,GetSaveProperty('FixedColor'));
      frWriteString(Stream,GetSaveProperty('DataSet'));
    end;
end;
procedure TfrChartView.SaveToXML(XML: TLrXMLConfig; const Path: String);
begin
  inherited SaveToXML(XML, Path);
  XML.SetValue(Path+'ChartType/Value', GetSaveProperty('ChartType'));
  XML.SetValue(Path+'FieldX/Value', GetSaveProperty('FieldX'));
  XML.SetValue(Path+'FieldY/Value', GetSaveProperty('FieldY'));
  XML.SetValue(Path+'FieldColor/Value', GetSaveProperty('FieldColor'));
  XML.SetValue(Path+'FieldText/Value', GetSaveProperty('FieldText'));
  XML.SetValue(Path+'FixedColor/Value', GetSaveProperty('FixedColor'));

  XML.SetValue(Path+'DataSet/Value', GetSaveProperty('DataSet'));
end;
procedure TfrChartView.Assign(From: TfrView);
begin
  debugln('TaChart:'+Self.Name+'('+IntToHex(Integer(Pointer(Self)),8)+')'+','+Self.fDataSetStr+':Assign');
  inherited Assign(From);
  if From is TfrChartView then
    begin
      Self.ChartType:=(From as TfrChartView).ChartType;
      Self.FieldX:=(From as TfrChartView).FieldX;
      Self.FieldY:=(From as TfrChartView).FieldY;
      Self.FieldColor:=(From as TfrChartView).FieldColor;
      Self.FieldText:=(From as TfrChartView).FieldText;
      Self.FixedColor:=(From as TfrChartView).FixedColor;
      Self.DataSet:=(From as TfrChartView).DataSet;
    end;
end;

{------------------------------------------------------------------------}
procedure TfrChartForm.ShowEditor(t: TfrView);
begin
  frGetComponents(CurReport.Owner, TfrDataset, cbDataSource.Items, nil);

  cbDataSource.Items.Insert(0, sNotAssigned);
  with TfrChartView(t) do
    begin
      aView := TfrChartView(t);
      cbType.Text := ChartType;
      cbDataSource.Text:=DataSet;
      cbFieldX.Text:=FieldX;
      cbFieldY.Text:=FieldY;
      cbText.Text:=FieldText;
      cbFixColor.Selected:=FixedColor;
      if ShowModal = mrOk then
        begin
          ChartType:=cbType.Text;
          DataSet:=cbDataSource.Text;
          FieldX := cbFieldX.Text;
          FieldY := cbFieldY.Text;
          FieldColor:=cbColor.Text;
          FieldText:=cbText.Text;
          FixedColor:=cbFixColor.Selected;
        end;
    end;
end;

procedure TfrChartForm.FormCreate(Sender: TObject);
begin
//  Caption := sChartFormCaption;
end;

procedure TfrChartForm.cbDataSourceSelect(Sender: TObject);
var
  i: Integer;
begin
  if not Assigned(aView) then exit;
  with aView do
    begin
      DataSet:=cbDataSource.Text;
      cbFieldY.Items.Clear;
      if Assigned(FDS) and (FDS is TfrDBDataSet) then
        begin
          for i := 0 to TfrDBDataSet(FDS).DataSet.FieldCount-1 do
            cbFieldY.Items.Add(TfrDBDataSet(FDS).DataSet.FieldDefs[i].Name);
        end;
      cbFieldX.Items.Assign(cbFieldY.Items);
      cbColor.Items.Assign(cbFieldY.Items);
      cbText.Items.Assign(cbFieldY.Items);
    end;
end;

{ TfrChartObject }
constructor TfrChartObject.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  
  if not assigned(frChartForm) {and not (csDesigning in ComponentState)} then
  begin
    frChartForm:=TfrChartForm.Create(nil);
    frChartForm.cbType.Items.Assign(SeriesClassRegistry);
    frRegisterObject(TfrChartView, frChartForm.Image1.Picture.Bitmap,
      sInsChart, frChartForm);
  end;
end;

initialization

  frChartForm:=nil;

finalization

  if Assigned(frChartForm) then
    frChartForm.Free;

end.
