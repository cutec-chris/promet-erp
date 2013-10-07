unit ubarcodes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, LResources, Graphics,
  ubasic,uqr,zint,urender,uaztec,udatamatrix;

type

TBarcodeQR_ECCLevel= (eBarcodeQR_ECCLevel_Auto=0,
                      eBarcodeQR_ECCLevel_L=1,eBarcodeQR_ECCLevel_M=2,
                      eBarcodeQR_ECCLevel_Q=3,eBarcodeQR_ECCLevel_H=4);
TBarcodeAztecRune_Value= 0..999;

{ TLazBarcodeCustomBase }

TLazBarcodeCustomBase=class(TGraphicControl)
private
  mIsPainting: Boolean;
  function GetStrictSize: Boolean;
  procedure SetStrictSize(const AValue: Boolean);
  function GetBackgroundColor: TColor;
  function GetForegroundColor: TColor;
  procedure SetBackgroundColor(const AValue: TColor);
  procedure SetForegroundColor(const AValue: TColor);
protected
  FQR: PointerTo_zint_symbol;
  FLastErrorString: UTF8String;
  FBackgroundColor: TColor;
  FForegroundColor: TColor;
  FStrictSize: Boolean;
  procedure GenerateAndInvalidate;
  procedure Paint; override;
  procedure Resize; override;
  procedure intfPaintOnCanvas(const aTargetCanvas: TCanvas; const aRect: TRect);
public
  constructor Create(AOwner: TComponent); override;
  destructor Destroy; override;
  procedure PaintOnCanvas(const aTargetCanvas: TCanvas; const aRect: TRect);
  procedure Generate; virtual; abstract;
published
  property StrictSize: Boolean read GetStrictSize write SetStrictSize;
  property BackgroundColor: TColor read GetBackgroundColor write SetBackgroundColor default clWhite;
  property ForegroundColor: TColor read GetForegroundColor write SetForegroundColor default clBlack;
end;

{ TLazBarcodeCustomText }

TLazBarcodeCustomText=class(TLazBarcodeCustomBase)
private
  function GetText: UTF8String;
  procedure SetText(const AValue: UTF8String);
protected
  FText: UTF8String;
public
  constructor Create(AOwner: TComponent); override;
published
  property Text: UTF8String read GetText write SetText;
end;

{ TBarcodeQR }

TBarcodeQR=class(TLazBarcodeCustomText)
private
  procedure SetECCLevel(const AValue: TBarcodeQR_ECCLevel);
protected
  FECCLevel: TBarcodeQR_ECCLevel;
  procedure UpdateECCLevel;
public
  procedure Generate; override;
published
  property ECCLevel: TBarcodeQR_ECCLevel read FECCLevel write SetECCLevel default eBarcodeQR_ECCLevel_Auto;
end;

{ TBarcodeMicroQR }

TBarcodeMicroQR=class(TBarcodeQR)
private
protected
public
  procedure Generate; override;
published
end;

{ TBarcodeAztec }

TBarcodeAztec=class(TLazBarcodeCustomText)
private
protected
public
  procedure Generate; override;
published
end;

{ TBarcodeAztecRune }

TBarcodeAztecRune=class(TLazBarcodeCustomBase)
private
  procedure SetValue(const AValue: TBarcodeAztecRune_Value);
protected
  FValue: TBarcodeAztecRune_Value;
public
  procedure Generate; override;
published
  property Value: TBarcodeAztecRune_Value read FValue write SetValue;
end;

{ TBarcodeDataMatrix }

TBarcodeDataMatrix=class(TLazBarcodeCustomText)
private
protected
public
  procedure Generate; override;
published
end;

procedure Register;

implementation

procedure Register;
begin
  {$I tbarcodeqr.lrs}
  RegisterComponents('Laz Barcodes 2D',[TBarcodeQR]);
  {$I tbarcodemicroqr.lrs}
  RegisterComponents('Laz Barcodes 2D',[TBarcodeMicroQR]);
  {$I tbarcodeaztec.lrs}
  RegisterComponents('Laz Barcodes 2D',[TBarcodeAztec]);
  {$I tbarcodeaztecrune.lrs}
  RegisterComponents('Laz Barcodes 2D',[TBarcodeAztecRune]);
  {$I tbarcodedatamatrix.lrs}
  RegisterComponents('Laz Barcodes 2D',[TBarcodeDataMatrix]);
end;

{ TBarcodeDataMatrix }

procedure TBarcodeDataMatrix.Generate;
var
  ErrorCode: integer;
begin
  if Assigned(FQR) then begin
    ZBarcode_Delete(FQR);
    FQR:=nil;
  end;
  if Length(FText)>0 then begin
    FQR:=ZBarcode_Create();
    with FQR^ do begin
      border_width:=1;
    end;
    ErrorCode:=dmatrix(FQR,@FText[1],Length(FText));
    if ErrorCode<>0 then begin
      FLastErrorString:=FQR^.errtxt;
      exit;
    end;
  end;
end;

{ TBarcodeAztecRune }

procedure TBarcodeAztecRune.SetValue(const AValue: TBarcodeAztecRune_Value);
begin
  if FValue=AValue then exit;
  FValue:=AValue;
  GenerateAndInvalidate;
end;

procedure TBarcodeAztecRune.Generate;
var
  ErrorCode: integer;
  NumberText: ansistring;
begin
  if Assigned(FQR) then begin
    ZBarcode_Delete(FQR);
    FQR:=nil;
  end;
  NumberText:=IntToStr(FValue);
  if (Length(NumberText)>0) and (Length(NumberText)<=3) then begin
    FQR:=ZBarcode_Create();
    with FQR^ do begin
      border_width:=0;
    end;
    ErrorCode:=aztec_runes(FQR,@NumberText[1],Length(NumberText));
    if ErrorCode<>0 then begin
      FLastErrorString:=FQR^.errtxt;
      exit;
    end;
  end;
end;

{ TBarcodeAztec }

procedure TBarcodeAztec.Generate;
var
  ErrorCode: integer;
begin
  if Assigned(FQR) then begin
    ZBarcode_Delete(FQR);
    FQR:=nil;
  end;
  if Length(FText)>0 then begin
    FQR:=ZBarcode_Create();
    with FQR^ do begin
      border_width:=0;
    end;
    ErrorCode:=aztec(FQR,@FText[1],Length(FText));
    if ErrorCode<>0 then begin
      FLastErrorString:=FQR^.errtxt;
      exit;
    end;
  end;
end;

{ TLazBarcodeCustomBase }

function TLazBarcodeCustomBase.GetStrictSize: Boolean;
begin
  Result:=FStrictSize;
end;

procedure TLazBarcodeCustomBase.SetStrictSize(const AValue: Boolean);
begin
  if FStrictSize<>AValue then begin
    FStrictSize:=AValue;
    GenerateAndInvalidate;
  end;
end;

function TLazBarcodeCustomBase.GetBackgroundColor: TColor;
begin
  Result:=FBackgroundColor;
end;

function TLazBarcodeCustomBase.GetForegroundColor: TColor;
begin
  Result:=FForegroundColor;
end;

procedure TLazBarcodeCustomBase.SetBackgroundColor(const AValue: TColor);
begin
  if FBackgroundColor<>AValue then begin
    FBackgroundColor:=AValue;
    GenerateAndInvalidate;
  end;
end;

procedure TLazBarcodeCustomBase.SetForegroundColor(const AValue: TColor);
begin
  if FForegroundColor<>AValue then begin
    FForegroundColor:=AValue;
    GenerateAndInvalidate;
  end;
end;

procedure TLazBarcodeCustomBase.GenerateAndInvalidate;
begin
  Generate;
  Self.Invalidate;
end;

procedure TLazBarcodeCustomBase.Paint;
begin
  if mIsPainting then exit;
  mIsPainting:=true;
  intfPaintOnCanvas(Canvas, self.ClientRect);
  mIsPainting:=false;
end;

procedure TLazBarcodeCustomBase.Resize;
begin
  inherited Resize;
  Generate;
end;

constructor TLazBarcodeCustomBase.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBackgroundColor:=clWhite;
  FForegroundColor:=clBlack;
  FStrictSize:=true;
end;

destructor TLazBarcodeCustomBase.Destroy;
begin
  if Assigned(FQR) then begin
    ZBarcode_Delete(FQR);
    FQR:=nil;
  end;
  inherited Destroy;
end;

procedure TLazBarcodeCustomBase.PaintOnCanvas(const aTargetCanvas: TCanvas;
  const aRect: TRect);
begin
  //Destroy rendering
  Generate;
  //Create new rendering
  intfPaintOnCanvas(aTargetCanvas,aRect);
  //Destroy rendering, new rendering generated when paint called.
  Generate;
end;

procedure TLazBarcodeCustomBase.intfPaintOnCanvas(const aTargetCanvas: TCanvas;
  const aRect: TRect);
var
  ErrorCode: integer;
  Line: PointerTo_zint_render_line;
  BaseX,BaseY: integer;
  X,Y: integer;
  procedure ClearBackground;
  begin
    aTargetCanvas.Brush.Color:=FBackgroundColor;
    aTargetCanvas.FillRect(aRect);
  end;
begin
  if not aTargetCanvas.HandleAllocated then exit;
  if not Assigned(FQR) then begin
    ClearBackground;
    exit;
  end;
  if not Assigned(FQR^.rendered) then begin
    X:=aRect.Right-aRect.Left+1;
    Y:=aRect.Bottom-aRect.Top+1;
    if not FStrictSize then begin
      ErrorCode:=render_plot(FQR,X,Y);
    end else begin
      BaseX:=FQR^.width+FQR^.border_width*2;
      BaseY:=FQR^.rows+FQR^.border_width*2;
      ErrorCode:=render_plot(FQR,X-(X mod BaseX),(Y-(Y mod BaseY)));
    end;
    if ErrorCode<>1 then begin
      FLastErrorString:=FQR^.errtxt;
      exit;
    end else begin
      FLastErrorString:='';
    end;
  end;
  if Assigned(FQR^.rendered) then begin
    Line:=FQR^.rendered^.lines;
    ClearBackground;
    aTargetCanvas.Brush.Color:=FForegroundColor;
    while Assigned(Line) do begin
      aTargetCanvas.FillRect( round(Line^.x)+aRect.Left,round(Line^.y)+aRect.Top,
                              round(Line^.x+Line^.width)+aRect.Left,round(Line^.y+Line^.length)+aRect.Top);
      Line:=Line^.next;
    end;
  end;
end;

function TLazBarcodeCustomText.GetText: UTF8String;
begin
  Result:=FText;
end;

procedure TLazBarcodeCustomText.SetText(const AValue: UTF8String);
begin
  if FText<>AValue then begin
    FText:=AValue;
    GenerateAndInvalidate;
  end;
end;

constructor TLazBarcodeCustomText.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FText:=ClassName;
end;

{ TBarcodeQR }

procedure TBarcodeQR.SetECCLevel(const AValue: TBarcodeQR_ECCLevel);
begin
  if FECCLevel=AValue then exit;
  FECCLevel:=AValue;
  GenerateAndInvalidate;
end;

procedure TBarcodeQR.UpdateECCLevel;
begin
  case FECCLevel of
    eBarcodeQR_ECCLevel_L: FQR^.option_1:=1;
    eBarcodeQR_ECCLevel_M: FQR^.option_1:=2;
    eBarcodeQR_ECCLevel_Q: FQR^.option_1:=3;
    eBarcodeQR_ECCLevel_H: FQR^.option_1:=4;
    else begin
      FQR^.option_1:=0;
      FECCLevel:=eBarcodeQR_ECCLevel_Auto;
    end;
  end;
end;

procedure TBarcodeQR.Generate;
var
  ErrorCode: integer;
begin
  if Assigned(FQR) then begin
    ZBarcode_Delete(FQR);
    FQR:=nil;
  end;
  FQR:=ZBarcode_Create();
  with FQR^ do begin
    border_width:=4;
  end;
  UpdateECCLevel;
  ErrorCode:=qr_code(FQR,@FText[1],Length(FText));
  if ErrorCode<>0 then begin
    FLastErrorString:=FQR^.errtxt;
    exit;
  end;
end;

{ TBarcodeMicroQR }

procedure TBarcodeMicroQR.Generate;
var
  ErrorCode: integer;
begin
  if Assigned(FQR) then begin
    ZBarcode_Delete(FQR);
    FQR:=nil;
  end;
  FQR:=ZBarcode_Create();
  with FQR^ do begin
    border_width:=4;
  end;
  UpdateECCLevel;
  ErrorCode:=microqr(FQR,@FText[1],Length(FText));
  if ErrorCode<>0 then begin
    FLastErrorString:=FQR^.errtxt;
    exit;
  end;
end;


end.

