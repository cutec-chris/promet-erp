unit uThumbViewer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Graphics, LCLType, Forms;

type
  TThumbViewer = class(TScrollingWinControl)
  private
    FBitmap : TBitmap;
    FBorder: Integer;
    FCount: Integer;
    FRowHeight : Integer;
    FColCount : Integer;
    FRowCount : Integer;
    FScale : real;
    FMarginLeft : Integer;
    FThumbHeight: Integer;
    FThumbWidth: Integer;
    procedure SetBorder(AValue: Integer);
    procedure SetCount(AValue: Integer);
    procedure SetThumbHeight(AValue: Integer);
    procedure SetThumbWidth(AValue: Integer);
  protected
    procedure Recalculate;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure EraseBackground(DC: HDC); override;
    procedure Paint; override;
    procedure Resize; override;
    property ThumbWidth : Integer read FThumbWidth write SetThumbWidth;
    property ThumbHeight : Integer read FThumbHeight write SetThumbHeight;
    property Border : Integer read FBorder write SetBorder;
    property Count : Integer read FCount write SetCount;
  end;

implementation

procedure TThumbViewer.SetCount(AValue: Integer);
begin
  if FCount=AValue then Exit;
  FCount:=AValue;
  Recalculate;
  VertScrollBar.
end;

procedure TThumbViewer.SetBorder(AValue: Integer);
begin
  if FBorder=AValue then Exit;
  FBorder:=AValue;
end;

procedure TThumbViewer.SetThumbHeight(AValue: Integer);
begin
  if FThumbHeight=AValue then Exit;
  FThumbHeight:=AValue;
end;

procedure TThumbViewer.SetThumbWidth(AValue: Integer);
begin
  if FThumbWidth=AValue then Exit;
  FThumbWidth:=AValue;
end;

procedure TThumbViewer.Recalculate;
begin
  if (FThumbWidth=0) or (FScale=0) then exit;
  FColCount:=Width div round(((FThumbWidth)+(FBorder*2))*FScale);
  FMarginLeft:=round(Width-((FColCount*((FThumbWidth+FBorder)*FScale))-(FBorder*FScale))) div 2;
end;

constructor TThumbViewer.Create(AOwner: TComponent);
begin
  FBitmap := TBitmap.Create;
  inherited Create(AOwner);
  DoubleBuffered:=True;
  FScale := 0.5;
  FBorder:=10;
  FThumbHeight:=428;
  FThumbWidth:=310;
  Recalculate;
end;

destructor TThumbViewer.Destroy;
begin
  FBitmap.Free;
  inherited Destroy;
end;

procedure TThumbViewer.EraseBackground(DC: HDC);
begin
end;

procedure TThumbViewer.Paint;
var
  x, y: Integer;
  xw: Int64;
  yh: Int64;
  aRect: TRect;
  aStyle : TTextStyle;
begin
  try
    // Draws squares
    FBitmap.Canvas.Pen.Color := clBlack;
    xw := round((FThumbWidth+FBorder)*FScale);
    yh := round((FThumbHeight+FBorder)*FScale);
    for x := 0 to FColCount-1 do
      for y := 0 to Height div yh do
        begin
          if y*FColCount+x <= FCount then
            begin
              aRect := Rect(round(FMarginLeft+((x*xw)+(FBorder*FScale))),round(FBorder+(y*yh)),round(FMarginLeft+(((x+1)*xw))),round(((y+1)*yh)+(FBorder*FScale)));
              FBitmap.Canvas.Rectangle(aRect);
              FBitmap.Canvas.TextRect(aRect,aRect.Left,aRect.Top,IntToStr(y*FColCount+x)); ;
            end;
        end;

    Canvas.Draw(0, 0, FBitmap);
  finally
  end;

  inherited Paint;
end;

procedure TThumbViewer.Resize;
begin
  inherited Resize;
  // Initializes the Bitmap Size
  fBitmap.Height := Height;
  fBitmap.Width := Width;

  // Draws the background
  fBitmap.Canvas.Pen.Color := clWhite;
  fBitmap.Canvas.Rectangle(0, 0, Width, Height);

  Recalculate;

  Invalidate;
end;

end.

