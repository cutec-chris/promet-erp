(****************************************** |*******************************************
 *  RarBar, sloupcove zobrazovani procent * |*  RarBar, progress gauge with bar design *
 *     komponenta pro Delphi 3,4   (32b)  * |*               for Delphi 3,4   (32b)    *
 *                 (c) 1999 BEALsoft      * |*                 (c) 1999 BEALsoft       *
 *                       v1.0             * |*                       v1.0              *
 *________________________________________* |*_________________________________________*
 *    !! TATO KOMPONENTA JE ZDARMA !!     * |*     !! THIS COMPONENT IS FREE !!        *
 ****************************************** |*******************************************)
// Autor / Author :
// aberka@atlas.cz, ICQ UIN 2365308, http://bealsoft.cjb.net/ or http://come.to/aberka

unit RarBar;

interface

uses
  LMessages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs;

type
  TRarBar = class(TGraphicControl)
  private
    { Private declarations }
    FLineColor, FTopColor, FSideColor1, FSideColor2, FEmptyColor1, FEmptyColor2,
    FEmptyFrameColor1, FEmptyFrameColor2, FBottomFrameColor, FBottomColor,
    FFilledFrameColor, FFilledColor, FFilledSideColor1, FFilledSideColor2 : TColor;
    FMin, FMax, FProgress : Integer;
    FFont                 : TFont;
    FShowPerc             : Boolean;
//    FTransparent          : Boolean;

    TopX,TopY             : Integer;
    Size                  : Integer;

    procedure Draw;
    procedure SetFont(Value : TFont);
    procedure SetMin(Value : Integer);
    procedure SetMax(Value : Integer);
    procedure SetProgress(Value : Integer);
    procedure SetShowPerc(Value : Boolean);
//    procedure SetTransparent(Value : Boolean);

  protected
    { Protected declarations }
    procedure Paint; override;

  public
    { Public declarations }
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;

  published
    { Published declarations }
    property LineColor : TColor
      read FLineColor write FLineColor;
    property TopColor : TColor
      read FTopColor write FTopColor;
    property SideColor1 : TColor
      read FSideColor1 write FSideColor1;
    property SideColor2 : TColor
      read FSideColor2 write FSideColor2;
    property EmptyColor1 : TColor
      read FEmptyColor1 write FEmptyColor1;
    property EmptyColor2 : TColor
      read FEmptyColor2 write FEmptyColor2;
    property EmptyFrameColor1 : TColor
      read FEmptyFrameColor1 write FEmptyFrameColor1;
    property EmptyFrameColor2 : TColor
      read FEmptyFrameColor2 write FEmptyFrameColor2;
    property BottomFrameColor : TColor
      read FBottomFrameColor write FBottomFrameColor;
    property BottomColor : TColor
      read FBottomColor write FBottomColor;
    property FilledFrameColor : TColor
      read FFilledFrameColor write FFilledFrameColor;
    property FilledColor : TColor
      read FFilledColor write FFilledColor;
    property FilledSideColor1 : TColor
      read FFilledSideColor1 write FFilledSideColor1;
    property FilledSideColor2 : TColor
      read FFilledSideColor2 write FFilledSideColor2;

    property Min : Integer
      read FMin write SetMin;
    property Max : Integer
      read FMax write SetMax;
    property Progress : Integer
      read FProgress write SetProgress;
    property Font : TFont
      read FFont write SetFont;
    property ShowPercentage : Boolean
      read FShowPerc write SetShowPerc;
//    property Transparent : Boolean
//      read FTransparent write SetTransparent;

    property Color;

  end;

procedure Register;

implementation

uses ExtCtrls;

procedure Register;
begin
  RegisterComponents('Samples', [TRarBar]);
end;

constructor TRarBar.Create(AOwner : TComponent);
begin
  inherited;

  FFont:=TFont.Create;
  FLineColor:=$FFE0E0;
  FTopColor:=$FF8080;
  FSideColor1:=$E06868;
  FSideColor2:=$FF8080;
  FEmptyFrameColor1:=$A06868;
  FEmptyFrameColor2:=$BF8080;
  FEmptyColor1:=$C06868;
  FEmptyColor2:=$DF8080;
  FBottomFrameColor:=$64408C;
  FBottomColor:=$7A408C;
  FFilledFrameColor:=$8060A0;
  FFilledSideColor1:=$823C96;
  FFilledSideColor2:=$8848C0;
  FFilledColor:=$A060A0;

  FShowPerc:=True;
  FMin:=0;
  FMax:=100;
  FProgress:=0;
//  FTransparent:=False;

  Height:=180;
  Width:=60;
  Color:=clBtnFace;
  FFont.Style:=[fsBold];
  FFont.Color:=clPurple;
end;

destructor TRarBar.Destroy;
begin
  FFont.Free;

  inherited;
end;

procedure TRarBar.SetFont(Value : TFont);
begin
  FFont.Assign(Value);
end;

procedure TRarBar.Paint;
begin
  inherited;
//  TopY:=5;
  TopY:=5;
  Size:=Height-TopY-5;
  Draw;
end;

procedure TRarBar.Draw;
procedure DrawFrame(Canvas : TCanvas);
begin
  with Canvas do
  begin
    Pen.Color:=LineColor; Pen.Width:=1; Pen.Style:=psSolid; Pen.Mode:=pmCopy;

    MoveTo(TopX,TopY+5);
    LineTo(PenPos.X+15,PenPos.Y-5);
    LineTo(PenPos.X+15,PenPos.Y+5);
    LineTo(PenPos.X-15,PenPos.Y+5);
    LineTo(PenPos.X-15,PenPos.Y-5);

    LineTo(PenPos.X,PenPos.Y+(Size-10));
    LineTo(PenPos.X+15,PenPos.Y+5);
    LineTo(PenPos.X,PenPos.Y-(Size-10));
    MoveTo(PenPos.X,PenPos.Y+(Size-10));
    LineTo(PenPos.X+15,PenPos.Y-5);
    LineTo(PenPos.X,PenPos.Y-(Size-10));
  end;
end;
var Points : Array[1..4] of TPoint;
    MemI   : TBitmap;
    Prog   : Integer;
    R      : Real;
    Perc   : Integer;
    S      : String;
begin
  if (Size = 0) or ((FMax - FMin) = 0) then
  begin
    Perc:=0;
    Prog:=0;
  end else
  begin
    R:=(Progress - FMin) / ( (FMax - FMin) / (Size-10));
    Prog:=Round(R);
    Perc:=Round(R / ((Size-10) / 100));
  end;

  if Prog<0 then Prog:=0 else
    if (Prog>Size-10) then Prog:=Size-10;

  MemI:=TBitmap.Create;
  MemI.Width:=Width;
  MemI.Height:=Height;
//  if FTransparent then
//    MemI.Transparent:=True else
//    MemI.Transparent:=False;

  with MemI.Canvas do
  begin
(* Transparency
    if not FTransparent then
    begin *)
    Brush.Color:=Color;
    FillRect(ClipRect);

    DrawFrame(MemI.Canvas);

    Brush.Color:=TopColor;
    FloodFill(TopX+(15 div 2),TopY+5,Pixels[TopX+(15 div 2),TopY+5],fsSurface);

    Brush.Color:=SideColor1;
    FloodFill(TopX+1,TopY+6,Pixels[TopX+1,TopY+6],fsSurface);
    Brush.Color:=SideColor2;
    FloodFill(TopX+(15*2)-1,TopY+6,Pixels[TopX+(15*2)-1,TopY+6],fsSurface);

    if (Prog>0) then
    begin
      MoveTo(TopX,TopY+Size-5);
      Pen.Color:=BottomFrameColor;
      LineTo(PenPos.X+15,PenPos.Y-5);
      LineTo(PenPos.X+15,PenPos.Y+5);
      Brush.Color:=BottomColor;
      FloodFill(TopX+(15 div 2),TopY+Size-5,SideColor1,fsSurface);
      FloodFill(TopX+15+(15 div 2),TopY+Size-5,SideColor2,fsSurface);

      Brush.Color:=FilledColor;
      Pen.Color:=FilledFrameColor;

      Points[1]:=Point(TopX+15,TopY+Size-Prog);
      Points[2]:=Point(TopX,TopY+Size-Prog-5);
      Points[3]:=Point(TopX+15,TopY+Size-Prog-(2*5));
      Points[4]:=Point(TopX+15*2,TopY+Size-Prog-5);
      Polygon(Points);

      Brush.Color:=FilledSideColor1;
      FloodFill(TopX+1,TopY+Size-5-(Prog div 2),SideColor1,fsSurface);
      Brush.Color:=FilledSideColor2;
      FloodFill(TopX+(15*2)-1,TopY+Size-5-(Prog div 2),SideColor2,fsSurface);

      DrawFrame(MemI.Canvas);
    end else
    begin
      {EMPTY}
      MoveTo(TopX,TopY+Size-5);
      Pen.Color:=EmptyFrameColor1;
      LineTo(PenPos.X+15,PenPos.Y-5);
      Pen.Color:=EmptyFrameColor2;
      LineTo(PenPos.X+15,PenPos.Y+5);
      DrawFrame(MemI.Canvas);

      Brush.Color:=EmptyColor1;
      FloodFill(TopX+(15 div 2),TopY+Size-5,SideColor1,fsSurface);
      Brush.Color:=EmptyColor2;
      FloodFill(TopX+15+(15 div 2),TopY+Size-5,SideColor2,fsSurface);
    end;
    if FShowPerc then
    begin
//      if FTransparent then Brush.Color:=clNone else
      Brush.Color:=Color;
      S:=IntToStr(Perc)+' %';
      MemI.Canvas.Font.Assign(FFont);
      MemI.Canvas.TextOut(TopX+(2*15)+3,TopY+Size-Prog-TextHeight(S),S);
    end;
  end;
(* // Transparency
  if Transparent then
  begin
    Canvas.Brush.Color:=Canvas.Pixels[0,0];
    Canvas.FillRect(Canvas.ClipRect);
  end else *)

  Canvas.Draw(0,0,MemI);
  MemI.Free;
end;

procedure TRarBar.SetMin(Value : Integer);
begin
  FMin:=Value;
  Invalidate;
end;

procedure TRarBar.SetMax(Value : Integer);
begin
  FMax:=Value;
  Invalidate;
end;

procedure TRarBar.SetProgress(Value : Integer);
begin
  FProgress:=Value;
  Invalidate;
end;

procedure TRarBar.SetShowPerc(Value : Boolean);
begin
  FShowPerc:=Value;
  Invalidate;
end;

(* Transparency
procedure TRarBar.SetTransparent(Value : Boolean);
begin
  FTransparent:=Value;
  Draw;
end;
*)

end.