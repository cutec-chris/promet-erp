{*****************************************}
{                                         }
{  Copyright (c) 2012 by C.Ulrich         }
{                                         }
{*****************************************}
unit lr_2dbarcodes;

interface

uses
  Classes, SysUtils, LResources,
  Graphics, Controls, Forms, Dialogs,Buttons,
  StdCtrls, Menus, LCLType, ExtCtrls,LR_Class, IpHtml, LR_Desgn;


type
  Tfr2dBarcodeViewObject = class(TComponent)  // fake component
  end;
  Tfr2dBarcodeView = class(TfrMemoView)
  private
  protected
    procedure ShowContent;
    function CalcHeight: Integer; override;
  public
    constructor Create(AOwnerPage:TfrPage); override;
    destructor Destroy;override;
    procedure Draw(aCanvas: TCanvas); override;
  end;
  TfrRichViewForm = class(TfrObjEditorForm)
    Image1: TImage;
    Panel1: TPanel;
  end;

procedure register;
var
  frRichViewForm : TfrRichViewForm;
implementation
{$R *.lfm}
uses LR_Var, LR_Flds, LR_Const, LR_Utils, InterfaceBase;
resourcestring
  sInsBarcodetext = 'Insert 2d Barcode object';
procedure register;
begin
  RegisterComponents('LazReport', [Tfr2dBarcodeViewObject]);
end;

procedure Tfr2dBarcodeView.ShowContent;
var
  SavX: Integer;
  SavY: Integer;
begin
  SavX:=X;
  SavY:=Y;
  //FHTML.Render(Canvas,Rect(0,0,DRect.Right-DRect.Left,DRect.Bottom-DRect.Top),False,DRect.TopLeft);
  X := SavX;
  Y := SavY;
end;

function Tfr2dBarcodeView.CalcHeight: Integer;
var
  aRect: TRect;
begin
  //aRect := FHTML.GetPageRect(Canvas,DRect.Right-DRect.Left,0);
  Result := aRect.Bottom-aRect.Top;
end;

constructor Tfr2dBarcodeView.Create(AOwnerPage:TfrPage);
begin
  inherited Create(AOwnerPage);
  //FHTML := TUnprotectedHTML.Create;
  BeginUpdate;
  try
    BaseName := 'RichView';
  Finally
    EndUpdate;
  end;
end;

destructor Tfr2dBarcodeView.Destroy;
begin
  //FHTML.Free;
  inherited Destroy;
end;

procedure Tfr2dBarcodeView.Draw(aCanvas: TCanvas);
var
  NeedWrap: Boolean;
  newdx: Integer;
  OldScaleX, OldScaleY: Double;
  IsVisible: boolean;
begin
  BeginDraw(aCanvas);
  if ((Flags and flAutoSize) <> 0) and (Memo.Count > 0) and  (DocMode <> dmDesigning) then
  begin
    newdx := CalcHeight;

    if Alignment=Classes.taRightJustify then
    begin
      x := x + dx - newdx;
      dx := newdx;
    end
    else
      dx := newdx;
  end;
  Streaming := False;
  if Memo1.Text <> Memo.Text then
    begin
      //SetHTML;
    end;
  newdx := CalcHeight;

  OldScaleX := ScaleX;
  OldScaleY := ScaleY;
  ScaleX := 1;
  ScaleY := 1;
  CalcGaps;
  ScaleX := OldScaleX;
  ScaleY := OldScaleY;
  RestoreCoord;

  CalcGaps;

  if Flags and flHideDuplicates <> 0 then
    IsVisible := (flIsDuplicate and Flags = 0)
  else
    IsVisible := true;

  if IsVisible then
  begin
    if not Exporting then ShowBackground;
    if not Exporting then ShowFrame;
    if Memo1.Count > 0 then
      ShowContent;
  end;

  RestoreCoord;
end;

initialization
  if not assigned(frRichViewForm) then
  begin
    frRichViewForm := TfrRichViewForm.Create(nil);
    frRegisterObject(Tfr2dBarcodeView, frRichViewForm.Image1.Picture.Bitmap,
                         sInsBarcodetext, nil);
  end;
finalization
  frRichViewForm.Free;
end.
